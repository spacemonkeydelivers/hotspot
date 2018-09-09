/*
 * Copyright (c) 2000, 2014, Oracle and/or its affiliates. All rights reserved.
 * Copyright 2012, 2014 SAP AG. All rights reserved.
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS FILE HEADER.
 *
 * This code is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License version 2 only, as
 * published by the Free Software Foundation.
 *
 * This code is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 * version 2 for more details (a copy is included in the LICENSE file that
 * accompanied this code).
 *
 * You should have received a copy of the GNU General Public License version
 * 2 along with this work; if not, write to the Free Software Foundation,
 * Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA.
 *
 * Please contact Oracle, 500 Oracle Parkway, Redwood Shores, CA 94065 USA
 * or visit www.oracle.com if you need additional information or have any
 * questions.
 *
 */

#include "precompiled.hpp"
#include "interpreter/interpreter.hpp"
#include "memory/resourceArea.hpp"
#include "oops/markOop.hpp"
#include "oops/method.hpp"
#include "oops/oop.inline.hpp"
#include "runtime/frame.inline.hpp"
#include "runtime/handles.inline.hpp"
#include "runtime/javaCalls.hpp"
#include "runtime/monitorChunk.hpp"
#include "runtime/signature.hpp"
#include "runtime/stubCodeGenerator.hpp"
#include "runtime/stubRoutines.hpp"
#include "vmreg_riscv.inline.hpp"
#ifdef COMPILER1
#include "c1/c1_Runtime1.hpp"
#include "runtime/vframeArray.hpp"
#endif

#ifdef ASSERT
void RegisterMap::check_location_valid() {
}
#endif // ASSERT

// Stack walking for signal handling/profiling

bool frame::safe_for_sender(JavaThread *thread) {
  address   sp = (address)_sp;
  address   fp = (address)_fp;
  address   unextended_sp = (address)_unextended_sp;
  // sp must be within the stack
  bool sp_safe = (sp <= thread->stack_base()) &&
                 (sp >= thread->stack_base() - thread->stack_size());

  if (!sp_safe) {
    return false;
  }

  // unextended sp must be within the stack and above or equal sp
  bool unextended_sp_safe = (unextended_sp <= thread->stack_base()) &&
                            (unextended_sp >= sp);

  if (!unextended_sp_safe) {
    return false;
  }

  // an fp must be within the stack and above (but not equal) sp
  bool fp_safe = (fp <= thread->stack_base()) && (fp > sp);

  // We know sp/unextended_sp are safe only fp is questionable here

  // If the current frame is known to the code cache then we can attempt to
  // to construct the sender and do some validation of it. This goes a long way
  // toward eliminating issues when we get in frame construction code

  if (_cb != NULL ) {

    // First check if frame is complete and tester is reliable
    // Unfortunately we can only check frame complete for runtime stubs and nmethod
    // other generic buffer blobs are more problematic so we just assume they are
    // ok. adapter blobs never have a frame complete and are never ok.

    if (!_cb->is_frame_complete_at(_pc)) {
      if (_cb->is_nmethod() || _cb->is_adapter_blob() || _cb->is_runtime_stub()) {
        return false;
      }
    }
    // Entry frame checks
    if (is_entry_frame()) {
      // an entry frame must have a valid fp.

      if (!fp_safe) return false;

      // Validate the JavaCallWrapper an entry frame must have

      address jcw = (address)entry_frame_call_wrapper();

      bool jcw_safe = (jcw <= thread->stack_base()) && ( jcw > fp);

      return jcw_safe;

    }

    intptr_t* sender_sp = NULL;
    address   sender_pc = NULL;

    if (is_interpreted_frame()) {
      // fp must be safe
      if (!fp_safe) {
        return false;
      }

      sender_pc = (address) at(frame_ra_index);
      sender_sp = (intptr_t*) fp;

    } else {
      // must be some sort of compiled/runtime frame
      // fp does not have to be safe (though it probably is)

      sender_sp = _unextended_sp + _cb->frame_size();
      sender_pc = (address) sender_sp[frame_ra_index];
    }

    // We must always be able to find a recognizable pc
    CodeBlob* sender_blob = CodeCache::find_blob_unsafe(sender_pc);
    if (sender_pc == NULL ||  sender_blob == NULL) {
      return false;
    }

    // If the potential sender is the interpreter then we can do some more checking
    if (Interpreter::contains(sender_pc)) {

      intptr_t *saved_fp = (intptr_t*) sender_sp[frame_fp_index];
      bool saved_fp_safe = ((address)saved_fp <= thread->stack_base()) && (saved_fp > sender_sp);

      if (!saved_fp_safe) {
        return false;
      }

      // construct the potential sender

      frame sender(sender_sp, saved_fp, sender_pc);

      return sender.is_interpreted_frame_valid(thread);

    }

    // Could just be some random pointer within the codeBlob

    if (!sender_blob->code_contains(sender_pc)) return false;

    // We should never be able to see an adapter if the current frame is something from code cache

    if ( sender_blob->is_adapter_blob()) {
      return false;
    }

    // Could be the call_stub

    if (StubRoutines::returns_to_call_stub(sender_pc)) {
      intptr_t *saved_fp = (intptr_t*) sender_sp[frame_fp_index];
      bool saved_fp_safe = ((address)saved_fp <= thread->stack_base()) && (saved_fp > sender_sp);

      if (!saved_fp_safe) {
        return false;
      }

      // construct the potential sender

      frame sender(sender_sp, saved_fp, sender_pc);

      // Validate the JavaCallWrapper an entry frame must have
      address jcw = (address)sender.entry_frame_call_wrapper();

      bool jcw_safe = (jcw <= thread->stack_base()) && ( jcw > (address)sender.fp());

      return jcw_safe;
    }

    // If the frame size is 0 something is bad because every nmethod has a non-zero frame size
    // because the return address counts against the callee's frame.

    if (sender_blob->frame_size() == 0) {
      assert(!sender_blob->is_nmethod(), "should count return address at least");
      return false;
    }

    // We should never be able to see anything here except an nmethod. If something in the
    // code cache (current frame) is called by an entity within the code cache that entity
    // should not be anything but the call stub (already covered), the interpreter (already covered)
    // or an nmethod.

    assert(sender_blob->is_nmethod(), "Impossible call chain");

    // Could put some more validation for the potential non-interpreted sender
    // frame we'd create by calling sender if I could think of any. Wait for next crash in forte...

    // One idea is seeing if the sender_pc we have is one that we'd expect to call to current cb

    // We've validated the potential sender that would be created
    return true;
  }

  // Must be native-compiled frame. Since sender will try and use fp to find
  // linkages it must be safe

  if (!fp_safe) {
    return false;
  }

  // Will the pc we fetch be non-zero (which we'll find at the oldest frame)

  if ( (address) at(frame_ra_index) == NULL) return false;

  // could try and do some more potential verification of native frame

  return true;

}


void frame::patch_pc(Thread* thread, address pc) {
  address *saved_pc =  &((address *)sp())[frame_ra_index];

  if (TracePcPatching) {
    tty->print_cr("patch_pc at address" INTPTR_FORMAT " [" INTPTR_FORMAT " -> " INTPTR_FORMAT "] ",
                   saved_pc, *saved_pc, pc);
  }

  *saved_pc = pc;
  _cb = CodeCache::find_blob(pc);

  address original_pc = nmethod::get_deopt_original_pc(this);
  if (original_pc != NULL) {
    address orig = (((nmethod*)_cb)->get_original_pc(this));
    assert(orig == _pc, "expected original to be stored before patching");
    _deopt_state = is_deoptimized;
    // leave _pc as is
  } else {
    _deopt_state = not_deoptimized;
    _pc = pc;
  }
}

//
// Frame queries
//

bool frame::is_interpreted_frame() const  {
  //fprintf(stderr, "is_interpreted_frame pc: %p\n", pc());
  return Interpreter::contains(pc());
}

intptr_t* frame::entry_frame_argument_at(int offset) const {
  int index = (Interpreter::expr_offset_in_bytes(offset)/wordSize);
  // Entry frame's arguments are always in relation to fp()
  return addr_at(index);
}

// sender_sp

intptr_t* frame::interpreter_frame_sender_sp() const {
  assert(is_interpreted_frame(), "interpreted frame expected");
  return sender_sp();
}

void frame::set_interpreter_frame_sender_sp(intptr_t* sender_sp) {
  assert(is_interpreted_frame(), "interpreted frame expected");
  *((intptr_t**) sp_addr_at(frame_fp_index)) = sender_sp;
}

// Used by template based interpreter deoptimization
void frame::interpreter_frame_set_saved_sp(intptr_t* sp) {
  *((intptr_t**) addr_at(interpreter_frame_saved_SP_index)) = sp;
}


frame frame::sender_for_entry_frame(RegisterMap* map) const {
  assert(map != NULL, "map must be set");
  // Java frame called from C; skip all C frames and return top C
  // frame of that chunk as the sender
  JavaFrameAnchor* jfa = entry_frame_call_wrapper()->anchor();
  assert(!entry_frame_is_first(), "next Java fp must be non zero");
  assert(jfa->last_Java_sp() > sp(), "must be above this frame on stack");
  map->clear();
  assert(map->include_argument_oops(), "should be set by clear");
  if (jfa->last_Java_pc() != NULL ) {
    frame fr(jfa->last_Java_sp(), jfa->last_Java_fp(), jfa->last_Java_pc());
    return fr;
  }
  frame fr(jfa->last_Java_sp(), jfa->last_Java_fp());
  return fr;
}

frame frame::sender_for_interpreter_frame(RegisterMap* map) const {
  // sp is the raw sp from the sender after adapter or interpreter extension
  intptr_t* sp = (intptr_t*) sender_sp();
  intptr_t* fp = link();

  // This is the sp before any possible extension (adapter/locals).
  intptr_t* unextended_sp = (intptr_t*) fp[interpreter_frame_saved_SP_index];

  // Since the interpreter always saves FP, if we record where it is then
  // we don't have to always save it on entry and exit from c2 compiled
  // code; on entry will be enough.
  // Note: We don't need to update the locations of the callee-saved registers
  // used in interpreter frames, because in the interpreter, callee-saved are 
  // only used for the Interp State regs (Rmethod, etc). These are found by 
  // the VM (and updated by GC) using defined interfaces for accessing their 
  // frame slots.
  // FIXME: RISCV: This shouldn't be necessary, because of our strict use of FP.
#ifdef unnecessary //  COMPILER2
  if (map->update_map()) {
    map->set_location(FP->as_VMReg(), (address)&sp[frame_fp_index]);
    map->set_location(FP->as_VMReg()->next(), (address)&sp[frame_fp_index]);
  }
#endif /* COMPILER2 */
  return frame(sp, unextended_sp, fp, sender_pc());
}


//-----------------------------sender_for_compiled_frame-----------------------
frame frame::sender_for_compiled_frame(RegisterMap* map) const {
  assert(map != NULL, "map must be set");

  // frame owned by optimizing compiler
  intptr_t* sender_sp = NULL;

  assert(_cb->frame_size() >= 0, "must have non-zero frame size");
  sender_sp = *(intptr_t**)sp_addr_at(frame_fp_index);
  address sender_pc = (address)sender_sp[frame_ra_index];

#ifdef assert
#if 0  // FIXME: Tile: See if this assert can be re-enabled
  intptr_t* fp_slot = sp_addr_at(frame_fp_index);
  address sender_pc = (address)sender_sp[frame_ra_index];
  assert(*fp_slot == (intptr_t)sender_sp, "expected stored FP in frame");
#endif
#endif

  intptr_t *saved_fp = (intptr_t*)sender_sp[frame_fp_index];

  if (map->update_map()) {
    // Tell GC to use argument oopmaps for some runtime stubs that need it.
    // For C1, the runtime stub might not have oop maps, so set this flag
    // outside of update_register_map.
    map->set_include_argument_oops(
      _cb->caller_must_gc_arguments(map->thread()));

    // Unlike X86, we don't need to update the location of the frame pointer
    // register, because we don't allow C2 to allocate it. It is always the
    // frame pointer, updated by call/return code.

    // Update the register map for those in this code blob.
    if (_cb->oop_maps() != NULL) {
      OopMapSet::update_register_map(this, map);
    }
  }

  assert(sender_sp != sp(), "must have changed");
  return frame(sender_sp, saved_fp, sender_pc);
}

frame frame::sender(RegisterMap* map) const {
  map->set_include_argument_oops(false);

  if (is_entry_frame())       return sender_for_entry_frame(map);
  if (is_interpreted_frame()) return sender_for_interpreter_frame(map);
  assert(_cb == CodeCache::find_blob(pc()),"Must be the same");

  if (_cb != NULL) {
    return sender_for_compiled_frame(map);
  }
  // Must be native-compiled frame, i.e. the marshaling code for native
  // methods that exists in the core system.
  return frame(sender_sp(), link(), sender_pc());
}


bool frame::interpreter_frame_equals_unpacked_fp(intptr_t* fp) {
  assert(is_interpreted_frame(), "must be interpreter frame");
  Method* method = *interpreter_frame_method_addr();
  // When unpacking an optimized frame the frame pointer is
  // adjusted with:
  int diff = (method->max_locals() - method->size_of_parameters()) *
             Interpreter::stackElementWords;
  return _fp == (fp - diff);
}

void frame::pd_gc_epilog() {
  // FIXME: RISCV:  This function only does something on sparc.
  //              Why not needed on other platforms?
}

bool frame::is_interpreted_frame_valid(JavaThread* thread) const {

  assert(is_interpreted_frame(), "Not an interpreted frame");
  // These are reasonable sanity checks
  if (fp() == 0 || (intptr_t(fp()) & (wordSize-1)) != 0) {
    return false;
  }
  if (sp() == 0 || (intptr_t(sp()) & (wordSize-1)) != 0) {
    return false;
  }
  if (fp() - interpreter_frame_local_words < sp()) {
    return false;
  }

  // These are hacks to keep us out of trouble.
  // The problem with these is that they mask other problems
  if (fp() <= sp()) {        // this attempts to deal with unsigned comparison above
    return false;
  }

  // do some validation of frame elements

  // first the method

  Method* m = *interpreter_frame_method_addr();

  // validate the method we'd find in this potential sender
  if (!m->is_valid_method()) return false;

  // stack frames shouldn't be much larger than max_stack elements

  if (fp() - sp() > 1024 + m->max_stack()*Interpreter::stackElementSize) {
    return false;
  }

  // validate bci/bcx

  intptr_t  bcx    = interpreter_frame_bcx();
  if (m->validate_bci_from_bcx(bcx) < 0) {
    return false;
  }

  // validate ConstantPoolCache*

  ConstantPoolCache* cp = *interpreter_frame_cache_addr();

  if (cp == NULL || !cp->is_metaspace_object()) return false;

  // validate locals

  address locals =  (address) *interpreter_frame_locals_addr();

  if (locals > thread->stack_base() || locals < (address) fp()) return false;

  // We'd have to be pretty unlucky to be mislead at this point

  return true;
}

BasicType frame::interpreter_frame_result(oop* oop_result, jvalue* value_result) {

  assert(is_interpreted_frame(), "interpreted frame expected");
  Method* method = *interpreter_frame_method_addr();
  BasicType type = method->result_type();

  intptr_t* tos_addr = (intptr_t*)interpreter_frame_tos_address();

  switch (type) {
    case T_OBJECT  :
    case T_ARRAY   : {
      oop obj;
      if (method->is_native()) {
        obj = (oop) (void*) at(interpreter_frame_oop_temp_offset);
      } else {
        oop* obj_p = (oop*)tos_addr;
        obj = (obj_p == NULL) ? (oop)NULL : *obj_p;
      }
      assert(obj == NULL || Universe::heap()->is_in(obj), "sanity check");
      *oop_result = obj;
      break;
    }
    case T_BOOLEAN : value_result->z = *(jboolean*)tos_addr; break;
    case T_BYTE    : value_result->b = *(jbyte*)tos_addr; break;
    case T_CHAR    : value_result->c = *(jchar*)tos_addr; break;
    case T_SHORT   : value_result->s = *(jshort*)tos_addr; break;
    case T_INT     : value_result->i = *(jint*)tos_addr; break;
    case T_LONG    : value_result->j = *(jlong*)tos_addr; break;
    case T_FLOAT   : value_result->f = *(jfloat*)tos_addr; break;
    case T_DOUBLE  : value_result->d = *(jdouble*)tos_addr; break;
    case T_VOID    : /* Nothing to do */ break;
    default        : ShouldNotReachHere();
  }

  return type;
}

#ifndef PRODUCT

void frame::describe_pd(FrameValues& values, int frame_no) {
  if (is_interpreted_frame()) {
#ifdef CC_INTERP
    interpreterState istate = get_interpreterState();
    values.describe(frame_no, (intptr_t*)istate, "istate");
    values.describe(frame_no, (intptr_t*)&(istate->_thread), " thread");
    values.describe(frame_no, (intptr_t*)&(istate->_bcp), " bcp");
    values.describe(frame_no, (intptr_t*)&(istate->_locals), " locals");
    values.describe(frame_no, (intptr_t*)&(istate->_constants), " constants");
    values.describe(frame_no, (intptr_t*)&(istate->_method), err_msg(" method = %s", istate->_method->name_and_sig_as_C_string()));
    values.describe(frame_no, (intptr_t*)&(istate->_mdx), " mdx");
    values.describe(frame_no, (intptr_t*)&(istate->_stack), " stack");
    values.describe(frame_no, (intptr_t*)&(istate->_msg), err_msg(" msg = %s", BytecodeInterpreter::C_msg(istate->_msg)));
    values.describe(frame_no, (intptr_t*)&(istate->_result), " result");
    values.describe(frame_no, (intptr_t*)&(istate->_prev_link), " prev_link");
    values.describe(frame_no, (intptr_t*)&(istate->_oop_temp), " oop_temp");
    values.describe(frame_no, (intptr_t*)&(istate->_stack_base), " stack_base");
    values.describe(frame_no, (intptr_t*)&(istate->_stack_limit), " stack_limit");
    values.describe(frame_no, (intptr_t*)&(istate->_monitor_base), " monitor_base");
    values.describe(frame_no, (intptr_t*)&(istate->_frame_bottom), " frame_bottom");
    values.describe(frame_no, (intptr_t*)&(istate->_last_Java_pc), " last_Java_pc");
    values.describe(frame_no, (intptr_t*)&(istate->_last_Java_fp), " last_Java_fp");
    values.describe(frame_no, (intptr_t*)&(istate->_last_Java_sp), " last_Java_sp");
    values.describe(frame_no, (intptr_t*)&(istate->_self_link), " self_link");
    values.describe(frame_no, (intptr_t*)&(istate->_native_fresult), " native_fresult");
    values.describe(frame_no, (intptr_t*)&(istate->_native_lresult), " native_lresult");
#else
#define DESCRIBE_ADDRESS(name) \
  values.describe(frame_no, (intptr_t*) addr_at(name), #name);

    DESCRIBE_ADDRESS(interpreter_frame_saved_SP_index); 
    DESCRIBE_ADDRESS(interpreter_frame_Rbcp_index);        
    DESCRIBE_ADDRESS(interpreter_frame_Rmethod_index);     
    DESCRIBE_ADDRESS(interpreter_frame_Rlocals_index);     
    DESCRIBE_ADDRESS(interpreter_frame_Rmonitors_index);   
    DESCRIBE_ADDRESS(interpreter_frame_RcpoolCache_index); 
    DESCRIBE_ADDRESS(interpreter_frame_Rmdx_index);        
    DESCRIBE_ADDRESS(interpreter_frame_Resp_index);        
    DESCRIBE_ADDRESS(interpreter_frame_oop_temp_offset);   
    DESCRIBE_ADDRESS(interpreter_frame_result_temp);       
#endif
  }
}
#endif


intptr_t* frame::interpreter_frame_tos_at(jint offset) const {
  int index = (Interpreter::expr_offset_in_bytes(offset)/wordSize) - 1;
  return &interpreter_frame_tos_address()[index];
}

intptr_t *frame::initial_deoptimization_info() {
  // unused... but returns fp() to minimize changes introduced by 7087445
  return fp();
}

#ifndef PRODUCT
// This is a generic constructor which is only used by pns() in debug.cpp.
frame::frame(void* sp, void* fp, void* pc) : _sp((intptr_t*)sp), _fp((intptr_t*)fp), _unextended_sp((intptr_t*)sp) {
  find_codeblob_and_deopt_state();
}
#endif
