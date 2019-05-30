/*
 * Copyright (c) 1997, 2015, Oracle and/or its affiliates. All rights reserved.
 * Copyright 2012, 2015 SAP AG. All rights reserved.
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
#include "asm/macroAssembler.inline.hpp"
#include "interpreter/bytecodeHistogram.hpp"
#include "interpreter/interpreter.hpp"
#include "interpreter/interpreterGenerator.hpp"
#include "interpreter/interpreterRuntime.hpp"
#include "interpreter/templateTable.hpp"
#include "oops/arrayOop.hpp"
#include "oops/methodData.hpp"
#include "oops/method.hpp"
#include "oops/oop.inline.hpp"
#include "prims/jvmtiExport.hpp"
#include "prims/jvmtiThreadState.hpp"
#include "prims/methodHandles.hpp"
#include "runtime/arguments.hpp"
#include "runtime/deoptimization.hpp"
#include "runtime/frame.inline.hpp"
#include "runtime/sharedRuntime.hpp"
#include "runtime/stubRoutines.hpp"
#include "runtime/synchronizer.hpp"
#include "runtime/timer.hpp"
#include "runtime/vframeArray.hpp"
#include "utilities/debug.hpp"
#ifdef COMPILER1
#include "c1/c1_Runtime1.hpp"
#endif

#define __ _masm->

#ifdef PRODUCT
#define BLOCK_COMMENT(str) // nothing
#else
#define BLOCK_COMMENT(str) __ block_comment(str)
#endif

#define BIND(label) bind(label); BLOCK_COMMENT(#label ":")

int AbstractInterpreter::BasicType_as_index(BasicType type) {
  int i = 0;
  switch (type) {
    case T_BOOLEAN: i = 0; break;
    case T_CHAR   : i = 1; break;
    case T_BYTE   : i = 2; break;
    case T_SHORT  : i = 3; break;
    case T_INT    : i = 4; break;
    case T_LONG   : i = 5; break;
    case T_VOID   : i = 6; break;
    case T_FLOAT  : i = 7; break;
    case T_DOUBLE : i = 8; break;
    case T_OBJECT : i = 9; break;
    case T_ARRAY  : i = 9; break;
    default       : ShouldNotReachHere();
  }
  assert(0 <= i && i < AbstractInterpreter::number_of_result_handlers, "index out of bounds");
  return i;
}

address AbstractInterpreterGenerator::generate_slow_signature_handler() {
  // TODO: Implement
  address entry = __ pc();

  printf("INIT_LOG: generate_slow_signature_handler\n\n");

  __ unimplemented("slow signature handler not implemented");

  /* TILE CODE but very useful to us, just hard to think about stack frames right now...
  // Set last_java_frame to the current native method frame - interpreter
  // register state has been saved.  The prior frame to this might be either
  // an interpreter frame, or a compiler frame with C2I adapter.
  __ set_last_Java_frame(X2_SP, X8_FP, X1_RA);

  // We have to move the arguments from the callee's expression stack to the
  // argument registers and the memory argument slots already allocated in 
  // the native call frame.  Here, we extend the stack enough space to hold
  // copies of the register arguments, and store a pointer to the memory
  // arg slots in the first slot of this extended space.   The runtime (C++)
  // slow_signature_handler will copy the args into these two areas, performing
  // alignment as necessary, and we'll then pop the stack extension contents 
  // into the arg registers and clean up. The extension size includes space 
  // for an FP/LR pair for our call out, plus a slot to save our LR in (which
  // we can't put @SP at entry, since the outer routine has already set up
  // the frame for the native call.)
  // The result handler address is left in Rret0.

  int extension_size = wordSize * (Argument::n_register_parameters +
                                   frame::frame_min_size + 1);

  __ addi  (Tint3, SP, frame::frame_min_size * wordSize);
  __ addi  (Tint0, SP, -wordSize);                   eob;
  __ st_ptr(Tint0, LR);
  __ addi  (Tint0, SP, -extension_size);
  __ addi  (Tint1, SP, frame::frame_FP_offset-extension_size);      eob;
  __ st_ptr(Tint1, FP);
  __ move  (SP, Tint0);
  __ addi  (Tint2, Tint0, frame::frame_min_size * wordSize); eob;
  __ st_ptr(Tint2, Tint3);
  __ move  (Rarg0, Gthread);
  __ move  (Rarg1, X23_method);                   eob;
  __ move  (Rarg2, X22_locals);
  __ move  (Rarg3, Tint2);                           eob;
 
  __ call_VM(CAST_FROM_FN_PTR(address, InterpreterRuntime::slow_signature_handler));
  __ reset_last_Java_frame();

  // Result handler is returned in Rret0 (which == Rarg0). We will not
  // disturb it.  Note that we skip Rarg0 when copying - that will be
  // used for the thread argument (and slow_signature_handler knows it.)
  assert(Argument::n_register_parameters == 8, "Adjust slow signature handler");

  __ addi  (Tint0, SP, frame::frame_memory_args_word_sp_offset +wordSize); eob;
  __ ld_ptr(Rarg1, Tint0);
  __ addi  (Tint0, Tint0, wordSize);       eob;
  __ ld_ptr(Rarg2, Tint0);
  __ addi  (Tint0, Tint0, wordSize);       eob;
  __ ld_ptr(Rarg3, Tint0);
  __ addi  (Tint0, Tint0, wordSize);       eob;
  __ ld_ptr(Rarg4, Tint0);
  __ addi  (Tint0, Tint0, wordSize);       eob;
  __ ld_ptr(Rarg5, Tint0);
  __ addi  (Tint0, Tint0, wordSize);       eob;
  __ ld_ptr(Rarg6, Tint0);
  __ addi  (Tint0, Tint0, wordSize);       eob;
  __ ld_ptr(Rarg7, Tint0);
  __ addi  (Tint0, Tint0, wordSize);       eob;
  __ ld_ptr(Rarg8, Tint0);
  __ addi  (Tint0, Tint0, wordSize);       eob;
  __ ld_ptr(Rarg9, Tint0);
  __ addi  (Tint0, Tint0, wordSize);       eob;
  __ ld_ptr(LR, Tint0);
  __ addi  (SP, Tint0, wordSize);          eob;
  __ ret   (LR);                           eob;
  */

  return entry;
}

address AbstractInterpreterGenerator::generate_result_handler_for(BasicType type) {
  //
  // Registers alive
  //   R10_RET0
  //   X1_RA
  //
  // Registers updated
  //   X10_RET0
  //

  Label done;
  address entry = __ pc();

  switch (type) {
  case T_BOOLEAN:
    // convert !=0 to 1
    __ beq(X10_RET0, XZERO, done);
    __ addi(X10_RET0, XZERO, 1);
    break;
  case T_BYTE:
     // sign extend 8 bits
     __ slli(X10_RET0, X10_RET0, 24);
     __ addiw(X10_RET0, X10_RET0, 0);
     __ srai(X10_RET0, X10_RET0, 24);
     break;
  case T_CHAR:
     // zero extend 16 bits
     __ slli(X10_RET0, X10_RET0, 48);
     __ srli(X10_RET0, X10_RET0, 48);
     break;
  case T_SHORT:
     // sign extend 16 bits
     __ slli(X10_RET0, X10_RET0, 16);
     __ addiw(X10_RET0, X10_RET0, 0);
     __ srai(X10_RET0, X10_RET0, 16);
     break;
  case T_INT:
     // sign extend 32 bits
     __ addiw(X10_RET0, X10_RET0, 0);
     break;
  case T_LONG:
     break;
  case T_OBJECT:
    // unbox result if not null
    __ beq(X10_RET0, XZERO, done);
    __ ld(X10_RET0, 0, X10_RET0);
    __ verify_oop(X10_RET0);
    break;
  case T_FLOAT:
     break;
  case T_DOUBLE:
     break;
  case T_VOID:
     break;
  default: ShouldNotReachHere();
  }

  //TODO: Do I need to do restoration here? Where does PPC do restoration?

  __ BIND(done);
  __ jr(X1_RA);

  return entry;
}

// Abstract method entry.
//
address InterpreterGenerator::generate_abstract_entry(void) {
  address entry = __ pc();
  __ unimplemented(__func__);
  return entry;
  /*
  address entry = __ pc();

  //
  // Registers alive
  //   R16_thread     - JavaThread*
  //   R19_method     - callee's method (method to be invoked)
  //   R1_SP          - SP prepared such that caller's outgoing args are near top
  //   LR             - return address to caller
  //
  // Stack layout at this point:
  //
  //   0       [TOP_IJAVA_FRAME_ABI]         <-- R1_SP
  //           alignment (optional)
  //           [outgoing Java arguments]
  //           ...
  //   PARENT  [PARENT_IJAVA_FRAME_ABI]
  //            ...
  //

  // Can't use call_VM here because we have not set up a new
  // interpreter state. Make the call to the vm and make it look like
  // our caller set up the JavaFrameAnchor.
  __ set_top_ijava_frame_at_SP_as_last_Java_frame(R1_SP, R12_scratch2*/ /*tmp*/ /*);

  // Push a new C frame and save LR.
  __ save_LR_CR(R0);
  __ push_frame_reg_args(0, R11_scratch1);

  // This is not a leaf but we have a JavaFrameAnchor now and we will
  // check (create) exceptions afterward so this is ok.
  __ call_VM_leaf(CAST_FROM_FN_PTR(address, InterpreterRuntime::throw_AbstractMethodError),
                  R16_thread);

  // Pop the C frame and restore LR.
  __ pop_frame();
  __ restore_LR_CR(R0);

  // Reset JavaFrameAnchor from call_VM_leaf above.
  __ reset_last_Java_frame();

#ifdef CC_INTERP
  // Return to frame manager, it will handle the pending exception.
  __ blr();
#else
  // We don't know our caller, so jump to the general forward exception stub,
  // which will also pop our full frame off. Satisfy the interface of
  // SharedRuntime::generate_forward_exception()
  __ load_const_optimized(R11_scratch1, StubRoutines::forward_exception_entry(), R0);
  __ mtctr(R11_scratch1);
  __ bctr();
#endif

  return entry;
  */
}

// Call an accessor method (assuming it is resolved, otherwise drop into
// vanilla (slow path) entry.
address InterpreterGenerator::generate_accessor_entry(void) {
  address entry = __ pc();
  __ unimplemented(__func__);
  return entry;
  /*
  if (!UseFastAccessorMethods && (!FLAG_IS_ERGO(UseFastAccessorMethods))) {
    return NULL;
  }

  Label Lslow_path, Lacquire;

  const Register
         Rclass_or_obj = R3_ARG1,
         Rconst_method = R4_ARG2,
         Rcodes        = Rconst_method,
         Rcpool_cache  = R5_ARG3,
         Rscratch      = R11_scratch1,
         Rjvmti_mode   = Rscratch,
         Roffset       = R12_scratch2,
         Rflags        = R6_ARG4,
         Rbtable       = R7_ARG5;

  static address branch_table[number_of_states];

  address entry = __ pc();

  // Check for safepoint:
  // Ditch this, real man don't need safepoint checks.

  // Also check for JVMTI mode
  // Check for null obj, take slow path if so.
  __ ld(Rclass_or_obj, Interpreter::stackElementSize, CC_INTERP_ONLY(R17_tos) NOT_CC_INTERP(R15_esp));
  __ lwz(Rjvmti_mode, thread_(interp_only_mode));
  __ cmpdi(CCR1, Rclass_or_obj, 0);
  __ cmpwi(CCR0, Rjvmti_mode, 0);
  __ crorc(*/ /*CCR0 eq*/ /*2, */ /*CCR1 eq*/ /*4+2, */ /*CCR0 eq*/ /*2);
  __ beq(CCR0, Lslow_path); // this==null or jvmti_mode!=0

  // Do 2 things in parallel:
  // 1. Load the index out of the first instruction word, which looks like this:
  //    <0x2a><0xb4><index (2 byte, native endianess)>.
  // 2. Load constant pool cache base.
  __ ld(Rconst_method, in_bytes(Method::const_offset()), R19_method);
  __ ld(Rcpool_cache, in_bytes(ConstMethod::constants_offset()), Rconst_method);

  __ lhz(Rcodes, in_bytes(ConstMethod::codes_offset()) + 2, Rconst_method); // Lower half of 32 bit field.
  __ ld(Rcpool_cache, ConstantPool::cache_offset_in_bytes(), Rcpool_cache);

  // Get the const pool entry by means of <index>.
  const int codes_shift = exact_log2(in_words(ConstantPoolCacheEntry::size()) * BytesPerWord);
  __ slwi(Rscratch, Rcodes, codes_shift); // (codes&0xFFFF)<<codes_shift
  __ add(Rcpool_cache, Rscratch, Rcpool_cache);

  // Check if cpool cache entry is resolved.
  // We are resolved if the indices offset contains the current bytecode.
  ByteSize cp_base_offset = ConstantPoolCache::base_offset();
  // Big Endian:
  __ lbz(Rscratch, in_bytes(cp_base_offset) + in_bytes(ConstantPoolCacheEntry::indices_offset()) + 7 - 2, Rcpool_cache);
  __ cmpwi(CCR0, Rscratch, Bytecodes::_getfield);
  __ bne(CCR0, Lslow_path);
  __ isync(); // Order succeeding loads wrt. load of _indices field from cpool_cache.

  // Finally, start loading the value: Get cp cache entry into regs.
  __ ld(Rflags, in_bytes(cp_base_offset) + in_bytes(ConstantPoolCacheEntry::flags_offset()), Rcpool_cache);
  __ ld(Roffset, in_bytes(cp_base_offset) + in_bytes(ConstantPoolCacheEntry::f2_offset()), Rcpool_cache);

  // Following code is from templateTable::getfield_or_static
  // Load pointer to branch table
  __ load_const_optimized(Rbtable, (address)branch_table, Rscratch);

  // Get volatile flag
  __ rldicl(Rscratch, Rflags, 64-ConstantPoolCacheEntry::is_volatile_shift, 63); // extract volatile bit
  // note: sync is needed before volatile load on RISCV64

  // Check field type
  __ rldicl(Rflags, Rflags, 64-ConstantPoolCacheEntry::tos_state_shift, 64-ConstantPoolCacheEntry::tos_state_bits);

#ifdef ASSERT
  Label LFlagInvalid;
  __ cmpldi(CCR0, Rflags, number_of_states);
  __ bge(CCR0, LFlagInvalid);

  __ ld(R9_ARG7, 0, R1_SP);
  __ ld(R10_ARG8, 0, R21_sender_SP);
  __ cmpd(CCR0, R9_ARG7, R10_ARG8);
  __ asm_assert_eq("backlink", 0x543);
#endif // ASSERT
  __ mr(R1_SP, R21_sender_SP); // Cut the stack back to where the caller started.

  // Load from branch table and dispatch (volatile case: one instruction ahead)
  __ sldi(Rflags, Rflags, LogBytesPerWord);
  __ cmpwi(CCR6, Rscratch, 1); // volatile?
  if (support_IRIW_for_not_multiple_copy_atomic_cpu) {
    __ sldi(Rscratch, Rscratch, exact_log2(BytesPerInstWord)); // volatile ? size of 1 instruction : 0
  }
  __ ldx(Rbtable, Rbtable, Rflags);

  if (support_IRIW_for_not_multiple_copy_atomic_cpu) {
    __ subf(Rbtable, Rscratch, Rbtable); // point to volatile/non-volatile entry point
  }
  __ mtctr(Rbtable);
  __ bctr();

#ifdef ASSERT
  __ bind(LFlagInvalid);
  __ stop("got invalid flag", 0x6541);

  bool all_uninitialized = true,
       all_initialized   = true;
  for (int i = 0; i<number_of_states; ++i) {
    all_uninitialized = all_uninitialized && (branch_table[i] == NULL);
    all_initialized   = all_initialized   && (branch_table[i] != NULL);
  }
  assert(all_uninitialized != all_initialized, "consistency"); // either or

  __ fence(); // volatile entry point (one instruction before non-volatile_entry point)
  if (branch_table[vtos] == 0) branch_table[vtos] = __ pc(); // non-volatile_entry point
  if (branch_table[dtos] == 0) branch_table[dtos] = __ pc(); // non-volatile_entry point
  if (branch_table[ftos] == 0) branch_table[ftos] = __ pc(); // non-volatile_entry point
  __ stop("unexpected type", 0x6551);
#endif

  if (branch_table[itos] == 0) { // generate only once
    __ align(32, 28, 28); // align load
    __ fence(); // volatile entry point (one instruction before non-volatile_entry point)
    branch_table[itos] = __ pc(); // non-volatile_entry point
    __ lwax(X10_RET0, Rclass_or_obj, Roffset);
    __ beq(CCR6, Lacquire);
    __ blr();
  }

  if (branch_table[ltos] == 0) { // generate only once
    __ align(32, 28, 28); // align load
    __ fence(); // volatile entry point (one instruction before non-volatile_entry point)
    branch_table[ltos] = __ pc(); // non-volatile_entry point
    __ ldx(X10_RET0, Rclass_or_obj, Roffset);
    __ beq(CCR6, Lacquire);
    __ blr();
  }

  if (branch_table[btos] == 0) { // generate only once
    __ align(32, 28, 28); // align load
    __ fence(); // volatile entry point (one instruction before non-volatile_entry point)
    branch_table[btos] = __ pc(); // non-volatile_entry point
    __ lbzx(X10_RET0, Rclass_or_obj, Roffset);
    __ extsb(X10_RET0, X10_RET0);
    __ beq(CCR6, Lacquire);
    __ blr();
  }

  if (branch_table[ctos] == 0) { // generate only once
    __ align(32, 28, 28); // align load
    __ fence(); // volatile entry point (one instruction before non-volatile_entry point)
    branch_table[ctos] = __ pc(); // non-volatile_entry point
    __ lhzx(X10_RET0, Rclass_or_obj, Roffset);
    __ beq(CCR6, Lacquire);
    __ blr();
  }

  if (branch_table[stos] == 0) { // generate only once
    __ align(32, 28, 28); // align load
    __ fence(); // volatile entry point (one instruction before non-volatile_entry point)
    branch_table[stos] = __ pc(); // non-volatile_entry point
    __ lhax(X10_RET0, Rclass_or_obj, Roffset);
    __ beq(CCR6, Lacquire);
    __ blr();
  }

  if (branch_table[atos] == 0) { // generate only once
    __ align(32, 28, 28); // align load
    __ fence(); // volatile entry point (one instruction before non-volatile_entry point)
    branch_table[atos] = __ pc(); // non-volatile_entry point
    __ load_heap_oop(X10_RET0, (RegisterOrConstant)Roffset, Rclass_or_obj);
    __ verify_oop(X10_RET0);
    //__ dcbt(X10_RET0); // prefetch
    __ beq(CCR6, Lacquire);
    __ blr();
  }

  __ align(32, 12);
  __ bind(Lacquire);
  __ twi_0(X10_RET0);
  __ isync(); // acquire
  __ blr();

#ifdef ASSERT
  for (int i = 0; i<number_of_states; ++i) {
    assert(branch_table[i], "accessor_entry initialization");
    //tty->print_cr("accessor_entry: branch_table[%d] = 0x%llx (opcode 0x%llx)", i, branch_table[i], *((unsigned int*)branch_table[i]));
  }
#endif

  __ bind(Lslow_path);
  __ branch_to_entry(Interpreter::entry_for_kind(Interpreter::zerolocals), Rscratch);
  __ flush();

  return entry;
  */
}

// Interpreter intrinsic for WeakReference.get().
// 1. Don't push a full blown frame and go on dispatching, but fetch the value
//    into R8 and return quickly
// 2. If G1 is active we *must* execute this intrinsic for corrrectness:
//    It contains a GC barrier which puts the reference into the satb buffer
//    to indicate that someone holds a strong reference to the object the
//    weak ref points to!
address InterpreterGenerator::generate_Reference_get_entry(void) {
  address entry = __ pc();
  __ unimplemented(__func__);
  return entry;
  /*
  // Code: _aload_0, _getfield, _areturn
  // parameter size = 1
  //
  // The code that gets generated by this routine is split into 2 parts:
  //    1. the "intrinsified" code for G1 (or any SATB based GC),
  //    2. the slow path - which is an expansion of the regular method entry.
  //
  // Notes:
  // * In the G1 code we do not check whether we need to block for
  //   a safepoint. If G1 is enabled then we must execute the specialized
  //   code for Reference.get (except when the Reference object is null)
  //   so that we can log the value in the referent field with an SATB
  //   update buffer.
  //   If the code for the getfield template is modified so that the
  //   G1 pre-barrier code is executed when the current method is
  //   Reference.get() then going through the normal method entry
  //   will be fine.
  // * The G1 code can, however, check the receiver object (the instance
  //   of java.lang.Reference) and jump to the slow path if null. If the
  //   Reference object is null then we obviously cannot fetch the referent
  //   and so we don't need to call the G1 pre-barrier. Thus we can use the
  //   regular method entry code to generate the NPE.
  //
  // This code is based on generate_accessor_enty.

  address entry = __ pc();

  const int referent_offset = java_lang_ref_Reference::referent_offset;
  guarantee(referent_offset > 0, "referent offset not initialized");

  if (UseG1GC) {
     Label slow_path;

    // Debugging not possible, so can't use __ skip_if_jvmti_mode(slow_path, GR31_SCRATCH);

    // In the G1 code we don't check if we need to reach a safepoint. We
    // continue and the thread will safepoint at the next bytecode dispatch.

    // If the receiver is null then it is OK to jump to the slow path.
    __ ld(X10_RET0, Interpreter::stackElementSize, CC_INTERP_ONLY(R17_tos) NOT_CC_INTERP(R15_esp)); // get receiver

    // Check if receiver == NULL and go the slow path.
    __ cmpdi(CCR0, X10_RET0, 0);
    __ beq(CCR0, slow_path);

    // Load the value of the referent field.
    __ load_heap_oop(X10_RET0, referent_offset, X10_RET0);

    // Generate the G1 pre-barrier code to log the value of
    // the referent field in an SATB buffer. Note with
    // these parameters the pre-barrier does not generate
    // the load of the previous value.

    // Restore caller sp for c2i case.
#ifdef ASSERT
      __ ld(R9_ARG7, 0, R1_SP);
      __ ld(R10_ARG8, 0, R21_sender_SP);
      __ cmpd(CCR0, R9_ARG7, R10_ARG8);
      __ asm_assert_eq("backlink", 0x544);
#endif // ASSERT
    __ mr(R1_SP, R21_sender_SP); // Cut the stack back to where the caller started.

    __ g1_write_barrier_pre(noreg,         // obj
                            noreg,         // offset
                            X10_RET0,        // pre_val
                            R11_scratch1,  // tmp
                            R12_scratch2,  // tmp
                            true);         // needs_frame

    __ blr();

    // Generate regular method entry.
    __ bind(slow_path);
    __ branch_to_entry(Interpreter::entry_for_kind(Interpreter::zerolocals), R11_scratch1);
    __ flush();

    return entry;
  } else {
    return generate_accessor_entry();
  }
  */
}

void Deoptimization::unwind_callee_save_values(frame* f, vframeArray* vframe_array) {
  ShouldNotReachHere();
  /*
  // This code is sort of the equivalent of C2IAdapter::setup_stack_frame back in
  // the days we had adapter frames. When we deoptimize a situation where a
  // compiled caller calls a compiled caller will have registers it expects
  // to survive the call to the callee. If we deoptimize the callee the only
  // way we can restore these registers is to have the oldest interpreter
  // frame that we create restore these values. That is what this routine
  // will accomplish.

  // At the moment we have modified c2 to not have any callee save registers
  // so this problem does not exist and this routine is just a place holder.

  assert(f->is_interpreted_frame(), "must be interpreted");
  */
}
