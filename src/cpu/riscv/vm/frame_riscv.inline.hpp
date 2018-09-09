/*
 * Copyright (c) 2000, 2013, Oracle and/or its affiliates. All rights reserved.
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

#ifndef CPU_RISCV_VM_FRAME_RISCV_INLINE_HPP
#define CPU_RISCV_VM_FRAME_RISCV_INLINE_HPP

#include "code/codeCache.hpp"

// Find codeblob and set deopt_state.
inline void frame::find_codeblob_and_deopt_state() {
  assert(_pc != NULL, "precondition: must have PC");

  _cb = CodeCache::find_blob(_pc);

  address original_pc = nmethod::get_deopt_original_pc(this);
  if (original_pc != NULL) {
    _pc = original_pc;
    _deopt_state = is_deoptimized;
  } else {
    _deopt_state = not_deoptimized;
  }

  assert(((uint64_t)_sp & (alignment_in_bytes-1)) == 0, "SP must be 16-byte aligned");
}

// Inline functions for Tile frames
//
//
// Constructors
//

inline frame::frame() {
  _pc = NULL;
  _sp = NULL;
  _unextended_sp = NULL;
  _fp = NULL;
  _cb = NULL;
  _deopt_state = unknown;
}

inline frame::frame(intptr_t* sp, intptr_t* fp, address pc) {
  _sp = sp;
  _unextended_sp = sp;
  _fp = fp;
  _pc = pc;
  find_codeblob_and_deopt_state();
}

inline frame::frame(intptr_t* sp, intptr_t* unextended_sp, 
                    intptr_t* fp, address pc) {
  _sp = sp;
  _unextended_sp = unextended_sp;
  _fp = fp;
  _pc = pc;
  find_codeblob_and_deopt_state();
}

inline frame::frame(intptr_t* sp, intptr_t* fp) {
  _sp = sp;
  _unextended_sp = sp;
  _fp = fp;
  _pc = (address)(fp[frame_ra_index]);
  find_codeblob_and_deopt_state();
}


//
// Frame IDs and comparators
//

// Return a unique id for this frame. The id must allow distinguishing both
// identity and younger/older relationship. NULL represents an invalid 
// (incomparable) frame.

inline intptr_t* frame::id(void) const { return unextended_sp(); }
inline intptr_t* frame::unextended_sp() const { return _unextended_sp; }

// Return true if the frame is younger (more recent activation) than the 
// frame represented by id
inline bool frame::is_younger(intptr_t* id) const { 
  assert(this->id() != NULL && id != NULL, "NULL frame id");
  return this->id() < id ; 
}

// Return true if the frame is older (less recent activation) than the frame 
// represented by id
inline bool frame::is_older(intptr_t* id) const { 
  assert(this->id() != NULL && id != NULL, "NULL frame id");
  return this->id() > id ; 
}

inline bool frame::equal(frame other) const {
  bool ret =  sp() == other.sp()
    && unextended_sp() == other.unextended_sp()
    && fp() == other.fp()
    && pc() == other.pc();
  assert(!ret || ret && cb() == other.cb() && 
         _deopt_state == other._deopt_state, "inconsistent construction");
  return ret;
}

//
// Standard frame references
//

inline intptr_t* frame::link() const { 
  return (intptr_t*) *(intptr_t **)addr_at(frame_fp_index); 
}
inline void frame::set_link(intptr_t* addr) { 
  *(intptr_t **)addr_at(frame_fp_index) = addr; 
}

inline address* frame::sender_pc_addr() const {
  return (address*) addr_at(frame_ra_index); 
}

inline address frame::sender_pc() const { return *sender_pc_addr(); }

inline intptr_t* frame::sender_sp() const  { return fp(); }

inline address* frame::saved_pc_addr() const {
  return (address*) sp_addr_at(frame_ra_index);
}

inline address frame::saved_pc() const { return *saved_pc_addr(); }

inline int frame::frame_size(RegisterMap* map) const {
  return sender_sp() - sp(); 
}

inline intptr_t* frame::real_fp() const {
  return fp();
}

inline intptr_t** frame::interpreter_frame_locals_addr() const {
  return (intptr_t**) addr_at(interpreter_frame_Rlocals_index);
}

inline intptr_t* frame::interpreter_frame_saved_sp() const {
  return *(intptr_t**) addr_at(interpreter_frame_saved_SP_index);
}

inline intptr_t* frame::interpreter_frame_bcx_addr() const {
  return (intptr_t*) addr_at(interpreter_frame_Rbcp_index);
}

inline intptr_t* frame::interpreter_frame_mdx_addr() const {
  return (intptr_t*) addr_at(interpreter_frame_Rmdx_index);
}

inline ConstantPoolCache** frame::interpreter_frame_cache_addr() const {
  return (ConstantPoolCache**) addr_at(interpreter_frame_RcpoolCache_index);
}

inline Method** frame::interpreter_frame_method_addr() const {
  return (Method**) addr_at(interpreter_frame_Rmethod_index);
}

// address of stack parameters
inline address* frame::native_param_addr(int idx) const {
  return (address*) addr_at(frame_args_index + idx);
}

inline oop* frame::interpreter_frame_temp_oop_addr() const {
  return (oop *) addr_at(interpreter_frame_oop_temp_offset);
}

//
// Java expression stack
//

inline jint frame::interpreter_frame_expression_stack_direction() { 
  return -1; 
}

inline intptr_t** frame::interpreter_frame_esp_addr() const {
  return (intptr_t**) addr_at(interpreter_frame_Resp_index);
}

// bottom(base) of the expression stack (highest address)
inline intptr_t* frame::interpreter_frame_expression_stack() const {
  return (intptr_t*)(*interpreter_frame_monitors_addr()) - 1;
}

// top of expression stack (lowest address)
inline intptr_t* frame::interpreter_frame_tos_address() const {
  return *interpreter_frame_esp_addr() + 1;
}

inline void frame::interpreter_frame_set_tos_address( intptr_t* x ) {
  *interpreter_frame_esp_addr() = x - 1;
}

//
// Monitor array elements
//

inline BasicObjectLock**  frame::interpreter_frame_monitors_addr() const {
  return (BasicObjectLock**) addr_at(interpreter_frame_Rmonitors_index);
}
 
// End is lower in memory than begin,and beginning element is oldest element
// The pointer is always one past the last monitor.

inline BasicObjectLock* frame::interpreter_frame_monitor_begin() const  {
  int rounded_reg_save_words = round_to(frame::interpreter_frame_local_words, 
                                        WordsPerLong);
  return (BasicObjectLock *)addr_at(-rounded_reg_save_words);
}

inline BasicObjectLock* frame::interpreter_frame_monitor_end() const  {
  return *interpreter_frame_monitors_addr();
}

#ifndef CC_INTERP
inline void frame::interpreter_frame_set_monitor_end(BasicObjectLock* monitors)
{
  *interpreter_frame_monitors_addr() = monitors;
}
#endif

inline int frame::interpreter_frame_monitor_size() {
  return round_to(BasicObjectLock::size(), WordsPerLong);
}


//
// Entry Frame functions
//
//
inline JavaCallWrapper** frame::entry_frame_call_wrapper_addr() const {
  return (JavaCallWrapper**)(addr_at(entry_frame_call_wrapper_addr_index));
}

//
// For frames for which the method returns an object pointer, access the
// slot reserved for saving the result. Used at safepoints to allow GC
// to find/fix the pointer.
//
inline oop  frame::saved_oop_result(RegisterMap* map) const {
  return *((oop*) map->location(X10->as_VMReg()));
}

inline void frame::set_saved_oop_result(RegisterMap* map, oop obj) {
  *((oop*) map->location(X10->as_VMReg())) = obj;
}

// Compiled Frame functions
// FIXME: RISCV:
// These functions are defined on one or the other of the other platforms, 
// but seemingly unused:

inline int frame::local_offset_for_compiler(int local_index, int nof_args, 
                                            int max_nof_locals, 
                                            int max_nof_monitors) {
  warning("unimplemented!\n");
}

inline int frame::monitor_offset_for_compiler(int local_index, int nof_args, 
                                              int max_nof_locals, 
                                              int max_nof_monitors) {
  warning("unimplemented!\n");
}

inline int frame::min_local_offset_for_compiler(int nof_args, 
                                                int max_nof_locals, 
                                                int max_nof_monitors) {
  warning("unimplemented!\n");
}

inline bool frame::volatile_across_calls(Register reg) {
  warning("unimplemented!\n");
}

inline int frame::pd_oop_map_offset_adjustment() const {
  warning("unimplemented!\n");
}

#endif // CPU_RISCV_VM_FRAME_RISCV_INLINE_HPP
