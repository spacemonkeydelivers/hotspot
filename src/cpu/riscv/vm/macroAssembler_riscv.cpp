/*
 * Copyright (c) 1997, 2014, Oracle and/or its affiliates. All rights reserved.
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
#include "asm/macroAssembler.inline.hpp"
#include "compiler/disassembler.hpp"
#include "gc_interface/collectedHeap.inline.hpp"
#include "interpreter/interpreter.hpp"
#include "memory/cardTableModRefBS.hpp"
#include "memory/resourceArea.hpp"
#include "prims/methodHandles.hpp"
#include "runtime/biasedLocking.hpp"
#include "runtime/interfaceSupport.hpp"
#include "runtime/objectMonitor.hpp"
#include "runtime/os.hpp"
#include "runtime/sharedRuntime.hpp"
#include "runtime/stubRoutines.hpp"
#include "utilities/macros.hpp"
#if INCLUDE_ALL_GCS
#include "gc_implementation/g1/g1CollectedHeap.inline.hpp"
#include "gc_implementation/g1/g1SATBCardTableModRefBS.hpp"
#include "gc_implementation/g1/heapRegion.hpp"
#endif // INCLUDE_ALL_GCS

#ifdef PRODUCT
#define BLOCK_COMMENT(str) // nothing
#else
#define BLOCK_COMMENT(str) block_comment(str)
#endif

#ifdef ASSERT
// On RISC, there's no benefit to verifying instruction boundaries.
bool AbstractAssembler::pd_check_instruction_mark() { return false; }
#endif

void MacroAssembler::align(int modulus, int max, int rem) {
  int padding = (rem + modulus - (offset() % modulus)) % modulus;
  if (padding > max) return;
  for (int c = (padding >> 2); c > 0; --c) { nop(); }
}

#ifdef _LP64
// Patch compressed oops or klass constants.
// Assembler sequence is
// 1) compressed oops:
//    lis  rx = const.hi
//    ori rx = rx | const.lo
// 2) compressed klass:
//    lis  rx = const.hi
//    clrldi rx = rx & 0xFFFFffff // clearMS32b, optional
//    ori rx = rx | const.lo
// Clrldi will be passed by.
int MacroAssembler::patch_set_narrow_oop(address a, address bound, narrowOop data) {
  Unimplemented();
  /*
  assert(UseCompressedOops, "Should only patch compressed oops");

  const address inst2_addr = a;
  const int inst2 = *(int *)inst2_addr;

  // The relocation points to the second instruction, the ori,
  // and the ori reads and writes the same register dst.
  const int dst = inv_rta_field(inst2);
  assert(is_ori(inst2) && inv_rs_field(inst2) == dst, "must be ori reading and writing dst");
  // Now, find the preceding addis which writes to dst.
  int inst1 = 0;
  address inst1_addr = inst2_addr - BytesPerInstWord;
  bool inst1_found = false;
  while (inst1_addr >= bound) {
    inst1 = *(int *)inst1_addr;
    if (is_lis(inst1) && inv_rs_field(inst1) == dst) { inst1_found = true; break; }
    inst1_addr -= BytesPerInstWord;
  }
  assert(inst1_found, "inst is not lis");

  int xc = (data >> 16) & 0xffff;
  int xd = (data >>  0) & 0xffff;

  set_imm((int *)inst1_addr, (short)(xc)); // see enc_load_con_narrow_hi/_lo
  set_imm((int *)inst2_addr,        (xd)); // unsigned int
  return (int)((intptr_t)inst2_addr - (intptr_t)inst1_addr);
  */
}

// Get compressed oop or klass constant.
narrowOop MacroAssembler::get_narrow_oop(address a, address bound) {
  Unimplemented();
  /*
  assert(UseCompressedOops, "Should only patch compressed oops");

  const address inst2_addr = a;
  const int inst2 = *(int *)inst2_addr;

  // The relocation points to the second instruction, the ori,
  // and the ori reads and writes the same register dst.
  const int dst = inv_rta_field(inst2);
  assert(is_ori(inst2) && inv_rs_field(inst2) == dst, "must be ori reading and writing dst");
  // Now, find the preceding lis which writes to dst.
  int inst1 = 0;
  address inst1_addr = inst2_addr - BytesPerInstWord;
  bool inst1_found = false;

  while (inst1_addr >= bound) {
    inst1 = *(int *) inst1_addr;
    if (is_lis(inst1) && inv_rs_field(inst1) == dst) { inst1_found = true; break;}
    inst1_addr -= BytesPerInstWord;
  }
  assert(inst1_found, "inst is not lis");

  uint xl = ((unsigned int) (get_imm(inst2_addr, 0) & 0xffff));
  uint xh = (((get_imm(inst1_addr, 0)) & 0xffff) << 16);

  return (int) (xl | xh);
  */
}
#endif // _LP64

// Get the constant from a `load_const_patchable' sequence.
intptr_t MacroAssembler::get_const_patchable(address a) {
  assert(is_load_const_at(a), "not a load of a constant");
  const int *p = (const int*) a;
  intptr_t x = 0;
  if (!is_lui(*p))
    ShouldNotReachHere();
  x += get_imm20(p[0]);
  for (int i = 1; i < 8; i += 2) {
    x <<= 11;
    x |= get_imm12(p[i]);
  }
  return x;
}

// Patch the 64 bit constant of a `load_const' sequence. This is a low
// level procedure. It neither flushes the instruction cache nor is it
// mt safe.
void MacroAssembler::patch_const(address a, long x) {
  Unimplemented();
  /*
  assert(is_load_const_at(a), "not a load of a constant");
  int *p = (int*) a;
  if (is_ori(*(p+1))) {
    set_imm(0 + p, (x >> 48) & 0xffff);
    set_imm(1 + p, (x >> 32) & 0xffff);
    set_imm(3 + p, (x >> 16) & 0xffff);
    set_imm(4 + p, x & 0xffff);
  } else if (is_lis(*(p+1))) {
    set_imm(0 + p, (x >> 48) & 0xffff);
    set_imm(2 + p, (x >> 32) & 0xffff);
    set_imm(1 + p, (x >> 16) & 0xffff);
    set_imm(3 + p, x & 0xffff);
  } else {
    ShouldNotReachHere();
  }
  */
}

AddressLiteral MacroAssembler::allocate_metadata_address(Metadata* obj) {
  assert(oop_recorder() != NULL, "this assembler needs a Recorder");
  int index = oop_recorder()->allocate_metadata_index(obj);
  RelocationHolder rspec = metadata_Relocation::spec(index);
  return AddressLiteral((address)obj, rspec);
}

AddressLiteral MacroAssembler::constant_metadata_address(Metadata* obj) {
  assert(oop_recorder() != NULL, "this assembler needs a Recorder");
  int index = oop_recorder()->find_index(obj);
  RelocationHolder rspec = metadata_Relocation::spec(index);
  return AddressLiteral((address)obj, rspec);
}

AddressLiteral MacroAssembler::allocate_oop_address(jobject obj) {
  assert(oop_recorder() != NULL, "this assembler needs an OopRecorder");
  int oop_index = oop_recorder()->allocate_oop_index(obj);
  return AddressLiteral(address(obj), oop_Relocation::spec(oop_index));
}

AddressLiteral MacroAssembler::constant_oop_address(jobject obj) {
  assert(oop_recorder() != NULL, "this assembler needs an OopRecorder");
  int oop_index = oop_recorder()->find_index(obj);
  return AddressLiteral(address(obj), oop_Relocation::spec(oop_index));
}

RegisterOrConstant MacroAssembler::delayed_value_impl(intptr_t* delayed_value_addr,
                                                      Register tmp, int offset) {
  Unimplemented();
  /*
  intptr_t value = *delayed_value_addr;
  if (value != 0) {
    return RegisterOrConstant(value + offset);
  }

  // Load indirectly to solve generation ordering problem.
  // static address, no relocation
  int simm16_offset = load_const_optimized(tmp, delayed_value_addr, noreg, true);
  ld(tmp, simm16_offset, tmp); // must be aligned ((xa & 3) == 0)

  if (offset != 0) {
    addi(tmp, tmp, offset);
  }
*/

  return RegisterOrConstant(tmp);
}

#ifndef PRODUCT
void MacroAssembler::pd_print_patched_instruction(address branch) {
  Unimplemented(); // TODO: RISCV port
}
#endif // ndef PRODUCT

// Emit a NOT mt-safe patchable 64 bit absolute call/jump.
void MacroAssembler::jump64_patchable(address dest, relocInfo::relocType rt, bool and_link) {
  // relocate here
  if (rt != relocInfo::none) {
    relocate(rt);
  }

  // generates constant for a patchable jump
  load_const_patchable(X30, (intptr_t) dest);
  if (and_link)
    jalr(X1_RA, X30, 0);
  else
    jr(X30);
}

// Identify a bxx64_patchable instruction.
bool MacroAssembler::is_jump64_patchable_at(address instruction_addr, bool and_link) {
  unsigned int* instr = (unsigned int*) instruction_addr;
  unsigned int jinstr = instr[jump64_patchable_instruction_count-1];
  return (is_jalr(jinstr) && (
              (!and_link && get_rd(jinstr) == 0) || 
              ( and_link && get_rd(jinstr) == X1_RA->encoding()) 
                             )  
         );
}

// Set dest address of a bxx64_patchable instruction.
void MacroAssembler::set_dest_of_jump64_patchable_at(address instruction_addr, address dest, bool and_link) {
  ResourceMark rm;
  int code_size = MacroAssembler::jump64_patchable_size;
  CodeBuffer buf(instruction_addr, code_size);
  MacroAssembler masm(&buf);
  masm.jump64_patchable(dest, relocInfo::none, and_link);
  ICache::riscv_flush_icache();
}

// Get dest address of a jump64_patchable instruction.
address MacroAssembler::get_dest_of_jump64_patchable_at(address instruction_addr, bool and_link) {
  if (is_jump64_patchable_at(instruction_addr, and_link))
    return (address) (unsigned long) get_const_patchable(instruction_addr);

  // If that's not a patchable jump, we messed up.
  ShouldNotReachHere();
  return NULL;
}

// Uses ordering which corresponds to ABI:
//    _savegpr0_14:  sd  r14,-144(r1)
//    _savegpr0_15:  sd  r15,-136(r1)
//    _savegpr0_16:  sd  r16,-128(r1)
void MacroAssembler::save_nonvolatile_gprs(Register dst, int offset) {
  sd( X8, offset, dst);    offset += 8;
  sd( X9, offset, dst);    offset += 8;
  sd(X18, offset, dst);    offset += 8;
  sd(X19, offset, dst);    offset += 8;
  sd(X20, offset, dst);    offset += 8;
  sd(X21, offset, dst);    offset += 8;
  sd(X22, offset, dst);    offset += 8;
  sd(X23, offset, dst);    offset += 8;
  sd(X24, offset, dst);    offset += 8;
  sd(X25, offset, dst);    offset += 8;
  sd(X26, offset, dst);    offset += 8;
  sd(X27, offset, dst);    offset += 8;

  fsd( F8, offset, dst);   offset += 8;
  fsd( F9, offset, dst);   offset += 8;
  fsd(F18, offset, dst);   offset += 8;
  fsd(F19, offset, dst);   offset += 8;
  fsd(F20, offset, dst);   offset += 8;
  fsd(F21, offset, dst);   offset += 8;
  fsd(F22, offset, dst);   offset += 8;
  fsd(F23, offset, dst);   offset += 8;
  fsd(F24, offset, dst);   offset += 8;
  fsd(F25, offset, dst);   offset += 8;
  fsd(F26, offset, dst);   offset += 8;
  fsd(F27, offset, dst);
}

// Uses ordering which corresponds to ABI:
//    _restgpr0_14:  ld   r14,-144(r1)
//    _restgpr0_15:  ld   r15,-136(r1)
//    _restgpr0_16:  ld   r16,-128(r1)
void MacroAssembler::restore_nonvolatile_gprs(Register src, int offset) {
  ld( X8, offset, src);    offset += 8;
  ld( X9, offset, src);    offset += 8;
  ld(X18, offset, src);    offset += 8;
  ld(X19, offset, src);    offset += 8;
  ld(X20, offset, src);    offset += 8;
  ld(X21, offset, src);    offset += 8;
  ld(X22, offset, src);    offset += 8;
  ld(X23, offset, src);    offset += 8;
  ld(X24, offset, src);    offset += 8;
  ld(X25, offset, src);    offset += 8;
  ld(X26, offset, src);    offset += 8;
  ld(X27, offset, src);    offset += 8;

  fld( F8, offset, src);   offset += 8;
  fld( F9, offset, src);   offset += 8;
  fld(F18, offset, src);   offset += 8;
  fld(F19, offset, src);   offset += 8;
  fld(F20, offset, src);   offset += 8;
  fld(F21, offset, src);   offset += 8;
  fld(F22, offset, src);   offset += 8;
  fld(F23, offset, src);   offset += 8;
  fld(F24, offset, src);   offset += 8;
  fld(F25, offset, src);   offset += 8;
  fld(F26, offset, src);   offset += 8;
  fld(F27, offset, src);
}

// For verify_oops.
void MacroAssembler::save_volatile_gprs(Register dst, int offset) {
  Unimplemented();
  /*
  sd(X5,  offset, dst);   offset += 8;
  sd(X6,  offset, dst);   offset += 8;
  sd(X7,  offset, dst);   offset += 8;
  sd(X10, offset, dst);   offset += 8;
  sd(X11, offset, dst);   offset += 8;
  sd(X12, offset, dst);   offset += 8;
  sd(X13, offset, dst);   offset += 8;
  sd(X14, offset, dst);   offset += 8;
  sd(X15, offset, dst);   offset += 8;
  sd(X16, offset, dst);   offset += 8;
  sd(X17, offset, dst);   offset += 8;
  sd(X29, offset, dst);   offset += 8;
  sd(X30, offset, dst);   offset += 8;
  sd(X31, offset, dst);
  */
}

// For verify_oops.
void MacroAssembler::restore_volatile_gprs(Register src, int offset) {
  Unimplemented();
  /*
  ld(X5,  offset, dst);   offset += 8;
  ld(X6,  offset, dst);   offset += 8;
  ld(X7,  offset, dst);   offset += 8;
  ld(X10, offset, dst);   offset += 8;
  ld(X11, offset, dst);   offset += 8;
  ld(X12, offset, dst);   offset += 8;
  ld(X13, offset, dst);   offset += 8;
  ld(X14, offset, dst);   offset += 8;
  ld(X15, offset, dst);   offset += 8;
  ld(X16, offset, dst);   offset += 8;
  ld(X17, offset, dst);   offset += 8;
  ld(X29, offset, dst);   offset += 8;
  ld(X30, offset, dst);   offset += 8;
  ld(X31, offset, dst);
  */
}

void MacroAssembler::resize_frame(Register offset, Register tmp) {
#if 0
#ifdef ASSERT
  assert_different_registers(offset, tmp, X2_SP);
  andi_(tmp, offset, frame::alignment_in_bytes-1);
  asm_assert_eq("resize_frame: unaligned", 0x204);
#endif
#endif // #if 0

  // tmp <- *(SP)
  ld(tmp, 9 * frame::frame_elem_size, X2_SP);
  // addr <- SP + offset;
  add(X28_T3, X2_SP, offset);
  // *(addr) <- tmp;
  sd(tmp, 0, X28_T3);
  // SP <- addr
  mv(X2_SP, X28_T3);
//  sdux(tmp, X2_SP, offset);
}

void MacroAssembler::resize_frame(int offset, Register tmp) {
  Unimplemented();
  /*
  assert(is_simm(offset, 16), "too big an offset");
  assert_different_registers(tmp, X2_SP);
  assert((offset & (frame::alignment_in_bytes-1))==0, "resize_frame: unaligned");
  // tmp <- *(SP)
  ld(tmp, _abi(callers_sp), X2_SP);
  // addr <- SP + offset;
  // *(addr) <- tmp;
  // SP <- addr
  sdu(tmp, offset, X2_SP);
  */
}

void MacroAssembler::resize_frame_absolute(Register addr, Register tmp1, Register tmp2) {
  // (addr == tmp1) || (addr == tmp2) is allowed here!
  assert(tmp1 != tmp2, "must be distinct");

  // compute offset w.r.t. current stack pointer
  // tmp_1 <- addr - SP (!)
  sub(tmp1, addr, X2_SP);

  // atomically update SP keeping back link.
  resize_frame(tmp1/* offset */, tmp2/* tmp */);
}

void MacroAssembler::push_frame(Register bytes, Register tmp) {
#ifdef ASSERT
  assert(bytes != tmp, "r0 not allowed here");
  andi(tmp, bytes, frame::alignment_in_bytes-1);
  asm_assert_eq(tmp, XZERO, "push_frame(Reg, Reg): unaligned", 0x203);
#endif
  sd(X1_RA, frame::frame_ra_offset, X2_SP);
  sd(X8_FP, frame::frame_fp_offset, X2_SP);
  mv(X8_FP, X2_SP);
  sub(X2_SP, X2_SP, bytes);
}

// Push a frame of size `bytes'.
void MacroAssembler::push_frame(unsigned int bytes, Register tmp) {
  long offset = align_addr(bytes, frame::alignment_in_bytes);
  sd(X1_RA, frame::frame_ra_offset, X2_SP);
  sd(X8_FP, frame::frame_fp_offset, X2_SP);
  mv(X8_FP, X2_SP);
  if (is_simm(-offset, 12)) {
    addi(X2_SP, X2_SP, -offset);
  } else {
    li(tmp, -offset);
    add(X2_SP, X2_SP, tmp);
  }
}

// Pop current C frame.
void MacroAssembler::pop_frame() {
  mv(X2_SP, X8_FP);
  ld(X1_RA, frame::frame_ra_offset, X2_SP);
  ld(X8_FP, frame::frame_fp_offset, X2_SP);
}

// Generic version of a call to C function via an address in a register
// Updates and returns _last_calls_return_pc.
address MacroAssembler::branch_to(Register fn_entry, bool and_link) {
  // do a call or a branch
  if (and_link) {
    jalr(X1_RA, fn_entry, 0);
  } else {
    jalr(XZERO, fn_entry, 0);
  }
  _last_calls_return_pc = pc();

  return _last_calls_return_pc;
}

// Call a C function via a function descriptor and use full C calling
// conventions.
// We don't use the TOC in generated code, so there is no need to save
// and restore its value.
address MacroAssembler::call_c(Register fd) {
  Unimplemented();
  /*
  return branch_to(fd, */ /*and_link=*/ /*true,
                       */ /*save toc=*/ /*false,
                       */ /*restore toc=*/ /*false,
                       */ /*load toc=*/ /*true,
                       */ /*load env=*/ /*true);
  */
  return pc();
}

address MacroAssembler::call_c_and_return_to_caller(Register fd) {
  Unimplemented();
  /*
  return branch_to(fd, */ /*and_link=*/ /*false,
                       */ /*save toc=*/ /*false,
                       */ /*restore toc=*/ /*false,
                       */ /*load toc=*/ /*true,
                       */ /*load env=*/ /*true);
  */
  return pc();
}

address MacroAssembler::call_c(address fn_entry, relocInfo::relocType rt) {
  if (rt != relocInfo::none) {
    load_const(X7_T2, fn_entry);
    return branch_to(X7_T2, /*and_link=*/ true);
  } else {
    // TODO: This call does not need to be relocatable, do more aggressive
    // optimizations.
    load_const(X7_T2, fn_entry);
    return branch_to(X7_T2, /*and_link=*/ true);
  }
}

void MacroAssembler::call_VM_base(Register oop_result,
                                  Register last_java_sp,
                                  address  entry_point,
                                  bool     check_exceptions) {
  BLOCK_COMMENT("call_VM {");
  // Determine last_java_sp register.
  if (!last_java_sp->is_valid()) {
    last_java_sp = X2_SP;
  }
  Register last_java_pc = X6_T1;
  address entry = pc();

  li(last_java_pc, (intptr_t) entry);

  set_last_Java_frame(last_java_sp, X8_FP, last_java_pc);

  // ARG1 must hold thread address.
  mv(X10_ARG0, X20_thread);
  address return_pc = call_c(entry_point, relocInfo::none);

  reset_last_Java_frame();

  // Check for pending exceptions.
  if (check_exceptions) {
    // We don't check for exceptions here.
    ShouldNotReachHere();
  }

  // Get oop result if there is one and reset the value in the thread.
  if (oop_result->is_valid()) {
    get_vm_result(oop_result);
  }

  _last_calls_return_pc = return_pc;
  BLOCK_COMMENT("} call_VM");
}

void MacroAssembler::call_VM_leaf_base(address entry_point) {
  BLOCK_COMMENT("call_VM_leaf {");
  call_c(entry_point, relocInfo::none);
  BLOCK_COMMENT("} call_VM_leaf");
}

void MacroAssembler::call_VM(Register oop_result, address entry_point, bool check_exceptions) {
  call_VM_base(oop_result, noreg, entry_point, check_exceptions);
}

void MacroAssembler::call_VM(Register oop_result, address entry_point, Register arg_1,
                             bool check_exceptions) {
  // X10_ARG0 is reserved for the thread.
  mv_if_needed(X11_ARG1, arg_1);
  call_VM(oop_result, entry_point, check_exceptions);
}

void MacroAssembler::call_VM(Register oop_result, address entry_point, Register arg_1, Register arg_2,
                             bool check_exceptions) {
  // X10_ARG0 is reserved for the thread
  mv_if_needed(X11_ARG1, arg_1);
  assert(arg_2 != X11_ARG1, "smashed argument");
  mv_if_needed(X12_ARG2, arg_2);
  call_VM(oop_result, entry_point, check_exceptions);
}

void MacroAssembler::call_VM(Register oop_result, address entry_point, Register arg_1, Register arg_2, Register arg_3,
                             bool check_exceptions) {
  // X10_ARG0 is reserved for the thread
  mv_if_needed(X11_ARG1, arg_1);
  assert(arg_2 != X11_ARG1, "smashed argument");
  mv_if_needed(X12_ARG2, arg_2);
  mv_if_needed(X13_ARG3, arg_3);
  call_VM(oop_result, entry_point, check_exceptions);
}

void MacroAssembler::call_VM_leaf(address entry_point) {
  call_VM_leaf_base(entry_point);
}

void MacroAssembler::call_VM_leaf(address entry_point, Register arg_1) {
  mv_if_needed(X10_ARG0, arg_1);
  call_VM_leaf(entry_point);
}

void MacroAssembler::call_VM_leaf(address entry_point, Register arg_1, Register arg_2) {
  mv_if_needed(X10_ARG0, arg_1);
  assert(arg_2 != X10_ARG0, "smashed argument");
  mv_if_needed(X11_ARG1, arg_2);
  call_VM_leaf(entry_point);
}

void MacroAssembler::call_VM_leaf(address entry_point, Register arg_1, Register arg_2, Register arg_3) {
  mv_if_needed(X10_ARG0, arg_1);
  assert(arg_2 != X10_ARG0, "smashed argument");
  mv_if_needed(X11_ARG1, arg_2);
  assert(arg_3 != X10_ARG0 && arg_3 != X11_ARG1, "smashed argument");
  mv_if_needed(X12_ARG2, arg_3);
  call_VM_leaf(entry_point);
}

// Check whether instruction is a read access to the polling page
// which was emitted by load_from_polling_page(..).
bool MacroAssembler::is_load_from_polling_page(int instruction, void* ucontext,
                                               address* polling_address_ptr) {
  Unimplemented();
#if 0
  if (!is_ld(instruction))
    return false; // It's not a ld. Fail.

  int rt = inv_rt_field(instruction);
  int ra = inv_ra_field(instruction);
  int ds = inv_ds_field(instruction);
  if (!(ds == 0 && ra != 0 && rt == 0)) {
    return false; // It's not a ld(r0, X, ra). Fail.
  }

  if (!ucontext) {
    // Set polling address.
    if (polling_address_ptr != NULL) {
      *polling_address_ptr = NULL;
    }
    return true; // No ucontext given. Can't check value of ra. Assume true.
  }

#ifdef LINUX
  // Ucontext given. Check that register ra contains the address of
  // the safepoing polling page.
  ucontext_t* uc = (ucontext_t*) ucontext;
  // Set polling address.
  address addr = (address)uc->uc_mcontext.regs->gpr[ra] + (ssize_t)ds;
  if (polling_address_ptr != NULL) {
    *polling_address_ptr = addr;
  }
  return os::is_poll_address(addr);
#else
  // Not on Linux, ucontext must be NULL.
  ShouldNotReachHere();
  return false;
#endif
  return false;
#endif
}

bool MacroAssembler::is_memory_serialization(int instruction, JavaThread* thread, void* ucontext) {
  Unimplemented();
  /*
#ifdef LINUX
  ucontext_t* uc = (ucontext_t*) ucontext;

  if (is_stwx(instruction) || is_stwux(instruction)) {
    int ra = inv_ra_field(instruction);
    int rb = inv_rb_field(instruction);

    // look up content of ra and rb in ucontext
    address ra_val=(address)uc->uc_mcontext.regs->gpr[ra];
    long rb_val=(long)uc->uc_mcontext.regs->gpr[rb];
    return os::is_memory_serialize_page(thread, ra_val+rb_val);
  } else if (is_stw(instruction) || is_stwu(instruction)) {
    int ra = inv_ra_field(instruction);
    int d1 = inv_d1_field(instruction);

    // look up content of ra in ucontext
    address ra_val=(address)uc->uc_mcontext.regs->gpr[ra];
    return os::is_memory_serialize_page(thread, ra_val+d1);
  } else {
    return false;
  }
#else
  // workaround not needed on !LINUX :-)
  ShouldNotCallThis();
  return false;
#endif
  */
  return false;
}

void MacroAssembler::bang_stack_with_offset(int offset) {
  Unimplemented();
  /*
  // When increasing the stack, the old stack pointer will be written
  // to the new top of stack according to the RISCV64 abi.
  // Therefore, stack banging is not necessary when increasing
  // the stack by <= os::vm_page_size() bytes.
  // When increasing the stack by a larger amount, this method is
  // called repeatedly to bang the intermediate pages.

  // Stack grows down, caller passes positive offset.
  assert(offset > 0, "must bang with positive offset");

  long sdoffset = -offset;

  if (is_simm(sdoffset, 16)) {
    // Signed 16 bit offset, a simple sd is ok.
    if (UseLoadInstructionsForStackBangingRISCV64) {
      ld(R0, (int)(signed short)sdoffset, X2_SP);
    } else {
      sd(R0,(int)(signed short)sdoffset, X2_SP);
    }
  } else if (is_simm(sdoffset, 31)) {
    const int hi = MacroAssembler::largeoffset_si16_si16_hi(sdoffset);
    const int lo = MacroAssembler::largeoffset_si16_si16_lo(sdoffset);

    Register tmp = R11;
    addis(tmp, X2_SP, hi);
    if (UseLoadInstructionsForStackBangingRISCV64) {
      ld(R0,  lo, tmp);
    } else {
      sd(R0, lo, tmp);
    }
  } else {
    ShouldNotReachHere();
  }
  */
}

// If instruction is a stack bang of the form
//    sd    R0,    x(Ry),       (see bang_stack_with_offset())
//    sdu   X2_SP, x(X2_SP),    (see push_frame(), resize_frame())
// or sdux  X2_SP, Rx, X2_SP    (see push_frame(), resize_frame())
// return the banged address. Otherwise, return 0.
address MacroAssembler::get_stack_bang_address(int instruction, void *ucontext) {
  Unimplemented();
  /*
#ifdef LINUX
  ucontext_t* uc = (ucontext_t*) ucontext;
  int rs = inv_rs_field(instruction);
  int ra = inv_ra_field(instruction);
  if (   (is_ld(instruction)   && rs == 0 &&  UseLoadInstructionsForStackBangingRISCV64)
      || (is_sd(instruction)  && rs == 0 && !UseLoadInstructionsForStackBangingRISCV64)
      || (is_sdu(instruction) && rs == 1)) {
    int ds = inv_ds_field(instruction);
    // return banged address
    return ds+(address)uc->uc_mcontext.regs->gpr[ra];
  } else if (is_sdux(instruction) && rs == 1) {
    int rb = inv_rb_field(instruction);
    address sp = (address)uc->uc_mcontext.regs->gpr[1];
    long rb_val = (long)uc->uc_mcontext.regs->gpr[rb];
    return ra != 1 || rb_val >= 0 ? NULL         // not a stack bang
                                  : sp + rb_val; // banged address
  }
  return NULL; // not a stack bang
#else
  // workaround not needed on !LINUX :-)
  ShouldNotCallThis();
  return NULL;
#endif
  */
  return NULL;
}

// Look up the method for a megamorphic invokeinterface call.
// The target method is determined by <intf_klass, itable_index>.
// The receiver klass is in recv_klass.
// On success, the result will be in method_result, and execution falls through.
// On failure, execution transfers to the given label.
void MacroAssembler::lookup_interface_method(Register recv_klass,
                                             Register intf_klass,
                                             RegisterOrConstant itable_index,
                                             Register method_result,
                                             Register scan_temp,
                                             Register sethi_temp,
                                             Label& L_no_such_interface) {
  Unimplemented();
  /*
  assert_different_registers(recv_klass, intf_klass, method_result, scan_temp);
  assert(itable_index.is_constant() || itable_index.as_register() == method_result,
         "caller must use same register for non-constant itable index as for method");

  // Compute start of first itableOffsetEntry (which is at the end of the vtable).
  int vtable_base = InstanceKlass::vtable_start_offset() * wordSize;
  int itentry_off = itableMethodEntry::method_offset_in_bytes();
  int logMEsize   = exact_log2(itableMethodEntry::size() * wordSize);
  int scan_step   = itableOffsetEntry::size() * wordSize;
  int log_vte_size= exact_log2(vtableEntry::size() * wordSize);

  lwz(scan_temp, InstanceKlass::vtable_length_offset() * wordSize, recv_klass);
  // %%% We should store the aligned, prescaled offset in the klassoop.
  // Then the next several instructions would fold away.

  sldi(scan_temp, scan_temp, log_vte_size);
  addi(scan_temp, scan_temp, vtable_base);
  add(scan_temp, recv_klass, scan_temp);

  // Adjust recv_klass by scaled itable_index, so we can free itable_index.
  if (itable_index.is_register()) {
    Register itable_offset = itable_index.as_register();
    sldi(itable_offset, itable_offset, logMEsize);
    if (itentry_off) addi(itable_offset, itable_offset, itentry_off);
    add(recv_klass, itable_offset, recv_klass);
  } else {
    long itable_offset = (long)itable_index.as_constant();
    load_const_optimized(sethi_temp, (itable_offset<<logMEsize)+itentry_off); // static address, no relocation
    add(recv_klass, sethi_temp, recv_klass);
  }

  // for (scan = klass->itable(); scan->interface() != NULL; scan += scan_step) {
  //   if (scan->interface() == intf) {
  //     result = (klass + scan->offset() + itable_index);
  //   }
  // }
  Label search, found_method;

  for (int peel = 1; peel >= 0; peel--) {
    // %%%% Could load both offset and interface in one ldx, if they were
    // in the opposite order. This would save a load.
    ld(method_result, itableOffsetEntry::interface_offset_in_bytes(), scan_temp);

    // Check that this entry is non-null. A null entry means that
    // the receiver class doesn't implement the interface, and wasn't the
    // same as when the caller was compiled.
    cmpd(CCR0, method_result, intf_klass);

    if (peel) {
      beq(CCR0, found_method);
    } else {
      bne(CCR0, search);
      // (invert the test to fall through to found_method...)
    }

    if (!peel) break;

    bind(search);

    cmpdi(CCR0, method_result, 0);
    beq(CCR0, L_no_such_interface);
    addi(scan_temp, scan_temp, scan_step);
  }

  bind(found_method);

  // Got a hit.
  int ito_offset = itableOffsetEntry::offset_offset_in_bytes();
  lwz(scan_temp, ito_offset, scan_temp);
  ldx(method_result, scan_temp, recv_klass);
  */
}

// virtual method calling
void MacroAssembler::lookup_virtual_method(Register recv_klass,
                                           RegisterOrConstant vtable_index,
                                           Register method_result) {
  Unimplemented();
  /*

  assert_different_registers(recv_klass, method_result, vtable_index.register_or_noreg());

  const int base = InstanceKlass::vtable_start_offset() * wordSize;
  assert(vtableEntry::size() * wordSize == wordSize, "adjust the scaling in the code below");

  if (vtable_index.is_register()) {
    sldi(vtable_index.as_register(), vtable_index.as_register(), LogBytesPerWord);
    add(recv_klass, vtable_index.as_register(), recv_klass);
  } else {
    addi(recv_klass, recv_klass, vtable_index.as_constant() << LogBytesPerWord);
  }
  ld(R19_method, base + vtableEntry::method_offset_in_bytes(), recv_klass);
  */
}

/////////////////////////////////////////// subtype checking ////////////////////////////////////////////

void MacroAssembler::check_klass_subtype_fast_path(Register sub_klass,
                                                   Register super_klass,
                                                   Register temp1_reg,
                                                   Register temp2_reg,
                                                   Label& L_success,
                                                   Label& L_failure) {
  Unimplemented();
  /*

  const Register check_cache_offset = temp1_reg;
  const Register cached_super       = temp2_reg;

  assert_different_registers(sub_klass, super_klass, check_cache_offset, cached_super);

  int sco_offset = in_bytes(Klass::super_check_offset_offset());
  int sc_offset  = in_bytes(Klass::secondary_super_cache_offset());

  // If the pointers are equal, we are done (e.g., String[] elements).
  // This self-check enables sharing of secondary supertype arrays among
  // non-primary types such as array-of-interface. Otherwise, each such
  // type would need its own customized SSA.
  // We move this check to the front of the fast path because many
  // type checks are in fact trivially successful in this manner,
  // so we get a nicely predicted branch right at the start of the check.
  cmpd(CCR0, sub_klass, super_klass);
  beq(CCR0, L_success);

  // Check the supertype display:
  lwz(check_cache_offset, sco_offset, super_klass);
  // The loaded value is the offset from KlassOopDesc.

  ldx(cached_super, check_cache_offset, sub_klass);
  cmpd(CCR0, cached_super, super_klass);
  beq(CCR0, L_success);

  // This check has worked decisively for primary supers.
  // Secondary supers are sought in the super_cache ('super_cache_addr').
  // (Secondary supers are interfaces and very deeply nested subtypes.)
  // This works in the same check above because of a tricky aliasing
  // between the super_cache and the primary super display elements.
  // (The 'super_check_addr' can address either, as the case requires.)
  // Note that the cache is updated below if it does not help us find
  // what we need immediately.
  // So if it was a primary super, we can just fail immediately.
  // Otherwise, it's the slow path for us (no success at this point).

  cmpwi(CCR0, check_cache_offset, sc_offset);
  bne(CCR0, L_failure);
  // bind(slow_path); // fallthru
  */
}

void MacroAssembler::check_klass_subtype_slow_path(Register sub_klass,
                                                   Register super_klass,
                                                   Register temp1_reg,
                                                   Register temp2_reg,
                                                   Label* L_success,
                                                   Register result_reg) {
  Unimplemented();
  /*
  const Register array_ptr = temp1_reg; // current value from cache array
  const Register temp      = temp2_reg;

  assert_different_registers(sub_klass, super_klass, array_ptr, temp);

  int source_offset = in_bytes(Klass::secondary_supers_offset());
  int target_offset = in_bytes(Klass::secondary_super_cache_offset());

  int length_offset = Array<Klass*>::length_offset_in_bytes();
  int base_offset   = Array<Klass*>::base_offset_in_bytes();

  Label hit, loop, failure, fallthru;

  ld(array_ptr, source_offset, sub_klass);

  //assert(4 == arrayOopDesc::length_length_in_bytes(), "precondition violated.");
  lwz(temp, length_offset, array_ptr);
  cmpwi(CCR0, temp, 0);
  beq(CCR0, result_reg!=noreg ? failure : fallthru); // length 0

  mtctr(temp); // load ctr

  bind(loop);
  // Oops in table are NO MORE compressed.
  ld(temp, base_offset, array_ptr);
  cmpd(CCR0, temp, super_klass);
  beq(CCR0, hit);
  addi(array_ptr, array_ptr, BytesPerWord);
  bdnz(loop);

  bind(failure);
  if (result_reg!=noreg) li(result_reg, 1); // load non-zero result (indicates a miss)
  b(fallthru);

  bind(hit);
  sd(super_klass, target_offset, sub_klass); // save result to cache
  if (result_reg != noreg) li(result_reg, 0); // load zero result (indicates a hit)
  if (L_success != NULL) b(*L_success);

  bind(fallthru);
  */
}

// Try fast path, then go to slow one if not successful
void MacroAssembler::check_klass_subtype(Register sub_klass,
                         Register super_klass,
                         Register temp1_reg,
                         Register temp2_reg,
                         Label& L_success) {
  Unimplemented();
  /*
  Label L_failure;
  check_klass_subtype_fast_path(sub_klass, super_klass, temp1_reg, temp2_reg, L_success, L_failure);
  check_klass_subtype_slow_path(sub_klass, super_klass, temp1_reg, temp2_reg, &L_success);
  bind(L_failure); // Fallthru if not successful.
  */
}

void MacroAssembler::check_method_handle_type(Register mtype_reg, Register mh_reg,
                                              Register temp_reg,
                                              Label& wrong_method_type) {
  Unimplemented();
  /*
  assert_different_registers(mtype_reg, mh_reg, temp_reg);
  // Compare method type against that of the receiver.
  load_heap_oop_not_null(temp_reg, delayed_value(java_lang_invoke_MethodHandle::type_offset_in_bytes, temp_reg), mh_reg);
  cmpd(CCR0, temp_reg, mtype_reg);
  bne(CCR0, wrong_method_type);
  */
}

RegisterOrConstant MacroAssembler::argument_offset(RegisterOrConstant arg_slot,
                                                   Register temp_reg,
                                                   int extra_slot_offset) {
  // cf. TemplateTable::prepare_invoke(), if (load_receiver).
  int stackElementSize = Interpreter::stackElementSize;
  int offset = extra_slot_offset * stackElementSize;
  if (arg_slot.is_constant()) {
    offset += arg_slot.as_constant() * stackElementSize;
    return offset;
  } else {
    assert(temp_reg != noreg, "must specify");
    slli(temp_reg, arg_slot.as_register(), exact_log2(stackElementSize));
    if (offset != 0)
      addi(temp_reg, temp_reg, offset);
    return temp_reg;
  }
}

// Write serialization page so VM thread can do a pseudo remote membar.
// We use the current thread pointer to calculate a thread specific
// offset to write to within the page. This minimizes bus traffic
// due to cache line collision.
void MacroAssembler::serialize_memory(Register thread, Register tmp1, Register tmp2) {
  Unimplemented();
  /*
  srdi(tmp2, thread, os::get_serialize_page_shift_count());

  int mask = os::vm_page_size() - sizeof(int);
  if (Assembler::is_simm(mask, 16)) {
    andi(tmp2, tmp2, mask);
  } else {
    lis(tmp1, (int)((signed short) (mask >> 16)));
    ori(tmp1, tmp1, mask & 0x0000ffff);
    andr(tmp2, tmp2, tmp1);
  }

  load_const(tmp1, (long) os::get_memory_serialize_page());
  release();
  stwx(R0, tmp1, tmp2);
  */
}


// GC barrier helper macros

// Write the card table byte if needed.
void MacroAssembler::card_write_barrier_post(Register Rstore_addr, Register Rnew_val, Register Rtmp) {
  Unimplemented();
  /*
  CardTableModRefBS* bs = (CardTableModRefBS*) Universe::heap()->barrier_set();
  assert(bs->kind() == BarrierSet::CardTableModRef ||
         bs->kind() == BarrierSet::CardTableExtension, "wrong barrier");
#ifdef ASSERT
  cmpdi(CCR0, Rnew_val, 0);
  asm_assert_ne("null oop not allowed", 0x321);
#endif
  card_table_write(bs->byte_map_base, Rtmp, Rstore_addr);
  */
}

// Write the card table byte.
void MacroAssembler::card_table_write(jbyte* byte_map_base, Register Rtmp, Register Robj) {
  Unimplemented();
  /*
  assert_different_registers(Robj, Rtmp, R0);
  load_const_optimized(Rtmp, (address)byte_map_base, R0);
  srdi(Robj, Robj, CardTableModRefBS::card_shift);
  li(R0, 0); // dirty
  if (UseConcMarkSweepGC) membar(Assembler::StoreStore);
  stbx(R0, Rtmp, Robj);
  */
}

#if INCLUDE_ALL_GCS
// General G1 pre-barrier generator.
// Goal: record the previous value if it is not null.
void MacroAssembler::g1_write_barrier_pre(Register Robj, RegisterOrConstant offset, Register Rpre_val,
                                          Register Rtmp1, Register Rtmp2, bool needs_frame) {
  Unimplemented();
  /*
  Label runtime, filtered;

  // Is marking active?
  if (in_bytes(PtrQueue::byte_width_of_active()) == 4) {
    lwz(Rtmp1, in_bytes(JavaThread::satb_mark_queue_offset() + PtrQueue::byte_offset_of_active()), R16_thread);
  } else {
    guarantee(in_bytes(PtrQueue::byte_width_of_active()) == 1, "Assumption");
    lbz(Rtmp1, in_bytes(JavaThread::satb_mark_queue_offset() + PtrQueue::byte_offset_of_active()), R16_thread);
  }
  cmpdi(CCR0, Rtmp1, 0);
  beq(CCR0, filtered);

  // Do we need to load the previous value?
  if (Robj != noreg) {
    // Load the previous value...
    if (UseCompressedOops) {
      lwz(Rpre_val, offset, Robj);
    } else {
      ld(Rpre_val, offset, Robj);
    }
    // Previous value has been loaded into Rpre_val.
  }
  assert(Rpre_val != noreg, "must have a real register");

  // Is the previous value null?
  cmpdi(CCR0, Rpre_val, 0);
  beq(CCR0, filtered);

  if (Robj != noreg && UseCompressedOops) {
    decode_heap_oop_not_null(Rpre_val);
  }

  // OK, it's not filtered, so we'll need to call enqueue. In the normal
  // case, pre_val will be a scratch G-reg, but there are some cases in
  // which it's an O-reg. In the first case, do a normal call. In the
  // latter, do a save here and call the frameless version.

  // Can we store original value in the thread's buffer?
  // Is index == 0?
  // (The index field is typed as size_t.)
  const Register Rbuffer = Rtmp1, Rindex = Rtmp2;

  ld(Rindex, in_bytes(JavaThread::satb_mark_queue_offset() + PtrQueue::byte_offset_of_index()), R16_thread);
  cmpdi(CCR0, Rindex, 0);
  beq(CCR0, runtime); // If index == 0, goto runtime.
  ld(Rbuffer, in_bytes(JavaThread::satb_mark_queue_offset() + PtrQueue::byte_offset_of_buf()), R16_thread);

  addi(Rindex, Rindex, -wordSize); // Decrement index.
  sd(Rindex, in_bytes(JavaThread::satb_mark_queue_offset() + PtrQueue::byte_offset_of_index()), R16_thread);

  // Record the previous value.
  sdx(Rpre_val, Rbuffer, Rindex);
  b(filtered);

  bind(runtime);

  // VM call need frame to access(write) O register.
  if (needs_frame) {
    save_LR_CR(Rtmp1);
    push_frame_reg_args(0, Rtmp2);
  }

  if (Rpre_val->is_volatile() && Robj == noreg) mr(R31, Rpre_val); // Save pre_val across C call if it was preloaded.
  call_VM_leaf(CAST_FROM_FN_PTR(address, SharedRuntime::g1_wb_pre), Rpre_val, R16_thread);
  if (Rpre_val->is_volatile() && Robj == noreg) mr(Rpre_val, R31); // restore

  if (needs_frame) {
    pop_frame();
    restore_LR_CR(Rtmp1);
  }

  bind(filtered);
  */
}

// General G1 post-barrier generator
// Store cross-region card.
void MacroAssembler::g1_write_barrier_post(Register Rstore_addr, Register Rnew_val, Register Rtmp1, Register Rtmp2, Register Rtmp3, Label *filtered_ext) {
  Unimplemented();
  /*
  Label runtime, filtered_int;
  Label& filtered = (filtered_ext != NULL) ? *filtered_ext : filtered_int;
  assert_different_registers(Rstore_addr, Rnew_val, Rtmp1, Rtmp2);

  G1SATBCardTableModRefBS* bs = (G1SATBCardTableModRefBS*) Universe::heap()->barrier_set();
  assert(bs->kind() == BarrierSet::G1SATBCT ||
         bs->kind() == BarrierSet::G1SATBCTLogging, "wrong barrier");

  // Does store cross heap regions?
  if (G1RSBarrierRegionFilter) {
    xorr(Rtmp1, Rstore_addr, Rnew_val);
    srdi_(Rtmp1, Rtmp1, HeapRegion::LogOfHRGrainBytes);
    beq(CCR0, filtered);
  }

  // Crosses regions, storing NULL?
#ifdef ASSERT
  cmpdi(CCR0, Rnew_val, 0);
  asm_assert_ne("null oop not allowed (G1)", 0x322); // Checked by caller on RISCV64, so following branch is obsolete:
  //beq(CCR0, filtered);
#endif

  // Storing region crossing non-NULL, is card already dirty?
  assert(sizeof(*bs->byte_map_base) == sizeof(jbyte), "adjust this code");
  const Register Rcard_addr = Rtmp1;
  Register Rbase = Rtmp2;
  load_const_optimized(Rbase, (address)bs->byte_map_base, */ /*temp*/ /* Rtmp3);

  srdi(Rcard_addr, Rstore_addr, CardTableModRefBS::card_shift);

  // Get the address of the card.
  lbzx(*/ /*card value*/ /* Rtmp3, Rbase, Rcard_addr);
  cmpwi(CCR0, Rtmp3, (int)G1SATBCardTableModRefBS::g1_young_card_val());
  beq(CCR0, filtered);

  membar(Assembler::StoreLoad);
  lbzx(*/ /*card value*/ /* Rtmp3, Rbase, Rcard_addr);  // Reload after membar.
  cmpwi(CCR0, Rtmp3 */ /* card value */ /*, CardTableModRefBS::dirty_card_val());
  beq(CCR0, filtered);

  // Storing a region crossing, non-NULL oop, card is clean.
  // Dirty card and log.
  li(Rtmp3, CardTableModRefBS::dirty_card_val());
  //release(); // G1: oops are allowed to get visible after dirty marking.
  stbx(Rtmp3, Rbase, Rcard_addr);

  add(Rcard_addr, Rbase, Rcard_addr); // This is the address which needs to get enqueued.
  Rbase = noreg; // end of lifetime

  const Register Rqueue_index = Rtmp2,
                 Rqueue_buf   = Rtmp3;
  ld(Rqueue_index, in_bytes(JavaThread::dirty_card_queue_offset() + PtrQueue::byte_offset_of_index()), R16_thread);
  cmpdi(CCR0, Rqueue_index, 0);
  beq(CCR0, runtime); // index == 0 then jump to runtime
  ld(Rqueue_buf, in_bytes(JavaThread::dirty_card_queue_offset() + PtrQueue::byte_offset_of_buf()), R16_thread);

  addi(Rqueue_index, Rqueue_index, -wordSize); // decrement index
  sd(Rqueue_index, in_bytes(JavaThread::dirty_card_queue_offset() + PtrQueue::byte_offset_of_index()), R16_thread);

  sdx(Rcard_addr, Rqueue_buf, Rqueue_index); // store card
  b(filtered);

  bind(runtime);

  // Save the live input values.
  call_VM_leaf(CAST_FROM_FN_PTR(address, SharedRuntime::g1_wb_post), Rcard_addr, R16_thread);

  bind(filtered_int);
  */
}
#endif // INCLUDE_ALL_GCS

// Values for last_Java_pc, and last_Java_sp must comply to the rules
// in frame_ppc.hpp.
void MacroAssembler::set_last_Java_frame(Register last_Java_sp, Register last_Java_fp, Register last_Java_pc) {
  // Always set last_Java_pc and flags first because once last_Java_sp
  // is visible has_last_Java_frame is true and users will look at the
  // rest of the fields. (Note: flags should always be zero before we
  // get here so doesn't need to be set.)


  // Verify that last_Java_pc was zeroed on return to Java
  assert_different_registers(X5_T0, last_Java_pc);
  asm_assert_mem8_is_zero(X5_T0, in_bytes(JavaThread::last_Java_pc_offset()), X20_thread,
                          "last_Java_pc not zeroed before leaving Java", 0x200);

  // When returning from calling out from Java mode the frame anchor's
  // last_Java_pc will always be set to NULL. It is set here so that
  // if we are doing a call to native (not VM) that we capture the
  // known pc and don't have to rely on the native call having a
  // standard frame linkage where we can find the pc.
  if (last_Java_pc != noreg)
    sd(last_Java_pc, in_bytes(JavaThread::last_Java_pc_offset()), X20_thread);

  // May need to handle case where fp isn't given.
  assert(last_Java_fp != noreg, "Must give a frame pointer to set last frame.");
  sd(last_Java_fp, in_bytes(JavaThread::frame_anchor_offset()) +                                                                                    in_bytes(JavaFrameAnchor::last_Java_fp_offset()), X20_thread);

  // Set last_Java_sp last, order the stores.
  OrderAccess::fence();
  sd(last_Java_sp, in_bytes(JavaThread::last_Java_sp_offset()), X20_thread);
}

void MacroAssembler::reset_last_Java_frame(void) {
  asm_assert_mem8_isnot_zero(X5_T0, in_bytes(JavaThread::last_Java_sp_offset()),
                             X20_thread, "SP was not set, still zero", 0x202);

  BLOCK_COMMENT("reset_last_Java_frame {");

  // _last_Java_sp = 0
  sd(XZERO, in_bytes(JavaThread::last_Java_sp_offset()), X20_thread);

  // _last_Java_fp = 0
  sd(XZERO, in_bytes(JavaThread::frame_anchor_offset()) +                                                                                    in_bytes(JavaFrameAnchor::last_Java_fp_offset()), X20_thread);



  // _last_Java_pc = 0
  sd(XZERO, in_bytes(JavaThread::last_Java_pc_offset()), X20_thread);
  BLOCK_COMMENT("} reset_last_Java_frame");
}

void MacroAssembler::get_vm_result(Register oop_result) {
  // Read:
  //   X20_thread
  //   X20_thread->in_bytes(JavaThread::vm_result_offset())
  //
  // Updated:
  //   oop_result
  //   X20_thread->in_bytes(JavaThread::vm_result_offset())

  ld(oop_result, in_bytes(JavaThread::vm_result_offset()), X20_thread);
  sd(XZERO, in_bytes(JavaThread::vm_result_offset()), X20_thread);

  verify_oop(oop_result);
}

void MacroAssembler::get_vm_result_2(Register metadata_result) {
  // Read:
  //   X20_thread
  //   X20_thread->in_bytes(JavaThread::vm_result_2_offset())
  //
  // Updated:
  //   metadata_result
  //   X20_thread->in_bytes(JavaThread::vm_result_2_offset())

  ld(metadata_result, in_bytes(JavaThread::vm_result_2_offset()), X20_thread);
  sd(XZERO, in_bytes(JavaThread::vm_result_2_offset()), X20_thread);
}


void MacroAssembler::encode_klass_not_null(Register dst, Register src) {
  Unimplemented();
  /*
  Register current = (src != noreg) ? src : dst; // Klass is in dst if no src provided.
  if (Universe::narrow_klass_base() != 0) {
    // Use dst as temp if it is free.
    load_const(R0, Universe::narrow_klass_base(), (dst != current && dst != R0) ? dst : noreg);
    sub(dst, current, R0);
    current = dst;
  }
  if (Universe::narrow_klass_shift() != 0) {
    srdi(dst, current, Universe::narrow_klass_shift());
    current = dst;
  }
  mv_if_needed(dst, current); // Move may be required.
  */
}

void MacroAssembler::store_klass(Register dst_oop, Register klass, Register ck) {
  Unimplemented();
  /*
  if (UseCompressedClassPointers) {
    encode_klass_not_null(ck, klass);
    stw(ck, oopDesc::klass_offset_in_bytes(), dst_oop);
  } else {
    sd(klass, oopDesc::klass_offset_in_bytes(), dst_oop);
  }
  */
}

void MacroAssembler::store_klass_gap(Register dst_oop, Register val) {
  Unimplemented();
  /*
  if (UseCompressedClassPointers) {
    if (val == noreg) {
      val = R0;
      li(val, 0);
    }
    stw(val, oopDesc::klass_gap_offset_in_bytes(), dst_oop); // klass gap if compressed
  }
  */
}

int MacroAssembler::instr_size_for_decode_klass_not_null() {
  if (!UseCompressedClassPointers) return 0;
  int num_instrs = 1;  // shift or move
  if (Universe::narrow_klass_base() != 0) num_instrs = 7;  // shift + load const + add
  return num_instrs * BytesPerInstWord;
}

void MacroAssembler::decode_klass_not_null(Register dst, Register src) {
  assert(dst != X28_T3, "Dst reg may not be X28, as X28 is used here.");
  if (src == noreg) src = dst;
  Register shifted_src = src;
  if (Universe::narrow_klass_shift() != 0 ||
      Universe::narrow_klass_base() == 0 && src != dst) {  // Move required.
    shifted_src = dst;
    slli(shifted_src, src, Universe::narrow_klass_shift());
  }
  if (Universe::narrow_klass_base() != 0) {
    load_const(X28, Universe::narrow_klass_base());
    add(dst, shifted_src, X28_T3);
  }
}

void MacroAssembler::load_klass(Register dst, Register src) {
  if (UseCompressedClassPointers) {
    lwu(dst, oopDesc::klass_offset_in_bytes(), src);
    // Attention: no null check here!
    decode_klass_not_null(dst, dst);
  } else {
    ld(dst, oopDesc::klass_offset_in_bytes(), src);
  }
}

void MacroAssembler::load_klass_with_trap_null_check(Register dst, Register src) {
  Unimplemented();
  /*
  if (!os::zero_page_read_protected()) {
    if (TrapBasedNullChecks) {
      trap_null_check(src);
    }
  }
  load_klass(dst, src);
  */
}

void MacroAssembler::reinit_heapbase(Register d) {
  if (Universe::heap() != NULL) {
    load_const(d, Universe::narrow_ptrs_base());
  } else {
    // Heap not yet allocated. Load indirectly.
    load_const(d, Universe::narrow_ptrs_base_addr());
  }
}

// Clear Array
// Kills both input registers. tmp == R0 is allowed.
void MacroAssembler::clear_memory_doubleword(Register base_ptr, Register cnt_dwords, Register tmp) {
  Unimplemented();
  /*
  // Procedure for large arrays (uses data cache block zero instruction).
    Label startloop, fast, fastloop, small_rest, restloop, done;
    const int cl_size         = VM_Version::get_cache_line_size(),
              cl_dwords       = cl_size>>3,
              cl_dw_addr_bits = exact_log2(cl_dwords),
              dcbz_min        = 1;                     // Min count of dcbz executions, needs to be >0.

//2:
    cmpdi(CCR1, cnt_dwords, ((dcbz_min+1)<<cl_dw_addr_bits)-1); // Big enough? (ensure >=dcbz_min lines included).
    blt(CCR1, small_rest);                                      // Too small.
    rldicl_(tmp, base_ptr, 64-3, 64-cl_dw_addr_bits);           // Extract dword offset within first cache line.
    beq(CCR0, fast);                                            // Already 128byte aligned.

    subfic(tmp, tmp, cl_dwords);
    mtctr(tmp);                        // Set ctr to hit 128byte boundary (0<ctr<cl_dwords).
    subf(cnt_dwords, tmp, cnt_dwords); // rest.
    li(tmp, 0);
//10:
  bind(startloop);                     // Clear at the beginning to reach 128byte boundary.
    sd(tmp, 0, base_ptr);             // Clear 8byte aligned block.
    addi(base_ptr, base_ptr, 8);
    bdnz(startloop);
//13:
  bind(fast);                                  // Clear 128byte blocks.
    srdi(tmp, cnt_dwords, cl_dw_addr_bits);    // Loop count for 128byte loop (>0).
    andi(cnt_dwords, cnt_dwords, cl_dwords-1); // Rest in dwords.
    mtctr(tmp);                                // Load counter.
//16:
  bind(fastloop);
    dcbz(base_ptr);                    // Clear 128byte aligned block.
    addi(base_ptr, base_ptr, cl_size);
    bdnz(fastloop);
    if (InsertEndGroupRISCV64) { endgroup(); } else { nop(); }
//20:
  bind(small_rest);
    cmpdi(CCR0, cnt_dwords, 0);        // size 0?
    beq(CCR0, done);                   // rest == 0
    li(tmp, 0);
    mtctr(cnt_dwords);                 // Load counter.
//24:
  bind(restloop);                      // Clear rest.
    sd(tmp, 0, base_ptr);             // Clear 8byte aligned block.
    addi(base_ptr, base_ptr, 8);
    bdnz(restloop);
//27:
  bind(done);
  */
}

/////////////////////////////////////////// String intrinsics ////////////////////////////////////////////

// Search for a single jchar in an jchar[].
//
// Assumes that result differs from all other registers.
//
// Haystack, needle are the addresses of jchar-arrays.
// NeedleChar is needle[0] if it is known at compile time.
// Haycnt is the length of the haystack. We assume haycnt >=1.
//
// Preserves haystack, haycnt, kills all other registers.
//
// If needle == R0, we search for the constant needleChar.
void MacroAssembler::string_indexof_1(Register result, Register haystack, Register haycnt,
                                      Register needle, jchar needleChar,
                                      Register tmp1, Register tmp2) {
  Unimplemented();
  /*

  assert_different_registers(result, haystack, haycnt, needle, tmp1, tmp2);

  Label L_InnerLoop, L_FinalCheck, L_Found1, L_Found2, L_Found3, L_NotFound, L_End;
  Register needle0 = needle, // Contains needle[0].
           addr = tmp1,
           ch1 = tmp2,
           ch2 = R0;

//2 (variable) or 3 (const):
   if (needle != R0) lhz(needle0, 0, needle); // Preload needle character, needle has len==1.
   dcbtct(haystack, 0x00);                        // Indicate R/O access to haystack.

   srwi_(tmp2, haycnt, 1);   // Shift right by exact_log2(UNROLL_FACTOR).
   mr(addr, haystack);
   beq(CCR0, L_FinalCheck);
   mtctr(tmp2);              // Move to count register.
//8:
  bind(L_InnerLoop);             // Main work horse (2x unrolled search loop).
   lhz(ch1, 0, addr);        // Load characters from haystack.
   lhz(ch2, 2, addr);
   (needle != R0) ? cmpw(CCR0, ch1, needle0) : cmplwi(CCR0, ch1, needleChar);
   (needle != R0) ? cmpw(CCR1, ch2, needle0) : cmplwi(CCR1, ch2, needleChar);
   beq(CCR0, L_Found1);   // Did we find the needle?
   beq(CCR1, L_Found2);
   addi(addr, addr, 4);
   bdnz(L_InnerLoop);
//16:
  bind(L_FinalCheck);
   andi_(R0, haycnt, 1);
   beq(CCR0, L_NotFound);
   lhz(ch1, 0, addr);        // One position left at which we have to compare.
   (needle != R0) ? cmpw(CCR1, ch1, needle0) : cmplwi(CCR1, ch1, needleChar);
   beq(CCR1, L_Found3);
//21:
  bind(L_NotFound);
   li(result, -1);           // Not found.
   b(L_End);

  bind(L_Found2);
   addi(addr, addr, 2);
//24:
  bind(L_Found1);
  bind(L_Found3);                  // Return index ...
   subf(addr, haystack, addr); // relative to haystack,
   srdi(result, addr, 1);      // in characters.
  bind(L_End);
  */
}


// Implementation of IndexOf for jchar arrays.
//
// The length of haystack and needle are not constant, i.e. passed in a register.
//
// Preserves registers haystack, needle.
// Kills registers haycnt, needlecnt.
// Assumes that result differs from all other registers.
// Haystack, needle are the addresses of jchar-arrays.
// Haycnt, needlecnt are the lengths of them, respectively.
//
// Needlecntval must be zero or 15-bit unsigned immediate and > 1.
void MacroAssembler::string_indexof(Register result, Register haystack, Register haycnt,
                                    Register needle, ciTypeArray* needle_values, Register needlecnt, int needlecntval,
                                    Register tmp1, Register tmp2, Register tmp3, Register tmp4) {
  Unimplemented();
  /*

  // Ensure 0<needlecnt<=haycnt in ideal graph as prerequisite!
  Label L_TooShort, L_Found, L_NotFound, L_End;
  Register last_addr = haycnt, // Kill haycnt at the beginning.
           addr      = tmp1,
           n_start   = tmp2,
           ch1       = tmp3,
           ch2       = R0;

  // **************************************************************************************************
  // Prepare for main loop: optimized for needle count >=2, bail out otherwise.
  // **************************************************************************************************

//1 (variable) or 3 (const):
   dcbtct(needle, 0x00);    // Indicate R/O access to str1.
   dcbtct(haystack, 0x00);  // Indicate R/O access to str2.

  // Compute last haystack addr to use if no match gets found.
  if (needlecntval == 0) { // variable needlecnt
//3:
   subf(ch1, needlecnt, haycnt);      // Last character index to compare is haycnt-needlecnt.
   addi(addr, haystack, -2);          // Accesses use pre-increment.
   cmpwi(CCR6, needlecnt, 2);
   blt(CCR6, L_TooShort);          // Variable needlecnt: handle short needle separately.
   slwi(ch1, ch1, 1);                 // Scale to number of bytes.
   lwz(n_start, 0, needle);           // Load first 2 characters of needle.
   add(last_addr, haystack, ch1);     // Point to last address to compare (haystack+2*(haycnt-needlecnt)).
   addi(needlecnt, needlecnt, -2);    // Rest of needle.
  } else { // constant needlecnt
  guarantee(needlecntval != 1, "IndexOf with single-character needle must be handled separately");
  assert((needlecntval & 0x7fff) == needlecntval, "wrong immediate");
//5:
   addi(ch1, haycnt, -needlecntval);  // Last character index to compare is haycnt-needlecnt.
   lwz(n_start, 0, needle);           // Load first 2 characters of needle.
   addi(addr, haystack, -2);          // Accesses use pre-increment.
   slwi(ch1, ch1, 1);                 // Scale to number of bytes.
   add(last_addr, haystack, ch1);     // Point to last address to compare (haystack+2*(haycnt-needlecnt)).
   li(needlecnt, needlecntval-2);     // Rest of needle.
  }

  // Main Loop (now we have at least 3 characters).
//11:
  Label L_OuterLoop, L_InnerLoop, L_FinalCheck, L_Comp1, L_Comp2, L_Comp3;
  bind(L_OuterLoop); // Search for 1st 2 characters.
  Register addr_diff = tmp4;
   subf(addr_diff, addr, last_addr); // Difference between already checked address and last address to check.
   addi(addr, addr, 2);              // This is the new address we want to use for comparing.
   srdi_(ch2, addr_diff, 2);
   beq(CCR0, L_FinalCheck);       // 2 characters left?
   mtctr(ch2);                       // addr_diff/4
//16:
  bind(L_InnerLoop);                // Main work horse (2x unrolled search loop)
   lwz(ch1, 0, addr);           // Load 2 characters of haystack (ignore alignment).
   lwz(ch2, 2, addr);
   cmpw(CCR0, ch1, n_start); // Compare 2 characters (1 would be sufficient but try to reduce branches to CompLoop).
   cmpw(CCR1, ch2, n_start);
   beq(CCR0, L_Comp1);       // Did we find the needle start?
   beq(CCR1, L_Comp2);
   addi(addr, addr, 4);
   bdnz(L_InnerLoop);
//24:
  bind(L_FinalCheck);
   rldicl_(addr_diff, addr_diff, 64-1, 63); // Remaining characters not covered by InnerLoop: (addr_diff>>1)&1.
   beq(CCR0, L_NotFound);
   lwz(ch1, 0, addr);                       // One position left at which we have to compare.
   cmpw(CCR1, ch1, n_start);
   beq(CCR1, L_Comp3);
//29:
  bind(L_NotFound);
   li(result, -1); // not found
   b(L_End);


   // **************************************************************************************************
   // Special Case: unfortunately, the variable needle case can be called with needlecnt<2
   // **************************************************************************************************
//31:
 if ((needlecntval>>1) !=1 ) { // Const needlecnt is 2 or 3? Reduce code size.
  int nopcnt = 5;
  if (needlecntval !=0 ) ++nopcnt; // Balance alignment (other case: see below).
  if (needlecntval == 0) {         // We have to handle these cases separately.
  Label L_OneCharLoop;
  bind(L_TooShort);
   mtctr(haycnt);
   lhz(n_start, 0, needle);    // First character of needle
  bind(L_OneCharLoop);
   lhzu(ch1, 2, addr);
   cmpw(CCR1, ch1, n_start);
   beq(CCR1, L_Found);      // Did we find the one character needle?
   bdnz(L_OneCharLoop);
   li(result, -1);             // Not found.
   b(L_End);
  } // 8 instructions, so no impact on alignment.
  for (int x = 0; x < nopcnt; ++x) nop();
 }

  // **************************************************************************************************
  // Regular Case Part II: compare rest of needle (first 2 characters have been compared already)
  // **************************************************************************************************

  // Compare the rest
//36 if needlecntval==0, else 37:
  bind(L_Comp2);
   addi(addr, addr, 2); // First comparison has failed, 2nd one hit.
  bind(L_Comp1);            // Addr points to possible needle start.
  bind(L_Comp3);            // Could have created a copy and use a different return address but saving code size here.
  if (needlecntval != 2) {  // Const needlecnt==2?
   if (needlecntval != 3) {
    if (needlecntval == 0) beq(CCR6, L_Found); // Variable needlecnt==2?
    Register ind_reg = tmp4;
    li(ind_reg, 2*2);   // First 2 characters are already compared, use index 2.
    mtctr(needlecnt);   // Decremented by 2, still > 0.
//40:
   Label L_CompLoop;
   bind(L_CompLoop);
    lhzx(ch2, needle, ind_reg);
    lhzx(ch1, addr, ind_reg);
    cmpw(CCR1, ch1, ch2);
    bne(CCR1, L_OuterLoop);
    addi(ind_reg, ind_reg, 2);
    bdnz(L_CompLoop);
   } else { // No loop required if there's only one needle character left.
    lhz(ch2, 2*2, needle);
    lhz(ch1, 2*2, addr);
    cmpw(CCR1, ch1, ch2);
    bne(CCR1, L_OuterLoop);
   }
  }
  // Return index ...
//46:
  bind(L_Found);
   subf(addr, haystack, addr); // relative to haystack, ...
   srdi(result, addr, 1);      // in characters.
//48:
  bind(L_End);
  */
}

// Implementation of Compare for jchar arrays.
//
// Kills the registers str1, str2, cnt1, cnt2.
// Kills cr0, ctr.
// Assumes that result differes from the input registers.
void MacroAssembler::string_compare(Register str1_reg, Register str2_reg, Register cnt1_reg, Register cnt2_reg,
                                    Register result_reg, Register tmp_reg) {
  Unimplemented();
  /*
   assert_different_registers(result_reg, str1_reg, str2_reg, cnt1_reg, cnt2_reg, tmp_reg);

   Label Ldone, Lslow_case, Lslow_loop, Lfast_loop;
   Register cnt_diff = R0,
            limit_reg = cnt1_reg,
            chr1_reg = result_reg,
            chr2_reg = cnt2_reg,
            addr_diff = str2_reg;

   // Offset 0 should be 32 byte aligned.
//-4:
    dcbtct(str1_reg, 0x00);  // Indicate R/O access to str1.
    dcbtct(str2_reg, 0x00);  // Indicate R/O access to str2.
//-2:
   // Compute min(cnt1, cnt2) and check if 0 (bail out if we don't need to compare characters).
    subf(result_reg, cnt2_reg, cnt1_reg);  // difference between cnt1/2
    subf_(addr_diff, str1_reg, str2_reg);  // alias?
    beq(CCR0, Ldone);                   // return cnt difference if both ones are identical
    srawi(limit_reg, result_reg, 31);      // generate signmask (cnt1/2 must be non-negative so cnt_diff can't overflow)
    mr(cnt_diff, result_reg);
    andr(limit_reg, result_reg, limit_reg); // difference or zero (negative): cnt1<cnt2 ? cnt1-cnt2 : 0
    add_(limit_reg, cnt2_reg, limit_reg);  // min(cnt1, cnt2)==0?
    beq(CCR0, Ldone);                   // return cnt difference if one has 0 length

    lhz(chr1_reg, 0, str1_reg);            // optional: early out if first characters mismatch
    lhzx(chr2_reg, str1_reg, addr_diff);   // optional: early out if first characters mismatch
    addi(tmp_reg, limit_reg, -1);          // min(cnt1, cnt2)-1
    subf_(result_reg, chr2_reg, chr1_reg); // optional: early out if first characters mismatch
    bne(CCR0, Ldone);                   // optional: early out if first characters mismatch

   // Set loop counter by scaling down tmp_reg
    srawi_(chr2_reg, tmp_reg, exact_log2(4)); // (min(cnt1, cnt2)-1)/4
    ble(CCR0, Lslow_case);                 // need >4 characters for fast loop
    andi(limit_reg, tmp_reg, 4-1);            // remaining characters

   // Adapt str1_reg str2_reg for the first loop iteration
    mtctr(chr2_reg);                 // (min(cnt1, cnt2)-1)/4
    addi(limit_reg, limit_reg, 4+1); // compare last 5-8 characters in slow_case if mismatch found in fast_loop
//16:
   // Compare the rest of the characters
   bind(Lfast_loop);
    ld(chr1_reg, 0, str1_reg);
    ldx(chr2_reg, str1_reg, addr_diff);
    cmpd(CCR0, chr2_reg, chr1_reg);
    bne(CCR0, Lslow_case); // return chr1_reg
    addi(str1_reg, str1_reg, 4*2);
    bdnz(Lfast_loop);
    addi(limit_reg, limit_reg, -4); // no mismatch found in fast_loop, only 1-4 characters missing
//23:
   bind(Lslow_case);
    mtctr(limit_reg);
//24:
   bind(Lslow_loop);
    lhz(chr1_reg, 0, str1_reg);
    lhzx(chr2_reg, str1_reg, addr_diff);
    subf_(result_reg, chr2_reg, chr1_reg);
    bne(CCR0, Ldone); // return chr1_reg
    addi(str1_reg, str1_reg, 1*2);
    bdnz(Lslow_loop);
//30:
   // If strings are equal up to min length, return the length difference.
    mr(result_reg, cnt_diff);
    nop(); // alignment
//32:
   // Otherwise, return the difference between the first mismatched chars.
   bind(Ldone);
  */
}


// Compare char[] arrays.
//
// str1_reg   USE only
// str2_reg   USE only
// cnt_reg    USE_DEF, due to tmp reg shortage
// result_reg DEF only, might compromise USE only registers
void MacroAssembler::char_arrays_equals(Register str1_reg, Register str2_reg, Register cnt_reg, Register result_reg,
                                        Register tmp1_reg, Register tmp2_reg, Register tmp3_reg, Register tmp4_reg,
                                        Register tmp5_reg) {
  Unimplemented();
  /*

  // Str1 may be the same register as str2 which can occur e.g. after scalar replacement.
  assert_different_registers(result_reg, str1_reg, cnt_reg, tmp1_reg, tmp2_reg, tmp3_reg, tmp4_reg, tmp5_reg);
  assert_different_registers(result_reg, str2_reg, cnt_reg, tmp1_reg, tmp2_reg, tmp3_reg, tmp4_reg, tmp5_reg);

  // Offset 0 should be 32 byte aligned.
  Label Linit_cbc, Lcbc, Lloop, Ldone_true, Ldone_false;
  Register index_reg = tmp5_reg;
  Register cbc_iter  = tmp4_reg;

//-1:
  dcbtct(str1_reg, 0x00);  // Indicate R/O access to str1.
  dcbtct(str2_reg, 0x00);  // Indicate R/O access to str2.
//1:
  andi(cbc_iter, cnt_reg, 4-1);            // Remaining iterations after 4 java characters per iteration loop.
  li(index_reg, 0); // init
  li(result_reg, 0); // assume false
  srwi_(tmp2_reg, cnt_reg, exact_log2(4)); // Div: 4 java characters per iteration (main loop).

  cmpwi(CCR1, cbc_iter, 0);             // CCR1 = (cbc_iter==0)
  beq(CCR0, Linit_cbc);                 // too short
    mtctr(tmp2_reg);
//8:
    bind(Lloop);
      ldx(tmp1_reg, str1_reg, index_reg);
      ldx(tmp2_reg, str2_reg, index_reg);
      cmpd(CCR0, tmp1_reg, tmp2_reg);
      bne(CCR0, Ldone_false);  // Unequal char pair found -> done.
      addi(index_reg, index_reg, 4*sizeof(jchar));
      bdnz(Lloop);
//14:
  bind(Linit_cbc);
  beq(CCR1, Ldone_true);
    mtctr(cbc_iter);
//16:
    bind(Lcbc);
      lhzx(tmp1_reg, str1_reg, index_reg);
      lhzx(tmp2_reg, str2_reg, index_reg);
      cmpw(CCR0, tmp1_reg, tmp2_reg);
      bne(CCR0, Ldone_false);  // Unequal char pair found -> done.
      addi(index_reg, index_reg, 1*sizeof(jchar));
      bdnz(Lcbc);
    nop();
  bind(Ldone_true);
  li(result_reg, 1);
//24:
  bind(Ldone_false);
  */
}


void MacroAssembler::char_arrays_equalsImm(Register str1_reg, Register str2_reg, int cntval, Register result_reg,
                                           Register tmp1_reg, Register tmp2_reg) {
  Unimplemented();
  /*
  // Str1 may be the same register as str2 which can occur e.g. after scalar replacement.
  assert_different_registers(result_reg, str1_reg, tmp1_reg, tmp2_reg);
  assert_different_registers(result_reg, str2_reg, tmp1_reg, tmp2_reg);
  assert(sizeof(jchar) == 2, "must be");
  assert(cntval >= 0 && ((cntval & 0x7fff) == cntval), "wrong immediate");

  Label Ldone_false;

  if (cntval < 16) { // short case
    if (cntval != 0) li(result_reg, 0); // assume false

    const int num_bytes = cntval*sizeof(jchar);
    int index = 0;
    for (int next_index; (next_index = index + 8) <= num_bytes; index = next_index) {
      ld(tmp1_reg, index, str1_reg);
      ld(tmp2_reg, index, str2_reg);
      cmpd(CCR0, tmp1_reg, tmp2_reg);
      bne(CCR0, Ldone_false);
    }
    if (cntval & 2) {
      lwz(tmp1_reg, index, str1_reg);
      lwz(tmp2_reg, index, str2_reg);
      cmpw(CCR0, tmp1_reg, tmp2_reg);
      bne(CCR0, Ldone_false);
      index += 4;
    }
    if (cntval & 1) {
      lhz(tmp1_reg, index, str1_reg);
      lhz(tmp2_reg, index, str2_reg);
      cmpw(CCR0, tmp1_reg, tmp2_reg);
      bne(CCR0, Ldone_false);
    }
    // fallthrough: true
  } else {
    Label Lloop;
    Register index_reg = tmp1_reg;
    const int loopcnt = cntval/4;
    assert(loopcnt > 0, "must be");
    // Offset 0 should be 32 byte aligned.
    //2:
    dcbtct(str1_reg, 0x00);  // Indicate R/O access to str1.
    dcbtct(str2_reg, 0x00);  // Indicate R/O access to str2.
    li(tmp2_reg, loopcnt);
    li(index_reg, 0); // init
    li(result_reg, 0); // assume false
    mtctr(tmp2_reg);
    //8:
    bind(Lloop);
    ldx(R0, str1_reg, index_reg);
    ldx(tmp2_reg, str2_reg, index_reg);
    cmpd(CCR0, R0, tmp2_reg);
    bne(CCR0, Ldone_false);  // Unequal char pair found -> done.
    addi(index_reg, index_reg, 4*sizeof(jchar));
    bdnz(Lloop);
    //14:
    if (cntval & 2) {
      lwzx(R0, str1_reg, index_reg);
      lwzx(tmp2_reg, str2_reg, index_reg);
      cmpw(CCR0, R0, tmp2_reg);
      bne(CCR0, Ldone_false);
      if (cntval & 1) addi(index_reg, index_reg, 2*sizeof(jchar));
    }
    if (cntval & 1) {
      lhzx(R0, str1_reg, index_reg);
      lhzx(tmp2_reg, str2_reg, index_reg);
      cmpw(CCR0, R0, tmp2_reg);
      bne(CCR0, Ldone_false);
    }
    // fallthru: true
  }
  li(result_reg, 1);
  bind(Ldone_false);
  */
}


void MacroAssembler::asm_assert(Register v1, Register v2, bool check_equal, const char *msg, int id) {
#ifdef ASSERT
  Label ok;
  if (check_equal) {
    beq(v1, v2, ok);
  } else {
    bne(v1, v2, ok);
  }
  stop(msg, id);
  bind(ok);
#endif
}

void MacroAssembler::asm_assert_mems_zero(bool check_equal, int size, Register tmp, int mem_offset,
                                          Register mem_base, const char* msg, int id) {
#ifdef ASSERT
  switch (size) {
    case 4:
      lwu(tmp, mem_offset, mem_base);
      break;
    case 8:
      ld(tmp, mem_offset, mem_base);
      break;
    default:
      ShouldNotReachHere();
  }
  Label ok;
  if (check_equal) {
    beq(tmp, XZERO, ok);
  } else {
    bne(tmp, XZERO, ok);
  }
  stop(msg, id);
  bind(ok);
#endif // ASSERT
}

void MacroAssembler::verify_thread() {
  if (VerifyThread) {
    unimplemented("'VerifyThread' currently not implemented on RISCV");
  }
}

// READ: oop. KILL: X28. Volatile floats perhaps.
void MacroAssembler::verify_oop(Register oop, const char* msg) {
  // TODO: implement
  return;
  /*
  if (!VerifyOops) {
    return;
  }

  address*/ /* FunctionDescriptor** */ /*fd = StubRoutines::verify_oop_subroutine_entry_address();
  const Register tmp = X5; // Will be preserved.
  const int nbytes_save = 14*8; // Volatile gprs except X28.
  save_volatile_gprs(X2_SP, -nbytes_save); // except X28

  if (oop == tmp) addi(X11_ARG1, oop, 0);
  push_frame_reg_args(nbytes_save, tmp);
  // load FunctionDescriptor** / entry_address *
  load_const(tmp, fd);
  // load FunctionDescriptor* / entry_address
  ld(tmp, 0, tmp);
  if (oop != tmp) mv_if_needed(X11_ARG1, oop);
  load_const(X10_ARG0, (address) msg);
  // Call destination for its side effect.
  call_c(tmp);

  pop_frame();
  restore_volatile_gprs(X2_SP, -nbytes_save); // except X28
  */
}

const char* stop_types[] = {
  "stop",
  "untested",
  "unimplemented",
  "shouldnotreachhere"
};

static void stop_on_request(int tp, const char* msg) {
  tty->print("RISCV assembly code requires stop: (%s) %s\n", stop_types[tp%/*stop_end*/4], msg);
  guarantee(false, err_msg("RISCV assembly code requires stop: %s", msg));
}

// Call a C-function that prints output.
void MacroAssembler::stop(int type, const char* msg, int id) {
#ifndef PRODUCT
  block_comment(err_msg("stop: %s %s {", stop_types[type%stop_end], msg));
#else
  block_comment("stop {");
#endif

  // setup arguments
  li(X10_ARG0, type);
  load_const(X11_ARG1, (void *)msg);
  call_VM_leaf(CAST_FROM_FN_PTR(address, stop_on_request), X10_ARG0, X11_ARG1);
  illtrap();
  emit_int32(id);
  block_comment("} stop;");
}

#ifndef PRODUCT
// Write pattern 0x0101010101010101 in memory region [low-before, high+after].
// Val, addr are temp registers.
// If low == addr, addr is killed.
// High is preserved.
void MacroAssembler::zap_from_to(Register low, int before, Register high, int after, Register val, Register addr) {
  Unimplemented();
  /*
  if (!ZapMemory) return;

  assert_different_registers(low, val);

  BLOCK_COMMENT("zap memory region {");
  load_const_optimized(val, 0x0101010101010101);
  int size = before + after;
  if (low == high && size < 5 && size > 0) {
    int offset = -before*BytesPerWord;
    for (int i = 0; i < size; ++i) {
      sd(val, offset, low);
      offset += (1*BytesPerWord);
    }
  } else {
    addi(addr, low, -before*BytesPerWord);
    assert_different_registers(high, val);
    if (after) addi(high, high, after * BytesPerWord);
    Label loop;
    bind(loop);
    sd(val, 0, addr);
    addi(addr, addr, 8);
    cmpd(CCR6, addr, high);
    ble(CCR6, loop);
    if (after) addi(high, high, -after * BytesPerWord);  // Correct back to old value.
  }
  BLOCK_COMMENT("} zap memory region");
  */
}
void MacroAssembler::null_check(Register reg, int offset) {
  Unimplemented();
}

#endif // !PRODUCT

