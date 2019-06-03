/*
 * Copyright (c) 2002, 2013, Oracle and/or its affiliates. All rights reserved.
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

#ifndef CPU_RISCV_VM_MACROASSEMBLER_RISCV_INLINE_HPP
#define CPU_RISCV_VM_MACROASSEMBLER_RISCV_INLINE_HPP

#include "asm/assembler.inline.hpp"
#include "asm/macroAssembler.hpp"
#include "asm/codeBuffer.hpp"
#include "code/codeCache.hpp"

inline void MacroAssembler::round_to(Register r, int modulus) {
  unimplemented(__func__);
  /*
  assert(is_power_of_2_long((jlong)modulus), "must be power of 2");
  addi(r, r, modulus-1);
  clrrdi(r, r, log2_long((jlong)modulus));
  */
}

// Move register if destination register and target register are different.
inline void MacroAssembler::mv_if_needed(Register rd, Register rs) {
  if (rs != rd) mv(rd, rs);
}
inline void MacroAssembler::fmv_s_if_needed(FloatRegister rd, FloatRegister rs) {
  if (rs != rd) fmv_s(rd, rs);
}
inline void MacroAssembler::fmv_d_if_needed(FloatRegister rd, FloatRegister rs) {
  if (rs != rd) fmv_d(rd, rs);
}
inline void MacroAssembler::endgroup_if_needed(bool needed) {
  unimplemented(__func__);
  /*
  if (needed) {
    endgroup();
  }
  */
}

inline void MacroAssembler::membar(int bits) {
  unimplemented(__func__);
  /*
  // TODO: use elemental_membar(bits) for Power 8 and disable optimization of acquire-release
  // (Matcher::post_membar_release where we use RISCV64_ONLY(xop == Op_MemBarRelease ||))
  if (bits & StoreLoad) sync(); else lwsync();
  */
}

inline void MacroAssembler::release() { Assembler::fence(15, 15); }
inline void MacroAssembler::acquire() { Assembler::fence(15, 15); }
inline void MacroAssembler::fence()   { Assembler::fence(15, 15); }

#ifdef _LP64
// Detect narrow oop constants.
inline bool MacroAssembler::is_set_narrow_oop(address a, address bound) {
  return false;
  //unimplemented(__func__);
  /*
  const address inst2_addr = a;
  const int inst2 = *(int *)a;
  // The relocation points to the second instruction, the ori.
  if (!is_ori(inst2)) return false;

  // The ori reads and writes the same register dst.
  const int dst = inv_rta_field(inst2);
  if (inv_rs_field(inst2) != dst) return false;

  // Now, find the preceding addis which writes to dst.
  int inst1 = 0;
  address inst1_addr = inst2_addr - BytesPerInstWord;
  while (inst1_addr >= bound) {
    inst1 = *(int *) inst1_addr;
    if (is_lis(inst1) && inv_rs_field(inst1) == dst) return true;
    inst1_addr -= BytesPerInstWord;
  }
  return false;
  */
}
#endif


inline bool MacroAssembler::is_load_const_at(address a) {
  // Technically, this could be wrong, but its highly unlikely
  // to find this pattern anywhere except a load_const_patchable.
  // The li pseudo-op, when faced with a 64-bit case, uses different 
  // instructions, so this should fail
  const int* p = (int *) a;
  return is_lui(p[0]) && 
         is_ori(p[1]) &&
         is_slli(p[2]) && get_shamt(p[2]) == 11 &&
         is_ori(p[3]) &&
         is_slli(p[4]) && get_shamt(p[4]) == 11 &&
         is_ori(p[5]) &&
         is_slli(p[6]) && get_shamt(p[6]) == 11 &&
         is_ori(p[7]);
}

inline void MacroAssembler::set_oop_constant(jobject obj, Register d) {
  unimplemented(__func__);
  /*
  set_oop(constant_oop_address(obj), d);
  */
}

inline void MacroAssembler::set_oop(AddressLiteral obj_addr, Register d) {
  unimplemented(__func__);
  /*
  assert(obj_addr.rspec().type() == relocInfo::oop_type, "must be an oop reloc");
  load_const(d, obj_addr);
  */
}

inline void MacroAssembler::pd_patch_instruction(address branch, address target) {
  jint& stub_inst = *(jint*) branch;
  stub_inst = patched_branch((intptr_t)target, stub_inst, (intptr_t)branch);
}

inline address MacroAssembler::call_stub(Register function_entry) {
  jalr(X1_RA, function_entry, 0);
  return pc();
}

inline void MacroAssembler::call_stub_and_return_to(Register function_entry, Register return_pc) {
  assert_different_registers(function_entry, return_pc);
  // TODO: this might not be okay... we should save X1_RA
  mv(X1_RA, return_pc);
  jr(function_entry);
}

// Get the pc where the last emitted call will return to.
inline address MacroAssembler::last_calls_return_pc() {
  return _last_calls_return_pc;
}

// Read from the polling page, its address is already in a register.
inline void MacroAssembler::load_from_polling_page(Register polling_page_address, int offset) {
  unimplemented(__func__);
  /*
  ld(R0, offset, polling_page_address);
  */
}

// Trap-instruction-based checks.

inline void MacroAssembler::trap_null_check(Register a, trap_to_bits cmp) {
  unimplemented(__func__);
  /*
  assert(TrapBasedNullChecks, "sanity");
  tdi(cmp, a*/ /*reg a*/ /*, 0);
  */
}
inline void MacroAssembler::trap_zombie_not_entrant() {
  unimplemented(__func__);
  /*
  tdi(traptoUnconditional, 0*/ /*reg 0*/ /*, 1);
  */
}
inline void MacroAssembler::trap_should_not_reach_here() {
  unimplemented(__func__);
  /*
  tdi_unchecked(traptoUnconditional, 0*/ /*reg 0*/ /*, 2);
  */
}

inline void MacroAssembler::trap_ic_miss_check(Register a, Register b) {
  unimplemented(__func__);
  /*
  td(traptoGreaterThanUnsigned | traptoLessThanUnsigned, a, b);
  */
}

// Do an explicit null check if access to a+offset will not raise a SIGSEGV.
// Either issue a trap instruction that raises SIGTRAP, or do a compare that
// branches to exception_entry.
// No support for compressed oops (base page of heap). Does not distinguish
// loads and stores.
inline void MacroAssembler::null_check_throw(Register a, int offset, Register temp_reg,
                                             address exception_entry) {
  unimplemented(__func__);
  /*
  if (!ImplicitNullChecks || needs_explicit_null_check(offset) || !os::zero_page_read_protected()) {
    if (TrapBasedNullChecks) {
      assert(UseSIGTRAP, "sanity");
      trap_null_check(a);
    } else {
      Label ok;
      bne(a, X0_ZERO, ok);
      load_const(temp_reg, exception_entry);
      jr(temp_reg);
      bind(ok);
    }
  }
  */
}

inline void MacroAssembler::load_with_trap_null_check(Register d, int si16, Register s1) {
  unimplemented(__func__);
  /*
  if (!os::zero_page_read_protected()) {
    if (TrapBasedNullChecks) {
      trap_null_check(s1);
    }
  }
  ld(d, si16, s1);
  */
}

inline void MacroAssembler::load_heap_oop_not_null(Register d, RegisterOrConstant offs, Register s1) {
  unimplemented(__func__);
  /*
  if (UseCompressedOops) {
    lwz(d, offs, s1);
    // Attention: no null check here!
    decode_heap_oop_not_null(d);
  } else {
    ld(d, offs, s1);
  }
  */
}

inline void MacroAssembler::store_heap_oop_not_null(Register d, RegisterOrConstant offs, Register s1, Register tmp) {
  unimplemented(__func__);
  /*
  if (UseCompressedOops) {
    Register compressedOop = encode_heap_oop_not_null((tmp != noreg) ? tmp : d, d);
    stw(compressedOop, offs, s1);
  } else {
    std(d, offs, s1);
  }
  */
}

inline void MacroAssembler::load_heap_oop(Register d, RegisterOrConstant offs, Register s1) {
  unimplemented(__func__);
  /*
  if (UseCompressedOops) {
    lwu(d, offs, s1);
    decode_heap_oop(d);
  } else {
    ld(d, offs, s1);
  }
  */
}

inline Register MacroAssembler::encode_heap_oop_not_null(Register d, Register src) {
  unimplemented(__func__);
  /*
  Register current = (src!=noreg) ? src : d; // Compressed oop is in d if no src provided.
  if (Universe::narrow_oop_base() != NULL) {
    sub(d, current, R30);
    current = d;
  }
  if (Universe::narrow_oop_shift() != 0) {
    srdi(d, current, LogMinObjAlignmentInBytes);
    current = d;
  }
  return current; // Encoded oop is in this register.
  */
  return d;
}

inline void MacroAssembler::decode_heap_oop_not_null(Register d) {
  unimplemented(__func__);
  /*
  if (Universe::narrow_oop_shift() != 0) {
    assert (LogMinObjAlignmentInBytes == Universe::narrow_oop_shift(), "decode alg wrong");
    sldi(d, d, LogMinObjAlignmentInBytes);
  }
  if (Universe::narrow_oop_base() != NULL) {
    add(d, d, X27);
  }
  */
}

inline void MacroAssembler::decode_heap_oop(Register d) {
  unimplemented(__func__);
  /*
  Label isNull;
  if (Universe::narrow_oop_base() != NULL) {
    beq(d, X0_ZERO, isNull);
  }
  if (Universe::narrow_oop_shift() != 0) {
    assert (LogMinObjAlignmentInBytes == Universe::narrow_oop_shift(), "decode alg wrong");
    slli(d, d, LogMinObjAlignmentInBytes);
  }
  if (Universe::narrow_oop_base() != NULL) {
    add(d, d, X27);
  }
  bind(isNull);
  */
}

// SIGTRAP-based range checks for arrays.
inline void MacroAssembler::trap_range_check_l(Register a, Register b) {
  unimplemented(__func__);
  /*
  tw (traptoLessThanUnsigned,                  a*/ /*reg a*/ /*, b*/ /*reg b*/ /*);
  */
}
inline void MacroAssembler::trap_range_check_l(Register a, int si16) {
  unimplemented(__func__);
  /*
  twi(traptoLessThanUnsigned,                  a*/ /*reg a*/ /*, si16);
  */
}
inline void MacroAssembler::trap_range_check_le(Register a, int si16) {
  unimplemented(__func__);
  /*
  twi(traptoEqual | traptoLessThanUnsigned,    a*/ /*reg a*/ /*, si16);
  */
}
inline void MacroAssembler::trap_range_check_g(Register a, int si16) {
  unimplemented(__func__);
  /*
  twi(traptoGreaterThanUnsigned,               a*/ /*reg a*/ /*, si16);
  */
}
inline void MacroAssembler::trap_range_check_ge(Register a, Register b) {
  unimplemented(__func__);
  /*
  tw (traptoEqual | traptoGreaterThanUnsigned, a*/ /*reg a*/ /*, b*/ /*reg b*/ /*);
  */
}
inline void MacroAssembler::trap_range_check_ge(Register a, int si16) {
  unimplemented(__func__);
  /*
  twi(traptoEqual | traptoGreaterThanUnsigned, a*/ /*reg a*/ /*, si16);
  */
}

inline address MacroAssembler::function_entry() { return pc(); }

#endif // CPU_RISCV_VM_MACROASSEMBLER_RISCV_INLINE_HPP
