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

#ifndef CPU_RISCV_VM_ASSEMBLER_RISCV_INLINE_HPP
#define CPU_RISCV_VM_ASSEMBLER_RISCV_INLINE_HPP

#include "asm/assembler.inline.hpp"
#include "asm/codeBuffer.hpp"
#include "code/codeCache.hpp"

inline void Assembler::emit_int32(int x) {
#ifdef ASSERT
  if (debug_is_emitting()) {
    fprintf(stderr, "Emitting: ");
    print_instruction(x);
    fprintf(stderr, "\n");
  }
#endif
  AbstractAssembler::emit_int32(x);
}

inline void Assembler::bytecode_marker(uint16_t code) {
#ifdef ASSERT
  // Emit a load from address 0 to force a trap
  Assembler::lb(XZERO, code, XZERO);
#endif
}

inline void Assembler::print_tos() {
  Assembler::lb(XZERO, 0xff, XZERO);
}

inline void Assembler::pre_inst() {
#ifdef ASSERT
  if (debug_is_tracing()) {
    // Emit a load from address 0 to force a trap
    AbstractAssembler::emit_int32(MATCH_LB);
  }
#endif
}

inline void Assembler::emit_data(int x) {
  emit_int32(x);
}

inline void Assembler::emit_data(int x, relocInfo::relocType rtype) {
  relocate(rtype);
  emit_int32(x);
}

inline void Assembler::emit_data(int x, RelocationHolder const& rspec) {
  relocate(rspec);
  emit_int32(x);
}

// Emit an address
inline address Assembler::emit_addr(const address addr) {
  address start = pc();
  emit_address(addr);
  return start;
}

// Assembler functions are actually just inlined in assembler.hpp

// Issue an illegal instruction. 0 is guaranteed to be an illegal instruction.
inline void Assembler::illtrap() { Assembler::emit_int32(0); }
inline bool Assembler::is_illtrap(int x) { return x == 0; }

inline void Assembler::load_const(Register d, void* x) {
   load_const_patchable(d, (intptr_t) x);
}

// Load a 64 bit constant encoded by a `Label'. This works for bound
// labels as well as unbound ones. For unbound labels, the code will
// be patched as soon as the label gets bound.
inline void Assembler::load_const(Register d, Label& L) {
  load_const_patchable(d, (intptr_t)target(L));
}

// Load a 64 bit constant encoded by an AddressLiteral. patchable.
inline void Assembler::load_const(Register d, AddressLiteral& a) {
  assert(d != X0, "X0 not allowed");
  // First relocate (we don't change the offset in the RelocationHolder,
  // just pass a.rspec()), then delegate to load_const(Register, long).
  relocate(a.rspec());
  load_const_patchable(d, (intptr_t)a.value());
}

// Instruction Emitters -- Generated
inline void Assembler::beq      (int Cbimm12hi, Register Crs1, Register Crs2, int Cbimm12lo) { pre_inst(); emit_int32(MATCH_BEQ|bimm12hi(Cbimm12hi)|rs1(Crs1)|rs2(Crs2)|bimm12lo(Cbimm12lo)); }
inline void Assembler::bne      (int Cbimm12hi, Register Crs1, Register Crs2, int Cbimm12lo) { pre_inst(); emit_int32(MATCH_BNE|bimm12hi(Cbimm12hi)|rs1(Crs1)|rs2(Crs2)|bimm12lo(Cbimm12lo)); }
inline void Assembler::blt      (int Cbimm12hi, Register Crs1, Register Crs2, int Cbimm12lo) { pre_inst(); emit_int32(MATCH_BLT|bimm12hi(Cbimm12hi)|rs1(Crs1)|rs2(Crs2)|bimm12lo(Cbimm12lo)); }
inline void Assembler::bge      (int Cbimm12hi, Register Crs1, Register Crs2, int Cbimm12lo) { pre_inst(); emit_int32(MATCH_BGE|bimm12hi(Cbimm12hi)|rs1(Crs1)|rs2(Crs2)|bimm12lo(Cbimm12lo)); }
inline void Assembler::bltu     (int Cbimm12hi, Register Crs1, Register Crs2, int Cbimm12lo) { pre_inst(); emit_int32(MATCH_BLTU|bimm12hi(Cbimm12hi)|rs1(Crs1)|rs2(Crs2)|bimm12lo(Cbimm12lo)); }
inline void Assembler::bgeu     (int Cbimm12hi, Register Crs1, Register Crs2, int Cbimm12lo) { pre_inst(); emit_int32(MATCH_BGEU|bimm12hi(Cbimm12hi)|rs1(Crs1)|rs2(Crs2)|bimm12lo(Cbimm12lo)); }
inline void Assembler::jalr     (Register Crd , Register Crs1, int Cimm12   ) { pre_inst(); emit_int32(MATCH_JALR|rd(Crd)|rs1(Crs1)|imm12(Cimm12)); }
inline void Assembler::jal      (Register Crd , int Cjimm20  ) { pre_inst(); emit_int32(MATCH_JAL|rd(Crd)|jimm20_disp(Cjimm20)); }
inline void Assembler::lui      (Register Crd , int Cimm20   ) { pre_inst(); emit_int32(MATCH_LUI|rd(Crd)|imm20(Cimm20)); }
inline void Assembler::auipc    (Register Crd , int Cimm20   ) { pre_inst(); emit_int32(MATCH_AUIPC|rd(Crd)|imm20(Cimm20)); }
inline void Assembler::addi     (Register Crd , Register Crs1, int Cimm12   ) { pre_inst(); emit_int32(MATCH_ADDI|rd(Crd)|rs1(Crs1)|imm12(Cimm12)); }
inline void Assembler::slli     (Register Crd , Register Crs1, int Cshamt   ) { pre_inst(); emit_int32(MATCH_SLLI|rd(Crd)|rs1(Crs1)|shamt(Cshamt)); }
inline void Assembler::slti     (Register Crd , Register Crs1, int Cimm12   ) { pre_inst(); emit_int32(MATCH_SLTI|rd(Crd)|rs1(Crs1)|imm12(Cimm12)); }
inline void Assembler::sltiu    (Register Crd , Register Crs1, int Cimm12   ) { pre_inst(); emit_int32(MATCH_SLTIU|rd(Crd)|rs1(Crs1)|imm12(Cimm12)); }
inline void Assembler::xori     (Register Crd , Register Crs1, int Cimm12   ) { pre_inst(); emit_int32(MATCH_XORI|rd(Crd)|rs1(Crs1)|imm12(Cimm12)); }
inline void Assembler::srli     (Register Crd , Register Crs1, int Cshamt   ) { pre_inst(); emit_int32(MATCH_SRLI|rd(Crd)|rs1(Crs1)|shamt(Cshamt)); }
inline void Assembler::srai     (Register Crd , Register Crs1, int Cshamt   ) { pre_inst(); emit_int32(MATCH_SRAI|rd(Crd)|rs1(Crs1)|shamt(Cshamt)); }
inline void Assembler::ori      (Register Crd , Register Crs1, int Cimm12   ) { pre_inst(); emit_int32(MATCH_ORI|rd(Crd)|rs1(Crs1)|imm12(Cimm12)); }
inline void Assembler::andi     (Register Crd , Register Crs1, int Cimm12   ) { pre_inst(); emit_int32(MATCH_ANDI|rd(Crd)|rs1(Crs1)|imm12(Cimm12)); }
inline void Assembler::add      (Register Crd , Register Crs1, Register Crs2) { pre_inst(); emit_int32(MATCH_ADD|rd(Crd)|rs1(Crs1)|rs2(Crs2)); }
inline void Assembler::sub      (Register Crd , Register Crs1, Register Crs2) { pre_inst(); emit_int32(MATCH_SUB|rd(Crd)|rs1(Crs1)|rs2(Crs2)); }
inline void Assembler::sll      (Register Crd , Register Crs1, Register Crs2) { pre_inst(); emit_int32(MATCH_SLL|rd(Crd)|rs1(Crs1)|rs2(Crs2)); }
inline void Assembler::slt      (Register Crd , Register Crs1, Register Crs2) { pre_inst(); emit_int32(MATCH_SLT|rd(Crd)|rs1(Crs1)|rs2(Crs2)); }
inline void Assembler::sltu     (Register Crd , Register Crs1, Register Crs2) { pre_inst(); emit_int32(MATCH_SLTU|rd(Crd)|rs1(Crs1)|rs2(Crs2)); }
inline void Assembler::xor_     (Register Crd , Register Crs1, Register Crs2) { pre_inst(); emit_int32(MATCH_XOR_|rd(Crd)|rs1(Crs1)|rs2(Crs2)); }
inline void Assembler::srl      (Register Crd , Register Crs1, Register Crs2) { pre_inst(); emit_int32(MATCH_SRL|rd(Crd)|rs1(Crs1)|rs2(Crs2)); }
inline void Assembler::sra      (Register Crd , Register Crs1, Register Crs2) { pre_inst(); emit_int32(MATCH_SRA|rd(Crd)|rs1(Crs1)|rs2(Crs2)); }
inline void Assembler::or_      (Register Crd , Register Crs1, Register Crs2) { pre_inst(); emit_int32(MATCH_OR_|rd(Crd)|rs1(Crs1)|rs2(Crs2)); }
inline void Assembler::and_     (Register Crd , Register Crs1, Register Crs2) { pre_inst(); emit_int32(MATCH_AND_|rd(Crd)|rs1(Crs1)|rs2(Crs2)); }
inline void Assembler::addiw    (Register Crd , Register Crs1, int Cimm12   ) { pre_inst(); emit_int32(MATCH_ADDIW|rd(Crd)|rs1(Crs1)|imm12(Cimm12)); }
inline void Assembler::slliw    (Register Crd , Register Crs1, int Cshamtw  ) { pre_inst(); emit_int32(MATCH_SLLIW|rd(Crd)|rs1(Crs1)|shamtw(Cshamtw)); }
inline void Assembler::srliw    (Register Crd , Register Crs1, int Cshamtw  ) { pre_inst(); emit_int32(MATCH_SRLIW|rd(Crd)|rs1(Crs1)|shamtw(Cshamtw)); }
inline void Assembler::sraiw    (Register Crd , Register Crs1, int Cshamtw  ) { pre_inst(); emit_int32(MATCH_SRAIW|rd(Crd)|rs1(Crs1)|shamtw(Cshamtw)); }
inline void Assembler::addw     (Register Crd , Register Crs1, Register Crs2) { pre_inst(); emit_int32(MATCH_ADDW|rd(Crd)|rs1(Crs1)|rs2(Crs2)); }
inline void Assembler::subw     (Register Crd , Register Crs1, Register Crs2) { pre_inst(); emit_int32(MATCH_SUBW|rd(Crd)|rs1(Crs1)|rs2(Crs2)); }
inline void Assembler::sllw     (Register Crd , Register Crs1, Register Crs2) { pre_inst(); emit_int32(MATCH_SLLW|rd(Crd)|rs1(Crs1)|rs2(Crs2)); }
inline void Assembler::srlw     (Register Crd , Register Crs1, Register Crs2) { pre_inst(); emit_int32(MATCH_SRLW|rd(Crd)|rs1(Crs1)|rs2(Crs2)); }
inline void Assembler::sraw     (Register Crd , Register Crs1, Register Crs2) { pre_inst(); emit_int32(MATCH_SRAW|rd(Crd)|rs1(Crs1)|rs2(Crs2)); }
inline void Assembler::lb       (Register Crd , Register Crs1, int Cimm12   ) { pre_inst(); emit_int32(MATCH_LB|rd(Crd)|rs1(Crs1)|imm12(Cimm12)); }
inline void Assembler::lh       (Register Crd , Register Crs1, int Cimm12   ) { pre_inst(); emit_int32(MATCH_LH|rd(Crd)|rs1(Crs1)|imm12(Cimm12)); }
inline void Assembler::lw       (Register Crd , Register Crs1, int Cimm12   ) { pre_inst(); emit_int32(MATCH_LW|rd(Crd)|rs1(Crs1)|imm12(Cimm12)); }
inline void Assembler::ld       (Register Crd , Register Crs1, int Cimm12   ) { pre_inst(); emit_int32(MATCH_LD|rd(Crd)|rs1(Crs1)|imm12(Cimm12)); }
inline void Assembler::lbu      (Register Crd , Register Crs1, int Cimm12   ) { pre_inst(); emit_int32(MATCH_LBU|rd(Crd)|rs1(Crs1)|imm12(Cimm12)); }
inline void Assembler::lhu      (Register Crd , Register Crs1, int Cimm12   ) { pre_inst(); emit_int32(MATCH_LHU|rd(Crd)|rs1(Crs1)|imm12(Cimm12)); }
inline void Assembler::lwu      (Register Crd , Register Crs1, int Cimm12   ) { pre_inst(); emit_int32(MATCH_LWU|rd(Crd)|rs1(Crs1)|imm12(Cimm12)); }
inline void Assembler::sb       (int Cimm12hi , Register Crs1, Register Crs2, int Cimm12lo ) { pre_inst(); emit_int32(MATCH_SB|imm12hi(Cimm12hi)|rs1(Crs1)|rs2(Crs2)|imm12lo(Cimm12lo)); }
inline void Assembler::sh       (int Cimm12hi , Register Crs1, Register Crs2, int Cimm12lo ) { pre_inst(); emit_int32(MATCH_SH|imm12hi(Cimm12hi)|rs1(Crs1)|rs2(Crs2)|imm12lo(Cimm12lo)); }
inline void Assembler::sw       (int Cimm12hi , Register Crs1, Register Crs2, int Cimm12lo ) { pre_inst(); emit_int32(MATCH_SW|imm12hi(Cimm12hi)|rs1(Crs1)|rs2(Crs2)|imm12lo(Cimm12lo)); }
inline void Assembler::sd       (int Cimm12hi , Register Crs1, Register Crs2, int Cimm12lo ) { pre_inst(); emit_int32(MATCH_SD|imm12hi(Cimm12hi)|rs1(Crs1)|rs2(Crs2)|imm12lo(Cimm12lo)); }
inline void Assembler::fence    (int Cpred         , int Csucc         ) { pre_inst(); emit_int32(MATCH_FENCE|pred(Cpred)|succ(Csucc)); }
inline void Assembler::fence_i  () { pre_inst(); emit_int32(MATCH_FENCE_I); }
inline void Assembler::mul      (Register Crd , Register Crs1, Register Crs2) { pre_inst(); emit_int32(MATCH_MUL|rd(Crd)|rs1(Crs1)|rs2(Crs2)); }
inline void Assembler::mulh     (Register Crd , Register Crs1, Register Crs2) { pre_inst(); emit_int32(MATCH_MULH|rd(Crd)|rs1(Crs1)|rs2(Crs2)); }
inline void Assembler::mulhsu   (Register Crd , Register Crs1, Register Crs2) { pre_inst(); emit_int32(MATCH_MULHSU|rd(Crd)|rs1(Crs1)|rs2(Crs2)); }
inline void Assembler::mulhu    (Register Crd , Register Crs1, Register Crs2) { pre_inst(); emit_int32(MATCH_MULHU|rd(Crd)|rs1(Crs1)|rs2(Crs2)); }
inline void Assembler::div      (Register Crd , Register Crs1, Register Crs2) { pre_inst(); emit_int32(MATCH_DIV|rd(Crd)|rs1(Crs1)|rs2(Crs2)); }
inline void Assembler::divu     (Register Crd , Register Crs1, Register Crs2) { pre_inst(); emit_int32(MATCH_DIVU|rd(Crd)|rs1(Crs1)|rs2(Crs2)); }
inline void Assembler::rem      (Register Crd , Register Crs1, Register Crs2) { pre_inst(); emit_int32(MATCH_REM|rd(Crd)|rs1(Crs1)|rs2(Crs2)); }
inline void Assembler::remu     (Register Crd , Register Crs1, Register Crs2) { pre_inst(); emit_int32(MATCH_REMU|rd(Crd)|rs1(Crs1)|rs2(Crs2)); }
inline void Assembler::mulw     (Register Crd , Register Crs1, Register Crs2) { pre_inst(); emit_int32(MATCH_MULW|rd(Crd)|rs1(Crs1)|rs2(Crs2)); }
inline void Assembler::divw     (Register Crd , Register Crs1, Register Crs2) { pre_inst(); emit_int32(MATCH_DIVW|rd(Crd)|rs1(Crs1)|rs2(Crs2)); }
inline void Assembler::divuw    (Register Crd , Register Crs1, Register Crs2) { pre_inst(); emit_int32(MATCH_DIVUW|rd(Crd)|rs1(Crs1)|rs2(Crs2)); }
inline void Assembler::remw     (Register Crd , Register Crs1, Register Crs2) { pre_inst(); emit_int32(MATCH_REMW|rd(Crd)|rs1(Crs1)|rs2(Crs2)); }
inline void Assembler::remuw    (Register Crd , Register Crs1, Register Crs2) { pre_inst(); emit_int32(MATCH_REMUW|rd(Crd)|rs1(Crs1)|rs2(Crs2)); }
inline void Assembler::amoadd_w (Register Crd , Register Crs1, Register Crs2, int Caqrl    ) { pre_inst(); emit_int32(MATCH_AMOADD_W|rd(Crd)|rs1(Crs1)|rs2(Crs2)|aqrl(Caqrl)); }
inline void Assembler::amoxor_w (Register Crd , Register Crs1, Register Crs2, int Caqrl    ) { pre_inst(); emit_int32(MATCH_AMOXOR_W|rd(Crd)|rs1(Crs1)|rs2(Crs2)|aqrl(Caqrl)); }
inline void Assembler::amoor_w  (Register Crd , Register Crs1, Register Crs2, int Caqrl    ) { pre_inst(); emit_int32(MATCH_AMOOR_W|rd(Crd)|rs1(Crs1)|rs2(Crs2)|aqrl(Caqrl)); }
inline void Assembler::amoand_w (Register Crd , Register Crs1, Register Crs2, int Caqrl    ) { pre_inst(); emit_int32(MATCH_AMOAND_W|rd(Crd)|rs1(Crs1)|rs2(Crs2)|aqrl(Caqrl)); }
inline void Assembler::amomin_w (Register Crd , Register Crs1, Register Crs2, int Caqrl    ) { pre_inst(); emit_int32(MATCH_AMOMIN_W|rd(Crd)|rs1(Crs1)|rs2(Crs2)|aqrl(Caqrl)); }
inline void Assembler::amomax_w (Register Crd , Register Crs1, Register Crs2, int Caqrl    ) { pre_inst(); emit_int32(MATCH_AMOMAX_W|rd(Crd)|rs1(Crs1)|rs2(Crs2)|aqrl(Caqrl)); }
inline void Assembler::amominu_w(Register Crd , Register Crs1, Register Crs2, int Caqrl    ) { pre_inst(); emit_int32(MATCH_AMOMINU_W|rd(Crd)|rs1(Crs1)|rs2(Crs2)|aqrl(Caqrl)); }
inline void Assembler::amomaxu_w(Register Crd , Register Crs1, Register Crs2, int Caqrl    ) { pre_inst(); emit_int32(MATCH_AMOMAXU_W|rd(Crd)|rs1(Crs1)|rs2(Crs2)|aqrl(Caqrl)); }
inline void Assembler::amoswap_w(Register Crd , Register Crs1, Register Crs2, int Caqrl    ) { pre_inst(); emit_int32(MATCH_AMOSWAP_W|rd(Crd)|rs1(Crs1)|rs2(Crs2)|aqrl(Caqrl)); }
inline void Assembler::lr_w     (Register Crd , Register Crs1, int Caqrl    ) { pre_inst(); emit_int32(MATCH_LR_W|rd(Crd)|rs1(Crs1)|aqrl(Caqrl)); }
inline void Assembler::sc_w     (Register Crd , Register Crs1, Register Crs2, int Caqrl    ) { pre_inst(); emit_int32(MATCH_SC_W|rd(Crd)|rs1(Crs1)|rs2(Crs2)|aqrl(Caqrl)); }
inline void Assembler::amoadd_d (Register Crd , Register Crs1, Register Crs2, int Caqrl    ) { pre_inst(); emit_int32(MATCH_AMOADD_D|rd(Crd)|rs1(Crs1)|rs2(Crs2)|aqrl(Caqrl)); }
inline void Assembler::amoxor_d (Register Crd , Register Crs1, Register Crs2, int Caqrl    ) { pre_inst(); emit_int32(MATCH_AMOXOR_D|rd(Crd)|rs1(Crs1)|rs2(Crs2)|aqrl(Caqrl)); }
inline void Assembler::amoor_d  (Register Crd , Register Crs1, Register Crs2, int Caqrl    ) { pre_inst(); emit_int32(MATCH_AMOOR_D|rd(Crd)|rs1(Crs1)|rs2(Crs2)|aqrl(Caqrl)); }
inline void Assembler::amoand_d (Register Crd , Register Crs1, Register Crs2, int Caqrl    ) { pre_inst(); emit_int32(MATCH_AMOAND_D|rd(Crd)|rs1(Crs1)|rs2(Crs2)|aqrl(Caqrl)); }
inline void Assembler::amomin_d (Register Crd , Register Crs1, Register Crs2, int Caqrl    ) { pre_inst(); emit_int32(MATCH_AMOMIN_D|rd(Crd)|rs1(Crs1)|rs2(Crs2)|aqrl(Caqrl)); }
inline void Assembler::amomax_d (Register Crd , Register Crs1, Register Crs2, int Caqrl    ) { pre_inst(); emit_int32(MATCH_AMOMAX_D|rd(Crd)|rs1(Crs1)|rs2(Crs2)|aqrl(Caqrl)); }
inline void Assembler::amominu_d(Register Crd , Register Crs1, Register Crs2, int Caqrl    ) { pre_inst(); emit_int32(MATCH_AMOMINU_D|rd(Crd)|rs1(Crs1)|rs2(Crs2)|aqrl(Caqrl)); }
inline void Assembler::amomaxu_d(Register Crd , Register Crs1, Register Crs2, int Caqrl    ) { pre_inst(); emit_int32(MATCH_AMOMAXU_D|rd(Crd)|rs1(Crs1)|rs2(Crs2)|aqrl(Caqrl)); }
inline void Assembler::amoswap_d(Register Crd , Register Crs1, Register Crs2, int Caqrl    ) { pre_inst(); emit_int32(MATCH_AMOSWAP_D|rd(Crd)|rs1(Crs1)|rs2(Crs2)|aqrl(Caqrl)); }
inline void Assembler::lr_d     (Register Crd , Register Crs1, int Caqrl    ) { pre_inst(); emit_int32(MATCH_LR_D|rd(Crd)|rs1(Crs1)|aqrl(Caqrl)); }
inline void Assembler::sc_d     (Register Crd , Register Crs1, Register Crs2, int Caqrl    ) { pre_inst(); emit_int32(MATCH_SC_D|rd(Crd)|rs1(Crs1)|rs2(Crs2)|aqrl(Caqrl)); }
inline void Assembler::scall    () { pre_inst(); emit_int32(MATCH_SCALL); }
inline void Assembler::sbreak   () { pre_inst(); emit_int32(MATCH_SBREAK); }
inline void Assembler::sret     () { pre_inst(); emit_int32(MATCH_SRET); }
inline void Assembler::sfence_vm(Register Crs1) { pre_inst(); emit_int32(MATCH_SFENCE_VM|rs1(Crs1)); }
inline void Assembler::wfi      () { pre_inst(); emit_int32(MATCH_WFI); }
inline void Assembler::mrth     () { pre_inst(); emit_int32(MATCH_MRTH); }
inline void Assembler::mrts     () { pre_inst(); emit_int32(MATCH_MRTS); }
inline void Assembler::hrts     () { pre_inst(); emit_int32(MATCH_HRTS); }
inline void Assembler::csrrw    (Register Crd , Register Crs1, int Cimm12   ) { pre_inst(); emit_int32(MATCH_CSRRW|rd(Crd)|rs1(Crs1)|imm12(Cimm12)); }
inline void Assembler::csrrs    (Register Crd , Register Crs1, int Cimm12   ) { pre_inst(); emit_int32(MATCH_CSRRS|rd(Crd)|rs1(Crs1)|imm12(Cimm12)); }
inline void Assembler::csrrc    (Register Crd , Register Crs1, int Cimm12   ) { pre_inst(); emit_int32(MATCH_CSRRC|rd(Crd)|rs1(Crs1)|imm12(Cimm12)); }
inline void Assembler::csrrwi   (Register Crd , Register Crs1, int Cimm12   ) { pre_inst(); emit_int32(MATCH_CSRRWI|rd(Crd)|rs1(Crs1)|imm12(Cimm12)); }
inline void Assembler::csrrsi   (Register Crd , Register Crs1, int Cimm12   ) { pre_inst(); emit_int32(MATCH_CSRRSI|rd(Crd)|rs1(Crs1)|imm12(Cimm12)); }
inline void Assembler::csrrci   (Register Crd , Register Crs1, int Cimm12   ) { pre_inst(); emit_int32(MATCH_CSRRCI|rd(Crd)|rs1(Crs1)|imm12(Cimm12)); }
inline void Assembler::fadd_s   (FloatRegister Crd , FloatRegister Crs1, FloatRegister Crs2, int Crm           ) { pre_inst(); emit_int32(MATCH_FADD_S|rd(Crd)|rs1(Crs1)|rs2(Crs2)|rm(Crm)); }
inline void Assembler::fsub_s   (FloatRegister Crd , FloatRegister Crs1, FloatRegister Crs2, int Crm           ) { pre_inst(); emit_int32(MATCH_FSUB_S|rd(Crd)|rs1(Crs1)|rs2(Crs2)|rm(Crm)); }
inline void Assembler::fmul_s   (FloatRegister Crd , FloatRegister Crs1, FloatRegister Crs2, int Crm           ) { pre_inst(); emit_int32(MATCH_FMUL_S|rd(Crd)|rs1(Crs1)|rs2(Crs2)|rm(Crm)); }
inline void Assembler::fdiv_s   (FloatRegister Crd , FloatRegister Crs1, FloatRegister Crs2, int Crm           ) { pre_inst(); emit_int32(MATCH_FDIV_S|rd(Crd)|rs1(Crs1)|rs2(Crs2)|rm(Crm)); }
inline void Assembler::fsgnj_s  (FloatRegister Crd , FloatRegister Crs1, FloatRegister Crs2) { pre_inst(); emit_int32(MATCH_FSGNJ_S|rd(Crd)|rs1(Crs1)|rs2(Crs2)); }
inline void Assembler::fsgnjn_s (FloatRegister Crd , FloatRegister Crs1, FloatRegister Crs2) { pre_inst(); emit_int32(MATCH_FSGNJN_S|rd(Crd)|rs1(Crs1)|rs2(Crs2)); }
inline void Assembler::fsgnjx_s (FloatRegister Crd , FloatRegister Crs1, FloatRegister Crs2) { pre_inst(); emit_int32(MATCH_FSGNJX_S|rd(Crd)|rs1(Crs1)|rs2(Crs2)); }
inline void Assembler::fmin_s   (FloatRegister Crd , FloatRegister Crs1, FloatRegister Crs2) { pre_inst(); emit_int32(MATCH_FMIN_S|rd(Crd)|rs1(Crs1)|rs2(Crs2)); }
inline void Assembler::fmax_s   (FloatRegister Crd , FloatRegister Crs1, FloatRegister Crs2) { pre_inst(); emit_int32(MATCH_FMAX_S|rd(Crd)|rs1(Crs1)|rs2(Crs2)); }
inline void Assembler::fsqrt_s  (FloatRegister Crd , FloatRegister Crs1, int Crm           ) { pre_inst(); emit_int32(MATCH_FSQRT_S|rd(Crd)|rs1(Crs1)|rm(Crm)); }
inline void Assembler::fadd_d   (FloatRegister Crd , FloatRegister Crs1, FloatRegister Crs2, int Crm           ) { pre_inst(); emit_int32(MATCH_FADD_D|rd(Crd)|rs1(Crs1)|rs2(Crs2)|rm(Crm)); }
inline void Assembler::fsub_d   (FloatRegister Crd , FloatRegister Crs1, FloatRegister Crs2, int Crm           ) { pre_inst(); emit_int32(MATCH_FSUB_D|rd(Crd)|rs1(Crs1)|rs2(Crs2)|rm(Crm)); }
inline void Assembler::fmul_d   (FloatRegister Crd , FloatRegister Crs1, FloatRegister Crs2, int Crm           ) { pre_inst(); emit_int32(MATCH_FMUL_D|rd(Crd)|rs1(Crs1)|rs2(Crs2)|rm(Crm)); }
inline void Assembler::fdiv_d   (FloatRegister Crd , FloatRegister Crs1, FloatRegister Crs2, int Crm           ) { pre_inst(); emit_int32(MATCH_FDIV_D|rd(Crd)|rs1(Crs1)|rs2(Crs2)|rm(Crm)); }
inline void Assembler::fsgnj_d  (FloatRegister Crd , FloatRegister Crs1, FloatRegister Crs2) { pre_inst(); emit_int32(MATCH_FSGNJ_D|rd(Crd)|rs1(Crs1)|rs2(Crs2)); }
inline void Assembler::fsgnjn_d (FloatRegister Crd , FloatRegister Crs1, FloatRegister Crs2) { pre_inst(); emit_int32(MATCH_FSGNJN_D|rd(Crd)|rs1(Crs1)|rs2(Crs2)); }
inline void Assembler::fsgnjx_d (FloatRegister Crd , FloatRegister Crs1, FloatRegister Crs2) { pre_inst(); emit_int32(MATCH_FSGNJX_D|rd(Crd)|rs1(Crs1)|rs2(Crs2)); }
inline void Assembler::fmin_d   (FloatRegister Crd , FloatRegister Crs1, FloatRegister Crs2) { pre_inst(); emit_int32(MATCH_FMIN_D|rd(Crd)|rs1(Crs1)|rs2(Crs2)); }
inline void Assembler::fmax_d   (FloatRegister Crd , FloatRegister Crs1, FloatRegister Crs2) { pre_inst(); emit_int32(MATCH_FMAX_D|rd(Crd)|rs1(Crs1)|rs2(Crs2)); }
inline void Assembler::fcvt_s_d (FloatRegister Crd , FloatRegister Crs1, int Crm           ) { pre_inst(); emit_int32(MATCH_FCVT_S_D|rd(Crd)|rs1(Crs1)|rm(Crm)); }
inline void Assembler::fcvt_d_s (FloatRegister Crd , FloatRegister Crs1, int Crm           ) { pre_inst(); emit_int32(MATCH_FCVT_D_S|rd(Crd)|rs1(Crs1)|rm(Crm)); }
inline void Assembler::fsqrt_d  (FloatRegister Crd , FloatRegister Crs1, int Crm           ) { pre_inst(); emit_int32(MATCH_FSQRT_D|rd(Crd)|rs1(Crs1)|rm(Crm)); }
inline void Assembler::fle_s    (Register Crd      , FloatRegister Crs1, FloatRegister Crs2) { pre_inst(); emit_int32(MATCH_FLE_S|rd(Crd)|rs1(Crs1)|rs2(Crs2)); }
inline void Assembler::flt_s    (Register Crd      , FloatRegister Crs1, FloatRegister Crs2) { pre_inst(); emit_int32(MATCH_FLT_S|rd(Crd)|rs1(Crs1)|rs2(Crs2)); }
inline void Assembler::feq_s    (Register Crd      , FloatRegister Crs1, FloatRegister Crs2) { pre_inst(); emit_int32(MATCH_FEQ_S|rd(Crd)|rs1(Crs1)|rs2(Crs2)); }
inline void Assembler::fle_d    (Register Crd      , FloatRegister Crs1, FloatRegister Crs2) { pre_inst(); emit_int32(MATCH_FLE_D|rd(Crd)|rs1(Crs1)|rs2(Crs2)); }
inline void Assembler::flt_d    (Register Crd      , FloatRegister Crs1, FloatRegister Crs2) { pre_inst(); emit_int32(MATCH_FLT_D|rd(Crd)|rs1(Crs1)|rs2(Crs2)); }
inline void Assembler::feq_d    (Register Crd      , FloatRegister Crs1, FloatRegister Crs2) { pre_inst(); emit_int32(MATCH_FEQ_D|rd(Crd)|rs1(Crs1)|rs2(Crs2)); }
inline void Assembler::fcvt_w_s (Register Crd      , FloatRegister Crs1, int Crm           ) { pre_inst(); emit_int32(MATCH_FCVT_W_S|rd(Crd)|rs1(Crs1)|rm(Crm)); }
inline void Assembler::fcvt_wu_s(Register Crd      , FloatRegister Crs1, int Crm           ) { pre_inst(); emit_int32(MATCH_FCVT_WU_S|rd(Crd)|rs1(Crs1)|rm(Crm)); }
inline void Assembler::fcvt_l_s (Register Crd      , FloatRegister Crs1, int Crm           ) { pre_inst(); emit_int32(MATCH_FCVT_L_S|rd(Crd)|rs1(Crs1)|rm(Crm)); }
inline void Assembler::fcvt_lu_s(Register Crd      , FloatRegister Crs1, int Crm           ) { pre_inst(); emit_int32(MATCH_FCVT_LU_S|rd(Crd)|rs1(Crs1)|rm(Crm)); }
inline void Assembler::fmv_x_s  (Register Crd      , FloatRegister Crs1) { pre_inst(); emit_int32(MATCH_FMV_X_S|rd(Crd)|rs1(Crs1)); }
inline void Assembler::fclass_s (Register Crd      , FloatRegister Crs1) { pre_inst(); emit_int32(MATCH_FCLASS_S|rd(Crd)|rs1(Crs1)); }
inline void Assembler::fcvt_w_d (Register Crd      , FloatRegister Crs1, int Crm           ) { pre_inst(); emit_int32(MATCH_FCVT_W_D|rd(Crd)|rs1(Crs1)|rm(Crm)); }
inline void Assembler::fcvt_wu_d(Register Crd      , FloatRegister Crs1, int Crm           ) { pre_inst(); emit_int32(MATCH_FCVT_WU_D|rd(Crd)|rs1(Crs1)|rm(Crm)); }
inline void Assembler::fcvt_l_d (Register Crd      , FloatRegister Crs1, int Crm           ) { pre_inst(); emit_int32(MATCH_FCVT_L_D|rd(Crd)|rs1(Crs1)|rm(Crm)); }
inline void Assembler::fcvt_lu_d(Register Crd      , FloatRegister Crs1, int Crm           ) { pre_inst(); emit_int32(MATCH_FCVT_LU_D|rd(Crd)|rs1(Crs1)|rm(Crm)); }
inline void Assembler::fmv_x_d  (Register Crd      , FloatRegister Crs1) { pre_inst(); emit_int32(MATCH_FMV_X_D|rd(Crd)|rs1(Crs1)); }
inline void Assembler::fclass_d (Register Crd      , FloatRegister Crs1) { pre_inst(); emit_int32(MATCH_FCLASS_D|rd(Crd)|rs1(Crs1)); }
inline void Assembler::fcvt_s_w (FloatRegister Crd , Register Crs1     , int Crm           ) { pre_inst(); emit_int32(MATCH_FCVT_S_W|rd(Crd)|rs1(Crs1)|rm(Crm)); }
inline void Assembler::fcvt_s_wu(FloatRegister Crd , Register Crs1     , int Crm           ) { pre_inst(); emit_int32(MATCH_FCVT_S_WU|rd(Crd)|rs1(Crs1)|rm(Crm)); }
inline void Assembler::fcvt_s_l (FloatRegister Crd , Register Crs1     , int Crm           ) { pre_inst(); emit_int32(MATCH_FCVT_S_L|rd(Crd)|rs1(Crs1)|rm(Crm)); }
inline void Assembler::fcvt_s_lu(FloatRegister Crd , Register Crs1     , int Crm           ) { pre_inst(); emit_int32(MATCH_FCVT_S_LU|rd(Crd)|rs1(Crs1)|rm(Crm)); }
inline void Assembler::fmv_s_x  (FloatRegister Crd , Register Crs1     ) { pre_inst(); emit_int32(MATCH_FMV_S_X|rd(Crd)|rs1(Crs1)); }
inline void Assembler::fcvt_d_w (FloatRegister Crd , Register Crs1     , int Crm           ) { pre_inst(); emit_int32(MATCH_FCVT_D_W|rd(Crd)|rs1(Crs1)|rm(Crm)); }
inline void Assembler::fcvt_d_wu(FloatRegister Crd , Register Crs1     , int Crm           ) { pre_inst(); emit_int32(MATCH_FCVT_D_WU|rd(Crd)|rs1(Crs1)|rm(Crm)); }
inline void Assembler::fcvt_d_l (FloatRegister Crd , Register Crs1     , int Crm           ) { pre_inst(); emit_int32(MATCH_FCVT_D_L|rd(Crd)|rs1(Crs1)|rm(Crm)); }
inline void Assembler::fcvt_d_lu(FloatRegister Crd , Register Crs1     , int Crm           ) { pre_inst(); emit_int32(MATCH_FCVT_D_LU|rd(Crd)|rs1(Crs1)|rm(Crm)); }
inline void Assembler::fmv_d_x  (FloatRegister Crd , Register Crs1     ) { pre_inst(); emit_int32(MATCH_FMV_D_X|rd(Crd)|rs1(Crs1)); }
inline void Assembler::flw      (FloatRegister Crd , Register Crs1     , int Cimm12        ) { pre_inst(); emit_int32(MATCH_FLW|rd(Crd)|rs1(Crs1)|imm12(Cimm12)); }
inline void Assembler::fld      (FloatRegister Crd , Register Crs1     , int Cimm12        ) { pre_inst(); emit_int32(MATCH_FLD|rd(Crd)|rs1(Crs1)|imm12(Cimm12)); }
inline void Assembler::fsw      (int Cimm12hi      , Register Crs1     , FloatRegister Crs2, int Cimm12lo      ) { pre_inst(); emit_int32(MATCH_FSW|imm12hi(Cimm12hi)|rs1(Crs1)|rs2(Crs2)|imm12lo(Cimm12lo)); }
inline void Assembler::fsd      (int Cimm12hi      , Register Crs1     , FloatRegister Crs2, int Cimm12lo      ) { pre_inst(); emit_int32(MATCH_FSD|imm12hi(Cimm12hi)|rs1(Crs1)|rs2(Crs2)|imm12lo(Cimm12lo)); }
inline void Assembler::fmadd_s  (FloatRegister Crd , FloatRegister Crs1, FloatRegister Crs2, FloatRegister Crs3, int Crm           ) { pre_inst(); emit_int32(MATCH_FMADD_S|rd(Crd)|rs1(Crs1)|rs2(Crs2)|rs3(Crs3)|rm(Crm)); }
inline void Assembler::fmsub_s  (FloatRegister Crd , FloatRegister Crs1, FloatRegister Crs2, FloatRegister Crs3, int Crm           ) { pre_inst(); emit_int32(MATCH_FMSUB_S|rd(Crd)|rs1(Crs1)|rs2(Crs2)|rs3(Crs3)|rm(Crm)); }
inline void Assembler::fnmsub_s (FloatRegister Crd , FloatRegister Crs1, FloatRegister Crs2, FloatRegister Crs3, int Crm           ) { pre_inst(); emit_int32(MATCH_FNMSUB_S|rd(Crd)|rs1(Crs1)|rs2(Crs2)|rs3(Crs3)|rm(Crm)); }
inline void Assembler::fnmadd_s (FloatRegister Crd , FloatRegister Crs1, FloatRegister Crs2, FloatRegister Crs3, int Crm           ) { pre_inst(); emit_int32(MATCH_FNMADD_S|rd(Crd)|rs1(Crs1)|rs2(Crs2)|rs3(Crs3)|rm(Crm)); }
inline void Assembler::fmadd_d  (FloatRegister Crd , FloatRegister Crs1, FloatRegister Crs2, FloatRegister Crs3, int Crm           ) { pre_inst(); emit_int32(MATCH_FMADD_D|rd(Crd)|rs1(Crs1)|rs2(Crs2)|rs3(Crs3)|rm(Crm)); }
inline void Assembler::fmsub_d  (FloatRegister Crd , FloatRegister Crs1, FloatRegister Crs2, FloatRegister Crs3, int Crm           ) { pre_inst(); emit_int32(MATCH_FMSUB_D|rd(Crd)|rs1(Crs1)|rs2(Crs2)|rs3(Crs3)|rm(Crm)); }
inline void Assembler::fnmsub_d (FloatRegister Crd , FloatRegister Crs1, FloatRegister Crs2, FloatRegister Crs3, int Crm           ) { pre_inst(); emit_int32(MATCH_FNMSUB_D|rd(Crd)|rs1(Crs1)|rs2(Crs2)|rs3(Crs3)|rm(Crm)); }
inline void Assembler::fnmadd_d (FloatRegister Crd , FloatRegister Crs1, FloatRegister Crs2, FloatRegister Crs3, int Crm           ) { pre_inst(); emit_int32(MATCH_FNMADD_D|rd(Crd)|rs1(Crs1)|rs2(Crs2)|rs3(Crs3)|rm(Crm)); }

#define check_imm(x) assert((-(1 << 11) <= (x) && (x) < (1 << 11)), "offset out of range")

// Aliases for instruction emitters (purely for readability)
inline void Assembler::beq  (Register a, Register b, Label& l) { pre_inst(); int bimm12=bimm12_disp((intptr_t)target(l),(intptr_t)pc()); emit_int32(MATCH_BEQ|bimm12|rs1(a)|rs2(b)); }
inline void Assembler::bne  (Register a, Register b, Label& l) { pre_inst(); int bimm12=bimm12_disp((intptr_t)target(l),(intptr_t)pc()); emit_int32(MATCH_BNE|bimm12|rs1(a)|rs2(b)); }
inline void Assembler::blt  (Register a, Register b, Label& l) { pre_inst(); int bimm12=bimm12_disp((intptr_t)target(l),(intptr_t)pc()); emit_int32(MATCH_BLT|bimm12|rs1(a)|rs2(b)); }
inline void Assembler::bge  (Register a, Register b, Label& l) { pre_inst(); int bimm12=bimm12_disp((intptr_t)target(l),(intptr_t)pc()); emit_int32(MATCH_BGE|bimm12|rs1(a)|rs2(b)); }
inline void Assembler::bltu (Register a, Register b, Label& l) { pre_inst(); int bimm12=bimm12_disp((intptr_t)target(l),(intptr_t)pc()); emit_int32(MATCH_BLTU|bimm12|rs1(a)|rs2(b)); }
inline void Assembler::bgeu (Register a, Register b, Label& l) { pre_inst(); int bimm12=bimm12_disp((intptr_t)target(l),(intptr_t)pc()); emit_int32(MATCH_BGEU|bimm12|rs1(a)|rs2(b)); }
inline void Assembler::sb   (Register a     , int a_imm12,  Register b) { check_imm(a_imm12); sb( (a_imm12 >> 5) & 0x7F, b, a, a_imm12 & 0x1F); }
inline void Assembler::sh   (Register a     , int a_imm12,  Register b) { check_imm(a_imm12); sh( (a_imm12 >> 5) & 0x7F, b, a, a_imm12 & 0x1F); }
inline void Assembler::sw   (Register a     , int a_imm12,  Register b) { check_imm(a_imm12); sw( (a_imm12 >> 5) & 0x7F, b, a, a_imm12 & 0x1F); }
inline void Assembler::sd   (Register a     , int a_imm12,  Register b) { check_imm(a_imm12); sd( (a_imm12 >> 5) & 0x7F, b, a, a_imm12 & 0x1F); }
inline void Assembler::fsw  (FloatRegister a, int a_imm12,  Register b) { check_imm(a_imm12); fsw((a_imm12 >> 5) & 0x7F, b, a, a_imm12 & 0x1F); }
inline void Assembler::fsd  (FloatRegister a, int a_imm12,  Register b) { check_imm(a_imm12); fsd((a_imm12 >> 5) & 0x7F, b, a, a_imm12 & 0x1F); }
inline void Assembler::lb   (Register d     , int a_imm12,  Register a) { lb (d, a, a_imm12); }
inline void Assembler::lh   (Register d     , int a_imm12,  Register a) { lh (d, a, a_imm12); }
inline void Assembler::lw   (Register d     , int a_imm12,  Register a) { lw (d, a, a_imm12); }
inline void Assembler::ld   (Register d     , int a_imm12,  Register a) { ld (d, a, a_imm12); }
inline void Assembler::lbu  (Register d     , int a_imm12,  Register a) { lbu(d, a, a_imm12); }
inline void Assembler::lhu  (Register d     , int a_imm12,  Register a) { lhu(d, a, a_imm12); }
inline void Assembler::lwu  (Register d     , int a_imm12,  Register a) { lwu(d, a, a_imm12); }
inline void Assembler::flw  (FloatRegister d, int a_imm12,  Register a) { flw(d, a, a_imm12); }
inline void Assembler::fld  (FloatRegister d, int a_imm12,  Register a) { fld(d, a, a_imm12); }

// Some more useful aliases
inline void Assembler::mv   (Register d     , Register s     ) { ori(d, s, 0); }
inline void Assembler::fmv_s(FloatRegister d, FloatRegister s) { fsgnj_s(d, s, s); }
inline void Assembler::fmv_d(FloatRegister d, FloatRegister s) { fsgnj_d(d, s, s); }
inline void Assembler::j    (Label& l       ) { pre_inst(); int a_jimm20=(intptr_t) target(l) - (intptr_t) pc(); emit_int32(MATCH_JAL|jimm20_disp(a_jimm20)); }
inline void Assembler::jr   (Register d     ) { pre_inst(); emit_int32(MATCH_JALR|rs1(d)); }
inline void Assembler::nop  () { pre_inst(); emit_int32(MATCH_ADDI); };

#endif // CPU_RISCV_VM_ASSEMBLER_RISCV_INLINE_HPP
