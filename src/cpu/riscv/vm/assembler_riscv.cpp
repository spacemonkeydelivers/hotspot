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
#include "asm/assembler.inline.hpp"
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

int AbstractAssembler::code_fill_byte() {
  return 0x00;                  // illegal instruction 0x00000000
}

void Assembler::print_instruction(int inst) {
  fprintf(stderr, "DASM(%08X)", inst);
}

// Patch instruction `inst' at offset `inst_pos' to refer to
// `dest_pos' and return the resulting instruction.  We should have
// pcs, not offsets, but since all is relative, it will work out fine.
int Assembler::patched_branch(intptr_t dest_pos, int inst, intptr_t inst_pos) {
  int pos = dest_pos - inst_pos;
  int m = 0; // mask for displacement field
  int v = 0; // new value for displacement field

  if (is_jal(inst)) {
    m = jimm20_disp(-1);
    v = jimm20_disp(pos);
  } else if(is_jalr(inst)) {
    // make sure pos fits in 12-bit immediate
    assert((-(1 << 11) <= pos && pos < (1 << 11)), "jalr offset out of range");
    m = imm12(-1);
    v = imm12(pos);
  } else {
    m = bimm12hi(0x7F) | bimm12lo(0x1F);
    v = bimm12_disp(dest_pos, inst_pos);
  }

  return (inst & (~m)) | v;
}

// Return the offset, relative to _code_begin, of the destination of
// the branch inst at offset pos.
int Assembler::branch_destination(int inst, int pos) {
  Unimplemented();
  /*
  int r = 0;
  switch (inv_op_ppc(inst)) {
    case b_op:  r = bxx_destination_offset(inst, pos); break;
    case bc_op: r = inv_bd_field(inst, pos); break;
    default: ShouldNotReachHere();
  }
  return r;
  */
}

#define IS_SEXT_32_IMM(x) ((x) & ~(0x7fffffff)) == 0 || ((x) & ~(0x7fffffff)) == ~(0x7fffffff)
#define SEXT_IMM_64(x, s) ((x) & (1 << (s)-1) ? (x) | (((intptr_t) -1) << (s)) : (x) & ~(((intptr_t) -1) << (s)))

// Load all constants up to 64 bits, optimizing for the size of constant.
void Assembler::li(Register d, intptr_t x) {
  load_const_patchable(d, x);
  /* TODO: Implement efficient version.
  int shift = 12;
  intptr_t lower = (int32_t) x << (32-shift) >> (32-shift);
  intptr_t upper = x - lower;

  if (IS_SEXT_32_IMM(x)) {
    Register Radd = XZERO;
    if (upper) {
      lui(d, (x >> 12) & 0xfffff);
      Radd = d;
    }
    if (lower || Radd == XZERO)
      addiw(d, Radd, SEXT_IMM_64(x, 12));
  } else {
    while(((upper >> shift) & 1) == 0)
      shift++;
    li(d, upper >> shift);
    slli(d, d, shift);
    if (lower)
      addi(d, d, SEXT_IMM_64(lower, 12));
  }
  */
}

// Load constants exactly 64 bits in size.
void Assembler::load_const_patchable(Register d, intptr_t x) {
  intptr_t xa = (x >> 44) & 0xfffff; // 20 bit unsigned segment
  intptr_t xb = (x >> 33) & 0x7ff;   // 11 bit unsigned segment
  intptr_t xc = (x >> 22) & 0x7ff;   // 11 bit unsigned segment
  intptr_t xd = (x >> 11) & 0x7ff;   // 11 bit unsigned segment
  intptr_t xe = (x >>  0) & 0x7ff;   // 11 bit unsigned segment
  lui( d, sext(xa, 20));
  ori( d, d, xb);
  slli(d, d, 11);
  ori( d, d, xc);
  slli(d, d, 11);
  ori( d, d, xd);
  slli(d, d, 11);
  ori( d, d, xe);
}

// Load pointers to vm-based locations that don't change and 
// don't need to be patched. Return an 11-bit sign-extended offset 
// which can be used in a load or store.
int Assembler::load_vm_ptr(Register d, void* x) {
  intptr_t xa = ((intptr_t)x >> 44) & 0xfffff; // 20 bit unsigned segment
  intptr_t xb = ((intptr_t)x >> 33) & 0x7ff;   // 11 bit unsigned segment
  intptr_t xc = ((intptr_t)x >> 22) & 0x7ff;   // 11 bit unsigned segment
  intptr_t xd = ((intptr_t)x >> 11) & 0x7ff;   // 11 bit unsigned segment
  intptr_t xe = ((intptr_t)x >>  0) & 0x7ff;   // 11 bit unsigned segment
  lui( d, sext(xa, 20));
  ori( d, d, xb);
  slli(d, d, 11);
  ori( d, d, xc);
  slli(d, d, 11);
  ori( d, d, xd);
  slli(d, d, 11);
  return (int) SEXT_IMM_64(xe, 11);
}

#ifndef PRODUCT
// Test of riscv assembler.
void Assembler::test_asm() {

  beq(24, X2, X2, 25);
  bne(-17, X18, X17, 0);
  blt(-22, X22, X23, -28);
  bge(27, X30, X5, -17);
  bltu(-16, X30, X25, -14);
  bgeu(28, X27, X6, 21);
  jalr(X17, X21, 17);
  jal(X16, 23);

  lui(X0, -5);
  auipc(X8, 2);
  addi(X28, X12, -25);
  slli(X23, X0, -20);
  slti(X18, X22, 12);
  sltiu(X25, X13, -19);
  xori(X7, X17, 24);
  srli(X19, X29, 4);
  srai(X1, X25, 5);
  ori(X22, X9, -16);
  andi(X0, X7, -7);

  add(X4, X10, X27);
  sub(X16, X28, X11);
  sll(X30, X14, X0);
  slt(X21, X7, X2);
  sltu(X8, X27, X18);
  xor_(X6, X9, X9);
  srl(X24, X18, X3);
  sra(X21, X10, X23);
  or_(X18, X9, X14);
  and_(X1, X15, X2);

  addiw(X24, X22, -30);
  slliw(X28, X5, -27);
  srliw(X0, X28, 20);
  sraiw(X19, X1, -3);
  addw(X11, X2, X25);
  subw(X24, X1, X20);
  sllw(X19, X15, X0);
  srlw(X12, X6, X29);
  sraw(X5, X1, X9);

  lb(X27, X20, -4);
  lh(X26, X4, -6);
  lw(X28, X18, -27);
  ld(X18, X10, 6);
  lbu(X30, X10, 1);
  lhu(X19, X23, -18);
  lwu(X17, X24, -14);

  sb(-2, X30, X28, 28);
  sh(-18, X4, X29, 25);
  sw(13, X20, X7, 0);
  sd(-31, X23, X20, 30);

  fence(9, -5);
  fence_i();

  mul(X28, X27, X28);
  mulh(X2, X5, X11);
  mulhsu(X16, X18, X31);
  mulhu(X5, X15, X17);
  div(X31, X22, X22);
  divu(X25, X17, X15);
  rem(X5, X30, X7);
  remu(X28, X4, X7);

  mulw(X27, X31, X13);
  divw(X7, X25, X1);
  divuw(X12, X28, X20);
  remw(X12, X18, X24);
  remuw(X0, X31, X27);

  amoadd_w(X3, X7, X28, 14);
  amoxor_w(X27, X27, X29, 11);
  amoor_w(X27, X0, X0, -4);
  amoand_w(X21, X2, X14, 11);
  amomin_w(X31, X26, X11, 18);
  amomax_w(X13, X25, X27, -22);
  amominu_w(X7, X12, X20, 18);
  amomaxu_w(X21, X14, X14, -19);
  amoswap_w(X11, X10, X8, -11);

  lr_w(X3, X2, 30);
  sc_w(X12, X11, X1, -19);

  amoadd_d(X26, X1, X24, 6);
  amoxor_d(X16, X17, X0, 24);
  amoor_d(X2, X7, X30, 23);
  amoand_d(X9, X12, X4, -22);
  amomin_d(X1, X18, X31, 21);
  amomax_d(X1, X12, X20, -15);
  amominu_d(X25, X15, X27, 11);
  amomaxu_d(X5, X3, X6, 18);
  amoswap_d(X21, X25, X13, 0);

  lr_d(X30, X28, -14);
  sc_d(X3, X21, X6, -4);

  scall();
  sbreak();
  sret();
  sfence_vm(X14);
  wfi();
  mrth();
  mrts();
  hrts();

  csrrw(X23, X1, -28);
  csrrs(X0, X22, 12);
  csrrc(X11, X11, -2);
  csrrwi(X5, X21, -18);
  csrrsi(X12, X21, 2);
  csrrci(X27, X20, 24);

  fadd_s(F3, F7, F4, 2);
  fsub_s(F28, F5, F25, 23);
  fmul_s(F13, F11, F6, 3);
  fdiv_s(F17, F17, F5, -20);
  fsgnj_s(F16, F14, F12);
  fsgnjn_s(F7, F1, F4);
  fsgnjx_s(F19, F1, F22);
  fmin_s(F11, F6, F22);
  fmax_s(F16, F25, F22);
  fsqrt_s(F23, F9, 18);

  fadd_d(F0, F14, F16, 5);
  fsub_d(F9, F25, F7, 0);
  fmul_d(F5, F11, F30, -16);
  fdiv_d(F9, F27, F13, 17);
  fsgnj_d(F31, F27, F0);
  fsgnjn_d(F24, F3, F26);
  fsgnjx_d(F28, F28, F11);
  fmin_d(F30, F20, F28);
  fmax_d(F16, F29, F9);

  fcvt_s_d(F22, F27, -20);
  fcvt_d_s(F11, F8, 20);

  fsqrt_d(F27, F17, 3);

  fle_s(X30, F14, F27);
  flt_s(X15, F21, F3);
  feq_s(X27, F4, F15);
  fle_d(X2, F27, F13);
  flt_d(X9, F28, F26);
  feq_d(X12, F27, F4);

  fcvt_w_s(X2, F13, 25);
  fcvt_wu_s(X13, F6, -5);
  fcvt_l_s(X2, F29, 22);
  fcvt_lu_s(X8, F8, -7);

  fmv_x_s(X24, F0);
  fclass_s(X5, F0);

  fcvt_w_d(X9, F16, -23);
  fcvt_wu_d(X17, F28, 20);
  fcvt_l_d(X31, F18, -18);
  fcvt_lu_d(X16, F3, -17);

  fmv_x_d(X14, F27);
  fclass_d(X2, F6);

  fcvt_s_w(F7, X23, 29);
  fcvt_s_wu(F17, X28, -14);
  fcvt_s_l(F1, X31, -20);
  fcvt_s_lu(F18, X31, -27);

  fmv_s_x(F0, X25);

  fcvt_d_w(F7, X26, 11);
  fcvt_d_wu(F17, X23, 13);
  fcvt_d_l(F12, X16, 9);
  fcvt_d_lu(F12, X5, 14);

  fmv_d_x(F12, X10);

  flw(F12, X8, 16);
  fld(F29, X8, -10);
  fsw(-1, X8, F24, 19);
  fsd(10, X7, F8, -21);

  fmadd_s(F8, F30, F0, F23, 20);
  fmsub_s(F21, F23, F26, F5, 23);
  fnmsub_s(F0, F3, F10, F31, -32);
  fnmadd_s(F23, F5, F3, F10, 4);
  fmadd_d(F31, F5, F25, F14, -22);
  fmsub_d(F18, F27, F30, F25, 12);
  fnmsub_d(F22, F7, F17, F30, -23);
  fnmadd_d(F0, F15, F1, F27, -5);

  tty->print_cr("\ntest_asm disassembly (0x%lx 0x%lx):", p2i(code()->insts_begin()), p2i(code()->insts_end()));
  code()->decode();
}

#endif // !PRODUCT
