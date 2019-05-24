/*
 * Copyright (c) 2002, 2013, Oracle and/or its affiliates. All rights reserved.
 * Copyright 2012, 2013 SAP AG. All rights reserved.
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

#ifndef CPU_RISCV_VM_ASSEMBLER_RISCV_HPP
#define CPU_RISCV_VM_ASSEMBLER_RISCV_HPP

#include "asm/register.hpp"

// Address is an abstraction used to represent a memory location
// as used in assembler instructions.
// RISCV instructions grok either baseReg + indexReg or baseReg + disp.
// So far we do not use this as simplification by this class is low
// on RISCV with its simple addressing mode. Use RegisterOrConstant to
// represent an offset.
class Address VALUE_OBJ_CLASS_SPEC {
 public:
  enum ScaleFactor {
    no_scale = -1,
    times_1  =  0,
    times_2  =  1,
    times_4  =  2,
    times_8  =  3,
    times_ptr = 3 // TODO: FIX for RV32
  };
};

class AddressLiteral VALUE_OBJ_CLASS_SPEC {
 private:
  address          _address;
  RelocationHolder _rspec;

  RelocationHolder rspec_from_rtype(relocInfo::relocType rtype, address addr) {
    switch (rtype) {
    case relocInfo::external_word_type:
      return external_word_Relocation::spec(addr);
    case relocInfo::internal_word_type:
      return internal_word_Relocation::spec(addr);
    case relocInfo::opt_virtual_call_type:
      return opt_virtual_call_Relocation::spec();
    case relocInfo::static_call_type:
      return static_call_Relocation::spec();
    case relocInfo::runtime_call_type:
      return runtime_call_Relocation::spec();
    case relocInfo::none:
      return RelocationHolder();
    default:
      ShouldNotReachHere();
      return RelocationHolder();
    }
  }

 protected:
  // creation
  AddressLiteral() : _address(NULL), _rspec(NULL) {}

 public:
  AddressLiteral(address addr, RelocationHolder const& rspec)
    : _address(addr),
      _rspec(rspec) {}

  AddressLiteral(address addr, relocInfo::relocType rtype = relocInfo::none)
    : _address((address) addr),
      _rspec(rspec_from_rtype(rtype, (address) addr)) {}

  AddressLiteral(oop* addr, relocInfo::relocType rtype = relocInfo::none)
    : _address((address) addr),
      _rspec(rspec_from_rtype(rtype, (address) addr)) {}

  intptr_t value() const { return (intptr_t) _address; }

  const RelocationHolder& rspec() const { return _rspec; }
};

// Argument is an abstraction used to represent an outgoing
// actual argument or an incoming formal parameter, whether
// it resides in memory or in a register, in a manner consistent
// with the RISCV Application Binary Interface, or ABI. This is
// often referred to as the native or C calling convention.

class Argument VALUE_OBJ_CLASS_SPEC {
 private:
  int _number;  // The number of the argument.
 public:
  enum {
    // Only 8 registers may contain integer parameters.
    n_register_parameters = 8,
    // Can have up to 8 floating registers.
    n_float_register_parameters = 8,

    // RISCV C calling conventions.
    // The first eight arguments are passed in int regs if they are int.
    n_int_register_parameters_c = 8,
    // The first eight float arguments are passed in float regs.
    n_float_register_parameters_c = 8,
    // Only the first 8 parameters are not placed on the stack.
    n_regs_not_on_stack_c = 8,
  };
  // creation
  Argument(int number) : _number(number) {}

  int number() const { return _number; }

  // Locating register-based arguments:
  bool is_register() const { return _number < n_register_parameters; }

  Register as_register() const {
    assert(is_register(), "must be a register argument");
    return as_Register(number() + X11_ARG1->encoding());
  }
};

class Assembler : public AbstractAssembler {
 protected:
  // Displacement routines
  static int  patched_branch(intptr_t dest_pos, int inst, intptr_t inst_pos);
  static int  branch_destination(int inst, int pos);

  friend class AbstractAssembler;

  // Code patchers need various routines like inv_wdisp()
  friend class NativeInstruction;
  friend class NativeGeneralJump;
  friend class Relocation;

 public:
  static void print_instruction(int inst);

  /* Auto-generated by riscv-opcodes -- https://github.com/riscv/riscv-opcodes */
  enum opcode_mask {
    MASK_ADD = 0xfe00707f,
    MASK_ADDI = 0x707f,
    MASK_ADDIW = 0x707f,
    MASK_ADDW = 0xfe00707f,
    MASK_AMOADD_D = 0xf800707f,
    MASK_AMOADD_W = 0xf800707f,
    MASK_AMOAND_D = 0xf800707f,
    MASK_AMOAND_W = 0xf800707f,
    MASK_AMOMAXU_D = 0xf800707f,
    MASK_AMOMAXU_W = 0xf800707f,
    MASK_AMOMAX_D = 0xf800707f,
    MASK_AMOMAX_W = 0xf800707f,
    MASK_AMOMINU_D = 0xf800707f,
    MASK_AMOMINU_W = 0xf800707f,
    MASK_AMOMIN_D = 0xf800707f,
    MASK_AMOMIN_W = 0xf800707f,
    MASK_AMOOR_D = 0xf800707f,
    MASK_AMOOR_W = 0xf800707f,
    MASK_AMOSWAP_D = 0xf800707f,
    MASK_AMOSWAP_W = 0xf800707f,
    MASK_AMOXOR_D = 0xf800707f,
    MASK_AMOXOR_W = 0xf800707f,
    MASK_AND_ = 0xfe00707f,
    MASK_ANDI = 0x707f,
    MASK_AUIPC = 0x7f,
    MASK_BEQ = 0x707f,
    MASK_BGE = 0x707f,
    MASK_BGEU = 0x707f,
    MASK_BLT = 0x707f,
    MASK_BLTU = 0x707f,
    MASK_BNE = 0x707f,
    MASK_CSRRC = 0x707f,
    MASK_CSRRCI = 0x707f,
    MASK_CSRRS = 0x707f,
    MASK_CSRRSI = 0x707f,
    MASK_CSRRW = 0x707f,
    MASK_CSRRWI = 0x707f,
    MASK_DIV = 0xfe00707f,
    MASK_DIVU = 0xfe00707f,
    MASK_DIVUW = 0xfe00707f,
    MASK_DIVW = 0xfe00707f,
    MASK_FADD_D = 0xfe00007f,
    MASK_FADD_S = 0xfe00007f,
    MASK_FCLASS_D = 0xfff0707f,
    MASK_FCLASS_S = 0xfff0707f,
    MASK_FCVT_D_L = 0xfff0007f,
    MASK_FCVT_D_LU = 0xfff0007f,
    MASK_FCVT_D_S = 0xfff0007f,
    MASK_FCVT_D_W = 0xfff0007f,
    MASK_FCVT_D_WU = 0xfff0007f,
    MASK_FCVT_LU_D = 0xfff0007f,
    MASK_FCVT_LU_S = 0xfff0007f,
    MASK_FCVT_L_D = 0xfff0007f,
    MASK_FCVT_L_S = 0xfff0007f,
    MASK_FCVT_S_D = 0xfff0007f,
    MASK_FCVT_S_L = 0xfff0007f,
    MASK_FCVT_S_LU = 0xfff0007f,
    MASK_FCVT_S_W = 0xfff0007f,
    MASK_FCVT_S_WU = 0xfff0007f,
    MASK_FCVT_WU_D = 0xfff0007f,
    MASK_FCVT_WU_S = 0xfff0007f,
    MASK_FCVT_W_D = 0xfff0007f,
    MASK_FCVT_W_S = 0xfff0007f,
    MASK_FDIV_D = 0xfe00007f,
    MASK_FDIV_S = 0xfe00007f,
    MASK_FENCE = 0x707f,
    MASK_FENCE_I = 0x707f,
    MASK_FEQ_D = 0xfe00707f,
    MASK_FEQ_S = 0xfe00707f,
    MASK_FLD = 0x707f,
    MASK_FLE_D = 0xfe00707f,
    MASK_FLE_S = 0xfe00707f,
    MASK_FLT_D = 0xfe00707f,
    MASK_FLT_S = 0xfe00707f,
    MASK_FLW = 0x707f,
    MASK_FMADD_D = 0x600007f,
    MASK_FMADD_S = 0x600007f,
    MASK_FMAX_D = 0xfe00707f,
    MASK_FMAX_S = 0xfe00707f,
    MASK_FMIN_D = 0xfe00707f,
    MASK_FMIN_S = 0xfe00707f,
    MASK_FMSUB_D = 0x600007f,
    MASK_FMSUB_S = 0x600007f,
    MASK_FMUL_D = 0xfe00007f,
    MASK_FMUL_S = 0xfe00007f,
    MASK_FMV_D_X = 0xfff0707f,
    MASK_FMV_S_X = 0xfff0707f,
    MASK_FMV_X_D = 0xfff0707f,
    MASK_FMV_X_S = 0xfff0707f,
    MASK_FNMADD_D = 0x600007f,
    MASK_FNMADD_S = 0x600007f,
    MASK_FNMSUB_D = 0x600007f,
    MASK_FNMSUB_S = 0x600007f,
    MASK_FSD = 0x707f,
    MASK_FSGNJN_D = 0xfe00707f,
    MASK_FSGNJN_S = 0xfe00707f,
    MASK_FSGNJX_D = 0xfe00707f,
    MASK_FSGNJX_S = 0xfe00707f,
    MASK_FSGNJ_D = 0xfe00707f,
    MASK_FSGNJ_S = 0xfe00707f,
    MASK_FSQRT_D = 0xfff0007f,
    MASK_FSQRT_S = 0xfff0007f,
    MASK_FSUB_D = 0xfe00007f,
    MASK_FSUB_S = 0xfe00007f,
    MASK_FSW = 0x707f,
    MASK_HRTS = 0xffffffff,
    MASK_JAL = 0x7f,
    MASK_JALR = 0x707f,
    MASK_LB = 0x707f,
    MASK_LBU = 0x707f,
    MASK_LD = 0x707f,
    MASK_LH = 0x707f,
    MASK_LHU = 0x707f,
    MASK_LR_D = 0xf9f0707f,
    MASK_LR_W = 0xf9f0707f,
    MASK_LUI = 0x7f,
    MASK_LW = 0x707f,
    MASK_LWU = 0x707f,
    MASK_MRTH = 0xffffffff,
    MASK_MRTS = 0xffffffff,
    MASK_MUL = 0xfe00707f,
    MASK_MULH = 0xfe00707f,
    MASK_MULHSU = 0xfe00707f,
    MASK_MULHU = 0xfe00707f,
    MASK_MULW = 0xfe00707f,
    MASK_OR_ = 0xfe00707f,
    MASK_ORI = 0x707f,
    MASK_REM = 0xfe00707f,
    MASK_REMU = 0xfe00707f,
    MASK_REMUW = 0xfe00707f,
    MASK_REMW = 0xfe00707f,
    MASK_SB = 0x707f,
    MASK_SBREAK = 0xffffffff,
    MASK_SCALL = 0xffffffff,
    MASK_SC_D = 0xf800707f,
    MASK_SC_W = 0xf800707f,
    MASK_SD = 0x707f,
    MASK_SFENCE_VM = 0xfff07fff,
    MASK_SH = 0x707f,
    MASK_SLL = 0xfe00707f,
    MASK_SLLI = 0xfc00707f,
    MASK_SLLIW = 0xfe00707f,
    MASK_SLLW = 0xfe00707f,
    MASK_SLT = 0xfe00707f,
    MASK_SLTI = 0x707f,
    MASK_SLTIU = 0x707f,
    MASK_SLTU = 0xfe00707f,
    MASK_SRA = 0xfe00707f,
    MASK_SRAI = 0xfc00707f,
    MASK_SRAIW = 0xfe00707f,
    MASK_SRAW = 0xfe00707f,
    MASK_SRET = 0xffffffff,
    MASK_SRL = 0xfe00707f,
    MASK_SRLI = 0xfc00707f,
    MASK_SRLIW = 0xfe00707f,
    MASK_SRLW = 0xfe00707f,
    MASK_SUB = 0xfe00707f,
    MASK_SUBW = 0xfe00707f,
    MASK_SW = 0x707f,
    MASK_WFI = 0xffffffff,
    MASK_XOR_ = 0xfe00707f,
    MASK_XORI = 0x707f
  };

  /* Auto-generated by riscv-opcodes -- https://github.com/riscv/riscv-opcodes */
  enum opcode_match {
    MATCH_ADD = 0x33,
    MATCH_ADDI = 0x13,
    MATCH_ADDIW = 0x1b,
    MATCH_ADDW = 0x3b,
    MATCH_AMOADD_D = 0x302f,
    MATCH_AMOADD_W = 0x202f,
    MATCH_AMOAND_D = 0x6000302f,
    MATCH_AMOAND_W = 0x6000202f,
    MATCH_AMOMAXU_D = 0xe000302f,
    MATCH_AMOMAXU_W = 0xe000202f,
    MATCH_AMOMAX_D = 0xa000302f,
    MATCH_AMOMAX_W = 0xa000202f,
    MATCH_AMOMINU_D = 0xc000302f,
    MATCH_AMOMINU_W = 0xc000202f,
    MATCH_AMOMIN_D = 0x8000302f,
    MATCH_AMOMIN_W = 0x8000202f,
    MATCH_AMOOR_D = 0x4000302f,
    MATCH_AMOOR_W = 0x4000202f,
    MATCH_AMOSWAP_D = 0x800302f,
    MATCH_AMOSWAP_W = 0x800202f,
    MATCH_AMOXOR_D = 0x2000302f,
    MATCH_AMOXOR_W = 0x2000202f,
    MATCH_AND_ = 0x7033,
    MATCH_ANDI = 0x7013,
    MATCH_AUIPC = 0x17,
    MATCH_BEQ = 0x63,
    MATCH_BGE = 0x5063,
    MATCH_BGEU = 0x7063,
    MATCH_BLT = 0x4063,
    MATCH_BLTU = 0x6063,
    MATCH_BNE = 0x1063,
    MATCH_CSRRC = 0x3073,
    MATCH_CSRRCI = 0x7073,
    MATCH_CSRRS = 0x2073,
    MATCH_CSRRSI = 0x6073,
    MATCH_CSRRW = 0x1073,
    MATCH_CSRRWI = 0x5073,
    MATCH_DIV = 0x2004033,
    MATCH_DIVU = 0x2005033,
    MATCH_DIVUW = 0x200503b,
    MATCH_DIVW = 0x200403b,
    MATCH_FADD_D = 0x2000053,
    MATCH_FADD_S = 0x53,
    MATCH_FCLASS_D = 0xe2001053,
    MATCH_FCLASS_S = 0xe0001053,
    MATCH_FCVT_D_L = 0xd2200053,
    MATCH_FCVT_D_LU = 0xd2300053,
    MATCH_FCVT_D_S = 0x42000053,
    MATCH_FCVT_D_W = 0xd2000053,
    MATCH_FCVT_D_WU = 0xd2100053,
    MATCH_FCVT_LU_D = 0xc2300053,
    MATCH_FCVT_LU_S = 0xc0300053,
    MATCH_FCVT_L_D = 0xc2200053,
    MATCH_FCVT_L_S = 0xc0200053,
    MATCH_FCVT_S_D = 0x40100053,
    MATCH_FCVT_S_L = 0xd0200053,
    MATCH_FCVT_S_LU = 0xd0300053,
    MATCH_FCVT_S_W = 0xd0000053,
    MATCH_FCVT_S_WU = 0xd0100053,
    MATCH_FCVT_WU_D = 0xc2100053,
    MATCH_FCVT_WU_S = 0xc0100053,
    MATCH_FCVT_W_D = 0xc2000053,
    MATCH_FCVT_W_S = 0xc0000053,
    MATCH_FDIV_D = 0x1a000053,
    MATCH_FDIV_S = 0x18000053,
    MATCH_FENCE = 0xf,
    MATCH_FENCE_I = 0x100f,
    MATCH_FEQ_D = 0xa2002053,
    MATCH_FEQ_S = 0xa0002053,
    MATCH_FLD = 0x3007,
    MATCH_FLE_D = 0xa2000053,
    MATCH_FLE_S = 0xa0000053,
    MATCH_FLT_D = 0xa2001053,
    MATCH_FLT_S = 0xa0001053,
    MATCH_FLW = 0x2007,
    MATCH_FMADD_D = 0x2000043,
    MATCH_FMADD_S = 0x43,
    MATCH_FMAX_D = 0x2a001053,
    MATCH_FMAX_S = 0x28001053,
    MATCH_FMIN_D = 0x2a000053,
    MATCH_FMIN_S = 0x28000053,
    MATCH_FMSUB_D = 0x2000047,
    MATCH_FMSUB_S = 0x47,
    MATCH_FMUL_D = 0x12000053,
    MATCH_FMUL_S = 0x10000053,
    MATCH_FMV_D_X = 0xf2000053,
    MATCH_FMV_S_X = 0xf0000053,
    MATCH_FMV_X_D = 0xe2000053,
    MATCH_FMV_X_S = 0xe0000053,
    MATCH_FNMADD_D = 0x200004f,
    MATCH_FNMADD_S = 0x4f,
    MATCH_FNMSUB_D = 0x200004b,
    MATCH_FNMSUB_S = 0x4b,
    MATCH_FSD = 0x3027,
    MATCH_FSGNJN_D = 0x22001053,
    MATCH_FSGNJN_S = 0x20001053,
    MATCH_FSGNJX_D = 0x22002053,
    MATCH_FSGNJX_S = 0x20002053,
    MATCH_FSGNJ_D = 0x22000053,
    MATCH_FSGNJ_S = 0x20000053,
    MATCH_FSQRT_D = 0x5a000053,
    MATCH_FSQRT_S = 0x58000053,
    MATCH_FSUB_D = 0xa000053,
    MATCH_FSUB_S = 0x8000053,
    MATCH_FSW = 0x2027,
    MATCH_HRTS = 0x20500073,
    MATCH_JAL = 0x6f,
    MATCH_JALR = 0x67,
    MATCH_LB = 0x3,
    MATCH_LBU = 0x4003,
    MATCH_LD = 0x3003,
    MATCH_LH = 0x1003,
    MATCH_LHU = 0x5003,
    MATCH_LR_D = 0x1000302f,
    MATCH_LR_W = 0x1000202f,
    MATCH_LUI = 0x37,
    MATCH_LW = 0x2003,
    MATCH_LWU = 0x6003,
    MATCH_MRTH = 0x30600073,
    MATCH_MRTS = 0x30500073,
    MATCH_MUL = 0x2000033,
    MATCH_MULH = 0x2001033,
    MATCH_MULHSU = 0x2002033,
    MATCH_MULHU = 0x2003033,
    MATCH_MULW = 0x200003b,
    MATCH_OR_ = 0x6033,
    MATCH_ORI = 0x6013,
    MATCH_REM = 0x2006033,
    MATCH_REMU = 0x2007033,
    MATCH_REMUW = 0x200703b,
    MATCH_REMW = 0x200603b,
    MATCH_SB = 0x23,
    MATCH_SBREAK = 0x100073,
    MATCH_SCALL = 0x73,
    MATCH_SC_D = 0x1800302f,
    MATCH_SC_W = 0x1800202f,
    MATCH_SD = 0x3023,
    MATCH_SFENCE_VM = 0x10100073,
    MATCH_SH = 0x1023,
    MATCH_SLL = 0x1033,
    MATCH_SLLI = 0x1013,
    MATCH_SLLIW = 0x101b,
    MATCH_SLLW = 0x103b,
    MATCH_SLT = 0x2033,
    MATCH_SLTI = 0x2013,
    MATCH_SLTIU = 0x3013,
    MATCH_SLTU = 0x3033,
    MATCH_SRA = 0x40005033,
    MATCH_SRAI = 0x40005013,
    MATCH_SRAIW = 0x4000501b,
    MATCH_SRAW = 0x4000503b,
    MATCH_SRET = 0x10000073,
    MATCH_SRL = 0x5033,
    MATCH_SRLI = 0x5013,
    MATCH_SRLIW = 0x501b,
    MATCH_SRLW = 0x503b,
    MATCH_SUB = 0x40000033,
    MATCH_SUBW = 0x4000003b,
    MATCH_SW = 0x2023,
    MATCH_WFI = 0x10200073,
    MATCH_XOR_ = 0x4033,
    MATCH_XORI = 0x4013
  };

#ifdef ASSERT
  int debug_trace;
  int debug_emit;
#endif

 public:
  // Helper functions for groups of instructions

  // instruction must start at passed address
  static int instr_len(unsigned char *instr) { return BytesPerInstWord; }

  // instruction must be left-justified in argument
  static int instr_len(unsigned long instr)  { return BytesPerInstWord; }

  // longest instructions
  static int instr_maxlen() { return BytesPerInstWord; }

  // Test if x is within signed immediate range for nbits.
  static bool is_simm(int x, unsigned int nbits) {
    assert(0 < nbits && nbits < 32, "out of bounds");
    const int   min      = -( ((int)1) << nbits-1 );
    const int   maxplus1 =  ( ((int)1) << nbits-1 );
    return min <= x && x < maxplus1;
  }

  static bool is_simm(jlong x, unsigned int nbits) {
    assert(0 < nbits && nbits < 64, "out of bounds");
    const jlong min      = -( ((jlong)1) << nbits-1 );
    const jlong maxplus1 =  ( ((jlong)1) << nbits-1 );
    return min <= x && x < maxplus1;
  }

  // Test if x is within unsigned immediate range for nbits
  static bool is_uimm(int x, unsigned int nbits) {
    assert(0 < nbits && nbits < 32, "out of bounds");
    const int   maxplus1 = ( ((int)1) << nbits );
    return 0 <= x && x < maxplus1;
  }

  static bool is_uimm(jlong x, unsigned int nbits) {
    assert(0 < nbits && nbits < 64, "out of bounds");
    const jlong maxplus1 =  ( ((jlong)1) << nbits );
    return 0 <= x && x < maxplus1;
  }

 protected:
  // helpers

  // X is supposed to fit in a field "nbits" wide
  // and be sign-extended. Check the range.
  static void assert_signed_range(intptr_t x, int nbits) {
    assert(nbits == 32 || (-(1 << nbits-1) <= x && x < (1 << nbits-1)),
           "value out of range");
  }

  static void assert_signed_word_disp_range(intptr_t x, int nbits) {
    assert((x & 3) == 0, "not word aligned");
    assert_signed_range(x, nbits + 2);
  }

  static void assert_unsigned_const(int x, int nbits) {
    assert(juint(x) < juint(1 << nbits), "unsigned constant out of range");
  }

  static int fmask(juint hi_bit, juint lo_bit) {
    assert(hi_bit >= lo_bit && hi_bit < 32, "bad bits");
    return (1 << ( hi_bit-lo_bit + 1 )) - 1;
  }

  // sign extend
  static int sext(int r, int nbits) {
    int mask = (1 << nbits)-1;
    return (r & (1 << (nbits-1))) != 0 ? r = r | (~mask) : r;
  }

  // inverse of u_field
  static int inv_u_field(int x, int hi_bit, int lo_bit) {
    juint r = juint(x) >> lo_bit;
    r &= fmask(hi_bit, lo_bit);
    return int(r);
  }

  // inverse of s_field
  static int inv_s_field(int x, int hi_bit, int lo_bit) {
    int r = inv_u_field(x, hi_bit, lo_bit);
    return sext(r, hi_bit-lo_bit+1);
  }

  static int u_field(int x, int hi_bit, int lo_bit) {
    assert((x & ~fmask(hi_bit, lo_bit)) == 0, "value out of range");
    int r = x << lo_bit;
    assert(inv_u_field(r, hi_bit, lo_bit) == x, "just checking");
    return r;
  }

  // Same as u_field for signed values
  static int s_field(int x, int hi_bit, int lo_bit) {
    int nbits = hi_bit - lo_bit + 1;
    assert(nbits == 32 || (-(1 << nbits-1) <= x && x < (1 << nbits-1)), "value out of range");
    x &= fmask(hi_bit, lo_bit);
    int r = x << lo_bit;
    return r;
  }

 public:
  // instruction fields
  static int rd(        int x          ) { return u_field(x, 11,  7); }
  static int rs1(       int x          ) { return u_field(x, 19, 15); }
  static int rs2(       int x          ) { return u_field(x, 24, 20); }
  static int rs3(       int x          ) { return u_field(x, 31, 27); }
  static int rd(        Register r     ) { return rd( r->encoding()); }
  static int rs1(       Register r     ) { return rs1(r->encoding()); }
  static int rs2(       Register r     ) { return rs2(r->encoding()); }
  static int rs3(       Register r     ) { return rs3(r->encoding()); }
  static int rd(        FloatRegister r) { return rd( r->encoding()); }
  static int rs1(       FloatRegister r) { return rs1(r->encoding()); }
  static int rs2(       FloatRegister r) { return rs2(r->encoding()); }
  static int rs3(       FloatRegister r) { return rs3(r->encoding()); }
  static int aqrl(      int x          ) { return u_field(x, 26, 25); }
  static int pred(      int x          ) { return u_field(x, 27, 24); }
  static int succ(      int x          ) { return u_field(x, 23, 20); }
  static int rm(        int x          ) { return u_field(x, 14, 12); }
  static int imm20(     int x          ) { return s_field(x, 31, 12); }
  static int jimm20(    int x          ) { return u_field(x, 31, 12); }
  static int imm12(     int x          ) { return s_field(x, 31, 20); }
  static int imm12hi(   int x          ) { return u_field(x, 31, 25); }
  static int bimm12hi(  int x          ) { return u_field(x, 31, 25); }
  static int imm12lo(   int x          ) { return u_field(x, 11,  7); }
  static int bimm12lo(  int x          ) { return u_field(x, 11,  7); }
  static int zimm(      int x          ) { return u_field(x, 19, 15); }
  static int shamt(     int x          ) { return u_field(x, 25, 20); }
  static int shamtw(    int x          ) { return u_field(x, 24, 20); }
  static int vseglen(   int x          ) { return u_field(x, 31, 29); }

  // field extraction
  static int get_rd(    int x          ) { return inv_u_field(x, 11,  7); }
  static int get_rs1(   int x          ) { return inv_u_field(x, 19, 15); }
  static int get_imm12( int x          ) { return inv_s_field(x, 31, 20); }
  static int get_imm20( int x          ) { return inv_s_field(x, 31, 12); }
  static int get_shamt( int x          ) { return inv_u_field(x, 25, 20); }

 protected:
  // Compute relative address for branch (2-byte alignment in RISC-V).
  static intptr_t disp(intptr_t x, intptr_t off) {
    int xx = x - off;
    xx = xx >> 1;
    return xx;
  }

  static int bimm12_disp(intptr_t x, intptr_t off) {
    int xx = disp(x, off);
    assert((-(1 << 11) <= xx && xx < (1 << 11)), "branch offset out of range");
    int bit12 = (xx & 0x800) >> 11;
    int bit11 = (xx & 0x400) >> 10;
    int bit1_4 = xx & 0xF;
    int bit5_10 = (xx & 0x3F0) >> 4;
    return bimm12hi((bit12 << 6) | bit5_10) | bimm12lo((bit1_4 << 1) | bit11);
  }

  static int jimm20_disp(int offset) {
    int xx = offset >> 1; // 2-byte alignment
    assert((-(1 << 19) <= xx && xx < (1 << 19)), "branch offset out of range");
    int bit20 = (xx & 0x80000) >> 19;
    int bit1_10 = xx & 0x3FF;
    int bit11 = (xx & 0x400) >> 10;
    int bit12_19 = (xx & 0x7F800) >> 11;
    return jimm20((bit20 << 19) | (bit1_10 << 9) | (bit11 << 8) | bit12_19);
  }

 public:
  // signed immediate, in low bits, nbits long
  static int simm(int x, int nbits) {
    assert_signed_range(x, nbits);
    return x & ((1 << nbits) - 1);
  }

  // unsigned immediate, in low bits, nbits long
  static int uimm(int x, int nbits) {
    assert_unsigned_const(x, nbits);
    return x & ((1 << nbits) - 1);
  }

  static void set_imm12(int* instr, int new_imm) {
    *instr = (*instr & (~imm12(-1))) | imm12(new_imm);
  }

  static inline int hi16_signed(  int x) { return (int)(int16_t)(x >> 16); }
  static inline int lo16_unsigned(int x) { return x & 0xffff; }

 protected:

  // Extract the top 32 bits in a 64 bit word.
  static int32_t hi32(int64_t x) {
    int32_t r = int32_t((uint64_t)x >> 32);
    return r;
  }

 public:

  static inline unsigned int align_addr(unsigned int addr, unsigned int a) {
    return ((addr + (a - 1)) & ~(a - 1));
  }

  static inline bool is_aligned(unsigned int addr, unsigned int a) {
    return (0 == addr % a);
  }

  void flush() {
    AbstractAssembler::flush();
  }

#ifdef ASSERT
  void debug_start_trace() { debug_trace = 1; }
  void debug_stop_trace() { debug_trace = 0; }
  bool debug_is_tracing() { return debug_trace != 0; }

  void debug_start_emit() { debug_emit = 1; }
  void debug_stop_emit() { debug_emit = 0; }
  bool debug_is_emitting() { return debug_emit != 0; }
#endif

  inline void pre_inst();
  inline void bytecode_marker(uint16_t code);
  inline void print_tos();

  inline void emit_int32(int);  // shadows AbstractAssembler::emit_int32
  inline void emit_data(int);
  inline void emit_data(int, RelocationHolder const&);
  inline void emit_data(int, relocInfo::relocType rtype);

  // Emit an address.
  inline address emit_addr(const address addr = NULL);

  /////////////////////////////////////////////////////////////////////////////////////
  // RISCV instructions
  /////////////////////////////////////////////////////////////////////////////////////

  // Issue an illegal instruction.
  inline void illtrap();
  static inline bool is_illtrap(int x);

  /* Instruction Emitters */
  inline void beq      (int Cbimm12hi, Register Crs1, Register Crs2, int Cbimm12lo); 
  inline void bne      (int Cbimm12hi, Register Crs1, Register Crs2, int Cbimm12lo); 
  inline void blt      (int Cbimm12hi, Register Crs1, Register Crs2, int Cbimm12lo); 
  inline void bge      (int Cbimm12hi, Register Crs1, Register Crs2, int Cbimm12lo); 
  inline void bltu     (int Cbimm12hi, Register Crs1, Register Crs2, int Cbimm12lo); 
  inline void bgeu     (int Cbimm12hi, Register Crs1, Register Crs2, int Cbimm12lo); 
  inline void jalr     (Register Crd , Register Crs1, int Cimm12   ); 
  inline void jal      (Register Crd , int Cjimm20  ); 
  inline void lui      (Register Crd , int Cimm20   ); 
  inline void auipc    (Register Crd , int Cimm20   ); 
  inline void addi     (Register Crd , Register Crs1, int Cimm12   ); 
  inline void slli     (Register Crd , Register Crs1, int Cshamt   ); 
  inline void slti     (Register Crd , Register Crs1, int Cimm12   ); 
  inline void sltiu    (Register Crd , Register Crs1, int Cimm12   ); 
  inline void xori     (Register Crd , Register Crs1, int Cimm12   ); 
  inline void srli     (Register Crd , Register Crs1, int Cshamt   ); 
  inline void srai     (Register Crd , Register Crs1, int Cshamt   ); 
  inline void ori      (Register Crd , Register Crs1, int Cimm12   ); 
  inline void andi     (Register Crd , Register Crs1, int Cimm12   ); 
  inline void add      (Register Crd , Register Crs1, Register Crs2); 
  inline void sub      (Register Crd , Register Crs1, Register Crs2); 
  inline void sll      (Register Crd , Register Crs1, Register Crs2); 
  inline void slt      (Register Crd , Register Crs1, Register Crs2); 
  inline void sltu     (Register Crd , Register Crs1, Register Crs2); 
  inline void xor_     (Register Crd , Register Crs1, Register Crs2); 
  inline void srl      (Register Crd , Register Crs1, Register Crs2); 
  inline void sra      (Register Crd , Register Crs1, Register Crs2); 
  inline void or_      (Register Crd , Register Crs1, Register Crs2); 
  inline void and_     (Register Crd , Register Crs1, Register Crs2); 
  inline void addiw    (Register Crd , Register Crs1, int Cimm12   ); 
  inline void slliw    (Register Crd , Register Crs1, int Cshamtw  );
  inline void srliw    (Register Crd , Register Crs1, int Cshamtw  );
  inline void sraiw    (Register Crd , Register Crs1, int Cshamtw  );
  inline void addw     (Register Crd , Register Crs1, Register Crs2); 
  inline void subw     (Register Crd , Register Crs1, Register Crs2); 
  inline void sllw     (Register Crd , Register Crs1, Register Crs2); 
  inline void srlw     (Register Crd , Register Crs1, Register Crs2); 
  inline void sraw     (Register Crd , Register Crs1, Register Crs2); 
  inline void lb       (Register Crd , Register Crs1, int Cimm12   ); 
  inline void lh       (Register Crd , Register Crs1, int Cimm12   ); 
  inline void lw       (Register Crd , Register Crs1, int Cimm12   ); 
  inline void ld       (Register Crd , Register Crs1, int Cimm12   ); 
  inline void lbu      (Register Crd , Register Crs1, int Cimm12   ); 
  inline void lhu      (Register Crd , Register Crs1, int Cimm12   ); 
  inline void lwu      (Register Crd , Register Crs1, int Cimm12   ); 
  inline void sb       (int Cimm12hi , Register Crs1, Register Crs2, int Cimm12lo );
  inline void sh       (int Cimm12hi , Register Crs1, Register Crs2, int Cimm12lo );
  inline void sw       (int Cimm12hi , Register Crs1, Register Crs2, int Cimm12lo );
  inline void sd       (int Cimm12hi , Register Crs1, Register Crs2, int Cimm12lo );
  inline void fence    (int Cpred         , int Csucc         ); 
  inline void fence_i  (); 
  inline void mul      (Register Crd , Register Crs1, Register Crs2); 
  inline void mulh     (Register Crd , Register Crs1, Register Crs2); 
  inline void mulhsu   (Register Crd , Register Crs1, Register Crs2); 
  inline void mulhu    (Register Crd , Register Crs1, Register Crs2); 
  inline void div      (Register Crd , Register Crs1, Register Crs2); 
  inline void divu     (Register Crd , Register Crs1, Register Crs2); 
  inline void rem      (Register Crd , Register Crs1, Register Crs2); 
  inline void remu     (Register Crd , Register Crs1, Register Crs2); 
  inline void mulw     (Register Crd , Register Crs1, Register Crs2); 
  inline void divw     (Register Crd , Register Crs1, Register Crs2); 
  inline void divuw    (Register Crd , Register Crs1, Register Crs2); 
  inline void remw     (Register Crd , Register Crs1, Register Crs2); 
  inline void remuw    (Register Crd , Register Crs1, Register Crs2); 
  inline void amoadd_w (Register Crd , Register Crs1, Register Crs2, int Caqrl    ); 
  inline void amoxor_w (Register Crd , Register Crs1, Register Crs2, int Caqrl    ); 
  inline void amoor_w  (Register Crd , Register Crs1, Register Crs2, int Caqrl    ); 
  inline void amoand_w (Register Crd , Register Crs1, Register Crs2, int Caqrl    ); 
  inline void amomin_w (Register Crd , Register Crs1, Register Crs2, int Caqrl    ); 
  inline void amomax_w (Register Crd , Register Crs1, Register Crs2, int Caqrl    ); 
  inline void amominu_w(Register Crd , Register Crs1, Register Crs2, int Caqrl    ); 
  inline void amomaxu_w(Register Crd , Register Crs1, Register Crs2, int Caqrl    ); 
  inline void amoswap_w(Register Crd , Register Crs1, Register Crs2, int Caqrl    ); 
  inline void lr_w     (Register Crd , Register Crs1, int Caqrl    ); 
  inline void sc_w     (Register Crd , Register Crs1, Register Crs2, int Caqrl    ); 
  inline void amoadd_d (Register Crd , Register Crs1, Register Crs2, int Caqrl    ); 
  inline void amoxor_d (Register Crd , Register Crs1, Register Crs2, int Caqrl    ); 
  inline void amoor_d  (Register Crd , Register Crs1, Register Crs2, int Caqrl    ); 
  inline void amoand_d (Register Crd , Register Crs1, Register Crs2, int Caqrl    ); 
  inline void amomin_d (Register Crd , Register Crs1, Register Crs2, int Caqrl    ); 
  inline void amomax_d (Register Crd , Register Crs1, Register Crs2, int Caqrl    ); 
  inline void amominu_d(Register Crd , Register Crs1, Register Crs2, int Caqrl    ); 
  inline void amomaxu_d(Register Crd , Register Crs1, Register Crs2, int Caqrl    ); 
  inline void amoswap_d(Register Crd , Register Crs1, Register Crs2, int Caqrl    ); 
  inline void lr_d     (Register Crd , Register Crs1, int Caqrl    ); 
  inline void sc_d     (Register Crd , Register Crs1, Register Crs2, int Caqrl    ); 
  inline void scall    (); 
  inline void sbreak   (); 
  inline void sret     (); 
  inline void sfence_vm(Register Crs1);
  inline void wfi      (); 
  inline void mrth     (); 
  inline void mrts     (); 
  inline void hrts     (); 
  inline void csrrw    (Register Crd , Register Crs1, int Cimm12   ); 
  inline void csrrs    (Register Crd , Register Crs1, int Cimm12   ); 
  inline void csrrc    (Register Crd , Register Crs1, int Cimm12   ); 
  inline void csrrwi   (Register Crd , Register Crs1, int Cimm12   ); 
  inline void csrrsi   (Register Crd , Register Crs1, int Cimm12   ); 
  inline void csrrci   (Register Crd , Register Crs1, int Cimm12   ); 
  inline void fadd_s   (FloatRegister Crd , FloatRegister Crs1, FloatRegister Crs2, int Crm           ); 
  inline void fsub_s   (FloatRegister Crd , FloatRegister Crs1, FloatRegister Crs2, int Crm           ); 
  inline void fmul_s   (FloatRegister Crd , FloatRegister Crs1, FloatRegister Crs2, int Crm           ); 
  inline void fdiv_s   (FloatRegister Crd , FloatRegister Crs1, FloatRegister Crs2, int Crm           ); 
  inline void fsgnj_s  (FloatRegister Crd , FloatRegister Crs1, FloatRegister Crs2); 
  inline void fsgnjn_s (FloatRegister Crd , FloatRegister Crs1, FloatRegister Crs2); 
  inline void fsgnjx_s (FloatRegister Crd , FloatRegister Crs1, FloatRegister Crs2); 
  inline void fmin_s   (FloatRegister Crd , FloatRegister Crs1, FloatRegister Crs2); 
  inline void fmax_s   (FloatRegister Crd , FloatRegister Crs1, FloatRegister Crs2); 
  inline void fsqrt_s  (FloatRegister Crd , FloatRegister Crs1, int Crm           ); 
  inline void fadd_d   (FloatRegister Crd , FloatRegister Crs1, FloatRegister Crs2, int Crm           ); 
  inline void fsub_d   (FloatRegister Crd , FloatRegister Crs1, FloatRegister Crs2, int Crm           ); 
  inline void fmul_d   (FloatRegister Crd , FloatRegister Crs1, FloatRegister Crs2, int Crm           ); 
  inline void fdiv_d   (FloatRegister Crd , FloatRegister Crs1, FloatRegister Crs2, int Crm           ); 
  inline void fsgnj_d  (FloatRegister Crd , FloatRegister Crs1, FloatRegister Crs2); 
  inline void fsgnjn_d (FloatRegister Crd , FloatRegister Crs1, FloatRegister Crs2); 
  inline void fsgnjx_d (FloatRegister Crd , FloatRegister Crs1, FloatRegister Crs2); 
  inline void fmin_d   (FloatRegister Crd , FloatRegister Crs1, FloatRegister Crs2); 
  inline void fmax_d   (FloatRegister Crd , FloatRegister Crs1, FloatRegister Crs2); 
  inline void fcvt_s_d (FloatRegister Crd , FloatRegister Crs1, int Crm           ); 
  inline void fcvt_d_s (FloatRegister Crd , FloatRegister Crs1, int Crm           ); 
  inline void fsqrt_d  (FloatRegister Crd , FloatRegister Crs1, int Crm           ); 
  inline void fle_s    (Register Crd      , FloatRegister Crs1, FloatRegister Crs2); 
  inline void flt_s    (Register Crd      , FloatRegister Crs1, FloatRegister Crs2); 
  inline void feq_s    (Register Crd      , FloatRegister Crs1, FloatRegister Crs2); 
  inline void fle_d    (Register Crd      , FloatRegister Crs1, FloatRegister Crs2); 
  inline void flt_d    (Register Crd      , FloatRegister Crs1, FloatRegister Crs2); 
  inline void feq_d    (Register Crd      , FloatRegister Crs1, FloatRegister Crs2); 
  inline void fcvt_w_s (Register Crd      , FloatRegister Crs1, int Crm           ); 
  inline void fcvt_wu_s(Register Crd      , FloatRegister Crs1, int Crm           ); 
  inline void fcvt_l_s (Register Crd      , FloatRegister Crs1, int Crm           ); 
  inline void fcvt_lu_s(Register Crd      , FloatRegister Crs1, int Crm           ); 
  inline void fmv_x_s  (Register Crd      , FloatRegister Crs1); 
  inline void fclass_s (Register Crd      , FloatRegister Crs1); 
  inline void fcvt_w_d (Register Crd      , FloatRegister Crs1, int Crm           );
  inline void fcvt_wu_d(Register Crd      , FloatRegister Crs1, int Crm           );
  inline void fcvt_l_d (Register Crd      , FloatRegister Crs1, int Crm           );
  inline void fcvt_lu_d(Register Crd      , FloatRegister Crs1, int Crm           );
  inline void fmv_x_d  (Register Crd      , FloatRegister Crs1); 
  inline void fclass_d (Register Crd      , FloatRegister Crs1); 
  inline void fcvt_s_w (FloatRegister Crd , Register Crs1     , int Crm           ); 
  inline void fcvt_s_wu(FloatRegister Crd , Register Crs1     , int Crm           ); 
  inline void fcvt_s_l (FloatRegister Crd , Register Crs1     , int Crm           ); 
  inline void fcvt_s_lu(FloatRegister Crd , Register Crs1     , int Crm           ); 
  inline void fmv_s_x  (FloatRegister Crd , Register Crs1     ); 
  inline void fcvt_d_w (FloatRegister Crd , Register Crs1     , int Crm           ); 
  inline void fcvt_d_wu(FloatRegister Crd , Register Crs1     , int Crm           ); 
  inline void fcvt_d_l (FloatRegister Crd , Register Crs1     , int Crm           ); 
  inline void fcvt_d_lu(FloatRegister Crd , Register Crs1     , int Crm           ); 
  inline void fmv_d_x  (FloatRegister Crd , Register Crs1     ); 
  inline void flw      (FloatRegister Crd , Register Crs1     , int Cimm12        ); 
  inline void fld      (FloatRegister Crd , Register Crs1     , int Cimm12        ); 
  inline void fsw      (int Cimm12hi      , Register Crs1     , FloatRegister Crs2, int Cimm12lo      ); 
  inline void fsd      (int Cimm12hi      , Register Crs1     , FloatRegister Crs2, int Cimm12lo      ); 
  inline void fmadd_s  (FloatRegister Crd , FloatRegister Crs1, FloatRegister Crs2, FloatRegister Crs3, int Crm           ); 
  inline void fmsub_s  (FloatRegister Crd , FloatRegister Crs1, FloatRegister Crs2, FloatRegister Crs3, int Crm           ); 
  inline void fnmsub_s (FloatRegister Crd , FloatRegister Crs1, FloatRegister Crs2, FloatRegister Crs3, int Crm           ); 
  inline void fnmadd_s (FloatRegister Crd , FloatRegister Crs1, FloatRegister Crs2, FloatRegister Crs3, int Crm           ); 
  inline void fmadd_d  (FloatRegister Crd , FloatRegister Crs1, FloatRegister Crs2, FloatRegister Crs3, int Crm           ); 
  inline void fmsub_d  (FloatRegister Crd , FloatRegister Crs1, FloatRegister Crs2, FloatRegister Crs3, int Crm           ); 
  inline void fnmsub_d (FloatRegister Crd , FloatRegister Crs1, FloatRegister Crs2, FloatRegister Crs3, int Crm           ); 
  inline void fnmadd_d (FloatRegister Crd , FloatRegister Crs1, FloatRegister Crs2, FloatRegister Crs3, int Crm           ); 

    /* Aliases for instruction emitters (purely for readability); */
  inline void beq  (Register a     , Register b,   Label& l); 
  inline void bne  (Register a     , Register b,   Label& l); 
  inline void blt  (Register a     , Register b,   Label& l); 
  inline void bge  (Register a     , Register b,   Label& l); 
  inline void bltu (Register a     , Register b,   Label& l); 
  inline void bgeu (Register a     , Register b,   Label& l); 
  inline void sb   (Register a     , int a_imm12,  Register b); 
  inline void sh   (Register a     , int a_imm12,  Register b); 
  inline void sw   (Register a     , int a_imm12,  Register b); 
  inline void sd   (Register a     , int a_imm12,  Register b); 
  inline void fsw  (FloatRegister a, int a_imm12,  Register b); 
  inline void fsd  (FloatRegister a, int a_imm12,  Register b); 
  inline void lb   (Register d     , int a_imm12,  Register a); 
  inline void lh   (Register d     , int a_imm12,  Register a); 
  inline void lw   (Register d     , int a_imm12,  Register a); 
  inline void ld   (Register d     , int a_imm12,  Register a); 
  inline void lbu  (Register d     , int a_imm12,  Register a); 
  inline void lhu  (Register d     , int a_imm12,  Register a); 
  inline void lwu  (Register d     , int a_imm12,  Register a); 
  inline void flw  (FloatRegister d, int a_imm12,  Register a); 
  inline void fld  (FloatRegister d, int a_imm12,  Register a); 

  /* Some useful pseudo-instructions */
         void li   (Register d     , intptr_t x     );
  inline void mv   (Register d     , Register s     ); 
  inline void fmv_s(FloatRegister d, FloatRegister s); 
  inline void fmv_d(FloatRegister d, FloatRegister s); 
  inline void j    (Label& l       ); 
  inline void jr   (Register d     ); 
  inline void nop  ();

  /* Check if instruction is of a certain kind */
  static bool is_beq(int x) { return MATCH_BEQ == (x & MASK_BEQ); }
  static bool is_bne(int x) { return MATCH_BNE == (x & MASK_BNE); }
  static bool is_blt(int x) { return MATCH_BLT == (x & MASK_BLT); }
  static bool is_bge(int x) { return MATCH_BGE == (x & MASK_BGE); }
  static bool is_bltu(int x) { return MATCH_BLTU == (x & MASK_BLTU); }
  static bool is_bgeu(int x) { return MATCH_BGEU == (x & MASK_BGEU); }
  static bool is_jalr(int x) { return MATCH_JALR == (x & MASK_JALR); }
  static bool is_jal(int x) { return MATCH_JAL == (x & MASK_JAL); }
  static bool is_lui(int x) { return MATCH_LUI == (x & MASK_LUI); }
  static bool is_auipc(int x) { return MATCH_AUIPC == (x & MASK_AUIPC); }
  static bool is_addi(int x) { return MATCH_ADDI == (x & MASK_ADDI); }
  static bool is_slli(int x) { return MATCH_SLLI == (x & MASK_SLLI); }
  static bool is_slti(int x) { return MATCH_SLTI == (x & MASK_SLTI); }
  static bool is_sltiu(int x) { return MATCH_SLTIU == (x & MASK_SLTIU); }
  static bool is_xori(int x) { return MATCH_XORI == (x & MASK_XORI); }
  static bool is_srli(int x) { return MATCH_SRLI == (x & MASK_SRLI); }
  static bool is_srai(int x) { return MATCH_SRAI == (x & MASK_SRAI); }
  static bool is_ori(int x) { return MATCH_ORI == (x & MASK_ORI); }
  static bool is_andi(int x) { return MATCH_ANDI == (x & MASK_ANDI); }
  static bool is_add(int x) { return MATCH_ADD == (x & MASK_ADD); }
  static bool is_sub(int x) { return MATCH_SUB == (x & MASK_SUB); }
  static bool is_sll(int x) { return MATCH_SLL == (x & MASK_SLL); }
  static bool is_slt(int x) { return MATCH_SLT == (x & MASK_SLT); }
  static bool is_sltu(int x) { return MATCH_SLTU == (x & MASK_SLTU); }
  static bool is_xor(int x) { return MATCH_XOR_ == (x & MASK_XOR_); }
  static bool is_srl(int x) { return MATCH_SRL == (x & MASK_SRL); }
  static bool is_sra(int x) { return MATCH_SRA == (x & MASK_SRA); }
  static bool is_or(int x) { return MATCH_OR_ == (x & MASK_OR_); }
  static bool is_and(int x) { return MATCH_AND_ == (x & MASK_AND_); }
  static bool is_addiw(int x) { return MATCH_ADDIW == (x & MASK_ADDIW); }
  static bool is_slliw(int x) { return MATCH_SLLIW == (x & MASK_SLLIW); }
  static bool is_srliw(int x) { return MATCH_SRLIW == (x & MASK_SRLIW); }
  static bool is_sraiw(int x) { return MATCH_SRAIW == (x & MASK_SRAIW); }
  static bool is_addw(int x) { return MATCH_ADDW == (x & MASK_ADDW); }
  static bool is_subw(int x) { return MATCH_SUBW == (x & MASK_SUBW); }
  static bool is_sllw(int x) { return MATCH_SLLW == (x & MASK_SLLW); }
  static bool is_srlw(int x) { return MATCH_SRLW == (x & MASK_SRLW); }
  static bool is_sraw(int x) { return MATCH_SRAW == (x & MASK_SRAW); }
  static bool is_lb(int x) { return MATCH_LB == (x & MASK_LB); }
  static bool is_lh(int x) { return MATCH_LH == (x & MASK_LH); }
  static bool is_lw(int x) { return MATCH_LW == (x & MASK_LW); }
  static bool is_ld(int x) { return MATCH_LD == (x & MASK_LD); }
  static bool is_lbu(int x) { return MATCH_LBU == (x & MASK_LBU); }
  static bool is_lhu(int x) { return MATCH_LHU == (x & MASK_LHU); }
  static bool is_lwu(int x) { return MATCH_LWU == (x & MASK_LWU); }
  static bool is_sb(int x) { return MATCH_SB == (x & MASK_SB); }
  static bool is_sh(int x) { return MATCH_SH == (x & MASK_SH); }
  static bool is_sw(int x) { return MATCH_SW == (x & MASK_SW); }
  static bool is_sd(int x) { return MATCH_SD == (x & MASK_SD); }
  static bool is_fence(int x) { return MATCH_FENCE == (x & MASK_FENCE); }
  static bool is_fence_i(int x) { return MATCH_FENCE_I == (x & MASK_FENCE_I); }
  static bool is_mul(int x) { return MATCH_MUL == (x & MASK_MUL); }
  static bool is_mulh(int x) { return MATCH_MULH == (x & MASK_MULH); }
  static bool is_mulhsu(int x) { return MATCH_MULHSU == (x & MASK_MULHSU); }
  static bool is_mulhu(int x) { return MATCH_MULHU == (x & MASK_MULHU); }
  static bool is_div(int x) { return MATCH_DIV == (x & MASK_DIV); }
  static bool is_divu(int x) { return MATCH_DIVU == (x & MASK_DIVU); }
  static bool is_rem(int x) { return MATCH_REM == (x & MASK_REM); }
  static bool is_remu(int x) { return MATCH_REMU == (x & MASK_REMU); }
  static bool is_mulw(int x) { return MATCH_MULW == (x & MASK_MULW); }
  static bool is_divw(int x) { return MATCH_DIVW == (x & MASK_DIVW); }
  static bool is_divuw(int x) { return MATCH_DIVUW == (x & MASK_DIVUW); }
  static bool is_remw(int x) { return MATCH_REMW == (x & MASK_REMW); }
  static bool is_remuw(int x) { return MATCH_REMUW == (x & MASK_REMUW); }
  static bool is_amoadd_w(int x) { return MATCH_AMOADD_W == (x & MASK_AMOADD_W); }
  static bool is_amoxor_w(int x) { return MATCH_AMOXOR_W == (x & MASK_AMOXOR_W); }
  static bool is_amoor_w(int x) { return MATCH_AMOOR_W == (x & MASK_AMOOR_W); }
  static bool is_amoand_w(int x) { return MATCH_AMOAND_W == (x & MASK_AMOAND_W); }
  static bool is_amomin_w(int x) { return MATCH_AMOMIN_W == (x & MASK_AMOMIN_W); }
  static bool is_amomax_w(int x) { return MATCH_AMOMAX_W == (x & MASK_AMOMAX_W); }
  static bool is_amominu_w(int x) { return MATCH_AMOMINU_W == (x & MASK_AMOMINU_W); }
  static bool is_amomaxu_w(int x) { return MATCH_AMOMAXU_W == (x & MASK_AMOMAXU_W); }
  static bool is_amoswap_w(int x) { return MATCH_AMOSWAP_W == (x & MASK_AMOSWAP_W); }
  static bool is_lr_w(int x) { return MATCH_LR_W == (x & MASK_LR_W); }
  static bool is_sc_w(int x) { return MATCH_SC_W == (x & MASK_SC_W); }
  static bool is_amoadd_d(int x) { return MATCH_AMOADD_D == (x & MASK_AMOADD_D); }
  static bool is_amoxor_d(int x) { return MATCH_AMOXOR_D == (x & MASK_AMOXOR_D); }
  static bool is_amoor_d(int x) { return MATCH_AMOOR_D == (x & MASK_AMOOR_D); }
  static bool is_amoand_d(int x) { return MATCH_AMOAND_D == (x & MASK_AMOAND_D); }
  static bool is_amomin_d(int x) { return MATCH_AMOMIN_D == (x & MASK_AMOMIN_D); }
  static bool is_amomax_d(int x) { return MATCH_AMOMAX_D == (x & MASK_AMOMAX_D); }
  static bool is_amominu_d(int x) { return MATCH_AMOMINU_D == (x & MASK_AMOMINU_D); }
  static bool is_amomaxu_d(int x) { return MATCH_AMOMAXU_D == (x & MASK_AMOMAXU_D); }
  static bool is_amoswap_d(int x) { return MATCH_AMOSWAP_D == (x & MASK_AMOSWAP_D); }
  static bool is_lr_d(int x) { return MATCH_LR_D == (x & MASK_LR_D); }
  static bool is_sc_d(int x) { return MATCH_SC_D == (x & MASK_SC_D); }
  static bool is_scall(int x) { return MATCH_SCALL == (x & MASK_SCALL); }
  static bool is_sbreak(int x) { return MATCH_SBREAK == (x & MASK_SBREAK); }
  static bool is_sret(int x) { return MATCH_SRET == (x & MASK_SRET); }
  static bool is_sfence_vm(int x) { return MATCH_SFENCE_VM == (x & MASK_SFENCE_VM); }
  static bool is_wfi(int x) { return MATCH_WFI == (x & MASK_WFI); }
  static bool is_mrth(int x) { return MATCH_MRTH == (x & MASK_MRTH); }
  static bool is_mrts(int x) { return MATCH_MRTS == (x & MASK_MRTS); }
  static bool is_hrts(int x) { return MATCH_HRTS == (x & MASK_HRTS); }
  static bool is_csrrw(int x) { return MATCH_CSRRW == (x & MASK_CSRRW); }
  static bool is_csrrs(int x) { return MATCH_CSRRS == (x & MASK_CSRRS); }
  static bool is_csrrc(int x) { return MATCH_CSRRC == (x & MASK_CSRRC); }
  static bool is_csrrwi(int x) { return MATCH_CSRRWI == (x & MASK_CSRRWI); }
  static bool is_csrrsi(int x) { return MATCH_CSRRSI == (x & MASK_CSRRSI); }
  static bool is_csrrci(int x) { return MATCH_CSRRCI == (x & MASK_CSRRCI); }
  static bool is_fadd_s(int x) { return MATCH_FADD_S == (x & MASK_FADD_S); }
  static bool is_fsub_s(int x) { return MATCH_FSUB_S == (x & MASK_FSUB_S); }
  static bool is_fmul_s(int x) { return MATCH_FMUL_S == (x & MASK_FMUL_S); }
  static bool is_fdiv_s(int x) { return MATCH_FDIV_S == (x & MASK_FDIV_S); }
  static bool is_fsgnj_s(int x) { return MATCH_FSGNJ_S == (x & MASK_FSGNJ_S); }
  static bool is_fsgnjn_s(int x) { return MATCH_FSGNJN_S == (x & MASK_FSGNJN_S); }
  static bool is_fsgnjx_s(int x) { return MATCH_FSGNJX_S == (x & MASK_FSGNJX_S); }
  static bool is_fmin_s(int x) { return MATCH_FMIN_S == (x & MASK_FMIN_S); }
  static bool is_fmax_s(int x) { return MATCH_FMAX_S == (x & MASK_FMAX_S); }
  static bool is_fsqrt_s(int x) { return MATCH_FSQRT_S == (x & MASK_FSQRT_S); }
  static bool is_fadd_d(int x) { return MATCH_FADD_D == (x & MASK_FADD_D); }
  static bool is_fsub_d(int x) { return MATCH_FSUB_D == (x & MASK_FSUB_D); }
  static bool is_fmul_d(int x) { return MATCH_FMUL_D == (x & MASK_FMUL_D); }
  static bool is_fdiv_d(int x) { return MATCH_FDIV_D == (x & MASK_FDIV_D); }
  static bool is_fsgnj_d(int x) { return MATCH_FSGNJ_D == (x & MASK_FSGNJ_D); }
  static bool is_fsgnjn_d(int x) { return MATCH_FSGNJN_D == (x & MASK_FSGNJN_D); }
  static bool is_fsgnjx_d(int x) { return MATCH_FSGNJX_D == (x & MASK_FSGNJX_D); }
  static bool is_fmin_d(int x) { return MATCH_FMIN_D == (x & MASK_FMIN_D); }
  static bool is_fmax_d(int x) { return MATCH_FMAX_D == (x & MASK_FMAX_D); }
  static bool is_fcvt_s_d(int x) { return MATCH_FCVT_S_D == (x & MASK_FCVT_S_D); }
  static bool is_fcvt_d_s(int x) { return MATCH_FCVT_D_S == (x & MASK_FCVT_D_S); }
  static bool is_fsqrt_d(int x) { return MATCH_FSQRT_D == (x & MASK_FSQRT_D); }
  static bool is_fle_s(int x) { return MATCH_FLE_S == (x & MASK_FLE_S); }
  static bool is_flt_s(int x) { return MATCH_FLT_S == (x & MASK_FLT_S); }
  static bool is_feq_s(int x) { return MATCH_FEQ_S == (x & MASK_FEQ_S); }
  static bool is_fle_d(int x) { return MATCH_FLE_D == (x & MASK_FLE_D); }
  static bool is_flt_d(int x) { return MATCH_FLT_D == (x & MASK_FLT_D); }
  static bool is_feq_d(int x) { return MATCH_FEQ_D == (x & MASK_FEQ_D); }
  static bool is_fcvt_w_s(int x) { return MATCH_FCVT_W_S == (x & MASK_FCVT_W_S); }
  static bool is_fcvt_wu_s(int x) { return MATCH_FCVT_WU_S == (x & MASK_FCVT_WU_S); }
  static bool is_fcvt_l_s(int x) { return MATCH_FCVT_L_S == (x & MASK_FCVT_L_S); }
  static bool is_fcvt_lu_s(int x) { return MATCH_FCVT_LU_S == (x & MASK_FCVT_LU_S); }
  static bool is_fmv_x_s(int x) { return MATCH_FMV_X_S == (x & MASK_FMV_X_S); }
  static bool is_fclass_s(int x) { return MATCH_FCLASS_S == (x & MASK_FCLASS_S); }
  static bool is_fcvt_w_d(int x) { return MATCH_FCVT_W_D == (x & MASK_FCVT_W_D); }
  static bool is_fcvt_wu_d(int x) { return MATCH_FCVT_WU_D == (x & MASK_FCVT_WU_D); }
  static bool is_fcvt_l_d(int x) { return MATCH_FCVT_L_D == (x & MASK_FCVT_L_D); }
  static bool is_fcvt_lu_d(int x) { return MATCH_FCVT_LU_D == (x & MASK_FCVT_LU_D); }
  static bool is_fmv_x_d(int x) { return MATCH_FMV_X_D == (x & MASK_FMV_X_D); }
  static bool is_fclass_d(int x) { return MATCH_FCLASS_D == (x & MASK_FCLASS_D); }
  static bool is_fcvt_s_w(int x) { return MATCH_FCVT_S_W == (x & MASK_FCVT_S_W); }
  static bool is_fcvt_s_wu(int x) { return MATCH_FCVT_S_WU == (x & MASK_FCVT_S_WU); }
  static bool is_fcvt_s_l(int x) { return MATCH_FCVT_S_L == (x & MASK_FCVT_S_L); }
  static bool is_fcvt_s_lu(int x) { return MATCH_FCVT_S_LU == (x & MASK_FCVT_S_LU); }
  static bool is_fmv_s_x(int x) { return MATCH_FMV_S_X == (x & MASK_FMV_S_X); }
  static bool is_fcvt_d_w(int x) { return MATCH_FCVT_D_W == (x & MASK_FCVT_D_W); }
  static bool is_fcvt_d_wu(int x) { return MATCH_FCVT_D_WU == (x & MASK_FCVT_D_WU); }
  static bool is_fcvt_d_l(int x) { return MATCH_FCVT_D_L == (x & MASK_FCVT_D_L); }
  static bool is_fcvt_d_lu(int x) { return MATCH_FCVT_D_LU == (x & MASK_FCVT_D_LU); }
  static bool is_fmv_d_x(int x) { return MATCH_FMV_D_X == (x & MASK_FMV_D_X); }
  static bool is_flw(int x) { return MATCH_FLW == (x & MASK_FLW); }
  static bool is_fld(int x) { return MATCH_FLD == (x & MASK_FLD); }
  static bool is_fsw(int x) { return MATCH_FSW == (x & MASK_FSW); }
  static bool is_fsd(int x) { return MATCH_FSD == (x & MASK_FSD); }
  static bool is_fmadd_s(int x) { return MATCH_FMADD_S == (x & MASK_FMADD_S); }
  static bool is_fmsub_s(int x) { return MATCH_FMSUB_S == (x & MASK_FMSUB_S); }
  static bool is_fnmsub_s(int x) { return MATCH_FNMSUB_S == (x & MASK_FNMSUB_S); }
  static bool is_fnmadd_s(int x) { return MATCH_FNMADD_S == (x & MASK_FNMADD_S); }
  static bool is_fmadd_d(int x) { return MATCH_FMADD_D == (x & MASK_FMADD_D); }
  static bool is_fmsub_d(int x) { return MATCH_FMSUB_D == (x & MASK_FMSUB_D); }
  static bool is_fnmsub_d(int x) { return MATCH_FNMSUB_D == (x & MASK_FNMSUB_D); }
  static bool is_fnmadd_d(int x) { return MATCH_FNMADD_D == (x & MASK_FNMADD_D); }

  // Emit several instructions to load a 64 bit constant. This issues a fixed
  // instruction pattern so that the constant can be patched later on.
  enum {
    load_const_size = 8 * BytesPerInstWord,
    Store = 0x5,
    Load = 0xA,
    Full = 0xF,
  };
  inline void load_const( Register d, void* a          );
  inline void load_const( Register d, Label& L         );
  inline void load_const( Register d, AddressLiteral& a);
          int load_vm_ptr(Register d, void* a          );
         void load_const_patchable(Register d, intptr_t x);

  // Trap instructions TO bits
  enum trap_to_bits {
    // single bits
    traptoLessThanSigned      = 1 << 4, // 0, left end
    traptoGreaterThanSigned   = 1 << 3,
    traptoEqual               = 1 << 2,
    traptoLessThanUnsigned    = 1 << 1,
    traptoGreaterThanUnsigned = 1 << 0, // 4, right end

    // compound ones
    traptoUnconditional       = (traptoLessThanSigned |
                                 traptoGreaterThanSigned |
                                 traptoEqual |
                                 traptoLessThanUnsigned |
                                 traptoGreaterThanUnsigned)
  };

  // Creation
  Assembler(CodeBuffer* code) : AbstractAssembler(code) {
#ifdef ASSERT
    debug_trace = 0;
    debug_emit = 0;
#endif
#ifdef CHECK_DELAY
    delay_state = no_delay;
#endif
  }

  // Testing
#ifndef PRODUCT
  void test_asm();
#endif
};


#endif // CPU_RISCV_VM_ASSEMBLER_RISCV_HPP
