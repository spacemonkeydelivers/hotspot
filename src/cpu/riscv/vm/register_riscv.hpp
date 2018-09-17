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

#ifndef CPU_RISCV_VM_REGISTER_RISCV_HPP
#define CPU_RISCV_VM_REGISTER_RISCV_HPP

#include "asm/register.hpp"
#include "vm_version_riscv.hpp"

// forward declaration
class Address;
class VMRegImpl;
typedef VMRegImpl* VMReg;

//  RISCV64 registers
//
//  See riscv.org/download.html#tab_spec_user_isa
//  x0        Hard-wired zero (N/A)
//  x1        Return address (volatile)
//  x2        Stack pointer (nonvolatile)
//  x3        Global pointer (N/A)
//  x4        Thread pointer (N/A)
//  x5-7      Temporaries (volatile)
//  x8        Saved register/frame pointer (nonvolatile)
//  x9        Saved register (nonvolatile)
//  x10-11    Function arguments/return values (volatile)
//  x12-17    Function arguments (volatile)
//  x18-27    Saved registers (nonvolatile)
//  x28-31    Temporaries (volatile)
//
//  f0-7      FP temporaries (volatile)
//  f8-f9     FP saved registers (nonvolatile)
//  f10-f11   FP arguments/return values (volatile)
//  f12-f17   FP arguments (volatile)
//  f18-f27   FP saved registers (nonvolatile)
//  f28-f31   FP temporaries (volatile)
//


// Use Register as shortcut
class RegisterImpl;
typedef RegisterImpl* Register;

inline Register as_Register(int encoding) {
  assert(encoding >= 0 && encoding < 32, "bad register encoding");
  return (Register)(intptr_t)encoding;
}

// The implementation of integer registers for the Power architecture
class RegisterImpl: public AbstractRegisterImpl {
 public:
  enum {
    number_of_registers = 32
  };

  // general construction
  inline friend Register as_Register(int encoding);

  // accessors
  int      encoding()  const { assert(is_valid(), "invalid register"); return value(); }
  VMReg    as_VMReg();
  Register successor() const { return as_Register(encoding() + 1); }

  // TODO: update testers
  // testers
  bool is_valid()       const { return ( 0 <= (value()&0x7F) && (value()&0x7F) <  number_of_registers); }
  bool is_volatile()    const { return ( 0 <= (value()&0x7F) && (value()&0x7F) <= 13 ); }
  bool is_nonvolatile() const { return (14 <= (value()&0x7F) && (value()&0x7F) <= 31 ); }

  const char* name() const;
};

// The integer registers of the RISCV architecture
CONSTANT_REGISTER_DECLARATION(Register, noreg, (-1));
CONSTANT_REGISTER_DECLARATION(Register, X0,   (0));
CONSTANT_REGISTER_DECLARATION(Register, X1,   (1));
CONSTANT_REGISTER_DECLARATION(Register, X2,   (2));
CONSTANT_REGISTER_DECLARATION(Register, X3,   (3));
CONSTANT_REGISTER_DECLARATION(Register, X4,   (4));
CONSTANT_REGISTER_DECLARATION(Register, X5,   (5));
CONSTANT_REGISTER_DECLARATION(Register, X6,   (6));
CONSTANT_REGISTER_DECLARATION(Register, X7,   (7));
CONSTANT_REGISTER_DECLARATION(Register, X8,   (8));
CONSTANT_REGISTER_DECLARATION(Register, X9,   (9));
CONSTANT_REGISTER_DECLARATION(Register, X10, (10));
CONSTANT_REGISTER_DECLARATION(Register, X11, (11));
CONSTANT_REGISTER_DECLARATION(Register, X12, (12));
CONSTANT_REGISTER_DECLARATION(Register, X13, (13));
CONSTANT_REGISTER_DECLARATION(Register, X14, (14));
CONSTANT_REGISTER_DECLARATION(Register, X15, (15));
CONSTANT_REGISTER_DECLARATION(Register, X16, (16));
CONSTANT_REGISTER_DECLARATION(Register, X17, (17));
CONSTANT_REGISTER_DECLARATION(Register, X18, (18));
CONSTANT_REGISTER_DECLARATION(Register, X19, (19));
CONSTANT_REGISTER_DECLARATION(Register, X20, (20));
CONSTANT_REGISTER_DECLARATION(Register, X21, (21));
CONSTANT_REGISTER_DECLARATION(Register, X22, (22));
CONSTANT_REGISTER_DECLARATION(Register, X23, (23));
CONSTANT_REGISTER_DECLARATION(Register, X24, (24));
CONSTANT_REGISTER_DECLARATION(Register, X25, (25));
CONSTANT_REGISTER_DECLARATION(Register, X26, (26));
CONSTANT_REGISTER_DECLARATION(Register, X27, (27));
CONSTANT_REGISTER_DECLARATION(Register, X28, (28));
CONSTANT_REGISTER_DECLARATION(Register, X29, (29));
CONSTANT_REGISTER_DECLARATION(Register, X30, (30));
CONSTANT_REGISTER_DECLARATION(Register, X31, (31));

//
// Because RISCV has many registers, #define'ing values for them is
// beneficial in code size and is worth the cost of some of the
// dangers of defines. If a particular file has a problem with these
// defines then it's possible to turn them off in that file by
// defining DONT_USE_REGISTER_DEFINES. Register_definition_ppc.cpp
// does that so that it's able to provide real definitions of these
// registers for use in debuggers and such.
//

#ifndef DONT_USE_REGISTER_DEFINES
#define noreg ((Register)(noreg_RegisterEnumValue))
#define X0 ((Register)(X0_RegisterEnumValue))
#define X1 ((Register)(X1_RegisterEnumValue))
#define X2 ((Register)(X2_RegisterEnumValue))
#define X3 ((Register)(X3_RegisterEnumValue))
#define X4 ((Register)(X4_RegisterEnumValue))
#define X5 ((Register)(X5_RegisterEnumValue))
#define X6 ((Register)(X6_RegisterEnumValue))
#define X7 ((Register)(X7_RegisterEnumValue))
#define X8 ((Register)(X8_RegisterEnumValue))
#define X9 ((Register)(X9_RegisterEnumValue))
#define X10 ((Register)(X10_RegisterEnumValue))
#define X11 ((Register)(X11_RegisterEnumValue))
#define X12 ((Register)(X12_RegisterEnumValue))
#define X13 ((Register)(X13_RegisterEnumValue))
#define X14 ((Register)(X14_RegisterEnumValue))
#define X15 ((Register)(X15_RegisterEnumValue))
#define X16 ((Register)(X16_RegisterEnumValue))
#define X17 ((Register)(X17_RegisterEnumValue))
#define X18 ((Register)(X18_RegisterEnumValue))
#define X19 ((Register)(X19_RegisterEnumValue))
#define X20 ((Register)(X20_RegisterEnumValue))
#define X21 ((Register)(X21_RegisterEnumValue))
#define X22 ((Register)(X22_RegisterEnumValue))
#define X23 ((Register)(X23_RegisterEnumValue))
#define X24 ((Register)(X24_RegisterEnumValue))
#define X25 ((Register)(X25_RegisterEnumValue))
#define X26 ((Register)(X26_RegisterEnumValue))
#define X27 ((Register)(X27_RegisterEnumValue))
#define X28 ((Register)(X28_RegisterEnumValue))
#define X29 ((Register)(X29_RegisterEnumValue))
#define X30 ((Register)(X30_RegisterEnumValue))
#define X31 ((Register)(X31_RegisterEnumValue))
#endif

// Use FloatRegister as shortcut
class FloatRegisterImpl;
typedef FloatRegisterImpl* FloatRegister;

inline FloatRegister as_FloatRegister(int encoding) {
  assert(encoding >= 0 && encoding < 32, "bad float register encoding");
  return (FloatRegister)(intptr_t)encoding;
}

// The implementation of float registers for the RISCV architecture
class FloatRegisterImpl: public AbstractRegisterImpl {
 public:
  enum {
    number_of_registers = 32
  };

  // construction
  inline friend FloatRegister as_FloatRegister(int encoding);

  // accessors
  int           encoding() const { assert(is_valid(), "invalid register"); return value(); }
  VMReg         as_VMReg();
  FloatRegister successor() const { return as_FloatRegister(encoding() + 1); }

  // testers
  bool is_valid()       const { return (0  <=  value()       &&  value()       < number_of_registers); }

  const char* name() const;
};

// The float registers of the RISCV architecture
CONSTANT_REGISTER_DECLARATION(FloatRegister, fnoreg, (-1));
CONSTANT_REGISTER_DECLARATION(FloatRegister, F0,  ( 0));
CONSTANT_REGISTER_DECLARATION(FloatRegister, F1,  ( 1));
CONSTANT_REGISTER_DECLARATION(FloatRegister, F2,  ( 2));
CONSTANT_REGISTER_DECLARATION(FloatRegister, F3,  ( 3));
CONSTANT_REGISTER_DECLARATION(FloatRegister, F4,  ( 4));
CONSTANT_REGISTER_DECLARATION(FloatRegister, F5,  ( 5));
CONSTANT_REGISTER_DECLARATION(FloatRegister, F6,  ( 6));
CONSTANT_REGISTER_DECLARATION(FloatRegister, F7,  ( 7));
CONSTANT_REGISTER_DECLARATION(FloatRegister, F8,  ( 8));
CONSTANT_REGISTER_DECLARATION(FloatRegister, F9,  ( 9));
CONSTANT_REGISTER_DECLARATION(FloatRegister, F10, (10));
CONSTANT_REGISTER_DECLARATION(FloatRegister, F11, (11));
CONSTANT_REGISTER_DECLARATION(FloatRegister, F12, (12));
CONSTANT_REGISTER_DECLARATION(FloatRegister, F13, (13));
CONSTANT_REGISTER_DECLARATION(FloatRegister, F14, (14));
CONSTANT_REGISTER_DECLARATION(FloatRegister, F15, (15));
CONSTANT_REGISTER_DECLARATION(FloatRegister, F16, (16));
CONSTANT_REGISTER_DECLARATION(FloatRegister, F17, (17));
CONSTANT_REGISTER_DECLARATION(FloatRegister, F18, (18));
CONSTANT_REGISTER_DECLARATION(FloatRegister, F19, (19));
CONSTANT_REGISTER_DECLARATION(FloatRegister, F20, (20));
CONSTANT_REGISTER_DECLARATION(FloatRegister, F21, (21));
CONSTANT_REGISTER_DECLARATION(FloatRegister, F22, (22));
CONSTANT_REGISTER_DECLARATION(FloatRegister, F23, (23));
CONSTANT_REGISTER_DECLARATION(FloatRegister, F24, (24));
CONSTANT_REGISTER_DECLARATION(FloatRegister, F25, (25));
CONSTANT_REGISTER_DECLARATION(FloatRegister, F26, (26));
CONSTANT_REGISTER_DECLARATION(FloatRegister, F27, (27));
CONSTANT_REGISTER_DECLARATION(FloatRegister, F28, (28));
CONSTANT_REGISTER_DECLARATION(FloatRegister, F29, (29));
CONSTANT_REGISTER_DECLARATION(FloatRegister, F30, (30));
CONSTANT_REGISTER_DECLARATION(FloatRegister, F31, (31));

#ifndef DONT_USE_REGISTER_DEFINES
#define fnoreg ((FloatRegister)(fnoreg_FloatRegisterEnumValue))
#define F0     ((FloatRegister)(    F0_FloatRegisterEnumValue))
#define F1     ((FloatRegister)(    F1_FloatRegisterEnumValue))
#define F2     ((FloatRegister)(    F2_FloatRegisterEnumValue))
#define F3     ((FloatRegister)(    F3_FloatRegisterEnumValue))
#define F4     ((FloatRegister)(    F4_FloatRegisterEnumValue))
#define F5     ((FloatRegister)(    F5_FloatRegisterEnumValue))
#define F6     ((FloatRegister)(    F6_FloatRegisterEnumValue))
#define F7     ((FloatRegister)(    F7_FloatRegisterEnumValue))
#define F8     ((FloatRegister)(    F8_FloatRegisterEnumValue))
#define F9     ((FloatRegister)(    F9_FloatRegisterEnumValue))
#define F10    ((FloatRegister)(   F10_FloatRegisterEnumValue))
#define F11    ((FloatRegister)(   F11_FloatRegisterEnumValue))
#define F12    ((FloatRegister)(   F12_FloatRegisterEnumValue))
#define F13    ((FloatRegister)(   F13_FloatRegisterEnumValue))
#define F14    ((FloatRegister)(   F14_FloatRegisterEnumValue))
#define F15    ((FloatRegister)(   F15_FloatRegisterEnumValue))
#define F16    ((FloatRegister)(   F16_FloatRegisterEnumValue))
#define F17    ((FloatRegister)(   F17_FloatRegisterEnumValue))
#define F18    ((FloatRegister)(   F18_FloatRegisterEnumValue))
#define F19    ((FloatRegister)(   F19_FloatRegisterEnumValue))
#define F20    ((FloatRegister)(   F20_FloatRegisterEnumValue))
#define F21    ((FloatRegister)(   F21_FloatRegisterEnumValue))
#define F22    ((FloatRegister)(   F22_FloatRegisterEnumValue))
#define F23    ((FloatRegister)(   F23_FloatRegisterEnumValue))
#define F24    ((FloatRegister)(   F24_FloatRegisterEnumValue))
#define F25    ((FloatRegister)(   F25_FloatRegisterEnumValue))
#define F26    ((FloatRegister)(   F26_FloatRegisterEnumValue))
#define F27    ((FloatRegister)(   F27_FloatRegisterEnumValue))
#define F28    ((FloatRegister)(   F28_FloatRegisterEnumValue))
#define F29    ((FloatRegister)(   F29_FloatRegisterEnumValue))
#define F30    ((FloatRegister)(   F30_FloatRegisterEnumValue))
#define F31    ((FloatRegister)(   F31_FloatRegisterEnumValue))
#endif // DONT_USE_REGISTER_DEFINES

// Maximum number of incoming arguments that can be passed in i registers.
const int RISCV_ARGS_IN_REGS_NUM = 8;

// Need to know the total number of registers of all sorts for SharedInfo.
// Define a class that exports it.
class ConcreteRegisterImpl : public AbstractRegisterImpl {
 public:
  enum {
    // This number must be large enough to cover REG_COUNT (defined by c2) registers.
    // There is no requirement that any ordering here matches any ordering c2 gives
    // it's optoregs.
    number_of_registers =
        (RegisterImpl::number_of_registers +
        FloatRegisterImpl::number_of_registers) * 2
  };

  static const int max_gpr;
  static const int max_fpr;
};

// Common register declarations used in assembler code.
REGISTER_DECLARATION(Register,      XZERO,      X0);  // N/A
REGISTER_DECLARATION(Register,      X1_RA,      X1);  // volatile
REGISTER_DECLARATION(Register,      X2_SP,      X2);  // non-volatile
REGISTER_DECLARATION(Register,      X5_T0,      X5);  // volatile
REGISTER_DECLARATION(Register,      X6_T1,      X6);  // volatile
REGISTER_DECLARATION(Register,      X7_T2,      X7);  // volatile
REGISTER_DECLARATION(Register,      X8_FP,      X8);  // volatile
REGISTER_DECLARATION(Register,      X8_S0,      X8);  // non-volatile
REGISTER_DECLARATION(Register,      X9_S1,      X9);  // non-volatile
REGISTER_DECLARATION(Register,      X10_ARG0,   X10); // volatile
REGISTER_DECLARATION(Register,      X10_RET0,   X10); // volatile
REGISTER_DECLARATION(Register,      X11_ARG1,   X11); // volatile
REGISTER_DECLARATION(Register,      X11_RET1,   X11); // volatile
REGISTER_DECLARATION(Register,      X12_ARG2,   X12); // volatile
REGISTER_DECLARATION(Register,      X13_ARG3,   X13); // volatile
REGISTER_DECLARATION(Register,      X14_ARG4,   X14); // volatile
REGISTER_DECLARATION(Register,      X15_ARG5,   X15); // volatile
REGISTER_DECLARATION(Register,      X16_ARG6,   X16); // volatile
REGISTER_DECLARATION(Register,      X17_ARG7,   X17); // volatile
REGISTER_DECLARATION(Register,      X28_T3,     X28); // volatile
REGISTER_DECLARATION(Register,      X29_T4,     X29); // volatile
REGISTER_DECLARATION(Register,      XJUNK,      X30); // volatile
REGISTER_DECLARATION(FloatRegister, F10_ARG0,   F10); // volatile
REGISTER_DECLARATION(FloatRegister, F10_RET0,   F10); // volatile
REGISTER_DECLARATION(FloatRegister, F11_ARG1,   F11); // volatile
REGISTER_DECLARATION(FloatRegister, F11_RET1,   F11); // volatile
REGISTER_DECLARATION(FloatRegister, F12_ARG2,   F12); // volatile
REGISTER_DECLARATION(FloatRegister, F13_ARG3,   F13); // volatile
REGISTER_DECLARATION(FloatRegister, F14_ARG4,   F14); // volatile
REGISTER_DECLARATION(FloatRegister, F15_ARG5,   F15); // volatile
REGISTER_DECLARATION(FloatRegister, F16_ARG6,   F16); // volatile
REGISTER_DECLARATION(FloatRegister, F17_ARG7,   F17); // volatile

#ifndef DONT_USE_REGISTER_DEFINES
#define      XZERO      AS_REGISTER(Register, X0)  // N/A
#define      X1_RA      AS_REGISTER(Register, X1)  // volatile
#define      X2_SP      AS_REGISTER(Register, X2)  // non-volatile
#define      X5_T0      AS_REGISTER(Register, X5)  // volatile
#define      X6_T1      AS_REGISTER(Register, X6)  // volatile
#define      X7_T2      AS_REGISTER(Register, X7)  // volatile
#define      X8_FP      AS_REGISTER(Register, X8)  // volatile
#define      X8_S0      AS_REGISTER(Register, X8)  // non-volatile
#define      X9_S1      AS_REGISTER(Register, X9)  // non-volatile
#define      X10_ARG0   AS_REGISTER(Register, X10) // volatile
#define      X10_RET0   AS_REGISTER(Register, X10) // volatile
#define      X11_ARG1   AS_REGISTER(Register, X11) // volatile
#define      X11_RET1   AS_REGISTER(Register, X11) // volatile
#define      X12_ARG2   AS_REGISTER(Register, X12) // volatile
#define      X13_ARG3   AS_REGISTER(Register, X13) // volatile
#define      X14_ARG4   AS_REGISTER(Register, X14) // volatile
#define      X15_ARG5   AS_REGISTER(Register, X15) // volatile
#define      X16_ARG6   AS_REGISTER(Register, X16) // volatile
#define      X17_ARG7   AS_REGISTER(Register, X17) // volatile
#define      X28_T3     AS_REGISTER(Register, X28) // volatile
#define      X29_T4     AS_REGISTER(Register, X29) // volatile
#define      XJUNK      AS_REGISTER(Register, X30) // volatile
#define      F10_ARG0   AS_REGISTER(FloatRegister, F10)  // volatile
#define      F10_RET0   AS_REGISTER(FloatRegister, F10)  // volatile
#define      F11_ARG1   AS_REGISTER(FloatRegister, F11)  // volatile
#define      F11_RET1   AS_REGISTER(FloatRegister, F11)  // volatile
#define      F12_ARG2   AS_REGISTER(FloatRegister, F12)  // volatile
#define      F13_ARG3   AS_REGISTER(FloatRegister, F13)  // volatile
#define      F14_ARG4   AS_REGISTER(FloatRegister, F14)  // volatile
#define      F15_ARG5   AS_REGISTER(FloatRegister, F15)  // volatile
#define      F16_ARG6   AS_REGISTER(FloatRegister, F16)  // volatile
#define      F17_ARG7   AS_REGISTER(FloatRegister, F17)  // volatile
#endif

// Register declarations to be used in frame manager assembly code.
// Use only non-volatile registers in order to keep values across C-calls.
#ifdef CC_INTERP
REGISTER_DECLARATION(Register, X18_state,      X18);      // address of new cInterpreter.
REGISTER_DECLARATION(Register, X19_prev_state, X19);      // address of old cInterpreter
#else // CC_INTERP
REGISTER_DECLARATION(Register, X18_bcp,        X18);
REGISTER_DECLARATION(Register, X19_esp,        X19);
REGISTER_DECLARATION(FloatRegister, F18_ftos,  F18);
#endif // CC_INTERP
REGISTER_DECLARATION(Register, X20_thread,     X20);      // address of current thread
REGISTER_DECLARATION(Register, X21_tos,        X21);      // address of Java tos (prepushed).
REGISTER_DECLARATION(Register, X22_locals,     X22);      // address of first param slot (receiver).
REGISTER_DECLARATION(Register, X23_method,     X23);      // address of current method
#ifndef DONT_USE_REGISTER_DEFINES
#ifdef CC_INTERP
#define X18_state         AS_REGISTER(Register, X18)
#define X19_prev_state    AS_REGISTER(Register, X19)
#else // CC_INTERP
#define X18_bcp           AS_REGISTER(Register, X18)
#define X19_esp           AS_REGISTER(Register, X19)
#define F18_ftos          AS_REGISTER(FloatRegister, F18)
#endif // CC_INTERP
#define X20_thread        AS_REGISTER(Register, X20)
#define X21_tos           AS_REGISTER(Register, X21)
#define X22_locals        AS_REGISTER(Register, X22)
#define X23_method        AS_REGISTER(Register, X23)
#endif

// Temporary registers to be used within frame manager. We can use
// the non-volatiles because the call stub has saved them.
// Use only non-volatile registers in order to keep values across C-calls.
REGISTER_DECLARATION(Register, X24_tmp1, X24);
REGISTER_DECLARATION(Register, X25_tmp2, X25);
REGISTER_DECLARATION(Register, X26_tmp3, X26);
REGISTER_DECLARATION(Register, X27_tmp4, X27);
#ifndef CC_INTERP
REGISTER_DECLARATION(Register, X24_dispatch_addr,     X24);
REGISTER_DECLARATION(Register, X25_templateTableBase, X25);
REGISTER_DECLARATION(Register, X26_monitor,           X26);
REGISTER_DECLARATION(Register, X27_constPoolCache,    X27);
REGISTER_DECLARATION(Register, X9_mdx,                X9);
REGISTER_DECLARATION(Register, X31_narrowOopBase,     X31);
#endif // CC_INTERP

#ifndef DONT_USE_REGISTER_DEFINES
#define X24_tmp1         AS_REGISTER(Register, X24)
#define X25_tmp2         AS_REGISTER(Register, X25)
#define X26_tmp3         AS_REGISTER(Register, X26)
#define X27_tmp4         AS_REGISTER(Register, X27)
#ifndef CC_INTERP
//    Lmonitors  : monitor pointer
//    LcpoolCache: constant pool cache
//    mdx: method data index
#define X24_dispatch_addr     AS_REGISTER(Register, X24)
#define X25_templateTableBase AS_REGISTER(Register, X25)
#define X26_monitor           AS_REGISTER(Register, X26)
#define X27_constPoolCache    AS_REGISTER(Register, X27)
#define X9_mdx                AS_REGISTER(Register, X9)
#define X31_narrowOopBase     AS_REGISTER(Register, X31)
#endif

#endif

#endif // CPU_RISCV_VM_REGISTER_RISCV_HPP
