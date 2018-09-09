/*
 * Copyright (c) 1997, 2013, Oracle and/or its affiliates. All rights reserved.
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

// make sure the defines don't screw up the declarations later on in this file
#define DONT_USE_REGISTER_DEFINES

#include "precompiled.hpp"
#include "asm/macroAssembler.hpp"
#include "asm/register.hpp"
#include "register_riscv.hpp"
#include "interp_masm_riscv.hpp"

// Integer registers
REGISTER_DEFINITION(Register, noreg);
REGISTER_DEFINITION(Register, X0);
REGISTER_DEFINITION(Register, X1);
REGISTER_DEFINITION(Register, X2);
REGISTER_DEFINITION(Register, X3);
REGISTER_DEFINITION(Register, X4);
REGISTER_DEFINITION(Register, X5);
REGISTER_DEFINITION(Register, X6);
REGISTER_DEFINITION(Register, X7);
REGISTER_DEFINITION(Register, X8);
REGISTER_DEFINITION(Register, X9);
REGISTER_DEFINITION(Register, X10);
REGISTER_DEFINITION(Register, X11);
REGISTER_DEFINITION(Register, X12);
REGISTER_DEFINITION(Register, X13);
REGISTER_DEFINITION(Register, X14);
REGISTER_DEFINITION(Register, X15);
REGISTER_DEFINITION(Register, X16);
REGISTER_DEFINITION(Register, X17);
REGISTER_DEFINITION(Register, X18);
REGISTER_DEFINITION(Register, X19);
REGISTER_DEFINITION(Register, X20);
REGISTER_DEFINITION(Register, X21);
REGISTER_DEFINITION(Register, X22);
REGISTER_DEFINITION(Register, X23);
REGISTER_DEFINITION(Register, X24);
REGISTER_DEFINITION(Register, X25);
REGISTER_DEFINITION(Register, X26);
REGISTER_DEFINITION(Register, X27);
REGISTER_DEFINITION(Register, X28);
REGISTER_DEFINITION(Register, X29);
REGISTER_DEFINITION(Register, X30);
REGISTER_DEFINITION(Register, X31);

// Floating point registers
REGISTER_DEFINITION(FloatRegister, fnoreg);
REGISTER_DEFINITION(FloatRegister, F0);
REGISTER_DEFINITION(FloatRegister, F1);
REGISTER_DEFINITION(FloatRegister, F2);
REGISTER_DEFINITION(FloatRegister, F3);
REGISTER_DEFINITION(FloatRegister, F4);
REGISTER_DEFINITION(FloatRegister, F5);
REGISTER_DEFINITION(FloatRegister, F6);
REGISTER_DEFINITION(FloatRegister, F7);
REGISTER_DEFINITION(FloatRegister, F8);
REGISTER_DEFINITION(FloatRegister, F9);
REGISTER_DEFINITION(FloatRegister, F10);
REGISTER_DEFINITION(FloatRegister, F11);
REGISTER_DEFINITION(FloatRegister, F12);
REGISTER_DEFINITION(FloatRegister, F13);
REGISTER_DEFINITION(FloatRegister, F14);
REGISTER_DEFINITION(FloatRegister, F15);
REGISTER_DEFINITION(FloatRegister, F16);
REGISTER_DEFINITION(FloatRegister, F17);
REGISTER_DEFINITION(FloatRegister, F18);
REGISTER_DEFINITION(FloatRegister, F19);
REGISTER_DEFINITION(FloatRegister, F20);
REGISTER_DEFINITION(FloatRegister, F21);
REGISTER_DEFINITION(FloatRegister, F22);
REGISTER_DEFINITION(FloatRegister, F23);
REGISTER_DEFINITION(FloatRegister, F24);
REGISTER_DEFINITION(FloatRegister, F25);
REGISTER_DEFINITION(FloatRegister, F26);
REGISTER_DEFINITION(FloatRegister, F27);
REGISTER_DEFINITION(FloatRegister, F28);
REGISTER_DEFINITION(FloatRegister, F29);
REGISTER_DEFINITION(FloatRegister, F30);
REGISTER_DEFINITION(FloatRegister, F31);

