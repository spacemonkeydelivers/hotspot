/*
 * Copyright (c) 2012, 2013, Oracle and/or its affiliates. All rights reserved.
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

#ifndef SHARE_VM_ASM_MACROASSEMBLER_INLINE_HPP
#define SHARE_VM_ASM_MACROASSEMBLER_INLINE_HPP

#include "asm/macroAssembler.hpp"

#ifdef TARGET_ARCH_x86
// no macroAssembler_x86.inline.hpp
#endif
#ifdef TARGET_ARCH_sparc
# include "macroAssembler_sparc.inline.hpp"
#endif
#ifdef TARGET_ARCH_zero
# include "assembler_zero.inline.hpp"
#endif
#ifdef TARGET_ARCH_arm
# include "macroAssembler_arm.inline.hpp"
#endif
#ifdef TARGET_ARCH_ppc
# include "macroAssembler_ppc.inline.hpp"
#endif
#ifdef TARGET_ARCH_riscv
# include "macroAssembler_riscv.inline.hpp"
#endif

#endif // SHARE_VM_ASM_MACROASSEMBLER_INLINE_HPP
