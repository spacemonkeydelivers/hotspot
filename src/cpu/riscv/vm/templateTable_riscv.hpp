/*
 * Copyright (c) 2014, Oracle and/or its affiliates. All rights reserved.
 * Copyright 2013, 2014 SAP AG. All rights reserved.
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

#ifndef CPU_RISCV_VM_TEMPLATETABLE_RISCV_64_HPP
#define CPU_RISCV_VM_TEMPLATETABLE_RISCV_64_HPP

  static void prepare_invoke(int byte_no, Register Rmethod, Register Rret_addr, Register Rindex, Register Rrecv, Register Rflags, Register Rscratch);
  static void invokevfinal_helper(Register Rmethod, Register Rflags, Register Rscratch1, Register Rscratch2);
  static void generate_vtable_call(Register Rrecv_klass, Register Rindex, Register Rret, Register Rtemp);
  static void invokeinterface_object_method(Register Rrecv_klass, Register Rret, Register Rflags, Register Rindex, Register Rtemp, Register Rtemp2);

  static void if_cmp_common(Register Rfirst, Register Rsecond, Condition cc);

#endif // CPU_RISCV_VM_TEMPLATETABLE_RISCV_64_HPP
