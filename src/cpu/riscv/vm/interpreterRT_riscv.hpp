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

#ifndef CPU_RISCV_VM_INTERPRETERRT_RISCV_HPP
#define CPU_RISCV_VM_INTERPRETERRT_RISCV_HPP

#include "memory/allocation.hpp"

// native method calls

class SignatureHandlerGenerator: public NativeSignatureIterator {
 private:
  MacroAssembler* _masm;
  // number of already used floating-point argument registers
  int _num_used_fp_arg_regs;

  void pass_int();
  void pass_long();
  void pass_double();
  void pass_float();
  void pass_object();

 public:
  // Creation
  SignatureHandlerGenerator(methodHandle method, CodeBuffer* buffer) : NativeSignatureIterator(method) {
    _masm = new MacroAssembler(buffer);
    _num_used_fp_arg_regs = 0;
  }

  // Code generation
  void generate(uint64_t fingerprint);
};

// Support for generate_slow_signature_handler.
static address get_result_handler(JavaThread* thread, Method* method);

// A function to get the signature.
static address get_signature(JavaThread* thread, Method* method);

#endif // CPU_RISCV_VM_INTERPRETERRT_RISCV_HPP
