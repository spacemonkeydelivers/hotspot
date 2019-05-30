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

#include "precompiled.hpp"
#include "asm/assembler.inline.hpp"
#include "interpreter/interpreter.hpp"
#include "interpreter/interpreterRuntime.hpp"
#include "memory/allocation.inline.hpp"
#include "memory/universe.inline.hpp"
#include "oops/method.hpp"
#include "oops/oop.inline.hpp"
#include "runtime/handles.inline.hpp"
#include "runtime/icache.hpp"
#include "runtime/interfaceSupport.hpp"
#include "runtime/signature.hpp"

#define __ _masm->

// Access macros for Java and C arguments.
// The first Java argument is at index -1.
#define locals_j_arg_at(index)    (Interpreter::local_offset_in_bytes(index)), X22_locals
#define locals_j_arg_at_r(index)  X22_locals, (Interpreter::local_offset_in_bytes(index))
// The first C argument is at index 0.
#define sp_c_arg_at(index)        (frame::frame_args_offset + (index)*wordSize), X8_FP

// Implementation of SignatureHandlerGenerator

void InterpreterRuntime::SignatureHandlerGenerator::pass_int() {
  Argument jni_arg(jni_offset());
  Register r = jni_arg.is_register() ? jni_arg.as_register() : X5_T0;

  __ lw(r, locals_j_arg_at(offset())); // sign extension of integer
  if (DEBUG_ONLY(true ||) !jni_arg.is_register()) {
    __ sd(r, sp_c_arg_at(jni_arg.number()));
  }
}

void InterpreterRuntime::SignatureHandlerGenerator::pass_long() {
  Argument jni_arg(jni_offset());
  Register r = jni_arg.is_register() ? jni_arg.as_register() : X5_T0;

  __ ld(r, locals_j_arg_at(offset()+1)); // long resides in upper slot
  if (DEBUG_ONLY(true ||) !jni_arg.is_register()) {
    __ sd(r, sp_c_arg_at(jni_arg.number()));
  }
}

void InterpreterRuntime::SignatureHandlerGenerator::pass_float() {
  // clobbers X5
  FloatRegister fp_reg = (_num_used_fp_arg_regs < 8/*max_fp_register_arguments*/)
                         ? as_FloatRegister((_num_used_fp_arg_regs++) + F10_ARG0->encoding())
                         : F0;

  __ flw(fp_reg, locals_j_arg_at(offset()));
  if (DEBUG_ONLY(true ||) jni_offset() > 8) {
    __ fsw(fp_reg, sp_c_arg_at(jni_offset()));
  }
}

void InterpreterRuntime::SignatureHandlerGenerator::pass_double() {
  // clobbers X5
  FloatRegister fp_reg = (_num_used_fp_arg_regs < 8/*max_fp_register_arguments*/)
                         ? as_FloatRegister((_num_used_fp_arg_regs++) + F10_ARG0->encoding())
                         : F0;

  __ fld(fp_reg, locals_j_arg_at(offset()+1));
  if (DEBUG_ONLY(true ||) jni_offset() > 8) {
    __ fsd(fp_reg, sp_c_arg_at(jni_offset()));
  }
}

void InterpreterRuntime::SignatureHandlerGenerator::pass_object() {
  Argument jni_arg(jni_offset());
  Register r = jni_arg.is_register() ? jni_arg.as_register() : X6_T1;

  // The handle for a receiver will never be null.
  bool do_NULL_check = offset() != 0 || is_static();

  Label do_null;
  if (do_NULL_check) {
    __ ld(X5_T0, locals_j_arg_at(offset()));
    __ addi(r, X0, 0);
    __ beq(X5_T0, X0, do_null);
  }
  __ addi(r, locals_j_arg_at_r(offset()));
  __ bind(do_null);
  if (DEBUG_ONLY(true ||) !jni_arg.is_register()) {
    __ sd(r, sp_c_arg_at(jni_arg.number()));
  }
}

void InterpreterRuntime::SignatureHandlerGenerator::generate(uint64_t fingerprint) {
#if 0
#if !defined(ABI_ELFv2)
  // Emit fd for current codebuffer. Needs patching!
  __ emit_fd();
#endif
#endif // #if 0

  // Generate code to handle arguments.
  iterate(fingerprint);

  // Return the result handler.
  __ load_const(X10_RET0, AbstractInterpreter::result_handler(method()->result_type()));
  __ jr(X1_RA);

  __ flush();
}

#undef __

// Implementation of SignatureHandlerLibrary

void SignatureHandlerLibrary::pd_set_handler(address handler) {
  // Nothing to do on RISC-V.
}

// Code from Tile guys for slow signature handling
class SlowSignatureHandler: public NativeSignatureIterator {
private:
  address   _from;
  intptr_t* _to;
  intptr_t* _to_mem;
  uint      _slots_filled;

  virtual void pass_int() {
    *_to++ = *(jint *)(_from+Interpreter::local_offset_in_bytes(0));
    _from -= Interpreter::stackElementSize;
    // If we have filled the register slots, start filling the mem slots
    if (++_slots_filled == Argument::n_register_parameters)
      _to = _to_mem;
  }

  virtual void pass_object() {
    // pass address of from
    intptr_t *from_addr = (intptr_t*)(_from +
                                      Interpreter::local_offset_in_bytes(0));
    *_to++ = (*from_addr == 0) ? NULL : (intptr_t) from_addr;
    _from -= Interpreter::stackElementSize;
    if (++_slots_filled == Argument::n_register_parameters)
      _to = _to_mem;
  }

#ifdef _LP64
  // Note: pass_float is 64-bit only - otherwise, pass_int is used
  virtual void pass_float() {
    pass_int();
  }

  virtual void pass_long() {
    _to[0] = *(intptr_t*)(_from+Interpreter::local_offset_in_bytes(1));
    _to += 1;
    _from -= 2*Interpreter::stackElementSize;
    if (++_slots_filled == Argument::n_register_parameters)
      _to = _to_mem;
  }

  virtual void pass_double() {
    pass_long();
  }

#else
  // pass_double() is pass_long() and pass_float() is _LP64 only
  virtual void pass_long() {
    // Longs/Doubles must be aligned in even numbered regs or
    // stack slots
    if ((_slots_filled & 1) != 0) {
      _to[0] = 0;
      if (++_slots_filled == Argument::n_register_parameters)
        _to = _to_mem;
      else
        _to++;
    }
    _to[0] = *(intptr_t*)(_from+Interpreter::local_offset_in_bytes(1));
    _to[1] = *(intptr_t*)(_from+Interpreter::local_offset_in_bytes(0));
    _to += 2;
    _from -= 2*Interpreter::stackElementSize();
    if (++_slots_filled == Argument::n_register_parameters)
      _to = _to_mem;
  }
#endif // _LP64

public:
  SlowSignatureHandler(methodHandle method, address from, intptr_t* to_regs,
                       intptr_t* to_mem): NativeSignatureIterator(method) {
    _from = from;
    _to_mem  = to_mem;
    // Skip the first slot, which will be for the thread arg, and if static,
    // the second as well - it will be the class pointer.
    _slots_filled = method->is_static() ? 2 : 1;
    _to = to_regs + _slots_filled;
  }
};

IRT_ENTRY(address, InterpreterRuntime::slow_signature_handler(
            JavaThread* thread,
            Method* method,
            intptr_t* from,
            intptr_t* to ))
  methodHandle m(thread, method);
  assert(m->is_native(), "sanity check");

  { // debug scope
    ResourceMark rm;
    // lookup native function entry point if it doesn't exist
        const char* name = method->name()->as_C_string();
    const char* kl = method->klass_name()->as_C_string();
    printf("INIT_LOG: slow_signature_handler for method: %s of class %s | Thread: %p\n", name, kl, thread);
  }

  // On RISCV, the 'to' vector is in 2 segments: The first for the args that
  // go into registers, and the second for the memory args. The latter is the
  // address of the actual stack locations, and its address is passed in the
  // first entry of the 'to' vector. (We can't change this routine's arg list.)
  intptr_t* to_mem = (intptr_t*) *to;
  SlowSignatureHandler(m, (address)from, to, to_mem).iterate(UCONST64(-1));
  // return result handler
  return Interpreter::result_handler(m->result_type());
IRT_END


// Access function to get the signature.
IRT_ENTRY(address, InterpreterRuntime::get_signature(JavaThread* thread, Method* method))
  methodHandle m(thread, method);
  assert(m->is_native(), "sanity check");
  Symbol *s = m->signature();
  return (address) s->base();
IRT_END

IRT_ENTRY(address, InterpreterRuntime::get_result_handler(JavaThread* thread, Method* method))
  methodHandle m(thread, method);
  assert(m->is_native(), "sanity check");
  return AbstractInterpreter::result_handler(m->result_type());
IRT_END
