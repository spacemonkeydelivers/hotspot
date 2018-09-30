/*
 * Copyright (c) 1999, 2013, Oracle and/or its affiliates. All rights reserved.
 * Copyright (c) 2015, 2016, Loongson Technology. All rights reserved.
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
#include "c1/c1_MacroAssembler.hpp"
#include "c1/c1_Runtime1.hpp"
#include "classfile/systemDictionary.hpp"
#include "gc_interface/collectedHeap.hpp"
#include "interpreter/interpreter.hpp"
#include "oops/arrayOop.hpp"
#include "oops/markOop.hpp"
#include "runtime/basicLock.hpp"
#include "runtime/biasedLocking.hpp"
#include "runtime/os.hpp"
#include "runtime/stubRoutines.hpp"

void C1_MacroAssembler::verified_entry() {
  Unimplemented();
  #if 0
  if (C1Breakpoint)int3();
  // build frame
  verify_FPU(0, "method_entry");
  #endif
}

void C1_MacroAssembler::verify_stack_oop(int stack_offset) {
  Unimplemented();
  #if 0
  if (!VerifyOops) return;
  //  verify_oop_addr(Address(esp, stack_offset));
  verify_oop_addr(Address(SP, stack_offset));
  #endif
}

void C1_MacroAssembler::build_frame(int frame_size_in_bytes, int bang_size_in_bytes) {
  Unimplemented();
  #if 0
  assert(bang_size_in_bytes >= frame_size_in_bytes, "stack bang size incorrect");
  // Make sure there is enough stack space for this method's activation.
  // Note that we do this before doing an enter(). This matches the
  // ordering of C2's stack overflow check / esp decrement and allows
  // the SharedRuntime stack overflow handling to be consistent
  // between the two compilers.
  generate_stack_overflow_check(bang_size_in_bytes);

  push(RA);
  push(FP);
  move(FP, SP);

  decrement(SP, frame_size_in_bytes); // does not emit code for frame_size == 0
  #endif
}

