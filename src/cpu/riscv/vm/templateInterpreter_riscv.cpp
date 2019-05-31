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

#include "precompiled.hpp"
#ifndef CC_INTERP
#include "asm/macroAssembler.inline.hpp"
#include "interpreter/bytecodeHistogram.hpp"
#include "interpreter/interpreter.hpp"
#include "interpreter/interpreterGenerator.hpp"
#include "interpreter/interpreterRuntime.hpp"
#include "interpreter/templateTable.hpp"
#include "oops/arrayOop.hpp"
#include "oops/methodData.hpp"
#include "oops/method.hpp"
#include "oops/oop.inline.hpp"
#include "prims/jvmtiExport.hpp"
#include "prims/jvmtiThreadState.hpp"
#include "runtime/arguments.hpp"
#include "runtime/deoptimization.hpp"
#include "runtime/frame.inline.hpp"
#include "runtime/sharedRuntime.hpp"
#include "runtime/stubRoutines.hpp"
#include "runtime/synchronizer.hpp"
#include "runtime/timer.hpp"
#include "runtime/vframeArray.hpp"
#include "utilities/debug.hpp"
#include "utilities/macros.hpp"

#undef __
#define __ _masm->

#ifdef PRODUCT
#define BLOCK_COMMENT(str) /* nothing */
#else
#define BLOCK_COMMENT(str) __ block_comment(str)
#endif

#define BIND(label) bind(label); BLOCK_COMMENT(#label ":")

//-----------------------------------------------------------------------------

// Actually we should never reach here since we do stack overflow checks before pushing any frame.
address TemplateInterpreterGenerator::generate_StackOverflowError_handler() {
  address entry = __ pc();
  __ unimplemented("generate_StackOverflowError_handler");
  return entry;
}

address TemplateInterpreterGenerator::generate_ArrayIndexOutOfBounds_handler(const char* name) {
  __ unimplemented("array index out of bounds handler");
  return __ pc();
  /*
  address entry = __ pc();
  __ empty_expression_stack();
  __ load_const_optimized(R4_ARG2, (address) name);
  // Index is in R17_tos.
  __ mr(R5_ARG3, R17_tos);
  __ call_VM(noreg, CAST_FROM_FN_PTR(address, InterpreterRuntime::throw_ArrayIndexOutOfBoundsException));
  return entry;
  */
}

#if 0
// Call special ClassCastException constructor taking object to cast
// and target class as arguments.
address TemplateInterpreterGenerator::generate_ClassCastException_verbose_handler() {
  ShouldNotReachHere();
  /*
  address entry = __ pc();

  // Expression stack must be empty before entering the VM if an
  // exception happened.
  __ empty_expression_stack();

  // Thread will be loaded to R3_ARG1.
  // Target class oop is in register R5_ARG3 by convention!
  __ call_VM(noreg, CAST_FROM_FN_PTR(address, InterpreterRuntime::throw_ClassCastException_verbose), R17_tos, R5_ARG3);
  // Above call must not return here since exception pending.
  DEBUG_ONLY(__ should_not_reach_here();)
  return entry;
  */
}
#endif

address TemplateInterpreterGenerator::generate_ClassCastException_handler() {
  address entry = __ pc();
  __ unimplemented("generate_ClassCastException_handler");
  /*
  // Expression stack must be empty before entering the VM if an
  // exception happened.
  __ empty_expression_stack();

  // Load exception object.
  // Thread will be loaded to R3_ARG1.
  __ call_VM(noreg, CAST_FROM_FN_PTR(address, InterpreterRuntime::throw_ClassCastException), R17_tos);
#ifdef ASSERT
  // Above call must not return here since exception pending.
  __ should_not_reach_here();
#endif
  */
  return entry;
}

address TemplateInterpreterGenerator::generate_exception_handler_common(const char* name, const char* message, bool pass_oop) {
  address entry = __ pc();
  __ unimplemented("generate_exception_handler_common");
  /*
  Register Rexception = R17_tos;

  // Expression stack must be empty before entering the VM if an exception happened.
  __ empty_expression_stack();

  __ load_const_optimized(R4_ARG2, (address) name, R11_scratch1);
  if (pass_oop) {
    __ mr(R5_ARG3, Rexception);
    __ call_VM(Rexception, CAST_FROM_FN_PTR(address, InterpreterRuntime::create_klass_exception), false);
  } else {
    __ load_const_optimized(R5_ARG3, (address) message, R11_scratch1);
    __ call_VM(Rexception, CAST_FROM_FN_PTR(address, InterpreterRuntime::create_exception), false);
  }

  // Throw exception.
  __ mr(R3_ARG1, Rexception);
  __ load_const_optimized(R11_scratch1, Interpreter::throw_exception_entry(), R12_scratch2);
  __ mtctr(R11_scratch1);
  __ bctr();

  */
  return entry;
}

address TemplateInterpreterGenerator::generate_continuation_for(TosState state) {
  address entry = __ pc();
  __ unimplemented("generate_continuation_for");
  return entry;
}

// This entry is returned to when a call returns to the interpreter.
// When we arrive here, we expect that the callee stack frame is already popped.
address TemplateInterpreterGenerator::generate_return_entry_for(TosState state, int step, size_t index_size) {
  address entry = __ pc();

  __ unimplemented("generate_return_entry_for");
  /*
  // Move the value out of the return register back to the TOS cache of current frame.
  switch (state) {
    case ltos:
    case btos:
    case ctos:
    case stos:
    case atos:
    case itos: __ mr(X21_tos, X10_RET0); break;   // RET -> TOS cache
    case ftos:
    case dtos: __ fmr(F18_ftos, F10_RET0); break; // TOS cache -> GR_FRET
    case vtos: break;                           // Nothing to do, this was a void return.
    default  : ShouldNotReachHere();
  }

  __ restore_interpreter_state(R11_scratch1); // Sets R11_scratch1 = fp.
  __ ld(R12_scratch2, _ijava_state_neg(top_frame_sp), R11_scratch1);
  __ resize_frame_absolute(R12_scratch2, R11_scratch1, R0);

  // Compiled code destroys templateTableBase, reload.
  __ load_const(X25_templateTableBase, (address)Interpreter::dispatch_table((TosState)0));

  if (state == atos) {
    __ profile_return_type(R3_RET, R11_scratch1, R12_scratch2);
  }

  const Register cache = R11_scratch1;
  const Register size  = R12_scratch2;
  __ get_cache_and_index_at_bcp(cache, 1, index_size);

  // Get least significant byte of 64 bit value:
  __ lbu(size, in_bytes(ConstantPoolCache::base_offset() + ConstantPoolCacheEntry::flags_offset()), cache);
  __ slli(size, size, Interpreter::logStackElementSize);
  __ add(X19_esp, X19_esp, size);
  __ dispatch_next(state, step);
  */
  return entry;
}

address TemplateInterpreterGenerator::generate_deopt_entry_for(TosState state, int step) {
  address entry = __ pc();
  __ unimplemented("generate_deopt_entry_for");
  /*
  // If state != vtos, we're returning from a native method, which put it's result
  // into the result register. So move the value out of the return register back
  // to the TOS cache of current frame.

  switch (state) {
    case ltos:
    case btos:
    case ctos:
    case stos:
    case atos:
    case itos: __ mr(R17_tos, R3_RET); break;   // GR_RET -> TOS cache
    case ftos:
    case dtos: __ fmr(F15_ftos, F1_RET); break; // TOS cache -> GR_FRET
    case vtos: break;                           // Nothing to do, this was a void return.
    default  : ShouldNotReachHere();
  }

  // Load LcpoolCache @@@ should be already set!
  __ get_constant_pool_cache(R27_constPoolCache);

  // Handle a pending exception, fall through if none.
  __ check_and_forward_exception(R11_scratch1, R12_scratch2);

  // Start executing bytecodes.
  __ dispatch_next(state, step);
  */
  return entry;
}

// A result handler converts the native result into java format.
// Use the shared code between c++ and template interpreter.
address TemplateInterpreterGenerator::generate_result_handler_for(BasicType type) {
  return AbstractInterpreterGenerator::generate_result_handler_for(type);
}

address TemplateInterpreterGenerator::generate_safept_entry_for(TosState state, address runtime_entry) {
  address entry = __ pc();

  __ unimplemented("generate_safept_entry_for");
  /*
  __ push(state);
  __ call_VM(noreg, runtime_entry);
  __ dispatch_via(vtos, Interpreter::_normal_table.table_for(vtos));
  */

  return entry;
}

// Helpers for commoning out cases in the various type of method entries.

// Increment invocation count & check for overflow.
//
// Note: checking for negative value instead of overflow
//       so we have a 'sticky' overflow test.
//
void TemplateInterpreterGenerator::generate_counter_incr(Label* overflow, Label* profile_method, Label* profile_method_continue) {
  ShouldNotReachHere();
  /*
  // Note: In tiered we increment either counters in method or in MDO depending if we're profiling or not.
  Register Rscratch1   = R11_scratch1;
  Register Rscratch2   = R12_scratch2;
  Register R3_counters = R3_ARG1;
  Label done;

  if (TieredCompilation) {
    const int increment = InvocationCounter::count_increment;
    const int mask = ((1 << Tier0InvokeNotifyFreqLog) - 1) << InvocationCounter::count_shift;
    Label no_mdo;
    if (ProfileInterpreter) {
      const Register Rmdo = Rscratch1;
      // If no method data exists, go to profile_continue.
      __ ld(Rmdo, in_bytes(Method::method_data_offset()), R19_method);
      __ cmpdi(CCR0, Rmdo, 0);
      __ beq(CCR0, no_mdo);

      // Increment backedge counter in the MDO.
      const int mdo_bc_offs = in_bytes(MethodData::backedge_counter_offset()) + in_bytes(InvocationCounter::counter_offset());
      __ lwz(Rscratch2, mdo_bc_offs, Rmdo);
      __ addi(Rscratch2, Rscratch2, increment);
      __ stw(Rscratch2, mdo_bc_offs, Rmdo);
      __ load_const_optimized(Rscratch1, mask, R0);
      __ and_(Rscratch1, Rscratch2, Rscratch1);
      __ bne(CCR0, done);
      __ b(*overflow);
    }

    // Increment counter in MethodCounters*.
    const int mo_bc_offs = in_bytes(MethodCounters::backedge_counter_offset()) + in_bytes(InvocationCounter::counter_offset());
    __ bind(no_mdo);
    __ get_method_counters(R19_method, R3_counters, done);
    __ lwz(Rscratch2, mo_bc_offs, R3_counters);
    __ addi(Rscratch2, Rscratch2, increment);
    __ stw(Rscratch2, mo_bc_offs, R3_counters);
    __ load_const_optimized(Rscratch1, mask, R0);
    __ and_(Rscratch1, Rscratch2, Rscratch1);
    __ beq(CCR0, *overflow);

    __ bind(done);

  } else {

    // Update standard invocation counters.
    Register Rsum_ivc_bec = R4_ARG2;
    __ get_method_counters(R19_method, R3_counters, done);
    __ increment_invocation_counter(R3_counters, Rsum_ivc_bec, R12_scratch2);
    // Increment interpreter invocation counter.
    if (ProfileInterpreter) {  // %%% Merge this into methodDataOop.
      __ lwz(R12_scratch2, in_bytes(MethodCounters::interpreter_invocation_counter_offset()), R3_counters);
      __ addi(R12_scratch2, R12_scratch2, 1);
      __ stw(R12_scratch2, in_bytes(MethodCounters::interpreter_invocation_counter_offset()), R3_counters);
    }
    // Check if we must create a method data obj.
    if (ProfileInterpreter && profile_method != NULL) {
      const Register profile_limit = Rscratch1;
      int pl_offs = __ load_const_optimized(profile_limit, &InvocationCounter::InterpreterProfileLimit, R0, true);
      __ lwz(profile_limit, pl_offs, profile_limit);
      // Test to see if we should create a method data oop.
      __ cmpw(CCR0, Rsum_ivc_bec, profile_limit);
      __ blt(CCR0, *profile_method_continue);
      // If no method data exists, go to profile_method.
      __ test_method_data_pointer(*profile_method);
    }
    // Finally check for counter overflow.
    if (overflow) {
      const Register invocation_limit = Rscratch1;
      int il_offs = __ load_const_optimized(invocation_limit, &InvocationCounter::InterpreterInvocationLimit, R0, true);
      __ lwz(invocation_limit, il_offs, invocation_limit);
      assert(4 == sizeof(InvocationCounter::InterpreterInvocationLimit), "unexpected field size");
      __ cmpw(CCR0, Rsum_ivc_bec, invocation_limit);
      __ bge(CCR0, *overflow);
    }

    __ bind(done);
  }
  */
}

// Generate code to initiate compilation on invocation counter overflow.
void TemplateInterpreterGenerator::generate_counter_overflow(Label& continue_entry) {
  ShouldNotReachHere();
  /*
  // Generate code to initiate compilation on the counter overflow.

  // InterpreterRuntime::frequency_counter_overflow takes one arguments,
  // which indicates if the counter overflow occurs at a backwards branch (NULL bcp)
  // We pass zero in.
  // The call returns the address of the verified entry point for the method or NULL
  // if the compilation did not complete (either went background or bailed out).
  //
  // Unlike the C++ interpreter above: Check exceptions!
  // Assumption: Caller must set the flag "do_not_unlock_if_sychronized" if the monitor of a sync'ed
  // method has not yet been created. Thus, no unlocking of a non-existing monitor can occur.

  __ li(R4_ARG2, 0);
  __ call_VM(noreg, CAST_FROM_FN_PTR(address, InterpreterRuntime::frequency_counter_overflow), R4_ARG2, true);

  // Returns verified_entry_point or NULL.
  // We ignore it in any case.
  __ b(continue_entry);
  */
}

void TemplateInterpreterGenerator::generate_stack_overflow_check(Register Rmem_frame_size, Register Rscratch1) {
  assert_different_registers(Rmem_frame_size, Rscratch1);
  __ generate_stack_overflow_check_with_compare_and_throw(Rmem_frame_size, Rscratch1);
}

void TemplateInterpreterGenerator::unlock_method(bool check_exceptions) {
  ShouldNotReachHere();
  /*
  __ unlock_object(R26_monitor, check_exceptions);
  */
}

// Lock the current method, interpreter register window must be set up!
void TemplateInterpreterGenerator::lock_method(Register Rflags, Register Rscratch1, Register Rscratch2, bool flags_preloaded) {
  ShouldNotReachHere();
  /*
  const Register Robj_to_lock = Rscratch2;

  {
    if (!flags_preloaded) {
      __ lwz(Rflags, method_(access_flags));
    }

#ifdef ASSERT
    // Check if methods needs synchronization.
    {
      Label Lok;
      __ testbitdi(CCR0, R0, Rflags, JVM_ACC_SYNCHRONIZED_BIT);
      __ btrue(CCR0,Lok);
      __ stop("method doesn't need synchronization");
      __ bind(Lok);
    }
#endif // ASSERT
  }

  // Get synchronization object to Rscratch2.
  {
    const int mirror_offset = in_bytes(Klass::java_mirror_offset());
    Label Lstatic;
    Label Ldone;

    __ testbitdi(CCR0, R0, Rflags, JVM_ACC_STATIC_BIT);
    __ btrue(CCR0, Lstatic);

    // Non-static case: load receiver obj from stack and we're done.
    __ ld(Robj_to_lock, R18_locals);
    __ b(Ldone);

    __ bind(Lstatic); // Static case: Lock the java mirror
    __ ld(Robj_to_lock, in_bytes(Method::const_offset()), R19_method);
    __ ld(Robj_to_lock, in_bytes(ConstMethod::constants_offset()), Robj_to_lock);
    __ ld(Robj_to_lock, ConstantPool::pool_holder_offset_in_bytes(), Robj_to_lock);
    __ ld(Robj_to_lock, mirror_offset, Robj_to_lock);

    __ bind(Ldone);
    __ verify_oop(Robj_to_lock);
  }

  // Got the oop to lock => execute!
  __ add_monitor_to_stack(true, Rscratch1, R0);

  __ std(Robj_to_lock, BasicObjectLock::obj_offset_in_bytes(), R26_monitor);
  __ lock_object(R26_monitor, Robj_to_lock);
  */
}

//
// Generate a fixed interpreter frame

void TemplateInterpreterGenerator::generate_fixed_frame(Register Rsize_of_parameters, Register Rsize_of_locals) {
  Label Lno_resize;

  Register Rparent_frame_resize = X13_ARG3, // Frame will grow by this number of bytes.
           Rtop_frame_size      = X14_ARG4,
           Rconst_method        = X15_ARG5,
           Rtotal_size          = X16_ARG6,
           Rsaved_SP            = X16_ARG6,
           Rnew_locals          = X16_ARG6;

  assert_different_registers(Rsize_of_parameters, Rsize_of_locals, Rparent_frame_resize, Rtop_frame_size);

  __ ld(Rconst_method, method_(const));
  __ lhu(Rsize_of_parameters /* number of params */, in_bytes(ConstMethod::size_of_parameters_offset()), Rconst_method);
  __ lhu(    Rsize_of_locals /* number of locals */, in_bytes(ConstMethod::size_of_locals_offset()),     Rconst_method);
  __ slli(Rsize_of_parameters, Rsize_of_parameters, Interpreter::logStackElementSize);
  __ slli(    Rsize_of_locals,     Rsize_of_locals, Interpreter::logStackElementSize);
  __ lhu(Rtop_frame_size, in_bytes(ConstMethod::max_stack_offset()), Rconst_method);
  __ slli(Rtop_frame_size, Rtop_frame_size, Interpreter::logStackElementSize);
  __ sub(X6_T1, Rsize_of_locals, Rsize_of_parameters); // >=0
  __ sub(Rparent_frame_resize, X2_SP, X19_esp); // <0, off by Interpreter::stackElementSize!
  __ add(Rparent_frame_resize, Rparent_frame_resize, X6_T1);

  // Check for SP extension (clear if negative)
  __ bge(Rparent_frame_resize, XZERO, Lno_resize);
  __ addi(Rparent_frame_resize, XZERO, 0);
  __ bind(Lno_resize);
  
  // Ensure stack alignment (may waste up to 16 bytes)
  __ addi(Rparent_frame_resize, Rparent_frame_resize, 15);
  __ andi(Rparent_frame_resize, Rparent_frame_resize, -16);
  __ addi(Rtop_frame_size, Rtop_frame_size, 15);
  __ andi(Rtop_frame_size, Rtop_frame_size, -16);

  // Compute full size
  __ add( Rtotal_size, Rparent_frame_resize, Rtop_frame_size);
  __ addi(Rtotal_size, Rtotal_size, -frame::interpreter_frame_local_words_base_offset);
    
  generate_stack_overflow_check(Rtotal_size, X6_T1);

  // Save the old SP before adapting parent frame
  __ addi(Rsaved_SP, X2_SP, 0);

  // Resize parent frame
  __ sub(X2_SP, X2_SP, Rparent_frame_resize);

  // Store old stack registers
  __ sd(X1_RA, frame::frame_ra_index * frame::frame_elem_size, X2_SP);
  __ sd(X8_FP, frame::frame_fp_index * frame::frame_elem_size, X2_SP);

  // Store old SP of parent frame
  __ sd(Rsaved_SP, frame::interpreter_frame_saved_SP_index * frame::frame_elem_size, X2_SP);

  // Get new locals pointer
  __ add(Rnew_locals, X19_esp, Rsize_of_parameters);

  // Compute new state registers and extend stack
  __ add(X8_FP, X2_SP, XZERO);
  __ add(X22_locals, Rnew_locals, XZERO);
  __ addi(X2_SP, X2_SP, frame::interpreter_frame_local_words_base_offset*frame::frame_elem_size);
  __ add(X19_esp, X2_SP, XZERO);
  __ add(X26_monitor, X2_SP, XZERO);
  __ ld(X27_constPoolCache, in_bytes(ConstMethod::constants_offset()), Rconst_method);
  __ ld(X27_constPoolCache, ConstantPool::cache_offset_in_bytes(), X27_constPoolCache);
  __ addi(X18_bcp, Rconst_method, in_bytes(ConstMethod::codes_offset()));
  // TODO: setup Rmethod

  // Do final resize
  __ sub(X2_SP, X2_SP, Rtop_frame_size);

  // Setup method data pointer
  if (ProfileInterpreter) {
    Label zero_continue;
    __ ld(X9_mdx, method_(method_data));
    __ beq(X9_mdx, XZERO, zero_continue);
    __ addi(X9_mdx, X9_mdx, in_bytes(MethodData::data_offset()));
    __ bind(zero_continue);
  }
}

// End of helpers

// ============================================================================
// Various method entries
//

// Empty method, generate a very fast return. We must skip this entry if
// someone's debugging, indicated by the flag
// "interp_mode" in the Thread obj.
// Note: empty methods are generated mostly methods that do assertions, which are
// disabled in the "java opt build".
address TemplateInterpreterGenerator::generate_empty_entry(void) {
  address entry = __ pc();
  __ unimplemented(__func__);
  return entry;
  /*
  if (!UseFastEmptyMethods) {
    NOT_PRODUCT(__ should_not_reach_here();)
    return Interpreter::entry_for_kind(Interpreter::zerolocals);
  }

  Label Lslow_path;
  const Register Rjvmti_mode = R11_scratch1;
  address entry = __ pc();

  __ lwz(Rjvmti_mode, thread_(interp_only_mode));
  __ cmpwi(CCR0, Rjvmti_mode, 0);
  __ bne(CCR0, Lslow_path); // jvmti_mode!=0

  // Noone's debuggin: Simply return.
  // Pop c2i arguments (if any) off when we return.
#ifdef ASSERT
    __ ld(R9_ARG7, 0, R1_SP);
    __ ld(R10_ARG8, 0, R21_sender_SP);
    __ cmpd(CCR0, R9_ARG7, R10_ARG8);
    __ asm_assert_eq("backlink", 0x545);
#endif // ASSERT
  __ mr(R1_SP, R21_sender_SP); // Cut the stack back to where the caller started.

  // And we're done.
  __ blr();

  __ bind(Lslow_path);
  __ branch_to_entry(Interpreter::entry_for_kind(Interpreter::zerolocals), R11_scratch1);
  __ flush();

  return entry;
  */
}

// Support abs and sqrt like in compiler.
// For others we can use a normal (native) entry.

inline bool math_entry_available(AbstractInterpreter::MethodKind kind) {
  ShouldNotReachHere();
  return false;
  /*
  // Provide math entry with debugging on demand.
  // Note: Debugging changes which code will get executed:
  // Debugging or disabled InlineIntrinsics: java method will get interpreted and performs a native call.
  // Not debugging and enabled InlineIntrinics: processor instruction will get used.
  // Result might differ slightly due to rounding etc.
  if (!InlineIntrinsics && (!FLAG_IS_ERGO(InlineIntrinsics))) return false; // Generate a vanilla entry.

  return ((kind==Interpreter::java_lang_math_sqrt && VM_Version::has_fsqrt()) ||
          (kind==Interpreter::java_lang_math_abs));
  */
}

address TemplateInterpreterGenerator::generate_math_entry(AbstractInterpreter::MethodKind kind) {
  address entry = __ pc();
  __ unimplemented(__func__);
  return entry;
  /*
  if (!math_entry_available(kind)) {
    NOT_PRODUCT(__ should_not_reach_here();)
    return Interpreter::entry_for_kind(Interpreter::zerolocals);
  }

  Label Lslow_path;
  const Register Rjvmti_mode = R11_scratch1;
  address entry = __ pc();

  // Provide math entry with debugging on demand.
  __ lwz(Rjvmti_mode, thread_(interp_only_mode));
  __ cmpwi(CCR0, Rjvmti_mode, 0);
  __ bne(CCR0, Lslow_path); // jvmti_mode!=0

  __ lfd(F1_RET, Interpreter::stackElementSize, R15_esp);

  // Pop c2i arguments (if any) off when we return.
#ifdef ASSERT
  __ ld(R9_ARG7, 0, R1_SP);
  __ ld(R10_ARG8, 0, R21_sender_SP);
  __ cmpd(CCR0, R9_ARG7, R10_ARG8);
  __ asm_assert_eq("backlink", 0x545);
#endif // ASSERT
  __ mr(R1_SP, R21_sender_SP); // Cut the stack back to where the caller started.

  if (kind == Interpreter::java_lang_math_sqrt) {
    __ fsqrt(F1_RET, F1_RET);
  } else if (kind == Interpreter::java_lang_math_abs) {
    __ fabs(F1_RET, F1_RET);
  } else {
    ShouldNotReachHere();
  }

  // And we're done.
  __ blr();

  // Provide slow path for JVMTI case.
  __ bind(Lslow_path);
  __ branch_to_entry(Interpreter::entry_for_kind(Interpreter::zerolocals), R12_scratch2);
  __ flush();

  return entry;
  */
}

// Interpreter stub for calling a native method. (asm interpreter)
// This sets up a somewhat different looking stack for calling the
// native method than the typical interpreter frame setup.
//
// On entry:
//  X18_bcp,            frame::interpreter_frame_Rbcp_index       *frame::frame_elem_size, X8_FP);             
//  X23_method,         frame::interpreter_frame_Rmethod_index    *frame::frame_elem_size, X8_FP);             
//  X22_locals,         frame::interpreter_frame_Rlocals_index    *frame::frame_elem_size, X8_FP);             
//  X26_monitor,        frame::interpreter_frame_Rmonitors_index  *frame::frame_elem_size, X8_FP);             
//  X27_constPoolCache, frame::interpreter_frame_RcpoolCache_index*frame::frame_elem_size, X8_FP);             
//  X9_mdx,             interpreter_frame_Rmdx_index       *frame::frame_elem_size, X8_FP);             
//  X19_esp,            
//  X20 - thread ???
//
// Interpreter stub for calling a native method. (asm interpreter)
// This sets up a somewhat different looking stack for calling the
// native method than the typical interpreter frame setup.
address TemplateInterpreterGenerator::generate_native_entry(bool synchronized) {

  printf("INIT_LOG: call generate_native_entry, synchronized = %d\n", synchronized);

  // determine code generation flags
  bool inc_counter  = UseCompiler || CountCompiledCalls;
  // Rsender: sender's sp
  // Rmethod: Method*
  address entry_point = __ pc();

#if 0
#ifndef CORE
  const Address invocation_counter(Rmethod,in_bytes(MethodCounters::invocation_counter_offset() +   // Fu: 20130814
  InvocationCounter::counter_offset()));
#endif
#endif

  // get parameter size (always needed)
  // the size in the java stack
  Register NUM_ARGS = X5_T0;
  Register JAVA_FP = X6_T1;
  Register R_thread = X20;
  Register TMP0 = X7_T2;
  Register TMP1 = X28_T3;

  __ dbgtrace_gencode_post(R_thread, TMP0, "%s: %s pc 0x%llx \n",
                           __PRETTY_FUNCTION__, "generate_native_entry", entry_point);

  __ ld(NUM_ARGS, X23_method, in_bytes(Method::const_offset()));
  __ lhu(NUM_ARGS, NUM_ARGS, in_bytes(ConstMethod::size_of_parameters_offset()));   // Fu: 20130814

  // native calls don't need the stack size check since they have no expression stack
  // and the arguments are already on the stack and we only add a handful of words
  // to the stack

  // Rmethod: Method*
  // V0: size of parameters
  // Layout of frame at this point
  //
  // [ argument word n-1  ] <--- sp
  //   ...
  // [ argument word 0    ]

  // for natives the size of locals is zero

  // compute beginning of parameters (S7)
  __ slliw(JAVA_FP, NUM_ARGS, Address::times_8);
  __ addi(JAVA_FP, JAVA_FP, (-1) * wordSize);
  __ add(JAVA_FP, JAVA_FP, X2_SP);


  // add 2 zero-initialized slots for native calls
  __ addi(X2_SP, X2_SP, (-2) * wordSize);
  __ sd(XZERO, 1 * wordSize, X2_SP);  // slot for native oop temp offset (setup via runtime)
  __ sd(XZERO, 0 * wordSize, X2_SP);  // slot for static native result handler3 (setup via runtime)

  // Layout of frame at this point
  // [ method holder mirror  ] <--- sp
  // [ result type info      ]
  // [ argument word n-1     ] <--- T0
  //   ...
  // [ argument word 0       ] <--- LVP

#if 0
#ifndef CORE
  if (inc_counter) __ lw(T3, invocation_counter);  // (pre-)fetch invocation count
#endif
#endif

  // initialize fixed part of activation frame
  generate_fixed_frame(TMP0, TMP1);
  // after this function, the layout of frame is as following
  //
  // [ monitor block top        ] <--- sp ( the top monitor entry )
  // [ byte code pointer (0)    ] (if native, bcp = 0)
  // [ constant pool cache      ]
  // [ Method*                  ]
  // [ locals offset            ]
  // [ sender's sp              ]
  // [ sender's fp              ]
  // [ return address           ] <--- fp
  // [ method holder mirror     ]
  // [ result type info         ]
  // [ argumnet word n-1        ] <--- sender's sp
  //   ...
  // [ argument word 0          ] <--- S7


  // make sure method is native & not abstract
#if 0
#ifdef ASSERT
  __ lw(T0, Rmethod, in_bytes(Method::access_flags_offset()));
  {
    Label L;
    __ andi(AT, T0, JVM_ACC_NATIVE);
    __ bne(AT, R0, L);
    __ delayed()->nop();
    __ stop("tried to execute native method as non-native");
    __ bind(L);
  }
  {
    Label L;
    __ andi(AT, T0, JVM_ACC_ABSTRACT);
    __ beq(AT, R0, L);
    __ delayed()->nop();
    __ stop("tried to execute abstract method in interpreter");
    __ bind(L);
  }
#endif
#endif // #if 0

  // Since at this point in the method invocation the exception handler
  // would try to exit the monitor of synchronized methods which hasn't
  // been entered yet, we set the thread local variable
  // _do_not_unlock_if_synchronized to true. The remove_activation will
  // check this flag.
  __ addi(TMP0, XZERO, (int)true);
  __ sb(TMP0, in_bytes(JavaThread::do_not_unlock_if_synchronized_offset()), R_thread);

#if 0
#ifndef CORE
  // increment invocation count & check for overflow
  Label invocation_counter_overflow;
  if (inc_counter) {
    generate_counter_incr(&invocation_counter_overflow, NULL, NULL);
  }

  Label continue_after_compile;
  __ bind(continue_after_compile);
#endif // CORE
#endif // #if 0

  // TODO: implement this method
  // bang_stack_shadow_pages(true);

  __ sb(XZERO, in_bytes(JavaThread::do_not_unlock_if_synchronized_offset()), R_thread);

  // check for synchronized methods
  // Must happen AFTER invocation_counter check and stack overflow check,
  // so method is not locked if overflows.
  if (synchronized) {
    // TODO: implement this method
    // lock_method(XZERO, TMP0, TMP1, false);
  } else {
    // no synchronization necessary
#if 0
#ifdef ASSERT
    {
      Label L;
      __ lw(T0, Rmethod, in_bytes(Method::access_flags_offset()));
      __ andi(AT, T0, JVM_ACC_SYNCHRONIZED);
      __ beq(AT, R0, L);
      __ delayed()->nop();
      __ stop("method needs synchronization");
      __ bind(L);
    }
#endif
#endif // #if 0
  }

  // after method_lock, the layout of frame is as following
  //
  // [ monitor entry            ] <--- sp
  //   ...
  // [ monitor entry            ]
  // [ monitor block top        ] ( the top monitor entry )
  // [ byte code pointer (0)    ] (if native, bcp = 0)
  // [ constant pool cache      ]
  // [ Method*                  ]
  // [ locals offset            ]
  // [ sender's sp              ]
  // [ sender's fp              ]
  // [ return address           ] <--- fp
  // [ method holder mirror     ]
  // [ result type info         ]
  // [ argumnet word n-1        ] <--- ( sender's sp )
  //   ...
  // [ argument word 0          ] <--- S7

  // start execution
#if 0
#ifdef ASSERT
  {
    Label L;
    __ ld(TMP0, X8_FP, frame::interpreter_frame_monitor_block_top_offset * wordSize);
    __ beq(TMP0, X2_SP, L);
    __ stop("broken stack frame setup in interpreter in asm");
    __ bind(L);
  }
#endif
#endif // #if 0

  // jvmti/jvmpi support
  // TODO: implement this method
  //__ notify_method_entry();

  // work registers
  const Register R_method = X23_method;
  //const Register thread = T2;
  const Register t      = TMP0;

  // TODO: we may want to uncomment this if get_method is required elsewhere
  // NOTE: get_method is not used at the moment
  // __ get_method(R_method) 
  __ verify_oop(R_method);
  {
    // WTF IS GOING ON?! This whole section is a mess, review is needed
    Label L, Lstatic;
    __ ld(TMP0, R_method, in_bytes(Method::const_offset()));
    __ lhu(TMP0, TMP0, in_bytes(ConstMethod::size_of_parameters_offset()));
    // MIPS n64 ABI: caller does not reserve space for the register auguments.
    // A0 and A1(if needed)
    __ lw(TMP1, R_method, in_bytes(Method::access_flags_offset()));
    __ andi(TMP1, TMP1, JVM_ACC_STATIC);
    __ beq(TMP1, XZERO, Lstatic);
    __ addi(TMP0, TMP0, 1);
    __ bind(Lstatic);
    __ addi(TMP0, TMP0, -7);
    // original mips instruction: __ blez(TMP0, L);
    // was replaced to:
    // START REPLACEMENT
    __ beq(TMP0, XZERO, L);
    __ blt(TMP0, XZERO, L);
    // END REPLACEMENT
    __ slliw(TMP0, TMP0, Address::times_8);
    __ sub(X2_SP, X2_SP, TMP0);
    __ bind(L);
  }
  // ???
  __ addi(TMP1, XZERO, -(StackAlignmentInBytes));
  __ and_(X2_SP, X2_SP, TMP1);
  __ or_(TMP1, XZERO, X2_SP);
  // [                          ] <--- sp
  //   ...                        (size of parameters - 8 )
  // [ monitor entry            ]
  //   ...
  // [ monitor entry            ]
  // [ monitor block top        ] ( the top monitor entry )
  // [ byte code pointer (0)    ] (if native, bcp = 0)
  // [ constant pool cache      ]
  // [ Method*                  ]
  // [ locals offset            ]
  // [ sender's sp              ]
  // [ sender's fp              ]
  // [ return address           ] <--- fp
  // [ method holder mirror     ]
  // [ result type info         ]
  // [ argumnet word n-1        ] <--- ( sender's sp )
  //   ...
  // [ argument word 0          ] <--- LVP
  
  __ dbgtrace_gencode_post(R_thread, TMP0, "%s: %s pc 0x%llx\n", __PRETTY_FUNCTION__,
                           " InterpreterRuntime::prepare_native_call", __ pc());

  // get signature handler
  {
    Label L;
    __ ld(TMP0, R_method, in_bytes(Method::signature_handler_offset()));
    __ bne(TMP0, XZERO, L);
    __ call_VM(XZERO, CAST_FROM_FN_PTR(address,
               InterpreterRuntime::prepare_native_call), R_method);
//    __ get_method(R_method);
    __ dbgtrace_gencode_post(R_thread, TMP0, "%s: %s pc 0x%llx\n", __PRETTY_FUNCTION__,
                             " InterpreterRuntime::prepare_native_call AFTER CALL", __ pc());
    __ ld(TMP0, R_method, in_bytes(Method::signature_handler_offset()));
    __ add(XZERO, X5, X5);
    __ bind(L);
    __ add(XZERO, X6, X6);
  }

  // call signature handler
  // FIXME: when change codes in InterpreterRuntime, note this point
  // from: begin of parameters
//  assert(InterpreterRuntime::SignatureHandlerGenerator::from() == LVP, "adjust this code");
  // to: current sp
//  assert(InterpreterRuntime::SignatureHandlerGenerator::to  () == SP, "adjust this code");
  // temp: T3
//  assert(InterpreterRuntime::SignatureHandlerGenerator::temp() == t  , "adjust this code");

  __ jalr(X1_RA, TMP0, 0);
  __ dbgtrace_gencode_post(R_thread, TMP0, "%s: %s pc 0x%llx\n", __PRETTY_FUNCTION__,
                           " InterpreterRuntime::prepare_native_call AFTER JR", __ pc());
//  __ get_method(R_method);


  //
  // if native function is static, and its second parameter has type length of double word,
  // and first parameter has type length of word, we have to reserve one word
  // for the first parameter, according to mips o32 abi.
  // if native function is not static, and its third parameter has type length of double word,
  // and second parameter has type length of word, we have to reserve one word for the second
  // parameter.
  //


  // result handler is in V0
  // set result handler
  __ sd(X10_RET0, (frame::interpreter_frame_result_handler_offset)*wordSize, X8_FP);

#define FIRSTPARA_SHIFT_COUNT 5
#define SECONDPARA_SHIFT_COUNT 9
#define THIRDPARA_SHIFT_COUNT 13
#define PARA_MASK  0xf

  // pass mirror handle if static call
  {
    Label L;
    const int mirror_offset = in_bytes(Klass::java_mirror_offset());
    __ lw(TMP0, R_method, in_bytes(Method::access_flags_offset()));
    __ andi(TMP0, TMP0, JVM_ACC_STATIC);
    __ beq(TMP0, XZERO, L);

    // get mirror
    __ ld(TMP0, R_method, in_bytes(Method:: const_offset()));
    __ ld(TMP0, TMP0, in_bytes(ConstMethod::constants_offset())); //??
    __ ld(TMP0, TMP0, ConstantPool::pool_holder_offset_in_bytes());
    __ ld(TMP0, TMP0, mirror_offset);
    // copy mirror into activation frame
    //__ sw(t, FP, frame::interpreter_frame_oop_temp_offset * wordSize);
    // pass handle to mirror
    __ sd(TMP0, frame::interpreter_frame_oop_temp_offset * wordSize, X8_FP);
    __ addi(TMP0, X8_FP, frame::interpreter_frame_oop_temp_offset * wordSize);
    __ ori(X11_ARG1, TMP0, 0);
    __ bind(L);
  }

  // [ mthd holder mirror ptr   ] <--- sp  --------------------| (only for static method)
  // [                          ]                              |
  //   ...                        size of parameters(or +1)    |
  // [ monitor entry            ]                              |
  //   ...                                                     |
  // [ monitor entry            ]                              |
  // [ monitor block top        ] ( the top monitor entry )    |
  // [ byte code pointer (0)    ] (if native, bcp = 0)         |
  // [ constant pool cache      ]                              |
  // [ Method*                  ]                              |
  // [ locals offset            ]                              |
  // [ sender's sp              ]                              |
  // [ sender's fp              ]                              |
  // [ return address           ] <--- fp                      |
  // [ method holder mirror     ] <----------------------------|
  // [ result type info         ]
  // [ argumnet word n-1        ] <--- ( sender's sp )
  //   ...
  // [ argument word 0          ] <--- S7


  // get native function entry point
  { Label L;
    __ ld(TMP0, R_method, in_bytes(Method::native_function_offset()));
#if 0
    ExternalAddress unsatisfied(SharedRuntime::native_method_throw_unsatisfied_link_error_entry());
    __ li(TMP1, unsatisfied);
#endif
    __ li(TMP1, 0);
    __ bne(TMP1, TMP0, L);
    __ call_VM(XZERO, CAST_FROM_FN_PTR(address, InterpreterRuntime::prepare_native_call), R_method);
//    __ get_method(R_method);
    __ verify_oop(R_method);
    __ ld(TMP1, R_method, in_bytes(Method::native_function_offset()));
    __ bind(L);
    // Dirty hack
    __ ld(TMP1, R_method, in_bytes(Method::native_function_offset()));
  }


  // pass JNIEnv
  // native function in T9
#if 0
#ifndef OPT_R_thread
  __ get_thread(thread);
#endif
#endif // #if 0
  __ addi(TMP0, R_thread, in_bytes(JavaThread::jni_environment_offset()));
  __ ori(X10_ARG0, TMP0, 0);
  // [ jni environment          ] <--- sp
  // [ mthd holder mirror ptr   ] ---------------------------->| (only for static method)
  // [                          ]                              |
  //   ...                        size of parameters           |
  // [ monitor entry            ]                              |
  //   ...                                                     |
  // [ monitor entry            ]                              |
  // [ monitor block top        ] ( the top monitor entry )    |
  // [ byte code pointer (0)    ] (if native, bcp = 0)         |
  // [ constant pool cache      ]                              |
  // [ Method*                  ]                              |
  // [ locals offset            ]                              |
  // [ sender's sp              ]                              |
  // [ sender's fp              ]                              |
  // [ return address           ] <--- fp                      |
  // [ method holder mirror     ] <----------------------------|
  // [ result type info         ]
  // [ argumnet word n-1        ] <--- ( sender's sp )
  //   ...
  // [ argument word 0          ] <--- S7

  // set_last_Java_frame_before_call
  __ sd(X8_FP, in_bytes(JavaFrameAnchor::last_Java_fp_offset()), R_thread);
  // Change state to native (we save the return address in the thread, since it might not
  // be pushed on the stack when we do a a stack traversal). It is enough that the pc()
  // points into the right code segment. It does not have to be the correct return pc.
  __ li(TMP0, (intptr_t)__ pc());
  __ sd(TMP0, in_bytes(JavaThread::last_Java_pc_offset()), R_thread);
  __ sd(X2_SP, in_bytes(JavaThread::last_Java_sp_offset()), R_thread);

  // change thread state
#ifdef ASSERT
  {
    Label L;
    __ lw(TMP0, R_thread, in_bytes(JavaThread::thread_state_offset()));
    __ addi(TMP0, TMP0, (-1) * _thread_in_Java);
    __ beq(TMP0, XZERO, L);
    __ stop("Wrong thread state in native stub");
    __ bind(L);
  }
#endif

  __ ori(TMP0, XZERO, _thread_in_native);
  __ sw(TMP0, in_bytes(JavaThread::thread_state_offset()), R_thread);

  // call native method
  __ jalr(X1_RA, TMP1, 0);
  __ dbgtrace_gencode_post(R_thread, TMP0, "%s: %s pc 0x%llx\n", __PRETTY_FUNCTION__,
                           " AFTER CALLING NATIVE INIT ", __ pc());
  // result potentially in V0 or F0


  // via _last_native_pc and not via _last_jave_sp
  // NOTE: the order of theses push(es) is known to frame::interpreter_frame_result.
  //  If the order changes or anything else is added to the stack the code in
  // interpreter_frame_result will have to be changed.
  //FIXME, should modify here
  // save return value to keep the value from being destroyed by other calls
  __ push(dtos);
  __ push(ltos);

  // change thread state
//  __ get_thread(thread);
  __ ori(TMP0, XZERO, _thread_in_native_trans);
  __ sw(TMP0, in_bytes(JavaThread::thread_state_offset()), R_thread);

#if 0
  if( os::is_MP() ) __ sync(); // Force this write out before the read below
#endif

  // check for safepoint operation in progress and/or pending suspend requests
  { Label Continue;

    // Don't use call_VM as it will see a possible pending exception and forward it
    // and never return here preventing us from clearing _last_native_pc down below.
    // Also can't use call_VM_leaf either as it will check to see if BCP & LVP are
    // preserved and correspond to the bcp/locals pointers. So we do a runtime call
    // by hand.
    //
    Label L;
    __ li(TMP1, (intptr_t)SafepointSynchronize::address_of_state());
    __ lw(TMP1, TMP1, 0);
    __ bne(TMP1, XZERO, L);
    __ lw(TMP1, R_thread, in_bytes(JavaThread::suspend_flags_offset()));
    __ beq(TMP1, XZERO, Continue);
    __ bind(L);
    __ ori(X10_ARG0, R_thread, 0);
    __ call_c(CAST_FROM_FN_PTR(address, JavaThread::check_special_condition_for_native_trans),
                             relocInfo::runtime_call_type);

#ifndef OPT_R_thread
//    __ get_thread(thread);
#endif
    //add for compressedoops
    __ reinit_heapbase(X31_narrowOopBase);
    __ bind(Continue);
  }

  // change thread state
  __ ori(TMP0, XZERO, _thread_in_Java);
  __ sw(TMP0, in_bytes(JavaThread::thread_state_offset()), R_thread);
  __ reset_last_Java_frame();

  // reset handle block
  __ ld(TMP0, R_thread, in_bytes(JavaThread::active_handles_offset()));
  __ sw(XZERO, JNIHandleBlock::top_offset_in_bytes(), R_thread);

  // If result was an oop then unbox and save it in the frame
  { Label L;
    Label no_oop, store_result;
    //FIXME, addi only support 16-bit imeditate
    __ ld(TMP0, X8_FP, frame::interpreter_frame_result_handler_offset*wordSize);
    __ li(TMP1, (intptr_t)AbstractInterpreter::result_handler(T_OBJECT));
    __ bne(TMP1, TMP0, no_oop);
    __ pop(ltos);
    __ beq(X10_ARG0, XZERO, store_result);
    // unbox
    __ ld(X10_ARG0, X10_ARG0, 0);
    __ bind(store_result);
    __ sd(X10_ARG0, (frame::interpreter_frame_oop_temp_offset)*wordSize, X8_FP);
    // keep stack depth as expected by pushing oop which will eventually be discarded
    __ push(ltos);
    __ bind(no_oop);
  }
  {
    Label no_reguard;
    __ lw(TMP0, R_thread, in_bytes(JavaThread::stack_guard_state_offset()));
    __ ori(TMP1, XZERO, JavaThread::stack_guard_yellow_disabled);
    __ bne(TMP0, TMP1, no_reguard);
//    __ pushad();
    __ ori(XJUNK, X2_SP, 0);
    __ ori(TMP1, XZERO, -StackAlignmentInBytes);
    __ and_(X2_SP, X2_SP, TMP1);
    __ call_c(CAST_FROM_FN_PTR(address, SharedRuntime::reguard_yellow_pages), relocInfo::runtime_call_type);
    __ ori(X2_SP, XJUNK, 0);
//    __ popad();
    //add for compressedoops
    __ reinit_heapbase(X31_narrowOopBase);
    __ bind(no_reguard);
  }

/* Old code of unknown origin (commented out on 31/05/2k19)
  // restore BCP to have legal interpreter frame,
  // i.e., bci == 0 <=> BCP == code_base()
  // Can't call_VM until bcp is within reasonable.
//  __ get_method(R_method);      // method is junk from thread_in_native to now.
  __ verify_oop(R_method);
  __ ld(TMP1, R_method, in_bytes(Method::const_offset()));
//__ lea(TMP1, Address(TMP1, in_bytes(ConstMethod::codes_offset())));
  __ ld(TMP1, TMP1, in_bytes(ConstMethod::codes_offset()));
  // ???
  __ ori(X18_bcp, TMP1, 0);
*/

/* Equivalent x86 code (commented out on 31/05/2k19)
1312   __ movptr(r13, Address(method, Method::const_offset()));   // get ConstMethod*
1313   __ lea(r13, Address(r13, ConstMethod::codes_offset()));    // get codebase
*/

/* Replaced the above sample on (31/05/2k19) */
  __ verify_oop(R_method);
  __ ld(TMP1, R_method, in_bytes(Method::const_offset()));
  __ addi(TMP1, TMP1, in_bytes(ConstMethod::codes_offset()));
  __ ori(X18_bcp, TMP1, 0);

  {
    Label L;
    __ lw(TMP0, R_thread, in_bytes(Thread::pending_exception_offset()));
    __ beq(TMP0, XZERO, L);
    // Note: At some point we may want to unify this with the code used in
    // call_VM_base();
    // i.e., we should use the StubRoutines::forward_exception code. For now this
    // doesn't work here because the sp is not correctly set at this point.
    __ call_VM(XZERO,
               CAST_FROM_FN_PTR(address,
               InterpreterRuntime::throw_pending_exception));
    __ should_not_reach_here();
    __ bind(L);
  }
  // do unlocking if necessary
  {
    Label L;
    __ lw(TMP1, R_method, in_bytes(Method::access_flags_offset()));
    __ andi(TMP1, TMP1, JVM_ACC_SYNCHRONIZED);
    __ beq(TMP1, XZERO, L);
    // the code below should be shared with interpreter macro assembler implementation
    {
      Label unlock;
      // BasicObjectLock will be first in list,
      // since this is a synchronized method. However, need
      // to check that the object has not been unlocked by
      // an explicit monitorexit bytecode.
#if 0
      __ addi(X10_ARG0, X8_FP, frame::interpreter_frame_initial_sp_offset * wordSize - (int)sizeof(BasicObjectLock));
#endif
      __ addi(X10_ARG0, X8_FP, -8 * wordSize - (int)sizeof(BasicObjectLock));
      // address of first monitor

      __ ld(TMP1, X10_ARG0, BasicObjectLock::obj_offset_in_bytes());
      __ bne(TMP1, XZERO, unlock);

      // Entry already unlocked, need to throw exception
      __ call_VM(XZERO, CAST_FROM_FN_PTR(address,
      InterpreterRuntime::throw_illegal_monitor_state_exception));
      __ should_not_reach_here();

      __ bind(unlock);
      __ unlock_object(X10_ARG0);
    }
    __ bind(L);
  }

  // jvmti/jvmpi support
  // Note: This must happen _after_ handling/throwing any exceptions since
  //       the exception handler code notifies the runtime of method exits
  //       too. If this happens before, method entry/exit notifications are
  //       not properly paired (was bug - gri 11/22/99).
//  __ notify_method_exit(false, vtos, InterpreterMacroAssembler::NotifyJVMTI);

  // restore potential result in V0,
  // call result handler to restore potential result in ST0 & handle result

  __ pop(ltos);
  __ pop(dtos);

  __ dbgtrace_gencode_post(R_thread, TMP0, "%s: %s pc 0x%llx\n", __PRETTY_FUNCTION__,
                           " BEFORE FRAME RESULT HANDLER", __ pc());

  __ ld(TMP0, X8_FP, (frame::interpreter_frame_result_handler_offset) * wordSize);
  __ jalr(X1_RA, TMP0, 0);

  
  __ dbgtrace_gencode_post(R_thread, TMP0, "%s: %s pc 0x%llx\n", __PRETTY_FUNCTION__,
                           " BEFORE REMOVING ACTIVATION ", __ pc());

  // remove activation
#if 0
  __ ld(X2_SP, X8_FP, frame::interpreter_frame_sender_sp_offset * wordSize); // get sender sp
  __ ld(X1_RA, X8_FP, frame::interpreter_frame_return_addr_offset * wordSize); // get return address
  __ ld(X8_FP, X8_FP, frame::interpreter_frame_sender_fp_offset * wordSize); // restore sender's fp
#endif

  // equivalent x86 code
  // 1383   // remove activation
  // 1384   __ movptr(t, Address(rbp,
  // 1385                        frame::interpreter_frame_sender_sp_offset *
  // 1386                        wordSize)); // get sender sp
  // 1387   __ leave();                                // remove frame anchor
  // 1388   __ pop(rdi);                               // get return address
  // 1389   __ mov(rsp, t);                            // set sp to sender sp
  //
  __ ld(X2_SP, X8_FP, -1 * wordSize); // NOTE: (this is not needed, left for debug, values shall be overwritten)
  __ ld(X1_RA, X8_FP, 1 * wordSize);  // NOTE: (this is not needed, left for debug, values shall be overwritten)
  // __ ld(X8_FP, X8_FP, 0); // restore sender's fp

// "fixed" code (31/05/2k19)
  // [ locals offset            ]                              |
  // [ sender's sp              ]                              |
  // [ sender's fp              ]                              |
  // [ return address           ] <--- fp                      |
  __ ld(X1_RA, X8_FP, -1 * wordSize); // get return address
  __ ld(X2_SP, X8_FP, -3 * wordSize); // get sender sp
  __ ld(X8_FP, X8_FP, -2 * wordSize); // restore sender's fp
  
  __ dbgtrace_gencode_post(R_thread, TMP0, "%s: %s pc 0x%llx\n", __PRETTY_FUNCTION__,
                           " AFTER REMOVING ACTIVATION BEFORE JUMP OUT", __ pc());
  __ jr(X1_RA);

#if 0
#ifndef CORE
  if (inc_counter) {
    // Handle overflow of counter and compile method
    __ bind(invocation_counter_overflow);
    generate_counter_overflow(&continue_after_compile);
    // entry_point is the beginning of this
    // function and checks again for compiled code
  }
#endif

#endif // #if 0
  __ dbgtrace_gencode_post(R_thread, TMP0, "%s: %s pc 0x%llx\n", __PRETTY_FUNCTION__,
                           " BEFORE generate_native_entry UNIMPL ", __ pc());
  return entry_point;
}

#if 0
// Interpreter stub for calling a native method. (asm interpreter)
// This sets up a somewhat different looking stack for calling the
// native method than the typical interpreter frame setup.
//
// On entry:
//   R19_method    - method
//   R16_thread    - JavaThread*
//   R15_esp       - intptr_t* sender tos
//
//   abstract stack (grows up)
//     [  IJava (caller of JNI callee)  ]  <-- ASP
//        ...
address TemplateInterpreterGenerator::generate_native_entry(bool synchronized) {
  address entry = __ pc();
  __ unimplemented(__func__);
  return entry;
  /*

  address entry = __ pc();

  const bool inc_counter = UseCompiler || CountCompiledCalls;

  // -----------------------------------------------------------------------------
  // Allocate a new frame that represents the native callee (i2n frame).
  // This is not a full-blown interpreter frame, but in particular, the
  // following registers are valid after this:
  // - R19_method
  // - R18_local (points to start of argumuments to native function)
  //
  //   abstract stack (grows up)
  //     [  IJava (caller of JNI callee)  ]  <-- ASP
  //        ...

  const Register signature_handler_fd = R11_scratch1;
  const Register pending_exception    = R0;
  const Register result_handler_addr  = R31;
  const Register native_method_fd     = R11_scratch1;
  const Register access_flags         = R22_tmp2;
  const Register active_handles       = R11_scratch1; // R26_monitor saved to state.
  const Register sync_state           = R12_scratch2;
  const Register sync_state_addr      = sync_state;   // Address is dead after use.
  const Register suspend_flags        = R11_scratch1;

  //=============================================================================
  // Allocate new frame and initialize interpreter state.

  Label exception_return;
  Label exception_return_sync_check;
  Label stack_overflow_return;

  // Generate new interpreter state and jump to stack_overflow_return in case of
  // a stack overflow.
  //generate_compute_interpreter_state(stack_overflow_return);

  Register size_of_parameters = R22_tmp2;

  generate_fixed_frame(true, size_of_parameters, noreg */ /* unused */ /*);

  //=============================================================================
  // Increment invocation counter. On overflow, entry to JNI method
  // will be compiled.
  Label invocation_counter_overflow, continue_after_compile;
  if (inc_counter) {
    if (synchronized) {
      // Since at this point in the method invocation the exception handler
      // would try to exit the monitor of synchronized methods which hasn't
      // been entered yet, we set the thread local variable
      // _do_not_unlock_if_synchronized to true. If any exception was thrown by
      // runtime, exception handling i.e. unlock_if_synchronized_method will
      // check this thread local flag.
      // This flag has two effects, one is to force an unwind in the topmost
      // interpreter frame and not perform an unlock while doing so.
      __ li(R0, 1);
      __ stb(R0, in_bytes(JavaThread::do_not_unlock_if_synchronized_offset()), R16_thread);
    }
    generate_counter_incr(&invocation_counter_overflow, NULL, NULL);

    __ BIND(continue_after_compile);
    // Reset the _do_not_unlock_if_synchronized flag.
    if (synchronized) {
      __ li(R0, 0);
      __ stb(R0, in_bytes(JavaThread::do_not_unlock_if_synchronized_offset()), R16_thread);
    }
  }

  // access_flags = method->access_flags();
  // Load access flags.
  assert(access_flags->is_nonvolatile(),
         "access_flags must be in a non-volatile register");
  // Type check.
  assert(4 == sizeof(AccessFlags), "unexpected field size");
  __ lwz(access_flags, method_(access_flags));

  // We don't want to reload R19_method and access_flags after calls
  // to some helper functions.
  assert(R19_method->is_nonvolatile(),
         "R19_method must be a non-volatile register");

  // Check for synchronized methods. Must happen AFTER invocation counter
  // check, so method is not locked if counter overflows.

  if (synchronized) {
    lock_method(access_flags, R11_scratch1, R12_scratch2, true);

    // Update monitor in state.
    __ ld(R11_scratch1, 0, R1_SP);
    __ std(R26_monitor, _ijava_state_neg(monitors), R11_scratch1);
  }

  // jvmti/jvmpi support
  __ notify_method_entry();

  //=============================================================================
  // Get and call the signature handler.

  __ ld(signature_handler_fd, method_(signature_handler));
  Label call_signature_handler;

  __ cmpdi(CCR0, signature_handler_fd, 0);
  __ bne(CCR0, call_signature_handler);

  // Method has never been called. Either generate a specialized
  // handler or point to the slow one.
  //
  // Pass parameter 'false' to avoid exception check in call_VM.
  __ call_VM(noreg, CAST_FROM_FN_PTR(address, InterpreterRuntime::prepare_native_call), R19_method, false);

  // Check for an exception while looking up the target method. If we
  // incurred one, bail.
  __ ld(pending_exception, thread_(pending_exception));
  __ cmpdi(CCR0, pending_exception, 0);
  __ bne(CCR0, exception_return_sync_check); // Has pending exception.

  // Reload signature handler, it may have been created/assigned in the meanwhile.
  __ ld(signature_handler_fd, method_(signature_handler));
  __ twi_0(signature_handler_fd); // Order wrt. load of klass mirror and entry point (isync is below).

  __ BIND(call_signature_handler);

  // Before we call the signature handler we push a new frame to
  // protect the interpreter frame volatile registers when we return
  // from jni but before we can get back to Java.

  // First set the frame anchor while the SP/FP registers are
  // convenient and the slow signature handler can use this same frame
  // anchor.

  // We have a TOP_IJAVA_FRAME here, which belongs to us.
  __ set_top_ijava_frame_at_SP_as_last_Java_frame(R1_SP, R12_scratch2*/ /*tmp*/ /*);

  // Now the interpreter frame (and its call chain) have been
  // invalidated and flushed. We are now protected against eager
  // being enabled in native code. Even if it goes eager the
  // registers will be reloaded as clean and we will invalidate after
  // the call so no spurious flush should be possible.

  // Call signature handler and pass locals address.
  //
  // Our signature handlers copy required arguments to the C stack
  // (outgoing C args), R3_ARG1 to R10_ARG8, and FARG1 to FARG13.
  __ mr(R3_ARG1, R18_locals);
#if !defined(ABI_ELFv2)
  __ ld(signature_handler_fd, 0, signature_handler_fd);
#endif

  __ call_stub(signature_handler_fd);

  // Remove the register parameter varargs slots we allocated in
  // compute_interpreter_state. SP+16 ends up pointing to the ABI
  // outgoing argument area.
  //
  // Not needed on RISCV64.
  //__ add(SP, SP, Argument::n_register_parameters*BytesPerWord);

  assert(result_handler_addr->is_nonvolatile(), "result_handler_addr must be in a non-volatile register");
  // Save across call to native method.
  __ mr(result_handler_addr, R3_RET);

  __ isync(); // Acquire signature handler before trying to fetch the native entry point and klass mirror.

  // Set up fixed parameters and call the native method.
  // If the method is static, get mirror into R4_ARG2.
  {
    Label method_is_not_static;
    // Access_flags is non-volatile and still, no need to restore it.

    // Restore access flags.
    __ testbitdi(CCR0, R0, access_flags, JVM_ACC_STATIC_BIT);
    __ bfalse(CCR0, method_is_not_static);

    // constants = method->constants();
    __ ld(R11_scratch1, in_bytes(Method::const_offset()), R19_method);
    __ ld(R11_scratch1, in_bytes(ConstMethod::constants_offset()), R11_scratch1);
    // pool_holder = method->constants()->pool_holder();
    __ ld(R11_scratch1*/ /*pool_holder*/ /*, ConstantPool::pool_holder_offset_in_bytes(),
          R11_scratch1*/ /*constants*/ /*);

    const int mirror_offset = in_bytes(Klass::java_mirror_offset());

    // mirror = pool_holder->klass_part()->java_mirror();
    __ ld(R0*/ /*mirror*/ /*, mirror_offset, R11_scratch1*/ /*pool_holder*/ /*);
    // state->_native_mirror = mirror;

    __ ld(R11_scratch1, 0, R1_SP);
    __ std(R0*/ /*mirror*/ /*, _ijava_state_neg(oop_tmp), R11_scratch1);
    // R4_ARG2 = &state->_oop_temp;
    __ addi(R4_ARG2, R11_scratch1, _ijava_state_neg(oop_tmp));
    __ BIND(method_is_not_static);
  }

  // At this point, arguments have been copied off the stack into
  // their JNI positions. Oops are boxed in-place on the stack, with
  // handles copied to arguments. The result handler address is in a
  // register.

  // Pass JNIEnv address as first parameter.
  __ addir(R3_ARG1, thread_(jni_environment));

  // Load the native_method entry before we change the thread state.
  __ ld(native_method_fd, method_(native_function));

  //=============================================================================
  // Transition from _thread_in_Java to _thread_in_native. As soon as
  // we make this change the safepoint code needs to be certain that
  // the last Java frame we established is good. The pc in that frame
  // just needs to be near here not an actual return address.

  // We use release_store_fence to update values like the thread state, where
  // we don't want the current thread to continue until all our prior memory
  // accesses (including the new thread state) are visible to other threads.
  __ li(R0, _thread_in_native);
  __ release();

  // TODO RISCV port assert(4 == JavaThread::sz_thread_state(), "unexpected field size");
  __ stw(R0, thread_(thread_state));

  if (UseMembar) {
    __ fence();
  }

  //=============================================================================
  // Call the native method. Argument registers must not have been
  // overwritten since "__ call_stub(signature_handler);" (except for
  // ARG1 and ARG2 for static methods).
  __ call_c(native_method_fd);

  __ li(R0, 0);
  __ ld(R11_scratch1, 0, R1_SP);
  __ std(R3_RET, _ijava_state_neg(lresult), R11_scratch1);
  __ stfd(F1_RET, _ijava_state_neg(fresult), R11_scratch1);
  __ std(R0*/ /*mirror*/ /*, _ijava_state_neg(oop_tmp), R11_scratch1); // reset

  // Note: C++ interpreter needs the following here:
  // The frame_manager_lr field, which we use for setting the last
  // java frame, gets overwritten by the signature handler. Restore
  // it now.
  //__ get_PC_trash_LR(R11_scratch1);
  //__ std(R11_scratch1, _top_ijava_frame_abi(frame_manager_lr), R1_SP);

  // Because of GC R19_method may no longer be valid.

  // Block, if necessary, before resuming in _thread_in_Java state.
  // In order for GC to work, don't clear the last_Java_sp until after
  // blocking.

  //=============================================================================
  // Switch thread to "native transition" state before reading the
  // synchronization state. This additional state is necessary
  // because reading and testing the synchronization state is not
  // atomic w.r.t. GC, as this scenario demonstrates: Java thread A,
  // in _thread_in_native state, loads _not_synchronized and is
  // preempted. VM thread changes sync state to synchronizing and
  // suspends threads for GC. Thread A is resumed to finish this
  // native method, but doesn't block here since it didn't see any
  // synchronization in progress, and escapes.

  // We use release_store_fence to update values like the thread state, where
  // we don't want the current thread to continue until all our prior memory
  // accesses (including the new thread state) are visible to other threads.
  __ li(R0*/ /*thread_state*/ /*, _thread_in_native_trans);
  __ release();
  __ stw(R0*/ /*thread_state*/ /*, thread_(thread_state));
  if (UseMembar) {
    __ fence();
  }
  // Write serialization page so that the VM thread can do a pseudo remote
  // membar. We use the current thread pointer to calculate a thread
  // specific offset to write to within the page. This minimizes bus
  // traffic due to cache line collision.
  else {
    __ serialize_memory(R16_thread, R11_scratch1, R12_scratch2);
  }

  // Now before we return to java we must look for a current safepoint
  // (a new safepoint can not start since we entered native_trans).
  // We must check here because a current safepoint could be modifying
  // the callers registers right this moment.

  // Acquire isn't strictly necessary here because of the fence, but
  // sync_state is declared to be volatile, so we do it anyway
  // (cmp-br-isync on one path, release (same as acquire on RISCV64) on the other path).
  int sync_state_offs = __ load_const_optimized(sync_state_addr, SafepointSynchronize::address_of_state(), */ /*temp*/ /*R0, true);

  // TODO RISCV port assert(4 == SafepointSynchronize::sz_state(), "unexpected field size");
  __ lwz(sync_state, sync_state_offs, sync_state_addr);

  // TODO RISCV port assert(4 == Thread::sz_suspend_flags(), "unexpected field size");
  __ lwz(suspend_flags, thread_(suspend_flags));

  Label sync_check_done;
  Label do_safepoint;
  // No synchronization in progress nor yet synchronized.
  __ cmpwi(CCR0, sync_state, SafepointSynchronize::_not_synchronized);
  // Not suspended.
  __ cmpwi(CCR1, suspend_flags, 0);

  __ bne(CCR0, do_safepoint);
  __ beq(CCR1, sync_check_done);
  __ bind(do_safepoint);
  __ isync();
  // Block. We do the call directly and leave the current
  // last_Java_frame setup undisturbed. We must save any possible
  // native result across the call. No oop is present.

  __ mr(R3_ARG1, R16_thread);
#if defined(ABI_ELFv2)
  __ call_c(CAST_FROM_FN_PTR(address, JavaThread::check_special_condition_for_native_trans),
            relocInfo::none);
#else
  __ call_c(CAST_FROM_FN_PTR(FunctionDescriptor*, JavaThread::check_special_condition_for_native_trans),
            relocInfo::none);
#endif

  __ bind(sync_check_done);

  //=============================================================================
  // <<<<<< Back in Interpreter Frame >>>>>

  // We are in thread_in_native_trans here and back in the normal
  // interpreter frame. We don't have to do anything special about
  // safepoints and we can switch to Java mode anytime we are ready.

  // Note: frame::interpreter_frame_result has a dependency on how the
  // method result is saved across the call to post_method_exit. For
  // native methods it assumes that the non-FPU/non-void result is
  // saved in _native_lresult and a FPU result in _native_fresult. If
  // this changes then the interpreter_frame_result implementation
  // will need to be updated too.

  // On RISCV64, we have stored the result directly after the native call.

  //=============================================================================
  // Back in Java

  // We use release_store_fence to update values like the thread state, where
  // we don't want the current thread to continue until all our prior memory
  // accesses (including the new thread state) are visible to other threads.
  __ li(R0*/ /*thread_state*/ /*, _thread_in_Java);
  __ release();
  __ stw(R0*/ /*thread_state*/ /*, thread_(thread_state));
  if (UseMembar) {
    __ fence();
  }

  __ reset_last_Java_frame();

  // Jvmdi/jvmpi support. Whether we've got an exception pending or
  // not, and whether unlocking throws an exception or not, we notify
  // on native method exit. If we do have an exception, we'll end up
  // in the caller's context to handle it, so if we don't do the
  // notify here, we'll drop it on the floor.
  __ notify_method_exit(true*/ /*native method*/ /*,
                        ilgl */ /*illegal state (not used for native methods)*/ /*,
                        InterpreterMacroAssembler::NotifyJVMTI,
                        false */ /*check_exceptions*/ /*);

  //=============================================================================
  // Handle exceptions

  if (synchronized) {
    // Don't check for exceptions since we're still in the i2n frame. Do that
    // manually afterwards.
    unlock_method(false);
  }

  // Reset active handles after returning from native.
  // thread->active_handles()->clear();
  __ ld(active_handles, thread_(active_handles));
  // TODO RISCV port assert(4 == JNIHandleBlock::top_size_in_bytes(), "unexpected field size");
  __ li(R0, 0);
  __ stw(R0, JNIHandleBlock::top_offset_in_bytes(), active_handles);

  Label exception_return_sync_check_already_unlocked;
  __ ld(R0*/ /*pending_exception*/ /*, thread_(pending_exception));
  __ cmpdi(CCR0, R0*/ /*pending_exception*/ /*, 0);
  __ bne(CCR0, exception_return_sync_check_already_unlocked);

  //-----------------------------------------------------------------------------
  // No exception pending.

  // Move native method result back into proper registers and return.
  // Invoke result handler (may unbox/promote).
  __ ld(R11_scratch1, 0, R1_SP);
  __ ld(R3_RET, _ijava_state_neg(lresult), R11_scratch1);
  __ lfd(F1_RET, _ijava_state_neg(fresult), R11_scratch1);
  __ call_stub(result_handler_addr);

  __ merge_frames(*/ /*top_frame_sp*/ /* R21_sender_SP, */ /*return_pc*/ /* R0, R11_scratch1, R12_scratch2);

  // Must use the return pc which was loaded from the caller's frame
  // as the VM uses return-pc-patching for deoptimization.
  __ mtlr(R0);
  __ blr();

  //-----------------------------------------------------------------------------
  // An exception is pending. We call into the runtime only if the
  // caller was not interpreted. If it was interpreted the
  // interpreter will do the correct thing. If it isn't interpreted
  // (call stub/compiled code) we will change our return and continue.

  __ BIND(exception_return_sync_check);

  if (synchronized) {
    // Don't check for exceptions since we're still in the i2n frame. Do that
    // manually afterwards.
    unlock_method(false);
  }
  __ BIND(exception_return_sync_check_already_unlocked);

  const Register return_pc = R31;

  __ ld(return_pc, 0, R1_SP);
  __ ld(return_pc, _abi(lr), return_pc);

  // Get the address of the exception handler.
  __ call_VM_leaf(CAST_FROM_FN_PTR(address, SharedRuntime::exception_handler_for_return_address),
                  R16_thread,
                  return_pc */ /* return pc */ /*);
  __ merge_frames(*/ /*top_frame_sp*/ /* R21_sender_SP, noreg, R11_scratch1, R12_scratch2);

  // Load the PC of the the exception handler into LR.
  __ mtlr(R3_RET);

  // Load exception into R3_ARG1 and clear pending exception in thread.
  __ ld(R3_ARG1*/ /*exception*/ /*, thread_(pending_exception));
  __ li(R4_ARG2, 0);
  __ std(R4_ARG2, thread_(pending_exception));

  // Load the original return pc into R4_ARG2.
  __ mr(R4_ARG2*/ /*issuing_pc*/ /*, return_pc);

  // Return to exception handler.
  __ blr();

  //=============================================================================
  // Counter overflow.

  if (inc_counter) {
    // Handle invocation counter overflow.
    __ bind(invocation_counter_overflow);

    generate_counter_overflow(continue_after_compile);
  }

  return entry;
  */
}
#endif // #if 0

// Generic interpreted method entry to (asm) interpreter.
//
address TemplateInterpreterGenerator::generate_normal_entry(bool synchronized) {
  bool inc_counter = UseCompiler || CountCompiledCalls;
  address entry = __ pc();
  // Generate the code to allocate the interpreter stack frame.
  Register Rsize_of_parameters = X11_ARG1, // Written by generate_fixed_frame.
           Rsize_of_locals     = X12_ARG2; // Written by generate_fixed_frame.

  //__ debug_start_trace();
  generate_fixed_frame(Rsize_of_parameters, Rsize_of_locals);

#ifdef FAST_DISPATCH
  __ unimplemented("Fast dispatch in generate_normal_entry");
#if 0
  __ set((intptr_t)Interpreter::dispatch_table(), IdispatchTables);
  // Set bytecode dispatch table base.
#endif
#endif

  // --------------------------------------------------------------------------
  // Zero out non-parameter locals.
  // Note: *Always* zero out non-parameter locals as Sparc does. It's not
  // worth to ask the flag, just do it.
  Register Rslot_addr = X13_ARG3,
           Rnum       = X14_ARG4;
  Label Lno_locals, Lzero_loop;

  // Set up the zeroing loop.
  __ beq(Rsize_of_locals, XZERO, Lno_locals);
  __ sub(Rnum, Rsize_of_locals, Rsize_of_parameters);
  __ sub(Rslot_addr, X22_locals, Rsize_of_parameters);
  __ srli(Rnum, Rnum, Interpreter::logStackElementSize);

  // The zero locals loop.
  __ bind(Lzero_loop);
  __ sd(XZERO, 0, Rslot_addr);
  __ addi(Rslot_addr, Rslot_addr, -Interpreter::stackElementSize);
  __ addi(Rnum, Rnum, -1);
  __ bne(Rnum, XZERO, Lzero_loop);

  __ bind(Lno_locals);

  // --------------------------------------------------------------------------
  // Counter increment and overflow check.
  Label invocation_counter_overflow,
        profile_method,
        profile_method_continue;
  if (inc_counter || ProfileInterpreter) {

    Register Rdo_not_unlock_if_synchronized_addr = X6_T1;
    if (synchronized) {
      // Since at this point in the method invocation the exception handler
      // would try to exit the monitor of synchronized methods which hasn't
      // been entered yet, we set the thread local variable
      // _do_not_unlock_if_synchronized to true. If any exception was thrown by
      // runtime, exception handling i.e. unlock_if_synchronized_method will
      // check this thread local flag.
      // This flag has two effects, one is to force an unwind in the topmost
      // interpreter frame and not perform an unlock while doing so.
      __ li(X5_T0, 1);
      __ sb(X5_T0, in_bytes(JavaThread::do_not_unlock_if_synchronized_offset()), X20_thread);
    }

    // Argument and return type profiling.
    /* TODO: Implement profiling and counters
    __ profile_parameters_type(X11_ARG1, X12_ARG2, X13_ARG3, X14_ARG4);

    // Increment invocation counter and check for overflow.
    if (inc_counter) {
      generate_counter_incr(&invocation_counter_overflow, &profile_method, &profile_method_continue);
    }

    __ bind(profile_method_continue);
    */

    // Reset the _do_not_unlock_if_synchronized flag.
    if (synchronized) {
      __ sb(XZERO, in_bytes(JavaThread::do_not_unlock_if_synchronized_offset()), X20_thread);
    }
  }

  // --------------------------------------------------------------------------
  // Locking of synchronized methods. Must happen AFTER invocation_counter
  // check and stack overflow check, so method is not locked if overflows.
  /* TODO: Implement method locking
  if (synchronized) {
    lock_method(R3_ARG1, R4_ARG2, R5_ARG3);
  }
#ifdef ASSERT
  else {
    Label Lok;
    __ lwu(X5_T0, in_bytes(Method::access_flags_offset()), X23_method);
    __ andi_(X5_T0, X5_T0, JVM_ACC_SYNCHRONIZED);
    __ asm_assert_eq("method needs synchronization", 0x8521);
    __ bind(Lok);
  }
#endif // ASSERT
  */

  __ verify_thread();

  // --------------------------------------------------------------------------
  // JVMTI support
  // TODO: Add JVMTI support
  //__ notify_method_entry();

  // --------------------------------------------------------------------------
  // Start executing instructions.
  __ dispatch_next(vtos);

  // --------------------------------------------------------------------------
  // Out of line counter overflow and MDO creation code.
  /* TODO: Implement this stuff at some point
  if (ProfileInterpreter) {
    // We have decided to profile this method in the interpreter.
    __ bind(profile_method);
    __ call_VM(noreg, CAST_FROM_FN_PTR(address, InterpreterRuntime::profile_method));
    __ set_method_data_pointer_for_bcp();
    __ b(profile_method_continue);
  }

  if (inc_counter) {
    // Handle invocation counter overflow.
    __ bind(invocation_counter_overflow);
    generate_counter_overflow(profile_method_continue);
  }
  */
  //__ debug_stop_trace();
  return entry;
}

// =============================================================================
// Entry points

address AbstractInterpreterGenerator::generate_method_entry(
                                        AbstractInterpreter::MethodKind kind) {
  // Determine code generation flags.
  bool synchronized = false;
  address entry_point = NULL;

  // TODO: Implement all of these methods for entry point generation
  switch (kind) {
  case Interpreter::zerolocals             :                                                                              break;
  case Interpreter::zerolocals_synchronized: synchronized = true;                                                         break;
  case Interpreter::native                 : entry_point = ((InterpreterGenerator*) this)->generate_native_entry(false);  break;
  case Interpreter::native_synchronized    : entry_point = ((InterpreterGenerator*) this)->generate_native_entry(true);   break;
  case Interpreter::empty                  : entry_point = ((InterpreterGenerator*) this)->generate_empty_entry();        break;
  case Interpreter::accessor               : entry_point = ((InterpreterGenerator*) this)->generate_accessor_entry();     break;
  case Interpreter::abstract               : entry_point = ((InterpreterGenerator*) this)->generate_abstract_entry();     break;
  case Interpreter::java_lang_math_sin     : // fall thru
  case Interpreter::java_lang_math_cos     : // fall thru
  case Interpreter::java_lang_math_tan     : // fall thru
  case Interpreter::java_lang_math_abs     : // fall thru
  case Interpreter::java_lang_math_log     : // fall thru
  case Interpreter::java_lang_math_log10   : // fall thru
  case Interpreter::java_lang_math_sqrt    : // fall thru
  case Interpreter::java_lang_math_pow     : // fall thru
  case Interpreter::java_lang_math_exp     : entry_point = ((InterpreterGenerator*) this)->generate_math_entry(kind);     break;
  case Interpreter::java_lang_ref_reference_get
                                           : entry_point = ((InterpreterGenerator*)this)->generate_Reference_get_entry(); break;
  default                                  : ShouldNotReachHere();                                                        break;
  }

  if (entry_point) {
    return entry_point;
  }

  return ((InterpreterGenerator*) this)->generate_normal_entry(synchronized);
}

// These should never be compiled since the interpreter will prefer
// the compiled version to the intrinsic version.
bool AbstractInterpreter::can_be_compiled(methodHandle m) {
  //TODO: check on math_entry_available
  return false;
  /*
  return !math_entry_available(method_kind(m));
  */
}

static int size_activation_helper(int callee_extra_locals, int max_stack, 
                                  int monitor_size) {

  // Figure out the size of an interpreter frame (in words) given that we have 
  // a fully allocated expression stack, the callee will have 
  // callee_extra_locals (so we can account for frame extension) and 
  // monitor_size for monitors. Basically we need to calculate this exactly 
  // like generate_fixed_frame/generate_compute_interpreter_state.
  //
  // Ensure that the stack stays 16-byte aligned. Monitor size and fixed
  // interpreter frame sizes are already aligned.

  // callee_locals and max_stack are counts, not the size in frame.
  const int locals_size =
    round_to(callee_extra_locals * Interpreter::stackElementWords, 
         2 * WordsPerLong);

  // Add one to the expression stack size because Resp always points to the 
  // next available entry, and we prefer not to have it point to the saved FP 
  // frame slot which follows.  As in generate_fixed_frame, add one if 
  // profiling, for a temp save slot around VM calls.

  const int max_stack_words = 
    round_to((max_stack + 1 +
              (ProfileInterpreter ? WordsPerLong : 0)) *
         Interpreter::stackElementWords, 2 * WordsPerLong);
  return (max_stack_words + locals_size + monitor_size +
      frame::interpreter_frame_local_words +
      frame::frame_min_size);
}

// How much stack a method top interpreter activation needs in words. This
// is used only to determine if there is space available above the stack
// shadow pages for an interpreter activation from native code, and thus
// includes a call stub frame and an interpreter frame.
int AbstractInterpreter::size_top_interpreter_activation(Method* method) {

  int call_stub_frame_size = frame::entry_frame_local_size;

  // Save space for one monitor to get into the interpreted method in case
  // the method is synchronized
  int monitor_size    = method->is_synchronized() ?
                                1*frame::interpreter_frame_monitor_size() : 0;
  return size_activation_helper(method->max_locals(), method->max_stack(),
                                 monitor_size)  + call_stub_frame_size;
}

// Returns number of stackElementWords needed for the interpreter frame with the
// given sections.
// This overestimates the stack by one slot in case of alignments.
int AbstractInterpreter::size_activation(int max_stack,
                                         int temps,
                                         int extra_args,
                                         int monitors,
                                         int callee_params,
                                         int callee_locals,
                                         bool is_top_frame) {
  // Note: This calculation must exactly parallel the frame setup
  // in AbstractInterpreterGenerator::generate_method_entry.
  assert(Interpreter::stackElementWords == 1, "sanity");
  const int max_alignment_space = StackAlignmentInBytes / Interpreter::stackElementSize;
  const int abi_scratch = is_top_frame ? (frame::entry_frame_local_size / Interpreter::stackElementSize) :
                                         (frame::frame_min_size / Interpreter::stackElementSize);
  const int size =
    max_stack                                                +
    (callee_locals - callee_params)                          +
    monitors * frame::interpreter_frame_monitor_size()       +
    max_alignment_space                                      +
    abi_scratch                                              +
    frame::interpreter_frame_local_words / Interpreter::stackElementSize;

  // Fixed size of an interpreter frame, align to 16-byte.
  return (size & -2);
}

// Fills a sceletal interpreter frame generated during deoptimizations.
//
// Parameters:
//
// interpreter_frame != NULL:
//   set up the method, locals, and monitors.
//   The frame interpreter_frame, if not NULL, is guaranteed to be the
//   right size, as determined by a previous call to this method.
//   It is also guaranteed to be walkable even though it is in a skeletal state
//
// is_top_frame == true:
//   We're processing the *oldest* interpreter frame!
//
// pop_frame_extra_args:
//   If this is != 0 we are returning to a deoptimized frame by popping
//   off the callee frame. We want to re-execute the call that called the
//   callee interpreted, but since the return to the interpreter would pop
//   the arguments off advance the esp by dummy popframe_extra_args slots.
//   Popping off those will establish the stack layout as it was before the call.
//
void AbstractInterpreter::layout_activation(Method* method,
                                            int tempcount,
                                            int popframe_extra_args,
                                            int moncount,
                                            int caller_actual_parameters,
                                            int callee_param_count,
                                            int callee_locals_count,
                                            frame* caller,
                                            frame* interpreter_frame,
                                            bool is_top_frame,
                                            bool is_bottom_frame) {
  ShouldNotReachHere();
  /*

  const int abi_scratch = is_top_frame ? (frame::abi_reg_args_size / Interpreter::stackElementSize) :
                                         (frame::abi_minframe_size / Interpreter::stackElementSize);

  intptr_t* locals_base  = (caller->is_interpreted_frame()) ?
    caller->interpreter_frame_esp() + caller_actual_parameters :
    caller->sp() + method->max_locals() - 1 + (frame::abi_minframe_size / Interpreter::stackElementSize) ;

  intptr_t* monitor_base = caller->sp() - frame::ijava_state_size / Interpreter::stackElementSize ;
  intptr_t* monitor      = monitor_base - (moncount * frame::interpreter_frame_monitor_size());
  intptr_t* esp_base     = monitor - 1;
  intptr_t* esp          = esp_base - tempcount - popframe_extra_args;
  intptr_t* sp           = (intptr_t *) (((intptr_t) (esp_base - callee_locals_count + callee_param_count - method->max_stack()- abi_scratch)) & -StackAlignmentInBytes);
  intptr_t* sender_sp    = caller->sp() + (frame::abi_minframe_size - frame::abi_reg_args_size) / Interpreter::stackElementSize;
  intptr_t* top_frame_sp = is_top_frame ? sp : sp + (frame::abi_minframe_size - frame::abi_reg_args_size) / Interpreter::stackElementSize;

  interpreter_frame->interpreter_frame_set_method(method);
  interpreter_frame->interpreter_frame_set_locals(locals_base);
  interpreter_frame->interpreter_frame_set_cpcache(method->constants()->cache());
  interpreter_frame->interpreter_frame_set_esp(esp);
  interpreter_frame->interpreter_frame_set_monitor_end((BasicObjectLock *)monitor);
  interpreter_frame->interpreter_frame_set_top_frame_sp(top_frame_sp);
  if (!is_bottom_frame) {
    interpreter_frame->interpreter_frame_set_sender_sp(sender_sp);
  }
  */
}

// =============================================================================
// Exceptions

void TemplateInterpreterGenerator::generate_throw_exception() {
  // TODO: Implement
  Interpreter::_rethrow_exception_entry = __ pc();
  Interpreter::_throw_exception_entry = __ pc();
  Interpreter::_remove_activation_preserving_args_entry = __ pc();
  Interpreter::_remove_activation_entry = __ pc();
  __ unimplemented("exceptions can't be thrown yet, though I appreciate the effort of trying...");
  /*
  Register Rexception    = R17_tos,
           Rcontinuation = R3_RET;

  // --------------------------------------------------------------------------
  // Entry point if an method returns with a pending exception (rethrow).
  Interpreter::_rethrow_exception_entry = __ pc();
  {
    __ restore_interpreter_state(R11_scratch1); // Sets R11_scratch1 = fp.
    __ ld(R12_scratch2, _ijava_state_neg(top_frame_sp), R11_scratch1);
    __ resize_frame_absolute(R12_scratch2, R11_scratch1, R0);

    // Compiled code destroys templateTableBase, reload.
    __ load_const_optimized(R25_templateTableBase, (address)Interpreter::dispatch_table((TosState)0), R11_scratch1);
  }

  // Entry point if a interpreted method throws an exception (throw).
  Interpreter::_throw_exception_entry = __ pc();
  {
    __ mr(Rexception, R3_RET);

    __ verify_thread();
    __ verify_oop(Rexception);

    // Expression stack must be empty before entering the VM in case of an exception.
    __ empty_expression_stack();
    // Find exception handler address and preserve exception oop.
    // Call C routine to find handler and jump to it.
    __ call_VM(Rexception, CAST_FROM_FN_PTR(address, InterpreterRuntime::exception_handler_for_exception), Rexception);
    __ mtctr(Rcontinuation);
    // Push exception for exception handler bytecodes.
    __ push_ptr(Rexception);

    // Jump to exception handler (may be remove activation entry!).
    __ bctr();
  }

  // If the exception is not handled in the current frame the frame is
  // removed and the exception is rethrown (i.e. exception
  // continuation is _rethrow_exception).
  //
  // Note: At this point the bci is still the bxi for the instruction
  // which caused the exception and the expression stack is
  // empty. Thus, for any VM calls at this point, GC will find a legal
  // oop map (with empty expression stack).

  // In current activation
  // tos: exception
  // bcp: exception bcp

  // --------------------------------------------------------------------------
  // JVMTI PopFrame support

  Interpreter::_remove_activation_preserving_args_entry = __ pc();
  {
    // Set the popframe_processing bit in popframe_condition indicating that we are
    // currently handling popframe, so that call_VMs that may happen later do not
    // trigger new popframe handling cycles.
    __ lwz(R11_scratch1, in_bytes(JavaThread::popframe_condition_offset()), R16_thread);
    __ ori(R11_scratch1, R11_scratch1, JavaThread::popframe_processing_bit);
    __ stw(R11_scratch1, in_bytes(JavaThread::popframe_condition_offset()), R16_thread);

    // Empty the expression stack, as in normal exception handling.
    __ empty_expression_stack();
    __ unlock_if_synchronized_method(vtos, */ /* throw_monitor_exception */ /* false, */ /* install_monitor_exception */ /* false);

    // Check to see whether we are returning to a deoptimized frame.
    // (The PopFrame call ensures that the caller of the popped frame is
    // either interpreted or compiled and deoptimizes it if compiled.)
    // Note that we don't compare the return PC against the
    // deoptimization blob's unpack entry because of the presence of
    // adapter frames in C2.
    Label Lcaller_not_deoptimized;
    Register return_pc = R3_ARG1;
    __ ld(return_pc, 0, R1_SP);
    __ ld(return_pc, _abi(lr), return_pc);
    __ call_VM_leaf(CAST_FROM_FN_PTR(address, InterpreterRuntime::interpreter_contains), return_pc);
    __ cmpdi(CCR0, R3_RET, 0);
    __ bne(CCR0, Lcaller_not_deoptimized);

    // The deoptimized case.
    // In this case, we can't call dispatch_next() after the frame is
    // popped, but instead must save the incoming arguments and restore
    // them after deoptimization has occurred.
    __ ld(R4_ARG2, in_bytes(Method::const_offset()), R19_method);
    __ lhz(R4_ARG2 */ /* number of params */ /*, in_bytes(ConstMethod::size_of_parameters_offset()), R4_ARG2);
    __ slwi(R4_ARG2, R4_ARG2, Interpreter::logStackElementSize);
    __ addi(R5_ARG3, R18_locals, Interpreter::stackElementSize);
    __ subf(R5_ARG3, R4_ARG2, R5_ARG3);
    // Save these arguments.
    __ call_VM_leaf(CAST_FROM_FN_PTR(address, Deoptimization::popframe_preserve_args), R16_thread, R4_ARG2, R5_ARG3);

    // Inform deoptimization that it is responsible for restoring these arguments.
    __ load_const_optimized(R11_scratch1, JavaThread::popframe_force_deopt_reexecution_bit);
    __ stw(R11_scratch1, in_bytes(JavaThread::popframe_condition_offset()), R16_thread);

    // Return from the current method into the deoptimization blob. Will eventually
    // end up in the deopt interpeter entry, deoptimization prepared everything that
    // we will reexecute the call that called us.
    __ merge_frames(*/ /*top_frame_sp*/ /* R21_sender_SP, */ /*reload return_pc*/ /* return_pc, R11_scratch1, R12_scratch2);
    __ mtlr(return_pc);
    __ blr();

    // The non-deoptimized case.
    __ bind(Lcaller_not_deoptimized);

    // Clear the popframe condition flag.
    __ li(R0, 0);
    __ stw(R0, in_bytes(JavaThread::popframe_condition_offset()), R16_thread);

    // Get out of the current method and re-execute the call that called us.
    __ merge_frames(*/ /*top_frame_sp*/ /* R21_sender_SP, */ /*return_pc*/ /* noreg, R11_scratch1, R12_scratch2);
    __ restore_interpreter_state(R11_scratch1);
    __ ld(R12_scratch2, _ijava_state_neg(top_frame_sp), R11_scratch1);
    __ resize_frame_absolute(R12_scratch2, R11_scratch1, R0);
    if (ProfileInterpreter) {
      __ set_method_data_pointer_for_bcp();
      __ ld(R11_scratch1, 0, R1_SP);
      __ std(R28_mdx, _ijava_state_neg(mdx), R11_scratch1);
    }
#if INCLUDE_JVMTI
    Label L_done;

    __ lbz(R11_scratch1, 0, R14_bcp);
    __ cmpwi(CCR0, R11_scratch1, Bytecodes::_invokestatic);
    __ bne(CCR0, L_done);

    // The member name argument must be restored if _invokestatic is re-executed after a PopFrame call.
    // Detect such a case in the InterpreterRuntime function and return the member name argument, or NULL.
    __ ld(R4_ARG2, 0, R18_locals);
    __ MacroAssembler::call_VM(R4_ARG2, CAST_FROM_FN_PTR(address, InterpreterRuntime::member_name_arg_or_null), R4_ARG2, R19_method, R14_bcp, false);
    __ restore_interpreter_state(R11_scratch1, */ /*bcp_and_mdx_only*/ /* true);
    __ cmpdi(CCR0, R4_ARG2, 0);
    __ beq(CCR0, L_done);
    __ std(R4_ARG2, wordSize, R15_esp);
    __ bind(L_done);
#endif // INCLUDE_JVMTI
    __ dispatch_next(vtos);
  }
  // end of JVMTI PopFrame support

  // --------------------------------------------------------------------------
  // Remove activation exception entry.
  // This is jumped to if an interpreted method can't handle an exception itself
  // (we come from the throw/rethrow exception entry above). We're going to call
  // into the VM to find the exception handler in the caller, pop the current
  // frame and return the handler we calculated.
  Interpreter::_remove_activation_entry = __ pc();
  {
    __ pop_ptr(Rexception);
    __ verify_thread();
    __ verify_oop(Rexception);
    __ std(Rexception, in_bytes(JavaThread::vm_result_offset()), R16_thread);

    __ unlock_if_synchronized_method(vtos, */ /* throw_monitor_exception */ /* false, true);
    __ notify_method_exit(false, vtos, InterpreterMacroAssembler::SkipNotifyJVMTI, false);

    __ get_vm_result(Rexception);

    // We are done with this activation frame; find out where to go next.
    // The continuation point will be an exception handler, which expects
    // the following registers set up:
    //
    // RET:  exception oop
    // ARG2: Issuing PC (see generate_exception_blob()), only used if the caller is compiled.

    Register return_pc = R31; // Needs to survive the runtime call.
    __ ld(return_pc, 0, R1_SP);
    __ ld(return_pc, _abi(lr), return_pc);
    __ call_VM_leaf(CAST_FROM_FN_PTR(address, SharedRuntime::exception_handler_for_return_address), R16_thread, return_pc);

    // Remove the current activation.
    __ merge_frames(*/ /*top_frame_sp*/ /* R21_sender_SP, */ /*return_pc*/ /* noreg, R11_scratch1, R12_scratch2);

    __ mr(R4_ARG2, return_pc);
    __ mtlr(R3_RET);
    __ mr(R3_RET, Rexception);
    __ blr();
  }
  */
}

// JVMTI ForceEarlyReturn support.
// Returns "in the middle" of a method with a "fake" return value.
address TemplateInterpreterGenerator::generate_earlyret_entry_for(TosState state) {

  Register Rscratch1 = X6_T1,
           Rscratch2 = X7_T2;

  address entry = __ pc();
  __ unimplemented("generate_earlyret_entry_for");
  /*
  __ empty_expression_stack();

  __ load_earlyret_value(state, Rscratch1);

  __ ld(Rscratch1, in_bytes(JavaThread::jvmti_thread_state_offset()), X20_thread);
  // Clear the earlyret state.
  __ sw(XZERO, in_bytes(JvmtiThreadState::earlyret_state_offset()), Rscratch1);

  __ remove_activation(state, false, false);
  // Copied from TemplateTable::_return.
  // Restoration of lr done by remove_activation.
  switch (state) {
    case ltos:
    case btos:
    case ctos:
    case stos:
    case atos:
    case itos: __ mv(X10_RET0, X21_tos); break;
    case ftos: __ fmv_s(F10_RET0, F18_ftos); break;
    case dtos: __ fmv_d(F10_RET0, F18_ftos); break;
    case vtos: // This might be a constructor. Final fields (and volatile fields on RISCV64) need
               // to get visible before the reference to the object gets stored anywhere.
               __ fence(Assembler::Store, Assembler::Store); break;
    default  : ShouldNotReachHere();
  }
  __ jr(X1_RA);
  */
  return entry;
} // end of ForceEarlyReturn support

//-----------------------------------------------------------------------------
// Helper for vtos entry point generation

void TemplateInterpreterGenerator::set_vtos_entry_points(Template* t,
                                                         address& bep,
                                                         address& cep,
                                                         address& sep,
                                                         address& aep,
                                                         address& iep,
                                                         address& lep,
                                                         address& fep,
                                                         address& dep,
                                                         address& vep) {
  assert(t->is_valid() && t->tos_in() == vtos, "illegal template");
  Label L;

  aep = __ pc();  __ push_ptr();  __ j(L);
  fep = __ pc();  __ push_f();    __ j(L);
  dep = __ pc();  __ push_d();    __ j(L);
  lep = __ pc();  __ push_l();    __ j(L);
  bep = cep = sep = iep = __ pc();  __ push_i();
  vep = __ pc();
  __ bind(L);
  generate_and_dispatch(t);
}

//-----------------------------------------------------------------------------
// Generation of individual instructions

// helpers for generate_and_dispatch

InterpreterGenerator::InterpreterGenerator(StubQueue* code)
  : TemplateInterpreterGenerator(code) {
  generate_all(); // Down here so it can be "virtual".
}

//-----------------------------------------------------------------------------

// Non-product code
#ifndef PRODUCT
address TemplateInterpreterGenerator::generate_trace_code(TosState state) {
  //__ flush_bundle();
  address entry = __ pc();

  const char *bname = NULL;
  uint tsize = 0;
  switch(state) {
  case ftos:
    bname = "trace_code_ftos {";
    tsize = 2;
    break;
  case btos:
    bname = "trace_code_btos {";
    tsize = 2;
    break;
  case ctos:
    bname = "trace_code_ctos {";
    tsize = 2;
    break;
  case stos:
    bname = "trace_code_stos {";
    tsize = 2;
    break;
  case itos:
    bname = "trace_code_itos {";
    tsize = 2;
    break;
  case ltos:
    bname = "trace_code_ltos {";
    tsize = 3;
    break;
  case atos:
    bname = "trace_code_atos {";
    tsize = 2;
    break;
  case vtos:
    // Note: In case of vtos, the topmost of stack value could be a int or doubl
    // In case of a double (2 slots) we won't see the 2nd stack value.
    // Maybe we simply should print the topmost 3 stack slots to cope with the problem.
    bname = "trace_code_vtos {";
    tsize = 2;

    break;
  case dtos:
    bname = "trace_code_dtos {";
    tsize = 3;
    break;
  default:
    ShouldNotReachHere();
  }
  BLOCK_COMMENT(bname);

  // Support short-cut for TraceBytecodesAt.
  // Don't call into the VM if we don't want to trace to speed up things.
  Label Lskip_vm_call;
  if (TraceBytecodesAt > 0 && TraceBytecodesAt < max_intx) {
    int offs1 = __ load_vm_ptr(X6_T1, (address) &TraceBytecodesAt);
    int offs2 = __ load_vm_ptr(X7_T2, (address) &BytecodeCounter::_counter_value);
    __ ld(X6_T1, offs1, X6_T1);
    __ lw(X7_T2, offs2, X7_T2);
    __ blt(X7_T2, X6_T1, Lskip_vm_call);
  }

  __ push(state);
  // Load 2 topmost expression stack values.
  __ ld(X13_ARG3, tsize*Interpreter::stackElementSize, X19_esp);
  __ ld(X12_ARG2, Interpreter::stackElementSize, X19_esp);

  // store link register (ra)
  __ sd(X1_RA, -8, X2_SP);
  __ addi(X2_SP, X2_SP, -16);

  __ call_VM(noreg, CAST_FROM_FN_PTR(address, SharedRuntime::trace_bytecode), /* unused */ X11_ARG1, X12_ARG2, X13_ARG3, false);

  // load link register (ra)
  __ addi(X2_SP, X2_SP, 16);
  __ ld(X1_RA, -8, X2_SP);

  __ pop(state);

  if (TraceBytecodesAt > 0 && TraceBytecodesAt < max_intx) {
    __ bind(Lskip_vm_call);
  }
  __ jr(X1_RA);
  BLOCK_COMMENT("} trace_code");
  return entry;
}

void TemplateInterpreterGenerator::count_bytecode() {
  ShouldNotReachHere();
  /*
  int offs = __ load_const_optimized(R11_scratch1, (address) &BytecodeCounter::_counter_value, R12_scratch2, true);
  __ lwz(R12_scratch2, offs, R11_scratch1);
  __ addi(R12_scratch2, R12_scratch2, 1);
  __ stw(R12_scratch2, offs, R11_scratch1);
  */
}

void TemplateInterpreterGenerator::histogram_bytecode(Template* t) {
  ShouldNotReachHere();
  /*
  int offs = __ load_const_optimized(R11_scratch1, (address) &BytecodeHistogram::_counters[t->bytecode()], R12_scratch2, true);
  __ lwz(R12_scratch2, offs, R11_scratch1);
  __ addi(R12_scratch2, R12_scratch2, 1);
  __ stw(R12_scratch2, offs, R11_scratch1);
  */
}

void TemplateInterpreterGenerator::histogram_bytecode_pair(Template* t) {
  ShouldNotReachHere();
  /*
  const Register addr = R11_scratch1,
                 tmp  = R12_scratch2;
  // Get index, shift out old bytecode, bring in new bytecode, and store it.
  // _index = (_index >> log2_number_of_codes) |
  //          (bytecode << log2_number_of_codes);
  int offs1 = __ load_const_optimized(addr, (address)&BytecodePairHistogram::_index, tmp, true);
  __ lwz(tmp, offs1, addr);
  __ srwi(tmp, tmp, BytecodePairHistogram::log2_number_of_codes);
  __ ori(tmp, tmp, ((int) t->bytecode()) << BytecodePairHistogram::log2_number_of_codes);
  __ stw(tmp, offs1, addr);

  // Bump bucket contents.
  // _counters[_index] ++;
  int offs2 = __ load_const_optimized(addr, (address)&BytecodePairHistogram::_counters, R0, true);
  __ sldi(tmp, tmp, LogBytesPerInt);
  __ add(addr, tmp, addr);
  __ lwz(tmp, offs2, addr);
  __ addi(tmp, tmp, 1);
  __ stw(tmp, offs2, addr);
  */
}

void TemplateInterpreterGenerator::trace_bytecode(Template* t) {
  ShouldNotReachHere();
  /*
  // Call a little run-time stub to avoid blow-up for each bytecode.
  // The run-time runtime saves the right registers, depending on
  // the tosca in-state for the given template.

  assert(Interpreter::trace_code(t->tos_in()) != NULL,
         "entry must have been generated");

  // Note: we destroy LR here.
  __ bl(Interpreter::trace_code(t->tos_in()));
  */
}

void TemplateInterpreterGenerator::stop_interpreter_at() {
  ShouldNotReachHere();
  /*
  Label L;
  int offs1 = __ load_const_optimized(R11_scratch1, (address) &StopInterpreterAt, R0, true);
  int offs2 = __ load_const_optimized(R12_scratch2, (address) &BytecodeCounter::_counter_value, R0, true);
  __ ld(R11_scratch1, offs1, R11_scratch1);
  __ lwa(R12_scratch2, offs2, R12_scratch2);
  __ cmpd(CCR0, R12_scratch2, R11_scratch1);
  __ bne(CCR0, L);
  __ illtrap();
  __ bind(L);
  */
}

#endif // !PRODUCT
#endif // !CC_INTERP
