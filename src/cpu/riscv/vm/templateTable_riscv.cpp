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
#include "asm/macroAssembler.inline.hpp"
#include "interpreter/interpreter.hpp"
#include "interpreter/interpreterRuntime.hpp"
#include "interpreter/templateInterpreter.hpp"
#include "interpreter/templateTable.hpp"
#include "memory/universe.inline.hpp"
#include "oops/objArrayKlass.hpp"
#include "oops/oop.inline.hpp"
#include "prims/methodHandles.hpp"
#include "runtime/sharedRuntime.hpp"
#include "runtime/stubRoutines.hpp"
#include "runtime/synchronizer.hpp"
#include "utilities/macros.hpp"

#ifndef CC_INTERP

#define ___STRINGIFY2(x) #x
#define ___STRINGIFY(x) ___STRINGIFY2(x)
#define __LOCATION__ __FILE__ ":" ___STRINGIFY(__LINE__)

#undef __
#define __ _masm->

// ============================================================================
// Misc helpers

// Do an oop store like *(base + index) = val OR *(base + offset) = val
// (only one of both variants is possible at the same time).
// Index can be noreg.
// Kills:
//   Rbase, Rtmp
/*
static void do_oop_store(InterpreterMacroAssembler* _masm,
                         Register           Rbase,
                         RegisterOrConstant offset,
                         Register           Rval,         // Noreg means always null.
                         Register           Rtmp1,
                         Register           Rtmp2,
                         Register           Rtmp3,
                         BarrierSet::Name   barrier,
                         bool               precise,
                         bool               check_null) {
  __ unimplemented(__LOCATION__);
  assert_different_registers(Rtmp1, Rtmp2, Rtmp3, Rval, Rbase);

  switch (barrier) {
#if INCLUDE_ALL_GCS
    case BarrierSet::G1SATBCT:
    case BarrierSet::G1SATBCTLogging:
      {
        // Load and record the previous value.
        __ g1_write_barrier_pre(Rbase, offset,
                                Rtmp3, */ /* holder of pre_val ? */ /*
                                Rtmp1, Rtmp2, false */ /* frame */ /*);

        Label Lnull, Ldone;
        if (Rval != noreg) {
          if (check_null) {
            __ beq(Rval, X0_ZERO, Lnull);
          }
          __ store_heap_oop_not_null(Rval, offset, Rbase, */ /*Rval must stay uncompressed.*/ /* Rtmp1);
          // Mark the card.
          if (!(offset.is_constant() && offset.as_constant() == 0) && precise) {
            __ add(Rbase, offset, Rbase);
          }
          __ g1_write_barrier_post(Rbase, Rval, Rtmp1, Rtmp2, Rtmp3, */ /*filtered (fast path)*/ /* &Ldone);
          if (check_null) { __ b(Ldone); }
        }

        if (Rval == noreg || check_null) { // Store null oop.
          Register Rnull = Rval;
          __ bind(Lnull);
          if (Rval == noreg) {
            Rnull = Rtmp1;
            __ li(Rnull, 0);
          }
          if (UseCompressedOops) {
            __ sw(Rnull, offset, Rbase);
          } else {
            __ sd(Rnull, offset, Rbase);
          }
        }
        __ bind(Ldone);
      }
      break;
#endif // INCLUDE_ALL_GCS
    case BarrierSet::CardTableModRef:
    case BarrierSet::CardTableExtension:
      {
        Label Lnull, Ldone;
        if (Rval != noreg) {
          if (check_null) {
            __ beq(Rval, X0_ZERO, Lnull);
          }
          __ store_heap_oop_not_null(Rval, offset, Rbase, */ /*Rval should better stay uncompressed.*/ /* Rtmp1);
          // Mark the card.
          if (!(offset.is_constant() && offset.as_constant() == 0) && precise) {
            __ add(Rbase, offset, Rbase);
          }
          __ card_write_barrier_post(Rbase, Rval, Rtmp1);
          if (check_null) {
            __ j(Ldone);
          }
        }

        if (Rval == noreg || check_null) { // Store null oop.
          Register Rnull = Rval;
          __ bind(Lnull);
          if (Rval == noreg) {
            Rnull = Rtmp1;
            __ li(Rnull, 0);
          }
          if (UseCompressedOops) {
            __ sw(Rnull, offset, Rbase);
          } else {
            __ sd(Rnull, offset, Rbase);
          }
        }
        __ bind(Ldone);
      }
      break;
    case BarrierSet::ModRef:
    case BarrierSet::Other:
      ShouldNotReachHere();
      break;
    default:
      ShouldNotReachHere();
  }
}
*/

// ============================================================================
// Platform-dependent initialization

void TemplateTable::pd_initialize() {
  // No riscv-specific initialization.
}

Address TemplateTable::at_bcp(int offset) {
  // Not used on riscv.
  ShouldNotReachHere();
  return Address();
}

// Patches the current bytecode (ptr to it located in bcp)
// in the bytecode stream with a new one.
void TemplateTable::patch_bytecode(Bytecodes::Code new_bc, Register Rnew_bc, Register Rtemp, bool load_bc_into_bc_reg /*=true*/, int byte_no) {
  __ unimplemented(__LOCATION__);
  /*
  // With sharing on, may need to test method flag.
  if (!RewriteBytecodes) return;
  Label L_patch_done;

  switch (new_bc) {
    case Bytecodes::_fast_aputfield:
    case Bytecodes::_fast_bputfield:
    case Bytecodes::_fast_cputfield:
    case Bytecodes::_fast_dputfield:
    case Bytecodes::_fast_fputfield:
    case Bytecodes::_fast_iputfield:
    case Bytecodes::_fast_lputfield:
    case Bytecodes::_fast_sputfield:
    {
      // We skip bytecode quickening for putfield instructions when
      // the put_code written to the constant pool cache is zero.
      // This is required so that every execution of this instruction
      // calls out to InterpreterRuntime::resolve_get_put to do
      // additional, required work.
      assert(byte_no == f1_byte || byte_no == f2_byte, "byte_no out of range");
      assert(load_bc_into_bc_reg, "we use bc_reg as temp");
      __ get_cache_and_index_at_bcp(Rtemp */ /* dst = cache */ /*, 1);
      // ((*(cache+indices))>>((1+byte_no)*8))&0xFF:
#if defined(VM_LITTLE_ENDIAN)
      __ lbu(Rnew_bc, in_bytes(ConstantPoolCache::base_offset() + ConstantPoolCacheEntry::indices_offset()) + 1 + byte_no, Rtemp);
#else
      __ lbu(Rnew_bc, in_bytes(ConstantPoolCache::base_offset() + ConstantPoolCacheEntry::indices_offset()) + 7 - (1 + byte_no), Rtemp);
#endif
      __ cmpwi(CCR0, Rnew_bc, 0);
      __ li(Rnew_bc, (unsigned int)(unsigned char)new_bc);
      __ beq(CCR0, L_patch_done);
      // __ isync(); // acquire not needed
      break;
    }

    default:
      assert(byte_no == -1, "sanity");
      if (load_bc_into_bc_reg) {
        __ li(Rnew_bc, (unsigned int)(unsigned char)new_bc);
      }
  }

  if (JvmtiExport::can_post_breakpoint()) {
    Label L_fast_patch;
    __ lbu(Rtemp, 0, R14_bcp);
    __ cmpwi(CCR0, Rtemp, (unsigned int)(unsigned char)Bytecodes::_breakpoint);
    __ bne(CCR0, L_fast_patch);
    // Perform the quickening, slowly, in the bowels of the breakpoint table.
    __ call_VM(noreg, CAST_FROM_FN_PTR(address, InterpreterRuntime::set_original_bytecode_at), R19_method, R14_bcp, Rnew_bc);
    __ j(L_patch_done);
    __ bind(L_fast_patch);
  }

  // Patch bytecode.
  __ sb(Rnew_bc, 0, R14_bcp);

  __ bind(L_patch_done);
  */
}


// ============================================================================
// Individual instructions

void TemplateTable::nop() {
  transition(vtos, vtos);
  // Nothing to do.
}

void TemplateTable::shouldnotreachhere() {
  transition(vtos, vtos);
  __ stop("shouldnotreachhere bytecode");
}

void TemplateTable::aconst_null() {
  transition(vtos, atos);
  __ bytecode_marker(0x01);
  __ li(X21_tos, 0);
}

void TemplateTable::iconst(int value) {
  transition(vtos, itos);
  assert(value >= -1 && value <= 5, "");
  __ bytecode_marker(0x03 + value);
  __ li(X21_tos, value);
}

void TemplateTable::lconst(int value) {
  transition(vtos, ltos);
  assert(value >= -1 && value <= 5, "");
  __ bytecode_marker(0x09);
  __ li(X21_tos, value);
}

void TemplateTable::fconst(int value) {
  transition(vtos, ftos);
  if (value < 0 || value > 2) ShouldNotReachHere();
  __ bytecode_marker(0x0c + value);
  __ addi(X5_T0, X0, value);
  __ fcvt_s_w(F18_ftos, X5_T0, 0);
}

void TemplateTable::dconst(int value) {
  transition(vtos, dtos);
  if (value < 0 || value > 2) ShouldNotReachHere();
  __ bytecode_marker(0x0e + value);
  __ addi(X5_T0, X0, value);
  __ fcvt_d_w(F18_ftos, X5_T0, 0);
}

void TemplateTable::bipush() {
  transition(vtos, itos);
  __ bytecode_marker(0x10);
  __ lb(X21_tos, 1, X18_bcp);
}

void TemplateTable::sipush() {
  transition(vtos, itos);
  __ bytecode_marker(0x11);
  __ lh(X21_tos, 1, X18_bcp);
}

void TemplateTable::ldc(bool wide) {
  __ bytecode_marker(0x12 + (int) wide);
  __ unimplemented("ldc not implemented");
  /*
  Register Rscratch1 = X5_T0,
           Rscratch2 = X6_T1,
           Rscratch3 = X7_T2,
           Rcpool    = X10_ARG0;

  transition(vtos, vtos);
  Label notInt, notClass, exit;

  __ get_cpool_and_tags(Rcpool, Rscratch2); // Set Rscratch2 = &tags.
  if (wide) { // Read index.
    __ get_2_byte_integer_at_bcp(1, Rscratch1, InterpreterMacroAssembler::Unsigned);
  } else {
    __ lbu(Rscratch1, 1, X18_bcp);
  }

  const int base_offset = ConstantPool::header_size() * wordSize;
  const int tags_offset = Array<u1>::base_offset_in_bytes();

  // Get type from tags.
  __ addi(Rscratch2, Rscratch2, tags_offset);
  __ add(Rscratch2, Rscratch2, Rscratch1);
  __ lbu(Rscratch2, 0, Rscratch2);

  __ li(Rscratch3, (int) JVM_CONSTANT_UnresolvedClass);
  __ bne(Rscratch2, Rscratch3, notClass); // Not an unresolved class?

  __ li(Rscratch3, (int) JVM_CONSTANT_UnresolvedClassInError);
  __ bne(Rscratch2, Rscratch3, notClass); // Not an unresolved class in error state?

  __ li(Rscratch3, (int) JVM_CONSTANT_Class);
  __ bne(Rscratch2, Rscratch3, notClass); // Not a regular class?

  // Need to call vm to get java mirror of the class.
  __ ori(X11, wide ? 1 : 0);
  call_VM(X21_tos, CAST_FROM_FN_PTR(address, InterpreterRuntime::ldc), X11);
  __ push(atos);
  __ j(exit);

  __ bind(notClass);
  __ addi(Rcpool, Rcpool, base_offset);
  __ slli(Rscratch1, Rscratch1, LogBytesPerWord);
  __ li(Rscratch3, (int) JVM_CONSTANT_Integer);
  __ bne(Rscratch2, Rscratch3, notInt);

  __ add(Rscratch1, Rscratch1, Rcpool);
  __ lw(X21_tos, 0, Rscratch1);
  __ push(itos);
  __ j(exit);

  __ bind(notInt);
#ifdef ASSERT
  // String and Object are rewritten to fast_aldc
  __ li(Rscratch3, (int) JVM_CONSTANT_Float);
  __ asm_assert_eq("unexpected type", 0x8765);
#endif
  __ add(Rcpool, Rcpool, Rscratch1);
  __ flw(F18_ftos, 0, Rcpool);
  __ push(ftos);

  __ bind(exit);
  */
}

// Fast path for caching oop constants.
void TemplateTable::fast_aldc(bool wide) {
  __ unimplemented("fast_adlc not implemented");
  /*
  transition(vtos, atos);

  int index_size = wide ? sizeof(u2) : sizeof(u1);
  const Register Rscratch = X5_T0;
  Label resolved;

  // We are resolved if the resolved reference cache entry contains a
  // non-null object (CallSite, etc.)
  __ get_cache_index_at_bcp(Rscratch, 1, index_size);  // Load index.
  __ load_resolved_reference_at_index(X21_tos, Rscratch);
  __ bne(X21_tos, X0_ZER0, resolved);
  __ li(X10_ARG0, (int) bytecode());

  address entry = CAST_FROM_FN_PTR(address, InterpreterRuntime::resolve_ldc);

  // First time invocation - must resolve first.
  __ call_VM(X21_tos, entry, X10_ARG0);

  __ bind(resolved);
  __ verify_oop(X21_tos);
  */
}

void TemplateTable::ldc2_w() {
  __ bytecode_marker(0x13);
  __ unimplemented("ldc2_w not implemented");
  /*
  transition(vtos, vtos);
  Label Llong, Lexit;

  Register Rindex = X5_T0,
           Rcpool = X6_T1,
           Rtag   = X10_ARG0,
           Rscratch = X7_T2;
  __ get_cpool_and_tags(Rcpool, Rtag);
  __ get_2_byte_integer_at_bcp(1, Rindex, InterpreterMacroAssembler::Unsigned);

  const int base_offset = ConstantPool::header_size() * wordSize;
  const int tags_offset = Array<u1>::base_offset_in_bytes();
  // Get type from tags.
  __ addi(Rcpool, Rcpool, base_offset);
  __ addi(Rtag, Rtag, tags_offset);

  __ add(Rtag, Rtag, Rindex);
  __ lbu(Rtag, 0, Rtag);

  __ slli(Rindex, Rindex, LogBytesPerWord);
  __ li(Rscratch, JVM_CONSTANT_Double);
  __ bne(Rtag, Rscratch, Llong);
  // A double can be placed at word-aligned locations in the constant pool.
  // Check out Conversions.java for an example.
  // Also ConstantPool::header_size() is 20, which makes it very difficult
  // to double-align double on the constant pool. SG, 11/7/97
  __ add(F18_ftos, Rcpool, Rindex);
  __ fld(F18_ftos, 0, F18_ftos);
  __ push(dtos);
  __ j(Lexit);

  __ bind(Llong);
  __ add(X21_tos, Rcpool, Rindex);
  __ ld(X21_tos, 0, X21_tos);
  __ push(ltos);

  __ bind(Lexit);
  */
}

// Get the locals index located in the bytecode stream at bcp + offset.
void TemplateTable::locals_index(Register Rdst, int offset) {
  __ lbu(Rdst, offset, X18_bcp);
}

void TemplateTable::iload() {
  transition(vtos, itos);
  __ bytecode_marker(0x15);

  // Get the local value into tos
  const Register Rindex = X25_tmp2;
  locals_index(Rindex);

  // Rewrite iload,iload  pair into fast_iload2
  //         iload,caload pair into fast_icaload
  /*
  if (RewriteFrequentPairs) {
    Label Lrewrite, Ldone;
    Register Rnext_byte  = X10_ARG0,
             Rrewrite_to = X11_ARG1,
             Rscratch    = X5_T0;

    // get next byte
    __ lbu(Rnext_byte, Bytecodes::length_for(Bytecodes::_iload), X18_bcp);

    // if _iload, wait to rewrite to iload2. We only want to rewrite the
    // last two iloads in a pair. Comparing against fast_iload means that
    // the next bytecode is neither an iload or a caload, and therefore
    // an iload pair.
    __ li(Rscratch, (unsigned int)(unsigned char)Bytecodes::_iload);
    __ beq(Rnext_byte, Rscratch, Ldone);

    __ li(Rscratch, (unsigned int)(unsigned char)Bytecodes::_fast_iload);
    __ li(Rrewrite_to, (unsigned int)(unsigned char)Bytecodes::_fast_iload2);
    __ beq(Rnext_byte, Rscratch, Lrewrite);

    __ li(Rscratch, (unsigned int)(unsigned char)Bytecodes::_caload);
    __ li(Rrewrite_to, (unsigned int)(unsigned char)Bytecodes::_fast_icaload);
    __ beq(Rnext_byte, Rscratch, Lrewrite);

    __ li(Rrewrite_to, (unsigned int)(unsigned char)Bytecodes::_fast_iload);

    __ bind(Lrewrite);
    patch_bytecode(Bytecodes::_iload, Rrewrite_to, Rscratch, false);
    __ bind(Ldone);
  }
  */

  __ load_local_int(X21_tos, Rindex, Rindex);
}

// Load 2 integers in a row without dispatching
void TemplateTable::fast_iload2() {
  __ unimplemented("fast_iload2 __ unimplemented");
  /*
  transition(vtos, itos);

  __ lbu(X10_ARG0, 1, X18_bcp);
  __ lbu(X21_tos, Bytecodes::length_for(Bytecodes::_iload) + 1, X18_bcp);

  __ load_local_int(X10_ARG0, X5_T0, X10_ARG0);
  __ load_local_int(X21_tos, X6_T1, X21_tos);
  __ push_i(X10_ARG0);
  */
}

void TemplateTable::fast_iload() {
  __ unimplemented("fast_iload __ unimplemented");
  /*
  transition(vtos, itos);
  // Get the local value into tos

  const Register Rindex = X5_T0;
  locals_index(Rindex);
  __ load_local_int(X21_tos, Rindex, Rindex);
  */
}

// Load a local variable type long from locals area to TOS cache register.
// Local index resides in bytecodestream.
void TemplateTable::lload() {
  transition(vtos, ltos);
  __ bytecode_marker(0x16);

  const Register Rindex = X5_T0;
  locals_index(Rindex);
  __ load_local_long(X21_tos, Rindex, Rindex);
}

void TemplateTable::fload() {
  transition(vtos, ftos);
  __ bytecode_marker(0x17);

  const Register Rindex = X5_T0;
  locals_index(Rindex);
  __ load_local_float(F18_ftos, Rindex, Rindex);
}

void TemplateTable::dload() {
  transition(vtos, dtos);
  __ bytecode_marker(0x18);

  const Register Rindex = X5_T0;
  locals_index(Rindex);
  __ load_local_double(F18_ftos, Rindex, Rindex);
}

void TemplateTable::aload() {
  __ bytecode_marker(0x19);
  __ unimplemented("aload not implemented");
  /*
  transition(vtos, atos);

  const Register Rindex = X5_T0;
  locals_index(Rindex);
  __ load_local_ptr(X21_tos, Rindex, Rindex);
  */
}

void TemplateTable::locals_index_wide(Register Rdst) {
  // Offset is 2, not 1, because Lbcp points to wide prefix code.
  __ get_2_byte_integer_at_bcp(2, XJUNK, Rdst, InterpreterMacroAssembler::Unsigned);
}

void TemplateTable::wide_iload() {
  // Get the local value into tos.

  __ bytecode_marker(0x15);
  const Register Rindex = X5_T0;
  locals_index_wide(Rindex);
  __ load_local_int(X21_tos, Rindex, Rindex);
}

void TemplateTable::wide_lload() {
  transition(vtos, ltos);
  __ bytecode_marker(0x16);

  const Register Rindex = X5_T0;
  locals_index_wide(Rindex);
  __ load_local_long(X21_tos, Rindex, Rindex);
}

void TemplateTable::wide_fload() {
  transition(vtos, ftos);
  __ bytecode_marker(0x17);

  const Register Rindex = X5_T0;
  locals_index_wide(Rindex);
  __ load_local_float(F18_ftos, Rindex, Rindex);
}

void TemplateTable::wide_dload() {
  transition(vtos, dtos);
  __ bytecode_marker(0x18);

  const Register Rindex = X5_T0;
  locals_index_wide(Rindex);
  __ load_local_double(F18_ftos, Rindex, Rindex);
}

void TemplateTable::wide_aload() {
  __ bytecode_marker(0x19);
  __ unimplemented(__LOCATION__);
  /*
  transition(vtos, atos);

  const Register Rindex = X5_T0;
  locals_index_wide(Rindex);
  __ load_local_ptr(X21_tos, Rindex, Rindex);
  */
}

void TemplateTable::iaload() {
  __ bytecode_marker(0x2e);
  __ unimplemented(__LOCATION__);
  /*
  transition(itos, itos);

  const Register Rload_addr = X10_ARG0,
                 Rarray     = X11_ARG1,
                 Rtemp      = X12_ARG2;
  __ index_check(Rarray, X21_tos */ /* index */ /*, LogBytesPerInt, Rtemp, Rload_addr);
  __ lw(X21_tos, arrayOopDesc::base_offset_in_bytes(T_INT), Rload_addr);
  */
}

void TemplateTable::laload() {
  __ bytecode_marker(0x2f);
  __ unimplemented(__LOCATION__);
  /*
  transition(itos, ltos);

  const Register Rload_addr = X10_ARG0,
                 Rarray     = X11_ARG1,
                 Rtemp      = X12_ARG2;
  __ index_check(Rarray, X21_tos */ /* index */ /*, LogBytesPerLong, Rtemp, Rload_addr);
  __ ld(X21_tos, arrayOopDesc::base_offset_in_bytes(T_LONG), Rload_addr);
  */
}

void TemplateTable::faload() {
  __ bytecode_marker(0x30);
  __ unimplemented(__LOCATION__);
  /*
  transition(itos, ftos);

  const Register Rload_addr = X10_ARG0,
                 Rarray     = X11_ARG1,
                 Rtemp      = X12_ARG2;
  __ index_check(Rarray, X21_tos */ /* index */ /*, LogBytesPerInt, Rtemp, Rload_addr);
  __ flw(F18_ftos, arrayOopDesc::base_offset_in_bytes(T_FLOAT), Rload_addr);
  */
}

void TemplateTable::daload() {
  __ bytecode_marker(0x31);
  __ unimplemented(__LOCATION__);
  /*
  transition(itos, dtos);

  const Register Rload_addr = X10_ARG0,
                 Rarray     = X11_ARG1,
                 Rtemp      = X12_ARG2;
  __ index_check(Rarray, X21_tos */ /* index */ /*, LogBytesPerLong, Rtemp, Rload_addr);
  __ fld(F18_ftos, arrayOopDesc::base_offset_in_bytes(T_DOUBLE), Rload_addr);
  */
}

void TemplateTable::aaload() {
  __ bytecode_marker(0x32);
  __ unimplemented(__LOCATION__);
  /*
  transition(itos, atos);

  // tos: index
  // result tos: array
  const Register Rload_addr = X10_ARG0,
                 Rarray     = X11_ARG1,
                 Rtemp      = X12_ARG2;
  __ index_check(Rarray, X21_tos */ /* index */ /*, UseCompressedOops ? 2 : LogBytesPerWord, Rtemp, Rload_addr);
  __ load_heap_oop(X21_tos, arrayOopDesc::base_offset_in_bytes(T_OBJECT), Rload_addr);
  __ verify_oop(X21_tos);
  //__ dcbt(X21_tos); // prefetch
  */
}

void TemplateTable::baload() {
  __ bytecode_marker(0x33);
  __ unimplemented(__LOCATION__);
  /*
  transition(itos, itos);

  const Register Rload_addr = X10_ARG0,
                 Rarray     = X11_ARG1,
                 Rtemp      = X12_ARG2;
  __ index_check(Rarray, X21_tos */ /* index */ /*, 0, Rtemp, Rload_addr);
  __ lb(X21_tos, arrayOopDesc::base_offset_in_bytes(T_BYTE), Rload_addr);
  */
}

void TemplateTable::caload() {
  __ bytecode_marker(0x34);
  __ unimplemented(__LOCATION__);
  /*
  transition(itos, itos);

  const Register Rload_addr = X10_ARG0,
                 Rarray     = X11_ARG1,
                 Rtemp      = X12_ARG2;
  __ index_check(Rarray, X21_tos */ /* index */ /*, LogBytesPerShort, Rtemp, Rload_addr);
  __ lhu(X21_tos, arrayOopDesc::base_offset_in_bytes(T_CHAR), Rload_addr);
  */
}

// Iload followed by caload frequent pair.
void TemplateTable::fast_icaload() {
  __ unimplemented(__LOCATION__);
  /*
  transition(vtos, itos);

  const Register Rload_addr = X10_ARG0,
                 Rarray     = X11_ARG1,
                 Rtemp      = X5_T0;

  locals_index(X21_tos);
  __ load_local_int(X21_tos, Rtemp, X21_tos);
  __ index_check(Rarray, X21_tos */ /* index */ /*, LogBytesPerShort, Rtemp, Rload_addr);
  __ lhu(X21_tos, arrayOopDesc::base_offset_in_bytes(T_CHAR), Rload_addr);
  */
}

void TemplateTable::saload() {
  __ bytecode_marker(0x35);
  __ unimplemented(__LOCATION__);
  /*
  transition(itos, itos);

  const Register Rload_addr = X5_T0,
                 Rarray     = X6_T1,
                 Rtemp      = X10_ARG0;
  __ index_check(Rarray, X21_tos */ /* index */ /*, LogBytesPerShort, Rtemp, Rload_addr);
  __ lh(X21_tos, arrayOopDesc::base_offset_in_bytes(T_SHORT), Rload_addr);
  */
}

void TemplateTable::iload(int n) {
  transition(vtos, itos);
  __ bytecode_marker(0x1a);

  __ lwu(X21_tos, Interpreter::local_offset_in_bytes(n), X22_locals);
}

void TemplateTable::lload(int n) {
  transition(vtos, ltos);
  __ bytecode_marker(0x1e);

  __ ld(X21_tos, Interpreter::local_offset_in_bytes(n + 1), X22_locals);
}

void TemplateTable::fload(int n) {
  transition(vtos, ftos);
  __ bytecode_marker(0x22);

  __ flw(F18_ftos, Interpreter::local_offset_in_bytes(n), X22_locals);
}

void TemplateTable::dload(int n) {
  transition(vtos, dtos);
  __ bytecode_marker(0x26);

  __ fld(F18_ftos, Interpreter::local_offset_in_bytes(n + 1), X22_locals);
}

void TemplateTable::aload(int n) {
  __ bytecode_marker(0x2a);
  __ unimplemented(__LOCATION__);
  /*
  transition(vtos, atos);

  __ ld(X21_tos, Interpreter::local_offset_in_bytes(n), X22_locals);
  */
}

void TemplateTable::aload_0() {
  __ bytecode_marker(0x2a);
  __ unimplemented(__LOCATION__);
  /*
  transition(vtos, atos);
  // According to bytecode histograms, the pairs:
  //
  // _aload_0, _fast_igetfield
  // _aload_0, _fast_agetfield
  // _aload_0, _fast_fgetfield
  //
  // occur frequently. If RewriteFrequentPairs is set, the (slow)
  // _aload_0 bytecode checks if the next bytecode is either
  // _fast_igetfield, _fast_agetfield or _fast_fgetfield and then
  // rewrites the current bytecode into a pair bytecode; otherwise it
  // rewrites the current bytecode into _0 that doesn't do
  // the pair check anymore.
  //
  // Note: If the next bytecode is _getfield, the rewrite must be
  //       delayed, otherwise we may miss an opportunity for a pair.
  //
  // Also rewrite frequent pairs
  //   aload_0, aload_1
  //   aload_0, iload_1
  // These bytecodes with a small amount of code are most profitable
  // to rewrite.

  if (RewriteFrequentPairs) {

    Label Lrewrite, Ldont_rewrite;
    Register Rnext_byte  = X10_ARG0,
             Rrewrite_to = X13_ARG3,
             Rscratch    = X5_T0;

    // Get next byte.
    __ lbu(Rnext_byte, Bytecodes::length_for(Bytecodes::_aload_0), X18_bcp);

    // If _getfield, wait to rewrite. We only want to rewrite the last two bytecodes in a pair.
    __ li(Rscratch, (unsigned int)(unsigned char)Bytecodes::_getfield);
    __ beq(Rnext_byte, Rscratch, Ldont_rewrite);

    __ li(Rscratch, (unsigned int)(unsigned char)Bytecodes::_fast_igetfield);
    __ li(Rrewrite_to, (unsigned int)(unsigned char)Bytecodes::_fast_iaccess_0);
    __ beq(Rnext_byte, Rscratch, Lrewrite);

    __ li(Rscratch, (unsigned int)(unsigned char)Bytecodes::_fast_agetfield);
    __ li(Rrewrite_to, (unsigned int)(unsigned char)Bytecodes::_fast_aaccess_0);
    __ beq(Rnext_byte, Rscratch, Lrewrite);

    __ li(Rscratch, (unsigned int)(unsigned char)Bytecodes::_fast_fgetfield);
    __ li(Rrewrite_to, (unsigned int)(unsigned char)Bytecodes::_fast_faccess_0);
    __ beq(Rnext_byte, Rscratch, Lrewrite);

    __ li(Rrewrite_to, (unsigned int)(unsigned char)Bytecodes::_fast_aload_0);

    __ bind(Lrewrite);
    patch_bytecode(Bytecodes::_aload_0, Rrewrite_to, Rscratch, false);
    __ bind(Ldont_rewrite);
  }

  // Do actual aload_0 (must do this after patch_bytecode which might call VM and GC might change oop).
  aload(0);
  */
}

void TemplateTable::istore() {
  transition(itos, vtos);
  __ bytecode_marker(0x36);

  const Register Rindex = X5_T0;
  locals_index(Rindex);
  __ store_local_int(X21_tos, Rindex);
}

void TemplateTable::lstore() {
  transition(ltos, vtos);
  __ bytecode_marker(0x37);
  const Register Rindex = X5_T0;
  locals_index(Rindex);
  __ store_local_long(X21_tos, Rindex);
}

void TemplateTable::fstore() {
  transition(ftos, vtos);
  __ bytecode_marker(0x38);

  const Register Rindex = X5_T0;
  locals_index(Rindex);
  __ store_local_float(F18_ftos, Rindex);
}

void TemplateTable::dstore() {
  transition(dtos, vtos);
  __ bytecode_marker(0x39);

  const Register Rindex = X5_T0;
  locals_index(Rindex);
  __ store_local_double(F18_ftos, Rindex);
}

void TemplateTable::astore() {
  __ bytecode_marker(0x3a);
  __ unimplemented(__LOCATION__);
  /*
  transition(vtos, vtos);

  const Register Rindex = X5_T0;
  __ pop_ptr();
  __ verify_oop_or_return_address(X21_tos, Rindex);
  locals_index(Rindex);
  __ store_local_ptr(X21_tos, Rindex);
  */
}

void TemplateTable::wide_istore() {
  transition(vtos, vtos);
  __ bytecode_marker(0x36);

  const Register Rindex = X5_T0;
  __ pop_i();
  locals_index_wide(Rindex);
  __ store_local_int(X21_tos, Rindex);
}

void TemplateTable::wide_lstore() {
  transition(vtos, vtos);
  __ bytecode_marker(0x37);

  const Register Rindex = X5_T0;
  __ pop_l();
  locals_index_wide(Rindex);
  __ store_local_long(X21_tos, Rindex);
}

void TemplateTable::wide_fstore() {
  transition(vtos, vtos);
  __ bytecode_marker(0x38);

  const Register Rindex = X5_T0;
  __ pop_f();
  locals_index_wide(Rindex);
  __ store_local_float(F18_ftos, Rindex);
}

void TemplateTable::wide_dstore() {
  transition(vtos, vtos);
  __ bytecode_marker(0x39);

  const Register Rindex = X5_T0;
  __ pop_d();
  locals_index_wide(Rindex);
  __ store_local_double(F18_ftos, Rindex);
}

void TemplateTable::wide_astore() {
  __ bytecode_marker(0x3a);
  __ unimplemented(__LOCATION__);
  /*
  transition(vtos, vtos);

  const Register Rindex = X5_T0;
  __ pop_ptr();
  __ verify_oop_or_return_address(X21_tos, Rindex);
  locals_index_wide(Rindex);
  __ store_local_ptr(X21_tos, Rindex);
  */
}

void TemplateTable::iastore() {
  __ bytecode_marker(0x4f);
  __ unimplemented(__LOCATION__);
  /*
  transition(itos, vtos);

  const Register Rindex      = X10_ARG0,
                 Rstore_addr = X11_ARG1,
                 Rarray      = X12_ARG2,
                 Rtemp       = X13_ARG3;
  __ pop_i(Rindex);
  __ index_check(Rarray, Rindex, LogBytesPerInt, Rtemp, Rstore_addr);
  __ sw(X21_tos, arrayOopDesc::base_offset_in_bytes(T_INT), Rstore_addr);
  */
}

void TemplateTable::lastore() {
  __ bytecode_marker(0x50);
  __ unimplemented(__LOCATION__);
  /*
  transition(ltos, vtos);

  const Register Rindex      = X10_ARG0,
                 Rstore_addr = X11_ARG1,
                 Rarray      = X12_ARG2,
                 Rtemp       = X13_ARG3;
  __ pop_i(Rindex);
  __ index_check(Rarray, Rindex, LogBytesPerLong, Rtemp, Rstore_addr);
  __ sd(X21_tos, arrayOopDesc::base_offset_in_bytes(T_LONG), Rstore_addr);
  */
}

void TemplateTable::fastore() {
  __ bytecode_marker(0x51);
  __ unimplemented(__LOCATION__);
  /*
  transition(ftos, vtos);

  const Register Rindex      = X10_ARG0,
                 Rstore_addr = X11_ARG1,
                 Rarray      = X12_ARG2,
                 Rtemp       = X13_ARG3;
  __ pop_i(Rindex);
  __ index_check(Rarray, Rindex, LogBytesPerInt, Rtemp, Rstore_addr);
  __ fsw(F18_ftos, arrayOopDesc::base_offset_in_bytes(T_FLOAT), Rstore_addr);
  */
}

void TemplateTable::dastore() {
  __ bytecode_marker(0x52);
  __ unimplemented(__LOCATION__);
  /*
  transition(dtos, vtos);

  const Register Rindex      = X10_ARG0,
                 Rstore_addr = X11_ARG1,
                 Rarray      = X12_ARG2,
                 Rtemp       = X13_ARG3;
  __ pop_i(Rindex);
  __ index_check(Rarray, Rindex, LogBytesPerLong, Rtemp, Rstore_addr);
  __ fsd(F18_ftos, arrayOopDesc::base_offset_in_bytes(T_DOUBLE), Rstore_addr);
  */
}

// Pop 3 values from the stack and...
void TemplateTable::aastore() {
  __ bytecode_marker(0x53);
  __ unimplemented(__LOCATION__);
  /*
  transition(vtos, vtos);

  Label Lstore_ok, Lis_null, Ldone;
  const Register Rindex    = X10_ARG0,
                 Rarray    = X11_ARG1,
                 Rscratch  = X5_T0,
                 Rscratch2 = X6_T1,
                 Rarray_klass = X12_ARG2,
                 Rarray_element_klass = Rarray_klass,
                 Rvalue_klass = X13_ARG3,
                 Rstore_addr = X26_tmp3;    // Use register which survives VM call.

  __ ld(X21_tos, Interpreter::expr_offset_in_bytes(0), X19_esp); // Get value to store.
  __ lwu(Rindex, Interpreter::expr_offset_in_bytes(1), X19_esp); // Get index.
  __ ld(Rarray, Interpreter::expr_offset_in_bytes(2), X19_esp);  // Get array.

  __ verify_oop(X21_tos);
  __ index_check_without_pop(Rarray, Rindex, UseCompressedOops ? 2 : LogBytesPerWord, Rscratch, Rstore_addr);
  // Rindex is dead!
  Register Rscratch3 = Rindex;

  // Do array store check - check for NULL value first.
  __ beq(X21_tos, 0, Lis_null);

  __ load_klass(Rarray_klass, Rarray);
  __ load_klass(Rvalue_klass, X21_tos);

  // Do fast instanceof cache test.
  __ ld(Rarray_element_klass, in_bytes(ObjArrayKlass::element_klass_offset()), Rarray_klass);

  // Generate a fast subtype check. Branch to store_ok if no failure. Throw if failure.
  __ gen_subtype_check(Rvalue_klass */ /*subklass*/ /*, Rarray_element_klass */ /*superklass*/ /*, Rscratch, Rscratch2, Rscratch3, Lstore_ok);

  // Fell through: subtype check failed => throw an exception.
  __ load_dispatch_table(X5_T0, (address*)Interpreter::_throw_ArrayStoreException_entry);
  __ jr(X5_T0);

  __ bind(Lis_null);
  do_oop_store(_masm, Rstore_addr, arrayOopDesc::base_offset_in_bytes(T_OBJECT), noreg */ /* 0 */ /*,
               Rscratch, Rscratch2, Rscratch3, _bs->kind(), true */ /* precise */ /*, false */ /* check_null */ /*);
  __ profile_null_seen(Rscratch, Rscratch2);
  __ j(Ldone);

  // Store is OK.
  __ bind(Lstore_ok);
  do_oop_store(_masm, Rstore_addr, arrayOopDesc::base_offset_in_bytes(T_OBJECT), X21_tos */ /* value */ /*,
               Rscratch, Rscratch2, Rscratch3, _bs->kind(), true */ /* precise */ /*, false */ /* check_null */ /*);

  __ bind(Ldone);
  // Adjust sp (pops array, index and value).
  __ addi(X19_esp, X19_esp, 3 * Interpreter::stackElementSize);
  */
}

void TemplateTable::bastore() {
  __ bytecode_marker(0x54);
  __ unimplemented(__LOCATION__);
  /*
  transition(itos, vtos);

  const Register Rindex   = X5_T0,
                 Rarray   = X6_T1,
                 Rscratch = X10_ARG0;
  __ pop_i(Rindex);
  // tos: val
  // Rarray: array ptr (popped by index_check)
  __ index_check(Rarray, Rindex, 0, Rscratch, Rarray);
  __ sb(X21_tos, arrayOopDesc::base_offset_in_bytes(T_BYTE), Rarray);
  */
}

void TemplateTable::castore() {
  __ bytecode_marker(0x55);
  __ unimplemented(__LOCATION__);
  /*
  transition(itos, vtos);

  const Register Rindex   = X5_T0,
                 Rarray   = X6_T1,
                 Rscratch = X10_ARG0;
  __ pop_i(Rindex);
  // tos: val
  // Rarray: array ptr (popped by index_check)
  __ index_check(Rarray, Rindex, LogBytesPerShort, Rscratch, Rarray);
  __ sh(X21_tos, arrayOopDesc::base_offset_in_bytes(T_CHAR), Rarray);
  */
}

void TemplateTable::sastore() {
  __ unimplemented(__LOCATION__);
  /*
  castore();
  */
}

void TemplateTable::istore(int n) {
  transition(itos, vtos);
  __ bytecode_marker(0x3b);
  __ print_tos();
  __ sw(X21_tos, Interpreter::local_offset_in_bytes(n), X22_locals);
}

void TemplateTable::lstore(int n) {
  transition(ltos, vtos);
  __ bytecode_marker(0x3f);
  __ sd(X21_tos, Interpreter::local_offset_in_bytes(n + 1), X22_locals);
}

void TemplateTable::fstore(int n) {
  transition(ftos, vtos);
  __ bytecode_marker(0x43);
  __ fsw(F18_ftos, Interpreter::local_offset_in_bytes(n), X22_locals);
}

void TemplateTable::dstore(int n) {
  transition(dtos, vtos);
  __ bytecode_marker(0x47);
  __ fsd(F18_ftos, Interpreter::local_offset_in_bytes(n + 1), X22_locals);
}

void TemplateTable::astore(int n) {
  __ bytecode_marker(0x4b);
  __ unimplemented(__LOCATION__);
  /*
  transition(vtos, vtos);

  __ pop_ptr();
  __ verify_oop_or_return_address(X21_tos, X5_T0);
  __ sd(X21_tos, Interpreter::local_offset_in_bytes(n), X22_locals);
  */
}

void TemplateTable::pop() {
  transition(vtos, vtos);
  __ bytecode_marker(0x57);

  __ addi(X19_esp, X19_esp, Interpreter::stackElementSize);
}

void TemplateTable::pop2() {
  transition(vtos, vtos);
  __ bytecode_marker(0x58);

  __ addi(X19_esp, X19_esp, Interpreter::stackElementSize * 2);
}

void TemplateTable::dup() {
  transition(vtos, vtos);
  __ bytecode_marker(0x59);

  __ ld(X5_T0, Interpreter::stackElementSize, X19_esp);
  __ push_ptr(X5_T0);
}

void TemplateTable::dup_x1() {
  transition(vtos, vtos);
  __ bytecode_marker(0x5a);

  Register Ra = X5_T0,
           Rb = X6_T1;
  // stack: ..., a, b
  __ ld(Rb, Interpreter::stackElementSize,     X19_esp);
  __ ld(Ra, Interpreter::stackElementSize * 2, X19_esp);
  __ sd(Rb, Interpreter::stackElementSize * 2, X19_esp);
  __ sd(Ra, Interpreter::stackElementSize,     X19_esp);
  __ push_ptr(Rb);
  // stack: ..., b, a, b
}

void TemplateTable::dup_x2() {
  transition(vtos, vtos);
  __ bytecode_marker(0x5b);

  Register Ra = X5_T0,
           Rb = X6_T1,
           Rc = X10_ARG0;

  // stack: ..., a, b, c
  __ ld(Rc, Interpreter::stackElementSize,     X19_esp);  // load c
  __ ld(Ra, Interpreter::stackElementSize * 3, X19_esp);  // load a
  __ sd(Rc, Interpreter::stackElementSize * 3, X19_esp); // store c in a
  __ ld(Rb, Interpreter::stackElementSize * 2, X19_esp);  // load b
  // stack: ..., c, b, c
  __ sd(Ra, Interpreter::stackElementSize * 2, X19_esp); // store a in b
  // stack: ..., c, a, c
  __ sd(Rb, Interpreter::stackElementSize,     X19_esp); // store b in c
  __ push_ptr(Rc);                                        // push c
  // stack: ..., c, a, b, c
}

void TemplateTable::dup2() {
  transition(vtos, vtos);
  __ bytecode_marker(0x5c);

  Register Ra = X5_T0,
           Rb = X6_T1;
  // stack: ..., a, b
  __ ld(Rb, Interpreter::stackElementSize,     X19_esp);
  __ ld(Ra, Interpreter::stackElementSize * 2, X19_esp);
  __ push_2ptrs(Ra, Rb);
  // stack: ..., a, b, a, b
}

void TemplateTable::dup2_x1() {
  transition(vtos, vtos);
  __ bytecode_marker(0x5d);

  Register Ra = X5_T0,
           Rb = X6_T1,
           Rc = X10_ARG0;
  // stack: ..., a, b, c
  __ ld(Rc, Interpreter::stackElementSize,     X19_esp);
  __ ld(Rb, Interpreter::stackElementSize * 2, X19_esp);
  __ sd(Rc, Interpreter::stackElementSize * 2, X19_esp);
  __ ld(Ra, Interpreter::stackElementSize * 3, X19_esp);
  __ sd(Ra, Interpreter::stackElementSize,     X19_esp);
  __ sd(Rb, Interpreter::stackElementSize * 3, X19_esp);
  // stack: ..., b, c, a
  __ push_2ptrs(Rb, Rc);
  // stack: ..., b, c, a, b, c
}

void TemplateTable::dup2_x2() {
  transition(vtos, vtos);
  __ bytecode_marker(0x5e);

  Register Ra = X5_T0,
           Rb = X6_T1,
           Rc = X10_ARG0,
           Rd = X11_ARG1;
  // stack: ..., a, b, c, d
  __ ld(Rb, Interpreter::stackElementSize * 3, X19_esp);
  __ ld(Rd, Interpreter::stackElementSize,     X19_esp);
  __ sd(Rb, Interpreter::stackElementSize,     X19_esp);  // store b in d
  __ sd(Rd, Interpreter::stackElementSize * 3, X19_esp);  // store d in b
  __ ld(Ra, Interpreter::stackElementSize * 4, X19_esp);
  __ ld(Rc, Interpreter::stackElementSize * 2, X19_esp);
  __ sd(Ra, Interpreter::stackElementSize * 2, X19_esp);  // store a in c
  __ sd(Rc, Interpreter::stackElementSize * 4, X19_esp);  // store c in a
  // stack: ..., c, d, a, b
  __ push_2ptrs(Rc, Rd);
  // stack: ..., c, d, a, b, c, d
}

void TemplateTable::swap() {
  transition(vtos, vtos);
  __ bytecode_marker(0x5f);
  // stack: ..., a, b

  Register Ra = X5_T0,
           Rb = X6_T1;
  // stack: ..., a, b
  __ ld(Rb, Interpreter::stackElementSize,     X19_esp);
  __ ld(Ra, Interpreter::stackElementSize * 2, X19_esp);
  __ sd(Rb, Interpreter::stackElementSize * 2, X19_esp);
  __ sd(Ra, Interpreter::stackElementSize,     X19_esp);
  // stack: ..., b, a
}

void TemplateTable::iop2(Operation op) {
  transition(itos, itos);
  __ bytecode_marker(0x60);

  Register Rscratch = X5_T0;

  __ pop_i(Rscratch);
  // tos  = number of bits to shift
  // Rscratch = value to shift
  switch (op) {
    case  add:   __ add(X21_tos, Rscratch, X21_tos); break;
    case  sub:   __ sub(X21_tos, Rscratch, X21_tos); break;
    case  mul:   __ mulw(X21_tos, Rscratch, X21_tos); break;
    case  _and:  __ and_(X21_tos, Rscratch, X21_tos); break;
    case  _or:   __ or_(X21_tos, Rscratch, X21_tos); break;
    case  _xor:  __ xor_(X21_tos, Rscratch, X21_tos); break;
    case  shl:   __ sllw(X21_tos, Rscratch, X21_tos); break;
    case  shr:   __ sraw(X21_tos, Rscratch, X21_tos); break;
    case  ushr:  __ srlw(X21_tos, Rscratch, X21_tos); break;
    default:     ShouldNotReachHere();
  }
}

void TemplateTable::lop2(Operation op) {
  transition(ltos, ltos);
  __ bytecode_marker(0x61);

  Register Rscratch = X5_T0;
  __ pop_l(Rscratch);
  switch (op) {
    case  add:   __ add(X21_tos, Rscratch, X21_tos); break;
    case  sub:   __ sub(X21_tos, Rscratch, X21_tos); break;
    case  _and:  __ and_(X21_tos, Rscratch, X21_tos); break;
    case  _or:   __ or_(X21_tos, Rscratch, X21_tos); break;
    case  _xor:  __ xor_(X21_tos, Rscratch, X21_tos); break;
    default:     ShouldNotReachHere();
  }
}

void TemplateTable::idiv() {
  __ bytecode_marker(0x6c);
  __ unimplemented(__LOCATION__);
  /*
  transition(itos, itos);

  Label Lnormal, Lexception, Ldone;
  Register Rdividend = X5_T0; // Used by irem.

  __ addi(R0, X21_tos, 1);
  __ cmplwi(CCR0, R0, 2);
  __ bgt(CCR0, Lnormal); // divisor <-1 or >1

  __ cmpwi(CCR1, X21_tos, 0);
  __ beq(CCR1, Lexception); // divisor == 0

  __ pop_i(Rdividend);
  __ mullw(X21_tos, Rdividend, X21_tos); // div by +/-1
  __ b(Ldone);

  __ bind(Lexception);
  __ load_dispatch_table(X5_T0, (address*)Interpreter::_throw_ArithmeticException_entry);
  __ mtctr(X5_T0);
  __ bctr();

  __ align(32, 12);
  __ bind(Lnormal);
  __ pop_i(Rdividend);
  __ divw(X21_tos, Rdividend, X21_tos); // Can't divide minint/-1.
  __ bind(Ldone);
  */
}

void TemplateTable::irem() {
  __ bytecode_marker(0x70);
  __ unimplemented(__LOCATION__);
  /*
  transition(itos, itos);

  __ mr(X6_T1, X21_tos);
  idiv();
  __ mullw(X21_tos, X21_tos, X6_T1);
  __ subf(X21_tos, X21_tos, X5_T0); // Dividend set by idiv.
  */
}

void TemplateTable::lmul() {
  __ bytecode_marker(0x69);
  __ unimplemented(__LOCATION__);
  /*
  transition(ltos, ltos);

  __ pop_l(X5_T0);
  __ mulld(X21_tos, X5_T0, X21_tos);
  */
}

void TemplateTable::ldiv() {
  __ bytecode_marker(0x6d);
  __ unimplemented(__LOCATION__);
  /*
  transition(ltos, ltos);

  Label Lnormal, Lexception, Ldone;
  Register Rdividend = X5_T0; // Used by lrem.

  __ addi(R0, X21_tos, 1);
  __ cmpldi(CCR0, R0, 2);
  __ bgt(CCR0, Lnormal); // divisor <-1 or >1

  __ cmpdi(CCR1, X21_tos, 0);
  __ beq(CCR1, Lexception); // divisor == 0

  __ pop_l(Rdividend);
  __ mulld(X21_tos, Rdividend, X21_tos); // div by +/-1
  __ b(Ldone);

  __ bind(Lexception);
  __ load_dispatch_table(X5_T0, (address*)Interpreter::_throw_ArithmeticException_entry);
  __ mtctr(X5_T0);
  __ bctr();

  __ align(32, 12);
  __ bind(Lnormal);
  __ pop_l(Rdividend);
  __ divd(X21_tos, Rdividend, X21_tos); // Can't divide minint/-1.
  __ bind(Ldone);
  */
}

void TemplateTable::lrem() {
  __ bytecode_marker(0x71);
  __ unimplemented(__LOCATION__);
  /*
  transition(ltos, ltos);

  __ mr(X6_T1, X21_tos);
  ldiv();
  __ mulld(X21_tos, X21_tos, X6_T1);
  __ subf(X21_tos, X21_tos, X5_T0); // Dividend set by ldiv.
  */
}

void TemplateTable::lshl() {
  __ bytecode_marker(0x79);
  __ unimplemented(__LOCATION__);
  /*
  transition(itos, ltos);

  __ rldicl(X21_tos, X21_tos, 0, 64-6); // Extract least significant bits.
  __ pop_l(X5_T0);
  __ sld(X21_tos, X5_T0, X21_tos);
  */
}

void TemplateTable::lshr() {
  __ bytecode_marker(0x7b);
  __ unimplemented(__LOCATION__);
  /*
  transition(itos, ltos);

  __ rldicl(X21_tos, X21_tos, 0, 64-6); // Extract least significant bits.
  __ pop_l(X5_T0);
  __ srad(X21_tos, X5_T0, X21_tos);
  */
}

void TemplateTable::lushr() {
  __ bytecode_marker(0x7d);
  __ unimplemented(__LOCATION__);
  /*
  transition(itos, ltos);

  __ rldicl(X21_tos, X21_tos, 0, 64-6); // Extract least significant bits.
  __ pop_l(X5_T0);
  __ srd(X21_tos, X5_T0, X21_tos);
  */
}

void TemplateTable::fop2(Operation op) {
  __ bytecode_marker(0x62);
  __ unimplemented(__LOCATION__);
  /*
  transition(ftos, ftos);

  switch (op) {
    case add: __ pop_f(F0_SCRATCH); __ fadds(F18_ftos, F0_SCRATCH, F18_ftos); break;
    case sub: __ pop_f(F0_SCRATCH); __ fsubs(F18_ftos, F0_SCRATCH, F18_ftos); break;
    case mul: __ pop_f(F0_SCRATCH); __ fmuls(F18_ftos, F0_SCRATCH, F18_ftos); break;
    case div: __ pop_f(F0_SCRATCH); __ fdivs(F18_ftos, F0_SCRATCH, F18_ftos); break;
    case rem:
      __ pop_f(F1_ARG1);
      __ fmr(F2_ARG2, F18_ftos);
      __ call_VM_leaf(CAST_FROM_FN_PTR(address, SharedRuntime::frem));
      __ fmr(F18_ftos, F1_RET);
      break;

    default: ShouldNotReachHere();
  }
  */
}

void TemplateTable::dop2(Operation op) {
  __ bytecode_marker(0x63);
  __ unimplemented(__LOCATION__);
  /*
  transition(dtos, dtos);

  switch (op) {
    case add: __ pop_d(F0_SCRATCH); __ fadd(F18_ftos, F0_SCRATCH, F18_ftos); break;
    case sub: __ pop_d(F0_SCRATCH); __ fsub(F18_ftos, F0_SCRATCH, F18_ftos); break;
    case mul: __ pop_d(F0_SCRATCH); __ fmul(F18_ftos, F0_SCRATCH, F18_ftos); break;
    case div: __ pop_d(F0_SCRATCH); __ fdiv(F18_ftos, F0_SCRATCH, F18_ftos); break;
    case rem:
      __ pop_d(F1_ARG1);
      __ fmr(F2_ARG2, F18_ftos);
      __ call_VM_leaf(CAST_FROM_FN_PTR(address, SharedRuntime::drem));
      __ fmr(F18_ftos, F1_RET);
      break;

    default: ShouldNotReachHere();
  }
  */
}

// Negate the value in the TOS cache.
void TemplateTable::ineg() {
  __ bytecode_marker(0x74);
  transition(itos, itos);

  __ sub(X21_tos, XZERO, X21_tos);
}

// Negate the value in the TOS cache.
void TemplateTable::lneg() {
  __ bytecode_marker(0x75);
  transition(ltos, ltos);

  __ sub(X21_tos, XZERO, X21_tos);
}

void TemplateTable::fneg() {
  __ bytecode_marker(0x76);
  __ unimplemented(__LOCATION__);
  /*
  transition(ftos, ftos);

  __ fneg(F18_ftos, F18_ftos);
  */
}

void TemplateTable::dneg() {
  __ bytecode_marker(0x77);
  __ unimplemented(__LOCATION__);
  /*
  transition(dtos, dtos);

  __ fneg(F18_ftos, F18_ftos);
  */
}

// Increments a local variable in place.
void TemplateTable::iinc() {
  __ bytecode_marker(0x84);
  transition(vtos, vtos);

  const Register Rindex     = X5_T0,
                 Rincrement = X28_T3,
                 Rvalue     = X6_T1;

  locals_index(Rindex);              // Load locals index from bytecode stream.
  __ lb(Rincrement, 2, X18_bcp);     // Load increment from the bytecode stream.

  __ load_local_int(Rvalue, Rindex, Rindex); // Puts address of local into Rindex.

  __ add(Rvalue, Rincrement, Rvalue);
  __ sw(Rvalue, 0, Rindex);
}

void TemplateTable::wide_iinc() {
  __ bytecode_marker(0x84);
  __ unimplemented(__LOCATION__);
  /*
  transition(vtos, vtos);

  Register Rindex       = X5_T0,
           Rlocals_addr = Rindex,
           Rincr        = X6_T1;
  locals_index_wide(Rindex);
  __ get_2_byte_integer_at_bcp(4, Rincr, InterpreterMacroAssembler::Signed);
  __ load_local_int(X21_tos, Rlocals_addr, Rindex);
  __ add(X21_tos, Rincr, X21_tos);
  __ sw(X21_tos, 0, Rlocals_addr);
  */
}

void TemplateTable::convert() {
  __ bytecode_marker(0x85);
  __ unimplemented(__LOCATION__);
  /*
  // %%%%% Factor this first part across platforms
#ifdef ASSERT
  TosState tos_in  = ilgl;
  TosState tos_out = ilgl;
  switch (bytecode()) {
    case Bytecodes::_i2l: // fall through
    case Bytecodes::_i2f: // fall through
    case Bytecodes::_i2d: // fall through
    case Bytecodes::_i2b: // fall through
    case Bytecodes::_i2c: // fall through
    case Bytecodes::_i2s: tos_in = itos; break;
    case Bytecodes::_l2i: // fall through
    case Bytecodes::_l2f: // fall through
    case Bytecodes::_l2d: tos_in = ltos; break;
    case Bytecodes::_f2i: // fall through
    case Bytecodes::_f2l: // fall through
    case Bytecodes::_f2d: tos_in = ftos; break;
    case Bytecodes::_d2i: // fall through
    case Bytecodes::_d2l: // fall through
    case Bytecodes::_d2f: tos_in = dtos; break;
    default             : ShouldNotReachHere();
  }
  switch (bytecode()) {
    case Bytecodes::_l2i: // fall through
    case Bytecodes::_f2i: // fall through
    case Bytecodes::_d2i: // fall through
    case Bytecodes::_i2b: // fall through
    case Bytecodes::_i2c: // fall through
    case Bytecodes::_i2s: tos_out = itos; break;
    case Bytecodes::_i2l: // fall through
    case Bytecodes::_f2l: // fall through
    case Bytecodes::_d2l: tos_out = ltos; break;
    case Bytecodes::_i2f: // fall through
    case Bytecodes::_l2f: // fall through
    case Bytecodes::_d2f: tos_out = ftos; break;
    case Bytecodes::_i2d: // fall through
    case Bytecodes::_l2d: // fall through
    case Bytecodes::_f2d: tos_out = dtos; break;
    default             : ShouldNotReachHere();
  }
  transition(tos_in, tos_out);
#endif

  // Conversion
  Label done;
  switch (bytecode()) {
    case Bytecodes::_i2l:
      __ extsw(X21_tos, X21_tos);
      break;

    case Bytecodes::_l2i:
      // Nothing to do, we'll continue to work with the lower bits.
      break;

    case Bytecodes::_i2b:
      __ extsb(X21_tos, X21_tos);
      break;

    case Bytecodes::_i2c:
      __ rldicl(X21_tos, X21_tos, 0, 64-2*8);
      break;

    case Bytecodes::_i2s:
      __ extsh(X21_tos, X21_tos);
      break;

    case Bytecodes::_i2d:
      __ extsw(X21_tos, X21_tos);
    case Bytecodes::_l2d:
      __ push_l_pop_d();
      __ fcfid(F18_ftos, F18_ftos);
      break;

    case Bytecodes::_i2f:
      __ extsw(X21_tos, X21_tos);
      __ push_l_pop_d();
      if (VM_Version::has_fcfids()) { // fcfids is >= Power7 only
        // Comment: alternatively, load with sign extend could be done by lfiwax.
        __ fcfids(F18_ftos, F18_ftos);
      } else {
        __ fcfid(F18_ftos, F18_ftos);
        __ frsp(F18_ftos, F18_ftos);
      }
      break;

    case Bytecodes::_l2f:
      if (VM_Version::has_fcfids()) { // fcfids is >= Power7 only
        __ push_l_pop_d();
        __ fcfids(F18_ftos, F18_ftos);
      } else {
        // Avoid rounding problem when result should be 0x3f800001: need fixup code before fcfid+frsp.
        __ mr(X10_ARG0, X21_tos);
        __ call_VM_leaf(CAST_FROM_FN_PTR(address, SharedRuntime::l2f));
        __ fmr(F18_ftos, F1_RET);
      }
      break;

    case Bytecodes::_f2d:
      // empty
      break;

    case Bytecodes::_d2f:
      __ frsp(F18_ftos, F18_ftos);
      break;

    case Bytecodes::_d2i:
    case Bytecodes::_f2i:
      __ fcmpu(CCR0, F18_ftos, F18_ftos);
      __ li(X21_tos, 0); // 0 in case of NAN
      __ bso(CCR0, done);
      __ fctiwz(F18_ftos, F18_ftos);
      __ push_d_pop_l();
      break;

    case Bytecodes::_d2l:
    case Bytecodes::_f2l:
      __ fcmpu(CCR0, F18_ftos, F18_ftos);
      __ li(X21_tos, 0); // 0 in case of NAN
      __ bso(CCR0, done);
      __ fctidz(F18_ftos, F18_ftos);
      __ push_d_pop_l();
      break;

    default: ShouldNotReachHere();
  }
  __ bind(done);
  */
}

// Long compare
void TemplateTable::lcmp() {
  __ bytecode_marker(0x94);
  __ unimplemented(__LOCATION__);
  /*
  transition(ltos, itos);

  const Register Rscratch = X5_T0;
  __ pop_l(Rscratch); // first operand, deeper in stack

  __ cmpd(CCR0, Rscratch, X21_tos); // compare
  __ mfcr(X21_tos); // set bit 32..33 as follows: <: 0b10, =: 0b00, >: 0b01
  __ srwi(Rscratch, X21_tos, 30);
  __ srawi(X21_tos, X21_tos, 31);
  __ orr(X21_tos, Rscratch, X21_tos); // set result as follows: <: -1, =: 0, >: 1
  */
}

// fcmpl/fcmpg and dcmpl/dcmpg bytecodes
// unordered_result == -1 => fcmpl or dcmpl
// unordered_result ==  1 => fcmpg or dcmpg
void TemplateTable::float_cmp(bool is_float, int unordered_result) {
  __ bytecode_marker(0x95);
  __ unimplemented(__LOCATION__);
  /*
  const FloatRegister Rfirst  = F0_SCRATCH,
                      Rsecond = F18_ftos;
  const Register Rscratch = X5_T0;

  if (is_float) {
    __ pop_f(Rfirst);
  } else {
    __ pop_d(Rfirst);
  }

  Label Lunordered, Ldone;
  __ fcmpu(CCR0, Rfirst, Rsecond); // compare
  if (unordered_result) {
    __ bso(CCR0, Lunordered);
  }
  __ mfcr(X21_tos); // set bit 32..33 as follows: <: 0b10, =: 0b00, >: 0b01
  __ srwi(Rscratch, X21_tos, 30);
  __ srawi(X21_tos, X21_tos, 31);
  __ orr(X21_tos, Rscratch, X21_tos); // set result as follows: <: -1, =: 0, >: 1
  if (unordered_result) {
    __ b(Ldone);
    __ bind(Lunordered);
    __ load_const_optimized(X21_tos, unordered_result);
  }
  __ bind(Ldone);
  */
}

void TemplateTable::branch(bool is_jsr, bool is_wide) {
  __ bytecode_marker(0x99);
  __ debug_start_trace();

  // TODO: Implement non-trivial parts of branching

  // Note: on SPARC, we use InterpreterMacroAssembler::if_cmp also.
  __ verify_thread();

  const Register Rscratch1    = X5_T0,
                 //Rscratch2    = X6_T1,
                 //Rscratch3    = X10_ARG0,
                 //R4_counters  = X11_ARG1,
                 //bumped_count = R31,
                 Rdisp        = X7_T2;

  // TODO: Profile branches
  //__ profile_taken_branch(Rscratch1, bumped_count);

  // Get (wide) offset.
  if (is_wide) {
    __ get_4_byte_integer_at_bcp(1, Rscratch1, Rdisp, InterpreterMacroAssembler::Signed);
  } else {
    __ get_2_byte_integer_at_bcp(1, Rscratch1, Rdisp, InterpreterMacroAssembler::Signed);
  }

  // --------------------------------------------------------------------------
  // Handle all the JSR stuff here, then exit.
  // It's much shorter and cleaner than intermingling with the
  // non-JSR normal-branch stuff occurring below.
  if (is_jsr) {
    __ unimplemented(__LOCATION__);
    /*
    // Compute return address as bci in Otos_i.
    __ ld(Rscratch1, in_bytes(Method::const_offset()), R19_method);
    __ addi(Rscratch2, X18_bcp, -in_bytes(ConstMethod::codes_offset()) + (is_wide ? 5 : 3));
    __ subf(X21_tos, Rscratch1, Rscratch2);

    // Bump bcp to target of JSR.
    __ add(X18_bcp, Rdisp, X18_bcp);
    // Push returnAddress for "ret" on stack.
    __ push_ptr(X21_tos);
    // And away we go!
    __ dispatch_next(vtos);
    */
    return;
  }
  /*

  // --------------------------------------------------------------------------
  // Normal (non-jsr) branch handling

  const bool increment_invocation_counter_for_backward_branches = UseCompiler && UseLoopCounter;
  if (increment_invocation_counter_for_backward_branches) {
    //__ unimplemented("branch invocation counter");

    Label Lforward;
    __ add(X18_bcp, Rdisp, X18_bcp); // Add to bc addr.

    // Check branch direction.
    __ cmpdi(CCR0, Rdisp, 0);
    __ bgt(CCR0, Lforward);

    __ get_method_counters(R19_method, R4_counters, Lforward);

    if (TieredCompilation) {
      Label Lno_mdo, Loverflow;
      const int increment = InvocationCounter::count_increment;
      const int mask = ((1 << Tier0BackedgeNotifyFreqLog) - 1) << InvocationCounter::count_shift;
      if (ProfileInterpreter) {
        Register Rmdo = Rscratch1;

        // If no method data exists, go to profile_continue.
        __ ld(Rmdo, in_bytes(Method::method_data_offset()), R19_method);
        __ cmpdi(CCR0, Rmdo, 0);
        __ beq(CCR0, Lno_mdo);

        // Increment backedge counter in the MDO.
        const int mdo_bc_offs = in_bytes(MethodData::backedge_counter_offset()) + in_bytes(InvocationCounter::counter_offset());
        __ lwz(Rscratch2, mdo_bc_offs, Rmdo);
        __ load_const_optimized(Rscratch3, mask, R0);
        __ addi(Rscratch2, Rscratch2, increment);
        __ stw(Rscratch2, mdo_bc_offs, Rmdo);
        __ and_(Rscratch3, Rscratch2, Rscratch3);
        __ bne(CCR0, Lforward);
        __ b(Loverflow);
      }

      // If there's no MDO, increment counter in method.
      const int mo_bc_offs = in_bytes(MethodCounters::backedge_counter_offset()) + in_bytes(InvocationCounter::counter_offset());
      __ bind(Lno_mdo);
      __ lwz(Rscratch2, mo_bc_offs, R4_counters);
      __ load_const_optimized(Rscratch3, mask, R0);
      __ addi(Rscratch2, Rscratch2, increment);
      __ stw(Rscratch2, mo_bc_offs, R19_method);
      __ and_(Rscratch3, Rscratch2, Rscratch3);
      __ bne(CCR0, Lforward);

      __ bind(Loverflow);

      // Notify point for loop, pass branch bytecode.
      __ call_VM(noreg, CAST_FROM_FN_PTR(address, InterpreterRuntime::frequency_counter_overflow), X18_bcp, true);

      // Was an OSR adapter generated?
      // O0 = osr nmethod
      __ cmpdi(CCR0, R3_RET, 0);
      __ beq(CCR0, Lforward);

      // Has the nmethod been invalidated already?
      __ lwz(R0, nmethod::entry_bci_offset(), R3_RET);
      __ cmpwi(CCR0, R0, InvalidOSREntryBci);
      __ beq(CCR0, Lforward);

      // Migrate the interpreter frame off of the stack.
      // We can use all registers because we will not return to interpreter from this point.

      // Save nmethod.
      const Register osr_nmethod = R31;
      __ mr(osr_nmethod, R3_RET);
      __ set_top_ijava_frame_at_SP_as_last_Java_frame(R1_SP, X5_T0);
      __ call_VM_leaf(CAST_FROM_FN_PTR(address, SharedRuntime::OSR_migration_begin), X20_thread);
      __ reset_last_Java_frame();
      // OSR buffer is in ARG1.

      // Remove the interpreter frame.
      __ merge_frames(*/ /*top_frame_sp*/ /* R21_sender_SP, */ /*return_pc*/ /* R0, X5_T0, X6_T1);

      // Jump to the osr code.
      __ ld(X5_T0, nmethod::osr_entry_point_offset(), osr_nmethod);
      __ mtlr(R0);
      __ mtctr(X5_T0);
      __ bctr();

    } else {

      const Register invoke_ctr = Rscratch1;
      // Update Backedge branch separately from invocations.
      __ increment_backedge_counter(R4_counters, invoke_ctr, Rscratch2, Rscratch3);

      if (ProfileInterpreter) {
        __ test_invocation_counter_for_mdp(invoke_ctr, Rscratch2, Lforward);
        if (UseOnStackReplacement) {
          __ test_backedge_count_for_osr(bumped_count, X18_bcp, Rscratch2);
        }
      } else {
        if (UseOnStackReplacement) {
          __ test_backedge_count_for_osr(invoke_ctr, X18_bcp, Rscratch2);
        }
      }
    }

    __ bind(Lforward);

  } else {
  */
    // Bump bytecode pointer by displacement (take the branch).
    __ add(X18_bcp, Rdisp, X18_bcp); // Add to bc addr.
  /*}*/
  // Continue with bytecode @ target.
  // %%%%% Like Intel, could speed things up by moving bytecode fetch to code above,
  // %%%%% and changing dispatch_next to dispatch_only.
  __ dispatch_next(vtos);

  __ debug_stop_trace();
}

// Helper function for if_cmp* methods below.
// Factored out common compare and branch code.
void TemplateTable::if_cmp_common(Register Rfirst, Register Rsecond, Condition cc) {
  Label Lnot_taken;
  __ bytecode_marker(0x9a);
  // Note: The condition code we get is the condition under which we
  // *fall through*! So we have to inverse the CC here.

  switch (cc) {
    case TemplateTable::equal:          __ bne(Rfirst, Rsecond, Lnot_taken); break;
    case TemplateTable::not_equal:      __ beq(Rfirst, Rsecond, Lnot_taken); break;
    case TemplateTable::less:           __ bge(Rfirst, Rsecond, Lnot_taken); break;
    case TemplateTable::less_equal:     __ blt(Rsecond, Rfirst, Lnot_taken); break;
    case TemplateTable::greater:        __ bge(Rsecond, Rfirst, Lnot_taken); break;
    case TemplateTable::greater_equal:  __ blt(Rfirst, Rsecond, Lnot_taken); break;
    default: ShouldNotReachHere();
  }

  // Condition is false => Jump!
  branch(false, false);

  // Condition is not true => Continue.
  __ align(32, 12);
  __ bind(Lnot_taken);
  //__ profile_not_taken_branch(Rscratch1, Rscratch2);
}

// Compare integer values with zero and fall through if CC holds, branch away otherwise.
void TemplateTable::if_0cmp(Condition cc) {
  transition(itos, vtos);

  if_cmp_common(XZERO, X21_tos, cc);
}

// Compare integer values and fall through if CC holds, branch away otherwise.
//
// Interface:
//  - Rfirst: First operand  (older stack value)
//  - tos:    Second operand (younger stack value)
void TemplateTable::if_icmp(Condition cc) {
  transition(itos, vtos);

  const Register Rfirst  = X5_T0,
                 Rsecond = X21_tos;

  __ pop_i(Rfirst);
  if_cmp_common(Rfirst, Rsecond, cc);
}

void TemplateTable::if_nullcmp(Condition cc) {
  transition(atos, vtos);

  if_cmp_common(XZERO, X21_tos, cc);
}

void TemplateTable::if_acmp(Condition cc) {
  transition(atos, vtos);

  const Register Rfirst  = X5_T0,
                 Rsecond = X21_tos;

  __ pop_ptr(Rfirst);
  if_cmp_common(Rfirst, Rsecond, cc);
}

void TemplateTable::ret() {
  __ bytecode_marker(0xa9);
  __ unimplemented(__LOCATION__);
  /*
  locals_index(X5_T0);
  __ load_local_ptr(X21_tos, X5_T0, X5_T0);

  __ profile_ret(vtos, X21_tos, X5_T0, X6_T1);

  __ ld(X5_T0, in_bytes(Method::const_offset()), R19_method);
  __ add(X5_T0, X21_tos, X5_T0);
  __ addi(X18_bcp, X5_T0, in_bytes(ConstMethod::codes_offset()));
  __ dispatch_next(vtos);
  */
}

void TemplateTable::wide_ret() {
  __ bytecode_marker(0xa9);
  __ unimplemented(__LOCATION__);
  /*
  transition(vtos, vtos);

  const Register Rindex = X10_ARG0,
                 Rscratch1 = X5_T0,
                 Rscratch2 = X6_T1;

  locals_index_wide(Rindex);
  __ load_local_ptr(X21_tos, X21_tos, Rindex);
  __ profile_ret(vtos, X21_tos, Rscratch1, X6_T1);
  // Tos now contains the bci, compute the bcp from that.
  __ ld(Rscratch1, in_bytes(Method::const_offset()), R19_method);
  __ addi(Rscratch2, X21_tos, in_bytes(ConstMethod::codes_offset()));
  __ add(X18_bcp, Rscratch1, Rscratch2);
  __ dispatch_next(vtos);
  */
}

void TemplateTable::tableswitch() {
  __ bytecode_marker(0xaa);
  __ unimplemented(__LOCATION__);
  /*
  transition(itos, vtos);

  Label Ldispatch, Ldefault_case;
  Register Rlow_byte         = X10_ARG0,
           Rindex            = Rlow_byte,
           Rhigh_byte        = X11_ARG1,
           Rdef_offset_addr  = X12_ARG2, // is going to contain address of default offset
           Rscratch1         = X5_T0,
           Rscratch2         = X6_T1,
           Roffset           = X13_ARG3;

  // Align bcp.
  __ addi(Rdef_offset_addr, X18_bcp, BytesPerInt);
  __ clrrdi(Rdef_offset_addr, Rdef_offset_addr, log2_long((jlong)BytesPerInt));

  // Load lo & hi.
  __ get_u4(Rlow_byte, Rdef_offset_addr, BytesPerInt, InterpreterMacroAssembler::Unsigned);
  __ get_u4(Rhigh_byte, Rdef_offset_addr, 2 *BytesPerInt, InterpreterMacroAssembler::Unsigned);

  // Check for default case (=index outside [low,high]).
  __ cmpw(CCR0, X21_tos, Rlow_byte);
  __ cmpw(CCR1, X21_tos, Rhigh_byte);
  __ blt(CCR0, Ldefault_case);
  __ bgt(CCR1, Ldefault_case);

  // Lookup dispatch offset.
  __ sub(Rindex, X21_tos, Rlow_byte);
  __ extsw(Rindex, Rindex);
  __ profile_switch_case(Rindex, Rhigh_byte */ /* scratch */ /*, Rscratch1, Rscratch2);
  __ sldi(Rindex, Rindex, LogBytesPerInt);
  __ addi(Rindex, Rindex, 3 * BytesPerInt);
#if defined(VM_LITTLE_ENDIAN)
  __ lwbrx(Roffset, Rdef_offset_addr, Rindex);
  __ extsw(Roffset, Roffset);
#else
  __ lwax(Roffset, Rdef_offset_addr, Rindex);
#endif
  __ b(Ldispatch);

  __ bind(Ldefault_case);
  __ profile_switch_default(Rhigh_byte, Rscratch1);
  __ get_u4(Roffset, Rdef_offset_addr, 0, InterpreterMacroAssembler::Signed);

  __ bind(Ldispatch);

  __ add(X18_bcp, Roffset, X18_bcp);
  __ dispatch_next(vtos);
  */
}

void TemplateTable::lookupswitch() {
  __ bytecode_marker(0xab);
  __ unimplemented(__LOCATION__);
  /*
  transition(itos, itos);
  __ stop("lookupswitch bytecode should have been rewritten");
  */
}

// Table switch using linear search through cases.
// Bytecode stream format:
// Bytecode (1) | 4-byte padding | default offset (4) | count (4) | value/offset pair1 (8) | value/offset pair2 (8) | ...
// Note: Everything is big-endian format here.
void TemplateTable::fast_linearswitch() {
  __ bytecode_marker(0xab);
  __ unimplemented(__LOCATION__);
  /*
  transition(itos, vtos);

  Label Lloop_entry, Lsearch_loop, Lcontinue_execution, Ldefault_case;
  Register Rcount           = X10_ARG0,
           Rcurrent_pair    = X11_ARG1,
           Rdef_offset_addr = X12_ARG2, // Is going to contain address of default offset.
           Roffset          = R31,     // Might need to survive C call.
           Rvalue           = X6_T1,
           Rscratch         = X5_T0,
           Rcmp_value       = X21_tos;

  // Align bcp.
  __ addi(Rdef_offset_addr, X18_bcp, BytesPerInt);
  __ clrrdi(Rdef_offset_addr, Rdef_offset_addr, log2_long((jlong)BytesPerInt));

  // Setup loop counter and limit.
  __ get_u4(Rcount, Rdef_offset_addr, BytesPerInt, InterpreterMacroAssembler::Unsigned);
  __ addi(Rcurrent_pair, Rdef_offset_addr, 2 * BytesPerInt); // Rcurrent_pair now points to first pair.

  __ mtctr(Rcount);
  __ cmpwi(CCR0, Rcount, 0);
  __ bne(CCR0, Lloop_entry);

  // Default case
  __ bind(Ldefault_case);
  __ get_u4(Roffset, Rdef_offset_addr, 0, InterpreterMacroAssembler::Signed);
  if (ProfileInterpreter) {
    __ profile_switch_default(Rdef_offset_addr, Rcount*/ /* scratch */ /*);
  }
  __ b(Lcontinue_execution);

  // Next iteration
  __ bind(Lsearch_loop);
  __ bdz(Ldefault_case);
  __ addi(Rcurrent_pair, Rcurrent_pair, 2 * BytesPerInt);
  __ bind(Lloop_entry);
  __ get_u4(Rvalue, Rcurrent_pair, 0, InterpreterMacroAssembler::Unsigned);
  __ cmpw(CCR0, Rvalue, Rcmp_value);
  __ bne(CCR0, Lsearch_loop);

  // Found, load offset.
  __ get_u4(Roffset, Rcurrent_pair, BytesPerInt, InterpreterMacroAssembler::Signed);
  // Calculate case index and profile
  __ mfctr(Rcurrent_pair);
  if (ProfileInterpreter) {
    __ sub(Rcurrent_pair, Rcount, Rcurrent_pair);
    __ profile_switch_case(Rcurrent_pair, Rcount */ /*scratch*/ /*, Rdef_offset_addr*/ /*scratch*/ /*, Rscratch);
  }

  __ bind(Lcontinue_execution);
  __ add(X18_bcp, Roffset, X18_bcp);
  __ dispatch_next(vtos);
  */
}

// Table switch using binary search (value/offset pairs are ordered).
// Bytecode stream format:
// Bytecode (1) | 4-byte padding | default offset (4) | count (4) | value/offset pair1 (8) | value/offset pair2 (8) | ...
// Note: Everything is big-endian format here. So on little endian machines, we have to revers offset and count and cmp value.
void TemplateTable::fast_binaryswitch() {
  __ bytecode_marker(0xab);
  __ unimplemented(__LOCATION__);
  /*

  transition(itos, vtos);
  // Implementation using the following core algorithm: (copied from Intel)
  //
  // int binary_search(int key, LookupswitchPair* array, int n) {
  //   // Binary search according to "Methodik des Programmierens" by
  //   // Edsger W. Dijkstra and W.H.J. Feijen, Addison Wesley Germany 1985.
  //   int i = 0;
  //   int j = n;
  //   while (i+1 < j) {
  //     // invariant P: 0 <= i < j <= n and (a[i] <= key < a[j] or Q)
  //     // with      Q: for all i: 0 <= i < n: key < a[i]
  //     // where a stands for the array and assuming that the (inexisting)
  //     // element a[n] is infinitely big.
  //     int h = (i + j) >> 1;
  //     // i < h < j
  //     if (key < array[h].fast_match()) {
  //       j = h;
  //     } else {
  //       i = h;
  //     }
  //   }
  //   // R: a[i] <= key < a[i+1] or Q
  //   // (i.e., if key is within array, i is the correct index)
  //   return i;
  // }

  // register allocation
  const Register Rkey     = X21_tos;          // already set (tosca)
  const Register Rarray   = X10_ARG0;
  const Register Ri       = X11_ARG1;
  const Register Rj       = X12_ARG2;
  const Register Rh       = X13_ARG3;
  const Register Rscratch = X5_T0;

  const int log_entry_size = 3;
  const int entry_size = 1 << log_entry_size;

  Label found;

  // Find Array start,
  __ addi(Rarray, X18_bcp, 3 * BytesPerInt);
  __ clrrdi(Rarray, Rarray, log2_long((jlong)BytesPerInt));

  // initialize i & j
  __ li(Ri,0);
  __ get_u4(Rj, Rarray, -BytesPerInt, InterpreterMacroAssembler::Unsigned);

  // and start.
  Label entry;
  __ b(entry);

  // binary search loop
  { Label loop;
    __ bind(loop);
    // int h = (i + j) >> 1;
    __ srdi(Rh, Rh, 1);
    // if (key < array[h].fast_match()) {
    //   j = h;
    // } else {
    //   i = h;
    // }
    __ sldi(Rscratch, Rh, log_entry_size);
#if defined(VM_LITTLE_ENDIAN)
    __ lwbrx(Rscratch, Rscratch, Rarray);
#else
    __ lwzx(Rscratch, Rscratch, Rarray);
#endif

    // if (key < current value)
    //   Rh = Rj
    // else
    //   Rh = Ri
    Label Lgreater;
    __ cmpw(CCR0, Rkey, Rscratch);
    __ bge(CCR0, Lgreater);
    __ mr(Rj, Rh);
    __ b(entry);
    __ bind(Lgreater);
    __ mr(Ri, Rh);

    // while (i+1 < j)
    __ bind(entry);
    __ addi(Rscratch, Ri, 1);
    __ cmpw(CCR0, Rscratch, Rj);
    __ add(Rh, Ri, Rj); // start h = i + j >> 1;

    __ blt(CCR0, loop);
  }

  // End of binary search, result index is i (must check again!).
  Label default_case;
  Label continue_execution;
  if (ProfileInterpreter) {
    __ mr(Rh, Ri);              // Save index in i for profiling.
  }
  // Ri = value offset
  __ sldi(Ri, Ri, log_entry_size);
  __ add(Ri, Ri, Rarray);
  __ get_u4(Rscratch, Ri, 0, InterpreterMacroAssembler::Unsigned);

  Label not_found;
  // Ri = offset offset
  __ cmpw(CCR0, Rkey, Rscratch);
  __ beq(CCR0, not_found);
  // entry not found -> j = default offset
  __ get_u4(Rj, Rarray, -2 * BytesPerInt, InterpreterMacroAssembler::Unsigned);
  __ b(default_case);

  __ bind(not_found);
  // entry found -> j = offset
  __ profile_switch_case(Rh, Rj, Rscratch, Rkey);
  __ get_u4(Rj, Ri, BytesPerInt, InterpreterMacroAssembler::Unsigned);

  if (ProfileInterpreter) {
    __ b(continue_execution);
  }

  __ bind(default_case); // fall through (if not profiling)
  __ profile_switch_default(Ri, Rscratch);

  __ bind(continue_execution);

  __ extsw(Rj, Rj);
  __ add(X18_bcp, Rj, X18_bcp);
  __ dispatch_next(vtos);
  */
}

void TemplateTable::_return(TosState state) {
  __ bytecode_marker(0xb1);
  __ unimplemented(__LOCATION__);
  /*
  transition(state, state);
  assert(_desc->calls_vm(),
         "inconsistent calls_vm information"); // call in remove_activation

  if (_desc->bytecode() == Bytecodes::_return_register_finalizer) {

    Register Rscratch     = X5_T0,
             Rklass       = X6_T1,
             Rklass_flags = Rklass;
    Label Lskip_register_finalizer;

    // Check if the method has the FINALIZER flag set and call into the VM to finalize in this case.
    assert(state == vtos, "only valid state");
    __ ld(X21_tos, 0, X22_locals);

    // Load klass of this obj.
    __ load_klass(Rklass, X21_tos);
    __ lwz(Rklass_flags, in_bytes(Klass::access_flags_offset()), Rklass);
    __ testbitdi(CCR0, R0, Rklass_flags, exact_log2(JVM_ACC_HAS_FINALIZER));
    __ bfalse(CCR0, Lskip_register_finalizer);

    __ call_VM(noreg, CAST_FROM_FN_PTR(address, InterpreterRuntime::register_finalizer), X21_tos */ /* obj */ /*);

    __ align(32, 12);
    __ bind(Lskip_register_finalizer);
  }

  // Move the result value into the correct register and remove memory stack frame.
  __ remove_activation(state, */ /* throw_monitor_exception */ /* true);
  // Restoration of lr done by remove_activation.
  switch (state) {
    case ltos:
    case btos:
    case ctos:
    case stos:
    case atos:
    case itos: __ mr(R3_RET, X21_tos); break;
    case ftos:
    case dtos: __ fmr(F1_RET, F18_ftos); break;
    case vtos: // This might be a constructor. Final fields (and volatile fields on RISCV64) need
               // to get visible before the reference to the object gets stored anywhere.
               __ membar(Assembler::StoreStore); break;
    default  : ShouldNotReachHere();
  }
  __ blr();
  */
}

// ============================================================================
// Constant pool cache access
//
// Memory ordering:
//
// Like done in C++ interpreter, we load the fields
//   - _indices
//   - _f12_oop
// acquired, because these are asked if the cache is already resolved. We don't
// want to float loads above this check.
// See also comments in ConstantPoolCacheEntry::bytecode_1(),
// ConstantPoolCacheEntry::bytecode_2() and ConstantPoolCacheEntry::f1();

// Call into the VM if call site is not yet resolved
//
// Input regs:
//   - None, all passed regs are outputs.
//
// Returns:
//   - Rcache:  The const pool cache entry that contains the resolved result.
//   - Rresult: Either noreg or output for f1/f2.
//
// Kills:
//   - Rscratch
void TemplateTable::resolve_cache_and_index(int byte_no, Register Rcache, Register Rscratch, size_t index_size) {

  __ get_cache_and_index_at_bcp(Rcache, 1, index_size);
  Label Lresolved, Ldone;

  assert(byte_no == f1_byte || byte_no == f2_byte, "byte_no out of range");
  // We are resolved if the indices offset contains the current bytecode.
  __ lbu(Rscratch, in_bytes(ConstantPoolCache::base_offset() + ConstantPoolCacheEntry::indices_offset()) + byte_no + 1, Rcache);
  // Acquire by cmp-br-isync (see below).
  __ addi(Rscratch, Rscratch, -(int)bytecode());
  __ beq(Rscratch, XZERO, Lresolved);

  address entry = NULL;
  switch (bytecode()) {
    case Bytecodes::_getstatic      : // fall through
    case Bytecodes::_putstatic      : // fall through
    case Bytecodes::_getfield       : // fall through
    case Bytecodes::_putfield       : entry = CAST_FROM_FN_PTR(address, InterpreterRuntime::resolve_get_put); break;
    case Bytecodes::_invokevirtual  : // fall through
    case Bytecodes::_invokespecial  : // fall through
    case Bytecodes::_invokestatic   : // fall through
    case Bytecodes::_invokeinterface: entry = CAST_FROM_FN_PTR(address, InterpreterRuntime::resolve_invoke); break;
    case Bytecodes::_invokehandle   : entry = CAST_FROM_FN_PTR(address, InterpreterRuntime::resolve_invokehandle); break;
    case Bytecodes::_invokedynamic  : entry = CAST_FROM_FN_PTR(address, InterpreterRuntime::resolve_invokedynamic); break;
    default                         : ShouldNotReachHere(); break;
  }
  __ li(X11_ARG1, (int)bytecode());
  __ call_VM(noreg, entry, X11_ARG1, true);

  // Update registers with resolved info.
  __ get_cache_and_index_at_bcp(Rcache, 1, index_size);
  __ j(Ldone);

  __ bind(Lresolved);
  __ fence_i(); // Order load wrt. succeeding loads.
  __ bind(Ldone);
}

// Load the constant pool cache entry at field accesses into registers.
// The Rcache and Rindex registers must be set before call.
// Input:
//   - Rcache, Rindex
// Output:
//   - Robj, Roffset, Rflags
void TemplateTable::load_field_cp_cache_entry(Register Robj,
                                              Register Rcache,
                                              Register Rindex /* unused on RISCV64 */,
                                              Register Roffset,
                                              Register Rflags,
                                              bool is_static = false) {
  assert_different_registers(Rcache, Rflags, Roffset);
  // assert(Rindex == noreg, "parameter not used on RISCV64");

  ByteSize cp_base_offset = ConstantPoolCache::base_offset();
  __ ld(Rflags, in_bytes(cp_base_offset) + in_bytes(ConstantPoolCacheEntry::flags_offset()), Rcache);
  __ ld(Roffset, in_bytes(cp_base_offset) + in_bytes(ConstantPoolCacheEntry::f2_offset()), Rcache);
  if (is_static) {
    __ ld(Robj, in_bytes(cp_base_offset) + in_bytes(ConstantPoolCacheEntry::f1_offset()), Rcache);
    __ ld(Robj, in_bytes(Klass::java_mirror_offset()), Robj);
    // Acquire not needed here. Following access has an address dependency on this value.
  }
}

// Load the constant pool cache entry at invokes into registers.
// Resolve if necessary.

// Input Registers:
//   - None, bcp is used, though
//
// Return registers:
//   - Rmethod       (f1 field or f2 if invokevirtual)
//   - Ritable_index (f2 field)
//   - Rflags        (flags field)
//
// Kills:
//   - R21
//
void TemplateTable::load_invoke_cp_cache_entry(int byte_no,
                                               Register Rmethod,
                                               Register Ritable_index,
                                               Register Rflags,
                                               bool is_invokevirtual,
                                               bool is_invokevfinal,
                                               bool is_invokedynamic) {

  ByteSize cp_base_offset = ConstantPoolCache::base_offset();
  // Determine constant pool cache field offsets.
  assert(is_invokevirtual == (byte_no == f2_byte), "is_invokevirtual flag redundant");
  const int method_offset = in_bytes(cp_base_offset + (is_invokevirtual ? ConstantPoolCacheEntry::f2_offset() : ConstantPoolCacheEntry::f1_offset()));
  const int flags_offset  = in_bytes(cp_base_offset + ConstantPoolCacheEntry::flags_offset());
  // Access constant pool cache fields.
  const int index_offset  = in_bytes(cp_base_offset + ConstantPoolCacheEntry::f2_offset());

  Register Rcache = X24_tmp1;

  if (is_invokevfinal) {
    assert(Ritable_index == noreg, "register not used");
    // Already resolved.
    __ get_cache_and_index_at_bcp(Rcache, 1);
  } else {
    resolve_cache_and_index(byte_no, Rcache, X7_T2, is_invokedynamic ? sizeof(u4) : sizeof(u2));
  }

  __ ld(Rmethod, method_offset, Rcache);
  __ ld(Rflags, flags_offset, Rcache);

  if (Ritable_index != noreg) {
    __ ld(Ritable_index, index_offset, Rcache);
  }
}

// ============================================================================
// Field access

// Volatile variables demand their effects be made known to all CPU's
// in order. Store buffers on most chips allow reads & writes to
// reorder; the JMM's ReadAfterWrite.java test fails in -Xint mode
// without some kind of memory barrier (i.e., it's not sufficient that
// the interpreter does not reorder volatile references, the hardware
// also must not reorder them).
//
// According to the new Java Memory Model (JMM):
// (1) All volatiles are serialized wrt to each other. ALSO reads &
//     writes act as aquire & release, so:
// (2) A read cannot let unrelated NON-volatile memory refs that
//     happen after the read float up to before the read. It's OK for
//     non-volatile memory refs that happen before the volatile read to
//     float down below it.
// (3) Similar a volatile write cannot let unrelated NON-volatile
//     memory refs that happen BEFORE the write float down to after the
//     write. It's OK for non-volatile memory refs that happen after the
//     volatile write to float up before it.
//
// We only put in barriers around volatile refs (they are expensive),
// not _between_ memory refs (that would require us to track the
// flavor of the previous memory refs). Requirements (2) and (3)
// require some barriers before volatile stores and after volatile
// loads. These nearly cover requirement (1) but miss the
// volatile-store-volatile-load case.  This final case is placed after
// volatile-stores although it could just as well go before
// volatile-loads.

// The registers cache and index expected to be set before call.
// Correct values of the cache and index registers are preserved.
// Kills:
//   Rcache (if has_tos)
//   Rscratch
void TemplateTable::jvmti_post_field_access(Register Rcache, Register Rscratch, bool is_static, bool has_tos) {
  __ unimplemented(__LOCATION__);
  /*

  assert_different_registers(Rcache, Rscratch);

  if (JvmtiExport::can_post_field_access()) {
    ByteSize cp_base_offset = ConstantPoolCache::base_offset();
    Label Lno_field_access_post;

    // Check if post field access in enabled.
    int offs = __ load_const_optimized(Rscratch, JvmtiExport::get_field_access_count_addr(), R0, true);
    __ lwz(Rscratch, offs, Rscratch);

    __ cmpwi(CCR0, Rscratch, 0);
    __ beq(CCR0, Lno_field_access_post);

    // Post access enabled - do it!
    __ addi(Rcache, Rcache, in_bytes(cp_base_offset));
    if (is_static) {
      __ li(X21_tos, 0);
    } else {
      if (has_tos) {
        // The fast bytecode versions have obj ptr in register.
        // Thus, save object pointer before call_VM() clobbers it
        // put object on tos where GC wants it.
        __ push_ptr(X21_tos);
      } else {
        // Load top of stack (do not pop the value off the stack).
        __ ld(X21_tos, Interpreter::expr_offset_in_bytes(0), X19_esp);
      }
      __ verify_oop(X21_tos);
    }
    // tos:   object pointer or NULL if static
    // cache: cache entry pointer
    __ call_VM(noreg, CAST_FROM_FN_PTR(address, InterpreterRuntime::post_field_access), X21_tos, Rcache);
    if (!is_static && has_tos) {
      // Restore object pointer.
      __ pop_ptr(X21_tos);
      __ verify_oop(X21_tos);
    } else {
      // Cache is still needed to get class or obj.
      __ get_cache_and_index_at_bcp(Rcache, 1);
    }

    __ align(32, 12);
    __ bind(Lno_field_access_post);
  }
  */
}

// kills X5_T0
void TemplateTable::pop_and_check_object(Register Roop) {
  __ unimplemented(__LOCATION__);
  /*
  Register Rtmp = X5_T0;

  assert_different_registers(Rtmp, Roop);
  __ pop_ptr(Roop);
  // For field access must check obj.
  __ null_check_throw(Roop, -1, Rtmp);
  __ verify_oop(Roop);
  */
}

// RISCV64: implement volatile loads as fence-store-acquire.
void TemplateTable::getfield_or_static(int byte_no, bool is_static) {
  transition(vtos, vtos);
  Label Lacquire, Lisync;

  // Runtime call in patch_bytecode
  const Register Rcache        = X10_ARG0,
                 Rclass_or_obj = X28_T3,
                 Roffset       = X29_T4,
                 Rflags        = X7_T2,
                 Rbtable       = X12_ARG2,
                 Rbc           = X13_ARG3,
                 Rscratch      = X6_T1;

  static address field_branch_table[number_of_states],
                 static_branch_table[number_of_states];

  address* branch_table = is_static ? static_branch_table : field_branch_table;

  // Get field offset.
  resolve_cache_and_index(byte_no, Rcache, Rscratch, sizeof(u2));

  // TODO: JVMTI support
  //jvmti_post_field_access(Rcache, Rscratch, is_static, false);

  // Load after possible GC.
  load_field_cp_cache_entry(Rclass_or_obj, Rcache, noreg, Roffset, Rflags, is_static);

  // Load pointer to branch table.
  __ load_const(Rbtable, (address)branch_table);

  // Get volatile flag.
// FIXME: wtf
//  __ srl(Rscratch, Rflags, ConstantPoolCacheEntry::is_volatile_shift);
  __ andi(Rscratch, Rscratch, 1);
  // TODO: Figure out what this means on PPC: "Note: sync is needed before volatile load on PPC64."

  // Check field type.
// FIXME: wtf
//  __ srl(Rflags, Rflags, ConstantPoolCacheEntry::tos_state_shift);
  __ andi(Rflags, Rflags, (1 << ConstantPoolCacheEntry::tos_state_bits) - 1);

#ifdef ASSERT
  Label LFlagInvalid;
  __ li(XJUNK, number_of_states);
  __ bge(Rflags, XJUNK, LFlagInvalid);
#endif

  // Load from branch table and dispatch (volatile case: one instruction ahead).
  __ slli(Rflags, Rflags, LogBytesPerWord);
  if (support_IRIW_for_not_multiple_copy_atomic_cpu) {
    __ slli(Rscratch, Rscratch, exact_log2(BytesPerInstWord)); // Volatile ? size of 1 instruction : 0.
  }
  __ add(Rbtable, Rbtable, Rflags);
  __ ld(Rbtable, 0, Rbtable);

  // Get the obj from stack.
  if (!is_static) {
    pop_and_check_object(Rclass_or_obj); // Kills X5_T0.
  } else {
    __ verify_oop(Rclass_or_obj);
  }

  if (support_IRIW_for_not_multiple_copy_atomic_cpu) {
    __ sub(Rbtable, Rbtable, Rscratch); // Point to volatile/non-volatile entry point.
  }
  __ jr(Rbtable);

#ifdef ASSERT
  __ bind(LFlagInvalid);
  __ stop("got invalid flag", 0x654);

  // __ bind(Lvtos);
  address pc_before_fence = __ pc();
  __ fence(); // Volatile entry point (one instruction before non-volatile_entry point).
  assert(__ pc() - pc_before_fence == (ptrdiff_t)BytesPerInstWord, "must be single instruction");
  assert(branch_table[vtos] == 0, "can't compute twice");
  branch_table[vtos] = __ pc(); // non-volatile_entry point
  __ stop("vtos unexpected", 0x655);
#endif

  __ align(32, 28, 28); // Align load.
  // __ bind(Ldtos);
  __ fence(); // Volatile entry point (one instruction before non-volatile_entry point).
  assert(branch_table[dtos] == 0, "can't compute twice");
  branch_table[dtos] = __ pc(); // non-volatile_entry point
  __ unimplemented("getfield_or_static double");
  /*
  __ lfdx(F18_ftos, Rclass_or_obj, Roffset);
  __ push(dtos);
  if (!is_static) patch_bytecode(Bytecodes::_fast_dgetfield, Rbc, Rscratch);
  {
    Label acquire_double;
    __ beq(CCR6, acquire_double); // Volatile?
    __ dispatch_epilog(vtos, Bytecodes::length_for(bytecode()));

    __ bind(acquire_double);
    __ fcmpu(CCR0, F18_ftos, F18_ftos); // Acquire by cmp-br-isync.
    __ beq_predict_taken(CCR0, Lisync);
    __ b(Lisync); // In case of NAN.
  }
  */

  __ align(32, 28, 28); // Align load.
  // __ bind(Lftos);
  __ fence(); // Volatile entry point (one instruction before non-volatile_entry point).
  assert(branch_table[ftos] == 0, "can't compute twice");
  branch_table[ftos] = __ pc(); // non-volatile_entry point
  __ unimplemented("getfield_or_static float");
  /*
  __ lfsx(F18_ftos, Rclass_or_obj, Roffset);
  __ push(ftos);
  if (!is_static) { patch_bytecode(Bytecodes::_fast_fgetfield, Rbc, Rscratch); }
  {
    Label acquire_float;
    __ beq(CCR6, acquire_float); // Volatile?
    __ dispatch_epilog(vtos, Bytecodes::length_for(bytecode()));

    __ bind(acquire_float);
    __ fcmpu(CCR0, F18_ftos, F18_ftos); // Acquire by cmp-br-isync.
    __ beq_predict_taken(CCR0, Lisync);
    __ b(Lisync); // In case of NAN.
  }
  */

  __ align(32, 28, 28); // Align load.
  // __ bind(Litos);
  __ fence(); // Volatile entry point (one instruction before non-volatile_entry point).
  assert(branch_table[itos] == 0, "can't compute twice");
  branch_table[itos] = __ pc(); // non-volatile_entry point
  __ add(Roffset, Rclass_or_obj, Roffset); 
  __ lw(X21_tos, 0, Roffset);
  __ push(itos);
  if (!is_static) patch_bytecode(Bytecodes::_fast_igetfield, Rbc, Rscratch);
  __ bne(XZERO, Rscratch, Lacquire); // Volatile?
  __ dispatch_epilog(vtos, Bytecodes::length_for(bytecode()));

  __ align(32, 28, 28); // Align load.
  // __ bind(Lltos);
  __ fence(); // Volatile entry point (one instruction before non-volatile_entry point).
  assert(branch_table[ltos] == 0, "can't compute twice");
  branch_table[ltos] = __ pc(); // non-volatile_entry point
  __ add(Roffset, Rclass_or_obj, Roffset); 
  __ ld(X21_tos, 0, Roffset);
  __ push(ltos);
  if (!is_static) patch_bytecode(Bytecodes::_fast_lgetfield, Rbc, Rscratch);
  __ bne(XZERO, Rscratch, Lacquire); // Volatile?
  __ dispatch_epilog(vtos, Bytecodes::length_for(bytecode()));

  __ align(32, 28, 28); // Align load.
  // __ bind(Lbtos);
  __ fence(); // Volatile entry point (one instruction before non-volatile_entry point).
  assert(branch_table[btos] == 0, "can't compute twice");
  branch_table[btos] = __ pc(); // non-volatile_entry point
  __ add(Roffset, Rclass_or_obj, Roffset); 
  __ lb(X21_tos, 0, Roffset);
  __ push(btos);
  if (!is_static) patch_bytecode(Bytecodes::_fast_bgetfield, Rbc, Rscratch);
  __ bne(XZERO, Rscratch, Lacquire); // Volatile?
  __ dispatch_epilog(vtos, Bytecodes::length_for(bytecode()));

  __ align(32, 28, 28); // Align load.
  // __ bind(Lctos);
  __ fence(); // Volatile entry point (one instruction before non-volatile_entry point).
  assert(branch_table[ctos] == 0, "can't compute twice");
  branch_table[ctos] = __ pc(); // non-volatile_entry point
  __ add(Roffset, Rclass_or_obj, Roffset); 
  __ lhu(X21_tos, 0, Roffset);
  __ push(ctos);
  if (!is_static) patch_bytecode(Bytecodes::_fast_cgetfield, Rbc, Rscratch);
  __ bne(XZERO, Rscratch, Lacquire); // Volatile?
  __ dispatch_epilog(vtos, Bytecodes::length_for(bytecode()));

  __ align(32, 28, 28); // Align load.
  // __ bind(Lstos);
  __ fence(); // Volatile entry point (one instruction before non-volatile_entry point).
  assert(branch_table[stos] == 0, "can't compute twice");
  branch_table[stos] = __ pc(); // non-volatile_entry point
  __ add(Roffset, Rclass_or_obj, Roffset); 
  __ lh(X21_tos, 0, Roffset);
  __ push(stos);
  if (!is_static) patch_bytecode(Bytecodes::_fast_sgetfield, Rbc, Rscratch);
  __ bne(XZERO, Rscratch, Lacquire); // Volatile?
  __ dispatch_epilog(vtos, Bytecodes::length_for(bytecode()));

  __ align(32, 28, 28); // Align load.
  // __ bind(Latos);
  __ fence(); // Volatile entry point (one instruction before non-volatile_entry point).
  assert(branch_table[atos] == 0, "can't compute twice");
  branch_table[atos] = __ pc(); // non-volatile_entry point
  __ load_heap_oop(X21_tos, (RegisterOrConstant)Roffset, Rclass_or_obj);
  __ verify_oop(X21_tos);
  __ push(atos);
  if (!is_static) patch_bytecode(Bytecodes::_fast_agetfield, Rbc, Rscratch);
  __ bne(XZERO, Rscratch, Lacquire); // Volatile?
  __ dispatch_epilog(vtos, Bytecodes::length_for(bytecode()));

  __ align(32, 12);
  __ bind(Lacquire);
  __ fence(); // TODO: be less restrictive; should be acquire semantics
  __ bind(Lisync);
  __ fence_i(); // acquire

#ifdef ASSERT
  for (int i = 0; i<number_of_states; ++i) {
    assert(branch_table[i], "get initialization");
    //tty->print_cr("get: %s_branch_table[%d] = 0x%llx (opcode 0x%llx)",
    //              is_static ? "static" : "field", i, branch_table[i], *((unsigned int*)branch_table[i]));
  }
#endif
}

void TemplateTable::getfield(int byte_no) {
  __ bytecode_marker(0xb4);
  __ unimplemented(__LOCATION__);
  /*
  getfield_or_static(byte_no, false);
  */
}

void TemplateTable::getstatic(int byte_no) {
  __ bytecode_marker(0xb2);
  getfield_or_static(byte_no, true);
}

// The registers cache and index expected to be set before call.
// The function may destroy various registers, just not the cache and index registers.
void TemplateTable::jvmti_post_field_mod(Register Rcache, Register Rscratch, bool is_static) {
  __ unimplemented(__LOCATION__);
  /*

  assert_different_registers(Rcache, Rscratch, X13_ARG3);

  if (JvmtiExport::can_post_field_modification()) {
    Label Lno_field_mod_post;

    // Check if post field access in enabled.
    int offs = __ load_const_optimized(Rscratch, JvmtiExport::get_field_modification_count_addr(), R0, true);
    __ lwz(Rscratch, offs, Rscratch);

    __ cmpwi(CCR0, Rscratch, 0);
    __ beq(CCR0, Lno_field_mod_post);

    // Do the post
    ByteSize cp_base_offset = ConstantPoolCache::base_offset();
    const Register Robj = Rscratch;

    __ addi(Rcache, Rcache, in_bytes(cp_base_offset));
    if (is_static) {
      // Life is simple. Null out the object pointer.
      __ li(Robj, 0);
    } else {
      // In case of the fast versions, value lives in registers => put it back on tos.
      int offs = Interpreter::expr_offset_in_bytes(0);
      Register base = X19_esp;
      switch(bytecode()) {
        case Bytecodes::_fast_aputfield: __ push_ptr(); offs+= Interpreter::stackElementSize; break;
        case Bytecodes::_fast_iputfield: // Fall through
        case Bytecodes::_fast_bputfield: // Fall through
        case Bytecodes::_fast_cputfield: // Fall through
        case Bytecodes::_fast_sputfield: __ push_i(); offs+=  Interpreter::stackElementSize; break;
        case Bytecodes::_fast_lputfield: __ push_l(); offs+=2*Interpreter::stackElementSize; break;
        case Bytecodes::_fast_fputfield: __ push_f(); offs+=  Interpreter::stackElementSize; break;
        case Bytecodes::_fast_dputfield: __ push_d(); offs+=2*Interpreter::stackElementSize; break;
        default: {
          offs = 0;
          base = Robj;
          const Register Rflags = Robj;
          Label is_one_slot;
          // Life is harder. The stack holds the value on top, followed by the
          // object. We don't know the size of the value, though; it could be
          // one or two words depending on its type. As a result, we must find
          // the type to determine where the object is.
          __ ld(Rflags, in_bytes(ConstantPoolCacheEntry::flags_offset()), Rcache); // Big Endian
          __ rldicl(Rflags, Rflags, 64-ConstantPoolCacheEntry::tos_state_shift, 64-ConstantPoolCacheEntry::tos_state_bits);

          __ cmpwi(CCR0, Rflags, ltos);
          __ cmpwi(CCR1, Rflags, dtos);
          __ addi(base, X19_esp, Interpreter::expr_offset_in_bytes(1));
          __ crnor(*/ /*CR0 eq*/ /*2, */ /*CR1 eq*/ /*4+2, */ /*CR0 eq*/ /*2);
          __ beq(CCR0, is_one_slot);
          __ addi(base, X19_esp, Interpreter::expr_offset_in_bytes(2));
          __ bind(is_one_slot);
          break;
        }
      }
      __ ld(Robj, offs, base);
      __ verify_oop(Robj);
    }

    __ addi(X13_ARG3, X19_esp, Interpreter::expr_offset_in_bytes(0));
    __ call_VM(noreg, CAST_FROM_FN_PTR(address, InterpreterRuntime::post_field_modification), Robj, Rcache, X13_ARG3);
    __ get_cache_and_index_at_bcp(Rcache, 1);

    // In case of the fast versions, value lives in registers => put it back on tos.
    switch(bytecode()) {
      case Bytecodes::_fast_aputfield: __ pop_ptr(); break;
      case Bytecodes::_fast_iputfield: // Fall through
      case Bytecodes::_fast_bputfield: // Fall through
      case Bytecodes::_fast_cputfield: // Fall through
      case Bytecodes::_fast_sputfield: __ pop_i(); break;
      case Bytecodes::_fast_lputfield: __ pop_l(); break;
      case Bytecodes::_fast_fputfield: __ pop_f(); break;
      case Bytecodes::_fast_dputfield: __ pop_d(); break;
      default: break; // Nothin' to do.
    }

    __ align(32, 12);
    __ bind(Lno_field_mod_post);
  }
  */
}

// RISCV64: implement volatile stores as release-store (return bytecode contains an additional release).
void TemplateTable::putfield_or_static(int byte_no, bool is_static) {
  Label Lvolatile;

  // Runtime call in do_oop_store, also in patch_bytecode
  const Register Rcache        = X12_ARG2, // Do not use ARG1/2 (causes trouble in jvmti_post_field_mod).
                 // FIXME: wtf
                 Rclass_or_obj = X31,      // Needs to survive C call.
                 // FIXME: wtf
                 Roffset       = X22,      // Needs to survive C call.
                 Rflags        = X10_ARG0,
                 Rbtable       = X11_ARG1,
                 Rscratch      = X5_T0,
                 Rscratch2     = X6_T1,
                 Rscratch3     = X13_ARG3,
                 Rbc           = Rscratch3;

  static address field_branch_table[number_of_states],
                 static_branch_table[number_of_states];

  address* branch_table = is_static ? static_branch_table : field_branch_table;

  // Stack (grows up):
  //  value
  //  obj

  // Load the field offset.
  resolve_cache_and_index(byte_no, Rcache, Rscratch, sizeof(u2));

  // TODO: Support JVMTI
  //jvmti_post_field_mod(Rcache, Rscratch, is_static);
  
  // Load the field CP cache entry
  load_field_cp_cache_entry(Rclass_or_obj, Rcache, noreg, Roffset, Rflags, is_static);

  // Load pointer to branch table.
  __ load_const(Rbtable, (address)branch_table);

  // Get volatile flag.
// FIXME: wtf
//  __ srl(Rscratch, Rflags, ConstantPoolCacheEntry::is_volatile_shift);
  __ andi(Rscratch, Rscratch, 1);

  // Check field type.
// FIXME: wtf
//  __ srl(Rflags, Rflags, ConstantPoolCacheEntry::tos_state_shift);
  __ andi(Rflags, Rflags, (1 << ConstantPoolCacheEntry::tos_state_bits) - 1);

#ifdef ASSERT
  Label LFlagInvalid;
  __ li(XJUNK, number_of_states);
  __ bge(Rflags, XJUNK, LFlagInvalid);
#endif

  // Load from branch table and dispatch (volatile case: one instruction ahead).
  __ slli(Rflags, Rflags, LogBytesPerWord);
  __ slli(Rscratch, Rscratch, exact_log2(BytesPerInstWord)); // Volatile? size of instruction 1 : 0.
  __ add(Rbtable, Rbtable, Rflags);
  __ ld(Rbtable, 0, Rbtable);

  __ sub(Rbtable, Rbtable, Rscratch); // Point to volatile/non-volatile entry point.
  __ jr(Rbtable);

#ifdef ASSERT
  __ bind(LFlagInvalid);
  __ stop("got invalid flag", 0x656);

  // __ bind(Lvtos);
  address pc_before_release = __ pc();
  __ fence(); // TODO: make release semantics. Volatile entry point (one instruction before non-volatile_entry point).
  assert(__ pc() - pc_before_release == (ptrdiff_t)BytesPerInstWord, "must be single instruction");
  assert(branch_table[vtos] == 0, "can't compute twice");
  branch_table[vtos] = __ pc(); // non-volatile_entry point
  __ stop("vtos unexpected", 0x657);
#endif

  __ align(32, 28, 28); // Align pop.
  // __ bind(Ldtos);
  __ fence(); // TODO: make release semantics. Volatile entry point (one instruction before non-volatile_entry point).
  assert(branch_table[dtos] == 0, "can't compute twice");
  branch_table[dtos] = __ pc(); // non-volatile_entry point
  __ unimplemented("putfield_or_static double");
  /*
  __ pop(dtos);
  if (!is_static) { pop_and_check_object(Rclass_or_obj); } // Kills X5_T0.
  __ stfdx(F18_ftos, Rclass_or_obj, Roffset);
  if (!is_static) { patch_bytecode(Bytecodes::_fast_dputfield, Rbc, Rscratch, true, byte_no); }
  if (!support_IRIW_for_not_multiple_copy_atomic_cpu) {
    __ bne(Rscratch, XZERO, Lvolatile); // Volatile?
  }
  __ dispatch_epilog(vtos, Bytecodes::length_for(bytecode()));
  */

  __ align(32, 28, 28); // Align pop.
  // __ bind(Lftos);
  __ fence(); // TODO: make release semantics. Volatile entry point (one instruction before non-volatile_entry point).
  assert(branch_table[ftos] == 0, "can't compute twice");
  branch_table[ftos] = __ pc(); // non-volatile_entry point
  __ unimplemented("putfield_or_static float");
  /*
  __ pop(ftos);
  if (!is_static) { pop_and_check_object(Rclass_or_obj); } // Kills X5_T0.
  __ stfsx(F18_ftos, Rclass_or_obj, Roffset);
  if (!is_static) { patch_bytecode(Bytecodes::_fast_fputfield, Rbc, Rscratch, true, byte_no); }
  if (!support_IRIW_for_not_multiple_copy_atomic_cpu) {
    __ bne(Rscratch, XZERO, Lvolatile); // Volatile?
  }
  __ dispatch_epilog(vtos, Bytecodes::length_for(bytecode()));
  */

  __ align(32, 28, 28); // Align pop.
  // __ bind(Litos);
  __ fence(); // TODO: make release semantics. Volatile entry point (one instruction before non-volatile_entry point).
  assert(branch_table[itos] == 0, "can't compute twice");
  branch_table[itos] = __ pc(); // non-volatile_entry point
  __ pop(itos);
  if (!is_static) { pop_and_check_object(Rclass_or_obj); } // Kills X5_T0.
  __ add(Roffset, Rclass_or_obj, Roffset);
  __ sw(X21_tos, 0, Roffset);
  if (!is_static) { patch_bytecode(Bytecodes::_fast_iputfield, Rbc, Rscratch, true, byte_no); }
  if (!support_IRIW_for_not_multiple_copy_atomic_cpu) {
    __ bne(Rscratch, XZERO, Lvolatile); // Volatile?
  }
  __ dispatch_epilog(vtos, Bytecodes::length_for(bytecode()));

  __ align(32, 28, 28); // Align pop.
  // __ bind(Lltos);
  __ fence(); // TODO: make release semantics. Volatile entry point (one instruction before non-volatile_entry point).
  assert(branch_table[ltos] == 0, "can't compute twice");
  branch_table[ltos] = __ pc(); // non-volatile_entry point
  __ pop(ltos);
  if (!is_static) { pop_and_check_object(Rclass_or_obj); } // Kills X5_T0.
  __ add(Roffset, Rclass_or_obj, Roffset);
  __ sd(X21_tos, 0, Roffset);
  if (!is_static) { patch_bytecode(Bytecodes::_fast_lputfield, Rbc, Rscratch, true, byte_no); }
  if (!support_IRIW_for_not_multiple_copy_atomic_cpu) {
    __ bne(Rscratch, XZERO, Lvolatile); // Volatile?
  }
  __ dispatch_epilog(vtos, Bytecodes::length_for(bytecode()));

  __ align(32, 28, 28); // Align pop.
  // __ bind(Lbtos);
  __ fence(); // TODO: make release semantics. Volatile entry point (one instruction before non-volatile_entry point).
  assert(branch_table[btos] == 0, "can't compute twice");
  branch_table[btos] = __ pc(); // non-volatile_entry point
  __ pop(btos);
  if (!is_static) { pop_and_check_object(Rclass_or_obj); } // Kills X5_T0.
  __ add(Roffset, Rclass_or_obj, Roffset);
  __ sb(X21_tos, 0, Roffset);
  if (!is_static) { patch_bytecode(Bytecodes::_fast_bputfield, Rbc, Rscratch, true, byte_no); }
  if (!support_IRIW_for_not_multiple_copy_atomic_cpu) {
    __ bne(Rscratch, XZERO, Lvolatile); // Volatile?
  }
  __ dispatch_epilog(vtos, Bytecodes::length_for(bytecode()));

  __ align(32, 28, 28); // Align pop.
  // __ bind(Lctos);
  __ fence(); // TODO: make release semantics. Volatile entry point (one instruction before non-volatile_entry point).
  assert(branch_table[ctos] == 0, "can't compute twice");
  branch_table[ctos] = __ pc(); // non-volatile_entry point
  __ pop(ctos);
  if (!is_static) { pop_and_check_object(Rclass_or_obj); } // Kills X5_T0..
  __ add(Roffset, Rclass_or_obj, Roffset);
  __ sh(X21_tos, 0, Roffset);
  if (!is_static) { patch_bytecode(Bytecodes::_fast_cputfield, Rbc, Rscratch, true, byte_no); }
  if (!support_IRIW_for_not_multiple_copy_atomic_cpu) {
    __ bne(Rscratch, XZERO, Lvolatile); // Volatile?
  }
  __ dispatch_epilog(vtos, Bytecodes::length_for(bytecode()));

  __ align(32, 28, 28); // Align pop.
  // __ bind(Lstos);
  __ fence(); // TODO: make release semantics. Volatile entry point (one instruction before non-volatile_entry point).
  assert(branch_table[stos] == 0, "can't compute twice");
  branch_table[stos] = __ pc(); // non-volatile_entry point
  __ pop(stos);
  if (!is_static) { pop_and_check_object(Rclass_or_obj); } // Kills X5_T0.
  __ add(Roffset, Rclass_or_obj, Roffset);
  __ sh(X21_tos, 0, Roffset);
  if (!is_static) { patch_bytecode(Bytecodes::_fast_sputfield, Rbc, Rscratch, true, byte_no); }
  if (!support_IRIW_for_not_multiple_copy_atomic_cpu) {
    __ bne(Rscratch, XZERO, Lvolatile); // Volatile?
  }
  __ dispatch_epilog(vtos, Bytecodes::length_for(bytecode()));

  __ align(32, 28, 28); // Align pop.
  // __ bind(Latos);
  __ fence(); // TODO: make release semantics. Volatile entry point (one instruction before non-volatile_entry point).
  assert(branch_table[atos] == 0, "can't compute twice");
  branch_table[atos] = __ pc(); // non-volatile_entry point
  __ unimplemented("putfield_or_static oop");
  /*
  __ pop(atos);
  if (!is_static) { pop_and_check_object(Rclass_or_obj); } // kills X5_T0
  */
  //do_oop_store(_masm, Rclass_or_obj, Roffset, X21_tos, Rscratch, Rscratch2, Rscratch3, _bs->kind(), false /* precise */ , true /* check null */);
  /*
  if (!is_static) { patch_bytecode(Bytecodes::_fast_aputfield, Rbc, Rscratch, true, byte_no); }
  */
  if (!support_IRIW_for_not_multiple_copy_atomic_cpu) {
    __ bne(Rscratch, XZERO, Lvolatile); // Volatile?
    __ dispatch_epilog(vtos, Bytecodes::length_for(bytecode()));

    __ align(32, 12);
    __ bind(Lvolatile);
    __ fence();
  }
  // fallthru: __ b(Lexit);

#ifdef ASSERT
  for (int i = 0; i<number_of_states; ++i) {
    assert(branch_table[i], "put initialization");
    //tty->print_cr("put: %s_branch_table[%d] = 0x%llx (opcode 0x%llx)",
    //              is_static ? "static" : "field", i, branch_table[i], *((unsigned int*)branch_table[i]));
  }
#endif
}

void TemplateTable::putfield(int byte_no) {
  __ bytecode_marker(0xb5);
  __ unimplemented(__LOCATION__);
  /*
  putfield_or_static(byte_no, false);
  */
}

void TemplateTable::putstatic(int byte_no) {
  __ bytecode_marker(0xb3);
  putfield_or_static(byte_no, true);
}

// See SPARC. On RISCV64, we have a different jvmti_post_field_mod which does the job.
void TemplateTable::jvmti_post_fast_field_mod() {
  __ unimplemented(__LOCATION__);
  /*
  __ should_not_reach_here();
  */
}

void TemplateTable::fast_storefield(TosState state) {
  __ unimplemented(__LOCATION__);
  /*
  transition(state, vtos);

  const Register Rcache        = X12_ARG2,  // Do not use ARG1/2 (causes trouble in jvmti_post_field_mod).
                 Rclass_or_obj = R31,      // Needs to survive C call.
                 Roffset       = R22_tmp2, // Needs to survive C call.
                 Rflags        = X10_ARG0,
                 Rscratch      = X5_T0,
                 Rscratch2     = X6_T1,
                 Rscratch3     = X11_ARG1;
  const ConditionRegister CR_is_vol = CCR2; // Non-volatile condition register (survives runtime call in do_oop_store).

  // Constant pool already resolved => Load flags and offset of field.
  __ get_cache_and_index_at_bcp(Rcache, 1);
  jvmti_post_field_mod(Rcache, Rscratch, false */ /* not static */ /*);
  load_field_cp_cache_entry(noreg, Rcache, noreg, Roffset, Rflags, false);

  // Get the obj and the final store addr.
  pop_and_check_object(Rclass_or_obj); // Kills X5_T0.

  // Get volatile flag.
  __ rldicl_(Rscratch, Rflags, 64-ConstantPoolCacheEntry::is_volatile_shift, 63); // Extract volatile bit.
  if (!support_IRIW_for_not_multiple_copy_atomic_cpu) { __ cmpdi(CR_is_vol, Rscratch, 1); }
  {
    Label LnotVolatile;
    __ beq(CCR0, LnotVolatile);
    __ release();
    __ align(32, 12);
    __ bind(LnotVolatile);
  }

  // Do the store and fencing.
  switch(bytecode()) {
    case Bytecodes::_fast_aputfield:
      // Store into the field.
      do_oop_store(_masm, Rclass_or_obj, Roffset, X21_tos, Rscratch, Rscratch2, Rscratch3, _bs->kind(), false */ /* precise */ /*, true */ /* check null */ /*);
      break;

    case Bytecodes::_fast_iputfield:
      __ stwx(X21_tos, Rclass_or_obj, Roffset);
      break;

    case Bytecodes::_fast_lputfield:
      __ stdx(X21_tos, Rclass_or_obj, Roffset);
      break;

    case Bytecodes::_fast_bputfield:
      __ stbx(X21_tos, Rclass_or_obj, Roffset);
      break;

    case Bytecodes::_fast_cputfield:
    case Bytecodes::_fast_sputfield:
      __ sthx(X21_tos, Rclass_or_obj, Roffset);
      break;

    case Bytecodes::_fast_fputfield:
      __ stfsx(F18_ftos, Rclass_or_obj, Roffset);
      break;

    case Bytecodes::_fast_dputfield:
      __ stfdx(F18_ftos, Rclass_or_obj, Roffset);
      break;

    default: ShouldNotReachHere();
  }

  if (!support_IRIW_for_not_multiple_copy_atomic_cpu) {
    Label LVolatile;
    __ beq(CR_is_vol, LVolatile);
    __ dispatch_epilog(vtos, Bytecodes::length_for(bytecode()));

    __ align(32, 12);
    __ bind(LVolatile);
    __ fence();
  }
  */
}

void TemplateTable::fast_accessfield(TosState state) {
  __ unimplemented(__LOCATION__);
  /*
  transition(atos, state);

  Label LisVolatile;
  ByteSize cp_base_offset = ConstantPoolCache::base_offset();

  const Register Rcache        = X10_ARG0,
                 Rclass_or_obj = X21_tos,
                 Roffset       = R22_tmp2,
                 Rflags        = R23_tmp3,
                 Rscratch      = X6_T1;

  // Constant pool already resolved. Get the field offset.
  __ get_cache_and_index_at_bcp(Rcache, 1);
  load_field_cp_cache_entry(noreg, Rcache, noreg, Roffset, Rflags, false);

  // JVMTI support
  jvmti_post_field_access(Rcache, Rscratch, false, true);

  // Get the load address.
  __ null_check_throw(Rclass_or_obj, -1, Rscratch);

  // Get volatile flag.
  __ rldicl_(Rscratch, Rflags, 64-ConstantPoolCacheEntry::is_volatile_shift, 63); // Extract volatile bit.
  __ bne(CCR0, LisVolatile);

  switch(bytecode()) {
    case Bytecodes::_fast_agetfield:
    {
      __ load_heap_oop(X21_tos, (RegisterOrConstant)Roffset, Rclass_or_obj);
      __ verify_oop(X21_tos);
      __ dispatch_epilog(state, Bytecodes::length_for(bytecode()));

      __ bind(LisVolatile);
      if (support_IRIW_for_not_multiple_copy_atomic_cpu) { __ fence(); }
      __ load_heap_oop(X21_tos, (RegisterOrConstant)Roffset, Rclass_or_obj);
      __ verify_oop(X21_tos);
      __ twi_0(X21_tos);
      __ isync();
      break;
    }
    case Bytecodes::_fast_igetfield:
    {
      __ lwax(X21_tos, Rclass_or_obj, Roffset);
      __ dispatch_epilog(state, Bytecodes::length_for(bytecode()));

      __ bind(LisVolatile);
      if (support_IRIW_for_not_multiple_copy_atomic_cpu) { __ fence(); }
      __ lwax(X21_tos, Rclass_or_obj, Roffset);
      __ twi_0(X21_tos);
      __ isync();
      break;
    }
    case Bytecodes::_fast_lgetfield:
    {
      __ ldx(X21_tos, Rclass_or_obj, Roffset);
      __ dispatch_epilog(state, Bytecodes::length_for(bytecode()));

      __ bind(LisVolatile);
      if (support_IRIW_for_not_multiple_copy_atomic_cpu) { __ fence(); }
      __ ldx(X21_tos, Rclass_or_obj, Roffset);
      __ twi_0(X21_tos);
      __ isync();
      break;
    }
    case Bytecodes::_fast_bgetfield:
    {
      __ lbzx(X21_tos, Rclass_or_obj, Roffset);
      __ extsb(X21_tos, X21_tos);
      __ dispatch_epilog(state, Bytecodes::length_for(bytecode()));

      __ bind(LisVolatile);
      if (support_IRIW_for_not_multiple_copy_atomic_cpu) { __ fence(); }
      __ lbzx(X21_tos, Rclass_or_obj, Roffset);
      __ twi_0(X21_tos);
      __ extsb(X21_tos, X21_tos);
      __ isync();
      break;
    }
    case Bytecodes::_fast_cgetfield:
    {
      __ lhzx(X21_tos, Rclass_or_obj, Roffset);
      __ dispatch_epilog(state, Bytecodes::length_for(bytecode()));

      __ bind(LisVolatile);
      if (support_IRIW_for_not_multiple_copy_atomic_cpu) { __ fence(); }
      __ lhzx(X21_tos, Rclass_or_obj, Roffset);
      __ twi_0(X21_tos);
      __ isync();
      break;
    }
    case Bytecodes::_fast_sgetfield:
    {
      __ lhax(X21_tos, Rclass_or_obj, Roffset);
      __ dispatch_epilog(state, Bytecodes::length_for(bytecode()));

      __ bind(LisVolatile);
      if (support_IRIW_for_not_multiple_copy_atomic_cpu) { __ fence(); }
      __ lhax(X21_tos, Rclass_or_obj, Roffset);
      __ twi_0(X21_tos);
      __ isync();
      break;
    }
    case Bytecodes::_fast_fgetfield:
    {
      __ lfsx(F18_ftos, Rclass_or_obj, Roffset);
      __ dispatch_epilog(state, Bytecodes::length_for(bytecode()));

      __ bind(LisVolatile);
      Label Ldummy;
      if (support_IRIW_for_not_multiple_copy_atomic_cpu) { __ fence(); }
      __ lfsx(F18_ftos, Rclass_or_obj, Roffset);
      __ fcmpu(CCR0, F18_ftos, F18_ftos); // Acquire by cmp-br-isync.
      __ bne_predict_not_taken(CCR0, Ldummy);
      __ bind(Ldummy);
      __ isync();
      break;
    }
    case Bytecodes::_fast_dgetfield:
    {
      __ lfdx(F18_ftos, Rclass_or_obj, Roffset);
      __ dispatch_epilog(state, Bytecodes::length_for(bytecode()));

      __ bind(LisVolatile);
      Label Ldummy;
      if (support_IRIW_for_not_multiple_copy_atomic_cpu) { __ fence(); }
      __ lfdx(F18_ftos, Rclass_or_obj, Roffset);
      __ fcmpu(CCR0, F18_ftos, F18_ftos); // Acquire by cmp-br-isync.
      __ bne_predict_not_taken(CCR0, Ldummy);
      __ bind(Ldummy);
      __ isync();
      break;
    }
    default: ShouldNotReachHere();
  }
  */
}

void TemplateTable::fast_xaccess(TosState state) {
  __ unimplemented(__LOCATION__);
  /*
  transition(vtos, state);

  Label LisVolatile;
  ByteSize cp_base_offset = ConstantPoolCache::base_offset();
  const Register Rcache        = X10_ARG0,
                 Rclass_or_obj = X21_tos,
                 Roffset       = R22_tmp2,
                 Rflags        = R23_tmp3,
                 Rscratch      = X6_T1;

  __ ld(Rclass_or_obj, 0, X22_locals);

  // Constant pool already resolved. Get the field offset.
  __ get_cache_and_index_at_bcp(Rcache, 2);
  load_field_cp_cache_entry(noreg, Rcache, noreg, Roffset, Rflags, false);

  // JVMTI support not needed, since we switch back to single bytecode as soon as debugger attaches.

  // Needed to report exception at the correct bcp.
  __ addi(X18_bcp, X18_bcp, 1);

  // Get the load address.
  __ null_check_throw(Rclass_or_obj, -1, Rscratch);

  // Get volatile flag.
  __ rldicl_(Rscratch, Rflags, 64-ConstantPoolCacheEntry::is_volatile_shift, 63); // Extract volatile bit.
  __ bne(CCR0, LisVolatile);

  switch(state) {
  case atos:
    {
      __ load_heap_oop(X21_tos, (RegisterOrConstant)Roffset, Rclass_or_obj);
      __ verify_oop(X21_tos);
      __ dispatch_epilog(state, Bytecodes::length_for(bytecode()) - 1); // Undo bcp increment.

      __ bind(LisVolatile);
      if (support_IRIW_for_not_multiple_copy_atomic_cpu) { __ fence(); }
      __ load_heap_oop(X21_tos, (RegisterOrConstant)Roffset, Rclass_or_obj);
      __ verify_oop(X21_tos);
      __ twi_0(X21_tos);
      __ isync();
      break;
    }
  case itos:
    {
      __ lwax(X21_tos, Rclass_or_obj, Roffset);
      __ dispatch_epilog(state, Bytecodes::length_for(bytecode()) - 1); // Undo bcp increment.

      __ bind(LisVolatile);
      if (support_IRIW_for_not_multiple_copy_atomic_cpu) { __ fence(); }
      __ lwax(X21_tos, Rclass_or_obj, Roffset);
      __ twi_0(X21_tos);
      __ isync();
      break;
    }
  case ftos:
    {
      __ lfsx(F18_ftos, Rclass_or_obj, Roffset);
      __ dispatch_epilog(state, Bytecodes::length_for(bytecode()) - 1); // Undo bcp increment.

      __ bind(LisVolatile);
      Label Ldummy;
      if (support_IRIW_for_not_multiple_copy_atomic_cpu) { __ fence(); }
      __ lfsx(F18_ftos, Rclass_or_obj, Roffset);
      __ fcmpu(CCR0, F18_ftos, F18_ftos); // Acquire by cmp-br-isync.
      __ bne_predict_not_taken(CCR0, Ldummy);
      __ bind(Ldummy);
      __ isync();
      break;
    }
  default: ShouldNotReachHere();
  }
  __ addi(X18_bcp, X18_bcp, -1);
  */
}

// ============================================================================
// Calls

// Common code for invoke
//
// Input:
//   - byte_no
//
// Output:
//   - Rmethod:        The method to invoke next.
//   - Rret_addr:      The return address to return to.
//   - Rindex:         MethodType (invokehandle) or CallSite obj (invokedynamic)
//   - Rrecv:          Cache for "this" pointer, might be noreg if static call.
//   - Rflags:         Method flags from const pool cache.
//
//  Kills:
//   - Rscratch1
//
void TemplateTable::prepare_invoke(int byte_no,
                                   Register Rmethod,  // linked method (or i-klass)
                                   Register Rret_addr,// return address
                                   Register Rindex,   // itable index, MethodType, etc.
                                   Register Rrecv,    // If caller wants to see it.
                                   Register Rflags,   // If caller wants to test it.
                                   Register Rscratch
                                   ) {
  // Determine flags.
  const Bytecodes::Code code = bytecode();
  const bool is_invokeinterface  = code == Bytecodes::_invokeinterface;
  const bool is_invokedynamic    = code == Bytecodes::_invokedynamic;
  const bool is_invokehandle     = code == Bytecodes::_invokehandle;
  const bool is_invokevirtual    = code == Bytecodes::_invokevirtual;
  const bool is_invokespecial    = code == Bytecodes::_invokespecial;
  const bool load_receiver       = (Rrecv != noreg);
  assert(load_receiver == (code != Bytecodes::_invokestatic && code != Bytecodes::_invokedynamic), "");

  assert_different_registers(Rmethod, Rindex, Rflags, Rscratch);
  assert_different_registers(Rmethod, Rrecv, Rflags, Rscratch);
  assert_different_registers(Rret_addr, Rscratch);

  load_invoke_cp_cache_entry(byte_no, Rmethod, Rindex, Rflags, is_invokevirtual, false, is_invokedynamic);

  // Saving of SP done in call_from_interpreter.

  // Maybe push "appendix" to arguments.
  /*
   * TODO: Add support for invokedynamic and invokehandle
   * Only support invokestatic right now
   *
  if (is_invokedynamic || is_invokehandle) {
    Label Ldone;
    __ rldicl_(R0, Rflags, 64-ConstantPoolCacheEntry::has_appendix_shift, 63);
    __ beq(CCR0, Ldone);
    // Push "appendix" (MethodType, CallSite, etc.).
    // This must be done before we get the receiver,
    // since the parameter_size includes it.
    __ load_resolved_reference_at_index(Rscratch, Rindex);
    __ verify_oop(Rscratch);
    __ push_ptr(Rscratch);
    __ bind(Ldone);
  }
  */

  // Load receiver if needed (after appendix is pushed so parameter size is correct).
  if (load_receiver) {
    const Register Rparam_count = Rscratch;
    __ andi(Rparam_count, Rflags, ConstantPoolCacheEntry::parameter_size_mask);
    __ load_receiver(Rparam_count, Rrecv);
    __ verify_oop(Rrecv);
  }

  // Get return address.
  {
    Register Rtable_addr = Rscratch;
    Register Rret_type = Rret_addr;
    address table_addr = (address) Interpreter::invoke_return_entry_table_for(code);

    // Get return type. It's coded into the upper 4 bits of the lower half of the 64 bit value.
    __ srli(Rret_type, Rflags, ConstantPoolCacheEntry::tos_state_shift);
    uint64_t mask = ((uint64_t) 0xffffffff) >> (32-ConstantPoolCacheEntry::tos_state_bits);
    // Fine to do in immediate; mask should be using 4 bits at the maximum
    __ andi(Rret_type, Rret_type, mask);
    __ load_dispatch_table(Rtable_addr, (address*)table_addr);
    __ slli(Rret_type, Rret_type, LogBytesPerWord);
    // Get return address.
    __ add(Rscratch, Rtable_addr, Rret_type);
    __ ld(Rret_addr, 0, Rscratch);
  }
}

// Helper for virtual calls. Load target out of vtable and jump off!
// Kills all passed registers.
void TemplateTable::generate_vtable_call(Register Rrecv_klass, Register Rindex, Register Rret, Register Rtemp) {
  __ unimplemented(__LOCATION__);
  /*

  assert_different_registers(Rrecv_klass, Rtemp, Rret);
  const Register Rtarget_method = Rindex;

  // Get target method & entry point.
  const int base = InstanceKlass::vtable_start_offset() * wordSize;
  // Calc vtable addr scale the vtable index by 8.
  __ sldi(Rindex, Rindex, exact_log2(vtableEntry::size() * wordSize));
  // Load target.
  __ addi(Rrecv_klass, Rrecv_klass, base + vtableEntry::method_offset_in_bytes());
  __ ldx(Rtarget_method, Rindex, Rrecv_klass);
  // Argument and return type profiling.
  __ profile_arguments_type(Rtarget_method, Rrecv_klass */ /* scratch1 */ /*, Rtemp */ /* scratch2 */ /*, true);
  __ call_from_interpreter(Rtarget_method, Rret, Rrecv_klass */ /* scratch1 */ /*, Rtemp */ /* scratch2 */ /*);
  */
}

// Virtual or final call. Final calls are rewritten on the fly to run through "fast_finalcall" next time.
void TemplateTable::invokevirtual(int byte_no) {
  __ bytecode_marker(0xb6);
  __ unimplemented(__LOCATION__);
  /*
  transition(vtos, vtos);

  Register Rtable_addr = X5_T0,
           Rret_type = X6_T1,
           Rret_addr = X12_ARG2,
           Rflags = R22_tmp2, // Should survive C call.
           Rrecv = X10_ARG0,
           Rrecv_klass = Rrecv,
           Rvtableindex_or_method = R31, // Should survive C call.
           Rnum_params = X11_ARG1,
           Rnew_bc = X13_ARG3;

  Label LnotFinal;

  load_invoke_cp_cache_entry(byte_no, Rvtableindex_or_method, noreg, Rflags, */ /*virtual*/ /* true, false, false);

  __ testbitdi(CCR0, R0, Rflags, ConstantPoolCacheEntry::is_vfinal_shift);
  __ bfalse(CCR0, LnotFinal);

  patch_bytecode(Bytecodes::_fast_invokevfinal, Rnew_bc, X6_T1);
  invokevfinal_helper(Rvtableindex_or_method, Rflags, X5_T0, X6_T1);

  __ align(32, 12);
  __ bind(LnotFinal);
  // Load "this" pointer (receiver).
  __ rldicl(Rnum_params, Rflags, 64, 48);
  __ load_receiver(Rnum_params, Rrecv);
  __ verify_oop(Rrecv);

  // Get return type. It's coded into the upper 4 bits of the lower half of the 64 bit value.
  __ rldicl(Rret_type, Rflags, 64-ConstantPoolCacheEntry::tos_state_shift, 64-ConstantPoolCacheEntry::tos_state_bits);
  __ load_dispatch_table(Rtable_addr, Interpreter::invoke_return_entry_table());
  __ sldi(Rret_type, Rret_type, LogBytesPerWord);
  __ ldx(Rret_addr, Rret_type, Rtable_addr);
  __ null_check_throw(Rrecv, oopDesc::klass_offset_in_bytes(), X5_T0);
  __ load_klass(Rrecv_klass, Rrecv);
  __ verify_klass_ptr(Rrecv_klass);
  __ profile_virtual_call(Rrecv_klass, X5_T0, X6_T1, false);

  generate_vtable_call(Rrecv_klass, Rvtableindex_or_method, Rret_addr, X5_T0);
  */
}

void TemplateTable::fast_invokevfinal(int byte_no) {
  __ unimplemented(__LOCATION__);
  /*
  transition(vtos, vtos);

  assert(byte_no == f2_byte, "use this argument");
  Register Rflags  = R22_tmp2,
           Rmethod = R31;
  load_invoke_cp_cache_entry(byte_no, Rmethod, noreg, Rflags, */ /*virtual*/ /* true, */ /*is_invokevfinal*/ /* true, false);
  invokevfinal_helper(Rmethod, Rflags, X5_T0, X6_T1);
  */
}

void TemplateTable::invokevfinal_helper(Register Rmethod, Register Rflags, Register Rscratch1, Register Rscratch2) {
  __ unimplemented(__LOCATION__);
  /*

  assert_different_registers(Rmethod, Rflags, Rscratch1, Rscratch2);

  // Load receiver from stack slot.
  Register Rrecv = Rscratch2;
  Register Rnum_params = Rrecv;

  __ ld(Rnum_params, in_bytes(Method::const_offset()), Rmethod);
  __ lhz(Rnum_params */ /* number of params */ /*, in_bytes(ConstMethod::size_of_parameters_offset()), Rnum_params);

  // Get return address.
  Register Rtable_addr = Rscratch1,
           Rret_addr   = Rflags,
           Rret_type   = Rret_addr;
  // Get return type. It's coded into the upper 4 bits of the lower half of the 64 bit value.
  __ rldicl(Rret_type, Rflags, 64-ConstantPoolCacheEntry::tos_state_shift, 64-ConstantPoolCacheEntry::tos_state_bits);
  __ load_dispatch_table(Rtable_addr, Interpreter::invoke_return_entry_table());
  __ sldi(Rret_type, Rret_type, LogBytesPerWord);
  __ ldx(Rret_addr, Rret_type, Rtable_addr);

  // Load receiver and receiver NULL check.
  __ load_receiver(Rnum_params, Rrecv);
  __ null_check_throw(Rrecv, -1, Rscratch1);

  __ profile_final_call(Rrecv, Rscratch1);
  // Argument and return type profiling.
  __ profile_arguments_type(Rmethod, Rscratch1, Rscratch2, true);

  // Do the call.
  __ call_from_interpreter(Rmethod, Rret_addr, Rscratch1, Rscratch2);
  */
}

void TemplateTable::invokespecial(int byte_no) {
  __ bytecode_marker(0xb7);
  __ unimplemented(__LOCATION__);
  /*
  assert(byte_no == f1_byte, "use this argument");
  transition(vtos, vtos);

  Register Rtable_addr = X10_ARG0,
           Rret_addr   = X11_ARG1,
           Rflags      = X12_ARG2,
           Rreceiver   = X13_ARG3,
           Rmethod     = R31;

  prepare_invoke(byte_no, Rmethod, Rret_addr, noreg, Rreceiver, Rflags, X5_T0);

  // Receiver NULL check.
  __ null_check_throw(Rreceiver, -1, X5_T0);

  __ profile_call(X5_T0, X6_T1);
  // Argument and return type profiling.
  __ profile_arguments_type(Rmethod, X5_T0, X6_T1, false);
  __ call_from_interpreter(Rmethod, Rret_addr, X5_T0, X6_T1);
  */
}

void TemplateTable::invokestatic(int byte_no) {
  assert(byte_no == f1_byte, "use this argument");
  transition(vtos, vtos);
  __ bytecode_marker(0xb8);

  Register Rtable_addr = X10_ARG0,
           Rret_addr   = X11_ARG1,
           Rflags      = X12_ARG2;

  prepare_invoke(byte_no, X23_method, Rret_addr, noreg, noreg, Rflags, X5_T0);

  // TODO: Add profiling, port from PPC
  // __ profile_call(X5_T0, X6_T1);
  // Argument and return type profiling.
  // __ profile_arguments_type(R19_method, X5_T0, X6_T1, false);
  __ call_from_interpreter(X23_method, Rret_addr, X5_T0, X6_T1);
}

void TemplateTable::invokeinterface_object_method(Register Rrecv_klass,
                                                  Register Rret,
                                                  Register Rflags,
                                                  Register Rindex,
                                                  Register Rtemp1,
                                                  Register Rtemp2) {
  __ unimplemented(__LOCATION__);
  /*

  assert_different_registers(Rindex, Rret, Rrecv_klass, Rflags, Rtemp1, Rtemp2);
  Label LnotFinal;

  // Check for vfinal.
  __ testbitdi(CCR0, R0, Rflags, ConstantPoolCacheEntry::is_vfinal_shift);
  __ bfalse(CCR0, LnotFinal);

  Register Rscratch = Rflags; // Rflags is dead now.

  // Final call case.
  __ profile_final_call(Rtemp1, Rscratch);
  // Argument and return type profiling.
  __ profile_arguments_type(Rindex, Rscratch, Rrecv_klass */ /* scratch */ /*, true);
  // Do the final call - the index (f2) contains the method.
  __ call_from_interpreter(Rindex, Rret, Rscratch, Rrecv_klass */ /* scratch */ /*);

  // Non-final callc case.
  __ bind(LnotFinal);
  __ profile_virtual_call(Rrecv_klass, Rtemp1, Rscratch, false);
  generate_vtable_call(Rrecv_klass, Rindex, Rret, Rscratch);
  */
}

void TemplateTable::invokeinterface(int byte_no) {
  __ bytecode_marker(0xb9);
  __ unimplemented(__LOCATION__);
  /*
  assert(byte_no == f1_byte, "use this argument");
  transition(vtos, vtos);

  const Register Rscratch1        = X5_T0,
                 Rscratch2        = X6_T1,
                 Rscratch3        = X16_ARG6,
                 Rscratch4        = X17_ARG7,
                 Rtable_addr      = Rscratch2,
                 Rinterface_klass = X12_ARG2,
                 Rret_type        = X15_ARG5,
                 Rret_addr        = Rret_type,
                 Rindex           = X13_ARG3,
                 Rreceiver        = X11_ARG1,
                 Rrecv_klass      = Rreceiver,
                 Rflags           = X14_ARG4;

  prepare_invoke(byte_no, Rinterface_klass, Rret_addr, Rindex, Rreceiver, Rflags, Rscratch1);

  // Get receiver klass.
  __ null_check_throw(Rreceiver, oopDesc::klass_offset_in_bytes(), Rscratch3);
  __ load_klass(Rrecv_klass, Rreceiver);

  // Check corner case object method.
  Label LobjectMethod;

  __ testbitdi(CCR0, R0, Rflags, ConstantPoolCacheEntry::is_forced_virtual_shift);
  __ btrue(CCR0, LobjectMethod);

  // Fallthrough: The normal invokeinterface case.
  __ profile_virtual_call(Rrecv_klass, Rscratch1, Rscratch2, false);

  // Find entry point to call.
  Label Lthrow_icc, Lthrow_ame;
  // Result will be returned in Rindex.
  __ mr(Rscratch4, Rrecv_klass);
  __ mr(Rscratch3, Rindex);
  __ lookup_interface_method(Rrecv_klass, Rinterface_klass, Rindex, Rindex, Rscratch1, Rscratch2, Lthrow_icc);

  __ cmpdi(CCR0, Rindex, 0);
  __ beq(CCR0, Lthrow_ame);
  // Found entry. Jump off!
  // Argument and return type profiling.
  __ profile_arguments_type(Rindex, Rscratch1, Rscratch2, true);
  __ call_from_interpreter(Rindex, Rret_addr, Rscratch1, Rscratch2);

  // Vtable entry was NULL => Throw abstract method error.
  __ bind(Lthrow_ame);
  __ mr(Rrecv_klass, Rscratch4);
  __ mr(Rindex, Rscratch3);
  call_VM(noreg, CAST_FROM_FN_PTR(address, InterpreterRuntime::throw_AbstractMethodError));

  // Interface was not found => Throw incompatible class change error.
  __ bind(Lthrow_icc);
  __ mr(Rrecv_klass, Rscratch4);
  call_VM(noreg, CAST_FROM_FN_PTR(address, InterpreterRuntime::throw_IncompatibleClassChangeError));

  __ should_not_reach_here();

  // Special case of invokeinterface called for virtual method of
  // java.lang.Object. See ConstantPoolCacheEntry::set_method() for details:
  // The invokeinterface was rewritten to a invokevirtual, hence we have
  // to handle this corner case. This code isn't produced by javac, but could
  // be produced by another compliant java compiler.
  __ bind(LobjectMethod);
  invokeinterface_object_method(Rrecv_klass, Rret_addr, Rflags, Rindex, Rscratch1, Rscratch2);
  */
}

void TemplateTable::invokedynamic(int byte_no) {
  __ unimplemented(__LOCATION__);
  /*
  transition(vtos, vtos);

  const Register Rret_addr = X10_ARG0,
                 Rflags    = X11_ARG1,
                 Rmethod   = R22_tmp2,
                 Rscratch1 = X5_T0,
                 Rscratch2 = X6_T1;

  if (!EnableInvokeDynamic) {
    // We should not encounter this bytecode if !EnableInvokeDynamic.
    // The verifier will stop it. However, if we get past the verifier,
    // this will stop the thread in a reasonable way, without crashing the JVM.
    __ call_VM(noreg, CAST_FROM_FN_PTR(address, InterpreterRuntime::throw_IncompatibleClassChangeError));
    // The call_VM checks for exception, so we should never return here.
    __ should_not_reach_here();
    return;
  }

  prepare_invoke(byte_no, Rmethod, Rret_addr, Rscratch1, noreg, Rflags, Rscratch2);

  // Profile this call.
  __ profile_call(Rscratch1, Rscratch2);

  // Off we go. With the new method handles, we don't jump to a method handle
  // entry any more. Instead, we pushed an "appendix" in prepare invoke, which happens
  // to be the callsite object the bootstrap method returned. This is passed to a
  // "link" method which does the dispatch (Most likely just grabs the MH stored
  // inside the callsite and does an invokehandle).
  // Argument and return type profiling.
  __ profile_arguments_type(Rmethod, Rscratch1, Rscratch2, false);
  __ call_from_interpreter(Rmethod, Rret_addr, Rscratch1 */ /* scratch1 */ /*, Rscratch2 */ /* scratch2 */ /*);
  */
}

void TemplateTable::invokehandle(int byte_no) {
  __ unimplemented(__LOCATION__);
  /*
  transition(vtos, vtos);

  const Register Rret_addr = X10_ARG0,
                 Rflags    = X11_ARG1,
                 Rrecv     = X12_ARG2,
                 Rmethod   = R22_tmp2,
                 Rscratch1 = X5_T0,
                 Rscratch2 = X6_T1;

  if (!EnableInvokeDynamic) {
    // Rewriter does not generate this bytecode.
    __ should_not_reach_here();
    return;
  }

  prepare_invoke(byte_no, Rmethod, Rret_addr, Rscratch1, Rrecv, Rflags, Rscratch2);
  __ verify_method_ptr(Rmethod);
  __ null_check_throw(Rrecv, -1, Rscratch2);

  __ profile_final_call(Rrecv, Rscratch1);

  // Still no call from handle => We call the method handle interpreter here.
  // Argument and return type profiling.
  __ profile_arguments_type(Rmethod, Rscratch1, Rscratch2, true);
  __ call_from_interpreter(Rmethod, Rret_addr, Rscratch1 */ /* scratch1 */ /*, Rscratch2 */ /* scratch2 */ /*);
  */
}

// =============================================================================
// Allocation

// Puts allocated obj ref onto the expression stack.
void TemplateTable::_new() {
  __ bytecode_marker(0xbb);
  __ unimplemented(__LOCATION__);
  /*
  transition(vtos, atos);

  Label Lslow_case,
        Ldone,
        Linitialize_header,
        Lallocate_shared,
        Linitialize_object;  // Including clearing the fields.

  const Register RallocatedObject = X21_tos,
                 RinstanceKlass   = X16_ARG6,
                 Rscratch         = X5_T0,
                 Roffset          = X15_ARG5,
                 Rinstance_size   = Roffset,
                 Rcpool           = X11_ARG1,
                 Rtags            = X10_ARG0,
                 Rindex           = X12_ARG2;

  const bool allow_shared_alloc = Universe::heap()->supports_inline_contig_alloc() && !CMSIncrementalMode;

  // --------------------------------------------------------------------------
  // Check if fast case is possible.

  // Load pointers to const pool and const pool's tags array.
  __ get_cpool_and_tags(Rcpool, Rtags);
  // Load index of constant pool entry.
  __ get_2_byte_integer_at_bcp(1, Rindex, InterpreterMacroAssembler::Unsigned);

  if (UseTLAB) {
    // Make sure the class we're about to instantiate has been resolved
    // This is done before loading instanceKlass to be consistent with the order
    // how Constant Pool is updated (see ConstantPoolCache::klass_at_put).
    __ addi(Rtags, Rtags, Array<u1>::base_offset_in_bytes());
    __ lbzx(Rtags, Rindex, Rtags);

    __ cmpdi(CCR0, Rtags, JVM_CONSTANT_Class);
    __ bne(CCR0, Lslow_case);

    // Get instanceKlass (load from Rcpool + sizeof(ConstantPool) + Rindex*BytesPerWord).
    __ sldi(Roffset, Rindex, LogBytesPerWord);
    __ addi(Rscratch, Rcpool, sizeof(ConstantPool));
    __ isync(); // Order load of instance Klass wrt. tags.
    __ ldx(RinstanceKlass, Roffset, Rscratch);

    // Make sure klass is fully initialized and get instance_size.
    __ lbz(Rscratch, in_bytes(InstanceKlass::init_state_offset()), RinstanceKlass);
    __ lwz(Rinstance_size, in_bytes(Klass::layout_helper_offset()), RinstanceKlass);

    __ cmpdi(CCR1, Rscratch, InstanceKlass::fully_initialized);
    // Make sure klass does not have has_finalizer, or is abstract, or interface or java/lang/Class.
    __ andi_(R0, Rinstance_size, Klass::_lh_instance_slow_path_bit); // slow path bit equals 0?

    __ crnand(*/ /*CR0 eq*/ /*2, */ /*CR1 eq*/ /*4+2, */ /*CR0 eq*/ /*2); // slow path bit set or not fully initialized?
    __ beq(CCR0, Lslow_case);

    // --------------------------------------------------------------------------
    // Fast case:
    // Allocate the instance.
    // 1) Try to allocate in the TLAB.
    // 2) If fail, and the TLAB is not full enough to discard, allocate in the shared Eden.
    // 3) If the above fails (or is not applicable), go to a slow case (creates a new TLAB, etc.).

    Register RoldTopValue = RallocatedObject; // Object will be allocated here if it fits.
    Register RnewTopValue = X13_ARG3;
    Register RendValue    = X14_ARG4;

    // Check if we can allocate in the TLAB.
    __ ld(RoldTopValue, in_bytes(JavaThread::tlab_top_offset()), X20_thread);
    __ ld(RendValue,    in_bytes(JavaThread::tlab_end_offset()), X20_thread);

    __ add(RnewTopValue, Rinstance_size, RoldTopValue);

    // If there is enough space, we do not CAS and do not clear.
    __ cmpld(CCR0, RnewTopValue, RendValue);
    __ bgt(CCR0, allow_shared_alloc ? Lallocate_shared : Lslow_case);

    __ std(RnewTopValue, in_bytes(JavaThread::tlab_top_offset()), X20_thread);

    if (ZeroTLAB) {
      // The fields have already been cleared.
      __ b(Linitialize_header);
    } else {
      // Initialize both the header and fields.
      __ b(Linitialize_object);
    }

    // Fall through: TLAB was too small.
    if (allow_shared_alloc) {
      Register RtlabWasteLimitValue = X17_ARG7;
      Register RfreeValue = RnewTopValue;

      __ bind(Lallocate_shared);
      // Check if tlab should be discarded (refill_waste_limit >= free).
      __ ld(RtlabWasteLimitValue, in_bytes(JavaThread::tlab_refill_waste_limit_offset()), X20_thread);
      __ subf(RfreeValue, RoldTopValue, RendValue);
      __ srdi(RfreeValue, RfreeValue, LogHeapWordSize); // in dwords
      __ cmpld(CCR0, RtlabWasteLimitValue, RfreeValue);
      __ bge(CCR0, Lslow_case);

      // Increment waste limit to prevent getting stuck on this slow path.
      __ addi(RtlabWasteLimitValue, RtlabWasteLimitValue, (int)ThreadLocalAllocBuffer::refill_waste_limit_increment());
      __ std(RtlabWasteLimitValue, in_bytes(JavaThread::tlab_refill_waste_limit_offset()), X20_thread);
    }
    // else: No allocation in the shared eden. // fallthru: __ b(Lslow_case);
  }
  // else: Always go the slow path.

  // --------------------------------------------------------------------------
  // slow case
  __ bind(Lslow_case);
  call_VM(X21_tos, CAST_FROM_FN_PTR(address, InterpreterRuntime::_new), Rcpool, Rindex);

  if (UseTLAB) {
    __ b(Ldone);
    // --------------------------------------------------------------------------
    // Init1: Zero out newly allocated memory.

    if (!ZeroTLAB || allow_shared_alloc) {
      // Clear object fields.
      __ bind(Linitialize_object);

      // Initialize remaining object fields.
      Register Rbase = Rtags;
      __ addi(Rinstance_size, Rinstance_size, 7 - (int)sizeof(oopDesc));
      __ addi(Rbase, RallocatedObject, sizeof(oopDesc));
      __ srdi(Rinstance_size, Rinstance_size, 3);

      // Clear out object skipping header. Takes also care of the zero length case.
      __ clear_memory_doubleword(Rbase, Rinstance_size);
      // fallthru: __ b(Linitialize_header);
    }

    // --------------------------------------------------------------------------
    // Init2: Initialize the header: mark, klass
    __ bind(Linitialize_header);

    // Init mark.
    if (UseBiasedLocking) {
      __ ld(Rscratch, in_bytes(Klass::prototype_header_offset()), RinstanceKlass);
    } else {
      __ load_const_optimized(Rscratch, markOopDesc::prototype(), R0);
    }
    __ std(Rscratch, oopDesc::mark_offset_in_bytes(), RallocatedObject);

    // Init klass.
    __ store_klass_gap(RallocatedObject);
    __ store_klass(RallocatedObject, RinstanceKlass, Rscratch); // klass (last for cms)

    // Check and trigger dtrace event.
    {
      SkipIfEqualZero skip_if(_masm, Rscratch, &DTraceAllocProbes);
      __ push(atos);
      __ call_VM_leaf(CAST_FROM_FN_PTR(address, SharedRuntime::dtrace_object_alloc));
      __ pop(atos);
    }
  }

  // continue
  __ bind(Ldone);

  // Must prevent reordering of stores for object initialization with stores that publish the new object.
  __ membar(Assembler::StoreStore);
  */
}

void TemplateTable::newarray() {
  __ bytecode_marker(0xbc);
  __ unimplemented(__LOCATION__);
  /*
  transition(itos, atos);

  __ lbz(R4, 1, X18_bcp);
  __ extsw(R5, X21_tos);
  call_VM(X21_tos, CAST_FROM_FN_PTR(address, InterpreterRuntime::newarray), R4, R5 */ /* size */ /*);

  // Must prevent reordering of stores for object initialization with stores that publish the new object.
  __ membar(Assembler::StoreStore);
  */
}

void TemplateTable::anewarray() {
  __ bytecode_marker(0xbd);
  __ unimplemented(__LOCATION__);
  /*
  transition(itos, atos);

  __ get_constant_pool(R4);
  __ get_2_byte_integer_at_bcp(1, R5, InterpreterMacroAssembler::Unsigned);
  __ extsw(R6, X21_tos); // size
  call_VM(X21_tos, CAST_FROM_FN_PTR(address, InterpreterRuntime::anewarray), R4 */ /* pool */ /*, R5 */ /* index */ /*, R6 */ /* size */ /*);

  // Must prevent reordering of stores for object initialization with stores that publish the new object.
  __ membar(Assembler::StoreStore);
  */
}

// Allocate a multi dimensional array
void TemplateTable::multianewarray() {
  __ bytecode_marker(0xc5);
  __ unimplemented(__LOCATION__);
  /*
  transition(vtos, atos);

  Register Rptr = R31; // Needs to survive C call.

  // Put ndims * wordSize into frame temp slot
  __ lbz(Rptr, 3, X18_bcp);
  __ sldi(Rptr, Rptr, Interpreter::logStackElementSize);
  // Esp points past last_dim, so set to R4 to first_dim address.
  __ add(R4, Rptr, X19_esp);
  call_VM(X21_tos, CAST_FROM_FN_PTR(address, InterpreterRuntime::multianewarray), R4 */ /* first_size_address */ /*);
  // Pop all dimensions off the stack.
  __ add(X19_esp, Rptr, X19_esp);

  // Must prevent reordering of stores for object initialization with stores that publish the new object.
  __ membar(Assembler::StoreStore);
  */
}

void TemplateTable::arraylength() {
  __ bytecode_marker(0xbe);
  __ unimplemented(__LOCATION__);
  /*
  transition(atos, itos);

  Label LnoException;
  __ verify_oop(X21_tos);
  __ null_check_throw(X21_tos, arrayOopDesc::length_offset_in_bytes(), X5_T0);
  __ lwa(X21_tos, arrayOopDesc::length_offset_in_bytes(), X21_tos);
  */
}

// ============================================================================
// Typechecks

void TemplateTable::checkcast() {
  __ bytecode_marker(0xc0);
  __ unimplemented(__LOCATION__);
  /*
  transition(atos, atos);

  Label Ldone, Lis_null, Lquicked, Lresolved;
  Register Roffset         = X13_ARG3,
           RobjKlass       = X11_ARG1,
           RspecifiedKlass = X12_ARG2, // Generate_ClassCastException_verbose_handler will read value from this register.
           Rcpool          = X5_T0,
           Rtags           = X6_T1;

  // Null does not pass.
  __ cmpdi(CCR0, X21_tos, 0);
  __ beq(CCR0, Lis_null);

  // Get constant pool tag to find out if the bytecode has already been "quickened".
  __ get_cpool_and_tags(Rcpool, Rtags);

  __ get_2_byte_integer_at_bcp(1, Roffset, InterpreterMacroAssembler::Unsigned);

  __ addi(Rtags, Rtags, Array<u1>::base_offset_in_bytes());
  __ lbzx(Rtags, Rtags, Roffset);

  __ cmpdi(CCR0, Rtags, JVM_CONSTANT_Class);
  __ beq(CCR0, Lquicked);

  // Call into the VM to "quicken" instanceof.
  __ push_ptr();  // for GC
  call_VM(noreg, CAST_FROM_FN_PTR(address, InterpreterRuntime::quicken_io_cc));
  __ get_vm_result_2(RspecifiedKlass);
  __ pop_ptr();   // Restore receiver.
  __ b(Lresolved);

  // Extract target class from constant pool.
  __ bind(Lquicked);
  __ sldi(Roffset, Roffset, LogBytesPerWord);
  __ addi(Rcpool, Rcpool, sizeof(ConstantPool));
  __ isync(); // Order load of specified Klass wrt. tags.
  __ ldx(RspecifiedKlass, Rcpool, Roffset);

  // Do the checkcast.
  __ bind(Lresolved);
  // Get value klass in RobjKlass.
  __ load_klass(RobjKlass, X21_tos);
  // Generate a fast subtype check. Branch to cast_ok if no failure. Return 0 if failure.
  __ gen_subtype_check(RobjKlass, RspecifiedKlass, */ /*3 temp regs*/ /* Roffset, Rcpool, Rtags, */ /*target if subtype*/ /* Ldone);

  // Not a subtype; so must throw exception
  // Target class oop is in register X13_ARG3 == RspecifiedKlass by convention.
  __ load_dispatch_table(X5_T0, (address*)Interpreter::_throw_ClassCastException_entry);
  __ mtctr(X5_T0);
  __ bctr();

  // Profile the null case.
  __ align(32, 12);
  __ bind(Lis_null);
  __ profile_null_seen(X5_T0, Rtags); // Rtags used as scratch.

  __ align(32, 12);
  __ bind(Ldone);
  */
}

// Output:
//   - tos == 0: Obj was null or not an instance of class.
//   - tos == 1: Obj was an instance of class.
void TemplateTable::instanceof() {
  __ bytecode_marker(0xc1);
  __ unimplemented(__LOCATION__);
  /*
  transition(atos, itos);

  Label Ldone, Lis_null, Lquicked, Lresolved;
  Register Roffset         = X12_ARG2,
           RobjKlass       = X11_ARG1,
           RspecifiedKlass = X13_ARG3, // Generate_ClassCastException_verbose_handler will expect the value in this register.
           Rcpool          = X5_T0,
           Rtags           = X6_T1;

  // Null does not pass.
  __ cmpdi(CCR0, X21_tos, 0);
  __ beq(CCR0, Lis_null);

  // Get constant pool tag to find out if the bytecode has already been "quickened".
  __ get_cpool_and_tags(Rcpool, Rtags);

  __ get_2_byte_integer_at_bcp(1, Roffset, InterpreterMacroAssembler::Unsigned);

  __ addi(Rtags, Rtags, Array<u1>::base_offset_in_bytes());
  __ lbzx(Rtags, Rtags, Roffset);

  __ cmpdi(CCR0, Rtags, JVM_CONSTANT_Class);
  __ beq(CCR0, Lquicked);

  // Call into the VM to "quicken" instanceof.
  __ push_ptr();  // for GC
  call_VM(noreg, CAST_FROM_FN_PTR(address, InterpreterRuntime::quicken_io_cc));
  __ get_vm_result_2(RspecifiedKlass);
  __ pop_ptr();   // Restore receiver.
  __ b(Lresolved);

  // Extract target class from constant pool.
  __ bind(Lquicked);
  __ sldi(Roffset, Roffset, LogBytesPerWord);
  __ addi(Rcpool, Rcpool, sizeof(ConstantPool));
  __ isync(); // Order load of specified Klass wrt. tags.
  __ ldx(RspecifiedKlass, Rcpool, Roffset);

  // Do the checkcast.
  __ bind(Lresolved);
  // Get value klass in RobjKlass.
  __ load_klass(RobjKlass, X21_tos);
  // Generate a fast subtype check. Branch to cast_ok if no failure. Return 0 if failure.
  __ li(X21_tos, 1);
  __ gen_subtype_check(RobjKlass, RspecifiedKlass, */ /*3 temp regs*/ /* Roffset, Rcpool, Rtags, */ /*target if subtype*/ /* Ldone);
  __ li(X21_tos, 0);

  if (ProfileInterpreter) {
    __ b(Ldone);
  }

  // Profile the null case.
  __ align(32, 12);
  __ bind(Lis_null);
  __ profile_null_seen(Rcpool, Rtags); // Rcpool and Rtags used as scratch.

  __ align(32, 12);
  __ bind(Ldone);
  */
}

// =============================================================================
// Breakpoints

void TemplateTable::_breakpoint() {
  __ unimplemented(__LOCATION__);
  /*
  transition(vtos, vtos);

  // Get the unpatched byte code.
  __ call_VM(noreg, CAST_FROM_FN_PTR(address, InterpreterRuntime::get_original_bytecode_at), R19_method, X18_bcp);
  __ mr(R31, R3_RET);

  // Post the breakpoint event.
  __ call_VM(noreg, CAST_FROM_FN_PTR(address, InterpreterRuntime::_breakpoint), R19_method, X18_bcp);

  // Complete the execution of original bytecode.
  __ dispatch_Lbyte_code(vtos, R31, Interpreter::normal_table(vtos));
  */
}

// =============================================================================
// Exceptions

void TemplateTable::athrow() {
  __ bytecode_marker(0xbf);
  __ unimplemented(__LOCATION__);
  /*
  transition(atos, vtos);

  // Exception oop is in tos
  __ verify_oop(X21_tos);

  __ null_check_throw(X21_tos, -1, X5_T0);

  // Throw exception interpreter entry expects exception oop to be in R3.
  __ mr(R3_RET, X21_tos);
  __ load_dispatch_table(X5_T0, (address*)Interpreter::throw_exception_entry());
  __ mtctr(X5_T0);
  __ bctr();
  */
}

// =============================================================================
// Synchronization
// Searches the basic object lock list on the stack for a free slot
// and uses it to lock the obect in tos.
//
// Recursive locking is enabled by exiting the search if the same
// object is already found in the list. Thus, a new basic lock obj lock
// is allocated "higher up" in the stack and thus is found first
// at next monitor exit.
void TemplateTable::monitorenter() {
  __ bytecode_marker(0xc2);
  __ unimplemented(__LOCATION__);
  /*
  transition(atos, vtos);

  __ verify_oop(X21_tos);

  Register Rcurrent_monitor  = X5_T0,
           Rcurrent_obj      = X6_T1,
           Robj_to_lock      = X21_tos,
           Rscratch1         = X10_ARG0,
           Rscratch2         = X11_ARG1,
           Rscratch3         = X12_ARG2,
           Rcurrent_obj_addr = X13_ARG3;

  // ------------------------------------------------------------------------------
  // Null pointer exception.
  __ null_check_throw(Robj_to_lock, -1, X5_T0);

  // Try to acquire a lock on the object.
  // Repeat until succeeded (i.e., until monitorenter returns true).

  // ------------------------------------------------------------------------------
  // Find a free slot in the monitor block.
  Label Lfound, Lexit, Lallocate_new;
  ConditionRegister found_free_slot = CCR0,
                    found_same_obj  = CCR1,
                    reached_limit   = CCR6;
  {
    Label Lloop, Lentry;
    Register Rlimit = Rcurrent_monitor;

    // Set up search loop - start with topmost monitor.
    __ add(Rcurrent_obj_addr, BasicObjectLock::obj_offset_in_bytes(), R26_monitor);

    __ ld(Rlimit, 0, R1_SP);
    __ addi(Rlimit, Rlimit, - (frame::ijava_state_size + frame::interpreter_frame_monitor_size_in_bytes() - BasicObjectLock::obj_offset_in_bytes())); // Monitor base

    // Check if any slot is present => short cut to allocation if not.
    __ cmpld(reached_limit, Rcurrent_obj_addr, Rlimit);
    __ bgt(reached_limit, Lallocate_new);

    // Pre-load topmost slot.
    __ ld(Rcurrent_obj, 0, Rcurrent_obj_addr);
    __ addi(Rcurrent_obj_addr, Rcurrent_obj_addr, frame::interpreter_frame_monitor_size() * wordSize);
    // The search loop.
    __ bind(Lloop);
    // Found free slot?
    __ cmpdi(found_free_slot, Rcurrent_obj, 0);
    // Is this entry for same obj? If so, stop the search and take the found
    // free slot or allocate a new one to enable recursive locking.
    __ cmpd(found_same_obj, Rcurrent_obj, Robj_to_lock);
    __ cmpld(reached_limit, Rcurrent_obj_addr, Rlimit);
    __ beq(found_free_slot, Lexit);
    __ beq(found_same_obj, Lallocate_new);
    __ bgt(reached_limit, Lallocate_new);
    // Check if last allocated BasicLockObj reached.
    __ ld(Rcurrent_obj, 0, Rcurrent_obj_addr);
    __ addi(Rcurrent_obj_addr, Rcurrent_obj_addr, frame::interpreter_frame_monitor_size() * wordSize);
    // Next iteration if unchecked BasicObjectLocks exist on the stack.
    __ b(Lloop);
  }

  // ------------------------------------------------------------------------------
  // Check if we found a free slot.
  __ bind(Lexit);

  __ addi(Rcurrent_monitor, Rcurrent_obj_addr, -(frame::interpreter_frame_monitor_size() * wordSize) - BasicObjectLock::obj_offset_in_bytes());
  __ addi(Rcurrent_obj_addr, Rcurrent_obj_addr, - frame::interpreter_frame_monitor_size() * wordSize);
  __ b(Lfound);

  // We didn't find a free BasicObjLock => allocate one.
  __ align(32, 12);
  __ bind(Lallocate_new);
  __ add_monitor_to_stack(false, Rscratch1, Rscratch2);
  __ mr(Rcurrent_monitor, R26_monitor);
  __ addi(Rcurrent_obj_addr, R26_monitor, BasicObjectLock::obj_offset_in_bytes());

  // ------------------------------------------------------------------------------
  // We now have a slot to lock.
  __ bind(Lfound);

  // Increment bcp to point to the next bytecode, so exception handling for async. exceptions work correctly.
  // The object has already been poped from the stack, so the expression stack looks correct.
  __ addi(X18_bcp, X18_bcp, 1);

  __ std(Robj_to_lock, 0, Rcurrent_obj_addr);
  __ lock_object(Rcurrent_monitor, Robj_to_lock);

  // Check if there's enough space on the stack for the monitors after locking.
  Label Lskip_stack_check;
  // Optimization: If the monitors stack section is less then a std page size (4K) don't run
  // the stack check. There should be enough shadow pages to fit that in.
  __ ld(Rscratch3, 0, R1_SP);
  __ sub(Rscratch3, Rscratch3, R26_monitor);
  __ cmpdi(CCR0, Rscratch3, 4*K);
  __ blt(CCR0, Lskip_stack_check);

  DEBUG_ONLY(__ untested("stack overflow check during monitor enter");)
  __ li(Rscratch1, 0);
  __ generate_stack_overflow_check_with_compare_and_throw(Rscratch1, Rscratch2);

  __ align(32, 12);
  __ bind(Lskip_stack_check);

  // The bcp has already been incremented. Just need to dispatch to next instruction.
  __ dispatch_next(vtos);
  */
}

void TemplateTable::monitorexit() {
  __ bytecode_marker(0xc3);
  __ unimplemented(__LOCATION__);
  /*
  transition(atos, vtos);
  __ verify_oop(X21_tos);

  Register Rcurrent_monitor  = X5_T0,
           Rcurrent_obj      = X6_T1,
           Robj_to_lock      = X21_tos,
           Rcurrent_obj_addr = X10_ARG0,
           Rlimit            = X11_ARG1;
  Label Lfound, Lillegal_monitor_state;

  // Check corner case: unbalanced monitorEnter / Exit.
  __ ld(Rlimit, 0, R1_SP);
  __ addi(Rlimit, Rlimit, - (frame::ijava_state_size + frame::interpreter_frame_monitor_size_in_bytes())); // Monitor base

  // Null pointer check.
  __ null_check_throw(Robj_to_lock, -1, X5_T0);

  __ cmpld(CCR0, R26_monitor, Rlimit);
  __ bgt(CCR0, Lillegal_monitor_state);

  // Find the corresponding slot in the monitors stack section.
  {
    Label Lloop;

    // Start with topmost monitor.
    __ addi(Rcurrent_obj_addr, R26_monitor, BasicObjectLock::obj_offset_in_bytes());
    __ addi(Rlimit, Rlimit, BasicObjectLock::obj_offset_in_bytes());
    __ ld(Rcurrent_obj, 0, Rcurrent_obj_addr);
    __ addi(Rcurrent_obj_addr, Rcurrent_obj_addr, frame::interpreter_frame_monitor_size() * wordSize);

    __ bind(Lloop);
    // Is this entry for same obj?
    __ cmpd(CCR0, Rcurrent_obj, Robj_to_lock);
    __ beq(CCR0, Lfound);

    // Check if last allocated BasicLockObj reached.

    __ ld(Rcurrent_obj, 0, Rcurrent_obj_addr);
    __ cmpld(CCR0, Rcurrent_obj_addr, Rlimit);
    __ addi(Rcurrent_obj_addr, Rcurrent_obj_addr, frame::interpreter_frame_monitor_size() * wordSize);

    // Next iteration if unchecked BasicObjectLocks exist on the stack.
    __ ble(CCR0, Lloop);
  }

  // Fell through without finding the basic obj lock => throw up!
  __ bind(Lillegal_monitor_state);
  call_VM(noreg, CAST_FROM_FN_PTR(address, InterpreterRuntime::throw_illegal_monitor_state_exception));
  __ should_not_reach_here();

  __ align(32, 12);
  __ bind(Lfound);
  __ addi(Rcurrent_monitor, Rcurrent_obj_addr,
          -(frame::interpreter_frame_monitor_size() * wordSize) - BasicObjectLock::obj_offset_in_bytes());
  __ unlock_object(Rcurrent_monitor);
  */
}

// ============================================================================
// Wide bytecodes

// Wide instructions. Simply redirects to the wide entry point for that instruction.
void TemplateTable::wide() {
  __ unimplemented(__LOCATION__);
  /*
  transition(vtos, vtos);

  const Register Rtable = X5_T0,
                 Rindex = X6_T1,
                 Rtmp   = R0;

  __ lbz(Rindex, 1, X18_bcp);

  __ load_dispatch_table(Rtable, Interpreter::_wentry_point);

  __ slwi(Rindex, Rindex, LogBytesPerWord);
  __ ldx(Rtmp, Rtable, Rindex);
  __ mtctr(Rtmp);
  __ bctr();
  // Note: the bcp increment step is part of the individual wide bytecode implementations.
  */
}
#endif // !CC_INTERP
