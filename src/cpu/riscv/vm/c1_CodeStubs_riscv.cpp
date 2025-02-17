/*
 * Copyright (c) 1999, 2013, Oracle and/or its affiliates. All rights reserved.
 * Copyright (c) 2015, 2018, Loongson Technology. All rights reserved.
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
#include "c1/c1_CodeStubs.hpp"
#include "c1/c1_FrameMap.hpp"
#include "c1/c1_LIRAssembler.hpp"
#include "c1/c1_MacroAssembler.hpp"
#include "c1/c1_Runtime1.hpp"
#include "nativeInst_riscv.hpp"
#include "runtime/sharedRuntime.hpp"
#include "utilities/macros.hpp"
#include "vmreg_riscv.inline.hpp"
#if INCLUDE_ALL_GCS
#include "gc_implementation/g1/g1SATBCardTableModRefBS.hpp"
#endif


#define __ ce->masm()->

float ConversionStub::float_zero = 0.0;
double ConversionStub::double_zero = 0.0;

void ConversionStub::emit_code(LIR_Assembler* ce) {
  __ bind(_entry);
  assert(bytecode() == Bytecodes::_f2i || bytecode() == Bytecodes::_d2i, "other conversions do not require stub");
}

#ifdef TIERED
void CounterOverflowStub::emit_code(LIR_Assembler* ce) {
  Unimplemented();
  #if 0
  __ bind(_entry);
  ce->store_parameter(_method->as_register(), 1);
  ce->store_parameter(_bci, 0);
  __ call(Runtime1::entry_for(Runtime1::counter_overflow_id), relocInfo::runtime_call_type);
  __ delayed()->nop();
  ce->add_call_info_here(_info);
  ce->verify_oop_map(_info);

  __ b_far(_continuation);
  __ delayed()->nop();
  #endif
}
#endif // TIERED

RangeCheckStub::RangeCheckStub(CodeEmitInfo* info, LIR_Opr index,
                               bool throw_index_out_of_bounds_exception)
  : _throw_index_out_of_bounds_exception(throw_index_out_of_bounds_exception)
  , _index(index)
{
  _info = info == NULL ? NULL : new CodeEmitInfo(info);
}


void RangeCheckStub::emit_code(LIR_Assembler* ce) {
  Unimplemented();
  #if 0
  __ bind(_entry);
  if (_info->deoptimize_on_exception()) {
    address a = Runtime1::entry_for(Runtime1::predicate_failed_trap_id);
    __ call(a, relocInfo::runtime_call_type);
    __ delayed()->nop();
    ce->add_call_info_here(_info);
    ce->verify_oop_map(_info);
    debug_only(__ should_not_reach_here());
    return;
  }

  // pass the array index on stack because all registers must be preserved
  if (_index->is_cpu_register()) {
    ce->store_parameter(_index->as_register(), 0);
  } else {
    ce->store_parameter(_index->as_jint(), 0);
  }
  Runtime1::StubID stub_id;
  if (_throw_index_out_of_bounds_exception) {
    stub_id = Runtime1::throw_index_exception_id;
  } else {
    stub_id = Runtime1::throw_range_check_failed_id;
  }
  __ call(Runtime1::entry_for(stub_id), relocInfo::runtime_call_type);
  __ delayed()->nop();
  ce->add_call_info_here(_info);
  ce->verify_oop_map(_info);
  debug_only(__ should_not_reach_here());
  #endif
}

PredicateFailedStub::PredicateFailedStub(CodeEmitInfo* info) {
  _info = new CodeEmitInfo(info);
}


void PredicateFailedStub::emit_code(LIR_Assembler* ce) {
  Unimplemented();
  #if 0
  __ bind(_entry);
  address a = Runtime1::entry_for(Runtime1::predicate_failed_trap_id);
  __ call(a, relocInfo::runtime_call_type);
  __ delayed()->nop();
  ce->add_call_info_here(_info);
  ce->verify_oop_map(_info);
  debug_only(__ should_not_reach_here());
  #endif
}

void DivByZeroStub::emit_code(LIR_Assembler* ce) {
  Unimplemented();
  #if 0
  if (_offset != -1) {
    ce->compilation()->implicit_exception_table()->append(_offset, __ offset());
  }
  __ bind(_entry);
  __ call(Runtime1::entry_for(Runtime1::throw_div0_exception_id), relocInfo::runtime_call_type);
  __ delayed()->nop();
  ce->add_call_info_here(_info);
  debug_only(__ should_not_reach_here());
  #endif
}


// Implementation of NewInstanceStub

NewInstanceStub::NewInstanceStub(LIR_Opr klass_reg, LIR_Opr result, ciInstanceKlass* klass, CodeEmitInfo* info, Runtime1::StubID stub_id) {
  _result = result;
  _klass = klass;
  _klass_reg = klass_reg;
  _info = new CodeEmitInfo(info);
  assert(stub_id == Runtime1::new_instance_id                 ||
         stub_id == Runtime1::fast_new_instance_id            ||
         stub_id == Runtime1::fast_new_instance_init_check_id,
         "need new_instance id");
  _stub_id   = stub_id;
}

// i use T4 as klass register, V0 as result register. MUST accord with Runtime1::generate_code_for.
void NewInstanceStub::emit_code(LIR_Assembler* ce) {
  Unimplemented();
  #if 0
  assert(__ sp_offset() == 0, "frame size should be fixed");
  __ bind(_entry);
#ifndef _LP64
  assert(_klass_reg->as_register() == T4, "klass_reg must in T4");
#else
  assert(_klass_reg->as_register() == A4, "klass_reg must in A4");
#endif
  __ call(Runtime1::entry_for(_stub_id), relocInfo::runtime_call_type);
  __ delayed()->nop();
  ce->add_call_info_here(_info);
  ce->verify_oop_map(_info);
  assert(_result->as_register() == V0, "result must in V0,");
  __ b_far(_continuation);
  __ delayed()->nop();
  #endif
}


// Implementation of NewTypeArrayStub

NewTypeArrayStub::NewTypeArrayStub(LIR_Opr klass_reg, LIR_Opr length, LIR_Opr result, CodeEmitInfo* info) {
  _klass_reg = klass_reg;
  _length = length;
  _result = result;
  _info = new CodeEmitInfo(info);
}

// i use T2 as length register, T4 as klass register, V0 as result register.
// MUST accord with Runtime1::generate_code_for
void NewTypeArrayStub::emit_code(LIR_Assembler* ce) {
  Unimplemented();
  #if 0
  assert(__ sp_offset() == 0, "frame size should be fixed");
  __ bind(_entry);
  assert(_length->as_register() == T2, "length must in T2,");
#ifndef _LP64
  assert(_klass_reg->as_register() == T4, "klass_reg must in T4");
#else
  assert(_klass_reg->as_register() == A4, "klass_reg must in A4");
#endif

  __ call(Runtime1::entry_for(Runtime1::new_type_array_id), relocInfo::runtime_call_type);
  __ delayed()->nop();
  ce->add_call_info_here(_info);
  ce->verify_oop_map(_info);

  assert(_result->as_register() == V0, "result must in V0,");
  __ b_far(_continuation);
  __ delayed()->nop();
  #endif
}


// Implementation of NewObjectArrayStub

NewObjectArrayStub::NewObjectArrayStub(LIR_Opr klass_reg, LIR_Opr length, LIR_Opr result, CodeEmitInfo* info) {
  _klass_reg = klass_reg;
  _result = result;
  _length = length;
  _info = new CodeEmitInfo(info);
}


void NewObjectArrayStub::emit_code(LIR_Assembler* ce) {
  Unimplemented();
  #if 0
  assert(__ sp_offset() == 0, "frame size should be fixed");
  __ bind(_entry);
  assert(_length->as_register() == T2, "length must in T2");
#ifndef _LP64
  assert(_klass_reg->as_register() == T4, "klass_reg must in T4");
#else
  assert(_klass_reg->as_register() == A4, "klass_reg must in A4");
#endif
  __ call(Runtime1::entry_for(Runtime1::new_object_array_id), relocInfo::runtime_call_type);
  __ delayed()->nop();
  ce->add_call_info_here(_info);
  ce->verify_oop_map(_info);
  assert(_result->as_register() == V0, "result must in V0");
  __ b_far(_continuation);
  __ delayed()->nop();
  #endif
}


// Implementation of MonitorAccessStubs

MonitorEnterStub::MonitorEnterStub(LIR_Opr obj_reg, LIR_Opr lock_reg, CodeEmitInfo* info)
: MonitorAccessStub(obj_reg, lock_reg)
{
  _info = new CodeEmitInfo(info);
}


void MonitorEnterStub::emit_code(LIR_Assembler* ce) {
  Unimplemented();
  #if 0
  assert(__ sp_offset() == 0, "frame size should be fixed");
  __ bind(_entry);
  ce->store_parameter(_obj_reg->as_register(),  1);
  ce->store_parameter(_lock_reg->is_single_cpu()? _lock_reg->as_register() : _lock_reg->as_register_lo(), 0);
  Runtime1::StubID enter_id;
  if (ce->compilation()->has_fpu_code()) {
    enter_id = Runtime1::monitorenter_id;
  } else {
    enter_id = Runtime1::monitorenter_nofpu_id;
  }
  __ call(Runtime1::entry_for(enter_id), relocInfo::runtime_call_type);
  __ delayed()->nop();
  ce->add_call_info_here(_info);
  ce->verify_oop_map(_info);
  __ b_far(_continuation);
  __ delayed()->nop();
  #endif
}


void MonitorExitStub::emit_code(LIR_Assembler* ce) {
  Unimplemented();
  #if 0
  __ bind(_entry);
  if (_compute_lock) {
    // lock_reg was destroyed by fast unlocking attempt => recompute it
    ce->monitor_address(_monitor_ix, _lock_reg);
  }
  ce->store_parameter(_lock_reg->as_register(), 0);
  // note: non-blocking leaf routine => no call info needed
  Runtime1::StubID exit_id;
  if (ce->compilation()->has_fpu_code()) {
    exit_id = Runtime1::monitorexit_id;
  } else {
    exit_id = Runtime1::monitorexit_nofpu_id;
  }
  __ call(Runtime1::entry_for(exit_id), relocInfo::runtime_call_type);
  __ delayed()->nop();

  __ b_far(_continuation);
  __ delayed()->nop();
  #endif
}


// Implementation of patching:
// - Copy the code at given offset to an inlined buffer (first the bytes, then the number of bytes)
// - Replace original code with a call to the stub
// At Runtime:
// - call to stub, jump to runtime
// - in runtime: preserve all registers (especially objects, i.e., source and destination object)
// - in runtime: after initializing class, restore original code, reexecute instruction

//int PatchingStub::_patch_info_offset = -NativeGeneralJump::instruction_size;
int PatchingStub::_patch_info_offset = -NativeCall::instruction_size;

void PatchingStub::align_patch_site(MacroAssembler* masm) {
  masm->align(round_to(NativeGeneralJump::instruction_size, wordSize));
}

void PatchingStub::emit_code(LIR_Assembler* ce) {
  Unimplemented();
  #if 0
  assert(NativeCall::instruction_size <= _bytes_to_copy && _bytes_to_copy <= 0xFF, "not enough room for call");

  Label call_patch;

  // static field accesses have special semantics while the class
  // initializer is being run so we emit a test which can be used to
  // check that this code is being executed by the initializing
  // thread.
  address being_initialized_entry = __ pc();
  if (CommentedAssembly) {
    __ block_comment(" patch template");
  }
  if (_id == load_klass_id) {
    // produce a copy of the load klass instruction for use by the being initialized case
//#ifdef ASSERT
    address start = __ pc();
//#endif
    Metadata* o = NULL;
    RelocationHolder rspec = metadata_Relocation::spec(_index);
    __ relocate(rspec);
    __ li48(_obj, (long)o);
    while ((intx)__ pc() - (intx)start < NativeCall::instruction_size) {
      __ nop();
    }
#ifdef ASSERT
    for (int i = 0; i < _bytes_to_copy; i++) {
      address ptr = (address)(_pc_start + i);
      int a_byte = (*ptr) & 0xFF;
      assert(a_byte == *start++, "should be the same code");
    }
#endif
  } else if (_id == load_mirror_id || _id == load_appendix_id) {
    address start = __ pc();
    jobject o = NULL;
    RelocationHolder rspec = oop_Relocation::spec(_index);
    __ relocate(rspec);
    __ li48(_obj, (long)o);
    while ((intx)__ pc() - (intx)start < NativeCall::instruction_size) {
      __ nop();
    }
#ifdef ASSERT
    for (int i = 0; i < _bytes_to_copy; i++) {
      address ptr = (address)(_pc_start + i);
      int a_byte = (*ptr) & 0xFF;
      assert(a_byte == *start++, "should be the same code");
    }
#endif
  } else {
    // make a copy the code which is going to be patched.
    assert((_bytes_to_copy&3)==0, "change this code");
    address start = __ pc();
    for ( int i = 0; i < _bytes_to_copy; i+=4) {
      __ emit_int32(*(int*)(_pc_start + i));
      //make the site look like a nop
      *(int*)(_pc_start + i)=0;
    }
    while ((intx)__ pc() - (intx)start < NativeCall::instruction_size) {
      __ nop();
    }
  }

  address end_of_patch = __ pc();
  int bytes_to_skip = 0;
  if (_id == load_mirror_id) {
    int offset = __ offset();
    if (CommentedAssembly) {
      __ block_comment(" being_initialized check");
    }
    assert(_obj != NOREG, "must be a valid register");
#ifndef OPT_THREAD
    Register tmp = AT;
    __ get_thread(tmp);
#else
    Register tmp = TREG;
#endif
    Register tmp2 = T9;
    __ ld_ptr(tmp2, Address(_obj, java_lang_Class::klass_offset_in_bytes()));
    __ ld_ptr(tmp2, Address(tmp2, InstanceKlass::init_thread_offset()));
    __ bne(tmp, tmp2, call_patch);
    __ delayed()->nop();

    // access_field patches may execute the patched code before it's
    // copied back into place so we need to jump back into the main
    // code of the nmethod to continue execution.
    __ b_far(_patch_site_continuation);
    __ delayed()->nop();
    bytes_to_skip += __ offset() - offset;

  }

  if (CommentedAssembly) {
    __ block_comment("patch data");
  }
  // Now emit the patch record telling the runtime how to find the
  // pieces of the patch.  We only need 3 bytes but for alignment, we
  // need 4 bytes
  int sizeof_patch_record = 4;
  bytes_to_skip += sizeof_patch_record;

  // emit the offsets needed to find the code to patch
  int being_initialized_entry_offset = __ pc() - being_initialized_entry + sizeof_patch_record;

  // patch_info_pc offset | size of b instruction(8)| patched code size
  assert(being_initialized_entry_offset % 4 == 0, "must be divided by four");
  assert(bytes_to_skip % 4 == 0, "must be divided by four");
  // Fix out of range of char. refer to load_klass_or_mirror_patch_id.
  being_initialized_entry_offset >>= 2;
  bytes_to_skip >>= 2;
  assert((char)being_initialized_entry_offset == being_initialized_entry_offset, "just check");
  assert((char)bytes_to_skip == bytes_to_skip, "just check");
  assert((char)_bytes_to_copy == _bytes_to_copy, "just check");
  __ emit_int32(being_initialized_entry_offset<<8 | (bytes_to_skip<<16) | (_bytes_to_copy<<24) );

  address patch_info_pc = __ pc();
  // FIXME: byte_skip can not be contained in a byte
  bytes_to_skip <<= 2;
  assert(patch_info_pc - end_of_patch == bytes_to_skip, "incorrect patch info");

  address entry = __ pc();
  NativeGeneralJump::insert_unconditional((address)_pc_start, entry);
  address target = NULL;
  relocInfo::relocType reloc_type = relocInfo::none;
  switch (_id) {
    case access_field_id:  target = Runtime1::entry_for(Runtime1::access_field_patching_id); break;
    case load_klass_id:    target = Runtime1::entry_for(Runtime1::load_klass_patching_id); reloc_type = relocInfo::metadata_type; break;
    case load_mirror_id:   target = Runtime1::entry_for(Runtime1::load_mirror_patching_id); reloc_type = relocInfo::oop_type; break;
    case load_appendix_id: target = Runtime1::entry_for(Runtime1::load_appendix_patching_id); reloc_type = relocInfo::oop_type; break;
    default: ShouldNotReachHere();
  }
  __ bind(call_patch);


  if (CommentedAssembly) {
    __ block_comment("patch entry point");
  }
  __ relocate(relocInfo::runtime_call_type);
  __ li48(T9, (long)target);
  __ jalr(T9);
  __ delayed()->nop();
  assert(_patch_info_offset == (patch_info_pc - __ pc()), "must not change");
  ce->add_call_info_here(_info);
  int jmp_off = __ offset();
  __ b_far(_patch_site_entry);
  __ delayed()->nop();
  // Add enough nops so deoptimization can overwrite the jmp above with a call
  // and not destroy the world.
  for (int j = __ offset(); j < jmp_off + NativeCall::instruction_size; j += 4 ) {
    __ nop();
  }
  if (_id == load_klass_id || _id == load_mirror_id || _id == load_appendix_id) {
    CodeSection* cs = __ code_section();
    address pc = (address)_pc_start;
    RelocIterator iter(cs, pc, pc + 1);
    relocInfo::change_reloc_info_for_address(&iter, pc, reloc_type, relocInfo::none);
  }
  #endif 
}


void DeoptimizeStub::emit_code(LIR_Assembler* ce) {
  Unimplemented();
  #if 0
  __ bind(_entry);
  __ call(Runtime1::entry_for(Runtime1::deoptimize_id), relocInfo::runtime_call_type);
  __ delayed()->nop();
  ce->add_call_info_here(_info);
  DEBUG_ONLY(__ should_not_reach_here());
  #endif
}


void ImplicitNullCheckStub::emit_code(LIR_Assembler* ce) {
  Unimplemented();
  #if 0
  ce->compilation()->implicit_exception_table()->append(_offset, __ offset());
  __ bind(_entry);
  __ call(Runtime1::entry_for(Runtime1::throw_null_pointer_exception_id), relocInfo::runtime_call_type);
  __ delayed()->nop();
  ce->add_call_info_here(_info);
  debug_only(__ should_not_reach_here());
  #endif
}


// i dont know which register to use here, i just assume A1 here. FIXME
void SimpleExceptionStub::emit_code(LIR_Assembler* ce) {
  Unimplemented();
  #if 0
  assert(__ sp_offset() == 0, "frame size should be fixed");

  __ bind(_entry);
  // pass the object on stack because all registers must be preserved
  if (_obj->is_cpu_register()) {
    ce->store_parameter(_obj->as_register(), 0);
  }
  __ call(Runtime1::entry_for(_stub), relocInfo::runtime_call_type);
  __ delayed()->nop();
  ce->add_call_info_here(_info);
  debug_only(__ should_not_reach_here());
  #endif
}


void ArrayCopyStub::emit_code(LIR_Assembler* ce) {
  Unimplemented();
  #if 0
  //---------------slow case: call to native-----------------
  __ bind(_entry);
  // Figure out where the args should go
  // This should really convert the IntrinsicID to the methodOop and signature
  // but I don't know how to do that.
  //
  VMRegPair args[5];
  BasicType signature[5] = { T_OBJECT, T_INT, T_OBJECT, T_INT, T_INT};
  SharedRuntime::java_calling_convention(signature, args, 5, true);

  // push parameters
  // (src, src_pos, dest, destPos, length)
  Register r[5];
  r[0] = src()->as_register();
  r[1] = src_pos()->as_register();
  r[2] = dst()->as_register();
  r[3] = dst_pos()->as_register();
  r[4] = length()->as_register();

  // next registers will get stored on the stack
  for (int i = 0; i < 5 ; i++ ) {
    VMReg r_1 = args[i].first();
    if (r_1->is_stack()) {
      int st_off = r_1->reg2stack() * wordSize;
      __ sw( r[i],  SP, st_off);
    } else {
      assert(r[i] == args[i].first()->as_Register(), "Wrong register for arg ");
    }
  }

  ce->align_call(lir_static_call);

  ce->emit_static_call_stub();
  __ call(SharedRuntime::get_resolve_static_call_stub(), relocInfo::static_call_type);
  __ delayed()->nop();
  ce->add_call_info_here(info());

#ifndef PRODUCT
#ifndef _LP64
  __ lui(T8, Assembler::split_high((int)&Runtime1::_arraycopy_slowcase_cnt));
  __ lw(AT, T8, Assembler::split_low((int)&Runtime1::_arraycopy_slowcase_cnt));
  __ addiu(AT, AT, 1);
  __ sw(AT, T8, Assembler::split_low((int)&Runtime1::_arraycopy_slowcase_cnt));
#else
  __ li(T8, (long)&Runtime1::_arraycopy_slowcase_cnt);
  __ lw(AT, T8, 0);
  __ daddiu(AT, AT, 1);
  __ sw(AT, T8, 0);
#endif
#endif

  __ b_far(_continuation);
  __ delayed()->nop();
  #endif
}

/////////////////////////////////////////////////////////////////////////////
#if INCLUDE_ALL_GCS

void G1PreBarrierStub::emit_code(LIR_Assembler* ce) {
  Unimplemented();
}

void G1PostBarrierStub::emit_code(LIR_Assembler* ce) {
  Unimplemented();
}

#endif // INCLUDE_ALL_GCS
/////////////////////////////////////////////////////////////////////////////

#undef __
