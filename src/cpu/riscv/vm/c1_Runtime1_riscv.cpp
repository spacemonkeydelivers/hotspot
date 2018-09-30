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
#include "asm/assembler.hpp"
#include "c1/c1_Defs.hpp"
#include "c1/c1_MacroAssembler.hpp"
#include "c1/c1_Runtime1.hpp"
#include "interpreter/interpreter.hpp"
#include "nativeInst_riscv.hpp"
#include "oops/compiledICHolder.hpp"
#include "oops/oop.inline.hpp"
#include "prims/jvmtiExport.hpp"
#include "register_riscv.hpp"
#include "runtime/sharedRuntime.hpp"
#include "runtime/signature.hpp"
#include "runtime/vframeArray.hpp"
#include "utilities/macros.hpp"
#include "vmreg_riscv.inline.hpp"
#if INCLUDE_ALL_GCS
#include "gc_implementation/g1/g1SATBCardTableModRefBS.hpp"
#endif

OopMapSet* Runtime1::generate_code_for(StubID id, StubAssembler* sasm) {
  // for better readability
  const bool must_gc_arguments = true;
  const bool dont_gc_arguments = false;


  // default value; overwritten for some optimized stubs that are called
  // from methods that do not use the fpu
  bool save_fpu_registers = true;


  // stub code & info for the different stubs
  OopMapSet* oop_maps = NULL;

  Unimplemented();
  #if 0
  switch (id) {
    case forward_exception_id:
      {
        oop_maps = generate_handle_exception(id, sasm);
        __ leave();
        __ jr(RA);
        __ delayed()->nop();
      }
      break;

    case new_instance_id:
    case fast_new_instance_id:
    case fast_new_instance_init_check_id:
      {
        Register klass = A4; // Incoming
        Register obj   = V0; // Result

        if (id == new_instance_id) {
          __ set_info("new_instance", dont_gc_arguments);
        } else if (id == fast_new_instance_id) {
          __ set_info("fast new_instance", dont_gc_arguments);
        } else {
          assert(id == fast_new_instance_init_check_id, "bad StubID");
          __ set_info("fast new_instance init check", dont_gc_arguments);
        }

        if ((id == fast_new_instance_id || id == fast_new_instance_init_check_id)
             && UseTLAB && FastTLABRefill) {
          Label slow_path;
          Register obj_size = T0;
          Register t1       = T2;
          Register t2       = T3;
          assert_different_registers(klass, obj, obj_size, t1, t2);
          if (id == fast_new_instance_init_check_id) {
            // make sure the klass is initialized
            __ ld_ptr(AT, Address(klass, in_bytes(InstanceKlass::init_state_offset())));
            __ move(t1, InstanceKlass::fully_initialized);
            __ bne(AT, t1, slow_path);
            __ delayed()->nop();
          }
#ifdef ASSERT
          // assert object can be fast path allocated
          {
            Label ok, not_ok;
            __ lw(obj_size, klass, in_bytes(Klass::layout_helper_offset()));
            __ blez(obj_size, not_ok);
            __ delayed()->nop();
            __ andi(t1 , obj_size, Klass::_lh_instance_slow_path_bit);
            __ beq(t1, R0, ok);
            __ delayed()->nop();
            __ bind(not_ok);
            __ stop("assert(can be fast path allocated)");
            __ should_not_reach_here();
            __ bind(ok);
          }
#endif // ASSERT
          // if we got here then the TLAB allocation failed, so try
          // refilling the TLAB or allocating directly from eden.

          Label retry_tlab, try_eden;
          __ tlab_refill(retry_tlab, try_eden, slow_path); // does not destroy edx (klass)

          __ bind(retry_tlab);

          // get the instance size
          __ lw(obj_size, klass, in_bytes(Klass::layout_helper_offset()));
          __ tlab_allocate(obj, obj_size, 0, t1, t2, slow_path);
          __ initialize_object(obj, klass, obj_size, 0, t1, t2);
          __ verify_oop(obj);
          __ jr(RA);
          __ delayed()->nop();

#ifndef OPT_THREAD
          const Register thread = T8;
          __ get_thread(thread);
#else
          const Register thread = TREG;
#endif

          __ bind(try_eden);

          // get the instance size
          __ lw(obj_size, klass, in_bytes(Klass::layout_helper_offset()));
          __ eden_allocate(obj, obj_size, 0, t1, t2, slow_path);
          __ incr_allocated_bytes(thread, obj_size, 0);

          __ initialize_object(obj, klass, obj_size, 0, t1, t2);
          __ verify_oop(obj);
          __ jr(RA);
          __ delayed()->nop();

          __ bind(slow_path);
        }
        __ enter();
        OopMap* map = save_live_registers(sasm, 0);
        int call_offset = __ call_RT(obj, noreg, CAST_FROM_FN_PTR(address, new_instance), klass);
        oop_maps = new OopMapSet();
        oop_maps->add_gc_map(call_offset, map);
        restore_live_registers_except_V0(sasm);
        __ verify_oop(obj);
        __ leave();
        __ jr(RA);
        __ delayed()->nop();

        // V0: new instance
      }
      break;


#ifdef TIERED
//FIXME, I hava no idea which register to use
    case counter_overflow_id:
      {
#ifndef _LP64
        Register bci = T5;
#else
        Register bci = A5;
#endif
        Register method = AT;
        __ enter();
        OopMap* map = save_live_registers(sasm, 0);
        // Retrieve bci
        __ lw(bci, Address(FP, 2*BytesPerWord));// FIXME:wuhui.ebp==??
        __ ld(method, Address(FP, 3*BytesPerWord));
        int call_offset = __ call_RT(noreg, noreg, CAST_FROM_FN_PTR(address, counter_overflow), bci, method);
        oop_maps = new OopMapSet();
        oop_maps->add_gc_map(call_offset, map);
        restore_live_registers(sasm);
        __ leave();
        __ jr(RA);
        __ delayed()->nop();
      }
      break;
#endif // TIERED



    case new_type_array_id:
    case new_object_array_id:
      {
        // i use T2 as length register, T4 as klass register, V0 as result register.
        // MUST accord with NewTypeArrayStub::emit_code, NewObjectArrayStub::emit_code
        Register length   = T2; // Incoming
#ifndef _LP64
        Register klass    = T4; // Incoming
#else
        Register klass    = A4; // Incoming
#endif
        Register obj      = V0; // Result

        if (id == new_type_array_id) {
          __ set_info("new_type_array", dont_gc_arguments);
        } else {
          __ set_info("new_object_array", dont_gc_arguments);
        }

        if (UseTLAB && FastTLABRefill) {
          Register arr_size = T0;
          Register t1       = T1;
          Register t2       = T3;
          Label slow_path;
          assert_different_registers(length, klass, obj, arr_size, t1, t2);

          // check that array length is small enough for fast path
          __ move(AT, C1_MacroAssembler::max_array_allocation_length);
          __ sltu(AT, AT, length);
          __ bne(AT, R0, slow_path);
          __ delayed()->nop();

          // if we got here then the TLAB allocation failed, so try
          // refilling the TLAB or allocating directly from eden.
          Label retry_tlab, try_eden;
          //T0,T1,T5,T8 have changed!
          __ tlab_refill(retry_tlab, try_eden, slow_path); // preserves T2 & T4

          __ bind(retry_tlab);

          // get the allocation size: (length << (layout_helper & 0x1F)) + header_size
          __ lw(t1, klass, in_bytes(Klass::layout_helper_offset()));
          __ andi(AT, t1, 0x1f);
          __ sllv(arr_size, length, AT);
          __ srl(t1, t1, Klass::_lh_header_size_shift);
          __ andi(t1, t1, Klass::_lh_header_size_mask);
          __ add(arr_size, t1, arr_size);
          __ addi(arr_size, arr_size, MinObjAlignmentInBytesMask);  // align up
          __ move(AT, ~MinObjAlignmentInBytesMask);
          __ andr(arr_size, arr_size, AT);


          __ tlab_allocate(obj, arr_size, 0, t1, t2, slow_path);  // preserves arr_size
          __ initialize_header(obj, klass, length,t1,t2);
          __ lbu(t1, Address(klass, in_bytes(Klass::layout_helper_offset())
                                    + (Klass::_lh_header_size_shift / BitsPerByte)));
          assert(Klass::_lh_header_size_shift % BitsPerByte == 0, "bytewise");
          assert(Klass::_lh_header_size_mask <= 0xFF, "bytewise");
          __ andi(t1, t1, Klass::_lh_header_size_mask);
          __ sub(arr_size, arr_size, t1);  // body length
          __ add(t1, t1, obj);             // body start
          __ initialize_body(t1, arr_size, 0, t2);
          __ verify_oop(obj);
          __ jr(RA);
          __ delayed()->nop();

#ifndef OPT_THREAD
          const Register thread = T8;
          __ get_thread(thread);
#else
          const Register thread = TREG;
#endif

          __ bind(try_eden);
          // get the allocation size: (length << (layout_helper & 0x1F)) + header_size
          __ lw(t1, klass, in_bytes(Klass::layout_helper_offset()));
          __ andi(AT, t1, 0x1f);
          __ sllv(arr_size, length, AT);
          __ srl(t1, t1, Klass::_lh_header_size_shift);
          __ andi(t1, t1, Klass::_lh_header_size_mask);
          __ add(arr_size, t1, arr_size);
          __ addi(arr_size, arr_size, MinObjAlignmentInBytesMask);  // align up
          __ move(AT, ~MinObjAlignmentInBytesMask);
          __ andr(arr_size, arr_size, AT);
          __ eden_allocate(obj, arr_size, 0, t1, t2, slow_path);  // preserves arr_size
          __ incr_allocated_bytes(thread, arr_size, 0);

          __ initialize_header(obj, klass, length,t1,t2);
          __ lbu(t1, Address(klass, in_bytes(Klass::layout_helper_offset())
                                    + (Klass::_lh_header_size_shift / BitsPerByte)));
          __ andi(t1, t1, Klass::_lh_header_size_mask);
          __ sub(arr_size, arr_size, t1);  // body length
          __ add(t1, t1, obj);             // body start

          __ initialize_body(t1, arr_size, 0, t2);
          __ verify_oop(obj);
          __ jr(RA);
          __ delayed()->nop();
          __ bind(slow_path);
        }


        __ enter();
        OopMap* map = save_live_registers(sasm, 0);
        int call_offset;
        if (id == new_type_array_id) {
          call_offset = __ call_RT(obj, noreg,
                                    CAST_FROM_FN_PTR(address, new_type_array), klass, length);
        } else {
          call_offset = __ call_RT(obj, noreg,
                                   CAST_FROM_FN_PTR(address, new_object_array), klass, length);
        }

        oop_maps = new OopMapSet();
        oop_maps->add_gc_map(call_offset, map);
        restore_live_registers_except_V0(sasm);
        __ verify_oop(obj);
        __ leave();
        __ jr(RA);
        __ delayed()->nop();
      }
      break;

    case new_multi_array_id:
      {
        StubFrame f(sasm, "new_multi_array", dont_gc_arguments);
       //refer to c1_LIRGenerate_mips.cpp:do_NewmultiArray
        // V0: klass
        // T2: rank
        // T0: address of 1st dimension
        //__ call_RT(V0, noreg, CAST_FROM_FN_PTR(address, new_multi_array), A1, A2, A3);
        //OopMap* map = save_live_registers(sasm, 4);
        OopMap* map = save_live_registers(sasm, 0);
        int call_offset = __ call_RT(V0, noreg, CAST_FROM_FN_PTR(address, new_multi_array),
            V0,T2,T0);
        oop_maps = new OopMapSet();
        oop_maps->add_gc_map(call_offset, map);
        //FIXME
        restore_live_registers_except_V0(sasm);
        // V0: new multi array
        __ verify_oop(V0);
      }
      break;


    case register_finalizer_id:
      {
        __ set_info("register_finalizer", dont_gc_arguments);

        // The object is passed on the stack and we haven't pushed a
        // frame yet so it's one work away from top of stack.
        //reference to LIRGenerator::do_RegisterFinalizer, call_runtime
        __ move(V0, A0);
        __ verify_oop(V0);
        // load the klass and check the has finalizer flag
        Label register_finalizer;
#ifndef _LP64
        Register t = T5;
#else
        Register t = A5;
#endif
        //__ ld_ptr(t, Address(V0, oopDesc::klass_offset_in_bytes()));
        __ load_klass(t, V0);
        __ lw(t, Address(t, Klass::access_flags_offset()));
        __ move(AT, JVM_ACC_HAS_FINALIZER);
        __ andr(AT, AT, t);

        __ bne(AT, R0, register_finalizer);
        __ delayed()->nop();
        __ jr(RA);
        __ delayed()->nop();
        __ bind(register_finalizer);
        __ enter();
        OopMap* map = save_live_registers(sasm, 0 /*num_rt_args */);

        int call_offset = __ call_RT(noreg, noreg, CAST_FROM_FN_PTR(address,
              SharedRuntime::register_finalizer), V0);
        oop_maps = new OopMapSet();
        oop_maps->add_gc_map(call_offset, map);

        // Now restore all the live registers
        restore_live_registers(sasm);

        __ leave();
        __ jr(RA);
        __ delayed()->nop();
      }
      break;

//  case range_check_failed_id:
  case throw_range_check_failed_id:
      {
        StubFrame f(sasm, "range_check_failed", dont_gc_arguments);
        oop_maps = generate_exception_throw(sasm, CAST_FROM_FN_PTR(address,
              throw_range_check_exception),true);
      }
      break;

      case throw_index_exception_id:
      {
        // i use A1 as the index register, for this will be the first argument, see call_RT
        StubFrame f(sasm, "index_range_check_failed", dont_gc_arguments);
        oop_maps = generate_exception_throw(sasm, CAST_FROM_FN_PTR(address,
              throw_index_exception), true);
      }
      break;

  case throw_div0_exception_id:
      { StubFrame f(sasm, "throw_div0_exception", dont_gc_arguments);
        oop_maps = generate_exception_throw(sasm, CAST_FROM_FN_PTR(address,
              throw_div0_exception), false);
      }
      break;

  case throw_null_pointer_exception_id:
      {
        StubFrame f(sasm, "throw_null_pointer_exception", dont_gc_arguments);
        oop_maps = generate_exception_throw(sasm, CAST_FROM_FN_PTR(address,
              throw_null_pointer_exception),false);
      }
      break;

  case handle_exception_nofpu_id:
    save_fpu_registers = false;
     // fall through
  case handle_exception_id:
    {
      StubFrame f(sasm, "handle_exception", dont_gc_arguments);
      //OopMap* oop_map = save_live_registers(sasm, 1, save_fpu_registers);
      oop_maps = generate_handle_exception(id, sasm);
    }
    break;
  case handle_exception_from_callee_id:
    {
      StubFrame f(sasm, "handle_exception_from_callee", dont_gc_arguments);
      oop_maps = generate_handle_exception(id, sasm);
    }
    break;
  case unwind_exception_id:
    {
      __ set_info("unwind_exception", dont_gc_arguments);
      generate_unwind_exception(sasm);
    }
    break;


  case throw_array_store_exception_id:
    {
      StubFrame f(sasm, "throw_array_store_exception", dont_gc_arguments);
      // tos + 0: link
      //     + 1: return address
      oop_maps = generate_exception_throw(sasm, CAST_FROM_FN_PTR(address,
            throw_array_store_exception), false);
    }
    break;

  case throw_class_cast_exception_id:
    {
      StubFrame f(sasm, "throw_class_cast_exception", dont_gc_arguments);
      oop_maps = generate_exception_throw(sasm, CAST_FROM_FN_PTR(address,
            throw_class_cast_exception), true);
    }
    break;

  case throw_incompatible_class_change_error_id:
    {
      StubFrame f(sasm, "throw_incompatible_class_cast_exception", dont_gc_arguments);
      oop_maps = generate_exception_throw(sasm,
            CAST_FROM_FN_PTR(address, throw_incompatible_class_change_error), false);
    }
    break;

  case slow_subtype_check_id:
    {
    //actually , We do not use it
      // A0:klass_RInfo    sub
      // A1:k->encoding() super
      __ set_info("slow_subtype_check", dont_gc_arguments);
      __ st_ptr(T0, SP, (-1) * wordSize);
      __ st_ptr(T1, SP, (-2) * wordSize);
      __ addiu(SP, SP, (-2) * wordSize);

      Label miss;
      __ check_klass_subtype_slow_path(A0, A1, T0, T1, NULL, &miss);

      __ addiu(V0, R0, 1);
      __ addiu(SP, SP, 2 * wordSize);
      __ ld_ptr(T0, SP, (-1) * wordSize);
      __ ld_ptr(T1, SP, (-2) * wordSize);
      __ jr(RA);
      __ delayed()->nop();


      __ bind(miss);
      __ move(V0, R0);
      __ addiu(SP, SP, 2 * wordSize);
      __ ld_ptr(T0, SP, (-1) * wordSize);
      __ ld_ptr(T1, SP, (-2) * wordSize);
      __ jr(RA);
      __ delayed()->nop();
    }
    break;

  case monitorenter_nofpu_id:
    save_fpu_registers = false;// fall through

  case monitorenter_id:
    {
      StubFrame f(sasm, "monitorenter", dont_gc_arguments);
      OopMap* map = save_live_registers(sasm, 0, save_fpu_registers);

      f.load_argument(1, V0); // V0: object
#ifndef _LP64
      f.load_argument(0, T6); // T6: lock address
      int call_offset = __ call_RT(noreg, noreg, CAST_FROM_FN_PTR(address,
           monitorenter), V0, T6);
#else
      f.load_argument(0, A6); // A6: lock address
      int call_offset = __ call_RT(noreg, noreg, CAST_FROM_FN_PTR(address,
           monitorenter), V0, A6);
#endif

      oop_maps = new OopMapSet();
      oop_maps->add_gc_map(call_offset, map);
      restore_live_registers(sasm, save_fpu_registers);
    }
    break;

  case monitorexit_nofpu_id:
    save_fpu_registers = false;
        // fall through
  case monitorexit_id:
    {
      StubFrame f(sasm, "monitorexit", dont_gc_arguments);
      OopMap* map = save_live_registers(sasm, 0, save_fpu_registers);

#ifndef _LP64
      f.load_argument(0, T6); // eax: lock address
#else
      f.load_argument(0, A6); // A6: lock address
#endif
      // note: really a leaf routine but must setup last java sp
      //       => use call_RT for now (speed can be improved by
      //       doing last java sp setup manually)
#ifndef _LP64
      int call_offset = __ call_RT(noreg, noreg,
                                    CAST_FROM_FN_PTR(address, monitorexit), T6);
#else
      int call_offset = __ call_RT(noreg, noreg,
                                    CAST_FROM_FN_PTR(address, monitorexit), A6);
#endif
      oop_maps = new OopMapSet();
      oop_maps->add_gc_map(call_offset, map);
      restore_live_registers(sasm, save_fpu_registers);

    }
    break;
        //  case init_check_patching_id:
  case access_field_patching_id:
    {
      StubFrame f(sasm, "access_field_patching", dont_gc_arguments);
      // we should set up register map
      oop_maps = generate_patching(sasm, CAST_FROM_FN_PTR(address, access_field_patching));

    }
    break;

  case load_klass_patching_id:
    {
      StubFrame f(sasm, "load_klass_patching", dont_gc_arguments);
      // we should set up register map
      oop_maps = generate_patching(sasm, CAST_FROM_FN_PTR(address,
            move_klass_patching));
    }
    break;
/*  case jvmti_exception_throw_id:
    {
      // V0: exception oop
      // V1: exception pc
      StubFrame f(sasm, "jvmti_exception_throw", dont_gc_arguments);
      // Preserve all registers across this potentially blocking call
      const int num_rt_args = 2;  // thread, exception oop
      //OopMap* map = save_live_registers(sasm, num_rt_args);
      OopMap* map = save_live_registers(sasm, 0);
      int call_offset = __ call_RT(noreg, noreg, CAST_FROM_FN_PTR(address,
            Runtime1::post_jvmti_exception_throw), V0);
      oop_maps = new OopMapSet();
      oop_maps->add_gc_map(call_offset,  map);
      restore_live_registers(sasm);
    }*/
  case load_mirror_patching_id:
    {
      StubFrame f(sasm, "load_mirror_patching" , dont_gc_arguments);
      oop_maps = generate_patching(sasm, CAST_FROM_FN_PTR(address, move_mirror_patching));
    }
    break;

  case load_appendix_patching_id:
    {
      StubFrame f(sasm, "load_appendix_patching", dont_gc_arguments);
      // we should set up register map
      oop_maps = generate_patching(sasm, CAST_FROM_FN_PTR(address, move_appendix_patching));
    }
    break;

  case dtrace_object_alloc_id:
    {
      // V0:object
      StubFrame f(sasm, "dtrace_object_alloc", dont_gc_arguments);
      // we can't gc here so skip the oopmap but make sure that all
      // the live registers get saved.
      save_live_registers(sasm, 0);

      __ push_reg(V0);
      __ move(A0, V0);
      __ call(CAST_FROM_FN_PTR(address, SharedRuntime::dtrace_object_alloc),
          relocInfo::runtime_call_type);
      __ delayed()->nop();
      __ super_pop(V0);

      restore_live_registers(sasm);
    }
    break;

  case fpu2long_stub_id:
    {
      //FIXME, I hava no idea how to port this
      //tty->print_cr("fpu2long_stub_id unimplemented yet!");
    }
    break;

  case deoptimize_id:
    {
      StubFrame f(sasm, "deoptimize", dont_gc_arguments);
      const int num_rt_args = 1;  // thread
      OopMap* oop_map = save_live_registers(sasm, num_rt_args);
      int call_offset = __ call_RT(noreg, noreg, CAST_FROM_FN_PTR(address, deoptimize));
      oop_maps = new OopMapSet();
      oop_maps->add_gc_map(call_offset, oop_map);
      restore_live_registers(sasm);
      DeoptimizationBlob* deopt_blob = SharedRuntime::deopt_blob();
      assert(deopt_blob != NULL, "deoptimization blob must have been created");
      __ leave();
      __ jmp(deopt_blob->unpack_with_reexecution(), relocInfo::runtime_call_type);
      __ delayed()->nop();
    }
   break;

  case predicate_failed_trap_id:
    {
      StubFrame f(sasm, "predicate_failed_trap", dont_gc_arguments);

      OopMap* map = save_live_registers(sasm, 1);

      int call_offset = __ call_RT(noreg, noreg, CAST_FROM_FN_PTR(address, predicate_failed_trap));
      oop_maps = new OopMapSet();
      oop_maps->add_gc_map(call_offset, map);
      restore_live_registers(sasm);
      __ leave();
      DeoptimizationBlob* deopt_blob = SharedRuntime::deopt_blob();
      assert(deopt_blob != NULL, "deoptimization blob must have been created");

      __ jmp(deopt_blob->unpack_with_reexecution(), relocInfo::runtime_call_type);
      __ delayed()->nop();
    }
   break;

  default:
    {
      StubFrame f(sasm, "unimplemented entry", dont_gc_arguments);
      __ move(A1, (int)id);
      __ call_RT(noreg, noreg, CAST_FROM_FN_PTR(address, unimplemented_entry), A1);
      __ should_not_reach_here();
    }
    break;
  }
  #endif
  return oop_maps;
}

void Runtime1::initialize_pd() {
  // nothing to do
}

#undef __

const char *Runtime1::pd_name_for_address(address entry) {
  return "<unknown function>";
}
