/*
 * Copyright (c) 2002, 2013, Oracle and/or its affiliates. All rights reserved.
 * Copyright 2012, 2014 SAP AG. All rights reserved.
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

#ifndef CPU_RISCV_VM_MACROASSEMBLER_RISCV_HPP
#define CPU_RISCV_VM_MACROASSEMBLER_RISCV_HPP

#include "asm/assembler.hpp"

// MacroAssembler extends Assembler by a few frequently used macros.

class ciTypeArray;

class MacroAssembler: public Assembler {
 public:
  MacroAssembler(CodeBuffer* code) : Assembler(code) {}

  //
  // Optimized instruction emitters
  //

  inline static int largeoffset_si16_si16_hi(int si31) { return (si31 + (1<<15)) >> 16; }
  inline static int largeoffset_si16_si16_lo(int si31) { return si31 - (((si31 + (1<<15)) >> 16) << 16); }

  inline void round_to(Register r, int modulus);

  // Move register if destination register and target register are different
  inline void mv_if_needed(Register rd, Register rs);
  inline void fmv_s_if_needed(FloatRegister rd, FloatRegister rs);
  inline void fmv_d_if_needed(FloatRegister rd, FloatRegister rs);
  inline void endgroup_if_needed(bool needed);

  // Memory barriers.
  inline void membar(int bits);
  inline void release();
  inline void acquire();
  inline void fence();

  // nop padding
  void align(int modulus, int max = 252, int rem = 0);

#ifdef _LP64
  // Patch narrow oop constant.
  inline static bool is_set_narrow_oop(address a, address bound);
  static int patch_set_narrow_oop(address a, address bound, narrowOop data);
  static narrowOop get_narrow_oop(address a, address bound);
#endif

  inline static bool is_load_const_at(address a);

  // Get the 64 bit constant from a `load_const' sequence.
  static long get_const_patchable(address load_const);

  // Patch the 64 bit constant of a `load_const' sequence. This is a
  // low level procedure. It neither flushes the instruction cache nor
  // is it atomic.
  static void patch_const(address load_const, long x);

  // Metadata in code that we have to keep track of.
  AddressLiteral allocate_metadata_address(Metadata* obj); // allocate_index
  AddressLiteral constant_metadata_address(Metadata* obj); // find_index
  // Oops used directly in compiled code are stored in the constant pool,
  // and loaded from there.
  // Allocate new entry for oop in constant pool. Generate relocation.
  AddressLiteral allocate_oop_address(jobject obj);
  // Find oop obj in constant pool. Return relocation with it's index.
  AddressLiteral constant_oop_address(jobject obj);

  // Find oop in constant pool and emit instructions to load it.
  // Uses constant_oop_address.
  inline void set_oop_constant(jobject obj, Register d);
  // Same as load_address.
  inline void set_oop         (AddressLiteral obj_addr, Register d);

  // Read runtime constant:  Issue load if constant not yet established,
  // else use real constant.
  virtual RegisterOrConstant delayed_value_impl(intptr_t* delayed_value_addr,
                                                Register tmp,
                                                int offset);

  //
  // branch, jump
  //

  inline void pd_patch_instruction(address branch, address target);
  NOT_PRODUCT(static void pd_print_patched_instruction(address branch);)

  // Emit, identify and patch a NOT mt-safe patchable 64 bit absolute call/jump.
 public:
  enum {
    jump64_patchable_instruction_count = (8/*load_const_patchable*/ + 1/*jump*/),
    jump64_patchable_size              = jump64_patchable_instruction_count * BytesPerInstWord,
  };
  void jump64_patchable(                         address target, relocInfo::relocType rt, bool and_link);
  static bool is_jump64_patchable_at(            address instruction_addr, bool and_link);
  static void set_dest_of_jump64_patchable_at(   address instruction_addr, address target, bool and_link);
  static address get_dest_of_jump64_patchable_at(address instruction_addr, bool and_link);

  //
  // Support for frame handling
  //

  // some ABI-related functions
  void save_nonvolatile_gprs(   Register dst_base, int offset);
  void restore_nonvolatile_gprs(Register src_base, int offset);
  void save_volatile_gprs(   Register dst_base, int offset);
  void restore_volatile_gprs(Register src_base, int offset);

  // Resize current frame either relatively wrt to current SP or absolute.
  void resize_frame(Register offset, Register tmp);
  void resize_frame(int      offset, Register tmp);
  void resize_frame_absolute(Register addr, Register tmp1, Register tmp2);

  // Push a frame of size bytes.
  void push_frame(Register bytes, Register tmp);

  // Push a frame of size `bytes'. No abi space provided.
  void push_frame(unsigned int bytes, Register tmp);

  // pop current C frame
  void pop_frame();

  //
  // Calls
  //

 private:
  address _last_calls_return_pc;

  // Generic version of a call to C function via an address in a register
  // updates and returns _last_calls_return_pc.
  address branch_to(Register fn_entry, bool and_link);

 public:

  // Get the pc where the last call will return to. returns _last_calls_return_pc.
  inline address last_calls_return_pc();

  // Call a C function via a function descriptor and use full C
  // calling conventions. Updates and returns _last_calls_return_pc.
  address call_c(Register function_descriptor);
  // For tail calls: only branch, don't link, so callee returns to caller of this function.
  address call_c_and_return_to_caller(Register function_descriptor);
  address call_c(address fn_entry, relocInfo::relocType rt);

 protected:

  // It is imperative that all calls into the VM are handled via the
  // call_VM macros. They make sure that the stack linkage is setup
  // correctly. call_VM's correspond to ENTRY/ENTRY_X entry points
  // while call_VM_leaf's correspond to LEAF entry points.
  //
  // This is the base routine called by the different versions of
  // call_VM. The interpreter may customize this version by overriding
  // it for its purposes (e.g., to save/restore additional registers
  // when doing a VM call).
  //
  // If no last_java_sp is specified (noreg) then SP will be used instead.
  virtual void call_VM_base(
     // where an oop-result ends up if any; use noreg otherwise
    Register        oop_result,
    // to set up last_Java_frame in stubs; use noreg otherwise
    Register        last_java_sp,
    // the entry point
    address         entry_point,
    // flag which indicates if exception should be checked
    bool            check_exception = true
  );

  // Support for VM calls. This is the base routine called by the
  // different versions of call_VM_leaf. The interpreter may customize
  // this version by overriding it for its purposes (e.g., to
  // save/restore additional registers when doing a VM call).
  void call_VM_leaf_base(address entry_point);

 public:
  // Call into the VM.
  // Passes the thread pointer (in R3_ARG1) as a prepended argument.
  // Makes sure oop return values are visible to the GC.
  void call_VM(Register oop_result, address entry_point, bool check_exceptions = true);
  void call_VM(Register oop_result, address entry_point, Register arg_1, bool check_exceptions = true);
  void call_VM(Register oop_result, address entry_point, Register arg_1, Register arg_2, bool check_exceptions = true);
  void call_VM(Register oop_result, address entry_point, Register arg_1, Register arg_2, Register arg3, bool check_exceptions = true);
  void call_VM_leaf(address entry_point);
  void call_VM_leaf(address entry_point, Register arg_1);
  void call_VM_leaf(address entry_point, Register arg_1, Register arg_2);
  void call_VM_leaf(address entry_point, Register arg_1, Register arg_2, Register arg_3);

  // Call a stub function via a function descriptor
  inline address call_stub(Register function_entry);
  inline void call_stub_and_return_to(Register function_entry, Register return_pc);

  //
  // Java utilities
  //

  // Read from the polling page, its address is already in a register.
  inline void load_from_polling_page(Register polling_page_address, int offset = 0);
  // Check whether instruction is a read access to the polling page
  // which was emitted by load_from_polling_page(..).
  static bool is_load_from_polling_page(int instruction, void* ucontext/*may be NULL*/,
                                        address* polling_address_ptr = NULL);

  // Check whether instruction is a write access to the memory
  // serialization page realized by one of the instructions stw, stwu,
  // stwx, or stwux.
  static bool is_memory_serialization(int instruction, JavaThread* thread, void* ucontext);

  // Support for NULL-checks
  //
  // Generates code that causes a NULL OS exception if the content of reg is NULL.
  // If the accessed location is M[reg + offset] and the offset is known, provide the
  // offset. No explicit code generation is needed if the offset is within a certain
  // range (0 <= offset <= page_size).

  // Stack overflow checking
  void bang_stack_with_offset(int offset);

  // If instruction is a stack bang of the form ld, stdu, or
  // stdux, return the banged address. Otherwise, return 0.
  static address get_stack_bang_address(int instruction, void* ucontext);

  // Cmpxchg semantics
  enum {
    MemBarNone = 0,
    MemBarRel  = 1,
    MemBarAcq  = 2,
    MemBarFenceAfter = 4 // use powers of 2
  };

  // interface method calling
  void lookup_interface_method(Register recv_klass,
                               Register intf_klass,
                               RegisterOrConstant itable_index,
                               Register method_result,
                               Register temp_reg, Register temp2_reg,
                               Label& no_such_interface);

  // virtual method calling
  void lookup_virtual_method(Register recv_klass,
                             RegisterOrConstant vtable_index,
                             Register method_result);

  // Test sub_klass against super_klass, with fast and slow paths.

  // The fast path produces a tri-state answer: yes / no / maybe-slow.
  // One of the three labels can be NULL, meaning take the fall-through.
  // If super_check_offset is -1, the value is loaded up from super_klass.
  // No registers are killed, except temp_reg and temp2_reg.
  // If super_check_offset is not -1, temp2_reg is not used and can be noreg.
  void check_klass_subtype_fast_path(Register sub_klass,
                                     Register super_klass,
                                     Register temp1_reg,
                                     Register temp2_reg,
                                     Label& L_success,
                                     Label& L_failure);

  // The rest of the type check; must be wired to a corresponding fast path.
  // It does not repeat the fast path logic, so don't use it standalone.
  // The temp_reg can be noreg, if no temps are available.
  // It can also be sub_klass or super_klass, meaning it's OK to kill that one.
  // Updates the sub's secondary super cache as necessary.
  void check_klass_subtype_slow_path(Register sub_klass,
                                     Register super_klass,
                                     Register temp1_reg,
                                     Register temp2_reg,
                                     Label* L_success = NULL,
                                     Register result_reg = noreg);

  // Simplified, combined version, good for typical uses.
  // Falls through on failure.
  void check_klass_subtype(Register sub_klass,
                           Register super_klass,
                           Register temp1_reg,
                           Register temp2_reg,
                           Label& L_success);

  // Method handle support (JSR 292).
  void check_method_handle_type(Register mtype_reg, Register mh_reg, Register temp_reg, Label& wrong_method_type);

  RegisterOrConstant argument_offset(RegisterOrConstant arg_slot, Register temp_reg, int extra_slot_offset = 0);

  // Support for serializing memory accesses between threads
  void serialize_memory(Register thread, Register tmp1, Register tmp2);

  // GC barrier support.
  void card_write_barrier_post(Register Rstore_addr, Register Rnew_val, Register Rtmp);
  void card_table_write(jbyte* byte_map_base, Register Rtmp, Register Robj);

#if INCLUDE_ALL_GCS
  // General G1 pre-barrier generator.
  void g1_write_barrier_pre(Register Robj, RegisterOrConstant offset, Register Rpre_val,
                            Register Rtmp1, Register Rtmp2, bool needs_frame = false);
  // General G1 post-barrier generator
  void g1_write_barrier_post(Register Rstore_addr, Register Rnew_val, Register Rtmp1,
                             Register Rtmp2, Register Rtmp3, Label *filtered_ext = NULL);
#endif

  // Support for managing the JavaThread pointer (i.e.; the reference to
  // thread-local information).

  // Support for last Java frame (but use call_VM instead where possible):
  // access R16_thread->last_Java_sp.
  void set_last_Java_frame(Register last_java_sp, Register last_java_fp, Register last_Java_pc);
  void reset_last_Java_frame(void);
  void set_top_ijava_frame_at_SP_as_last_Java_frame(Register sp, Register tmp1);

  // Read vm result from thread: oop_result = R16_thread->result;
  void get_vm_result  (Register oop_result);
  void get_vm_result_2(Register metadata_result);

  static bool needs_explicit_null_check(intptr_t offset);

  // Trap-instruction-based checks.
  // Range checks can be distinguished from zero checks as they check 32 bit,
  // zero checks all 64 bits (tw, td).
  inline void trap_null_check(Register a, trap_to_bits cmp = traptoEqual);
  static bool is_trap_null_check(int x) {
    //return is_tdi(x, traptoEqual,               -1/*any reg*/, 0) ||
    //       is_tdi(x, traptoGreaterThanUnsigned, -1/*any reg*/, 0);
    return false;
  }

  inline void trap_zombie_not_entrant();
  static bool is_trap_zombie_not_entrant(int x) { /*return is_tdi(x, traptoUnconditional, 0*//*reg 0*//*, 1);*/ return false; }

  inline void trap_should_not_reach_here();
  static bool is_trap_should_not_reach_here(int x) { /*return is_tdi(x, traptoUnconditional, 0*//*reg 0*//*, 2);*/ return false; }

  inline void trap_ic_miss_check(Register a, Register b);
  static bool is_trap_ic_miss_check(int x) {
    //return is_td(x, traptoGreaterThanUnsigned | traptoLessThanUnsigned, -1/*any reg*/, -1/*any reg*/);
    return false;
  }

  // Implicit or explicit null check, jumps to static address exception_entry.
  inline void null_check_throw(Register a, int offset, Register temp_reg, address exception_entry);

  // Check accessed object for null. Use SIGTRAP-based null checks on AIX.
  inline void load_with_trap_null_check(Register d, int si16, Register s1);

  // Load heap oop and decompress. Loaded oop may not be null.
  inline void load_heap_oop_not_null(Register d, RegisterOrConstant offs, Register s1 = noreg);
  inline void store_heap_oop_not_null(Register d, RegisterOrConstant offs, Register s1,
                                      /*specify if d must stay uncompressed*/ Register tmp = noreg);

  // Null allowed.
  inline void load_heap_oop(Register d, RegisterOrConstant offs, Register s1 = noreg);

  // Encode/decode heap oop. Oop may not be null, else en/decoding goes wrong.
  inline Register encode_heap_oop_not_null(Register d, Register src = noreg);
  inline void decode_heap_oop_not_null(Register d);

  // Null allowed.
  inline void decode_heap_oop(Register d);

  // Load/Store klass oop from klass field. Compress.
  void load_klass(Register dst, Register src);
  void load_klass_with_trap_null_check(Register dst, Register src);
  void store_klass(Register dst_oop, Register klass, Register tmp = X5_T0);
  void store_klass_gap(Register dst_oop, Register val = noreg); // Will store 0 if val not specified.
  static int instr_size_for_decode_klass_not_null();
  void decode_klass_not_null(Register dst, Register src = noreg);
  void encode_klass_not_null(Register dst, Register src = noreg);

  // Load common heap base into register.
  void reinit_heapbase(Register d);

  // SIGTRAP-based range checks for arrays.
  inline void trap_range_check_l(Register a, Register b);
  inline void trap_range_check_l(Register a, int si16);
  static bool is_trap_range_check_l(int x) {
    //return (is_tw (x, traptoLessThanUnsigned, -1/*any reg*/, -1/*any reg*/) ||
    //        is_twi(x, traptoLessThanUnsigned, -1/*any reg*/)                  );
    return false;
  }
  inline void trap_range_check_le(Register a, int si16);
  static bool is_trap_range_check_le(int x) {
    //return is_twi(x, traptoEqual | traptoLessThanUnsigned, -1/*any reg*/);
    return false;
  }
  inline void trap_range_check_g(Register a, int si16);
  static bool is_trap_range_check_g(int x) {
    //return is_twi(x, traptoGreaterThanUnsigned, -1/*any reg*/);
    return false;
  }
  inline void trap_range_check_ge(Register a, Register b);
  inline void trap_range_check_ge(Register a, int si16);
  static bool is_trap_range_check_ge(int x) {
    //return (is_tw (x, traptoEqual | traptoGreaterThanUnsigned, -1/*any reg*/, -1/*any reg*/) ||
    //        is_twi(x, traptoEqual | traptoGreaterThanUnsigned, -1/*any reg*/)                  );
    return false;
  }
  static bool is_trap_range_check(int x) {
    //return is_trap_range_check_l(x) || is_trap_range_check_le(x) ||
    //       is_trap_range_check_g(x) || is_trap_range_check_ge(x);
    return false;
  }

  void clear_memory_doubleword(Register base_ptr, Register cnt_dwords, Register tmp = X5_T0);

  // Needle of length 1.
  void string_indexof_1(Register result, Register haystack, Register haycnt,
                        Register needle, jchar needleChar,
                        Register tmp1, Register tmp2);
  // General indexof, eventually with constant needle length.
  void string_indexof(Register result, Register haystack, Register haycnt,
                      Register needle, ciTypeArray* needle_values, Register needlecnt, int needlecntval,
                      Register tmp1, Register tmp2, Register tmp3, Register tmp4);
  void string_compare(Register str1_reg, Register str2_reg, Register cnt1_reg, Register cnt2_reg,
                      Register result_reg, Register tmp_reg);
  void char_arrays_equals(Register str1_reg, Register str2_reg, Register cnt_reg, Register result_reg,
                          Register tmp1_reg, Register tmp2_reg, Register tmp3_reg, Register tmp4_reg,
                          Register tmp5_reg);
  void char_arrays_equalsImm(Register str1_reg, Register str2_reg, int cntval, Register result_reg,
                             Register tmp1_reg, Register tmp2_reg);

  //
  // Debugging
  //

  // assert on cr0
  void asm_assert(Register v1, Register v2, bool check_equal, const char* msg, int id);
  void asm_assert_eq(Register v1, Register v2, const char* msg, int id) { asm_assert(v1, v2, true, msg, id); }
  void asm_assert_ne(Register v1, Register v2, const char* msg, int id) { asm_assert(v1, v2, false, msg, id); }

 private:
  void asm_assert_mems_zero(bool check_equal, int size, Register tmp, int mem_offset, Register mem_base,
                            const char* msg, int id);

 public:

  void asm_assert_mem8_is_zero(Register tmp, int mem_offset, Register mem_base, const char* msg, int id) {
    asm_assert_mems_zero(true,  8, tmp, mem_offset, mem_base, msg, id);
  }
  void asm_assert_mem8_isnot_zero(Register tmp, int mem_offset, Register mem_base, const char* msg, int id) {
    asm_assert_mems_zero(false, 8, tmp, mem_offset, mem_base, msg, id);
  }

  // Verify R16_thread contents.
  void verify_thread();

  // Emit code to verify that reg contains a valid oop if +VerifyOops is set.
  void verify_oop(Register reg, const char* s = "broken oop");

  // TODO: verify method and klass metadata (compare against vptr?)
  void _verify_method_ptr(Register reg, const char * msg, const char * file, int line) {}
  void _verify_klass_ptr(Register reg, const char * msg, const char * file, int line) {}

  // Convenience method returning function entry. For the ELFv1 case
  // creates function descriptor at the current address and returs
  // the pointer to it. For the ELFv2 case returns the current address.
  inline address function_entry();

#define verify_method_ptr(reg) _verify_method_ptr(reg, "broken method " #reg, __FILE__, __LINE__)
#define verify_klass_ptr(reg) _verify_klass_ptr(reg, "broken klass " #reg, __FILE__, __LINE__)

 private:

  enum {
    stop_stop                = 0,
    stop_untested            = 1,
    stop_unimplemented       = 2,
    stop_shouldnotreachhere  = 3,
    stop_end                 = 4
  };
  void stop(int type, const char* msg, int id);

 public:
  // Prints msg, dumps registers and stops execution.
  void stop         (const char* msg = "", int id = 0) { stop(stop_stop,               msg, id); }
  void untested     (const char* msg = "", int id = 0) { stop(stop_untested,           msg, id); }
  void unimplemented(const char* msg = "", int id = 0) { stop(stop_unimplemented,      msg, id); }
  void should_not_reach_here()                         { stop(stop_shouldnotreachhere,  "", -1); }

  void zap_from_to(Register low, int before, Register high, int after, Register val, Register addr) PRODUCT_RETURN;
};

#endif // CPU_RISCV_VM_MACROASSEMBLER_RISCV_HPP
