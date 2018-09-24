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

#ifndef CPU_RISCV_VM_NATIVEINST_RISCV_HPP
#define CPU_RISCV_VM_NATIVEINST_RISCV_HPP

#include "asm/assembler.hpp"
#include "asm/macroAssembler.hpp"
#include "memory/allocation.hpp"
#include "runtime/icache.hpp"
#include "runtime/os.hpp"
#include "utilities/top.hpp"

// We have interfaces for the following instructions:
//
// - NativeInstruction
//   - NativeCall
//   - NativeFarCall
//   - NativeMovConstReg
//   - NativeJump
//   - NativeIllegalInstruction

// The base class for different kinds of native instruction abstractions.
// It provides the primitive operations to manipulate code relative to this.
class NativeInstruction VALUE_OBJ_CLASS_SPEC {
  friend class Relocation;

 public:
  bool is_sigtrap_ic_miss_check() {
    assert(UseSIGTRAP, "precondition");
    return MacroAssembler::is_trap_ic_miss_check(long_at(0));
  }

  bool is_sigtrap_null_check() {
    assert(UseSIGTRAP && TrapBasedNullChecks, "precondition");
    return MacroAssembler::is_trap_null_check(long_at(0));
  }

  // We use a special trap for marking a method as not_entrant or zombie
  // iff UseSIGTRAP.
  bool is_sigtrap_zombie_not_entrant() {
    assert(UseSIGTRAP, "precondition");
    return MacroAssembler::is_trap_zombie_not_entrant(long_at(0));
  }

  // We use an illtrap for marking a method as not_entrant or zombie
  // iff !UseSIGTRAP.
  bool is_sigill_zombie_not_entrant() {
    assert(!UseSIGTRAP, "precondition");
    // Work around a C++ compiler bug which changes 'this'.
    return NativeInstruction::is_sigill_zombie_not_entrant_at(addr_at(0));
  }
  static bool is_sigill_zombie_not_entrant_at(address addr);

#ifdef COMPILER2
  // SIGTRAP-based implicit range checks
  bool is_sigtrap_range_check() {
    assert(UseSIGTRAP && TrapBasedRangeChecks, "precondition");
    return MacroAssembler::is_trap_range_check(long_at(0));
  }
#endif

  // 'should not reach here'.
  bool is_sigtrap_should_not_reach_here() {
    return MacroAssembler::is_trap_should_not_reach_here(long_at(0));
  }

  bool is_safepoint_poll() {
    // Is the current instruction a POTENTIAL read access to the polling page?
    // The current arguments of the instruction are not checked!
    return MacroAssembler::is_load_from_polling_page(long_at(0), NULL);
  }

  bool is_memory_serialization(JavaThread *thread, void *ucontext) {
    // Is the current instruction a write access of thread to the
    // memory serialization page?
    return MacroAssembler::is_memory_serialization(long_at(0), thread, ucontext);
  }

  address get_stack_bang_address(void *ucontext) {
    // If long_at(0) is not a stack bang, return 0. Otherwise, return
    // banged address.
    return MacroAssembler::get_stack_bang_address(long_at(0), ucontext);
  }

#ifdef ASSERT
  bool is_debug_trace_trap(int *opcode) {
    if (MacroAssembler::is_lb(long_at(0)) && 
        MacroAssembler::get_rs1(long_at(0)) == 0 &&
        MacroAssembler::get_rd(long_at(0)) == 0) {
      *opcode = MacroAssembler::get_imm12(long_at(0));
      return true;
    }
    return false;
  }

  bool is_load_from(int *alignment, int *rs1, int *offset) {
    if (MacroAssembler::is_ld(long_at(0))) {
      *alignment = 8; 
      *rs1 = MacroAssembler::get_rs1(long_at(0));
      *offset = MacroAssembler::get_imm12(long_at(0));
    }
    if (MacroAssembler::is_lw(long_at(0)) ||
        MacroAssembler::is_lwu(long_at(0))) {
      *alignment = 4; 
      *rs1 = MacroAssembler::get_rs1(long_at(0));
      *offset = MacroAssembler::get_imm12(long_at(0));
    }
    if (MacroAssembler::is_lh(long_at(0)) ||
        MacroAssembler::is_lhu(long_at(0))) {
      *alignment = 2; 
      *rs1 = MacroAssembler::get_rs1(long_at(0));
      *offset = MacroAssembler::get_imm12(long_at(0));
    }
    if (MacroAssembler::is_lb(long_at(0)) ||
        MacroAssembler::is_lbu(long_at(0))) {
      *alignment = 1; 
      *rs1 = MacroAssembler::get_rs1(long_at(0));
      *offset = MacroAssembler::get_imm12(long_at(0));
      return true;
    }
    return false;
  }

  void print() {
    fprintf(stderr, "Executing: %08X ", addr_at(0));
    MacroAssembler::print_instruction(long_at(0));
    fprintf(stderr, "\n");
  }
#endif

 protected:
  address  addr_at(int offset) const    { return address(this) + offset; }
  int      long_at(int offset) const    { return *(int*)addr_at(offset); }

 public:
  void verify() NOT_DEBUG_RETURN;
  bool is_jump() { Unimplemented(); return false; }
};

inline NativeInstruction* nativeInstruction_at(address address) {
  NativeInstruction* inst = (NativeInstruction*)address;
  inst->verify();
  return inst;
}

// The NativeCall is an abstraction for accessing/manipulating call
// instructions. It is used to manipulate inline caches, primitive &
// dll calls, etc.
//
// Sparc distinguishes `NativeCall' and `NativeFarCall'. On RISCV64,
// at present, we provide a single class `NativeCall' representing the
// sequence `load_const, mtctr, bctrl' or the sequence 'ld_from_toc,
// mtctr, bctrl'.
class NativeCall: public NativeInstruction {
 public:

  enum ppc_specific_constants {
    load_const_instruction_size                 = 28,
    load_const_from_method_toc_instruction_size = 16,
    instruction_size                            = 16 // Used in shared code for calls with reloc_info.
  };

  static bool is_call_at(address a) {
    return Assembler::is_jal(*(int*)(a));
  }

  static bool is_call_before(address return_address) {
    return NativeCall::is_call_at(return_address - 4);
  }

  address instruction_address() const {
    return addr_at(0);
  }

  address next_instruction_address() const {
    // We have only jal.
    assert(MacroAssembler::is_jal(*(int*)instruction_address()), "Should be jal instruction!");
    return addr_at(4);
  }

  address return_address() const {
    return next_instruction_address();
  }

  address destination() const;

  // The parameter assert_lock disables the assertion during code generation.
  void set_destination_mt_safe(address dest, bool assert_lock = true);

  address get_trampoline();

  void verify_alignment() {} // do nothing on ppc
  void verify() NOT_DEBUG_RETURN;
};

inline NativeCall* nativeCall_at(address instr) {
  NativeCall* call = (NativeCall*)instr;
  call->verify();
  return call;
}

inline NativeCall* nativeCall_before(address return_address) {
  NativeCall* call = NULL;
  if (MacroAssembler::is_jal(*(int*)(return_address - 4)))
    call = (NativeCall*)(return_address - 4);
  call->verify();
  return call;
}

// The NativeFarCall is an abstraction for accessing/manipulating native
// call-anywhere instructions.
// Used to call native methods which may be loaded anywhere in the address
// space, possibly out of reach of a call instruction.
class NativeFarCall: public NativeInstruction {
 public:
  // We use MacroAssembler::bl64_patchable() for implementing a
  // call-anywhere instruction.

  // Checks whether instr points at a NativeFarCall instruction.
  static bool is_far_call_at(address instr) {
    return MacroAssembler::is_jump64_patchable_at(instr, true);
  }

  // Does the NativeFarCall implementation use a pc-relative encoding
  // of the call destination?
  // Used when relocating code.
  bool is_pcrelative() {
    assert(MacroAssembler::is_jump64_patchable_at((address)this, true),
           "unexpected call type");
    return false;
  }

  // Returns the NativeFarCall's destination.
  address destination() const {
    assert(MacroAssembler::is_jump64_patchable_at((address)this, true),
           "unexpected call type");
    return MacroAssembler::get_dest_of_jump64_patchable_at((address)this, true);
  }

  // Sets the NativeCall's destination, not necessarily mt-safe.
  // Used when relocating code.
  void set_destination(address dest) {
    // Set new destination (implementation of call may change here).
    assert(MacroAssembler::is_jump64_patchable_at((address)this, true),
           "unexpected call type");
    MacroAssembler::set_dest_of_jump64_patchable_at((address)this, dest, true);
  }

  void verify() NOT_DEBUG_RETURN;
};

// Instantiates a NativeFarCall object starting at the given instruction
// address and returns the NativeFarCall object.
inline NativeFarCall* nativeFarCall_at(address instr) {
  NativeFarCall* call = (NativeFarCall*)instr;
  call->verify();
  return call;
}

// An interface for accessing/manipulating native set_oop imm, reg instructions.
// (used to manipulate inlined data references, etc.)
class NativeMovConstReg: public NativeInstruction {
 public:

  enum ppc_specific_constants {
    load_const_instruction_size                 = 20,
    load_const_from_method_toc_instruction_size =  8,
    instruction_size                            =  8 // Used in shared code for calls with reloc_info.
  };

  address instruction_address() const {
    return addr_at(0);
  }

  address next_instruction_address() const;

  // (The [set_]data accessor respects oop_type relocs also.)
  intptr_t data() const;

  // Patch the code stream.
  address set_data_plain(intptr_t x, CodeBlob *code);
  // Patch the code stream and oop pool.
  void set_data(intptr_t x);

  // Patch narrow oop constants. Use this also for narrow klass.
  void set_narrow_oop(narrowOop data, CodeBlob *code = NULL);

  void verify() NOT_DEBUG_RETURN;
};

inline NativeMovConstReg* nativeMovConstReg_at(address address) {
  NativeMovConstReg* test = (NativeMovConstReg*)address;
  test->verify();
  return test;
}

// The NativeJump is an abstraction for accessing/manipulating native
// jump-anywhere instructions.
class NativeJump: public NativeInstruction {
 public:
  // We use MacroAssembler::b64_patchable() for implementing a
  // jump-anywhere instruction.

  enum ppc_specific_constants {
    instruction_size = MacroAssembler::jump64_patchable_size
  };

  // Checks whether instr points at a NativeJump instruction.
  static bool is_jump_at(address instr) {
    return MacroAssembler::is_jump64_patchable_at(instr, false);
  }

  // Does the NativeJump implementation use a pc-relative encoding
  // of the call destination?
  // Used when relocating code or patching jumps.
  bool is_pcrelative() {
    return false;
  }

  // Returns the NativeJump's destination.
  address jump_destination() const {
    if (MacroAssembler::is_jump64_patchable_at((address)this, false)) {
      return MacroAssembler::get_dest_of_jump64_patchable_at((address)this, false);
    } else {
      ShouldNotReachHere();
      return NULL;
    }
  }

  // Sets the NativeJump's destination, not necessarily mt-safe.
  // Used when relocating code or patching jumps.
  void set_jump_destination(address dest) {
    // Set new destination (implementation of call may change here).
    if (MacroAssembler::is_jump64_patchable_at((address)this, false)) {
      MacroAssembler::set_dest_of_jump64_patchable_at((address)this, dest, false);
    } else {
      ShouldNotReachHere();
    }
  }

  // MT-safe insertion of native jump at verified method entry
  static void patch_verified_entry(address entry, address verified_entry, address dest);

  void verify() NOT_DEBUG_RETURN;

  static void check_verified_entry_alignment(address entry, address verified_entry) {
    // We just patch one instruction on ppc64, so the jump doesn't have to
    // be aligned. Nothing to do here.
  }
};

// Instantiates a NativeJump object starting at the given instruction
// address and returns the NativeJump object.
inline NativeJump* nativeJump_at(address instr) {
  NativeJump* call = (NativeJump*)instr;
  call->verify();
  return call;
}

// An interface for accessing/manipulating native moves of the form:
//       lui   AT, split_high(offset)
//       addiu AT, split_low(offset)
//       add   reg, reg, AT
//       lb/lbu/sb/lh/lhu/sh/lw/sw/lwc1/swc1 dest, reg, 0
//       [lw/sw/lwc1/swc1                    dest, reg, 4]
//     or
//       lb/lbu/sb/lh/lhu/sh/lw/sw/lwc1/swc1 dest, reg, offset
//       [lw/sw/lwc1/swc1                    dest, reg, offset+4]
//
// Warning: These routines must be able to handle any instruction sequences
// that are generated as a result of the load/store byte,word,long
// macros.

class NativeMovRegMem: public NativeInstruction {
 public:
  enum mips_specific_constants {
    instruction_offset  = 0,
    hiword_offset   = 4,
    ldst_offset     = 12,
    immediate_size  = 4,
    ldst_size       = 16
  };

  //offset is less than 16 bits.
  bool is_immediate() const { return !MacroAssembler::is_lui(long_at(instruction_offset)); }
  bool is_64ldst() const {
    return false;
    #if 0
    if (is_immediate()) {
      return (Assembler::opcode(long_at(hiword_offset)) == Assembler::opcode(long_at(instruction_offset))) &&
       (Assembler::imm_off(long_at(hiword_offset)) == Assembler::imm_off(long_at(instruction_offset)) + wordSize);
    } else {
      return (Assembler::opcode(long_at(ldst_offset+hiword_offset)) == Assembler::opcode(long_at(ldst_offset))) &&
       (Assembler::imm_off(long_at(ldst_offset+hiword_offset)) == Assembler::imm_off(long_at(ldst_offset)) + wordSize);
    }
    #endif
  }

  address instruction_address() const       { return addr_at(instruction_offset); }
  address next_instruction_address() const  {
    return addr_at( (is_immediate()? immediate_size : ldst_size) + (is_64ldst()? 4 : 0));
  }

  int   offset() const;

  void  set_offset(int x);

  void  add_offset_in_bytes(int add_offset)     { set_offset ( ( offset() + add_offset ) ); }

  void verify();
  void print ();

  // unit test stuff
  static void test() {}

 private:
  inline friend NativeMovRegMem* nativeMovRegMem_at (address address);
};

inline NativeMovRegMem* nativeMovRegMem_at (address address) {
  NativeMovRegMem* test = (NativeMovRegMem*)(address - NativeMovRegMem::instruction_offset);
#ifdef ASSERT
  test->verify();
#endif
  return test;
}

//64 bits:
//    far jump:
//          lui   rd, imm(63...48);
//          ori   rd, rd, imm(47...32);
//          dsll  rd, rd, 16;
//          ori   rd, rd, imm(31...16);
//          dsll  rd, rd, 16;
//          ori   rd, rd, imm(15...0);
//          jalr  rd
//          nop
//
class NativeGeneralJump: public NativeInstruction {
 public:
  enum mips_specific_constants {
    instruction_offset   =    0,
    beq_opcode           =    0x10000000,//000100|00000|00000|offset
    b_mask         =    0xffff0000,
    short_size      =    8,
#ifndef _LP64
    instruction_size   =    4 * BytesPerInstWord
#else
    instruction_size   =    6 * BytesPerInstWord
#endif
  };

  bool is_short() const { return (long_at(instruction_offset) & b_mask) == beq_opcode; }
#ifdef _LP64
  bool is_b_far() { Unimplemented(); return false; }
#endif
  address instruction_address() const { return addr_at(instruction_offset); }
  address jump_destination() { Unimplemented(); return 0; }

  void  patch_set48_gs(address dest) { Unimplemented(); }
  void  patch_set48(address dest) { Unimplemented(); }

  void  patch_on_jr_gs(address dest) { Unimplemented(); }
  void  patch_on_jr(address dest) { Unimplemented(); }

  void  patch_on_j_gs(address dest) { Unimplemented(); }
  void  patch_on_j(address dest) { Unimplemented(); }

  void  patch_on_j_only(address dest) { Unimplemented(); }

  void  set_jump_destination(address dest) { Unimplemented(); }

  // Creation
  inline friend NativeGeneralJump* nativeGeneralJump_at(address address);

  // Insertion of native general jump instruction
  static void insert_unconditional(address code_pos, address entry) { Unimplemented(); };
  static void replace_mt_safe(address instr_addr, address code_buffer) { Unimplemented(); };
  static void check_verified_entry_alignment(address entry, address verified_entry) { Unimplemented(); }
  static void patch_verified_entry(address entry, address verified_entry, address dest) { Unimplemented(); }

  void verify() { Unimplemented(); }
};

inline NativeGeneralJump* nativeGeneralJump_at(address address) {
  NativeGeneralJump* jump = (NativeGeneralJump*)(address);
  debug_only(jump->verify();)
  return jump;
}

#endif // CPU_RISCV_VM_NATIVEINST_RISCV_HPP
