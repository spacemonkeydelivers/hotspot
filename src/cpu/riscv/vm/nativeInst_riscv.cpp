/*
 * Copyright (c) 1997, 2014, Oracle and/or its affiliates. All rights reserved.
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

#include "precompiled.hpp"
#include "asm/macroAssembler.inline.hpp"
#include "memory/resourceArea.hpp"
#include "nativeInst_riscv.hpp"
#include "oops/oop.inline.hpp"
#include "runtime/handles.hpp"
#include "runtime/sharedRuntime.hpp"
#include "runtime/stubRoutines.hpp"
#include "utilities/ostream.hpp"
#ifdef COMPILER1
#include "c1/c1_Runtime1.hpp"
#endif

// We use an illtrap for marking a method as not_entrant or zombie iff !UseSIGTRAP
// Work around a C++ compiler bug which changes 'this'
bool NativeInstruction::is_sigill_zombie_not_entrant_at(address addr) {
  ShouldNotReachHere();
  /*
  assert(!UseSIGTRAP, "precondition");
  if (*(int*)addr != 0 */ /*illtrap*/ /*) return false;
  CodeBlob* cb = CodeCache::find_blob_unsafe(addr);
  if (cb == NULL || !cb->is_nmethod()) return false;
  nmethod *nm = (nmethod *)cb;
  // This method is not_entrant or zombie iff the illtrap instruction is
  // located at the verified entry point.
  return nm->verified_entry_point() == addr;
  */
}

void NativeMovRegMem::set_offset(int x) {
  Unimplemented();
  #if 0
  if (is_immediate()) {
    assert(Assembler::is_simm16(x), "just check");
    set_int_at(0, (int_at(0)&0xffff0000) | (x&0xffff) );
    if (is_64ldst()) {
      assert(Assembler::is_simm16(x+4), "just check");
      set_int_at(4, (int_at(4)&0xffff0000) | ((x+4)&0xffff) );
    }
  } else {
    set_int_at(0, (int_at(0) & 0xffff0000) | (Assembler::split_high(x) & 0xffff));
    set_int_at(4, (int_at(4) & 0xffff0000) | (Assembler::split_low(x) & 0xffff));
  }
  ICache::invalidate_range(addr_at(0), 8);
  #endif
}

void NativeMovRegMem::verify() {
  Unimplemented();
  #if 0
  int offset = 0;

  if ( Assembler::opcode(int_at(0)) == Assembler::lui_op ) {
#ifndef _LP64
    if ( (Assembler::opcode(int_at(4)) != Assembler::addiu_op) ||
         (Assembler::opcode(int_at(8)) != Assembler::special_op) ||
         (Assembler::special(int_at(8)) != Assembler::add_op))
#else
      /* Jin: fit MIPS64 */
    if ( (Assembler::opcode(int_at(4)) != Assembler::addiu_op &&
          Assembler::opcode(int_at(4)) != Assembler::daddiu_op ) ||
         (Assembler::opcode(int_at(8)) != Assembler::special_op) ||
         (Assembler::special(int_at(8)) != Assembler::add_op
       && Assembler::special(int_at(8)) != Assembler::dadd_op))
#endif
      fatal ("not a mov [reg+offs], reg instruction");
      offset += 12;
  }

  switch(Assembler::opcode(int_at(offset))) {
    case Assembler::lb_op:
    case Assembler::lbu_op:
    case Assembler::lh_op:
    case Assembler::lhu_op:
    case Assembler::lw_op:
    case Assembler::lwu_op:
    LP64_ONLY(case Assembler::ld_op:)
    case Assembler::lwc1_op:
    LP64_ONLY(case Assembler::ldc1_op:)
    case Assembler::sb_op:
    case Assembler::sh_op:
    case Assembler::sw_op:
    LP64_ONLY(case Assembler::sd_op:)
    case Assembler::swc1_op:
    LP64_ONLY(case Assembler::sdc1_op:)
      break;
    default:
      fatal ("not a mov [reg+offs], reg instruction");
  }
  #endif
}

#ifdef ASSERT
void NativeInstruction::verify() {
  // Make sure code pattern is actually an instruction address.
  address addr = addr_at(0);
  if (addr == 0 || ((intptr_t)addr & 3) != 0) {
    fatal("not an instruction address");
  }
}
#endif // ASSERT

// Extract call destination from a NativeCall. The call might use a trampoline stub.
address NativeCall::destination() const {
  ShouldNotReachHere();
  /*
  address addr = (address)this;
  address destination = Assembler::bxx_destination(addr);

  // Do we use a trampoline stub for this call?
  CodeBlob* cb = CodeCache::find_blob_unsafe(addr);   // Else we get assertion if nmethod is zombie.
  assert(cb && cb->is_nmethod(), "sanity");
  nmethod *nm = (nmethod *)cb;
  if (nm->stub_contains(destination) && is_NativeCallTrampolineStub_at(destination)) {
    // Yes we do, so get the destination from the trampoline stub.
    const address trampoline_stub_addr = destination;
    destination = NativeCallTrampolineStub_at(trampoline_stub_addr)->destination(nm);
  }

  return destination;
  */
}

// Similar to replace_mt_safe, but just changes the destination. The
// important thing is that free-running threads are able to execute this
// call instruction at all times. Thus, the displacement field must be
// instruction-word-aligned.
//
// Used in the runtime linkage of calls; see class CompiledIC.
//
// Add parameter assert_lock to switch off assertion
// during code generation, where no patching lock is needed.
void NativeCall::set_destination_mt_safe(address dest, bool assert_lock) {
  ShouldNotReachHere();
  /*
  assert(!assert_lock ||
         (Patching_lock->is_locked() || SafepointSynchronize::is_at_safepoint()),
         "concurrent code patching");

  ResourceMark rm;
  int code_size = 1 * BytesPerInstWord;
  address addr_call = addr_at(0);
  assert(MacroAssembler::is_bl(*(int*)addr_call), "unexpected code at call-site");

  CodeBuffer cb(addr_call, code_size + 1);
  MacroAssembler* a = new MacroAssembler(&cb);

  // Patch the call.
  if (!ReoptimizeCallSequences || !a->is_within_range_of_b(dest, addr_call)) {
    address trampoline_stub_addr = get_trampoline();

    // We did not find a trampoline stub because the current codeblob
    // does not provide this information. The branch will be patched
    // later during a final fixup, when all necessary information is
    // available.
    if (trampoline_stub_addr == 0)
      return;

    // Patch the constant in the call's trampoline stub.
    NativeCallTrampolineStub_at(trampoline_stub_addr)->set_destination(dest);
    dest = trampoline_stub_addr;
  }

  OrderAccess::release();
  a->bl(dest);

  ICache::ppc64_flush_icache_bytes(addr_call, code_size);
  */
}

address NativeCall::get_trampoline() {
  ShouldNotReachHere();
  /*
  address call_addr = addr_at(0);

  CodeBlob *code = CodeCache::find_blob(call_addr);
  assert(code != NULL, "Could not find the containing code blob");

  // There are no relocations available when the code gets relocated
  // because of CodeBuffer expansion.
  if (code->relocation_size() == 0)
    return NULL;

  address bl_destination = Assembler::bxx_destination(call_addr);
  if (code->content_contains(bl_destination) &&
      is_NativeCallTrampolineStub_at(bl_destination))
    return bl_destination;

  // If the codeBlob is not a nmethod, this is because we get here from the
  // CodeBlob constructor, which is called within the nmethod constructor.
  return trampoline_stub_Relocation::get_trampoline_for(call_addr, (nmethod*)code);
  */
}

#ifdef ASSERT
void NativeCall::verify() {
  ShouldNotReachHere();
  /*
  address addr = addr_at(0);

  if (!NativeCall::is_call_at(addr)) {
    tty->print_cr("not a NativeCall at " PTR_FORMAT, p2i(addr));
    // TODO: RISCV port: Disassembler::decode(addr - 20, addr + 20, tty);
    fatal(err_msg("not a NativeCall at " PTR_FORMAT, p2i(addr)));
  }
  */
}
#endif // ASSERT

#ifdef ASSERT
void NativeFarCall::verify() {
  ShouldNotReachHere();
  /*
  address addr = addr_at(0);

  NativeInstruction::verify();
  if (!NativeFarCall::is_far_call_at(addr)) {
    tty->print_cr("not a NativeFarCall at " PTR_FORMAT, p2i(addr));
    // TODO: RISCV port: Disassembler::decode(addr, 20, 20, tty);
    fatal(err_msg("not a NativeFarCall at " PTR_FORMAT, p2i(addr)));
  }
  */
}
#endif // ASSERT

address NativeMovConstReg::next_instruction_address() const {
  ShouldNotReachHere();
  /*
#ifdef ASSERT
  CodeBlob* nm = CodeCache::find_blob(instruction_address());
  assert(!MacroAssembler::is_set_narrow_oop(addr_at(0), nm->content_begin()), "Should not patch narrow oop here");
#endif

  if (MacroAssembler::is_load_const_from_method_toc_at(addr_at(0))) {
    return addr_at(load_const_from_method_toc_instruction_size);
  } else {
    return addr_at(load_const_instruction_size);
  }
  */
}

intptr_t NativeMovConstReg::data() const {
  ShouldNotReachHere();
  /*
  address   addr = addr_at(0);

  if (MacroAssembler::is_load_const_at(addr)) {
    return MacroAssembler::get_const(addr);
  }

  CodeBlob* cb = CodeCache::find_blob_unsafe(addr);
  if (MacroAssembler::is_set_narrow_oop(addr, cb->content_begin())) {
    narrowOop no = (narrowOop)MacroAssembler::get_narrow_oop(addr, cb->content_begin());
    return cast_from_oop<intptr_t>(oopDesc::decode_heap_oop(no));
  } else {
    assert(MacroAssembler::is_load_const_from_method_toc_at(addr), "must be load_const_from_pool");

    address ctable = cb->content_begin();
    int offset = MacroAssembler::get_offset_of_load_const_from_method_toc_at(addr);
    return *(intptr_t *)(ctable + offset);
  }
  */
}

address NativeMovConstReg::set_data_plain(intptr_t data, CodeBlob *cb) {
  ShouldNotReachHere();
  /*
  address addr         = instruction_address();
  address next_address = NULL;
  if (!cb) cb = CodeCache::find_blob(addr);

  if (cb != NULL && MacroAssembler::is_load_const_from_method_toc_at(addr)) {
    // A load from the method's TOC (ctable).
    assert(cb->is_nmethod(), "must be nmethod");
    const address ctable = cb->content_begin();
    const int toc_offset = MacroAssembler::get_offset_of_load_const_from_method_toc_at(addr);
    *(intptr_t *)(ctable + toc_offset) = data;
    next_address = addr + BytesPerInstWord;
  } else if (cb != NULL &&
             MacroAssembler::is_calculate_address_from_global_toc_at(addr, cb->content_begin())) {
    // A calculation relative to the global TOC.
    if (MacroAssembler::get_address_of_calculate_address_from_global_toc_at(addr, cb->content_begin()) !=
        (address)data) {
      const int invalidated_range =
        MacroAssembler::patch_calculate_address_from_global_toc_at(addr, cb->content_begin(),
                                                                   (address)data);
      const address start = invalidated_range < 0 ? addr + invalidated_range : addr;
      // FIXME:
      const int range = invalidated_range < 0 ? 4 - invalidated_range : 8;
      ICache::ppc64_flush_icache_bytes(start, range);
    }
    next_address = addr + 1 * BytesPerInstWord;
  } else if (MacroAssembler::is_load_const_at(addr)) {
    // A normal 5 instruction load_const code sequence.
    if (MacroAssembler::get_const(addr) != (long)data) {
      // This is not mt safe, ok in methods like CodeBuffer::copy_code().
      MacroAssembler::patch_const(addr, (long)data);
      ICache::ppc64_flush_icache_bytes(addr, load_const_instruction_size);
    }
    next_address = addr + 5 * BytesPerInstWord;
  } else if (MacroAssembler::is_bl(* (int*) addr)) {
    // A single branch-and-link instruction.
    ResourceMark rm;
    const int code_size = 1 * BytesPerInstWord;
    CodeBuffer cb(addr, code_size + 1);
    MacroAssembler* a = new MacroAssembler(&cb);
    a->bl((address) data);
    ICache::ppc64_flush_icache_bytes(addr, code_size);
    next_address = addr + code_size;
  } else {
    ShouldNotReachHere();
  }

  return next_address;
  */
}

void NativeMovConstReg::set_data(intptr_t data) {
  ShouldNotReachHere();
  /*
  // Store the value into the instruction stream.
  CodeBlob *cb = CodeCache::find_blob(instruction_address());
  address next_address = set_data_plain(data, cb);

  // Also store the value into an oop_Relocation cell, if any.
  if (cb && cb->is_nmethod()) {
    RelocIterator iter((nmethod *) cb, instruction_address(), next_address);
    oop* oop_addr = NULL;
    Metadata** metadata_addr = NULL;
    while (iter.next()) {
      if (iter.type() == relocInfo::oop_type) {
        oop_Relocation *r = iter.oop_reloc();
        if (oop_addr == NULL) {
          oop_addr = r->oop_addr();
          *oop_addr = cast_to_oop(data);
        } else {
          assert(oop_addr == r->oop_addr(), "must be only one set-oop here") ;
        }
      }
      if (iter.type() == relocInfo::metadata_type) {
        metadata_Relocation *r = iter.metadata_reloc();
        if (metadata_addr == NULL) {
          metadata_addr = r->metadata_addr();
          *metadata_addr = (Metadata*)data;
        } else {
          assert(metadata_addr == r->metadata_addr(), "must be only one set-metadata here");
        }
      }
    }
  }
  */
}

void NativeMovConstReg::set_narrow_oop(narrowOop data, CodeBlob *code /* = NULL */) {
  ShouldNotReachHere();
  /*
  address   addr = addr_at(0);
  CodeBlob* cb = (code) ? code : CodeCache::find_blob(instruction_address());
  if (MacroAssembler::get_narrow_oop(addr, cb->content_begin()) == (long)data) return;
  const int invalidated_range =
    MacroAssembler::patch_set_narrow_oop(addr, cb->content_begin(), (long)data);
  const address start = invalidated_range < 0 ? addr + invalidated_range : addr;
  // FIXME:
  const int range = invalidated_range < 0 ? 4 - invalidated_range : 8;
  ICache::ppc64_flush_icache_bytes(start, range);
  */
}

// Do not use an assertion here. Let clients decide whether they only
// want this when assertions are enabled.
#ifdef ASSERT
void NativeMovConstReg::verify() {
  ShouldNotReachHere();
  /*
  address   addr = addr_at(0);
  if (! MacroAssembler::is_load_const_at(addr) &&
      ! MacroAssembler::is_load_const_from_method_toc_at(addr)) {
    CodeBlob* cb = CodeCache::find_blob_unsafe(addr);   // find_nmethod() asserts if nmethod is zombie.
    if (! (cb != NULL && MacroAssembler::is_calculate_address_from_global_toc_at(addr, cb->content_begin())) &&
        ! (cb != NULL && MacroAssembler::is_set_narrow_oop(addr, cb->content_begin())) &&
        ! MacroAssembler::is_bl(*((int*) addr))) {
      tty->print_cr("not a NativeMovConstReg at " PTR_FORMAT, p2i(addr));
      // TODO: RISCV port: Disassembler::decode(addr, 20, 20, tty);
      fatal(err_msg("not a NativeMovConstReg at " PTR_FORMAT, p2i(addr)));
    }
  }
  */
}
#endif // ASSERT

void NativeJump::patch_verified_entry(address entry, address verified_entry, address dest) {
  ShouldNotReachHere();
  /*
  ResourceMark rm;
  int code_size = 1 * BytesPerInstWord;
  CodeBuffer cb(verified_entry, code_size + 1);
  MacroAssembler* a = new MacroAssembler(&cb);
#ifdef COMPILER2
  assert(dest == SharedRuntime::get_handle_wrong_method_stub(), "expected fixed destination of patch");
#endif
  // Patch this nmethod atomically. Always use illtrap/trap in debug build.
  if (DEBUG_ONLY(false &&) a->is_within_range_of_b(dest, a->pc())) {
    a->b(dest);
  } else {
    // The signal handler will continue at dest=OptoRuntime::handle_wrong_method_stub().
    if (TrapBasedNotEntrantChecks) {
      // We use a special trap for marking a method as not_entrant or zombie.
      a->trap_zombie_not_entrant();
    } else {
      // We use an illtrap for marking a method as not_entrant or zombie.
      a->illtrap();
    }
  }
  ICache::ppc64_flush_icache_bytes(verified_entry, code_size);
  */
}

#ifdef ASSERT
void NativeJump::verify() {
  address addr = addr_at(0);

  NativeInstruction::verify();
  if (!NativeJump::is_jump_at(addr)) {
    tty->print_cr("not a NativeJump at " PTR_FORMAT, p2i(addr));
    // TODO: RISCV port: Disassembler::decode(addr, 20, 20, tty);
    fatal(err_msg("not a NativeJump at " PTR_FORMAT, p2i(addr)));
  }
}
#endif // ASSERT

