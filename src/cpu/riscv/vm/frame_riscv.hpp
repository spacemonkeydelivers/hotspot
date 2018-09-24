/*
 * Copyright (c) 2000, 2013, Oracle and/or its affiliates. All rights reserved.
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

#ifndef CPU_RISCV_VM_FRAME_RISCV_HPP
#define CPU_RISCV_VM_FRAME_RISCV_HPP

#include "runtime/synchronizer.hpp"
#include "utilities/top.hpp"

//
//----------------------------------------------------------------------------
//
// Layout of interpreter frame in memory:
//
//    The bottom of the caller's frame is on the left, the new callee frame on 
//    the right, showing how the 'locals' area in callee is set up to overlap 
//    the parameters set up in the caller.  The parameters are on top of the
//    caller's Java expression stack, and the callee (who knows the expected
//    argument count) can initially locate them by the caller's Resp 
//    (expression stack pointer) value.
//
//
//         JUST BEFORE CALL
//    +------------------------+
//    | ...                    |
//    +------------------------+
//    | Java Stack slot 0      |
//    | Java Stack slot 1      |
//    | ...                    |
//    | Java Stack slot (n)    |           AFTER INITIALIZATION
//    +------------------------+ ------ +------------------------+
//    |                        |        |                        | <- Rlocals
//    |  Outgoing parameters   |        | Incoming parameters    |
//    |                        |        |                        | 
//    +------------------------+ ------ +------------------------+
//    |   Unused Java Stack    |<- Resp |                        |
//    |    Slots . . .         |        | Locals                 |
//    |                        |<- SP   |                        | <- saved_SP
//    +------------------------+        |                        | <- FP
//                                      +------------------------+
//                                      | Return Address         | 
//                                      | Caller's FP            |
//                                      +------------------------+
//                                      | saved_SP               |
//                                      | iState Registers (7)   |
//                                      | Oop temp + align (2)   |
//                                      +------------------------+
//                                      | Java Stack slot 0      | <- Resp,
//                                      | Java Stack slot 1      |    Rmonitors
//                                      |    . . .               |
//                                      | Java Stack slot n      | <- SP
//                                      +------------------------+
//
//             After some operands are pushed onto the Java stack, and some
//             monitors are created, the regions below FP will look like this:
//
//                                         AFTER SOME EXECUTION
//                                      +------------------------+
//                                      | Return Address         | 
//                                      | Caller's FP            |
//                                      +------------------------+
//                                      | saved_SP               |
//       X19 = Resp                     | iState Registers (7)   |
//       X18 = Rbcp                     | Oop temp + align (2)   |
//       X23 = Rmethod                  +------------------------+
//       X22 = Rlocals                  | Monitor block 0        |
//       X26 = Rmonitors                |    . . .               |
//       X27 = RcpoolCache              | Monitor block n        |
//       X9  = Rmdx                     +------------------------+
//                                      | Java Stack slot 0      | <- Rmonitors
//                                      | Java Stack slot 1      |
//                                      |    . . .               | <- Resp
//                                      | Java Stack slot n      | <- SP
//                                      +------------------------+
//
// The Interpreter State Registers are used as follows:
//    Resp         first free element of expression stack
//                 (which grows towards __lower__ addresses)
//    Rbcp         is set to address of bytecode to execute
//                 It may at times (during GC) be an index instead.
//    Rmethod      the method being interpreted
//    Rlocals      the base pointer for accessing the locals array
//                 (lower-numbered locals have higher addresses)
//    Rmonitors    the base pointer for accessing active monitors
//    RcpoolCache  a saved pointer to the method's constant pool cache
//    Rmdx         a pointer to the profiling data for this method
//    saved_SP     the initial SP for the caller, before adjustment by 
//                 adapters or for callee's locals
//
//  Allocated adjacent to the ISR slots is an 'oop result temp slot', with
//  alignment filler.  This is mainly used in the native call stub frame, to 
//  hold the result of the call if it is an object pointer. This allows the 
//  object pointer to be found updated if a GC occurs at the safepoint on 
//  return from the native call.  In interpreter frames, it is also sometimes
//  used as a 'spill' slot for preserving an object pointer in a temp reg 
//  across a call out of the interpreter (via call_VM).
//
//  The architected SP (R54), LR (R55), and FP (R52) are used.
//  The architected argument registers (R0-R9), which are caller-save, are 
//  unused in the interpreter and thus never saved.  Compiled code may need to
//  save them around calls, if they are relied on for local values.
//  
//  Registers used but NOT stored in frame:
//
//    Global Registers (callee-save, but not saved in interpreter or compiled 
//                      code, only on calls in from native code)
//      X20 = Gthread
//      R31 = Gheap_base (Base of Java heap if UseCompressedOops is enabled.
//                        Otherwise, contains polling page address, so we can
//                        avoid constantly reloading it.)
//      R32 = GdispatchTables
//
//    Output Registers
//      X10/X11 are used as return value registers by interpreted, compiled, 
//      and native code.
//
//
//----------------------------------------------------------------------------


 public:

  // Frame layout for all frames:

  // FP-based offsets for frame
  //   Don't use SP to avoid keeping track of frame size
  enum frame_contents {
    frame_args_index  = 0,
    frame_args_offset = 0,
    frame_ra_index    = -1,
    frame_ra_offset   = frame_ra_index * wordSize,
    frame_fp_index    = -2,
    frame_fp_offset   = frame_fp_index * wordSize,
    frame_min_size    = 3,
    frame_min_bytes   = frame_min_size * wordSize,
    frame_elem_size   = wordSize
  };
  
  // Unimplemented
  enum {
    jit_out_preserve_size = 0,
    jit_in_preserve_size  = 0
  };

  enum {
    // Frame alignment on stack
    alignment_in_bytes = 16,
    // log_2(16*8 bits) = 7.
    log_2_of_alignment_in_bits = 7,
    pc_return_offset = 0,
    memory_parameter_word_sp_offset = 0
  };

 private:

  // frame pointer for this frame
  intptr_t* _fp;

  // The frame's stack pointer before it has been extended by a c2i adapter;
  // needed by deoptimization
  intptr_t* _unextended_sp;

 public:

  enum spill_nonvolatiles {
    spill_nonvolatiles_size = 24 * wordSize,
  };

  enum interpreter_frame_vm_locals {
    interpreter_frame_saved_SP_index       = -3, 
    interpreter_frame_Rbcp_index           = -4,
    interpreter_frame_Rmethod_index        = -5,
    interpreter_frame_Rlocals_index        = -6,
    interpreter_frame_Rmonitors_index      = -7,
    interpreter_frame_RcpoolCache_index    = -8,
    interpreter_frame_Rmdx_index           = -9,
    interpreter_frame_Resp_index           = -10,
    interpreter_frame_oop_temp_offset      = -11, // shared code uses this name
    interpreter_frame_result_temp          = -12,
    // interpreter_frame_result_temp is used by InterpreterMacroAssembler's
    // JVMTI support code
    interpreter_frame_local_words          =  12, // includes frame_min_size
    interpreter_frame_local_words_base_offset = -interpreter_frame_local_words
                                                * wordSize,

    // native stub frame needs 2 extra words in outgoing param area
    // for class and jnienv arguments
    native_stub_extra_outgoing_argument_words = 2,

    // The outermost interpreter frame created by deoptimization has extra
    // slots used for restoring caller's callee-saved registers which may 
    // have been used by C2. This number must be at least as large as the
    // number of callee-saved regs specified as allocatable in tile.ad + 1.
    // One additional slot is used as a mask word to indicate which register
    // values are live.  The indices begin at the word after saved FP, hence
    // start at 2.

    //TODO: Figure out where in deopt code these are used...
    deopt_reg_mask_index = 2,
    deopt_R45_index      = 3,
    deopt_R46_index      = 4,
    deopt_R47_index      = 5,
    deopt_R48_index      = 6,
    deopt_R49_index      = 7,
    deopt_R50_index      = 8,
    deopt_R51_index      = 9,
    deopt_callee_register_slots = 8
  };

  // Compiled code frames have only a tiny fixed part, corresponding to the
  // Rsaved_SP slot in interpreter frames. This is required so that returns
  // from native method calls, or OnStackReplacement transitions, can undo 
  // the stack extension which may have been added by a C2I adapter This slot
  // is only filled in by C2I, though it is allocated in all compiled frames. 
  // The same slot is needed in entry frames for OSR transitions; the slot
  // must be available because the callee doesn't know if the caller is an
  // interpreter, compiled, or entry frame.
  enum compiler_frame_vm_locals {
    compiler_frame_saved_SP_index = interpreter_frame_saved_SP_index,
    compiler_frame_words = 1
  };

  enum entry_frame_vm_locals {
    entry_frame_std_saved_SP_index      = interpreter_frame_saved_SP_index,
    entry_frame_call_wrapper_addr_index = -4,
    entry_frame_result_ptr_index        = -5,
    entry_frame_result_type_index       = -6,
    entry_frame_args_tos_addr_index     = -7, 
    entry_frame_local_size              = 8, // Needs to be even since stack is 16-byte aligned
    entry_frame_local_bytes             = entry_frame_local_size * wordSize,
    entry_frame_local_words_base_offset = -entry_frame_local_bytes
  };

 public:

  // Accessors for fields
  intptr_t* fp() const { return _fp; }

 private:

  // Find codeblob and set deopt_state.
  inline void find_codeblob_and_deopt_state();

 public:

  // Constructors
  inline frame(intptr_t* sp, intptr_t* fp, address pc);
  inline frame(intptr_t* sp, intptr_t* unextended_sp, intptr_t* fp, address pc);
  inline frame(intptr_t* sp, intptr_t* fp);
  
  inline address* sender_pc_addr() const;

  // expression stack tos if we are nested in a java call
  intptr_t* interpreter_frame_saved_sp() const;

  // deoptimization support
  void interpreter_frame_set_saved_sp(intptr_t* sp);

  inline void           interpreter_frame_set_tos_address(intptr_t* x);

  // return address of param, zero origin index.
  inline address* native_param_addr(int idx) const;

  intptr_t*  sp_addr_at(int index) const { return &sp()[index]; }
  intptr_t   sp_at(     int index) const { return *sp_addr_at(index); }

  inline address* saved_pc_addr() const;
  inline address saved_pc() const;

 private:
  // THESE ARE FROM THE SPARC VERSION

  ConstantPoolCache**   interpreter_frame_cpoolcache_addr() const;
  BasicObjectLock**     interpreter_frame_monitors_addr() const;
  intptr_t**            interpreter_frame_esp_addr() const;

  // monitors:

  BasicObjectLock* interpreter_frame_monitors()           
    const { return *interpreter_frame_monitors_addr(); }
  void interpreter_frame_set_monitors(BasicObjectLock* monitors) 
    { *interpreter_frame_monitors_addr() = monitors; }

// Interpreter Frame Creation Summary:
//
//     Input Registers:
//
//        FP (R52) - caller's frame pointer
//        SP (R54) - implicit
//        RA (R55) - return address
//        Resp (R33) - top of caller's expression stack
//        Tmethod (R10) - callee method pointer
//
//     Actions:
//        - Adjust caller's SP to make room for Locals in Caller's frame, if 
//          necessary, so they are adjacent to params.  There may be no 
//          adjustment if the size needed for locals is less than the amount 
//          of max expression stack space in the caller. SP must remain 
//          16-byte aligned.
//        - If stack was adjusted, move caller-stored FP up to new slot
//        - Store lr (R55, return address) in the architected slot
//        - set FP with "move R52, SP", then adjust SP for new frame using SUB,
//          as dictated by native ABI
//        - Store Interpreter state registers (R33-R40) in frame, but modify 
//          the stored Resp so that on return, the parameters on the caller's 
//          stack are effectively 'popped'.  Note that this set includes 
//          'saved_SP' for the caller, which is the SP value prior to any 
//          adjustment here.
//        - Set ISP registers for the current frame/method state:
//               o  Rmethod = Tmethod (R10) input
//               o  Rlocals = base of Params, computed from caller's Resp
//               o  Resp = current expression stack pointer, initially = base.
//                  Resp always points to the next available slot, not the last
//                  element pushed.
//               o  Rbcp = byte code pointer for the callee method
//               o  Rmonitors = the top of the monitor region, initially = 
//                  Resp. (monitors are added to the stack as needed, and the 
//                  entire expression stack is moved in memory to make room. 
//                  Initially, there are none. Since it points to the top of 
//                  the monitor region, it also points to the base of the
//                  expression stack.) Rmonitors is always doubleword aligned.
//                  The monitors are traversed by walking down from a fixed FP
//                  offset to Rmonitors.
//               o  RcpoolCache = constant pool cache pointer
//               o  Rmdx = pointer to profiling data for callee 
//                  method
//               o  Rsaved_SP = initially zero. This is used at calls from 
//                  interpreted code to remember the original SP, at the call
//                  site prior to any extension by either an I2C adapter (which
//                  may modify it to marshall the stack arguments into their 
//                  necessary slots), OR by an interpreted callee which may 
//                  modify it to allocate its 'locals' adjacent to the 
//                  parameters.  In either case, this allows SP to be restored
//                  correctly.  (NOTE: The sparc version uses a "last_SP" for 
//                  the adapter case, and uses the separate "saved_SP" for the
//                  general case.  The x86 version has last_SP and 'sender_sp'.
//                  We may need to split this into two, and thus incur another
//                  register save, but I don't yet see why both are required.
//
//----------------------------------------------------------------------------
//
// Layout of Java compiled frame in memory:
//
//    Compiled Java frames do not have a frame analogous to the interpreted 
//    frame.  The standard defined in the Tile Processor Application Binary 
//    Interface is used, with a couple of small exceptions:
//
//        - Global Registers Gthread and Gheap_base are maintained and assumed
//          to be set up on entry.  They are read-only to compiled code, and 
//          thus not saved/restored.
//        - Global Register GdispatchTables is not used by compiled code, but 
//          the register must not be used. We want it to 'pass thru' back into
//          the interpreter.
//        - If the method calls out, Resp and Rsaved_SP must be saved either at
//          entry or around calls, because they are input to interpreted 
//          routines from C2I adapters.
//        - Non-native calls out must pass the target method in Tmethod (R10).
//          Parameters are passed in registers 0-9, and the stack, as per the 
//          standard.
//
//----------------------------------------------------------------------------
//
// Interpreted-to-compiled (I2C) Adapters (AdapterGenerator::gen_i2c_adapter)
//
//    I2C adapters do not create a frame, but modify the frame of the 
//    interpreted caller.  They must:
//        - Copy the arguments from the expression stack to the argument 
//          registers, with those beyond 10 going to stack slots.  The stack 
//          slots at the top of the space reserved for the caller's expression
//          stack can be used. It is VERY likely we won't have to adjust SP, 
//          because all parameters were on the stack previously,  AND we're 
//          keeping the first 10 in registers. But if a callee has a huge 
//          argument list containing longs/doubles requiring stronger alignment
//          in native calls, it is possible we may need to bump SP.  In that 
//          case, the saved Frame Pointer must also be moved up to the new 
//          position just below the stack top.  The diagram below shows the 
//          case where we don't need to change SP.  (Callers have saved the
//          original SP in Rsaved_SP so that it can be restored on return.)  
//          If SP DOES have to modified, 16-byte-alignment must be preserved.
//        - Modify the caller's Resp so that on return, the expression stack 
//          will be popped. (Resp is callee save, so if the compiled code uses
//          it, it will be save/restored)
//        - On return, set SP to Rsaved_SP to remove any stack adjustments.
//
//      Stack prior to Adapter
//    +------------------------+  
//    | Java Stack slot 0      |
//    | Java Stack slot 1      |
//    | ...                    |
//    | Java Stack slot (n)    |              After Adapter
//    +------------------------+ ------ +------------------------+
//    |                        |        |                        | <-- Resp
//    |          All           |        |  Unused expression     |
//    |  Outgoing parameters   |        |     stack slots        |
//    +------------------------+        |                        |
//    | Unused Expression Stack|<- Resp +------------------------+
//    |   Slots ...            |        | Outgoing params > 10   |
//    +------------------------+ ------ +------------------------+
//    | Caller's Frame Pointer |        | Caller's Frame Pointer |
//    |  empty slot for RA     |<- SP   |  empty slot for RA     | <-- SP
//    +------------------------+        +------------------------+
//
//
//-----------------------------------------------------------------------------
//
// Compiled-to-interpreter (C2I) Adapters (AdapterGenerator::gen_c2i_adapter)
//
//   C2I adapters do not create a frame, but modify the frame of the compiled
//   caller.   They must:
//        - Modify the caller's SP to allow copying all arguments onto the 
//          stack. The saved FP will need to be moved as a result. Stack 
//          alignment must be preserved.
//        - Set the Resp pointer to point above the last pushed argument. 
//          The callee will use this for locating the base of the arguments on
//          the stack.
//        - Pass the method pointer of the callee in Tmethod (R10).
//        - Move the original SP to Rsaved_SP so the interpreted callee will 
//          return SP to that point.
//                                              
//
//      Stack prior to Adapter
//    +------------------------+
//    |  Compiled code stack   |
//    |    . . .               |              After Adapter
//    +------------------------+ ------ +------------------------+
//    |  Outgoing Args > 10    |        |                        | 
//    +------------------------+        |   All outgoing args    |
//    | Caller's Frame Pointer |        |    copied to stack     |
//    |  empty slot for RA     |<- SP   |                        | <- Rsaved_SP
//    +------------------------+        |                        |
//    |                        |        +------------------------+
//                                      | Caller's Frame Pointer | <-- Resp
//                                      |  empty slot for RA     | <-- SP
//                                      +------------------------+
//                                      |                        |
//
//
//----------------------------------------------------------------------------
//
// Java Call Stub  (stubGenerator_sparc.cpp - generate_call_stub)
//
//   Calls from native code to Java come in through the Java Call Stub, 
//   generated by stubGenerator, which is called via JavaCalls::call_helper.  
//   The Java Call Stub must perform a function similar to the C2I adapter, 
//   setting up the stack for an interpreter call, except that it creates its 
//   own frame, rather than modifying the frame of its caller.  The 
//   interpreted entry point is always called, but may be redirected to 
//   compiled code via an I2C adapter.
//
//   The stub must also establish the global registers expected by 
//   interpreted/compiled code: Gthread, Gheap_base, and Gdispatch_tables, and
//   hence must save the incoming values of those (callee-saved) registers.  
//   It also must set up Resp and Tmethod, both expected by interpreted code, 
//   and save the old value of Resp (Tmethod is caller-save).
//
//   The stub must save the "Link" argument in a slot accessible later in the 
//   call chain.  The arguments to the callee are received as a vector whose 
//   address is one of the parameters, and thus they are not in the arg 
//   registers and stack as they would be in a standard call.  The stub must 
//   copy them onto the stack locations expected by the interpreter, and point
//   Resp accordingly.  The first stub argument is a "link" (JavaCallWrapper 
//   address), which must be available during stack walks, so is saved in a 
//   dedicated frame location as well.
//
//   After calling the target, the stub must copy the result to the location 
//   indicated by the output parameter and of course restore the saved regs 
//   before returning.
//
//
//     Stack prior to Call Stub
//    +------------------------+
//    |  Native code's stack   |
//    |          .             |   
//    |          .             |            After Call Stub
//    +------------------------+ ------ +------------------------+
//    | Caller's Frame Pointer |        | Caller's Frame Pointer |
//    |  empty slot for RA     |<- SP   |   Saved return addr    | <-- FP
//    +------------------------+ ------ +------------------------+
//    |                        |        | Saved Link arg, Result |
//                                      |  Ptr and Type,  Resp,  |
//                                      |  Gthread, Gheap_base,  |
//                                      |    Gdispatch_tables    |
//                                      +------------------------+
//                                      |     All arguments      |  
//                                      |   copied to stack      |
//                                      +------------------------+
//                                      |  Stub's Frame Pointer  | <-- Resp
//                                      |  empty slot for RA     | <-- SP
//                                      +------------------------+
//                                      |                        |
//
//----------------------------------------------------------------------------
//
// Interpreter-to-Native Entry Stub  
//    (InterpreterGenerator::generate_native_entry)
//
//  This operates rather like an I2C adapter, except that a new frame is
//  created, rather than modifying the existing caller's frame. The
//  parameters must be moved to registers/stack, with arguments that are
//  heap addresses converted into handles. There is only one Int-to-native 
//  stub, (two, if you count the synchronized version) but multiple per-
//  signature signature handlers for marshalling the arguments. The caller's 
//  Resp is adjusted so the arguments are effectively 'popped' on return.
//
//  The fixed portion of this frame is identical to the normal interpreter
//  frame (containing the ISR registers and oop result slots). This is followed
//  by the in-memory portion of the argument list. Object pointer arguments
//  are 'converted' to handles by passing the address of the slot in the
//  caller's Java stack that contains the object pointer. 
//
//  The 'oop result' slot is used to temporarily hold an object pointer result
//  from the native call across possible GCs.
//
//
//         JUST BEFORE CALL
//    +------------------------+
//    | Java Stack slot 0      |
//    | Java Stack slot 1      |
//    | ...                    |
//    | Java Stack slot (n)    |
//    +------------------------+ ------ +------------------------+
//    |                        |        |                        | <-- Resp
//    |  Outgoing parameters   |        |                        |
//    |                        |        .                        .
//    +------------------------+        .                        .
//    |   Unused Java Stack    |<- Resp 
//    |    Slots . . .         |             After Stub Call
//    +------------------------+ ------ +------------------------+ 
//    | Caller's Frame Pointer |        | Caller's Frame Pointer |
//    |  empty slot for RA     |<- SP   |  Saved Return Address  | <-- FP
//    +------------------------+ ------ +------------------------+
//                                      |      Interpreter       |
//                                      |   State Registers (8)  |
//                                      |  Oop result+align (2)  |
//                                      +------------------------+
//                                      |  outgoing parameters   |
//                                      |        > 10            |
//                                      +------------------------+
//                                      |  Stub's Frame Pointer  |
//                                      |   empty slot for RA    | <-- SP
//                                      +------------------------+
//                                      |                        | 
//
//
//----------------------------------------------------------------------------
//
// Compiled to Native Wrapper (SharedRuntime::generate_native_wrapper)
//
//  Compiled-to-native wrappers adjust the stack and parameters for a call to 
//  native  code from compiled Java.  In particular, they must create handles
//  for any heap pointer arguments, and provide hidden arguments for static 
//  methods, so there is a good amount of argument register shuffling.  There
//  is an individual wrapper  generated per signature.
//
//  As always, 16-byte-alignment of the modified SP must be preserved, and 
//  long/double arguments in the native argument slots (>10) must be strongly 
//  aligned.
//
//  If PreferInterpreterNativeStubs is set, Hotspot will avoid creating 
//  compiled=>native adapters and go through the Interpreter=>Native instead. 
//  So supporting these wrappers can be considered an optimization.
//
//
//     Stack prior to Wrapper
//    +------------------------+
//    |  Compiled code stack   |
//    |          .             |
//    +------------------------+
//    |  Outgoing parameters   |
//    |         > 10           |            After wrapper call
//    +------------------------+ ------ +------------------------+
//    | Caller's Frame Pointer |        | Caller's Frame Pointer |
//    |  empty slot for RA     |<- SP   |   Saved return addr    | <-- FP
//    +------------------------+ ------ +------------------------+
//    |                        |        |  Handles for heap args |
//                                      |                        |
//                                      +------------------------+
//                                      | Class handle if static |
//                                      +------------------------+
//                                      |  Lock word (if synch)  |
//                                      +------------------------+
//                                      | return value slot      |
//                                      +------------------------+
//                                      |  Outgoing parameters   |
//                                      |        > 10            |
//                                      +------------------------+
//                                      |  Wrapper Frame Pointer |
//                                      |  empty slot for RA     | <-- SP
//                                      +------------------------+
//                                      |                        |
//
//
//
//-----------------------------------------------------------------------------
//
// Brief summary of the relevant parts of the native Tile ABI
//
//     Register   Assembler name  Type          Purpose
//     0-9        r0 - r9         Caller-saved  Param passing / return values
//     10 - 29    r10 - r29       Caller-saved
//     30 - 51    r30 - r51       Callee-saved
//     52         r52             Callee-saved  optional frame pointer
//     53         tp              Dedicated     Thread-local data
//     54         sp              Dedicated     Stack pointer
//     55         lr              Dedicated     Return address
//
// Stack grows downward.  At a call, arguments beyond the first 10 register 
// args are placed on the stack.   A pair consisting of the Caller's FP and a 
// slot for the return address are also placed on the stack. This might be 
// pictured as:
//
//               .
//      |          .             |
//      |  Caller's local stack  |
//      +------------------------+
//      |  Outgoing Args if >10  |
//      +------------------------+
//      | Caller's Frame Pointer |
//      |   Empty slot for RA    |<- SP
//      +------------------------+
//
// After the call, the callee's new FP is set to point to SP at the time of 
// the call, and the caller's return address received in R55 is written into 
// the stack slot. (Leaf routines need not follow this convention.)
//
// 'long long' and double arguments must be doubleword aligned. When passed in
// registers, they must begin in even-numbered registers and any 'padding' 
// register skipped.
//


#endif // CPU_RISCV_VM_FRAME_RISCV_HPP
