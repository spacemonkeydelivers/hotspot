/*
 * Copyright (c) 1997, 2013, Oracle and/or its affiliates. All rights reserved.
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
#include "code/debugInfoRec.hpp"
#include "code/icBuffer.hpp"
#include "code/vtableStubs.hpp"
#include "interpreter/interpreter.hpp"
#include "oops/compiledICHolder.hpp"
#include "prims/jvmtiRedefineClassesTrace.hpp"
#include "runtime/sharedRuntime.hpp"
#include "runtime/vframeArray.hpp"
#include "vmreg_riscv.inline.hpp"
#ifdef COMPILER1
#include "c1/c1_Runtime1.hpp"
#endif
#ifdef COMPILER2
#include "opto/runtime.hpp"
#include "adfiles/ad_riscv_64.hpp"
#endif

#define __ masm->

#ifdef PRODUCT
#define BLOCK_COMMENT(str) // nothing
#else
#define BLOCK_COMMENT(str) __ block_comment(str)
#endif

#define BIND(label) bind(label); BLOCK_COMMENT(#label ":")


class RegisterSaver {
 // Used for saving volatile registers.
 public:

  static OopMap* push_frame_and_save_live_registers(MacroAssembler* masm,
                         int* out_frame_size_in_bytes,
                         bool generate_oop_map,
                         int return_pc_adjustment);
  static void restore_live_registers_and_pop_frame(MacroAssembler* masm,
                         int frame_size_in_bytes,
                         bool restore_scratch);

  static void push_frame_and_save_argument_registers(MacroAssembler* masm,
                         Register r_temp,
                         int frame_size,
                         int total_args,
                         const VMRegPair *regs, const VMRegPair *regs2 = NULL);
  static void restore_argument_registers_and_pop_frame(MacroAssembler*masm,
                         int frame_size,
                         int total_args,
                         const VMRegPair *regs, const VMRegPair *regs2 = NULL);

  // During deoptimization only the result registers need to be restored
  // all the other values have already been extracted.
  static void restore_result_registers(MacroAssembler* masm, int frame_size_in_bytes);

  // Constants and data structures:

  typedef enum {
    int_reg           = 0,
    float_reg         = 1
  } RegisterType;

  typedef enum {
    reg_size          = 8,
    half_reg_size     = reg_size / 2
  } RegisterConstants;

  typedef struct {
    RegisterType        reg_type;
    int                 reg_num;
    VMReg               vmreg;
  } LiveRegType;
};

#define RegisterSaver_LiveIntReg(regname) \
  { RegisterSaver::int_reg,     regname->encoding(), regname->as_VMReg() }

#define RegisterSaver_LiveFloatReg(regname) \
  { RegisterSaver::float_reg,   regname->encoding(), regname->as_VMReg() }

static const RegisterSaver::LiveRegType RegisterSaver_LiveRegs[] = {
  // Live registers which get spilled to the stack. Register
  // positions in this array correspond directly to the stack layout.

  //
  // live float registers:
  //
  RegisterSaver_LiveFloatReg( F0  ),
  RegisterSaver_LiveFloatReg( F1  ),
  RegisterSaver_LiveFloatReg( F2  ),
  RegisterSaver_LiveFloatReg( F3  ),
  RegisterSaver_LiveFloatReg( F4  ),
  RegisterSaver_LiveFloatReg( F5  ),
  RegisterSaver_LiveFloatReg( F6  ),
  RegisterSaver_LiveFloatReg( F7  ),
  RegisterSaver_LiveFloatReg( F8  ),
  RegisterSaver_LiveFloatReg( F9  ),
  RegisterSaver_LiveFloatReg( F10 ),
  RegisterSaver_LiveFloatReg( F11 ),
  RegisterSaver_LiveFloatReg( F12 ),
  RegisterSaver_LiveFloatReg( F13 ),
  RegisterSaver_LiveFloatReg( F14 ),
  RegisterSaver_LiveFloatReg( F15 ),
  RegisterSaver_LiveFloatReg( F16 ),
  RegisterSaver_LiveFloatReg( F17 ),
  RegisterSaver_LiveFloatReg( F18 ),
  RegisterSaver_LiveFloatReg( F19 ),
  RegisterSaver_LiveFloatReg( F20 ),
  RegisterSaver_LiveFloatReg( F21 ),
  RegisterSaver_LiveFloatReg( F22 ),
  RegisterSaver_LiveFloatReg( F23 ),
  RegisterSaver_LiveFloatReg( F24 ),
  RegisterSaver_LiveFloatReg( F25 ),
  RegisterSaver_LiveFloatReg( F26 ),
  RegisterSaver_LiveFloatReg( F27 ),
  RegisterSaver_LiveFloatReg( F28 ),
  RegisterSaver_LiveFloatReg( F29 ),
  RegisterSaver_LiveFloatReg( F30 ),
  RegisterSaver_LiveFloatReg( F31 ),
  //
  // live integer registers:
  //
  //RegisterSaver_LiveIntReg( X0  ), // no need to save (is zero)
  //RegisterSaver_LiveIntReg( X1  ), // return address
  //RegisterSaver_LiveIntReg( X2  ), // stack pointer
  //RegisterSaver_LiveIntReg( X3  ), // global pointer
  //RegisterSaver_LiveIntReg( X4  ), // thread pointer
  RegisterSaver_LiveIntReg(   X5  ),
  RegisterSaver_LiveIntReg(   X6  ),
  RegisterSaver_LiveIntReg(   X7  ),
  //RegisterSaver_LiveIntReg( X8  ), // frame pointer
  RegisterSaver_LiveIntReg(   X9  ),
  RegisterSaver_LiveIntReg(   X10 ),
  RegisterSaver_LiveIntReg(   X11 ),
  RegisterSaver_LiveIntReg(   X12 ),
  RegisterSaver_LiveIntReg(   X13 ),
  RegisterSaver_LiveIntReg(   X14 ),
  RegisterSaver_LiveIntReg(   X15 ),
  RegisterSaver_LiveIntReg(   X16 ),
  RegisterSaver_LiveIntReg(   X17 ),
  RegisterSaver_LiveIntReg(   X18 ),
  RegisterSaver_LiveIntReg(   X19 ),
  RegisterSaver_LiveIntReg(   X20 ),
  RegisterSaver_LiveIntReg(   X21 ),
  RegisterSaver_LiveIntReg(   X22 ),
  RegisterSaver_LiveIntReg(   X23 ),
  RegisterSaver_LiveIntReg(   X24 ),
  RegisterSaver_LiveIntReg(   X25 ),
  RegisterSaver_LiveIntReg(   X26 ),
  RegisterSaver_LiveIntReg(   X27 ),
  RegisterSaver_LiveIntReg(   X28 ),
  RegisterSaver_LiveIntReg(   X29 ),
  RegisterSaver_LiveIntReg(   X30 ),
  RegisterSaver_LiveIntReg(   X31 )
};

OopMap* RegisterSaver::push_frame_and_save_live_registers(MacroAssembler* masm,
                         int* out_frame_size_in_bytes,
                         bool generate_oop_map,
                         int return_pc_adjustment) {
  // Push an frame_min_size-frame and store all registers which may be live.
  // If requested, create an OopMap: Record volatile registers as
  // callee-save values in an OopMap so their save locations will be
  // propagated to the RegisterMap of the caller frame during
  // StackFrameStream construction (needed for deoptimization; see
  // compiledVFrame::create_stack_value).
  // If return_pc_adjustment != 0 adjust the return pc by return_pc_adjustment.

  int i;
  int offset;

  // calcualte frame size
  const int regstosave_num       = sizeof(RegisterSaver_LiveRegs) /
                                   sizeof(RegisterSaver::LiveRegType);
  const int register_save_size   = regstosave_num * reg_size;
  const int frame_size_in_bytes  = round_to(register_save_size, frame::alignment_in_bytes);
  *out_frame_size_in_bytes       = frame_size_in_bytes;
  const int frame_size_in_slots  = frame_size_in_bytes / sizeof(jint);

  // OopMap frame size is in c2 stack slots (sizeof(jint)) not bytes or words.
  OopMap* map = generate_oop_map ? new OopMap(frame_size_in_slots, 0) : NULL;

  BLOCK_COMMENT("push_frame_and_save_live_registers {");

  // Save x31 in the last slot of the not yet pushed frame so that we
  // can use it as scratch reg.
  __ sd(X31, -reg_size*(frame::frame_min_size+1), X2_SP);
  assert(-reg_size == -frame_size_in_bytes + ((regstosave_num-1)*reg_size), "consistency check");

  // TODO: Verify if its okay to modify RA here...
  if (return_pc_adjustment != 0) {
    if (__ is_simm(return_pc_adjustment, 12)) {
      __ addi(X1_RA, X1_RA, return_pc_adjustment);
    } else {
      __ li(X31, return_pc_adjustment);
      __ add(X1_RA, X1_RA, X31);
    }
  }

  // push a new frame
  __ push_frame(frame_size_in_bytes, X31);

  // save all registers (ints and floats)
  offset = 0;
  for (int i = 0; i < regstosave_num; i++) {
    int reg_num  = RegisterSaver_LiveRegs[i].reg_num;
    int reg_type = RegisterSaver_LiveRegs[i].reg_type;

    switch (reg_type) {
      case RegisterSaver::int_reg: {
        if (reg_num != 31) { // We spilled X31 right at the beginning.
          __ sd(as_Register(reg_num), offset, X2_SP);
        }
        break;
      }
      case RegisterSaver::float_reg: {
        __ fsd(as_FloatRegister(reg_num), offset, X2_SP);
        break;
      }
      default:
        ShouldNotReachHere();
    }

    if (generate_oop_map) {
      map->set_callee_saved(VMRegImpl::stack2reg(offset>>2),
                            RegisterSaver_LiveRegs[i].vmreg);
      map->set_callee_saved(VMRegImpl::stack2reg((offset + half_reg_size)>>2),
                            RegisterSaver_LiveRegs[i].vmreg->next());
    }
    offset += reg_size;
  }

  BLOCK_COMMENT("} push_frame_and_save_live_registers");

  // And we're done.
  return map;
}


// Pop the current frame and restore all the registers that we
// saved.
void RegisterSaver::restore_live_registers_and_pop_frame(MacroAssembler* masm,
                                                         int frame_size_in_bytes,
                                                         bool restore_scratch) {
  int i;
  int offset;
  const int regstosave_num       = sizeof(RegisterSaver_LiveRegs) /
                                   sizeof(RegisterSaver::LiveRegType);
  const int register_save_size   = regstosave_num * reg_size;

  BLOCK_COMMENT("restore_live_registers_and_pop_frame {");

  // restore all registers (ints and floats)
  offset = 0;
  for (int i = 0; i < regstosave_num; i++) {
    int reg_num  = RegisterSaver_LiveRegs[i].reg_num;
    int reg_type = RegisterSaver_LiveRegs[i].reg_type;

    switch (reg_type) {
      case RegisterSaver::int_reg: {
        if (reg_num != 31) // X31 restored at the end, it's the tmp reg!
          __ ld(as_Register(reg_num), offset, X2_SP);
        break;
      }
      case RegisterSaver::float_reg: {
        __ fld(as_FloatRegister(reg_num), offset, X2_SP);
        break;
      }
      default:
        ShouldNotReachHere();
    }
    offset += reg_size;
  }

  // pop the frame
  __ pop_frame();

  // restore scratch register's value
  if (restore_scratch)
    __ ld(X31, -reg_size*frame::frame_min_size, X2_SP);

  BLOCK_COMMENT("} restore_live_registers_and_pop_frame");
}

void RegisterSaver::push_frame_and_save_argument_registers(MacroAssembler* masm, Register r_temp,
                                                           int frame_size,int total_args, const VMRegPair *regs,
                                                           const VMRegPair *regs2) {
  ShouldNotReachHere();
  /*
  __ push_frame(frame_size, r_temp);
  int st_off = frame_size - wordSize;
  for (int i = 0; i < total_args; i++) {
    VMReg r_1 = regs[i].first();
    VMReg r_2 = regs[i].second();
    if (!r_1->is_valid()) {
      assert(!r_2->is_valid(), "");
      continue;
    }
    if (r_1->is_Register()) {
      Register r = r_1->as_Register();
      __ std(r, st_off, R1_SP);
      st_off -= wordSize;
    } else if (r_1->is_FloatRegister()) {
      FloatRegister f = r_1->as_FloatRegister();
      __ stfd(f, st_off, R1_SP);
      st_off -= wordSize;
    }
  }
  if (regs2 != NULL) {
    for (int i = 0; i < total_args; i++) {
      VMReg r_1 = regs2[i].first();
      VMReg r_2 = regs2[i].second();
      if (!r_1->is_valid()) {
        assert(!r_2->is_valid(), "");
        continue;
      }
      if (r_1->is_Register()) {
        Register r = r_1->as_Register();
        __ std(r, st_off, R1_SP);
        st_off -= wordSize;
      } else if (r_1->is_FloatRegister()) {
        FloatRegister f = r_1->as_FloatRegister();
        __ stfd(f, st_off, R1_SP);
        st_off -= wordSize;
      }
    }
  }
  */
}

void RegisterSaver::restore_argument_registers_and_pop_frame(MacroAssembler*masm, int frame_size,
                                                             int total_args, const VMRegPair *regs,
                                                             const VMRegPair *regs2) {
  ShouldNotReachHere();
  /*
  int st_off = frame_size - wordSize;
  for (int i = 0; i < total_args; i++) {
    VMReg r_1 = regs[i].first();
    VMReg r_2 = regs[i].second();
    if (r_1->is_Register()) {
      Register r = r_1->as_Register();
      __ ld(r, st_off, R1_SP);
      st_off -= wordSize;
    } else if (r_1->is_FloatRegister()) {
      FloatRegister f = r_1->as_FloatRegister();
      __ lfd(f, st_off, R1_SP);
      st_off -= wordSize;
    }
  }
  if (regs2 != NULL)
    for (int i = 0; i < total_args; i++) {
      VMReg r_1 = regs2[i].first();
      VMReg r_2 = regs2[i].second();
      if (r_1->is_Register()) {
        Register r = r_1->as_Register();
        __ ld(r, st_off, R1_SP);
        st_off -= wordSize;
      } else if (r_1->is_FloatRegister()) {
        FloatRegister f = r_1->as_FloatRegister();
        __ lfd(f, st_off, R1_SP);
        st_off -= wordSize;
      }
    }
  __ pop_frame();
  */
}

// Restore the registers that might be holding a result.
void RegisterSaver::restore_result_registers(MacroAssembler* masm, int frame_size_in_bytes) {
  ShouldNotReachHere();
  /*
  int i;
  int offset;
  const int regstosave_num       = sizeof(RegisterSaver_LiveRegs) /
                                   sizeof(RegisterSaver::LiveRegType);
  const int register_save_size   = regstosave_num * reg_size;
  const int register_save_offset = frame_size_in_bytes - register_save_size;

  // restore all result registers (ints and floats)
  offset = register_save_offset;
  for (int i = 0; i < regstosave_num; i++) {
    int reg_num  = RegisterSaver_LiveRegs[i].reg_num;
    int reg_type = RegisterSaver_LiveRegs[i].reg_type;
    switch (reg_type) {
      case RegisterSaver::int_reg: {
        if (as_Register(reg_num)==R3_RET) // int result_reg
          __ ld(as_Register(reg_num), offset, R1_SP);
        break;
      }
      case RegisterSaver::float_reg: {
        if (as_FloatRegister(reg_num)==F1_RET) // float result_reg
          __ lfd(as_FloatRegister(reg_num), offset, R1_SP);
        break;
      }
      case RegisterSaver::special_reg: {
        // Special registers don't hold a result.
        break;
      }
      default:
        ShouldNotReachHere();
    }
    offset += reg_size;
  }
  */
}

#ifdef COMPILER2
static int reg2slot(VMReg r) {
  ShouldNotReachHere();
  return 0;
  /*
  return r->reg2stack() + SharedRuntime::out_preserve_stack_slots();
  */
}

static int reg2offset(VMReg r) {
  ShouldNotReachHere();
  return 0;
  /*
  return (r->reg2stack() + SharedRuntime::out_preserve_stack_slots()) * VMRegImpl::stack_slot_size;
  */
}
#endif

// ---------------------------------------------------------------------------
// Read the array of BasicTypes from a signature, and compute where the
// arguments should go. Values in the VMRegPair regs array refer to 4-byte
// quantities. Values less than VMRegImpl::stack0 are registers, those above
// refer to 4-byte stack slots. All stack slots are based off of the stack pointer
// as framesizes are fixed.
// VMRegImpl::stack0 refers to the first slot 0(sp).
// and VMRegImpl::stack0+1 refers to the memory word 4-bytes higher. Register
// up to RegisterImpl::number_of_registers) are the 64-bit
// integer registers.

// Note: the INPUTS in sig_bt are in units of Java argument words, which are
// either 32-bit or 64-bit depending on the build. The OUTPUTS are in 32-bit
// units regardless of build. Of course for i486 there is no 64 bit build

// The Java calling convention is a "shifted" version of the C ABI.
// By skipping the first C ABI register we can call non-static jni methods
// with small numbers of arguments without having to shuffle the arguments
// at all. Since we control the java ABI we ought to at least get some
// advantage out of it.

const VMReg java_iarg_reg[8] = {
  X10->as_VMReg(),
  X11->as_VMReg(),
  X12->as_VMReg(),
  X13->as_VMReg(),
  X14->as_VMReg(),
  X15->as_VMReg(),
  X16->as_VMReg(),
  X17->as_VMReg()
};

const VMReg java_farg_reg[8] = {
  F10->as_VMReg(),
  F11->as_VMReg(),
  F12->as_VMReg(),
  F13->as_VMReg(),
  F14->as_VMReg(),
  F15->as_VMReg(),
  F16->as_VMReg(),
  F17->as_VMReg(),
};

const int num_java_iarg_registers = sizeof(java_iarg_reg) / sizeof(java_iarg_reg[0]);
const int num_java_farg_registers = sizeof(java_farg_reg) / sizeof(java_farg_reg[0]);

int SharedRuntime::java_calling_convention(const BasicType *sig_bt,
                                           VMRegPair *regs,
                                           int total_args_passed,
                                           int is_outgoing) {
  // TODO: figure this out
  // C2c calling conventions for compiled-compiled calls.
  // Put 8 ints/longs into registers _AND_ 8 float/doubles into
  // registers _AND_ put the rest on the stack.

  const int inc_stk_for_intfloat   = 1; // 1 slots for ints and floats
  const int inc_stk_for_longdouble = 2; // 2 slots for longs and doubles

  int i;
  VMReg reg;
  int stk = 0;
  int ireg = 0;
  int freg = 0;

  // We put the first 8 arguments into registers and the rest on the
  // stack, float arguments are already in their argument registers
  // due to c2c calling conventions (see calling_convention).
  for (int i = 0; i < total_args_passed; ++i) {
    switch(sig_bt[i]) {
    case T_BOOLEAN:
    case T_CHAR:
    case T_BYTE:
    case T_SHORT:
    case T_INT:
      if (ireg < num_java_iarg_registers) {
        // Put int/ptr in register
        reg = java_iarg_reg[ireg];
        ++ireg;
      } else {
        // Put int/ptr on stack.
        reg = VMRegImpl::stack2reg(stk);
        stk += inc_stk_for_intfloat;
      }
      regs[i].set1(reg);
      break;
    case T_LONG:
      assert(sig_bt[i+1] == T_VOID, "expecting half");
      if (ireg < num_java_iarg_registers) {
        // Put long in register.
        reg = java_iarg_reg[ireg];
        ++ireg;
      } else {
        // Put long on stack. They must be aligned to 2 slots.
        if (stk & 0x1) ++stk;
        reg = VMRegImpl::stack2reg(stk);
        stk += inc_stk_for_longdouble;
      }
      regs[i].set2(reg);
      break;
    case T_OBJECT:
    case T_ARRAY:
    case T_ADDRESS:
      if (ireg < num_java_iarg_registers) {
        // Put ptr in register.
        reg = java_iarg_reg[ireg];
        ++ireg;
      } else {
        // Put ptr on stack. Objects must be aligned to 2 slots too,
        // because "64-bit pointers record oop-ishness on 2 aligned
        // adjacent registers." (see OopFlow::build_oop_map).
        if (stk & 0x1) ++stk;
        reg = VMRegImpl::stack2reg(stk);
        stk += inc_stk_for_longdouble;
      }
      regs[i].set2(reg);
      break;
    case T_FLOAT:
      if (freg < num_java_farg_registers) {
        // Put float in register.
        reg = java_farg_reg[freg];
        ++freg;
      } else {
        // Put float on stack.
        reg = VMRegImpl::stack2reg(stk);
        stk += inc_stk_for_intfloat;
      }
      regs[i].set1(reg);
      break;
    case T_DOUBLE:
      assert(sig_bt[i+1] == T_VOID, "expecting half");
      if (freg < num_java_farg_registers) {
        // Put double in register.
        reg = java_farg_reg[freg];
        ++freg;
      } else {
        // Put double on stack. They must be aligned to 2 slots.
        if (stk & 0x1) ++stk;
        reg = VMRegImpl::stack2reg(stk);
        stk += inc_stk_for_longdouble;
      }
      regs[i].set2(reg);
      break;
    case T_VOID:
      // Do not count halves.
      regs[i].set_bad();
      break;
    default:
      ShouldNotReachHere();
    }
  }
  return round_to(stk, 2);
}

#ifdef COMPILER2
// Calling convention for calling C code.
int SharedRuntime::c_calling_convention(const BasicType *sig_bt,
                                        VMRegPair *regs,
                                        VMRegPair *regs2,
                                        int total_args_passed) {
  ShouldNotReachHere();
  return 0;
  /*
  // Calling conventions for C runtime calls and calls to JNI native methods.
  //
  // RISCV64 convention: Hoist the first 8 int/ptr/long's in the first 8
  // int regs, leaving int regs undefined if the arg is flt/dbl. Hoist
  // the first 13 flt/dbl's in the first 13 fp regs but additionally
  // copy flt/dbl to the stack if they are beyond the 8th argument.

  const VMReg iarg_reg[8] = {
    R3->as_VMReg(),
    R4->as_VMReg(),
    R5->as_VMReg(),
    R6->as_VMReg(),
    R7->as_VMReg(),
    R8->as_VMReg(),
    R9->as_VMReg(),
    R10->as_VMReg()
  };

  const VMReg farg_reg[13] = {
    F1->as_VMReg(),
    F2->as_VMReg(),
    F3->as_VMReg(),
    F4->as_VMReg(),
    F5->as_VMReg(),
    F6->as_VMReg(),
    F7->as_VMReg(),
    F8->as_VMReg(),
    F9->as_VMReg(),
    F10->as_VMReg(),
    F11->as_VMReg(),
    F12->as_VMReg(),
    F13->as_VMReg()
  };

  // Check calling conventions consistency.
  assert(sizeof(iarg_reg) / sizeof(iarg_reg[0]) == Argument::n_int_register_parameters_c &&
         sizeof(farg_reg) / sizeof(farg_reg[0]) == Argument::n_float_register_parameters_c,
         "consistency");

  // `Stk' counts stack slots. Due to alignment, 32 bit values occupy
  // 2 such slots, like 64 bit values do.
  const int inc_stk_for_intfloat   = 2; // 2 slots for ints and floats
  const int inc_stk_for_longdouble = 2; // 2 slots for longs and doubles

  int i;
  VMReg reg;
  // Leave room for C-compatible ABI_REG_ARGS.
  int stk = (frame::abi_reg_args_size - frame::jit_out_preserve_size) / VMRegImpl::stack_slot_size;
  int arg = 0;
  int freg = 0;

  // Avoid passing C arguments in the wrong stack slots.
#if defined(ABI_ELFv2)
  assert((SharedRuntime::out_preserve_stack_slots() + stk) * VMRegImpl::stack_slot_size == 96,
         "passing C arguments in wrong stack slots");
#else
  assert((SharedRuntime::out_preserve_stack_slots() + stk) * VMRegImpl::stack_slot_size == 112,
         "passing C arguments in wrong stack slots");
#endif
  // We fill-out regs AND regs2 if an argument must be passed in a
  // register AND in a stack slot. If regs2 is NULL in such a
  // situation, we bail-out with a fatal error.
  for (int i = 0; i < total_args_passed; ++i, ++arg) {
    // Initialize regs2 to BAD.
    if (regs2 != NULL) regs2[i].set_bad();

    switch(sig_bt[i]) {

    //
    // If arguments 0-7 are integers, they are passed in integer registers.
    // Argument i is placed in iarg_reg[i].
    //
    case T_BOOLEAN:
    case T_CHAR:
    case T_BYTE:
    case T_SHORT:
    case T_INT:
      // We must cast ints to longs and use full 64 bit stack slots
      // here. We do the cast in GraphKit::gen_stub() and just guard
      // here against loosing that change.
      assert(CCallingConventionRequiresIntsAsLongs,
             "argument of type int should be promoted to type long");
      guarantee(i > 0 && sig_bt[i-1] == T_LONG,
                "argument of type (bt) should have been promoted to type (T_LONG,bt) for bt in "
                "{T_BOOLEAN, T_CHAR, T_BYTE, T_SHORT, T_INT}");
      // Do not count halves.
      regs[i].set_bad();
      --arg;
      break;
    case T_LONG:
      guarantee(sig_bt[i+1] == T_VOID    ||
                sig_bt[i+1] == T_BOOLEAN || sig_bt[i+1] == T_CHAR  ||
                sig_bt[i+1] == T_BYTE    || sig_bt[i+1] == T_SHORT ||
                sig_bt[i+1] == T_INT,
                "expecting type (T_LONG,half) or type (T_LONG,bt) with bt in {T_BOOLEAN, T_CHAR, T_BYTE, T_SHORT, T_INT}");
    case T_OBJECT:
    case T_ARRAY:
    case T_ADDRESS:
    case T_METADATA:
      // Oops are already boxed if required (JNI).
      if (arg < Argument::n_int_register_parameters_c) {
        reg = iarg_reg[arg];
      } else {
        reg = VMRegImpl::stack2reg(stk);
        stk += inc_stk_for_longdouble;
      }
      regs[i].set2(reg);
      break;

    //
    // Floats are treated differently from int regs:  The first 13 float arguments
    // are passed in registers (not the float args among the first 13 args).
    // Thus argument i is NOT passed in farg_reg[i] if it is float.  It is passed
    // in farg_reg[j] if argument i is the j-th float argument of this call.
    //
    case T_FLOAT:
      if (freg < Argument::n_float_register_parameters_c) {
        // Put float in register ...
        reg = farg_reg[freg];
        ++freg;

        // Argument i for i > 8 is placed on the stack even if it's
        // placed in a register (if it's a float arg). Aix disassembly
        // shows that xlC places these float args on the stack AND in
        // a register. This is not documented, but we follow this
        // convention, too.
        if (arg >= Argument::n_regs_not_on_stack_c) {
          // ... and on the stack.
          guarantee(regs2 != NULL, "must pass float in register and stack slot");
          VMReg reg2 = VMRegImpl::stack2reg(stk LINUX_ONLY(+1));
          regs2[i].set1(reg2);
          stk += inc_stk_for_intfloat;
        }

      } else {
        // Put float on stack.
        reg = VMRegImpl::stack2reg(stk LINUX_ONLY(+1));
        stk += inc_stk_for_intfloat;
      }
      regs[i].set1(reg);
      break;
    case T_DOUBLE:
      assert(sig_bt[i+1] == T_VOID, "expecting half");
      if (freg < Argument::n_float_register_parameters_c) {
        // Put double in register ...
        reg = farg_reg[freg];
        ++freg;

        // Argument i for i > 8 is placed on the stack even if it's
        // placed in a register (if it's a double arg). Aix disassembly
        // shows that xlC places these float args on the stack AND in
        // a register. This is not documented, but we follow this
        // convention, too.
        if (arg >= Argument::n_regs_not_on_stack_c) {
          // ... and on the stack.
          guarantee(regs2 != NULL, "must pass float in register and stack slot");
          VMReg reg2 = VMRegImpl::stack2reg(stk);
          regs2[i].set2(reg2);
          stk += inc_stk_for_longdouble;
        }
      } else {
        // Put double on stack.
        reg = VMRegImpl::stack2reg(stk);
        stk += inc_stk_for_longdouble;
      }
      regs[i].set2(reg);
      break;

    case T_VOID:
      // Do not count halves.
      regs[i].set_bad();
      --arg;
      break;
    default:
      ShouldNotReachHere();
    }
  }

  return round_to(stk, 2);
  */
}
#endif // COMPILER2

static address gen_c2i_adapter(MacroAssembler *masm,
                            int total_args_passed,
                            int comp_args_on_stack,
                            const BasicType *sig_bt,
                            const VMRegPair *regs,
                            Label& call_interpreter,
                            const Register& ientry) {
  address c2i_entrypoint;
/*
  const Register sender_SP = R21_sender_SP; // == R21_tmp1
  const Register code      = R22_tmp2;
  //const Register ientry  = R23_tmp3;
  const Register value_regs[] = { R24_tmp4, R25_tmp5, R26_tmp6 };
  const int num_value_regs = sizeof(value_regs) / sizeof(Register);
  int value_regs_index = 0;

  const Register return_pc = R27_tmp7;
  const Register tmp       = R28_tmp8;

  assert_different_registers(sender_SP, code, ientry, return_pc, tmp);

  // Adapter needs TOP_IJAVA_FRAME_ABI.
  const int adapter_size = frame::top_ijava_frame_abi_size +
                           round_to(total_args_passed * wordSize, frame::alignment_in_bytes);
*/

  // regular (verified) c2i entry point
  c2i_entrypoint = __ pc();
  __ unimplemented("c2i adapter not implemented");
/*
  // Does compiled code exists? If yes, patch the caller's callsite.
  __ ld(code, method_(code));
  __ cmpdi(CCR0, code, 0);
  __ ld(ientry, method_(interpreter_entry)); // preloaded
  __ beq(CCR0, call_interpreter);


  // Patch caller's callsite, method_(code) was not NULL which means that
  // compiled code exists.
  __ mflr(return_pc);
  __ std(return_pc, _abi(lr), R1_SP);
  RegisterSaver::push_frame_and_save_argument_registers(masm, tmp, adapter_size, total_args_passed, regs);

  __ call_VM_leaf(CAST_FROM_FN_PTR(address, SharedRuntime::fixup_callers_callsite), R19_method, return_pc);

  RegisterSaver::restore_argument_registers_and_pop_frame(masm, adapter_size, total_args_passed, regs);
  __ ld(return_pc, _abi(lr), R1_SP);
  __ ld(ientry, method_(interpreter_entry)); // preloaded
  __ mtlr(return_pc);


  // Call the interpreter.
  __ BIND(call_interpreter);
  __ mtctr(ientry);

  // Get a copy of the current SP for loading caller's arguments.
  __ mr(sender_SP, R1_SP);

  // Add space for the adapter.
  __ resize_frame(-adapter_size, R12_scratch2);

  int st_off = adapter_size - wordSize;

  // Write the args into the outgoing interpreter space.
  for (int i = 0; i < total_args_passed; i++) {
    VMReg r_1 = regs[i].first();
    VMReg r_2 = regs[i].second();
    if (!r_1->is_valid()) {
      assert(!r_2->is_valid(), "");
      continue;
    }
    if (r_1->is_stack()) {
      Register tmp_reg = value_regs[value_regs_index];
      value_regs_index = (value_regs_index + 1) % num_value_regs;
      // The calling convention produces OptoRegs that ignore the out
      // preserve area (JIT's ABI). We must account for it here.
      int ld_off = (r_1->reg2stack() + SharedRuntime::out_preserve_stack_slots()) * VMRegImpl::stack_slot_size;
      if (!r_2->is_valid()) {
        __ lwz(tmp_reg, ld_off, sender_SP);
      } else {
        __ ld(tmp_reg, ld_off, sender_SP);
      }
      // Pretend stack targets were loaded into tmp_reg.
      r_1 = tmp_reg->as_VMReg();
    }

    if (r_1->is_Register()) {
      Register r = r_1->as_Register();
      if (!r_2->is_valid()) {
        __ stw(r, st_off, R1_SP);
        st_off-=wordSize;
      } else {
        // Longs are given 2 64-bit slots in the interpreter, but the
        // data is passed in only 1 slot.
        if (sig_bt[i] == T_LONG || sig_bt[i] == T_DOUBLE) {
          DEBUG_ONLY( __ li(tmp, 0); __ std(tmp, st_off, R1_SP); )
          st_off-=wordSize;
        }
        __ std(r, st_off, R1_SP);
        st_off-=wordSize;
      }
    } else {
      assert(r_1->is_FloatRegister(), "");
      FloatRegister f = r_1->as_FloatRegister();
      if (!r_2->is_valid()) {
        __ stfs(f, st_off, R1_SP);
        st_off-=wordSize;
      } else {
        // In 64bit, doubles are given 2 64-bit slots in the interpreter, but the
        // data is passed in only 1 slot.
        // One of these should get known junk...
        DEBUG_ONLY( __ li(tmp, 0); __ std(tmp, st_off, R1_SP); )
        st_off-=wordSize;
        __ stfd(f, st_off, R1_SP);
        st_off-=wordSize;
      }
    }
  }

  // Jump to the interpreter just as if interpreter was doing it.

#ifdef CC_INTERP
  const Register tos = R17_tos;
#else
  const Register tos = R15_esp;
  __ load_const_optimized(R25_templateTableBase, (address)Interpreter::dispatch_table((TosState)0), R11_scratch1);
#endif

  // load TOS
  __ addi(tos, R1_SP, st_off);

  // Frame_manager expects initial_caller_sp (= SP without resize by c2i) in R21_tmp1.
  assert(sender_SP == R21_sender_SP, "passing initial caller's SP in wrong register");
  __ bctr();
*/
  return c2i_entrypoint;
}

static void gen_i2c_adapter(MacroAssembler *masm,
                            int total_args_passed,
                            int comp_args_on_stack,
                            const BasicType *sig_bt,
                            const VMRegPair *regs) {

  __ unimplemented("i2c adapter not implemented");
  /*
  // Load method's entry-point from method.
  __ ld(R12_scratch2, in_bytes(Method::from_compiled_offset()), R19_method);
  __ mtctr(R12_scratch2);

  // We will only enter here from an interpreted frame and never from after
  // passing thru a c2i. Azul allowed this but we do not. If we lose the
  // race and use a c2i we will remain interpreted for the race loser(s).
  // This removes all sorts of headaches on the x86 side and also eliminates
  // the possibility of having c2i -> i2c -> c2i -> ... endless transitions.

  // Note: r13 contains the senderSP on entry. We must preserve it since
  // we may do a i2c -> c2i transition if we lose a race where compiled
  // code goes non-entrant while we get args ready.
  // In addition we use r13 to locate all the interpreter args as
  // we must align the stack to 16 bytes on an i2c entry else we
  // lose alignment we expect in all compiled code and register
  // save code can segv when fxsave instructions find improperly
  // aligned stack pointer.

#ifdef CC_INTERP
  const Register ld_ptr = R17_tos;
#else
  const Register ld_ptr = R15_esp;
#endif

  const Register value_regs[] = { R22_tmp2, R23_tmp3, R24_tmp4, R25_tmp5, R26_tmp6 };
  const int num_value_regs = sizeof(value_regs) / sizeof(Register);
  int value_regs_index = 0;

  int ld_offset = total_args_passed*wordSize;

  // Cut-out for having no stack args. Since up to 2 int/oop args are passed
  // in registers, we will occasionally have no stack args.
  int comp_words_on_stack = 0;
  if (comp_args_on_stack) {
    // Sig words on the stack are greater-than VMRegImpl::stack0. Those in
    // registers are below. By subtracting stack0, we either get a negative
    // number (all values in registers) or the maximum stack slot accessed.

    // Convert 4-byte c2 stack slots to words.
    comp_words_on_stack = round_to(comp_args_on_stack*VMRegImpl::stack_slot_size, wordSize)>>LogBytesPerWord;
    // Round up to miminum stack alignment, in wordSize.
    comp_words_on_stack = round_to(comp_words_on_stack, 2);
    __ resize_frame(-comp_words_on_stack * wordSize, R11_scratch1);
  }

  // Now generate the shuffle code.  Pick up all register args and move the
  // rest through register value=Z_R12.
  BLOCK_COMMENT("Shuffle arguments");
  for (int i = 0; i < total_args_passed; i++) {
    if (sig_bt[i] == T_VOID) {
      assert(i > 0 && (sig_bt[i-1] == T_LONG || sig_bt[i-1] == T_DOUBLE), "missing half");
      continue;
    }

    // Pick up 0, 1 or 2 words from ld_ptr.
    assert(!regs[i].second()->is_valid() || regs[i].first()->next() == regs[i].second(),
            "scrambled load targets?");
    VMReg r_1 = regs[i].first();
    VMReg r_2 = regs[i].second();
    if (!r_1->is_valid()) {
      assert(!r_2->is_valid(), "");
      continue;
    }
    if (r_1->is_FloatRegister()) {
      if (!r_2->is_valid()) {
        __ lfs(r_1->as_FloatRegister(), ld_offset, ld_ptr);
        ld_offset-=wordSize;
      } else {
        // Skip the unused interpreter slot.
        __ lfd(r_1->as_FloatRegister(), ld_offset-wordSize, ld_ptr);
        ld_offset-=2*wordSize;
      }
    } else {
      Register r;
      if (r_1->is_stack()) {
        // Must do a memory to memory move thru "value".
        r = value_regs[value_regs_index];
        value_regs_index = (value_regs_index + 1) % num_value_regs;
      } else {
        r = r_1->as_Register();
      }
      if (!r_2->is_valid()) {
        // Not sure we need to do this but it shouldn't hurt.
        if (sig_bt[i] == T_OBJECT || sig_bt[i] == T_ADDRESS || sig_bt[i] == T_ARRAY) {
          __ ld(r, ld_offset, ld_ptr);
          ld_offset-=wordSize;
        } else {
          __ lwz(r, ld_offset, ld_ptr);
          ld_offset-=wordSize;
        }
      } else {
        // In 64bit, longs are given 2 64-bit slots in the interpreter, but the
        // data is passed in only 1 slot.
        if (sig_bt[i] == T_LONG || sig_bt[i] == T_DOUBLE) {
          ld_offset-=wordSize;
        }
        __ ld(r, ld_offset, ld_ptr);
        ld_offset-=wordSize;
      }

      if (r_1->is_stack()) {
        // Now store value where the compiler expects it
        int st_off = (r_1->reg2stack() + SharedRuntime::out_preserve_stack_slots())*VMRegImpl::stack_slot_size;

        if (sig_bt[i] == T_INT   || sig_bt[i] == T_FLOAT ||sig_bt[i] == T_BOOLEAN ||
            sig_bt[i] == T_SHORT || sig_bt[i] == T_CHAR  || sig_bt[i] == T_BYTE) {
          __ stw(r, st_off, R1_SP);
        } else {
          __ std(r, st_off, R1_SP);
        }
      }
    }
  }

  BLOCK_COMMENT("Store method");
  // Store method into thread->callee_target.
  // We might end up in handle_wrong_method if the callee is
  // deoptimized as we race thru here. If that happens we don't want
  // to take a safepoint because the caller frame will look
  // interpreted and arguments are now "compiled" so it is much better
  // to make this transition invisible to the stack walking
  // code. Unfortunately if we try and find the callee by normal means
  // a safepoint is possible. So we stash the desired callee in the
  // thread and the vm will find there should this case occur.
  __ std(R19_method, thread_(callee_target));

  // Jump to the compiled code just as if compiled code was doing it.
  __ bctr();
  */
}

AdapterHandlerEntry* SharedRuntime::generate_i2c2i_adapters(MacroAssembler *masm,
                                                            int total_args_passed,
                                                            int comp_args_on_stack,
                                                            const BasicType *sig_bt,
                                                            const VMRegPair *regs,
                                                            AdapterFingerPrint* fingerprint) {
  address i2c_entry;
  address c2i_unverified_entry;
  address c2i_entry;

  // entry: i2c
  __ align(CodeEntryAlignment);
  i2c_entry = __ pc();
  gen_i2c_adapter(masm, total_args_passed, comp_args_on_stack, sig_bt, regs);

  // entry: c2i unverified
  __ align(CodeEntryAlignment);
  BLOCK_COMMENT("c2i unverified entry");
  c2i_unverified_entry = __ pc();

  Label call_interpreter;
  const Register ientry         = X26_tmp3;
/*
  // inline_cache contains a compiledICHolder
  const Register ic             = R19_method;
  const Register ic_klass       = R11_scratch1;
  const Register receiver_klass = R12_scratch2;
  const Register code           = R21_tmp1;
  const Register ientry         = R23_tmp3;

  assert_different_registers(ic, ic_klass, receiver_klass, R3_ARG1, code, ientry);
  assert(R11_scratch1 == R11, "need prologue scratch register");

  assert(!MacroAssembler::needs_explicit_null_check(oopDesc::klass_offset_in_bytes()),
         "klass offset should reach into any page");
  // Check for NULL argument if we don't have implicit null checks.
  if (!ImplicitNullChecks || !os::zero_page_read_protected()) {
    if (TrapBasedNullChecks) {
      __ trap_null_check(R3_ARG1);
    } else {
      Label valid;
      __ cmpdi(CCR0, R3_ARG1, 0);
      __ bne_predict_taken(CCR0, valid);
      // We have a null argument, branch to ic_miss_stub.
      __ b64_patchable((address)SharedRuntime::get_ic_miss_stub(),
                       relocInfo::runtime_call_type);
      __ BIND(valid);
    }
  }
  // Assume argument is not NULL, load klass from receiver.
  __ load_klass(receiver_klass, R3_ARG1);

  __ ld(ic_klass, CompiledICHolder::holder_klass_offset(), ic);

  if (TrapBasedICMissChecks) {
    __ trap_ic_miss_check(receiver_klass, ic_klass);
  } else {
    Label valid;
    __ cmpd(CCR0, receiver_klass, ic_klass);
    __ beq_predict_taken(CCR0, valid);
    // We have an unexpected klass, branch to ic_miss_stub.
    __ b64_patchable((address)SharedRuntime::get_ic_miss_stub(),
                     relocInfo::runtime_call_type);
    __ BIND(valid);
  }

  // Argument is valid and klass is as expected, continue.

  // Extract method from inline cache, verified entry point needs it.
  __ ld(R19_method, CompiledICHolder::holder_method_offset(), ic);
  assert(R19_method == ic, "the inline cache register is dead here");

  __ ld(code, method_(code));
  __ cmpdi(CCR0, code, 0);
  __ ld(ientry, method_(interpreter_entry)); // preloaded
  __ beq_predict_taken(CCR0, call_interpreter);

  // Branch to ic_miss_stub.
  __ b64_patchable((address)SharedRuntime::get_ic_miss_stub(), relocInfo::runtime_call_type);
*/

  __ unimplemented("i2c2i adapter not implemented");

  // entry: c2i
  c2i_entry = gen_c2i_adapter(masm, total_args_passed, comp_args_on_stack, sig_bt, regs, call_interpreter, ientry);

  return AdapterHandlerLibrary::new_entry(fingerprint, i2c_entry, c2i_entry, c2i_unverified_entry);
}

#ifdef COMPILER2
// An oop arg. Must pass a handle not the oop itself.
static void object_move(MacroAssembler* masm,
                        int frame_size_in_slots,
                        OopMap* oop_map, int oop_handle_offset,
                        bool is_receiver, int* receiver_offset,
                        VMRegPair src, VMRegPair dst,
                        Register r_caller_sp, Register r_temp_1, Register r_temp_2) {
  ShouldNotReachHere();
  /*
  assert(!is_receiver || (is_receiver && (*receiver_offset == -1)),
         "receiver has already been moved");

  // We must pass a handle. First figure out the location we use as a handle.

  if (src.first()->is_stack()) {
    // stack to stack or reg

    const Register r_handle = dst.first()->is_stack() ? r_temp_1 : dst.first()->as_Register();
    Label skip;
    const int oop_slot_in_callers_frame = reg2slot(src.first());

    guarantee(!is_receiver, "expecting receiver in register");
    oop_map->set_oop(VMRegImpl::stack2reg(oop_slot_in_callers_frame + frame_size_in_slots));

    __ addi(r_handle, r_caller_sp, reg2offset(src.first()));
    __ ld(  r_temp_2, reg2offset(src.first()), r_caller_sp);
    __ cmpdi(CCR0, r_temp_2, 0);
    __ bne(CCR0, skip);
    // Use a NULL handle if oop is NULL.
    __ li(r_handle, 0);
    __ bind(skip);

    if (dst.first()->is_stack()) {
      // stack to stack
      __ std(r_handle, reg2offset(dst.first()), R1_SP);
    } else {
      // stack to reg
      // Nothing to do, r_handle is already the dst register.
    }
  } else {
    // reg to stack or reg
    const Register r_oop      = src.first()->as_Register();
    const Register r_handle   = dst.first()->is_stack() ? r_temp_1 : dst.first()->as_Register();
    const int oop_slot        = (r_oop->encoding()-R3_ARG1->encoding()) * VMRegImpl::slots_per_word
                                + oop_handle_offset; // in slots
    const int oop_offset = oop_slot * VMRegImpl::stack_slot_size;
    Label skip;

    if (is_receiver) {
      *receiver_offset = oop_offset;
    }
    oop_map->set_oop(VMRegImpl::stack2reg(oop_slot));

    __ std( r_oop,    oop_offset, R1_SP);
    __ addi(r_handle, R1_SP, oop_offset);

    __ cmpdi(CCR0, r_oop, 0);
    __ bne(CCR0, skip);
    // Use a NULL handle if oop is NULL.
    __ li(r_handle, 0);
    __ bind(skip);

    if (dst.first()->is_stack()) {
      // reg to stack
      __ std(r_handle, reg2offset(dst.first()), R1_SP);
    } else {
      // reg to reg
      // Nothing to do, r_handle is already the dst register.
    }
  }
  */
}

static void int_move(MacroAssembler*masm,
                     VMRegPair src, VMRegPair dst,
                     Register r_caller_sp, Register r_temp) {
  ShouldNotReachHere();
  /*
  assert(src.first()->is_valid() && src.second() == src.first()->next(), "incoming must be long-int");
  assert(dst.first()->is_valid() && dst.second() == dst.first()->next(), "outgoing must be long");

  if (src.first()->is_stack()) {
    if (dst.first()->is_stack()) {
      // stack to stack
      __ lwa(r_temp, reg2offset(src.first()), r_caller_sp);
      __ std(r_temp, reg2offset(dst.first()), R1_SP);
    } else {
      // stack to reg
      __ lwa(dst.first()->as_Register(), reg2offset(src.first()), r_caller_sp);
    }
  } else if (dst.first()->is_stack()) {
    // reg to stack
    __ extsw(r_temp, src.first()->as_Register());
    __ std(r_temp, reg2offset(dst.first()), R1_SP);
  } else {
    // reg to reg
    __ extsw(dst.first()->as_Register(), src.first()->as_Register());
  }
  */
}

static void long_move(MacroAssembler*masm,
                      VMRegPair src, VMRegPair dst,
                      Register r_caller_sp, Register r_temp) {
  ShouldNotReachHere();
  /*
  assert(src.first()->is_valid() && src.second() == src.first()->next(), "incoming must be long");
  assert(dst.first()->is_valid() && dst.second() == dst.first()->next(), "outgoing must be long");

  if (src.first()->is_stack()) {
    if (dst.first()->is_stack()) {
      // stack to stack
      __ ld( r_temp, reg2offset(src.first()), r_caller_sp);
      __ std(r_temp, reg2offset(dst.first()), R1_SP);
    } else {
      // stack to reg
      __ ld(dst.first()->as_Register(), reg2offset(src.first()), r_caller_sp);
    }
  } else if (dst.first()->is_stack()) {
    // reg to stack
    __ std(src.first()->as_Register(), reg2offset(dst.first()), R1_SP);
  } else {
    // reg to reg
    if (dst.first()->as_Register() != src.first()->as_Register())
      __ mr(dst.first()->as_Register(), src.first()->as_Register());
  }
  */
}

static void float_move(MacroAssembler*masm,
                       VMRegPair src, VMRegPair dst,
                       Register r_caller_sp, Register r_temp) {
  ShouldNotReachHere();
  /*
  assert(src.first()->is_valid() && !src.second()->is_valid(), "incoming must be float");
  assert(dst.first()->is_valid() && !dst.second()->is_valid(), "outgoing must be float");

  if (src.first()->is_stack()) {
    if (dst.first()->is_stack()) {
      // stack to stack
      __ lwz(r_temp, reg2offset(src.first()), r_caller_sp);
      __ stw(r_temp, reg2offset(dst.first()), R1_SP);
    } else {
      // stack to reg
      __ lfs(dst.first()->as_FloatRegister(), reg2offset(src.first()), r_caller_sp);
    }
  } else if (dst.first()->is_stack()) {
    // reg to stack
    __ stfs(src.first()->as_FloatRegister(), reg2offset(dst.first()), R1_SP);
  } else {
    // reg to reg
    if (dst.first()->as_FloatRegister() != src.first()->as_FloatRegister())
      __ fmr(dst.first()->as_FloatRegister(), src.first()->as_FloatRegister());
  }
  */
}

static void double_move(MacroAssembler*masm,
                        VMRegPair src, VMRegPair dst,
                        Register r_caller_sp, Register r_temp) {
  ShouldNotReachHere();
  /*
  assert(src.first()->is_valid() && src.second() == src.first()->next(), "incoming must be double");
  assert(dst.first()->is_valid() && dst.second() == dst.first()->next(), "outgoing must be double");

  if (src.first()->is_stack()) {
    if (dst.first()->is_stack()) {
      // stack to stack
      __ ld( r_temp, reg2offset(src.first()), r_caller_sp);
      __ std(r_temp, reg2offset(dst.first()), R1_SP);
    } else {
      // stack to reg
      __ lfd(dst.first()->as_FloatRegister(), reg2offset(src.first()), r_caller_sp);
    }
  } else if (dst.first()->is_stack()) {
    // reg to stack
    __ stfd(src.first()->as_FloatRegister(), reg2offset(dst.first()), R1_SP);
  } else {
    // reg to reg
    if (dst.first()->as_FloatRegister() != src.first()->as_FloatRegister())
      __ fmr(dst.first()->as_FloatRegister(), src.first()->as_FloatRegister());
  }
  */
}

void SharedRuntime::save_native_result(MacroAssembler *masm, BasicType ret_type, int frame_slots) {
  ShouldNotReachHere();
  /*
  switch (ret_type) {
    case T_BOOLEAN:
    case T_CHAR:
    case T_BYTE:
    case T_SHORT:
    case T_INT:
      __ stw (R3_RET,  frame_slots*VMRegImpl::stack_slot_size, R1_SP);
      break;
    case T_ARRAY:
    case T_OBJECT:
    case T_LONG:
      __ std (R3_RET,  frame_slots*VMRegImpl::stack_slot_size, R1_SP);
      break;
    case T_FLOAT:
      __ stfs(F1_RET, frame_slots*VMRegImpl::stack_slot_size, R1_SP);
      break;
    case T_DOUBLE:
      __ stfd(F1_RET, frame_slots*VMRegImpl::stack_slot_size, R1_SP);
      break;
    case T_VOID:
      break;
    default:
      ShouldNotReachHere();
      break;
  }
  */
}

void SharedRuntime::restore_native_result(MacroAssembler *masm, BasicType ret_type, int frame_slots) {
  ShouldNotReachHere();
  /*
  switch (ret_type) {
    case T_BOOLEAN:
    case T_CHAR:
    case T_BYTE:
    case T_SHORT:
    case T_INT:
      __ lwz(R3_RET,  frame_slots*VMRegImpl::stack_slot_size, R1_SP);
      break;
    case T_ARRAY:
    case T_OBJECT:
    case T_LONG:
      __ ld (R3_RET,  frame_slots*VMRegImpl::stack_slot_size, R1_SP);
      break;
    case T_FLOAT:
      __ lfs(F1_RET, frame_slots*VMRegImpl::stack_slot_size, R1_SP);
      break;
    case T_DOUBLE:
      __ lfd(F1_RET, frame_slots*VMRegImpl::stack_slot_size, R1_SP);
      break;
    case T_VOID:
      break;
    default:
      ShouldNotReachHere();
      break;
  }
  */
}

static void save_or_restore_arguments(MacroAssembler* masm,
                                      const int stack_slots,
                                      const int total_in_args,
                                      const int arg_save_area,
                                      OopMap* map,
                                      VMRegPair* in_regs,
                                      BasicType* in_sig_bt) {
  ShouldNotReachHere();
  /*
  // If map is non-NULL then the code should store the values,
  // otherwise it should load them.
  int slot = arg_save_area;
  // Save down double word first.
  for (int i = 0; i < total_in_args; i++) {
    if (in_regs[i].first()->is_FloatRegister() && in_sig_bt[i] == T_DOUBLE) {
      int offset = slot * VMRegImpl::stack_slot_size;
      slot += VMRegImpl::slots_per_word;
      assert(slot <= stack_slots, "overflow (after DOUBLE stack slot)");
      if (map != NULL) {
        __ stfd(in_regs[i].first()->as_FloatRegister(), offset, R1_SP);
      } else {
        __ lfd(in_regs[i].first()->as_FloatRegister(), offset, R1_SP);
      }
    } else if (in_regs[i].first()->is_Register() &&
        (in_sig_bt[i] == T_LONG || in_sig_bt[i] == T_ARRAY)) {
      int offset = slot * VMRegImpl::stack_slot_size;
      if (map != NULL) {
        __ std(in_regs[i].first()->as_Register(), offset, R1_SP);
        if (in_sig_bt[i] == T_ARRAY) {
          map->set_oop(VMRegImpl::stack2reg(slot));
        }
      } else {
        __ ld(in_regs[i].first()->as_Register(), offset, R1_SP);
      }
      slot += VMRegImpl::slots_per_word;
      assert(slot <= stack_slots, "overflow (after LONG/ARRAY stack slot)");
    }
  }
  // Save or restore single word registers.
  for (int i = 0; i < total_in_args; i++) {
    // RISCV64: pass ints as longs: must only deal with floats here.
    if (in_regs[i].first()->is_FloatRegister()) {
      if (in_sig_bt[i] == T_FLOAT) {
        int offset = slot * VMRegImpl::stack_slot_size;
        slot++;
        assert(slot <= stack_slots, "overflow (after FLOAT stack slot)");
        if (map != NULL) {
          __ stfs(in_regs[i].first()->as_FloatRegister(), offset, R1_SP);
        } else {
          __ lfs(in_regs[i].first()->as_FloatRegister(), offset, R1_SP);
        }
      }
    } else if (in_regs[i].first()->is_stack()) {
      if (in_sig_bt[i] == T_ARRAY && map != NULL) {
        int offset_in_older_frame = in_regs[i].first()->reg2stack() + SharedRuntime::out_preserve_stack_slots();
        map->set_oop(VMRegImpl::stack2reg(offset_in_older_frame + stack_slots));
      }
    }
  }
  */
}

// Check GC_locker::needs_gc and enter the runtime if it's true. This
// keeps a new JNI critical region from starting until a GC has been
// forced. Save down any oops in registers and describe them in an
// OopMap.
static void check_needs_gc_for_critical_native(MacroAssembler* masm,
                                               const int stack_slots,
                                               const int total_in_args,
                                               const int arg_save_area,
                                               OopMapSet* oop_maps,
                                               VMRegPair* in_regs,
                                               BasicType* in_sig_bt,
                                               Register tmp_reg ) {
  ShouldNotReachHere();
  /*
  __ block_comment("check GC_locker::needs_gc");
  Label cont;
  __ lbz(tmp_reg, (RegisterOrConstant)(intptr_t)GC_locker::needs_gc_address());
  __ cmplwi(CCR0, tmp_reg, 0);
  __ beq(CCR0, cont);

  // Save down any values that are live in registers and call into the
  // runtime to halt for a GC.
  OopMap* map = new OopMap(stack_slots * 2, 0 */ /* arg_slots*/ /*);
  save_or_restore_arguments(masm, stack_slots, total_in_args,
                            arg_save_area, map, in_regs, in_sig_bt);

  __ mr(R3_ARG1, R16_thread);
  __ set_last_Java_frame(R1_SP, noreg);

  __ block_comment("block_for_jni_critical");
  address entry_point = CAST_FROM_FN_PTR(address, SharedRuntime::block_for_jni_critical);
#if defined(ABI_ELFv2)
  __ call_c(entry_point, relocInfo::runtime_call_type);
#else
  __ call_c(CAST_FROM_FN_PTR(FunctionDescriptor*, entry_point), relocInfo::runtime_call_type);
#endif
  address start           = __ pc() - __ offset(),
          calls_return_pc = __ last_calls_return_pc();
  oop_maps->add_gc_map(calls_return_pc - start, map);

  __ reset_last_Java_frame();

  // Reload all the register arguments.
  save_or_restore_arguments(masm, stack_slots, total_in_args,
                            arg_save_area, NULL, in_regs, in_sig_bt);

  __ BIND(cont);

#ifdef ASSERT
  if (StressCriticalJNINatives) {
    // Stress register saving.
    OopMap* map = new OopMap(stack_slots * 2, 0 */ /* arg_slots*/ /*);
    save_or_restore_arguments(masm, stack_slots, total_in_args,
                              arg_save_area, map, in_regs, in_sig_bt);
    // Destroy argument registers.
    for (int i = 0; i < total_in_args; i++) {
      if (in_regs[i].first()->is_Register()) {
        const Register reg = in_regs[i].first()->as_Register();
        __ neg(reg, reg);
      } else if (in_regs[i].first()->is_FloatRegister()) {
        __ fneg(in_regs[i].first()->as_FloatRegister(), in_regs[i].first()->as_FloatRegister());
      }
    }

    save_or_restore_arguments(masm, stack_slots, total_in_args,
                              arg_save_area, NULL, in_regs, in_sig_bt);
  }
#endif
  */
}

static void move_ptr(MacroAssembler* masm, VMRegPair src, VMRegPair dst, Register r_caller_sp, Register r_temp) {
  ShouldNotReachHere();
  /*
  if (src.first()->is_stack()) {
    if (dst.first()->is_stack()) {
      // stack to stack
      __ ld(r_temp, reg2offset(src.first()), r_caller_sp);
      __ std(r_temp, reg2offset(dst.first()), R1_SP);
    } else {
      // stack to reg
      __ ld(dst.first()->as_Register(), reg2offset(src.first()), r_caller_sp);
    }
  } else if (dst.first()->is_stack()) {
    // reg to stack
    __ std(src.first()->as_Register(), reg2offset(dst.first()), R1_SP);
  } else {
    if (dst.first() != src.first()) {
      __ mr(dst.first()->as_Register(), src.first()->as_Register());
    }
  }
  */
}

// Unpack an array argument into a pointer to the body and the length
// if the array is non-null, otherwise pass 0 for both.
static void unpack_array_argument(MacroAssembler* masm, VMRegPair reg, BasicType in_elem_type,
                                  VMRegPair body_arg, VMRegPair length_arg, Register r_caller_sp,
                                  Register tmp_reg, Register tmp2_reg) {
  ShouldNotReachHere();
  /*
  assert(!body_arg.first()->is_Register() || body_arg.first()->as_Register() != tmp_reg,
         "possible collision");
  assert(!length_arg.first()->is_Register() || length_arg.first()->as_Register() != tmp_reg,
         "possible collision");

  // Pass the length, ptr pair.
  Label set_out_args;
  VMRegPair tmp, tmp2;
  tmp.set_ptr(tmp_reg->as_VMReg());
  tmp2.set_ptr(tmp2_reg->as_VMReg());
  if (reg.first()->is_stack()) {
    // Load the arg up from the stack.
    move_ptr(masm, reg, tmp, r_caller_sp, */ /*unused*/ /* R0);
    reg = tmp;
  }
  __ li(tmp2_reg, 0); // Pass zeros if Array=null.
  if (tmp_reg != reg.first()->as_Register()) __ li(tmp_reg, 0);
  __ cmpdi(CCR0, reg.first()->as_Register(), 0);
  __ beq(CCR0, set_out_args);
  __ lwa(tmp2_reg, arrayOopDesc::length_offset_in_bytes(), reg.first()->as_Register());
  __ addi(tmp_reg, reg.first()->as_Register(), arrayOopDesc::base_offset_in_bytes(in_elem_type));
  __ bind(set_out_args);
  move_ptr(masm, tmp, body_arg, r_caller_sp, */ /*unused*/ /* R0);
  move_ptr(masm, tmp2, length_arg, r_caller_sp, */ /*unused*/ /* R0); // Same as move32_64 on RISCV64.
  */
}

static void verify_oop_args(MacroAssembler* masm,
                            methodHandle method,
                            const BasicType* sig_bt,
                            const VMRegPair* regs) {
  ShouldNotReachHere();
  /*
  Register temp_reg = R19_method;  // not part of any compiled calling seq
  if (VerifyOops) {
    for (int i = 0; i < method->size_of_parameters(); i++) {
      if (sig_bt[i] == T_OBJECT ||
          sig_bt[i] == T_ARRAY) {
        VMReg r = regs[i].first();
        assert(r->is_valid(), "bad oop arg");
        if (r->is_stack()) {
          __ ld(temp_reg, reg2offset(r), R1_SP);
          __ verify_oop(temp_reg);
        } else {
          __ verify_oop(r->as_Register());
        }
      }
    }
  }
  */
}

static void gen_special_dispatch(MacroAssembler* masm,
                                 methodHandle method,
                                 const BasicType* sig_bt,
                                 const VMRegPair* regs) {
  ShouldNotReachHere();
  /*
  verify_oop_args(masm, method, sig_bt, regs);
  vmIntrinsics::ID iid = method->intrinsic_id();

  // Now write the args into the outgoing interpreter space
  bool     has_receiver   = false;
  Register receiver_reg   = noreg;
  int      member_arg_pos = -1;
  Register member_reg     = noreg;
  int      ref_kind       = MethodHandles::signature_polymorphic_intrinsic_ref_kind(iid);
  if (ref_kind != 0) {
    member_arg_pos = method->size_of_parameters() - 1;  // trailing MemberName argument
    member_reg = R19_method;  // known to be free at this point
    has_receiver = MethodHandles::ref_kind_has_receiver(ref_kind);
  } else if (iid == vmIntrinsics::_invokeBasic) {
    has_receiver = true;
  } else {
    fatal(err_msg_res("unexpected intrinsic id %d", iid));
  }

  if (member_reg != noreg) {
    // Load the member_arg into register, if necessary.
    SharedRuntime::check_member_name_argument_is_last_argument(method, sig_bt, regs);
    VMReg r = regs[member_arg_pos].first();
    if (r->is_stack()) {
      __ ld(member_reg, reg2offset(r), R1_SP);
    } else {
      // no data motion is needed
      member_reg = r->as_Register();
    }
  }

  if (has_receiver) {
    // Make sure the receiver is loaded into a register.
    assert(method->size_of_parameters() > 0, "oob");
    assert(sig_bt[0] == T_OBJECT, "receiver argument must be an object");
    VMReg r = regs[0].first();
    assert(r->is_valid(), "bad receiver arg");
    if (r->is_stack()) {
      // Porting note:  This assumes that compiled calling conventions always
      // pass the receiver oop in a register.  If this is not true on some
      // platform, pick a temp and load the receiver from stack.
      fatal("receiver always in a register");
      receiver_reg = R11_scratch1;  // TODO (hs24): is R11_scratch1 really free at this point?
      __ ld(receiver_reg, reg2offset(r), R1_SP);
    } else {
      // no data motion is needed
      receiver_reg = r->as_Register();
    }
  }

  // Figure out which address we are really jumping to:
  MethodHandles::generate_method_handle_dispatch(masm, iid,
                                                 receiver_reg, member_reg, */ /*for_compiler_entry:*/ /* true);
  */
}

#endif // COMPILER2

// ---------------------------------------------------------------------------
// Generate a native wrapper for a given method. The method takes arguments
// in the Java compiled code convention, marshals them to the native
// convention (handlizes oops, etc), transitions to native, makes the call,
// returns to java state (possibly blocking), unhandlizes any result and
// returns.
//
// Critical native functions are a shorthand for the use of
// GetPrimtiveArrayCritical and disallow the use of any other JNI
// functions.  The wrapper is expected to unpack the arguments before
// passing them to the callee and perform checks before and after the
// native call to ensure that they GC_locker
// lock_critical/unlock_critical semantics are followed.  Some other
// parts of JNI setup are skipped like the tear down of the JNI handle
// block and the check for pending exceptions it's impossible for them
// to be thrown.
//
// They are roughly structured like this:
//   if (GC_locker::needs_gc())
//     SharedRuntime::block_for_jni_critical();
//   tranistion to thread_in_native
//   unpack arrray arguments and call native entry point
//   check for safepoint in progress
//   check if any thread suspend flags are set
//     call into JVM and possible unlock the JNI critical
//     if a GC was suppressed while in the critical native.
//   transition back to thread_in_Java
//   return to caller
//
nmethod *SharedRuntime::generate_native_wrapper(MacroAssembler *masm,
                                                methodHandle method,
                                                int compile_id,
                                                BasicType *in_sig_bt,
                                                VMRegPair *in_regs,
                                                BasicType ret_type) {
  ShouldNotReachHere();
  return NULL;
  /*
#ifdef COMPILER2
  if (method->is_method_handle_intrinsic()) {
    vmIntrinsics::ID iid = method->intrinsic_id();
    intptr_t start = (intptr_t)__ pc();
    int vep_offset = ((intptr_t)__ pc()) - start;
    gen_special_dispatch(masm,
                         method,
                         in_sig_bt,
                         in_regs);
    int frame_complete = ((intptr_t)__ pc()) - start;  // not complete, period
    __ flush();
    int stack_slots = SharedRuntime::out_preserve_stack_slots();  // no out slots at all, actually
    return nmethod::new_native_nmethod(method,
                                       compile_id,
                                       masm->code(),
                                       vep_offset,
                                       frame_complete,
                                       stack_slots / VMRegImpl::slots_per_word,
                                       in_ByteSize(-1),
                                       in_ByteSize(-1),
                                       (OopMapSet*)NULL);
  }

  bool is_critical_native = true;
  address native_func = method->critical_native_function();
  if (native_func == NULL) {
    native_func = method->native_function();
    is_critical_native = false;
  }
  assert(native_func != NULL, "must have function");

  // First, create signature for outgoing C call
  // --------------------------------------------------------------------------

  int total_in_args = method->size_of_parameters();
  // We have received a description of where all the java args are located
  // on entry to the wrapper. We need to convert these args to where
  // the jni function will expect them. To figure out where they go
  // we convert the java signature to a C signature by inserting
  // the hidden arguments as arg[0] and possibly arg[1] (static method)
  //
  // Additionally, on ppc64 we must convert integers to longs in the C
  // signature. We do this in advance in order to have no trouble with
  // indexes into the bt-arrays.
  // So convert the signature and registers now, and adjust the total number
  // of in-arguments accordingly.
  int i2l_argcnt = convert_ints_to_longints_argcnt(total_in_args, in_sig_bt); // RISCV64: pass ints as longs.

  // Calculate the total number of C arguments and create arrays for the
  // signature and the outgoing registers.
  // On ppc64, we have two arrays for the outgoing registers, because
  // some floating-point arguments must be passed in registers _and_
  // in stack locations.
  bool method_is_static = method->is_static();
  int  total_c_args     = i2l_argcnt;

  if (!is_critical_native) {
    int n_hidden_args = method_is_static ? 2 : 1;
    total_c_args += n_hidden_args;
  } else {
    // No JNIEnv*, no this*, but unpacked arrays (base+length).
    for (int i = 0; i < total_in_args; i++) {
      if (in_sig_bt[i] == T_ARRAY) {
        total_c_args += 2; // RISCV64: T_LONG, T_INT, T_ADDRESS (see convert_ints_to_longints and c_calling_convention)
      }
    }
  }

  BasicType *out_sig_bt = NEW_RESOURCE_ARRAY(BasicType, total_c_args);
  VMRegPair *out_regs   = NEW_RESOURCE_ARRAY(VMRegPair, total_c_args);
  VMRegPair *out_regs2  = NEW_RESOURCE_ARRAY(VMRegPair, total_c_args);
  BasicType* in_elem_bt = NULL;

  // Create the signature for the C call:
  //   1) add the JNIEnv*
  //   2) add the class if the method is static
  //   3) copy the rest of the incoming signature (shifted by the number of
  //      hidden arguments).

  int argc = 0;
  if (!is_critical_native) {
    convert_ints_to_longints(i2l_argcnt, total_in_args, in_sig_bt, in_regs); // RISCV64: pass ints as longs.

    out_sig_bt[argc++] = T_ADDRESS;
    if (method->is_static()) {
      out_sig_bt[argc++] = T_OBJECT;
    }

    for (int i = 0; i < total_in_args ; i++ ) {
      out_sig_bt[argc++] = in_sig_bt[i];
    }
  } else {
    Thread* THREAD = Thread::current();
    in_elem_bt = NEW_RESOURCE_ARRAY(BasicType, i2l_argcnt);
    SignatureStream ss(method->signature());
    int o = 0;
    for (int i = 0; i < total_in_args ; i++, o++) {
      if (in_sig_bt[i] == T_ARRAY) {
        // Arrays are passed as int, elem* pair
        Symbol* atype = ss.as_symbol(CHECK_NULL);
        const char* at = atype->as_C_string();
        if (strlen(at) == 2) {
          assert(at[0] == '[', "must be");
          switch (at[1]) {
            case 'B': in_elem_bt[o] = T_BYTE; break;
            case 'C': in_elem_bt[o] = T_CHAR; break;
            case 'D': in_elem_bt[o] = T_DOUBLE; break;
            case 'F': in_elem_bt[o] = T_FLOAT; break;
            case 'I': in_elem_bt[o] = T_INT; break;
            case 'J': in_elem_bt[o] = T_LONG; break;
            case 'S': in_elem_bt[o] = T_SHORT; break;
            case 'Z': in_elem_bt[o] = T_BOOLEAN; break;
            default: ShouldNotReachHere();
          }
        }
      } else {
        in_elem_bt[o] = T_VOID;
        switch(in_sig_bt[i]) { // RISCV64: pass ints as longs.
          case T_BOOLEAN:
          case T_CHAR:
          case T_BYTE:
          case T_SHORT:
          case T_INT: in_elem_bt[++o] = T_VOID; break;
          default: break;
        }
      }
      if (in_sig_bt[i] != T_VOID) {
        assert(in_sig_bt[i] == ss.type(), "must match");
        ss.next();
      }
    }
    assert(i2l_argcnt==o, "must match");

    convert_ints_to_longints(i2l_argcnt, total_in_args, in_sig_bt, in_regs); // RISCV64: pass ints as longs.

    for (int i = 0; i < total_in_args ; i++ ) {
      if (in_sig_bt[i] == T_ARRAY) {
        // Arrays are passed as int, elem* pair.
        out_sig_bt[argc++] = T_LONG; // RISCV64: pass ints as longs.
        out_sig_bt[argc++] = T_INT;
        out_sig_bt[argc++] = T_ADDRESS;
      } else {
        out_sig_bt[argc++] = in_sig_bt[i];
      }
    }
  }


  // Compute the wrapper's frame size.
  // --------------------------------------------------------------------------

  // Now figure out where the args must be stored and how much stack space
  // they require.
  //
  // Compute framesize for the wrapper. We need to handlize all oops in
  // incoming registers.
  //
  // Calculate the total number of stack slots we will need:
  //   1) abi requirements
  //   2) outgoing arguments
  //   3) space for inbound oop handle area
  //   4) space for handlizing a klass if static method
  //   5) space for a lock if synchronized method
  //   6) workspace for saving return values, int <-> float reg moves, etc.
  //   7) alignment
  //
  // Layout of the native wrapper frame:
  // (stack grows upwards, memory grows downwards)
  //
  // NW     [ABI_REG_ARGS]             <-- 1) R1_SP
  //        [outgoing arguments]       <-- 2) R1_SP + out_arg_slot_offset
  //        [oopHandle area]           <-- 3) R1_SP + oop_handle_offset (save area for critical natives)
  //        klass                      <-- 4) R1_SP + klass_offset
  //        lock                       <-- 5) R1_SP + lock_offset
  //        [workspace]                <-- 6) R1_SP + workspace_offset
  //        [alignment] (optional)     <-- 7)
  // caller [JIT_TOP_ABI_48]           <-- r_callers_sp
  //
  // - *_slot_offset Indicates offset from SP in number of stack slots.
  // - *_offset      Indicates offset from SP in bytes.

  int stack_slots = c_calling_convention(out_sig_bt, out_regs, out_regs2, total_c_args) // 1+2)
                  + SharedRuntime::out_preserve_stack_slots(); // See c_calling_convention.

  // Now the space for the inbound oop handle area.
  int total_save_slots = num_java_iarg_registers * VMRegImpl::slots_per_word;
  if (is_critical_native) {
    // Critical natives may have to call out so they need a save area
    // for register arguments.
    int double_slots = 0;
    int single_slots = 0;
    for (int i = 0; i < total_in_args; i++) {
      if (in_regs[i].first()->is_Register()) {
        const Register reg = in_regs[i].first()->as_Register();
        switch (in_sig_bt[i]) {
          case T_BOOLEAN:
          case T_BYTE:
          case T_SHORT:
          case T_CHAR:
          case T_INT:  */ /*single_slots++;*/ /* break; // RISCV64: pass ints as longs.
          case T_ARRAY:
          case T_LONG: double_slots++; break;
          default:  ShouldNotReachHere();
        }
      } else if (in_regs[i].first()->is_FloatRegister()) {
        switch (in_sig_bt[i]) {
          case T_FLOAT:  single_slots++; break;
          case T_DOUBLE: double_slots++; break;
          default:  ShouldNotReachHere();
        }
      }
    }
    total_save_slots = double_slots * 2 + round_to(single_slots, 2); // round to even
  }

  int oop_handle_slot_offset = stack_slots;
  stack_slots += total_save_slots;                                                // 3)

  int klass_slot_offset = 0;
  int klass_offset      = -1;
  if (method_is_static && !is_critical_native) {                                  // 4)
    klass_slot_offset  = stack_slots;
    klass_offset       = klass_slot_offset * VMRegImpl::stack_slot_size;
    stack_slots       += VMRegImpl::slots_per_word;
  }

  int lock_slot_offset = 0;
  int lock_offset      = -1;
  if (method->is_synchronized()) {                                                // 5)
    lock_slot_offset   = stack_slots;
    lock_offset        = lock_slot_offset * VMRegImpl::stack_slot_size;
    stack_slots       += VMRegImpl::slots_per_word;
  }

  int workspace_slot_offset = stack_slots;                                        // 6)
  stack_slots         += 2;

  // Now compute actual number of stack words we need.
  // Rounding to make stack properly aligned.
  stack_slots = round_to(stack_slots,                                             // 7)
                         frame::alignment_in_bytes / VMRegImpl::stack_slot_size);
  int frame_size_in_bytes = stack_slots * VMRegImpl::stack_slot_size;


  // Now we can start generating code.
  // --------------------------------------------------------------------------

  intptr_t start_pc = (intptr_t)__ pc();
  intptr_t vep_start_pc;
  intptr_t frame_done_pc;
  intptr_t oopmap_pc;

  Label    ic_miss;
  Label    handle_pending_exception;

  Register r_callers_sp = R21;
  Register r_temp_1     = R22;
  Register r_temp_2     = R23;
  Register r_temp_3     = R24;
  Register r_temp_4     = R25;
  Register r_temp_5     = R26;
  Register r_temp_6     = R27;
  Register r_return_pc  = R28;

  Register r_carg1_jnienv        = noreg;
  Register r_carg2_classorobject = noreg;
  if (!is_critical_native) {
    r_carg1_jnienv        = out_regs[0].first()->as_Register();
    r_carg2_classorobject = out_regs[1].first()->as_Register();
  }


  // Generate the Unverified Entry Point (UEP).
  // --------------------------------------------------------------------------
  assert(start_pc == (intptr_t)__ pc(), "uep must be at start");

  // Check ic: object class == cached class?
  if (!method_is_static) {
  Register ic = as_Register(Matcher::inline_cache_reg_encode());
  Register receiver_klass = r_temp_1;

  __ cmpdi(CCR0, R3_ARG1, 0);
  __ beq(CCR0, ic_miss);
  __ verify_oop(R3_ARG1);
  __ load_klass(receiver_klass, R3_ARG1);

  __ cmpd(CCR0, receiver_klass, ic);
  __ bne(CCR0, ic_miss);
  }


  // Generate the Verified Entry Point (VEP).
  // --------------------------------------------------------------------------
  vep_start_pc = (intptr_t)__ pc();

  __ save_LR_CR(r_temp_1);
  __ generate_stack_overflow_check(frame_size_in_bytes); // Check before creating frame.
  __ mr(r_callers_sp, R1_SP);                       // Remember frame pointer.
  __ push_frame(frame_size_in_bytes, r_temp_1);          // Push the c2n adapter's frame.
  frame_done_pc = (intptr_t)__ pc();

  // Native nmethod wrappers never take possesion of the oop arguments.
  // So the caller will gc the arguments.
  // The only thing we need an oopMap for is if the call is static.
  //
  // An OopMap for lock (and class if static), and one for the VM call itself.
  OopMapSet *oop_maps = new OopMapSet();
  OopMap    *oop_map  = new OopMap(stack_slots * 2, 0 */ /* arg_slots*/ /*);

  if (is_critical_native) {
    check_needs_gc_for_critical_native(masm, stack_slots, total_in_args, oop_handle_slot_offset, oop_maps, in_regs, in_sig_bt, r_temp_1);
  }

  // Move arguments from register/stack to register/stack.
  // --------------------------------------------------------------------------
  //
  // We immediately shuffle the arguments so that for any vm call we have
  // to make from here on out (sync slow path, jvmti, etc.) we will have
  // captured the oops from our caller and have a valid oopMap for them.
  //
  // Natives require 1 or 2 extra arguments over the normal ones: the JNIEnv*
  // (derived from JavaThread* which is in R16_thread) and, if static,
  // the class mirror instead of a receiver. This pretty much guarantees that
  // register layout will not match. We ignore these extra arguments during
  // the shuffle. The shuffle is described by the two calling convention
  // vectors we have in our possession. We simply walk the java vector to
  // get the source locations and the c vector to get the destinations.

  // Record sp-based slot for receiver on stack for non-static methods.
  int receiver_offset = -1;

  // We move the arguments backward because the floating point registers
  // destination will always be to a register with a greater or equal
  // register number or the stack.
  //   in  is the index of the incoming Java arguments
  //   out is the index of the outgoing C arguments

#ifdef ASSERT
  bool reg_destroyed[RegisterImpl::number_of_registers];
  bool freg_destroyed[FloatRegisterImpl::number_of_registers];
  for (int r = 0 ; r < RegisterImpl::number_of_registers ; r++) {
    reg_destroyed[r] = false;
  }
  for (int f = 0 ; f < FloatRegisterImpl::number_of_registers ; f++) {
    freg_destroyed[f] = false;
  }
#endif // ASSERT

  for (int in = total_in_args - 1, out = total_c_args - 1; in >= 0 ; in--, out--) {

#ifdef ASSERT
    if (in_regs[in].first()->is_Register()) {
      assert(!reg_destroyed[in_regs[in].first()->as_Register()->encoding()], "ack!");
    } else if (in_regs[in].first()->is_FloatRegister()) {
      assert(!freg_destroyed[in_regs[in].first()->as_FloatRegister()->encoding()], "ack!");
    }
    if (out_regs[out].first()->is_Register()) {
      reg_destroyed[out_regs[out].first()->as_Register()->encoding()] = true;
    } else if (out_regs[out].first()->is_FloatRegister()) {
      freg_destroyed[out_regs[out].first()->as_FloatRegister()->encoding()] = true;
    }
    if (out_regs2[out].first()->is_Register()) {
      reg_destroyed[out_regs2[out].first()->as_Register()->encoding()] = true;
    } else if (out_regs2[out].first()->is_FloatRegister()) {
      freg_destroyed[out_regs2[out].first()->as_FloatRegister()->encoding()] = true;
    }
#endif // ASSERT

    switch (in_sig_bt[in]) {
      case T_BOOLEAN:
      case T_CHAR:
      case T_BYTE:
      case T_SHORT:
      case T_INT:
        guarantee(in > 0 && in_sig_bt[in-1] == T_LONG,
                  "expecting type (T_LONG,bt) for bt in {T_BOOLEAN, T_CHAR, T_BYTE, T_SHORT, T_INT}");
        break;
      case T_LONG:
        if (in_sig_bt[in+1] == T_VOID) {
          long_move(masm, in_regs[in], out_regs[out], r_callers_sp, r_temp_1);
        } else {
          guarantee(in_sig_bt[in+1] == T_BOOLEAN || in_sig_bt[in+1] == T_CHAR  ||
                    in_sig_bt[in+1] == T_BYTE    || in_sig_bt[in+1] == T_SHORT ||
                    in_sig_bt[in+1] == T_INT,
                 "expecting type (T_LONG,bt) for bt in {T_BOOLEAN, T_CHAR, T_BYTE, T_SHORT, T_INT}");
          int_move(masm, in_regs[in], out_regs[out], r_callers_sp, r_temp_1);
        }
        break;
      case T_ARRAY:
        if (is_critical_native) {
          int body_arg = out;
          out -= 2; // Point to length arg. RISCV64: pass ints as longs.
          unpack_array_argument(masm, in_regs[in], in_elem_bt[in], out_regs[body_arg], out_regs[out],
                                r_callers_sp, r_temp_1, r_temp_2);
          break;
        }
      case T_OBJECT:
        assert(!is_critical_native, "no oop arguments");
        object_move(masm, stack_slots,
                    oop_map, oop_handle_slot_offset,
                    ((in == 0) && (!method_is_static)), &receiver_offset,
                    in_regs[in], out_regs[out],
                    r_callers_sp, r_temp_1, r_temp_2);
        break;
      case T_VOID:
        break;
      case T_FLOAT:
        float_move(masm, in_regs[in], out_regs[out], r_callers_sp, r_temp_1);
        if (out_regs2[out].first()->is_valid()) {
          float_move(masm, in_regs[in], out_regs2[out], r_callers_sp, r_temp_1);
        }
        break;
      case T_DOUBLE:
        double_move(masm, in_regs[in], out_regs[out], r_callers_sp, r_temp_1);
        if (out_regs2[out].first()->is_valid()) {
          double_move(masm, in_regs[in], out_regs2[out], r_callers_sp, r_temp_1);
        }
        break;
      case T_ADDRESS:
        fatal("found type (T_ADDRESS) in java args");
        break;
      default:
        ShouldNotReachHere();
        break;
    }
  }

  // Pre-load a static method's oop into ARG2.
  // Used both by locking code and the normal JNI call code.
  if (method_is_static && !is_critical_native) {
    __ set_oop_constant(JNIHandles::make_local(method->method_holder()->java_mirror()),
                        r_carg2_classorobject);

    // Now handlize the static class mirror in carg2. It's known not-null.
    __ std(r_carg2_classorobject, klass_offset, R1_SP);
    oop_map->set_oop(VMRegImpl::stack2reg(klass_slot_offset));
    __ addi(r_carg2_classorobject, R1_SP, klass_offset);
  }

  // Get JNIEnv* which is first argument to native.
  if (!is_critical_native) {
    __ addi(r_carg1_jnienv, R16_thread, in_bytes(JavaThread::jni_environment_offset()));
  }

  // NOTE:
  //
  // We have all of the arguments setup at this point.
  // We MUST NOT touch any outgoing regs from this point on.
  // So if we must call out we must push a new frame.

  // Get current pc for oopmap, and load it patchable relative to global toc.
  oopmap_pc = (intptr_t) __ pc();
  __ calculate_address_from_global_toc(r_return_pc, (address)oopmap_pc, true, true, true, true);

  // We use the same pc/oopMap repeatedly when we call out.
  oop_maps->add_gc_map(oopmap_pc - start_pc, oop_map);

  // r_return_pc now has the pc loaded that we will use when we finally call
  // to native.

  // Make sure that thread is non-volatile; it crosses a bunch of VM calls below.
  assert(R16_thread->is_nonvolatile(), "thread must be in non-volatile register");


# if 0
  // DTrace method entry
# endif

  // Lock a synchronized method.
  // --------------------------------------------------------------------------

  if (method->is_synchronized()) {
    assert(!is_critical_native, "unhandled");
    ConditionRegister r_flag = CCR1;
    Register          r_oop  = r_temp_4;
    const Register    r_box  = r_temp_5;
    Label             done, locked;

    // Load the oop for the object or class. r_carg2_classorobject contains
    // either the handlized oop from the incoming arguments or the handlized
    // class mirror (if the method is static).
    __ ld(r_oop, 0, r_carg2_classorobject);

    // Get the lock box slot's address.
    __ addi(r_box, R1_SP, lock_offset);

#   ifdef ASSERT
    if (UseBiasedLocking) {
      // Making the box point to itself will make it clear it went unused
      // but also be obviously invalid.
      __ std(r_box, 0, r_box);
    }
#   endif // ASSERT

    // Try fastpath for locking.
    // fast_lock kills r_temp_1, r_temp_2, r_temp_3.
    __ compiler_fast_lock_object(r_flag, r_oop, r_box, r_temp_1, r_temp_2, r_temp_3);
    __ beq(r_flag, locked);

    // None of the above fast optimizations worked so we have to get into the
    // slow case of monitor enter. Inline a special case of call_VM that
    // disallows any pending_exception.

    // Save argument registers and leave room for C-compatible ABI_REG_ARGS.
    int frame_size = frame::abi_reg_args_size +
                     round_to(total_c_args * wordSize, frame::alignment_in_bytes);
    __ mr(R11_scratch1, R1_SP);
    RegisterSaver::push_frame_and_save_argument_registers(masm, R12_scratch2, frame_size, total_c_args, out_regs, out_regs2);

    // Do the call.
    __ set_last_Java_frame(R11_scratch1, r_return_pc);
    assert(r_return_pc->is_nonvolatile(), "expecting return pc to be in non-volatile register");
    __ call_VM_leaf(CAST_FROM_FN_PTR(address, SharedRuntime::complete_monitor_locking_C), r_oop, r_box, R16_thread);
    __ reset_last_Java_frame();

    RegisterSaver::restore_argument_registers_and_pop_frame(masm, frame_size, total_c_args, out_regs, out_regs2);

    __ asm_assert_mem8_is_zero(thread_(pending_exception),
       "no pending exception allowed on exit from SharedRuntime::complete_monitor_locking_C", 0);

    __ bind(locked);
  }


  // Publish thread state
  // --------------------------------------------------------------------------

  // Use that pc we placed in r_return_pc a while back as the current frame anchor.
  __ set_last_Java_frame(R1_SP, r_return_pc);

  // Transition from _thread_in_Java to _thread_in_native.
  __ li(R0, _thread_in_native);
  __ release();
  // TODO: RISCV port assert(4 == JavaThread::sz_thread_state(), "unexpected field size");
  __ stw(R0, thread_(thread_state));
  if (UseMembar) {
    __ fence();
  }


  // The JNI call
  // --------------------------------------------------------------------------
#if defined(ABI_ELFv2)
  __ call_c(native_func, relocInfo::runtime_call_type);
#else
  FunctionDescriptor* fd_native_method = (FunctionDescriptor*) native_func;
  __ call_c(fd_native_method, relocInfo::runtime_call_type);
#endif


  // Now, we are back from the native code.


  // Unpack the native result.
  // --------------------------------------------------------------------------

  // For int-types, we do any needed sign-extension required.
  // Care must be taken that the return values (R3_RET and F1_RET)
  // will survive any VM calls for blocking or unlocking.
  // An OOP result (handle) is done specially in the slow-path code.

  switch (ret_type) {
    case T_VOID:    break;        // Nothing to do!
    case T_FLOAT:   break;        // Got it where we want it (unless slow-path).
    case T_DOUBLE:  break;        // Got it where we want it (unless slow-path).
    case T_LONG:    break;        // Got it where we want it (unless slow-path).
    case T_OBJECT:  break;        // Really a handle.
                                  // Cannot de-handlize until after reclaiming jvm_lock.
    case T_ARRAY:   break;

    case T_BOOLEAN: {             // 0 -> false(0); !0 -> true(1)
      Label skip_modify;
      __ cmpwi(CCR0, R3_RET, 0);
      __ beq(CCR0, skip_modify);
      __ li(R3_RET, 1);
      __ bind(skip_modify);
      break;
      }
    case T_BYTE: {                // sign extension
      __ extsb(R3_RET, R3_RET);
      break;
      }
    case T_CHAR: {                // unsigned result
      __ andi(R3_RET, R3_RET, 0xffff);
      break;
      }
    case T_SHORT: {               // sign extension
      __ extsh(R3_RET, R3_RET);
      break;
      }
    case T_INT:                   // nothing to do
      break;
    default:
      ShouldNotReachHere();
      break;
  }


  // Publish thread state
  // --------------------------------------------------------------------------

  // Switch thread to "native transition" state before reading the
  // synchronization state. This additional state is necessary because reading
  // and testing the synchronization state is not atomic w.r.t. GC, as this
  // scenario demonstrates:
  //   - Java thread A, in _thread_in_native state, loads _not_synchronized
  //     and is preempted.
  //   - VM thread changes sync state to synchronizing and suspends threads
  //     for GC.
  //   - Thread A is resumed to finish this native method, but doesn't block
  //     here since it didn't see any synchronization in progress, and escapes.

  // Transition from _thread_in_native to _thread_in_native_trans.
  __ li(R0, _thread_in_native_trans);
  __ release();
  // TODO: RISCV port assert(4 == JavaThread::sz_thread_state(), "unexpected field size");
  __ stw(R0, thread_(thread_state));


  // Must we block?
  // --------------------------------------------------------------------------

  // Block, if necessary, before resuming in _thread_in_Java state.
  // In order for GC to work, don't clear the last_Java_sp until after blocking.
  Label after_transition;
  {
    Label no_block, sync;

    if (os::is_MP()) {
      if (UseMembar) {
        // Force this write out before the read below.
        __ fence();
      } else {
        // Write serialization page so VM thread can do a pseudo remote membar.
        // We use the current thread pointer to calculate a thread specific
        // offset to write to within the page. This minimizes bus traffic
        // due to cache line collision.
        __ serialize_memory(R16_thread, r_temp_4, r_temp_5);
      }
    }

    Register sync_state_addr = r_temp_4;
    Register sync_state      = r_temp_5;
    Register suspend_flags   = r_temp_6;

    __ load_const(sync_state_addr, SafepointSynchronize::address_of_state(), */ /*temp*/ /* sync_state);

    // TODO: RISCV port assert(4 == SafepointSynchronize::sz_state(), "unexpected field size");
    __ lwz(sync_state, 0, sync_state_addr);

    // TODO: RISCV port assert(4 == Thread::sz_suspend_flags(), "unexpected field size");
    __ lwz(suspend_flags, thread_(suspend_flags));

    __ acquire();

    Label do_safepoint;
    // No synchronization in progress nor yet synchronized.
    __ cmpwi(CCR0, sync_state, SafepointSynchronize::_not_synchronized);
    // Not suspended.
    __ cmpwi(CCR1, suspend_flags, 0);

    __ bne(CCR0, sync);
    __ beq(CCR1, no_block);

    // Block. Save any potential method result value before the operation and
    // use a leaf call to leave the last_Java_frame setup undisturbed. Doing this
    // lets us share the oopMap we used when we went native rather than create
    // a distinct one for this pc.
    __ bind(sync);

    address entry_point = is_critical_native
      ? CAST_FROM_FN_PTR(address, JavaThread::check_special_condition_for_native_trans_and_transition)
      : CAST_FROM_FN_PTR(address, JavaThread::check_special_condition_for_native_trans);
    save_native_result(masm, ret_type, workspace_slot_offset);
    __ call_VM_leaf(entry_point, R16_thread);
    restore_native_result(masm, ret_type, workspace_slot_offset);

    if (is_critical_native) {
      __ b(after_transition); // No thread state transition here.
    }
    __ bind(no_block);
  }

  // Publish thread state.
  // --------------------------------------------------------------------------

  // Thread state is thread_in_native_trans. Any safepoint blocking has
  // already happened so we can now change state to _thread_in_Java.

  // Transition from _thread_in_native_trans to _thread_in_Java.
  __ li(R0, _thread_in_Java);
  __ release();
  // TODO: RISCV port assert(4 == JavaThread::sz_thread_state(), "unexpected field size");
  __ stw(R0, thread_(thread_state));
  if (UseMembar) {
    __ fence();
  }
  __ bind(after_transition);

  // Reguard any pages if necessary.
  // --------------------------------------------------------------------------

  Label no_reguard;
  __ lwz(r_temp_1, thread_(stack_guard_state));
  __ cmpwi(CCR0, r_temp_1, JavaThread::stack_guard_yellow_disabled);
  __ bne(CCR0, no_reguard);

  save_native_result(masm, ret_type, workspace_slot_offset);
  __ call_VM_leaf(CAST_FROM_FN_PTR(address, SharedRuntime::reguard_yellow_pages));
  restore_native_result(masm, ret_type, workspace_slot_offset);

  __ bind(no_reguard);


  // Unlock
  // --------------------------------------------------------------------------

  if (method->is_synchronized()) {

    ConditionRegister r_flag   = CCR1;
    const Register r_oop       = r_temp_4;
    const Register r_box       = r_temp_5;
    const Register r_exception = r_temp_6;
    Label done;

    // Get oop and address of lock object box.
    if (method_is_static) {
      assert(klass_offset != -1, "");
      __ ld(r_oop, klass_offset, R1_SP);
    } else {
      assert(receiver_offset != -1, "");
      __ ld(r_oop, receiver_offset, R1_SP);
    }
    __ addi(r_box, R1_SP, lock_offset);

    // Try fastpath for unlocking.
    __ compiler_fast_unlock_object(r_flag, r_oop, r_box, r_temp_1, r_temp_2, r_temp_3);
    __ beq(r_flag, done);

    // Save and restore any potential method result value around the unlocking operation.
    save_native_result(masm, ret_type, workspace_slot_offset);

    // Must save pending exception around the slow-path VM call. Since it's a
    // leaf call, the pending exception (if any) can be kept in a register.
    __ ld(r_exception, thread_(pending_exception));
    assert(r_exception->is_nonvolatile(), "exception register must be non-volatile");
    __ li(R0, 0);
    __ std(R0, thread_(pending_exception));

    // Slow case of monitor enter.
    // Inline a special case of call_VM that disallows any pending_exception.
    __ call_VM_leaf(CAST_FROM_FN_PTR(address, SharedRuntime::complete_monitor_unlocking_C), r_oop, r_box);

    __ asm_assert_mem8_is_zero(thread_(pending_exception),
       "no pending exception allowed on exit from SharedRuntime::complete_monitor_unlocking_C", 0);

    restore_native_result(masm, ret_type, workspace_slot_offset);

    // Check_forward_pending_exception jump to forward_exception if any pending
    // exception is set. The forward_exception routine expects to see the
    // exception in pending_exception and not in a register. Kind of clumsy,
    // since all folks who branch to forward_exception must have tested
    // pending_exception first and hence have it in a register already.
    __ std(r_exception, thread_(pending_exception));

    __ bind(done);
  }

# if 0
  // DTrace method exit
# endif

  // Clear "last Java frame" SP and PC.
  // --------------------------------------------------------------------------

  __ reset_last_Java_frame();

  // Unpack oop result.
  // --------------------------------------------------------------------------

  if (ret_type == T_OBJECT || ret_type == T_ARRAY) {
    Label skip_unboxing;
    __ cmpdi(CCR0, R3_RET, 0);
    __ beq(CCR0, skip_unboxing);
    __ ld(R3_RET, 0, R3_RET);
    __ bind(skip_unboxing);
    __ verify_oop(R3_RET);
  }


  // Reset handle block.
  // --------------------------------------------------------------------------
  if (!is_critical_native) {
  __ ld(r_temp_1, thread_(active_handles));
  // TODO: RISCV port assert(4 == JNIHandleBlock::top_size_in_bytes(), "unexpected field size");
  __ li(r_temp_2, 0);
  __ stw(r_temp_2, JNIHandleBlock::top_offset_in_bytes(), r_temp_1);


  // Check for pending exceptions.
  // --------------------------------------------------------------------------
  __ ld(r_temp_2, thread_(pending_exception));
  __ cmpdi(CCR0, r_temp_2, 0);
  __ bne(CCR0, handle_pending_exception);
  }

  // Return
  // --------------------------------------------------------------------------

  __ pop_frame();
  __ restore_LR_CR(R11);
  __ blr();


  // Handler for pending exceptions (out-of-line).
  // --------------------------------------------------------------------------

  // Since this is a native call, we know the proper exception handler
  // is the empty function. We just pop this frame and then jump to
  // forward_exception_entry.
  if (!is_critical_native) {
  __ align(InteriorEntryAlignment);
  __ bind(handle_pending_exception);

  __ pop_frame();
  __ restore_LR_CR(R11);
  __ b64_patchable((address)StubRoutines::forward_exception_entry(),
                       relocInfo::runtime_call_type);
  }

  // Handler for a cache miss (out-of-line).
  // --------------------------------------------------------------------------

  if (!method_is_static) {
  __ align(InteriorEntryAlignment);
  __ bind(ic_miss);

  __ b64_patchable((address)SharedRuntime::get_ic_miss_stub(),
                       relocInfo::runtime_call_type);
  }

  // Done.
  // --------------------------------------------------------------------------

  __ flush();

  nmethod *nm = nmethod::new_native_nmethod(method,
                                            compile_id,
                                            masm->code(),
                                            vep_start_pc-start_pc,
                                            frame_done_pc-start_pc,
                                            stack_slots / VMRegImpl::slots_per_word,
                                            (method_is_static ? in_ByteSize(klass_offset) : in_ByteSize(receiver_offset)),
                                            in_ByteSize(lock_offset),
                                            oop_maps);

  if (is_critical_native) {
    nm->set_lazy_critical_native(true);
  }

  return nm;
#else
  ShouldNotReachHere();
  return NULL;
#endif // COMPILER2
  */
}

// This function returns the adjust size (in number of words) to a c2i adapter
// activation for use during deoptimization.
int Deoptimization::last_frame_adjust(int callee_parameters, int callee_locals) {
  return round_to((callee_locals - callee_parameters) * Interpreter::stackElementWords, frame::alignment_in_bytes);
}

uint SharedRuntime::out_preserve_stack_slots() {
#ifdef COMPILER2
  return frame::jit_out_preserve_size / VMRegImpl::stack_slot_size;
#else
  return 0;
#endif
}

#ifdef COMPILER2
// Frame generation for deopt and uncommon trap blobs.
static void push_skeleton_frame(MacroAssembler* masm, bool deopt,
                                /* Read */
                                Register unroll_block_reg,
                                /* Update */
                                Register frame_sizes_reg,
                                Register number_of_frames_reg,
                                Register pcs_reg,
                                /* Invalidate */
                                Register frame_size_reg,
                                Register pc_reg) {
  ShouldNotReachHere();
  /*

  __ ld(pc_reg, 0, pcs_reg);
  __ ld(frame_size_reg, 0, frame_sizes_reg);
  __ std(pc_reg, _abi(lr), R1_SP);
  __ push_frame(frame_size_reg, R0*/ /*tmp*/ /*);
#ifdef CC_INTERP
  __ std(R1_SP, _parent_ijava_frame_abi(initial_caller_sp), R1_SP);
#else
#ifdef ASSERT
  __ load_const_optimized(pc_reg, 0x5afe);
  __ std(pc_reg, _ijava_state_neg(ijava_reserved), R1_SP);
#endif
  __ std(R1_SP, _ijava_state_neg(sender_sp), R1_SP);
#endif // CC_INTERP
  __ addi(number_of_frames_reg, number_of_frames_reg, -1);
  __ addi(frame_sizes_reg, frame_sizes_reg, wordSize);
  __ addi(pcs_reg, pcs_reg, wordSize);
  */
}

// Loop through the UnrollBlock info and create new frames.
static void push_skeleton_frames(MacroAssembler* masm, bool deopt,
                                 /* read */
                                 Register unroll_block_reg,
                                 /* invalidate */
                                 Register frame_sizes_reg,
                                 Register number_of_frames_reg,
                                 Register pcs_reg,
                                 Register frame_size_reg,
                                 Register pc_reg) {
  ShouldNotReachHere();
  /*
  Label loop;

 // _number_of_frames is of type int (deoptimization.hpp)
  __ lwa(number_of_frames_reg,
             Deoptimization::UnrollBlock::number_of_frames_offset_in_bytes(),
             unroll_block_reg);
  __ ld(pcs_reg,
            Deoptimization::UnrollBlock::frame_pcs_offset_in_bytes(),
            unroll_block_reg);
  __ ld(frame_sizes_reg,
            Deoptimization::UnrollBlock::frame_sizes_offset_in_bytes(),
            unroll_block_reg);

  // stack: (caller_of_deoptee, ...).

  // At this point we either have an interpreter frame or a compiled
  // frame on top of stack. If it is a compiled frame we push a new c2i
  // adapter here

  // Memorize top-frame stack-pointer.
  __ mr(frame_size_reg*/ /*old_sp*/ /*, R1_SP);

  // Resize interpreter top frame OR C2I adapter.

  // At this moment, the top frame (which is the caller of the deoptee) is
  // an interpreter frame or a newly pushed C2I adapter or an entry frame.
  // The top frame has a TOP_IJAVA_FRAME_ABI and the frame contains the
  // outgoing arguments.
  //
  // In order to push the interpreter frame for the deoptee, we need to
  // resize the top frame such that we are able to place the deoptee's
  // locals in the frame.
  // Additionally, we have to turn the top frame's TOP_IJAVA_FRAME_ABI
  // into a valid PARENT_IJAVA_FRAME_ABI.

  __ lwa(R11_scratch1,
             Deoptimization::UnrollBlock::caller_adjustment_offset_in_bytes(),
             unroll_block_reg);
  __ neg(R11_scratch1, R11_scratch1);

  // R11_scratch1 contains size of locals for frame resizing.
  // R12_scratch2 contains top frame's lr.

  // Resize frame by complete frame size prevents TOC from being
  // overwritten by locals. A more stack space saving way would be
  // to copy the TOC to its location in the new abi.
  __ addi(R11_scratch1, R11_scratch1, - frame::parent_ijava_frame_abi_size);

  // now, resize the frame
  __ resize_frame(R11_scratch1, pc_reg*/ /*tmp*/ /*);

  // In the case where we have resized a c2i frame above, the optional
  // alignment below the locals has size 32 (why?).
  __ std(R12_scratch2, _abi(lr), R1_SP);

  // Initialize initial_caller_sp.
#ifdef CC_INTERP
  __ std(frame_size_reg*/ /*old_sp*/ /*, _parent_ijava_frame_abi(initial_caller_sp), R1_SP);
#else
#ifdef ASSERT
 __ load_const_optimized(pc_reg, 0x5afe);
 __ std(pc_reg, _ijava_state_neg(ijava_reserved), R1_SP);
#endif
 __ std(frame_size_reg, _ijava_state_neg(sender_sp), R1_SP);
#endif // CC_INTERP

#ifdef ASSERT
  // Make sure that there is at least one entry in the array.
  __ cmpdi(CCR0, number_of_frames_reg, 0);
  __ asm_assert_ne("array_size must be > 0", 0x205);
#endif

  // Now push the new interpreter frames.
  //
  __ bind(loop);
  // Allocate a new frame, fill in the pc.
  push_skeleton_frame(masm, deopt,
                      unroll_block_reg,
                      frame_sizes_reg,
                      number_of_frames_reg,
                      pcs_reg,
                      frame_size_reg,
                      pc_reg);
  __ cmpdi(CCR0, number_of_frames_reg, 0);
  __ bne(CCR0, loop);

  // Get the return address pointing into the frame manager.
  __ ld(R0, 0, pcs_reg);
  // Store it in the top interpreter frame.
  __ std(R0, _abi(lr), R1_SP);
  // Initialize frame_manager_lr of interpreter top frame.
#ifdef CC_INTERP
  __ std(R0, _top_ijava_frame_abi(frame_manager_lr), R1_SP);
#endif
  */
}
#endif

void SharedRuntime::generate_deopt_blob() {
  // Allocate space for the code
  ResourceMark rm;
  // Setup code generation tools
  CodeBuffer buffer("deopt_blob", 2048, 1024);
  InterpreterMacroAssembler* masm = new InterpreterMacroAssembler(&buffer);
  Label exec_mode_initialized;
  int frame_size_in_words;
  OopMap* map = NULL;
  OopMapSet *oop_maps = new OopMapSet();

  // TODO: Fix this sometime wayyyy in the future
  // size of ABI112 plus spill slots for R3_RET and F1_RET.
  // const int frame_size_in_bytes = frame::abi_reg_args_spill_size;
  // const int frame_size_in_slots = frame_size_in_bytes / sizeof(jint);
  int first_frame_size_in_bytes = 0; // frame size of "unpack frame" for call to fetch_unroll_info.

  const Register exec_mode_reg = X24_tmp1;

  const address start = __ pc();

#ifdef COMPILER2
  /*
  // --------------------------------------------------------------------------
  // Prolog for non exception case!

  // We have been called from the deopt handler of the deoptee.
  //
  // deoptee:
  //                      ...
  //                      call X
  //                      ...
  //  deopt_handler:      call_deopt_stub
  //  cur. return pc  --> ...
  //
  // So currently SR_LR points behind the call in the deopt handler.
  // We adjust it such that it points to the start of the deopt handler.
  // The return_pc has been stored in the frame of the deoptee and
  // will replace the address of the deopt_handler in the call
  // to Deoptimization::fetch_unroll_info below.
  // We can't grab a free register here, because all registers may
  // contain live values, so let the RegisterSaver do the adjustment
  // of the return pc.
  const int return_pc_adjustment_no_exception = -HandlerImpl::size_deopt_handler();

  // Push the "unpack frame"
  // Save everything in sight.
  map = RegisterSaver::push_frame_reg_args_and_save_live_registers(masm,
                                                                   &first_frame_size_in_bytes,
                                                                   */ /*generate_oop_map=*/ /* true,
                                                                   return_pc_adjustment_no_exception,
                                                                   RegisterSaver::return_pc_is_lr);
  assert(map != NULL, "OopMap must have been created");

  __ li(exec_mode_reg, Deoptimization::Unpack_deopt);
  // Save exec mode for unpack_frames.
  __ b(exec_mode_initialized);

  // --------------------------------------------------------------------------
  // Prolog for exception case

  // An exception is pending.
  // We have been called with a return (interpreter) or a jump (exception blob).
  //
  // - R3_ARG1: exception oop
  // - R4_ARG2: exception pc

  int exception_offset = __ pc() - start;

  BLOCK_COMMENT("Prolog for exception case");

  // The RegisterSaves doesn't need to adjust the return pc for this situation.
  const int return_pc_adjustment_exception = 0;

  // Push the "unpack frame".
  // Save everything in sight.
  assert(R4 == R4_ARG2, "exception pc must be in r4");
  RegisterSaver::push_frame_reg_args_and_save_live_registers(masm,
                                                             &first_frame_size_in_bytes,
                                                             */ /*generate_oop_map=*/ /* false,
                                                             return_pc_adjustment_exception,
                                                             RegisterSaver::return_pc_is_r4);

  // Deopt during an exception. Save exec mode for unpack_frames.
  __ li(exec_mode_reg, Deoptimization::Unpack_exception);

  // Store exception oop and pc in thread (location known to GC).
  // This is needed since the call to "fetch_unroll_info()" may safepoint.
  __ std(R3_ARG1, in_bytes(JavaThread::exception_oop_offset()), R16_thread);
  __ std(R4_ARG2, in_bytes(JavaThread::exception_pc_offset()),  R16_thread);

  // fall through

  // --------------------------------------------------------------------------
  __ BIND(exec_mode_initialized);

  {
  const Register unroll_block_reg = R22_tmp2;

  // We need to set `last_Java_frame' because `fetch_unroll_info' will
  // call `last_Java_frame()'. The value of the pc in the frame is not
  // particularly important. It just needs to identify this blob.
  __ set_last_Java_frame(R1_SP, noreg);

  // With EscapeAnalysis turned on, this call may safepoint!
  __ call_VM_leaf(CAST_FROM_FN_PTR(address, Deoptimization::fetch_unroll_info), R16_thread);
  address calls_return_pc = __ last_calls_return_pc();
  // Set an oopmap for the call site that describes all our saved registers.
  oop_maps->add_gc_map(calls_return_pc - start, map);

  __ reset_last_Java_frame();
  // Save the return value.
  __ mr(unroll_block_reg, R3_RET);

  // Restore only the result registers that have been saved
  // by save_volatile_registers(...).
  RegisterSaver::restore_result_registers(masm, first_frame_size_in_bytes);

  // In excp_deopt_mode, restore and clear exception oop which we
  // stored in the thread during exception entry above. The exception
  // oop will be the return value of this stub.
  Label skip_restore_excp;
  __ cmpdi(CCR0, exec_mode_reg, Deoptimization::Unpack_exception);
  __ bne(CCR0, skip_restore_excp);
  __ ld(R3_RET, in_bytes(JavaThread::exception_oop_offset()), R16_thread);
  __ ld(R4_ARG2, in_bytes(JavaThread::exception_pc_offset()), R16_thread);
  __ li(R0, 0);
  __ std(R0, in_bytes(JavaThread::exception_pc_offset()),  R16_thread);
  __ std(R0, in_bytes(JavaThread::exception_oop_offset()), R16_thread);
  __ BIND(skip_restore_excp);

  // reload narrro_oop_base
  if (UseCompressedOops && Universe::narrow_oop_base() != 0) {
    __ load_const_optimized(R30, Universe::narrow_oop_base());
  }

  __ pop_frame();

  // stack: (deoptee, optional i2c, caller of deoptee, ...).

  // pop the deoptee's frame
  __ pop_frame();

  // stack: (caller_of_deoptee, ...).

  // Loop through the `UnrollBlock' info and create interpreter frames.
  push_skeleton_frames(masm, true*/ /*deopt*/ /*,
                       unroll_block_reg,
                       R23_tmp3,
                       R24_tmp4,
                       R25_tmp5,
                       R26_tmp6,
                       R27_tmp7);

  // stack: (skeletal interpreter frame, ..., optional skeletal
  // interpreter frame, optional c2i, caller of deoptee, ...).
  }

  // push an `unpack_frame' taking care of float / int return values.
  __ push_frame(frame_size_in_bytes, R0*/ /*tmp*/ /*);

  // stack: (unpack frame, skeletal interpreter frame, ..., optional
  // skeletal interpreter frame, optional c2i, caller of deoptee,
  // ...).

  // Spill live volatile registers since we'll do a call.
  __ std( R3_RET, _abi_reg_args_spill(spill_ret),  R1_SP);
  __ stfd(F1_RET, _abi_reg_args_spill(spill_fret), R1_SP);

  // Let the unpacker layout information in the skeletal frames just
  // allocated.
  __ get_PC_trash_LR(R3_RET);
  __ set_last_Java_frame(*/ /*sp*/ /*R1_SP, */ /*pc*/ /*R3_RET);
  // This is a call to a LEAF method, so no oop map is required.
  __ call_VM_leaf(CAST_FROM_FN_PTR(address, Deoptimization::unpack_frames),
                  R16_thread*/ /*thread*/ /*, exec_mode_reg*/ /*exec_mode*/ /*);
  __ reset_last_Java_frame();

  // Restore the volatiles saved above.
  __ ld( R3_RET, _abi_reg_args_spill(spill_ret),  R1_SP);
  __ lfd(F1_RET, _abi_reg_args_spill(spill_fret), R1_SP);

  // Pop the unpack frame.
  __ pop_frame();
  __ restore_LR_CR(R0);

  // stack: (top interpreter frame, ..., optional interpreter frame,
  // optional c2i, caller of deoptee, ...).

  // Initialize R14_state.
#ifdef CC_INTERP
  __ ld(R14_state, 0, R1_SP);
  __ addi(R14_state, R14_state, -frame::interpreter_frame_cinterpreterstate_size_in_bytes());
  // Also inititialize R15_prev_state.
  __ restore_prev_state();
#else
  __ restore_interpreter_state(R11_scratch1);
  __ load_const_optimized(R25_templateTableBase, (address)Interpreter::dispatch_table((TosState)0), R11_scratch1);
#endif // CC_INTERP


  // Return to the interpreter entry point.
  __ blr();
  __ flush();
  */
#else // COMPILER2
  __ unimplemented("deopt blob needed only with compiler");
//  int exception_offset = __ pc() - start;
#endif // COMPILER2
  Unimplemented();
  int exception_offset = __ pc() - start;
  _deopt_blob = DeoptimizationBlob::create(&buffer, oop_maps, 0, exception_offset, 0, first_frame_size_in_bytes / wordSize);
}

#ifdef COMPILER2
void SharedRuntime::generate_uncommon_trap_blob() {
  ShouldNotReachHere();
  /*
  // Allocate space for the code.
  ResourceMark rm;
  // Setup code generation tools.
  CodeBuffer buffer("uncommon_trap_blob", 2048, 1024);
  InterpreterMacroAssembler* masm = new InterpreterMacroAssembler(&buffer);
  address start = __ pc();

  Register unroll_block_reg = R21_tmp1;
  Register klass_index_reg  = R22_tmp2;
  Register unc_trap_reg     = R23_tmp3;

  OopMapSet* oop_maps = new OopMapSet();
  int frame_size_in_bytes = frame::abi_reg_args_size;
  OopMap* map = new OopMap(frame_size_in_bytes / sizeof(jint), 0);

  // stack: (deoptee, optional i2c, caller_of_deoptee, ...).

  // Push a dummy `unpack_frame' and call
  // `Deoptimization::uncommon_trap' to pack the compiled frame into a
  // vframe array and return the `UnrollBlock' information.

  // Save LR to compiled frame.
  __ save_LR_CR(R11_scratch1);

  // Push an "uncommon_trap" frame.
  __ push_frame_reg_args(0, R11_scratch1);

  // stack: (unpack frame, deoptee, optional i2c, caller_of_deoptee, ...).

  // Set the `unpack_frame' as last_Java_frame.
  // `Deoptimization::uncommon_trap' expects it and considers its
  // sender frame as the deoptee frame.
  // Remember the offset of the instruction whose address will be
  // moved to R11_scratch1.
  address gc_map_pc = __ get_PC_trash_LR(R11_scratch1);

  __ set_last_Java_frame(*/ /*sp*/ /*R1_SP, */ /*pc*/ /*R11_scratch1);

  __ mr(klass_index_reg, R3);
  __ call_VM_leaf(CAST_FROM_FN_PTR(address, Deoptimization::uncommon_trap),
                  R16_thread, klass_index_reg);

  // Set an oopmap for the call site.
  oop_maps->add_gc_map(gc_map_pc - start, map);

  __ reset_last_Java_frame();

  // Pop the `unpack frame'.
  __ pop_frame();

  // stack: (deoptee, optional i2c, caller_of_deoptee, ...).

  // Save the return value.
  __ mr(unroll_block_reg, R3_RET);

  // Pop the uncommon_trap frame.
  __ pop_frame();

  // stack: (caller_of_deoptee, ...).

  // Allocate new interpreter frame(s) and possibly a c2i adapter
  // frame.
  push_skeleton_frames(masm, false*/ /*deopt*/ /*,
                       unroll_block_reg,
                       R22_tmp2,
                       R23_tmp3,
                       R24_tmp4,
                       R25_tmp5,
                       R26_tmp6);

  // stack: (skeletal interpreter frame, ..., optional skeletal
  // interpreter frame, optional c2i, caller of deoptee, ...).

  // Push a dummy `unpack_frame' taking care of float return values.
  // Call `Deoptimization::unpack_frames' to layout information in the
  // interpreter frames just created.

  // Push a simple "unpack frame" here.
  __ push_frame_reg_args(0, R11_scratch1);

  // stack: (unpack frame, skeletal interpreter frame, ..., optional
  // skeletal interpreter frame, optional c2i, caller of deoptee,
  // ...).

  // Set the "unpack_frame" as last_Java_frame.
  __ get_PC_trash_LR(R11_scratch1);
  __ set_last_Java_frame(*/ /*sp*/ /*R1_SP, */ /*pc*/ /*R11_scratch1);

  // Indicate it is the uncommon trap case.
  __ li(unc_trap_reg, Deoptimization::Unpack_uncommon_trap);
  // Let the unpacker layout information in the skeletal frames just
  // allocated.
  __ call_VM_leaf(CAST_FROM_FN_PTR(address, Deoptimization::unpack_frames),
                  R16_thread, unc_trap_reg);

  __ reset_last_Java_frame();
  // Pop the `unpack frame'.
  __ pop_frame();
  // Restore LR from top interpreter frame.
  __ restore_LR_CR(R11_scratch1);

  // stack: (top interpreter frame, ..., optional interpreter frame,
  // optional c2i, caller of deoptee, ...).

#ifdef CC_INTERP
  // Initialize R14_state, ...
  __ ld(R11_scratch1, 0, R1_SP);
  __ addi(R14_state, R11_scratch1, -frame::interpreter_frame_cinterpreterstate_size_in_bytes());
  // also initialize R15_prev_state.
  __ restore_prev_state();
#else
  __ restore_interpreter_state(R11_scratch1);
  __ load_const_optimized(R25_templateTableBase, (address)Interpreter::dispatch_table((TosState)0), R11_scratch1);
#endif // CC_INTERP

  // Return to the interpreter entry point.
  __ blr();

  masm->flush();

  _uncommon_trap_blob = UncommonTrapBlob::create(&buffer, oop_maps, frame_size_in_bytes/wordSize);
  */
}
#endif // COMPILER2

// Generate a special Compile2Runtime blob that saves all registers, and setup oopmap.
SafepointBlob* SharedRuntime::generate_handler_blob(address call_ptr, int poll_type) {
  assert(StubRoutines::forward_exception_entry() != NULL,
         "must be generated before");

  ResourceMark rm;
  OopMapSet *oop_maps = new OopMapSet();
  OopMap* map;

  // Allocate space for the code. Setup code generation tools.
  CodeBuffer buffer("handler_blob", 2048, 1024);
  MacroAssembler* masm = new MacroAssembler(&buffer);

  address start = __ pc();
  int frame_size_in_bytes = 0;

  // Save registers, fpu state, and flags.
  map = RegisterSaver::push_frame_and_save_live_registers(masm,
                                                          &frame_size_in_bytes,
                                                          /*generate_oop_map=*/ true,
                                                          /*return_pc_adjustment=*/ 0);

  // The following is basically a call_VM. However, we need the precise
  // address of the call in order to generate an oopmap. Hence, we do all the
  // work outselves.
  __ set_last_Java_frame(/*sp=*/ X2_SP, /*fp=*/ X8_FP, /*pc=*/ noreg);

  // The return address must always be correct so that the frame constructor
  // never sees an invalid pc.

  // Do the call
  __ call_VM_leaf(call_ptr, X20_thread);
  address calls_return_pc = __ last_calls_return_pc();

  // Set an oopmap for the call site. This oopmap will map all
  // oop-registers and debug-info registers as callee-saved. This
  // will allow deoptimization at this safepoint to find all possible
  // debug-info recordings, as well as let GC find all oops.
  oop_maps->add_gc_map(calls_return_pc - start, map);

  Label noException;

  // Clear the last Java frame.
  __ reset_last_Java_frame();

  BLOCK_COMMENT("  Check pending exception.");
  __ ld(XZERO, thread_(pending_exception));
  __ beq(X5_T0, XZERO, noException);

  // Exception pending
  RegisterSaver::restore_live_registers_and_pop_frame(masm, frame_size_in_bytes, true);

  BLOCK_COMMENT("  Jump to forward_exception_entry.");
  // Jump to forward_exception_entry, with the issuing PC in LR
  // so it looks like the original nmethod called forward_exception_entry.
  __ jump64_patchable(StubRoutines::forward_exception_entry(), relocInfo::runtime_call_type, false);

  // No exception case.
  __ BIND(noException);

  // Normal exit, restore registers and exit.
  RegisterSaver::restore_live_registers_and_pop_frame(masm, frame_size_in_bytes, true);
  __ jr(X1_RA);

  // Make sure all code is generated
  masm->flush();

  // Fill-out other meta info
  // CodeBlob frame size is in words.
  return SafepointBlob::create(&buffer, oop_maps, frame_size_in_bytes / wordSize);
}

// generate_resolve_blob - call resolution (static/virtual/opt-virtual/ic-miss)
//
// Generate a stub that calls into the vm to find out the proper destination
// of a java call. All the argument registers are live at this point
// but since this is generic code we don't know what they are and the caller
// must do any gc of the args.
//
RuntimeStub* SharedRuntime::generate_resolve_blob(address destination, const char* name) {
  // allocate space for the code
  ResourceMark rm;

  CodeBuffer buffer(name, 1000, 512);
  MacroAssembler* masm = new MacroAssembler(&buffer);

  int frame_size_in_bytes;

  OopMapSet *oop_maps = new OopMapSet();
  OopMap* map = NULL;

  address start = __ pc();

  map = RegisterSaver::push_frame_and_save_live_registers(masm,
                                                          &frame_size_in_bytes,
                                                          /*generate_oop_map*/ true,
                                                          /*return_pc_adjustment*/ 0);

  // Use noreg as last_Java_pc, the return pc will be reconstructed
  // from the physical frame.
  __ set_last_Java_frame(/*sp=*/ X2_SP, /*fp=*/ X8_FP, noreg);

  int frame_complete = __ offset();

  // Pass R19_method as 2nd (optional) argument, used by
  // counter_overflow_stub.
  __ call_VM_leaf(destination, X20_thread, X23_method);
  address calls_return_pc = __ last_calls_return_pc();
  // Set an oopmap for the call site.
  // We need this not only for callee-saved registers, but also for volatile
  // registers that the compiler might be keeping live across a safepoint.
  // Create the oopmap for the call's return pc.
  oop_maps->add_gc_map(calls_return_pc - start, map);

  // X10_RET0 contains the address we are going to jump to assuming no exception got installed.

  // clear last_Java_sp
  __ reset_last_Java_frame();

  // Check for pending exceptions.
  BLOCK_COMMENT("Check for pending exceptions.");
  Label pending;
  __ ld(X6_T1, thread_(pending_exception));
  __ bne(X6_T1, XZERO, pending);

  __ mv(X31, X10_RET0); // We won't restore the scratch register, assume "short-lived"

  RegisterSaver::restore_live_registers_and_pop_frame(masm, frame_size_in_bytes, false);

  // Get the returned method.
  __ get_vm_result_2(X23_method);
  __ jr(X31);

  // Pending exception after the safepoint.
  __ BIND(pending);

  RegisterSaver::restore_live_registers_and_pop_frame(masm, frame_size_in_bytes, true);

  // exception pending => remove activation and forward to exception handler

  __ ld(X10_RET0, thread_(pending_exception));
  __ sd(XZERO, in_bytes(JavaThread::vm_result_offset()), X20_thread);
  __ jump64_patchable(StubRoutines::forward_exception_entry(), relocInfo::runtime_call_type, false);

  // -------------
  // Make sure all code is generated.
  masm->flush();

  // return the blob
  // frame_size_words or bytes??
  return RuntimeStub::new_runtime_stub(name, &buffer, frame_complete, frame_size_in_bytes/wordSize,
                                       oop_maps, true);
}

// Is vector's size (in bytes) bigger than a size saved by default?
// 16 bytes XMM registers are saved by default using fxsave/fxrstor instructions.
bool SharedRuntime::is_wide_vector(int size) {
  return size > 16;
}

