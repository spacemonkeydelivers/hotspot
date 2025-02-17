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
#include "asm/assembler.inline.hpp"
#include "asm/macroAssembler.inline.hpp"
#include "compiler/disassembler.hpp"
#include "memory/resourceArea.hpp"
#include "runtime/java.hpp"
#include "runtime/stubCodeGenerator.hpp"
#include "utilities/defaultStream.hpp"
#include "vm_version_riscv.hpp"
#ifdef TARGET_OS_FAMILY_aix
# include "os_aix.inline.hpp"
#endif
#ifdef TARGET_OS_FAMILY_linux
# include "os_linux.inline.hpp"
#endif

# include <sys/sysinfo.h>

int VM_Version::_features = VM_Version::unknown_m;
int VM_Version::_measured_cache_line_size = 128; // default value
const char* VM_Version::_features_str = "";
bool VM_Version::_is_determine_features_test_running = false;


#define MSG(flag)   \
  if (flag && !FLAG_IS_DEFAULT(flag))                                  \
      jio_fprintf(defaultStream::error_stream(),                       \
                  "warning: -XX:+" #flag " requires -XX:+UseSIGTRAP\n" \
                  "         -XX:+" #flag " will be disabled!\n");

void VM_Version::initialize() {
  // Test which instructions are supported and measure cache line size.
  determine_features();

  if (!UseSIGTRAP) {
    MSG(TrapBasedICMissChecks);
    MSG(TrapBasedNotEntrantChecks);
    MSG(TrapBasedNullChecks);
    FLAG_SET_ERGO(bool, TrapBasedNotEntrantChecks, false);
    FLAG_SET_ERGO(bool, TrapBasedNullChecks,       false);
    FLAG_SET_ERGO(bool, TrapBasedICMissChecks,     false);
  }

#ifdef COMPILER2
  if (!UseSIGTRAP) {
    MSG(TrapBasedRangeChecks);
    FLAG_SET_ERGO(bool, TrapBasedRangeChecks, false);
  }

  // On Power6 test for section size.
  if (PowerArchitectureRISCV64 == 6) {
    determine_section_size();
  // TODO: RISCV port } else {
  // TODO: RISCV port PdScheduling::power6SectorSize = 0x20;
  }

  MaxVectorSize = 8;
#endif

  // RISCV64 supports 8-byte compare-exchange operations (see
  // Atomic::cmpxchg and StubGenerator::generate_atomic_cmpxchg_ptr)
  // and 'atomic long memory ops' (see Unsafe_GetLongVolatile).
  _supports_cx8 = false;

  UseSSE = 0; // Only on x86 and x64

  intx cache_line_size = _measured_cache_line_size;

  if (FLAG_IS_DEFAULT(AllocatePrefetchStyle)) AllocatePrefetchStyle = 1;

  assert(AllocatePrefetchStyle >= 0, "AllocatePrefetchStyle should be positive");

  FLAG_SET_DEFAULT(UseCRC32Intrinsics, false);

  FLAG_SET_DEFAULT(UseAES, false);
  FLAG_SET_DEFAULT(UseAESIntrinsics, false);

  FLAG_SET_DEFAULT(UseSHA, false);
  FLAG_SET_DEFAULT(UseSHA1Intrinsics, false);
  FLAG_SET_DEFAULT(UseSHA256Intrinsics, false);
  FLAG_SET_DEFAULT(UseSHA512Intrinsics, false);
}

void VM_Version::print_features() {
  tty->print_cr("Version: %s cache_line_size = %d", cpu_features(), (int) get_cache_line_size());
}

#ifdef COMPILER2
// Determine section size on power6: If section size is 8 instructions,
// there should be a difference between the two testloops of ~15 %. If
// no difference is detected the section is assumed to be 32 instructions.
void VM_Version::determine_section_size() {
  ShouldNotReachHere();
  /*

  int unroll = 80;

  const int code_size = (2* unroll * 32 + 100)*BytesPerInstWord;

  // Allocate space for the code.
  ResourceMark rm;
  CodeBuffer cb("detect_section_size", code_size, 0);
  MacroAssembler* a = new MacroAssembler(&cb);

  uint32_t *code = (uint32_t *)a->pc();
  // Emit code.
  void (*test1)() = (void(*)())(void *)a->function_entry();

  Label l1;

  a->li(R4, 1);
  a->sldi(R4, R4, 28);
  a->b(l1);
  a->align(CodeEntryAlignment);

  a->bind(l1);

  for (int i = 0; i < unroll; i++) {
    // Schleife 1
    // ------- sector 0 ------------
    // ;; 0
    a->nop();                   // 1
    a->fpnop0();                // 2
    a->fpnop1();                // 3
    a->addi(R4,R4, -1); // 4

    // ;;  1
    a->nop();                   // 5
    a->fmr(F6, F6);             // 6
    a->fmr(F7, F7);             // 7
    a->endgroup();              // 8
    // ------- sector 8 ------------

    // ;;  2
    a->nop();                   // 9
    a->nop();                   // 10
    a->fmr(F8, F8);             // 11
    a->fmr(F9, F9);             // 12

    // ;;  3
    a->nop();                   // 13
    a->fmr(F10, F10);           // 14
    a->fmr(F11, F11);           // 15
    a->endgroup();              // 16
    // -------- sector 16 -------------

    // ;;  4
    a->nop();                   // 17
    a->nop();                   // 18
    a->fmr(F15, F15);           // 19
    a->fmr(F16, F16);           // 20

    // ;;  5
    a->nop();                   // 21
    a->fmr(F17, F17);           // 22
    a->fmr(F18, F18);           // 23
    a->endgroup();              // 24
    // ------- sector 24  ------------

    // ;;  6
    a->nop();                   // 25
    a->nop();                   // 26
    a->fmr(F19, F19);           // 27
    a->fmr(F20, F20);           // 28

    // ;;  7
    a->nop();                   // 29
    a->fmr(F21, F21);           // 30
    a->fmr(F22, F22);           // 31
    a->brnop0();                // 32

    // ------- sector 32 ------------
  }

  // ;; 8
  a->cmpdi(CCR0, R4, unroll);   // 33
  a->bge(CCR0, l1);             // 34
  a->blr();

  // Emit code.
  void (*test2)() = (void(*)())(void *)a->function_entry();
  // uint32_t *code = (uint32_t *)a->pc();

  Label l2;

  a->li(R4, 1);
  a->sldi(R4, R4, 28);
  a->b(l2);
  a->align(CodeEntryAlignment);

  a->bind(l2);

  for (int i = 0; i < unroll; i++) {
    // Schleife 2
    // ------- sector 0 ------------
    // ;; 0
    a->brnop0();                  // 1
    a->nop();                     // 2
    //a->cmpdi(CCR0, R4, unroll);
    a->fpnop0();                  // 3
    a->fpnop1();                  // 4
    a->addi(R4,R4, -1);           // 5

    // ;; 1

    a->nop();                     // 6
    a->fmr(F6, F6);               // 7
    a->fmr(F7, F7);               // 8
    // ------- sector 8 ---------------

    // ;; 2
    a->endgroup();                // 9

    // ;; 3
    a->nop();                     // 10
    a->nop();                     // 11
    a->fmr(F8, F8);               // 12

    // ;; 4
    a->fmr(F9, F9);               // 13
    a->nop();                     // 14
    a->fmr(F10, F10);             // 15

    // ;; 5
    a->fmr(F11, F11);             // 16
    // -------- sector 16 -------------

    // ;; 6
    a->endgroup();                // 17

    // ;; 7
    a->nop();                     // 18
    a->nop();                     // 19
    a->fmr(F15, F15);             // 20

    // ;; 8
    a->fmr(F16, F16);             // 21
    a->nop();                     // 22
    a->fmr(F17, F17);             // 23

    // ;; 9
    a->fmr(F18, F18);             // 24
    // -------- sector 24 -------------

    // ;; 10
    a->endgroup();                // 25

    // ;; 11
    a->nop();                     // 26
    a->nop();                     // 27
    a->fmr(F19, F19);             // 28

    // ;; 12
    a->fmr(F20, F20);             // 29
    a->nop();                     // 30
    a->fmr(F21, F21);             // 31

    // ;; 13
    a->fmr(F22, F22);             // 32
  }

  // -------- sector 32 -------------
  // ;; 14
  a->cmpdi(CCR0, R4, unroll); // 33
  a->bge(CCR0, l2);           // 34

  a->blr();
  uint32_t *code_end = (uint32_t *)a->pc();
  a->flush();

  double loop1_seconds,loop2_seconds, rel_diff;
  uint64_t start1, stop1;

  start1 = os::current_thread_cpu_time(false);
  (*test1)();
  stop1 = os::current_thread_cpu_time(false);
  loop1_seconds = (stop1- start1) / (1000 *1000 *1000.0);


  start1 = os::current_thread_cpu_time(false);
  (*test2)();
  stop1 = os::current_thread_cpu_time(false);

  loop2_seconds = (stop1 - start1) / (1000 *1000 *1000.0);

  rel_diff = (loop2_seconds - loop1_seconds) / loop1_seconds *100;

  if (PrintAssembly) {
    ttyLocker ttyl;
    tty->print_cr("Decoding section size detection stub at " INTPTR_FORMAT " before execution:", p2i(code));
    Disassembler::decode((u_char*)code, (u_char*)code_end, tty);
    tty->print_cr("Time loop1 :%f", loop1_seconds);
    tty->print_cr("Time loop2 :%f", loop2_seconds);
    tty->print_cr("(time2 - time1) / time1 = %f %%", rel_diff);

    if (rel_diff > 12.0) {
      tty->print_cr("Section Size 8 Instructions");
    } else{
      tty->print_cr("Section Size 32 Instructions or Power5");
    }
  }

#if 0 // TODO: RISCV port
  // Set sector size (if not set explicitly).
  if (FLAG_IS_DEFAULT(Power6SectorSize128RISCV64)) {
    if (rel_diff > 12.0) {
      PdScheduling::power6SectorSize = 0x20;
    } else {
      PdScheduling::power6SectorSize = 0x80;
    }
  } else if (Power6SectorSize128RISCV64) {
    PdScheduling::power6SectorSize = 0x80;
  } else {
    PdScheduling::power6SectorSize = 0x20;
  }
#endif
  if (UsePower6SchedulerRISCV64) Unimplemented();
  */
}
#endif // COMPILER2

void VM_Version::determine_features() {
}

static int saved_features = 0;

void VM_Version::allow_all() {
  saved_features = _features;
  _features      = all_features_m;
}

void VM_Version::revert() {
  _features = saved_features;
}
