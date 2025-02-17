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

#ifndef CPU_RISCV_VM_GLOBALS_RISCV_HPP
#define CPU_RISCV_VM_GLOBALS_RISCV_HPP

#include "utilities/globalDefinitions.hpp"
#include "utilities/macros.hpp"

// Sets the default values for platform dependent flags used by the runtime system.
// (see globals.hpp)

define_pd_global(bool, ConvertSleepToYield,   true);
define_pd_global(bool, ShareVtableStubs,      false); // Improves performance markedly for mtrt and compress.
define_pd_global(bool, NeedsDeoptSuspend,     false); // Only register window machines need this.


define_pd_global(bool, ImplicitNullChecks,    true);  // Generate code for implicit null checks.
define_pd_global(bool, TrapBasedNullChecks,   true);
define_pd_global(bool, UncommonNullCast,      true);  // Uncommon-trap NULLs passed to check cast.

// Use large code-entry alignment.
define_pd_global(intx, CodeEntryAlignment,    64);
define_pd_global(intx, OptoLoopAlignment,     16);
define_pd_global(intx, InlineFrequencyCount,  100);
define_pd_global(intx, InlineSmallCode,       1500);

define_pd_global(intx, PreInflateSpin,        10);

// Flags for template interpreter.
define_pd_global(bool, RewriteBytecodes,      true);
define_pd_global(bool, RewriteFrequentPairs,  true);

define_pd_global(bool, UseMembar,             false);

define_pd_global(bool, PreserveFramePointer,  false);

// GC Ergo Flags
define_pd_global(uintx, CMSYoungGenPerWorker, 16*M);  // Default max size of CMS young gen, per GC worker thread.

define_pd_global(uintx, TypeProfileLevel, 0);

// Platform dependent flag handling: flags only defined on this platform.
#define ARCH_FLAGS(develop, product, diagnostic, experimental, notproduct)  \
                                                                            \
  /* Load poll address from thread. This is used to implement per-thread */ \
  /* safepoints on platforms != IA64. */                                    \
  product(bool, LoadPollAddressFromThread, false,                           \
          "Load polling page address from thread object (required for "     \
          "per-thread safepoints on platforms != IA64)")                    \
                                                                            \
  product(uintx, PowerArchitectureRISCV64, 0,                                 \
          "CPU Version: x for PowerX. Currently recognizes Power5 to "      \
          "Power7. Default is 0. CPUs newer than Power7 will be "           \
          "recognized as Power7.")                                          \
                                                                            \
  /* Reoptimize code-sequences of calls at runtime, e.g. replace an */      \
  /* indirect call by a direct call.                                */      \
  product(bool, ReoptimizeCallSequences, true,                              \
          "Reoptimize code-sequences of calls at runtime.")                 \
                                                                            \
  product(bool, UseLoadInstructionsForStackBangingRISCV64, false,             \
          "Use load instructions for stack banging.")                       \
                                                                            \
  /* special instructions */                                                \
                                                                            \
  product(bool, UseCountLeadingZerosInstructionsRISCV64, true,                \
          "Use count leading zeros instructions.")                          \
                                                                            \
  product(bool, UseExtendedLoadAndReserveInstructionsRISCV64, false,          \
          "Use extended versions of load-and-reserve instructions.")        \
                                                                            \
  product(bool, UseRotateAndMaskInstructionsRISCV64, true,                    \
          "Use rotate and mask instructions.")                              \
                                                                            \
  product(bool, UseStaticBranchPredictionInCompareAndSwapRISCV64, true,       \
          "Use static branch prediction hints in CAS operations.")          \
  product(bool, UseStaticBranchPredictionForUncommonPathsRISCV64, false,      \
          "Use static branch prediction hints for uncommon paths.")         \
                                                                            \
  product(bool, UsePower6SchedulerRISCV64, false,                             \
          "Use Power6 Scheduler.")                                          \
                                                                            \
  product(bool, InsertEndGroupRISCV64, false,                                 \
          "Insert EndGroup instructions to optimize for Power6.")           \
                                                                            \
  /* Trap based checks. */                                                  \
  /* Trap based checks use the ppc trap instructions to check certain */    \
  /* conditions. This instruction raises a SIGTRAP caught by the      */    \
  /* exception handler of the VM.                                     */    \
  product(bool, UseSIGTRAP, true,                                           \
          "Allow trap instructions that make use of SIGTRAP. Use this to "  \
          "switch off all optimizations requiring SIGTRAP.")                \
  product(bool, TrapBasedICMissChecks, true,                                \
          "Raise and handle SIGTRAP if inline cache miss detected.")        \
  product(bool, TrapBasedNotEntrantChecks, true,                            \
          "Raise and handle SIGTRAP if calling not entrant or zombie"       \
          " method.")                                                       \
  product(bool, TraceTraps, false, "Trace all traps the signal handler"     \
          "handles.")                                                       \
                                                                            \
  product(bool, ZapMemory, false, "Write 0x0101... to empty memory."        \
          " Use this to ease debugging.")                                   \


#endif // CPU_RISCV_VM_GLOBALS_RISCV_HPP
