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

#ifndef CPU_RISCV_VM_ICACHE_RISCV_HPP
#define CPU_RISCV_VM_ICACHE_RISCV_HPP

// Interface for updating the instruction cache.  Whenever the VM modifies
// code, part of the processor instruction cache potentially has to be flushed.

class ICache : public AbstractICache {
  friend class ICacheStubGenerator;

  static int riscv_flush_icache_stub(address addr, int lines, int magic);

 public:
  enum {
    // These don't matter...
    log2_line_size = 1,
    line_size      = 1
  };

  static void riscv_flush_icache();
};

#endif // CPU_RISCV_VM_ICACHE_RISCV_HPP
