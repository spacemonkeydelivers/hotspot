/*
 * Copyright (c) 2000, 2013, Oracle and/or its affiliates. All rights reserved.
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

#include "precompiled.hpp"
#include "assembler_riscv.inline.hpp"
#include "runtime/icache.hpp"

// Use inline assembler to implement icache flush.
void ICache::riscv_flush_icache() {
  __asm__ __volatile__("fence.i" : : : "memory");
}

// Need this signiture for shared code to work on stub...
int ICache::riscv_flush_icache_stub(address a, int lines, int magic) {
  __asm__ __volatile__("fence.i" : : : "memory");
  return magic;
}

void ICacheStubGenerator::generate_icache_flush(ICache::flush_icache_stub_t* flush_icache_stub) {
  StubCodeMark mark(this, "ICache", "flush_icache_stub");

  *flush_icache_stub = (ICache::flush_icache_stub_t)ICache::riscv_flush_icache_stub;

  // First call to flush itself
  ICache::invalidate_range((address)(*flush_icache_stub), 0);
}
