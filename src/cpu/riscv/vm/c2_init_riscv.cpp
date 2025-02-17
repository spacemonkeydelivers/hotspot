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
#include "opto/compile.hpp"
#include "opto/node.hpp"
#include "runtime/globals.hpp"
#include "utilities/debug.hpp"

// processor dependent initialization for ppc

void Compile::pd_compiler2_init() {

  // Power7 and later
  if (PowerArchitectureRISCV64 > 6) {
    if (FLAG_IS_DEFAULT(UsePopCountInstruction)) {
      FLAG_SET_ERGO(bool, UsePopCountInstruction, true);
    }
  }

  if (PowerArchitectureRISCV64 == 6) {
    if (FLAG_IS_DEFAULT(InsertEndGroupRISCV64)) {
      FLAG_SET_ERGO(bool, InsertEndGroupRISCV64, true);
    }
  }
}
