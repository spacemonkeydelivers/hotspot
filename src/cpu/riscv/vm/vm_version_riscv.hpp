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

#ifndef CPU_RISCV_VM_VM_VERSION_RISCV_HPP
#define CPU_RISCV_VM_VM_VERSION_RISCV_HPP

#include "runtime/globals_extension.hpp"
#include "runtime/vm_version.hpp"

class VM_Version: public Abstract_VM_Version {
protected:
  enum Feature_Flag {
    num_features // last entry to count features
  };
  enum Feature_Flag_Set {
    unknown_m             =  0,
    all_features_m        = -1
  };
  static int  _features;
  static int  _measured_cache_line_size;
  static const char* _features_str;
  static bool _is_determine_features_test_running;

  static void print_features();
  static void determine_features(); // also measures cache line size
  static void determine_section_size();
public:
  // Initialization
  static void initialize();

  static bool is_determine_features_test_running() { return _is_determine_features_test_running; }

  static const char* cpu_features() { return _features_str; }

  static int get_cache_line_size()  { return _measured_cache_line_size; }

  // Assembler testing
  static void allow_all();
  static void revert();
};

#endif // CPU_RISCV_VM_VM_VERSION_RISCV_HPP
