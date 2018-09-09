/*
 * Copyright (c) 1997, 2013, Oracle and/or its affiliates. All rights reserved.
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

#ifndef OS_CPU_LINUX_RISCV_VM_ATOMIC_LINUX_RISCV_INLINE_HPP
#define OS_CPU_LINUX_RISCV_VM_ATOMIC_LINUX_RISCV_INLINE_HPP

#include "runtime/atomic.hpp"
#include "runtime/os.hpp"
#include "vm_version_riscv.hpp"

// Implementation of class atomic

inline void Atomic::store    (jbyte    store_value, jbyte*    dest) { *dest = store_value; }
inline void Atomic::store    (jshort   store_value, jshort*   dest) { *dest = store_value; }
inline void Atomic::store    (jint     store_value, jint*     dest) { *dest = store_value; }
inline void Atomic::store    (jlong    store_value, jlong*    dest) { *dest = store_value; }
inline void Atomic::store_ptr(intptr_t store_value, intptr_t* dest) { *dest = store_value; }
inline void Atomic::store_ptr(void*    store_value, void*     dest) { *(void**)dest = store_value; }

inline void Atomic::store    (jbyte    store_value, volatile jbyte*    dest) { *dest = store_value; }
inline void Atomic::store    (jshort   store_value, volatile jshort*   dest) { *dest = store_value; }
inline void Atomic::store    (jint     store_value, volatile jint*     dest) { *dest = store_value; }
inline void Atomic::store    (jlong    store_value, volatile jlong*    dest) { *dest = store_value; }
inline void Atomic::store_ptr(intptr_t store_value, volatile intptr_t* dest) { *dest = store_value; }
inline void Atomic::store_ptr(void*    store_value, volatile void*     dest) { *(void* volatile *)dest = store_value; }

inline jlong Atomic::load(volatile jlong* src) { return *src; }

//
// semantic barrier instructions:
// (as defined in orderAccess.hpp)
//
// - release         orders Store|Store,       (maps to lwsync)
//                           Load|Store
// - acquire         orders  Load|Store,       (maps to lwsync)
//                           Load|Load
// - fence           orders Store|Store,       (maps to sync)
//                           Load|Store,
//                           Load|Load,
//                          Store|Load
//

inline jint     Atomic::add    (jint     add_value, volatile jint*     dest) {

  unsigned int result;

  __asm__ __volatile__ (
    "   amoadd.w   %0, %1, (%2)    \n"
    : /*%0*/"=&r" (result)
    : /*%1*/"r" (add_value), /*%2*/"r" (dest)
    : "memory" );

  return (jint) result;
}


inline intptr_t Atomic::add_ptr(intptr_t add_value, volatile intptr_t* dest) {

  long result;

  __asm__ __volatile__ (
    "   amoadd.d   %0, %1, (%2)    \n"
    : /*%0*/"=&r" (result)
    : /*%1*/"r" (add_value), /*%2*/"r" (dest)
    : "memory" );  
    
  return (intptr_t) result;
}

inline void*    Atomic::add_ptr(intptr_t add_value, volatile void*     dest) {
  return (void*)add_ptr(add_value, (volatile intptr_t*)dest);
}


inline void Atomic::inc    (volatile jint*     dest) {

  unsigned int temp;
  int temp2;

  __asm__ __volatile__ (
    "   addi       %[temp2], zero, 1             \n"
    "   amoadd.w   %[temp], %[temp2], (%[dest])  \n"
    : [temp]  "=&r" (temp), 
      [temp2] "=&r" (temp), 
              "=m"  (*dest)
    : [dest]  "r"   (dest), 
              "m"   (*dest)
    : "memory"
    );

}

inline void Atomic::inc_ptr(volatile intptr_t* dest) {

  long temp;
  long temp2;

  __asm__ __volatile__ (
    "   addi       %[temp2], zero, 1             \n"
    "   amoadd.d   %[temp], %[temp2], (%[dest])  \n"
    : [temp]  "=&r" (temp), 
      [temp2] "=&r" (temp), 
              "=m"  (*dest)
    : [dest]  "r"   (dest), 
              "m"   (*dest)
    : "memory"
    );

}

inline void Atomic::inc_ptr(volatile void*     dest) {
  inc_ptr((volatile intptr_t*)dest);
}


inline void Atomic::dec    (volatile jint*     dest) {

  unsigned int temp;
  int temp2;

  __asm__ __volatile__ (
    "   addi       %[temp2], zero, -1            \n"
    "   amoadd.w   %[temp], %[temp2], (%[dest])  \n"
    : [temp]  "=&r" (temp), 
      [temp2] "=&r" (temp), 
              "=m"  (*dest)
    : [dest]  "r"   (dest), 
              "m"   (*dest)
    : "memory"
    );

}

inline void Atomic::dec_ptr(volatile intptr_t* dest) {

  long temp;
  long temp2;

  __asm__ __volatile__ (
    "   addi       %[temp2], zero, -1            \n"
    "   amoadd.d   %[temp], %[temp2], (%[dest])  \n"
    : [temp]  "=&r" (temp), 
      [temp2] "=&r" (temp), 
              "=m"  (*dest)
    : [dest]  "r"   (dest), 
              "m"   (*dest)
    : "memory"
    );

}

inline void Atomic::dec_ptr(volatile void*     dest) {
  dec_ptr((volatile intptr_t*)dest);
}

inline jint Atomic::xchg(jint exchange_value, volatile jint* dest) {

  unsigned int old_value;

  __asm__ __volatile__ (
    "   amoswap.w   %0, %3, (%2) \n"
    : "=&r" (old_value), "=m" (*dest)
    : "r" (dest), "r" (exchange_value), "m" (*dest)
    : "memory"
    );

  return (jint) old_value;
}

inline intptr_t Atomic::xchg_ptr(intptr_t exchange_value, volatile intptr_t* dest) {

  long old_value;

  __asm__ __volatile__ (
    "   amoswap.d   %0, %3, (%2) \n"
    : "=&r" (old_value), "=m" (*dest)
    : "r" (dest), "r" (exchange_value), "m" (*dest)
    : "memory"
    );

  return (intptr_t) old_value;
}

inline void* Atomic::xchg_ptr(void* exchange_value, volatile void* dest) {
  return (void*)xchg_ptr((intptr_t)exchange_value, (volatile intptr_t*)dest);
}

inline jint Atomic::cmpxchg(jint exchange_value, volatile jint* dest, jint compare_value) {

  unsigned int old_value;
  uint64_t result;

  __asm__ __volatile__ (
    "1:                                                \n"
    "   lr.w       %[old_value], (%[dest])             \n"
    "   bne        %[cmp_value], %[old_value], 2f      \n"
    "   sc.w       %[result], %[xcg_value], (%[dest])  \n"
    "   bne        %[result], zero, 1b                 \n"
    "2:                                                \n"
    : [old_value] "=&r" (old_value), 
      [result]    "=&r" (result), 
                  "=m"  (*dest)
    : [dest]      "r"   (dest), 
      [cmp_value] "r"   (compare_value), 
      [xcg_value] "r"   (exchange_value), 
                  "m"   (*dest)
    : "memory"
    );

  return (jint) old_value;
}

inline jlong Atomic::cmpxchg(jlong exchange_value, volatile jlong* dest, jlong compare_value) {

  long old_value;
  uint64_t result;

  __asm__ __volatile__ (
    "1:                                                \n"
    "   lr.d       %[old_value], (%[dest])             \n"
    "   bne        %[cmp_value], %[old_value], 2f      \n"
    "   sc.d       %[result], %[xcg_value], (%[dest])  \n"
    "   bne        %[result], zero, 1b                 \n"
    "2:                                                \n"
    : [old_value] "=&r" (old_value), 
      [result]    "=&r" (result), 
                  "=m"  (*dest)
    : [dest]      "r"   (dest), 
      [cmp_value] "r"   (compare_value), 
      [xcg_value] "r"   (exchange_value), 
                  "m"   (*dest)
    : "memory"
    );

  return (jlong) old_value;
}

inline intptr_t Atomic::cmpxchg_ptr(intptr_t exchange_value, volatile intptr_t* dest, intptr_t compare_value) {
  return (intptr_t)cmpxchg((jlong)exchange_value, (volatile jlong*)dest, (jlong)compare_value);
}

inline void* Atomic::cmpxchg_ptr(void* exchange_value, volatile void* dest, void* compare_value) {
  return (void*)cmpxchg((jlong)exchange_value, (volatile jlong*)dest, (jlong)compare_value);
}

#endif // OS_CPU_LINUX_RISCV_VM_ATOMIC_LINUX_RISCV_INLINE_HPP
