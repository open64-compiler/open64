/*
   Copyright (C) 2019-2022 Xcalibyte (Shenzhen) Limited.

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
 */

// ==================================================================
// vsa_annot.h
// define builtin annotations for VSA
// ==================================================================

#ifndef vsa_annot_INCLUDED
#define vsa_annot_INCLUDED

#include "defs.h"
#include "opt_defs.h"

class VSA;
class CODEREP;

// VSA annotation (V_ANNOT)
// refer osprey/doc/design_flag_prop.txt for documentation

// annotation represented by bit
// these annotations are applicable on var (by CODEREP with CK_VAR) and vor (by
// VSYM_OBJ_REP).
enum ANT_KIND {
  ANT_ZERO     =  0,  // var was zero: v = 0
  ANT_CONST    =  1,  // var was constant: v = constant
  ANT_READ     =  2,  // var was read: <- v
  ANT_WRITE    =  3,  // var was written: v <-
  ANT_DIV      =  4,  // var was used as divisor: blah / v, blah % v
  ANT_LB       =  5,  // var has lower bound: v > blah, v >= blah
  ANT_UB       =  6,  // var has upper bound: v < blah; v <= blah
  ANT_MALLOC   =  7,  // var has pointer from malloc: v = malloc()
  ANT_FREE     =  8,  // var has pointer to free: free(v)
  ANT_REALLOC  =  9,  // var has pointer from realloc. use with malloc/free
  ANT_FORMAL   = 10,  // var has value from formal: v <- param
  ANT_ACTUAL   = 11,  // var was used as actual: blah(v)
  ANT_GLOBAL   = 12,  // var has value from global: v <- global
  ANT_RETVAL   = 13,  // var has value from return value: v <- retval
  ANT_DUMMY0   = 14,  // not used
  ANT_DUMMY1   = 15,  // not used
  ANT_VSYM     = 16,  // vsym offset
  ANT_VZERO    = ANT_VSYM + ANT_ZERO,    // vsym was zero: *v = 0
  ANT_VCONST   = ANT_VSYM + ANT_CONST,   // vsym was constant: *v = constant
  ANT_VREAD    = ANT_VSYM + ANT_READ,    // vsym was read: <- *v
  ANT_VWRITE   = ANT_VSYM + ANT_WRITE,   // vsym was written: *v <-
  ANT_VDIV     = ANT_VSYM + ANT_DIV,     // vsym was used as divisor: blah / *v, blah % *v
  ANT_VLB      = ANT_VSYM + ANT_LB,      // vsym has lower bound: *v > blah, *v >= blah
  ANT_VUB      = ANT_VSYM + ANT_UB,      // vsym has upper bound: *v < blah, *v <= blah
  ANT_VMALLOC  = ANT_VSYM + ANT_MALLOC,  // vsym has pointer from malloc: *v = malloc()
  ANT_VFREE    = ANT_VSYM + ANT_FREE,    // vsym has pointer to free: free(*v)
  ANT_VREALLOC = ANT_VSYM + ANT_REALLOC, // vsym has pointer from realloc: *v = realloc()
  ANT_VFORMAL  = ANT_VSYM + ANT_FORMAL,  // vsym has value from formal: *v = parm
  ANT_VACTUAL  = ANT_VSYM + ANT_ACTUAL,  // vsym used as actual: blah(*v)
  ANT_VGLOBAL  = ANT_VSYM + ANT_GLOBAL,  // vsym has value from global: *v <- global
  ANT_VRETVAL  = ANT_VSYM + ANT_RETVAL,  // vsym has value from return value: *v <- retval
  ANT_KIND_LAST                       // always the last one
};

// for each annot, we uses 2 bits
enum ANT_STAT {
  ANT_NO    = 0x0,        // var doesn't have this annotation
  ANT_YES   = 0x1,        // var has this annotation on all paths
  ANT_MAYBE = 0x2,        // var may have this annotation on some paths
  ANT_MIN   = 0,          // pseudo min value
  ANT_MAX   = 3,          // pseudo max value
};

#define ANNOT_BITS         2                                  // 2 bits for 1 annot
#define ANNOT_PER_UNIT     (sizeof(V_ANNOT)*8/ANNOT_BITS)     // max annot in V_ANNOT
#define ANNOT_MASK         (((UINT64)1<<ANNOT_BITS)-1)        // mask
#define ANNOT_VAL(ant)     (ANNOT_MASK << (ANNOT_BITS * ant)) // annotation value

// utility class to manipulate vsa annotation
class VANT_UTIL {
private:
  enum {
    // flags can be passed from caller to callee by parameters in forward propagation
    ANNOT_PARMIN_FWD_MASK = ANNOT_VAL(ANT_ZERO)     | // actual/formal is 0
                            ANNOT_VAL(ANT_CONST)    | // actual/formal is constant
                            ANNOT_VAL(ANT_WRITE)    | // actual/formal was written
                            ANNOT_VAL(ANT_LB)       | // actual/formal has lower bound
                            ANNOT_VAL(ANT_UB)       | // actual/formal has upper bound
                            ANNOT_VAL(ANT_MALLOC)   | // actual/formal pointer from malloc
                            ANNOT_VAL(ANT_FREE)     | // actual/formal pointer been freed
                            ANNOT_VAL(ANT_GLOBAL)   | // actual/formal value from global
                            ANNOT_VAL(ANT_VZERO)    | // *(actual/formal) is 0
                            ANNOT_VAL(ANT_VCONST)   | // *(actual/formal) is constant
                            ANNOT_VAL(ANT_VWRITE)   | // *(actual/formal) was written
                            ANNOT_VAL(ANT_VLB)      | // *(actual/formal) has lower bound
                            ANNOT_VAL(ANT_VUB)      | // *(actual/formal) has upper bound
                            ANNOT_VAL(ANT_VMALLOC)  | // *(actual/formal) pointer from malloc
                            ANNOT_VAL(ANT_VFREE)    | // *(actual/formal) pointer been freed
                            ANNOT_VAL(ANT_VGLOBAL),  // *(actual/formal) value from global

    // flags can be passed back from callee to caller by parameters in backward propagation
    ANNOT_PARMIN_BWD_MASK = ANNOT_VAL(ANT_READ)     | // actual/formal read in callee
                            ANNOT_VAL(ANT_DIV)      | // actual/formal used as divisor in callee
                            ANNOT_VAL(ANT_FREE)     | // actual/formal pointer freed in callee
                            ANNOT_VAL(ANT_VZERO)    | // *(actual/formal) set to 0 in callee
                            ANNOT_VAL(ANT_VCONST)   | // *(actual/formal) set to constant in callee
                            ANNOT_VAL(ANT_VREAD)    | // *(actual/formal) read in callee
                            ANNOT_VAL(ANT_VWRITE)   | // *(actual/formal) written in callee
                            ANNOT_VAL(ANT_VDIV)     | // *(actual/formal) used as divisor in callee
                            ANNOT_VAL(ANT_VMALLOC)  | // *(actual/formal) malloc'ed in callee
                            ANNOT_VAL(ANT_VFREE),     // *(actual/formal) pointer freed in callee

    // flags can be passed from callee to caller by output parameter in backward propagation
    ANNOT_PARMOUT_BWD_MASK = ANNOT_VAL(ANT_READ)    | // actual/formal read in callee
                             ANNOT_VAL(ANT_DIV)     | // actual/formal used as divisor in callee
                             ANNOT_VAL(ANT_FREE)    | // actual/formal pointer freed in callee
                             ANNOT_VAL(ANT_VZERO)   | // *(actual/formal) set to 0 in callee
                             ANNOT_VAL(ANT_VCONST)  | // *(actual/formal) set to constant in callee
                             ANNOT_VAL(ANT_VREAD)   | // *(actual/formal) read in callee
                             ANNOT_VAL(ANT_VWRITE)  | // *(actual/formal) written in callee
                             ANNOT_VAL(ANT_VDIV)    | // *(actual/formal) used as divisor in callee
                             ANNOT_VAL(ANT_VMALLOC) | // *(actual/formal) malloc'ed in callee
                             ANNOT_VAL(ANT_VFREE)   | // *(actual/formal) freed in callee
                             ANNOT_VAL(ANT_VGLOBAL),  // *(actual/formal) value from global

    // flags can be passed from callee to caller by return value in forward propagation
    ANNOT_RETV_FWD_MASK = ANNOT_VAL(ANT_ZERO)       | // retv is 0
                          ANNOT_VAL(ANT_CONST)      | // retv is constant
                          ANNOT_VAL(ANT_WRITE)      | // retv written in callee
                          ANNOT_VAL(ANT_LB)         | // retv has lower bound
                          ANNOT_VAL(ANT_UB)         | // retv has upper bound
                          ANNOT_VAL(ANT_MALLOC)     | // retv pointer from malloc
                          ANNOT_VAL(ANT_FREE)       | // retv pointer freed
                          ANNOT_VAL(ANT_GLOBAL)     | // retv from global
                          ANNOT_VAL(ANT_VZERO)      | // *(retv) is 0
                          ANNOT_VAL(ANT_VCONST)     | // *(retv) is constant
                          ANNOT_VAL(ANT_VWRITE)     | // *(retv) was written
                          ANNOT_VAL(ANT_VLB)        | // *(retv) has lower bound
                          ANNOT_VAL(ANT_VUB)        | // *(retv) has upper bound
                          ANNOT_VAL(ANT_VMALLOC)    | // *(retv) pointer from malloc
                          ANNOT_VAL(ANT_VFREE)      | // *(retv) pointer freed
                          ANNOT_VAL(ANT_VGLOBAL),     // *(retv) value from global

    // flags can be passed from caller to callee by return value in backward propagation
    ANNOT_RETV_BWD_MASK = ANNOT_VAL(ANT_READ)       | // retv read in caller after callsite
                          ANNOT_VAL(ANT_DIV)        | // retv used as divisor in caller after callsite
                          ANNOT_VAL(ANT_FREE)       | // retv pointer freed in caller after callsite
                          ANNOT_VAL(ANT_VREAD)      | // *(retv) read in caller after callsite
                          ANNOT_VAL(ANT_VDIV)       | // *(retv) used as divisor in caller after callsite
                          ANNOT_VAL(ANT_VFREE),       // *(retv) pointer freed in caller after callsite

    // flags propagate from rhs to lhs
    ANNOT_PROP_FWD_MASK = ANNOT_VAL(ANT_ZERO)       | // lhs is zero
                          ANNOT_VAL(ANT_CONST)      | // lhs is constant
                          ANNOT_VAL(ANT_WRITE)      | // lhs is write
                          ANNOT_VAL(ANT_LB)         | // lhs has lower bound
                          ANNOT_VAL(ANT_UB),          // lhs has upper bound

    // flags propagate from lhs to rhs
    ANNOT_PROP_BWD_MASK = ANNOT_VAL(ANT_READ)       | // rhs is read
                          ANNOT_VAL(ANT_FREE)       | // rhs pointer is freed
                          ANNOT_VAL(ANT_VFREE)      | // rhs pointer is freed
                          ANNOT_VAL(ANT_VREAD)      | // lhs's vread to rhs
                          ANNOT_VAL(ANT_VWRITE),      // lhs's vwrite to rhs

    // flags only for pointer
    ANNOT_PROP_PTR_MASK = ANNOT_VAL(ANT_MALLOC)     | // ptr is malloced
                          ANNOT_VAL(ANT_FREE)       | // ptr is freed
                          ANNOT_VAL(ANT_REALLOC)    | // ptr is realloced
                          ANNOT_VAL(ANT_VZERO)      | // *(ptr) <- 0
                          ANNOT_VAL(ANT_VCONST)     | // *(ptr) <- constant
                          ANNOT_VAL(ANT_VREAD)      | // <- *(ptr)
                          ANNOT_VAL(ANT_VWRITE)     | // *(ptr) <-
                          ANNOT_VAL(ANT_VDIV),        // x / *(ptr)
  };

public:
  // empty annotation
  static V_ANNOT Empty() {
    return 0;
  }

  // set annotation
  static V_ANNOT Set(V_ANNOT v, ANT_KIND annot, ANT_STAT stat) {
    Is_True(annot < (ANT_KIND)ANNOT_PER_UNIT, ("annot out of bound"));
    Is_True(stat >= ANT_MIN && stat <= ANT_MAX, ("annot state wrong"));
    V_ANNOT val = (V_ANNOT)stat << (annot * ANNOT_BITS);
    v |= val;
    return v;
  }

  // init V_ANNOT with one annotation
  static V_ANNOT Init(ANT_KIND annot, ANT_STAT stat) {
    V_ANNOT v = Empty();
    return Set(v, annot, stat);
  }

  // Var_annot: only returns the annot on v
  static V_ANNOT Var_annot(V_ANNOT v) {
    return (V_ANNOT)((UINT32)v);
  }

  // Vor_annot: only returns the annot on *v
  static V_ANNOT Vor_annot(V_ANNOT v) {
    return (V_ANNOT)((v >> 32) << 32);
  }

  // Var_2_vor: *p = x, copy x's var flag to p's vor flag
  static V_ANNOT Var_2_vor(V_ANNOT v) {
    return (V_ANNOT)(v << 32);
  }

  // Vor_2_var: x = *p, copy p's vor flag to x's var flag
  static V_ANNOT Vor_2_var(V_ANNOT v) {
    return (V_ANNOT)(v >> 32);
  }

  // get annotation
  static ANT_STAT Get(V_ANNOT v, ANT_KIND annot) {
    Is_True(annot < (ANT_KIND)ANNOT_PER_UNIT, ("annot out of bound"));
    V_ANNOT val = v >> (annot * ANNOT_BITS);
    return (ANT_STAT)(val & ANNOT_MASK);
  }

  // copy annotation
  static V_ANNOT Copy(V_ANNOT dst, V_ANNOT src, ANT_KIND annot) {
    Is_True(annot < (ANT_KIND)ANNOT_PER_UNIT, ("annot out of bound"));
    V_ANNOT mask = ((V_ANNOT)ANNOT_MASK) << (annot * ANNOT_BITS);
    return (dst & (~mask)) | (src & mask);
  }

  static V_ANNOT Copy2(V_ANNOT dst, V_ANNOT src, ANT_KIND annot1, ANT_KIND annot2) {
    Is_True(annot1 < (ANT_KIND)ANNOT_PER_UNIT, ("annot out of bound"));
    Is_True(annot2 < (ANT_KIND)ANNOT_PER_UNIT, ("annot out of bound"));
    V_ANNOT mask = (((UINT64)ANNOT_MASK) << (annot1 * ANNOT_BITS)) |
                   (((UINT64)ANNOT_MASK) << (annot2 * ANNOT_BITS));
    return (dst & (~mask)) | (src & mask);
  }

  // and operation
  static V_ANNOT And(V_ANNOT v1, V_ANNOT v2) {
    return v1 & v2;
  }

  // or operation
  static V_ANNOT Or(V_ANNOT v1, V_ANNOT v2) {
    return v1 | v2;
  }

  // YES -> MAY
  static V_ANNOT Yes_to_maybe(V_ANNOT v) {
    V_ANNOT yes_mask = 0x5555555555555555ULL;
    return (V_ANNOT)((v & yes_mask) << 1);
  }

  // MAY -> YES
  static V_ANNOT Maybe_to_yes(V_ANNOT v) {
    V_ANNOT may_mask = 0xaaaaaaaaaaaaaaaaULL;
    return (V_ANNOT)((v & may_mask) >> 1);
  }

  // join annotation from different path for phi
  // NO  U NO  -> NO    00 U 00 -> 00
  // NO  U YES -> MAY   00 U 01 -> 10
  // NO  U MAY -> MAY   00 U 10 -> 10
  // YES U NO  -> MAY   01 U 00 -> 10
  // YES U YES -> YES   01 U 01 -> 01
  // YES U MAY -> MAY   01 U 10 -> 10
  // MAY U NO  -> MAY   10 U 00 -> 10
  // MAY U YEA -> MAY   10 U 10 -> 10
  // MAY U MAY -> MAY   10 U 10 -> 10
  static V_ANNOT Join(V_ANNOT v1, V_ANNOT v2) {
    // calculate maybe flag
    V_ANNOT may_mask = 0xaaaaaaaaaaaaaaaaULL;
    V_ANNOT res_maybe = (v1 | v2) & may_mask;
    // calculate yes flag
    V_ANNOT yes_mask = 0x5555555555555555ULL;
    V_ANNOT v1_yes = v1 & yes_mask;
    V_ANNOT v2_yes = v2 & yes_mask;
    V_ANNOT res_yes = v1_yes & v2_yes;
    // convert yes flag to maybe flag
    V_ANNOT res_maybe_from_yes = (v1_yes ^ v2_yes) << 1;
    // calculate result
    return res_maybe | res_yes | res_maybe_from_yes; 
  }

  // merge actual to formal for pass in parameter
  static V_ANNOT Merge_parm(V_ANNOT formal, V_ANNOT actual) {
    V_ANNOT passin = Join(formal & ANNOT_PARMIN_FWD_MASK,
                          actual & ANNOT_PARMIN_FWD_MASK);
    return (formal & ~ANNOT_PARMIN_FWD_MASK) | passin;
  }

  // merge formal to actual for pass in parameter
  static V_ANNOT Merge_parmin_rev(V_ANNOT actual, V_ANNOT formal) {
    V_ANNOT passin = Join(actual & ANNOT_PARMIN_BWD_MASK,
                          formal & ANNOT_PARMIN_BWD_MASK);
    return (actual & ~ANNOT_PARMIN_BWD_MASK) | passin;
  }

  // merge formal to actual for pass out parameter
  static V_ANNOT Merge_parmout_rev(V_ANNOT actual, V_ANNOT formal) {
    V_ANNOT passout = Join(actual & ANNOT_PARMOUT_BWD_MASK,
                           formal & ANNOT_PARMOUT_BWD_MASK);
    return (actual & ~ANNOT_PARMOUT_BWD_MASK) | passout;
  }

  // merge return value from callee to caller
  static V_ANNOT Merge_retv(V_ANNOT caller, V_ANNOT callee) {
    V_ANNOT retv = Join(caller & ANNOT_RETV_FWD_MASK,
                        callee & ANNOT_RETV_FWD_MASK);
    return (caller & ~ANNOT_RETV_FWD_MASK) | retv;
  }

  // merge return value back from caller to callee
  static V_ANNOT Merge_retv_rev(V_ANNOT callee, V_ANNOT caller) {
    V_ANNOT retv_back = Join(callee & ANNOT_RETV_BWD_MASK,
                           caller & ANNOT_RETV_BWD_MASK);
    return (callee & ~ANNOT_RETV_BWD_MASK) | retv_back;
  }

  // backward propagate annotation
  static V_ANNOT Bwd_prop_annot(V_ANNOT lhs) {
    return (lhs & ANNOT_PROP_BWD_MASK);
  }

  // keep pointer related annotation
  static V_ANNOT Keep_ptr_annot(V_ANNOT lhs) {
    return (lhs & ANNOT_PROP_PTR_MASK);
  }

  // remove pointer related annotation
  static V_ANNOT Remove_ptr_annot(V_ANNOT lhs) {
    return (lhs & ~ANNOT_PROP_PTR_MASK);
  }

  // clear annotation
  static V_ANNOT Clear(V_ANNOT v, ANT_KIND annot) {
    Is_True(annot < (ANT_KIND)ANNOT_PER_UNIT, ("annot out of bound"));
    V_ANNOT val = ~((V_ANNOT)ANNOT_MASK << (annot * ANNOT_BITS));
    v &= val;
    return v;
  }

  // dump annotation into file
  static void Dump(FILE *fp, V_ANNOT v);

  // dump annotation into stdout
  static void Dump(V_ANNOT v);

};  /* VANT_UTIL */

V_ANNOT Get_cr_annot(VSA *vsa, CODEREP *cr);

#endif /* vsa_annot_INCLUDED */
