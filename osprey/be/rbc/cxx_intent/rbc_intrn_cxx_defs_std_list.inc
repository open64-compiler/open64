/*
 * Copyright (C) 2019-2022 Xcalibyte (Shenzhen) Limited.
 */

/* macros to declare rbc for std::list intrinsic */
#define DECL_RBC_for_ZNSt3__14listC1Ev() \
  DECL_RBC_for_container_init()

#define DECL_RBC_for_ZNSt3__14listC1ERKNS_4listIT_T0_EE() \
  rbc.Model_decl(rbc.Set_parm_mod(rbc.Get_arg(1))); \
  rbc.Model_decl(rbc.Set_parm_deref(rbc.Get_arg(2))); \
  rbc.Model_decl(rbc.Set_func_container_init(rbc.Get_arg(1), rbc.Get_arg(2)));

#define DECL_RBC_for_ZNSt3__14list9push_backERKT_() \
  DECL_RBC_for_container_append()

#define DECL_RBC_for_ZNSt3__14list9push_backEOT_() \
  DECL_RBC_for_container_append()

#define DECL_RBC_for_ZNKSt3__14list4backEv() \
  DECL_RBC_for_container_back()

#define DECL_RBC_for_ZNSt3__14list4backEv() \
  DECL_RBC_for_container_back()
