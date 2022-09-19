/*
 * Copyright (C) 2019-2022 Xcalibyte (Shenzhen) Limited.
 */

/* macros to declare rbc for std::map intrinsic */
#define DECL_RBC_for_ZNSt3__13mapC1Ev() \
  DECL_RBC_for_container_init()

/* std::__1::map<typename _Key, typename _Tp, typename _Compare, typename _Allocator>::map(const map<_Key, _Tp, _Compare, _Allocator> & __m) */
#define DECL_RBC_for_ZNSt3__13mapC1ERKNS_3mapIT_T0_T1_T2_EE() \
  rbc.Model_decl(rbc.Set_parm_mod(rbc.Get_arg(1))); \
  rbc.Model_decl(rbc.Set_parm_deref(rbc.Get_arg(2))); \
  rbc.Model_decl(rbc.Set_func_container_init(rbc.Get_arg(1), rbc.Get_arg(2)));

/* std::__1::map<typename _Key, typename _Tp, typename _Compare, typename _Allocator>::operator[](std::__1::map::key_type && __k) */
#define DECL_RBC_for_ZNSt3__13mapixEOT_() \
  DECL_RBC_for_map_get()



