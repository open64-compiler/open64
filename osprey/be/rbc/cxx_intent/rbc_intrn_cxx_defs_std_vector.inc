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

/* macros to declare rbc for std::vector intrinsic */

// std::vector::vector()
#define DECL_RBC_for_ZNSt3__16vectorC1Ev() \
  DECL_RBC_for_container_init()

#define DECL_RBC_for_ZNSt3__16vectorC1ERKT0_() \
  DECL_RBC_for_container_init()

#define DECL_RBC_for_ZNSt3__16vectorC1ENS_13__vector_baseIT_T0_E9size_typeE() \
  DECL_RBC_for_container_init()

#define DECL_RBC_for_ZNSt3__16vectorC1ENS_13__vector_baseIT_T0_E9size_typeERKS3_() \
  DECL_RBC_for_container_init()

#define DECL_RBC_for_ZNSt3__16vectorC1ENS_13__vector_baseIT_T0_E9size_typeERKS2_() \
  DECL_RBC_for_container_init()

#define DECL_RBC_for_ZNSt3__16vectorC1ENS_13__vector_baseIT_T0_E9size_typeERKS2_RKS3_() \
  DECL_RBC_for_container_init()

#define DECL_RBC_for_ZNSt3__16vectorC1ETL0__NS_9enable_ifIXaaaasr19__is_input_iteratorIS1_EE5valuentsr21__is_forward_iteratorIS1_EE5valuesr16is_constructibleIT_NS_15iterator_traitsIS1_E9referenceEEE5valueES1_E4typeE() \
  DECL_RBC_for_container_init()

#define DECL_RBC_for_ZNSt3__16vectorC1ETL0__S1_RKT0_PNS_9enable_ifIXaaaasr19__is_input_iteratorIS1_EE5valuentsr21__is_forward_iteratorIS1_EE5valuesr16is_constructibleIT_NS_15iterator_traitsIS1_E9referenceEEE5valueEvE4typeE() \
  DECL_RBC_for_container_init()

#define DECL_RBC_for_ZNSt3__16vectorC1ETL0__NS_9enable_ifIXaasr21__is_forward_iteratorIS1_EE5valuesr16is_constructibleIT_NS_15iterator_traitsIS1_E9referenceEEE5valueES1_E4typeE() \
  DECL_RBC_for_container_init()

#define DECL_RBC_for_ZNSt3__16vectorC1ETL0__S1_RKT0_PNS_9enable_ifIXaasr21__is_forward_iteratorIS1_EE5valuesr16is_constructibleIT_NS_15iterator_traitsIS1_E9referenceEEE5valueEvE4typeE() \
  DECL_RBC_for_container_init()

#define DECL_RBC_for_ZNSt3__16vectorC1ERKNS_6vectorIT_T0_EE() \
  rbc.Model_decl(rbc.Set_parm_mod(rbc.Get_arg(1))); \
  rbc.Model_decl(rbc.Set_parm_deref(rbc.Get_arg(2))); \
  rbc.Model_decl(rbc.Set_func_container_init(rbc.Get_arg(1), rbc.Get_arg(2)));

#define DECL_RBC_for_ZNSt3__16vectorC1ESt16initializer_listIT_E() \
  DECL_RBC_for_container_init()

#define DECL_RBC_for_ZNSt3__16vectorC1ESt16initializer_listIT_ERKT0_() \
  DECL_RBC_for_container_init()

#define DECL_RBC_for_ZNSt3__16vectorC1EONS_6vectorIT_T0_EE() \
  DECL_RBC_for_container_init()

#define DECL_RBC_for_ZNSt3__16vectorC1EONS_6vectorIT_T0_EERKS3_() \
  DECL_RBC_for_container_init()


// std::vector::push_back()
#define DECL_RBC_for_ZNSt3__16vector9push_backENS_13__vector_baseIT_T0_E15const_referenceE() \
  DECL_RBC_for_container_append()

#define DECL_RBC_for_ZNSt3__16vector9push_backEOT_() \
  DECL_RBC_for_container_append()

// std::vector::back()
#define DECL_RBC_for_ZNSt3__16vector4backEv() \
  DECL_RBC_for_container_back()

#define DECL_RBC_for_ZNKSt3__16vector4backEv() \
  DECL_RBC_for_container_back()

// std::vector::operator[](), std::vector::at()
#define DECL_RBC_for_vector_at() \
  rbc.Model_decl(rbc.Set_func_coll_get_ref(rbc.Get_arg(1), rbc.Get_arg(2))); \
  rbc.Model_decl(rbc.Set_parm_deref(rbc.Get_arg(1))); \
  rbc.Model_decl(rbc.Copy_tag(rbc.Get_ret(), rbc.Get_arg(1)));

#define DECL_RBC_for_ZNSt3__16vectorixENS_13__vector_baseIT_T0_E9size_typeE() \
  DECL_RBC_for_vector_at()

#define DECL_RBC_for_ZNKSt3__16vectorixENS_13__vector_baseIT_T0_E9size_typeE() \
  DECL_RBC_for_vector_at()

#define DECL_RBC_for_ZNSt3__16vector2atENS_13__vector_baseIT_T0_E9size_typeE() \
  DECL_RBC_for_vector_at()

#define DECL_RBC_for_ZNKSt3__16vector2atENS_13__vector_baseIT_T0_E9size_typeE() \
  DECL_RBC_for_vector_at()


// std::vector::insert
#define DECL_RBC_for_ZNSt3__16vector6insertENS_11__wrap_iterINS_13__vector_baseIT_T0_E13const_pointerEEENS5_9size_typeENS5_15const_referenceE() \
  rbc.Model_decl(rbc.Set_func_coll_insert(rbc.Get_arg(1), rbc.Get_arg(2), rbc.Get_arg(3), rbc.Get_arg(4))); \
  rbc.Model_decl(rbc.Set_parm_mod(rbc.Get_arg(1))); \
  rbc.Model_decl(rbc.Set_parm_deref(rbc.Get_arg(2))); \
  rbc.Model_decl(rbc.Set_parm_deref(rbc.Get_arg(3)));

// std::vector::end()
#define DECL_RBC_for_vector_end() \
  rbc.Model_decl(rbc.Set_func_coll_end(rbc.Get_arg(1))); \
  rbc.Model_decl(rbc.Set_parm_deref(rbc.Get_arg(1)));

#define DECL_RBC_for_ZNSt3__16vector3endEv() \
  DECL_RBC_for_vector_end()

#define DECL_RBC_for_ZNKSt3__16vector3endEv() \
  DECL_RBC_for_vector_end()
