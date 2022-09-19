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

#include "sys_base.h"
#include "rbc_base.h"

/*
  ATTENTION:
      Part of c++ functions have different mangled name when use different compiler frontend
    for example: c++ std namespace function, some version starts with std::,
    and some starts with std::__1:: or std::c++11:: and so on.

      Just select a mangled name, and write rbc rules, and the rule will automatically apply
    on same methods that the original method signature are same.

      Futher more for generic methods, just select a mangled name with any specialized type and
    write rbc rule. And that rule will apply on other methods that is same generic method.

    For example:
    std::list<int, std::allocator<int> >::push_back(int const&) => _ZNSt4listIiSaIiEE9push_backERKi
    std::__cxx11::list<int, std::allocator<int> >::push_back(int const&) => _ZNSt7__cxx114listIiSaIiEE9push_backERKi
    Just select one method, and then write rule. And the rule will apply any type of list::push_back(int const&).
*/

#ifdef __cplusplus
extern "C" {
#endif

RBC_ENGINE rbc;

// mangled function name of mersenne_twister_engine (clang++)
void _ZNSt3__123mersenne_twister_engineImLm32ELm624ELm397ELm31ELm2567483615ELm11ELm4294967295ELm7ELm2636928640ELm15ELm4022730752ELm18ELm1812433253EEC1Em(void *, unsigned long)
{
  rbc.Rbc_assert(!rbc.Is_parm_constant(rbc.Get_arg(2)), "MSC51-CPP");
  rbc.Rbc_assert(!rbc.Parm_is_def_by_func(rbc.Get_arg(2), "time"), "MSC51-CPP");
}

sighandler_t signal(int signum, sighandler_t handler)
{
  rbc.Rbc_assert(!rbc.Is_parm_plain_old_func(rbc.Get_arg(2)), "MSC54-CPP");
  return NULL;
}

void *operator new(size_t)
{
  rbc.Rbc_assert(rbc.Is_return_value_checked(), "MEM55-CPP");
  rbc.Model_decl(rbc.Set_tag(rbc.Get_ret(), (char *)"new"));
}

void* operator new[](size_t)
{
  rbc.Model_decl(rbc.Set_tag(rbc.Get_ret(), (char *)"new[]"));
}

void operator delete(void*)
{
  //rbc.Rbc_assert(rbc.Is_tag_set(rbc.Get_arg(1), "new"), "MEM51-CPP");
}

void operator delete[](void*)
{
  //rbc.Rbc_assert(rbc.Is_tag_set(rbc.Get_arg(1), "new[]"), "MEM51-CPP");
}

// mangled function name of basic_ostream (g++)
void _ZStlsISt11char_traitsIcEERSt13basic_ostreamIcT_ES5_PKc(void *, char const*)
{
  rbc.Model_decl(rbc.Set_parm_mod(rbc.Get_arg(1)));
  rbc.Model_decl(rbc.Set_parm_deref(rbc.Get_arg(2)));
}

// mangled function name of basic_ostream (clang++)
void _ZNSt3__1lsINS_11char_traitsIcEEEERNS_13basic_ostreamIcT_EES6_PKc(void *, char const*)
{
  rbc.Model_decl(rbc.Set_parm_mod(rbc.Get_arg(1)));
  rbc.Model_decl(rbc.Set_parm_deref(rbc.Get_arg(2)));
}

// std::__cxx11::list<int, std::allocator<int> >::push_back(int&&)
void _ZNSt7__cxx114listIiSaIiEE9push_backEOi()
{
  rbc.Model_decl(rbc.Merge_tag(rbc.Get_this_pointer(), rbc.Get_this_pointer(), rbc.Get_arg(1)));
  rbc.Model_decl(rbc.Set_func_coll_append_ref(rbc.Get_this_pointer(), rbc.Get_arg(1)));
}

// std::__cxx11::list<int, std::allocator<int> >::back()
void _ZNSt7__cxx114listIiSaIiEE4backEv()
{
  rbc.Model_decl(rbc.Set_func_coll_back_ref(rbc.Get_this_pointer()));
  rbc.Model_decl(rbc.Eval_tag(rbc.Get_ret(), rbc.Get_this_pointer()));
}

#ifdef __cplusplus
} // extern C
#endif
