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

#include <stdio.h>
#include <pthread.h>
#include "rbc_base.h"

#ifdef __cplusplus
extern "C" {
#endif

RBC_ENGINE rbc;

// case #1
int pthread_mutex_lock(pthread_mutex_t *mutex)
{
  rbc.Model_decl(rbc.Fsm_use("pml"));
  // case #7
  rbc.Rbc_assert(rbc.Is_tag_set(rbc.Get_arg(1),"initialised"), "UIL");
  rbc.Model_decl(rbc.Set_parm_deref(rbc.Get_arg(1)));
  return 0;
}

int pthread_mutex_unlock(pthread_mutex_t *mutex)
{
  rbc.Model_decl(rbc.Fsm_use("pml"));
  return 0;
}

int pml_c_fsm(void)
{
  rbc.Model_decl(rbc.Fsm_build_begin("pml"));
  rbc.Model_decl(rbc.Fsm_new_start_state("start"));
  rbc.Model_decl(rbc.Fsm_new_final_state("finish"));

  rbc.Model_decl(rbc.Fsm_add_transition("start", "pthread_mutex_lock", rbc.Get_arg(1),
                                        1, "locked", NULL, 1000));
  rbc.Model_decl(rbc.Fsm_add_transition("locked", "pthread_mutex_unlock", rbc.Get_arg(1),
                                        1, "finish", NULL, 1001));
  rbc.Model_decl(rbc.Fsm_set_default_action("locked", "MLU", 1002));
  rbc.Model_decl(rbc.Fsm_build_end("pml"));
}

// case #2
class spinlock_t
{
};

class raw_spinlock_t
{
};

struct lock_class_key
{
};

int pthread_mutex_init(pthread_mutex_t *mutex, const pthread_mutexattr_t *attr)
{
  rbc.Rbc_assert(!rbc.Is_called_in_thread(), "LIIT");
  // case #7
  rbc.Model_decl(rbc.Set_tag(rbc.Get_arg(1), (char *)"initialised"));
  rbc.Model_decl(rbc.Set_parm_deref(rbc.Get_arg(1)));
  rbc.Model_decl(rbc.Set_parm_mod(rbc.Get_arg(1)));
  return 0;
}

void spin_lock_init(spinlock_t *lock)
{
  rbc.Rbc_assert(!rbc.Is_called_in_thread(), "LIIT");
}

void __raw_spin_lock_init(raw_spinlock_t *lock, const char *name,
                          struct lock_class_key *key)
{
  rbc.Rbc_assert(!rbc.Is_called_in_thread(), "LIIT");

  rbc.Model_decl(rbc.Set_parm_mod(rbc.Get_arg(1)));
  // rbc.Model_decl(rbc.Set_parm_deref(rbc.Get_arg(1)));
  rbc.Model_decl(rbc.Set_tag(rbc.Get_arg(1), (char *)"lock_init"));
  rbc.Rbc_assert(!rbc.Is_tag_set(rbc.Get_arg(1), "lock_init"), "DBLI");
}

// case #3
class spinlock
{
};

void glob_rule()
{
  rbc.For_all_func(rbc.Rbc_assert(rbc.Is_parm_type_addr_passed("spinlock"), "SLAE"));
}

// case #5
void test_isr1(int irq, pthread_spinlock_t *lock)
{
  rbc.Rbc_assert(rbc.Do_not_call("_raw_spin_lock"), "SAC");
}

void test_isr2(int irq, pthread_spinlock_t *lock)
{
  rbc.Model_decl(rbc.Set_func_tag("test_isr1", rbc.Get_ret(), rbc.Get_arg(1), rbc.Get_arg(2)));
}

// case #6
struct completion;
void complete(struct completion *x)
{
  rbc.Model_decl(rbc.Set_func_thread(1));
}

// case #7
void mutex_lock(pthread_mutex_t *arg1){
  rbc.Model_decl(rbc.Set_parm_deref(rbc.Get_arg(1)));
}

void mutex_initialise(pthread_mutex_t *arg1){
  rbc.Model_decl(rbc.Set_parm_mod(rbc.Get_arg(1)));
}

#ifdef __cplusplus
} // extern C
#endif
