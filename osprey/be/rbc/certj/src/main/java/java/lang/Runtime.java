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

package java.lang;
import io.xc5.*;
import java.io.File;


public class Runtime {
  // [cert fio14]
  public void exit(int var1) {
    RBC_ENGINE.Model_decl(RBC_ENGINE.Fsm_use("fio14"));
  }

  // [cert fio14]
  public void halt(int var1) {
    RBC_ENGINE.Model_decl(RBC_ENGINE.Fsm_use("fio14"));
  }

  public Process exec(String command) {
    RBC_ENGINE.Rbc_assert(
      RBC_ENGINE.Is_tag_attr_set(RBC_ENGINE.Get_arg(1), "tainted", "sanitize_cmd"),
      "IDS07-J");
    return null;
  }

  public Process exec(String[] cmdarray) {
    RBC_ENGINE.Rbc_assert(
      RBC_ENGINE.Is_tag_attr_set(RBC_ENGINE.Get_arg(1), "tainted", "sanitize_cmd"),
      "IDS07-J");
    return null;
  }

  public Process exec(String[] cmdarray, String[] envp) {
    RBC_ENGINE.Rbc_assert(
      RBC_ENGINE.Is_tag_attr_set(RBC_ENGINE.Get_arg(1), "tainted", "sanitize_cmd"),
      "IDS07-J");
    RBC_ENGINE.Rbc_assert(
      RBC_ENGINE.Is_tag_attr_set(RBC_ENGINE.Get_arg(2), "tainted", "sanitize_cmd"),
      "IDS07-J");
    return null;
  }

  public Process exec(String[] cmdarray, String[] envp, File dir) {
    RBC_ENGINE.Rbc_assert(
      RBC_ENGINE.Is_tag_attr_set(RBC_ENGINE.Get_arg(1), "tainted", "sanitize_cmd"),
      "IDS07-J");
    RBC_ENGINE.Rbc_assert(
      RBC_ENGINE.Is_tag_attr_set(RBC_ENGINE.Get_arg(2), "tainted", "sanitize_cmd"),
      "IDS07-J");
    return null;
  }

  public Process exec(String command, String[] envp) {
    RBC_ENGINE.Rbc_assert(
      RBC_ENGINE.Is_tag_attr_set(RBC_ENGINE.Get_arg(1), "tainted", "sanitize_cmd"),
      "IDS07-J");
    RBC_ENGINE.Rbc_assert(
      RBC_ENGINE.Is_tag_attr_set(RBC_ENGINE.Get_arg(2), "tainted", "sanitize_cmd"),
      "IDS07-J");
    return null;
  }

  public Process exec(String command, String[] envp, File dir) {
    RBC_ENGINE.Rbc_assert(
      RBC_ENGINE.Is_tag_attr_set(RBC_ENGINE.Get_arg(1), "tainted", "sanitize_cmd"),
      "IDS07-J");
    RBC_ENGINE.Rbc_assert(
      RBC_ENGINE.Is_tag_attr_set(RBC_ENGINE.Get_arg(2), "tainted", "sanitize_cmd"),
      "IDS07-J");
    return null;
  }

  public void addShutdownHook(Thread thread) {
    RBC_ENGINE.Model_decl(
      RBC_ENGINE.Exec_eval(RBC_ENGINE.EXEC_KIND.SET_FUNC_SHUTDOWN, 0, 0, 0));
  }
}
