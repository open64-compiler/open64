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

package java.io;

import io.xc5.Constants;
import io.xc5.RBC_ENGINE;

import java.net.URI;

public class File {
  public File(String pathname) {
    RBC_ENGINE.Model_decl(RBC_ENGINE.Fsm_use("fio02"));
    RBC_ENGINE.Model_decl(RBC_ENGINE.Copy_tag(RBC_ENGINE.Get_this_pointer(), RBC_ENGINE.Get_arg(1)));
    RBC_ENGINE.Rbc_assert(
      RBC_ENGINE.Not(RBC_ENGINE.Is_tag_set(RBC_ENGINE.Get_arg(1), "tainted")),
      "FIO16-J"
    );
  }

  public File(File parent, String child) {
    RBC_ENGINE.Rbc_assert(
      RBC_ENGINE.Not(RBC_ENGINE.Is_tag_set(RBC_ENGINE.Get_arg(2), "tainted")),
      "FIO16-J"
    );
  }

  public File(String parent, String child) {
    RBC_ENGINE.Rbc_assert(
      RBC_ENGINE.Not(RBC_ENGINE.Is_tag_set(RBC_ENGINE.Get_arg(1), "tainted")),
      "FIO16-J"
    );
    RBC_ENGINE.Rbc_assert(
      RBC_ENGINE.Not(RBC_ENGINE.Is_tag_set(RBC_ENGINE.Get_arg(2), "tainted")),
      "FIO16-J"
    );
  }

  public File(URI uri) {
    RBC_ENGINE.Rbc_assert(
      RBC_ENGINE.Not(RBC_ENGINE.Is_tag_set(RBC_ENGINE.Get_arg(1), "tainted")),
      "FIO16-J"
    );
  }

  // [cert exp00]
  // [cert fio02]
  public boolean delete() {
    /*
    RBC_ENGINE.Rbc_assert(
      RBC_ENGINE.Post_check(
        RBC_ENGINE.Get_ret()
      ),
      Constants.XVSA_ERR_CODE.XVSA_CHECK_BEFORE_USE,
      "FIO02-J",
      "Detect and handle file-related errors",
      RBC_ENGINE.RULE_CATEGORY.CERT,
      "",
      0
    );
    */
    RBC_ENGINE.Model_decl(RBC_ENGINE.Fsm_use("fio02"));
    return true;
  }

  /*
  public static File[] listRoots() {
    RBC_ENGINE.Rbc_assert(false, Constants.XVSA_ERR_CODE.XVSA_CHECK_BEFORE_USE, "", "", 0, "", 0);
    return null;
  }
  */
}
