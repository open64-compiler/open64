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


public class FileInputStream {

  // Modelling of fopen
  FileInputStream(File file) {
    // Maybe extract the path ? need to double check?
    // RBC_ENGINE.Model_decl(RBC_ENGINE.Rsc_add(RBC_ENGINE.Get_arg(1),
    //         null,
    //         RBC_ENGINE.RSC_KIND.FILE));
    RBC_ENGINE.Rbc_assert(
      RBC_ENGINE.Is_tag_attr_set(RBC_ENGINE.Get_arg(1), "sensitive", "sanitize_path"),
      "FIO16-J"
    );
  }

  FileInputStream(FileDescriptor fdObj) {
    // RBC_ENGINE.Model_decl(RBC_ENGINE.Rsc_add(RBC_ENGINE.Get_arg(1),
    //         null,
    //         RBC_ENGINE.RSC_KIND.FILE));
  }

  FileInputStream(String fileName) {
    RBC_ENGINE.Model_decl(RBC_ENGINE.Fsm_use("sec01"));
    RBC_ENGINE.Rbc_assert(
      RBC_ENGINE.Not(RBC_ENGINE.Is_tag_set(RBC_ENGINE.Get_arg(1), "tainted")),
      "FIO16-J"
    );
  }

  public void close () {
    // RBC_ENGINE.Model_decl(RBC_ENGINE.Rsc_delete(RBC_ENGINE.Get_this_pointer(),
    //         RBC_ENGINE.RSC_KIND.FILE));
  }

  public int available() throws IOException {
    // RBC_ENGINE.Rbc_assert(RBC_ENGINE.Post_check_rsc_value(RBC_ENGINE.Get_ret(),
    //         "ne",
    //         -1,
    //         null,
    //         RBC_ENGINE.RSC_KIND.NONE,
    //         RBC_ENGINE.RSC_STATUS.NONE) |
    //                 RBC_ENGINE.Post_check_rsc_value(RBC_ENGINE.Get_ret(),
    //                         "narrowed",
    //                         -1,
    //                         null,
    //                         RBC_ENGINE.RSC_KIND.NONE,
    //                         RBC_ENGINE.RSC_STATUS.NONE), Constants.XVSA_ERR_CODE.XVSA_CHECK_BEFORE_USE,
    //         "NO-FILE_INPUT_STREAM-NARROWED",
    //         "ERR_CERT_FILEINPUTSTREAM",
    //         RBC_ENGINE.RULE_CATEGORY.CERT,
    //         "Try to remove or modify the code to avoid double free", 0);
    return 0;
  }

  // [cert fio08]
  // Classes inherited from InputStream or Reader's read method
  // cannot cast return value to byte before check end of file
  // Need new API to check if a value is narrowed
  public int read() {
    // return value either not checked with -1 (end of file)
    // or not narrowed
    // RBC_ENGINE.Rbc_assert(RBC_ENGINE.Post_check_rsc_value(RBC_ENGINE.Get_ret(),
    //   "ne",
    //   -1,
    //   null,
    //   RBC_ENGINE.RSC_KIND.NONE,
    //   RBC_ENGINE.RSC_STATUS.NONE) |
    //   RBC_ENGINE.Post_check_rsc_value(RBC_ENGINE.Get_ret(),
    //     "narrowed",
    //     -1,
    //     null,
    //     RBC_ENGINE.RSC_KIND.NONE,
    //     RBC_ENGINE.RSC_STATUS.NONE), Constants.XVSA_ERR_CODE.XVSA_CHECK_BEFORE_USE,
    //    "NO-FILE_INPUT_STREAM-NARROWED",
    //     "ERR_CERT_FILEINPUTSTREAM",
    //     RBC_ENGINE.RULE_CATEGORY.CERT,
    //     "Try to remove or modify the code to avoid double free", 0);
    return 0;
  }
}
