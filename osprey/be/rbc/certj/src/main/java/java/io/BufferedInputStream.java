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


public class BufferedInputStream {

  // [cert fio06]
  // Assert when the first param resource been created before the construction
  //
  public BufferedInputStream(InputStream var1) {
    /*
    RBC_ENGINE.Model_decl(RBC_ENGINE.Rsc_add(RBC_ENGINE.Get_arg(1),
                                             null,
                                             RBC_ENGINE.RSC_KIND.FILE));

    RBC_ENGINE.Rbc_assert(!RBC_ENGINE.Pre_rsc_is_set_status(RBC_ENGINE.Get_arg(1),
                                                       RBC_ENGINE.RSC_KIND.FILE,
                                                       RBC_ENGINE.RSC_STATUS.CREATED),
                      Constants.XVSA_ERR_CODE.XVSA_CHECK_BEFORE_USE,
            "",
            "",
            RBC_ENGINE.RULE_CATEGORY.CERT,
            "",
            0);
    */
  }

  // [cert fio06]
  public BufferedInputStream(InputStream var1, int var2) {
    /*
    RBC_ENGINE.Model_decl(RBC_ENGINE.Rsc_add(RBC_ENGINE.Get_arg(1),
                                             null,
                                             RBC_ENGINE.RSC_KIND.FILE));
    RBC_ENGINE.Rbc_assert(
      !RBC_ENGINE.Pre_rsc_is_set_status(
        RBC_ENGINE.Get_arg(1), RBC_ENGINE.RSC_KIND.FILE, RBC_ENGINE.RSC_STATUS.CREATED
      ),
      Constants.XVSA_ERR_CODE.XVSA_CHECK_BEFORE_USE,
      "",
      "",
      RBC_ENGINE.RULE_CATEGORY.CERT,
      "",
      0
    );
    */
  }
}
