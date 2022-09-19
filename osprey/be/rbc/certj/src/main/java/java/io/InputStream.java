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

import io.xc5.RBC_ENGINE;

// cert fio10
// assert when parameter 1's length was not checked after call read
// Need api to provide get resource's length
public abstract class InputStream implements Closeable {
  public int read(byte[] var1) {
//    RBC_ENGINE.Rbc_assert(!RBC_ENGINE.Post_check(RBC_ENGINE.Get_mem_size(RBC_ENGINE.Get_arg(1)) > RBC_ENGINE.Get_value(RBC_ENGINE.Get_arg(2));
//    RBC_ENGINE.Rbc_assert(!RBC_ENGINE.Post_check(RBC_ENGINE.Get_arg(1),
//      RBC_ENGINE.Rsc_get(RBC_ENGINE.Get_arg(1),
//        RBC_ENGINE.RSC_KIND.ALLOC),
//      RBC_ENGINE.RSC_KIND.ALLOC,
//      null),
//      Constants.XVSA_ERR_CODE.XVSA_CHECK_BEFORE_USE);
    return 0;
  }

  public int read(byte[] var1, int var2, int var3) {
    //RBC_ENGINE.Rbc_assert(!RBC_ENGINE.Post_check(RBC_ENGINE.Get_mem_size(RBC_ENGINE.Get_arg(1)) > RBC_ENGINE.Get_value(RBC_ENGINE.Get_arg(2)), ));
//      RBC_ENGINE.Rsc_get(RBC_ENGINE.Get_arg(1),
//        RBC_ENGINE.RSC_KIND.ALLOC.Get_value()),
//        RBC_ENGINE.RSC_KIND.ALLOC.Get_value(),
//      null), Constants.XVSA_ERR_CODE.XVSA_CHECK_BEFORE_USE);
    return 0;
  }
}
