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

public class RandomAccessFile
{
  public RandomAccessFile(File file, String mode)
  {
    // CERT SEC02
    // file.getPath should not be overwritten
    RBC_ENGINE.Rbc_assert(
      RBC_ENGINE.Exec_eval(
        RBC_ENGINE.EXEC_KIND.CMP_EQ,
        RBC_ENGINE.Exec_eval(RBC_ENGINE.EXEC_KIND.IS_OBJ_METH_OVERRIDE,
                             RBC_ENGINE.Exec_eval(RBC_ENGINE.EXEC_KIND.GET_ARG,1),
                             "getPath()Ljava.lang.String;"),
        0),
        "SEC02-J"
     );
  }
}
