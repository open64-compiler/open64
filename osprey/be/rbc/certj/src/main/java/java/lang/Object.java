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

import io.xc5.RBC_ENGINE;
public class Object
{
  public boolean equals(Object obj) {
    // CERT EXP02
    // do not allow array type to call this function
    RBC_ENGINE.Rbc_assert(
      RBC_ENGINE.Exec_eval(
        RBC_ENGINE.EXEC_KIND.OR,
        RBC_ENGINE.Exec_eval(
          RBC_ENGINE.EXEC_KIND.CMP_NE,
          RBC_ENGINE.Exec_eval(
            RBC_ENGINE.EXEC_KIND.GET_TYPE_KIND,
            RBC_ENGINE.Exec_eval(RBC_ENGINE.EXEC_KIND.GET_THIS_POINTER)
          ),
          RBC_ENGINE.TYPE_KIND.ARRAY
        ),  // arg0 is array type
        RBC_ENGINE.Exec_eval(
          RBC_ENGINE.EXEC_KIND.CMP_NE,
          RBC_ENGINE.Exec_eval(
            RBC_ENGINE.EXEC_KIND.GET_TYPE_KIND,
            RBC_ENGINE.Exec_eval(RBC_ENGINE.EXEC_KIND.GET_ARG, 1)
          ),
          RBC_ENGINE.TYPE_KIND.ARRAY
        ) // arg1 is array type
      ),
      "EXP02-J"
    );
    return true;
  }

  public final Class<?> getClass() {
    RBC_ENGINE.Model_decl(RBC_ENGINE.Fsm_use("obj09"));
    return null;
  }
}
