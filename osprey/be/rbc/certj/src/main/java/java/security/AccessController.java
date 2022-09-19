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

package java.security;
import io.xc5.RBC_ENGINE;
import java.security.PrivilegedExceptionAction;

public final class AccessController {
  public static Object doPrivileged( PrivilegedExceptionAction var1) {
    RBC_ENGINE.Model_decl(RBC_ENGINE.Fsm_use("sec01"));

    RBC_ENGINE.Model_decl(
      RBC_ENGINE.Exec_eval(RBC_ENGINE.EXEC_KIND.IMPLICIT_CALL,
                           RBC_ENGINE.Exec_eval(RBC_ENGINE.EXEC_KIND.GET_ARG,1),
                           "run()Ljava.lang.Object;",
                           var1
      )
    );

    return null;
  }
  public static Object doPrivileged( PrivilegedAction var1) {
    RBC_ENGINE.Model_decl(
      RBC_ENGINE.Exec_eval(RBC_ENGINE.EXEC_KIND.IMPLICIT_CALL,
                           RBC_ENGINE.Exec_eval(RBC_ENGINE.EXEC_KIND.GET_ARG,1),
                           "run()Ljava.lang.Object;",
                           var1
      )
    );

    return null;
  }
}
