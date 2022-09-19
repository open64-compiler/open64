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

public class String {
  public String(byte[] var1) {
    RBC_ENGINE.Model_decl(RBC_ENGINE.Copy_tag(RBC_ENGINE.Get_this_pointer(), RBC_ENGINE.Get_arg(1)));
    RBC_ENGINE.Model_decl(RBC_ENGINE.Set_implicit_assign(RBC_ENGINE.Get_this_pointer(), RBC_ENGINE.Get_arg(1)));

  }

  public String replaceAll( String var1, String var2) {
    RBC_ENGINE.Model_decl(RBC_ENGINE.Fsm_use("ids01_11"));
    return null;
  }

  public byte[] getBytes() {
    RBC_ENGINE.Model_decl(
      RBC_ENGINE.Set_implicit_assign(RBC_ENGINE.Get_ret(),
                                     RBC_ENGINE.Get_this_pointer())
    );
    RBC_ENGINE.Model_decl(RBC_ENGINE.Copy_tag(RBC_ENGINE.Get_ret(), RBC_ENGINE.Get_this_pointer()));
    return null;
  }

  public char charAt(int index) {
    RBC_ENGINE.Model_decl(
      // the first parameter is string pointer, second parater is index
      RBC_ENGINE.Set_func_str_get(RBC_ENGINE.Get_this_pointer(), RBC_ENGINE.Get_arg(1))
    );
    return '0';
  }

  public boolean equals(Object obj) {
    RBC_ENGINE.Model_decl(RBC_ENGINE.Fsm_use("obj09"));
    return false;
  }

}
