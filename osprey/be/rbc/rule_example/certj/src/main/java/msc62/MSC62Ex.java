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

import io.xc5.RBC_ENGINE;
public class MSC62Ex {
  void regUser1(String userName, byte[] passwd, String regType) {
    RBC_ENGINE.Model_decl(RBC_ENGINE.Fsm_use("msc62"));
  }
  void regUser2(String userName, String passwd) {
      RBC_ENGINE.Model_decl(RBC_ENGINE.Fsm_use("msc62"));
      RBC_ENGINE.Rbc_assert(
        RBC_ENGINE.Not(
          RBC_ENGINE.Is_const_str_eq(
            RBC_ENGINE.Get_type_name(RBC_ENGINE.Get_arg(2)),
            "java.lang.String"
          )
        ),
        "MSC62-J"
      );
    }

  void saveUser(String userName, byte[] passwd) {
    RBC_ENGINE.Model_decl(RBC_ENGINE.Fsm_use("msc62"));
  }

  private byte[] unSecureHash(byte [] passwd) {
    RBC_ENGINE.Model_decl(RBC_ENGINE.Set_implicit_assign(RBC_ENGINE.Get_ret(), RBC_ENGINE.Get_arg(1)));
    return null;
  }

  private byte[] secureHash(byte [] passwd) {
    RBC_ENGINE.Model_decl(RBC_ENGINE.Set_implicit_assign(RBC_ENGINE.Get_ret(), RBC_ENGINE.Get_arg(1)));
    return null;
  }
}
