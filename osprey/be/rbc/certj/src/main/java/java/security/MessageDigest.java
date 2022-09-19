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

public class MessageDigest {
  public static MessageDigest getInstance(String algorithm) {
    RBC_ENGINE.Model_decl(RBC_ENGINE.Set_implicit_assign(RBC_ENGINE.Get_ret(), RBC_ENGINE.Get_arg(1)));
    RBC_ENGINE.Rbc_assert(
      RBC_ENGINE.Not(RBC_ENGINE.Is_str_match(RBC_ENGINE.Get_arg(1), "^(MD5|SHA1)$")),
      "MSC62-J");
    RBC_ENGINE.Rbc_rule_exception("MSC62-J", 1);
    return null;
  }

  public static MessageDigest getInstance(String algorithm, String provider) {
    RBC_ENGINE.Model_decl(RBC_ENGINE.Set_implicit_assign(RBC_ENGINE.Get_ret(), RBC_ENGINE.Get_arg(1)));
    RBC_ENGINE.Rbc_assert(
      RBC_ENGINE.Not(RBC_ENGINE.Is_str_match(RBC_ENGINE.Get_arg(1), "^(MD5|SHA1)$")),
      "MSC62-J");
    RBC_ENGINE.Rbc_rule_exception("MSC62-J", 1);
    return null;
  }

  public byte[] digest(byte[] var1) {
    RBC_ENGINE.Model_decl(RBC_ENGINE.Copy_tag(RBC_ENGINE.Get_ret(), RBC_ENGINE.Get_arg(1)));
    RBC_ENGINE.Model_decl(RBC_ENGINE.If(
      RBC_ENGINE.Is_str_match(RBC_ENGINE.Get_this_pointer(), "^(MD5|SHA1)$"),
      1,
      RBC_ENGINE.Set_tag_attr(RBC_ENGINE.Get_ret(), RBC_ENGINE.Get_arg(1), "sensitive", "secure_hash")
    ));

    // used for FSM key matching, should provide another API
    RBC_ENGINE.Model_decl(RBC_ENGINE.Set_implicit_assign(RBC_ENGINE.Get_ret(), RBC_ENGINE.Get_arg(1)));

    return null;
  }
}
