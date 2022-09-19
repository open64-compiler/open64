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
import java.util.List;

public final class ProcessBuilder {
  public ProcessBuilder(String... var1) {
    RBC_ENGINE.Model_decl(RBC_ENGINE.Copy_tag(RBC_ENGINE.Get_this_pointer(), RBC_ENGINE.Get_arg(1)));
    RBC_ENGINE.Model_decl(RBC_ENGINE.Set_parm_mod(RBC_ENGINE.Get_this_pointer()));
    RBC_ENGINE.Model_decl(RBC_ENGINE.Set_parm_deref(RBC_ENGINE.Get_arg(1)));

  }
  public ProcessBuilder(List<String> var1) {
    RBC_ENGINE.Model_decl(RBC_ENGINE.Copy_tag(RBC_ENGINE.Get_this_pointer(), RBC_ENGINE.Get_arg(1)));
    RBC_ENGINE.Model_decl(RBC_ENGINE.Set_parm_mod(RBC_ENGINE.Get_this_pointer()));
    RBC_ENGINE.Model_decl(RBC_ENGINE.Set_parm_deref(RBC_ENGINE.Get_arg(1)));
  }
  public ProcessBuilder command(List<String> var1) {
    RBC_ENGINE.Model_decl(RBC_ENGINE.Copy_tag(RBC_ENGINE.Get_this_pointer(), RBC_ENGINE.Get_arg(1)));
    RBC_ENGINE.Model_decl(RBC_ENGINE.Set_parm_mod(RBC_ENGINE.Get_this_pointer()));
    RBC_ENGINE.Model_decl(RBC_ENGINE.Set_parm_deref(RBC_ENGINE.Get_arg(1)));
    return null;
  }
  public ProcessBuilder command(String... var1) {
    RBC_ENGINE.Model_decl(RBC_ENGINE.Copy_tag(RBC_ENGINE.Get_this_pointer(), RBC_ENGINE.Get_arg(1)));
    RBC_ENGINE.Model_decl(RBC_ENGINE.Set_parm_mod(RBC_ENGINE.Get_this_pointer()));
    RBC_ENGINE.Model_decl(RBC_ENGINE.Set_parm_deref(RBC_ENGINE.Get_arg(1)));
    return null;
  }
  public Process start() {
    // Rbc_assert(!Is_tag_set, "IDS07-J")
    RBC_ENGINE.Rbc_assert(
      RBC_ENGINE.Is_tag_attr_set(RBC_ENGINE.Get_this_pointer(), "tainted", "sanitize_cmd"),
      "IDS07-J");
    return null;
  }
}
