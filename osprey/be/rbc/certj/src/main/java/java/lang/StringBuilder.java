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

public final class StringBuilder {
    public StringBuilder append(String str) {
        // this = this + str
        RBC_ENGINE.Model_decl(
          RBC_ENGINE.Set_implicit_assign(RBC_ENGINE.Get_this_pointer(),
                                         RBC_ENGINE.Get_arg(1))
        );
        RBC_ENGINE.Model_decl(
          RBC_ENGINE.Set_implicit_assign(RBC_ENGINE.Get_this_pointer(),
                                         RBC_ENGINE.Get_this_pointer())
        );
        // ret = this + str
        RBC_ENGINE.Model_decl(
          RBC_ENGINE.Set_implicit_assign(RBC_ENGINE.Get_ret(),
                                         RBC_ENGINE.Get_this_pointer())
        );
        RBC_ENGINE.Model_decl(
          RBC_ENGINE.Set_implicit_assign(RBC_ENGINE.Get_ret(),
                                         RBC_ENGINE.Get_arg(1))
        );

        RBC_ENGINE.Model_decl(RBC_ENGINE.Set_parm_mod(RBC_ENGINE.Get_this_pointer()));
        RBC_ENGINE.Model_decl(RBC_ENGINE.Merge_tag(RBC_ENGINE.Get_this_pointer(),
                                                   RBC_ENGINE.Get_this_pointer(),
                                                   RBC_ENGINE.Get_arg(1)));
        RBC_ENGINE.Model_decl(RBC_ENGINE.Merge_tag(RBC_ENGINE.Get_ret(),
                                                   RBC_ENGINE.Get_this_pointer(),
                                                   RBC_ENGINE.Get_arg(1)));

        return null;
    }

    public StringBuilder append(Object obj) {
      // this = this + obj
      RBC_ENGINE.Model_decl(
        RBC_ENGINE.Set_implicit_assign(RBC_ENGINE.Get_this_pointer(),
                                       RBC_ENGINE.Get_arg(1))
      );
      RBC_ENGINE.Model_decl(
        RBC_ENGINE.Set_implicit_assign(RBC_ENGINE.Get_this_pointer(),
                                       RBC_ENGINE.Get_this_pointer())
      );
      // ret = this + obj
      RBC_ENGINE.Model_decl(
        RBC_ENGINE.Set_implicit_assign(RBC_ENGINE.Get_ret(),
                                       RBC_ENGINE.Get_this_pointer())
      );
      RBC_ENGINE.Model_decl(
        RBC_ENGINE.Set_implicit_assign(RBC_ENGINE.Get_ret(),
                                       RBC_ENGINE.Get_arg(1))
      );

      RBC_ENGINE.Model_decl(RBC_ENGINE.Set_parm_mod(RBC_ENGINE.Get_this_pointer()));
      RBC_ENGINE.Model_decl(RBC_ENGINE.Merge_tag(RBC_ENGINE.Get_this_pointer(),
                                                 RBC_ENGINE.Get_this_pointer(),
                                                 RBC_ENGINE.Get_arg(1)));
      RBC_ENGINE.Model_decl(RBC_ENGINE.Merge_tag(RBC_ENGINE.Get_ret(),
                                                 RBC_ENGINE.Get_this_pointer(),
                                                 RBC_ENGINE.Get_arg(1)));
      return null;
    }

    public StringBuilder append(int i) {
        // this = this + i
        RBC_ENGINE.Model_decl(
          RBC_ENGINE.Set_implicit_assign(RBC_ENGINE.Get_this_pointer(),
                                         RBC_ENGINE.Get_arg(1))
        );
        RBC_ENGINE.Model_decl(
          RBC_ENGINE.Set_implicit_assign(RBC_ENGINE.Get_this_pointer(),
                                         RBC_ENGINE.Get_this_pointer())
        );
        // ret = this + i
        RBC_ENGINE.Model_decl(
          RBC_ENGINE.Set_implicit_assign(RBC_ENGINE.Get_ret(),
                                         RBC_ENGINE.Get_this_pointer())
        );
        RBC_ENGINE.Model_decl(
          RBC_ENGINE.Set_implicit_assign(RBC_ENGINE.Get_ret(),
                                         RBC_ENGINE.Get_arg(1))
        );

        RBC_ENGINE.Model_decl(RBC_ENGINE.Set_parm_mod(RBC_ENGINE.Get_this_pointer()));
        RBC_ENGINE.Model_decl(RBC_ENGINE.Merge_tag(RBC_ENGINE.Get_this_pointer(),
                                                   RBC_ENGINE.Get_this_pointer(),
                                                   RBC_ENGINE.Get_arg(1)));
        RBC_ENGINE.Model_decl(RBC_ENGINE.Merge_tag(RBC_ENGINE.Get_ret(),
                                                   RBC_ENGINE.Get_this_pointer(),
                                                   RBC_ENGINE.Get_arg(1)));
        return null;
    }

    public String toString() {
        RBC_ENGINE.Model_decl(
          RBC_ENGINE.Set_implicit_assign(RBC_ENGINE.Get_ret(),
                                         RBC_ENGINE.Get_this_pointer())
        );
        RBC_ENGINE.Model_decl(RBC_ENGINE.Copy_tag(RBC_ENGINE.Get_ret(), RBC_ENGINE.Get_this_pointer()));
        return null;
    }
}
