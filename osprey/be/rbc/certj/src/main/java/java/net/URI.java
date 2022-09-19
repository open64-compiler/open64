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

package java.net;

import io.xc5.Constants;
import io.xc5.RBC_ENGINE;

public final class URI {
  public URI(String str) {
    RBC_ENGINE.Model_decl(RBC_ENGINE.Copy_tag(RBC_ENGINE.Get_this_pointer(), RBC_ENGINE.Get_arg(1)));
  }

  public URI(String scheme,
            String userInfo, String host, int port,
            String path, String query, String fragment) {
    RBC_ENGINE.Model_decl(RBC_ENGINE.Copy_tag(RBC_ENGINE.Get_this_pointer(), RBC_ENGINE.Get_arg(5)));
  }

  public URI(String scheme,
              String authority,
              String path, String query, String fragment) {
    RBC_ENGINE.Model_decl(RBC_ENGINE.Copy_tag(RBC_ENGINE.Get_this_pointer(), RBC_ENGINE.Get_arg(3)));
  }

  public URI(String scheme, String host, String path, String fragment) {
    RBC_ENGINE.Model_decl(RBC_ENGINE.Copy_tag(RBC_ENGINE.Get_this_pointer(), RBC_ENGINE.Get_arg(3)));
  }
}
