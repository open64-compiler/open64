//-*-Java-*-

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

// =============================================================================
// =============================================================================
//
// HttpServletRequest.java
// Package: javaee-api [ third party library ]
// 
// Local Git: git@gitlab.com:xc5sz/xc5jfe/mvntest/servlet-api.git
// =============================================================================
// =============================================================================
package javax.servlet.http;

import io.xc5.*;
import java.util.Enumeration;

public interface HttpServletRequest {
  default public String getQueryString() {
    RBC_ENGINE.Model_decl(RBC_ENGINE.Set_tag(RBC_ENGINE.Get_ret(), "tainted"));
    RBC_ENGINE.Model_decl(RBC_ENGINE.Set_tag(RBC_ENGINE.Get_ret(), "sensitive"));
    return null;
  }
  default public Enumeration<String> getHeaders(String var1) {
    RBC_ENGINE.Model_decl(RBC_ENGINE.Set_tag(RBC_ENGINE.Get_ret(), "tainted"));
    RBC_ENGINE.Model_decl(RBC_ENGINE.Set_tag(RBC_ENGINE.Get_ret(), "sensitive"));
    return null;
  }
  default public String getHeader(String var1) {
    RBC_ENGINE.Model_decl(RBC_ENGINE.Set_tag(RBC_ENGINE.Get_ret(), "tainted"));
    RBC_ENGINE.Model_decl(RBC_ENGINE.Set_tag(RBC_ENGINE.Get_ret(), "sensitive"));
    return null;
  }
  default public Cookie[] getCookies() {
    RBC_ENGINE.Model_decl(RBC_ENGINE.Set_tag(RBC_ENGINE.Get_ret(), "tainted"));
    RBC_ENGINE.Model_decl(RBC_ENGINE.Set_tag(RBC_ENGINE.Get_ret(), "sensitive"));
    return null;
  }

  default public Enumeration<String> getHeaderNames() {
    RBC_ENGINE.Model_decl(RBC_ENGINE.Set_tag(RBC_ENGINE.Get_ret(), "tainted"));
    RBC_ENGINE.Model_decl(RBC_ENGINE.Set_tag(RBC_ENGINE.Get_ret(), "sensitive"));
    return null;
  }
}
