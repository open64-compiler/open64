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
// HttpSession.java
// Package: javaee-api [ third party library ]
// 
// Local Git: git@gitlab.com:xc5sz/xc5jfe/mvntest/servlet-api.git
// =============================================================================
// =============================================================================
package javax.servlet.http;

import io.xc5.*;
public interface HttpSession {
  public default void setAttribute(String var1, Object var2)
  {
    // Rbc_assert(!Is_tag_attr_set, "IDS15-J")
    RBC_ENGINE.Rbc_assert(
      RBC_ENGINE.Is_tag_attr_set(RBC_ENGINE.Get_arg(1), "sensitive", "sanitize_session"),
      "IDS15-J");
    RBC_ENGINE.Rbc_assert(
      RBC_ENGINE.Is_tag_attr_set(RBC_ENGINE.Get_arg(2), "sensitive", "sanitize_session"),
      "IDS15-J");
  }
  public default void putValue(String var1, Object var2)
  {
    // Rbc_assert(!Is_tag_attr_set, "IDS15-J")
    RBC_ENGINE.Rbc_assert(
      RBC_ENGINE.Is_tag_attr_set(RBC_ENGINE.Get_arg(1), "sensitive", "sanitize_session"),
      "IDS15-J");
    RBC_ENGINE.Rbc_assert(
      RBC_ENGINE.Is_tag_attr_set(RBC_ENGINE.Get_arg(2), "sensitive", "sanitize_session"),
      "IDS15-J");
  }
}
