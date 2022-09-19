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

package javax.naming.directory;
import io.xc5.RBC_ENGINE;
import javax.naming.NamingEnumeration;
import javax.naming.Name;

public class InitialDirContext {
  public NamingEnumeration<SearchResult> search(String var1, String filter, SearchControls var3) {
    // Rbc_assert(!Is_tag_set, "IDS54-J")
    RBC_ENGINE.Rbc_assert( 
      RBC_ENGINE.Not(RBC_ENGINE.Is_tag_set(RBC_ENGINE.Get_arg(2), "tainted")), 
      "IDS54-J");
    return null;
  }

  public NamingEnumeration<SearchResult> search(Name var1, String filter, SearchControls var3) {
    // Rbc_assert(!Is_tag_set, "IDS54-J")
    RBC_ENGINE.Rbc_assert( 
      RBC_ENGINE.Not(RBC_ENGINE.Is_tag_set(RBC_ENGINE.Get_arg(2), "tainted")), 
      "IDS54-J");
    return null;
  }

  public NamingEnumeration<SearchResult> search(String var1, String filterExpr, Object[] var3, SearchControls var4) {
    // Rbc_assert(!Is_tag_set, "IDS54-J")
    RBC_ENGINE.Rbc_assert( 
      RBC_ENGINE.Not(RBC_ENGINE.Is_tag_set(RBC_ENGINE.Get_arg(2), "tainted")), 
      "IDS54-J");
    return null;
  }

  public NamingEnumeration<SearchResult> search(Name var1, String filterExpr, Object[] var3, SearchControls var4) {
    // Rbc_assert(!Is_tag_set, "IDS54-J")
    RBC_ENGINE.Rbc_assert( 
      RBC_ENGINE.Not(RBC_ENGINE.Is_tag_set(RBC_ENGINE.Get_arg(2), "tainted")), 
      "IDS54-J");
    return null;
  }
}
