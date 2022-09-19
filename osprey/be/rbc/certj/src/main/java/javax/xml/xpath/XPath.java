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

package javax.xml.xpath;

import io.xc5.RBC_ENGINE;
import org.xml.sax.InputSource;

public interface XPath {

  default public XPathExpression compile(String var1)
  {
    // Rbc_assert(!Is_tag_set, "IDS53-J")
    RBC_ENGINE.Rbc_assert(
      RBC_ENGINE.Not(RBC_ENGINE.Is_tag_set(RBC_ENGINE.Get_arg(1), "tainted")),
      "IDS53-J");
    return null;
  }
  default public String evaluate(String var1, Object var2){
        // Rbc_assert(!Is_tag_set, "IDS53-J")
    RBC_ENGINE.Rbc_assert(
      RBC_ENGINE.Not(RBC_ENGINE.Is_tag_set(RBC_ENGINE.Get_arg(1), "tainted")),
      "IDS53-J");
    return null;
  }

  default public String evaluate(String var1, InputSource var2) {
    // Rbc_assert(!Is_tag_set, "IDS53-J")
    RBC_ENGINE.Rbc_assert(
      RBC_ENGINE.Not(RBC_ENGINE.Is_tag_set(RBC_ENGINE.Get_arg(1), "tainted")),
      "IDS53-J");
    return null;
  }
}
