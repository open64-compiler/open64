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

package java.util;
import java.io.InputStream;
import io.xc5.RBC_ENGINE;

public class Properties {
  public String getProperty(String key, String defaultValue) {
    RBC_ENGINE.Rbc_assert(1, "dummy");
    return null;
  }

  public void load(InputStream inStream) {
    RBC_ENGINE.Rbc_assert(1, "dummy");
    return;
  }
}
