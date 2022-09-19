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

package java.util.zip;

import io.xc5.Constants;
import io.xc5.RBC_ENGINE;

public class ZipInputStream {

  public int read(byte[] b, int off, int len) {
    // Rbc_assert(Is_compression_extraction_safe, "IDS04-J")
    RBC_ENGINE.Rbc_assert(
      RBC_ENGINE.Is_compression_extraction_safe(RBC_ENGINE.Get_ret()), "IDS04-J"
    );
    return 0;
  }
}
