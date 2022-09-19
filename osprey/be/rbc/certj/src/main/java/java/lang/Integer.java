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

public final class Integer
{
    // [IDS16]
    public static int parseInt(String s) {
      RBC_ENGINE.Model_decl(RBC_ENGINE.Func_performs_sanitize());
      return 0;    
    }
    public static int parseInt(String s, int radix) {
      RBC_ENGINE.Model_decl(RBC_ENGINE.Func_performs_sanitize());
      return 0;    
    }
    public static int parseUnsignedInt(String s) {
      RBC_ENGINE.Model_decl(RBC_ENGINE.Func_performs_sanitize());
      return 0;    
    }
    public static int parseUnsignedInt(String s, int radix) {
      RBC_ENGINE.Model_decl(RBC_ENGINE.Func_performs_sanitize());
      return 0;    
    }
    public static Integer valueOf(String s) {
      RBC_ENGINE.Model_decl(RBC_ENGINE.Copy_tag(RBC_ENGINE.Get_ret(), RBC_ENGINE.Get_arg(1)));
      return null;
    }
    public static Integer valueOf(int i) {
      RBC_ENGINE.Model_decl(RBC_ENGINE.Copy_tag(RBC_ENGINE.Get_ret(), RBC_ENGINE.Get_arg(1)));
      return null;
    }
    public static Integer	valueOf(String s, int radix) {
      RBC_ENGINE.Model_decl(RBC_ENGINE.Copy_tag(RBC_ENGINE.Get_ret(), RBC_ENGINE.Get_arg(1)));
      return null;
    }
    public int intValue() {
      RBC_ENGINE.Model_decl(RBC_ENGINE.Copy_tag(RBC_ENGINE.Get_ret(), RBC_ENGINE.Get_this_pointer()));
      return 0;
    }
    public byte byteValue() {
      RBC_ENGINE.Model_decl(RBC_ENGINE.Copy_tag(RBC_ENGINE.Get_ret(), RBC_ENGINE.Get_this_pointer()));
      return 0;
    }
    public double doubleValue() {
      RBC_ENGINE.Model_decl(RBC_ENGINE.Copy_tag(RBC_ENGINE.Get_ret(), RBC_ENGINE.Get_this_pointer()));
      return 0;
    }
    public float floatValue() {
      RBC_ENGINE.Model_decl(RBC_ENGINE.Copy_tag(RBC_ENGINE.Get_ret(), RBC_ENGINE.Get_this_pointer()));
      return 0;
    }
    public long longValue() {
      RBC_ENGINE.Model_decl(RBC_ENGINE.Copy_tag(RBC_ENGINE.Get_ret(), RBC_ENGINE.Get_this_pointer()));
      return 0;
    }
    public short shortValue() {
      RBC_ENGINE.Model_decl(RBC_ENGINE.Copy_tag(RBC_ENGINE.Get_ret(), RBC_ENGINE.Get_this_pointer()));
      return 0;
    }
    
    
}
