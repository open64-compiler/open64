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

package java.io;

import io.xc5.RBC_ENGINE;

public class PrintStream {

    // [cert fio09]
    // FIXME: need to update to value range api
    public void write(int b) {
        /*
        RBC_ENGINE.Rbc_assert(
          RBC_ENGINE.Pre_rsc_check_value(
            RBC_ENGINE.Get_arg(1), "le", 255, null, 0, 0) & RBC_ENGINE.Pre_rsc_check_value(RBC_ENGINE.Get_arg(1), "be", 0, null, 0, 0), 1,
                "", "", 0, "", 0);
        */
    }
    public PrintStream(OutputStream o) {
      RBC_ENGINE.Model_decl(RBC_ENGINE.Fsm_use("fio14"));
    }
    public void close() {
      RBC_ENGINE.Model_decl(RBC_ENGINE.Fsm_use("fio14"));
    }
}
