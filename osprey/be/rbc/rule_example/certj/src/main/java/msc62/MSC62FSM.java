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

import io.xc5.RBC_ENGINE;

public class MSC62FSM {
  public static void msc62() {
    RBC_ENGINE.Model_decl(RBC_ENGINE.Fsm_build_begin("msc62"));
    RBC_ENGINE.Model_decl(RBC_ENGINE.Fsm_new_start_state("init"));
    RBC_ENGINE.Model_decl(RBC_ENGINE.Fsm_new_final_state("end"));

    RBC_ENGINE.Model_decl(
      RBC_ENGINE.Fsm_add_transition(
        "init", "_ZN7MSC62Ex8regUser1EJvPN4java4lang6StringEP6JArrayIcES3_",
        RBC_ENGINE.Get_arg(2),
        1,
        "inputPasswd",
        "", 129
      )
    );
    RBC_ENGINE.Model_decl(
      RBC_ENGINE.Fsm_add_transition(
        "init", "_ZN7MSC62Ex8regUser2EJvPN4java4lang6StringES3_",
        RBC_ENGINE.Get_arg(2),
        1,
        "inputPasswd",
        "", 129
      )
    );
    RBC_ENGINE.Model_decl(
        RBC_ENGINE.Fsm_add_transition(
          "inputPasswd", "_ZN7MSC62Ex8saveUserEJvPN4java4lang6StringEP6JArrayIcE",
          RBC_ENGINE.Get_arg(2),
          RBC_ENGINE.And(
            RBC_ENGINE.Is_tag_set(RBC_ENGINE.Get_arg(2), "salt"),
            RBC_ENGINE.Is_tag_attr_set(RBC_ENGINE.Get_arg(2), "sensitive", "secure_hash")),
          "end", "", 130
        )
      );

    RBC_ENGINE.Model_decl(
        RBC_ENGINE.Fsm_add_transition(
          "inputPasswd", "_ZN7MSC62Ex8saveUserEJvPN4java4lang6StringEP6JArrayIcE",
          RBC_ENGINE.Get_arg(2),
          RBC_ENGINE.Not(
            RBC_ENGINE.And(
              RBC_ENGINE.Is_tag_set(RBC_ENGINE.Get_arg(2), "salt"),
              RBC_ENGINE.Is_tag_attr_set(RBC_ENGINE.Get_arg(2), "sensitive", "secure_hash"))),
          "unsafeStore", "MSC62-J", 131
        )
    );
    RBC_ENGINE.Model_decl(RBC_ENGINE.Fsm_build_end("msc62"));
  }
}
