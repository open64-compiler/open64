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

public class IDS16FSM {
  public static void ids16() {
    RBC_ENGINE.Model_decl(RBC_ENGINE.Fsm_build_begin("ids16_ids17"));
    RBC_ENGINE.Model_decl(RBC_ENGINE.Fsm_new_start_state("init"));
    RBC_ENGINE.Model_decl(RBC_ENGINE.Fsm_new_final_state("end"));
    // void IDS16Ex::getXMLStr()
    RBC_ENGINE.Model_decl(
      RBC_ENGINE.Fsm_add_transition(
        "init", "_ZN7IDS16Ex9getXMLStrEJPN4java4lang6StringEv",
        RBC_ENGINE.Get_ret(),
        1,
        "xmlInput",
        "", 132
      )
    );
    // void javax::xml::parsers::SAXParser::parse(org::xml::sax::InputSource*, org::xml::sax::helpers::DefaultHandler*)
    RBC_ENGINE.Model_decl(
      RBC_ENGINE.Fsm_add_transition(
        "xmlInput", "_ZN5javax3xml7parsers9SAXParser5parseEJvPN3org3xml3sax11InputSourceEPNS5_7helpers14DefaultHandlerE",
        RBC_ENGINE.Get_arg(1),
        1, "sanitized", "", 101
      )
    );

    // java::io::FilterOutputStream::write(JArray<char>*)
    RBC_ENGINE.Model_decl(
      RBC_ENGINE.Fsm_add_transition(
        "sanitized", "_ZN4java2io18FilterOutputStream5writeEJvP6JArrayIcE",
        RBC_ENGINE.Get_arg(1),
        1, "end", "", 133
      )
    );

    // java::io::FilterOutputStream::write(JArray<char>*)
    RBC_ENGINE.Model_decl(
      RBC_ENGINE.Fsm_add_transition(
        "xmlInput", "_ZN4java2io18FilterOutputStream5writeEJvP6JArrayIcE",
        RBC_ENGINE.Get_arg(1),
        RBC_ENGINE.Not(RBC_ENGINE.Is_tag_attr_set(RBC_ENGINE.Get_arg(1), "tainted", "sanitize_xml")),
        "end",
        "IDS16-J", 125
      )
    );
    RBC_ENGINE.Model_decl(RBC_ENGINE.Fsm_build_end("ids16_ids17"));
  }
}
