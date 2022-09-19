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

package javax.xml.parsers;

import io.xc5.RBC_ENGINE;
import java.io.InputStream;
import org.xml.sax.InputSource;
import org.xml.sax.helpers.DefaultHandler;
import org.xml.sax.XMLReader;

public final class SAXParser {
  public XMLReader getXMLReader() {
    RBC_ENGINE.Model_decl(RBC_ENGINE.Fsm_use("ids16_ids17"));
    RBC_ENGINE.Set_implicit_assign(RBC_ENGINE.Get_ret(),
                                   RBC_ENGINE.Get_this_pointer()
    );
    return null;
  }

  public void parse( InputStream var1, DefaultHandler var2) {
    RBC_ENGINE.Model_decl(RBC_ENGINE.Fsm_use("ids16_ids17"));
  }
  public void parse( InputSource var1, DefaultHandler var2) {
    RBC_ENGINE.Model_decl(RBC_ENGINE.Fsm_use("ids16_ids17"));
  }
}
