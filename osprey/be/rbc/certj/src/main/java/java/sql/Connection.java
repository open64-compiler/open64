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

package java.sql;
import java.lang.String;
import java.sql.*;
import io.xc5.RBC_ENGINE;

public interface Connection {
  default public PreparedStatement prepareStatement(String sql) {
    RBC_ENGINE.Model_decl(RBC_ENGINE.Copy_tag(RBC_ENGINE.Get_ret(), RBC_ENGINE.Get_arg(1)));
    return null;
  }

  default public CallableStatement prepareCall(String var1) {
    RBC_ENGINE.Model_decl(RBC_ENGINE.Copy_tag(RBC_ENGINE.Get_ret(), RBC_ENGINE.Get_arg(1)));
    return null;
  }

  default public CallableStatement prepareCall(String sql, int resultSetType,
                              int resultSetConcurrency) {
    RBC_ENGINE.Model_decl(RBC_ENGINE.Copy_tag(RBC_ENGINE.Get_ret(), RBC_ENGINE.Get_arg(1)));
    return null;
  }

  default public PreparedStatement prepareStatement(String sql, int resultSetType,
                                       int resultSetConcurrency) {
    RBC_ENGINE.Model_decl(RBC_ENGINE.Copy_tag(RBC_ENGINE.Get_ret(), RBC_ENGINE.Get_arg(1)));
    return null;
  }

  default public PreparedStatement prepareStatement(String var1, int var2, int var3, int var4) {
    RBC_ENGINE.Model_decl(RBC_ENGINE.Copy_tag(RBC_ENGINE.Get_ret(), RBC_ENGINE.Get_arg(1)));
    return null;
  }

  default public CallableStatement prepareCall(String var1, int var2, int var3, int var4) {
    RBC_ENGINE.Model_decl(RBC_ENGINE.Copy_tag(RBC_ENGINE.Get_ret(), RBC_ENGINE.Get_arg(1)));
    return null;
  }

  default public PreparedStatement prepareStatement(String var1, int var2) {
    RBC_ENGINE.Model_decl(RBC_ENGINE.Copy_tag(RBC_ENGINE.Get_ret(), RBC_ENGINE.Get_arg(1)));
    return null;
  }

  default public PreparedStatement prepareStatement(String var1, int[] var2) {
    RBC_ENGINE.Model_decl(RBC_ENGINE.Copy_tag(RBC_ENGINE.Get_ret(), RBC_ENGINE.Get_arg(1)));
    return null;
  }

  default public PreparedStatement prepareStatement(String var1, String[] var2) {
    RBC_ENGINE.Model_decl(RBC_ENGINE.Copy_tag(RBC_ENGINE.Get_ret(), RBC_ENGINE.Get_arg(1)));
    return null;
  }
}
