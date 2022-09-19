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

public class DriverManager {
  public static Connection getConnection(String url,
      String user, String password) {
    // Rbc_assert(!Is_init_by_const_str, "MSC03-J")
    RBC_ENGINE.Rbc_assert(
      RBC_ENGINE.Is_init_by_const_str(RBC_ENGINE.Get_arg(3)), "MSC03-J"
    );
    return null;
  }
}
