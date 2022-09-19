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
import io.xc5.RBC_ENGINE;

public interface Statement {
  default public ResultSet executeQuery(String sql) {
    // Rbc_assert(!Is_tag_set, "IDS00-J")
    RBC_ENGINE.Rbc_assert(
      RBC_ENGINE.Is_tag_attr_set(RBC_ENGINE.Get_arg(1), "tainted", "sanitize_sql"),
      "IDS00-J");
    return null; 
  }

  default public int executeUpdate(String var1) {
    // Rbc_assert(!Is_tag_set, "IDS00-J")
    RBC_ENGINE.Rbc_assert(
      RBC_ENGINE.Is_tag_attr_set(RBC_ENGINE.Get_arg(1), "tainted", "sanitize_sql"),
      "IDS00-J");
    return 0;
  }

  default public int executeUpdate(String var1, int var2)
  {
    // Rbc_assert(!Is_tag_set, "IDS00-J")
    RBC_ENGINE.Rbc_assert(
      RBC_ENGINE.Is_tag_attr_set(RBC_ENGINE.Get_arg(1), "tainted", "sanitize_sql"),
      "IDS00-J");
    return 0;
  }

  default public int executeUpdate(String var1, int[] var2)
  {
    // Rbc_assert(!Is_tag_set, "IDS00-J")
    RBC_ENGINE.Rbc_assert(
      RBC_ENGINE.Is_tag_attr_set(RBC_ENGINE.Get_arg(1), "tainted", "sanitize_sql"),
      "IDS00-J");
    return 0;
  }

  default public int executeUpdate(String var1, String[] var2)
  {
    // Rbc_assert(!Is_tag_set, "IDS00-J")
    RBC_ENGINE.Rbc_assert(
      RBC_ENGINE.Is_tag_attr_set(RBC_ENGINE.Get_arg(1), "tainted", "sanitize_sql"),
      "IDS00-J");
    return 0;
  }

  default public boolean execute(String var1)
  {
    // Rbc_assert(!Is_tag_set, "IDS00-J")
    RBC_ENGINE.Rbc_assert(
      RBC_ENGINE.Is_tag_attr_set(RBC_ENGINE.Get_arg(1), "tainted", "sanitize_sql"),
      "IDS00-J");
    return false;
  }

  default public boolean execute(String var1, int var2)
  {
    // Rbc_assert(!Is_tag_set, "IDS00-J")
    RBC_ENGINE.Rbc_assert(
      RBC_ENGINE.Is_tag_attr_set(RBC_ENGINE.Get_arg(1), "tainted", "sanitize_sql"),
      "IDS00-J");
    return false;
  }
  default public boolean execute(String var1, int[] var2)
  {
    // Rbc_assert(!Is_tag_set, "IDS00-J")
    RBC_ENGINE.Rbc_assert(
      RBC_ENGINE.Is_tag_attr_set(RBC_ENGINE.Get_arg(1), "tainted", "sanitize_sql"),
      "IDS00-J");
    return false;
  }

  default public boolean execute(String var1, String[] var2)
  {
    // Rbc_assert(!Is_tag_set, "IDS00-J")
    RBC_ENGINE.Rbc_assert(
      RBC_ENGINE.Is_tag_attr_set(RBC_ENGINE.Get_arg(1), "tainted", "sanitize_sql"),
      "IDS00-J");
    return false;
  }

  default void addBatch(String var1) {
    // Rbc_assert(!Is_tag_set, "IDS00-J")
    RBC_ENGINE.Rbc_assert(
      RBC_ENGINE.Is_tag_attr_set(RBC_ENGINE.Get_arg(1), "tainted", "sanitize_sql"),
      "IDS00-J");
    return;
  }
}
