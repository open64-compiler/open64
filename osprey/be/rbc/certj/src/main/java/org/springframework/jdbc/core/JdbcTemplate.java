//-*-Java-*-

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

// =============================================================================
// =============================================================================
//
// JdbcTemplate.java
// Package: springframework-jdbc:v4.1.9 [third party library]
// Official Git: 
// https://github.com/spring-projects/spring-framework/tree/v4.1.9.RELEASE/
// Local Git:
// git@gitlab.com:xc5sz/xc5jfe/mvntest/spring-framework.git(branch 4.1.9)
//
// =============================================================================
// =============================================================================
package org.springframework.jdbc.core;
import io.xc5.RBC_ENGINE;
import java.util.List;
import java.util.Map;
import org.springframework.jdbc.support.rowset.SqlRowSet;

public class JdbcTemplate {
  public List<Map<String, Object>> queryForList(String sql) {
    // Rbc_assert(!Is_tag_set, "IDS00-J")
    RBC_ENGINE.Rbc_assert(
      RBC_ENGINE.Is_tag_attr_set(RBC_ENGINE.Get_arg(1), "tainted", "sanitize_sql"),
      "IDS00-J");
    return null; 
  }

  public int queryForInt(String sql) {
    // Rbc_assert(!Is_tag_set, "IDS00-J")
    RBC_ENGINE.Rbc_assert(
      RBC_ENGINE.Is_tag_attr_set(RBC_ENGINE.Get_arg(1), "tainted", "sanitize_sql"),
      "IDS00-J");
    return 0; 
  }

  public long queryForLong(String sql) {
    // Rbc_assert(!Is_tag_set, "IDS00-J")
    RBC_ENGINE.Rbc_assert(
      RBC_ENGINE.Is_tag_attr_set(RBC_ENGINE.Get_arg(1), "tainted", "sanitize_sql"),
      "IDS00-J");
    return 0; 
  }

  public Map<String, Object> queryForMap(String sql) {
    // Rbc_assert(!Is_tag_set, "IDS00-J")
    RBC_ENGINE.Rbc_assert(
      RBC_ENGINE.Is_tag_attr_set(RBC_ENGINE.Get_arg(1), "tainted", "sanitize_sql"),
      "IDS00-J");
    return null; 
  }

  public <T> T queryForObject(String sql, Object[] args, Class<T> requiredType) {
    // Rbc_assert(!Is_tag_set, "IDS00-J")
    RBC_ENGINE.Rbc_assert(
      RBC_ENGINE.Is_tag_attr_set(RBC_ENGINE.Get_arg(1), "tainted", "sanitize_sql"),
      "IDS00-J");
    return null; 
  }

  public SqlRowSet queryForRowSet(String sql) {
    // Rbc_assert(!Is_tag_set, "IDS00-J")
    RBC_ENGINE.Rbc_assert(
      RBC_ENGINE.Is_tag_attr_set(RBC_ENGINE.Get_arg(1), "tainted", "sanitize_sql"),
      "IDS00-J");
    return null; 
  }

  public <T> List<T> query(String sql, RowMapper<T> rowMapper) {
    // Rbc_assert(!Is_tag_set, "IDS00-J")
    RBC_ENGINE.Rbc_assert(
      RBC_ENGINE.Is_tag_attr_set(RBC_ENGINE.Get_arg(1), "tainted", "sanitize_sql"),
      "IDS00-J");
    return null; 
  }

  public void execute(final String sql) {
    // Rbc_assert(!Is_tag_set, "IDS00-J")
    RBC_ENGINE.Rbc_assert(
      RBC_ENGINE.Is_tag_attr_set(RBC_ENGINE.Get_arg(1), "tainted", "sanitize_sql"),
      "IDS00-J");
    return; 
  }

  public int[] batchUpdate(final String... sql) {
    // Rbc_assert(!Is_tag_set, "IDS00-J")
    RBC_ENGINE.Rbc_assert(
      RBC_ENGINE.Is_tag_attr_set(RBC_ENGINE.Get_arg(1), "tainted", "sanitize_sql"),
      "IDS00-J");
    return null; 
  }
}
