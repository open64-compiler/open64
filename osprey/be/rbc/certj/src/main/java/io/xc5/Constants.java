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

package io.xc5;

import java.util.Arrays;

public class Constants {
  public static class XVSA_ERR_CODE {
    public static final int XVSA_MEMCPY_OVERFLOW = 1;
    public static final int XVSA_ATEXIT_MAY_NOT_RETURN = 2;
    public static final int XVSA_ATEXIT_MAY_ENTER_RECURSION = 3;
    public static final int XVSA_MAY_BE_SYMBOLIC_LINK = 4;
    public static final int XVSA_FILE_WRITABLE = 5;
    public static final int XVSA_CHECK_BEFORE_USE = 6;
    public static final int XVSA_SEED_PSEUDORANDOM = 7;
    public static final int XVSA_VERIFY_SL_RETURN = 8;
    public static final int XVSA_VERIFY_POS_RETURN = 9;
    public static final int XVSA_NO_SYSTEM_CALL = 10;
    public static final int XVSA_READLINK_SIZE = 11;
    public static final int XVSA_NOT_ENOUGH_MEMORY_MALLOCED = 12;
    public static final int XVSA_ASCTIME_PARAMETER = 13;
    public static final int XVSA_PUTENV_PARAMETER = 14;
    public static final int XVSA_SIGHDL_ASYN_SAFE = 15;
    public static final int XVSA_FILE_NOT_OPEN = 16;
    public static final int XVSA_GETENV_RESULT = 17;
    public static final int XVSA_STRNCPY_OVERFLOW = 18;
    public static final int XVSA_STRCPY_OVERFLOW = 19;
  };

  /**
   *
   *     SAMPLE OF RBC_ASSERT
   *
   *
   RBC_ENGINE.Rbc_assert(false,
   Constants.XVSA_ERR_CODE.XVSA_CHECK_BEFORE_USE,
   "FileInputStream.C1Ev(String f)",
   "npd in Arrays.equals",
   RBC_ENGINE.RULE_CATEGORY.CERT,
   "try to eliminate this", 0);
   */
}
