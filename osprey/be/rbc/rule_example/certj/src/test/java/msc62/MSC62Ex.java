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
//
// MSC62-J: Store passwords using a hash function
// regUser1: no salt/no secure hash
// regUser2: passwd stored by String
//
//
// =============================================================================

import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.security.SecureRandom;

public class MSC62Ex {
  private void regUser1(String userName, byte[] passwd, String regType) {
    try {
      String salt = genSalt();
      String combPasswd = salt + passwd;
      byte[] secureHash = secureHash(combPasswd.getBytes());
      byte[] noSaltPasswd = secureHash(passwd);
      byte[] unsecureHash = unSecureHash(combPasswd.getBytes());
      if(regType.equals("NO_SALT_HASH")) {
        saveUser(userName, passwd);             // MSC62-J, no salt, no hash
      } else if(regType.equals("NO_SALT")) {
        saveUser(userName, noSaltPasswd);       // MSC62-J no salt
      } else if(regType.equals("UNSECURE_HASH")){
        saveUser(userName, unsecureHash);       // MSC62-J no secure hash
      } else {
        saveUser(userName, secureHash);         // good
      }
    } catch (NoSuchAlgorithmException e) {
      e.printStackTrace();
    }
  }

  private void regUser2(String userName, String passwd) {
    try {
      String combPasswd = passwd + genSalt();
      byte[] secureHash = secureHash(combPasswd.getBytes());
      saveUser(userName, secureHash);
    } catch (NoSuchAlgorithmException e) {
      e.printStackTrace();
    }
  }


  private void saveUser(String userName, byte[] passwd) {
    // store the user info
  }

  private String genSalt()
  {
    SecureRandom rand = new SecureRandom();
    byte[] salt = new byte[16];
    rand.nextBytes(salt);   // salt used
    return new String(salt);
  }

  private byte[] unSecureHash(byte [] passwd) throws NoSuchAlgorithmException{
    MessageDigest msgDigest = MessageDigest.getInstance("MD5");  // not secure hash algorithm
    return msgDigest.digest(passwd);
  }

  private byte[] secureHash(byte [] passwd) throws NoSuchAlgorithmException{
    MessageDigest msgDigest = MessageDigest.getInstance("SHA256");  // secure hash algorithm
    return msgDigest.digest(passwd);
  }

  private void testRegUser(String userName, String passwd, String regType) {
    regUser1(userName, passwd.getBytes(), regType);
    regUser2(userName, passwd);  // MSC62-J passwd is String type
  }

}
