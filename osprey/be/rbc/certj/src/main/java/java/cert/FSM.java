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

package java.cert;

import io.xc5.RBC_ENGINE;

public class FSM {
  public static void fio02() {
    RBC_ENGINE.Model_decl(RBC_ENGINE.Fsm_build_begin("fio02"));
    RBC_ENGINE.Model_decl(RBC_ENGINE.Fsm_new_start_state("start"));
    RBC_ENGINE.Model_decl(RBC_ENGINE.Fsm_new_final_state("accept"));
    RBC_ENGINE.Model_decl(
      RBC_ENGINE.Fsm_add_transition(
        "start", "_ZN4java2io4FileC1EPNS_4lang6StringE", 
        RBC_ENGINE.Get_this_pointer(),
        1, "open", "", 86
      )
    );
    RBC_ENGINE.Model_decl(
      RBC_ENGINE.Fsm_add_transition(
        "open", "_ZN4java2io4File7delete$EJbv",
        RBC_ENGINE.Get_this_pointer(),
        1, "tmp_drop", "", 87
      )
    );
    RBC_ENGINE.Model_decl(
      RBC_ENGINE.Fsm_add_transition(
        "tmp_drop", "if", null,
        1, "accept", "", 88
      )
    );
    RBC_ENGINE.Model_decl(RBC_ENGINE.Fsm_set_default_action("tmp_drop", "FIO02-J", 70));
    RBC_ENGINE.Model_decl(RBC_ENGINE.Fsm_build_end("fio02"));
  }

  public static void fio05() {
    RBC_ENGINE.Model_decl(RBC_ENGINE.Fsm_build_begin("fio05"));
    RBC_ENGINE.Model_decl(RBC_ENGINE.Fsm_new_start_state("init"));
    RBC_ENGINE.Model_decl(RBC_ENGINE.Fsm_new_final_state("end"));
    RBC_ENGINE.Model_decl(
      RBC_ENGINE.Fsm_add_transition(
        "init", "_ZN4java3nio10CharBuffer4wrapEJPS1_P6JArrayIwE", 
        RBC_ENGINE.Get_ret(),
        // FIXME, condition should be arg 1 is private
        1, "created", "", 89
      )
    );
    RBC_ENGINE.Model_decl(
      RBC_ENGINE.Fsm_add_transition(
        "init", "_ZN4java3nio10CharBuffer8allocateEJPS1_i", 
        RBC_ENGINE.Get_ret(),
        1, "end", "", 90
      )
    );
    RBC_ENGINE.Model_decl(
      RBC_ENGINE.Fsm_add_transition(
        "created", "_ZN4java3nio10CharBuffer9duplicateEJPS1_v", 
        RBC_ENGINE.Get_ret(),
        1, "created", "", 91
      )
    );
    RBC_ENGINE.Model_decl(
      RBC_ENGINE.Fsm_add_transition(
        "created", "_ZN4java3nio10CharBuffer16asReadOnlyBufferEJPS1_v", 
        RBC_ENGINE.Get_ret(),
        1, "copyed", "", 92
      )
    );
    RBC_ENGINE.Model_decl(
      RBC_ENGINE.Fsm_add_transition(
        "created", "return", null,
        // FIXME, condition should be function is public
        1, "end", "FIO05-J", 93
      )
    );
    RBC_ENGINE.Model_decl(RBC_ENGINE.Fsm_build_end("fio05"));
  }


  public static void fio14() {
    RBC_ENGINE.Model_decl(RBC_ENGINE.Fsm_build_begin("fio14"));
    RBC_ENGINE.Model_decl(RBC_ENGINE.Fsm_new_start_state("init"));
    RBC_ENGINE.Model_decl(RBC_ENGINE.Fsm_new_final_state("end"));
    // java::io::PrintStream::PrintStream(java::io::OutputStream*)
    RBC_ENGINE.Model_decl(
      RBC_ENGINE.Fsm_add_transition(
        "init", "_ZN4java2io11PrintStreamC1EPNS0_12OutputStreamE", 
        RBC_ENGINE.Get_this_pointer(),
        1, "open", "", 94
        )
      );
    // void java::io::PrintStream::close()
    RBC_ENGINE.Model_decl(
      RBC_ENGINE.Fsm_add_transition(
        "open", "_ZN4java2io11PrintStream5closeEJvv", 
        RBC_ENGINE.Get_this_pointer(),
        1, "close", "", 95
        )
      );
    // void java::io::PrintStream::close()
    RBC_ENGINE.Model_decl(
      RBC_ENGINE.Fsm_add_transition(
        "close", "_ZN4java2io11PrintStream5closeEJvv",
        RBC_ENGINE.Get_this_pointer(),
        1, "close", "DBF", 96
        )
      );
    // void java::lang::Runtime::halt(int)
    RBC_ENGINE.Model_decl(
      RBC_ENGINE.Fsm_add_transition(
        "open", "_ZN4java4lang7Runtime4haltEJvi", null,
        1, "end", "FIO14-J", 97
        )
      );
    // void java::lang::Runtime::halt(int)
    RBC_ENGINE.Model_decl(
      RBC_ENGINE.Fsm_add_transition(
        "close", "_ZN4java4lang7Runtime4haltEJvi", null,
        1, "end", "", 97
        )
      );
    RBC_ENGINE.Model_decl(RBC_ENGINE.Fsm_set_default_action("open", "FIO14-J", 70));
    RBC_ENGINE.Model_decl(RBC_ENGINE.Fsm_build_end("fio14"));
  }

  public static void ids16_ids17() {
    RBC_ENGINE.Model_decl(RBC_ENGINE.Fsm_build_begin("ids16_ids17"));
    RBC_ENGINE.Model_decl(RBC_ENGINE.Fsm_new_start_state("init"));
    RBC_ENGINE.Model_decl(RBC_ENGINE.Fsm_new_final_state("end"));
    // javax::xml::parsers::SAXParser* javax::xml::parsers::SAXParserFactory::newSAXParser()
    RBC_ENGINE.Model_decl(
      RBC_ENGINE.Fsm_add_transition(
        "init", "_ZN5javax3xml7parsers16SAXParserFactory12newSAXParserEJPNS1_9SAXParserEv", 
        RBC_ENGINE.Get_ret(),
        1, "created", "", 98
        )
      );
    // org::xml::sax::XMLReader* javax::xml::parsers::SAXParser::getXMLReader()
    RBC_ENGINE.Model_decl(
      RBC_ENGINE.Fsm_add_transition(
        "created", "_ZN5javax3xml7parsers9SAXParser12getXMLReaderEJPN3org3xml3sax9XMLReaderEv", 
        RBC_ENGINE.Get_this_pointer(),
        1, "xmlread", "", 99
        )
      );
    // void org::xml::sax::XMLReader::setEntityResolver(org::xml::sax::EntityResolver*)
    RBC_ENGINE.Model_decl(
      RBC_ENGINE.Fsm_add_transition(
        "xmlread", "_ZN3org3xml3sax9XMLReader17setEntityResolverEJvPNS1_14EntityResolverE", 
        RBC_ENGINE.Get_this_pointer(),
        1, "sanitized", "", 100
        )
      );
    // void org::xml::sax::XMLReader::parse(org::xml::sax::InputSource*)
    RBC_ENGINE.Model_decl(
      RBC_ENGINE.Fsm_add_transition(
        "sanitized", "_ZN3org3xml3sax9XMLReader5parseEJvPNS1_11InputSourceE", 
        RBC_ENGINE.Get_this_pointer(),
        1, "end", "", 101
        )
      );
    // void javax::xml::parsers::SAXParser::parse(java::io::InputStream*, org::xml::sax::helpers::DefaultHandler*)
    RBC_ENGINE.Model_decl(
      RBC_ENGINE.Fsm_add_transition(
        "sanitized", "_ZN5javax3xml7parsers9SAXParser5parseEJvPN4java2io11InputStreamEPN3org3xml3sax7helpers14DefaultHandlerE", 
        RBC_ENGINE.Get_this_pointer(),
        1, "end", "", 101
        )
      );
    // void org::xml::sax::XMLReader::parse(org::xml::sax::InputSource*)
    RBC_ENGINE.Model_decl(
      RBC_ENGINE.Fsm_add_transition(
        "xmlread", "_ZN3org3xml3sax9XMLReader5parseEJvPNS1_11InputSourceE",
        RBC_ENGINE.Get_this_pointer(),
        1, "end", "IDS17-J", 101
        )
      );
    // void javax::xml::parsers::SAXParser::parse(java::io::InputStream*, org::xml::sax::helpers::DefaultHandler*)
    RBC_ENGINE.Model_decl(
      RBC_ENGINE.Fsm_add_transition(
        "xmlread", "_ZN5javax3xml7parsers9SAXParser5parseEJvPN4java2io11InputStreamEPN3org3xml3sax7helpers14DefaultHandlerE",
        RBC_ENGINE.Get_this_pointer(),
        1, "end", "IDS17-J", 101
        )
      );
    /* Comment out as do not support key list 
    // void javax::xml::parsers::SAXParser::parse(org::xml::sax::InputSource*, org::xml::sax::helpers::DefaultHandler*)
    RBC_ENGINE.Model_decl(
      RBC_ENGINE.Fsm_add_transition(
        "xmlread", "_ZN5javax3xml7parsers9SAXParser5parseEJvPN3org3xml3sax11InputSourceEPNS5_7helpers14DefaultHandlerE",
        RBC_ENGINE.Get_this_pointer(),
        1, "end", "IDS17-J"
        )
      );
    */

    // void org::xml::sax::XMLReader::parse(org::xml::sax::InputSource*)
    RBC_ENGINE.Model_decl(
      RBC_ENGINE.Fsm_add_transition(
        "created", "_ZN3org3xml3sax9XMLReader5parseEJvPNS1_11InputSourceE",
        RBC_ENGINE.Get_this_pointer(),
        1, "end", "IDS17-J", 101
        )
      );
    // void javax::xml::parsers::SAXParser::parse(java::io::InputStream*, org::xml::sax::helpers::DefaultHandler*)
    RBC_ENGINE.Model_decl(
      RBC_ENGINE.Fsm_add_transition(
        "created", "_ZN5javax3xml7parsers9SAXParser5parseEJvPN4java2io11InputStreamEPN3org3xml3sax7helpers14DefaultHandlerE",
        RBC_ENGINE.Get_this_pointer(),
        1, "end", "IDS17-J", 101
        )
      );
    /* Comment out as do not support key list 
    // void javax::xml::parsers::SAXParser::parse(org::xml::sax::InputSource*, org::xml::sax::helpers::DefaultHandler*)
    RBC_ENGINE.Model_decl(
      RBC_ENGINE.Fsm_add_transition(
        "created", "_ZN5javax3xml7parsers9SAXParser5parseEJvPN3org3xml3sax11InputSourceEPNS5_7helpers14DefaultHandlerE",
        RBC_ENGINE.Get_this_pointer(),
        1, "end", "IDS17-J"
        )
      );
    */
    RBC_ENGINE.Model_decl(RBC_ENGINE.Fsm_build_end("ids16_ids17"));
  }

  public static void ids01_11() {
    RBC_ENGINE.Model_decl(RBC_ENGINE.Fsm_build_begin("ids01_11"));
    RBC_ENGINE.Model_decl(RBC_ENGINE.Fsm_new_start_state("init"));
    RBC_ENGINE.Model_decl(RBC_ENGINE.Fsm_new_final_state("end"));
    // java::lang::String* java::text::Normalizer::normalize(java::lang::CharSequence*, java::text::Normalizer$Form*)
    RBC_ENGINE.Model_decl(
      RBC_ENGINE.Fsm_add_transition(
        "init", "_ZN4java4text10Normalizer9normalizeEJPNS_4lang6StringEPNS2_12CharSequenceEPNS0_15Normalizer$FormE", 
        RBC_ENGINE.Get_arg(1),
        1, "normalized", "", 102
        )
      );
    RBC_ENGINE.Model_decl(
      RBC_ENGINE.Fsm_add_transition(
        "init", "_ZN4java4util5regex7Pattern7matcherEJPNS1_7MatcherEPNS_4lang12CharSequenceE",
        RBC_ENGINE.Get_arg(1),
        1, "matched_b", "", 103
        )
      );
    RBC_ENGINE.Model_decl(
      RBC_ENGINE.Fsm_add_transition(
        "matched_b", "_ZN4java4text10Normalizer9normalizeEJPNS_4lang6StringEPNS2_12CharSequenceEPNS0_15Normalizer$FormE",
        RBC_ENGINE.Get_arg(1),
        1, "end", "IDS01-J", 102
        )
      );
    // java::lang::String* java::lang::String::replaceAll(java::lang::String*, java::lang::String*)
    RBC_ENGINE.Model_decl(
      RBC_ENGINE.Fsm_add_transition(
        "normalized", "_ZN4java4lang6String10replaceAllEJPS1_S2_S2_", 
        RBC_ENGINE.Get_this_pointer(),
        1, "filtered", "", 104
        )
      );
    // java::util::regex::Matcher* java::util::regex::Pattern::matcher(java::lang::CharSequence*)
    RBC_ENGINE.Model_decl(
      RBC_ENGINE.Fsm_add_transition(
        "normalized", "_ZN4java4util5regex7Pattern7matcherEJPNS1_7MatcherEPNS_4lang12CharSequenceE", 
        RBC_ENGINE.Get_arg(1),
        1, "matched_a", "", 103
        )
      );
    // java::util::regex::Matcher* java::util::regex::Pattern::matcher(java::lang::CharSequence*)
    RBC_ENGINE.Model_decl(
      RBC_ENGINE.Fsm_add_transition(
        "filtered", "_ZN4java4util5regex7Pattern7matcherEJPNS1_7MatcherEPNS_4lang12CharSequenceE", 
        RBC_ENGINE.Get_arg(1),
        1, "matched_a", "", 103
        )
      );
    // java::lang::String* java::lang::String::replaceAll(java::lang::String*, java::lang::String*)
    RBC_ENGINE.Model_decl(
      RBC_ENGINE.Fsm_add_transition(
        "matched_a", "_ZN4java4lang6String10replaceAllEJPS1_S2_S2_",
        RBC_ENGINE.Get_this_pointer(),
        1, "end", "IDS11-J", 104
        )
      );
    RBC_ENGINE.Model_decl(RBC_ENGINE.Fsm_set_default_action("filtered", "IDS01-J", 70));
    RBC_ENGINE.Model_decl(RBC_ENGINE.Fsm_build_end("ids01_11"));
  }

  public static void sec07() {
    RBC_ENGINE.Model_decl(RBC_ENGINE.Fsm_build_begin("sec07"));
    RBC_ENGINE.Model_decl(RBC_ENGINE.Fsm_new_start_state("init"));
    RBC_ENGINE.Model_decl(RBC_ENGINE.Fsm_new_final_state("end"));
    // java::security::PermissionCollection* java::net::URLClassLoader::getPermissions(java::security::CodeSource*)
    RBC_ENGINE.Model_decl(
      RBC_ENGINE.Fsm_add_transition(
        "init", "_ZN4java3net14URLClassLoader14getPermissionsEJPNS_8security20PermissionCollectionEPNS2_10CodeSourceE",
        RBC_ENGINE.Get_arg(1),
        RBC_ENGINE.Exec_eval(RBC_ENGINE.EXEC_KIND.CMP_EQ,
                             RBC_ENGINE.Exec_eval(RBC_ENGINE.EXEC_KIND.CALL_SUPER, 0, 0, 0),
                             0,
                             0),
        "getPermission", "", 105
        )
      );

    // java::security::PermissionCollection* java::net::URLClassLoader::getPermissions(java::security::CodeSource*)
    RBC_ENGINE.Model_decl(
      RBC_ENGINE.Fsm_add_transition(
        "getPermission", "_ZN4java3net14URLClassLoader14getPermissionsEJPNS_8security20PermissionCollectionEPNS2_10CodeSourceE",
        RBC_ENGINE.Get_arg(1),
        RBC_ENGINE.Exec_eval(RBC_ENGINE.EXEC_KIND.CALL_SUPER, 0, 0, 0),
        "end", "", 106
        )
      );

    RBC_ENGINE.Model_decl(RBC_ENGINE.Fsm_set_default_action("getPermission", "SEC07-J", 70));
    RBC_ENGINE.Model_decl(RBC_ENGINE.Fsm_build_end("sec07"));
  }

  public static void env03() {
    RBC_ENGINE.Model_decl(RBC_ENGINE.Fsm_build_begin("env03"));
    RBC_ENGINE.Model_decl(RBC_ENGINE.Fsm_new_start_state("init"));
    RBC_ENGINE.Model_decl(RBC_ENGINE.Fsm_new_final_state("end"));
    // java::lang::RuntimePermission::RuntimePermission(java::lang::String*)
    RBC_ENGINE.Model_decl(
      RBC_ENGINE.Fsm_add_transition(
        "init",
        "_ZN4java4lang17RuntimePermissionC1EPNS0_6StringE",
        RBC_ENGINE.Get_this_pointer(),
        RBC_ENGINE.Is_str_eq(RBC_ENGINE.Get_arg(1), "createClassLoader"),
        "invalid_perm", "", 107
        )
      );
    // java::lang::reflect::ReflectPermission::ReflectPermission(java::lang::String*)
    RBC_ENGINE.Model_decl(
      RBC_ENGINE.Fsm_add_transition(
        "init",
        "_ZN4java4lang17RuntimePermissionC1EPNS0_6StringES3_",
        RBC_ENGINE.Get_this_pointer(),
        RBC_ENGINE.Is_str_eq(RBC_ENGINE.Get_arg(1), "createClassLoader"),
        "invalid_perm", "", 108
        )
      );
    // java::lang::reflect::ReflectPermission::ReflectPermission(java::lang::String*)
    RBC_ENGINE.Model_decl(
      RBC_ENGINE.Fsm_add_transition(
        "init",
        "_ZN4java4lang7reflect17ReflectPermissionC1EPNS0_6StringE",
        RBC_ENGINE.Get_this_pointer(),
        RBC_ENGINE.Is_str_eq(RBC_ENGINE.Get_arg(1), "suppressAccessChecks"),
        "invalid_perm", "", 109
        )
      );
     // java::lang::reflect::ReflectPermission::ReflectPermission(java::lang::String*, java::lang::String*)
    RBC_ENGINE.Model_decl(
      RBC_ENGINE.Fsm_add_transition(
        "init",
        "_ZN4java4lang7reflect17ReflectPermissionC1EPNS0_6StringES4_",
        RBC_ENGINE.Get_this_pointer(),
        RBC_ENGINE.Is_str_eq(RBC_ENGINE.Get_arg(1), "suppressAccessChecks"),
        "invalid_perm", "", 109
        )
      );
    // void java::security::PermissionCollection::add(java::security::Permission*)
    RBC_ENGINE.Model_decl(
      RBC_ENGINE.Fsm_add_transition(
        "invalid_perm",
        "_ZN4java8security20PermissionCollection3addEJvPNS0_10PermissionE",
        RBC_ENGINE.Get_arg(1),
        1,
        "end", "ENV03-J", 110
        )
      );
    RBC_ENGINE.Model_decl(RBC_ENGINE.Fsm_build_end("env03"));
  }

  public static void sec01() {
    RBC_ENGINE.Model_decl(RBC_ENGINE.Fsm_build_begin("sec01"));
    RBC_ENGINE.Model_decl(RBC_ENGINE.Fsm_new_start_state("init"));
    RBC_ENGINE.Model_decl(RBC_ENGINE.Fsm_new_final_state("end"));
    // java::lang::Object* java::security::AccessController::doPrivileged(java::security::PrivilegedExceptionAction*)
    RBC_ENGINE.Model_decl(
      RBC_ENGINE.Fsm_add_transition(
        "init", "_ZN4java8security16AccessController12doPrivilegedEJPNS_4lang6ObjectEPNS0_25PrivilegedExceptionActionE", 
        null,
        1, "inPrivileged", "", 111
        )
      );  
    // java::io::FileInputStream::FileInputStream(java::lang::String*)
    RBC_ENGINE.Model_decl(
      RBC_ENGINE.Fsm_add_transition(
        "inPrivileged", "_ZN4java2io15FileInputStreamC1EPNS_4lang6StringE",
        RBC_ENGINE.Get_arg(1),
        RBC_ENGINE.Exec_eval(RBC_ENGINE.EXEC_KIND.CMP_EQ,
                             RBC_ENGINE.Exec_eval(RBC_ENGINE.EXEC_KIND.PRE_SANITIZED,
                                                  RBC_ENGINE.Exec_eval(RBC_ENGINE.EXEC_KIND.GET_ARG, 1)),
                             0),
        "end", "SEC01-J", 112
        )
      );  
    // RBC_ENGINE.Model_decl(RBC_ENGINE.Fsm_set_default_action("inPrivileged", ""));
    RBC_ENGINE.Model_decl(RBC_ENGINE.Fsm_build_end("sec01"));
  }

  public static void sec06() {
    RBC_ENGINE.Model_decl(RBC_ENGINE.Fsm_build_begin("sec06"));
    RBC_ENGINE.Model_decl(RBC_ENGINE.Fsm_new_start_state("init"));
    RBC_ENGINE.Model_decl(RBC_ENGINE.Fsm_new_final_state("end"));
    // java::lang::Class* java::lang::ClassLoader::loadClass(java::lang::String*)
    RBC_ENGINE.Model_decl(
      RBC_ENGINE.Fsm_add_transition(
        "init", "_ZN4java4lang11ClassLoader9loadClassEJPNS0_5ClassEPNS0_6StringE", RBC_ENGINE.Get_ret(),
        RBC_ENGINE.Exec_eval(RBC_ENGINE.EXEC_KIND.FUNC_INVOKED_BY_SUBCLASS,
                             "java.net.URLClassLoader"), 
        "loadClass", "", 113
        )
      );
    // void java::security::cert::Certificate::verify(java::security::PublicKey*)
    RBC_ENGINE.Model_decl(
      RBC_ENGINE.Fsm_add_transition(
        "loadClass", "_ZN4java8security4cert11Certificate6verifyEJvPNS0_9PublicKeyE", 
        RBC_ENGINE.Get_this_pointer(),
        1, "verified", "", 114
        )
      );
    // java::lang::Object* java::lang::reflect::Method::invoke(java::lang::Object*, JArray<java::lang::Object*>*)
    RBC_ENGINE.Model_decl(
      RBC_ENGINE.Fsm_add_transition(
        "verified", "_ZN4java4lang7reflect6Method6invokeEJPNS0_6ObjectES4_P6JArrayIS4_E", RBC_ENGINE.Get_this_pointer(),
        1, "end", "", 115
        )
      );

    RBC_ENGINE.Model_decl(
      RBC_ENGINE.Fsm_add_transition(
        "loadClass", "_ZN4java4lang5Class9getMethodEJPNS0_7reflect6MethodEPNS0_6StringEP6JArrayIPS1_E", RBC_ENGINE.Get_ret(),
        1, "getMethod", "", 116
        )
      );
    RBC_ENGINE.Model_decl(
      RBC_ENGINE.Fsm_add_transition(
        "verified", "_ZN4java4lang5Class9getMethodEJPNS0_7reflect6MethodEPNS0_6StringEP6JArrayIPS1_E", RBC_ENGINE.Get_ret(),
        1, "getMethod2", "", 116
        )
    );

    // java::lang::Object* java::lang::reflect::Method::invoke(java::lang::Object*, JArray<java::lang::Object*>*)
    RBC_ENGINE.Model_decl(
      RBC_ENGINE.Fsm_add_transition(
        "getMethod", "_ZN4java4lang7reflect6Method6invokeEJPNS0_6ObjectES4_P6JArrayIS4_E", RBC_ENGINE.Get_this_pointer(),
        1, "end", "SEC06-J", 115
        )
      );
    // java::lang::Object* java::lang::reflect::Method::invoke(java::lang::Object*, JArray<java::lang::Object*>*)
    RBC_ENGINE.Model_decl(
      RBC_ENGINE.Fsm_add_transition(
        "getMethod2", "_ZN4java4lang7reflect6Method6invokeEJPNS0_6ObjectES4_P6JArrayIS4_E", RBC_ENGINE.Get_this_pointer(),
        1, "end", "", 115
        )
      );
    RBC_ENGINE.Model_decl(RBC_ENGINE.Fsm_build_end("sec06"));
  }

  public static void msc02() {
    RBC_ENGINE.Model_decl(RBC_ENGINE.Fsm_build_begin("msc02"));
    RBC_ENGINE.Model_decl(RBC_ENGINE.Fsm_new_start_state("init"));
    RBC_ENGINE.Model_decl(RBC_ENGINE.Fsm_new_final_state("end"));
    // public java.util.Random()
    RBC_ENGINE.Model_decl(
      RBC_ENGINE.Fsm_add_transition(
        "init", "_ZN4java4util6RandomC1Ev",
        RBC_ENGINE.Get_this_pointer(), 1, "end", "MSC02-J", 117
        )
      );
    // public java.util.Random(long seed)
    RBC_ENGINE.Model_decl(
      RBC_ENGINE.Fsm_add_transition(
        "init", "_ZN4java4util6RandomC1Ex",
        RBC_ENGINE.Get_this_pointer(), 1, "end", "MSC02-J", 117
        )
      );
    // public static double java.lang.Math.random()
    RBC_ENGINE.Model_decl(
      RBC_ENGINE.Fsm_add_transition(
        "init", "_ZN4java4lang4Math6randomEJdv",
        RBC_ENGINE.Get_ret(), 1, "end", "MSC02-J", 117
        )
      );
    RBC_ENGINE.Model_decl(RBC_ENGINE.Fsm_build_end("msc02"));
  }

  public static void msc61() {
    RBC_ENGINE.Model_decl(RBC_ENGINE.Fsm_build_begin("msc61"));
    RBC_ENGINE.Model_decl(RBC_ENGINE.Fsm_new_start_state("init"));
    RBC_ENGINE.Model_decl(RBC_ENGINE.Fsm_new_final_state("end"));
    // javax::crypto::Cipher* javax::crypto::Cipher::getInstance(java::lang::String*)
    RBC_ENGINE.Model_decl(
      RBC_ENGINE.Fsm_add_transition(
        "init", "_ZN5javax6crypto6Cipher11getInstanceEJPS1_PN4java4lang6StringE", 
        RBC_ENGINE.Get_arg(1),
        RBC_ENGINE.Exec_eval(RBC_ENGINE.EXEC_KIND.STR_REG_MATCH,
                             RBC_ENGINE.Exec_eval(RBC_ENGINE.EXEC_KIND.GET_ARG, 1),
                             "^DES|^AES$|^AES/ECB"),
        "end",
        "MSC61-J", 118
      )
    );
    // javax::crypto::Cipher* javax::crypto::Cipher::getInstance(java::lang::String*, java::security::Provider*)
    RBC_ENGINE.Model_decl(
      RBC_ENGINE.Fsm_add_transition(
        "init", "_ZN5javax6crypto6Cipher11getInstanceEJPS1_PN4java4lang6StringEPNS3_8security8ProviderE", 
        RBC_ENGINE.Get_arg(1),
        RBC_ENGINE.Exec_eval(RBC_ENGINE.EXEC_KIND.STR_REG_MATCH,
                             RBC_ENGINE.Exec_eval(RBC_ENGINE.EXEC_KIND.GET_ARG, 1),
                             "^DES|^AES$|^AES/ECB"),
        "end",
        "MSC61-J", 118
      )
    );
    // javax::crypto::Cipher* javax::crypto::Cipher::getInstance(java::lang::String*, java::lang::String*)
    RBC_ENGINE.Model_decl(
      RBC_ENGINE.Fsm_add_transition(
        "init", "_ZN5javax6crypto6Cipher11getInstanceEJPS1_PN4java4lang6StringES6_", 
        RBC_ENGINE.Get_arg(1),
        RBC_ENGINE.Exec_eval(RBC_ENGINE.EXEC_KIND.STR_REG_MATCH,
                             RBC_ENGINE.Exec_eval(RBC_ENGINE.EXEC_KIND.GET_ARG, 1),
                             "^DES|^AES$|^AES/ECB"),
        "end",
        "MSC61-J", 118
      )
    );

    RBC_ENGINE.Model_decl(RBC_ENGINE.Fsm_build_end("msc61"));
  }
  
  public static void fio52() {
    RBC_ENGINE.Model_decl(RBC_ENGINE.Fsm_build_begin("fio52"));
    RBC_ENGINE.Model_decl(RBC_ENGINE.Fsm_new_start_state("init"));
    RBC_ENGINE.Model_decl(RBC_ENGINE.Fsm_new_final_state("end"));
    // javax::servlet::http::Cookie::Cookie(java::lang::String*, java::lang::String*)
    RBC_ENGINE.Model_decl(
      RBC_ENGINE.Fsm_add_transition(
        "init", "_ZN5javax7servlet4http6CookieC1EPN4java4lang6StringES6_",
        RBC_ENGINE.Get_this_pointer(),
        RBC_ENGINE.Not(RBC_ENGINE.Is_tag_attr_set(RBC_ENGINE.Get_arg(2), "sensitive", "sanitize_cookie")),
        "tainted_cookie",
        "", 119
      )
    );
    // javax::servlet::http::Cookie::setSecure(bool)
    RBC_ENGINE.Model_decl(
      RBC_ENGINE.Fsm_add_transition(
        "tainted_cookie", "_ZN5javax7servlet4http6Cookie9setSecureEJvb",
        RBC_ENGINE.Get_this_pointer(),
        RBC_ENGINE.Not(RBC_ENGINE.Get_value(RBC_ENGINE.Get_arg(1))),
        "tainted_cookie",
        "", 120
      )
    );
    // javax::servlet::http::Cookie::setSecure(bool)
    RBC_ENGINE.Model_decl(
      RBC_ENGINE.Fsm_add_transition(
        "tainted_cookie", "_ZN5javax7servlet4http6Cookie9setSecureEJvb",
        RBC_ENGINE.Get_this_pointer(),
        RBC_ENGINE.Get_value(RBC_ENGINE.Get_arg(1)),
        "safe_cookie",
        "", 121
      )
    );
    // javax::servlet::http::HttpServletResponse::addCookie(javax::servlet::http::Cookie*)
    RBC_ENGINE.Model_decl(
      RBC_ENGINE.Fsm_add_transition(
        "tainted_cookie", 
        "_ZN5javax7servlet4http19HttpServletResponse9addCookieEJvPNS1_6CookieE",
        RBC_ENGINE.Get_arg(1),
        1,
        "end",
        "FIO52-J", 122
      )
    );
    // javax::servlet::http::HttpServletResponse::addCookie(javax::servlet::http::Cookie*)
    RBC_ENGINE.Model_decl(
      RBC_ENGINE.Fsm_add_transition(
        "safe_cookie", 
        "_ZN5javax7servlet4http19HttpServletResponse9addCookieEJvPNS1_6CookieE",
        RBC_ENGINE.Get_arg(1),
        1,
        "end",
        "", 122
      )
    );

    RBC_ENGINE.Model_decl(RBC_ENGINE.Fsm_build_end("fio52"));
  }

  public static void ids51() {
    RBC_ENGINE.Model_decl(RBC_ENGINE.Fsm_build_begin("ids51"));
    RBC_ENGINE.Model_decl(RBC_ENGINE.Fsm_new_start_state("init"));
    RBC_ENGINE.Model_decl(RBC_ENGINE.Fsm_new_final_state("end"));
    // java::io::PrintWriter* javax::servlet::ServletResponse::getWriter()
    RBC_ENGINE.Model_decl(
      RBC_ENGINE.Fsm_add_transition(
        "init",
        "_ZN5javax7servlet15ServletResponse9getWriterEJPN4java2io11PrintWriterEv",
        RBC_ENGINE.Get_ret(),
        1,
        "response",
        "", 123
      )
    );
    // void java::io::PrintWriter::println(java::lang::String*)
    RBC_ENGINE.Model_decl(
      RBC_ENGINE.Fsm_add_transition(
        "response", "_ZN4java2io11PrintWriter7printlnEJvPNS_4lang6StringE",
        RBC_ENGINE.Get_this_pointer(),
        RBC_ENGINE.Not(RBC_ENGINE.Is_tag_attr_set(RBC_ENGINE.Get_arg(1), "tainted", "sanitize_xss")),
        "end",
        "IDS51-J", 124
      )
    );
    // void java::io::PrintWriter::println(JArray<wchar_t>*)
    RBC_ENGINE.Model_decl(
      RBC_ENGINE.Fsm_add_transition(
        "response", "_ZN4java2io11PrintWriter7printlnEJvP6JArrayIwE",
        RBC_ENGINE.Get_this_pointer(),
        RBC_ENGINE.Not(RBC_ENGINE.Is_tag_attr_set(RBC_ENGINE.Get_arg(1), "tainted", "sanitize_xss")),
        "end",
        "IDS51-J", 124
      )
    );
    // void java::io::PrintWriter::print(java::lang::String*)
    RBC_ENGINE.Model_decl(
      RBC_ENGINE.Fsm_add_transition(
        "response", "_ZN4java2io11PrintWriter5printEJvPNS_4lang6StringE",
        RBC_ENGINE.Get_this_pointer(),
        RBC_ENGINE.Not(RBC_ENGINE.Is_tag_attr_set(RBC_ENGINE.Get_arg(1), "tainted", "sanitize_xss")),
        "end",
        "IDS51-J", 124
      )
    );
    // void java::io::PrintWriter::print(JArray<wchar_t>*)
    RBC_ENGINE.Model_decl(
      RBC_ENGINE.Fsm_add_transition(
        "response", "_ZN4java2io11PrintWriter5printEJvP6JArrayIwE",
        RBC_ENGINE.Get_this_pointer(),
        RBC_ENGINE.Not(RBC_ENGINE.Is_tag_attr_set(RBC_ENGINE.Get_arg(1), "tainted", "sanitize_xss")),
        "end",
        "IDS51-J", 124
      )
    );
    // java::io::PrintWriter* java::io::PrintWriter::printf(java::lang::String*, JArray<java::lang::Object*>*)
    RBC_ENGINE.Model_decl(
      RBC_ENGINE.Fsm_add_transition(
        "response", "_ZN4java2io11PrintWriter6printfEJPS1_PNS_4lang6StringEP6JArrayIPNS3_6ObjectEE",
        RBC_ENGINE.Get_this_pointer(),
        RBC_ENGINE.Or(RBC_ENGINE.Not(RBC_ENGINE.Is_tag_attr_set(RBC_ENGINE.Get_arg(1), "tainted", "sanitize_xss")),
                      RBC_ENGINE.Not(RBC_ENGINE.Is_tag_attr_set(RBC_ENGINE.Get_arg(2), "tainted", "sanitize_xss"))),
        "end",
        "IDS51-J", 124
      )
    );
    // java::io::PrintWriter* java::io::PrintWriter::printf(java::util::Locale*,
    //                                                     java::lang::String*,
    //                                                     JArray<java::lang::Object*>*)
    RBC_ENGINE.Model_decl(
      RBC_ENGINE.Fsm_add_transition(
        "response", "_ZN4java2io11PrintWriter6printfEJPS1_PNS_4util6LocaleEPNS_4lang6StringEP6JArrayIPNS6_6ObjectEE",
        RBC_ENGINE.Get_this_pointer(),
        RBC_ENGINE.Or(RBC_ENGINE.Not(RBC_ENGINE.Is_tag_attr_set(RBC_ENGINE.Get_arg(2), "tainted", "sanitize_xss")),
                      RBC_ENGINE.Not(RBC_ENGINE.Is_tag_attr_set(RBC_ENGINE.Get_arg(3), "tainted", "sanitize_xss"))),
        "end",
        "IDS51-J", 124
      )
    );
    // void java::io::PrintWriter::write(JArray<wchar_t>*)
    RBC_ENGINE.Model_decl(
      RBC_ENGINE.Fsm_add_transition(
        "response", "_ZN4java2io11PrintWriter5writeEJvP6JArrayIwE",
        RBC_ENGINE.Get_this_pointer(),
        RBC_ENGINE.Not(RBC_ENGINE.Is_tag_attr_set(RBC_ENGINE.Get_arg(1), "tainted", "sanitize_xss")),
        "end",
        "IDS51-J", 125
      )
    );
    // void java::io::PrintWriter::write(java::lang::String*)
    RBC_ENGINE.Model_decl(
      RBC_ENGINE.Fsm_add_transition(
        "response", "_ZN4java2io11PrintWriter5writeEJvPNS_4lang6StringE",
        RBC_ENGINE.Get_this_pointer(),
        RBC_ENGINE.Not(RBC_ENGINE.Is_tag_attr_set(RBC_ENGINE.Get_arg(1), "tainted", "sanitize_xss")),
        "end",
        "IDS51-J", 125
      )
    );
    // void java::io::PrintWriter::write(java::lang::String*, int, int)
    RBC_ENGINE.Model_decl(
      RBC_ENGINE.Fsm_add_transition(
        "response", "_ZN4java2io11PrintWriter5writeEJvPNS_4lang6StringEii",
        RBC_ENGINE.Get_this_pointer(),
        RBC_ENGINE.Not(RBC_ENGINE.Is_tag_attr_set(RBC_ENGINE.Get_arg(1), "tainted", "sanitize_xss")),
        "end",
        "IDS51-J", 125
      )
    );
    // void java::io::PrintWriter::write(JArray<wchar_t>*, int, int)
    RBC_ENGINE.Model_decl(
      RBC_ENGINE.Fsm_add_transition(
        "response", "_ZN4java2io11PrintWriter5writeEJvP6JArrayIwEii",
        RBC_ENGINE.Get_this_pointer(),
        RBC_ENGINE.Not(RBC_ENGINE.Is_tag_attr_set(RBC_ENGINE.Get_arg(1), "tainted", "sanitize_xss")),
        "end",
        "IDS51-J", 125
      )
    );
    // java::io::PrintWriter* java::io::PrintWriter::format(java::util::Locale*,
    //                                                      java::lang::String*,
    //                                                      JArray<java::lang::Object*>*)
    RBC_ENGINE.Model_decl(
      RBC_ENGINE.Fsm_add_transition(
        "response", 
        "_ZN4java2io11PrintWriter6formatEJPS1_PNS_4util6LocaleEPNS_4lang6StringEP6JArrayIPNS6_6ObjectEE",
        RBC_ENGINE.Get_this_pointer(),
        RBC_ENGINE.Or(RBC_ENGINE.Not(RBC_ENGINE.Is_tag_attr_set(RBC_ENGINE.Get_arg(2), "tainted", "sanitize_xss")),
                      RBC_ENGINE.Not(RBC_ENGINE.Is_tag_attr_set(RBC_ENGINE.Get_arg(3), "tainted", "sanitize_xss"))),
        "end",
        "IDS51-J", 125
      )
    );
    // java::io::PrintWriter* java::io::PrintWriter::format(java::lang::String*, JArray<java::lang::Object*>*)
    RBC_ENGINE.Model_decl(
      RBC_ENGINE.Fsm_add_transition(
        "response", 
        "_ZN4java2io11PrintWriter6formatEJPS1_PNS_4lang6StringEP6JArrayIPNS3_6ObjectEE",
        RBC_ENGINE.Get_this_pointer(),
        RBC_ENGINE.Or(RBC_ENGINE.Not(RBC_ENGINE.Is_tag_attr_set(RBC_ENGINE.Get_arg(1), "tainted", "sanitize_xss")),
                      RBC_ENGINE.Not(RBC_ENGINE.Is_tag_attr_set(RBC_ENGINE.Get_arg(2), "tainted", "sanitize_xss"))),
        "end",
        "IDS51-J", 125
      )
    );
    RBC_ENGINE.Model_decl(RBC_ENGINE.Fsm_build_end("ids51"));
  }

  public static void obj09()
  {
    RBC_ENGINE.Model_decl(RBC_ENGINE.Fsm_build_begin("obj09"));
    RBC_ENGINE.Model_decl(RBC_ENGINE.Fsm_new_start_state("init"));
    RBC_ENGINE.Model_decl(RBC_ENGINE.Fsm_new_final_state("end"));
    // java::lang::Class* java::lang::Object::getClass
    RBC_ENGINE.Model_decl(
      RBC_ENGINE.Fsm_add_transition(
        "init",
        "_ZN4java4lang6Object8getClassEJPNS0_5ClassEv",
        RBC_ENGINE.Get_ret(),
        1,
        "get_class",
        "", 126
      )
    );
    // java::lang::String* java::lang::Class::getName()
    RBC_ENGINE.Model_decl(
      RBC_ENGINE.Fsm_add_transition(
        "get_class",
        "_ZN4java4lang5Class7getNameEJPNS0_6StringEv",
        RBC_ENGINE.Get_this_pointer(),
        1,
        "get_class_name",
        "", 127
      )
    );
    // bool java::lang::String::equals(java::lang::Object*)
    RBC_ENGINE.Model_decl(
      RBC_ENGINE.Fsm_add_transition(
        "get_class_name",
        "_ZN4java4lang6String6equalsEJbPNS0_6ObjectE",
        RBC_ENGINE.Get_this_pointer(),
        1,
        "end",
        "OBJ09-J", 128
      )
    );
    RBC_ENGINE.Model_decl(RBC_ENGINE.Fsm_build_end("obj09"));
  }
}

class TAG_CONF {
  static void Init_tag() {
    RBC_ENGINE.Model_decl(RBC_ENGINE.Set_tag_const_defval("tainted", RBC_ENGINE.TAG_DEF_VAL.UNSET));
    RBC_ENGINE.Model_decl(RBC_ENGINE.Set_tag_input_defval("tainted", RBC_ENGINE.TAG_DEF_VAL.SET));
    RBC_ENGINE.Model_decl(RBC_ENGINE.Set_tag_input_defval("sensitive", RBC_ENGINE.TAG_DEF_VAL.SET));
  }
}
