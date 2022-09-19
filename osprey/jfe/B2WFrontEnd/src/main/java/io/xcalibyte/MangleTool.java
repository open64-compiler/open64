/*
 * Copyright (C) 2021 Xcalibyte (Shenzhen) Limited.
 */

package io.xcalibyte;

import soot.*;
import soot.jimple.StaticFieldRef;

import java.util.*;
import static org.assertj.core.api.Assertions.assertThat;
import static soot.util.backend.ASMBackendUtils.toTypeDesc;

public class MangleTool {

  public static class NativeType {
    public static final int CLASS            = 1;
    public static final int FIELDSTUB        = 2;
    public static final int FIELD            = 3;
    public static final int JNI              = 4;
    public static final int SIGNATURE        = 5;
    public static final int METHOD_JDK_1     = 6;
    public static final int METHOD_JNI_SHORT = 7;
    public static final int METHOD_JNI_LONG  = 8;
  };

  static int methodThrowCount = 0;

  // Wrapper for mangleMethod
  static String mangle(SootMethod method) {
    return mangleMethod(
      method.getDeclaringClass(), method.getName(), method.getParameterTypes(), method.getReturnType()
    );
  }

  // non static method mangle name needn't add this pointer to args
  static String mangle(SootMethodRef methodRef) {
    return mangleMethod(
      methodRef.declaringClass(), methodRef.name(), methodRef.parameterTypes(), methodRef.returnType()
    );
  }

  // Mangle field name into GCC conforming name
  static String mangle(SootField field) {
    assertThat(field.isStatic()).as("Non static field need not mangling, field: " + field.toString()).isTrue();
    StringBuilder mangleName = new StringBuilder();
    initMangling(mangleName);
    mangleName.append(mangleClass(field.getDeclaringClass())); // FIXME : not compressing @Jason
    mangleName.append(mangleMemberName(field.getName()));
    mangleName.append('E');
    return mangleName.toString();
  }

  static String mangle(StaticFieldRef fieldRef) {
    return mangle(fieldRef.getField());
  }

  private static String[] internalGCJClazzList = new String[]{
          "java.lang.Byte", "java.lang.Boolean", "java.lang.Integer", "java.lang.Long", "java.lang.Double",
          "java.lang.Float", "java.lang.Char"};
  private static Set<String> internalGCJClazz = new HashSet(Arrays.asList(internalGCJClazzList));
  
  static String mangleForPrimitiveGCJType(String clazzName){ //int char soot given
    /*if(internalGCJClazz.contains(clazzName)){
      String clazzSplits[] = clazzName.split("\\.");
      return "_Jv_" + clazzSplits[clazzSplits.length - 1].toLowerCase() + "Class";
    }
    else return mangleForClassInternalSymbol(clazzName, InternalConstants.SymbolCategory.CLASS_SYMBOL);
    */
    return "_Jv_" + clazzName + "Class";
  }


  public static String mangleForMethodThrowTable(SootMethod method) {
    return "method_throw_" + (methodThrowCount ++);
  }

  // GCC class symbol internal symbol
  static String mangleForClassInternalSymbol(String clazzName, InternalConstants.SymbolCategory cate) {
    StringBuilder mangleName = new StringBuilder();
    switch (cate) {
      case VTABLE: {
        initMangling(mangleName);
        mangleName.append("TV");
        mangleName.append(mangleClass(clazzName));
        mangleName.append('E');
        break;
      }
      case CLASS_SYMBOL: {
        initMangling(mangleName);
        mangleName.append(mangleClass(clazzName));
        mangleName.append(6);
        mangleName.append("class");
        mangleName.append("$E");
        break;
      }
      case METHOD_TABLE: {
        mangleName.append("_MT_");
        mangleName.append(clazzName.replace('.', '_'));
        break;
      }
      case FIELD_TABLE: {
        mangleName.append("_FL_");
        mangleName.append(clazzName.replace('.', '_'));
        break;
      }
      case CATCHES_TABLE: {
        mangleName.append("_catch_classes_");
        mangleName.append(clazzName);
        break;
      }
      case INTERFACE_TABLE: {
        mangleName.append("_IF_");
        mangleName.append(clazzName.replace('.', '_'));
        break;
      }
      case CLASS_SYMBOL_PTR: {
        initMangling(mangleName);
        mangleName.append(mangleClass(clazzName));
        mangleName.append(7);
        mangleName.append("class");
        mangleName.append("$$E");
        break;
      }
      case GCJ_JCR_CLASS_PTR: {
        mangleName.append("_Jv_JCR_SECTION_data");
        break;
      }
      case GCJ_CONSTANTS_TAGS:{
        mangleName.append("_CT_");
        mangleName.append(clazzName.replace('.', '_'));
        break;
      }
      case GCJ_CONSTANTS_DATA:{
        mangleName.append("_CD_");
        mangleName.append(clazzName.replace('.', '_'));
        break;
      }
      case GCJ_METHOD_THROW_TABLE: {
        mangleName.append(mangleClass(clazzName));
        break;
      }
      default: {
        assertThat(false).as("Don't support symbol category : " + cate).isTrue();
      }
    }
    return mangleName.toString();
  }

  // Mangle into GCC form,
  // Basically a method = clazzName + methodName + parameters
  // Results are
  // for constructors:  mangled(className) + "C1E" + parameters
  // for normal functions: mangled(className) + functionName + "E" + parameters
  // when no parameter is given, use 'v' for void
  static String mangleMethod(SootClass clazz, String name, List<Type> paraTypes, Type returnType) {
    StringBuilder mangleName = new StringBuilder();
    MangleCompressionTable compress = new MangleCompressionTable();
    initMangling(mangleName);
    mangleRecordType(compress, false, RefType.v(clazz), mangleName);
    if (name.equals(SootMethod.constructorName)) {
      mangleName.append("C1");
    } else {
      mangleName.append(mangleMemberName(name));
    }
    mangleName.append('E');
    if(returnType != null && !name.equals(SootMethod.constructorName)) {
      mangleName.append('J');
      mangleName.append(mangeTypeWithCompression(returnType, compress));
    }
    if (paraTypes.size() == 0) {
      mangleName.append('v');
    } else {
      for (Type t : paraTypes) {
        mangleName.append(mangeTypeWithCompression(t, compress));
      }
    }
    return mangleName.toString();
  }

  // Mangle into GCC form
  static String mangeTypeWithCompression(Type t, MangleCompressionTable compress) {
    return mangleType(t, compress);
  }
    
  // Mangle for Method with Parameters for Java, used in calculating overrides
  static String mangleForMethodOverride(String name, List<Type> paraTypes, Type retType) {
    StringBuilder mangleName = new StringBuilder();
    mangleName.append(mangleForSignature(retType));
    mangleName.append(name);
    for (Type type : paraTypes) {
      mangleName.append(type.toString());
    }
    return mangleName.toString();
  }
  // Mangle into Java Method Signature, used in Method Table etc.
  static String mangleForMethodSignature(SootMethod method) {
    StringBuilder mangleSignature = new StringBuilder();
    mangleSignature.append('(');
    for (Type arg : method.getParameterTypes()) {
      mangleSignature.append(mangleForSignature(arg));
    }
    mangleSignature.append(')');
    mangleSignature.append(mangleForSignature(method.getReturnType()));
    return mangleSignature.toString();
  }

  // Mangle into Java Signature, used in Class Symbols, Field Names, newMultiArray etc.
  static String mangleForSignature(Type type) {
    switch (type.toString()) {
      case SootConstants.TYPE_BOOLEAN:
        return "Z";
      case SootConstants.TYPE_CHAR:
        return "C";
      case SootConstants.TYPE_VOID:
        return "V";
      case SootConstants.TYPE_BYTE:
      case SootConstants.TYPE_BYTECHAR:
        return "B";
      case SootConstants.TYPE_SHORT:
      case SootConstants.TYPE_SHORTCHAR:
        return "S";
      case SootConstants.TYPE_INT:
        return "I";
      case SootConstants.TYPE_LONG:
        return "J";
      case SootConstants.TYPE_FLOAT:
        return "F";
      case SootConstants.TYPE_DOUBLE:
        return "D";
      default:
        if (type instanceof ArrayType) {
          return "[" + mangleForSignature(((ArrayType) type).getElementType());
        } else if (type instanceof RefType) {
          return "L" + ((RefType) type).getClassName() + ";";
        }
        assertThat(false).as("GCJType is not primitive type, type : " + type).isTrue();
        return "";
    }
  }

  static String mangleForClassConstant(Type t){
    if (t instanceof RefType) {
      return ((RefType) t).getClassName();
    } else {
      return mangleForSignature(t);
    }
  }

  // FIXME: not compressing @Jason
  // Directly mangle into GCC conforming, not-compressing form  
  private static String mangleClass(SootClass clazz) {
    StringBuilder mangleName = new StringBuilder();
    mangleName.append('N');
    String qualifiedName = clazz.getName();
    String[] spName = qualifiedName.split("\\.");
    for (String sp : spName) {
      mangleName.append(sp.getBytes().length);
      mangleName.append(sp);
    }
    return mangleName.toString();
  }

  private static String mangleClass(String clazzName) {
    StringBuilder mangleName = new StringBuilder();
    mangleName.append('N');
    String[] spName = clazzName.split("\\.");
    for (String sp : spName) {
      mangleName.append(sp.getBytes().length);
      mangleName.append(sp);
    }
    return mangleName.toString();
  }

  // Requirement for all GCC conforming names
  private static void initMangling(StringBuilder sb) {
    sb.append("_Z");
  }

  static String mangleType(Type t, MangleCompressionTable compress) {
    switch (t.toString()) {
      case SootConstants.TYPE_BOOLEAN:
        return "b";
      case SootConstants.TYPE_CHAR:
        return "w";
      case SootConstants.TYPE_VOID:
        return "v";
      case SootConstants.TYPE_BYTE:
      case SootConstants.TYPE_BYTECHAR:
        return "c";
      case SootConstants.TYPE_SHORT:
      case SootConstants.TYPE_SHORTCHAR:
        return "s";
      case SootConstants.TYPE_INT:
        return "i";
      case SootConstants.TYPE_LONG:
        return "x";
      case SootConstants.TYPE_FLOAT:
        return "f";
      case SootConstants.TYPE_DOUBLE:
        return "d";
      default:
        if (t instanceof RefType) {
          return manglePointerType(compress, (RefType) t);
        } else if (t instanceof ArrayType) {
          return mangleArrayType(compress, (ArrayType) t);
        }
        //assertThat(false ).as( "Bad type : " + t.toString()).isTrue();
        return "?";
    }
  }

  /***
   * Mangle for RefType, which uses a compress table
   * @param compress compressTable used inside the process of mangling a single function's Name
   * @param type RefType Value to be mangled
   * @return the resulting String;
   */
  private static String manglePointerType(MangleCompressionTable compress, RefType type) {
    int match;
    /* Search for the type already in the compression table */
    if ((match = compress.findCompressionPointerMatch(type)) >= 0) {
      return emitCompressionString(match);
    }
    /* This didn't work. We start by mangling the pointed-to type */
    StringBuilder sb = new StringBuilder();
    sb.append('P');
    if(mangleRecordType(compress, true, type, sb) != 0) sb.append('E');
    /* Don't forget to insert the pointer type in the table */
    compress.add("P"+transformJavaStyleName(type.getClassName()));
    return sb.toString();
  }

  /***
   * Mangleing Record Type
   * @param compress
   * @param refType
   * @param parentAppender
   * @return
   */
  private static int mangleRecordType(MangleCompressionTable compress, boolean isWrappedInPointer, RefType refType, StringBuilder parentAppender) {
    StringBuilder ret = new StringBuilder();
    // Should parent add E after this record type
    boolean nadded_p = false;
    // WHether package name exists
    boolean qualified = ((refType.getSootClass().getPackageName()).length() > 0);
    // qualified always true, originally (!className.isEmpty()) because all method must belong to a class, thus qualified;
    List<String> matchTypeList = getPackageGCJNameList(refType.getSootClass().getName());
    assertThat(matchTypeList.size()).as("package gcj name list must not be empty").isGreaterThan(0);

    int match = -1;
    // Finding a possible match (from 0 to compression table size - 1)
    if ((match = compress.findCompressionRecordMatch(refType)) >= 0) {
	// We should add an N to the suffix of the name in case of (an incomplete match) while inside a pointer
	// e.g. java.io.Integer[] could be PNS1_6IntegerE, left for parent to append the 'E'
      if(isWrappedInPointer && compress.getMangledStringAtPos(match).length() != matchTypeList.get(matchTypeList.size() - 1).length()) {
        ret.append("N");
        nadded_p = true;
      }
      // Append a reduced part as "Sx_"
      String reduced = emitCompressionString(match);
      ret.append(reduced);
    }
    // We need to verify that the matched part is complete
    String matchedString = match == -1 ? "" : compress.getMangledStringAtPos(match);
    // If "Sx_" does not fulfill the whole class name, there's more to add
    if(matchedString.length() != matchTypeList.get(matchTypeList.size() - 1).length()){
      // If not inside a pointer or a qualified name, under circumstance we should add a 'N' in case it's not there
      // e.g. java.io.File could be mangled to S1_N4FileE or N4java2io4FileE
      if ((!isWrappedInPointer || qualified) && !nadded_p){
        nadded_p = true;
        ret.append("N");    //String original = compression_records.get(position);
      }

      // In case of an incomplete match, we should add remaining parts to the compressTable
      // In case of no-match, we should add all strings 
      boolean addingToTable = matchedString.isEmpty(); // assert variable
      for (int i = 0; i < matchTypeList.size(); i++) {
	// If already in the compressTable, skip it.
        if (!addingToTable && matchedString.equals(matchTypeList.get(i))) addingToTable = true;
	// Others should be add to the compressTable
        else if (addingToTable)
          compress.add(matchTypeList.get(i)); // inserting the remaining strings to compressTable;
      }
      assertThat(addingToTable).as("must there be at least one addition to the compress table").isTrue();
      // Skip the compressed part and emit the rest
      int skipLen = (match == -1) ? 0 : matchedString.length();
      ret.append(matchTypeList.get(matchTypeList.size() - 1).substring(skipLen));
    }
    // Conclusively adding to parent appender
    parentAppender.append(ret);
    // Returning 1 for parent to add E, and 0 for not letting parent to write E
    return nadded_p ? 1 : 0;
  }

  private static String mangleArrayType(MangleCompressionTable compress, ArrayType ptype) {

    int match;
    StringBuilder mangleName = new StringBuilder();

    /* Maybe we have what we're looking for in the compression table. */
    if ((match = compress.findCompressionArrayMatch (ptype)) >= 0)
      return emitCompressionString (match);

    /* We know for a fact that all arrays are pointers */
    mangleName.append('P');

    /* Maybe we already have a Jarray<t> => 6JArrayI...E somewhere. PSx_ will be enough. */
    if ((match = compress.findCompressionRawMatch("6JArrayI" + mangleArrayElementTypeRaw(ptype.getElementType()) + "E")) >= 0) {
      mangleName.append(emitCompressionString(match));
      return mangleName.toString();
    }

    /* Maybe we already have just JArray somewhere */
    if ((match = compress.findCompressionRawMatch ("6JArray")) >= 0) {
      mangleName.append(emitCompressionString(match));
    } else {
      /* Start the template mangled name */
      mangleName.append("6JArray");
      /* Insert in the compression table */
      compress.add("6JArray");
    }

    //Mangle the sub-type, here is a recursive invocation point.
    String generated = mangleType(ptype.getElementType(), compress);
    mangleName.append('I');
    mangleName.append(generated);
    mangleName.append('E');

    // The 6JArrayI....E was not in the compressTable, insert it with P6JArrayI....E
    compress.add("6JArrayI" + mangleArrayElementTypeRaw(ptype.getElementType()) + "E");
    compress.add("P6JArrayI" + mangleArrayElementTypeRaw(ptype.getElementType()) + "E");

    return mangleName.toString();
  }

  // Used to fully transform names of element without compressTable
  //   (used in creating a matchTypeList etc.)
  // If element is:
  // an Array: java.io.File[] to P6JArrayI4Njava2io4FileE,
  // a  Class: java.io.File   to 4java2io4File
  // a  Type:  int            to i
  private static String mangleArrayElementTypeRaw(Type t) {
    if(t instanceof RefType) {
      return transformJavaStyleName(((RefType) t).getClassName());
    } else if (t instanceof ArrayType) {
      // mangle it without compressTable
      return "P6JArrayI" + mangleArrayElementTypeRaw(((ArrayType) t).getElementType()) + "E";
    } else {
      switch (t.toString()) {
        case SootConstants.TYPE_BOOLEAN:
          return "b";
        case SootConstants.TYPE_CHAR:
          return "w";
        case SootConstants.TYPE_VOID:
          return "v";
        case SootConstants.TYPE_BYTE:
        case SootConstants.TYPE_BYTECHAR:
          return "c";
        case SootConstants.TYPE_SHORT:
        case SootConstants.TYPE_SHORTCHAR:
          return "s";
        case SootConstants.TYPE_INT:
          return "i";
        case SootConstants.TYPE_LONG:
          return "x";
        case SootConstants.TYPE_FLOAT:
          return "f";
        case SootConstants.TYPE_DOUBLE:
          return "d";
      }
    }
    assertThat(false).as("cannot handle type : " + t).isTrue();
    return "";
  }

  // Mangle a member name into length + name
  // for cxxKeyWordsSet,
  //   'while'  => 6$while
  private static String mangleMemberName(String name) {
    String nName = name;
    for (String invalid : MangleTool.invalidCharSet) {
      if (nName.contains(invalid)) {
        nName = nName.replaceAll(invalid, "");
      }
    }
    if (cxxKeyWordsSet.contains(name)) {
      nName += '$';
    }
    nName = String.valueOf(nName.getBytes().length) + nName;
    return nName;
  }

  // This is usually used for processing a package name
  private static List<String> getPackageGCJNameList(String javaStyleName) {
    List<String> packages = new ArrayList<>();
    int cursor, qualifications, wholeLen = javaStyleName.length();

    for (cursor = 0, qualifications = 0; cursor < wholeLen; cursor ++)
      if (javaStyleName.charAt(cursor) == '.')
        qualifications += 1;

    int i;
    for (i = 0, cursor = 0; i < qualifications; cursor++)
    {
      if (javaStyleName.charAt(cursor) == '.')
      {
        String singleUnitOfPackageList = javaStyleName.substring(0, cursor); // TODO: whether i or i+1
        packages.add(transformJavaStyleName(singleUnitOfPackageList));
        i += 1;
      }
    }
    packages.add(transformJavaStyleName(javaStyleName));
    return packages;
  }

  // e.g.  java.io.File => 4java2io4File
  private static String transformJavaStyleName(String javaStyle) {
    StringBuilder sb = new StringBuilder();
    String[] splitted = javaStyle.split("\\.");
    for(String t : splitted){
      sb.append(t.length());
      sb.append(t);
    }
    return sb.toString();
  }

    
  static class MangleCompressionTable {

    static class CompressionUnit {
      List<String> gcjNameList;
      boolean isCompleteClass;
      CompressionUnit (List<String> gcjStyleNameList){
        gcjNameList = gcjStyleNameList;
        assertThat(gcjStyleNameList.size() > 0)
                .as("partLen must not exceed length of total package name list").isTrue();
        isCompleteClass = true;
      }
      CompressionUnit (List<String> gcjStyleList, int count){
        assertThat(count < gcjStyleList.size()).isTrue();
        for(int i = 0; i < count; i++){
          gcjNameList.add(gcjStyleList.get(i));
        }
      }

      public CompressionUnit(String gcjStyleName) {
        gcjNameList = new ArrayList<>();
        gcjNameList.add(gcjStyleName);
      }

      public boolean isRawEqual(String rawGcjStyleString) {
        return gcjNameList.get(gcjNameList.size() - 1).equals(rawGcjStyleString);
      }
    }

    List<CompressionUnit> compressionNameList = new ArrayList<>();

    int findCompressionRecordMatch(RefType f){
      return findCompressionRecordMatch(f.getClassName());
    }

    // e.g. java.io.File.open(java.io.File, java.io.Me, java.io.Me, java.io.Me.VV)
    //      _ZN4java2io4File4openE S1_       NS0_2MeE      S2_        NS2_2VVE
    //  This function finds the javaStyleName transformed
    //  (e.g. in 1x2yy3zzz form)
    //  inside the compressTable
    int findCompressionRecordMatch(String javaStyleName) {
      int i, match = -1;
      // The gcj name list of java style name
      List<String> current = getPackageGCJNameList(javaStyleName);
      int currentCursor = 0, savedCurrentCursor = 0;
      for (i = 0; i < compressionNameList.size() && currentCursor < current.size(); i++)
      {  // Iterate over compression table
        CompressionUnit compressionEntry = compressionNameList.get(i);
        if (!current.isEmpty() && compressionEntry.isRawEqual(current.get(currentCursor)))
        // if current compression_table_item is the same as the current does
        // and not in the middle of a package name (or 'else block' would be executed before)
        {
          match = i;
          savedCurrentCursor = currentCursor;
          currentCursor ++;
        } // TODO: Check whether need to skip something , like gcj does.
      }
      return match;
    }

    int findCompressionArrayMatch(ArrayType t) {
      for(int i = 0; i < compressionNameList.size(); i++)
        if(compressionNameList.get(i).isRawEqual("P6JArrayI" + mangleArrayElementTypeRaw(t.getElementType()) + "E")) return i;
      return -1;
    }

    int findCompressionPointerMatch(RefType t) {
      for(int i = 0; i < compressionNameList.size(); i++)
        if(compressionNameList.get(i).isRawEqual("P"+transformJavaStyleName(t.getClassName()))) return i;
      return -1;
    }

    int findCompressionRawMatch(String rawGcjStyleString){
      for(int i = 0; i < compressionNameList.size(); i++)
        if(compressionNameList.get(i).isRawEqual(rawGcjStyleString)) return i;
      return -1;
    }

    public void add(String gcjStyleName) {
      compressionNameList.add(new CompressionUnit(gcjStyleName));
    }

    /***
     * Get the last mangled string
     * @param position
     * @return
     */
    public String getMangledStringAtPos(int position) {
      return compressionNameList.get(position).gcjNameList.get(compressionNameList.get(position).gcjNameList.size() - 1);
    }
  }

  // Mapping the match into Sx_ form or S_
  // 0=>S_,
  // 1=>S0_, 2=>S1_, 3=>S2_ .....
  static String emitCompressionString(int pos){
    StringBuilder sb = new StringBuilder();
    if(pos < 0){
      return "";
    }
    pos -= 1; // when pos is 0, no digit is needed, being S_ is enough.
    if (pos >= 0)
    {
      final byte[] digits = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ".getBytes();
      int n;
      int m=1;
      /* How many digits for I in base 36? */
      for (n = pos; n >= 36; n /= 36, m *=36);
      /* Write the digits out */
      while (m > 0)
      {
        int digit = pos / m;
        sb.append((char) digits[digit]);
        pos -= digit * m;
        m /= 36;
      }
    }
    return "S"+sb.toString()+"_";
  }

  private static String[] cxxKeyWords = {
    "_Complex",
    "__alignof",
    "__alignof__",
    "__asm",
    "__asm__",
    "__attribute",
    "__attribute__",
    "__builtin_va_arg",
    "__complex",
    "__complex__",
    "__const",
    "__const__",
    "__extension__",
    "__imag",
    "__imag__",
    "__inline",
    "__inline__",
    "__label__",
    "__null",
    "__real",
    "__real__",
    "__restrict",
    "__restrict__",
    "__signed",
    "__signed__",
    "__typeof",
    "__typeof__",
    "__volatile",
    "__volatile__",
    "and",
    "and_eq",
    "asm",
    "auto",
    "bitand",
    "bitor",
    "bool",
    "break",
    "case",
    "catch",
    "char",
    "class",
    "compl",
    "const",
    "const_cast",
    "continue",
    "default",
    "delete",
    "do",
    "double",
    "dynamic_cast",
    "else",
    "enum",
    "explicit",
    "export",
    "extern",
    "false",
    "float",
    "for",
    "friend",
    "goto",
    "if",
    "inline",
    "int",
    "long",
    "mutable",
    "namespace",
    "new",
    "not",
    "not_eq",
    "operator",
    "or",
    "or_eq",
    "private",
    "protected",
    "public",
    "register",
    "reinterpret_cast",
    "return",
    "short",
    "signed",
    "sizeof",
    "static",
    "static_cast",
    "struct",
    "switch",
    "template",
    "this",
    "throw",
    "true",
    "try",
    "typedef",
    "typeid",
    "typename",
    "typeof",
    "union",
    "unsigned",
    "using",
    "virtual",
    "void",
    "volatile",
    "wchar_t",
    "while",
    "xor",
    "xor_eq"
  };
  private static String[] invalidChar = {"<", "?", ">", ";", "\"", "\'", ":", "."};

  private static HashSet<String> cxxKeyWordsSet = new HashSet<>(Arrays.asList(cxxKeyWords));

  private static HashSet<String> invalidCharSet = new HashSet<>(Arrays.asList(invalidChar));

  private static long mangleUtfCount = 1;
  static String mangleForUTF(String str) {
    if (str.length() > 0) {
      return "_Utf" + (mangleUtfCount++);
    } else {
      return "_Utf0";
    }
  }

  static String mangleVptr(String originName) {
    return ".vptr" + originName;
  }

  public static String mangleSign(Type elem, int dimension) {
    StringBuilder result = new StringBuilder();
    for(int i = 0; i < dimension; i++){
      result.append("[");
    }
    if (elem instanceof ArrayType){
      result.append(elem.toString());
    }else if (elem instanceof RefType) {
      result.append(mangleForArrayBase(elem));
    } else if (elem instanceof PrimType) {
      result.append(mangleForArrayBase(elem));
    }
    return result.toString();
  }

  private static String mangleForArrayBase(Type t) {
    switch (t.toString()) {
      case SootConstants.TYPE_BOOLEAN:
        return "Z";
      case SootConstants.TYPE_CHAR:
        return "C";
      case SootConstants.TYPE_VOID:
        assertThat(false).as("void type is not allowed in array").isTrue();
      case SootConstants.TYPE_BYTE:
      case SootConstants.TYPE_BYTECHAR:
        return "B";
      case SootConstants.TYPE_SHORT:
      case SootConstants.TYPE_SHORTCHAR:
        return "S";
      case SootConstants.TYPE_INT:
        return "I";
      case SootConstants.TYPE_LONG:
        return "J";
      case SootConstants.TYPE_FLOAT:
        return "F";
      case SootConstants.TYPE_DOUBLE:
        return "D";
      default:
        if (t instanceof RefType) {
          return mangleRefTypeSign((RefType) t);
        } else if(t instanceof ArrayType) {
          assertThat(false).as("Array's baseType should not be a array anymore.").isTrue();
        }
        //assertThat(false ).as( "Bad type : " + t.toString()).isTrue();
        return "?";
    }
  }

  private static String mangleRefTypeSign(RefType t) {
    String mangleName = "";
    mangleName += "P";
    mangleName += mangleClass(t.getSootClass());
    mangleName += "E";
    return mangleName;
  }

  // TODO: implement by check the sigature
  private static boolean genLongName(SootClass sc, SootMethod method) {
    List<SootMethod> methods = sc.getMethods();
    String methodName = method.getName();
    for(int i = 0; i < methods.size(); i++) {
      SootMethod otherMethod = methods.get(i);
      if(otherMethod != method &&
        otherMethod.isNative() &&
        otherMethod.getName().equals(methodName)) {
        return true;
      }
    }
    return false;
  }

  /* Warning: Intentional ASCII operation. */
  private static final boolean isalnum(char ch) {
    return ch <= 0x7f && /* quick test */
      ((ch >= 'A' && ch <= 'Z') ||
        (ch >= 'a' && ch <= 'z') ||
        (ch >= '0' && ch <= '9'));
  }

  private static final boolean isprint(char ch) {
    return ch >= 32 && ch <= 126;
  }


  public static String mangleChar(char ch) {
    String s = Integer.toHexString(ch);
    int nzeros = 5 - s.length();
    char[] result = new char[6];
    result[0] = '_';
    for (int i = 1; i <= nzeros; i++)
      result[i] = '0';
    for (int i = nzeros+1, j = 0; i < 6; i++, j++)
      result[i] = s.charAt(j);
    return new String(result);
  }


  public static String mangleNativeName(CharSequence name, int mtype) {
    StringBuilder result = new StringBuilder(100);
    int length = name.length();

    for (int i = 0; i < length; i++) {
      char ch = name.charAt(i);
      if (isalnum(ch)) {
        result.append(ch);
      } else if ((ch == '.') &&
        mtype == NativeType.CLASS) {
        result.append('_');
      } else if (( ch == '$') &&
        mtype == NativeType.CLASS) {
        result.append('_');
        result.append('_');
      } else if (ch == '_' && mtype == NativeType.FIELDSTUB) {
        result.append('_');
      } else if (ch == '_' && mtype == NativeType.CLASS) {
        result.append('_');
      } else if (mtype == NativeType.JNI) {
        String esc = null;
        if (ch == '_')
          esc = "_1";
        else if (ch == '.')
          esc = "_";
        else if (ch == ';')
          esc = "_2";
        else if (ch == '[')
          esc = "_3";
        if (esc != null) {
          result.append(esc);
        } else {
          result.append(mangleChar(ch));
        }
      } else if (mtype == NativeType.SIGNATURE) {
        if (isprint(ch)) {
          result.append(ch);
        } else {
          result.append(mangleChar(ch));
        }
      } else {
        result.append(mangleChar(ch));
      }
    }
    return result.toString();
  }



  public static String mangleJNIMethod(RefType thisType, int methodNum) {
    assertThat(thisType.hasSootClass()).as("thisType is not a ref to soot class").isTrue();
    SootClass sc = thisType.getSootClass();
    List<SootMethod> methods = sc.getMethods();
    SootMethod method = null;
    for(int i=0; i < methods.size(); i++) {
      if(methods.get(i).getNumber() == methodNum) {
        method = methods.get(i);
        break;
      }
    }
    assertThat(method != null).as("cannot find method from SootClass method list");
    boolean genLong = genLongName(sc, method);

    StringBuilder result = new StringBuilder(100);
    result.append("Java_");

    /* JNI */
    result.append(mangleNativeName(sc.getName(), NativeType.JNI));
    result.append('_');
    String methodName = method.getName();
    //CharSequence name = new CharArray(methodName.toCharArray(), 0, methodName.length(), true );
    result.append(mangleNativeName(methodName, NativeType.JNI));
    if (genLong) {
      result.append("__");
      String sig = method.getDesc();
      if(sig == null) {
        SootMethodRef methodRef = method.makeRef();
        sig = toTypeDesc(methodRef);
      }
      sig = sig.substring(1);
      sig = sig.substring(0, sig.lastIndexOf(')'));
      sig = sig.replace('/', '.');
      result.append(mangleNativeName(sig, NativeType.JNI));
    }

    return result.toString();

  }
}
