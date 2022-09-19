/*
 * Copyright (C) 2021 Xcalibyte (Shenzhen) Limited.
 */

package io.xcalibyte;

import java.util.*;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import soot.*;
import soot.jimple.toolkits.typing.fast.BottomType;
import soot.options.Options;

import static org.assertj.core.api.Assertions.*;

import static io.xcalibyte.InternalConstants.*;
import static io.xcalibyte.WhirlConstants.*;

class TypeHandler {
  private static Map<Object, Long> cache = new HashMap<>();
  private static Map<String, Long> cacheClass = new HashMap<>();
  private static Logger logger = LogManager.getLogger(TypeHandler.class);

  private TypeHandler() {
  }

  static TyAttr getTypeAttribute(int modifier) {
    if (Modifier.isVolatile(modifier)) {
      return TyAttr.TY_ATTR_NULL;
//      return JavaToWhirlConst.getTyAttr(Modifier.VOLATILE);
    } else if (Modifier.isFinal(modifier)) {
      return JavaToWhirlConst.getTyAttr(Modifier.FINAL);
    }
    return TyAttr.TY_ATTR_NULL;
  }

  static long getType(Type t) {
    return getType(t, TyAttr.TY_ATTR_NULL);
  }


  static long getType(SootField sfd) {
    return getType(sfd.getType(), getTypeAttribute(sfd.getModifiers()));
  }

  static long getType(Local jlvar) {
    return getType(jlvar.getType(), TyAttr.TY_ATTR_NULL);
  }

  static long getType(SootMethod sootmethod) {
    return getMethodType(sootmethod.getReturnType(), getMethodParaTypes(sootmethod));
  }

  static long getType(SootMethodRef methodRef) {
    return getMethodType(methodRef.returnType(), getMethodParaTypes(methodRef));
  }

  private static Map<String, Long> arrayCache = new HashMap<>();

  static long getSubTypeArray(long eltType, int size, int align) {
    String name = eltType + "-ARRAY-" + size + "-ALIGN-" + align;
    if (arrayCache.containsKey(name)) {
      return arrayCache.get(name);
    }
    long idx = BGenDriver.createArrayType(size, eltType);
    BGenDriver.setTyAlign(idx, align);
    arrayCache.put(name, idx);
    return idx;
  }

  // add this pointer to non static method first arg
  static List<Type> getMethodParaTypes(SootMethod method) {
    List<Type> paraTypes = null;
    if (!method.isStatic()) {
      paraTypes = new ArrayList<>();
      paraTypes.add(method.getDeclaringClass().getType());
      paraTypes.addAll(method.getParameterTypes());
    } else {
      paraTypes = method.getParameterTypes();
    }
    return paraTypes;
  }

  static List<Type> getMethodParaTypes(SootMethodRef methodRef) {
    List<Type> paraTypes = null;
    if (!methodRef.isStatic()) {
      paraTypes = new ArrayList<>();
      paraTypes.add(methodRef.declaringClass().getType());
      paraTypes.addAll(methodRef.parameterTypes());
    } else {
      paraTypes = methodRef.parameterTypes();
    }
    return paraTypes;
  }

  static long getMethodType(Type retType, List<Type> paraTypes) {
    String typeSig = getMethodTypeSig(retType, paraTypes);
    if (cache.containsKey(typeSig)) {
      return cache.get(typeSig);
    }
    int paramCount = paraTypes.size();
    long[] varTypeIdxArray = new long[paramCount];
    for (int i = 0; i < paramCount; i++) {
      varTypeIdxArray[i] = getType(paraTypes.get(i), TyAttr.TY_ATTR_NULL);
    }
    long retTypeIdx = getType(retType, TyAttr.TY_ATTR_NULL);
    long typeIdx = BGenDriver.createFunctionType(retTypeIdx, varTypeIdxArray, BCRConfig.DEFAULT_PU_ALIGN);
    cache.put(typeSig, typeIdx);
    return typeIdx;
  }

  private static long getType(Type type, TyAttr typeAttr) {
    long id = -1;
    if (cache.containsKey(type)) {
      id = cache.get(type);
    } else if (type instanceof PrimType || type instanceof VoidType || type instanceof NullType || type instanceof BottomType) {
      id = BGenDriver.getPrimitiveType(JavaToWhirlConst.getMType(type));
      cache.put(type, id);
    } else if (type instanceof RefType) {
      SootClass clazz = ((RefType) type).getSootClass();
      long pointedIdx = getType(clazz);
      id = BGenDriver.createRefType(ANON_PTR, pointedIdx);
    } else if (type instanceof ArrayType) {
      long pointed_type = 0;
      if (((ArrayType) type).numDimensions == 1) {
        long elem_type = getType(((ArrayType) type).getElementType(), TyAttr.TY_ATTR_NULL);
        pointed_type = createGCJVariateLengthyArray(type.toString(), elem_type);
      } else {
        pointed_type = createGCJMultiArray((ArrayType) type); // Array of array
      }
      id = BGenDriver.createRefType(ANON_PTR, pointed_type);
      cache.put(type, id);
    } else {
      assertThat(false ).as( "not support type: " + type.toString()).isTrue();
    }
    if (typeAttr != TyAttr.TY_ATTR_NULL) {
      id = BGenDriver.setTypeAttribute(id, typeAttr.toInt());
    }
    return id;
  }

  private static String getMethodTypeSig(Type retType, List<Type> paraTypes) {
    StringBuilder sig = new StringBuilder();
    sig.append(retType.toString());
    sig.append('(');
    for (int i = 0; i < paraTypes.size(); i++) {
      sig.append(paraTypes.get(i).toString());
      if (i != paraTypes.size() - 1) {
        sig.append(',');
      }
    }
    sig.append(')');
    return sig.toString();
  }

  private static long createGCJVariateLengthyArray(String name, long elem_ty) {
    long id = 0;
    if (cacheClass.containsKey(name)) {
      id = cacheClass.get(name);
    } else {
      SootClass objClazz = Scene.v().getSootClass(SootConstants.CLASS_OBJECT);
      id = BGenDriver.createClassWithFields(name,
              new String[]{".vptr_primArray", ".anonymous", "length", "data"},
              new long[]{getInternal(GCJType.GCJ_VTABLE_PTR),
                      TypeHandler.getType(objClazz),
                      BGenDriver.getPrimitiveType(Mtype.MTYPE_I4.toInt()),
                      BGenDriver.createCommonArrayType(0, elem_ty, 1, 0, 0, 0)
                      // FIXME: add multiple WN to here
              });
      BGenDriver.setMiscFlags(id, WhirlConstants.FlagKind.FLAG_TYFLAG.toInt(),
                              WhirlConstants.TyFlag.TY_IS_ARRAY_CLASS.toInt() | 
                              WhirlConstants.TyFlag.TY_CONTENT_SEEN.toInt());
      cacheClass.put(name, id);
    }
    return id;
  }

  private static long createGCJMultiArray(ArrayType type) {
    String name = type.toString();
    if (type.numDimensions == 1) {
      return createGCJVariateLengthyArray(name, getType(type.baseType));
    } else {
      if (cacheClass.containsKey(name)) {
        return cacheClass.get(name);
      }
      long elemTy = createGCJMultiArray((ArrayType) type.getElementType()); //dimension >= 2
      long tyIdx = createGCJVariateLengthyArray(name, elemTy);
      cacheClass.put(name, tyIdx);
      return tyIdx;
    }
  }

  // ==================================== For Class GCJType ========================================

  private static Map<SootClass, Map<SootField, Integer>> fieldIdCache = new HashMap<>();
  private static Map<SootClass, Integer> classExpandFieldCntCache = new HashMap<> ();

  static long getType(SootClass clazz) {
    if(Options.v().src_prec() == Options.src_prec_apk && clazz.resolvingLevel() < SootClass.SIGNATURES) {
      logger.debug("  [B2W FIX FOR SOOT getType(SootClass): try resolve class " + clazz.getName() + "with SIGNATURES");
      try {
        Scene.v().forceResolve(clazz.getName(), SootClass.SIGNATURES);
      }catch (soot.SootResolver.SootClassNotFoundException e) {
        // skip the exception
        logger.debug("[B2W FIX FOR SOOT getType(SootClass): resolve failed" + clazz.getName());
      }
    }
    return getType(clazz, false);
  }

  static long getType(SootClass clazz, boolean generateRelated) {

    if (cache.containsKey(clazz)) {
      return cache.get(clazz);
    }
    if (clazz.hasSuperclass()) {
      getType(clazz.getSuperclass());
    }
    if(!generateRelated) {
      generateRelated = Scene.v().getApplicationClasses().contains(clazz);
    }
    List<SootField> unResolvedFieldTypes = new ArrayList<>();
    if (generateRelated) {
      for (SootClass i : clazz.getInterfaces()) {
        getType(i);
      }
    }
    long clazzTypeIdx = BGenDriver.createClassType(clazz.getName(), BCRConfig.DEFAULT_CLASS_SIZE, 0);
    cache.put(clazz, clazzTypeIdx);
    long startFieldIdx = BGenDriver.jniGetCurrentFieldIdx();
    long vptrFieldIdx = BGenDriver.allocateSpotInClass(clazzTypeIdx, MangleTool.mangleVptr(clazz.getShortName()), 0);
    long tmpFildIdx = 0;
    if (clazz.hasSuperclass()) {
      tmpFildIdx = BGenDriver.allocateSpotInClass(clazzTypeIdx, clazz.getSuperclass().getName(), 0);
    }
    // WHIRL required a sequential field index, so we should allocate them first, and then set field type
    Map<SootField, Long> fieldIdxMap = new HashMap<>();
    for (SootField field: clazz.getFields()) {
      if (field.isStatic()) {
        continue;
      }
      long fieldIdx = BGenDriver.allocateSpotInClass(clazzTypeIdx, field.getName(), 0);
      fieldIdxMap.put(field, fieldIdx);
    }
    long endFieldIdx = BGenDriver.jniGetCurrentFieldIdx() - 1;
    long vptrTypeIdx = getInternal(GCJType.V_PTR_PTR);
    BGenDriver.jniSetFldType(clazzTypeIdx, vptrFieldIdx, vptrTypeIdx, 0);
    if (clazz.hasSuperclass()) {
      long superClassTypeIdx = getType(clazz.getSuperclass());
      BGenDriver.jniSetFldType(
        clazzTypeIdx, tmpFildIdx, superClassTypeIdx, 0
      );
      //BGenDriver.jniSetFldBaseClass(tmpFildIdx);
    }
    for (SootField field : clazz.getFields()) {
      if (field.isStatic()) {
        continue;
      }
      long fieldTypeIdx;
      Type fieldType = field.getType();
      // generate a pointer to void for reference type and Array GCJType field first , and add them to a list
      // for later resolve
      if(fieldType instanceof RefType || fieldType instanceof ArrayType) {
        fieldTypeIdx = BGenDriver.jniMakePointerType(BGenDriver.getPrimitiveType(Mtype.MTYPE_V.toInt()));
        if(generateRelated) {
          unResolvedFieldTypes.add(field);
        }
      } else {
        fieldTypeIdx = getType(fieldType);
      }
      BGenDriver.jniSetFldType(clazzTypeIdx, fieldIdxMap.get(field), fieldTypeIdx, 0);
    }
    BGenDriver.finalizeFields(clazzTypeIdx, startFieldIdx, endFieldIdx);

    // Resolve field type: get the real type if type is defined by user code
    if(generateRelated) {
      for (int fieldIdx = 0; fieldIdx < unResolvedFieldTypes.size(); fieldIdx++) {
        SootField field = unResolvedFieldTypes.get(fieldIdx);
        Type fieldType = field.getType();
        assertThat(fieldType instanceof RefType || fieldType instanceof ArrayType).isTrue().
          as("unresolved type should be reference or array type");
        long fieldTypeIdx = getType(fieldType);
        BGenDriver.jniSetFldType(clazzTypeIdx, fieldIdxMap.get(field), fieldTypeIdx, 0);
      }
    }
    return clazzTypeIdx;
  }

  /**
   * recursive add field to map, every class need all super class's fields and it's own fields in map
   * a given field, with different base type class, has different field offset
   * @param fieldOffsetMap field offset map
   * @param clazz field declared class
   * @param startOffset start offset, entry class need pass 0
   * @return
   */
  private static int addField(Map<SootField, Integer> fieldOffsetMap, SootClass clazz, int startOffset) {
    // vptr offset
    startOffset += 1;
    // super class offset add one
    if (clazz.hasSuperclass()) {
      startOffset = addField(fieldOffsetMap, clazz.getSuperclass(), startOffset + 1);
    }
    for (SootField field: clazz.getFields()) {
      if (field.isStatic()) {
        continue;
      }
      fieldOffsetMap.put(field, ++startOffset);
    }
    return startOffset;
  }

  static long getFieldIdx(SootFieldRef fieldRef) {
    SootClass clazz = fieldRef.declaringClass();
    SootField field = fieldRef.resolve();
    if (fieldIdCache.containsKey(clazz)) {
      assertThat(fieldIdCache.get(clazz).containsKey(field)).as("field offset cache contains class, " +
        "but don't contains field, class:" + clazz + ", field : " + field).isTrue();
      return fieldIdCache.get(clazz).get(field);
    }
    getType(fieldRef.declaringClass());
    Map<SootField, Integer> fieldOffsetMap = new HashMap<>();
    addField(fieldOffsetMap, clazz, 0);
    fieldIdCache.put(clazz, fieldOffsetMap);
    assertThat(fieldIdCache.containsKey(clazz)).as("class type resolve error, can't get field id, class : " + clazz);
    assertThat(fieldIdCache.get(clazz).containsKey(field)).as("field offset cache contains class, " +
      "but don't contains field, class:" + clazz + ", field : " + field).isTrue();
    return fieldIdCache.get(clazz).get(field);
  }

  private static Map<GCJType, Long> internalCache = new HashMap<>();

  static long getInternal(GCJType GCJTypeInfo) {
    long typeIdx;
    long u2 = BGenDriver.getPrimitiveType(Mtype.MTYPE_U2.toInt());
    long u4 = BGenDriver.getPrimitiveType(Mtype.MTYPE_U4.toInt());
    long i8 = BGenDriver.getPrimitiveType(Mtype.MTYPE_I8.toInt());
    long i4 = BGenDriver.getPrimitiveType(Mtype.MTYPE_I4.toInt());
    long i2 = BGenDriver.getPrimitiveType(Mtype.MTYPE_I2.toInt());
    long i1 = BGenDriver.getPrimitiveType(Mtype.MTYPE_I1.toInt());
    if (internalCache.containsKey(GCJTypeInfo)) {
      return internalCache.get(GCJTypeInfo);
    }
    switch (GCJTypeInfo) {
      case JAVA_LANG_OBJECT: {
        typeIdx = getType(Scene.v().getSootClass(SootConstants.CLASS_OBJECT));
        break;
      }
      case JAVA_LANG_OBJECT_PTR: {
        typeIdx = BGenDriver.jniMakePointerType(getInternal(GCJType.JAVA_LANG_OBJECT));
        break;
      }
      case JAVA_LANG_THROWABLE: {
        typeIdx = getType(Scene.v().getSootClass(SootConstants.CLASS_THROWABLE));
        break;
      }
      case JAVA_LANG_THROWABLE_PTR: {
        typeIdx = BGenDriver.jniMakePointerType(getInternal(GCJType.JAVA_LANG_THROWABLE));
        break;
      }
      case JAVA_LANG_CLASS: {
        long vptr = getInternal(GCJType.V_PTR);
        long methodptr = vptr, otableptr = vptr, atableptr = vptr, threadptr = vptr;
        long classloaderptr = vptr, objectArrPtr = vptr, engineptr = vptr, utfptr = vptr,
             methodsymptr = vptr, protectionPtr = vptr, typeAssertionPtr = vptr;
        long catchptr = vptr; //FIXME: recursive getInternal(GCJType.GCJ_CATCH_PTR);
        long classptr = vptr; //FIXME: recursive getInternal(GCJType.JAVA_LANG_CLASS_PTR);
        long fieldptr = vptr; //FIXME: getInternal(GCJType.GCJ_FIELD_PTR);
        long u1ptr = getInternal(GCJType.U1_PTR);
        long vtableptr = vptr; //FIXME: getInternal(GCJType.GCJ_VTABLE_PTR);
        String[] fieldName = new String[] {
        ".vptr_abi",
        "next_or_version", // Chain for class pool.  This also doubles as the ABI version number.
        "name", //_Jv_Utf8Const * Name of class.
        "accflags",  // _Jv_ushort accflags; Access flags for class.
        "superclass", // jclass uperclass; The superclass, or null for Object.
        "constants",  // _Jv_Constants constants
        "methods",  // If array, element class *
        "method_count", // If primitive, character used to represent this type in Signiture
        "vtable_method_count", //Vtable's method count
        "fields", //fields
        "size_in_bytes", //jint
        "field_count", //jshort
        "static_field_count", //jshort
        "vtable", // Vtalbe ptr
        "otable", //Virtual method offset table _Jv_OffsetTable *otable;
        "otable_syms", //_Jv_MethodSymbol *otable_syms
        "atable", //_Jv_AddressTable Address Table
        "atable_syms",// _Jv_MethodSymbol
        "itable", //_Jv_AddressTable Interface Table
        "itable_syms",//_Jv_MethodSymbol
        "catch_classes", //_Jv_CatchClass *
        "interfaces", // jclass *
        "loader", // java::lang::ClassLoader *
        "interface_count", //jshort
        "state", //jbyte State of this class.
        "thread", //java::lang::Thread * The thread which has locked this class.  Used during class init
        "depth", // jshort How many levels of "extends" this class is removed from Obj
        "ancestors", // jclass * Vector of this class's superclasses, ordered by decreasing
        "idt", // _Jv_IDispatchTable *idt (for regular) / jshor* ioffsets (for interfaces)
        "arrayclass", // jclass Pointer to the class that represents an array of this class
        "protectionDomain", //java::security::ProtectionDomain * Security Domain to which this class belongs (or null).
        "assertion_table", //_Jv_TypeAssertion * Pointer to the type assertion table for this class
        "hack_signers", // JArray<jobject> * Signers of this class (or null).
        "chain", // jclass chain Used by Jv_PopClass and _Jv_PushClass to communicate with StackTrace.
        "aux_info", //  void * Additional data, specific to the generator (JIT, native, interpreter) of this class.
        "engine", //_Jv_ExecutionEngine * Execution engine.
        "reflection_data"}; // unsigned char * Reflection data
        long[] types = new long[]{
          vptr, classptr, utfptr,
          i2, classptr, getInternal(GCJType.GCJ_CONSTANTS_TABLE),
          /* 32  methods */ methodptr, i2, i2, fieldptr,
          i4, i2, i2, vtableptr,
          otableptr, methodsymptr,
          atableptr, methodsymptr,
          atableptr, methodsymptr,
          catchptr,  classptr,
          classloaderptr,
          /*_Interface_count */ i2, i1, threadptr, i2,
          classptr, /*union*/ vptr, classptr,
          protectionPtr,
          typeAssertionPtr, objectArrPtr,
          classptr, /*aux_info*/ vptr,
          engineptr,
          u1ptr
        };
        typeIdx = BGenDriver.createClassWithFields(JvName.LANG_CLASS, fieldName, types);
        break;
      }
      case GCJ_CONSTANTS_TABLE:{
        /**
         * struct _Jv_Constants
         * {
         *   jint size;
         *   jbyte *tags;
         *   _Jv_word *data;
         * };
         */
        String[] nameConstants = new String[]{"size", "tags", "data"};
        long vptr = getInternal(GCJType.V_PTR);
        long[] typeConstants = new long[] {u4, vptr, vptr};
        typeIdx = BGenDriver.createClassWithFields("_gcj_constants", nameConstants, typeConstants);
        break;
      }
      case JAVA_LANG_CLASS_PTR: {
        typeIdx = BGenDriver.jniMakePointerType(getInternal(GCJType.JAVA_LANG_CLASS));
        break;
      }
      case JAVA_LANG_CLASS_PTR_PTR: {
        typeIdx = BGenDriver.jniMakePointerType(getInternal(GCJType.JAVA_LANG_CLASS_PTR));
        break;
      }
      case V_PTR: {
        typeIdx = BGenDriver.jniMakePointerType(BGenDriver.getPrimitiveType(Mtype.MTYPE_V.toInt()));
        break;
      }
      case V_PTR_PTR: {
        typeIdx = BGenDriver.jniMakePointerType(getInternal(GCJType.V_PTR));
        break;
      }
      case V_PTR_PTR_PTR: {
        typeIdx = BGenDriver.jniMakePointerType(getInternal(GCJType.V_PTR_PTR));
        break;
      }
      case GCJ_FIELD_OFFSET: {
        typeIdx = getInternal(GCJType.V_PTR); //TODO: Add union here.
        break;
      }
      case GCJ_VTABLE: {
        long ptr_ptr = getInternal(GCJType.V_PTR_PTR);
        String[] names = new String[]{"top_offset", "type_info", "class", "gc_descr", "methods"};
        long [] types = new long[]{ ptr_ptr, ptr_ptr,
        getInternal(GCJType.JAVA_LANG_CLASS_PTR),
        getInternal(GCJType.V_PTR),
        getInternal(GCJType.V_PTR)};
        typeIdx = BGenDriver.createClassWithFields(JvName._JV_VTABLE, names, types);
        break;
      }
      case GCJ_VTABLE_PTR: {
        typeIdx = BGenDriver.jniMakePointerType(getInternal(GCJType.GCJ_VTABLE));
        break;
      }
      case GCJ_METHOD: {
        String[] names = new String[]{"name","signature", "accflags", "index", "ncode", "throws"};
        long[] fields = new long[]{
        getInternal(GCJType.GCJ_UTF8CONST_PTR),
        getInternal(GCJType.GCJ_UTF8CONST_PTR),
        u2, i2, //i2 is a compromisation where Jv_Method is declared as ushort, but gcj set it to -1!
        getInternal(GCJType.V_PTR),
        getInternal(GCJType.V_PTR_PTR)};
        typeIdx = BGenDriver.createClassWithFields(JvName._JV_METHOD, names, fields);
        break;
      }
      case GCJ_METHOD_PTR: {
        typeIdx = BGenDriver.jniMakePointerType(getInternal(GCJType.GCJ_METHOD));
        break;
      }
      case GCJ_FIELD: {
        String[] names = new String[]{"name", "type", "flags", "bsize", "u_ofst_or_addr"};
        long[] fields = new long[]{getInternal(GCJType.GCJ_UTF8CONST_PTR),
        getInternal(GCJType.JAVA_LANG_CLASS_PTR), u2, u2,
        getInternal(GCJType.GCJ_FIELD_OFFSET)};
        typeIdx = BGenDriver.createClassWithFields(".anon.fields.gcj.table", names, fields);
        break;
      }
      case GCJ_METHOD_SYMBOL: {
        /** struct _Jv_MethodSymbol
         *        *   _Jv_Utf8Const *class_name;
         *        *   _Jv_Utf8Const *name;
         *        *   _Jv_Utf8Const *signature;  */
        typeIdx = getInternal(GCJType.V_PTR);
        break;
      }
      case GCJ_METHOD_SYMBOL_PTR: {
        typeIdx = getInternal(GCJType.V_PTR_PTR);
        break;
      }
      case GCJ_ATABLE:{
       /** struct _Jv_AddressTable
       *   jint state;
       *   void *addresses[]; */
        typeIdx = getInternal(GCJType.V_PTR);
        break;
      }
      case GCJ_ATABLE_PTR:{
        typeIdx = getInternal(GCJType.V_PTR);
        break;
      }
      case GCJ_OTABLE:{
         //* struct _Jv_OffsetTable
         //*   jint state;
         //*   jint offsets[];
        typeIdx = getInternal(GCJType.V_PTR);
        break;
      }
      case GCJ_OTABLE_PTR:{
        typeIdx = getInternal(GCJType.V_PTR);
        break;
      }
      case U1_PTR:{
        typeIdx = BGenDriver.jniMakePointerType(BGenDriver.getPrimitiveType(Mtype.MTYPE_U1.toInt()));
        break;
      }
      case U2_PTR:{
        typeIdx = BGenDriver.jniMakePointerType(BGenDriver.getPrimitiveType(Mtype.MTYPE_U2.toInt()));
        break;
      }
      case U4_PTR:{
        typeIdx = BGenDriver.jniMakePointerType(BGenDriver.getPrimitiveType(Mtype.MTYPE_U4.toInt()));
        break;
      }
      case U8_PTR:{
        typeIdx = BGenDriver.jniMakePointerType(BGenDriver.getPrimitiveType(Mtype.MTYPE_U8.toInt()));
        break;
      }
      case GCJ_FIELD_PTR: {
        typeIdx = BGenDriver.jniMakePointerType(getInternal(GCJType.GCJ_FIELD));
        break;
      }
      case GCJ_CATCH: {
        /***
         * struct _Jv_CatchClass
         * {
         *   java::lang::Class **address;
         *   _Jv_Utf8Const *classname;
         * };
         */
        long[] fields = new long[]{
                getInternal(GCJType.JAVA_LANG_CLASS_PTR_PTR),
                getInternal(GCJType.GCJ_UTF8CONST_PTR)};
        String[] names = new String[]{"address", "classname"};
        typeIdx = BGenDriver.createClassWithFields(".anon.catches.gcj.table", names, fields);
        break;
      }
      case GCJ_CATCH_PTR: {
        typeIdx = BGenDriver.jniMakePointerType(getInternal(GCJType.GCJ_CATCH));
        break;
      }
      case GCJ_UTF8CONST: {
        assertThat(false).as( "[TypeHandler::getInternal] DO NOT USE getInternal(GCJ_UTF8CONST), use getUtfStringType(int len) instead!").isTrue();
      }
      case GCJ_UTF8CONST_PTR: {
        typeIdx = getInternal(GCJType.V_PTR);
        break;
      }
      case GCJ_UTF8CONST_PTR_PTR: {
        typeIdx = getInternal(GCJType.V_PTR_PTR);
        break;
      }
      case TYPE_INFO: {
        typeIdx = BGenDriver.createClassType(
          "__type_info_pseudo", 1,0);
        long[] fields = new long[2];
        fields[0] = BGenDriver.allocateSpotInClass(typeIdx, ".abi_vtable", 0);
        fields[1] = BGenDriver.allocateSpotInClass(typeIdx, ".abi_typename", 0);
        setFldAsPointerEx(typeIdx, fields[0]);
        setFldAsPointerEx(typeIdx, fields[1]);
        BGenDriver.finalizeFields(typeIdx, fields[0], fields[1]);
        break;
      }
      // Used for non-inherit
      case CLASS_TYPE_INFO: {
        // assert if is doing allocate field of another class.
        typeIdx = BGenDriver.createClassType("__class_type_info_pseudo", BCRConfig.DEFAULT_CLASS_SIZE, 0);
        long field = BGenDriver.allocateSpotInClass(typeIdx, ".anonymous.typeinfo", 0);
        BGenDriver.jniSetFldType(typeIdx, field, getInternal(GCJType.TYPE_INFO), 0);
        BGenDriver.finalizeFields(typeIdx, field, field);
        break;
      }
      case VMI_CLASS_TYPE_INFO: {
        typeIdx = BGenDriver.createClassType("__vmi_type_info_pseudo", BCRConfig.DEFAULT_CLASS_SIZE, 0);
        long field = BGenDriver.allocateSpotInClass(typeIdx, ".anonymous.typeinfo", 0);
        BGenDriver.jniSetFldType(typeIdx, field, getInternal(GCJType.TYPE_INFO), 0);
        BGenDriver.finalizeFields(typeIdx, field, field);
        break;
      }
      case BASE_CLASS_TYPE_INFO: {
        typeIdx = BGenDriver.createClassType("__base_class_type_info_pseudo", BCRConfig.DEFAULT_CLASS_SIZE, 0);
        long[] fields  = new long[2];
        fields[0] = BGenDriver.allocateSpotInClass(typeIdx, ".anon_ptr", 0);
        fields[1] = BGenDriver.allocateSpotInClass(typeIdx, "anonymous", 0);
        BGenDriver.jniSetFldType(typeIdx, fields[1],
          getInternal(GCJType.TYPE_INFO), 0);
        BGenDriver.jniSetFldType(typeIdx, fields[1],
          BGenDriver.getPrimitiveType(Mtype.MTYPE_I8.toInt()), 0);
        BGenDriver.finalizeFields(typeIdx, fields[0], fields[1]);
        break;
      }
      case EH_REGION_TABLE: {
        long elt = BGenDriver.getPrimitiveType(Mtype.MTYPE_U4.toInt());
        typeIdx = getSubTypeArray(elt, 2, 4); // Align : sizeof(U4);
        break;
      }
      default: {
        assertThat(false ).as( ("[TypeHandler::getInternal] Cannot determine internal's ")).isTrue();
        return 0;
      }
    }
    internalCache.put(GCJTypeInfo, typeIdx);
    return typeIdx;
  }

  private static void setFldAsPointerEx(long cls_type, long field) {
    BGenDriver.jniSetFldType(cls_type, field, getInternal(GCJType.V_PTR), 0);
  }

  static Map<Integer, Long> stringArr = new HashMap<>();
  static Map<Integer, Long> stringUtf8Arr = new HashMap<>();

  static long getStringArray(int len) {
    if (stringArr.containsKey(len)) {
      return stringArr.get(len);
    }
    long dataType = getSubTypeArray(
      BGenDriver.getPrimitiveType(Mtype.MTYPE_U1.toInt()), len, 1);
    stringArr.put(len, dataType);
    return dataType;
  }

  /***
   * Please add 1 (\000) to the length given;
   * for example string "apple" has size == 6
   * @param len WITH ENDIAN INCLUDED
   * @return
   */
  static long getUtfStringType(int len){
    if (stringUtf8Arr.containsKey(len)) {
      return stringUtf8Arr.get(len);
    }
    long u2Type = BGenDriver.getPrimitiveType(Mtype.MTYPE_U2.toInt());
    int alignment = (int) BGenDriver.getTySize(u2Type);
    long typeIdx = BGenDriver.createClassType(JvName._JV_UTF8CONST + "_" + len,
            BCRConfig.DEFAULT_CLASS_SIZE, // no-use
            alignment);
    long dataType = getStringArray(len);
    long[] fields = new long[3];
    fields[0] = BGenDriver.allocateSpotInClass(typeIdx, "hash", 0);
    fields[1] = BGenDriver.allocateSpotInClass(typeIdx, "length", 0);
    fields[2] = BGenDriver.allocateSpotInClass(typeIdx, "data", 0);
    BGenDriver.jniSetFldType(typeIdx, fields[0], u2Type, 0);
    BGenDriver.jniSetFldType(typeIdx, fields[1], u2Type, 0);
    BGenDriver.jniSetFldType(typeIdx, fields[2], dataType, 0);
    BGenDriver.finalizeFields(typeIdx, fields[0], fields[2]);
    stringUtf8Arr.put(len, typeIdx);
    return typeIdx;
  }

  static SootClass getBoxedClass(Type sf) {
    if(sf instanceof PrimType){
      // Create PrimType For
      return ((PrimType) sf).boxedType().getSootClass();
    }else if(sf instanceof VoidType) {
      // Create Void GCJType
      assertThat(false).as("Field cannot be declared as Void GCJType :" + sf).isTrue();
    }else if(sf instanceof NullType) {
      assertThat(false).as("Field cannot be declared as Null GCJType :" + sf).isTrue();
    }else if(sf instanceof BottomType) {
      //assertThat(sf instanceof BottomType).as("Field cannot be declared as Bottom GCJType :" + sf).isFalse();
    }else if (sf instanceof RefType) {
      RefType refT = (RefType) sf;
      return refT.getSootClass();
    }else if(sf instanceof ArrayType){
      //assertThat(sf instanceof BottomType).as("Field cannot be declared as Bottom GCJType :" + sf).isFalse();
    }else{
      //assertThat(false).as("Cannot box for type : " + sf).isTrue();
    };
    return null;
  }

  public static Type getBottomType(ArrayType type) {
    Type single = type.getElementType();
    if (single instanceof ArrayType) {
      return getBottomType((ArrayType) single);
    } else if (single instanceof PrimType || single instanceof RefType) {
      return single;
    } else if (single instanceof NullType || single instanceof VoidType) {
      assertThat(false).as("cannot accept array of void or null").isTrue();
    }else{
      assertThat(false).as("unknown element type: "+type).isTrue();
    }
    return null;
  }

  public static long getTypeForArrayUse(Type bottomType) {
    if (bottomType instanceof BooleanType) {
      return getType(ByteType.v()); // FIXME: fix for testcase
    } else {
      return getType(bottomType);
    }
  }

  static long getEHRegionSupp(int exceptionNum) {
    exceptionNum = exceptionNum < 1 ? 1 : exceptionNum;
    long eltType =  BGenDriver.getPrimitiveType(Mtype.MTYPE_U4.toInt());
    return TypeHandler.getSubTypeArray(eltType, exceptionNum + 1, BCRConfig.ALIGN);
  }

  public static long getMethodThrowsTable(int throwsCount) {
    long eltType = getInternal(GCJType.GCJ_UTF8CONST_PTR);
    return TypeHandler.getSubTypeArray(eltType, throwsCount, BCRConfig.ALIGN);
  }
}
