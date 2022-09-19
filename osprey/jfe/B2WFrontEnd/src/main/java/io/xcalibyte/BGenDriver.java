/*
 * Copyright (C) 2021 Xcalibyte (Shenzhen) Limited.
 */

package io.xcalibyte;

import java.util.*;
import static org.assertj.core.api.Assertions.*;

public class BGenDriver {

  //============== Initialization & Finalization ==================
  public static native void bgenInit(boolean trace, int logLevel, int alignWidth);
  // called after bgenInit
  public static native void bgenInitOpenIrFile(String outputFilePath);
  // called after bgenOpenIrFile
  public static native void bgenInitSetDST(String inputFilePath);

  // close a file
  public static native void bgenFileClose();
  // finishup all files
  public static native void bgenFileFinish();

  //============== GCJType ==================
  public static native long getPrimitiveType(int Mtype);
                                        /* input: parameter's mtype
                                           function return type : TY_IDX */
  public static native long setTypeAttribute(long idx, int attr);
                                        /* idx = tyIdx, attr = TY_CONST || TY_VOLATILE
                                           function return type : TY_IDX */
  public static native void setTypePuAttribute(long idx, int attr);
                                        /* idx = tyIdx, attr = TyPuFlag
                                           function return type : TY_IDX */
  public static native long createFunctionType(long retIdx, long[] argTyIdx, int align);
                                        /* retIdx = functions' return type, argTyIdx = a series of arguments' tyIdx
                                           align = 1
                                           function return type : TY_IDX  */
  public static native long createClassType(String name, int size, int align);
                                        /* name = the name of the class
                                           size = 1
                                           align = (*)
                                           function return type : TY_IDX  */
  public static native long createRefType(String Name, long pointeeIdx);
                                        /* name = the name of the reference
                                           pointeeIdx = TY_IDX of pointee
                                           function return type : TY_IDX  */

  /*==========================================================================
   * class_ty     = TY_IDX of the class
   * fld_name     = member name
   * offset       = offset in the class
   * return       FLD_IDX
   *==========================================================================*/
  public static native long allocateSpotInClass(long class_ty, String fld_name, long offset);

  /* class_ty     = TY_IDX of pointee */
  public static native long jniMakePointerType(long ty);

  /**********************************************************************
   * Set Miscellaneous Flag
   **********************************************************************/
  public static native void setMiscFlags(long idx, long kind, long flag);

  /**
   * @param size: array size
   * @param elm_ty: TY_IDX of array's element
   * @return: TY_IDX of the array type
   */
  public static native long createArrayType(long size, long elm_ty);

  /**
   * Called when after setting class member fields
   *
   * @param ty: TY_IDX of the class
   * @param first_fld_idx: from jniGetCurrentFieldIdx()
   * @param last_fld_idx: last field
   */
  public static native void finalizeFields(long ty, long first_fld_idx, long last_fld_idx);


  /***
   * Printing GCJType's Info via TY::Print()
   * @param ty
   */
  public static native void printTypeInfo(long ty);

  /***
   * Get GCJType's recorded size, (Note: invalid before calling finalizeFields for structs)
   * @param ty
   */
  public static native long getTySize(long ty);

  /**
   * Set filed as baseclass
   * @param fld_idx : field's index
   */
  public static native void jniSetFldBaseClass(long fld_idx);

  /******************************************************
   * Set an alignment length to a typeIdx
   * @param typeIdx TY_IDX
   * @param align align, default is 8
   *****************************************************/
  public static native void setTyAlign(long typeIdx, int align);

  /*********************************************************
   * Get type's alignment from TY
   * @param typeIdx TY_IDX
   * @return alignment
   *********************************************************/
  public static native int getTyAlign(long typeIdx);


  /*===================================================================================
     @param UINT    size (number of elements)
     @param TY_IDX  element's ty-idx
     @param UINT    align_width
     @param WN     *upper bound wn;
     @param WN     *block(wn to be put in)
     @param WN     *size  wn (normally (uwn) multiplies sizeof(ty))
   ===================================================================================*/
  public static native long createCommonArrayType(long size, long elm_ty, int align,
                                                  long ubound_wn, long block_wn, long size_wn);

  /*===================================================================================
     @param TY_IDX  arr ptr type
     @param WN     *upper bound wn;
   ===================================================================================*/
  public static native void setArbUbd(long arrPtrType, long size_wn);

  /**===================================================================================
   * May be called after allocateSpotInClass
   * @param class_ty : TY_IDX of the class
   * @param fld_idx : field
   * @param typeidx : field TY_IDX
   * @param offset : not used
   * @return FLD_IDX
  ===================================================================================*/
  public static native long jniSetFldType(long class_ty, long fld_idx,
                                          long typeidx, long offset);

  /**===================================================================================
   * @return first avaliable FLD_IDX
  ===================================================================================*/
  public static native long jniGetCurrentFieldIdx();

  /*****************************************************************************
   * B2W_set_fld_as_pointer   Used to setup a field as a pointer,
   * @ param class_ty  TY_IDX of the class being pointed
   * @ param fld       field index
   * @ return          FLD_IDX
   ****************************************************************************/
  @Deprecated
  private static native long jniSetFldAsPointer(long class_ty, long fld);

  /*============================================================================
    Set file is rbc file
   *==========================================================================*/
  public static native void setFileRbc();

  /*============================================================================
  Set file is vtable file
 *==========================================================================*/
  public static native void setFileVTable();
  
  /*============================================================================
    Set PU's language as CXX
   *==========================================================================*/
  public static native void setPuLangCXX(long func_sym);

  /*============================================================================
    Set PU's language as CXX
   *==========================================================================*/
  public static native void setPuLangC(long func_sym);

  /*============================================================================
    Set PU's language as Java
   *==========================================================================*/
  public static native void setPuLangJava(long func_sym);

  /*============================================================================
    Set PU as Class's constructor
   *==========================================================================*/
  public static native void setPuConstructor(long func_sym, long class_ty);

  /*============================================================================
    Set PU to be not returning functions
    This includes
    1. 'exit'            like system-functions
    2. 'throw'           like eh-functions
    3. 'unwind'/'rewind' like eh-functions
   *==========================================================================*/
  public static native void setPuNoReturn(long func_sym);

  public static native void setPuIsMainPu(long func_sym);

  public static native void clearPuIsInlie(long func_sym);

  public static native void setPuNoInline(long func_sym);

  /*============================================================================
    Set PU as rbc
   *==========================================================================*/
  public static native void setCurrPuRbc();

  //============== Symbol ================
  /***========================================================================================
   * Creating function's symbol with given typeIdx
   *==========================================================================================*/
  public static native long createFunctionSymbol(String name, long typeIdx, int sClass, int eClass, int level);

  public static native long createExternFunctionSymbol(String name, long typeIdx);


  public static native void bindFunctionParams(long[] args, long funcSt, long entryWn);

  /**============================================================================================
   * creating a var symbol for Method/ClassSym/Exception_ref/class$/class$$/SystemMethod/SystemCall
   // FIXME: package and class name may have utf-8 character, should convert to c char and append
   * @param name      var symbol's name
   * @param typeIdx   var's typeIdx, primitive/struct/array allowed
   * @param sClass    storage class, see the enum ST_SCLASS
   * @param eClass    export level, see the enum ST_EXPORT
   * @param level     lexical level (global var = 1, local val/parameter = 2)
   * @return ST_IDX   created symbol_idx
  *===========================================================================================*/
  public static native long createVarSymbol(String name, long typeIdx, int sClass, int eClass, int level);

  public static long createVarSymbol(String name, long typeIdx, WhirlConstants.SClass sClass, WhirlConstants.SExport eClass, int level){
    return createVarSymbol(name, typeIdx, sClass.toInt(), eClass.toInt(), level);
  }

  /**============================================================================================
   * set up relation between class type and its vtable
   * @param symIdx    vtable symbol
   * @param typeIdx   the class type which has vtable symbol(symIdx)
   *===========================================================================================*/
  public static native void setVTable(long symIdx, long typeIdx);
  
  /**============================================================================================
   * set up relation between class type and its class symbol
   * @param symIdx    class symbol
   * @param typeIdx   the class type which has class symbol(symIdx)
   *===========================================================================================*/
  public static native void setClassSymbol(long symIdx, long typeIdx);

  /**============================================================================================
   * set class type's copy constructor function
   * @param typeIdx   the class type
   * @param symIdx    the symbol index of copy constructor
   *===========================================================================================*/
  public static native void setClassCopyCtor(long typeIdx, long symIdx);

  /**
   * Get the symbol for a mtype's preg, (already created during initialization)
   * @param mtype : mtype
   * @return : ST_IDX
   */
  public static native long createPregSymbol(int mtype);

  /*
     create a preg, to be used for return val, params, middle vars
  */
  public static native long createPreg(int mtype, String name);

  /*=============================
   * ST_IDX -> ST *
   *============================*/
  public static native long getStPtr(long st_idx);


  /***
   * Printing GCJType's Info via ST::Print()
   * @param ty
   */
  public static native void printSymbolInfo(long ty);

  /***
   * Initializing a static ST with values(in masked mode),
   *  see detials in WhirlConstant.Initial
   * @param values  array of masked initialing data
   * @param st_idx  st to be initialized
   * @return INITO_IDX the initialized inito
   */
  public static native long initializeWithArray(long[] values, long st_idx);

  public static long initializeWithList(List<Long> initValueList, long st_idx) {
    long[] initValueArray = new long[initValueList.size()];
    for(int i = 0; i < initValueList.size(); i++){
      assert initValueList.get(i) != null :
        ("[SymbolInitializer::getInstance] got a null on " + i + "th argument");
      initValueArray[i] = initValueList.get(i);
    }
    return initializeWithArray(initValueArray, st_idx);
  }

  /**
   * @return return value preg's ST_IDX
   */
  public static native long getReturnValPreg();


  //============== JNI(WN API) ===============

  /**
   * @return block wn
   */
  public static native long jniCreateBlock();
  public static native long jniCreateRegion(int kind, long body, long pragma, long exits, int initoIdx);

  public static native long jniMapTabCreate(long poolPtr);

  public static native long jniPoolInitialize();

  public static native long jniNewScope(long poolPtr);

  /**
   * @param wn : wn address
   * @param line_num: line_num from soot
   */
  public static native void jniSetLineNum(long wn, long line_num);

  public static native int jniGetLineNum(long wn);

  public static native void jniSetCurrentSrcFile(String fileName, int file_system_flag);

  public static native long jniCreateIf(long test, long then_block, long else_block);

  public static native long jniCreateTrueBr(long labelNum, long cmpWn);

  public static native long jniCreateFalseBr(long labelNum, long cmpWn);

  public static native void jniInsertBlockFirst(long block, long stmt);

  public static native void jniInsertBlockLast(long block, long stmt);

  public static native void jniInsertBlockBefore(long block, long stmt, long pos);

  public static native void jniInsertBlockAfter(long block, long stmt, long pos);

  public static native long jniGetParentBlock(long stmt);

  public static native long jniLDA(int res_mtype, long offset, long sym_idx);

  public static native long jniLDID(int res_mtype, long offset, long sym_idx, long ret_ty_idx);

  public static native long jniLDIDField(int mtype, long offset, long sym_idx, long ty_idx, long field_id);

  public static native long jniSTID(int desc_mtype, long offset, long sym_idx, long ty_idx, long val);

  /**
   * store a value into member field
   * @param ret_mtype : WN mtype
   * @param offset
   * @param ty_idx : class obj type, not pointer type
   * @param addr_wn : class obj pointer, should be ldid
   * @param field_id : member field index, attention, not field idx, but field index, start from 1
   * @return
   */
  public static native long jniILoad(int ret_mtype, long offset, long ty_idx, long addr_wn, long field_id);

  /**
   * store a value into member field
   * @param ret_mtype : WN mtype
   * @param offset
   * @param ty_idx : class obj pointer type
   * @param addr_wn : class obj pointer, should be ldid
   * @param value_wn : right value
   * @param field_id : member field index, attention, not field idx, but field index, start from 1
   * @return
   */
  public static native long jniIStore(int ret_mtype, long offset, long ty_idx, long addr_wn, long value_wn, long field_id);

  public static native long jniSTIDField(int ret_mtype, long offset, long sym_idx, long ty_idx, long val, long field_id);

  public static native long getBodyFromEntry(long entry);

  public static native long getPointeeTyFromTY(long ty);

  public static native long getTyFromST(long st_idx);

  /**
   * @param wn_ptr: wn address
   * @return : rtype of wn
   */
  public static native int  getRtypeFromWN(long wn_ptr);

  public static native int  getMTypeFromTypeIdx(long type_idx);

  public static native long jniCreateBinary(int opr, long ret_mtype, long lfs, long rhs);

  public static native long jniCreateUnary(int opr, long ret_mtype, long rhs);

  public static native long jniCreateTernary(int opr, long ret_mtype, long kid0, long kid1, long kid2);

  public static native long jniIntConst(int mtype, long value);

  public static native long jniFloatConst(int mtype, double value);

  public static native long jniCreateReturn();

  public static native long jniCreateReturnVal(long val);

  public static native long funcICall(long rtype, long desc, int arg_num, long ty_idx);

  public static native long printWN(long wn);

  //-------------- Conversion ---------------------
  // from int to to_type
  public static native long intTypeConversion(long wn, byte/* TY_ID = UINT8*/ to_type);
  // from float to to_type
  public static native long floatTypeConversion(long wn, byte/* TY_ID = UINT8*/ to_type);
  // from from_type to to_type
  public static native long typeConversion(long wn, byte/* TY_ID = UINT8*/ to_type);

  //-------------- Unary Operatotors -------------
  public static native long jniIlda(int opr, int rtype, int desc, int offset, long ty);

  public static native long jniExtractBits(int rtype, int desc, long kid0);

  public static native long jniParm(int rtype, long kid0, long ty, int flag);

  public static native long jniAsmInput(String name, int opnd_num, long kid0);

  public static native long jniAlloca(long kid0);

  //-------------- Binary Operatotors -------------
  public static native long jniComma(int rtype, int desc, long lhs, long rhs);

  public static native long jniRcomma(int rtype, int desc, long lhs, long rhs);

  public static native long jniRelational(int opr, int rtype, long lhs, long rhs);

  public static native long jniLabel();

  public static native long jniCreateGoto(long label_wn, int linenum);

  public static native long getLabelNum(long label_wn);

  public static native long jniCreateSwitch(int num_entries, long value, long block, long deflt, long last_label_number);

  public static native long jniCreateCasegoto(long case_value, long case_label_number);

  public static native long jniCall(int return_mtype, int desc, int arg_num, long st_idx);

  // WHIRL Array operator
  // @array_base: the address of array
  // @dims: size 2n, where n is the dim of the array.
  //    dim[0]-dim[n-1] size of each dim, dim[n]-dim[2n-1]: index in each dim.
  public static native long jniArray(long array_base, long[] dims, long ty_idx);

  // misc
  public static native void jniSetWNKid(long wn, int idx, long kid);

  // Call flags
  public static native void jniSetCallFlag(long wn, int flag);

  public static native long mtypeToPreg(int mtype);

  /**
   *
   * @param opr : intrinsic type, call or opr
   * @param mtype : opr type
   * @param desc : default set to MTYPE_V
   * @return : return opr num
   */
  public static native int jniOpcodeMakeOp(int opr, int mtype, int desc);

  /**
   *
   * @param opc : opcode that the intrinsic accept
   * @param iopc : intrinsic num code
   * @param kid_num : opcode operator num
   * @param kids : opcode kids, must be parameter WN
   * @return : intrinsic WN, if op is OPR_INTRINSIC_CALL, we should convert it to comma expr
   */
  public static native long jniCreateIntrinsic(int opc, int iopc, int kid_num, long[] kids);

  // *** Scope Must Be 1(Global)
  public static native long startFunction(long sym_id, long args_count, int scope, int line);

  public static native void finishFunction(int src_pos);

  static native long jniNewInito(long st_idx);

  //============== Tracing ======================
  private static native void outputTrace(String str);

  static void info(String str){
      outputTrace(str);
  }

  static void setFuncParm(long call_wn, long[] args) {
    for (int i = 0; i < args.length; i++)
      jniSetWNKid(call_wn, i, args[i]);
  }

  // retTypeIdx: if no return, value set to -1
  static long createIntrinsic(int opr, int rMtype, long retTypeIdx, int iopc, int[] kidsMtypes, long[] kidsType, long[] kids) {
    assert opr == WhirlConstants.Operator.OPR_INTRINSIC_CALL.toInt()
      || opr == WhirlConstants.Operator.OPR_INTRINSIC_OP.toInt():
      "Intrinsic opr is not allowed, opr : " + opr;

    long[] parms = new long[kids.length];
    for(int i = 0; i < kids.length; i++) {
      parms[i] = jniParm(kidsMtypes[i], kids[i], kidsType[i], WhirlConstants.Flag.WN_PARM_BY_VALUE.toInt());
    }

    int opc = jniOpcodeMakeOp(opr, rMtype, WhirlConstants.Mtype.MTYPE_V.toInt());
    long intrinsticWN = jniCreateIntrinsic(opc, iopc, parms.length, parms);
    if(iopc == WhirlConstants.Intrinsic.INTRN_ALLOC_OBJ.toInt() ||
       iopc == WhirlConstants.Intrinsic.INTRN_NEW_MULTI_ARR.toInt() ||
       iopc == WhirlConstants.Intrinsic.INTRN_NEW_OBJ_ARR.toInt() ||
       iopc == WhirlConstants.Intrinsic.INTRN_NEW_PRIM_ARR.toInt()) {
      jniSetCallFlag(intrinsticWN, WhirlConstants.CallFlags.WN_CALL_DOES_MEM_ALLOC.toInt());
    }
    if(opr == WhirlConstants.Operator.OPR_INTRINSIC_CALL.toInt()) {
      if(rMtype != WhirlConstants.Mtype.MTYPE_V.toInt()) {
        long blockWN = jniCreateBlock();
        jniInsertBlockLast(blockWN, intrinsticWN);
        long regIdx = getReturnValPreg();
        long ldidWN = jniLDID(rMtype, -1, regIdx, retTypeIdx);
        B2WGenerator.requestLineNum(blockWN, intrinsticWN);
        return jniComma(
          rMtype, WhirlConstants.Mtype.MTYPE_V.toInt(), blockWN, ldidWN
        );
      } else {
        return intrinsticWN;
      }
    } else {
      return intrinsticWN;
    }
  }

  static long createCallStmt(int rtype, int desc, long symIdx, int[] kidsMtype,  long[] kidsType, long[] kids, long currSrcPos) {
    assertThat(kidsMtype != null && kids != null).as("kinsMtype is null or kids is null.").isTrue();
    assertThat(kidsMtype.length == kids.length).as("kidsMtype length : " + kidsMtype.length +
      ", kids length : " + kids.length).isTrue();
    long callWN = jniCall(rtype, desc, kids.length, symIdx);
    BGenDriver.jniSetLineNum(callWN, currSrcPos);
    for (int i = 0; i < kids.length; i++) {
      int type = kidsMtype[i];
      long parm = jniParm(type, kids[i], kidsType[i], WhirlConstants.Flag.WN_PARM_BY_VALUE.toInt());
      jniSetWNKid(callWN, i, parm);
    }
    int vType = WhirlConstants.Mtype.MTYPE_V.toInt();
    if (rtype != vType) {
      long blockWN = jniCreateBlock();
      jniInsertBlockLast(blockWN, callWN);
      long ldidWN = jniLDID(rtype, -1, getReturnValPreg(), getPrimitiveType(rtype));
      B2WGenerator.requestLineNum(blockWN, callWN);
      return jniComma(rtype, vType, blockWN, ldidWN);
    }
    return callWN;
  }

  static long createClassWithFields(String name, String[] fldNames, long[] types) {
    int alignment = BCRConfig.ALIGN;
    assertThat(alignment).isGreaterThanOrEqualTo(0);
    long typeIdx = createClassType(name, BCRConfig.DEFAULT_CLASS_SIZE, alignment);
    int len = fldNames.length;
    List<Long> fields = new ArrayList<>();
    Map<String, List<Object>> classFields = new HashMap<>();
    assertThat(len).isEqualTo(types.length).as("[createClassWithFields] name & type length mismatch " + fldNames.length + "," + types.length);
    for (int i = 0; i < len; i++) {
      fields.add(allocateSpotInClass(typeIdx, fldNames[i], 0));
    }
    for (int i = 0; i < len; i++) {
      jniSetFldType(typeIdx, fields.get(i), types[i], 0);
    }
    finalizeFields(typeIdx, fields.get(0), fields.get(len - 1));
    classFields.put("fld_idx", Collections.singletonList(fields));
    classFields.put("types", Collections.singletonList(types));
    classFields.put("names", Arrays.asList((Object[]) fldNames));
    return typeIdx;
  }

  //============== JNI(Exception Handling API) ===============
  static native void jniInitEHSym(long[] ehThrowsSym);
  public static native long jniGetExecPtr();
  public static native void jniBuildEHBegin();
  public static native long jniBuildTryCatch(long beginWn, long endWn, long parentHandlerWN, long[] handlerWn, long[] handlerSt);
  public static native void jniBuildEHEnd();

  /***============================================================
   * Setup REGION_SUPP for target region, should use after jniBuildTryCatch and before jniBuildEHEnd;
   * @param regionWn regionWn to set regionSupp to
   * @param comparatorLabel comparatorLabel, label for filter(comparator) must be already dealt with in jniBuildTryCatch.
   * @param ehRegionSuppSym Call SymbolHandler.getEHHandlerSymbol for getting an symIdx that fits catchSymList
   * @param catchSymList the catch ptr symbol list
  ===============================================================*/
  public static native void jniSetupRegionInito(long regionWn, long comparatorLabel, long ehRegionSuppSym, long[] catchSymList);

  /***============================================================
   * Create a new Region.
   * @param body   wn of block
   * @param exit   wn of block
   * @param pragma wn of block
   * @param suppInito  can be retreived from initialize with array (for the eh_table symbol)
  ===============================================================*/
  public static native long jniBuildEHRegion(long body, long exit, long pragma, long suppInito);

  /***============================================================
   * Retreive Try's Comparator(Filter) Label_IDX
   *============================================================== */
  public static native long getTryComparator(long tryIdx);

  //============== Internal Use ==================
  /***
   * Saving this str to the temporary table
   * Usage: INITO to-use, other circumstances...
   * @param str
   * @return TEMP_ID of the generated temporary-string
   */
  public static native long saveAsTempString(String str);
  public static native long saveAsTempByteArray(byte[] val);
  public static native int getStringUTF8Length(String val);
}
