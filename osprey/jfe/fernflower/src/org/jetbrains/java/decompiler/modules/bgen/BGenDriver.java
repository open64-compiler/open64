/*
 * Copyright (C) 2021 Xcalibyte (Shenzhen) Limited.
 */

package org.jetbrains.java.decompiler.modules.bgen;

import   org.apache.logging.log4j.LogManager;
import   org.apache.logging.log4j.Logger;
import   org.jetbrains.java.decompiler.main.decompiler.ConsoleDecompiler;
import org.jetbrains.java.decompiler.modules.b2w.handler.WhirlConstants;

public class BGenDriver {

  private static final int LOG_LEVEL_ALL = 0;

  private static native void bgenInit(String outputFilePath, int logLevel);
  public  static native void bgenFinish();

  //============== Type ==================
  public  static native long getPrimitiveType(int typeKind, int typeAttributes);
  public  static native long createFunctionType(long retValIdx,long [] argTypes,int staticFlag);
  public  static native long createClassType(String name, int size, int alignWidth);
  public  static native long createArrayType(long size, long elm_ty, int alignWidth);
  public  static native long createCommonArrayType(long size, long elm_ty, int align,
                                                   long ubound, long block, long sizewn);

  //============== Symbol ================
  // *** Function Symbol S,E,L = 7,5,1
  public  static native long createFunctionSymbol(String name,long typeIdx,int sClass,int eClass,int level);

  // *** Var Symbol S,E,L = 1,0,2 or 2,0,2
  public  static native long createVarSymbol(String name,long typeIdx,int sClass,int eClass,int level);
  public  static native long getStPtr(long st_idx);

  //============== JNI(WN API) ===============

  public  static native long jniWnCreateBlock();
  public  static native long jniWnMapTabCreate(long poolPtr);
  public  static native long jniPoolInitialize();
  public  static native long jniNewScope(long poolPtr);
  public  static native void jniSetLineNum(long wn, long line_num);

  public static native long  jniCreateIf (long test, long then_block, long else_block) ;
  public static native void  jniInsertBlockFirst (long block, long stmt);
  public static native void  jniInsertBlockLast (long block, long stmt);
  public static native void  jniInsertBlockBefore (long block, long stmt, long pos);
  public static native void  jniInsertBlockAfter (long block, long stmt, long pos);
  public static native long  jniLDID (int ret_mtype, long offset, long ret_val_preg, long ret_ty_idx);
  public static native long  jniLDIDField (int ret_mtype, long offset, long ret_val_preg, long ret_ty_idx, int field_id);
  public static native long  jniSTID (int ret_mtype, long offset, long ret_val_preg, long ret_ty_idx, long val);
  public static native long  jniSTIDField (int ret_mtype, long offset, long ret_val_preg, long ret_ty_idx, long val, int field_id);
  public static native long  wnGetBodyFromEntry(long entry);
  public static native long  getTyFromST(long st_idx);
  public static native long  getTyFromWN(long wn_ptr);

  public static native long  jniWNCreateBinary(int opr, long ret_mtype, long lfs, long rhs);
  public static native long  jniWNCreateUnary(int opr, long ret_mtype, long lhs );
  public static native long  jniWNCreateTernary(int opr, long ret_mtype, long kid0, long kid1, long kid2);

  public static native long  jniWNIntConst(int mtype, long value);
  public static native long  jniWNFloatConst(int mtype, long value);

  public static native void  initializeStaticIntVar(long st, long vkind, long val);
  public static native void  initializeStaticRealVar(long st, long vkind, double initial_value);

  public static native long  funcICall(long rtype, long desc, int n, long ty); //n = kid_count
  public static native long  funcCall(long type, long desc, int n, long s);


  public static native int   getFirstFieldIdx ();
  public static native int   addFieldToClass  (long class_ty, String name,
                                              long typeidx, int offset);
  public static native void  finalizeFields   (long ty, int first_fld_idx);


  //-------------- Flow Control -------------------
  public static native long createWhileDo(long test, long body);
  public static native long createDoWhile(long test, long body);
  public static native long createDO(long index, long start, long end, long step,
                                     long body, long loop_info);



  public static native long  jniCreateReturn();
  public static native long  jniCreateGoto(int label_number);
  public static native long  jniCreateLabel(int label_number,
                                            long label_flag, long loop_info);

  //-------------- Conversion ---------------------
  public static native long intTypeConversion(long wn, byte/* TY_ID = UINT8*/ to_type);
  public static native long floatTypeConversion(long wn, byte/* TY_ID = UINT8*/ to_type);



  //-------------- Binary Operatotors -------------
  public static native long  jniNE  (int ret_mtype, long lhs, long rhs);
  public static native long  jniAdd (int ret_mtype, long lhs, long rhs);
   

  // *** Scope Must Be 1(Global)
  public  static native long startFunction(long sym_id, long ty_id, int scope);
  public  static native void finishFunction(long src_pos);

  //============== Tracing ======================
  private static native void outputTrace(String str);
  public static void traceLog(String str) {
    outputTrace(str);
  }

  public static void traceLog(boolean cond, String str) {
    if(cond) {
      outputTrace(str);
    }
  }

  public static void bgenInit(String outputFileName) {
    bgenInit(outputFileName, LOG_LEVEL_ALL);
  }

  public static Logger l = LogManager.getLogger(BGenDriver.class);

  private static Boolean isAvailable = true;

  public static Boolean getIsAvailable() {
    return isAvailable;
  }

  public static void setIsAvailable(Boolean isAvailable) {
    BGenDriver.isAvailable = isAvailable;
  }

  public static void finishUp() {
    if(getIsAvailable()) {
      l.info("Outputting to default .B path : out.B ");
      BGenDriver.bgenFinish();  // invoke the native method
      l.info("out.B generated");
    }else{
      throw new UnsatisfiedLinkError("Error : [main] Whirl API Not Loaded ! ");
    }
  }

  public static long addPrimitiveType(int typeKind, int attribute){
    if(getIsAvailable()){
      try{
        getPrimitiveType(typeKind, attribute);
      }
      catch (Exception e){
        l.info("Error : AddPrimitive Error"+e.getMessage());
        e.printStackTrace();
      }

    }
    else {
      throw new UnsatisfiedLinkError("Error : [main] Whirl API Not Loaded ! ");
    }

    return getPrimitiveType(typeKind, attribute);


  }

  public static long getFunctionType(long retValIdx,long [] argTypes,int staticFlag){
    if(getIsAvailable()){
      try{
        createFunctionType(retValIdx, argTypes, staticFlag);
      }
      catch (Exception e) {
        l.info("Error : createFunctionType Error" + e.getMessage());
        e.printStackTrace();
      }
    }
    else {
      throw new UnsatisfiedLinkError("Error : [main] Whirl API Not Loaded ! ");}
    return createFunctionType(retValIdx, argTypes, staticFlag);
  };


  public static long getClassType(String name,int size, int alignWidth){

    if(getIsAvailable()){
      try{
        createClassType(name, size, alignWidth);
      }
      catch (Exception e){
        l.info("Error : createClassType Error"+e.getMessage());
        e.printStackTrace();
      }
    }
    else {
      throw new UnsatisfiedLinkError("Error : [main] Whirl API Not Loaded ! ");
    }
    return createClassType(name, size, alignWidth);
  }


  public static void main(String[] args){
     System.loadLibrary("b2w");
     toWHIRL();
  }



  public static int toWHIRL() //throws UnsatisfiedLinkError
  {
    try {

      bgenInit("MyJavaOutput.B");
      outputTrace("SAMPLE_TRACE");

      long[] array = new long[]{};//{addPrimitiveType(4, 0), addPrimitiveType(5, 0), addPrimitiveType(6, 0)};
      long voidType = addPrimitiveType(10, 0);

      createClassType("Main", 10, 1);

     // long[] array1 = new long[]{addPrimitiveType(1, 0), addPrimitiveType(1, 0), addPrimitiveType(3, 0)};
     // getFunctionType(addPrimitiveType(4, 0), array1, 1);


      l.info("[toWHIRL] Creating a function");
      long pool  = jniPoolInitialize();
      l.info("[toWHIRL] Initialized POOL : " + pool);

      long out   = jniWnMapTabCreate(pool);
      long scope = jniNewScope(pool);
      long varSt = createVarSymbol("javar",
        getPrimitiveType(WhirlConstants.Mtype.MTYPE_I4.toInt(),0),
        WhirlConstants.SClass.SCLASS_UGLOBAL.toInt(),
        WhirlConstants.SExport.EXPORT_PREEMPTIBLE.toInt(),
        1);

      long baseType = getFunctionType(addPrimitiveType(1, 0), array, 4 /* align width */);
      long funcSt = createFunctionSymbol("void()", baseType, 7, 5, 1);

      long entry = startFunction(funcSt, array.length, 2 /* scope, must be 2 */);
      //1,0,2 == local Var ; 2,0,2 == parameter
      createVarSymbol("JavaLocalVar", addPrimitiveType(WhirlConstants.Mtype.MTYPE_I4.toInt(), 0), 1, 0, 2);
      long lhs = jniWNIntConst(WhirlConstants.Mtype.MTYPE_I4.toInt(), 10);
      long test = jniNE(WhirlConstants.Mtype.MTYPE_I4.toInt(),
        lhs,
        jniLDID(WhirlConstants.Mtype.MTYPE_I4.toInt(),0, varSt, getTyFromST(varSt)));
      long then_block = jniWnCreateBlock();
      {
        long then_block1 = jniWnCreateBlock();
        long else_block1 = jniWnCreateBlock();
        long if_1 = jniCreateIf (test, then_block1, else_block1);
        jniInsertBlockLast(then_block, if_1);
      }

      long else_block = jniWnCreateBlock();
      {

        long then_block2 = jniWnCreateBlock();
        long else_block2 = jniWnCreateBlock();
        long if_2 = jniCreateIf (test, then_block2, else_block2);
        jniInsertBlockLast(else_block, if_2);


      }

      long arrayTy = createArrayType(20, getPrimitiveType(WhirlConstants.Mtype.MTYPE_I4.toInt(),
                             0), 0 );
      createVarSymbol("myArray", arrayTy, WhirlConstants.SClass.SCLASS_UGLOBAL.toInt(),
                       WhirlConstants.SExport.EXPORT_PREEMPTIBLE.toInt(), 1);


      long if_ = jniCreateIf (test, then_block, else_block);
      jniInsertBlockLast(wnGetBodyFromEntry(entry), if_);



      finishFunction(5000 /** src position */);

      long func_call = funcCall(WhirlConstants.Mtype.MTYPE_I4.toInt(), WhirlConstants.Mtype.MTYPE_I4.toInt(), 0, funcSt);
      jniInsertBlockLast(wnGetBodyFromEntry(entry), func_call);


      long class_type = createClassType("simpleClass_50", 1, 4);
      //System.out.printf("%s%u\n", "createClassType -> ", class_type);

      long int_type = getPrimitiveType(WhirlConstants.Mtype.MTYPE_I4.toInt(), 0);
      int fld0 = addFieldToClass(class_type,  "myfirstField", int_type, 0);
      finalizeFields(class_type, fld0);

      long return_ = jniCreateReturn();
      jniInsertBlockLast(wnGetBodyFromEntry(entry), return_);



      bgenFinish();
    }catch (UnsatisfiedLinkError es){
      l.error("\n\n[ERROR] : Cannot load JNI B2w library in BGenDriver.java  ES:"+es.getMessage()+"\n\n");
    }catch (Exception e){
      l.error("\n\n[ERROR] : Cannot load JNI B2w library in BGenDriver.java "+e.getMessage()+"\n\n");
      //e.printStackTrace();
    }
    return 0;
  }
}
