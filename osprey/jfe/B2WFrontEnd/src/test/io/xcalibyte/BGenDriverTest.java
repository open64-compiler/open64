/*
 * Copyright (C) 2021 Xcalibyte (Shenzhen) Limited.
 */

package io.xcalibyte;

import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;
import java.util.ArrayList;
import java.util.List;

import static io.xcalibyte.WhirlConstants.SClass.SCLASS_EXTERN;
import static io.xcalibyte.WhirlConstants.SClass.SCLASS_UGLOBAL;
import static io.xcalibyte.WhirlConstants.SExport.EXPORT_PREEMPTIBLE;
import static org.junit.Assert.*;

public class BGenDriverTest{

    private static final int DEFAULT_CLASS_SIZE = 1;
    private static final int DEFAULT_ALIGN = 8;
    private static final int DEFAULT_PU_ALIGN = 1;
    private static final long DEFAULT_OFFSET = 0;
    private static final int GLOBAL_SYMTAB = 1;
    private static final int DEFAULT_SCOPE_FUNCTION = 2;
    private static final long IDX_MAX = 100000; // only for this test

    @BeforeClass
    public static void setUp() {
        System.loadLibrary("macbcb");
        BGenDriver.bgenInit(true, 0xffffffff, 64);
        BGenDriver.bgenInitOpenIrFile("MyJavaTest.B");
        BGenDriver.bgenInitSetDST("MyJavaTest.class");
    }

    @AfterClass
    public static void tearDown() {
        BGenDriver.bgenFileClose();
        BGenDriver.bgenFileFinish();
    }

    @org.junit.Test
    public void getPrimitiveType() {
        for(WhirlConstants.Mtype m : WhirlConstants.Mtype.values()){
            assertTrue(BGenDriver.getPrimitiveType(m.toInt()) >= (1<<2));
        };
    }

    @org.junit.Test
    public void setTypeAttribute() {
        for(WhirlConstants.Mtype m : WhirlConstants.Mtype.values()){
            long type_idx_before =  BGenDriver.getPrimitiveType(m.toInt());
            for (WhirlConstants.TyAttr t : WhirlConstants.TyAttr.values()){
                assertTrue(BGenDriver.setTypeAttribute(type_idx_before, t.toInt()) != 0);
            }
        };
    }

    @org.junit.Test
    public void createFunctionType() {
        newFunc();
    }

    private long newFunc() {
        long int_type  = BGenDriver.getPrimitiveType(WhirlConstants.Mtype.MTYPE_I4.toInt());
        long long_type = BGenDriver.getPrimitiveType(WhirlConstants.Mtype.MTYPE_U8.toInt());
        long[] args = new long[]{long_type, int_type, long_type};

        // Generating the function symbol for level 1
        long funcType = BGenDriver.createFunctionType(int_type, args, BCRConfig.DEFAULT_PU_ALIGN);
        assertTrue(funcType != 0);

        return funcType;
    }

    @org.junit.Test
    public  void createClassType() {
        long       classType  = BGenDriver.createClassType("java.lang.Object", DEFAULT_CLASS_SIZE, DEFAULT_ALIGN);
        long       startField = BGenDriver.jniGetCurrentFieldIdx();
        assertTrue(classType > 0 && (classType >> 8) > 0);
    }

    @org.junit.Test
    public void createRefType() {
        long idx = BGenDriver.getPrimitiveType(WhirlConstants.Mtype.MTYPE_I4.toInt());
        long myRef = BGenDriver.createRefType("iReference", idx);
        assertTrue(isValidTypeIdx(idx));
        assertEquals(BGenDriver.getTySize(myRef), BCRConfig.ALIGN);
    }

    private boolean isValidTypeIdx(long idx) {
        return idx > 0 && (idx >> 8) > 0 && (idx >> 8) < IDX_MAX;
    }

    @org.junit.Test
    public void createArrayType() {
        long       classType  = BGenDriver.createClassType("java.lang.Object.2", DEFAULT_CLASS_SIZE, DEFAULT_ALIGN);
        long       startField = BGenDriver.jniGetCurrentFieldIdx();
        List<Long> flds       = new ArrayList<>();
        flds.add(BGenDriver.allocateSpotInClass(classType, ".vptr", DEFAULT_OFFSET));
        flds.add(BGenDriver.allocateSpotInClass(classType, "somewhatint", DEFAULT_OFFSET));
        flds.add(BGenDriver.allocateSpotInClass(classType, "int[]", DEFAULT_OFFSET));
        long       endField   = BGenDriver.jniGetCurrentFieldIdx();
        assertEquals(endField, startField + 3);
        long idx = BGenDriver.getPrimitiveType(WhirlConstants.Mtype.MTYPE_I4.toInt());
        long vptr = BGenDriver.jniMakePointerType(idx);
        long array_idx = BGenDriver.createArrayType(100, idx);
        BGenDriver.jniSetFldType(classType, flds.get(flds.size() - 3), vptr, DEFAULT_OFFSET);
        BGenDriver.jniSetFldType(classType, flds.get(flds.size() - 2), idx, DEFAULT_OFFSET);
        BGenDriver.jniSetFldType(classType, flds.get(flds.size() - 1), array_idx, DEFAULT_OFFSET);
        BGenDriver.finalizeFields(classType, startField, flds.get(flds.size() - 1));
    }

    @org.junit.Test
    public void allocateSpotInClass() {
         //ok
    }

    @org.junit.Test
    public void jniMakePointerType() {
        long int_type = BGenDriver.getPrimitiveType(WhirlConstants.Mtype.MTYPE_I4.toInt());
        long ptr = BGenDriver.jniMakePointerType(int_type);
        assertTrue(ptr != 0);
    }

    @org.junit.Test
    public void finalizeFields() {
        // ok
    }

    @org.junit.Test
    public void jniSetFldBaseClass() {
        long i2type = BGenDriver.getPrimitiveType(WhirlConstants.Mtype.MTYPE_I2.toInt());
        long first_field = BGenDriver.jniGetCurrentFieldIdx();
        long anotherClassType = BGenDriver.createClassWithFields("superclass", new String[]{"ok"}, new long[]{i2type});
        long classType = BGenDriver.createClassType("java.lang.Object.3", DEFAULT_CLASS_SIZE, DEFAULT_ALIGN);
        long field = BGenDriver.allocateSpotInClass(classType, "subclass", 0);
        long subclass_fld_idx = BGenDriver.jniSetFldType(classType, field, anotherClassType, 0);
        BGenDriver.jniSetFldBaseClass(subclass_fld_idx);
        BGenDriver.finalizeFields(classType, first_field, field);
    }

    @org.junit.Test
    public void jniCreateCommonArrayType() {
    }

    @org.junit.Test
    public void jniAddFieldToClass() {
    }

    @org.junit.Test
    public void jniSetFldType() {
    }

    @org.junit.Test
    public void jniCreatePrimArray() {
    }

    @org.junit.Test
    public void jniGetCurrentFieldIdx() {
    }

    @org.junit.Test
    public void jniSetFldAsPointerEx() {
    }

    @org.junit.Test
    public void createFunctionSymbol() {
        long funcType = newFunc();

        long pool = BGenDriver.jniPoolInitialize();
        assertTrue(pool != 0);

        BGenDriver.jniMapTabCreate(pool);
        long scope = BGenDriver.jniNewScope(pool);
        assertEquals(2, scope);

        long funcSt = BGenDriver.createFunctionSymbol("_ZN6myJavaTestC1Ev", funcType,
                SCLASS_EXTERN.toInt(), EXPORT_PREEMPTIBLE.toInt(), GLOBAL_SYMTAB);
        assertTrue(funcSt != 0);
        //BGenDriver.startFunction(funcSt, 0, DEFAULT_SCOPE_FUNCTION);
        //BGenDriver.finishFunction(0);
    }

    @org.junit.Test
    public void bindFunctionParams() {
    }

    @org.junit.Test
    public void createVarSymbol() {

    }

    @org.junit.Test
    public void createPregSymbol() {
    }

    @org.junit.Test
    public void createPreg() {
    }

    @org.junit.Test
    public void getStPtr() {
    }

    @org.junit.Test
    public void initializeStaticIntVar() {
    }

    @org.junit.Test
    public void initializeStaticRealVar() {
    }

    @org.junit.Test
    public void getReturnValPreg() {
    }

    @org.junit.Test
    public void jniCreateBlock() {
    }

    @org.junit.Test
    public void jniMapTabCreate() {
    }

    @org.junit.Test
    public void jniPoolInitialize() {
    }

    @org.junit.Test
    public void jniNewScope() {
    }

    @org.junit.Test
    public void jniSetLineNum() {
    }

    @org.junit.Test
    public void jniCreateIf() {
    }

    @org.junit.Test
    public void jniInsertBlockFirst() {
    }

    @org.junit.Test
    public void jniInsertBlockLast() {
    }

    @org.junit.Test
    public void jniInsertBlockBefore() {
    }

    @org.junit.Test
    public void jniInsertBlockAfter() {
    }

    @org.junit.Test
    public void jniLDA() {
    }

    @org.junit.Test
    public void jniLDID() {
    }

    @org.junit.Test
    public void jniLDIDField() {
    }

    @org.junit.Test
    public void jniSTID() {
    }

    @org.junit.Test
    public void jniSTIDField() {
    }

    @org.junit.Test
    public void getBodyFromEntry() {
    }

    @org.junit.Test
    public void getTyFromST() {
    }

    @org.junit.Test
    public void getTyFromWN() {
    }

    @org.junit.Test
    public void getRtypeFromWN() {
    }

    @org.junit.Test
    public void jniCreateBinary() {
    }

    @org.junit.Test
    public void jniCreateUnary() {
    }

    @org.junit.Test
    public void jniCreateTernary() {
    }

    @org.junit.Test
    public void jniIntConst() {
    }

    @org.junit.Test
    public void jniFloatConst() {
    }

    @org.junit.Test
    public void jniCreateReturn() {
    }

    @org.junit.Test
    public void jniCreateReturnVal() {
    }

    @org.junit.Test
    public void intTypeConversion() {
    }

    @org.junit.Test
    public void floatTypeConversion() {
    }

    @org.junit.Test
    public void jniNeg() {
    }

    @org.junit.Test
    public void jniAbs() {
    }

    @org.junit.Test
    public void jniSqrt() {
    }

    @org.junit.Test
    public void jniRsqrt() {
    }

    @org.junit.Test
    public void jniRecip() {
    }

    @org.junit.Test
    public void jniParen() {
    }

    @org.junit.Test
    public void jniRnd() {
    }

    @org.junit.Test
    public void jniTrunc() {
    }

    @org.junit.Test
    public void jniCeil() {
    }

    @org.junit.Test
    public void jniFloor() {
    }

    @org.junit.Test
    public void jniBnot() {
    }

    @org.junit.Test
    public void jniLnot() {
    }

    @org.junit.Test
    public void jniIlda() {
    }

    @org.junit.Test
    public void jniExtractBits() {
    }

    @org.junit.Test
    public void jniParm() {
    }

    @org.junit.Test
    public void jniAsmInput() {
    }

    @org.junit.Test
    public void jniAlloca() {
    }

    @org.junit.Test
    public void jniAdd() {
    }

    @org.junit.Test
    public void jniSub() {
    }

    @org.junit.Test
    public void jniMpy() {
    }

    @org.junit.Test
    public void jniHighmpy() {
    }

    @org.junit.Test
    public void jniDiv() {
    }

    @org.junit.Test
    public void jniMod() {
    }

    @org.junit.Test
    public void jniRem() {
    }

    @org.junit.Test
    public void jniMax() {
    }

    @org.junit.Test
    public void jniMin() {
    }

    @org.junit.Test
    public void jniEQ() {
    }

    @org.junit.Test
    public void jniNE() {
    }

    @org.junit.Test
    public void jniGE() {
    }

    @org.junit.Test
    public void jniGT() {
    }

    @org.junit.Test
    public void jniLE() {
    }

    @org.junit.Test
    public void jniLT() {
    }

    @org.junit.Test
    public void jniBand() {
    }

    @org.junit.Test
    public void jniBior() {
    }

    @org.junit.Test
    public void jniBnor() {
    }

    @org.junit.Test
    public void jniBxor() {
    }

    @org.junit.Test
    public void jniLand() {
    }

    @org.junit.Test
    public void jniLior() {
    }

    @org.junit.Test
    public void jniCand() {
    }

    @org.junit.Test
    public void jniCior() {
    }

    @org.junit.Test
    public void jniShl() {
    }

    @org.junit.Test
    public void jniAshr() {
    }

    @org.junit.Test
    public void jniLshr() {
    }

    @org.junit.Test
    public void jniComposeBits() {
    }

    @org.junit.Test
    public void jniRrotate() {
    }

    @org.junit.Test
    public void jniComma() {
    }

    @org.junit.Test
    public void jniRcomma() {
    }

    @org.junit.Test
    public void jniRelational() {
    }

    @org.junit.Test
    public void jniLabel() {
    }

    @org.junit.Test
    public void jniCreateGoto() {
    }

    @org.junit.Test
    public void getLabelNum() {
    }

    @org.junit.Test
    public void jniCreateSwitch() {
    }

    @org.junit.Test
    public void jniCreateCasegoto() {
    }

    @org.junit.Test
    public void jniCall() {
    }

    @org.junit.Test
    public void jniSetWNKid() {
    }

    @org.junit.Test
    public void mtypeToPreg() {
    }

    @org.junit.Test
    public void startFunction() {
    }

    @org.junit.Test
    public void finishFunction() {
    }

    @org.junit.Test
    public void traceLog() {
    }

    @org.junit.Test
    public void traceLog1() {
    }

    @org.junit.Test
    public void bgenInit() {
    }

    @org.junit.Test
    public void getIsAvailable() {
    }

    @org.junit.Test
    public void setIsAvailable() {
    }

    @org.junit.Test
    public void finishUp() {
    }
    @org.junit.Test
    public void addPrimitiveType() {
    }

    @org.junit.Test
    public void getFunctionType() {
    }

    @org.junit.Test
    public void getClassType() {
    }

    @org.junit.Test
    public void setFuncParm() {
    }

    @org.junit.Test
    public void main() {
        //BGenDriver.main(new String[1]);
    }

    @org.junit.Test
    public void toWHIRL() {
       // BGenDriver.toWHIRL();
    }

    @Test
    public void bgenFinish() {
    }

    @Test
    public void createCommonArrayType() {
    }

    @Test
    public void createExternFunctionSymbol() {
    }

    @Test
    public void jniILoad() {
    }

    @Test
    public void jniIStore() {
    }

    @Test
    public void getMTypeFromTypeIdx() {
    }

    @Test
    public void typeConversion() {
    }

    @Test
    public void jniSetSTIsVTable() {
    }

    @Test
    public void jniArray() {
    }

    @Test
    public void jniOpcodeMakeOp() {
    }

    @Test
    public void jniCreateIntrinsic() {
    }

    @Test
    public void createIntrinsic() {
    }

    @Test
    public void setTypePuAttribute() {
    }

    @Test
    public void printTypeInfo() {
    }

    @Test
    public void getTySize() {
    }

    @Test
    public void setPuLangCXX() {
    }

    @Test
    public void setPuLangC() {
    }

    @Test
    public void setPuLangJava() {
    }

    @Test
    public void setPuConstructor() {
    }

    @Test
    public void setPuDestructor() {
    }

    @Test
    public void printSymbolInfo() {
    }

    @Test
    public void initializeWithArray() {
      //  BGenDriver.initializeWithArray();
    }

    @Test
    public void initializeWithList() {
        long simpleIdx = createSimpleVar(); //u2,u4/u2/u4
        SymbolInitializer.InitValAppender list = new SymbolInitializer.InitValAppender(simpleIdx);
        list.add(WhirlConstants.Initial.MASK, WhirlConstants.Initial.START_BLOCK);
        list.add(WhirlConstants.Initial.U2_MASK, 10);
        list.add(WhirlConstants.Initial.U4_MASK, 10);
        list.add(WhirlConstants.Initial.U2_MASK, 10);
        list.add(WhirlConstants.Initial.U4_MASK, 10);
        list.add(WhirlConstants.Initial.MASK, WhirlConstants.Initial.END_BLOCK);
        list.finish();
        //BGenDriver.initializeWithList();
    }

    private long createSimpleVar() {
        long clsType = createNewSimlpleClassType();
        long clsSym = BGenDriver.createVarSymbol("", clsType, SCLASS_UGLOBAL, EXPORT_PREEMPTIBLE, WhirlConstants.DefaultValue.LEVEL);
        return clsSym;
    }

    @Test
    public void initializeStringVar() {
    }

    @Test
    public void jniCreateRegion() {
    }

    @Test
    public void jniGetParentBlock() {
    }

    @Test
    public void getPointeeTyFromTY() {
    }

    @Test
    public void jniLDAString() {
    }

    @Test
    public void funcICall() {
    }

    @Test
    public void info() {
        BGenDriver.info("Hello!");
    }

    long createNewSimlpleClassType(){
        long u2 = BGenDriver.getPrimitiveType(WhirlConstants.Mtype.MTYPE_U2.toInt());
        long u4 = BGenDriver.getPrimitiveType(WhirlConstants.Mtype.MTYPE_U4.toInt());
        long classType = BGenDriver.createClassWithFields("t_createClassWithFields_16", new String[]{"u2a", "u4a", "u2b", "u4b"}, new long[]{u2, u4, u2, u4});
        assertEquals(BGenDriver.getTySize(classType), 16);

        long cType8 = BGenDriver.createClassWithFields("t_createClassWithFields_8", new String[]{"u2a", "u2b", "u4b"}, new long[]{u2, u2, u4});
        assertEquals(BGenDriver.getTySize(cType8), 8);


        long cType24 = BGenDriver.createClassWithFields("t_createClassWithFields_24",
                new String[]{ "u4a", "u2b", "u4b", "u2b", "u4c", "u2c"},
                new long[]  {u4, u2, u4, u2, u4, u2});
        if(BGenDriver.getTySize(cType24) != 24){
            BGenDriver.printTypeInfo(cType24);
        }
        assertEquals(BGenDriver.getTySize(cType24), 24);
        return classType;
    }

    @Test
    public void createClassWithFields() {
        createNewSimlpleClassType();
    }

    @Test
    public void jniBuildEHBegin() {
    }

    @Test
    public void jniBuildTryCatch() {
    }

    @Test
    public void jniBuildEHEnd() {
    }

    @Test
    public void saveAsTempString() {
    }
}
