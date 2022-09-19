/*
 * Copyright (C) 2021 Xcalibyte (Shenzhen) Limited.
 */

package io.xcalibyte;

import soot.*;
import soot.jimple.IntConstant;
import soot.jimple.Jimple;
import soot.jimple.JimpleBody;

import java.util.ArrayList;
import java.util.List;

class NativeMethodCreator {

  private static Local[] createLocalParameters(JimpleBody body, SootMethod method) {
    List<Type> parameterTypes = method.getParameterTypes();
    PatchingChain<Unit> bodyUnits = body.getUnits();
    int localIdx = 0;
    int realParCnt = parameterTypes.size();
    Local thisVar = null;
    Local[] locals = new Local[realParCnt + 1];
    Type thisType = method.getDeclaringClass().getType();
    thisVar = Jimple.v().newLocal("this", thisType);
    locals[localIdx++] = thisVar;
    if(!method.isStatic()) {
      body.getLocals().add(thisVar);
      bodyUnits.add(Jimple.v().newIdentityStmt(thisVar, Jimple.v().newThisRef((RefType)thisType)));
    }
    for(int i = 0; i < parameterTypes.size(); i++) {
      Type t = parameterTypes.get(i);
      locals[localIdx] = Jimple.v().newLocal("para" + localIdx, t);
      body.getLocals().add(locals[localIdx]);
      bodyUnits.add(Jimple.v().newIdentityStmt(locals[localIdx], Jimple.v().newParameterRef(t, i)));
      localIdx++;
    }
    return locals;
  }

  public static void createCallToNative(SootMethod method) {
    JimpleBody body = Jimple.v().newBody(method);
    PatchingChain<Unit> bodyUnits = body.getUnits();
    Local[] locals = createLocalParameters(body, method);

    List<Value> parValues = new ArrayList<>();
    List<Value> auxValues = new ArrayList<>();

    // gen call to GetJNIEnvNewFrame
    Type envType = Scene.v().getObjectType(); // TODO: change to JNIEnv * type
    Local envValue = Jimple.v().newLocal("env", envType);
    body.getLocals().add(envValue);
    String getEnv = "_Jv_GetJNIEnvNewFrame";
    bodyUnits.add(Jimple.v().newAssignStmt(envValue,
      Jimple.v().newNativeExpr(getEnv, method, parValues, auxValues, envValue)));
    parValues.clear();
    auxValues.clear();

    // gen call to _Jv_LookupJNIMethod
    parValues.add(envValue);
    for(int i=0; i< locals.length; i++) {
      parValues.add(locals[i]);
    }

    Value methodRet = Jimple.v().newLocal("methodRetVal", method.getReturnType());
    Value methodNum = IntConstant.v(method.getNumber());
    auxValues.add(methodRet);
    auxValues.add(methodNum);
    Type lookupRType = Scene.v().getObjectType(); // TODO: change to void * type
    Local lookupRValue = Jimple.v().newLocal("funPtr", lookupRType);
    body.getLocals().add((Local)methodRet);
    body.getLocals().add(lookupRValue);
    String funName = "_Jv_LookupJNIMethod";
    if(method.getReturnType() instanceof VoidType) {
      bodyUnits.add(Jimple.v().newNativeStmt(Jimple.v().newNativeExpr(funName, method, parValues, auxValues, lookupRValue)));
    } else {
      bodyUnits.add(Jimple.v().newAssignStmt(methodRet, Jimple.v().newNativeExpr(funName, method, parValues, auxValues, lookupRValue)));
      bodyUnits.add(Jimple.v().newReturnStmt(methodRet));
    }


    method.setModifiers(method.getModifiers() & ~Modifier.NATIVE);
    method.setActiveBody(body);
    method.setModifiers(method.getModifiers() | Modifier.NATIVE);
  }

}