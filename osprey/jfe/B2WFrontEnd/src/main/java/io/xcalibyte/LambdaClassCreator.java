/*
 * Copyright (C) 2021 Xcalibyte (Shenzhen) Limited.
 */

package io.xcalibyte;

import soot.*;
import soot.asm.AsmUtil;
import soot.jimple.*;
import soot.jimple.internal.JDynamicInvokeExpr;

import java.util.*;

import static org.assertj.core.api.Assertions.assertThat;

class LambdaClassCreator {

  static final String getLambdaMethodName = "get$Lambda";

  private static final int REF_invokeVirtual = 5;
  private static final int REF_invokeStatic	= 6;
  private static final int REF_invokeSpecial = 7;
  private static final int REF_newInvokeSpecial = 8;
  private static final int REF_invokeInterface = 9;

  private static final int FLAG_MARKERS = 1 << 1;

  private static int lambdaClassIndex = 1;

  private static Local[] createLocalParameters(JimpleBody body, List<Type> parameterTypes) {
    Local[] parameters = new Local[parameterTypes.size()];
    PatchingChain<Unit> bodyUnits = body.getUnits();

    for(int i = 0; i < parameterTypes.size(); i++) {
      Type t = parameterTypes.get(i);
      parameters[i] = Jimple.v().newLocal("para" + i, t);
      body.getLocals().add(parameters[i]);
      bodyUnits.add(Jimple.v().newIdentityStmt(parameters[i], Jimple.v().newParameterRef(t, i)));
    }
    return parameters;
  }

  private static Local[] createLocalFields(JimpleBody body, SootClass lambdaClass) {
    Local[] fields = new Local[lambdaClass.getFields().size()];
    PatchingChain<Unit> bodyUnits = body.getUnits();

    int index = 0;
    for(SootField field: lambdaClass.getFields()) {
      Type t = field.getType();
      Local localField = Jimple.v().newLocal("field" + index, t);
      body.getLocals().add(localField);
      bodyUnits.add(
        Jimple.v().newAssignStmt(
          localField, Jimple.v().newInstanceFieldRef(body.getLocals().getFirst(), field.makeRef())
        )
      );
      fields[index++] = localField;
    }
    return fields;
  }

  private static Local convertType(JimpleBody body, Local l) {

    return l;
  }

  private static Local[] convertParameterTypes(JimpleBody body, Local[] parameterTypes) {
    Local[] convertedArgs = new Local[parameterTypes.length];

    for(int i = 0; i < parameterTypes.length; i++) {
      convertedArgs[i] = convertType(body, parameterTypes[i]);
    }
    return convertedArgs;
  }

  private static SootMethod createLambdaClassConstruct(SootClass lambdaClass, SootMethodRef invokeMethodRef) {
    SootMethod method = new SootMethod(
      SootMethod.constructorName, invokeMethodRef.parameterTypes(), VoidType.v(), Modifier.PRIVATE
    );
    JimpleBody body = Jimple.v().newBody(method);
    PatchingChain<Unit> bodyUnits = body.getUnits();
    Local l0 = Jimple.v().newLocal("l0", lambdaClass.getType());
    body.getLocals().add(l0);
    // l0 = this
    bodyUnits.add(Jimple.v().newIdentityStmt(l0, Jimple.v().newThisRef(lambdaClass.getType())));
    Local[] parameters = createLocalParameters(body, invokeMethodRef.parameterTypes());
    // l0.SpecialInvoke()
    bodyUnits.add(
      Jimple.v().newInvokeStmt(
        Jimple.v().newSpecialInvokeExpr(
          l0, Scene.v().makeMethodRef(lambdaClass.getSuperclass(),
            SootMethod.constructorName, Collections.emptyList(), VoidType.v(), false)
        )
      )
    );
    int fieldIndex = 0;
    for(SootField field: lambdaClass.getFields()) {
      bodyUnits.add(
        Jimple.v().newAssignStmt(Jimple.v().newInstanceFieldRef(l0, field.makeRef()), parameters[fieldIndex++])
      );
    }
    // return
    bodyUnits.add(Jimple.v().newReturnVoidStmt());
    method.setActiveBody(body);
    return method;
  }

  static private SootMethod createLambdaClassBridgeMethod(SootClass lambdaClass,
         SootMethodRef samMethodRef, String samMethodDesc, MethodHandle methodHandle, String instantiatedMethodDesc) {
    SootMethodRef handleMethodRef = methodHandle.getMethodRef();
    List<Type> samMethodTypes = AsmUtil.toJimpleDesc(samMethodDesc);
    List<Type> samMethodParameterTypes = samMethodTypes.subList(0, samMethodTypes.size() - 1);
    Type samMethodReturnType = samMethodTypes.get(samMethodTypes.size() - 1);
    List<Type> instiantedMethodType = AsmUtil.toJimpleDesc(instantiatedMethodDesc);
    List<Type> instiantedMethodParameterTypes = instiantedMethodType.subList(0, instiantedMethodType.size() - 1);
    Type instiantedMethodReturnType = instiantedMethodType.get(instiantedMethodType.size() - 1);
    SootMethod impMethod = handleMethodRef.declaringClass().getMethod(
      handleMethodRef.name(), handleMethodRef.parameterTypes(), handleMethodRef.returnType());
    SootMethodRef impMethodRef = impMethod.makeRef();
    SootMethod method = new SootMethod(
      samMethodRef.name(), samMethodParameterTypes, samMethodReturnType, Modifier.PUBLIC);
    JimpleBody body = Jimple.v().newBody(method);
    PatchingChain<Unit> bodyUnits = body.getUnits();
    Local l0 = Jimple.v().newLocal("l0", lambdaClass.getType());
    body.getLocals().add(l0);
    bodyUnits.add(Jimple.v().newIdentityStmt(l0, Jimple.v().newThisRef(lambdaClass.getType())));
    Local[] parameters = createLocalParameters(body, samMethodParameterTypes);
    Local[] fields = createLocalFields(body, lambdaClass);
    Local[] convertedParameters = convertParameterTypes(body, parameters);
    int tag = methodHandle.tag;
    boolean implIsInstanceMethod = (tag == REF_invokeVirtual || tag == REF_invokeSpecial || tag == REF_invokeInterface);
    int includesReceiver = 0;
    if(tag == REF_invokeVirtual || tag == REF_invokeSpecial || tag == REF_invokeInterface) {
      includesReceiver = 1;
    }
    List<Local> allArgs = new ArrayList<>();
    allArgs.addAll(Arrays.asList(fields));
    allArgs.addAll(Arrays.asList(convertedParameters));
    Local r = null;
    InvokeExpr invokeExpr = null;
    boolean isReturnVoid = impMethod.getReturnType() == VoidType.v();
    List<Local> args = allArgs.subList(includesReceiver, allArgs.size());
    if(!isReturnVoid) {
      r = Jimple.v().newLocal("r", impMethod.getReturnType());
      body.getLocals().add(r);
    } else if(tag == REF_newInvokeSpecial) {
      r = Jimple.v().newLocal("r", impMethod.getDeclaringClass().getType());
      body.getLocals().add(r);
    }
    switch(tag) {
      case REF_invokeVirtual:
        invokeExpr = Jimple.v().newVirtualInvokeExpr(allArgs.get(0), impMethodRef, args);
        break;
      case REF_invokeStatic:
        invokeExpr = Jimple.v().newStaticInvokeExpr(impMethodRef, args);
        break;
      case REF_invokeSpecial:
        invokeExpr = Jimple.v().newSpecialInvokeExpr(allArgs.get(0), impMethodRef, args);
        break;
      case REF_newInvokeSpecial:
        invokeExpr = Jimple.v().newSpecialInvokeExpr(r, impMethodRef, args);
        break;
      case REF_invokeInterface:
        invokeExpr = Jimple.v().newInterfaceInvokeExpr(allArgs.get(0), impMethodRef, args);
        break;
      default:
        assertThat(false).as("Not support method handle tag : " + tag).isTrue();
        break;
    }
    if(r == null) {
      assertThat(tag != REF_newInvokeSpecial).as(
        "REF_newInvokeSpecial, imp method return void, method : " + impMethodRef.toString()).isTrue();
      bodyUnits.add(Jimple.v().newInvokeStmt(invokeExpr));
      bodyUnits.add(Jimple.v().newReturnVoidStmt());
    } else {
      if(tag == REF_newInvokeSpecial) {
        bodyUnits.add(Jimple.v().newAssignStmt(r, Jimple.v().newNewExpr(impMethod.getDeclaringClass().getType())));
        bodyUnits.add(Jimple.v().newInvokeStmt(invokeExpr));
      } else {
        bodyUnits.add(Jimple.v().newAssignStmt(r, invokeExpr));
      }
      Local rc = convertType(body, r);
      bodyUnits.add(Jimple.v().newReturnStmt(rc));
    }
    method.setActiveBody(body);
    return method;
  }

  private static SootMethod createLambdaClassGetLambdaMethod(SootClass lambdaClass, SootMethodRef invokeMethodRef) {
    SootClass lambdaInterface = lambdaClass.getInterfaces().getFirst();
    SootMethod method = new SootMethod(
      getLambdaMethodName, invokeMethodRef.parameterTypes(),
      lambdaInterface.getType(), Modifier.PRIVATE | Modifier.STATIC
    );
    JimpleBody body = Jimple.v().newBody(method);
    PatchingChain<Unit> bodyUnits = body.getUnits();
    Local[] parameters = createLocalParameters(body, invokeMethodRef.parameterTypes());
    Local r = Jimple.v().newLocal("r", lambdaInterface.getType());
    body.getLocals().add(r);
    // LambdaInterface r = new LambdaClass
    bodyUnits.add(Jimple.v().newAssignStmt(r, Jimple.v().newNewExpr(lambdaClass.getType())));
    // r = LambdaClass.init
    bodyUnits.add(
      Jimple.v().newInvokeStmt(
        Jimple.v().newSpecialInvokeExpr(r,
          lambdaClass.getMethod(SootMethod.constructorName, invokeMethodRef.parameterTypes()).makeRef(), parameters)
      )
    );
    // return r
    bodyUnits.add(Jimple.v().newReturnStmt(r));
    method.setActiveBody(body);
    return method;
  }

  private static void addInterfaces(SootClass lambdaClass, SootClass samBase, JDynamicInvokeExpr expr) {
    boolean isAltBootstrapMethod = expr.getBootstrapMethod().getName().equals("altMetafactory");
    Set<SootClass> interfaces = new HashSet<>();
    interfaces.add(samBase);
    if(isAltBootstrapMethod) {
      List<Value> args = expr.getBootstrapArgs();
      assertThat(args.size() >= 4).as(
        "altMetafactory args less than 4, args size : " + args.size()).isTrue();
      assertThat(args.get(3) instanceof IntConstant).as(
        "altMetafctory arg 3 is not integer, arg 3 : " + args.get(3)).isTrue();
      int flags = ((IntConstant) args.get(3)).value;
      if((flags & FLAG_MARKERS) != 0) {
        int markerCount = ((IntConstant) args.get(4)).value;
        for(int i = 4; i < (4 + markerCount); i++) {
          ClassConstant marker = ((ClassConstant) args.get(i));
          SootClass markerType = ((RefType) AsmUtil.toJimpleType(marker.value)).getSootClass();
          interfaces.add(markerType);
        }
      }
    }
    for(SootClass i : interfaces) {
      lambdaClass.addInterface(i);
    }
  }

  static SootClass createLambdaClass(JDynamicInvokeExpr expr, SootClass callSiteClass) {
    // now we just handle java lambda bootstrap method
    SootMethod bootstrapMethod = expr.getBootstrapMethod();
    assertThat(bootstrapMethod.getDeclaringClass().getName().equals("java.lang.invoke.LambdaMetafactory")).as(
      "bootstrap is not belong LambdaMetafactory, bootstrap method : " + bootstrapMethod).isTrue();
    assertThat(bootstrapMethod.getName().equals("metafactory") ||
      bootstrapMethod.getName().equals("altMetafactory")).as(
        "Don't support this bootstrap method, method: " + bootstrapMethod).isTrue();
    SootMethodRef methodRef = expr.getMethodRef();
    List<Value> args = expr.getBootstrapArgs();
    assertThat(methodRef.returnType() instanceof RefType).as(
      "Dynamic invoke method handler don't return class type, type : " + methodRef.returnType()).isTrue();
    SootClass lambdaInterface = ((RefType) methodRef.returnType()).getSootClass();
    assertThat(lambdaInterface.isInterface()).as(
      "Lambda implement class is not interface, class : " + lambdaInterface).isTrue();
    assertThat(args.get(0) instanceof ClassConstant).as(
      "Bootstrap arg 0 is not ClassConstant, args 0 : " + args.get(0)).isTrue();
    assertThat(args.get(1) instanceof MethodHandle).as(
      "Bootstrap Arg 1 is not MethodHandle, arg 2 : " + args.get(1)).isTrue();
    assertThat(args.get(2) instanceof ClassConstant).as(
      "Bootstrap arg 2 is not ClassConstant, args 2 : " + args.get(2)).isTrue();
    String samMethodDesc = ((ClassConstant) args.get(0)).value;
    MethodHandle methodHandle = (MethodHandle) args.get(1);
    String instantiatedMethodDesc = ((ClassConstant) args.get(2)).value;
    String lambdaClassName = callSiteClass.getName() + "$$Lambda$" + lambdaClassIndex++;
    SootClass lambdaClass = new SootClass(lambdaClassName, Modifier.FINAL | Modifier.SYNTHETIC);
    lambdaClass.setSuperclass(Scene.v().getSootClass(SootConstants.CLASS_OBJECT));
    lambdaClass.setOuterClass(callSiteClass);
    addInterfaces(lambdaClass, lambdaInterface, expr);
    SootMethodRef invokeMethodRef = expr.getMethodRef();
    for(int i = 0; i < invokeMethodRef.parameterTypes().size(); i++) {
      Type t = invokeMethodRef.parameterType(i);
      lambdaClass.addField(Scene.v().makeSootField("field" + i, t, Modifier.PRIVATE | Modifier.FINAL));
    }
    lambdaClass.addMethod(createLambdaClassConstruct(lambdaClass, expr.getMethodRef()));
    lambdaClass.addMethod(createLambdaClassBridgeMethod(lambdaClass, methodRef, samMethodDesc,
      methodHandle, instantiatedMethodDesc));
    lambdaClass.addMethod(createLambdaClassGetLambdaMethod(lambdaClass, expr.getMethodRef()));
    return lambdaClass;
  }
}
