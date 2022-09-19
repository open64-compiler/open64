#include "org_jetbrains_java_decompiler_modules_bgen_BGenDriver.h"
#include <stdio.h>

/*
 * Class:     org_jetbrains_java_decompiler_modules_bgen_BGenDriver
 * Method:    bgenInit
 * Signature: ()V
 */
void JNICALL Java_org_jetbrains_java_decompiler_modules_bgen_BGenDriver_bgenInit
  (JNIEnv *env, jclass clazz, jstring outputFilePath) {
    const char *p = env->GetStringUTFChars(outputFilePath, JNI_FALSE);
    printf("######## start init, output file path: %s.\n", p);
    env->ReleaseStringUTFChars(outputFilePath, p);
}

/*
 * Class:     org_jetbrains_java_decompiler_modules_bgen_BGenDriver
 * Method:    bgenFinish
 * Signature: ()V
 */
void JNICALL Java_org_jetbrains_java_decompiler_modules_bgen_BGenDriver_bgenFinish
  (JNIEnv *env, jclass clazz) {
  printf("######## finish all.\n");
}

