/*
 * Copyright (C) 2021 Xcalibyte (Shenzhen) Limited.
 */

#ifndef isa_gen_def_INCLUDE
#define isa_gen_def_INCLUDE

#ifdef __cplusplus
extern "C" {
#endif

// ====================================================================
// The original open64 cg support 1 target, different target map to
// different compiled cg; for testing purpose, wasm taget will coexist
// on x86 target; and wasm target info will be packed into another
// archive, and then linked into cg.so, so all exported global symbols
// related with wasm target should be renamed.
//
// Then in cg module, diffenent symbol names can be used to distinguish
// between x86 and wasm.
//
// The following macro TI_SUFFIX represent target info suffix, will
// append a suffix into all exported global symbols.
// ====================================================================

#ifdef WASM
#define TI_SUFFIX "_wasm"
#else
#define TI_SUFFIX ""
#endif

#ifdef __cplusplus
}
#endif

#endif // isa_gen_def_INCLUDE