#ifndef isa_gen_targ_extra_INCLUDE
#define isa_gen_targ_extra_INCLUDE

#ifdef __cplusplus
extern "C" {
#endif

#ifdef WASM
// ====================================================================
// If build as wasm target info, the original method TOP_Name will be
// named TOP_Name_wasm, so define a method as an wrap if build as wasm
// target info, thus can prevent change every method call from TOP_Name
// to TOP_Name_wasm.
//
// But should pay more attention, each file will include current header
// file, must include topcode.h first. The enum TOP was defined in
// topcode.h
// ====================================================================
inline const char *TOP_Name(TOP topcode) {
  return TOP_Name_wasm(topcode);
}
#endif

#ifdef __cplusplus
}
#endif

#endif // isa_gen_targ_extra_INCLUDE