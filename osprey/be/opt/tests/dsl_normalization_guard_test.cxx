/* Copyright (C) 2026 Open64 Project */

/* LN2-d native exporter and mapped-preservation fixture, no runtime linkage.
 * Design: doc/AI-COMPILER-LAYERNORM-RUNTIME-GUARDS-v0.1.md. */
#define DSL_NORMALIZATION_PLAN_NO_MAIN
#include "dsl_normalization_plan_test.cxx"

/* Export every source alternative; keep native bytes and condensed trace
 * joined to original mapped source. Valid refusals never alter output. */
int main(int argc,char **argv)
{
  if (argc!=5) return 2;
  char *args[]={argv[0],argv[1],argv[4]};
  if (LN1b_Source_Producer(3,args)) return 1;
  WN *scope=PU_Info_tree_ptr(Current_PU_Info);
  OPT_Fast_Math=FALSE;Roundoff_Level=ROUNDOFF_NONE;IEEE_Arithmetic=IEEE_ACCURATE;
  FILE *trace=fopen(argv[2],"wx");if (!trace) return 1;
  unsigned index=0;
  for (WN *s=WN_first(WN_func_body(scope));s;s=WN_next(s)) {
    if (WN_operator(s)!=OPR_STID || WN_operator(WN_kid0(s))!=OPR_DSL) continue;
    ++index;
    for (UINT32 kind=1;kind<=3;++kind) {
      WOPT_DSL_NORMALIZATION_PLAN_REQUEST r;
      OPEN64_NORMALIZATION_GUARD g,quiet,saved;memset(&g,0x5a,sizeof(g));saved=g;
      if (!LN2_Request(scope,s,argv[3],kind,&r)) return 1;
      WOPT_DSL_NORMALIZATION_PLAN_BINDING b;
      if (!WOPT_DSL_bind_normalization_plan(scope,s,&r,&b,NULL,trace)) return 1;
      BOOL ok=WOPT_DSL_export_normalization_guard(scope,s,&r,&g,NULL,trace);
      if (!Check(ok==(b.status==0) && (ok || !memcmp(&g,&saved,sizeof(g))),"export_bound_only")) return 1;
      if (ok) {
        if (!WOPT_DSL_export_normalization_guard(scope,s,&r,&quiet,NULL,NULL) || memcmp(&g,&quiet,sizeof(g))) return 1;
        char path[4096];if (snprintf(path,sizeof(path),"%s.%u.%u.guard",argv[1],index,kind)>=(int)sizeof(path)) return 1;
        FILE *f=fopen(path,"wx");if (!f) return 1;
        if (fwrite(&g,sizeof(g),1,f)!=1 || fclose(f)) return 1;
      }
      long length=ftell(trace);quiet=saved;
      if (!Check(!WOPT_DSL_export_normalization_guard(WN_func_body(scope),s,&r,&quiet,NULL,trace) &&
          !WOPT_DSL_export_normalization_guard(scope,WN_COPY_Tree(s),&r,&quiet,NULL,trace) &&
          !WOPT_DSL_export_normalization_guard(scope,s,NULL,&quiet,NULL,trace) &&
          !WOPT_DSL_export_normalization_guard(scope,s,&r,NULL,NULL,trace) &&
          !memcmp(&quiet,&saved,sizeof(quiet)) && ftell(trace)==length,"invalid_export_no_publication")) return 1;
    }
  }
  if (fclose(trace)) return 1;
  char path[4096];if (snprintf(path,sizeof(path),"%s.preserved.B",argv[1])>=(int)sizeof(path)) return 1;
  DSL_BUILDER_MAPPED_IMAGE_REQUEST checkpoint={};checkpoint.path=path;
  return LN1c_Source_Checkpoint(&checkpoint) ? 0:1;
}
