/* Copyright (C) 2026 Open64 Project */

/* LN4 selected C0/C1/C2 executable WHIRL, independent of reduction coding.
 * Design: doc/AI-COMPILER-LAYERNORM-EXECUTABLE-LOWERING-v0.1.md.
 * Analysis stays source-PU-local; materialization uses a driver-selected empty
 * device PU. CUDA emission consumes the resulting ordinary WHIRL only. */
#include <limits.h>
#include <fenv.h>
#if defined(__SSE__)
#include <xmmintrin.h>
#endif
#include <stddef.h>
#include <set>
#include <string.h>
#include "opt_dsl_normalization_lower.h"
#include "wn_util.h"
#include "targ_const.h"
#include "config.h"
#include "config_opt.h"
#include "config_dsl.h"
#include "pu_info.h"
#include "stab.h"

namespace {
/* FNV identity is a corruption detector, not an authorization mechanism. */
static UINT64 LN4_Hash(UINT64 hash,const void *data,size_t size)
{
  const unsigned char *p=(const unsigned char *)data;
  for (size_t i=0;i<size;++i) hash=(hash^p[i])*UINT64(1099511628211ULL);
  return hash;
}

/* Include a fixed-width integer independently of host pointer addresses. */
static void LN4_Word(UINT64 &hash,UINT64 value)
{
  hash=LN4_Hash(hash,&value,sizeof(value));
}

/* Include text bytes and terminator so concatenated names stay unambiguous. */
static void LN4_Text(UINT64 &hash,const char *text)
{
  hash=LN4_Hash(hash,text,strlen(text)+1);
}

/* Hash the unchanged legacy numeric prefix. Imported compiler-only evidence
 * is appended explicitly, including every section key and live footprint;
 * zero/unbound handoffs therefore retain their exact historical seal. */
static UINT64 LN4_Plan_Seal(const WOPT_DSL_NORMALIZATION_LOWERING &p)
{
  UINT64 hash=LN4_Hash(1469598103934665603ULL,&p,offsetof(WOPT_DSL_NORMALIZATION_LOWERING,seal));
  if (p.gpu_stamp.bound) {
    hash=LN4_Hash(hash,&p.gpu_stamp.bound,sizeof(p.gpu_stamp.bound));
    hash=LN4_Hash(hash,p.gpu_stamp.resource,sizeof(p.gpu_stamp.resource));
    hash=LN4_Hash(hash,p.gpu_stamp.execution,sizeof(p.gpu_stamp.execution));
    hash=LN4_Hash(hash,p.gpu_stamp.budget,sizeof(p.gpu_stamp.budget));
    hash=LN4_Hash(hash,p.gpu_stamp.context,sizeof(p.gpu_stamp.context));
    LN4_Word(hash,p.gpu_live_bytes);
  }
  return hash;
}

/* Reject stale imported evidence before allocation or verification. Native
 * preparation already bound ranges/events/numerical meaning; current imported
 * limits additionally admit declared CTA/RF/shared and backing reservations.
 * No probe, re-ranking or runtime resource discharge occurs here. */
static BOOL LN4_Plan_Valid(const WOPT_DSL_NORMALIZATION_LOWERING &p)
{
  if (!DSL_gpu_plan_stamp_current(p.gpu_stamp,VHO_DSL_GPU_Context)) return FALSE;
  const DSL_GPU_PLAN_SNAPSHOT gpu=DSL_GPU_PLAN_SNAPSHOT::Capture(VHO_DSL_GPU_Context);
  DSL_HARDWARE_DESCRIPTION_RECORD hw;
  const OPEN64_NORMALIZATION_GUARD &g=p.guard;
  return p.version==1 && p.seal==LN4_Plan_Seal(p) &&
      DSL_hardware_description_get_arch((UINT32)g.architecture,&hw) && p.revision==hw.revision &&
      (!gpu.Bound() || (gpu.Architecture()==hw.architecture &&
       gpu.Fits_storage(p.gpu_live_bytes) &&
       gpu.Fits_CTA(g.threads,p.registers_per_thread,g.reserved_shared) && g.reserved_shared<=49152)) &&
      g.abi_version==1 && g.size==sizeof(g) && g.source_version==2 &&
      g.kind>=1 && g.kind<=3 && g.rows && g.rows<=INT_MAX && g.width && g.width<=16777216 &&
      g.ctas==g.rows && g.threads && (gpu.Bound() ? g.threads<=1024 : g.threads<=hw.resources.max_threads_per_cta) &&
      p.registers_per_thread && p.registers_per_thread<=hw.resources.max_registers_per_thread &&
      p.cost.status==WOPT_DSL_NORM_COST_COMPLETE && p.cost.resident_ctas &&
      g.fp_options==0 && g.route==WOPT_DSL_NORM_ROUTE_PTX_POINTER_SCALAR_V1 &&
      g.environment==WOPT_DSL_NORM_ENV_RNE_GRADUAL_NO_TRAPS_CLASS &&
      (g.kind==1 ? !g.shared && !g.reserved_shared :
       g.stride>=4*g.width && !(g.stride%4) && g.shared==g.stride+8 &&
       g.reserved_shared>=g.shared && !(g.reserved_shared%4) && g.reserved_shared<=g.shared_limit);
}

/* Scope identity comes from the driver, never a self-selected PU traversal. */
static BOOL LN4_Active()
{
  return Current_PU_Info && CURRENT_SYMTAB>GLOBAL_SYMTAB && Scope_tab[CURRENT_SYMTAB].st &&
      ST_st_idx(Scope_tab[CURRENT_SYMTAB].st)==PU_Info_proc_sym(Current_PU_Info) &&
      PU_Info_tree_ptr(Current_PU_Info) && WN_operator(PU_Info_tree_ptr(Current_PU_Info))==OPR_FUNC_ENTRY;
}

/* Scalar primitive preflight uses precisely these established FP controls. */
static BOOL LN4_Strict()
{
  return WOPT_DSL_normalization_compiler_environment_ready();
}

/* Existing declarations with builtin names must carry the canonical ABI;
 * refuse collisions before constructing any symbols or executable nodes. */
static BOOL LN4_Builtins_Valid()
{
  const ST *s;ST_IDX idx;
  FOREACH_SYMBOL(GLOBAL_SYMTAB,s,idx) {
    if (!strcmp(ST_name(s),"threadIdx") || !strcmp(ST_name(s),"blockIdx")) {
      if (ST_class(s)!=CLASS_VAR || ST_sclass(s)!=SCLASS_EXTERN || TY_kind(ST_type(s))!=KIND_STRUCT || TY_size(ST_type(s))!=12) return FALSE;
      FLD_HANDLE f=TY_fld(ST_type(s));
      const char *names[]={"x","y","z"};
      for (UINT32 i=0;i<3;++i) {
        if (f.Is_Null() || strcmp(FLD_name(f),names[i]) || FLD_ofst(f)!=i*4 || FLD_type(f)!=MTYPE_To_TY(MTYPE_U4) ||
            (i==2)!=!!FLD_last_field(f)) return FALSE;
        if (i!=2) f=FLD_next(f);
      }
    }
    if (!strcmp(ST_name(s),"__syncthreads")) {
      if (ST_class(s)!=CLASS_FUNC || TY_kind(ST_pu_type(s))!=KIND_FUNCTION) return FALSE;
      TYLIST_IDX types=TY_tylist(ST_pu_type(s));
      if (!types || TYLIST_type(Tylist_Table[types])!=MTYPE_To_TY(MTYPE_V) || TYLIST_type(Tylist_Table[types+1])) return FALSE;
    }
  }
  return TRUE;
}

struct LN4_CONTEXT {
  const WOPT_DSL_NORMALIZATION_LOWERING *plan;
  SRCPOS source;
  TY_IDX pointer;
  ST *x,*gamma,*beta,*y,*arena,*scratch,*addresses[3];
  ST *tid,*row,*k,*sum,*mu,*variance,*q,*d,*u,*z;
  ST *thread,*block,*barrier;
};

/* Declare an active-device local at the caller's complete source coordinate;
 * placement attributes remain explicit caller obligations. */
static ST *LN4_Local(const char *name,TY_IDX type,SRCPOS source)
{
  ST *s=New_ST(CURRENT_SYMTAB);ST_Init(s,Save_Str(name),CLASS_VAR,SCLASS_AUTO,EXPORT_LOCAL,type);Set_ST_Srcpos(*s,source);return s;
}

/* Every scalar read gets a fresh tree, preserving WHIRL's single-parent rule. */
static WN *LN4_Load(ST *s)
{
  return WN_Ldid(TY_mtype(ST_type(s)),0,s,ST_type(s));
}

/* Consume one owned expression in a typed active-local assignment. */
static WN *LN4_Store(ST *s,WN *value)
{
  return WN_Stid(TY_mtype(ST_type(s)),0,s,ST_type(s),value);
}

/* Index and comparison constants are range-checked signed I4 coordinates. */
static WN *LN4_I4(INT32 value)
{
  return WN_Intconst(MTYPE_I4,value);
}

/* Preserve positive finite epsilon and exact count/identity binary32 words. */
static WN *LN4_F4(UINT32 bits)
{
  float value;memcpy(&value,&bits,4);return WN_Floatconst(MTYPE_F4,value);
}

/* Address arithmetic is 64-bit even when row/feature coordinates fit I4. */
static WN *LN4_Address(ST *base,WN *index)
{
  WN *address=ST_sclass(base)==SCLASS_FORMAL?LN4_Load(base):WN_Lda(Pointer_Mtype,0,base);
  return WN_Add(Pointer_Mtype,address,WN_Mpy(Pointer_Mtype,
      WN_Cvt(MTYPE_I4,Pointer_Mtype,index),WN_Intconst(Pointer_Mtype,4)));
}

/* Global matrix address uses widened row*N+k; affine vectors use k alone. */
static WN *LN4_Matrix_Address(const LN4_CONTEXT &c,ST *base)
{
  WN *index=WN_Add(Pointer_Mtype,WN_Mpy(Pointer_Mtype,
      WN_Cvt(MTYPE_I4,Pointer_Mtype,LN4_Load(c.row)),WN_Intconst(Pointer_Mtype,c.plan->guard.width)),
      WN_Cvt(MTYPE_I4,Pointer_Mtype,LN4_Load(c.k)));
  return WN_Add(Pointer_Mtype,LN4_Load(base),WN_Mpy(Pointer_Mtype,index,WN_Intconst(Pointer_Mtype,4)));
}

/* Ordinary memory reads retain physical symbol and explicit offset identity. */
static WN *LN4_Read(WN *address)
{
  return WN_Iload(MTYPE_F4,0,MTYPE_To_TY(MTYPE_F4),address);
}

/* Ordinary memory stores are explicit executable effects, not emitter hints. */
static WN *LN4_Write(const LN4_CONTEXT &c,WN *address,WN *value)
{
  return WN_Istore(MTYPE_F4,0,c.pointer,address,value);
}

/* Package exact scalar operands in three private F32 carriers. ASM loads both
 * operands before writing result; unpack is the final explicit load. */
static void LN4_Scalar(const LN4_CONTEXT &c,WN *body,ST *result,UINT32 op,WN *left,WN *right=NULL)
{
  WN_INSERT_BlockLast(body,LN4_Write(c,LN4_Address(c.scratch,LN4_I4(1)),left));
  if (right) WN_INSERT_BlockLast(body,LN4_Write(c,LN4_Address(c.scratch,LN4_I4(2)),right));
  WN *args[3]={LN4_Load(c.addresses[0]),LN4_Load(c.addresses[1]),LN4_Load(c.addresses[2])};
  WN *operation=WOPT_DSL_create_normalization_scalar((UINT32)c.plan->guard.architecture,op,args,right?3:2,c.source);
  FmtAssert(operation,("LN4 scalar construction failed after preflight"));
  WN_INSERT_BlockLast(body,operation);
  WN_INSERT_BlockLast(body,LN4_Store(result,LN4_Read(LN4_Address(c.scratch,LN4_I4(0)))));
}

/* Canonical high-level counted loops preserve ordered statistics and tails. */
static WN *LN4_Loop(const LN4_CONTEXT &c,WN *body,BOOL cooperative)
{
  return WN_CreateDO(WN_CreateIdname(0,c.k),LN4_Store(c.k,cooperative?LN4_Load(c.tid):LN4_I4(0)),
      WN_LT(MTYPE_I4,LN4_Load(c.k),LN4_I4(c.plan->guard.width)),
      LN4_Store(c.k,WN_Add(MTYPE_I4,LN4_Load(c.k),LN4_I4(cooperative?c.plan->guard.threads:1))),body,NULL);
}

/* Lane-zero branches contain no uniform barrier, including short rows. */
static WN *LN4_Owner(const LN4_CONTEXT &c,WN *body)
{
  return WN_CreateIf(WN_EQ(MTYPE_I4,LN4_Load(c.tid),LN4_I4(0)),body,WN_CreateBlock());
}

/* Required producer/consumer boundaries are effectful uniform CTA calls. */
static void LN4_Barrier(const LN4_CONTEXT &c,WN *body)
{
  WN_INSERT_BlockLast(body,WN_Call(MTYPE_V,MTYPE_V,0,ST_st_idx(c.barrier)));
}

/* One reusable global coordinate declaration, never a cross-PU analysis. */
static ST *LN4_Coordinate(const char *name,SRCPOS source)
{
  const ST *old;ST_IDX idx;
  FOREACH_SYMBOL(GLOBAL_SYMTAB,old,idx) if (ST_class(old)==CLASS_VAR && !strcmp(ST_name(old),name)) return &St_Table[idx];
  TY_IDX type;TY &t=New_TY(type);TY_Init(t,12,KIND_STRUCT,MTYPE_M,Save_Str("open64_ln4_dim3"));Set_TY_align(type,4);
  const char *fields[]={"x","y","z"};
  for (UINT32 i=0;i<3;++i) {FLD_HANDLE f=New_FLD();if (!i) Set_TY_fld(t,f);FLD_Init(f,Save_Str(fields[i]),MTYPE_To_TY(MTYPE_U4),i*4);if (i==2) Set_FLD_last_field(f);}
  ST *s=New_ST(GLOBAL_SYMTAB);ST_Init(s,Save_Str(name),CLASS_VAR,SCLASS_EXTERN,EXPORT_PREEMPTIBLE,type);Set_ST_Srcpos(*s,source);return s;
}

/* Read the recorded x-coordinate field; no name-to-coordinate emitter guess. */
static WN *LN4_Coord(ST *symbol)
{
  return WN_Cvt(MTYPE_U4,MTYPE_I4,WN_Ldid(MTYPE_U4,0,ST_st_idx(symbol),ST_type(symbol),1));
}

/* Create a standard effectful barrier declaration with explicit void ABI. */
static ST *LN4_Barrier_Symbol(SRCPOS source)
{
  const ST *old;ST_IDX idx;
  FOREACH_SYMBOL(GLOBAL_SYMTAB,old,idx) if (ST_class(old)==CLASS_FUNC && !strcmp(ST_name(old),"__syncthreads")) return &St_Table[idx];
  TY_IDX type=Make_Function_Type(MTYPE_To_TY(MTYPE_V));PU_IDX pi;PU &p=New_PU(pi);PU_Init(p,type,GLOBAL_SYMTAB+1);Set_PU_c_lang(p);
  ST *s=New_ST(GLOBAL_SYMTAB);ST_Init(s,Save_Str("__syncthreads"),CLASS_FUNC,SCLASS_EXTERN,EXPORT_PREEMPTIBLE,type);Set_ST_pu(s,pi);Set_ST_Srcpos(*s,source);return s;
}

/* Ordered mean reads either global X or C2's staged generation zero. */
static WN *LN4_Mean(const LN4_CONTEXT &c)
{
  WN *body=WN_CreateBlock(),*loop=WN_CreateBlock();WN_INSERT_BlockLast(body,LN4_Store(c.sum,LN4_F4(0)));
  LN4_Scalar(c,loop,c.sum,1,LN4_Load(c.sum),LN4_Read(c.plan->guard.kind==3?
      LN4_Address(c.arena,LN4_Load(c.k)):LN4_Matrix_Address(c,c.x)));
  WN_INSERT_BlockLast(body,LN4_Loop(c,loop,FALSE));
  float count=(float)c.plan->guard.width;UINT32 bits;memcpy(&bits,&count,4);
  LN4_Scalar(c,body,c.mu,4,LN4_Load(c.sum),LN4_F4(bits));
  if (c.arena) WN_INSERT_BlockLast(body,LN4_Write(c,LN4_Address(c.arena,LN4_I4(c.plan->guard.stride/4)),LN4_Load(c.mu)));
  return LN4_Owner(c,body);
}

/* Compute centered values once for retained paths after mean publication. */
static WN *LN4_Center(const LN4_CONTEXT &c)
{
  WN *active=WN_CreateBlock(),*loop=WN_CreateBlock();
  WN_INSERT_BlockLast(active,LN4_Store(c.mu,LN4_Read(LN4_Address(c.arena,LN4_I4(c.plan->guard.stride/4)))));
  LN4_Scalar(c,loop,c.d,2,LN4_Read(c.plan->guard.kind==3?LN4_Address(c.arena,LN4_Load(c.k)):LN4_Matrix_Address(c,c.x)),LN4_Load(c.mu));
  WN_INSERT_BlockLast(loop,LN4_Write(c,LN4_Address(c.arena,LN4_Load(c.k)),LN4_Load(c.d)));
  WN_INSERT_BlockLast(active,LN4_Loop(c,loop,TRUE));
  return WN_CreateIf(WN_LT(MTYPE_I4,LN4_Load(c.tid),LN4_I4(c.plan->guard.width)),active,WN_CreateBlock());
}

/* Lane zero retains ascending centered-square accumulation and all four
 * explicit finalize operations: divide, epsilon add, sqrt, reciprocal. */
static WN *LN4_Variance(const LN4_CONTEXT &c)
{
  WN *body=WN_CreateBlock(),*loop=WN_CreateBlock();WN_INSERT_BlockLast(body,LN4_Store(c.sum,LN4_F4(0)));
  if (c.arena) WN_INSERT_BlockLast(loop,LN4_Store(c.d,LN4_Read(LN4_Address(c.arena,LN4_Load(c.k)))));
  else LN4_Scalar(c,loop,c.d,2,LN4_Read(LN4_Matrix_Address(c,c.x)),LN4_Load(c.mu));
  LN4_Scalar(c,loop,c.u,3,LN4_Load(c.d),LN4_Load(c.d));LN4_Scalar(c,loop,c.sum,1,LN4_Load(c.sum),LN4_Load(c.u));
  WN_INSERT_BlockLast(body,LN4_Loop(c,loop,FALSE));
  float count=(float)c.plan->guard.width;UINT32 bits;memcpy(&bits,&count,4);
  LN4_Scalar(c,body,c.variance,4,LN4_Load(c.sum),LN4_F4(bits));
  LN4_Scalar(c,body,c.u,1,LN4_Load(c.variance),LN4_F4(c.plan->guard.epsilon_bits));
  LN4_Scalar(c,body,c.z,5,LN4_Load(c.u));LN4_Scalar(c,body,c.q,4,LN4_F4(0x3f800000),LN4_Load(c.z));
  if (c.arena) WN_INSERT_BlockLast(body,LN4_Write(c,LN4_Address(c.arena,LN4_I4(c.plan->guard.stride/4+1)),LN4_Load(c.q)));
  return LN4_Owner(c,body);
}

/* Pointwise affine epilogue consumes only valid elements, with q hoisted to
 * one shared broadcast per active warp; no implicit FMA/reassociation. */
static WN *LN4_Epilogue(const LN4_CONTEXT &c)
{
  WN *body=WN_CreateBlock(),*loop=WN_CreateBlock();
  if (c.arena) WN_INSERT_BlockLast(body,LN4_Store(c.q,LN4_Read(LN4_Address(c.arena,LN4_I4(c.plan->guard.stride/4+1)))));
  if (c.arena) WN_INSERT_BlockLast(loop,LN4_Store(c.d,LN4_Read(LN4_Address(c.arena,LN4_Load(c.k)))));
  else LN4_Scalar(c,loop,c.d,2,LN4_Read(LN4_Matrix_Address(c,c.x)),LN4_Load(c.mu));
  LN4_Scalar(c,loop,c.u,3,LN4_Load(c.d),LN4_Load(c.q));
  LN4_Scalar(c,loop,c.z,3,LN4_Load(c.u),LN4_Read(LN4_Address(c.gamma,LN4_Load(c.k))));
  LN4_Scalar(c,loop,c.d,1,LN4_Load(c.z),LN4_Read(LN4_Address(c.beta,LN4_Load(c.k))));
  WN_INSERT_BlockLast(loop,LN4_Write(c,LN4_Matrix_Address(c,c.y),LN4_Load(c.d)));
  WN_INSERT_BlockLast(body,LN4_Loop(c,loop,c.arena!=NULL));
  return c.arena?WN_CreateIf(WN_LT(MTYPE_I4,LN4_Load(c.tid),LN4_I4(c.plan->guard.width)),body,WN_CreateBlock()):LN4_Owner(c,body);
}

/* Construct all ordered executable phases, leaving uniform barriers outside
 * owner/valid-element branches. No semantic DSL operation survives here. */
static WN *LN4_Body(const LN4_CONTEXT &c)
{
  WN *body=WN_CreateBlock();
  WN_INSERT_BlockLast(body,LN4_Store(c.tid,LN4_Coord(c.thread)));WN_INSERT_BlockLast(body,LN4_Store(c.row,LN4_Coord(c.block)));
  for (UINT32 i=0;i<3;++i) WN_INSERT_BlockLast(body,LN4_Store(c.addresses[i],LN4_Address(c.scratch,LN4_I4(i))));
  if (c.plan->guard.kind==3) {
    WN *stage=WN_CreateBlock();WN_INSERT_BlockLast(stage,LN4_Write(c,LN4_Address(c.arena,LN4_Load(c.k)),LN4_Read(LN4_Matrix_Address(c,c.x))));
    WN_INSERT_BlockLast(body,LN4_Loop(c,stage,TRUE));LN4_Barrier(c,body);
  }
  WN_INSERT_BlockLast(body,LN4_Mean(c));
  if (c.arena) {LN4_Barrier(c,body);WN_INSERT_BlockLast(body,LN4_Center(c));LN4_Barrier(c,body);}
  WN_INSERT_BlockLast(body,LN4_Variance(c));if (c.arena) LN4_Barrier(c,body);
  WN_INSERT_BlockLast(body,LN4_Epilogue(c));if (c.arena) LN4_Barrier(c,body);
  WN_INSERT_BlockLast(body,WN_CreateReturn());return body;
}

/* Hash executable structure/symbol attributes, reject shared WN nodes and
 * unsupported DSL constructs; ASM has a separate exact scalar verifier. */
static BOOL LN4_Tree(const WN *n,UINT64 &hash,WOPT_DSL_NORMALIZATION_DEVICE &facts,std::set<const WN *> &seen)
{
  if (!n || !seen.insert(n).second) return FALSE;
  ++facts.nodes;OPCODE opc=WN_opcode(n);OPERATOR op=WN_operator(n);
  if (op==OPR_DSL) return FALSE;
  LN4_Word(hash,opc);LN4_Word(hash,WN_kid_count(n));
  if (OPCODE_has_sym(opc)) {
    ST_IDX idx=WN_st_idx(n);if (!idx) return FALSE;
    const ST &s=St_Table[idx];LN4_Word(hash,idx);LN4_Text(hash,ST_name(s));LN4_Word(hash,ST_class(s));
    LN4_Word(hash,ST_sclass(s));LN4_Word(hash,ST_export(s));LN4_Word(hash,ST_device_symbol_kind(s));
    if (ST_class(s)==CLASS_VAR) {
      TY_IDX ty=ST_type(s);LN4_Word(hash,ty);LN4_Word(hash,TY_kind(ty));LN4_Word(hash,TY_mtype(ty));
      LN4_Word(hash,TY_size(ty));LN4_Word(hash,TY_align(ty));LN4_Word(hash,TY_flags(ty));LN4_Word(hash,ST_is_value_parm(s));
      if (TY_kind(ty)==KIND_POINTER) {LN4_Word(hash,TY_kind(TY_pointed(ty)));LN4_Word(hash,TY_mtype(TY_pointed(ty)));LN4_Word(hash,TY_flags(TY_pointed(ty)));}
      if (TY_kind(ty)==KIND_ARRAY) {
        ARB_HANDLE bounds=TY_arb(ty);
        if (bounds.Is_Null() || ARB_dimension(bounds)!=1 || !ARB_const_lbnd(bounds) || !ARB_const_ubnd(bounds) || !ARB_const_stride(bounds)) return FALSE;
        LN4_Word(hash,TY_mtype(TY_etype(ty)));LN4_Word(hash,ARB_lbnd_val(bounds));
        LN4_Word(hash,ARB_ubnd_val(bounds));LN4_Word(hash,ARB_stride_val(bounds));
      }
    }
  }
  if (OPCODE_has_offset(opc)) LN4_Word(hash,WN_offset(n));
  if (OPCODE_has_1ty(opc) || OPCODE_has_2ty(opc)) LN4_Word(hash,WN_ty(n));
  if (OPCODE_has_2ty(opc)) LN4_Word(hash,WN_load_addr_ty(n));
  if (op==OPR_IDNAME) LN4_Word(hash,WN_idname_offset(n));
  if (op==OPR_LDID || op==OPR_STID || op==OPR_ILOAD || op==OPR_ISTORE) LN4_Word(hash,WN_field_id(n));
  if (op==OPR_INTCONST) LN4_Word(hash,WN_const_val(n));
  if (op==OPR_CONST) {float f=(float)Targ_To_Host_Float(Tcon_Table[ST_tcon(WN_st(n))]);UINT32 b;memcpy(&b,&f,4);LN4_Word(hash,b);}
  if (op==OPR_DO_LOOP) ++facts.loops;
  if (op==OPR_CALL) {LN4_Word(hash,WN_call_flag(n));if (!strcmp(ST_name(WN_st(n)),"__syncthreads")) ++facts.barriers;}
  if (op==OPR_ASM_STMT) {
    BOOL valid=FALSE;for (UINT32 i=1;i<=5;++i) if (WOPT_DSL_verify_normalization_scalar(n,i)) valid=TRUE;
    if (!valid) return FALSE;++facts.scalar_operations;LN4_Text(hash,WN_asm_string(n));LN4_Word(hash,WN_asm_flag(n));
  }
  if (op==OPR_ASM_INPUT) {LN4_Text(hash,WN_asm_input_constraint(n));LN4_Word(hash,WN_asm_opnd_num(n));}
  if (op==OPR_BLOCK) {for (const WN *s=WN_first(n);s;s=WN_next(s)) if (!LN4_Tree(s,hash,facts,seen)) return FALSE;}
  else for (INT32 k=0;k<WN_kid_count(n);++k) if (!LN4_Tree(WN_kid(n,k),hash,facts,seen)) return FALSE;
  return TRUE;
}

/* Derive proof-of-identity from the active real device tree, not a sidecar
 * instruction list; target placement attributes participate in the hash. */
static BOOL LN4_Facts(const WOPT_DSL_NORMALIZATION_LOWERING &p,WOPT_DSL_NORMALIZATION_DEVICE *facts)
{
  if (!LN4_Active() || ST_device_symbol_kind(St_Table[PU_Info_proc_sym(Current_PU_Info)])!=DEVICE_SYMBOL_KERNEL) return FALSE;
  memset(facts,0,sizeof(*facts));facts->plan_seal=p.seal;UINT64 hash=1469598103934665603ULL;std::set<const WN *> seen;
  if (!LN4_Tree(PU_Info_tree_ptr(Current_PU_Info),hash,*facts,seen)) return FALSE;
  facts->tree_seal=hash;return facts->barriers==(p.guard.kind==1?0U:p.guard.kind==2?4U:5U) &&
      facts->loops==(p.guard.kind==1?3U:p.guard.kind==2?4U:5U) && facts->scalar_operations==(p.guard.kind==1?13U:12U);
}

/* Map generated statements to the selected source member; expression kids
 * retain ordinary WHIRL rules rather than receiving invented statement fields. */
static void LN4_Position(WN *n,SRCPOS source)
{
  if (OPCODE_has_next_prev(WN_opcode(n)) || WN_operator(n)==OPR_FUNC_ENTRY) WN_Set_Linenum(n,source);
  if (WN_operator(n)==OPR_BLOCK) {for (WN *s=WN_first(n);s;s=WN_next(s)) LN4_Position(s,source);}
  else for (INT32 i=0;i<WN_kid_count(n);++i) if (WN_kid(n,i)) LN4_Position(WN_kid(n,i),source);
}

/* Compact action rows are written only after successful materialization or
 * verification; original source coordinate remains the final column. */
static void LN4_Trace(const char *action,const WOPT_DSL_NORMALIZATION_LOWERING &p,
    const WOPT_DSL_NORMALIZATION_DEVICE &d,FILE *trace)
{
  if (!trace) return;
  fprintf(trace,"AI_NORM_LOWER 1 %s %llu %llu %llu %llu %llu %llu %llu %llu %llu %llu %llu %llu\n",action,
      (unsigned long long)p.guard.pu,(unsigned long long)p.guard.node,(unsigned long long)p.guard.kind,
      (unsigned long long)p.seal,(unsigned long long)d.tree_seal,(unsigned long long)d.nodes,
      (unsigned long long)d.loops,(unsigned long long)d.scalar_operations,(unsigned long long)d.barriers,
      (unsigned long long)p.guard.reserved_shared,(unsigned long long)p.guard.threads,(unsigned long long)p.guard.source_line);
}
}

/* Constant construction and 64-bit pointer ASM require a strict compiler
 * session, independently of the numerical environment promised to the GPU. */
BOOL WOPT_DSL_normalization_compiler_environment_ready()
{
  if (Pointer_Size!=8 || Pointer_Mtype!=MTYPE_U8 || OPT_Fast_Math ||
      Roundoff_Level!=ROUNDOFF_NONE || IEEE_Arithmetic!=IEEE_ACCURATE || fegetround()!=FE_TONEAREST) return FALSE;
#if defined(__SSE__)
  UINT32 control=_mm_getcsr();
  if ((control & ((1U<<15) | (1U<<6) | 0x6000U)) || (control & 0x1f80U)!=0x1f80U) return FALSE;
#endif
  return TRUE;
}

/* Gate the expected imported stamp before recapturing current source/costs.
 * Recompute LN3 and compare the whole POD result; even an unchanged numeric
 * choice cannot revive stale calibration or budget evidence. Preserve outputs
 * on refusal and seal the imported handoff without changing legacy seals. */
BOOL WOPT_DSL_prepare_normalization_lowering(WN *scope,WN *assignment,
    const WOPT_DSL_NORMALIZATION_COST_REQUEST *r,const WOPT_DSL_NORMALIZATION_COST_RESULT *expected,
    WOPT_DSL_NORMALIZATION_LOWERING *output,FILE *diagnostic,FILE *trace)
{
  if (!r || !expected || !output ||
      !DSL_gpu_plan_stamp_current(expected->gpu_stamp,VHO_DSL_GPU_Context)) return FALSE;
  WOPT_DSL_NORMALIZATION_COST_RESULT current;
  if (!WOPT_DSL_select_normalization_plan(scope,assignment,r,&current,diagnostic,NULL) ||
      memcmp(&current,expected,sizeof(current)) || !current.selected_kind) return FALSE;
  WOPT_DSL_NORMALIZATION_LOWERING p;memset(&p,0,sizeof(p));
  p.version=1;p.revision=r->profile.description_revision;p.profile=r->profile;p.guard=current.selected_guard;
  p.cost=current.costs[current.selected_kind-1];p.registers_per_thread=r->registers_per_thread[current.selected_kind-1];
  p.max_jobs=r->max_jobs;p.margin=r->margin;
  p.gpu_stamp=current.gpu_stamp;p.gpu_live_bytes=current.gpu_live_bytes;p.seal=LN4_Plan_Seal(p);
  if (!LN4_Plan_Valid(p)) return FALSE;
  *output=p;
  if (trace) fprintf(trace,"AI_NORM_LOWER 1 PREPARE %llu %llu %llu %llu %llu %llu\n",(unsigned long long)p.guard.pu,
      (unsigned long long)p.guard.node,(unsigned long long)p.guard.kind,(unsigned long long)p.seal,
      (unsigned long long)p.cost.total,(unsigned long long)p.guard.source_line);
  return TRUE;
}

/* The driver selects a new empty kernel PU. Revalidate the selected imported
 * handoff before symbol/WN allocation; only that active scope is mutated.
 * Recoverable refusal preserves the empty PU and output; no selection occurs. */
BOOL WOPT_DSL_materialize_normalization_device(const WOPT_DSL_NORMALIZATION_LOWERING *p,SRCPOS source,
    WOPT_DSL_NORMALIZATION_DEVICE *output,FILE *diagnostic,FILE *trace)
{
  if (!p || !output || !LN4_Plan_Valid(*p) || !LN4_Active() || !LN4_Strict() || !source ||
      !SRCPOS_filenum(source) || SRCPOS_linenum(source)<=0) return FALSE;
  WN *old=PU_Info_tree_ptr(Current_PU_Info);
  ST *function=&St_Table[PU_Info_proc_sym(Current_PU_Info)];
  if (WN_num_formals(old) || !WN_func_body(old) || WN_operator(WN_func_body(old))!=OPR_BLOCK ||
      WN_first(WN_func_body(old)) || ST_device_symbol_kind(*function)!=DEVICE_SYMBOL_KERNEL || !LN4_Builtins_Valid()) return FALSE;
  LN4_CONTEXT c;memset(&c,0,sizeof(c));c.plan=p;c.source=source;c.pointer=Make_Pointer_Type(MTYPE_To_TY(MTYPE_F4));
  TY_IDX type;TY &t=New_TY(type);TY_Init(t,0,KIND_FUNCTION,MTYPE_UNKNOWN,0);Set_TY_has_prototype(type);
  TYLIST_IDX list;Set_TYLIST_type(New_TYLIST(list),MTYPE_To_TY(MTYPE_V));Set_TY_tylist(t,list);
  for (UINT32 i=0;i<4;++i) Set_TYLIST_type(New_TYLIST(list),c.pointer);
  Set_TYLIST_type(New_TYLIST(list),TY_IDX_ZERO);Set_PU_prototype(Pu_Table[ST_pu(function)],type);
  const char *names[]={"x","gamma","beta","y"};ST *formals[4];
  for (UINT32 i=0;i<4;++i) {formals[i]=New_ST(CURRENT_SYMTAB);ST_Init(formals[i],Save_Str(names[i]),CLASS_VAR,SCLASS_FORMAL,EXPORT_LOCAL,c.pointer);Set_ST_is_value_parm(formals[i]);Set_ST_Srcpos(*formals[i],source);}
  c.x=formals[0];c.gamma=formals[1];c.beta=formals[2];c.y=formals[3];
  if (p->guard.kind!=1) {
    TY_IDX arena=Make_Array_Type(MTYPE_F4,1,p->guard.reserved_shared/4);Set_TY_align(arena,16);Set_TY_is_user_align(arena);
    c.arena=LN4_Local("ln_shared",arena,source);Set_ST_device_symbol_kind(*c.arena,DEVICE_SYMBOL_SHARED);
  }
  c.scratch=LN4_Local("ln_carrier",Make_Array_Type(MTYPE_F4,1,3),source);
  c.addresses[0]=LN4_Local("ln_out_ptr",c.pointer,source);c.addresses[1]=LN4_Local("ln_left_ptr",c.pointer,source);c.addresses[2]=LN4_Local("ln_right_ptr",c.pointer,source);
  c.tid=LN4_Local("ln_tid",MTYPE_To_TY(MTYPE_I4),source);c.row=LN4_Local("ln_row",MTYPE_To_TY(MTYPE_I4),source);c.k=LN4_Local("ln_k",MTYPE_To_TY(MTYPE_I4),source);
  c.sum=LN4_Local("ln_sum",MTYPE_To_TY(MTYPE_F4),source);c.mu=LN4_Local("ln_mu",MTYPE_To_TY(MTYPE_F4),source);
  c.variance=LN4_Local("ln_var",MTYPE_To_TY(MTYPE_F4),source);c.q=LN4_Local("ln_q",MTYPE_To_TY(MTYPE_F4),source);
  c.d=LN4_Local("ln_d",MTYPE_To_TY(MTYPE_F4),source);c.u=LN4_Local("ln_u",MTYPE_To_TY(MTYPE_F4),source);c.z=LN4_Local("ln_z",MTYPE_To_TY(MTYPE_F4),source);
  c.thread=LN4_Coordinate("threadIdx",source);c.block=LN4_Coordinate("blockIdx",source);c.barrier=c.arena?LN4_Barrier_Symbol(source):NULL;
  WN *entry=WN_CreateEntry(4,function,WN_CreateBlock(),NULL,NULL);
  for (UINT32 i=0;i<4;++i) WN_kid(entry,i)=WN_CreateIdname(0,formals[i]);
  Set_PU_Info_tree_ptr(Current_PU_Info,entry);WN_func_body(entry)=LN4_Body(c);LN4_Position(entry,source);
  WOPT_DSL_NORMALIZATION_DEVICE d;FmtAssert(LN4_Facts(*p,&d),("LN4 generated tree contract failed"));
  *output=d;LN4_Trace("MATERIALIZE",*p,d,trace);
  if (trace) {
    const char *actions[]={"FORMALS_SUBRANGE_X_GAMMA_BETA_Y","PRIVATE_CARRIER_3_F32","SHARED_ARENA",
      "X_COPY_TO_SHARED","MEAN_ASCENDING_F32","MU_PUBLISH_BARRIER","CENTER_FROM_X","CENTER_X_TO_D_IN_PLACE",
      "CENTER_VISIBLE_BARRIER","VARIANCE_ASCENDING_F32","Q_PUBLISH_BARRIER","AFFINE_POINTWISE_F32","READERS_RELEASE_BARRIER"};
    for (UINT32 i=0;i<sizeof(actions)/sizeof(actions[0]);++i) {
      BOOL emit=i<2 || i==4 || i==9 || i==11 || (c.arena && i!=3 && i!=6 && i!=7) ||
          (p->guard.kind==3 && (i==3 || i==7)) || (p->guard.kind==2 && i==6);
      if (emit) fprintf(trace,"AI_NORM_LOWER 1 ACTION %llu %s %llu %llu\n",(unsigned long long)p->guard.kind,actions[i],
          (unsigned long long)(i==2?p->guard.reserved_shared:0),(unsigned long long)p->guard.source_line);
      if (i==3 && p->guard.kind==3) fprintf(trace,"AI_NORM_LOWER 1 ACTION 3 ROW_VISIBLE_BARRIER 0 %llu\n",(unsigned long long)p->guard.source_line);
    }
  }
  return TRUE;
}

/* Revalidate the imported selected handoff, then compare actual active device
 * WHIRL, scalar instructions, barriers and resource attributes. Read-only;
 * stale context/tree evidence preserves output and cannot authorize emission. */
BOOL WOPT_DSL_verify_normalization_device(const WOPT_DSL_NORMALIZATION_LOWERING *p,
    const WOPT_DSL_NORMALIZATION_DEVICE *expected,FILE *diagnostic,FILE *trace)
{
  WOPT_DSL_NORMALIZATION_DEVICE actual;
  if (!p || !expected || !LN4_Plan_Valid(*p) || !LN4_Facts(*p,&actual) || memcmp(&actual,expected,sizeof(actual))) return FALSE;
  LN4_Trace("VERIFY",*p,actual,trace);return TRUE;
}
