/* Copyright (C) 2026 Open64 Project */
/* VHO-owned Q5 synchronous quantized MLA plan qualification. Detached source
 * contracts and selected-only native construction; no runtime dispatch policy.
 * Design: doc/AI-COMPILER-MLA-Q5-SELECTED-PLANS-v1.0.md. */
#ifndef tensor_quantized_plan_INCLUDED
#define tensor_quantized_plan_INCLUDED
#include "tensor_quantized_movement.h"
struct VHO_Q5_LAYOUT {
    VHO_TENSOR_SERVING_SCHEMA schema;
    UINT32 start,encoded[4],indices;
    std::vector<UINT32> removed;
    /* Missing identities cannot become guessed source/scale joins. */
    VHO_Q5_LAYOUT():start(0),indices(~0U) {for(unsigned i=0;i<4;++i) encoded[i]=~0U;}
};
struct VHO_Q5_PLAN {
    UINT32 width,group;
    std::string source_key,selection_key,native_key,binding_key;
    /* Width zero retains the ordinary materialized source as executable fallback. */
    VHO_Q5_PLAN():width(0),group(1) {}
};
/* Recognize one closed Q2 attention domain and exact S8/U4 decode/gather chains.
 * All surviving uses and private tile capacities must validate before mutation. */
extern bool VHO_q5_layout(const VHO_TENSOR_PROGRAM &,VHO_Q5_LAYOUT &,std::string &);
/* Versioned full descriptor; Q4 hypotheses never grant Q5 application rights. */
extern const char *VHO_q5_catalog();
extern std::string VHO_q5_plan_key(const VHO_Q5_PLAN &);
/* Canonical header-only native serialization. Decode leaves output unchanged. */
extern std::string VHO_q5_plan_encode(const VHO_Q5_PLAN &);
extern bool VHO_q5_plan_decode(const std::string &,VHO_Q5_PLAN &,std::string &);
/* Stage exactly one fixed algorithm body in the active Q1 buffer ABI. Caller
 * owns native savepoint, verification, commit/rollback and analysis lifetime. */
extern WN *VHO_q5_build_high(const VHO_TENSOR_PROGRAM &,const VHO_Q5_PLAN &,WN *);
/* Explanations describe actual scalar High ownership; GPU mapping is unqualified. */
extern std::string VHO_q5_explanation(const VHO_Q5_PLAN &,const VHO_TENSOR_PROGRAM * = NULL);
#endif
