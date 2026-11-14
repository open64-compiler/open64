/* Copyright (C) 2026 Open64 Project */
/* Owned Q1 algorithm persistence and phase annotation, no JSON library ABI.
 * Design: doc/AI-COMPILER-MLA-Q1-PRODUCTION-v1.0.md. Native owners alone select
 * scopes and mutate; these records grant no optimization/search authority. */
#ifndef tensor_q1_program_INCLUDED
#define tensor_q1_program_INCLUDED
#include "tensor_kernel_lower.h"
/* Canonical versioned metadata, independent of an external recipe file. */
extern std::string VHO_q1_program_encode(const VHO_TENSOR_PROGRAM &);
/* Fail closed and leave output unchanged on malformed/stale source data. */
extern bool VHO_q1_program_decode(const std::string &,VHO_TENSOR_PROGRAM &,std::string &);
/* Read the active function's complete algorithm binding without native mutation. */
extern bool VHO_q1_program_read(WN *,VHO_TENSOR_PROGRAM &,std::string &);
/* Restore source explanations only at proved operation-loop joins. This is
 * annotation after PREOPT, outside analysis/search and before fact capture. */
extern unsigned VHO_q1_restore_comments(WN *,const VHO_TENSOR_PROGRAM &);
/* Optional driver/phase checkpoint using the existing mapped writer. The
 * active PU is copied only for subsection state; no scope switch occurs. */
extern bool VHO_q1_checkpoint(WN *,const char *,bool);
#endif
