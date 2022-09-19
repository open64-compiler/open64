/*
   Copyright (C) 2019-2022 Xcalibyte (Shenzhen) Limited.

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
 */

// ==================================================================
//
// Module: opt_vsa_vcg.h
//
// ==================================================================

//
// Visualization data structures in IPSA and VSA based on VCG
//
// Prepare: install graph-easy and graphviz
// $ sudo apt install libgraph-easy-perl
// $ sudo apt install graphviz
//
// Usage:
// 1. Generate call graph vcg file
// $ xvsa ... -VSA:-VSA:vcg_cg=proj_name
// 2. Convert proj_name_cg.vcg to dot file
// $ graph-easy proj_name_cg.vcg --as_dot --output=proj_name_cg.dot
// 3. Draw dot or convert to svg
// $ dot -Tsvg proj_name_cg.dot -o proj_name_cg.svg
// 4. View svg file with a browser
//

#ifndef opt_vsa_vcg_INCLUDED
#define opt_vsa_vcg_INCLUDED


#include "defs.h"
#include "errors.h"
#include "erglob.h"
#include "vcg.h"
#include "cxx_memory.h"

// =============================================================================
// IPSA_VCG_BASE
// Base class for VCG builder for IPSA
// =============================================================================
class IPSA_VCG_BASE {
private:
  CXX_MEM_POOL _pool;      // memory pool for VCG dump
  const char  *_fname;     // VCG file name

  IPSA_VCG_BASE(const IPSA_VCG_BASE &);             // disable copy ctor
  IPSA_VCG_BASE &operator=(const IPSA_VCG_BASE &);  // disable assign oper

protected:
  IPSA_VCG_BASE(const char *fname)
    : _pool("IPSA VCG pool", FALSE), _fname(fname) {}

  const char *Fname() const { return _fname; }
  MEM_POOL *Mem_pool() { return _pool(); }

  BOOL Is_root(const char *fname)
  {
    BOOL ret = FALSE;
    if (strncmp(fname, "Os_Task", 7) == 0 ||
        strncmp(fname, "Os_Isr", 6) == 0 ||
        strncmp(fname, "ShutdownHook", 12) == 0 ||
        strncmp(fname, "Brs_PreMainStartup", 18) == 0) {
      ret = TRUE;
    }
    return ret;
  }

  VCGNode *Build_node(const char *title, const char *label, NodeShape shape)
  {
    VCGNode *node = CXX_NEW(VCGNode(title, label, shape), Mem_pool());
    Is_True(node != NULL, ("VCG out of memory"));
    if (Is_root(label)) {
      node->backGroundColor(LightGreen);
    }
    return node;
  }

  VCGEdge *Build_edge(const char *src, const char *dst, VCGEdgeLineStyle style)
  {
    VCGEdge *edge = CXX_NEW(VCGEdge(src, dst), Mem_pool());
    Is_True(edge != NULL, ("VCG out of memory"));
    edge->lineStyle(style);
    return edge;
  }
}; // IPSA_VCG_BASE


#endif /* opt_vsa_vcg_INCLUDED */
