/*
 * Copyright (C) 2009 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

/*
 * Copyright 2002, 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
 */

/*

  Copyright (C) 2000, 2001 Silicon Graphics, Inc.  All Rights Reserved.

  This program is free software; you can redistribute it and/or modify it
  under the terms of version 2 of the GNU General Public License as
  published by the Free Software Foundation.

  This program is distributed in the hope that it would be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  

  Further, this software is distributed without any warranty that it is
  free of the rightful claim of any third person regarding infringement 
  or the like.  Any license provided herein, whether implied or 
  otherwise, applies only to this software file.  Patent licenses, if 
  any, provided herein do not apply to combinations of this program with 
  other software, or any other product whatsoever.  

  You should have received a copy of the GNU General Public License along
  with this program; if not, write the Free Software Foundation, Inc., 59
  Temple Place - Suite 330, Boston MA 02111-1307, USA.

  Contact information:  Silicon Graphics, Inc., 1600 Amphitheatre Pky,
  Mountain View, CA 94043, or:

  http://www.sgi.com

  For further information regarding this notice, see:

  http://oss.sgi.com/projects/GenInfo/NoticeExplan

*/


#include <stdint.h>
#ifdef USE_PCH
#include "lno_pch.h"
#endif // USE_PCH
#pragma hdrstop

#include <math.h>
#ifdef KEY // to get DBL_MAX
#include <float.h>
#endif
#include <sys/types.h>
#include <limits.h>
#include "pu_info.h"
#include "lnoutils.h"
#include "lnopt_main.h"
#include "stdlib.h"
#include "fiz_fuse.h"
#include "ara.h"
#include "cross_snl.h"
#include "snl_utils.h"
#include "cross_cache.h"
#include "parmodel.h" 

static INT cross_loop_debug_level = 2;
BOOL running_cross_loop_analysis = FALSE;

/* --------------------------------------------------------------------- */
ARA_REF_INFO::ARA_REF_INFO(ARA_REF* ref, ARA_LOOP_INFO* leaf)
{
  
  _is_messy = ref->Is_Messy();

  _ref =  CXX_NEW(ARA_REF(*ref), &LNO_local_pool);
  _proj_ref = CXX_NEW(ARA_REF(*ref), &LNO_local_pool);

  if (!ref->Is_Too_Messy()) {
    REGION_ITER iter(&_ref->Image());
    _dim = iter.First()->Num_Dim();
  } else {
    _dim = -1;
  }

  ARA_LOOP_INFO *node = leaf;
  
  while (node) {
   _proj_ref->Image().RegionUN_Projection(node->Depth(), *node);
    node = node->Parent();
  }

  if (cross_loop_debug_level >= 3) {
    fprintf(stdout, "Before : \n");
    _ref->Print(stdout);
    fprintf(stdout, "After : \n");
    _proj_ref->Print(stdout);
  }
}


void ARA_REF_INFO::Print(FILE *file)
{
  fprintf(file, "IS_MESSY : %s\n", (_is_messy) ? "TRUE" : "FALSE");
  fprintf(file, "Before Projection :\n");
  _ref->Print(file);
  fprintf(file, "After Projection  :\n");
  _proj_ref->Print(file);
  fprintf(file, "Dimensions : %d\n", _dim);
  
}

/* ---------------------------------------------------------------------- */
ARRAY_SNL_INFO::ARRAY_SNL_INFO(WN* wn_outer, INT nloops, 
			       ARA_LOOP_INFO *ara_root) :
_rd_ref_list(&LNO_local_pool),
_wr_ref_list(&LNO_local_pool)
{
  _snl_root = wn_outer;
  _depth    = nloops;
  _ara_root = ara_root;

  _snl_leaf = SNL_Get_Inner_Snl_Loop(wn_outer, nloops);
  DO_LOOP_INFO *dli = Get_Do_Loop_Info(_snl_leaf);
  _ara_leaf = dli->ARA_Info;
}

void ARRAY_SNL_INFO::Add_Reference(ARA_REF_INFO_ST * ref_list, ARA_REF *ref)
{
  // We spilt the regions unioned in ref and store them in ARA_REF_INFO
  REGION_ITER iter(&ref->Image());
  

  for (REGION *curr = iter.First(); !iter.Is_Empty(); curr = iter.Next()) {
    ARA_REF * new_ref = CXX_NEW(ARA_REF(*ref), &LNO_local_pool);


    // need a better way of emptying the list */
    while (!new_ref->Image().Is_Empty()) {
      REGION * tmp = new_ref->Image().Remove_Headnode();
    }


    // now insert the current reference into the list
    if (cross_loop_debug_level >= 3) {
      fprintf(stdout, "Before Inserting :\n");
      curr->Print(stdout);
      new_ref->Print(stdout);
    }

    new_ref->Image().Append(CXX_NEW(REGION(*curr), &LNO_local_pool));
    if (cross_loop_debug_level >= 3) {
      fprintf(stdout, "After Inserting :\n");
      new_ref->Print(stdout);
    }


    ref_list->Push(CXX_NEW(ARA_REF_INFO(new_ref, _ara_leaf), &LNO_local_pool));
  }
}

void ARRAY_SNL_INFO::Add_Write_Reference(ARA_REF *ref)
{
  Add_Reference(&_wr_ref_list, ref);
}

void ARRAY_SNL_INFO::Add_Read_Reference(ARA_REF *ref)
{
  Add_Reference(&_rd_ref_list, ref);
}



/*
 * Walk_SNL : go through the SNL and gather all
 *            the references
 */

void ARRAY_SNL_INFO::Walk_SNL(void)
{
  /*
   * we are ignoring all the refernces made in the
   * sandwich code between loops
   */
  WN* inner_most = _snl_leaf;
  ARA_LOOP_INFO *ali = _ara_leaf;

  if (ali == NULL) {
    return; /* no innermost data to walk! */
  }
  
  ali->Walk_Block(WN_do_body(inner_most));

  if (cross_loop_debug_level >= 3) {
    fprintf(stdout, "References :\n");
    ali->Print(stdout, FALSE);
    fprintf(stdout,"\n");
  }

  /*
   * Store the array refs in our structure 
   */
  ARA_REF_ST & def_stack = ali->DEF();
  INT i;
  for (i = 0; i < def_stack.Elements(); ++i) {
    Add_Write_Reference(def_stack.Bottom_nth(i));
  }

  ARA_REF_ST & use_stack = ali->USE();
  for (i = 0; i < use_stack.Elements(); ++i) {
    Add_Read_Reference(use_stack.Bottom_nth(i));
  }
}

void ARRAY_SNL_INFO::Print(FILE *file) 
{
  fprintf(file, "Read References : \n");
  INT i;
  for (i = 0; i < _rd_ref_list.Elements(); ++i) {
    _rd_ref_list.Bottom_nth(i)->Print(file);
  }

  fprintf(file, "Write References : \n");
  for (i = 0; i < _wr_ref_list.Elements(); ++i) {
    _wr_ref_list.Bottom_nth(i)->Print(file);
  }
  fprintf(file, "_snl_root = %p\n _snl_leaf = %p\n _depth = %d\n"
	  "_ara_root = %p\n _ara_leaf = %p\n", 
	  _snl_root, _snl_leaf, _depth, _ara_root, _ara_leaf);
	  
}

/* ---------------------------------------------------------------------- */
/*
 * Gather_Array_References
 * 
 *       Go through the SNL and add the array references as regions.
 *       No merging or projection is done at this point
 */

void Gather_Array_References (ARRAY_SNL_INFO *asi)
{
  asi->Walk_SNL();
}


/*
 * NAME : SNL_Array_Analysis :
 *       
 *        Analyzes an SNL to determine the array usage patterns
 */

void SNL_Array_Analysis(WN* wn_outer, INT nloops)
{
  // we are going to use some of the structure used
  // for array region analysis
  ARA_LOOP_INFO *ara_root =
    CXX_NEW(ARA_LOOP_INFO(wn_outer, NULL, TRUE), &ARA_memory_pool);  
  DO_LOOP_INFO* dli = Get_Do_Loop_Info(wn_outer);
  ARA_Initialize_Loops(wn_outer, ara_root);

  ARRAY_SNL_INFO *asi = CXX_NEW(ARRAY_SNL_INFO(wn_outer, nloops, ara_root),
				&LNO_local_pool);

  /* Create the array references read/ written in this SNL */
  Gather_Array_References(asi);

  if (cross_loop_debug_level >= 2) {
    asi->Print(stdout);
  }

  ARA_Cleanup(wn_outer);
}


INT Intersect_References(CACHE_CONTENTS *cc, ARRAY_SNL_INFO *asi, INT32 parallel_loop,
			  ARA_REF_INFO_ST *refs, double *denom, double *numer)
{
  *denom = 0.0;
  *numer = 0.0;

  for (INT i = 0; i < refs->Elements(); ++i) {
    ARA_REF_INFO *ari = refs->Bottom_nth(i);

    FmtAssert(!ari->Is_Messy(), ("Determiner_Intersection : messy reference"));
    CACHE_REGION *cr = CXX_NEW(CACHE_REGION(ari, asi, parallel_loop), &LNO_local_pool);
    if (cr->Is_Messy()) {
      return 0;
    }

    if (cross_loop_debug_level >= 3) {
      fprintf(stdout, "Cache Region : \n");
      cr->Print(stdout);
    }

    INT32 isize = cc->Intersect_Region(cr);
    INT32 rsize = cr->Region_Size();

    if (rsize == -1 || isize == -1) {
      return 0;
    }

    FmtAssert(isize != -1, ("Determine_Intersection : intersection failed"));
    FmtAssert(rsize != -1, ("Determine_Intersection : region size failed"));  

    if (cross_loop_debug_level >= 3) {
      fprintf(stdout, "Intersection size : %d\n", isize);
      fprintf(stdout, "Region size : %d\n", rsize);
    }

    *numer += isize;
    *denom += rsize;

    CXX_DELETE(cr, &LNO_local_pool);
  }

  return 1;
}

void Add_References(CACHE_CONTENTS *cc, ARRAY_SNL_INFO *asi, INT32 parallel_loop,
		    ARA_REF_INFO_ST *refs, ACCESS_TYPE atype)
{
  for (INT i = 0; i < refs->Elements(); ++i) {
    ARA_REF_INFO *ari = refs->Bottom_nth(i);

    FmtAssert(!ari->Is_Messy(), ("Add_References : messy reference"));
    CACHE_REGION *cr = CXX_NEW(CACHE_REGION(ari, asi, parallel_loop), &LNO_local_pool);

    if (cross_loop_debug_level >= 3) {
      fprintf(stdout, "Cache Region : \n");
      cr->Print(stdout);
    }

    cc->Add_Region(cr, atype);
  }
}

// intersect the references in asi with the contents of the cache
// return the ratio of the data accessed already present in the cache
double Determine_Intersection(CACHE_CONTENTS *cc, ARRAY_SNL_INFO *asi, INT32 parallel_loop)
{
  double rd_numer = 0.0;
  double rd_denom = 0.0;
  double wr_numer = 0.0;
  double wr_denom = 0.0;

  ARA_REF_INFO_ST *rd_refs = & asi->Read_Refs();
  ARA_REF_INFO_ST *wr_refs = & asi->Write_Refs();

  if (!Intersect_References(cc, asi, parallel_loop, rd_refs, &rd_denom, &rd_numer)) {
    return 0.0;
  }

  if (!Intersect_References(cc, asi, parallel_loop, wr_refs, &wr_denom, &wr_numer)) {
    return 0.0;
  }

  if (rd_denom + wr_denom == 0.0) {
    return 1.0;
  } else {
    double ratio = (rd_numer + wr_numer) / (rd_denom + wr_denom);
    return MIN(ratio, 1.0);
  }
}


// update the cache contents 
void Update_Cache_Contents(CACHE_CONTENTS *cc, ARRAY_SNL_INFO *asi, INT32 parallel_loop)
{
  ARA_REF_INFO_ST *rd_refs = & asi->Read_Refs();
  ARA_REF_INFO_ST *wr_refs = & asi->Write_Refs();

  // first add the write references - then the read.
  // this hueristic will remove the regions killed
  // from the cache first
  Add_References(cc, asi, parallel_loop, wr_refs, CACHE_WRITE_ONLY);
  Add_References(cc, asi, parallel_loop, rd_refs, CACHE_READ_ONLY);
}
			   
// Given a path of execution determine the cache contents
// at each point of the execution
static double Explore_Path(SNL_STREAM *sst, INT32* curr_path, double curr_min)
{
  if (cross_loop_debug_level >= 1) {
    fprintf(stdout, "Execution path : [");
    for (INT i = 0; i < sst->Num_SNL(); ++i) {
      fprintf(stdout, " %d ", curr_path[i]);
    }
    fprintf(stdout, "]\n");
  }
    
  CACHE_CONTENTS *cc = CXX_NEW(CACHE_CONTENTS(FULLY_ASSOCIATIVE, LONG_MAX, NOMINAL_PROCS, 
					    sst->Get_Ali()), &LNO_local_pool);

  double cost = 0.0;
  for (INT i = 0; i < sst->Num_SNL(); ++i) {
    CROSS_SNL_INFO *csi = sst->Get_SNL(i);
    double stage_cost = 0.0;

    // find the parallel loop
    INT parallel_loop = 0;
    PARALLEL_INFO *pi;
    if (curr_path[i] != -1) {
      pi = csi->Get_Parallel_Option(curr_path[i]);
      parallel_loop = pi->Parallel_Loop() + 1;
      FmtAssert(parallel_loop >= 1 && parallel_loop <= csi->SNL_Depth(),
		("Illegal parallel loop"));
      if (cross_loop_debug_level >= 2) {
	pi->Print(stdout);
      }

      stage_cost = pi->Machine_Cost() + pi->Reduction_Cost() 
	+ pi->Parallel_Overhead_Cost();

      if (pi->Is_Doacross()) {
	stage_cost += pi->Doacross_Overhead();
      }
    } else {
      stage_cost = csi->Get_Seq_Machine_Cost();
    

      if (cross_loop_debug_level >= 2) {
	fprintf(stdout, "seq machine cost = %lf seq cache cost = %lf\n",
		csi->Get_Seq_Machine_Cost(),csi->Get_Seq_Cache_Cost()); 
      }
    }
    
    double iratio = Determine_Intersection(cc, csi->Get_Array_References(), 
					   parallel_loop);
    if (curr_path[i] != -1) {
      stage_cost = stage_cost + (1.0 - iratio) * pi->Cache_Cost();
    } else {
      stage_cost = stage_cost + (1.0 - iratio) * csi->Get_Seq_Cache_Cost();
    }

    cost = cost + stage_cost;

    if (cross_loop_debug_level >= 2) {
      fprintf(stdout, "Intersection ratio : %lf\n", iratio);
      fprintf(stdout, "stage cost = %lf curr cost = %lf \n", stage_cost, cost);
    }

    if (cross_loop_debug_level >= 3) {
      fprintf(stdout, "CACHE_CONTENTS before updating : \n");
      cc->Print(stdout);
      fprintf(stdout, "References of current SNL : \n");
      csi->Get_Array_References()->Print(stdout);
    }

    Update_Cache_Contents(cc, csi->Get_Array_References(), parallel_loop);

    if (cross_loop_debug_level >= 3) {
      fprintf(stdout, "CACHE_CONTENTS after updating : \n");
      cc->Print(stdout);
    }

    cc->Compact_Cache();

    if (cross_loop_debug_level >= 3) {
      fprintf(stdout, "CACHE_CONTENTS after compacting : \n");
      cc->Print(stdout);
    }

    if (cost > curr_min) {
      break;
    }
  }

  if (cross_loop_debug_level >= 1) {
    if (cost > curr_min) {
      fprintf(stdout, "Cost of path >= %lf\n", cost);
    } else {
      fprintf(stdout, "Cost of path : %lf\n", cost);
    }
  }

  CXX_DELETE(cc, &LNO_local_pool);
  return cost;
}


// evaluate all the parallel paths in the execution
static void Evaluate_Parallel_Paths(SNL_STREAM *sst)
{
  double min = DBL_MAX;
  for (sst->Stream_Init(FALSE); !sst->Stream_Over(); sst->Stream_Next()) {
    INT32* curr_path = sst->Stream_Curr();
    double cost = Explore_Path(sst, curr_path, min);
    if (cost < min) {
      min = cost;
      sst->Set_Min_Path(min);
    }
  }
}


// determine the minimum path given the stream
static void Stream_Analysis(SNL_STREAM *sst)
{
  INT i;
  for (i = 0; i < sst->Num_SNL(); ++i) {
    CROSS_SNL_INFO *csi = sst->Get_SNL(i);

    
    // go through the SNL and pick up the set of parallel options
    double seq_mc_cost;
    double seq_cc_cost;
    SNL_Parallelization_Costs(csi->SNL_Root(), csi->SNL_Depth(),
			      csi->Parallel_Options(),& seq_cc_cost, & seq_mc_cost);
    csi->Set_Seq_Machine_Cost(seq_mc_cost);
    csi->Set_Seq_Cache_Cost(seq_cc_cost);

    ARA_LOOP_INFO *ara_root =
      CXX_NEW(ARA_LOOP_INFO(csi->SNL_Root(), NULL, TRUE), &ARA_memory_pool);  
    DO_LOOP_INFO* dli = Get_Do_Loop_Info(csi->SNL_Root());
    ARA_Initialize_Loops(csi->SNL_Root(), ara_root);

    ARRAY_SNL_INFO *asi = CXX_NEW(ARRAY_SNL_INFO(csi->SNL_Root(), csi->SNL_Depth(), ara_root),
				&LNO_local_pool);
    
    /* Create the array references read/ written in this SNL */
    Gather_Array_References(asi);
    csi->Set_Array_References(asi);

    if (cross_loop_debug_level >= 1) {
      PARALLEL_INFO_ST *pist = csi->Parallel_Options();

      fprintf(stdout, "SNL : %d\n", i);
      for (INT j = 0; j < pist->Elements(); ++j) {
	pist->Bottom_nth(j)->Print(stdout);
      }

      if  (cross_loop_debug_level >= 2) {
	asi->Print(stdout);
      }
    }

    // remove all but the outermore parallel loops
    csi->Weed_Out_Inner();
    csi->Sort_Parallel_Options();
  }
   
  // determine the best combination of parallelism
  Evaluate_Parallel_Paths(sst);

  if (cross_loop_debug_level >= 1) {
    sst->Print(stdout);
  }

  for (i = 0; i < sst->Num_SNL(); ++i) {
    CROSS_SNL_INFO *csi = sst->Get_SNL(i);
    ARA_Cleanup(csi->SNL_Root());
  }
}

/*
 * Get the parent loop of a given loop
 * NULL if outermost loop
 */
WN * Get_Parent_Loop(WN *wn)
{
  if (wn) {
    WN * parent = LWN_Get_Parent(wn);
    if (parent == NULL) {
      return NULL;
    }
    if (WN_opcode(parent) == OPC_DO_LOOP) {
      return parent;
    } else {
      return Get_Parent_Loop(parent);
    }
  }
  return NULL;
}

/*
 *
 */
extern void Cross_Loop_Cache_Analysis(PU_Info* current_pu, WN* func_nd)
{
  cross_loop_debug_level = Get_Trace(TP_LNOPT2, TT_CROSS_LOOP);

  MEM_POOL_Push(&LNO_local_pool);

  if (cross_loop_debug_level >= 1) {
    fprintf(stdout, "### Cross Loop Analysis (Begin)\n");
  }

  FIZ_FUSE_INFO* ffi=
    CXX_NEW(FIZ_FUSE_INFO(&LNO_local_pool), &LNO_local_pool);
  ffi->Build(func_nd, TRUE);


  // go through the SNLs and identify sibling SNLs
  // all the siblings are stored in a list associated with the parent loop. 
  // TODO : not all sibling SNLs can really be considered together :
  //        need to ensure no other constructs intervene

  SNL_STREAM_ST snl_streams(&LNO_local_pool);
  WN_MAP snl_map = WN_MAP_Create(&LNO_local_pool);

  INT i;
  for (i = 0; i < ffi->Num_Snl(); i++) {

    if (ffi->Get_Type(i) == Invalid || ffi->Get_Type(i) == Non_SNL)
      continue; 
   
    if (cross_loop_debug_level >= 1) {
      fprintf(stdout, "SNL : %d\n", i);
    }

    WN* wn_outer_loop = ffi->Get_Wn(i);     
    INT nloops = ffi->Get_Depth(i); 
    WN* parent = Get_Parent_Loop(wn_outer_loop);
    CROSS_SNL_INFO *csi = CXX_NEW(CROSS_SNL_INFO(wn_outer_loop, nloops), &LNO_local_pool);
    WN * key = (parent == NULL) ? func_nd : parent;
    SNL_STREAM *sst = (SNL_STREAM *) WN_MAP_Get(snl_map, key);
    if (sst != NULL) {
	sst->Add_SNL(csi);
    } else {
      SNL_STREAM * sst = CXX_NEW(SNL_STREAM(parent), & LNO_local_pool);
      sst->Add_SNL(csi);
      WN_MAP_Set(snl_map, key, sst);
      snl_streams.Push(sst);
    }
  }

  for (i = 0; i < snl_streams.Elements(); ++i) {
    SNL_STREAM *sst = snl_streams.Bottom_nth(i);
    Stream_Analysis(sst);
    sst->Cleanup();
  }
  
  MEM_POOL_Pop(&LNO_local_pool);

  if (cross_loop_debug_level >= 1) {
    fprintf(TFile,  "### Cross Loop Analysis (End)\n");
  } 
}


// ---------------------------------------------------------------------------

CROSS_SNL_INFO::CROSS_SNL_INFO(WN* parent, INT32 nloops) 
  : _pist(&LNO_local_pool)
{
  _snl_root = parent;
  _depth = nloops;
  _asi = NULL;
}

// weed out all options except for the nmin minimum ones
void CROSS_SNL_INFO::Weed_Out_Minimum(INT nmin)
{
  if (nmin > _pist.Elements())
    return;

  // we need to sort the elements in pist
  // we will use an inefficient sort - we do not
  // expect very many entries
  PARALLEL_INFO_ST tmp_pist(&LNO_local_pool);


  while (tmp_pist.Elements() != nmin) {
    double min_cost = DBL_MAX;
    PARALLEL_INFO *min_pi = NULL;

    for (INT i = 0; i < _pist.Elements(); ++i) {
      PARALLEL_INFO *pi = _pist.Bottom_nth(i);
   
      //  if it is alreeady in tmp_pist then skip this element
      BOOL found = false;
      for (INT j = 0; j < tmp_pist.Elements(); ++j ) {
	if (tmp_pist.Bottom_nth(j) == pi) {
	  found = TRUE;
	  break;
	}
      }

      if (found) {
	continue;
      }

      if (pi->Cost() < min_cost) {
	min_pi = pi;
	min_cost = pi->Cost();
      }
    }

    FmtAssert(min_pi != NULL, ("Could  not find the minimum costs"));
    tmp_pist.Push(min_pi);
  }

  _pist.Clear();

  // refill
  for (INT j = 0; j < tmp_pist.Elements(); ++j) {
    _pist.Push(tmp_pist.Bottom_nth(j));
  }
}


// weed out all options except for the nmin minimum ones
void CROSS_SNL_INFO::Sort_Parallel_Options(void)
{
  INT nmin = _pist.Elements();

  // we need to sort the elements in pist
  // we will use an inefficient sort - we do not
  // expect very many entries
  PARALLEL_INFO_ST tmp_pist(&LNO_local_pool);


  while (tmp_pist.Elements() != nmin) {
    double min_cost = DBL_MAX;
    PARALLEL_INFO *min_pi = NULL;

    for (INT i = 0; i < _pist.Elements(); ++i) {
      PARALLEL_INFO *pi = _pist.Bottom_nth(i);
   
      //  if it is already in tmp_pist then skip this element
      BOOL found = false;
      for (INT j = 0; j < tmp_pist.Elements(); ++j ) {
	if (tmp_pist.Bottom_nth(j) == pi) {
	  found = TRUE;
	  break;
	}
      }

      if (found) {
	continue;
      }

      if (pi->Cost() < min_cost) {
	min_pi = pi;
	min_cost = pi->Cost();
      }
    }

    FmtAssert(min_pi != NULL, ("Could  not find the minimum costs"));
    tmp_pist.Push(min_pi);
  }

  _pist.Clear();

  // refill
  for (INT j = 0; j < tmp_pist.Elements(); ++j) {
    _pist.Push(tmp_pist.Bottom_nth(nmin-j-1));
  }
}

// weed everything out but the outer-most parallel loops
void CROSS_SNL_INFO::Weed_Out_Inner(void)
{
  PARALLEL_INFO_ST tmp_pist(&LNO_local_pool);
  
  // for each loop make sure that only the permutation
  // with that loop outermost survives this stage
  for (INT i = 0; i < _depth; ++i) {
    PARALLEL_INFO *min_pi = NULL;
    INT min_depth = INT_MAX;

    for (INT j = 0; j < _pist.Elements(); ++j) {
      PARALLEL_INFO *pi = _pist.Bottom_nth(j);
      if (pi->Parallel_Loop() != i) {
	continue;
      }

      if (pi->Parallel_Depth() <= min_depth) {
	if (pi->Parallel_Depth() == min_depth && pi->Cost() > min_pi->Cost()) {
	  continue;
	}
	min_pi = pi;
	min_depth = pi->Parallel_Depth();
      }
    }

    if (min_pi != NULL) {
      tmp_pist.Push(min_pi);
    }
  }

   // empty
  _pist.Clear();
 
  // refill
  for (INT j = 0; j < tmp_pist.Elements(); ++j) {
    _pist.Push(tmp_pist.Bottom_nth(j));
  }
}

void CROSS_SNL_INFO::Print(FILE *f)
{
  fprintf(f, "number of loops (_depth) = %d\n", _depth);
  fprintf(f, "sequential machine cost = %lf\n", _seq_mch_cost);
  fprintf(f, "sequential cache cost = %lf\n", _seq_cache_cost);

  for (INT i = 0; i < _pist.Elements(); ++i) {
    fprintf(f, "Parallel option %d : \n", i);
    _pist.Bottom_nth(i)->Print(f);
  }

  fprintf(f, "\n_snl_root = %p\n _asi = %p\n", _snl_root, _asi);
}

// ----------------------------------------------------------------------------

SNL_STREAM::SNL_STREAM(WN *parent) 
  : _st(&LNO_local_pool)
{
  _parent = parent;
  _ara_info = CXX_NEW(ARA_LOOP_INFO(parent, NULL, TRUE), &LNO_local_pool);

  _parallel_only = TRUE;
  _options = NULL;
  _min_path = NULL;
  _finished = TRUE;
  _min_cost = DBL_MAX;
}

SNL_STREAM::~SNL_STREAM(void)
{
  Cleanup();

  if (_options) {
    CXX_DELETE_ARRAY(_options, &LNO_local_pool);
  }
  if (_min_path) {
    CXX_DELETE_ARRAY(_min_path, &LNO_local_pool);
  }
}


void SNL_STREAM::Print(FILE *f)
{
  fprintf(f, "_parent = %p\n _ara_info = %p\n", _parent, _ara_info);

  if (_min_path) {
    fprintf(f, "Minimum Cost : %lf\n", _min_cost);
    
    fprintf (f, "Minimum Path : [");

    INT i;
    for (i = 0; i < _st.Elements(); ++i) {
      fprintf(f, " %d", _min_path[i]);
    }
    fprintf(f, "]\n");
  
    for (i = 0; i < _st.Elements(); ++i) {
      fprintf(f, "%d :\n", i);
      if (_min_path[i] != -1) {
	_st.Bottom_nth(i)->Get_Parallel_Option(_min_path[i])->Print(f);
      } else {
	fprintf(f, "Sequential\n");
      }
    }
  }
}

void SNL_STREAM::Cleanup(void)
{
  if (_ara_info) {
    CXX_DELETE(_ara_info, &LNO_local_pool);
    _ara_info = NULL;
  }
  if (_parent != NULL) {
    DO_LOOP_INFO *dli = Get_Do_Loop_Info(_parent);
    dli->ARA_Info = NULL;
    _parent = NULL;
  }
}


void SNL_STREAM::Stream_Init(BOOL parallel_only)
{
  _parallel_only = parallel_only;

  if (_options == NULL) {
    _options = CXX_NEW_ARRAY(INT32, _st.Elements(), &LNO_local_pool);
  } 

  for (INT i = 0; i < _st.Elements(); ++i) {
    CROSS_SNL_INFO *csi = _st.Bottom_nth(i);
    _options[i] = csi->Num_Parallel_Options() - 1;
  }

  _finished = FALSE;
}

// return the current sequence of parallel options
INT32 *SNL_STREAM::Stream_Curr(void) 
{
  FmtAssert(_options != NULL && !_finished, ("Illegal SNL_Stream operation : curr"));

  return _options;
}

// set the current sequence to be the min one
void SNL_STREAM::Set_Min_Path(double cost) 
{
  if (_min_path == NULL) {
    _min_path = CXX_NEW_ARRAY(INT32, _st.Elements(), &LNO_local_pool); 
  }

  for (INT i = 0; i < _st.Elements(); ++i) {
    _min_path[i] = _options[i];
  }
  _min_cost = cost;
}


// return the next sequence of parallel loops in the stream
void SNL_STREAM::Stream_Next(void)
{
  FmtAssert(_options != NULL,("Illegal SNL_Stream operation : next")) ;
  
  INT i = _st.Elements() - 1;
  if (!_finished) {
    while (i >= 0) {
      if (_parallel_only) {
	// we do not want the sequential option
	if (_options[i] > 0) {
	  _options[i] = _options[i] - 1; 
	  break;
	} else {
	  _options[i] = _st.Bottom_nth(i)->Num_Parallel_Options() - 1;
	  i--;
	}
      } else {
	// also allow the sequential option
	if (_options[i] > -1) {
	  _options[i] = _options[i] - 1; 
	  break;
	} else {
	  _options[i] = _st.Bottom_nth(i)->Num_Parallel_Options() - 1;
	  i--;
	}
      }
    }
    
    if (i < 0) {
      _finished = TRUE;
    }
  }
}


BOOL SNL_STREAM::Stream_Over(void)
{
  return _finished;
}
    



