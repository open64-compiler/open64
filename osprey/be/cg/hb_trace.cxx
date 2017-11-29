/*
 * Copyright 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
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


#include "defs.h"
#include "config.h"
#include "mempool.h"
#include "tracing.h"
#include "timing.h"
#include "cgir.h"
#include "cg.h"
#include "cg_flags.h"
#include "ttype.h"
#include "bitset.h"
#include "bb_set.h"
#include "freq.h"
#include "whirl2ops.h"
#include "dominate.h"
#include "findloops.h"
#include "cg_sched_est.h"
#include "opt_points_to.h"
#include "opt_alias_mgr.h"

#include "hb.h"
#include "hb_path.h"
#include "hb_trace.h"

FILE *HB_TFile;

/////////////////////////////////////
void
HB_Trace_Init(void)
/////////////////////////////////////
//
//  See interface description.
//
/////////////////////////////////////
{
  if (HB_Trace(HB_TRACE_TO_STDOUT)) {
    HB_TFile = stdout;
  } else {
    HB_TFile = TFile;
  }
}

/////////////////////////////////////
void
HB_PATH_Trace(HB_PATH* path)
/////////////////////////////////////
//
// See interface description.
//
/////////////////////////////////////
{
  fprintf(HB_TFile, "<HB> Path data:\n");
  fprintf(HB_TFile, "<HB>   Priority: %lf\n", HB_PATH_Priority(path));
  fprintf(HB_TFile, "<HB>   Probability: %lf\n", HB_PATH_Probability(path));
  fprintf(HB_TFile, "<HB>   Hazard Multiplier: %lf\n",
	  HB_PATH_Hazard_Multiplier(path));
  fprintf(HB_TFile, "<HB>   Number Ops: %d\n", HB_PATH_Num_Ops(path));
  fprintf(HB_TFile, "<HB>   Schedule Height: %d\n",
	  HB_PATH_Schedule_Height(path));
  BB_SET_Print(HB_PATH_Blocks(path), HB_TFile);
  fprintf(HB_TFile, "\n");
}


/////////////////////////////////////
void
HB_Trace_If_Convert_Blocks(HB* hb)
/////////////////////////////////////
//
//  See interface description.
//
/////////////////////////////////////
{
  fprintf(HB_TFile, "\n<HB> Hyperblock entry: BB%d\n", BB_id(HB_Entry(hb)));
  if (HB_Exit(hb)) {
    fprintf(HB_TFile, "<HB> Hyperblock exit: BB%d\n", BB_id(HB_Exit(hb)));
  }
  if (HB_Fall_Thru_Exit(hb)) {
    fprintf(HB_TFile, "<HB> Hyperblock fall thru exit: BB%d\n",
	    BB_id(HB_Fall_Thru_Exit(hb)));
  }
  fprintf(HB_TFile, "<HB> If-converting following blocks: ");
  BB_SET_Print(HB_Blocks(hb), HB_TFile);
  fprintf(HB_TFile, "\n");
}

/////////////////////////////////////
void
HB_Trace_HB_List()
/////////////////////////////////////
//
//  See interface description.
//
/////////////////////////////////////
{
  std::list<HB*>::iterator hbi;

  fprintf(HB_TFile, "\n<HB> Current hyperblocks: \n");
  for (hbi = HB_list.begin(); hbi != HB_list.end(); hbi++) {
    fprintf(HB_TFile, "  ");
    (*hbi)->Print();
    fprintf(HB_TFile, "\n");
  }
}

//
// Print a hyperblock's info
//

void HB::Print(void)
{
  if (blocks) {
    BB_SET_Print(blocks, HB_TFile);
  } else {
    fprintf(HB_TFile,"{}");
  }
    
  fprintf(HB_TFile," %d/",BB_id(entry));
  if (exit) {
    fprintf(HB_TFile,"%d",BB_id(exit));
  } 
  if (fall_thru_exit) {
    fprintf(HB_TFile,"/%d",BB_id(fall_thru_exit));
  }
  
  fprintf(HB_TFile, "\n");
}

// Print spaces on the HB_TFile

static void Indent(INT indent)
{
  INT i;
  for (i=0; i < indent*3; i++) {
    putc(' ',HB_TFile);
  }
}

/////////////////////////////////////
void
HB_Trace_Print_Cand_Tree(HB_CAND_TREE* c, INT indent)
/////////////////////////////////////
//
//  See interface description.
//
/////////////////////////////////////
{
  if (!c) return;

  HB * hb = HB_CAND_TREE_Candidate(c);
  Indent(indent);
  if (HB_CAND_TREE_Check_Flag(c,HCT_ERASE)) {
    fprintf(HB_TFile, "E");
  }
  hb->Print();
  if (!HB_CAND_TREE_Kids(c).empty()) {
    std::list<HB_CAND_TREE*>::iterator k;
    for (k = HB_CAND_TREE_Kids(c).begin(); k != HB_CAND_TREE_Kids(c).end();
	 k++) {
      if (BB_SET_ContainsP(HB_Blocks(HB_CAND_TREE_Candidate(c)),
			   HB_Blocks(HB_CAND_TREE_Candidate(*k)))) {
	HB_Trace_Print_Cand_Tree(*k, indent+1);
      } else {
	Indent(indent+1);
	fprintf(HB_TFile, "Bad subtree ");
	BB_SET_Print(HB_Blocks(HB_CAND_TREE_Candidate(*k)),HB_TFile);
	fprintf(HB_TFile, "\n");
	HB_Trace_Print_Cand_Tree(*k, indent+1);
      }	
    }
  }
}

/////////////////////////////////////
void
HB_Trace_Candidates(const char* tstring, std::list<HB_CAND_TREE*>& cands)
/////////////////////////////////////
//
//  See interface description.
//
/////////////////////////////////////
{
  std::list<HB_CAND_TREE*>::iterator c;

  fprintf(HB_TFile, "\n<HB> Candidates after %s: \n", tstring);
  for (c = cands.begin(); c != cands.end(); c++) {
    HB_Trace_Print_Cand_Tree(*c,0);
    fprintf(HB_TFile, "\n");
  }
}


void dump_cand_tree(HB_CAND_TREE* c)
{
  FILE *f;
  f = HB_TFile;
  HB_TFile = stdout;
  HB_Trace_Print_Cand_Tree(c,0);
  HB_TFile = f;
}
  
void dump_cand_trees(std::list<HB_CAND_TREE*> &cands)
{
  std::list<HB_CAND_TREE*>::iterator c;
  printf("-----------------------\n");
  for (c = cands.begin(); c != cands.end(); c++) {
    dump_cand_tree(*c);
    printf("\n");
  }
}

void dump_hb(HB *hb)
{
  FILE *f;
  f = HB_TFile;
  HB_TFile = stdout;
  hb->Print();
  printf("block list { ");
  std::list<BB*>::iterator bbi;
  FOR_ALL_BB_STLLIST_ITEMS_FWD(hb->block_list, bbi) {
    printf("%d ",BB_id(*bbi));
  }
  printf("}\n");
  HB_TFile = f;
}


