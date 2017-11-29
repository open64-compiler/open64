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


//-*-c++-*-
//============================================================================
//
// Module: region_init.cxx
// $Revision: 1.9 $
// $Date: 05/12/05 08:59:31-08:00 $
// $Author: bos@eng-24.pathscale.com $
// $Source: /scratch/mee/2.4-65/kpro64-pending/be/region/SCCS/s.region_init.cxx $
//
// Revision history:
//  27-FEB-95 dahl - Original Version
//
// Description:
//   Find all regions in a PU and create RIDs for them.
//   Identify all region exits and convert them to OPC_REGION_EXIT.
//   Emit warnings about goto's that enter the middle of a region.
//   Create region exit blocks.
//
// Algorithm:
//   I) for each region, construct 3 sets:
//	1) head_list: label that is the first statement in a region
//	2) label_list: labels that are not the first statement in the region.
//	3) goto_list: gotos, region exits, truebr, falsebr
//   II) at each region (bottom up):
//	1) cancel out any gotos/region_exits/truebr/falsebr where the
//		corresponding label is in the head_list or label_list.
//	2) any leftover region exits in the goto list are errors
//	3) promote leftover gotos/truebr/falsebr to next higher level
//		as region_exits, convert to regions exits in code also
//	4) promote head labels to next higher level label_list
//
// Propagate option pragma strings to region pragma blocks.
// Note: options are inherited.
// example:
//	PU (-A)		--> command line + -A
//	 RGN 1 (-B)	--> command line + -A + -B
//	  RGN 2 (-C)	--> command line + -A + -B + -C
//	 RGN 3 (-D)	--> command line + -A + -B + -D
//
//============================================================================

#define region_init_CXX	"region_init.cxx"
#ifdef _KEEP_RCS_ID
static char *rcs_id = region_init_CXX"$Revision: 1.9 $";
#endif /* _KEEP_RCS_ID */

#include "wn.h"			// WN type
#include "wn_util.h"		// wn accessors
#include "ir_reader.h"		// for fdump_tree
#include "region_util.h"	// RID structure
#include "tracing.h"		// TFile
#include "erglob.h"		// EC_Rgn_Ill_Entry
#include "config.h"		// Region_Skip_List
#include "cxx_memory.h"		// CXX_NEW
#include "region_whirl_templates.h"	// REGION_search_block

//============================================================================

class RINIT; // Forward declarations
class LABEL_ALIAS;

// class for keeping track of gotos/region_exits/truebr/falsebr
class GOTO {
private:
  WN *_wn;		// goto/region_exit/truebr/falsebr
  WN *_block;		// enclosing block for truebr/falsebr cases
  RID *_rid;		// the rid for this region
  BOOL _outside;	// TRUE: goes to outside region, FALSE: stays inside
  BOOL _io_goto;	// TRUE: IO stmt implicit goto, FALSE: regular entry
  BOOL _comp_goto;	// TRUE: part of a compgoto statment
  INT64 _linenum;	// linenum of goto
  GOTO *_next;		// next in goto_list

public:
  GOTO()			{ }
  ~GOTO()			{ }
  BOOL Trace(void)		{ return Get_Trace(TP_REGION,
					     TT_REGION_RGN_INIT_DEBUG); }
  WN *Wn(void)			{ return _wn; }
  void Set_wn(WN *wn)		{ _wn = wn; }
  void Set_block(WN *block)	{ _block = block; }
  void Set_rid(RID *rid)	{ _rid = rid; }
  void Set_linenum(INT64 line)	{ _linenum = line; }
  void Set_io_goto(BOOL io)	{ _io_goto = io; }
  void Set_comp_goto(BOOL comp) { _comp_goto = comp; }
  void Set_label_number(WN *wn) { WN_label_number(_wn) = WN_label_number(wn);
				  WN_st_idx(_wn) = WN_st_idx(wn); }
  OPCODE Opcode(void)		{ return WN_opcode(_wn); }
  // converts goto/branch to region_exit
  void Set_region_exit(RID *rid, RINIT *rinit);
  INT32 Label_number(void)	{ return WN_label_number(_wn); }
  WN *Block(void)		{ return _block; }
  RID *Rid(void)		{ return _rid; }
  INT64 Linenum(void)		{ return _linenum; }
  BOOL Outside(void)		{ return _outside; }
  BOOL Is_io_goto(void)		{ return _io_goto; }
  BOOL Is_comp_goto(void)	{ return _comp_goto; }
  void Set_inside(void)		{ _outside = FALSE; }
  void Set_outside(void)	{ _outside = TRUE; }
  GOTO *Next(void)		{ return _next; }
  void Set_next(GOTO *gtmp)	{ _next = gtmp; }
  BOOL Compare_labels(WN *label); // compare a goto label and a WN label
  BOOL Compare_labels(LABEL_IDX label); // compare a goto label and a label_idx
  BOOL Compare_labels(GOTO *label); // compare two goto labels
  void Modify_exits(RID *, RID *, WN *); // modify all exits between regions
  void fdump(FILE *fd)		{ fprintf(fd," %s %s L%d %s RGN %d\n",
				    _outside ? "" : "<cancelled>",
				    OPCODE_name(Opcode()),
				    Is_io_goto()?WN_label_number(_wn):
					  Label_number(),
				    Is_io_goto() ? "(IO exit)" : "",
				    RID_id(Rid()) ); }
};

// class for keeping track of first label in a region and what it is replaced
// with
class RGN_LABEL {
private:
  WN *_block;		// enclosing block for label
  WN *_label;		// original label
  WN *_replace;		// the label that old external gotos point to
  WN *_split;		// the label added before the region for external gotos
  RGN_LABEL *_next;
public:
  RGN_LABEL()				{ _split = NULL; } // all others set
  ~RGN_LABEL()				{ }
  WN *Block(void)		        { return _block; }
  WN *Label(void)			{ return _label; }
  WN *Replace_label(void)		{ return _replace; }
  WN *Split_label(void)			{ return _split; }
  RGN_LABEL *Next(void)			{ return _next; }
  void Set_block(WN *block)	        { _block = block; }
  void Set_label(WN *label)		{ _label = label; }
  void Set_replace_label(WN *replace)	{ _replace = replace; }
  void Set_split_label(WN *split)	{ _split = split; }
  void Set_next(RGN_LABEL *next)	{ _next = next; }
  void Print(const char *str);
};

// For label aliases, only need to know the label number so this is used
// to save some space.
class LITE_LABEL {
private:
  INT32  _label;
  ST_IDX _st;
  LITE_LABEL *_next;
public:
  LITE_LABEL()				{ _next = NULL; }
  ~LITE_LABEL()				{ }
  INT32 Label(void)			{ return _label; }
  ST_IDX St(void)			{ return _st; }
  LITE_LABEL *Next(void)		{ return _next; }
  void Set_label(INT32 label)		{ _label = label; }
  void Set_st(ST_IDX st)		{ _st = st; }
  void Set_next(LITE_LABEL *next)	{ _next = next; }
};

// For keeping track of multiple region exits that goto the same
// label. We create new labels for the region exits so there are no duplicates
// in the exit list. This class keeps track of each label and its aliases.
class LABEL_ALIAS {
private:
  INT32 _key_label_no;		// original label, search key
  LITE_LABEL *_alias_list;	// aliased labels
  LITE_LABEL *_last;		// last entry in alias_list
  LABEL_ALIAS *_next;		// linked list of all original labels 
public:
  LABEL_ALIAS()				{ _key_label_no = -1;
					  _alias_list = NULL;
					  _last = NULL;
					  _next = NULL;
					}
  ~LABEL_ALIAS()			{ }
  LABEL_ALIAS *Next(void)		{ return _next; }
  INT32 Key(void)			{ return _key_label_no; }
  LITE_LABEL *Alias_list(void)		{ return _alias_list; }
  LITE_LABEL *Last(void)		{ return _last; }
  void Set_next(LABEL_ALIAS *next)	{ _next = next; }
  void Set_key(INT32 key)		{ _key_label_no = key; }
  void Set_alias_list(LITE_LABEL *ll)	{ _alias_list = ll; }
  void Set_last(LITE_LABEL *ll)		{ _last = ll; }
  void Add_label_alias(INT32, ST_IDX, MEM_POOL *);
  void Merge_label_alias(LABEL_ALIAS *, MEM_POOL *);
  void Print(FILE *fd);
};


// class for keeping track of the labels and gotos for each region
class RINIT {

private:
  BOOL _trace_flag;	// for tracing the recursion
  MEM_POOL *_mem_pool;	// local mem_pool
  RID *_rid;		// so we know which one we are processing
  INT32 _nregions;	// number of non-MP regions processed
  INT32 _nexits;	// number of exits from the region
  RGN_LABEL *_head_list;// list of labels at very top of region (entry point)
  RGN_LABEL *_label_list;// list of labels that appear lower in the region
  GOTO *_goto_list;// list of gotos, T/F branches, and region exits
  LABEL_ALIAS *_label_alias; // list of labels and their aliases

  void Process_region(WN *, WN *, INT32, RID *, char *);

public:
  RINIT(RID *, MEM_POOL *);
  ~RINIT()			{ }
  BOOL Trace(void)		{ return _trace_flag; }
  MEM_POOL *Mem_pool(void)	{ return _mem_pool; }
  INT32 Nregions(void)		{ return _nregions; }
  INT32 Nexits(void)		{ return _nexits; }
  RGN_LABEL *Head_list(void)	{ return _head_list; }
  RGN_LABEL *Label_list(void)	{ return _label_list; }
  GOTO *Goto_list(void)		{ return _goto_list; }
  LABEL_ALIAS *Label_alias(void){ return _label_alias; }
  RID *Rid(void)		{ return _rid; }
  void Set_head_list(RGN_LABEL *head)   { _head_list = head; }
  void Set_label_list(RGN_LABEL *label) { _label_list = label; }
  void Set_goto_list(GOTO *goto_) { _goto_list = goto_; }
  void Set_label_alias(LABEL_ALIAS *la) { _label_alias = la; }
  void Add_goto(WN *, WN *, BOOL, BOOL);//add a goto/region_exit/truebr/falsebr
  void Concat_goto(GOTO *);             // concatenate two goto lists
  RGN_LABEL *Add_label(WN *, WN *);	// labels mention in a region,
                                        // not first stmt 
  RGN_LABEL *Add_head_label(WN *, WN *);// first statement is a label
  void Print_sets(void);	        // print sets
  GOTO *Cancel_internal_gotos(BOOL);    // find gotos with labels in same rgn
  void Handle_split_label(WN *, WN *, WN *, RINIT *);// label at begin of rgn
  void Region_init(WN *, INT32, RID *, char *);
  void Add_label_alias(WN *, WN *);     // label aliases
  void Expand_label_alias(RGN_LABEL *); // if label is aliased, expand it
  void Merge_label_alias(LABEL_ALIAS *);// merge two label alias lists
};

//============================================================================

// modify all exits lists from one rid to another
// _wn is old region exit
void
GOTO::Modify_exits(RID *child_rid, RID *parent_rid, WN *new_wn)
{
  Is_True(child_rid != NULL, ("GOTO::Modify_exits, child_rid is NULL"));
  Is_True(parent_rid != NULL, ("GOTO::Modify_exits, parent_rid is NULL"));
  Is_True(child_rid != parent_rid, ("GOTO::Modify_exits, RIDs are the same"));
  Is_True(new_wn != NULL, ("GOTO::Modify_exits, NULL new_wn"));
  Is_True(_wn != NULL, ("GOTO::Modify_exits, NULL _wn"));

  // start at child rid and work up to parent rid
  // exits are assumed to be unique for regions already processed
  for (RID *rtmp = child_rid; rtmp != parent_rid; rtmp = RID_parent(rtmp)) {
    // search exit list and find exit for _wn
    Is_True(RID_rwn(rtmp) != NULL,
	    ("GOTO::Modify_exits, RID WN pointer is wrong"));
    Is_True(WN_region_exits(RID_rwn(rtmp)) != NULL,
	    ("GOTO::Modify_exits, exits list is wrong"));
    WN *exit_block = WN_region_exits(RID_rwn(rtmp));
    Is_True(exit_block != NULL && WN_opcode(exit_block) == OPC_BLOCK,
	    ("GOTO::Modify_exits, exit_block is wrong"));

    // delete old one (we can't use WN_DELETE_FromBlock on _wn because
    // the WN for the exit list is different from the WN in the code,
    // _wn is the WN in the code).
    WN *wtmp = REGION_search_block(exit_block, 
				   comp_same_label_no(WN_label_number(_wn)));
    Is_True(wtmp != NULL, ("GOTO::Modify_exits, exit not found in exit list"));
    WN_DELETE_FromBlock(exit_block, wtmp);

    if (WN_operator(new_wn) == OPR_LABEL) {
	// want region_exit in the exit block.
	// this happens via split_labels.
        new_wn = WN_CreateRegionExit (WN_label_number(new_wn));
    }
    Is_Trace(Trace(),(TFile,
	    "  GOTO::Modify_Exits, exit L%d replaced by L%d in RGN %d\n",
	    WN_label_number(_wn), WN_label_number(new_wn), RID_id(rtmp)));
    // add new exit to exit list
    WN_INSERT_BlockLast(exit_block, WN_CopyNode(new_wn));
  }
}

// add a region exit, handles duplicates in the exit list
void
GOTO::Set_region_exit(RID *rid, RINIT *rinit)
{
  Is_True(rid != NULL, ("GOTO::Set_region_exit, NULL rid"));
  Is_True(RID_rwn(rid) != NULL, ("GOTO::Set_region_exit, NULL rwn"));
  Is_True(!Is_comp_goto(),
    ("GOTO::Set_region_exit, a COMPGOTO must have targets inside the "
     "region, RID %d", RID_id(rid)));

  switch (WN_opcode(_wn)) {
    case OPC_TRUEBR:		// need to rewrite to region exit
    case OPC_FALSEBR:
    {
      // create a new label
      LABEL_IDX label_no;
      New_LABEL(CURRENT_SYMTAB, label_no);
      WN *wn_label = WN_CreateLabel (label_no, 0, NULL);
      WN_Set_Linenum(wn_label, WN_Get_Linenum(_wn));
      // create a region_exit to original label
      WN *wn_rgn_exit = WN_CreateRegionExit (WN_label_number(_wn));
      WN_Set_Linenum(wn_rgn_exit, WN_Get_Linenum(_wn));
      Is_Trace(Trace(),(TFile,
			"===== Set_region_exit(T/Fbr), converting %s L%d to\n",
			OPCODE_name(WN_opcode(_wn)), WN_label_number(_wn)));
      // make the opposite branch goto the new label
      if (WN_opcode(_wn) == OPC_TRUEBR)
	WN_set_opcode(_wn, OPC_FALSEBR);
      else
	WN_set_opcode(_wn, OPC_TRUEBR);
      WN_label_number(_wn) = label_no;
      // insert the new code into the block
      WN_INSERT_BlockAfter(_block, _wn, wn_rgn_exit);
      WN_INSERT_BlockAfter(_block, wn_rgn_exit, wn_label);
      Is_Trace(Trace(),(TFile,"\t%s L%d, %s L%d, L%d:\n",
	  OPCODE_name(WN_opcode(_wn)), label_no,
	  OPCODE_name(WN_opcode(wn_rgn_exit)), WN_label_number(wn_rgn_exit),
	  label_no));
      _wn = wn_rgn_exit; // _wn is the new region_exit
    }
      break;

    case OPC_GOTO:		// convert directly to region exit
      Is_Trace(Trace(),(TFile,
		     "===== Set_region_exit(goto), converting %s L%d to %s\n",
			OPCODE_name(Opcode()), Label_number(),
			OPCODE_name(OPC_REGION_EXIT)));
      WN_set_opcode(_wn, OPC_REGION_EXIT);
      break;

    case OPC_U8LDA: // PPP this should be fixed to use operators
    case OPC_U4LDA:
    {
      Is_True(Is_io_goto(),("GOTO::Set_region_exit, error in IO stmt"));

      // for IO implicit gotos, patch the IO stmt to goto a new label at
      // the end of the region. At that label is the region exit to the
      // original label.
      LABEL_IDX new_label_no;
      LABEL_IDX old_label_no = WN_label_number(_wn);
      New_LABEL(CURRENT_SYMTAB, new_label_no);
      WN *new_label_wn = WN_CreateLabel (new_label_no, 0, NULL);

      Is_Trace(Trace(),(TFile,
       "===== Set_region_exit, IO exit, converting %s L%d to %s L%d, RGN %d\n",
			OPCODE_name(Opcode()), old_label_no,
			OPCODE_name(Opcode()), new_label_no,
			RID_id(rid)));

      WN_Set_Linenum(new_label_wn, WN_Get_Linenum(_wn));
      WN_label_number(_wn) = new_label_no;
      WN *rgn_exit_wn = WN_CreateRegionExit (old_label_no);
      WN_Set_Linenum(rgn_exit_wn, WN_Get_Linenum(_wn));
      WN_INSERT_BlockLast(Block(), new_label_wn);
      WN_INSERT_BlockLast(Block(), rgn_exit_wn);

      // add exit to region exit list (at bottom of this routine)
      _wn = rgn_exit_wn;
    }
      break;

    case OPC_REGION_EXIT:	// do nothing
      // for a fall-thru, we did not put it in the exit list so we can here
      break;
    default:
      FmtAssert(0,("GOTO::Set_region_exit, unknown region exit: %s",
		   OPCODE_name(WN_opcode(_wn))));
      break;
  }

  // for convenience
  WN *exit_block = WN_region_exits(RID_rwn(rid));
  Is_True(exit_block && WN_opcode(exit_block) == OPC_BLOCK,
	  ("GOTO::Set_region_exit, exit block is not a block"));

  // search for duplicate in exit list and rename if necessary
  if (REGION_search_block(exit_block,
			  comp_same_label_no(WN_label_number(_wn)))) {

    // create a new label for the region exit
    LABEL_IDX new_label_no;
    New_LABEL(CURRENT_SYMTAB, new_label_no);
    Is_Trace(Trace(), (TFile,"  GOTO::Set_region_exit, avoiding duplicate: "
		      "L%d --> L%d\n",WN_label_number(_wn), new_label_no));
    WN *rgn_exit_wn = WN_CreateRegionExit (new_label_no);
    // enter in label alias table
    rinit->Add_label_alias(_wn, rgn_exit_wn);

    // if rgn_exit_wn was inside a previously (nested) processed region,
    // we need to update its exit list and all the regions between
    if (Rid() != rid)
      Modify_exits(Rid(), rid, rgn_exit_wn);

    // delete old region exit and use new one
    WN_INSERT_BlockAfter(Block(), _wn, rgn_exit_wn);
    WN_DELETE_FromBlock(Block(), _wn);

    // change goto itself
    Is_Trace(Trace(),(TFile,"  GOTO::Set_region_exit, changed L%d to L%d\n",
		      WN_label_number(_wn),WN_label_number(rgn_exit_wn)));
    _wn = rgn_exit_wn;
  }

  // check for duplicates
  Is_True(!REGION_search_block(exit_block,
			       comp_same_label_no(WN_label_number(_wn))),
	  ("GOTO::Set_region_exit, duplicate exit in RGN %d exit list",
	   RID_id(rid)));

  // insert region exit into exits block for region, no duplicates
  WN_INSERT_BlockLast(exit_block, WN_CopyNode(_wn));
}

// compares the goto label to the WN label
BOOL
GOTO::Compare_labels(WN *label)
{
  if (Is_io_goto())
    return WN_label_number(Wn()) == WN_label_number(label);
  else
    return Label_number() == WN_label_number(label);
}

// compares the goto label to the ST for a label
BOOL
GOTO::Compare_labels(LABEL_IDX label)
{
  if (Is_io_goto())
    return WN_label_number(Wn()) == label;
  else
    return Label_number() == label;
}

// compares two goto labels
BOOL
GOTO::Compare_labels(GOTO *label)
{
  INT32 l1 = (Is_io_goto()) ?	WN_label_number(Wn()) :
  				Label_number();
  INT32 l2 = (label->Is_io_goto()) ? WN_label_number(label->Wn()) :
  				label->Label_number();
  return l1 == l2;
}

//============================================================================

// add a entry to a alias label list
void
LABEL_ALIAS::Add_label_alias(INT32 new_label,
                             ST_IDX new_st,
                             MEM_POOL *mem_pool)
{
  LITE_LABEL *ltmp;
#ifdef Is_True_On
  // search current aliases and make sure there are no duplicates
  for (ltmp = Alias_list(); ltmp; ltmp = ltmp->Next()) {
    Is_True(ltmp->Label() != new_label,
	    ("LABEL_ALIAS::Add_label_alias, duplicate in label alias list"));
    // verify last pointer
    if (ltmp->Next() == NULL)
      Is_True(Last() == ltmp,
	      ("LABEL_ALIAS::Add_label_alias, Last pointer not set right"));
  }
#endif
  // create a new label and add to list
  ltmp = CXX_NEW(LITE_LABEL(), mem_pool);

  Is_True(new_label != 0, ("LABEL_ALIAS::Add_label_alias, invalid new_label"));
  ltmp->Set_label(new_label);

  // add to end of list
  if (Last())
    Last()->Set_next(ltmp);
  else
    Set_alias_list(ltmp);
  Set_last(ltmp);
}

// Merge two rows together and eliminate the duplicates
// O(N^2)
void
LABEL_ALIAS::Merge_label_alias(LABEL_ALIAS *la2, MEM_POOL *mem_pool)
{
  Is_True(Alias_list() != NULL && Last() != NULL,
	  ("LABEL_ALIAS::Merge_label_alias, Alias_list is NULL"));
  Is_True(la2->Alias_list() != NULL && la2->Last() != NULL,
	  ("LABEL_ALIAS::Merge_label_alias, la2->Alias_list is NULL"));

  for (LITE_LABEL *ltmp2 = la2->Alias_list(); ltmp2; ltmp2 = ltmp2->Next()) {
    BOOL duplicate = FALSE;
    for (LITE_LABEL *ltmp1 = Alias_list(); ltmp1; ltmp1 = ltmp1->Next()) {
      if (ltmp1->Label() == ltmp2->Label()) {
	duplicate = TRUE;
	break;
      }
    } // for (ltmp1 ...)
    if (!duplicate) // add the unique label to Alias_list()
      Add_label_alias(ltmp2->Label(), ltmp2->St(), mem_pool);
  } // for (ltmp2 ...)
}

void
LABEL_ALIAS::Print(FILE *fd)
{
  for (LABEL_ALIAS *ltmp = this; ltmp; ltmp = ltmp->Next()) {
    fprintf(fd,"  Key: L%d, aliases: ",ltmp->Key());
    for (LITE_LABEL *lltmp = ltmp->Alias_list(); lltmp; lltmp = lltmp->Next()){
      fprintf(fd,"L%d ",lltmp->Label());
#ifdef Is_True_On
      // verify Last pointer
      if (lltmp->Next() == NULL)
	Is_True(ltmp->Last() == lltmp,
		("LABEL_ALIAS::Print, Last pointer wrong"));
#endif
    }
    fprintf(fd,"\n");
  }
}

//============================================================================

RINIT::RINIT(RID *rid, MEM_POOL *mem_pool) :
  _rid(rid), _mem_pool(mem_pool)
{
  _trace_flag = Get_Trace(TP_REGION, TT_REGION_RGN_INIT_DEBUG);
  _nregions = 0;
  _nexits = 0;
  _head_list = NULL;
  _label_list = NULL;
  _goto_list = NULL;
  _label_alias = NULL;
}

inline void
RINIT::Add_goto(WN *wn, WN *block, BOOL io, BOOL comp)
{
  GOTO *gnew = CXX_NEW(GOTO(), Mem_pool());

  if (io) { // IO goto
    Is_True(WN_operator(wn) == OPR_LDA,
	    ("RINIT::Add_goto, IO, unexpected opcode: %s",
	     OPCODE_name(WN_opcode(wn))));
    gnew->Set_io_goto(TRUE);	// special implicit IO goto
    gnew->Set_comp_goto(FALSE); // not a comp goto
  } else if (comp) { // comp goto
    Is_True(WN_opcode(wn) == OPC_GOTO,
	    ("RINIT::Add_goto, COMPGOTO, unexpected opcode: %s",
	     OPCODE_name(WN_opcode(wn))));
    gnew->Set_io_goto(FALSE);	// not an IO goto
    gnew->Set_comp_goto(TRUE); // is a comp goto
  } else { // regular goto
    Is_True(WN_opcode(wn) == OPC_GOTO || WN_opcode(wn) == OPC_TRUEBR ||
	    WN_opcode(wn) == OPC_FALSEBR || WN_opcode(wn) == OPC_REGION_EXIT,
	    ("RINIT::Add_goto, unexpected opcode: %s",
	     OPCODE_name(WN_opcode(wn))));
    gnew->Set_io_goto(FALSE); // regular goto list entry
    gnew->Set_comp_goto(FALSE); // regular goto list entry
  }
  Is_True(WN_opcode(block) == OPC_BLOCK,
	  ("RINIT::Add_goto, unexpected opcode: %s",
	   OPCODE_name(WN_opcode(block))));

  gnew->Set_wn(wn);	// pointer to wn in case we have to change it later
  gnew->Set_block(block);// pointer to enclosing block for truebr/falsebr
  gnew->Set_rid(Rid()); // rid for this region
  gnew->Set_outside();	// assume the goto exits the region
  gnew->Set_linenum(WN_Get_Linenum(wn));	// linenum for goto
  gnew->Set_next(_goto_list);
  _goto_list = gnew;
}

// second form, concatenate two goto lists
inline void
RINIT::Concat_goto(GOTO *gnew)
{
  if (gnew != NULL) {
    // find the end of the list
    GOTO *gtmp;
    for (gtmp=gnew; gtmp->Next() != NULL; gtmp=gtmp->Next())
      ;
    // concatenate
    gtmp->Set_next(_goto_list);
    _goto_list = gnew;
  }
}

// add a single label to the label list
// check that the label is not in the _head_list first
RGN_LABEL *
RINIT::Add_label(WN *wn, WN *block)
{
  RGN_LABEL *htmp;
  Is_True(WN_opcode(wn) == OPC_LABEL,
	  ("RINIT::Add_label, unexpected opcode: %s",
	   OPCODE_name(WN_opcode(wn))));
  for (htmp=Head_list(); htmp; htmp=htmp->Next()) {
    if (WN_label_number(htmp->Label()) == WN_label_number(wn))
      return htmp;
  }
  htmp = CXX_NEW(RGN_LABEL(), Mem_pool());
  htmp->Set_block(block);
  htmp->Set_label(wn);
  htmp->Set_replace_label(WN_CopyNode(wn)); // replacement is same for now
  htmp->Set_next(Label_list());
  Set_label_list(htmp);
  return htmp;
}

// add a single label to the head label list
RGN_LABEL *
RINIT::Add_head_label(WN *wn, WN *block)
{
  Is_True(WN_opcode(wn) == OPC_LABEL,
	  ("RINIT::Add_head_label, unexpected opcode: %s",
	   OPCODE_name(WN_opcode(wn))));
  RGN_LABEL *htmp = CXX_NEW(RGN_LABEL(), Mem_pool());
  htmp->Set_block(block);
  htmp->Set_label(WN_CopyNode(wn)); // need to copy because head label is moved
  htmp->Set_replace_label(WN_CopyNode(wn));
  htmp->Set_next(Head_list());
  Set_head_list(htmp);
  return htmp;
}

// add a label alias to the list
void
RINIT::Add_label_alias(WN *old_wn, WN *new_wn)
{
  // search and find label key
  LABEL_ALIAS *ltmp;
  BOOL found = FALSE;
  for (ltmp = Label_alias(); ltmp; ltmp = ltmp->Next()) {
    if (ltmp->Key() == WN_label_number(old_wn)) {
      found = TRUE;
      break;
    }
  }
  
  if (!found) { // not found, create new label alias list
    ltmp = CXX_NEW(LABEL_ALIAS(), Mem_pool());
    ltmp->Set_key(WN_label_number(old_wn));
    ltmp->Set_next(Label_alias());	// linked list of alias label lists
    Set_label_alias(ltmp);
  }
  ltmp->Add_label_alias(WN_label_number(new_wn), WN_st_idx(new_wn), Mem_pool());
  Is_Trace(Trace(), (TFile,"  RINIT::Add_label_alias(%s), L%d --> L%d, "
		     "RGN %d\n", found ? "found" : "new",
		     WN_label_number(old_wn), WN_label_number(new_wn),
		     RID_id(Rid())));
  Is_Trace_cmd(Trace(), Label_alias()->Print(TFile));
}

// if a label is aliased, expand it
void
RINIT::Expand_label_alias(RGN_LABEL *label)
{
  Is_True(Label_alias() != NULL, ("RINIT::Expand_label_alias, no alias list"));
  Is_True(label->Block() != NULL, ("RINIT::Expand_label_alias, NULL block"));
  Is_True(label->Label() != NULL && WN_opcode(label->Label()) == OPC_LABEL,
	  ("RINIT::Expand_label_alias, label is wrong"));

  LABEL_ALIAS *prev = NULL;
  for (LABEL_ALIAS *ltmp = Label_alias(); ltmp; ltmp = ltmp->Next()) {
    if (ltmp->Key() == WN_label_number(label->Label())) {

      Is_True(ltmp->Alias_list() != NULL && ltmp->Last() != NULL,
	      ("RINIT::Expand_label_alias, alias list is wrong"));

      // insert all the aliases for the key label after it
      LITE_LABEL *ntmp;
      for (ntmp=ltmp->Alias_list(); ntmp; ntmp=ntmp->Next()) {
	if (Trace())
	  fprintf(TFile,"RINIT::Expand_label_alias, expanding L%d to L%d for "
		  "RGN %d\n",
		  WN_label_number(label->Label()), ntmp->Label(),
		  RID_id(Rid()));
	Is_True(ntmp->Label() != 0,
		("RINIT::Expand_label_alias, incorrect LITE_LABEL"));
	WN *wtmp = WN_CreateLabel (ntmp->Label(), 0, NULL);
	WN_INSERT_BlockAfter(label->Block(), label->Label(), wtmp);

	// add to the current label list
	Add_label(wtmp, label->Block());

#ifdef Is_True_On
	// verify Last pointer
	if (ntmp->Next() == NULL)
	  Is_True(ltmp->Last() == ntmp,
		  ("RINIT::Expand_label_alias, Last pointer is wrong"));
#endif
      }

      // now delete the key so we don't reinsert later
      Is_Trace(Trace(),(TFile,
			"RINIT::Expand_label_alias, deleting list for L%d\n",
			ltmp->Key()));
      ltmp->Set_alias_list(NULL);
      ltmp->Set_last(NULL);
      if (prev == NULL)
	Set_label_alias(ltmp->Next());
      else
	prev->Set_next(ltmp->Next());

      break;
    } // if (ltmp->Key() == WN_label_number(label->Label()))
    prev = ltmp;
  } // for (LABEL_ALIAS *ltmp = Label_alias(); ltmp; ltmp = ltmp->Next())
}

// Merge two label alias lists, both are 2-dimensional
// la is probably bigger than Label_alias() so merge Label_alias() into la
// this is destructive to the lists
void
RINIT::Merge_label_alias(LABEL_ALIAS *la)
{
  // only need to merge if la contains something
  if (la) {

    LABEL_ALIAS *new_list = NULL;

    // Scan once through Label_alias and either merge a row or add to new_list.
    // Since we delete rows out of Label_alias as we go, we need a goto to
    // restart the outer loop.
start:
    LABEL_ALIAS *prev1 = NULL;
    for (LABEL_ALIAS *ltmp1 = Label_alias(); ltmp1; ltmp1 = ltmp1->Next()) {

      // look for same key in la
      BOOL merged = FALSE;
      for (LABEL_ALIAS *ltmp2 = la; ltmp2; ltmp2 = ltmp2->Next()) {
	// Merge rows if same key
	if (ltmp1->Key() == ltmp2->Key()) {
	  Is_True(ltmp1->Alias_list() != NULL && ltmp1->Last() != NULL,
		  ("RINIT::Merge_label_alias, ltmp1->Alias_list is NULL"));
	  Is_True(ltmp2->Alias_list() != NULL && ltmp2->Last() != NULL,
		  ("RINIT::Merge_label_alias, ltmp2->Alias_list is NULL"));
	  // there may be duplicates in the row, eliminate
	  ltmp2->Merge_label_alias(ltmp1, Mem_pool());
	  merged = TRUE;
	  break;
	}
      } // ltmp2 (la) loop

      // if we didn't find a match, it is a new row, add to new_list
      if (!merged) {
	// remove from Label_alias list
	if (prev1 == NULL)
	  Set_label_alias(ltmp1->Next());
	else
	  prev1->Set_next(ltmp1->Next());
	// add to new_list
	ltmp1->Set_next(new_list);
	new_list = ltmp1;
	// start over
	goto start;
      }
      prev1 = ltmp1;

    } // ltmp1 (Label_alias) loop

    // connect new_list to la list
    // find end of la list
    LABEL_ALIAS *ltmp2;
    for (ltmp2 = la; ltmp2->Next() != NULL; ltmp2 = ltmp2->Next())
      ; // empty body
    ltmp2->Set_next(new_list);

    // put la list into Label_alias() for final result
    Set_label_alias(la);
  } // if (la) 
}

void
RGN_LABEL::Print(const char *str)
{
  fprintf(TFile,"\t%s:   ",str);
  fdump_wn(TFile,Label());
  if (WN_label_number(Label()) != WN_label_number(Replace_label())) {
    fprintf(TFile,"\treplace: ");
    fdump_wn(TFile,Replace_label());
  }
  if (Split_label() != NULL) {
    fprintf(TFile,"\tsplit:   ");
    fdump_wn(TFile,Split_label());
  }
}

void
RINIT::Print_sets(void)
{
  RGN_LABEL *htmp;

  fprintf(TFile,"RGN %d\n",RID_id(Rid()));
  fprintf(TFile,"goto_list:\n");
  if (_goto_list != NULL) {
    for (GOTO *gtmp=_goto_list; gtmp; gtmp=gtmp->Next())
      gtmp->fdump(TFile);
  } else
    fprintf(TFile," <null>\n");

  fprintf(TFile,"label_list:\n");
  if (_label_list != NULL) {
    for (htmp=Label_list(); htmp; htmp=htmp->Next())
      htmp->Print("label");
  } else
    fprintf(TFile," <null>\n");

  fprintf(TFile,"head_list:\n");
  if (Head_list() != NULL) {
    for (RGN_LABEL *htmp=Head_list(); htmp; htmp=htmp->Next())
      htmp->Print("head ");
  } else
    fprintf(TFile," <null>\n");

  fprintf(TFile,"alias label list:\n");
  if (Label_alias() != NULL) {
    Label_alias()->Print(TFile);
  } else
    fprintf(TFile," <null>\n");
}

// Cancel out goto-label pairs that match within a region
// Return a list of the left-overs converted to region_exits for parent's list
// Destructive to the goto list so we delete it to avoid confusion
// If prop_up is TRUE, then propagate the uncancelled gotos up (we
// want to do this for all regions other than the root region).
GOTO *
RINIT::Cancel_internal_gotos(BOOL prop_up)
{
  GOTO *gtmp;
  RGN_LABEL *htmp;
  BOOL found;

  //==================================================================
  // First expand any labels that are aliased
  // This has to be done in it's own loop because it affects the
  // length of Label_list().
  if (Label_alias()) {
    for (htmp=Label_list(); htmp && Label_alias(); htmp=htmp->Next()) {
      // see if this label is aliased, if so, expand
      Expand_label_alias(htmp);
    }
    // might as well do the head list while we are at it
    for (htmp=Head_list(); htmp && Label_alias(); htmp=htmp->Next()) {
      // see if this label is aliased, if so, expand
      Expand_label_alias(htmp);
    }
  }

  // cancel out gotos that goto labels within the region
  for (gtmp=_goto_list; gtmp; gtmp=gtmp->Next()) {
    found = FALSE;
    if (gtmp->Outside()) { // only look at those not yet cancelled

      //==================================================================
      // cancel gotos that go to labels inside the region
      for (htmp=Label_list(); htmp; htmp=htmp->Next()) {
	Is_True(htmp->Label() != NULL,
		("RINIT::Cancel_internal_gotos, null label"));
	Is_True(htmp->Replace_label() != NULL,
		("RINIT::Cancel_internal_gotos, null replace label"));

	// compare goto and label
	if (gtmp->Compare_labels(htmp->Label())) {
	  gtmp->Set_inside(); // cancel out goto and label
	  if (Trace()) {
	    fprintf(TFile,
	     "===== RINIT::Cancel_internal_gotos: cancelled to label_list:\n");
	    gtmp->fdump(TFile);
	  }
	  found = TRUE;
	  break;
	}

	// now check replace list if different from label list
	if (WN_label_number(htmp->Label()) !=
	    WN_label_number(htmp->Replace_label()) &&
	    gtmp->Compare_labels(htmp->Replace_label())) {
	  gtmp->Set_inside(); // cancel out goto and label
	  if (Trace()) {
	    fprintf(TFile,
		"===== RINIT::Cancel_internal_gotos: cancelled to replace:\n");
	    gtmp->fdump(TFile);
	    fprintf(TFile,"  1) rewriting L%d to L%d for RGN %d\n",
		    gtmp->Label_number(), WN_label_number(htmp->Label()),
		    RID_id(gtmp->Rid()));
	  }
	  // check if goto we just changed was inside another region
	  if (gtmp->Rid() != Rid()) {
	    Is_Trace(Trace(),
		     (TFile,"  1) rewriting exit list L%d to L%d for RGN %d\n",
		      gtmp->Label_number(),
		      WN_label_number(htmp->Label()),
		      RID_id(gtmp->Rid())));
	    // need to fix the exit list, swap old exit for new one
	    gtmp->Modify_exits(gtmp->Rid(), Rid(), htmp->Label());
	  }
	  // convert goto to use label
	  gtmp->Set_label_number(htmp->Label()); // convert goto to use label
	  found = TRUE;
	  break;
	}
      } // for (htmp=Label_list(); htmp; htmp=htmp->Next())
      if (found)
	continue;

      //==================================================================
      // look through head list for labels also
      for (htmp=Head_list(); htmp; htmp=htmp->Next()) {
	Is_True(htmp->Label() != NULL,
		("RINIT::Cancel_internal_gotos, null head label"));
	Is_True(htmp->Replace_label() != NULL,
		("RINIT::Cancel_internal_gotos, null head replace label"));

	// first check head list
	if (gtmp->Compare_labels(htmp->Label())) {
	  gtmp->Set_inside(); // cancel out goto and label
	  if (Trace()) {
	    fprintf(TFile,
	      "===== RINIT::Cancel_internal_gotos: cancelled to head:\n");
	    gtmp->fdump(TFile);
	  }
	  break;
	}

	// now check replace list if different from head list
	if (WN_label_number(htmp->Label()) !=
	    WN_label_number(htmp->Replace_label()) &&
	    gtmp->Compare_labels(htmp->Replace_label())) {
	  gtmp->Set_inside(); // cancel out goto and label
	  Is_True(htmp->Split_label() != NULL,
		  ("RINIT::Cancel_internal_gotos, replace label exists (L%d) "
		   "without split label",
		   WN_label_number(htmp->Replace_label())));
	  if (Trace()) {
	    fprintf(TFile,"===== RINIT::Cancel_internal_gotos: cancelled "
		    "to head replace:\n");
	    gtmp->fdump(TFile);
	    fprintf(TFile,"  2) rewriting L%d to L%d for RGN %d\n",
		    gtmp->Label_number(), WN_label_number(htmp->Split_label()),
		    RID_id(gtmp->Rid()));
	  }
	  // check if goto we just changed was inside another region
	  if (gtmp->Rid() != Rid()) {
	    Is_Trace(Trace(),
		     (TFile,"  2) rewriting exit list L%d to L%d for RGN %d\n",
		      gtmp->Label_number(),
		      WN_label_number(htmp->Split_label()),
		      RID_id(gtmp->Rid())));
	    // need to fix the exit list, swap old exit for new one
	    gtmp->Modify_exits(gtmp->Rid(), Rid(), htmp->Split_label());
	  }
	  // convert goto to use split label
	  gtmp->Set_label_number(htmp->Split_label());
	  break;
	}
      } // for (htmp=Head_list(); htmp; htmp=htmp->Next())

    } // if (gtmp->Outside())
  } // for (gtmp=_goto_list; gtmp; gtmp=gtmp->Next())

  // transform goto_list outsiders into region_exits and return list
  // also counts number on list
  _nexits = 0;
  GOTO *gnew = NULL, *gtmp_next;
  for (gtmp=Goto_list(); gtmp; gtmp=gtmp_next) {
    gtmp_next = gtmp->Next();
    if (gtmp->Outside()) {
      // if this is an MP region and we have a goto out, it is an error
      if (RID_TYPE_mp(Rid())) {
	char label_str[20];
	sprintf(label_str,"L%d",gtmp->Label_number());
	ErrMsg(EC_Rgn_Ill_Exit, label_str, Srcpos_To_Line(gtmp->Linenum()));
      }
      
      // convert to REGION_EXIT and accumulate number of unique exits
      // if prop_up is not set, we are at the root, don't add up exits,
      //   and don't convert the gotos.
      if (prop_up) {
	gtmp->Set_region_exit(Rid(), this); // converts to region exit
	_nexits++;
      }
      
      // link into list
      gtmp->Set_next(gnew);
      gnew = gtmp;

      if (Trace()) {
	fprintf(TFile,
	    "===== RINIT::Cancel_internal_gotos, propagated up from RGN %d:\n",
		RID_id(Rid()));
	gtmp->fdump(TFile);
      }
    }
  }

  Set_goto_list(NULL); // this isn't right anymore so delete it
  return gnew;
}

// deal with a label at the beginning of a region, split the label and
// make all internal gotos goto a new label and all outside ones continue
// to point to the original label.
// The parameter rinit is the child region, `this' is the parent.
void
RINIT::Handle_split_label(WN *region, WN *orig_label, WN *block, RINIT *rinit)
{
  // We have a label at the beginning of the inner region about to be
  // processed. We need to split it and move the original one up
  // replace is what the old gotos will match to.

  // generate new label for beginning of region (inside use only)
  LABEL_IDX label_no;
  New_LABEL(CURRENT_SYMTAB, label_no);
  WN *label_wn = WN_CreateLabel (label_no, 0, NULL);
  WN_Set_Linenum(label_wn, WN_Get_Linenum(region));
  RGN_LABEL *head = rinit->Add_head_label(label_wn, block); // new head label

  // add new label to beginning of region
  WN_INSERT_BlockAfter(WN_region_body(region), orig_label, label_wn);
  // delete original from region
  WN *orig_wn = WN_CopyNode(orig_label); // this is the label seen outside
  WN_DELETE_FromBlock(WN_region_body(region), orig_label);
  orig_label = NULL; // this was destroyed
  // insert original label before region
  WN_INSERT_BlockBefore(block, region, orig_wn);

  head->Set_replace_label(orig_wn);	// gotos match to this
  // gotos that pointed to orig_wn will match to head replacement
  // change them to point to the new label_wn.
  head->Set_split_label(label_wn);

  if (Trace())
    fprintf(TFile,"RINIT::Handle_split_label, inserted outside label L%d for "
	    "L%d in RGN %d (parent RGN %d)\n", WN_label_number(label_wn),
	    WN_label_number(orig_wn), RID_id(rinit->Rid()), RID_id(Rid()));

  // if the orig label is at the beginning of the outer region, it
  // goes into the head_list for that region
  RGN_LABEL *uplevel;
  uplevel = (WN_first(WN_region_body(RID_rwn(Rid()))) == orig_wn) ?
    Add_head_label(orig_wn, block) : Add_label(orig_wn, block);
  uplevel->Set_replace_label(orig_wn);
  if (Trace())
    fprintf(TFile,"RINIT::Handle_split_label, propagated L%d up to "
	    "RGN %d\n", WN_label_number(orig_wn), RID_id(Rid()));
}

// handle the processing of a region for Region_init
void
RINIT::Process_region(WN *wtmp, WN *block, INT32 level, RID *root,
		      char *parent_options)
{
  WN *wtmp2;
  char *region_options = NULL;

  Set_PU_has_region (Get_Current_PU ());
  Is_True(wtmp && WN_opcode(wtmp) == OPC_REGION,
	  ("RINIT::Process_region, not a region"));
  RID *rid = RID_Create(WN_region_id(wtmp), level, wtmp);
  RID_level(rid) = RL_RGN_INIT;
  RID_bounds_exist(rid) = REGION_BOUND_UNKNOWN;
  RID_is_glue_code(rid) = FALSE;
  Is_True(block != NULL, ("RINIT::Process_region, block is NULL"));

  REGION_kind_to_type(wtmp, rid);

  if (!RID_TYPE_mp(rid))
    _nregions++;	// only count non-MP regions for LNO

  Is_True(RID_type(rid) != RID_TYPE_undefined,
	  ("RINIT::Process_region, unknown region type"));

  // generate options for this region (non-transparent regions)
  if (!RID_TYPE_transparent(rid)) {
    // cases:  region  parent		action
    // --------------------------------------------------------------------
    // 1)	Y	Y      		concatenate parent's and region's
    // 2)	Y	N		no change, use region's options (stab)
    // 3)	N	Y		propagate parent's options to region
    // 4)	N	N		no change, leave as NULL
    region_options = REGION_get_options_string(wtmp);
    // If parent has options, make copy regardless if this region has options.
    // The child region inherits the parents options and adds its own.
    if (parent_options != NULL) {
      INT len = strlen(parent_options) + 2 +
	(region_options ? strlen(region_options) : 0);
      char *tmp = CXX_NEW_ARRAY(char, len, &REGION_mem_pool);
      strcpy(tmp, parent_options);
      strcat(tmp, " ");
      if (region_options)
	strcat(tmp, region_options);
      region_options = tmp;
    }
    RID_options(rid) = region_options; // set the options for this region
  }

  WN_MAP_Set(RID_map, wtmp, (void *)rid);
  RID_Add_kid(rid, root);
  // Insert a REGION_EXIT for fall through unless last statement
  // is a goto or return or region_exit.
  // MP and EH regions are single exit and so do not add a
  // region_exit, it would be redundant.
  wtmp2 = WN_last(WN_region_body(wtmp));
#if defined(TARG_SL)
  // sl2 parallel regions are single exit and so do not add a region_exit
  if (wtmp2 && !RID_TYPE_sl2_para(rid) && !RID_TYPE_mp(rid) && !RID_TYPE_eh(rid) && 
      WN_opcode(wtmp2) != OPC_GOTO && WN_opcode(wtmp2) != OPC_RETURN &&
      WN_opcode(wtmp2) != OPC_REGION_EXIT) {
#else 
  if (wtmp2 && !RID_TYPE_mp(rid) && !RID_TYPE_eh(rid) && 
      WN_opcode(wtmp2) != OPC_GOTO && WN_opcode(wtmp2) != OPC_RETURN &&
      WN_opcode(wtmp2) != OPC_REGION_EXIT) {
#endif
    // insert fall-through (adds to region exit block also)
    WN *wtmp3 = REGION_add_exit(block, WN_next(wtmp), wtmp);
    RID_num_exits(rid)++;
    Is_Trace(Trace(),(TFile,"RINIT::Process_region, added fall-thru "
	     "REGION_EXIT L%d, RGN %d\n", WN_label_number(wtmp3),RID_id(rid)));
  }

  // these next two lines can be removed after 7.2 (see PV 457243)
#ifdef TARG_SL //add_type_for_minor  //PARA_EXTENSION
  if (!RID_TYPE_eh(rid) && !RID_TYPE_mp(rid) && !RID_TYPE_sl2_para(rid)) 
#else 
  if (!RID_TYPE_eh(rid) && !RID_TYPE_mp(rid)) 
#endif   	
    REGION_has_black_regions(rid); // set black bit to top region

  // if region requires bounds, better tell root rid (propagates up to PU)
  if (!RID_TYPE_transparent(rid) || RID_contains_bounds(rid))
#ifdef TARG_SL //fork_joint
  {
       /* if region requires bounds we need propagate the flag up to PU, when rid is not 
         * region 0 (PU) 
         */ 
         
         if(RID_TYPE_sl2_para(rid)) 
              RID_contains_bounds(rid) = TRUE;

         RID*  tmp_rid = root; 
         while(!RID_TYPE_func_entry(tmp_rid))
            tmp_rid = RID_parent(tmp_rid); 
         
	  RID_contains_bounds(tmp_rid) = TRUE;   // propagate the flag to PU if root is not func_entry
   }
#else 
    RID_contains_bounds(root) = TRUE;
#endif //fork_joint

  //-----------------------------------------------------------------
  // visit body looking for nested regions
  RINIT rinit(rid, Mem_pool());

  // check if first statement is a pre-existing label
  BOOL found_split = FALSE;
  wtmp2 = WN_first(WN_region_body(wtmp));
  if (wtmp2 && WN_opcode(wtmp2) == OPC_LABEL) { // region might be empty
    Handle_split_label(wtmp, wtmp2, block, &rinit);
    found_split = TRUE;
  }

  rinit.Region_init(WN_region_body(wtmp), level, rid, region_options);
  _nregions += rinit.Nregions();

  // propagate head label up when generated one level down
  // only do if Handle_split_label was not done before
  wtmp2 = WN_first(WN_region_body(wtmp));
  if (wtmp2 && !found_split && rinit.Head_list() &&
      WN_opcode(wtmp2) == OPC_LABEL)
    Handle_split_label(wtmp, wtmp2, block, &rinit);

  if (Trace()) {
    fprintf(TFile,
	    "===== RINIT::Process_region, RGN %d, %s, loop level = %d\n",
	    RID_id(rid),RID_type_str(RID_type(rid)),level);
    rinit.Print_sets();
  }

#ifdef KEY
// bug 3144: A region is not aware of any label in its nested regions.
// Since we allow goto into a region, we need a parent region to know of 
// labels in its nested regions, so that gotos can be cancelled out properly.
// So, propagate the labels in a nested region to its parent.
  for (RGN_LABEL * labels = rinit.Label_list(); 
       labels;
       labels = labels->Next())
    Add_label (labels->Label(), labels->Block());
#endif // KEY

  // cancel goto-label pairs inside the region, add leftovers to parent
  // sets number of exits (these are the leftovers propagated up)
  GOTO *glist = rinit.Cancel_internal_gotos(TRUE);
  Concat_goto(glist);
  Merge_label_alias(rinit.Label_alias());
  Is_Trace(Trace(), (TFile,
		     "Merge_label_alias from RGN %d to RGN %d\n",
		     RID_id(rinit.Rid()), RID_id(Rid())));
  Is_Trace_cmd(Trace(), Label_alias()->Print(TFile));
  RID_num_exits(rid) = REGION_count_exits(WN_region_exits(RID_rwn(rid)));
  Is_Trace(Trace(), (TFile,"RINIT::Process_region, RGN %d, after "
		     "Cancel_internal_gotos, num_exits = %d\n",
		     RID_id(rid),RID_num_exits(rid)));
  Is_Trace_cmd(Trace(), fdump_tree(TFile,WN_region_exits(RID_rwn(rid))));
}

// ======================================================================
// Region_init (recursive through Process_region)
// Look through a block of WHIRL nodes for regions. Create a RID for each
// and link into the RID tree. Used by REGION_Initialize. This is necessary
// to spot and build RIDs for user pragma regions in the source.
// Returns the number of regions it creates.
// ======================================================================
void
RINIT::Region_init(WN *block, INT32 level, RID *root, char *options)
{
#ifdef KEY
// bug 3740: traverse kids
  if (WN_operator(block) != OPR_BLOCK)
  {
    for (INT32 i=0; i<WN_kid_count(block); i++)
      Region_init (WN_kid(block, i), level, root, options);
    return;
  }
#endif

  for (WN *wtmp=WN_first(block); wtmp; wtmp=WN_next(wtmp)) {

    switch (WN_operator(wtmp)) {
      case OPR_REGION:
        // If the region exit list exists already (-mp program)
        // delete it, it messes up the duplication detecting algorithm
        Is_True(WN_region_exits(wtmp) &&
		WN_opcode(WN_region_exits(wtmp)) == OPC_BLOCK,
		("RINIT::Region_init, malformed region exit block"));
        if (WN_first(WN_region_exits(wtmp)) != NULL) {
	  WN_DELETE_Tree(WN_region_exits(wtmp));
	  WN_region_exits(wtmp) =  WN_CreateBlock();
	}
	// recursive wrt Region_init
        Process_region(wtmp, block, level, root, options);
	break;
      case OPR_DO_LOOP:
	Region_init(WN_do_body(wtmp), level+1, root, options);
	break;
      case OPR_WHILE_DO:
      case OPR_DO_WHILE:
	Region_init(WN_while_body(wtmp), level+1, root, options);
	break;
      case OPR_BLOCK:
	Region_init(wtmp, level, root, options);
	break;
      case OPR_IF:
	Region_init(WN_then(wtmp), level, root, options);
	Region_init(WN_else(wtmp), level, root, options);
	break;
      case OPR_IO:
        if (WN_operator(RID_rwn(root)) == OPR_REGION && !RID_TYPE_mp(root)) {
	  for (INT32 i=0; i<WN_kid_count(wtmp); i++) {
	    WN *kid = WN_kid(wtmp, i);
	    if (WN_opcode(kid) == OPC_IO_ITEM &&
		(WN_io_item(kid) == IOC_END || WN_io_item(kid)==IOC_ERR ||
		 WN_io_item(kid) == IOC_EOR)) {
	      WN *kid0 = WN_kid0(kid);

	      // Add this IO statement to special IO goto list for processing
	      // at end. Save WN * to IO stmt, the rid, and the block to
	      // insert the goto patch. If the destination of the IO stmt is
	      // outside the region, we modify the IO stmt to point to a new
	      // label and then once there we do a region_exit.
	      Add_goto(kid0, WN_region_body(RID_rwn(root)), TRUE, FALSE);
	    }
	  }
        }
        break;
      case OPR_GOTO:
      case OPR_TRUEBR:
      case OPR_FALSEBR:
      case OPR_REGION_EXIT:
	Add_goto(wtmp, block, FALSE, FALSE);
	break;
      case OPR_COMPGOTO:
      { WN *gotoblock = WN_kid(wtmp,1);
	Is_True(WN_opcode(gotoblock) == OPC_BLOCK,
		("RINIT::Region_init, comp goto in wrong form"));
	for (WN *wtmp2=WN_first(gotoblock); wtmp2; wtmp2=WN_next(wtmp2))
	  Add_goto(wtmp2, gotoblock, FALSE, TRUE);
      }
        break;
      case OPR_LABEL:
        Add_label(wtmp, block);
	break;
      case OPR_RETURN:
        REGION_propagate_return(root);
        break;

      default:
#ifdef KEY
// bug 3740: traverse kids
        for (INT32 i=0; i<WN_kid_count(wtmp); i++)
          Region_init (WN_kid (wtmp, i), level, root, options);
#endif // KEY
	break;
    } // switch (WN_operator(wtmp))

  } // for (wtmp=WN_first(block); wtmp; wtmp=WN_next(wtmp))
}

// ======================================================================
// this is only called if SYMTAB_has_rgn(Current_Symtab) is true
// called by REGION_Initialize
// ======================================================================
static INT
REGION_init(WN *itree, RID *root)
{
  Is_True(WN_opcode(itree) == OPC_FUNC_ENTRY,
	  ("REGION_init must be called on a func_entry"));

  Set_Error_Phase("Region Init");

  MEM_POOL REGION_init_pool;
  MEM_POOL_Initialize(&REGION_init_pool, "REGION_init_pool", FALSE);
  MEM_POOL_Push(&REGION_init_pool);
  
  RINIT rtmp(root, &REGION_init_pool);
  rtmp.Region_init(WN_func_body(itree), 0, root, RID_options(root));

  GOTO *g_unresolved = rtmp.Cancel_internal_gotos(0);
  // any gotos left over at top level are illegal entries
  for (GOTO *gtmp=g_unresolved; gtmp; gtmp=gtmp->Next()) {
    BOOL found = FALSE;
    LABEL_IDX label;
    for (label=0; label < Scope_tab[CURRENT_SYMTAB].label_tab->Size();label++){
      if (gtmp->Compare_labels(label)) {
#ifndef KEY
	Is_Trace_cmd(rtmp.Trace(), fdump_tree(TFile, itree));
	Is_Trace_cmd(rtmp.Trace(), RID_Tree_Print(TFile, root));
        char buffer [20];
        sprintf(buffer, "%d", label);
	ErrMsg(EC_Rgn_Ill_Entry, buffer, Srcpos_To_Line(gtmp->Linenum()));
	found = TRUE;
#else
// We are not checking for jumps from outside a region into a region. The
// compiler itself generates jumps inside exception handlers which can
// potentially jump into a region from outside. Doing a bit more work, one
// solution is enclosing the handlers inside some special region, and
// checking for it here.
                                                                                
// Without something like this, we disable it. We have found the target of
// the goto, so break out.
        found = TRUE;
        break;
#endif
      }
    }
    if (!found) {
      Is_Trace_cmd(rtmp.Trace(), fdump_tree(TFile, itree));
      Is_Trace_cmd(rtmp.Trace(), RID_Tree_Print(TFile, root));
      char label_str[20];
      sprintf(label_str,"L%d",gtmp->Label_number());
      ErrMsg(EC_Rgn_Ill_Entry, label_str, Srcpos_To_Line(gtmp->Linenum()));
    }
  }

  if (rtmp.Trace()) {
    fprintf(TFile,"===== Region_init, %s%d\n",
	    RID_TYPE_func_entry(root) ? "PU" : "RGN", RID_id(root));
    rtmp.Print_sets();
  }

  INT32 nregions = rtmp.Nregions(); // doesn't count PU
  MEM_POOL_Delete(&REGION_init_pool);
  return nregions;
}

// ======================================================================
// Region_skip (recursive)
// If skip is set, take an extra pass through and remove some regions
// before any labels, gotos, or RIDs are messed with.
// Tricky: returns TRUE if is removed a region. In this case the
// loop through the statements one block up will no longer work because
// the iterator points to the old region. Restart from this block.
// ======================================================================
static bool Region_skip(WN *block, WN *wn)
{
  WN *wtmp;
  INT i;

  Is_True(wn, ("Region_skip, NULL wn"));

  switch (WN_operator(wn)) {

    case OPR_REGION:
      Is_True(block && WN_operator(block) == OPR_BLOCK,
	      ("Region_skip, NULL block"));
      if (Query_Skiplist(Region_Skip_List, WN_region_id(wn))) {
	ErrMsg (EC_Region_Skipped, WN_region_id(wn) );
#ifdef Is_True_On
	// find wn following the region
	for (wtmp = WN_first(block); wtmp && wtmp != wn; wtmp = WN_next(wtmp))
	  ;
	Is_True(wtmp == wn, ("Region_skip, cannot find wn in block"));
#endif
	// code following the region
	wtmp = WN_next(wn);
	// remove the region
	WN *region_body = WN_region_body(WN_EXTRACT_FromBlock(block, wn));
	Is_True(WN_operator(region_body) == OPR_BLOCK,
		("Region_skip, region body is not a block"));
	// re-insert region body
	// these routines removes the block within a block
	if (wtmp == NULL) // region was last statement in block
	  WN_INSERT_BlockLast(block, region_body);
	else
	  WN_INSERT_BlockBefore(block, wtmp, region_body);
	// return TRUE which causes the enclosing block to be rescanned
	return TRUE;
      } else
	Region_skip(NULL, WN_region_body(wn));
      break;

    case OPR_FUNC_ENTRY:
      Is_True(!block, ("Region_skip, non-NULL block in func_entry"));
      Is_True(WN_operator(WN_func_body(wn)) == OPR_BLOCK,
	      ("Region_skip, func_body messed up"));
      Region_skip(NULL, WN_func_body(wn));
      break;

    case OPR_BLOCK:
    { BOOL redoit;
      do {
	redoit = FALSE;
	for (wtmp=WN_first(wn); wtmp; wtmp=WN_next(wtmp)) {
	  // Tricky: if we find a region one level down and remove it,
	  // this loop will end because next is NULL. So we set the
	  // redoit flag and redo the block from the beginning.
	  if (Region_skip(wn, wtmp)) {
	    redoit = TRUE;
	    break;
	  }
	}
      } while (redoit);
    }
      break;

    default:
      for (i=0; i<WN_kid_count(wn); i++)
	Region_skip(block, WN_kid(wn,i));
      break;
  }
  return FALSE;
}

#ifdef Is_True_On
// ======================================================================
// verify_region_initialization
// check that it is right, recursive on RIDs
// ======================================================================
static void verify_region_initialization(RID *rid, RID *root)
{
  RID *rtmp;

  Is_True(rid != NULL, ("verify_region_initialization, NULL rid"));
  Is_Trace(Get_Trace(TP_REGION, TT_REGION_ALL),
	   (TFile,"verify_region_initialization, RGN %d\n",RID_id(rid)));

  // checks WHIRL <-> RID mapping
  REGION_consistency_check(RID_rwn(rid));

  // check RID_parent pointer
  if (RID_TYPE_func_entry(rid))
    Is_True(root == NULL, ("verify_region_initialization, non-NULL root"));
  else
    Is_True(RID_parent(rid) == root,
	    ("verify_region_initialization, messed up parent"));
  
  for (rtmp=RID_first_kid(rid); rtmp; rtmp=RID_next(rtmp))
    verify_region_initialization(rtmp, rid);
}
#endif

// ======================================================================
// REGION_Initialize, called by be/driver.c
//   returns number of regions currently in PU
//   create RIDs for all regions
//   correctly mark all region exits
//   look for incorrect region placement
//   create initial boundary sets
// NOTE: this could be moved to when the .B file is read in for efficiency
// ======================================================================
MEM_POOL  REGION_mem_pool;		// mempool for all region structures
WN_MAP    RID_map = WN_MAP_UNDEFINED;	// map for WHIRL to RID connection

#define REGION_MAP_ARRAY_SIZE 32
static WN_MAP region_map_array[REGION_MAP_ARRAY_SIZE];
static INT32 region_map_index = -1;

extern "C"
INT REGION_Initialize(WN *wn, BOOL has_rgns)
{
  RID *rid;
  INT nregions = 0;

  // Create and push a pool for region-related memory
  { 
    // Initialize just once, then Push/Pop
    static BOOL region_mem_pool_initialized = FALSE;
    if (region_mem_pool_initialized == FALSE) {
      MEM_POOL_Initialize(&REGION_mem_pool, "REGION", TRUE);
      region_mem_pool_initialized = TRUE;
    }
  }
  MEM_POOL_Push(&REGION_mem_pool);

  RID_map = WN_MAP_Create(&REGION_mem_pool);
  WN_MAP_Set_dont_copy(RID_map, TRUE);

  // save RID_map in an array becuse it may change in nested PUs
  region_map_index++;
  FmtAssert(0 <= region_map_index && region_map_index < REGION_MAP_ARRAY_SIZE,
            ("REGION_Initialize: region_map_index is out of range"));
  region_map_array[region_map_index] = RID_map;

  Is_True(WN_opcode(wn) == OPC_FUNC_ENTRY,
	  ("REGION_Initialize, not a func_entry"));

  rid = RID_Create(0/* PU is always region 0 */, 0, wn);
  RID_level(rid) = RL_RGN_INIT;
  RID_bounds_exist(rid) = REGION_BOUND_UNKNOWN;
  RID_has_return(rid) = REGION_NO_RETURN;
  RID_num_exits(rid) = 0;
  RID_TYPE_func_entry_Set(rid); // this rid is not really a region
  RID_options(rid) = REGION_get_options_string(wn); // options for PU
  WN_MAP_Set(RID_map, wn, (void *)rid);
  nregions++;

  // now scan PU looking for regions to skip if skip is on
  if (Region_Skip_List != NULL)
    Region_skip(NULL,wn);

  // now look for user inserted regions within the PUs (REGION_init)
  // this should really be moved to when the .B file is 
  // read in because it looks at every node
  if (has_rgns)
    nregions += REGION_init(wn, rid);

  if (Get_Trace(TKIND_IR, TP_REGION)) {
    fputs(DBar, TFile);
    fprintf(TFile, "After Region Init:\n");
    fdump_tree(TFile, wn);
    fputs(DBar, TFile);
  }
  if (Get_Trace(TP_REGION, TT_REGION_ALL)) {
    fprintf(TFile,"===== REGION_Initialize, nregions = %d\n",nregions);
    RID_WN_Tree_Print(TFile,wn);
    Is_Trace_cmd(Get_Trace(TP_REGION, TT_REGION_ALL),
		 fdump_tree(TFile,wn));
    Is_Trace_cmd(Get_Trace(TP_REGION, TT_REGION_ALL),
		 fprintf(TFile,"%s\n",DBar));
  }
#ifdef Is_True_On
  Set_Error_Phase("Verify Region Init");
  verify_region_initialization(REGION_get_rid(wn), NULL);
#endif
  return nregions;
}

// =======================================================================
// REGION_Finalize
// =======================================================================
extern "C" void REGION_Finalize(void)
{
  WN_MAP_Delete(RID_map);
  MEM_POOL_Pop(&REGION_mem_pool);

  region_map_index--;
  if (region_map_index >= 0) {
    // if there is a saved region map, then restore it
    RID_map = region_map_array[region_map_index];
  }
  else {
    // otherwise, set it to undefined
    RID_map = WN_MAP_UNDEFINED;
  }
}
