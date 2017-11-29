/*
 * Copyright (C) 2008-2010 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

//-*-c++-*-
// ====================================================================
// ====================================================================
//
// Module: opt_bb.h
// $Revision: 1.1.1.1 $
// $Date: 2005/10/21 19:00:00 $
// $Author: marcel $
// $Source: /proj/osprey/CVS/open64/osprey1.0/be/opt/opt_bb.h,v $
//
// ====================================================================
//
// Copyright (C) 2000, 2001 Silicon Graphics, Inc.  All Rights Reserved.
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of version 2 of the GNU General Public License as
// published by the Free Software Foundation.
//
// This program is distributed in the hope that it would be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//
// Further, this software is distributed without any warranty that it
// is free of the rightful claim of any third person regarding
// infringement  or the like.  Any license provided herein, whether
// implied or otherwise, applies only to this software file.  Patent
// licenses, if any, provided herein do not apply to combinations of
// this program with other software, or any other product whatsoever.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write the Free Software Foundation,
// Inc., 59 Temple Place - Suite 330, Boston MA 02111-1307, USA.
//
// Contact information:  Silicon Graphics, Inc., 1600 Amphitheatre Pky,
// Mountain View, CA 94043, or:
//
// http://www.sgi.com
//
// For further information regarding this notice, see:
//
// http://oss.sgi.com/projects/GenInfo/NoticeExplan
//
// ====================================================================
//
// Description:
//
//                  Basic Block Nodes
//                  -----------------
//
//     This is the basic block data structure that carrys control flow
//     related information.  It is used to represent the control flow
//     related information as well as the list of statements inside
//     basic block.
//
// Reserved prefix:
// ----------------
//
//     BB              for basic block node
//
// Exported types:
// ---------------
//     BB_NODE
//
//          A basic block node.  Every basic block node contains the
//          following  fields:
//
//              IDTYPE       _id
//                    A unique identifier for a basic block.  It's
//                    numbered according to order of occurrence in the
//                    procdure.
//
//              mUINT8       _loopdepth
//                    loop nesting depth.  It is initialized to 0, and
//                    set by the CFG::Analyze_loops function.  This
//                    member is used to identify candidate of loop
//                    invariant recognition.
//
//              BB_FLAG      _flags
//                    The list of flags it contains are defined in
//                    BB_FLAG.
//
//              BB_KIND      _kind
//                    The list of basic block kind are defined in
//                    BB_KIND.
//
//              BB_LIST     *_pred;
//                    The list of predecessors.
//
//              BB_LIST     *_succ;
//                    The list of successors.
//
//                    NOTE:
//                    Both predecessor list and successor list are of
//                    BB_LIST type.  Since most likely the list
//                    contains only one or two basic blocks, it does
//                    not justify to use BB_LIST_CONTAINER.  So, when
//                    we add a bb node to the list, we use the more
//                    efficient prepend method.
//
//              BB_NODE     *_idom;
//                    point to immediate dominator block
//
//              BB_NODE     *_ipdom;
//                    point to immediate post dominator block
//
//		BB_LIST     *_dom_bbs
//		      list of dominated blocks
//
//		BB_LIST     *_pdom_bbs
//		      list of post-dominated blocks
//
//		IDTYPE       _dom_dfs_id
//		      number in preorder dominator tree walk
//
//		IDTYPE       _dom_dfs_last
//		      _dom_dfs_id number of last dominated block
//
//		IDTYPE       _pdom_dfs_id
//		      number in preorder post-dominator tree walk
//
//		IDTYPE       _pdom_dfs_last
//		      _pdom_dfs_id number of last post-dominated block
//
//
//              BB_NODE_SET *_dom_frontier;
//                    point to dominance frontier set
//
//              BB_NODE_SET *_rcfg_dom_frontier;
//                    point to dominance frontier set for reverse
//                    control graph
//
//              BB_NODE_SET *_control_dependence;
//                    point to control dependence control graph
//
//              mUINT32      _labnam;
//                    Represents the label number of this block.
//
//		WN           *_lab_loop_info;
//		      Represents opc_loop_info wn attached to label
//
//              WN          *_firststmt;
//                    point to first WHIRL statement node in the basic
//                    block.  After the value number is completed,
//                    this member is cleared.  At the preopt emitter
//                    time, this member is reinitialized.
//
//              WN          *_laststmt;
//                    point to last WHIRL statement node of the basic block
//
//              STMT_LIST    _stmtlist;
//                    list of internal statement representation.  The
//                    class STMT_LIST is defined in opt_stmt.h
//
//              WN          *_entry_wn;
//                    wn statement node for the function entry block
//
//              BB_LOOP     *_innermost;
//                    the innermost loop this BB belongs to.  If this
//                    BB is not in any loop, the innermost member
//                    contains NULL pointer.  The pointer is
//                    initialized to NULL, and set by the
//                    CFG::Analyze_loops function.
//
//              BB_LOOP     *_loop;
//                    Loop information.
//
//              BB_IFINFO   *_ifinfo;
//                    If statement information
//
//              BB_SWITCH   *_switchinfo/*_ioinfo;
//                    comp_goto (switch) statement information
//		      io statement information about bbs may jump to
//
//              PHI_LIST    *_phi_list;
//                    point to a list of phi functions.  Any bb on an
//                    join point contains phi_list.
//
//		CODEREP_LIST *_pvl;
//		      point to the propagated variable list (created by copy
//		      propagation for variables propagated past this BB)
//
//              SRCPOS       _linenum;
//                    the source position starting this block
//
//              Here are the list of bit vectors that are used in main
//              opt and RVI phase.  Please note the union structure.
//              You have to be very careful when you shuffle these bit
//              vectors' layout, because the bit vector are reused in
//              different phases of the optimizer.  For every bit
//              vector, there is notion about when it is generated.
//
//              IDX_32_SET *_loc_appear;
//                 used for RVI (def'd or ref'd in bb)
//
//              IDX_32_SET *_loc_def;
//                 used for RVI (def'd in bb)
//
//              IDX_32_SET *_loc_upwd;
//                 used for RVI (local upward exposed use)
//
//              IDX_32_SET *_defreach;
//                 used for RVI (def reaches entry of bb)
//
//              IDX_32_SET *_live_at_exit;
//                 used for RVI (live-in to successors)
//
//              IDX_32_SET *_live_out;
//                 used for RVI (poss. aliased use in succ)
//
//              IDX_32_SET *_loc_mu_ref;
//                 used for RVI (local aliased ref)
//
//              WN *_loc_mu_wn;
//                 used for RVI (wn stmt that has mu ref)
//
//              IDX_32_SET *_loc_chi_def;
//                 used for RVI (local aliased def)
//
//              IDX_32_SET *_unstored_defs;
//                 used for RVI (set of defs unstored by last ref)
//
//              RVI_ANN_LIST *_rvi_anns;
//                 used for RVI (annotations)
//
//              IDX_32 _last_stid_bitpos;
//              BOOL   _last_stid_has_chi;
//                 used for RVI (info about stid that ends block)
//
//       Exported Methods:
//
//          All member access methods and member setting methods are
//          skipped here.
// 
//          void         Print(FILE *fp=stderr);
//                  print the bb structure
//          void         Print_wn(FILE *fp=stderr);
//                  print the bb structure
//          void         Print_ssa(FILE *fp=stderr);
//                  print the bb structure
//          void         Print_head(FILE *fp=stderr) const;
//                  print the cfg info of the bb
//          void         Print_locattr(FILE *fp=stderr);
//                  print the local attributes
//          void         Validate(FILE *fp = stderr) const;
//                  validate assumptions in bb
//          void         Gen_wn(EMITTER *);
//                  generate the WHIRL stmt list from the stmtlist
//          void         Gen_insertions(MAIN_EMITTER *);
//                  process inserts at end of BB
//          void         Append_pred(BB_NODE *bb, MEM_POOL *pool);
//                  to append a 'bb' in the pred list of 'this'
//          void         Append_succ(BB_NODE *bb, MEM_POOL *pool);
//                  to append a 'bb' in the succ list of 'this'
//          void         Remove_pred(BB_NODE *bb, MEM_POOL *pool);
//                  to remove the 'bb' from the pred list of 'this'
//          void         Remove_succ(BB_NODE *bb, MEM_POOL *pool);
//                  to remove the 'bb' from the succ list of 'this'
//          void         Replace_pred(BB_NODE *old, BB_NODE *new)
//                  to replace the 'old' from the pred list with 'new'
//          void         Replace_succ(BB_NODE *old, BB_NODE *new)
//                  to replace the 'old' from the succ list with 'new'
//          void         Connect_wn(WN *wn);
//                  to connect the 'wn' node at the end of the WN
//                  statement list of this bb.
//          void         Connect_wn_list(WN *wn_f, WN *wn_l);
//                  similar to connect_wn, just connect a list instead
//          CODEREP     *Find_cr(const CODEREP *cr);
//                  defined in opt_find.cxx
//                  Find the coderep with the same bit position as the 'cr'
//          CODEREP     *Find_def(const CODEREP *cr, BOOL ignore_dead_phi);
//                  defined in opt_find.cxx
//                  Find the coderep that is the definition point of
//                  the given 'cr'
//                  if ignore_dead_phi is FALSE, return NULL when a dead phi is reached
//	    void         Insert_wn_after( WN *wn, WN *after_this )
//		    add wn after after_this (if after_this is null,
//		    inserts at the beginning of the block)
//	    void         Insert_wn_before( WN *wn, WN *before_this )
//		    add wn before before_this (if before_this is null,
//		    inserts at the end of the block)
//          void         Append_wn_before_branch(WN *wn);
//                  append wn to end of bb, but before any branch
//          void         Prepend_wn_after_labels(WN *wn);
//                  prepend wn to start of bb, but after labels, pragmas, etc.
//          BOOL         Is_first(void)    const;
//                  is this BB the first in the prev-next chain
//          BOOL         Is_last(void)     const;
//                  is this BB the last in the prev-next chain
//          STMTREP     *Add_stmtnode(WN*, MEM_POOL*);
//                  build/append a statement node based on the WHIRL
//          void         Append_stmtrep(STMTREP*);
//                  add a stmtrep to the end of a block, but before any
//                  ending  branch
//          void         Prepend_stmtrep (STMTREP *s);
//                  add a stmtrep to the beginning of a block, but
//                  after any label
//          void         Remove_stmtrep(STMTREP*);
//                  remove a statement
//	    void	Append_stmt_before_branch(STMTREP*);
//		    append as last statement in BB but before any jump stmt
//          STMTREP     *Branch_stmtrep(void);
//                  check if the block has a branch stmt_rep, and return it
//          STMTREP     *Label_stmtrep(void);
//                  check if the block has a label stmt_rep, and return it
//          void         Romove_label_stmtrep()
//                  remove all labels
//          void         Add_label_stmtrep(MEM_POOL *pool);
//                  add a label stmt_rep if it does not have one
//          void         Remove_phi_reference( INT32 whichpred );
//                  update all of this block's phi nodes to remove
//                  values that come from predecessor whichpred (0 is
//                  first pred) 
//          void         Insert_tree(CODEREP *);
//                  add a tree to the insertions and subinsert bitvector
//          WN          *Find_outermost_loopstmt(CODEREP *cr)
//                  Find the outermost loopstmt that has phi function
//                  for the same aux_id of CODEREP 'cr'.
//       
//     BB_IFINFO
//
//          The high WHIRL if statement is partially lowered to
//          construct the control flow graph in order to compute the
//          dominator frontier for the purpose of SSA representation.
//          This process happens in Preopt, which is called from LNO
//          or IPA phase.  Because these two phases operate on high
//          WHIRL, at the end of the Preopt, when we regenerate the
//          high WHIRL representation, one of the thing we have to do
//          is to raise the CFG back to the high WHIRL if statement.
//          To aid this process, we create this BB_IFINFO structure at
//          the CFG construction time and use this info at the emitter
//          phase.  It keep tracks of:
//              SRCPOS   _thenloc;
//                    records the source code position for the then
//                    block.
//              SRCPOS   _elseloc;
//                    records the source code position for the else
//                    block. 
//              BB_NODE *_cond;
//                    points to the block that keeps the branch
//                    condition.
//              BB_NODE *_then;
//                    points to the then block
//              BB_NODE *_else;
//                    points to the else block
//              BB_NODE *_merge;
//                    points to the merge block, which join then and
//                    else part of the if statement
//
//          Since this class is purely for book keeping.  All methods
//          are for member access.
//
//     BB_LOOP
//
//          The loop nesting structure are embedded in the CFG.  We
//          keep the abstraction in this data structure for two
//          preopt, mainopt:
//             a. Preopt phase, this structure is used as book keeper
//                              such that the emitter can reconstruct
//                              the high level loop statement.  In the
//                              IVR phase, we use the loop structure
//                              to identify the primary induction
//                              variable.  At the emitter phase, we
//                              also use this structure to determine
//                              what loop cannot be raised to DO_LOOP,
//                              and raise it to WHILE_DO if necessary.
//             b. Mainopt phase,this structure is used in Strength
//                              Reduction and Linear Function Testing
//                              Replacement.  We use this structure to
//                              find out if an expression is a loop
//                              invariant.
//          The loop structure is computed by the function
//          CFG::Analyze_loops.  The loop structure remains valid till
//          the CFG is modified due to Aggrassive DCE or other
//          optimization.  When that happens, the phase that change
//          the CFG is reposible to call the function
//          CFG::Invalidate_loops.  After this call, any call to
//          CFG::Analyze_loops will recompute the loop structure.
//
//          BB_LOOP is derived from SLIST_NODE defined in cxx_base.h.
//          Additional members in this class are:
//              BB_LOOP     *_child;
//                    points to its first child
//              BB_LOOP     *_parent;
//                    points to its parent loop
//              WN          *_loopstmt;
//                    The high level loop statement
//              WN          *_index;
//                    the primary IV for the do_loop
//              BB_NODE     *_start;
//                    Valid only for DO_LOOP in Preopt
//              BB_NODE     *_dohead;
//                    for all loops in Mainopt, overlap with _start
//              BB_NODE     *_end;
//                    All loop has this field
//              BB_NODE     *_body;
//                    the first body block
//              BB_NODE     *_step;
//                    Valid only for DO_LOOP in Preopt
//              BB_NODE     *_merge;
//                    The bb follows the loop
//              BB_NODE     *_dotail;
//                    for all loops in Mainopt, overlap with _merge
//              BB_NODE_SET *_body_set;
//                    the bitset that contains all BBs in the loop body 
//		      according to nesting inside the structured control flow
//		      statements
//              BB_NODE_SET *_true_body_set;
//                    the bitset that contains all BBs in the loop body
//		      according to control flow analysis; must be a subset of
//		      _body_set
//
//          Public methods:
//
//              BB_LOOP     *Append (BB_LOOP*, MEM_POOL *);
//                    insert at tail, return head
//              BOOL         Contains (BB_LOOP *l) const;
//                    return TRUE if 'this' is 'l' or its parent loop.
//
//          All other methods are for member access only.
//
//     BB_LOOP_CONTAINER
//     BB_LOOP_CONST_ITER
//     BB_LOOP_ITER
//
//          Just like any other class that is derived from SLIST_NODE,
//          we define the container class and iterator class for
//          building and iterate the loop structure.
//
//     BB_SWITCH
//
//          This structure is created in the preopt phase for raising
//          back the high WHIRL at the emit time.  The members are:
//
//              BB_NODE   *_def_blk;
//                    default block for switch
//              BB_NODE   **_case_blks;
//                    array of blocks for each case
//              INT32     _num_entries;
//                    how many blocks in case_blks
//
//          All public methods are for member access only.
//
//     BB_LIST : SLIST_NODE
//
//          A singly linked list of BB_NODE.  We use this class to
//          build the predecessor/successor/dominator/post-dominator
//          list.
//
//     BB_LIST_CONTAINER : SLIST
//     BB_LIST_CONST_ITER : SLIST_ITER
//
//          The standard container and iterator for singly linked list.
//
//     BB_FLAG
//
//          The set of flags that we tag to the BB_NODE.
//
//          BB_WILLEXIT  = 0x01
//                this block can reach exit block
//          BB_REACHED   = 0x02
//                this block is reachable from entry
//          BB_DFORDER   = 0x04
//                to indicate whether reaching node is processed in
//                depth-first ordering 
//          BB_HASUJP    = 0x08
//                this block has ujp statement
//          BB_WNGEND    = 0x10
//                the WN code generated
//          BB_HASPRAGMA = 0x20
//                block has pragma statements in it
//          BB_IFMERGE   = 0x40
//                merged block for IF/COMPGOTO
//          BB_HASCALL   = 0x80
//                block has call in it
//          BB_CALLREL   = 0x100
//                block may have call-related code at top, which must
//		  be kept at the top of the block
//          BB_VN_PROCESSED   = 0x200
//                block has been processed by SSA's Value_number routine
//
//     BB_KIND
//
//          BB_GOTO
//                single target BB
//          BB_LOGIF
//                logical if
//          BB_VARGOTO
//                variable goto
//          BB_ENTRY
//                the entry bb
//          BB_EXIT
//                the exit bb
//	    BB_REGIONEXIT
//		  bb exits the region (goto)
//          BB_DOSTART
//                init block
//          BB_DOEND
//                ending condition
//          BB_DOSTEP
//                increment
//          BB_DOHEAD
//                dohead block
//          BB_DOTAIL
//                dotail block
//          BB_IO
//                block that ends with IO stmt that has control flow
//          BB_WHILEEND
//                ending condition for while statement
//          BB_REGIONSTART
//		  start of a region (should be empty)
//          BB_REPEATBODY
//                first BB in repeat body
//          BB_REPEATEND
//                ending condition for repeat statement
//          BB_SUMMARY
//                summary BB
// ====================================================================

#if !defined(opt_bb_INCLUDED) || defined(opt_bb_CXX)
#ifndef opt_bb_INCLUDED
#ifdef _KEEP_RCS_ID
static char *opt_bbrcs_id =  opt_bb_INCLUDED"$Revision$";
#endif /* _KEEP_RCS_ID */

#ifndef opt_base_INCLUDED
#include "cxx_base.h"
#endif
#ifndef cxx_template_INCLUDED
#include "cxx_template.h"
#endif
#ifndef opt_defs_INCLUDED
#include "opt_defs.h"
#endif
#include "wn.h"
#include "wn_util.h"
#ifndef opt_stmt_INCLUDED
#include "opt_stmt.h"
#endif
#ifndef opt_wn_INCLUDED
#include "opt_wn.h"
#endif
#ifndef srcpos_INCLUDED
#include "srcpos.h"
#endif

#include "defs.h"		// Is_True
#include "cxx_memory.h"
#include "opt_util.h"
#include "region_util.h"	// RID_num_exits

// forward declarations
typedef struct region_id RID;
class BB_LIST; 
class BB_NODE;
class BB_NODE_SET;
class BB_REGION;
class CFG;
class CODEREP;
class CODEREP_LIST;
class DU_MANAGER;
class CODEMAP;
class EMITTER;
class EXP_PHI;
class EXP_PHI_LIST;
class MAIN_EMITTER;
class IDX_32_SET;
class ITABLE;
class IVR;
class IV_CAND;
class OPT_STAB;
class PHI_LIST;
class RVI_ANN_LIST;
class EXP_OCCURS_CONTAINER;
class SC_NODE;
class SC_LIST;

class BB_IFINFO {
private:
  SRCPOS   _thenloc;
  SRCPOS   _elseloc;
  BB_NODE *_cond;
  BB_NODE *_then;
  BB_NODE *_else;
  BB_NODE *_merge;
  BB_IFINFO(const BB_IFINFO&);
  BB_IFINFO& operator = (const BB_IFINFO&);
public:
  BB_IFINFO(void);
  BB_IFINFO(SRCPOS   thenloc,
	    SRCPOS   elseloc,
	    BB_NODE *bb_cond,
	    BB_NODE *bb_then,
	    BB_NODE *bb_else,
	    BB_NODE *bb_merge)   { _thenloc = thenloc;
				   _elseloc = elseloc;
				   _cond = bb_cond;
				   _then = bb_then;
				   _else = bb_else;
				   _merge = bb_merge;
				 }
  ~BB_IFINFO(void);
  SRCPOS   Thenloc(void) const   { return _thenloc; }
  SRCPOS   Elseloc(void) const   { return _elseloc; }
  BB_NODE *Cond(void)  const     { return _cond; }
  BB_NODE *Then(void) const      { return _then; }
  BB_NODE *Else(void) const      { return _else; }
  BB_NODE *Merge(void) const     { return _merge; }
  void     Set_then(BB_NODE * i) { _then = i; }
  void     Set_else(BB_NODE * i) { _else = i; }
  void     Set_merge(BB_NODE * i) { _merge = i; }
  void     Set_cond(BB_NODE * i) { _cond = i; }
};

enum LOOP_FLAGS {
  LOOP_EMPTY           =  0x0,    // no flag
  LOOP_DO              =  0x1,    // loop created from DO_LOOP
  LOOP_WHILE           =  0x2,    // loop created from WHILEDO
  LOOP_REPEAT          =  0x4,    // loop created from DOWHILE
  LOOP_PREOPT          =  0x10,   // preopt's loop structure
  LOOP_PRE_DO          =  0x11,   // preopt's loop created from DO_LOOP
  LOOP_PRE_WHILE       =  0x12,   // preopt's loop created from WHILEDO
  LOOP_PRE_REPEAT      =  0x14,   // preopt's loop created from DOWHILE
  LOOP_TYPES           =  0x17,   // all loop types
  LOOP_ORIGINAL_DO     =  0x20,   // a original do-loop
  LOOP_IS_MP           =  0x40,
  LOOP_IS_PDO          =  0x80,
  LOOP_HAS_REAL_OCC    = 0x100,   // in SSAPRE: loop contains a real
				  // occurrence of the current
				  // expression (for LFTR termination
				  // guarantee).
  LOOP_HAS_MV_ALIAS    = 0x200,   // Memory references in this loop are
                                  // associated with loop multi-version
                                  // alias groups
};

enum MP_TY { MP_REGION, MP_DOACROSS = 0x40, MP_PDO = 0x80};

#if defined(TARG_SL) //PARA_EXTENSION
enum SL2_PARA_TY { SL2_PARA_REGION};
#endif

class BB_LOOP : public SLIST_NODE{
private:
                                 // next point to its sibling
  BB_LOOP      *_child;          // points to its first child
  BB_LOOP      *_parent;         // points to its parent loop

  // SCF loop information.  
  //
  WN           *_loopstmt;       // The high level loop statement
  WN           *_index;          // the primary IV for the do_loop
  union {
    BB_NODE    *_start;          // Valid only for DO_LOOP in Preopt
    BB_NODE    *_dohead;         // for DO_LOOP and WHILE_loop
  } _u1;
  BB_NODE      *_end;            // All loop has this field
  BB_NODE      *_body;           // the first body block
  BB_NODE      *_step;           // Valid only for DO_LOOP in Preopt
  union {
    BB_NODE    *_merge;          // The bb follows the loop
    BB_NODE    *_dotail;         // for DO_LOOP and WHILE_loop
  } _u2;
  BB_NODE_SET  *_body_set;       // The set of loop body BBs from SCF statements


  // Control Flow Loop information. Vaild in PREOPT and MAINOPT.
  //
  BOOL          well_formed;      //  is a valid loop!
  BOOL          has_entry_guard;  //  true for well-formed DO_LOOP and WHILE_LOOP
  BOOL          _valid_doloop;
  BB_NODE      *header;
  BB_NODE      *tail;
  BB_NODE      *preheader;
  BB_NODE      *loopback;
  INT           header_pred_count;
  INT           preheader_pred_num;
  INT           loopback_pred_num;
  bool          test_at_entry;
  bool          test_at_exit;
  bool          exit_early;
  INT           depth;      // nesting depth from procedure entry
  INT           max_depth;  // max nesting depth of all contained loops

  BB_NODE_SET  *_true_body_set;    // The set of true loop body BBs from control
				   //   flow analysis

  // Misc
  CODEREP      *_iv;             // Induction variable of the loop
  CODEREP      *_iv_replacement; // LFTR replaced iv with this expr


  IDX_32_SET   *_lftr_non_candidates; // it is useless to perform LFTR2 because
				 // these variables cannot disappear in the loop

  // either trip_count_stmt or trip_count_expr is set, but not both!
  STMTREP      *_trip_count_stmt;// Trip count of the loop
  CODEREP      *_trip_count_expr;// if the trip count is a const
  CODEREP      *_entry_test;     // the assertion { entry_test } is always valid inside the loop

  WN           *_wn_trip_count;  // WN * (trip count of the loop)
  LOOP_FLAGS    _flags;          // all kinds of flags for this loop
  WN           *_orig_wn;        // keep the original WN node to maintain the maps
  BOOL          _promoted_do;    // is a promoted do-loop
  INT32         _size_estimate;  // rough estimate of size of the loop body

  BB_LOOP(const BB_LOOP&);
  BB_LOOP& operator = (const BB_LOOP&);

  DECLARE_SLIST_NODE_CLASS(BB_LOOP)

  // is this given cr invariant in this loop nest
  BOOL         Invariant_cr_rec( CODEREP * ) const;

public:
  BB_LOOP(void);
  BB_LOOP (WN *index, BB_NODE *start, BB_NODE *end, BB_NODE *body,
	       BB_NODE *step, BB_NODE *merge);
  ~BB_LOOP(void) {};
  BB_LOOP     *Child(void) const    { return _child;}
  BB_LOOP     *Parent(void) const   { return _parent;}
  WN          *Loopstmt(void) const { return _loopstmt; }
  WN          *Index(void) const    { return _index; }
  void         Set_index(WN *p) { _index = p; }
  BB_NODE     *Start(void) const    { return _u1._start; }
  void         Set_start(BB_NODE *p) { _u1._start = p; }
  BB_NODE     *Dohead(void) const   { return _u1._dohead; }
  BB_NODE     *End(void) const      { return _end; }
  void         Set_end(BB_NODE *p) { _end = p; }
  BB_NODE     *Body(void) const     { return _body; }
  void         Set_body(BB_NODE *p) { _body = p; }
  BB_NODE     *Step(void) const     { return _step; }
  void         Set_step(BB_NODE *p) { _step = p; }
  void         Set_merge(BB_NODE *b){ _u2._merge = b; }
  BB_NODE     *Merge(void) const    { return _u2._merge; }
  BB_NODE     *Dotail(void) const   { return _u2._dotail; }
  BB_NODE     *Header(void) const   { return header; }
  void         Set_header(BB_NODE *h)    { header = h; }
  BB_NODE     *Tail(void) const          { return tail; }
  void         Set_tail(BB_NODE *t)      { tail = t; }
  BB_NODE     *Preheader(void) const     { return preheader; }
  void         Set_preheader(BB_NODE *p) { preheader = p; }
  BB_NODE     *Loopback(void) const      { return loopback; }
  void         Set_loopback(BB_NODE *l)  { loopback = l; }
  BB_NODE_SET *Body_set(void) const      { return _body_set; }
  BB_NODE_SET *True_body_set(void) const { return _true_body_set; }
  BOOL         Valid_doloop() const { return _valid_doloop; }
  void         Set_valid_doloop()   { _valid_doloop = TRUE; }
  void         Reset_valid_doloop() { _valid_doloop = FALSE; }
  void         Set_child(BB_LOOP *l){ _child = l; }
  void         Set_parent
                       (BB_LOOP *l) { _parent = l; }
  void         Set_loopstmt(WN *l)  { _loopstmt = l; }
  void         Set_body_set
                   (BB_NODE_SET *s) { _body_set = s; }
  void         Set_true_body_set
                   (BB_NODE_SET *s) { _true_body_set = s; }
  BB_LOOP     *Append (BB_LOOP*);   // insert at tail, return head
  BB_LOOP     *Append_list(BB_LOOP *);
  void         Print(FILE *fp=stderr) const;
  void         Init_bit_vectors(ITABLE *, MEM_POOL *);
  void         Summarize_taltered( void );
  void         Summarize_naltered( void );
  WN          *Orig_wn(void) const  { return _orig_wn; }
  void         Set_orig_wn(WN *wn)  { _orig_wn = wn; }
  BOOL         Promoted_do() const  { return _promoted_do; }
  void         Set_promoted_do()    { _promoted_do = TRUE; }
  BOOL         Well_formed() const  { return well_formed; }
  void         Set_well_formed()    { well_formed = TRUE; }
  void         Reset_well_formed()  { well_formed = FALSE; }
  BOOL         Has_entry_guard() const { return has_entry_guard; }
  void         Set_has_entry_guard() { has_entry_guard = TRUE; }
  void         Reset_has_entry_guard() { has_entry_guard = FALSE; }
  INT          Depth() const { return depth; }
  INT          Max_depth() const { return max_depth; }
  void         Set_depth(INT d) { depth = d; }
  void         Set_max_depth(INT d) { max_depth = d; }

  // Return TRUE if 'this' is parent loop of loop 'l'.
  BOOL         Contains(const BB_LOOP* l) const;

  // wrapper for Invariant_cr_rec (private)
  BOOL         Invariant_cr( CODEREP * ) const;
  // is expr constant relative to index in the loop?
  BOOL         Index_relative_expr( CODEREP *expr,
				    const CODEREP *index ) const;
  
  BOOL  Test_at_entry(void) const { return test_at_entry; }
  BOOL  Test_at_exit(void) const  { return test_at_exit;  }
  BOOL  Exit_early(void) const    { return exit_early; }
  void  Set_test_at_entry(bool b) { test_at_entry = b; }
  void  Set_test_at_exit(bool b)  { test_at_exit = b; }
  void  Set_exit_early(bool b)    { exit_early = b; }
  
  INT   Header_pred_count(void) const  { return header_pred_count; }
  INT   Preheader_pred_num(void) const { return preheader_pred_num; }
  INT   Loopback_pred_num(void) const  { return loopback_pred_num; }
  void  Set_header_pred_count(INT i)  { header_pred_count = i; }
  void  Set_preheader_pred_num(INT i) { preheader_pred_num = i; }
  void  Set_loopback_pred_num(INT i)  { loopback_pred_num = i; }

  // the loop is guarded by an entry test
  BOOL         Loop_is_guarded(void) { return (_flags == LOOP_DO || _flags == LOOP_WHILE); }

  LOOP_FLAGS   Flags(void) const             { return _flags; }
  void         Set_flag(LOOP_FLAGS  f)       { _flags =(LOOP_FLAGS)(_flags|f);}
  BOOL         Is_flag_set(LOOP_FLAGS f)const{ return ((_flags & f) == f); }
  void         Clear_flag(LOOP_FLAGS f)      { _flags = (LOOP_FLAGS) (_flags & ~f); }
  CODEREP     *Iv(void) const                { return _iv; }
  void         Set_iv(CODEREP *cr)           { _iv = cr; }
  CODEREP     *Iv_replacement(void) const    { return _iv_replacement; }
  void         Set_iv_replacement(CODEREP *cr){ _iv_replacement = cr; }
  IDX_32_SET  *Lftr_non_candidates(void) const { return _lftr_non_candidates; }
  void	       Set_lftr_non_candidates(IDX_32_SET *x) { _lftr_non_candidates = x; }
  STMTREP     *Trip_count_stmt(void) const        { return _trip_count_stmt; }
  void         Set_trip_count_stmt(STMTREP *stmt) { _trip_count_stmt = stmt; }
  CODEREP     *Trip_count_expr(void) const        { return _trip_count_expr; }
  void         Set_trip_count_expr(CODEREP *expr) { _trip_count_expr = expr; }
  CODEREP     *Entry_test(void) const        { return _entry_test; }
  void         Set_entry_test(CODEREP *expr) { _entry_test = expr; }
  WN          *Wn_trip_count(void) const     { return _wn_trip_count; }
  void         Set_wn_trip_count(WN *wn)     { _wn_trip_count = wn; }
  INT32        Size_estimate(void) const     { return _size_estimate; }
  void         Set_size_estimate(INT32 n)    { _size_estimate = n; }
  void         Incr_size_estimate(INT32 n)    { _size_estimate += n; }
};

class BB_LOOP_CONTAINER : public SLIST {
private:
  DECLARE_SLIST_CLASS( BB_LOOP_CONTAINER, BB_LOOP )

  BB_LOOP_CONTAINER(const BB_LOOP_CONTAINER&);
  BB_LOOP_CONTAINER& operator = (const BB_LOOP_CONTAINER&);

public:  
  ~BB_LOOP_CONTAINER(void)	{}

  void     Append  (BB_LOOP *loop,     // append loop to the tail
		    MEM_POOL *pool);
};

class BB_LOOP_CONST_ITER : public SLIST_ITER {
  DECLARE_SLIST_CONST_ITER_CLASS( BB_LOOP_CONST_ITER, BB_LOOP, BB_LOOP_CONTAINER )
public:
  void     Init()               {}
};

class BB_LOOP_ITER : public SLIST_ITER {
  DECLARE_SLIST_ITER_CLASS( BB_LOOP_ITER, BB_LOOP, BB_LOOP_CONTAINER )
public:
  void     Init()               {}
};


// a private class for the bb_node class that is used to note which
// blocks a switch statement may jump to.  
// This class is also used for high-whirl IO statements that have
// control flow.
class BB_SWITCH {
  friend class BB_NODE;
  private:
  BB_NODE   *_def_blk;	// default block for switch
  BB_NODE   **_case_blks;	// array of blocks for each case
  INT32     _num_entries;	// how many blocks in case_blks

  BB_SWITCH(void);

  public:
  BB_SWITCH( INT32 num_entries, MEM_POOL *mem_pool ) :
    _num_entries(num_entries)
  { 
    _case_blks = (BB_NODE **)
      OPT_POOL_Alloc(mem_pool, num_entries*sizeof(BB_NODE*), -1);
    _def_blk = NULL;
  }
  ~BB_SWITCH(void)		{}
}; // end private class


// max number of entries for a bb_switch used for IO blocks
enum { IO_TAB_SIZE = 5 };


class BB_LIST : public SLIST_NODE {
private:
  BB_NODE *node;
           BB_LIST(const BB_LIST&);
           BB_LIST& operator = (const BB_LIST&);

public:
           BB_LIST(void)             {}
           BB_LIST(BB_NODE *nd)      {node = nd;}
          ~BB_LIST(void)             {}

  DECLARE_SLIST_NODE_CLASS( BB_LIST )
  BB_LIST *Append  (BB_NODE *bb,      // insert at tail, return head
		    MEM_POOL *pool);

  BB_LIST  *Prepend(BB_NODE *bb, MEM_POOL *pool)
    {
      BB_LIST *new_bblst = (BB_LIST*)CXX_NEW( BB_LIST(bb), pool );
      new_bblst->Set_Next(this);
      return new_bblst;
    }

  BB_LIST *Remove  (BB_NODE *bb,      // remove bb, return head
		    MEM_POOL *pool);
  BOOL     Contains(BB_NODE *bb)const;// test bb_list contains bb
  void     Print   (FILE *fp=stderr)
                   const;             // print the list, just id

  void     Init(BB_NODE *nd)          { node = nd; }
  void     Clear(void)                { node = NULL; }
  INT32    Pos(BB_NODE *bb);
  void     Set_node(BB_NODE *bb)      { node = bb; }

  // Does the list contain more than one distinct BB? For our
  // BB_LISTs, we maintain the invariant that they contain no
  // duplicates. Therefore, the list contains multiple BB's iff its
  // Next() field is non-NULL.
  BOOL     Multiple_bbs(void) const   { return Next() != NULL; }

  // member access functions
  BB_NODE *Node(void) const  {return node;}
};

class BB_LIST_CONTAINER : public SLIST {
private:
  DECLARE_SLIST_CLASS( BB_LIST_CONTAINER, BB_LIST )

  BB_LIST_CONTAINER(const BB_LIST_CONTAINER&);
  BB_LIST_CONTAINER& operator = (const BB_LIST_CONTAINER&);

public:  
  ~BB_LIST_CONTAINER(void)	{};

  void     Append  (BB_NODE *bb,     // append bb to the tail
		    MEM_POOL *pool);
  void     Prepend (BB_NODE *bb,     // prepend bb to the head
		    MEM_POOL *pool);
  void     Remove(BB_NODE *bb,       // remove bb, return head
		  MEM_POOL *pool);
                                     // remove the first BB from the list
  BB_NODE *Remove_head(MEM_POOL *pool);
  BOOL     Contains(BB_NODE *bb)
           const;                    // test bb_list contains bb
};

class BB_LIST_ITER : public SLIST_ITER {
  DECLARE_SLIST_CONST_ITER_CLASS( BB_LIST_ITER, BB_LIST, BB_LIST_CONTAINER )
public:
  void     Init(void)       {}
  void Validate_unique(FILE *fp=stderr);
                                  // validate the uniqueness in the list
  BB_NODE *First_bb(void)   { return (First()) ? Cur()->Node():NULL; }
  BB_NODE *Next_bb(void)    { return (Next())  ? Cur()->Node():NULL; }
  BB_NODE *Cur_bb(void)     { return (Cur())   ? Cur()->Node():NULL; }
  BB_NODE *First_elem(void) { return (First()) ? Cur()->Node():NULL; }
  BB_NODE *Next_elem(void)  { return (Next())  ? Cur()->Node():NULL; }
};

enum BB_FLAG {
  BB_WILLEXIT  = 0x001,	// this block can reach exit block
  BB_REACHED   = 0x002,	// this block is reachable from entry
  BB_DFORDER   = 0x004,	// to indicate whether reaching node is 
			// processed in depth-first ordering
  BB_HASUJP    = 0x008,	// this block has ujp statement
  BB_WNGEND    = 0x010,	// the WN code generated
  BB_HASPRAGMA = 0x020,	// block has pragma statements in it
  BB_IFMERGE   = 0x040,	// merged block for IF/COMPGOTO
  BB_HASCALL   = 0x080,	// block has call in it
  BB_CALLREL   = 0x100,	// block may have call-related code at top
  BB_VN_PROCESSED = 0x200,  // block has been processed by SSA's Value_number()
  BB_MP_REGION = 0x400, // block is inside a MP region
  BB_TLBS_PROCESSING = 0x800, // being processed by Compute_true_loop_body_set
  BB_REGIONEND = 0x1000, // is (or was) the Region_end() of an BB_REGION
#if defined(TARG_SL) //PARA_EXTENSION
  BB_SL2_PARA_REGION = 0x2000, // block is inside a SL2 parallel region
#endif
  BB_EH_REGION = 0x4000,
};

#define BB_VISIT    (BB_DFORDER)

// NOTE: update the array BB_kind_name whenever this type is changed.

enum BB_KIND {
  BB_UNKNOWN,

//  ************************************************************************
//  MUST UPDATE raise_func[] in opt_emit.cxx whenever BB_KIND is changed!!!
//  ************************************************************************

  BB_GOTO,		// single target BB
  BB_LOGIF,		// logical if
  BB_VARGOTO,		// variable goto
  BB_ENTRY,		// the entry bb
  BB_EXIT,		// the exit bb
  BB_DOSTART,		// init block
  BB_DOEND,		// ending condition
  BB_DOSTEP,		// increment
  BB_DOHEAD,		// dohead block
  BB_DOTAIL,		// dotail block
  BB_IO,		// io block
  BB_WHILEEND,		// ending condition for while statement
  BB_REGIONSTART,	// starting block for a region
  BB_REGIONEXIT,	// exit block for a region (goto)
  BB_REPEATBODY,	// start block for repeat statement
  BB_REPEATEND,		// ending condition for repeat statement
  BB_SUMMARY		// summary BB
};
#endif  // opt_bb_INCLUDED

#if defined(opt_bb_CXX) && !defined(BB_kind_name_DEFINED)
#define BB_kind_name_DEFINED "BB_kind_name"
static const char *BB_kind_name[] = {
  "UNKNOWN",
  "GOTO",
  "LOGIF",
  "VARGOTO",
  "ENTRY",
  "EXIT",
  "DOSTART",
  "DOEND",
  "DOSTEP",
  "DOHEAD",
  "DOTAIL",
  "IO",
  "WHILEEND",
  "REGIONSTART",
  "REGIONEXIT",
  "REPEATBODY",
  "REPEATEND",
  "SUMMARY"
};
#endif // opt_bb_CXX

#ifndef opt_bb_INCLUDED
#define opt_bb_INCLUDED	"opt_bb.h"
class BB_NODE : public CHAIN_NODE {
  DECLARE_CHAIN_NODE_CLASS( BB_NODE )
private:
  IDTYPE       _id;          // number according to order of occurrence
			     // in proc
  INT          _loopdepth;
  mUINT16      _rid_id;      // which region this BB belongs to
  BB_FLAG      _flags;       // see BB_FLAG for flags it contains
  BB_KIND      _kind;
  BB_LIST     *_pred;        // predecessor list
  BB_LIST     *_succ;        // successor list
  BB_NODE     *_idom;        // point to immediate dominator block
  BB_NODE     *_ipdom;       // point to immediate post-dominator block
  BB_LIST     *_dom_bbs;     // list of blocks that are dominated
  BB_LIST     *_pdom_bbs;    // list of blocks that are post-dominated
  IDTYPE       _dom_dfs_id;  //	number in preorder dominator tree walk
  IDTYPE       _dom_dfs_last;// _dom_dfs_id number of last dominated block
  IDTYPE       _pdom_dfs_id; // number in preorder post-dominator tree walk
  IDTYPE       _pdom_dfs_last;// _pdom_dfs_id number of last post-dominated block
  BB_NODE_SET *_dom_frontier;// point to dominance frontier set
  BB_NODE_SET *_rcfg_dom_frontier;// point to dominance frontier set for reverse CFG
  IDTYPE       _layout_id;          // it is used to represent where to place BB in reconstrucing CFG
  RID         *_rid;          // which region BB belongs to, cannot use _rid_id because it may not be unique
  
  // structure containing everything related to the label for this bb
  struct {
    mINT32     _labnam;      // label number in the WN node
    WN        *_lab_loop_info;//opc_loop_info wn attached to label
  } _labels;
  WN          *_firststmt;   // pt to first statement node of block, used only before enter into htable and at emit time
  WN          *_laststmt;    // pt to last statement node of block, same usage as firststmt
  STMT_LIST    _stmtlist;    // list of internal statement representation
  BB_LOOP     *_innermost;   // the innermost loop this BB belongs to
  BB_LOOP     *_loop;        // DO loop information
  union {
    BB_IFINFO *_ifinfo;      // If statement information
    BB_SWITCH *_switchinfo;  // comp_goto (switch) stmt information
    BB_SWITCH *_ioinfo;	     // io stmt info about bbs may branch to
    WN        *_entrywn;     // wn for entry block, valid for BB_ENTRY
    BB_REGION *_regioninfo;  // region information, valid for BB_REGIONSTART,
			     // BB_REGIONEXIT, and BB_EXIT
    // NOTE: a region does not have _entrywn because it has no parameters
  } _hi;
  			     // and regions
  PHI_LIST    *_phi_list;    // pt to list of phi nodes
  EXP_PHI_LIST
              *_iphi_list;   // pt to list of EXP_OCCURS (IPHI) nodes

  SRCPOS       _linenum;     // the source position starting this block
  UINT32       _freq;	     // frequency info (from profile feedback)

  union {
    IDTYPE      _rpo_id;     // number in reverse postorder CFG walk
  } _u0;

  // dataflow equation sets and PRE/SSA related data structure
  union {
    IDX_32_SET *_loc_appear; // used for RVI (def'd or ref'd in bb)
  } _u1;
  union {
    IDX_32_SET *_loc_def;    // used for RVI (def'd in bb)
  } _u2;
  union {
    EXP_PHI    *_exp_phi;    // exp phi, used in the PRE/SSA phase
    IDX_32_SET *_loc_upwd;   // used for RVI (local upward exposed use)
  } _u3;
  union {
    IDX_32_SET *_defreach;   // used for RVI (def reaches entry of bb)
  } _u4;
  union {
    IDX_32_SET *_live_at_exit;// used for RVI (live-in to successors)
  } _u5;
  union {
    IDX_32_SET *_live_out;   // used for RVI (poss. aliased use in succ)
  } _u6;
  union {
    IDX_32_SET *_loc_mu_ref; // used for RVI (local aliased ref)
  } _u7;
  union {
    IDX_32_SET *_loc_chi_def;// used for RVI (local aliased def)
  } _u8;
  union {
    WN *_loc_mu_wn;    	     // used for RVI (wn with mu ref)
  } _u9;
  union {
    RVI_ANN_LIST *_rvi_anns; // used for RVI (annotations)
  } _u10;
  union {
    IDX_32 _last_stid_bitpos;// used for RVI
  } _u11;
  union {
    BOOL _last_stid_has_chi; // used for RVI
  } _u12;
  union {
    IDX_32_SET *_unstored_defs;// used for RVI (unstored defs)
  } _u13;

  	       BB_NODE& operator = (const BB_NODE&);

public:
  void         Clear(void);

  BB_NODE(void)                            { Clear(); }
  BB_NODE(IDTYPE i)                        { Clear(); _id = i; }
  BB_NODE(const BB_NODE&);

  ~BB_NODE(void)                           {}

  void         Print(FILE *fp=stderr) const;// print the bb structure
  void         PrintVis(void) const;            // print the bb structure
  void         Print_wn(FILE *fp=stderr) const;  // print the bb structure
  void         Print_ssa(FILE *fp=stderr) const; // print the bb structure
  void         Print_head(FILE *fp=stderr) // print the cfg info of the bb
               const;
  void         Print_locattr(FILE
                             *fp=stderr);  // print the local attributes
  void         Validate(FILE *fp = stderr) // validate assumptions in bb
               const;
  void         Gen_wn(EMITTER *, BOOL copy_phi=TRUE); // generate the WHIRL stmt
		                           // list from the stmtlist
  void	       Gen_insertions(MAIN_EMITTER *); // process inserts at end of BB
  WN          *Find_outermost_loopstmt     // Find the outermost
                      (CODEREP *cr) const; // loopstmt that has phi
                                           // function for the same
                                           // aux_id of CODEREP
                                           // 'cr'. defined in opt_du.cxx

  void         Append_pred(BB_NODE *bb,
			   MEM_POOL *pool) { _pred = _pred->Append(bb,pool); }
  void         Append_succ(BB_NODE *bb,
			   MEM_POOL *pool) { _succ = _succ->Append(bb,pool); }
  void         Prepend_pred(BB_NODE * bb, MEM_POOL * pool) { _pred = _pred->Prepend(bb, pool); }
  void         Prepend_succ(BB_NODE *bb, MEM_POOL * pool) { _succ = _succ->Prepend(bb, pool); }
  void         Remove_pred(BB_NODE *bb, MEM_POOL *pool) 
			{ if (_pred != NULL)
			    _pred = _pred->Remove(bb,pool); 
			}
  void         Remove_succ(BB_NODE *bb, MEM_POOL *pool) 
			{ if (_succ != NULL)
			    _succ = _succ->Remove(bb,pool);
			}
  void         Remove_preds(MEM_POOL * pool);
  void         Remove_succs(MEM_POOL * pool);
  void         Replace_pred( BB_NODE *old_pred, BB_NODE *new_pred );
  void         Replace_succ( BB_NODE *old_succ, BB_NODE *new_succ );

  void         Connect_wn(WN *wn)
               { STMT_CONTAINER stmt_cont(Firststmt(), Laststmt());
		 stmt_cont.Append(wn);
	       }

  // Find the coderep with the same bit position as the 'cr'
  CODEREP     *Find_cr(const CODEREP *cr);	// (opt_find.cxx)
  // Find the coderep that is the definition point of the given 'cr'
  CODEREP     *Find_def(const CODEREP *cr);	// (opt_find.cxx)

  // Find the coderep that is the definition point of the given VAR (by matching the auxid)
  CODEREP     *Find_def_by_auxid(const CODEREP *cr);

  // add wn after after_this (if after_this is null, inserts at the 
  // beginning of the block)
  void         Insert_wn_after( WN *wn, WN *after_this );
  // add wn before before_this (if before_this is null, inserts at the 
  // end of the block)
  void         Insert_wn_before( WN *wn, WN *before_this );
  // append wn to end of bb, but before any branch
  void         Append_wn_before_branch(WN *wn);
  // prepend wn to start of bb, but after labels, pragmas, etc.
  void         Prepend_wn_after_labels(WN *wn);

  void         Connect_wn_list(WN *wn_f, WN *wn_l)
               { STMT_CONTAINER stmt_cont(Firststmt(), Laststmt());
		 stmt_cont.Append_list(wn_f, wn_l);
	       }

  void         Add_pragma(WN *wn, STMTREP *stmt, MEM_POOL *pool);
  BOOL         Is_first(void)    const    {return Prev() == NULL;}
  BOOL         Is_last(void)     const    {return Next() == NULL;}

  WN          *Get_do_start(void) const;  // for preopt, do loop access
  WN          *Get_do_end(void)   const;  // for preopt, do loop access
  WN          *Get_do_step(void)  const;  // for preopt, do loop access

  // member access functions
  IDTYPE       Id(void)          const  { return _id;}
  void         Set_id(IDTYPE i )        { _id = i;}
  IDTYPE       Rpo_id(void) const       { return _u0._rpo_id; }
  void         Set_rpo_id(IDTYPE id)    { _u0._rpo_id = id; }
  mUINT8       Loopdepth(void)   const  { return _loopdepth;}
  void         Set_loopdepth(mUINT8 dep){ _loopdepth = dep;}
  mUINT16      Rid_id(void)         const  { return _rid_id;}
  void         Set_rid_id(mUINT16 id)   { _rid_id = id;}
  RID          *Rid(void)          const{ return _rid;}
  void         Set_rid(RID* rid)        { _rid = rid; }
  BB_FLAG      Flag(void)        const  { return _flags; }
  BOOL         Willexit(void)    const  { return (_flags & BB_WILLEXIT);}
  void         Set_willexit(void)       { _flags=(BB_FLAG)(_flags|BB_WILLEXIT);}
  void         Reset_willexit(void)     { _flags=(BB_FLAG)(_flags&~BB_WILLEXIT);}
  void         Reset_visit(void)        { _flags=(BB_FLAG)(_flags&~BB_VISIT);}
  void         Set_flag(INT32 f)        { _flags=(BB_FLAG)(_flags|f);}
  BOOL         Wngend(void)    const    { return (_flags & BB_WNGEND);}
  void         Set_wngend(void)         { _flags=(BB_FLAG)(_flags|BB_WNGEND);}
  void         Reset_wngend(void)       { _flags=(BB_FLAG)(_flags&~BB_WNGEND);}
  BOOL         Reached(void)     const  { return (_flags & BB_REACHED);}
  void         Set_reached(void )       { _flags=(BB_FLAG)(_flags|BB_REACHED);}
  void         Reset_reached(void )     { _flags=(BB_FLAG)(_flags&~BB_REACHED);}
  BOOL         Dforder(void)     const  { return (_flags & BB_DFORDER);}
  void         Set_dforder(void )       { _flags=(BB_FLAG)(_flags|BB_DFORDER);}
  void         Reset_dforder(void )     { _flags=(BB_FLAG)(_flags&~BB_DFORDER);}
  BOOL         Hasujp(void)      const  { return (_flags & BB_HASUJP);}
  void         Set_hasujp(void)         { _flags=(BB_FLAG)(_flags|BB_HASUJP);}
  void         Reset_hasujp(void)       { _flags=(BB_FLAG)(_flags&~BB_HASUJP);}
  void         Set_ifmerge(void)        { _flags=(BB_FLAG)(_flags|BB_IFMERGE);}
  void         Reset_ifmerge(void)      { _flags=(BB_FLAG)(_flags&~BB_IFMERGE);}
  BOOL         Ifmerge(void)     const  { return (_flags & BB_IFMERGE); }
  BOOL         Haspragma(void)   const  { return (_flags & BB_HASPRAGMA);}
  void         Set_haspragma(void)      { _flags=(BB_FLAG)(_flags|BB_HASPRAGMA);}
  void         Reset_haspragma(void)    { _flags=(BB_FLAG)(_flags&~BB_HASPRAGMA);}
  BOOL         Hascall(void)   const    { return (_flags & BB_HASCALL);}
  void         Set_hascall(void)        { _flags=(BB_FLAG)(_flags|BB_HASCALL);}
  void         Reset_hascall(void)      { _flags=(BB_FLAG)(_flags&~BB_HASCALL);}
  BOOL         VN_processed(void)const  { return (_flags & BB_VN_PROCESSED);}
  void         Set_vn_processed(void)   { _flags=(BB_FLAG)(_flags|BB_VN_PROCESSED);}
  BOOL         MP_region(void)   const  { return (_flags & BB_MP_REGION);}
  void         Set_MP_region(void)      { _flags=(BB_FLAG)(_flags|BB_MP_REGION);}
  void         Reset_MP_region(void)    { _flags=(BB_FLAG)(_flags&~BB_MP_REGION);}

  BOOL         EH_region(void)   const  { return (_flags & BB_EH_REGION);}
  void         Set_EH_region(void)      { _flags=(BB_FLAG)(_flags|BB_EH_REGION);}
  void         Reset_EH_region(void)    { _flags=(BB_FLAG)(_flags&~BB_EH_REGION);}

  IDTYPE       layout_Id(void)          const  { return _layout_id;}
  void         Set_layout_id(IDTYPE i )        { _layout_id = i;}
  void         Set_layout_id(BB_NODE * node)   { _layout_id = node->layout_Id() ? node->layout_Id() : node->Id(); }
    
#if defined(TARG_SL) //PARA_EXTENSION
  BOOL         SL2_para_region(void) const    { return (_flags & BB_SL2_PARA_REGION);}
  void         Set_SL2_para_region(void)      { _flags=(BB_FLAG)(_flags|BB_SL2_PARA_REGION);}
  void         Reset_SL2_para_region(void)    { _flags=(BB_FLAG)(_flags&~BB_SL2_PARA_REGION);}
  BOOL         Parallel_Region(void)    { return (SL2_para_region() || MP_region()); }
#else 
  BOOL         Parallel_Region(void)    { return  MP_region(); }
#endif  
  BOOL         TLBS_processing(void) const { return (_flags & BB_TLBS_PROCESSING);}
  void         Set_TLBS_processing(void) { _flags=(BB_FLAG)(_flags|BB_TLBS_PROCESSING);}
  void         Reset_TLBS_processing(void)    { _flags=(BB_FLAG)(_flags&~BB_TLBS_PROCESSING);}
  BOOL         Callrel(void)   const    { return (_flags & BB_CALLREL);}
  void         Set_callrel(void)        { _flags=(BB_FLAG)(_flags|BB_CALLREL);}
  void         Reset_callrel(void)      { _flags=(BB_FLAG)(_flags&~BB_CALLREL);}
  BOOL         Regionend(void)   const    { return (_flags & BB_REGIONEND);}
  void         Set_regionend(void)        { _flags=(BB_FLAG)(_flags|BB_REGIONEND);}
  void         Reset_regionend(void)      { _flags=(BB_FLAG)(_flags&~BB_REGIONEND);}

  BB_KIND      Kind(void)        const  { return _kind;}
  void         Set_kind(BB_KIND k)      { _kind = k; }

  const char  *Kind_name(void)   const;
  BB_LIST     *Pred(void)        const  { return _pred;}
  void         Set_pred(BB_LIST *bblst) { _pred = bblst;}
  BB_NODE     *Nth_pred(INT32)   const;
  BB_NODE     *Nth_succ(INT32)   const;
  BB_NODE     *Last_succ();
  BB_LIST     *Succ(void)        const  { return _succ;}
  void         Set_succ(BB_LIST *bblst) { _succ = bblst;}

  // Dominance and postdominance
  BB_NODE     *Idom(void)        const  { return _idom; }
  void	       Set_idom(BB_NODE *domin) { _idom = domin; }
  BB_NODE     *Ipdom(void) 	 const	{ return _ipdom; }
  void	       Set_ipdom(BB_NODE *pdomin){ _ipdom = pdomin; }
  BB_LIST     *Dom_bbs(void) 	 const	{ return _dom_bbs; }
  void         Set_dom_bbs(BB_LIST *bblst){ _dom_bbs = bblst; }
  void         Add_dom_bbs(BB_NODE *bb,
			   MEM_POOL *p) { _dom_bbs = _dom_bbs->Append(bb,p); }
  BB_LIST     *Pdom_bbs(void) 	 const	{ return _pdom_bbs; }
  void         Set_pdom_bbs
                        (BB_LIST *bblst){ _pdom_bbs = bblst; }
  void         Add_pdom_bbs(BB_NODE *bb,
			    MEM_POOL *p){ _pdom_bbs = _pdom_bbs->Append(bb,p);}
  IDTYPE       Dom_dfs_id(void) const   { return _dom_dfs_id; }
  IDTYPE       Dom_dfs_last(void) const { return _dom_dfs_last; }
  void         Set_dom_dfs_id(IDTYPE  id)   { _dom_dfs_id = id; }
  void         Set_dom_dfs_last(IDTYPE  id) { _dom_dfs_last = id; }
  IDTYPE       Pdom_dfs_id(void) const   { return _pdom_dfs_id; }
  IDTYPE       Pdom_dfs_last(void) const { return _pdom_dfs_last; }
  void         Set_pdom_dfs_id(IDTYPE  id)   { _pdom_dfs_id = id; }
  void         Set_pdom_dfs_last(IDTYPE  id) { _pdom_dfs_last = id; }
  mUINT32      Dom_descendant_cnt(void) const 
                              { return _dom_dfs_last - _dom_dfs_id; }
  mUINT32      Pdom_descendant_cnt(void) const 
                              { return _pdom_dfs_last - _pdom_dfs_id; }

  BOOL         Dominates_strictly( const BB_NODE *bb ) const {
    Is_True(bb != NULL, ("BB_NODE::Dominates_strictly(BB%u, NULL)", Id()));
    return ( bb->Dom_dfs_id() >  Dom_dfs_id() &&
	     bb->Dom_dfs_id() <= Dom_dfs_last() ); }

  BOOL         Postdominates_strictly( const BB_NODE *bb ) const {
    Is_True(bb != NULL, ("BB_NODE::Postdominates_strictly(BB%u, NULL)", Id()));
    return ( bb->Pdom_dfs_id() >  Pdom_dfs_id() &&
	     bb->Pdom_dfs_id() <= Pdom_dfs_last() ); }

  BOOL         Dominates( const BB_NODE *bb ) const {
    Is_True(bb != NULL, ("BB_NODE::Dominates(BB%u, NULL)", Id()));
    return ( bb->Dom_dfs_id() >= Dom_dfs_id() &&
	     bb->Dom_dfs_id() <= Dom_dfs_last() ); }

  BOOL         Postdominates( const BB_NODE *bb ) const {
    Is_True(bb != NULL, ("BB_NODE::Postdominates(BB%u, NULL)", Id()));
    return ( bb->Pdom_dfs_id() >= Pdom_dfs_id() &&
	     bb->Pdom_dfs_id() <= Pdom_dfs_last() ); }

  // Which bb does *this fall through to?
  const BB_NODE *Falls_thru_to(void) const;

  BB_NODE_SET *Dom_frontier(void)const  { return _dom_frontier; }
  void	       Set_dom_frontier
                       (BB_NODE_SET *b) { _dom_frontier = b; }
  BB_NODE_SET *Rcfg_dom_frontier
                       (void) const     { return _rcfg_dom_frontier; }
  void	       Set_rcfg_dom_frontier
                       (BB_NODE_SET *b) { _rcfg_dom_frontier = b; }
  void         Compute_rcfg_itrdom_frontier( BB_NODE_SET *itrcd) const;
  // label-related methods
  INT32        Labnam(void)      const  { return _labels._labnam;}
  void         Set_labnam(mUINT32 l )   { _labels._labnam = l;}
  WN	      *Label_loop_info(void) const
		{ return _labels._lab_loop_info; }
  void	       Set_label_loop_info(WN *li)
		{ _labels._lab_loop_info = li; }

  SRCPOS       Linenum(void)	 const  { return _linenum; }
  void         Set_linenum(SRCPOS ln)	{ _linenum = ln; }
  UINT32       Freq(void)	 const  { return _freq; }
  void         Set_freq(UINT32 n)	{ _freq = n; }

  // PRE/SSA related data structure
  EXP_PHI     *Exp_phi(void) const      { return _u3._exp_phi; }
  void         Set_exp_phi(EXP_PHI *ep) { _u3._exp_phi = ep; }

  // dataflow equation sets
  // local attributes
  // calculated sets
  // rvi local attributes
  IDX_32_SET *Loc_appear(void) const	{ return _u1._loc_appear; }
  IDX_32_SET *Loc_def(void) const	{ return _u2._loc_def; }
  IDX_32_SET *Loc_upwd(void) const	{ return _u3._loc_upwd; }
  // rvi calculated sets
  IDX_32_SET *Defreach(void) const	{ return _u4._defreach; }
  IDX_32_SET *Live_at_exit(void) const	{ return _u5._live_at_exit; }
  IDX_32_SET *Live_out(void) const	{ return _u6._live_out; }
  IDX_32_SET *Loc_mu_ref(void) const	{ return _u7._loc_mu_ref; }
  IDX_32_SET *Loc_chi_def(void) const	{ return _u8._loc_chi_def; }
  WN *Loc_mu_wn(void) const		{ return _u9._loc_mu_wn; }
  RVI_ANN_LIST *Rvi_anns(void) const	{ return _u10._rvi_anns; }
  IDX_32 Last_stid_bitpos(void) const	{ return _u11._last_stid_bitpos;}
  BOOL Last_stid_has_chi(void) const	{ return _u12._last_stid_has_chi;}
  IDX_32_SET *Unstored_defs(void) const	{ return _u13._unstored_defs;}

  // rvi sets
  void Set_loc_appear(IDX_32_SET *set)	{ _u1._loc_appear = set; }
  void Set_loc_def(IDX_32_SET *set)	{ _u2._loc_def = set; }
  void Set_loc_upwd(IDX_32_SET *set)	{ _u3._loc_upwd = set; }
  void Set_defreach(IDX_32_SET *set)	{ _u4._defreach = set; }
  void Set_live_at_exit(IDX_32_SET *set){ _u5._live_at_exit = set; }
  void Set_live_out(IDX_32_SET *set)	{ _u6._live_out = set; }
  void Set_loc_mu_ref(IDX_32_SET *set)	{ _u7._loc_mu_ref = set; }
  void Set_loc_chi_def(IDX_32_SET *set)	{ _u8._loc_chi_def = set; }
  void Set_loc_mu_wn(WN *mwn)		{ _u9._loc_mu_wn = mwn; }
  void Set_rvi_anns(RVI_ANN_LIST *ral)	{ _u10._rvi_anns = ral; }
  void Set_last_stid_bitpos(IDX_32 i)	{ _u11._last_stid_bitpos = i;}
  void Set_last_stid_has_chi(BOOL b)	{ _u12._last_stid_has_chi = b;}
  void Set_unstored_defs(IDX_32_SET *s) { _u13._unstored_defs = s;}

  // The following 2 dataflow equation computation are defined in
  // opt_dfeqn.cxx 
  void Set_subinsert  ( CODEREP *cr );  // set subinsert for the subtree
  void Reset_subdelete( CODEREP *cr );  // Reset subdelete for the subtree

  WN          *Firststmt(void)   const  { return _firststmt;}
  void         Set_firststmt(WN *stmt)  { _firststmt = stmt; }
  WN          *Laststmt(void)    const  { return _laststmt;}
  void         Set_laststmt(WN *stmt)   { _laststmt = stmt; }
  void         Insert_last(WN *);       // insert around the last
					// statement. in opt_emit.cxx
  void         Init_stmt(WN *stmt)      { _firststmt = _laststmt = stmt; }
  // determine if there is a branch statement ending this block
  WN          *Branch_wn(void) const;
  // determine if there is a label statement starting this block
  WN          *Label_wn(void) const;
  BB_LOOP     *Innermost(void) const    { return _innermost; }
  void         Set_innermost(BB_LOOP *i){ _innermost = i; }
  WN          *Entrywn(void) const      { Is_True(Kind() == BB_ENTRY,
						  ("BB is not entry_BB"));
					  return _hi._entrywn; }
  void         Set_entrywn(WN *wn)      { _hi._entrywn = wn; }
  BB_LOOP     *Loop(void)    const      { return _loop;}
  void         Set_loop(BB_LOOP *li)    { _loop = li; }
  void         Print_loop(BB_LOOP *l);
  WN          *Loopindex(void) const    { Is_True(Kind() != BB_LOGIF,
						  ("Illegal use of loop"));
					  return (_loop)?
					    _loop->Index() : NULL; }
  WN          *Loopstmt(void) const     { Is_True(Kind() != BB_LOGIF,
						  ("Illegal use of loop"));
					  return (_loop)?
					    _loop->Loopstmt() : NULL; }
  void         Set_loopstmt(WN *loop)   { Is_True(Kind() != BB_LOGIF,
						  ("Illegal use of loop"));
                                          _loop->Set_loopstmt(loop); }
  BB_NODE_SET *Loopbodyset(void) const  { Is_True(Kind() != BB_LOGIF,
						  ("Illegal use of loop"));
					  return (_loop)?
					    _loop->Body_set() : NULL; }
  void         Set_loopbodyset
                  (BB_NODE_SET *s)      { Is_True(Kind() != BB_LOGIF,
						  ("Illegal use of loop"));
					  if (_loop)
					    _loop->Set_body_set(s); }
  BB_NODE     *Loopstart(void) const    { Is_True(Kind() != BB_LOGIF,
						  ("Illegal use of loop"));
					  return (_loop)?
					    _loop->Start() : NULL; }
  BB_NODE     *Loophead(void) const     { Is_True(Kind() != BB_LOGIF,
						  ("Illegal use of loop"));
					  return (_loop)?
					    _loop->Dohead() : NULL; }
  BB_NODE     *Loopend(void) const      { Is_True(Kind() != BB_LOGIF,
						  ("Illegal use of loop"));
					  return (_loop)?
					    _loop->End() : NULL; }
  BB_NODE     *Loopbody(void) const     { Is_True(Kind() != BB_LOGIF,
						  ("Illegal use of loop"));
					  return (_loop)?
					    _loop->Body() : NULL; }
  BB_NODE     *Loopstep(void) const     { Is_True(Kind() != BB_LOGIF,
						  ("Illegal use of loop"));
					  return (_loop)?
					    _loop->Step() : NULL; }
  BB_NODE     *Loopmerge(void) const    { Is_True(Kind() != BB_LOGIF,
						  ("Illegal use of loop"));
					  return (_loop)?
					    _loop->Merge() : NULL; }
  BB_NODE     *Looptail(void) const     { Is_True(Kind() != BB_LOGIF,
						  ("Illegal use of loop"));
					  return (_loop)?
					    _loop->Dotail() : NULL; }
  BB_IFINFO   *Ifinfo(void)    const    { return (Kind() == BB_LOGIF || Ifmerge()) ? 
                                              _hi._ifinfo : NULL ; }
  void         Set_ifinfo(BB_IFINFO
			    *ii)        { Is_True((Kind() == BB_LOGIF) || Ifmerge(),
						  ("Illegal use of ifinfo"));
					  _hi._ifinfo = ii; }
  SRCPOS       Then_loc(void) const     { Is_True(Kind() == BB_LOGIF,
						  ("Illegal use of ifinfo"));
					  return (_hi._ifinfo)?
					    _hi._ifinfo->Thenloc() : 0; }
  SRCPOS       Else_loc(void) const     { Is_True(Kind() == BB_LOGIF,
						  ("Illegal use of ifinfo"));
					  return (_hi._ifinfo)?
					    _hi._ifinfo->Elseloc() : 0; }
  BB_NODE     *If_cond(void) const      { Is_True(Kind() == BB_LOGIF || Ifmerge(),
						  ("Illegal use of ifinfo"));
					  return (_hi._ifinfo)?
					    _hi._ifinfo->Cond() : NULL; }
  BB_NODE     *If_then(void) const      { Is_True(Kind() == BB_LOGIF,
						  ("Illegal use of ifinfo"));
					  return (_hi._ifinfo)?
					    _hi._ifinfo->Then() : NULL; }
  BB_NODE     *If_else(void) const      { Is_True(Kind() == BB_LOGIF,
						  ("Illegal use of ifinfo"));
					  return (_hi._ifinfo)?
					    _hi._ifinfo->Else() : NULL; }
  BB_NODE     *If_merge(void) const     { Is_True(Kind() == BB_LOGIF,
						  ("Illegal use of ifinfo"));
					  return (_hi._ifinfo)?
					    _hi._ifinfo->Merge() : NULL; }

  BB_SWITCH   *Switchinfo(void) const   { Is_True(Kind() == BB_VARGOTO,
					  ("Illegal use of switchinfo"));
					  return _hi._switchinfo;}
  void         Set_switchinfo
                        (BB_SWITCH *si) { Is_True(Kind() == BB_VARGOTO,
					  ("Illegal use of switchinfo"));
					  _hi._switchinfo = si;}
  BB_NODE     *Switchcase
                 (INT32 case_elt) const	{ Is_True(Kind() == BB_VARGOTO,
					  ("Illegal use of switchinfo"));
					  return
					    _hi._switchinfo->_case_blks[case_elt];}
  void         Set_switchcase
                 (BB_NODE *bb,
		  INT32 case_elt )      { _hi._switchinfo->_case_blks[case_elt]
					    = bb; }
  BB_NODE     *Switchdefault(void) const{ return _hi._switchinfo->_def_blk; }
  void         Set_switchdefault
                 ( BB_NODE *default_bb ){ _hi._switchinfo->_def_blk=default_bb; }
  INT32        Switchentries(void) const{ return _hi._switchinfo->_num_entries; }

  BB_SWITCH   *IOinfo(void) const
			{ Is_True(Kind() == BB_IO,
			    ("Illegal use of IOinfo"));
			  return _hi._ioinfo;
			}
  void         Set_ioinfo (BB_SWITCH *ioi)
			{ Is_True(Kind() == BB_IO,
			    ("Illegal use of IOinfo"));
			  _hi._ioinfo = ioi;
			}
  BB_NODE     *IO_bb (INT32 i) const
			{ Is_True(Kind() == BB_IO,
			    ("Illegal use of IOinfo"));
			  return _hi._ioinfo->_case_blks[i];}
  void         Set_io_bb (BB_NODE *bb, INT32 i )
			{ Is_True( i < IO_TAB_SIZE, ("Too many BBs") );
			  _hi._ioinfo->_case_blks[i] = bb;
			}
  void	       Set_io_entries( INT32 num_entries )
			{ Is_True( num_entries <= IO_TAB_SIZE,
			    ("Too many IO control statements") );
			  _hi._ioinfo->_num_entries = num_entries;
			}
  INT32        IO_entries(void) const
			{ return _hi._ioinfo->_num_entries; }

  // note: Regioninfo() and Set_regioninfo are only valid for BB_REGIONSTART,
  // BB_REGIONEXIT, and BB_EXIT nodes. For BB_ENTRY nodes, go use the Rid()
  // in the cfg.
  BB_REGION   *Regioninfo(void) const
			{ Is_True(Kind() == BB_REGIONSTART ||
				  Kind() == BB_REGIONEXIT ||
				  Kind() == BB_EXIT,
			    ("Illegal use of Regioninfo, %s",Kind_name()) );
			  return _hi._regioninfo;
			}
  void         Set_regioninfo(BB_REGION *region)
			{ Is_True(Kind() == BB_REGIONSTART ||
				  Kind() == BB_REGIONEXIT ||
				  Kind() == BB_EXIT,
			    ("Illegal use of Regioninfo, %s",Kind_name()) );
			  _hi._regioninfo = region;
			}

  //
  // methods to deal with stmtrep representation
  //

  const STMT_LIST *Stmtlist(void) const	{ return &_stmtlist; }
  STMT_LIST   *Stmtlist(void) 		{ return &_stmtlist; }
  const STMTREP *First_stmtrep(void) const { return Stmtlist()->Head(); }
  STMTREP *First_stmtrep(void)		{ return Stmtlist()->Head(); }
  const STMTREP *Last_stmtrep(void) const { return Stmtlist()->Tail(); }
  STMTREP     *Last_stmtrep(void)	{ return Stmtlist()->Tail(); }

  STMTREP     *Add_stmtnode(WN*,        // build/append a statement node
			    MEM_POOL*); // based on the WHIRL

  void         Append_stmtrep(STMTREP*);// add a stmtrep to the end of
					// a block, but before any
					// ending branch
  void         Prepend_stmtrep          // add a stmtrep to the
                           (STMTREP *s);// beginning of a block, but
					// after any label

  void         Remove_stmtrep(STMTREP*);// remove a statement
  void         Remove_stmt(WN *); // remove a statement
  void         Unlink_stmt(WN *); // unlink a statement
  void         Prepend_stmt(WN *); // add a statement to the beginning of a block,
                                   // but after any non-executable statements.

  void	       Append_stmt_before_branch(STMTREP *stmt);

  STMTREP     *Branch_stmtrep(void);	// check if the block has a
					// branch stmt_rep, and return it

  STMTREP     *Label_stmtrep(void);	// check if the block has a
					// label stmt_rep, and return it
  
  void         Add_label(CFG *cfg);
  void         Remove_label_stmtrep();
  void         Add_label_stmtrep        // add a label stmt_rep if it
                     (MEM_POOL *pool);  // does not have one


  PHI_LIST    *Phi_list(void)    const  { return _phi_list; }
  void         Set_phi_list(PHI_LIST *p){ _phi_list = p; }
  void         Remove_phi_reference( INT32 whichpred );
  BOOL         Has_valid_phi();
  BOOL         Only_fall_through_phi();

  EXP_PHI_LIST
              *Iphi_list(void)   const  { return _iphi_list; }
  void         Set_iphi_list(EXP_PHI_LIST *i)
                                        { _iphi_list = i; }

  void         Insert_tree(CODEREP *);  // add a tree to the insertions and subinsert bitvector

  void         Insert_stmtrep_before(STMTREP *, STMTREP *);
  void         Insert_stmtrep_after(STMTREP *, STMTREP *);
  BOOL         Update_freq_from_WN(const WN*);
  void         Set_next(BB_NODE *bb) { Set_Next(bb); }
  void         Set_prev(BB_NODE *bb) { Set_Prev(bb); }
  BOOL         Is_empty();

  // Can this BB be cloned?
  BOOL         Clonable(BOOL           allow_loop_cloning, 
			const BVECTOR *cr_vol_map = NULL,
			BOOL allow_clone_calls=FALSE);
  INT32        Code_size_est(void) const;

  // Does this BB dominate every BB in the given  SC_NODE?
  BOOL Is_dom(SC_NODE *);
  // Does this BB post-dominate every BB in the given SC_NODE?
  BOOL Is_postdom(SC_NODE *);
  // Is every pair of WN statements in this BB_NODE and the given BB_NODE identical?
  BOOL Compare_Trees(BB_NODE *);

  // Count of executable statements in this BB_NODE.
  int  Executable_stmt_count();
  // Does this BB_NODE end with a branch targeting the given BB_NODE?
  BOOL Is_branch_to(BB_NODE *);
  // Find first executable statement in this BB_NODE.
  WN * First_executable_stmt();

#if defined(TARG_SL)
  // if predecessors of current bb are from different region, this function  
  // is used to set expression phi not downsafe to prevent code motion 
  BOOL  Preds_or_succs_from_different_region(BOOL is_pred = TRUE); 
#endif
}; // end BB_NODE class



// a private class for bb_node class that is used to note which
// blocks are in a region
class BB_REGION {
  friend class BB_NODE;

private:
  BB_NODE   *_region_start;	// the start of the region
  BB_NODE   *_region_end;	// last block in the region
  RID       *_rid;		// the RID info
  BB_REGION *_parent;		// the BB_REGION of parent
  WN	    *_region_exits;	// region exit table
  WN	    *_region_pragmas;	// region pragma list
  WN        *_orig_wn;		// original WN node
  INT32	     _region_num_exits;	// number of exits in exit table
  INT64	     _region_line_num;	// line number of region
  INITO_IDX  _ereg_supp;	// save INITO structure for emit

  // private constructor so it cannot be used
  BB_REGION(void);
  BB_REGION(const BB_REGION&);
  BB_REGION& operator = (const BB_REGION&);

public:
  BB_REGION(BB_NODE *region_start, BB_NODE *region_end, RID *rid,
	    BB_REGION *parent, WN *pragmas, WN *exits,
	    INITO_IDX ereg_supp, INT64 linenum, WN *orig_wn) :
    _region_start(region_start),
    _region_end(region_end),
    _rid(rid),
    _parent(parent),
    _ereg_supp(ereg_supp),
    _region_line_num(linenum),
    _orig_wn(orig_wn)
  {
    _region_num_exits = RID_num_exits(_rid); // PPP is this needed?

    // need to copy the WHIRL nodes for the pragmas and exits
    // we never use the same whirl that came in (mmapped)
    _region_pragmas = WN_COPY_Tree_With_Map(pragmas);
    _region_exits = WN_COPY_Tree_With_Map(exits);

    if (_region_end != NULL)
       _region_end->Set_regionend();
  }

  BB_NODE *Region_start(void) const		{ return _region_start; }
  void Set_region_start(BB_NODE *region_start)	{ _region_start= region_start;}
  BB_NODE *Region_end(void) const		{ return _region_end; }
  void Set_region_end(BB_NODE *region_end)
  {
    _region_end = region_end; 
    if (_region_end != NULL)
      _region_end->Set_regionend();
  }
  RID *Rid(void) const				{ return _rid; }
  void Set_rid(RID *rid)			{ _rid = rid; }
  BB_REGION *Parent(void) const			{ return _parent; }
  void Set_parent(BB_REGION *parent)		{ _parent = parent; }
  INITO_IDX Ereg_supp(void) const		{ return _ereg_supp; }
  INT64 Region_line_num(void) const		{ return _region_line_num; }

  // region exit goto table, region pragma list
  WN *Region_pragma_list(void) const		{ return _region_pragmas; }
  void Set_region_pragmas(WN *pragmas)		{ _region_pragmas = pragmas; }
  WN *Region_exit_list(void) const		{ return _region_exits; }
  INT32	Region_num_exits(void) const		{ return _region_num_exits; }
  void Set_region_num_exits(RID *rid)
    { Is_True(rid != NULL, ("BB_REGION::Set_region_num_exits, NULL RID"));
      _region_num_exits = RID_num_exits(rid);
    }
  WN *Orig_wn(void) const                       { return _orig_wn; }
  void Set_orig_wn(WN *wn)                      { _orig_wn = wn; }

  // Find first and last statements in a region. Follows CFG until
  // it finds something.
  void	      Find_first_last_stmt(BB_NODE *, BB_NODE *, WN **, WN **);

}; // end bb_region class

#endif  // opt_bb_INCLUDED
#endif  // opt_bb_INCLUDED || opt_bb_CXX
