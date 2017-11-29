/*
 * Copyright (C) 2008-2010 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

//-*-c++-*-

/*
 * Copyright 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
 */

// ====================================================================
// ====================================================================
//
// Module: opt_cfg.h
// $Revision: 1.1.1.1 $
// $Date: 2005/10/21 19:00:00 $
// $Author: marcel $
// $Source: /proj/osprey/CVS/open64/osprey1.0/be/opt/opt_cfg.h,v $
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
//                  Control Flow Graph
//                  ------------------
//
//      In preopt phase, the cfg construction performs some form of
//      lowering for the following opcodes.  As result:
//      Do loop: comprise 4 nodes, INIT, END, BODY, STEP.
//               The INIT block contains the init statement in the
//               original WHIRL tree.  The END block contains a
//               OPR_TRUEBR statement which uses the ending condition
//               in the original WHIRL tree.  The STEP block contains
//               the step increment statement in the original tree.
//               +-------+
//               |  INIT |
//               +-------+
//                   |
//                   v
//                   ^
//          +---- < END > ----> loop exit
//          |        v
//          |        |
//          |        v
//          |    +-------+
//          |    |  BODY |
//          |    +-------+
//          |        |
//          |        v
//          |    +-------+
//          |    |  STEP |
//          |    +-------+
//          |        |
//          +--------+
//
//      While_do loop: comprise 2 nodes, END, BODY.
//                   |
//                   v
//                   ^
//          +---- < END > ----> loop exit
//          |        v
//          |        |
//          |        v
//          |    +-------+
//          |    |  BODY |
//          |    +-------+
//          |        |
//          +--------+
//
//      Do_While loop: comprise 2 nodes, BODY, END.
//                   |
//          +-----+  |
//          |     |  |
//          |     v  v
//          |    +-------+
//          |    |  BODY |
//          |    +-------+
//          |        |
//          |        v
//          |        ^
//          +     < END > ----> loop exit
//          |        v
//          |        |
//          +--------+
//
//      If statement: creates a new bb that contains a OPR_TRUEBR
//                    statement which uses the if condition in the
//                    original tree.  The rest of if statement is
//                    connected from this bb.
//
//      Conditional Branch: The ST label is replaced with the pointer
//                    to the branch target BB.
//
// ====================================================================
// ====================================================================


#ifndef opt_cfg_INCLUDED
#define opt_cfg_INCLUDED	"opt_cfg.h"
#ifdef _KEEP_RCS_ID
static char *opt_cfgrcs_id =	opt_cfg_INCLUDED"$Revision: 1.7 $";
#endif /* _KEEP_RCS_ID */

#ifndef opt_bb_INCLUDED
#include "opt_bb.h"
#endif
#ifndef opt_proactive_INCLUDED
#include "opt_proactive.h"
#endif
#ifndef opt_fb_INCLUDED
#include "opt_fb.h"
#endif
#ifndef opt_base_INCLUDED
#include "opt_base.h"
#endif
#ifndef CXX_MEMORY_INCLUDED
#include "cxx_memory.h"
#endif
#ifndef ERRORS_INCLUDED
#include "errors.h"
#endif
#ifndef region_util_INCLUDED
#include "region_util.h"
#endif

// forward declarations
typedef struct region_id RID;
class EXC;
class MOD_PHI_BB_CONTAINER;
class OPT_STAB;
class OPT_TAIL;
class LMV_CFG_ADAPTOR; 
class LOOP_UNROLL_UTIL;

class CFG {
friend class EXITBB_ITER;
friend class OPT_TAIL;
friend class LOOP_UNROLL_UTIL;
private:
  BOOL		_trace;		// -ttOPT:0x0008 or 0x0004

  DYN_ARRAY<BB_NODE*> _bb_vec;     // handle to bb table   
  DYN_ARRAY<BB_NODE*> _entry_vec;  // hold all entry points
  DYN_ARRAY<BB_NODE*> _exit_vec;   // hold early exit
  DYN_ARRAY<BB_NODE*> _notreach_vec;// bbs not reached from an entry
  DYN_ARRAY<BB_NODE*> _agoto_pred_vec; // bbs containing AGOTO statements
  DYN_ARRAY<BB_NODE*> _agoto_succ_vec; // bbs possibly reached from AGOTO

  BB_NODE    **_dfs_vec;       // all bb's pointer in DFS order
  INT32        _dfs_vec_sz;    // the number of entries in it
  BB_NODE    **_po_vec;        // all bb's pointer in CFG post-order
  INT32        _po_vec_sz;     // the number of entries in it
  BB_NODE    **_dpo_vec;       // dominator tree preorder BB vector
  INT32        _dpo_vec_sz;    // the number of entries in it
  BB_NODE    **_pdo_vec;       // post-dominator tree preorder BB vector
  INT32        _pdo_vec_sz;    // the number of entries in it
  MEM_POOL    *_mem_pool;      // permenant memory pool
  MEM_POOL    *_loc_pool;      // local memory pool
  MEM_POOL    * _sc_pool;      // SC nodes and trees pool
  BB_NODE     *_entry_bb;      // assume _entry_bb dominate all bbs.
  BB_NODE     *_exit_bb;       // assume exit_bb post dominate all bbs.
  BB_NODE     *_first_bb;      // in source order, excluding the fake entry/exit
  BB_NODE     *_last_bb;       // in source order, excluding the fake entry/exit
  BB_NODE     *_fake_entry_bb; // the fake entry BB
  BB_NODE     *_fake_exit_bb;  // the fake exit BB
  BB_LOOP     *_loops;         // the loop structure used in mainopt
  BOOL         _loops_valid;   // the loop structure are valid
  SC_NODE *    _sc_root;        // the root SC_NODE for the funtion
  IDTYPE       _last_sc_id;     // ID for last-allocated SC node
  STACK<SC_NODE *> * _sc_parent_stack; // a stack of parent SC_NODEs. Used during SC
                                      // tree construction.
  MAP         *_sc_map;               // map from BB_NODE Id to SC_NODE *. Used during
                                      // SC tree construction.
  MAP         *_clone_map;            // map from original block id to cloned block id. 
                                      // scratch field
  EXC         *_exc;           // handle to the exception handling
  INT32        _last_stmt_id;  // stmt_id

  // The following members and methods are for CFG construction use.
  BB_NODE     *_current_bb;     // point to bb currently constucting
  IDTYPE       _first_bb_id;    // the first bb id in the comp unit
  IDTYPE       _last_bb_id;     // to assign bb_id, maintain source order!
  MAP         *_label_map;      // map from label to bb
  IDTYPE       _orig_last_label;// the original last label number for this PU
  IDTYPE       _last_label_num; // the last label number for this PU
  INT32        _cur_loop_depth; // nesting depth of loops
  BOOL         _lower_fully;    // to lower hi-scf statements with all
                                // the label/goto inserted
  OPT_STAB    *_opt_stab;       // opt_stab pointer
  CODEMAP     *_htable;
  BOOL         _calls_break;    // do calls break basic blocks?
  BOOL         _rvi_break_stmt; // break at every stmt for rvi?
  OPT_FEEDBACK *_feedback;      // Pointer to feedback data -- NULL if no data
  STACK<MP_TY> _mp_type;        // the mp region type
  STACK<RID *> _mp_rid;         // the rid of the region
  STACK<BB_REGION *> _bb_region;// the BB_REGION of the parent (not just mp)
  STACK<RID *> _eh_rid;         // the stack of eh_region's rid
  
#if defined(TARG_SL) //PARA_EXTENSION
  STACK<SL2_PARA_TY> _sl2_para_type;  // the sl2_para region type
  STACK<RID *> _sl2_para_rid;   // the rid of the region
#endif
  RID	      *_rid;		// RID pointer
  REGION_LEVEL _rgn_level;	// context for cfg: preopt/mainopt/rvi
  BOOL         _has_regions;	// does the cfg have region nodes?
  INT32        _dohead_cnt;     // number of DOHEAD block, for PRE
  BOOL         _allow_clone_calls; // allow clone block having calls

  BB_NODE_SET *_bb_set;         // A scratch bb set for temporary use
  BB_NODE_SET *_non_true_body_set; // scratch bb set for use in Compute_true_loop_body_set

               CFG(const CFG&);
               CFG(void);
               CFG& operator = (const CFG&);

  void         Connect_predsucc (BB_NODE *p, BB_NODE *s)
			{ if (!p->Succ()->Contains(s)) {
			    p->Append_succ(s,_mem_pool);
			    s->Append_pred(p,_mem_pool);
			  } else {
			    Is_True(s->Pred()->Contains(p), 
			      ("Bad CFG preds and succs"));
			  }
			}
  void       Prepend_predsucc (BB_NODE *p, BB_NODE *s)
			{ if (!p->Succ()->Contains(s)) {
			    p->Prepend_succ(s,_mem_pool);
			    s->Prepend_pred(p,_mem_pool);
			  } else {
			    Is_True(s->Pred()->Contains(p), 
			      ("Bad CFG preds and succs"));
			  }
			}
  void         DisConnect_predsucc (BB_NODE *p, BB_NODE *s)
			{ p->Remove_succ(s,_mem_pool);
			  s->Remove_pred(p,_mem_pool);
			}
  void         Connect_after(BB_NODE *p, BB_NODE *s)
			{ p->Insert_After(s);
			  Connect_predsucc(p, s);
			}
  void         Init_stmtlist(BB_NODE *b, WN *stmt)
			{ b->Set_firststmt(stmt);
			  b->Set_laststmt(stmt);
			}
  IDTYPE       Alloc_bb_id(void)
			{ return _last_bb_id = _bb_vec.Newidx(); }

  BB_NODE     *New_bb( BOOL connect, BB_KIND k = BB_GOTO );

  // create a new block, but do not add it to the cfg
  BB_NODE     *Create_bb( BB_KIND k = BB_GOTO )
			{ BB_NODE *tmp= CXX_NEW(BB_NODE(), _mem_pool);
			  tmp->Set_kind(k);
			  return tmp;
			}
  // SC tree manipulation routines.
  SC_NODE *   Add_sc(BB_NODE * bb, SC_TYPE type);
  SC_NODE *   Unlink_sc(BB_NODE *bb);
  SC_NODE    *Get_sc_from_bb(BB_NODE * bb) const
  {
    return (SC_NODE *) _sc_map->Get_val((POINTER) bb->Id());
  }

  void        Add_sc_map (BB_NODE * bb, SC_NODE *sc)
  {
    _sc_map->Add_map((POINTER) bb->Id(), (POINTER)sc);
  }

  void Remove_sc_map (BB_NODE * bb, SC_NODE *sc) {
    MAP_LIST * map_lst = _sc_map->Find_map_list((POINTER) bb->Id());
    if (map_lst->Val() == (POINTER) sc)
      map_lst->Set_val(NULL);
  }

  void        SC_init();
  void        Fix_WN_label(WN *);

  // attach the block to the cfg, and make it the current block
  void         Append_bb( BB_NODE *bb )
			{ bb->Set_id(Alloc_bb_id());
			  _bb_vec[bb->Id()] = bb;
			  if ( _last_bb != NULL ) {
			    _last_bb->Insert_After(bb);
			  }
			  _last_bb = bb;
			  _current_bb = bb;
			  if (Do_pro_loop_trans())
                              Add_sc(bb, SC_BLOCK);
			}

  void         Set_current_bb(BB_NODE *b)
			{ _current_bb = b; }

  // Find out which blocks are not reached from an entrypoint
  // (see also public version Find_not_reached())
  // "can_disconnect" means there are no phi-nodes in the cfg yet, so
  // we can simply disconnect unreached blocks from their preds/succs
  void         Process_not_reached( BOOL can_disconnect );
  void         Prop_entry(BB_NODE*) const;

  // Find all of the blocks that exit from the region, and fill in
  // the _exit_vec[] array.  'is_whirl' says that block still contains
  // whirl statements rather than stmtreps.
  void         Find_exit_blocks( void );
  // Find blocks that do not exit
  void         Find_no_exit_blocks( BB_NODE *bb, BB_NODE_SET *instack );
  void         Process_no_exit(void);
  void         Bkwd_prop_exit(BB_NODE*) const;

  // add a true entrypoint into the region
  void         Add_altentry(BB_NODE *bb) 
		{ _entry_vec[_entry_vec.Newidx()] = bb;}
  // add a block that exits the region or does not reach an exit
  void         Add_earlyexit(BB_NODE *bb)
		{ _exit_vec[_exit_vec.Newidx()] = bb; }
  // add a block that is not reachable from one of the entrypoints
  void         Add_notreach(BB_NODE *bb)
		{ _notreach_vec[_notreach_vec.Newidx()] = bb;
		  bb->Init_stmt(NULL); // sets first and last WN pointers
		}

  void         Init_last_label
                     (const IDTYPE lab)  { _last_label_num =
					     _orig_last_label = lab; }
  BOOL         Verify_wn(WN *wn, WN *parent, WN_MAP wn_map);
  void         Fill_DFS_vec(BB_NODE *bb);     // fill up _dfs_vec and _po_vec

  // Internal routines for initialization of _dpo_vec and _pdo_vec
  void         Init_dpo_vec(BB_NODE*, INT *id);
  void         Init_pdo_vec(BB_NODE*, INT *id);


  enum END_BLOCK { 
    END_UNKNOWN,	// bad value
    END_NOT, 		// does not end block
    END_FALLTHRU,	// ends block, and falls-through to next block
    END_BREAK,		// ends block, does not fall-through
  };
  void         Create_empty_preheader (WN* loop); 
  // lower various high-level construct statements to CFG so they are
  // valid (just as if lowerer had handled them)
  void         Lower_do_loop(WN *wn, END_BLOCK *ends_bb );
  void         Lower_do_while(WN *wn, END_BLOCK *ends_bb );
  void         Lower_while_do(WN *wn, END_BLOCK *ends_bb );
  INT	       Is_simple_expr(WN *wn);
  void         Lower_if_stmt(WN *wn, END_BLOCK *ends_bb );
  WN          *if_convert(WN *wn);
  BOOL         wn_is_return_convert(WN *wn);
  // add various high-level construct statements to CFG so they can
  // later be raised back up (mostly preopt phase)
  void         Add_one_io_stmt(WN *wn, END_BLOCK *ends_bb);
  void         Add_one_do_loop_stmt(WN *wn, END_BLOCK *ends_bb );
  void         Add_one_do_while_stmt(WN *wn, END_BLOCK *ends_bb );
  void         Add_one_while_do_stmt(WN *wn, END_BLOCK *ends_bb );
  void         Add_one_if_stmt(WN *wn, END_BLOCK *ends_bb );
  void         Add_one_compgoto_stmt(WN *wn, END_BLOCK *ends_bb );
  void         Add_one_region(WN *wn, END_BLOCK *ends_bb );
  void         Add_one_stmt(WN *func_nd, END_BLOCK *ends_bb );
  BB_NODE     *Process_entry( WN *wn, END_BLOCK *ends_bb );

  void         Create_label_stmt(BB_NODE *bb);  // in the bb

  // copy the block of pragma statements into bb
  void         Copy_xpragmas_into(BB_NODE *bb, WN *pragmas);

  // Create a BB with label
  BB_NODE     *Create_labelled_bb(BB_KIND k=BB_GOTO);
  // Add loop entry test to current block
  BB_NODE     *Create_entrytest(WN *cond, BB_NODE *target);
  BB_NODE     *Create_loopbody(WN *);
  // Create loop exit test bb
  BB_NODE     *Create_exittest(WN *, BB_NODE*, BB_KIND);
  // Create conditional test
  BB_NODE     *Create_conditional(WN *, BB_NODE *, BB_NODE *, BOOL, WN **);
  // generate the opc_loop_info and attach to the body_bb
  void         Create_loop_info( BB_NODE *body_bb, WN *loop_wn );
  void         Create_blank_loop_info( BB_NODE *body_bb );

  // locate all of the blocks in a given region, filling in the
  // sets of blocks in each loop FOR PREOPT
  BB_LOOP     *Ident_loop( BB_NODE *first_bb, BB_NODE *last_bb,
			   INT32 loopnest, BB_LOOP *cur_loop );

  void         Set_loop_bb_set(BB_LOOP *loop);
  BOOL	       Check_if_it_can_reach_body_first_bb(BB_NODE *bb, BB_LOOP *loop);
  void	       Compute_true_loop_body_set(BB_LOOP *loop);
  BOOL         Loop_itself_is_empty(BB_LOOP *loop);
  BB_LOOP     *Get_last_loop(BB_LOOP *loop);
  void         Remove_loop_construct (BB_LOOP *);

  // screen out those BBs originally assigned to the 'loop' but
  // actually does not belong to the 'loop'.
  void         Screen_out_false_loopnest(BB_LOOP *loop, BB_LOOP *sibling);

  void         Ident_mp_regions(void);
  void         Ident_eh_regions(void);
#if defined(TARG_SL) //PARA_EXTENSION
  void         Ident_sl2_para_regions(void);
#endif

  // functions about loop multiversioning 
  BB_NODE*     LMV_clone_block (const BB_NODE* src, LMV_CFG_ADAPTOR*);
  BB_NODE*     LMV_create_alike_block (BB_KIND kind, BB_NODE* model);
  void         LMV_clone_pred_succ_relationship (LMV_CFG_ADAPTOR*); 
  void         LMV_clone_loop_body (LMV_CFG_ADAPTOR*); 
  void         LMV_update_internal_labels (LMV_CFG_ADAPTOR*);
  BB_LOOP*     LMV_clone_BB_LOOPs (LMV_CFG_ADAPTOR*);
  BB_LOOP*     LMV_clone_BB_LOOP (LMV_CFG_ADAPTOR*, BB_LOOP*);
  void         LMV_gen_precondioning_stuff (LMV_CFG_ADAPTOR*);
  void         LMV_clone_BB_IFINFO (LMV_CFG_ADAPTOR* );
  void         LMV_clone_frequency (LMV_CFG_ADAPTOR*);

  //bottom test loop check
  BOOL bottom_test_loop(WN* while_do);

  // From here on, all are public access functions
public:
              CFG(MEM_POOL *pool,
		  MEM_POOL *lpool);
              ~CFG(void);

  BOOL	       Trace(void) const { return _trace; }
  void         Print(FILE *fp=stderr, 
		     BOOL dfs_order = TRUE, IDTYPE bb_id = (IDTYPE) -1);
  void         PrintLoopVis(BB_LOOP * loop, int & id);
  void         PrintVis(BOOL draw_loops);
  void         PrintCDVis(void);
  void         Validate(FILE *fp=stderr);

  // create the control-flow graph
  void         Create(WN *func_nd,	// the top-level whirl
                      BOOL lower_scf,	// lower structured cntl-flow
		      BOOL calls_break, // calls break blocks
		      REGION_LEVEL rgn_level,  // caller level:
  		      			// preopt/mainopt/rvi
		      OPT_STAB *opt_stab,// optimizer symbol table
		      BOOL do_tail, // do tail recursion?
		      MEM_POOL * sc_pool);  // pool for SC nodes and trees

  void         Compute_dom_tree          // compute dominator tree or
                     (BOOL build_dom);   // post-dominator tree
  void         Compute_dom_frontier(void);
  void         Compute_bbs_reached(void);
  void         Compute_control_dependence
                     (void);

  MEM_POOL    *Mem_pool(void)       const{ return _mem_pool; }
  MEM_POOL    *Loc_pool(void)       const{ return _loc_pool; }
  MEM_POOL    *SC_pool(void)        const{ return _sc_pool; }
  CODEMAP     *Htable(void)         const{ return _htable; }
  void         Set_htable(CODEMAP *h)    { _htable = h; }
  IDTYPE       First_bb_id(void)    const{ return _first_bb_id; }
  IDTYPE       Last_bb_id(void)     const{ return _last_bb_id; }
  mUINT32      Total_bb_count(void) const{ return _last_bb_id+1; }

  BOOL         Removable_bb( const BB_NODE *bb ) const;
  void         Remove_bb(BB_NODE *bb);
  void         Remove_path(BB_NODE *pred, BB_NODE *succ);
  // completely eliminate bb from cfg, loopinfo, phi, usecnt etc. 
  // Only use AFTER SSA is done
  void         Delete_bb(BB_NODE *bb,
                         MOD_PHI_BB_CONTAINER *);
#ifdef KEY
  void         Delete_bbs(BB_LIST *bbs,
                         MOD_PHI_BB_CONTAINER *);
#endif
  BB_NODE     *Entry_bb(void)       const{ return _entry_bb; }
  BB_NODE     *Exit_bb(void)        const{ return _exit_bb; }
  BB_NODE     *Fake_entry_bb(void)  const{ return _fake_entry_bb; }
  BB_NODE     *Fake_exit_bb(void)   const{ return _fake_exit_bb; }
  BB_NODE     *First_bb(void)       const{ return _first_bb; }
  BB_NODE     *Last_bb(void)        const{ return _last_bb; }
  void         Set_last_bb( BB_NODE *bb) { _last_bb = bb; }
  BB_NODE     *Get_bb(mINT32 idx)   const{ return _bb_vec[idx]; }

  BB_NODE     *Func_entry_bb(void) const;

  // Split one BB into two consecutive ones.  The new BB is returned
  // to the caller.  Assumes only whirl statements.
  // The given wn becomes the last statement in the old block.
  BB_NODE     *Split_bb_with_wns(BB_NODE *, WN *);

  // Change_block_kind is used by DCE and CFG to fix up loop-info and
  // other data structures when part of it is not reachable.
  void	       Change_block_kind(BB_NODE *, BB_KIND) const;

  BB_LOOP     *Loops(void)          const{ return _loops; }
  BOOL         Loops_valid()        const{ return _loops_valid; }
  void         Set_loops(BB_LOOP *l)     { _loops = l; }
  void         Set_loops_valid(BOOL b)   { _loops_valid = b; }
  void         Set_exc(EXC *exc)         { _exc = exc; }
  EXC         *Exc(void)            const{ return _exc; }

  void         Reset_stmt_id(void)       { _last_stmt_id = INVALID_STMT_ID; }
  INT32        Get_stmt_id(void)         { return ++_last_stmt_id; }

  void         Invalidate_loops(void);

  LABEL_IDX    Alloc_label(void) const
    {
      LABEL_IDX idx;
      New_LABEL(CURRENT_SYMTAB, idx);
      return idx;
    }

  INT32        Cur_loop_depth(void) const{ return _cur_loop_depth; }
  void         Set_cur_loop_depth(INT32 newdepth)  
		{ _cur_loop_depth = newdepth; }

  // Prepend wn at the beginning of bb
  void         Prepend_wn_in(BB_NODE *bb, WN *wn);
  // Append wn at the end of bb
  void         Append_wn_in(BB_NODE *bb, WN *wn);

  // is high-level structured control being lowered fully?
  BOOL         Lower_fully(void) const	 { return _lower_fully; }
  // do calls end basic blocks?
  BOOL         Calls_break(void) const	 { return _calls_break; }
  // do we need to break each stmt into a block for RVI?
  BOOL         Rvi_break_stmt(void) const{ return _rvi_break_stmt; }
  void         Set_rvi_break_stmt(BOOL b){ _rvi_break_stmt = b; }
  // profile feedback info for this PU
  OPT_FEEDBACK *Feedback(void) const          { return _feedback; }
  void         Set_feedback(OPT_FEEDBACK *fb) { _feedback = fb; }
  BOOL         Has_feedback(void) const       { return _feedback != NULL; }

  // stack for BB_REGION when processing inside a region
  // (all regions, not just mp)
  void         Push_bb_region(BB_REGION *bbr) { _bb_region.Push(bbr); }
  BB_REGION   *Pop_bb_region(void)            { return _bb_region.Pop(); }
  BB_REGION   *Top_bb_region(void) const      { return _bb_region.Top(); }
  void         Clear_bb_region(void)  	      { _bb_region.Clear(); }
  BOOL	       Null_bb_region(void) const     { return _bb_region.Is_Empty(); }

  // Assuming the MP region has only one entry one exit
  void         Push_mp_type(MP_TY t)     { _mp_type.Push(t);}
  void         Push_mp_rid(RID *rid)     { _mp_rid.Push(rid); }
  MP_TY        Pop_mp_type(void)         { return _mp_type.Pop(); }
  RID         *Pop_mp_rid(void)          { return _mp_rid.Pop(); }
  MP_TY        Top_mp_type(void) const   { return _mp_type.Top(); }
  RID         *Top_mp_rid(void) const    { return _mp_rid.Top(); }
  BOOL         NULL_mp_type(void) const  { return _mp_type.Is_Empty(); }
  void         Clear_mp_type(void)       { _mp_type.Clear(); }
  void         Clear_mp_rid(void)        { _mp_rid.Clear(); }

  void         Push_eh_rid(RID *rid)     { _eh_rid.Push(rid); }
  RID         *Pop_eh_rid(void)          { return _eh_rid.Pop(); }
  RID         *Top_eh_rid(void) const    { return _eh_rid.Top(); }
  void         Clear_eh_rid(void)        { _eh_rid.Clear(); }
  BOOL         Null_eh_rid(void) const   { return _eh_rid.Is_Empty(); }

  BOOL         Inside_mp_do(void)        { return !NULL_mp_type() &&
                                             Top_mp_type() != MP_REGION;
                                         }
#if defined(TARG_SL) //PARA_EXTENSION
  // Assuming the SL2 parallel region has only one entry one exit
  void      Push_sl2_para_type(SL2_PARA_TY t)     { _sl2_para_type.Push(t);}
  void      Push_sl2_para_rid(RID *rid)  { _sl2_para_rid.Push(rid); }
  SL2_PARA_TY   Pop_sl2_para_type(void)  { return _sl2_para_type.Pop(); }
  RID       *Pop_sl2_para_rid(void)      { return _sl2_para_rid.Pop(); }
  SL2_PARA_TY   Top_sl2_para_type(void) const   { return _sl2_para_type.Top(); }
  RID       *Top_sl2_para_rid(void) const    { return _sl2_para_rid.Top(); }
  BOOL    NULL_sl2_para_type(void) const  { return _sl2_para_type.Is_Empty(); }
  void      Clear_sl2_para_type(void)       { _sl2_para_type.Clear(); }
  void      Clear_sl2_para_rid(void)        { _sl2_para_rid.Clear(); }

  BOOL    Inside_sl2_para_do(void)        { return !NULL_sl2_para_type() &&
                                             Top_sl2_para_type() != SL2_PARA_REGION;
                                         }

#endif

  // return the RID tree for this PU or RID for just the region
  RID         *Rid(void) const	 	 { return _rid; }
  // which phase is building a cfg (preopt/mainopt/rvi)
  REGION_LEVEL Rgn_level(void) const	 { return _rgn_level; }
  // were there any region nodes seen in the program
  void         Set_has_regions(void)	 { _has_regions = TRUE; }
  BOOL         Has_regions(void) const	 { return _has_regions; }
  // total number of DOHEAD block in the PU
  void         Inc_dohead_cnt(void)      { _dohead_cnt++; }
  INT32        Dohead_cnt(void) const    { return _dohead_cnt; }

  // return the block associated with the given label number (which 
  // comes either from a WN or is generated internally
  BB_NODE     *Get_bb_from_label(INT32 l) const
                                { return (BB_NODE*)
				       _label_map->Get_val((POINTER)(INTPTR)l);
				}
  void         Append_label_map (INT32 labnum, BB_NODE *bb)
				{ _label_map->Add_map((POINTER)(INTPTR)labnum,
						      (POINTER)bb); 
				  bb->Set_labnam(labnum);
				}

  BB_NODE     *Add_bb_to_edge(BB_NODE *, BB_NODE *);

  // Function name is self-explanatory
  const BB_LOOP *Find_innermost_loop_contains(BB_NODE *bb);

  // Find a region with the given pragma, that encloses the given BB
  // Note that the region encloses "bb" and does not start with it
  BB_NODE *Find_enclosing_region_bb( BB_NODE *, WN_PRAGMA_ID );
#ifdef KEY
  // Find a parallel region that dominates the given BB.
  // Note that the region encloses "bb" and does not start with it
  BB_NODE *Find_enclosing_parallel_region_bb( BB_NODE *);
#endif

  // Determine if this loop is the outermost one in a parallel region
  // (any sort of parallel loop or region for which we probably do not
  //  wish to perform IVR on...)
  BOOL Is_outermost_loop_in_parallel_region( BB_LOOP *loop,
					     WN_PRAGMA_ID pragma_id );


  // Find blocks in the cfg that are not reached from an entrypoint
  // (sets Reached() flag for them)
  void         Find_not_reached();

  // Connect all agoto blocks to all labels that could be targets
  void         Connect_agotos();

  // Remove bb from the list of blocks containing AGOTOs (used during DCE)
  void         Remove_agoto_pred( BB_NODE *bb );

  // Return the number of blocks containing potential agoto target labels
  INT32        Agoto_succ_entries();

  // Return the idx'th block containing a potential agoto target labels
  BB_NODE     *Agoto_succ_bb(INT32 idx);

  // process multiple entry or exit blocks.  'is_whirl' means that
  // the blocks contain whirl statements rather than stmtreps
  void         Process_multi_entryexit( BOOL is_whirl );

  // update pred/succ arcs of fake entry/exit blocks so they are not
  // reachable from normal blocks
  void         Remove_fake_entryexit_arcs( void );

  // Rebuild or mark for rebuilding all auxilliary data structures:
  // dom/pdom trees, _dpo_vec, _po_vec...
  void         Invalidate_and_update_aux_info(BOOL);

  // Remove critical edge: add a BB_NODE to each CFG edge where the
  // source BB has more than one successors and the sink BB has more
  // than one predecessors.  Return the number of edges coverted.
  INT          Remove_critical_edge();

  // fill in provided array of BB_NODE *'s so they are ordered in
  // pred-first order.  (i.e., all of a block's predecessors come
  // before it in the array)
  // Also return the number of entries in the array.  
  // Uses the Dforder() flag.
  void	       Get_pred_first_vec( BB_NODE *bbvec[], INT32 *numbbs );
  BB_NODE    **Dfs_vec(void);
  INT32        Dfs_vec_sz(void) const { return _dfs_vec_sz; }

  BB_NODE    **Po_vec(void);
  INT32        Po_vec_sz(void) const { return _po_vec_sz; }
  BB_NODE     *Po_Bb(INT32 n) const  { return _po_vec[n]; }

  // Dpo_vec() must be invoked before Dpo_Bb(n) can be accessed
  BB_NODE    **Dpo_vec(void);
  INT32        Dpo_vec_sz(void) const { return _dpo_vec_sz; }
  BB_NODE     *Dpo_Bb(INT32 n) const  { return _dpo_vec[n]; }

  // Pdo_vec() must be invoked before Pdo_Bb(n) can be accessed
  BB_NODE    **Pdo_vec(void);
  INT32        Pdo_vec_sz(void) const { return _pdo_vec_sz; }
  BB_NODE     *Pdo_Bb(INT32 n) const  { return _pdo_vec[n]; }

  BOOL         Verify_tree(WN *);
  BOOL         Verify_cfg(void);
  BOOL         Verify_label(void);
  

  // find cyclic regions in the cfg, and attempt to convert them to
  // higher-level loops by changing their block kinds and setting the
  // appropriate information.
  void         Find_loop_cycles( void );

  BB_LOOP     *Analyze_loops(void);  // Analyze the loop structure

  BB_NODE     *Find_entry_bb(void);  // used by emitters to find code block
  
  BOOL         Fall_through(BB_NODE *bb1, BB_NODE *bb2);  // bb1 falls through to bb2

  void         Delete_empty_BB();

  BOOL         Allow_clone_calls (void) const { return _allow_clone_calls; }
  void         Set_allow_clone_calls (BOOL b) { _allow_clone_calls = b; }

  // Clone a BB_NODE
  void         Clone_bb(IDTYPE source, IDTYPE dest, BOOL clone_wn);
  // Clone a list of BB_NODEs
  void         Clone_bbs(BB_NODE *, BB_NODE *, BB_NODE **, BB_NODE **, BOOL clone_wn, float scale);
  // Clone a BB_IFINFO
  BB_IFINFO *  Clone_ifinfo(BB_IFINFO *);
  // Clone a BB_LOOP
  BB_LOOP *    Clone_loop(BB_LOOP *);
  // Clone a SC_NODE
  SC_NODE *    Clone_sc(SC_NODE *, BOOL, float, SC_NODE **);
  // Create a SC node 
  SC_NODE *    Create_sc(SC_TYPE type);
  void         Freq_propagate(SC_NODE *);
  void         Freq_scale(SC_NODE *, float scale);
  void         Freq_scale(BB_NODE *, SC_NODE *, float scale);
  SC_NODE *    Split(SC_NODE *);
  SC_NODE *    Insert_block_after(SC_NODE *);
  SC_NODE *    Insert_block_before(SC_NODE *);
  SC_NODE *    Insert_if_before(SC_NODE *, BB_NODE *);
  void         Fix_info(SC_NODE *);

  // Create a new block and allocate it.
  BB_NODE     *Create_and_allocate_bb( BB_KIND k )
                        { BB_NODE *bb = Create_bb(k);
			  bb->Set_id(Alloc_bb_id());
			  _bb_vec[bb->Id()] = bb;
			  return bb;
			}

  // code generation for loop multiversioning.
  BOOL         LMV_eligible_for_multiversioning (const BB_LOOP*, BOOL);
  void         LMV_clone_loop (LMV_CFG_ADAPTOR*);

  // for the phase of removing useless store loop
  BB_NODE * ULSE_insert_bb_and_merge(STMTREP*, BB_NODE*, BB_NODE*);
  BOOL         If_convertible_cond(WN* wn);
  BOOL         If_conv_criteria_met(WN* wn);
  BOOL         Cand_is_select(WN *wn);
  BOOL         Cand_is_return_inside_select(WN *wn);
  BOOL         Screen_cand(WN* wn);
#if defined(TARG_SL)
  BOOL         Is_Sub_ILOAD_Tree(WN *wn, WN *parent_wn, WN * mode_wn);
#endif
  WN*          Conv_to_select(WN* wn);

  // Obtain root of the SC tree
  SC_NODE *   SC_root(void)           { return _sc_root; }
  // Query whether to do proactive loop nest optimization transformations
  BOOL Do_pro_loop_trans()     { return (_sc_root != NULL); }
  // Free SC tree and related storages.
  void Free_sc(void);
  // Obtain the cloned version of a BB_NODE
  BB_NODE *  Get_cloned_bb(BB_NODE *);
  // Create a LABEL WN and add it to the BB_NODE
  LABEL_IDX  Add_label_with_wn(BB_NODE * bb);
};


// CFG_ITER iterates forward through the bb next chain in the cfg
//
class CFG_ITER {
private:
  const CFG *cfg;
  BB_NODE   *cur;

  CFG_ITER(const CFG_ITER&);
  CFG_ITER& operator = (const CFG_ITER&);

public:
  CFG_ITER(void)             { cfg = NULL; cur = NULL; }
  CFG_ITER(CFG *g)           { cfg = g; cur = NULL; }
  CFG_ITER(const CFG *g)     { cfg = g; cur = NULL; }
  ~CFG_ITER(void)            {}

  void     Init(CFG *g)      { cfg = g; }
  void     Init(void)        {}
  BB_NODE *First(void)       { return cur = cfg->First_bb(); }
  BB_NODE *Last(void)        { return cur = cfg->Last_bb(); }
  BB_NODE *Next(void)        { return cur = ((cur)? cur->Next() : NULL); }
  BB_NODE *Prev(void)        { return cur = ((cur)? cur->Prev() : NULL); }
  BB_NODE *Cur(void)         { return cur; }
  BOOL Is_Empty(void)        { return cur == NULL; }
  BOOL Is_Empty_Reverse(void){ return cur == NULL; }
  BB_NODE *First_elem(void)  { return cur = cfg->First_bb(); }
  BB_NODE *Next_elem(void)   { return cur = ((cur)? cur->Next() : NULL); }
  BB_NODE *Prev_elem(void)   { return cur = ((cur)? cur->Prev() : NULL); }
};

// Iterator to access the depth-first BB array
class DFSBB_ITER {
  BB_NODE **_dfs_bbs;     // depth-first ordering of bb_nodes
  INT32     _bbs_count;   // how many entries in dfs_bbs
  INT32     _cur;
  
  // private constructor so it cannot be used
  DFSBB_ITER(void);
  DFSBB_ITER(const DFSBB_ITER&);
  DFSBB_ITER& operator = (const DFSBB_ITER&);

  BB_NODE *Dfs_Bb(INT32 n) const{ return _dfs_bbs[n]; }
  INT32    Bbs_count(void) const{ return _bbs_count; }

public:
  DFSBB_ITER(CFG *cfg)           { _dfs_bbs = cfg->Dfs_vec();
                                   _bbs_count = cfg->Dfs_vec_sz(); 
                                 }
  ~DFSBB_ITER(void)              {}

  void     Init(void)            {}
  INT32    First(void)           { return _cur = 0; }
  INT32    Last(void)            { return _cur = Bbs_count() - 1; }
  INT32    Next(void)            { return _cur = _cur + 1; }
  INT32    Prev(void)            { return _cur = _cur - 1; }
  INT32    Cur(void)             { return _cur; }
  BOOL     Is_Empty(void)        { return _cur >= Bbs_count(); }
  BOOL     Is_Empty_Reverse(void){ return _cur < 0; }
  BB_NODE *First_elem(void)      { First(); return Dfs_Bb(_cur); }
  BB_NODE *Last_elem(void)       { Last();  return Dfs_Bb(_cur); }
  BB_NODE *Next_elem(void)       { Next();  return Dfs_Bb(_cur); }
  BB_NODE *Prev_elem(void)       { Prev();  return Dfs_Bb(_cur); }

};


// Iterator to access the postorder BB array 
//
class POBB_ITER {
  BB_NODE **_po_bbs;     // post ordering of bb_nodes
  INT32     _bbs_count;  // how many entries in po_bbs
  INT32     _cur;	 // current PO number
  
  // private constructor so it cannot be used
  POBB_ITER(void);
  POBB_ITER(const POBB_ITER&);
  POBB_ITER& operator = (const POBB_ITER&);

  BB_NODE *Po_Bb(INT32 n) const{ return _po_bbs[n]; }
  INT32    Bbs_count(void) const{ return _bbs_count; }

public:
  POBB_ITER(CFG *cfg)
    {
      _po_bbs = cfg->Po_vec();
      _bbs_count = cfg->Po_vec_sz(); 
    }

  ~POBB_ITER(void)              {}

  void     Init(void)            {}
  INT32    First(void)           { return _cur = 0; }
  INT32    Last(void)            { return _cur = Bbs_count() - 1; }
  INT32    Next(void)            { return _cur = _cur + 1; }
  INT32    Prev(void)            { return _cur = _cur - 1; }
  INT32    Cur(void)             { return _cur; }
  BOOL     Is_Empty(void)        { return _cur >= Bbs_count(); }
  BOOL     Is_Empty_Reverse(void){ return _cur < 0; }
  BB_NODE *First_elem(void)      { First(); return Po_Bb(_cur); }
  BB_NODE *Last_elem(void)       { Last();  return Po_Bb(_cur); }
  BB_NODE *Next_elem(void)       { Next();  return Po_Bb(_cur); }
  BB_NODE *Prev_elem(void)       { Prev();  return Po_Bb(_cur); }
};

// Iterator to access the postorder BB array in reverse order
// (i.e. a reverse postorder traversal)
//
class RPOBB_ITER {
  POBB_ITER _po_iter;   // post-order iterator, which we traverse backwards
  
  // private constructor so it cannot be used
  RPOBB_ITER(void);
  RPOBB_ITER(const RPOBB_ITER&);
  RPOBB_ITER& operator = (const RPOBB_ITER&);

public:
  RPOBB_ITER(CFG *cfg) : _po_iter(cfg) {}
  ~RPOBB_ITER(void)              {}

  void     Init(void)            {}
  INT32    First(void)           { return _po_iter.Last(); }
  INT32    Last(void)            { return _po_iter.First(); }
  INT32    Next(void)            { return _po_iter.Prev(); }
  INT32    Prev(void)            { return _po_iter.Next(); }
  INT32    Cur(void)             { return Last() + First() - _po_iter.Cur(); }
  BOOL     Is_Empty(void)        { return _po_iter.Is_Empty_Reverse(); }
  BOOL     Is_Empty_Reverse(void){ return _po_iter.Is_Empty(); }
  BB_NODE *First_elem(void)      { return _po_iter.Last_elem(); }
  BB_NODE *Last_elem(void)       { return _po_iter.First_elem(); }
  BB_NODE *Next_elem(void)       { return _po_iter.Prev_elem(); }
  BB_NODE *Prev_elem(void)       { return _po_iter.Next_elem(); }
};


// Iterator to access the dominator/post-dominator tree preorder BB array
class DPOBB_ITER {
  BB_NODE **_dpo_bbs;     // dominator/post-dominator tree preorder vector
  INT32     _bbs_count;   // how many entries in dpo_bbs
  INT32     _cur;	  // current DPO number

  // private constructor so it cannot be used
  DPOBB_ITER(void);
  DPOBB_ITER(const DPOBB_ITER&);
  DPOBB_ITER& operator = (const DPOBB_ITER&);

  BB_NODE *Dpo_Bb(INT32 n) const{ return _dpo_bbs[n]; }
  INT32    Bbs_count(void) const{ return _bbs_count; }

public:
  DPOBB_ITER(CFG *cfg, BOOL dom = TRUE)
    {
      if (dom) {
	_dpo_bbs = cfg->Dpo_vec();
	_bbs_count = cfg->Dpo_vec_sz();
      } else {
	_dpo_bbs = cfg->Pdo_vec();
	_bbs_count = cfg->Pdo_vec_sz();
      }
    }

  ~DPOBB_ITER(void)              {}

  void     Init(void)            {}
  INT32    First(void)           { return _cur = 0; }
  INT32    Last(void)            { return _cur = Bbs_count() - 1; }
  INT32    Next(void)            { return _cur = _cur + 1; }
  INT32    Prev(void)            { return _cur = _cur - 1; }
  INT32    Cur(void)             { return _cur; }
  BOOL     Is_Empty(void)        { return _cur >= Bbs_count(); }
  BOOL     Is_Empty_Reverse(void){ return _cur < 0; }
  BB_NODE *First_elem(void)      { First(); return Dpo_Bb(_cur); }
  BB_NODE *Last_elem(void)       { Last();  return Dpo_Bb(_cur); }
  BB_NODE *Next_elem(void)       { Next();  return Dpo_Bb(_cur); }
  BB_NODE *Prev_elem(void)       { Prev();  return Dpo_Bb(_cur); }
};

// Iterator to access the exit_vec dynamic array
class EXITBB_ITER {
  CFG 	   *_cfg;
  INT32     _cur;

  
  // private constructor so it cannot be used
  EXITBB_ITER(void);
  EXITBB_ITER(const EXITBB_ITER&);
  EXITBB_ITER& operator = (const EXITBB_ITER&);

public:
  EXITBB_ITER(CFG *cfg)           { _cfg = cfg; }
  ~EXITBB_ITER(void)              {}

  BB_NODE *Cur_exit_bb() const	 { return _cfg->_exit_vec[_cur]; }
  void     Init(void)            {}
  void     First(void)           { _cur = 0; }
  void     Next(void)            { _cur++; }
  BOOL     Is_Empty(void)        { return _cur > _cfg->_exit_vec.Lastidx(); }
};

class MOD_PHI_BB : public SLIST_NODE {
  BB_NODE  *_bb;       // the basic that has its phi_list modified
  PHI_LIST *_old_lst;  // old list that got removed from the bb->Phi_list
  PHI_LIST *_new_lst;  // new list that got attached to the bb->Phi_list

  // unwanted member functions
            MOD_PHI_BB(const MOD_PHI_BB&);
            MOD_PHI_BB& operator = (const MOD_PHI_BB&);
            MOD_PHI_BB(void);
public:

            MOD_PHI_BB(BB_NODE *bb, PHI_LIST *old_lst, PHI_LIST *new_lst):
                      _bb(bb), _old_lst(old_lst), _new_lst(new_lst) {}
           ~MOD_PHI_BB(void)               {}

  BB_NODE  *Bb(void)      const            { return _bb; }
  PHI_LIST *Old_lst(void) const            { return _old_lst; }
  PHI_LIST *New_lst(void) const            { return _new_lst; }
  void      Set_new_lst(PHI_LIST *lst)     { _new_lst = lst; }
};

class MOD_PHI_BB_CONTAINER : public SLIST {
private:
  MEM_POOL *_mem_pool;

  MOD_PHI_BB_CONTAINER(const MOD_PHI_BB_CONTAINER&);
  MOD_PHI_BB_CONTAINER& operator = (const MOD_PHI_BB_CONTAINER&);

  DECLARE_SLIST_CLASS( MOD_PHI_BB_CONTAINER, MOD_PHI_BB )

public:  
  MOD_PHI_BB_CONTAINER(MEM_POOL *pool):_mem_pool(pool) {}
  ~MOD_PHI_BB_CONTAINER(void)              {}

  void      Add_entry(BB_NODE  *bb,        // add entry to this container
                      PHI_LIST *old_lst,
                      PHI_LIST *new_lst, 
                      MEM_POOL *pool);
};

class MOD_PHI_BB_ITER : public SLIST_ITER {
  DECLARE_SLIST_ITER_CLASS( MOD_PHI_BB_ITER, MOD_PHI_BB, MOD_PHI_BB_CONTAINER )
public:
  void     Init()                          {}
};

extern BOOL Remove_region_exit(BB_NODE *, BOOL);
extern void Remove_region_entry(BB_NODE *);

inline BOOL Is_backedge(BB_NODE *tail, BB_NODE *head)
{
  return head->Dominates(tail);
}

extern void Add_new_auxid_to_entry_chis(AUX_ID, CFG *, CODEMAP *, OPT_STAB *);

#endif  // opt_cfg_INCLUDED
