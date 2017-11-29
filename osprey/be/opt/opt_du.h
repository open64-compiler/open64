//-*-c++-*-
// ====================================================================
// ====================================================================
//
// Module: opt_du.h
// $Revision: 1.1.1.1 $
// $Date: 2005/10/21 19:00:00 $
// $Author: marcel $
// $Source: /proj/osprey/CVS/open64/osprey1.0/be/opt/opt_du.h,v $
//
// Revision history:
//  12-JAN-95 shin - Original Version
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
//   The DEF-USE chain with WHIRL represention for LNO.
//
//  Exported type:
//
//   DU_NODE:      One node in U-D chain or U-D chain.  It points to
//                 the WN node that store or reference a value of a
//                  memory location. 
//
//   DEF_LIST:     The linked list of DEF_NODE. It's a derived class
//                 from SLIST defined in cxx_base.h.  There is an
//                 additional field that point the loop statement if
//                 this def list is derived from a phi-function, and
//                 that phi is associated with a loop structure.
//
//   DEF_LIST_ITER:The iterator for the DEF_LIST.  It's a derived class
//                 from SLIST_ITER defined in cxx_base.h
//
//   USE_LIST:     The linked list of USE_NODE. It's a derived class
//                 from SLIST defined in cxx_base.h
//
//   USE_LIST_ITER:The iterator for the USE_LIST.  It's a derived class
//                 from SLIST_ITER defined in cxx_base.h
//
//   DU_MANAGER:   The interface object for the D-U and U-D chain.  It
//                 owns its own memory pool.  It provides the access
//                 function to Create/Set/Get map of D-U and U-D
//                 chain.  The optimizer's COMP_UNIT maintains a
//                 pointer to DU_MANAGER.  Since the mem_pool get
//                 initialized and pushed in the ctor and popped in
//                 the dtor, It must be created and deleted by the
//                 caller of the optimizer. The emitter of the
//                 optimizer would generate the D-U and U-D chain if
//                 the DU_MANAGER exists.
//
//  Exported functions:
//
//   Please refer to cxx_base.h for all exported member functions for
//   DU_NODE, DEF_LIST, DEF_LIST_ITER, USE_NODE, USE_LIST,
//   and USE_LIST_ITER.
//
//   DU_NODE::DU_NODE
//   DU_NODE::~DU_NODE
//   DU_NODE::Print    : Print the content of the DEF_NODE.
//   DU_NODE::Wn       : return the WN node of the DEF_NODE.
//
//   DU_MANAGER::DU_MANAGER
//   DU_MANAGER::~DU_MANAGER
//
//   DU_MANAGER::Add_Def_Use: Add a link between the def and the use
//		WN//def	    : the def node of the D-U/U-D chain
//		WN//use	    : the use node of the D-U/U-D chain
//   WARNING: Ud_Add_Def and Du_Add_Use will become obsololete.  Please
//            use Add_Def_Use instead.
//   DU_MANAGER::Ud_Add_Def : Add a def to a U-D chain
//               WN *use    : the use node of the U-D chain
//               WN *def    : the def node of the U-D chain
//               DEF_FLAGS f: the flag for def, DF_LOOP_PHI or DF_EMPTY
//   DU_MANAGER::Du_Add_Use : Add a use to a D-U chain
//               WN *def    : the def node of the D-U chain
//               WN *use    : the use node of the D-U chain
//
//   DU_MANAGER::Delete_Def_Use : Remove the link between def and use
//               WN *use    : the use node of the U-D chain
//               WN *def    : the def node of the U-D chain
//   WARNING: Ud_Delete_Def and Du_Delete_Use will become obsolete.
//	      Please use Delete_Def_Use instead.
//   DU_MANAGER::Ud_Delete_Def : Delete a def from a U-D chain
//               WN *use    : the use node of the U-D chain
//               WN *def    : the def node of the U-D chain
//   DU_MANAGER::Du_Delete_Use : Delete a use from a D-U chain
//               WN *def    : the def node of the D-U chain
//               WN *use    : the use node of the D-U chain
//
//   DU_MANAGER::Ud_Get_Def : Get the def_list of a U-D chain
//               WN *use    : the use node of the D-U chain
//   DU_MANAGER::Remove_Use_From_System(WN *use)
//		Remove use from the def-use system.  Get rid of all
//		defs on its chain and get rid of it from chains of any
//		def
//   DU_MANAGER::Remove_Def_From_System(WN *def)
//		Remove def from the def-use system.  Get rid of all
//		uses on its chain and get rid of it from chains of any
//		use.
//
//   DU_MANAGER::Create_Def_List: Create empty def_list for the use
//               WN *use    : attach the def_list to this use node
//   DU_MANAGER::Create_Use_List: Create empty use_list for the def
//               WN *def    : attach the use_list to this def node
//
//   DU_MANAGER::Du_built: Was any DU information built?
//
//   DU_MANAGER::Verify( BOOL tracing )
//              Return TRUE if the DU information is valid, and
//              FALSE otherwise.  Prints verification information
//              if tracing=true.
//
// ====================================================================
//    IPA Interface
// ====================================================================
//
//   IDTYPE    Get_bb_id(WN *wn) 
//          Returns the BB-id of a WHIRL statement.
//
//   IDTYPE    Get_entry_bb() 
//          Returns the BB-id the entry BB or the fake entry BB where
//          it is a multi-entry PU.
//
//   IDTYPE    Get_exit_bb() 
//          Returns the BB-id of the exit BB or the fake exit BB if
//          the PU has multiple exits.
//   
//   IDTYPE   *Get_succ_vec(IDTYPE bbx)
//          Return a null-terminated array of BB-ids that are the
//          successors of bbx.   Note that this function is overloaded
//          to return an array of exit BBs if bbx is the fake exit BB.
//
//   IDTYPE    Get_cd(IDTYPE bbx)
//          Return the immediate "control dependent" BB-id of bbx.
//          Notice that the "control dependent" has the following
//          meaning to faciliate IPA analysis.
//          a) a BB that always executes is "control-dependent" on the
//             entry BB.
//          b) statements of a DO-loop is control-dependent on the
//             loop-exit test, unless the statement is control-
//             dependent on a condition inside the loop.
//          c) if a BB is control-dependent on more than one BB 
//             (with the exception of cond b.), then it is control-
//             dependent is not represented and Get_cd() will return 0.
//             The implication is that statements in a do-while did not
//             have their CD represented.  This need to be reviewed
//             when LNO starts to handle do-while loops.
//          d) if the CD node do not dominate bbx, then Get_cd() will
//             return 0.
//
//   WN       *Get_first_stmt(IDTYPE bbx)
//          Returns the first WHIRL statement of bbx.
//
//   WN       *Get_last_stmt(IDTYPE bbx)
//          Returns the last WHIRL statement of bbx.
//
//   BOOL      CD_is_fall_thru(IDTYPE bbx)
//          Returns TRUE if bbx postdominates the fall-thru edge of
//          its CD.
//
//   BOOL      CD_is_br_taken(IDTYPE bbx)
//          Returns TRUE if bbx postdominates the taken edge of its CD.
//
//   BOOL      Dominate(IDTYPE bbx, IDTYPE bby);
//          Returns TRUE if bbx dominates bby or bbx == bby.
//          i.e., this function should not be used to identify
//          single-BB loops.
//
// ====================================================================
// ====================================================================


#ifndef opt_du_INCLUDED
#define opt_du_INCLUDED	"opt_du.h"
#ifdef _KEEP_RCS_ID
static char *opt_durcs_id = 	opt_du_INCLUDED"$Revision$";
#endif /* _KEEP_RCS_ID */

#ifndef defs_INCLUDED
#include "defs.h"
#endif

#include "opt_defs.h"

#ifndef mempool_INCLUDED
#include "mempool.h"
#endif
#ifndef optimizer_INCLUDED
#include "optimizer.h"
#endif
#ifndef wn_INCLUDED
#include "wn.h"
#endif
#ifndef wn_map_INCLUDED
#include "wn_map.h"
#endif
#ifndef cxx_base_INCLUDED
#include "cxx_base.h"
#endif

// forward declaration
class ALIAS_MANAGER;
class BB_LOOP;
class OPT_STAB;
class CFG;
class BB_SUMMARY_INFO;

class DU_NODE : public SLIST_NODE {
private:
  WN       *_wn;    // the WN node that define this memory content

  DU_NODE(const DU_NODE&);
  DU_NODE& operator = (const DU_NODE&);
  void      Init(WN *wn)               { _wn = wn; }
  void      Clear(void)                { _wn = NULL;}

public:
  DU_NODE(void);
  DU_NODE(WN *wn)                      { Init(wn); }
  ~DU_NODE(void)                       { Clear(); }

  DECLARE_SLIST_NODE_CLASS( DU_NODE )

  void      Print   (FILE *fp=stderr)
                    const;             // print the list

  WN       *Wn(void) const             { return _wn; }
};

class DEF_LIST : public SLIST {
  union {
    WN      *_loop_stmt;
    BB_LOOP *_loop_info;
  } _u1;

  enum dl_flags {
    DL_NONE		= 0x00,
    DL_INCOMPLETE	= 0x01	// is def-list incomplete?
    // next will be 0x02, 0x04, 0x08, etc.
  };
  mUINT8 _flags;	// use dl_flags values

  DECLARE_SLIST_CLASS( DEF_LIST, DU_NODE )

private:
  DEF_LIST(const DEF_LIST&);
  DEF_LIST& operator = (const DEF_LIST&);

public:
  DEF_LIST( DU_NODE *du, UINT flags )
		{ _flags = flags;
		  _u1._loop_stmt = NULL;
		  SLIST::Init(du);
		}
  ~DEF_LIST(void)
		{ _u1._loop_stmt = NULL; 
		  _flags = DL_NONE;
		}
  void     Print   (FILE *fp=stderr);  // print the list

  // Loop_stmt for LNO: According to Dror and Ding-Kai, the loop
  // statement for a use is defined as the outermost DO_LOOP
  // containing the use and whose entry is dominated by a definition
  // reaching this use.
  WN      *Loop_stmt(void) const       { return _u1._loop_stmt; }
  void     Set_loop_stmt(WN *l)        { _u1._loop_stmt = l; }

  BB_LOOP *Loop_info(void) const       { return _u1._loop_info; }
  void     Set_loop_info(BB_LOOP *l)   { _u1._loop_info = l; }

  // does the list contain this def WN?
  BOOL Contains( const WN *wn );

  // flag access
  BOOL Incomplete( void ) const	
		{ return (_flags&DL_INCOMPLETE) != 0; }
  void Set_Incomplete( void )
		{ if (this) _flags |= DL_INCOMPLETE; }
  void Reset_Incomplete( void )
		{ _flags &= ~DL_INCOMPLETE; }
};

class DEF_LIST_CONST_ITER : public SLIST_ITER {
private:
  DEF_LIST_CONST_ITER(const DEF_LIST_CONST_ITER&);
  DEF_LIST_CONST_ITER& operator = (const DEF_LIST_CONST_ITER&);

  DECLARE_SLIST_CONST_ITER_CLASS( DEF_LIST_CONST_ITER, DU_NODE, DEF_LIST )
};

class DEF_LIST_ITER : public SLIST_ITER {
private:
  DEF_LIST_ITER(const DEF_LIST_ITER&);
  DEF_LIST_ITER& operator = (const DEF_LIST_ITER&);

  DECLARE_SLIST_ITER_CLASS( DEF_LIST_ITER, DU_NODE, DEF_LIST )
};

class USE_LIST : public SLIST {
  enum ul_flags {
    UL_NONE		= 0x00,
    UL_INCOMPLETE	= 0x01	// is use-list incomplete?
    // next will be 0x02, 0x04, 0x08, etc.
  };
  mUINT8 _flags;	// use ul_flags values

  DECLARE_SLIST_CLASS( USE_LIST, DU_NODE )

private:
  USE_LIST(const USE_LIST&);
  USE_LIST& operator = (const USE_LIST&);

public:
  USE_LIST( DU_NODE *du, UINT flags )
		{ _flags = flags;
		  SLIST::Init(du);
		}
  ~USE_LIST(void)
		{ _flags = UL_NONE; }

  void Init( void )
		{ _flags = UL_NONE; }

  // does the list contain this use WN?
  BOOL Contains( const WN *wn );

  // flag access
  BOOL Incomplete( void ) const	
		{ return (_flags&UL_INCOMPLETE) != 0; }
  void Set_Incomplete( void )
		{ _flags |= UL_INCOMPLETE; }
  void Reset_Incomplete( void )
		{ _flags &= ~UL_INCOMPLETE; }

  void     Print   (FILE *fp=stderr); // print the list
};

class USE_LIST_ITER : public SLIST_ITER {
private:
  USE_LIST_ITER(const USE_LIST_ITER&);
  USE_LIST_ITER& operator = (const USE_LIST_ITER&);

  DECLARE_SLIST_ITER_CLASS( USE_LIST_ITER, DU_NODE, USE_LIST )
};

class USE_LIST_CONST_ITER : public SLIST_ITER {
private:
  USE_LIST_CONST_ITER(const USE_LIST_CONST_ITER&);
  USE_LIST_CONST_ITER& operator = (const USE_LIST_CONST_ITER&);

  DECLARE_SLIST_CONST_ITER_CLASS( USE_LIST_CONST_ITER, DU_NODE, USE_LIST )
};

class DU_MANAGER {
private:
  MEM_POOL       _mem_pool;
  WN_MAP         _du_map;
  WN_MAP         _ud_map;
  WN_MAP         _val_restored_map;   // global variable whose value is restored
  WN_MAP         _bb_map;     
  WN            *_entry_wn;
  OPT_STAB      *_opt_stab;
  ALIAS_MANAGER *_alias_mgr;
  OPT_PHASE      _opt_phase;
  BOOL		 _du_built;
  BOOL           _tracing;

  // data structure for IPA summary info
  BB_SUMMARY_INFO *_bb_summary;
  IDTYPE           _entry_bb;
  IDTYPE           _exit_bb;
  INT              _bb_cnt;

  // private verification routines
  BOOL Verify_add_wn_to_map( WN *wn, WN_MAP *wn_in_tree_map ) const;
  BOOL Verify_du_chains_in_tree( WN *wn, WN_MAP *wn_in_tree_map ) const;
  BOOL Verify_wn_in_tree(void) const;
  BOOL Verify_scalar_usage( WN *wn ) const;

  DU_MANAGER(const DU_MANAGER&);
  DU_MANAGER& operator = (const DU_MANAGER&);

public:
  DU_MANAGER(void);
  ~DU_MANAGER(void)              { WN_MAP_Delete(_du_map);
				   WN_MAP_Delete(_ud_map);
				   if (_val_restored_map != WN_MAP_UNDEFINED)
				     WN_MAP_Delete(_val_restored_map);
                                   WN_MAP_Delete(_bb_map);
				   OPT_POOL_Pop(&_mem_pool, DU_DUMP_FLAG);
				   OPT_POOL_Delete(&_mem_pool, DU_DUMP_FLAG);
                                   _opt_stab = NULL;
				   _entry_wn = NULL;
				 }

  void      Set_alias_mgr(ALIAS_MANAGER *);
  void      Set_Opt_Stab(OPT_STAB
                         *os)    { _opt_stab = os; }
  OPT_STAB *Opt_Stab(void)const  { return _opt_stab; }
  void      Set_Entry_Wn(WN *wn) { _entry_wn = wn; }
  WN       *Entry_Wn(void) const { return _entry_wn; }
  void      Set_opt_phase(
                     OPT_PHASE p){ _opt_phase = p; }
  OPT_PHASE Opt_phase(void) const{ return _opt_phase; }

  // was the DU information built?
  void      Set_du_built(void)   { _du_built = TRUE; }
  BOOL      Du_built(void) const { return _du_built; }

  void      Set_Tracing(BOOL trc){ _tracing = trc; }
  BOOL      Tracing(void) const  { return _tracing; }

  void      Print_Ud(WN *use,
		     FILE *fp=stderr); // print one U-D list
  void      Print_Du(WN *def,
		     FILE *fp=stderr); // print one D-U list

#ifdef Is_True_On
  void      Print_Du_Info(FILE *fp=stderr); // print all D-U lists
#endif

  void      Add_Def_Use( WN *def, WN *use );
  void      Delete_Def_Use( WN *def, WN *use );

  // note: these four should become private (or obsolete), so please
  //       try to use Add_Def_Use()/Delete_Def_Use() instead
  void      Ud_Add_Def(WN *use, WN *def);
  void      Du_Add_Use(WN *def, WN *use);
  void      Ud_Delete_Def(WN *use, WN *def);
  void      Du_Delete_Use(WN *def, WN *use);

  void      Ud_Put_Def(WN *use,
		       DEF_LIST *deflst) const { WN_MAP_Set(_ud_map, use,
							    deflst); }
  void      Du_Put_Use(WN *def,
		       USE_LIST *uselst) const { WN_MAP_Set(_du_map, def,
							    uselst); }
  DEF_LIST *Ud_Get_Def(WN *use)          const { return (DEF_LIST *)
						   WN_MAP_Get(_ud_map,use); }
  USE_LIST *Du_Get_Use(WN *def)          const { return (USE_LIST *)
						   WN_MAP_Get(_du_map,def); }
  void Remove_Use_From_System(WN *use);
  void Remove_Def_From_System(WN *def);

  void Create_Def_List(WN *use);
  void Create_Use_List(WN *def);

  void Du_Set_Incomplete( WN *def );

  void Set_value_restored(WN *def);

  BOOL Is_value_restored(WN *def)   { return (_val_restored_map != WN_MAP_UNDEFINED) &&
					WN_MAP32_Get(_val_restored_map, def); }

  BOOL Verify(void);


  // Temporary interface for IPA array privatization analysis.
  //   TO BE DELETE once Wilson finished the real interface.
  //
  void      Alloc_IPA_summary(CFG *cfg);
  void      Set_bb_id(WN *wn, INT bbid)  { WN_MAP32_Set(_bb_map, wn, bbid); }
  void      Collect_BB_id(WN_MAP, WN *);
  void      Collect_CFG(CFG *);

#ifdef Is_True_On
  void      Check_bb_id(IDTYPE bb_id)  { FmtAssert( bb_id < _bb_cnt,
						    ("DU_MANAGER: invalid BB id")); }
#else
  void      Check_bb_id(IDTYPE) {}
#endif

  // Interface exposed to IPA.
  IDTYPE    Get_bb_id(WN *wn)          { return WN_MAP32_Get(_bb_map, wn); }
  IDTYPE    Get_entry_bb()             { return _entry_bb; }
  IDTYPE    Get_exit_bb()              { return _exit_bb; }
  IDTYPE   *Get_succ_vec(IDTYPE);
  IDTYPE    Get_cd(IDTYPE);
  WN       *Get_first_stmt(IDTYPE);
  WN       *Get_last_stmt(IDTYPE);
  BOOL      CD_is_fall_thru(IDTYPE);
  BOOL      CD_is_br_taken(IDTYPE);
  BOOL      Dominate(IDTYPE, IDTYPE);
};

extern "C" DU_MANAGER* Create_Du_Manager(MEM_POOL *);
extern "C" void        Delete_Du_Manager(DU_MANAGER *, MEM_POOL *);
extern "C" BOOL        Du_Built(DU_MANAGER *);

#endif  // opt_emit_INCLUDED
