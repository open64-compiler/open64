/*
 * Copyright (C) 2008-2011 Advanced Micro Devices, Inc.  All Rights Reserved.
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


//-*-c++-*-
/* ====================================================================
* ====================================================================
*
* Module: opt_alias_mgr.cxx
* $Revision: 1.1.1.1 $
* $Date: 2005/10/21 19:00:00 $
* $Author: marcel $
* $Source: /proj/osprey/CVS/open64/osprey1.0/be/com/opt_alias_mgr.cxx,v $
*
* Revision history:
*  04-APR-95 lo - Split from opt_alias.cxx
*
* Description:
*
* ====================================================================
* ====================================================================
*/

#ifdef USE_PCH
#include "be_com_pch.h"
#endif /* USE_PCH */
#pragma hdrstop
#ifdef _KEEP_RCS_ID
#define opt_alias_mgr_CXX	"opt_alias_mgr.cxx"
static char *rcs_id = 	opt_alias_mgr_CXX"$Revision: 1.7 $";
#endif /* _KEEP_RCS_ID */

#include "string.h"

#include "defs.h"
#include "config.h"
#include "opt_config.h"
#include "errors.h"
#include "erglob.h"
#include "tracing.h"
#include "cxx_base.h"
#include "cxx_template.h"
#include "wn.h"
#include "wn_map.h"
#include "wn_util.h"
#include "ir_reader.h"
#include "stblock.h"
#include "opt_defs.h"
#include "opt_base.h"
#include "opt_alias_mgr.h"
#include "opt_alias_class.h"
#include "opt_alias_rule.h"
#include "opt_points_to.h"
#include "region_util.h"
#include "targ_sim.h"
#include "glob.h"
#include "pu_info.h"
#include "nystrom_alias_analyzer.h"

static BOOL in_ipa_pu_list(char *function_name);
static BOOL in_pure_call_list(char *function_name);

#define ALIAS_TRACE_FLAG  0x1000000 /* trace alias analysis for CG          */
#define ALIAS_DUMP_FLAG      0x0800 /* trace alias analysis		    */

// ====================================================================
//   Support maintaining restricted pointer attributes
// ====================================================================

class RESTRICTED_MAP {
  struct INFO {
    UINT _restricted:1;
    UINT _unique_pt:1;
    UINT _depth:3;  // must match with POINTS_TO
    ST *_based_sym;
  };
  ALIAS_MANAGER      *_am;
  WN_MAP              _map;
  MEM_POOL           *_pu_pool;
  vector<const ST *,
    mempool_allocator<const ST *> > _invalid_based_syms;

public:
#ifdef Is_True_On
  BOOL           _saved;
#endif

  RESTRICTED_MAP(MEM_POOL *pu_pool) :
    _invalid_based_syms(pu_pool),
    _am(NULL),
#if Is_True_On
    _saved(FALSE),
#endif
    _pu_pool(pu_pool),
    _map(WN_MAP_Create(pu_pool))
      { WN_MAP_Set_dont_copy(_map, TRUE);}

  ~RESTRICTED_MAP(void) { 
    WN_MAP_Delete(_map);
  }

  void Note_invalid_based_symbol(const ST *st) {
    _invalid_based_syms.push_back(st);
  }

  void Save_info(WN *);
  BOOL Restore_info(WN *, POINTS_TO *);
  void Verify_info(const WN *, const POINTS_TO *);
  void Set_alias_mgr(ALIAS_MANAGER *am) { _am = am; }
  void Erase_entry_if_invalid(WN *);
  void Remove_info(WN *);
  MEM_POOL *Pu_pool(void)  { return _pu_pool; }
};

static RESTRICTED_MAP *Restricted_map = NULL;

// Save_info can only be called from the PREOPT phase.
//
void
RESTRICTED_MAP::Save_info(WN *wn)
{
  if (wn == NULL)
    return;

  if (OPERATOR_is_scalar_iload (WN_operator (wn)) ||
      OPERATOR_is_scalar_istore (WN_operator (wn)) ||
      WN_operator(wn) == OPR_MLOAD ||
      WN_operator(wn) == OPR_MSTORE ||
      WN_operator(wn) == OPR_PARM) {
    IDTYPE id = _am->Id(wn);
    if (id > _am->Preg_id()) {
      POINTS_TO *pt = _am->Pt(id);
      if (pt != NULL && pt->Based_sym() != NULL) {
	struct INFO *pinfo = CXX_NEW(struct INFO, Pu_pool());
	pinfo->_based_sym = pt->Based_sym();
	pinfo->_depth = pt->Based_sym_depth();
	pinfo->_unique_pt = (pt->Unique_pt() != 0);
	pinfo->_restricted = (pt->Restricted() != 0);
	WN_MAP_Set(_map, wn, (void *) pinfo);
	if (Get_Trace( TP_GLOBOPT, ALIAS_TRACE_FLAG)) {
	  fprintf(TFile, "--- RESTRICTED_MAP::Save_info WN %d ST %s(%d) %s %s\n", WN_map_id(wn),
		  ST_name(pt->Based_sym()), pt->Based_sym_depth(),
		  pinfo->_restricted ? "restricted" : "",
		  pinfo->_unique_pt  ? "unique_pt"  : "");
	  fdump_tree(TFile, wn);
	}
      }
    }
  }

  // any regions in the whirl are black boxes, ignore
  if (WN_opcode(wn) == OPC_REGION) {
    RID *rid = REGION_get_rid(wn);
    Is_True(rid != NULL,("RESTRICTED_MAP::Save_info, NULL rid"));
    if (RID_level(rid) >= RL_RBI) // Save_info is called at very end of LNO
      return;
  }

  WN *stmt;
  if (WN_opcode(wn) == OPC_BLOCK) 
    for (stmt = WN_first(wn); stmt != NULL; stmt = WN_next(stmt))  
      Save_info(stmt);
  else if ( !OPCODE_is_black_box( WN_opcode(wn) ) ) {
    for (INT i = 0; i < WN_kid_count(wn); i++) 
      Save_info(WN_kid(wn,i));
  }
}

void
RESTRICTED_MAP::Erase_entry_if_invalid(WN *wn)
{
  struct INFO *pinfo = (struct INFO *) WN_MAP_Get(_map, wn);
  if (pinfo != NULL) {
    Is_True(pinfo->_based_sym != NULL,
	    ("Restricted map entry must have non-NULL based_sym"));
    if (find(_invalid_based_syms.begin(), _invalid_based_syms.end(),
	     pinfo->_based_sym) != _invalid_based_syms.end()) {
      Remove_info(wn);
    }
  }
}

// Save_info can only be called from the PREOPT phase.
//
void
RESTRICTED_MAP::Remove_info(WN *wn)
{
  WN_MAP_Set(_map, wn, NULL);
}

BOOL
RESTRICTED_MAP::Restore_info(WN *wn, POINTS_TO *pt)
{
  struct INFO *pinfo = (struct INFO *) WN_MAP_Get(_map, wn);
  if (pinfo != NULL) {
    pt->Set_expr_kind(EXPR_IS_ADDR);
    if (pinfo->_unique_pt)
      pt->Set_unique_pt();
    else
      pt->Reset_unique_pt();
    if (pinfo->_restricted)
      pt->Set_restricted();
    else
      pt->Reset_restricted();
    pt->Set_based_sym(pinfo->_based_sym);
    pt->Set_based_sym_depth(pinfo->_depth);
    if (Get_Trace( TP_GLOBOPT, ALIAS_TRACE_FLAG)) {
      fprintf(TFile, "--- RESTRICTED_MAP::Restore_info WN %d ST %s(%d) %s %s\n", WN_map_id(wn),
	      ST_name(pt->Based_sym()), pt->Based_sym_depth(),
	      pt->Restricted() ? "restricted" : "", 
	      pt->Unique_pt() ? "unique_pt" : "");
      fdump_tree_no_st(TFile, wn);
    }
    return TRUE;
  }
  return FALSE;
}

BOOL Update_From_Restricted_Map(WN *const wn, POINTS_TO *const pt)
{
  if (WOPT_Enable_Restricted_Map && Restricted_map != NULL) {
    return Restricted_map->Restore_info(wn, pt);
  }
  return FALSE;
}

void
RESTRICTED_MAP::Verify_info(const WN        *const wn,
			    const POINTS_TO *const pt)
{
#if Is_True_On
  struct INFO *pinfo = (struct INFO *) WN_MAP_Get(_map, wn);
  if (pinfo != NULL) {
    Is_True(_saved, ("RESTRICTED_MAP: Map must be saved"));
    Is_True(pt->Expr_kind() == EXPR_IS_ADDR,
	    ("RESTRICTED_MAP: Wrong Expr_kind()"));
    if (pinfo->_unique_pt) {
      Is_True(pt->Unique_pt(),
	      ("RESTRICTED_MAP: Lost Unique_pt()"));
    }
    else {
      Is_True(!pt->Unique_pt(),
	      ("RESTRICTED_MAP: Spurious Unique_pt()"));
    }
    if (pinfo->_restricted) {
      Is_True(pt->Restricted(),
	      ("RESTRICTED_MAP: Lost Restricted()"));
    }
    else {
      Is_True(!pt->Restricted(),
	      ("RESTRICTED_MAP: Spurious Restricted()"));
    }
    Is_True(pt->Based_sym() == pinfo->_based_sym,
	    ("RESTRICTED_MAP: Incorrect Based_sym()"));
    Is_True(pt->Based_sym_depth() == pinfo->_depth,
	    ("RESTRICTED_MAP: Incorrect Based_sym_depth()"));
  }
  else {
    if (_saved) {
      // We can't assert anything here because when it behaves
      // correctly, LNO removes entries from the restricted map (via
      // Note_Invalid_Based_Symbol) under some conditions (like when
      // it distributes/rehapes an array).
    }
  }
#endif
}

void
Verify_Restricted_Map(const WN *const wn, const POINTS_TO *const pt)
{
  if (WOPT_Enable_Restricted_Map && Restricted_map != NULL) {
    Restricted_map->Verify_info(wn, pt);
  }
}

// Save all restricted pointers
void Create_Restricted_Map(MEM_POOL *pu_pool)
{
  if (WOPT_Enable_Restricted_Map) {
    Restricted_map = CXX_NEW(RESTRICTED_MAP(pu_pool), pu_pool);
    // Restricted_map->Save_info(tree);
  } 
  // otherwise quietly ignore this call.
}


void Copy_Restricted_Map(WN *tree, ALIAS_MANAGER *am)
{
  if (WOPT_Enable_Restricted_Map) {
    Restricted_map->Set_alias_mgr(am);
    Restricted_map->Save_info(tree);
#if Is_True_On
    Restricted_map->_saved = TRUE;
#endif
  }
}


void Delete_Restricted_Map(void)
{
  if (WOPT_Enable_Restricted_Map && Restricted_map != NULL) {
    MEM_POOL *pu_pool = Restricted_map->Pu_pool();
    CXX_DELETE(Restricted_map, pu_pool);
    Restricted_map = NULL;
  }
}


void Erase_Restricted_Mapping(WN *wn)
{
  if (WOPT_Enable_Restricted_Map && Restricted_map != NULL) {
    // The Restricted_Map could be undefined in a nested PU created by the MP lowerer.
    // See the implementation of IPA_WN_Move_Maps_PU.
    //
    if (WN_map_id(wn) == WN_MAP_UNDEFINED)
      return;
    Restricted_map->Remove_info(wn);
  }
}

void Note_Invalid_Based_Symbol(const ST *st)
{
  if (WOPT_Enable_Restricted_Map && Restricted_map != NULL) {
    Restricted_map->Note_invalid_based_symbol(st);
  }
}

// ====================================================================
// ALIAS MANAGER constructor.
// It set up its own memory pool.
//
ALIAS_MANAGER::ALIAS_MANAGER(WN *entryWN)
{
  MEM_POOL_Initialize(&_mem_pool, "ALIAS_pool", FALSE);
  MEM_POOL_Push(&_mem_pool);

  // Note: C++ is broken in that it uses the same preprocessor as C,
  // and the C preprocessor sees '<' as "less than" instead of "begin
  // template argument". Therefore it seems we can't use CXX_NEW
  // properly to construct instances of template classes, so I've had
  // to introduce a stupid typedef. Yucky.

  typedef vector<IDTYPE, mempool_allocator<IDTYPE> > STUPID_COMPILER;
  _invalid_ip_alias_classes =
    CXX_NEW(STUPID_COMPILER(&_mem_pool), &_mem_pool);

  // initialized default context
  ALIAS_CONTEXT ac = (DEFAULT_COMMON_RULES | DEFAULT_ANALYSIS_RULES | DEFAULT_COMPATIABILITY_RULES);
  // Select default language rules.
  switch (PU_src_lang(Get_Current_PU())) {
  case PU_F77_LANG:
    ac |= DEFAULT_F_RULES;
    if (Alias_Pointer_Parms)
      ac |= F_PARM_RULE;
    if (Alias_Pointer_Cray)
      ac |= F_CRAY_POINTER_RULE;
    break;
  case PU_F90_LANG:
    ac |= DEFAULT_F_RULES;
    if (Alias_Pointer_Parms) 
      ac |= F_PARM_RULE;
    if (Alias_Pointer_Cray)
      ac |= F_CRAY_POINTER_RULE;
    ac |= DEFAULT_F90_RULES;
    break;
  case PU_C_LANG:
    ac |= DEFAULT_C_RULES;
    if (Alias_Pointer_Types)
      ac |= C_ANSI_RULE;
    if (Alias_Pointer_Strongly_Typed)
      ac |= C_STRONGLY_TYPED_RULE;
    if (Alias_Pointer_Named_Data)
      ac |= RAG_UNNAMED_RULE;
    if (Alias_Pointer_Restricted)
      ac |= RAG_RESTRICTED_RULE;
    if (Alias_Pointer_Disjoint)
      ac |= IBM_DISJOINT_RULE;
    break;
  case PU_CXX_LANG:
    ac |= DEFAULT_C_RULES;
    ac |= DEFAULT_CXX_RULES;
    if (Alias_Pointer_Types)
      ac |= C_ANSI_RULE;
    if (Alias_Pointer_Strongly_Typed)
      ac |= C_STRONGLY_TYPED_RULE;
    if (Alias_Pointer_Named_Data)
      ac |= RAG_UNNAMED_RULE;
    if (Alias_Pointer_Restricted)
      ac |= RAG_RESTRICTED_RULE;
    if (Alias_Pointer_Disjoint)
      ac |= IBM_DISJOINT_RULE;

    break;
  default:
    Is_True(FALSE, ("Language is unknown; mixed-language inlining illegal."));
  }

  // Here we create the AliasAnalyzer object, which serves as the
  // interface to the selected alias analysis algorithm.
  AliasAnalyzer::Create_Alias_Analyzer(ac,entryWN);

  Set_pu_context(ac);
  _rule = CXX_NEW(ALIAS_RULE(ac,AliasAnalyzer::aliasAnalyzer()), &_mem_pool);

  // Setup the trace flags.
  _trace = Get_Trace(TP_GLOBOPT, ALIAS_DUMP_FLAG);

  // Setup a dynamic array that is indexed by alias_id and
  //   points to a POINTS_TO structure.
  // The 0-entry is not used because the WN_mapping uses 0 to 
  // indicate un-mapped entries.
  // The 1-entry is reserved to be used by all PREGs.
  //
  _vec = CXX_NEW(DYN_ARRAY<POINTS_TO*>(&_mem_pool), &_mem_pool);
  _vec->Initidx(_preg_id);   // do not use the 0-entry and 1-entry.
  _map = WN_MAP32_Create(&_mem_pool);
  _homing_map = WN_MAP32_Create(&_mem_pool);
  _last_alias_id = _preg_id;   // starting from preg id.
  _no_alias_info_id = New_alias_id();

  // initialize the shared POINTS_TO that represents no alias information.
  POINTS_TO *npt = Pt(_no_alias_info_id);
  npt->Set_id(_no_alias_info_id);
  npt->Init(); 
  npt->Set_expr_kind(EXPR_IS_ADDR);
}

// ALIAS MANAGER destructor.
//   free all memory used in the alias manager;
//
ALIAS_MANAGER::~ALIAS_MANAGER(void) 
{
  CXX_DELETE(_invalid_ip_alias_classes, &_mem_pool);
  WN_MAP_Delete(_map);
  WN_MAP_Delete(_homing_map);
  MEM_POOL_Pop(&_mem_pool);
  MEM_POOL_Delete(&_mem_pool);
}

//  Obtain the alias_id of a WN node from the mapping.
IDTYPE 
ALIAS_MANAGER::Id(const WN *wn) const
{ 
  return WN_MAP32_Get(_map, wn); 
}

//  Assign an alias_id to the WN node by updating the mapping.
inline void     
ALIAS_MANAGER::Set_id(WN *wn, IDTYPE id) const 
{
  WN_MAP32_Set(_map, wn, id);
}

//  Set the current alias context.
inline void
ALIAS_MANAGER::Set_context(ALIAS_CONTEXT c)  
{ 
  _rule->Set_context(c);
}

//  Create a new alias id and a POINTS_TO structure.
//   Initialize the points_to to INVALID.
//   Append the points_to the dynamic array.
inline IDTYPE
ALIAS_MANAGER::New_alias_id(void) 
{
  IDTYPE id = ++_last_alias_id; 
  Vec()->Newidx();
  POINTS_TO *pt = CXX_NEW(POINTS_TO, &_mem_pool); 
  pt->Init();
  (*_vec)[id] = pt;
  pt->Set_id(id);
  return id;
}

inline POINTS_TO *
ALIAS_MANAGER::New_points_to(WN *wn)
{
  IDTYPE id = ++_last_alias_id; 
  Vec()->Newidx();
  POINTS_TO *pt = CXX_NEW(POINTS_TO, &_mem_pool); 
  pt->Init();
  (*_vec)[id] = pt;
  pt->Set_id(id);
  Set_id(wn,id);
  return pt;
}


#ifdef Is_True_On
static void 
Check_self_alias(ALIAS_MANAGER *am, WN *wn)
{
  // The following is for debugging.
  //    an object must alias to itself
  FmtAssert(Aliased(am, wn, wn), ("WN is not aliased to itself."));

  OPERATOR opr = WN_operator(wn);
  IDTYPE id = am->Id(wn);
  POINTS_TO *pt = (id != 0) ? am->Pt(id) : NULL;

  // an indirect memop must alias to itself with the LNO definition,
  // unless it is const-qualified.
  if ((OPERATOR_is_scalar_iload (opr) || OPERATOR_is_scalar_istore (opr)) &&
      !TY_is_const(WN_object_ty(wn)) &&
      !(pt != NULL && pt->Const()))
    FmtAssert(Overlapped_base(am, wn, wn), ("Indirect is not aliased to itself."));
}
#else
#define Check_self_alias(am,wn)
#endif


inline static bool Is_PREG_ldst(WN *wn)
{
  OPERATOR opr = WN_operator(wn);
  return ((OPERATOR_is_scalar_load (opr) || OPERATOR_is_scalar_store (opr)) &&
	  ST_sclass(WN_st(wn)) == SCLASS_REG);
}


//  Identify the alias id for the WN node.
//  If there is no alias_id assigned for the node, create a new one.
void
ALIAS_MANAGER::Gen_alias_id(WN *wn, POINTS_TO *pt)
{
  if (pt != NULL) {
    WN_MAP32_Set(WN_MAP_ALIAS_CLASS, wn, pt->Ip_alias_class());
 
    // Restore the WN to CGNodeId map
    NystromAliasAnalyzer *naa = static_cast<NystromAliasAnalyzer *>
                                (AliasAnalyzer::aliasAnalyzer());
    if (naa) 
    {
      AliasTag tag = pt->Alias_tag();
      // If the WN is not mapped to a CGNodeId, use the AliasTag from
      // the POINTS_TO to get the CGNodeId using the NystromAliasAnalyzers's
      // AliasTag to CGNodeId map that was constructed during createAliasTags
      if (WN_MAP32_Get(WN_MAP_ALIAS_CGNODE, wn) == 0) {
        OPERATOR opr = WN_operator(wn);
        if (OPERATOR_is_scalar_istore(opr) ||
            OPERATOR_is_scalar_iload(opr) ||
            OPERATOR_is_scalar_load(opr) ||
            OPERATOR_is_scalar_store(opr) ||
            opr == OPR_MSTORE ||
            opr == OPR_MLOAD) {
          CGNodeId id = naa->cgNodeId(tag);
          if (id != 0) {
            if (opr == OPR_ILDBITS || opr == OPR_MLOAD || opr == OPR_ILOAD)
              WN_MAP32_Set(WN_MAP_ALIAS_CGNODE, WN_kid0(wn), id);
            else
              WN_MAP32_Set(WN_MAP_ALIAS_CGNODE, wn, id);
          }
        }
      }
      // Set the WNs alias tag from the POINTS_TO
      if (tag >= InitialAliasTag)
        naa->setAliasTag(wn,pt->Alias_tag());
    }
  }

  if (!WOPT_Enable_CG_Alias) {
    Set_id(wn, No_alias_info_id());
    return;
  }

  if (Is_PREG_ldst(wn)) {
    Set_id(wn, Preg_id());
    return;
  }

  IDTYPE id;
  Is_True(pt != NULL && pt->Expr_kind() != EXPR_IS_INVALID, ("POINTS_TO is invalid."));
  if ((id = pt->Id()) == 0) {
    // Create a new alias_id if none is found.
    id = New_alias_id();
    POINTS_TO *npt = Pt(id);
    npt->Copy_fully(pt);
    pt->Set_id(id);
    npt->Set_id(id);
    Set_id(wn, id);
    if (_trace) {
      fprintf(TFile, "gen_alias_id<%d(map %d)>\n", id, WN_map_id(wn));
      pt->Print(TFile);
      fdump_tree(TFile, wn);
      fprintf(TFile,"aliased_with<%d,{",id);
      for (INT32 oldid = Preg_id() + 1; oldid <= id; oldid++) {
	if (Rule()->Aliased_Memop(Pt(oldid), Pt(id), Pt(oldid)->Ty(), Pt(id)->Ty()))
	  fprintf(TFile,"%d ", oldid);
      }
      fprintf(TFile, "}>\n");
    }
  } else {
    Set_id(wn, id);
    if (_trace) {
      fprintf(TFile, "set_alias_id<%d(map %d)>\n", id, WN_map_id(wn));
      pt->Print(TFile);
      fdump_tree(TFile, wn);
    }
  }
  
  Check_self_alias(this,wn);
}


// Generate a contagious section of POINTS_TO to represent
//  BARRIER and DEALLOCA
void ALIAS_MANAGER::Gen_alias_id_list(WN *wn, POINTS_TO_LIST *ptl)
{
  if (ptl == NULL) {
    Set_id(wn,0);
    return;
  }
  POINTS_TO_ITER def_iter;
  POINTS_TO_NODE *defn;
  IDTYPE first_id = 0;
  POINTS_TO *npt = NULL;
  FOR_ALL_NODE( defn, def_iter, Init(ptl)) {
    POINTS_TO *pt = defn->Pt();
    IDTYPE id = New_alias_id();
    if (first_id == 0) first_id = id;
    npt = Pt(id);
    npt->Copy_fully(pt);
    npt->Set_id(id);
    npt->Set_extended();
  }
  Set_id(wn, first_id);
  if (npt) npt->Reset_extended();
}

//  Duplicate any alias info from old_wn tree to new_wn.
void
ALIAS_MANAGER::Dup_tree_alias_id( const WN *old_wn, WN *new_wn )
{
  const OPCODE opc = WN_opcode(old_wn);

  Is_True( opc == WN_opcode(new_wn),
    ("ALIAS_MANAGER::Dup_tree_alias_id: non-matching trees") );

  // copy the id information over, if any is available for this node
  IDTYPE old_id = Id(old_wn);
  if ( old_id != 0 ) {
    Set_id( new_wn, old_id );
  }

  IDTYPE ip_alias_class = WN_MAP32_Get(WN_MAP_ALIAS_CLASS, old_wn);
  if (ip_alias_class != 0) {
    WN_MAP32_Set(WN_MAP_ALIAS_CLASS, new_wn, ip_alias_class);
  }

  AliasAnalyzer *aa = AliasAnalyzer::aliasAnalyzer();
  if (aa)
    aa->transferAliasTag(new_wn,old_wn);

  // now travel down the tree
  if ( opc == OPC_BLOCK ) {
    WN *old_bwn, *new_bwn;
    for ( old_bwn = WN_first(old_wn), new_bwn = WN_first(new_wn); 
	  old_bwn != NULL; 
	  old_bwn = WN_next(old_bwn), new_bwn = WN_next(new_bwn) )
    {
      Dup_tree_alias_id( old_bwn, new_bwn );
    }
  }
  else {
    // copy the homing information
    if ( OPCODE_is_load(opc) ) {
      Set_homing_load( new_wn, Homing_load(old_wn) );
    }
    else if ( OPCODE_is_store(opc) ) {
      Set_homing_store( new_wn, Homing_store(old_wn) );
    }

    for ( INT ikid = 0; ikid < WN_kid_count(old_wn); ikid++ ) {
      Dup_tree_alias_id( WN_kid(old_wn,ikid), WN_kid(new_wn,ikid) );
    }
  }
}


// Visit all nodes in the black-box statement.
// Assume all load/stores are aliased to everything.
void
ALIAS_MANAGER::Gen_black_box_alias(WN *wn)
{
  const OPCODE opc = WN_opcode(wn);
  if (OPCODE_is_load(opc) || OPCODE_is_store(opc)) 
    Set_id(wn, No_alias_info_id());
  
  for (INT i = 0; i < WN_kid_count(wn); i++) {
    Gen_black_box_alias( WN_kid(wn,i));
  }
}

// ************************************************************************
// Support for cross-DSO use of inlined functions. Some ALIAS_MANAGER
// member functions exist only to support alias classification in
// WOPT, and therefore they are defined in wopt.so (alternatives are
// to tolerate linking be.so with unresolved symbols, or to move all
// of alias classification into be.so; neither of these is a good
// idea). Those member functions defined in wopt.so need to call
// ALIAS_MANAGER::New_alias_id and ALIAS_MANAGER::Set_id, which are
// qualified as "inline" in opt_alias_mgr.h and therefore cannot be
// called across DSO's. Rather than sacrifice the compile-time
// performance we would lose by changing those member functions to be
// out-of-line, we define out-of-line versions here that are strictly
// for cross-DSO use. Their use by wopt.so is rather rare, and
// currently happens only at -O3.
// ************************************************************************

IDTYPE
ALIAS_MANAGER::Cross_dso_new_alias_id(void)
{
  return New_alias_id();
}

void
ALIAS_MANAGER::Cross_dso_set_id(WN *wn, IDTYPE id) const
{
  Set_id(wn, id);
}

// ************************************************************************
//   C INTERFACE TO IPA/LNO/CG
// ************************************************************************

//  Create an alias manager
ALIAS_MANAGER *Create_Alias_Manager(MEM_POOL *pu_pool, WN *entryWN)
{
  return CXX_NEW(ALIAS_MANAGER(entryWN), pu_pool);
}

//  Delete the alias manager
void Delete_Alias_Manager(ALIAS_MANAGER *am, MEM_POOL *pu_pool)
{
  CXX_DELETE (am, pu_pool);
}

//  Return the default alias context for the PU
//    IPA would need to get the default context to
//    attach it to the inlined region.
ALIAS_CONTEXT Get_Default_Alias_Context(ALIAS_MANAGER *am)
{
  return am->Pu_context();
}

//  Set the alias context for the current context.
void Set_Alias_Context(ALIAS_MANAGER *am, ALIAS_CONTEXT context)
{
  am->Set_context(context);
}

//  Revert the alias context to the PU context
void Reset_Alias_Context(ALIAS_MANAGER *am)
{
  am->Set_context(am->Pu_context());
}


//  Create an alias id for an WHIRL load/store expression.
//
void Create_alias(ALIAS_MANAGER *am, WN *wn)
{
  if (Is_PREG_ldst(wn)) {
    am->Set_id(wn, am->Preg_id());
  } else {
    POINTS_TO *pt = am->New_points_to(wn);
    pt->Analyze_WN_expr(wn);
    pt->Set_ty (WN_object_ty(wn)); // OSP_172
  }
}


//  Create an alias id for a new local variable
//  that is assumed to be not address taken, not passed.
//
void Create_local_alias(ALIAS_MANAGER *am, WN *wn)
{
  if (Is_PREG_ldst(wn)) {
    am->Set_id(wn, am->Preg_id());
  } else {
    POINTS_TO *pt = am->New_points_to(wn);
    pt->Analyze_WN_expr(wn);
    Is_True(pt->Local(), ("Create local alias:  not a local variable."));
  }
}


//  Create an alias id for a local/global variable
//  that is assumed to be not address taken, not passed.
//
void Create_global_alias(ALIAS_MANAGER *am, ST *st, WN *ldid, WN *iload)
{
  if (ldid != NULL) {
    Is_True(st == WN_st(ldid), 
	    ("Create_global_alias: ST and LDID does not match."));
    POINTS_TO *pt = am->New_points_to(ldid);
    pt->Analyze_WN_expr(ldid);
    pt->Set_not_addr_saved();
    pt->Set_not_addr_passed();
  }
  if (iload != NULL) {
    POINTS_TO *pt = am->New_points_to(iload);
    pt->Analyze_WN_expr(iload);

    if (WOPT_Enable_Restricted_Map && Restricted_map != NULL) {
      Restricted_map->Remove_info(iload);
    }
  }
}


//  Create a pair of alias id for the Fortran formal parameter
//
void Create_formal_alias(ALIAS_MANAGER *am, ST *st, WN *formal_addr, WN *formal)
{
  Is_True(ST_sclass(st) == SCLASS_FORMAL,
	  ("ST is not a formal parameter."));
  
  if (formal_addr != NULL) {
    Is_True(st == WN_st(formal_addr), 
	    ("Create_formal_alias: ST and LDID does not match."));
    POINTS_TO *pt = am->New_points_to(formal_addr);
    pt->Analyze_WN_expr(formal_addr);
    pt->Set_not_addr_saved();
    pt->Set_not_addr_passed();
    Is_True(pt->Local(), ("Create formal alias: not an addr of formal variable."));
  }
  
  if (formal != NULL) {
    POINTS_TO *pt = am->New_points_to(formal);
    pt->Analyze_WN_expr(formal);
    Is_True(pt->F_param(), ("Create formal alias: not a formal variable."));
  }
}


//  Create an alias id for a iload/istore accessing a
//  pointer to unique memory, i.e., malloc-ed memory.
//
void Create_unique_pointer_alias(ALIAS_MANAGER *am, ST *st, WN *ldid, WN *iload)
{
  Is_True( ST_pt_to_unique_mem(st), ("Create_pointer: ST is not PT_TO_UNIQUE_MEM."));

  if (ldid != NULL) {
    POINTS_TO *pt = am->New_points_to(ldid);
    pt->Analyze_WN_expr(ldid);
    Is_True(pt->Local(), ("Create unique pointer alias: not a local variable."));
  }

  if (iload != NULL) {
    POINTS_TO *pt = am->New_points_to(iload);
    pt->Analyze_WN_expr(iload);
    Is_True(pt->Unique_pt() && pt->Based_sym() != NULL,
	    ("Create unique pointer alias: not a local variable."));

    if (WOPT_Enable_Restricted_Map && Restricted_map != NULL) {
      Restricted_map->Remove_info(iload);
    }
  }
}


//  Create an alias id for a iload/istore accessing thru an LDA.
//
void Create_lda_array_alias(ALIAS_MANAGER *am, WN *lda, WN *iload)
{
  Is_True( WN_operator(lda) == OPR_LDA, ("Create_lda_array_alias:  need a LDA node."));

  if (iload != NULL) {
    ST *st = WN_st(lda);
    POINTS_TO *pt = am->New_points_to(iload);
    pt->Analyze_WN_expr(iload);
    pt->Set_byte_size(TY_size(ST_type(st)));
    if (TY_size(ST_type(st)) == 0) pt->Set_ofst_kind(OFST_IS_UNKNOWN);
    Is_True(pt->Base_kind() == BASE_IS_FIXED, 
	    ("Create_lda_array_alias: base is not fixed."));

    if (WOPT_Enable_Restricted_Map && Restricted_map != NULL) {
      Restricted_map->Remove_info(iload);
    }
  }
}


//  Returns TRUE if the variable has no alias
//
BOOL No_alias(const ALIAS_MANAGER *am, WN *wn)
{
  IDTYPE id = am->Id(wn);
  if (id == 0)
    return FALSE;   // we have no idea
  if (id == am->Preg_id()) 
    return TRUE;    // preg has no alias
  POINTS_TO *pt = am->Pt(id);
  return pt->No_alias();
}


//  Returns TRUE if the variable has a valid alias mapping
//
BOOL Valid_alias(const ALIAS_MANAGER *am, WN *wn)
{
  if (WN_map_id(wn) != -1)
  {
    IDTYPE id = am->Id(wn);

    return (id == 0) ? FALSE : TRUE;
  }
  return FALSE;
}

static
BOOL equivalent_struct(TY_IDX ty1, TY_IDX ty2)
{
    if (ty1 == ty2 ||
        (!TY_anonymous(ty1) && TY_name_idx(ty1) == TY_name_idx(ty2) &&
         TY_kind(ty1) == TY_kind(ty2) && TY_kind(ty1) == KIND_STRUCT)) { 
        return TRUE;
    }    

    return FALSE;
}


//  Alias analysis for two WN *
//
ALIAS_RESULT Aliased(const ALIAS_MANAGER *am, WN *wn1, WN *wn2,
                     BOOL ignore_loop_carried) 
{
  IDTYPE id1 = am->Id(wn1);
  IDTYPE id2 = am->Id(wn2);
  
  // Assign Preg_id to LDID/STID of pregs
  if (id1 == 0 && Is_PREG_ldst(wn1)) 
    am->Set_id(wn1, id1 = am->Preg_id());

  if (id2 == 0 && Is_PREG_ldst(wn2)) 
    am->Set_id(wn2, id2 = am->Preg_id());
  
  if (FILE_INFO_ipa(File_info))
  {
    // if -ipa is in effect, and if wn1 or wn2 is a call wn, further check that
    // the function called by wn1 or wn2 is not a user-defined function (seen by
    // ipa), in which case it may be a nice library function and does not alias
    char *called_pu_name;
    if (WN_operator(wn1) == OPR_CALL)
    {
      called_pu_name = ST_name(WN_st_idx(wn1));
      if (!in_ipa_pu_list(called_pu_name) &&
          in_pure_call_list(called_pu_name))
        return NOT_ALIASED;
    }
    if (WN_operator(wn2) == OPR_CALL)
    {
      called_pu_name = ST_name(WN_st_idx(wn2));
      if (!in_ipa_pu_list(called_pu_name) &&
          in_pure_call_list(called_pu_name))
        return NOT_ALIASED;
    }
  }

  // Complain if the WNs have no alias information.
  if (id1 == 0 || id2 == 0) 
    return POSSIBLY_ALIASED;

  //  If both are PREGs, they are not alias if
  //  their base ST is different or their offset is different.
  if (id1 == am->Preg_id() && id2 == am->Preg_id()) 
    return (WN_offset(wn1) != WN_offset(wn2)) ? NOT_ALIASED : SAME_LOCATION;

  //  If one of the WN is PREG and the other isn't, then
  //  they do not alias.
  if ((id1 == am->Preg_id() && id2 != am->Preg_id()) ||
      (id1 != am->Preg_id() && id2 == am->Preg_id())) 
    return NOT_ALIASED;
  
  POINTS_TO *pt1 = am->Pt(id1);
  POINTS_TO *pt2 = am->Pt(id2);
    
  Is_True(pt1 != NULL && pt2 != NULL, ("Aliased: null points to"));

  // If the id is the same, they share the same POINTS_TO.
  // If POINTS_TO is NULL (it has no alias other than itself.)
  if (id1 == id2) 
    return am->Rule()->Same_location(wn1, wn2, pt1, pt2) ? SAME_LOCATION : POSSIBLY_ALIASED;

  if (Alias_Pointer_Strongly_Typed &&
      (OPERATOR_is_scalar_iload (WN_operator(wn1)) ||
       OPERATOR_is_scalar_istore (WN_operator(wn1))) &&
      (OPERATOR_is_scalar_iload (WN_operator(wn2)) ||
       OPERATOR_is_scalar_istore (WN_operator(wn2)))) {
    TY_IDX ty1 = OPERATOR_is_load (WN_operator(wn1)) ?
	WN_load_addr_ty(wn1) : WN_ty(wn1);
    TY_IDX ty2 = OPERATOR_is_load (WN_operator(wn2)) ?
	WN_load_addr_ty(wn2) : WN_ty(wn2);
    if (ty1 != (TY_IDX) NULL && TY_kind(ty1) == KIND_POINTER &&
	ty2 != (TY_IDX) NULL && TY_kind(ty2) == KIND_POINTER &&
	!am->Rule()->Aliased_Strongly_Typed_Rule(TY_pointed(ty1), TY_pointed(ty2))) {
      return NOT_ALIASED;
    }
  }

  if (OPERATOR_is_store(WN_operator(wn1)) && OPERATOR_is_load(WN_operator(wn2)) ||
      OPERATOR_is_store(WN_operator(wn2)) && OPERATOR_is_load(WN_operator(wn1))) {
  
    // when the high level type show that pt1 and pt2 are two structs with same name, these
    // two structs could be equivelant type, thus could not rely on their high level type
    if (equivalent_struct(pt1->Highlevel_Ty(),pt2->Highlevel_Ty())) {
        if (am->Rule()->Aliased_Memop(pt1, pt2, pt1->Ty(), pt2->Ty(), ignore_loop_carried)) // OSP-172
          return POSSIBLY_ALIASED;
    }else {
        if (am->Rule()->Aliased_Memop(pt1, pt2, ignore_loop_carried))
          return POSSIBLY_ALIASED;
    }      
        
  } else {
    // cannot apply ANSI type rule to STORE <--> STORE.
    if (am->Rule()->Aliased_Memop(pt1, pt2, (TY_IDX)NULL, (TY_IDX)NULL, 
                                  ignore_loop_carried)) 
      return POSSIBLY_ALIASED;
  }
  return NOT_ALIASED;
}


ALIAS_RESULT
ALIAS_MANAGER::Aliased(const POINTS_TO *pt1, const POINTS_TO *pt2,
                       BOOL ignore_loop_carried)
{
  if (Rule()->Aliased_Memop(pt1, pt2, (TY_IDX) NULL, (TY_IDX) NULL, 
                            ignore_loop_carried)) 
    return POSSIBLY_ALIASED;
  return NOT_ALIASED;
}

ALIAS_RESULT
ALIAS_MANAGER::Aliased(WN *wn, const POINTS_TO *pt2,
                       BOOL ignore_loop_carried)
{
  IDTYPE id = Id(wn);
  
  // Assign Preg_id to LDID/STID of pregs
  if (id == 0 && Is_PREG_ldst(wn)) 
    Set_id(wn, id = Preg_id());
  
  if (id == Preg_id() &&
      pt2->Base_is_fixed() &&
      ST_sclass(pt2->Base()) == SCLASS_REG) {
    return (WN_offset(wn) != pt2->Byte_Ofst()) ? NOT_ALIASED : SAME_LOCATION;
  }
  return Aliased(Pt(id), pt2, ignore_loop_carried);
}

ALIAS_RESULT
ALIAS_MANAGER::Aliased(const POINTS_TO *pt1, WN *wn, 
                       BOOL ignore_loop_carried)
{
  return Aliased(wn, pt1, ignore_loop_carried);
}

void
ALIAS_MANAGER::Note_invalid_ip_alias_class(const WN *wn)
{
  IDTYPE alias_class = WN_MAP32_Get(WN_MAP_ALIAS_CLASS, wn);
  if (alias_class != PESSIMISTIC_AC_ID &&
      alias_class != OPTIMISTIC_AC_ID) {
    if (Get_Trace( TP_GLOBOPT, ALIAS_TRACE_FLAG)) {
      fprintf(TFile, "Alias manager: Noting IP alias class %d invalid\n",
	      alias_class);
    }
    _invalid_ip_alias_classes->push_back(alias_class);
  }
}

void
ALIAS_MANAGER::Erase_ip_alias_class_if_invalid(WN *wn)
{
  IDTYPE alias_class = WN_MAP32_Get(WN_MAP_ALIAS_CLASS, wn);
  if (find(_invalid_ip_alias_classes->begin(),
	   _invalid_ip_alias_classes->end(),
	   alias_class) != _invalid_ip_alias_classes->end()) {
    if (Get_Trace( TP_GLOBOPT, ALIAS_TRACE_FLAG)) {
      fprintf(TFile, "Alias manager: Invalidating IP alias class info on\n");
      fdump_tree(TFile, wn);
    }
    WN_MAP32_Set(WN_MAP_ALIAS_CLASS, wn, PESSIMISTIC_AC_ID);
  }
}

//  Alias analysis between memops and calls
//
ALIAS_RESULT Aliased_with_region(const ALIAS_MANAGER *am, const WN *wn, const WN *region_or_call, READ_WRITE how)
{
  IDTYPE id = am->Id(wn);
  if (id == 0) return POSSIBLY_ALIASED;  // assume aliased 

  if (id == am->Preg_id()) {
    if (OPERATOR_is_scalar_store (WN_operator(wn)) ||
	OPERATOR_is_scalar_load (WN_operator(wn))) {
      INT32 reg = WN_offset(wn);
      if (reg > Last_Dedicated_Preg_Offset)
	return NOT_ALIASED;
    }
    return POSSIBLY_ALIASED;  // don't know what kind of registers, be conservative.
  }

  POINTS_TO *pt = am->Pt(id);
  if (OPERATOR_is_call(WN_operator(region_or_call))) {

    if (Is_nested_call(region_or_call))
      return POSSIBLY_ALIASED;

    ST *call_st = NULL;
    if (WN_operator(region_or_call) == OPR_CALL)
      call_st = WN_st(region_or_call);
    else if (WN_operator(region_or_call) == OPR_PICCALL) 
      call_st = WN_st(region_or_call);

    if (call_st == NULL) 
      return POSSIBLY_ALIASED;

    // Test aliasing with the CALL
    READ_WRITE how_aliased =
      am->Rule()->Aliased_with_Call(call_st, WN_call_flag(region_or_call), pt);
    if (how_aliased != NO_READ_NO_WRITE &&
	(how_aliased == READ_AND_WRITE || how == how_aliased)) 
      return POSSIBLY_ALIASED;
    
    // Test aliasing with call-by-ref parameters
    for (INT32 i = 0; i < WN_kid_count(region_or_call); i++) {
      WN *wn = WN_kid(region_or_call,i);  
      if (WN_operator(wn) == OPR_PARM && 
	  (WN_Parm_By_Reference(wn)||WN_Parm_Dereference(wn))) {
	// check aliasing
	IDTYPE id2 = am->Id(wn);
	if (id2 == 0) return POSSIBLY_ALIASED;  // assume aliased 
	POINTS_TO *pt2 = am->Pt(id2);
	if (am->Rule()->Aliased_Memop(pt, pt2, (TY_IDX) NULL, (TY_IDX) NULL))
	  return POSSIBLY_ALIASED;
      }
    }
    return NOT_ALIASED;
  }

  OPERATOR opr = WN_operator(region_or_call);

  if (opr == OPR_DEALLOCA && pt->Not_alloca_mem())
    return NOT_ALIASED;

  if (opr == OPR_FORWARD_BARRIER ||
      opr == OPR_BACKWARD_BARRIER ||
      opr == OPR_DEALLOCA) {
    IDTYPE id = am->Id(wn);
    if (id != 0) {
      POINTS_TO *pt2;
      do {
	pt2 = am->Pt(id++);
	if (am->Rule()->Aliased_Memop(pt, pt2, (TY_IDX) NULL, (TY_IDX) NULL))
	  return POSSIBLY_ALIASED;
      } while (pt2->Extended());
      return NOT_ALIASED;
    }
  }

  if (opr == OPR_ASM_STMT) {
    if (am->Rule()->Aliased_with_Asm(region_or_call, pt) == NO_READ_NO_WRITE)
      return NOT_ALIASED;
  }

  return POSSIBLY_ALIASED;
}


// Alias analysis between intrinsic-op and memops
//
ALIAS_RESULT Aliased_with_intr_op(const ALIAS_MANAGER *am, const WN *intr_op, const WN *mem_op)
{
  Is_True(WN_operator(intr_op) == OPR_INTRINSIC_OP,
	  ("Aliased_with_intr_op: expecting OPR_INTRINSIC_OP."));
  
  IDTYPE id1 = am->Id(mem_op);
  if (id1 == am->Preg_id())    // register is not used by the intr_op
    return NOT_ALIASED;

  POINTS_TO *pt1 = am->Pt(id1);

  // go through call-by-ref all parameters
  for (INT32 i = 0; i < WN_kid_count(intr_op); i++) {
    WN *wn = WN_kid(intr_op,i);  
    if (WN_Parm_By_Reference(wn) || WN_Parm_Dereference(wn)) {
      // check aliasing
      IDTYPE id2 = am->Id(wn);
      if (id2 == 0) return POSSIBLY_ALIASED;  // assume aliased 
      POINTS_TO *pt2 = am->Pt(id2);
      if (am->Rule()->Aliased_Memop(pt1, pt2, (TY_IDX) NULL, (TY_IDX) NULL))
	return POSSIBLY_ALIASED;
    }
  }
  return NOT_ALIASED;
}


//  Update the ofst field if the WN node is in the form of
//       LDA ...
//     ARRAY ...
//   ILOAD/ISTORE.
//
//  only called from Overlapped_base().
//
//  We intentionally do not handle the case that the base is a LDID
//  because it should already be handled in Aliased_Memop().
//
static void
Check_range(POINTS_TO *pt, const WN *wn)
{
  if (OPERATOR_is_scalar_iload (WN_operator(wn)) ||
      OPERATOR_is_scalar_istore (WN_operator(wn))) {
    WN *wn_tmp = OPERATOR_is_load (WN_operator(wn)) ?
	WN_kid0(wn) : WN_kid1(wn);
    WN *wn_lda = NULL;
    if (WN_operator(wn_tmp) == OPR_ARRAY) {
      if (WN_operator(WN_kid0(wn_tmp)) == OPR_LDA)
	wn_lda = WN_kid0(wn_tmp);
    } else if (WN_operator(wn_tmp) == OPR_LDA) 
      wn_lda = wn_tmp;

    if (wn_lda != NULL) {
      mINT64 ofst = 0;
      ST *st = WN_st(wn_lda);
      ST *base = st;
      Expand_ST_into_base_and_ofst(st, 0, &base, &ofst);

      // From Mike Murphy:
      // ST_size is really TY_size(ST_type())
      // so it only works if ST_type() works.
      // The only things that don't have types are blocks (like .data),
      // so ST_size only works if ST_class != CLASS_BLOCK.

      if (ST_class(st) != CLASS_BLOCK && TY_size(ST_type(st)) > 0) {
	pt->Set_ofst_kind(OFST_IS_FIXED);
	pt->Set_byte_ofst(ofst);
	pt->Set_byte_size(TY_size(ST_type(st)));
      }
    }
  } else if (OPERATOR_is_scalar_store (WN_operator(wn)) ||
	     OPERATOR_is_scalar_load (WN_operator(wn))) {
    mINT64 ofst = 0;
    ST *st = WN_st(wn);
    ST *base = st;
    Expand_ST_into_base_and_ofst(st, 0, &base, &ofst);
      
    if (ST_class(st) != CLASS_BLOCK && TY_size(ST_type(st)) > 0) {
      pt->Set_ofst_kind(OFST_IS_FIXED);
      pt->Set_byte_ofst(ofst);
      pt->Set_byte_size(TY_size(ST_type(st)));
    }
  }
}

//  Determine if two array reference overlap.
//  Only handle simple forms.
//
ALIAS_RESULT Overlapped_base(const ALIAS_MANAGER *am, const WN *wn1, const WN *wn2)
{
  IDTYPE id1 = am->Id(wn1);
  IDTYPE id2 = am->Id(wn2);
  if (id1 == 0 || id2 == 0) 
    return POSSIBLY_ALIASED;

  if (id1 == am->Preg_id() || id2 == am->Preg_id()) 
    return POSSIBLY_ALIASED;

  POINTS_TO pt1, pt2;
  pt1.Copy_fully(am->Pt(id1));
  pt2.Copy_fully(am->Pt(id2));

  // if either base is based on some expression,
  // then remove any information related to the base.
  if (pt1.Base_kind() == BASE_IS_DYNAMIC)
    pt1.Set_base_kind(BASE_IS_UNKNOWN);
  if (pt2.Base_kind() == BASE_IS_DYNAMIC)
    pt2.Set_base_kind(BASE_IS_UNKNOWN);

  //  Pretend we know nothing about the offset field, so that alias analysis
  //  will determine dependence solely on base, TY ... information.
  //
  // if (!pt1.Base_is_fixed())
  pt1.Set_ofst_kind(OFST_IS_UNKNOWN);
  // if (!pt2.Base_is_fixed())
  pt2.Set_ofst_kind(OFST_IS_UNKNOWN);

  if (am->Rule()->Aliased_Memop(&pt1, &pt2, WN_object_ty(wn1), WN_object_ty(wn2))) {
    if (pt1.Same_base(&pt2)) {
      //  if the base is the same, then try to analyze the array range
      //  i.e., this would handle sections of a common block.
      Check_range(&pt1, wn1);
      Check_range(&pt2, wn2);

      // Convert the following assertion to return POSSILBY_ALIASED.
      // Fix 379050.  LNO converts a pointer expr into nested ARRAY expr
      // that wopt does not handle.  We will simply return POSSILBY_ALIASED, but it
      // should not affect performance because LNO does not handle them too.
      // Is_True(pt1.Base() == pt2.Base(), ("Overlapped_base: Inconsistent data layout detected."));
      if (pt1.Base() != pt2.Base())
	return POSSIBLY_ALIASED;
      
      if (pt1.Overlap(&pt2))
	return POSSIBLY_ALIASED;
      else
	return NOT_ALIASED;
    }
    return POSSIBLY_ALIASED;
  } else
    return NOT_ALIASED;
}


//   Copy alias info from one WN to the other.
//
void Copy_alias_info(const ALIAS_MANAGER *am, WN *wn1, WN *wn2) 
{
  const OPCODE opc1 = WN_opcode(wn1);
  const OPCODE opc2 = WN_opcode(wn2);

  WN_MAP32_Set(WN_MAP_ALIAS_CLASS, wn2,
	       WN_MAP32_Get(WN_MAP_ALIAS_CLASS, wn1));

  AliasAnalyzer *aa = AliasAnalyzer::aliasAnalyzer();
  if (aa)
    aa->transferAliasTag(wn2,wn1);

  IDTYPE id = am->Id(wn1);
  if (id == 0) {
    OPERATOR opr = OPCODE_operator(opc1);
    if ((OPERATOR_is_scalar_load (opr) ||
	 OPERATOR_is_scalar_store (opr)) &&
	ST_sclass(WN_st(wn1)) == SCLASS_REG) {
      id = am->Preg_id();
      am->Set_id(wn1, id);
    } else if (opr == OPR_PARM && !WN_Parm_By_Reference(wn1) && !WN_Parm_Dereference(wn1)) {
      // It has no alias info.
      am->Set_id(wn2, 0);    // cancel the original alias info in wn2
      return;
    } else {
      am->Set_id(wn2, 0);    // cancel the original alias info in wn2
      return;
    }
  }
  am->Set_id(wn2, id);

  // copy homing information
  if ( OPCODE_is_load(opc1) && OPCODE_is_load(opc2) ) {
    am->Set_homing_load( wn2, am->Homing_load(wn1) );
  }
  else if ( OPCODE_is_store(opc1) && OPCODE_is_store(opc2) ) {
    am->Set_homing_store( wn2, am->Homing_store(wn1) );
  }
}


//   Duplicate the alias info during unrolling.
//
void Duplicate_alias_info(ALIAS_MANAGER *am, WN *wn1, WN *wn2)
{
  const OPCODE opc1 = WN_opcode(wn1);
  const OPCODE opc2 = WN_opcode(wn2);

  WN_MAP32_Set(WN_MAP_ALIAS_CLASS, wn2,
	       WN_MAP32_Get(WN_MAP_ALIAS_CLASS, wn1));

  AliasAnalyzer *aa = AliasAnalyzer::aliasAnalyzer();
  if (aa)
    aa->transferAliasTag(wn2,wn1);

  // copy homing information
  if ( OPCODE_is_load(opc1) && OPCODE_is_load(opc2) ) {
    am->Set_homing_load( wn2, am->Homing_load(wn1) );
  }
  else if ( OPCODE_is_store(opc1) && OPCODE_is_store(opc2) ) {
    am->Set_homing_store( wn2, am->Homing_store(wn1) );
  }

  IDTYPE id = am->Id(wn1);
  if (id == 0) {
    OPERATOR opr = OPCODE_operator(opc1);
    if ((OPERATOR_is_scalar_load (opr) ||
	 OPERATOR_is_scalar_store (opr)) &&
	ST_sclass(WN_st(wn1)) == SCLASS_REG) {
      id = am->Preg_id();
      am->Set_id(wn1, id);
    } else if (opr == OPR_PARM && !WN_Parm_By_Reference(wn1) && !WN_Parm_Dereference(wn1)) {
      // It has no alias info.
      am->Set_id(wn2, 0);    // cancel the original alias info in wn2
      return;
    } else {
      am->Set_id(wn2, 0);    // cancel the original alias info in wn2
      return;
    }
  }
  if (id == am->Preg_id()) {  // transfer the Preg alias id
    am->Set_id(wn2, id);
  } else {
    POINTS_TO *pt = am->Pt(id);
    if (pt) {
      IDTYPE new_id = am->New_alias_id();
      POINTS_TO *npt = am->Pt(new_id);
      am->Set_id(wn2, new_id);
      npt->Copy_fully(pt);
      npt->Set_id(new_id);
      if (!npt->Base_is_fixed()) {
	npt->Set_base_kind(BASE_IS_UNKNOWN);
      }
    }
  }
}


void Note_Invalid_IP_Alias_Class(ALIAS_MANAGER *am, const WN *wn)
{
  am->Note_invalid_ip_alias_class(wn);
}

void Invalidate_Persistent_Alias_Info(ALIAS_MANAGER *am, WN *wn)
{
  am->Erase_ip_alias_class_if_invalid(wn);
  if (WOPT_Enable_Restricted_Map && Restricted_map != NULL) {
    Restricted_map->Erase_entry_if_invalid(wn);
  }
    if (WN_opcode(wn) == OPC_BLOCK) {
    WN *wn2;
    for (wn2 = WN_first(wn);
	 wn2 != NULL;
	 wn2 = WN_next(wn2)) {
      Invalidate_Persistent_Alias_Info(am, wn2);
    }
  }
  else {
    for (INT i = 0; i < WN_kid_count(wn); i++) {
      Invalidate_Persistent_Alias_Info(am, WN_kid(wn, i));
    }
  }
}

//  Convert an element access (wn1) into vector access (wn2)
void
Create_vector_alias(ALIAS_MANAGER *am, WN *wn1, WN *wn2)
{
  IDTYPE id1 = am->Id(wn1);
  if (id1 == 0) {
    am->Set_id(wn2, 0);
    return;
  }
  POINTS_TO *pt1 = am->Pt(id1);
  POINTS_TO *pt2 = am->New_points_to(wn2);
  pt2->Copy_fully(pt1);
  pt2->Set_ofst_kind(OFST_IS_UNKNOWN);
}


//  Dump the alias id on the tree, and print out what it alias with
void
ALIAS_MANAGER::Print( const WN *wn, FILE *fp ) const
{
  INT i;
  fprintf( fp, "\nFINAL WHIRL TREE DUMP WITH ALIAS ID:\n");
  fdump_tree_with_alias( fp, wn, _map, this );

  fprintf( fp, "\nPOINTS_TO TABLE DUMP:\n");
  for ( i = Preg_id()+1; i <= Vec()->Lastidx(); i++) {
    fprintf( fp, " %d: ", i );
    Pt(i)->Print(fp);
  }

  fprintf( fp, "\nALIAS ARC DUMP:\n");
  for ( i = Preg_id()+1; i <= Vec()->Lastidx(); i++) {
    fprintf( fp, "aliased_with<%d,{", i);
    for (INT32 oldid = Preg_id() + 1; oldid <= i; oldid++) {
      if (Rule()->Aliased_Memop(Pt(oldid), Pt(i), Pt(oldid)->Ty(), Pt(i)->Ty(), TRUE))
        fprintf( fp,"%d ", oldid);
    }
    fprintf( fp, "}>\n");
  }

  if (WN_opcode(wn) == OPC_REGION) {
    RID *rid = REGION_get_rid(wn);
    Is_True(rid != NULL, ("ALIAS_MANAGER::Print, NULL RID"));
    if (!RID_TYPE_mp(rid) && !RID_TYPE_eh(rid)) {
      fprintf(fp, "\nREGION BOUNDARY SETS:\n");
      RID_set_print(fp, rid);
    }
  }
}


//  C interface to ALIAS_MANAGER::Print
//
void Dump_alias_mgr(const struct ALIAS_MANAGER *am, const WN *tree, FILE *fp)
{
  am->Print(tree, fp);
}


// Print the alias information into a buffer
//
void Print_alias_info(char *buf, const ALIAS_MANAGER *am, const WN *wn)
{
  IDTYPE alias_id = am->Id(wn);

  //  when there is no alias, return emtpy string.
  if (alias_id == 0) {
    buf[0] ='\0';
    return;
  }

  POINTS_TO *pt = am->Pt(alias_id);

  if (pt->Expr_kind() == EXPR_IS_ADDR && pt->Base_kind() == BASE_IS_FIXED) {
    const char* st_name = ST_class(pt->Base()) == CLASS_CONST ? "<constant>" : ST_name(pt->Base());
    
    if (pt->Ofst_kind() == OFST_IS_FIXED) 
      sprintf(buf, "id:%d %s+0x%llx",
	      alias_id, 
	      ST_class(pt->Base())==CLASS_VAR ? st_name : "not_variable",
	      pt->Byte_Ofst());
    else
      sprintf(buf, "id:%d %s", alias_id, st_name);
  } else if (pt->F_param() && pt->Based_sym() != NULL) {
    sprintf(buf, "id:%d parm:%s", alias_id, ST_name(pt->Based_sym()));
  } else if (pt->Unique_pt() && pt->Based_sym() != NULL) {
    sprintf(buf, "id:%d uniq:%s", alias_id, ST_name(pt->Based_sym()));
  } else 
    sprintf(buf, "id:%d", alias_id);
}


//  Returns TRUE if the object (variable or array)
//  will be allocated and therefore safe for speculative
//  reference.
//
BOOL ALIAS_MANAGER::Safe_to_speculate(const WN *wn) const
{
  IDTYPE id = Id(wn);

  // no alias info, conservatively return FALSE;
  if (id == 0) {
   return FALSE; 
  }
  // WN accesses a register, return TRUE;
  if (id == Preg_id()) {
    return TRUE;
  }
  // The variable can be speculately loaded if there is a
  // fixed ST and the ofst and size is non-zero.
  // The check of non-zero size is for the case of extern  
  //
  POINTS_TO *pt = Pt(id);
  if (pt->Expr_kind() == EXPR_IS_ADDR &&
      pt->Base_kind() == BASE_IS_FIXED &&
      pt->Ofst_kind() == OFST_IS_FIXED &&
      pt->Safe_to_speculate()) {
    return TRUE;
  }
  return FALSE;
}


BOOL ALIAS_MANAGER::May_refer_to_alloca_mem(const WN *wn) const
{
  IDTYPE alias_id = Id(wn);
#ifdef KEY
  // no alias info, conservatively return TRUE;
  if (alias_id == 0) {
   return FALSE; 
  }
#endif
  POINTS_TO *pt = Pt(alias_id);
  if (_trace) {
    fprintf(TFile, "--- Checking for pointing to alloca memory:\n");
    fdump_tree_with_alias(TFile, wn, _map, this);
    fprintf(TFile, "   %s point to alloca mem\n",
	    pt->Not_alloca_mem() ? "does not" : "may");
  }
  return !pt->Not_alloca_mem();
}

BOOL May_refer_to_alloca_mem(const struct ALIAS_MANAGER *am, const WN *wn)
{
  return am->May_refer_to_alloca_mem(wn);
}


// ====================================================================
// Support "homing" information for CG
// ====================================================================

extern "C"
BOOL Homing_Load( const ALIAS_MANAGER *am, const WN *load_wn )
{
  return am->Homing_load(load_wn);
}

extern "C"
void Set_Homing_Load( ALIAS_MANAGER *am, WN *load_wn )
{
  am->Set_homing_load(load_wn,TRUE);
}

extern "C"
void Reset_Homing_Load( ALIAS_MANAGER *am, WN *load_wn )
{
  am->Set_homing_load(load_wn,FALSE);
}

extern "C"
BOOL Homing_Store( const ALIAS_MANAGER *am, const WN *store_wn )
{
  return am->Homing_store(store_wn);
}

extern "C"
void Set_Homing_Store( ALIAS_MANAGER *am, WN *store_wn )
{
  am->Set_homing_store(store_wn,TRUE);
}

extern "C"
void Reset_Homing_Store( ALIAS_MANAGER *am, WN *store_wn )
{
  am->Set_homing_store(store_wn,FALSE);
}




// C interface to get at POINTS_TO
// used when generating POINTS_TO list for region boundary in CG
//
extern "C"
POINTS_TO *Points_to(ALIAS_MANAGER *am, WN *wn)
{
  IDTYPE id = am->Id(wn);
  return am->Pt(id);
}

// used to copy the POINTS_TO from the alias manager's mempool to
// the REGION_mem_pool (REGION_add_wn_points_to and RBI::Add_To_PT_Set)
extern "C"
POINTS_TO *Points_to_copy(POINTS_TO *pt, MEM_POOL *rpool)
{
  POINTS_TO *tmp = CXX_NEW(POINTS_TO, rpool);
  tmp->Copy_fully(pt);
  return tmp;
}

// Is the function whose name is function_name in the list of functions seen by
// ipa?
static BOOL in_ipa_pu_list(char *function_name)
{
  PU_Info *pu;

  for (pu = (PU_Info *)Global_PU_Tree; pu != NULL; pu = PU_Info_next(pu))
  {
    char *pu_name = ST_name(PU_Info_proc_sym(pu));
    if (strcmp(pu_name, function_name) == 0)
      return TRUE;
  }
  return FALSE;
}

// Is the function whose name is function_name in the list of nice library
// functions?
static BOOL in_pure_call_list(char *function_name)
{
  if (strcmp(function_name, "malloc") == 0)
    return TRUE;
  if (strcmp(function_name, "calloc") == 0)
    return TRUE;
  if (strcmp(function_name, "realloc") == 0)
    return TRUE;
  if (strcmp(function_name, "printf") == 0)
    return TRUE;
  if (strcmp(function_name, "fprintf") == 0)
    return TRUE;
  if (strcmp(function_name, "exit") == 0)
    return TRUE;
  if (strcmp(function_name, "free") == 0)
    return TRUE;
  
  // add more
  return FALSE;
}
