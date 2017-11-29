/*
 * Copyright (C) 2009 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

//-*-c++-*-

/*
 * Copyright 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
 */

// ====================================================================
// ====================================================================
//
// Module: opt_dse.cxx
// $Revision: 1.1.1.1 $
// $Date: 2005/10/21 19:00:00 $
// $Author: marcel $
// $Source: /proj/osprey/CVS/open64/osprey1.0/be/opt/opt_dse.cxx,v $
//
// Revision history:
//  21-DEC-94 streich - Original Version
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
// Perform Dead-Store Elimination (dse) for the preopt phase.
// This is actually part of the SSA class.
//
// ====================================================================
// ====================================================================


#ifdef USE_PCH
#include "opt_pch.h"
#endif // USE_PCH
#pragma hdrstop


#ifdef _KEEP_RCS_ID
#define opt_dse_CXX	"opt_dse.cxx"
static char *rcs_id = 	opt_dse_CXX"$Revision: 1.17 $";
#endif /* _KEEP_RCS_ID */

#include "defs.h"
#include "opt_config.h"
#include "errors.h"
#include "erglob.h"
#include "tracing.h"

#include "opt_base.h"
#include "opt_cfg.h"
#include "opt_exc.h"
#include "opt_mu_chi.h"
#include "opt_ssa.h"
#include "opt_mu_chi.h"
#include "opt_util.h"
#include <vector> 
using std::vector;
#include "opt_alias_rule.h"
#include "idx_32_set.h"

// ====================================================================
// ====================================================================

class DSE {
  private:
    
    CFG *_cfg;			// handle on control-flow graph
    OPT_STAB *_opt_stab;	// handle on symbol table
    EXC      *_exc;
    MEM_POOL *_loc_pool;	// memory pool to allocate temporarily
    WN_MAP _live_wns;		// map of live WNs

    BOOL _tracing;		// are we tracing?
#ifdef TARG_SL
    vector <WN *> *_injury_aux_intrnop;
#endif
    vector <WN *> *_last_store_vec;
    vector <IDX_32_SET *> * _alias_aux_vec;

    //
    // access methods
    //
    CFG *Cfg( void ) const
		{ return _cfg; }
    OPT_STAB *Opt_stab( void ) const
		{ return _opt_stab; }
    MEM_POOL *Loc_pool( void ) const
		{ return _loc_pool; }
    BOOL Tracing( void ) const
		{ return _tracing; }

    //
    // dse support functions
    //
    BOOL Required_call( const WN *wn ) const;
    BOOL Required_istore( const WN *wn ) const;
    BOOL Required_stid( const WN *wn ) const;
    BOOL Required_load( const WN *wn ) const;
    BOOL Required_stmt( const WN *wn ) const;
    BOOL Check_Required_Expr( WN *wn ) const;
    BOOL Is_deleted_statement( WN *stmt ) const;

    void Set_Required_VSE( VER_STAB_ENTRY *, BOOL , WN * ) const;
    void Set_Required_PHI( VER_STAB_ENTRY *vse, WN *ref_wn ) const;
    void Set_Required_MU( MU_NODE *mu, BOOL real_use ) const;
    void Set_Required_CHI( CHI_NODE *chi, BOOL *chi_is_live ) const;
    void Set_Required_WN( WN *wn ) const;
    void Add_EH_exposed_use(WN *call) const;
#ifdef KEY
    void Add_entry_exposed_uses(WN *call) const;
#endif
    void Update_MU_list_for_call(BB_NODE *bb) const;
    // some inlined functions
    BOOL Live_wn( WN *wn ) const
		{ return WN_MAP32_Get( _live_wns, wn ); }
    void Set_live_wn( WN *wn ) const
		{ WN_MAP32_Set( _live_wns, wn, 1 ); }

#if defined(TARG_SL)
    void Repair_Injured_AuxIntrnOP(void) const ;
    void Append_Injured_AuxIntrnOp(WN *wn) const { _injury_aux_intrnop->insert(_injury_aux_intrnop->begin(), wn);};
#endif
    WN* Last_store(AUX_ID aid) const
              { return (*_last_store_vec)[aid]; }
    void Set_last_store( AUX_ID aid, WN *store) const 
              { (*_last_store_vec)[aid] = store; }
    BOOL Aliased_aux( AUX_ID id1, AUX_ID id2) const
              {
                if((*_alias_aux_vec)[id1]->MemberP(id2))
                  return TRUE;
                else
                  return FALSE;
              }
    //OSP_468, remove all Set_Required_Imp_VSE()
    //void Set_Required_Imp_VSE( VER_ID vid, BOOL real_use) const;
    BOOL Mem_WN_equiv_rec(WN *wn1, WN *wn2) const;
    BOOL Mem_WN_equiv(WN *wn1, WN* wn2) const;
    BOOL Same_memloc( WN* store1, WN* store2) const;
    VER_ID Prop_vsym_new_result( VER_ID vid ) const;
    void Propagate_vsym_wn( WN *wn ) const;
    void Propagate_vsym_bb( BB_NODE *bb ) const;

  public:

    DSE( CFG *cfg, OPT_STAB *opt_stab, MEM_POOL *pool, EXC *exc, BOOL tracing )
      : _cfg(cfg), _opt_stab(opt_stab), _loc_pool(pool), _exc(exc),
	_tracing(tracing)
      {
	// create a map to track live WNs
	_live_wns = WN_MAP32_Create(Loc_pool());
#ifdef TARG_SL
        _injury_aux_intrnop = CXX_NEW(vector<WN *>, pool);
#endif

        //init _last_store_vec
        INT asym_count = Opt_stab()->Lastidx() + 1;
        _last_store_vec = CXX_NEW(vector<WN *>, pool);
        _last_store_vec->insert(_last_store_vec->end(), asym_count, (WN*)NULL);

        //init _alias_set_vec
        _alias_aux_vec = CXX_NEW(vector<IDX_32_SET*>, pool);
        for( INT aid = 0; aid < asym_count; aid++) {
          IDX_32_SET *alias_set = CXX_NEW( IDX_32_SET(asym_count,  pool, OPTS_FALSE), pool);
          _alias_aux_vec->push_back(alias_set);
        }
        for(INT aid = 1; aid<asym_count; aid++) {
          (*_alias_aux_vec)[aid]->Union1D(aid);
          POINTS_TO *apt = Opt_stab()->Points_to(aid);
          for(INT nid = aid +1; nid<asym_count; nid++) {
            if(Opt_stab()->Rule()->Aliased_Memop( Opt_stab()->Points_to(nid), apt)) {
              (*_alias_aux_vec)[aid]->Union1D(nid);
              (*_alias_aux_vec)[nid]->Union1D(aid);
            }
          }
        }       
      }

    ~DSE( void )
      {
	// get rid of our live-wn tracking map
	WN_MAP_Delete( _live_wns );
      }

    void Dead_store_elim( void ) const;
    void Add_MU_list_for_calls( void ) const;

}; // end of class DSE


// ====================================================================
// Determine if the given WN call must be executed
// ====================================================================

BOOL
DSE::Required_call( const WN *wn ) const
{
  // todo note: see if the callee has side-effects.  If not, then it's
  // not a required call.
  Warn_todo("DSE::Required_call see if the callee has side-effects");
  return ( TRUE );
}

// ====================================================================
// Determine if the given WN store must be executed
// Handles ISTORE,MSTORE
// ====================================================================

BOOL
DSE::Required_istore(const WN *wn) const
{
  TY_IDX ty = WN_ty(wn);

  if (Ilod_TY_is_volatile(ty))
    return TRUE;

  // istore to unique pt
  OCC_TAB_ENTRY *occ = Opt_stab()->Get_occ(wn);
  if (occ->Points_to()->Unique_pt() ||
      occ->Points_to()->Restricted())
    return TRUE;

#ifdef KEY // deleting fetch of MTYPE_M return value can cause lowerer to omit
  	   // inserting the fake parm
  if (Opt_stab()->Phase() == PREOPT_IPA0_PHASE && WN_desc(wn) == MTYPE_M &&
      WN_opcode(WN_kid0(wn)) == OPC_MMLDID) {
    ST *s = Opt_stab()->St(Opt_stab()->Du_aux_id(WN_ver(WN_kid0(wn))));
    if (ST_class(s) == CLASS_PREG && Preg_Is_Dedicated(WN_offset(wn)))
      return TRUE;
  }
#endif

  return FALSE;
}

// ====================================================================
// Required_stid
//
// Determine if the given WN stid must be executed
// ====================================================================

BOOL
DSE::Required_stid( const WN *wn ) const
{
  TY_IDX ty = WN_ty(wn);

  // maybe this access is cast to volatile...
  if ( Lod_TY_is_volatile(ty) ) 
    return TRUE;

  // may be variable declared volatile
  IDTYPE du = WN_ver(wn);	// pointer to ver_stab
  if ( Opt_stab()->Du_is_volatile(du) )
    return TRUE;

  // store to physical reg is required (must be function return reg)
  ST *s = Opt_stab()->St(Opt_stab()->Du_aux_id(du));
  if (ST_class(s) == CLASS_PREG && Preg_Is_Dedicated(WN_offset(wn)))
    return TRUE;

  if (ST_sclass(s) == SCLASS_FORMAL)  // leave the details to DCE
    return TRUE;   

#ifdef KEY // bugs 5401 and 5267
  if (Opt_stab()->Aux_stab_entry(Opt_stab()->Du_aux_id(du))->Mp_no_dse())
    return TRUE;   
#endif

#ifdef KEY // deleting fetch of MTYPE_M return value can cause lowerer to omit
  	   // inserting the fake parm
  if (Opt_stab()->Phase() == PREOPT_IPA0_PHASE && WN_desc(wn) == MTYPE_M &&
      WN_opcode(WN_kid0(wn)) == OPC_MMLDID) {
    s = Opt_stab()->St(Opt_stab()->Du_aux_id(WN_ver(WN_kid0(wn))));
    if (ST_class(s) == CLASS_PREG && Preg_Is_Dedicated(WN_offset(wn)))
      return TRUE;
  }
#endif
#ifdef TARG_SL
  if (WN_Intrinsic_OP_Slave(WN_kid0(wn))) {
    Append_Injured_AuxIntrnOp((WN *)wn);
  }
#endif
  
  return FALSE;
}

// ====================================================================
// Determine if the given WN load must be executed
// Handles LDID,MLOAD,ILOAD
// ====================================================================

BOOL
DSE::Required_load( const WN *wn ) const
{
  TY_IDX ty = WN_ty(wn);
  if ( Lod_TY_is_volatile(ty) )
    return TRUE;

  const OPERATOR opr = WN_operator(wn);
  switch ( opr ) {
    case OPR_LDID:
    case OPR_LDBITS:
      {
	// may be variable declared volatile
	IDTYPE du = WN_ver(wn);	// pointer to ver_stab
	if ( Opt_stab()->Du_is_volatile(du) )
	  return TRUE;
      }
      break;

    case OPR_MLOAD:
      {
	// may point to a volatile object
	Is_True( TY_kind(ty) == KIND_POINTER,
	  ("Required_load: ty in MLOAD not pointer") );

	if (Ilod_TY_is_volatile(ty))
	  return TRUE;
      }
      break;

    case OPR_ILOAD:
    case OPR_ILOADX:
    case OPR_ILDBITS:
      {
	TY_IDX load_addr_ty = WN_load_addr_ty(wn);
	// may point to a volatile object
	Is_True( TY_kind(load_addr_ty) == KIND_POINTER,
	  ("Required_load: load_addr_ty in ILOAD not pointer") );

	if ( Ilod_TY_is_volatile(load_addr_ty) )
	  return TRUE;
      }
      break;

    default:
      Is_True( FALSE,
	("Required_load: unknown load: %s", 
	 OPCODE_name(WN_opcode(wn))) );
      break;
  }

  return FALSE;
}


// ====================================================================
// Determine if the WN statement must be executed, and therefore all
// of the variables it references must be defined
// ====================================================================

BOOL 
DSE::Required_stmt( const WN *wn ) const
{
  if ( ! WOPT_Enable_Dse_Aggressive )
    return TRUE;
  OPERATOR oper = WN_operator(wn);

  // handle those cases we immediately know about
  switch ( oper ) {
    case OPR_AGOTO:
    case OPR_ASSERT:
    case OPR_BACKWARD_BARRIER:
    case OPR_TRUEBR:
    case OPR_FALSEBR:
    case OPR_COMPGOTO:
    case OPR_FORWARD_BARRIER:
    case OPR_GOTO:
    case OPR_IO:
    case OPR_LABEL:
    case OPR_PRAGMA:
    case OPR_REGION:
    case OPR_RETURN:
    case OPR_RETURN_VAL:
    case OPR_PREFETCH:
    case OPR_PREFETCHX:
      return TRUE;

    case OPR_CALL:
    case OPR_ICALL:
    case OPR_INTRINSIC_CALL:
      return Required_call( wn );

    case OPR_MSTORE:
    case OPR_ISTORE:
    case OPR_ISTBITS:
    case OPR_ISTOREX:
      return Required_istore( wn );

    case OPR_STID:
    case OPR_STBITS:
      return Required_stid( wn );

    default:
      return TRUE;
  }
}


// ====================================================================
// Recursively set the flags that say this VER_STAB_ENTRY is required
// ====================================================================

void 
DSE::Set_Required_VSE( VER_STAB_ENTRY *vse, BOOL real_use, WN *ref_wn ) const
{
  // This line exists in PSC3.2. But it causes problems in OSP
  //if ( vse->Real_use() ) return;

  if (real_use)
    vse->Set_Real_use();

  // we only need to propagate this usage if the symbol
  // was not already marked as having a use
  if ( vse->Any_use() ) return;
  
  BOOL vse_live=TRUE;
  // now recursively follow this symbol's use-def chain
  switch ( vse->Type() ) {
    case WHIRL_STMT:
      vse->Set_Any_use();
      Set_Required_WN( vse->Wn() );
      break;
    case PHI_STMT:
      vse->Set_Any_use();
      if (ref_wn != NULL)
	vse->Set_ref_wn(ref_wn);
      Set_Required_PHI( vse, ref_wn );
      break;
    case CHI_STMT:
      Set_Required_CHI( vse->Chi(), &vse_live );
      break;
    case ENTRY_STMT:
      // no definition, value is just live-in to the region
      break;

    case MU_STMT:   // mu could not define anything
    case NO_STMT:
    default:
      ErrMsg( EC_Misc_Int, "Set_Required_VSE invalid type",
	vse->Type() );
      break;
  }

  if ( Tracing() ) {
    if(vse_live)
      fprintf( TFile, "<dse> Required VSE: var:%d version:%d\n",
	     vse->Aux_id(), vse->Version() );
    else
      fprintf( TFile, "<dse> Not Required VSE: var:%d version:%d\n",
	     vse->Aux_id(), vse->Version() );
  }
}

// ====================================================================
// Recursively set the flags that say this phi function and the 
// variables it references are necessary.
// ====================================================================

void 
DSE::Set_Required_PHI( VER_STAB_ENTRY *vse, WN *ref_wn ) const
{
  PHI_NODE *phi = vse->Phi();
  if ( phi->Live() ) {
    // phi processed already, but see if we have a ref_wn
    if ( vse->Ref_wn() != NULL || ref_wn == NULL ) {
      // we have all the info we can possibly gain, so we're done
      return;
    }
  }

  phi->Set_live();
  for ( INT32 opndnum = 0; opndnum < phi->Size(); opndnum++ ) {
    VER_ID phi_opnd = phi->Opnd(opndnum);
    VER_STAB_ENTRY *sym = Opt_stab()->Ver_stab_entry(phi_opnd);
    Set_last_store( sym->Aux_id(), NULL);
    Set_Required_VSE( sym, FALSE, ref_wn );
  }
}

// ====================================================================
// Recursively set the flags that say this mu statement and the 
// variables it references are necessary.
// ====================================================================

void 
DSE::Set_Required_MU( MU_NODE *mu, BOOL real_use ) const
{
  VER_STAB_ENTRY *ver = Opt_stab()->Ver_stab_entry(mu->Opnd());
  Set_last_store(ver->Aux_id(), NULL);

  // there may be implied mu-nodes, so that we need to traverse the alias set 
  const BS      *alias_set = Opt_stab()->Rule()->Alias_Set_Indirect( Opt_stab());
  for (AUX_ID idx = BS_Choose( alias_set );
    idx != (AUX_ID) BS_CHOOSE_FAILURE;
    idx = BS_Choose_Next ( alias_set, idx )) {
  
    // Volatile do not appear in any mu and chi
    if (!Opt_stab()->Aux_stab_entry(idx)->Is_volatile() ||
      Opt_stab()->Aux_stab_entry(idx)->Is_virtual() ){
      Set_last_store(idx, NULL);
    }
  }
  
  Set_Required_VSE( ver, real_use, NULL );
}



static bool Is_identity_asgn(WN *wn, OPT_STAB *opt_stab)
{
  OPERATOR opr = WN_operator(wn);
  if (! OPERATOR_is_scalar_store(opr)) return FALSE;
  WN *rhs = WN_kid(wn,0);
  if ((opr == OPR_STID && WN_operator(rhs) != OPR_LDID) ||
      (opr == OPR_STBITS && WN_operator(rhs) != OPR_LDBITS))
      return FALSE;
      
  return (opt_stab->Ver_stab_entry(WN_ver(wn))->Aux_id() ==
	  opt_stab->Ver_stab_entry(WN_ver(rhs))->Aux_id());
}

BOOL
DSE::Mem_WN_equiv_rec(WN *wn1, WN *wn2) const
{
  if (!wn1 || !wn2) return FALSE;
  if (!Mem_WN_equiv(wn1,wn2)) {
    return FALSE;
  }
  for (INT i=0; i<WN_kid_count(wn1); i++) {
    if (!Mem_WN_equiv_rec(WN_kid(wn1,i),WN_kid(wn2,i))) {
      return FALSE;
    }
  }
  return TRUE;
}

BOOL
DSE::Mem_WN_equiv(WN *wn1, WN *wn2) const
{
  if (!WN_Equiv(wn1, wn2)) 
    return FALSE;

  OPERATOR opr = WN_operator(wn1);
  if(OPERATOR_has_field_id(opr)) {
    if(WN_field_id(wn1) != WN_field_id(wn2))
      return FALSE;
  }

  if(opr == OPR_STBITS || opr == OPR_ISTBITS || opr == OPR_LDBITS || opr == OPR_ILDBITS) {
    if( WN_bit_offset(wn1) != WN_bit_offset(wn2))
      return FALSE;
    if( WN_bit_size(wn1) != WN_bit_size(wn2))
      return FALSE;
  }

  if (WN_has_mu(wn1, Cfg()->Rgn_level())) {
    MU_NODE *mu1 = Opt_stab()->Get_occ(wn1)->Mem_mu_node();
    MU_NODE *mu2 = Opt_stab()->Get_occ(wn2)->Mem_mu_node();
    if(mu1 != NULL && mu2 != NULL && mu1->Opnd() != mu2->Opnd() ) 
      return FALSE;
    if((mu1==NULL) != (mu2 == NULL))
      return FALSE;
  }

  return TRUE;
}

BOOL
DSE::Same_memloc( WN* store1, WN* store2) const
{
  FmtAssert(OPERATOR_is_scalar_istore(WN_operator(store1)) ||
                     OPERATOR_is_scalar_store (WN_operator(store1)) ||
                     WN_operator(store1) == OPR_MSTORE, 
                    ("DSE::Same_memloc: store1 is not istore"));
  FmtAssert(OPERATOR_is_scalar_istore(WN_operator(store2)) ||
                     OPERATOR_is_scalar_store (WN_operator(store2)) ||
                     WN_operator(store2) == OPR_MSTORE, 
                    ("DSE::Same_memloc: store2 is not istore"));
  
  OCC_TAB_ENTRY *occ1 = Opt_stab()->Get_occ(store1);
  OCC_TAB_ENTRY *occ2 = Opt_stab()->Get_occ(store2);
  FmtAssert(occ1 != NULL && occ2 != NULL, ("DSE::Same_memloc: occ == NULL"));
  POINTS_TO *pt1 = occ1->Points_to();
  POINTS_TO *pt2 = occ2->Points_to();
  FmtAssert(pt1 != NULL && pt2 != NULL, ("DSE::Same_memloc: points_to == NULL"));

  if (Opt_stab()->Rule()->Same_location(store1, store2, pt1, pt2)) {
#if defined(TARG_NVISA)
    //for dynamic array, we need to be more conservative
    INT i;
    TY_IDX ty;
    ty = ST_type(pt1->Base()); 
    if (TY_kind(ty) == KIND_ARRAY) {
      for (i = 0; i < TY_AR_ndims(ty); i++) {
        if ( !TY_AR_const_lbnd(ty, i) ||
             !TY_AR_const_ubnd(ty, i) )
          return FALSE;
      }
    }

    ty = ST_type(pt2->Base());
    if (TY_kind(ty) == KIND_ARRAY) {
      for (i = 0; i < TY_AR_ndims(ty); i++) {
        if ( !TY_AR_const_lbnd(ty, i) ||
             !TY_AR_const_ubnd(ty, i) )
          return FALSE;
      }
    }
#endif
    return TRUE;
  }
  else if (Mem_WN_equiv(store1, store2)) {
    if (WN_kid_count(store1)>1 && Mem_WN_equiv_rec(WN_kid1(store1), WN_kid1(store2))) 
      return TRUE;
  }
  return FALSE;
}

// ====================================================================
// Recursively set the flags that say this chi statement and the 
// variables it references are used.
// ====================================================================

void 
DSE::Set_Required_CHI( CHI_NODE *chi, BOOL *chi_is_live ) const
{
  AUX_ID vaux = chi->Aux_id();
  BOOL real_use = FALSE;
  WN *chiwn = Opt_stab()->Ver_stab_entry(chi->Result())->Chi_wn();
  if (OPERATOR_is_scalar_istore (WN_operator(chiwn))) {
    real_use = (Opt_stab()->Is_virtual(vaux) && 
		! Opt_stab()->Special_vsym(vaux) &&
		Opt_stab()->Get_occ(chiwn)->Aux_id() == vaux);
    // vsym corresponds to ISTORE: result set real-use also
    if (real_use)
      Opt_stab()->Ver_stab_entry(chi->Result())->Set_Real_use();
  }
  else if (!WOPT_Enable_Call_Zero_Version && WN_operator(chiwn) == OPR_CALL) {
    real_use = ! Opt_stab()->Special_vsym(vaux);
    if (real_use)
      Opt_stab()->Ver_stab_entry(chi->Result())->Set_Real_use();
  } else if (Is_identity_asgn(chiwn, Opt_stab())) {
    // Fix 623783 -- 
    //   keep the chi result of identity assignment to have non-zero
    //   version because otherwise identity assignment cannot be deleted 
    Opt_stab()->Ver_stab_entry(chi->Result())->Set_Real_use();
  }

  *chi_is_live = TRUE;
  if (OPERATOR_is_scalar_istore(WN_operator(chiwn)) || 
      OPERATOR_is_scalar_store(WN_operator(chiwn)) ||
      WN_operator(chiwn) == OPR_MSTORE) {
    WN *last_st = Last_store(vaux);
    if( last_st != NULL && chiwn != last_st )  {
      if( Same_memloc(chiwn, last_st) ) {
        *chi_is_live = FALSE;
        if ( Tracing() ) {
          fprintf ( TFile, "DSE::Set_Required_CHI, current chiwn is not set live:\n" );
          fdump_tree_no_st(TFile, chiwn);
      	}
      }
    }
    if(*chi_is_live)
      Set_last_store(vaux, chiwn);
  } 

  if(*chi_is_live) {
    VER_STAB_ENTRY *vsym = Opt_stab()->Ver_stab_entry(chi->Result());
    vsym->Set_Any_use();
    Set_Required_WN(chiwn);
  }

  if (OPERATOR_is_scalar_istore(WN_operator(chiwn)) || 
      OPERATOR_is_scalar_store(WN_operator(chiwn)) ||
      WN_operator(chiwn) == OPR_MSTORE) {
    if(*chi_is_live)
      Set_last_store(vaux, chiwn);
  }

  // The following breaks the use-def chain.  The definition
  // of the chi operand can become dse-dead.  It violates assertions
  // in opt_verify.cxx.  We can change the verifier to check this 
  // exact condition. However, this condition does not sufficienlty
  // guarantee that the statement is a "killing" definition.
  //   -Raymond  7/24/98.
  // 
  // if (real_use && Opt_stab()->Unique_vsym(vaux)) 
  //   return; // because it is a killing def (not a preserving def)

  VER_STAB_ENTRY *opnd_ver = Opt_stab()->Ver_stab_entry(chi->Opnd());
  Set_Required_VSE( opnd_ver, real_use, NULL );
}
// ====================================================================
// Recursively set the flags that say the variables it references
// (but not those it defines) are necessary.  If it is an STID node,
// the defined variable is also made live.
// ====================================================================

void 
DSE::Set_Required_WN( WN *wn ) const
{
  // have we fully processed this WN already?
  if ( Live_wn( wn ) ) {
    return;
  }

  // say we have fully processed this WN
  Set_live_wn( wn );

  const OPERATOR opr = WN_operator(wn);

  if ( Tracing() ) {
    fprintf( TFile, "<dse> required WN_map_id:%d (%s)\n", 
       WN_map_id(wn), OPERATOR_name(opr) );
  }

 
  // don't make the chi operands required, because the chi may be dead
  
  // make the vsym corresponding to ISTOREs required and real use
  if (OPERATOR_is_scalar_istore (opr)) {
    AUX_ID vaux = Opt_stab()->Get_occ(wn)->Aux_id();
    if (! Opt_stab()->Is_virtual(vaux) || Opt_stab()->Special_vsym(vaux))
      ;
    else {
      CHI_LIST_ITER chi_iter;
      CHI_NODE *cnode;
      FOR_ALL_NODE( cnode, chi_iter, Init(Opt_stab()->Get_generic_chi_list(wn))) {
        if (cnode->Aux_id() != vaux) continue;
        VER_STAB_ENTRY *vsym = Opt_stab()->Ver_stab_entry(cnode->Result());
        Set_Required_VSE( vsym, TRUE, NULL );
        break;
      }
    }
  }

  if ( WN_has_ver(wn) ) {
    VER_STAB_ENTRY *sym = Opt_stab()->Ver_stab_entry(WN_ver(wn));
    if(OPERATOR_is_scalar_load( WN_operator(wn) )) {
      //OSP_468
      //Set_Required_Imp_VSE(WN_ver(wn), TRUE);
      Set_last_store(sym->Aux_id(), NULL);
    }
    Set_Required_VSE( sym, TRUE, wn );
  }

  // do not dive into "black-boxes" and just rely on the mu/chi lists
  // any region node that made it this far (past CFG) is black to wopt
  if (opr != OPR_BLOCK && ! OPERATOR_is_black_box(opr) && opr != OPR_REGION) {
    // because all of the kids in this tree are used by a real whirl
    // node, their uses are real
    for ( INT32 kidnum = 0; kidnum < WN_kid_count(wn); kidnum++ ) {
      Set_Required_WN( WN_kid(wn,kidnum) );
    }
  }

  // The following is no longer needed because DCE keeps dse-dead chi nodes.
  //
  // if (opr == OPR_FORWARD_BARRIER ||
  //  opr == OPR_BACKWARD_BARRIER) {
  //  CHI_LIST_ITER chi_iter;
  //  CHI_NODE *cnode;
  //  FOR_ALL_NODE( cnode, chi_iter, Init(Opt_stab()->Get_generic_chi_list(wn))) {
  //  VER_STAB_ENTRY *vsym = Opt_stab()->Ver_stab_entry(cnode->Result());
  //      Set_Required_VSE( vsym, FALSE, NULL );
  //    }
  //  }

  if ( WN_has_mu(wn, Cfg()->Rgn_level()) ) {
    // process the list of mu operands as uses
    MU_LIST_ITER mu_iter;
    MU_NODE *mu;
    BOOL mu_reqd = (OPERATOR_is_scalar_iload (opr) ||
		    opr == OPR_RETURN || opr == OPR_RETURN_VAL ||
		    opr == OPR_MLOAD || opr == OPR_REGION || 
		    !WOPT_Enable_Call_Zero_Version && opr == OPR_CALL);
    BOOL mu_of_parm = opr == OPR_PARM;
    // iload's mu list must contain only is own virtual variable, and
    // that must be regarded as a real use

    OCC_TAB_ENTRY *occ = Opt_stab()->Get_occ(wn);
    if (occ->Is_stmt()) {
      MU_LIST *mu_list = occ->Stmt_mu_list();
      FOR_ALL_NODE( mu, mu_iter, Init(mu_list)) {
	Set_Required_MU( mu, mu_reqd || mu_of_parm);
      }
    } else  {
      Set_Required_MU( occ->Mem_mu_node(), mu_reqd || mu_of_parm);
      //OSP_468
      //Set_Required_Imp_VSE(occ->Mem_mu_node()->Opnd(), mu_reqd || mu_of_parm);
    }
  }
}

// ====================================================================
// Determine if the given WN expression must be executed, normally
// after Required_stmt has said the statement is not required,
// but there may be some part of the expression that must be evaluated.
// For those parts that must be evaluated, set the appropriate use
// flags.
// ====================================================================

BOOL
DSE::Check_Required_Expr( WN *wn ) const
{
  OPERATOR expr_oper = WN_operator(wn);
  
  switch ( expr_oper ) {
  case OPR_LDID:
  case OPR_LDBITS: 
  case OPR_ILOAD:
  case OPR_ILDBITS:
  case OPR_ILOADX:
  case OPR_MLOAD:
    if ( Required_load( wn ) ) {
      return TRUE;
    }
    break;
    
    case OPR_BLOCK:
      // don't do anything with blocks (their kidcount is wrong)
      break;

  default:
    for ( INT32 kidnum = 0; kidnum < WN_kid_count(wn); kidnum++ ) {
      if ( Check_Required_Expr( WN_kid(wn,kidnum) ) ) {
	return TRUE;
      }
    }
    break;
  }

  return FALSE;
}

// ====================================================================
// Determine if this statement has been deleted (should match the
// code in opt_ssa.cxx Value_number which skips code to add to htable)
// ====================================================================

BOOL
DSE::Is_deleted_statement( WN *stmt ) const
{
  const OPCODE opc   = WN_opcode(stmt);
  const OPERATOR opr = OPCODE_operator(opc);

  if (OPERATOR_is_scalar_store (opr) &&
      ! Opt_stab()->Du_any_use(WN_ver(stmt))) {
    return TRUE;
  }
  else if ( (OPERATOR_is_scalar_istore (opr) || opr == OPR_MSTORE) && 
	   WOPT_Enable_Dse_Aggressive && WN_has_chi(stmt, Cfg()->Rgn_level()) )
  {
    // check if it's a volatile store
    if ( Required_istore( stmt ) )
      return FALSE;
    else {

      // istore to unique pt
      OCC_TAB_ENTRY *occ = Opt_stab()->Get_occ(stmt);
      if (occ->Points_to()->Unique_pt()) {
	Warn_todo("Handle unique pts.");
        return TRUE;
      }

      CHI_LIST_ITER chi_iter;
      CHI_NODE *cnode;
      CHI_LIST *chi_list = Opt_stab()->Get_generic_chi_list(stmt);
      FOR_ALL_NODE( cnode, chi_iter, Init(chi_list) ) {
	if (cnode->Live()) {
	  // one chi is live, so statement can't be deleted
	  return FALSE;
	}
      }

      // must be deleted
      return TRUE;
    }
  }

  return FALSE;
}


VER_ID
DSE::Prop_vsym_new_result( VER_ID vid ) const
{
  VER_STAB_ENTRY *vse = Opt_stab()->Ver_stab_entry(vid);
  if ( vse->Type() == PHI_STMT ) {
    // we assume that the result is correct
    return vse->Phi()->Result();
  }
  else if ( vse->Type() == CHI_STMT ) {
    // is this statement live?
    if (vse->Chi()->Live()) {
      return vse->Chi()->Result();
    }
    else {
      return Prop_vsym_new_result(vse->Chi()->Opnd());
    }
  }
  else {
    return vid;
  }
}

void
DSE::Propagate_vsym_wn( WN *wn ) const
{
  if ( WN_has_ver(wn) ) {
    WN_set_ver(wn, Prop_vsym_new_result(WN_ver(wn)));
  }

  if ( WN_has_mu(wn, Cfg()->Rgn_level()) ) {
    // process the mu operand as use
    OCC_TAB_ENTRY *occ = Opt_stab()->Get_occ(wn);
    MU_NODE *mu = occ->Mem_mu_node();
    mu->Set_opnd( Prop_vsym_new_result(mu->Opnd()) );
  }
  return;
}

// ====================================================================
// After we've deleted some globals that had chis with a virtual
// variable in them, we need to go back and fix up the use-def chain
// to be correct.
//
// This involves "skipping" over dead statements and updating the
// references.  The algorithm does update the dead statements as well
// however, so each reference only has to look at its definition to see
// what the propagated value is.
//
// Do this in dominator-tree order, so it must be called for the first
// time with the entry-bb that dominates all blocks.
// ====================================================================

void
DSE::Propagate_vsym_bb( BB_NODE *bb ) const
{
  // propagate into the phi-nodes, if any
  PHI_LIST_ITER phi_iter;
  PHI_NODE     *phi;
  FOR_ALL_ELEM ( phi, phi_iter, Init(bb->Phi_list()) ) {
    if (phi->Live() ) {
	for ( INT pkid = 0; pkid < phi->Size(); pkid++ ) {
	  phi->Set_opnd(pkid, Prop_vsym_new_result(phi->Opnd(pkid)));
	}
    }
  }

  // handle the block's statements, INCLUDING non-live ones
  STMT_ITER stmt_iter;
  WN *stmt;
  FOR_ALL_ELEM(stmt, stmt_iter, Init(bb->Firststmt(),bb->Laststmt())) {
    // handle mu references only on live statements
    if ( !Is_deleted_statement(stmt) ) {
      // handle the statement's mu list
      if ( WN_has_mu(stmt, Cfg()->Rgn_level()) ) {
	MU_LIST *mu_list = _opt_stab->Get_stmt_mu_list(stmt);
	if (mu_list) {
	  MU_LIST_ITER mu_iter;
	  MU_NODE *mu;
	  FOR_ALL_NODE( mu, mu_iter, Init(mu_list)) {
            mu->Set_opnd( Prop_vsym_new_result(mu->Opnd()) );	  
          }
	}
      }

      for ( INT32 kidnum = 0; kidnum < WN_kid_count(stmt); kidnum++ ) {
        Propagate_vsym_wn( WN_kid(stmt, kidnum) );
      }

      // need to handle all chi statements, dead or not
      // handle the statement's chi list
      if ( WN_has_chi(stmt, Cfg()->Rgn_level())) {
        CHI_LIST *chi_list = _opt_stab->Get_generic_chi_list(stmt);
        if (chi_list) {
          CHI_LIST_ITER chi_iter;
          CHI_NODE *chi;
          FOR_ALL_NODE( chi, chi_iter, Init(chi_list)) {
          // propagate into the chi node's operand
          //
          // only do this for a non-dead chi; otherwise, we can have
          // live-range overlap when we resurrenct this dead chi:
          // resurrecting only updates along the def chain, not the
          // use chain. 
          // not updating the dead chi here is ok since all the uses
          // below this should be dead (or we have an overlapped
          // live range already).
          //
            if (chi->Live())
              chi->Set_opnd( Prop_vsym_new_result( chi->Opnd() ));
          }
        }
      }
    }
  }

  // do copy propagation for this block's dominated nodes
  BB_NODE *dom_bb;
  BB_LIST_ITER dom_bb_iter;
  FOR_ALL_ELEM(dom_bb, dom_bb_iter, Init(bb->Dom_bbs())) {
    Propagate_vsym_bb(dom_bb);
  }
}


#if defined(TARG_SL)
/* result = intrnsic_slave_op( master, ar2,ar3...)
 * the master is the result of master intrnsic op
 * if master is live, stmt of slave-intrnsic-op must live
 */
void 
DSE::Repair_Injured_AuxIntrnOP (void) const{
  for (INT32 i = 0; i < _injury_aux_intrnop->size(); i++) {
    WN *wn = (*_injury_aux_intrnop)[i];
    if (WN_operator(wn) == OPR_STID) {
      WN *rhs = WN_kid0(wn);
      if (WN_Intrinsic_OP_Slave(rhs)) {
        WN *op1 = WN_kid0(WN_kid0(rhs));// get first parameter of slave intrn op	
        if (WN_has_ver(op1)) {
          VER_STAB_ENTRY *sym = Opt_stab()->Ver_stab_entry(WN_ver(op1));
          if (sym->Real_use()) {
            // if the first parameter has the real use, set stmt of slave intrnsic op is require  
            Set_Required_WN(wn);
          }
        }	
      }
    }
    
  }

}

#endif

// ====================================================================
// Driver for the dead-store elimination phase
// ====================================================================

void
DSE::Dead_store_elim( void ) const
{
  if ( Tracing() ) {
    fprintf ( TFile, "DSE::Dead_store_elim (before dse)\n" );
  }

  VER_STAB_ITER ver_stab_iter(Opt_stab()->Ver_stab());
  VER_ID ssa_id;

  // clear all of the versions' uses
  FOR_ALL_NODE( ssa_id, ver_stab_iter, Init() ) {
    VER_STAB_ENTRY *vse = Opt_stab()->Ver_stab_entry(ssa_id);
    Assert( vse != NULL, 
      (EC_Misc_Int, "Dead_store_elim null version entry", ssa_id) );
    vse->Reset_Real_use();
    vse->Reset_Any_use();
    if (vse->Type() == PHI_STMT) {
      vse->Phi()->Reset_live();    
    }
    else if (vse->Type() == CHI_STMT) {
      vse->Chi()->Set_live(FALSE);
    }

    if ( Tracing() ) {
      vse->Print(TFile, ssa_id);
    }
  }

  // process all of the basic blocks
  // NOTE: Does a different order of traversing the blocks give better
  // locality and require fewer trips back through the use-def chain?
  CFG_ITER cfg_iter(Cfg());
  BB_NODE *bb;
  FOR_ALL_NODE_REVERSE( bb, cfg_iter, Init() ) {
    STMT_ITER stmt_iter;
    WN *wn;

    if ( Tracing() ) {
      fprintf( TFile, "<dse> block: %d\n", bb->Id() );
    }
    
    //  Preserve the unit-stride induction variable introduced
    //  by loop normalization.
    if (WOPT_Enable_IVR) {
      if (bb->Kind() == BB_DOSTEP) {
	FOR_ALL_ELEM(wn, stmt_iter, Init(bb->Firststmt(),bb->Laststmt())) {
	  Set_Required_WN( wn );
	}
      } else if (bb->Kind() == BB_WHILEEND || bb->Kind() == BB_DOEND || bb->Kind() == BB_REPEATEND) {
	if (bb->Laststmt() != NULL && WN_prev(bb->Laststmt()) != NULL)
	  Set_Required_WN( WN_prev(bb->Laststmt()) );
      }
    }

    // process all of the statements in the block
    // NOTE: Does a different order of traversing the statements give
    // better locality or require fewer trips back through the use-def
    // chain?
    FOR_ALL_ELEM_REVERSE(wn, stmt_iter, Init(bb->Firststmt(),bb->Laststmt())) {
      if ( Live_wn( wn ) ) {
	// already fully processed this statement
	continue;
      }
      else if ( Required_stmt( wn ) ) {
	Set_Required_WN( wn );
      }
      else if ( Check_Required_Expr(wn) ) {
	Set_Required_WN( wn );
      }
    }
  } // end bb iteration
    // repair stmt ,which rsh is a aux intrinsic op	 
#if defined(TARG_SL)
    Repair_Injured_AuxIntrnOP();
#endif

  // Update liveness of the ver-stab entries.
  // Must be after all updates of Any_use and Real_use.
  //
  FOR_ALL_NODE( ssa_id, ver_stab_iter, Init() ) {
    VER_STAB_ENTRY *vse = Opt_stab()->Ver_stab_entry(ssa_id);
    
    if (vse->Type() == PHI_STMT) {
      if (vse->Any_use()) {
        vse->Phi()->Set_live();
        vse->Phi()->Reset_dse_dead();
      }
      else {
	vse->Phi()->Reset_live();    
	vse->Phi()->Set_dse_dead();    
      }
    }
    else if (vse->Type() == CHI_STMT) {
      vse->Chi()->Set_live(vse->Any_use());
      vse->Chi()->Set_dse_dead(! vse->Any_use());
    }
  }

  if (Opt_stab()->Has_exc_handler()
#ifdef KEY
      || Opt_stab()->Has_nonlocal_goto_target()
#endif
      ) {
    Add_MU_list_for_calls();

    // update liveness because Add_MU_list_for_calls
    // changed the real_use and any_use
    FOR_ALL_NODE( ssa_id, ver_stab_iter, Init() ) {
      VER_STAB_ENTRY *vse = Opt_stab()->Ver_stab_entry(ssa_id);
    
      if (vse->Type() == PHI_STMT) {
	if (vse->Any_use()) {
	  vse->Phi()->Set_live();
          vse->Phi()->Reset_dse_dead();
	}
	else {
	  vse->Phi()->Reset_live();    
	  vse->Phi()->Set_dse_dead();    
	}
      }
      else if (vse->Type() == CHI_STMT) {
	vse->Chi()->Set_live(vse->Any_use());
        vse->Chi()->Set_dse_dead(! vse->Any_use());
      }
    }
  }

  FOR_ALL_NODE( ssa_id, ver_stab_iter, Init() ) {
    VER_STAB_ENTRY *vse = Opt_stab()->Ver_stab_entry(ssa_id);
    
    if (vse->Type() == ENTRY_STMT) {
      vse->Set_Real_use();	// regard entry version as real use
      vse->Set_Any_use();
    }
  }

  // For all the phi nodes of the loop entry block, mark the phi opnd
  // corresponding to the incoming edge to have real use.  This prevents
  // Find_zero_versions() to make it a zero version.
  // The reason for this change is because IVR cannot process this phi as IV if
  // its initial value is a zero version.  -Raymond

  PHI_NODE *phi;
  PHI_LIST_ITER phi_iter;
  FOR_ALL_NODE(bb, cfg_iter, Init()) {
    if (bb->Loop() && bb->Loop()->Well_formed() && bb->Loop()->Header() == bb) {
      INT32 pos = bb->Loop()->Header()->Pred()->Pos(bb);
      FOR_ALL_ELEM (phi, phi_iter, Init(bb->Loop()->Header()->Phi_list())) {
	if (phi->Live()) {
	  VER_STAB_ENTRY *vse = Opt_stab()->Ver_stab_entry(phi->Opnd(pos));
	  vse->Set_Real_use();
	  vse->Set_Any_use();
        }
      }
    }
  }

  Propagate_vsym_bb( Cfg()->Entry_bb() );

  if ( Tracing() ) {
    fprintf ( TFile, "SSA::Dead_store_elim (after dse)\n" );
    FOR_ALL_NODE( ssa_id, ver_stab_iter, Init() ) {
      VER_STAB_ENTRY *vse = Opt_stab()->Ver_stab_entry(ssa_id);
      fprintf(TFile, " [%3d]", ssa_id);
      vse->Print(TFile, ssa_id);
    }
    FOR_ALL_NODE( bb, cfg_iter, Init() ) {
      bb->Print_head(TFile);
      STMT_ITER stmt_iter;
      WN *wn;
      bb->Phi_list()->PRINT(TFile);
      FOR_ALL_ELEM(wn, stmt_iter, Init(bb->Firststmt(),bb->Laststmt())){
        if ( Is_deleted_statement( wn ) ) {
	  fprintf(TFile, "*** the following stmt is deleted. ***\n");
	}

	if ( WN_has_mu(wn, Cfg()->Rgn_level()) ) {
	  MU_LIST *mu_list = _opt_stab->Get_stmt_mu_list(wn);
	  if (mu_list) {
	    MU_LIST_ITER mu_iter;
	    fprintf(TFile, " mu<");
	    MU_NODE *mnode;
	    FOR_ALL_NODE( mnode, mu_iter, Init(mu_list)) {
	      fprintf(TFile, "%d/%d ", mnode->Aux_id(), mnode->Opnd());
	    }
	    fprintf(TFile, ">\n");
	  }
	}
	
	fdump_tree_no_st(TFile, wn);

	if ( WN_has_chi(wn, Cfg()->Rgn_level()) ) {
	  CHI_LIST *chi_list = _opt_stab->Get_generic_chi_list(wn);
	  if (chi_list) {
	    CHI_LIST_ITER chi_iter;
	    fprintf(TFile, " chi<");
	    CHI_NODE *cnode;
	    FOR_ALL_NODE( cnode, chi_iter, Init(chi_list)) {
	      fprintf(TFile, "%d/%d/%d ",
		      cnode->Aux_id(), cnode->Result(), cnode->Opnd());
	    }
	    fprintf(TFile, ">\n");
	  }
	}
      }
    }
  }
}

// ====================================================================
// create a new mu node without duplicate, and set the mu operand
// with the current version of coderep.
// ====================================================================

MU_NODE*
MU_LIST::New_mu_node_w_cur_vse(AUX_ID    var,
                               VER_ID    vse,
                               MEM_POOL *pool)
{
  // don't bother if already exist
  if (Contains(var)) return NULL;

  // add the current version of var to the mu_list
  MU_NODE *mnode = New_mu_node(var, pool);
  mnode->Set_opnd(vse);
  return mnode;
}

// ====================================================================
// Visiting all the enclosing exception scope to add the additional
// aliasing to the mu-list
// ====================================================================

void
DSE::Add_EH_exposed_use(WN *call) const
{
  if (_exc == NULL || _exc->Get_es_link(call) == NULL)
    return;

  // we append additional variables in the mu list for C++ EH here
  // The mu-list has already been process all address taken variables.
  Is_True(_exc != NULL, ("DSE::Add_EH_exposed_use, NULL exception scope (1)"));
  EXC_SCOPE *es = _exc->Get_es_link(call);
  Is_True(es != NULL, ("DSE::Add_EH_exposed_use, NULL exception scope (2)"));

  MU_LIST   *mu_list = _opt_stab->Get_stmt_mu_list(call);
  MU_NODE   *mu;
  AUX_ID     var;
  VER_ID     vse;
  EXC_SCOPE *exc_scope, *exc_scope2;

  for (exc_scope = es; exc_scope != NULL; exc_scope = exc_scope->Parent()) {
    EXC_SCOPE_TRY_ITER try_iter(exc_scope);
    FOR_ALL_ELEM(var, try_iter, Init()) {
      if (var != 0) {
	vse = _opt_stab->Stack(var)->Top();
	mu = mu_list->New_mu_node_w_cur_vse(var, vse, _cfg->Mem_pool());
	if (mu) {
	  Set_Required_MU( mu, FALSE );
	  if ( Tracing() )
	    fprintf( TFile, "<dse> Required EH_MU: var:%d\n", var );
	}
      }
    }
  }
}


#ifdef KEY
void
DSE::Add_entry_exposed_uses(WN *call) const
{
  // we append additional variables in the mu list due to nonlocal goto targets.
  // The mu-list has already been process all address taken variables.

  MU_LIST   *mu_list = _opt_stab->Get_stmt_mu_list(call);
  MU_NODE   *mu;
  AUX_ID     var;
  VER_ID     vse;
  WN 	    *optchi;
  CHI_LIST *chi;
  CHI_LIST_ITER chi_iter;
  CHI_NODE *cnode;
  BB_LIST_ITER entry_iter(_cfg->Fake_entry_bb()->Succ());
  FOR_ALL_ITEM(entry_iter, Init()) {
    BB_NODE *bb = entry_iter.Cur_bb();
    optchi = bb->Firststmt();
    if (optchi == NULL)
      continue;
    Is_True(WN_operator(optchi) == OPR_OPT_CHI,
	("DSE::Add_entry_exposed_uses: cannot find chi-list"));
    chi = _opt_stab->Get_stmt_chi_list(optchi);
    Is_True(chi != NULL,
	    ("DSE::Add_entry_exposed_uses: NULL chi"));
    FOR_ALL_NODE(cnode, chi_iter, Init(chi)) {
      var = cnode->Aux_id();
      vse = _opt_stab->Stack(var)->Top();
      mu = mu_list->New_mu_node_w_cur_vse(var, vse, _cfg->Mem_pool());
      if (mu) 
	Set_Required_MU( mu, FALSE );
    }
  }
}
#endif

// ====================================================================
// Visit the dominator tree to add mu to all calls nested inside any
// exception scope
// ====================================================================

void DSE::Update_MU_list_for_call(BB_NODE *bb) const
{
  PHI_LIST_ITER phi_iter;
  PHI_NODE *phi;
  BB_LIST_ITER bb_iter;
  STMT_ITER stmt_iter;
  BB_NODE *dom_bb;
  BB_LIST_ITER dom_bb_iter;
  WN *wn;
  AUX_ID var;

  //  Iterate through each phi-node 
  FOR_ALL_ELEM (phi, phi_iter, Init(bb->Phi_list())) {
    var = phi->Aux_id();
    _opt_stab->Stack(var)->Push(phi->Result());
  }

  //  Iterate through each statement
  FOR_ALL_ELEM (wn, stmt_iter, Init(bb->Firststmt(), bb->Laststmt())) {

    OPCODE   opc = WN_opcode(wn);
    OPERATOR opr = OPCODE_operator(opc);

    // Process Calls
    if ( opr == OPR_CALL || opr == OPR_ICALL ) {
#ifdef KEY
      if (_opt_stab->Has_exc_handler())
#endif
      Add_EH_exposed_use(wn);
#ifdef KEY
      else if (_opt_stab->Has_nonlocal_goto_target())
	Add_entry_exposed_uses(wn);
#endif
    }

    // Process Lhs
    if (OPERATOR_is_scalar_store (opr)) {
      var = _opt_stab->Du_aux_id(WN_ver(wn));
      _opt_stab->Stack(var)->Push(WN_ver(wn));
    }

    // Process Chi
    if ( WN_has_chi(wn, Cfg()->Rgn_level()) ) {
      CHI_LIST *chi_list = _opt_stab->Get_generic_chi_list(wn);
      if (chi_list) {
	CHI_LIST_ITER chi_iter;
      CHI_NODE *cnode;
	FOR_ALL_NODE( cnode, chi_iter, Init(chi_list)) {
	  var = cnode->Aux_id();
	  _opt_stab->Stack(var)->Push(cnode->Result());
	}
      } 
    }
  }

  FOR_ALL_ELEM (dom_bb, dom_bb_iter, Init(bb->Dom_bbs())) {
    Update_MU_list_for_call(dom_bb);  /* child */
  }

  //  The statements are processed in reverse order when poping vse
  //  stack.
  //
  FOR_ALL_ELEM_REVERSE (wn, stmt_iter, Init(bb->Firststmt(), bb->Laststmt())) {

    OPERATOR opr = WN_operator(wn);

    // Process side effect (chi)
    if ( WN_has_chi(wn, Cfg()->Rgn_level()) ) {
      CHI_LIST *chi_list = _opt_stab->Get_generic_chi_list(wn);
      if (chi_list) {
	CHI_LIST_ITER chi_iter;
	CHI_NODE *cnode;
	FOR_ALL_NODE( cnode, chi_iter, Init(chi_list)) {
	  var = cnode->Aux_id();
	  _opt_stab->Stack(var)->Pop();
	}
      } 
    }

    // Process LHS
    if (OPERATOR_is_scalar_store (opr)) {
      var = _opt_stab->Du_aux_id(WN_ver(wn));
      _opt_stab->Stack(var)->Pop();
    }
  }

  FOR_ALL_ELEM (phi, phi_iter, Init(bb->Phi_list())) {
    var = phi->Aux_id();
    _opt_stab->Stack(var)->Pop();
  }
}

// ====================================================================
// Driver for the Add MU list for calls
// ====================================================================

void
DSE::Add_MU_list_for_calls( void ) const
{
  AUX_ID var;
  MEM_POOL stack_pool;
  OPT_POOL_Initialize(&stack_pool, "DSE stack pool", FALSE, DSE_DUMP_FLAG);
  OPT_POOL_Push(&stack_pool, DSE_DUMP_FLAG);

  AUX_STAB_ITER opt_stab_iter(_opt_stab);
  // allocate vse-stack.
  //
  FOR_ALL_NODE(var, opt_stab_iter, Init()) {
    AUX_STAB_ENTRY *psym = _opt_stab->Aux_stab_entry(var);
    if (psym->Is_real_var() || psym->Is_virtual()) {
      psym->Set_stack(CXX_NEW(STACK<AUX_ID>(&stack_pool), &stack_pool));
    }
  } 

  Update_MU_list_for_call(_cfg->Entry_bb());

  OPT_POOL_Pop(&stack_pool, DSE_DUMP_FLAG);
  OPT_POOL_Delete(&stack_pool, DSE_DUMP_FLAG);
}


// ====================================================================
// SSA:Dead_store_elim
//
// Driver for the dead-store elimination phase
// ====================================================================

void
SSA::Dead_store_elim( CFG *cfg, OPT_STAB *opt_stab, EXC *exc )
{
  // get a local memory area
  OPT_POOL_Push( cfg->Loc_pool(), DSE_DUMP_FLAG );

  { // scope so we destruct this item before popping the mem pool
    DSE dse( cfg, opt_stab, cfg->Loc_pool(), exc,
	     Get_Trace( TP_GLOBOPT, DSE_DUMP_FLAG ) );

    // do it
    dse.Dead_store_elim();

  }

  // free up the our local memory area
  OPT_POOL_Pop( cfg->Loc_pool(), DSE_DUMP_FLAG );
}
