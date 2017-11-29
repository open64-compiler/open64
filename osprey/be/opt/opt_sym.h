/*
 * Copyright (C) 2008-2010 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

/*
 *  Copyright (C) 2006. QLogic Corporation. All Rights Reserved.
 */

//-*-c++-*-

/*
 * Copyright 2002, 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
 */

// ====================================================================
// ====================================================================
//
// Module: opt_sym.h
// $Revision: 1.1.1.1 $
// $Date: 2005/10/21 19:00:00 $
// $Author: marcel $
// $Source: /proj/osprey/CVS/open64/osprey1.0/be/opt/opt_sym.h,v $
//
// Revision history:
//  28-SEP-94 shin - Original Version
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
// opt_sym.h holds the interface to the optimizer's auxillary symbol
// table and the SSA version table.
//
// A number of classes are declared in this file:
//
// AUX_ID_NODE contains one AUX_ID value
// AUX_ID_LIST is a singly linked list of AUX_ID indices
// AUX_ID_LIST_ITER is a forward interator through AUX_ID_LIST
//
// AUX_STAB_ENTRY is an entry in the optimizer's auxillary symbol table
//
// VER_STAB_ENTRY is an entry in the SSA version table
//
// VER_STAB_LIST_NODE is a singly linked list of pointers to
// VER_STAB_ENTRY
//
// ST_CHAIN_INFO is used to implement a map from ST_IDX to a list of
// AUX_IDs.
//
// OPT_STAB is a large class that holds:
//   aux_stab -- the optimizer's auxilliary symbol table
//   ver_stab -- the SSA version table
//   _wn_sym_map -- mapping from WHIRL nodes to occ nodes
// and several bit vectors, and other variables related to symbols and
// alias analysis within the optimizer.
//
// VER_STAB_ITER is a forward iterator for the entries of VER_STAB
//
// AUX_STAB_ITER and AUX_STAB_REVERSE_ITER are iterators for the
// optimizer's auxillary symbol table.
//
// ====================================================================
// ====================================================================


#ifndef opt_sym_INCLUDED
#define opt_sym_INCLUDED	"opt_sym.h"

#include "defs.h"
#include "opt_defs.h"
#include "mempool.h"
#include "opt_array.h"
#include "mtypes.h"
#include "stab.h"
#include "cxx_base.h"
#include "cxx_memory.h"
#include "cxx_template.h"
#include "optimizer.h"
#include "opt_wn.h"
#include "opt_points_to.h"
#include "opt_alias_analysis.h"
#include "region_util.h"
#include "wn_map.h"
#include "opt_bb.h"
#include "be_symtab.h"
#include "id_map.h"
#include "wssa_defs.h"

extern "C" {
#include "bitset.h"
}

using idmap::ID_MAP;

//  Forward declaration
class COMP_UNIT;
class CFG;
class BB_NODE;
class BB_LIST;
class PHI_NODE;
class CODEREP;
class STMTREP;
class CODEMAP;
class VER_LIST;
class ALIAS_RULE;
class OCC_TAB_ENTRY;
class MU_NODE;
class MU_LIST;
class MU_LIST_ITER;
class CHI_NODE;
class CHI_LIST;
class CHI_LIST_ITER;
class VER_STAB_LIST_NODE;
class EXP_WORKLST;
class EXP_OCCURS;
class ALIAS_CLASSIFICATION;	// so we don't depend on opt_alias_class.h
class MEMOP_ANNOT_CR_SR_MGR;

#define VER_STAB_ARRAY_TYPE  SEGMENTED_ARRAY<VER_STAB_ENTRY,128>

enum INVALID_AUX_ID {
  ILLEGAL_AUX_ID = 0,	// this aux_id is always invalid
};


// ====================================================================
//
// AUX_ID_NODE contains one AUX_ID value
// AUX_ID_LIST is a internal linked list of AUX_ID_NODE
// AUX_ID_LIST_ITER iterates through an AUX_ID_LIST
//
// ====================================================================


class AUX_ID_NODE : public SLIST_NODE {
  DECLARE_SLIST_NODE_CLASS(AUX_ID_NODE)
private:
  AUX_ID     _aux_id;
public:
  AUX_ID_NODE(void) {};
  AUX_ID_NODE(const AUX_ID_NODE&);
  AUX_ID_NODE& operator = (const AUX_ID_NODE&);
  AUX_ID   Aux_id(void) const         { return _aux_id; }
  void     Set_aux_id(AUX_ID aux_id)  { _aux_id = aux_id; }
  void     Print(FILE *fp=stderr) const;
};


class AUX_ID_LIST : public SLIST {
  DECLARE_SLIST_CLASS (AUX_ID_LIST, AUX_ID_NODE)
private:
  AUX_ID_LIST(const AUX_ID_LIST&);
  AUX_ID_LIST& operator = (const AUX_ID_LIST&);
public:
  AUX_ID_NODE *New_aux_id_node(AUX_ID var, MEM_POOL *pool) {
    AUX_ID_NODE *p = (AUX_ID_NODE *) CXX_NEW (AUX_ID_NODE , pool);
    p->Set_aux_id(var);
    Append(p);
    return p;
  }
  void  Print(FILE *fp=stderr);
};


class AUX_ID_LIST_ITER : public SLIST_ITER {
private:
  DECLARE_SLIST_ITER_CLASS (AUX_ID_LIST_ITER, AUX_ID_NODE, AUX_ID_LIST)
  AUX_ID_LIST_ITER(const AUX_ID_LIST_ITER&);
  AUX_ID_LIST_ITER& operator = (const AUX_ID_LIST_ITER&);
public:
  ~AUX_ID_LIST_ITER(void)	{}
  AUX_ID_NODE *First_elem(void)	{ return First(); }
  AUX_ID_NODE *Next_elem(void)	{ return Next();  }
};


// ====================================================================
//
// AUX_STAB_ENTRY is an entry in the optimizer's auxillary symbol table.
//
// OPT_VAR_TYPE lists the AUX_STAB_ENTRY symbol types.
//
// AUXF_FLAGS, AUXF2_FLAGS, and AUXEF_FLAGS hold the flags for each
// aux stab entry.
//
// ====================================================================


enum OPT_VAR_TYPE {
#ifdef __MINGW32__
  VT_UNKNOWN_HACK  = 0,        // illegal value for error detection
#else
  VT_UNKNOWN       = 0,        // illegal value for error detection
#endif /* __MINGW32__ */
  VT_OTHER         = 0x1,      // a symbol (not scalar, not virtual)
  VT_NO_LDA_SCALAR = 0x2,      // scalar, not virtual
  VT_LDA_SCALAR    = 0x3,      // scalar,     lda-based virtual
  VT_LDA_VSYM      = 0x4,      // not scalar, lda-based virtual
  VT_UNIQUE_VSYM   = 0x5,      // not scalar, unique virtual
  VT_SPECIAL_VSYM  = 0x6,      // not scalar, special virtual (e.g. default vsym and return vsym)
};


enum AUXF_FLAGS {
  AUXF_CONST_INIT    = 0x01,	   // is this variable a init'd const var?
  AUXF_LR_SHRINK_CAND = 0x02,      // is candidate for live range shrinking
  AUXF_VOLATILE      = 0x04,       // volatile variable
  AUXF_LOOP_INDEX    = 0x08,       // loop index variable
  AUXF_DMOD          = 0x10,       // variable is directly modified
  AUXF_MP_FIRSTPRIVATE= 0x20,      // it is marked firstprivate in MP pragma
  AUXF_MP_SHARED     = 0x40,       // it is marked shared in MP pragma
  AUXF_MP_LASTLOCAL  = 0x80,       // it is marked lastlocal in MP pragma
  AUXF_NESTED_REF    = 0x100,      // has ref in nested subroutine
  AUXF_MP_REDUCTION  = 0x200,      // marked reduction in MP pragma
  AUXF_DONT_REPLACE_IV = 0x400,    // do not replace this IV by another IV
  AUXF_NO_SPRE       = 0x800,      // disable SPRE for this var
#ifdef KEY
  AUXF_MP_NO_DSE     = 0x800,	   // disable dead store elim due to MP regions
  				   // (not when phase is MAINOPT_PHASE)
#endif
  AUXF_EPRE_TEMP     = 0x1000,     // PREG introduced by EPRE
  AUXF_SPRE_TEMP     = 0x2000,     // PREG introduced by SPRE
  AUXF_SIGN_EXTD     = 0x4000,     // Is sign extended set by LPRE
  AUXF_DISABLE_LOCAL_RVI  = 0x8000, // Is live out of current REGION
#ifdef KEY
  AUXF_INDIRECT_ACCESS = 0x10000,  // indirect access of variable?
#endif
};


enum AUXF2_FLAGS {
  AUXF2_HAS_STORE_IN_PU = 0x1,     // has store in PU, used by SPRE 
  AUXF2_IS_SIGN_EXTD    = 0x2,     // a PREG content is sign extd
  AUXF2_IS_ZERO_EXTD    = 0x4,     // a PREG content is zero extd 
                                   // both sign/zero extd mean don't care
  AUXF2_IS_ADDRESS	= 0x8,	   // ADDRESSABILITY_IS_ADDRESS
  AUXF2_NOT_ADDRESS	= 0x10,	   // ADDRESSABILITY_NOT_ADDRESS
  AUXF2_PROP_CHAIN_SEEN = 0x20,    // FFA's IP alias propagation has
				   // seen the St_chain of this variable
  AUXF2_NO_REG		= 0x40,    // no register type (MMLDID/MSTID)
  AUXF2_LPRE_VNFRE_TEMP     	= 0x80,     // PREG introduced by LPRE and VNFRE
};


// Emitter-specific flags for each aux stab entry
enum AUXEF_FLAGS {
  AUXEF_SOME_VERSION_RENUMBERED = 0x01	// some version of this PREG
					// has been renumbered.
};


class AUX_STAB_ENTRY
{
  friend class OPT_STAB;  // allow OPT_STAB to access this fields directly.
  friend class SSU;	  // SSU needs to get at St_group
  friend class WOVP;	  // WOVP needs to modify Aux_stab_entry
  friend class WHIRL_SSA_EMITTER; // need access ST,Version in emitter 
  // Alias information

private:
  mINT8   stype;                      // Type of the symbol
  UINT8  _more_flags;	      // overflow field for flags field (AUXF2_FLAGS)
  UINT8  _mclass;                  // mtype class, e.g. INT, FLOAT, COMPLEX
  mTYPE_ID _mtype:8;                  // mtype 
  mINT32  _flags;                     // flags field (AUXF_FLAGS)
  ST      *st;                        // ST *
  mINT64  _st_ofst;                   // Offset from ST.
  WSSA::WST_IDX wst_idx;              // wssa_st_idx.used in wssa
  AUX_ID    st_chain;                 // chain of aux_sym pointing to
				      // the same ST. used at OPT_STAB
				      // build time to search for the
				      // right existing aux ID for a
				      // given ST, offset, and
				      // size. The head of this chain
				      // is found via st_chain_map.

  EXP_WORKLST  *_spre_node;           // worklst for SPRE
  
  union {
    VER_STAB_LIST_NODE *_nonzerophis; // head of list of versions defined by phi
				      // that may become zero version, for use
				      // during the zero version algorithm
    CODEREP *_cr_list;		      // head of list of all CK_VAR coderep 
				      // nodes for this aux_id
  } v;
  mUINT16 _field_id;		      // used only for MTYPE_BS; 0 otherwise
  POINTS_TO points_to;                // memory that this memop access	

  AUX_ID       st_group;	      // for scalar variables that are
				      // based on the same base_st
                                      // and that are statically
				      // aliased (by equivalence,
				      // union, etc.) st_group is
				      // maintained as a circularly
				      // linked list with the
				      // properties that:
                                      // 1. The entries in any list
				      // cover a contiguous region of
				      // address space (no gaps), and
                                      // 2. If two aux ID's overlap,
				      // they are in the same list,
				      // and
                                      // 3. Each list is minimal in
				      // size subject to the first two
				      // properties.

  AUX_ID_LIST *_aux_id_list;          // list of scalars that are
				      // aliased with the virtual
				      // variable (non-null only if
				      // there are corresponding
				      // iloads).

  AUX_ID       _home_symbol;           // Corresponding 'preg' of a variable
                                       // or home of a 'preg'
  mUINT16      _value_size;            // size of its content value    
  mUINT16      _version;               // Current SSA version
  CODEREP     *_zero_cr;	       // pt to the zero version coderep node
#ifdef KEY
  WN		*_wn;		       // to help create identity assignment
  				       // for BS vars
#endif

  //  Take a union of the following field to conserve space
  //	synonym is used during canonicalization for regular variables
  //	def_bbs is used during phi-node placement
  //	rename_ver is used to maintain a rename stack of VER
  //	rename_cr  is used to maintain a rename stack of CR
  //	rename_spre is used to maintain a rename stack of store versions in SPRE
  //	itab_bitpos is used during itable construction
  //    emitter_flags is used during preopt emit time
  //
  union {
    AUX_ID  synonym;		// an equivalent symbol
    BB_LIST *def_bbs;		// the list of BBs defining this symbol.
    struct {
      IDX_32        _itab_bitpos;	// itable bitpos assigned to var
    } itab;
    struct {
      VER_LIST        *versions;// list of CR for this opt_stab entry
      STACK<CODEREP*> *coderep;	// the current version for COPY_PROP
    } rename_cr;
    struct {
      INT32            un_used;	//  unused (version number used by SSA rename).
      STACK<AUX_ID>   *stack;	//  symbol stack used by SSA rename.
    } rename_ver;
    mUINT32 emitter_flags;
  } u;

  AUX_STAB_ENTRY(void)                   
		{ st_chain = 0; 
		  st = NULL;
		  _mclass= 0;
		  _mtype = MTYPE_UNKNOWN;
		  _flags = 0;
		  _more_flags = 0;
		  _spre_node = NULL;
		  _field_id = 0;
		  v._nonzerophis = NULL;
		  _home_symbol = ILLEGAL_AUX_ID;
		  _value_size = 0;
		  _zero_cr = NULL;
#ifdef KEY
		  _wn = NULL;
#endif
		};
  ~AUX_STAB_ENTRY(void);
  AUX_STAB_ENTRY(const AUX_STAB_ENTRY&);
  AUX_STAB_ENTRY& operator = (const AUX_STAB_ENTRY&);

  void     Clear_coderep(void)        { u.rename_cr.coderep->Clear(); }
  void	   Set_def_bbs(BB_LIST *v)    { u.def_bbs = v; }
  void	   Set_itab_bitpos(IDX_32 bp) { u.itab._itab_bitpos = bp; }
  UINT     Version(void) const        { return _version; }
  void     Set_version(UINT v)        { _version = v; }
  STACK<VER_ID> *Stack(void) const    { return u.rename_ver.stack; }
  VER_LIST *Versions(void) const      { return u.rename_cr.versions; }
  STACK<CODEREP*> *Coderep(void) const{ return u.rename_cr.coderep; }
  void     Set_coderep(STACK<CODEREP*> *cr)
    { u.rename_cr.coderep = cr; }
  AUX_ID   St_chain(void) const       { return st_chain; }
  AUX_ID   St_group(void) const       { return st_group; }

  void     Set_st_chain(AUX_ID i)     { st_chain = i; }
  void     Set_st_group(AUX_ID i)     { st_group = i; }
  void     Set_st(ST *s)              { st = s; }
  void     Set_st_ofst(mINT64 ofst)   { _st_ofst = ofst; }
  void     Set_stype(INT32 type)      { stype = type; }
  void     Set_mclass(INT32 mclass)   { _mclass = mclass; }
  void	   Set_mtype(MTYPE mtype)     { _mtype = mtype; }
  void     Set_synonym(AUX_ID i)      { u.synonym = i; }
  void     Set_aux_id_list(AUX_ID_LIST *a) 
    { _aux_id_list = a; }
  void     Set_wst_idx(WSSA::WST_IDX idx) { wst_idx = idx;  }
  WSSA::WST_IDX    Get_wst_idx() const    { return wst_idx; }

  // various flags
  void     Clear_flags(void)          { _flags = 0; _more_flags = 0; }
  void     Set_const_init(void)       { _flags |= AUXF_CONST_INIT; }

public:
  OPT_VAR_TYPE  Stype(void) const     { return (OPT_VAR_TYPE) stype; }
  INT32    Mclass(void) const         { return _mclass; }
  MTYPE	   Mtype(void) const	      { return _mtype; }
  ST	   *St(void) const	      { return st; }
  const char     *St_name(void);
  mINT64   St_ofst(void) const        { return _st_ofst; }    // relative to the ST
  const char *Base_name(void) const   { return (Base() && ST_class(Base()) == CLASS_VAR)
					  ? ST_name(Base()) : "null"; }
  AUX_ID   Home_sym(void) const       { return _home_symbol; }
  void     Set_home_sym(AUX_ID i)     { _home_symbol = i; }
  UINT     Value_size(void) const     { return _value_size; }
  void     Set_value_size(UINT vsize) { _value_size = vsize; }
  CODEREP *Zero_cr(void) const        { return _zero_cr; }
  void     Set_zero_cr(CODEREP *p)     { _zero_cr = p; }
#ifdef KEY
  WN      *Wn(void) const             { return _wn; }
  void     Set_wn(WN *w)              { _wn = w; }
#endif
  BOOL     Equivalent(AUX_STAB_ENTRY *);
  void     Prepend_def_bbs(BB_NODE *bb, MEM_POOL *p)
                                      { if ( u.def_bbs == NULL ||
					    u.def_bbs->Node() == NULL ||
					    u.def_bbs->Node()->Id()!=bb->Id() )
					  u.def_bbs = u.def_bbs->Prepend(bb,p);
				      }
  void     Clear_version(void)        { _version = 0; }
  void     Clear_stack(void)          { u.rename_ver.stack->Clear(); }
  BB_LIST *Def_bbs(void) const        { return u.def_bbs; }
  void     Set_stack(STACK<VER_ID> *s){ u.rename_ver.stack = s; }
  IDX_32   Itab_bitpos(void) const    { return u.itab._itab_bitpos; }
  AUX_ID_LIST *Aux_id_list(void)   
    { Is_True(Is_virtual(), ("only virtual var has aux_id list.")); 
      return _aux_id_list; }
  BOOL     Lr_shrink_cand(void)       { return _flags & AUXF_LR_SHRINK_CAND; }
  void     Set_lr_shrink_cand(void)   { _flags |= AUXF_LR_SHRINK_CAND; }
  BOOL     Is_volatile(void) const    { return _flags & AUXF_VOLATILE; }
  void     Set_volatile(void)         { _flags |= AUXF_VOLATILE; }
  BOOL     Mp_firstprivate(void)      { return _flags & AUXF_MP_FIRSTPRIVATE; }
  void     Set_mp_firstprivate(void)  { _flags |= AUXF_MP_FIRSTPRIVATE; }
  void     Reset_mp_firstprivate(void){ _flags &= ~AUXF_MP_FIRSTPRIVATE; }
  BOOL     Mp_shared(void)            { return _flags & AUXF_MP_SHARED; }
  void     Set_mp_shared(void)        { _flags |= AUXF_MP_SHARED; }
  void     Reset_mp_shared(void)      { _flags &= ~AUXF_MP_SHARED; }
  BOOL     Mp_lastlocal(void)         { return _flags & AUXF_MP_LASTLOCAL; }
  void     Set_mp_lastlocal(void)     { _flags |= AUXF_MP_LASTLOCAL; }
  void     Reset_mp_lastlocal(void)   { _flags &= ~AUXF_MP_LASTLOCAL; }
  BOOL     Mp_reduction(void)         { return _flags & AUXF_MP_REDUCTION; }
  void     Set_mp_reduction(void)     { _flags |= AUXF_MP_REDUCTION; }
  void     Reset_mp_reduction(void)   { _flags &= ~AUXF_MP_REDUCTION; }
  BOOL     Has_nested_ref(void) const { return _flags & AUXF_NESTED_REF; }
  void     Set_has_nested_ref(void)   { _flags |= AUXF_NESTED_REF; }
  void     Reset_has_nested_ref(void) { _flags &= ~AUXF_NESTED_REF; }
  BOOL     Loop_index(void)           { return _flags & AUXF_LOOP_INDEX; }
  void     Set_loop_index(void)       { _flags |= AUXF_LOOP_INDEX; }
  BOOL     Is_flag_const_init(void)   { return (_flags & AUXF_CONST_INIT); }
  BOOL     Dmod(void)                 { return (_flags & AUXF_DMOD); }
  void     Set_dmod(void)             { _flags |= AUXF_DMOD; }
  BOOL     Dont_replace_iv(void) const { return _flags & AUXF_DONT_REPLACE_IV; }
  void     Set_dont_replace_iv(void)  { _flags |= AUXF_DONT_REPLACE_IV; }
  BOOL     No_spre(void) const        { return _flags & AUXF_NO_SPRE; }
  void     Set_no_spre(void)          { _flags |= AUXF_NO_SPRE; }
#ifdef KEY
  BOOL     Mp_no_dse(void) const        { return _flags & AUXF_MP_NO_DSE; }
  void     Set_mp_no_dse(void)          { _flags |= AUXF_MP_NO_DSE; }
#endif
  BOOL     EPRE_temp(void) const      { return _flags & AUXF_EPRE_TEMP; }
  void     Set_EPRE_temp(void)        { _flags |= AUXF_EPRE_TEMP; }
  BOOL     SPRE_temp(void) const      { return _flags & AUXF_SPRE_TEMP; }
  void     Set_SPRE_temp(void)        { _flags |= AUXF_SPRE_TEMP; }
  BOOL     LPRE_sign_extd(void) const { return _flags & AUXF_SIGN_EXTD; }
  void     Set_LPRE_sign_extd(void)   { _flags |= AUXF_SIGN_EXTD; }
  BOOL     Disable_local_rvi(void) const{ return _flags & AUXF_DISABLE_LOCAL_RVI;}
  void     Set_disable_local_rvi(void)  { _flags |= AUXF_DISABLE_LOCAL_RVI; }
#ifdef KEY
  BOOL     Indirect_access(void) const{ return _flags & AUXF_INDIRECT_ACCESS;}
  void     Set_indirect_access(void)  { _flags |= AUXF_INDIRECT_ACCESS; }
#endif
  BOOL     Has_store_in_PU(void) const{ return _more_flags & AUXF2_HAS_STORE_IN_PU; }
  void     Set_has_store_in_PU(void)  { _more_flags |= AUXF2_HAS_STORE_IN_PU; }
  BOOL     Is_sign_extd(void) const   { return _more_flags & AUXF2_IS_SIGN_EXTD; }
  void     Set_sign_extd(void)        { _more_flags |= AUXF2_IS_SIGN_EXTD; }
  BOOL     Is_zero_extd(void) const   { return _more_flags & AUXF2_IS_ZERO_EXTD; }
  void     Set_zero_extd(void)        { _more_flags |= AUXF2_IS_ZERO_EXTD; }
  BOOL     Is_address(void) const     { return _more_flags & AUXF2_IS_ADDRESS; }
  void     Set_is_address(void)       { _more_flags |= AUXF2_IS_ADDRESS; }
  BOOL     Not_address(void) const    { return _more_flags & AUXF2_NOT_ADDRESS; }
  void     Set_not_address(void)      { _more_flags |= AUXF2_NOT_ADDRESS; }
  BOOL     Prop_chain_seen(void) const{ return _more_flags & AUXF2_PROP_CHAIN_SEEN; }
  void     Set_prop_chain_seen(void)  { _more_flags |= AUXF2_PROP_CHAIN_SEEN; }
  BOOL     No_register(void) const    { return _more_flags & AUXF2_NO_REG; }
  void     Set_no_register(void)      { _more_flags |= AUXF2_NO_REG; }
  BOOL     LPRE_VNFRE_temp(void) const { return _more_flags & AUXF2_LPRE_VNFRE_TEMP; }
  void     Set_LPRE_VNFRE_temp(void)   { _more_flags |= AUXF2_LPRE_VNFRE_TEMP; }

  void     Reset_emitter_flags(void)  { u.emitter_flags = 0; }
  BOOL     Some_version_renumbered(void) const
    { return u.emitter_flags & AUXEF_SOME_VERSION_RENUMBERED; }
  void     Set_some_version_renumbered(void)
    { u.emitter_flags |= AUXEF_SOME_VERSION_RENUMBERED; }

  BOOL     Has_multiple_signs(void) const;
  BOOL     Unique_vsym(void)          { return stype == VT_UNIQUE_VSYM; }
  BOOL     Special_vsym(void)         { return stype == VT_SPECIAL_VSYM; }
  BOOL     Lda_vsym(void)             { return stype == VT_LDA_VSYM; }
  BOOL     Is_virtual(void) const     { return stype >= VT_LDA_SCALAR; }
  BOOL     Is_real_var(void) const    { return stype == VT_NO_LDA_SCALAR || stype == VT_LDA_SCALAR; }
  AUX_ID   Synonym(void) const        { return u.synonym; }

  BOOL	   Has_def_by_const(void);

  //  Fast RVI of local variables
  BOOL     Is_local_rvi_candidate(BOOL varargs_func) const
    { return (Is_real_var() &&
	      !No_register() &&
	      !Has_nested_ref() &&
	      !Disable_local_rvi() &&
	      ST_class(st) != CLASS_PREG &&
	      ST_sclass(st) != SCLASS_FORMAL_REF &&
	      ST_sclass(st) != SCLASS_PSTATIC &&
	      (! (varargs_func && ST_sclass(st) == SCLASS_FORMAL) ) &&
	      ! Is_volatile() &&
	      Cr_list() &&
	      points_to.Local() &&
	      points_to.No_alias() &&	
	      ! points_to.F_param() &&
	      ! Has_multiple_signs() );
    }

  // Return TRUE if then aux_stab_entry represents a register.
  BOOL     Is_preg(void) const
    {
      return (stype == VT_NO_LDA_SCALAR && // to make sure st is non-NULL
	      ST_sclass(st) == SCLASS_REG);
    }

  // Return TRUE if then aux_stab_entry represents a non-dedicated register.
  BOOL     Is_non_dedicated_preg(void) const
    {
      return (Is_preg() &&
	      _st_ofst > Last_Dedicated_Preg_Offset);
    }

  // Return TRUE if then aux_stab_entry represents a dedicated register.
  BOOL     Is_dedicated_preg(void) const
    {
      return (Is_preg() &&
	      _st_ofst <= Last_Dedicated_Preg_Offset);
    }
  
  void     Change_to_new_preg(OPT_STAB *opt_stab, CODEMAP *htable);

  VER_STAB_LIST_NODE *Nonzerophis(void) const { return v._nonzerophis; }
  void	   Set_nonzerophis(VER_STAB_LIST_NODE *nz) { v._nonzerophis = nz; }
  CODEREP *Cr_list(void) const 	      { return v._cr_list; }
  void	   Set_cr_list(CODEREP *c)    { v._cr_list = c; }
  void     Set_spre_node(EXP_WORKLST *w) { _spre_node = w; }
  EXP_WORKLST *Spre_node(void) const     { return _spre_node; }
  UINT	   Field_id(void) const		{ return _field_id; }
  void	   Set_field_id(UINT i) 	{ _field_id = i; }
  
  //   Access to POINTS_TO information
  //
  POINTS_TO   *Points_to(void)          { return &points_to; }
  EXPR_KIND   Expr_kind(void)   const   { return points_to.Expr_kind(); }
  BASE_KIND   Base_kind(void)   const   { return points_to.Base_kind(); }
  OFST_KIND   Ofst_kind(void)   const   { return points_to.Ofst_kind(); }
  ST          *Base(void)       const   { return points_to.Base(); }
  mINT64      Base_byte_ofst(void) const { return points_to.Byte_Ofst(); }   // relative to Base().
  mINT64      Byte_size(void)   const   { return points_to.Byte_Size(); }
  UINT8	      Bit_size(void)    const   { return points_to.Bit_Size(); }
  UINT8       Bit_ofst(void)    const	{ return points_to.Bit_Ofst(); }
  TY_IDX      Ty(void)          const   { return points_to.Ty(); }
  BOOL        Base_is_fixed(void) const { return points_to.Base_is_fixed(); }

  //  Set members of POINTS_TO
  //
  void Set_expr_kind(EXPR_KIND expr_kind) { points_to.Set_expr_kind(expr_kind); }
  void Set_base_kind(BASE_KIND base_kind) { points_to.Set_base_kind(base_kind); }
  void Set_ofst_kind(OFST_KIND ofst_kind) { points_to.Set_ofst_kind(ofst_kind); }
  void Set_base(ST *base)                 { points_to.Set_base(base); }
  void Set_base_byte_ofst(mINT64 ofst)    { points_to.Set_byte_ofst(ofst); }
  void Set_byte_size(mINT64 size)         { points_to.Set_byte_size(size);} 
  void Set_bit_ofst_size(UINT8 ofst, UINT8 size) {
      points_to.Set_bit_ofst_size (ofst, size);
  }
  void Set_ty(TY_IDX ty)                  { points_to.Set_ty(ty); }

};


extern BOOL Completely_replaced(AUX_STAB_ENTRY *);


// ====================================================================
//
// VER_STAB_ENTRY is an entry in the SSA version table
//
// STMT_TYPE
// VER_STAB_FLAG
//
// ====================================================================


enum STMT_TYPE { 
  NO_STMT, 
  WHIRL_STMT, 
  PHI_STMT, 
  CHI_STMT, 
  MU_STMT,
  ENTRY_STMT,
};


// here are flag values used in the ver_stab_entry class below
enum VER_STAB_FLAG {
  VS_ANY_USE  = 0x01,		// used in whirl, or ssa-related op
  VS_REAL_USE = 0x02,		// used only in whirl op
  VS_ZERO_VERS = 0x04,		// determined by algorithm to be zero version
  // NOTE: the enum values should represent distinct flag bits, so
  // the next flags should be 0x04, 0x08, 0x10, etc.
  // Also, make sure the flags field is wide enough for these flags.
};


class VER_STAB_ENTRY {
  AUX_ID     aux_id;
  INT32      version;
  union {
    WN *wn;   // defining statement
    PHI_NODE *phi;
    CHI_NODE *chi;
//  MU_NODE  *mu;
  };
  BB_NODE    *bb;
  union {
    WN         *_ref_wn;  // the WN node that can give all the type information 
                          // setup by dse; used only when defined by phi 
    WN	       *_chi_wn;  // the WN node with which the chi is associated
  };
  CODEREP    *coderep;
  POINTS_TO  *points_to;
  VER_ID     _synonym;
  mINT8      type;
  mUINT8     flags;
  
 
public:
  VER_STAB_ENTRY(void)                { Init(); }
  ~VER_STAB_ENTRY(void)               {}

  void     Init(void)
    { aux_id = ILLEGAL_AUX_ID; version = 0; type = NO_STMT; _ref_wn = NULL; 
      bb = NULL; coderep = NULL; points_to = NULL; flags = 0; _synonym = 0; 
    }
  void     Init(AUX_ID vr, INT32 ver,
		BB_NODE *b,
		CODEREP *cr, STMT_TYPE t)
    { aux_id = vr; version = ver; type = t; bb = b; coderep = cr;
      points_to = NULL; _ref_wn = NULL; flags = 0; _synonym = 0; 
    }
  INT32    Version(void) const        { return version; }
  AUX_ID   Aux_id(void) const         { return aux_id; }
  BB_NODE *Bb(void) const             { return bb; }
  CODEREP *Coderep(void) const        { return coderep; }
  WN      *Wn(void) const             { return wn; }
  WN      *Ref_wn(void) const         { return _ref_wn; }
  WN      *Chi_wn(void) const         { return _chi_wn; }
  PHI_NODE *Phi(void) const           { return phi; }
  CHI_NODE *Chi(void) const           { return chi; }
//MU_NODE  *Mu(void) const            { return mu; }
  STMT_TYPE Type(void) const          { return (STMT_TYPE) type; }
  VER_ID    Synonym(void) const       { return _synonym; }

  POINTS_TO *Points_to(void) const    { return points_to; }
  void     Set_points_to(POINTS_TO *pt) { points_to = pt; }
  void     Set_version(INT32 v)       { version = v; }
  void     Set_aux_id(AUX_ID v)       { aux_id = v; }
  void     Set_stmt(WN *w, BB_NODE *b){ wn = w; type = WHIRL_STMT; bb = b;}
  void     Set_stmt(PHI_NODE *p,
		    BB_NODE *b)       { phi = p; type = PHI_STMT;  bb = b; }
  void     Set_stmt(CHI_NODE *p,
		    BB_NODE *b)       { chi = p; type = CHI_STMT;  bb = b; }
  void     Set_ref_wn(WN *wn)         { _ref_wn = wn; }
  void     Set_chi_wn(WN *wn)         { _chi_wn = wn; }
  void     Set_type(STMT_TYPE t)      { type = t; }
  void     Set_coderep(CODEREP *cr)   { coderep = cr; }
  void     Set_synonym(VER_ID v)      { _synonym = v; }

  BOOL	   Any_use(void)	      { return flags & VS_ANY_USE; }
  void	   Set_Any_use(void)	      { flags |= VS_ANY_USE; }
  void	   Reset_Any_use(void)	      { flags &= ~VS_ANY_USE; }
  BOOL	   Real_use(void)	      { return flags & VS_REAL_USE; }
  void	   Set_Real_use(void)	      { flags |= VS_REAL_USE; }
  void	   Reset_Real_use(void)	      { flags &= ~VS_REAL_USE; }
  BOOL	   Zero_vers(void)	      { return flags & VS_ZERO_VERS; }
  void	   Set_Zero_vers(void)	      { flags |= VS_ZERO_VERS; }

  void     Print(FILE *fp, VER_ID ver_id) const;
  void     Print_use(WN *wn, FILE *fp=stderr) const;
  void     Print_use(PHI_NODE *phi, BB_NODE *bb, FILE *fp) const;
};


class VER_STAB_LIST_NODE : public SLIST_NODE {
  VER_STAB_ENTRY *_vers;
	VER_STAB_LIST_NODE(const VER_STAB_LIST_NODE&);
	VER_STAB_LIST_NODE& operator = (const VER_STAB_LIST_NODE&);
public:
	VER_STAB_LIST_NODE(void)	{}
	VER_STAB_LIST_NODE(VER_STAB_ENTRY *v)	{ _vers = v; }
       ~VER_STAB_LIST_NODE(void)	{}

  DECLARE_SLIST_NODE_CLASS( VER_STAB_LIST_NODE )

  VER_STAB_LIST_NODE *Prepend(VER_STAB_LIST_NODE *v)
					{ v->Set_Next(this); return v; }

  // member access functions
  VER_STAB_ENTRY *Vers(void) const	{ return _vers; }
};


// ====================================================================
//
// class OPT_STAB
//
// OPT_STAB holds the optimizer's auxilliary symbol table, the SSA
// version table, and several related bitfields, WN maps, and other
// values.
//
// ST_CHAIN_INFO holds an AUX_ID for the st_chain_map in OPT_STAB.
// I don't think we really need ST_CHAIN_INFO: If "head" is NEVER set
// to 0, then we could just use ID_MAP<AUX_ID, ST_IDX> for st_chain_map
// and check for 0 instead of NULL when looking up ST_IDX.
//
// ====================================================================


class ST_CHAIN_INFO {
private:
  AUX_ID list_head;
public:
  ST_CHAIN_INFO() { }
  ST_CHAIN_INFO(AUX_ID head) : list_head(head) { }
  void   Set_list_head(AUX_ID head) { list_head = head; }
  AUX_ID List_head(void) const      { return list_head; }
  void   Print(FILE *) const { }
};


class OPT_STAB {
private:

  // ------------------------------------------------------------------
  // aux_stab    optimizer's auxiliary symbol table
  // _ver_stab   SSA version symbol table
  // _cfg        handle on the optimizer's control flow graph
  // htable      handle on the CODEREP hash table
  // _rule       handle on the rules used in alias analysis
  // pu_wn       the root of the WHIRL tree for this PU
  // _rgn_trace  trace flag for region bounds
  // ------------------------------------------------------------------

  DYN_ARRAY<AUX_STAB_ENTRY> aux_stab;
  VER_STAB_ARRAY_TYPE       *_ver_stab;
  CFG                       *_cfg;
  CODEMAP                   *htable;
  const ALIAS_RULE          *_rule;
  WN                        *pu_wn;
#ifdef KEY
  ALIAS_MANAGER             *_alias_mgr;
#endif
  OPT_PU_POINTS_TO_SUMMARIZER _pt_sum;
  BOOL			    _rgn_trace;

  // ------------------------------------------------------------------
  // Memory pools
  // ------------------------------------------------------------------

  MEM_POOL                  *mem_pool;       // mem pool passed in
  // MEM_POOL                  _aux_pool; 
  MEM_POOL                  _ver_pool;      // mem pool for ver_stab
  MEM_POOL                  _occ_pool;      // mem pool for occ_stab
  MEM_POOL                  _st_chain_pool; // mem pool for ST chain heads
  MEM_POOL                  _asm_pragma_pool;

  // ------------------------------------------------------------------
  // st_chain_map         mapping from ST_IDX to head of ST chain
  // _orig_last_preg      the original last preg number (not used?)
  // _last_preg_num       the last preg number
  // _default_vsym        default virtual sym (0 if none)
  // _return_vsym         return virtual sym
  // _phase               current optimization phase
  // _flow_free_analysis  alias analysis mode
  //                      TRUE during flow-free alias analysis (FFA)
  //                      FALSE during free-sensative alias analysis (FSA)
  // _allow_sim_type      TRUE iff we are allowed to use simulated types
  //                        in a RETURN mtype/whirl
  // _has_exc_handler     TRUE iff PU is an exception handling REGION
  // _points_to_globals   used for dce's extra alias analysis to
  //                        describe that something is aliased with all
  //                        globals
  // _is_varargs_func     TRUE iff PU has a variable number of arguments
  // ------------------------------------------------------------------

  ID_MAP<ST_CHAIN_INFO *, ST_IDX>  *st_chain_map;
  IDTYPE                    _orig_last_preg;
  IDTYPE                    _last_preg_num;
  AUX_ID                    _default_vsym;
  AUX_ID                    _return_vsym;
  OPT_PHASE		    _phase;
  BOOL                      _flow_free_analysis;
  BOOL                      _allow_sim_type;
  BOOL                      _has_exc_handler;
  POINTS_TO		   *_points_to_globals;
  BOOL                      _is_varargs_func;
#ifdef KEY
  BOOL                      _is_prototyped_func;
  BOOL                      _has_nonlocal_goto_target;
#endif

  // ------------------------------------------------------------------
  // Bit vectors of the OPT_ST attributes
  //
  // The following bit vectors summarize alias information of the
  // aux_stab entries.  The thirteen are calculated by
  // OPT_STAB::Update_attr_cache.  _inaccessible_to_callees is
  // calculated by ALIAS_CLASSIFICATION::Classify_memops() in
  // opt_alias_class.cxx.  The bit vectors _indirect, _call_by_value,
  // _call_by_ref, and  _asm_alias are derived from the others by
  // OPT_STAB::Collect_ST_attr
  //
  // For now, don't put a _not_auto bit set in OPT_STAB; _not_auto
  // information is needed only in POINTS_TO (see opt_alias_analysis.h),
  // and is derived in OPT_STAB::Collect_ST_attr (see opt_sym.cxx).
  //
  // _addr_saved         !pt->Not_addr_saved
  // _addr_passed        !pt->Not_addr_passed())
  // _addr_used_locally   st == NULL or BE_ST_addr_used_locally(st)
  // _external            pt->Global() or st == NULL
  // _local_static        
  // _dedicated        
  // _ref_formal          pt->Formal() && IS_FORTRAN && !ST_is_value_parm(st)
  // _named               pt->Named()
  // _const               pt->Const()
  // _unique_pt        
  // _virtual_var         WOPT_Enable_Update_Vsym && aux_stab[idx].Is_virtual()
  // _weak_var        
  // _weak_base        
  //
  // _inaccessible_to_callees        
  //
  // _indirect        
  // _call_by_value        
  // _call_by_ref        
  // _asm_alias        
  // ------------------------------------------------------------------

  BS                        *_addr_saved;
  BS                        *_addr_passed;
  BS                        *_addr_used_locally;
  BS                        *_external;
  BS                        *_local_static;
  BS                        *_dedicated;
  BS                        *_ref_formal;
  BS                        *_named;
  BS                        *_const;
  BS                        *_unique_pt;
  BS                        *_virtual_var;
  BS                        *_weak_var;
  BS                        *_weak_base;

  BS                        *_inaccessible_to_callees;

  BS                        *_indirect;
  BS                        *_call_by_value;
  BS                        *_call_by_ref;
  BS                        *_asm_alias;

  // ------------------------------------------------------------------
  // _wn_sym_map   mapping from WHIRL nodes to occ nodes
  // _wn_box_refs  black box references
  // _wn_box_defs  black box definitions
  // ------------------------------------------------------------------

  WN_MAP                    _wn_sym_map;
  WN_MAP                    _wn_box_refs;	// black box references
  WN_MAP                    _wn_box_defs;	// black box definitions

  // this data structure is used to map from alias-class to its corresponding
  // unique-vsym.
  typedef mempool_allocator< std::pair<IDTYPE, AUX_ID> > AC_VSYM_ALLOC;
  ID_MAP<IDTYPE, AUX_ID>  _ac_2_vsym_map;

  // ------------------------------------------------------------------
  // _rgn_level  context for alias analysis
  // _alias_classification  a handle on the mappings from:
  //                          AUX_ID to alias class
  //                          WN     to alias class
  // _const_found   tlog counter of PU static const in the aux symbol
  //                table, calculated by Collect_ST_attr
  // ------------------------------------------------------------------

  REGION_LEVEL		    _rgn_level;		// context for alias analysis

  // A handle on the mappings from:
  //     aux id to alias class
  //     WN     to alias class
  ALIAS_CLASSIFICATION     *_alias_classification;
  MEMOP_ANNOT_CR_SR_MGR    * _cr_sr_annot_mgr;

  INT32                     _const_found;  // tlog counter

  // ------------------------------------------------------------------

  OPT_STAB(void);
  OPT_STAB(const OPT_STAB&);
  OPT_STAB& operator = (const OPT_STAB&);


  void	   Convert_ST_to_AUX(WN *, WN *);	// convert ST into AUX_ID
  void     Convert_black_box(WN *);		// convert black box
  void     Convert_IO_statement(WN *,WN *,INT);	// convert opr_io
  void     Process_varfmt_for_cvt_io(WN * );	//   assist above

  POINTS_TO_LIST *Create_barrier_defs( WN *);
  BOOL     Not_affected_by_barrier(AUX_ID id, BB_NODE *bb);
  void     Compute_barrier_mu_chi( OCC_TAB_ENTRY *, POINTS_TO_LIST *,
				  BB_NODE *, BOOL, BOOL, BOOL, BOOL );
  void     Make_st_group(void);			// Setup alias group
  void     Update_attr_cache(AUX_ID, ST *, POINTS_TO *, BOOL *);
  void     Collect_ST_attr(void);		// Collect attributes
  void     Canonicalize(void);			// Canonicalize aliased variables
  void     Remap_aux_synonym(WN *);
  void     Allocate_mu_chi_and_virtual_var(WN *, BB_NODE *);
  void     Update_alias_set_with_virtual_var(void);
  void     Update_aux_id_list(AUX_ID);          // update the aux_id_list for the virtual variable
  BOOL     Var_is_loop_index(AUX_ID, BB_NODE *);
  void     Collect_f90_pointer_info(POINTS_TO *, const WN *);
  void     Collect_nested_ref_info(void);
  void     Generate_mu_and_chi_list(WN *, BB_NODE *);
  void     Compute_FSA_stmt_or_expr(WN *);	// Flow sensitive alias analysis for one WN stmt
  void     Compute_FSA_dominator_order(BB_NODE *);
  void     Compute_black_box_mu_chi(const WN *, OCC_TAB_ENTRY *);// Handle boxes
  BOOL     Has_read_only_parm(AUX_ID, WN *, INT32);
  void     Generate_call_mu_chi_by_value(WN *wn,  ST *, MU_LIST *mu, CHI_LIST *chi, INT32);
  void     Generate_asm_mu_chi(WN *wn, MU_LIST *, CHI_LIST *);
  void     Generate_call_mu_chi_by_ref(WN *wn, ST *, MU_LIST *mu, CHI_LIST *chi, INT32, BB_NODE *);
  void	   Generate_exit_mu(WN *);
  void     Add_nested_call_mu_chi(WN *wn, ST *, MU_LIST *mu, CHI_LIST *chi);

  void     Inc_const_found(void)    { _const_found++; }
  INT32    Const_found(void)        { return _const_found; }

  // called by Update_parent_region_bound_sets in alias manager
  void	   REGION_add_aux_id_points_to(POINTS_TO_SET **pset, AUX_ID aux_id);
  BOOL	   REGION_merge_aux_id_points_to(POINTS_TO_SET **pset, AUX_ID aux_id);
  void	   REGION_add_to_bound(RID *rid, AUX_ID aux_id, BOOL empty);
  BOOL	   REGION_verify_bound(RID *rid, AUX_ID aux_id);

  // Misc 
  BOOL     Its_ret_val_of_malloc (VER_ID ver);
#if defined(TARG_SL)
  void     Generate_call_mu_chi_by_intrninfo(WN *wn, MU_LIST *mu, CHI_LIST *chi);
  void     Refine_intrn_alias_info(WN *intrn);
  void     Refine_intrn_mu_chi_list(WN *intrn);
#endif

  // ------------------------------------------------------------------

public:
  OPT_STAB(MEM_POOL *);
  ~OPT_STAB(void);
  VER_STAB_ARRAY_TYPE  *Ver_stab(void) { return _ver_stab; }

  void     Create(COMP_UNIT *, REGION_LEVEL);
  AUX_ID   Create_vsym(EXPR_KIND k);
  AUX_ID   Create_preg(MTYPE preg_ty, const char *name = NULL, WN *home_wn = NULL);
  AUX_ID   Find_vsym_with_base(ST *);
#ifdef KEY
  AUX_ID   Find_vsym_with_st(ST *, BOOL, POINTS_TO * = NULL);
#else
  AUX_ID   Find_vsym_with_st(ST *);
#endif
  AUX_ID   Find_vsym_with_base_ofst_and_size(ST *, INT64, INT64, UINT8, UINT8);
  AUX_ID   Find_sym_with_st_and_ofst(ST *, INT64);
  void     Clear_coderep(void);
  BOOL     Verify_stack(void);
  void     New_coderep(MEM_POOL *pool);
  void     New_stack(MEM_POOL *pool);
  void     Check_stack(void);
  WN       *Pu(void) const               { return pu_wn; }
#ifdef KEY
  ALIAS_MANAGER *Alias_Mgr(void) const   { return _alias_mgr; }
#endif
  OPT_PU_POINTS_TO_SUMMARIZER* Points_to_summarizer (void) 
                                         { return &_pt_sum; }
  void     check_ipa_mod_ref_info (const ST * , const ST * , INT *, INT *);
  void     check_ipa_same_entry_exit_value_or_1_info(const ST *, const ST *, INT *);
  MEM_POOL *Occ_pool(void)               { return &_occ_pool; }
  MEM_POOL* Ver_pool(void)               { return &_ver_pool; }
  CFG      *Cfg(void) const              { return _cfg; }
  const ALIAS_RULE *Rule(void) const     { return _rule; }
  BOOL     Is_varargs_func(void) const   { return _is_varargs_func; }
#ifdef KEY
  BOOL     Is_prototyped_func(void) const{ return _is_prototyped_func; }
  BOOL     Has_nonlocal_goto_target(void) const   { return _has_nonlocal_goto_target; }
#endif
  BOOL     Allow_sim_type(void) const    { return _allow_sim_type; }
  BOOL     Has_exc_handler(void) const   { return _has_exc_handler; }

  AUX_STAB_ENTRY *Aux_stab_entry(AUX_ID v) const
    { return &aux_stab[v]; }

  VER_STAB_ENTRY *Ver_stab_entry(VER_ID v) const
                                         { return &(*_ver_stab)[v]; }
  INT32    Lastidx(void) const           { return aux_stab.Lastidx(); }
  INT32    Version(AUX_ID var) const     { return aux_stab[var].Version(); }
  BB_LIST  *Def_bbs(AUX_ID var) const     { return aux_stab[var].Def_bbs(); }
  VER_LIST *Versions(AUX_ID var) const   { return aux_stab[var].Versions(); }
  STACK<CODEREP*> *Coderep
                  (AUX_ID var) const     { return aux_stab[var].Coderep(); }
  TY_IDX   Ty(AUX_ID var) const          { return aux_stab[var].Ty(); }
  MTYPE    Stype(AUX_ID var) const       { return aux_stab[var].Stype(); }
  INT64    Base_ofst(AUX_ID var) const   { return aux_stab[var].Base_byte_ofst(); }
  UINT8	   Bit_size(AUX_ID var) const	 { return aux_stab[var].Bit_size (); }
  UINT8	   Bit_ofst(AUX_ID var) const	 { return aux_stab[var].Bit_ofst (); }
  INT64    St_ofst(AUX_ID var) const     { return aux_stab[var].St_ofst(); }
  ST	  *Base(AUX_ID var) const	 { return aux_stab[var].Base(); }
  ST      *St(AUX_ID var) const          { return aux_stab[var].St(); }
  const char *St_name(AUX_ID var) const  { return aux_stab[var].St_name(); }
  BOOL     Unique_vsym(AUX_ID var) const { return aux_stab[var].Unique_vsym(); }
  void     Init_mp_attribute(void);
  BOOL     Mp_shared(AUX_ID var) const   { return aux_stab[var].Mp_shared(); }
  void     Reset_mp_shared(AUX_ID var)   { aux_stab[var].Reset_mp_shared(); }
  BOOL     Mp_lastlocal(AUX_ID var) const{ return aux_stab[var].Mp_lastlocal(); }
  void     Set_mp_lastlocal(AUX_ID var)  { aux_stab[var].Set_mp_lastlocal();}
  void     Set_mp_firstprivate(AUX_ID var){ aux_stab[var].Set_mp_firstprivate();}
  BOOL     Mp_reduction(AUX_ID var) const{ return aux_stab[var].Mp_reduction(); }
  void     Set_mp_reduction(AUX_ID var)  { aux_stab[var].Set_mp_reduction();}
  void     Reset_mp_reduction(AUX_ID var){ aux_stab[var].Reset_mp_reduction(); }
  BOOL     Lda_vsym(AUX_ID var) const    { return aux_stab[var].Lda_vsym(); }
  BOOL     Special_vsym(AUX_ID var) const{ return aux_stab[var].Special_vsym(); }

  UINT16   Field_id(AUX_ID var) const	 { return aux_stab[var].Field_id(); }

  POINTS_TO *Points_to(AUX_ID var) const { return aux_stab[var].Points_to(); }
  void     Set_version(AUX_ID var,
		       UINT32 version)   { aux_stab[var].Set_version(version); }
  CODEREP *Zero_cr(AUX_ID var) const	 { return aux_stab[var].Zero_cr(); }
  void     Set_zero_cr(AUX_ID var,
		       CODEREP *p)       { aux_stab[var].Set_zero_cr(p); }

  void     Push_coderep(AUX_ID var,
			CODEREP *cr)     { Coderep(var)->Push(cr); }
  INT32    Stack_elements(AUX_ID var)    { return Coderep(var)->Elements(); }
  CODEREP *Top_coderep(AUX_ID var) const { return Coderep(var)->Top(); }
  CODEREP *Top_nth_coderep(AUX_ID var, INT32 n) const { return Coderep(var)->Top_nth(n); }
  CODEREP *Pop_coderep(AUX_ID var)       { return Coderep(var)->Pop(); }
  BOOL     NULL_coderep(AUX_ID var) const{ return Coderep(var)->Is_Empty(); }
  INT32	   Elements_coderep(AUX_ID var) const {return Coderep(var)->Elements();}
  STACK<VER_ID> *Stack(AUX_ID var) const { return aux_stab[var].Stack(); }
  AUX_ID   Du_aux_id(VER_ID du) const    { return Ver_stab_entry(du)->Aux_id(); }
  INT32    Du_version(VER_ID du) const   { return Ver_stab_entry(du)->Version(); }
  TY_IDX   Du_ty(VER_ID du) const        { return Ty(Du_aux_id(du)); }
  OPT_VAR_TYPE Du_stype(VER_ID du) const { return (OPT_VAR_TYPE)Stype(Du_aux_id(du)); }
  INT64    Du_base_ofst(VER_ID du) const { return Base_ofst(Du_aux_id(du)); }
  UINT8    Du_bit_size(VER_ID du) const  { return Bit_ofst(Du_aux_id(du)); }
  UINT8    Du_bit_ofst(VER_ID du) const  { return Bit_size(Du_aux_id(du)); }
  INT64    Du_st_ofst(VER_ID du) const   { return St_ofst(Du_aux_id(du)); }
  CODEREP *Du_coderep(VER_ID du) const   { return Ver_stab_entry(du)->Coderep(); }
  BOOL     Du_any_use(VER_ID du) const   { return Ver_stab_entry(du)->Any_use(); }
  BOOL     Du_real_use(VER_ID du) const  { return Ver_stab_entry(du)->Real_use(); }
  BOOL     Du_virtual_use(VER_ID du) const { return Du_any_use(du) && !Du_real_use(du); }
  BOOL     Du_unique_vsym(VER_ID du)const{ return Unique_vsym(Du_aux_id(du)); }
  BOOL     Du_zero_vers(VER_ID du) const  { return Ver_stab_entry(du)->Zero_vers(); }
  void     Du_set_coderep(VER_ID du,
			  CODEREP *cr)   { Ver_stab_entry(du)->Set_coderep(cr); }
  BOOL     Is_virtual(AUX_ID id) const   { return aux_stab[id].Is_virtual(); }
  BOOL     Is_real_var(AUX_ID id) const  { return aux_stab[id].Is_real_var(); }
  BOOL     Du_is_virtual(VER_ID du)const { return Is_virtual(Du_aux_id(du)); }

  BOOL     Is_volatile(AUX_ID id) const   { return aux_stab[id].Is_volatile(); }
  BOOL     Du_is_volatile(VER_ID du)const { return Is_volatile(Du_aux_id(du)); }

  AUX_ID_LIST *Aux_id_list(AUX_ID id) const 
			{ return aux_stab[id].Aux_id_list(); }

  REGION_LEVEL   Rgn_level(void) const	 { return _rgn_level; }

  VER_ID   Gen_name(AUX_ID);
  VER_ID   Gen_name_phi(PHI_NODE *);
  VER_ID   Gen_name_chi(CHI_NODE *, WN *wn);
  VER_ID   Get_name(AUX_ID);

  //  Enter into AUX_STAB
  AUX_ID   Enter_symbol(OPERATOR opr, ST *st, INT64 ofst, TY_IDX wn_object_ty, 
			BOOL is_volatile, WN* wn = NULL);
  AUX_ID   Enter_ded_preg(ST *, INT64, TY_IDX, INT32);
  AUX_ID   Identify_vsym(WN *);
#ifdef KEY
  AUX_ID   Allocate_vsym(WN *, POINTS_TO *);
#endif
  void     Count_syms(WN *);	// count the number of symbols in the PU

  //  Enter into VER_STAB
  void     Enter_du(AUX_ID du, WN *wn, BB_NODE *bb)
    { Ver_stab_entry(du)->Set_stmt(wn, bb); }

  void     Enter_du(AUX_ID du, PHI_NODE *phi, BB_NODE *bb) 
    { Ver_stab_entry(du)->Set_stmt(phi, bb); }

  void     Enter_du(AUX_ID du, CHI_NODE *chi, BB_NODE *bb)  
    { Ver_stab_entry(du)->Set_stmt(chi, bb); }

  void     Enter_du(AUX_ID du)       
    { Ver_stab_entry(du)->Set_type(ENTRY_STMT); }

  void     Delete_ver_pool(void)
    {
      OPT_POOL_Pop(&_ver_pool, MEM_DUMP_FLAG+8);
      OPT_POOL_Delete(&_ver_pool, MEM_DUMP_FLAG+8);
      // _ver_pool = NULL;
    }

  // ------------------------------------------------------------------
  //  Access to the Bitset attributes
  // ------------------------------------------------------------------

  BOOL     Addr_saved(AUX_ID idx) const  {  return BS_MemberP(_addr_saved, idx); }
  BOOL     Addr_passed(AUX_ID idx) const {  return BS_MemberP(_addr_passed, idx); }
  BOOL     Addr_used_locally(AUX_ID idx) const  {  return BS_MemberP(_addr_used_locally, idx); }
  BOOL     External(AUX_ID idx) const   {  return BS_MemberP(_external, idx); }
  BOOL     Local_static(AUX_ID idx) const { return BS_MemberP(_local_static, idx); }
  BOOL     Dedicated(AUX_ID idx) const  {  return BS_MemberP(_dedicated, idx); }
  BOOL     Ref_formal(AUX_ID idx) const {  return BS_MemberP(_ref_formal, idx); }
  BOOL     Named(AUX_ID idx) const      {  return BS_MemberP(_named, idx); }
  BOOL     Const(AUX_ID idx) const      {  return BS_MemberP(_const, idx); }
  BOOL     Unique_pt(AUX_ID idx) const  {  return BS_MemberP(_unique_pt, idx); }

  BOOL     Call_by_value(AUX_ID idx) const{ return BS_MemberP(_call_by_value, idx); }
  BOOL     Call_by_ref(AUX_ID idx) const{ return BS_MemberP(_call_by_ref, idx); }
  BOOL     Asm_alias(AUX_ID idx) const  { return BS_MemberP(_asm_alias, idx); }
  BOOL     Weak_var(AUX_ID idx) const   {  return BS_MemberP(_weak_var, idx); }
  BOOL     Weak_base(AUX_ID idx) const  {  return BS_MemberP(_weak_base, idx); }

  void     Set_addr_saved(AUX_ID idx)   {  _addr_saved = BS_Union1D(_addr_saved, idx, mem_pool); }
  void     Set_addr_passed(AUX_ID idx)  {  _addr_passed = BS_Union1D(_addr_passed, idx, mem_pool); }
  void     Set_addr_used_locally(AUX_ID idx) {  _addr_used_locally = BS_Union1D(_addr_used_locally, idx, mem_pool); }
  void     Set_external(AUX_ID idx)     {  _external   = BS_Union1D(_external, idx, mem_pool); } 
  void     Set_local_static(AUX_ID idx) {  _local_static = BS_Union1D(_local_static, idx, mem_pool); } 
  void     Set_dedicated(AUX_ID idx)    {  _dedicated  = BS_Union1D(_dedicated, idx, mem_pool); }
  void     Set_ref_formal(AUX_ID idx)   {  _ref_formal = BS_Union1D(_ref_formal, idx, mem_pool); }
  void     Set_named(AUX_ID idx)        {  _named      = BS_Union1D(_named, idx, mem_pool); }
  void     Set_const(AUX_ID idx)        {  _const      = BS_Union1D(_const, idx, mem_pool); }
  void     Reset_const(AUX_ID idx)      {  _const      = BS_Difference1D(_const, idx); }
  void     Set_unique_pt(AUX_ID idx)    {  _unique_pt  = BS_Union1D(_unique_pt, idx, mem_pool); }
  void     Set_virtual_var(AUX_ID idx)  {  _virtual_var= BS_Union1D(_virtual_var, idx, mem_pool); }
  void     Set_weak_var(AUX_ID idx)     {  _weak_var   = BS_Union1D(_weak_var, idx, mem_pool); }
  void     Set_weak_base(AUX_ID idx)    {  _weak_base  = BS_Union1D(_weak_base, idx, mem_pool); }

  BS *     Addr_saved(void) const       {  return _addr_saved; }
  BS *     Addr_passed(void) const      {  return _addr_passed; }
  BS *     Addr_used_locally(void) const {  return _addr_used_locally; }
  BS *     External(void) const         {  return _external; }
  BS *     Local_static(void) const     {  return _local_static; }
  BS *     Dedicated(void) const        {  return _dedicated; }
  BS *     Ref_formal(void) const       {  return _ref_formal; }
  BS *     Named(void) const            {  return _named; }
  BS *     Const(void) const            {  return _const; }
  BS *     Unique_pt(void) const        {  return _unique_pt; }
  BS *     Indirect(void) const         {  return _indirect; }
  BS *     Inaccessible_to_callees(void) const { return _inaccessible_to_callees; }
  BS *     Call_by_value(void) const    {  return _call_by_value; }
  BS *     Call_by_ref(void) const      {  return _call_by_ref; }
  BS *     Virtual_var(void) const      {  return _virtual_var; }
  BS *     Weak_var(void) const         {  return _weak_var; }
  BS *     Weak_base(void) const        {  return _weak_base; }
  BS *     Asm_alias(void) const        {  return _asm_alias; }
 
  void     Set_addr_saved(BS *bs)       { _addr_saved = bs; }
  void     Set_addr_passed(BS *bs)      { _addr_passed = bs; }
  void     Set_addr_used_locally(BS *bs) { _addr_used_locally = bs; }
  void     Set_external(BS *bs)         { _external = bs; }
  void     Set_local_static(BS *bs)     { _local_static = bs; }
  void     Set_dedicated(BS *bs)        { _dedicated = bs; }
  void     Set_ref_formal(BS *bs)       { _ref_formal = bs; }
  void     Set_named(BS *bs)            { _named = bs; }
  void     Set_const(BS *bs)            { _const = bs; }
  void     Set_unique_pt(BS *bs)        { _unique_pt = bs; }
  void     Set_indirect(BS *bs)         { _indirect = bs; }
  void     Set_inaccessible_to_callees(BS *bs) { _inaccessible_to_callees = bs; }
  void     Set_call_by_value(BS *bs)    { _call_by_value = bs; }
  void     Set_call_by_ref(BS *bs)      { _call_by_ref = bs; }
  void     Set_virtual_var(BS *bs)      { _virtual_var = bs; }
  void     Set_weak_var(BS *bs)         { _weak_var = bs; }
  void     Set_weak_base(BS *bs)        { _weak_base = bs; }
  void     Set_asm_alias(BS *bs)        { _asm_alias = bs; }

  // ------------------------------------------------------------------

  WN_MAP   WN_sym_map(void) const	{  return _wn_sym_map; }
  WN_MAP   WN_box_refs(void) const	{  return _wn_box_refs; }
  WN_MAP   WN_box_defs(void) const	{  return _wn_box_defs; }
  void     Set_WN_sym_map(WN_MAP wnm)	{  _wn_sym_map = wnm; }
  void     Set_WN_box_refs(WN_MAP wnm)	{  _wn_box_refs = wnm; }
  void     Set_WN_box_defs(WN_MAP wnm)	{  _wn_box_defs = wnm; }

  // ------------------------------------------------------------------

  //  Default virtual variable
  AUX_ID   Default_vsym(void) const     { return _default_vsym; }
  void     Set_default_vsym(AUX_ID vsym){ _default_vsym = vsym; }
  //  Return virtual variable
  AUX_ID   Return_vsym(void) const     { return _return_vsym; }
  void     Set_return_vsym(AUX_ID vsym){ _return_vsym = vsym; }

  OPT_PHASE Phase(void) const		{ return _phase; }

  //  Representation of references/definitions for black-boxes
  POINTS_TO_LIST *Black_box_refs(const WN *wn) const;
  POINTS_TO_LIST *Black_box_defs(const WN *wn) const;
  void     Add_black_box_ref( WN *wn, POINTS_TO *ref ) const;
  void     Add_black_box_def( WN *wn, POINTS_TO *def ) const;

  // ------------------------------------------------------------------
  // Alias Analysis
  // ------------------------------------------------------------------

  //  Contain what phase of alias analysis
  BOOL     FFA(void) const { return _flow_free_analysis; }
  BOOL     FSA(void) const { return !_flow_free_analysis; }
  void     Set_FFA(void)   { _flow_free_analysis = TRUE; }
  void     Set_FSA(void)   { _flow_free_analysis = FALSE; }

  //  Alias analysis and update
  BOOL     Transfer_alias_class_to_occ_and_aux(RID *, WN *);
  void     Transfer_alias_tag_to_occ_and_aux(RID *, WN *);
  void     Compute_FFA(RID *);		// Flow free alias analysis
  void     Compute_FFA_for_copy(WN *, BB_NODE *, BOOL);// FFA for i=i copy
  void     Compute_FSA(void);		// Flow sensitive alias analysis
  void     Remap_ver_synonym(WN *wn);

  //  Create the entry chi statement and its chi list
  WN       *Create_entry_chi_stmt(BB_NODE *);

  //  Update POINTS_TO info from an array node
  void     Analyze_Range(WN *, POINTS_TO *);

  ID_MAP<ST_CHAIN_INFO *, ST_IDX>  *St_chain_map(void) const
  		{ return st_chain_map; }

  // used for dce's extra alias analysis to describe that something
  // is aliased with all globals
  POINTS_TO *Points_to_globals( void ) const
		{ return _points_to_globals; }
  void     Set_points_to_globals( POINTS_TO *pt )
		{ _points_to_globals = pt; }

  //  The Simplify_Pointer_... functions are a set of
  //  mutually recursive routine to simplify pointer expressions.
  void     Simplify_Pointer_Ver(VER_ID ver, POINTS_TO *ai);
  void     Simplify_Pointer_Arith(WN *wn, POINTS_TO *ai);
  void     Simplify_Pointer(WN *wn_addr, POINTS_TO *ai);

  //  Lower the _base in the POINTS_TO the layout-ed base
  void     Lower_to_base(POINTS_TO *, WN *);

  //  Determine the base symbol by flow free analysis.x
  void     Analyze_Base_Flow_Free(POINTS_TO *pt, WN *wn);

  //  Determine the base symbol using the use-def chain.
  void     Analyze_Base_Flow_Sensitive(POINTS_TO *, WN *);
#if defined (TARG_SL)
  void     Analyze_Base_Flow_Free_for_Intrn(POINTS_TO *pt, WN *wn);
  void     Analyze_Base_Flow_Sensitive_for_Intrn(POINTS_TO *pt, WN *wn);
#endif
  //  Update the default vsym after flow sensitive analysis.
  void     Update_iload_vsym(OCC_TAB_ENTRY *);
  void     Update_istore_vsym(OCC_TAB_ENTRY *);

  //  Remove variables that does not need to be saved at RETURN
  void     Update_return_mu(void);

  //  Determine out the based pointer
  ST      *Find_Based_Pointer(WN *, INT *depth);

  //  Update the POINTS_TO with the based pointer
  void     Analyze_Based_Pointer(POINTS_TO *, WN *);

  //  Determine if this OCC need to use default vsym, and does it.
  AUX_ID   Adjust_vsym(AUX_ID vp_idx, OCC_TAB_ENTRY *occ);

  // ------------------------------------------------------------------

  //  PREG-related stuff
  void     Init_last_preg(const IDTYPE preg)
			{ _last_preg_num = _orig_last_preg = preg; }
  // allocate a new preg with a given type and name (may be NULL)
  // note: create_preg may return a preg that is not the last one
  // allocated because it may "pre-allocate" one, but not return it,
  // when the type is too large to fit in the target registers.
  IDTYPE   Alloc_preg(TYPE_ID mtype, const char *name = NULL, WN *home_wn = NULL)   
    {
      IDTYPE preg = Create_Preg(mtype,name,home_wn);
      _last_preg_num = Get_Preg_Num(PREG_Table_Size (CURRENT_SYMTAB));
      return preg;
    }

  IDTYPE   Last_preg(void) const             
			{ return _last_preg_num; }

  BOOL     Is_up_level_var(ST *st) const
                        { return ST_sym_class (st) == CLASS_VAR &&
                            GLOBAL_SYMTAB != ST_level(st) &&
                            CURRENT_SYMTAB != ST_level(st);
                        }

  // ------------------------------------------------------------------
  //  Various PRINT functions
  // ------------------------------------------------------------------

#ifdef KEY
  void     Print(FILE *fp=stderr, WN* entry_wn=NULL);
#endif
  void     Print_aux_entry(AUX_ID i, FILE *fp=stderr); // if fp == 0, fp is stderr
  void     Print_alias_info(FILE *fp=stderr);
#ifdef KEY
  void     Print_occ_tab(FILE *fp=stderr, WN* entry_wn=NULL);
#endif
  void     Print_top_nth_coderep(AUX_ID i, INT n, FILE *fp=stderr);

  // ------------------------------------------------------------------
  // manage the OCC_TAB
  // ------------------------------------------------------------------

#ifdef KEY
  OCC_TAB_ENTRY *Enter_occ_tab(WN *, AUX_ID, POINTS_TO * = NULL);
#else
  OCC_TAB_ENTRY *Enter_occ_tab(WN *, AUX_ID);
#endif
  OCC_TAB_ENTRY *Get_occ(const WN *) const;
  CHI_LIST *Get_mem_chi_list(const WN *) const;
  MU_NODE  *Get_mem_mu_node(const WN *) const;
  CHI_LIST *Get_stmt_chi_list(const WN *) const;
  MU_LIST  *Get_stmt_mu_list(const WN *) const;
  CHI_LIST *Get_generic_chi_list(const WN *) const;
  void      Update_pf_list(WN *, STMTREP *);

  // ------------------------------------------------------------------
  // manage the mapping of entries <-> itable bit positions
  // ------------------------------------------------------------------

  void     Clear_itab_bitpos( void );
  void     Set_itab_bitpos( AUX_ID var, IDX_32 bitpos )
		{ aux_stab[var].Set_itab_bitpos(bitpos); }
  void     Rename_aux_id_list_to_bitpos( void );

  // ------------------------------------------------------------------

  // entry chi
  void	   Create_entry_chi(void);

  // mu and chi for black box regions or mu for regions being processed
  void     Compute_region_mu_chi(WN *, RID *, BOOL, BB_NODE *);

  // reset the def_bbs field. Used in SSU.
  void     Reset_def_bbs(void);

  // screen out inconsistent signess variables
  void     Screen_rvi_candidates(void);

  void     Set_alias_classification(ALIAS_CLASSIFICATION &ac)
    { _alias_classification = &ac; }
  ALIAS_CLASSIFICATION     *Alias_classification(void) const
    { return _alias_classification; }

  MEMOP_ANNOT_CR_SR_MGR* Cr_sr_annot_mgr (void) const { return _cr_sr_annot_mgr;} 

  void     Incorporate_alias_class_info(void);

  // convert EH region pragma block from aux_ids to STs and offsets
  // called by main emitter
  void     Convert_EH_pragmas(WN *wn);

  BOOL     Safe_to_speculate(AUX_ID);

#ifdef KEY
  AUX_ID   Part_of_reg_size_symbol(AUX_ID);
#endif

  ST      *St_ptr(WN *wn) const      { return aux_stab[WN_aux(wn)].St(); }
  void    Summarize_points_to (void);
};


// ====================================================================
//
// AUX_STAB_ITER and AUX_STAB_REVERSE_ITER are iterators for the
// optimizer's auxiliary symbol table.   AUX_STAB_ITER iterates forward
// over the symbol table indices from 1 up to aux_stab->Lastidx() - 1.
// AUX_STAB_REVERSE_ITER iterates backwards over the symbol table
// indices from aux_stab->Lastidx() down to 1.
//
// Example of use:
//   AUX_STAB_ITER aux_stab_iter(Opt_stab()->Aux_stab());
//   AUX_ID aux_id;
//   FOR_ALL_NODE(aux_id, aux_stab_iter, Init()) {
//     AUX_STAB_ENTRY *sym = Opt_stab()->Aux_stab_entry(aux_id);
//
// ====================================================================


class AUX_STAB_ITER {
private:
  AUX_ID    sym_id;      // the symbol id
  OPT_STAB *stab;        // symbol table

  AUX_STAB_ITER(void);
  AUX_STAB_ITER(const AUX_STAB_ITER&);
  AUX_STAB_ITER& operator = (const AUX_STAB_ITER&);

public:
  AUX_STAB_ITER(OPT_STAB *symtab)
    : sym_id(1), stab(symtab)   {}
  ~AUX_STAB_ITER(void)          {}
  void   Init(void) const       {}
  AUX_ID First(void)            { return (sym_id = 1); }
  BOOL   Is_Empty(void)         { return sym_id > stab->Lastidx(); }
  AUX_ID Next(void)             { return ++sym_id; }
  AUX_ID Cur(void) const        { return sym_id; }
};


class AUX_STAB_REVERSE_ITER {
private:
  AUX_ID    sym_id;      // the symbol id
  OPT_STAB *stab;        // symbol table

  AUX_STAB_REVERSE_ITER(void);
  AUX_STAB_REVERSE_ITER(const AUX_STAB_REVERSE_ITER&);
  AUX_STAB_REVERSE_ITER& operator = (const AUX_STAB_REVERSE_ITER&);

public:
  AUX_STAB_REVERSE_ITER(OPT_STAB *symtab)
    : sym_id(symtab->Lastidx()), stab(symtab) {}
  ~AUX_STAB_REVERSE_ITER(void)  {}
  void   Init(void) const       {}
  AUX_ID First(void)            { return (sym_id = stab->Lastidx()); }
  BOOL  Is_Empty(void)          { return sym_id < 1; }
  AUX_ID Next(void)             { return --sym_id; }
  AUX_ID Cur(void) const        { return sym_id; }
};


// ====================================================================
//
// VER_STAB_ITER is an iterator for the SSA version table.  It iterates
// over the version table indices from 2 up to ver_stab->Size() - 1.
//
// Example of use:
//   VER_STAB_ITER ver_stab_iter(Opt_stab()->Ver_stab());
//   VER_ID ver_id;
//   FOR_ALL_NODE(ver_id, ver_stab_iter, Init()) {
//     VER_STAB_ENTRY *vse = Opt_stab()->Ver_stab_entry(ver_id);
//
// ====================================================================


class VER_STAB_ITER {
private:
  VER_ID               ver_id;      // the version id
  VER_STAB_ARRAY_TYPE *ver_stab;    // SSA version table

  VER_STAB_ITER(void);
  VER_STAB_ITER(const VER_STAB_ITER&);
  VER_STAB_ITER& operator = (const VER_STAB_ITER&);

public:
  VER_STAB_ITER(VER_STAB_ARRAY_TYPE *vertab)
    : ver_id(2), ver_stab(vertab) {}
  ~VER_STAB_ITER(void)	        {}
  void   Init(void) const       {}
  VER_ID First(void)            { return (ver_id = 2); }
  BOOL   Is_Empty(void)         { return ver_id >= ver_stab->Size(); }
  VER_ID Next(void)             { return ++ver_id; }
  VER_ID Cur(void) const        { return ver_id; }
};


// ====================================================================


#endif  // opt_sym_INCLUDED
