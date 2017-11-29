/*
 * Copyright (C) 2009 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

//-*-c++-*-

/*
 * Copyright 2002, 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
 */

// ====================================================================
// ====================================================================
//
// Module: opt_htable.h
// $Revision: 1.1.1.1 $
// $Date: 2005/10/21 19:00:00 $
// $Author: marcel $
// $Source: /proj/osprey/CVS/open64/osprey1.0/be/opt/opt_htable.h,v $
//
// Revision history:
//  27-SEP-94 shin - Original Version
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
// ====================================================================
// ====================================================================


#ifndef opt_htable_INCLUDED
#define opt_htable_INCLUDED "opt_htable.h"

#ifndef __ALLOCA_H
#include <alloca.h>
#endif

#ifndef ERRORS_INCLUDED
#include "errors.h"
#endif
#ifndef opt_defs_INCLUDED
#include "opt_defs.h"
#endif
#ifndef opt_array_INCLUDED
#include "opt_array.h"
#endif
#ifndef wn_INCLUDED
#include "wn.h"
#endif
#ifndef wn_map_INCLUDED
#include "wn_map.h"
#endif
#ifndef opt_util_INCLUDED
#include "opt_util.h"		// Warn_todo
#endif
#include "stab.h"
#include "cxx_base.h"
#include "cxx_memory.h"
#include "opt_leaf_iter.h"
#include "opt_wn.h"
#ifndef opt_wn_INCLUDED
#include "opt_wn.h"
#endif
#ifndef optimizer_INCLUDED
#include "optimizer.h"
#endif
#ifndef opt_stmt_INCLUDED
#include "opt_stmt.h"
#endif
#ifndef targ_const_INCLUDED
#include "targ_const.h"		// TCON structure
#endif

#include "tracing.h"
#include "id_map.h"
#include "opt_ssa.h"

#include <vector>
#include <map>
#include "be_memop_annot.h"

using idmap::ID_MAP;

// forward declarations
class BB_NODE;
class BB_LOOP;
class BB_NODE_SET;
class CANON_CR;
class CFG;
class CHI_LIST;
class CHI_NODE;
class CODEREP;
class COLOR;
class DU_MANAGER;
class EMITTER;
class EXC;
class EXC_SCOPE;
class IND_EXPR;
class ITABLE;
class MAIN_EMITTER;
class MU_NODE;
class MU_LIST;
class OCC_TAB_ENTRY;
class OPT_STAB;
class PHI_HASH_ENTRY;
class POINTS_TO;
class SSA;
class STMT_CONTAINER;
class STMTREP;
class USE_LIST;
class COPYPROP;

struct CLOBBER_PRAGMA_INFO {
  ST_IDX   preg_st_idx;
  PREG_IDX preg_number;
  INT32    clobber_string_idx;
};

struct CONSTRAINT_PRAGMA_INFO {
  ST_IDX   preg_st_idx;
  ST_IDX   constraint_st_idx;
  PREG_NUM asm_neg_preg;
  UINT32   asm_opnd_num;
};

struct ASM_PRAGMA_INFO {
  vector<CLOBBER_PRAGMA_INFO,
         mempool_allocator<CLOBBER_PRAGMA_INFO> > clobber_info;
  vector<CONSTRAINT_PRAGMA_INFO,
         mempool_allocator<CONSTRAINT_PRAGMA_INFO> > constraint_info;

  ASM_PRAGMA_INFO(MEM_POOL *pool) :
    clobber_info(pool),
    constraint_info(pool)
      { }
};

typedef CODEREP *CODEREP_P;

// TODO: declare locally till the symbol table is defined
#define NIL   -1

enum CODEKIND {
  CK_LDA     = 0x01,        // Load address
  CK_CONST   = 0x02,        // Compile time constant
  CK_RCONST  = 0x04,        // symbolic constant or constant in ST
  CK_VAR     = 0x08,        // Variable
  CK_IVAR    = 0x10,        // Indirect load
  CK_OP      = 0x20,        // Operator
  CK_DELETED = 0x40,        // Code node is deleted
// do not add new values without adding more bits to CODEREP's kind field
};

inline BOOL inCODEKIND( CODEKIND ck, INT ck_flags )
{
  return ( (ck & ck_flags) != 0 );
}

enum CR_FLAG {
  CF_EMPTY         = 0x00, // no flag set
  CF_C_P_PROCESSED = 0x01, // For non-leaf only: has been processed by copy
                           // propagation
  CF_LDA_LABEL     = 0x01, // this CK_LDA node is an LDA_LABEL
  CF_DONT_PROP     = 0x02, // Do not copy propagate this CK_VAR/CK_IVAR
  CF_C_P_REHASHED  = 0x02, // For non-leaf only: has been rehashed due to copy
                           // propagation of the var/ivar nodes within it;
                           // kid[0] will point to replacing node
  CF_SPRE_REMOVED  = 0x04, // the store of this variable has been removed by
			   // SPRE, so any use of this node needs to be renamed
  CF_DEF_BY_PHI    = 0x08, // defined by phi
  CF_DEF_BY_CHI    = 0x10, // defined by chi
                           // if defined by normal coderep, no flag
                           // but w defstmt
  CF_OWNED_BY_TEMP = 0x20, // for CK_OP and CK_IVAR only, in SSAPRE, coderep
			   // is to be changed to temporary
  CF_INCOMPLETE_USES = 0x40, // CK_VAR only; indicates it is converted from
			   // a zero version by Find_def(), but not all its 
			   // uses have been converted; i.e. there may be a
			   // zero-version use that should be this node
  CF_IS_ZERO_VERSION= 0x80,// is a zero version
  CF_FOLDED_LDID   = 0x100,// is folded from (ILOAD(LDA)) 
  CF_MADEUP_TYPE   = 0x200,// the type is made up by SSA
#ifdef TARG_SL
  // offset relative to internal memory (vbuf & sbuf)  and used as parameter 
  // in intrinisc_vbuf_offset and intrinsic_sbuf_offset
  CF_INTERNAL_MEM_OFFSET = 0x400, 
#endif 
  // do not add new values without checking CODEREP's flags field
};

enum ISOP_FLAG {
  ISOP_EMPTY         = 0x00, // no flag set
  ISOP_ICOPY_VISITED = 0x01, // has been visited by Propagatable during icopy
  ISOP_COPY_VISITED  = 0x02, // has been visited by Propagatable during copy
  ISOP_FOLD_EXPR_VISITED = 0x04, // has been visited by Fold_Expr
  ISOP_FOLD_TREE_VISITED = 0x08, // has been visited by Fold_Tree (not used)
  ISOP_DCE_VISITED  = 0x10, // has been visited during DCE
  ISOP_RETVSYM_VISITED = 0x20, // has been visited during DCE retvsym
  ISOP_VER_MAP_VISITED = 0x40, // has been visited by Verify_CODEMAP
  ISOP_CANON_VISITED   = 0x80, // has been visited by Canon_expr.
  ISOP_CONTAIN_VISITED = 0x100, // has been visited by Contains_only_the_var.
  ISOP_ANTLOC_VISITED  = 0x200, // has been visited by CODEREP::Antloc
  ISOP_INVARIANT_VISITED = 0x400, // has been visited by BB_LOOP::Invariant_cr
  ISOP_EMIT_CONST_VISITED = 0x800, // has been visited by 
  			     // MAIN_EMITTER::Check_expr_resolves_to_const
  ISOP_IVE_LIVE  = 0x1000, // has been visited during IVE
  ISOP_SSAPRE_OMITTED = 0x1000, // an occurrence omitted from SSAPRE worklst
  ISOP_IVE_VISITED = 0x2000, // has been visited during IVE for coderep
				// in the Source() bit vector
  ISOP_XLOWER_VISITED = 0x4000,// has been visited during Lower_to_extract_compose
  ISOP_LDAFOLD_VISITED = 0x8000,// has been visited during first pass of
  				// Fold_lda_iload_istore
  ISOP_LDAFOLD2_VISITED = 0x10000,// has been visited during second pass of
  				// Fold_lda_iload_istore
  ISOP_MTYPE_B_CR_VISITED = 0x20000,  // has been visited during m_mtype_b_cr
  ISOP_VERIFY_EXPR_VISITED = 0x40000, // has been visited during verify_version_expr
  ISOP_DEF_BEFORE_VISITED = 0x80000,  // has been visited during Def_before_use     
  // at most 22 bits for this enumeration, due to size of isop_flags field
};

enum ISVAR_FLAG {
  ISVAR_SAFE_TO_RENUMBER_PREG = 0x01,
  ISVAR_BIT_FIELD_VALID       = 0X02,	// Bit_size() and Bit_offset() valid
#ifdef KEY
  ISVAR_PROMOTE_TO_REG_SIZE   = 0x04,   // promotable byte- and half-sized vars
  					// to register size (4- or 8-bytes) via
  					// extract/compose
  ISVAR_MP_SHARED	      = 0x08,   // shared variable in MP region
#endif
};

enum ISCONST_FLAG {
  ISCONST_RVI_CANDIDATE = 0x1,    // used by TARG_SL 
}; 

// return value of Propagatable to tell whether an expression can be
// propagated to the current point in the code
enum PROPAGATABILITY { 
    NOT_PROPAGATABLE,		// cannot be propagated
    PROP_WITH_INVERSE,		// can be propagated only after non-current
				// versions are transformed to current version
				// using inverse function
    PROPAGATABLE,		// can be propagated
// must be in increasing order of propagatability so that, when combining
// two values, and just take the min
};

// return value of Check_if_result_is_address to tell whether we can infer
// that the result of an expression tree is a system address
enum ADDRESSABILITY {
    ADDRESSABILITY_UNKNOWN,		// cannot verify
    ADDRESSABILITY_NOT_ADDRESS,		// sure that it is not an address
    ADDRESSABILITY_IS_ADDRESS,		// sure that it is a system address
};
		

// GUIDELINE to use this iterator, define it on the stack
class EXP_KIDS_ITER {
  mUINT32   kid_count;
  mUINT32   cur_idx;
  CODEREP **kids;

  EXP_KIDS_ITER(void);
  EXP_KIDS_ITER(const EXP_KIDS_ITER&);
  EXP_KIDS_ITER& operator = (const EXP_KIDS_ITER&);
public:
  EXP_KIDS_ITER( mUINT32 cnt, CODEREP **kp);
  ~EXP_KIDS_ITER(void)            {}
  void      Init(void)            {}
  BOOL      Is_Empty(void)        { return (cur_idx>=kid_count);}
  CODEREP  *Cur_elem(void)        { return (!Is_Empty())?
                                      kids[cur_idx] : NULL; }
  CODEREP  *First_elem(void)      { cur_idx = 0; return Cur_elem(); }
  CODEREP  *Next_elem(void)       { cur_idx++; return Cur_elem(); }
  IDX_32    First(void)           { cur_idx = 0; return cur_idx; }
  IDX_32    Next(void)            { cur_idx++; return cur_idx; }
  IDX_32    Cur(void)             { return cur_idx; }
};

class CODEREP : public SLIST_NODE {
friend class CODEREP_CONTAINER;
friend class CODEMAP;
private:
  CODEKIND  kind:7;                  // code kind
  MTYPE     _dtyp:6;                 // data type
  MTYPE     dsctyp:6;                // descriptor type for various opcode
#ifdef TARG_SL
  UINT32    usecnt:12;               // number of times this node's
                                     // expression appears.
                                     // not used for ISCONST and ISLDA
  CR_FLAG   flags:11;                
#else
  UINT32    usecnt:13;               // number of times this node's
                                     // expression appears.
                                     // not used for ISCONST and ISLDA
  CR_FLAG   flags:10;    
#endif            
  UINT32   _is_sign_extd:1;          // load is sign
  UINT32    is_lcse:1;               // one bit for lcse, also used for IVE
  UINT32    is_saved:1;              // another bit for already saved
  UINT32   _is_volatile:1;           // volatile reference?
  IDTYPE   _bb_id:18;                // processing this CR in BB temp_id in local_attr or main_emitter
#define ILLEGAL_BB 0xfffff	     // initialization value for emit_bb

  INT32     _coderep_id;              // unique id of this node
  union {
    struct {
      union {
        IDX_32    bitpos;            // PRE bit-position
        IDTYPE    e_num;             // SSA-PRE E-number
	UINT32    asm_operand_num;   // Operand index for ASM_INPUT
				     // (which is never processed by PRE)
        IDTYPE    scalar_aux_id;     // used when lowering LDBITS nodes or
				     // when folding lda-iload/istore
	OCC_TAB_ENTRY *scalar_ivar_occ; // used when lowering ILDBITS nodes
      } _u01;
      union {
	mINT32   offset;             // offset for LOD/LDA/ILOD
	struct {
	  mUINT8 op_bit_offset;	     // for EXTRACT_BITS and COMPOSE_BITS
	  mUINT8 op_bit_size;	     // for EXTRACT_BITS and COMPOSE_BITS
	} op_bit_offset_size;
	INTRINSIC intrinsic;         // for INTRINSIC_CALL
#ifdef KEY
	ST_IDX   call_op_aux_id;     // for PURE_CALL_OP
#endif
        CODEREP *index;              // index register for ILOADX
        TY_IDX   ty_index;           // for TAS
	ST_IDX   asm_constraint;     // for ASM_INPUT
      } u11;
    } nonarr;
    INT64	elm_siz;	     // for OPR_ARRAY
  }u1;

  union {
    struct {                         // for code kind CK_VAR
      IDTYPE    aux_id;              // the entry number in aux symbol table 
      mUINT32   version;             // version for VAR
      union {
	// A CR defined by STMTREP has mu_list, but no chi_node or phi_node.
	// A CR defined by either chi or phi has no mu_list
	CHI_NODE *chi;               // the chi node that define this cr
	PHI_NODE *phi;               // the phi node that define this cr
      } def;
      mUINT16   _isvar_flags;        // flags specific to CK_VAR
      mUINT16  fieldid;		     // field id (also used as bit ofst/size)
      STMTREP  *defstmt;             // statement that defines this var
      TY_IDX    ty;                  // LOD type  
    } isvar;
    struct {                         // for code kind CK_LDA
      IDTYPE    aux_id;              // the entry number in aux symbol table 
      ST       *base_st;	     // the base
      TY_IDX    ty;                  // type pointer for LDA
      mUINT16   afieldid;	     // field id of the LDA
    } islda;
    union {                          // ISCONST ISRCONST
      ST        *const_id;            // symbolic constant or constant, ISRCONST
#if defined(TARG_SL)
      struct {
        INT64    const_val;           // constant value, ISCONST
        mUINT16  _isconst_flags; 
      }isconst_val; 
#else 
      INT64     const_val;           // constant value, ISCONST
#endif
    } isconst;
    // KEY: added fields _asm_input_dtyp, _asm_input_dsctyp, _unused
    struct {                         // for code kind ISOP
      OPERATOR  _opr:8;	             //
      ISOP_FLAG isop_flags:22;	     
      PROPAGATABILITY propagatability:2; // used during copy propagation
      mINT32    kid_count:14;        // number of kids
      MTYPE     _asm_input_dtyp:6;                  // data type
      MTYPE     _asm_input_dsctyp:6;                // descriptor type for various opcode
      mINT32    _num_of_min_max:6;   // number of minmax, collectively
      mUINT8    max_depth;           // used in estimating rehash cost (SSAPRE)
      IDTYPE    _temp_id:24;         // processing this CR in new PRE step1
      void * node_cache;             // Hold CR or BB pointer for parents on new differnt paths       
      CODEREP  *kids[3];             // array of kid pointers
    } isop;
    struct {                         // for code kind IVAR(ILOD)
      OPERATOR  _opr:8;	             // 
      mINT32    _num_of_min_max:6;   // number of minmax, collectively
      mINT32    _unused:2;     	     // unused
      mUINT16   ifieldid;	     // field id
      MU_NODE  *mu_node;	     // MU-list for this memory ref
      STMTREP  *defstmt;             // defining stmt for ILOD
      CODEREP  *base[2];             // the base address expr, base[0]
      //IDTYPE  occ;                 // dynamically created, access base[1]
      //TY     *ilod_ty;             // dynamically created, access base[2]
      //CODEREP *base[3];            // dynamically created, give STORE's
				     // base address
      //TY     *ilod_base_ty;        // base type of load_addr_type, base[4]
                                     // for MSTORE/MLOAD number of bytes (size)
    } isivar;
  }u2;
#define IVAR_EXTRA_NODE_CNT 3

  CODEREP& operator = (const CODEREP&);

  // try to keep coderep's volatile flag references only within
  // other access methods so we can more easily check that the
  // coderep is the right type to be using this flag, which should
  // only be used for vars (at least initially)
  BOOL      Is_volatile(void) const   { return _is_volatile; }
  void      Set_is_volatile(void)     { _is_volatile = 1; }
  void      Reset_is_volatile(void)   { _is_volatile = 0; }
  void      Assign_is_volatile(BOOL b){ _is_volatile = b; }

  // Put all the defaults here.  There should be only ONE place
  // to change the default values for CR.
  void Init(CODEKIND ck)
    {
      Set_kind(ck);
      Set_usecnt(0);
      Set_Bitpos(ILLEGAL_BP);
      Reset_is_lcse();
      Reset_is_saved();
      Reset_sign_extd();
      Reset_is_volatile();
      Set_emit_bb(ILLEGAL_BB);
      _coderep_id = 0;
      Reset_flags();
      if (ck == CK_IVAR) 
	Set_ivar_mu_node(NULL);
      if (ck == CK_OP || ck == CK_IVAR) 
	Set_Num_MinMax(-1);
    }

  INT32	Num_MinMax(void) const
  {
      CODEKIND ck = Kind();
      if (ck == CK_IVAR) return u2.isivar._num_of_min_max;
      if (ck == CK_OP)   return u2.isop._num_of_min_max;
      return 0;
  }

  void Set_Num_MinMax(INT32 v) 
  {
      CODEKIND ck = Kind();
      if (ck == CK_IVAR) u2.isivar._num_of_min_max = v ;
      if (ck == CK_OP)   u2.isop._num_of_min_max = v ;
  }

  // All opnds of the mu_list is defined by STMTREP *
  BOOL Match_mu_and_def(STMTREP *, INT32, OPT_STAB *) const;
  // Two mu_list are defined by the same STMTREP
  BOOL Match_mu_and_mu(MU_NODE *, INT32, OPT_STAB *) const;
  // called only by CODEREP::Antloc
  BOOL      Antloc_rec (BB_NODE *);      // this cr is antloc in bb

public:
  CODEREP(void)			{}
  CODEREP(const CODEREP &cr)	{ Copy(cr); }
  ~CODEREP(void)		{}

  INT32       Count_MinMax(void);

  // this has to be a define because it is sometimes used before the CR exists
#define Alloc_stack_cr(cnt) ((CODEREP*)alloca(sizeof(CODEREP)+(cnt)*sizeof(CODEREP *)))

  UINT32 Extra_ptrs_used(void) const
    {
      switch (Kind()) {
      case CK_OP:
	if (Kid_count() > 3) {
	  return Kid_count() - 3;
	}
	else {
	  return 0;
	}
      case CK_IVAR:
	return IVAR_EXTRA_NODE_CNT;
      default:
	return 0;
      }
    }

  size_t Extra_space_used(void) const
    { return sizeof(CODEREP *) * Extra_ptrs_used(); }

  void Init_lda(MTYPE wt, IDTYPE st, mINT32 ofst, TY_IDX tt, ST *bas,
  		UINT16 field_id = 0)
    {
      Init(CK_LDA); Set_dtyp(wt);  Set_lda_ty(tt); Set_offset(ofst);
      Set_lda_aux_id(st); Set_lda_base_st(bas); Set_afield_id(field_id);
#ifdef KEY
      Set_dsctyp(MTYPE_V);
#endif

    }

  void Init_const(MTYPE wt, INT64 v)
    {
      Init(CK_CONST); 
#if !defined(TARG_X8664) && !defined(TARG_NVISA)
      if (wt == MTYPE_U4 || wt == MTYPE_I4) 
	Set_dtyp_const_val(wt, (v << 32) >> 32);
#else
      if (wt == MTYPE_I4) 
	Set_dtyp_const_val(wt, (v << 32) >> 32);
      else if (wt == MTYPE_U4) 
	Set_dtyp_const_val(wt, (UINT64) ((UINT64) v << 32) >> 32);
#endif
      else Set_dtyp_const_val(wt, v);

#ifdef KEY
      Set_dsctyp(MTYPE_V);
#endif

    }

  void Init_rconst(MTYPE wt, ST *v)
    {
      Init(CK_RCONST); Set_dtyp(wt); Set_const_id(v);
#ifdef KEY
      Set_dsctyp(MTYPE_V);
#endif

    }

  void Init_op(OPCODE c, mINT16 kcnt)
    {
      Init(CK_OP); Set_opr(OPCODE_operator(c));
      Set_dtyp(OPCODE_rtype(c));
      Set_dsctyp(OPCODE_desc(c));
      Set_kid_count(kcnt);
      Set_temp_id(0);
      Reset_isop_flags();
      Set_max_depth(0);
      Set_ISOP_mtype_b_cache(NULL);
    }

  void Init_var(MTYPE wt, IDTYPE st, mUINT16 ver, MTYPE dt, mINT32 ofst,
		TY_IDX ty, UINT16 field_id)
    {
      Init(CK_VAR); Set_dtyp(wt); Set_aux_id(st); Set_version(ver);
      Set_dsctyp(dt); Set_offset(ofst); Set_lod_ty(ty); Set_defstmt(NULL); 
      Reset_isvar_flags(); Set_field_id(field_id);
    }

  void Init_ivar(OPCODE opc, MTYPE wt, OCC_TAB_ENTRY *occ, 
		 MTYPE dt, TY_IDX ldty, CODEREP *lbase, CODEREP *sbase,
		 INT32 ofst, CODEREP *size, UINT16 field_id)
    {
      Init(CK_IVAR); Set_opr(OPCODE_operator(opc)); Set_dtyp(wt); Set_ivar_occ(occ);
      Set_dsctyp(dt); Set_ilod_ty(ldty);
      Set_ilod_base(lbase); Set_istr_base(sbase);
      Set_offset(ofst); Set_mstore_size(size); Set_ivar_defstmt(NULL); 
      Set_i_field_id(field_id);
    }

  void Init_expr(OPCODE c, CODEREP *expr)
    {
      Init_op( c, 1 );
      Set_opnd(0, expr);
    }

  void	    Copy(const CODEREP &cr);  // copy fields, no allocate

  BOOL      Match(CODEREP *cr, 
		  INT32 mu_vsym_depth = 0,
		  OPT_STAB *opt_stab = NULL); // compare two nodes

  BOOL      Match_constval(const CODEREP *); // compare two const nodes

  // returns TRUE if this tree contains the node 'cr'; checks only
  // rvalues, not CK_IVAR lvalues.
  BOOL      Contains(const CODEREP *cr) const;

  // returns TRUE if this tree contains the same image(bitpos) as node 'cr'
  BOOL      Contains_image(const CODEREP *cr) const;

  BOOL      Satisfy_IV_cond(CODEREP*, // returns TRUE if this tree
                            BB_NODE_SET*,// contains the node 'cr'
                            INT);                                      
  BOOL      Removed_by_LDX(BB_NODE *bb)const;

  void      Print(INT32 indent,       // print the content for debugging
                  FILE *fp = stderr)
                  const;

  void      Print_node(INT32 indent,  // print the CODEREP
		       FILE *fp = stderr) const;

  char     *Print_str(BOOL) const;    // print CODEREP to a string
  char     *Print_kind(void) const;   // print Kind field to a string

  BOOL	    Verify_IR(CFG *, CODEMAP *, BOOL, INT);
				      // consistency check on opt IR
  INT	    Count_parents(CFG *, CODEMAP *);
  				      // part of verification, # parents

  WN       *Gen_wn(EMITTER*);	      // generate WN node from CODEREP
  WN       *Gen_wn(MAIN_EMITTER*, BB_NODE*);// mainopt emitter
  // mainopt emitter -- checks non-required exprs to see if any
  // must be saved to pregs
  void      Check_unsaved_exprs(MAIN_EMITTER*, BB_NODE*);
  void      Canon_expr(CODEMAP *htable);// canonicalize an expression
  BOOL      Ind_expr_canon(CODEMAP *htable,
                           CODEREP *exp,
                           CODEREP **factor,
                           INT64 *scale);

  // insert TAS and CVTL to expr
  //  CODEREP  *Convert_type(CODEMAP *, CODEREP *expr, MTYPE expr_ty);
  CODEREP  *Convert_type(CODEMAP *, CODEREP *expr, BOOL icopy_phase=FALSE);
  CODEREP  *Fixup_type(MTYPE,CODEMAP*); // Insert correct type conversion
  void      IncUsecnt_rec(void);	// recursive version of IncUsecnt
  void      DecUsecnt_rec(void);	// recursive version of DecUsecnt
  void      DecKidsUsecnt_rec(void);	// same as above, but kids only

  // member access functions
  CODEKIND  Kind(void) const          { return (CODEKIND)(0x7f&(INT)kind); }
  void      Set_kind(CODEKIND k)      { kind = k; }
  MTYPE     Dtyp(void) const          { return _dtyp; }
  // CK_CONST node is not allowed to use Set_dtyp, but must use 
  // Set_dtyp_const_val instead so that we can commonize the dtyp field
  // based on the value of const_val. In the rare case that we need to change
  // the dtyp of a CK_CONST node, Set_dtyp_strictly can be used.
  void      Set_dtyp(MTYPE dt)        { Is_True(Kind() != CK_CONST,
					  ("CODEREP::Set_dtyp, illegal kind"));
					_dtyp = dt; }
#ifdef KEY
  MTYPE     Asm_input_rtype(void)  const    { return u2.isop._asm_input_dtyp; }
  void      Set_asm_input_rtype(MTYPE dt)   { u2.isop._asm_input_dtyp = dt; }
  MTYPE     Asm_input_dsctype(void) const   { return u2.isop._asm_input_dsctyp; }
  MTYPE     Set_asm_input_dsctype(MTYPE dt) {  u2.isop._asm_input_dsctyp = dt; }
#endif
#if defined(TARG_SL) || defined(TARG_NVISA)
  void	    Set_dtyp_const_val(MTYPE dt, INT64 v) { 
					Is_True(Kind() == CK_CONST,
					    ("CODEREP::Set_dtyp_const_val, illegal kind"));
					// use given mtype if value fits
#if defined(TARG_SL)
					if ((dt == MTYPE_U4 || dt == MTYPE_I4) && (v == (v << 32) >> 32))
					  _dtyp = MTYPE_I4;
#else
					if (dt == MTYPE_U4
					  && (v == ((UINT64) v << 32) >> 32))
					  _dtyp = MTYPE_U4;
					else if (dt == MTYPE_I4
					  && (v == ((INT64) v << 32) >> 32))
					  _dtyp = MTYPE_I4;
#endif // TARG_SL
					else if (dt == MTYPE_U8)
					  _dtyp = MTYPE_U8;
					else 
					  _dtyp = MTYPE_I8;
#if defined(TARG_SL)
					u2.isconst.isconst_val.const_val = v; }
#else
					u2.isconst.const_val = v; }
#endif // TARG_SL
#else  // TARG_X8664 || TARG_IA64
#ifndef TARG_X8664
  void	    Set_dtyp_const_val(MTYPE dt, INT64 v) { Is_True(Kind() == CK_CONST,
					    ("CODEREP::Set_dtyp_const_val, illegal kind"));
					if (v == (v << 32) >> 32)
					  _dtyp = MTYPE_I4;
					else _dtyp = MTYPE_I8;
					u2.isconst.const_val = v; }
#else
  void	    Set_dtyp_const_val(MTYPE dt, UINT64 v) { Is_True(Kind() == CK_CONST,
					    ("CODEREP::Set_dtyp_const_val, illegal kind"));
					if (v == (v << 32) >> 32)
					  _dtyp = MTYPE_U4;
					else _dtyp = MTYPE_I8;
					u2.isconst.const_val = v; }
#endif
#endif // TARG_SL || TARG_NVISA
  void      Set_dtyp_strictly(MTYPE dt) { _dtyp = dt; }
  MTYPE     Dsctyp(void) const        { return dsctyp; }
  void      Set_dsctyp(const MTYPE t) { dsctyp = t; }
  mINT16    Usecnt(void) const        { return usecnt; }
  void      IncUsecnt(void)	      { if (usecnt >= 1023) Warn_todo
					  ("CODEREP::IncUsecnt overflow");
					usecnt++; }
  void      DecUsecnt(void)           { /* Is_True(usecnt > 0,
					  ("CODEREP::DecUsecnt underflow"));*/
    					usecnt--; }
  void      Set_usecnt(mINT16 c)      { usecnt = c;
					if (c < 0 && c > 1023) Warn_todo
					  ("CODEREP::Set_usecnt range"); }
  BOOL      Is_var_nodef(void) const  { if (Is_flag_set(CF_DEF_BY_PHI))
                                          return Defphi() == NULL;
                                        else if (Is_flag_set(CF_DEF_BY_CHI))
                                          return Defchi() == NULL;
                                        else return Defstmt() == NULL; }
  CR_FLAG   Flags(void) const         { return flags; }
  void      Reset_flags(void)         { flags = CF_EMPTY; }
  void      Reset_flag(CR_FLAG f)     { flags = (CR_FLAG)(flags&~f); }
  void      Assign_flags(CR_FLAG f)   { flags = f; }
  void      Set_flag(CR_FLAG f)       { flags = (CR_FLAG)(flags | f); }
  BOOL      Is_flag_set(CR_FLAG f)const{ return flags & f; }
  INT32	    Coderep_id(void) const    { return _coderep_id; }
  void	    Set_coderep_id(INT32 i)   { _coderep_id = i; }
  mINT32    Offset(void) const        { return u1.nonarr.u11.offset; }
  void      Set_offset(mINT32 ofst)   { Is_True(ofst == 0
					    || Use_Load_Store_Offset
					    || !Non_leaf()
					    || !(OPERATOR_is_load(Opr()) ||
						 OPERATOR_is_store(Opr()) ||
						 OPERATOR_is_prefetch(Opr())),
						("Set_offset: offset != 0"));
                                        u1.nonarr.u11.offset = ofst; }
  INT32     Op_bit_offset(void) const { return u1.nonarr.u11.op_bit_offset_size.op_bit_offset; }
  void	    Set_op_bit_offset(INT32 bofst) { u1.nonarr.u11.op_bit_offset_size.op_bit_offset = bofst; }
  INT32     Op_bit_size(void) const   { return u1.nonarr.u11.op_bit_offset_size.op_bit_size; }
  void	    Set_op_bit_size(INT32 bsiz) { u1.nonarr.u11.op_bit_offset_size.op_bit_size = bsiz; }
  CODEREP  *Index(void) const         { return u1.nonarr.u11.index; }
  void      Set_index(CODEREP *idx)   { u1.nonarr.u11.index = idx; }
  mINT32    Num_dim(void) const       { return u2.isop.kid_count >> 1; }
  INT64     Elm_siz(void) const       { return u1.elm_siz; }
  void      Set_elm_siz(INT64 siz)    { u1.elm_siz = siz; }
  INTRINSIC Intrinsic(void) const     { return u1.nonarr.u11.intrinsic; }
  void      Set_intrinsic(INTRINSIC i) { u1.nonarr.u11.intrinsic = i; }
#if defined(TARG_SL)
  BOOL      Is_C3_Intrinsic()         { return ((u1.nonarr.u11.intrinsic >= INTRN_C3_INTRINSIC_BEGIN) &&
			                (u1.nonarr.u11.intrinsic <= INTRN_C3_INTRINSIC_END)); };
#endif
#ifdef KEY
  ST_IDX    Call_op_aux_id(void) const { return u1.nonarr.u11.call_op_aux_id; }
  void      Set_call_op_aux_id(ST_IDX i) { u1.nonarr.u11.call_op_aux_id = i; }
#endif
  TY_IDX    Ty_index(void) const      { return u1.nonarr.u11.ty_index; }
  void      Set_ty_index(TY_IDX i)    { u1.nonarr.u11.ty_index = i; }
  ST_IDX    Asm_constraint(void) const{ return u1.nonarr.u11.asm_constraint; }
  void      Set_asm_constraint(ST_IDX i){u1.nonarr.u11.asm_constraint = i; }
  UINT32    Asm_opnd_num(void) const
    {
      Is_True(Opr() == OPR_ASM_INPUT, ("Only ASM_INPUT has Asm_opnd_num()"));
      return u1.nonarr._u01.asm_operand_num;
    }
  void      Set_asm_opnd_num(UINT32 i)
    {
      Is_True(Opr() == OPR_ASM_INPUT, ("Only ASM_INPUT has Asm_opnd_num()"));
      u1.nonarr._u01.asm_operand_num = i;
    }

  IDTYPE    Aux_id(void) const    { Is_True(Kind() == CK_VAR,
					("CODEREP::Var_aux_id, illegal kind"));
    					return u2.isvar.aux_id; }
  void      Set_aux_id(IDTYPE n)  { Is_True(Kind() == CK_VAR,
					("CODEREP::Set_aux_id, illegal kind"));
    					u2.isvar.aux_id = n; }
  IDTYPE    Version(void) const       { Is_True(Kind() == CK_VAR,
					   ("CODEREP::Version, illegal kind"));
					return u2.isvar.version; }
  void      Set_version(IDTYPE v)     { Is_True(Kind() == CK_VAR,
				       ("CODEREP::Set_version, illegal kind"));
					u2.isvar.version = v; }
  STMTREP  *Get_defstmt(void) const   { if (Kind()==CK_VAR)
                                          return Defstmt();
                                        if (Kind()==CK_IVAR)
                                          return Ivar_defstmt();
                                        return NULL;
                                      }
  STMTREP  *Defstmt(void) const       { Is_True(Kind() == CK_VAR,
				       ("CODEREP::Defstmt, not CK_VAR"));
                                        return u2.isvar.defstmt; }
  void      Set_defstmt(STMTREP *s)   { Is_True(Kind() == CK_VAR,
				       ("CODEREP::Set_defstmt, illegal kind"));
					u2.isvar.defstmt = s; }
  PHI_NODE *Defphi(void) const        { Is_True(Kind() == CK_VAR,
				        ("CODEREP::Defphi, illegal kind"));
					return u2.isvar.def.phi; }
  void      Set_defphi(PHI_NODE *phi) { Is_True(Kind() == CK_VAR,
				        ("CODEREP::Set_defphi, illegal kind"));
					u2.isvar.def.phi = phi; }
  CHI_NODE *Defchi(void) const        { Is_True(Kind() == CK_VAR,
				        ("CODEREP::Defchi, illegal kind"));
					return u2.isvar.def.chi; }
  void      Set_defchi(CHI_NODE *chi) { Is_True(Kind() == CK_VAR,
				        ("CODEREP::Set_defchi, illegal kind"));
					u2.isvar.def.chi = chi; }
  BB_NODE  *Defbb(void) const;        // the BB that defines this coderep
  BOOL      Def_at_entry(void) const; // the CR is defined at entry
  TY_IDX    Lod_ty(void) const        { Is_True(Kind() == CK_VAR,
				        ("CODEREP::Lod_ty, illegal kind"));
					return u2.isvar.ty; }
  void      Set_lod_ty(TY_IDX t)      { Is_True(Kind() == CK_VAR,
				        ("CODEREP::Set_lod_ty, illegal kind"));
					u2.isvar.ty = t; }
  void      Copy_type(CODEREP *cr)    { Set_dtyp_strictly(cr->Dtyp());
                                        Set_dsctyp(cr->Dsctyp());
                                        Set_lod_ty(MTYPE_To_TY(cr->Dtyp()));
					Assign_sign_extd(cr->Is_sign_extd());
                                      }

  BOOL      Is_var_volatile(void) const
		{ return Is_volatile(); }
  void      Set_var_volatile(void)
		{ Set_is_volatile(); }
  void      Reset_var_volatile(void)
		{ Reset_is_volatile(); }
  BOOL      Is_ivar_volatile(void) const;

  void Reset_isvar_flags(void)
    { u2.isvar._isvar_flags = 0; }

  void Set_isvar_flags(UINT32 flags) { u2.isvar._isvar_flags = flags; }

  UINT32 Isvar_flags(void) const     { return u2.isvar._isvar_flags; }

  void Set_safe_to_renumber_preg(void)
    { u2.isvar._isvar_flags |= ISVAR_SAFE_TO_RENUMBER_PREG; }

  void Reset_safe_to_renumber_preg(void)
  { u2.isvar._isvar_flags &= ~ISVAR_SAFE_TO_RENUMBER_PREG; }

  BOOL Safe_to_renumber_preg(void) const
  { return u2.isvar._isvar_flags & ISVAR_SAFE_TO_RENUMBER_PREG; }

  void Set_bit_field_valid() {
    u2.isvar._isvar_flags |= ISVAR_BIT_FIELD_VALID;
  }

  void Reset_bit_field_valid() {
    u2.isvar._isvar_flags &= ~ISVAR_BIT_FIELD_VALID;
  }

  BOOL Bit_field_valid() const {
    return u2.isvar._isvar_flags & ISVAR_BIT_FIELD_VALID;
  }

#ifdef KEY
  void Set_promote_to_reg_size() {
    u2.isvar._isvar_flags |= ISVAR_PROMOTE_TO_REG_SIZE;
  }

  void Reset_promote_to_reg_size() {
    u2.isvar._isvar_flags &= ~ISVAR_PROMOTE_TO_REG_SIZE;
  }

  BOOL Promote_to_reg_size() const {
    return u2.isvar._isvar_flags & ISVAR_PROMOTE_TO_REG_SIZE;
  }

  void Set_mp_shared() {
    u2.isvar._isvar_flags |= ISVAR_MP_SHARED;
  }

  BOOL Mp_shared() const {
    return u2.isvar._isvar_flags & ISVAR_MP_SHARED;
  }
#endif
#if defined(TARG_SL)
  void Set_RVI_Candidate() {
    u2.isconst.isconst_val._isconst_flags |= ISCONST_RVI_CANDIDATE; 
  }
  BOOL RVI_Candidate() {
    return u2.isconst.isconst_val._isconst_flags & ISCONST_RVI_CANDIDATE; 
  }
  void Reset_RVI_Candidate() {
    u2.isconst.isconst_val._isconst_flags &= ~ISCONST_RVI_CANDIDATE; 
  }

#endif

  void Reset_field_id(void) 	     { u2.isvar.fieldid = 0; }

  void Set_field_id(UINT field_id)   { u2.isvar.fieldid = field_id; }

  UINT Field_id(void) const	     { return u2.isvar.fieldid; }

  // all bit offset and size access functions below must match the
  // corresponding definition in wn_core.h.  We rely on the exactly same
  // layout of field_id and bit ofst/size so that we can copy this field
  // between a CODEREP and a WN.  This used to be a nicely aligned union of 
  // the form:
  // union {
  //   mUINT16 field_id;
  //   struct {
  //     mUINT8 bit_ofst;
  //     mUINT8 bit_size;
  //   } bits;
  // } field_id;
  //
  // But it turns out to be too error-prone because we need to do a lot of
  // bit shuffling everytime we read it from a WN to CODEREP, or vice versa.
  void Set_bit_offset_size (UINT8 ofst, UINT8 size) {
    u2.isvar.fieldid = (ofst << 7) + size;
    Set_bit_field_valid ();
  }

  UINT Bit_offset(void) const {
    Is_True(Bit_field_valid(), ("CODEREP::Bit_offset, bit field not valid"));
    return u2.isvar.fieldid >> 7;
  }

  UINT Bit_size(void) const {
    Is_True(Bit_field_valid(), ("CODEREP::Bit_offset, bit field not valid"));
    return u2.isvar.fieldid & 0x7f;
  }

  void Reset_i_field_id(void) 	     { u2.isivar.ifieldid = 0; }

  void Set_i_field_id(UINT field_id)   { u2.isivar.ifieldid = field_id; }

  UINT I_field_id(void) const	     { return u2.isivar.ifieldid; }

  void Set_i_bit_offset_size (UINT8 ofst, UINT8 size) {
    u2.isivar.ifieldid = (ofst << 7) + size;
  }

  UINT I_bit_offset(void) const	     { return u2.isivar.ifieldid >> 7; }

  UINT I_bit_size(void) const	     { return u2.isivar.ifieldid & 0x7f; }

  STMTREP  *Ivar_defstmt(void) const  { Is_True(Kind() == CK_IVAR,
				      ("CODEREP::Ivar_defstmt, illegal kind"));
					return u2.isivar.defstmt; }
  void      Set_ivar_defstmt(STMTREP *s) { Is_True(Kind() == CK_IVAR,
				  ("CODEREP::Set_ivar_defstmt, illegal kind"));
					   u2.isivar.defstmt = s; }
  IDTYPE    Lda_aux_id(void) const    { Is_True(Kind() == CK_LDA,
					("CODEREP::Lda_aux_id, illegal kind"));
    					return u2.islda.aux_id; }
  void      Set_lda_aux_id(IDTYPE n)  { Is_True(Kind() == CK_LDA,
				    ("CODEREP::Set_lda_aux_id, illegal kind"));
    					u2.islda.aux_id = n; }
  ST       *Lda_base_st(void) const    { Is_True(Kind() == CK_LDA,
				       ("CODEREP::Lda_base_st, illegal kind"));
    					return u2.islda.base_st; }
  void      Set_lda_base_st(ST *s)  { Is_True(Kind() == CK_LDA,
				   ("CODEREP::Set_lda_base_st, illegal kind"));
    					u2.islda.base_st = s; }
  TY_IDX    Lda_ty(void) const        { Is_True(Kind() == CK_LDA,
					("CODEREP::Lda_ty, illegal kind"));
					return u2.islda.ty; }
  void      Set_lda_ty(TY_IDX t)      { Is_True(Kind() == CK_LDA,
					("CODEREP::Set_lda_ty, illegal kind"));
					u2.islda.ty = t; }
  void Set_afield_id(UINT field_id)   { Is_True(Kind() == CK_LDA,
					("CODEREP::Set_afield_id, illegal kind"));
    					u2.islda.afieldid = field_id; }
  UINT Afield_id(void) const	      { Is_True(Kind() == CK_LDA,
					("CODEREP::Afield_id, illegal kind"));
    					return u2.islda.afieldid; }
  void      Set_const_val(INT64 v)    { Is_True(Kind() == CK_CONST,
				     ("CODEREP::Set_const_val, illegal kind"));
#if defined(TARG_SL)
					u2.isconst.isconst_val.const_val = v; }
#else
					u2.isconst.const_val = v; }
#endif
  INT64     Const_val(void) const     { Is_True(Kind() == CK_CONST,
					 ("CODEREP::Const_val, illegal kind"));
					Is_True(!MTYPE_float(_dtyp),
					 ("CODEREP::Const_val, illegal type"));
#if defined(TARG_SL)
					return u2.isconst.isconst_val.const_val; }
#else
					return u2.isconst.const_val; }
#endif
  // return the floating point value of a CK_RCONST
  // the Is_True checks are in Const_ftcon because that is the
  // 	routine that is always called
  double    Const_fval(const CODEMAP *htable) const
                                      { return Targ_To_Host_Float(
					  Const_ftcon2(htable)); }
  TCON_IDX  Const_ftcon(const CODEMAP *) const
                                      { Is_True(Kind() == CK_RCONST,
					("CODEREP::Const_fval, illegal kind"));
					Is_True(MTYPE_float(Dtyp()),
					("CODEREP::Const_fval, illegal type"));
					return ST_tcon(Const_id()); }
  TCON      Const_ftcon2(const CODEMAP *) const // will go away soon
                                      { Is_True(Kind() == CK_RCONST,
					("CODEREP::Const_fval, illegal kind"));
					Is_True(MTYPE_float(Dtyp()),
					("CODEREP::Const_fval, illegal type"));
					return STC_val(Const_id()); }
  ST        *Const_id(void) const     { Is_True(Kind() == CK_RCONST,
					  ("CODEREP::Const_id, illegal kind"));
					return u2.isconst.const_id; }
  void      Set_const_id(ST *v)       { Is_True(Kind() == CK_RCONST,
				      ("CODEREP::Set_const_id, illegal kind"));
					u2.isconst.const_id = v; }
  OPCODE    Op(void) const            { Is_True(Non_leaf(),
				                ("CODEREP::Op, illegal kind"));
					return OPCODE_make_op(Opr(), Dtyp(), Dsctyp()); }
  OPERATOR  Opr(void) const           { Is_True(Non_leaf(),
				                ("CODEREP::Opr, illegal kind"));
#ifndef __GNUC__
					return Kind() == CK_OP ? u2.isop._opr :
							u2.isivar._opr; }
#else
					Is_True(u2.isop._opr == u2.isivar._opr,
				("CODEREP::Opr, GNUC bug workaround violated"));
					return u2.isop._opr; }
#endif
  void      Set_opr(OPERATOR c)       { Is_True(Non_leaf(),
				            ("CODEREP::Set_opr, illegal kind"));
					(Kind() == CK_OP) ? u2.isop._opr = c :
						u2.isivar._opr = c; }
  mINT16    Kid_count(void) const     { Is_True(Kind() == CK_OP,
				        ("CODEREP::Kid_count, illegal kind %s",
					  Print_kind()));
					return u2.isop.kid_count; }
  void      Set_kid_count(mINT16 c)   { Is_True(Kind() == CK_OP,
				     ("CODEREP::Set_kid_count, illegal kind"));
					u2.isop.kid_count = c; }
  PROPAGATABILITY Propagatability(void) const { Is_True(Kind() == CK_OP,
					 ("CODEREP::Propagatability: illegal kind %s", Print_kind()));
					 return u2.isop.propagatability; }
  void	    Set_propagatability(PROPAGATABILITY p) { Is_True(Kind() == CK_OP,
					 ("CODEREP::Propagatability: illegal kind %s", Print_kind()));
					 u2.isop.propagatability = p; }
  ISOP_FLAG Isop_flags(void) const     { return u2.isop.isop_flags; }
  void      Reset_isop_flags(void)     { u2.isop.isop_flags = ISOP_EMPTY; }
  void      Set_isop_flag(ISOP_FLAG f){ u2.isop.isop_flags = (ISOP_FLAG)(u2.isop.isop_flags | f);}
  void      Reset_isop_flag(ISOP_FLAG f)   { u2.isop.isop_flags = (ISOP_FLAG)(u2.isop.isop_flags&~f); }
  BOOL      Is_isop_flag_set(ISOP_FLAG f)const{ return u2.isop.isop_flags & f; }

  void      Set_omitted(void)
    {
      FmtAssert(Kind() == CK_OP,
		("CODEREP::Set_omitted: Omission from worklist "
		 "implemented only for CK_OP"));
      Set_isop_flag(ISOP_SSAPRE_OMITTED);
    }

  BOOL      Omitted(void) const
    {
      if (Kind() == CK_OP) {
	return Is_isop_flag_set(ISOP_SSAPRE_OMITTED);
      }
      else {
	return FALSE;
      }
    }

  UINT8	    Max_depth(void) const     { Is_True(Kind() == CK_OP,
					("CODEREP::Max_depth: illegal kind"));
					return u2.isop.max_depth; }
  void	    Set_max_depth(UINT8 d)    { Is_True(Kind() == CK_OP,
					("CODEREP::Max_depth: illegal kind"));
					u2.isop.max_depth = d; }
  CODEREP  **Opnd_ptr(void) const      { Is_True(Kind() == CK_OP,
				        ("CODEREP::Opnd_ptr, illegal kind"));
					return (CODEREP **) u2.isop.kids; }
  CODEREP  *Opnd(INT16 i) const       { Is_True(Kind() == CK_OP,
				        ("CODEREP::Opnd, illegal kind %s",
					 Print_kind()));
					return u2.isop.kids[i]; }
  CODEREP  *Get_opnd(mINT16 i) const  { Is_True(Kind() == CK_OP,
				        ("CODEREP::Get_opnd, illegal kind"));
					return (i < u2.isop.kid_count) ?
                                         u2.isop.kids[i] : NULL; }
  CODEREP  *Skip_opnd(INT16 i) const;

  BOOL      Set_opnd(mINT16 i,
                     CODEREP *k)      { Is_True(Kind() == CK_OP,
				        ("CODEREP::Set_opnd, illegal kind"));
                                        if (k && k->Kind() == CK_VAR)
                                          Is_True(k->Dsctyp() != MTYPE_UNKNOWN,
                                          ("CODEREP::Set_opnd: UNKNOWN DSCTYP"));
					if (i < u2.isop.kid_count) {
                                          u2.isop.kids[i] = k;
					  return TRUE;
                                        } else
					  return FALSE;
                                      }
  CODEREP  *Ilod_base(void) const     { Is_True(Kind() == CK_IVAR,
				        ("CODEREP::Ilod_base, illegal kind"));
					return u2.isivar.base[0]; }
  void      Set_ilod_base(CODEREP *cr){ Is_True(Kind() == CK_IVAR,
				     ("CODEREP::Set_ilod_base, illegal kind"));
                                        if (cr && cr->Kind() == CK_VAR)
                                          Is_True(cr->Dsctyp() != MTYPE_UNKNOWN,
                                          ("CODEREP::Set_ilod_base: UNKNOWN DSCTYP"));
					u2.isivar.base[0] = cr; }

  OCC_TAB_ENTRY *Ivar_occ(void) const   { Is_True(Kind() == CK_IVAR,
						  ("CODEREP::Ivar_aux_id, illegal kind"));
					  return (OCC_TAB_ENTRY *)u2.isivar.base[1]; }
  void      Set_ivar_occ(OCC_TAB_ENTRY *occ){ Is_True(Kind() == CK_IVAR,
						      ("CODEREP::Set_ivar_aux_id, illegal kind"));
					      u2.isivar.base[1] = (CODEREP*)occ; }
  CODEREP  *Mload_size(void) const     { Is_True(Kind() == CK_IVAR,
				        ("CODEREP::Mload_size, illegal kind"));
					 return u2.isivar.base[4]; }
  void      Set_mload_size(CODEREP *cr){ Is_True(Kind() == CK_IVAR,
				    ("CODEREP::Set_mload_size, illegal kind"));
					 u2.isivar.base[4] = cr; }
  TY_IDX    Ilod_ty(void) const       { Is_True(Kind() == CK_IVAR,
				        ("CODEREP::Ilod_ty, illegal kind"));
					return (TY_IDX)(INTPTR)u2.isivar.base[2]; }
  void      Set_ilod_ty(TY_IDX ty)    { Is_True(Kind() == CK_IVAR,
				       ("CODEREP::Set_ilod_ty, illegal kind"));
					u2.isivar.base[2] = (CODEREP*)(INTPTR)ty; }
  CODEREP  *Mstore_size(void) const   { Is_True(Kind() == CK_IVAR,
				       ("CODEREP::Mstore_size, illegal kind"));
					return u2.isivar.base[4]; }
  void      Set_mstore_size(CODEREP *cr){ Is_True(Kind() == CK_IVAR,
						  ("CODEREP::Set_mstore_size, illegal kind"));
					  u2.isivar.base[4] = cr; }
  CODEREP  *Istr_base(void) const     { Is_True(Kind() == CK_IVAR,
				        ("CODEREP::Istr_base, illegal kind"));
					return u2.isivar.base[3]; }
  void      Set_istr_base(CODEREP *cr){ Is_True(Kind() == CK_IVAR,
				     ("CODEREP::Set_istr_base, illegal kind"));
                                        if (cr && cr->Kind() == CK_VAR)
                                        Is_True(cr->Dsctyp() != MTYPE_UNKNOWN,
                                        ("CODEREP::Set_ilod_base: UNKNOWN DSCTYP"));
					u2.isivar.base[3] = cr; }
  TY_IDX    Ilod_base_ty(void) const  { Is_True(Kind() == CK_IVAR,
						("CODEREP::Ilod_base_ty, illegal kind"));
					return (TY_IDX)(INTPTR) u2.isivar.base[4]; }
  void      Set_ilod_base_ty(TY_IDX ty){ Is_True(Kind() == CK_IVAR,
						 ("CODEREP::Set_ilod_base_ty, illegal kind"));
					 u2.isivar.base[4] = (CODEREP*)(INTPTR)ty; }

  void      Set_ivar_mu_node(MU_NODE *mu){ Is_True(Kind() == CK_IVAR,
						   ("CODEREP::Set_ivar_mu_node, illegal kind"));
					   u2.isivar.mu_node = mu; }
  MU_NODE  *Ivar_mu_node(void) const     { Is_True(Kind() == CK_IVAR,
						   ("CODEREP::Ivar_mu_node, illegal kind"));
                                           return u2.isivar.mu_node; }
  CODEREP  *Prepend(CODEREP *c)	      { c->Set_Next(this); return c; }

  // Find the vsym corr to this ivar from either the mu-node or chi-node.
  CODEREP  *Get_ivar_vsym(void) const;

  POINTS_TO *Points_to(OPT_STAB *opt_stab) const;
  
  // member functions dealing with the bit position
  IDX_32    Bitpos(void) const        { return u1.nonarr._u01.bitpos; }
  void      Set_Bitpos(IDX_32 bp)     { u1.nonarr._u01.bitpos = bp; }
  IDTYPE    E_num(void) const         { return u1.nonarr._u01.e_num; }
  void      Set_e_num(IDTYPE num)     { u1.nonarr._u01.e_num = num; }
  BOOL	    Compare_bitpos(const CODEREP *) const; // the bitpos must be legal
  BOOL      Same_bitpos(const CODEREP *) const;    // similar to Compare_bitpos, but can handle illegal BP
  char     *Print_bit(void) const;
  IDTYPE    Scalar_aux_id(void) const { return u1.nonarr._u01.scalar_aux_id; }
  void      Set_scalar_aux_id(IDTYPE i) { u1.nonarr._u01.scalar_aux_id = i; }
  OCC_TAB_ENTRY *Scalar_ivar_occ(void) const { return u1.nonarr._u01.scalar_ivar_occ; }
  void      Set_scalar_ivar_occ(OCC_TAB_ENTRY *o) { u1.nonarr._u01.scalar_ivar_occ = o; }

  CODEREP  *Var_type_conversion(CODEMAP *htable, MTYPE to_dtyp,
				MTYPE to_dsctyp, TY_IDX ty, UINT field_id);

  STMTREP  *Create_cpstmt(CODEREP *a, // create copy from 'this' to 'a'
                          MEM_POOL*p);// Return the created stmtrep.
  STMTREP  *Create_istr_stmt(CODEREP *a, // create copy from 'this' to 'a'
                          MEM_POOL*p);// Return the created stmtrep.
                                      // Generate OPR_ISTORE instead.
  BOOL      Is_const(void) const      { return inCODEKIND(Kind(),
                                                          CK_CONST|CK_RCONST);}

  BOOL      Non_leaf(void) const      { return inCODEKIND(Kind(),
                                                          CK_OP|CK_IVAR); }

  // functions used by SSA PRE
  BOOL      Ivar_has_e_num(void) const { Is_True(Kind() == CK_IVAR,
						 ("CODEREP::Ivar_has_e_num: illegal kind."));
                                         return (Dtyp() != MTYPE_M &&
                                         	(OPERATOR_is_scalar_iload(Opr()) ||
						 OPERATOR_is_scalar_istore(Opr())) );
                                       }
  BOOL      Exp_has_e_num(void) const; 
  BOOL      Is_integral_load_store(void) const 
    { return ((Kind() == CK_IVAR && (OPERATOR_is_scalar_iload (Opr()) ||
				     OPERATOR_is_scalar_istore (Opr()))
	       || Kind() == CK_VAR)
	      && MTYPE_is_integral(Dtyp())); 
    }

  // check if this tree references the variable
  BOOL      References_var( AUX_ID var );

  // member functions dealing with strength reduction and LFTR.
  CODEREP  *Find_cr(const CODEREP *cr); // return the 
					// coderep has the same bitpos as cr

  // similar to Find_cr, but create a new one if cannot find one for the bb.
  CODEREP  *Create_exp_in_bb(BB_NODE *, CODEMAP *);

  BOOL      Is_loop_invar(BB_NODE *); // Is 'this' cr a loop
                                      // invariant var. Return True if
                                      // 'this' is loop invariant

  // Tell whether it is safe to speculatively move an expression tree.
  BOOL      Can_be_speculated(OPT_STAB *) const;

  // reset a visited bit in all the CK_OP nodes in a CR tree
  // built on assumption that if a bit is not set, it is not set for all
  // nodes in the subtree.
  void	    Reset_isop_visited(ISOP_FLAG);

  // Three local attributes: antloc, avloc, cseloc regarding this coderep
  BOOL      Antloc(BB_NODE *);      // wrapper for Antloc_rec (private)
  BOOL      Avloc (BB_NODE *);      // this cr is avloc in bb
  BOOL      Cseloc(BB_NODE *);      // this cr is cseloc in bb
  BOOL      Is_lcse(void) const       { return is_lcse; }
  BOOL      Is_saved(void) const      { return is_saved; }
  BOOL	    On_workstack(void) const  { return is_lcse; } // IVE uses lcse bit
  IDTYPE    Emit_bb(void) const       { return _bb_id; }
  IDTYPE    Temp_id(void) const       { Is_True(Kind()==CK_OP,("CODEREP::Temp_id: not a CK_OP"));
					return u2.isop._temp_id; }
  void      Set_is_lcse(void)         { is_lcse = 1; }
  void      Set_is_saved(void)        { is_saved = 1; }
  void      Set_on_workstack(void)    { is_lcse = 1; } // IVE uses lcse bit
  void      Reset_is_lcse(void)       { is_lcse = 0; }
  void      Reset_is_saved(void)      { is_saved = 0; }
  void      Reset_on_workstack(void)  { is_lcse = 0; } // IVE uses lcse bit
  void      Assign_is_lcse(BOOL b)    { is_lcse = b; }
  void      Assign_is_saved(BOOL b)   { is_saved = b; }
  void      Set_emit_bb(IDTYPE bb)    { _bb_id = bb; }
  void      Set_temp_id(IDTYPE id)    { Is_True(Kind()==CK_OP,("CODEREP::Temp_id: not a CK_OP"));
					u2.isop._temp_id = id; }
  // for lcse in main emitter, see opt_main_emit.cxx for bodies

  // whether the default type of a load/iload is sign extended
  // a load/iload CR gets its default type when it is first seen
  BOOL      Is_sign_extd(void) const  { return _is_sign_extd; }
  void      Set_sign_extd(void)       { _is_sign_extd = 1; }
  void      Reset_sign_extd(void)     { _is_sign_extd = 0; }
  void      Assign_sign_extd(BOOL b)  { _is_sign_extd = b; }
  void      Set_sign_extension_flag(void);

  WN	   *Insert_expr_wn(MAIN_EMITTER *, BB_NODE *, BOOL);
  WN       *Store_to_preg(WN *, MAIN_EMITTER *);
  WN       *Load_from_preg(MAIN_EMITTER *, BOOL gen_cvt = TRUE);

  BOOL 	    Var_used(CODEREP *);  	// check usage of variable in expr
					     // (used in main emit)
  BOOL      Reference_version(CODEREP *cr);
  CODEREP  *Replace_version(CODEMAP *htable, CODEREP *cr, CODEREP *newcr);
  CODEREP  *Replace_CR(CODEMAP *htable, CODEREP *, CODEREP *);

  INT32     Offset_nbits(void) const  { return sizeof(u1.nonarr.u11.offset) * 8; }

  BOOL      Propagatable_into_loop(const BB_LOOP *) const;

  BOOL      Propagatable_along_path(const BB_NODE *, const BB_NODE *) const;

  BOOL Propagatable_for_ivr(OPT_STAB *sym) const;

  BOOL      Is_const_expr(void);   // composed of LDA, CONST, RCONST

  BOOL 	    Is_non_volatile_terminal(OPT_STAB *) const;  // used by new PRE

  BOOL      Divisable(const CODEREP *, OPT_STAB *) const;   // this coderep is divisable by a coderep

  ADDRESSABILITY Check_if_result_is_address(OPT_STAB *opt_stab) const;

  BOOL      Is_rvi_const_candidate(const CODEREP *, INT, const OPT_STAB *) const;
  BOOL      Is_rvi_lda_candidate(const CODEREP *, INT, const OPT_STAB *) const;

  void      Verify_CODEMAP(CODEMAP *, OPT_STAB *, BOOL);

  WN       *Rvi_home_wn( OPT_STAB *opt_stab ) const;
  BOOL      Contains_only_constants(void) const;
  BOOL	    Has_volatile_content(void) const;

  /* Functions [G,S]et_ISOP_mtype_b_cache and [G,S]et_ISOP_def_before_use_cache
   * get and set the node cache in the ISOP structure. This cache is used to two
   * ways. In Do_mtype_b_cr it is used to hold the new CR node created for other
   * parents on differing paths. In Def_before_use it is used to cache the last
   * BB_NODE that reached "this" node thus limiting redundant calls.
   */

  CODEREP*  Set_ISOP_mtype_b_cache (const CODEREP* CR)
  {
      u2.isop.node_cache = (void *)CR;
      return  (CODEREP*) u2.isop.node_cache;
  }

  CODEREP*  Get_ISOP_mtype_b_cache () const 
  { return (CODEREP*) u2.isop.node_cache; }

  BB_NODE*  Set_ISOP_def_before_use_cache (const BB_NODE* CR)
  {
      u2.isop.node_cache =(void*)CR;
      return   (BB_NODE*)u2.isop.node_cache;
  }

  BB_NODE*  Get_ISOP_def_before_use_cache () const
  { return (BB_NODE*) u2.isop.node_cache; }

  TY_IDX lod_addr_ty(void);
  TY_IDX object_ty(void);
}; // end of class CODEREP

// Functions to tell how much extra space should be allocated with a
// coderep of a particular size and number of kids. This is not a
// member function of class CODEREP because it is required in
// situations where the CODEREP hasn't been constructed [a member
// function is still possible, as long as it doesn't dereference
// "this", but that's an ugly approach]. These functions essentially
// match CODEREP::Extra_ptrs_used() and CODEREP::Extra_space_used() in
// terms of what they do.

inline UINT32
Extra_CODEREP_ptrs(const CODEKIND kind, const UINT32 items)
{
  switch (kind) {
  case CK_OP:
    return MAX(0, items - 1);
  case CK_IVAR:
    return IVAR_EXTRA_NODE_CNT;
  default:
    return 0;
  }
}

inline size_t
Extra_CODEREP_space(const CODEKIND kind, const UINT32 items)
{
  return sizeof(CODEREP *) * Extra_CODEREP_ptrs(kind, items);
}

class CODEREP_LIST : public SLIST_NODE {
private:
  CODEREP *node;
        CODEREP_LIST(const CODEREP_LIST&);
        CODEREP_LIST& operator = (const CODEREP_LIST&);
public:
        CODEREP_LIST(void)              {}
        CODEREP_LIST(CODEREP *nd)       { node = nd; }
       ~CODEREP_LIST(void)              {}

  DECLARE_SLIST_NODE_CLASS( CODEREP_LIST )

  void          Init(CODEREP *nd)       { node = nd; }
  CODEREP_LIST *Prepend (CODEREP *cr,   // insert in front, return head
                         MEM_POOL *pool);
  BOOL          Contains(CODEREP *cr) ; // check membership in list

  // member access functions
  CODEREP      *Node(void) const        { return node; }
}; // end of class CODEREP_LIST

class CODEREP_LIST_CONTAINER : public SLIST {
private:
  CODEREP_LIST_CONTAINER(const CODEREP_LIST_CONTAINER&);
  CODEREP_LIST_CONTAINER& operator = (const CODEREP_LIST_CONTAINER&);

  DECLARE_SLIST_CLASS( CODEREP_LIST_CONTAINER, CODEREP_LIST )
public:  
  ~CODEREP_LIST_CONTAINER(void)         {};
  BOOL Contains(CODEREP *cr) ;          // check membership in list
  void Prepend (CODEREP *cr,            // prepend cr to the head
                MEM_POOL *pool);
};

class CODEREP_LIST_ITER : public SLIST_ITER {
  DECLARE_SLIST_ITER_CLASS( CODEREP_LIST_ITER, CODEREP_LIST, CODEREP_LIST_CONTAINER )
public:
  void     Init(void)       { }
  CODEREP *First_elem(void) { return (First()) ? Cur()->Node():NULL; }
  CODEREP *Next_elem(void)  { return (Next())  ? Cur()->Node():NULL; }
};

class CR_PAIR_LIST : public SLIST_NODE {
private:
  CODEREP *_nd1;
  CODEREP *_nd2;
        CR_PAIR_LIST(const CR_PAIR_LIST&);
        CR_PAIR_LIST& operator = (const CR_PAIR_LIST&);
public:
        CR_PAIR_LIST(void)              {}
        CR_PAIR_LIST(CODEREP *nd1,
                     CODEREP *nd2)      { Init(nd1, nd2); }
       ~CR_PAIR_LIST(void)              {}

  DECLARE_SLIST_NODE_CLASS( CR_PAIR_LIST )

  void          Init(CODEREP *nd1,
                     CODEREP *nd2)      { if (nd1 > nd2) { _nd1 = nd1; _nd2 = nd2; }
                                          else { _nd1 = nd2; _nd2 = nd1; }
                                        }
  CR_PAIR_LIST *Prepend (CODEREP *nd1,  // insert in front, return head
                         CODEREP *nd2,
                         MEM_POOL *pool);
  BOOL          Contains(CODEREP *nd1,
                         CODEREP *nd2); // check membership in list

  // member access functions
  CODEREP      *Nd1(void) const         { return _nd1; }
  CODEREP      *Nd2(void) const         { return _nd2; }
}; // end of class CR_PAIR_LIST

class CR_PAIR_LIST_CONTAINER : public SLIST {
private:
  CR_PAIR_LIST_CONTAINER(const CR_PAIR_LIST_CONTAINER&);
  CR_PAIR_LIST_CONTAINER& operator = (const CR_PAIR_LIST_CONTAINER&);

  DECLARE_SLIST_CLASS( CR_PAIR_LIST_CONTAINER, CR_PAIR_LIST )
public:  
  ~CR_PAIR_LIST_CONTAINER(void)                {};
  BOOL Contains(CODEREP *nd1,
                CODEREP *nd2);          // check membership in list
  void Prepend (CODEREP *nd1,           // prepend cr to the head
                CODEREP *nd2,
                MEM_POOL *pool);
};

class CR_PAIR_LIST_ITER : public SLIST_ITER {
  DECLARE_SLIST_ITER_CLASS( CR_PAIR_LIST_ITER, CR_PAIR_LIST, CR_PAIR_LIST_CONTAINER)
};


enum { LARGE_KIDS_CNT = 100 };
// TODO: we need to make LARGE_KIDS_CNT dynamically set to
// the largest possible kid count that has occurred in the current PU,
// determined by an earlier pass over the PU

class LARGE_CODEREP {
private:
  CODEREP  body;
  CODEREP *kids[LARGE_KIDS_CNT];
  LARGE_CODEREP(const LARGE_CODEREP&);
  LARGE_CODEREP& operator = (const LARGE_CODEREP&);

public:
  LARGE_CODEREP(void) {}
  CODEREP *Body(void) { return &body; }
};


// A list of coderep hanging off the hash table.
// NOTE: No prepend, since the len is used by the valno.depth
//       No remove,  for the same reason
class CODEREP_CONTAINER : public SLIST {
private:
  mUINT32   len;                           // current length

  CODEREP_CONTAINER(const CODEREP_CONTAINER&);
  CODEREP_CONTAINER& operator = (const CODEREP_CONTAINER&);

  DECLARE_SLIST_CLASS( CODEREP_CONTAINER, CODEREP )
public:
  ~CODEREP_CONTAINER(void)   {}            // memory deallocation by
                                           // pop mempool

  void     Init(CODEREP *h)                { SLIST::Set_Head(h); len = 0; }
  void     Clear(void)                     { SLIST::Clear(); len = 0; }
  void     Add_it (CODEREP *cr,
                       const mUINT16 idx,
                       CODEMAP *htable);   // add cr to the container

  void     Delete_it  (CODEREP *cr,
                       const mUINT16 idx,
                       CODEMAP *htable);   // delete coderep from bucket

  BOOL     Contains(CODEREP *cr);          // test coderep_container
                                           // contains cr
  CODEREP *Find_cr(CODEREP *cr, INT32 mu_vsym_depth, // return the coderep if
		   OPT_STAB *sym);	   // it exists in MAP, return NULL
                                           // otherwise.
};

typedef CODEREP_CONTAINER *CODEREP_CP;

class CODEREP_ITER : public SLIST_ITER {
  DECLARE_SLIST_ITER_CLASS( CODEREP_ITER, CODEREP, CODEREP_CONTAINER )
};

class CODEMAP {
friend class CODEMAP_ITER;
private:
  SSA        *_ssa;
  MEM_POOL   *mem_pool;
  OPT_STAB   *sym;
  CFG        *_cfg;
  mUINT32     size;
  OPT_PHASE   _phase;			// the current compilation phase
  CODEREP   **hash_vec;
  WN_MAP      _prefetch_map;            // point from pretetch WN* to CR*
  INT32	      _coderep_id_cnt;		// for assigning unique ID's to CR nodes
  IDTYPE      _pre_temp_id;             // for PRE step1/6 book keeping
  INT32	      _num_iloadfolds;		// for statistics only
  INT32	      _num_istorefolds;		// for statistics only
  INT32	      _num_inputprops;		// for statistics only (icopy phase)
  INT32	      _num_mainprops;		// for statistics only (main prop phase)
  INT32	      _num_shrinks;		// for statistics only (shrink phase)
  BOOL        _tracing;
  BOOL                        _phi_hash_valid;
  ID_MAP<PHI_NODE *, PHI_KEY> _phi_id_map;

  CODEMAP(void);
  CODEMAP(const CODEMAP&);
  CODEMAP& operator = (const CODEMAP&);

  // hash functions
  IDX_32 Hash_lda(ST *st, IDTYPE ofst)	  { return (ST_st_idx(st)+ofst)%size; }
  // The following is temporary until we figure out how to remove
  // duplicates. We should hash on the value of the constant
  // represented rather than just its IDX so we can find duplicates.
  IDX_32 Hash_rconst(ST *st, IDTYPE ofst) { return (ST_st_idx(st)+ofst)%size; }
  IDX_32 Hash_const(UINT64 val)  	  { return (IDX_32)(val % size); }
  IDX_32 Hash_op_and_canon(CODEREP *cr, BOOL canonicalize = TRUE);
  IDX_32 Hash_ivar(CODEREP *cr, CODEREP *base);
  IDX_32 Hash(CODEREP *cr);		// call one of the above
  // search bucket, create CR if not found
  CODEREP *Find_or_append_CR(IDX_32 hash_idx, CODEREP *cr, INT32 mu_vsym_depth);
  // Create CR and append to bucket
  CODEREP *Append_CR(IDX_32 hash_idx, CODEREP *cr);
  // search bucket, error if not found and !ok_to_fail
  CODEREP *Find_CR(IDX_32 hash_idx, CODEREP *cr, INT32 mu_vsym_depth=0, BOOL ok_to_fail=FALSE);


  void        Alloc_hash_vec(void);       // called by the constructor
  void        Free_hash_vec(void);        // called by the destructor
  CODEREP    *Hash_bucket(IDX_32 idx)const// get the hash bucket for
                { return hash_vec[idx]; } // hash idx, idx should never
                                          // get out of bound

  // The following Canon_xxx functions are used to create canonicalized
  // expressions.
  BOOL        Canon_add_sub(WN   *wn,     // enter canonicalized add exp
                        OPT_STAB *opt_stab,
                        STMTREP  *stmt,
                        CANON_CR *ccr,
                        CODEREP  *cr,
			COPYPROP *copyprop);
  BOOL        Canon_neg(WN       *wn,     // enter canonicalized neg exp
                        OPT_STAB *opt_stab,
                        STMTREP  *stmt,
                        CANON_CR *ccr,
                        CODEREP  *cr,
			COPYPROP *copyprop);
  BOOL        Canon_mpy(WN       *wn,     // enter canonicalized mpy exp
                        OPT_STAB *opt_stab,
                        STMTREP  *stmt,
                        CANON_CR *ccr,
                        CODEREP  *cr,
			COPYPROP *copyprop);
  BOOL        Canon_cvt(WN       *wn,     // enter canonicalized cvt exp
                        OPT_STAB *opt_stab,
                        STMTREP  *stmt,
                        CANON_CR *ccr,
                        CODEREP  *cr,
			COPYPROP *copyprop);
  CODEREP    *Separate_iv_invar(CODEREP *,
                                BB_NODE *);// for comparisons

public:
  CODEMAP(mUINT32 hash_size, CFG *cfg, OPT_STAB *asym, 
	  SSA *ssa, IDX_32 phi_hash_size, OPT_PHASE phase, MEM_POOL *pool);
  ~CODEMAP(void);

  OPT_PHASE Phase(void) const		{ return _phase; }

  CFG        *Cfg(void) const { return _cfg; }

  BOOL      Tracing(void) const         { return _tracing; }

  // hash functions that search htable and create new node if not found
  CODEREP    *Hash_Lda(CODEREP *cr)
    { Is_True(cr->Kind() == CK_LDA,("CODEMAP::Hash_Lda, wrong kind"));
#ifdef TARG_SL
      IDX_32 hash_idx = Hash_lda(cr->Lda_base_st(),(IDTYPE)(cr->Offset() + cr->Is_flag_set(CF_INTERNAL_MEM_OFFSET)));
#else 
      IDX_32 hash_idx = Hash_lda(cr->Lda_base_st(),(IDTYPE)cr->Offset());
#endif
      return Find_or_append_CR(hash_idx,cr,0);
    }
  CODEREP    *Hash_Const(CODEREP *cr)
    { Is_True(cr->Kind() == CK_CONST,("CODEMAP::Hash_Const, wrong kind"));
      IDX_32 hash_idx = Hash_const(cr->Const_val());
      return Find_or_append_CR(hash_idx,cr,0);
    }
  CODEREP    *Hash_Rconst(CODEREP *cr)
    { Is_True(cr->Kind() == CK_RCONST,("CODEMAP::Hash_Rconst, wrong kind"));
      IDX_32 hash_idx = Hash_rconst(cr->Const_id(), (IDTYPE)0);
      return Find_or_append_CR(hash_idx,cr,0);
    }
  CODEREP    *Hash_Ivar(CODEREP *cr, INT32 mu_vsym_depth = 0);
  CODEREP    *Hash_New_ivar(CODEREP *cr)
    { Is_True(cr->Kind() == CK_IVAR,("CODEMAP::Hash_Ivar, wrong kind"));
      cr->Set_sign_extension_flag();
      IDX_32 hash_idx = Hash_ivar(cr, cr->Ilod_base() ?
				  cr->Ilod_base() : cr->Istr_base());
      return Append_CR(hash_idx,cr);
    }
  CODEREP    *Hash_Op(CODEREP *cr, BOOL canonicalize = TRUE)
    { Is_True(cr->Kind() == CK_OP,("CODEMAP::Hash_Op, wrong kind"));
      IDX_32 hash_idx = Hash_op_and_canon(cr, canonicalize);
      return OPCODE_is_volatile(cr->Op()) ? 
	Append_CR(hash_idx, cr) : Find_or_append_CR(hash_idx,cr,0);
    }

  CODEREP    *Add_lda_node(CODEREP *, INT64 /* new offset */);

  CODEREP    *Add_unary_node(OPCODE, CODEREP *);

  CODEREP    *Add_bin_node(OPCODE,    // opcode
                           CODEREP *, // kid 0
                           CODEREP *);// kid 1

  CODEREP    *Add_unary_node_and_fold(OPCODE,    // opcode
				      CODEREP *);// kid

  CODEREP    *Add_bin_node_and_fold(OPCODE,    // opcode
				    CODEREP *, // kid 0
				    CODEREP *, // kid 1
                                    BB_NODE *bb=NULL);

  CODEREP    *Add_expr_and_fold(CODEREP *);

  CODEREP    *Add_tertiary_node(OPCODE, CODEREP *, CODEREP *, CODEREP *);

  CODEREP    *Add_nary_node(OPCODE,     // opcode
			    CODEREP **, // array of kid pointers
			    mUINT32);   // how many kids

  CODEREP    *Add_const(MTYPE typ,
                        INT64 val);

  CODEREP    *Add_expr(WN *wn,        // given a WN node, add it to the coderep
                       OPT_STAB *,    // hash and return the coderep for it.
                       STMTREP *,
                       BOOL *,
		       COPYPROP *,
		       BOOL no_complex_preg = FALSE);

  BOOL        Add_expr(WN *wn,        // given a WN node, add it to
                       OPT_STAB *,    // the coderep hash and return
                       STMTREP *,     // the coderep and the constant
                       CANON_CR *,    // portion in the CANON_CR.
                                      // This function traverse wn
                                      // recursively down and build up
                                      // the coderep.
		       COPYPROP *,
		       BOOL no_complex_preg = FALSE);

  CODEREP    *Add_def(IDTYPE,         // given a ST id, add it to the
                      mINT16,         // coderep
                      STMTREP *,
                      MTYPE,
                      MTYPE,
                      mINT32,
                      TY_IDX,
		      UINT field_id,
		      BOOL is_store);

  CODEREP    *Add_idef(OPCODE,        // Add a ilod/mlod into coderep
		       OCC_TAB_ENTRY *,
		       STMTREP *,
		       MU_NODE *,
		       MTYPE,
		       MTYPE,
		       TY_IDX,
		       UINT field_id,
		       mINT32,
		       CODEREP *,
		       CODEREP *,
		       CODEREP *, 
                       OPT_STAB* );

  // add optparm node over parameters of various ops
  CODEREP    *Add_optparm( CODEREP * );

  CODEREP    *Add_tcon(TCON_IDX);	     // convert a tcon to a coderep

  CODEREP    *Cur_def(WN *wn, OPT_STAB*);

  CODEREP    *Iload_folded(WN *wn, CANON_CR *base_ccr);	// to fold an ILOAD

  CODEREP    *Rehash(CODEREP *cr,
                     BOOL canon=TRUE);

  void        New_temp_id(void)      { ++_pre_temp_id; }
  IDTYPE      Cur_temp_id(void) const{ return _pre_temp_id; }

  void	      Remove(CODEREP *cr);   // remove this node from htable

  SSA        *Ssa(void) const        { return _ssa; } 
  OPT_STAB   *Sym(void) const        { return sym; } 
  OPT_STAB   *Opt_stab(void) const   { return sym; }
  MEM_POOL   *Mem_pool(void) const   { return mem_pool; }
  WN_MAP      Prefetch_map(void)const{ return _prefetch_map; }
  INT32	      Coderep_id_cnt(void)const{ return _coderep_id_cnt; }
  INT32	      Next_coderep_id(void)  { return _coderep_id_cnt++; }

  void        Update_pref(CODEREP *ivar) const;

  void        Set_hash_bucket(IDX_32 idx,
                              CODEREP *l) { hash_vec[idx] = l; }
  INT32	      Num_iloadfolds(void) const { return _num_iloadfolds; }
  INT32	      Num_istorefolds(void) const { return _num_istorefolds; }
  void	      Inc_istorefolds(void)	{ _num_istorefolds++; }
  INT32	      Num_inputprops(void) const { return _num_inputprops; }
  void	      Inc_inputprops(void)	{ _num_inputprops++; }
  INT32	      Num_mainprops(void) const { return _num_mainprops; }
  void	      Inc_mainprops(void)	{ _num_mainprops++; }
  INT32	      Num_shrinks(void)   const { return _num_shrinks; }
  void	      Inc_shrinks(void)	        { _num_shrinks++; }

  // Expand the expression into loop-invariants or defined by phi, chi
  CODEREP    *Expand_expr(CODEREP *, const BB_LOOP *, INT32 *limit);    

  CODEREP    *Convert_to_loop_invar(CODEREP *, BB_LOOP *); 

  // canonicalization after copy prop
  CODEREP    *Canon_base(CODEREP *, INT64 *);   // canonicalize ilod_base, istr_base
  CODEREP    *Canon_rhs(CODEREP *);             // canonicalize regular expr
  BOOL        Canonicalize_compare(CODEREP *,   // return TRUE if cononicalized
                                   BB_NODE *,   // canonicalize comparisons
                                   BOOL *);     // has tree modified

  void        Reset_DCE_visited_flags();        // reset the DCE_VISITED flags
  void        Print(FILE *fp = stderr) const;
  void        Print_CR(CODEREP *cr, FILE *fp = stderr) const;
  void        Print_SR(STMTREP *cr, FILE *fp = stderr) const;

  //  var phi hash 
  void        Init_var_phi_hash(void);
  void        Verify_var_phi_hash(void);
  void        Enter_var_phi_hash(PHI_NODE *phi);
  void        Remove_var_phi_hash(PHI_NODE *phi);
  PHI_NODE   *Lookup_var_phi(const BB_NODE *bb, const AUX_ID id) const;

  void        Set_phi_hash_valid(void)     { _phi_hash_valid = TRUE; }
  void        Reset_phi_hash_valid(void)   { _phi_hash_valid = FALSE; }
  BOOL        Phi_hash_valid(void)const    { return _phi_hash_valid; }

  //  Enter an expression into the htable
  CODEREP    *Rehash_tree(CODEREP *,
                          BOOL,     // constant prop ?
                          BOOL *,   // folded ?
                          BB_NODE * // containing BB_NODE
                          );

  //  Convert a zero version phi opnd into non-zero version
  void        Fix_zero_version(PHI_NODE *, INT, bool allow_real_def = false);
  void        Fix_zero_version(CHI_NODE *, STMTREP *);

  //  Aggressive invariant detection
  void        Convert_iload_to_loop_invariant(BB_LOOP *, CODEREP *);

  //  Insert phi node
  void        Insert_var_phi(CODEREP *, BB_NODE *);

  //  Verify that all codereps are in the right hash bucket.
  void        Verify_hashing(void);
};


// Iterator for CODEMAP
//    iterates from 0 to size
//
class CODEMAP_ITER {
private:
  INT32     cr_bucket;      // the symbol id
  const CODEMAP  *codemap;

  CODEMAP_ITER(const CODEMAP_ITER&);
  CODEMAP_ITER& operator = (const CODEMAP_ITER&);

public:
        CODEMAP_ITER(CODEMAP *htable)       { cr_bucket = 0; codemap = htable; }
        CODEMAP_ITER(void)                  {}
       ~CODEMAP_ITER(void)		    {}
  void  Init(const CODEMAP *htable)         { codemap = htable; }
  INT32 First(void)                         { return (cr_bucket = 0); }
  BOOL  Is_Empty(void) const                { return cr_bucket >= codemap->size; }
  INT32 Next(void)                          { return ++cr_bucket; }
  INT32 Cur(void) const                     { return cr_bucket; }
  CODEREP *First_elem(void)                 { return codemap->Hash_bucket(First()); }
  CODEREP *Next_elem(void)                  { return codemap->Hash_bucket(Next()); }
};

// Internal representation for statement
class STMTREP : public CHAIN_NODE {
private:

  // flags for STMTREP: be sure to use a single bit per flag, and make
  // sure the field is large enough for the largest flag.  Be sure to
  // prepend "SRF_" on each flag value to keep its name unique
  enum STMTREP_FLAGS {
    SRF_NONE		= 0x00,
    SRF_USELIST         = 0x01, // is _use_list rather than _wn
    SRF_LIVE_STMT	= 0x02,
    SRF_BLACK_BOX	= 0x04,	// is this stmt a "black-box"
    SRF_ANTLOC          = 0x08, // is this stmt antloc, for store stmts only
    SRF_DEFINE_ZVER     = 0x10, // defines a zero version
    SRF_HAS_VOLATILE    = 0x20, // this statement has volatile load/store
    SRF_IDENTITY_ASGN   = 0x40, // this statement is of the form i = i.
    SRF_HAS_CTRL_FLOW   = 0x80, // this statement has control flow inside (IO) (black box only)
    SRF_IVR_INTRODUCED  = 0x100, // secondary IV introduced by IVR
    SRF_DCE_RETVSYM     = 0x200, // DCE processed for return vsym
    SRF_IV_UPDATE       = 0x400, // IV update statement
    SRF_NOT_IV_UPDATE   = 0x800, // NOT an IV update statement
    SRF_REPAIRED	= 0x1000,// iv update injury (if any) has been repaired
    SRF_RHS_SAVED       = 0x2000,// RHS saved to its preceding STMT
    SRF_SAVED_RHS       = 0x4000,// RHS used to be RHS of its sucessing STMT
    SRF_DIFF_SSU_VERSION = 0x8000,// set by SPRE iphi insertion and used in
				  // SPRE SSU renaming phase
    SRF_SIZE_VISITED    = 0x10000,// Used by No_truncation_by_value_size
    SRF_IPHI_INSERTED   = 0x20000,// during SSU construction, this store stmt
				  // has caused iphi's to be inserted
    // All 18 flag bits currently used; if you need another one,
    // reduce the _unused field by one bit and increase the _flags
    // field.
  };

  OPERATOR  _opr:8;
  MTYPE     _rtype:8;        // result type
  MTYPE     _desc:8;         // descriptor type 
  CODEREP   *_lhs;          // the lhs if it is assignment statement
  // TODO: define the alias variable list in case of pointer dereference
  CODEREP   *_rhs;          // the rhs of an assignment statement, or
                              // the expression in a OPR_TRUEBR

  // this union should be almost identical to that in WN, with as
  // many fields copied over for statements as necessary
  union {
    INT32      _label_number;   // same as WN_label_number()
    INT32      _call_flags;     // the WN_call_flags() field
    EXC_SCOPE *_exc_scope;      // exception scope info, defined in opt_exc.h
    POINTS_TO_LIST *_pt_list;   // POINTS_TO_LIST for BARRIER and DEALLOCA
    ST_IDX     _asm_string_idx; // for ASM_STMT
  } _u1;

  // this union should be almost identical to that in WN, with as
  // many fields copied over for statements as necessary
  //
  // Actually, this union has grown considerably since the above
  // comment was written.
  union {
    ST              *_st;	    // same as WN_st()
    TY_IDX           _ty;           // type pointer from wn
    WN              *_black_box_wn; // copy of orig wn -- not converted
    WN              *_orig_wn;	    // copy of orig wn (for easier emitting)
    WN              *_prefetch_wn;  // copy of orig wn (for easier emitting)
    WN              *_barrier;      // copy of orig wn (for easier emitting)
    IND_EXPR        *_ind_expr;     // list of induction expr update notes
    ASM_PRAGMA_INFO *_asm_pragma;   // information to reconstruct asm
				    // pragmas at emit time
  } _u2;

  // this union is controlled by the SRF_WN_LIST flag.  If it's set,
  // use the _wns_list value, else use the _wn value.  Should only
  // be used during emit time.
  union {
    WN     *_wn;              // original 'node' or generated WHIRL
    USE_LIST *_use_list;      // list of whirl generated from this
  } _u3;

  BB_NODE  *bb;               // the BB that it belongs to
  SRCPOS    _linenum;         // source position information

  // Store the constraint graph call site id for the Nystrom alias analyzer
  // so as to restore it during CODEMAP -> WHIRL translation
  mUINT32 _constraint_graph_callsite_id;

  union {
    UINT32     _label_flags;  // the label flags
#ifndef KEY
    UINT32     _asm_stmt_flags;  // the ASM_STMT flags
#endif
    MU_LIST   *_mu_list;      // list of possibly ref'd values
//    MTYPE      _rhs_type;     // rhs type for assignment statements
  } _u4;
  CHI_LIST *_chi_list;        // list of possibly modified'd values
  union {
    IDX_32    _bitpos;        // PRE bit-position
    IDX_32    _stmt_id;	      // new PRE stmt id
  } _u5;
  UINT        _flags : 18;       // field to hold various bit-flags
  UINT        _proj_op_uses : 2; // number of uses of the unique
				 // projectible operation on the RHS
				 // of STID.
#ifdef KEY
  UINT32      _str_red_num: 4;  // for IV update stmts, # of induction exprs
  				// injured by it during EPRE
  UINT32      _asm_stmt_flags:3;  // the ASM_STMT flags
#ifdef TARG_SL //fork_joint
 BOOL       _sl2_compgoto_para : 1; //used to mark if the stmt is a compgoto for sl2 major fork. 
 BOOL       _sl2_compgoto_for_minor : 1; // used to mark if the stmt is a compgoto for sl2 minor fork. 
 BOOL       _sl2_internal_mem_ofst : 1; // mark if the stmt is an istore
 UINT       _unused : 2;      // allocate new flag bits from here.
#else  
  UINT        _unused : 5;      // allocate new flag bits from here.
#endif
#else
  UINT        _unused : 12;      // allocate new flag bits from here.
#endif

  // initializer to be called by all constructors
  void Init(void)		{ _lhs = _rhs = NULL;
				  _opr = OPERATOR_UNKNOWN;
				  _rtype = _desc = MTYPE_UNKNOWN;
				  bb = NULL;
#ifndef KEY
				  _flags = SRF_NONE;
#else // need this if all optimization phases are disabled
				  _flags = SRF_LIVE_STMT;
#endif
				  _linenum = (SRCPOS)0;
				  _u4._mu_list = NULL;
				  _chi_list = NULL;
				  // clear largest field in u1
				  _u1._label_number = 0;
				  // clear largest field in u2
				  _u2._st = NULL;
				  // clear largest field in u3
				  _u3._wn = NULL; 
				  _u5._bitpos = ILLEGAL_BP;
				  // Show enough uses that if no
				  // analysis is done, we will emit
				  // the stmt unchanged.
				  _proj_op_uses = 2;
#if defined(TARG_SL)
				  _sl2_internal_mem_ofst = 0;
#endif
                                  _constraint_graph_callsite_id = 0;
				}

  STMTREP (const STMTREP&);
  STMTREP& operator = (const STMTREP&);

  DECLARE_CHAIN_NODE_CLASS( STMTREP )

public:
  STMTREP (OPCODE opc)        { Init(); 
				_opr = OPCODE_operator(opc); 
				_rtype = OPCODE_rtype(opc);
				_desc = OPCODE_desc(opc); }
  STMTREP (void)              { Init(); }
  ~STMTREP(void)              { Init(); }

  void      Init(CODEREP *l,
                 CODEREP *r,
                 OPCODE opc)  { _lhs=l; _rhs=r; 
				_opr = OPCODE_operator(opc); 
				_rtype = OPCODE_rtype(opc);
				_desc = OPCODE_desc(opc); }
  void      Enter_rhs(CODEMAP*, OPT_STAB*, COPYPROP*, EXC*); // enter rhs of the stmt to HASH
  void      Enter_lhs(CODEMAP*, OPT_STAB*, COPYPROP*); // enter rhs of the stmt to HASH
  void      Print(FILE *fp = stderr) const;
				     // print out this STMTREP and its subtree
  void      Print_node(FILE *fp = stderr) const; // print out this STMTREP
  char     *Print_str(BOOL);		    // print out STMTREP to a string
  BOOL	    Verify_IR(CFG *, CODEMAP *, INT);    // consistency check on opt IR
  WN       *Gen_wn(STMT_CONTAINER *, EMITTER *);// generate tree for STMTREP
  BOOL      Gen_wn(MAIN_EMITTER *);	    // mainopt emitter
  // mainopt emitter -- checks non-required statements to see if any
  // of its expressions must be saved to pregs
  void      Check_unsaved_exprs(MAIN_EMITTER*);

  WN	   *Insert_stmt_wn(MAIN_EMITTER *, BB_NODE *);
  BOOL	    Antloc_and_deletions(MAIN_EMITTER *, BB_NODE *);
  OPCODE    Op(void) const                  { return OPCODE_make_op(_opr, _rtype, _desc); }
  OPERATOR  Opr(void) const           	    { return _opr; }
  MTYPE	    Rtype(void) const		    { return _rtype; }
  MTYPE	    Desc(void) const		    { return _desc; }

  WN       *Wn(void) const                 { return _u3._wn; }
  void      Set_wn( WN *wn )               { _u3._wn = wn; }

  USE_LIST *Use_list(void) const
		{ Is_True( Is_use_list(), ("Not Is_use_list") );
		  return _u3._use_list; }
  void      Set_use_list( USE_LIST *uses )
		{ _u3._use_list = uses;
		  Set_is_use_list(); }

  CODEREP  *Lhs(void) const                 { return _lhs; }
  CODEREP  *Rhs(void) const                 { return _rhs; }
  BB_NODE  *Bb(void) const                  { return bb; }
  void	    Set_op(OPCODE opc)		    { _opr = OPCODE_operator(opc); 
					      _rtype = OPCODE_rtype(opc);
					      _desc = OPCODE_desc(opc); }
  void	    Set_opr(OPERATOR opr)	    { _opr = opr; }
  void	    Set_rtype(MTYPE rtype)	    { _rtype = rtype; }
  void	    Set_desc(MTYPE desc)	    { _desc = desc; }
  void      Set_bb(BB_NODE *b)              { bb = b; }
  void      Set_lhs(CODEREP *l)             { _lhs = l; }
  void      Set_rhs(CODEREP *r)             { _rhs = r; }

  STMTREP  *Duplicate(MEM_POOL *p);

  // get aliasing information associated with the statement
  POINTS_TO *Points_to(OPT_STAB *opt_stab) const;

  // set/reset various _flags 
  void      Reset_flag(CR_FLAG f)     { _flags = _flags&~f; }
  void      Set_flag(CR_FLAG f)       { _flags = _flags | f; }
  BOOL      Is_flag_set(CR_FLAG f)const{ return _flags & f; }
  // is this statement considered "live" or not?
  BOOL	    Live_stmt(void) const	{ return _flags & SRF_LIVE_STMT; }
  void	    Set_live_stmt(void)		{ _flags |= SRF_LIVE_STMT; }
  void	    Reset_live_stmt(void)	{ _flags &= ~SRF_LIVE_STMT; }
  // is this statement has "volatile access" or not?
  BOOL	    Volatile_stmt(void) const	{ return _flags & SRF_HAS_VOLATILE; }
  void	    Set_volatile_stmt(void)	{ _flags |= SRF_HAS_VOLATILE; }
  void	    Reset_volatile_stmt(void)	{ _flags &= ~SRF_HAS_VOLATILE; }
  // used by EXC_SCOPE stmt
  EXC_SCOPE *Exc_scope(void) const      { return _u1._exc_scope; }
  void	    Set_exc_scope(EXC_SCOPE *ex){ _u1._exc_scope = ex; }
  // is this statement considered a "black box" and is not modified
  BOOL	    Black_box(void) const	{ return _flags & SRF_BLACK_BOX; }
  void	    Set_black_box(void)		{ _flags |= SRF_BLACK_BOX; }
  void	    Reset_black_box(void)	{ _flags &= ~SRF_BLACK_BOX; }
  WN	   *Black_box_wn(void) const	{ return _u2._black_box_wn; }
  void      Set_black_box_wn(WN *box_wn){ _u2._black_box_wn = box_wn; }
  // used by various stmts
  WN       *Orig_wn(void) const	        { return _u2._orig_wn; }
  void	    Set_orig_wn(WN *orig_wn)	{ _u2._orig_wn = orig_wn; }
  // used by PREFETCH stmt
  WN       *Prefetch_wn(void) const	{ return _u2._prefetch_wn; }
  void	    Set_prefetch_wn(WN *pf_wn)	{ _u2._prefetch_wn = pf_wn; }
  // used by zero version only
  BOOL      Has_zver(void) const         { return _flags & SRF_DEFINE_ZVER; }
  void      Set_has_zver(void)           { _flags |= SRF_DEFINE_ZVER; }
  void      Reset_has_zver(void)         { _flags &= ~SRF_DEFINE_ZVER; }
  void      Recompute_has_zver(void);	// recompute this flag from scratch

  BOOL      Is_identity_asgn(void) const { return _flags & SRF_IDENTITY_ASGN; }
  void      Set_identity_asgn(void)      { _flags |= SRF_IDENTITY_ASGN; }
  void      Reset_identity_asgn(void)    { _flags &= ~SRF_IDENTITY_ASGN; }

  // is this statement anticipated locally
  BOOL      Antloc(void) const          { return _flags & SRF_ANTLOC; }
  void      Set_antloc(void)            { _flags |= SRF_ANTLOC; }
  void      Reset_antloc(void)          { _flags &= ~SRF_ANTLOC; }

  // should we access _wn or _wn_list?
  BOOL      Is_use_list(void) const     { return _flags & SRF_USELIST; }
  void      Set_is_use_list(void)       { _flags |= SRF_USELIST; }
  void      Reset_is_use_list(void)     { _flags &= ~SRF_USELIST; }

  // for IO statement, does this have goto insid eit?
  BOOL	    Has_ctrl_flow(void) const{ return _flags & SRF_HAS_CTRL_FLOW; }
  void	    Set_has_ctrl_flow(void)	{ _flags |= SRF_HAS_CTRL_FLOW; }

  // is this an assignment to secondary IV
  BOOL      Ivr_introduced(void) const  { return _flags & SRF_IVR_INTRODUCED; }
  void      Set_ivr_introduced(void)    { _flags |= SRF_IVR_INTRODUCED; }
  void      Reset_ivr_introduced(void)  { _flags &= ~SRF_IVR_INTRODUCED; }

  // DCE processed this statement for return vsym
  BOOL      Dce_retvsym(void) const     { return _flags & SRF_DCE_RETVSYM; }
  void      Set_dce_retvsym(void)       { _flags |= SRF_DCE_RETVSYM; }
  void      Reset_dce_retvsym(void)     { _flags &= ~SRF_DCE_RETVSYM; }

  // For use of EPRE/LPRE/SPRE, after statement ID's are
  // assigned. Defined in opt_etable.cxx.
  BOOL    Stmt_order_less_or_equal(const STMTREP *) const;

  // New PRE Strength-Reduction.  Does this statement update an IV?
  // Notice that we have both true and false so we know whether we've
  // actually determined if the statement does [not] update an IV
  BOOL      Iv_update(void) const       { return _flags & SRF_IV_UPDATE; }
  void      Reset_iv_update(void)       { _flags &= ~SRF_IV_UPDATE; }
  void      Set_iv_update(void)
    {
      Is_True(!Not_iv_update(),
	      ("STMTREP::Set_iv_update: Contradictory flags"));
      if (!(_flags & SRF_IV_UPDATE))
        _str_red_num = 0;
      _flags |= SRF_IV_UPDATE;
    }
  BOOL      Not_iv_update(void) const   { return _flags & SRF_NOT_IV_UPDATE; }
  void      Set_not_iv_update(void)
    {
      Is_True(!Iv_update(),
	      ("STMTREP::Set_not_iv_update: Contradictory flags"));
      _flags |= SRF_NOT_IV_UPDATE;
    }

  // member functions to support SSAPRE strength reduction. THESE
  // MEMBERS SHOULD BE CALLED ONLY FROM WITHIN MEMBER FUNCTIONS OF
  // class STR_RED! Don't call them from anywhere else because STR_RED
  // maintains the flags and they may not be consistent outside. If
  // you want to know whether an injury has been repaired, use
  // STR_RED::Repaired(const STMTREP *).
  void      Reset_repaired(void)        { _flags &= ~SRF_REPAIRED; }
  void      Set_repaired(void)          { _flags |= SRF_REPAIRED; }
  BOOL      Repaired(void) const        { return _flags & SRF_REPAIRED; }
  
  // SSAPRE CodeMotion step.  Has the RHS of this statement been saved
  // in to its preceding statement?
  void      Set_RHS_saved(void)         { _flags |= SRF_RHS_SAVED; }
  void      Reset_RHS_saved(void)       { _flags &= ~SRF_RHS_SAVED; }
  BOOL      Is_RHS_saved(void) const    { return _flags & SRF_RHS_SAVED; }

  void      Set_saved_RHS(void)         { _flags |= SRF_SAVED_RHS; }
  void      Reset_saved_RHS(void)       { _flags &= ~SRF_SAVED_RHS; }
  BOOL      Is_saved_RHS(void) const    { return _flags & SRF_SAVED_RHS; }

  void      Reset_RHS_saved_saved_RHS(void) { _flags &= ~(SRF_SAVED_RHS|SRF_RHS_SAVED); }

  void      Set_diff_ssu_version(void)  { _flags |= SRF_DIFF_SSU_VERSION; }
  BOOL      Is_diff_ssu_version(void) const { return _flags & SRF_DIFF_SSU_VERSION; }

  void      Set_size_visited(void)      { _flags |= SRF_SIZE_VISITED; }
  BOOL      Is_size_visited(void) const { return _flags & SRF_SIZE_VISITED; }
  void      Reset_size_visited(void)    { _flags &= ~SRF_SIZE_VISITED; }

  void      Set_iphi_inserted(void)  { _flags |= SRF_IPHI_INSERTED; }
  BOOL      Is_iphi_inserted(void) const { return _flags & SRF_IPHI_INSERTED; }

  // Projectable operation optimization: DIVREM, SINCOS, etc.
  UINT      Proj_op_uses(void) const    { return _proj_op_uses; }
  void      Set_proj_op_uses(UINT proj_op_uses)
    { _proj_op_uses = MIN(proj_op_uses, 2); }
  void      Inc_proj_op_uses(UINT inc = 1)
    {
      if (_proj_op_uses + inc < 3) {
	_proj_op_uses += inc;
      }
      else {
	_proj_op_uses = 2;
      }
    }

  SRCPOS    Linenum(void) const         { return _linenum; }
  void      Set_linenum(SRCPOS ln)      { _linenum = ln; }

  BOOL      Has_mu(void) const;
  BOOL      Has_chi(void) const;
  MU_LIST  *Mu_list(void) const	        { return _u4._mu_list; }
  void      Set_mu_list( MU_LIST *mu )  { _u4._mu_list = mu; }

  //  MTYPE     Rhs_type(void) const        { return _u4._rhs_type; }
  //  void      Set_rhs_type( MTYPE ty )    { _u4._rhs_type = ty; }

  CHI_LIST *Chi_list(void) const	{ return _chi_list; }
  void      Set_chi_list(CHI_LIST *chi) { _chi_list = chi; }

  // for those statements that have label_numbers associated with them,
  // (e.g., OPR_TRUEBR, OPR_GOTO) return the label of the block
  INT32     Label_number(void) const    { return _u1._label_number; }
  void      Set_label_number(INT32 lab) { _u1._label_number = lab; }
  
  INT32     Call_flags(void) const      { return _u1._call_flags; }
  void      Set_call_flags(INT32 flags) { _u1._call_flags = flags; }

  POINTS_TO_LIST *Pt_list(void) const   { return _u1._pt_list; }
  void      Set_pt_list(POINTS_TO_LIST *pt_list) { _u1._pt_list = pt_list; }

  ST_IDX    Asm_string_idx(void) const   { return _u1._asm_string_idx; }
  void      Set_asm_string_idx(ST_IDX i) { _u1._asm_string_idx = i; }

  TY_IDX    Ty(void) const              { return _u2._ty; }
  void      Set_ty(TY_IDX ty)           { _u2._ty = ty; }

  WN       *Barrier(void) const         { return _u2._barrier; }
  void      Set_barrier(WN *wn)         { _u2._barrier = wn; }

  // for those statements that have ST's associated with them
  // (e.g., OPR_STID, OPR_CALL, OPR_GOTO)
  ST        *St(void) const             { return _u2._st; }
  void       Set_st(ST *sym)            { _u2._st = sym; }

  // for statements that have induction expression update notes
  IND_EXPR  *Ind_expr(void) const       { return _u2._ind_expr; }
  void       Set_ind_expr(IND_EXPR *ie) { _u2._ind_expr = ie; }

  // for asm statements that have WHIRL pragmas for constraint and
  // clobber specifications attached 
  ASM_PRAGMA_INFO *Asm_pragma(void) const { return _u2._asm_pragma; }
  void       Set_asm_pragma(ASM_PRAGMA_INFO *a) { _u2._asm_pragma = a; }

  // for the label flags
  UINT32     Label_flags(void) const    { return _u4._label_flags; }
  void       Set_label_flags(UINT32 f)  { _u4._label_flags = f; }

  UINT32     Str_red_num(void) const	{ return _str_red_num; }
  void	     Inc_str_red_num(void)	{ _str_red_num++; }

#ifdef TARG_SL //fork_joint
  // we need passing fork compgoto flag from whirl node to stmtrep 
  BOOL      Fork_stmt_flags(void) const       { return _sl2_compgoto_para; }
  void      Set_fork_stmt_flags(BOOL f)       { _sl2_compgoto_para = f; }
  BOOL      Minor_fork_stmt_flags(void) const { return _sl2_compgoto_for_minor; } 
  void      Set_minor_fork_stmt_flags(BOOL f) { _sl2_compgoto_for_minor = f; } 
  BOOL      SL2_internal_mem_ofst(void) const { return _sl2_internal_mem_ofst; }
  void      Set_SL2_internal_mem_ofst(BOOL f) { _sl2_internal_mem_ofst = f; }
#endif 

  // for the ASM_STMT flags
#ifdef KEY
  UINT32     Asm_stmt_flags(void) const    { return _asm_stmt_flags; }
  void       Set_asm_stmt_flags(UINT32 f)  { _asm_stmt_flags = f; }
#else
  UINT32     Asm_stmt_flags(void) const    { return _u4._asm_stmt_flags; }
  void       Set_asm_stmt_flags(UINT32 f)  { _u4._asm_stmt_flags = f; }
#endif

  IDX_32    Bitpos(void) const          { return _u5._bitpos; }
  void      Set_Bitpos(IDX_32 bp)     	{ _u5._bitpos = bp; }
  char     *Print_bit(void) const;

  // new PRE stmt_id
  IDX_32    Stmt_id(void) const         { return _u5._stmt_id; }
  void      Set_stmt_id(IDX_32 sid)   	{ _u5._stmt_id = sid; }

  // Duplicate the functionality of various WN_Create... functions
  // except that these assume the instance of the STMTREP has already
  // been created with an opcode, and "this" is filled in with the
  // necessary information by these functions.
  // Always make sure to have the linenum parameter to force callers
  // to provide one.
  // Please keep this list in alphabetical order.

  // unconditional goto statement.  'sym' is non-NULL if the goto is to
  // a user-defined label.  'label_number' is the generated number for
  // the block being jumped to
  void      Init_Goto(ST *sym, INT32 label_number, SRCPOS ln)
    { Set_st(sym); Set_label_number(label_number);
      Set_linenum(ln); }

  // label statement.  'sym' is non-NULL if the label is for a user-
  // defined label.  'label_number' is the generated number for
  // the block being jumped to
  void      Init_Label(ST *sym, INT32 label_number, SRCPOS ln)
    { Set_st(sym); Set_label_number(label_number);
      Set_linenum(ln); }

  BOOL      Assign_same_var(STMTREP *b);// check if both stmtrep
                                        // assign to the same lhs.
  BOOL      Redefines_var( AUX_ID var );// check if this redefs var
  BOOL      References_var( AUX_ID var );// check if this refs var
  BOOL      Is_incr(void) const;        // is v = v +/- const, the RHS
                                        // pattern is recursively defined.
  void      Find_ind_incr(CFG *,        // Find the induction
                          ITABLE *);    // expression that would
                                        // require update after this
                                        // assignment statement.

  // is STMTREP a DEF w/const RHS. used by main emitter and IVE
  BOOL	    Const_prop_cand(const CODEREP *) const; 

  // do two STMTREP contain the same LHS.
  BOOL      Same_lhs(const STMTREP *) const;

  void      Replace_CR(CODEMAP *htable, CODEREP *, CODEREP *);

  void      Verify_CODEMAP(CODEMAP *htable, OPT_STAB *opt_stab);

  BOOL      Is_identity_assignment_removable(void) const;

  BOOL      Contains_volatile_ref(const BVECTOR &cr_vol_map) const;

  BOOL	    Has_zero_version_chi(void) const;

  void      Clone(STMTREP *, CODEMAP *, MEM_POOL *pool);

  // For the Nystrom alias analyzer
  mUINT32   Get_constraint_graph_callsite_id() const 
  {
    return _constraint_graph_callsite_id;
  }
  void      Set_constraint_graph_callsite_id(mUINT32 c)
  {
    _constraint_graph_callsite_id = c;
  }

}; // end of class STMTREP

//============================================================================
// Internal representation for an expression being built.
// This class maintains the subtree of CRs and the additive integer part
// separately for canonicalization.
class CANON_CR {
private:
  CODEREP  *_tree;                  // the CR part of the tree
  INT64	    _scale;            	    // the additive integer part

  // private constructor so it cannot be used
  CANON_CR(const CANON_CR&);
  CANON_CR& operator = (const CANON_CR&);

public:
  CANON_CR(void)                    {}
  ~CANON_CR(void)                   {}

  CODEREP  *Convert2cr(WN *wn,
                       CODEMAP *htable,
                       BOOL     foldit) const;
  CODEREP  *Convert2cr(MTYPE typ, OPERATOR opr, OPCODE opc, 
                       CODEMAP *htable, BOOL foldit) const;

  void      Trim_to_16bits(WN *wn,
			   CODEMAP *htable);

  // These are member access functions.
  CODEREP  *Tree(void) const        { return _tree; }
  void      Set_tree(CODEREP *t)    { _tree = t; }
  INT64     Scale(void) const       { return _scale; }
  void      Set_scale(INT64 s)      { _scale = s; }

  void      Print(FILE *fp = stderr) const;
};


// given the opcode of the operator and some info about which kid we
// are dealing with, come up with the type for the kid 
extern MTYPE Operand_type(OPCODE, INT, INT);


template <class traverseCR>
void traverseSR(STMTREP *stmt, traverseCR &traverse_cr)
{
   // This traversal will skip fake codereps (e.g. calls, etc). The function
   // object "traverseCR" must take parameters (CODEREP*,STMTREP*,INT32 kidno).
   //
   CODEREP * const rhs = stmt->Rhs();
   CODEREP * const lhs = stmt->Lhs();
	
   if (OPCODE_is_fake(stmt->Op()))
      for (INT32 i = 0; i < rhs->Kid_count(); i++)
	 traverse_cr(rhs->Opnd(i), stmt, i);
   else if (rhs != NULL) 
      traverse_cr(rhs, stmt, 0);
   if (lhs != NULL)
      traverse_cr(lhs, stmt, 1);
}

///////////////////////////////////////////////////////////////////////////
//
//   MEMOP_ANNOT_CR_SR_MAP is a map between CODEREP/STMTREP to their 
// corresponding annotations. 
//
///////////////////////////////////////////////////////////////////////////
//
struct cr_cmp {
  bool operator () (const CODEREP* cr1, const CODEREP* cr2) const {
    Is_True (cr1->Coderep_id() != 0 && cr2->Coderep_id() != 0, ("CODEREP does not has ID"));
    return cr1->Coderep_id() < cr2->Coderep_id(); 
  }
};
 
typedef std::pair<const CODEREP*, MEMOP_ANNOT*> CR_MEMANNOT_PAIR;
typedef mempool_allocator<CR_MEMANNOT_PAIR> CR_ANNOT_MAP_ALLOC;
typedef std::map<const CODEREP*, MEMOP_ANNOT*, cr_cmp, CR_ANNOT_MAP_ALLOC> CR_2_MEM_ANNOT_MAP;

struct sr_cmp {
  bool operator () (const STMTREP* sr1, const STMTREP* sr2) const {
    return sr1 < sr2;
  }
};

typedef std::pair<const STMTREP*, MEMOP_ANNOT*> SR_MEMANNOT_PAIR;
typedef mempool_allocator<SR_MEMANNOT_PAIR>    SR_ANNOT_MAP_ALLOC;
typedef std::map<const STMTREP*, MEMOP_ANNOT*, sr_cmp, SR_ANNOT_MAP_ALLOC>
        SR_2_MEM_ANNOT_MAP;

class MEMOP_ANNOT_CR_SR_MGR : public MEMOP_ANNOT_MGR {
private:
  CR_2_MEM_ANNOT_MAP _cr_map;
  SR_2_MEM_ANNOT_MAP _sr_map;
  BOOL _trace;
  BS* _imported;
  BS* _exported;

  void Set_imported (MEMOP_ANNOT* a) 
        { _imported = BS_Union1D (_imported, a->Id(), _mp); }
  BOOL Is_imported (MEMOP_ANNOT* a) const 
        { return BS_MemberP (_imported, a->Id());}

  void Set_exported (MEMOP_ANNOT* a)
        { _exported = BS_Union1D (_exported, a->Id(), _mp); }
  BOOL Is_exported (MEMOP_ANNOT* a)
        { return BS_MemberP (_exported, a->Id()); }

public:
  MEMOP_ANNOT_CR_SR_MGR (MEM_POOL* mp, BOOL trace);

  // Lookup the corresponding annotation 
  MEMOP_ANNOT*      Get_annot (CODEREP* cr);
  MEMOP_ANNOT_ITEM* Get_annot (CODEREP* cr, MEM_ANNOT_KIND kind) ;
  MEMOP_ANNOT*      Get_annot (STMTREP* sr) ;
  MEMOP_ANNOT_ITEM* Get_annot (STMTREP* sr, MEM_ANNOT_KIND kind);

  // Associate a MEMOP_ANNOT with given WN/CODEREP/STMTREP
  void Add_annot (CODEREP* cr, const MEMOP_ANNOT_ITEM& annot_item);
  void Add_annot (STMTREP* stmt, const MEMOP_ANNOT_ITEM& annot_item);
  void Set_annot (CODEREP* cr, MEMOP_ANNOT* annot);
  void Set_annot (STMTREP* sr, MEMOP_ANNOT* annot);

  // Import annotation from WN=>MEMOP_ANNOT map.
  MEMOP_ANNOT* Import_annot (CODEREP* cr, MEMOP_ANNOT* annot) ;
  MEMOP_ANNOT* Import_annot (STMTREP* sr, MEMOP_ANNOT* annot) ;
  MEMOP_ANNOT* Import_annot (MEMOP_ANNOT* annot) ;

  // Export the annotation associated with any descendant of <tree> 
  // to MEMOP_ANNOT_WN_MAP. If there is only one annotation, we have two
  // options:
  //   - inline the annotation in POINTS_TO, or 
  //   - allocate annot structure and associate it with corresponding WN
  //
  //  Of couse, the 2nd option is more expensive than the 1st.However, 
  //  we have to do that when this function is invoked by LNO preopt because
  //  POINTS_TOs will be discarded soon make the annotation lost. However, 
  //  the life-time of annotation structures is under control of preopt/wopt.
  //
  //  2nd option should be used when <inline_annot> is set, otherwise, 
  //  1st option is used.
  //
  void Export_annot (WN* tree, const ALIAS_MANAGER*, 
                     BOOL inline_annot, BOOL trace);

  // When active WN_MEMOP_ANNOT_MGR is NULL, we need to discard the "offline" 
  // annotation in that "offline" data structure is supposed to be allocated by 
  // WN_MEMOP_ANNOT_MGR.
  //
  void Discard_offline_annot (WN*, const ALIAS_MANAGER*, BOOL trace);

  void Print (FILE* f, BOOL verbose=FALSE) const;
};

#endif  /* // opt_htable_INCLUDED */
