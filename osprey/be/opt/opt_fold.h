//-*-c++-*-

/*
 * Copyright 2005, 2006 PathScale, Inc.  All Rights Reserved.
 */

// ====================================================================
// ====================================================================
//
// Module: opt_fold.h
// $Revision: 1.1.1.1 $
// $Date: 2005/10/21 19:00:00 $
// $Author: marcel $
// $Source: /proj/osprey/CVS/open64/osprey1.0/be/opt/opt_fold.h,v $
//
// Revision history:
//  31-MAY-95 dahl - Original Version
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
//	defines FOLD class and member functions for the interface to
//	the simplifier called by copy propagation
//
// ====================================================================
// ====================================================================


#ifndef opt_fold_INCLUDED
#define opt_fold_INCLUDED	"opt_fold.h"
#ifdef _KEEP_RCS_ID
static char *opt_foldrcs_id = opt_fold_INCLUDED"$Revision: 1.4 $";
#endif /* _KEEP_RCS_ID */

#ifndef opt_htable_INCLUDED
#include "opt_htable.h"		// for CODEREP
#endif

#ifndef config_INCLUDED
#include "config.h"		// for Force_IEEE_Comparisons
#endif

class FOLD
{
public:
  FOLD(void); // sets tracing flag

  // main entry points for folding to a constant
  CODEREP *Fold_Expr(CODEREP *);	// one level
  CODEREP *Fold_Tree(CODEREP *);	// entire tree

private:
  // a pointer is returned
#define NOHASH NULL	// did nothing
#define CONST  NULL	// folded to a constant

  // debugging print
  void Print(const char *, const CODEREP *);

  // routines to interface to simplifier
  CODEREP *CR_Simplify_Tree(CODEREP *);
  CODEREP *CR_Simplify_Expr(CODEREP *);
#ifdef KEY
  CODEREP *CR_Simplify_Iload(CODEREP *);
#endif

  // allow expr simplifier to look one level past CVT and CVTL
  BOOL check_convert(CODEREP *, CODEREP **, INT);

  // don't carry along or maintain any maps
  WN_MAP CR_SimpParentMap(void)		{ return (WN_MAP)0; }
};

// type definitions for wn_simp_code.h, a simpnode is a CODEREP not a WN now
typedef CODEREP * simpnode;

/* Accessors */
#define SIMPNODE_opcode			CR_opcode
#define SIMPNODE_operator		CR_operator
#define SIMPNODE_rtype(x)		(x)->Dtyp()
#define SIMPNODE_desc(x)		(x)->Dsctyp()
#define SIMPNODE_load_offset(x)		(x)->Offset()
#define SIMPNODE_cvtl_bits(x)		(x)->Offset()
#define SIMPNODE_st			CR_st
#define SIMPNODE_st_idx(x)              ST_st_idx(CR_st(x))
#define SIMPNODE_ty			CR_ty
#define SIMPNODE_object_ty(x)           (x)->object_ty()
#define SIMPNODE_load_addr_ty(x)        (x)->lod_addr_ty()
#define SIMPNODE_kid0			CR_kid0
// kid 1 means nothing to IVAR, only to OP
#define SIMPNODE_kid1(x)		(((x)->Kind() == CK_OP) ? \
					 (x)->Opnd(1) : NULL)
#define SIMPNODE_kid			CR_kid
#define SIMPNODE_kid_count		CR_kid_count
#define SIMPNODE_element_size(x)	(x)->Elm_siz()
#define SIMPNODE_idname_offset(x)	(x)->Offset()
#define SIMPNODE_lda_offset(x)		(x)->Offset()
#define SIMPNODE_label_number(x)	(x)->Offset() 
#define SIMPNODE_num_dim(x)		(x)->Num_dim()
#define SIMPNODE_array_base(x)		(x)->Opnd(0)
#define SIMPNODE_array_index(x,y)	(x)->Opnd((x)->Num_dim()+y+1)
#define SIMPNODE_array_dim(x,y)		(x)->Opnd(y+1)
#define SIMPNODE_intrinsic(x)		(x)->Intrinsic()
#define SIMPNODE_const_val(x)		(x)->Const_val()
#define SIMPNODE_fconst_val(x)		(x)->Const_ftcon2(fold_htable)
#define SIMPNODE_field_id(x)		(x)->Field_id()
#define SIMPNODE_i_field_id(x)		(x)->I_field_id()
#define SIMPNODE_bit_offset(x)		(x)->Bit_offset()
#define SIMPNODE_i_bit_offset(x)	(x)->I_bit_offset()
#define SIMPNODE_op_bit_offset(x)	(x)->Op_bit_offset()
#define SIMPNODE_op_bit_size(x)		(x)->Op_bit_size()
#define SIMPNODE_is_volatile(x)         (x)->Is_var_volatile()
/* on/off switch and trace file */
#define SIMPNODE_enable			WOPT_Enable_CRSIMP
#define TRACEFILE			TFile

/* Functions */
#define SIMPNODE_SimpCreateExp1		CR_SimpCreateExp1
#define SIMPNODE_SimpCreateExp2		CR_SimpCreateExp2
#define SIMPNODE_SimpCreateExp3		CR_SimpCreateExp3
#define SIMPNODE_SimpCreateCvtl		CR_SimpCreateCvtl
#define SIMPNODE_SimpCreateExtract      CR_SimpCreateExtract
#define SIMPNODE_SimpCreateDeposit      CR_SimpCreateDeposit
#define SIMPNODE_TREE_DELETE(x)		(x)->DecUsecnt_rec()
#define SIMPNODE_DELETE(x)		(x)->DecUsecnt()
#define SIMPNODE_CopyNode(x)		(((x)->Coderep_id() == 0) ? \
					 fold_htable->Rehash(x) : (x))
#define SIMPNODE_CreateIntconst		CR_CreateIntconst
#define SIMPNODE_CreateFloatconstFromTcon CR_CreateFPconst
#define SIMPNODE_Simplify_Initialize	CR_Simplify_Initialize
#define SIMPNODE_Compare_Symbols	CR_Compare_Symbols

extern simpnode SIMPNODE_SimplifyExp1(OPCODE, simpnode);
extern simpnode SIMPNODE_SimplifyExp2(OPCODE, simpnode, simpnode);
extern simpnode SIMPNODE_SimplifyExp3(OPCODE, simpnode, simpnode, simpnode);
extern simpnode SIMPNODE_SimplifyCvtl(OPCODE, INT16, simpnode);
extern simpnode SIMPNODE_SimplifyIntrinsic(OPCODE, UINT32, INT32, simpnode *);

// to interface for coderep rehash mechanism
extern void  Initialize_CR_simp(CODEMAP*);
extern INT32 CR_Compare_Symbols(CODEREP*, CODEREP*);
extern INT32 CR_Compare_Trees(CODEREP*, CODEREP*);

#endif  // opt_fold_INCLUDED
