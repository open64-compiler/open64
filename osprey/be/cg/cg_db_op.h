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


/* ====================================================================
 * ====================================================================
 *
 *  Module: cg_db_op.h
 *  $Revision: 1.2 $
 *  $Date: 02/11/07 23:41:20-00:00 $
 *  $Author: fchow@keyresearch.com $
 *  $Source: /scratch/mee/2.4-65/kpro64-pending/be/cg/SCCS/s.cg_db_op.h $
 *
 *  Revision history:
 *   8-Oct-95 - Original Version
 *
 *  Description:
 *  ============
 *
 *      Definition of private (auxiliary) fields in the OP data
 *      structure for use internally in the if conversion
 *      transformation.  This takes the form of defining the DB_AUX_OP
 *      data structure and defining a set of access macros for its
 *      fields.
 *
 *
 *  Prefixes: DB_AUX_OP   - for direct DB_AUX_OP field access
 *            DB_OP      - for access to DB_AUX_OP fields via a
 *                          containing AUX_OP
 *            OP          - for access to DB_AUX_OP fields via a
 *                          containing OP
 *
 *  Exported type:
 *
 *      typedef db_aux_op DB_AUX_OP
 *
 *  Exported functions:
 *
 *      void DB_OP_Initialize( BB *bb)
 *
 *      void DB_OP_Finalize( BB *bb )
 *
 * ====================================================================
 * ====================================================================
 */

#ifndef cg_db_op_INCLUDED
#define cg_db_op_INCLUDED


typedef struct db_aux_op {
  INT32 flags;
  mINT16 dbnum;
  UINT8  dbcondomega;
  OP *link;
  OP *pref_l1;
  OP *pref_l2;
  TN *condtn;
} DB_AUX_OP;

#define DB_AUX_OP_flags(x)	((x)->flags)
#define DB_AUX_OP_dbnum(x)	((x)->dbnum)
#define DB_AUX_OP_link(x)	((x)->link)
#define DB_AUX_OP_condtn(x)	((x)->condtn)
#define DB_AUX_OP_dbcondomega(x) ((x)->dbcondomega)
#define DB_AUX_OP_pref_l1(x)     ((x)->pref_l1)
#define DB_AUX_OP_pref_l2(x)     ((x)->pref_l2)

#define OP_db_aux_op(o)         ((DB_AUX_OP *)(OP_MAP_Get(DB_AUX_OP_Map, o)))
#define Set_OP_db_aux_op(o,v)   (OP_MAP_Set(DB_AUX_OP_Map, o, v))

#define OP_dbnum(o)		(DB_AUX_OP_dbnum(OP_db_aux_op(o))+0)
#define OPDB_flags(o)		(DB_AUX_OP_flags(OP_db_aux_op(o))+0)
#define OP_link(o)		(DB_AUX_OP_link(OP_db_aux_op(o))+0)
#define OP_condtn(o)		(DB_AUX_OP_condtn(OP_db_aux_op(o))+0)
#define OP_dbcondomega(o)       (DB_AUX_OP_dbcondomega(OP_db_aux_op(o))+0)
#define OP_pref_l1(o)           (DB_AUX_OP_pref_l1(OP_db_aux_op(o))+0)
#define OP_pref_l2(o)           (DB_AUX_OP_pref_l2(OP_db_aux_op(o))+0)

#define Set_OP_dbnum(o,v)	 (DB_AUX_OP_dbnum(OP_db_aux_op(o))=(v))
#define Set_OPDB_flags(o,v)	 (DB_AUX_OP_flags(OP_db_aux_op(o))=(v))
#define Set_OP_link(o,v)	 (DB_AUX_OP_link (OP_db_aux_op(o))=(v))
#define Set_OP_condtn(o,v)	 (DB_AUX_OP_condtn(OP_db_aux_op(o))=(v))
#define Set_OP_dbcondomega(o,v)  (DB_AUX_OP_dbcondomega(OP_db_aux_op(o))=(v))
#define Set_OP_pref_l1(o,v)      (DB_AUX_OP_pref_l1(OP_db_aux_op(o))=(v))
#define Set_OP_pref_l2(o,v)      (DB_AUX_OP_pref_l2(OP_db_aux_op(o))=(v))

/* Select was generated via negated condition, default is
 * the second operand instead of the third.
 */
#define OPDB_MASK_NSELCOND	 0x00001
#define OP_nselcond(o)		(OPDB_flags(o) & OPDB_MASK_NSELCOND)
#define Set_OP_nselcond(o)	(DB_AUX_OP_flags(OP_db_aux_op(o)) |= OPDB_MASK_NSELCOND)
#define Reset_OP_nselcond(o)	(DB_AUX_OP_flags(OP_db_aux_op(o)) &= ~OPDB_MASK_NSELCOND)
#define Invert_OP_nselcond(o)   (DB_AUX_OP_flags(OP_db_aux_op(o)) ^= OPDB_MASK_NSELCOND)

/* OP generates a guard (if) condition
 */
#define OPDB_MASK_GENICOND	 0x00002
#define OP_genicond(o)		(OPDB_flags(o) & OPDB_MASK_GENICOND)
#define Set_OP_genicond(o)	(DB_AUX_OP_flags(OP_db_aux_op(o)) |= OPDB_MASK_GENICOND)
#define Reset_OP_genicond(o)	(DB_AUX_OP_flags(OP_db_aux_op(o)) &= ~OPDB_MASK_GENICOND)

/* Allow cond. load to fault.
 */
#define OPDB_MASK_NOFAULT	 0x00004
#define OP_nofault(o)		(OPDB_flags(o) & OPDB_MASK_NOFAULT)
#define Set_OP_nofault(o)	(DB_AUX_OP_flags(OP_db_aux_op(o)) |= OPDB_MASK_NOFAULT)
#define Reset_OP_nofault(o)	(DB_AUX_OP_flags(OP_db_aux_op(o)) &= ~OPDB_MASK_NOFAULT)

/* OP is a CG phase generated select.
 */
#define OPDB_MASK_CGSELECT	 0x00010
#define OP_cgselect(o)		(OPDB_flags(o) & OPDB_MASK_CGSELECT)
#define Set_OP_cgselect(o)	(DB_AUX_OP_flags(OP_db_aux_op(o)) |= OPDB_MASK_CGSELECT)
#define Reset_OP_cgselect(o)	(DB_AUX_OP_flags(OP_db_aux_op(o)) &= ~OPDB_MASK_CGSELECT)

/* OP is a select generated from a cmove input to LA.  Implies
 * _cgselect, but also implies that this OP need not have some of the nice
 * properties that selects generated by IF conversion have.
 */
#define OPDB_MASK_FROMCMOVE	 0x00020
#define OP_fromcmove(o)		(OPDB_flags(o) & OPDB_MASK_FROMCMOVE)
#define Set_OP_fromcmove(o)	(DB_AUX_OP_flags(OP_db_aux_op(o)) |= OPDB_MASK_FROMCMOVE)
#define Reset_OP_fromcmove(o)	(DB_AUX_OP_flags(OP_db_aux_op(o)) &= ~OPDB_MASK_FROMCMOVE)

/* OP is a CG phase CFCC op.
 */
#define OPDB_MASK_CFCCOP	 0x00040
#define OP_CFCCOP(o)		(OPDB_flags(o) & OPDB_MASK_CFCCOP)
#define Set_OP_CFCCOP(o)	(DB_AUX_OP_flags(OP_db_aux_op(o)) |= OPDB_MASK_CFCCOP)
#define Reset_OP_CFCCOP(o)	(DB_AUX_OP_flags(OP_db_aux_op(o)) &= ~OPDB_MASK_CFCCOP)

/* CG phase has already inserted the necessary copies for an sfcc
 */
#define OPDB_MASK_COPYSFCC  	 0x00080
#define OP_copysfcc(o)		(OPDB_flags(o) & OPDB_MASK_COPYSFCC)
#define Set_OP_copysfcc(o)	(DB_AUX_OP_flags(OP_db_aux_op(o)) |= OPDB_MASK_COPYSFCC)
#define Reset_OP_copysfcc(o)	(DB_AUX_OP_flags(OP_db_aux_op(o)) &= ~OPDB_MASK_COPYSFCC)

/* OP returns a TN_is_fpu_int result and is already safe
 */
#define OPDB_MASK_SAFE  	 0x00100
#define OP_safe(o)		(OPDB_flags(o) & OPDB_MASK_SAFE)
#define Set_OP_safe(o)	        (DB_AUX_OP_flags(OP_db_aux_op(o)) |= OPDB_MASK_SAFE)
#define Reset_OP_safe(o)	(DB_AUX_OP_flags(OP_db_aux_op(o)) &= ~OPDB_MASK_SAFE)

/* select generated to guard an op by Transform_Conditional_Ops()
 */
#define OPDB_MASK_TCGUARD  	 0x00200
#define OP_tcguard(o)		(OPDB_flags(o) & OPDB_MASK_TCGUARD)
#define Set_OP_tcguard(o)	(DB_AUX_OP_flags(OP_db_aux_op(o)) |= OPDB_MASK_TCGUARD)
#define Reset_OP_tcguard(o)	(DB_AUX_OP_flags(OP_db_aux_op(o)) &= ~OPDB_MASK_TCGUARD)


/* This is equivalent to OP_invguard() but is stored in the DB_AUX_OP map
 */
#define OPDB_MASK_INVCONDTN  	 0x00400
#define OP_invcondtn(o)		((OPDB_flags(o) & OPDB_MASK_INVCONDTN) ? TRUE : FALSE)
#define Set_OP_invcondtn(o)	(DB_AUX_OP_flags(OP_db_aux_op(o)) |= OPDB_MASK_INVCONDTN)
#define Reset_OP_invcondtn(o)	(DB_AUX_OP_flags(OP_db_aux_op(o)) &= ~OPDB_MASK_INVCONDTN)

/* Temporary flag in the DB_AUX_OP map
 */
#define OPDB_MASK_FLAG1  	0x00800
#define OP_dbflag1(o)		(OPDB_flags(o) & OPDB_MASK_FLAG1)
#define Set_OP_dbflag1(o)	(DB_AUX_OP_flags(OP_db_aux_op(o)) |= OPDB_MASK_FLAG1)
#define Reset_OP_dbflag1(o)	(DB_AUX_OP_flags(OP_db_aux_op(o)) &= ~OPDB_MASK_FLAG1)

/* Cross iteration cse flag in the DB_AUX_OP map
 */
#define OPDB_MASK_CICSE  	0x01000
#define OP_cicse(o)		(OPDB_flags(o) & OPDB_MASK_CICSE)
#define Set_OP_cicse(o)	        (DB_AUX_OP_flags(OP_db_aux_op(o)) |= OPDB_MASK_CICSE)
#define Reset_OP_cicse(o)	(DB_AUX_OP_flags(OP_db_aux_op(o)) &= ~OPDB_MASK_CICSE)

/* Selects are sometimes being generated in terms of negated conditions,
 * rather than having a negated condition IR OP code, we just reverse
 * the operands.
 */
#define OP_seldefopndndx(op)	(OP_nselcond(op)?1:2)
#define OP_selndfopndndx(op)	(OP_nselcond(op)?2:1)

#define OP_seldefopnd(op)	OP_opnd(op,OP_seldefopndndx(op))
#define OP_seldefomega(op)	OP_omega(op,OP_seldefopndndx(op))

#define OP_selndfopnd(op)	OP_opnd(op,OP_selndfopndndx(op))
#define OP_selndfomega(op)	OP_omega(op,OP_selndfopndndx(op))

#define OP_selcondopnd(op) 	OP_opnd(op,0)
#define OP_selcondomega(op) 	OP_omega(op,0)

#define Set_OP_seldefopnd(op,v)	 Set_OP_opnd(op,OP_seldefopndndx(op),v)
#define Set_OP_selndfopnd(op,v)	 Set_OP_opnd(op,OP_selndfopndndx(op),v)
#define Set_OP_selcondopnd(op,v) Set_OP_opnd(op,0,v)

#define Set_OP_seldefomega(op,v)   Set_OP_omega(op,OP_seldefopndndx(op),v)
#define Set_OP_selndfomega(op,v)   Set_OP_omega(op,OP_selndfopndndx(op),v)
#define Set_OP_selcondomega(op,v)  Set_OP_omega(op,0,v)

/* Is the guard condition for the OP inverted?
 */
#define OP_invguard(op) ((DBnegateicr(OP_dbnum(op))) ? TRUE : FALSE)

/* Guard condition TN[omega] for OP.
 */
#define OP_guard(op) DoBodyICR(OP_dbnum(op))
#define OP_guard_omega(op) DoBodyICRomega(OP_dbnum(op))

extern OP_MAP DB_AUX_OP_Map;

extern void DB_Initialize(void);
extern void DB_Finalize(void);
extern void DB_Initialize_OP( OP *op, INT16 dbnum );
extern void DB_Initialize_BB( BB *bb, INT16 dbnum );
extern BOOL Is_DB_OP_Init( OP *op);
extern void DB_Copy_Aux_OP(OP *dest, OP *src);
extern void DB_Trace_BB(BB *bb);
extern void DB_Rename_Cond_TN_Init(BB *bb);
extern void DB_Rename_Cond_TN(BB *bb, TN *dest, TN *src, UINT8 omega);
extern void DB_Rename_Cond_TN_OPs(BB *bb);
extern void DB_Rename_Cond_TN_Finish( void );
extern TN *DB_Get_ICR(BB *bb, INT16 dbnum, UINT8 unrolling);
extern TN *DB_Get_BRCC(BB *bb, INT16 dbnum, UINT8 unrolling);

#endif /* cg_db_op_INCLUDED */



