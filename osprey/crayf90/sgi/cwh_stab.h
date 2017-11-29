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
 * Module: cwh_stab.h
 * $Revision$
 * $Date$
 * $Author$
 * $Source$
 *
 * Revision history:
 *  dd-mmm-95 - Original Version
 *
 * Description: Exports interfaces from cwh_stab.c (symbol tables).
 *              Most descriptions are in cwh_stab.{c,i}
 *
 * ====================================================================
 * ====================================================================
 */

#ifndef CWH_STAB_INCLUDED
#define CWH_STAB_INCLUDED

#ifdef _KEEP_RCS_ID
static char *rcs_id = "$Source$ $Revision$";
#endif /* _KEEP_RCS_ID */


/* --------------------------------------------------------
   TAGs for handing back items from fei_object and fei_seg 
   to PGDCS. PDGCS stores them and gives them back later.
   We need to know what we're given.
*/

enum is_form  {
  is_UNDEF=0,
  is_ST,  
  is_WN,
  is_CONST,
  is_SCLASS,
  is_PCONST,   /* Pattern constant */
  is_SCONST,   /* Pattern constant for a string */
  is_LIST      /* LIST from fei_name            */
} ;

typedef struct al {
  void * item  ;
  TY_IDX ty    ;       /* TY of item, restricted, see fn below*/
  enum is_form form ;
  struct al * next ;
}  STB_pkt;

#define cast_to_SCLASS(x) ((ST_SCLASS) (INTPS)(x)) 
#define cast_to_LIST(x) ((LIST *) (void *)(x)) 

extern STB_pkt * cwh_stab_packet(void * thing, enum is_form fm) ;
extern STB_pkt * cwh_stab_packet_typed(void * thing, enum is_form fm, TY_IDX  ty) ;

#define IS_FORMAL(s) (((ST_sclass(s) == SCLASS_FORMAL) ||         \
		       (ST_sclass(s) == SCLASS_FORMAL_REF)) &&    \
		      (!Has_Base_Block(st)))

#define IS_COMMON(s) ((ST_sclass(s) == SCLASS_COMMON) ||  \
		      (ST_sclass(s) == SCLASS_DGLOBAL)) 

#define IS_AUTO_OR_FORMAL(s) ((ST_sclass(s) == SCLASS_FORMAL) ||         \
		              (ST_sclass(s) == SCLASS_FORMAL_REF) ||     \
		              (ST_sclass(s) == SCLASS_AUTO))  
			      
/*------------------------------------------------------ 

 globals set in   cwh_stab.c , see   cwh_stab.i for details 

*/

extern ST * Procedure_ST ; 
extern ST * Altaddress_ST ; 
extern PREG_det preg_for_distribute;
extern BOOL  cwh_stab_pu_has_globals; /* Were any global symbols seen in the PU */
extern INT32 cwh_assign_label_id;


/*------------------------------------------------------

  general function definitions 

*/

extern WN *     cwh_stab_const(ST *st) ;
extern void     cwh_stab_end_procs(void);
extern void     cwh_stab_add_pragma(ST *st, WN_PRAGMA_ACCESSED_FLAGS flag );
extern void     cwh_stab_set_symtab(ST *st) ;
extern ST *     cwh_stab_const_ST(WN *wn) ;
extern ST *     cwh_stab_address_temp_ST(const char * name, TY_IDX  ty , BOOL uniq);
extern ST *     cwh_stab_temp_ST(TY_IDX ty, const char * name) ;
extern WN *     cwh_load_distribute_temp(void);
extern ST *     cwh_stab_main_ST(void) ;
extern TY_IDX   cwh_stab_altentry_TY(ST *st, BOOL expr);


extern void   New_Auxst (SYMTAB_IDX level, ST_IDX idx);
extern ST *   F90_New_ST (SYMTAB_IDX level);
extern void   cwh_stab_init_auxst_tab_size(void);
extern void   cwh_stab_set_tylist_for_entries(ST *st);
extern LABEL& F90_New_LABEL(SYMTAB_IDX scope, LABEL_IDX& label_idx);
extern void   cwh_stab_emit_commons_and_equivalences(SYMTAB_IDX level);
extern ST *   cwh_stab_mk_fn_0args(const char *name, ST_EXPORT eclass,SYMTAB_IDX level,TY_IDX rty) ;

#endif /* CWH_STAB_INCLUDED */


