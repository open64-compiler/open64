/*
 *  Copyright (C) 2006. QLogic Corporation. All Rights Reserved.
 */

/*
 * Copyright 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
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


/* ====================================================================
 * ====================================================================
 *
 * $Revision: 1.8 $
 * $Date: 05/01/13 22:11:47-08:00 $
 * $Author: scorrell@soapstone.internal.keyresearch.com $
 * $Source: crayf90/sgi/SCCS/s.cwh_auxst.cxx $
 *
 * Revision history:
 *  dd-mmm-95 - Original Version
 *
 * Description: This provides an auxiliary data structure for an ST
 *              to hang odds and ends on. There's an AUXST table associated
 *              with each level of the symtab, and it's deleted when
 *              the symtab is deleted. The definition is in cwh_auxst.i
 *              Most external entry points are defined via an ST and
 *              perhaps the name of a list.  The AUXST for an ST is 
 *              allocated, indirectly, by calls to cwh_auxst_find.
 *
 *              There are allocation/deallocation routines, some routines
 *              to deal with lists, some to extract details from the AUXST,
 *              and a few routines to update procedures STs with the 
 *              accumulated detail of arguments and types.
 * 
 *              Dump routines are at the end.
 *
 * ====================================================================
 * ====================================================================
 */
/*REFERENCED*/
static const char *source_file = __FILE__;

#ifdef _KEEP_RCS_ID
static char *rcs_id = "$Source: crayf90/sgi/SCCS/s.cwh_auxst.cxx $ $Revision: 1.8 $";
#endif /* _KEEP_RCS_ID */

/* general compiler includes */

#include "defs.h"
#include "glob.h"  
#include "stab.h"
#include "strtab.h"
#include "errors.h"
#include "targ_const.h"
#include "config_targ.h"
#include "const.h"
#include "wn.h"
#include "cxx_memory.h"
#include <stdio.h>

/* conversion includes */

#include "cwh_defines.h"
#include "cwh_preg.h"
#include "cwh_types.h"
#include "cwh_addr.h"
#include "cwh_auxst.h"
#include "cwh_auxst.i"
#include "sgi_cmd_line.h"

AUXST_TAB *Auxst_tab;

/*===================================================
 *
 * cwh_auxst_register_table
 *
 * For the current level, register an auxst table that
 * will automatically grow as the St_table grows.
 * convenient for a label table too.
 * 
 ====================================================
*/
extern void
cwh_auxst_register_table(void)
{
   Auxst_tab[CURRENT_SYMTAB].Auxst_table = CXX_NEW(AUXST_PTR_ARRAY(FE_Mempool), FE_Mempool);
   Scope_tab[CURRENT_SYMTAB].st_tab->Register(*(Auxst_tab[CURRENT_SYMTAB].Auxst_table));

   if (CURRENT_SYMTAB != GLOBAL_SYMTAB)
      Scope_tab[CURRENT_SYMTAB].label_tab->Register(Auxlabel_Table);

}

/*===================================================
 *
 * cwh_auxst_un_register_table
 *
 * Unregister the auxst table and auxlabel tables.
 *
 ====================================================
*/
extern void
cwh_auxst_un_register_table(void)
{
   Scope_tab[CURRENT_SYMTAB].st_tab->Un_register(*(Auxst_tab[CURRENT_SYMTAB].Auxst_table));
   CXX_DELETE(Auxst_tab[CURRENT_SYMTAB].Auxst_table, FE_Mempool);

   if (CURRENT_SYMTAB != GLOBAL_SYMTAB)
     Scope_tab[CURRENT_SYMTAB].label_tab->Un_register(Auxlabel_Table); 
}

/*===================================================
 *
 * cwh_auxst_alloc_container_table
 *
 * This allocates a table analogous to the scope table
 * in common/com; we can index into this table by level
 * and then get to the auxst table for that level; 
 * however, the entire mechanism should be transparent
 * and we should not ever need to explicitly refer to 
 * Auxst_tab. 
 *
 ====================================================
*/
void 
cwh_auxst_alloc_container_table(void) 
{
  Auxst_tab = (AUXST_TAB *) MEM_POOL_Alloc (FE_Mempool, 
			MAX_AUXST_LEVEL * sizeof(AUXST_TAB));
}

/*===================================================
 *
 * cwh_auxst_find
 *
 * Allocate or find field which holds details 
 * associated with this ST. The ST_temp holds the
 * pointer. The create flag builds auxst field, if absent.
 *
 ====================================================
*/
static AUXST *
cwh_auxst_find(ST *st, BOOL create)
{
  AUXST * o ;

  o = Auxst_Table[ST_st_idx(st)];

  if (o == NULL) { 
    if (create) {
      o = (AUXST *) malloc(sizeof(AUXST));

      bzero(o,sizeof(AUXST));

      AUXST_OwningST(o)= st ;
      AUXST_Next(o)    = Top_Auxst[ST_level(st)];
      AUXST_AssignId(o) = -1 ;
      AUXST_DstrPreg(o).preg = -1;

      USRCPOS_clear(AUXST_SrcPos(o));

      Auxst_Table[ST_st_idx(st)] = o;
      Top_Auxst[ST_level(st)] = o ;
    }
  }
  return(o);
}

/*===================================================
 *
 * cwh_auxst_clear
 *
 * set the Auxst_Table entry for incoming st to null.
 *
 ====================================================
*/
extern void
cwh_auxst_clear(ST *st)
{
   Auxst_Table[ST_st_idx(st)] = NULL;
}

/*===================================================
 *
 * cwh_auxst_free
 *
 * Free any AUXSTs, and set the corresponding 
 * ST_temp to NULL; 
 *
 ====================================================
*/
extern void
cwh_auxst_free(void)
{
  AUXST *o,*n;
  LIST  *l   ;


  o = Top_Auxst[CURRENT_SYMTAB];

  while (o != NULL ) {

    AUXST_Pragma(o) = NULL ;
    n = AUXST_Next(o);

    ST *st = AUXST_OwningST(o);

    Auxst_Table[ST_st_idx(st)] = NULL;

    l = cwh_auxst_find_list(o,l_COMLIST);
    cwh_auxst_free_list(&l);

#ifdef KEY /* Bug 5271 */
    l = cwh_auxst_find_list(o,l_PU_COMLIST);
    cwh_auxst_free_list(&l);
#endif /* KEY Bug 5271 */

    l = cwh_auxst_find_list(o,l_ALTENTRY);
    cwh_auxst_free_list(&l);

    l = cwh_auxst_find_list(o,l_RETURN_TEMPS);
    cwh_auxst_free_list(&l);

    l = cwh_auxst_find_list(o,l_NAMELIST);
    cwh_auxst_free_list(&l);

    l = cwh_auxst_find_list(o,l_SPLITLIST);
    cwh_auxst_free_list(&l);

    l = cwh_auxst_find_list(o,l_EQVLIST);
    cwh_auxst_free_list(&l);

    l = cwh_auxst_find_list(o,l_DST_COMLIST);
    cwh_auxst_free_list(&l);

    l = cwh_auxst_find_list(o,l_DST_PARMLIST);
    cwh_auxst_free_list(&l);

    if (AUXST_Stem(o) != NULL)
      free (AUXST_Stem(o)) ;

    if (AUXST_Dummies(o) != NULL) {
      if (AUXST_Dummies(o)->arglist != NULL)
	free (AUXST_Dummies(o)->arglist) ;
      free (AUXST_Dummies(o)) ;
    }
    free(o);
    o = n;
  }

  Top_Auxst[CURRENT_SYMTAB] = NULL;
}


/*===================================================
 *
 * cwh_auxst_clear_per_PU
 *
 * Clear any per_PU info associated with a global 
 * or host auxst. For now, this is just the pragma flag.
 * and the label table for assigned/computed gotos.
 *
 ====================================================
*/
extern void
cwh_auxst_clear_per_PU(void)
{
  AUXST * o ;
  SYMTAB_IDX s = CURRENT_SYMTAB;

  while (s >= GLOBAL_SYMTAB) {
    o = Top_Auxst[s] ;
    while (o != NULL ) {
      AUXST_Pragma(o) = NULL;
      o = AUXST_Next(o);
    }
    s-- ;
  }

  Auxlabel_Table.Clear(); 
}

/*===================================================
 *
 * cwh_auxst_get_list
 *
 * Given an ST address and a enum which
 * identifies which list to get, return a pointer
 * to the list.
 *
 ====================================================
*/
extern LIST *
cwh_auxst_get_list(ST * st,enum list_name list)
{
  LIST * l  = NULL;
  AUXST * o = cwh_auxst_find(st,FALSE);

  if (o)
    l = cwh_auxst_find_list(o,list);

  return l;
}

/*===================================================
 *
 * cwh_auxst_find_list
 *
 * Given an AUXST address and a enum which
 * identifies which list to get, return a pointer
 * to the list.
 *
 ====================================================
*/
static LIST *
cwh_auxst_find_list(AUXST * o, enum list_name list)
{
#ifdef KEY /* Bug 10177 */
  LIST *l = 0;
#else /* KEY Bug 10177 */
  LIST *l ;
#endif /* KEY Bug 10177 */

  switch (list) {
  case l_COMLIST:
    l = AUXST_Commons(o);
    break;

#ifdef KEY /* Bug 5271 */
  case l_PU_COMLIST:
    l = AUXST_PU_Commons(o);
    break;
#endif /* KEY Bug 5271 */

  case l_ALTENTRY:
    l = AUXST_Altentries(o);
    break;

  case l_NAMELIST:
    l = AUXST_Namelist(o);
    break;

  case l_RETURN_TEMPS:
    l = AUXST_RtnTemps(o);
    break;

  case l_SPLITLIST:
    l = AUXST_SplitCommons(o);
    break;

  case l_EQVLIST:
    l = AUXST_Equivs(o);
    break;

  case l_DST_COMLIST:
    l = AUXST_Dstcomlist(o);
    break;

  case l_DST_PARMLIST:
    l = AUXST_Dstparmlist(o);
    break;

  default:
    DevAssert((0),("list?"));
  }

  return l;
}

/*===================================================
 *
 * cwh_auxst_add_item
 *
 * add ST to AUXST of parent ST.
 *
 *  a) the list of elements in a COMMON block ST's 
 *     for DST info and full_split_common. If full_split
 *     then are order the STs by offset. 
 *
 *  b) a list of alternate entry points associated 
 *     procedure.
 *
 *  c) a list of Namelist items
 *  d) a list of equivalence items
 *  e) a list of alternate entry point return temp STs
 *  f) a list of parameters needed for DST info        
 *
 ====================================================
*/
extern void
cwh_auxst_add_item(ST * parent, ST *st, enum list_name list)
{
  AUXST   *o ;
  LIST    *c ;  
  BOOL     b ;

  b = FALSE;

  if (list == l_COMLIST) 
    b = TRUE;
#ifdef KEY /* Bug 5271 */
  if (list == l_PU_COMLIST) 
    b = TRUE;
#endif /* KEY Bug 5271 */

  o = cwh_auxst_find(parent,TRUE);
  c = cwh_auxst_find_list(o, list);

  cwh_auxst_add_to_list(&c,st,b);
}

/*===================================================
 *
 * cwh_auxst_find_item
 *
 * Does this name match any of the ST's  on 
 * the given LIST? 
 *
 ====================================================
*/
extern ST *
cwh_auxst_find_item(LIST *l, const char * name)
{
  ITEM  *t ;
  ST    *st;

  st = NULL ;
  if (l == NULL) return (NULL);
  t  = L_first(l) ;

  while (t != NULL) {
    if (strcmp(ST_name(I_element(t)),name) == 0) {
      st = I_element(t);
      break ;
    }
    t = I_next(t);
  }

  return(st);
}

/*===================================================
 *
 * cwh_auxst_add_list
 *
 * add list to AUXST of parent ST - only used for
 * namelist just now, but other enums are provided
 * (later, commented out...)
 *
 ====================================================
*/
extern void
cwh_auxst_add_list(ST * parent, LIST *l, enum list_name list)
{
  AUXST   *o ;

  o = cwh_auxst_find(parent,TRUE);

  switch (list) {
  case l_NAMELIST:
    *AUXST_Namelist(o) = *l;
    break;


  default:
    DevAssert((0),("list?"));

  }
}

/*===================================================
 *
 * cwh_auxst_next_element
 *
 * Finds the ST of the next item within a list 
 * in a parent ST's auxiliary info.
 *
 * If the ITEM argument is NULL, the first element is 
 * returned. If there are no more elements the 
 * result is NULL, otherwise the next item. The LIST
 * argument is examined & the correct list established.
 *
 ====================================================
*/
extern ITEM *
cwh_auxst_next_element(ST * parent, ITEM *i, enum list_name list)
{
  AUXST        *o;
  LIST         *l;
  
  if (i == NULL) { 
    o = cwh_auxst_find(parent,TRUE);

    if (o != NULL) {
      l = cwh_auxst_find_list(o,list);
      i = L_first(l);
    }
 } else
   i = I_next(i) ;

  return (i);
}

#ifdef KEY /* Bug 5271 */
/*===================================================
 *
 * cwh_clear_common_list
 *
 * Remove common variables left over from earlier program units so that the
 * Dwarf symbol table doesn't "accumulate" them as compilation proceeds
 *
 ====================================================
*/
extern void
cwh_clear_PU_common_list(ST *st)
{
    AUXST *o = cwh_auxst_find(st,TRUE);
    LIST *c = cwh_auxst_find_list(o, l_PU_COMLIST);
    LIST *d = c;
    cwh_auxst_free_list(&c);
    // cwh_auxst_free_list sets its arg ptr to null, but leaves the pointee
    // containing stale values!
    d->first = d->last = NULL;
    d->nitems = 0;
}
#endif /* KEY Bug 5271 */

/*===================================================
 *
 * cwh_auxst_add_to_list
 *
 * Add this ST to the list provided. A pointer to
 * the list is handed in - if NULL, the list is 
 * created. A pointer to the last item added is 
 * returned. If order is true, the list is ordered
 * by ST_ofst. (First == low).
 *
 ====================================================
*/
extern ITEM *
cwh_auxst_add_to_list(LIST ** lp, ST *st, BOOL order)
{
  ITEM * i;
  ITEM * n;
  ITEM * p;
  LIST * l;

  if (*lp == NULL) {
    *lp = (LIST *) malloc(sizeof(LIST));
    l = *lp ;
    L_first(l) = NULL ;
    L_last(l)  = NULL ;
    L_num(l)   = 0 ; 
  } 
  
  l = *lp ;
  i = (ITEM *)malloc(sizeof(ITEM))  ;

  I_element(i) = st;
  I_next(i) = NULL;

  if ( order ) {
    n = L_first(l) ;
    p = NULL ;
    
    while (n != NULL) {

      if (ST_ofst(I_element(n)) > ST_ofst(st)) {
  	I_next(i) = n;

	if (L_first(l) == n)
	  L_first(l) = i ;
	else 
	  I_next(p) = i;

	break;
      }
      p = n ;
      n = I_next(n);
    }
    
    if (L_first(l) == NULL) 
      L_first(l) = i;

    if (L_last(l) == NULL)
      L_last(l) = i;

    if (L_last(l) == p) {
      I_next(L_last(l)) = i;
      L_last(l) = i;
    }

  } else {

    if (L_first(l) == NULL) 
      L_first(l) = i;

    if (L_last(l) != NULL)
      I_next(L_last(l)) = i ;

    L_last(l) = i;
  }

  L_num(l) ++ ;

  return i;
}


/*===================================================
 *
 * cwh_auxst_free_list
 *
 * Free a LIST of items. Clears the LIST pointer.
 *
 ====================================================
*/
extern void
cwh_auxst_free_list (LIST ** lp)
{
  ITEM *i;
  ITEM *n;
  LIST *l;

  if (*lp != NULL) {
    l = *lp ;
    
    i = L_first(l) ;

    while (i != NULL) {
      n = I_next(i);
      free(i) ;
      i = n ;
    }
      
    *lp = NULL ;
  }
}

/*===================================================
 *
 * cwh_auxst_set_flag
 *
 * Set the given flag to TRUE or FALSE. Allocate
 * the AUXST if required. Invoked via macro (cwh_stab.h)
 *
 ====================================================
*/
extern void
cwh_auxst_set_flag(ST * st, enum flags_a f, BOOL val)
{
  AUXST *o ;

  o = cwh_auxst_find(st,TRUE);

  if (val)
    Set_AUXST_Flag(o,f);
  else
    Clear_AUXST_Flag(o,f);
}

/*===================================================
 *
 * cwh_auxst_read_flag
 *
 * Read the given flag in the AUXST. If no AUXST
 * returns F. Invoked via macro (cwh_auxst.h)
 *
 ====================================================
*/
extern BOOL
cwh_auxst_read_flag(ST * st, enum flags_a f)
{
  AUXST *o   ;
  BOOL  res = FALSE  ;

  res = FALSE;

  o = cwh_auxst_find(st,FALSE);

  if (o != NULL) 
    res = AUXST_Flag(o,f);

  return res ;
}

/*================================================================
 *
 * Set_ST_auxst_data_info
 *
 * Set data_info ptr 
 *
 *================================================================
 */
extern void 
Set_ST_auxst_data_info(ST *st, data_info_s * data_info)
{
   AUXST * o ;
   
   o = cwh_auxst_find(st,TRUE);
   AUXST_DataInfo(o) = data_info;
   return ;
}
/*================================================================
 *
 * ST_auxst_data_info
 *
 * retreive data_info ptr
 *
 *================================================================
 */
extern data_info_s *
ST_auxst_data_info(ST *st)
{
   AUXST * o ;
   
   o = cwh_auxst_find(st,FALSE);
   if (o) {
      return AUXST_DataInfo(o);
   } else {
      return (NULL);
   }
}

/*===================================================
 *
 * cwh_auxst_alloc_proc_entry
 *
 * For the entry point ST, allocate
 * a DUMMIES structure to record the argument list. 
 * Allocate 2 entries per dummy, in case they are
 * character dummies with the length at the end. 
 * Set the character_args_seen to point after all
 * regular dummies.
 * 
 * Record all entry point ST's on a list, so we
 * can refer to a TEXT symbol if possible.
 *
 * Set EP_Current to this entry point..
 *
 ====================================================
*/
extern void
cwh_auxst_alloc_proc_entry(ST *st,INT32 num_dum_args, TY_IDX ret_type)
{
  DUMMIES *p ;
  AUXST   *o ;

  o = cwh_auxst_find(st,TRUE);
  p = cwh_auxst_find_entry(st);

  if (p == NULL) 
    p = AUXST_Dummies(o) = (DUMMIES *) malloc(sizeof(DUMMIES));
  
  p->total_args = num_dum_args ;
  p->fe_given_args = num_dum_args ;
  p->args_seen = 0;
  p->arg_lengths_index  = num_dum_args ;
  
  p->parms = NULL;
  
  if (num_dum_args > 0) {
    p->parms = (PARMS *)malloc(sizeof(PARMS)*num_dum_args);
    for (INT32 i = 1; i <  num_dum_args; i++) {
      PARMS_next(&(p->parms[i-1])) = &(p->parms[i]);
    }
    PARMS_next(&(p->parms[num_dum_args-1])) = NULL;
  }

  p->last_parm_ty_seen  = p->parms;
  p->orig_ret_type = ret_type;
  p->ret_type = ret_type;

  p->last_len_ty_seen   = NULL;
  p->arglist = NULL;
  if (num_dum_args > 0) 
    p->arglist = (ST **) malloc(2 * num_dum_args * sizeof(ST *))  ;

  EP_Current = o ;

}

/*===================================================
 *
 * cwh_auxst_add_dummy
 *
 * add a dummy argument to the list associated
 * with the entry point. Add the type of the dummy
 * to the param list of of the entry's TY. 
 *
 * If it's a character object, add the length dummy 
 * and its type. If result is TRUE, then the dummy is
 * a function result and it goes after the address
 * and not at the end.
 *
 ====================================================
*/
extern void
cwh_auxst_add_dummy(ST * dummy, BOOL result)
{
  DUMMIES *e ;
  ST    *ln  ;
  TY_IDX ty  ;
  PARMS *tl ;
  PARMS *te ;
  PARMS *tn ;

  e  = AUXST_Dummies(EP_Current);
  ln = cwh_types_character_extra(dummy);

  DevAssert((e->total_args > e->args_seen),(" arglist overflow"));

  e->arglist[e->args_seen++] = dummy ;

  ty = ST_type(dummy);
  tl = e->last_parm_ty_seen ;
  
  if (ST_sclass(dummy) == SCLASS_FORMAL_REF)
    ty = Make_Pointer_Type(ty);

  PARMS_ty(tl) = ty;

  if (result)
    e->ret_type = ST_type(dummy);

  /* if character dummy, add len to function parm list   */
  /* unless a result len, when it goes after the address */
  /* (it will be first len seen).                        */

  if (ln != NULL) {

    tn = (PARMS *) malloc(sizeof(PARMS));
    PARMS_ty(tn) = Be_Type_Tbl(cwh_addr_char_len_typeid);
    PARMS_next(tn) = NULL;

    if (result) {  /* make it look as though FE gave the length */
      e->arg_lengths_index++ ;
      e->fe_given_args++ ;
      e->arglist[e->args_seen++] = ln ;

      te = (PARMS *) malloc(sizeof(PARMS));
      PARMS_ty(te) = ST_type(ln);
      PARMS_next(te) = PARMS_next(tl);
      PARMS_next(tl) = te;
      tl = te ;
      
    } else {
      
      if (e->last_len_ty_seen == NULL) {
	te = e->last_parm_ty_seen ;

	while(PARMS_next(te))
	  te = PARMS_next(te);

      } else 
	te = e->last_len_ty_seen ;
      
      PARMS_next(te) = tn ;
      e->last_len_ty_seen = tn ;
      e->arglist[e->arg_lengths_index++] = ln ;
    }
    e->total_args++;
  } 

  e->last_parm_ty_seen = PARMS_next(tl) ;    

}

/*===================================================
 *
 * cwh_auxst_patch_proc
 *
 * discovered the function result is a small struct,
 * which is passed by value. To avoid changing the 
 * FE, which assumes by address, the internal
 * structures for dummy args are patched up here.
 *
 ====================================================
*/
extern void
cwh_auxst_patch_proc(TY_IDX rty_idx)
{
  DUMMIES *e ;

  e  = AUXST_Dummies(EP_Current) ;

  e->ret_type = rty_idx ;
  e->parms = PARMS_next(e->parms);
  e->total_args --;
  e->arg_lengths_index --;
  e->fe_given_args --;

  e->last_parm_ty_seen = e->parms;   
}

/*===================================================
 *
 * cwh_auxst_find_entry
 *
 * Find an entry point, and return a pointer to
 * its DUMMIES structure.
 *
 * Sets up current entry point auxst data. 
 *
 ====================================================
*/
static DUMMIES *
cwh_auxst_find_entry(ST * entry)
{
  AUXST *o ;

  o = cwh_auxst_find(entry,FALSE);
  EP_Current = o ;
  return (AUXST_Dummies(o)) ;
}

/*===================================================
 *
 * cwh_auxst_find_srcpos_addr
 *
 * Returns the address of the srcpos field in AUXST.
 *
 ====================================================
*/
extern USRCPOS *
cwh_auxst_srcpos_addr(ST * st)
{
  AUXST *o ;

  o = cwh_auxst_find(st, TRUE);
  return (&(AUXST_SrcPos(o))) ;
}

/*===================================================
 *
 * cwh_auxst_find_srcpos_val
 *
 * Returns the value of the srcpos field in AUXST.
 *
 ====================================================
*/
extern USRCPOS 
cwh_auxst_srcpos_val(ST * st)
{
  AUXST *o ;

  o = cwh_auxst_find(st, TRUE);
  return (AUXST_SrcPos(o)) ;
}

/*===================================================
 *
 * cwh_auxst_distr_preg
 *
 * Returns (creates) the preg # for the associated PREG 
 * for distributed arrays.
 *
 ====================================================
*/
extern PREG_det
cwh_auxst_distr_preg(ST * st)
{
  AUXST *o ;

  o = cwh_auxst_find(st, TRUE);
  if (AUXST_DstrReg(o) == -1) {
    AUXST_DstrPreg(o) = cwh_preg_next_preg(MTYPE_I4, NULL, NULL);
  }
  return (AUXST_DstrPreg(o)) ;
}

#ifdef KEY /* Bug 4901 */
/*===============================================
 *
 * cwh_auxst_clear_stem_name
 *
 * Clears the stem for a DST name.
 * 
 *===============================================
 */ 
extern void
cwh_auxst_clear_stem_name(ST * st)
{
  AUXST *o = cwh_auxst_find(st, FALSE);
  if (o) {
    char *name = AUXST_Stem(o);
    if (name) {
      free(AUXST_Stem(o));
      AUXST_Stem(o) = NULL;
    }
  }
}
#endif /* KEY Bug 4901 */

/*===============================================
 *
 * cwh_extern_stem_name
 *
 * Returns or sets the stem for a DST name.
 * 
 *===============================================
 */ 
extern char *
cwh_auxst_stem_name(ST * st, char * name)
{
  char  * r ;
  AUXST * o ;

  r = name ;
  o = cwh_auxst_find(st, name != NULL) ;

  if ( o != NULL) {
    if (name)
      AUXST_Stem(o) = name;
    else
      r = AUXST_Stem(o) ;
  }

  return r ;
}

/*===================================================
 *
 * cwh_auxst_cri_pointee
 *
 * Set the CRI_pointee of the give CRI_pointer.
 * If the pointee is present, otherwise just lookup.
 * Used for initialization.
 *
 ====================================================
*/
extern ST *
cwh_auxst_cri_pointee(ST * ptr, ST * pointee)
{
  AUXST *o ;
  ST * res = pointee ;

  o = cwh_auxst_find(ptr,res != NULL);

  if (o) {
    if (pointee)
      AUXST_CRIPointee(o) = pointee ;
    else
      res = AUXST_CRIPointee(o);
  }
  return res;
}

/*===================================================
 *
 * cwh_auxst_pragma
 *
 * associate a pragme with an ST. This is preamble
 * information, usually for nested procedures.
 * It says host varble is read/modified etc.
 *
 ====================================================
*/
extern WN *
cwh_auxst_pragma(ST * ptr, WN * wn)
{
  AUXST *o ;
  WN * res = wn ;

  o = cwh_auxst_find(ptr,wn != NULL);

  if (o) {
    if (wn)
      AUXST_Pragma(o) = wn ;
    else
      res = AUXST_Pragma(o);
  }
  return res;
}
/*===================================================
 *
 * cwh_auxst_assign_id
 *
 * Returns the address of the auxiliary entry for this
 * label_idx. Currently used only for assigned goto's.
 *
 ====================================================
*/
INT32 *
cwh_auxst_assign_id(SYMTAB_IDX level, LABEL_IDX idx) 
{
  return &(Auxlabel_Table[idx].assign_id);
}

/*===================================================
 *
 * cwh_auxst_find_dummy
 *
 * Find if the dummy is already in the current list
 * ( for alternate entry points...)
 *
 ====================================================
*/
extern BOOL
cwh_auxst_find_dummy(ST * arg)
{
  DUMMIES *p ;
  INT16  i ;
  ST    **ap ;

  p = AUXST_Dummies(EP_Current);
  ap = p->arglist;

  for (i = 0 ; i < p->args_seen ; i ++ ) 
    if (arg == *ap++ )
      return (TRUE);

  return(FALSE);
}

/*===================================================
 *
 * cwh_auxst_find_dummy_len
 *
 * Find the length argument of the character ST 
 * in the current entry structure. If the dummy
 * is the result variable of a character function,
 * it's the second arg, not in the list of lengths.
 *
 ====================================================
*/
extern ST *
cwh_auxst_find_dummy_len(ST * arg)
{
  DUMMIES *p ;
  INT16  i,c ;
  ST    **ap ;

  p  = AUXST_Dummies(EP_Current);
  c  = 0 ;
  ap = p->arglist;

  /* char function result? */

  if (AUXST_Flag(EP_Current,f_RSLTTMP) &&
      cwh_types_is_character(p->ret_type))
    if (arg == *ap++ ) 
      return (p->arglist[1]);

  DevAssert((p->args_seen >= p->fe_given_args ),("Missing args"));

  /* no, look for character dummies */

  for (i = 0 ; i < p->fe_given_args ; i ++ ) {
    if(cwh_types_is_character(ST_type(*ap))) {
      if (arg == *ap) 
	return(p->arglist[p->args_seen+c]);
      else
	c++ ;
    }
    ap++ ;
  }

  return (NULL);
}

/*===================================================
 *
 * cwh_auxst_arglist
 *
 * Return a pointer to an array of STs which
 * describe the dummy argument list for the given
 * entry point. Indirectly sets current entry point
 * to entry.
 *
 ====================================================
*/
extern ST **
cwh_auxst_arglist(ST * entry)
{
  DUMMIES * e ;

  e = cwh_auxst_find_entry(entry);

  return (e->arglist);
}

/*===================================================
 *
 * cwh_auxst_num_dummies
 *
 * Return the number of dummies associated 
 * with this entry point, including hidden arguments.
 * Indirectly sets current entry point to entry.
 * 
 ====================================================
*/
extern INT16
cwh_auxst_num_dummies(ST * entry) 
{
  DUMMIES * e ;

  e = cwh_auxst_find_entry(entry);

  return (e->total_args);
}

/*===================================================
 *
 * cwh_auxst_set_tylist
 *
 * Each entry point's ST has the args information stored
 * in the PARMS list of AUXST; extract that information
 * and create TYLISTs for the entry points.
 *
 * goes with assumptions in cwh_types_mk_procedure_TY
 * and cwh_auxst_patch_proc
 *
 ====================================================
*/
extern void
cwh_auxst_set_tylist(ST *en)
{
 AUXST * o  ;
 DUMMIES *e ;
 INT32 i;
 TYLIST_IDX tylist_idx;
 PARMS *parms;

 o  = cwh_auxst_find(en, FALSE);
 e  = AUXST_Dummies(o);

 /* if total args == 0, may have transformed struct arg into */
 /* function result by value. If so it needs a return type   */

 if (e->fe_given_args == 0 && !e->ret_type) 
    return;

 TY& ty = Ty_Table[ST_pu_type(en)];

 (void) New_TYLIST (tylist_idx);
 Set_TY_tylist (ty, tylist_idx);

 if (ST_auxst_has_rslt_tmp(en) && 
     !(e->ret_type && (STRUCT_BY_VALUE(e->ret_type)))) {

   Tylist_Table [tylist_idx] = e->orig_ret_type;     /* returns MTYPE_V */

 } else {
   Tylist_Table [tylist_idx] = e->ret_type;
 }


 /* add each argument */
 
 parms = e->parms;

 for (i= 0 ; i < e->total_args; i++) {

    (void) New_TYLIST (tylist_idx);
    Tylist_Table [tylist_idx] = PARMS_ty(parms);
    parms = PARMS_next(parms);
 }

 /* mark end of argument list */

 (void) New_TYLIST (tylist_idx);
 Tylist_Table [tylist_idx] = 0;

}

/*===================================================
 *
 * cwh_auxst_dump_list
 *
 * Dump a LIST of items.
 *
 ====================================================
*/
extern void
cwh_auxst_dump_list (LIST * l, BOOL verbose)
{
  ITEM * i;

  if (l == NULL)
    return ;

  if (L_num(l) == 0)
    return ;

  i = L_first(l);

  while (i != NULL) {
    if (I_element(i) == NULL) 
      printf ("     < NULL ITEM ?>\n");
    else {
      if (verbose) 
	DUMP_ST(I_element(i));
      else 
	printf ("    %p (%s) \n",I_element(i),ST_name(I_element(i)));

      i = I_next(i);
    }
  }
  printf (" \n");
}

/*===================================================
 *
 * cwh_auxst_dump_dummies
 *
 * Dump a DUMMIES item
 *
 ====================================================
*/
static void
cwh_auxst_dump_dummies(DUMMIES * d)
{
  INT32  i,k,j ;

  if (d == NULL)
    return ;

  printf ("  DUMMIES : %p   next : %p  \n",
	  d,
	  d->next_entry);

  if (d->ret_type != 0)
    printf ("    result TY : %d, \n",d->ret_type);


  if (d->total_args != 0) {

    printf ("    args   : total# %d, #fe_given %d, #seen %d, # including lengths %d \n",
	    d->total_args,
	    d->fe_given_args,
	    d->args_seen,
	    d->arg_lengths_index);

    for (i = 0 ; i < d->args_seen ; i ++ ) {
      printf ("      arg ST : %p (%s) \n", 
	      d->arglist[i],
	      ST_name( d->arglist[i]));
    }

    for (i = d->fe_given_args; 
	 i < d->arg_lengths_index ; 
	 i ++) {

      printf ("      len ST : %p (%s) \n", 
	      d->arglist[i],
	      ST_name( d->arglist[i]));
    }

    j = d->args_seen;

    PARMS * te = d->parms;
    while(te && (j-- >0)) {
      printf ("          TY : 0x%x  %s \n", PARMS_ty(te), 
	      TY_name(PARMS_ty(te))) ;
      te = PARMS_next(te);
    }
  }
  printf("\n");
}

/*===================================================
 *
 * cwh_auxst_dump
 *
 * Dump an AUXST, given an ST.
 *
 ====================================================
*/
extern void
cwh_auxst_dump (ST * st)
{
  AUXST * o;
  LIST  * l;

  o = cwh_auxst_find(st,FALSE);

  if (o == NULL)
    return ;

  printf ("AUXST: %p   next: %p \n",o,AUXST_Next(o));

  if (AUXST_OwningST(o) != NULL ) {
    printf ("  associated ST: %p (%s) \n",
	    AUXST_OwningST(o),
	    ST_name(AUXST_OwningST(o))); 
  }

  if (USRCPOS_filenum(AUXST_SrcPos(o)) != 0)  {
    printf ("  file: %d line: %d \n",
	    USRCPOS_filenum(AUXST_SrcPos(o)), 
	    USRCPOS_linenum(AUXST_SrcPos(o)));
  }

  if (AUXST_Flag(o,f_ALTENT))
    printf ("  is alternate entry pt \n") ;

  if (AUXST_Flag(o,f_ALTTY))
    printf ("  alternate entry STs have same TY \n") ;

  if (AUXST_Flag(o,f_RSLTTMP))
    printf ("  first argument is result varbl \n");

  if (AUXST_Flag(o,f_ELEM))
    printf ("  elemental function \n");

  if (AUXST_Flag(o,f_NONCONT))
    printf ("  non-contiguous \n");

  if (AUXST_Flag(o,f_AUTO_OR_CPTR))
    printf ("  auto or cray pointer\n");

  if (AUXST_Flag(o,f_F90_PTR))
    printf ("  f90 pointer \n");

  if (AUXST_Flag(o,f_MODULE))
    printf ("  Common for module data \n");

  if (AUXST_Stem(o) != NULL) 
    printf ("  DST name: %s \n",AUXST_Stem(o));
  
  if (AUXST_Pragma(o)) 
    printf ("  pragma: WN %p \n",AUXST_Pragma(o)) ;

  if (AUXST_CRIPointee(o)) 
    printf ("  cri_pointee: ST %p (%s)\n",AUXST_CRIPointee(o),ST_name(AUXST_CRIPointee(o))) ;

  if (AUXST_DataInfo(o)) 
    printf ("  data info: %p \n",AUXST_DataInfo(o)) ;

  l = cwh_auxst_find_list(o,l_ALTENTRY) ;
  if (L_first(l) != NULL){
    printf ("  alternate entry points: \n") ;
    cwh_auxst_dump_list(l,FALSE);
  }

  l = cwh_auxst_find_list(o,l_COMLIST);
  if (L_first(l) != NULL){
    printf ("  common items: \n") ;
    cwh_auxst_dump_list(l,FALSE);
  }

#ifdef KEY /* Bug 5271 */
  l = cwh_auxst_find_list(o,l_PU_COMLIST);
  if (L_first(l) != NULL){
    printf ("  common items in current program unit: \n") ;
    cwh_auxst_dump_list(l,FALSE);
  }
#endif /* KEY Bug 5271 */

  l = cwh_auxst_find_list(o,l_EQVLIST);
  if (L_first(l) != NULL){
    printf ("  equivalence items: \n") ;
    cwh_auxst_dump_list(l,FALSE);
  }

  l = cwh_auxst_find_list(o,l_DST_COMLIST);
  if (L_first(l) != NULL){
    printf ("  commons for dst info: \n") ;
    cwh_auxst_dump_list(l,FALSE);
  }

  l = cwh_auxst_find_list(o,l_DST_PARMLIST);
  if (L_first(l) != NULL){
    printf ("  parameters for dst info: \n") ;
    cwh_auxst_dump_list(l,FALSE);
  }

  if (AUXST_Dummies(o) != NULL) 
    cwh_auxst_dump_dummies(AUXST_Dummies(o));

  l = cwh_auxst_find_list(o,l_NAMELIST);
  if (L_first(l) != NULL){
    printf ("  namelist items: \n") ;
    cwh_auxst_dump_list(l,FALSE);
  }

  l = cwh_auxst_find_list(o,l_RETURN_TEMPS);
  if (L_first(l) != NULL){
    printf ("  result temps: \n") ;
    cwh_auxst_dump_list(l,FALSE);
  }

  l = cwh_auxst_find_list(o,l_SPLITLIST);
  if (L_first(l) != NULL){
    printf ("  split commons: \n") ;
    cwh_auxst_dump_list(l,FALSE);
  }

  if (AUXST_AssignId(o) != -1) 
    printf ("  assign id:  0x%x \n", AUXST_AssignId(o));

  if (AUXST_DstrReg(o) != -1) 
    printf ("  distr preg: %d \n", AUXST_DstrReg(o));

  printf ("--\n");

}
