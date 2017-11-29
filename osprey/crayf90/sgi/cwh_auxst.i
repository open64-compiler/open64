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

/* -------------------------------------------
   Structure to track dummy arguments of a fn
   until can be converted into TYLIST
*/

typedef struct parmlist {
   TY_IDX ty;
   struct parmlist *next;
} PARMS;

#define PARMS_ty(o)       ((o)->ty)
#define PARMS_next(o)     ((o)->next)


/* -------------------------------------------
  Structures to associate dummy arguments with 
  an entry point ST. There doesn't happen to be
  any association with the procedure definition in
  the interface, and a little is needed for WN & dwarf.

  - fe_given_args is number FE says to pass, including fn result address
  - Total_args includes hidden arguments eg: char lengths.
  - Args_seen - number of arg slots filled so far
  - Arg_lengths_index - index to character length arguments, but
                       not including length of character function result.
  - Last_parm_ty_seen - TYLIST entry corresponding to Args_seen
                      on TEXT TY for procedure.
  - Last_len_ty_seen  - ditto for character lengths.
  - ret_type -  function result TY, for those which pass result as dummy
	   and are marked as subroutine.
  - orig_ret_type -  function result TY that was sent from fe, to keep
                     track of result by dummy and generate the correct 
		     TYLIST entry.

*/

typedef struct alist {
		INT32   total_args  ; 
		INT32   fe_given_args  ;
		INT32   args_seen   ;
		INT32   arg_lengths_index ;
		PARMS * last_parm_ty_seen;
		PARMS * last_len_ty_seen;
		struct alist  * next_entry ;
		ST    **arglist    ;
		PARMS *parms;
		TY_IDX ret_type;
		TY_IDX orig_ret_type;
} DUMMIES ;



/* ----------------------------------------------
 
  This defines an auxiliary struct for associating 
  odds & ends with an ST. It's pointed to by the ST_temp
  field, and allocated in cwh_stab.c

 - dummy:   is a list of dummy args for a TEXT entry ST  
 - pragma:  is a WN pragma - host variables of internal procs use them.
 - stptr:   pointer back to the ST, to clean up.
 - altentry:list of alternate entry point STs (DST) associated with procedure.
 - comlist: list of STs of elements of common block, associated with COMMON ST. (DST)   
 ifdef KEY  * Bug 5271 * 
 - pu_comlist: whereas comlist accumulates variables belonging to a common
            block across all program units, this contains only members known to
	    the current program unit. Before this fix, the Dwarf ST also
	    accumulated members, so the last program unit in the compilation
	    would know about members that only appeared in other compilation
	    units (bad when "x" means two different things in two different
	    instances of a particular common block!)
	    Clearing "comlist" at the beginning of each new program
	    unit fixed the Dwarf ST problem, but caused the WHIRL st to repeat
	    a member for each program unit even if that member was identical
	    in every program unit.
 endif  * KEY Bug 5271 * 
 - nlist:   list of STs which comprise this namelist.
 - rlist:   list of return varbl STs for a alternate entry point's shared temp 
 - splitlist:	list of child STs if this parent common block is split
 - stem:    ST's DST name if internal  or module procedure or MAIN
 - next:    pointer to next AUXST, to clean up.
 - distr_preg: associated preg for distributed arrays
 - pos:     declaration coordinates for this sym.
 - flags:   defined in cwh_stab.h
 - eqvlist: list of st's of elements of equivalence blocks.
 - data_info: pointer to structure containing information for statically intialized symbols
 - dst_comms: list of commons in Global Symtab that require DST info in this PU.
 - dst_parms: list of parameters in Global Symtab that require DST info in PU.
 - cri_pointee: for a CRI_Pointer, the ST of its CRU_pointee.
*/


typedef struct auxst {
	DUMMIES * dummy    ;
	WN      * pragma   ;
	ST      * stptr    ;
	LIST      altentry ;	
	LIST      comlist  ;
#ifdef KEY /* Bug 5271 */
	LIST      pu_comlist  ;
#endif /* KEY Bug 5271 */
	LIST      nlist    ;
	LIST      rlist    ;
	LIST	  splitlist;
	LIST      eqvlist;
	LIST      dst_comms;
	LIST      dst_parms;
	char    * stem     ;
        struct auxst * next_auxst;
	USRCPOS   pos      ;
	INT32     assign_id;
	PREG_det  distr_preg;
	ST      * cri_pointee;
	struct data_info_s *data_info; 
	UINT32    flags    ;
} AUXST ;


#define Set_AUXST_Flag(o,f)   ((o)->flags |= f)
#define Clear_AUXST_Flag(o,f) ((o)->flags &= ~f)
#define AUXST_Flag(o,f)       ((o)->flags & f)

#define AUXST_AssignId(o)     ((o)->assign_id)
#define AUXST_CRIPointee(o)   ((o)->cri_pointee) 
#define AUXST_DataInfo(o)     ((o)->data_info)
#define AUXST_Dummies(o)      ((o)->dummy) 
#define AUXST_DstrReg(o)      ((o)->distr_preg.preg)
#define AUXST_DstrTY(o)       ((o)->distr_preg.preg_ty)
#define AUXST_DstrST(o)       ((o)->distr_preg.preg_st)
#define AUXST_DstrPreg(o)     ((o)->distr_preg)
#define AUXST_Flags(o)        ((o)->flags)
#define AUXST_Next(o)         ((o)->next_auxst) 
#define AUXST_OwningST(o)     ((o)->stptr)
#define AUXST_Pragma(o)       ((o)->pragma) 
#define AUXST_SrcPos(o)       ((o)->pos)
#define AUXST_Stem(o)         ((o)->stem) 

#define AUXST_Commons(o)      ((LIST *) &(o)->comlist) 
#ifdef KEY /* Bug 5271 */
#define AUXST_PU_Commons(o)      ((LIST *) &(o)->pu_comlist) 
#endif /* KEY Bug 5271 */
#define AUXST_Namelist(o)     ((LIST *) &(o)->nlist)
#define AUXST_RtnTemps(o)     ((LIST *) &(o)->rlist)
#define AUXST_SplitCommons(o) ((LIST *) &(o)->splitlist)
#define AUXST_Altentries(o)   ((LIST *) &(o)->altentry)
#define AUXST_Equivs(o)       ((LIST *) &(o)->eqvlist) 
#define AUXST_Dstcomlist(o)   ((LIST *) &(o)->dst_comms)
#define AUXST_Dstparmlist(o)  ((LIST *) &(o)->dst_parms)



/* For convenience in deallocation, an auxst list for each
 * scope level is maintained.
 */

static AUXST * Top_Auxst[INTERNAL_LEVEL+1] = {NULL,NULL,NULL,NULL};
static AUXST * EP_Current ;

/*===================================================
 *
 * The following declarations handle the creation of Auxst_Table which will
 * automatically grow and shrink with the St_Table.
 *
 *===================================================
 */

#define MAX_AUXST_LEVEL 8

typedef RELATED_SEGMENTED_ARRAY<AUXST *> AUXST_PTR_ARRAY;

struct AUXST_TAB {
   AUXST_PTR_ARRAY *Auxst_table;
}; 

extern AUXST_TAB *Auxst_tab;

struct AUXST_TAB_SYMTAB_ACCESS;

typedef TABLE_INDEXED_BY_LEVEL8_AND_INDEX24<AUXST *, ST_IDX, SYMTAB_IDX,
                                            AUXST_TAB *, &Auxst_tab,
                                            AUXST_TAB_SYMTAB_ACCESS>
        AUXST_TABLE;

struct AUXST_TAB_SYMTAB_ACCESS {
  AUXST_TAB_SYMTAB_ACCESS(void) { }

  AUXST_PTR_ARRAY *operator()(AUXST_TAB **Auxst_tab_parm, SYMTAB_IDX level)
    { 
      DevAssert((level < MAX_AUXST_LEVEL), ("AUXST table: leve overflow"));
      return (*Auxst_tab_parm)[level].Auxst_table;
    } 
};

static AUXST_TABLE Auxst_Table;

/* auxiliary ST & Label tables */

struct AUX_LABEL_INFO {
   INT32 assign_id;
   AUX_LABEL_INFO(void) : assign_id(-1) { }
};

typedef RELATED_SEGMENTED_ARRAY<AUX_LABEL_INFO> FE_LABEL_TAB;

FE_LABEL_TAB Auxlabel_Table;


/* forward references */

static LIST *    cwh_auxst_find_list(AUXST * o, enum list_name list) ;
static AUXST*    cwh_auxst_find(ST *st, BOOL create) ;
static DUMMIES * cwh_auxst_find_entry(ST * entry) ;
static void      cwh_auxst_dump_dummies(DUMMIES * d);
