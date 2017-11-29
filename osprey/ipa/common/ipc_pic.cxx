/*
 * Copyright 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
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


#include <stdint.h>
#include <alloca.h>

#include "linker.h" // for sharable
#include "pass1.h"			// for used_gp_area
#include "main.h"			// for max_gpa_size
#include "ipc_weak.h"			// for symbols defined in ld

#include "symtab.h"
#include "ipa_option.h"
#include "ipc_symtab_merge.h"           // for AUX_ST
#include "ipc_pic.h"
#include "ld_ipa_interface.h"


// set IPA_Object_Name to a legal variable name
static void Get_IPA_Object_Name(){
  char *bname = basename(outfilename);

  while (*bname == '/')
    bname++;

  IPA_Object_Name = (char *) malloc (strlen(bname) + 1);

  int i;
  for(i=0; bname[i] != '\0'; i++) {
    if ( !isalnum(bname[i]) ) IPA_Object_Name[i] = '_';
    else IPA_Object_Name[i] = bname[i];
  }
  IPA_Object_Name[i] = '\0';
}

// returns a unique name, used for promoting static variables to globals
STR_IDX
Create_Unique_Name (const char* name)
{
    static UINT32 g_count = 0;
    char *new_name = (char *)alloca (strlen (name)+strlen(IPA_Object_Name)+10);

    char *p = new_name;
    
    if (name == 0) {
        *p++ = '.';
        *p++ = 'f';
    } else {
	while (*name != 0 && !(name[0] == '.' && name[1] == '.'))
	    *p++ = *name++;
    }

    *p++ = '.';
    *p++ = '.';

    strcpy( p, IPA_Object_Name);
    p += strlen(IPA_Object_Name);
    UINT i = ++g_count;

    do {
        *p++ = (i % 16) + 'A';
        i /= 16;
    } while (i > 0);

    *p = 0;

    return Save_Str(new_name);

} // Create_Unique_Name


struct PIC_OPT
{
    BOOL f_call_shared;
    BOOL hs_ignore;

    PIC_OPT (BOOL cs, BOOL hs) : f_call_shared (cs), hs_ignore (hs) {}

    void operator() (UINT32, ST* st) const {
	//
	// Code replacing Initialize_Addr_Taken() in ipaa.cxx
	//
	AUX_ST& aux_st = Aux_St_Table[ST_st_idx (st)];
	if (AUX_ST_flags (aux_st, (USED_IN_OBJ|USED_IN_DSO))) {
		Set_ST_addr_saved (&St_Table[ST_st_idx (st)]);
	}

	if (ST_sclass (st) == SCLASS_EXTERN) {
	    if (!ST_is_weak_symbol (st))
		return;
	    else if (ST_strong (st) == st)
		return;
	}

	BOOL address_taken = ST_addr_saved (st) || ST_addr_passed (st);

	ST_EXPORT export_class = ST_export(st);

	if (export_class == EXPORT_LOCAL ||
	    export_class == EXPORT_LOCAL_INTERNAL) {
	    if (ST_class (st) == CLASS_FUNC) {
		Set_ST_name_idx (st, Create_Unique_Name (ST_name (st)));
#ifdef KEY
// Reset the flag so that cg does not remove this function. Applies only to C functions.
		if (!IPA_Enable_Inline)
		    Clear_PU_is_extern_inline (Pu_Table [ST_pu (st)]);
#endif

		if (address_taken)
		    Set_ST_export (st, EXPORT_HIDDEN);
		else 
		    Set_ST_export (st, EXPORT_INTERNAL);
	    } else if (ST_class (st) == CLASS_VAR) {
		if ((ST_sclass (st) == SCLASS_COMMON ||
		     ST_sclass (st) == SCLASS_DGLOBAL) &&
		    ST_base_idx (st) != ST_st_idx (st)) {
		    // common block element
		    const ST& base_st = St_Table[ST_base_idx (st)];
		    if (ST_addr_saved (base_st) || ST_addr_passed (base_st))
			address_taken = TRUE;
		}
		if (!address_taken)
		    Set_ST_export (st, EXPORT_LOCAL_INTERNAL);
	    }
	    return;
	}

	if (hs_ignore || export_class != EXPORT_PREEMPTIBLE) {
	    if ((export_class == EXPORT_HIDDEN) && !address_taken) {
		/* see if we can change a user-specified HIDDEN symbol to
		   INTERNAL */
		Set_ST_export(st, EXPORT_INTERNAL);
	    } else if (export_class == EXPORT_PREEMPTIBLE &&
		       (ST_is_weak_symbol (st) &&
			ST_st_idx (st) != ST_strong_idx (*st)) ) {
		/*
		 * exported weak symbols:  we need to make sure that the
		 * corresponding strong is not marked dead or constant
		 */
		
		ST_IDX strong_st_idx = ST_strong_idx (*st);
		FmtAssert (strong_st_idx != 0, ("Strong symbol expected"));
		/*
		 * Set strong with used and prop. info from weak to strong
		 */
		Set_AUX_ST_flags( Aux_St_Table[strong_st_idx], 
				  USED_IN_OBJ|DEF_IN_DSO|USED_IN_DSO );
		Set_AUX_ST_flags ( Aux_St_Table[strong_st_idx],
				   AUX_ST_flags (Aux_St_Table[ST_st_idx(st)], 
						 USED_IN_OBJ|DEF_IN_DSO|USED_IN_DSO) );
	    } 

	    if (ST_is_weak_symbol (st) && ST_class (st) == CLASS_VAR) {
		if (ST_export (st) != EXPORT_PREEMPTIBLE &&
		    ST_st_idx (st) != ST_strong_idx (*st)) {
		    const ST* strong_st = ST_strong(st);
		    Set_ST_sclass (st, ST_sclass (strong_st));
		    Clear_ST_is_weak_symbol (st);
		    Synch_ST_flags(*st, *strong_st);
		} else 
		    Set_ST_not_gprel (st);
	    }

	    if (ST_is_weak_symbol(st))
	      return;
	}
	if(f_call_shared) {
	    // bug fix for OSP_219
	    //
	    if (AUX_ST_flags(Aux_St_Table[ST_st_idx (st)], (USED_IN_DSO|OBJ_COMMON))){
		/* common can be preempted by the def. in a DSO or OBJ_COMMON. 
		 */
		if (ST_sclass (st) != SCLASS_COMMON) {
		    Set_ST_export(st, EXPORT_PROTECTED);
		}
	    } else if (address_taken) {
		Set_ST_export(st, EXPORT_HIDDEN);
	    } else  {
		Set_ST_export(st, EXPORT_INTERNAL);
	    }
	} else if (IPA_Enable_Relocatable_Opt) {
	    if (ST_sclass (st) != SCLASS_COMMON) {
		Set_ST_export(st, EXPORT_PROTECTED);
	    }
	}
	if (ST_is_weak_symbol (st) && ST_class (st) == CLASS_VAR) {
	    if (ST_export (st) != EXPORT_PREEMPTIBLE &&
		ST_st_idx (st) != ST_strong_idx (*st)) {
		const ST* strong_st = ST_strong (st);
		Set_ST_sclass (st, ST_sclass (strong_st));
		Clear_ST_is_weak_symbol (st);
		Synch_ST_flags (*st, *strong_st);
	    } else 
		Set_ST_not_gprel (st);
	}
    }
}; // PIC_OPT


void
Pic_optimization ()
{
    INT f_call_shared = (ld_ipa_opt[LD_IPA_SHARABLE].flag == F_CALL_SHARED);
    INT hs_ignore = (ld_ipa_opt[LD_IPA_HIDES].flag == HS_IGNORE);

    Get_IPA_Object_Name(); 

    PIC_OPT pic_opt (f_call_shared, hs_ignore);
    For_all (St_Table, GLOBAL_SYMTAB, pic_opt);

    Verify_SYMTAB (GLOBAL_SYMTAB);
} // Pic_optimization


//----------------------------------------------------------------------
// When pic_opt is not turned on, we still need to change all static
// functions to hidden global (with unique name) because functions within
// the same file might be written out to multiple output files.
//----------------------------------------------------------------------
struct fix_static_func
{
      const unsigned shared_status;
  
      fix_static_func (unsigned cs) : shared_status (cs) {}
  
      void operator() (UINT32, ST* st) const {
        BOOL call_shared = (shared_status == F_CALL_SHARED);
        BOOL non_shared = (shared_status == F_NON_SHARED);
  
        ST_EXPORT eclass = ST_export (st);
        if (eclass != EXPORT_LOCAL && eclass != EXPORT_LOCAL_INTERNAL) {
            if (ST_is_weak_symbol (st) && ST_sclass (st) != SCLASS_EXTERN &&
                call_shared)
                Clear_ST_is_weak_symbol (st);
            else if ((ST_sclass(st) != SCLASS_EXTERN) && non_shared)
                // For non_shared, 
                Set_ST_export (st, EXPORT_HIDDEN);
            return;
        }

	if (ST_class (st) == CLASS_FUNC) {
	    if (eclass == EXPORT_LOCAL) {
		Set_ST_name_idx (st, Create_Unique_Name (ST_name (st)));
		Set_ST_export (st, EXPORT_HIDDEN);
#ifdef KEY
// Reset the flag so that cg does not remove this function. Applies only to C functions.
		if (!IPA_Enable_Inline)
		    Clear_PU_is_extern_inline (Pu_Table [ST_pu (st)]);
#endif
	    } else if (eclass == EXPORT_LOCAL_INTERNAL) {
		Set_ST_name_idx (st, Create_Unique_Name (ST_name (st)));
		Set_ST_export (st, EXPORT_INTERNAL);
	    }
	} 
    }
};


void
Fix_up_static_functions ()
{
    For_all (St_Table, GLOBAL_SYMTAB, fix_static_func (ld_ipa_opt[LD_IPA_SHARABLE].flag));
    Verify_SYMTAB (GLOBAL_SYMTAB);
} // Fix_up_static_functions 


struct Opt_global_var
{
    void operator() (UINT32, ST* st) const {

	if (ST_class (st) != CLASS_VAR)
	    return;

	BOOL common_block_element = FALSE;
	ST_EXPORT export_class = ST_export (st);
	Clear_ST_is_global_as_local(st);

	switch (ST_sclass (st)) {
	case SCLASS_COMMON:
	case SCLASS_DGLOBAL:
	    if ((export_class == EXPORT_LOCAL ||
		 export_class == EXPORT_LOCAL_INTERNAL) &&
		ST_base_idx (st) != ST_st_idx (st)) {
		
		common_block_element = TRUE;
		ST& base_st = ST_base (*st);
		if (ST_addr_saved (st) || ST_addr_passed (st)) {
		    Clear_ST_is_not_used (base_st);
		    Clear_ST_is_const_var (base_st);
		} else {
		    if (ST_is_not_used (base_st))
			Set_ST_is_not_used (st);
		    if (ST_is_const_var (base_st))
			Set_ST_is_const_var (st);
		}
	    }
	    if (ST_is_equivalenced (st))
		return;
	    // fall through
	    
	case SCLASS_FSTATIC:
	case SCLASS_UGLOBAL:
	    break;

	default:
	    return;
	}

	ST_IDX st_idx = ST_st_idx (st);
	TY_IDX ty = ST_type (st);
	AUX_ST& aux_st = Aux_St_Table[st_idx];
	if (TY_is_volatile (ty) ||
	    (aux_st.flags & USED_IN_OBJ) ||
	    (aux_st.flags & USED_IN_DSO) ||
	    (export_class != EXPORT_INTERNAL &&
	     export_class != EXPORT_LOCAL_INTERNAL &&
	     export_class != EXPORT_PROTECTED)) {
	    
	    Set_AUX_ST_flags (aux_st, IGNORE_REFCOUNTS);
	    return;
	}
	if (ST_is_weak_symbol (st) && ST_st_idx (st) != ST_base_idx (st)) {
	    Set_AUX_ST_flags (aux_st, IGNORE_REFCOUNTS);
	    return;
	}

	if (ST_addr_saved (st) || ST_addr_passed (st)) {
	    Set_AUX_ST_flags (aux_st, IGNORE_REFCOUNTS);
	    return;
	}

	ST_IDX base_st_idx = ST_base_idx (st);

	if (AUX_ST_refcount(Aux_St_Table[base_st_idx]) == 0 &&
	    IPA_Enable_DVE && !common_block_element) {
	    // we can't delete unused common block elements without
	    // restructuring the common block; so we don't support that for 
	    // now. 
	    Set_ST_is_not_used (st);
	    if (Trace_IPA || Trace_Perf)
		fprintf(TFile, "%s is marked NOT USED\n", ST_name (st));
	} else if (AUX_ST_modcount(Aux_St_Table[base_st_idx]) == 0 &&
		   IPA_Enable_CGI) {
	    Set_ST_is_const_var (st);
	    if (Trace_IPA || Trace_Perf) {
		fprintf(TFile, "%s is marked CONSTANT ", ST_name (st));
		if (ST_is_initialized (st) && !ST_init_value_zero (st)) {
		    const INITO_IDX idx = ST_To_INITO_Map[ST_st_idx (st) ];
		    if (idx)
			Inito_Table[idx].Print(TFile);
		    else
			fputs ("<unknown>\n", TFile);
		} else
		    fprintf (TFile, "0\n");
	    }
	}
    }
};

void
Optimize_Global_Variables () 
{
	For_all (St_Table, GLOBAL_SYMTAB, Opt_global_var());
}

/*  AUTOGNUM CODE */

typedef UINT32 AUX_IDX;
static AUX_IDX *Refcount_sym_idx;

static inline ST_IDX
next_gp_rel_candidate(const ST_IDX i)
{
    return ( make_ST_IDX ( Refcount_sym_idx[i], GLOBAL_SYMTAB) );
}

	/*******************************************************
		Function: ref_count_cmp
		For purpose of autognum, we combine the ref_count
		with mod_count.

	 *******************************************************/
int
ref_count_cmp (const ST_IDX *p_aux_x, const ST_IDX  *q_aux_x)
{
    int p_dontcare, q_dontcare;
    int p_count, q_count;

    if ( *p_aux_x == 0 ) { // Don't sort 0th entry
       	return ( -1 );
    }
    ST & pst = St_Table[make_ST_IDX (*p_aux_x, GLOBAL_SYMTAB)];
    ST & qst = St_Table[make_ST_IDX (*q_aux_x, GLOBAL_SYMTAB)];

    /* sort symbol table index according to sizes of symbol and 
     * descending reference count order, with smaller size taking precedence
     * ie symbol(s) with the highest reference count get(s) the smallest
     * array index number
     */

    /* always put UNDEFs and LOCALs and FUNCs at the end of the array 
     * make sure undefined COMMONs are NOT ignored since they would be
     * allocated by ld later so need to treat them like a defined global DATA
     */
    p_dontcare = ( ST_storage_class (pst) == SCLASS_EXTERN           ||
	               ST_class (pst) != CLASS_VAR                       ||
	              (ST_storage_class (pst) == SCLASS_COMMON && 
	               AUX_ST_flags(Aux_St_Tab[*p_aux_x], OBJ_COMMON)) )
				  ? 1 : 0;
    q_dontcare = ( ST_storage_class (qst) == SCLASS_EXTERN           ||
	               ST_class (qst) != CLASS_VAR                       ||
	              (ST_storage_class (qst) == SCLASS_COMMON && 
	               AUX_ST_flags(Aux_St_Tab[*q_aux_x], OBJ_COMMON)) )
				  ? 1 : 0;

    if (p_dontcare || q_dontcare)
       	return (p_dontcare - q_dontcare);

 
    /* zero reference count symbols are put at the end of the array */
    p_count = AUX_ST_refcount(Aux_St_Tab[*p_aux_x]) + 
	          AUX_ST_modcount(Aux_St_Tab[*p_aux_x]);
    q_count = AUX_ST_refcount(Aux_St_Tab[*q_aux_x]) + 
	          AUX_ST_modcount(Aux_St_Tab[*q_aux_x]);
    if (p_count == 0 || q_count == 0)
	return q_count - p_count;

    if (TY_size (ST_type(pst)) < TY_size (ST_type(qst)) ) {
	return (-1);
    }
    else if (TY_size (ST_type(pst)) > TY_size (ST_type(qst)) ) {
		return (1);
    }

    if (p_count > q_count) {
		return (-1);
    } else if (p_count < q_count) {
		return (1);
    } else {
	return 0;
    }
} /* ref_count_cmp */

void
free_ref_count_array (void)
{
    if (Refcount_sym_idx) {
    FREE (Refcount_sym_idx);
    Refcount_sym_idx = 0;
    }
} /* free_ref_count_array */

	/*******************************************************
		Function: sort_symbol_ref_count_array
	 *******************************************************/
void
sort_symbol_ref_count_array(void)
{
    if (!Refcount_sym_idx) {
	register int i;

	INT32 symtab_count = ST_Table_Size ( GLOBAL_SYMTAB );
	Refcount_sym_idx = (AUX_IDX *)MALLOC((symtab_count+1)*sizeof(AUX_IDX));
	MALLOC_ASSERT(Refcount_sym_idx);
	MEMSET(Refcount_sym_idx, 0, symtab_count*sizeof(AUX_IDX));

	for (i=0; i<symtab_count; i++) {
            Refcount_sym_idx[i] = i;
	}
	qsort(Refcount_sym_idx, symtab_count, sizeof(AUX_IDX),
	    (int (*) (const void *, const void *))ref_count_cmp);
    }
}


static inline BOOL
can_be_gp_rel (const ST& st)
{
    return (ST_export(st) != EXPORT_PREEMPTIBLE &&
	    !ST_not_gprel(st) && !ST_gprel(st));
}

static INT
IP_tag_symbol_gp_rel (INT avail_gp_area)
{
    INT IPA_idx;
    INT gp_area_left = avail_gp_area;
    ST_IDX	i;

#define IMMEDIATE_MAX_VALUE	0x7fff

    sort_symbol_ref_count_array();
    
    if (Trace_IPA || Trace_Perf) {
	fprintf (TFile,
		 "Total gp-relative area available for AutoGnum = %"_fmt_v"\n",
		 avail_gp_area);
	if (IPA_user_gnum)
	    fprintf (TFile, "User-specified gnum = %d\n", IPA_user_gnum);
    }


    for (i = 1; i < ST_Table_Size ( GLOBAL_SYMTAB ) && gp_area_left > 0; i++) {

	ST_IDX  next_stx = next_gp_rel_candidate(i);

	if (next_stx == 0)
	    break;
	ST& st = St_Table[next_stx];

	if (ST_sclass(st) == SCLASS_EXTERN || ST_class (st) != CLASS_VAR ||
	    ST_base_idx (st) != next_stx)
	    continue;

	if (IPA_user_gnum && TY_size (ST_type(st) > IPA_user_gnum))
	    break;

	/* we only want those symbols that are
	 * (1) defined in a WHIRL file
	 * (2) used ONLY in WHIRL files
	 * (3) external DATA symbols that are tagged STO_INTERNAL, STO_HIDDEN
	 *     and STO_PROTECTED
	 */

	if (IPA_Enable_DVE && ST_is_not_used (st) )
	    continue;

	if (IPA_Enable_CGI && ST_is_const_var (st) &&
	    ST_is_initialized(st))
	    continue;

        if ((ST_sclass (st) == SCLASS_COMMON ||
	     ST_sclass (st) == SCLASS_UGLOBAL ||
	     ST_sclass (st) == SCLASS_DGLOBAL ||
	     ST_sclass (st) == SCLASS_FSTATIC ||
	     ST_sclass (st) == SCLASS_PSTATIC) &&
	    can_be_gp_rel (st) &&
	    !AUX_ST_flags (Aux_St_Tab[ST_IDX_index(next_stx)],
			   DEF_IN_OBJ|USED_IN_OBJ|USED_IN_DSO)) {

	    /* for COMMON symbols, we don't want its size to be greater
	     * than 0x7fff since GPREL relocation's immediate field
	     * is only 16 bits and any elements in the COMMON that have
	     * offset greater than 0x7fff, the GPREL relocation would not
	     * be able to handle
	     */
	    if (ST_storage_class (st) == SCLASS_COMMON && 
		(TY_size (ST_type(st)) > IMMEDIATE_MAX_VALUE)) {
	        if (Trace_IPA || Trace_Perf)
		    fprintf (TFile, "%s of size (%"_fmt_v") is NOT marked "
			     "gp-relative because it is a COMMON symbol with"
			     " size > 32767(0x7fff)\n",
			     DEMANGLE (ST_name (st)),
			     (INT)TY_size (ST_type(st)));
		continue;
	    }

	    if (gp_area_left < TY_size (ST_type(st))) {
	        if (Trace_IPA || Trace_Perf)
		    fprintf (TFile, "%s of size (%"_fmt_v") is NOT marked gp-relative because there is NO AutoGnum area left\n",
			     DEMANGLE (ST_name (st)), (INT)TY_size (ST_type(st)));
                break;
	    }
	    gp_area_left -= TY_size (ST_type(st));
	    Set_ST_gprel (st);
#ifdef TODO
	    /* Need to port this for Ipa_tlog */
	    IPO_Gprel_Sym_Count++;
#endif

	    if (Trace_IPA || Trace_Perf)
		fprintf (TFile, "%s of size (%"_fmt_v") is marked gp-relative\n",
			 DEMANGLE (ST_name (st)), (INT)TY_size (ST_type(st)));
        } else if (Trace_IPA || Trace_Perf) {
	    fprintf (TFile, "%s of size (%"_fmt_v") is NOT marked"
		     " gp-relative by IPA because ", DEMANGLE (ST_name (st)),
		     (INT)TY_size (ST_type(st))); 
	    if (!(ST_export(st) != EXPORT_LOCAL && 
		  ST_export(st) != EXPORT_LOCAL_INTERNAL)) 
		fprintf (TFile, "it is NOT defined as GLOBAL");
	    if (ST_class (st) != CLASS_VAR) 
		fprintf (TFile, "it is NOT defined as DATA");
	    if (AUX_ST_flags(Aux_St_Tab[ST_IDX_index(next_stx)],
			     USED_IN_OBJ|USED_IN_DSO))
		fprintf (TFile, "it is used by real object");
	    if (!((ST_export(st) == EXPORT_INTERNAL) ||
		  (ST_export(st) == EXPORT_HIDDEN) ||
		  (ST_export(st) == EXPORT_PROTECTED))) {
		fprintf (TFile, "it is preemptible");
	    }
	    if (ST_not_gprel(st))
		fprintf (TFile, "it is marked FORCE_NO_GP_REL");
	    if ( ST_gprel(st) )
		fprintf (TFile, "it is already marked FORCE_GP_REL");
	    fprintf (TFile, ".\n");
  	}
    }
    free_ref_count_array ();

    for (i = 1; i < ST_Table_Size ( GLOBAL_SYMTAB ); ++i) {
	// Now tag the COMMON elements based off a GP-rel COMMON
	ST_IDX idx = make_ST_IDX ( i, GLOBAL_SYMTAB);
	ST& st = St_Table[idx];
	if ((ST_sclass(st) == SCLASS_COMMON) && (ST_base_idx (st) != ST_st_idx (st))) {
	    const ST& base_st = St_Table[ST_base_idx(st)];
	    if (ST_gprel(base_st))
		Set_ST_gprel(st);
	}
    }
    return (avail_gp_area - gp_area_left);
}  /* IP_tag_symbol_gp_rel */


struct count_WHIRL_external_gots
{
    INT& WHIRL_count;
    
    count_WHIRL_external_gots (INT& count) : WHIRL_count (count) {}
    
    void operator() (UINT32, ST* st) const {

	if (ST_is_not_used (st))
	    return;

	AUX_ST& aux_st = Aux_St_Table[ST_st_idx (st)];
	if (AUX_ST_flags (aux_st, (USED_IN_OBJ|USED_IN_DSO))) {
	    ++WHIRL_count;
	    return;
	}
	
	if (ST_is_weak_symbol (st)) {
	    ++WHIRL_count;
	    return;
	}
	
	ST_EXPORT export_class = ST_export(st);
	BOOL address_taken = FALSE;
	ST_SCLASS sclass = ST_sclass(st);

	/* need to check for addr_taken and used_in_obj 
	 * if so, need got
 	 * else not
	 */
	if (ld_ipa_opt[LD_IPA_SHARABLE].flag == F_CALL_SHARED) {
	    /* All defined symbol whose addresses are taken will NOT
	     * have a GOT since those addresses would not be moved 
	     * ie no .dynrel entry will be generated for them
	     */
	    if (sclass != SCLASS_EXTERN)
		address_taken = 0;
	    else
		address_taken = ST_addr_saved (st) || ST_addr_passed (st);
	}

	if (sclass == SCLASS_EXTERN && (export_class == EXPORT_PREEMPTIBLE ||
					address_taken)) {
	    ++WHIRL_count;
	}
    }
}; // count_WHIRL_external_gots

static INT
get_estimate_external_gots (void)
{
    static INT count = -1;

    if (count != -1)
	return (count);

    count = 0;
    count_WHIRL_external_gots WHIRL_ext_gots (count);
    For_all (St_Table, GLOBAL_SYMTAB, WHIRL_ext_gots);
    count += Count_elf_external_gots ();
    return (count);
}


static inline INT 
IP_get_max_gpa_size()
{
    return (max_gpa_size-10000);
}


static INT
IP_estimate_got_size ()
{
    int num_got;
    float factor;
    num_got = get_estimate_external_gots();

    if (IPA_Has_Fortran)		/* need to add more GOTs for
					   intrinsic functions in case */ 
	num_got += IPA_Num_Fortran_Intrinsics;

    if (IPA_Extgot_Factor == 0) {
	IPA_Extgot_Factor = 210 - (UINT32)((float) num_got /
				   ((float) IP_get_max_gpa_size() /
				    sizeof(ELF_ADDR)) * 100);
    }
   
    if (Trace_IPA || Trace_Perf) {
	if (IPA_Has_Fortran) {
	    fprintf (TFile,
		     "Including estimation of FORTRAN INSTRINSIC (total %d), ",
		     IPA_Num_Fortran_Intrinsics);
	}
	fprintf (TFile, "number of estimated external got entries = %d, "
		 "percentage used for estimating the final .got size = %d%%\n",
		 num_got, IPA_Extgot_Factor);
    }
    return (((int)((float)num_got * (float)((float)IPA_Extgot_Factor/100)) +1) * sizeof(ELF_ADDR));
}


static inline UINT
Gp_Area_Size ()
{
    UINT32 max_gpa_size = IP_get_max_gpa_size ();
#ifdef KEY
    return MIN (max_gpa_size, IPA_Gspace);
#else
    return min (max_gpa_size, IPA_Gspace);
#endif
}

static void 
IPO_tag_sym_gp_rel(void)
{
    INT available_gp_area =
	Gp_Area_Size () - IP_estimate_got_size () - used_gp_area;

    if (available_gp_area > 0) {
        available_gp_area -= IP_tag_symbol_gp_rel (available_gp_area);
    }
    
    if (Trace_IPA || Trace_Perf) {
        fprintf (TFile,
		 "Total gp area used = %d, available gp area left = %d\n ",
		 Gp_Area_Size () - available_gp_area, available_gp_area);
    }
}

/* Check to see if the output object needs only ONE GOT
 *   yes -- return TRUE
 *   no  -- return FALSE
 */
static BOOL
IPO_only_one_got(void)
{
    /* no need to do any 03 optimization if there is going to be
     * multiple GOTs generated from ld later
     */
    if ( (IP_estimate_got_size() + used_gp_area) > Gp_Area_Size () ) {
	return FALSE;
    }
    return TRUE;
}

void
Autognum_Driver()
{
    BOOL one_got = FALSE;
    Refcount_sym_idx = 0;
    if (IPA_Enable_Picopt || IPA_Enable_Relocatable_Opt || 
	IPA_Enable_AutoGnum )
        one_got = IPO_only_one_got();

    // turns off Autognum if multigot is on, whether by specification or 
    // calculation , this can be fixed once we know how to deal with
    // tagging symbol gp_relative under multigot
    if (one_got == FALSE) { 
 	IPA_Enable_AutoGnum = FALSE;
	if (Trace_IPA || Trace_Perf) {
	    fprintf  (TFile,
		      "AutoGnum optimization are turned off due to multigot\n");
	}
    }

    if (IPA_Enable_AutoGnum) {
	Temporary_Error_Phase ephase ("IPA Auto GP-relative Data Layout");
	if (Verbose) {
	    fprintf (stderr, "GP-relative data layout ...");
	    fflush (stderr);
	}
	if (Trace_IPA || Trace_Perf) {
	    fprintf (TFile, "\t<<<GP-relative Data Layout begins>>>\n");
	}
	IPO_tag_sym_gp_rel ();
	if (Trace_IPA || Trace_Perf)
	    fprintf  (TFile,
		      "\t<<<Auto GP-relative Data Layout completed>>>\n");
    }
}// autognum_driver
