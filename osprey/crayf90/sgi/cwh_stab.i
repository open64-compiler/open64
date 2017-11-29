/*
 * Copyright (C) 2008. PathScale, LLC. All Rights Reserved.
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

/* fe90/i_cvrt.c needs variables crayf90_def_main and def_main_u, but can't
 * handle the rest of this file. */
#if !defined(FOR_I_CVRT)

static OPCODE Intconst_Opcode [4] = {
  OPC_I4INTCONST, OPC_U4INTCONST, OPC_I8INTCONST, OPC_U8INTCONST
};

#define ERR_OP -1

#define I4_OP   0
#define U4_OP   1
#define I8_OP   2
#define U8_OP   3

#define F4_OP   0
#define F8_OP   1
#define FQ_OP   2

#define C4_OP   0
#define C8_OP   1
#define CQ_OP   2


static const mINT8 op_form [MTYPE_LAST + 1] = {
  ERR_OP,  /* MTYPE_UNKNOWN */
  ERR_OP,  /* MTYPE_B   */
  I4_OP,   /* MTYPE_I1  */
  I4_OP,   /* MTYPE_I2  */
  I4_OP,   /* MTYPE_I4  */
  I8_OP,   /* MTYPE_I8  */
  U4_OP,   /* MTYPE_U1  */
  U4_OP,   /* MTYPE_U2  */
  U4_OP,   /* MTYPE_U4  */
  U8_OP,   /* MTYPE_U8  */
  F4_OP,   /* MTYPE_F4  */
  F8_OP,   /* MTYPE_F8  */
  ERR_OP,  /* MTYPE_F10 */
  ERR_OP,  /* MTYPE_F16 */
  ERR_OP,  /* MTYPE_STR */
  FQ_OP,   /* MTYPE_FQ  */
  ERR_OP,  /* MTYPE_M   */
  C4_OP,   /* MTYPE_C4  */
  C8_OP,   /* MTYPE_C8  */
  CQ_OP,   /* MTYPE_CQ  */
  ERR_OP,  /* MTYPE_V   */
};


static const OPCODE Const_Opcode [MTYPE_LAST + 1] = {
  OPCODE_UNKNOWN,    	/* MTYPE_UNKNOWN */
  OPCODE_UNKNOWN,    	/* MTYPE_UNKNOWN */
  OPC_I4INTCONST, 	/* MTYPE_I1 */
  OPC_I4INTCONST, 	/* MTYPE_I2 */
  OPC_I4INTCONST, 	/* MTYPE_I4 */
  OPC_I8INTCONST, 	/* MTYPE_I8 */
  OPC_U4INTCONST, 	/* MTYPE_U1 */
  OPC_U4INTCONST, 	/* MTYPE_U2 */
  OPC_U4INTCONST, 	/* MTYPE_U4 */
  OPC_U8INTCONST, 	/* MTYPE_U8 */
  OPC_F4CONST,    	/* MTYPE_F4 */
  OPC_F8CONST,    	/* MTYPE_F8 */
  OPCODE_UNKNOWN,    	/* MTYPE_F10 */
  OPCODE_UNKNOWN,    	/* MTYPE_F16 */
  OPCODE_UNKNOWN,    	/* MTYPE_STR */
  OPC_FQCONST,    	/* MTYPE_FQ */
  OPCODE_UNKNOWN,    	/* MTYPE_M */
  OPC_C4CONST,    	/* MTYPE_C4 */
  OPC_C8CONST,    	/* MTYPE_C8 */
  OPC_CQCONST,    	/* MTYPE_CQ */
  OPCODE_UNKNOWN,     	/* MTYPE_V */
};

/* Given an OBJECT_SYM, return a ST_Class */

static const ST_CLASS object_map[Name+1] = {
CLASS_UNK,   /* Unknown_Sym    		*/
CLASS_VAR,   /*	User_Variable  		*/
CLASS_VAR,   /*	Dummy_Arg      		*/
CLASS_VAR,   /*	Function_Rslt  		*/
CLASS_VAR,   /*	Compiler_Temp  		*/
CLASS_VAR,   /*	CRI_Pointee    		*/
CLASS_VAR,   /*	Component      		*/
CLASS_UNK,   /*	Vector_Temp    		*/
CLASS_UNK,   /* Vector_Maskk   		*/
CLASS_UNK,   /*	Vector_Iota    		*/
CLASS_UNK,   /*	MPP_object     		*/
CLASS_UNK,   /*	Call_Dummy     		*/
CLASS_VAR,   /*	Dummy_Procedure		*/
CLASS_VAR,   /*	Hosted_User_Variable	*/
CLASS_VAR,   /*	Hosted_Dummy_Arg	*/
CLASS_UNK,   /*	Scratch_Symbol  	*/
CLASS_VAR,   /*	Hosted_Dummy_Procedure	*/
CLASS_VAR,   /*	Hosted_Compiler_Temp	*/
CLASS_NAME,  /*	Name                	*/
} ;


/* Given an SEGMENT_TYPE return a ST_SCLASS */

/* hack for based varbls - will fail in symtab verification */
/* unless ST_SCLASS is changed in fei_object */

#define SCLASS_BASED SCLASS_COUNT  
	

static const ST_SCLASS  segment_map[Seg_Coment+1] = {
SCLASS_UNKNOWN,	/* Seg_Unknown             */
SCLASS_FSTATIC,	/* Seg_Static              */
SCLASS_AUTO,	/* Seg_Stack               */
SCLASS_FORMAL,	/* Seg_Formal              */
SCLASS_COMMON,	/* Seg_Common              */
SCLASS_EXTERN,	/* Seg_Extern              */
SCLASS_DGLOBAL,	/* Seg_Exported            */   
SCLASS_UNKNOWN,	/* Seg_Task_Common         */
SCLASS_UGLOBAL,	/* Seg_Soft_External       */   
SCLASS_UNKNOWN,	/* Seg_Global_Breg         */
SCLASS_UNKNOWN,	/* Seg_Global_Treg         */
SCLASS_PSTATIC,	/* Seg_Static_Name         */
SCLASS_BASED,	/* Seg_Based               */
SCLASS_UNKNOWN,	/* Seg_Equivalenced        */
SCLASS_UNKNOWN,	/* Seg_Restricted          */
SCLASS_FSTATIC,	/* Seg_Distributed         */
SCLASS_UNKNOWN,	/* Seg_LM_Static           */
SCLASS_UNKNOWN,	/* Seg_LM_Common           */
SCLASS_UNKNOWN,	/* Seg_LM_Extern           */
SCLASS_UNKNOWN,	/* Seg_Auxiliary           */
SCLASS_PSTATIC,	/* Seg_Static_Local        */  
SCLASS_AUTO,	/* Seg_Non_Local_Stack     */  
SCLASS_FORMAL,	/* Seg_Non_Local_Formal    */  
SCLASS_AUTO,	/* Seg_Hosted_Stack        */  
SCLASS_UNKNOWN,	/* Seg_Threadprivate       */
SCLASS_COMMENT,	/* Seg_Coment              */  
} ;

/*
  ST of current (ie: last fei_proc_def)  procedure - retained 
  so entry points can be tacked on a LIST associated with the 
  procedure. Also used in DST information, and to set PU flags.
 
  The Alttemp_ST is used for entry points where the 
  result is a pointer: eg: characters, arrays. It holds
  the ST of the first pointer seen.

  The Altbase_ST is used for scalar entry points. If 
  a CQ result, then we may need to create a temp, as
  CQs are passed by address, when Altaddress_ST is the
  original address.

  Main ST is the ST of an EXTERNAL used for DST info in
  main programs with a name..

  
*/

ST * Procedure_ST ;
static ST * Alttemp_ST ;
static ST * Altbase_ST ;
ST * Altaddress_ST ;
static ST * Main_ST;
static BOOL Has_nested_proc ;

#define NOT_IN_PU (CURRENT_SYMTAB == GLOBAL_SYMTAB)

/* a counter, so we initialize the symbol table correctly */
/* Alternate entry points bump the count.                 */

static INT32 entry_point_count = 0 ;

/* 
   list of entry point's STs with SCLASS_TEXT to be used
   for STs in function references, instead of generating
   an EXTERNAL. Essential for internal procedure calls.
   Saves duplicate DSTs, too.
*/

static LIST * Top_Text = NULL ;

/* 
  List of namelist items from fei_name to be added
  to a namelist struct in fei_namelist.
*/

static LIST * Namelist ;


/* 
   list of items handed back to the interface from ST and TY
   creation routines. STB_pkt is defined in cwh_stab.h.
   The list allows deletion after each PU. (sole purpose).

   FIX - memory pool?
*/

static STB_pkt *STB_list ;


/* 
   array of ST's which appeared within an internal procedure 
   as hosted variables. Allows search through these, not
   whole of host's SYMTAB. 
*/

#define HOST_ST_SIZE_CHANGE 512
static INT32 Host_Current_Size = 0;
static ST ** Host_STs =  NULL;
static INT32 Host_Top = -1 ;

/* 
   A list of COMMONS or equivalence STs. The flds of the block
   have to be contiguous. Don't know how many there are, so we
   just accumulate them, and create the block later.

   Equivalences are per PU.
   Hosted_Equivalences are per Host PU, seen within nested routines.
   Commons_Already_Seen are per file ie: COMMONS in the global symtab
   which are candidates for reuse.
*/

static LIST * Equivalences;
static LIST * Hosted_Equivalences;
static LIST * Commons_Already_Seen;


/* flags which control COMMON STs */

BOOL FE_Partial_Split = TRUE ;


/*
  Definitions used to split COMMON blocks.
  into 16K and 512K blocks These are lifted from edvar.c
*/

static INT32   FE_Full_Split_Array_Limit = 16384;

static struct _full_split_st {
  INT64         rel_offset;
  int           delta;
} FE_Full_Split_Limits [] = {

   16 * 1024, 819,
  512 * 1024, 26214
};

typedef struct {
  ST *    fp ;
  INT32   prev_array_index;
  INT64   first_offset;
  INT64   last_offset;
} FIELD_ITEM, *FIELDS;

#define FIELDS_fp(i) (fp_table[i].fp)
#define FIELDS_prev_array_index(i) (fp_table[i].prev_array_index)
#define FIELDS_first_offset(i) (fp_table[i].first_offset)
#define FIELDS_last_offset(i) (fp_table[i].last_offset)

static INT32 FE_Full_Split_Limits_Count = 2;

/* preg / temp used for distribute */
PREG_det preg_for_distribute;
static ST *st_for_distribute_temp;

BOOL cwh_stab_pu_has_globals; /* Were any global symbols seen in the PU */

#endif /* !defined(FOR_I_CVRT) */

/* strings for default MAIN, used by BE/Cray FE */

static  const char * const crayf90_def_main = "main___" ;
static  const char * const def_main_u = "MAIN__" ;
#ifndef KEY
static  char * const def_main   = "MAIN_" ;
#else
/* Bug 2672- use the mangled name in Dwarf output to match the assembly name */
static  const char * const def_main   = "MAIN__" ;
#endif

#if !defined(FOR_I_CVRT)

/* forward references */

static ST *     cwh_stab_earlier_hosted(const char * name);
static void     cwh_stab_enter_hosted(ST * st) ;
static void     cwh_stab_adjust_name(ST * st) ;
static void     cwh_stab_set_linenum(ST *st, INT32 lineno);
static void     cwh_stab_formal_ref(ST * st, BOOL host);
static void     cwh_stab_find_overlaps(FIELDS fp, INT32 nf);
static BOOL     cwh_stab_split_common(ST * c, FIELDS fp_table, INT32 nf) ;
static BOOL     need_to_split (INT64 cur_offset,INT64 base_offset, INT64 rel_offset,int    delta );
static void     cwh_stab_dump_FIELDS(FIELDS fp_table, INT32 ftom, INT32 to) ;
static void     cwh_stab_full_split(ST *c, enum list_name list = l_COMLIST) ;
static void     cwh_stab_emit_split(ST * c, FIELDS fp_table, INT32 from, INT32 to);
static ST *     cwh_stab_split_ST(ST * c, INT64 low_off, INT64 high_off) ;
static ST *     cwh_stab_common_ST(char *name,INT64 size,mUINT16 al);
static void     cwh_stab_altres_offset(ST *st,BOOL hosted) ;
static void     cwh_stab_altres_offset_comp(ST *st,BOOL allF4C4) ;
static ST *     cwh_stab_altentry_temp(char * name, BOOL hosted) ;
static void     cwh_stab_distrib_pragmas(ST * st);
static void cwh_stab_mk_flds(ST *block,enum list_name list);
static void cwh_stab_emit_list(LIST ** lp, enum list_name list, void (*fp) (ST *, enum list_name)) ;
static void cwh_stab_to_list_of_equivs(ST *st, BOOL hosted) ;
static ST*  cwh_stab_earlier_common(char *name_string, BOOL is_dup);
static ST*  cwh_stab_seen_common_element(ST *c, INT64 offset,char * name) ;

static PU_IDX cwh_stab_mk_pu(TY_IDX pty, SYMTAB_IDX level);

static INTPTR fei_proc_parent  ( char          *name_string,
                                 INT32         lineno,
                                 INT32         sym_class,
                                 INT32         unused1,
                                 INT32         num_dum_args,
                                 INT32         parent_stx,
                                 INT32         first_st_idx,
                                 INT32         aux_idx,
                                 TYPE          result_type,
                                 INTPTR        st_idx,
                                 INT64	       flags );

static INTPTR fei_proc_def     ( char          *name_string,
                                 INT32         lineno,
                                 INT32         sym_class,
                                 INT32         Class,
                                 INT32         unused1,
                                 INT32         unused2,
                                 INT32         num_dum_args,
                                 INT32         parent_stx,
                                 INT32         first_st_idx,
                                 INT32         aux_idx,
                                 TYPE          result_type,
                                 INT32         cmcs_node,
                                 INT32         st_idx,
                                 INT64	       flags );

static INTPTR fei_proc_imp     ( INT32         lineno,
                                 const char    *name_string,
                                 INT32         unused1,
                                 INT32         unused2,
                                 INT32         sclass,
                                 INT32         Class,
                                 TYPE          result_type,
                                 INT64         flags);


static void cwh_stab_adjust_base_name(ST * st);

#endif /* !defined(FOR_I_CVRT) */


			     






