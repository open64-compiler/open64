/*
 *  Copyright (C) 2006. QLogic Corporation. All Rights Reserved.
 */

/*
 * Copyright 2002, 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
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


#ifndef erglob_INCLUDED
#define erglob_INCLUDED
#ifdef __cplusplus
extern "C" {
#endif

/* ====================================================================
 * ====================================================================
 *
 * Module: erglob.h
 * $Revision: 1.1.1.1 $
 * $Date: 2005/10/21 19:00:00 $
 * $Author: marcel $
 * $Source: /proj/osprey/CVS/open64/osprey1.0/common/com/erglob.h,v $
 *
 * Revision history:
 *  07-Sep-89 - Original Version
 *  24-Jan-91 - Copied for TP/Muse
 *  21-Oct-91 - Finished removing Josie codes.
 *
 * Description:
 *
 * Define the global error codes for use with the error message handler
 * errors.c.  This file should contain those codes which are common to
 * multiple tools or to the entire compiler (but see also erlib.h and
 * erlink.h).  The associated error descriptors may be found in the
 * associated file erglob.desc.
 *
 * ====================================================================
 * ====================================================================
 */


#ifdef _KEEP_RCS_ID
static char *erglob_rcs_id = "$Source: common/com/SCCS/s.erglob.h $ $Revision: 1.15 $";
#endif /* _KEEP_RCS_ID */

#include "errors.h"	/* Always needed */

/* The error codes in each erxxx.h file should start from some multiple
 * of 1000, which is the phase number.
 */
#define EC_BASE_GLOBAL	EP_GLOBAL*1000

/* The following error code should be the first global error code:
 * it is used to report unrecognized error codes.
 */
#define EC_Undef_Code	EC_BASE_GLOBAL

/* The following provide predefined error codes for lazy users:
 * the message parameters are a string identifying the location
 * of the error, and a second parameter of the indicated type.
 */
#define EC_Misc_Int	EC_Undef_Code+1		/* str, int */
#define EC_Misc_Float	EC_Undef_Code+2		/* str, float */
#define EC_Misc_Double	EC_Undef_Code+3		/* str, double */
#define EC_Misc_Pointer	EC_Undef_Code+4		/* str, pointer */
#define EC_Misc_String	EC_Undef_Code+5		/* str, str */
#define EC_Misc_Strtab	EC_Undef_Code+6		/* str, strtab */
#define EC_Misc_Symtab	EC_Undef_Code+7		/* str, symtab */
#define EC_Misc_TN	EC_Undef_Code+8		/* str, tn */
#define EC_Misc_Node	EC_Undef_Code+9		/* str, treenode */

/* Give up on signal: */
#define EC_Signal	EC_Undef_Code+10	/* str, str */

/* Give up once there are too many errors: */
#define EC_Too_Many	EC_Undef_Code+11	/* int */

/* Unimplemented/obsolete functions: */
#define EC_Unimplemented EC_Undef_Code+12	/* str */
#define EC_Obsolete	EC_Undef_Code+13	/* str, str */

/* Assertion failures: */
#define EC_Assertion	EC_Undef_Code+14	/* s, int, s */
/* assertion failure before any error messages */
#define EC_Bad_Assertion EC_Undef_Code+15	/* s, int, s */

/* Memory management errors: */
#define EC_Null_Addr	EC_Undef_Code+16	/* str */
#define EC_No_Mem	EC_Undef_Code+17	/* str */
#define EC_Mem_Leak	EC_Undef_Code+18	/* str */

/* Invalid case reached in a switch statement: */
#define EC_Invalid_Case EC_Undef_Code+19	/* str, int */

/* Warning of unimplemented optimization: */
#define EC_Development_Warning EC_Undef_Code+20

#ifdef KEY
/* Error for unimplemented features: */
#define EC_Unimplemented_Feature EC_Undef_Code+24 /* str */
/* Abort due to misc user-caused reason: */
#define EC_Misc_User_Abort       EC_Undef_Code+25 /* str */
#endif // KEY

/* Trace package errors: */
#define EC_Trace_Func	EC_Undef_Code+30	/* int */
#define EC_Trace_Phase	EC_Undef_Code+31	/* int, int, int */
#define EC_Trace_BBs	EC_Undef_Code+32	/* int */
#define EC_Trace_PUs	EC_Undef_Code+33	/* str */
#define EC_Trace_REGIONs EC_Undef_Code+34	/* str */
#define EC_Trace_Open	EC_Undef_Code+35	/* str, syserr */
#define EC_Trace_Control EC_Undef_Code+36	/* int, int */

/* Command line processing: */
#define EC_Flag_Digit	EC_Undef_Code+40	/* int */
#define EC_Flag_Range	EC_Undef_Code+41	/* int,int,int,str */
#define EC_Flag_Int_Expected EC_Undef_Code+42	/* str */
#define EC_List_Flag	EC_Undef_Code+43	/* int, str */
#define EC_Trace_Flag	EC_Undef_Code+44	/* int, str */
#define EC_Target_Flag	EC_Undef_Code+45	/* int, str */
#define EC_Target_Dup	EC_Undef_Code+46	/* int, str */
#define EC_Unknown_Flag	EC_Undef_Code+47	/* int, str */
#define EC_File_Name	EC_Undef_Code+48	/* int, str */
#define EC_File_Flag	EC_Undef_Code+49	/* int, str */
#define EC_Arg_Flag	EC_Undef_Code+50	/* int, str */
#define EC_Dir_Flag	EC_Undef_Code+51	/* int, str */
#define EC_Flag_Opt	EC_Undef_Code+52	/* int, str */
#define EC_No_Sources	EC_Undef_Code+53	/* none */
#define EC_Not_In_Grp	EC_Undef_Code+54	/* str, str, str */
#define EC_Ambig_In_Grp	EC_Undef_Code+55	/* str, str, str */
#define EC_Inv_Grp_Val	EC_Undef_Code+56	/* str, str, str, str */
#define EC_Fix_g_O	EC_Undef_Code+57	/* none */
#define EC_GOT_Size	EC_Undef_Code+58	/* str */
#define EC_Inv_SpecFile	EC_Undef_Code+59	/* str, str */
#define EC_SpecFile_Opt	EC_Undef_Code+60	/* str, str, str */
#define EC_Opt_Conflict	EC_Undef_Code+61	/* str, str, str */
#define EC_Obsolete_Opt	EC_Undef_Code+62	/* str */
#define EC_Replaced_Opt	EC_Undef_Code+63	/* str, str */
#define EC_Unimp_Opt	EC_Undef_Code+64	/* str */
#define EC_Inv_Opt_Val	EC_Undef_Code+65	/* str */
#ifdef KEY
#define EC_No_Opt_Val   EC_Undef_Code+66	/* str, str */
#define EC_No_Apo_Early_Mp   EC_Undef_Code+67	/* none */
#endif // KEY

/* Control processing: */
#define EC_Unimp_Ctrl	EC_Undef_Code+70	/* str */
#define EC_Unrec_Group	EC_Undef_Code+71	/* str */
#define EC_Ctrl_Syntax	EC_Undef_Code+72	/* str */
#define EC_Ctrl_Paren	EC_Undef_Code+73	/* str */
#define EC_Unimp_Align	EC_Undef_Code+74	/* str,int */
#define EC_Unimp_Actrl	EC_Undef_Code+75	/* str */
#define EC_Inv_Ctrl_Val	EC_Undef_Code+76	/* str,str */
#define EC_Ctrl_Numeric	EC_Undef_Code+77	/* str */
#define EC_Ctrl_Integer	EC_Undef_Code+78	/* str */
#define EC_Ctrl_Range	EC_Undef_Code+79	/* int,str,int,int */
#define EC_Group_Range	EC_Undef_Code+80	/* int,str,int,int */
#define EC_Group_Mult	EC_Undef_Code+81	/* str */
#define EC_File_Scope	EC_Undef_Code+82	/* str */
#define EC_Routine_Scope EC_Undef_Code+83	/* str */
#define EC_Unimp_Once	EC_Undef_Code+84	/* str */
#define EC_Change_AA	EC_Undef_Code+85	/* str */
#define EC_Change_BE	EC_Undef_Code+86	/* str */
#define EC_Override	EC_Undef_Code+87	/* str,str */
#define EC_Inv_Ctrl_Chg	EC_Undef_Code+88	/* symtab */

/* Bit vector support errors: */
#define EC_BV_Invalid	EC_Undef_Code+90	/* int, str, int */
#define EC_BV_Length	EC_Undef_Code+91	/* str */
#define EC_Zero_Input	EC_Undef_Code+92	/* str */
#define EC_Not_Power_2	EC_Undef_Code+93	/* str, int */

/* Olimit support errors: */
#define EC_Olimit_Exceeded	EC_Undef_Code+100	/* str, int */
#define EC_File_Olimit_Exceeded	EC_Undef_Code+101	/* int */
#define EC_Not_Optimized	EC_Undef_Code+102	/* str, int */
#define EC_LNO_Backoff		EC_Undef_Code+103	/* str, int, int */
#define EC_ORI_Invoked		EC_Undef_Code+104	/* str, int */
#define EC_Region_Skipped	EC_Undef_Code+105	/* int */

/* Lower optimization for non-ANSI setjmp support */
#define EC_Not_Ansi_Setjmp      EC_Undef_Code+106       /* str, int, int */

#ifdef KEY
/* Olimit support warnings: */
#define EC_Olimit_Slow		EC_Undef_Code+107	/* str, int */
/* IPA inconsistent options warning: */
#define EC_Ipa_Options		EC_Undef_Code+108	/* */
/* Same symbol declared as function and variable error from IPA */
#define EC_Inc_Types		EC_Undef_Code+109	/* str */
#endif

/* Tree (ND) support: */
#define EC_B_Access	EC_Undef_Code+110	/* int, int */
#define EC_Ill_Tree_Op	EC_Undef_Code+111	/* str, str */
#define EC_Bad_Tree	EC_Undef_Code+112	/* tree, tree, str */
#define EC_Bad_Node	EC_Undef_Code+113	/* same as EC_Bad_Tree */
#define EC_Null_Tree	EC_Undef_Code+114	/* str */
#define EC_Inv_Treenum	EC_Undef_Code+115	/* int, tree, tree, int */

#ifdef KEY
#define EC_Bad_Omp	EC_Undef_Code+116	/* str */
#define EC_Ill_Alias	EC_Undef_Code+117	/* str, str, str */
#endif /* KEY */

/* Region errors: */
#define EC_Rgn_Ill_Entry	EC_Undef_Code+120	/* str */
#define EC_Rgn_Ill_Exit		EC_Undef_Code+121	/* str */

/* Symbol table (TY, FLD, ST, etc.) support: */
#define EC_Ill_Scope	EC_Undef_Code+130	/* int, str */
#define EC_Ill_Align	EC_Undef_Code+131	/* int, str */
#define EC_Null_TY	EC_Undef_Code+132	/* str */
#define EC_Null_FLD	EC_Undef_Code+133	/* str */
#define EC_Null_ST	EC_Undef_Code+134	/* str */
#define EC_Inv_ST	EC_Undef_Code+135	/* sym, str */
#define EC_Sym_Class	EC_Undef_Code+136	/* str, sym, str */
#define EC_Acc_Class	EC_Undef_Code+137	/* sym, str, str */
#define EC_Acc_Class2	EC_Undef_Code+138	/* sym, str, str, str */
#define EC_Acc_Class3	EC_Undef_Code+139	/* sym, str, str, str, str */
#define EC_Sym_Sclass	EC_Undef_Code+140	/* str, sym, str */
#define EC_Acc_Sclass	EC_Undef_Code+141	/* sym, str, str */
#define EC_Acc_Aform	EC_Undef_Code+142	/* sym, str, str */
#define EC_Inv_TY	EC_Undef_Code+143	/* sym, str */
#define EC_Typ_Kind	EC_Undef_Code+144	/* str, str, str */
#define EC_Typ_Size	EC_Undef_Code+145	/* str, int, str */
#define EC_Acc_Kind	EC_Undef_Code+146	/* str, int, str, str */
#define EC_Acc_Kind2	EC_Undef_Code+147	/* str, int, str, str, str */
#define EC_Trunc_Sym	EC_Undef_Code+148	/* int, str */
#define EC_No_Scope	EC_Undef_Code+149	/* int, str */
#define EC_Bad_Scope	EC_Undef_Code+150	/* int, str */
#define EC_Inv_Slink	EC_Undef_Code+151	/* sym, str */

/* BB/INS/TN support errors: */
#define EC_Ill_BB_Kind	EC_Undef_Code+170	/* int, int */
#define EC_Ill_BB_Cond	EC_Undef_Code+171	/* int, int, str */
#define EC_PU_BB_Count	EC_Undef_Code+172	/* int, int, str */
#define EC_Null_Goto	EC_Undef_Code+173	/* int, str */
#define EC_TN_Count	EC_Undef_Code+176	/* str */
#define EC_Out_Of	EC_Undef_Code+177	/* str, str */
#define EC_TN_Size	EC_Undef_Code+178	/* int */
#define EC_Null_XTN	EC_Undef_Code+179	/* tn, int, str */
#define EC_Ill_XTN	EC_Undef_Code+180	/* tn, int */
#define EC_XTN_Kind	EC_Undef_Code+181	/* tn, int, str */
#define EC_Dup_XTN	EC_Undef_Code+182	/* tn */
#define EC_Inv_Match_TN	EC_Undef_Code+183	/* tn, tn */
#define EC_Null_TN	EC_Undef_Code+184	/* str */

/* Constant handling: */
#define EC_Ill_Divide	EC_Undef_Code+200	/* none */
#define EC_Ill_Modulus	EC_Undef_Code+201	/* none */
#define EC_Ill_UDivide	EC_Undef_Code+202	/* none */
#define EC_Ill_UModulus	EC_Undef_Code+203	/* none */
#define EC_Large_Const	EC_Undef_Code+204	/* str */
#define EC_Exp_Oflow	EC_Undef_Code+205	/* int, int */
#define EC_Ill_Int_Oflow EC_Undef_Code+206	/* int, str, int */
#define EC_Ill_Quad_Const EC_Undef_Code+207	/* int, int, int, int */

/* TDT manipulation: */
#define EC_TDT		EC_Undef_Code+210	/* str */
#define EC_Bad_Segment	EC_Undef_Code+211	/* int, str */
#define EC_Bad_Base	EC_Undef_Code+212	/* sym, int, str */
#define EC_Inv_RClass	EC_Undef_Code+213	/* str, str */
#define EC_Inv_Mtype	EC_Undef_Code+215	/* str, str */
#define EC_Quad_Unimpl	EC_Undef_Code+217	/* str */

/* Elf and Dwarf Generation. */
#define EC_Elf_Idx	EC_Undef_Code+221	/* int, str */
#define EC_Elf_Align	EC_Undef_Code+222	/* int, int */
#define EC_Elf_Error	EC_Undef_Code+223	/* str */
#define EC_Elf_Size64   EC_Undef_Code+224       /* int64, str */
#define EC_Elf_Ofst64   EC_Undef_Code+225       /* int64, str */

/* IR writer/reader (mtob/btom): */
#define EC_Neg_Treenum	EC_Undef_Code+231	/* int, tree, str */
#define EC_Out_Of_Sync	EC_Undef_Code+232	/* str, int */
#define EC_Excess_Count	EC_Undef_Code+233	/* str, str, int */
#define EC_Seek_DotB	EC_Undef_Code+234	/* str, int, int32, syserr */
#define EC_Read_DotB	EC_Undef_Code+235	/* int, int, str, syserr */
#define EC_Write_DotB	EC_Undef_Code+236	/* int, int, str, syserr */

/* Configuration: */
#define EC_Inv_Target	EC_Undef_Code+240	/* str, int */
#define EC_Inv_TARG	EC_Undef_Code+241	/* str, str */
#define EC_Incons_TARG	EC_Undef_Code+242	/* str, str, str, str */
#define EC_Inv_FPRs	EC_Undef_Code+243	/* int */
#define EC_FPR_16	EC_Undef_Code+244	/* none */
#define EC_FPR_32	EC_Undef_Code+245	/* none */
#define EC_Inv_OPT	EC_Undef_Code+246	/* str, str */
#ifdef KEY
#define EC_Inv_x87_Prec	EC_Undef_Code+247	/* int */
#endif

/* Pragma errors: */
#define EC_Pragma_Scope	EC_Undef_Code+270	/* str, str */
#define EC_Is_Bad_Pragma_Abort    EC_Undef_Code+271 /* str, str, str */

/* Constant folding errors: */
#define EC_Zero_And	EC_Undef_Code+290	/* int */


/* Command-line SWP option processing */
#define EC_Ambig_P_Heur	EC_Undef_Code+320	/* str */
#define EC_Not_P_Heur	EC_Undef_Code+321	/* str */
#define EC_Bad_P_Heur_S	EC_Undef_Code+322	/* str */
#define EC_P_Heur_No_II_BT EC_Undef_Code+323	/* str */
#define EC_P_Heur_No_II_MU EC_Undef_Code+324	/* str */
#define EC_P_Heur_No_II_RA EC_Undef_Code+325	/* str */
#define EC_P_Heur_No_BT	EC_Undef_Code+326	/* str */
#define EC_P_Heur_No_MU	EC_Undef_Code+327	/* str */
#define EC_P_Heur_No_RA	EC_Undef_Code+328	/* str */

#ifdef KEY
/* ASM operands: */
#define EC_Inv_Asm_Opnd EC_Undef_Code+340       /* none */
#define EC_Misc_Asm EC_Undef_Code+341		/* str */
#endif
/* Symtab Alias Storage Verfication: */
#define EC_Sym_Sto_Cla EC_Undef_Code+354        /* str,str */



/* ====================================================================
 *
 * File manipulation error codes
 *
 * ====================================================================
 */

/* Define the initial error code to use: */
#define EC_BASE_FILE	EC_BASE_GLOBAL+400

/* File manipulation error codes: */
#define EC_Src_Exists	EC_BASE_FILE		/* str */
#define EC_Src_Open	EC_BASE_FILE+1		/* str, err */
#define EC_Src_Create	EC_BASE_FILE+2		/* str, err */
#define EC_Src_Delete	EC_BASE_FILE+3		/* str, err */
#define EC_Src_Close	EC_BASE_FILE+4		/* str, err */
#define EC_No_Src	EC_BASE_FILE+5		/* str */

#define EC_Trc_Exists	EC_BASE_FILE+6		/* str */
#define EC_Trc_Open	EC_BASE_FILE+7		/* str, err */
#define EC_Trc_Create	EC_BASE_FILE+8		/* str, err */
#define EC_Trc_Delete	EC_BASE_FILE+9		/* str, err */
#define EC_Trc_Close	EC_BASE_FILE+10		/* str, err */
#define EC_No_Trc	EC_BASE_FILE+11		/* str */

#define EC_Lst_Exists	EC_BASE_FILE+12		/* str */
#define EC_Lst_Open	EC_BASE_FILE+13		/* str, err */
#define EC_Lst_Create	EC_BASE_FILE+14		/* str, err */
#define EC_Lst_Delete	EC_BASE_FILE+15		/* str, err */
#define EC_Lst_Close	EC_BASE_FILE+16		/* str, err */
#define EC_No_Lst	EC_BASE_FILE+17		/* str */

#define EC_Cpp_Exists	EC_BASE_FILE+18		/* str */
#define EC_Cpp_Open	EC_BASE_FILE+19		/* str, err */
#define EC_Cpp_Create	EC_BASE_FILE+20		/* str, err */
#define EC_Cpp_Delete	EC_BASE_FILE+21		/* str, err */
#define EC_Cpp_Close	EC_BASE_FILE+22		/* str, err */
#define EC_No_Cpp	EC_BASE_FILE+23		/* str */

#define EC_IR_Exists	EC_BASE_FILE+24		/* str */
#define EC_IR_Open	EC_BASE_FILE+25		/* str, err */
#define EC_IR_Create	EC_BASE_FILE+26		/* str, err */
#define EC_IR_Delete	EC_BASE_FILE+27		/* str, err */
#define EC_IR_Close	EC_BASE_FILE+28		/* str, err */
#define EC_IR_Write	EC_BASE_FILE+29		/* str, err */
#define EC_IR_Magic	EC_BASE_FILE+30		/* int, str */
#define EC_IR_Revision	EC_BASE_FILE+31		/* str, str */
#define EC_No_IR	EC_BASE_FILE+32		/* str */
#define EC_IR_Scn_Read	EC_BASE_FILE+33		/* str, str */
#define EC_IR_Scn_Write	EC_BASE_FILE+34		/* str, str */

#define EC_Ipa_Exists	EC_BASE_FILE+35		/* str */
#define EC_Ipa_Open	EC_BASE_FILE+36		/* str, err */
#define EC_Ipa_Create	EC_BASE_FILE+37		/* str, err */
#define EC_Ipa_Delete	EC_BASE_FILE+38		/* str, err */
#define EC_Ipa_Close	EC_BASE_FILE+39		/* str, err */
#define EC_No_Ipa	EC_BASE_FILE+40		/* str */
#define EC_Inv_Ipa	EC_BASE_FILE+41		/* str */
#define EC_Obs_Ipa	EC_BASE_FILE+42		/* str */
#define EC_Ipa_Rename   EC_BASE_FILE+43         /* str, str */
#ifdef KEY
#define EC_Ipa_Infile   EC_BASE_FILE+44         /* str */
#define EC_Ipa_Outfile  EC_BASE_FILE+45         /* str */
#endif

#define EC_Asm_Exists	EC_BASE_FILE+50		/* str */
#define EC_Asm_Open	    EC_BASE_FILE+51		/* str, err */
#define EC_Asm_Create	EC_BASE_FILE+52		/* str, err */
#define EC_Asm_Delete	EC_BASE_FILE+53		/* str, err */
#define EC_Asm_Close	EC_BASE_FILE+54		/* str, err */
#define EC_No_Asm	    EC_BASE_FILE+55		/* str */
#ifdef KEY
#define EC_Asm_Write	    EC_BASE_FILE+56	/* str */
#endif

#define EC_X_Exists	EC_BASE_FILE+60		/* str */
#define EC_X_Open	EC_BASE_FILE+61		/* str, err */
#define EC_X_Create	EC_BASE_FILE+62		/* str, err */
#define EC_X_Delete	EC_BASE_FILE+63		/* str, err */
#define EC_X_Close	EC_BASE_FILE+64		/* str, err */
#define EC_No_X		EC_BASE_FILE+65		/* str */

/* Cpp invocation error codes: */
#define EC_Cpp_Prep	EC_BASE_FILE+70		/* str */
#define EC_Cpp_Exec	EC_BASE_FILE+71		/* str, err */

/* Linker invocation error codes: */
#define EC_Link         EC_BASE_FILE+75		/* none */
#define EC_Link_Exec	EC_BASE_FILE+76		/* err */

/* Transformation log file error codes: */
#define EC_Tlog_Exists	EC_BASE_FILE+80		/* str */
#define EC_Tlog_Open	EC_BASE_FILE+81		/* str, err */
#define EC_Tlog_Create	EC_BASE_FILE+82		/* str, err */
#define EC_Tlog_Delete	EC_BASE_FILE+83		/* str, err */
#define EC_Tlog_Close	EC_BASE_FILE+84		/* str, err */
#define EC_No_Tlog	    EC_BASE_FILE+85		/* str */

/* Feedback file error codes: */
#define EC_FB_File_Fmt	EC_BASE_FILE+90		/* str, str */
#define EC_FB_Dup_Scn	EC_BASE_FILE+91		/* str, str */
#define EC_FB_Miss_Scn	EC_BASE_FILE+92		/* str, str */
#define EC_FB_Unk_Scn	EC_BASE_FILE+93		/* str */
#define EC_FB_Ent_Size	EC_BASE_FILE+94		/* str, str, int, int */
#define EC_FB_File_Old  EC_BASE_FILE+95	 	/* str */
#ifdef KEY
#define EC_FB_No_File   EC_BASE_FILE+96  	/* str */
#endif

/* CIF file error codes: */
#define EC_Cif_Open     EC_BASE_FILE+100	/* str, err */
#define EC_Cif_Write    EC_BASE_FILE+101	/* str, err */
#define EC_Cif_Close    EC_BASE_FILE+102	/* str, err */
#define EC_GI_Fork      EC_BASE_FILE+103	/* str, err */
#define EC_GI_Exec      EC_BASE_FILE+104	/* str, err */

/* IPA LNO file error codes: */ 
#define EC_IPALNO_Create    EC_BASE_FILE+110    /* str, err */ 
#define EC_IPALNO_Close     EC_BASE_FILE+111	/* str, err */ 
#define EC_IPALNO_Open      EC_BASE_FILE+112	/* str, err */ 
#define EC_IPALNO_Revision  EC_BASE_FILE+113	/* str, err */ 

/* IPA dladd error codes: */
#define EC_IP_Load_Dso  EC_BASE_FILE+120    /* str, str */


#ifdef __cplusplus
}
#endif
#endif /* erglob_INCLUDED */
