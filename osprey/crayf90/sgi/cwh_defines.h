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


/* ====================================================================
 * ====================================================================
 *
 * Module: cwh_defines.h
 * $Revision: 1.5 $
 * $Date: 04/12/21 14:57:32-08:00 $
 * $Author: bos@eng-25.internal.keyresearch.com $
 * $Source: /home/bos/bk/kpro64-pending/crayf90/sgi/SCCS/s.cwh_defines.h $
 *
 * Revision history:
 *  dd-mmm-95 - Original Version
 *
 * Description: Holds translation defintions used by all modules.
 *
 * ====================================================================
 * ====================================================================
 */

#ifndef CWH_DEFINES_INCLUDED
#define CWH_DEFINES_INCLUDED

#ifdef _KEEP_RCS_ID
static char *rcs_id = "$Source: /home/bos/bk/kpro64-pending/crayf90/sgi/SCCS/s.cwh_defines.h $ $Revision: 1.5 $";
#endif /* _KEEP_RCS_ID */


/* dubious casts and types for use with PDGCS */

typedef INT64 OFFSET_64 ; 
typedef unsigned long ULONG ;
typedef long SLONG ;
#if ((defined(TARG_IA64) || defined(TARG_X8664)) && defined(_LP64))
#define cast_to_TY(x) ((TY_IDX)x) 
#define cast_to_WN(x) ((WN *) (void *)x) 
#define cast_to_ST(x) ((ST *) (void *)x) 
#define cast_to_LB(x) ((LABEL_IDX)x) 
#define cast_to_uint(x) ((unsigned int) x)
#define cast_to_int(x) ((int)x)
#define cast_to_long(x) ((long)(void*)x)
#define cast_to_void(x) ((void *)x)
#define cast_to_STB(x) ((STB_pkt *) (void *)x)
#else
#define cast_to_TY(x) ((TY_IDX) (INTPTR)(void *)x) 
#define cast_to_WN(x) ((WN *) (void *)x) 
#define cast_to_ST(x) ((ST *) (void *)x) 
#define cast_to_LB(x) ((LABEL_IDX) (INTPTR) (void *)x) 
#define cast_to_uint(x) ((unsigned long) (INTPTR) (void *)x)
#define cast_to_int(x) ((long ) (void *)x)
#define cast_to_long(x) ((long)(void*)x)
#define cast_to_void(x) ((void *)x)
#define cast_to_STB(x) ((STB_pkt *) (void *)x)
#endif


/* dump macros */

#define DUMP_TY(ty) dump_ty(ty)
#define DUMP_ST(st) dump_st(st)
#define DUMP_LB(lb) dump_label(lb)
#define DUMP_WN(wn) fdump_tree(stdout,wn);
#define DUMP_STK cwh_stk_dump()


extern void dump_ty (TY_IDX ty);
extern void dump_st (ST * st);
extern void dump_label(LABEL_IDX lb);

/* macros for operations on PDGCS arguments */

#define bit_to_byte(a) (a >> 3)
#define bit_to_byte_WN(a) (cwh_expr_bincalc(OPR_DIV,WN_COPY_Tree(a),WN_CreateIntconst (OPC_U4INTCONST,8)))
#define byte_to_bit(a) (a << 3)
#define test_flag(f,sh) ((INT32)((f >> sh) & 1))

/* Symbol table levels */

#define GLOBAL_LEVEL 1
#define HOST_LEVEL GLOBAL_LEVEL + 1
#define INTERNAL_LEVEL HOST_LEVEL + 1

/* Handy macros */

#define WNOPR(w) (WN_operator(w))
#define WNRTY(w) (WN_rtype(w))
#define IS_GLOBAL_SYMTAB(s) (SYMTAB_level(s) == GLOBAL_LEVEL)
#define IN_HOST_PU (CURRENT_SYMTAB == HOST_LEVEL)
#define IN_NESTED_PU (CURRENT_SYMTAB >= INTERNAL_LEVEL)

#define RESULT_SIZE 16
#define RESULT_ALIGN 8


/*
   A WN and TY pair. The  TY is that of the item the WN addresses because
   when a structure component is addressed,  the TY of the LDA/LDID is the
   structure's, so the component's TY is recorded here.
*/

typedef struct {
  WN * wn ;           
  TY_IDX  ty ;
} W_node ; 

#define W_wn(a) (a.wn)
#define W_ty(a) (a.ty)
#define iW_wn(a) (a->wn)
#define iW_ty(a) (a->ty)


/* 
    structures to associate a list of ST's with a parent
    ST, eg: the elements of a COMMON with the common ST,
    or alternate entry points of a procedure. 
    Just for DST information. 
*/

typedef struct blist {
		ST * element ;
		struct blist * nxt ;
} ITEM ;	

#define I_element(i)  ((i)->element)
#define I_next(i)     ((i)->nxt)

typedef struct clist {
		ITEM * first  ;
		ITEM * last   ;
	        INT32  nitems ;
} LIST ;	

#define L_first(l)	((l)->first)
#define L_last(l)	((l)->last)
#define L_num(l)	((l)->nitems)


/* local constants */

#define OPC_UNKNOWN 0
#define MAX_ARY_DIMS 7
#define NUM_LOG_KINDS 4

#define ANULL -1

/* Memory management */
extern MEM_POOL *FE_Mempool;


/* 
 * Calling convention for structure results. TODO: fix in FE.
 *
 * Would like to defer manipulation of ABI calling convention to BE. Unfortunately
 * both the MIPS and IA64 ABIs want to return small structure results in registers.
 * The BE handles this, but for structure results where the structure contains a 
 * dope vector, the FE doesn't know how to initialize the dope (with eg: n_dim,type,
 * flags), if the function result is not passed as the first dummy argument.
 * It's easier to fix this in the conversion to whirl for now, & the macros
 * below control this. Example...
 *
 *  TYPE VARYING_STRING
 *      CHARACTER,DIMENSION(:),POINTER :: chars
 *  ENDTYPE VARYING_STRING
 *
 *  FUNCTION s_concat_c(....
 *      type(VARYING_STRING)            :: s_concat_c
 *      ALLOCATE(s_concat_c%chars(1:la+lb))
 *      iia = SIZE(s_concat_c%chars)
 *
 * Note this isn't in globals.m, because the size of the dope could be smaller than
 * the struct-in-regs size. (eg: scalar pointer on IA64). In that case, the result
 * will have to be passed as the 1st dummy.
 */

#undef RESULT_SIZE 

#ifdef TARG_MIPS
#define RESULT_SIZE 16
#endif
#ifdef TARG_IA32
#if defined(BUILD_OS_DARWIN)
/* Darwin -m32 returns small structures via registers */
#define RESULT_SIZE 8
#else /* defined(BUILD_OS_DARWIN) */
#define RESULT_SIZE 0
#endif /* defined(BUILD_OS_DARWIN) */
#endif
#ifdef TARG_IA64 
#define RESULT_SIZE 64
#endif
#if defined(TARG_X8664) || defined(TARG_LOONGSON)
#define RESULT_SIZE 16
#endif

#define STRUCT_BY_VALUE(ty) ((TY_kind(ty) == KIND_STRUCT)      \
			     && (TY_size(ty) <= RESULT_SIZE)   \
                             && !cwh_types_contains_dope(ty)) 


#endif /* CWH_DEFINES_INCLUDED */

