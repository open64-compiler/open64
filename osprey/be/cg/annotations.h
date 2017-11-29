/*
 * Copyright (C) 2009 Advanced Micro Devices, Inc.  All Rights Reserved.
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



/* =======================================================================
 * =======================================================================
 *
 *  Module: annotations.h
 *  $Revision: 1.9 $
 *  $Date: 05/12/05 08:59:02-08:00 $
 *  $Author: bos@eng-24.pathscale.com $
 *  $Source: /scratch/mee/2.4-65/kpro64-pending/be/cg/SCCS/s.annotations.h $
 *
 *  Description:
 *  ============
 *
 *  General Annotation facility for attaching information to any data
 *  structure. The basic type is ANNOTATION. An annotation list can
 *  be added to a data structure with a field of type (ANNOTATION *).
 *
 *	typedef struct annotation {
 *	  ANNOTATION_KIND kind;
 *        struct annotation *next;
 *	  void *info;
 *      } ANNOTATION;
 *
 *  Thus the information is a pointer sized object 'info' that contains 
 *  a pointer an object of the appropriate type.
 *
 *
 *  When to use Annotations:
 *  ========================
 *
 *  Annotations should be used to attach information that is present only
 *  in a few instances of the data structure. An example is ENTRYINFO
 *  which needs to be present only for entry BBs. Using a BB_MAP for 
 *  this purpose is too wasteful and so is adding a field the BB data
 *  structure for this purpose.
 *
 *  Adding an Annotation:
 *  =====================
 *
 *    ANNOTATION *ANNOT_Add (ANNOTATION *annot_list,
 *			     ANNOTATION_KIND kind,
 *	  		     void *info,
 *	  		     MEM_POOL *pool)
 *
 *	Add an annotation with the given 'kind' and containing 'info'
 *      to the end of 'annot_list'. Memory is allocated from 'pool'
 *      for this new annotation. The return value is the updated list.
 *
 *  Unlinking an Annotation:
 *  ========================
 *
 *    ANNOTATION *ANNOT_Unlink (ANNOTATION *annot_list,
 *			        ANNOTATION *this)
 *
 *  Return a list which does not include the first instance of this.
 *  this is not deleted, and may still point into the original list.
 *
 *  Retreiving Annotations:
 *  =======================
 *
 *    ANNOTATION *ANNOT_Get (ANNOTATION *annot_list, ANNOTATION_KIND kind)
 *
 * 	Get the first annotation of the given 'kind' from the 'annot_list'.
 * 	The return value is NULL, if there is no annotation of this 'kind'.
 *
 *    If there can be more than one annotation of a certain kind, the 
 *    macros ANNOT_First and ANNOT_Next can be used to get the whole
 *    list. For example:
 *
 *	ANNOTATION *ant;
 *
 *	for (ant = ANNOT_First(annot_list, kind); 
 *	     ant != NULL; 
 *	     ant = ANNOT_Next (ant, kind))
 *	{
 *	  use ANNOT_info(ant) ....
 *	}
 *
 * =======================================================================
 */

#ifndef ANNOTATIONS_INCLUDED
#define ANNOTATIONS_INCLUDED

#include "mempool.h"
#include "srcpos.h"
#include "symtab.h"
#include "register.h"

typedef enum {
  ANNOT_LABEL 	  = 0,
  ANNOT_PRAGMA    = 1,
  ANNOT_ENTRYINFO = 2,
  ANNOT_EXITINFO  = 3,
  ANNOT_CALLINFO  = 4,
  ANNOT_NOTE	  = 5,
  ANNOT_LOOPINFO  = 6,
  ANNOT_SWITCH 	  = 7,
  ANNOT_ROTATING_KERNEL = 8,
  ANNOT_ASMINFO   = 9,
  ANNOT_INLINE    = 10
} ANNOTATION_KIND;

class WN;
struct tn;
struct ti_res_count;  // forward declaration

typedef struct annotation {
  ANNOTATION_KIND kind;
  struct annotation *next;
  void *info;
} ANNOTATION;

#define ANNOT_next(a)	((a)->next)
#define ANNOT_info(a)	((a)->info)
#define ANNOT_kind(a)   ((a)->kind)

/* type of information to be returned for the different annotation kinds */
#define ANNOT_label(a)		((LABEL_IDX)(INTPTR)ANNOT_info(a))
#define ANNOT_pragma(a)		((WN *)ANNOT_info(a))
#define ANNOT_entryinfo(a)  	((ENTRYINFO *)ANNOT_info(a))
#define ANNOT_exitinfo(a)  	((EXITINFO *)ANNOT_info(a))
#define ANNOT_callinfo(a)  	((CALLINFO *)ANNOT_info(a))
#define ANNOT_note(a)  		((NOTE *)ANNOT_info(a))
#define ANNOT_loopinfo(a)	((LOOPINFO *)ANNOT_info(a))
#define ANNOT_switch(a)		((ST *)ANNOT_info(a))
#define ANNOT_rotating_kernel(a)   ((ROTATING_KERNEL_INFO*)ANNOT_info(a))
#define ANNOT_asminfo(a)	((ASMINFO *)ANNOT_info(a))
#define ANNOT_inline(a)         ((LABEL_IDX)(INTPTR)ANNOT_info(a))


typedef struct loopinfo {
 WN *wn;			/* LOOP_INFO WHIRL node */
 struct tn *trip_count_tn;	/* TN holding trip count (if any) */
 SRCPOS srcpos;			/* source position of start of body */
 BOOL   is_multiversion;
 BOOL   is_vectorized;          /* loop is vectorized (yes/no) */
 BOOL   is_align_peeled;        /* loop is peeled for alignment (yes/no) */
} LOOPINFO;

#define LOOPINFO_wn(x)			((x)->wn)
#define LOOPINFO_srcpos(x)		((x)->srcpos)
#define LOOPINFO_line(x)		(Srcpos_To_Line(LOOPINFO_srcpos(x)))
#define LOOPINFO_trip_count_tn(x)	((x)->trip_count_tn)
#define LOOPINFO_multiversion(x)	((x)->is_multiversion)
#define LOOPINFO_vectorized(x)          ((x)->is_vectorized)
#define LOOPINFO_align_peeled(x)        ((x)->is_align_peeled)


typedef	struct entryinfo {
  ST *name;		/* entry point name.		     */
  struct op *sp_adj;	/* Entry SP adjustment operation     */
  WN *entry_wn;               /* pointer to entry WN.              */
  SRCPOS    srcpos;	/* first source position of func.    */
} ENTRYINFO;

#define ENTRYINFO_name(x)	((x)->name)
#define ENTRYINFO_sp_adj(x)	((x)->sp_adj)
#define ENTRYINFO_entry_wn(x)   ((x)->entry_wn)
#define ENTRYINFO_srcpos(x)	((x)->srcpos)


typedef struct exitinfo {
  struct op *sp_adj;	/* Exit SP adjustment operation */
  SRCPOS    srcpos;	/* source position of function exit */
} EXITINFO;

#define EXITINFO_sp_adj(x)    ((x)->sp_adj)
#define EXITINFO_srcpos(x)    ((x)->srcpos)

typedef struct callinfo {
  ST *call_st;
  WN *call_wn;
} CALLINFO;

#define CALLINFO_call_st(x)	((x)->call_st)
#define CALLINFO_call_wn(x)	((x)->call_wn)


struct ROTATING_KERNEL_INFO {
  BOOL succeeded;
  INT  failure_code;
  INT ii;
  INT stage_count;
  INT min_ii;
  INT res_min_ii;
  INT rec_min_ii;
  INT sched_len;
  INT min_sched_len;
  struct ti_res_count *res_counts;
  REGISTER_SET live_in[ISA_REGISTER_CLASS_MAX+1];
  REGISTER_SET kill[ISA_REGISTER_CLASS_MAX+1];
  vector<struct tn *> copyin;
  vector<struct tn *> copyout;
  vector<struct tn *> localdef;
};

#define ROTATING_KERNEL_INFO_succeeded(x)     ((x)->succeeded)
#define ROTATING_KERNEL_INFO_failure_code(x)  ((x)->failure_code)
#define ROTATING_KERNEL_INFO_live_in(x)       ((x)->live_in)
#define ROTATING_KERNEL_INFO_kill(x)          ((x)->kill)
#define ROTATING_KERNEL_INFO_ii(x)            ((x)->ii)
#define ROTATING_KERNEL_INFO_stage_count(x)   ((x)->stage_count)
#define ROTATING_KERNEL_INFO_min_ii(x)        ((x)->min_ii)
#define ROTATING_KERNEL_INFO_res_min_ii(x)    ((x)->res_min_ii)
#define ROTATING_KERNEL_INFO_rec_min_ii(x)    ((x)->rec_min_ii)
#define ROTATING_KERNEL_INFO_sched_len(x)     ((x)->sched_len)
#define ROTATING_KERNEL_INFO_min_sched_len(x) ((x)->min_sched_len)
#define ROTATING_KERNEL_INFO_res_counts(x)    ((x)->res_counts)
#define ROTATING_KERNEL_INFO_copyin(x)        ((x)->copyin)
#define ROTATING_KERNEL_INFO_copyout(x)       ((x)->copyout)
#define ROTATING_KERNEL_INFO_localdef(x)      ((x)->localdef)

#ifdef KEY 
typedef struct asminfo {
  REGISTER_SET livein[ISA_REGISTER_CLASS_MAX+1];
  REGISTER_SET liveout[ISA_REGISTER_CLASS_MAX+1];
  REGISTER_SET kill[ISA_REGISTER_CLASS_MAX+1];
} ASMINFO;

#define ASMINFO_livein(x)	((x)->livein)
#define ASMINFO_liveout(x)	((x)->liveout)
#define ASMINFO_kill(x)		((x)->kill)
#endif

extern ANNOTATION *ANNOT_Add (
  ANNOTATION *annot_list, 
  ANNOTATION_KIND kind, 
  void *info,
  MEM_POOL *pool);

extern ANNOTATION *ANNOT_Unlink (
  ANNOTATION *annot_list, 
  ANNOTATION *this1);

extern ANNOTATION *ANNOT_Get (ANNOTATION *annot_list, ANNOTATION_KIND kind);

#define ANNOT_First(list,kind)	(ANNOT_Get (list, kind))
#define ANNOT_Next(list,kind)	(ANNOT_Get (ANNOT_next(list), kind))

/***********************************************************************/
// ASM_OP_ANNOT is an annotation on an OP rather than a BB.
// ANNOT_asminfo on a bb can get you to the asm op,
// then use ASM_OP_* to access these fields.

#define ASM_OP_size	50

struct ASM_OP_ANNOT 
{
  const WN* wn;

  REGISTER_SET clobber_set[ISA_REGISTER_CLASS_MAX+1];

  const char* result_constraint[ASM_OP_size];
  ISA_REGISTER_SUBCLASS result_subclass[ASM_OP_size];
  mUINT32 result_position[ASM_OP_size];
  bool result_clobber[ASM_OP_size];
  bool result_memory[ASM_OP_size];

  const char* opnd_constraint[ASM_OP_size];
  ISA_REGISTER_SUBCLASS opnd_subclass[ASM_OP_size];
  mUINT32 opnd_position[ASM_OP_size];
  bool opnd_memory[ASM_OP_size];
#ifdef KEY
  void* opnd_offset[ASM_OP_size];
#endif
};

#define ASM_OP_wn(x)			((x)->wn)
#define ASM_OP_clobber_set(x)		((x)->clobber_set)
#define ASM_OP_result_constraint(x)	((x)->result_constraint)
#define ASM_OP_result_subclass(x)	((x)->result_subclass)
#define ASM_OP_result_position(x)	((x)->result_position)
#define ASM_OP_result_clobber(x)	((x)->result_clobber)
#define ASM_OP_result_memory(x)		((x)->result_memory)
#define ASM_OP_opnd_constraint(x)	((x)->opnd_constraint)
#define ASM_OP_opnd_subclass(x)		((x)->opnd_subclass)
#define ASM_OP_opnd_position(x)		((x)->opnd_position)
#define ASM_OP_opnd_memory(x)		((x)->opnd_memory)

#ifdef KEY
#define ASM_OP_opnd_offset(x)		((x)->opnd_offset)
#endif
#endif /* ANNOTATIONS_INCLUDED */

