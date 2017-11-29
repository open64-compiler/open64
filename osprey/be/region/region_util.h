/*
 * Copyright (C) 2008-2010 Advanced Micro Devices, Inc.  All Rights Reserved.
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


/*-*-c++-*-*/
/* ====================================================================
 *
 * Module: region_util.h
 * $Revision: 1.2 $
 * $Date: 02/11/07 23:41:58-00:00 $
 * $Author: fchow@keyresearch.com $
 * $Source: /scratch/mee/2.4-65/kpro64-pending/be/region/SCCS/s.region_util.h $
 *
 * Revision history:
 *  31-MAY-95 wdl - Original Version
 *
 * Description:
 *	Definitions for region stuff in back-end.
 * ====================================================================*/

#ifndef	region_util_INCLUDED
#define	region_util_INCLUDED

#include "defs.h"                /* for wn_core.h */
#include "mempool.h"             /* for MEM_POOL  */
#include "opcode.h"              /* for wn_core.h */
#include "opcode_core.h"         /* for wn_core.h */
#include "opcode_gen_core.h"     /* for wn_core.h */
#include "srcpos.h"
#include "wn_core.h"             /* for wn_map.h  */
#include "wn_map.h"              /* for WN_MAP    */
#include "preg_list.h"           /* for PREG_LIST */
#include "stab.h"		 /* for ST	  */
#include "wn_lower.h"		 /* for LOWER_ACTIONS */
#ifdef __cplusplus
class POINTS_TO;
#else
#define POINTS_TO void           /* Evil Hack because POINTS_TO is in C++ */
#endif

#ifdef __cplusplus
extern "C" {
#endif

/* structures defined elsewhere */
struct cgrin;

/* warning: the order of this type must correspond to phase order */
/* any changes here should also be made to region_util.c, RID_level_str() */
typedef	enum {
  RL_UNKNOWN,   /* unknown REGION level				*/
  RL_SRC,       /* processed to source level (source to source)	*/
  RL_MP,	/* processed by MPC (source level)		*/
  RL_RGN_INIT,  /* processed by REGION_initialize in driver	*/
  RL_IPA_PREOPT,/* processed by IPA controlled Preopt		*/
  RL_LNO_PREOPT,/* processed by LNO controlled Preopt		*/
  RL_LNO1_PREOPT, /* processed by LNO controlled Preopt	        */
  RL_LNO,	/* processed by LNO				*/
  RL_DU_PREOPT,	/* processed by LNO DU checking			*/
  RL_RAIL,	/* processed by RAIL				*/
  RL_RBI,	/* processed by RBI				*/
  RL_PREOPT,    /* processed by Mainopt's Preopt emitter	*/
  RL_MAINOPT,	/* processed by Mainopt emitter			*/
  RL_RVI1,	/* processed by RVI emitter (phase 1)		*/
  RL_RVI2,	/* processed by RVI emitter (phase 2)		*/
  RL_CG,        /* processed to the beginning of CG		*/
  RL_CGSCHED,   /* processed past CG scheduling			*/
  RL_LAST	/* > last legal value				*/
} REGION_LEVEL;

struct region_flags_struct {
  mUINT16 level : 5;	       /* how far the region has been processed*/
  mUINT16 gra_flags : 4;
  mUINT16 return_flag : 1;     /* region contains a return	       */
  mUINT16 glue_code_flag : 1;  /* glue code region created by cg       */
  mUINT16 contains_black : 1;  /* TRUE if PU can contain a black region*/
  mUINT16 contains_bounds : 1; /* TRUE if PU has regions w/ bounds     */
  mUINT16 bounds_exist : 1;    /* region boundaries have been created  */
  mUINT16 aliased_to_globals : 1; /* bound sets aliased to globals     */
  mUINT16 aliased_to_indirects : 1; /* bound sets aliased to indirects */
  mUINT16 contains_uplevel : 1; /* region contains uplevel references  */
  mUINT16 contains_barrier : 1; /* region contains barrier w/ no STs   */
  mUINT16 unused : 3;
};

typedef enum {
  RID_FLAGS_has_reg_alloc = 0x1, /* register allocation exists		*/
  RID_FLAGS_was_gra	  = 0x2  /* GRA did it (else must be SWP)	*/
} REGION_FLAGS;

/* Here's how these types work:

   loop: a region inserted around a loop by RAIL
   pragma: a region inserted by user pragmas
   olimit: a region inserted by the olimit heuristic
   func_entry: always refers to the enclosing func_entry

   The BE driver iterator can be taught to look for loop/func_entries
   or normal/func_entries. Func_entry is always recognized. No type can
   be zero - it messes up the logic.
*/
typedef enum {
  RID_TYPE_undefined  = 0x00,  /* initialized value			     */
  RID_TYPE_func_entry = 0x01,  /* root RID for PU associated w/func_entry    */
  RID_TYPE_loop	      = 0x02,  /* it's a loop (RAIL)			     */
  RID_TYPE_pragma     = 0x04,  /* it's an arbitrary region 		     */
  RID_TYPE_olimit     = 0x08,  /* it's an arbitrary region 		     */
  RID_TYPE_mp	      = 0x10,  /* it's a MP region (transparent)	     */
  RID_TYPE_rpi	      = 0x20,  /* it's a RPI region			     */
  RID_TYPE_cold       = 0x40,  /* it's a cold region (transparent)	     */
  RID_TYPE_swp	      = 0x80,  /* it's a SWP loop (transparent)		     */
#ifdef TARG_SL2 //fork_joint
  RID_TYPE_major  = 0x100, /* region type for major region */
  RID_TYPE_minor = 0x200,  /* region type for minor region*/
#endif 
  RID_TYPE_eh	        = 0x3f000, /* EH region mask (all EH are transparent)*/
  RID_TYPE_try	        = 0x01000, /* it's a try-block			     */
  RID_TYPE_cleanup      = 0x02000, /* it's a cleanup region		     */
  RID_TYPE_exc_spec     = 0x04000, /* it's an exception specification region */
  RID_TYPE_mask	        = 0x08000, /* it's an EH mask region		     */
  RID_TYPE_guard        = 0x10000, /* it's an EH guard region                */
  RID_TYPE_null_cleanup = 0x20000  /* it's a cleanup region to be deleted    */
} RID_TYPE;

/* debug flags for regions */
typedef enum {
  TT_REGION_USER_DEBUG =	0x0001,
  TT_REGION_LARGE_PU_DEBUG =	0x0002,
  TT_REGION_LARGE_PU_OFF =	0x0004,
  TT_REGION_RGN_INIT_DEBUG =    0x0008,
  TT_REGION_LNO_DEBUG =		0x0010,
  TT_REGION_RAIL_DEBUG =	0x0020,
  TT_REGION_RBI_DEBUG =		0x0040,
  TT_REGION_WOPT_DEBUG =	0x0080,
  TT_REGION_CG_DEBUG =		0x0100,
  TT_REGION_BOUND_DEBUG =       0x0200,
  TT_REGION_ALL =		0xffffffff
} REGION_DEBUG_FLAGS;

/* Flag to tell us whether the boundary info is just NULL or really
   doesn't exist. This is used by Preopt to tell if it should be
   conservative or not in it's analysis. */
typedef enum {
  REGION_BOUND_UNKNOWN = FALSE,
  REGION_BOUND_EXISTS = TRUE
} REGION_BOUND_EXIST;

/* Flag to tell us whether region has any return statements in it.
   This is useful once the region is black for building CFGs - you
   need to know if this region can exit the PU. */
typedef enum {
  REGION_NO_RETURN = FALSE,
  REGION_RETURN = TRUE
} REGION_RETURN_ENUM;

typedef union region_flags_union {
  UINT32 flags;
  struct region_flags_struct rfs;
} URFLAG;

/* RID_Create should assign a new id, this has to be an invalid RID id */
#define RID_CREATE_NEW_ID	-1

#define URFLAG_flags(r)          ((r).flags)
#define URFLAG_clear(r)         ( (r).flags = 0 )
#define URFLAG_rfs(r)            ((r).rfs)
#define URFLAG_level(r)          ((r).rfs.level)
#define URFLAG_type(r)           ((r).rfs.type)
#define URFLAG_gra_flags(r)      ((r).rfs.gra_flags)
#define URFLAG_return_flag(r)	 ((r).rfs.return_flag)
#define URFLAG_glue_code_flag(r) ((r).rfs.glue_code_flag)
#define URFLAG_contains_black(r) ((r).rfs.contains_black)
#define URFLAG_contains_bounds(r) ((r).rfs.contains_bounds)
#define URFLAG_bounds_exist(r)   ((r).rfs.bounds_exist)
#define URFLAG_aliased_to_globals(r) ((r).rfs.aliased_to_globals)
#define URFLAG_aliased_to_indirects(r) ((r).rfs.aliased_to_indirects)
#define URFLAG_contains_uplevel(r) ((r).rfs.contains_uplevel)
#define URFLAG_contains_barrier(r) ((r).rfs.contains_barrier)

typedef struct points_to_ref {
  POINTS_TO            *Pt;
  struct points_to_ref *Next;
} POINTS_TO_SET;

struct EH_RANGE;

/* RID */
typedef struct region_id {
  INT id;		/* unique identifier, first in struct so that	*/
			/* refs don't require offsets			*/
  RID_TYPE rid_type;	/* type, 1:1 mapping to WHIRL region kind	*/
  INT depth;		/* region nesting depth				*/
  			/* right now this is used for the loop level    */
  SRCPOS srcpos;	/* linenum for start of the original SCF, 	*/
			/* may be NULL					*/
  UINT32 flags;		/* includes WHIRL level				*/
  struct cgrin *cginfo; /* points to info for CG			*/
  INT32 num_exits;	/* number of exits from the region; 		*/
  			/* this is WN_num_entries			*/
  /* boundary sets 							*/
  PREG_LIST *pregs_in;
  PREG_LIST **pregs_out;/* those pregs which are live and defreach	*/
			/* at each exit. If num_exits > 1, these lists  */
			/* are in 1-1 correspondence with the exits in  */
			/* kid1 of the rwn, i.e. the first list		*/
			/* corresponds to the first goto target, etc.	*/
  PREG_LIST *pregs_quad; /* tells which pregs are quads (FQ)		*/
  PREG_LIST *pregs_complex_quad; /* tells which pregs are complex quads	(CQ) */
  POINTS_TO_SET	*used_in;
  POINTS_TO_SET *def_in_live_out;

  char *options;	/* options to compile this region with		*/

  WN *rwn;		/* The WHIRL node.  This field should be	*/
			/* cleared if the WN is deleted			*/

  WN *parent_block;	/* The WHIRL parent block that contains this    */
  			/* region, used by the driver to replace a 	*/
			/* region once it has been processed.		*/

  struct region_id *parent;	/* Parent, first_kid, and next		*/
  struct region_id *first_kid;	/* allow traversal of the tree of	*/
  struct region_id *next;	/* RIDs					*/

  LOWER_ACTIONS	lowered; /* lowerer actions already applied to region	*/
  struct EH_RANGE  *eh_range_ptr; /* pointer to current EH range   	*/
  INT32 num_eh_ranges;   /* how many eh_ranges in this eh region        */

} RID;

/* RID access macros */
#define RID_id(r)		((r)->id)
#define RID_depth(r)         	((r)->depth)
#define RID_srcpos(r)        	((r)->srcpos)
#define RID_flags(r)         	((r)->flags)
#define RID_cginfo(r)        	((r)->cginfo)
#define RID_num_exits(r)     	((r)->num_exits)
#define RID_options(r)		((r)->options)
#define RID_rwn(r)           	((r)->rwn)
#define RID_parent(r)        	((r)->parent)
#define RID_parent_block(r)     ((r)->parent_block)
#define RID_first_kid(r)     	((r)->first_kid)
#define RID_next(r)          	((r)->next)
#define RID_rloop(r)         	((r)->rloop)
#define RID_lowered(r)       	((r)->lowered)
#define RID_eh_range_ptr(r) 	((r)->eh_range_ptr)
#define RID_num_eh_ranges(r)    ((r)->num_eh_ranges)
#define RID_type(r)          	((r)->rid_type)

/* flag macros */
#define RID_level(r)         (URFLAG_level(*((URFLAG *)(&RID_flags(r)))))
#define RID_gra_flags(r)     (URFLAG_gra_flags(*((URFLAG *)(&RID_flags(r)))))
#define RID_has_return(r)    (URFLAG_return_flag(*((URFLAG *)(&RID_flags(r)))))
#define RID_is_glue_code(r)  (URFLAG_glue_code_flag(*((URFLAG *)(&RID_flags(r)))))
#define RID_contains_black(r)  (URFLAG_contains_black(*((URFLAG *)(&RID_flags(r)))))
#define RID_contains_bounds(r)  (URFLAG_contains_bounds(*((URFLAG *)(&RID_flags(r)))))
#define RID_bounds_exist(r)  (URFLAG_bounds_exist(*((URFLAG *)(&RID_flags(r)))))
#define RID_aliased_to_globals(r)  (URFLAG_aliased_to_globals(*((URFLAG *)(&RID_flags(r)))))
#define RID_aliased_to_indirects(r)  (URFLAG_aliased_to_indirects(*((URFLAG *)(&RID_flags(r)))))
#define RID_contains_uplevel(r)  (URFLAG_contains_uplevel(*((URFLAG *)(&RID_flags(r)))))
#define RID_contains_barrier(r)  (URFLAG_contains_barrier(*((URFLAG *)(&RID_flags(r)))))

/* boundary set macros */
#define RID_pregs_in(r)      ((r)->pregs_in)
#define RID_pregs_quad(r)    ((r)->pregs_quad)
#define RID_pregs_complex_quad(r)    ((r)->pregs_complex_quad)
#define RID_pregs_out(r)     ((r)->pregs_out)
#define RID_pregs_out_i(r,i) (((r)->pregs_out)?(((r)->pregs_out)[(i)]):((PREG_LIST *)NULL))
#define RID_pregs_set_out_i(r,i) (((r)->pregs_out)[(i)])
#define RID_used_in(r)	     ((r)->used_in)
#define RID_def_in_live_out(r)	     ((r)->def_in_live_out)

/* CG macros */
#define RID_has_reg_alloc(r)        (RID_gra_flags(r) & RID_FLAGS_has_reg_alloc)
#define RID_has_reg_alloc_Set(r)    (RID_gra_flags(r) |= RID_FLAGS_has_reg_alloc) 
#define RID_has_reg_alloc_Reset(r)  (RID_gra_flags(r) &= ~RID_FLAGS_has_reg_alloc) 
#define RID_was_gra(r)              (RID_gra_flags(r) & RID_FLAGS_was_gra)
#define RID_was_gra_Set(r)          (RID_gra_flags(r) |= RID_FLAGS_was_gra) 
#define RID_was_gra__Reset(r)       (RID_gra_flags(r) &= ~RID_FLAGS_was_gra) 

/* RID type macros */
#define RID_TYPE_func_entry(r)       (RID_type(r) & RID_TYPE_func_entry)
#define RID_TYPE_func_entry_Set(r)   (RID_type(r) = \
				     (RID_TYPE)(RID_type(r) | RID_TYPE_func_entry)) 
#define RID_TYPE_func_entry_Reset(r) (RID_type(r) = \
				     (RID_TYPE)(RID_type(r) & ~RID_TYPE_func_entry))

#define RID_TYPE_loop(r)             (RID_type(r) & RID_TYPE_loop)
#define RID_TYPE_loop_Set(r)	     (RID_type(r) = \
				     (RID_TYPE)(RID_type(r) | RID_TYPE_loop))
#define RID_TYPE_loop_Reset(r)       (RID_type(r) = \
				     (RID_TYPE)(RID_type(r) & ~RID_TYPE_loop))

#define RID_TYPE_pragma(r)           (RID_type(r) & RID_TYPE_pragma)
#define RID_TYPE_pragma_Set(r)       (RID_type(r) = \
				     (RID_TYPE)(RID_type(r) | RID_TYPE_pragma))
#define RID_TYPE_pragma_Reset(r)     (RID_type(r) = \
				     (RID_TYPE)(RID_type(r) & ~RID_TYPE_pragma))

#define RID_TYPE_olimit(r)           (RID_type(r) & RID_TYPE_olimit)
#define RID_TYPE_olimit_Set(r)       (RID_type(r) = \
				     (RID_TYPE)(RID_type(r) | RID_TYPE_olimit))
#define RID_TYPE_olimit_Reset(r)     (RID_type(r) = \
				     (RID_TYPE)(RID_type(r) & ~RID_TYPE_olimit))

#define RID_TYPE_mp(r)               (RID_type(r) & RID_TYPE_mp)
#define RID_TYPE_mp_Set(r)           (RID_type(r) = \
				     (RID_TYPE)(RID_type(r) | RID_TYPE_mp))
#define RID_TYPE_mp_Reset(r)         (RID_type(r) = \
				     (RID_TYPE)(RID_type(r) & ~RID_TYPE_mp))

#define RID_TYPE_rpi(r)              (RID_type(r) & RID_TYPE_rpi)
#define RID_TYPE_rpi_Set(r)          (RID_type(r) = \
				     (RID_TYPE)(RID_type(r) | RID_TYPE_rpi))
#define RID_TYPE_rpi_Reset(r)        (RID_type(r) = \
				     (RID_TYPE)(RID_type(r) & ~RID_TYPE_rpi))

#define RID_TYPE_cold(r)             (RID_type(r) & RID_TYPE_cold)
#define RID_TYPE_cold_Set(r)         (RID_type(r) = \
				     (RID_TYPE)(RID_type(r) | RID_TYPE_cold))
#define RID_TYPE_cold_Reset(r)       (RID_type(r) = \
				     (RID_TYPE)(RID_type(r) & ~RID_TYPE_cold))

#define RID_TYPE_swp(r)              (RID_type(r) & RID_TYPE_swp)
#define RID_TYPE_swp_Set(r)          (RID_type(r) = \
				     (RID_TYPE)(RID_type(r) | RID_TYPE_swp))
#define RID_TYPE_swp_Reset(r)        (RID_type(r) = \
				     (RID_TYPE)(RID_type(r) & ~RID_TYPE_swp))

#ifdef TARG_SL2 //fork_joint
#define RID_TYPE_major(r)     (RID_type(r) & RID_TYPE_major)
#define RID_TYPE_major_Set(r)         (RID_type(r) = \
				     (RID_TYPE)(RID_type(r) | RID_TYPE_major))
#define RID_TYPE_major_Reset(r)       (RID_type(r) = \
				     (RID_TYPE)(RID_type(r) & ~RID_TYPE_major))
#define RID_TYPE_minor(r)     (RID_type(r) & RID_TYPE_minor)
#define RID_TYPE_minor_Set(r)         (RID_type(r) = \
				     (RID_TYPE)(RID_type(r) | RID_TYPE_minor))
#define RID_TYPE_minor_Reset(r)       (RID_type(r) = \
				     (RID_TYPE)(RID_type(r) & ~RID_TYPE_minor))
#endif 
				     

#define RID_TYPE_eh(r)               (RID_type(r) & RID_TYPE_eh)
/* makes no sense to have set for EH, it is a mask */

#define RID_TYPE_try(r)              (RID_type(r) & RID_TYPE_try)
#define RID_TYPE_try_Set(r)          (RID_type(r) = \
				     (RID_TYPE)(RID_type(r) | RID_TYPE_try))
#define RID_TYPE_try_Reset(r)        (RID_type(r) = \
				     (RID_TYPE)(RID_type(r) & ~RID_TYPE_try))

#define RID_TYPE_cleanup(r)          (RID_type(r) & RID_TYPE_cleanup)
#define RID_TYPE_cleanup_Set(r)      (RID_type(r) = \
				     (RID_TYPE)(RID_type(r) | RID_TYPE_cleanup))
#define RID_TYPE_cleanup_Reset(r)    (RID_type(r) = \
				     (RID_TYPE)(RID_type(r) & ~RID_TYPE_cleanup))

#define RID_TYPE_exc_spec(r)         (RID_type(r) & RID_TYPE_exc_spec)
#define RID_TYPE_exc_spec_Set(r)     (RID_type(r) = \
				     (RID_TYPE)(RID_type(r) | RID_TYPE_exc_spec))
#define RID_TYPE_exc_spec_Reset(r)   (RID_type(r) = \
				     (RID_TYPE)(RID_type(r) & ~RID_TYPE_exc_spec))

#define RID_TYPE_mask(r)             (RID_type(r) & RID_TYPE_mask)
#define RID_TYPE_mask_Set(r)         (RID_type(r) = \
				     (RID_TYPE)(RID_type(r) | RID_TYPE_mask))
#define RID_TYPE_mask_Reset(r)       (RID_type(r) = \
				     (RID_TYPE)(RID_type(r) & ~RID_TYPE_mask))

#define RID_TYPE_guard(r)             (RID_type(r) & RID_TYPE_guard)
#define RID_TYPE_guard_Set(r)         (RID_type(r) = \
				     (RID_TYPE)(RID_type(r) | RID_TYPE_guard))
#define RID_TYPE_guard_Reset(r)       (RID_type(r) = \
				     (RID_TYPE)(RID_type(r) & ~RID_TYPE_guard))

#define RID_TYPE_null_cleanup(r)             (RID_type(r) & RID_TYPE_null_cleanup)
#define RID_TYPE_null_cleanup_Set(r)         (RID_type(r) = \
				     (RID_TYPE)(RID_type(r) | RID_TYPE_null_cleanup))
#define RID_TYPE_null_cleanup_Reset(r)       (RID_type(r) = \
				     (RID_TYPE)(RID_type(r) & ~RID_TYPE_guard))

/* transparent regions (no boundary): PU, EH, MP, SWP, cold */
#define RID_TYPE_transparent(r) (   RID_TYPE_func_entry(r) \
				 || RID_TYPE_mp(r) \
				 || RID_TYPE_eh(r) \
				 || RID_TYPE_swp(r) \
				 || RID_TYPE_cold(r))

#if defined(TARG_SL2)
#define RID_TYPE_sl2_para(r)   ( RID_TYPE_major(r) \
	                     || RID_TYPE_minor(r))
#endif
/* RID tree iterator */
/* the type tells what loop we are doing:
   loop/func_entry: regions introduced by RAIL, loop around MainOpt/CG
   pragma/olimit/func_entry: regions introduced by user pragmas or Olimit
   			heuristic, loop around LNO/MainOpt/CG
 */
#define REGION_STACK_SIZE 10
typedef struct region_cs_iter {
  RID    *me;
  RID    *kid;
  RID_TYPE type;	/* type of region to loop through with iterator	   */
  WN *parent_block;	/* block enclosing the region */
  WN *region_marker[REGION_STACK_SIZE]; /* mark where code was */
  INT32 region_marker_sp;
  BOOL   is_pu;		/* region is an actual PU */
  BOOL	 is_not_stacked;/* region is not stacked (PU or same as outer loop) */
} REGION_CS_ITER;

#define REGION_CS_ITER_me(i)        		((i)->me)
#define REGION_CS_ITER_kid(i)       		((i)->kid)
#define REGION_CS_ITER_type(i)      		((i)->type)
#define REGION_CS_ITER_parent_block(i) 		((i)->parent_block)
#define REGION_CS_ITER_sp(i)	    		((i)->region_marker_sp)
#define REGION_CS_ITER_marker(i,j)  		((i)->region_marker[j])
#define REGION_CS_ITER_is_pu(i)	    		((i)->is_pu)
#define REGION_CS_ITER_is_not_stacked(i)    	((i)->is_not_stacked)
#define REGION_CS_ITER_wn(i)        		(RID_rwn((i)->kid))

/*--------------------------------------------------------------------------*/
/* These variables are live across entire back end	*/
extern WN_MAP RID_map;
extern MEM_POOL REGION_mem_pool;

/* latest region id, defined in common/com/wn.c */
extern INT32 New_Region_Id(void);
extern INT32 Last_Region_Id(void);

/* Replace the old WN tree with a new one and update the RID pointers */
extern void REGION_new_wn(WN *, WN *);

/* Make sure RID and WN map are consistent */
extern BOOL REGION_consistency_check(WN *);

/* Update alias information for glue code */
extern void REGION_update_alias_info(WN *, struct ALIAS_MANAGER *);

/* Get the RID from the WN map */
extern RID *REGION_get_rid(const WN *);

/* Given a WN *, find the RID tree root WN which is the PU */
extern WN *REGION_find_pu(WN *);

/* Given a RID that will be processed to black, mark the root RID */
extern BOOL REGION_has_black_regions(RID *);

/* Set the region_level for this RID and all its children */
extern void REGION_set_level(RID *, REGION_LEVEL);

/* count the exits in the exit block of a region */
extern BOOL REGION_count_exits(WN *);

/* recount exits, fix up any boundary sets that have changed */
extern void REGION_fix_up_exits(RID *, WN *);

/* look at all exit block between two rids and find a given label number */
extern BOOL REGION_scan_exits(WN *, INT32);

/* Allocate a RID from the REGION_mem_pool and fill in the fields with
   the id (or RID_CREATE_NEW_ID) and appropriate map values from the wn.
   If wn is NULL, fill in reasonable default values. */
extern RID *RID_Create(INT, INT, WN *);

/* Given two RIDs make the first the kid of the second */
extern void RID_Add_kid(RID *, RID *);

/* Remove a RID from the RID tree */
extern void RID_unlink(RID *);

/* Replace old_rid with new_rid */
extern void RID_replace(RID *old_rid, RID *new_rid);

/* delete a RID from the tree */
extern void RID_Delete(WN_MAP_TAB *, WN *);
extern void RID_Delete2(RID *);

/* clone a RID and connect into tree */
extern void REGION_clone(WN *, WN *, WN *);

/* reattach a RID and WN, update the level and last preg */
extern void REGION_emit(RID *, WN *, INT32, INT32, INT64);

/* copy in and out sets from one rid to another */
extern void RID_copy_sets(RID *, RID *);

/* find the RID level for Preopt (it's called so many places) */
extern REGION_LEVEL RID_preopt_level(INT);

/* search a boundary set for a given PREG */
extern BOOL REGION_search_preg_set(PREG_LIST *, PREG_NUM);

/* create a POINTS_TO for the wn given and add it to the list given */
extern void REGION_add_wn_points_to(POINTS_TO_SET **, WN *,
				 struct ALIAS_MANAGER *);

/* add a POINTS_TO to the list given, similar to REGION_add_wn_points_to */
extern void REGION_add_points_to(POINTS_TO_SET **, POINTS_TO *,
				 struct ALIAS_MANAGER *);

/* allocate from a specific pool and copy a points_to into it */
extern POINTS_TO *Points_to_copy(POINTS_TO *, MEM_POOL *);

extern void Get_symbol_info_for_cvt_io(POINTS_TO *, WN *);
			/* in opt_sym.cxx */
extern POINTS_TO *Points_to(struct ALIAS_MANAGER *, WN *);
			/* in opt_alias_mgr.cxx */

/* add a PREG to either the pregs_in or pregs_out lists (pregs_quad also) */
extern BOOL REGION_add_preg_in(RID *rid, PREG_NUM pr, TYPE_ID quad);
extern BOOL REGION_add_preg_out(RID *rid, INT32 which_set, PREG_NUM pr,
				TYPE_ID quad);

/* remove a PREG from either the pregs_in or ALL the pregs_out sets */
/* does quad also */
extern BOOL REGION_remove_preg(RID *rid, PREG_NUM pr, BOOL outset);

/* add a label after the region and the OPC_REGION_EXIT at the end */
extern WN *REGION_add_exit(WN *, WN *, WN *);

/* delete a region exit from the exit block and decrement RID_num_exits */
extern void REGION_delete_exit(RID *, INT32, WN *, BOOL);

/* Find the options pragma for the PU or region */
extern char *REGION_get_options_string(WN *);

/* tell if a region is an EH region based on WHIRL kind */
extern BOOL REGION_is_EH(WN *);

/* tell if a region is an MP region based on WHIRL pragmas */
extern BOOL REGION_is_mp(WN *);

#if defined(TARG_SL2)
/* tell if a region is an SL2 region based on WHIRL kind */
extern BOOL REGION_is_sl2_para(WN*); 
#endif

/* tell if a region is an EH region based on WHIRL kind */
extern BOOL REGION_is_EH(WN *);

/* tell if a call is a fake call (inside the pragma part of an EH region) */
extern BOOL WN_Fake_Call_EH_Region(WN *,WN_MAP);


/* convert from RID_TYPE_* to REGION_KIND_* (common/com/wn_core.h) */
extern REGION_KIND REGION_type_to_kind(RID *);

/* convert from REGION_KIND_* to RID_TYPE_* */
extern void REGION_kind_to_type(WN *, RID *);

/* set REGION_RETURN bit on current RID and propagate up to parent */
extern void REGION_propagate_return(RID *);

/* return the text for the level this RID is compiled to */
extern char *RID_level_str(RID *rid);

/* return text for RID type */
extern char *RID_type_str(RID_TYPE type);

/* Print: pregs_in, pregs_out, pregs_quad,	*/
/* used_in, def_in_live_out */
extern void RID_set_print(FILE *, RID *);
/* this is defined in opt_alias_analysis.cxx and exported from wopt.so */
extern void Print_points_to(FILE *, POINTS_TO *);

/* Print the given region_id to file */
extern void RID_Fprint(FILE *, RID *);

/* Print the tree of RIDs rooted at rid to TFile */
extern void RID_Tree_Print(FILE *, RID *);

extern bool RID_is_valid(RID *, RID*);

/* Print the tree of RIDs rooted at the RID of the given WN to TFile */
extern void RID_WN_Tree_Print(FILE *, WN *);

#ifdef __cplusplus
}
#endif

#endif /* region_util_INCLUDED */
