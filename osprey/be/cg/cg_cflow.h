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




#ifndef cg_cflow_INCLUDED
#define cg_cflow_INCLUDED

#ifdef CG_CFLOW
INT DB_count;		/* The number of BBs in the DoBody */
INT Original_DB_count;	/* The number of BBs in the original DoBody */
#else
extern INT DB_count;	/* The number of BBs in the DoBody */
extern INT Original_DB_count;	/* The number of BBs in original DoBody */
#define BBinloop_count	DB_count	/* for old code... */
#endif 

extern BOOL CG_DoBody_Cflow_Done;

#include "bitset.h"

#define DBbit(x,y)	BS_MemberP(x,y)
#define DBset(x,y)	(x=BS_Union1D(x,y,&MEM_phase_pool))
#define DBreset(x,y)	(x=BS_Difference1D(x,y))
#define DBclear(x)	(x=BS_ClearD(x))


/* The following data structure describes the basic block control flow
 * structure within the DoBody:
 */

typedef struct DOBODY_CFLOW {
    INT		flags;			/* Miscellaneous boolean flags */
    INT		next;			/* Next DOBODYelt in DoBody */
    INT		prev;			/* Previous DOBODYelt in DoBody */
    INT         kind;                   /* save BB_kind() flag */
    BB		*bb;		        /* Original BB */
    TN		*assigned_ICR;		/* ICR assigned to this BB */
    TN          *prolog_ICR;            /* renamed prolog conditional */
    TN		*BRCC;			/* ICR for branch condition, only set
					 * for BB_LOGIF BBs */
    TN          *original_ICR;
    TN          *original_BRCC;
    BOOL	BRCCsense;		/* if BRCC is true, first succ.
					 * is taken */
    TN		*ICICR[2];		/* if conversion condition TNs
					 * assoc. with successors. */
    BOOL	ICICRinv[2];		/* set if assoc. ICICR is
					 * "true low". */
    OP		*CFCCOP;		/* FCC to ICC conversion op */
    OPS         orig_ops;               /* original ops */
    mINT16	predecessor_cnt;	/* Number of predecessor BBs */
    mINT16	successor_cnt;		/* Number of successor BBs */
    INT16	*preds;	                /* Predecessor BBs */
    INT16	*succs;	                /* Successor BBs */
    float	*succ_probs;		/* Successor probabilities */
    BS 	        *ancestors;		/* Ancestors of this BB */
    BS  	*descendents;		/* Descendents of this BB */
    BS  	*dominators;		/* Dominators of this BB */
    BS 	        *collectors;		/* Collectors of this BB */
    mINT16	assigned_ICR_omega;	/* omega of use of assigned_ICR */
    mUINT8	imm_dominator;		/* Immediate dominator */
    mUINT8	imm_collector;		/* Immediate collector */
    float       freq;                   /* BB frequency */
    OP		*entry_sp_adj;		/* BB_entry_sp_adj */
    OP		*exit_sp_adj;		/* BB_exit_sp_adj */
} DOBODYelt;

#ifdef CG_CFLOW
DOBODYelt		*DOBODYelts;
#else
extern DOBODYelt	*DOBODYelts;
#endif

#define DoBody(x)	(DOBODYelts+(x))				
#define DoBodyFlags(x)	(DoBody(x)->flags)
#define DoBodyNext(x)	(DoBody(x)->next)
#define DoBodyPrev(x)	(DoBody(x)->prev)
#define DoBodyKind(x)	(DoBody(x)->kind)
#define DoBodyBB(x)	(DoBody(x)->bb)
#define DoBodyICR(x)	(DoBody(x)->assigned_ICR)
#define DoBodyPrologICR(x) (DoBody(x)->prolog_ICR)
#define DoBodyICRomega(x) (DoBody(x)->assigned_ICR_omega)
#define DoBodyBRCC(x)	(DoBody(x)->BRCC)
#define DoBodyOriginalICR(x)	(DoBody(x)->original_ICR)
#define DoBodyOriginalBRCC(x)	(DoBody(x)->original_BRCC)
#define DoBodyBRCCsense(x) (DoBody(x)->BRCCsense)
#define DoBodyICICR(x,i) (DoBody(x)->ICICR[i])
#define DoBodyICICRinv(x,i) (DoBody(x)->ICICRinv[i])
#define DoBodyCFCCOP(x)	(DoBody(x)->CFCCOP)
#define DoBodyOrigOPS(x) (DoBody(x)->orig_ops)
#define DoBodyPreds(x)	(DoBody(x)->predecessor_cnt)
#define DoBodySuccs(x)	(DoBody(x)->successor_cnt)
#define DoBodyPred(x,i)	(DoBody(x)->preds[i])
#define DoBodySucc(x,i)	(DoBody(x)->succs[i])
#define DoBodySuccProb(x,i) (DoBody(x)->succ_probs[i])
#define DoBodyAncs(x)	(DoBody(x)->ancestors)
#define DoBodyDescs(x)	(DoBody(x)->descendents)
#define DoBodyDoms(x)	(DoBody(x)->dominators)
#define DoBodyColls(x)	(DoBody(x)->collectors)
#define DoBodyIDom(x)	(DoBody(x)->imm_dominator)
#define DoBodyIColl(x)	(DoBody(x)->imm_collector)

#define DoBodyPredP(x)	(DoBody(x)->preds)
#define DoBodySuccP(x)	(DoBody(x)->succs)
#define DoBodySuccProbP(x) (DoBody(x)->succ_probs)

#define DoBodyFreq(x)   (DoBody(x)->freq)
#define DoBodyEntrySpAdj(x)	(DoBody(x)->entry_sp_adj)
#define DoBodyExitSpAdj(x)	(DoBody(x)->exit_sp_adj)

/* Query individual ancestor/descendent or dominator/collector bits: */
#define DoBodyAnc(i,j)	( DBbit (DoBodyAncs(i), j) )
#define DoBodyDesc(i,j)	( DBbit (DoBodyDescs(i), j) )
#define DoBodyDom(i,j)	( DBbit (DoBodyDoms(i), j) )
#define DoBodyColl(i,j)	( DBbit (DoBodyColls(i), j) )

/* Flag to suppress insertion of Pac-Man stufficr(%r0): */
#define DB_NOSTUFF	0x1
#define DBnostuff(x)	(DoBodyFlags(x) & DB_NOSTUFF)
#define SetDBnostuff(x)	(DoBodyFlags(x) |= DB_NOSTUFF)

/* Flag to indicate that a BB is "executed" when DoBodyICR(dbnum)
 * is negated
 */
#define DB_NEGATEICR	0x2
#define DBnegateicr(x)	(DoBodyFlags(x) & DB_NEGATEICR)
#define SetDBnegateicr(x) (DoBodyFlags(x) |= DB_NEGATEICR)

/* If BRCC is true, then succ. 0 is executed.
 */
#define DB_SUCC0ONICR	0x4
#define DBsucc0onicr(x)	(DoBodyFlags(x) & DB_SUCC0ONICR)
#define SetDBsucc0onicr(x) (DoBodyFlags(x) |= DB_SUCC0ONICR)

/* BRCC is a Floating CC.
 */
#define DB_BRCCISF	0x8
#define DBbrccisf(x)	(DoBodyFlags(x) & DB_BRCCISF)
#define SetDBbrccisf(x)	(DoBodyFlags(x) |= DB_BRCCISF)

/* Flag for local use in walking the DoBody block list
 */
#define DB_FLAG1	0x10
#define DBflag1(x)	(DoBodyFlags(x) & DB_FLAG1)
#define SetDBflag1(x)	(DoBodyFlags(x) |= DB_FLAG1)
#define ResetDBflag1(x)	(DoBodyFlags(x) &= ~DB_FLAG1)

/* BB is always executed
 */
#define DB_ALWAYS	0x20
#define DBalways(x)	(DoBodyFlags(x) & DB_ALWAYS)
#define SetDBalways(x) (DoBodyFlags(x) |= DB_ALWAYS)

/* BRCC is unnormalized.
 */
#define DB_BRCCUNN	0x40
#define DBbrccunn(x)	(DoBodyFlags(x) & DB_BRCCUNN)
#define SetDBbrccunn(x)	(DoBodyFlags(x) |= DB_BRCCUNN)

/* if DB_BRCCSUCC is set, the target BB is DoBodySucc(dbnum,1) */
#define DB_BRCCSUCC	0x80
#define DBbrccsucc(x)	(DoBodyFlags(x) & DB_BRCCSUCC)
#define SetDBbrccsucc(x)	(DoBodyFlags(x) |= DB_BRCCSUCC)

/* if DB_HASFREQ is set, the BB has freq asscociated with it */
#define DB_HASFREQ	0x100
#define DBhasfreq(x)	(DoBodyFlags(x) & DB_HASFREQ)
#define SetDBhasfreq(x)	(DoBodyFlags(x) |= DB_HASFREQ)
#define ResetDBhasfreq(x)	(DoBodyFlags(x) &= ~DB_HASFREQ)

/* if DB_HASFBFREQ is set, the BB has feedback freq asscociated with it */
#define DB_HASFBFREQ	0x200
#define DBhasfbfreq(x)	(DoBodyFlags(x) & DB_HASFBFREQ)
#define SetDBhasfbfreq(x)	(DoBodyFlags(x) |= DB_HASFBFREQ)
#define ResetDBhasfbfreq(x)	(DoBodyFlags(x) &= ~DB_HASFBFREQ)


#endif /* cg_cflow_INCLUDED */

