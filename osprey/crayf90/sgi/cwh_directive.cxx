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
 * Module: cwh_directive
 * $Revision: 1.11 $
 * $Date: 05/09/22 10:54:47-07:00 $
 * $Author: gautam@jacinth.keyresearch $
 *
 * Description: contains routines to support directives, converting
 *              from Cray IR to WHIRL. Entry points from
 *              PDGCS layer are
 * 
 *		fei_task_var - called to process various vars in clauses
 *		fei_parallel_region - CMIC$ parallel
 *		fei_endparallel_region - CMIC$ end parallel
 *		fei_doparallel - CMIC$ doparallel
 *		fei_task_endloop - called at the end of a parallel loop
 *		fei_doall - CMIC$ doall
 *		fei_doacross - C$DOACROSS
 *		fei_parallel - C$PAR parallel
 *		fei_endparallel - C$PAR end parallel
 *		fei_pdo - C$par pdo
 *		fei_endpdo - C$PAR end pdo
 * 		fei_paralleldo - C$PAR PARALLEL DO
 * 		fei_singleprocess - C$PAR SINGLE PROCESS
 * 		fei_endsingleprocess - C$PAR END SINGLE PROCESS
 * 		fei_criticalsection - C$PAR CRITICAL SECTION
 * 		fei_endcriticalsection - C$PAR END CRITICAL SECTION
 * 		fei_barrier - C$PAR BARRIER
 * 		fei_section - C$PAR SECTION
 * 		fei_psection - C$PAR PSECTION
 * 		fei_endpsection - C$PAR END PSECTION
 * 		fei_copy_in - C$COPYIN
 *              fei_dynamic
 *              fei_redistribute
 *              fei_interchange
 *              fei_blockable  
 *              fei_fission    
 *              fei_fuse       
 *              fei_assert           
 *              fei_fill_symbol      
 *              fei_align_symbol     
 *              fei_unroll           
 *              fei_page_place       
 *              fei_prefetch_ref_disable 
 *              fei_prefetch_ref         
 *              fei_prefetch             
 *              fei_prefetch_manual      
 *              fei_regionbegin          
 *              fei_regionend            
 *              fei_section_gp           
 *              fei_section_nongp        
 *              fei_blocking_size        
 *              fei_opaque
 *
 * ====================================================================
 * ====================================================================
 */

static const char *source_file = __FILE__;

/* sgi includes */

#include "defs.h"
#include "glob.h"  
#include "stab.h"
#include "strtab.h"
#include "errors.h"
#include "targ_const.h"
#include "config_targ.h"  
#include "const.h"
#include "pu_info.h"
#include "wn.h"
#include "wn_util.h"
#include "f90_utils.h"
#include "targ_sim.h"

/* FE includes */

#include "i_cvrt.h"

/* conversion includes */

#include "cwh_defines.h"
#include "cwh_addr.h"
#include "cwh_expr.h"
#include "cwh_stk.h"
#include "cwh_block.h"
#include "cwh_types.h"
#include "cwh_preg.h"
#include "cwh_stab.h"
#include "cwh_auxst.h"
#include "cwh_stmt.h"
#include "cwh_directive.h"
#include "cwh_directive.i"

static int task_var_count; /* count the number of private, shared, lastlocal */
static int task_nest_count; /* number of indexes in NEST clause */
static int task_lastthread_count; /* number of vars in LASTTHREAD */
static int task_affinity_count; /* number of indexes in AFFINITY */

/*===============================================
 *
 * fei_task_var
 *
 * Generate the pragma for a variable in a parallel directive
 * list (SHARED, PRIVATE, etc...) and push it onto the stack.
 *
 * The stack popped when processing a directive
 *
 *===============================================
 */ 
extern INTPTR
fei_task_var( INTPTR	sym_idx,
	      INT32	context)
{
  STB_pkt *p;
  WN *wn;
#ifdef KEY /* Bug 10177 */
  int op_code = 0;
#else /* KEY Bug 10177 */
  int op_code;
#endif /* KEY Bug 10177 */
  ST *st;
  p = cast_to_STB(sym_idx);
  DevAssert((p->form == is_ST),("Odd object ref"));

  st = (ST *)p->item;
  if (Has_Base_Block(st)) {
    ST * base = ST_base(st);
    if (ST_is_temp_var(base))
      if (ST_sclass(base) == SCLASS_AUTO)
        if (!ST_is_return_var(base))
           /* Do not generate a pragma for an automatic array */
           /* The base var is also on the list, and that is the */
           /* one that is needed. */
           return(sym_idx);
  }

  switch((CONTEXT_TYPE) context) {
    case Context_Omp_Private:
    case Context_Private:
	/* generate pragma for a LOCAL */
	/*  - these pragmas must be attached to the upcoming parallel region */
	/* DLAI - what about the offset? */
	wn = WN_CreatePragma(WN_PRAGMA_LOCAL, (ST *)p->item, 0, /* offset= */0);
        if ((CONTEXT_TYPE) context == Context_Omp_Private) 
          WN_set_pragma_omp(wn);
        cwh_stk_push(wn, WN_item);
        task_var_count++;
	break;
    case Context_Omp_Shared:
    case Context_Shared:
	wn = WN_CreatePragma(WN_PRAGMA_SHARED, (ST *)p->item, 0, /*offset=*/0);
        if ((CONTEXT_TYPE) context == Context_Omp_Shared) 
          WN_set_pragma_omp(wn);
        cwh_stk_push(wn, WN_item);
        task_var_count++;
	break;
    case Context_Omp_Lastprivate:
    case Context_Lastlocal:
	wn = WN_CreatePragma(WN_PRAGMA_LASTLOCAL, (ST *)p->item, 0, /*offset=*/0);
        if ((CONTEXT_TYPE) context == Context_Omp_Lastprivate) 
          WN_set_pragma_omp(wn);
        cwh_stk_push(wn, WN_item);
        task_var_count++;
	break;
    case Context_Omp_Firstprivate:
    case Context_Getfirst:
        wn = WN_CreatePragma(WN_PRAGMA_FIRSTPRIVATE, (ST *)p->item, 0, /*offset=*/0);
        if ((CONTEXT_TYPE) context == Context_Omp_Firstprivate) 
          WN_set_pragma_omp(wn);
        cwh_stk_push(wn, WN_item);
        task_var_count++;
        break;
    case Context_Affinity:
    case Context_Omp_Affinity:
	wn = WN_CreateXpragma( WN_PRAGMA_AFFINITY, (ST_IDX) NULL, 1);
        if ((CONTEXT_TYPE) context == Context_Omp_Affinity)
          WN_set_pragma_omp(wn);
	WN_kid0(wn) = cwh_addr_address_ST( (ST *)p->item, /* offset=*/ 0 );
        cwh_stk_push(wn, WN_item);
        cwh_stk_push(WN_COPY_Tree(wn), WN_item);
        task_affinity_count++;
	break;
    case Context_Nest:
    case Context_Omp_Nest:
	/* for now throw away the nest var, we may want to make it lastlocal
	   later - dlai */
        task_nest_count++;
	break;
    case Context_Lastthread:
	wn = WN_CreatePragma(WN_PRAGMA_LASTTHREAD, (ST *)p->item, 0, /*offset=*/0);
        cwh_stk_push(wn, WN_item);
	task_lastthread_count++;
	break;
    case Context_Omp_Copyin:
        wn = WN_CreatePragma(WN_PRAGMA_COPYIN, (ST *)p->item, 0, /*offset=*/0);
        WN_set_pragma_omp(wn);
        cwh_stk_push(wn, WN_item);
        task_var_count++;
        break;
    /* added by jhs, 02/7/22 */
    case Context_Omp_Copyprivate:
        wn = WN_CreatePragma(WN_PRAGMA_COPYPRIVATE, (ST *)p->item, 0, /*offset=*/0);
     	WN_set_pragma_omp(wn);
        cwh_stk_push(wn, WN_item);
        task_var_count++;
    	 break;
    case Context_Omp_Reduction_Max:
    case Context_Omp_Reduction_Min:
    case Context_Omp_Reduction_Band:
    case Context_Omp_Reduction_Bor:
    case Context_Omp_Reduction_Bneqv:
    case Context_Omp_Reduction_Plus:
    case Context_Omp_Reduction_Mult:
    case Context_Omp_Reduction_Minus:
    case Context_Omp_Reduction_And:
    case Context_Omp_Reduction_Or:
    case Context_Omp_Reduction_Eqv:
    case Context_Omp_Reduction_Neqv:
        switch((CONTEXT_TYPE) context) {
        case Context_Omp_Reduction_Max:
            op_code = OPR_MAX;
            break;
        case Context_Omp_Reduction_Min:
            op_code = OPR_MIN;
            break;
        case Context_Omp_Reduction_Band:
            op_code = OPR_BAND;
            break;
        case Context_Omp_Reduction_Bor:
            op_code = OPR_BIOR;
            break;
        case Context_Omp_Reduction_Bneqv:
            op_code = OPR_BXOR;
            break;
        case Context_Omp_Reduction_Plus:
            op_code = OPR_ADD;
            break;
        case Context_Omp_Reduction_Mult:
            op_code = OPR_MPY;
            break;
        case Context_Omp_Reduction_Minus:
            op_code = OPR_SUB;
            break;
        case Context_Omp_Reduction_And:
            op_code = OPR_LAND;
            break;
        case Context_Omp_Reduction_Or:
            op_code = OPR_LIOR;
            break;
        case Context_Omp_Reduction_Eqv:
            op_code = OPR_EQ;
            break;
        case Context_Omp_Reduction_Neqv:
            op_code = OPR_NE;
            break;
        }
        wn = WN_CreatePragma(WN_PRAGMA_REDUCTION, (ST *)p->item, 0, op_code);
        WN_set_pragma_omp(wn);
        cwh_stk_push(wn, WN_item);
        task_var_count++;
        break;
    default:
	DevAssert((0), ("Unimplemented fei_task_var type"));
	break;
  }
  return sym_idx;
} /* fei_task_var */

/*===============================================
 *
 * cwh_region
 *
 * Uility to generate a REGION with the given ID.
 * enerates a REGION.  The region is attached to
 * the current block, & pushed on the block stack.
 * The region pragmas are made the current block. 
 * The body of the region is returned.
 *
 *===============================================
 */ 
static WN *
cwh_region(REGION_KIND kind)
{
  WN *body,*pragmas,*exits,*region;
 
  /* create region on current block */

  body    = WN_CreateBlock ();
  pragmas = WN_CreateBlock ();
  exits   = WN_CreateBlock ();
  region  = WN_CreateRegion (kind,
			     body,
			     pragmas,
			     exits,
			     -1, 
			     0);
  cwh_block_append(region);

  /* push the region on the block stack and   */
  /* make the region pragmas the current block*/

  cwh_block_push_region(region);
  cwh_block_set_current(pragmas);

  return(body);
} 

/*===============================================
 *
 * cwh_mp_region
 *
 * This generates a REGION and marks it as a mp region.  These are
 * needed for each parallel loop or region.  Also attaches all
 * collected pragmas and task vars to the region pragma list.
 *
 * wn_pragma_id - the pragma of the region (DOACROSS, PARALLEL, etc...)
 * threadcount,datacount - count of exprs on stack for affinity clause
 * ontocount, reductioncount, chunkcount - count of exprs on stack
 * 
 * returns the region body block (which the caller should set as the
 * current block after all the pragmas are loaded into the region pragma
 * block
 *
 *===============================================
 */ 
extern WN *
cwh_mp_region(       WN_PRAGMA_ID wn_pragma_id,
                     int threadcount,
                     int datacount,
		     int ontocount,
		     int reductioncount,
                     int chunkcount,
		     int is_omp)
{

  WN *body;
  WN *wn, *wn1;
  WN *affinity_block=NULL;
 
  /* create region */

  body = cwh_region(REGION_KIND_MP);

  /* now attach all applicable pragmas */
  cwh_stmt_add_pragma(wn_pragma_id, is_omp,(ST_IDX) NULL, nested_do_descriptor.current,
 		nested_do_descriptor.depth);

  /* attach the chunk,reduction,onto,affinity exprs */

  if (chunkcount) {
    wn = cwh_expr_operand(NULL);
    cwh_stmt_add_xpragma(WN_PRAGMA_CHUNKSIZE,is_omp,wn);
  }

  while (reductioncount) {     /* pop the expressions */

    wn1 = cwh_expr_address(f_NONE);

    if (WNOPR(wn1)==OPR_LDA) {
      cwh_stmt_add_pragma(WN_PRAGMA_REDUCTION,is_omp,WN_st(wn1),WN_lda_offset(wn1));

    } else if (WNOPR(wn1) == OPR_LDID) {
      cwh_stmt_add_pragma(WN_PRAGMA_REDUCTION,is_omp,WN_st(wn1),WN_load_offset(wn1));

    } else { /* an array */

      DevAssert((WN_operator(wn1)==OPR_ARRAY),("Odd reduction expression"));
      cwh_stmt_add_xpragma(WN_PRAGMA_REDUCTION,is_omp,wn1) ;
    }

    reductioncount--;
  }
  if (ontocount) {
    /* onto expressions are popped off in reverse order, so we need to
       build the nodes in reverse before attaching */
    WN *onto_block = WN_CreateBlock();
    while (ontocount) {
      /* pop the expressions */
      wn = WN_CreateXpragma( WN_PRAGMA_ONTO, (ST_IDX) NULL, 1);
      if (is_omp) WN_set_pragma_omp(wn);
      WN_kid0(wn) = cwh_expr_operand(NULL);
      WN_Set_Linenum (wn, USRCPOS_srcpos(current_srcpos) );
      WN_INSERT_BlockFirst(onto_block, wn);
      ontocount--;
    }
    cwh_block_append(onto_block);
  }
  if (threadcount || datacount) {     /* Set the Needs LNO bit for affinity */

    cwh_directive_set_LNO_flags();
  }

  while (threadcount) {
    /* pop the expressions and add to the affinty_block and the
       top_of_loop_additions block.  The top_of_loop_additions
       is prepended to the next DO loop seen.  This is because
       we need to duplicate the affinity pragma nodes for LNO */
    if (affinity_block==NULL) affinity_block=WN_CreateBlock();
    wn = WN_CreateXpragma( WN_PRAGMA_THREAD_AFFINITY, (ST_IDX) NULL, 1);
    if (is_omp)
      WN_set_pragma_omp(wn);
    WN_kid0(wn) = cwh_expr_operand(NULL);
    WN_Set_Linenum (wn, USRCPOS_srcpos(current_srcpos) );
    WN_INSERT_BlockLast(affinity_block,wn);

    wn = WN_CreateXpragma( WN_PRAGMA_THREAD_AFFINITY, (ST_IDX) NULL, 1);
    WN_kid0(wn) = cwh_expr_operand(NULL);
    cwh_directive_add_pragma_to_loop(wn,is_omp);
    threadcount--;
  }
  while (datacount) {
    /* pop the expressions and add to the affintiy_block and the
       top_of_loop_additions block.  The top_of_loop_additions
       is prepended to the next DO loop seen.  This is because
       we need to duplicate the affinity pragma nodes for LNO */
    WN *array_exp;
    ST *st;
    PREG_det preg;
    if (affinity_block==NULL) affinity_block=WN_CreateBlock();
    wn = WN_CreateXpragma( WN_PRAGMA_DATA_AFFINITY, (ST_IDX) NULL, 1);
    if (is_omp) WN_set_pragma_omp(wn);
    WN_kid0(wn) = cwh_expr_operand(NULL);
    WN_Set_Linenum (wn, USRCPOS_srcpos(current_srcpos) );
    WN_INSERT_BlockLast(affinity_block,wn);

    wn = WN_CreateXpragma( WN_PRAGMA_DATA_AFFINITY, (ST_IDX) NULL, 1);
    WN_kid0(wn) = array_exp = cwh_expr_operand(NULL);
    cwh_directive_add_pragma_to_loop(wn,is_omp);

    /* for DATA affinity - we need the preg associated with the distributed
       array symbol, get the ST for the ARRAY expression */
    DevAssert((WNOPR(array_exp)==OPR_ILOAD),("Odd expr in data affinity"));
    array_exp=WN_kid0(array_exp);  
    DevAssert((WNOPR(array_exp)==OPR_ARRAY), ("Odd expr in data affinity"));
    wn = WN_kid0(array_exp);
    DevAssert((WNOPR(wn)==OPR_LDA || WNOPR(wn)==OPR_LDID),("Not a regular array in data affinity"));
    st = WN_st(wn);
    preg = cwh_auxst_distr_preg(st);
    /* do a load of the preg */
    wn = WN_CreateXpragma( WN_PRAGMA_DATA_AFFINITY, (ST_IDX) NULL, 1);
    if (is_omp) WN_set_pragma_omp(wn);
    WN_kid0(wn) = WN_CreateLdid ( OPC_I4I4LDID, preg.preg, preg.preg_st, preg.preg_ty);
    WN_Set_Linenum (wn, USRCPOS_srcpos(current_srcpos) );
    WN_INSERT_BlockLast(affinity_block,wn);

    wn = WN_CreateXpragma( WN_PRAGMA_DATA_AFFINITY, (ST_IDX) NULL, 1);
    WN_kid0(wn) = WN_CreateLdid ( OPC_I4I4LDID, preg.preg, preg.preg_st, preg.preg_ty);
    cwh_directive_add_pragma_to_loop(wn,is_omp);
    datacount--;
  }
  /* pop off the affinity pragmas */
  while(task_affinity_count) {
    wn = cwh_stk_pop_WN();
    WN_Set_Linenum (wn, USRCPOS_srcpos(current_srcpos) );
    WN_INSERT_BlockFirst(affinity_block,wn);
    wn = cwh_stk_pop_WN();
    WN_Set_Linenum (wn, USRCPOS_srcpos(current_srcpos));    
    cwh_block_append_given_id(wn,Top_of_Loop_Block,TRUE);
    task_affinity_count--;
  }

  /* now attach all task vars passed through fei_task_var */

  while (task_var_count) {
    wn = cwh_stk_pop_WN();

    if ((WN_operator(wn) == OPR_PRAGMA) && 
	(WN_pragma(wn) == WN_PRAGMA_LOCAL)) 
      cwh_block_add_to_enclosing_regions(WN_PRAGMA_LOCAL,WN_st(wn));
    else
      cwh_block_append(wn);
    task_var_count--;
  }

  /* and any lastthread var */
  if (task_lastthread_count==1) {
    /* pop the pragma */
    wn = cwh_stk_pop_WN();
    cwh_block_append(wn);
    task_lastthread_count=0;
  }
  /* now put the affinity block (if any) */
  if (affinity_block) {
    cwh_block_append(affinity_block);
  }
 
  return(body);
} /* cwh_mp_region */


/*===============================================
 *
 * fei_parallel_region
 *
 * handles a CMIC$ parallel
 *
 * This generates a REGION and marks it as a mp region (cwh_mp_region
 * does all the work).  IF and MAXCPUS clauses handled here.
 *
 * the return value is not relevant
 *
 *===============================================
 */ 
extern int
fei_parallel_region       ( INTPTR ifexpr,
                            INTPTR maxcpus,
                            INT32 context_start,
                            INT32 context_end,
                            INT32 lineno,
                            INT32 flags )
{
 /* generate a region and mark it as a parallel region.
    Attach pragmas for any of the arguments supplied as well
    as all task vars collected.
 */
  WN *body;
 
  nested_do_descriptor.depth = 0;
  body = cwh_mp_region(WN_PRAGMA_PARALLEL_BEGIN,0,0,0,0,0,0);

  /* now attach all applicable pragmas */
  /* autoscope not handled - DLAI */

  cwh_directive_load_value_pragma(ifexpr,WN_PRAGMA_IF);
  cwh_directive_load_value_pragma(maxcpus,WN_PRAGMA_NUMTHREADS);

  cwh_directive_set_PU_flags(FALSE);

  /* append statements to region body */

  cwh_block_set_current(body);
  return(context_start);	/* return anything - its not used */
}

/*===============================================
 *
 * fei_endparallel_region
 *
 * handles a CMIC$ end parallel
 *
 * pops the region so new statements will be 
 * attached to the original body
 *
 *===============================================
 */ 
extern void
fei_endparallel_region    ( INT32 task_x,
                            INT32 lineno )
{
  cwh_directive_pop_and_nowait(FALSE,FALSE);
} 

/*===============================================
 *
 * fei_doparallel
 *
 * handles the CMIC$ do parallel directive
 *
 * generates whirl like a C$PAR PDO, handles 
 * translating the schedtype. Nest depth is 1, 
 * because not doacross or pdo with a nest clause.
 *
 * the return value is not relevant
 *
 *===============================================
 */ 
extern int
fei_doparallel            ( INT32 induc_symx,
                            INT32 work_dist,
                            INTPTR work_dist_opnd,
                            INT32 lineno )
{
  /* begin a new parallel region for the parallel loop */
  WN *body;
 
  /* Nest depth is 1 in pragma because not doacross */
  /* or pdo with a nest clause                      */

  nested_do_descriptor.depth = 1;
  task_nest_count = 0;
  nested_do_descriptor.current = 0;
  nested_do_descriptor.explicit_end = TRUE;
  nested_do_descriptor.type = WN_PRAGMA_PDO_BEGIN;
  body = cwh_mp_region(WN_PRAGMA_PDO_BEGIN,0,0,0,0,0,0);

  /* but set nest depth back to 0, so fei_enddo pops region*/

  nested_do_descriptor.depth = 0;

  /* work distribution */

  cwh_directive_work_dist(work_dist,work_dist_opnd);

  /* make region no-wait. A cmic$ enddo will alway be present for the barrier */

  cwh_stmt_add_pragma(WN_PRAGMA_NOWAIT);

  /* append statements to region body */
  cwh_block_set_current(body);
  /* mark next DO loop as a parallel loop */
  parallel_do_count = 1;

  cwh_directive_set_PU_flags(FALSE);

  return(0);	/* return anything - its not used */
}

/*===============================================
 *
 * fei_task_endloop
 *
 * processes a CMIC$ END DO
 * 
 * The end of the loop has terminated the parallel 
 * region, but a barrier is added unless the ENDDO 
 * is adjacent to an ENDPRARALLEL( implicit barrier).
 * (not implemented in yet, need flag from FE).
 *
 *===============================================
 */ 
extern void
fei_task_endloop          ( INT32 taskloop_x,
                            INT32 lineno,
                            INT32 nowait )
{
  WN *wn ;

  if (! nowait) {

    wn = WN_CreatePragma(WN_PRAGMA_BARRIER,(ST_IDX) NULL,0,0);
    cwh_directive_barrier_insert(wn, 0);
  }
}

/*===============================================
 *
 * fei_doall
 *
 * processes a CMIC$ DOALL directive
 *
 * we translate this to a DOACROSS
 *  - emit the MP region
 *  - attach pragmas
 *  - translate the work distribution
 *
 *===============================================
 */ 
extern void
fei_doall                 ( INTPTR ifexpr,
                            INTPTR maxcpus, 
                            INT32 context_start,
                            INT32 context_end,
                            INT32 induc_symx,
                            INT32 work_dist, 
                            INTPTR work_dist_opnd,
                            INT32 flags,
                            INT32 lineno )
{
  WN *body;

  nested_do_descriptor.depth = 1;	/* DOALL can only have one nest */
  task_nest_count = 0;
  nested_do_descriptor.current = 0;
  nested_do_descriptor.explicit_end = FALSE;
  nested_do_descriptor.type = WN_PRAGMA_DOACROSS;
  body = cwh_mp_region(WN_PRAGMA_DOACROSS,0,0,0,0,0,0);

  /* now attach all applicable pragmas */

  cwh_directive_load_value_pragma(ifexpr,WN_PRAGMA_IF);
  cwh_directive_load_value_pragma(maxcpus,WN_PRAGMA_NUMTHREADS);
  cwh_directive_work_dist(work_dist,work_dist_opnd);

  /* append statements to region body */
  cwh_block_set_current(body);
  /* mark next DO loop as a DOALL loop */
  parallel_do_count = 1;

  cwh_directive_set_PU_flags(FALSE);
}

/*===============================================
 *
 * cwh_doacross
 *
 * handles a C$DOACROSS or C$PAR PARALLEL DO  directive
 *
 * creates a MP region, attaches pragmas, and sets up the nesting
 * data structure.
 *
 *===============================================
 */ 
static void
cwh_doacross(INTPTR task_if_idx,
	     int schedtype,
	     int threadcount,
	     int datacount,
	     int  ontocount,
	     int reductioncount,
	     int chunkcount,
	     WN_PRAGMA_ID pragma_id,
	     int is_omp)
{
  WN *body;

  nested_do_descriptor.depth = task_nest_count;
  task_nest_count = 0;
  nested_do_descriptor.current = 0;
  nested_do_descriptor.explicit_end = FALSE;
  nested_do_descriptor.type = pragma_id;
  body = cwh_mp_region(pragma_id,threadcount,datacount,
		       ontocount,reductioncount,chunkcount,is_omp);

  /* now attach all applicable pragmas */

  cwh_directive_load_value_pragma(task_if_idx,WN_PRAGMA_IF);

  if (schedtype != WN_PRAGMA_SCHEDTYPE_UNKNOWN) {
    cwh_stmt_add_pragma(WN_PRAGMA_MPSCHEDTYPE, FALSE,(ST_IDX) NULL, schedtype);
  }

  /* append statements to region body */
  cwh_block_set_current(body);
  /* mark next n DO loops as a DOACROSS / PARALLELDO loop */
  if (nested_do_descriptor.depth) {
    parallel_do_count =nested_do_descriptor.depth;
  } else {
    parallel_do_count = 1;
  }

  cwh_directive_set_PU_flags((nested_do_descriptor.depth > 1));
} 

/*===============================================
 *
 * fei_doacross
 *
 * handles a C$DOACROSS directive
 *
 * calls cwh_doacross to do real work
 *
 *===============================================
 */ 
extern void
fei_doacross(INTPTR task_if_idx,
	     int schedtype,
	     int threadcount,
	     int datacount,
	     int  ontocount,
	     int reductioncount,
	     int chunkcount)
{
  cwh_doacross(task_if_idx,schedtype,threadcount,datacount,ontocount,
	       reductioncount,chunkcount,WN_PRAGMA_DOACROSS,0);
}

/*===============================================
 *
 * fei_paralleldo
 *
 * handles a C$PAR PARALLEL DO directive
 *
 * calls cwh_doacross to do real work
 *
 *===============================================
 */ 
extern void
fei_paralleldo( INTPTR  task_if_idx,
		int  schedtype,
		int  threadcount,
		int  datacount,
		int  ontocount,
		int  reductioncount,
		int  chunkcount )
{
  cwh_doacross(task_if_idx,schedtype,threadcount,datacount,ontocount,
	       reductioncount,chunkcount,WN_PRAGMA_PARALLEL_DO,0);
}


/*===============================================
 *
 * cwh_parallel
 *
 * process a C$PAR PARALLEL and C$OMP PARALLEL
 *
 * creates an MP region and attaches pragmas
 *
 *===============================================
 */
static void
cwh_parallel (INTPTR  task_if_idx,
	      int  defaultt,
	      int  is_omp)
{
  WN *body;

  task_nest_count = 0;
  body = cwh_mp_region(WN_PRAGMA_PARALLEL_BEGIN,0,0,0,0,0,is_omp);

  /* now attach all applicable pragmas */

  cwh_directive_load_value_pragma(task_if_idx,WN_PRAGMA_IF);

  if (defaultt) {     /* there is a DEFAULT clause */

    DevAssert((defaultt > 0 && defaultt < MAX_PRAGMA_DEFAULT),("Odd defaultt"));
    cwh_stmt_add_pragma(WN_PRAGMA_DEFAULT,FALSE,(ST_IDX) NULL,defaultt);
  }

  /* append statements to region body */
  cwh_block_set_current(body);

  cwh_directive_set_PU_flags(FALSE);

} /* cwh_parallel */

/*===============================================
 *
 * fei_parallel
 *
 * process a C$PAR PARALLEL
 *
 * creates an MP region and attaches pragmas
 *
 *===============================================
 */ 
extern void
fei_parallel (INTPTR  task_if_idx)
{
  cwh_parallel (task_if_idx, 0, 0);
} 

/*===============================================
 *
 * fei_endparallel
 *
 * processes a C$PAR END PARALLEL
 *
 * simply pops off the MP region
 *
 *===============================================
 */ 
extern void
fei_endparallel (void)
{
  cwh_directive_pop_and_nowait(FALSE,FALSE);
} 

/*===============================================
 *
 * fei_pdo
 *
 * processes a C$PAR PDO
 *
 * similar to fei_doacross
 *
 *===============================================
 */ 
extern void
fei_pdo                         ( int  sched_type,
                                  int  ordered,
                                  int  thread_count,
                                  int  data_count,
                                  int  onto_count,
				  int  reduction_count,
                                  int  chunk_count )
{
  /* begin a new parallel region for the parallel loop */
  WN *body,*wn;
 
  nested_do_descriptor.depth = task_nest_count;
  task_nest_count = 0;
  nested_do_descriptor.current = 0;
  nested_do_descriptor.explicit_end = TRUE;
  nested_do_descriptor.type = WN_PRAGMA_PDO_BEGIN;
  body = cwh_mp_region(WN_PRAGMA_PDO_BEGIN,thread_count,data_count,
		onto_count,reduction_count, chunk_count,0);
  /* schedtype */
  if (sched_type != WN_PRAGMA_SCHEDTYPE_UNKNOWN) {
    cwh_stmt_add_pragma(WN_PRAGMA_MPSCHEDTYPE, FALSE,(ST_IDX) NULL, sched_type);
  }

  if (ordered) {

    wn = WN_CreatePragma(WN_PRAGMA_ORDERED, (ST_IDX) NULL, 0, 0);
    cwh_block_append(wn);
  }

  /* append statements to region body */
  cwh_block_set_current(body);
  /* mark next n DO loops nested loops */
  if (nested_do_descriptor.depth) {
   parallel_do_count =nested_do_descriptor.depth;
  } else {
   parallel_do_count = 1;
  }

  cwh_directive_set_PU_flags(nested_do_descriptor.depth > 1);
  
} /* fei_pdo */

/*===============================================
 *
 * fei_endpdo
 *
 * process a C$PAR END PDO [nowait]  
 *
 * similar to ending any other parallel loop, but we may need to add
 * a NOWAIT pragma to the region pragma list.
 *
 *===============================================
 */ 
extern void
fei_endpdo                ( int  nowait )
{
  cwh_directive_pop_and_nowait(nowait,FALSE);
}

/*===============================================
 *
 * fei_singleprocess
 *
 * process a C$PAR SINGLE PROCESS
 *
 *===============================================
 */ 
extern void
fei_singleprocess( void )
{
  WN *body;

  body = cwh_mp_region(WN_PRAGMA_SINGLE_PROCESS_BEGIN,0,0,0,0,0,0);

  /* append statements to region body */
  cwh_block_set_current(body);

  /* set the MP and uplevel bits on the symtab */

   cwh_directive_set_PU_flags(FALSE);

} /* fei_singleprocess */

/*===============================================
 *
 * fei_endsingleprocess
 *
 * process a C$PAR END SINGLE PROCESS
 *
 *===============================================
 */ 
extern void
fei_endsingleprocess ( int  nowait )
{
  cwh_directive_pop_and_nowait(nowait,FALSE);
}

/*===============================================
 *
 * fei_criticalsection
 *
 * process a C$PAR CRITICAL SECTION
 *
 * if there is a user variable, generate an Xpragma, otherwise generate
 *  a pragma.   Followed by a barrier.
 *
 *===============================================
 */ 
extern void
fei_criticalsection       ( int  var_count )
{
  WN *wn;

  if (var_count==0) {
    cwh_stmt_add_pragma(WN_PRAGMA_CRITICAL_SECTION_BEGIN);

  } else {

    DevAssert((var_count==1),("too many critical section vars"));
    wn = cwh_expr_address(f_NONE);
    cwh_stmt_add_xpragma(WN_PRAGMA_CRITICAL_SECTION_BEGIN,FALSE,wn);
  }

  wn = WN_CreateBarrier( FALSE, 0 );
  cwh_block_append(wn);
}

/*===============================================
 *
 * fei_endcriticalsection
 *
 * handles a C$PAR END CRITICAL SECTION
 *
 * generates a barrier and a pragma
 *
 *===============================================
 */ 
extern void
fei_endcriticalsection    ( void )
{
  WN *wn;
  wn = WN_CreateBarrier( TRUE, 0 );
  cwh_block_append(wn);
  cwh_stmt_add_pragma(WN_PRAGMA_CRITICAL_SECTION_END);
} 

/*===============================================
 *
 * fei_barrier
 *
 * handles a C$PAR BARRIER
 *
 * generates a barrier pragma surrounded by barriers
 *
 *===============================================
 */ 
extern void
fei_barrier      ( void )
{
  WN *wn;

  wn = WN_CreatePragma(WN_PRAGMA_BARRIER,(ST_IDX) NULL,0,0);
  cwh_directive_barrier_insert(wn, 0);
} 

/*===============================================
 *
 * fei_section
 *
 * handles a C$PAR SECTION
 *
 * simply generates a WN_PRAGMA_SECTION pragma
 *
 *===============================================
 */ 
extern void
fei_section               ( void )
{
  cwh_stmt_add_pragma(WN_PRAGMA_SECTION);
}

/*===============================================
 *
 * fei_psection
 *
 * handles a C$PAR PSECTION
 *
 * generates a WN_PRAGMA_PSECTION_BEGIN, 
 * local vars may add additional pragmas. 
 * Set current block to region body
 *
 *===============================================
 */ 
extern void
fei_psection              ( void )
{
  WN *body;
  body = cwh_mp_region(WN_PRAGMA_PSECTION_BEGIN,0,0,0,0,0,0);

  cwh_block_set_current(body);
  cwh_directive_set_PU_flags(FALSE);
}

/*===============================================
 *
 * fei_endpsection
 *
 * handles a C$PAR END PSECTION
 *
 * simply adds a WN_PRAGMA_PSECTION_END pragma
 *
 *===============================================
 */ 
extern void
fei_endpsection           ( int  nowait )
{
  cwh_directive_pop_and_nowait(nowait,FALSE);
} 

/*===============================================
 *
 * fei_copy_in
 *
 * handles a C$COPYIN
 *
 * each argument is a ST on the stack, for common blocks we generate
 *  a pragma, for common block members, we generate an Xpragma.
 * as a special case, split commons generate a pragma for each child
 *
 *===============================================
 */ 
extern void
fei_copy_in               ( int   list_count )
{
  int i;
  ST *st;
  WN *wn;
  BOOL create_xpragma;
  ITEM *split_st;

  for(i=0; i< list_count; i++) {
    create_xpragma = TRUE;
    if (cwh_stk_get_class()==ST_item || cwh_stk_get_class() == ST_item_whole_array) {

      /*  may be a simple var or a common block */
      st = cwh_stk_pop_ST();

      if (ST_sclass(st) == SCLASS_COMMON && ST_base_idx(st) == ST_st_idx(st)) {
	create_xpragma = FALSE;
	/* a common block, create a pragma for it */
	/* if this common has been split, then we have to issue a
	   pragma for each child.  We can tell if the parent has been split
	   by checking if the AUXST's splitlist is empty or not */
	split_st=cwh_auxst_next_element(st,(ST_IDX) NULL,l_SPLITLIST);
	
	if (split_st) {
	  while(split_st) {
	    cwh_stmt_add_pragma(WN_PRAGMA_COPYIN, FALSE, I_element(split_st));
	    split_st=cwh_auxst_next_element(st,split_st,l_SPLITLIST);
	  }
	} else { 	/* this is a normal common (not split) */
	  cwh_stmt_add_pragma(WN_PRAGMA_COPYIN, FALSE, st);
	}
      } else {
	/* simple var - put it back */
	cwh_stk_push(st,ST_item);
      }
    }
    /* at this point create_xpragma is TRUE if there is an expression on the stack to 
       use for the Xpragma */

    if (create_xpragma) {
      wn = cwh_expr_address(f_NONE);
      cwh_stmt_add_xpragma(WN_PRAGMA_COPYIN,FALSE,wn);
    }
  }
  cwh_directive_set_PU_flags(FALSE);
  
} /* fei_copy_in */

void
fei_dynamic               ( int   list_count )
{
  int i;
  ST *st;
  for(i=0; i< list_count; i++) {
    st = cwh_stk_pop_ST();
    cwh_stmt_add_pragma(WN_PRAGMA_DYNAMIC, FALSE,st);
  }
}

void
fei_redistribute          ( INTPTR   array,
                            int   dim,
                            int   distribution,
                            int   cyclic_exists,
                            int   onto_exists )
{
  /* for each distribution, build the pragma */
  static WN *redistribute_block;	/* holds the built up pragma list */
  static int onto_count;		/* counts # of onto exprs pushed */
  INT cyclic_constant;		/* the constant for cyclic if non-expr */
  BOOL cyclic_is_constant;	/* CYCLIC value is constant (or missing) */
  ST *st;
  TY_IDX ty;
  STB_pkt *p;
  WN *wn, *wn1, *wn_for_cyclic_expr=NULL;
  WN *lb,*ub,*s1;
  PREG_det preg;

  p = cast_to_STB(array);
  st = (ST *)p->item;
  ty = ST_type(st);
  /* we want the array type, if this is a pointer type, deref it to the
     array */
  if (TY_kind(ty) == KIND_POINTER) {
    ty = TY_pointed(ty);
  }
  /* initialize if first dim */
  if (dim==1) {
    onto_count=0;
    redistribute_block=WN_CreateBlock();
  }
  /* generate the pragma for this dimension and add to the top of the
     block, the order should be distribution, extent, cyclic_expr (if any).
     Since we always add to the top of the block, we generate the pragmas
     in reverse. */
  cyclic_constant = 1;		/* default value  if missing */
  cyclic_is_constant=TRUE;
  if (cyclic_exists) {
    /* there is a cyclic expr or cyclic constant */
    wn1 = cwh_expr_operand(NULL);
    if(WN_operator(wn1)==OPR_INTCONST) {
      cyclic_constant = WN_const_val(wn1);
    } else {
      /* an expression, build an Xpragma node */
      cyclic_is_constant=FALSE;
      wn_for_cyclic_expr = WN_CreateXpragma(WN_PRAGMA_REDISTRIBUTE, st, 1);
      WN_kid0(wn_for_cyclic_expr) = wn1;
    }
  }
  /* build the XPragma representing the extent for this dimension */
  lb = cwh_types_bound_WN(ty,dim-1,LOW);
  ub = cwh_types_bound_WN(ty,dim-1,UPPER);
  s1 = WN_Intconst(MTYPE_I4,1);
  wn = WN_CreateXpragma(WN_PRAGMA_REDISTRIBUTE, st, 1);
  WN_kid0(wn) = cwh_addr_extent(lb,ub,s1);
  WN_INSERT_BlockFirst(redistribute_block, wn);
  if (wn_for_cyclic_expr) {
    /* insert the cyclic expr */
    WN_INSERT_BlockFirst(redistribute_block, wn_for_cyclic_expr);
  }

  /* now build the Pragma for the distribute */

  wn = WN_CreatePragma (WN_PRAGMA_REDISTRIBUTE,st,0,0);
  WN_pragma_index(wn) = TY_AR_ndims(Ty_Table[ty])-dim;

  switch(distribution) {
    case Block_Dist:
      WN_pragma_distr_type(wn)=DISTRIBUTE_BLOCK;
      break;
    case Star_Dist:
      WN_pragma_distr_type(wn)=DISTRIBUTE_STAR;
      break;
    case Cyclic_Dist:
      if (cyclic_is_constant) {
        WN_pragma_distr_type(wn)=DISTRIBUTE_CYCLIC_CONST;
	WN_pragma_preg(wn) = cyclic_constant;
      } else {
        WN_pragma_distr_type(wn)=DISTRIBUTE_CYCLIC_EXPR;
      }
      break;
    default:
      DevAssert((0),("fei_redistribute: unexpected distribution"));
  }
  WN_INSERT_BlockFirst(redistribute_block, wn);

  /* leave the onto expressions stacked, they will be stacked so
     the last dimension is popped first. */
  if (onto_exists) onto_count++;

  /* if this is the last dimension, add the dummy preg stores to
     the end of the block, then add the ONTO exprs to the end of
     the block, then dump the entire block to the statement list */

  if (dim==TY_AR_ndims(Ty_Table[ty])) {

    /* add the dummy preg stores */
    preg = cwh_auxst_distr_preg(st);
    wn = cwh_load_distribute_temp();
    wn = WN_CreateStid( OPC_I4STID, preg.preg, preg.preg_st, preg.preg_ty, wn);
    WN_INSERT_BlockLast(redistribute_block, wn);

    /* create another write to the global preg for all distributed arrays */
    if (preg_for_distribute.preg==-1) {
      preg_for_distribute=cwh_preg_next_preg(MTYPE_I4, NULL, NULL);
    }
    wn = cwh_load_distribute_temp();
    wn = WN_CreateStid( OPC_I4STID, preg_for_distribute.preg,
	preg_for_distribute.preg_st, preg_for_distribute.preg_ty, wn);
    WN_INSERT_BlockLast(redistribute_block, wn);

    /* add any ONTO exprs */
    while(onto_count--) {
      wn1 = cwh_expr_operand(NULL);
      wn = WN_CreateXpragma(WN_PRAGMA_ONTO,st,1);
      WN_kid0(wn) = wn1;
      WN_INSERT_BlockLast(redistribute_block, wn);
    }

    cwh_block_append(redistribute_block);
    cwh_directive_set_LNO_flags();
  }
}

static void
cwh_reorder           ( int expressions, WN_PRAGMA_ID wn_pragma_id )
{
  int *order;
  ST **st_list;
  int i;
  WN *wn;

  order= (int *) malloc(sizeof(int)*expressions);
  st_list= (ST **)malloc(sizeof(ST *)*expressions);

  /* pop constants off and set positions in the order array */

  for(i=expressions; i> 0; i--) /* reverse order */ {
    wn = cwh_expr_operand(NULL);
    DevAssert((WN_operator(wn)==OPR_INTCONST),("cwh_reorder: expected constant"));
    DevAssert((WN_const_val(wn)<=expressions),("cwh_reorder: constant out of bounds"));
    order[WN_const_val(wn)-1]=i;
  }

  /* pop off ST's and save */
  for(i=expressions-1; i>=0; i--) /* reverse order */ {
    st_list[i] = cwh_stk_pop_ST();
  }

  /* now generate whirl */
  for(i=0; i<expressions; i++) { 
    cwh_stmt_add_pragma(wn_pragma_id, FALSE,st_list[i], order[i],i+1);
  }
  free(order);
  free(st_list);
} 

void  fei_interchange           ( int expressions )
{
  cwh_reorder(expressions,WN_PRAGMA_INTERCHANGE);
} 


void  fei_blockable             ( int expressions )
{
  cwh_reorder(expressions,WN_PRAGMA_BLOCKABLE);
} 

/*===============================================
 *
 * fei_fission
 *
 *===============================================
*/
void  fei_fission               ( void )
{
  WN *wn;
  wn = cwh_expr_operand(NULL);
  DevAssert((WN_operator(wn)==OPR_INTCONST),("fei_fission: expected constant"));
  cwh_stmt_add_pragma(WN_PRAGMA_FISSION,FALSE,(ST_IDX) NULL,WN_const_val(wn),0);

} /* fei_fission */
#ifdef KEY
/*===============================================
 *
 * fei_forall
 *
 *===============================================
*/
void  fei_forall               ( void )
{
  WN *wn;
  cwh_stmt_add_pragma(WN_PRAGMA_FORALL,FALSE,(ST_IDX) NULL,0,0);

} /* fei_forall */
#endif
/*===============================================
 *
 * fei_flush
 *
 *===============================================
*/
extern void
fei_flush(int list_count)
{
  WN *sync;

  sync = WN_Create_Intrinsic(OPC_VINTRINSIC_CALL,INTRN_SYNCHRONIZE,0,NULL);
  cwh_directive_barrier_insert(sync,list_count) ;
}

/*===============================================
 *
 * fei_fuse
 *
 *===============================================
*/
void  
fei_fuse (int  level)
{
  WN *wn;
  wn = cwh_expr_operand(NULL);
  DevAssert((WN_operator(wn)==OPR_INTCONST),("fei_fuse: expected constant"));
  cwh_stmt_add_pragma(WN_PRAGMA_FUSE,FALSE,(ST_IDX) NULL,WN_const_val(wn), level);
} 

/*===============================================
 *
 * fei_assert
 *
 *===============================================
*/
void
fei_assert ( int  assertion, int  list_count )
{
  /* for now - the FE doesnt parse arguments to the assertions, so we
    only get the assertion code.  When the FE is updated we would need
    to modify the code here to handle the arguments
  */
  /* global asserts are not handled yet - DLAI */
  WN *arg;

  DevAssert((map_asserts[assertion].fe_enum == assertion),
	    ("map_asserts table bad")); /* mismatch between map_asserts and globals.m */

  switch(assertion) {
   case MIPS_ASSERT_CONCURRENTCALL:
   case MIPS_ASSERT_NOCONCURRENTCALL:
   case MIPS_ASSERT_NOEQUIVALENCEHAZARD:
   case MIPS_ASSERT_BOUNDSVIOLATIONS:
   case MIPS_ASSERT_NOBOUNDSVIOLATIONS:
   case MIPS_ASSERT_EQUIVALENCEHAZARD:
   case MIPS_ASSERT_TEMPORARIESFORCONSTANTARGUMENTS:
   case MIPS_ASSERT_NOTEMPORARIESFORCONSTANTARGUMENTS:
   case MIPS_ASSERT_BENIGN:
   case MIPS_ASSERT_NOINTERCHANGE:
   case MIPS_ASSERT_USECOMPRESS:
   case MIPS_ASSERT_USEEXPAND:
   case MIPS_ASSERT_USECONTROLLEDSTORE:
   case MIPS_ASSERT_USEGATHER:
   case MIPS_ASSERT_USESCATTER:
     cwh_stmt_add_pragma(map_asserts[assertion].wn_pragma_id);
     break;

   case MIPS_ASSERT_ARGUMENTALIASING:
     Set_PU_args_aliased (Pu_Table[ST_pu(Procedure_ST)]);
     cwh_stmt_add_pragma(map_asserts[assertion].wn_pragma_id);
     break;

   case MIPS_ASSERT_NOARGUMENTALIASING:
     Clear_PU_args_aliased (Pu_Table[ST_pu(Procedure_ST)]);
     cwh_stmt_add_pragma(map_asserts[assertion].wn_pragma_id);
     break;

   case MIPS_ASSERT_DO:
   case MIPS_ASSERT_DOPREFER:
     while (list_count) {
	arg = cwh_expr_operand(NULL);
	cwh_stmt_add_pragma(map_asserts[assertion].wn_pragma_id,
			    FALSE,
			    (ST_IDX) NULL,
			    WN_const_val(arg));
	list_count--;
     }
     break;

   case MIPS_ASSERT_PERMUTATION:
     while (list_count) {
	arg = cwh_expr_address(f_NONE);
	cwh_stmt_add_pragma(map_asserts[assertion].wn_pragma_id, 
			    FALSE,
			    WN_st(arg),
			    WN_load_offset(arg));
	list_count--;
     }
     break;

   default:
     DevWarn("fei_assert: assertion not implemented");
     while (list_count) {
       cwh_stk_pop_whatever();
       list_count--;
     }
     break;
  }
} /* fei_assert */

/*===============================================
 *
 * fei_fill_symbol
 * 
 * handles c*$* fill_symbol. The stack contains
 * the ST's to be popped.
 *
 *===============================================
 */ 
extern void
fei_fill_symbol(INT32  count, INT32  C_value )
{
  cwh_directive_fill_align(count,C_value,WN_PRAGMA_FILL);
} 

/*===============================================
 *
 * fei_align_symbol
 * 
 * handles c*$*  align_symbol. The stack contains
 * the ST's to be popped. They can be variables
 * or COMMON symbols.
 *
 *===============================================
 */ 
extern void
fei_align_symbol (INT32  count,INT32   C_value )
{
  cwh_directive_fill_align(count,C_value,WN_PRAGMA_ALIGN);
} 

/*===============================================
 *
 * cwh_directive_fill_align
 * 
 * Utility for fei_align/fill_symbol.
 * Pops the STs and adds the appropriate pragma
 *
 *===============================================
 */ 
static void
cwh_directive_fill_align(INT32 count,INT32 C_value, WN_PRAGMA_ID pragma )
{
  ST *st;
  PU & pu = Pu_Table[ST_pu(Procedure_ST)];
  BOOL pu_is_l2 = (CURRENT_SYMTAB == 1+GLOBAL_SYMTAB) ;

  while (count-- > 0) {

    st = cwh_stk_pop_ST();
    Set_ST_is_fill_align(st);

    if ((ST_level(st) != GLOBAL_SYMTAB) ||
        ((ST_level(st) == GLOBAL_SYMTAB) && pu_is_l2)) {

      /* The PU needs fill-align lowering if the symbol is local,
       * or if the symbol is global and this is the first PU.
       */
      Set_PU_needs_fill_align_lowering (pu);
    }
    cwh_stmt_add_pragma(pragma,FALSE,st,0,C_value);
  }
}

void
fei_unroll( void )
{
  WN *wn1;
  wn1 = cwh_expr_operand(NULL);
  DevAssert((WN_operator(wn1)==OPR_INTCONST),("fei_unroll: expected constant"));

  if (WN_const_val(wn1) != 0) {
    cwh_stmt_add_pragma(WN_PRAGMA_UNROLL, FALSE,(ST_IDX) NULL, WN_const_val(wn1), -1);
  }
  WN_Delete(wn1);
} 

void
fei_page_place( void )
{
  /* there are 3 operands on the stack */

  WN *wn1,*wn2,*wn3;
  wn3 = cwh_expr_operand(NULL);
  wn2 = cwh_expr_operand(NULL);
  wn1 = cwh_expr_address(f_NONE);

  cwh_stmt_add_xpragma(WN_PRAGMA_PAGE_PLACE,FALSE,wn1);
  cwh_stmt_add_xpragma(WN_PRAGMA_PAGE_PLACE,FALSE,wn2);
  cwh_stmt_add_xpragma(WN_PRAGMA_PAGE_PLACE,FALSE,wn3);

  cwh_directive_set_LNO_flags();

} 

void
fei_prefetch_ref_disable  ( INTPTR   array,
                            int   size )
{
  STB_pkt *p;
  ST *st;

  p = cast_to_STB(array);
  DevAssert((p->form == is_ST),("Odd object ref"));
  st = (ST *)p->item;
  if (size==-1) size=0;
  cwh_stmt_add_pragma(WN_PRAGMA_PREFETCH_REF_DISABLE,FALSE,st,0,size);
} 


void
fei_prefetch_ref          ( int   stride,
                            int   level,
                            int   kind,
                            int   size )
{
   INT lev1,lev2;
   INT str1,str2,itemp;
   WN * wn;
   WN * t;

   /* Build the PREFETCH pragma */
   if (size == -1) size = 0;
   cwh_stmt_add_pragma(WN_PRAGMA_PREFETCH_REF,
		       FALSE,
		       (ST_IDX) NULL,
		       0,
		       size);

   /* Pick up the level numbers */
   if (level == 2) {
      t = cwh_expr_operand(NULL);
      lev2 = WN_const_val(t);
      t = cwh_expr_operand(NULL);
      lev1 = WN_const_val(t);
   } else if (level == 1) {
      t = cwh_expr_operand(NULL);
      lev1 = WN_const_val(t);
      lev2 = 0;
   } else {
      lev1 = 2;
      lev2 = 0;
   }

   /* Pick up the strides */
   if (stride == 2) {
      t = cwh_expr_operand(NULL);
      str2 = WN_const_val(t);
      t = cwh_expr_operand(NULL);
      str1 = WN_const_val(t);
   } else if (stride == 1) {
      t = cwh_expr_operand(NULL);
      str1 = WN_const_val(t);
      str2 = 0;
   } else {
      str1 = 1;
      str2 = 0;
   }

   if (lev1 == 2) {
      /* swap the two strides */
      itemp = str1;
      str1 = str2;
      str2 = itemp;
   }

   /* Get the ARRAY node */
   wn = cwh_expr_address(f_NONE);
   wn = WN_CreatePrefetch ( 0, 0, wn );
   WN_pf_set_confidence(wn, 3);
   WN_pf_set_manual(wn);      
   if (kind == 0) {
      WN_pf_set_read(wn);
   } else {
      WN_pf_set_write(wn);
   }

   WN_pf_set_stride_1L(wn, str1);
   WN_pf_set_stride_2L(wn, str2);
   
   cwh_block_append(wn);
} /* fei_prefetch_ref */


void
fei_prefetch(int   n1,
             int   n2 )
{
  cwh_stmt_add_pragma(WN_PRAGMA_PREFETCH,FALSE,(ST_IDX) NULL,n1,n2);
}

#ifdef KEY /* Bug 2660 */
static ST *cwh_create_str_st(char *string);

void
fei_options(char *n1)
{
  ST *st = cwh_create_str_st(n1);
  cwh_stmt_add_options_pragma(st);
}
#endif /* KEY Bug 2660 */

void
fei_prefetch_manual( int   n )
{
  cwh_stmt_add_pragma(WN_PRAGMA_PREFETCH_MANUAL,FALSE,(ST_IDX) NULL,n,0);
} 

void
fei_regionbegin           ( void )
{
  WN *body = cwh_region(REGION_KIND_PRAGMA);
  cwh_block_set_current(body);
} /* fei_regionbegin */

void
fei_regionend             ( void )
{
  /* end the region */
  (void) cwh_block_pop_region();
} /* fei_regionend */


void
fei_section_gp            ( int   list_count )
{
  int i;
  ST *st;
  for (i=0; i<list_count; i++) {
    st = cwh_stk_pop_ST();

    if (Has_Base_Block(st) && (ST_sclass(ST_base(st))==SCLASS_COMMON)) {
      /* set flags on parent common */
      st = ST_base(st);
    }
    Set_ST_gprel(st);
  }
} /* fei_section_gp */

void
fei_section_nongp         ( int   list_count )
{
  int i;
  ST *st;
  for (i=0; i<list_count; i++) {
    st = cwh_stk_pop_ST();

    if (Has_Base_Block(st) && (ST_sclass(ST_base(st))==SCLASS_COMMON)) {
      /* set flags on parent common */
      st = ST_base(st);
    }
    Set_ST_not_gprel(st);
  }
} /* fei_section_nongp */

void
fei_blocking_size         ( void )
{
  WN *wn1,*wn2;
  wn1 = cwh_expr_operand(NULL);
  wn2 = cwh_expr_operand(NULL);
  DevAssert((WN_operator(wn1)==OPR_INTCONST),("fei_blocking_size: expected constant"));
  DevAssert((WN_operator(wn2)==OPR_INTCONST),("fei_blocking_size: expected constant"));

  cwh_stmt_add_pragma(WN_PRAGMA_BLOCKING_SIZE, 
		      FALSE,
		      (ST_IDX) NULL, 
		      WN_const_val(wn2),
		      WN_const_val(wn1));

} /* fei_blocking_size */

void
fei_opaque  ( void )
{
  WN *save ;
  save = cwh_block_set_region_pragmas();
  cwh_stmt_add_pragma(WN_PRAGMA_OPAQUE);
  cwh_block_set_current(save);
} /* fei_opaque */

void 
fei_concurrentize ( int	state )
{
  WN_PRAGMA_ID id;

  if (state) {
    id = WN_PRAGMA_KAP_CONCURRENTIZE ;
  }
  else {
    id  = WN_PRAGMA_KAP_NOCONCURRENTIZE;
  }
  cwh_stmt_add_pragma(id);
}

int
fei_par_case ( INT32 task_x, INT32 lineno )
{
  WN *body;

  if (task_x == 0) {
    body = cwh_mp_region(WN_PRAGMA_PSECTION_BEGIN,0,0,0,0,0,0);

    /* append statements to region body */
    cwh_block_set_current(body);

    cwh_directive_set_PU_flags(FALSE);

  } else {
      cwh_stmt_add_pragma(WN_PRAGMA_SECTION);
  }
  return (1);
}


void
fei_par_endcase ( INT32 task_x,  INT32 lineno )
{
  (void) cwh_block_pop_region();
}


/* TBD guard/endguard currently ignore the lock number.  Do this for 7.2+ */

int
fei_guard ( INT32 guard_num, INT32 lineno )
{
   WN *wn;

   cwh_stmt_add_pragma(WN_PRAGMA_CRITICAL_SECTION_BEGIN);

   wn = WN_CreateBarrier (FALSE,0);
   cwh_block_append(wn);
   return (1);
}

void 
fei_endguard (INT32 task_x, INT32 guard_num, INT32 lineno )
{
   WN *wn;
   wn = WN_CreateBarrier (TRUE,0);
   cwh_block_append(wn);

   cwh_stmt_add_pragma(WN_PRAGMA_CRITICAL_SECTION_END);
}

/*===============================================
 *
 *
 * fei_parallelsections_open_mp
 *
 * updated: by jhs, 02/7/20
 *===============================================
*/
void
fei_parallelsections_open_mp(INTPTR task_if_idx,
			     INTPTR task_num_threads_idx,
                             int defaultt)
{
  WN *body;

  task_nest_count = 0;
  body = cwh_mp_region(WN_PRAGMA_PARALLEL_SECTIONS,0,0,0,0,0,1);

  /* now attach all applicable pragmas */

  cwh_directive_load_value_pragma(task_if_idx,WN_PRAGMA_IF,TRUE);
  cwh_directive_load_value_pragma(task_num_threads_idx, WN_PRAGMA_NUMTHREADS, TRUE);

  if (defaultt) {     /* there is a DEFAULT clause */

    DevAssert((defaultt > 0 && defaultt < MAX_PRAGMA_DEFAULT),("Odd defaultt"));
    cwh_stmt_add_pragma(WN_PRAGMA_DEFAULT,TRUE,(ST_IDX) NULL,defaultt);
  }

  /* append statements to region body */

  cwh_block_set_current(body);

  cwh_directive_set_PU_flags(FALSE);
}

/*===============================================
 *
 * fei_paralleldo_open_mp 
 * 
 * 
 *===============================================
*/ 
void 
fei_paralleldo_open_mp (INTPTR task_if_idx,
			INTPTR task_num_threads_idx,
			int defaultt,
			int ordered,
			int scheduletype,
			INTPTR schedulechunk,
			int threadcount,
			int datacount,
			int ontocount)
{
  WN *body;

  nested_do_descriptor.depth = task_nest_count;
  task_nest_count = 0;
  nested_do_descriptor.current = 0;
  nested_do_descriptor.explicit_end = TRUE;
  nested_do_descriptor.type = WN_PRAGMA_PARALLEL_DO;
  body = cwh_mp_region(WN_PRAGMA_PARALLEL_DO,threadcount,datacount,ontocount,
                       0,0,1);

  /* now attach all applicable pragmas */

  cwh_directive_load_value_pragma(task_if_idx,WN_PRAGMA_IF,TRUE);
  cwh_directive_load_value_pragma(task_num_threads_idx, WN_PRAGMA_NUMTHREADS, TRUE);

  if (defaultt) {      /* there is a DEFAULT clause */

    DevAssert((defaultt > 0 && defaultt < MAX_PRAGMA_DEFAULT),("Odd defaultt"));
    cwh_stmt_add_pragma(WN_PRAGMA_DEFAULT,TRUE,(ST_IDX) NULL,defaultt);
  }

  if (scheduletype != WN_PRAGMA_SCHEDTYPE_UNKNOWN) {

    cwh_stmt_add_pragma(WN_PRAGMA_MPSCHEDTYPE, TRUE, (ST_IDX) NULL, scheduletype);
    cwh_directive_load_value_pragma(schedulechunk,WN_PRAGMA_CHUNKSIZE,TRUE);
  }

  /* ordered */

  if (ordered) {
    cwh_stmt_add_pragma(WN_PRAGMA_ORDERED,TRUE);
  }

  /* append statements to region body */
  cwh_block_set_current(body);

  /* mark next n DO loops nested loops */
  if (nested_do_descriptor.depth) {
   parallel_do_count =nested_do_descriptor.depth;
  } else {
   parallel_do_count = 1;
  }

  /* set the MP and uplevel bits on the symtab */

  cwh_directive_set_PU_flags(nested_do_descriptor.depth > 1);
}

/*===============================================
 *
 * fei_single_open_mp 
 * 
 * create a new region & make body current block
 *
 *===============================================
*/ 
void 
fei_single_open_mp (void) 
{
  WN *body;

  body = cwh_mp_region(WN_PRAGMA_SINGLE_PROCESS_BEGIN,0,0,0,0,0,1);

  cwh_block_set_current(body);

  cwh_directive_set_PU_flags(FALSE);
}

/*===============================================
 *
 * fei_sections_open_mp 
 * 
 * create a new region & make body current block
 * 
 *===============================================
*/ 
void 
fei_sections_open_mp(void)
{
  WN *body;
  body = cwh_mp_region(WN_PRAGMA_PSECTION_BEGIN,0,0,0,0,0,1);

  cwh_block_set_current(body);

  cwh_directive_set_PU_flags(FALSE);

}

/*===============================================
 *
 * fei_do_open_mp 
 * 
 * 
 *===============================================
*/ 
void
fei_do_open_mp (int ordered,
		int scheduletype,
		INTPTR schedulechunk,
		int threadcount,
		int datacount,
		int ontocount)
{
  WN *body;

  nested_do_descriptor.depth = task_nest_count;
  task_nest_count = 0;
  nested_do_descriptor.current = 0;
  nested_do_descriptor.explicit_end = TRUE;
  nested_do_descriptor.type = WN_PRAGMA_PDO_BEGIN;
  body = cwh_mp_region(WN_PRAGMA_PDO_BEGIN,threadcount,datacount,ontocount,
                       0,0,1);

  if (scheduletype != WN_PRAGMA_SCHEDTYPE_UNKNOWN) {

    cwh_stmt_add_pragma(WN_PRAGMA_MPSCHEDTYPE, TRUE,(ST_IDX) NULL, scheduletype);
    cwh_directive_load_value_pragma(schedulechunk,WN_PRAGMA_CHUNKSIZE,TRUE);
  }

  if (ordered) {
    cwh_stmt_add_pragma(WN_PRAGMA_ORDERED, TRUE);
  }

  /* append statements to region body */
  cwh_block_set_current(body);

  /* mark next n DO loops nested loops */
  if (nested_do_descriptor.depth) {
   parallel_do_count =nested_do_descriptor.depth;
  } else {
   parallel_do_count = 1;
  }

  cwh_directive_set_PU_flags(nested_do_descriptor.depth > 1);
}

/*===============================================
 *
 * fei_workshare_open_mp 
 * 
 * create a new region & make body current block
 * 
 *===============================================
*/ 
void
fei_workshare_open_mp (void)
{
  WN *body;

  body = cwh_mp_region(WN_PRAGMA_PWORKSHARE_BEGIN,0,0,0,0,0,1);

  cwh_block_set_current(body);

  cwh_directive_set_PU_flags(FALSE);
}

/*===============================================
 *
 * fei_endworkshare_open_mp 
 * 
 * similar to ending any other parallel loop, but we may need to add
 * a NOWAIT pragma to the region pragma list.
 * 
 *===============================================
*/ 
extern void 
fei_endworkshare_open_mp     ( INT32 nowait )
{
  cwh_directive_pop_and_nowait(nowait,TRUE);
}

/*===============================================
 *
 * fei_endparallelworkshare_open_mp 
 * 
 *===============================================
*/ 
extern void 
fei_endparallelworkshare_open_mp   ( void )
{
  cwh_directive_pop_and_nowait(FALSE,TRUE);
}

/*===============================================
 *
 * fei_parallelworkshare_open_mp 
 * 
 *===============================================
*/ 
void 
fei_parallelworkshare_open_mp(INTPTR task_if_idx,
		     INTPTR task_num_threads_idx,
		     INT32 defaultt) 
{
  WN *body;

  task_nest_count = 0;
  body = cwh_mp_region(WN_PRAGMA_PARALLEL_WORKSHARE,0,0,0,0,0,TRUE);

  /* now attach all applicable pragmas */

  cwh_directive_load_value_pragma(task_if_idx,WN_PRAGMA_IF, TRUE);
  cwh_directive_load_value_pragma(task_num_threads_idx, WN_PRAGMA_NUMTHREADS, TRUE);

  if (defaultt) {     /* there is a DEFAULT clause */

    DevAssert((defaultt > 0 && defaultt < MAX_PRAGMA_DEFAULT),("Odd defaultt"));
    cwh_stmt_add_pragma(WN_PRAGMA_DEFAULT,TRUE,(ST_IDX) NULL,defaultt);
  }

  /* append statements to region body */
  cwh_block_set_current(body);

  cwh_directive_set_PU_flags(FALSE);
}

/*===============================================
 *
 * fei_parallel_open_mp 
 * 
 *===============================================
*/ 
void 
fei_parallel_open_mp(INTPTR task_if_idx,
		     INTPTR task_num_threads_idx,
		     int defaultt) 
{
  WN *body;

  task_nest_count = 0;
  body = cwh_mp_region(WN_PRAGMA_PARALLEL_BEGIN,0,0,0,0,0,TRUE);

  /* now attach all applicable pragmas */

  cwh_directive_load_value_pragma(task_if_idx,WN_PRAGMA_IF, TRUE);
  cwh_directive_load_value_pragma(task_num_threads_idx, WN_PRAGMA_NUMTHREADS, TRUE);

  if (defaultt) {     /* there is a DEFAULT clause */

    DevAssert((defaultt > 0 && defaultt < MAX_PRAGMA_DEFAULT),("Odd defaultt"));
    cwh_stmt_add_pragma(WN_PRAGMA_DEFAULT,FALSE,(ST_IDX) NULL,defaultt);
  }

  /* append statements to region body */
  cwh_block_set_current(body);

  cwh_directive_set_PU_flags(FALSE);
}
/*===============================================
 *
 * cwh_create_str_st
 *
 * generate a ST to represent a character string
 *
 *===============================================
*/
static ST *
cwh_create_str_st(char *string)
{
  TCON           tcon;
  TY_IDX ty;
  ST *st;

  tcon = Host_To_Targ_String ( MTYPE_STRING,
                               string,
                               strlen(string));
  ty = Be_Type_Tbl(MTYPE_I1);
  st = Gen_String_Sym ( &tcon, ty, FALSE );

  return (st);
}
/*===============================================
 *
 * fei_critical_open_mp 
 * 
 * 
 *===============================================
*/ 
extern void 
fei_critical_open_mp        ( char *name )
{
  WN *wn;
  ST *st = NULL;

  if (name != NULL) 
    st = cwh_create_str_st(name);

  cwh_stmt_add_pragma(WN_PRAGMA_CRITICAL_SECTION_BEGIN, TRUE, st);

  wn = WN_CreateBarrier( FALSE, 0 );
  WN_set_pragma_omp(wn);
  cwh_block_append(wn);

  cwh_directive_set_PU_flags(FALSE);
}

/*===============================================
 *
 * fei_endcritical_open_mp 
 * 
 *===============================================
*/ 
extern void 
fei_endcritical_open_mp     ( char *name )
{
  WN *wn;
  ST *st;

  wn = WN_CreateBarrier( TRUE, 0 );
  WN_set_pragma_omp(wn);
  cwh_block_append(wn);

  st = NULL;

  if (name != NULL) 
    st = cwh_create_str_st(name);

  cwh_stmt_add_pragma(WN_PRAGMA_CRITICAL_SECTION_END, TRUE,st);
}

/*===============================================
 *
 * fei_barrier_open_mp 
 * 
 *===============================================
*/ 
extern void 
fei_barrier_open_mp         ( void )
{
  WN *wn;

  wn = WN_CreatePragma(WN_PRAGMA_BARRIER,(ST_IDX) NULL,0,0);
  WN_set_pragma_omp(wn);
  cwh_directive_barrier_insert(wn, 0);
}

/*===============================================
 *
 * fei_section_open_mp
 *
 * handles a C$OMP SECTION
 *
 * simply generates a WN_PRAGMA_SECTION pragma
 *
 *===============================================
 */
extern void
fei_section_open_mp         ( void )
{
  cwh_stmt_add_pragma(WN_PRAGMA_SECTION,TRUE);
} 

/*===============================================
 *
 * fei_master_open_mp 
 * 
 * 
 *===============================================
*/ 
extern void 
fei_master_open_mp          ( void )
{
  WN *body;

  body = cwh_mp_region(WN_PRAGMA_MASTER_BEGIN,0,0,0,0,0,1);

  /* Set current block to region body */
  cwh_block_set_current(body);

  /* set the MP and uplevel bits on the symtab */

  cwh_directive_set_PU_flags(FALSE);
}

/*===============================================
 *
 * fei_endmaster_open_mp 
 * 
 * 
 *===============================================
*/ 
extern void 
fei_endmaster_open_mp       ( void )
{
   cwh_directive_pop_and_nowait(FALSE,TRUE);
}

/*===============================================
 *
 * fei_ordered_open_mp 
 * 
 * 
 *===============================================
*/
extern void 
fei_ordered_open_mp         ( void )
{
  WN *wn;

  cwh_stmt_add_pragma(WN_PRAGMA_ORDERED_BEGIN,TRUE);

  wn = WN_CreateBarrier( FALSE, 0 );
  WN_set_pragma_omp(wn);
  cwh_block_append(wn);
}

/*===============================================
 *
 * fei_endsingle_open_mp 
 * 
 * similar to ending any other parallel construct
 * 
 *===============================================
*/ 
extern void 
fei_endsingle_open_mp       ( int nowait )
{
  WN *region, *wn ;
  WN * pragma_blk ;
  WN * old_blk ;
  WN_PRAGMA_ID  p ;

  region = cwh_block_pop_region();
  
  pragma_blk = WN_region_pragmas(region);
  old_blk = cwh_block_exchange_current(pragma_blk);

  while (task_var_count) {
    wn = cwh_stk_pop_WN();

    if (WN_operator(wn) == OPR_PRAGMA)
      cwh_block_append(wn);
    task_var_count--;
  }
  
  if (nowait) 
    p = WN_PRAGMA_NOWAIT;
  else 
    p = WN_PRAGMA_END_MARKER;

  wn = WN_CreatePragma (p, (ST_IDX) NULL, 0, 0);

  WN_set_pragma_omp(wn);
  cwh_block_append(wn);

  cwh_block_set_current(old_blk);
}

/*===============================================
 *
 * fei_enddo_open_mp 
 * 
 * similar to ending any other parallel loop, but we may need to add
 * a NOWAIT pragma to the region pragma list.
 * 
 *===============================================
*/ 
extern void
fei_enddo_open_mp           ( int nowait )
{
  cwh_directive_pop_and_nowait(nowait,TRUE);
}

/*===============================================
 *
 * fei_endsections_open_mp 
 * 
 * similar to ending any other parallel loop, but we may need to add
 * a NOWAIT pragma to the region pragma list.
 * 
 *===============================================
*/ 
extern void 
fei_endsections_open_mp     ( int nowait )
{
  cwh_directive_pop_and_nowait(nowait,TRUE);
}

/*===============================================
 *
 * fei_endordered_open_mp 
 * 
 * 
 *===============================================
*/ 
extern void 
fei_endordered_open_mp      ( void )
{
  WN *wn;

  wn = WN_CreateBarrier( TRUE, 0 );
  WN_set_pragma_omp(wn);
  cwh_block_append(wn);

  cwh_stmt_add_pragma(WN_PRAGMA_ORDERED_END,TRUE);
}

/*===============================================
 *
 * fei_endparalleldo_open_mp 
 * 
 * 
 *===============================================
*/ 
extern void 
fei_endparalleldo_open_mp   ( void )
{
  (void) cwh_block_pop_region();
}

/*===============================================
 *
 * fei_endparallel_open_mp 
 *
 * processes a C$OMP END PARALLEL
 *
 * simply pops off the MP region
 *
 *===============================================
 */
extern void
fei_endparallel_open_mp     ( void )
{
  cwh_directive_pop_and_nowait(FALSE,TRUE);
}

/*===============================================
 *
 * fei_endparallelsections_open_mp 
 * 
 * 
 *===============================================
*/ 
extern void 
fei_endparallelsections_open_mp( void )
{
  cwh_directive_pop_and_nowait(FALSE,TRUE);
} 

/*===============================================
 *
 * fei_flush_open_mp 
 * 
 * 
 *===============================================
*/ 
extern void 
fei_flush_open_mp(int list_count)
{
WN *sync;

  sync = WN_Create_Intrinsic(OPC_VINTRINSIC_CALL,INTRN_SYNCHRONIZE,0,NULL);
  cwh_directive_barrier_insert(sync,list_count) ;
}
/*===============================================
 *
 * fei_atomic_open_mp 
 * 
 * 
 *===============================================
*/ 
extern void 
fei_atomic_open_mp(void)
{
  cwh_stmt_add_pragma(WN_PRAGMA_ATOMIC,TRUE);
  cwh_directive_set_PU_flags(FALSE);
}

/*
 *================================================================
 * 
 * cwh_directive_add_do_loop_directive
 * 
 * Adds directive to the list of directives to be
 * inserted just before the next DO loop
 *
 *================================================================
 */
extern void
cwh_directive_add_do_loop_directive(WN *directive) 
{
   if (!do_loop_directive_block) {
      do_loop_directive_block = WN_CreateBlock();
   }
   WN_INSERT_BlockLast(do_loop_directive_block,directive);
}

/*
 *================================================================
 * 
 * cwh_directive_insert_do_loop_directives
 * 
 * Inserts all the deferred DO loop directives. 
 *
 *================================================================
 */

extern void
cwh_directive_insert_do_loop_directives(void) 
{
   if (do_loop_directive_block) {
      cwh_block_append(do_loop_directive_block);
      do_loop_directive_block = NULL;
   }
}

/*================================================================
 *
 * cwh_directive_barrier_insert
 * 
 * Insert a pragma eg: a barrier, and add to the 
 * current block. The pragma ins, if any, is inserted 
 * between forward and backward barriers.
 * 
 * args is the number of STs (names) on the stack
 * to be added to the barriers. TOS is last kid..
 *
 *================================================================
 */
extern void
cwh_directive_barrier_insert(WN *ins, int  args)
{
  WN *wn1;
  WN *wn2;
  ST *st;

  wn1 = WN_CreateBarrier(TRUE, args);
  wn2 = WN_CreateBarrier(FALSE, args);
  if (args) {
     while(args--) { 

        st = cwh_stk_pop_ST();

        /* CAUTION: use of 'args' below is code trick to reverse
            the operands popped off the stack.  Be careful if you hack with
            the code here */

        if (Barrier_Lvalues_On) {
           
           WN_kid(wn1,args) = cwh_addr_address_ST(st, 0, 0);
           WN_kid(wn2,args) = cwh_addr_address_ST(st, 0, 0);
        }
        else {
           WN_kid(wn1,args) = WN_CreateIdname(0,st);
           WN_kid(wn2,args) = WN_CreateIdname(0,st);
        }
     }
  }
  cwh_block_append(wn1);
  if (ins != NULL) 
    cwh_block_append(ins);
  cwh_block_append(wn2);
}

/*================================================================
 *
 * cwh_directive_pragma_to_region
 * 
 * Add given pragma to the given region. The
 * region need not be the current block.
 * 
 *================================================================
*/
static void
cwh_directive_pragma_to_region(WN * prag, WN * region) 
{
  WN * pr_blk ;
  WN * ol_blk ;

  pr_blk = WN_region_pragmas(region);
  ol_blk = cwh_block_exchange_current(pr_blk);
  cwh_block_append(prag);

  cwh_block_set_current(ol_blk);
}

/*================================================================
 *
 * cwh_directive_set_PU_flags
 * 
 * Set MP & uplevel flags on current PU.
 * 
 * If nested, set needs LNO
 * 
 *================================================================
*/
// Bug 3836
#ifdef KEY
extern void 
cwh_directive_set_PU_flags(BOOL nested)
#else
static void 
cwh_directive_set_PU_flags(BOOL nested)
#endif
{
  Set_PU_has_mp (Get_Current_PU ());
  Set_FILE_INFO_has_mp (File_info);
  Set_PU_uplevel (Get_Current_PU ());

  if (nested) 
    cwh_directive_set_LNO_flags() ;
}

/*================================================================
 *
 * cwh_directive_set_LNO_flags
 * 
 * Sets needs LNO on PU & file
 * 
 *================================================================
*/
static void
cwh_directive_set_LNO_flags(void)
{
  Set_PU_mp_needs_lno (Get_Current_PU());
  Set_FILE_INFO_needs_lno (File_info);
}

/*================================================================
 *
 * cwh_directive_pop_and_nowait
 * 
 * Pop the current region and issue a NOWAIT to the region
 * pragmas if required. If NOWAIT absent, issue an ENDMARKER,
 * to mark the line number at the end of the region.
 *
 *================================================================
*/
static void
cwh_directive_pop_and_nowait( BOOL nowait, BOOL is_omp)
{
  WN *region, *wn ;
  WN_PRAGMA_ID  p ;

  region = cwh_block_pop_region();

  if (nowait) 
    p = WN_PRAGMA_NOWAIT;
  else 
    p = WN_PRAGMA_END_MARKER;

  wn = WN_CreatePragma (p, (ST_IDX) NULL, 0, 0);

  if (is_omp)
    WN_set_pragma_omp(wn);

  cwh_directive_pragma_to_region(wn,region);
}

/*================================================================
 *
 * cwh_directive_add_to_loop
 * 
 * Adds the given WN to the Top_of_Loop block. (to last)
 * 
 *================================================================
*/
static void
cwh_directive_add_pragma_to_loop(WN * wn, BOOL is_omp)
{
  if (is_omp) 
      WN_set_pragma_omp(wn);
  WN_Set_Linenum (wn, USRCPOS_srcpos(current_srcpos));    
  cwh_block_append_given_id(wn,Top_of_Loop_Block,FALSE);
}

/*================================================================
 *
 * cwh_directive_work_dist
 * 
 * Utility routine for work distribution in fei_doparallel
 * and fei_doall
 * 
 *================================================================
*/
static void
cwh_directive_work_dist(INT32 work_dist, INTPTR work_dist_opnd)
{
  WN * wn1 ;
  STB_pkt *p;

  switch(work_dist) {
  case 0:         /* default: no need to do anything */
    break;

  case 1: /* single */
    cwh_stmt_add_pragma(WN_PRAGMA_MPSCHEDTYPE,FALSE,(ST_IDX) NULL, WN_PRAGMA_SCHEDTYPE_DYNAMIC);
    wn1 = WN_CreateIntconst ( OPC_I4INTCONST, 1);
    cwh_stmt_add_xpragma(WN_PRAGMA_CHUNKSIZE,FALSE,wn1);
    break;

  case 2: /* vector, not accepted, ignore */
    break;

  case 3: /* guided */
    cwh_stmt_add_pragma(WN_PRAGMA_MPSCHEDTYPE, FALSE,(ST_IDX) NULL, WN_PRAGMA_SCHEDTYPE_GSS);
    break;

  case 4: /* numchunks */
    DevAssert((0), ("Unimplemented numchunks"));
    break;

  case 5: /* chunksize */
    DevAssert((work_dist_opnd!=0),("Expected chunk value"));
    cwh_directive_load_value_pragma(work_dist_opnd,WN_PRAGMA_CHUNKSIZE);
    break;

  default:
    DevAssert((0), ("Unknown work distr"));
    break;
  }
}

/*================================================================
 *
 * cwh_directive_load_value_pragma
 * 
 * Utility routine for maxcpus, if_expr etc. Given an item
 * packet, extracts the ST, loads the value and puts a pragma
 * with the load into the current block.
 *
 * Used in parallel region, doall etc.
 *
 *================================================================
*/
static void
cwh_directive_load_value_pragma(INTPTR item, WN_PRAGMA_ID pragma, BOOL is_omp)
{
  WN * wn1 ;
  STB_pkt *p;

  if (item) {

    p = cast_to_STB(item);
    DevAssert((p->form == is_ST),("Odd item"));

    wn1 = cwh_addr_load_ST((ST *)p->item, 0, 0);
    cwh_stmt_add_xpragma(pragma,is_omp,wn1);
  }
}

/*================================================================
 *
 * fei_copyin_bound
 *
 * Generates the COPYIN_BOUND xpragma and appends it to the
 * current block.
 *
 * Used for bounds temps that are flow dependent.
 *
 *================================================================
*/

extern void
fei_copyin_bound(INTPTR sym_idx)
{
  STB_pkt *p;
  WN *wn;
  ST *st;

  p = cast_to_STB(sym_idx);
  DevAssert((p->form == is_ST),("Odd object ref"));

  st = cast_to_ST(p->item);

  if (ST_sym_class(st) == CLASS_VAR &&
      !ST_auxst_xpragma_copyin(st)) {

    wn = WN_CreateXpragma ( WN_PRAGMA_COPYIN_BOUND, (ST_IDX) NULL, 1 );
    WN_kid0(wn) = cwh_addr_load_ST(st,0,0);
    cwh_block_append(wn);
    Set_ST_auxst_xpragma_copyin(st,TRUE);
  }
}
