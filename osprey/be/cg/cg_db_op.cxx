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
 *  Module: cg_db_op.c
 *  $Revision: 1.2 $
 *  $Date: 02/11/07 23:41:20-00:00 $
 *  $Author: fchow@keyresearch.com $
 *  $Source: /scratch/mee/2.4-65/kpro64-pending/be/cg/SCCS/s.cg_db_op.cxx $
 *
 *  Revision comments:
 *
 *  28-Jun-1995 - Initial version
 *
 *  Description:
 *  ============
 *
 * =======================================================================
 * ======================================================================= */

#ifdef _KEEP_RCS_ID
static char *source_file = __FILE__;
static char *rcs_id = "";
#endif /* _KEEP_RCS_ID */

#include "defs.h"
#include "resource.h"
#include "errors.h"
#include "tracing.h"
#include "mempool.h"
#include "bb.h"
#include "op.h"
#include "op_map.h"
#include "cg_cflow.h"
#include "cg_db_op.h"
#include "cg_flags.h"
#include "tn_map.h"
#include "cg_loop.h"

OP_MAP DB_AUX_OP_Map = NULL;

void DB_Initialize(void)

{
  if ( !DB_AUX_OP_Map ) 
    DB_AUX_OP_Map = OP_MAP_Create();
}

void DB_Initialize_OP( OP *op, INT16 dbnum ) 
{
  
  Is_True(DB_AUX_OP_Map , ("DB_AUX_OP_map not allocated"));
  Is_True(OP_MAP_Get(DB_AUX_OP_Map, op) == NULL, ("DB_AUX_OP already allocated"));
  OP_MAP_Set(DB_AUX_OP_Map, op, (void *)TYPE_P_ALLOC(DB_AUX_OP));
  Set_OPDB_flags(op,0);
  Set_OP_dbnum(op,dbnum);
  Set_OP_link(op,NULL);
  Set_OP_condtn(op,OP_guard(op));
  if ( OP_invguard(op) ) 
    Set_OP_invcondtn(op);
 
}



BOOL Is_DB_OP_Init(OP *op) 
{
  if ( !DB_AUX_OP_Map ) 
    return FALSE;

  if ( !OP_MAP_Get(DB_AUX_OP_Map, op ) ) 
    return FALSE;

  return TRUE;
}


void DB_Copy_Aux_OP(OP *dest, OP *src)
{

  if ( !Is_DB_OP_Init(src) ) return;
  if ( !Is_DB_OP_Init(dest) ) DB_Initialize_OP(dest,OP_dbnum(src));
  
  Set_OPDB_flags(dest,OPDB_flags(src));
  Set_OP_dbnum(dest,OP_dbnum(src));
  Set_OP_link(dest,OP_link(src));
  Set_OP_condtn(dest,OP_condtn(src));

}


void DB_Trace_BB(BB *bb)
{
  OP *op;

  if ( !CG_enable_reverse_if_conversion ||
      Original_DB_count < 2 ) return;


  fprintf ( TFile, "\n<cgprep> ****************************************");
  fprintf(TFile, " if converted BB:%d \n",BB_id(bb));
  FOR_ALL_BB_OPs(bb, op) {
    if ( !Is_DB_OP_Init(op) ) {
      fprintf(TFile,"  DB:?? \n");
      fprintf(TFile,"      ");
      Print_OP_No_SrcLine(op);
    } else {
      fprintf(TFile,"  DB:%d -> BB:%d, "
	      ,OP_dbnum(op), BB_id(DoBodyBB(OP_dbnum(op))));
      if (DoBodyICR(OP_dbnum(op)) ) {
	fprintf ( TFile, " %s<ICR:",DBnegateicr(OP_dbnum(op)) ? "!" : "");
	Print_TN ( DoBodyICR(OP_dbnum(op)), FALSE );
	if (DoBodyICRomega(OP_dbnum(op))) 
	  fprintf(TFile, "[%d]", DoBodyICRomega(OP_dbnum(op)));
	fprintf(TFile, ">");
      }
      if (DoBodyBRCC(OP_dbnum(op)) ) {
	if ( DBbrccisf(OP_dbnum(op)) ) {
	  fprintf ( TFile, " <%s",DoBodyBRCCsense(OP_dbnum(op)) ? "bc1f " : "bc1t ");
	} else {
	  fprintf ( TFile, " <%s",DoBodyBRCCsense(OP_dbnum(op)) ? "beq " : "bne ");
	}
	Print_TN ( DoBodyBRCC(OP_dbnum(op)), FALSE );
	fprintf(TFile, ">");
      }

      if ( OP_condtn(op) ) {
	fprintf ( TFile, " %s<CTN:",OP_invcondtn(op) ? "!" : "");
	Print_TN ( OP_condtn(op), FALSE );
	fprintf(TFile, ">");
      }
      if ( OP_cgselect(op) ) 
	fprintf ( TFile, " <ifc select> ");
      if ( OP_CFCCOP(op) ) 
	fprintf ( TFile, " <CFCCOP> ");

      fprintf(TFile, "\n");
      fprintf(TFile,"      ");
      Print_OP_No_SrcLine(op);
    }
  }
  fprintf ( TFile, "<cgprep> ****************************************\n\n");

}

static MEM_POOL rif_pool;
static hTN_MAP rif_tn_map;
static BOOL is_rif_map_init = FALSE;
static BB_MAP rif_bb_map_ICR;
static BB_MAP rif_bb_map_BRCC;

void DB_Rename_Cond_TN_Init(BB *bb )
{
  TN **db_rename_bb_ICR;
  TN **db_rename_bb_BRCC;
  static ST *last_PU = NULL;

  if (last_PU != Get_Current_PU_ST()) {
    is_rif_map_init = FALSE;
    last_PU = Get_Current_PU_ST();
  }

  if ( !(Original_DB_count > 1 && BB_loophead(bb)) ) return;
  if (CG_enable_reverse_if_conversion == FALSE) return;

  if ( !is_rif_map_init ) {
    MEM_POOL_Initialize(&rif_pool,"reverse_if_conversion_pool",TRUE);
    rif_bb_map_ICR  = BB_MAP_Create();
    rif_bb_map_BRCC = BB_MAP_Create();
  }

  is_rif_map_init = TRUE;
  if ( !BB_MAP_Get(rif_bb_map_ICR, bb) ) {
    INT16 dbnum;
    UINT8 ntimes = BB_unrollings(bb);
    UINT8 unrolling;
    INT16 offset;
    TN *unroll_tn;
    ntimes = ntimes > 0 ? ntimes : 1;
    offset = ntimes * Original_DB_count;
    db_rename_bb_ICR  = TYPE_MEM_POOL_ALLOC_N(TN *, &rif_pool, offset);
    db_rename_bb_BRCC = TYPE_MEM_POOL_ALLOC_N(TN *, &rif_pool, offset);
    BB_MAP_Set(rif_bb_map_ICR, bb, db_rename_bb_ICR);
    BB_MAP_Set(rif_bb_map_BRCC, bb, db_rename_bb_BRCC);

    for (dbnum = 1; dbnum <= Original_DB_count; dbnum++ ) {
      for ( unrolling = 0 ; unrolling < ntimes ; unrolling++ ) {
	db_rename_bb_ICR[dbnum-1+unrolling*Original_DB_count] = (ntimes > 1 ) ? 
	  CG_LOOP_unroll_names_get(DoBodyICR(dbnum), unrolling) :
	    DoBodyICR(dbnum);
	unroll_tn = (ntimes > 1 ) ? 
	  CG_LOOP_unroll_names_get(DoBodyBRCC(dbnum), unrolling) :
	    DoBodyBRCC(dbnum);
	db_rename_bb_BRCC[dbnum-1+unrolling*Original_DB_count] = unroll_tn;
      }
    }
  }
  MEM_POOL_Push(&rif_pool);
  rif_tn_map = hTN_MAP_Create(&rif_pool);
}

void DB_Rename_Cond_TN(BB *bb, TN *dest, TN *src, UINT8 omega)
{
  if (CG_enable_reverse_if_conversion == FALSE) return;

  if ( !TN_is_if_conv_cond(dest) ) return;

  if ( !(Original_DB_count > 1 && BB_loophead(bb)) ) return;

  if ( !is_rif_map_init ) return;
  hTN_MAP_Set(rif_tn_map, dest, src);

  Set_TN_is_if_conv_cond(src);
  Reset_TN_is_if_conv_cond(dest);

}

inline TN *find_last_cond_tn (BB *bb, TN *tn ) 
{
  TN *new_tn;
  TN *last_tn;
  
  if ( !tn ) return tn;

  for ( new_tn = tn; new_tn; new_tn = (TN *)hTN_MAP_Get(rif_tn_map, last_tn) ) {
    last_tn = new_tn;
  }

  return (tn == last_tn) ? NULL : last_tn;

}

void DB_Rename_Cond_TN_Finish( void ) 
{
  if ( !is_rif_map_init ) return;
  BB_MAP_Delete(rif_bb_map_ICR);
  BB_MAP_Delete(rif_bb_map_BRCC);
  MEM_POOL_Delete(&rif_pool);
  is_rif_map_init = FALSE;
  Original_DB_count = 1;
}

inline TN *rif_cond_tn_get_ICR(BB *bb, INT16 dbnum, UINT8 unrolling)
{
  INT16 offset = unrolling * Original_DB_count;
  TN **entry = (TN **)BB_MAP_Get(rif_bb_map_ICR, bb);
  return entry ? entry[dbnum-1+offset] : NULL;
}

inline void rif_cond_tn_set_ICR(BB *bb, INT16 dbnum, TN *tn, UINT8 unrolling)
{
  INT16 offset = unrolling * Original_DB_count;
  TN **entry = (TN **)BB_MAP_Get(rif_bb_map_ICR, bb);
  if ( !entry ) return;
  entry[dbnum-1+offset] = tn;
}

inline TN *rif_cond_tn_get_BRCC(BB *bb, INT16 dbnum, UINT8 unrolling)
{
  INT16 offset = unrolling * Original_DB_count;
  TN **entry = (TN **)BB_MAP_Get(rif_bb_map_BRCC, bb);
  return entry ? entry[dbnum-1+offset] : NULL;
}

inline void rif_cond_tn_set_BRCC(BB *bb, INT16 dbnum, TN *tn, UINT8 unrolling )
{
  INT16 offset = unrolling * Original_DB_count;
  TN **entry = (TN **)BB_MAP_Get(rif_bb_map_BRCC, bb);
  if ( !entry ) return;
  entry[dbnum-1+offset] = tn;
}

void DB_Rename_Cond_TN_OPs(BB *bb ) 
{
  OP *op;
  TN *new_tn = NULL;
  UINT8 ntimes = BB_unrollings(bb);
  UINT8 unrolling;
  ntimes = ntimes > 0 ? ntimes : 1;

  if ( !(Original_DB_count > 1 && BB_loophead(bb)) ) return;

  if ( !is_rif_map_init ) return;

  if ( BB_MAP_Get(rif_bb_map_ICR, bb) ) {
    INT16 dbnum;

    for ( dbnum = 1; dbnum <=  Original_DB_count; dbnum++ ) {
      for ( unrolling = 0 ; unrolling < ntimes ; unrolling++ ) {
	if (new_tn = find_last_cond_tn(bb,rif_cond_tn_get_ICR(bb,dbnum, unrolling)) ) {
	  Set_TN_is_if_conv_cond(new_tn);
	  rif_cond_tn_set_ICR(bb,dbnum,new_tn, unrolling);
	}
	if (new_tn = find_last_cond_tn(bb,rif_cond_tn_get_BRCC(bb,dbnum, unrolling)) ) {
	  Set_TN_is_if_conv_cond(new_tn);
	  rif_cond_tn_set_BRCC(bb,dbnum,new_tn, unrolling);
	}
      }
    }
  }

  FOR_ALL_BB_OPs(bb, op ) {
    if ( Is_DB_OP_Init(op) ) {
      if ( new_tn = find_last_cond_tn(bb,OP_condtn(op)) ) 
	Set_OP_condtn(op, new_tn);
    }
  }
  
  MEM_POOL_Pop(&rif_pool);
}

TN *DB_Get_ICR(BB *bb, INT16 dbnum, UINT8 unrolling)
{
  TN *tn;
  if ( !is_rif_map_init ) return DoBodyICR(dbnum);
  tn = rif_cond_tn_get_ICR(bb, dbnum,unrolling);
  return tn ? tn : DoBodyICR(dbnum);
}

TN *DB_Get_BRCC(BB *bb, INT16 dbnum, UINT8 unrolling)
{
  TN *tn;
  if ( !is_rif_map_init ) return DoBodyBRCC(dbnum);
  tn = rif_cond_tn_get_BRCC(bb, dbnum,unrolling);
  return tn ? tn : DoBodyBRCC(dbnum);
}






