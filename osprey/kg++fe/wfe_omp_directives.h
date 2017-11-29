/*
 * Copyright 2005, 2006 PathScale, Inc.  All Rights Reserved.
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of version 2 of the GNU General Public License as
 * published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it would be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 *
 * Further, this software is distributed without any warranty that it is
 * free of the rightful claim of any third person regarding infringement
 * or the like.  Any license provided herein, whether implied or
 * otherwise, applies only to this software file.  Patent licenses, if
 * any, provided herein do not apply to combinations of this program with
 * other software, or any other product whatsoever.
 *
 * You should have received a copy of the GNU General Public License along
 * with this program; if not, write the Free Software Foundation, Inc., 59
 * Temple Place - Suite 330, Boston MA 02111-1307, USA.
 */

#ifndef _GCCFE_WFE_OMP_DIRECIVES_H_
#define _GCCFE_WFE_OMP_DIRECIVES_H_

// Contains a TREE var whose ST has been computed. We keep track of the
// TREE here so we can access the ST in it, as well as all the information
// GNU has put into it.
struct ST_list {
    tree var;
    ST_list *next;
} ;

struct WN_list {
    WN *wn;
    WN_list *next;
} ;

struct schedule_2_wn
{
   enum schedule_kind_type  schedule_2_kind;
   WN *chunk_size_wn;
};

struct Parallel_clause_wn_type
{
   WN *if_clause;
   WN *num_threads_clause;
   ST_list *private_clause;
   ST_list *firstprivate_clause;
   ST_list *shared_clause;
   ST_list *copyin_clause;
   WN_list *reduction_clause;
   enum default_type default_clause;   // needs to specify how to value 
}  ;


 
extern void 
WFE_expand_start_parallel (struct Parallel_clause_wn_type * parallel_clause_wn);

extern void 
WFE_expand_end_parallel ();

struct For_clause_wn_type
{
   ST_list *private_clause;
   ST_list *firstprivate_clause;
   ST_list *lastprivate_clause;
   WN_list *reduction_clause;
   bool ordered_clause;
   enum schedule_kind_type  schedule_1_clause;
   struct schedule_2_wn  schedule_2_clause;  
   bool nowait_clause;
};

extern void 
WFE_expand_start_for (struct For_clause_wn_type * for_clause_wn);

extern void 
WFE_expand_end_for( );

struct Sections_clause_wn_type
{
   ST_list *private_clause;
   ST_list *firstprivate_clause;
   ST_list *lastprivate_clause;
   WN_list *reduction_clause;
   bool nowait_clause;
};

extern void 
WFE_expand_start_sections (struct Sections_clause_wn_type * sections_clause_wn);

extern void 
WFE_expand_end_sections( );

extern void 
WFE_expand_start_section ();

extern void 
WFE_expand_end_section ( );

#ifdef TARG_SL2 
extern void 
WFE_expand_start_sl2_sections (BOOL is_minor_thread);

extern void 
WFE_expand_end_sl2_sections( );

extern void 
WFE_expand_start_sl2_section (BOOL is_minor_thread);

extern void 
WFE_expand_end_sl2_section ();
#endif 



struct Single_clause_wn_type
{
   ST_list *private_clause;
   ST_list *firstprivate_clause;
   ST_list *copyprivate_clause;
   bool nowait_clause;
};

extern void 
WFE_expand_start_single (struct Single_clause_wn_type * single_clause_wn);

extern void 
WFE_expand_end_single( );

struct Parallel_for_clause_wn_type
{
   WN *if_clause;
   WN *num_threads_clause;
   ST_list *private_clause;
   ST_list *firstprivate_clause;
   ST_list *lastprivate_clause;
   ST_list *shared_clause;
   ST_list *copyin_clause;
   WN_list *reduction_clause;
   enum default_type default_clause;   // needs to specify how to value 
   bool ordered_clause;
   enum schedule_kind_type schedule_1_clause;
   struct schedule_2_wn  schedule_2_clause;  
};

extern void WFE_expand_start_parallel_for
 (struct Parallel_for_clause_wn_type * parallel_for_clause_wn);

extern void WFE_expand_end_parallel_for ();

struct Parallel_sections_clause_wn_type
{
   WN *if_clause;
   WN *num_threads_clause;
   ST_list *private_clause;
   ST_list *firstprivate_clause;
   ST_list *lastprivate_clause;
   ST_list *shared_clause;
   ST_list *copyin_clause;
   WN_list *reduction_clause;
   enum default_type default_clause;   // needs to specify how to value 
}  ;

extern void WFE_expand_start_parallel_sections 
(struct Parallel_sections_clause_wn_type * parallel_sections_clause_wn);

extern void WFE_expand_end_parallel_sections ();

extern void WFE_expand_start_master ( );

extern void WFE_expand_end_master ();

extern void  WFE_expand_start_critical (ST *region_phrase,char* name);

extern void  WFE_expand_end_critical ( );

extern void  WFE_expand_start_atomic ();

extern void WFE_expand_end_atomic ();

extern void  WFE_expand_start_ordered ( );

extern void  WFE_expand_end_ordered ( );

extern void  WFE_expand_barrier ( );

extern void  WFE_expand_flush (WN_list *flush_variables);

extern void WFE_expand_start_do_loop (WN *, WN *, WN *, WN *);

extern void WFE_expand_end_do_loop (void);

extern BOOL Trace_Omp;
#endif
