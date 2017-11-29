/*
 * Copyright (C) 2007. QLogic Corporation. All Rights Reserved.
 */
/*
 * Copyright 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
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


#ifndef _GCCFE_OMP_DIRECIVES_H_
#define _GCCFE_OMP_DIRECIVES_H_

#ifdef _LANGUAGE_C_PLUS_PLUS
extern "C"{
#endif

extern void expand_start_parallel_or_combined_parallel (gs_t);

extern void expand_start_parallel (gs_t);
extern void expand_end_parallel ( );


///////////////////////////////////////////////////////////////////////////////////////////////////////

extern void expand_start_for (gs_t stmt);
extern void expand_end_for (void);

/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
extern void expand_start_sections (gs_t stmt);
extern void expand_end_sections (void);

extern void expand_start_section (gs_t stmt);
extern void expand_end_section (void);

/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
extern void expand_start_single (gs_t stmt);
extern void expand_end_single (void);

/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

extern void expand_start_parallel_for (gs_t stmt);
extern void expand_end_parallel_for (void);

/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

extern void expand_start_parallel_sections (gs_t stmt);
extern void expand_end_parallel_sections (void);

/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
extern void expand_start_master (gs_t stmt);
extern void expand_end_master (void);

/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

extern void expand_start_critical (gs_t region_phrase);
extern void expand_end_critical ( );

/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
extern void expand_barrier ();

/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
extern void  expand_start_atomic (gs_t stmt);  
extern void  expand_end_atomic (void);   
extern void  check_atomic_expression ( gs_t atomic_expression );

/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
extern void expand_flush (gs_t flush_v_list);

/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
extern void expand_start_ordered (gs_t stmt);
extern void expand_end_ordered (void);

/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
extern int check_do_loop_for(gs_t init_expr, gs_t logical_expr, gs_t incr_expr);
extern void expand_start_do_loop (gs_t, gs_t, gs_t);
extern void expand_end_do_loop(void);

/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

#ifdef _LANGUAGE_C_PLUS_PLUS
}
#endif

#endif

