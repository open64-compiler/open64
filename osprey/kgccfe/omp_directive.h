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

extern void expand_start_parallel (struct parallel_clause_list * clause_list);
extern void expand_end_parallel ( );

extern struct parallel_clause_list * chain_parallel_list_on 
	(struct parallel_clause_list * pclause_list, struct parallel_clause_list * pclause);

extern struct parallel_clause_list * 
	build_parallel_clause_list (tree,
	                            enum parallel_clause_type,
				    enum default_type,
				    enum reduction_op_type);

///////////////////////////////////////////////////////////////////////////////////////////////////////

extern void expand_start_for (struct for_clause_list * clause_list);
extern void expand_end_for ();

extern struct for_clause_list * chain_for_list_on 
	(struct for_clause_list * fclause_list, struct for_clause_list * fclause);

extern struct for_clause_list * 
	build_for_clause_list (tree, enum for_clause_type,
	                       enum schedule_kind_type,
			       enum reduction_op_type);

/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
extern void expand_start_sections (struct sections_clause_list * clause_list);
extern void expand_end_sections ();

extern struct sections_clause_list * chain_sections_list_on 
	(struct sections_clause_list * sclause_list, struct sections_clause_list * sclause);

extern struct sections_clause_list * 
	build_sections_clause_list (tree,
	                               enum sections_clause_type,
				       enum reduction_op_type);
extern void expand_start_section ();
extern void expand_end_section ();

/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
extern void expand_start_single (struct single_clause_list * clause_list);
extern void expand_end_single ();

extern struct single_clause_list * chain_single_list_on
	(struct single_clause_list * sclause_list, struct single_clause_list * sclause);

extern struct single_clause_list * 
	build_single_clause_list (tree, enum single_clause_type);

/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

extern void expand_start_parallel_for (struct parallel_for_clause_list * clause_list);
extern void expand_end_parallel_for ( );
extern struct parallel_for_clause_list * chain_parallel_for_list_on 
	(struct parallel_for_clause_list * fclause_list,struct parallel_for_clause_list * fclause);

extern struct parallel_for_clause_list *
	build_parallel_for_clause_list (tree, enum parallel_for_clause_type,
	                                enum default_type,
					enum schedule_kind_type,
					enum reduction_op_type);

/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

extern void expand_start_parallel_sections (struct parallel_sections_clause_list * clause_list);
extern void expand_end_parallel_sections ();

extern struct parallel_sections_clause_list * chain_parallel_sections_list_on
	(struct parallel_sections_clause_list * sclause_list, struct parallel_sections_clause_list * sclause);

extern struct parallel_sections_clause_list * 
	build_parallel_sections_clause_list (tree,
	                                     enum parallel_sections_clause_type,
                                             enum default_type,
					     enum reduction_op_type);

/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
extern void expand_start_master ();
extern void expand_end_master ();

/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

extern void expand_start_critical (tree region_phrase);
extern void expand_end_critical ( );

/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
extern void expand_barrier ();

/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
extern void  expand_start_atomic ();  
extern void  expand_end_atomic ();   
extern void  check_atomic_expression ( tree atomic_expression );

/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
extern void expand_flush (tree flush_v_list);

/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
extern void expand_threadprivate (tree threadprivate_v_list);

/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
extern void expand_start_ordered ();
extern void expand_end_ordered ();

/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
extern int check_do_loop_for(tree init_expr, tree logical_expr, tree incr_expr);
extern void expand_start_do_loop (tree, tree, tree, struct nesting *);
extern void expand_end_do_loop(struct nesting *);



#ifdef TARG_SL2 //fork_joint
extern void expand_start_sl2_sections (bool is_minor_thread);
extern void expand_end_sl2_sections ();
extern void expand_start_sl2_section (bool is_minor_thread);
extern void expand_end_sl2_section ();
#endif 

/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

#ifdef _LANGUAGE_C_PLUS_PLUS
}
#endif

#endif

