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

#ifndef _GCCFE_OMP_TYPES_H_
#define _GCCFE_OMP_TYPES_H_


enum omp_tree_type
{
  parallel_dir_b,
  parallel_dir_e,
  for_dir_b,
  for_dir_e,
  sections_cons_b,
  sections_cons_e,
  section_cons_b,
  section_cons_e,
  single_cons_b,
  single_cons_e,
  par_for_cons_b,
  par_for_cons_e,
  par_sctn_cons_b,
  par_sctn_cons_e,
  master_cons_b,
  master_cons_e,
  critical_cons_b,
  critical_cons_e,
  barrier_dir,
  flush_dir,
  atomic_cons_b,
  atomic_cons_e,
  thdprv_dir,
  ordered_cons_b,
  ordered_cons_e,
  options_dir,
#ifdef TARG_SL //fork_joint   
  sl2_sections_cons_b,
  sl2_minor_sections_cons_b,
  sl2_sections_cons_e,
  sl2_section_cons_b,
  sl2_section_cons_e,   
  sl2_minor_section_cons_b,
  sl2_minor_section_cons_e,
#endif
  exec_freq_dir
};

///////////////////////////////////////////////////////////////////
enum reduction_op_type
{
	REDUCTION_OPR_BAND,//'&'
	REDUCTION_OPR_BIOR,//"|"
	REDUCTION_OPR_BXOR,//'^'
	REDUCTION_OPR_ADD,//'+'
	REDUCTION_OPR_MPY,//'*'
	REDUCTION_OPR_SUB,//'-'
	REDUCTION_OPR_CAND,//ANDAND
	REDUCTION_OPR_CIOR//OROR
};

struct reduction 
{
	enum reduction_op_type reduction_op;
	tree var_list;
};

enum default_type
{
	default_shared,
	default_none,
	no_default
};

enum parallel_clause_type
{
	p_if,
	p_num_threads,
	p_private,
	p_firstprivate,
	p_shared,
	p_default,
	p_reduction,
	p_copyin
};

union parallel_clause_node
{
	tree expr_no_commas;
	tree var_list;
	enum default_type defaulttype;
	struct reduction reduction_node;
};

struct parallel_clause_list
{
	enum parallel_clause_type type;
	union parallel_clause_node node;
	struct parallel_clause_list * next;
};

enum schedule_kind_type 
{
    SK_STATIC,
    SK_DYNAMIC,
    SK_GUIDED,
    SK_RUNTIME,
    SK_NONE
};

struct schedule_2 
{
	enum schedule_kind_type schedule_kind;
	tree chunk_size;
};

enum for_clause_type
{
	f_private,
	f_firstprivate,
	f_lastprivate,
	f_reduction,
	f_ordered,
	f_schedule_1,
	f_schedule_2,
	f_nowait
};

union for_clause_node
{
	tree var_list;
	tree expr_no_commas;
	struct reduction reduction_node;
	enum schedule_kind_type schedule_kind;
	struct schedule_2 schedule_node;
	int ordered_nowait;
};

struct for_clause_list
{
	enum for_clause_type type;
	union for_clause_node node;
	struct for_clause_list * next;
};

enum sections_clause_type
{
	sections_private,
	sections_firstprivate,
	sections_lastprivate,
	sections_reduction,
	sections_nowait
};

union sections_clause_node
{
	tree var_list;
	struct reduction reduction_node;
	int nowait;
};

struct sections_clause_list
{
	enum sections_clause_type type;
	union sections_clause_node node;
	struct sections_clause_list * next;
};

enum single_clause_type
{
	single_private,
	single_firstprivate,
	single_copyprivate,
	single_nowait
};

union single_clause_node
{
	tree var_list;
	int nowait;
};

struct single_clause_list
{
	enum single_clause_type type;
	union single_clause_node node;
	struct single_clause_list * next;
};

enum parallel_for_clause_type
{
	p_for_if,
	p_for_num_threads,
	p_for_private,
	p_for_copyprivate,
	p_for_firstprivate,
	p_for_lastprivate,
	p_for_shared,
	p_for_default,
	p_for_reduction,
	p_for_copyin,
	p_for_ordered,
	p_for_schedule_1,
	p_for_schedule_2
};

union parallel_for_clause_node
{
	tree expr_no_commas;
	tree var_list;
	enum default_type defaulttype;
	struct reduction reduction_node;
	enum schedule_kind_type schedule_kind;
	struct schedule_2 schedule_node;
	int ordered;
};

struct parallel_for_clause_list
{
	enum parallel_for_clause_type type;
	union parallel_for_clause_node node;
	struct parallel_for_clause_list * next;
};

enum parallel_sections_clause_type
{
	p_sections_if,
	p_sections_num_threads,
	p_sections_private,
	p_sections_copyprivate,
	p_sections_firstprivate,
	p_sections_lastprivate,
	p_sections_shared,
	p_sections_default,
	p_sections_reduction,
	p_sections_copyin
};

union parallel_sections_clause_node
{
	tree expr_no_commas;
	tree var_list;
	enum default_type defaulttype;
	struct reduction reduction_node;
};

struct parallel_sections_clause_list
{
	enum parallel_sections_clause_type type;
	union parallel_sections_clause_node node;
	struct parallel_sections_clause_list * next;
};
#endif
