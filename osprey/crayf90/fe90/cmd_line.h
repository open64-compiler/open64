/*
 * Copyright (C) 2010 Advanced Micro Devices, Inc.  All Rights Reserved.
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



/* USMID:  "\n@(#)5.0_pl/headers/cmd_line.h	5.6	09/27/99 15:43:51\n" */
 
extern char		       *orig_cmd_line;

extern boolean		enter_cmd_line_cc_define(char *, char *, boolean);
extern token_values_type	get_dir_token_from_str(char *);


/****************************************\
|* Static data used within cmd_line.c.  *|
\****************************************/
static boolean		accept_stream		= FALSE;

static boolean		set_debug_option	= FALSE;
static boolean		set_aggress_option	= FALSE;
static boolean		set_bottom_load_option	= FALSE;
static boolean		set_eu_option		= FALSE;
static boolean		set_ieeeconform_option	= FALSE;
static boolean		set_i_option		= FALSE;
static boolean		set_inlinefrom_option	= FALSE;
static boolean		set_loop_align_option	= FALSE;
static boolean		set_pattern_option	= FALSE;
static boolean		set_pipeline_option	= FALSE;
static boolean		set_recurrence_option	= FALSE;
static boolean		set_round_option	= FALSE;
static boolean		set_scalar_option	= FALSE;
static boolean		set_source_form_option	= FALSE;
static boolean		set_stream_option	= FALSE;
static boolean		set_support_lvl_option	= FALSE;
static boolean		set_task_option		= FALSE;
static boolean		set_taskinner_option	= FALSE;
static boolean		set_trunc_option	= FALSE;
static boolean		set_vector_option	= FALSE;
static boolean		set_vsearch_option	= FALSE;
static boolean		set_zeroinc_option	= FALSE;

# if defined(_ACCEPT_CMD_X)
static boolean		set_MPP_num_pes		= FALSE;
# endif

static boolean		no_preprocessing	= FALSE;


/******************************************************************\
*  The following are needed for validate_O_option.               *
\******************************************************************/

char		scalar_lvl_str[][8]	= {
			"scalar0",
			"scalar1",
			"scalar2",
			"scalar3"
                };

char		stream_lvl_str[][8]	= {
			"stream0",
			"stream1",
			"stream2",
			"stream3"
                };

char		task_lvl_str[][6]		= {
			"task0",
			"task1",
			"task2",
			"task3"
                };

char		vector_lvl_str[][8]	= {
			"vector0",
			"vector1",
			"vector2",
			"vector3"
                };


/******************************************************************************\
|*  Default option values for install tool segldr "set" commands.             *|
|*  This is to set default cmd line option values during the install.         *|
\******************************************************************************/

/* for the -e options, 0 => -d, 1 => -e  */
int	cft90_dash_e_a_option			= 0;	/* -da */
int	cft90_dash_e_i_option			= 0;	/* -di */
int	cft90_dash_e_n_option			= 0;	/* -dn */

int	cft90_dash_e_p_option			= 1;	/* -ep */
int	cft90_dash_e_r_option			= 1;	/* -er */
int	cft90_dash_e_v_option			= 0;	/* -dv */


        /* for -i option, value is 32 or 64 */

        /* eu is strong round (rounded integer divide) on non IEEE machines */
        /* du is reciprical divide on IEEE machines.                        */

# if defined(_TARGET_SV2)
int	cft90_dash_i_option			= 32;	/* -i 64 */
int	cft90_dash_e_m_option			= 1;	/* -dm */
int	cft90_dash_e_t_option			= 0;	/* -et */
int	cft90_dash_e_u_option			= 1;	/* -du */
int	cft90_dash_X_option			= 1;	/* -X1 */
int	cft90_dash_a_dalign_option		= 1;	/* -a dalign */
# elif defined(_TARGET_OS_UNICOS)
int	cft90_dash_i_option			= 64;	/* -i 64 */
int	cft90_dash_e_m_option			= 0;	/* -dm */
int	cft90_dash_e_t_option			= 1;	/* -et */
int	cft90_dash_e_u_option			= 0;	/* -du */
int	cft90_dash_X_option			= 1;	/* -X1 */
int	cft90_dash_a_dalign_option		= 0;	/* -a dalign */
# elif defined(_TARGET_OS_MAX)
int     cft90_dash_i_option                     = 64;   /* -i 64 */
int	cft90_dash_e_m_option			= 0;	/* -dm */
int	cft90_dash_e_t_option			= 0;	/* -dt */
int	cft90_dash_e_u_option			= 0;	/* -du */
int	cft90_dash_X_option			= 0;	/* -Xn */
int	cft90_dash_a_dalign_option		= 0;	/* -a dalign */
# elif (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
int	cft90_dash_i_option			= 32;	/* -i 32 */
int	cft90_dash_e_m_option			= 1;	/* -em */
int	cft90_dash_e_t_option			= 0;	/* -dt */
int	cft90_dash_e_u_option			= 1;	/* -eu */
int	cft90_dash_X_option			= 1;	/* -X1 */
int	cft90_dash_a_dalign_option		= 0;	/* -a dalign */
# else
int	cft90_dash_i_option			= 32;	/* -i 32 */
int	cft90_dash_e_m_option			= 0;	/* -dm */
int	cft90_dash_e_t_option			= 0;	/* -dt */
int	cft90_dash_e_u_option			= 1;	/* -eu */
int	cft90_dash_X_option			= 1;	/* -X1 */
int	cft90_dash_a_dalign_option		= 0;	/* -a dalign */
# endif

/* for -m, value is -m value .... 0,1,2,3,4 */
int	cft90_dash_m_option			= 3;	/* -m 3 */

/* for -N option, value is 72 or 80 */
int	cft90_dash_N_option			= 72;	/* -N 72 */

/* the next four ... levels 0,1,2,3 */

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
int	cft90_dash_O_bl_option			= 0;
int	cft90_dash_O_fastint_option		= 0;	/* -O fastint */
int	cft90_dash_O_fusion_option		= 1;	/* -O fusion  */
int	cft90_dash_O_ieeeconform_option		= 0;
int	cft90_dash_O_jump_option		= 0;
int	cft90_dash_O_loopalign_option		= 0;
int	cft90_dash_O_scalar_option		= 1;	/* -O scalar1 */
int	cft90_dash_O_split_option		= 0;	/* -O split */
int	cft90_dash_O_support_option		= 1;	/* -O 1 */
int	cft90_dash_O_task_option		= 0;	/* -O task0 */
int	cft90_dash_O_taskinner_option		= 1;	/* -O taskinner */
int	cft90_dash_O_threshold_option		= 1;	/* -O threshold */
int	cft90_dash_O_unroll_option		= 1;	/* -O unroll0 */
int	cft90_dash_O_vector_option		= 0;	/* -O vector0 */
int	cft90_dash_O_vsearch_option		= 0;	/* -O vsearch */
# elif defined(_TARGET_OS_SOLARIS)
int	cft90_dash_O_bl_option			= 0;
int	cft90_dash_O_fastint_option		= 0;	/* -O fastint */
int	cft90_dash_O_ieeeconform_option		= 0;
int	cft90_dash_O_fusion_option		= 0;	/* -O fusion  */
int	cft90_dash_O_jump_option		= 0;
int	cft90_dash_O_loopalign_option		= 0;
int	cft90_dash_O_scalar_option		= 1;	/* -O scalar1 */
int	cft90_dash_O_split_option		= 0;	/* -O split */
int	cft90_dash_O_support_option		= 1;	/* -O 1 */
int	cft90_dash_O_task_option		= 1;	/* -O task1 */
int	cft90_dash_O_taskinner_option		= 1;	/* -O taskinner */
int	cft90_dash_O_threshold_option		= 1;	/* -O threshold */
int	cft90_dash_O_unroll_option		= 0;	/* -O unroll0 */
int	cft90_dash_O_vector_option		= 0;	/* -O vector0 */
int	cft90_dash_O_vsearch_option		= 0;	/* -O vsearch */
# elif defined(_TARGET_OS_MAX)
int	cft90_dash_O_bl_option			= 0;
int	cft90_dash_O_fastint_option		= 0;	/* -O fastint */
int	cft90_dash_O_fusion_option		= 0;	/* -O fusion  */
int	cft90_dash_O_ieeeconform_option		= 0;
int	cft90_dash_O_jump_option		= 1;
int	cft90_dash_O_loopalign_option		= 0;
int	cft90_dash_O_scalar_option		= 2;	/* -O scalar2 */
int	cft90_dash_O_split_option		= 1;	/* -O split1 */
int	cft90_dash_O_support_option		= 2;	/* -O 2 */
int	cft90_dash_O_task_option		= 0;	/* -O task0 */
int	cft90_dash_O_taskinner_option		= 0;	/* -O taskinner */
int	cft90_dash_O_threshold_option		= 0;	/* -O threshold */
int	cft90_dash_O_unroll_option		= 0;	/* -O unroll0 */
int	cft90_dash_O_vector_option		= 2;	/* -O vector2 */
int	cft90_dash_O_vsearch_option		= 0;	/* -O vsearch */
# elif defined(_TARGET_SV2)
int	cft90_dash_O_bl_option			= 0;
int	cft90_dash_O_fastint_option		= 1;	/* -O fastint */
int	cft90_dash_O_fusion_option		= 0;	/* -O fusion  */
int	cft90_dash_O_ieeeconform_option		= 0;
int	cft90_dash_O_jump_option		= 0;
int	cft90_dash_O_loopalign_option		= 0;
int	cft90_dash_O_scalar_option		= 2;	/* -O scalar2 */
int	cft90_dash_O_split_option		= 0;	/* -O split */
int	cft90_dash_O_support_option		= 2;	/* -O 2 */
int	cft90_dash_O_task_option		= 0;	/* -O task0 */
int	cft90_dash_O_taskinner_option		= 0;	/* -O taskinner */
int	cft90_dash_O_threshold_option		= 1;	/* -O threshold */
int	cft90_dash_O_vector_option		= 2;	/* -O vector2 */
int	cft90_dash_O_vsearch_option		= 1;	/* -O vsearch */

/* Unroll level 2 is always on for PVP.  It cannot be changed */
/* at the commandline.  Directives are recognized.            */

int	cft90_dash_O_unroll_option		= 2;	/* -O unroll2 */

# elif defined(_TARGET_OS_UNICOS)
int	cft90_dash_O_bl_option			= 0;
int	cft90_dash_O_fastint_option		= 1;	/* -O fastint */
int	cft90_dash_O_fusion_option		= 0;	/* -O fusion  */
int	cft90_dash_O_ieeeconform_option		= 0;
int	cft90_dash_O_jump_option		= 0;
int	cft90_dash_O_loopalign_option		= 0;
int	cft90_dash_O_scalar_option		= 2;	/* -O scalar2 */
int	cft90_dash_O_split_option		= 0;	/* -O split */
int	cft90_dash_O_support_option		= 2;	/* -O 2 */
int	cft90_dash_O_task_option		= 1;	/* -O task1 */
int	cft90_dash_O_taskinner_option		= 0;	/* -O taskinner */
int	cft90_dash_O_threshold_option		= 1;	/* -O threshold */
int	cft90_dash_O_vector_option		= 2;	/* -O vector2 */
int	cft90_dash_O_vsearch_option		= 1;	/* -O vsearch */

/* Unroll level 2 is always on for PVP.  It cannot be changed */
/* at the commandline.  Directives are recognized.            */

int	cft90_dash_O_unroll_option		= 2;	/* -O unroll2 */

# else
int	cft90_dash_O_bl_option			= 0;
int	cft90_dash_O_fastint_option		= 0;	/* -O fastint */
int	cft90_dash_O_ieeeconform_option		= 0;
int	cft90_dash_O_fusion_option		= 0;	/* -O fusion  */
int	cft90_dash_O_jump_option		= 0;
int	cft90_dash_O_loopalign_option		= 0;
int	cft90_dash_O_scalar_option		= 2;	/* -O scalar2 */
int	cft90_dash_O_split_option		= 0;	/* -O split1 */
int	cft90_dash_O_support_option		= 1;	/* -O 2 */
int	cft90_dash_O_task_option		= 0;	/* -O task0 */
int	cft90_dash_O_taskinner_option		= 0;	/* -O taskinner */
int	cft90_dash_O_threshold_option		= 0;	/* -O threshold */
int	cft90_dash_O_unroll_option		= 0;	/* -O unroll0 */
int	cft90_dash_O_vector_option		= 0;	/* -O vector0 */
int	cft90_dash_O_vsearch_option		= 0;	/* -O vsearch */
# endif


/* These are available on all platforms. */

/* the rest are 0 => off, 1 => on */
int	cft90_dash_O_aggress_option		= 0;
int	cft90_dash_O_overindex_option		= 0;
int	cft90_dash_O_pattern_option		= 1;	/* -O pattern */
int	cft90_dash_O_recurrence_option		= 1;
int	cft90_dash_O_zeroinc_option		= 0;

int	cft90_dash_G_debug_option		= 4;	/* No_Debugging */
