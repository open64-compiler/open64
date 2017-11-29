/*
 * Copyright (C) 2010 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

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


static char USMID[] = "\n@(#)3.0_pl/sources/mifcvrt.c	3.8	01/17/97 12:00:48\n";



# include "defines.h"		/* Machine dependent ifdefs */

# include "host.m"		/* Host machine dependent macros. */
# include "host.h"		/* Host machine dependent header. */
# include "target.m"		/* Target machine dependent macros. */
# include "target.h"		/* Target machine dependent header. */

# include "globals.m"
# include "tokens.m"
# include "sytb.m"
# include "debug.m"
# include "s_globals.m"

# include "globals.h"
# include "tokens.h"
# include "sytb.h"
# include "p_globals.h"

# if defined(_STANDALONE_FRONT_END)

# include "asm.h"

# include <sys/types.h>
# include <sys/stat.h>

# if defined(_HOST_OS_UNICOS) || defined(_HOST_OS_MAX)
# include <sys/target.h>
# endif

# include <mif_dialect.h>
# include <mif.h>
# include <mif_util.h>
# include <mif_io.h>


/*****************************************************************\
|* Function prototypes of static functions declared in this file *|
\*****************************************************************/

enum evcontext { value, address };
enum proc_call_class { Definition, Parent, Imported };

static	void	create_option_tbl(void);
static	void	cvrt_proc_to_mif(FILE *, char *);
static	void	cvrt_sytb_to_mif(int);
static	void	cvrt_exp_to_mif(mopd_t *, mblk_t*, int, fld_type,
                                mopnflag_t, enum evcontext);
static	void	cvrt_ir_to_mif(int);
static	int	get_basic_type(int);
static	int	get_ptr_type(fld_type, int);
static	int	get_type_idx(int);
static	void	cvrt_dummy_procedure(int);
static	int	cvrt_darg_list(int);
static	void	cvrt_proc(int, int, enum proc_call_class);
static	int	cvrt_derived_type(int);
static	int	cvrt_label(int, mopnflag_t, mpos_t);
static	int	cvrt_attr_ntry(int);
static	void	cvrt_const (char *, int, int, mopd_t *);
static	int	fold_exp(int, fld_type, int *, int *, int *);
static	void	cvrt_data_impl_do(int, fld_type);
static	void	write_mod_tbl_file_name(FILE *);
static	void	init_subprog_info(int);
        mopd_t  mif_opn_add(mblk_t *, mop_t, int, mpos_t, unsigned long,
                        mopd_t, mopd_t, mopd_t);


/*************************************************************\
|* Things that really should be in header files, but aren't. *|
\*************************************************************/

extern char frontend_version[];
extern char *getenv (const char *);


/******************************************\
|* Local data structures for this module. *|
\******************************************/

static	msubprog_t msp;			 /* MIF subprogram under construction */
static  char	   hostname[MACHINENAMELEN];
static	int	   srcix;		 /* outermost source file index */
static	int	   optionix;	 	 /* option table index */
static  char       *src_path = 0;
static	int	   local_scope;          /* current local scope index */
static	int	   host_scope;           /* current host scope index */
static	int	   F90_prod_mif_idx;	 /* product index */
static	mopd_t	   *mif_attr_map;	 /* MIF symbol/func opds for attrs */
static	int	   *mif_attr_type_map;	 /* MIF type indices for attrs */
static	int	   mif_attr_map_size;
static	mopd_t	   *mif_const_map;       /* MIF con indices for constants */
static	int	   mif_const_map_size;
static	int	   *mif_stor_blk_map;	 /* MIF symbol indices for stor blks */
static	int	   mif_stor_blk_map_size;
static	FILE	   *MIF_fp = 0;	 	 /* File pointer for MIF output */
static	boolean    unsigned_type = FALSE;
static	int	   data_value_idx;
static	int	   data_values_consumed = 0;
static	long_type  do_control_var[16];
static	long_type  implied_do_idx[16];
static	int        do_control_idx = NONE;/* number of active implied DO's - 1 */
static  int        *task_region_stk = 0;
static  int        task_region_top = -1;
static  int        task_region_alloc = 0;
static  int        loop_end_label_idx;
static  int        parallel_loop_end_label_idx;
static  int        loop_tregend_blk_idx;
static  int        loop_tregend_opn_idx;
static  int        loop_region_idx;
static  int        processing_aloc = FALSE;



/******************************************************************************\
|*									      *|
|* Description:								      *|
|*   This an entry point which is required to be defined when using the MIF   *|
|* textual interface.  It handles the situation where a MIF routine is unable *|
|* to allocate more memory.  Currently, all that is done here is to generate  *|
|* an internal error.  In the future, it is possible to deallocate some memory*|
|* with this function and then return.					      *|
|*									      *|
|* Input parameters:							      *|
|* Source file being processed, current line number, number of bytes          *|
|* attempting to allocate.						      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/
void
out_of_memory (char *srcfilename, int lineno, int bytes) {
      PRINTMSG(1, 1044, Internal, 0, "mifcvrt.c: out of memory");
} /* out_of_memory */




/******************************************************************************\
|*									      *|
|* Description:								      *|
|*   This function inserts an initializer into the correct position for an    *|
|* object.   Initializers from the frontend are not guarenteed to be in       *|
|* offset order.							      *|
|*									      *|
|* Input parameters:							      *|
|* 	Index of init table entry being added, mtag for table that object     *|
|* resides in, and index of object.					      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/
static
void
insert_init (int initix, mtag_t objtag, int objix) {
   /* either a global or local symbol table entry */
   msym_t *obj_entry = objtag==mtag_gsym ? &msp.gsym[objix] :
   						&msp.lsym[objix];
   int newoffset = msp.init[initix].offset;

   TRACE (Func_Entry, "insert_init", NULL);

   if (INVALID(obj_entry->init)) { /* no entries yet */
      obj_entry->init = initix;
   }
   else {
      int curr = obj_entry->init;
      int prev; /* will be initialized because of above test */

      /* walk through list finding place to insert */
      while (VALID(curr)) {
         prev = curr;
         curr = msp.init[curr].next;
      }

      msp.init[prev].next = initix;
      msp.init[initix].next = NONE;
   }

   TRACE (Func_Exit, "insert_init", NULL);

} /* insert_init */



/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Find the file source position using just the global line number.      *|
|*									      *|
|* Input parameters:							      *|
|*	global_line - the current global line number                          *|
|*									      *|
|* Output parameters:							      *|
|*	srcix - the global source index into the MIF src table                *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/
static int      source_position(int global_line)
{
   int		act_file_line;
   int		i;
   int		j;
   int          glb_idx;

   TRACE (Func_Entry, "source_position", NULL);

# ifdef _DEBUG
   if (global_line <= 0) {
      PRINTMSG(1, 1044, Internal, 0, "source_position: bad global line");
   }
# endif

   GLOBAL_LINE_TO_FILE_LINE(global_line, glb_idx, act_file_line);

   /* Determine the source code position of the operation. */
   if (global_line_tbl_idx == 1) {
      i = 1;
   } 
   else {
      i = 1;
      while (GL_GLOBAL_LINE(i) <= global_line) {
         i = i + 1;
         if (i > global_line_tbl_idx) {
            break;
         }
      }
      i = i - 1;
   }

   srcix = GL_MIF_FILE_ID(i);

# ifdef _DEBUG
   if (srcix < 0) {
      PRINTMSG(1, 1044, Internal, 0, "source_position: bad srcix");
   }
# endif

   return(act_file_line);

   TRACE (Func_Exit, "source_position", NULL);

} /* source_position */



/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	This does some initialization, then calls the routines to convert     *|
|*	the symbol table and and IR to the intermediate form. Then it cleans  *|
|*	up.								      *|
|*									      *|
|*	This routine should handle internal program units. (Whichever way     *|
|*	it's decided).							      *|
|*									      *|
|* Input parameters:							      *|
|*	output file stdio pointer					      *|
|*	compiler generation date					      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/
void cvrt_to_mif (char	*compiler_gen_date)

{
   int		 child_idx;
   char          msgbuf[512];


   TRACE (Func_Entry, "cvrt_to_mif", NULL);

   if (!MIF_fp) {
      MIF_fp = mif_open_output(MIF_file, msgbuf);

      if (!MIF_fp) {
         PRINTMSG(1, 1043, Error, 0, MIF_file);
         return;
      }
   }

   /* Emit intermediate text for a program unit.  It is called once     */
   /* for each BLOCKDATA, PROGRAM, SUBROUTINE, MODULE, external         */
   /* SUBROUTINE or external FUNCTION.                                  */

   /* Modules must go thru first, because MODULE PDT's must be first.  Clear  */
   /* SCP_FIRST_CHILD_IDX for the module, so its children do not get sent     */
   /* through. (cvrt_proc_to_mif finds the innermost child and sends it       */
   /* first.) After the module is sent through, send all its children via the */
   /* normal order.  Then when that's through send the module information     */
   /* table.                                                                  */

   if (ATP_PGM_UNIT(SCP_ATTR_IDX(curr_scp_idx)) == Module) {
      child_idx = SCP_FIRST_CHILD_IDX(curr_scp_idx);
      SCP_FIRST_CHILD_IDX(curr_scp_idx) = NULL_IDX;

      cvrt_proc_to_mif(MIF_fp, compiler_gen_date);

      if (child_idx != NULL_IDX) {
         curr_scp_idx = child_idx;
         cvrt_proc_to_mif(MIF_fp, compiler_gen_date);
      }

      curr_scp_idx = MAIN_SCP_IDX;

      /* Cray and MPP go out in a special &%% module because of segldr. */

# if defined(_TARGET_OS_UNICOS)
      write_mod_tbl_file_name(MIF_fp);
# endif

   }
   else {
      cvrt_proc_to_mif(MIF_fp, compiler_gen_date);
   }

   return;

   TRACE (Func_Exit, "cvrt_to_mif", NULL);

}  /*  cvrt_to_mif  */



/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	This does initialization, then converts a subprogram's symbols and IR *|
|*	to the intermediate text. Then it cleans up.			      *|
|*									      *|
|* Input parameters:							      *|
|*	output file stdio pointer					      *|
|*	compiler generation date					      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/
static void cvrt_proc_to_mif (FILE	*out_fp,
			      char	*compiler_gen_date)

{
   int		i, j, k, l;
   int		pgm_attr_idx;
   int		pgm_code;
   int		attr;
   int		pgm_data;
   int		save_curr_scp_idx;
   char		*p;
   char		*name_ptr;
   boolean	check_scp = TRUE;
   mpos_t	pos;


   TRACE (Func_Entry, "cvrt_proc_to_mif", NULL);

PROCESS_SIBLING:

   /* The innermost children are converted first.  The external		*/
   /* procedure goes out last.                                          */

   if (SCP_FIRST_CHILD_IDX(curr_scp_idx) != NULL_IDX) {
      save_curr_scp_idx	= curr_scp_idx;
      curr_scp_idx = SCP_FIRST_CHILD_IDX(curr_scp_idx);
      cvrt_proc_to_mif(out_fp, compiler_gen_date);
      curr_scp_idx = save_curr_scp_idx;
   }

   mif_attr_map_size = attr_tbl_idx + 1;
   MEM_ALLOC (mif_attr_map, mopd_t, mif_attr_map_size);
   MEM_ALLOC (mif_attr_type_map, int, mif_attr_map_size);
   for (i = 0; i < mif_attr_map_size; i++) {
       mif_attr_map[i] = mopd_null;
       mif_attr_type_map[i] = NONE;
   }

   mif_const_map_size = const_tbl_idx + 1;
   MEM_ALLOC (mif_const_map, mopd_t, mif_const_map_size);
   for (i = 0; i < mif_const_map_size; i++) {
       mif_const_map[i] = mopd_null;
   }

   mif_stor_blk_map_size = stor_blk_tbl_idx + 1;
   MEM_ALLOC (mif_stor_blk_map, int, mif_stor_blk_map_size);
   for (i = 0; i < mif_stor_blk_map_size; i++) {
       mif_stor_blk_map[i] = NONE;
   }

   pgm_attr_idx = SCP_ATTR_IDX(curr_scp_idx);
   ATP_SCP_ALIVE(pgm_attr_idx) = TRUE;

   /* Subprogram header information */
   init_subprog_info (pgm_attr_idx);
   name_ptr = ATP_EXT_NAME_PTR(pgm_attr_idx);
   msp.name = mnpool(&msp, name_ptr);

   /* Allocate scope table entries for this routine. */
   local_scope = mifalloc[mtag_scope](&msp);

   host_scope = NONE;
   if (SCP_PARENT_IDX(curr_scp_idx) != NULL_IDX) { /* a host exists */
      host_scope = mifalloc[mtag_scope](&msp);
   }
   else {  
      if (ATP_PGM_UNIT(SCP_ATTR_IDX(curr_scp_idx)) != Module) {
         host_scope = mifalloc[mtag_scope](&msp);
      }
   }

   /* Construct the type table. */
   msp.immtype = get_basic_type(INTEGER_DEFAULT_TYPE);

# ifdef _HOST32
   msp.immtype = get_basic_type(Integer_4);
# endif

   /* Construct the symbol table. */
   cvrt_sytb_to_mif(curr_scp_idx);

   /* For each and every routine there is a local scope table entry  */
   /* and a host scope table entry.  Here we make those scope table  */
   /* entries point directly to the function table of the local      */
   /* and host routines respectively.   If a routine does not have a */
   /* host, then the host scope entry for that routine will point to */
   /* itself.                                                        */
   attr = SCP_ATTR_IDX(curr_scp_idx);
   msp.scope[local_scope].func = mif_attr_map[attr].val;

   source_position(SH_GLB_LINE(SCP_FIRST_SH_IDX(curr_scp_idx)));
   pos = mpos_null;
   pos.line = source_position(SH_GLB_LINE(SCP_FIRST_SH_IDX(curr_scp_idx)));
   pos.src = srcix;
   msp.scope[local_scope].start = pos;

# if defined(_DEBUG)

      if (SCP_SB_HOSTED_STACK_IDX(curr_scp_idx) != NULL_IDX &&
          SB_LEN_FLD(SCP_SB_HOSTED_STACK_IDX(curr_scp_idx)) != CN_Tbl_Idx) {
         PRINTMSG(SB_DEF_LINE(SCP_SB_HOSTED_STACK_IDX(curr_scp_idx)), 1201,
                  Internal, 
                  SB_DEF_COLUMN(SCP_SB_HOSTED_STACK_IDX(curr_scp_idx)),
                  SB_NAME_PTR(SCP_SB_HOSTED_STACK_IDX(curr_scp_idx)));
      }

# endif

   if (SCP_PARENT_IDX(curr_scp_idx) != NULL_IDX) { /* a host exists */
      attr = SCP_ATTR_IDX(SCP_PARENT_IDX(curr_scp_idx));

      if (ATP_PGM_UNIT(attr) == Module) {  /* no host - point to itself */
         attr = SCP_ATTR_IDX(curr_scp_idx);
         msp.scope[host_scope].func = mif_attr_map[attr].val;
         msp.scope[host_scope].start = pos;
         msp.scope[host_scope].flags |= mscopeflag_host;

         /* JBL - Range issue here. */

         if (SCP_SB_HOSTED_STACK_IDX(curr_scp_idx) != NULL_IDX) {
            msp.scope[host_scope].size = 
            CN_INT_TO_C(SB_LEN_IDX(SCP_SB_HOSTED_STACK_IDX(curr_scp_idx))); 
         }
         else {
            msp.scope[host_scope].size = 0;
         }
      }
      else {  /* point to the host */
         pos = mpos_null;
         pos.line = source_position
              (SH_GLB_LINE(SCP_FIRST_SH_IDX(SCP_PARENT_IDX(curr_scp_idx))));
         pos.src = srcix;
         msp.scope[host_scope].func = mif_attr_map[attr].val;
         msp.scope[host_scope].start = pos;
         msp.scope[host_scope].flags |= mscopeflag_host;

         /* JBL - Range issue here. */

         if (SCP_SB_HOSTED_STACK_IDX(SCP_PARENT_IDX(curr_scp_idx)) != NULL_IDX){
            msp.scope[host_scope].size =
            CN_INT_TO_C(SB_LEN_IDX(SCP_SB_HOSTED_STACK_IDX(
                                   SCP_PARENT_IDX(curr_scp_idx)))); 
         }
         else {
            msp.scope[host_scope].size = 0;
         }
      }
   }
   else {  /* no host - point to itself */
      attr = SCP_ATTR_IDX(curr_scp_idx);

      if (ATP_PGM_UNIT(attr) != Module) {  
         msp.scope[host_scope].func = mif_attr_map[attr].val;
         msp.scope[host_scope].start = pos;
         msp.scope[host_scope].flags |= mscopeflag_host;

         /* JBL - Range issue here. */

         if (SCP_SB_HOSTED_STACK_IDX(curr_scp_idx) != NULL_IDX) {
            msp.scope[host_scope].size =
            CN_INT_TO_C(SB_LEN_IDX(SCP_SB_HOSTED_STACK_IDX(curr_scp_idx))); 
         }
         else {
            msp.scope[host_scope].size = 0;
         }
      }
   }

   init_directive(3);

   /* Convert the code. */
   cvrt_ir_to_mif(curr_scp_idx);

   ATP_SCP_ALIVE(pgm_attr_idx) = FALSE;

   /* free the tables */

   if (curr_scp_idx == MAIN_SCP_IDX) {

      if (ATP_PGM_UNIT(SCP_ATTR_IDX(curr_scp_idx)) != Module) {
         check_scp = FALSE;   /* Tables are gone */
      }

# if !defined(_TARGET_OS_MAX) && !defined(_TARGET_OS_UNICOS)
      else {

         /* Cray and MPP go out in a special &%% module because of segldr. */

         write_mod_tbl_file_name(out_fp);
      }
# endif
   }


   /* Write the subprogram */

   mifwrite(out_fp, &msp, cmd_line_flags.output_format, "/bin/cat");

   miffree (&msp);

   MEM_FREE (mif_attr_map);
   MEM_FREE (mif_attr_type_map);
   MEM_FREE (mif_const_map);
   MEM_FREE (mif_stor_blk_map);

   if (check_scp && SCP_SIBLING_IDX(curr_scp_idx) != NULL_IDX) {
      curr_scp_idx = SCP_SIBLING_IDX(curr_scp_idx);
      goto PROCESS_SIBLING;
   }

   TRACE (Func_Exit, "cvrt_proc_to_mif", NULL);

}  /* cvrt_proc_to_mif */



/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Convert symbol table to the intermediate form.			      *|
|*									      *|
|* Input parameters:							      *|
|*	Subprogram							      *|
|*	Scope index							      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/
static void	cvrt_sytb_to_mif(int	scp_idx)
{
   int		al_idx;
   int		attr_idx;
   int		prev_idx;
   int		tmp_scp_idx;
   int		name_idx;
   int		pgm_idx;

   TRACE (Func_Entry, "cvrt_sytb_to_mif", NULL);

   /* Claim a function table entry for the main entry right now. */
   pgm_idx = SCP_ATTR_IDX(scp_idx);
   mif_attr_map[pgm_idx].tag = mtag_func;
   mif_attr_map[pgm_idx].val = mifalloc[mtag_func](&msp);
   msp.deffunc = mif_attr_map[pgm_idx].val;

   /*
    * Claim function table entries for all parent programming units (i.e.
    * current subprogram is a nested procedure).  This is done because
    * dummy args and local stack variables are host associated.
    */
   prev_idx = msp.deffunc;
   tmp_scp_idx = SCP_PARENT_IDX(scp_idx);
   while (tmp_scp_idx != NULL_IDX) {
      pgm_idx = SCP_ATTR_IDX(tmp_scp_idx);
      mif_attr_map[pgm_idx].tag = mtag_func;
      mif_attr_map[pgm_idx].val = mifalloc[mtag_func](&msp);

      /* Set up the parent subprogram link. */
      msp.func[prev_idx].within = mif_attr_map[pgm_idx].val;
      prev_idx = mif_attr_map[pgm_idx].val;
      tmp_scp_idx = SCP_PARENT_IDX(tmp_scp_idx);
   }

   /* Sends main entry point, alternate entry points, function      */
   /* results, all dummy arguments, and any accessed derived types. */

   cvrt_proc(SCP_ATTR_IDX(scp_idx), SCP_ENTRY_IDX(scp_idx), Definition);

   for (name_idx = SCP_LN_FW_IDX(scp_idx) + 1;
        name_idx < SCP_LN_LW_IDX(scp_idx);
        name_idx++) {
      attr_idx = LN_ATTR_IDX(name_idx);
      cvrt_attr_ntry(attr_idx);
   }

   al_idx = SCP_ATTR_LIST(curr_scp_idx);
   while (al_idx != NULL_IDX) {
      if (AT_OBJ_CLASS(AL_ATTR_IDX(al_idx))  == Data_Obj        &&
          ATD_CLASS(AL_ATTR_IDX(al_idx))     == Compiler_Tmp    &&
          AT_REFERENCED(AL_ATTR_IDX(al_idx)) == Not_Referenced) {

         /* intentionally blank. Don't send unreferenced tmps through */
      }
      else if (mif_attr_map [AL_ATTR_IDX(al_idx)].tag == mtag_none) {
         cvrt_attr_ntry(AL_ATTR_IDX(al_idx));
      }

      al_idx = AL_NEXT_IDX(al_idx);
   }

   /* Send all parent procedures */
   tmp_scp_idx = SCP_PARENT_IDX(scp_idx);
   while (tmp_scp_idx != NULL_IDX) {
      cvrt_proc(SCP_ATTR_IDX(tmp_scp_idx),
                SCP_ENTRY_IDX(tmp_scp_idx),
                Parent);
      tmp_scp_idx = SCP_PARENT_IDX(tmp_scp_idx);
   }

   TRACE (Func_Exit, "cvrt_sytb_to_mif", NULL);

}  /* cvrt_sytb_to_mif */



/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Add a list of tasking symbols to MIF.				      *|
|*									      *|
|* Input parameters:							      *|
|*	list_idx - index into frontend's IL table which references the list   *|
|*		   of symbols.						      *|
|*	default_usage - default tasking symbol usage of symbol.		      *|
|*	dope_is_value - flag if dope symbols should be treated as value usage *|
|*									      *|
|* Output parameters:							      *|
|*	t - tasking region which symbols are to be added to.		      *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/
static void	add_tasking_symbols(mtaskreg_t *t,
				    int list_idx,
				    mtaskusage_t default_usage,
				    boolean dope_is_value)
{
	int sublist_idx;
       	int symix;
	mtasksym_t *s;
	mtasksym_t *l;

	TRACE (Func_Entry, "add_tasking_symbols", NULL);

	/* Process list of variables */
	if (IL_FLD(list_idx) == IL_Tbl_Idx) {
	   sublist_idx = IL_IDX(list_idx);
	   while (sublist_idx) {
        	symix = mifalloc[mtag_tasksym](&msp);
		s = msp.tasksym + symix;

		/* Add symbol to tail of list */
                l = msp.tasksym + t->tasksym;
                if (VALID(t->tasksym)) {
                   while (VALID(l->next)) {
                      l = msp.tasksym + l->next;
                   }
                   l->next = symix;
                }
                else {
                   t->tasksym = symix;
                }

		if (dope_is_value &&
		    AT_OBJ_CLASS(IL_IDX(sublist_idx)) == Data_Obj &&
		    (ATD_IM_A_DOPE(IL_IDX(sublist_idx)) ||
		    (TYP_TYPE(ATD_TYPE_IDX(IL_IDX(sublist_idx))) == Structure &&
#ifdef KEY /* Bug 6845 */
			 (ATT_ALLOCATABLE_CPNT(TYP_IDX(ATD_TYPE_IDX(
				     IL_IDX(sublist_idx)))) ||
			 ATT_POINTER_CPNT(TYP_IDX(ATD_TYPE_IDX(
				     IL_IDX(sublist_idx)))))
#else /* KEY Bug 6845 */
			 ATT_POINTER_CPNT(TYP_IDX(ATD_TYPE_IDX(
				     IL_IDX(sublist_idx))))
#endif /* KEY Bug 6845 */
				     ))) {
		    /* must be VALUE not PRIVATE for dope vectors */
		   s->taskusage = mtaskusage_value;
		}
		else {
		   s->taskusage = default_usage;
		}
		s->sym = mif_attr_map[IL_IDX(sublist_idx)];

		sublist_idx = IL_NEXT_LIST_IDX(sublist_idx);
	   }
	}

	TRACE (Func_Exit, "cvrt_sytb_to_mif", NULL);

} /* add_tasking_symbols */



/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Map frontend work distribution to MIF distribution.		      *|
|*									      *|
|* Input parameters:							      *|
|*	val - frontend work distribution value				      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	MIF work distibution value					      *|
|*									      *|
\******************************************************************************/
static mworkdist_t	map_work_distribution(int val)
{
	   switch (val) {
	   case CMIC_WORK_DIST_SINGLE:
		return mworkdist_single;

	   case CMIC_WORK_DIST_VECTOR:
		return mworkdist_vector;

	   case CMIC_WORK_DIST_GUIDED:
		return mworkdist_guided;

	   case CMIC_WORK_DIST_NUMCHUNKS:
		return mworkdist_numchunks;

	   case CMIC_WORK_DIST_CHUNKSIZE:
		return mworkdist_chunksize;

	   case CMIC_WORK_DIST_NCPUS_CHUNKS:
		return mworkdist_ncpus_chunks;

	   default:
            PRINTMSG(1, 1044, Internal, 0,
		"map_work_distribution:  unexpected work distribution");
	   }

} /* map_work_distribution */



/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Start a new tasking region.					      *|
|*									      *|
|* Input parameters:							      *|
|*	ir_idx - frontend ir index for tasking region			      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	MIF taskreg index						      *|
|*									      *|
\******************************************************************************/
static int	start_task_region(mblk_t		*blk,
				  int			ir_idx,
                                  mopnflag_t	        flags,
				  mpos_t		pos)
{
        int tskix = mifalloc[mtag_taskreg](&msp);
	mtaskreg_t *t = msp.taskreg + tskix;
	int list_idx;
       	int symix;
	mtasksym_t *s;
	mopd_t opd0;
	mtype_t typ;
	mtasksym_t *l;

	TRACE (Func_Entry, "start_task_region", NULL);

	if (IR_OPR(ir_idx)==Case_Cmic_Opr &&
		msp.taskreg[task_region_stk[task_region_top]].regionclass==
			mregionclass_case) {
	   /* end previous case region and reuse task region stack entry */
           opd0.tag = mtag_taskreg;
           opd0.val = task_region_stk[task_region_top];
	   mif_opn_add(blk, 
                   mop_tregend, 
                   get_basic_type(NONE),
                   pos,
		   flags, 
                   opd0, 
                   mopd_null, 
                   mopd_null);
	}
	else {
	   /* See if there is enough room in the tasking region stack */
	   if (++task_region_top >= task_region_alloc) {
	       task_region_alloc += 5;
               MEM_REALLOC(task_region_stk, int, task_region_alloc);
	   }
	}

	/* Stack tasking region */
	task_region_stk[task_region_top] = tskix;

	/* Do region specific setting */
	switch (IR_OPR(ir_idx)) {
	case Case_Cmic_Opr:
	   t->regionclass = mregionclass_case;
	   t->within = task_region_stk[task_region_top-1];
	   break;

	case Guard_Cmic_Opr:
	   t->regionclass = mregionclass_guard;

	   /* Add temp if numbered guard */
	   if (IR_FLD_L(ir_idx) != NO_Tbl_Idx) {
		t->guardtemp = cvrt_attr_ntry(IR_IDX_L(ir_idx));
	   }
	   break;

	case Doall_Cmic_Opr:
	   t->regionclass = mregionclass_doall;

	   /* Process if temp (list element 0) */
	   list_idx = IR_IDX_L(ir_idx);
	   if (IL_FLD(list_idx) != NO_Tbl_Idx) {
		t->iftemp = cvrt_attr_ntry(IL_IDX(list_idx));
	   }

	   /* Process SHARED variables (list element 1) */
	   list_idx = IL_NEXT_LIST_IDX(list_idx);
	   add_tasking_symbols(t, list_idx, mtaskusage_shared, FALSE);

	   /* Process PRIVATE variables (list element 2) */
	   list_idx = IL_NEXT_LIST_IDX(list_idx);
	   add_tasking_symbols(t, list_idx, mtaskusage_private, TRUE);

	   /* See if AUTOSCOPING was specified (list element 3) */
	   list_idx = IL_NEXT_LIST_IDX(list_idx);
	   if (IL_FLD(list_idx) == CN_Tbl_Idx) {
		t->flags |= mtaskregflag_defaultusage;
	   }

	   /* Process CONTROL variables (list element 4) */
	   list_idx = IL_NEXT_LIST_IDX(list_idx);
	   /* BECKER - MIF does not have context usage, but PDGCS currently
	    * treats it the same as context iterate.
	    */
	   add_tasking_symbols(t, list_idx, mtaskusage_iterate, FALSE);

	   /* See if SAVELAST was specified (list element 5) */
	   list_idx = IL_NEXT_LIST_IDX(list_idx);
	   if (IL_FLD(list_idx) == CN_Tbl_Idx) {
		t->flags |= mtaskregflag_savelast;
	   }

	   /* Process maxcpus temp (list element 6) */
	   list_idx = IL_NEXT_LIST_IDX(list_idx);
	   if (IL_FLD(list_idx) != NO_Tbl_Idx) {
		t->maxcpustemp = cvrt_attr_ntry(IL_IDX(list_idx));
	   }

	   /* Process work distribution class (list element 7) */
	   list_idx = IL_NEXT_LIST_IDX(list_idx);
	   t->workdist = map_work_distribution(CN_INT_TO_C(IL_IDX(list_idx)));

	   /* Process work distribution temp (list element 8) */
	   list_idx = IL_NEXT_LIST_IDX(list_idx);
	   if (IL_FLD(list_idx) != NO_Tbl_Idx) {
		t->disttemp = cvrt_attr_ntry(IL_IDX(list_idx));
	   }

	   /* Process loop control variable */
           symix = mifalloc[mtag_tasksym](&msp);
	   s = msp.tasksym + symix;

           /* Add symbol to tail of list */
           l = msp.tasksym + t->tasksym;
           if (VALID(t->tasksym)) {
              while (VALID(l->next)) {
                 l = msp.tasksym + l->next;
              }
              l->next = symix;
           }
           else {
              t->tasksym = symix;
           }

	   s->taskusage = mtaskusage_induction;
	   s->sym = mif_attr_map[IR_IDX_R(ir_idx)];

	   /* BECKER - Hack code to be removed once frontend
 	    * clearly marks the end of a parallel loop.
	    *
	    * Preserve label index which ends parallel loop.
            */
	   parallel_loop_end_label_idx = loop_end_label_idx;
	   break;

	case Doparallel_Cmic_Opr:
	   t->regionclass = mregionclass_loop;
	   t->within = task_region_stk[task_region_top-1];

	   /* Process work distribution class (list element 0) */
	   list_idx = IR_IDX_L(ir_idx);
	   t->workdist = map_work_distribution(CN_INT_TO_C(IL_IDX(list_idx)));

	   /* Process work distribution temp (list element 1) */
	   list_idx = IL_NEXT_LIST_IDX(list_idx);
	   if (IL_FLD(list_idx) != NO_Tbl_Idx) {
		t->disttemp = cvrt_attr_ntry(IL_IDX(list_idx));
	   }

	   /* Process loop control variable */
           symix = mifalloc[mtag_tasksym](&msp);
	   s = msp.tasksym + symix;
	   t->tasksym = symix;
	   s->taskusage = mtaskusage_induction;
	   s->sym =  mif_attr_map[IR_IDX_R(ir_idx)];

	   /* BECKER - Hack code to be removed once frontend
 	    * clearly marks the end of a parallel loop.
	    *
	    * Preserve label index which ends parallel loop.
            */
	   parallel_loop_end_label_idx = loop_end_label_idx;
	   break;

	case Parallel_Cmic_Opr:
	   t->regionclass = mregionclass_parallel;

	   /* Process if temp (list element 0) */
	   list_idx = IR_IDX_L(ir_idx);
	   if (IL_FLD(list_idx) != NO_Tbl_Idx) {
		t->iftemp = cvrt_attr_ntry(IL_IDX(list_idx));
	   }

	   /* Process SHARED variables (list element 1) */
	   list_idx = IL_NEXT_LIST_IDX(list_idx);
	   add_tasking_symbols(t, list_idx, mtaskusage_shared, FALSE);

	   /* Process PRIVATE variables (list element 2) */
	   list_idx = IL_NEXT_LIST_IDX(list_idx);
	   add_tasking_symbols(t, list_idx, mtaskusage_private, TRUE);

	   /* See if AUTOSCOPING was specified (list element 3) */
	   list_idx = IL_NEXT_LIST_IDX(list_idx);
	   if (IL_FLD(list_idx) == CN_Tbl_Idx) {
		t->flags |= mtaskregflag_defaultusage;
	   }

	   /* Process CONTROL variables (list element 4) */
	   list_idx = IL_NEXT_LIST_IDX(list_idx);
	   /* BECKER - MIF does not have context usage, but PDGCS currently
	    * treats it the same as context iterate.
	    */
	   add_tasking_symbols(t, list_idx, mtaskusage_iterate, FALSE);

	   /* Process maxcpus temp (list element 5) */
	   list_idx = IL_NEXT_LIST_IDX(list_idx);
	   if (IL_FLD(list_idx) != NO_Tbl_Idx) {
		t->maxcpustemp = cvrt_attr_ntry(IL_IDX(list_idx));
	   }
	   break;

	default:
            PRINTMSG(1, 1044, Internal, 0, 
                     "start_task_region:  unexpected OPR");
	   break;
	}

	TRACE (Func_Exit, "start_task_region", NULL);

	return tskix;

} /* start_task_region */



/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Convert an expression to the intermediate form.			      *|
|*									      *|
|* Input parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/
static void	cvrt_exp_to_mif(mopd_t		*result,
				mblk_t		*blk,
				int		ir_idx,
                                fld_type	field,
                                mopnflag_t      flags,
				enum evcontext	context)
{

   int		attr_idx;
   int          base_attr;
   boolean      bound_chk;
   int		next_idx;
   int		temp_ir_idx;
   long_type    i,j;
   int		unused1;
   int		unused2;
   int		basic;
   int		opnix;
   int		tskix;
   mopd_t	opd, opd0, opd1, opd2;
   int		typeix;
   mop_t	op;
   mtype_t	type;
   mpos_t	pos     = mpos_null;
   mopn_t	*o;
   mopnflag_t	save_flags;


   TRACE (Func_Entry, "cvrt_exp_to_mif", NULL);

   *result = mopd_null;


switch (field) {
   
case NO_Tbl_Idx :
   break;


case CN_Tbl_Idx :

   if (ir_idx >= mif_const_map_size || 
      (mif_const_map[ir_idx].tag == mtag_none)) {
      /* Constant not seen before.  Convert and add to mapping table. */

      cvrt_const((char *)&CN_CONST(ir_idx),
 	         CN_TYPE_IDX(ir_idx),
		 CN_TYPE_IDX(ir_idx),
		 result);

      if (ir_idx < mif_const_map_size) {
         mif_const_map[ir_idx] = *result;
      }
   }
   else {
      /* Constant was seen before, get converted constant from mapping table. */
      *result = mif_const_map[ir_idx];
   }

   break;


case IL_Tbl_Idx :

   /* Convert the list into a chain of list pseudo-operations. */
   for (; ir_idx != NULL_IDX; ir_idx = IL_NEXT_LIST_IDX(ir_idx)) {
      if (IL_IDX(ir_idx) != NULL_IDX) {
         cvrt_exp_to_mif(&opd0, 
                         blk,
                         IL_IDX(ir_idx), 
                         IL_FLD(ir_idx),
			 flags, 
                         context);
      }
      else {
	 opd0 = mopd_null;
      }

      *result = mif_opn_add(blk, 
                        mop_list, 
                        mopdtype(&msp, opd0, blk->opn),
			mpos_null, 
                        0, 
                        opd0, 
                        *result, 
                        mopd_null);
   } 
   break;


case AT_Tbl_Idx :

   if (AT_OBJ_CLASS(ir_idx) == Data_Obj) {
      if (ATD_CLASS(ir_idx) == Compiler_Tmp &&
          ATD_TMP_INIT_NOT_DONE(ir_idx)) {
         insert_init_stmt_for_tmp(ir_idx);
      }

      if (context == address) {
         *result = mif_opn_add(blk, 
                           mop_loc,
                           get_ptr_type(AT_Tbl_Idx, ir_idx), 
                           mpos_null, 
                           0,
                           mif_attr_map[ir_idx], 
                           mopd_null, 
                           mopd_null);
      }
      else {
         *result = mif_attr_map[ir_idx];
      }
   }
   else if (AT_OBJ_CLASS(ir_idx) == Pgm_Unit) {
      if ((ATP_PROC(ir_idx) == Extern_Proc) &&
          (AT_ACTUAL_ARG(ir_idx)) &&
          (ATP_DCL_EXTERNAL(ir_idx))) {
         *result = mif_opn_add(blk, 
                           mop_loc, 
                           get_ptr_type(AT_Tbl_Idx, ir_idx), 
                           pos, 
                           0,
                           mif_attr_map[ir_idx], 
                           mopd_null, 
                           mopd_null);
      }
      else {
         *result = mif_attr_map[ir_idx];
      }
   }
   break;


case IR_Tbl_Idx :


# ifdef _DEBUG
   if (IR_TYPE_IDX(ir_idx) == NULL_IDX) {
      PRINTMSG(IR_LINE_NUM(ir_idx), 
               993, 
               Internal,
               IR_COL_NUM(ir_idx));
   }
# endif

   basic = get_basic_type(IR_TYPE_IDX(ir_idx));

   /* Set flag which controls array syntax expansion in tmod. */
   if (IR_RANK(ir_idx)) {
      flags |= mopnflag_array;
   }
   else {
      flags &= ~mopnflag_array;
   }

   /* Determine the source code position of the operation. */
   pos = mpos_null;
   pos.line = source_position(IR_LINE_NUM(ir_idx));
   pos.src = srcix;
   pos.col = IR_COL_NUM(ir_idx);

   switch (IR_OPR(ir_idx)) { 

   /* Niladic intrinsics */
   case Ranf_Opr :
	*result = mif_opn_add(blk, mop_ranf, basic, pos,
	               flags | mopnflag_precious |
		               mopnflag_immobile |
		               mopnflag_runtime |
		               mopnflag_variant |
		               mopnflag_distinct,
                       mopd_null, mopd_null, mopd_null);
        break;

   case Rtc_Opr :
	*result = mif_opn_add(blk, mop_clock, basic, pos,
	               flags | mopnflag_immobile |
		               mopnflag_runtime |
		               mopnflag_variant |
		               mopnflag_distinct,
                       mopd_null, mopd_null, mopd_null);
        break;

   case Numarg_Opr :
	*result = mif_opn_add(blk, mop_numargs, basic, pos,
			      flags, mopd_null, mopd_null, mopd_null);
	break;

   case My_Pe_Opr :
        break;

   case Argchck_Present_Opr :
	*result = mif_opn_add(blk, mop_argchk, basic, pos,
			      flags, mopd_null, mopd_null, mopd_null);
        break;

   case Argchck_Loc_Opr :
	*result = mif_opn_add(blk, mop_argckloc, basic, pos,
			      flags, mopd_null, mopd_null, mopd_null);
        break;

   case Readsm_Opr :
        opd0 = mopd_0;
        opd0.val = _semget_op; 
	*result = mif_opn_add(blk, mop_asm, basic, pos,
			      flags, opd0, mopd_null, mopd_null);
	break;

   case Remote_Write_Barrier_Opr :
        opd0 = mopd_0;
        opd0.val = _remote_write_barrier_op;
        *result = mif_opn_add(blk, mop_asm, basic, pos,
                              flags, opd0, mopd_null, mopd_null);
        break;

   case Memory_Barrier_Opr :
        opd0 = mopd_0;
        opd0.val = _memory_barrier_op;
        *result = mif_opn_add(blk, mop_asm, basic, pos,
                              flags, opd0, mopd_null, mopd_null);
        break;

   case Write_Memory_Barrier_Opr :
        opd0 = mopd_0;
        opd0.val = _write_memory_barrier_op;
        *result = mif_opn_add(blk, mop_asm, basic, pos,
                              flags, opd0, mopd_null, mopd_null);
        break;

   case Mul_Opr : 
	*result = mif_opn_add(blk, mop_bmul, basic, pos,
			      flags, mopd_null, mopd_null, mopd_null);
	break;

   case Mcbl_Opr : 
	*result = mif_opn_add(blk, mop_bmclr, basic, pos,
			      flags, mopd_null, mopd_null, mopd_null);
	break;

   case Get_Ieee_Exceptions_Opr : 
	*result = mif_opn_add(blk, mop_get_all_estat, basic, pos,
			      flags, mopd_null, mopd_null, mopd_null);
	break;

   case Get_Ieee_Interrupts_Opr : 
	*result = mif_opn_add(blk, mop_get_interupt, basic, pos,
			      flags, mopd_null, mopd_null, mopd_null);
	break;

   case Get_Ieee_Rounding_Mode_Opr : 
	*result = mif_opn_add(blk, mop_getround, basic, pos,
			      flags, mopd_null, mopd_null, mopd_null);
	break;

   case Endcase_Cmic_Opr: 
   case Endguard_Cmic_Opr: 
   case Enddo_Cmic_Opr:
   case Endparallel_Cmic_Opr:
	/* Additional flag setting */
	switch (IR_OPR(ir_idx)) { 
        case Enddo_Cmic_Opr:

	    /* BECKER - Hack code to be removed once frontend
             * clearly marks the end of a parallel loop.
	     *
	     * Remove tregend generated at end of loop.
	     * Assume last visited region was a task loop.
	     */
	    msp.blk[loop_tregend_blk_idx].opn[loop_tregend_opn_idx] = mopn_null;
	    task_region_top++;
	    task_region_stk[task_region_top] = loop_region_idx;

           msp.taskreg[task_region_stk[task_region_top]].flags |=
						mtaskregflag_extendloop;
	   break;

        case Endcase_Cmic_Opr: 
           msp.taskreg[task_region_stk[task_region_top]].flags |=
						mtaskregflag_lastcase;
	   break;
	}

        opd0.tag = mtag_taskreg;
        opd0.val = task_region_stk[task_region_top--];
	*result = mif_opn_add(blk, 
                          mop_tregend, 
                          get_basic_type(NONE),
                          pos,
			  flags, 
                          opd0, 
                          mopd_null, 
                          mopd_null);
	break;


   /* Monadic intrinsics */
   case Int_Mult_Upper_Opr :
   case Get_Ieee_Status_Opr : 
   case Numcpus_Cmic_Opr : 
   case Mmx_Opr : 
   case Mld_Opr : 
   case Uplus_Opr :
   case Uminus_Opr :
   case Paren_Opr :
   case Present_Opr :
   case Abs_Opr :
   case Cos_Opr :
   case Sin_Opr :
   case Tan_Opr :
   case Acos_Opr :
   case Asin_Opr :
   case Atan_Opr :
   case Cot_Opr :
   case Exp_Opr :
   case Sqrt_Opr :
   case Cosh_Opr :
   case Sinh_Opr :
   case Tanh_Opr :
   case Log_10_Opr :
   case Log_E_Opr :
   case Conjg_Opr :
   case Dble_Opr :
   case Int_Opr :
   case Logical_Opr :
   case Real_Opr :
   case Ichar_Opr :
   case Char_Opr :
   case Cvrt_Opr :
   case Cvrt_Unsigned_Opr :
   case Leadz_Opr :
   case Poppar_Opr :
   case Popcnt_Opr :
   case Not_Opr :
   case Bnot_Opr :
   case Nint_Opr :
   case Anint_Opr :
   case Aint_Opr :
   case Aimag_Opr :
   case Clen_Opr :
   case Len_Trim_Opr :
   case Adjustl_Opr :
   case Adjustr_Opr :
   case Ceiling_Opr :
   case Floor_Opr :
   case Exponent_Opr :
   case Fraction_Opr :
   case Unit_Opr :
   case Getpos_Opr :
   case Length_Opr :
   case Transpose_Opr:
   case Mask_Opr :
   case Ranget_Opr :
   case Ranset_Opr :
   case Set_Ieee_Status_Opr :
   case Set_Ieee_Exceptions_Opr :
   case Set_Ieee_Interrupts_Opr :
   case Set_Ieee_Rounding_Mode_Opr :
   case Test_Ieee_Interrupt_Opr :
   case Test_Ieee_Exception_Opr :
   case Enable_Ieee_Interrupt_Opr :
   case Disable_Ieee_Interrupt_Opr :
   case Ieee_Finite_Opr :
   case Ieee_Is_Nan_Opr :
   case Ieee_Class_Opr :

        opd0 = mopd_null;
        opd1 = mopd_null;
        opd2 = mopd_null;

        cvrt_exp_to_mif(&opd0, 
                        blk,
                        IR_IDX_L(ir_idx), 
                        IR_FLD_L(ir_idx),
		        flags, 
                        value);

	switch (IR_OPR(ir_idx)) { 
        case Int_Mult_Upper_Opr:
             opd1 = opd0;
             opd0 = mopd_0;
             opd0.val = _int_mult_upper_op;
	     op = mop_asm;
	     break;
	case Numcpus_Cmic_Opr :
	     op = mop_set_numcpus;
	     break;
	case Mmx_Opr :
	     op = mop_bmmx;
	     break;
	case Mld_Opr :
	     op = mop_bmld;
	     break;
	case Uplus_Opr :
	     op = mop_xmit;
	     break;
	case Paren_Opr :
	     op = mop_paren;
	     break;
	case Present_Opr :
             op = mop_present;
             break;
	case Uminus_Opr :
	     op = mop_neg;
	     break;
	case Abs_Opr :
	     if (msp.type[mopdtype(&msp, opd0, blk->opn)].u.class ==
			mtypeclass_complex) {
	        op = mop_cabs;
	     }
	     else {
	        op = mop_abs;
	     }
	     break;
	case Cos_Opr :
	     op = mop_cos;
	     break;
	case Sin_Opr :
	     op = mop_sin;
	     break;
	case Tan_Opr :
	     op = mop_tan;
	     break;
	case Acos_Opr :
	     op = mop_acos;
	     break;
	case Asin_Opr :
	     op = mop_asin;
	     break;
	case Atan_Opr :
	     op = mop_atan;
	     break;
	case Cot_Opr :
	     op = mop_cot;
	     break;
	case Exp_Opr :
	     op = mop_exp;
	     break;
	case Sqrt_Opr :
	     op = mop_sqrt;
	     break;
	case Cosh_Opr :
	     op = mop_cosh;
	     break;
	case Sinh_Opr :
	     op = mop_sinh;
	     break;
	case Tanh_Opr :
	     op = mop_tanh;
	     break;
	case Log_10_Opr :
	     op = mop_log10;
	     break;
	case Log_E_Opr :
	     op = mop_log;
	     break;
	case Conjg_Opr :
	     op = mop_conjg;
	     break;
        case Dble_Opr :
        case Int_Opr :
        case Logical_Opr :
        case Real_Opr :
        case Ichar_Opr :
        case Char_Opr :
        case Cvrt_Opr :
	     op = mop_cast;
	     break;
        case Cvrt_Unsigned_Opr :
             unsigned_type = TRUE;
             basic = get_basic_type(IR_TYPE_IDX(ir_idx));
             unsigned_type = FALSE;
	     op = mop_cast;
	     break;
        case Leadz_Opr :
	     op = mop_lead0;
	     break;
        case Poppar_Opr :
	     op = mop_parity;
	     break;
        case Popcnt_Opr :
	     op = mop_pop;
	     break;
        case Not_Opr :
	     op = mop_not;
	     break;
        case Bnot_Opr :
	     op = mop_not;
	     break;
        case Nint_Opr :
        case Anint_Opr :
	     op = mop_round;
	     break;
        case Aint_Opr :
	     op = mop_trunc;
	     break;
        case Aimag_Opr :
	     op = mop_cast;
             opd2 = mopd_1;	/* select imaginary part */
	     break;
        case Clen_Opr :
	     op = mop_chlen;
	     break;
	case Len_Trim_Opr :
	     op = mop_chlentr;
	     break;
	case Adjustl_Opr :
	     op = mop_chadjl;
	     break;
	case Adjustr_Opr :
	     op = mop_chadjr;
	     break;
	case Unit_Opr :
	     op = mop_unit;
	     break;
	case Getpos_Opr :
	     op = mop_getpos;
	     break;
	case Length_Opr :
	     op = mop_length;
	     break;
	case Ceiling_Opr :
	     op = mop_ceiling;
	     break;
	case Floor_Opr :
	     op = mop_floor;
	     break;
	case Exponent_Opr :
	     op = mop_getexpo;
	     break;
	case Fraction_Opr :
	     op = mop_fract;
	     break;
        case Transpose_Opr:
	     op = mop_transp;
	     break;
   	case Mask_Opr :
	     op = mop_mask;
	     break;
	case Ranget_Opr :
	     op = mop_ranget;
	     break;
	case Ranset_Opr :
	     op = mop_ranset;
	     break;
        case Set_Ieee_Status_Opr :
	     op = mop_set_stat;
	     break;
        case Get_Ieee_Status_Opr :
             opd1 = opd0;
             opd0 = mopd_0;
             opd0.val = _readSR_op;
	     op = mop_asm;
	break;

        case Set_Ieee_Exceptions_Opr :
	     op = mop_set_all_estat;
	     break;
        case Set_Ieee_Interrupts_Opr :
	     op = mop_set_interupt;
	     break;
        case Set_Ieee_Rounding_Mode_Opr :
	     op = mop_setround;
	     break;
        case Test_Ieee_Interrupt_Opr :
	     op = mop_tst_interupt;
	     break;
        case Test_Ieee_Exception_Opr :
	     op = mop_tst_estat;
	     break;
        case Enable_Ieee_Interrupt_Opr :
	     op = mop_enbl_interupt;
	     break;
        case Disable_Ieee_Interrupt_Opr :
	     op = mop_dsbl_interupt;
	     break;
        case Ieee_Finite_Opr :
	     op = mop_isfinite;
	     break;
        case Ieee_Is_Nan_Opr :
	     op = mop_isnan;
	     break;
        case Ieee_Class_Opr :
	     op = mop_fpclass;
	     break;
	}

        *result = mif_opn_add(blk, 
			      op, 
                              basic, 
		 	      pos, 
                              flags, 
 			      opd0, 
			      opd1, 
			      opd2);
        break;


   /* Dyadic intrinsics */
   case Set_Ieee_Exception_Opr :
   case Clear_Ieee_Exception_Opr :
   case Spacing_Opr :
   case Rrspacing_Opr :
   case I24mult_Opr : 
   case Mldmx_Opr :
   case All_Opr:
   case Any_Opr:
   case Count_Opr:
   case Atan2_Opr :
   case Cmplx_Opr :
   case Dim_Opr :
   case Mod_Opr :
   case Modulo_Opr :
   case Sign_Opr :
   case Scale_Opr :
   case Set_Exponent_Opr :
   case Dprod_Opr :
   case Fcd_Opr :
   case Shiftl_Opr :
   case Shiftr_Opr :
   case Shifta_Opr :
   case Shift_Opr :
   case Dot_Product_Opr :
   case Matmul_Opr :
   case Minloc_Opr :
   case Maxloc_Opr :
   case Ieee_Next_After_Opr :
   case Ieee_Unordered_Opr :
   case Ieee_Remainder_Opr :
   case Ieee_Int_Opr :
   case Ieee_Real_Opr :
   case Ieee_Copy_Sign_Opr :
   case Ieee_Binary_Scale_Opr :
   case Ieee_Exponent_Opr :

	opd0 = mopd_null;
	opd1 = mopd_null;
	opd2 = mopd_null;

	i = IR_IDX_L(ir_idx);
        cvrt_exp_to_mif(&opd0, blk, IL_IDX(i), IL_FLD(i), flags, value);
	i = IL_NEXT_LIST_IDX(i);
        cvrt_exp_to_mif(&opd1, blk, IL_IDX(i), IL_FLD(i), flags, value);

	switch (IR_OPR(ir_idx)) {
	case Spacing_Opr :
	     op = mop_spacing;
	     break;
	case Rrspacing_Opr :
  	     op = mop_rrspcng;
	     break;
	case I24mult_Opr :
             op = mop_i24mult;
             break;
	case Mldmx_Opr :
	     op = mop_bmldmx;
             break;
	case All_Opr :
	     op = mop_all;
             opd2 = opd0;
             opd0 = mopd_null;
             break;
	case Any_Opr :
	     op = mop_any;
             opd2 = opd0;
             opd0 = mopd_null;
             break;
	case Count_Opr :
	     op = mop_count;
             opd2 = opd0;
             opd0 = mopd_null;
             break;
	case Atan2_Opr :
	     op = mop_atan2;
             break;
        case Cmplx_Opr :
	     op = mop_cast;
             break;
        case Dim_Opr :
	     op = mop_dim;
             break;
        case Mod_Opr :
	     op = mop_rem;
             break;
        case Modulo_Opr :
	     op = mop_mod;
             break;
	case Sign_Opr :
	     op = mop_sign;
	     break;
	case Scale_Opr :
	     op = mop_scale;
	     break;
	case Set_Exponent_Opr :
	     op = mop_setexpo;
	     break;
	case Dprod_Opr :
	     op = mop_mul;
	     break;
	case Fcd_Opr :
	     op = mop_cast;
	     break;
	case Shiftl_Opr :
	     op = mop_lsh;
	     opd2 = opd1;
	     opd1 = mopd_null;
	     break;
	case Shiftr_Opr :
	     op = mop_rsh;
             opd2 = opd1;
	     opd1 = opd0;
	     opd0 = mopd_null;
	     break;
	case Shifta_Opr :
	     op = mop_mrsh;
             opd2 = opd1;
	     opd1 = opd0;
	     opd0 = mopd_null;
	     break;
	case Shift_Opr :
	     opd2 = opd1;
	     opd1 = opd0;
	     op = mop_lsh;
	     break;
        case Dot_Product_Opr :
	     op = mop_dotprod;
	     break;
        case Matmul_Opr :
	     op = mop_matmul;
	     break;
        case Minloc_Opr :
	     op = mop_minloc;
	     opd2 = opd1;
	     opd1 = mopd_null;
	     break;
        case Maxloc_Opr :
	     op = mop_maxloc;
	     opd2 = opd1;
	     opd1 = mopd_null;
	     break;
        case Ieee_Next_After_Opr :
	     op = mop_nextafter;
	     break;
        case Ieee_Unordered_Opr :
	     op = mop_isunordered;
	     break;
        case Ieee_Remainder_Opr :
	     op = mop_remainder;
	     break;
        case Ieee_Int_Opr :
	     op = mop_ieee_trunc; 
	     break;
        case Ieee_Real_Opr :
	     op = mop_ieee_round;
	     break;
        case Ieee_Copy_Sign_Opr :
	     op = mop_sign_xfer;
	     break;
        case Ieee_Binary_Scale_Opr :
	     op = mop_scalb;
	     break;
        case Ieee_Exponent_Opr :
	     op = mop_logb;
	     break;
        case Set_Ieee_Exception_Opr :
	     op = mop_set_estat;
	     break;
        case Clear_Ieee_Exception_Opr :
	     op = mop_clr_estat;
	     break;

        }

        *result = mif_opn_add(blk, op, basic, pos, flags, opd0, opd1, opd2);
        break;


   /* Triadic intrinsics */
   case Nearest_Opr :
   case Dshiftl_Opr :
   case Dshiftr_Opr :
   case Cvmgp_Opr :
   case Cvmgm_Opr :
   case Cvmgz_Opr :
   case Cvmgn_Opr :
   case Cvmgt_Opr :
   case Csmg_Opr :
   case Index_Opr :
   case Scan_Opr :
   case Verify_Opr :
   case Cshift_Opr :
   case Product_Opr :
   case Sum_Opr :
   case Minval_Opr :
   case Maxval_Opr :
   case Spread_Opr :
   case Eoshift_Opr :

	i = IR_IDX_L(ir_idx);
        cvrt_exp_to_mif(&opd0, blk, IL_IDX(i), IL_FLD(i), flags, value);
	i = IL_NEXT_LIST_IDX(i);
        cvrt_exp_to_mif(&opd1, blk, IL_IDX(i), IL_FLD(i), flags, value);
	i = IL_NEXT_LIST_IDX(i);
        cvrt_exp_to_mif(&opd2, blk, IL_IDX(i), IL_FLD(i), flags, value);

	switch (IR_OPR(ir_idx)) {
	case Nearest_Opr :
	     op = mop_nearest;
	     break;
	case Dshiftl_Opr :
             op = mop_lsh;
             break;
	case Dshiftr_Opr :
             op = mop_rsh;
             break;
	case Cvmgp_Opr :
	case Cvmgm_Opr :
	case Cvmgz_Opr :
	case Cvmgn_Opr :
	case Cvmgt_Opr :
             op = mop_pick;
             break;
	case Csmg_Opr :
             op = mop_mrg;
             break;
	case Index_Opr :
             op = mop_chindex;
             break;
	case Scan_Opr :
             op = mop_chscan;
             break;
	case Verify_Opr :
             op = mop_chver;
             break;
        case Cshift_Opr :
             op = mop_cshift;
             break;
        case Product_Opr :
             op = mop_product;
             break;
        case Sum_Opr :
             op = mop_sum;
             break;
        case Minval_Opr :
             op = mop_minval;
             break;
        case Maxval_Opr :
             op = mop_maxval;
             break;
        case Spread_Opr :
             op = mop_spread;
             break;
        case Eoshift_Opr :
             cvrt_exp_to_mif(&opd2, blk, i, IL_Tbl_Idx, flags, value);
             op = mop_eoshift;
             break;
        }

        *result = mif_opn_add(blk, op, basic, pos, flags, opd0, opd1, opd2);
        break;


   case Loc_Opr :
        cvrt_exp_to_mif(&opd0, 
                        blk,
	                IR_IDX_L(ir_idx), 
                        IR_FLD_L(ir_idx),
			flags, 
                        value);

        *result = mif_opn_add(blk,
                              mop_loc,
                              get_basic_type(IR_TYPE_IDX(ir_idx)),
                              mpos_null,
                              0,
                              opd0,
                              mopd_null,
                              mopd_null);
        break;


   case Const_Tmp_Loc_Opr :
        cvrt_exp_to_mif(&opd0, 
                        blk,
	                IR_IDX_L(ir_idx), 
                        IR_FLD_L(ir_idx),
			flags, 
                        value);

        *result = mif_opn_add(blk, 
                          mop_loc, 
                          get_ptr_type(IR_Tbl_Idx, ir_idx),
                          pos, 
                          flags,
                          opd0, 
                          mopd_null, 
                          mopd_null);
        break;


   case Aloc_Opr :
        processing_aloc = TRUE;
        cvrt_exp_to_mif(result, blk,
			IR_IDX_L(ir_idx), IR_FLD_L(ir_idx),
			flags, address);
        processing_aloc = FALSE;
	opd2 = mopd_0;
	opd2.val = maliasclass_restrict;
	type = msp.type[mopdtype(&msp, *result, blk->opn)];
	type.maddr.aliasing = maliasclass_restrict;
        *result = mif_opn_add(blk, mop_alias, 
                              mtype_lookup(&msp, &type), pos, flags,
                              *result, mopd_null, opd2);
        break;


   case Plus_Opr :
   case Minus_Opr :
   case Mult_Opr :
   case Div_Opr :
   case Real_Div_To_Int_Opr :
   case Power_Opr :
   case And_Opr :
   case Band_Opr :
   case Or_Opr :
   case Bor_Opr :
   case Neqv_Opr :       
   case Bneqv_Opr :
   case Eqv_Opr:
   case Beqv_Opr :       
   case Case_Range_Opr :
   case Lg_Opr :

        cvrt_exp_to_mif(&opd0, blk,
			IR_IDX_L(ir_idx), IR_FLD_L(ir_idx),
			flags, value);
        cvrt_exp_to_mif(&opd1, blk,
			IR_IDX_R(ir_idx), IR_FLD_R(ir_idx),
			flags, value);

	switch (IR_OPR(ir_idx)) { 
        case Plus_Opr :
	   op = mop_add;
	   break;
        case Minus_Opr :
	   op = mop_sub;
	   break;
        case Mult_Opr :
	   op = mop_mul;
	   break;
        case Div_Opr :
	   op = mop_div;
	   break;
	case Real_Div_To_Int_Opr :
	   op = mop_rdiv;
	   break;
        case Power_Opr :
	   op = mop_pow;
	   break;
	case And_Opr :
	case Band_Opr :
	   op = mop_and;
	   break;
	case Or_Opr :
	case Bor_Opr :
	   op = mop_or;
	   break;
	case Neqv_Opr :       
	case Bneqv_Opr :
	   op = mop_xor;
	   break;
	case Eqv_Opr :       
	case Beqv_Opr :
	   op = mop_eqv;
	   break;
        case Case_Range_Opr :
	   op = mop_range;
	   break;
        case Lg_Opr :
           op = mop_islg;
           break;
	}

        *result = mif_opn_add(blk, op, basic, pos, 
                              flags, opd0, opd1, mopd_null);
        break;


   /* Relations */
   case Eq_Opr : 
   case Ne_Opr : 
   case Lt_Opr : 
   case Le_Opr : 
   case Gt_Opr : 
   case Ge_Opr : 
   case Llt_Opr : 
   case Lle_Opr : 
   case Lgt_Opr : 
   case Lge_Opr : 

	if (IR_FLD_L(ir_idx) == IL_Tbl_Idx) {
	   i = IR_IDX_L(ir_idx);
           cvrt_exp_to_mif(&opd0, blk, IL_IDX(i), IL_FLD(i), flags, value);
	   i = IL_NEXT_LIST_IDX(i);
           cvrt_exp_to_mif(&opd1, blk, IL_IDX(i), IL_FLD(i), flags, value);
	}
	else {
           cvrt_exp_to_mif(&opd0, blk,
			   IR_IDX_L(ir_idx), IR_FLD_L(ir_idx),
			   flags, value);
           cvrt_exp_to_mif(&opd1, blk,
			   IR_IDX_R(ir_idx), IR_FLD_R(ir_idx),
			   flags, value);
	}

	opd2 = mopd_0;
	switch(IR_OPR(ir_idx)) {
	case Eq_Opr:
	   opd2.val = mrelation_EQ;
	   break;
	case Ne_Opr:
	   opd2.val = mrelation_LT | mrelation_GT;
	   break;
	case Lt_Opr : 
	case Llt_Opr : 
	   opd2.val = mrelation_LT;
	   break;
	case Le_Opr : 
	case Lle_Opr : 
	   opd2.val = mrelation_LT | mrelation_EQ;
	   break;
	case Gt_Opr : 
	case Lgt_Opr : 
	   opd2.val = mrelation_GT;
	   break;
	case Ge_Opr : 
	case Lge_Opr : 
	   opd2.val = mrelation_GT | mrelation_EQ;
	   break;
	}

        *result = mif_opn_add(blk, mop_cmp, basic, 
                              pos, flags, opd0, opd1, opd2);
        break;


   /* N-ary intrinsics */
   case Case_Cmic_Opr:        
   case Guard_Cmic_Opr:       
   case Doall_Cmic_Opr:       
   case Doparallel_Cmic_Opr:  
   case Parallel_Cmic_Opr:    

        opd0.tag = mtag_taskreg;
        opd0.val = start_task_region(blk, ir_idx, flags, pos);
        *result = mif_opn_add(blk, 
                          mop_tregbegin, 
                          get_basic_type(NONE), 
                          pos,
                          flags, 
                          opd0, 
                          mopd_null,  
                          mopd_null);
        break;


   case Max_Opr :
   case Min_Opr :
   case Concat_Opr :

	switch (IR_OPR(ir_idx)) {
	   case Max_Opr : 	      
                op = mop_max;	   
                break;

	   case Min_Opr :	      
                op = mop_min;	   
                break;

	   case Concat_Opr :	      
                op = mop_cat;	   
                break;

	}


        i = 0;

        for (temp_ir_idx = IR_IDX_L(ir_idx);
	     temp_ir_idx != NULL_IDX;
	     temp_ir_idx = IL_NEXT_LIST_IDX(temp_ir_idx)) {

           if (IL_IDX(temp_ir_idx) != NULL_IDX) {

	      if (!i++) {
                 /* First operand */
                 cvrt_exp_to_mif(result, blk,
			         IL_IDX(temp_ir_idx), IL_FLD(temp_ir_idx),
				 flags, value);
	      }
	      else {

                 cvrt_exp_to_mif(&opd1, blk,
			         IL_IDX(temp_ir_idx), IL_FLD(temp_ir_idx),
				 flags, value);

	         if (opd1.tag == mtag_sx &&
	             blk->opn[opd1.val].flags & mopnflag_array) {
	            flags |= mopnflag_array;
	         }

        	*result = mif_opn_add(blk, op, basic, pos, flags,
                               *result, opd1, mopd_null);
	      }
           }
        } 

        break;


   /* Assignment */
   case Asg_Opr :
   case Alt_Return_Opr :
   case Dv_Whole_Copy_Opr :

        if ((IR_FLD_L(ir_idx) == AT_Tbl_Idx) &&
            (AT_OBJ_CLASS(IR_IDX_L(ir_idx)) == Label)) {

           attr_idx = IR_IDX_L(ir_idx);
           mif_attr_map[attr_idx].val = cvrt_label(attr_idx, flags, pos);

           /* compute address of label */
           type = *mtype_null[mtypeclass_blkaddr];
           type.mblkaddr.size = mint(&msp,
                                     msp.immtype,
                                     (unsigned long)TARGET_BITS_PER_WORD);

           opd1 = mif_opn_add(blk, 
                              mop_loc, 
                              mtype_lookup(&msp, &type),
                              pos, 
                              flags,
                              mif_attr_map[attr_idx], 
                              mopd_null, 
                              mopd_null);

           /* store it */
           op = mop_asg;
           opd0 = mif_attr_map[IR_IDX_R(ir_idx)];
        }
	else {

	   /* RHS */
           cvrt_exp_to_mif(&opd1, 
                           blk,
			   IR_IDX_R(ir_idx), 
                           IR_FLD_R(ir_idx),
			   flags, 
                           value);

	   if (IR_FLD_L(ir_idx) == AT_Tbl_Idx &&
               AT_OBJ_CLASS(IR_IDX_L(ir_idx)) == Data_Obj &&
               ATD_CLASS(IR_IDX_L(ir_idx)) != Dummy_Argument) {
              /* direct assignment */
	      opd0 = mif_attr_map[IR_IDX_L(ir_idx)];
	      op = mop_asg;
	   }
	   else {
              cvrt_exp_to_mif(&opd0, 
                              blk,
			      IR_IDX_L(ir_idx), 
                              IR_FLD_L(ir_idx),
			      flags, 
                              address);
	      op = mop_st;
	   }

        }

        mif_opn_add(blk, 
                op, 
                get_basic_type(NONE),     
                pos, 
                flags,
	        opd0, 
                opd1, 
                mopd_null);
        break;




   case Flat_Array_Asg_Opr : 
        /* RHS */
        cvrt_exp_to_mif(&opd1,
                        blk,
                        IR_IDX_R(ir_idx),
                        IR_FLD_R(ir_idx),
                        flags,
                        value);

        cvrt_exp_to_mif(&opd0,
                        blk,
                        IR_IDX_L(ir_idx),
                        IR_FLD_L(ir_idx),
                        flags,
                        address);
        op = mop_constr;

        mif_opn_add(blk, 
                op, 
                get_basic_type(NONE),
                pos, 
                flags,
                opd0, 
                opd1, 
                mopd_null);
        break;






   case Dv_Def_Asg_Opr :
        cvrt_exp_to_mif(&opd0, 
                        blk,
                        IR_IDX_L(ir_idx), 
                        IR_FLD_L(ir_idx),
                        flags, 
                        address);

        cvrt_exp_to_mif(&opd1, 
                        blk,
                        IR_IDX_L(IR_IDX_R(ir_idx)), 
                        IR_FLD_L(IR_IDX_R(ir_idx)),
                        flags, 
                        value);

        mif_opn_add(blk, 
                mop_dvdef, 
                get_basic_type(NONE), 
                pos, 
                flags,
                opd0, 
                opd1, 
                mopd_null);
        break;




   case Ptr_Asg_Opr :

	/* RHS */
        cvrt_exp_to_mif(&opd1, blk,
			IR_IDX_R(ir_idx), 
                        IR_FLD_R(ir_idx),
			flags, 
                        address);

	/* LHS */
        cvrt_exp_to_mif(&opd0, 
                        blk,
	                IR_IDX_L(ir_idx), 
                        IR_FLD_L(ir_idx),
			flags, 
                        value);

        attr_idx = find_base_attr(&IR_OPND_R(ir_idx), &unused1, &unused2);

        opd0 = mif_opn_add(blk, 
                           mop_dvfield, 
                           get_ptr_type(AT_Tbl_Idx, attr_idx), 
                           pos,
		           flags, 
                           opd0, 
                           mopd_1, 
                           mopd_null); /* field 1: base address */

        mif_opn_add(blk, 
                    mop_st, 
                    get_basic_type(NONE),
	            pos, 
                    flags, 
                    opd0, 
                    opd1, 
                    mopd_null);
        break;


   case Where_Opr :

	/* LHS */
	i = IR_IDX_L(ir_idx);
        cvrt_exp_to_mif(&opd0, 
                        blk,
			IL_IDX(i), 
                        IL_FLD(i),
			flags, 
                        address);

	/* Mask */
	i = IL_NEXT_LIST_IDX(i);
        cvrt_exp_to_mif(&opd2, 
                        blk,
			IL_IDX(i), 
                        IL_FLD(i),
			flags, 
                        value);

        opd0 = mif_opn_add(blk, mop_where, mopdtype(&msp, opd0,
						blk->opn), pos,
                    flags | mopnflag_distinct,
                    opd0, opd2, mopd_null);

	/* RHS */
	i = IL_NEXT_LIST_IDX(i);
        cvrt_exp_to_mif(&opd1, blk,
			IL_IDX(i), IL_FLD(i),
			flags, value);

	/* Explicit conversion */
	if (VALID (basic) &&
	    mopdtype(&msp, opd1, blk->opn) != basic &&
	    msp.type[basic].u.class != mtypeclass_fchar) {
           opd1 = mif_opn_add(blk, mop_cast, basic, pos, flags,
                       opd1, mopd_null, mopd_null);
	}

        mif_opn_add(blk, 
                mop_st, 
                get_basic_type(NONE),
                pos, 
                flags,
	        opd0, 
                opd1, 
                mopd_null);
        break;



   case Read_Formatted_Opr :
   case Write_Formatted_Opr :
   case Read_Unformatted_Opr :
   case Write_Unformatted_Opr :
   case Read_Namelist_Opr :
   case Write_Namelist_Opr :

	/* control list */
        cvrt_exp_to_mif(&opd0, 
                        blk,
			IR_IDX_L(ir_idx), 
                        IR_FLD_L(ir_idx),
		        flags, 
                        value);

	/* I/O list */
        cvrt_exp_to_mif(&opd1, 
                        blk,
			IR_IDX_R(ir_idx), 
                        IR_FLD_R(ir_idx),
			flags, 
                        value);

        opd2 = mopd_0;

	switch(IR_OPR(ir_idx)) {
	case Read_Formatted_Opr :
        case Read_Unformatted_Opr :
	case Read_Namelist_Opr :
	   opd2.val |= mIOflag_read;
	   break;
	case Write_Formatted_Opr :
        case Write_Unformatted_Opr :
	case Write_Namelist_Opr :
	   opd2.val |= mIOflag_write;
	   break;
	}
	switch(IR_OPR(ir_idx)) {
	case Read_Formatted_Opr :
	case Write_Formatted_Opr :
	   opd2.val |= mIOflag_formatted;
	   break;
        case Read_Unformatted_Opr :
        case Write_Unformatted_Opr :
	   opd2.val |= mIOflag_unformatted;
	   break;
	case Read_Namelist_Opr :
	case Write_Namelist_Opr :
	   opd2.val |= mIOflag_namelist;
	   break;
	}

        *result = mif_opn_add(blk, 
                          mop_IO, 
                          msp.immtype,
			  pos, 
                          flags, 
                          opd0, 
                          opd1, 
                          opd2);
        break;


   case Inquire_Iolength_Opr:
        /* length */
        cvrt_exp_to_mif(&opd0, 
                        blk,
                        IR_IDX_L(ir_idx), 
                        IR_FLD_L(ir_idx),
                        flags, 
                        value);

        /* I/O list */
        cvrt_exp_to_mif(&opd1, 
                        blk,
                        IR_IDX_R(ir_idx), 
                        IR_FLD_R(ir_idx),
                        flags, 
                        value);

        *result = mif_opn_add(blk, 
                          mop_IOlength, basic,
			  pos, 
                          flags, 
                          opd0, 
                          opd1, 
                          mopd_null);
        break;

   case Implied_Do_Opr :

	/* This code is simplier than functional interface code because
         * Init_Opr does much of the work.  */

	j = IL_NEXT_LIST_IDX(IR_IDX_R(ir_idx));
	cvrt_exp_to_mif(&opd0, blk, IL_IDX(j), IL_FLD(j), flags, value);
	j = IL_NEXT_LIST_IDX(j);
	cvrt_exp_to_mif(&opd1, blk, IL_IDX(j), IL_FLD(j), flags, value);
	j = IL_NEXT_LIST_IDX(j);
	cvrt_exp_to_mif(&opd2, blk, IL_IDX(j), IL_FLD(j), flags, value);

	/* Turn on flag to do array syntax expansion. */
        opd2 = mif_opn_add(blk, mop_triplet, msp.immtype, pos,
                    flags | mopnflag_array,
                    opd0, opd1, opd2);

	/* I/O list */
        cvrt_exp_to_mif(&opd0, 
                        blk,
			IR_IDX_L(ir_idx), 
                        IR_FLD_L(ir_idx),
			flags, 
                        value);

	/* Control variable */
	cvrt_exp_to_mif(&opd1, 
                        blk,
			IL_IDX(IR_IDX_R(ir_idx)), 
                        IL_FLD(IR_IDX_R(ir_idx)),
			flags, 
                        value);

        *result = mif_opn_add(blk, mop_implDO, msp.immtype, pos,
                       flags | mopnflag_array,
                       opd0, opd1, opd2);
        break;


   case Call_Opr :

        if (ATP_PROC(IR_IDX_L(ir_idx)) == Dummy_Proc) {
           if (TYP_TYPE(IR_TYPE_IDX(ir_idx)) == Typeless) {
	      basic = get_basic_type(NONE);
           }
        }
	else if (ATP_HAS_ALT_RETURN(IR_IDX_L(ir_idx))) {
	   basic = msp.immtype;
        }
	else if (ATP_PGM_UNIT(IR_IDX_L(ir_idx)) == Subroutine ||
	         ATP_EXTRA_DARG(IR_IDX_L(ir_idx)) ||
		 INVALID (basic)) {
	   basic = get_basic_type(NONE);
	}

	/* Destination */
        cvrt_exp_to_mif(&opd0, 
                        blk,
			IR_IDX_L(ir_idx), 
                        IR_FLD_L(ir_idx),
			flags, 
                        value);

	/* Args */
        if (IR_IDX_R(ir_idx) != NULL_IDX) {
           cvrt_exp_to_mif(&opd1, blk,
			   IR_IDX_R(ir_idx), 
                           IR_FLD_R(ir_idx),
			   flags, 
                           value);
        }
	else {
	   opd1 = mopd_null;
	}

        if (cdir_switches.do_inline) {

           /* Turn on inlining if !DIR$ INLINE is specified or if inline */
           /* level is Inline_Lvl_2 or Inline_Lvl_3.  Do not turn it on  */
           /* if !DIR$ INLINE NEVER is specified for the callee.         */

           if (!ATP_INLINE_NEVER(IR_IDX_L(ir_idx))) {
              flags |= mopnflag_inline;
           }
        }
        else if (opt_flags.inline_lvl > 0 &&
                 ATP_INLINE_ALWAYS(IR_IDX_L(ir_idx))) {
           flags |= mopnflag_inline;
        }
        else if (cdir_switches.noinline || opt_flags.inline_lvl == 0) {
           flags |= mopnflag_noinline;
        }

        *result = mif_opn_add(blk, 
                              mop_call, 
                              basic, pos, flags,
                              opd0, opd1, mopd_null);
        break;


   case Whole_Subscript_Opr :
   case Section_Subscript_Opr :
   case Subscript_Opr :

        base_attr = find_left_attr(&(IR_OPND_L(ir_idx)));

        bound_chk = (cdir_switches.bounds ||
                     ATD_BOUNDS_CHECK(base_attr)) &&
                    ! ATD_NOBOUNDS_CHECK(base_attr);

	/* base variable */
        cvrt_exp_to_mif(&opd0, blk,
			IR_IDX_L(ir_idx), 
                        IR_FLD_L(ir_idx),
			flags, address);

	opd1 = mopd_null;

	/* i_cvrt.c contains code to test IR_CONTIG_ARRAY and omit the  */
	/* emission of the subscripts if the reference is a whole array */
	/* reference to an array with contiguous storage. To ease the   */
	/* job of array syntax translation, we'll always use explicit   */
	/* triplets in the textual interface instead.                   */
	for (next_idx = IR_IDX_R(ir_idx);
	     next_idx != NULL_IDX;
	     next_idx = IL_NEXT_LIST_IDX(next_idx)) {

	   cvrt_exp_to_mif(&opd, blk,
			   IL_IDX(next_idx), 
                           IL_FLD(next_idx),
			   flags, value);

           opd1 = mif_opn_add(blk, 
                          mop_list, 
                          mopdtype(&msp, opd, blk->opn),
                          pos, 
                          flags,
                          opd, 
                          opd1, 
                          mopd_null);
        }

        save_flags = flags;

        if (bound_chk) {
           flags |= mopnflag_validate;
        }

        *result = mif_opn_add(blk, 
                              mop_index, 
                              get_ptr_type(IR_Tbl_Idx, ir_idx),
                              pos, 
                              flags,
                              opd0, 
                              opd1, 
                              mopd_null);

        /* Explicit loads */
        if (context == value) {

           flags = save_flags;

           *result = mif_opn_add(blk, 
                             mop_ld, 
                             basic, 
                             pos, 
                             flags,
                             *result, 
                             mopd_null, 
                             mopd_null);
        }

	break;


   case Triplet_Opr :

	i = IR_IDX_L(ir_idx);	/* list of (start, limit, stride) */
	cvrt_exp_to_mif(&opd0, blk, IL_IDX(i), IL_FLD(i), flags, value);
	i = IL_NEXT_LIST_IDX(i);
	cvrt_exp_to_mif(&opd1, blk, IL_IDX(i), IL_FLD(i), flags, value);
	i = IL_NEXT_LIST_IDX(i);
	cvrt_exp_to_mif(&opd2, blk, IL_IDX(i), IL_FLD(i), flags, value);

	/* Turn on flag to do array syntax expansion. */
        *result = mif_opn_add(blk, 
                          mop_triplet, 
                          basic, 
                          pos,
                          flags | mopnflag_array,
                          opd0, 
                          opd1, 
                          opd2);
	break;


   case Whole_Substring_Opr :
   case Substring_Opr :

        base_attr = find_left_attr(&(IR_OPND_L(ir_idx)));

        bound_chk = cmd_line_flags.runtime_substring;

	/* Base variable */
	cvrt_exp_to_mif(&opd0, blk,
			IR_IDX_L(ir_idx), 
                        IR_FLD_L(ir_idx),
			flags, address);

	/* get starting index */
	i = IR_IDX_R(ir_idx);	/* list of (start index, end index, length) */
	cvrt_exp_to_mif(&opd1, blk, IL_IDX(i), IL_FLD(i), flags, value);

        /* get the length */
	i = IL_NEXT_LIST_IDX(IL_NEXT_LIST_IDX(i));
	cvrt_exp_to_mif(&opd2, blk, IL_IDX(i), IL_FLD(i), flags, value);

        save_flags = flags;

        if (bound_chk) {
           flags |= mopnflag_validate;
        }

        *result = mif_opn_add(blk, mop_substr, get_ptr_type(IR_Tbl_Idx, ir_idx),
                       pos, flags & ~mopnflag_array,
                       opd0, opd1, opd2);

        /* Explicit loads */
        if (context == value) {

           flags = save_flags;

           *result = mif_opn_add(blk, 
                             mop_ld, 
                             basic, 
                             pos, 
                             flags,
                             *result, 
                             mopd_null, 
                             mopd_null);
        }

	break;


   case Struct_Opr :

	/* base variable */
        cvrt_exp_to_mif(&opd0, blk,
			IR_IDX_L(ir_idx), IR_FLD_L(ir_idx),
			flags, address);

	/* component */
	if (IR_FLD_R(ir_idx) == AT_Tbl_Idx &&
	    AT_OBJ_CLASS(IR_IDX_R(ir_idx)) == Data_Obj) {
	   opd1 = mif_attr_map[IR_IDX_R(ir_idx)];
	   typeix = get_ptr_type(AT_Tbl_Idx, IR_IDX_R(ir_idx));
	}
        else {
           cvrt_exp_to_mif(&opd1, blk,
			   IR_IDX_R(ir_idx), 
                           IR_FLD_R(ir_idx),
			   flags, 
                           value);

	   typeix = get_ptr_type(IR_Tbl_Idx, ir_idx);
	}

	*result = mif_opn_add(blk, 
                          mop_field, 
                          typeix, pos, flags,
                          opd0, 
                          opd1, 
                          mopd_null);

        /* Explicit load */
        if (context==value) {
	   /* result type is type of field member */
           *result = mif_opn_add(blk, mop_ld, 
                                 msp.type[typeix].u.base, pos, flags,
                                 *result, mopd_null, mopd_null);
        }
	break;


   /* Dope vector operations */

   case Dv_Deref_Opr :
        cvrt_exp_to_mif(result, 
                        blk,
			IR_IDX_L(ir_idx), 
                        IR_FLD_L(ir_idx),
			flags, 
                        value);

	/* If value is needed, dereference dope vector via a mop_ld.          */
	/* Note: mop_ld has a dope vector instead of an address as an operand */
	/* Result type is the basic type referenced by the dope vector.       */
        if (context == value) {
           *result = mif_opn_add(blk, 
                             mop_ld, 
                             basic, 
                             pos, 
                             flags,
                             *result, 
                             mopd_null, 
                             mopd_null);
	}
	break;


   case Dv_Access_Base_Addr :
   case Dv_Set_Base_Addr :
   case Dv_Access_El_Len :
   case Dv_Set_El_Len :
   case Dv_Access_N_Dim :
   case Dv_Set_N_Dim :
   case Dv_Access_Assoc :
   case Dv_Set_Assoc :
   case Dv_Access_Ptr_Alloc :
   case Dv_Set_Ptr_Alloc :
   case Dv_Access_P_Or_A :
   case Dv_Set_P_Or_A :
   case Dv_Access_A_Contig :
   case Dv_Set_A_Contig :
   case Dv_Access_Typ_Code :
   case Dv_Set_Typ_Code :
   case Dv_Access_Orig_Base :
   case Dv_Set_Orig_Base :
   case Dv_Access_Orig_Size :
   case Dv_Set_Orig_Size :
   case Dv_Access_Low_Bound :
   case Dv_Set_Low_Bound :
   case Dv_Access_Extent :
   case Dv_Set_Extent :
   case Dv_Access_Stride_Mult :
   case Dv_Set_Stride_Mult :

        cvrt_exp_to_mif(&opd0, 
                        blk,
			IR_IDX_L(ir_idx), 
                        IR_FLD_L(ir_idx),
			flags, 
                        value);

	/* First operand is field being accessed. */
	opd1 = mopd_0;
	switch (IR_OPR(ir_idx)) {
	   /* Has no dimension operand */
	   case Dv_Set_Base_Addr : /* BECKER - overloading with Ptr_Asg_Opr
				    ** even though i_cvrt.c used different
				    ** interface */
	   case Dv_Access_Base_Addr :	opd1.val = 1;	break;
	   case Dv_Set_El_Len :
	   case Dv_Access_El_Len :	opd1.val = 2;	break;
	   case Dv_Set_Assoc :
	   case Dv_Access_Assoc :	opd1.val = 3;	break;
	   case Dv_Set_Ptr_Alloc :
	   case Dv_Access_Ptr_Alloc :	opd1.val = 4;	break;
	   case Dv_Set_P_Or_A :
	   case Dv_Access_P_Or_A :	opd1.val = 5;	break;
	   case Dv_Set_A_Contig :
	   case Dv_Access_A_Contig :	opd1.val = 6;	break;
	   case Dv_Set_N_Dim :
	   case Dv_Access_N_Dim :	opd1.val = 7;	break;
	   case Dv_Set_Typ_Code :
	   case Dv_Access_Typ_Code :	opd1.val = 8;	break;
	   case Dv_Set_Orig_Base :
	   case Dv_Access_Orig_Base :	opd1.val = 9;	break;
	   case Dv_Set_Orig_Size :
	   case Dv_Access_Orig_Size :	opd1.val = 10;	break;

	   /* Has dimension operand */
	   case Dv_Set_Low_Bound :
	   case Dv_Access_Low_Bound :	opd1.val = 0;	break;
	   case Dv_Set_Extent :
	   case Dv_Access_Extent :	opd1.val = 1;	break;
	   case Dv_Set_Stride_Mult :
	   case Dv_Access_Stride_Mult :	opd1.val = 2;	break;
	}

	/* Second operand is dimension being accessed. */
	opd2 = mopd_null;
	switch (IR_OPR(ir_idx)) {
	   case Dv_Set_Low_Bound :
	   case Dv_Access_Low_Bound :
	   case Dv_Set_Extent :
	   case Dv_Access_Extent :
	   case Dv_Set_Stride_Mult :
	   case Dv_Access_Stride_Mult :
              opd2 = mopd_0;
              opd2.val = IR_DV_DIM(ir_idx);
	}

        opd0 = mif_opn_add(blk, 
                       mop_dvfield, 
                       get_ptr_type(IR_Tbl_Idx, ir_idx),
		       pos, 
                       flags, 
                       opd0, 
                       opd1, 
                       opd2);

	/* Create a load or store of field reference. */
	opd1 = mopd_null;
	switch (IR_OPR(ir_idx)) {
	   case Dv_Set_Base_Addr :
	   case Dv_Set_El_Len :
	   case Dv_Set_Assoc :
	   case Dv_Set_Ptr_Alloc :
	   case Dv_Set_P_Or_A :
	   case Dv_Set_A_Contig :
	   case Dv_Set_N_Dim :
	   case Dv_Set_Typ_Code :
	   case Dv_Set_Orig_Base :
	   case Dv_Set_Orig_Size :
	   case Dv_Set_Low_Bound :
	   case Dv_Set_Extent :
	   case Dv_Set_Stride_Mult :
              cvrt_exp_to_mif(&opd1, 
                              blk,
			      IR_IDX_R(ir_idx), 
                              IR_FLD_R(ir_idx),
			      flags, 
                              value);
	      /* Set a field */
	      op = mop_st;
	      basic = get_basic_type(NONE);
	      break;

	   default :
	      /* Load a field */
	      op = mop_ld;
	}

        *result = mif_opn_add(blk, op, basic, pos, 
                              flags, opd0, opd1, mopd_null);
	break;


#if ! defined _ALLOCATE_IS_CALL
   case Allocate_Opr:
   case Deallocate_Opr:
	cvrt_exp_to_mif(&opd0, blk, IR_IDX_R(ir_idx), IR_FLD_R(ir_idx),
                        flags, 
                        value);

	cvrt_exp_to_mif(&opd1, blk, IR_IDX_L(ir_idx), IR_FLD_L(ir_idx),
                        flags, 
                        value);

        *result = mif_opn_add(blk, 
                          mop_allocate, 
	                  get_basic_type(NONE),
                          pos,
			  flags, 
                          opd0, 
                          opd1, 
                          mopd_null);
	break;
#endif

   case SSD_Alloc_Opr :
   case Alloc_Opr :

        cvrt_exp_to_mif(&opd0, blk,
			IR_IDX_L(ir_idx), 
                        IR_FLD_L(ir_idx),
			flags, 
                        value);

        /* convert words to bytes */
        opd1 = mopd_0;
# ifdef _HEAP_REQUEST_IN_WORDS
        opd1.val = 8;  /* convert to bytes */
# else
        opd1.val = 1;  /* leave as bytes */
# endif
        opd0 = mif_opn_add(blk, 
                           mop_mul, 
                           msp.immtype, 
                           pos, 
                           flags,
                           opd0, 
                           opd1, 
                           mopd_null);


	op = IR_OPR(ir_idx) == SSD_Alloc_Opr ? mop_SSD : mop_alloc;

	/* Have to create pointer type of result because */
	/* frontend treats result of alloc as int.       */
        *result = mif_opn_add(blk, 
                              op, 
                              get_ptr_type(IR_Tbl_Idx, ir_idx), 
                              pos, flags,
                              opd0, 
                              mopd_null, 
                              mopd_null);
	break;


   case Dealloc_Opr :
   case SSD_Dealloc_Opr :
        cvrt_exp_to_mif(&opd0, 
                        blk,
			IR_IDX_L(ir_idx), 
                        IR_FLD_L(ir_idx),
			flags, 
                        value);

	/* Cast to pointer type, frontend keeps address in int variable. */
        opd0 = mif_opn_add(blk, 
                           mop_cast,
                           get_ptr_type(IR_Tbl_Idx, ir_idx),
		           pos,  
                           flags, 
                           opd0, 
                           mopd_null, 
                           mopd_null);

	op = IR_OPR(ir_idx) == SSD_Dealloc_Opr ? mop_SSDfree : mop_free;

        *result = mif_opn_add(blk, 
                              op, 
	                      get_basic_type(NONE),
                              pos, 
                              flags,
                              opd0, 
                              mopd_null, 
                              mopd_null);
	break;

   default:
	PRINTMSG(IR_LINE_NUM(ir_idx), 
                 1044, 
                 Internal, 
                 IR_COL_NUM(ir_idx),
		 "cvrt_exp_to_mif: unexpected operator");
        break;

   }
   break;

}
    
TRACE (Func_Exit, "cvrt_exp_to_mif", NULL);
}  /* cvrt_exp_to_mif */



/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Translate the code of a scope into blocks and arcs.		      *|
|*									      *|
|* Input parameters:							      *|
|*	Subprogram							      *|
|*	Scope								      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/
static void	cvrt_ir_to_mif(int	scp_idx)
{
   int			 attr_idx;
   int			 baseattr;
   int			 basic;
   int			 blkix        	= NONE;
   int			 case_ct;
   size_offset_type	 char_bit;
   int			 cn_idx;
   int			 curr_sh;
   mopnflag_t   	 flags;
   int			 fldattr;
   int			 i, j;
   int          	 idx;
   int			 initix;
   int			 ir_idx;
   int          	 l_idx;
   int			 lastblkix;
   int          	 lcv;
   int			 length;
   int          	 list_idx1;
   int          	 list_idx2;
   int			 loc_offset_idx;
   int			 lt0, eq0, gt0;
   int			 nested_case_ct;
   mopn_t		*o;
   long          	 offset;
   size_offset_type	 offset1;
   mop_t		 op;
   mopd_t		 opd, opd0, opd1, opd2;
   opnd_type    	 opnd;
   int			 opnix;
   mpos_t		 pos;
   size_offset_type	 result;
   long			 t;
   int			 tmp_sh;
   mtype_t		 typ;
   int			 typeix;
   boolean      	 unused;


   TRACE (Func_Entry, "cvrt_ir_to_mif", NULL);

   /* process each statement of function */
   for (curr_sh = SCP_FIRST_SH_IDX(scp_idx);
        curr_sh != NULL_IDX;
        curr_sh = SH_NEXT_IDX(curr_sh)) {

      /* Determine the source code position of the operation. */
      pos = mpos_null;
      pos.line = source_position(SH_GLB_LINE(curr_sh));
      pos.src = srcix;
      pos.col = SH_COL_NUM(curr_sh);

      msp.scope[local_scope].end.line = pos.line;
      msp.scope[local_scope].end.src = pos.src;

      if (VALID(host_scope)) {
         msp.scope[host_scope].end.line = pos.line;
         msp.scope[host_scope].end.src = pos.src;
      }

      flags = 0;
      if (SH_COMPILER_GEN(curr_sh)) {
         flags |= mopnflag_syn;	/* mark compiler-generated operations */
      }

      ir_idx = SH_IR_IDX(curr_sh);
      if (ir_idx != NULL_IDX) {

         switch(IR_OPR(ir_idx)) {

	 /* Operators that introduce control flow or generate no code. */
	 case Vector_Cdir_Opr :
	 case Novector_Cdir_Opr :
         case Ivdep_Cdir_Opr :
         case Unroll_Cdir_Opr :
         case Nounroll_Cdir_Opr :
         case Vsearch_Cdir_Opr :
         case Novsearch_Cdir_Opr :
         case Recurrence_Cdir_Opr :
         case Norecurrence_Cdir_Opr :
         case Cachealign_Cdir_Opr :
         case Align_Cdir_Opr :
         case Nextscalar_Cdir_Opr :
         case Shortloop128_Cdir_Opr :
         case Shortloop_Cdir_Opr: 
         case Task_Cdir_Opr: 
         case Notask_Cdir_Opr: 
         case Prefervector_Cdir_Opr: 
         case Prefertask_Cdir_Opr: 
         case Bl_Cdir_Opr: 
         case Nobl_Cdir_Opr: 
         case Permutation_Cmic_Opr:
         case Cncall_Cmic_Opr:
         case Maxcpus_Cmic_Opr:
            break; /* ignore */

       
         case Inline_Cdir_Opr :
            cdir_switches.do_inline = TRUE;
            cdir_switches.noinline  = FALSE;
            break;

         case Noinline_Cdir_Opr :
            cdir_switches.do_inline = FALSE;
            cdir_switches.noinline  = TRUE;
            break;

         case Bounds_Cdir_Opr :
            if (IR_FLD_L(ir_idx) == IL_Tbl_Idx) {
               list_idx1 = IR_IDX_L(ir_idx);
      
               while (list_idx1) {
                  attr_idx = IL_IDX(list_idx1);
      
                  /* if ATD_NOBOUNDS_CHECK set,  */
                  /* clear and remove from nobounds list */
      
                  if (ATD_NOBOUNDS_CHECK(attr_idx)) {
                     ATD_NOBOUNDS_CHECK(attr_idx) = FALSE;
                     list_idx2 = cdir_switches.nobounds_il_list;
      
                     while (list_idx2 != NULL_IDX) {
                        if (IL_IDX(list_idx2) == attr_idx) {
                           /* remove the attr from the list */
      
                           if (list_idx2 == cdir_switches.nobounds_il_list) {
                              cdir_switches.nobounds_il_list =
                                                   IL_NEXT_LIST_IDX(list_idx2);
                              if (cdir_switches.nobounds_il_list) {
                                 IL_PREV_LIST_IDX(
                                         cdir_switches.nobounds_il_list) =
                                                                     NULL_IDX;
                              }
                           }
                           else {
                              IL_NEXT_LIST_IDX(IL_PREV_LIST_IDX(list_idx2)) =
                                 IL_NEXT_LIST_IDX(list_idx2);
                              if (IL_NEXT_LIST_IDX(list_idx2)) {
                                 IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx2)) =
                                             IL_PREV_LIST_IDX(list_idx2);
                              }
                           }
                           FREE_IR_LIST_NODE(list_idx2);
      
                           break;
                        }
                        list_idx2 = IL_NEXT_LIST_IDX(list_idx2);
                     }
                  }
      
                  /* now add to bounds list if not already there */
      
                  if (ATD_BOUNDS_CHECK(attr_idx) == FALSE) {
                     ATD_BOUNDS_CHECK(attr_idx) = TRUE;
      
                     NTR_IR_LIST_TBL(list_idx2);
                     IL_FLD(list_idx2) = AT_Tbl_Idx;
                     IL_IDX(list_idx2) = attr_idx;
      
                     IL_NEXT_LIST_IDX(list_idx2) = cdir_switches.bounds_il_list;
                     IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx2)) = list_idx2;
                     cdir_switches.bounds_il_list = list_idx2;
                  }
      
                  list_idx1 = IL_NEXT_LIST_IDX(list_idx1);
               }
            }
            else {
               cdir_switches.bounds = TRUE;
      
               /* clear the NOBOUNDS flag on all attrs in the nobounds list */
      
               list_idx1 = cdir_switches.nobounds_il_list;
               cdir_switches.nobounds_il_list = NULL_IDX;
      
               while (list_idx1) {
                  attr_idx = IL_IDX(list_idx1);
                  ATD_NOBOUNDS_CHECK(attr_idx) = FALSE;
      
                  list_idx2 = list_idx1;
                  list_idx1 = IL_NEXT_LIST_IDX(list_idx1);
                  FREE_IR_LIST_NODE(list_idx2);
               }
            }
            break;
      
         case Nobounds_Cdir_Opr :
      
            if (IR_FLD_L(ir_idx) == IL_Tbl_Idx) {
               list_idx1 = IR_IDX_L(ir_idx);
      
               while (list_idx1) {
                  attr_idx = IL_IDX(list_idx1);
      
                  /* if ATD_BOUNDS_CHECK set, */
                  /* clear and remove from bounds list */
      
                  if (ATD_BOUNDS_CHECK(attr_idx)) {
                     ATD_BOUNDS_CHECK(attr_idx) = FALSE;
                     list_idx2 = cdir_switches.bounds_il_list;
      
                     while (list_idx2 != NULL_IDX) {
                        if (IL_IDX(list_idx2) == attr_idx) {
                           /* remove the attr from the list */
      
                           if (list_idx2 == cdir_switches.bounds_il_list) {
                              cdir_switches.bounds_il_list =
                                                   IL_NEXT_LIST_IDX(list_idx2);
                              if (cdir_switches.bounds_il_list) {
                                 IL_PREV_LIST_IDX(
                                           cdir_switches.bounds_il_list) =
                                                                     NULL_IDX;
                              }
                           }
                           else {
                              IL_NEXT_LIST_IDX(IL_PREV_LIST_IDX(list_idx2)) =
                                 IL_NEXT_LIST_IDX(list_idx2);
                              if (IL_NEXT_LIST_IDX(list_idx2)) {
                                 IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx2)) =
                                             IL_PREV_LIST_IDX(list_idx2);
                              }
                           }
                           FREE_IR_LIST_NODE(list_idx2);
      
                           break;
                        }
                        list_idx2 = IL_NEXT_LIST_IDX(list_idx2);
                     }
                  }
      
                  /* now add to nobounds list if not already there */
      
                  if (ATD_NOBOUNDS_CHECK(attr_idx) == FALSE) {
                     ATD_NOBOUNDS_CHECK(attr_idx) = TRUE;
      
                     NTR_IR_LIST_TBL(list_idx2);
                     IL_FLD(list_idx2) = AT_Tbl_Idx;
                     IL_IDX(list_idx2) = attr_idx;
      
                     IL_NEXT_LIST_IDX(list_idx2) = 
                                     cdir_switches.nobounds_il_list;
                     IL_PREV_LIST_IDX(IL_NEXT_LIST_IDX(list_idx2)) = list_idx2;
                     cdir_switches.nobounds_il_list = list_idx2;
                  }
      
                  list_idx1 = IL_NEXT_LIST_IDX(list_idx1);
               }
            }
            else {
               cdir_switches.bounds = FALSE;
      
               /* clear the BOUNDS flag on all attrs in the nobounds list */
      
               list_idx1 = cdir_switches.bounds_il_list;
               cdir_switches.bounds_il_list = NULL_IDX;

               while (list_idx1) {
                  attr_idx = IL_IDX(list_idx1);
                  ATD_BOUNDS_CHECK(attr_idx) = FALSE;
      
                  list_idx2 = list_idx1;
                  list_idx1 = IL_NEXT_LIST_IDX(list_idx1);
                  FREE_IR_LIST_NODE(list_idx2);
               }
            }
            break;


	 case Entry_Opr :
            lastblkix = blkix;
	    blkix = mifalloc[mtag_blk](&msp);
	    msp.blk[blkix].pos = pos;
	    msp.blk[blkix].scope = local_scope;
	    if (VALID(lastblkix)) {
	       mflow_local(&msp, lastblkix, blkix);
	    }

            mif_opn_add(&msp.blk[blkix],
	 	    mop_entry, 
	            get_basic_type(NONE),
                    pos, 
                    flags,
	            mif_attr_map[IR_IDX_L(ir_idx)], 
                    mopd_null, 
                    mopd_null);
	    break;



         case Return_Opr :
            if (INVALID(blkix)) {
               blkix = mifalloc[mtag_blk](&msp);
               msp.blk[blkix].pos = pos;
               msp.blk[blkix].scope = local_scope;
            }

            cvrt_exp_to_mif(&opd1, &msp.blk[blkix],
                            IR_IDX_L(ir_idx), IR_FLD_L(ir_idx),
                            flags, value); /* alt */

            cvrt_exp_to_mif(&opd0, &msp.blk[blkix],
                            IR_IDX_R(ir_idx), IR_FLD_R(ir_idx),
                            flags, value); /* value */

            mif_opn_add(&msp.blk[blkix],
                        mop_return,
                        get_basic_type(NONE),
                        pos,
                        flags,
                        opd0,
                        opd1,
                        mopd_null);

            blkix = NONE;
            break;



         case Label_Opr :
            attr_idx = IR_IDX_L(ir_idx);
            if (ATL_CLASS(attr_idx) == Lbl_Format ||
                ATL_CLASS(attr_idx) <= Lbl_User &&
                !ATL_EXECUTABLE(attr_idx)) {
               break;
            }

	    mif_attr_map[attr_idx].val = cvrt_label(attr_idx, flags, pos);
            lastblkix = blkix;
            blkix = mif_attr_map[attr_idx].val;
	    msp.blk[blkix].pos = pos;
	    if (VALID(lastblkix)) {
	       mflow_local(&msp, lastblkix, blkix);
	    }
	    break;


	 case Suppress_Opr :
            attr_idx = IR_IDX_R(ir_idx);
	    mif_attr_map[attr_idx].val = cvrt_label(attr_idx, flags, pos);
            lastblkix = blkix;
            blkix = mif_attr_map[attr_idx].val;
	    msp.blk[blkix].pos = pos;
	    if (VALID(lastblkix)) {
	       mflow_local(&msp, lastblkix, blkix);
	    }

	    /* Create as operands all variables referenced */
	    cvrt_exp_to_mif(&opd0, 
                            &msp.blk[blkix],
			    IR_IDX_L(ir_idx), 
                            IR_FLD_L(ir_idx),
			    flags, 
                            value);

            mif_opn_add(&msp.blk[blkix], 
                    mop_supp, 
	            get_basic_type(NONE),
	   	    pos,
                    flags |
	                mopnflag_precious |
			mopnflag_immobile |
			mopnflag_variant |
			mopnflag_distinct,
                    opd0, mopd_null, mopd_null);
	    break;


         case Br_Uncond_Opr :
            attr_idx = IR_IDX_R(ir_idx);
	    mif_attr_map[attr_idx].val = cvrt_label(attr_idx, flags, mpos_null);
	    if (INVALID(blkix)) {
	       blkix = mifalloc[mtag_blk](&msp);
	       msp.blk[blkix].pos = pos;
	       msp.blk[blkix].scope = local_scope;
	    }
	    mflow_local(&msp, blkix, mif_attr_map[attr_idx].val);
            blkix = NONE;
            break;


         case Br_True_Opr :

            attr_idx = IR_IDX_R(ir_idx);
	    mif_attr_map[attr_idx].val = cvrt_label(attr_idx, flags, mpos_null);
	    if (INVALID(blkix)) {
	       blkix = mifalloc[mtag_blk](&msp);
	       msp.blk[blkix].pos = pos;
	       msp.blk[blkix].scope = local_scope;
	    }

	    /* create mif for condition test */
            cvrt_exp_to_mif(&opd0, &msp.blk[blkix],
			    IR_IDX_L(ir_idx), IR_FLD_L(ir_idx),
			    flags, value);
	    /* add binary test to end of block */
            mif_opn_add(&msp.blk[blkix], 
                    mop_if, 
	            get_basic_type(NONE),
	            pos, 
                    flags, 
                    opd0, 
                    mopd_null, 
                    mopd_null);

            lastblkix = blkix;
            blkix = mifalloc[mtag_blk](&msp);
	    msp.blk[blkix].pos = pos;
	    msp.blk[blkix].scope = local_scope;
	    /* Make arc to false block */
            mflow_local(&msp, lastblkix, blkix);
	    /* Make arc to true block */
            mflow_local(&msp, lastblkix, mif_attr_map[attr_idx].val);
	    msp.blk[lastblkix].next = blkix;

            break;


         case Br_Aif_Opr :

	    /* Make mif for equal label */
	    i= IR_IDX_R(ir_idx);	/* label list */
            attr_idx = IL_IDX(i);
	    mif_attr_map[attr_idx].val = cvrt_label(attr_idx, flags, mpos_null);
	    eq0 = mif_attr_map[attr_idx].val;

	    /* Make mif for greater than label */
	    i = IL_NEXT_LIST_IDX(i);
            attr_idx = IL_IDX(i);
	    mif_attr_map[attr_idx].val = cvrt_label(attr_idx, flags, mpos_null);
	    gt0 = mif_attr_map[attr_idx].val;

	    /* Make mif for less than label */
	    i = IL_NEXT_LIST_IDX(i);
            attr_idx = IL_IDX(i);
	    mif_attr_map[attr_idx].val = cvrt_label(attr_idx, flags, mpos_null);
	    lt0 = mif_attr_map[attr_idx].val;

	    if (INVALID(blkix)) {
	       blkix = mifalloc[mtag_blk](&msp);
	       msp.blk[blkix].pos = pos;
	       msp.blk[blkix].scope = local_scope;
	    }

            cvrt_exp_to_mif(&opd0, 
                            &msp.blk[blkix],
			    IR_IDX_L(ir_idx), 
                            IR_FLD_L(ir_idx),
			    flags, 
                            value);

	    /* If all three targets identical - create unconditional branch. */
	    if (lt0 == eq0 && eq0 == gt0) {
               mif_opn_add(&msp.blk[blkix], 
                       mop_paren, 
                       mopdtype(&msp, opd0, msp.blk[blkix].opn),
                       pos, 
                       flags,
                       opd0, 
                       mopd_null, 
                       mopd_null);
	       mflow_local(&msp, blkix, lt0);		/* uncond */
	       blkix = NONE;
	       break;
	    }

	    /* If two targets identical - create normal conditional branch. */
	    if (lt0 == eq0 || lt0 == gt0 || eq0 == gt0) {

	       /* Get a zero of the right type */
	       typeix = mopdtype(&msp, opd0, msp.blk[blkix].opn);
	       opd = mopd_0;
	       if (typeix != msp.immtype) {
                  opd = mif_opn_add(&msp.blk[blkix], mop_cast, 
                                    typeix, pos, flags,
                                    opd, mopd_null, mopd_null);
	       }

	       /* Turn into comparison against zero */
               opd1 = mopd_0;
	       if (lt0 == eq0) {
		  opd1.val = mrelation_GT;
	       }
	       else if (lt0 == gt0) {
	          opd1.val = mrelation_EQ;
	       }
               else {
		  opd1.val = mrelation_EQ | mrelation_GT;
	       }

               typ = *mtype_null[mtypeclass_bool];
               typ.mbool.kind = storage_bit_kind_tbl[LOGICAL_DEFAULT_TYPE];
               typ.mbool.size = mint(&msp,
                                     msp.immtype,
                   (unsigned long) storage_bit_size_tbl[LOGICAL_DEFAULT_TYPE]);
               typ.mbool.prec = storage_bit_prec_tbl[LOGICAL_DEFAULT_TYPE];
  
               opd0 = mif_opn_add(&msp.blk[blkix], mop_cmp, 
                                  mtype_lookup(&msp, &typ),
		  	          pos, 
                                  flags, 
                                  opd0, 
                                  opd, 
                                  opd1);

	       mflow_local(&msp, blkix, lt0);		/* F */
	       if (lt0 == eq0) {
		  mflow_local(&msp, blkix, gt0);	/* T */
	       }
	       else {
		  mflow_local(&msp, blkix, eq0);	/* T */
	       }

	       op = mop_if;
	       typeix = get_basic_type(NONE);
	    }
	    else {

	       /* 3-branch arithmetic IF */
	       op = mop_aif;

	       typeix = get_basic_type(NONE);
               mflow_local(&msp, blkix, lt0);
               mflow_local(&msp, blkix, eq0);
               mflow_local(&msp, blkix, gt0);

	    }

            mif_opn_add(&msp.blk[blkix], 
                    op, 
                    typeix, 
                    pos, 
                    flags,
                    opd0, 
                    mopd_null, 
                    mopd_null);

	    blkix = NONE;
            break;


         case Br_Asg_Opr :

	    if (INVALID(blkix)) {
	       blkix = mifalloc[mtag_blk](&msp);
	       msp.blk[blkix].pos = pos;
	       msp.blk[blkix].scope = local_scope;
	    }

            cvrt_exp_to_mif(&opd0, &msp.blk[blkix],
			    IR_IDX_L(ir_idx), IR_FLD_L(ir_idx),
			    flags, value);
	    /* create indirect jump operation */
            (void) mif_opn_add(&msp.blk[blkix], 
                           mop_ijmp, 
	                   get_basic_type(NONE),
			   pos, 
                           flags, 
                           opd0, 
                           mopd_null, 
                           mopd_null);

	    /* create arc from indirect jump to all reachable labels. */
            for (attr_idx = SCP_ASSIGN_LBL_CHAIN(curr_scp_idx);
                 attr_idx != NULL_IDX;
                 attr_idx = ATL_NEXT_ASG_LBL_IDX(attr_idx)) {
	        mif_attr_map[attr_idx].val = 
                         cvrt_label(attr_idx, flags, mpos_null);
	        mflow_local(&msp, blkix, mif_attr_map[attr_idx].val);
	    }

	    blkix = NONE;
	    break;


         case Br_Index_Opr :

	    if (INVALID(blkix)) {
	       blkix = mifalloc[mtag_blk](&msp);
	       msp.blk[blkix].pos = pos;
	       msp.blk[blkix].scope = local_scope;
	    }

	    /* fall-through case for default */
            lastblkix = blkix;
            blkix = mifalloc[mtag_blk](&msp);
	    msp.blk[blkix].pos = pos;
	    msp.blk[blkix].scope = local_scope;
	    msp.blk[lastblkix].next = blkix;

	    /* Build a sequence of 'list' operations, each carrying a	    */
	    /* 'case' value in opd[0] and a successor list index in opd[2]. */
	    opd0 = mopd_0;			/* 'case' values */
	    opd1 = mopd_null;			/* list link */
	    opd2 = mopd_0;			/* successor list indices */
	    for (i = 0, j = IR_IDX_R(ir_idx);
	         i < IR_LIST_CNT_R(ir_idx);
	         i++, j = IL_NEXT_LIST_IDX(j)) {

	       attr_idx = IL_IDX(j);

               mif_attr_map[attr_idx].val = 
                        cvrt_label(attr_idx, flags, mpos_null);

	       opd0.val = i + 1;
	       opd2.val = mflow_local(&msp, lastblkix,
				      mif_attr_map[attr_idx].val);

               opd1 = mif_opn_add(&msp.blk[lastblkix], mop_list,
			mopdtype(&msp, opd0, msp.blk[lastblkix].opn), pos,
			flags, opd0, opd1, opd2);
	    }

            cvrt_exp_to_mif(&opd0, &msp.blk[lastblkix],
			    IR_IDX_L(ir_idx), IR_FLD_L(ir_idx),
			    flags, value);
	    opd2.val = mflow_local(&msp, lastblkix, blkix); /* default */

            mif_opn_add(&msp.blk[lastblkix], 
                    mop_switch,
	            get_basic_type(NONE),
                    pos, 
                    flags, 
                    opd0,  
                    opd1,  
                    opd2);
	    break;


         case Select_Opr :
	 /* Create new basic block in control flow graph if not within one. */

	    if (INVALID(blkix)) {
	       blkix = mifalloc[mtag_blk](&msp);
	       msp.blk[blkix].pos = pos;
	       msp.blk[blkix].scope = local_scope;
	    }

            /* Flatten select statement to a switch. */

            /* Scan forward to locate CASEs. Skip over nested CASEs. */
            opd = mopd_null;	  /* default case */
            opd1 = mopd_null;	  /* list link */
	    opd2 = mopd_0;	  /* succ list index */
            /* # of CASEs left */
            case_ct = CN_INT_TO_C(IL_IDX(IR_IDX_R(ir_idx))); 
            nested_case_ct = 0;
            for (tmp_sh = SH_NEXT_IDX(curr_sh);
                 case_ct && tmp_sh != NULL_IDX;
                 tmp_sh = SH_NEXT_IDX(tmp_sh)) {

               if (SH_IR_IDX(tmp_sh) == NULL_IDX) {
                  continue;
               }

               if (IR_OPR(SH_IR_IDX(tmp_sh)) == Select_Opr) {
                  /* nested SELECT CASE construct */
                  nested_case_ct +=
                     CN_INT_TO_C(IL_IDX(IR_IDX_R(SH_IR_IDX(tmp_sh))));
               }

               else if (IR_OPR(SH_IR_IDX(tmp_sh)) == Case_Opr) {

		  if (IR_IDX_L(SH_IR_IDX(tmp_sh)) == NULL_IDX) {
                     /* Default case */
                     continue;
                  }

                  if (nested_case_ct) {	/* nested CASE; skip */
                     nested_case_ct--;
                  }

                  else {		/* CASE corresponds to current SELECT */

                     case_ct--;

                     /* Allocate a basic block for the case now. */
                     i = mifalloc[mtag_blk](&msp);
	             msp.blk[i].pos = pos;
	             msp.blk[i].scope = local_scope;

                     /* Save the block index in the text. This is a little    */
                     /* dubious, but we're just overwriting the line # field. */
                     IR_LINE_NUM_R(SH_IR_IDX(tmp_sh)) = i;

                     cvrt_exp_to_mif(&opd0, &msp.blk[blkix],
		                     IR_IDX_L(SH_IR_IDX(tmp_sh)),
                                     IR_FLD_L(SH_IR_IDX(tmp_sh)),
                                     flags, value);
	             opd2.val = mflow_local(&msp, blkix, i);
                     opd1 = mif_opn_add(&msp.blk[blkix], mop_list, 
				mopdtype(&msp, opd0, msp.blk[blkix].opn),
				pos, flags, opd0, opd1, opd2);
                  }
               }
            }

            /* Selector */
            cvrt_exp_to_mif(&opd0, 
                            &msp.blk[blkix],
			    IR_IDX_L(ir_idx), 
                            IR_FLD_L(ir_idx),
			    flags, value);

            /* Explicit DEFAULT or implicit "break" label */
            i = IL_NEXT_LIST_IDX(IR_IDX_R(ir_idx));
            if (IR_LIST_CNT_R(ir_idx) > 2) {
               i = IL_NEXT_LIST_IDX(i);
            }
	    attr_idx = IL_IDX(i);
	    mif_attr_map[attr_idx].val = cvrt_label(attr_idx, flags, mpos_null);

            /* Default successor index. */
            opd = mopd_0;
            opd.val = mflow_local(&msp, blkix, mif_attr_map[attr_idx].val);

            mif_opn_add(&msp.blk[blkix], 
                    mop_switch, 
	            get_basic_type(NONE),
	            pos, 
                    flags, 
                    opd0, 
                    opd1, 
                    opd);

            blkix = NONE;

	    break;


         case Case_Opr :
            if (IR_IDX_L(ir_idx) == NULL_IDX) {
               /* Ignore default case */
               break;
            }

            /* Move to block allocated during SELECT processing. */
            lastblkix = blkix;
	    blkix = IR_LINE_NUM_R(ir_idx);
	    msp.blk[blkix].pos = pos;
	    if (VALID(lastblkix)) {
	       mflow_local(&msp, lastblkix, blkix);
	    }
	    break;


         case Loop_Info_Opr :

	    /* BECKER - Hack code to be removed once frontend
	     * clearly marks the end of a parallel loop.
	     *
	     * Need to recognize start and end of loop to correctly
	     * mark boundaries of a tasked loop.
	     */
	    idx = IR_IDX_R(ir_idx);
	    idx = IL_NEXT_LIST_IDX(idx);
	    idx = IL_IDX(idx);
	    idx = IL_NEXT_LIST_IDX(idx);
	    loop_end_label_idx = IL_IDX(idx);
            break;


         case Loop_End_Opr :
            break;


	 case Init_Opr :

            if (IR_FLD_L(ir_idx) == IR_Tbl_Idx &&
                IR_OPR(IR_IDX_L(ir_idx)) == Implied_Do_Opr) {

	       /* Initialize constant output list */
	       data_value_idx = IR_IDX_R(ir_idx);
	       data_values_consumed = 0;

	       cvrt_data_impl_do(IR_IDX_L(ir_idx), IR_FLD_L(ir_idx));
	    }
	    else {

	       /* simple initializer prepared by s_data.c */

	       initix = mifalloc[mtag_init](&msp);
	       msp.init[initix].offset = fold_exp(IR_IDX_L(ir_idx), 
                                                  IR_FLD_L(ir_idx),
			                          &baseattr,  
                                                  &fldattr, 
                                                  &typeix);

	       i = IR_IDX_R(ir_idx); /* constant */

	       cn_idx = IL_IDX(i);

               switch (TYP_TYPE(CN_TYPE_IDX(cn_idx))) {

               case Character : 
                  if ((IR_FLD_L(ir_idx) == IR_Tbl_Idx) &&
                      ((IR_OPR(IR_IDX_L(ir_idx)) == Substring_Opr) ||
                       (IR_OPR(IR_IDX_L(ir_idx)) == Whole_Substring_Opr))) {
                     l_idx = IR_IDX_R(IR_IDX_L(ir_idx));
                     l_idx = IL_NEXT_LIST_IDX(l_idx);
                     l_idx = IL_NEXT_LIST_IDX(l_idx);
	             msp.init[initix].size = CN_INT_TO_C(IL_IDX(l_idx)) * 
                                                                     CHAR_BIT;
                  }
                  else {
	             msp.init[initix].size =
		     CN_INT_TO_C(TYP_IDX(CN_TYPE_IDX(cn_idx))) * CHAR_BIT;
                  }
                  break;

               case Typeless : 
                  msp.init[initix].size = TYP_BIT_LEN(CN_TYPE_IDX(cn_idx));
                  break;

	       default :
                  if (msp.type[typeix].u.size.tag != mtag_imm) {
                     t = mif_con_to_host_long(&msp,
                          msp.type[typeix].u.size.val);
                  }
                  else {
                     t = msp.type[typeix].u.size.val;
                  }

	          msp.init[initix].size = t;
                  break;
               }

	       cvrt_const((char *)&CN_CONST(cn_idx),
		          CN_TYPE_IDX(cn_idx),
		          ATD_TYPE_IDX(fldattr),
		          &msp.init[initix].val);

	       i = IL_NEXT_LIST_IDX(i);	/* rep count */
	       msp.init[initix].count = CN_INT_TO_C(IL_IDX(i));
    
	       i = IL_NEXT_LIST_IDX(i);	/* stride */
	       msp.init[initix].stride = CN_INT_TO_C(IL_IDX(i));
	       if (!msp.init[initix].stride) {
	          msp.init[initix].stride = msp.init[initix].size;
	       }

	       insert_init(initix, 
                           mif_attr_map[baseattr].tag,
			   mif_attr_map[baseattr].val);
	    }
            break;


         case Init_Reloc_Opr :
	    initix = mifalloc[mtag_init](&msp);

            if (msp.ldexpr == 0) {
               mifalloc[mtag_ldexpr](&msp);
            }

            offset = fold_exp(IR_IDX_L(ir_idx),
                              IR_FLD_L(ir_idx),
                              &baseattr, &fldattr, &typeix);

# if defined(_TARGET_OS_SOLARIS) || defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN))
            loc_offset_idx = IL_IDX(IL_NEXT_LIST_IDX(IR_IDX_R(ir_idx)));


            COPY_OPND(opnd, IL_OPND(IR_IDX_R(ir_idx)));
            attr_idx = find_left_attr(&opnd);

# ifdef _DEBUG
            if (SB_FIRST_ATTR_IDX(ATD_STOR_BLK_IDX(attr_idx)) == NULL_IDX) {
               PRINTMSG(IR_LINE_NUM(ir_idx), 1049, Internal, 
                        IR_COL_NUM(ir_idx));
            }
# endif

            offset1.idx	= ATD_OFFSET_IDX(attr_idx);
            offset1.fld	= ATD_OFFSET_FLD(attr_idx);
            result.idx	= ATD_OFFSET_IDX(SB_FIRST_ATTR_IDX(
                                                 ATD_STOR_BLK_IDX(attr_idx)));
            result.fld	= ATD_OFFSET_FLD(SB_FIRST_ATTR_IDX(
                                                 ATD_STOR_BLK_IDX(attr_idx)));

            size_offset_binary_calc(&offset1, &result, Minus_Opr, &result);

            offset1.idx	= loc_offset_idx;
            offset1.fld	= CN_Tbl_Idx;

            size_offset_binary_calc(&offset1, &result, Plus_Opr, &result);

            attr_idx = SB_FIRST_ATTR_IDX(ATD_STOR_BLK_IDX(attr_idx));
    
            OPND_FLD(opnd) = AT_Tbl_Idx;
            OPND_IDX(opnd) = attr_idx;
            OPND_LINE_NUM(opnd) = IR_LINE_NUM(ir_idx);
            OPND_COL_NUM(opnd)  = IR_COL_NUM(ir_idx);

            /* assumes there is a loc or aloc on top */
            COPY_OPND(IR_OPND_L(IL_IDX(IR_IDX_R(ir_idx))), opnd);
            IR_OPR(IL_IDX(IR_IDX_R(ir_idx))) = Aloc_Opr;
            IR_TYPE_IDX(IL_IDX(IR_IDX_R(ir_idx))) = CRI_Ptr_8;

            cvrt_exp_to_mif(&opd1, 
                            msp.ldexpr,
		            IL_IDX(IR_IDX_R(ir_idx)),
                            IL_FLD(IR_IDX_R(ir_idx)),
                            flags, 
                            value);

            char_bit.fld	 = CN_Tbl_Idx;
            char_bit.idx	 = CN_INTEGER_CHAR_BIT_IDX;

            size_offset_binary_calc(&result, &char_bit, Div_Opr, &result);

# ifdef _TARGET32
            if (TYP_LINEAR(result.type_idx) == Integer_8) {
               result.constant[0]	= result.constant[1];
               result.type_idx		= CG_INTEGER_DEFAULT_TYPE;
               result.fld		= NO_Tbl_Idx;
            }
# endif

            if (result.fld == NO_Tbl_Idx) {
               cn_idx = ntr_const_tbl(result.type_idx, FALSE, result.constant);
            }
            else if (result.fld == CN_Tbl_Idx) {
               cn_idx = result.idx;
            }
            else {  /* Should be constant here. */
               PRINTMSG(IR_LINE_NUM(ir_idx), 1201, Internal, 
                        IR_COL_NUM(ir_idx), " ");
            }

            cvrt_exp_to_mif(&opd2,
                            msp.ldexpr,
                            cn_idx,
                            CN_Tbl_Idx,
                            flags,
                            value);

            opd0 = mif_opn_add(msp.ldexpr,
                               mop_pinc,
                               mopdtype(&msp, opd1, msp.ldexpr->opn),
                               mpos_null,
                               0,
                               opd1,
                               opd2,
                               mopd_null);
# else
            cvrt_exp_to_mif(&opd0, 
                            msp.ldexpr,
		            IL_IDX(IR_IDX_R(ir_idx)),
                            IL_FLD(IR_IDX_R(ir_idx)),
                            flags, 
                            value);
# endif

            msp.init[initix].val = opd0;
	    msp.init[initix].offset = offset;
            msp.init[initix].count = 1;
            msp.init[initix].size = TARGET_BITS_PER_WORD;
            msp.init[initix].stride = msp.init[initix].size;

	    insert_init(initix, 
                        mif_attr_map[baseattr].tag,
			mif_attr_map[baseattr].val);
            break;



         case Use_Opr:

# if defined(_MODULE_TO_DOT_o)

            if (ATP_MOD_PATH_IDX(IR_IDX_L(ir_idx)) != NULL_IDX) {

            if (INVALID(blkix)) {
               blkix = mifalloc[mtag_blk](&msp);
               msp.blk[blkix].pos = pos;
               msp.blk[blkix].scope = local_scope;
            }
 
            length = ATP_MOD_PATH_LEN(IR_IDX_L(ir_idx));
 
            typ = *mtype_null[mtypeclass_raw];
            typ.mraw.size = mint(&msp,
                                 msp.immtype,
                                 (unsigned long)(length * CHAR_BIT));
 
            opd0.tag = mtag_con;
            opd0.val = mcon_lookup(&msp,
                                   mtype_lookup(&msp, &typ),
                                (char *)ATP_MOD_PATH_NAME_PTR(IR_IDX_L(ir_idx)),
                                   NONE);
 
            length = ATP_EXT_NAME_LEN(IR_IDX_L(ir_idx));
 
            typ = *mtype_null[mtypeclass_raw];
            typ.mraw.size = mint(&msp,
                                 msp.immtype,
                                 (unsigned long)(length * CHAR_BIT));
 
            opd1.tag = mtag_con;
            opd1.val = mcon_lookup(&msp,
                                   mtype_lookup(&msp, &typ),
                                   (char *)ATP_EXT_NAME_PTR(IR_IDX_L(ir_idx)),
                                   NONE);
 
            mif_opn_add(&msp.blk[blkix],
                        mop_usepath,
                        get_basic_type(NONE),
                        pos,
                        flags,
                        opd0,
                        opd1,
                        mopd_null);
            }
# endif

            break;
 
         default:
	    if (INVALID(blkix)) {
	       blkix = mifalloc[mtag_blk](&msp);
	       msp.blk[blkix].pos = pos;
	       msp.blk[blkix].scope = local_scope;
	    }

            cvrt_exp_to_mif(&opd, 
                            &msp.blk[blkix],
			    ir_idx, 
                            IR_Tbl_Idx,
			    flags, 
                            address);
	    break;
         }
      }
   }

   TRACE (Func_Exit, "cvrt_ir_to_mif", NULL);

}  /* cvrt_ir_to_mif */



/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Acquire intermediate form type table index for a frontend	      *|
|*	type entry.							      *|
|*									      *|
|* Input parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	intermediate form type table index				      *|
|*									      *|
\******************************************************************************/

static int get_basic_type(int	type_idx)

{
   int		idx;
   mtype_t 	typ;
   int          btype;

   TRACE (Func_Entry, "get_basic_type", NULL);

   if (type_idx == NONE) {
      typ = *mtype_null[mtypeclass_void];
      typ.mvoid.size = mint(&msp, msp.immtype, (unsigned long) 0);
      idx = mtype_lookup(&msp, &typ);
   }
   else {

   switch (TYP_TYPE(type_idx)) {
 
   case Typeless :
      typ = *mtype_null[mtypeclass_raw];
      typ.mraw.size = mint(&msp, msp.immtype, 
                           (unsigned long) TYP_BIT_LEN(type_idx));
      idx = mtype_lookup(&msp, &typ);
      break;


   case Integer :
      typ = *mtype_null[mtypeclass_int];

      switch (TYP_LINEAR(type_idx)) {
      case Integer_1:
      case Integer_2:
      case Integer_4:
# ifdef _TARGET64
         if (unsigned_type) {
            typ.mint.flags |= mtypeflag_unsigned;
         }
# endif
         break;
      }

      typ.mint.kind = storage_bit_kind_tbl[TYP_LINEAR(type_idx)];
      typ.mint.size = mint(&msp, msp.immtype, 
          (unsigned long) storage_bit_size_tbl[TYP_LINEAR(type_idx)]);
      typ.mint.prec = storage_bit_prec_tbl[TYP_LINEAR(type_idx)];
      idx =  mtype_lookup(&msp, &typ);
      break;


   case Logical :
      typ = *mtype_null [mtypeclass_bool];
      typ.mint.kind = storage_bit_kind_tbl[TYP_LINEAR(type_idx)];
      typ.mint.size = mint(&msp, msp.immtype, 
          (unsigned long) storage_bit_size_tbl[TYP_LINEAR(type_idx)]);
      typ.mint.prec = storage_bit_prec_tbl[TYP_LINEAR(type_idx)];
      idx = mtype_lookup(&msp, &typ);
      break;


   case Real :
      typ = *mtype_null[mtypeclass_float];
      typ.mint.kind = storage_bit_kind_tbl[TYP_LINEAR(type_idx)];
      typ.mint.size = mint(&msp, msp.immtype, 
          (unsigned long) storage_bit_size_tbl[TYP_LINEAR(type_idx)]);
      typ.mint.prec = storage_bit_prec_tbl[TYP_LINEAR(type_idx)];
      idx = mtype_lookup(&msp, &typ);
      break;


   case Complex :
      typ = *mtype_null [mtypeclass_complex];
      typ.mint.kind = storage_bit_kind_tbl[TYP_LINEAR(type_idx)];
      typ.mint.size = mint(&msp, msp.immtype, 
          (unsigned long) storage_bit_size_tbl[TYP_LINEAR(type_idx)]);
      typ.mint.prec = storage_bit_prec_tbl[TYP_LINEAR(type_idx)];
      idx = mtype_lookup(&msp, &typ);
      break;


   case Character :
      typ = *mtype_null[mtypeclass_fchar];
      typ.mfchar.prec = storage_bit_prec_tbl[TYP_LINEAR(type_idx)];

      switch (TYP_CHAR_CLASS(type_idx)) {
      case Assumed_Size_Char :
      case Var_Len_Char :
         typ.mfchar.temp.val = cvrt_attr_ntry(TYP_IDX(type_idx));
         typ.mfchar.temp.tag = mif_attr_map[TYP_IDX(type_idx)].tag;
         break;

      case Const_Len_Char :
         typ.mfchar.size = mint(&msp, msp.immtype, 
          (unsigned long) (CHAR_BIT * CN_INT_TO_C(TYP_IDX(type_idx))));
         break;
      }

      idx = mtype_lookup(&msp, &typ);
      break;


   case Structure :
      idx = cvrt_derived_type(TYP_IDX(type_idx));
      break;


   case CRI_Ptr :
      /* CRI Pointer has no base type      */ 
      /* since a pointer can have multiple */
      /* pointees of different types.      */

      typ = *mtype_null[mtypeclass_addr];
      typ.maddr.size = mint(&msp, msp.immtype, 
          (unsigned long) storage_bit_size_tbl[CRI_Ptr_8]);
      typ.maddr.prec = TYP_PTR_INCREMENT(type_idx);
      typ.maddr.aliasing = maliasclass_restrict;
      idx = mtype_lookup(&msp, &typ);
      break;


   case CRI_Ch_Ptr :
      /* base type of CRI char pointer is a CHARACTER *(*) */

      typ = *mtype_null[mtypeclass_fchar];
      typ.mfchar.prec = CHAR_BIT;
      btype = mtype_lookup(&msp, &typ);

      typ = *mtype_null[mtypeclass_addr];
      typ.maddr.size = mint(&msp, msp.immtype, 
          (unsigned long) storage_bit_size_tbl[CRI_Ch_Ptr_8]);
      typ.maddr.prec = storage_bit_prec_tbl[CRI_Ch_Ptr_8];
      typ.maddr.base = btype;
      typ.maddr.aliasing = maliasclass_restrict;

      idx = mtype_lookup(&msp, &typ);
      break;


   case CRI_Parcel_Ptr :
      typ = *mtype_null[mtypeclass_blkaddr];
      typ.mblkaddr.size = mint(&msp, msp.immtype, 
          (unsigned long) storage_bit_size_tbl[CRI_Parcel_Ptr_8]);
      idx = mtype_lookup(&msp, &typ);
      break;


   default :
      PRINTMSG(1, 1044, Internal, 0, "unexpected TYP_TYPE value");

   }  /* End switch */

   }  /* Else */

   TRACE (Func_Exit, "get_basic_type", NULL);

   return(idx);

}  /* get_basic_type */



/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Construct a pointer type for a given pointee type.		      *|
|*									      *|
|* Input parameters:							      *|
|*	Frontend type index						      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	Index of created intermediate type				      *|
|*									      *|
\******************************************************************************/
static int get_ptr_type(fld_type	field,
			int 		idx)
{
   int typ_idx;
   mtype_t type;


   TRACE (Func_Exit, "get_ptr_type", NULL);

   type = *mtype_null[mtypeclass_addr];

   if (field == AT_Tbl_Idx) {
      type.maddr.base = get_type_idx(idx);

      if (AT_OBJ_CLASS(idx) == Data_Obj &&
          (ATD_CLASS(idx) == Dummy_Argument ||
           ATD_CLASS(idx) == Function_Result ||
           ATD_CLASS(idx) == CRI__Pointee)) {
         type.maddr.aliasing = maliasclass_restrict;
      }
      else {
         type.maddr.aliasing = maliasclass_anytype;
      }
   }
   else if (field == IR_Tbl_Idx) {
      type.maddr.base = get_basic_type(IR_TYPE_IDX(idx));

      if (IR_OPR(idx) == Const_Tmp_Loc_Opr) {
         type.maddr.aliasing = maliasclass_restrict;
      }
      else {
         type.maddr.aliasing = maliasclass_anytype;
      }
   }

   typ_idx = type.maddr.base;
   while (VALID(msp.type[typ_idx].u.base)) {
      typ_idx = msp.type[typ_idx].u.base;
   }

   if (msp.type[typ_idx].u.class == mtypeclass_fchar) {
      type.maddr.size = mint(&msp, msp.immtype, 
          (unsigned long) storage_bit_size_tbl[CRI_Ch_Ptr_8]);
      type.maddr.prec = storage_bit_prec_tbl[CRI_Ch_Ptr_8];
   }
   else {
      type.maddr.size = mint(&msp, msp.immtype, 
          (unsigned long) storage_bit_size_tbl[CRI_Ptr_8]);
      type.maddr.prec = storage_bit_prec_tbl[CRI_Ptr_8];
   }

   TRACE (Func_Exit, "get_ptr_type", NULL);

   return(mtype_lookup(&msp, &type));
} /* get_ptr_type */



/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Construct an intermediate form type table entry for an attribute.     *|
|*									      *|
|* Input parameters:							      *|
|*	attribute index							      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	intermediate form type table index				      *|
|*									      *|
\******************************************************************************/
static int get_type_idx(int	input_idx)
{
   int			array_idx;
   int			attr_idx;
   int			i;
   int			j;
   int			mtype;
   size_offset_type	size;
   long			temp;
   int			tmp_idx;
   mtype_t		typ;
   int			type_idx;
   long			val;


   TRACE (Func_Entry, "get_type_idx", NULL);

   if (VALID(mif_attr_type_map[input_idx])) {
      /* If already translated, return translated value */
      return(mif_attr_type_map[input_idx]);
   }

   mtype = 0;

   switch (AT_OBJ_CLASS(input_idx)) {

   case Data_Obj:
      attr_idx = input_idx;

      if (ATD_CLASS(attr_idx) == CRI__Pointee &&
          TYP_TYPE(ATD_TYPE_IDX(ATD_PTR_IDX(attr_idx))) == CRI_Ch_Ptr) {
	 /* Cray character pointee must be an assumed-length character */
         typ = *mtype_null[mtypeclass_fchar];
	 typ.mfchar.prec = storage_bit_prec_tbl[CRI_Ch_Ptr_8];
         typ.mfchar.temp.val = cvrt_attr_ntry(ATD_PTR_IDX(attr_idx));
         typ.mfchar.temp.tag = mif_attr_map[ATD_PTR_IDX(attr_idx)].tag;

	 type_idx = mtype_lookup(&msp, &typ);
      }
      else {
         type_idx = get_basic_type(ATD_TYPE_IDX(attr_idx));
      }
      break;

   case Pgm_Unit:
      if (ATP_PGM_UNIT(input_idx) == Function) {
         attr_idx = ATP_RSLT_IDX(input_idx);

         /* When a function result is an array or CHARACTER, the temps are */
         /* just templates, never defined nor referenced in the IR. They   */
         /* still need to get to PDGCS.                                    */
         if (ATP_EXPL_ITRFC(input_idx) && !ATP_SCP_ALIVE(input_idx)) {
            if (TYP_TYPE(ATD_TYPE_IDX(attr_idx)) == Character &&
                TYP_CHAR_CLASS(ATD_TYPE_IDX(attr_idx)) != Const_Len_Char) {
               cvrt_attr_ntry(TYP_IDX(ATD_TYPE_IDX(attr_idx)));
            }
         }

         type_idx = get_basic_type(ATD_TYPE_IDX(attr_idx));
      }
      else {
         type_idx = get_basic_type(NONE);
         goto EXIT;
      }
      break;

   default:
      PRINTMSG(AT_DEF_LINE(input_idx), 450, Internal, 0, "get_type_idx");
      break;

   }  /* End switch */

   /* Deferred_Shape and Assumed_Shape arrays will always be dope vectors. */

   if (ATD_IM_A_DOPE(attr_idx)) {  /* Pointers */

      typ = *mtype_null [mtypeclass_dope];
      typ.mdope.base 	= type_idx;
      typ.mdope.rank	= (ATD_ARRAY_IDX(attr_idx) == NULL_IDX)
			? 0
			: BD_RANK(ATD_ARRAY_IDX(attr_idx));
      typ.mdope.size = mint(&msp, msp.immtype, 
          (unsigned long) (TARGET_BITS_PER_WORD * (6 + 3 * typ.mdope.rank)));
      type_idx  = mtype_lookup(&msp, &typ);
   }

   else if (ATD_ARRAY_IDX(attr_idx) != NULL_IDX) {

      array_idx	= ATD_ARRAY_IDX(attr_idx);

      typ = *mtype_null[mtypeclass_array];
      typ.marray.base = get_basic_type(ATD_TYPE_IDX(attr_idx));
      typ.marray.rank = BD_RANK(array_idx);

      if (typ.marray.rank) {

	 /* Allocate arrays of mif operands for array information */

         MEM_ALLOC(typ.marray.low,    mopd_t, typ.marray.rank);
         MEM_ALLOC(typ.marray.stride, mopd_t, typ.marray.rank);
         MEM_ALLOC(typ.marray.extent, mopd_t, typ.marray.rank);
      }

      if (BD_ARRAY_CLASS(array_idx) <= Assumed_Size) {
          size = stor_bit_size_of(attr_idx, FALSE, FALSE);

         /* Size of one element*/

         if (size.fld == CN_Tbl_Idx) {
            typ.marray.size = mint(&msp, msp.immtype, 
                              (unsigned long) CN_INT_TO_C(size.idx));
         }
         else {
            typ.marray.size = mint(&msp, msp.immtype, 
                              (unsigned long) CN_BIG_INT_TO_LONG(size));
         }
      }

      for (i = 1; i <= BD_RANK(array_idx); i++) {

	 /* Lower bound */
         if (BD_LB_FLD(array_idx, i) == CN_Tbl_Idx) {
            cvrt_exp_to_mif(&typ.marray.low[i-1], 0,
			    BD_LB_IDX(array_idx,i), CN_Tbl_Idx,
			    mopnflag_syn, value);
         }
         else {
            j = BD_LB_IDX(array_idx, i);
            typ.marray.low[i-1].tag = mtag_lsym;
	    typ.marray.low[i-1].val = cvrt_attr_ntry(j);
         }

	 /* Stride multiplier */
         if (BD_SM_FLD(array_idx, i) == CN_Tbl_Idx) {
            cvrt_exp_to_mif(&typ.marray.stride[i-1], 0,
			    BD_SM_IDX(array_idx,i), CN_Tbl_Idx,
			    mopnflag_syn, value);
         }
         else {
            j = BD_SM_IDX(array_idx, i);
            typ.marray.stride[i-1].tag = mtag_lsym;
	    typ.marray.stride[i-1].val = cvrt_attr_ntry(j);
         }

	 /* Extent */
         if (BD_XT_FLD(array_idx, i) == CN_Tbl_Idx) {
            cvrt_exp_to_mif(&typ.marray.extent[i-1], 
                            0,
			    BD_XT_IDX(array_idx,i), 
                            CN_Tbl_Idx,
			    mopnflag_syn, 
                            value);

            if (typ.marray.size.tag != mtag_imm) {
               temp = mif_con_to_host_long(&msp, typ.marray.size.val);
            }
            else {
               temp = typ.marray.size.val;
            }

            typ.marray.size = mint(&msp, msp.immtype, 
             (unsigned long) (temp * CN_INT_TO_C(BD_XT_IDX(array_idx, i))));
         }
         else {
            j = BD_XT_IDX(array_idx, i);
            typ.marray.extent[i-1].tag = mtag_lsym;
            typ.marray.extent[i-1].val = cvrt_attr_ntry(j);
            typ.marray.size = mint(&msp, msp.immtype, (unsigned long) 0);
         }
      } 

      type_idx = mtype_lookup(&msp, &typ);
   }

EXIT:
   mif_attr_type_map[input_idx] = type_idx;
   return(type_idx);

   TRACE (Func_Exit, "get_type_idx", NULL);
}  /* get_type_idx */



/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	cvrt_dummy_procedure converts a dummy argument procedure into a       *|
|*      symbol table entry of a pointer to a function.                        *|
|*									      *|
|* Input parameters:							      *|
|*	attr_idx	   -> Index of attr entry for dummy procedure         *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NOTHING                                 			      *|
|*									      *|
\******************************************************************************/
static void	cvrt_dummy_procedure(int	attr_idx)

{
   mtype_t	ftype, ptype;
   int		symix;

   TRACE (Func_Entry, "cvrt_dummy_procedure", NULL);

   /* Build a prototype */
   ftype = *mtype_null [mtypeclass_func];
   ftype.mfunc.base  = get_type_idx(attr_idx);
   ftype.mfunc.flags = mtypeflag_arg_mystery;
   ptype = *mtype_null[mtypeclass_addr];
   ptype.maddr.size = mint(&msp, msp.immtype, (unsigned long) 
                           storage_bit_size_tbl[CRI_Ptr_8]);
   ptype.maddr.prec = storage_bit_prec_tbl[CRI_Ptr_8];
   ptype.maddr.aliasing = maliasclass_restrict; 
   ptype.maddr.base = mtype_lookup(&msp, &ftype);

   symix = mifalloc[mtag_lsym](&msp);
   mif_attr_map[attr_idx].tag = mtag_lsym;
   mif_attr_map[attr_idx].val = symix;
   msp.lsym[symix].name = mnpool(&msp, AT_OBJ_NAME_PTR(attr_idx));
   msp.lsym[symix].scope = local_scope;

   /* Set flag if hosted stack is used */
   if (AT_REF_IN_CHILD(attr_idx)) {
      msp.lsym[symix].flags |= msymflag_child_ref;
   }
   if (AT_DEF_IN_CHILD(attr_idx)) {
      msp.lsym[symix].flags |= msymflag_child_def;
   }
   if (AT_DEFINED(attr_idx)) {
      msp.lsym[symix].flags |= msymflag_modified;
   }
   msp.lsym[symix].storage = mstorage_formal;
   msp.lsym[symix].type = mtype_lookup(&msp, &ptype);

   TRACE (Func_Exit, "cvrt_dummy_procedure", NULL);

}  /* cvrt_dummy_procedure */




/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	cvrt_darg_list sends the dummy argument list for the given program.   *|
|*									      *|
|* Input parameters:							      *|
|*	pgm_attr_idx	   -> Index of attr entry for program unit, whose     *|
|*			      dummy args need to be converted.		      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	Secondary symbol table list first index				      *|
|*									      *|
\******************************************************************************/
static int	cvrt_darg_list(int	pgm_attr_idx)

{
   int		i;
   int		attr_idx;
   int		size;
   int		sn_idx;
   mtype_t	ftype, ptype;
   msym_t	*s;
   int		symix = NONE;
   int		sym2ix;
   int		first_sym2_idx = NONE;
   int		last_sym2_idx = NONE;


   TRACE (Func_Entry, "cvrt_darg_list", NULL);

   for (i = 0; i < ATP_NUM_DARGS(pgm_attr_idx); i++) {

      sn_idx = ATP_FIRST_IDX(pgm_attr_idx) + i;
      attr_idx = SN_ATTR_IDX(sn_idx);

      if (!ATP_IN_INTERFACE_BLK(pgm_attr_idx)) {
         /* Get primary symbol table entry for dummy argument */
         if (mif_attr_map[attr_idx].tag == mtag_none) {
       	    /* if not seen before, convert to mif */

            if (AT_OBJ_CLASS(attr_idx) == Data_Obj) {
               cvrt_attr_ntry(attr_idx);
            }
            else {
               cvrt_dummy_procedure(attr_idx);
            }
         }

         symix = mif_attr_map[attr_idx].val;
      }

      /* Create secondary symbol table entry for dummy argument */
      sym2ix = mifalloc[mtag_sym2](&msp);

      if ((symix != NONE) && (msp.lsym[symix].flags & msymflag_implicit)) {
         msp.sym2[sym2ix].flags |= msym2flag_implicit;
      }

      if (AT_OPTIONAL(attr_idx)) {
         msp.sym2[sym2ix].flags |= msym2flag_optional;
      }

      msp.sym2[sym2ix].lang = mlang_F90;
      msp.sym2[sym2ix].name = mnpool(&msp, AT_OBJ_NAME_PTR(attr_idx));

      if (symix != NONE) {
         s = &msp.lsym[symix];
         s->argsym2_ct += 1;

         if (s->argsym2_ct == 1) {
            MEM_ALLOC(s->argsym2, int, s->argsym2_ct);
         }
         else {
            MEM_REALLOC(s->argsym2, int, s->argsym2_ct);
         }

         s->argsym2[s->argsym2_ct-1] = sym2ix;

         if (AT_OBJ_CLASS (attr_idx) == Data_Obj) {
            symix = msp.lsym[symix].base.val;
         }

         msp.sym2[sym2ix].type = msp.lsym[symix].type;
      }
      else {
         /* Turn Fortran dummy arguments into based variables, pointees. */
         msp.sym2[sym2ix].type = get_ptr_type(AT_Tbl_Idx, attr_idx);
      }

      /* Note: sym2.base is filled in later by cvrt_proc */

      if (AT_OBJ_CLASS(attr_idx) == Data_Obj) {
         switch (ATD_INTENT(attr_idx)) {
            case Intent_In :
               msp.sym2[sym2ix].intent = mintent_in;
               break;
            case Intent_Out :
               msp.sym2[sym2ix].intent = mintent_out;
               break;
            default :
               if (ATD_CLASS(attr_idx) == Function_Result) {
                  msp.sym2[sym2ix].intent = mintent_out;
               }
               else {
                  msp.sym2[sym2ix].intent = mintent_in_out;
               }
               break;
         }
      }
      else	/* Dummy procedure */
         msp.sym2[sym2ix].intent = mintent_in_out;

      if (VALID(last_sym2_idx)) {
         msp.sym2[last_sym2_idx].next = sym2ix;
      }
      else {
         first_sym2_idx = sym2ix;
      }
      last_sym2_idx = sym2ix;
   }

   TRACE (Func_Exit, "cvrt_darg_list", NULL);

   return(first_sym2_idx);

}   /* cvrt_darg_list */




/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Convert a procedure						      *|
|*									      *|
|* Input parameters:							      *|
|*      attr_idx      -> Attr index of entry point.                           *|
|*      alt_entry_idx ->						      *|
|*      call_type     -> This is an enum and can be a Definition, Parent or   *|
|*                       Imported.  These determine which PDGCS inteface      *|
|*                       routine to call and what arguments need to be sent.  *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/
static void  cvrt_proc(int		attr_idx,
		       int		alt_entry_idx,
		       enum proc_call_class call_type)

{

   int		i;
   int		pgm_unit;
   int		funcix;
   mtype_t	type;
   mtype_t	typ;
   int		proc;
   int		main_entry_idx;
   char		*p;
   int		rslt_idx;
   int		parent_attr;
   mpos_t	pos;


   TRACE (Func_Entry, "cvrt_proc", NULL);

   if (call_type == Imported) {
      /* Test to see if procedure needs to be converted */

      if (ATP_PROC(attr_idx) == Dummy_Proc) {
         if (mif_attr_map[attr_idx].tag == mtag_none) {
	    /* if not seen before, convert to mif */
            cvrt_dummy_procedure(attr_idx);
	 }
         goto EXIT;
      }

      if (ATP_PGM_UNIT(attr_idx) == Module && ATP_IN_CURRENT_COMPILE(attr_idx)){
         /* Only need to convert modules that are not in the current */
         /* compilation unit.                                        */
         goto EXIT;
      }
      else if (ATP_PGM_UNIT(SCP_ATTR_IDX(curr_scp_idx)) == Module) {
         /* Don't convert anything else from a module; it's not needed. */
         goto EXIT;
      }
      else if (AT_REFERENCED(attr_idx) == Not_Referenced &&
          !ATP_DCL_EXTERNAL(attr_idx)) {
         /* Program units get converted only if they're referenced or */
         /* declared EXTERNAL. We do send interfaces if they're not   */
         /* referenced, however. EXTERNAL items are immortal, as they */
         /* are in CFT77.					      */
         goto EXIT;
      }
   }

   pgm_unit = ATP_PGM_UNIT(attr_idx);

   /* Build a function table entry. */
   if (mif_attr_map[attr_idx].tag != mtag_func) {
      mif_attr_map[attr_idx].tag = mtag_func;
      mif_attr_map[attr_idx].val = mifalloc[mtag_func](&msp);
   }

   pos = mpos_null;
   pos.line = source_position(AT_DEF_LINE(attr_idx));
   pos.src = srcix;
   pos.col = AT_DEF_COLUMN(attr_idx);

   funcix = mif_attr_map[attr_idx].val;
   msp.func[funcix].lang = mlang_F90;
   msp.func[funcix].pos = pos;

   /* Flags */
   if (attr_idx == glb_tbl_idx[Buffer_In_Attr_Idx]) {
      msp.func[funcix].flags |= mfuncflag_buffer_in;
   }
   else if (attr_idx == glb_tbl_idx[Buffer_Out_Attr_Idx]) {
      msp.func[funcix].flags |= mfuncflag_buffer_out;
   }

   if (AT_ACTUAL_ARG(attr_idx)) {
      msp.func[funcix].flags |= mfuncflag_passed;
   }

   if (AT_REFERENCED(attr_idx) == Not_Referenced && 
       ATP_DCL_EXTERNAL(attr_idx) &&
       ATP_PGM_UNIT(attr_idx) == Pgm_Unknown) {
      msp.func[funcix].flag2s |= mfuncflag2_unknown;
   }

# if defined(_ACCEPT_CMD_k)

   if (cmd_line_flags.solaris_profile) {
      msp.func[funcix].flags |= mfuncflag_flowtrace;
   }

# elif defined(_ACCEPT_FLOW)

   if (cdir_switches.flow) {
      msp.func[funcix].flags |= mfuncflag_flowtrace;
   }
# endif

   if (ATP_PROC(attr_idx) == Intrin_Proc) {
      msp.func[funcix].flags |= mfuncflag_intrinsic;
   }

   switch (pgm_unit) {
   case Module:
      msp.func[funcix].flags |= mfuncflag_module;
      break;

   case Program:
      msp.func[funcix].flags |= mfuncflag_main;
      break;
   }

   if (call_type == Definition) {
      msp.func[funcix].flags |= mfuncflag_defined;
   }

   /* Names */
   msp.func[funcix].name = mnpool(&msp, AT_OBJ_NAME_PTR(attr_idx));
   if (ATP_PROC(attr_idx) == Intrin_Proc) {
      msp.func[funcix].genericname =
	 mnpool(&msp, AT_OBJ_NAME_PTR(ATP_INTERFACE_IDX(attr_idx)));
   }
   if (ATP_EXT_NAME_IDX(attr_idx) != NULL_IDX &&
       ATP_EXT_NAME_IDX(attr_idx) != AT_NAME_IDX(attr_idx)) {
      msp.func[funcix].extname = mnpool(&msp, ATP_EXT_NAME_PTR(attr_idx));
   }

   if ((ATP_PROC(attr_idx) == Module_Proc) || 
       (ATP_PROC(attr_idx) == Intern_Proc)) {
      if (call_type == Definition) {
         parent_attr = SCP_ATTR_IDX(SCP_PARENT_IDX(curr_scp_idx));
         msp.func[funcix].within = mif_attr_map[parent_attr].val;
      }
      else if (ATP_SCP_IDX(attr_idx) != NULL_IDX) {
         parent_attr = SCP_ATTR_IDX(SCP_PARENT_IDX(ATP_SCP_IDX(attr_idx)));
         msp.func[funcix].within = mif_attr_map[parent_attr].val;
      }
   }

   if (ATP_SCP_IDX(attr_idx) != NULL_IDX &&
       ATP_HAS_TASK_DIRS(SCP_ATTR_IDX(ATP_SCP_IDX(attr_idx)))) {
      msp.func[funcix].flags |= mfuncflag_hastaskdir;
   }

   if (ATP_ALT_ENTRY(attr_idx)) {
      msp.func[funcix].primary = mif_attr_map[alt_entry_idx].val;
   }

   /* Function result variable */

   if (!ATP_EXTRA_DARG(attr_idx) &&
       (pgm_unit == Function || ATP_HAS_ALT_RETURN(attr_idx))) {

      if (ATP_HAS_ALT_RETURN(attr_idx)) {
         rslt_idx = ATP_RSLT_IDX(attr_idx);
         pgm_unit = Function;
      }
      else {
         rslt_idx = ATP_RSLT_IDX(attr_idx);
      }

      if (call_type == Definition || call_type == Parent &&
          SB_HOSTED_STACK(ATD_STOR_BLK_IDX(rslt_idx))) {
         cvrt_attr_ntry(rslt_idx);
      }
   }


   /* Build a prototype */
   type = *mtype_null [mtypeclass_func];
   if (ATP_VFUNCTION(attr_idx)) {
      msp.func[funcix].flags |= mfuncflag_vfunc;
   }

   if (ATP_NOSIDE_EFFECTS(attr_idx)) {
      msp.func[funcix].flags |= mfuncflag_pure;
   }

   if (ATP_RECURSIVE(attr_idx)) {
      msp.func[funcix].flags |= mfuncflag_recursive;
   }

   if (ATP_INLINE_ALWAYS(attr_idx)) {
      msp.func[funcix].flags |= mfuncflag_inline;
   }

   if (ATP_INLINE_NEVER(attr_idx)) {
      msp.func[funcix].flags |= mfuncflag_noinline;
   }

   if (on_off_flags.recursive &&
       (ATP_PGM_UNIT(attr_idx) == Function || 
        ATP_PGM_UNIT(attr_idx) == Subroutine)) {
      msp.func[funcix].flags |= mfuncflag_recursive;
   }

   if (call_type == Definition && 
       (opt_flags.over_index | 
                  ATP_HAS_OVER_INDEXING(SCP_ATTR_IDX(curr_scp_idx)))) {
      msp.func[funcix].flags |= mfuncflag_overindex;
   }

   /* Determine return type */
   if (pgm_unit == Function && !ATP_EXTRA_DARG(attr_idx)) {
      type.mfunc.base = get_type_idx(ATP_RSLT_IDX(attr_idx));
   }
   else if (pgm_unit == Subroutine && ATP_HAS_ALT_RETURN(attr_idx)) {
      type.mfunc.base = get_basic_type(CG_INTEGER_DEFAULT_TYPE);
   }
   else {
      type.mfunc.base = get_basic_type(NONE);
   }

   if (ATP_EXTRA_DARG(attr_idx)) {
      msp.func[funcix].flags |= mfuncflag_result_is_arg;
   }

   if (ATP_ARGCHCK_CALL(attr_idx)) {
      msp.func[funcix].flags |= mfuncflag_has_dcheck_arg;
   }

   if (pgm_unit == Subroutine || pgm_unit == Function) {
      if ((ATP_PROC(attr_idx) == Intrin_Proc) ||
          ((call_type == Imported) &&
          (!ATP_IN_INTERFACE_BLK(attr_idx)))) {
         type.mfunc.flags |= mtypeflag_arg_mystery;
      }
      else {
         type.mfunc.list = cvrt_darg_list(attr_idx);

      }
   }

   if (ATP_DCL_EXTERNAL(attr_idx) ||
       (ATP_PGM_UNIT(attr_idx)==Module &&
        (ATP_SCP_IDX(attr_idx)!=curr_scp_idx ||
         !ATP_IN_CURRENT_COMPILE(attr_idx)))) {
      type.mfunc.flags |= mtypeflag_external;
      type.mfunc.flags |= mtypeflag_arg_mystery;
   }

   msp.func[funcix].type = mtype_lookup(&msp, &type);

   /* Establish back links from formal arguments to prototype */
   for (i = type.mfunc.list; VALID(i); i = msp.sym2[i].next) {
      msp.sym2[i].base = msp.func[funcix].type;
   }


   /* Recursively convert alternate entry points. Set ATP_SCP_ALIVE so if */
   /* there are any bounds temps, they will get the right storage block.  */
   if (alt_entry_idx != NULL_IDX && !ATP_ALT_ENTRY(attr_idx)) {
      main_entry_idx = attr_idx;
      while (alt_entry_idx != NULL_IDX) {
         attr_idx = AL_ATTR_IDX(alt_entry_idx);
         ATP_SCP_ALIVE(attr_idx) = TRUE;
         cvrt_proc(attr_idx, main_entry_idx, call_type);
         ATP_SCP_ALIVE(attr_idx) = FALSE;
         alt_entry_idx = AL_NEXT_IDX(alt_entry_idx);
      }
   }


EXIT:
   TRACE (Func_Exit, "cvrt_proc", NULL);

} /* cvrt_proc */




/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Construct an intermediate type table entry for a derived type.	      *|
|*									      *|
|* Input parameters:							      *|
|*	derived type attribute index					      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/
static int  cvrt_derived_type(int	dt_attr_idx)

{
   mtype_t	typ;
   int		attr_idx;
   int		dt_idx;
   int		idx;
   int		sn_idx;
   int		type_idx;
   int		sym2ix;
   int		last_sym2_idx = NONE;


   TRACE (Func_Entry, "cvrt_derived_type", NULL);

   while (AT_ATTR_LINK(dt_attr_idx) != NULL_IDX) {
      dt_attr_idx = AT_ATTR_LINK(dt_attr_idx);
   }

   /* If type already defined, return mapped value */
   if (mif_attr_map[dt_attr_idx].tag != mtag_none) {
      return(mif_attr_map[dt_attr_idx].val);
   }

   /* Since aggregate may reference itself, need to allocate entry for
   ** aggregate up front, even though members are not yet defined. */
   type_idx = mifalloc[mtag_type](&msp);
   mif_attr_map[dt_attr_idx].tag = mtag_type;
   mif_attr_map[dt_attr_idx].val = type_idx;
   msp.type[type_idx].maggr.class = mtypeclass_aggr;
   if (ATT_CHAR_SEQ(dt_attr_idx)) {
      typ = *mtype_null[mtypeclass_fchar];
      typ.mfchar.size = mint(&msp, msp.immtype, (unsigned long) CHAR_BIT);
      typ.mfchar.prec = CHAR_BIT;
      msp.type[type_idx].maggr.base = mtype_lookup(&msp, &typ);
   }
   else {
      typ = *mtype_null[mtypeclass_raw];
      typ.mraw.size = mint(&msp, msp.immtype, 
                      (unsigned long) TARGET_BITS_PER_WORD);
      msp.type[type_idx].maggr.base = mtype_lookup(&msp, &typ);
   }

   if (ATT_SEQUENCE_SET(dt_attr_idx)) {
      /* SEQUENCE statement specified within aggregate. */
      msp.type[type_idx].maggr.flags |= mtypeflag_sequence;
   }

   /* JBL - range issue here */

   msp.type[type_idx].maggr.size = mint(&msp, msp.immtype, 
           (unsigned long) CN_INT_TO_C(ATT_STRUCT_BIT_LEN_IDX(dt_attr_idx)));
   msp.type[type_idx].maggr.name = mnpool(&msp, AT_OBJ_NAME_PTR(dt_attr_idx));

   /* If the whole thing - all components is character and the whole thing is */
   /* sequenced then it's all byte_aligned, otherwise it's word aligned.      */

   /* walk all members of aggregate - put into sym2 table */
   for (sn_idx = ATT_FIRST_CPNT_IDX(dt_attr_idx);
	sn_idx != NULL_IDX;
	sn_idx = SN_SIBLING_LINK(sn_idx)) {

      attr_idx = SN_ATTR_IDX(sn_idx);

      /* get a new sym2 entry */
      sym2ix = mifalloc[mtag_sym2](&msp);
      mif_attr_map[attr_idx].tag = mtag_sym2;
      mif_attr_map[attr_idx].val = sym2ix;
      msp.sym2[sym2ix].name = mnpool(&msp, AT_OBJ_NAME_PTR(attr_idx));

      /* JBL - Range issue here, */

# if defined(_DEBUG)

      if (ATD_OFFSET_FLD(attr_idx) != CN_Tbl_Idx) {
         PRINTMSG(AT_DEF_LINE(attr_idx), 1201, Internal,
                  AT_DEF_COLUMN(attr_idx), AT_OBJ_NAME_PTR(attr_idx));
      }
# endif

      msp.sym2[sym2ix].offset = mint(&msp, msp.immtype,
	      (unsigned long) CN_INT_TO_C(ATD_CPNT_OFFSET_IDX(attr_idx)));
      idx = get_type_idx(attr_idx);
      msp.sym2[sym2ix].type = idx;
      msp.sym2[sym2ix].base = type_idx;

      if (VALID(last_sym2_idx)) {
	 msp.sym2[last_sym2_idx].next = sym2ix;
      }
      else {
	 /* have aggregate type reference first member */
         msp.type[type_idx].maggr.list = sym2ix;
      }
      last_sym2_idx = sym2ix;

   } /* for all members of aggregate */


   TRACE (Func_Exit, "cvrt_derived_type", NULL);

   return type_idx;

}   /* cvrt_derived_type */




/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Construct a basic block for a label.				      *|
|*									      *|
|* Input parameters:							      *|
|*	Label attribute index						      *|
|*									      *|
|* Output parameters:							      *|
|*	Basic block index						      *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/
static int cvrt_label(int	 attr_idx,
                      mopnflag_t flags,
                      mpos_t	 pos)

{
   int			blkix;
   int			il_idx;
   mopd_t	        list, opd0, opd1, opd;
   int			safevl_idx;
   mtype_t              typ;
   int			unroll_idx;


   TRACE (Func_Entry, "cvrt_label", NULL);

   if (mif_attr_map[attr_idx].tag == mtag_none) {
      blkix = mifalloc[mtag_blk](&msp);
      msp.blk[blkix].scope = local_scope;
      msp.blk[blkix].dbgclass = ATL_DEBUG_CLASS(attr_idx);
      mif_attr_map[attr_idx].tag = mtag_blk;
      mif_attr_map[attr_idx].val = blkix;
   }
   else {
      blkix = mif_attr_map[attr_idx].val;
   }
 
   if (cmd_line_flags.debug_lvl <= Debug_Lvl_1 ||  /* -ez -ed -G0 -G1 */
       ATL_IN_ASSIGN(attr_idx)) {
      msp.blk[blkix].flags |= mblkflag_precious;
   }

   if (ATL_ALIGN(attr_idx)) {
      msp.blk[blkix].flags |= mblkflag_alignblk;
   }

   opd0.tag = mtag_imm;
   opd0.val = 0;

   if (ATL_PREFERVECTOR(attr_idx)) opd0.val |= mloopflag_prefer_vector;
   if (ATL_NEXTSCALAR(attr_idx))   opd0.val |= mloopflag_nextscalar;
   if (ATL_IVDEP(attr_idx))	   opd0.val |= mloopflag_ivdep;
   if (ATL_SPLIT(attr_idx))	   opd0.val |= mloopflag_streamsplit;
   if (!ATL_BL(attr_idx))	   opd0.val |= mloopflag_nobl;
   if (ATL_CNCALL(attr_idx))	   opd0.val |= mloopflag_cncall;

   /* ATL_UNROLL_DIR is set to the following criteria from PDGCS:        */
   /* mif uses !mloopflag_nounroll to set FEI_LABDEF_UNROLL.             */

   /* fei_new_labdef(), opt_flags, FEI_LABDEF_UNROLL                     */

   /*   Set this to TRUE if                                              */
   /*              ( ( "-Ounroll2" is enabled )                          */
   /*           OR ( ( there is a [NO]UNROLL directive for the label )   */
   /*                AND ( "-Ounroll1" is enabled ) )                    */

   if (!ATL_UNROLL_DIR(attr_idx))  opd0.val |= mloopflag_nounroll;
   if (ATL_NORECURRENCE(attr_idx)) opd0.val |= mloopflag_noreduce;
   if (ATL_NOVSEARCH(attr_idx))	   opd0.val |= mloopflag_novsearch;
   if (ATL_NOVECTOR(attr_idx)) 	   opd0.val |= mloopflag_novector;
   if (ATL_NOTASK(attr_idx))	   opd0.val |= mloopflag_notask;
   if (opt_flags.loopalign)        opd0.val |= mloopflag_align;
   if (ATL_TOP_OF_LOOP(attr_idx))  opd0.val |= mloopflag_loopchk;
   if (ATL_PREFERTASK(attr_idx))   opd0.val |= mloopflag_prefer_task;

   /* List contains directives in this order           */
   /* safevl, Unroll, mark name, maxcpus, cache bypass */
   /* Only need safevl and unroll here.                */

   safevl_idx           = NULL_IDX;
   unroll_idx           = NULL_IDX;

   if (ATL_DIRECTIVE_LIST(attr_idx) != NULL_IDX) {
      il_idx = IL_IDX(ATL_DIRECTIVE_LIST(attr_idx)) + Safevl_Dir_Idx;

      if (IL_FLD(il_idx) == CN_Tbl_Idx) {
         safevl_idx     = IL_IDX(il_idx);
      }

      il_idx = IL_IDX(ATL_DIRECTIVE_LIST(attr_idx)) + Unroll_Dir_Idx;

      if (IL_FLD(il_idx) == CN_Tbl_Idx) {
         unroll_idx     = IL_IDX(il_idx);
      }
   }

   /* If at a definition of a label in the IR. */

   if (((pos.line != 0) && 
        (ATL_TOP_OF_LOOP(attr_idx))) ||

       ((pos.line != 0) && 
        (ATL_CLASS(attr_idx) == Lbl_User) && 
        (!ATL_IN_ASSIGN(attr_idx)) &&
        (ATL_EXECUTABLE(attr_idx)))) {

      if (ATL_CLASS(attr_idx) == Lbl_Format ||
          (ATL_CLASS(attr_idx) <= Lbl_User && !ATL_EXECUTABLE(attr_idx))) {
         /* Intentionally blank */
      }
      else {
         list = mopd_null;
         if  (ATL_UNROLL_DIR(attr_idx) ||
              ATL_IVDEP(attr_idx) ||
              ATL_SHORTLOOP(attr_idx) ||
              ATL_SHORTLOOP128(attr_idx)) {

            if (ATL_SHORTLOOP(attr_idx)) {
               opd.tag = mtag_imm;
               opd.val = 64;
            }
            else if (ATL_SHORTLOOP128(attr_idx)) {
               opd.tag = mtag_imm;
               opd.val = 128;
            }
            else {
               opd = mopd_null;
            }

            list = mif_opn_add(&msp.blk[blkix],
                        mop_list, 
                        mopdtype(&msp, opd, msp.blk[blkix].opn), 
                        pos, 
                        flags,
                        opd, 
                        mopd_null,
                        mopd_null);

            if (ATL_IVDEP(attr_idx)) {
               opd.tag = mtag_imm;
               opd.val = CN_INT_TO_C(safevl_idx);
            }
            else {
               opd = mopd_null;
            }

            list = mif_opn_add(&msp.blk[blkix],
                        mop_list, 
                        mopdtype(&msp, opd, msp.blk[blkix].opn),
                        pos, 
                        flags,
                        opd, 
                        list, 
                        mopd_null);

            if (ATL_UNROLL_DIR(attr_idx)) {
               opd.tag = mtag_imm;
               opd.val = CN_INT_TO_C(unroll_idx);
            }
            else {
               opd = mopd_null;
            }

            list = mif_opn_add(&msp.blk[blkix],
                        mop_list, 
                        mopdtype(&msp, opd, msp.blk[blkix].opn),
                        pos, 
                        flags,
                        opd, 
                        list, 
                        mopd_null);
         }

         mif_opn_add(&msp.blk[blkix],
                     mop_loopopt, 
                     get_basic_type(NONE),
                     pos, 
                     flags,
	  	     opd0, 
                     list,
                     mopd_null);
      }
   }

	 /* BECKER - Hack code to be removed once frontend
          * clearly marks the end of a parallel loop.
	  *
	  * Code to end Doparallel or Doall tasking region.
	  */
   else if (!msp.blk[blkix].opn_ct) {
	 /* First time blk encountered becuase not operations */
	 if (VALID(task_region_top) &&
	     parallel_loop_end_label_idx==attr_idx &&
	     (msp.taskreg[task_region_stk[task_region_top]].regionclass==
		mregionclass_doall ||
	      msp.taskreg[task_region_stk[task_region_top]].regionclass==
		mregionclass_loop)) {

	       	opd0.tag = mtag_taskreg;
	        opd0.val = task_region_stk[task_region_top--];
		mif_opn_add(&msp.blk[blkix], 
                            mop_tregend,
		 	    get_basic_type(NONE), 
                            pos,
			    flags, opd0, 
                            mopd_null, 
                            mopd_null);

		loop_tregend_blk_idx = blkix;
		loop_tregend_opn_idx = msp.blk[blkix].opn_ct-1;
		loop_region_idx = opd0.val;
	 }
   }

   msp.blk[blkix].name = mnpool(&msp, AT_OBJ_NAME_PTR(attr_idx));

   TRACE (Func_Exit, "cvrt_label", NULL);

   return(mif_attr_map[attr_idx].val);

}   /* cvrt_label */



/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Represent an attribute entry in the intermediate form.		      *|
|*									      *|
|* Input parameters:							      *|
|*	Attribute index							      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	mapped value (also placed in global mif_attr_map array)		      *|
|*									      *|
\******************************************************************************/
static int cvrt_attr_ntry(int	attr_idx)

{
   int		array_idx;
   int		child_idx;
   int		class;
   long		constant[2]		= {0L, 0L};
   long         flags                   = 0;
   int		i;
   int		j;
   int		ptr_idx			= NULL_IDX;
   int		sb_idx;
   int		sn_idx;
   int		const_idx;
   int		symix;
   int		sbsymix;
   int		ptrsymix;
   mstorage_t	container_storage;
   int		local_sb_idx;
   int		parent_idx;
   mtype_t	type;
   int		host_sb_idx = NULL_IDX;
   mtag_t	sym_tag;
   msym_t	sym			= mgsym_null;
   mtype_t 	typ;
   mopd_t	opd0;
   mpos_t	pos;

   TRACE (Func_Entry, "cvrt_attr_ntry", NULL);

   /* If previously computed, return known value. */
   if (mif_attr_map[attr_idx].tag != mtag_none) {
      return(mif_attr_map[attr_idx].val);
   }

   child_idx = attr_idx;
   while (AT_ATTR_LINK(attr_idx) != NULL_IDX) {
      attr_idx = AT_ATTR_LINK(attr_idx);
   }

   pos = mpos_null;
   pos.line = source_position(AT_DEF_LINE(attr_idx));
   pos.src = srcix;
   pos.col = AT_DEF_COLUMN(attr_idx);
   sym.pos = pos;


   switch (AT_OBJ_CLASS(attr_idx)) {

   case Data_Obj:

      sym.fill = mfill_none;
      sym.align = TARGET_BITS_PER_WORD;
      sb_idx = ATD_STOR_BLK_IDX(attr_idx);

      switch (ATD_CLASS(attr_idx)) {

      case Atd_Unknown:
      case Variable:
      case Compiler_Tmp: 
         if (ATD_AUTOMATIC(attr_idx)) {
            sym.storage = mstorage_auto;
	    sym.base.tag = mtag_lsym;
            sym.base.val = cvrt_attr_ntry(ATD_AUTO_BASE_IDX(attr_idx));
	    sym_tag = mtag_lsym;
         }
         break;

      case CRI__Pointee:
         sym.storage = mstorage_based;
         sym.base.val = cvrt_attr_ntry(ATD_PTR_IDX(attr_idx));
         sym.base.tag = mif_attr_map[ATD_PTR_IDX(attr_idx)].tag;

	 sym_tag = mtag_lsym;

         if (SB_BLK_TYPE(ATD_STOR_BLK_IDX(ATD_PTR_IDX(attr_idx))) == Static) {
	    sym_tag = mtag_gsym;
         }
         break;

      case Function_Result:
         sym.flags |= msymflag_funcresult;
         break;

      case Constant:
         const_idx = ATD_CONST_IDX(attr_idx);

         /* array OR aggregate constant */
         if (ATD_FLD(attr_idx) == AT_Tbl_Idx) {
            cvrt_attr_ntry(const_idx);
            goto EXIT;
         }

         symix = mifalloc[mtag_lsym](&msp);
         mif_attr_map[attr_idx].tag = mtag_lsym;
         mif_attr_map[attr_idx].val = symix;

         if (! AT_COMPILER_GEND(attr_idx)) {
            sym.flags |= msymflag_user;
         }
         msp.lsym[symix] = sym;
         msp.lsym[symix].scope = local_scope;
         msp.lsym[symix].storage = mstorage_const;
         msp.lsym[symix].type = get_basic_type(ATD_TYPE_IDX(attr_idx));

         cvrt_exp_to_mif(&opd0, 
                         0,
	                 const_idx, 
                         CN_Tbl_Idx,
		         mopnflag_syn,
                         value);

	 msp.lsym[symix].offset = opd0;
         msp.lsym[symix].name = mnpool(&msp, AT_OBJ_NAME_PTR(attr_idx));
	 goto EXIT;

      }  /* end switch */


      /* Type */
      sym.type = get_type_idx(attr_idx);

      /* Offset */

      /* JBL - Range issue here. */

      if (ATD_OFFSET_ASSIGNED(attr_idx)) {

# if defined(_DEBUG)

         if (ATD_OFFSET_FLD(attr_idx) != CN_Tbl_Idx) {
            PRINTMSG(AT_DEF_LINE(attr_idx), 1201, Internal,
                     AT_DEF_COLUMN(attr_idx), AT_OBJ_NAME_PTR(attr_idx));
         }
# endif
         sym.offset = mint(&msp,
                           msp.immtype,
                         (unsigned long) CN_INT_TO_C(ATD_OFFSET_IDX(attr_idx)));

      }

      /* Storage block (if necessary) */
      if ((sym.storage == mstorage_none) && (sym.base.tag == mtag_none)) {
         flags = 0;
         container_storage = mstorage_none;
         sym.storage = mstorage_member;

         switch (SB_BLK_TYPE(sb_idx)) {
         case Static :
	    sym_tag = mtag_gsym;
            if (SB_MODULE(sb_idx)) {
               container_storage = mstorage_module;
            }
            else {
	       /* Frontend places static data in a specially created common */
	       /* block so that nested procedures can access it.            */
               container_storage = mstorage_common;
            }
            break;
         case Stack :
         case Non_Local_Stack :
	    sym_tag = mtag_lsym;
            sym.storage = mstorage_stack;
            break;
         case Formal :
         case Non_Local_Formal :
	    sym_tag = mtag_lsym;
            sym.storage = mstorage_formal;
            break;
         case Based :
	    sym_tag = mtag_lsym;
            sym.storage = mstorage_based;
            break;
         case Static_Local :
         case Static_Named :
            sym_tag = mtag_gsym;
            container_storage = mstorage_static;
            sym.scope = local_scope;
            break;
         case Common :
	    sym_tag = mtag_gsym;
            container_storage = mstorage_common;
            break;
         case Extern :
	    sym_tag = mtag_gsym;
            container_storage = mstorage_extern;
            break;
         case Exported :
	    sym_tag = mtag_gsym;
            container_storage = mstorage_exported;
            break;
         case Task_Common :
	    sym_tag = mtag_gsym;
            container_storage = mstorage_taskcommon;
            break;
         case Soft_External :
	    sym_tag = mtag_gsym;
            container_storage = mstorage_soft;
            break;
         case Equivalenced :
	    sym_tag = mtag_lsym;
            container_storage = mstorage_equiv;
            break;
         default :
            PRINTMSG(1, 1044, Internal, 0, "unexpected storage block class");
         }


         if (SB_SCP_IDX(sb_idx) != curr_scp_idx) {

            if ((!SB_HOSTED_STACK(sb_idx)) &&
                (SB_BLK_TYPE(sb_idx) != Formal) &&
                (INVALID(mif_stor_blk_map[sb_idx]))) {

               local_sb_idx = srch_stor_blk_tbl(SB_NAME_PTR(sb_idx),
                                                SB_NAME_LEN(sb_idx),
                                                curr_scp_idx);

               if (VALID(mif_stor_blk_map[local_sb_idx])) {
                  mif_stor_blk_map[sb_idx] = mif_stor_blk_map[local_sb_idx];
               }
               else {
                  host_sb_idx = sb_idx;
                  sb_idx = local_sb_idx;
               }
            }
         }

         if ((sym.storage == mstorage_member) &&
             (sym.base.tag == mtag_none) &&
             (container_storage != mstorage_none)) {

            if (INVALID(mif_stor_blk_map[sb_idx])) {

               if (sym_tag == mtag_gsym) {
                  sbsymix = mifalloc[mtag_gsym](&msp);
                  if (container_storage == mstorage_static) {
                     msp.gsym[sbsymix].scope = local_scope;
                  }
               }
               else {
                  sbsymix = mifalloc[mtag_lsym](&msp);
                  if ((AT_HOST_ASSOCIATED(attr_idx)) ||
                      (mif_attr_map[SCP_ATTR_IDX(SB_SCP_IDX(sb_idx))].val !=
                       msp.deffunc)) {
                     msp.lsym[sbsymix].scope = host_scope;
                  }
                  else {
                     msp.lsym[sbsymix].scope = local_scope;
                  }
               }

               /* Create another symbol for the storage block. */
               mif_stor_blk_map[sb_idx] = sbsymix;
               if (host_sb_idx != NULL_IDX) {
                  mif_stor_blk_map[host_sb_idx] = sbsymix;
               }

               if (SB_BLK_TYPE(sb_idx) == Common) {  
                  flags |= msymflag_user_common;
               }

               if (SB_SAVED(sb_idx)) {
                  flags |= msymflag_save;
               }

               if (SB_HOSTED_STACK(sb_idx)) {
                  flags |= msymflag_child_ref | msymflag_child_def;
               }

               /* JBL - Range issue here */

# if defined(_DEBUG)

               if (SB_LEN_FLD(sb_idx) != CN_Tbl_Idx) {
                  PRINTMSG(SB_DEF_LINE(sb_idx), 1201, Internal, 
                           SB_DEF_COLUMN(sb_idx),
                           SB_NAME_PTR(sb_idx));
               }
# endif

               type = *mtype_null[mtypeclass_raw];
               type.mraw.size = mint(&msp,
                                     msp.immtype,
                        (unsigned long) (CN_INT_TO_C(SB_LEN_IDX(sb_idx))));

	       if (sym_tag == mtag_gsym) {
                  msp.gsym[sbsymix].flags = flags;
                  msp.gsym[sbsymix].name = mnpool(&msp, SB_NAME_PTR(sb_idx));
                  msp.gsym[sbsymix].storage = container_storage;
                  msp.gsym[sbsymix].type = mtype_lookup(&msp, &type);
               }
               else {
                  msp.lsym[sbsymix].flags = flags;
                  msp.lsym[sbsymix].name = mnpool(&msp, SB_NAME_PTR(sb_idx));
                  msp.lsym[sbsymix].storage = container_storage;
                  msp.lsym[sbsymix].type = mtype_lookup(&msp, &type);
               }
            }

	    sym.base.tag = sym_tag;
            sym.base.val = mif_stor_blk_map[sb_idx];
         }

         if (sym.storage == mstorage_formal) {
            /* Turn Fortran dummy arguments into based variables, pointees */
            /* of new restricted pointers that will be the actual formal   */
            /* arguments. This translation permits the intermediate        */
            /* language to have a uniform representation of arguments as   */
            /* a pass-by-value convention.                                 */

            ptrsymix = mifalloc[mtag_lsym](&msp);
            msp.lsym[ptrsymix].storage = mstorage_formal;
            msp.lsym[ptrsymix].type = get_ptr_type(AT_Tbl_Idx, attr_idx);
	    msp.lsym[ptrsymix].name = mnpool(&msp, AT_OBJ_NAME_PTR(attr_idx));
            if (AT_REF_IN_CHILD(attr_idx)) { 
               msp.lsym[ptrsymix].flags |= msymflag_child_ref;
            }
            if (AT_DEF_IN_CHILD(attr_idx)) {
               msp.lsym[ptrsymix].flags |= msymflag_child_def;
            }

            sym.storage = mstorage_based;
            sym.base.tag = mtag_lsym;
            sym.base.val = ptrsymix;

            if ((AT_HOST_ASSOCIATED(attr_idx)) ||
                (mif_attr_map[SCP_ATTR_IDX(SB_SCP_IDX(sb_idx))].val != 
                 msp.deffunc)) {
               msp.lsym[ptrsymix].scope = host_scope;
            }
            else {
               msp.lsym[ptrsymix].scope = local_scope;
            }
 
            /* This block of code updates the scopes of the */
            /* temps for character lengths. If the variable */
            /* is not referenced in a nested scope, the     */
            /* scope of the temps may not be correct.  This */
            /* code assures they are correct.               */
            if (TYP_FLD(ATD_TYPE_IDX(attr_idx)) == AT_Tbl_Idx) {
               j = TYP_IDX(ATD_TYPE_IDX(attr_idx));
               if (mif_attr_map[j].tag == mtag_lsym) {
                  msp.lsym[mif_attr_map[j].val].scope =
                  msp.lsym[ptrsymix].scope;
               }
               else if (mif_attr_map[j].tag == mtag_gsym) {
                  msp.gsym[mif_attr_map[j].val].scope =
                  msp.lsym[ptrsymix].scope;
               }
            }

            /* This block of code update the scopes of the  */
            /* temps created for arrays.   If the array     */
            /* is not referenced in a nested scope, the     */
            /* scope of the temps may not be correct.  This */
            /* code assures they are correct.               */
            if (ATD_ARRAY_IDX(attr_idx) != NULL_IDX) {
               array_idx = ATD_ARRAY_IDX(attr_idx);
               for (i = 1; i <= BD_RANK(array_idx); i++) {
                  if (BD_XT_FLD(array_idx, i) == AT_Tbl_Idx) {
                     j = BD_XT_IDX(array_idx, i);
                     if (mif_attr_map[j].tag == mtag_lsym) {
                        msp.lsym[mif_attr_map[j].val].scope = 
                        msp.lsym[ptrsymix].scope;
                     }
                     else if (mif_attr_map[j].tag == mtag_gsym) {
                        msp.gsym[mif_attr_map[j].val].scope = 
                        msp.lsym[ptrsymix].scope;
                     } 
                  }

                  if (BD_SM_FLD(array_idx, i) == AT_Tbl_Idx) {
                     j = BD_SM_IDX(array_idx, i);
                     if (mif_attr_map[j].tag == mtag_lsym) {
                        msp.lsym[mif_attr_map[j].val].scope = 
                        msp.lsym[ptrsymix].scope;
                     }
                     else if (mif_attr_map[j].tag == mtag_gsym) {
                        msp.gsym[mif_attr_map[j].val].scope = 
                        msp.lsym[ptrsymix].scope;
                     } 
                  }

                  if (BD_LB_FLD(array_idx, i) == AT_Tbl_Idx) {
                     j = BD_LB_IDX(array_idx, i);
                     if (mif_attr_map[j].tag == mtag_lsym) {
                        msp.lsym[mif_attr_map[j].val].scope = 
                        msp.lsym[ptrsymix].scope;
                     }
                     else if (mif_attr_map[j].tag == mtag_gsym) {
                        msp.gsym[mif_attr_map[j].val].scope = 
                        msp.lsym[ptrsymix].scope;
                     } 
                  }
               }
            }
         }
      }


      /* Alignment */
      if (TYP_TYPE(ATD_TYPE_IDX(attr_idx)) == Character) {
         if ((ATD_IN_COMMON(attr_idx)) || 
             (ATD_CLASS(attr_idx) == Struct_Component) ||
             (ATD_CLASS(attr_idx) == Function_Result)) {
            sym.align = CHAR_BIT;
         }
         else if (ATD_CLASS(attr_idx) == Function_Result &&
                  TYP_TYPE(ATD_TYPE_IDX(attr_idx)) == Structure) {
            sym.align = (ATT_CHAR_SEQ(TYP_IDX(ATD_TYPE_IDX(attr_idx))))
				? CHAR_BIT
				: TARGET_BITS_PER_WORD;
         }
      }
      else if (TYP_TYPE(ATD_TYPE_IDX(attr_idx)) == CRI_Ch_Ptr) {
         sym.align = storage_bit_size_tbl[CRI_Ch_Ptr_8]; 
      }


      /* I/O type code */
      if (ATD_CLASS(attr_idx) == Variable  ||
          ATD_CLASS(attr_idx) == Compiler_Tmp ||
          ATD_CLASS(attr_idx) == Dummy_Argument ||
          ATD_CLASS(attr_idx) == Function_Result ||
          ATD_CLASS(attr_idx) == CRI__Pointee) {

         make_io_type_code(ATD_TYPE_IDX(attr_idx), constant);

         typ = *mtype_null[mtypeclass_raw];
         typ.mraw.size = mint(&msp,
                              msp.immtype,
                              (unsigned long)TARGET_BITS_PER_WORD);

         sym.IOcode = mcon_lookup(&msp,
                                  mtype_lookup(&msp, &typ),
                                  (char *)constant,
                                  NONE);
      }


      /* Flags */
      if (ATD_CLASS(attr_idx) == Compiler_Tmp || 
          ATD_CLASS(attr_idx) == Variable) {
         sym.flags |= msymflag_addrtaken;
      }

      if (ATD_ARRAY_IDX(attr_idx) != NULL_IDX) {
         if (BD_ARRAY_CLASS(ATD_ARRAY_IDX(attr_idx)) == Assumed_Shape) {
            sym.flags |= msymflag_assumed_shape;
         }
      }

      if (ATD_SYMMETRIC(attr_idx))            sym.flags |= msymflag_symmetric;
      if (ATD_POINTER(attr_idx))              sym.flags |= msymflag_pointer;
      if (ATD_AUXILIARY(attr_idx))            sym.flags |= msymflag_auxstore;
      if (ATD_TARGET(attr_idx))               sym.flags |= msymflag_target;
      if (ATD_EQUIV(attr_idx))                sym.flags |= msymflag_equiv;
      if (ATD_ALLOCATABLE(attr_idx))          sym.flags |= msymflag_allocatable;
      if (ATD_PERMUTATION(attr_idx))          sym.flags |= msymflag_permuted;
      if (ATD_SAVED(attr_idx))                sym.flags |= msymflag_save;
      if (AT_DEFINED(attr_idx))               sym.flags |= msymflag_modified;
      if (ATD_CLASS(attr_idx) != Compiler_Tmp)sym.flags |= msymflag_user;
      if (AT_REF_IN_CHILD(attr_idx))          sym.flags |= msymflag_child_ref;
      if (AT_DEF_IN_CHILD(attr_idx))          sym.flags |= msymflag_child_def;

      sym.name = mnpool(&msp, AT_OBJ_NAME_PTR(attr_idx));

      symix = mifalloc[sym_tag](&msp);
      mif_attr_map[attr_idx].tag = sym_tag;
      mif_attr_map[attr_idx].val = symix;
      if (sym_tag == mtag_gsym) {
 	 msp.gsym[symix] = sym;
      }
      else {
         if ((AT_HOST_ASSOCIATED(attr_idx)) ||
             (mif_attr_map[SCP_ATTR_IDX(SB_SCP_IDX(sb_idx))].val != 
              msp.deffunc)) {
            sym.scope = host_scope;
         }
         else {
            sym.scope = local_scope;
         }

	 msp.lsym[symix] = sym;
      }

      break;


   case Pgm_Unit:
      if (mif_attr_map[attr_idx].tag != mtag_func) {
         cvrt_proc(attr_idx, NULL_IDX, Imported);
      }
      break;


   case Label:
      cvrt_label(attr_idx, 0, mpos_null);
      break;


   case Interface:

      if (ATP_PGM_UNIT(SCP_ATTR_IDX(curr_scp_idx)) != Module) {

	 /* This is an interface block that has the same name as one of */
         /* its program units.  The program unit has to go through the  */
         /* interface.                                                  */
         if (ATI_PROC_IDX(attr_idx) != NULL_IDX) {
            cvrt_attr_ntry(ATI_PROC_IDX(attr_idx));
         }

         /* Establish the generic name of the routines */
         sn_idx = ATI_FIRST_SPECIFIC_IDX(attr_idx);
         for (i = 0; i < ATI_NUM_SPECIFICS(attr_idx); i++) {
            if (SN_ATTR_IDX(sn_idx) != NULL_IDX) {
               cvrt_attr_ntry(SN_ATTR_IDX(sn_idx));
            
               /* don't copy name unless proc was actually sent */
               if (mif_attr_map[SN_ATTR_IDX(sn_idx)].tag != mtag_none) {
                  msp.func[mif_attr_map[SN_ATTR_IDX(sn_idx)].val].genericname =
                  mnpool(&msp, AT_OBJ_NAME_PTR(attr_idx));
               }
            }
            sn_idx = SN_SIBLING_LINK(sn_idx);
         }
      }
      break;


   case Derived_Type:
      cvrt_derived_type(attr_idx);
      break;


   }  /* End switch */

   mif_attr_map[child_idx] = mif_attr_map[attr_idx];

EXIT:

   TRACE (Func_Exit, "cvrt_attr_ntry", NULL);

   return(mif_attr_map[attr_idx].val);

}  /* cvrt_attr_ntry */




/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Interface to the constant folding routines for conversion in data     *|
|*	statement processing. TYPELESS constants are left typeless so that    *|
|*	multiple words may be initialized with single constants.	      *|
|*									      *|
|* Input parameters:							      *|
|*	incoming constant, its type table index, and target type index	      *|
|*									      *|
|* Output parameters:							      *|
|*	operand								      *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/
static void cvrt_const (char	*constptr,
			int	origtypeix,
			int	desttypeix,
			mopd_t	*opd)
{

   int		basic;
   char         cbuf[16000];

   TRACE (Func_Entry, "cvrt_const", NULL);

   basic = get_basic_type(desttypeix);

   if (TYP_TYPE(origtypeix) == Typeless) {
      basic = get_basic_type(origtypeix);
   }

   if (TYP_TYPE(origtypeix) != Typeless) {

      switch(TYP_TYPE(desttypeix)) {
	 case Character :
	    folder_driver(constptr,             /* first operand value */
                          origtypeix,           /* first operand type index */
                          NULL,                 /* second operand value */
                          NULL_IDX,             /* second operand type index */
	                  &cbuf,                /* result value */
                          &desttypeix,          /* result type index */
                          msp.scope[local_scope].start.line, 
                          0,                    /* col number of constant */
                          1,                    /* number of vararg arguments */
                          Cvrt_Opr);
            constptr = cbuf;
	    break;

	 case Integer :
	 case Logical :
	 case Real :
	 case Complex :
            if (TYP_LINEAR(origtypeix) != TYP_LINEAR(desttypeix)) {
	       folder_driver(constptr,          /* first operand value */
                          origtypeix,           /* first operand type index */
                          NULL,                 /* second operand value */
                          NULL_IDX,             /* second operand type index */
	                  &cbuf,                /* result value */
                          &desttypeix,          /* result type index */
                          msp.scope[local_scope].start.line, 
                          0,                    /* col number of constant */
                          1,                    /* number of vararg arguments */
                          Cvrt_Opr);
               constptr = cbuf;
	       break;
            }
      }
   }

   opd->tag = mtag_con;
   opd->val = mcon_lookup(&msp, basic, constptr, NONE);

   TRACE (Func_Exit, "cvrt_const", NULL);
} /* cvrt_const */



/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Fold an expression containing constants and DATA implied DO control   *|
|*	variables. Addressing operations are evaluated into bit offsets.      *|
|*									      *|
|* Input parameters:							      *|
|*	index and field type						      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	integer result							      *|
|*									      *|
\******************************************************************************/
static int	fold_exp(int		idx,
			 fld_type	field,
                         int		*baseattr,	/* base variable */
                         int		*fldattr,	/* base component */
			 int		*typeix)	/* MIF type index */
{

   int		res;
   int		i;
   int		vv;
   int		cn_idx;
   opnd_type    l_opnd;
   int          lb, str, off;
   int		next_idx;
   int		sym2ix;
   int		dim;
   int		atypeix, btypeix;

   TRACE (Func_Entry, "fold_exp", NULL);

/* BECKER - function is not TARGET sensitive.  Need to use arith.a */

   switch (field) {

      case CN_Tbl_Idx :
         res = CN_INT_TO_C(idx);
         if (typeix) {
            *typeix = get_basic_type(CN_TYPE_IDX(idx));
         }
         break;
   
      case AT_Tbl_Idx :
         for (i = 0; i <= do_control_idx; i++) {
            if (do_control_var[i] == idx) {
               res = implied_do_idx[i];
               if (typeix) {
                  *typeix = msp.immtype;
               }
               break;
            }
         }
         if (i > do_control_idx) {
	    res = 0;
            if (baseattr) {
               *baseattr = idx;
            }
            if (fldattr) {
               *fldattr = idx;
            }
            if (typeix) {
	       if (mif_attr_map[idx].tag == mtag_gsym) {
                  *typeix = msp.gsym[mif_attr_map[idx].val].type;
	       }
	       else {
                  *typeix = msp.lsym[mif_attr_map[idx].val].type;
	       }
            }
         }
         break;

      case IR_Tbl_Idx :

         switch (IR_OPR(idx)) { 

            case Plus_Opr :
                 res = fold_exp(IR_IDX_L(idx), IR_FLD_L(idx), 0, 0, 0) +
                       fold_exp(IR_IDX_R(idx), IR_FLD_R(idx), 0, 0, 0);
                 break;

            case Minus_Opr :
                 res = fold_exp(IR_IDX_L(idx), IR_FLD_L(idx), 0, 0, 0) -
                       fold_exp(IR_IDX_R(idx), IR_FLD_R(idx), 0, 0, 0);
                 break;

            case Mult_Opr :
                 res = fold_exp(IR_IDX_L(idx), IR_FLD_L(idx), 0, 0, 0) *
                       fold_exp(IR_IDX_R(idx), IR_FLD_R(idx), 0, 0, 0);
                 break;

            case Div_Opr :
                 res = fold_exp(IR_IDX_L(idx), IR_FLD_L(idx), 0, 0, 0) /
                       fold_exp(IR_IDX_R(idx), IR_FLD_R(idx), 0, 0, 0);
                 break;

            case Power_Opr :
                 res = (long)pow((double)fold_exp(IR_IDX_L(idx),
                                                  IR_FLD_L(idx), 0, 0, 0),
                                 (double)fold_exp(IR_IDX_R(idx),
                                                  IR_FLD_R(idx), 0, 0, 0));
                 break;

            case Uplus_Opr :
            case Paren_Opr :
                 res = fold_exp(IR_IDX_L(idx), IR_FLD_L(idx),
				baseattr, fldattr, typeix);
                 break;

            case Uminus_Opr :
                 res = -fold_exp(IR_IDX_L(idx), IR_FLD_L(idx), 0, 0, 0);
                 break;

            case Subscript_Opr :
            case Section_Subscript_Opr :
            case Whole_Subscript_Opr :

                 /* base variable */
		 res = fold_exp(IR_IDX_L(idx), IR_FLD_L(idx),
				baseattr, fldattr, &atypeix);
                 btypeix = msp.type[atypeix].marray.base;
                 if (typeix) {
                    *typeix = btypeix;
                 }

                 off = 0;
                 next_idx = IR_IDX_R(idx);
                 for (dim = 0; dim < msp.type[atypeix].marray.rank; dim++) {

                    COPY_OPND(l_opnd, IL_OPND(next_idx));
                    /* l_opnd will always be a scalar expression here */
                    vv = 1;
                    cn_idx = get_next_array_expr_element(&l_opnd,
                                                         &vv);
                    COPY_OPND(IL_OPND(next_idx), l_opnd);
                    i = CN_INT_TO_C(cn_idx);

                    if (msp.type[atypeix].marray.low[dim].tag != mtag_imm) {
                       lb = mif_con_to_host_long(&msp, 
                            msp.type[atypeix].marray.low[dim].val);
                    }
                    else {
                       lb = msp.type[atypeix].marray.low[dim].val;
                    }

                    if (msp.type[atypeix].marray.stride[dim].tag != mtag_imm) {
                       str = mif_con_to_host_long(&msp, 
                             msp.type[atypeix].marray.stride[dim].val);
                    }
                    else {
                       str = msp.type[atypeix].marray.stride[dim].val;
                    }

                    off += (i - lb) * str;
                    next_idx = IL_NEXT_LIST_IDX(next_idx);
                 }

		 /* compute bit offset from element offset, using right units */
                 while (VALID(msp.type[btypeix].u.base)) {
                    btypeix = msp.type[btypeix].u.base;
                 }
                 if (msp.type[btypeix].u.class == mtypeclass_fchar) {
                    off *= msp.type[btypeix].mfchar.prec;
                 }
                 else {
                    off *= TARGET_BITS_PER_WORD;
                 }
		 res += off;

	         break;

            case Whole_Substring_Opr :
            case Substring_Opr :
                 res = fold_exp(IR_IDX_L(idx), 
                                IR_FLD_L(idx),
				baseattr, 
                                fldattr, 
                                &btypeix);

                 if (typeix) {
                    *typeix = btypeix;
                 }

                 i = fold_exp(IL_IDX(IR_IDX_R(idx)), 
                              IL_FLD(IR_IDX_R(idx)),
                              0, 0, 0);

                 res += (i - 1) * msp.type[btypeix].mfchar.prec;
	         break;

            case Struct_Opr :
		 res = fold_exp(IR_IDX_L(idx), 
                                IR_FLD_L(idx),
				baseattr, 0, &atypeix);

                 if (fldattr) {
                    *fldattr = IR_IDX_R(idx);
                 }

                 sym2ix = mif_attr_map[IR_IDX_R(idx)].val;
                 res += msp.sym2[sym2ix].offset.val;

                 if (typeix) {
                    *typeix = msp.sym2[sym2ix].type;
                 }
	         break;

            default :
                 PRINTMSG(1, 1044, Internal, 0, "fold_exp: unknown operator");
         }
	 break;

      default :
          PRINTMSG(1, 1044, Internal, 0, "fold_exp: bad field type");
   }

   TRACE (Func_Exit, "fold_exp", NULL);

   return(res);
} /* fold_exp */



/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Process a DATA statement implied DO				      *|
|*									      *|
|* Input parameters:							      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/
static void	cvrt_data_impl_do(int		idx,
				  fld_type	field)
{
   int		l_idx;
   opnd_type    l_opnd;
   int		initix;
   int		bpinitix;
   int		cn_idx;
   int		count;
   int		listidx;
   int		start, end, step;	/* I=start,end,step */
   int		dir;
   int		baseattr;
   int		fldattr;
   int		typeix, blanktypeix;
   int		vv;
   long		t;
   mtype_t	type;
   long_type	the_constant[MAX_WORDS_FOR_NUMERIC];

   TRACE (Func_Entry, "cvrt_data_impl_do", NULL);

   switch (field) {

      case IL_Tbl_Idx :
         while (idx != NULL_IDX) {
            cvrt_data_impl_do(IL_IDX(idx), IL_FLD(idx));
            idx = IL_NEXT_LIST_IDX(idx);
         }
         break;

      case IR_Tbl_Idx :

         if (IR_OPR(idx) == Implied_Do_Opr) {

            do_control_idx++;

            /* Extract loop control information */
            listidx = IR_IDX_R(idx);
            do_control_var[do_control_idx] = IL_IDX(listidx);
	    listidx = IL_NEXT_LIST_IDX(listidx);
	    start = fold_exp(IL_IDX(listidx), IL_FLD(listidx), 0, 0, 0);
	    listidx = IL_NEXT_LIST_IDX(listidx);
	    end = fold_exp(IL_IDX(listidx), IL_FLD(listidx), 0, 0, 0);
	    listidx = IL_NEXT_LIST_IDX(listidx);
	    step = fold_exp(IL_IDX(listidx), IL_FLD(listidx), 0, 0, 0);
	    listidx = IL_NEXT_LIST_IDX(listidx);

            /* Iterate over the loop body */
	    dir = step > 0 ? 1 : -1;
            for (implied_do_idx[do_control_idx] = start;
	         dir * implied_do_idx[do_control_idx] <= dir * end;
		 implied_do_idx[do_control_idx] += step) {

                the_constant[0] = implied_do_idx[do_control_idx];

                if (listidx == NULL_IDX) {
                   /* intentionally blank */
                }
                else {
                   COPY_OPND(l_opnd, IL_OPND(listidx));
                   vv = implied_do_idx[do_control_idx];
                   cn_idx = get_next_array_expr_element(&l_opnd, &vv);
                   COPY_OPND(IL_OPND(listidx), l_opnd);

                   the_constant[0] = CN_INT_TO_C(cn_idx);
                }

# ifdef _TARGET32
                if (TYP_LINEAR(ATD_TYPE_IDX(do_control_var[do_control_idx]))
                                                 == Integer_8) {

                   the_constant[1] = the_constant[0];
                   the_constant[0] = 0;
                }
# endif
# ifdef KEY
                SET_LCV_CONST(do_control_var[do_control_idx],
                              (the_constant[0]),
                              num_host_wds[TYP_LINEAR(ATD_TYPE_IDX(
                                            do_control_var[do_control_idx]))],
                              num_host_wds[TYP_LINEAR(ATD_TYPE_IDX(
                                            do_control_var[do_control_idx]))]);
# else
                SET_LCV_CONST(do_control_var[do_control_idx],
                              (the_constant[0]),
                              num_host_wds[TYP_LINEAR(ATD_TYPE_IDX(
                                            do_control_var[do_control_idx]))]);
# endif

                cvrt_data_impl_do(IR_IDX_L(idx), IR_FLD_L(idx));
	    }

            do_control_idx--;

	 }
	 else {

	    /* Must be a variable reference; perform a single initialization */
	    initix = mifalloc[mtag_init](&msp);

	    /* Determine base variable, bit offset, and field size of */
	    /* this initialization.				      */
	    msp.init[initix].offset = fold_exp(idx, 
                                               field,
					       &baseattr, 
                                               &fldattr, 
                                               &typeix);

	    /* Get next constant */
            count = 1;
	    if (IL_FLD(data_value_idx) == IR_Tbl_Idx) {
	       cn_idx = IR_IDX_R(IL_IDX(data_value_idx));
               if (IR_FLD_R(IL_IDX(data_value_idx)) == IR_Tbl_Idx) {
                  count = CN_INT_TO_C(IR_IDX_L(cn_idx));
	          cn_idx = IR_IDX_R(cn_idx);
               }
	    }
	    else if (IL_FLD(data_value_idx) == CN_Tbl_Idx) {
	       cn_idx = IL_IDX(data_value_idx);
	    }

	    /* Convert to type of destination variable. */
	    cvrt_const((char *)&CN_CONST(cn_idx),
		       CN_TYPE_IDX(cn_idx),
		       ATD_TYPE_IDX(fldattr),
		       &msp.init[initix].val);

            if ((TYP_TYPE(CN_TYPE_IDX(cn_idx)) == Character) &&
                (TYP_TYPE(ATD_TYPE_IDX(fldattr)) != Integer)) {
               if ((IR_OPR(idx) == Substring_Opr) ||
                   (IR_OPR(idx) == Whole_Substring_Opr)) {
                  l_idx = IR_IDX_R(idx);
                  l_idx = IL_NEXT_LIST_IDX(l_idx);
                  l_idx = IL_NEXT_LIST_IDX(l_idx);
	          msp.init[initix].size = CN_INT_TO_C(IL_IDX(l_idx)) * CHAR_BIT;
               }
               else {
                  msp.init[initix].size =
                  CN_INT_TO_C(TYP_IDX(CN_TYPE_IDX(cn_idx))) * CHAR_BIT;
               }
            }
            else {
               if (msp.type[typeix].u.size.tag != mtag_imm) {
                  t = mif_con_to_host_long(&msp, 
                          msp.type[typeix].u.size.val);
               }
               else {
                  t = msp.type[typeix].u.size.val;
               }

	       msp.init[initix].size = t;
            }
	    msp.init[initix].count = count;
	    msp.init[initix].stride = msp.init[initix].size;

	    insert_init(initix, 
                        mif_attr_map[baseattr].tag,
		        mif_attr_map[baseattr].val);

	    /* Step to the next constant for the next initializer */
	    if (IL_FLD(data_value_idx) != IR_Tbl_Idx ||
		++data_values_consumed ==
		   /* rep ct */ CN_INT_TO_C(IR_IDX_L(IL_IDX(data_value_idx)))) {
		data_value_idx = IL_NEXT_LIST_IDX(data_value_idx);
	        data_values_consumed = 0;
	    }

	 }
         break;
   }

   TRACE (Func_Exit, "cvrt_data_impl_do", NULL);
} /* cvrt_data_impl_do */




/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Read a module information table from a file and package it as a	      *|
|*	intermediate language subprogram.				      *|
|*									      *|
|* Input parameters:							      *|
|*	Output file stdio pointer					      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/
static void write_mod_tbl_file_name (FILE *out_fp)
{

   int		 fp_idx;
   long		 end_ftell;
   char		*mod_info_fn;
   int		 mod_info_len;
   char		*mod_info_tab;
   FILE		*modfile;
   long		 start_ftell;
   mtype_t	 type;


   TRACE (Func_Entry, "write_mod_tbl_file_name", NULL);

   /* NOTE:  CRAY is the only implementation that requires the @%% stuff. */

   fp_idx	= ATP_MOD_PATH_IDX(SCP_ATTR_IDX(MAIN_SCP_IDX));

   if (!FP_OUTPUT_TO_O(fp_idx)) {
      return;  /* Do not want permanent module output */
   }


# if defined(_TARGET_OS_UNICOS)

   init_subprog_info (NULL_IDX);

   /* reate special name to identify module information. */

   msp.name	= mnpool(&msp, "@%%");

   /* Create a function returning void type. */

   type			= *mtype_null[mtypeclass_func];
   type.mfunc.base	= mtype_lookup(&msp, mtype_null[mtypeclass_void]);

   /* Create a function which holds the module information */

   msp.deffunc			= mifalloc[mtag_func](&msp);
   msp.func[msp.deffunc].name	= msp.name;
   msp.func[msp.deffunc].flags |= mfuncflag_module;
   msp.func[msp.deffunc].lang	= mlang_F90;
   msp.func[msp.deffunc].type	= mtype_lookup(&msp, &type);

   /* Output module information subprogram and */
   /* free memory of in-core representation.   */

# endif

   msp.modfile = mnpool(&msp, FP_NAME_PTR(fp_idx));

# if defined(_TARGET_OS_UNICOS)

   mifwrite (out_fp, &msp, cmd_line_flags.output_format, "/bin/cat");
   miffree (&msp);

#  endif

   TRACE (Func_Exit, "write_mod_tbl_file_name", NULL);

} /* write_mod_tbl_file_name */




/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Set up MIF subprogram header , src table, and product information     *|
|*									      *|
|* Input parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/

static void init_subprog_info(int	attr_idx) {

   /* only initialize environment information once */

   static char	targetname [MACHINENAMELEN];
   static struct stat	statbuf;

   int          act_file_line;
   int          i;
   int          j;
   mpos_t	pos;
   char         *act_file_name;
   char         *act_path_name;

# if defined(_HOST_OS_UNICOS) || defined(_HOST_OS_MAX)
   struct target data;
# else
   char *p;
#endif

   TRACE (Func_Entry, "init_subprog_info", NULL);


   if (src_path == 0) {

      /* first time function called, get environment information */

      gethostname(hostname, MACHINENAMELEN);

      src_path = get_src_path_name();
      stat (src_path, &statbuf);

# if defined(_HOST_OS_UNICOS) || defined(_HOST_OS_MAX)
         target(MC_GET_TARGET, &data);
         strcpy (targetname, (char *)&data.mc_pmt);
# else

      p = getenv("TARGET");

      if (p) {
	 strcpy (targetname, p);
      }
      else { /* If TARGET not set, assume same as host */
	 strcpy (targetname, hostname);
      }
# endif
   }

   msp = msubprog_null;

   /* Subprogram header information */
   msp.version = MIFVERS;

   /* command line info */
   if (cmd_line_flags.dalign) {
      msp.flags |= msubprogflag_dalign;
   }

   /* If this is a module procedure or internal procedure that has come */
   /* in via a USE statement and is being sent across only for inlining */
   /* purposes, mark is as reference only.                              */

   if (SCP_IS_USED_PROC(curr_scp_idx)) {
      msp.flags |= msubprogflag_refonly;
   }

   /* Initialize product table entry for frontend */
   F90_prod_mif_idx = mifalloc[mtag_prod](&msp);
   msp.prod[F90_prod_mif_idx].lang	= mlang_F90;
   msp.prod[F90_prod_mif_idx].component	= mnpool(&msp, "CFT90 frontend");
   msp.prod[F90_prod_mif_idx].config	= mnpool(&msp,
# ifdef _STANDALONE_FRONT_END
                         "standalone"
# else
                         "composite"
# endif
# ifdef _DEBUG
			 " debug"
# endif
# ifdef _TARGET32
			 " 32bit"
# endif
# ifdef CRAY
			 " Cray"
# endif
# ifdef SPARC
			 " SPARC"
# endif
			 );
   msp.prod[F90_prod_mif_idx].version = mnpool(&msp, frontend_version);


   /* Initialize source table entry for source file. */
   srcix = mifalloc[mtag_src](&msp);  
   act_file_name = GL_FILE_NAME_PTR(1);
   act_path_name = GL_PATH_NAME_PTR(1);
   msp.src[srcix].path = mnpool(&msp, act_path_name);
   msp.src[srcix].origname = mnpool(&msp, act_file_name);
   msp.src[srcix].host = mnpool(&msp, hostname);
   msp.src[srcix].time = statbuf.st_ctime;
   msp.src[srcix].product = F90_prod_mif_idx;
   msp.src[srcix].lines	= GL_SOURCE_LINES(1);
   GL_MIF_FILE_ID(1) = srcix;
   
   for (i = 2; i <= global_line_tbl_idx; i++) {
   
      if (GL_INCLUDE_FILE_LINE(i) != 0) {
         srcix = mifalloc[mtag_src](&msp);  
         act_file_name = GL_FILE_NAME_PTR(i);
         act_path_name = GL_PATH_NAME_PTR(i);
         msp.src[srcix].path = mnpool(&msp, act_path_name);
         msp.src[srcix].origname = mnpool(&msp, act_file_name);
         msp.src[srcix].host = mnpool(&msp, hostname);
         msp.src[srcix].time = statbuf.st_ctime;
         msp.src[srcix].product = F90_prod_mif_idx;
         msp.src[srcix].lines = GL_SOURCE_LINES(i);
         pos = mpos_null;
         pos.src = GL_MIF_FILE_ID(i-1);
         pos.line = GL_INCLUDE_FILE_LINE(i);
         pos.col = GL_INCLUDE_FILE_COL(i);
         msp.src[srcix].pos = pos;
         GL_MIF_FILE_ID(i) = srcix;
      }
      else {
         j = i-1;
         while (GL_CIF_FILE_ID(i) != GL_CIF_FILE_ID(j)) {
           j = j - 1;
         }
         GL_MIF_FILE_ID(i) = GL_MIF_FILE_ID(j);
      }
   }


   /* Initialize the options table. */
   optionix = mifalloc[mtag_option](&msp);      

   create_option_tbl();
   msp.option->target = mnpool(&msp, targetname);

   TRACE (Func_Exit, "init_subprog_info", NULL);

} /* init_subprog_info */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Set up MIF option table.                                              *|
|*									      *|
|* Input parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/
static void create_option_tbl(void)

{
   int		 disable_idx;
   int		 i;
   Uint		*mif_disable_msg_list = 0;
   int		 msg_num;


   TRACE (Func_Entry, "create_option_tbl", NULL);

   /* optimization levels */

   msp.option[optionix].inline_level	= opt_flags.inline_lvl;
   msp.option[optionix].vector_level	= opt_flags.vector_lvl;
   msp.option[optionix].scalar_level	= opt_flags.scalar_lvl;
   msp.option[optionix].task_level	= opt_flags.task_lvl;

   /* optmization control flags */

   msp.option[optionix].opt_flags	= 0;

   if (opt_flags.neg_msgs) {
      msp.option[optionix].opt_flags  |= moptflag_negmsg;
   }

   if (opt_flags.aggress) {
      msp.option[optionix].opt_flags	|= moptflag_aggress;
   }

   if (opt_flags.pattern) {
      msp.option[optionix].opt_flags	|= moptflag_pattern;
   }

   if (opt_flags.taskinner) {
      msp.option[optionix].opt_flags	|= moptflag_taskinner;
   }

   if (opt_flags.threshold) {
      msp.option[optionix].opt_flags	|= moptflag_threshold;
   }

   if (opt_flags.zeroinc) {
      msp.option[optionix].opt_flags	|= moptflag_zeroinc;
   }

   if (opt_flags.over_index) {
      msp.option[optionix].opt_flags	|= moptflag_overindex;
   }

   if (cmd_line_flags.runtime_conformance) {
      msp.option[optionix].opt_flags	|= moptflag_conform;
   }

   if (opt_flags.ieeeconform) {
      msp.option[optionix].opt_flags    |= moptflag_ieeeconform;
   }

   /* From PDGCS:                                            */
   /* PDGCS_init(), init_flags, PDGCS_INIT_MEM_HIER_OPT      */

   /*   Set this bit to TRUE if                              */ 
   /*           ( ( "-Ounroll2" is enabled )                 */

   /* mif uses this flag to set PDGCS_INIT_MEM_HIER_OPT      */


   if (opt_flags.unroll_lvl == Unroll_Lvl_2) {
      msp.option[optionix].opt_flags	|= moptflag_unroll;
   }

   if (opt_flags.split_lvl > Split_Lvl_0) {
      msp.option[optionix].opt_flags	|= moptflag_streamsplit;
   }

   /* Fortran does not have an unroll count.  Leave unset for now. */
   /* unsigned int unroll_count : 8;  */

   /* maximum number of ERRORs to emit */

   if (on_off_flags.abort_if_any_errors) {
      msp.option[optionix].error_limit	= 1;
   }
   else if (on_off_flags.abort_on_100_errors) {
      msp.option[optionix].error_limit	= 100;
   }
   else {
      msp.option[optionix].error_limit	= 0;  /* 0 means no limit. */
   }

   /* number of disabled messages */

   if (cmd_line_flags.num_msgs_suppressed) {
      MEM_ALLOC(mif_disable_msg_list, Uint, cmd_line_flags.num_msgs_suppressed);

      for (disable_idx = 0;
           disable_idx < cmd_line_flags.num_msgs_suppressed; 
           disable_idx++) {

         for (i = 0;  i < MAX_MSG_DISABLE_SIZE;  ++i) {

            if (msg_suppress_tbl[i] != 0) {

               for (msg_num = i * HOST_BITS_PER_WORD;
                    msg_num < (i + 1) * HOST_BITS_PER_WORD;
                    ++msg_num) {

                  if (GET_MSG_SUPPRESS_TBL(msg_num)) {
                     mif_disable_msg_list[disable_idx] = msg_num;
                 }
               }
            }
         }
      }
   }

   msp.option[optionix].disabled_msgs	 = mif_disable_msg_list;
   msp.option[optionix].disabled_msgs_ct = cmd_line_flags.num_msgs_suppressed;

   /* messages enabled  - Fortran does not accept this option  */
   /*                     unsigned int *enabled_msgs;          */

   msp.option[optionix].enabled_msgs_ct	= 0;

   /* message severity control */

   msp.option[optionix].msg_severityflags = mmsg_severityflag_internal |
                                            mmsg_severityflag_limit |
                                            mmsg_severityflag_log_Error |
                                            mmsg_severityflag_log_Summary |
                                            mmsg_severityflag_log_warning;

   if (on_off_flags.check_std) {  /* Issue ANSI */
      msp.option[optionix].msg_severityflags |= mmsg_severityflag_ansi;
   }

   if (cmd_line_flags.msg_lvl_suppressed == Error_Lvl) {
      msp.option[optionix].msg_severityflags |= mmsg_severityflag_error;
   }
   else if (cmd_line_flags.msg_lvl_suppressed == Warning_Lvl) {
      msp.option[optionix].msg_severityflags |= mmsg_severityflag_warning |
                                                mmsg_severityflag_error;
   }
   else if (cmd_line_flags.msg_lvl_suppressed == Caution_Lvl) {
      msp.option[optionix].msg_severityflags |= mmsg_severityflag_caution |
                                                mmsg_severityflag_warning |
                                                mmsg_severityflag_error;
   }
   else if (cmd_line_flags.msg_lvl_suppressed == Note_Lvl) {
      msp.option[optionix].msg_severityflags |= mmsg_severityflag_note    |
                                                mmsg_severityflag_caution |
                                                mmsg_severityflag_warning |
                                                mmsg_severityflag_error;
   }
   else if (cmd_line_flags.msg_lvl_suppressed == Comment_Lvl) {
      msp.option[optionix].msg_severityflags |= mmsg_severityflag_comment |
                                                mmsg_severityflag_note    |
                                                mmsg_severityflag_caution |
                                                mmsg_severityflag_warning |
                                                mmsg_severityflag_error;
   }


   if ((cif_flags & MESSAGE_RECS) || opt_flags.msgs) {
      msp.option[optionix].msg_severityflags |= mmsg_severityflag_info |
                                                mmsg_severityflag_vector |
                                                mmsg_severityflag_scalar |
                                                mmsg_severityflag_table |
                                                mmsg_severityflag_inline |
                                                mmsg_severityflag_tasking |
                                                mmsg_severityflag_optimization;
   }

   /* debugging level */

   if (cmd_line_flags.debug_lvl != 4) {
      msp.option[optionix].debug_level	= cmd_line_flags.debug_lvl;
   }

   /* tool control flags */

   msp.option[optionix].tool_flags	= 0;

   if (on_off_flags.MPP_apprentice) {
      msp.option[optionix].tool_flags	|= mtoolflag_apprentice;
   }

   if (on_off_flags.atexpert) {
      msp.option[optionix].tool_flags	|= mtoolflag_atexpert;
   }

   if (on_off_flags.flowtrace_option) {
      msp.option[optionix].tool_flags	|= mtoolflag_flowtrace;
   }

   if (cmd_line_flags.solaris_profile) {
      msp.option[optionix].tool_flags	|= mtoolflag_prof;
   }

   if (cmd_line_flags.runtime_argument ||
       cmd_line_flags.runtime_arg_entry) {
      msp.option[optionix].tool_flags	|= mtoolflag_dummyarg_check;
   }

   /* number of truncation bits */

   msp.option[optionix].trunc_bits	= cmd_line_flags.truncate_bits;

/* Don't know what this is. */


   /* arithmetic control flags */

   msp.option[optionix].arith_flags = 0;

   if (on_off_flags.enable_double_precision) {
      msp.option[optionix].arith_flags = marithflag_enable_double_precision;
   }

   if (on_off_flags.reciprical_divide) {
      msp.option[optionix].arith_flags |= marithflag_ieeedivide;
   }

   if (!on_off_flags.round_mult_operations) {

      /* Turn on truncation vs rounding.  Can have one or other. */

      msp.option[optionix].arith_flags |= marithflag_truncate_mode;
   }

   if (opt_flags.fastint) {
      msp.option[optionix].arith_flags |= marithflag_fastmd;
   }

   /* C only - Fortran does not have target commandline option. */
   /*          signed int target : 32;	 name pool index        */

   msp.option[optionix].target_flags	= 0;

   if (cmd_line_flags.dalign) {
      msp.option[optionix].target_flags	|= mtargetflag_dalign;
   }

   /* The -et option causes automatics to be allocated on stack  */
   /* rather than on the heap.                                   */

   if (on_off_flags.alloc_autos_on_stack) {
      msp.option[optionix].target_flags	|= mtargetflag_limit_heap;
   }

   if (on_off_flags.indef_init) {
      msp.option[optionix].target_flags	|= mtargetflag_indef;
   }

   msp.option[optionix].target_flags	|= mtargetflag_anytype_aliasing;

   if (on_off_flags.upper_case_names) {
      msp.option[optionix].target_flags	|= mtargetflag_sparc_upper_case;
   }

   if (cmd_line_flags.small_pic_model) {
      msp.option[optionix].target_flags	|= mtargetflag_little_pic_model;
   }

   if (cmd_line_flags.large_pic_model) {
      msp.option[optionix].target_flags	|= mtargetflag_big_pic_model;
   }

   return;

   TRACE (Func_Exit, "create_option_tbl", NULL);

} /* create_option_tbl */



/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Close the MIF file after all processing.                              *|
|*									      *|
|* Input parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/
void terminate_mif(void)

{
   char          msgbuf[512];


   TRACE (Func_Entry, "terminate_mif", NULL);

   if (MIF_fp) {

      if (mif_close_output(MIF_fp, MIF_file, msgbuf) == MIF_FALSE) {
         PRINTMSG(1, 1042, Error, 0, MIF_file);
      }
   }

   return;

   TRACE (Func_Exit, "terminate_mif", NULL);

} /* terminate_mif */

# endif
