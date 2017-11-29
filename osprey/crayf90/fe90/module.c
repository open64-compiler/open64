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


static char USMID[] = "\n@(#)5.0_pl/sources/module.c	5.17	09/30/99 15:47:54\n";

# include "defines.h"		/* Machine dependent ifdefs */

# include "host.m"		/* Host machine dependent macros.*/
# include "host.h"		/* Host machine dependent header.*/
# include "target.m"		/* Target machine dependent macros.*/
# include "target.h"		/* Target machine dependent header.*/

# include "globals.m"
# include "tokens.m"
# include "sytb.m"  
# include "p_globals.m"  
# include "debug.m"
# include "module.m"

# include "globals.h"
# include "tokens.h"
# include "sytb.h"
# include "p_globals.h"
# include "module.h"

# include <ar.h>

# include <sys/types.h>
# include <sys/stat.h>

# include <dirent.h>

# include <errno.h>

# if (defined(_TARGET_OS_IRIX) || defined(_TARGET_OS_LINUX) || defined(_TARGET_OS_DARWIN)) && defined(_MODULE_TO_DOT_o)
# include <fcntl.h>
# include <libelf.h>
# include <sys/elf.h>

  /* These are the originator string and .note type this program removes  */
  /* from files.                                                          */

# define NOTE_ORIG_NAME  "Cray Research, Incorporated\0"
# define NOTE_ORGNAM_LEN 28
# define NOTE_TYPE        1

# elif defined(_TARGET_OS_SOLARIS) && defined(_MODULE_TO_DOT_o)
# include <fcntl.h>
# include <libelf.h>
# include <sys/elf_SPARC.h>

  /* These are the originator string and .note type this program removes  */
  /* from files.                                                          */

# define NOTE_ORIG_NAME  "Cray Research, Incorporated\0"
# define NOTE_ORGNAM_LEN 28
# define NOTE_TYPE        1
# endif

# if !defined(AR_HDR_SIZE)
# define	AR_HDR_SIZE	sizeof(ar_hdr_type)
# endif

/******************************************************************************\
|*                                                                            *|
|* Notes if a new field is added to the symbol tables.                        *|
|*                                                                            *|
|*   1) Add it to the appropriate set_mod_link_tbl_for ... routine            *|
|*      This routine sets the KEEP_ME and IDX flags in the mod link table     *|
|*      so that the table entry gets kept during compression.                 *|
|*                                                                            *|
|*   2) Add it to update_idxs_in_attr_entry or compress_tbls for non attr     *|
|*      fields.  These routines do the actual compression and reset the       *|
|*      fields.  compress_tbls calls update_idxs_in_attr_entry.               *|
|*                                                                            *|
|*   3) Add it to update_new_idxs_after_input.  This routine sets the indexes *|
|*      after a module is read in.  When a module is in a file, all the table *|
|*      indexes are 1 based.  When it is read in, each new table is           *|
|*      concatenated to the existing table, so all the indexs need the table  *|
|*      size before reading added to the index.  This is generally the        *|
|*      location to correct for symbol table changes.                         *|
|*                                                                            *|
\******************************************************************************/


/******************************************************************************\
|*                                                                            *|
|* Steps for compressing tables.  (Both partial and full compressions.)       *|
|*                                                                            *|
|*   These routines allow for full and partial compressions.  (See #2 to      *|
|*   set indexes for a partial compression.  A full compression is everything *|
|*   in a table.  A partial compression starts at a given index in a table    *|
|*   and goes to the end of a table.  These routines will not do a compression*|
|*   in the middle of a table.                                                *|
|*                                                                            *|
|*   1) Allocate the mod_link_tbl.  This table is allocated so that it is as  *|
|*      big as the largest symbol/ir table.  If between marking the entries   *|
|*      in the mod_link_tbl and the actual compression, any tables grow       *|
|*      always make sure the mod_link_tbl size is still big enough to cover   *|
|*      the new entries.  No matter what table is being compressed, the mod_  *|
|*      link_tbl must be allocated as large as the largest table.             *|
|*                                                                            *|
|*   2) Set the zeroth entries in the mod_link_tbl to one less than the       *|
|*      starting index for compression.  If this is a full compression, then  *|
|*      they should be set to 0.  Anything in a table past this index is      *|
|*      subject to compression.                                               *|
|*                                                                            *|
|*   3) Call set_mod_link_tbl_for_attr  each attr entry that needs to be kept.*|
|*      If this is a partial compression, this only needs to be called for    *|
|*      those attr entries in the part of the attr table to be compressed.    *|
|*      This routine calls set_mod_link_tbl routines for other tables.  If the*|
|*      local name table is to be compressed, these entries will have to be   *|
|*      marked.  (See create_mod_info_tbl for how this is done for a full     *|
|*      compression.)  This routine marks each table entry that needs to be   *|
|*      kept in the mod_link_tbl, with its current index.  That way if the    *|
|*      item being compressed points into a part of the table not being       *|
|*      compressed, it will get the correct index out of the mod_link_tbl.    *|
|*                                                                            *|
|*   4) Call assign_new_idxs.  It will start at the point of compression for  *|
|*      each table.  If the mod_link_tbl is set for that entry, it will be    *|
|*      given a new index.  (The new index for a table starts at the index    *|
|*      where compression starts and is incremented each time a table entry   *|
|*      is kept.)  The new index is put into the mod_link_tbl for the entry.  *|
|*                                                                            *|
|*   5) Call compress_tbls to move the table entries to their new index spots.*|
|*      This is where actual compression takes place.  As the entries are     *|
|*      being moved, all their links are updated with the correct links from  *|
|*      the mod_link_tbl.                                                     *|
|*                                                                            *|
|*   Example:  Compress the attr_tbl starting at index 65.  attr_tbl_idx = 70 *|
|*             Entries  66, 68 and 70 are to be kept.                         *|
|*             1) Allocate mod_link_tbl as large as the largest table.        *|
|*             2) ML_AT_IDX(0) = 64  (65 - one)                               *|
|*                The rest of the tables mod_link_tbl[0] entries are set to   *|
|*                the current table indexes so that no other table compresses.*|
|*             3) Call set_mod_link_tbl_for_attr for attr entries 66, 68 & 70.*|
|*             4) Call assign_new_idxs.       Old Idx        New Idx          *|
|*                                              66             65             *|
|*                                              68             66             *|
|*                                              70             67             *|
|*             5) Call compress_tbls which moves the attr tbl entries.        *|
|*                attr_tbl_idx is set to 67.                                  *|
|*                                                                            *|
|*   NOTE:  compress_tbls and assign_new_idxs will go through all the tables, *|
|*      so in the above example, any references to attrs 66, 68 and 70 will   *|
|*      be reset.  If there are references to attrs 67 or 69, these           *|
|*      references will be set to NULL_IDX.  This is where alot of bugs are   *|
|*      found during module processing.  Full compressions are done when      *|
|*      files are written out for modules or inlining.  Partial compressions  *|
|*      are done when files for module or inlining are read in and also for   *|
|*	interface block compression.                                          *|
|*                                                                            *|
|*                                                                            *|
\******************************************************************************/

extern	boolean	is_directory(char *);

/*****************************************************************\
|* function prototypes of static functions declared in this file *|
\*****************************************************************/
static	void	allocate_mod_link_tbl (int);
static	void	assign_new_idxs (boolean);
static	void	assign_new_idxs_after_input (int);
static	void	check_ir_for_attrs (int);
static	void	check_il_for_attrs (int);
static	void	compress_tbls (int, boolean);
static	void	compress_type_tbl (int);

# if defined(_TARGET_OS_SOLARIS) && defined(_MODULE_TO_DOT_o)
static	boolean	do_elf_notes_section(Elf_Data *, int, int);
static	boolean	do_elf_object(Elf *, Elf32_Ehdr	*, int, int);
# endif

# if defined(_DEBUG)
static	void	dump_pdt(FILE *);
static	void	print_mod_tbl(void);
# endif

static	void	find_files_in_directory(int);
static	void	merge_interfaces (int, int);
static	void	not_visible_semantics (int, int, int);
static	int	ntr_file_in_fp_tbl(int, char *, int);
static	FILE   *open_module_file (int, int);
static	void	process_procs_for_inlining (int);
static	boolean	read_in_module_tbl (int, int, FILE *, char *);
static	boolean	read_module_tbl_header (int, int, FILE *);
static	boolean	read_sytb_from_module_file(int, FILE *, char *);
static	boolean	rename_only_semantics (int, boolean);
static	boolean	resolve_attr(int);
static	void	resolve_all_components(int, int);
static	void	resolve_used_modules (int);
static	void	set_attr_flds_for_output (void);
static	void	set_mod_link_tbl_for_attr (int);
static	void	set_mod_link_tbl_for_bd (int);
static	void	set_mod_link_tbl_for_cn (int);
static	void	set_mod_link_tbl_for_ir (int);
static	void	set_mod_link_tbl_for_il (int);
static	void	set_mod_link_tbl_for_typ (int);
static	boolean	srch_ar_file_for_module_tbl (int, int *, int, FILE *);
static	boolean	srch_for_module_tbl (int, int *, int, int, FILE *);
static	void	update_idxs_in_attr_entry (int, int);
static	void	update_intrinsic (int);

# if defined(_TARGET_OS_SOLARIS) && defined(_MODULE_TO_DOT_o)
static	boolean	srch_elf_file_for_module_tbl(int, int);
# endif


/***********************************\
|* Globals used only in this file  *|
\***********************************/

static	boolean	alternate_entry;
static	boolean	count_derived_types;
static	boolean	only_update_new_tbl_entries;
static  boolean	inline_search;
static	int	list_of_modules_in_module;
static	long	mod_file_end_offset;
static	long	num_module_derived_types;
static	int	save_const_pool_idx;
static	int	save_const_tbl_idx;
static	boolean	search_for_duplicate_attrs;

extern	char	compiler_gen_date[];

# if (defined(_HOST_OS_IRIX) || defined(_HOST_OS_LINUX) || defined(_HOST_OS_DARWIN))
# pragma inline set_mod_link_tbl_for_typ
# pragma inline set_mod_link_tbl_for_cn
# pragma inline set_mod_link_tbl_for_ir
# pragma inline set_mod_link_tbl_for_bd
# else
# pragma _CRI inline set_mod_link_tbl_for_typ
# pragma _CRI inline set_mod_link_tbl_for_cn
# pragma _CRI inline set_mod_link_tbl_for_ir
# pragma _CRI inline set_mod_link_tbl_for_bd
# endif


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	This routine enters a rename and/or only name for a USE statement     *|
|*	into the rename only table.  All entries are kept in sorted order.    *|
|*	Rename entries actually get two entries in the table.  The original   *|
|*	name entry is the sorted one.  The new name entry is indexed off the  *|
|*	original name entry.  The whole list of renames/only entries is       *|
|*	indexed by ATP_USE_LIST for the module.                               *|
|*									      *|
|* Input parameters:							      *|
|*	module_idx - The attr index for the module specified in the use stmt. *|
|*	ro_idx     - The new name index if this is a rename entry.   (This    *|
|*	             routine is called to enter the new name first.  The ro   *|
|*	             index is returned.  Then it is called again with the     *|
|*	             original name and the ro index.  An entry is made for    *|
|*	             the original name and the rename entry is hung off of it.*|
|*	rename_entry -> TRUE if this is the new name.  It doesn't need to be  *|
|*	                sorted or added to the list.                          *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	ro_idx just entered.						      *|
|*									      *|
\******************************************************************************/
int  make_ro_entry(int		module_idx,
                   int		ro_idx,
		   boolean	rename_entry)

{
   int		cmp_idx;
   int		matched;
   int		np_idx;
   int		prev_idx;


   TRACE (Func_Entry, "make_ro_entry", NULL);

   if (ro_idx == NULL_IDX) {
      ++rename_only_tbl_idx;
      CHECK_TBL_ALLOC_SIZE(rename_only_tbl, rename_only_tbl_idx);

      ro_idx = rename_only_tbl_idx;

      CLEAR_TBL_NTRY(rename_only_tbl, ro_idx);

      NTR_NAME_POOL(TOKEN_ID(token).words, TOKEN_LEN(token), np_idx);

      RO_LINE_NUM(ro_idx)	= TOKEN_LINE(token);
      RO_COLUMN_NUM(ro_idx)	= TOKEN_COLUMN(token);
      RO_NAME_LEN(ro_idx)	= TOKEN_LEN(token);
      RO_NAME_IDX(ro_idx)	= np_idx;
   }

   if (rename_entry) {   /* Do not sort - assume it is a rename entry */
      RO_RENAME_NAME(ro_idx)	= TRUE;
   }
   else {  /* Find a sorted spot for it */
      RO_RENAME_NAME(ro_idx)	= FALSE;

      if (ATP_USE_LIST(module_idx) == NULL_IDX) {
         ATP_USE_LIST(module_idx) = ro_idx;
      }
      else {
         cmp_idx	= ATP_USE_LIST(module_idx);
         prev_idx	= NULL_IDX;

         for (;;) {
            matched = compare_names(RO_NAME_LONG(cmp_idx),
                                    RO_NAME_LEN(cmp_idx),
                                    TOKEN_ID(token).words,
                                    TOKEN_LEN(token));

            if (matched >= 0) {  

               /* Name in table is same or greater than new name.  Add the */
               /* new name before the current index.                       */

               RO_NEXT_IDX(ro_idx) = cmp_idx;

               if (prev_idx == NULL_IDX) {
                  ATP_USE_LIST(module_idx) = ro_idx;
               }
               else {
                  RO_NEXT_IDX(prev_idx) = ro_idx;
               }
               break;
            }
            else {
               prev_idx	= cmp_idx;
               cmp_idx	= RO_NEXT_IDX(cmp_idx);

               if (cmp_idx == NULL_IDX) {  /* Add at end */
                  RO_NEXT_IDX(prev_idx) = ro_idx;
                  break;
               }
            }
         }
      }
   }

   TRACE (Func_Exit, "make_ro_entry", NULL);

   return(ro_idx);

}   /* make_ro_entry */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Search the renames table to see if this new name exists already.      *|
|*	This would happen if the same name was used to rename something twice.*|
|*	If it is found, RO_DUPLICATE_RENAME is set for both entries.  Errors  *|
|*	will be issued during use_stmt_semantics.                             *|
|*									      *|
|* Input parameters:							      *|
|*	rename_idx -> The ro index for the name to search for. 		      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/
void	check_for_duplicate_renames(int		rename_idx)

{
   int		ro_idx;


   TRACE (Func_Entry, "check_for_duplicate_renames", NULL);

   for (ro_idx = 1; ro_idx < rename_only_tbl_idx; ro_idx++) {

      if (RO_RENAME_NAME(ro_idx) &&
          (compare_names(RO_NAME_LONG(rename_idx),
                         RO_NAME_LEN(rename_idx),
                         RO_NAME_LONG(ro_idx),
                         RO_NAME_LEN(ro_idx)) == 0) && ro_idx != rename_idx) {
         RO_DUPLICATE_RENAME(rename_idx)	= TRUE;
         RO_DUPLICATE_RENAME(ro_idx)		= TRUE;
         break;
      }
   }

   TRACE (Func_Exit, "check_for_duplicate_renames", NULL);

   return;

}   /* check_for_duplicate_renames */

/******************************************************************************\
|*                                                                            *|
|* Description:                                                               *|
|*      Allocate and clear the module link table.  This is used for table     *|
|*      compression.                                                          *|
|*                                                                            *|
|* Input parameters:                                                          *|
|*      size -> Size to allocate.  If this is zero, we will calculate the     *|
|*              size to allocate, by finding the largest table.               *|
|*                                                                            *|
|* Output parameters:                                                         *|
|*      NONE                      				              *|
|*                                                                            *|
|* Returns:                                                                   *|
|*      NOTHING                      				              *|
|*                                                                            *|
\******************************************************************************/
static	void allocate_mod_link_tbl(int		size)

{
   long		*idx;
   long		 new_size;


   TRACE (Func_Entry, "allocate_mod_link_tbl", NULL);

   if (size == 0) {

      /* Find the largest table and allocate the mod link table to this size. */

      new_size = (attr_tbl_idx > bounds_tbl_idx)? attr_tbl_idx : bounds_tbl_idx;
      new_size = (new_size > const_tbl_idx)	? new_size : const_tbl_idx;
      new_size = (new_size > const_pool_idx)	? new_size : const_pool_idx;
      new_size = (new_size > loc_name_tbl_idx)	? new_size : loc_name_tbl_idx;
      new_size = (new_size > name_pool_idx)	? new_size : name_pool_idx;
      new_size = (new_size > sec_name_tbl_idx)	? new_size : sec_name_tbl_idx;
      new_size = (new_size > stor_blk_tbl_idx)	? new_size : stor_blk_tbl_idx;
      new_size = (new_size > type_tbl_idx)	? new_size : type_tbl_idx;
      new_size = (new_size > ir_tbl_idx)	? new_size : ir_tbl_idx;
      new_size = (new_size > ir_list_tbl_idx)	? new_size : ir_list_tbl_idx;
      new_size = (new_size > sh_tbl_idx)	? new_size : sh_tbl_idx;
   }
   else {
      new_size	= size;
   }

   new_size++;		/* Do not use entry 0, so increase size by 1 */

   CHECK_TBL_ALLOC_SIZE(mod_link_tbl, new_size);
   mod_link_tbl_idx	= mod_link_tbl_size - 1;

   idx = ((long *) (&mod_link_tbl[0]));

   memset(idx, 0, mod_link_tbl_size * NUM_ML_WDS * TARGET_BYTES_PER_WORD);

   TRACE (Func_Exit, "allocate_mod_link_tbl", NULL);

   return;

}  /* allocate_mod_link_tbl */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*									      *|
|* Input parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	false if we printed error 855 saying we won't create module file      *|
|*									      *|
\******************************************************************************/
#ifdef KEY /* Bug 3477 */
extern	boolean	create_mod_info_file(void)
#else
extern	void	create_mod_info_file(void)
#endif /* KEY Bug 3477 */
{
#ifdef KEY /* Bug 10177 */
   		int		 ga_idx = 0;
#else /* KEY Bug 10177 */
   		int		 ga_idx;
#endif /* KEY Bug 10177 */
		FILE		*fp_file_ptr;
   		int		 fp_idx			= NULL_IDX;
   		int		 idx;
   		int		 length;
   		long		*mod_idx;
   		int		 module_attr_idx;
   		int		 name_idx;
		long_type	 offset;
   static	int		 preinline_fp_idx	= NULL_IDX;
		long		*ptr;
   		int		 wd_len;

# if defined(_MODULE_TO_DOT_M) || defined(_MODULE_TO_DOT_o)
   		char		*mod_name_ptr;
   		char		*src_name_ptr;
# endif

# if defined(_MODULE_TO_DOT_M)
		FILE		*fp_file_ptr;
   static	int		 m_file_fp_idx		= NULL_IDX;
# endif


   TRACE (Func_Entry, "create_mod_info_file", NULL);

   module_attr_idx = SCP_ATTR_IDX(MAIN_SCP_IDX);

   /* The module is a global name, so it was entered into the global name   */
   /* table during parse_module_stmt.  The global name table entry contains */
   /* an index to the file path entry for this module, so that if we have   */
   /* a USE statement referencing the module during this compilation, we    */
   /* can find it in the file quickly by using the offset in its file path  */
   /* table entry.  We can also use it to detect duplicate modules.         */

   if (!srch_global_name_tbl(AT_OBJ_NAME_PTR(module_attr_idx),
                             AT_NAME_LEN(module_attr_idx),
                             &name_idx)) {

      if (num_prog_unit_errors == 0) {
         PRINTMSG(AT_DEF_LINE(module_attr_idx), 1250, Internal,
                  AT_DEF_COLUMN(module_attr_idx),
                  AT_OBJ_NAME_PTR(module_attr_idx));
      }
      else if (ATP_PGM_UNIT(module_attr_idx) == Module ||
               !AT_DCL_ERR(module_attr_idx)) {
         ntr_global_name_tbl(module_attr_idx, NULL_IDX, name_idx);
      }
      else {  /* Error in attr for function name. - Bypass */
         ga_idx		= NULL_IDX;
         name_idx	= NULL_IDX;
      }
   }

   if (name_idx != NULL_IDX) {
      ga_idx	= GN_ATTR_IDX(name_idx);

      if (GA_OBJ_CLASS(ga_idx) == Common_Block) {
         ga_idx	= GAC_PGM_UNIT_IDX(ga_idx);

         if (ga_idx == NULL_IDX && num_prog_unit_errors == 0) {
            PRINTMSG(AT_DEF_LINE(module_attr_idx), 1250, Internal,
                     AT_DEF_COLUMN(module_attr_idx),
                     AT_OBJ_NAME_PTR(module_attr_idx));
         }
         else {  /* Name must have been in error situation.  Make an entry. */
            ntr_global_name_tbl(module_attr_idx, NULL_IDX, name_idx);
            ga_idx	= GN_ATTR_IDX(name_idx);
         }
      }
   }

   if (num_prog_unit_errors > 0) {

      if (ga_idx != NULL_IDX && GAP_FP_IDX(ga_idx) != NULL_IDX) {

         /* We already have a module by this name and have created a mod */
         /* file for it.  Set SCP_IN_ERR so we don't write out a another */
         /* module table.  We would never find this one, because the     */
         /* search would always hit the first one.                       */

         SCP_IN_ERR(MAIN_SCP_IDX) = TRUE;
      }

      AT_DCL_ERR(module_attr_idx)    = TRUE;

      if (ATP_PGM_UNIT(module_attr_idx) == Module) {
         PRINTMSG(AT_DEF_LINE(module_attr_idx), 855, Error,
                  AT_DEF_COLUMN(module_attr_idx),
                  AT_OBJ_NAME_PTR(module_attr_idx));

#ifdef KEY /* Bug 3477 */
	 return FALSE;
#else
         if (SCP_IN_ERR(MAIN_SCP_IDX)) {
            return;
         }
#endif /* KEY Bug 3477 */
      }
      else {  /* Inline information file */
         PRINTMSG(AT_DEF_LINE(module_attr_idx), 1322, Error,
                  AT_DEF_COLUMN(module_attr_idx),
                  AT_OBJ_NAME_PTR(module_attr_idx));
      }
   }

   offset = 0;

   /* If we are in a preinline compile, everything goes out to the       */
   /* preinline_file including modules.                                  */

   if (dump_flags.preinline) {

      if (preinline_fp_idx != NULL_IDX) {
         fp_idx		= preinline_fp_idx;
         fp_file_ptr	= fopen(FP_NAME_PTR(fp_idx), "ab");
         offset		= ftell(fp_file_ptr);
         fclose(fp_file_ptr);
      }
      else {
         TBL_REALLOC_CK(file_path_tbl, 1);
         CLEAR_TBL_NTRY(file_path_tbl, file_path_tbl_idx);

         fp_idx				= file_path_tbl_idx;      
         preinline_fp_idx		= fp_idx;
         FP_NAME_LEN(fp_idx)		= strlen(preinline_file);
         FP_NAME_IDX(fp_idx)		= str_pool_idx + 1;
         FP_SRCH_THE_FILE(fp_idx)	= FALSE;
         length				= WORD_LEN(FP_NAME_LEN(fp_idx));

         /* We do not do inlining in a preinline   */
         /* compile, so this, can just be File_Fp. */

         FP_CLASS(fp_idx)		= File_Fp;

         TBL_REALLOC_CK(str_pool, length);

         ptr = (long *) (&str_pool[FP_NAME_IDX(fp_idx)].name_long);

         memset(ptr, 0, length * TARGET_BYTES_PER_WORD);

         strcpy(FP_NAME_PTR(fp_idx), preinline_file);

         /* We do not do inlining in a preinline compile so always    */
         /* put this on the module path in case it contains a module. */

         FP_NEXT_FILE_IDX(fp_idx) = module_path_idx;
      }
   }

   /* This creates a name for the module output file.  These are not     */
   /* true temp files because they need to last beyond the frontend.     */

   /* There are three naming schemes: _MODULE_TO_DOT_o, _MODULE_TO_DOT_M */
   /* and module to .mod (-em). If it is DOT_o, temp files are created   */
   /* for each module called .file.module.m.  The file names are passed  */
   /* thru the interface, where the backend puts the files where it      */
   /* wants.  If the file ends with .m, the backend must clean the file  */
   /* up.  If it ends with .mn, the frontend must remove the file.       */
   /* If DOT_M, the modules are all put in the same file.M file.  If     */
   /* .mod), the modules are each put to a file called modulename.mod.   */

   /* How the commandline option and defines work together:  If -dm is   */
   /* specified then either _MODULE_TO_DOT_M or _MODULE_TO_DOT_o         */
   /* is the default.                                                    */

   if (on_off_flags.module_to_mod) {

      if (fp_idx == NULL_IDX) {

         /* Create module.mod for the name. */
         /* Also, check to see if user specified a dir for the .mod files. */

         if (cmd_line_flags.mod_out_path) {
            strcpy(&(mod_file_name[0]), mod_out_path);
            strcat(mod_file_name, "/");
            strcat(mod_file_name, AT_OBJ_NAME_PTR(module_attr_idx));
         }
         else {
            strcpy(&(mod_file_name[0]), AT_OBJ_NAME_PTR(module_attr_idx));
         }
         strcat(mod_file_name, ".mod");
         TBL_REALLOC_CK(file_path_tbl, 1);
         CLEAR_TBL_NTRY(file_path_tbl, file_path_tbl_idx);

         fp_idx			  = file_path_tbl_idx;      
         FP_NEXT_FILE_IDX(fp_idx) = (ATP_PGM_UNIT(module_attr_idx) != Module) ? 
                                     inline_path_idx : module_path_idx;
         FP_NAME_LEN(fp_idx)	  = strlen(mod_file_name);
         FP_NAME_IDX(fp_idx)	  = str_pool_idx + 1;
         FP_SRCH_THE_FILE(fp_idx) = FALSE;
         length			  = WORD_LEN(FP_NAME_LEN(fp_idx));
         FP_CLASS(fp_idx)	  = File_Fp;

         TBL_REALLOC_CK(str_pool, length);

         for (idx = FP_NAME_IDX(fp_idx); idx <= str_pool_idx; idx++) {        
            str_pool[idx].name_long = 0;
         }

         strcpy(FP_NAME_PTR(fp_idx), mod_file_name);
      }
   }
   else { /* Default to MODULE_TO_DOT_o or MODULE_TO_DOT_M */

# if defined(_MODULE_TO_DOT_o)

      if (fp_idx == NULL_IDX) {
         mod_file_name[0]	= '.';
         mod_name_ptr	= &(mod_file_name[1]);
         src_name_ptr	= strrchr (src_file, SLASH);
         src_name_ptr	= (src_name_ptr == NULL) ? src_file : src_name_ptr+1;

         while (*mod_name_ptr++ = *src_name_ptr++);

         /* This returns a pointer to the last */
         /* occurence of dot in the file name  */

         src_name_ptr = strrchr (mod_file_name, DOT);

         if (src_name_ptr != NULL && 
             (EQUAL_STRS(src_name_ptr, ".f") ||
              EQUAL_STRS(src_name_ptr, ".f90"))){
             src_name_ptr++;
         }
         else {                  /* Just append module.m on */
             strcpy(src_name_ptr, ".");
             src_name_ptr++;
         }

         TBL_REALLOC_CK(file_path_tbl, 1);

         strncpy(src_name_ptr, 
                 AT_OBJ_NAME_PTR(module_attr_idx), 
                 AT_NAME_LEN(module_attr_idx));

         src_name_ptr	+= AT_NAME_LEN(module_attr_idx);

         /* KAY - Use for running the frontend alone. */


         strcpy(src_name_ptr, ".mn");     /* Backend will not delete this. */

         TBL_REALLOC_CK(file_path_tbl, 1);
         CLEAR_TBL_NTRY(file_path_tbl, file_path_tbl_idx);

         fp_idx			  = file_path_tbl_idx;      
         FP_NEXT_FILE_IDX(fp_idx) = (ATP_PGM_UNIT(module_attr_idx) != Module) ? 
                                     inline_path_idx : module_path_idx;
         FP_NAME_LEN(fp_idx)	  = strlen(mod_file_name);
         FP_NAME_IDX(fp_idx)	  = str_pool_idx + 1;
         FP_SRCH_THE_FILE(fp_idx) = FALSE;
         length			  = WORD_LEN(FP_NAME_LEN(fp_idx));
         FP_CLASS(fp_idx)	  = File_Fp;

         /* This file will be read up and copied by the backend.  It will be  */
         /* deleted by the backend (if suffix is .m) or the frontend if       */
         /* suffix if the file name suffix is .mn.                            */

         FP_TMP_FILE(fp_idx)	= TRUE;

         TBL_REALLOC_CK(str_pool, length);

         ptr = (long *) (&str_pool[FP_NAME_IDX(fp_idx)].name_long);

         memset(ptr, 0, length * TARGET_BYTES_PER_WORD);

         strcpy(FP_NAME_PTR(fp_idx), mod_file_name);

         if (num_prog_unit_errors == 0 && !dump_flags.no_module_output) {

            /* Send file name through interface to be put into the .o file. */
   
            FP_OUTPUT_TO_O(fp_idx)	= cmd_line_flags.binary_output;
         }
      }

# elif defined(_MODULE_TO_DOT_M)

      if (fp_idx != NULL_IDX) {

         /* intentionally blank */

      }
      else if (m_file_fp_idx != NULL_IDX) {
         fp_idx		= m_file_fp_idx;
         fp_file_ptr	= fopen(FP_NAME_PTR(fp_idx), "ab");
         offset		= ftell(fp_file_ptr);
         fclose(fp_file_ptr);
      }
      else {
         mod_name_ptr	= &(mod_file_name[0]);
         src_name_ptr	= strrchr (src_file, SLASH);
         src_name_ptr	= (src_name_ptr == NULL) ? src_file : src_name_ptr+1;

         while (*mod_name_ptr++ = *src_name_ptr++);

         /* This returns a pointer to the last */
         /* occurence of dot in the file name  */

         src_name_ptr = strrchr (mod_file_name, DOT);

         if (src_name_ptr != NULL && 
             (EQUAL_STRS(src_name_ptr, ".f") ||
              EQUAL_STRS(src_name_ptr, ".f90"))){
            src_name_ptr++;
         }
         else {                  /* Just append module.m on */
            strcpy(src_name_ptr, ".");
            src_name_ptr++;
         }

         strcpy(src_name_ptr, "M");

         TBL_REALLOC_CK(file_path_tbl, 1);
         CLEAR_TBL_NTRY(file_path_tbl, file_path_tbl_idx);

         fp_idx			  = file_path_tbl_idx;      
         FP_NEXT_FILE_IDX(fp_idx) = (ATP_PGM_UNIT(module_attr_idx) != Module) ? 
                                   inline_path_idx : module_path_idx;
         FP_NAME_LEN(fp_idx)	  = strlen(mod_file_name);
         FP_NAME_IDX(fp_idx)	  = str_pool_idx + 1;
         FP_SRCH_THE_FILE(fp_idx) = FALSE;
         length			  = WORD_LEN(FP_NAME_LEN(fp_idx));
         FP_CLASS(fp_idx)	  = File_Fp;

         TBL_REALLOC_CK(str_pool, length);

         for (idx = FP_NAME_IDX(fp_idx); idx <= str_pool_idx; idx++) {        
            str_pool[idx].name_long = 0;
         }

         strcpy(FP_NAME_PTR(fp_idx), mod_file_name);

         m_file_fp_idx	= fp_idx;
         fp_file_ptr	= fopen(mod_file_name, "wb");
         fclose(fp_file_ptr);
      }
# endif
   }

   /* Keep track of the file index for the first module written out.  This */
   /* will get updated with a directory listing all modules in this file.  */

   if (ATP_PGM_UNIT(module_attr_idx) == Module) {

      if (module_path_idx == NULL_IDX) {
         module_path_idx	= fp_idx;
      }
   }
   else if (inline_path_idx == NULL_IDX && !dump_flags.preinline) {
      inline_path_idx		= fp_idx;
   }

   /* Create an entry for the module being written out.  fp_idx  */
   /* is the file path table index for the file entry.           */

   TBL_REALLOC_CK(file_path_tbl, 1);
   CLEAR_TBL_NTRY(file_path_tbl, file_path_tbl_idx);

   /* Put this module at the top of the list. */

   FP_MODULE_IDX(file_path_tbl_idx)	= FP_MODULE_IDX(fp_idx);
   FP_MODULE_IDX(fp_idx)		= file_path_tbl_idx;

   FP_NAME_LEN(file_path_tbl_idx)	= AT_NAME_LEN(module_attr_idx);
   FP_NAME_IDX(file_path_tbl_idx)	= str_pool_idx + 1;
   FP_OFFSET(file_path_tbl_idx)		= offset;
   FP_FILE_IDX(file_path_tbl_idx)	= fp_idx;     /* Link to file entry */
   FP_CLASS(file_path_tbl_idx)		= Current_Compile_Fp;

   mod_idx				= &(mit_header.wd[0]);

   for (idx=0; idx < sizeof(mit_header_type) / TARGET_BYTES_PER_WORD; idx++) {
      *mod_idx = 0;
       mod_idx++;
   }
   if (ga_idx != NULL_IDX) {
      GAP_FP_IDX(ga_idx)	= file_path_tbl_idx;
   }
   name_idx		= AT_NAME_IDX(module_attr_idx);
   wd_len		= WORD_LEN(AT_NAME_LEN(module_attr_idx));
   mod_idx		= MD_NAME_LONG;
   MD_NAME_LEN		= AT_NAME_LEN(module_attr_idx);

   TBL_REALLOC_CK(str_pool, wd_len);

   for (idx = FP_NAME_IDX(file_path_tbl_idx); idx <= str_pool_idx; idx++) {
      *mod_idx			= name_pool[name_idx].name_long;
      str_pool[idx].name_long	= name_pool[name_idx].name_long;
      name_idx++;
      mod_idx++;
   }

   SCP_FILE_PATH_IDX(curr_scp_idx)	= fp_idx;

   TRACE (Func_Exit, "create_mod_info_file", NULL);

#ifdef KEY /* Bug 3477 */
   return TRUE;
#else
   return;
#endif /* KEY Bug 3477 */

}  /* create_mod_info_file */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Create the module link table, which is used to output the module      *|
|*	information table.                                                    *|
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
void  create_mod_info_tbl(void)

{
   int		attr_idx;
   int		name_idx;


   TRACE (Func_Entry, "create_mod_info_tbl", NULL);

   if (dump_flags.preinline && num_prog_unit_errors > 0) {

      /* Do not write out any tables.  Just the mod header. */

      return;
   }

   allocate_mod_link_tbl(0);  /* Determine size from longest table. */

   /* global flag used to tell set_mod_link_tbl_for_attr */
   /* that it should check all attrs for duplicates.     */

   search_for_duplicate_attrs	= FALSE;  /* Do not search */

   for (name_idx = SCP_LN_FW_IDX(MAIN_SCP_IDX) + 1;
        name_idx < SCP_LN_LW_IDX(MAIN_SCP_IDX); 
        name_idx++) {

      attr_idx = LN_ATTR_IDX(name_idx);

      if (attr_idx == SCP_ATTR_IDX(MAIN_SCP_IDX)) {
         KEEP_ATTR(attr_idx);
         ML_LN_KEEP_ME(name_idx)	=  TRUE;
         ML_LN_IDX(name_idx)		=  name_idx;
      }
      else if (AT_PRIVATE(attr_idx) || AT_OBJ_CLASS(attr_idx) == Label) {

         /* If object is PRIVATE, the name must not go into the name table.  */
         /* Also, Labels do not go out for the module itself.                */

      }
      else if (IS_STMT_ENTITY(attr_idx)) {

         /* This item is only used as the loop control variable in    */
         /* an implied-do.  It is only in the scope of the implied-do */
         /* and should not be written out to the module file.         */
      }
      else if (AT_OBJ_CLASS(attr_idx) == Data_Obj &&
               ATD_SYMBOLIC_CONSTANT(attr_idx) &&
               ATD_CLASS(attr_idx) == Constant) {

         /* N$PES was specified as a constant.  Do not output the actual     */
         /* constant to the module.  It should have been replaced all over.  */
      }
      else if (AT_USE_ASSOCIATED(attr_idx) &&
               AT_OBJ_CLASS(attr_idx) == Pgm_Unit &&
               ATP_PGM_UNIT(attr_idx) == Module) {

         /* This module has been use associated into this scope.  We do not */
         /* want the name in the local name table, although we do want to   */
         /* have the attribute entry for the module go out.                 */
      }
      else if (!ML_AT_KEEP_ME(attr_idx)) {

         /* if ML_AT_KEEP_ME is set, this attr entry has been processed. */
         /* It got processed because it was indexed to by another attr.  */
         /* (For example:  A derived type.)  Check to see if we need to  */
         /* keep the local name or not in the next else clause.          */

         if (AT_OBJ_CLASS(attr_idx) == Pgm_Unit &&
             ATP_PROC(attr_idx) == Module_Proc &&
             !AT_USE_ASSOCIATED(attr_idx)) {

            /* This is a module procedure declared during this       */
            /* compilation.  We need to resolve out any duplicate    */
            /* attrs that were used to describe the interface for    */
            /* the module procedure.  An example of a duplicate is   */
            /* if the same module is USEd in the module and the      */
            /* module procedure.  If a type from the module is used  */
            /* to describe the module procedure interface, then      */
            /* there will be duplicate attrs for the derived type.   */
            /* This mechanism resolves them to the same attr.        */

            search_for_duplicate_attrs	= TRUE;
         }

         KEEP_ATTR(attr_idx);
         ML_LN_KEEP_ME(name_idx)	= TRUE;
         ML_LN_IDX(name_idx)		= name_idx;
         search_for_duplicate_attrs	= FALSE;
      }
      else {
         ML_LN_KEEP_ME(name_idx)	= TRUE;
         ML_LN_IDX(name_idx)		= name_idx;

         /* This name gets included because of a link with another attr. */
         /* Can be a derived type, function result, CRI pointee, ect...  */
         /* Its attr_idx has already been marked as being needed.        */
      }
   }

   TRACE (Func_Exit, "create_mod_info_tbl", NULL);

   return;

}   /* create_mod_info_tbl */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Clear these fields in the attr entry, before it is written out to the *|
|*	module file.                                                          *|
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
static void  set_attr_flds_for_output()

{
   int		attr_idx;


   TRACE (Func_Entry, "set_attr_flds_for_output", NULL);

   for (attr_idx = 1; attr_idx <= attr_tbl_idx; attr_idx++) {

      if (AT_ORIG_NAME_IDX(attr_idx) == NULL_IDX) {
         AT_ORIG_NAME_IDX(attr_idx) = AT_NAME_IDX(attr_idx);
         AT_ORIG_NAME_LEN(attr_idx) = AT_NAME_LEN(attr_idx);
      }

      switch (AT_OBJ_CLASS(attr_idx)) {
      case Data_Obj:

         if (ATD_CLASS(attr_idx) == Compiler_Tmp) {
            ATD_TMP_GEN_ZERO(attr_idx)		= FALSE;
         }

         if (ATD_CLASS(attr_idx) == Dummy_Argument && ATD_SF_DARG(attr_idx)) {
            ATD_SF_ARG_IDX(attr_idx)	= NULL_IDX;
            ATD_SF_LINK(attr_idx)	= NULL_IDX;
         }
         break;

      case Pgm_Unit:
         ATP_SCP_IDX(attr_idx)			= NULL_IDX;

         if (ATP_PGM_UNIT(attr_idx) == Module) {
            ATP_MODULE_STR_IDX(attr_idx)	= NULL_IDX;
         }
         else if (ATP_PROC(attr_idx) == Intrin_Proc) {
            ATP_INTERFACE_IDX(attr_idx)		= NULL_IDX;
         }
         break;

      case Derived_Type:
         AT_DEFINED(attr_idx)			= FALSE;
         ATT_CIF_DT_ID(attr_idx) 		= 0;
         ATT_SCP_IDX(attr_idx)			= NULL_IDX;
         break;

      case Label:  /* Do not clear AT_DEFINED here. */
         break;

      case Interface:
         ATI_HAS_NON_MOD_PROC(attr_idx)		= FALSE;
         break;

      case Stmt_Func:
      case Namelist_Grp:
         break;

      }  /* End switch */
   }  /* End for */


   TRACE (Func_Exit, "set_attr_flds_for_output", NULL);

   return;

}   /* set_attr_flds_for_output */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	All these attr entries must be included in the entries that will be   *|
|*	compressed.  Mark this attr and everything it links to, as being      *|
|*	saved during the compression.                                         *|
|*									      *|
|* Input parameters:							      *|
|*	attr_idx   -> Index of attribute to process.                          *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/
static	void  set_mod_link_tbl_for_attr(int	attr_idx)

{
   int		bd_idx;
   int		il_idx;
   int		save_duplicate_attr_flag;
   int		sb_idx;
   int		sh_idx;
   int		sn_idx;


   TRACE (Func_Entry, "set_mod_link_tbl_for_attr", NULL);

   if (ML_AT_KEEP_ME(attr_idx)) {

      /* All the links for this attr have been set, plus */
      /* resolve_attr has been called, if need be.       */

      return;
   }

   if ((ML_AT_SEARCH_ME(attr_idx) ||
       search_for_duplicate_attrs) && !ML_AT_SEARCHED(attr_idx)) {

      if (AT_MODULE_IDX(attr_idx) != NULL_IDX) {
         AT_REFERENCED(AT_MODULE_IDX(attr_idx)) = Referenced;
      }

      if (resolve_attr(attr_idx)) {

         /* If resolve_attr returns TRUE, this object is in this scope  */
         /* already.  Do not set any links for this attr.  We will use  */
         /* the attr that is already in this scope.  Mark the attr      */
         /* resolved to, so that it gets kept.                          */
       
         KEEP_ATTR(ML_AT_IDX(attr_idx));
         return;
      }
   }

   if (ML_AT_IDX(attr_idx) != NULL_IDX && ML_AT_IDX(attr_idx) != attr_idx) {

      /* This attr is being replaced by the attr in ML_AT_IDX.  */
      /* Keep the attr in ML_AT_IDX.                            */

      KEEP_ATTR(ML_AT_IDX(attr_idx));
      return;
   }

   ML_AT_KEEP_ME(attr_idx)		= TRUE;
   ML_AT_IDX(attr_idx)			= attr_idx;
   ML_NP_KEEP_ME(AT_NAME_IDX(attr_idx))	= TRUE;
   ML_NP_IDX(AT_NAME_IDX(attr_idx))	= AT_NAME_IDX(attr_idx);
   ML_NP_LEN(AT_NAME_IDX(attr_idx))	= AT_NAME_LEN(attr_idx);

   if (AT_ORIG_NAME_IDX(attr_idx) != NULL_IDX) {
      ML_NP_KEEP_ME(AT_ORIG_NAME_IDX(attr_idx))	= TRUE;
      ML_NP_IDX(AT_ORIG_NAME_IDX(attr_idx))	= AT_ORIG_NAME_IDX(attr_idx);
      ML_NP_LEN(AT_ORIG_NAME_IDX(attr_idx))	= AT_ORIG_NAME_LEN(attr_idx);
   }

   if (AT_ATTR_LINK(attr_idx) != NULL_IDX && !AT_IGNORE_ATTR_LINK(attr_idx)) {
      KEEP_ATTR(AT_ATTR_LINK(attr_idx));
   }

   if (AT_MODULE_IDX(attr_idx) != NULL_IDX) {
      KEEP_ATTR(AT_MODULE_IDX(attr_idx));
   }

   switch (AT_OBJ_CLASS(attr_idx)) {
   case Data_Obj:

# if defined(_F_MINUS_MINUS)
      bd_idx	= ATD_PE_ARRAY_IDX(attr_idx);

      if (bd_idx != NULL_IDX && !ML_BD_KEEP_ME(bd_idx)) {
         set_mod_link_tbl_for_bd(bd_idx);
      }
# endif

      bd_idx	= ATD_RESHAPE_ARRAY_IDX(attr_idx);

      if (bd_idx != NULL_IDX && !ML_BD_KEEP_ME(bd_idx)) {
         set_mod_link_tbl_for_bd(bd_idx);
      }

      bd_idx	= ATD_ARRAY_IDX(attr_idx);
      sb_idx	= ATD_STOR_BLK_IDX(attr_idx);

      if (bd_idx != NULL_IDX && !ML_BD_KEEP_ME(bd_idx)) {
         set_mod_link_tbl_for_bd(bd_idx);
      }

      bd_idx	= ATD_DISTRIBUTION_IDX(attr_idx);

      if (bd_idx != NULL_IDX && !ML_BD_KEEP_ME(bd_idx)) {
         set_mod_link_tbl_for_bd(bd_idx);
      }

      if (sb_idx != NULL_IDX) {
         ML_SB_KEEP_ME(sb_idx)			= TRUE;
         ML_SB_IDX(sb_idx)			= sb_idx;
         ML_NP_KEEP_ME(SB_NAME_IDX(sb_idx))	= TRUE;
         ML_NP_IDX(SB_NAME_IDX(sb_idx))		= SB_NAME_IDX(sb_idx);
         ML_NP_LEN(SB_NAME_IDX(sb_idx))		= SB_NAME_LEN(sb_idx);
#ifdef KEY /* Bug 14150 */
	 int sb_ext_name_idx = SB_EXT_NAME_IDX(sb_idx);
	 if (sb_ext_name_idx) {
	   ML_NP_KEEP_ME(sb_ext_name_idx) = TRUE;
	   ML_NP_IDX(sb_ext_name_idx) = sb_ext_name_idx;
	   ML_NP_LEN(sb_ext_name_idx) = SB_EXT_NAME_LEN(sb_idx);
	 }
#endif /* KEY Bug 14150 */

         if (SB_FIRST_ATTR_IDX(sb_idx) != NULL_IDX) {
            KEEP_ATTR(SB_FIRST_ATTR_IDX(sb_idx));
         }

         switch (SB_LEN_FLD(sb_idx)) {
         case AT_Tbl_Idx:
            KEEP_ATTR(SB_LEN_IDX(sb_idx));
            break;

         case CN_Tbl_Idx:
            KEEP_CN(SB_LEN_IDX(sb_idx));
            break;

         case IR_Tbl_Idx:
            KEEP_IR(SB_LEN_IDX(sb_idx));
            break;

         case IL_Tbl_Idx:
            set_mod_link_tbl_for_il(SB_LEN_IDX(sb_idx));
            break;
         }

         if (SB_MODULE_IDX(sb_idx) != NULL_IDX) {
            KEEP_ATTR(SB_MODULE_IDX(sb_idx));
         }
      }

      switch (ATD_CLASS(attr_idx)) {
      case CRI__Pointee:
         set_mod_link_tbl_for_typ(ATD_TYPE_IDX(attr_idx));
         KEEP_ATTR(ATD_PTR_IDX(attr_idx));
         break;

      case Dummy_Argument:

         if (!ATD_INTRIN_DARG(attr_idx)) {
            set_mod_link_tbl_for_typ(ATD_TYPE_IDX(attr_idx));
         }
         break;

      case Constant:
         set_mod_link_tbl_for_typ(ATD_TYPE_IDX(attr_idx));

         if (ATD_FLD(attr_idx) == CN_Tbl_Idx) {
            KEEP_CN(ATD_CONST_IDX(attr_idx));
         }
         else {

            /* If we are resolving attrs, we do not search for this tmp   */
            /* attr because its name is the same as the constant.  These  */
            /* two are a pair so we do not need to do a seperate search.  */

            ML_AT_SEARCHED(ATD_CONST_IDX(attr_idx))	= TRUE;
            KEEP_ATTR(ATD_CONST_IDX(attr_idx));
         }
         break;

      case Compiler_Tmp:

         if (ATD_NEXT_MEMBER_IDX(attr_idx) != NULL_IDX) {
            KEEP_ATTR(ATD_NEXT_MEMBER_IDX(attr_idx));
         }

         if (ATD_DEFINING_ATTR_IDX(attr_idx) != NULL_IDX) {
            KEEP_ATTR(ATD_DEFINING_ATTR_IDX(attr_idx));
         }

         if (ATD_AUTOMATIC(attr_idx)) {
            KEEP_ATTR(ATD_AUTO_BASE_IDX(attr_idx));
         }
         else if (ATD_OFFSET_ASSIGNED(attr_idx)) {

            switch (ATD_OFFSET_FLD(attr_idx)) {
            case AT_Tbl_Idx:
               KEEP_ATTR(ATD_OFFSET_IDX(attr_idx));
               break;
            case CN_Tbl_Idx:
               KEEP_CN(ATD_OFFSET_IDX(attr_idx));
               break;
            case IR_Tbl_Idx:
               KEEP_IR(ATD_OFFSET_IDX(attr_idx));
               break;
            case IL_Tbl_Idx:
               set_mod_link_tbl_for_il(ATD_OFFSET_IDX(attr_idx));
               break;
            }
         }

         set_mod_link_tbl_for_typ(ATD_TYPE_IDX(attr_idx));

         switch (ATD_FLD(attr_idx)) {
         case CN_Tbl_Idx:
            KEEP_CN(ATD_TMP_IDX(attr_idx));
            break;

         case AT_Tbl_Idx:
            KEEP_ATTR(ATD_TMP_IDX(attr_idx));
            break;

         case IL_Tbl_Idx:
            set_mod_link_tbl_for_il(ATD_TMP_IDX(attr_idx));
            break;

         case IR_Tbl_Idx:
            KEEP_IR(ATD_TMP_IDX(attr_idx));
            break;
         }
         break;

      case Function_Result:

         if (ATD_OFFSET_ASSIGNED(attr_idx)) {

            switch (ATD_OFFSET_FLD(attr_idx)) {
            case AT_Tbl_Idx:
               KEEP_ATTR(ATD_OFFSET_IDX(attr_idx));
               break;
            case CN_Tbl_Idx:
               KEEP_CN(ATD_OFFSET_IDX(attr_idx));
               break;
            case IR_Tbl_Idx:
               KEEP_IR(ATD_OFFSET_IDX(attr_idx));
               break;
            case IL_Tbl_Idx:
               set_mod_link_tbl_for_il(ATD_OFFSET_IDX(attr_idx));
               break;
            }
         }
         set_mod_link_tbl_for_typ(ATD_TYPE_IDX(attr_idx));
         break;
        

      case Struct_Component:

         switch (ATD_OFFSET_FLD(attr_idx)) {
         case AT_Tbl_Idx:
            KEEP_ATTR(ATD_CPNT_OFFSET_IDX(attr_idx));
            break;
         case CN_Tbl_Idx:
            KEEP_CN(ATD_CPNT_OFFSET_IDX(attr_idx));
            break;
         case IR_Tbl_Idx:
            KEEP_IR(ATD_CPNT_OFFSET_IDX(attr_idx));
            break;
         case IL_Tbl_Idx:
            set_mod_link_tbl_for_il(ATD_CPNT_OFFSET_IDX(attr_idx));
            break;
         }

         if (ATD_CPNT_INIT_IDX(attr_idx) != NULL_IDX) {

            switch (ATD_FLD(attr_idx)) {
            case AT_Tbl_Idx:
               KEEP_ATTR(ATD_CPNT_INIT_IDX(attr_idx));
               break;
            case CN_Tbl_Idx:
               KEEP_CN(ATD_CPNT_INIT_IDX(attr_idx));
               break;
            case IR_Tbl_Idx:
               KEEP_IR(ATD_CPNT_INIT_IDX(attr_idx));
               break;
            case IL_Tbl_Idx:
               set_mod_link_tbl_for_il(ATD_CPNT_INIT_IDX(attr_idx));
               break;
            }
         }

         set_mod_link_tbl_for_typ(ATD_TYPE_IDX(attr_idx));
         KEEP_ATTR(ATD_DERIVED_TYPE_IDX(attr_idx));
         break;

      case Variable:

         if (ATD_DATA_INIT(attr_idx)) {

            if (ATD_FLD(attr_idx) == NO_Tbl_Idx) {
               /* Intentionally blank */
            }
            else if (ATD_FLD(attr_idx) == AT_Tbl_Idx) {

               /* If we are resolving attrs, we do not search for this tmp   */
               /* attr because its name is the same as the variable.  These  */
               /* two are a pair so we do not need to do a seperate search.  */

               ML_AT_SEARCHED(ATD_VARIABLE_TMP_IDX(attr_idx))	= TRUE;
               KEEP_ATTR(ATD_VARIABLE_TMP_IDX(attr_idx));
            }
            else if (ATD_FLD(attr_idx) == IL_Tbl_Idx) {

               /* See previous note. */

               il_idx = ATD_VARIABLE_TMP_IDX(attr_idx);

               while (il_idx != NULL_IDX) {
   
                  if (IL_FLD(il_idx) == AT_Tbl_Idx) {
                     ML_AT_SEARCHED(IL_IDX(il_idx)) = TRUE;
                  }
                  il_idx = IL_NEXT_LIST_IDX(il_idx);
               }

               set_mod_link_tbl_for_il(ATD_VARIABLE_TMP_IDX(attr_idx));
            }
         }
         else if (ATD_FLD(attr_idx) == AT_Tbl_Idx) {
            KEEP_ATTR(ATD_VARIABLE_TMP_IDX(attr_idx));
         }

         if (ATD_NEXT_MEMBER_IDX(attr_idx) != NULL_IDX) {
            KEEP_ATTR(ATD_NEXT_MEMBER_IDX(attr_idx));
         }

         if (ATD_ASSIGN_TMP_IDX(attr_idx) != NULL_IDX) {
            KEEP_ATTR(ATD_ASSIGN_TMP_IDX(attr_idx));
         }

         if (ATD_AUTOMATIC(attr_idx)) {
            KEEP_ATTR(ATD_AUTO_BASE_IDX(attr_idx));
         }
         else if (ATD_OFFSET_ASSIGNED(attr_idx)) {

            switch (ATD_OFFSET_FLD(attr_idx)) {
            case AT_Tbl_Idx:
               KEEP_ATTR(ATD_OFFSET_IDX(attr_idx));
               break;
            case CN_Tbl_Idx:
               KEEP_CN(ATD_OFFSET_IDX(attr_idx));
               break;
            case IR_Tbl_Idx:
               KEEP_IR(ATD_OFFSET_IDX(attr_idx));
               break;
            case IL_Tbl_Idx:
               set_mod_link_tbl_for_il(ATD_OFFSET_IDX(attr_idx));
               break;
            }
         }

         /* Intentional fall through */

      default:

         set_mod_link_tbl_for_typ(ATD_TYPE_IDX(attr_idx));
         break;
      }
      break;


   case Pgm_Unit:

      ML_NP_KEEP_ME(ATP_EXT_NAME_IDX(attr_idx))	= TRUE;
      ML_NP_IDX(ATP_EXT_NAME_IDX(attr_idx))	= ATP_EXT_NAME_IDX(attr_idx);
      ML_NP_LEN(ATP_EXT_NAME_IDX(attr_idx))	= ATP_EXT_NAME_LEN(attr_idx);

      if (ATP_PGM_UNIT(attr_idx) == Module) {

         if (ATP_MOD_PATH_LEN(attr_idx) > 0) {
            ML_NP_KEEP_ME(ATP_MOD_PATH_IDX(attr_idx)) = TRUE;
            ML_NP_IDX(ATP_MOD_PATH_IDX(attr_idx)) = ATP_MOD_PATH_IDX(attr_idx);
            ML_NP_LEN(ATP_MOD_PATH_IDX(attr_idx)) = ATP_MOD_PATH_LEN(attr_idx);
         }
      }
      else {

         if (ATP_RSLT_IDX(attr_idx) != NULL_IDX) {
            KEEP_ATTR(ATP_RSLT_IDX(attr_idx));
         }

         if (ATP_NUM_DARGS(attr_idx) > 0) {

            for (sn_idx = ATP_FIRST_IDX(attr_idx); 
                 sn_idx < (ATP_FIRST_IDX(attr_idx) + ATP_NUM_DARGS(attr_idx));
                 sn_idx++) {

               ML_SN_KEEP_ME(sn_idx)	= TRUE;
               ML_SN_IDX(sn_idx)	= sn_idx;

               KEEP_ATTR(SN_ATTR_IDX(sn_idx));
            }
         }

         /* This flag works for all 3 uses of this routine.             */

         /* 1) It is called when a use statement is processed.          */
         /*    ATP_MAY_INLINE will be set for all procedures            */
         /*    coming from the module that carry IR/SH with them.       */
         /* 2) During interface processing, if there are any use        */
         /*    associated procedures, they will have the mod            */
         /*    inlinable flag set correctly.  If they are not, they     */
         /*    will not have the mod inlinable flag set.                */
         /* 3) During processing, to send module info out, only those   */
         /*    procedures that have the mod inlinable flag set, go out. */

         if (ATP_MAY_INLINE(attr_idx)) {

            /* This is the body of the module/internal procedure.  We   */
            /* do not want to search for duplicate attrs here.  If we   */
            /* do, we get things confused because of host association.  */

            save_duplicate_attr_flag	= search_for_duplicate_attrs;
            search_for_duplicate_attrs	= FALSE;
            sh_idx			= ATP_FIRST_SH_IDX(attr_idx);

            while (sh_idx != NULL_IDX) {
               ML_SH_KEEP_ME(sh_idx)	= TRUE;
               ML_SH_IDX(sh_idx)	= sh_idx;

               if (SH_IR_IDX(sh_idx) != NULL_IDX) {
                  KEEP_IR(SH_IR_IDX(sh_idx));
               }
               sh_idx			= SH_NEXT_IDX(sh_idx);
            }

            if (ATP_PROC(attr_idx) != Dummy_Proc &&
                ATP_PROC(attr_idx) != Intrin_Proc) {

               if (ATP_PARENT_IDX(attr_idx) != NULL_IDX) {
                  KEEP_ATTR(ATP_PARENT_IDX(attr_idx));
               }
            }
            search_for_duplicate_attrs	= save_duplicate_attr_flag;
         }
         else if (ATP_PROC(attr_idx) != Intrin_Proc) {
            ATP_FIRST_SH_IDX(attr_idx)	= NULL_IDX;
         }
      }
      break;

   case Label:

      if (ATL_CLASS(attr_idx) == Lbl_Format) {
         KEEP_ATTR(ATL_PP_FORMAT_TMP(attr_idx));
         KEEP_ATTR(ATL_FORMAT_TMP(attr_idx));
      }
      else if (ATL_DIRECTIVE_LIST(attr_idx) != NULL_IDX) {
         set_mod_link_tbl_for_il(ATL_DIRECTIVE_LIST(attr_idx));
      }

      if (ATL_NEXT_ASG_LBL_IDX(attr_idx) != NULL_IDX) {
         KEEP_ATTR(ATL_NEXT_ASG_LBL_IDX(attr_idx));
      }
      break;

   case Interface:
      set_mod_link_tbl_for_typ(ATD_TYPE_IDX(attr_idx));

      /* During resolve_attrs we do not search for interface names.  */
      /* We cannot gurantee they are the same because of merging.    */

      if (ATI_PROC_IDX(attr_idx) != NULL_IDX) {

         /* If we are resolving attrs, we do not search for this proc */
         /* attr because it has the same name as the interface name.  */

         ML_AT_SEARCHED(ATI_PROC_IDX(attr_idx))	= TRUE;
      }

      sn_idx = ATI_FIRST_SPECIFIC_IDX(attr_idx);

      while (sn_idx != NULL_IDX) {
         ML_SN_KEEP_ME(sn_idx)	= TRUE;
         ML_SN_IDX(sn_idx)	= sn_idx;
         KEEP_ATTR(SN_ATTR_IDX(sn_idx));
         sn_idx			= SN_SIBLING_LINK(sn_idx);
      }
      break;

   case Derived_Type:

      switch (ATT_STRUCT_BIT_LEN_FLD(attr_idx)) {
      case AT_Tbl_Idx:
         KEEP_ATTR(ATT_STRUCT_BIT_LEN_IDX(attr_idx));
         break;

      case CN_Tbl_Idx:
         KEEP_CN(ATT_STRUCT_BIT_LEN_IDX(attr_idx));
         break;

      case IR_Tbl_Idx:
         KEEP_IR(ATT_STRUCT_BIT_LEN_IDX(attr_idx));
         break;

      case IL_Tbl_Idx:
         set_mod_link_tbl_for_il(ATT_STRUCT_BIT_LEN_IDX(attr_idx));
         break;
      }

      sn_idx			= ATT_FIRST_CPNT_IDX(attr_idx);

      while (sn_idx != NULL_IDX) {
         ML_SN_KEEP_ME(sn_idx)	= TRUE;
         ML_SN_IDX(sn_idx)	= sn_idx;

         /* We do not resolve components, because the name are not unique. */

         ML_AT_SEARCHED(SN_ATTR_IDX(sn_idx)) = TRUE;
         KEEP_ATTR(SN_ATTR_IDX(sn_idx));
         sn_idx	= SN_SIBLING_LINK(sn_idx);
      }
      break;

   case Namelist_Grp:
      sn_idx = ATN_FIRST_NAMELIST_IDX(attr_idx);

      while (sn_idx != NULL_IDX) {
         ML_SN_KEEP_ME(sn_idx)	= TRUE;
         ML_SN_IDX(sn_idx)	= sn_idx;
         KEEP_ATTR (SN_ATTR_IDX(sn_idx));
         sn_idx			= SN_SIBLING_LINK(sn_idx);
      }

      if (ATN_NAMELIST_DESC(attr_idx) != NULL_IDX) {
         KEEP_ATTR(ATN_NAMELIST_DESC(attr_idx));
      }
      break;


   case Stmt_Func:
      set_mod_link_tbl_for_typ(ATD_TYPE_IDX(attr_idx));

      if (ATP_NUM_DARGS(attr_idx) > 0) {

         for (sn_idx = ATP_FIRST_IDX(attr_idx); 
              sn_idx < (ATP_FIRST_IDX(attr_idx) +ATP_NUM_DARGS(attr_idx));
              sn_idx++) {

            ML_SN_KEEP_ME(sn_idx)	= TRUE;
            ML_SN_IDX(sn_idx)		= sn_idx;
            KEEP_ATTR(SN_ATTR_IDX(sn_idx));
         }
      }

      switch (ATS_SF_FLD(attr_idx)) {
      case CN_Tbl_Idx:
         KEEP_CN(ATS_SF_IDX(attr_idx));
         break;

      case AT_Tbl_Idx:
         KEEP_ATTR(ATS_SF_IDX(attr_idx));
         break;

      case IL_Tbl_Idx:
         set_mod_link_tbl_for_il(ATS_SF_IDX(attr_idx));
         break;

      case IR_Tbl_Idx:
         KEEP_IR(ATS_SF_IDX(attr_idx));
         break;
      }
      break;

   }  /* End switch */

   TRACE (Func_Exit, "set_mod_link_tbl_for_attr ", NULL);

   return;

}   /* set_mod_link_tbl_for_attr  */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Set fields in the module link table for BD.                           *|
|*									      *|
|* Input parameters:							      *|
|*	bd_idx      => Index to set link fields for.                          *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/
static	void  set_mod_link_tbl_for_bd(int	bd_idx)

{
   int		dim;


   TRACE (Func_Entry, "set_mod_link_tbl_for_bd ", NULL);

   ML_BD_KEEP_ME(bd_idx)	= TRUE;
   ML_BD_IDX(bd_idx)		= bd_idx;

   if (BD_DIST_NTRY(bd_idx)) {

      for (dim = 1; dim <= BD_RANK(bd_idx); dim++) {

         if (BD_CYCLIC_FLD(bd_idx, dim) == CN_Tbl_Idx) {
            KEEP_CN(BD_CYCLIC_IDX(bd_idx, dim));
         }
         else if (BD_CYCLIC_FLD(bd_idx, dim) == AT_Tbl_Idx) {
            KEEP_ATTR(BD_CYCLIC_IDX(bd_idx, dim));
         }

         if (BD_ONTO_FLD(bd_idx, dim) == CN_Tbl_Idx) {
            KEEP_CN(BD_ONTO_IDX(bd_idx, dim));
         }
         else if (BD_ONTO_FLD(bd_idx, dim) == AT_Tbl_Idx) {
            KEEP_ATTR(BD_ONTO_IDX(bd_idx, dim));
         }
      }
   }
   else if (BD_ARRAY_CLASS(bd_idx) != Deferred_Shape) {

      if (BD_LEN_FLD(bd_idx) == CN_Tbl_Idx) {
         KEEP_CN(BD_LEN_IDX(bd_idx));
      }
      else if (BD_LEN_FLD(bd_idx) == AT_Tbl_Idx) {
         KEEP_ATTR(BD_LEN_IDX(bd_idx));
      }

      for (dim = 1; dim <= BD_RANK(bd_idx); dim++) {

         if (BD_LB_FLD(bd_idx, dim) == CN_Tbl_Idx) {
            KEEP_CN(BD_LB_IDX(bd_idx, dim));
         }
         else if (BD_LB_FLD(bd_idx, dim) == AT_Tbl_Idx) {
            KEEP_ATTR(BD_LB_IDX(bd_idx, dim));
         }

         if (BD_UB_FLD(bd_idx, dim) == CN_Tbl_Idx) {
            KEEP_CN(BD_UB_IDX(bd_idx, dim));
         }
         else if (BD_UB_FLD(bd_idx, dim) == AT_Tbl_Idx) {
            KEEP_ATTR(BD_UB_IDX(bd_idx, dim));
         }

         if (BD_XT_FLD(bd_idx, dim) == CN_Tbl_Idx) {
            KEEP_CN(BD_XT_IDX(bd_idx, dim));
         }
         else if (BD_XT_FLD(bd_idx, dim) == AT_Tbl_Idx) {
            KEEP_ATTR(BD_XT_IDX(bd_idx, dim));
         }

         if (BD_SM_FLD(bd_idx, dim) == CN_Tbl_Idx) {
            KEEP_CN(BD_SM_IDX(bd_idx, dim));
         }
         else if (BD_SM_FLD(bd_idx, dim) == AT_Tbl_Idx) {
            KEEP_ATTR(BD_SM_IDX(bd_idx, dim));
         }
      }
   }

   TRACE (Func_Exit, "set_mod_link_tbl_for_bd ", NULL);

   return;

}   /* set_mod_link_tbl_for_bd  */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Set fields in the module link table for IR.                           *|
|*									      *|
|* Input parameters:							      *|
|*	ir_idx      => Index to set link fields for.                          *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/
static	void  set_mod_link_tbl_for_ir(int	ir_idx)

{

   TRACE (Func_Entry, "set_mod_link_tbl_for_ir", NULL);

   if (ML_IR_KEEP_ME(ir_idx)) {
      return;
   }

   ML_IR_KEEP_ME(ir_idx)	= TRUE;
   ML_IR_IDX(ir_idx)		= ir_idx;

   set_mod_link_tbl_for_typ(IR_TYPE_IDX(ir_idx));

   switch (IR_FLD_L(ir_idx)) {
   case CN_Tbl_Idx:
      KEEP_CN(IR_IDX_L(ir_idx));
      break;

   case AT_Tbl_Idx:
      KEEP_ATTR(IR_IDX_L(ir_idx));
      break;

   case IR_Tbl_Idx:
      KEEP_IR(IR_IDX_L(ir_idx));
      break;

   case IL_Tbl_Idx:
      set_mod_link_tbl_for_il(IR_IDX_L(ir_idx));
      break;

   case NO_Tbl_Idx:
   case SH_Tbl_Idx:
      break;
   }

   switch (IR_FLD_R(ir_idx)) {
   case CN_Tbl_Idx:
      KEEP_CN(IR_IDX_R(ir_idx));
      break;

   case AT_Tbl_Idx:
      KEEP_ATTR(IR_IDX_R(ir_idx));
      break;

   case IR_Tbl_Idx:
      KEEP_IR(IR_IDX_R(ir_idx));
      break;

   case IL_Tbl_Idx:
      set_mod_link_tbl_for_il(IR_IDX_R(ir_idx));
      break;

   case NO_Tbl_Idx:
   case SH_Tbl_Idx:
      break;
   }
   
   TRACE (Func_Exit, "set_mod_link_tbl_for_ir", NULL);

   return;

}   /* set_mod_link_tbl_for_ir */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Set fields in the module link table for IL.                           *|
|*									      *|
|* Input parameters:							      *|
|*	list_idx    => Index to set link fields for.                          *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/
static	void  set_mod_link_tbl_for_il(int	list_idx)

{

   TRACE (Func_Entry, "set_mod_link_tbl_for_il", NULL);

   if (ML_IL_KEEP_ME(list_idx)) {
      return;
   }

   while (list_idx != NULL_IDX) {
      ML_IL_KEEP_ME(list_idx)	= TRUE;
      ML_IL_IDX(list_idx)	= list_idx;

      switch (IL_FLD(list_idx)) {
         case CN_Tbl_Idx:
            KEEP_CN(IL_IDX(list_idx));
            break;

         case AT_Tbl_Idx:
            KEEP_ATTR(IL_IDX(list_idx));
            break;

         case IR_Tbl_Idx:
            KEEP_IR(IL_IDX(list_idx));
            break;

         case IL_Tbl_Idx:
            set_mod_link_tbl_for_il(IL_IDX(list_idx));
            break;

         case NO_Tbl_Idx:
         case SH_Tbl_Idx:
            break;
      }
      list_idx = IL_NEXT_LIST_IDX(list_idx);
   }
   
   TRACE (Func_Exit, "set_mod_link_tbl_for_il", NULL);

   return;

}   /* set_mod_link_tbl_for_il */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Set fields in the module link table for a constant table entry.       *|
|*									      *|
|* Input parameters:							      *|
|*	cn_idx => Index of constant table entry to have links set.            *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/
static	void  set_mod_link_tbl_for_cn(int	cn_idx)

{
   size_offset_type	len;
   long			length;
   int			type_idx;


   TRACE (Func_Entry, "set_mod_link_tbl_for_cn", NULL);

   /* KAY - TEMPORARY - REPLACE WITH INTERNAL ERROR               */
   /*       If cn_idx is NULL, it should be an internal situation */

   if (cn_idx == NULL_IDX) {
      PRINTMSG(stmt_start_line, 626, Internal, stmt_start_col,
               "non zero cn_idx",
               "set_mod_link_tbl_for_cn");
   }

   if (!ML_CN_KEEP_ME(cn_idx)) {
      ML_CN_KEEP_ME(cn_idx)	= TRUE;
      ML_CN_IDX(cn_idx)		= cn_idx;
      type_idx			= CN_TYPE_IDX(cn_idx);

      set_mod_link_tbl_for_typ(type_idx);

      switch (TYP_TYPE(type_idx)) {
      case Typeless:
         length	= (long) (CN_EXTRA_ZERO_WORD(cn_idx) ?
                              STORAGE_WORD_SIZE(TYP_BIT_LEN(type_idx)) + 1 :
                              STORAGE_WORD_SIZE(TYP_BIT_LEN(type_idx)));
         break;

      case Character:
         len.idx	= TYP_IDX(type_idx);
         len.fld	= CN_Tbl_Idx;

         BYTES_TO_WORDS(len, TARGET_BITS_PER_WORD);

         if (len.fld == CN_Tbl_Idx) {
            length = (long) CN_INT_TO_C(len.idx);  /* KAYKAY */
         }
         else {
            length = (long) F_INT_TO_C(len.constant, TYP_LINEAR(len.type_idx));
         }
         length	= CN_EXTRA_ZERO_WORD(cn_idx) ? length + 1: length;
         break;

# if defined(_TARGET_OS_MAX)
      case Complex:
         if (TYP_LINEAR(type_idx) == Complex_4) {

            /* Complex_4 constants are stored in two words on t3e */

            length = 2;
         }
         else {
            length = TARGET_BITS_TO_WORDS(
                  storage_bit_size_tbl[TYP_LINEAR(CN_TYPE_IDX(cn_idx))]);
         }
         break;
# endif

      default:
         length	= TARGET_BITS_TO_WORDS(
                  storage_bit_size_tbl[TYP_LINEAR(CN_TYPE_IDX(cn_idx))]);
         break;
      }

      ML_CP_LEN(CN_POOL_IDX(cn_idx))		= length;
      ML_CP_KEEP_ME(CN_POOL_IDX(cn_idx))	= TRUE;
      ML_CP_IDX(CN_POOL_IDX(cn_idx))		= CN_POOL_IDX(cn_idx);

# if defined(_HOST32) 

      if (DALIGN_TEST_CONDITION(type_idx)) {
          ML_CP_DALIGN_ME(CN_POOL_IDX(cn_idx))  = TRUE;
      }

# endif
   }

   TRACE (Func_Exit, "set_mod_link_tbl_for_cn", NULL);

   return;

}   /* set_mod_link_tbl_for_cn */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Set fields in the module link table for TYP.                          *|
|*									      *|
|* Input parameters:							      *|
|*	typ_idx     => Index to set link fields for.                          *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/
static	void  set_mod_link_tbl_for_typ(int	typ_idx)

{
   int		attr_idx;


   TRACE (Func_Entry, "set_mod_link_tbl_for_typ", NULL);

   if (typ_idx != NULL_IDX && !ML_TYP_KEEP_ME(typ_idx)) {
      ML_TYP_KEEP_ME(typ_idx)	= TRUE;
      ML_TYP_IDX(typ_idx)	= typ_idx;

      if (TYP_TYPE(typ_idx) == Character) {

         if (TYP_FLD(typ_idx) == CN_Tbl_Idx) {
            KEEP_CN(TYP_IDX(typ_idx));
         }
         else if (TYP_FLD(typ_idx) == AT_Tbl_Idx) {
            KEEP_ATTR(TYP_IDX(typ_idx));
         }
      }
      else if (TYP_TYPE(typ_idx) == Structure) {
         attr_idx = TYP_IDX(typ_idx);
   
         while (AT_ATTR_LINK(attr_idx) != NULL_IDX) {
            attr_idx = AT_ATTR_LINK(attr_idx);
         }
   
         TYP_IDX(typ_idx) = attr_idx;

         KEEP_ATTR(attr_idx);
      }
   }

   TRACE (Func_Exit, "set_mod_link_tbl_for_typ", NULL);

   return;

}  /* set_mod_link_tbl_for_typ */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Go through the module link table, assigning new indexes to all        *|
|*      entries that must be left in the compressed tables.                   *|
|*	The zeroth entry of the mod link table contains the index to start    *|
|*      for each of the tables.  This is the mod link index to start with     *|
|*	and the tbl index to start assigning with.			      *|
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
static void  assign_new_idxs(boolean	resolving_duplicates)

{
   int		at_new_tbl_idx	= ML_AT_IDX(0) + 1;
   int		bd_new_tbl_idx	= ML_BD_IDX(0) + 1;
   int		cn_new_tbl_idx	= ML_CN_IDX(0) + 1;
   int		cp_new_tbl_idx	= ML_CP_IDX(0) + 1;
   int		end_idx;
   int		idx;
   int		il_new_tbl_idx	= ML_IL_IDX(0) + 1;
   int		ir_new_tbl_idx	= ML_IR_IDX(0) + 1;
   int		ln_new_tbl_idx	= ML_LN_IDX(0) + 1;
   int		mod_idx;
   int		new_idx;
   int		np_new_tbl_idx	= ML_NP_IDX(0) + 1;
   int		sb_new_tbl_idx	= ML_SB_IDX(0) + 1;
   int		sh_new_tbl_idx	= ML_SH_IDX(0) + 1;
   int		sn_new_tbl_idx	= ML_SN_IDX(0) + 1;
   int		typ_new_tbl_idx	= ML_TYP_IDX(0)+ 1;


   /* All the ML_xx_IDX(0) are the last used indexes for the tables */


   TRACE (Func_Entry, "assign_new_idxs", NULL);

   if (save_const_tbl_idx != NULL_IDX) {

      /* We are processing an incoming module.  We have left room */
      /* for a copy of the full constant table and constant pool. */

      end_idx		= const_tbl_idx;
      const_tbl_idx	= save_const_tbl_idx;
      const_pool_idx	= save_const_pool_idx;

  Pragma("_CRI ivdep")        

      for (mod_idx = 1; mod_idx < cn_new_tbl_idx; mod_idx++) {
         ML_CN_IDX(mod_idx)	= mod_idx;
      }

  Pragma("_CRI ivdep")        

      for (mod_idx = 1; mod_idx < cp_new_tbl_idx; mod_idx++) {
         ML_CP_IDX(mod_idx)	= mod_idx;
      }

      for (mod_idx = cn_new_tbl_idx; mod_idx <= end_idx; mod_idx++) {

         if (ML_CN_KEEP_ME(mod_idx)) {
            new_idx		= ntr_const_tbl(CN_TYPE_IDX(mod_idx),
                                                CN_EXTRA_ZERO_WORD(mod_idx),
                                                &CN_CONST(mod_idx));
            ML_CN_IDX(mod_idx)	= new_idx;
         }
      }

      ML_CN_IDX(0)	= const_tbl_idx;
      ML_CP_IDX(0)	= const_pool_idx;
   }

  Pragma("_CRI ivdep")        
   for (mod_idx = 1; mod_idx < at_new_tbl_idx; mod_idx++) {
      ML_AT_IDX(mod_idx)		= mod_idx;
      ML_AT_COMPRESSED_IDX(mod_idx)	= TRUE;
      ML_AT_KEEP_ME(mod_idx)		= TRUE;
   }

   if (resolving_duplicates) {

      for (mod_idx = at_new_tbl_idx; mod_idx <= attr_tbl_idx; mod_idx++) {

         if (ML_AT_KEEP_ME(mod_idx)) {
            ML_AT_IDX(mod_idx)			= at_new_tbl_idx;
            ML_AT_COMPRESSED_IDX(mod_idx)	= TRUE;
            at_new_tbl_idx++;
         }
         else if (ML_AT_IDX(mod_idx) != mod_idx && 
                  ML_AT_IDX(mod_idx) != NULL_IDX) {

            /* This attr has been resolved to another attr.  Find out if    */
            /* any attr in this chain needs to be kept.  If the attr does   */
            /* not need to be kept, just skip to the next attr.             */

            idx = mod_idx;

            /* Search until we find the attr that gets kept in this chain.  */
            /* If ML_AT_KEEP_ME is set, we've found the attr that gets kept */
            /* in the chain.  If ML_AT_COMPRESSED_IDX is set, this attr in  */
            /* the chain has already been changed to index to its new       */
            /* compressed index.  Following is an example of how this can   */
            /* happen:     ML_AT_IDX(344) = 1268.  ML_AT_KEEP_ME(1268) = T  */
            /* We enter this code with 344.  After this code sequence,      */
            /* ML_AT_IDX(1268) = 344, ML_AT_KEEP_ME(1268) = FALSE.          */
            /* ML_AT_IDX(344) = 20 (The new compressed index.)              */
            /* ML_AT_KEEP_ME(344) = TRUE.  ML_AT_COMPRESSED_IDX(344) = TRUE.*/
            /* 								    */
            /* We enter this code with 1268.  After this code sequence,     */
            /* ML_AT_IDX(1268) = 20 and ML_AT_COMPRESSED_IDX(1268) = TRUE.  */
            /* 								    */
            /* Now, suppose we also have ML_AT_IDX(3000) = 1268.            */
            /* We enter this code with 3000.  We will do the following loop,*/
            /* but will stop at 1268, because it already has its compressed */
            /* index.  We will then use that index.  So in conclusion,      */
            /* attrs 3000, 1268 and 344 are all the same.  What we want is  */
            /* just one copy of this attr at index 20, when compression is  */
            /* finished.                                                    */

            while (idx != NULL_IDX &&
                   !ML_AT_KEEP_ME(idx) &&
                   !ML_AT_COMPRESSED_IDX(idx)) {
               idx = ML_AT_IDX(idx);
            }

            if (idx > mod_idx) {

               /* The duplicate attr being kept has not been assigned a new */
               /* index yet.  Switch the attrs that are being kept, since   */
               /* we are ready to assign a new index to this attr.          */

               /* Since these are the same thing - switch the actual info   */
               /* in the attr, so that all information about which entries  */
               /* in which tables should be kept will remain okay.          */

               COPY_ATTR_NTRY(AT_WORK_IDX, mod_idx);
               COPY_ATTR_NTRY(mod_idx, idx);
               COPY_ATTR_NTRY(idx, AT_WORK_IDX);

               ML_AT_KEEP_ME(idx)	= FALSE;
               ML_AT_KEEP_ME(mod_idx)	= TRUE;

               /* Set the higher duplicate attr ML_AT_IDX to lower duplicate */
               /* attr index.  When the higher duplicate attr comes up, it   */
               /* will fall through here but take the else clause instead of */
               /* this clause.  That will get its ML_AT_IDX set correctly.   */
               /* We cannot set ML_AT_IDX directly, because when the higher  */
               /* attr comes through, it won't know that its ML_AT_IDX has   */
               /* already been adjusted.                                     */

               ML_AT_IDX(idx)			= mod_idx;
               ML_AT_IDX(mod_idx)		= at_new_tbl_idx;
               ML_AT_COMPRESSED_IDX(mod_idx)	= TRUE;
               at_new_tbl_idx++;
            }
            else if (idx != NULL_IDX) {
               ML_AT_IDX(mod_idx)		= ML_AT_IDX(idx);
               ML_AT_COMPRESSED_IDX(mod_idx)	= TRUE;
            }
         }
      }
   }
   else {

  Pragma("_CRI ivdep")        
      for (mod_idx = at_new_tbl_idx; mod_idx <= attr_tbl_idx; mod_idx++) {

         if (ML_AT_KEEP_ME(mod_idx)) {
            ML_AT_IDX(mod_idx)	= at_new_tbl_idx;
            at_new_tbl_idx++;
         }
      }
   }

  Pragma("_CRI ivdep")        
   for (mod_idx = 1; mod_idx < bd_new_tbl_idx; mod_idx++) {
      ML_BD_IDX(mod_idx)	= mod_idx;
   }

  Pragma("_CRI ivdep")        
   for (mod_idx = bd_new_tbl_idx; mod_idx <= bounds_tbl_idx; mod_idx++) {

      if (ML_BD_KEEP_ME(mod_idx)) {
         ML_BD_IDX(mod_idx)	= bd_new_tbl_idx;
         bd_new_tbl_idx		= bd_new_tbl_idx + BD_NTRY_SIZE(mod_idx);
      }
   }

   if (save_const_tbl_idx == NULL_IDX) {

  Pragma("_CRI ivdep")        
      for (mod_idx = 1; mod_idx < cn_new_tbl_idx; mod_idx++) {
         ML_CN_IDX(mod_idx)	= mod_idx;
      }
   
  Pragma("_CRI ivdep")        
      for (mod_idx = cn_new_tbl_idx; mod_idx <= const_tbl_idx; mod_idx++) {
   
         if (ML_CN_KEEP_ME(mod_idx)) {
            ML_CN_IDX(mod_idx)	= cn_new_tbl_idx;
            cn_new_tbl_idx++;
         }
      }

  Pragma("_CRI ivdep")        
      for (mod_idx = 1; mod_idx < cp_new_tbl_idx; mod_idx++) {
         ML_CP_IDX(mod_idx)	= mod_idx;
      }

      end_idx = const_pool_idx;

      for (mod_idx = cp_new_tbl_idx; mod_idx <= end_idx; mod_idx++) {

         if (ML_CP_KEEP_ME(mod_idx)) {

# if defined(_HOST32)
   
            if (ML_CP_DALIGN_ME(mod_idx)) {

               while ((((long)&const_pool[cp_new_tbl_idx]) % 8) != 0) {
                  cp_new_tbl_idx++;
                  TBL_REALLOC_CK(const_pool, 1);
               }
   
               if (const_pool_idx > mod_link_tbl_idx) {
                  idx = mod_link_tbl_idx;
                  TBL_REALLOC_CK(mod_link_tbl, const_pool_idx);
   
                  for (; idx <= mod_link_tbl_idx; idx++) {
                     CLEAR_TBL_NTRY(mod_link_tbl, idx);
                  }
               }
   
            }
   
# endif
            ML_CP_IDX(mod_idx)	= cp_new_tbl_idx;
            cp_new_tbl_idx		+= ML_CP_LEN(mod_idx);
         }
      }
   }

  Pragma("_CRI ivdep")        
   for (mod_idx = 1; mod_idx < il_new_tbl_idx; mod_idx++) {
      ML_IL_IDX(mod_idx)	= mod_idx;
   }

  Pragma("_CRI ivdep")        
   for (mod_idx = il_new_tbl_idx; mod_idx <= ir_list_tbl_idx; mod_idx++) {

      if (ML_IL_KEEP_ME(mod_idx)) {
         ML_IL_IDX(mod_idx) = il_new_tbl_idx;
         il_new_tbl_idx++;
      }
   }

  Pragma("_CRI ivdep")        
   for (mod_idx = 1; mod_idx < ir_new_tbl_idx; mod_idx++) {
      ML_IR_IDX(mod_idx)	= mod_idx;
   }

  Pragma("_CRI ivdep")        
   for (mod_idx = ir_new_tbl_idx; mod_idx <= ir_tbl_idx; mod_idx++) {

      if (ML_IR_KEEP_ME(mod_idx)) {
         ML_IR_IDX(mod_idx) = ir_new_tbl_idx;
         ir_new_tbl_idx++;
      }
   }

  Pragma("_CRI ivdep")        
   for (mod_idx = 1; mod_idx < ln_new_tbl_idx; mod_idx++) {
      ML_LN_IDX(mod_idx)	= mod_idx;
   }

  Pragma("_CRI ivdep")        
   for (mod_idx = ln_new_tbl_idx; mod_idx <= loc_name_tbl_idx; mod_idx++) {

      if (ML_LN_KEEP_ME(mod_idx)) {
         ML_LN_IDX(mod_idx) = ln_new_tbl_idx;
         ln_new_tbl_idx++;
      }
   }

  Pragma("_CRI ivdep")        
   for (mod_idx = 1; mod_idx < np_new_tbl_idx; mod_idx++) {
      ML_NP_IDX(mod_idx)	= mod_idx;
   }

  Pragma("_CRI ivdep")        
   for (mod_idx = np_new_tbl_idx; mod_idx <= name_pool_idx; mod_idx++) {

      if (ML_NP_KEEP_ME(mod_idx)) {
         ML_NP_IDX(mod_idx)	= np_new_tbl_idx;
         ML_NP_LEN(mod_idx)	= WORD_LEN(ML_NP_LEN(mod_idx));
         np_new_tbl_idx		+= ML_NP_LEN(mod_idx);
      }
   }

  Pragma("_CRI ivdep")        
   for (mod_idx = 1; mod_idx < sb_new_tbl_idx; mod_idx++) {
      ML_SB_IDX(mod_idx)	= mod_idx;
   }

  Pragma("_CRI ivdep")        
   for (mod_idx = sb_new_tbl_idx; mod_idx <= stor_blk_tbl_idx; mod_idx++) {

      if (ML_SB_KEEP_ME(mod_idx)) {
         ML_SB_IDX(mod_idx)	= sb_new_tbl_idx;
         sb_new_tbl_idx++;
      }
   }

  Pragma("_CRI ivdep")        
   for (mod_idx = 1; mod_idx < sn_new_tbl_idx; mod_idx++) {
      ML_SN_IDX(mod_idx)	= mod_idx;
   }

  Pragma("_CRI ivdep")        
   for (mod_idx = sn_new_tbl_idx; mod_idx <= sec_name_tbl_idx; mod_idx++) {

      if (ML_SN_KEEP_ME(mod_idx)) {
         ML_SN_IDX(mod_idx)	= sn_new_tbl_idx;
         sn_new_tbl_idx++;
      }
   }

  Pragma("_CRI ivdep")        
   for (mod_idx = 1; mod_idx < sh_new_tbl_idx; mod_idx++) {
      ML_SH_IDX(mod_idx)	= mod_idx;
   }

  Pragma("_CRI ivdep")        
   for (mod_idx = sh_new_tbl_idx; mod_idx <= sh_tbl_idx; mod_idx++) {

      if (ML_SH_KEEP_ME(mod_idx)) {
         ML_SH_IDX(mod_idx)	= sh_new_tbl_idx;
         sh_new_tbl_idx++;
      }
   }

  Pragma("_CRI ivdep")        
   for (mod_idx = 1; mod_idx < typ_new_tbl_idx; mod_idx++) {
      ML_TYP_IDX(mod_idx)	= mod_idx;
   }

   /* Assigning typ indexes is handled in compress_typ_table */

   TRACE (Func_Exit, "assign_new_idxs", NULL);

   return;

}   /* assign_new_idxs */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	This moves the tables, writing over entries that should be compressed *|
|*      out.                                                                  *|
|*									      *|
|* Input parameters:							      *|
|*	al_idx -> Index to start checking attr_list_tbl for updating the      *|
|*	          AL_ATTR_IDX.                                                *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/
static void  compress_tbls(int		al_idx,
			   boolean	from_interface)
{
   int		at_idx;
   int		bd_idx;
   int		cn_idx;
   int		cp_idx;
   int		dim;
   int		end_idx;
   int		idx;
   int		il_idx;
   int		ir_idx;
   int		ln_idx;
   int		mod_idx;
   int		np_idx;
   int		sb_idx;
   int		sh_idx;
   int		sn_idx;
   int		start_idx;
   int		typ_idx;



   TRACE (Func_Entry, "compress_tbls", NULL);

   /* The zeroth entry in the module link table contains the starting index   */
   /* for each table.  This allows partial compression at the end of tables.  */
   /* After this field is saved, clear these entries.  They must be NULL_IDX  */
   /* so that if a field has NULL_IDX in it, it will not change.   All other  */
   /* fields are required to be set correctly or they will end up NULL.       */

   at_idx = ML_AT_IDX(0);
   bd_idx = ML_BD_IDX(0);
   cn_idx = ML_CN_IDX(0);
   cp_idx = ML_CP_IDX(0);
   il_idx = ML_IL_IDX(0);
   ir_idx = ML_IR_IDX(0);
   ln_idx = ML_LN_IDX(0);
   np_idx = ML_NP_IDX(0);
   sb_idx = ML_SB_IDX(0);
   sh_idx = ML_SH_IDX(0);
   sn_idx = ML_SN_IDX(0);
   typ_idx= ML_TYP_IDX(0);

   CLEAR_TBL_NTRY(mod_link_tbl, NULL_IDX);

   /* Compresses the type table, by sharing duplicate entries */
   /* and collapsing out unneeded type entries.               */

   compress_type_tbl(typ_idx);

   if (!only_update_new_tbl_entries) {
      update_idxs_in_attr_entry(1,at_idx);
   }

   start_idx	= at_idx+1;


   for (mod_idx = start_idx; mod_idx <= attr_tbl_idx; mod_idx++) {

      if (ML_AT_KEEP_ME(mod_idx)) {
         ++at_idx;
         COPY_ATTR_NTRY(at_idx, mod_idx);
      }
   }

   update_idxs_in_attr_entry(start_idx, at_idx);

   if (count_derived_types) {

      for (mod_idx = start_idx; mod_idx <= at_idx; mod_idx++) {
         if (AT_OBJ_CLASS(mod_idx) == Derived_Type) {
            num_module_derived_types++;
         }
      }
   }

   /* mod_idx	= (only_update_new_tbl_entries) ? bd_idx + 1 : 1; */
   mod_idx	= 1;

   while (mod_idx <= bd_idx) {

      if (!BD_USED_NTRY(mod_idx)) {  /* Entry from the free list */
         BD_NEXT_FREE_NTRY(mod_idx)	= ML_BD_IDX(BD_NEXT_FREE_NTRY(mod_idx));
         mod_idx = mod_idx + BD_NTRY_SIZE(mod_idx);
      }
      else if (BD_DIST_NTRY(mod_idx)) {

         for (dim = 1; dim <= BD_RANK(mod_idx); dim++) {

            if (BD_CYCLIC_FLD(mod_idx, dim) == CN_Tbl_Idx) {
               BD_CYCLIC_IDX(mod_idx, dim) = 
                                  ML_CN_IDX(BD_CYCLIC_IDX(mod_idx, dim));
            }
            else if (BD_CYCLIC_FLD(mod_idx, dim) == AT_Tbl_Idx) {
               BD_CYCLIC_IDX(mod_idx, dim) = 
                                  ML_AT_IDX(BD_CYCLIC_IDX(mod_idx, dim));
            }

            if (BD_ONTO_FLD(mod_idx, dim) == CN_Tbl_Idx) {
               BD_ONTO_IDX(mod_idx, dim) = 
                                  ML_CN_IDX(BD_ONTO_IDX(mod_idx, dim));
            }
            else if (BD_ONTO_FLD(mod_idx, dim) == AT_Tbl_Idx) {
               BD_ONTO_IDX(mod_idx, dim) = 
                                  ML_AT_IDX(BD_ONTO_IDX(mod_idx, dim));
            }
         }
         mod_idx = mod_idx + BD_RANK(mod_idx) + 1;  /* 1 for header */
      }
      else if (BD_ARRAY_CLASS(mod_idx) != Deferred_Shape) {

         if (BD_LEN_FLD(mod_idx) == CN_Tbl_Idx) {
            BD_LEN_IDX(mod_idx) = ML_CN_IDX(BD_LEN_IDX(mod_idx));
         }
         else if (BD_LEN_FLD(mod_idx) == AT_Tbl_Idx) {
            BD_LEN_IDX(mod_idx)	= ML_AT_IDX(BD_LEN_IDX(mod_idx));
         }

         for (dim = 1; dim <= BD_RANK(mod_idx); dim++) {

            if (BD_LB_FLD(mod_idx, dim) == CN_Tbl_Idx) {
               BD_LB_IDX(mod_idx, dim) = ML_CN_IDX(BD_LB_IDX(mod_idx, dim));
            }
            else if (BD_LB_FLD(mod_idx, dim) == AT_Tbl_Idx) {
               BD_LB_IDX(mod_idx, dim) = ML_AT_IDX(BD_LB_IDX(mod_idx, dim));
            }

            if (BD_UB_FLD(mod_idx, dim) == CN_Tbl_Idx) {
               BD_UB_IDX(mod_idx, dim) = ML_CN_IDX(BD_UB_IDX(mod_idx, dim));
            }
            else if (BD_UB_FLD(mod_idx, dim) == AT_Tbl_Idx) {
               BD_UB_IDX(mod_idx, dim) = ML_AT_IDX(BD_UB_IDX(mod_idx, dim));
            }

            if (BD_XT_FLD(mod_idx, dim) == CN_Tbl_Idx) {
               BD_XT_IDX(mod_idx, dim) = ML_CN_IDX(BD_XT_IDX(mod_idx, dim));
            }
            else if (BD_XT_FLD(mod_idx, dim) == AT_Tbl_Idx) {
               BD_XT_IDX(mod_idx, dim) = ML_AT_IDX(BD_XT_IDX(mod_idx, dim));
            }

            if (BD_SM_FLD(mod_idx, dim) == CN_Tbl_Idx) {
               BD_SM_IDX(mod_idx, dim) = ML_CN_IDX(BD_SM_IDX(mod_idx, dim));
            }
            else if (BD_SM_FLD(mod_idx, dim) == AT_Tbl_Idx) {
               BD_SM_IDX(mod_idx, dim) = ML_AT_IDX(BD_SM_IDX(mod_idx, dim));
            }
         }
         mod_idx = mod_idx + BD_RANK(mod_idx) + 1;  /* 1 for header */
      }
      else {
         mod_idx++;
      }
   }

   start_idx	= bd_idx+1;

   for (mod_idx = start_idx; mod_idx <= bounds_tbl_idx; mod_idx++) {

      if (ML_BD_KEEP_ME(mod_idx)) {
         ++bd_idx;
         COPY_BD_NTRY(bd_idx, mod_idx);

         if (BD_DIST_NTRY(bd_idx) ||
             BD_ARRAY_CLASS(bd_idx) != Deferred_Shape) {

            bd_idx = bd_idx + BD_RANK(bd_idx);
         }
      }
   }

   for (mod_idx = start_idx; mod_idx <= bd_idx; mod_idx++) {

      if (BD_DIST_NTRY(mod_idx)) {

         for (dim = 1; dim <= BD_RANK(mod_idx); dim++) {

            if (BD_CYCLIC_FLD(mod_idx, dim) == CN_Tbl_Idx) {
               BD_CYCLIC_IDX(mod_idx, dim) =
                                  ML_CN_IDX(BD_CYCLIC_IDX(mod_idx, dim));
            }
            else if (BD_CYCLIC_FLD(mod_idx, dim) == AT_Tbl_Idx) {
               BD_CYCLIC_IDX(mod_idx, dim) =
                                  ML_AT_IDX(BD_CYCLIC_IDX(mod_idx, dim));
            }

            if (BD_ONTO_FLD(mod_idx, dim) == CN_Tbl_Idx) {
               BD_ONTO_IDX(mod_idx, dim) =
                                  ML_CN_IDX(BD_ONTO_IDX(mod_idx, dim));
            }
            else if (BD_ONTO_FLD(mod_idx, dim) == AT_Tbl_Idx) {
               BD_ONTO_IDX(mod_idx, dim) =
                                  ML_AT_IDX(BD_ONTO_IDX(mod_idx, dim));
            }
         }
         mod_idx = mod_idx + BD_RANK(mod_idx);
      }
      else if (BD_ARRAY_CLASS(mod_idx) != Deferred_Shape) {

         if (BD_LEN_FLD(mod_idx) == CN_Tbl_Idx) {
            BD_LEN_IDX(mod_idx)	= ML_CN_IDX(BD_LEN_IDX(mod_idx));
         }
         else if (BD_LEN_FLD(mod_idx) == AT_Tbl_Idx) {
            BD_LEN_IDX(mod_idx)	= ML_AT_IDX(BD_LEN_IDX(mod_idx));
         }

         for (dim = 1; dim <= BD_RANK(mod_idx); dim++) {

            if (BD_LB_FLD(mod_idx, dim) == CN_Tbl_Idx) {
               BD_LB_IDX(mod_idx, dim) = ML_CN_IDX(BD_LB_IDX(mod_idx, dim));
            }
            else if (BD_LB_FLD(mod_idx, dim) == AT_Tbl_Idx) {
               BD_LB_IDX(mod_idx, dim) = ML_AT_IDX(BD_LB_IDX(mod_idx, dim));
            }

            if (BD_UB_FLD(mod_idx, dim) == CN_Tbl_Idx) {
               BD_UB_IDX(mod_idx, dim) = ML_CN_IDX(BD_UB_IDX(mod_idx, dim));
            }
            else if (BD_UB_FLD(mod_idx, dim) == AT_Tbl_Idx) {
               BD_UB_IDX(mod_idx, dim) = ML_AT_IDX(BD_UB_IDX(mod_idx, dim));
            }

            if (BD_XT_FLD(mod_idx, dim) == CN_Tbl_Idx) {
               BD_XT_IDX(mod_idx, dim) = ML_CN_IDX(BD_XT_IDX(mod_idx, dim));
            }
            else if (BD_XT_FLD(mod_idx, dim) == AT_Tbl_Idx) {
               BD_XT_IDX(mod_idx, dim) = ML_AT_IDX(BD_XT_IDX(mod_idx, dim));
            }

            if (BD_SM_FLD(mod_idx, dim) == CN_Tbl_Idx) {
               BD_SM_IDX(mod_idx, dim) = ML_CN_IDX(BD_SM_IDX(mod_idx, dim));
            }
            else if (BD_SM_FLD(mod_idx, dim) == AT_Tbl_Idx) {
               BD_SM_IDX(mod_idx, dim) = ML_AT_IDX(BD_SM_IDX(mod_idx, dim));
            }
         }
         mod_idx = mod_idx + BD_RANK(mod_idx);
      }
   }

   start_idx	= cn_idx+1;

  Pragma("_CRI ivdep")        
   for (mod_idx = cn_idx+1; mod_idx <= const_tbl_idx; mod_idx++) {

      if (ML_CN_KEEP_ME(mod_idx)) {
         const_tbl[++cn_idx]	= const_tbl[mod_idx];
      }
   }

# if defined(_DEBUG)
   for (mod_idx = 1; mod_idx <= const_tbl_idx; mod_idx++) {

      if (CN_POOL_IDX(mod_idx) == NULL_IDX) {
         PRINTMSG(stmt_start_line, 1349, Internal, 0, mod_idx);
      }
   }
# endif


   for (mod_idx = 1; mod_idx <= cn_idx; mod_idx++) {
      CN_TYPE_IDX(mod_idx)	= ML_TYP_IDX(CN_TYPE_IDX(mod_idx));
      CN_POOL_IDX(mod_idx)	= ML_CP_IDX(CN_POOL_IDX(mod_idx));
   }

   for (mod_idx = cp_idx+1; mod_idx <= const_pool_idx; mod_idx++) {

      if (ML_CP_KEEP_ME(mod_idx)) {

# if defined(_HOST32) 

         if (ML_CP_DALIGN_ME(mod_idx)) {
            cp_idx = ML_CP_IDX(mod_idx) - 1;
         }
# endif

         for (idx = 0; idx < ML_CP_LEN(mod_idx); idx++) {
            const_pool[++cp_idx]	= const_pool[mod_idx+idx];
         }
      }
   }

   for (mod_idx = (from_interface || !only_update_new_tbl_entries) ?
                  1 : (il_idx+1); mod_idx <= il_idx; mod_idx++) {
      IL_NEXT_LIST_IDX(mod_idx)	= ML_IL_IDX(IL_NEXT_LIST_IDX(mod_idx));

      if (!IL_ARG_DESC_VARIANT(mod_idx)) {
         IL_PREV_LIST_IDX(mod_idx) = ML_IL_IDX(IL_PREV_LIST_IDX(mod_idx));
      }

      switch (IL_FLD(mod_idx)) {
      case CN_Tbl_Idx:
         IL_IDX(mod_idx) = ML_CN_IDX(IL_IDX(mod_idx));
         break;

      case AT_Tbl_Idx:
         IL_IDX(mod_idx) = ML_AT_IDX(IL_IDX(mod_idx));
         break;
      
      case IL_Tbl_Idx:
         IL_IDX(mod_idx) = ML_IL_IDX(IL_IDX(mod_idx));
         break;

      case IR_Tbl_Idx:
         IL_IDX(mod_idx) = ML_IR_IDX(IL_IDX(mod_idx));
         break;
      }
   }

   start_idx	= il_idx + 1;

   for (mod_idx = start_idx; mod_idx <= ir_list_tbl_idx; mod_idx++) {

      if (ML_IL_KEEP_ME(mod_idx)) {
         ir_list_tbl[++il_idx]		= ir_list_tbl[mod_idx];
      }
   }

   for (mod_idx = start_idx; mod_idx <= il_idx; mod_idx++) {
      IL_NEXT_LIST_IDX(mod_idx)	= ML_IL_IDX(IL_NEXT_LIST_IDX(mod_idx));

      if (!IL_ARG_DESC_VARIANT(mod_idx)) {
         IL_PREV_LIST_IDX(mod_idx) = ML_IL_IDX(IL_PREV_LIST_IDX(mod_idx));
      }

      switch (IL_FLD(mod_idx)) {
      case CN_Tbl_Idx:
         IL_IDX(mod_idx) = ML_CN_IDX(IL_IDX(mod_idx));
         break;

      case AT_Tbl_Idx:
         IL_IDX(mod_idx) = ML_AT_IDX(IL_IDX(mod_idx));
         break;
      
      case IL_Tbl_Idx:
         IL_IDX(mod_idx) = ML_IL_IDX(IL_IDX(mod_idx));
         break;

      case IR_Tbl_Idx:
         IL_IDX(mod_idx) = ML_IR_IDX(IL_IDX(mod_idx));
         break;
      }
   }

   for (mod_idx = (from_interface || !only_update_new_tbl_entries) ?
                  1 : (ir_idx + 1); mod_idx <= ir_idx; mod_idx++) {
      IR_TYPE_IDX(mod_idx)	= ML_TYP_IDX(IR_TYPE_IDX(mod_idx));

      switch (IR_FLD_L(mod_idx)) {
      case CN_Tbl_Idx:
         IR_IDX_L(mod_idx) = ML_CN_IDX(IR_IDX_L(mod_idx));
         break;

      case AT_Tbl_Idx:
         IR_IDX_L(mod_idx) = ML_AT_IDX(IR_IDX_L(mod_idx));
         break;
         
      case IL_Tbl_Idx:
         IR_IDX_L(mod_idx) = ML_IL_IDX(IR_IDX_L(mod_idx));
         break;

      case IR_Tbl_Idx:
         IR_IDX_L(mod_idx) = ML_IR_IDX(IR_IDX_L(mod_idx));
         break;
      }

      switch (IR_FLD_R(mod_idx)) {
      case CN_Tbl_Idx:
         IR_IDX_R(mod_idx) = ML_CN_IDX(IR_IDX_R(mod_idx));
         break;

      case AT_Tbl_Idx:
         IR_IDX_R(mod_idx) = ML_AT_IDX(IR_IDX_R(mod_idx));
         break;
         
      case IL_Tbl_Idx:
         IR_IDX_R(mod_idx) = ML_IL_IDX(IR_IDX_R(mod_idx));
         break;

      case IR_Tbl_Idx:
         IR_IDX_R(mod_idx) = ML_IR_IDX(IR_IDX_R(mod_idx));
         break;
      }
   }

   start_idx = ir_idx+1;

   for (mod_idx = start_idx; mod_idx <= ir_tbl_idx; mod_idx++) {

      if (ML_IR_KEEP_ME(mod_idx)) {
         ir_tbl[++ir_idx]	= ir_tbl[mod_idx];
      }
   }

   for (mod_idx = start_idx; mod_idx <= ir_idx; mod_idx++) {
      IR_TYPE_IDX(mod_idx)	= ML_TYP_IDX(IR_TYPE_IDX(mod_idx));

      switch (IR_FLD_L(mod_idx)) {
      case CN_Tbl_Idx:
         IR_IDX_L(mod_idx) = ML_CN_IDX(IR_IDX_L(mod_idx));
         break;

      case AT_Tbl_Idx:
         IR_IDX_L(mod_idx) = ML_AT_IDX(IR_IDX_L(mod_idx));
         break;
      
      case IL_Tbl_Idx:
         IR_IDX_L(mod_idx) = ML_IL_IDX(IR_IDX_L(mod_idx));
         break;

      case IR_Tbl_Idx:
         IR_IDX_L(mod_idx) = ML_IR_IDX(IR_IDX_L(mod_idx));
         break;
      }

      switch (IR_FLD_R(mod_idx)) {
      case CN_Tbl_Idx:
         IR_IDX_R(mod_idx) = ML_CN_IDX(IR_IDX_R(mod_idx));
         break;

      case AT_Tbl_Idx:
         IR_IDX_R(mod_idx) = ML_AT_IDX(IR_IDX_R(mod_idx));
         break;
      
      case IL_Tbl_Idx:
         IR_IDX_R(mod_idx) = ML_IL_IDX(IR_IDX_R(mod_idx));
         break;

      case IR_Tbl_Idx:
         IR_IDX_R(mod_idx) = ML_IR_IDX(IR_IDX_R(mod_idx));
         break;
      }
   }

   start_idx	= ln_idx+1;

   for (mod_idx = start_idx; mod_idx <= loc_name_tbl_idx; mod_idx++) {

      if (ML_LN_KEEP_ME(mod_idx)) {
         loc_name_tbl[++ln_idx] = loc_name_tbl[mod_idx];
      }
   }

   for (mod_idx = start_idx; mod_idx <= ln_idx; mod_idx++) {

     if (LN_ATTR_IDX(mod_idx) != NULL_IDX) {
        LN_ATTR_IDX(mod_idx)	= ML_AT_IDX(LN_ATTR_IDX(mod_idx));
        LN_NAME_IDX(mod_idx)	= AT_NAME_IDX(LN_ATTR_IDX(mod_idx));
        LN_NAME_LEN(mod_idx)	= AT_NAME_LEN(LN_ATTR_IDX(mod_idx));
     }
     else {
        LN_NAME_IDX(mod_idx)	= ML_NP_IDX(LN_NAME_IDX(mod_idx));
     }
   }

   for (mod_idx = np_idx+1; mod_idx <= name_pool_idx; mod_idx++) {

      if (ML_NP_KEEP_ME(mod_idx)) {

         for (idx = 0; idx < ML_NP_LEN(mod_idx); idx++) {
            name_pool[++np_idx].name_long = name_pool[mod_idx+idx].name_long;
         }
      }
   }

   /* for (mod_idx = (only_update_new_tbl_entries) ? (sb_idx + 1) : 1; */
   for (mod_idx = 1; mod_idx <= sb_idx; mod_idx++) {
      SB_NAME_IDX(mod_idx)		= ML_NP_IDX(SB_NAME_IDX(mod_idx));
      SB_MODULE_IDX(mod_idx)		= ML_AT_IDX(SB_MODULE_IDX(mod_idx));
#ifdef KEY /* Bug 14150 */
      int sb_ext_name_idx = SB_EXT_NAME_IDX(mod_idx);
      if (sb_ext_name_idx) {
	SB_EXT_NAME_IDX(mod_idx)	= ML_NP_IDX(sb_ext_name_idx);
      }
#endif /* KEY Bug 14150 */

      if (SB_FIRST_ATTR_IDX(mod_idx) != NULL_IDX) {
         SB_FIRST_ATTR_IDX(mod_idx) = ML_AT_IDX(SB_FIRST_ATTR_IDX(mod_idx));
      }

      switch (SB_LEN_FLD(mod_idx)) {
      case AT_Tbl_Idx:
         SB_LEN_IDX(mod_idx)		= ML_AT_IDX(SB_LEN_IDX(mod_idx));
         break;
     
      case CN_Tbl_Idx:
         SB_LEN_IDX(mod_idx)		= ML_CN_IDX(SB_LEN_IDX(mod_idx));
         break;

      case IL_Tbl_Idx:
         SB_LEN_IDX(mod_idx)		= ML_IL_IDX(SB_LEN_IDX(mod_idx));
         break;

      case IR_Tbl_Idx:
         SB_LEN_IDX(mod_idx)		= ML_IR_IDX(SB_LEN_IDX(mod_idx));
         break;
      }
   }

   start_idx = sb_idx + 1;

   for (mod_idx = start_idx; mod_idx <= stor_blk_tbl_idx; mod_idx++) {

      if (ML_SB_KEEP_ME(mod_idx)) {
         stor_blk_tbl[++sb_idx]		= stor_blk_tbl[mod_idx];
      }
   }

   for (mod_idx = start_idx; mod_idx <= sb_idx; mod_idx++) {
      SB_NAME_IDX(mod_idx)		= ML_NP_IDX(SB_NAME_IDX(mod_idx));
#ifdef KEY /* Bug 14150 */
      int sb_ext_name_idx = SB_EXT_NAME_IDX(mod_idx);
      if (sb_ext_name_idx) {
	SB_EXT_NAME_IDX(mod_idx)	= ML_NP_IDX(sb_ext_name_idx);
      }
#endif /* KEY Bug 14150 */
      SB_MODULE_IDX(mod_idx)		= ML_AT_IDX(SB_MODULE_IDX(mod_idx));

      if (SB_FIRST_ATTR_IDX(mod_idx) != NULL_IDX) {
         SB_FIRST_ATTR_IDX(mod_idx) = ML_AT_IDX(SB_FIRST_ATTR_IDX(mod_idx));
      }

      switch (SB_LEN_FLD(mod_idx)) {
      case AT_Tbl_Idx:
         SB_LEN_IDX(mod_idx)		= ML_AT_IDX(SB_LEN_IDX(mod_idx));
         break;
     
      case CN_Tbl_Idx:
         SB_LEN_IDX(mod_idx)		= ML_CN_IDX(SB_LEN_IDX(mod_idx));
         break;

      case IL_Tbl_Idx:
         SB_LEN_IDX(mod_idx)		= ML_IL_IDX(SB_LEN_IDX(mod_idx));
         break;

      case IR_Tbl_Idx:
         SB_LEN_IDX(mod_idx)		= ML_IR_IDX(SB_LEN_IDX(mod_idx));
         break;
      }
   }

   for (mod_idx = (from_interface || !only_update_new_tbl_entries) ?
                  1 : (sh_idx + 1); mod_idx <= sh_idx; mod_idx++) {
      SH_NEXT_IDX(mod_idx)		= ML_SH_IDX(SH_NEXT_IDX(mod_idx));
      SH_PREV_IDX(mod_idx)		= ML_SH_IDX(SH_PREV_IDX(mod_idx));
      SH_IR_IDX(mod_idx)		= ML_IR_IDX(SH_IR_IDX(mod_idx));

      if (SH_STMT_TYPE(mod_idx) != Statement_Num_Stmt) {
         SH_PARENT_BLK_IDX(mod_idx)	= ML_SH_IDX(SH_PARENT_BLK_IDX(mod_idx));
      }
   }

   start_idx = sh_idx + 1;

   for (mod_idx = start_idx; mod_idx <= sh_tbl_idx; mod_idx++) {

      if (ML_SH_KEEP_ME(mod_idx)) {
         sh_tbl[++sh_idx]		= sh_tbl[mod_idx];
      }
   }


   for (mod_idx = start_idx; mod_idx <= sh_idx; mod_idx++) {
      SH_NEXT_IDX(mod_idx)		= ML_SH_IDX(SH_NEXT_IDX(mod_idx));
      SH_PREV_IDX(mod_idx)		= ML_SH_IDX(SH_PREV_IDX(mod_idx));
      SH_IR_IDX(mod_idx)		= ML_IR_IDX(SH_IR_IDX(mod_idx));

      if (SH_STMT_TYPE(mod_idx) != Statement_Num_Stmt) {
         SH_PARENT_BLK_IDX(mod_idx)	= ML_SH_IDX(SH_PARENT_BLK_IDX(mod_idx));
      }
   }

   /* Need SN_NAME_LEN because of renames situations. */

   /* for (mod_idx = (only_update_new_tbl_entries) ? (sn_idx+1) : 1;  */
   for (mod_idx = 1; mod_idx <= sn_idx; mod_idx++) {
      SN_ATTR_IDX(mod_idx)	= ML_AT_IDX(SN_ATTR_IDX(mod_idx));
      SN_NAME_IDX(mod_idx)	= AT_NAME_IDX(SN_ATTR_IDX(mod_idx));
      SN_NAME_LEN(mod_idx)	= AT_NAME_LEN(SN_ATTR_IDX(mod_idx));
      SN_SIBLING_LINK(mod_idx)	= ML_SN_IDX(SN_SIBLING_LINK(mod_idx));
   }

   start_idx = sn_idx + 1;

   for (mod_idx = start_idx; mod_idx <= sec_name_tbl_idx; mod_idx++) {

      if (ML_SN_KEEP_ME(mod_idx)) {
         sec_name_tbl[++sn_idx]		= sec_name_tbl[mod_idx];
      }
   }

   for (mod_idx = start_idx; mod_idx <= sec_name_tbl_idx; mod_idx++) {
      SN_ATTR_IDX(mod_idx)		= ML_AT_IDX(SN_ATTR_IDX(mod_idx));
      SN_NAME_IDX(mod_idx)		= AT_NAME_IDX(SN_ATTR_IDX(mod_idx));
      SN_NAME_LEN(mod_idx)		= AT_NAME_LEN(SN_ATTR_IDX(mod_idx));
      SN_SIBLING_LINK(mod_idx)		= ML_SN_IDX(SN_SIBLING_LINK(mod_idx));
   }

   attr_tbl_idx		= at_idx;
   attr_aux_tbl_idx	= at_idx;
   bounds_tbl_idx	= bd_idx;
   const_tbl_idx	= cn_idx;
   const_pool_idx	= cp_idx;
   loc_name_tbl_idx	= ln_idx;
   ir_list_tbl_idx	= il_idx;
   ir_tbl_idx		= ir_idx;
   name_pool_idx	= np_idx;
   stor_blk_tbl_idx	= sb_idx;
   sec_name_tbl_idx	= sn_idx;
   sh_tbl_idx		= sh_idx;

   /* If this is a partial compression, the only entries added to the */
   /* attr_list_tbl must point to new attributes coming in during USE */
   /* processing.                                                     */

   for (mod_idx = al_idx+1; mod_idx <= attr_list_tbl_idx; mod_idx++) {
      
# if defined(_DEBUG)
     end_idx	= ML_AT_IDX(AL_ATTR_IDX(mod_idx));

     if (!AL_FREE(mod_idx) &&
          AL_ATTR_IDX(mod_idx) != NULL_IDX &&
         !ML_AT_KEEP_ME(AL_ATTR_IDX(mod_idx)) &&
         !ML_AT_COMPRESSED_IDX(AL_ATTR_IDX(mod_idx))) {


        /* This attr is not being kept.  It should have been cleared. */

        PRINTMSG(stmt_start_line, 1321, Internal, 0, mod_idx, 
                 AL_ATTR_IDX(mod_idx));
     }

# endif

      AL_ATTR_IDX(mod_idx) = ML_AT_IDX(AL_ATTR_IDX(mod_idx));
   }

   mod_idx = SCP_HN_FW_IDX(curr_scp_idx) + 1;
   end_idx = SCP_HN_LW_IDX(curr_scp_idx);

   while (mod_idx < end_idx) {

      if (!ML_AT_KEEP_ME(HN_ATTR_IDX(mod_idx))) {

         if (!ML_AT_COMPRESSED_IDX(HN_ATTR_IDX(mod_idx))) {
            remove_hidden_name_ntry(mod_idx);
            end_idx = SCP_HN_LW_IDX(curr_scp_idx);
         }
      }
      mod_idx++;
   }


   for (mod_idx = SCP_HN_FW_IDX(curr_scp_idx) + 1;
        mod_idx < SCP_HN_LW_IDX(curr_scp_idx); mod_idx++) {
      HN_ATTR_IDX(mod_idx)	= ML_AT_IDX(HN_ATTR_IDX(mod_idx));
      HN_NAME_IDX(mod_idx)	= ML_NP_IDX(HN_NAME_IDX(mod_idx));
   }

   /* This updates the scp attr index, but be careful, because        */
   /* everything else in the scope table could be bad pointers.       */

   SCP_ATTR_IDX(curr_scp_idx) = ML_AT_IDX(SCP_ATTR_IDX(curr_scp_idx));

   TRACE (Func_Exit, "compress_tbls", NULL);

   return;

}   /* compress_tbls */

/******************************************************************************\
|*									      *|
|* Description:								      *|
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
static	void  update_idxs_in_attr_entry(int	start_idx,
					int	end_idx)
{
   int		at_idx;


   TRACE (Func_Entry, "update_idxs_in_attr_entry", NULL);

   for (at_idx	= start_idx; at_idx <= end_idx; at_idx++) {

   if (!AT_IGNORE_ATTR_LINK(at_idx)) {
       AT_ATTR_LINK(at_idx)	= ML_AT_IDX(AT_ATTR_LINK(at_idx));
   }

   AT_NAME_IDX(at_idx)		= ML_NP_IDX(AT_NAME_IDX(at_idx));
   AT_ORIG_NAME_IDX(at_idx)	= ML_NP_IDX(AT_ORIG_NAME_IDX(at_idx));
   AT_MODULE_IDX(at_idx)	= ML_AT_IDX(AT_MODULE_IDX(at_idx));

   switch (AT_OBJ_CLASS(at_idx)) {
   case Data_Obj:
      ATD_ARRAY_IDX(at_idx)	   = ML_BD_IDX(ATD_ARRAY_IDX(at_idx));
      ATD_DISTRIBUTION_IDX(at_idx) = ML_BD_IDX(ATD_DISTRIBUTION_IDX(at_idx));
      ATD_STOR_BLK_IDX(at_idx)	   = ML_SB_IDX(ATD_STOR_BLK_IDX(at_idx));
      ATD_RESHAPE_ARRAY_IDX(at_idx)= ML_BD_IDX(ATD_RESHAPE_ARRAY_IDX(at_idx));

# if defined(_F_MINUS_MINUS)
      ATD_PE_ARRAY_IDX(at_idx)	   = ML_BD_IDX(ATD_PE_ARRAY_IDX(at_idx));
# endif

      switch (ATD_CLASS(at_idx)) {
      case Function_Result:

         if (ATD_OFFSET_ASSIGNED(at_idx)) {

            switch (ATD_OFFSET_FLD(at_idx)) {
            case AT_Tbl_Idx:
               ATD_OFFSET_IDX(at_idx) = ML_AT_IDX(ATD_OFFSET_IDX(at_idx));
               break;

            case CN_Tbl_Idx:
               ATD_OFFSET_IDX(at_idx) = ML_CN_IDX(ATD_OFFSET_IDX(at_idx));
               break;

            case IR_Tbl_Idx:
               ATD_OFFSET_IDX(at_idx) = ML_IR_IDX(ATD_OFFSET_IDX(at_idx));
               break;

            case IL_Tbl_Idx:
               ATD_OFFSET_IDX(at_idx) = ML_IL_IDX(ATD_OFFSET_IDX(at_idx));
               break;
            }
         }

         ATD_TYPE_IDX(at_idx)	= ML_TYP_IDX(ATD_TYPE_IDX(at_idx));
         ATD_FUNC_IDX(at_idx)	= ML_AT_IDX(ATD_FUNC_IDX(at_idx));
         break;


      case Constant:

         ATD_TYPE_IDX(at_idx)	= ML_TYP_IDX(ATD_TYPE_IDX(at_idx));

         switch (ATD_FLD(at_idx)) {
         case AT_Tbl_Idx:
            ATD_CONST_IDX(at_idx) = ML_AT_IDX(ATD_CONST_IDX(at_idx));
            break;
 
         default:
            ATD_CONST_IDX(at_idx) = ML_CN_IDX(ATD_CONST_IDX(at_idx));
            break;
         }
         break;


      case CRI__Pointee:
         ATD_TYPE_IDX(at_idx)	= ML_TYP_IDX(ATD_TYPE_IDX(at_idx));
         ATD_PTR_IDX(at_idx)	= ML_AT_IDX(ATD_PTR_IDX(at_idx));
         break;

      case Compiler_Tmp:

         ATD_NEXT_MEMBER_IDX(at_idx)  = ML_AT_IDX(ATD_NEXT_MEMBER_IDX(at_idx));
         ATD_DEFINING_ATTR_IDX(at_idx)=ML_AT_IDX(ATD_DEFINING_ATTR_IDX(at_idx));

         if (ATD_AUTOMATIC(at_idx)) {
            ATD_AUTO_BASE_IDX(at_idx) = ML_AT_IDX(ATD_AUTO_BASE_IDX(at_idx));
         }
         else if (ATD_OFFSET_ASSIGNED(at_idx)) {

            switch (ATD_OFFSET_FLD(at_idx)) {
            case AT_Tbl_Idx:
               ATD_OFFSET_IDX(at_idx) = ML_AT_IDX(ATD_OFFSET_IDX(at_idx));
               break;

            case CN_Tbl_Idx:
               ATD_OFFSET_IDX(at_idx) = ML_CN_IDX(ATD_OFFSET_IDX(at_idx));
               break;

            case IR_Tbl_Idx:
               ATD_OFFSET_IDX(at_idx) = ML_IR_IDX(ATD_OFFSET_IDX(at_idx));
               break;

            case IL_Tbl_Idx:
               ATD_OFFSET_IDX(at_idx) = ML_IL_IDX(ATD_OFFSET_IDX(at_idx));
               break;
            }
         }

         ATD_TYPE_IDX(at_idx)	= ML_TYP_IDX(ATD_TYPE_IDX(at_idx));

         switch (ATD_FLD(at_idx)) {
         case CN_Tbl_Idx:
            ATD_TMP_IDX(at_idx)   = ML_CN_IDX(ATD_TMP_IDX(at_idx));
            break;

         case AT_Tbl_Idx:
            ATD_TMP_IDX(at_idx)   = ML_AT_IDX(ATD_TMP_IDX(at_idx));
            break;

         case IR_Tbl_Idx:
            ATD_TMP_IDX(at_idx)   = ML_IR_IDX(ATD_TMP_IDX(at_idx));
            break;

         case IL_Tbl_Idx:
            ATD_TMP_IDX(at_idx)   = ML_IL_IDX(ATD_TMP_IDX(at_idx));
            break;
         }
         break;


      case Dummy_Argument:

         if (!ATD_INTRIN_DARG(at_idx)) {
            ATD_TYPE_IDX(at_idx)	= ML_TYP_IDX(ATD_TYPE_IDX(at_idx));
         }
         break;


      case Struct_Component:

         switch (ATD_OFFSET_FLD(at_idx)) {
         case AT_Tbl_Idx:
            ATD_CPNT_OFFSET_IDX(at_idx) =ML_AT_IDX(ATD_CPNT_OFFSET_IDX(at_idx));
            break;

         case CN_Tbl_Idx:
            ATD_CPNT_OFFSET_IDX(at_idx) =ML_CN_IDX(ATD_CPNT_OFFSET_IDX(at_idx));
            break;

         case IR_Tbl_Idx:
            ATD_CPNT_OFFSET_IDX(at_idx) =ML_IR_IDX(ATD_CPNT_OFFSET_IDX(at_idx));
            break;

         case IL_Tbl_Idx:
            ATD_CPNT_OFFSET_IDX(at_idx) =ML_IL_IDX(ATD_CPNT_OFFSET_IDX(at_idx));
            break;
         }

         switch (ATD_FLD(at_idx)) {
         case AT_Tbl_Idx:
            ATD_CPNT_INIT_IDX(at_idx)	= ML_AT_IDX(ATD_CPNT_INIT_IDX(at_idx));
            break;

         case CN_Tbl_Idx:
            ATD_CPNT_INIT_IDX(at_idx)	= ML_CN_IDX(ATD_CPNT_INIT_IDX(at_idx));
            break;

         case IR_Tbl_Idx:
            ATD_CPNT_INIT_IDX(at_idx)	= ML_IR_IDX(ATD_CPNT_INIT_IDX(at_idx));
            break;

         case IL_Tbl_Idx:
            ATD_CPNT_INIT_IDX(at_idx)	= ML_IL_IDX(ATD_CPNT_INIT_IDX(at_idx));
            break;
         }
         ATD_TYPE_IDX(at_idx)	      = ML_TYP_IDX(ATD_TYPE_IDX(at_idx));
         ATD_DERIVED_TYPE_IDX(at_idx) = ML_AT_IDX(ATD_DERIVED_TYPE_IDX(at_idx));
         break;


      case Variable:

         switch (ATD_FLD(at_idx)) {
         case AT_Tbl_Idx:
            ATD_VARIABLE_TMP_IDX(at_idx) =
                         ML_AT_IDX(ATD_VARIABLE_TMP_IDX(at_idx));
            break;

         case IL_Tbl_Idx:
            ATD_VARIABLE_TMP_IDX(at_idx) =
                         ML_IL_IDX(ATD_VARIABLE_TMP_IDX(at_idx));
            break;
         }

         ATD_ASSIGN_TMP_IDX(at_idx)   = ML_AT_IDX(ATD_ASSIGN_TMP_IDX(at_idx));
         ATD_NEXT_MEMBER_IDX(at_idx)  = ML_AT_IDX(ATD_NEXT_MEMBER_IDX(at_idx));

         if (ATD_AUTOMATIC(at_idx)) {
            ATD_AUTO_BASE_IDX(at_idx) = ML_AT_IDX(ATD_AUTO_BASE_IDX(at_idx));
         }
         else if (ATD_OFFSET_ASSIGNED(at_idx)) {

            switch (ATD_OFFSET_FLD(at_idx)) {
            case AT_Tbl_Idx:
               ATD_OFFSET_IDX(at_idx) = ML_AT_IDX(ATD_OFFSET_IDX(at_idx));
               break;

            case CN_Tbl_Idx:
               ATD_OFFSET_IDX(at_idx) = ML_CN_IDX(ATD_OFFSET_IDX(at_idx));
               break;

            case IR_Tbl_Idx:
               ATD_OFFSET_IDX(at_idx) = ML_IR_IDX(ATD_OFFSET_IDX(at_idx));
               break;

            case IL_Tbl_Idx:
               ATD_OFFSET_IDX(at_idx) = ML_IL_IDX(ATD_OFFSET_IDX(at_idx));
               break;
            }
         }

         /* Intentional fall through */

      default:
         ATD_TYPE_IDX(at_idx)	= ML_TYP_IDX(ATD_TYPE_IDX(at_idx));
         break;

      }  /* End switch */
      break;

   case Pgm_Unit:

      if (ATP_PGM_UNIT(at_idx) == Module) {
         ATP_MOD_PATH_IDX(at_idx) = ML_NP_IDX(ATP_MOD_PATH_IDX(at_idx));
      }
      else {
         ATP_RSLT_IDX(at_idx)	= ML_AT_IDX(ATP_RSLT_IDX(at_idx));
         ATP_FIRST_IDX(at_idx)	= ML_SN_IDX(ATP_FIRST_IDX(at_idx));

         if (ATP_PROC(at_idx) != Intrin_Proc && ATP_PROC(at_idx) != Dummy_Proc){
            ATP_FIRST_SH_IDX(at_idx)= ML_SH_IDX(ATP_FIRST_SH_IDX(at_idx));
            ATP_PARENT_IDX(at_idx)  = ML_AT_IDX(ATP_PARENT_IDX(at_idx));
         }
      }

      ATP_EXT_NAME_IDX(at_idx)	= ML_NP_IDX(ATP_EXT_NAME_IDX(at_idx));
      break;

   case Label:

      ATL_NEXT_ASG_LBL_IDX(at_idx) = ML_AT_IDX(ATL_NEXT_ASG_LBL_IDX(at_idx));

      if (AT_DEFINED(at_idx)) {
         ATL_DEF_STMT_IDX(at_idx)  = ML_SH_IDX(ATL_DEF_STMT_IDX(at_idx));
      }

      if (ATL_CLASS(at_idx) == Lbl_Format) {
         ATL_PP_FORMAT_TMP(at_idx) = ML_AT_IDX(ATL_PP_FORMAT_TMP(at_idx));
         ATL_FORMAT_TMP(at_idx)    = ML_AT_IDX(ATL_FORMAT_TMP(at_idx));
      }
      else {
         ATL_DIRECTIVE_LIST(at_idx)= ML_IL_IDX(ATL_DIRECTIVE_LIST(at_idx));

         if (ATL_CLASS(at_idx) == Lbl_User) {
            ATL_BLK_STMT_IDX(at_idx)  = ML_SH_IDX(ATL_BLK_STMT_IDX(at_idx));
         }
      }
      break;


   case Derived_Type:

      if (ATT_STRUCT_BIT_LEN_IDX(at_idx) != NULL_IDX) {

         switch (ATT_STRUCT_BIT_LEN_FLD(at_idx)) {
         case CN_Tbl_Idx:
            ATT_STRUCT_BIT_LEN_IDX(at_idx) = 
                                 ML_CN_IDX(ATT_STRUCT_BIT_LEN_IDX(at_idx));
            break;

         case AT_Tbl_Idx:
            ATT_STRUCT_BIT_LEN_IDX(at_idx) = 
                                 ML_AT_IDX(ATT_STRUCT_BIT_LEN_IDX(at_idx));
            break;

         case IL_Tbl_Idx:
            ATT_STRUCT_BIT_LEN_IDX(at_idx) = 
                                 ML_IL_IDX(ATT_STRUCT_BIT_LEN_IDX(at_idx));
            break;

         case IR_Tbl_Idx:
            ATT_STRUCT_BIT_LEN_IDX(at_idx) = 
                                 ML_IR_IDX(ATT_STRUCT_BIT_LEN_IDX(at_idx));
            break;

         case NO_Tbl_Idx:
            ATT_STRUCT_BIT_LEN_FLD(at_idx) = CN_Tbl_Idx;
            ATT_STRUCT_BIT_LEN_IDX(at_idx) = 
                                 ML_CN_IDX(ATT_STRUCT_BIT_LEN_IDX(at_idx));
            break;
         }
      }

      ATT_FIRST_CPNT_IDX(at_idx)	= ML_SN_IDX(ATT_FIRST_CPNT_IDX(at_idx));
      break;

   case Interface:

      ATI_FIRST_SPECIFIC_IDX(at_idx) =ML_SN_IDX(ATI_FIRST_SPECIFIC_IDX(at_idx));
      ATI_PROC_IDX(at_idx)		= ML_AT_IDX(ATI_PROC_IDX(at_idx));
      ATD_TYPE_IDX(at_idx)		= ML_TYP_IDX(ATD_TYPE_IDX(at_idx));
      break;

   case Namelist_Grp:

      ATN_FIRST_NAMELIST_IDX(at_idx) =ML_SN_IDX(ATN_FIRST_NAMELIST_IDX(at_idx));
      ATN_LAST_NAMELIST_IDX(at_idx)  = ML_SN_IDX(ATN_LAST_NAMELIST_IDX(at_idx));

      if (ATN_NAMELIST_DESC(at_idx) != NULL_IDX) {
         ATN_NAMELIST_DESC(at_idx)   = ML_AT_IDX(ATN_NAMELIST_DESC(at_idx));
      }
      break;

   case Stmt_Func:

      ATD_TYPE_IDX(at_idx) = ML_TYP_IDX(ATD_TYPE_IDX(at_idx));
      ATP_FIRST_IDX(at_idx) = ML_SN_IDX(ATP_FIRST_IDX(at_idx));

      switch (ATS_SF_FLD(at_idx)) {
      case CN_Tbl_Idx:
         ATS_SF_IDX(at_idx) = ML_CN_IDX(ATS_SF_IDX(at_idx));
         break;

      case AT_Tbl_Idx:
         ATS_SF_IDX(at_idx) = ML_AT_IDX(ATS_SF_IDX(at_idx));
         break;

      case IR_Tbl_Idx:
         ATS_SF_IDX(at_idx) = ML_IR_IDX(ATS_SF_IDX(at_idx));
         break;

      case IL_Tbl_Idx:
         ATS_SF_IDX(at_idx) = ML_IL_IDX(ATS_SF_IDX(at_idx));
         break;
      }
      break;
   }  /* End switch */
   }  /* End For */

   TRACE (Func_Exit, "update_idxs_in_attr_entry", NULL);

   return;

}   /* update_idxs_in_attr_entry */

/******************************************************************************\
|*									      *|
|* Description:								      *|
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
extern	void  output_mod_info_file(void)

{
   int		 al_idx;
   int		 idx;
   int		 module_attr_idx;
   FILE		*mod_file_ptr		= NULL;
   long		*mod_idx;
   int		 name_idx;
   int		 sb_idx;
   int		 wd_len;


   TRACE (Func_Entry, "output_mod_info_file", NULL);

   module_attr_idx	= SCP_ATTR_IDX(curr_scp_idx);

   if (dump_flags.preinline) {  /* Append these files */
      mod_file_ptr = fopen(FP_NAME_PTR(SCP_FILE_PATH_IDX(curr_scp_idx)), "ab");
   }
   else if (on_off_flags.module_to_mod) {
      mod_file_ptr = fopen(FP_NAME_PTR(SCP_FILE_PATH_IDX(curr_scp_idx)), "wb");
   }
   else {

#     if defined(_MODULE_TO_DOT_M)      /* These are all appended */
         mod_file_ptr = fopen(FP_NAME_PTR(SCP_FILE_PATH_IDX(curr_scp_idx)),
                              "ab");
#     else
         mod_file_ptr = fopen(FP_NAME_PTR(SCP_FILE_PATH_IDX(curr_scp_idx)),
                              "wb");
#     endif
   }

#ifdef KEY /* Bug 3474 */
   Uint save_file_idx = SCP_FILE_PATH_IDX(curr_scp_idx);
#endif /* KEY Bug 3474 */
   SCP_FILE_PATH_IDX(curr_scp_idx) = NULL_IDX;

   if (ATP_PGM_UNIT(module_attr_idx) == Module) {
      ATP_MOD_PATH_IDX(module_attr_idx)	= NULL_IDX;
      ATP_MOD_PATH_LEN(module_attr_idx)	= 0;
   }

   if (mod_file_ptr == NULL) {
#ifdef KEY /* Bug 3474 */
      PRINTMSG(AT_DEF_LINE(module_attr_idx), 1665, Error,
               AT_DEF_COLUMN(module_attr_idx),
               AT_OBJ_NAME_PTR(module_attr_idx),
               FP_NAME_PTR(save_file_idx),
	       strerror(errno));
#else
      PRINTMSG(AT_DEF_LINE(module_attr_idx), 1665, Error,
               AT_DEF_COLUMN(module_attr_idx),
               AT_OBJ_NAME_PTR(module_attr_idx),
               FP_NAME_PTR(SCP_FILE_PATH_IDX(curr_scp_idx)));
#endif /* KEY Bug 3474 */
      goto EXIT;
   }

   MD_PDT_HDR_TYPE		= COMPILER_INFO_TABLE_TYPE;
   MD_VERSION_NUM		= MD_CURRENT_VERSION;
   MD_TARGET			= target_os;
   MD_ENABLE_DOUBLE_PRECISION	= on_off_flags.enable_double_precision;
   MD_DEFAULT_INTEGER_TYPE	= INTEGER_DEFAULT_TYPE;
   MD_HAS_ERRORS		= (num_prog_unit_errors > 0);
   MD_DALIGN			= cmd_line_flags.dalign;
   MD_CF77TYPES			= cmd_line_flags.s_cf77types;
   MD_DEFAULT32			= cmd_line_flags.s_default32;
   MD_DEFAULT64			= cmd_line_flags.s_default64;
   MD_FLOAT64			= cmd_line_flags.s_float64;

   MD_NEW_CONST_TBL		= TRUE;

   /* At this point, if we're outputing for inlining we need to check for   */
   /* alternate entries.  If they exist, an md_header_descriptor is written */
   /* out for each.  The actual tables for the alternate entry follow in    */
   /* main entry and can can be found by reading the 026 table until the    */
   /* next main entry is found.                                             */

   if (ATP_MAY_INLINE(SCP_ATTR_IDX(MAIN_SCP_IDX)) && 
       SCP_ENTRY_IDX(MAIN_SCP_IDX) != NULL_IDX) {
      al_idx		= SCP_ENTRY_IDX(MAIN_SCP_IDX);
      MD_ALTERNATE_ENTRY= TRUE;
      MD_PDT_HDR_LEN	= sizeof(mit_header_type)/TARGET_BYTES_PER_WORD;

# if defined(_HOST32) && defined(_TARGET64)

      /* PDT size must be in 64 bit increment sizes. */

      MD_PDT_HDR_LEN = (MD_PDT_HDR_LEN + 1) / 2;
# endif

      while (al_idx != NULL_IDX) {
         name_idx	= AT_NAME_IDX(AL_ATTR_IDX(al_idx));
         MD_NAME_LEN	= AT_NAME_LEN(AL_ATTR_IDX(al_idx));
         wd_len		= WORD_LEN(MD_NAME_LEN);
         mod_idx	= MD_NAME_LONG;

         for (idx = 0; idx < wd_len; idx++) {
            *mod_idx	= name_pool[name_idx].name_long;
            name_idx++;
            mod_idx++;
         }

         fwrite(&mit_header, sizeof(mit_header_type), 1, mod_file_ptr);
         al_idx		= AL_NEXT_IDX(al_idx);
      }

      /* Reset the original name in the header. */

      name_idx		= AT_NAME_IDX(module_attr_idx);
      MD_NAME_LEN	= AT_NAME_LEN(module_attr_idx);
      wd_len		= WORD_LEN(MD_NAME_LEN);
      mod_idx		= MD_NAME_LONG;

      for (idx = 0; idx < wd_len; idx++) {
         *mod_idx	= name_pool[name_idx].name_long;
         name_idx++;
         mod_idx++;
      }
   }

   MD_ALTERNATE_ENTRY		= FALSE;

   if (dump_flags.preinline && num_prog_unit_errors > 0) {

      /* Do not write out any tables.  Just a header with error set. */

      attr_tbl_idx	= NULL_IDX;
      attr_aux_tbl_idx	= NULL_IDX;
      bounds_tbl_idx	= NULL_IDX;
      const_tbl_idx	= NULL_IDX;
      const_pool_idx	= NULL_IDX;
      ir_tbl_idx	= NULL_IDX;
      ir_list_tbl_idx	= NULL_IDX;
      loc_name_tbl_idx	= NULL_IDX;
      name_pool_idx	= NULL_IDX;
      sec_name_tbl_idx	= NULL_IDX;
      stor_blk_tbl_idx	= NULL_IDX;
      type_tbl_idx	= NULL_IDX;
      sh_tbl_idx	= NULL_IDX;
   }
   else {

      /* Do not write out any tables.  Just the mod header. */

      /* In error situations, module_attr_idx may not be in the local name    */
      /* table so it has to be specifically included for output.  This module */
      /* output is only used to finish this compilation because of errors.    */

      ML_AT_IDX(module_attr_idx)	= module_attr_idx;

      /* assign_new_idxs needs to have ML_AT_IDX(0) = NULL_IDX, ML_BD_IDX(0)  */
      /* = NULL_IDX -  ect..  The zeroth entry of the mod link table contains */
      /* the index to start compression at for each table.  When compressing  */
      /* tables for module output, everything gets compressed, so the zeroth  */
      /* entry should be all zeros to signify that everything gets compressed.*/
      /* The zeroth entry is set to all NULL_IDX's when the table is allocated*/

      /* Resolve duplicate attr idxs in mod link table.         */

      save_const_pool_idx	= NULL_IDX;
      save_const_tbl_idx	= NULL_IDX;

      /* If we're not creating a preinline file and if MODINLINE is not on    */
      /* for this module, then we will not be writing out the SH table, so    */
      /* set ML_SH_IDX equal to sh_tbl_idx, so we do nothing in assign_new..  */
      /* Then clear sh_tbl_idx and ML_SH_IDX so we do nothing in compress_..  */

      if (!ATP_MAY_INLINE(SCP_ATTR_IDX(MAIN_SCP_IDX))) {
         ML_SH_IDX(0)	= sh_tbl_idx;  /* SH table not needed.  */
         assign_new_idxs(TRUE);
         ML_SH_IDX(0)	= NULL_IDX;
         sh_tbl_idx	= NULL_IDX;
      }
      else {
         assign_new_idxs(TRUE);
      }

      /* Do table compression, but do not update the attribute entries in the */
      /* attr_list_tbl.  Stop updating from happening, by passing the last    */
      /* used index in attr_list_tbl.  compress_tbls goes through the attr    */
      /* list table starting at the entry past the entry passed in.           */

      num_module_derived_types	= 0;  /* Not used - clear to prevent overflow */
      count_derived_types	= FALSE;
      compress_tbls(attr_list_tbl_idx, FALSE); 

      /* module_attr_idx may have been moved during compression. */

      module_attr_idx		= ML_AT_IDX(module_attr_idx);

      /* Certain flds in the attr table need to be cleared, such as line      */
      /* numbers cif ids, the referenced flag ect..                           */
      /* Also, if alternate entries, reset ATP_FIRST_SH_IDX to the main       */
      /* entry's ATP_FIRST                                                    */

      set_attr_flds_for_output();

      /* Any bounds table free list is destroyed. */

      BD_NEXT_FREE_NTRY(BD_FREE_LIST_IDX)	= NULL_IDX;

      /* Set ATP_SCP_ALIVE for the module attr, so that it can be recognized  */
      /* when the module is read in again.  ATP_SCP_ALIVE is turned off for   */
      /* everything after the semantic pass in s_driver.                      */

      ATP_SCP_ALIVE(module_attr_idx)	= TRUE;

      for (sb_idx	= 1; sb_idx <= stor_blk_tbl_idx; sb_idx++) {
         SB_CIF_SYMBOL_ID(sb_idx)    = NULL_IDX;
      }
   }

   /* Note on table sizes.  tbl_idx is the last used item in the table.       */
   /* The 0th entry should not be written out to the module info table,       */
   /* so using tbl_idx as the size of the table to write out is correct.      */
   /* That is also why when the tables are written they start at [1].         */

   MD_TBL_TYPE(Attr_Tbl)	= Attr_Tbl;
   MD_NUM_ENTRIES(Attr_Tbl)	= attr_tbl_idx;
   MD_TBL_TYPE(Bounds_Tbl)	= Bounds_Tbl;
   MD_NUM_ENTRIES(Bounds_Tbl)	= bounds_tbl_idx;
   MD_TBL_TYPE(Const_Tbl)	= Const_Tbl;
   MD_NUM_ENTRIES(Const_Tbl)	= const_tbl_idx;
   MD_TBL_TYPE(Const_Pool)	= Const_Pool;
   MD_NUM_ENTRIES(Const_Pool)	= const_pool_idx;
   MD_TBL_TYPE(Ir_Tbl)		= Ir_Tbl;
   MD_NUM_ENTRIES(Ir_Tbl)	= ir_tbl_idx;
   MD_TBL_TYPE(Ir_List_Tbl)	= Ir_List_Tbl;
   MD_NUM_ENTRIES(Ir_List_Tbl)	= ir_list_tbl_idx;
   MD_TBL_TYPE(Loc_Name_Tbl)	= Loc_Name_Tbl;
   MD_NUM_ENTRIES(Loc_Name_Tbl)	= loc_name_tbl_idx;
   MD_TBL_TYPE(Name_Pool)	= Name_Pool;
   MD_NUM_ENTRIES(Name_Pool)	= name_pool_idx;
   MD_TBL_TYPE(Sec_Name_Tbl)	= Sec_Name_Tbl;
   MD_NUM_ENTRIES(Sec_Name_Tbl)	= sec_name_tbl_idx;
   MD_TBL_TYPE(Stor_Blk_Tbl)	= Stor_Blk_Tbl;
   MD_NUM_ENTRIES(Stor_Blk_Tbl)	= stor_blk_tbl_idx;
   MD_TBL_TYPE(Type_Tbl)	= Type_Tbl;
   MD_NUM_ENTRIES(Type_Tbl)	= type_tbl_idx;
   MD_TBL_TYPE(Sh_Tbl)		= Sh_Tbl;
   MD_NUM_ENTRIES(Sh_Tbl)	= sh_tbl_idx;

   MD_PDT_HDR_LEN		= (attr_tbl_idx * NUM_AT_WDS) +
       				  (bounds_tbl_idx * NUM_BD_WDS) +
       				  (const_tbl_idx * NUM_CN_WDS) +
       				  (const_pool_idx * NUM_CP_WDS) +
      				  (ir_list_tbl_idx * NUM_IL_WDS) +
      				  (ir_tbl_idx * NUM_IR_WDS) +
      				  (loc_name_tbl_idx * NUM_LN_WDS) +
      				  (name_pool_idx * NUM_NP_WDS) +
      				  (sec_name_tbl_idx * NUM_SN_WDS) +
      				  (stor_blk_tbl_idx * NUM_SB_WDS) +
       				  (type_tbl_idx * NUM_TYP_WDS) +
       				  (sh_tbl_idx * NUM_SH_WDS) +
      				  (MD_TBL_BYTE_SIZE/TARGET_BYTES_PER_WORD) +
      				  ((sizeof(mit_descriptor_type) / 
                                      TARGET_BYTES_PER_WORD) * Num_Of_Tbls);

/* KAY */

   /* PDT size must be in 64 bit increment sizes. */

# if defined(_HOST32) && defined(_TARGET64)
   MD_PDT_HDR_LEN = (MD_PDT_HDR_LEN + 1) / 2;
# endif

   fwrite(&mit_header, sizeof(mit_header_type), 1, mod_file_ptr);

   fwrite(&mit_descriptor[1], 
          sizeof(mit_descriptor_type),
          Num_Of_Tbls,
          mod_file_ptr);

   OUTPUT_TBL_TO_MODULE(mod_file_ptr, module_attr_idx, name_pool);
   OUTPUT_TBL_TO_MODULE(mod_file_ptr, module_attr_idx, loc_name_tbl);
   OUTPUT_TBL_TO_MODULE(mod_file_ptr, module_attr_idx, attr_tbl);
   OUTPUT_TBL_TO_MODULE(mod_file_ptr, module_attr_idx, bounds_tbl);
   OUTPUT_TBL_TO_MODULE(mod_file_ptr, module_attr_idx, const_tbl);
   OUTPUT_TBL_TO_MODULE(mod_file_ptr, module_attr_idx, const_pool);
   OUTPUT_TBL_TO_MODULE(mod_file_ptr, module_attr_idx, sec_name_tbl);
   OUTPUT_TBL_TO_MODULE(mod_file_ptr, module_attr_idx, stor_blk_tbl);
   OUTPUT_TBL_TO_MODULE(mod_file_ptr, module_attr_idx, type_tbl);
   OUTPUT_TBL_TO_MODULE(mod_file_ptr, module_attr_idx, ir_tbl);
   OUTPUT_TBL_TO_MODULE(mod_file_ptr, module_attr_idx, ir_list_tbl);
   OUTPUT_TBL_TO_MODULE(mod_file_ptr, module_attr_idx, sh_tbl);

   fflush(mod_file_ptr);
   fclose(mod_file_ptr);

   const_tbl_idx++;  /* Adjust so it is set to next available index */

EXIT:

   TBL_FREE(mod_link_tbl);

   TRACE (Func_Exit, "output_mod_info_file", NULL);

   return;

}  /* output_mod_info_file */

#ifdef KEY /* Bug 5089 */
/*
 * module_attr_idx	AT_Tbl_Idx for a module
 * return		TRUE if that module is intrinsic ieee_features,
 *			ieee_exceptions, or ieee_arithmetic
 */
static boolean
is_ieee(int module_attr_idx) {
  extern boolean LANG_IEEE_Save;
  return AT_IS_INTRIN(module_attr_idx) &&
    LANG_IEEE_Save &&
    0 == strncmp(AT_OBJ_NAME_PTR(module_attr_idx), "IEEE", 4);
}
#endif /* KEY Bug 5089 */
/******************************************************************************\
|*									      *|
|* Description:								      *|
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
void	use_stmt_semantics(void)
				
{
   int		 al_idx;
   int		 attr_idx;
   int		 attr_list_free_list;
   int		 bd_idx;
   int		 host_attr_idx;
   int		 host_name_idx;
   int		 interface_list;
   int		 ln_idx;  
   int		 match;
   int		 module_attr_idx;
   int		 module_list_idx;
   int		 name_idx;
   int		 new_name_idx;
   int		 new_sn_idx;
   int		 save_attr_list_tbl_idx;
   int		 srch_attr_idx;
   int		 start_ln_idx;
   boolean	 use_only;
   int		 use_ir_idx;


   TRACE (Func_Entry, "use_stmt_semantics", NULL);

   /* global flag used to tell set_mod_link_tbl_for_attr */
   /* that it should check all attrs for duplicates.     */
   /* This refers to search_for_duplicate_attrs          */

   list_of_modules_in_module	= NULL_IDX;
   module_list_idx   		= SCP_USED_MODULE_LIST(curr_scp_idx);
   attr_list_free_list		= AL_NEXT_IDX(NULL_IDX);
   interface_list		= NULL_IDX;

   keep_module_procs = (opt_flags.inline_lvl > Inline_Lvl_0) ||
                        ATP_MAY_INLINE(SCP_ATTR_IDX(MAIN_SCP_IDX));

   /* Find the last module on the list.  This is really the first module   */
   /* specified on a USE list.  This way we get the ordering correct, plus */
   /* since we are backing up the list and the list is extended at the     */
   /* bottom, we don't end up trying to process the newly added indirectly */
   /* referenced modules.  The list is extended in resolve_used_modules.   */
   /* All modules that are indirectly brought in during USE association    */
   /* are added to this list.  This helps get messages issued correctly    */
   /* and keeps CIF happy.  See resolve_used_modules for more details.     */
   /* All the modules specified on the USE statement are specifed on this  */
   /* list first, because these are the attr indexes we want to use.       */

   while (AL_NEXT_IDX(module_list_idx) != NULL_IDX) {
      module_list_idx	= AL_NEXT_IDX(module_list_idx);
   }

   while (module_list_idx != NULL_IDX) {
      module_attr_idx			= AL_ATTR_IDX(module_list_idx);
      only_update_new_tbl_entries	= TRUE;

      /* For next iteration */

      module_list_idx	= AL_PREV_MODULE_IDX(module_list_idx); 

      if (ATP_IMPLICIT_USE_MODULE(module_attr_idx)) {

         /* Need to generate the Use_Opr - Have a curr_stmt_sh - use it */

        /* Generate IR for this USE statement.  Need to keep the attr so that */
        /* it can be passed thru the PDGCS interface during IR conversion.    */
        /* Do not need pass2 semantics for this statement.                    */

        NTR_IR_TBL(use_ir_idx);
        IR_OPR(use_ir_idx)		= Use_Opr;
        IR_TYPE_IDX(use_ir_idx)		= TYPELESS_DEFAULT_TYPE;
        IR_LINE_NUM(use_ir_idx)		= stmt_start_line;
        IR_COL_NUM(use_ir_idx)		= stmt_start_col;
        IR_IDX_L(use_ir_idx)		= module_attr_idx;
        IR_FLD_L(use_ir_idx)		= AT_Tbl_Idx;
        IR_LINE_NUM_L(use_ir_idx)	= stmt_start_line;
        IR_COL_NUM_L(use_ir_idx)	= stmt_start_col;

        gen_sh(Before,
               Use_Stmt,
               stmt_start_line,
               stmt_start_col,
               FALSE,
               FALSE,
               TRUE); /* Compiler gen'd */

        SH_P2_SKIP_ME(SH_PREV_IDX(curr_stmt_sh_idx))	= TRUE;
        SH_IR_IDX(SH_PREV_IDX(curr_stmt_sh_idx))	= use_ir_idx;
      }

      if (on_off_flags.issue_ansi_messages ||
          GET_MESSAGE_TBL(message_warning_tbl, 953) ||
          GET_MESSAGE_TBL(message_error_tbl, 953)) {

         /* Non standard to let a module have the same name as a local */
         /* entity in a parent scope.  Don't issue if the entity in    */
         /* parent scope is a module.  This means we are using the     */
         /* module in several scopes and is legal and standard.        */

         srch_attr_idx = srch_host_sym_tbl(AT_OBJ_NAME_PTR(module_attr_idx),
                                           AT_NAME_LEN(module_attr_idx),
                                           &name_idx,
                                           FALSE);

         if (srch_attr_idx != NULL_IDX && 
             (AT_OBJ_CLASS(srch_attr_idx) != Pgm_Unit ||
              ATP_PGM_UNIT(srch_attr_idx) != Module)) {
            PRINTMSG(AT_DEF_LINE(module_attr_idx), 953, Ansi,
                     AT_DEF_COLUMN(module_attr_idx),
                     AT_OBJ_NAME_PTR(module_attr_idx));
         }
      }

      save_attr_list_tbl_idx	= attr_list_tbl_idx;

      /* Force all new attr list table entries to the end so we can */
      /* find the new ones for compression.                         */

      AL_NEXT_IDX(NULL_IDX)	= NULL_IDX;
      use_only			= ATP_USE_TYPE(module_attr_idx) == Use_Only;

      if (!find_prog_unit_tbl(module_attr_idx)) {

         /* Couldn't find the module or bad reads */

         goto EXIT;
      }

#ifdef KEY /* Bug 5089 */
      int intrinsic_module = AT_IS_INTRIN(module_attr_idx);
      /* F2003: Scope which accesses ieee_* intrinsic modules must save FPU
       * state on entry and restore it (ORing flags) on exit.
       * decl_semantics_driver() takes care of access by host association. */
      if (is_ieee(module_attr_idx)) {
	SCP_USES_IEEE(curr_scp_idx) = TRUE;
      }
#endif /* KEY Bug 5089 */

      start_ln_idx	= loc_name_tbl_idx - MD_NUM_ENTRIES(Loc_Name_Tbl) + 1;
      attr_idx		= attr_tbl_idx - MD_NUM_ENTRIES(Attr_Tbl) + 1;

# ifdef _DEBUG
      if (start_ln_idx <= 0) {
         PRINTMSG(1, 626, Internal, 0, "positive start_ln_idx",
                  "use_stmt_semantics");
      }
      if (attr_idx <= 0) {
         PRINTMSG(1, 626, Internal, 0, "positive attr_idx",
                  "use_stmt_semantics");
      }
# endif

      if (ATP_USE_LIST(module_attr_idx) != NULL_IDX) {
         rename_only_semantics(module_attr_idx, use_only);
      }

      /* Go through all new entries in the local name table.  Decide which */
      /* ones to keep and which to toss.  This merges the new entries into */
      /* the old local name table as it goes through.  Since the new       */
      /* entries follow the old, as the old table grows the new table      */
      /* shrinks.  Both tables are in alphabetical order and we start at   */
      /* the top of the new table.  If we are keeping the new entry we     */
      /* move it up to its proper position in the old table.  We enter it  */
      /* by moving everything down in the old table after its proper       */
      /* position.  One entry of space has been left between the old and   */
      /* new tables to make sure that as the old table grows it does not   */
      /* write over the current entry being processed from the new table.  */

      ln_idx		= SCP_LN_FW_IDX(curr_scp_idx) + 1;

      for (new_name_idx = start_ln_idx;
           new_name_idx <= loc_name_tbl_idx; 
           new_name_idx++) {

         if (use_only) {

            /* This module is brought in with an ONLY.  Throw out       */
            /* everything that is not specified on the ONLY statement.  */
            /* If this is a renamed item from an ONLY list, LN_NEW_NAME */
            /* will be set, but LN_IN_ONLY_LIST will not be set, unless */
            /* the name itself is specified in the only list.           */

            if (!LN_IN_ONLY_LIST(new_name_idx) && 
                !LN_NEW_NAME(new_name_idx)) {
               continue;
            }

            ML_LN_KEEP_ME(new_name_idx)		= TRUE;
            LN_IN_ONLY_LIST(new_name_idx)	= FALSE;
            LN_NEW_NAME(new_name_idx)		= FALSE;
            LN_RENAMED(new_name_idx)		= FALSE;
         }
         else {  /* Possible renames */

            if (LN_RENAMED(new_name_idx)) {
               continue;   /* This has been renamed.  Throw out. */
            }

            ML_LN_KEEP_ME(new_name_idx)	= TRUE;
            LN_NEW_NAME(new_name_idx)	= FALSE;
         }

         attr_idx	= LN_ATTR_IDX(new_name_idx);

#ifdef KEY /* Bug 5089 */
         /* Look up the original name of this entity in the
	  * intrinsic_module_table. If we find it, change the attr_tbl_entry
	  * so that a call to it will be replaced by intrinsic code. */
         if (intrinsic_module && NULL_IDX != AT_ORIG_NAME_IDX(attr_idx)) {
	   intrinsic_module_lookup(attr_idx);
	 }
#endif /* KEY Bug 5089 */

         /* Find the new entries position in the old local name table. */

         do {

            if (ln_idx >= SCP_LN_LW_IDX(curr_scp_idx)) {
               ln_idx = SCP_LN_LW_IDX(curr_scp_idx);
               match  = -1;
            }
            else {
               match = compare_names(LN_NAME_LONG(new_name_idx),
                                     LN_NAME_LEN(new_name_idx),
                                     LN_NAME_LONG(ln_idx),
                                     LN_NAME_LEN(ln_idx));

               if (match > 0) {
                  ln_idx++;
               } 
            } 
         } 
         while (match > 0);

         if (match == 0) {
            not_visible_semantics(attr_idx,          /* new attr index  */
                                  ln_idx,            /* Old name index  */
                                  module_attr_idx);
            AT_REFERENCED(AT_MODULE_IDX(attr_idx)) = Referenced;
         }
         else {
            (SCP_LN_LW_IDX(curr_scp_idx))++;

            NTR_NAME_IN_LN_TBL(ln_idx, new_name_idx);

            LN_DEF_LOC(new_name_idx)	= TRUE;

            if (!ML_AT_SEARCHED(attr_idx) && resolve_attr(attr_idx)) {

               /* If resolve attr is TRUE, we are not keeping the attr */
               /* entry, because the same object is already in this    */
               /* scope and we are going to use that attr entry.       */

               KEEP_ATTR(ML_AT_IDX(attr_idx));
            }
            else {

               /* If we are keeping this attr, set_mod_link_tbl_for_attr */
               /* will call resolve_attr for all the dependent attrs.    */

               AT_REFERENCED(AT_MODULE_IDX(attr_idx)) = Referenced;

               KEEP_ATTR(attr_idx);

               if (AT_OBJ_CLASS(attr_idx) == Interface &&
                   !AT_IS_INTRIN(attr_idx) &&
                   SCP_PARENT_IDX(curr_scp_idx) != NULL_IDX &&
                   !SCP_IS_INTERFACE(curr_scp_idx)) {

                  /* Add to the top of the interface list. */

                  /* The following code implements interp 99                */
                  /* If two or more generic interfaces that are accessible  */
                  /* in a scoping unit have the same name, ..., they are    */
                  /* interpreted as a single generic interface.             */

                  /* We actually do the host association after all the use  */
                  /* statements for this scope are processed.  If we don't  */
                  /* wait, we end up putting new scopes after the current   */
                  /* scopes.                                                */

                  NTR_ATTR_LIST_TBL(al_idx);
                  AL_ATTR_IDX(al_idx)	= attr_idx;
                  AL_NEXT_IDX(al_idx)	= interface_list;
                  interface_list	= al_idx;
               }
            }
         }

         if (AT_NAME_IDX(attr_idx) != AT_ORIG_NAME_IDX(attr_idx)) { /* Renamed*/

            if (AT_OBJ_CLASS(attr_idx) == Data_Obj) {

               if (ATD_STOR_BLK_IDX(attr_idx) != NULL_IDX) {
                  SB_HAS_RENAMES(ATD_STOR_BLK_IDX(attr_idx)) = TRUE;
               }
            }
            else if (AT_OBJ_CLASS(attr_idx) == Pgm_Unit &&
                     ATP_PROC(attr_idx) == Function &&
                     !ATP_RSLT_NAME(attr_idx)  &&
                     ATD_STOR_BLK_IDX(ATP_RSLT_IDX(attr_idx)) != NULL_IDX) {
               SB_HAS_RENAMES(ATD_STOR_BLK_IDX(ATP_RSLT_IDX(attr_idx))) = TRUE;
            }
         }
      }

      /* At this point, all new attribute entries have been checked to see */
      /* if they already exist in this scope, because of a previous use    */
      /* statement.  If the attr will get put into the new local name      */
      /* table, it has not been checked yet for not visible semantics.     */
      /* That is done as the old an new entries are merged.  If the attr   */
      /* will not go in the new local name table, both the local name tbl  */
      /* and the hidden name table were searched for the attr entry.  If   */
      /* it was found already in either table, the attr is marked so that  */
      /* the new attr idx becomes the old.  Any new attrs are entered into */
      /* the hidden name table.                                            */


      /* resolve_used_modules will issue CIF records and messages where    */
      /* necessary for all the modules brought in via this USE statement.  */

      resolve_used_modules(module_attr_idx);

      loc_name_tbl_idx = SCP_LN_LW_IDX(curr_scp_idx);

      /* The compression is a partial compression.  It only compresses the */
      /* tables just read in from the module table.  It is compressing out */
      /* entries not needed after the ONLY list(s) have been processed.    */
      /* These entries may point into the section of the tables not being  */
      /* compressed.  The compression algorithm handles this, but entries  */
      /* that are being kept cannot index to entries that are being        */
      /* compressed out.                                                   */


      /* At this point the local name table does not contain anything that */
      /* needs to be compressed out.  Do not compress the ln table.        */

      ML_LN_IDX(0) = SCP_LN_LW_IDX(curr_scp_idx);

      /* Keep everything on the bounds table free list.  It's easier to    */
      /* keep it, than to attempt to collapse it out, because we do not    */
      /* know if the free entries are in the area being collapsed or in    */
      /* the area being left alone.                                        */

      bd_idx       = BD_NEXT_FREE_NTRY(BD_FREE_LIST_IDX);

      while (bd_idx != NULL_IDX) {
         ML_BD_KEEP_ME(bd_idx)	= TRUE;
         bd_idx			= BD_NEXT_FREE_NTRY(bd_idx);
      }

      /* Resolve duplicate entries and share constant table entries. */

      assign_new_idxs(TRUE);

      save_const_pool_idx	= NULL_IDX;
      save_const_tbl_idx	= NULL_IDX;
      num_module_derived_types	= 0;
      count_derived_types	= TRUE;

      compress_tbls(save_attr_list_tbl_idx, FALSE);

      if (CURR_BLK != Interface_Body_Blk) {

         /* Interface_Body_Blk stuff is counted during interface collapse. */

         num_of_derived_types   += num_module_derived_types;
      }

      num_module_derived_types	= 0;

      BD_NEXT_FREE_NTRY(BD_FREE_LIST_IDX) = 
                                ML_BD_IDX(BD_NEXT_FREE_NTRY(BD_FREE_LIST_IDX));

      for (new_name_idx = SCP_LN_FW_IDX(curr_scp_idx) + 1;
           new_name_idx < SCP_LN_LW_IDX(curr_scp_idx); new_name_idx++) {
         LN_ATTR_IDX(new_name_idx) = ML_AT_IDX(LN_ATTR_IDX(new_name_idx));
         LN_NAME_IDX(new_name_idx) = AT_NAME_IDX(LN_ATTR_IDX(new_name_idx));
         LN_NAME_LEN(new_name_idx) = AT_NAME_LEN(LN_ATTR_IDX(new_name_idx));
      }

EXIT:
      ATP_SCP_ALIVE(module_attr_idx)	= FALSE;
      ATP_USE_LIST(module_attr_idx)	= NULL_IDX;
      TBL_FREE(mod_link_tbl);
   }

   al_idx	= interface_list;

   while (al_idx != NULL_IDX) {
      attr_idx		= AL_ATTR_IDX(al_idx);
      host_attr_idx	= srch_host_sym_tbl(AT_OBJ_NAME_PTR(attr_idx),
                                            AT_NAME_LEN(attr_idx),
                                            &host_name_idx,
                                            TRUE);

      if (host_attr_idx != NULL_IDX &&
          !AT_NOT_VISIBLE(host_attr_idx) &&
           AT_OBJ_CLASS(host_attr_idx) == Interface) {

         /* Found this in a host scope.  Just concatenate the */
         /* hosted one following the new one from the module. */
         /* Based on concatenation rules we do not check for  */
         /* duplicates.  Duplicates should get errors.        */
         /* Duplicates that are actually from the same module */
         /* are ignored during semantic checking of the block.*/

         new_sn_idx	= ATI_FIRST_SPECIFIC_IDX(attr_idx);

         while (SN_SIBLING_LINK(new_sn_idx) != NULL_IDX) {
            new_sn_idx	= SN_SIBLING_LINK(new_sn_idx);
         }

         SN_SIBLING_LINK(new_sn_idx) = ATI_FIRST_SPECIFIC_IDX(host_attr_idx);
         ATI_NUM_SPECIFICS(attr_idx) = ATI_NUM_SPECIFICS(attr_idx) +
                                       ATI_NUM_SPECIFICS(host_attr_idx); 
      }
      al_idx	= AL_NEXT_IDX(al_idx);
   }

   al_idx = SCP_USED_MODULE_LIST(curr_scp_idx);

   while (al_idx != NULL_IDX) {

      if (ATP_INDIRECT_MODULE(AL_ATTR_IDX(al_idx))) {
         ADD_ATTR_TO_LOCAL_LIST(AL_ATTR_IDX(al_idx));
#ifdef KEY /* Bug 5089 */
	 if (is_ieee(AL_ATTR_IDX(al_idx))) {
	   SCP_USES_IEEE(curr_scp_idx) = TRUE;
	 }
#endif /* KEY Bug 5089 */
      }
      al_idx	= AL_NEXT_IDX(al_idx);
   }

   free_attr_list(interface_list);
   free_attr_list(attr_list_free_list);
   free_attr_list(SCP_USED_MODULE_LIST(curr_scp_idx));

   SCP_USED_MODULE_LIST(curr_scp_idx)	= NULL_IDX;

   TBL_FREE(rename_only_tbl);

   keep_module_procs			= FALSE;

   TRACE (Func_Exit, "use_stmt_semantics", NULL);

   return;

}   /* use_stmt_semantics */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	This routine processes the rename only list.  It checks to make sure  *|
|*	all the specified names are in the incoming module and marks them if  *|
|*	they are specified in an ONLY list.  If they are renamed, it adds a   *|
|*	new entry for the new name to the incoming local name table.          *|
|*									      *|
|* Input parameters:							      *|
|*	module_attr_idx -> The module being processed.			      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/
static	boolean	rename_only_semantics(int	module_attr_idx,
				      boolean	use_only)
{
   int		 attr_idx;
   int		 begin_idx;
#ifdef KEY /* Bug 10177 */
   int		 cif_symbol_id = 0;
#else /* KEY Bug 10177 */
   int		 cif_symbol_id;
#endif /* KEY Bug 10177 */
   int		 end_idx;
   int		 func_idx;
   boolean	 has_renames		= FALSE;
   int		 i;
   int		 idx;
   int		 length;
   int		 ln_idx;
   int		 match;
#ifdef KEY /* Bug 10177 */
   int		 name_idx = 0;
#else /* KEY Bug 10177 */
   int		 name_idx;
#endif /* KEY Bug 10177 */
   int		 new_attr_idx;
   int		 new_name_idx;
   int		 np_idx;
   int		 rename_idx;
   int		 ro_idx;

# if defined(_HOST64) && !defined(_WHIRL_HOST64_TARGET64)
   long		*name_tbl_base;		/* name table base address */
# endif


   TRACE (Func_Entry, "rename_only_semantics", NULL);

   ro_idx	= ATP_USE_LIST(module_attr_idx);
   ln_idx	= loc_name_tbl_idx - MD_NUM_ENTRIES(Loc_Name_Tbl) + 1;
   begin_idx	= SCP_LN_FW_IDX(curr_scp_idx);
   end_idx	= SCP_LN_LW_IDX(curr_scp_idx);

   /* Add a word for the all 1's word for table searches.  A word  */
   /* was left for the all 0's word, when the modules was read in. */

   TBL_REALLOC_CK(loc_name_tbl, 1);

   /* Set the current scope to the incoming local name table. */

   SCP_LN_FW_IDX(curr_scp_idx)		= ln_idx - 1;
   SCP_LN_LW_IDX(curr_scp_idx)		= loc_name_tbl_idx;
   loc_name_tbl[ln_idx-1]		= loc_name_tbl[begin_idx];
   loc_name_tbl[loc_name_tbl_idx]	= loc_name_tbl[end_idx];

   while (ro_idx != NULL_IDX) {
      rename_idx	= RO_RENAME_IDX(ro_idx);
      attr_idx		= NULL_IDX;
   
      /* This WHILE finds the specified name in the local */
      /* name table of the module being read in.          */

      for (;;) {

         if (ln_idx >= loc_name_tbl_idx) {

            /* The name in the ONLY/rename list is larger than the last */
            /* name in the USEd module list.  This means it won't be    */
            /* found.  Set match to not found and take the error path.  */

            ln_idx = loc_name_tbl_idx;
            match  = -1;
            break;
         }

         match = compare_names(RO_NAME_LONG(ro_idx),
                               RO_NAME_LEN(ro_idx),
                               LN_NAME_LONG(ln_idx),
                               LN_NAME_LEN(ln_idx));

         if (match > 0) {

            /* The name in the ONLY/rename list is larger than the name */
            /* in the USEd module list.  Clear ML_AT_LN_NAME in case    */
            /* this attr gets used indirectly. (ie:  It's a type attr.) */
            /* Keep looping and looking.                                */

            ML_AT_LN_NAME(LN_ATTR_IDX(ln_idx)) = !use_only;

            ln_idx++;

         } 
         else if (LN_NEW_NAME(ln_idx)) {

            /* This is a new name added during this while processing    */
            /* from a rename list.  Keep looping and looking.           */

            ln_idx++;
         } 
         else {
            break;
         } 
      }  /* end for - match is always <= 0 */ 


      if (match == 0) {  /* Found the name in the module */
         attr_idx	= LN_ATTR_IDX(ln_idx);
         name_idx	= ln_idx;

         /* If the name is a USE'd module, then the name was NOT found.    */
         /* We keep any USE'd modules in the attribute table for           */
         /* bookkeeping purposes, but the module name cannot be specified  */
         /* on a rename or ONLY list.                                      */

         if (AT_OBJ_CLASS(attr_idx) == Pgm_Unit &&
             ATP_PGM_UNIT(attr_idx) == Module) {
             match = -1;
         }
      }
         
      if (match < 0) {  /* Didn't find the name in the module */
         name_idx	= ln_idx;

         if (attr_idx != NULL_IDX) {  /* Module name */
            AT_DCL_ERR(attr_idx)	= TRUE;

            if (AT_OBJ_CLASS(attr_idx) == Pgm_Unit &&
                ATP_PGM_UNIT(attr_idx) == Module && ATP_SCP_ALIVE(attr_idx)) {

               /* Cannot specify the module name in a rename only list     */
               /* Continue to the next item in the ro list.  If we renamed */
               /* the current module, it would just cause problems.        */

               PRINTMSG(RO_LINE_NUM(ro_idx), 1098, Error,
                        RO_COLUMN_NUM(ro_idx),
                        RO_NAME_PTR(ro_idx));
               ro_idx	= RO_NEXT_IDX(ro_idx);
               continue;
            }
            else {
               PRINTMSG(RO_LINE_NUM(ro_idx), 732, Error,
                        RO_COLUMN_NUM(ro_idx),
                        RO_NAME_PTR(ro_idx),
                        AT_OBJ_NAME_PTR(module_attr_idx));


               /* This module is a hidden module.  (indirectly used.)     */
               /* Make a new attr for the rename name for error recovery. */

               NTR_ATTR_TBL(attr_idx);

               idx	= attr_tbl_idx;

               if (idx > mod_link_tbl_idx) {
                  length = idx - mod_link_tbl_idx;
                  idx    = mod_link_tbl_idx + 1;
                  TBL_REALLOC_CK(mod_link_tbl, length);

                  for (; idx <= mod_link_tbl_idx; idx++) {
                     CLEAR_TBL_NTRY(mod_link_tbl, idx);
                  }
               }

               ML_AT_LN_NAME(attr_idx)		= TRUE;
               AT_DCL_ERR(attr_idx)		= TRUE;
               AT_NAME_IDX(attr_idx)		= RO_NAME_IDX(ro_idx);
               AT_NAME_LEN(attr_idx)		= RO_NAME_LEN(ro_idx);
               AT_ORIG_NAME_IDX(attr_idx)	= RO_NAME_IDX(ro_idx);
               AT_ORIG_NAME_LEN(attr_idx)	= RO_NAME_LEN(ro_idx);

               /* Need to set this as USE ASSOCIATED from the module */
               /* to prevent bad error recovery problems.            */

               AT_USE_ASSOCIATED(attr_idx)	= TRUE;
               AT_MODULE_IDX(attr_idx)		= module_attr_idx;

               /* Give it an intrinsic type */

               SET_IMPL_TYPE(attr_idx);
            }
         }
         else {
            PRINTMSG(RO_LINE_NUM(ro_idx), 732, Error,
                     RO_COLUMN_NUM(ro_idx),
                     RO_NAME_PTR(ro_idx),
                     AT_OBJ_NAME_PTR(module_attr_idx));

            NTR_NAME_POOL(RO_NAME_LONG(ro_idx),
                          RO_NAME_LEN(ro_idx),
                          np_idx);

            /* Make an error entry into the local name table. */

            TBL_REALLOC_CK(loc_name_tbl, 1);

            /* Adding to local name table for last (most recent) scope.  No   */
            /* adjusting of other scope local name table entries is necessary.*/

            SCP_LN_LW_IDX(curr_scp_idx)	= loc_name_tbl_idx;
# if defined(_HOST64) && !defined(_WHIRL_HOST64_TARGET64)
            name_tbl_base		= (long *) loc_name_tbl;
# endif

#  pragma _CRI ivdep
            for (i = loc_name_tbl_idx; i >= name_idx; i--) {
# if defined(_HOST64) && !defined(_WHIRL_HOST64_TARGET64)
               name_tbl_base [i] = name_tbl_base [i-1];
# else
               loc_name_tbl [i]  = loc_name_tbl [i-1];
# endif
            }

            NTR_ATTR_TBL(attr_idx);

            idx	= (attr_tbl_idx > name_pool_idx) ? attr_tbl_idx : name_pool_idx;

            if (idx > mod_link_tbl_idx) {
               length = idx - mod_link_tbl_idx;
               idx    = mod_link_tbl_idx + 1;
               TBL_REALLOC_CK(mod_link_tbl, length);

               for (; idx <= mod_link_tbl_idx; idx++) {
                  CLEAR_TBL_NTRY(mod_link_tbl, idx);
               }
            }

            ML_AT_LN_NAME(attr_idx)	= TRUE;
            LN_ATTR_IDX(name_idx)	= attr_idx;
            LN_NAME_IDX(name_idx)	= np_idx;
            LN_NAME_LEN(name_idx)	= RO_NAME_LEN(ro_idx);
            AT_DCL_ERR(attr_idx)	= TRUE;
            AT_NAME_IDX(attr_idx)	= np_idx;
            AT_NAME_LEN(attr_idx)	= RO_NAME_LEN(ro_idx);
            AT_ORIG_NAME_IDX(attr_idx)	= np_idx;
            AT_ORIG_NAME_LEN(attr_idx)	= RO_NAME_LEN(ro_idx);

            /* Need to set this as USE ASSOCIATED from the module */
            /* to prevent bad error recovery problems.            */

            AT_USE_ASSOCIATED(attr_idx)	= TRUE;
            AT_MODULE_IDX(attr_idx)	= module_attr_idx;

            /* Give it an intrinsic type */

            SET_IMPL_TYPE(attr_idx);
         }
      }

      if (cif_flags & BASIC_RECS) {

         if (!LN_RENAMED(name_idx) && !LN_IN_ONLY_LIST(name_idx)) {

            /* The RO records are alphabetized by name.  If this name has  */
            /* been seen in an ONLY list or RENAMED, it already has a CIF  */
            /* symbol id, otherwise it needs a new symbol id.  Pass 0 as   */
            /* the symbol id, so cif_rename_rec will generate a new symbol */
            /* id.                                                        */

            cif_symbol_id = 0;
         }
      }


      if (rename_idx == NULL_IDX) {  /* ONLY without a renames. */

         if (LN_RENAMED(name_idx)) {

            /* This has been renamed already.  There are two local name  */
            /* entries that point to the same attr.  The original name   */
            /* and the renamed name.  Copy the attr entry so that the    */
            /* renamed local name gets its own attr.  Need to set        */
            /* ATD_EQUIV on both attr entries, because there are now two */
            /* objects with different name.  They are effectively        */
            /* equivalenced.  Need to reset the name on the attr.  It    */
            /* It is set to the new name.                                */

            /* before:                                                   */
            /*   name from module => old_attr     (input from module)    */
            /*   new_name         => old_attr     (RENAME specified)     */
            /* after:                                                    */
            /*   name from module => new_attr     (ONLY specified)       */
            /*   new_name         => old_attr     (RENAME specified)     */

            NTR_ATTR_TBL(new_attr_idx);

            if (attr_tbl_idx > mod_link_tbl_idx) {
               length = attr_tbl_idx - mod_link_tbl_idx;
               idx    = mod_link_tbl_idx + 1;
               TBL_REALLOC_CK(mod_link_tbl, length);

               for (; idx <= mod_link_tbl_idx; idx++) {
                  CLEAR_TBL_NTRY(mod_link_tbl, idx);
               }
            }

            if (AT_OBJ_CLASS(attr_idx) == Data_Obj) {
               ATD_EQUIV(attr_idx)		= TRUE;
            }

            COPY_ATTR_NTRY(new_attr_idx, attr_idx);
            AT_CIF_SYMBOL_ID(new_attr_idx)	= 0;
            AT_NAME_IDX(new_attr_idx)		= LN_NAME_IDX(name_idx);
            AT_NAME_LEN(new_attr_idx)		= LN_NAME_LEN(name_idx);
            attr_idx				= new_attr_idx;
            LN_ATTR_IDX(name_idx)		= attr_idx;
            ML_AT_LN_NAME(attr_idx)		= TRUE;
         }

         LN_IN_ONLY_LIST(name_idx)	= TRUE;
         AT_DEF_LINE(attr_idx)		= RO_LINE_NUM(ro_idx);
         AT_DEF_COLUMN(attr_idx)	= RO_COLUMN_NUM(ro_idx);

         if ((cif_flags & XREF_RECS) != 0) {  /* Only */
            cif_usage_rec(attr_idx,
                          AT_Tbl_Idx,
                          RO_LINE_NUM(ro_idx),
                          RO_COLUMN_NUM(ro_idx),
                          CIF_Symbol_Reference);
         }
      }
      else { /* Put new name into incoming symbol table. */
         has_renames	= TRUE;

         if (RO_DUPLICATE_RENAME(rename_idx) &&
             AT_OBJ_CLASS(attr_idx) != Interface) {

            /* This rename name has been specified twice in the rename list */

            PRINTMSG(RO_LINE_NUM(rename_idx), 1015, Error,
                     RO_COLUMN_NUM(rename_idx),
                     RO_NAME_PTR(rename_idx));
         }

         if (LN_RENAMED(name_idx) || LN_IN_ONLY_LIST(name_idx)) {

            /* This has been renamed or specified in an ONLY list already.   */
            /* Need a new attr entry.  If this is renamed there are two      */
            /* local entries pointing to the same attr.  (See comment above).*/
            /* If this is specified in an ONLY list we need to make a new    */
            /* local name/attr combination because of the different name.    */

            NTR_ATTR_TBL(new_attr_idx);

            if (attr_tbl_idx > mod_link_tbl_idx) {
               length = attr_tbl_idx - mod_link_tbl_idx;
               idx    = mod_link_tbl_idx + 1;
               TBL_REALLOC_CK(mod_link_tbl, length);

               for (; idx <= mod_link_tbl_idx; idx++) {
                  CLEAR_TBL_NTRY(mod_link_tbl, idx);
               }
            }

            if (AT_OBJ_CLASS(attr_idx) == Data_Obj) {
               ATD_EQUIV(attr_idx)	= TRUE;
            }
            COPY_ATTR_NTRY(new_attr_idx, attr_idx);
            attr_idx			= new_attr_idx;
            AT_CIF_SYMBOL_ID(attr_idx)	= 0;
         }

         LN_RENAMED(name_idx)		= TRUE;
         ML_AT_LN_NAME(attr_idx)	= TRUE;

         /* The current scopes SCP_LN_FW_IDX and SCP_LN_LW_IDX have been */
         /* set to point to the new scope.  It may not be NULL, but that */
         /* is okay.  We are just looking for a place to put the name.   */

         new_attr_idx	= srch_sym_tbl(RO_NAME_PTR(rename_idx),
                                       RO_NAME_LEN(rename_idx),
                                       &new_name_idx);

         TBL_REALLOC_CK(loc_name_tbl, 1);

         if (loc_name_tbl_idx > mod_link_tbl_idx) {
            length = loc_name_tbl_idx - mod_link_tbl_idx;
            idx    = mod_link_tbl_idx + 1;
            TBL_REALLOC_CK(mod_link_tbl, length);

            for (; idx <= mod_link_tbl_idx; idx++) {
               CLEAR_TBL_NTRY(mod_link_tbl, idx);
            }
         }

         /* Adding to local name table for last (most recent) scope.  No   */
         /* adjusting of other scope local name table entries is necessary.*/

         SCP_LN_LW_IDX(curr_scp_idx)	= loc_name_tbl_idx;
# if defined(_HOST64) && !defined(_WHIRL_HOST64_TARGET64)
         name_tbl_base			= (long *) loc_name_tbl;
# endif

#  pragma _CRI ivdep
         for (i = loc_name_tbl_idx; i >= new_name_idx; i--) {
# if defined(_HOST64) && !defined(_WHIRL_HOST64_TARGET64)
            name_tbl_base [i] = name_tbl_base [i-1];
# else
            loc_name_tbl [i]  = loc_name_tbl [i-1];
# endif
         }

         /* Adjust ln_idx from the match loop.  ln_idx is used to search */
         /* through this local name table.  Members of the table have    */
         /* just shifted, so ln_idx must shift as well.                  */

         if (new_name_idx < ln_idx) {
            ln_idx++;
         }

         LN_ATTR_IDX(new_name_idx)	= attr_idx;
         LN_NAME_IDX(new_name_idx)	= RO_NAME_IDX(rename_idx);
         LN_NAME_LEN(new_name_idx)	= RO_NAME_LEN(rename_idx);
         LN_DEF_LOC(new_name_idx)	= TRUE;
         LN_NEW_NAME(new_name_idx)	= TRUE;
         LN_RENAMED(new_name_idx)	= FALSE;
         AT_DEF_LINE(attr_idx)		= RO_LINE_NUM(rename_idx);
         AT_DEF_COLUMN(attr_idx)	= RO_COLUMN_NUM(rename_idx);

         if (cif_flags & BASIC_RECS) {
            cif_symbol_id = cif_rename_rec(ro_idx,
                                           cif_symbol_id,
                                           attr_idx,
                                           module_attr_idx);

            if ((cif_flags & XREF_RECS) != 0) {
               cif_usage_rec(cif_symbol_id,
                             NO_Tbl_Idx,
                             RO_LINE_NUM(ro_idx),
                             RO_COLUMN_NUM(ro_idx),
                             CIF_Symbol_Reference);

               cif_usage_rec(attr_idx,
                             AT_Tbl_Idx,
                             RO_LINE_NUM(rename_idx),
                             RO_COLUMN_NUM(rename_idx),
                             CIF_Symbol_Declaration);
            }
         }

         AT_NAME_IDX(attr_idx)		= LN_NAME_IDX(new_name_idx);
         AT_NAME_LEN(attr_idx)		= LN_NAME_LEN(new_name_idx);
         AT_ORIG_MODULE_IDX(attr_idx)	= module_attr_idx;

         if (AT_OBJ_CLASS(attr_idx) == Interface ||
             AT_OBJ_CLASS(attr_idx) == Pgm_Unit) {
            func_idx	= attr_idx;

            if (AT_OBJ_CLASS(func_idx) == Interface &&
                ATI_PROC_IDX(func_idx) != NULL_IDX) {
               func_idx			= ATI_PROC_IDX(func_idx);
               AT_NAME_IDX(func_idx)	= LN_NAME_IDX(new_name_idx);
               AT_NAME_LEN(func_idx)	= LN_NAME_LEN(new_name_idx);
               AT_DEF_LINE(func_idx)	= AT_DEF_LINE(attr_idx);
               AT_DEF_COLUMN(func_idx)	= AT_DEF_COLUMN(attr_idx);
            }

            if (AT_OBJ_CLASS(func_idx) == Pgm_Unit &&
                !ATP_RSLT_NAME(func_idx) &&
                ATP_PGM_UNIT(func_idx) != Module &&
                ATP_RSLT_IDX(func_idx) != NULL_IDX) {
               func_idx			= ATP_RSLT_IDX(func_idx);
               AT_NAME_IDX(func_idx)	= LN_NAME_IDX(new_name_idx);
               AT_NAME_LEN(func_idx)	= LN_NAME_LEN(new_name_idx);
               AT_DEF_LINE(func_idx)	= AT_DEF_LINE(attr_idx);
               AT_DEF_COLUMN(func_idx)	= AT_DEF_COLUMN(attr_idx);
            }
         }
         else if (AT_OBJ_CLASS(attr_idx) == Data_Obj &&
                  ATD_CLASS(attr_idx) == Constant &&
                  ATD_FLD(attr_idx) == AT_Tbl_Idx) {
            AT_NAME_IDX(ATD_CONST_IDX(attr_idx)) = LN_NAME_IDX(new_name_idx);
            AT_NAME_LEN(ATD_CONST_IDX(attr_idx)) = LN_NAME_LEN(new_name_idx);
         }
      }
      ro_idx			= RO_NEXT_IDX(ro_idx);
   }

   SCP_LN_FW_IDX(curr_scp_idx)	= begin_idx;
   SCP_LN_LW_IDX(curr_scp_idx)	= end_idx;
   loc_name_tbl_idx--;		/* Don't need all ones entry anymore */

   TRACE (Func_Exit, "rename_only_semantics", NULL);

   return(has_renames);

}   /* rename_only_semantics */

/*
 * On behalf of find_prog_unit_tbl, search for a module in either
 * inline_path_idx, module_path_idx (for ordinary modules) or
 * intrinsic_module_path_idx (for F2003 intrinsic modules.)
 *
 * module_attr_idx	module to look for
 * fp_file_idx		head of list in which to search (inline_path_idx,
 *			module_path_idx, or intrinsic_module_path_idx)
 * mod_file_ptr_ptr	caller passes mod_file_ptr to us by reference
 * output_fp_file_idx	value of fp_file_idx changes during this function;
 *			we return its final value to the caller via this arg
 * returns		true if module found
 */
#ifdef KEY /* Bug 5089 */
static boolean look_for_module(int module_attr_idx, int fp_file_idx,
   FILE **mod_file_ptr_ptr, int *output_fp_file_idx) {
   char		file_name[MAX_FILE_NAME_SIZE];
   char	       *file_name_ptr;
   int		fn_length;
   boolean	archive;
   FILE *mod_file_ptr = *mod_file_ptr_ptr;
#endif /* KEY Bug 5089 */
   if (on_off_flags.module_to_mod && !inline_search) {
      strcpy(file_name, AT_OBJ_NAME_PTR(module_attr_idx));
      fn_length			= AT_NAME_LEN(module_attr_idx);
      file_name[fn_length++]	= '.';
      file_name[fn_length++]	= 'm';
      file_name[fn_length++]	= 'o';
      file_name[fn_length++]	= 'd';
   }
   else {
      fn_length			= 0;
   }
   file_name[fn_length]	= '\0';

   while (fp_file_idx != NULL_IDX) {
#ifdef KEY /* Bug 5089 */
      int
#endif /* KEY Bug 5089 */
      fp_module_idx		= NULL_IDX;
#ifdef KEY /* Bug 5089 */
      int
#endif /* KEY Bug 5089 */
      next_fp_module_idx	= FP_MODULE_IDX(fp_file_idx); /* 1st module */

      while (next_fp_module_idx != NULL_IDX) {
         fp_module_idx		= next_fp_module_idx;
         next_fp_module_idx	= FP_MODULE_IDX(fp_module_idx);

         if (compare_names(AT_OBJ_NAME_LONG(module_attr_idx),
                           AT_NAME_LEN(module_attr_idx),
                           FP_NAME_LONG(fp_module_idx),
                           FP_NAME_LEN(fp_module_idx)) == 0) {

            /* Found the matching module.  Open the file and read header. */

            mod_file_ptr = open_module_file(module_attr_idx,
                                            fp_file_idx);
            

#ifdef KEY /* Bug 5089 */
            boolean
#endif /* KEY Bug 5089 */
            found = (mod_file_ptr == NULL) ?
                     FALSE : read_module_tbl_header(module_attr_idx,
                                                    fp_module_idx,
                                                    mod_file_ptr);
#ifdef KEY /* Bug 5089 */
	    *mod_file_ptr_ptr = mod_file_ptr;
	    *output_fp_file_idx = fp_file_idx;
            return found;
#else /* KEY Bug 5089 */
            goto FOUND;
#endif /* KEY Bug 5089 */
         }
      }  /* End while - looking through module names in same file. */

      if (FP_SRCH_THE_FILE(fp_file_idx)) {

         /* All files specified on the commandline will come through as  */
         /* Unknown_Fp.  Here is where we determine what they are.       */

         if (FP_CLASS(fp_file_idx) == Unknown_Fp) {

            /* Determine if this is a directory or a file.  */
            /* If directory, convert to list of files.   If */
            /* file, they will be marked as Elf files if on */
            /* solaris and as regular files if not solaris. */

            find_files_in_directory(fp_file_idx);

            if (FP_CLASS(fp_file_idx) == Directory_Fp) {

               /* Skip the directory and go the next file or dir in the    */
               /* file path table.  This is most likely a file specified   */
               /* in the directory, but if the directory is empty, then    */
               /* this is the file or dir following the directory.         */

               fp_file_idx	= FP_NEXT_FILE_IDX(fp_file_idx);
               continue;
            }
         }

# if defined(_TARGET_OS_SOLARIS) && defined(_MODULE_TO_DOT_o)

         if (FP_CLASS(fp_file_idx) == Elf_File_Fp ||
             FP_CLASS(fp_file_idx) == Unknown_Fp) {

            if (srch_elf_file_for_module_tbl(module_attr_idx, fp_file_idx)) {
#ifdef KEY /* Bug 5089 */
	       *mod_file_ptr_ptr = mod_file_ptr;
	       *output_fp_file_idx = fp_file_idx;
	       return TRUE;
#else /* KEY Bug 5089 */
               found	= TRUE;
	       goto FOUND;
#endif /* KEY Bug 5089 */
            }

            /* Either this is not an elf file, or we really didn't find it. */
            /* If this is an Elf file, it will be marked as such by routine */
            /* srch_elf_file_for_module_tbl.                                */

         }
# endif

         switch (FP_CLASS(fp_file_idx)) {
         case Mod_File_Fp:

            if (on_off_flags.module_to_mod && !inline_search) {
               file_name_ptr = NULL;
               file_name_ptr = strrchr(FP_NAME_PTR(fp_file_idx), SLASH);

               if (file_name_ptr == NULL) {  /* No path before name */
                  file_name_ptr = FP_NAME_PTR(fp_file_idx);
               }
               else {
                  ++file_name_ptr;  /* Skip slash */
               }

               if (strncmp(file_name, file_name_ptr, fn_length) == 0) {

                  /* Found file_name.mod */

                  mod_file_ptr = open_module_file(module_attr_idx, fp_file_idx);

                  if (mod_file_ptr == NULL) {       /* Not able to open file. */
                     continue;                      /* Try the next file.     */
                  }

                  if (srch_for_module_tbl(module_attr_idx,
                                          &fp_module_idx,
                                          fp_file_idx,
                                          0,
                                          mod_file_ptr)) {
#ifdef KEY /* Bug 5089 */
		     *mod_file_ptr_ptr = mod_file_ptr;
		     *output_fp_file_idx = fp_file_idx;
		     return TRUE;
#else /* KEY Bug 5089 */
                     found	= TRUE;
		     goto FOUND;
#endif /* KEY Bug 5089 */
                  }
               }
            }
            break;

         case File_Fp:
         case Archive_File_Fp:
         case Unknown_Fp:         /* Look for modules in the Non-Elf files. */

            mod_file_ptr = open_module_file(module_attr_idx, fp_file_idx);

            if (mod_file_ptr == NULL) {       /* Not able to open file. */
               continue;                      /* Try the next file.     */
            }

            if (FP_OFFSET(fp_file_idx) > 0) {

               /* Assume this must be set to File_Fp or Archive_Fp.   */

               archive = (FP_CLASS(fp_file_idx) == Archive_File_Fp);
            }
            else {  /* we don't know what kind of file this is yet. */
#ifdef KEY /* Bug 5089 */
	       char ar_string[SARMAG];
	       int num_recs_read;
#endif /* KEY Bug 5089 */

               for (num_recs_read = 0; num_recs_read < SARMAG; num_recs_read++){
                  ar_string[num_recs_read] = '\n';
               }

               num_recs_read = fread((char *) ar_string, 
                                     sizeof(char),
                                     (size_t) SARMAG, 
                                     mod_file_ptr);
            
               if (num_recs_read == (size_t) SARMAG){
                   archive = (strncmp(ar_string, ARMAG, (size_t) SARMAG) == 0);
               }
               else {
                  archive = FALSE;
               }

               if (archive) {
                  FP_CLASS(fp_file_idx)	= Archive_File_Fp;
               }
               else {
                  FP_CLASS(fp_file_idx)	= File_Fp;

                  if (!FSEEK(mod_file_ptr, 0, SEEK_SET)) {

                     /* Reset to file start failed.  Skip to next file. */

                     fclose(mod_file_ptr);
                     fp_file_idx	= FP_NEXT_FILE_IDX(fp_file_idx);
                     continue;
                  }
               }
            }

            if (!archive) {

               if (FP_OFFSET(fp_file_idx) > 0 &&
                   !FSEEK(mod_file_ptr, FP_OFFSET(fp_file_idx), SEEK_CUR)) {

                  /* Seek failed.  Try the next file. */

                  fclose(mod_file_ptr);
                  fp_file_idx	= FP_NEXT_FILE_IDX(fp_file_idx);
                  continue; 
               }

               if (srch_for_module_tbl(module_attr_idx,
                                       &fp_module_idx,
                                       fp_file_idx,
                                       0,
                                       mod_file_ptr)) {
#ifdef KEY /* Bug 5089 */
		  *mod_file_ptr_ptr = mod_file_ptr;
		  *output_fp_file_idx = fp_file_idx;
		  return TRUE;
#else /* KEY Bug 5089 */
                  found	= TRUE;
		  goto FOUND;
#endif /* KEY Bug 5089 */
               }
            }
            else if (srch_ar_file_for_module_tbl(module_attr_idx,
                                                 &fp_module_idx,
                                                 fp_file_idx,
                                                 mod_file_ptr)) {
#ifdef KEY /* Bug 5089 */
	       *mod_file_ptr_ptr = mod_file_ptr;
	       *output_fp_file_idx = fp_file_idx;
	       return TRUE;
#else /* KEY Bug 5089 */
               found	= TRUE;
	       goto FOUND;
#endif /* KEY Bug 5089 */
            }

            fclose(mod_file_ptr);
            break;

         default:
            break;

         }  /* end switch */
      }
      fp_file_idx	= FP_NEXT_FILE_IDX(fp_file_idx);
   }
#ifdef KEY /* Bug 5089 */
   *mod_file_ptr_ptr = mod_file_ptr;
   *output_fp_file_idx = fp_file_idx;
   return FALSE;
}
#endif /* KEY Bug 5089 */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Open and search the input file looking for a module info table.       *|
|*									      *|
|* Input parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	TRUE if program unit is found.                                        *|
|*									      *|
\******************************************************************************/
boolean	find_prog_unit_tbl(int	module_attr_idx)

{
#ifndef KEY /* Bug 5089 */
   boolean	archive;
   char		ar_string[SARMAG];
#endif /* KEY Bug 5089 */
   boolean	found		= FALSE;
#ifndef KEY /* Bug 5089 */
   char		file_name[40];
   char	       *file_name_ptr;
   int		fn_length;
#endif /* KEY Bug 5089 */
   int		fp_file_idx;
   int		fp_module_idx;
   int		ga_idx;
   FILE	       *mod_file_ptr;
   int		name_idx;
#ifndef KEY /* Bug 5089 */
   int		next_fp_module_idx;
   int		num_recs_read;
#endif /* KEY Bug 5089 */
   boolean	save_keep_module_procs;


   TRACE (Func_Entry, "find_prog_unit_tbl", NULL);

   alternate_entry		= FALSE;
   save_keep_module_procs	= keep_module_procs;

   if (ATP_PGM_UNIT(module_attr_idx) == Module) {
      inline_search	= FALSE;
   }
   else if (ATP_PROC(module_attr_idx) == Module_Proc ||
            ATP_PROC(module_attr_idx) == Intern_Proc) {

      /* Don't waste time searching for these.  The Intern_Proc would */
      /* be an internal error situation and the module_proc is not    */
      /* available because the module was not compiled with modinline */

      goto DONE;
   }
   else {
      inline_search	= TRUE;
   }

   /* Finding the module shortcut:  Once we see a module or write out */
   /* a module, we retain the information as to file name and offset  */
   /* into file, so that we can quickly find it again.  We search the */
   /* global name table to find the module and then see if we have    */
   /* file information about it.                                      */

   if (
#ifdef KEY /* Bug 5089 */
      /*
       * If we're looking for an intrinsic module, don't search the global
       * name table unless -intrinsic_module_gen was set, because ordinarily
       * a module defined in the current compilation cannot be intrinsic.
       */
      (on_off_flags.intrinsic_module_gen || !AT_IS_INTRIN(module_attr_idx)) &&
#endif /* KEY Bug 5089 */
      srch_global_name_tbl(AT_OBJ_NAME_PTR(module_attr_idx),
                            AT_NAME_LEN(module_attr_idx),
                            &name_idx)) {
      ga_idx	= GN_ATTR_IDX(name_idx);

      if (GA_OBJ_CLASS(ga_idx) == Common_Block) {
         ga_idx	= GAC_PGM_UNIT_IDX(ga_idx);
      }

      if (ga_idx != NULL_IDX && GAP_FP_IDX(ga_idx) != NULL_IDX) {

         /* The file and the position in that file for this module are known */
         /* already, so just open the file and seek to that position.        */

         fp_module_idx	= GAP_FP_IDX(ga_idx);
         fp_file_idx	= FP_FILE_IDX(fp_module_idx);
         mod_file_ptr	= open_module_file(module_attr_idx,fp_file_idx);
         found		= (mod_file_ptr == NULL) ? FALSE :
                                        read_module_tbl_header(module_attr_idx,
                                                               fp_module_idx,
                                                               mod_file_ptr);

         if (found) {
            ATP_IN_CURRENT_COMPILE(module_attr_idx) = 
                           FP_CLASS(fp_module_idx) == Current_Compile_Fp;
            goto FOUND;
         }
         else {

            /* There's a potential file problem here.  If this is supposed */
            /* to be in the current compilation, issue a LIMIT.  Something */
            /* is wrong.  Otherwise issue a not found error.  We could get */
            /* fancy and redo the search again.  Maybe later.  (KAY)       */

            if (FP_CLASS(fp_module_idx) == Current_Compile_Fp &&
                !inline_search) {
               PRINTMSG(AT_DEF_LINE(module_attr_idx), 1249, Limit,
                        AT_DEF_COLUMN(module_attr_idx),
                        AT_OBJ_NAME_PTR(module_attr_idx));
            }
            else {
               goto ERROR;
            }
         }
      }
      else if (ATP_PGM_UNIT(module_attr_idx) == GAP_PGM_UNIT(ga_idx) && 
               ATP_PGM_UNIT(module_attr_idx) == Module &&
               !inline_search && GA_DEFINED(GN_ATTR_IDX(name_idx))) {
 
         /* Found this name in the global name table.  Check to see if it */
         /* is defined.  If so, there must have been scoping problems or  */
         /* errors (if not a MODULE), so no program unit was written out. */

         goto ERROR;
      }
   }

#ifdef KEY /* Bug 5089 */
   if (inline_search) {
     (void) look_for_module(module_attr_idx, inline_path_idx, &mod_file_ptr,
       &fp_file_idx);
   }
   else {
     /* Normally look first in nonintrinsic directories, then in intrinsic
      * directories. But if qualified with "intrinsic" or "non_intrinsic"
      * in the "use" statement, look in only the appropriate place.
      *
      * F2003 C1110 (R1109) says "A scoping unit shall not access an intrinsic
      * module and a nonintrinsic module of the same name." This case is
      * obviously a violation:
      *
      *   use, intrinsic :: xyz !sets AT_IS_INTRIN
      *   use, non_intrinsic :: xyz !sets ATT_NON_INTRIN
      *
      * But this case is a violation only if the "use" without either
      * "intrinsic" or "non_intrinsic" finds a non-intrinsic module:
      *
      *   use :: xyz !sets ATT_NO_MODULE_NATURE
      *   use, intrinsic :: xyz !sets AT_IS_INTRIN
      *
      * And this case is a violation only if the "use" without either
      * "intrinsic" or "non_intrinsic" finds an intrinsic module:
      *
      *   use :: xyz !sets ATT_NO_MODULE_NATURE
      *   use, non_intrinsic :: xyz !sets AT_IS_INTRIN
      */
     boolean violation = FALSE;
     boolean found_intrinsic = FALSE;
     if (AT_IS_INTRIN(module_attr_idx) && ATT_NON_INTRIN(module_attr_idx)) {
       violation = TRUE;
     }
     else if (ATT_NO_MODULE_NATURE(module_attr_idx)) {
       found = look_for_module(module_attr_idx, module_path_idx, &mod_file_ptr,
         &fp_file_idx);
       if (found) {
         if (AT_IS_INTRIN(module_attr_idx)) {
	   violation = TRUE;
	 }
       }
       else {
	 found_intrinsic = found = look_for_module(module_attr_idx,
	   intrinsic_module_path_idx, &mod_file_ptr, &fp_file_idx);
	 if (found_intrinsic && ATT_NON_INTRIN(module_attr_idx)) {
	   violation = TRUE;
	 }
       }
     }
     else if (ATT_NON_INTRIN(module_attr_idx)) { // non_intrinsic alone
       found = look_for_module(module_attr_idx, module_path_idx, &mod_file_ptr,
         &fp_file_idx);
     }
     else if (AT_IS_INTRIN(module_attr_idx)) { // intrinsic alone
       found_intrinsic = found = look_for_module(module_attr_idx,
         intrinsic_module_path_idx, &mod_file_ptr, &fp_file_idx);
     }
     else {
       PRINTMSG(AT_DEF_LINE(module_attr_idx), 1044, Internal,
		AT_DEF_COLUMN(module_attr_idx),
		"ATT_NO_MODULE_NATURE should be set");
     }

     /* We no longer need to remember which module-nature keywords appeared in
      * various "use" statements for this module in this scope; henceforth, we
      * need to remember what kind of module we found. If it was intrinsic,
      * we must mark it as such, and must use special external (linker) name */
     AT_IS_INTRIN(module_attr_idx) = found_intrinsic;
     ATT_NON_INTRIN(module_attr_idx) = FALSE;
     ATT_NO_MODULE_NATURE(module_attr_idx) = FALSE;
     if (found_intrinsic) {
       MAKE_EXTERNAL_NAME(module_attr_idx, AT_NAME_IDX(module_attr_idx), 
	 AT_NAME_LEN(module_attr_idx));
     }

     if (violation) {
       PRINTMSG(AT_DEF_LINE(module_attr_idx), 1678, Error,
	 AT_DEF_COLUMN(module_attr_idx), AT_OBJ_NAME_PTR(module_attr_idx));
     }
   }

   if (found) {
     goto FOUND;
   }
#else /* KEY Bug 5089 */
   fp_file_idx = (inline_search) ? inline_path_idx : module_path_idx;
#endif /* KEY Bug 5089 */

ERROR:

   /* Program unit is not found.  If this is a MODULE, issue an error.  If   */
   /* If we're searching for a program unit for inlining, just return FALSE. */

   if (!AT_DCL_ERR(module_attr_idx) && !inline_search) {
      AT_DCL_ERR(module_attr_idx) = TRUE;

      if (compare_names(AT_OBJ_NAME_LONG(module_attr_idx),
                        AT_NAME_LEN(module_attr_idx),
                        AT_OBJ_NAME_LONG(SCP_ATTR_IDX(MAIN_SCP_IDX)),
                        AT_NAME_LEN(SCP_ATTR_IDX(MAIN_SCP_IDX))) == 0) {

         /* Trying to include the current module */

         PRINTMSG(AT_DEF_LINE(module_attr_idx), 1027, Error,
                  AT_DEF_COLUMN(module_attr_idx),
                  AT_OBJ_NAME_PTR(module_attr_idx));
      }
      else {
         PRINTMSG(AT_DEF_LINE(module_attr_idx), 292, Error,
                  AT_DEF_COLUMN(module_attr_idx),
                  AT_OBJ_NAME_PTR(module_attr_idx));
      }
   }

   return(FALSE);

FOUND:

   if (!inline_search && (cif_flags & BASIC_RECS)) {
      cif_use_module_rec(module_attr_idx,
                         fp_file_idx,
                         FALSE);
   }

   if (dump_flags.mod_version) {  /* Print out the module version */

      printf("Module %s is compiled with module version number %d. \n",
             AT_OBJ_NAME_PTR(module_attr_idx), MD_VERSION_NUM);

   }

   /* This module is in this compilation unit, but it has compile time errors.*/
   /* Issue a not found message - but read in the module for error recovery.  */

   if (MD_HAS_ERRORS) {

      if (!inline_search) {
         PRINTMSG(AT_DEF_LINE(module_attr_idx), 894, Error,
                  AT_DEF_COLUMN(module_attr_idx),
                  AT_OBJ_NAME_PTR(module_attr_idx));
         AT_DCL_ERR(module_attr_idx) = TRUE;
      }
      else {
         found = FALSE;
      }
   }

   if (MD_VERSION_NUM > MD_CURRENT_VERSION) {
      PRINTMSG(AT_DEF_LINE(module_attr_idx), 1181, Error,
               AT_DEF_COLUMN(module_attr_idx),
               FP_NAME_PTR(fp_file_idx));
      AT_DCL_ERR(module_attr_idx) = TRUE;
      found = FALSE;
   }

   if (MD_VERSION_NUM <= MD_LAST_3_0_VERSION) {
      PRINTMSG(AT_DEF_LINE(module_attr_idx), 1181, Error,
               AT_DEF_COLUMN(module_attr_idx),
               FP_NAME_PTR(fp_file_idx));
      AT_DCL_ERR(module_attr_idx) = TRUE;
      found = FALSE;
   }
   else if (MD_VERSION_NUM <= MD_LAST_4_0_VERSION) {

      /* Warning that this is an older module version and will not  */
      /* be supported in the following release.                     */

      PRINTMSG(AT_DEF_LINE(module_attr_idx), 1157, Warning,
               AT_DEF_COLUMN(module_attr_idx),
               FP_NAME_PTR(fp_file_idx));
   }


#if defined(_HOST32) && defined(_TARGET64)

   if (MD_TARGET != target_os) {

      if (ATP_PGM_UNIT(module_attr_idx) == Module) {
         PRINTMSG(AT_DEF_LINE(module_attr_idx), 1055, Error,
                  AT_DEF_COLUMN(module_attr_idx),
                  FP_NAME_PTR(fp_file_idx));
         AT_DCL_ERR(module_attr_idx) = TRUE;
         found = FALSE;
      }
      else {
         PRINTMSG(AT_DEF_LINE(module_attr_idx), 1246, Error,
                  AT_DEF_COLUMN(module_attr_idx),
                  FP_NAME_PTR(fp_file_idx));
         AT_DCL_ERR(module_attr_idx) = TRUE;
         found = FALSE;
      }
   }
# else

   if (MD_TARGET != target_os) {

      if (ATP_PGM_UNIT(module_attr_idx) == Module) {
         PRINTMSG(AT_DEF_LINE(module_attr_idx), 725, Error,
                  AT_DEF_COLUMN(module_attr_idx),
                  AT_OBJ_NAME_PTR(module_attr_idx));
         AT_DCL_ERR(module_attr_idx) = TRUE;
      }
      else {
         PRINTMSG(AT_DEF_LINE(module_attr_idx), 1247, Error,
                  AT_DEF_COLUMN(module_attr_idx),
                  AT_OBJ_NAME_PTR(module_attr_idx));
         AT_DCL_ERR(module_attr_idx) = TRUE;
      }
   }
# endif

   if (!FP_SYSTEM_FILE(fp_file_idx)) {

      if (MD_CF77TYPES != cmd_line_flags.s_cf77types) {
         PRINTMSG(AT_DEF_LINE(module_attr_idx), 1248, Error,
                  AT_DEF_COLUMN(module_attr_idx),
                  AT_OBJ_NAME_PTR(module_attr_idx),
                  "-s i");
         AT_DCL_ERR(module_attr_idx) = TRUE;
      }
      else if (MD_DEFAULT32 != cmd_line_flags.s_default32) {
         PRINTMSG(AT_DEF_LINE(module_attr_idx), 1248, Error,
                  AT_DEF_COLUMN(module_attr_idx),
                  AT_OBJ_NAME_PTR(module_attr_idx),
                  "-s default32");
         AT_DCL_ERR(module_attr_idx) = TRUE;
      }
      else if (MD_DEFAULT64 != cmd_line_flags.s_default64) {
         PRINTMSG(AT_DEF_LINE(module_attr_idx), 1248, Error,
                  AT_DEF_COLUMN(module_attr_idx),
                  AT_OBJ_NAME_PTR(module_attr_idx),
                  "-s default64");
         AT_DCL_ERR(module_attr_idx) = TRUE;
      }
      else if (MD_FLOAT64 != cmd_line_flags.s_float64) {
         PRINTMSG(AT_DEF_LINE(module_attr_idx), 1248, Error,
                  AT_DEF_COLUMN(module_attr_idx),
                  AT_OBJ_NAME_PTR(module_attr_idx),
                  "-s float64");
         AT_DCL_ERR(module_attr_idx) = TRUE;
      }
      else if (MD_DEFAULT_INTEGER_TYPE != INTEGER_DEFAULT_TYPE) {
#ifdef KEY /* Bug 7359 */
         PRINTMSG(AT_DEF_LINE(module_attr_idx), 623, Warning,
                  AT_DEF_COLUMN(module_attr_idx),
                  AT_OBJ_NAME_PTR(module_attr_idx));
#else /* KEY Bug 7359 */
         PRINTMSG(AT_DEF_LINE(module_attr_idx), 623, Error,
                  AT_DEF_COLUMN(module_attr_idx),
                  AT_OBJ_NAME_PTR(module_attr_idx));
#endif /* KEY Bug 7359 */
         AT_DCL_ERR(module_attr_idx) = TRUE;
      }

      if (MD_ENABLE_DOUBLE_PRECISION != on_off_flags.enable_double_precision) {
         PRINTMSG(AT_DEF_LINE(module_attr_idx), 618, Error,
                  AT_DEF_COLUMN(module_attr_idx),
                  AT_OBJ_NAME_PTR(module_attr_idx));
         AT_DCL_ERR(module_attr_idx) = TRUE;
      }

#if defined(_ACCEPT_CMD_a_dalign)

      if (MD_DALIGN != cmd_line_flags.dalign) {
         PRINTMSG(AT_DEF_LINE(module_attr_idx), 1011, Error,
                  AT_DEF_COLUMN(module_attr_idx),
                  AT_OBJ_NAME_PTR(module_attr_idx));
         AT_DCL_ERR(module_attr_idx) = TRUE;
      }
# endif

   }

   if (FP_CLASS(fp_file_idx) != Elf_File_Fp) {

      /* Elf files were read in during the elf search */

      if (found) {
         if (!read_in_module_tbl(fp_file_idx, 
                                 module_attr_idx, 
                                 mod_file_ptr, 
                                 NULL)) {
            found	= FALSE;
         }
      }
      fclose(mod_file_ptr);
   }

   if (ATP_PGM_UNIT(module_attr_idx) != Module) {

      if (found && AT_ATTR_LINK(module_attr_idx) != NULL_IDX) {
         ATP_FIRST_SH_IDX(module_attr_idx) = 
                          ATP_FIRST_SH_IDX(AT_ATTR_LINK(module_attr_idx));
      }
      else {
         found = FALSE;
      }
      TBL_FREE(mod_link_tbl);
   }

DONE:

   alternate_entry	= FALSE;
   keep_module_procs	= save_keep_module_procs;
   inline_search	= FALSE;

   TRACE (Func_Exit, "find_prog_unit_tbl", NULL);

   return(found);

}  /* find_prog_unit_tbl */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Open the module file for reading.  Issue an error if there are        *|
|*	problems.							      *|
|*									      *|
|* Input parameters:							      *|
|*	module_attr_idx -> Attr index describing module being USEd.	      *|
|*	fp_file_idx	-> Index to file path table of file to open.	      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	the file pointer to the file just opened.			      *|
|*									      *|
\******************************************************************************/
static	FILE	*open_module_file(int	module_attr_idx,
				  int	fp_file_idx)

{
   char		*lib_error;
   FILE		*mod_file_ptr;


   TRACE (Func_Entry, "open_module_file", NULL);

   mod_file_ptr = fopen(FP_NAME_PTR(fp_file_idx), "rb");

   if (mod_file_ptr == NULL_IDX) {

      if (FP_FILE_IDX(fp_file_idx) != NULL_IDX &&
          FP_CLASS(FP_FILE_IDX(fp_file_idx)) != Directory_Fp) {
  
         /* Don't issue an error for individual files in directories. */

         lib_error = strerror(errno);

         PRINTMSG(AT_DEF_LINE(module_attr_idx), 1132, Warning,
                  AT_DEF_COLUMN(module_attr_idx),
                  FP_NAME_PTR(fp_file_idx),
                  lib_error,
                  AT_OBJ_NAME_PTR(module_attr_idx));
      }
      FP_SRCH_THE_FILE(fp_file_idx)	= FALSE;
      mod_file_end_offset		= 0;
   }
   else {

      /* Do not use FSEEK macro here, because this is where we are       */
      /* setting mod_file_end_offset, which is used by the FSEEK macro.  */

      fseek(mod_file_ptr, 0, SEEK_END); /* Seek to end of file */

      mod_file_end_offset = ftell(mod_file_ptr);

      fseek(mod_file_ptr, 0, SEEK_SET); /* Seek to start of file */
   }

   TRACE (Func_Exit, "open_module_file", NULL);

   return(mod_file_ptr);

}  /* open_module_file */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	srch_ar_file_for_module_tbl searches archival libraries for module    *|
|*	information tables.  This routine can search archival libraries that  *|
|*	have the Cray format or the sparc format.  (These differ for member   *|
|*	names over 15 characters.)  This will search one archive file for a   *|
|*	specific module name.  If it finds the module, mod_file_ptr will be   *|
|*	left pointing to the start of the module and TRUE will be returned.   *|
|*	If it finds a problem with the file or it doesn't find the module     *|
|*	FALSE will be returned.                                               *|
|*									      *|
|* Input parameters:							      *|
|*	module_attr_idx	-> Attr index of module to search for.		      *|
|*	fp_file_idx	-> File path table index to entry describing archive  *|
|*			   library to seach.                                  *|
|*	fp_module_idx	-> If this a resumed search, this is a file path      *|
|*		           table index for the last module found in this      *|
|*		           library.  If no modules have been found or if this *|
|*		           is not a resumed search, then this index is NULL.  *|
|*	module_file_ptr	-> FILE pointer to archive library to read from.      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	TRUE  -> The module has been found.  mod_file_ptr will be set to      *|
|*	         start of module information table.                           *|
|*	FALSE -> The module was not found.                                    *|
|*									      *|
\******************************************************************************/
static	boolean	srch_ar_file_for_module_tbl(int		 module_attr_idx,
					    int		*fp_module_idx,
					    int		 fp_file_idx,
					    FILE	*mod_file_ptr)

{
   typedef	struct		ar_hdr	ar_hdr_type;

   		ar_hdr_type	ar_header;
   static	char		ar_name[256];
   		boolean		found;
   		int		idx;
   		boolean		in_middle_of_file;
   		long_type	member_start_offset;
#ifdef KEY /* Bug 10177 */
   		int		name_length = 0;
#else /* KEY Bug 10177 */
   		int		name_length;
#endif /* KEY Bug 10177 */
   		long_type	name_tbl_offset;
   		int		num_recs_read;
   		long_type	offset;
   		int		size;


   TRACE (Func_Entry, "srch_ar_file_for_module_tbl", NULL);

   /* At entry, we are either starting the search in a new archive library */
   /* or resuming the search from a given offset.  If we are resuming the  */
   /* search we are pointing to whatever follows a module information      */
   /* table entry.  This may be a new archive member or a new Cray PDT     */
   /* member.  If FP_OFFSET for the file is non-zero we are resuming the   */
   /* search.  If the search is being resumed, then mod_file_ptr is set to */
   /* the start of the file.  It needs to skip ARMAG, the archive magic    */
   /* header string.  If this is a new file being searched, mod_file_ptr   */
   /* is set just past ARMAG.                                              */

   offset		= FP_OFFSET(fp_file_idx);

   if (offset > 0) {

      if (!FSEEK(mod_file_ptr, SARMAG, SEEK_SET)) {
         return(FALSE);     /* Seek failed.  Exit to look in another file. */
      }
      in_middle_of_file	= TRUE;
   }
   else {
      in_middle_of_file	= FALSE;
   }

   found		= FALSE;

   /* name_tbl_offset contains the offset in the archive of the // table.  */
   /* The // archive holds names of archive members which are greater than */
   /* 15 characters.  This table always preceeds all normal member files.  */
   /* The only table which may preceed this table, is the symbol table,    */
   /* a special table created if there are relocatables in the archive.    */
   /* This special table can be ignored by this search.  The // table is   */
   /* found in archives for sparcs systems.                                */

   name_tbl_offset	= 0;

   while (!found) {   /* Loop through archival members. */
      num_recs_read	= fread(&ar_header,
                                AR_HDR_SIZE, /* Macro from ar.h */
                                1,
                                mod_file_ptr);

      if (feof(mod_file_ptr)) {
         FP_SRCH_THE_FILE(fp_file_idx)  = FALSE;
         break;
      }

      member_start_offset	= ftell(mod_file_ptr);

      if (num_recs_read != 1) {
         PRINTMSG(AT_DEF_LINE(module_attr_idx), 726, Error,
                  AT_DEF_COLUMN(module_attr_idx),
                  AT_OBJ_NAME_PTR(module_attr_idx));
         FP_SRCH_THE_FILE(fp_file_idx)	= FALSE;
         break;
      }

      /* Calculate actual size of member.  */

      size	= (size_t) atoi(&ar_header.ar_size[0]);

      /* Skip known non module files within the archive.  On */
      /* Solaris, the string table is //.  Keep the offset   */
      /* into the // table, in case we hit names that are    */
      /* longer than 15 chars and need to access them.  On   */
      /* Crays, skip .relotable, .cldtable and .directory.   */

      if (strncmp (ar_header.ar_name, "// ", 3) == 0) {
         name_tbl_offset = member_start_offset;

         if (!FSEEK(mod_file_ptr, size, SEEK_CUR)) {
            break;
         }
      }
      else if (in_middle_of_file && 
              (size + AR_HDR_SIZE + member_start_offset) < offset) {

         /* If this is a resumed search we need to find which member we */
         /* are searching in.  We do this by reading up each member and */
         /* checking to see if the offset falls within that particular  */
         /* member.  We need to do this, because we cannot tell how     */
         /* far we are from the end of the member when we resume.  We   */
         /* need the members header to tell us the size of the member.  */
         /* Once we are in the correct member, we calculate the size    */
         /* left in the member by taking the offset to be resumed to    */
         /* and subtracting off the offset of the start of the member.  */
         /* Then we take the size of the member and subtract off how    */
         /* far we are inside the member.  In this else clause, we      */
         /* haven't reached the correct member yet, so skip this one.   */

         if (!FSEEK(mod_file_ptr, size, SEEK_CUR)) {
            break;
         }
      }
      else if ((strncmp (ar_header.ar_name, ".directory", 10) == 0) ||
                (strncmp (ar_header.ar_name, ".cldtable", 9) == 0) ||
                (strncmp (ar_header.ar_name, ".relotable", 10) == 0)) {

         if (!FSEEK(mod_file_ptr, size, SEEK_CUR)) {
            break;
         }
      }
      else {  /* Assume a searchable member. */

         if (in_middle_of_file) {

            /* We have found the member where we are going to resume */
            /* the search.  Set size to how much of this member we   */
            /* have to search through yet and then set mod_file_ptr  */
            /* to the point where we resume the search.              */

            size		= size - (offset - member_start_offset);
            in_middle_of_file	= FALSE;

            if (!FSEEK(mod_file_ptr, offset, SEEK_SET)) {
               break;
            }
         }
         else {  /* Find the name of the member. */

            if (strncmp(ar_header.ar_name, "#1/", 3) == 0) {

               /* On Crays, if the member name is greater than 15 characters */
               /* ar_name contains #1/length of name.  The name follows      */
               /* ar_header before the member starts.  This section reads in */
               /* the name.                                                  */

               ar_header.ar_name[sizeof (ar_header.ar_name) - 1]= '\0';
               name_length	= (size_t) atoi(&ar_header.ar_name[3]);
               num_recs_read	= fread(&ar_name, name_length, 1, mod_file_ptr);

               /* Subtract off any filename chars that may appear after */
               /* the header.  This is a Cray specific thing.           */

               size			= size - name_length;
               member_start_offset	= ftell(mod_file_ptr);

               if (num_recs_read != 1) {
                  PRINTMSG(AT_DEF_LINE(module_attr_idx), 726, Error,
                           AT_DEF_COLUMN(module_attr_idx),
                           AT_OBJ_NAME_PTR(module_attr_idx));
                  FP_SRCH_THE_FILE(fp_file_idx)	= FALSE;
                  break;
               }
            }

            else if (ar_header.ar_name[0] == '/') {

               /* On sparc systems, if the name is longer than 15 characters */
               /* ar_name contains /number, where number is an index into    */
               /* the // string table of the name of the member.  This       */
               /* section reads up that name and determines its length.      */

               switch (ar_header.ar_name[1]) {
               case '0':
               case '1':
               case '2':
               case '3':
               case '4':
               case '5':
               case '6':
               case '7':
               case '8':
               case '9':

                  /* This is an index to the // entry of the name.  */
                  /* The name is longer than 15 characters.         */
                  /* The // archive member (name_tbl_offset) should */
                  /* always be first or second before any named     */
                  /* normal members.  If we don't have a // archive */
                  /* member then there's a file problem, so issue   */
                  /* an error.                                      */

                  if (name_tbl_offset == 0) {
                     PRINTMSG(AT_DEF_LINE(module_attr_idx), 726, Error,
                              AT_DEF_COLUMN(module_attr_idx),
                              AT_OBJ_NAME_PTR(module_attr_idx));
                     FP_SRCH_THE_FILE(fp_file_idx)	= FALSE;
                     goto EXIT;
                  }

                  idx	= (size_t) atoi(&ar_header.ar_name[1]);

                  if (!FSEEK(mod_file_ptr, (name_tbl_offset + idx), SEEK_SET)) {
                     goto EXIT;
                  }

                  if (fgets(ar_name, sizeof(ar_name), mod_file_ptr) == NULL) {
                     goto EXIT;
                  }

                  if (!FSEEK(mod_file_ptr, member_start_offset, SEEK_SET)) {
                     goto EXIT;
                  }

                  for (idx = 0; idx < sizeof(ar_name); idx++) {

                     /* ar has a BUG. Not all file names are '/' terminated */
                     /* some are blank terminated. */

                     if (ar_name[idx] == '/' ||
                         ar_name[idx] == ' ') {
                        name_length = idx;
                        break;
                     }
                     else if (ar_name[idx] == '\n') {
                        break;
                     }
                  }
                  break;
               }
            }
            else {

               /* This is a plain old < 15 characters name of a member. */
               /* Determine the length of the name.                     */

               (void) memcpy ((void *)ar_name, 
                              (void *)&ar_header.ar_name,
                              sizeof(ar_header.ar_name));
            
               /* Determine the actual length of this file name */

               for (idx = 0; idx < sizeof(ar_header.ar_name); idx++) {

                  /* ar has a BUG. Not all file names are '/' terminated */
                  /* some are blank terminated. */

                  if (ar_name[idx] == '/' ||
                      ar_name[idx] == ' ') {
                     name_length = idx;
                     break;
                  }
               }
            }

# if defined(_MODULE_TO_DOT_o)
            if (on_off_flags.module_to_mod && !FP_SYSTEM_FILE(fp_file_idx)) {

               /* This module is created on systems that do not put their   */
               /* module information in the .o file, but in a .mod file.    */
               /* Play it safe while looking into archive files and only    */
               /* select out .mod files to search through.  Skip the rest.  */
   
               if (name_length < 5 ||
                  ar_name[name_length-1] != 'd' || 
                  ar_name[name_length-2] != 'o' || 
                  ar_name[name_length-3] != 'm' || 
                  ar_name[name_length-4] != '.') {

                  if (!FSEEK(mod_file_ptr, size, SEEK_CUR)) {
                     break;
                  }
                  size = 0;   /* Exit to next member. */
               }
            }

# elif !defined(_MODULE_TO_DOT_M)
            if (on_off_flags.module_to_mod) {

               /* This module is created on systems that do not put their   */
               /* module information in the .o file, but in a .mod file.    */
               /* Play it safe while looking into archive files and only    */
               /* select out .mod files to search through.  Skip the rest.  */
   
               if (name_length < 5 ||
                  ar_name[name_length-1] != 'd' || 
                  ar_name[name_length-2] != 'o' || 
                  ar_name[name_length-3] != 'm' || 
                  ar_name[name_length-4] != '.') {

                  if (!FSEEK(mod_file_ptr, size, SEEK_CUR)) {
                     break;
                  }
                  size = 0;   /* Exit to next member. */
               }
            }
# endif

# if defined(_MODULE_TO_DOT_M)

            /* This module is created on systems that do not put their   */
            /* module information in the .o file, but in a .M file.      */
            /* Play it safe while looking into archive files and only    */
            /* select out .M files to search through.  Skip the rest.    */

            if (name_length < 3 ||
               ar_name[name_length-1] != 'M' ||
               ar_name[name_length-2] != '.') {

               if (!FSEEK(mod_file_ptr, size, SEEK_CUR)) {
                  break;
               }
               size = 0;   /* Exit to next member. */
            }
# endif
         }

         found	= srch_for_module_tbl(module_attr_idx,
                                      fp_module_idx,
                                      fp_file_idx,
                                      size,
                                      mod_file_ptr);
      }

      /* Next archive header starts on an even byte boundary. */

      if (!found && (ftell(mod_file_ptr) & 01)) {

         if (!FSEEK(mod_file_ptr, 1L, SEEK_CUR)) {
            break;
         }
      }
   }
 
EXIT:

   TRACE (Func_Exit, "srch_ar_file_for_module_tbl", NULL);

   return(found);

}  /* srch_ar_file_for_module_tbl */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*									      *|
|* Input parameters:							      *|
|*      module_attr_idx -> Attr index of module being searched for.           *|
|*      fp_module_idx   -> Fp index to entry describing module.  This routine *|
|*                         changes the value as new modules are found.   This *|
|*                         is NULL if we haven't found any modules in this    *|
|*                         file yet.                                          *|
|*      fp_file_idx     -> Fp index to entry describing file containing module*|
|*      size		-> If this is an archive file, size if the size left  *|
|*                         of this member in the archive file.                *|
|*      mod_file_ptr    -> Ptr to open file holding module.                   *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	TRUE if we found the module.                                          *|
|*									      *|
\******************************************************************************/
static	boolean	srch_for_module_tbl(int		 module_attr_idx,
				    int		*fp_module_idx,
				    int		 fp_file_idx,
				    int		 size,
				    FILE	*mod_file_ptr)

{
   boolean	found		= FALSE;
   int		idx;
   long	       *mod_name_idx;
   int		name_len;
   int		num_recs_read;
   long_type	offset;


   TRACE (Func_Entry, "srch_for_module_tbl", NULL);

   /* On systems where the module table is buried in the .o files   */
   /* we have to search through the PDT loops for the module table. */
   /* If the module table is not buried in the .o files, but is its */
   /* own member, then this while loop will only be executed once.  */

# if defined(_DEBUG)

   if (dump_flags.pdt_dump) {
      print_fp(fp_file_idx);
   }
# endif

   while (!found) {

      if (FP_CLASS(fp_file_idx) == Archive_File_Fp && size <= 0) {

         /* Have reached end of this member.  Or we've gone off in the weeds */

         break;
      }

      offset		= ftell(mod_file_ptr);
      num_recs_read	= fread(&mit_header, 
                                MD_PDT_HEADER_BYTE_SIZE,
                                1,
                                mod_file_ptr);

      if (feof(mod_file_ptr)) {
   
         /* Found the end of this file, but didn't find the module */
         /* Try the next file in the list.                         */
   
         FP_SRCH_THE_FILE(fp_file_idx) = FALSE;
         break;
      }
   
      if (num_recs_read != 1) {
         PRINTMSG(AT_DEF_LINE(module_attr_idx), 726, Error,
                  AT_DEF_COLUMN(module_attr_idx),
                  AT_OBJ_NAME_PTR(module_attr_idx));
         FP_SRCH_THE_FILE(fp_file_idx)	= FALSE;
         break;
      }

      if (MD_PDT_HDR_TYPE != COMPILER_INFO_TABLE_TYPE) {
   
# if defined(_DEBUG)

         if (dump_flags.pdt_dump) {
            dump_pdt(mod_file_ptr);
         }
# endif
         /* Not a module information table.  Find the next loader */
         /* table in this file.  Reset to start of this table and */
         /* then seek to the end of this table.  If the header    */
         /* length is zero, we have stumbled on a bad file,       */
         /* perhaps another vendors mixed with ours???            */
   
         if (MD_PDT_HDR_LEN == 0 ||
             !FSEEK(mod_file_ptr, offset, SEEK_SET) ||
             !FSEEK(mod_file_ptr, (long) MD_PDT_HDR_LEN * TARGET_BYTES_PER_WORD,
                                  SEEK_CUR)) {
            FP_SRCH_THE_FILE(fp_file_idx) = FALSE;
            break;
         }

         /* Offset points to start of this table.  ftell returns */
         /* position of end of this table.  Decrease size by the */
         /* size of this table that we are skipping.             */

         if (FP_CLASS(fp_file_idx) == Archive_File_Fp) {
            size -= (ftell(mod_file_ptr) - offset);
          }
         continue;
      }

         /* Not a module information table.  Find the next loader */

      num_recs_read = fread(MD_AFTER_PDT,
                            MD_TBL_BYTE_SIZE - MD_PDT_HEADER_BYTE_SIZE,
                            1,
                            mod_file_ptr);
      
      if (feof(mod_file_ptr)) {

         /* Found the end of this file, but didn't find the module */
         /* Try the next file in the list.                         */

         FP_SRCH_THE_FILE(fp_file_idx) = FALSE;
         break;
      }

      if (num_recs_read != 1) {
         PRINTMSG(AT_DEF_LINE(module_attr_idx), 726, Error,
                  AT_DEF_COLUMN(module_attr_idx),
                  AT_OBJ_NAME_PTR(module_attr_idx));
         FP_SRCH_THE_FILE(fp_file_idx) = FALSE;
         break;
      }

# if defined(_DEBUG)

      if (dump_flags.pdt_dump) {
         dump_pdt(mod_file_ptr);
      }
# endif

      /* Found a module.  Save the information in the file_path_tbl. */
      /* If it matches we need the info, if it doesn't it will be    */
      /* easier to search for next time.                             */
   
      TBL_REALLOC_CK(file_path_tbl, 1);
      CLEAR_TBL_NTRY(file_path_tbl, file_path_tbl_idx);
      name_len	= MD_NAME_LEN;

      if (*fp_module_idx == NULL_IDX) {
         *fp_module_idx			= fp_file_idx;
         FP_MODULE_IDX(*fp_module_idx)	= file_path_tbl_idx;
         *fp_module_idx			= file_path_tbl_idx;
         FP_NAME_LEN(*fp_module_idx)	= name_len;
         FP_NAME_IDX(*fp_module_idx)	= str_pool_idx + 1;
         FP_FILE_IDX(*fp_module_idx)	= fp_file_idx;
         FP_OFFSET(*fp_module_idx)	= offset;
         FP_CLASS(*fp_module_idx)	= (inline_search) ? Inline_Fp:Module_Fp;
         mod_name_idx			= MD_NAME_LONG;

         TBL_REALLOC_CK(str_pool, WORD_LEN(name_len));

         for (idx = FP_NAME_IDX(*fp_module_idx); idx <= str_pool_idx; idx++) {
            str_pool[idx].name_long = *mod_name_idx;
               mod_name_idx++;
         }
      }
      else if (FP_OFFSET(*fp_module_idx) == -1) {

         /* Searching for this name.  Should be next in this file. */

         if (compare_names(AT_OBJ_NAME_LONG(module_attr_idx),
                           AT_NAME_LEN(module_attr_idx),
                           MD_NAME_LONG,
                           name_len) != 0) {

             /* Cute little problem here - kay */

         }
         else { /* Found module - update offset */
            FP_OFFSET(*fp_module_idx)	= offset;
         }
      }
      else {

        /* Found a module.  Do not have a list of modules. So make one. */

         FP_MODULE_IDX(*fp_module_idx)	= file_path_tbl_idx;
         *fp_module_idx			= file_path_tbl_idx;
         FP_NAME_LEN(*fp_module_idx)	= name_len;
         FP_NAME_IDX(*fp_module_idx)	= str_pool_idx + 1;
         FP_FILE_IDX(*fp_module_idx)	= fp_file_idx;
         FP_OFFSET(*fp_module_idx)	= offset;
         FP_CLASS(*fp_module_idx)	= (inline_search) ? Inline_Fp:Module_Fp;
         mod_name_idx			= MD_NAME_LONG;

         TBL_REALLOC_CK(str_pool, WORD_LEN(name_len));

         for (idx = FP_NAME_IDX(*fp_module_idx); idx <= str_pool_idx; idx++) {
            str_pool[idx].name_long = *mod_name_idx;
            mod_name_idx++;
         }
      }

      if (compare_names(AT_OBJ_NAME_LONG(module_attr_idx),
                        AT_NAME_LEN(module_attr_idx),
                        MD_NAME_LONG,
                        name_len) != 0) {

         /* Reset to start of this table and    */
         /* then seek to the end of this table. */

         if (!FSEEK(mod_file_ptr, offset, SEEK_SET) ||
              MD_PDT_HDR_LEN == 0 ||
             !FSEEK(mod_file_ptr, (long) MD_PDT_HDR_LEN * TARGET_BYTES_PER_WORD,
                                  SEEK_CUR)) {
            FP_SRCH_THE_FILE(fp_file_idx)	= FALSE;
            break;
         }

         /* Offset points to start of this table.  ftell returns */
         /* position of end of this table.  Decrease size by the */
         /* size of this table that we are skipping.             */

         if (FP_CLASS(fp_file_idx) == Archive_File_Fp) {
             size -= (ftell(mod_file_ptr) - offset);
         }
      }
      else {

         /* We found the module we are looking for.  Set FP_OFFSET */
         /* to the end of this module information table.  All      */
         /* modules that we have found before this module have     */
         /* been entered in the file path table so they can be     */
         /* found again really easy.  Thus we set FP_OFFSET to the */
         /* end of this table, because we have already searched    */
         /* and recorded all modules up to this point.  If we need */
         /* to search for a module and it is not already listed in */
         /* the file path table, we will resume our search at this */
         /* point.  MD_PDT_HDR_LEN is the length of this module    */
         /* information table.  It is kept in number of words and  */
         /* needs to be reset to bytes.                            */

         FP_OFFSET(fp_file_idx)	= offset + 
                                 ((long)MD_PDT_HDR_LEN * TARGET_BYTES_PER_WORD);
         found = TRUE;
      }
   }

   TRACE (Func_Exit, "srch_for_module_tbl", NULL);

   return(found);

} /* srch_for_module_tbl */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*									      *|
|* Input parameters:							      *|
|*      module_attr_idx -> Attr index of module being searched for.           *|
|*      fp_module_idx   -> Fp index to entry describing module.               *|
|*      mod_file_ptr    -> Ptr to open file holding module.                   *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	TRUE - if we found and read module header successfully.               *|
|*									      *|
\******************************************************************************/
static	boolean	read_module_tbl_header(int	module_attr_idx,
				       int	fp_module_idx,
				       FILE    *mod_file_ptr)
{
   boolean	found			= FALSE;
   int		num_recs_read;
   long_type	offset;


   TRACE (Func_Entry, "read_module_tbl_header", NULL);

   if (FP_OFFSET(fp_module_idx) == -1) {

      /* This file has just been written out - Don't */
      /* know where the table is, so find it.        */

      offset = 0;

      while (!feof(mod_file_ptr)) {
         num_recs_read = fread(&mit_header,
                               MD_PDT_HEADER_BYTE_SIZE,
                               1,
                               mod_file_ptr);

         if (num_recs_read != 1 || feof(mod_file_ptr)) {
            fp_module_idx	= NULL_IDX;
            break;
         }

         if (MD_PDT_HDR_TYPE != COMPILER_INFO_TABLE_TYPE) {
            offset	= offset + ((long) MD_PDT_HDR_LEN * 
                                           TARGET_BYTES_PER_WORD);

            if (!FSEEK(mod_file_ptr,
                       (((long) MD_PDT_HDR_LEN * TARGET_BYTES_PER_WORD) - 
                         (long) MD_PDT_HEADER_BYTE_SIZE), SEEK_CUR)) {
               fp_module_idx = NULL_IDX;
               break;
            }
            continue;
         }

         num_recs_read = fread(MD_AFTER_PDT,
                               MD_TBL_BYTE_SIZE - MD_PDT_HEADER_BYTE_SIZE,
                               1,
                               mod_file_ptr);

         if (num_recs_read != 1 || feof(mod_file_ptr)) {
            fp_module_idx = NULL_IDX;
            break;
         }

         if (compare_names(AT_OBJ_NAME_LONG(module_attr_idx),
                           AT_NAME_LEN(module_attr_idx),
                           MD_NAME_LONG,
                           MD_NAME_LEN) == 0) {
            break;
         }

         offset	= offset + ((long) MD_PDT_HDR_LEN * TARGET_BYTES_PER_WORD);

         if (!FSEEK(mod_file_ptr, 
                    ((long)MD_PDT_HDR_LEN * TARGET_BYTES_PER_WORD) - 
                                            (long) MD_TBL_BYTE_SIZE,
                    SEEK_CUR)) {
            fp_module_idx = NULL_IDX;
            break;
         }
      }

      if (fp_module_idx != NULL_IDX) {
         FP_OFFSET(fp_module_idx)	= offset;
         found				= TRUE;
      }
   }
   else if (FSEEK(mod_file_ptr, FP_OFFSET(fp_module_idx), SEEK_CUR)) {
      num_recs_read = fread(&mit_header,
                            MD_TBL_BYTE_SIZE,
                            1,
                            mod_file_ptr);

      if (num_recs_read != 1) {
         PRINTMSG(AT_DEF_LINE(module_attr_idx), 726, Error,
                  AT_DEF_COLUMN(module_attr_idx),
                  AT_OBJ_NAME_PTR(module_attr_idx));
      }
      else {
         found		= TRUE;
      }
   }

   TRACE (Func_Exit, "read_module_tbl_header", NULL);

   return(found);

}  /* read_module_tbl_header */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Read in the module information table if it is in a file.              *|
|*	Copy in the module information table if it is in an elf buffer.       *|
|*									      *|
|* Input parameters:							      *|
|*	module_attr_idx => Attr index of the module to be read in.            *|
|*	mod_file_ptr    => Pointer to file, it mod table is in file.          *|
|*	mod_info_tbl    => Pointer to mod table in elf buffer.                *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	TRUE if a module info table was successfully brought in.              *|
|*									      *|
\******************************************************************************/
static	boolean  read_in_module_tbl(int		 fp_file_idx,
				    int		 module_attr_idx,
				    FILE	*mod_file_ptr,
				    char	*mod_info_tbl)

{
   int			al_idx;
   int			end_sb_idx;
   int			idx;
   int			ln_idx;
   int			name_idx;
   int			num_recs_read;
   boolean		ok			= TRUE;
   int			old_attr_tbl_idx	= attr_tbl_idx;
   int			old_bounds_tbl_idx	= bounds_tbl_idx;
   int			old_const_tbl_idx	= const_tbl_idx;
   int			old_const_pool_idx	= const_pool_idx;
   int			old_ir_tbl_idx		= ir_tbl_idx;
   int			old_ir_list_tbl_idx	= ir_list_tbl_idx;
   int			old_ln_idx;
   int			old_name_pool_idx;
   int			old_sec_name_tbl_idx	= sec_name_tbl_idx;
   int			old_sh_tbl_idx		= sh_tbl_idx;
   int			old_stor_blk_tbl_idx	= stor_blk_tbl_idx;
   int			old_type_tbl_idx	= type_tbl_idx;
   boolean		only_stmt;
   int			save_attr_list_start;
   int			save_attr_list_end;
   int			sb_idx;
   int			srch_sb_idx;


   TRACE (Func_Entry, "read_in_module_tbl", NULL);

   if (MD_HAS_ERRORS && inline_search) {

      /* Do not read if not module and there is errors. */

      return(FALSE);
   }

   /* If this is an alternate entry, we need to seek forward */
   /* to find the table entries and the main entry.  There   */
   /* will be an mit_header for each alternate entry,        */
   /* followed by the main entry.                            */

   alternate_entry	= MD_ALTERNATE_ENTRY;

   while (MD_ALTERNATE_ENTRY) {

      if (mod_info_tbl != NULL) {  /* Copying from elf buffer */
         memcpy((void *) &mit_header.wd[0],
                (char *) mod_info_tbl,
                (sizeof(mit_header_type)));

        mod_info_tbl += (sizeof(mit_header_type));
      }
      else {
         num_recs_read = fread(&mit_header,
                               sizeof(mit_header_type),
                               1,
                               mod_file_ptr);
# if defined(_DEBUG)

         if (dump_flags.pdt_dump) {
            dump_pdt(mod_file_ptr);
         }
# endif

         if (num_recs_read != 1) {
            PRINTMSG(AT_DEF_LINE(module_attr_idx), 726, Error,
                     AT_DEF_COLUMN(module_attr_idx),
                     AT_OBJ_NAME_PTR(module_attr_idx));
            ok	= FALSE;
            goto EXIT;
         }
      }
   }

   /* Store the module path into the name pool, before we read in the new     */
   /* module stuff into the name pool.  This way it won't get compressed out. */

   if (!inline_search) {

      if (!ATP_IN_CURRENT_COMPILE(module_attr_idx)) {
         ATP_MOD_PATH_LEN(module_attr_idx)	 = FP_NAME_LEN(fp_file_idx);
         ATP_MOD_PATH_IDX(module_attr_idx)	 = name_pool_idx + 1;
         name_idx				 = FP_NAME_IDX(fp_file_idx);

         TBL_REALLOC_CK(name_pool, WORD_LEN(FP_NAME_LEN(fp_file_idx)));

         for (idx = ATP_MOD_PATH_IDX(module_attr_idx);idx<=name_pool_idx;idx++){
            name_pool[idx].name_long = str_pool[name_idx].name_long;
            name_idx++;
         }
      }
      only_stmt		= ATP_USE_TYPE(module_attr_idx) == Use_Only;
 
   }
   else {
      only_stmt		= FALSE;
   }
 
   old_name_pool_idx	= name_pool_idx;


   if (mod_info_tbl != NULL) { /* Elf file - Will always be 3 or greater */
       memcpy((void *) &mit_descriptor[1].wd, 
              (char *) mod_info_tbl,
              (sizeof(mit_descriptor_type) * Num_Of_Tbls));

      mod_info_tbl += (sizeof(mit_descriptor_type) * Num_Of_Tbls);
   }
   else {
      num_recs_read = fread(&mit_descriptor[1],
                            sizeof(mit_descriptor_type),
                            Num_Of_Tbls,
                            mod_file_ptr);

      if (num_recs_read != Num_Of_Tbls) {
         PRINTMSG(AT_DEF_LINE(module_attr_idx), 726, Error,
                  AT_DEF_COLUMN(module_attr_idx),
                  AT_OBJ_NAME_PTR(module_attr_idx));
         ok	= FALSE;
         goto EXIT;
      }
   }


   if (MD_NUM_ENTRIES(Loc_Name_Tbl) == 1 && !inline_search) { 

      /* There is always 1 entry - the name of the module */

      ok = FALSE;

      if (only_stmt) {
         PRINTMSG(AT_DEF_LINE(module_attr_idx), 793, Error,
                  AT_DEF_COLUMN(module_attr_idx),
                  AT_OBJ_NAME_PTR(module_attr_idx));
         AT_DCL_ERR(module_attr_idx) = TRUE;
      }
      else if (ATP_USE_LIST(module_attr_idx) != NULL_IDX) {  /* rename-list */
         PRINTMSG(AT_DEF_LINE(module_attr_idx), 934, Error,
                  AT_DEF_COLUMN(module_attr_idx),
                  AT_OBJ_NAME_PTR(module_attr_idx));
         AT_DCL_ERR(module_attr_idx) = TRUE;
      }
      else {

         /* There is no only-list or rename-list, so just issue */
         /* a warning that the module is empty.                 */

         PRINTMSG(AT_DEF_LINE(module_attr_idx), 867, Warning,
                  AT_DEF_COLUMN(module_attr_idx),
                  AT_OBJ_NAME_PTR(module_attr_idx));
      }
      goto EXIT;
   }

# ifdef _DEBUG
   if (!inline_search && loc_name_tbl_idx != SCP_LN_LW_IDX(curr_scp_idx)) {
      PRINTMSG(AT_DEF_LINE(module_attr_idx), 832, Internal,
               AT_DEF_COLUMN(module_attr_idx),
               AT_OBJ_NAME_PTR(SCP_ATTR_IDX(curr_scp_idx)));
   }
# endif

   /* Add 2 buffer entries between the old and new local name tables.  One */
   /* of these is used to hold the all zero word, so that srch_sym_tbl can */
   /* be used with the new stuff from the module.                          */

   ++loc_name_tbl_idx;
   ++loc_name_tbl_idx;

   old_ln_idx		= loc_name_tbl_idx;

   if (!inline_search) {

      /* Leave space in the constant table so that we can use ntr_const_tbl   */
      /* to make sure everything is entered after the current constant table. */

      save_const_tbl_idx	= const_tbl_idx;
      const_tbl_idx 	       += (MD_NUM_ENTRIES(Const_Tbl) + 2);
      old_const_tbl_idx		= const_tbl_idx;

      save_const_pool_idx	= const_pool_idx;
      const_pool_idx           += (MD_NUM_ENTRIES(Const_Pool) + 2);
      old_const_pool_idx	= const_pool_idx;
   }
   else {
      save_const_tbl_idx	= NULL_IDX;
      save_const_pool_idx	= NULL_IDX;
   }

   /* KAY - What if we are at the end of the constant pool?  */

# if defined(_HOST32) 

   /* If this is host32, the table always went out on the 1st index */
   /* so make sure it always comes in on a non-daligned index.      */
   /* Everything should come out okay that way.  If const_pool_idx  */
   /* is double aligned, then the address following it will be      */
   /* non-double aligned and exactly what we want.                  */

   while ((((long)&const_pool[const_pool_idx]) % 8) != 0) {
      const_pool_idx++;
   }
   old_const_pool_idx	= const_pool_idx;

# endif

   if (!read_sytb_from_module_file(module_attr_idx,
                                   mod_file_ptr,
                                   mod_info_tbl)) {

      loc_name_tbl_idx = old_ln_idx - 2;
      ok = FALSE;
      goto EXIT;
   }

   /* Although a constant entry may be larger than one const_tbl entry,      */
   /* entries being read from the module info file are all treated as each   */
   /* being one const tbl entry in length.  First room must be made for all  */
   /* the new entries.  They must all be read in, because if we treated each */
   /* as individual entries, we couldn't determine their size, because type  */
   /* CHARACTER uses another const tbl entry to tell how long the char is.   */

   /* Allocate mod link table and clear all entries. */

   /* The mod_link_tbl will be used for compression and straightening out    */
   /* table indexes.  In simple use semantics, entries will be set, but they */
   /* will never be used.  We have to set table entries, because we do not   */
   /* know if we will use them until we process all tables in the module.    */

   allocate_mod_link_tbl(0);  /* Let routine determine size. */

   /* Loop and set the local name table entries.  ln_idx is set earlier.     */


   for (ln_idx = old_ln_idx+1; ln_idx <= loc_name_tbl_idx; ln_idx++) {
      LN_NAME_IDX(ln_idx)	 = old_name_pool_idx + LN_NAME_IDX(ln_idx);
      LN_ATTR_IDX(ln_idx)	 = old_attr_tbl_idx + LN_ATTR_IDX(ln_idx);
      ML_AT_LN_NAME(LN_ATTR_IDX(ln_idx))	= TRUE;
   }

   /* This loops thru the new storage block entries, looking for duplicates  */
   /* and also folding in the stack block to the parents and generally       */
   /* resolving the storage block entries.  At this point,                   */
   /* old_stor_blk_tbl_idx indexes to the end of the storage block table     */
   /* before the entries were read up from the module.  stor_blk_tbl_idx     */
   /* indexes to the end of the storage block table.  stor_blk_tbl_idx -     */
   /* old_stor_blk_tbl_idx, gives the number of new entries in the table.    */
   /* To get the original index of a storage block coming in from the module */
   /* take (idx - old_stor_blk_tbl_idx).  This is needed to set the correct  */
   /* entry in the mod_link_tbl, so that all the new module entries pointing */
   /* to the storage block table can be updated in assign_new_idxs_after_    */
   /* input.  Set the stor_blk_tbl_idx to the end of the old table, so that  */
   /* searches will not search the new entries.                              */

   end_sb_idx		= stor_blk_tbl_idx;
   stor_blk_tbl_idx	= old_stor_blk_tbl_idx;

   for (idx = stor_blk_tbl_idx+1; idx <= end_sb_idx; idx++) {
      SB_NAME_IDX(idx)		= old_name_pool_idx + SB_NAME_IDX(idx);
#ifdef KEY /* Bug 14150 */
      int ext_name_idx = SB_EXT_NAME_IDX(idx);
      SB_EXT_NAME_IDX(idx)	= ext_name_idx ?
        (old_name_pool_idx + ext_name_idx) :
	0;
#endif /* KEY Bug 14150 */
      SB_HAS_RENAMES(idx)	= FALSE;
      SB_DEF_LINE(idx)		= AT_DEF_LINE(module_attr_idx);
      SB_DEF_COLUMN(idx)	= AT_DEF_COLUMN(module_attr_idx);
      SB_SCP_IDX(idx)		= curr_scp_idx;
      SB_ORIG_SCP_IDX(idx)	= curr_scp_idx;
      SB_LAST_ATTR_LIST(idx)	= NULL_IDX;

      if (SB_FIRST_ATTR_IDX(idx) != NULL_IDX) {
         SB_FIRST_ATTR_IDX(idx) = old_attr_tbl_idx + SB_FIRST_ATTR_IDX(idx);
      }

      switch (SB_LEN_FLD(idx)) {
      case CN_Tbl_Idx:
         SB_LEN_IDX(idx)	= old_const_tbl_idx + SB_LEN_IDX(idx);
         break;

      case AT_Tbl_Idx:
         SB_LEN_IDX(idx)	= old_attr_tbl_idx + SB_LEN_IDX(idx);
         break;

      case IR_Tbl_Idx:
         SB_LEN_IDX(idx)	= old_ir_tbl_idx + SB_LEN_IDX(idx);
         break;

      case IL_Tbl_Idx:
         SB_LEN_IDX(idx)	= old_ir_list_tbl_idx + SB_LEN_IDX(idx);
         break;

      /* KAY - This case can be removed when we no longer support 3.0 */

      default:
         SB_LEN_FLD(idx)	= CN_Tbl_Idx;
         SB_LEN_IDX(idx)	= old_const_tbl_idx + SB_LEN_IDX(idx);
         break;
      }

      /* If we're inline searching we never want to mix storage blocks. */

      if (!inline_search) {
         SB_MODULE_IDX(idx)	= (SB_MODULE_IDX(idx) == NULL_IDX) ?
                                          module_attr_idx :
                                          old_attr_tbl_idx + SB_MODULE_IDX(idx);
         SB_USE_ASSOCIATED(idx)	= TRUE;
         srch_sb_idx		= srch_stor_blk_tbl(SB_NAME_PTR(idx), 
                                                    SB_NAME_LEN(idx),
                                                    curr_scp_idx);
      }
      else {
         srch_sb_idx		= NULL_IDX;

         if (SB_MODULE_IDX(idx) != NULL_IDX) {
            SB_MODULE_IDX(idx)	= old_attr_tbl_idx + SB_MODULE_IDX(idx);
         }
      }

      if (srch_sb_idx == NULL_IDX) { 
         sb_idx			= ++stor_blk_tbl_idx;
         stor_blk_tbl[sb_idx]	= stor_blk_tbl[idx];

      }

      /* If we find a common storage block, it can only have gotten here     */
      /* thru use association.   If we have   character*(i) function j()     */
      /* where i is host associated, i's storage block does not get copied   */
      /* down until attr resolution time.                                    */

      else if (SB_IS_COMMON(idx) &&
               (compare_names(AT_OBJ_NAME_LONG(SB_MODULE_IDX(srch_sb_idx)),
                              AT_NAME_LEN(SB_MODULE_IDX(srch_sb_idx)),
                              AT_OBJ_NAME_LONG(SB_MODULE_IDX(idx)),
                              AT_NAME_LEN(SB_MODULE_IDX(idx))) != 0)) {

         /* Mark the new storage block as being defined in multiple scopes.  */
         /* Also, mark it as hidden, so only the first storage block will be */
         /* found when searches are made for this storage block.  During     */
         /* storage_blk_resolution, when this block is found, SB_MERGED_BLK  */
         /* will be updated to the end (or start) of the SB_MERGED_BLK list. */
         /* Then the original block will be marked SB_DEF_MULT_SCPS.         */
         /* final_attr_resolution,  ATD_STOR_BLK_IDX will be updated to      */
         /* SB_MERGED_BLK and then SB_DEF_MULT_SCPS will be checked to see   */
         /* if ATD_EQUIV needs to be set.  If this is an only_stmt, nothing  */
         /* gets changed in the original block and the new block gets        */
         /* compressed out.                                                  */

         sb_idx				= ++stor_blk_tbl_idx;
         stor_blk_tbl[sb_idx]		= stor_blk_tbl[idx];
         SB_DEF_MULT_SCPS(sb_idx)	= TRUE;
         SB_HIDDEN(sb_idx)		= TRUE;
         SB_MERGED_BLK_IDX(sb_idx)	= srch_sb_idx;
      }
      else { /* This is the same common block from the same module or it     */
             /* is a static based or darg block.  Share the block.           */

         sb_idx = srch_sb_idx;
      }
      
      ML_SB_IDX(idx - old_stor_blk_tbl_idx)	= sb_idx;
   }

   if (keep_module_procs) {

      /* Create a list of all module procedures that can be inlined.       */
      /* Save attr_list_idx so we can delete the new list when we're done. */

      save_attr_list_start		= SCP_ATTR_LIST(curr_scp_idx);
      save_attr_list_end		= SCP_ATTR_LIST_END(curr_scp_idx);
      SCP_ATTR_LIST(curr_scp_idx)	= NULL_IDX;
      SCP_ATTR_LIST_END(curr_scp_idx)	= NULL_IDX;

      assign_new_idxs_after_input(module_attr_idx);

      al_idx				= SCP_ATTR_LIST(curr_scp_idx);
      SCP_ATTR_LIST(curr_scp_idx)	= save_attr_list_start;
      SCP_ATTR_LIST_END(curr_scp_idx)	= save_attr_list_end;

      /* Have module procedures to use for inlining.  Mark the  */
      /* attrs so they do not get checked for duplicate attrs.  */

      process_procs_for_inlining(al_idx);
      free_attr_list(al_idx);
   }
   else {
      assign_new_idxs_after_input(module_attr_idx);
   }

   /* We want to use MD_NUM_ENTRIES(Stor_Blk_Tbl) here because we allocated */
   /* space for that many entries when we started.  The actual number of    */
   /* stor blk entries is most likely less because we collapsed the store   */
   /* block table previously.                                               */

   for (idx = 0; idx <= MD_NUM_ENTRIES(Stor_Blk_Tbl); idx++) {
      ML_SB_IDX(idx)	= NULL_IDX;
   }

# ifdef _DEBUG
   for (sb_idx = old_stor_blk_tbl_idx + 1;
           sb_idx <= stor_blk_tbl_idx; sb_idx++) {

      if (SB_LEN_FLD(sb_idx) == CN_Tbl_Idx &&
          TYP_TYPE(CN_TYPE_IDX(SB_LEN_IDX(sb_idx))) != Integer) {
         print_sb(sb_idx);
         print_cn(SB_LEN_IDX(sb_idx));
         PRINTMSG(1, 626, Internal, 0, "integer constant SB_LEN_IDX",
                  "read_in_module_tbl");
      }
   }
# endif

   ML_AT_IDX(0)		= old_attr_tbl_idx;
   ML_BD_IDX(0)		= old_bounds_tbl_idx;
   ML_CN_IDX(0)		= old_const_tbl_idx;
   ML_CP_IDX(0)		= old_const_pool_idx;
   ML_IL_IDX(0)		= old_ir_list_tbl_idx;
   ML_IR_IDX(0)		= old_ir_tbl_idx;
   ML_LN_IDX(0)		= SCP_LN_FW_IDX(curr_scp_idx);
   ML_NP_IDX(0)		= old_name_pool_idx;
   ML_SB_IDX(0)		= old_stor_blk_tbl_idx;
   ML_SN_IDX(0)		= old_sec_name_tbl_idx;
   ML_SH_IDX(0)		= old_sh_tbl_idx;
   ML_TYP_IDX(0)	= old_type_tbl_idx;

EXIT: 

   TRACE (Func_Exit, "read_in_module_tbl", NULL);

   return(ok);

}   /* read_in_module_tbl */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*									      *|
|* Input parameters:							      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*									      *|
\******************************************************************************/
static	boolean	read_sytb_from_module_file(int			module_attr_idx,
					   FILE		       *mod_file_ptr,
					   char		       *mod_info_tbl)

{
   long			*from_idx;
   int			 i;
   int			 idx;
   int			 j;
   int			 num_entries;
   int			 num_recs_read;
   boolean		 ok		= TRUE;
   old_const_tbl_type	*old_cn_tbl 	= NULL;
   old_ir_tbl_type	*old_ir_tbl 	= NULL;
#ifdef KEY /* Bug 10177 */
   int			 save_const_tbl_idx = 0;
   int			 save_ir_tbl_idx = 0;
#else /* KEY Bug 10177 */
   int			 save_const_tbl_idx;
   int			 save_ir_tbl_idx;
#endif /* KEY Bug 10177 */
   int			 size;
#ifdef KEY /* Bug 10177 */
   void			*tbl = 0;
#else /* KEY Bug 10177 */
   void			*tbl;
#endif /* KEY Bug 10177 */
   tbl_type_type	 tbl_type;
   long			*to_idx;


   TRACE (Func_Entry, "read_sytb_from_module_file", NULL);

   /* WARNING:  CHECK_TBL_ALLOC_SIZE may move the table. */

   for (idx = 1; idx <= Num_Of_Tbls; idx++) {
      tbl_type		= MD_TBL_TYPE(idx);
      num_entries	= MD_NUM_ENTRIES(idx);

      if (num_entries > 0) {

         switch (tbl_type) {

         case Attr_Tbl:

            /* If we're reading up a smaller version of the attr table, we */
            /* still want to make space for everything.  We'll adjust the  */
            /* older version after it has been read in.                    */

            CHECK_TBL_ALLOC_SIZE(attr_tbl, attr_tbl_idx + num_entries);
            CHECK_TBL_ALLOC_SIZE(attr_aux_tbl, attr_aux_tbl_idx + num_entries);

            size		= sizeof(attr_tbl_type);
            tbl			= &attr_tbl[attr_tbl_idx + 1];
            attr_tbl_idx       += num_entries;
            attr_aux_tbl_idx   += num_entries;
            break;

         case Bounds_Tbl:
            CHECK_TBL_ALLOC_SIZE(bounds_tbl, bounds_tbl_idx + num_entries);
            size		= sizeof(bounds_tbl_type);
            tbl			= &bounds_tbl[bounds_tbl_idx + 1];
            bounds_tbl_idx     += num_entries;
            break;

         case Const_Tbl:
            CHECK_TBL_ALLOC_SIZE(const_tbl, const_tbl_idx + num_entries);

            if (! MD_NEW_CONST_TBL) {  /* KAY - What version ?? */
               size	= sizeof(old_const_tbl_type);
               old_cn_tbl = (old_const_tbl_type *)malloc(size * num_entries);
               tbl	= old_cn_tbl;
               save_const_tbl_idx = const_tbl_idx + 1;
            }
            else {
               size	= sizeof(const_tbl_type);
               tbl	= &const_tbl[const_tbl_idx + 1];
            }
            const_tbl_idx      += num_entries;
            break;

         case Const_Pool:
            CHECK_TBL_ALLOC_SIZE(const_pool, const_pool_idx + num_entries);
            size		= sizeof(const_pool_type);
            tbl			= &const_pool[const_pool_idx + 1];
            const_pool_idx     += num_entries;
            break;

         case Ir_List_Tbl:
            CHECK_TBL_ALLOC_SIZE(ir_list_tbl, ir_list_tbl_idx + num_entries);
            size		= sizeof(ir_list_tbl_type);
            tbl			= &ir_list_tbl[ir_list_tbl_idx + 1];
            ir_list_tbl_idx    += num_entries;
            break;

         case Ir_Tbl:
            CHECK_TBL_ALLOC_SIZE(ir_tbl, ir_tbl_idx + num_entries);

            if (MD_VERSION_NUM > MD_LAST_4_0_VERSION) {
               size		= sizeof(ir_tbl_type);
               tbl		= &ir_tbl[ir_tbl_idx + 1];
            }
            else {
               size		= sizeof(old_ir_tbl_type);
               old_ir_tbl	= (old_ir_tbl_type *)malloc(size * num_entries);
               tbl		= old_ir_tbl;
               save_ir_tbl_idx	= ir_tbl_idx + 1;
            }
            ir_tbl_idx	       += num_entries;
            break;

         case Name_Pool:
            CHECK_TBL_ALLOC_SIZE(name_pool, name_pool_idx + num_entries);
            size		= sizeof(name_pool_type);
            tbl			= &name_pool[name_pool_idx + 1];
            name_pool_idx      += num_entries;
            break;

         case Sec_Name_Tbl:
            CHECK_TBL_ALLOC_SIZE(sec_name_tbl, sec_name_tbl_idx + num_entries);
            size		= sizeof(sec_name_tbl_type);
            tbl			= &sec_name_tbl[sec_name_tbl_idx + 1];
            sec_name_tbl_idx   += num_entries;
            break;

         case Sh_Tbl:
            size		= sizeof(sh_tbl_type);

            if (keep_module_procs || inline_search) {
               CHECK_TBL_ALLOC_SIZE(sh_tbl, sh_tbl_idx + num_entries);
               tbl		= &sh_tbl[sh_tbl_idx + 1];
               sh_tbl_idx      += num_entries;
            }
            else {  /* Skip this table - Not getting module procedures. */

               if (mod_info_tbl != NULL) {  /* Copying from elf buffer */
                  mod_info_tbl += (size * num_entries);
                  continue;
               }
               else if (!FSEEK(mod_file_ptr, (size * num_entries), SEEK_CUR)) {
                  ok = FALSE;  /* Let error message issue.  Bad file. */
               }
               else {
                  continue;
               }
               break;
            }
            break;

         case Stor_Blk_Tbl:
            CHECK_TBL_ALLOC_SIZE(stor_blk_tbl, stor_blk_tbl_idx + num_entries);
            size		= sizeof(stor_blk_tbl_type);
            tbl			= &stor_blk_tbl[stor_blk_tbl_idx + 1];
            stor_blk_tbl_idx   += num_entries;
            break;

         case Type_Tbl:
            CHECK_TBL_ALLOC_SIZE(type_tbl, type_tbl_idx + num_entries);
            size		= sizeof(type_tbl_type);
            tbl			= &type_tbl[type_tbl_idx + 1];
            type_tbl_idx       += num_entries;
            break;

         case Loc_Name_Tbl:
            CHECK_TBL_ALLOC_SIZE(loc_name_tbl, loc_name_tbl_idx + num_entries);
            size		= sizeof(loc_name_tbl_type);
            tbl			= &loc_name_tbl[loc_name_tbl_idx + 1];
            loc_name_tbl_idx   += num_entries;
            break;

         default:
            ok = FALSE;
            PRINTMSG(AT_DEF_LINE(module_attr_idx), 726, Error,
                     AT_DEF_COLUMN(module_attr_idx),
                     AT_OBJ_NAME_PTR(module_attr_idx));
            goto EXIT;
         }
   
         if (mod_info_tbl != NULL) {  /* Copying from elf buffer */
            (void) memcpy(tbl,
                         (char *) mod_info_tbl,
                         size * num_entries);
            mod_info_tbl += (size * num_entries);
         }
         else {
            num_recs_read = fread(tbl,
                                  size,
                                  num_entries,
                                  mod_file_ptr);

            if (num_recs_read != num_entries) {
               ok = FALSE;
               PRINTMSG(AT_DEF_LINE(module_attr_idx), 726, Error,
                        AT_DEF_COLUMN(module_attr_idx),
                        AT_OBJ_NAME_PTR(module_attr_idx));
               break;  /* File is bad - exit */
            }
         }

         if (tbl_type == Ir_Tbl && MD_VERSION_NUM <= MD_LAST_4_0_VERSION) {

            for (i = 0; i < num_entries; i++) {
               to_idx	= (long *) &(ir_tbl[save_ir_tbl_idx+i]);
               from_idx	= (long *) &(old_ir_tbl[i]);

# if defined(_HOST32)
               to_idx[0] = from_idx[0];
               to_idx[1] = from_idx[1];
               to_idx[2] = 0;
               to_idx[3] = 0;
               to_idx[4] = from_idx[2];
               to_idx[5] = from_idx[3];
               to_idx[6] = from_idx[4];
               to_idx[7] = from_idx[5];
# else
               to_idx[0] = from_idx[0];
               to_idx[1] = 0;
               to_idx[2] = from_idx[1];
               to_idx[3] = from_idx[2];
# endif
               IR_RANK(save_ir_tbl_idx+i)	= OLD_IR_RANK(i);
               IR_DV_DIM(save_ir_tbl_idx+i)	= OLD_IR_DV_DIM(i);
               IR_OPR(save_ir_tbl_idx+i)	= OLD_IR_OPR(i);
            }
            free(old_ir_tbl);
         }

         if (tbl_type == Const_Tbl) {
            if (! MD_NEW_CONST_TBL &&
                old_cn_tbl != NULL) {
               for (i = 0; i < num_entries; i++) {
                  to_idx = (long *) &(const_tbl[save_const_tbl_idx+i]);
                  from_idx = (long *) &(old_cn_tbl[i]);
                  Pragma("_CRI shortloop")
                  for (j = 0; j < OLD_NUM_CN_WDS; j++) {
                     to_idx[j] = from_idx[j];
                  }
               }
               free(old_cn_tbl);
            }
         }
      }
   }

EXIT:

   TRACE (Func_Exit, "read_sytb_from_module_file", NULL);

   return(ok);

}  /* read_sytb_from_module_file */

# if defined(_TARGET_OS_SOLARIS) && defined(_MODULE_TO_DOT_o)

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	srch_elf_file_for_module_tbl searches SPARC elf files for module      *|
|*	information tables.                                                   *|
|*									      *|
|* Input parameters:							      *|
|*	module_attr_idx	-> Attr index of module to search for.		      *|
|*	fp_file_idx	-> File path table index to entry describing elf file *|
|*			   to seach.                                          *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	TRUE  -> The module has been found.  Mod information table has been   *|
|*	         completely read in.                                          *|
|*	FALSE -> The module was not found.                                    *|
|*									      *|
\******************************************************************************/
static	boolean	srch_elf_file_for_module_tbl(int	 module_attr_idx,
					     int	 fp_file_idx)

{
   int		 fd;		/* File descriptor of ELF file      */
   boolean	 found		= FALSE;

   Elf		*file_elfd;	/* ELF descriptor for whole file    */
   Elf		*obj_elfd;	/* ELF descriptor for curr. object  */
   Elf_Cmd	 elf_cmd;	/* ELF command to read from file    */
   Elf32_Ehdr	*ehdr;		/* Header from current object       */


   TRACE (Func_Entry, "srch_elf_file_for_module_tbl", NULL);

   if ((fd = open(FP_NAME_PTR(fp_file_idx), O_RDONLY, 0)) == -1) {

      /* BHJ - need my 126 message here to say bad file. */

      return(found);
   }

   /* Check ELF version.   See elf(3E).  */
   
   /* Notice that the elf_version() call is the very first call to the ELF */
   /* library in the whole program.  It has no idea that a file has been   */
   /* opened, much less that the resulting fd is going to get passed to    */
   /* the ELF library.  It's just a check between the version of ELF the   */
   /* code was compiled with (from the <libelf.h> #include file), and the  */
   /* version of ELF the ELF library it's linked with was compiled with.   */

   if (elf_version(EV_CURRENT) == EV_NONE) {

      /* BHJ - Something is wrong with the file.  - See above comment.     */
      /*       May want a more descriptive message, like something is      */
      /*       wrong with the file.                                        */

      return(found);
   }

   /* Process the file, which may be either a simple file or an archive of */
   /* files.  The outer while{} loop will iterate once for a simple file,  */
   /* and as many times as there are files in the archive for an archive   */
   /* file.  In either case, anything that is not an ELF object file will  */
   /* be skipped, due to the elf32_getehdr() call returning zero.          */

   /* See elf(3E), elf_begin(3E), elf32_getehdr(3E), elf_next(3E),         */
   /* elf_end(3E).                                                         */

   /* This needs to be ELF_C_READ so that we can process archive files.    */
   /* If it is switched to ELF_C_RDWR, this code will not allow archive    */
   /* files here.                                                          */

   elf_cmd	= ELF_C_READ;
   file_elfd	= elf_begin(fd, elf_cmd, ((Elf *) NULL));

   while ((obj_elfd = elf_begin(fd, elf_cmd, file_elfd)) != ((Elf *) NULL)) {

      /* If you're not working with an ELF file, it's the elf32_getehdr()  */
      /* call that tells you so.  It yields back a ((Elf32_Ehdr *) NULL)   */
      /* in that case.  Actually, I think I may have coded it as a check   */
      /* against a 0 (zero) return value, but it's really a NULL pointer.  */


      if ((ehdr = elf32_getehdr(obj_elfd)) != 0) {

         /* This is an ELF object file.  Process it. */

         FP_CLASS(fp_file_idx)	= Elf_File_Fp;

         if (do_elf_object(obj_elfd, 
                           ehdr, 
                           module_attr_idx,
                           fp_file_idx)) {
            found = TRUE;
            break;
         }
      }

      elf_cmd = elf_next(obj_elfd);
      elf_end(obj_elfd);
   }

   elf_end(file_elfd);
   close(fd);

   TRACE (Func_Exit, "srch_elf_file_for_module_tbl", NULL);

   return(found);

} /* srch_elf_file_for_module_tbl */
# endif

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Process one object module from an ELF file.                           *|
|*									      *|
|* Input parameters:							      *|
|*	obj_elfd	-> Elf descriptor for current object.		      *|
|*	ehdr		-> Elf header for current object.     		      *|
|*	module_attr_idx	-> Attr index of module to search for.		      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	TRUE  -> The module has been found.  Mod information table has been   *|
|*	         completely read in.                                          *|
|*	FALSE -> The module was not found.                                    *|
|*									      *|
\******************************************************************************/
# if defined(_TARGET_OS_SOLARIS) && defined(_MODULE_TO_DOT_o)
static	boolean	do_elf_object(Elf		*obj_elfd,
			      Elf32_Ehdr	*ehdr,
			      int		 module_attr_idx,
			      int		 fp_file_idx)
{

   Elf_Data		*data;	/* Current section's data           */
   boolean		 found	= FALSE;
   Elf_Scn		*scn;	/* Current section                  */
   Elf32_Shdr		*shdr;	/* Current section's header         */


   TRACE (Func_Entry, "do_elf_object", NULL);

   /* Process each section of the current object.  * See elf_nextscn(3E). */

   scn = (Elf_Scn *) NULL;

   while ((scn = elf_nextscn(obj_elfd, scn)) != ((Elf_Scn *) NULL)) {

      /* If this section isn't for us, skip it.  * See elf32_getshdr(3E). */

      if ((shdr = elf32_getshdr(scn)) == ((Elf32_Shdr *) NULL)) {
          continue;
      }


      /* Skip sections that aren't .note ones. See /usr/include/sys/elf.h. */

      if (shdr->sh_type != SHT_NOTE) {
          continue;
      }


      /* Go through each data object in the section.  Typically,   */
      /* this loop will execute exactly once.  The only time it    */
      /* would iterate more than one time would be if the section  */
      /* were currently being built up by multiple calls to the    */
      /* elf_newdata(3E) routine during the creation of a new      */
      /* Elf file.                                                 */

      /* We skip data blocks that aren't made of plain vanilla     */
      /* bytes, because we don't know how to handle them.  They    */
      /* wouldn't be ours, anyway, so it's okay to do this.        */
      /* See elf_getdata(3E).                                      */

      data = ((Elf_Data *) NULL);

      while ((data = elf_getdata(scn, data)) != ((Elf_Data *) NULL)) {

         if (data->d_type == ELF_T_BYTE) {

            if (do_elf_notes_section(data, module_attr_idx, fp_file_idx)) {
               found = TRUE;
               break;
            }
         }
      }
   }  /* once-per-section loop */

   TRACE (Func_Exit, "do_elf_object", NULL);

   return(found);

} /* do_elf_object */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*      Process one .notes section from an ELF file.			      *|
|*									      *|
|*	Process each successive note in this .note section.  A		      *|
|*	single note looks like this:                              	      *|
|*									      *|
|*	  1 32-bit word:   length of name (below)                 	      *|
|*	  1 32-bit word:   length of descriptor (below)           	      *|
|*	  1 32-bit word:   type of note				  	      *|
|*	  0 or more bytes: name, padded with bytes of zero to     	      *|
|*	                   the next 32-bit word boundary          	      *|
|*	  0 or more bytes: descriptor (the data in this note),    	      *|
|*	                   padded with bytes of zero to the       	      *|
|*	                   next 32-bit boundary                   	      *|
|*									      *|
|*	The name and descriptor lengths (the first two words)     	      *|
|*	do not include the trailing padding bytes for the name    	      *|
|*	and the descriptor.  If the name (or descriptor) is an    	      *|
|*	even multiple of 32-bit words long (including zero),      	      *|
|*	no padding is added.  Name, descriptor, and type are      	      *|
|*	all chosen by the originator.  The name is typically      	      *|
|*	that of the originator.  The type can be used by an       	      *|
|*	originator to differentiate amongst multiple kinds of     	      *|
|*	data that said originator might place in the descriptor.  	      *|
|*									      *|
|*	Multiple notes, from multiple originators, may occur in   	      *|
|*	a .note section.  When traversing the notes in a section, 	      *|
|*	programs should pay attention to those they understand,   	      *|
|*	and skip those they don't.                                	      *|
|*									      *|
|*	This particular program removes entries that match "Cray  	      *|
|*	Research, Incorporated", and have type NOTE_TYPE.  Such   	      *|
|*	entries contain module information that originated in     	      *|
|*	.o files created by the SPARC f90 compiler, and was       	      *|
|*	included in an executable by ld(1).  Since such module    	      *|
|*	information is of no use in an executable, this utility   	      *|
|*	was created to remove it.                                 	      *|
|*									      *|
|*	See /usr/include/sys/elf.h, _SunOS 5.3 Linker and         	      *|
|*	    Libraries Manual_ (hardcopy or AnswerBook),           	      *|
|*	    chapter 5, page 137 ("Note Section").                 	      *|
|*									      *|
|*									      *|
|* Input parameters:							      *|
|*	data		-> Pointer to in copy memory of whole notes section.  *|
|*	module_attr_idx	-> Attr index of module to search for.		      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	TRUE  -> The module has been found.  Mod information table has been   *|
|*	         completely read in.                                          *|
|*	FALSE -> The module was not found.                                    *|
|*									      *|
\******************************************************************************/
static	boolean	do_elf_notes_section(Elf_Data		*data,
				     int		 module_attr_idx,
				     int		 fp_file_idx)
{

   Elf32_Word		 data_off_src;	/* # of source data bytes done      */
   boolean		 found;		/* TRUE if found module             */
   char			*mod_info_tbl;  /* Ptr to .note descriptor          */
   Elf32_Nhdr		*n_hdr;		/* .note block header               */
   char			*n_name_ptr;	/* Pointer to name in .note block   */
   unsigned int    	 namesz;	/* Bytes of name to print           */
   Elf32_Word		 note_size;	/* Size of this .note               */


   /* RUP_BYTES() rounds a byte count up to the next Elf32_Word boundary.  */

# define BRND_SIZE	(sizeof(Elf32_Word))
# define RUP_BYTES(n)	(((n) + BRND_SIZE - 1) & ~(BRND_SIZE - 1))


   TRACE (Func_Entry, "do_elf_notes_section", NULL);


   data_off_src	= 0;
   found	= FALSE;

   while (data_off_src < data->d_size) {

      /* Based on the note header that starts at the current    */
      /* location in this data block, find the size of the      */
      /* entry, and get a pointer to the name.                  */

      n_hdr      = (Elf32_Nhdr *) (((char *) data->d_buf) + data_off_src);
      note_size  = sizeof(Elf32_Nhdr)
                   + RUP_BYTES(n_hdr->n_namesz)
                   + RUP_BYTES(n_hdr->n_descsz);
      n_name_ptr = (char *) ((void *) (n_hdr + 1));
      namesz	 = n_hdr->n_namesz
                     - ((*(n_name_ptr + n_hdr->n_namesz - 1) == '\0') ? 1 : 0);
      mod_info_tbl = n_name_ptr + RUP_BYTES(n_hdr->n_namesz);


      /* Is this CRI SPARC f90 module information? */

      if (n_hdr->n_type == NOTE_TYPE
         && memcmp(n_name_ptr, NOTE_ORIG_NAME, NOTE_ORGNAM_LEN) == 0) {

         /* note_size bytes is the size of the module information.         */
         /* The elf data routine brings in the whole table to memory.      */
         /* Check the name to see if this is the module we're looking for. */

         (void) memcpy((long *) &mit_header.wd[0], 
                       ((char *) mod_info_tbl),
                       sizeof(mit_header_type));

         if (compare_names(AT_OBJ_NAME_LONG(module_attr_idx),
                           AT_NAME_LEN(module_attr_idx),
                           MD_NAME_LONG,
                           MD_NAME_LEN) == 0) {

           /* Found it.  Copy memory so we can use it. */
           /* Bump the pointer past mit_header.        */

           mod_info_tbl	+= sizeof(mit_header_type);

           read_in_module_tbl(fp_file_idx,
                              module_attr_idx,
                              NULL, 		 /* No file pointer */
                              mod_info_tbl);
           found = TRUE;
           break;
        }
     }

     /* Find the next note's offset.                           */
     /* note_size is the size of the module information table. */

     data_off_src += note_size;

   }  /* once-per-note loop */

   TRACE (Func_Exit, "do_elf_notes_section", NULL);

   return(found);

} /* do_elf_notes_section */
# endif

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Reassigns the indexes in the tables after a module has been read in.  *|
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
static void  assign_new_idxs_after_input(int	module_attr_idx)

{
   int		al_idx;
   int		at_idx		= attr_tbl_idx - MD_NUM_ENTRIES(Attr_Tbl);
   int		attr_idx;
   int		bd_idx		= bounds_tbl_idx - MD_NUM_ENTRIES(Bounds_Tbl);
   int		bounds_idx;
   int		cn_idx		= const_tbl_idx - MD_NUM_ENTRIES(Const_Tbl);
   int		column;
   int		const_idx;
   int		cp_idx		= const_pool_idx - MD_NUM_ENTRIES(Const_Pool);
   int		dim;
   int		il_idx		= ir_list_tbl_idx - MD_NUM_ENTRIES(Ir_List_Tbl);
   int		ir_idx		= ir_tbl_idx - MD_NUM_ENTRIES(Ir_Tbl);
   int		line;
   int		list_idx;
   int		mod_idx;
   int		name_idx;
   int		new_module_idx;
   int		np_idx		= name_pool_idx - MD_NUM_ENTRIES(Name_Pool);
   int		old_cn_idx;
   int		old_il_idx;
   int		save_il_free_list;
   int		sh_idx;
   int		sn_idx		= sec_name_tbl_idx-MD_NUM_ENTRIES(Sec_Name_Tbl);
   int		sn_name_idx;
   int		stmt_idx;
   int		typ_idx		= type_tbl_idx - MD_NUM_ENTRIES(Type_Tbl);
   int		type_idx;


   TRACE (Func_Entry, "assign_new_idxs_after_input", NULL);

   line		= AT_DEF_LINE(module_attr_idx);
   column	= AT_DEF_COLUMN(module_attr_idx);

   if (keep_module_procs || inline_search) {
      sh_idx	=  sh_tbl_idx - MD_NUM_ENTRIES(Sh_Tbl);
   }
   else {
      sh_idx	= NULL_IDX;
   }

# ifdef _DEBUG
   if (at_idx < 0) {
      PRINTMSG(1, 626, Internal, 0, "positive at_idx",
               "assign_new_idxs_after_input");
   }

   if (bd_idx < 0) {
      PRINTMSG(1, 626, Internal, 0, "positive bd_idx",
               "assign_new_idxs_after_input");
   }

   if (cn_idx < 0) {
      PRINTMSG(1, 626, Internal, 0, "positive cn_idx",
               "assign_new_idxs_after_input");
   }

   if (cp_idx < 0) {
      PRINTMSG(1, 626, Internal, 0, "positive cp_idx",
               "assign_new_idxs_after_input");
   }

   if (il_idx < 0) {
      PRINTMSG(1, 626, Internal, 0, "positive il_idx",
               "assign_new_idxs_after_input");
   }

   if (ir_idx < 0) {
      PRINTMSG(1, 626, Internal, 0, "positive ir_idx",
               "assign_new_idxs_after_input");
   }

   if (np_idx < 0) {
      PRINTMSG(1, 626, Internal, 0, "positive np_idx",
               "assign_new_idxs_after_input");
   }

   if (sn_idx < 0) {
      PRINTMSG(1, 626, Internal, 0, "positive sn_idx",
               "assign_new_idxs_after_input");
   }

   if (typ_idx < 0) {
      PRINTMSG(1, 626, Internal, 0, "positive typ_idx",
               "assign_new_idxs_after_input");
   }
# endif


   /* Keep old_cn_idx because we will be increasing the size of the constant */
   /* table entry and we don't want to overwrite those indexes.              */
   /* Also keep old_il_idx for the same reason.                              */

   old_cn_idx	= const_tbl_idx;
   old_il_idx	= ir_list_tbl_idx;

   /* Also - clear the IL free list pointer, to force the new lists to go  */
   /* to the end of the ir list table, rather than in the middle.          */

   save_il_free_list		= IL_NEXT_LIST_IDX(NULL_IDX);
   IL_NEXT_LIST_IDX(NULL_IDX)	= NULL_IDX;

   /* At this pointer the zeroth entry of the mod link table is all zeros. */
   /* If a field accessing the ML table is zero, it will remain zero,      */
   /* because ML_xx_IDX(0) = 0.                                            */

   new_module_idx = (inline_search) ? NULL_IDX : module_attr_idx;

   for (attr_idx = at_idx+1; attr_idx <= attr_tbl_idx; attr_idx++) {
      CLEAR_TBL_NTRY(attr_aux_tbl, attr_idx);
   }

   for (attr_idx = at_idx+1; attr_idx <= attr_tbl_idx; attr_idx++) {
      ML_AT_SEARCH_ME(attr_idx)		= TRUE;
      AT_DEF_LINE(attr_idx)		= line;
      AT_DEF_COLUMN(attr_idx)		= column;
      AT_NAME_IDX(attr_idx)		= np_idx + AT_NAME_IDX(attr_idx);
      AT_ORIG_NAME_IDX(attr_idx)	= np_idx + AT_ORIG_NAME_IDX(attr_idx);
      AT_ATTR_LINK(attr_idx)		= (AT_ATTR_LINK(attr_idx) == NULL_IDX) ?
                                         NULL_IDX:at_idx+AT_ATTR_LINK(attr_idx);
      if (!inline_search) {
         AT_USE_ASSOCIATED(attr_idx)	= TRUE;
      }

      if (AT_MODULE_IDX(attr_idx) == NULL_IDX) {
         AT_MODULE_IDX(attr_idx)	= new_module_idx; 
         AT_ORIG_MODULE_IDX(attr_idx)	= new_module_idx; 
      }
      else {
         AT_MODULE_IDX(attr_idx)	= at_idx + AT_MODULE_IDX(attr_idx); 
         AT_ORIG_MODULE_IDX(attr_idx)	= at_idx + AT_ORIG_MODULE_IDX(attr_idx);
      }

      switch (AT_OBJ_CLASS(attr_idx)) {
      case Data_Obj:

         if (ATD_ARRAY_IDX(attr_idx) != NULL_IDX) {
            ATD_ARRAY_IDX(attr_idx) += bd_idx;

            /* Share the deferred shape entries with the default entries. */

            if (BD_ARRAY_CLASS(ATD_ARRAY_IDX(attr_idx)) == Deferred_Shape) {
               ATD_ARRAY_IDX(attr_idx) = BD_RANK(ATD_ARRAY_IDX(attr_idx));
            }
         }

# if defined(_F_MINUS_MINUS)
         if (ATD_PE_ARRAY_IDX(attr_idx) != NULL_IDX) {
            ATD_PE_ARRAY_IDX(attr_idx)	+= bd_idx;
         }
# endif

         /* As the storage blocks are read in, they are resolved.  The new    */
         /* storage block index is kept in the module link table and is       */
         /* updated here.                                                     */

         ATD_STOR_BLK_IDX(attr_idx) = ML_SB_IDX(ATD_STOR_BLK_IDX(attr_idx));

         if (ATD_TYPE_IDX(attr_idx) != NULL_IDX) {
            ATD_TYPE_IDX(attr_idx) += typ_idx;
         }

         if (ATD_DISTRIBUTION_IDX(attr_idx) != NULL_IDX) {
            ATD_DISTRIBUTION_IDX(attr_idx) += bd_idx;
         }

         if (ATD_RESHAPE_ARRAY_IDX(attr_idx) != NULL_IDX) {
            ATD_RESHAPE_ARRAY_IDX(attr_idx)	+= bd_idx;
         }

         switch (ATD_CLASS(attr_idx)) {

         case Function_Result:

            ATD_FUNC_IDX(attr_idx) = at_idx + ATD_FUNC_IDX(attr_idx);

            if (ATD_OFFSET_ASSIGNED(attr_idx)) {

               switch (ATD_OFFSET_FLD(attr_idx)) {
               case AT_Tbl_Idx:
                  ATD_OFFSET_IDX(attr_idx) += at_idx;
                     break;

               case CN_Tbl_Idx:
                  ATD_OFFSET_IDX(attr_idx) += cn_idx;
                  break;

               case IR_Tbl_Idx:
                  ATD_OFFSET_IDX(attr_idx) += ir_idx;
                  break;

               case IL_Tbl_Idx:
                  ATD_OFFSET_IDX(attr_idx) += il_idx;
                  break;

               case NO_Tbl_Idx:
                  break;
               }
            }
            break;

         case Constant:

            ATD_CONST_IDX(attr_idx) += (ATD_FLD(attr_idx) == AT_Tbl_Idx) ?
                                       at_idx : cn_idx;
            break;

         case CRI__Pointee:

            ATD_PTR_IDX(attr_idx)   += at_idx;
            break;

         case Compiler_Tmp:

            if (ATD_NEXT_MEMBER_IDX(attr_idx) != NULL_IDX) {
               ATD_NEXT_MEMBER_IDX(attr_idx) += at_idx;
            }

            if (ATD_DEFINING_ATTR_IDX(attr_idx) != NULL_IDX) {
               ATD_DEFINING_ATTR_IDX(attr_idx) += at_idx;
            }

            if (ATD_AUTOMATIC(attr_idx)) {
               ATD_AUTO_BASE_IDX(attr_idx) += at_idx;
            }
            else if (ATD_OFFSET_ASSIGNED(attr_idx)) {

               switch (ATD_OFFSET_FLD(attr_idx)) {
               case AT_Tbl_Idx:
                  ATD_OFFSET_IDX(attr_idx) += at_idx;
                  break;

               case CN_Tbl_Idx:
                  ATD_OFFSET_IDX(attr_idx) += cn_idx;
                  break;

               case IR_Tbl_Idx:
                  ATD_OFFSET_IDX(attr_idx) += ir_idx;
                  break;

               case IL_Tbl_Idx:
                  ATD_OFFSET_IDX(attr_idx) += il_idx;
                  break;

               case NO_Tbl_Idx:
                  ATD_OFFSET_IDX(attr_idx) += cn_idx;
                  ATD_OFFSET_FLD(attr_idx)  = CN_Tbl_Idx;
                  break;
               }
            }

            switch (ATD_FLD(attr_idx)) {
            case CN_Tbl_Idx:
               ATD_TMP_IDX(attr_idx) = cn_idx + ATD_TMP_IDX(attr_idx);
               break;

            case AT_Tbl_Idx:
               ATD_TMP_IDX(attr_idx) = at_idx + ATD_TMP_IDX(attr_idx);
               break;

            case IR_Tbl_Idx:
               ATD_TMP_IDX(attr_idx) = ir_idx + ATD_TMP_IDX(attr_idx);
               break;

            case IL_Tbl_Idx:
               ATD_TMP_IDX(attr_idx) = il_idx + ATD_TMP_IDX(attr_idx);
               break;

            }

            break;

         case Struct_Component:

            switch (ATD_OFFSET_FLD(attr_idx)) {
            case AT_Tbl_Idx:
               ATD_CPNT_OFFSET_IDX(attr_idx) += at_idx;
               break;

            case CN_Tbl_Idx:
               ATD_CPNT_OFFSET_IDX(attr_idx) += cn_idx;
               break;

            case IR_Tbl_Idx:
               ATD_CPNT_OFFSET_IDX(attr_idx) += ir_idx;
               break;

            case IL_Tbl_Idx:
               ATD_CPNT_OFFSET_IDX(attr_idx) += il_idx;
               break;

            case NO_Tbl_Idx:
               ATD_CPNT_OFFSET_IDX(attr_idx) += cn_idx;
               ATD_OFFSET_FLD(attr_idx)	 = CN_Tbl_Idx;
               break;
            }

            if (ATD_CPNT_INIT_IDX(attr_idx) != NULL_IDX) {

               switch (ATD_FLD(attr_idx)) {
               case AT_Tbl_Idx:
                  ATD_CPNT_INIT_IDX(attr_idx) += at_idx;
                  break;

               case CN_Tbl_Idx:
                  ATD_CPNT_INIT_IDX(attr_idx) += cn_idx;
                  break;

               case IR_Tbl_Idx:
                  ATD_CPNT_INIT_IDX(attr_idx) += ir_idx;
                  break;

               case IL_Tbl_Idx:
                  ATD_CPNT_INIT_IDX(attr_idx) += il_idx;
                  break;
               }
            }

            ATD_DERIVED_TYPE_IDX(attr_idx) += at_idx;

            break;

         case Dummy_Argument:

#ifdef _TARGET_OS_UNICOS

            /* Unicos released earlier than everything else.  The following  */
            /* only needs to be done for the very first release.             */

            /* Version 1 has a bad type in this field - BHJ    */
            /* Remove this when we no longer support version 1 */

            if (ATD_INTRIN_DARG(attr_idx)) {
               ATD_TYPE_IDX(attr_idx)	= NULL_IDX;
            }
# endif

            break;

         case Variable:

            if (ATD_FLD(attr_idx) == NO_Tbl_Idx) {

               /* Intentionally blank */

            }
            else if (ATD_FLD(attr_idx) == AT_Tbl_Idx) {
               ATD_VARIABLE_TMP_IDX(attr_idx) += at_idx;
            }
            else if (ATD_FLD(attr_idx) == IL_Tbl_Idx) {
               ATD_VARIABLE_TMP_IDX(attr_idx) += il_idx;
            }

            if (ATD_ASSIGN_TMP_IDX(attr_idx) != NULL_IDX) {
               ATD_ASSIGN_TMP_IDX(attr_idx) += at_idx;
            }

            if (ATD_NEXT_MEMBER_IDX(attr_idx) != NULL_IDX) {
               ATD_NEXT_MEMBER_IDX(attr_idx) += at_idx;
            }

            if (ATD_AUTOMATIC(attr_idx)) {
               ATD_AUTO_BASE_IDX(attr_idx) += at_idx;
            }
            else if (ATD_OFFSET_ASSIGNED(attr_idx)) {

               switch (ATD_OFFSET_FLD(attr_idx)) {
               case AT_Tbl_Idx:
                  ATD_OFFSET_IDX(attr_idx) += at_idx;
                  break;

               case CN_Tbl_Idx:
                  ATD_OFFSET_IDX(attr_idx) += cn_idx;
                  break;

               case IR_Tbl_Idx:
                  ATD_OFFSET_IDX(attr_idx) += ir_idx;
                  break;

               case IL_Tbl_Idx:
                  ATD_OFFSET_IDX(attr_idx) += il_idx;
                  break;

               case NO_Tbl_Idx:
                  ATD_OFFSET_IDX(attr_idx) += cn_idx;
                  ATD_OFFSET_FLD(attr_idx)  = CN_Tbl_Idx;
                  break;
               }
            }

         default:
            break;

         }  /* End switch */

         break;

      case Pgm_Unit:


         ATP_EXT_NAME_IDX(attr_idx)	= np_idx + ATP_EXT_NAME_IDX(attr_idx);

         if (ATP_PGM_UNIT(attr_idx) == Module) {

            if (ATP_MOD_PATH_LEN(attr_idx) > 0) {
               ATP_MOD_PATH_IDX(attr_idx) = np_idx + ATP_MOD_PATH_IDX(attr_idx);
            }

            if (!inline_search) {

               /* CLear - this will be set if this module is referenced as */
               /* stuff is brought in.                                     */

               AT_REFERENCED(attr_idx)	= Not_Referenced;


               /* One of these is the module being read in.  The rest  */
               /* are modules indirectly referenced via this module.   */

               if (ATP_SCP_ALIVE(attr_idx)) { 

                  /* The module being USE associated */

                  AT_ATTR_LINK(attr_idx)	= module_attr_idx;
                  AT_IGNORE_ATTR_LINK(attr_idx)	= TRUE;
                  ATP_MODULE_STR_IDX(attr_idx)	=
                                     ATP_MODULE_STR_IDX(module_attr_idx);
               }
               else {

                  /* Is this module already in this scope, via a previous    */
                  /* use statement, either indirectly or directly?           */

                  list_idx = SCP_USED_MODULE_LIST(curr_scp_idx);

                  while (list_idx != NULL_IDX) {

# ifdef _DEBUG
                     if (AL_ATTR_IDX(list_idx) == NULL_IDX) {
                        PRINTMSG(stmt_start_line, 626, Internal, 0,
                                 "nonzero AL_ATTR_IDX", 
                                 "assign_new_idxs_after_input");
                     }
# endif
                     if (compare_names(AT_OBJ_NAME_LONG(attr_idx),
                                    AT_NAME_LEN(attr_idx),
                                    AT_OBJ_NAME_LONG(AL_ATTR_IDX(list_idx)),
                                    AT_NAME_LEN(AL_ATTR_IDX(list_idx))) == 0) {

                     /* Found the same module.  Temporarily link the two  */
                     /* modules together via AT_ATTR_LINK.  We need this  */
                     /* link, because we need to retain information about */
                     /* where both copies of the same module came from    */
                     /* until we find out if this copy of the module      */
                     /* stays here.  If it does, we will issue a caution  */
                     /* message and a CIF record.                         */
   
                        if (ATP_PGM_UNIT(module_attr_idx) == Module) {
                           AT_ATTR_LINK(attr_idx) = AL_ATTR_IDX(list_idx);
                           AT_IGNORE_ATTR_LINK(attr_idx) = TRUE;
                        }
                        ATP_MODULE_STR_IDX(attr_idx) = 
                                      ATP_MODULE_STR_IDX(AL_ATTR_IDX(list_idx));
                        break;
                     }
                     list_idx	= AL_NEXT_IDX(list_idx);
                  }
                
                  if (ATP_MODULE_STR_IDX(attr_idx) == NULL_IDX) {
                     name_idx = check_global_pgm_unit(attr_idx);
                     ATP_MODULE_STR_IDX(attr_idx) =  GN_NAME_IDX(name_idx);
                  }

                  ATP_INDIRECT_MODULE(attr_idx)	= TRUE;
               }

               /* Add to the top of the local list.		          */

               NTR_ATTR_LIST_TBL(al_idx);
               AL_ATTR_IDX(al_idx)	= attr_idx;

               AL_NEXT_IDX(al_idx)	= list_of_modules_in_module;

               if (list_of_modules_in_module != NULL_IDX) {
                  AL_PREV_MODULE_IDX(list_of_modules_in_module)	= al_idx;
               }
               list_of_modules_in_module			= al_idx;
            }
         }
         else {

            if (ATP_SCP_ALIVE(attr_idx)) { /* The pgm unit being searched for */
               AT_ATTR_LINK(module_attr_idx)		= attr_idx;
               AT_IGNORE_ATTR_LINK(module_attr_idx)	= TRUE;
            }

            /* We could be searching for this alternate entry.  Check */

            if (ATP_ALT_ENTRY(attr_idx) && alternate_entry &&
                (compare_names(AT_OBJ_NAME_LONG(attr_idx),
                               AT_NAME_LEN(attr_idx),
                               AT_OBJ_NAME_LONG(module_attr_idx),
                               AT_NAME_LEN(module_attr_idx)) == 0)) {
               AT_ATTR_LINK(module_attr_idx)		= attr_idx;
               AT_IGNORE_ATTR_LINK(module_attr_idx)	= TRUE;
            }

            if (ATP_FIRST_IDX(attr_idx) != NULL_IDX) {
               ATP_FIRST_IDX(attr_idx) = sn_idx + ATP_FIRST_IDX(attr_idx);
            }

            if (ATP_RSLT_IDX(attr_idx) != NULL_IDX) {
               ATP_RSLT_IDX(attr_idx) = at_idx + ATP_RSLT_IDX(attr_idx);
            }

            if (AT_IS_INTRIN(attr_idx)) {
               ML_AT_SEARCHED(attr_idx)	   = TRUE;
            }

            if (ATP_PROC(attr_idx) != Intrin_Proc &&
                ATP_PROC(attr_idx) != Dummy_Proc) {

               if ((keep_module_procs || 
                    ATP_PGM_UNIT(module_attr_idx) != Module) &&
                   ATP_FIRST_SH_IDX(attr_idx) != NULL_IDX) {

                  /* Here is where we decide whether to keep the IR/SH for */
                  /* the procedure.  If -O inlinable and this is a module  */ 
                  /* or if -O inline[1-3] is specified.                    */ 
                  /* Inlining may be on for this compilation, but that     */
                  /* doesn't mean that the module procedure being read in  */
                  /* actually had IR/SH saved for it.                      */


                  ATP_FIRST_SH_IDX(attr_idx)	+= sh_idx;

                  ATP_PARENT_IDX(attr_idx)	+= at_idx;

                  /* Add to list of procedures to send  */
                  /* across if inlining is turned on.   */


                  /* Should we be adding alternate entry points here KAY */

                  if (opt_flags.inline_lvl > Inline_Lvl_0) {

                     /* The module procedure must be first in the list,  */

                     if (ATP_PROC(attr_idx) == Module_Proc) {
                        NTR_ATTR_LIST_TBL(al_idx);
                        AL_ATTR_IDX(al_idx)	= attr_idx;
                        AL_NEXT_IDX(al_idx)	= SCP_ATTR_LIST(curr_scp_idx);
                        SCP_ATTR_LIST(curr_scp_idx)	= al_idx;
                     }
                     else {
                        ADD_ATTR_TO_LOCAL_LIST(attr_idx);
                     }
                  }
               }
               else {
                  ATP_PARENT_IDX(attr_idx)	= NULL_IDX;
                  ATP_FIRST_SH_IDX(attr_idx)	= NULL_IDX;
               }
            }

         }

         break;

      case Label:

         if (!keep_module_procs && !inline_search) {
            break;     /* Will throw out all labels */
         }

         if (ATL_DIRECTIVE_LIST(attr_idx) != NULL_IDX) {
            ATL_DIRECTIVE_LIST(attr_idx)  += il_idx;
         }

         if (ATL_NEXT_ASG_LBL_IDX(attr_idx) != NULL_IDX) {
            ATL_NEXT_ASG_LBL_IDX(attr_idx) += at_idx;
         }

         if (AT_DEFINED(attr_idx) && ATL_DEF_STMT_IDX(attr_idx) != NULL_IDX) {
            ATL_DEF_STMT_IDX(attr_idx)	+= sh_idx;
         }


         if (ATL_CLASS(attr_idx) == Lbl_Format) {
            ATL_PP_FORMAT_TMP(attr_idx) += at_idx;
            ATL_FORMAT_TMP(attr_idx)    += at_idx;
         }
         else if (ATL_CLASS(attr_idx) == Lbl_User &&
                  ATL_BLK_STMT_IDX(attr_idx) != NULL_IDX) {
            ATL_BLK_STMT_IDX(attr_idx) += sh_idx;
         }
         break;

      case Derived_Type:

         AT_DEFINED(attr_idx)	= TRUE;
         ATT_SCP_IDX(attr_idx)	= curr_scp_idx;

         if (ATT_FIRST_CPNT_IDX(attr_idx) != NULL_IDX) {
            ATT_FIRST_CPNT_IDX(attr_idx)	+= sn_idx;
         }

         if (!ATP_IN_CURRENT_COMPILE(module_attr_idx)) {

            /* If this module was created during this compilation, the index  */
            /* in this field is a valid index, because the global type table  */
            /* stays around for the whole compilation.  By maintaining this   */
            /* index, we save alot of time from searching the global type     */
            /* table to see if this type exists already.  If this module was  */
            /* created in a different compilation, this field must be clared. */

            ATT_GLOBAL_TYPE_IDX(attr_idx)	= NULL_IDX;
         }

         switch (ATT_STRUCT_BIT_LEN_FLD(attr_idx)) {
         case AT_Tbl_Idx:
            ATT_STRUCT_BIT_LEN_IDX(attr_idx) += at_idx;
            break;

         case CN_Tbl_Idx:
            ATT_STRUCT_BIT_LEN_IDX(attr_idx) += cn_idx;
            break;

         case IR_Tbl_Idx:
            ATT_STRUCT_BIT_LEN_IDX(attr_idx) += ir_idx;
            break;

         case IL_Tbl_Idx:
            ATT_STRUCT_BIT_LEN_IDX(attr_idx) += il_idx;
            break;

         case NO_Tbl_Idx:
            ATT_STRUCT_BIT_LEN_FLD(attr_idx)  = CN_Tbl_Idx;
            ATT_STRUCT_BIT_LEN_IDX(attr_idx) += cn_idx;
            break;
         }
         break;

      case Interface:

         /* We do not search for duplicate interface blocks, because */
         /* these blocks get concatenated, so we have to go through  */
         /* all the specific interfaces and throw out duplicates.    */

         ML_AT_SEARCHED(attr_idx)	   = TRUE;
         ATI_FIRST_SPECIFIC_IDX(attr_idx) += sn_idx; 

         if (ATD_TYPE_IDX(attr_idx) != NULL_IDX) {
            ATD_TYPE_IDX(attr_idx)	  += typ_idx;
         }

         if (ATI_PROC_IDX(attr_idx) != NULL_IDX) {
            ATI_PROC_IDX(attr_idx)	  += at_idx;
         }

         break;

      case Namelist_Grp:
   
         if (ATN_NAMELIST_DESC(attr_idx) != NULL_IDX) {
            ATN_NAMELIST_DESC(attr_idx)  = at_idx + ATN_NAMELIST_DESC(attr_idx);
         }

         ATN_FIRST_NAMELIST_IDX(attr_idx)	+= sn_idx;
         ATN_LAST_NAMELIST_IDX(attr_idx)	+= sn_idx; 
         break;

      case Stmt_Func:

         if (ATD_TYPE_IDX(attr_idx) != NULL_IDX) {
            ATD_TYPE_IDX(attr_idx)	  += typ_idx;
         }

         if (ATP_FIRST_IDX(attr_idx) != NULL_IDX) {
            ATP_FIRST_IDX(attr_idx) = sn_idx + ATP_FIRST_IDX(attr_idx);
         }

         switch (ATS_SF_FLD(attr_idx)) {
         case CN_Tbl_Idx:
            ATS_SF_IDX(attr_idx) = cn_idx + ATS_SF_IDX(attr_idx);
            break;

         case AT_Tbl_Idx:
            ATS_SF_IDX(attr_idx) = at_idx + ATS_SF_IDX(attr_idx);
            break;

         case IR_Tbl_Idx:
            ATS_SF_IDX(attr_idx) = ir_idx + ATS_SF_IDX(attr_idx);
            break;

         case IL_Tbl_Idx:
            ATS_SF_IDX(attr_idx) = il_idx + ATS_SF_IDX(attr_idx);
            break;
         }

         break;
      }  /* End switch */
   }

   bounds_idx = bd_idx + 1;

   while (bounds_idx <= bounds_tbl_idx) {
      BD_LINE_NUM(bounds_idx)	= line;
      BD_COLUMN_NUM(bounds_idx)	= column;

      if (!ATP_IN_CURRENT_COMPILE(module_attr_idx)) {

         /* If this module was created during this compilation, the index  */
         /* in this field is a valid index, because the global bounds table*/
         /* stays around for the whole compilation.  By maintaining this   */
         /* index, we save alot of time from searching the global bounds   */
         /* table to see if this bound exists already.  If this module was */
         /* created in a different compilation, this field must be clared. */

         BD_GLOBAL_IDX(bounds_idx)   = NULL_IDX;
      }

      if (BD_DIST_NTRY(bounds_idx)) {

         for (dim = 1; dim <= BD_RANK(bounds_idx); dim++) {

            if (BD_CYCLIC_FLD(bounds_idx, dim) == CN_Tbl_Idx) {
               BD_CYCLIC_IDX(bounds_idx, dim) = 
                                  cn_idx + BD_CYCLIC_IDX(bounds_idx, dim);
            }
            else if (BD_CYCLIC_FLD(bounds_idx, dim) == AT_Tbl_Idx) {
               BD_CYCLIC_IDX(bounds_idx, dim) = 
                                  at_idx + BD_CYCLIC_IDX(bounds_idx, dim);
            }

            if (BD_ONTO_FLD(bounds_idx, dim) == CN_Tbl_Idx) {
               BD_ONTO_IDX(bounds_idx, dim) = 
                                  cn_idx + BD_ONTO_IDX(bounds_idx, dim);
            }
            else if (BD_ONTO_FLD(bounds_idx, dim) == AT_Tbl_Idx) {
               BD_ONTO_IDX(bounds_idx, dim) = 
                                  at_idx + BD_ONTO_IDX(bounds_idx, dim);
            }
         }
         bounds_idx = bounds_idx + BD_RANK(bounds_idx);
      }
      else if (BD_ARRAY_CLASS(bounds_idx) != Deferred_Shape) {

         if (BD_LEN_FLD(bounds_idx) == CN_Tbl_Idx) {
            BD_LEN_IDX(bounds_idx) = cn_idx + BD_LEN_IDX(bounds_idx);
         }
         else if (BD_LEN_FLD(bounds_idx) == AT_Tbl_Idx) {
            BD_LEN_IDX(bounds_idx) = at_idx + BD_LEN_IDX(bounds_idx);
         }

         for (dim = 1; dim <= BD_RANK(bounds_idx); dim++) {

            if (BD_LB_FLD(bounds_idx, dim) == CN_Tbl_Idx) {
               BD_LB_IDX(bounds_idx, dim) = cn_idx + BD_LB_IDX(bounds_idx, dim);
            }
            else if (BD_LB_FLD(bounds_idx, dim) == AT_Tbl_Idx) {
               BD_LB_IDX(bounds_idx, dim) = at_idx + BD_LB_IDX(bounds_idx, dim);
            }

            if (BD_UB_FLD(bounds_idx, dim) == CN_Tbl_Idx) {
               BD_UB_IDX(bounds_idx, dim) = cn_idx + BD_UB_IDX(bounds_idx, dim);
            }
            else if (BD_UB_FLD(bounds_idx, dim) == AT_Tbl_Idx) {
               BD_UB_IDX(bounds_idx, dim) = at_idx + BD_UB_IDX(bounds_idx, dim);
            }

            if (BD_XT_FLD(bounds_idx, dim) == CN_Tbl_Idx) {
               BD_XT_IDX(bounds_idx, dim) = cn_idx + BD_XT_IDX(bounds_idx, dim);
            }
            else if (BD_XT_FLD(bounds_idx, dim) == AT_Tbl_Idx) {
               BD_XT_IDX(bounds_idx, dim) = at_idx + BD_XT_IDX(bounds_idx, dim);
            }

            if (BD_SM_FLD(bounds_idx, dim) == CN_Tbl_Idx) {
               BD_SM_IDX(bounds_idx, dim) = cn_idx + BD_SM_IDX(bounds_idx, dim);
            }
            else if (BD_SM_FLD(bounds_idx, dim) == AT_Tbl_Idx) {
               BD_SM_IDX(bounds_idx, dim) = at_idx + BD_SM_IDX(bounds_idx, dim);
            }
         }
         bounds_idx = bounds_idx + BD_RANK(bounds_idx);
      }
      ++bounds_idx;
   }

   for (sn_name_idx = sn_idx+1; sn_name_idx <= sec_name_tbl_idx; sn_name_idx++){
      SN_LINE_NUM(sn_name_idx)	= line;
      SN_COLUMN_NUM(sn_name_idx)= column;
      SN_NAME_IDX(sn_name_idx)	= np_idx + SN_NAME_IDX(sn_name_idx);
      SN_ATTR_IDX(sn_name_idx)	= at_idx + SN_ATTR_IDX(sn_name_idx);

      if (SN_SIBLING_LINK(sn_name_idx) != NULL_IDX) {
         SN_SIBLING_LINK(sn_name_idx) = sn_idx + SN_SIBLING_LINK(sn_name_idx);
      }
   }

# if defined(_DEBUG)
   for (const_idx = cn_idx+1; const_idx <= old_cn_idx; const_idx++) {

      if (CN_POOL_IDX(const_idx) == NULL_IDX) {
         PRINTMSG(stmt_start_line, 1349, Internal, 0, const_idx);
      }
   }
# endif

   for (const_idx = cn_idx+1; const_idx <= old_cn_idx; const_idx++) {
      CN_TYPE_IDX(const_idx)	= typ_idx + CN_TYPE_IDX(const_idx);
      CN_POOL_IDX(const_idx)	= cp_idx + CN_POOL_IDX(const_idx);
   }

   for (type_idx = typ_idx+1; type_idx <= type_tbl_idx; type_idx++) {

      if (TYP_TYPE(type_idx) == Character) {

         if (TYP_FLD(type_idx) == AT_Tbl_Idx) {
            TYP_IDX(type_idx) = at_idx + TYP_IDX(type_idx);

            if (TYP_ORIG_LEN_IDX(type_idx) != NULL_IDX) {
               TYP_ORIG_LEN_IDX(type_idx) = at_idx + TYP_ORIG_LEN_IDX(type_idx);
            }
         }
         else if (TYP_FLD(type_idx) == CN_Tbl_Idx) {
            TYP_IDX(type_idx) = cn_idx + TYP_IDX(type_idx);
         }
      }
      else if (TYP_TYPE(type_idx) == Structure) {
         TYP_IDX(type_idx)   = at_idx + TYP_IDX(type_idx);
      }

# if defined(_HOST32) && defined(_TARGET32)

      else if (MD_VERSION_NUM <= MD_LAST_4_0_VERSION && 
               TYP_TYPE(type_idx) == Typeless) {

         /* A 32 bit compile now carries a 64 bit type length */

         TYP_BIT_LEN(type_idx) = (long64) type_tbl[type_idx].wd1.old_bit_len;
      }
# endif
   }

   for (list_idx = il_idx+1; list_idx <= old_il_idx; list_idx++) {

      if (IL_NEXT_LIST_IDX(list_idx) != NULL_IDX) {
         IL_NEXT_LIST_IDX(list_idx) = il_idx + IL_NEXT_LIST_IDX(list_idx);
      }

      if (IL_ARG_DESC_VARIANT(list_idx)) {
         IL_ARG_DESC_IDX(list_idx)	= NULL_IDX;
      }
      else if (IL_PREV_LIST_IDX(list_idx) != NULL_IDX) {
         IL_PREV_LIST_IDX(list_idx) = il_idx + IL_PREV_LIST_IDX(list_idx);
      }

      switch (IL_FLD(list_idx)) {
      case CN_Tbl_Idx:
         IL_IDX(list_idx)	= cn_idx + IL_IDX(list_idx);
         IL_LINE_NUM(list_idx)	= line;
         IL_COL_NUM(list_idx)	= column;
         break;

      case AT_Tbl_Idx:
         IL_IDX(list_idx)	= at_idx + IL_IDX(list_idx);
         IL_LINE_NUM(list_idx)	= line;
         IL_COL_NUM(list_idx)	= column;
         break;
      
      case IL_Tbl_Idx:
         IL_IDX(list_idx)	= il_idx + IL_IDX(list_idx);

         /* Do not update line num.  It's not a line number */

         break;

      case IR_Tbl_Idx:
         IL_IDX(list_idx)	= ir_idx + IL_IDX(list_idx);
         IL_LINE_NUM(list_idx)	= line;
         IL_COL_NUM(list_idx)	= column;
         break;
      }
   }

   if (keep_module_procs || inline_search) {

      for (stmt_idx = sh_idx+1; stmt_idx <= sh_tbl_idx; stmt_idx++) {
         SH_GLB_LINE(stmt_idx)	= line;
         SH_COL_NUM(stmt_idx)	= column;

         if (SH_NEXT_IDX(stmt_idx) != NULL_IDX) {
            SH_NEXT_IDX(stmt_idx) = sh_idx + SH_NEXT_IDX(stmt_idx);
         }

         if (SH_PREV_IDX(stmt_idx) != NULL_IDX) {
            SH_PREV_IDX(stmt_idx) = sh_idx + SH_PREV_IDX(stmt_idx);
         }

         if (SH_PARENT_BLK_IDX(stmt_idx) != NULL_IDX &&
             SH_STMT_TYPE(stmt_idx) != Statement_Num_Stmt) {
            SH_PARENT_BLK_IDX(stmt_idx) = sh_idx + SH_PARENT_BLK_IDX(stmt_idx);
         }

         if (SH_IR_IDX(stmt_idx) != NULL_IDX) {
            SH_IR_IDX(stmt_idx) = ir_idx + SH_IR_IDX(stmt_idx);
         }
      }
   }

   

   for (mod_idx = ir_idx+1; mod_idx <= ir_tbl_idx; mod_idx++) {

      if (IR_TYPE_IDX(mod_idx) != NULL_IDX) {
         IR_TYPE_IDX(mod_idx)	= typ_idx + IR_TYPE_IDX(mod_idx);
      }
      IR_LINE_NUM(mod_idx)	= line;
      IR_COL_NUM(mod_idx)	= column;

      switch (IR_FLD_L(mod_idx)) {
      case CN_Tbl_Idx:
         IR_IDX_L(mod_idx)	= cn_idx + IR_IDX_L(mod_idx);
         IR_LINE_NUM_L(mod_idx)	= line;
         IR_COL_NUM_L(mod_idx)	= column;
         break;

      case AT_Tbl_Idx:
         IR_IDX_L(mod_idx) = at_idx + IR_IDX_L(mod_idx);
         IR_LINE_NUM_L(mod_idx)	= line;
         IR_COL_NUM_L(mod_idx)	= column;
         break;
      
      case IL_Tbl_Idx:
         IR_IDX_L(mod_idx) = il_idx + IR_IDX_L(mod_idx);

         /* Do not update line num.  It's not a line number */

         break;

      case IR_Tbl_Idx:
         IR_IDX_L(mod_idx) = ir_idx + IR_IDX_L(mod_idx);
         IR_LINE_NUM_L(mod_idx)	= line;
         IR_COL_NUM_L(mod_idx)	= column;
         break;

      case SH_Tbl_Idx:
         IR_IDX_L(mod_idx) = sh_idx + IR_IDX_L(mod_idx);
         IR_LINE_NUM_L(mod_idx)	= line;
         IR_COL_NUM_L(mod_idx)	= column;
         break;
      }

      switch (IR_FLD_R(mod_idx)) {
      case CN_Tbl_Idx:
         IR_IDX_R(mod_idx) = cn_idx + IR_IDX_R(mod_idx);
         IR_LINE_NUM_R(mod_idx)	= line;
         IR_COL_NUM_R(mod_idx)	= column;
         break;

      case AT_Tbl_Idx:
         IR_IDX_R(mod_idx) = at_idx + IR_IDX_R(mod_idx);
         IR_LINE_NUM_R(mod_idx)	= line;
         IR_COL_NUM_R(mod_idx)	= column;
         break;
      
      case IL_Tbl_Idx:
         IR_IDX_R(mod_idx) = il_idx + IR_IDX_R(mod_idx);

         /* Do not update line num.  It's not a line number */

         break;

      case IR_Tbl_Idx:
         IR_IDX_R(mod_idx) = ir_idx + IR_IDX_R(mod_idx);
         IR_LINE_NUM_R(mod_idx)	= line;
         IR_COL_NUM_R(mod_idx)	= column;
         break;

      case SH_Tbl_Idx:
         IR_IDX_R(mod_idx) = sh_idx + IR_IDX_R(mod_idx);
         IR_LINE_NUM_R(mod_idx)	= line;
         IR_COL_NUM_R(mod_idx)	= column;
         break;
      }
   }

   for (attr_idx = at_idx+1; attr_idx <= attr_tbl_idx; attr_idx++) {

      if (AT_IS_INTRIN(attr_idx) && AT_OBJ_CLASS(attr_idx) == Interface) {

#ifdef KEY /* Bug 5089 */
       /* If we're working on an "intrinsic module", then we don't expect
        * to find this interface in the table of intrinsic procedures. */
       if (!AT_IS_INTRIN(AT_MODULE_IDX(attr_idx))) {
#endif /* KEY Bug 5089 */

         /* Find this intrinsic in the current table.  Bring it in */
         /* and merge this interface with the intrinsic interface  */
         /* so old types, enums and indexes get set correctly.     */

         update_intrinsic(attr_idx);
#ifdef KEY /* Bug 5089 */
       }
#endif /* KEY Bug 5089 */
      }
   }

   IL_NEXT_LIST_IDX(NULL_IDX)	= save_il_free_list;

   TRACE (Func_Exit, "assign_new_idxs_after_input", NULL);

   return;

}   /* assign_new_idxs_after_input */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	This routine resolves two items with the same name in the local name  *|
|*	table during USE processing.  This resolves the MODULE name, concats  *|
|*	interfaces, checks to see if the program unit name or dummy arg names *|
|*	are being USED and finds out if this item is really NOT VISIBLE.  If  *|
|*	the item is from the same original module and has the same name, it   *|
|*	is VISIBLE.  This routine checks all of the above things and returns  *|
|*	TRUE it the new_attr is used.  It returns FALSE, if the new_attr is   *|
|*	not used.  Following is an example of NOT_VISIBLE code.               *|
|*									      *|
|*		MODULE A                                                      *|
|*		  integer B(100)                                              *|
|*		END MODULE A                                                  *|
|*		MODULE Z                                                      *|
|*		  use A                                                       *|
|*		END MODULE Z                                                  *|
|*		MODULE Y                                                      *|
|*		  use A                                                       *|
|*		  use Z        ! This is legal and B is visible               *|
|*		END MODULE Y                                                  *|
|*                                                                            *|
|*		MODULE A                                                      *|
|*		  integer B(100)                                              *|
|*		END MODULE A                                                  *|
|*		MODULE Z                                                      *|
|*		use A, C=>B                                                   *|
|*		END MODULE Z                                                  *|
|*		MODULE Y                                                      *|
|*		  use A                                                       *|
|*		  use Z, B=>C  ! This is legal and B is visible               *|
|*		END MODULE Y                                                  *|
|*									      *|
|* Input parameters:							      *|
|*	new_attr_idx	=> The attr index of the attr from the USE module.    *|
|*	old_name_idx	=> The local name tabl index of the attr found in the *|
|*	                   current scope's name table.                        *|
|*	module_attr_idx	=> The attr index of the USE module.                  *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NONE								      *|
|*									      *|
\******************************************************************************/
static void	not_visible_semantics(int	new_attr_idx,
				      int	old_name_idx,
				      int	module_attr_idx)

{
   int			il_idx;
   int			old_attr_idx;
   boolean		same_module;
   int			save_new_attr;
   int			sn_attr_idx;
   int			sn_idx;
   int			srch_il_idx;
   int			srch_sn_idx;


   TRACE (Func_Entry, "not_visible_semantics", NULL);

   old_attr_idx			= LN_ATTR_IDX(old_name_idx);
   save_new_attr		= NULL_IDX;
   only_update_new_tbl_entries	= FALSE;

   if (AT_OBJ_CLASS(new_attr_idx) == Interface &&
       AT_OBJ_CLASS(old_attr_idx) == Pgm_Unit &&
       ATI_PROC_IDX(new_attr_idx) != NULL_IDX) {
      save_new_attr	= new_attr_idx;
      new_attr_idx	= ATI_PROC_IDX(new_attr_idx);
   }
   else if (AT_OBJ_CLASS(old_attr_idx) == Interface &&
            AT_OBJ_CLASS(new_attr_idx) == Pgm_Unit &&
            ATI_PROC_IDX(old_attr_idx) != NULL_IDX) {
      old_attr_idx = ATI_PROC_IDX(old_attr_idx);
   }

   if (ML_AT_IDX(new_attr_idx) == old_attr_idx) {

      /* Resolve_attr found that this object is the same object.     */
      /* All references to this attr will be replaced with the       */
      /* old attr.  This local name entry does not have to be        */
      /* entered because one already exists, so just extt.           */

      /* Intentionally blank.                                        */
   }
   else if (AT_OBJ_CLASS(new_attr_idx) == Pgm_Unit &&
            ATP_PGM_UNIT(new_attr_idx) == Module) {

      /* Resolve_used_modules handles module names.                  */

      /* Intentionally blank.                                        */

   }
   else if (AT_USE_ASSOCIATED(old_attr_idx)) {

      /* We know the attrs have the same name.  Check if they are    */
      /* from the same module and have the same original name index. */

      same_module = (ATP_MODULE_STR_IDX(AT_MODULE_IDX(new_attr_idx)) == 
                     ATP_MODULE_STR_IDX(AT_MODULE_IDX(old_attr_idx)) ||
                     AT_ATTR_LINK(AT_MODULE_IDX(new_attr_idx)) == 
                                  AT_MODULE_IDX(old_attr_idx));

      if (same_module &&
          (compare_names(AT_ORIG_NAME_LONG(new_attr_idx),
                         AT_ORIG_NAME_LEN(new_attr_idx),
                         AT_ORIG_NAME_LONG(old_attr_idx),
                         AT_ORIG_NAME_LEN(old_attr_idx)) == 0)) {

         if (AT_OBJ_CLASS(old_attr_idx) == Interface) {
            merge_interfaces(new_attr_idx, old_attr_idx);
            KEEP_ATTR(old_attr_idx);
         }
         else {

            if (save_new_attr != NULL_IDX) {

               /* Specific case when the new attr is an interface */
               /* with the same name as the old attr.             */

               LN_ATTR_IDX(old_name_idx) = save_new_attr;
               LN_NAME_IDX(old_name_idx) = AT_NAME_IDX(save_new_attr);

               KEEP_ATTR(save_new_attr);
            }

            /* All references to this attr will be replaced with the    */
            /* old attr.  This local name entry does not have to be     */
            /* entered because one already exists, so just exit.        */

            ML_AT_IDX(new_attr_idx)	= old_attr_idx;
            ML_AT_KEEP_ME(new_attr_idx)	= FALSE;

            KEEP_ATTR(old_attr_idx);

            /* There may be references to associated attrs.  These attrs  */
            /* need to get set so that they will point to there new attr. */

            switch (AT_OBJ_CLASS(new_attr_idx)) {
            case Data_Obj:
         
               if (ATD_CLASS(new_attr_idx) == Variable && 
                   ATD_VARIABLE_TMP_IDX(new_attr_idx) != NULL_IDX) {

                  if (ATD_FLD(new_attr_idx) == AT_Tbl_Idx) {
                     ML_AT_IDX(ATD_VARIABLE_TMP_IDX(new_attr_idx)) =
                               ATD_VARIABLE_TMP_IDX(old_attr_idx);
                     ML_AT_KEEP_ME(ATD_VARIABLE_TMP_IDX(new_attr_idx))	= FALSE;
                  }
                  else if (ATD_FLD(new_attr_idx) == IL_Tbl_Idx) {
                     il_idx		= ATD_VARIABLE_TMP_IDX(new_attr_idx);
                     srch_il_idx	= ATD_VARIABLE_TMP_IDX(old_attr_idx);

                     while (il_idx != NULL_IDX) {
                        ML_AT_IDX(IL_IDX(il_idx))	= IL_IDX(srch_il_idx);
                        ML_AT_KEEP_ME(IL_IDX(il_idx))	= FALSE;
                        il_idx		= IL_NEXT_LIST_IDX(il_idx);
                        srch_il_idx	= IL_NEXT_LIST_IDX(srch_il_idx);
                     }
                  }
               }

               if (ATD_CLASS(new_attr_idx) == Constant && 
                                       ATD_FLD(new_attr_idx) == AT_Tbl_Idx) {
                  ML_AT_IDX(ATD_CONST_IDX(new_attr_idx)) = 
                                              ATD_CONST_IDX(old_attr_idx);
                  ML_AT_KEEP_ME(ATD_CONST_IDX(new_attr_idx))	= FALSE;
               }
               break;

            case Pgm_Unit:

               if (ATP_PGM_UNIT(new_attr_idx) == Function ||
                   ATP_PGM_UNIT(new_attr_idx) == Subroutine) {
                  srch_sn_idx	= ATP_FIRST_IDX(old_attr_idx);

                  for (sn_idx = ATP_FIRST_IDX(new_attr_idx); 
                       sn_idx < (ATP_FIRST_IDX(new_attr_idx) + 
                                 ATP_NUM_DARGS(new_attr_idx));
                       sn_idx++) {

                     ML_AT_IDX(SN_ATTR_IDX(sn_idx)) = SN_ATTR_IDX(srch_sn_idx);
                     ML_AT_KEEP_ME(SN_ATTR_IDX(sn_idx))	= FALSE;
                     srch_sn_idx++;
                  }

                  if (ATP_PGM_UNIT(new_attr_idx) == Function){
                     ML_AT_IDX(ATP_RSLT_IDX(new_attr_idx)) =
                               ATP_RSLT_IDX(old_attr_idx);
                     ML_AT_KEEP_ME(ATP_RSLT_IDX(new_attr_idx)) = FALSE;
                  }
               }
               break;

            case Derived_Type:
               sn_idx		= ATT_FIRST_CPNT_IDX(new_attr_idx);
               srch_sn_idx	= ATT_FIRST_CPNT_IDX(old_attr_idx);

               while (sn_idx != NULL_IDX && srch_sn_idx != NULL_IDX) {
                  ML_AT_IDX(SN_ATTR_IDX(sn_idx)) = SN_ATTR_IDX(srch_sn_idx);
                  ML_AT_KEEP_ME(SN_ATTR_IDX(sn_idx))	= FALSE;
                  sn_idx	= SN_SIBLING_LINK(sn_idx);
                  srch_sn_idx	= SN_SIBLING_LINK(srch_sn_idx);
               }
               break;

            }  /* End switch */
         }
      }
      else if (AT_OBJ_CLASS(old_attr_idx) == Interface &&
               AT_OBJ_CLASS(new_attr_idx) == Interface &&
               ATI_DEFINED_OPR(old_attr_idx) == ATI_DEFINED_OPR(new_attr_idx) &&
               ATI_INTERFACE_CLASS(old_attr_idx) == 
               ATI_INTERFACE_CLASS(new_attr_idx)) {
         merge_interfaces(new_attr_idx, old_attr_idx);
         KEEP_ATTR(old_attr_idx);
      }
      else if (AT_OBJ_CLASS(old_attr_idx) == Pgm_Unit &&
               ATP_IN_CURRENT_COMPILE(old_attr_idx)) {
         PRINTMSG(AT_DEF_LINE(new_attr_idx), 1053, Error,
                  AT_DEF_COLUMN(new_attr_idx),
                  AT_OBJ_NAME_PTR(new_attr_idx),
                  AT_OBJ_NAME_PTR(AT_MODULE_IDX(old_attr_idx)));
         AT_DCL_ERR(old_attr_idx)	= TRUE;
         AT_DCL_ERR(new_attr_idx)	= TRUE;

         /* Need to keep new attribute in case other new stuff uses it. */

         KEEP_ATTR(new_attr_idx);
      }
      else {
         AT_NOT_VISIBLE(old_attr_idx)	= TRUE;
         AT_NOT_VISIBLE(new_attr_idx)	= TRUE;

         if (AT_OBJ_CLASS(old_attr_idx) == Interface &&
             ATI_PROC_IDX(old_attr_idx) != NULL_IDX) {
            old_attr_idx = ATI_PROC_IDX(old_attr_idx);
            AT_NOT_VISIBLE(old_attr_idx) = TRUE;
         }

         if (AT_OBJ_CLASS(new_attr_idx) == Interface &&
             ATI_PROC_IDX(new_attr_idx) != NULL_IDX) {
            new_attr_idx = ATI_PROC_IDX(new_attr_idx);
            AT_NOT_VISIBLE(new_attr_idx) = TRUE;
         }

         if (AT_OBJ_CLASS(old_attr_idx) == Pgm_Unit &&
             ATP_PGM_UNIT(old_attr_idx) == Function &&
             !ATP_RSLT_NAME(old_attr_idx)) {
            AT_NOT_VISIBLE(ATP_RSLT_IDX(old_attr_idx)) = TRUE;
         }

         if (AT_OBJ_CLASS(new_attr_idx) == Pgm_Unit &&
             ATP_PGM_UNIT(new_attr_idx) == Function &&
             !ATP_RSLT_NAME(new_attr_idx)) {
            AT_NOT_VISIBLE(ATP_RSLT_IDX(new_attr_idx)) = TRUE;
         }

         /* Need to keep new attribute in case other new stuff uses it. */

         KEEP_ATTR(new_attr_idx);
      }
   }
   else {

      if (AT_OBJ_CLASS(old_attr_idx) == Data_Obj &&
          ATD_SYMBOLIC_CONSTANT(old_attr_idx) &&
          AT_OBJ_CLASS(new_attr_idx) == Data_Obj &&
          ATD_SYMBOLIC_CONSTANT(new_attr_idx)) {

         /* All references to new attr will be replaced with old attr. */

         ML_AT_IDX(new_attr_idx)	= old_attr_idx;
         ML_AT_KEEP_ME(new_attr_idx)	= FALSE;

         KEEP_ATTR(old_attr_idx);
      }
      else if (old_attr_idx == SCP_ATTR_IDX(curr_scp_idx)) {
         PRINTMSG(AT_DEF_LINE(module_attr_idx), 736, Error, 
                  AT_DEF_COLUMN(module_attr_idx),
                  AT_OBJ_NAME_PTR(old_attr_idx),
                  AT_OBJ_NAME_PTR(module_attr_idx));
         AT_DCL_ERR(old_attr_idx) = TRUE;
         AT_DCL_ERR(new_attr_idx) = TRUE;

         /* Need to keep new attribute in case other new stuff uses it. */

         KEEP_ATTR(new_attr_idx);
      }
      else if (AT_OBJ_CLASS(old_attr_idx) == Data_Obj &&
               ATD_CLASS(old_attr_idx) == Dummy_Argument) {
         
# ifdef _DEBUG
         sn_attr_idx = srch_kwd_name(AT_OBJ_NAME_PTR(old_attr_idx),
                                     AT_NAME_LEN(old_attr_idx),
                                     SCP_ATTR_IDX(curr_scp_idx),
                                     &sn_idx);

         if (sn_attr_idx == NULL_IDX) {
            PRINTMSG(AT_DEF_LINE(old_attr_idx), 989, Internal, 
                     AT_DEF_COLUMN(old_attr_idx),
                     AT_OBJ_NAME_PTR(old_attr_idx));
         }
# endif
         PRINTMSG(AT_DEF_LINE(module_attr_idx), 731, Error, 
                  AT_DEF_COLUMN(module_attr_idx),
                  AT_OBJ_NAME_PTR(old_attr_idx),
                  AT_OBJ_NAME_PTR(SCP_ATTR_IDX(curr_scp_idx)),
                  AT_OBJ_NAME_PTR(module_attr_idx));
         AT_DCL_ERR(old_attr_idx) = TRUE;
         AT_DCL_ERR(new_attr_idx) = TRUE;

         /* Need to keep new attribute in case other new stuff uses it. */

         KEEP_ATTR(new_attr_idx);
      }
      else if (ATP_PGM_UNIT(SCP_ATTR_IDX(curr_scp_idx)) == Function &&
               ATP_RSLT_IDX(SCP_ATTR_IDX(curr_scp_idx)) == old_attr_idx) {

         PRINTMSG(AT_DEF_LINE(module_attr_idx), 988, Error, 
                  AT_DEF_COLUMN(module_attr_idx),
                  AT_OBJ_NAME_PTR(old_attr_idx),
                  AT_OBJ_NAME_PTR(SCP_ATTR_IDX(curr_scp_idx)),
                  AT_OBJ_NAME_PTR(module_attr_idx));
         AT_DCL_ERR(old_attr_idx)		= TRUE;
         AT_DCL_ERR(new_attr_idx)		= TRUE;
         AT_DCL_ERR(SCP_ATTR_IDX(curr_scp_idx))	= TRUE;

         /* Need to keep new attribute in case other new stuff uses it. */

         KEEP_ATTR(new_attr_idx);
      }
      else if (AT_OBJ_CLASS(old_attr_idx) == Derived_Type ||
               AT_REFERENCED(old_attr_idx) != Not_Referenced) {

         /* If this is a derived type it was used to declare the function's */
         /* type.  Copy the new attribute information to the old attribute  */
         /* and keep the old attribute.  Do not keep the new attribute.     */

         /* Otherwise this object was used in a declaration bound for the   */
         /* function.  Copy the new attribute information to the old attr   */
         /* and keep the old attribute.  Do not keep the new attribute.     */

         /* BHJ - Currently all old_attr_idx's get all their indexes updated. */

         COPY_ATTR_NTRY(AT_WORK_IDX, old_attr_idx);
         COPY_ATTR_NTRY(old_attr_idx, new_attr_idx); /* Old = new */

         AT_REFERENCED(old_attr_idx)	= AT_REFERENCED(AT_WORK_IDX);
         AT_DEF_LINE(old_attr_idx)	= AT_DEF_LINE(AT_WORK_IDX);
         AT_DEF_COLUMN(old_attr_idx)	= AT_DEF_COLUMN(AT_WORK_IDX);
         AT_DEFINED(old_attr_idx)	= AT_DEFINED(AT_WORK_IDX);
         AT_ACTUAL_ARG(old_attr_idx)	= AT_ACTUAL_ARG(AT_WORK_IDX);
         AT_CIF_SYMBOL_ID(old_attr_idx)	= AT_CIF_SYMBOL_ID(AT_WORK_IDX);
         AT_DCL_ERR(old_attr_idx)	= AT_DCL_ERR(AT_WORK_IDX);

         KEEP_ATTR(new_attr_idx);

         ML_AT_KEEP_ME(new_attr_idx)	= FALSE;

         /* All references to new attr will be replaced with old attr. */

         ML_AT_IDX(new_attr_idx)	= old_attr_idx;
         LN_DEF_LOC(old_name_idx)	= TRUE;
         AT_DEFINED(old_attr_idx)	= TRUE;
      }
      else if (AT_OBJ_CLASS(old_attr_idx) == Label && 
               AT_DEF_LINE(old_attr_idx) == stmt_start_line) {

         /* Use stmts are not processed until the statement following them */
         /* is identified.  This stmt may have a construct name on it and  */
         /* that name is in the symbol table.  Call fnd_semantic_err to    */
         /* issue a message, because the construct name was declared after */
         /* the USE stmt that brought in the same-named item.  Mark        */
         /* AT_USE_ASSOCIATED flag so the correct message comes out.       */

         AT_USE_ASSOCIATED(old_attr_idx)	= TRUE;

         fnd_semantic_err(Obj_Construct,
                          AT_DEF_LINE(old_attr_idx),
                          AT_DEF_COLUMN(old_attr_idx),
                          old_attr_idx,
                          TRUE);

         /* Set local name table to point to new attr.  Old construct attr */
         /* is no longer in the name table.  Keep new attribute entry.     */

         LN_ATTR_IDX(old_name_idx)		= new_attr_idx;
         LN_NAME_IDX(old_name_idx)		= AT_NAME_IDX(new_attr_idx);
         LN_NAME_LEN(old_name_idx)		= AT_NAME_LEN(new_attr_idx);
         AT_DCL_ERR(new_attr_idx)		= TRUE;
         KEEP_ATTR(new_attr_idx);
      }
      else if (num_prog_unit_errors == 0) {

         /* There should not be anything in the local scope. */

         PRINTMSG(AT_DEF_LINE(module_attr_idx), 81, Internal, 
                  AT_DEF_COLUMN(module_attr_idx),
                  AT_OBJ_NAME_PTR(old_attr_idx),
                  old_attr_idx);
      }
      else {

         AT_DCL_ERR(new_attr_idx)	= TRUE;

         /* Need to keep new attribute in case other new stuff uses it. */

         KEEP_ATTR(new_attr_idx);
      }
   }

   TRACE (Func_Exit, "not_visible_semantics", NULL);

   return;

}   /* not_visible_semantics */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	This routine goes through the list of specific routines attached to   *|
|*	the new interface and matches them to the specific routines in the    *|
|*	old interface.  All duplicate specifics are removed from the new      *|
|*	list of specifics.  What is left on the new specific list is          *|
|*	appended to the list of old specifics.                                *|
|*									      *|
|* Input parameters:							      *|
|*	old_interface_idx -> attr index for interface that already exists     *|
|*	                     in the local name table.                         *|
|*	new_interface_idx -> attr index for interface with the same name as   *|
|*	                     old_interface_idx.  This interface is coming     *|
|*	                     in from the new module.                          *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/
static	void	merge_interfaces(int		new_interface_idx,
				 int		old_interface_idx)

{
#ifdef KEY /* Bug 10177 */
   int		end_sn_idx = 0;
#else /* KEY Bug 10177 */
   int		end_sn_idx;
#endif /* KEY Bug 10177 */
   boolean	found_intrin	= FALSE;
   int		last_old_sn_idx;
   boolean	move_intrin	= FALSE;
   int		new_attr_idx;
   int		new_module_idx;
   int		new_sn_idx;
   int		num_interfaces;
   int		old_attr_idx;
   int		old_module_idx;
   int		old_sn_idx;
   int		prev_sn_idx;
   boolean	same_module;
   int		sn_idx;


   TRACE (Func_Entry, "merge_interfaces", NULL);

   new_sn_idx	= ATI_FIRST_SPECIFIC_IDX(new_interface_idx);

   while (new_sn_idx != NULL_IDX) {
      new_attr_idx	= SN_ATTR_IDX(new_sn_idx);

      /* This call to resolve attr is trying to match new procedures with */
      /* old procedures.  We are not to the stage of setting the keep me  */
      /* flag yet.  That will be done later.                              */

      if (!ML_AT_SEARCHED(new_attr_idx)) {
         resolve_attr(new_attr_idx);
      }
      new_sn_idx	= SN_SIBLING_LINK(new_sn_idx);
   }

   new_sn_idx		= ATI_FIRST_SPECIFIC_IDX(new_interface_idx);
   old_sn_idx		= ATI_FIRST_SPECIFIC_IDX(old_interface_idx);
   num_interfaces	= ATI_NUM_SPECIFICS(old_interface_idx) +
                          ATI_NUM_SPECIFICS(new_interface_idx);
   last_old_sn_idx	= old_sn_idx;

   while (old_sn_idx != NULL_IDX) {
      old_attr_idx	= SN_ATTR_IDX(old_sn_idx);
      sn_idx		= new_sn_idx;
      prev_sn_idx	= NULL_IDX;
      old_module_idx	= AT_MODULE_IDX(old_attr_idx);

      if (AT_IS_INTRIN(old_attr_idx)) {
         found_intrin   = TRUE;
      }
      else if (found_intrin) {
         move_intrin    = TRUE;
      }

      while (sn_idx != NULL_IDX) {
         new_attr_idx = SN_ATTR_IDX(sn_idx);

         if (AT_IS_INTRIN(new_attr_idx)) {
            found_intrin        = TRUE;
         }
         else if (found_intrin) {
            move_intrin = TRUE;
         }

         if ((old_attr_idx == ML_AT_IDX(new_attr_idx)) ||
             (AT_IS_INTRIN(old_attr_idx) && AT_IS_INTRIN(new_attr_idx))) {
              
            /* If intrinsic keep all the old intrinsics, otherwise  */
            /* this is the same attr as found in resolve_attr.      */

            num_interfaces--;
            ML_AT_IDX(new_attr_idx)	= old_attr_idx;
            ML_AT_KEEP_ME(new_attr_idx)	= FALSE;
            sn_idx			= SN_SIBLING_LINK(sn_idx);

            /* new_sn_idx is the start of the list of new specifics to keep. */
            /* prev_sn_idx is the last new specific to keep in the list.     */
            /* Skip this one by setting sn_idx to the next SN_SIBLING_LINK.  */

            if (prev_sn_idx == NULL_IDX) { 
               new_sn_idx			= sn_idx;
            }
            else { 
               SN_SIBLING_LINK(prev_sn_idx)	= sn_idx;
            }
         }
         else if (old_module_idx != NULL_IDX) {
            new_module_idx = AT_MODULE_IDX(new_attr_idx);

            same_module = (ATP_MODULE_STR_IDX(new_module_idx) == 
                           ATP_MODULE_STR_IDX(old_module_idx)) ||

                           (AT_ATTR_LINK(new_module_idx) != NULL_IDX &&
                            ATP_MODULE_STR_IDX(AT_ATTR_LINK(new_module_idx)) ==
                            ATP_MODULE_STR_IDX(old_module_idx));

            if (same_module &&
               (compare_names(AT_ORIG_NAME_LONG(old_attr_idx),
                              AT_ORIG_NAME_LEN(old_attr_idx),
                              AT_ORIG_NAME_LONG(new_attr_idx),
                              AT_ORIG_NAME_LEN(new_attr_idx)) == 0)) {

               /* These are the same specific interfaces.  Throw out the new */
               /* specific.  Set merge links so that all references to the   */
               /* new specific will point to the old specific.               */

               num_interfaces--;
               ML_AT_IDX(new_attr_idx)		= old_attr_idx;
               ML_AT_KEEP_ME(new_attr_idx)	= FALSE;
               sn_idx				= SN_SIBLING_LINK(sn_idx);

               /* new_sn_idx is the start of the list of new specifics  */
               /* to keep.   prev_sn_idx is the last new specific to    */
               /* keep in the list.  Skip this one by setting sn_idx    */
               /* to the next SN_SIBLING_LINK.                          */

               if (prev_sn_idx == NULL_IDX) { 
                  new_sn_idx			= sn_idx;
               }
               else { 
                  SN_SIBLING_LINK(prev_sn_idx)	= sn_idx;
               }
            }
            else {
               prev_sn_idx	= sn_idx;
               sn_idx		= SN_SIBLING_LINK(sn_idx);
            }
         }
         else {
            prev_sn_idx	= sn_idx;
            sn_idx	= SN_SIBLING_LINK(sn_idx);
         }
      }

      last_old_sn_idx	= old_sn_idx;
      old_sn_idx	= SN_SIBLING_LINK(old_sn_idx);
   }

   /* Attach the new interface procedures at the end */
   /* of the list of the old interface procedures.   */

   if (new_sn_idx != NULL_IDX) {
      SN_SIBLING_LINK(last_old_sn_idx)		= new_sn_idx;
      ATI_NUM_SPECIFICS(old_interface_idx)	= num_interfaces;
   }

   /* Need to replace references to new interface with old interface. */
   /* Clear ATI_FIRST_SPECIFIC_IDX because the list is destroyed as   */
   /* the new specific list was merged into the old specific list.    */

   ATI_FIRST_SPECIFIC_IDX(new_interface_idx)	= NULL_IDX;
   ATI_NUM_SPECIFICS(new_interface_idx)		= 0;
   ML_AT_IDX(new_interface_idx)			= old_interface_idx;
   ML_AT_KEEP_ME(new_interface_idx)		= FALSE;

   if (move_intrin) {

      /* Need to move all intrinsics to the end of the interface. */

      new_sn_idx	= NULL_IDX;  /* The list of intrinsics */
      sn_idx		= NULL_IDX;  /* The end of the intrinsics */
      old_sn_idx	= ATI_FIRST_SPECIFIC_IDX(old_interface_idx);
      prev_sn_idx	= NULL_IDX;

      while (old_sn_idx != NULL_IDX) {

         if (AT_IS_INTRIN(SN_ATTR_IDX(old_sn_idx))) {

            if (new_sn_idx == NULL_IDX) {  /* Hook up to new list */
               new_sn_idx			= old_sn_idx;    /* head */
            }
            else {
               SN_SIBLING_LINK(end_sn_idx)	= old_sn_idx;
            }
            end_sn_idx				= old_sn_idx;    /* tail */

            if (prev_sn_idx == NULL_IDX) {
               ATI_FIRST_SPECIFIC_IDX(old_interface_idx) = 
                                          SN_SIBLING_LINK(old_sn_idx);
            }
            else {
               SN_SIBLING_LINK(prev_sn_idx)	= SN_SIBLING_LINK(old_sn_idx);
            }
            sn_idx				= SN_SIBLING_LINK(old_sn_idx);
            SN_SIBLING_LINK(old_sn_idx)		= NULL_IDX;
            old_sn_idx				= sn_idx;
         }
         else {
            prev_sn_idx = old_sn_idx;
            old_sn_idx	= SN_SIBLING_LINK(old_sn_idx);
         }
      }

      if (new_sn_idx != NULL_IDX) {
         SN_SIBLING_LINK(prev_sn_idx)	= new_sn_idx;
      }
   }

   TRACE (Func_Exit, "merge_interfaces", NULL);

   return;

}  /* merge_interfaces */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	 Combine list of new modules that came in with this module to the     *|
|*	 list of modules that came in, in previous USE statements. 	      *|
|*	 Issue any cautions about multiple uses of modules either	      *|
|*	 indirectly or a directly/indirectly combination.  Also, issue	      *|
|*	 CIF records for all indirectly used modules. 	                      *|
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
static	void	resolve_used_modules(int	module_attr_idx)
{
   int		 attr_idx;
   int		 list_idx;
   FILE		*m_ptr;
   int		 next_idx;
   int		 prev_idx;


   TRACE (Func_Entry, "resolve_used_modules", NULL);

   list_idx		= list_of_modules_in_module;

   while (list_idx != NULL_IDX) {
      attr_idx	= AL_ATTR_IDX(list_idx);

      if (ATP_SCP_ALIVE(attr_idx)) {  /* Current module */
         next_idx			= AL_NEXT_IDX(list_idx);

         /* All occurences of attr_idx become AT_ATTR_LINK(attr_idx). */

# ifdef _DEBUG
         if (AT_ATTR_LINK(attr_idx) == NULL_IDX) {
            PRINTMSG(1, 626, Internal, 0,
                     "nonzero AT_ATTR_LINK(attr_idx)",
                     "resolve_used_modules");
         }
# endif
         ML_AT_IDX(attr_idx)		= AT_ATTR_LINK(attr_idx);
         ML_AT_KEEP_ME(attr_idx)	= FALSE;
         prev_idx			= AL_PREV_MODULE_IDX(list_idx);

         if (prev_idx == NULL_IDX) {
            list_of_modules_in_module	= next_idx;
         }
         else {
            AL_NEXT_IDX(prev_idx)	= next_idx;
         }

         if (next_idx != NULL_IDX) {
            AL_PREV_MODULE_IDX(next_idx)= prev_idx;
         }

         AL_NEXT_IDX(list_idx)		= NULL_IDX;
         free_attr_list(list_idx);
         AT_ATTR_LINK(attr_idx)		= NULL_IDX;
         AT_IGNORE_ATTR_LINK(attr_idx)	= FALSE;
      }
      else if (AT_REFERENCED(attr_idx) == Not_Referenced ||
               (!ML_AT_KEEP_ME(attr_idx) && AT_ATTR_LINK(attr_idx)==NULL_IDX)) {

         /* This module came in multiple times and has been resolved */
         /* to the same module.  Remove this one from the list and   */
         /* only keep the resolved to module on the list.            */

         next_idx			= AL_NEXT_IDX(list_idx);
         prev_idx			= AL_PREV_MODULE_IDX(list_idx);

         /* Remove this attr from the list. */

         if (prev_idx == NULL_IDX) {
            list_of_modules_in_module	= next_idx;
         }
         else {
            AL_NEXT_IDX(prev_idx)	= next_idx;
         }

         if (next_idx != NULL_IDX) {
            AL_PREV_MODULE_IDX(next_idx)= prev_idx;
         }

         AL_NEXT_IDX(list_idx)		= NULL_IDX;
         free_attr_list(list_idx);
         AT_ATTR_LINK(attr_idx)		= NULL_IDX;
         AT_IGNORE_ATTR_LINK(attr_idx)	= FALSE;
      }

      /* Assumption:  Following are all indirect module references.  */

      else if (AT_ATTR_LINK(attr_idx) != NULL_IDX) {
         next_idx			= AL_NEXT_IDX(list_idx);

         /* All occurences of attr_idx become AT_ATTR_LINK(attr_idx). */

# ifdef _DEBUG
         if (AT_ATTR_LINK(attr_idx) == NULL_IDX) {
            PRINTMSG(1, 626, Internal, 0,
                     "nonzero AT_ATTR_LINK(attr_idx)",
                     "resolve_used_modules #2");
         }
# endif

         ML_AT_IDX(attr_idx)		= AT_ATTR_LINK(attr_idx);
         ML_AT_KEEP_ME(attr_idx)	= FALSE;

         if (!ATP_INDIRECT_MODULE(attr_idx)) {
            ATP_INDIRECT_MODULE(AT_ATTR_LINK(attr_idx))	= FALSE;
         }

         /* Issue a caution, if this module has already been use'd */
         /* into this scope via another indirect reference or via  */
         /* a direct reference.  Remove from list.  Everything has */
         /* been updated to the original attr_idx already.         */

         PRINTMSG(AT_DEF_LINE(module_attr_idx), 878, Caution,
                  AT_DEF_COLUMN(module_attr_idx),
                  AT_OBJ_NAME_PTR(attr_idx));

         prev_idx			= AL_PREV_MODULE_IDX(list_idx);

         if (prev_idx == NULL_IDX) {
            list_of_modules_in_module	= next_idx;
         }
         else {
            AL_NEXT_IDX(prev_idx)	= next_idx;
         }

         if (next_idx != NULL_IDX) {
            AL_PREV_MODULE_IDX(next_idx)= prev_idx;
         }

         AL_NEXT_IDX(list_idx)		= NULL_IDX;
         free_attr_list(list_idx);
         AT_ATTR_LINK(attr_idx)		= NULL_IDX;
         AT_IGNORE_ATTR_LINK(attr_idx)	= FALSE;

         if (cif_flags & BASIC_RECS) {  /* AT_ATTR_LINK must be NULL */

            /* Set so that we know this indirect module reference   */
            /* came thru this USE statement reference.              */

            AT_MODULE_IDX(attr_idx)	= module_attr_idx;

            cif_use_module_rec(attr_idx, NULL_IDX, TRUE);

            if ((cif_flags & XREF_RECS) != 0) {
               cif_usage_rec(attr_idx,
                             AT_Tbl_Idx,
                             AT_DEF_LINE(module_attr_idx),
                             AT_DEF_COLUMN(module_attr_idx),
                             CIF_Symbol_Is_Hidden_Used_Module);
            }
         }
      }
      else {
         next_idx			= AL_NEXT_IDX(list_idx);

         /* Check if the compiled module file still exists.      */

         if (ATP_MOD_PATH_IDX(attr_idx) != NULL_IDX) {
            m_ptr = fopen(ATP_MOD_PATH_NAME_PTR(attr_idx), "rb");

            if (m_ptr == NULL) {
               PRINTMSG(AT_DEF_LINE(module_attr_idx), 1193, Caution,
                        AT_DEF_COLUMN(module_attr_idx),
                        AT_OBJ_NAME_PTR(attr_idx),
                        AT_OBJ_NAME_PTR(module_attr_idx),
                        ATP_MOD_PATH_NAME_PTR(attr_idx));
            }
            else {
               fclose(m_ptr);
            }
         }

         if (cif_flags & BASIC_RECS) {  /* AT_ATTR_LINK must be NULL */

            /* Set so that we know this indirect module reference   */
            /* came thru this USE statement reference.              */

            AT_MODULE_IDX(attr_idx)		= module_attr_idx;
            ATP_INDIRECT_MODULE(attr_idx)	= TRUE;

            cif_use_module_rec(attr_idx, NULL_IDX, TRUE);

            if ((cif_flags & XREF_RECS) != 0) {
               cif_usage_rec(attr_idx,
                             AT_Tbl_Idx,
                             AT_DEF_LINE(module_attr_idx),
                             AT_DEF_COLUMN(module_attr_idx),
                             CIF_Symbol_Is_Hidden_Used_Module);
            }
         }
      }

      list_idx		= next_idx;
   }

   /* Add modules brought in by this module to complete list of modules. */

   list_idx = SCP_USED_MODULE_LIST(curr_scp_idx);

   while (list_idx != NULL_IDX) {

      if (AL_NEXT_IDX(list_idx) == NULL_IDX) {
         AL_NEXT_IDX(list_idx) = list_of_modules_in_module;
         AL_PREV_MODULE_IDX(list_of_modules_in_module) = list_idx;
         break;
      }
      list_idx = AL_NEXT_IDX(list_idx);
   }

   list_of_modules_in_module = NULL_IDX;

   TRACE (Func_Exit, "resolve_used_modules", NULL);

   return;

}  /* resolve_used_modules */

/******************************************************************************\
|*									      *|
|* Description:								      *|
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
static	boolean	resolve_attr(int	attr_idx)

{
   int		il_idx;
   boolean	found_attr	= FALSE;
   int		name_idx;
   int		np_idx;
   int		old_name_idx;
   int		sn_idx;
   int		srch_attr_idx;
   int		srch_il_idx;
   int		srch_sn_idx;


   TRACE (Func_Entry, "resolve_attr", NULL);

   /* Do not resolve attr linked objects.  What  */
   /* they are attr linked to will get resolved. */

   /* Also, we are only looking to resolve use associated variables. */

   if (!AT_MODULE_OBJECT(attr_idx) || AT_ATTR_LINK(attr_idx) != NULL_IDX) {
      goto EXIT;
   }

   switch (AT_OBJ_CLASS(attr_idx)) {
   case Data_Obj:

      switch (ATD_CLASS(attr_idx)) {
      case Function_Result:
      case Dummy_Argument:
      case Struct_Component:
         goto EXIT;

      case Constant:

         if (ATD_ARRAY_IDX(attr_idx) != NULL_IDX ||
             TYP_TYPE(ATD_TYPE_IDX(attr_idx)) == Structure) {

             /* This is a structure or array constructor and has a tmp */
             /* associated with it that has the same name.  Search on  */
             /* the tmp, since the tmp points to this constant attr.   */

            goto EXIT;
         }
         break;

      default:
         break;
      }
      break;

   case Pgm_Unit:

      if (AT_IS_INTRIN(attr_idx) || ATP_PROC(attr_idx) == Dummy_Proc) {
         goto EXIT;
      }
      break;

   case Interface:
   case Label:
      goto EXIT;

   }  /* End switch */

   /* If this attr belongs in the local name table, do not search the local */
   /* name table for it.  If this is use stmt processing, the check is done */
   /* when the new name table entries are merged together.  If this is      */
   /* interface processing, the only entry that has ML_AT_LN_NAME set is    */
   /* interface body and we don't need to search for that, because the LN   */
   /* entry points to the same attr as the input attr entry.                */

   if (!ML_AT_LN_NAME(attr_idx)) {
      srch_attr_idx		= srch_sym_tbl(AT_OBJ_NAME_PTR(attr_idx),
                                               AT_NAME_LEN(attr_idx),
                                               &name_idx);
      if (srch_attr_idx != NULL_IDX) {

         if (srch_attr_idx == attr_idx) {

            /* Exit with FALSE, because we just found this attr. */

            goto EXIT;
         }

         if (AT_MODULE_OBJECT(srch_attr_idx) &&
             ATP_MODULE_STR_IDX(AT_MODULE_IDX(srch_attr_idx)) ==
                                ATP_MODULE_STR_IDX(AT_MODULE_IDX(attr_idx))) {

            /* The attr from the local name table has the same local name and */
            /* is from the same module.  See if the orig name is the same.    */

            if (AT_OBJ_CLASS(srch_attr_idx) == AT_OBJ_CLASS(attr_idx) &&
                (compare_names(AT_ORIG_NAME_LONG(srch_attr_idx),
                               AT_ORIG_NAME_LEN(srch_attr_idx),
                               AT_ORIG_NAME_LONG(attr_idx),
                               AT_ORIG_NAME_LEN(attr_idx)) == 0)) {
                found_attr	= TRUE;
                goto FOUND;
            }
         }
      }
   }

   /* This attr has come through once already.  Attrs may come through      */
   /* multiple times if they are associated with another attr and they      */
   /* have their own entries in the local name table.  The attr may come    */
   /* thru during the local name table search and then it may be referenced */
   /* as part of a bound expression for another attr entry.  This stops     */
   /* them from being searched twice.                                       */

   if (ML_AT_SEARCHED(attr_idx)) {
      goto EXIT;
   }

   srch_attr_idx = NULL_IDX;
   srch_attr_idx = srch_hidden_name_tbl(AT_OBJ_NAME_PTR(attr_idx),
                                        AT_NAME_LEN(attr_idx),
                                        srch_attr_idx,
                                        &np_idx,
                                        &name_idx);

   if (srch_attr_idx != NULL_IDX) {  /* Found the name in hidden name tbl. */

      /* Check to see if we can find the same object.  The hidden name */
      /* table may have the same name entered multiple times.          */

      do {
         srch_attr_idx	= HN_ATTR_IDX(name_idx);

         if (srch_attr_idx == attr_idx) {

            /* Exit with FALSE, because we just found this attr. */

            goto EXIT;
         }

         if (AT_OBJ_CLASS(srch_attr_idx) == AT_OBJ_CLASS(attr_idx) &&
             ATP_MODULE_STR_IDX(AT_MODULE_IDX(srch_attr_idx)) ==
                                ATP_MODULE_STR_IDX(AT_MODULE_IDX(attr_idx)) &&
             (compare_names(AT_ORIG_NAME_LONG(srch_attr_idx),
                            AT_ORIG_NAME_LEN(srch_attr_idx),
                            AT_ORIG_NAME_LONG(attr_idx),
                            AT_ORIG_NAME_LEN(attr_idx)) == 0)) {
            found_attr = TRUE;

            /* This name is going to be in the local name      */
            /* table, so remove it from the hidden name table. */

            if (ML_AT_LN_NAME(attr_idx)) {
               remove_hidden_name_ntry(name_idx);
            }
            goto FOUND;
         }
         old_name_idx = name_idx;
         name_idx++;
      }
      while (HN_NAME_IDX(old_name_idx) == HN_NAME_IDX(name_idx));

      name_idx = old_name_idx;
   }

   /* Not in either table.  Put in hidden name table, if not in ln name tbl. */

   if (!ML_AT_LN_NAME(attr_idx)) {
      ntr_hidden_name_tbl(attr_idx,
                          np_idx,
                          name_idx);
   }

   goto EXIT;

FOUND:

   ML_AT_IDX(attr_idx)		= srch_attr_idx;
   ML_AT_KEEP_ME(attr_idx)	= FALSE;


   switch (AT_OBJ_CLASS(attr_idx)) {
   case Data_Obj:

      if (ATD_CLASS(attr_idx) == Variable && 
          ATD_VARIABLE_TMP_IDX(attr_idx) != NULL_IDX) {

         if (ATD_FLD(attr_idx) == AT_Tbl_Idx) {
            ML_AT_IDX(ATD_VARIABLE_TMP_IDX(attr_idx)) =
                      ATD_VARIABLE_TMP_IDX(srch_attr_idx);
            ML_AT_KEEP_ME(ATD_VARIABLE_TMP_IDX(attr_idx))	= FALSE;
         }
         else if (ATD_FLD(attr_idx) == IL_Tbl_Idx) {
            il_idx	= ATD_VARIABLE_TMP_IDX(attr_idx);
            srch_il_idx = ATD_VARIABLE_TMP_IDX(srch_attr_idx);

            while (il_idx != NULL_IDX) {
               ML_AT_IDX(IL_IDX(il_idx))	  = IL_IDX(srch_il_idx);
               ML_AT_KEEP_ME(IL_IDX(il_idx))	  = FALSE;
               il_idx			= IL_NEXT_LIST_IDX(il_idx);
               srch_il_idx		= IL_NEXT_LIST_IDX(srch_il_idx);
            }
         }
      }

      if (ATD_CLASS(attr_idx) == Constant && ATD_FLD(attr_idx) == AT_Tbl_Idx) {
         ML_AT_IDX(ATD_CONST_IDX(attr_idx))	= ATD_CONST_IDX(srch_attr_idx);
         ML_AT_KEEP_ME(ATD_CONST_IDX(attr_idx))	= FALSE;
      }

      if (TYP_TYPE(ATD_TYPE_IDX(attr_idx)) == Structure) {
         /* KAY, I added this because derived types were not always resolved */
         /*     BHJ */

         resolve_all_components(attr_idx, srch_attr_idx);
      }

      break;

   case Pgm_Unit:

      if (ATP_PGM_UNIT(attr_idx) == Function ||
          ATP_PGM_UNIT(attr_idx) == Subroutine) {
         srch_sn_idx	= ATP_FIRST_IDX(srch_attr_idx);

         for (sn_idx = ATP_FIRST_IDX(attr_idx); 
              sn_idx < (ATP_FIRST_IDX(attr_idx) + ATP_NUM_DARGS(attr_idx));
              sn_idx++) {

            ML_AT_IDX(SN_ATTR_IDX(sn_idx))	= SN_ATTR_IDX(srch_sn_idx);
            ML_AT_KEEP_ME(SN_ATTR_IDX(sn_idx))	= FALSE;
            srch_sn_idx++;
         }

         if (ATP_PGM_UNIT(attr_idx) == Function){
            ML_AT_IDX(ATP_RSLT_IDX(attr_idx))	  = ATP_RSLT_IDX(srch_attr_idx);
            ML_AT_KEEP_ME(ATP_RSLT_IDX(attr_idx)) = FALSE;
         }
      }
      break;

   case Derived_Type:
      sn_idx		= ATT_FIRST_CPNT_IDX(attr_idx);
      srch_sn_idx	= ATT_FIRST_CPNT_IDX(srch_attr_idx);

      while (sn_idx != NULL_IDX && srch_sn_idx != NULL_IDX) {
         ML_AT_IDX(SN_ATTR_IDX(sn_idx))		 = SN_ATTR_IDX(srch_sn_idx);
         ML_AT_KEEP_ME(SN_ATTR_IDX(sn_idx))	 = FALSE;
         sn_idx					 = SN_SIBLING_LINK(sn_idx);
         srch_sn_idx				 = SN_SIBLING_LINK(srch_sn_idx);
      }
      break;

   }  /* End switch */

EXIT:

   ML_AT_SEARCHED(attr_idx)	= TRUE;

   TRACE (Func_Exit, "resolve_attr", NULL);

   return(found_attr);

}  /* resolve_attr */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	<description>							      *|
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

static void resolve_all_components(int		attr_idx,
                                   int		srch_attr_idx)

{
   int		dt_idx;
   int		sn_idx;
   int		srch_dt_idx;
   int		srch_sn_idx;

   TRACE (Func_Entry, "resolve_all_components", NULL);

   dt_idx      = TYP_IDX(ATD_TYPE_IDX(attr_idx));
   srch_dt_idx = TYP_IDX(ATD_TYPE_IDX(srch_attr_idx));

   sn_idx      = ATT_FIRST_CPNT_IDX(dt_idx);
   srch_sn_idx = ATT_FIRST_CPNT_IDX(srch_dt_idx);

   while (sn_idx != NULL_IDX && srch_sn_idx != NULL_IDX) {
      ML_AT_IDX(SN_ATTR_IDX(sn_idx))     = SN_ATTR_IDX(srch_sn_idx);
      ML_AT_KEEP_ME(SN_ATTR_IDX(sn_idx)) = FALSE;
      
      if (TYP_TYPE(ATD_TYPE_IDX(SN_ATTR_IDX(sn_idx))) == Structure) {
         resolve_all_components(SN_ATTR_IDX(sn_idx), SN_ATTR_IDX(srch_sn_idx));
      }
      sn_idx                             = SN_SIBLING_LINK(sn_idx);
      srch_sn_idx                        = SN_SIBLING_LINK(srch_sn_idx);
   }


   TRACE (Func_Exit, "resolve_all_components", NULL);

   return;

}  /* resolve_all_components */

/******************************************************************************\
|*									      *|
|* Description:								      *|
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
void	collapse_interface_blk(int	interface_idx)

{
   int		 al_idx;
   int		 attr_idx;
   int		 bd_idx;
   int		 cn_idx;
   int		 ln_idx;
   int		 name_idx;
   int		 sn_idx;


   TRACE (Func_Entry, "collapse_interface_blk", NULL);

   if (interface_idx == NULL_IDX) {
      return;
   }

   /* This is used to tell parts of module processing to keep IR and SH for */
   /* used module and internal procedures.  Since collapse_interface_blk    */
   /* also uses parts of module processing, this flag needs to be set if    */
   /* we're going to keep IR/SH for inlining.                               */

   keep_module_procs = (opt_flags.inline_lvl > Inline_Lvl_0) ||
                        ATP_MAY_INLINE(SCP_ATTR_IDX(MAIN_SCP_IDX));

   allocate_mod_link_tbl(0);  /* Let routine determine size. */
   only_update_new_tbl_entries	= TRUE;

   ML_AT_IDX(0)         = BLK_AT_IDX(blk_stk_idx);
   ML_BD_IDX(0)         = BLK_BD_IDX(blk_stk_idx);
   ML_NP_IDX(0)         = BLK_NP_IDX(blk_stk_idx);
   ML_SB_IDX(0)         = BLK_SB_IDX(blk_stk_idx);
   ML_SN_IDX(0)         = BLK_SN_IDX(blk_stk_idx);
   ML_TYP_IDX(0)	= BLK_TYP_IDX(blk_stk_idx);

   /* This prevents the SH, LN, IR, IL, and Constant tables, and the constant */
   /* pool from being compressed.    					      */

   ML_LN_IDX(0)		= SCP_LN_LW_IDX(curr_scp_idx);
   ML_IR_IDX(0)		= ir_tbl_idx;
   ML_IL_IDX(0)		= ir_list_tbl_idx;
   ML_SH_IDX(0)		= sh_tbl_idx;
   ML_CN_IDX(0)         = const_tbl_idx;
   ML_CP_IDX(0)         = const_pool_idx;

   /* Because we do not collapse the constant table, there are some obscure */
   /* cases where we keep a constant table, but not its type.  This takes   */
   /* care of that problem.                                                 */

   for (cn_idx = BLK_CN_IDX(blk_stk_idx); cn_idx <= const_tbl_idx; cn_idx++) {
      set_mod_link_tbl_for_typ(CN_TYPE_IDX(cn_idx));
   }

   /* BHJ - Temp - Force calls to resolve_attr */

   for (attr_idx = ML_AT_IDX(0); attr_idx <= attr_tbl_idx; attr_idx++) {
      ML_AT_SEARCH_ME(attr_idx) = TRUE;
   }

   /* There are a few obscure cases, like a label on the end interface that */
   /* gets added after the attr table index is trapped.  Catch any attrs    */
   /* that were added after the attr table index was trapped and make sure  */
   /* they get kept.                                                        */

   for (ln_idx = SCP_LN_FW_IDX(curr_scp_idx) + 1;
        ln_idx < SCP_LN_LW_IDX(curr_scp_idx); ln_idx++) {

      if (LN_ATTR_IDX(ln_idx) > ML_AT_IDX(0)) { 
         KEEP_ATTR(LN_ATTR_IDX(ln_idx));
      }
   }

   /* The interface body attr may be in the main table before we marked   */
   /* the start of the attr table for the interface body.  Set ML_AT_IDX  */
   /* just in case.  If it is in the compressed part it will get a new    */
   /* index.  Mark SN_KEEP_ME for the interface secondary entry, because  */
   /* it is added after the secondary name table start is marked for the  */
   /* interface body.  If we don't do this, the SN entry will get         */
   /* compressed out, because nothing in the interface body references it.*/

   sn_idx = ATI_FIRST_SPECIFIC_IDX(interface_idx);

   while (sn_idx != NULL_IDX) {
      ML_SN_KEEP_ME(sn_idx)	= TRUE;
      attr_idx			= SN_ATTR_IDX(sn_idx);
      ML_AT_IDX(attr_idx)	= attr_idx;

      /* This name is in the local name table, so should not be hidden.   */

      ML_AT_LN_NAME(attr_idx)	= TRUE;

      /* The only non-hidden thing we need from this interface body is    */
      /* the name of the interface body.  set_mod_link_tbl_for_attr will  */
      /* call itself for all related attrs to this interface body attr.   */

      KEEP_ATTR(attr_idx);

      sn_idx			= SN_SIBLING_LINK(sn_idx);
   }

   /* ATI_FIRST_SPECIFIC_IDX is set on the intrinsic entry if it has been */
   /* expanded.  These cannot be collapsed so we keep expanded_intrinsic_ */
   /* list so that we can make sure these entries get kept.               */

   al_idx	= expanded_intrinsic_list;

   while (al_idx != NULL_IDX) {
      KEEP_ATTR(AL_ATTR_IDX(al_idx));
      al_idx	= AL_NEXT_IDX(al_idx);
   }

   /* Keep everything on the bounds table free list.  It's easier to keep */
   /* it, than to attempt to collapse it out, because we do not know if   */
   /* the free entries are in the area being collapsed or in the area     */
   /* being left alone.                                                   */

   bd_idx       = BD_NEXT_FREE_NTRY(BD_FREE_LIST_IDX);

   while (bd_idx != NULL_IDX) {
      ML_BD_KEEP_ME(bd_idx)	= TRUE;
      bd_idx			= BD_NEXT_FREE_NTRY(bd_idx);
   }

   save_const_pool_idx		= NULL_IDX;
   save_const_tbl_idx		= NULL_IDX;
   num_module_derived_types	= 0;
   count_derived_types		= TRUE;

   /* Resolve duplicate attrs. */

   assign_new_idxs(TRUE);

   /* Do table compression, but do not update the attribute entries in the    */
   /* attr_list_tbl.  Stop updating from happening, by passing the last       */
   /* used index in attr_list_tbl.  compress_tbls goes through the attr       */
   /* list table starting at the entry past the entry passed in.              */

   compress_tbls(NULL_IDX,	/* NULL means don't touch the attr_list_tbl.  */
                 TRUE);         /* Collapsing an interface block.             */

   num_of_derived_types        += num_module_derived_types;
   num_module_derived_types     = 0;

   /* We are looking for items in this list that are out of the collapse area */
   /* but might point into the collapsed area.  Update if they are found.     */
   /* Pick up sn_idx first so we can check if this sn entry needs updating.   */

   if (interface_idx <= BLK_AT_IDX(blk_stk_idx)) {
      update_idxs_in_attr_entry(interface_idx, interface_idx);
   }

   sn_idx	= ATI_FIRST_SPECIFIC_IDX(interface_idx);

   while (sn_idx != NULL_IDX) {

      if (sn_idx <= BLK_SN_IDX(blk_stk_idx)) {
         SN_SIBLING_LINK(sn_idx)	= ML_SN_IDX(SN_SIBLING_LINK(sn_idx));
         SN_ATTR_IDX(sn_idx)		= ML_AT_IDX(SN_ATTR_IDX(sn_idx));
      }

      if (SN_ATTR_IDX(sn_idx) <= BLK_AT_IDX(blk_stk_idx)) {
         update_idxs_in_attr_entry(SN_ATTR_IDX(sn_idx), SN_ATTR_IDX(sn_idx));
      }
      sn_idx = SN_SIBLING_LINK(sn_idx);
   }

   BD_NEXT_FREE_NTRY(BD_FREE_LIST_IDX) = 
                             ML_BD_IDX(BD_NEXT_FREE_NTRY(BD_FREE_LIST_IDX));

   /* attr_idx is the interface body.  Find its compressed index and      */
   /* replace the local name table entry with the compressed indexes.     */

   for (name_idx = SCP_LN_FW_IDX(curr_scp_idx) + 1;
        name_idx < SCP_LN_LW_IDX(curr_scp_idx); name_idx++) {

      /* Update local name table entries pointing to the interface bodies. */
      /* Since we didn't touch the LN table, these need to be updated.     */

      LN_ATTR_IDX(name_idx)	= ML_AT_IDX(LN_ATTR_IDX(name_idx));
      LN_NAME_IDX(name_idx)	= ML_NP_IDX(LN_NAME_IDX(name_idx));

# ifdef _DEBUG

      if (LN_ATTR_IDX(name_idx) == NULL_IDX) {
         PRINTMSG(AT_DEF_LINE(SCP_ATTR_IDX(curr_scp_idx)), 1421, Internal,
                  AT_DEF_COLUMN(SCP_ATTR_IDX(curr_scp_idx)), name_idx);
      }
# endif
     
   }

   al_idx	= expanded_intrinsic_list;

   while (al_idx != NULL_IDX) {
      sn_idx	= ATI_FIRST_SPECIFIC_IDX(AL_ATTR_IDX(al_idx));

      ATI_FIRST_SPECIFIC_IDX(AL_ATTR_IDX(al_idx)) = 
                         ML_SN_IDX(ATI_FIRST_SPECIFIC_IDX(AL_ATTR_IDX(al_idx)));
      ATI_PROC_IDX(interface_idx) = ML_AT_IDX(ATI_PROC_IDX(interface_idx));
      ATD_TYPE_IDX(interface_idx) = ML_TYP_IDX(ATD_TYPE_IDX(interface_idx));


      al_idx	= AL_NEXT_IDX(al_idx);
   }

   if (!SCP_IS_INTERFACE(curr_scp_idx)) {

      /* This is not a nested interface, so free the table. */

      TBL_FREE(mod_link_tbl);
   }
   else {

      /* Nested interface block.  We need to save the ML(0) entries to */
      /* compress the outer interface body blocks.                     */

      mod_link_tbl_idx = 1;
   }

   only_update_new_tbl_entries	= FALSE;

   TRACE (Func_Exit, "collapse_interface_blk", NULL);

   return;

}  /* collapse_interface_blk */

/******************************************************************************\
|*									      *|
|* Description:								      *|
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
static	void	compress_type_tbl(int	start_type_idx)

{
   int		i;
   int		idx;
   boolean	found;
   int		new_type_idx;
   long	       *null_base;
   int		type_idx;
   long	       *type_tbl_base;


   TRACE (Func_Entry, "compress_type_tbl", NULL);

   new_type_idx	= start_type_idx + 1;

   for (type_idx = new_type_idx; type_idx <= type_tbl_idx; type_idx++) {

      if (!ML_TYP_KEEP_ME(type_idx)) {
         continue;
      }

      if (TYP_TYPE(type_idx) == Character) {

         if (TYP_FLD(type_idx) == CN_Tbl_Idx) {
            TYP_IDX(type_idx) = ML_CN_IDX(TYP_IDX(type_idx));
         }
         else if (TYP_FLD(type_idx) == AT_Tbl_Idx) {
            TYP_IDX(type_idx)		= ML_AT_IDX(TYP_IDX(type_idx));
            TYP_ORIG_LEN_IDX(type_idx)  = ML_AT_IDX(TYP_ORIG_LEN_IDX(type_idx));
         }
      }
      else if (TYP_TYPE(type_idx) == Structure) {
         TYP_IDX(type_idx) = ML_AT_IDX(TYP_IDX(type_idx));
      }

      found	= FALSE;
      null_base	= (long *) &(type_tbl[type_idx]);

      for (idx = 1; idx <= start_type_idx; idx++) {
         found		= TRUE;
         type_tbl_base	= (long *) &(type_tbl[idx]);

         for (i = 0; i < NUM_TYP_WDS; i++) {

            if (null_base[i] != type_tbl_base[i]) {
                found = FALSE;
            }
         }

         if (found) {
            break;
         }
      }

      if (!found) {
         idx		= new_type_idx++;
         type_tbl[idx]	= type_tbl[type_idx];
      }
      
      ML_TYP_IDX(type_idx)	= idx;
   }

   /* Set the correct end of the type table. */

   type_tbl_idx		= new_type_idx - 1;

   TRACE (Func_Exit, "compress_type_tbl", NULL);

   return;

}  /* compress_type_tbl */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	find_files_in_directory checks each file in a directory for possible  *|
|*	           search candidates for inline and module files.             *|
|*									      *|
|* Input parameters:							      *|
|*	dir_idx    Index to fp table of the directory to expand.              *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/

static	void	find_files_in_directory(int	dir_idx)

{


   		DIR	*dirp;		/* Directory pointer */
   struct	dirent	*direntp;
		int	 fp_idx;
		boolean	 mod_file;
		int	 new_fp_idx;
                boolean	 okay;
   		char	 path[1024];
   struct	stat	 stat_buf;
   		char	*suffix;


   TRACE (Func_Entry, "find_files_in_directory", NULL);

   if (! is_directory(FP_NAME_PTR(dir_idx))) {

      /* Leave class as Unknown_Fp.  This will */
      /* get set in find_prog_unit_tbl.        */

      return;
   }

   dirp			= opendir(FP_NAME_PTR(dir_idx));
   fp_idx		= NULL_IDX;
   FP_CLASS(dir_idx)	= Directory_Fp;

   while ( (direntp = readdir( dirp )) != NULL ) {

      /* Check to see if the file should be added to the list of files to  */
      /* search for modules and inline program units.  The check makes     */
      /* sure it is a regular file.  It throw out directories.             */

      /* Construct the full path name of the file.  Basically this is the  */
      /* directory name '/' file name.  If the directory name does not     */
      /* start with a '/', insert a './' to make it a relative path name.  */

      if (EQUAL_STRS(FP_NAME_PTR(dir_idx), "./")) {
         strcpy(path, FP_NAME_PTR(dir_idx));
      }
      else if (FP_NAME(dir_idx) != '/') {
         strcpy(path, "./");
         strcat(path, FP_NAME_PTR(dir_idx));
         strcat(path, "/");
      }
      else {
         strcpy(path, FP_NAME_PTR(dir_idx));
         strcat(path, "/");
      }

      strcat(path, direntp->d_name);

      stat(path, &stat_buf);

      if ((stat_buf.st_mode & S_IFMT) == S_IFREG) {

         /* This returns a pointer to the last occurence */
         /* of a dot in the path.                        */

         suffix = strrchr (direntp->d_name, DOT);

         /* The suffix can be .a, .inl, .o, .M  or .mod depending on the host */
         /* machine.  For example, .a is not recognized on IRIX machines.     */

         if (suffix != NULL) {
            okay	= FALSE;
            mod_file	= FALSE;

            if (inline_search && EQUAL_STRS(suffix, ".inl")) { 
               okay = TRUE;
            }

# if !(defined(_HOST_OS_IRIX) || defined(_HOST_OS_LINUX) || defined(_HOST_OS_DARWIN))
            else if (EQUAL_STRS(suffix, ".a")) {
               okay = TRUE;
            }
# endif
            else if (on_off_flags.module_to_mod && EQUAL_STRS(suffix, ".mod")) {
               mod_file	= TRUE;
               okay	= TRUE;
            }

# if defined(_MODULE_TO_DOT_o)
            else if ((!on_off_flags.module_to_mod || FP_SYSTEM_FILE(dir_idx)) &&
                      EQUAL_STRS(suffix, ".o")) {
               okay = TRUE;
            }
# elif defined(_MODULE_TO_DOT_M)
            else if (!on_off_flags.module_to_mod && EQUAL_STRS(suffix, ".M")) {
               okay = TRUE;
            }
# endif
            if (okay) {
               new_fp_idx	= ntr_file_in_fp_tbl(dir_idx, path, fp_idx);
               fp_idx		= new_fp_idx;

               if (mod_file) {
                  FP_CLASS(fp_idx)	= Mod_File_Fp;
               }
            }
         }
      }
   }

   FP_SRCH_THE_FILE(dir_idx)	= FALSE;

   (void) closedir(dirp);

   TRACE (Func_Exit, "find_files_in_directory", NULL);

   return;

} /* find_files_in_directory */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	ntr_file_in_fp_tbl adds a file to its directory in the file path tbl  *|
|*									      *|
|* Input parameters:							      *|
|*	dir_idx       fp table index describing directory being searched.     *|
|*	path          path to directory being searched.                       *|
|*	fp_idx        fp table index of prev file found in this dir.          *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	fp_idx        fp table index of file just added to this dir's list.   *|
|*									      *|
\******************************************************************************/
static	int	ntr_file_in_fp_tbl(int		 dir_idx,
				   char		*path,
				   int		fp_idx)
{
   long	 	length;


   TRACE (Func_Entry, "ntr_file_in_fp_tbl", NULL);

   TBL_REALLOC_CK(file_path_tbl, 1);
   CLEAR_TBL_NTRY(file_path_tbl, file_path_tbl_idx);
   FP_NAME_IDX(file_path_tbl_idx)	= str_pool_idx + 1;
   length				= (long) strlen(path);
   FP_SYSTEM_FILE(file_path_tbl_idx)	= FP_SYSTEM_FILE(dir_idx);
   FP_SRCH_THE_FILE(file_path_tbl_idx)	= TRUE;
   FP_CLASS(file_path_tbl_idx)		= Unknown_Fp;
   FP_NAME_LEN(file_path_tbl_idx)	= length;
   FP_FILE_IDX(file_path_tbl_idx)	= dir_idx;

   TBL_REALLOC_CK(str_pool, WORD_LEN(length));

   str_pool[str_pool_idx].name_long	= 0; /* Zero out last word */

   strcpy(FP_NAME_PTR(file_path_tbl_idx), path);

   /* Insert after directory entry.  Must keep ordered */

   if (fp_idx == NULL_IDX) {
      fp_idx				= file_path_tbl_idx;
      FP_NEXT_FILE_IDX(fp_idx)		= FP_NEXT_FILE_IDX(dir_idx);
      FP_NEXT_FILE_IDX(dir_idx)		= fp_idx;
   }
   else {
      FP_NEXT_FILE_IDX(file_path_tbl_idx)	= FP_NEXT_FILE_IDX(fp_idx);
      FP_NEXT_FILE_IDX(fp_idx)			= file_path_tbl_idx;
      fp_idx					= file_path_tbl_idx;
   }

   TRACE (Func_Exit, "ntr_file_in_fp_tbl", NULL);

   return(fp_idx);

} /* ntr_file_in_fp_tbl */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	Is this a directory or a file?                                        *|
|*									      *|
|* Input parameters:							      *|
|*	Path name to check						      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	TRUE if directory, else FALSE					      *|
|*									      *|
\******************************************************************************/
boolean	is_directory (char *path)
{
   struct	stat	statbuf;


   TRACE (Func_Entry, "is_directory", NULL);

   if (stat (path, &statbuf)) {
      return(FALSE);                   /* stat(2) failed */
   }

   if ((statbuf.st_mode & S_IFDIR) == S_IFDIR) {
       return (TRUE);
   }

   TRACE (Func_Exit, "is_directory", NULL);

   return (FALSE);

} /* is_directory */


/******************************************************************************\
|*									      *|
|* Description:								      *|
|*									      *|
|* Input parameters:							      *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*									      *|
\******************************************************************************/
extern	void	clean_up_module_files(void)

{
# if defined(_MODULE_TO_DOT_o) 

   int		 fp_idx;


   TRACE (Func_Entry, "clean_up_module_files", NULL);

   /* KAY - This is not needed when we're running a standalone frontend. */

   if (!on_off_flags.module_to_mod) {

      for (fp_idx = 1; fp_idx <= file_path_tbl_idx; fp_idx++) {

          if (FP_TMP_FILE(fp_idx)) {
             remove(FP_NAME_PTR(fp_idx));
         }
      }
   }


   TRACE (Func_Exit, "clean_up_module_files", NULL);
# endif

   return;

} /* clean_up_module_files */

/******************************************************************************\
|*									      *|
|* Description:								      *|
|*	This routine takes an intrinsic interface coming in from a module and *|
|*	updates it to use the new intrinsic interface for this version.       *|
|*									      *|
|* Input parameters:							      *|
|*	mod_interface_idx -> attr index for interface from module.            *|
|*									      *|
|* Output parameters:							      *|
|*	NONE								      *|
|*									      *|
|* Returns:								      *|
|*	NOTHING								      *|
|*									      *|
\******************************************************************************/
static	void	update_intrinsic(int		mod_interface_idx)

{
   int		idx;
   int		intrin_interface_idx;
   int		intrin_sn_idx;
   long		length;
   int		mod_sn_idx;
   int		name_idx;
   int		num_interfaces;
   int		prev_sn_idx;
   int		save_expanded;
   int		save_first_specific;
   int		save_num_specifics;
   int		scp_idx;


   TRACE (Func_Entry, "update_intrinsic", NULL);

   scp_idx		= curr_scp_idx;
   curr_scp_idx		= INTRINSIC_SCP_IDX;
   intrin_interface_idx	= srch_sym_tbl(AT_OBJ_NAME_PTR(mod_interface_idx),
                                       AT_NAME_LEN(mod_interface_idx),
                                       &name_idx);
   save_expanded	= expanded_intrinsic_list;

   if (intrin_interface_idx == NULL_IDX) {

      /* We will get here if the intrinsic has been renamed. */

      if (AT_ORIG_NAME_IDX(mod_interface_idx) != 
          AT_NAME_IDX(mod_interface_idx)) {
         intrin_interface_idx = srch_sym_tbl(
                                         AT_ORIG_NAME_PTR(mod_interface_idx),
                                         AT_ORIG_NAME_LEN(mod_interface_idx),
                                        &name_idx);
      }

      if (intrin_interface_idx == NULL_IDX) {
          PRINTMSG(stmt_start_line, 626, Internal, 0,
                   "to find intrinsic",
                   "update_intrinsic");
      }

      /* 						*/
      /*  module module_intrinsics			*/
      /*  intrinsic alog 				*/
      /*  end module module_intrinsics			*/

      /*  module module_machine				*/
      /*  use module_intrinsics, draco_alog => alog	*/
      /*  end module module_machine			*/

      /*  subroutine mhderes 				*/
      /*  use module_machine, ONLY: draco_alog		*/
      /*  end subroutine mhderes 			*/
  
   }

   curr_scp_idx		= scp_idx;
   save_first_specific	= ATI_FIRST_SPECIFIC_IDX(intrin_interface_idx);
   save_num_specifics	= ATI_NUM_SPECIFICS(intrin_interface_idx);

   ATI_FIRST_SPECIFIC_IDX(intrin_interface_idx)	= NULL_IDX;
   ATI_NUM_SPECIFICS(intrin_interface_idx)	= 0;

   complete_intrinsic_definition(intrin_interface_idx);

   /* KAY - need to check more tables than this. */

   idx = (attr_tbl_idx > name_pool_idx) ? attr_tbl_idx : name_pool_idx;
   idx = (idx > sec_name_tbl_idx) ? idx : sec_name_tbl_idx;

   if (idx > mod_link_tbl_idx) {
      length = idx - mod_link_tbl_idx;
      idx    = mod_link_tbl_idx + 1;
      TBL_REALLOC_CK(mod_link_tbl, length);

      for (; idx <= mod_link_tbl_idx; idx++) {
         CLEAR_TBL_NTRY(mod_link_tbl, idx);
      }
   }

   /* Since multiple copies of a generic interface share sn entries,   */
   /* it turns out that the ATI_NUM_SPECIFICS may not match the actual */
   /* number of sn entries. So, count them. BHJ                        */

   mod_sn_idx           = ATI_FIRST_SPECIFIC_IDX(mod_interface_idx);

   num_interfaces = 0;

   while (mod_sn_idx != NULL_IDX) {
      num_interfaces++;
      mod_sn_idx = SN_SIBLING_LINK(mod_sn_idx);
   }

   mod_sn_idx           = ATI_FIRST_SPECIFIC_IDX(mod_interface_idx);
   intrin_sn_idx        = ATI_FIRST_SPECIFIC_IDX(intrin_interface_idx);
   ATI_NUM_SPECIFICS(mod_interface_idx) = num_interfaces;
   prev_sn_idx          = NULL_IDX;

   /* The assumption is that user definitions can be before or after  */
   /* the intrinsics, but all the intrinsic definitions are together. */
   /* Throw out the intrinsic specifics from the module and replace   */
   /* with the intrinsic specifics from this compiler.                */

   /* We can just reset the attrs in the secondary name table.  In    */
   /* general the number of specifics should be the same.  We'll take */
   /* care of odd cases after this loop.                              */

   while (mod_sn_idx != NULL_IDX && intrin_sn_idx != NULL_IDX) {

      if (AT_IS_INTRIN(SN_ATTR_IDX(mod_sn_idx))) {
         ML_AT_IDX(SN_ATTR_IDX(mod_sn_idx)) = SN_ATTR_IDX(intrin_sn_idx);
         intrin_sn_idx			    = SN_SIBLING_LINK(intrin_sn_idx);
      }

      prev_sn_idx	= mod_sn_idx;
      mod_sn_idx	= SN_SIBLING_LINK(mod_sn_idx);
   }

   while (mod_sn_idx != NULL_IDX) {  /* intrin_sn_idx must be NULL */

      /* These could be extra intrinsics coming from the module or they */
      /* could be user defines.  If they are specific intrinsics, just  */
      /* remove them.  They would be unavailable in this compilation.   */
      /* This could be a potential problem when we're reading old       */
      /* modules in and something has changed.                          */

      if (AT_IS_INTRIN(SN_ATTR_IDX(mod_sn_idx))) {

         /* Extra intrinsic - remove it? */

         if (prev_sn_idx == NULL_IDX) {

            /* mod_sn_idx is set to the first item */

            ATI_FIRST_SPECIFIC_IDX(mod_interface_idx) =
                                                  SN_SIBLING_LINK(mod_sn_idx);
         }
         else {
            SN_SIBLING_LINK(prev_sn_idx) = SN_SIBLING_LINK(mod_sn_idx);
         }

         mod_sn_idx = SN_SIBLING_LINK(mod_sn_idx);
         num_interfaces--;
      }
      else {  /* User defined specific - skip it */
         prev_sn_idx	= mod_sn_idx;
         mod_sn_idx	= SN_SIBLING_LINK(mod_sn_idx);
      }
   }

   if (intrin_sn_idx != NULL_IDX) {  /* mod_sn_idx is NULL */

      /* Have more intrinsics than old ones.  Add to end of list */

      if (prev_sn_idx == NULL_IDX) {  /* This shouldn't happen */
         ATI_FIRST_SPECIFIC_IDX(mod_interface_idx) = intrin_sn_idx;
      }
      else {
         SN_SIBLING_LINK(prev_sn_idx)	= intrin_sn_idx;
      }

      while (intrin_sn_idx != NULL_IDX) {
         num_interfaces++;
         intrin_sn_idx	= SN_SIBLING_LINK(intrin_sn_idx);
      }
   }


   ATI_FIRST_SPECIFIC_IDX(intrin_interface_idx)	= save_first_specific;
   ATI_NUM_SPECIFICS(intrin_interface_idx)	= save_num_specifics;

   ATI_NUM_SPECIFICS(mod_interface_idx)		= num_interfaces;
   ATI_INTRIN_TBL_IDX(mod_interface_idx)	= 
                      ATI_INTRIN_TBL_IDX(intrin_interface_idx);

   expanded_intrinsic_list			= save_expanded;

   TRACE (Func_Exit, "update_intrinsic", NULL);

   return;

}  /* update_intrinsic */

/******************************************************************************\
|*									      *|
|* Description:								      *|
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
static	void	process_procs_for_inlining(int	list_idx)

{
   int		al_idx;
   int		attr_idx;
   int		sh_idx;


   TRACE (Func_Entry, "process_procs_for_inlining", NULL);

   if (list_idx == NULL_IDX || num_prog_unit_errors != 0) {
      return;
   }

   al_idx		= list_idx;

   while (al_idx != NULL_IDX) {
      attr_idx	= AL_ATTR_IDX(al_idx);
      sh_idx	= ATP_FIRST_SH_IDX(attr_idx);

      while (sh_idx != NULL_IDX) {

         /* This is only used to set ML_AT_SEARCHED               */

         check_ir_for_attrs(SH_IR_IDX(sh_idx));
         sh_idx	= SH_NEXT_IDX(sh_idx);
      }
      al_idx			= AL_NEXT_IDX(al_idx);
   }

   TRACE (Func_Exit, "process_procs_for_inlining", NULL);

   return;

}  /* process_procs_for_inlining */

/******************************************************************************\
|*									      *|
|* Description:								      *|
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
static	void  check_ir_for_attrs(int	ir_idx)

{
   boolean	first;
   int		fld;
   int		idx;


   TRACE (Func_Entry, "check_ir_for_attrs", NULL);

   first	= TRUE;
   idx		= IR_IDX_L(ir_idx);
   fld		= IR_FLD_L(ir_idx);

AGAIN:

   switch (fld) {
   case CN_Tbl_Idx:
   case NO_Tbl_Idx:
   case SH_Tbl_Idx:
      break;

   case AT_Tbl_Idx:

      ML_AT_SEARCHED(idx)	= TRUE;
      break;

   case IR_Tbl_Idx:
      check_ir_for_attrs(idx);
      break;

   case IL_Tbl_Idx:
      check_il_for_attrs(idx);
      break;
   }

   if (first) {
      first	= FALSE;
      idx	= IR_IDX_R(ir_idx);
      fld	= IR_FLD_R(ir_idx);
      goto AGAIN;
   }
   
   TRACE (Func_Exit, "check_ir_for_attrs", NULL);

   return;

}   /* check_ir_for_attrs */

/******************************************************************************\
|*									      *|
|* Description:								      *|
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
static	void  check_il_for_attrs(int	list_idx)

{

   TRACE (Func_Entry, "check_il_for_attrs", NULL);

   while (list_idx != NULL_IDX) {

      switch (IL_FLD(list_idx)) {
      case CN_Tbl_Idx:
      case NO_Tbl_Idx:
      case SH_Tbl_Idx:
         break;

      case AT_Tbl_Idx:

         ML_AT_SEARCHED(IL_IDX(list_idx)) = TRUE;
         break;

      case IR_Tbl_Idx:
         check_ir_for_attrs(IL_IDX(list_idx));
         break;

      case IL_Tbl_Idx:
         check_il_for_attrs(IL_IDX(list_idx));
         break;
      }
      list_idx = IL_NEXT_LIST_IDX(list_idx);
   }
   
   TRACE (Func_Exit, "check_il_for_attrs", NULL);

   return;

}   /* check_il_for_attrs */

/******************************************************************************\
|*									      *|
|* Description:								      *|
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
# if defined(_DEBUG)

static	void	dump_pdt(FILE	*mod_file_ptr)
{

# if defined(_HOST_OS_UNICOS) || defined(_HOST_OS_MAX)
# include <relo.h>

   typedef	struct	pdttabl	pdt_table_type;

   struct	p_table {pdt_table_type pdt_static_table;
			 Uint		unused	: 32;
			 Uint		unused1	: 8;
			 Uint		pdtmul	: 8;
			 Uint		pdtcmtl	: 8;
			 Uint		pdtmtl	: 8;
			};

   typedef	struct	p_table	p_table_type;
   

   int			file_wd_length;
   char			mod_file_name[256];
   long			offset;
   int			name_wd_length;
   char		       *out;
   p_table_type		pdt_table;
   long			use_file_length;
   long			use_name_length;
   long			use_file_wd_length;
   long			use_name_wd_length;



   TRACE (Func_Entry, "dump_pdt", NULL);

   /* Dump each table as we look through it. */

   init_debug_file();  /* Check to see if debug output is open. */

   switch (MD_PDT_HDR_TYPE) {
   case PDT_TYPE:
      out = "Program descriptor table";
      break;
   case TXT_TYPE:
      out = "Text table";
      break;
   case REL_TYPE:
      out = "Relocation table";
      break;
   case XRL_TYPE:
      out = "Extended Relocation table";
      break;
   case MTT_TYPE:
      out = "Module Termination table";
      break;
   case LHT_TYPE:
      out = "Library (build) header table";
      break;
   case BLD_TYPE:
      out = "Build directory table";
      break;
   case SYM_TYPE:
      out = "Module symbol table";
      break;
   case CMB_TYPE:
      out = "Common block symbol table";
      break;
   case DMT_TYPE:
      out = "Extended debug map table";
      break;
   case GNT_TYPE:
      out = "Global symbol table";
      break;
   case PAS_TYPE:
      out = "Fortran 90 module table";
      break;
   default:
      out = "unknown";
      break;
   }

   fprintf(debug_file, "%-14s= %-6d  %-14s= %-40s\n",
                       "PDT_TABLE_LEN",  MD_PDT_HDR_LEN,
                       "PDT_TABLE_TYPE", out);

   if (MD_PDT_HDR_TYPE == PAS_TYPE) {
      fprintf(debug_file, "%-24s%-40s\n", " ", MD_NAME_PTR);
   }
      
   if (MD_PDT_HDR_TYPE != PDT_TYPE) {
      return;
   }

   offset		= ftell(mod_file_ptr) - TARGET_BYTES_PER_WORD;

   fseek(mod_file_ptr, offset, SEEK_SET);

   fread(&pdt_table, sizeof(pdt_table), 1, mod_file_ptr);
   name_wd_length = 0;
   file_wd_length = 0;

   if (pdt_table.pdt_static_table.pdtmnl != 0) {
      name_wd_length = (pdt_table.pdt_static_table.pdtmnl+7)/8;
   }
      
   if (pdt_table.pdt_static_table.pdtfnl != 0) {
      file_wd_length = (pdt_table.pdt_static_table.pdtfnl+7)/8;
   }

   if ((pdt_table.pdt_static_table.pdthdsz + name_wd_length + file_wd_length) 
        > 10 && pdt_table.pdtmul > 0) {

      /* Has module use paths. */

      fseek(mod_file_ptr, 
            (pdt_table.pdtcmtl + pdt_table.pdtmtl) * TARGET_BYTES_PER_WORD,
            SEEK_CUR);

      while (pdt_table.pdtmul > 0) {
         fread(&use_name_length, sizeof(long), 1, mod_file_ptr);

         use_file_length    = (0377 & use_name_length);
         use_name_length    = (use_name_length >> 8);
         use_file_wd_length = (use_file_length > 0)?((use_file_length+7)/8):0;
         use_name_wd_length = (use_name_length > 0)?((use_name_length+7)/8):0;

         fread(mod_file_name,
               use_file_wd_length * TARGET_BYTES_PER_WORD,
               1,
               mod_file_ptr);

         fprintf(debug_file, "%-14s= %-6d  %-14s= %-40s\n",
                             "path length ", use_file_length,
                             "module path", mod_file_name);

         fread(mod_file_name,
               use_name_wd_length * TARGET_BYTES_PER_WORD,
               1,
               mod_file_ptr);

         fprintf(debug_file, "%-14s= %-6d  %-14s= %-40s\n",
                             "name length ", use_name_length,
                             "module name", mod_file_name);

         pdt_table.pdtmul = pdt_table.pdtmul - 
                            (use_file_wd_length + use_name_wd_length + 1);
      }
   }

   if (file_wd_length > 0) {
      fseek(mod_file_ptr, 
            offset + 
            ((MD_PDT_HDR_LEN - (name_wd_length + file_wd_length)) *
              TARGET_BYTES_PER_WORD),
            SEEK_SET);
      fread(mod_file_name, 
            file_wd_length * TARGET_BYTES_PER_WORD,
            1, 
            mod_file_ptr);

      fprintf(debug_file, "%-14s= %-6d  %-14s= %-40s\n",
                          "length ", pdt_table.pdt_static_table.pdtfnl,
                          "module file", mod_file_name);
   }

   if (name_wd_length > 0) {
      fseek(mod_file_ptr, 
            offset + ((MD_PDT_HDR_LEN - name_wd_length)*TARGET_BYTES_PER_WORD),
            SEEK_SET);

      fread(mod_file_name, 
            name_wd_length * TARGET_BYTES_PER_WORD,
            1, 
            mod_file_ptr);

      fprintf(debug_file, "%-14s= %-6d  %-14s= %-40s\n",
                          "length ", pdt_table.pdt_static_table.pdtmnl,
                          "module name", mod_file_name);
   }

   fseek(mod_file_ptr, offset + TARGET_BYTES_PER_WORD, SEEK_SET);

# endif

   TRACE (Func_Exit, "dump_pdt", NULL);

   return;

}   /* dump_pdt */

/******************************************************************************\
|*									      *|
|* Description:								      *|
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

static	void	print_mod_tbl()
{
   int		idx;
   char	       *boolean_str[]          = { "F", "T" };


   TRACE (Func_Entry, "print_mod_tbl", NULL);

   init_debug_file();  /* Check to see if debug output is open. */

   fprintf(debug_file, "%-16s= %-7d %-16s= %-7d %-16s= %-8s\n",
           "MD_PDT_HDR_TYPE", MD_PDT_HDR_TYPE,
           "MD_PDT_HDR_LEN", MD_PDT_HDR_LEN,
           "MD_ALTERNATE_ENT", boolean_str[MD_ALTERNATE_ENTRY]);

   fprintf(debug_file, "%-16s= %-7s %-16s= %-7s %-16s= %-8s\n",
           "MD_CF77TYPES", boolean_str[MD_CF77TYPES],
           "MD_DALIGN", boolean_str[MD_DALIGN],
           "MD_DEFAULT_INTEGER_TYPE", boolean_str[MD_DEFAULT_INTEGER_TYPE]);

   fprintf(debug_file, "%-16s= %-7s %-16s= %-7s %-16s= %-8s\n",
           "MD_DEFAULT32", boolean_str[MD_DEFAULT32],
           "MD_FLOAT64", boolean_str[MD_FLOAT64],
           "MD_ENABLE_DOUBLE", boolean_str[MD_ENABLE_DOUBLE_PRECISION]);

   fprintf(debug_file, "%-16s= %-7s %-16s= %-7s %-16s= %-8d\n",
           "MD_HAS_ERRORS", boolean_str[MD_HAS_ERRORS],
           "MD_MODULE", boolean_str[MD_MODULE],
           "MD_NAME_LEN", MD_NAME_LEN);

   fprintf(debug_file, "%-16s= %-s\n",
           "MD_NAME_LONG", MD_NAME_PTR);

   fprintf(debug_file, "%-16s= %-7s %-16s= %-7d %-16s= %-8d\n",
           "MD_POINTER8", boolean_str[MD_POINTER8],
           "MD_TARGET", MD_TARGET,
           "MD_VERSION_NUM", MD_VERSION_NUM);

   for (idx = 1; idx <= Num_Of_Tbls; idx++) {

      if (MD_TBL_TYPE(idx) <= Num_Of_Tbls) {
         fprintf(debug_file, "%-16s= %-33s %-16s= %-8d\n",
                 "MD_TBL_TYPE", tbl_type_str[MD_TBL_TYPE(idx)],
                 "MD_NUM_ENTRIES", MD_NUM_ENTRIES(idx));
      }
      else {
         fprintf(debug_file, "%-16s= %-33s %-16s= %-8d\n",
                 "MD_TBL_TYPE", "ERROR",
                 "MD_NUM_ENTRIES", MD_NUM_ENTRIES(idx));
      }
   }

   TRACE (Func_Exit, "print_mod_tbl", NULL);

   return;

}   /* print_mod_tbl */

# endif
