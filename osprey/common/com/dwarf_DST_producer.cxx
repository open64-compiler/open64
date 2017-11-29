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


#include "dwarf_DST_producer.h"
#include "errors.h"        /* in ../common/util */
#ifdef KEY
#include <ctype.h>         /* for 'isdigit' */
#endif


         /*---------------------------------*
          * Macros used for error reporting *
          *---------------------------------*/

#define DST_ASSERT(truth, msg) Is_True(truth, (msg))


   /*----------------------------------------------------------
    * Current producer state, determines whether or not a
    * DST_block_kind can be begun, whether it is in the process
    * of being built, or whether it is completed.
    *----------------------------------------------------------*/


typedef enum DST_producer_state
{
   DST_begin_state,
   DST_making_include_dirs,
   DST_making_file_names,
   DST_making_macinfo,
   DST_making_dbg_info,
   DST_end_state
} DST_PRODUCER_STATE;

static DST_PRODUCER_STATE pstate = DST_begin_state;
static DST_DIR_IDX        last_include_dir = DST_INVALID_INIT;
static DST_FILE_IDX       last_file_name = DST_INVALID_INIT;
static DST_INFO_IDX       last_info_idx = DST_INVALID_INIT;
static DST_INFO_IDX       file_scope_info = DST_INVALID_INIT;
static DST_INFO_IDX       forced_exit_info = DST_INVALID_INIT;
#ifdef KEY
static DST_MACR_IDX       last_macro = DST_INVALID_INIT;
#endif

static mUINT16            num_file_names = 0;
static mUINT16            num_incl_dirs = 0;
#ifdef KEY
static mUINT16            num_macros = 0;
#endif
  
static BOOL               begin_PU = FALSE;
static mINT32             file_scope_locks = 0;


   /*-------------------------
    * Memory allocation stuff
    *-------------------------*/

#define DST_64_allign  8                 /* 8 bytes alignment */
#define DST_32_align   4                 /* 4 bytes alignment */
#define DST_char_align 1                 /* 1 bytes alignment */
#define DST_default_align DST_64_allign  /* default alignment */


/* Use 64 bits alignment for now, since that is guaranteed to work 
*/
#define DST_mk(type) DST_allocate(sizeof(type), DST_default_align)

#define DST_mk_file() (DST_mk(DST_FILE_NAME));
#define DST_mk_dir() (DST_mk(DST_INCLUDE_DIR));
#define DST_mk_info() (DST_mk(DST_INFO));
#define DST_mk_attr(type) (DST_mk(type));
#ifdef KEY
#define DST_mk_macro() (DST_mk(DST_MACR));
#endif

#if defined(MONGOOSE_BE)
/*	Don't call DST_enter_mk in the backend */
/*  MONGOOSE_BE also implies _LEGO_CLONER  */
#define DST_enter_mk(a, b) 
#endif

DST_STR_IDX
DST_mk_string(const char *s)
{
   DST_STR_IDX str_idx;

   if (s!=NULL)
   {
      str_idx = DST_allocate(strlen(s) + 1, DST_char_align);
      (void)strcpy(DST_STR_IDX_TO_PTR(str_idx), s);
   }
   else
      str_idx = DST_INVALID_IDX;
   
   return str_idx;
} /* DST_mk_string */


static DST_STR_IDX
DST_mk_name(const char *s)
{
   DST_STR_IDX str_idx;

   /* A general string may be an empty string, while a name attribute
    * must contain at least one non-null character or else it is invalid.
    */
#ifndef KEY
   if ((s != NULL) && (*s != '\0'))
#else
     // related to bug 1717 - struct and union types get 
     // anonymous type names like ._0, ._1, etc.
     // Avoid generating these names.
   if ((s != NULL) && (*s != '\0') && 
       !(*s == '.' && s[1] == '_' && isdigit(s[2])))
#endif
   {
      str_idx = DST_allocate(strlen(s) + 1, DST_char_align);
      (void)strcpy(DST_STR_IDX_TO_PTR(str_idx), s);
   }
   else
      str_idx = DST_INVALID_IDX;
   
   return str_idx;
} /* DST_mk_name */


#if (!defined(MONGOOSE_BE))
/* Changes pstate and current block if necessary.  Note that this
 * procedure only should be called when some storage is definitely
 * allocated and one of the global "last_????" variables are updated 
 * accordingly.  "last_idx" may be a DST_INFO_IDX, DST_FILE_INFO, or
 * DST_DIR_INFO.
*/
static void
DST_enter_mk(DST_PRODUCER_STATE new_state, DST_IDX last_idx)
{
   if (file_scope_locks && new_state==DST_making_dbg_info)
   {
      /* The new dbg info is to be entered only at file-scope */
      if (pstate != new_state)
      {
	 DST_return_to_block(last_idx); /* Return to info block */
	 pstate = new_state;
      }
      if (!DST_IS_NULL(file_scope_info) && DST_IS_NULL(forced_exit_info))
      {
	 forced_exit_info = last_info_idx;     /* Save current info idx */
	 DST_return_to_block(file_scope_info); /* Return to file-scope */
      }
   }
   else if (begin_PU && new_state==DST_making_dbg_info)
   {
      /* User must indicate intent to change to new PU (begin_PU) */
      DST_ASSERT(!DST_IS_NULL(file_scope_info), "Premature entry into PU");
      DST_begin_block(DST_local_scope_block);
      pstate = new_state;
      begin_PU = FALSE;
   }
   else if (pstate != new_state)
   {
      DST_return_to_block(last_idx); /* Return correct type of block */
      pstate = new_state;
   }
}
#endif /* (!define(MONGOOSE_BE)) */

#define DST_check_info_idx(required_tag, idx)\
   DST_ASSERT((DST_IS_FOREIGN_OBJ(idx)) || (DST_INFO_tag(DST_INFO_IDX_TO_PTR(idx)) == required_tag),\
	      "Found invalid DST_info index")

#if defined(_SUPPORT_IPA) || defined(_STANDALONE_INLINER)
    /* these function are not needed with _LEGO_CLONER */

/* set last_file_name so that we can add more files into the files dir */
static void 
DST_set_last_file_name(void)
{
    DST_IDX idx;
    num_file_names = 0;
    for (idx = DST_get_file_names (); !DST_IS_NULL(idx); idx = DST_FILE_NAME_next(DST_FILE_IDX_TO_PTR(idx))) {
        num_file_names++;
        last_file_name = idx;
    }
}

static void 
DST_set_last_include_dir(void)
{
    DST_IDX idx;
    num_incl_dirs = 0;
    for (idx = DST_get_include_dirs (); !DST_IS_NULL(idx); idx = DST_INCLUDE_DIR_next(DST_DIR_IDX_TO_PTR(idx))) {
	num_incl_dirs++;
	last_include_dir = idx;
    }
}

#endif
   

   /*-----------------------------------------------------------
    * Creation of ordered list of directories for include files
    *-----------------------------------------------------------*/

/* The entries will be listed in the order in which they are created.
*/
DST_IDX 
DST_mk_include_dir(char *path)
{
   DST_INCLUDE_DIR  *dir_ptr, *prev_ptr;
   DST_DIR_IDX       dir_idx;

   /* Reset pstate and the memory block used for allocation */
   if (DST_IS_NULL(last_include_dir))
   {
      DST_begin_block(DST_include_dirs_block); /* First dir entry */

#if !(defined(_SUPPORT_IPA) || defined(_STANDALONE_INLINER))
      pstate = DST_making_include_dirs;
#endif
   }
#if !(defined(_SUPPORT_IPA) || defined(_STANDALONE_INLINER))
   else
      DST_enter_mk(DST_making_include_dirs, last_include_dir);
#endif
   
   /* Create the entry */
   dir_idx  = DST_mk_dir();
   dir_ptr  = DST_DIR_IDX_TO_PTR(dir_idx);
   DST_INCLUDE_DIR_path(dir_ptr) = DST_mk_string(path);
   DST_INCLUDE_DIR_next(dir_ptr) = DST_INVALID_IDX;
   if (!DST_IS_NULL(last_include_dir))
   {
      prev_ptr = DST_DIR_IDX_TO_PTR(last_include_dir);
      DST_INCLUDE_DIR_next(prev_ptr) = dir_idx;
   }
   last_include_dir = dir_idx;
   num_incl_dirs += 1;
   return dir_idx;
}


mUINT16
DST_number_of_include_dirs(void)
{
   return num_incl_dirs;
}

   /*----------------------------------------
    * Creation of ordered list of file_names
    *----------------------------------------*/


/* The entries will be listed in the order in which they are created.
 * The incl_dir is the ordinal position of the dir in the include_dirs
 * list, the size is in bytes (zero if abscent), the modt is the last
 * modification time (e.g. a time_t from <sys/time.h>).
 * Returns the ordinal position of the file-name in the list
 * of file-names.
*/
DST_IDX
DST_mk_file_name(char *file_name, 
		 mUINT16 incl_dir,
		 UINT64  size,
		 UINT64  modt)
{
   DST_FILE_NAME *f_ptr, *prev_ptr;
   DST_FILE_IDX   f_idx;

   /* Reset pstate and the memory block used for allocation */
   if (DST_IS_NULL(last_file_name))
   {
      DST_begin_block(DST_file_names_block); /* First file entry */
      pstate = DST_making_file_names;
   }
#if !(defined(_SUPPORT_IPA) || defined(_STANDALONE_INLINER))
   else
      DST_enter_mk(DST_making_file_names, last_file_name);
#endif
   
   /* Create the entry */
   f_idx  = DST_mk_file();
   f_ptr  = DST_FILE_IDX_TO_PTR(f_idx);
   DST_FILE_NAME_name(f_ptr) = DST_mk_string(file_name);
   DST_FILE_NAME_dir(f_ptr)  = incl_dir;
   DST_FILE_NAME_size(f_ptr) = size;
   DST_FILE_NAME_modt(f_ptr) = modt;
   DST_FILE_NAME_next(f_ptr) = DST_INVALID_IDX;
   if (!DST_IS_NULL(last_file_name))
   {
      prev_ptr = DST_FILE_IDX_TO_PTR(last_file_name);
      DST_FILE_NAME_next(prev_ptr) = f_idx;
   }
   last_file_name = f_idx;
   num_file_names += 1;
   return f_idx;
}


mUINT16
DST_number_of_files(void)
{
   return num_file_names;
}

#ifdef KEY
mUINT16
DST_number_of_macros(void)
{
   return num_macros;
}
#endif

   /*------------------------------------------------
    * Creation of .debug_macinfo section information
    *------------------------------------------------*/


   /*---------------------------------------------
    * Creation of .debug_info section information
    *---------------------------------------------*/


/* The following two routines will force all new info entries to
 * be allocated in the file_scope memory region.  Be VERY CAREFUL
 * when using this facility; never call DST_begin_PU() or DST_end_PU
 * while a lock is active.  Locks may be nested to arbitrary depth,
 * and realeases must completely unnest such locks before a release
 * has any effect.
 */
void DST_lock_to_file_scope_mem(void)
{
   DST_ASSERT(file_scope_locks >= 0, "Erroneous state for file-scope lock");
   file_scope_locks += 1;
}

void DST_release_from_file_scope_mem(void)
{
   DST_ASSERT(file_scope_locks > 0, "Missing lock for file-scope release");
   file_scope_locks -= 1;
   if (!file_scope_locks && !DST_IS_NULL(forced_exit_info))
   {
      last_info_idx = forced_exit_info;
      forced_exit_info = DST_INVALID_IDX;
      if (pstate == DST_making_dbg_info)
	 DST_return_to_block(last_info_idx);
   }
}

mINT32 DST_unwind_locks(void)
{
   mINT32 number_of_locks = file_scope_locks;

   file_scope_locks = 1;
   DST_release_from_file_scope_mem();
   return number_of_locks;
}

extern void DST_wind_up_locks(mINT32 number_of_locks)
{
   file_scope_locks += number_of_locks;
}

BOOL DST_is_locked_to_file_scope_mem(void)
{
   return file_scope_locks > 0;
}



/* Initiates the fields of a new DST_INFO record.
*/
static DST_INFO_IDX
DST_init_info(DST_INFO_IDX info_idx, 
	      DST_DW_tag    tag, 
	      DST_flag      flag, 
	      DST_ATTR_IDX  attrs)
{
   DST_INFO *info_ptr;

   info_ptr = DST_INFO_IDX_TO_PTR(info_idx);
   DST_INFO_tag(info_ptr) = tag;
   DST_INFO_flag(info_ptr) = flag;
   DST_INFO_sibling(info_ptr) = DST_INVALID_IDX;
   DST_INFO_attributes(info_ptr) = attrs;
   DST_INFO_dieptr(info_ptr) = NULL;
   last_info_idx = attrs;
   return info_idx;
}


/* Creates a DW_TAG_compile_unit entry and returns its idx.
 * Must be called before making any other info section data
*/
DST_INFO_IDX 
DST_mk_compile_unit(char        *src_path,
		    char        *comp_dir,
		    char        *comp_info,
		    DST_language language,
		    DST_identifier_case id_case)
{
   DST_INFO_IDX      info_idx;
   DST_ATTR_IDX      cu_idx;
   DST_COMPILE_UNIT *cu;
   
   /* See if this is a valid change in state, then change state */
   DST_ASSERT(DST_IS_NULL(last_info_idx), 
	      "Illegal attempt to start DST file-scope twice");
   pstate = DST_making_dbg_info;

   /* Make the info record and its set of attributes */
   DST_begin_block(DST_file_scope_block);
   info_idx = DST_mk_info();
   cu_idx = DST_mk_attr(DST_COMPILE_UNIT);
   cu = DST_ATTR_IDX_TO_PTR(cu_idx, DST_COMPILE_UNIT);
   DST_COMPILE_UNIT_name(cu)     = DST_mk_string(src_path);
   DST_COMPILE_UNIT_comp_dir(cu) = DST_mk_string(comp_dir);
   DST_COMPILE_UNIT_producer(cu) = DST_mk_string(comp_info);
   DST_COMPILE_UNIT_language(cu) = language;
   DST_COMPILE_UNIT_identifier_case(cu) = id_case;
   DST_COMPILE_UNIT_first_child(cu) = DST_INVALID_IDX;
   DST_COMPILE_UNIT_last_child(cu) = DST_INVALID_IDX;

   return DST_init_info(info_idx, DW_TAG_compile_unit, DST_no_flag, cu_idx);
}


/* Must be called to allow memory allocation on a program unit (PU)
 * basis in the back-end.  Will start a new memory region with the 
 * subsequent _mk_ , upon which an index for the last_info_idx is saved
 * in "file_scope_info".  No nesting of PUs are allowed.  When all entries 
 * for the PU have been made, return by calling "DST_end_PU()".
*/
void
DST_begin_PU(void)
{
   DST_ASSERT(!file_scope_locks && DST_IS_NULL(file_scope_info) && !begin_PU, 
	      "Attempt to nest PU");
   begin_PU = TRUE;
   file_scope_info = last_info_idx; /* Would need a stack for nested PUs */
}


/* Must be called after all symbols local to a PU have been created
 * and before any more file-scope symbols are created.
*/
void
DST_end_PU(void)
{
   DST_ASSERT(!file_scope_locks && !DST_IS_NULL(file_scope_info) || begin_PU,
	      "Premature attempt to end DST PU");

   last_info_idx = file_scope_info;
   file_scope_info = DST_INVALID_IDX;
   if (begin_PU)
      begin_PU = FALSE;
   else
   {
      pstate = DST_making_dbg_info;
      DST_return_to_block(last_info_idx);
   }
}


/* Creates a DW_TAG_inlined_subroutine entry and returns its idx.
*/
DST_INFO_IDX 
DST_mk_inlined_subroutine(ST_IDX	low_pc,    /* ptr to front-end label */
			  ST_IDX	high_pc,   /* ptr to front-end label */
			  DST_INFO_IDX  abstract_origin,
			  DST_TYPE      abstract_dst)
{
   DST_INFO_IDX            info_idx;
   DST_ATTR_IDX            attr_idx;
   DST_flag                flag = DST_no_flag;
   DST_INLINED_SUBROUTINE *attr;
   DST_TYPE old_dst = Current_DST;

#if !(defined(_SUPPORT_IPA) || defined(_STANDALONE_INLINER))
   DST_enter_mk(DST_making_dbg_info, last_info_idx);
#endif
   Current_DST = abstract_dst;
   DST_check_info_idx(DW_TAG_subprogram, abstract_origin);
   Current_DST = old_dst;
   
   info_idx = DST_mk_info();
   attr_idx = DST_mk_attr(DST_INLINED_SUBROUTINE);
   attr = DST_ATTR_IDX_TO_PTR(attr_idx, DST_INLINED_SUBROUTINE);

   DST_ASSOC_INFO_st_idx(DST_INLINED_SUBROUTINE_low_pc (attr)) = low_pc;
   DST_ASSOC_INFO_st_idx(DST_INLINED_SUBROUTINE_high_pc(attr)) = high_pc;
   DST_INLINED_SUBROUTINE_abstract_origin(attr) = abstract_origin;
   DST_INLINED_SUBROUTINE_abstract_dst(attr) = abstract_dst;
   DST_INLINED_SUBROUTINE_first_child(attr) = DST_INVALID_IDX;
   DST_INLINED_SUBROUTINE_last_child(attr) = DST_INVALID_IDX;

   return DST_init_info(info_idx, DW_TAG_inlined_subroutine, 
		       flag, attr_idx);
}


#if defined(_SUPPORT_IPA) || defined(_STANDALONE_INLINER)
    /* These are not needed with _LEGO_CLONER */

void
DST_label_add_name(DST_LABEL *attr, char *label_name)
{
   DST_LABEL_name(attr) = DST_mk_name(label_name);
}

void
DST_lexical_block_add_name(DST_LEXICAL_BLOCK *attr, char *block_name)
{
   DST_LEXICAL_BLOCK_name(attr) = DST_mk_name(block_name);
}

/* Get an ordinal number for the given directory name.  Create one if it
 * has not yet been entered, otherwise return an existing number.
 * ordinal number starts at 1 so the first incl dir has ordinal number 1
*/
static  mUINT16
DST_get_ordinal_num(DST_DIR_IDX dir_idx, char *dir_name)
{
  DST_DIR_IDX		idx = dir_idx;
  mUINT16		num = 0;
  DST_INCLUDE_DIR 	*dir;

  if (DST_IS_NULL(idx))
	return 0;

  dir = DST_DIR_IDX_TO_PTR(idx);

  while (dir != NULL) {
	num += 1;
	if (strcmp(DST_STR_IDX_TO_PTR(DST_INCLUDE_DIR_path(dir)), dir_name) == 0)
      return num;
	else {
      idx = DST_INCLUDE_DIR_next(dir);
      if (!DST_IS_NULL(idx))
        dir = DST_DIR_IDX_TO_PTR(idx);
      else
        dir = NULL;
	}
  }

  if (dir == NULL) {            /* can't find it so need to create it */
	(void)DST_mk_include_dir(dir_name);
	return num_incl_dirs;
  }
  return 0;
}

/* get the include directory name of a given ordinal */
char *
DST_get_dirname(mUINT16 ordinal)
{
    DST_DIR_IDX d_idx = DST_get_include_dirs();
    DST_INCLUDE_DIR 	*dir;
    mUINT16 i;

    for (i = 0; i < ordinal; i++) {
        dir = DST_DIR_IDX_TO_PTR(d_idx);
	d_idx = DST_INCLUDE_DIR_next(dir);
    }
    return (DST_STR_IDX_TO_PTR(DST_INCLUDE_DIR_path(dir)));
}

/* get the filename of a given ordinal */
char *
DST_get_file(mUINT16 ordinal, UINT64 *file_size, UINT64 *fmod_time, char **dirname)
{
    DST_FILE_IDX f_idx = DST_get_file_names();
    DST_FILE_NAME 	*file;
    mUINT16 i;

    /* src position is not set correctly */
    if (ordinal == 0)
	return NULL;

    for (i = 0; i < ordinal; i++) {
        file = DST_FILE_IDX_TO_PTR(f_idx);
	f_idx = DST_FILE_NAME_next(file);
    }
    *file_size = DST_FILE_NAME_size(file);
    *fmod_time = DST_FILE_NAME_modt(file);
    *dirname = DST_get_dirname(DST_FILE_NAME_dir(file));
    return (DST_STR_IDX_TO_PTR(DST_FILE_NAME_name(file)));
}

/* try to see if filename is in the current DST, if so, return it's file 
 * index
 */
static mUINT16
DST_get_file_id(char *filename, char *dirname, UINT64 file_size, UINT64 fmod_time)
{
   DST_FILE_IDX   f_idx = DST_get_file_names();
   DST_FILE_NAME *f_ptr;
   mUINT16 	  num = 0;

   /* Create the entry */
   while (!DST_IS_NULL(f_idx)) {
	f_ptr = DST_FILE_IDX_TO_PTR(f_idx);
	num += 1;
	if ((DST_FILE_NAME_size(f_ptr) == file_size) &&
		(DST_FILE_NAME_modt(f_ptr) == fmod_time) &&
		(strcmp(filename, DST_STR_IDX_TO_PTR(DST_FILE_NAME_name(f_ptr))) == 0) &&
		(strcmp(dirname, DST_get_dirname(DST_FILE_NAME_dir(f_ptr))) == 0)) {
	    /* found a match */
	    return num;
   	}
  	f_idx = DST_FILE_NAME_next(f_ptr);
   }
   return 0;
}

/* add the file into the file directory, return the ordinal num of the
   new file
 */
static mUINT16
DST_enter_file(char *filename, char *dirname, UINT64 file_size, UINT64 fmod_time)
{
  DST_mk_file_name(filename, DST_get_ordinal_num(DST_get_include_dirs(), 
                              dirname),
                              file_size,
                              fmod_time);

  return (num_file_names);
}


mUINT16 
DST_get_cross_inlined_file_id
  (char *filename,      /* ptr to filename of the inlined routine */
   char *dirname,       /* ptr to directory path of the inlined routine */
   UINT64 file_size,    /* File size (bytes) */
   UINT64 fmod_time)    /* Last file mod time */
 
{
  mUINT16 file_id = 0;

  /* need to see if filename is already in the current DST */

  DST_set_last_include_dir();
  DST_set_last_file_name();

  file_id = DST_get_file_id(filename, dirname, file_size, fmod_time);
  if (file_id == 0) {
	return(DST_enter_file(filename, dirname, file_size, fmod_time));
  }
  return file_id;
}


DST_INFO_IDX 
DST_mk_cross_inlined_subroutine(
			  ST_IDX	 low_pc,    	    /* ptr to front-end label */
			  ST_IDX	 high_pc,   	    /* ptr to front-end label */
			  char		*name,	    	    /* ptr to parent routine's name */		
			  /* the following are all for new file entry */
			  mUINT16 	*file_index,	    /* returns the file_index of entry created for filename */
			  UINT64	file_size,	    /* File size (bytes) */
			  UINT64	fmod_time,	    /* Last file mod time */
   			  USRCPOS      	inl_decl,  	    /* inline routine's source position */
			  char		*filename,	    /* ptr to filename of the inlined routine */
			  char 		*dirname,	    /* ptr to directory path of the inlined routine */
			  DST_INFO_IDX  abstract_origin,    /* The abstract version of this subroutine */
			  DST_TYPE      abstract_dst)       /* dst where abstract_origin is located */ 
		
{
   DST_INFO_IDX            info_idx;
   DST_ATTR_IDX            attr_idx;
   DST_flag                flag = DST_no_flag;
   DST_INLINED_SUBROUTINE *attr;
   USRCPOS      	   decl;  	/* source position */
   mUINT16 file_id = 0;
   
   USRCPOS_clear(decl);
   USRCPOS_column(decl) = USRCPOS_column(inl_decl);
   USRCPOS_linenum(decl) = USRCPOS_linenum(inl_decl);
   info_idx = DST_mk_info();
   attr_idx = DST_mk_attr(DST_INLINED_SUBROUTINE);
   attr = DST_ATTR_IDX_TO_PTR(attr_idx, DST_INLINED_SUBROUTINE);

   DST_set_last_include_dir();
   DST_set_last_file_name();

   DST_ASSOC_INFO_st_idx(DST_INLINED_SUBROUTINE_low_pc (attr)) = low_pc;
   DST_ASSOC_INFO_st_idx(DST_INLINED_SUBROUTINE_high_pc(attr)) = high_pc;

   DST_INLINED_SUBROUTINE_abstract_name(attr) = DST_mk_name(name);

   /* need to see if filename is already in the current DST */

   if (*file_index == 0) {
       file_id = DST_get_file_id(filename, dirname, file_size, fmod_time);
       if (file_id == 0) {
	    file_id = DST_enter_file(filename, dirname, file_size, fmod_time);
       }
       *file_index = file_id;
   }

   USRCPOS_filenum(decl) = *file_index;

   DST_INLINED_SUBROUTINE_decl(attr) = decl;
   DST_INLINED_SUBROUTINE_abstract_origin(attr) = abstract_origin;;
   DST_INLINED_SUBROUTINE_abstract_dst(attr) = abstract_dst;
   DST_INLINED_SUBROUTINE_first_child(attr) = DST_INVALID_IDX;
   DST_INLINED_SUBROUTINE_last_child(attr) = DST_INVALID_IDX;

   return DST_init_info(info_idx, DW_TAG_inlined_subroutine, 
		       flag, attr_idx);
}

#endif

/* Creates a DW_TAG_subprogram entry and returns its idx.
*/
DST_INFO_IDX 
DST_mk_subprogram_memdef(USRCPOS      decl,  /* Source location */
			 ST_IDX	subpr, 
			 BOOL         is_prototyped,
			 DST_INFO_IDX spec)  /* decl in class */
{
   DST_INFO_IDX    info_idx;
   DST_ATTR_IDX    attr_idx;
   DST_flag        flag = DST_no_flag;
   DST_SUBPROGRAM *attr;

#if !(defined(_SUPPORT_IPA) || defined(_STANDALONE_INLINER))
   DST_enter_mk(DST_making_dbg_info, last_info_idx);
#endif
   DST_check_info_idx(DW_TAG_subprogram, spec);
   
   info_idx = DST_mk_info();
   attr_idx = DST_mk_attr(DST_SUBPROGRAM);
   attr = DST_ATTR_IDX_TO_PTR(attr_idx, DST_SUBPROGRAM);
   DST_SUBPROGRAM_memdef_decl(attr) = decl;
   DST_SUBPROGRAM_memdef_spec(attr) = spec;

   DST_ASSOC_INFO_st_idx(DST_SUBPROGRAM_memdef_st(attr)) = subpr;

   DST_SUBPROGRAM_memdef_first_child(attr) = DST_INVALID_IDX;
   DST_SUBPROGRAM_memdef_last_child(attr) = DST_INVALID_IDX;

   DST_SET_memdef(flag);
   if (is_prototyped)
      DST_SET_prototyped(flag);
   return DST_init_info(info_idx, DW_TAG_subprogram, 
		       DST_flag_memdef, attr_idx);
}


/* Creates a DW_TAG_subprogram entry and returns its idx
 * (for a regular declaration/definition).
*/
DST_INFO_IDX 
DST_mk_subprogram(USRCPOS      decl,
		  char        *name,
		  DST_INFO_IDX type,
		  DST_INFO_IDX origin,
		  ST_IDX       subpr, 
		  DST_inline   inlin,
		  DST_virtuality virtuality,
		  DST_vtable_elem_location vtable_elem_location,
		  BOOL         is_declaration,
		  BOOL         is_prototyped,
#ifdef KEY
                  BOOL         is_artificial,
#endif
		  BOOL         is_external)
{
   DST_INFO_IDX    info_idx;
   DST_ATTR_IDX    attr_idx;
   DST_flag        flag = DST_no_flag;
   DST_SUBPROGRAM *attr;

#if !(defined(_SUPPORT_IPA) || defined(_STANDALONE_INLINER))
   DST_enter_mk(DST_making_dbg_info, last_info_idx);
#endif
   
   info_idx = DST_mk_info();
   attr_idx = DST_mk_attr(DST_SUBPROGRAM);
   attr = DST_ATTR_IDX_TO_PTR(attr_idx, DST_SUBPROGRAM);
   if (is_declaration)
   {
      DST_SUBPROGRAM_decl_decl(attr) = decl;
      DST_SUBPROGRAM_decl_name(attr) = DST_mk_name(name);
      DST_SUBPROGRAM_decl_linkage_name(attr) = DST_INVALID_IDX;
      DST_SUBPROGRAM_decl_type(attr) = type;
      DST_SUBPROGRAM_decl_origin(attr) = origin;
      DST_SUBPROGRAM_decl_inline(attr) = inlin;
#ifdef CPLUSPLUSIL
      DST_SUBPROGRAM_decl_virtuality(attr) = virtuality;
      DST_SUBPROGRAM_decl_vtable_elem_location(attr) = vtable_elem_location;
#endif /* CPLUSPLUSIL */
      DST_SUBPROGRAM_decl_first_child(attr) = DST_INVALID_IDX;
      DST_SUBPROGRAM_decl_last_child(attr) = DST_INVALID_IDX;
      DST_SET_declaration(flag);
   }
   else
   {
      DST_SUBPROGRAM_def_decl(attr) = decl;
      DST_SUBPROGRAM_def_name(attr) = DST_mk_name(name);
      DST_SUBPROGRAM_decl_linkage_name(attr) = DST_INVALID_IDX;
      DST_SUBPROGRAM_def_pubname(attr) = DST_INVALID_IDX;
      DST_SUBPROGRAM_def_specification(attr) = DST_INVALID_IDX;
      DST_SUBPROGRAM_def_type(attr) = type;

      DST_ASSOC_INFO_st_idx(DST_SUBPROGRAM_def_st(attr)) = subpr;

      DST_SUBPROGRAM_def_inline(attr) = inlin;
#ifdef CPLUSPLUSIL
      DST_SUBPROGRAM_def_virtuality(attr) = virtuality;
      DST_SUBPROGRAM_def_vtable_elem_location(attr) = vtable_elem_location;
#endif /* CPLUSPLUSIL */
      DST_SUBPROGRAM_def_clone_origin(attr) = DST_INVALID_IDX;
      DST_SUBPROGRAM_def_first_child(attr) = DST_INVALID_IDX;
      DST_SUBPROGRAM_def_last_child(attr) = DST_INVALID_IDX;
   }
   if (is_artificial)
      DST_SET_artificial(flag);
   if (is_prototyped)
      DST_SET_prototyped(flag);
   if (is_external)
      DST_SET_external(flag);
  DST_INFO_IDX t =
    DST_init_info(info_idx, DW_TAG_subprogram, flag, attr_idx);
  return t;
}



#if defined(_SUPPORT_IPA) || defined(_STANDALONE_INLINER)
    /* This function is not needed with _LEGO_CLONER */

/* Turns an existing DW_TAG_subprogram entry into an abstract instance
 * just by setting the appropiate DW_AT_inline flag
*/
void
DST_subprogram_concrete_to_abstract(DST_INFO_IDX subprogram)
{
   DST_INFO        *info;
   DST_ATTR_IDX    attr_idx;
   DST_SUBPROGRAM  *attr;
   DST_flag        flag;

   info = DST_INFO_IDX_TO_PTR(subprogram);

   DST_ASSERT((DST_INFO_tag(info) == DW_TAG_subprogram), "Bad DST_INFO_IDX input to DST_subprogram_concrete_to_abstract -- should be DW_TAG_subprogram");
   
   flag = DST_INFO_flag(info);
   attr_idx = DST_INFO_attributes(info);
   attr = DST_ATTR_IDX_TO_PTR(attr_idx, DST_SUBPROGRAM);
   
   if (DST_IS_declaration(flag))
   {
      if (DST_SUBPROGRAM_decl_inline(attr) == DW_INL_declared_not_inlined)
          DST_SUBPROGRAM_decl_inline(attr) = DW_INL_declared_inlined;
      else /* this subprogram is inlined by IPA, not by user's request */
          DST_SUBPROGRAM_decl_inline(attr) = DW_INL_inlined;
   }
   else 
   {
      if (DST_SUBPROGRAM_def_inline(attr) == DW_INL_declared_not_inlined)
          DST_SUBPROGRAM_def_inline(attr) = DW_INL_declared_inlined;
      else /* this subprogram is inlined by IPA, not by user's request */
          DST_SUBPROGRAM_def_inline(attr) = DW_INL_inlined;
   }
}
#endif


#if defined(_SUPPORT_IPA) || defined(_STANDALONE_INLINER) || defined(_LEGO_CLONER)
/* Creates a DW_TAG_subprogram entry for a cloned subroutine and returns its idx
 * 
*/
DST_INFO_IDX 
DST_mk_cloned_subprogram(USRCPOS      decl,
		  char        *name,
		  DST_INFO_IDX type,
		  DST_INFO_IDX clone_origin,
		  ST_IDX       subst,   /* front-end st */
		  DST_inline   inlin,
		  DST_virtuality virtuality)
{
   DST_INFO_IDX    info_idx;
   DST_ATTR_IDX    attr_idx;
   DST_flag        flag = DST_no_flag;
   DST_SUBPROGRAM *attr;

   info_idx = DST_mk_info();
   attr_idx = DST_mk_attr(DST_SUBPROGRAM);
   attr = DST_ATTR_IDX_TO_PTR(attr_idx, DST_SUBPROGRAM);

   DST_SUBPROGRAM_def_decl(attr) = decl;
   DST_SUBPROGRAM_def_name(attr) = DST_mk_name(name);
   DST_SUBPROGRAM_decl_linkage_name(attr) = DST_INVALID_IDX;
   DST_SUBPROGRAM_def_pubname(attr) = DST_INVALID_IDX;
   DST_SUBPROGRAM_def_specification(attr) = DST_INVALID_IDX;
   DST_SUBPROGRAM_def_type(attr) = type;

   /* "st" starts off pointing to a front-end routine, which is
    * later converted to point to the corrsponding back-end ST entry.
    */
   DST_ASSOC_INFO_st_idx(DST_SUBPROGRAM_def_st (attr)) = subst;

  /* cloned subprogram are not declared inline or inlined initially
   * even if the original subprogram has been declared inline or has been
   * inlined by IPA
   */
  if (inlin == DW_INL_declared_inlined)
      DST_SUBPROGRAM_def_inline(attr) = DW_INL_declared_not_inlined;
  else 
      DST_SUBPROGRAM_def_inline(attr) = DW_INL_not_inlined;

   DST_SUBPROGRAM_def_virtuality(attr) = virtuality;
   DST_SUBPROGRAM_def_first_child(attr) = DST_INVALID_IDX;
   DST_SUBPROGRAM_def_last_child(attr) = DST_INVALID_IDX;
   DST_SUBPROGRAM_def_clone_origin(attr) = clone_origin;
   DST_SET_external(flag);
  return DST_init_info(info_idx, DW_TAG_subprogram, flag, attr_idx);
}
#endif


/*
 * Adds a pubname pseudo attribute to the given subprogram.  Used to name C++
 * member functions 
 */
void
DST_add_pubname_to_subprogram (DST_INFO_IDX subprogram,
			       char        *pubname)
{
   DST_INFO       *info;
   DST_ATTR_IDX    attr_idx;
   DST_SUBPROGRAM *attr;

   info = DST_INFO_IDX_TO_PTR(subprogram);
   attr_idx = DST_INFO_attributes(info);
   attr = DST_ATTR_IDX_TO_PTR(attr_idx, DST_SUBPROGRAM);

   DST_ASSERT(!(DST_IS_declaration (DST_INFO_flag(info))),
	      "Try to put pubname on declaration");

   DST_ASSERT (DST_INFO_tag(info) == DW_TAG_subprogram,
	       "Try to set pubname on non subprogram");

   DST_SUBPROGRAM_def_pubname(attr) = DST_mk_name(pubname);  
}


/* Adds a linkage_name (i.e. a mangled name) attribute to the given 
 * subprogram.  Called for C++ functions.
 * We should have a data version too.
*/
void
DST_add_linkage_name_to_subprogram(DST_INFO_IDX subprogram,
				   char        *linkage_name)
{
   DST_INFO       *info;
   DST_ATTR_IDX    attr_idx;
   DST_SUBPROGRAM *attr;

   info = DST_INFO_IDX_TO_PTR(subprogram);
   if(DST_INFO_tag(info) == DW_TAG_subprogram) {
   DST_ASSERT (DST_INFO_tag(info) == DW_TAG_subprogram,
          "Try to set linkage_name on non subprogram");
   attr_idx = DST_INFO_attributes(info);
   attr = DST_ATTR_IDX_TO_PTR(attr_idx, DST_SUBPROGRAM);
   if (DST_IS_declaration(DST_INFO_flag(info)))
        DST_SUBPROGRAM_decl_linkage_name(attr) = DST_mk_name(linkage_name);
   else
        DST_SUBPROGRAM_def_linkage_name(attr) = DST_mk_name(linkage_name);
   }
}
   

/* Adds a DW_AT_specification attribute linking the body definition of a
 * member function to its definition within a class.
*/
void
DST_add_specification_to_subprogram (DST_INFO_IDX subprogram_def,
				     DST_INFO_IDX subprogram_decl)
{
   DST_INFO       *def_info;
   DST_ATTR_IDX    def_attr_idx;
   DST_SUBPROGRAM *def_attr;

   def_info = DST_INFO_IDX_TO_PTR(subprogram_def);

   DST_ASSERT(!(DST_IS_declaration (DST_INFO_flag(def_info))),
	      "Try to put specification on declaration");
   DST_ASSERT (DST_INFO_tag(def_info) == DW_TAG_subprogram,
	       "Try to set specification on non subprogram");

   def_attr_idx = DST_INFO_attributes(def_info);
   def_attr = DST_ATTR_IDX_TO_PTR(def_attr_idx, DST_SUBPROGRAM);
   DST_SUBPROGRAM_def_specification(def_attr) = subprogram_decl;
}

/* Adds a DW_AT_specification attribute linking the definition of a static
 * member variable to its declaration within a class.
 */
void
DST_add_specification_to_variable (DST_INFO_IDX variable_def,
				   DST_INFO_IDX field_decl)
{
   DST_INFO       *def_info;
   DST_ATTR_IDX    def_attr_idx;
   DST_VARIABLE   *def_attr;

   def_info = DST_INFO_IDX_TO_PTR(variable_def);

   DST_ASSERT(!(DST_IS_declaration (DST_INFO_flag(def_info))),
	"Try to put specification on declaration");
   DST_ASSERT (DST_INFO_tag(def_info) == DW_TAG_variable,
	"Try to set field specification on non variable");

   def_attr_idx = DST_INFO_attributes(def_info);
   def_attr = DST_ATTR_IDX_TO_PTR(def_attr_idx, DST_VARIABLE);
   DST_VARIABLE_def_specification(def_attr) = field_decl;
}

#ifdef KEY
/* Adds a linkage name attribute to a variaable
 */
void
DST_add_linkage_name_to_variable (DST_INFO_IDX variable_def,
				   char *linkage_name)
{
   DST_INFO       *info;
   DST_ATTR_IDX    attr_idx;
   DST_VARIABLE   *attr;

   info = DST_INFO_IDX_TO_PTR(variable_def);

   DST_ASSERT (DST_INFO_tag(info) == DW_TAG_variable,
	"Try to set field linkage_name on non variable");

   attr_idx = DST_INFO_attributes(info);
   attr = DST_ATTR_IDX_TO_PTR(attr_idx, DST_VARIABLE);

   if (DST_IS_declaration(DST_INFO_flag(info))) {
        DST_VARIABLE_decl_linkage_name(attr) = DST_mk_name(linkage_name);
   } else {
        DST_VARIABLE_def_linkage_name(attr) = DST_mk_name(linkage_name);
   }
}

#endif

/* Creates a DW_TAG_entry_point entry and returns its idx
*/
DST_INFO_IDX 
DST_mk_entry_point(USRCPOS      decl,
		  char        *name,
		  DST_INFO_IDX type,
		  ST_IDX	subpr)
{
   DST_INFO_IDX    info_idx;
   DST_ATTR_IDX    attr_idx;
   DST_flag        flag = DST_no_flag;
   DST_ENTRY_POINT *attr;

#if !(defined(_SUPPORT_IPA) || defined(_STANDALONE_INLINER))
   DST_enter_mk(DST_making_dbg_info, last_info_idx);
#endif
   
   info_idx = DST_mk_info();
   attr_idx = DST_mk_attr(DST_ENTRY_POINT);
   attr = DST_ATTR_IDX_TO_PTR(attr_idx, DST_ENTRY_POINT);

   DST_ENTRY_POINT_name(attr) = DST_mk_name(name);
   DST_ENTRY_POINT_type(attr) = type;
   DST_ENTRY_POINT_decl(attr) = decl;
   DST_ENTRY_POINT_first_child(attr) = DST_INVALID_IDX;
   DST_ENTRY_POINT_last_child(attr) = DST_INVALID_IDX;
   DST_ASSOC_INFO_st_idx(DST_ENTRY_POINT_st(attr)) = subpr;
   return DST_init_info(info_idx, DW_TAG_entry_point, flag, attr_idx);
}

/* create a DW_TAG_common_block entry and return its index 
 *
*/
DST_INFO_IDX 
DST_mk_common_block(char	*name,
		    ST_IDX	subpr)
{
   DST_INFO_IDX    info_idx;
   DST_ATTR_IDX    attr_idx;
   DST_flag        flag = DST_no_flag;
   DST_COMMON_BLOCK *attr;

#if !(defined(_SUPPORT_IPA) || defined(_STANDALONE_INLINER))
   DST_enter_mk(DST_making_dbg_info, last_info_idx);
#endif
   
   info_idx = DST_mk_info();
   attr_idx = DST_mk_attr(DST_COMMON_BLOCK);
   attr = DST_ATTR_IDX_TO_PTR(attr_idx, DST_COMMON_BLOCK);
   DST_COMMON_BLOCK_name(attr) = DST_mk_name(name);
   DST_COMMON_BLOCK_first_child(attr) = DST_INVALID_IDX;
   DST_COMMON_BLOCK_last_child(attr) = DST_INVALID_IDX;
   DST_ASSOC_INFO_st_idx(DST_COMMON_BLOCK_st(attr)) = subpr;
   return DST_init_info(info_idx, DW_TAG_common_block, flag, attr_idx);
}

/* create a DW_TAG_common_inclusion and return its idx 
 *
*/
DST_INFO_IDX 
DST_mk_common_incl( USRCPOS      decl,
		    DST_INFO_IDX comblk)
{
   DST_INFO_IDX    info_idx;
   DST_ATTR_IDX    attr_idx;
   DST_flag        flag = DST_no_flag;
   DST_COMMON_INCL *attr;

#if !(defined(_SUPPORT_IPA) || defined(_STANDALONE_INLINER))
   DST_enter_mk(DST_making_dbg_info, last_info_idx);
#endif
   
   info_idx = DST_mk_info();
   attr_idx = DST_mk_attr(DST_COMMON_INCL);
   attr = DST_ATTR_IDX_TO_PTR(attr_idx, DST_COMMON_INCL);
   DST_COMMON_INCL_decl(attr) = decl;
   DST_COMMON_INCL_com_blk(attr) = comblk;
   return DST_init_info(info_idx, DW_TAG_common_inclusion, flag, attr_idx);
}

#ifdef KEY /* Bug 3507 */
/* create a DW_TAG_imported_declaration and return its idx 
 *
*/
DST_INFO_IDX 
DST_mk_imported_decl( char *mangled_name,
		    char *name)
{
   DST_INFO_IDX    info_idx;
   DST_ATTR_IDX    attr_idx;
   DST_IMPORTED_DECL *attr;

#if !(defined(_SUPPORT_IPA) || defined(_STANDALONE_INLINER))
   DST_enter_mk(DST_making_dbg_info, last_info_idx);
#endif
   
   info_idx = DST_mk_info();
   attr_idx = DST_mk_attr(DST_IMPORTED_DECL);
   attr = DST_ATTR_IDX_TO_PTR(attr_idx, DST_IMPORTED_DECL);
   /* I'm guessing the way to set this is to import a symbol, derived
    * from the mangled name, which has been planted in the .debug_info
    * section of the exporting .o file at the start of the DW_TAG_module. */
   DST_IMPORTED_DECL_import(attr) /* = ??? */;
   DST_IMPORTED_DECL_name(attr) = DST_mk_name(name);
   return DST_init_info(info_idx, DW_TAG_imported_declaration, DST_no_flag,
     attr_idx);
}

/* create a DW_TAG_imported_declaration and return its idx 
 *
*/
DST_INFO_IDX 
DST_mk_module(USRCPOS decl, /* source location */
		  char *name)
{
   DST_INFO_IDX    info_idx;
   DST_ATTR_IDX    attr_idx;
   DST_flag        flag = DST_no_flag;
   DST_MODULE *attr;

#if !(defined(_SUPPORT_IPA) || defined(_STANDALONE_INLINER))
   DST_enter_mk(DST_making_dbg_info, last_info_idx);
#endif
   
   info_idx = DST_mk_info();
   attr_idx = DST_mk_attr(DST_MODULE);
   attr = DST_ATTR_IDX_TO_PTR(attr_idx, DST_MODULE);
   DST_MODULE_decl(attr) = decl;
   DST_MODULE_name(attr) = DST_mk_name(name);
   DST_MODULE_first_child(attr) = DST_INVALID_IDX;
   DST_MODULE_last_child(attr) = DST_INVALID_IDX;
   DST_SET_declaration(flag);
   DST_INFO_IDX t =
     DST_init_info(info_idx, DW_TAG_module, flag, attr_idx);
   return t;
}
#endif /* KEY Bug 3507 */

/* Creates a DW_TAG_lexical_block entry and returns its idx.
*/
DST_INFO_IDX 
DST_mk_lexical_block(char         *name,         /* NULL if unnamed */
		     ST_IDX        low_pc, 
                     ST_IDX        high_pc,
		     DST_INFO_IDX  abstract_origin) /* NULL if none */
{
   DST_INFO_IDX       info_idx;
   DST_ATTR_IDX       attr_idx;
   DST_flag           flag = DST_no_flag;
   DST_LEXICAL_BLOCK *attr;

#if !(defined(_SUPPORT_IPA) || defined(_STANDALONE_INLINER))
   DST_enter_mk(DST_making_dbg_info, last_info_idx);
#endif
   info_idx = DST_mk_info();
   attr_idx = DST_mk_attr(DST_LEXICAL_BLOCK);
   attr = DST_ATTR_IDX_TO_PTR(attr_idx, DST_LEXICAL_BLOCK);
   DST_LEXICAL_BLOCK_name(attr) = DST_mk_name(name);
   if (low_pc != ST_IDX_ZERO && high_pc != ST_IDX_ZERO)
   {
      DST_ASSOC_INFO_st_idx(DST_LEXICAL_BLOCK_low_pc(attr)) = low_pc;
      DST_ASSOC_INFO_st_idx(DST_LEXICAL_BLOCK_high_pc(attr)) = high_pc;
   }
   DST_LEXICAL_BLOCK_abstract_origin(attr) = abstract_origin;
   DST_LEXICAL_BLOCK_first_child(attr) = DST_INVALID_IDX;
   DST_LEXICAL_BLOCK_last_child(attr) = DST_INVALID_IDX;

   return DST_init_info(info_idx, DW_TAG_lexical_block, flag, attr_idx);
}



/* Creates a DW_TAG_label entry and returns its idx.
*/
extern DST_INFO_IDX 
DST_mk_label(char         *name,            /* NULL if unnamed */
	     ST_IDX        low_pc,          /* ptr to front-end label */
	     DST_INFO_IDX  abstract_origin) /* NULL if none */
{
   DST_INFO_IDX info_idx;
   DST_ATTR_IDX attr_idx;
   DST_flag     flag = DST_no_flag;
   DST_LABEL   *attr;

   DST_ASSERT(low_pc != ST_IDX_ZERO, "Missing low_pc value for label");
   
#if !(defined(_SUPPORT_IPA) || defined(_STANDALONE_INLINER))
   DST_enter_mk(DST_making_dbg_info, last_info_idx);   
#endif
   info_idx = DST_mk_info();
   attr_idx = DST_mk_attr(DST_LABEL);
   attr = DST_ATTR_IDX_TO_PTR(attr_idx, DST_LABEL);
   DST_LABEL_name(attr) = DST_mk_name(name);

   DST_ASSOC_INFO_st_idx(DST_LABEL_low_pc(attr)) = low_pc;

   DST_LABEL_abstract_origin(attr) = abstract_origin;
   return DST_init_info(info_idx, DW_TAG_label, flag, attr_idx);
}



/* Creates a DW_TAG_variable entry for a constant variable and returns
 * its idx.
*/
DST_INFO_IDX 
DST_mk_variable_const(USRCPOS         decl, /* Source location */
		      char           *name,    /* Name of const variable */
		      DST_INFO_IDX    type,    /* Type of const variable */
                      BOOL            is_automatic,
                      BOOL            is_external,
		      DST_CONST_VALUE cval)    /* Constant value */
{
   DST_INFO_IDX  info_idx;
   DST_ATTR_IDX  attr_idx;
   DST_flag      flag = DST_no_flag;
   DST_VARIABLE *attr;

#if !(defined(_SUPPORT_IPA) || defined(_STANDALONE_INLINER))
   DST_enter_mk(DST_making_dbg_info, last_info_idx);
#endif
   
   info_idx = DST_mk_info();
   attr_idx = DST_mk_attr(DST_VARIABLE);
   attr = DST_ATTR_IDX_TO_PTR(attr_idx, DST_VARIABLE);
   DST_VARIABLE_constant_decl(attr) = decl;
   DST_VARIABLE_constant_name(attr) = DST_mk_name(name);
   DST_VARIABLE_constant_type(attr) = type;
   DST_VARIABLE_constant_cval(attr) = cval;
   DST_SET_const(flag);
   if (is_automatic)
      DST_SET_automatic(flag);
   else if (is_external)
      DST_SET_external(flag);
   return DST_init_info(info_idx, DW_TAG_variable, flag, attr_idx);
}

/* Creates a DW_TAG_variable entry for a variable in common block & returns
 * its idx.
*/
DST_INFO_IDX 
DST_mk_variable_comm( USRCPOS         decl, /* Source location */
		      char           *name,    /* Name of const variable */
		      DST_INFO_IDX    type,    /* Type of const variable */
		      ST_IDX	      var, 
		      UINT64 	      offset)    /* offset from common block */
{
   DST_INFO_IDX  info_idx;
   DST_ATTR_IDX  attr_idx;
   DST_flag      flag = DST_no_flag;
   DST_VARIABLE *attr;

#if !(defined(_SUPPORT_IPA) || defined(_STANDALONE_INLINER))
   DST_enter_mk(DST_making_dbg_info, last_info_idx);
#endif
   
   info_idx = DST_mk_info();
   attr_idx = DST_mk_attr(DST_VARIABLE);
   attr = DST_ATTR_IDX_TO_PTR(attr_idx, DST_VARIABLE);
   DST_VARIABLE_comm_decl(attr) = decl;
   DST_VARIABLE_comm_name(attr) = DST_mk_name(name);
   DST_VARIABLE_comm_type(attr) = type;
   DST_ASSOC_INFO_st_idx(DST_VARIABLE_comm_st(attr)) = var;
   DST_VARIABLE_comm_offs(attr) = offset;
   DST_SET_comm(flag);
   return DST_init_info(info_idx, DW_TAG_variable, flag, attr_idx);
}

/* Creates a DW_TAG_variable entry for the definition for a member of
 * class and returns its idx.
*/
DST_INFO_IDX 
DST_mk_variable_memdef(USRCPOS      decl, /* Source location */
		       ST_IDX var,
		       DST_INFO_IDX spec) /* Class member decl */
{
   DST_INFO_IDX  info_idx;
   DST_ATTR_IDX  attr_idx;
   DST_flag      flag = DST_no_flag;
   DST_VARIABLE *attr;

#if !(defined(_SUPPORT_IPA) || defined(_STANDALONE_INLINER))
   DST_enter_mk(DST_making_dbg_info, last_info_idx);
#endif
   
   info_idx = DST_mk_info();
   attr_idx = DST_mk_attr(DST_VARIABLE);
   attr = DST_ATTR_IDX_TO_PTR(attr_idx, DST_VARIABLE);
   DST_VARIABLE_memdef_decl(attr) = decl;
   DST_VARIABLE_memdef_spec(attr) = spec;

   /* location is obtained through the back-end version of st */
   DST_ASSOC_INFO_st_idx(DST_VARIABLE_memdef_st(attr)) = var;

   DST_SET_memdef(flag);
   return DST_init_info(info_idx, DW_TAG_variable, flag, attr_idx);
}


/* Creates a DW_TAG_variable entry for the definition/declaration
 * for other forms of variables than those handled above.
*/
DST_INFO_IDX 
DST_mk_variable(USRCPOS      decl,     /* Source location */
		char        *name,     /* Name of variable */
		DST_INFO_IDX type,     /* Type of variable */
		UINT64	     offs,     /* offset from front end variable */
		ST_IDX       var, 
		DST_INFO_IDX abstract_origin, /* for inlined proc */
		BOOL         is_declaration,
		BOOL         is_automatic,
		BOOL         is_external,    /* Type of variable */
		BOOL	     is_artificial)
{
   DST_INFO_IDX  info_idx;
   DST_ATTR_IDX  attr_idx;
   DST_flag      flag = DST_no_flag;
   DST_VARIABLE *attr;

   DST_ASSERT(!(is_automatic && is_declaration), "automatic must be def!");

#if !(defined(_SUPPORT_IPA) || defined(_STANDALONE_INLINER))
   DST_enter_mk(DST_making_dbg_info, last_info_idx);   
#endif
   info_idx = DST_mk_info();
   attr_idx = DST_mk_attr(DST_VARIABLE);
   attr = DST_ATTR_IDX_TO_PTR(attr_idx, DST_VARIABLE);
   if (is_declaration)
   {
      DST_VARIABLE_decl_decl(attr) = decl;
      DST_VARIABLE_decl_name(attr) = DST_mk_name(name);
      DST_VARIABLE_decl_type(attr) = type;
#ifdef KEY
      DST_VARIABLE_decl_linkage_name(attr) = DST_INVALID_IDX;
#endif
      DST_SET_declaration(flag);
   }   
   else
   {
      DST_VARIABLE_def_decl(attr) = decl;
      DST_VARIABLE_def_name(attr) = DST_mk_name(name);
      DST_VARIABLE_def_type(attr) = type;
      DST_VARIABLE_def_offs(attr) = offs;
      DST_VARIABLE_def_abstract_origin(attr) = abstract_origin;
      DST_VARIABLE_def_specification(attr) = DST_INVALID_IDX;
#ifdef KEY
      DST_VARIABLE_def_linkage_name(attr) = DST_INVALID_IDX;
#endif

      /* location is obtained through the back-end version of st */
      DST_ASSOC_INFO_st_idx(DST_VARIABLE_def_st(attr)) = var;
   }   
   if (is_automatic)
      DST_SET_automatic(flag);
   else if (is_external)
      DST_SET_external(flag);
   if (is_artificial)
      DST_SET_artificial(flag);
   return DST_init_info(info_idx, DW_TAG_variable, flag, attr_idx);
}


/* Creates a DW_TAG_formal_parameter entry.
*/
DST_INFO_IDX 
DST_mk_formal_parameter(USRCPOS       decl,        /* Source location */
			char         *name,        /* Name of parm */
			DST_INFO_IDX  type,        /* Type of parm */
			ST_IDX	      parm,        /* symbol */
                        DST_INFO_IDX  abstract_origin, /* For inlined proc */
			DST_INFO_IDX  default_val, /* (C++) param value */
			BOOL          is_optional, /* Optional param (C++) */
			BOOL          is_variable, /* Variable param  */
                                     // is_variable TRUE where the 
                                     // language says a change
                                     // in the argument changes the caller
                                     // value. FALSE for C, C++.

			BOOL 	      is_artificial, /* compiler inserted */
			BOOL          is_declaration_only)
{
   DST_INFO_IDX  info_idx;
   DST_ATTR_IDX  attr_idx;
   DST_flag      flag = DST_no_flag;
   DST_FORMAL_PARAMETER *attr;

#if !(defined(_SUPPORT_IPA) || defined(_STANDALONE_INLINER))
   DST_enter_mk(DST_making_dbg_info, last_info_idx);
#endif

   info_idx = DST_mk_info();
   attr_idx = DST_mk_attr(DST_FORMAL_PARAMETER);
   attr = DST_ATTR_IDX_TO_PTR(attr_idx, DST_FORMAL_PARAMETER);
   DST_FORMAL_PARAMETER_decl(attr) = decl;
   if (name != NULL)
       DST_FORMAL_PARAMETER_name(attr) = DST_mk_name(name);
   else 
       DST_FORMAL_PARAMETER_name(attr) = DST_INVALID_IDX;
   DST_FORMAL_PARAMETER_type(attr) = type;

   /* location is obtained through a pointer to back-end ST entry */
   DST_ASSOC_INFO_st_idx(DST_FORMAL_PARAMETER_st(attr)) = parm;
   
   DST_FORMAL_PARAMETER_default_val(attr) = default_val;
   DST_FORMAL_PARAMETER_abstract_origin(attr) = abstract_origin;
   if (is_declaration_only)
	DST_SET_declaration(flag);
   if (is_optional)
      DST_SET_optional_parm(flag);
   if (is_variable)
      DST_SET_variable_parm(flag);
   if (is_artificial)
      DST_SET_artificial(flag);
   return DST_init_info(info_idx, DW_TAG_formal_parameter, flag, attr_idx);
}



/* Creates a DW_TAG_unspecified_parameters entry.
*/
DST_INFO_IDX 
DST_mk_unspecified_parameters(USRCPOS  decl,    /* Source location */
                              DST_INFO_IDX  abstract_origin) /* For inlined */
{
   DST_INFO_IDX                info_idx;
   DST_ATTR_IDX                attr_idx;
   DST_flag                    flag = DST_no_flag;
   DST_UNSPECIFIED_PARAMETERS *attr;

#if !(defined(_SUPPORT_IPA) || defined(_STANDALONE_INLINER))
   DST_enter_mk(DST_making_dbg_info, last_info_idx);
#endif

   info_idx = DST_mk_info();
   attr_idx = DST_mk_attr(DST_UNSPECIFIED_PARAMETERS);
   attr = DST_ATTR_IDX_TO_PTR(attr_idx, DST_UNSPECIFIED_PARAMETERS);
   DST_UNSPECIFIED_PARAMETERS_decl(attr) = decl;
   DST_UNSPECIFIED_PARAMETERS_abstract_origin(attr) = abstract_origin;
   return DST_init_info(info_idx, DW_TAG_unspecified_parameters, 
		       flag, attr_idx);
}


/* Creates a DW_TAG_constant entry.
*/
DST_INFO_IDX 
DST_mk_constant_def(USRCPOS         decl,  /* Source location */
		    char           *name,  /* Name of constant */
		    DST_INFO_IDX    type,  /* Type of constant */
		    DST_CONST_VALUE cval,  /* Value of constant */
		    BOOL            is_external)  /* External? */
{
   DST_INFO_IDX  info_idx;
   DST_ATTR_IDX  attr_idx;
   DST_flag      flag = DST_no_flag;
   DST_CONSTANT *attr;

#if !(defined(_SUPPORT_IPA) || defined(_STANDALONE_INLINER))
   DST_enter_mk(DST_making_dbg_info, last_info_idx);
#endif

   info_idx = DST_mk_info();
   attr_idx = DST_mk_attr(DST_CONSTANT);
   attr = DST_ATTR_IDX_TO_PTR(attr_idx, DST_CONSTANT);
   DST_CONSTANT_def_decl(attr) = decl;
   DST_CONSTANT_def_name(attr) = DST_mk_name(name);
   DST_CONSTANT_def_type(attr) = type;
   DST_CONSTANT_def_cval(attr) = cval;
   if (is_external)
      DST_SET_external(flag);
   return DST_init_info(info_idx, DW_TAG_constant, flag, attr_idx);
}



/* Creates a DW_TAG_constant entry.
*/
DST_INFO_IDX 
DST_mk_constant_decl(USRCPOS       decl,  /* Source location */
		     char         *name,  /* Name of constant */
		     DST_INFO_IDX  type,  /* Type of constant */
		     BOOL          is_external)  /* External? */
{
   DST_INFO_IDX  info_idx;
   DST_ATTR_IDX  attr_idx;
   DST_flag      flag = DST_no_flag;
   DST_CONSTANT *attr;

#if !(defined(_SUPPORT_IPA) || defined(_STANDALONE_INLINER))
   DST_enter_mk(DST_making_dbg_info, last_info_idx);
#endif

   info_idx = DST_mk_info();
   attr_idx = DST_mk_attr(DST_CONSTANT);
   attr = DST_ATTR_IDX_TO_PTR(attr_idx, DST_CONSTANT);
   DST_CONSTANT_decl_decl(attr) = decl;
   DST_CONSTANT_decl_name(attr) = DST_mk_name(name);
   DST_CONSTANT_decl_type(attr) = type;
   if (is_external)
      DST_SET_external(flag);
   DST_SET_declaration(flag);
   return DST_init_info(info_idx, DW_TAG_constant, flag, attr_idx);
}



/* Creates a DW_TAG_basetype entry.
*/
DST_INFO_IDX 
DST_mk_basetype(const char      *name,      /* Name of type */
		DST_ATE_encoding encoding,  /* How to encode/interpret data */
		DST_size_t       byte_size) /* Size of object */
{
   DST_INFO_IDX  info_idx;
   DST_ATTR_IDX  attr_idx;
   DST_flag      flag = DST_no_flag;
   DST_BASETYPE *attr;

#if !(defined(_SUPPORT_IPA) || defined(_STANDALONE_INLINER))
   DST_enter_mk(DST_making_dbg_info, last_info_idx);
#endif

   info_idx = DST_mk_info();
   attr_idx = DST_mk_attr(DST_BASETYPE);
   attr = DST_ATTR_IDX_TO_PTR(attr_idx, DST_BASETYPE);
   DST_BASETYPE_name(attr) = DST_mk_name(name);
   DST_BASETYPE_encoding(attr) = encoding;
   DST_BASETYPE_byte_size(attr) = byte_size;

   return DST_init_info(info_idx, DW_TAG_base_type, flag, attr_idx);
}


/* Creates a DW_TAG_const_type entry.
*/
DST_INFO_IDX 
DST_mk_const_type(DST_INFO_IDX type)    /* Qualified type */
{
   DST_INFO_IDX    info_idx;
   DST_ATTR_IDX    attr_idx;
   DST_flag        flag = DST_no_flag;
   DST_CONST_TYPE *attr;

#if !(defined(_SUPPORT_IPA) || defined(_STANDALONE_INLINER))
   DST_enter_mk(DST_making_dbg_info, last_info_idx);
#endif

   info_idx = DST_mk_info();
   attr_idx = DST_mk_attr(DST_CONST_TYPE);
   attr = DST_ATTR_IDX_TO_PTR(attr_idx, DST_CONST_TYPE);
   DST_CONST_TYPE_type(attr) = type;
   return DST_init_info(info_idx, DW_TAG_const_type, flag, attr_idx);
}


/* Creates a DW_TAG_volatile_type entry.
*/
DST_INFO_IDX 
DST_mk_volatile_type(DST_INFO_IDX type)    /* Qualified type */
{
   DST_INFO_IDX       info_idx;
   DST_ATTR_IDX       attr_idx;
   DST_flag           flag = DST_no_flag;
   DST_VOLATILE_TYPE *attr;

#if !(defined(_SUPPORT_IPA) || defined(_STANDALONE_INLINER))
   DST_enter_mk(DST_making_dbg_info, last_info_idx);
#endif

   info_idx = DST_mk_info();
   attr_idx = DST_mk_attr(DST_VOLATILE_TYPE);
   attr = DST_ATTR_IDX_TO_PTR(attr_idx, DST_VOLATILE_TYPE);
   DST_VOLATILE_TYPE_type(attr) = type;
   return DST_init_info(info_idx, DW_TAG_volatile_type, flag, attr_idx);
}


/* Creates a DW_TAG_pointer_type entry.
*/
DST_INFO_IDX 
DST_mk_pointer_type(DST_INFO_IDX   type,          /* Type pointed to */
		    DST_addr_class address_class, /* How to dereference */
		    DST_size_t     byte_size)     /* Size */
{
   DST_INFO_IDX      info_idx;
   DST_ATTR_IDX      attr_idx;
   DST_flag          flag = DST_no_flag;
   DST_POINTER_TYPE *attr;

#if !(defined(_SUPPORT_IPA) || defined(_STANDALONE_INLINER))
   DST_enter_mk(DST_making_dbg_info, last_info_idx);
#endif

   info_idx = DST_mk_info();
   attr_idx = DST_mk_attr(DST_POINTER_TYPE);
   attr = DST_ATTR_IDX_TO_PTR(attr_idx, DST_POINTER_TYPE);
   DST_POINTER_TYPE_type(attr) = type;
   DST_POINTER_TYPE_address_class(attr) = address_class;
   DST_POINTER_TYPE_byte_size(attr) = byte_size;
   return DST_init_info(info_idx, DW_TAG_pointer_type, flag, attr_idx);
}


/* Creates a DW_TAG_reference_type entry.
*/
DST_INFO_IDX 
DST_mk_reference_type(DST_INFO_IDX   type,          /* Type pointed to */
		      DST_addr_class address_class, /* How to dereference */
		      DST_size_t     byte_size)     /* Size */
{
   DST_INFO_IDX       info_idx;
   DST_ATTR_IDX       attr_idx;
   DST_flag           flag = DST_no_flag;
   DST_REFERENCE_TYPE *attr;

#if !(defined(_SUPPORT_IPA) || defined(_STANDALONE_INLINER))
   DST_enter_mk(DST_making_dbg_info, last_info_idx);
#endif

   info_idx = DST_mk_info();
   attr_idx = DST_mk_attr(DST_REFERENCE_TYPE);
   attr = DST_ATTR_IDX_TO_PTR(attr_idx, DST_REFERENCE_TYPE);
   DST_REFERENCE_TYPE_type(attr) = type;
   DST_REFERENCE_TYPE_address_class(attr) = address_class;
   DST_REFERENCE_TYPE_byte_size(attr) = byte_size;
   return DST_init_info(info_idx, DW_TAG_reference_type, flag, attr_idx);
}


/* Creates a DW_TAG_typedef entry.
*/
DST_INFO_IDX
DST_mk_typedef(USRCPOS       decl,            /* Source location */
	       char         *name,            /* Name of type */
	       DST_INFO_IDX  type,            /* Defining type */
               DST_INFO_IDX  abstract_origin) /* In scope of inlined proc */
{
   DST_INFO_IDX info_idx;
   DST_ATTR_IDX attr_idx;
   DST_flag     flag = DST_no_flag;
   DST_TYPEDEF *attr;

#if !(defined(_SUPPORT_IPA) || defined(_STANDALONE_INLINER))
   DST_enter_mk(DST_making_dbg_info, last_info_idx);
#endif

   info_idx = DST_mk_info();
   attr_idx = DST_mk_attr(DST_TYPEDEF);
   attr = DST_ATTR_IDX_TO_PTR(attr_idx, DST_TYPEDEF);
   DST_TYPEDEF_decl(attr)            = decl;
   DST_TYPEDEF_name(attr)            = DST_mk_name(name);
   DST_TYPEDEF_type(attr)            = type;
   DST_TYPEDEF_abstract_origin(attr) = abstract_origin;
   return DST_init_info(info_idx, DW_TAG_typedef, flag, attr_idx);
}

/* Creates a DW_TAG_ptr_to_member_type entry.
*/
DST_INFO_IDX
DST_mk_ptr_to_member_type(USRCPOS	decl,	    /* Source location */
			  char *	name, 	    /* Name of type */
			  DST_INFO_IDX	type,       /* Type of member */
			  DST_INFO_IDX  class_type) /* Type of class */
{
   DST_INFO_IDX 	  info_idx;
   DST_ATTR_IDX		  attr_idx;
   DST_flag               flag = DST_no_flag;
   DST_PTR_TO_MEMBER_TYPE *attr;

#if !(defined(_SUPPORT_IPA) || defined(_STANDALONE_INLINER))
   DST_enter_mk(DST_making_dbg_info, last_info_idx);
#endif

   info_idx = DST_mk_info();
   attr_idx = DST_mk_attr(DST_PTR_TO_MEMBER_TYPE);
   attr     = DST_ATTR_IDX_TO_PTR(attr_idx, DST_PTR_TO_MEMBER_TYPE);
   DST_PTR_TO_MEMBER_TYPE_name(attr) = DST_mk_name(name);
   DST_PTR_TO_MEMBER_TYPE_type(attr) = type;
   DST_PTR_TO_MEMBER_TYPE_class_type(attr) = class_type;
   return DST_init_info(info_idx,
		        DW_TAG_ptr_to_member_type,
		        flag,
			attr_idx);
}

/* Creates a DW_TAG_array_type entry.
*/
DST_INFO_IDX
DST_mk_array_type(USRCPOS      decl,      /* Source location */
		  char        *name,      /* Name of type */
		  DST_INFO_IDX type,      /* Element type */
		  DST_size_t   byte_size, /* Size of array, if known */
		  DST_INFO_IDX abstract_origin, /* In scope of inlined proc */
		  BOOL         is_incomplete)   /* Incomplete array */
{
   DST_INFO_IDX    info_idx;
   DST_ATTR_IDX    attr_idx;
   DST_flag        flag = DST_no_flag;
   DST_ARRAY_TYPE *attr;

#if !(defined(_SUPPORT_IPA) || defined(_STANDALONE_INLINER))
   DST_enter_mk(DST_making_dbg_info, last_info_idx);
#endif

   info_idx = DST_mk_info();
   attr_idx = DST_mk_attr(DST_ARRAY_TYPE);
   attr = DST_ATTR_IDX_TO_PTR(attr_idx, DST_ARRAY_TYPE);
   DST_ARRAY_TYPE_decl(attr) = decl;
   DST_ARRAY_TYPE_name(attr) = DST_mk_name(name);
   DST_ARRAY_TYPE_type(attr) = type;
   DST_ARRAY_TYPE_byte_size(attr) = byte_size;
   DST_ARRAY_TYPE_abstract_origin(attr) = abstract_origin;
   DST_ARRAY_TYPE_first_child(attr) = DST_INVALID_IDX;
   DST_ARRAY_TYPE_last_child(attr) = DST_INVALID_IDX;
   if (is_incomplete)
      DST_SET_declaration(flag); /* incomplete array */
   return DST_init_info(info_idx, DW_TAG_array_type, flag, attr_idx);
}


/* Creates a DW_TAG_subrange_type entry.
*/
DST_INFO_IDX
DST_mk_subrange_type(DST_flag is_lb_cval,
		     DST_cval_ref low, 		/* lower bound */
		     DST_flag is_ub_cval,
		     DST_cval_ref high) 	/* upper bound */
{
   DST_INFO_IDX       info_idx;
   DST_ATTR_IDX       attr_idx;
   DST_flag           flag = DST_no_flag;
   DST_SUBRANGE_TYPE *attr;

#if !(defined(_SUPPORT_IPA) || defined(_STANDALONE_INLINER))
   DST_enter_mk(DST_making_dbg_info, last_info_idx);
#endif

   info_idx = DST_mk_info();
   attr_idx = DST_mk_attr(DST_SUBRANGE_TYPE);
   attr = DST_ATTR_IDX_TO_PTR(attr_idx, DST_SUBRANGE_TYPE);
   if (is_lb_cval) {
   	DST_SUBRANGE_TYPE_lower_cval(attr) = low.cval;
	DST_SET_lb_cval(flag);
   } else {
   	DST_SUBRANGE_TYPE_lower_ref(attr) = low.ref;
   }
   if (is_ub_cval) {
   	DST_SUBRANGE_TYPE_upper_cval(attr) = high.cval;
	DST_SET_ub_cval(flag);
   } else {
   	DST_SUBRANGE_TYPE_upper_ref(attr) = high.ref;
   }

   DST_SUBRANGE_TYPE_stride_ref(attr) = DST_INVALID_IDX ; /* F90 dope, only */
  
   return DST_init_info(info_idx, DW_TAG_subrange_type, flag, attr_idx);
}


/* Creates a DW_TAG_string_type entry.
*/
extern DST_INFO_IDX
DST_mk_string_type(USRCPOS	   decl,	/* Source location */
		   char		  *name,	/* Name of type */
		   DST_flag	   is_len_cval,
		   DST_cval_ref	   len)		/* Length of string */
{
   DST_INFO_IDX        info_idx;
   DST_ATTR_IDX        attr_idx;
   DST_flag            flag = DST_no_flag;
   DST_STRING_TYPE *attr;

#if !(defined(_SUPPORT_IPA) || defined(_STANDALONE_INLINER))
   DST_enter_mk(DST_making_dbg_info, last_info_idx);
#endif

   info_idx = DST_mk_info();
   attr_idx = DST_mk_attr(DST_STRING_TYPE);
   attr = DST_ATTR_IDX_TO_PTR(attr_idx, DST_STRING_TYPE);
   DST_STRING_TYPE_decl(attr) = decl;
   DST_STRING_TYPE_name(attr) = DST_mk_name(name);
   if (is_len_cval) {
   	DST_STRING_TYPE_len_cval(attr) = len.cval;
	DST_SET_cval(flag);
   } else {
   	DST_STRING_TYPE_len_ref(attr) = len.ref;
   }
   return DST_init_info(info_idx, DW_TAG_string_type, flag, attr_idx);
}


/* Creates a DW_TAG_structure_type entry.
*/
DST_INFO_IDX
DST_mk_structure_type(USRCPOS      decl,      /* Source location */
		      char        *name,      /* Name of type */
		      DST_size_t   byte_size, /* Size of struct, if known */
		      DST_INFO_IDX abstract_origin,
		      BOOL         is_incomplete)
{
   DST_INFO_IDX        info_idx;
   DST_ATTR_IDX        attr_idx;
   DST_flag            flag = DST_no_flag;
   DST_STRUCTURE_TYPE *attr;

#if !(defined(_SUPPORT_IPA) || defined(_STANDALONE_INLINER))
   DST_enter_mk(DST_making_dbg_info, last_info_idx);
#endif

   info_idx = DST_mk_info();
   attr_idx = DST_mk_attr(DST_STRUCTURE_TYPE);
   attr = DST_ATTR_IDX_TO_PTR(attr_idx, DST_STRUCTURE_TYPE);
   DST_STRUCTURE_TYPE_decl(attr) = decl;
   DST_STRUCTURE_TYPE_name(attr) = DST_mk_name(name);
   DST_STRUCTURE_TYPE_byte_size(attr) = byte_size;
   DST_STRUCTURE_TYPE_abstract_origin(attr) = abstract_origin;
   DST_STRUCTURE_TYPE_first_child(attr) = DST_INVALID_IDX;
   DST_STRUCTURE_TYPE_last_child(attr) = DST_INVALID_IDX;
   if (is_incomplete)
      DST_SET_declaration(flag);
   return DST_init_info(info_idx, DW_TAG_structure_type, flag, attr_idx);
}


/* Creates a DW_TAG_union_type entry.
*/
DST_INFO_IDX
DST_mk_union_type(USRCPOS      decl,      /* Source location */
		  char        *name,      /* Name of type */
		  DST_size_t   byte_size, /* Size of union, if known */
		  DST_INFO_IDX abstract_origin,
		  BOOL         is_incomplete)
{
   DST_INFO_IDX    info_idx;
   DST_ATTR_IDX    attr_idx;
   DST_flag        flag = DST_no_flag;
   DST_UNION_TYPE *attr;

#if !(defined(_SUPPORT_IPA) || defined(_STANDALONE_INLINER))
   DST_enter_mk(DST_making_dbg_info, last_info_idx);
#endif

   info_idx = DST_mk_info();
   attr_idx = DST_mk_attr(DST_UNION_TYPE);
   attr = DST_ATTR_IDX_TO_PTR(attr_idx, DST_UNION_TYPE);
   DST_UNION_TYPE_decl(attr) = decl;
   DST_UNION_TYPE_name(attr) = DST_mk_name(name);
   DST_UNION_TYPE_byte_size(attr) = byte_size;
   DST_UNION_TYPE_abstract_origin(attr) = abstract_origin;
   DST_UNION_TYPE_first_child(attr) = DST_INVALID_IDX;
   DST_UNION_TYPE_last_child(attr) = DST_INVALID_IDX;
   if (is_incomplete)
      DST_SET_declaration(flag);
   return DST_init_info(info_idx, DW_TAG_union_type, flag, attr_idx);
}


/* Creates a DW_TAG_class_type entry.
*/
DST_INFO_IDX
DST_mk_class_type(USRCPOS      decl,      /* Source location */
		  char        *name,      /* Name of type */
		  DST_size_t   byte_size, /* Size of union, if known */
		  DST_INFO_IDX abstract_origin,
		  BOOL         is_incomplete)
{
   DST_INFO_IDX    info_idx;
   DST_ATTR_IDX    attr_idx;
   DST_flag        flag = DST_no_flag;
   DST_CLASS_TYPE *attr;

#if !(defined(_SUPPORT_IPA) || defined(_STANDALONE_INLINER))
   DST_enter_mk(DST_making_dbg_info, last_info_idx);
#endif

   info_idx = DST_mk_info();
   attr_idx = DST_mk_attr(DST_CLASS_TYPE);
   attr = DST_ATTR_IDX_TO_PTR(attr_idx, DST_CLASS_TYPE);
   DST_CLASS_TYPE_decl(attr) = decl;
   DST_CLASS_TYPE_name(attr) = DST_mk_name(name);
   DST_CLASS_TYPE_byte_size(attr) = byte_size;
   DST_CLASS_TYPE_abstract_origin(attr) = abstract_origin;
   DST_CLASS_TYPE_first_child(attr) = DST_INVALID_IDX;
   DST_CLASS_TYPE_last_child(attr) = DST_INVALID_IDX;
   if (is_incomplete)
      DST_SET_declaration(flag);
   return DST_init_info(info_idx, DW_TAG_class_type, flag, attr_idx);
}


/* Creates a DW_TAG_member entry.  Note that memb_loc is the field-
 * offset within the struct for non-bitfields.  The byte_size, bit-
 * offset, and bit_size only apply to bitfields, while memb_loc is the
 * location of the block containing (properly aligned) the bitfield.
*/
#ifndef KEY
DST_INFO_IDX
DST_mk_member(USRCPOS      decl,       /* Source location */
	      char        *name,       /* Name of member */
              DST_INFO_IDX type,       /* Type of member */
	      DST_size_t   memb_loc,   /* Byte-offset of member container */
	      DST_size_t   byte_size,  /* Byte-size of container */
	      DST_bitsize  bit_offset, /* Offset within container */
	      DST_bitsize  bit_size,   /* Size of bitfield member */
	      BOOL         is_bitfield,
	      BOOL         is_static,
	      BOOL         is_declaration,
	      BOOL	   is_artificial)
#else
DST_INFO_IDX
DST_mk_member(USRCPOS      decl,       /* Source location */
	      char        *name,       /* Name of member */
              DST_INFO_IDX type,       /* Type of member */
	      DST_size_t   memb_loc,   /* Byte-offset of member container */
	      DST_size_t   byte_size,  /* Byte-size of container */
	      DST_bitsize  bit_offset, /* Offset within container */
	      DST_bitsize  bit_size,   /* Size of bitfield member */
	      BOOL         is_bitfield,
	      BOOL         is_static,
	      BOOL         is_declaration,
	      BOOL	   is_artificial,
	      DST_accessibility accessibility)
#endif
{
   DST_INFO_IDX info_idx;
   DST_ATTR_IDX attr_idx;
   DST_flag     flag = DST_no_flag;
   DST_MEMBER  *attr;

#if !(defined(_SUPPORT_IPA) || defined(_STANDALONE_INLINER))
   DST_enter_mk(DST_making_dbg_info, last_info_idx);
#endif

   info_idx = DST_mk_info();
   attr_idx = DST_mk_attr(DST_MEMBER);
   attr = DST_ATTR_IDX_TO_PTR(attr_idx, DST_MEMBER);
   DST_MEMBER_decl(attr) = decl;
   DST_MEMBER_name(attr) = DST_mk_name(name);
   DST_MEMBER_type(attr) = type;
   if (is_declaration)
      DST_SET_declaration(flag);
   else {
      DST_MEMBER_memb_loc(attr) = memb_loc;
      DST_MEMBER_byte_size(attr) = byte_size;
      DST_MEMBER_bit_offset(attr) = bit_offset;
      DST_MEMBER_bit_size(attr) = bit_size;
      if (is_bitfield)
     	 DST_SET_bitfield(flag);
   }
   if (is_static)
      DST_SET_static(flag);
   if (is_artificial)
      DST_SET_artificial(flag);
#ifdef KEY
   DST_MEMBER_accessibility(attr) = accessibility;
#endif
   return DST_init_info(info_idx, DW_TAG_member, flag, attr_idx);
}


/* Creates a DW_TAG_inheritance entry.
*/
#ifndef KEY
DST_INFO_IDX
DST_mk_inheritance(USRCPOS      decl,     /* Source location */
		   DST_INFO_IDX type,     /* Type of member */
		   DST_virtuality virtuality, /* AT_virtuality code */
		   DST_size_t   memb_loc) /* Byte-offset of member container */
#else
DST_INFO_IDX
DST_mk_inheritance(USRCPOS      decl,     /* Source location */
		   DST_INFO_IDX type,     /* Type of member */
		   DST_virtuality virtuality, /* AT_virtuality code */
		   DST_size_t   memb_loc, /* Byte-offset of member container */
		   DST_accessibility accessibility) /* AT_accessibility */
#endif
{
   DST_INFO_IDX info_idx;
   DST_ATTR_IDX attr_idx;
   DST_flag     flag = DST_no_flag;
   DST_INHERITANCE  *attr;

#if !(defined(_SUPPORT_IPA) || defined(_STANDALONE_INLINER))
   DST_enter_mk(DST_making_dbg_info, last_info_idx);
#endif

   info_idx = DST_mk_info();
   attr_idx = DST_mk_attr(DST_INHERITANCE);
   attr = DST_ATTR_IDX_TO_PTR(attr_idx, DST_INHERITANCE);
   DST_INHERITANCE_decl(attr) = decl;
   DST_INHERITANCE_type(attr) = type;
   DST_INHERITANCE_virtuality(attr) = virtuality;
   DST_INHERITANCE_memb_loc(attr) = memb_loc;
#ifdef KEY
   DST_INHERITANCE_accessibility(attr) = accessibility;
#endif
   return DST_init_info(info_idx, DW_TAG_inheritance, flag, attr_idx);
}


/* Creates a DW_TAG_template_type_parameter entry.
*/
extern DST_INFO_IDX
DST_mk_template_type_parameter(USRCPOS      decl,     /* Source location */
			       char        *name,     /* Name of formal */
			       DST_INFO_IDX type)     /* Actual type */
{
   DST_INFO_IDX info_idx;
   DST_ATTR_IDX attr_idx;
   DST_flag     flag = DST_no_flag;
   DST_TEMPLATE_TYPE_PARAMETER  *attr;

#if !(defined(_SUPPORT_IPA) || defined(_STANDALONE_INLINER))
   DST_enter_mk(DST_making_dbg_info, last_info_idx);
#endif

   info_idx = DST_mk_info();
   attr_idx = DST_mk_attr(DST_TEMPLATE_TYPE_PARAMETER);
   attr = DST_ATTR_IDX_TO_PTR(attr_idx, DST_TEMPLATE_TYPE_PARAMETER);
   DST_TEMPLATE_TYPE_PARAMETER_decl(attr) = decl;
   DST_TEMPLATE_TYPE_PARAMETER_name(attr) = DST_mk_name(name);
   DST_TEMPLATE_TYPE_PARAMETER_type(attr) = type;
   return DST_init_info(info_idx, DW_TAG_template_type_param,
                        flag, attr_idx);
}


/* Creates a DW_TAG_template_value_parameter entry.
*/
extern DST_INFO_IDX
DST_mk_template_value_parameter(USRCPOS         decl,     /* Source location */
			        char           *name,     /* Name of formal */
			        DST_INFO_IDX    type,     /* Actual type */
			        DST_CONST_VALUE cval)     /* Actual type */
{
   DST_INFO_IDX info_idx;
   DST_ATTR_IDX attr_idx;
   DST_flag     flag = DST_no_flag;
   DST_TEMPLATE_VALUE_PARAMETER  *attr;

#if !(defined(_SUPPORT_IPA) || defined(_STANDALONE_INLINER))
   DST_enter_mk(DST_making_dbg_info, last_info_idx);
#endif

   info_idx = DST_mk_info();
   attr_idx = DST_mk_attr(DST_TEMPLATE_VALUE_PARAMETER);
   attr = DST_ATTR_IDX_TO_PTR(attr_idx, DST_TEMPLATE_VALUE_PARAMETER);
   DST_TEMPLATE_VALUE_PARAMETER_decl(attr) = decl;
   DST_TEMPLATE_VALUE_PARAMETER_name(attr) = DST_mk_name(name);
   DST_TEMPLATE_VALUE_PARAMETER_cval(attr) = cval;
   DST_TEMPLATE_VALUE_PARAMETER_type(attr) = type;
   return DST_init_info(info_idx, DW_TAG_template_value_param,
                        flag, attr_idx);
}



/* Creates a DW_TAG_enumeration_type entry.
*/
DST_INFO_IDX
DST_mk_enumeration_type(USRCPOS      decl,      /* Source location */
			char        *name,      /* Name of type */
			DST_size_t   byte_size, /* Size, if known */
			DST_INFO_IDX abstract_origin, /* In inlined inst. */
			BOOL       is_incomplete)
{
   DST_INFO_IDX          info_idx;
   DST_ATTR_IDX          attr_idx;
   DST_flag              flag = DST_no_flag;
   DST_ENUMERATION_TYPE *attr;

#if !(defined(_SUPPORT_IPA) || defined(_STANDALONE_INLINER))
   DST_enter_mk(DST_making_dbg_info, last_info_idx);
#endif

   info_idx = DST_mk_info();
   attr_idx = DST_mk_attr(DST_ENUMERATION_TYPE);
   attr = DST_ATTR_IDX_TO_PTR(attr_idx, DST_ENUMERATION_TYPE);
   DST_ENUMERATION_TYPE_decl(attr) = decl;
   DST_ENUMERATION_TYPE_name(attr) = DST_mk_name(name);
   DST_ENUMERATION_TYPE_byte_size(attr) = byte_size;
   DST_ENUMERATION_TYPE_abstract_origin(attr) = abstract_origin;
   DST_ENUMERATION_TYPE_first_child(attr) = DST_INVALID_IDX;
   DST_ENUMERATION_TYPE_last_child(attr) = DST_INVALID_IDX;
   if (is_incomplete)
      DST_SET_declaration(flag);
   return DST_init_info(info_idx, DW_TAG_enumeration_type, flag, attr_idx);
}


/* Creates a DW_TAG_enumerator entry.
*/
DST_INFO_IDX 
DST_mk_enumerator(USRCPOS         decl,  /* Source location */
		  char           *name,  /* Name of enumerator */
		  DST_CONST_VALUE cval)  /* Value of enumerator */
{
   DST_INFO_IDX    info_idx;
   DST_ATTR_IDX    attr_idx;
   DST_flag        flag = DST_no_flag;
   DST_ENUMERATOR *attr;

#if !(defined(_SUPPORT_IPA) || defined(_STANDALONE_INLINER))
   DST_enter_mk(DST_making_dbg_info, last_info_idx);
#endif

   info_idx = DST_mk_info();
   attr_idx = DST_mk_attr(DST_ENUMERATOR);
   attr = DST_ATTR_IDX_TO_PTR(attr_idx, DST_ENUMERATOR);
   DST_ENUMERATOR_decl(attr) = decl;
   DST_ENUMERATOR_name(attr) = DST_mk_name(name);
   DST_ENUMERATOR_cval(attr) = cval;
   return DST_init_info(info_idx, DW_TAG_enumerator, flag, attr_idx);
}


/* Creates a DW_TAG_subroutine_type entry.
*/
DST_INFO_IDX
DST_mk_subroutine_type(USRCPOS      decl,            /* Source location */
		       char        *name,            /* Name */
		       DST_INFO_IDX type,            /* Return type */
		       DST_INFO_IDX abstract_origin, /* In inlined inst. */
		       BOOL         is_prototyped)
{
   DST_INFO_IDX         info_idx;
   DST_ATTR_IDX         attr_idx;
   DST_flag             flag = DST_no_flag;
   DST_SUBROUTINE_TYPE *attr;

#if !(defined(_SUPPORT_IPA) || defined(_STANDALONE_INLINER))
   DST_enter_mk(DST_making_dbg_info, last_info_idx);
#endif

   info_idx = DST_mk_info();
   attr_idx = DST_mk_attr(DST_SUBROUTINE_TYPE);
   attr = DST_ATTR_IDX_TO_PTR(attr_idx, DST_SUBROUTINE_TYPE);
   DST_SUBROUTINE_TYPE_decl(attr) = decl;
   DST_SUBROUTINE_TYPE_name(attr) = DST_mk_name(name);
   DST_SUBROUTINE_TYPE_type(attr) = type;
   DST_SUBROUTINE_TYPE_abstract_origin(attr) = abstract_origin;
   DST_SUBROUTINE_TYPE_first_child(attr) = DST_INVALID_IDX;
   DST_SUBROUTINE_TYPE_last_child(attr) = DST_INVALID_IDX;
   if (is_prototyped)
      DST_SET_prototyped(flag);
   return DST_init_info(info_idx, DW_TAG_subroutine_type, flag, attr_idx);
}

#ifdef KEY
/* Create a DW_TAG_namelist entry
 */
DST_INFO_IDX
DST_mk_namelist(USRCPOS decl, /* source location */
		char *name)
{
  DST_INFO_IDX         info_idx;
  DST_ATTR_IDX         attr_idx;
  DST_flag             flag = DST_no_flag;
  DST_NAMELIST *attr;
  
#if !(defined(_SUPPORT_IPA) || defined(_STANDALONE_INLINER))
  DST_enter_mk(DST_making_dbg_info, last_info_idx);
#endif
  
  info_idx = DST_mk_info();
  attr_idx = DST_mk_attr(DST_NAMELIST);
  attr = DST_ATTR_IDX_TO_PTR(attr_idx, DST_NAMELIST);
  DST_NAMELIST_decl(attr) = decl;
  DST_NAMELIST_name(attr) = DST_mk_name(name);
  DST_NAMELIST_first_child(attr) = DST_INVALID_IDX;
  DST_NAMELIST_last_child(attr) = DST_INVALID_IDX;
  return DST_init_info(info_idx, DW_TAG_namelist, flag, attr_idx);
}

/* Create a DW_TAG_namelist_item entry
 */
DST_INFO_IDX
DST_mk_namelist_item(USRCPOS decl, /* source location */
		     char *name)
{
  DST_INFO_IDX         info_idx;
  DST_ATTR_IDX         attr_idx;
  DST_flag             flag = DST_no_flag;
  DST_NAMELIST_ITEM *attr;
  
#if !(defined(_SUPPORT_IPA) || defined(_STANDALONE_INLINER))
  DST_enter_mk(DST_making_dbg_info, last_info_idx);
#endif
  
  info_idx = DST_mk_info();
  attr_idx = DST_mk_attr(DST_NAMELIST_ITEM);
  attr = DST_ATTR_IDX_TO_PTR(attr_idx, DST_NAMELIST_ITEM);
  DST_NAMELIST_ITEM_decl(attr) = decl;
  DST_NAMELIST_ITEM_name(attr) = DST_mk_name(name);
  return DST_init_info(info_idx, DW_TAG_namelist_item, flag, attr_idx);
}

/* Define/Undef a macro. 
 */
DST_MACR_IDX 
DST_mk_macr (UINT lineno, /* line number of macro */
	     char *macro, /* The macro */
	     INT  tag     /* DW_MACINFO_* */)
{
  DST_MACR *m_ptr, *prev_ptr;
  DST_MACR_IDX m_idx;

  /* Reset pstate and the memory block used for allocation */
  if (DST_IS_NULL(last_macro))
    {
      DST_begin_block(DST_macro_info_block); /* First file entry */
      pstate = DST_making_macinfo;
    }
#if !(defined(_SUPPORT_IPA) || defined(_STANDALONE_INLINER))
  else
    DST_enter_mk(DST_making_macinfo, last_macro);
#endif
  
  /* Create the entry */
  m_idx  = DST_mk_macro();
  m_ptr  = DST_MACR_IDX_TO_PTR(m_idx);
  DST_MACR_macro(m_ptr) = DST_mk_string(macro);
  DST_MACR_lineno(m_ptr) = lineno;
  DST_MACR_tag(m_ptr) = tag;
  DST_MACR_next(m_ptr) = DST_INVALID_IDX;
  if (!DST_IS_NULL(last_macro))
    {
      prev_ptr = DST_MACR_IDX_TO_PTR(last_macro);
      DST_MACR_next(prev_ptr) = m_idx;
    }
  last_macro = m_idx;
  num_macros += 1;
  return m_idx;
}

/* Start a file for macinfo section.
 */
DST_MACR_IDX 
DST_mk_macr_start_file (UINT lineno, /* a line number */
			UINT fileno  /* a file number */)
{
  DST_MACR *m_ptr, *prev_ptr;
  DST_MACR_IDX m_idx;

  /* Reset pstate and the memory block used for allocation */
  if (DST_IS_NULL(last_macro))
    {
      DST_begin_block(DST_macro_info_block); /* First file entry */
      pstate = DST_making_macinfo;
    }
#if !(defined(_SUPPORT_IPA) || defined(_STANDALONE_INLINER))
  else
    DST_enter_mk(DST_making_macinfo, last_macro);
#endif
  
  /* Create the entry */
  m_idx  = DST_mk_macro();
  m_ptr  = DST_MACR_IDX_TO_PTR(m_idx);
  DST_MACR_lineno(m_ptr) = lineno;
  DST_MACR_fileno(m_ptr) = fileno;
  DST_MACR_tag(m_ptr) = DW_MACINFO_start_file;
  DST_MACR_next(m_ptr) = DST_INVALID_IDX;
  if (!DST_IS_NULL(last_macro))
    {
      prev_ptr = DST_MACR_IDX_TO_PTR(last_macro);
      DST_MACR_next(prev_ptr) = m_idx;
    }
  last_macro = m_idx;
  num_macros += 1;
  return m_idx;
}

/* End a file for the macinfo section.
 */
DST_MACR_IDX 
DST_mk_macr_end_file (void)
{
  DST_MACR *m_ptr, *prev_ptr;
  DST_MACR_IDX m_idx;

  /* Reset pstate and the memory block used for allocation */
  if (DST_IS_NULL(last_macro))
    {
      DST_begin_block(DST_macro_info_block); /* First file entry */
      pstate = DST_making_macinfo;
    }
#if !(defined(_SUPPORT_IPA) || defined(_STANDALONE_INLINER))
  else
    DST_enter_mk(DST_making_macinfo, last_macro);
#endif
  
  /* Create the entry */
  m_idx  = DST_mk_macro();
  m_ptr  = DST_MACR_IDX_TO_PTR(m_idx);
  DST_MACR_tag(m_ptr) = DW_MACINFO_end_file;
  DST_MACR_next(m_ptr) = DST_INVALID_IDX;
  if (!DST_IS_NULL(last_macro))
    {
      prev_ptr = DST_MACR_IDX_TO_PTR(last_macro);
      DST_MACR_next(prev_ptr) = m_idx;
    }
  last_macro = m_idx;
  num_macros += 1;
  return m_idx;
}
#endif
