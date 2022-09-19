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
 * Module: config_flist.h
 * $Revision: 1.1.1.1 $
 * $Date: 2005/10/21 19:00:00 $
 * $Author: marcel $
 * $Source: /proj/osprey/CVS/open64/osprey1.0/common/com/config_flist.h,v $
 *
 * Revision history:
 *  25-Mar-97 - Original Version
 *
 * Description:
 *
 * Define the external interface to the internal flags representing the
 * -FLIST group options.  It is a single struct, so that addition of
 * flags for new options does not require additions to the be Exported
 * file.  Since these options do not change on a per region basis,
 * there is no need for push/pop operations like other groups (such
 * as -LNO) support.
 *
 * NOTE:  Only the standard group option reader, and routines in the
 * associated file config_flist.c, should modify the structs declared
 * here.  By following this discipline, leaving a few undefined flags
 * at the end of the struct, and adding new flags there, we can avoid 
 * serious version incompatibilities between be.so and its clients.
 *
 * ====================================================================
 *
 * To add a new option:
 *
 * (On conversion from the old FLIST implementation, I tried to use
 * naming which was mostly like what had been used before, but
 * consistent.  The instructions below reflect the results.)
 *
 *   1) In the FLIST_FLAGS options struct defined below, add a field to
 *	receive the new option value.  If you need a flag indicating
 *	whether the option was set explicitly on the command line, add
 *	a BOOL for that as well, with an appended "_set" in its name.
 *	(You might also need another field if the option will be used
 *	in a different form after configuration, i.e. the option value
 *	is a string that is converted to a number.  If so, add another
 *	field.)
 *
 *	The fields are starting out in alphabetical order by option
 *	name.  When adding new ones, keep in mind that adding them in
 *	the middle will create a required correspondence between the
 *	new be.so and lno.so (for purposes of using the later options).
 *	That may be alright, but if you want to avoid it, add the new
 *	fields just before the buffer at the end (and you can move
 *	them into place later when it doesn't matter, if you care).
 *
 *   2) Below the FLIST_FLAGS definition are #defines for the
 *	"FLIST_Option_Name" pseudo-variables that everyone will use to
 *	reference them.  Add #defines for your new ones.  Note that
 *	they all have FLIST_ prefixes.
 *
 *   3) There is only one instances of FLIST_FLAGS in config_flist.c.
 *	We do not support pushing/popping of flag values based on
 *      region boundaries (will we ever want to?).
 *
 *   4) The option group descriptor is also in config_flist.c.  Add your
 *	new option there.
 *
 * ====================================================================
 * ====================================================================
 */

#ifndef config_flist_INCLUDED
#define config_flist_INCLUDED

#ifdef _KEEP_RCS_ID
/*REFERENCED*/
static char *config_flist_h_rcs_id = "$Source: /proj/osprey/CVS/open64/osprey1.0/common/com/config_flist.h,v $ $Revision: 1.1.1.1 $";
#endif /* _KEEP_RCS_ID */

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

/* ====================================================================
 *
 * -FLIST: option group
 *
 * Define the global structure containing -FLIST option group flags.
 *
 * WARNING:  Most of the fields in this struct must be addressable by
 * an option group descriptor -- hence BOOL instead of mBOOL.
 *
 * ====================================================================
 */


typedef struct flist_flags {
   const char *orig_filename;  /* Fortran input (original source) file */
   const char *ftn_filename;   /* Fortran output (transformed) file */
   const char *loc_filename;   /* source-to-source location mapping file */
   BOOL        enabled;         /* Invoke whirl2f.so */
   BOOL        verbose;         /* Show translation information */
   BOOL        old_f77;         /* Use macros for new intrinsics */
   BOOL        ansi_format;     /* Line-formatting to f77 standard */
   BOOL        no_pragmas;      /* By default, emit pragmas */
   BOOL        emit_prefetch;   /* Emit comments for prefetches */
   BOOL        emit_all_regions;/* Emit cmplr-generated regions */
   BOOL        emit_linedirs;   /* Emit preproc line-directives */
   BOOL        emit_nested_pus; /* Emit code for nested PUs */
   BOOL        emit_frequency;  /* Emit feedback frequency info */
   BOOL        emit_cgtag;      /* Tag loops with loop_info address */
   BOOL        emit_pcf;      /* force PCF spellings for pragmas */
   BOOL        emit_omp;      /* force OMP spellings for pragmas */
   INT32       line_length;     /* 'zero' means: use the default */

  /* This buffer area allows references to new fields to be added in
   * later revisions, from other DSOs, without requiring a new be.so
   * or running the risk of referencing illegal data.  Assuming that
   * the buffer is initialized to zeroes, any such references will
   * simply pick up FALSE values (for the Booleans):
   */
  INT32 buffer[15];	/* Buffer space -- initialize to FALSE */
} FLIST_FLAGS;


/* ====================================================================
 *
 * -FLIST: option group
 *
 * Global data "objects" and manipulation functions.
 *
 * ====================================================================
 */

/* This is always the current set of option values: */
extern FLIST_FLAGS *Current_FLIST;

/* Define pseudo-variables for general usage: */
#define FLIST_orig_filename     Current_FLIST->orig_filename
#define FLIST_ftn_filename      Current_FLIST->ftn_filename
#define FLIST_loc_filename      Current_FLIST->loc_filename
#define FLIST_enabled           Current_FLIST->enabled
#define FLIST_verbose           Current_FLIST->verbose
#define FLIST_old_f77           Current_FLIST->old_f77
#define FLIST_ansi_format       Current_FLIST->ansi_format
#define FLIST_no_pragmas        Current_FLIST->no_pragmas
#define FLIST_emit_prefetch     Current_FLIST->emit_prefetch
#define FLIST_emit_all_regions  Current_FLIST->emit_all_regions
#define FLIST_emit_linedirs     Current_FLIST->emit_linedirs
#define FLIST_emit_nested_pus   Current_FLIST->emit_nested_pus
#define FLIST_emit_frequency    Current_FLIST->emit_frequency
#define FLIST_emit_cgtag        Current_FLIST->emit_cgtag
#define FLIST_emit_pcf          Current_FLIST->emit_pcf
#define FLIST_emit_omp          Current_FLIST->emit_omp
#define FLIST_line_length       Current_FLIST->line_length


#ifdef __cplusplus
}
#endif /* __cplusplus */
    
#endif /* config_flist_INCLUDED */
