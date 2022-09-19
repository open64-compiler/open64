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
 * Module: config_purple.h
 * $Revision: 1.1.1.1 $
 * $Date: 2005/10/21 19:00:00 $
 * $Author: marcel $
 * $Source: /proj/osprey/CVS/open64/osprey1.0/common/com/config_purple.h,v $
 *
 * Revision history:
 *  25-Mar-97 - Original Version
 *
 * Description:
 *
 * Define the external interface to the internal flags representing the
 * -PURPLE group options.  It is a single struct, so that addition of
 * flags for new options does not require additions to the be Exported
 * file.  Since these options do not change on a per region basis,
 * there is no need for push/pop operations like other groups (such
 * as -LNO) support.
 *
 * NOTE:  Only the standard group option reader, and routines in the
 * associated file config_purple.c, should modify the structs declared
 * here.  By following this discipline, leaving a few undefined flags
 * at the end of the struct, and adding new flags there, we can avoid 
 * serious version incompatibilities between be.so and its clients.
 *
 * ====================================================================
 *
 * To add a new option:
 *
 * (On conversion from the old PURPLE implementation, I tried to use
 * naming which was mostly like what had been used before, but
 * consistent.  The instructions below reflect the results.)
 *
 *   1) In the PURPLE_FLAGS options struct defined below, add a field to
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
 *   2) Below the PURPLE_FLAGS definition are #defines for the
 *	"PURPLE_Option_Name" pseudo-variables that everyone will use to
 *	reference them.  Add #defines for your new ones.  Note that
 *	they all have PURPLE_ prefixes.
 *
 *   3) There is only one instances of PURPLE_FLAGS in config_purple.c.
 *	We do not support pushing/popping of flag values based on
 *      region boundaries (will we ever want to?).
 *
 *   4) The option group descriptor is also in config_purple.c.  Add your
 *	new option there.
 *
 * ====================================================================
 * ====================================================================
 */

#ifndef config_purple_INCLUDED
#define config_purple_INCLUDED

#ifdef _KEEP_RCS_ID
/*REFERENCED*/
static char *config_purple_h_rcs_id = "$Source$ $Revision$";
#endif /* _KEEP_RCS_ID */

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

/* ====================================================================
 *
 * -PURPLE: option group
 *
 * Define the global structure containing -PURPLE option group flags.
 *
 * WARNING:  Most of the fields in this struct must be addressable by
 * an option group descriptor -- hence BOOL instead of mBOOL.
 *
 * ====================================================================
 */


typedef struct purple_flags 
{
   BOOL        enabled;             /* Enabled? */
   const char *orignsrc_filename;   /* Call-trace outfile */
   const char *callemit_filename;   /* Call-trace outfile */
   const char *valueemit_filename;  /* Value-trace outfile */
   const char *srcemit_filename;    /* Srcs-fragment outfile */
   const char *inclemit_filename;   /* Srcs-included outfile */
   const char *calltrace_filename;  /* Call-trace infile */
   BOOL        language_c;          /* Original in C */
   BOOL        language_ftn;        /* Original in Fortran */
   BOOL        warn;                /* Warn about constructs not handled */
   BOOL        verbose;             /* Show progress */
   BOOL        keep;                /* Keep call-graph file */

  /* This buffer area allows references to new fields to be added in
   * later revisions, from other DSOs, without requiring a new be.so
   * or running the risk of referencing illegal data.  Assuming that
   * the buffer is initialized to zeroes, any such references will
   * simply pick up FALSE values (for the Booleans):
   */
  INT32 buffer[16];	/* Buffer space -- initialize to FALSE */
} PURPLE_FLAGS;


/* ====================================================================
 *
 * -PURPLE: option group
 *
 * Global data "objects" and manipulation functions.
 *
 * ====================================================================
 */

/* This is always the current set of option values: */
extern PURPLE_FLAGS *Current_PURPLE;

/* Define pseudo-variables for general usage: */
#define PURPLE_enabled             Current_PURPLE->enabled
#define PURPLE_orignsrc_filename   Current_PURPLE->orignsrc_filename
#define PURPLE_callemit_filename   Current_PURPLE->callemit_filename
#define PURPLE_valueemit_filename  Current_PURPLE->valueemit_filename
#define PURPLE_srcemit_filename    Current_PURPLE->srcemit_filename
#define PURPLE_inclemit_filename   Current_PURPLE->inclemit_filename
#define PURPLE_calltrace_filename  Current_PURPLE->calltrace_filename
#define PURPLE_language_c          Current_PURPLE->language_c
#define PURPLE_language_ftn        Current_PURPLE->language_ftn
#define PURPLE_warn                Current_PURPLE->warn
#define PURPLE_verbose             Current_PURPLE->verbose
#define PURPLE_keep                Current_PURPLE->keep


#ifdef __cplusplus
}
#endif /* __cplusplus */
    
#endif /* config_purple_INCLUDED */
