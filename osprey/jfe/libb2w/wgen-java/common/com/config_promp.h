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
 * Module: config_promp.h
 * $Revision: 1.1.1.1 $
 * $Date: 2005/10/21 19:00:00 $
 * $Author: marcel $
 * $Source: /proj/osprey/CVS/open64/osprey1.0/common/com/config_promp.h,v $
 *
 * Revision history:
 *  25-Mar-97 - Original Version
 *
 * Description:
 *
 * Define the external interface to the internal flags representing the
 * -PROMP group options.  It is a single struct, so that addition of
 * flags for new options does not require additions to the be Exported
 * file.  Since these options do not change on a per region basis,
 * there is no need for push/pop operations like other groups (such
 * as -LNO) support.
 *
 * NOTE:  Only the standard group option reader, and routines in the
 * associated file config_promp.c, should modify the structs declared
 * here.  By following this discipline, leaving a few undefined flags
 * at the end of the struct, and adding new flags there, we can avoid 
 * serious version incompatibilities between be.so and its clients.
 *
 * ====================================================================
 *
 * To add a new option:
 *
 * (On conversion from the old PROMP implementation, I tried to use
 * naming which was mostly like what had been used before, but
 * consistent.  The instructions below reflect the results.)
 *
 *   1) In the PROMP_FLAGS options struct defined below, add a field to
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
 *   2) Below the PROMP_FLAGS definition are #defines for the
 *	"PROMP_Option_Name" pseudo-variables that everyone will use to
 *	reference them.  Add #defines for your new ones.  Note that
 *	they all have PROMP_ prefixes.
 *
 *   3) There is only one instances of PROMP_FLAGS in config_promp.c.
 *	We do not support pushing/popping of flag values based on
 *      region boundaries (will we ever want to?).
 *
 *   4) The option group descriptor is also in config_promp.c.  Add your
 *	new option there.
 *
 * ====================================================================
 * ====================================================================
 */

#ifndef config_promp_INCLUDED
#define config_promp_INCLUDED

#ifdef _KEEP_RCS_ID
/*REFERENCED*/
static char *config_promp_h_rcs_id = "$Source$ $Revision$";
#endif /* _KEEP_RCS_ID */

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

/* ====================================================================
 *
 * -PROMP: option group
 *
 * Define the global structure containing -PROMP option group flags.
 *
 * WARNING:  Most of the fields in this struct must be addressable by
 * an option group descriptor -- hence BOOL instead of mBOOL.
 *
 * ====================================================================
 */


typedef struct promp_flags 
{
   BOOL        enabled;             /* Enabled? */
   BOOL        owhile;              /* Emit construct descr for while loops */
   BOOL        show;                /* Show progress of anl file generation */
   const char *anl_filename;        /* Name of output file */
   const char *src_filename;        /* Name of original source file */
   UINT64      next_id;             /* Id numbers start at this number */

  /* This buffer area allows references to new fields to be added in
   * later revisions, from other DSOs, without requiring a new be.so
   * or running the risk of referencing illegal data.  Assuming that
   * the buffer is initialized to zeroes, any such references will
   * simply pick up FALSE values (for the Booleans):
   */
  INT32 buffer[16];	/* Buffer space -- initialize to FALSE */
} PROMP_FLAGS;


/* ====================================================================
 *
 * -PROMP: option group
 *
 * Global data "objects" and manipulation functions.
 *
 * ====================================================================
 */

/* This is always the current set of option values: */
extern PROMP_FLAGS *Current_PROMP;

/* Define pseudo-variables for general usage: */
#define PROMP_enabled             Current_PROMP->enabled
#define PROMP_owhile              Current_PROMP->owhile
#define PROMP_show                Current_PROMP->show
#define PROMP_next_id             Current_PROMP->next_id
#define PROMP_anl_filename        Current_PROMP->anl_filename
#define PROMP_src_filename        Current_PROMP->src_filename


#ifdef __cplusplus
}
#endif /* __cplusplus */
    
#endif /* config_promp_INCLUDED */
