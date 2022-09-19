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
 * $Source: /proj/osprey/CVS/open64/osprey1.0/common/com/config_purple.cxx,v $
 *
 * Revision history:
 *  25-Mar-97 - Original Version
 *
 * Description:
 *
 * Configure the -PURPLE option groups (included in config.c),
 * used by whirl2f.so.
 *
 * ====================================================================
 * ====================================================================
 */

/* This file is included in config.c, so it doesn't need its own set of
 * standard includes -- only the following:
 */
#include "config_purple.h"

PURPLE_FLAGS Purple_Flags = {
    FALSE, /* Enabled? */
    NULL,  /* Original source name */
    NULL,  /* Call-trace outfile */
    NULL,  /* Value-trace outfile */
    NULL,  /* Srcs-fragment outfile */
    NULL,  /* Srcs-included outfile */
    NULL,  /* Call-trace infile */
    FALSE, /* Original in C */
    FALSE, /* Original in Fortran */
    FALSE, /* Show progress */
    FALSE, /* Warn about problems */
    FALSE, /* Keep call-graph file */
    {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, /* buffer[16] */
};                                                    /* Purple_Flags */

PURPLE_FLAGS *Current_PURPLE = &Purple_Flags;

/* ====================================================================
 * Descriptor for the -LNO option group.
 * ====================================================================
 */

#define PRF Purple_Flags
static OPTION_DESC Options_PURPLE[] = {
    {OVK_BOOL, OV_SHY, FALSE, "", NULL, 0, 0, 0, &PRF.enabled, NULL,
     "Enable region instrumentation and code-extraction"},
    {OVK_BOOL, OV_SHY, FALSE, "warn", NULL, 0, 0, 0, &PRF.warn, NULL,
     "Warn about program constructs we cannot handle"},
    {OVK_BOOL, OV_SHY, FALSE, "show", NULL, 0, 0, 0, &PRF.verbose, NULL,
     "Show the progress of this processing step"},
    {OVK_BOOL, OV_SHY, FALSE, "keep", NULL, 0, 0, 0, &PRF.keep, NULL,
     "Keep intermediate files"},
    {OVK_BOOL, OV_SHY, FALSE, "lang_cc", NULL, 0, 0, 0, &PRF.language_c, NULL,
     "The original source was a C program"},
    {OVK_BOOL, OV_SHY, FALSE, "lang_f77", NULL, 0, 0, 0, &PRF.language_ftn,
     NULL, "The original source was a Fortran program"},
    {OVK_NAME, OV_SHY, FALSE, "trace_calls", NULL, 0, 0, 0,
     &PRF.callemit_filename, NULL,
     "Instrument to emit a dynamic call-graph (into the given file)"},
    {OVK_NAME, OV_SHY, FALSE, "trace_values", NULL, 0, 0, 0,
     &PRF.valueemit_filename, NULL,
     "Instrument to emit a value trace (into the given file)"},
    {OVK_NAME, OV_SHY, FALSE, "emit_srcs", NULL, 0, 0, 0, &PRF.srcemit_filename,
     NULL, "Emit temporary sources into the given file"},
    {OVK_NAME, OV_SHY, FALSE, "emit_incl", NULL, 0, 0, 0,
     &PRF.inclemit_filename, NULL,
     "Emit include-file for C into the given file"},
    {OVK_NAME, OV_SHY, FALSE, "call_trace", NULL, 0, 0, 0,
     &PRF.calltrace_filename, NULL, "Read call-trace from the given file"},
    {OVK_NAME, OV_SHY, FALSE, "orign_src", NULL, 0, 0, 0,
     &PRF.orignsrc_filename, NULL, "Original source file"},
    {OVK_COUNT} /* List terminator -- must be last */
};              /* Options_PURPLE */
#undef PRF
