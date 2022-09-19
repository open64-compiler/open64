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
 * $Source: /proj/osprey/CVS/open64/osprey1.0/common/com/config_flist.cxx,v $
 *
 * Revision history:
 *  25-Mar-97 - Original Version
 *
 * Description:
 *
 * Configure the -FLIST option groups (included in config.c),
 * used by whirl2f.so.
 *
 * ====================================================================
 * ====================================================================
 */

/* This file is included in config.c, so it doesn't need its own set of
 * standard includes -- only the following:
 */
#include "config_flist.h"
 

FLIST_FLAGS Flist_Flags =
{
   NULL,  /* orig_filename */
   NULL,  /* ftn_filename */
   NULL,  /* loc_filename */
   FALSE, /* enabled */
   TRUE,  /* verbose */
   FALSE, /* old_f77 */
   FALSE, /* ansi_format */
   FALSE, /* no_pragmas */
   FALSE, /* emit_prefetch */
   FALSE, /* emit_all_regions */
   FALSE, /* emit_linedirs */
   FALSE, /* emit_nested_pus */
   FALSE, /* emit_frequency */
   FALSE, /* emit_cgtag */
   FALSE, /* emit_pcf  */
   TRUE,  /* emit_omp  */
   0,     /* line_length */
   {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}, /* buffer[15] */
}; /* Flist_Flags */


FLIST_FLAGS *Current_FLIST = &Flist_Flags;


/* ====================================================================
 * Descriptor for the -LNO option group.
 * ====================================================================
 */

#define FLF Flist_Flags
static OPTION_DESC Options_FLIST[] =
{
  { OVK_BOOL,	OV_VISIBLE,	FALSE, "",               NULL,
       0, 0, 0, &FLF.enabled,          NULL,
       "Enable listing of transformed source" },
  { OVK_BOOL,	OV_VISIBLE,	FALSE, "show",           NULL,
       0, 0, 0, &FLF.verbose,          NULL,
       "Show progress of translation to transformed source" },
  { OVK_BOOL,	OV_SHY,		FALSE, "old_f77",        NULL,
       0, 0, 0, &FLF.old_f77,          NULL,
       "Emit code for older versions of f77" },
  { OVK_BOOL,	OV_VISIBLE,	FALSE, "ansi_format",    NULL,
       0, 0, 0, &FLF.ansi_format,      NULL,
       "Generate code with strict F77 formatting rules" },
  { OVK_BOOL,	OV_SHY,	        FALSE, "emit_frequency",  NULL,
       0, 0, 0, &FLF.emit_frequency,   NULL,
       "Emit feedback frequency information for statements" },
  { OVK_BOOL,	OV_SHY,	        FALSE, "no_pragmas",     NULL,
       0, 0, 0, &FLF.no_pragmas,       NULL,
       "Do not emit directives in transformed source" },
  { OVK_BOOL,	OV_VISIBLE,	FALSE, "emit_pfetch",    NULL,
       0, 0, 0, &FLF.emit_prefetch,    NULL,
       "Emit prefetch information in comments" },
  { OVK_BOOL,	OV_SHY,    	FALSE, "emit_regions",   NULL,
       0, 0, 0, &FLF.emit_all_regions, NULL,
       "Emit regions, even compiler-generated ones" },
  { OVK_BOOL,	OV_SHY,	        FALSE, "emit_linedirs",   NULL,
       0, 0, 0, &FLF.emit_linedirs,    NULL,
       "Map back to original source with line-directives" },
  { OVK_BOOL,	OV_SHY,	        FALSE, "emit_nested_pu", NULL,
       0, 0, 0, &FLF.emit_nested_pus,  NULL,
       "Emit nested PUs at file-level, i.e. out of context" },
  { OVK_BOOL,	OV_SHY,	        FALSE, "emit_cgtag", NULL,
       0, 0, 0, &FLF.emit_cgtag,  NULL,
       "Emit tags for do-loops for correlation with CG output" },
  { OVK_BOOL,	OV_VISIBLE,	FALSE, "emit_pcf",       NULL,
       0, 0, 0, &FLF.emit_pcf,   NULL,
       "Use PCF spellings for pragmas" },
  { OVK_BOOL,	OV_VISIBLE,	FALSE, "emit_omp",       NULL,
       0, 0, 0, &FLF.emit_omp,   NULL,
       "Use PCF spellings for pragmas" },
  { OVK_UINT32,	OV_VISIBLE,	FALSE, "linelength",     NULL,
       0, 0, 1024, &FLF.line_length,   NULL,
       "Set maximum line-length for transformed sources" },
  { OVK_NAME,	OV_SHY,	        FALSE, "src_file",       NULL,
       0, 0, 0, &FLF.orig_filename, NULL,
       "The name of the original source file" },
  { OVK_NAME,	OV_VISIBLE,	FALSE, "ftn_file",       NULL,
       0, 0, 0, &FLF.ftn_filename, NULL,
       "The name of the generated source file" },
  { OVK_NAME,	OV_SHY,	        FALSE, "loc_file",       NULL,
       0, 0, 0, &FLF.loc_filename, NULL,
       "The name of a source location map-table file"},
  { OVK_COUNT }			    /* List terminator -- must be last */
}; /* Options_FLIST */
#undef FLF

