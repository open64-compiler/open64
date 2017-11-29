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
 * Module: cwh_pdgcs.h
 * $Revision$
 * $Date$
 * $Author$
 * $Source$
 *
 * Revision history:
 *  dd-mmm-95 - Original Version
 *
 * Description: Provides prototypes for entry points in cwh_pdgcs.c
 *
 * ====================================================================
 * ====================================================================
 */

#ifndef CWH_PDGCS_INCLUDED
#define CWH_PDGCS_INCLUDED

#ifdef _KEEP_RCS_ID
static char *rcs_id = "$Source$ $Revision$";
#endif /* _KEEP_RCS_ID */


void PDGCS_initialize ( LANG           language,
                        INT32          init_flags,
                        char          *cmp_name,
                        char          *cmp_rel,
                        char          *obj_file_name,
                        char          *list_file_name,
                        INT32          trunc_bits,
                        INT32          debug_opts,
                        char          *src_path_name,
                        char          *cif_file_name,
                        char          *debug_file_name,
                        FILE          *debug_file,
                        FILE          *cif_file,
                        char          *src_fname,
                        char          *cwd,
                        INT32          n_pes);


void PDGCS_terminate ( int *errors );



#endif /* CWH_PDGCS_INCLUDED */






