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


/* -*-Mode: c++;-*- (Tell emacs to use c++ mode) */
// ====================================================================
// ====================================================================
//
// Module: opt_ipaa_io.h
// $Revision: 1.2 $
// $Date: 02/11/07 23:41:38-00:00 $
// $Author: fchow@keyresearch.com $
// $Source: /scratch/mee/2.4-65/kpro64-pending/be/com/SCCS/s.opt_ipaa_io.h $
//
// Revision history:
//  22-Nov-95 - Original Version
//
// Description:
//
// See also opt_ipaa_summary.(h|cxx) for general definitions of the
// IPAA/WOPT interface data structures.  This file contains just those
// I/O routines which must reside in be.so because they are called
// before wopt.so is loaded.
//
// ====================================================================
// ====================================================================

#ifndef cxx_opt_ipaa_io_INCLUDED
#define cxx_opt_ipaa_io_INCLUDED

#ifdef _KEEP_RCS_ID
static char *opt_ipaa_io_rcs_id = "$Source: /scratch/mee/2.4-65/kpro64-pending/be/com/SCCS/s.opt_ipaa_io.h $ $Revision: 1.2 $";
#endif /* _KEEP_RCS_ID */

extern "C" {
  // Read table from .B file
  void *IPAA_LOCAL_MAP_Read ( char *base, UINT32 size );

  // Write the table to the .B file
  void IPAA_LOCAL_MAP_Write ( void *localmap, struct output_file *fl );
}

// Global pointer to local-map for use in the back-end --
// defined in common/com/ir_bcom.c.
extern "C" void *IPAA_Local_Map;

#endif
