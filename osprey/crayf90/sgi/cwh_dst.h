/*
 * Copyright 2005, 2006 PathScale, Inc.  All Rights Reserved.
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


/* ====================================================================
 * ====================================================================
 *
 * Module: cwh_dst.h
 * $Revision: 1.5 $
 * $Date: 05/08/26 23:50:11-07:00 $
 * $Author: fchow@fluorspar.internal.keyresearch.com $
 * $Source: crayf90/sgi/SCCS/s.cwh_dst.h $
 *
 * Revision history:
 *  dd-mmm-95 - Original Version
 *
 * Description:
 *
 * ====================================================================
 * ====================================================================
 */

#ifndef CWH_DST_INCLUDED
#define CWH_DST_INCLUDED

#ifdef _KEEP_RCS_ID
static char *rcs_id = "$Source: crayf90/sgi/SCCS/s.cwh_dst.h $ $Revision: 1.5 $";
#endif /* _KEEP_RCS_ID */



extern void cwh_dst_init_file(char *src_path);
extern void cwh_dst_include(char *name) ;
extern void cwh_dst_write(void);
extern mUINT16 cwh_dst_enter_path(char * file_name) ;
extern DST_IDX cwh_dst_enter_pu(ST * st);
#ifdef KEY /* Bug 3507 */
extern void cwh_dst_module_vars(ST *en);
#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */
extern void cwh_dst_enter_module(char *module_name, char *file_name,
  INT32 local_lineno);
extern void cwh_dst_exit_module(void);
#ifdef __cplusplus
}
#endif /* __cplusplus */
#endif /* KEY Bug 3507 */
extern char * cwh_dst_filename_from_filenum(INT f);

#endif /* CWH_DST_INCLUDED */

