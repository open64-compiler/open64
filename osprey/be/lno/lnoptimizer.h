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
 * Module: lnoptimizer.h
 * $Revision$
 * $Date$
 * $Author$
 * $Source$
 *
 * Revision history:
 *  14-SEP-94 - Original Version
 *
 * Description:
 *
 * The external interface for the loop optimizer.
 *
 * ====================================================================
 * ====================================================================
 */

#ifndef lnoptimizer_INCLUDED
#define lnoptimizer_INCLUDED
#ifdef __cplusplus
extern "C" {
#endif

#include "wn.h"
#include "mempool.h"

#ifdef __cplusplus
}
#endif

/* Clients of the loop nest optimizer pass a WHIRL tree for the function, 
 * and receive back a possibly optimized version of the tree.
 */
extern WN *Lnoptimizer(PU_Info* current_pu, WN *func_nd, struct DU_MANAGER *du_mgr,
	struct  ALIAS_MANAGER *alias_mgr);


#endif /* lnoptimizer_INCLUDED */

