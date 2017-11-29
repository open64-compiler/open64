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
 * Module: aux_stab.cxx
 * $Revision: 1.2 $
 * $Date: 02/11/07 23:41:34-00:00 $
 * $Author: fchow@keyresearch.com $
 * $Source: /scratch/mee/2.4-65/kpro64-pending/be/com/SCCS/s.aux_stab.cxx $
 *
 * Description:
 *
 * Auxiliary (i.e. BE-specific) symbol table routines.  In addition
 * to stubs for defining symbols which are defined by front-end
 * aux_stab.c and needed by com/stab.c, this module contains functions
 * for the following purposes:
 *
 *   Symbol table emission to support the debugger.
 *
 *   Stack frame reorganization and temporary allocation.
 *
 *   Listing and tracing of the symbol table contents.
 *
 * WARNING:  The restructuring of and additions to this module for
 * stack frame reorganization and symbol table listing/tracing has
 * ignored considerations of running in SINGLE_PROCESS mode.  Some work
 * will be necessary to resurrect that capability.
 *
 * ====================================================================
 * ====================================================================
 */

#ifndef SINGLE_PROCESS
/* If SINGLE_PROCESS, this file is included in *fe/aux_stab.c,
 * so don't do this.
 */

#include "defs.h"
#include "config_targ.h"
#include "config_asm.h"
#include "erglob.h"
#include "erbe.h"
#include "glob.h"
#include "tracing.h"
#include "mempool.h"

#include "mtypes.h"
#include "strtab.h"
#include "opcode.h"
#include "targ_const.h"
#include "stab.h"
#include "tn.h"
#include "bb.h"
#include "ttype.h"
#include "wn.h"
#include "const.h"
#endif /* SINGLE_PROCESS */

