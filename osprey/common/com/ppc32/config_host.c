/*

  Copyright (C) 2006-2009 Tsinghua University.  All Rights Reserved.
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



static char *source_file = __FILE__;
static char *rcs_id = "$Source: /kpro64/common/com/ppc32/config_host.c,v $ $Revision: 1.1 $";

#include "defs.h"
#include "config.h"

#if defined(HOST_PPC32)
#define HOST_IS_64BIT 0
#endif


/* ====================================================================
 *
 * Preconfigure_Host
 *
 * Configuration of host-specific parameters, prior to flag processing.
 *
 * ====================================================================
 */

void
Preconfigure_Host ( void )
{
#if HOST_IS_LITTLE_ENDIAN
  Host_Byte_Sex = LITTLE_ENDIAN;
#else
  Host_Byte_Sex = BIG_ENDIAN;
#endif
  return;
}


/* ====================================================================
 *
 * Configure_Host
 *
 * Configuration of host-specific parameters, after flag processing.
 *
 * ====================================================================
 */

void
Configure_Host ( void )
{
  return;
}



/* ====================================================================
 *
 * Configure_Source_Host
 *
 * Reconfiguration of host-specific parameters for each source file.
 *
 * ====================================================================
 */

/*ARGSUSED*/
void
Configure_Source_Host ( char *filename )
{
  return;
}
