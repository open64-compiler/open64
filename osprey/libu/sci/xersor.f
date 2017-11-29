C
C
C  Copyright (C) 2000, 2001 Silicon Graphics, Inc.  All Rights Reserved.
C
C  This program is free software; you can redistribute it and/or modify it
C  under the terms of version 2.1 of the GNU Lesser General Public License 
C  as published by the Free Software Foundation.
C
C  This program is distributed in the hope that it would be useful, but
C  WITHOUT ANY WARRANTY; without even the implied warranty of
C  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  
C
C  Further, this software is distributed without any warranty that it is
C  free of the rightful claim of any third person regarding infringement 
C  or the like.  Any license provided herein, whether implied or 
C  otherwise, applies only to this software file.  Patent licenses, if
C  any, provided herein do not apply to combinations of this program with 
C  other software, or any other product whatsoever.  
C
C  You should have received a copy of the GNU Lesser General Public 
C  License along with this program; if not, write the Free Software 
C  Foundation, Inc., 59 Temple Place - Suite 330, Boston MA 02111-1307, 
C  USA.
C
C  Contact information:  Silicon Graphics, Inc., 1600 Amphitheatre Pky,
C  Mountain View, CA 94043, or:
C
C  http://www.sgi.com
C
C  For further information regarding this notice, see:
C
C  http://oss.sgi.com/projects/GenInfo/NoticeExplan
C
C


      subroutine xersor ( srname, info )
      implicit none
*
*     .. Scalar arguments ..
      integer            info
      character*7        srname
*
*
*     Purpose
*     =======
*
*       XERSOR is an error handler for the sorting routines. 
*              It is called by the sorting routines if an input 
*              parameter is invalid.  
*
*
*     Note
*     ====
*       Installers should consider modifying the STOP statement in 
*       order to call system-specific exception-handling facilities.
*
*
*     Parameters
*     ==========
*
*       srname - character*7.
*                On entry, srname specifies the name of the routine 
*                which called XERSOR.
*
*       info   - integer.
*                On entry, info specifies the position of the invalid
*                parameter in the parameter-list of the calling routine.
*
*
*  Auxiliary routine for sorting.
*  Written on 15-March-1991.

CDIR$ ID "@(#) libu/sci/xersor.f	92.0	10/08/98 14:57:41"

*     ... Executable Statements ...
*
        write( *,10 ) srname, info
10      format ( ' ** On entry to ', A7, ' parameter number ', I2,
     &           ' had an illegal value' )

*     End of xersor()
      return
      end
