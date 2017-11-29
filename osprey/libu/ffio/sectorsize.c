/*

  Copyright (C) 2000, 2001 Silicon Graphics, Inc.  All Rights Reserved.

  This program is free software; you can redistribute it and/or modify it
  under the terms of version 2.1 of the GNU Lesser General Public License 
  as published by the Free Software Foundation.

  This program is distributed in the hope that it would be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  

  Further, this software is distributed without any warranty that it is
  free of the rightful claim of any third person regarding infringement 
  or the like.  Any license provided herein, whether implied or 
  otherwise, applies only to this software file.  Patent licenses, if
  any, provided herein do not apply to combinations of this program with 
  other software, or any other product whatsoever.  

  You should have received a copy of the GNU Lesser General Public 
  License along with this program; if not, write the Free Software 
  Foundation, Inc., 59 Temple Place - Suite 330, Boston MA 02111-1307, 
  USA.

  Contact information:  Silicon Graphics, Inc., 1600 Amphitheatre Pky,
  Mountain View, CA 94043, or:

  http://www.sgi.com

  For further information regarding this notice, see:

  http://oss.sgi.com/projects/GenInfo/NoticeExplan

*/


#pragma ident "@(#) libu/ffio/sectorsize.c	92.1	06/29/99 13:16:47"


#include <sys/types.h>
#include <sys/stat.h>
#include <string.h>

_get_sector_size(char *name)
{
int status ;
struct stat check_stat ;
int nbytes ;
char local_name[256] ;
char *last_slash ;

   strcpy( local_name, name ) ;
   last_slash = strrchr( local_name, '/' ) ;
   if( last_slash == NULL )
   {
      strcpy(local_name, "." ) ;
   }
   else
   {
      last_slash[0] = '\0' ;
   }

   status = stat( local_name, &check_stat ) ;
   if( status == -1 )
   {
      return(1) ;
   }

   nbytes = check_stat.st_blksize ;
   if( nbytes == 0 ) nbytes = 4096 ;
   return( nbytes/4096) ;
   
}
