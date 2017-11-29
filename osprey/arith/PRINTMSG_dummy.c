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


#include <stdio.h>
#include <stdarg.h>
#include "arith.h"
#include "arith.internal.h"

void PRINTMSG(int pseudo_line_num, int msg_number,
              enum message_types msg_severity, int column_num, ...)
{
   va_list arg_ptr;

   va_start(arg_ptr, column_num);
   if(msg_severity==Internal || msg_severity==Error)
     fprintf(stderr, "ARITH FATAL INTERNAL ERROR #%d:  ", msg_number);
   else
     fprintf(stderr, "ARITH WARNING ERROR #%d:  ", msg_number);
   vfprintf(stderr, "file %s, line#%d\n", arg_ptr);
   va_end(arg_ptr);
   if(msg_severity==Internal || msg_severity==Error) exit(1);
}

static char rcsid [] = "$Id: PRINTMSG_dummy.c,v 1.1.1.1 2005/10/21 19:00:00 marcel Exp $";
