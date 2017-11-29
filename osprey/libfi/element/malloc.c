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


#pragma ident "@(#) libfi/element/malloc.c	92.2     08/19/99 11:34:13"

#include <fortran.h>
#include <cray/portdefs.h>
#include <malloc.h>

extern _f_int4 _MALLOC_I4_I4(_f_int4);
extern _f_int4 _MALLOC_I4_I8(_f_int8);
extern _f_int8 _MALLOC_I8_I4(_f_int4);
extern _f_int8 _MALLOC_I8_I8(_f_int8);

_f_int4 _MALLOC_I4_I4(_f_int4 size)
{
  void *addr;

  if(size == 0)
    return 0;
  else
    {
      addr = malloc(size);
      return ((_f_int4) addr);
    }
}

_f_int4 _MALLOC_I4_I8(_f_int8 size)
{
  void *addr;

  if(size == 0)
    return 0;
  else
    {
      addr = malloc(size);
      return ((_f_int4) addr);
    }
}

_f_int8 _MALLOC_I8_I4(_f_int4 size)
{
  void *addr;

  if(size == 0)
    return 0;
  else
    {
      addr = malloc(size);
      return ((_f_int8) addr);
    }
}

_f_int8 _MALLOC_I8_I8(_f_int8 size)
{
  void *addr;

  if(size == 0)
    return 0;
  else
    {
      addr = malloc(size);
      return ((_f_int8) addr);
    }
}
