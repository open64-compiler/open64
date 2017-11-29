
/*

  Copyright (C) 1999,2000,2001, Silicon Graphics, Inc.  All Rights Reserved.

  This program is free software; you can redistribute it and/or modify it
  under the terms of version 2.1 of the GNU Lesser General Public License
  as published by the Free Software Foundation.

  This program is distributed in the hope that it would be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  Further, any
  license provided herein, whether implied or otherwise, is limited to 
  this program in accordance with the express provisions of the 
  GNU Lesser General Public License.  

  Patent licenses, if any, provided herein do not apply to combinations 
  of this program with other product or programs, or any other product 
  whatsoever.  This program is distributed without any warranty that the 
  program is delivered free of the rightful claim of any third person by 
  way of infringement or the like.  

  See the GNU Lesser General Public License for more details.

  You should have received a copy of the GNU General Public License along
  with this program; if not, write the Free Software Foundation, Inc., 59
  Temple Place - Suite 330, Boston MA 02111-1307, USA.

*/

/* $Header: /proj/osprey/CVS/open64/osprey1.0/libU77/charutil.c,v 1.1.1.1 2005/10/21 19:00:00 marcel Exp $ */
/*	3.0 SID #	1.2	*/
#include <stdlib.h>
#include <string.h>

int
g_char (char *a, int alen, char *b)
{
   char           *x, *y;

   if (alen == 0)
      alen = (int) strlen (a);
#ifdef sgi_test
   /* This doesn't work if a string have tab character, e.g., before
    * blanks and then filename.  Using isspace() requires importing
    * it for the shared version.  Too late to make such a risky
    * change this late in the cypress release .  Calvin 8/8/91 */
   x = a + alen;
   for (; a < x && *a == ' '; a++);
   for (; a < x && *a != ' ';)
      *b++ = *a++;
   *b = '\0';
#else
   x = a + alen - 1;
   y = b + alen - 1;
   *(y + 1) = 0;
   for (; x >= a && *x == ' '; x--)
      *y-- = 0;
   for (; x >= a; *y-- = *x--);
#endif
   return (0);
}

int
b_char (char *a, char *b, int blen)
{
   int             i;

   for (i = 0; i < blen && *a != 0; i++)
      *b++ = *a++;
   for (; i < blen; i++)
      *b++ = ' ';
   return(0);
}


int
up_low (char c)
{
   if (c >= 'A' && c <= 'Z')
      return (c - 'A' + 'a');
   return ((int)c);
}

