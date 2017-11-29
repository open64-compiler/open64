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


#include "stdio.h"
#include "cmplrs/host.h"
#include "comargs.h"

extern int32 num_father_args, num_son_args;
extern uint32 father_addr[], father_len[];
extern uint32 son_addr[], son_len[];
extern void s_abort(void);
extern int32 set_arg_arr(uint32 **,uint32 *,uint32 *);

void set_args(uint32 *addr_arg1, uint32 *addr_arg2 )
{
    if (addr_arg1)
	num_father_args = set_arg_arr((uint32 **) addr_arg1, father_addr,
				      father_len);
    if (addr_arg2)
	num_son_args = set_arg_arr((uint32 **) addr_arg2,son_addr,son_len);
    comargs__[0] = 0;
}

int32
set_arg_arr(uint32 **iaddr,uint32 *result_addr,uint32 *result_len)
{
    int32 nargs = *(unsigned char *) iaddr;
    int32 i;
    uint32 charmask;
    uint32 *sp, *fp;

    sp = *iaddr;
    /* recreate the first byte of the stack pointer */
    *(char *)&sp = ((unsigned long)&sp) >> 24;
    /* the stack pointer cannot be smaller than the address of local variables
    in this function unless there has been a cross over at the 24-bit boundary 
    E.g: when 'sp' is suposed to be 7f000000 and &sp is at 7effff00 therefore
    recreating the stack pointer value using &sp will result in 7e000000 */
    if (sp < (unsigned *)&sp)
	(*(char *)&sp)++;
    fp = sp;
    for (i = 0; i < nargs; i++)
	*result_addr++ = *sp++;
    if (fp != *((unsigned **)*sp)) {
	printf("Function is not declared as varargs %x %x\n", fp, *((unsigned **)*sp));
	s_abort();
    }
    charmask = *((unsigned*)*(++sp));
    if (charmask!=0)
        for (i=0; i<nargs; i++) {
	    *result_len++ = (charmask&1) ? *++sp : 0;
	    charmask >>= 1;
        }
    else
	for (i=0; i<nargs; i++) *result_len++ = 0;
    return (nargs);
}
