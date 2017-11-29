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


/* USMID @(#) libu/ffio/sys_parse.h	92.0	10/08/98 14:57:41 */

/*  SYSTEM LAYER ASSIGN PARSING DEFINITION */

#ifdef	__mips
struct LAYER_OPTS __sys_opts[] = {
CLASS_SYSTEM, 0, 0, SYS_flags_AIO_CB, 0               , 0, 0, 0, "noaiocb",
CLASS_SYSTEM, 0, 0, SYS_flags_AIO_CB, SYS_flags_AIO_CB, 0, 0, 0, "aiocb"  ,
};
#define NUM_SYSTEM_OPTS     2
#else
#define NUM_SYSTEM_OPTS     0
#endif

#define NUM_SYSTEM_NUMERICS 0
#define NUM_SYSTEM_ALIAS    0

struct LAYER_DATA _system_data =
    {
         CLASS_SYSTEM,
         NO_TYPE,
         "system",
#ifdef	__mips
         "noaiocb",
#else
         "",
#endif
         0,
         0,
         NUM_SYSTEM_OPTS,
         1 ,
         NUM_SYSTEM_NUMERICS,
         NUM_SYSTEM_ALIAS,
#ifdef	__mips
         __sys_opts,
#else
         NULL,
#endif
         NULL,
         NULL,
         NULL
    };
