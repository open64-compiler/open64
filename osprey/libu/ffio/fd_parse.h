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


/* USMID @(#) libu/ffio/fd_parse.h	92.0	10/08/98 14:57:41 */

/*  FD LAYER ASSIGN PARSING DEFINITION */

#define NUM_FD_ALIAS    0
#define NUM_FD_OPTS     0


struct LAYER_NUMERICS _fd_numerics[]  = {
   { 1, 0, 'n' , 0 , 0xfffff , 0,  "file_descriptor" } , /*  number of blocks for buffer */
                     } ;
#define NUM_FD_NUMERICS (sizeof(_fd_numerics)/sizeof(struct LAYER_NUMERICS))

struct LAYER_DATA _fd_data =
    {
         CLASS_FD ,
         FLD_EXT_TYPE,
         "fd",
         "" ,
         0,
         0,
         NUM_FD_OPTS,
         1 ,
         NUM_FD_NUMERICS,
         NUM_FD_ALIAS ,
         NULL,
         _fd_numerics ,
         NULL,
         NULL
    } ;
struct LAYER_DATA _stdin_data =
    {
         CLASS_FD ,
         FLD_EXT_TYPE,
         "stdin",
         ":0" ,
         0,
         0,
         NUM_FD_OPTS,
         1 ,
         1,
         NUM_FD_ALIAS ,
         NULL,
         _fd_numerics ,
         NULL,
         NULL
     };
struct LAYER_DATA _stdout_data =
    {
         CLASS_FD ,
         FLD_EXT_TYPE,
         "stdout",
         ":1" ,
         0,
         0,
         NUM_FD_OPTS,
         1 ,
         1,
         NUM_FD_ALIAS ,
         NULL,
         _fd_numerics ,
         NULL,
         NULL
     };
struct LAYER_DATA _stderr_data =
    {
         CLASS_FD ,
         FLD_EXT_TYPE,
         "stderr",
         ":2" ,
         0,
         0,
         NUM_FD_OPTS,
         1 ,
         1,
         NUM_FD_ALIAS ,
         NULL,
         _fd_numerics ,
         NULL,
         NULL
     };
