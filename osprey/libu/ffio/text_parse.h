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


/* USMID @(#) libu/ffio/text_parse.h	92.1	10/07/99 22:14:06 */

/*  TEXT LAYER ASSIGN PARSING DEFINITION */
#include "txtio.h"
#define NUM_TEXT_ALIAS    0

struct LAYER_OPTS _text_opts[] = {
   CLASS_TEXT, _STR1M,  _INFO1_STR1(TEXT_NL)    ,  0, 0, _STR1M, 0, 0,  "nl" ,
   CLASS_TEXT, _STR1M,  _INFO1_STR1(TEXT_NL_WEOF), 0, 0, _STR1M, 0, 0,  "eof" ,
   CLASS_TEXT, _STR1M,  _INFO1_STR1(TEXT_205)   ,  0, 0, _STR1M, 0, 0, "c205",
   CLASS_TEXT, _STR1M,  _INFO1_STR1(TEXT_205)   ,  0, 0, _STR1M, 0, 0, "205",
   CLASS_TEXT, _STR1M,  _INFO1_STR1(TEXT_CTSS)  ,  0, 0, _STR1M, 0, 0, "ctss"
} ;

struct LAYER_NUMERICS _text_numerics[]  = {
#if	defined(__mips) || defined(_LITTLE_ENDIAN)
   1, 1, 'n' , 0 ,     0xff , 0 ,  "newline"     /*  new line character */   ,
   1, 1, 'n' , 0 ,  0xfffff , 0 ,  "bufsize"     /*  buffer size */
#else
   1, 0, 'n' , 0 ,     0xff , 0 ,  "newline"     /*  new line character */   ,
   1, 0, 'n' , 0 ,  0xfffff , 0 ,  "bufsize"     /*  buffer size */
#endif
} ;

#define NUM_TEXT_OPTS (sizeof(_text_opts)/sizeof(struct LAYER_OPTS))
#define NUM_TEXT_NUMERICS (sizeof(_text_numerics)/sizeof(struct LAYER_NUMERICS))

struct LAYER_DATA _text_data =
    {
         CLASS_TEXT ,
         FLD_EXT_TYPE,
         "text" ,
         ".nl" ,
         0,
         0,
         NUM_TEXT_OPTS,
         1 ,
         NUM_TEXT_NUMERICS,
         NUM_TEXT_ALIAS ,
         _text_opts ,
         _text_numerics ,
         NULL,
         NULL
    } ;

