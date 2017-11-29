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


/* USMID @(#) libu/ffio/user_parse.h	92.0	10/08/98 14:57:41 */

/*  USER LAYER ASSIGN PARSING DEFINITION */

#define NUM_USER_OPTS     0
#define NUM_USER_ALIAS    0

struct LAYER_NUMERICS _user_numerics[]  = {
   0, 0, 'n' , 0 ,  0, 0 ,  "num1",     /*  numeric 1 */   
   0, 0, 'n' , 0 ,  0, 0 ,  "num2",     /*  numeric 2 */
   0, 0, 'n' , 0 ,  0, 0 ,  "num3",     /*  numeric 3 */
   0, 0, 'n' , 0 ,  0, 0 ,  "num4",     /*  numeric 4 */
   0, 0, 'n' , 0 ,  0, 0 ,  "num5",     /*  numeric 5 */
   0, 0, 'n' , 0 ,  0, 0 ,  "num6",     /*  numeric 6 */
   0, 0, 'n' , 0 ,  0, 0 ,  "num7",     /*  numeric 7 */
   0, 0, 'n' , 0 ,  0, 0 ,  "num8",     /*  numeric 8 */
   0, 0, 'n' , 0 ,  0, 0 ,  "num9",     /*  numeric 9 */
   0, 0, 'n' , 0 ,  0, 0 ,  "num10",     /*  numeric 10 */
   0, 0, 'n' , 0 ,  0, 0 ,  "num11",     /*  numeric 11 */
   0, 0, 'n' , 0 ,  0, 0 ,  "num12",     /*  numeric 12 */
   0, 0, 'n' , 0 ,  0, 0 ,  "num13",     /*  numeric 13 */
   0, 0, 'n' , 0 ,  0, 0 ,  "num14",     /*  numeric 14 */
   0, 0, 'n' , 0 ,  0, 0 ,  "num15",     /*  numeric 15 */
   0, 0, 'n' , 0 ,  0, 0 ,  "num16",     /*  numeric 16 */
                     } ;

#define NUM_USER_NUMERICS (sizeof(_user_numerics)/sizeof(struct LAYER_NUMERICS))

/* Applications makes use of these uninitialized USRx structures */
struct LAYER_DATA _USR0_parse;
struct LAYER_DATA _USR1_parse;
struct LAYER_DATA _USR2_parse;
struct LAYER_DATA _USR3_parse;
struct LAYER_DATA _USR4_parse;
struct LAYER_DATA _USR5_parse;
struct LAYER_DATA _USR6_parse;
struct LAYER_DATA _USR7_parse;
struct LAYER_DATA _USR8_parse;
struct LAYER_DATA _USR9_parse;

struct LAYER_DATA _user_parse =
    {
         CLASS_USER ,
         FLD_EXT_TYPE,
         "user" ,
         "" ,
         0,
         0,
         NUM_USER_OPTS,
         1 ,
         NUM_USER_NUMERICS,
         NUM_USER_ALIAS ,
         NULL ,
         _user_numerics ,
         NULL,
         NULL
    } ;
struct LAYER_DATA _site_data =
    {
         CLASS_SITE ,
         FLD_EXT_TYPE,
         "site" ,
         "" ,
         0,
         0,
         NUM_USER_OPTS,
         1 ,
         NUM_USER_NUMERICS,
         NUM_USER_ALIAS ,
         NULL ,
         _user_numerics ,
         NULL,
         NULL
    } ;

struct LAYER_DATA _usr0_parse =
    {
         CLASS_USER0 ,
         FLD_EXT_TYPE,
         "user0" ,
         "" ,
         0,
         0,
         NUM_USER_OPTS,
         1 ,
         NUM_USER_NUMERICS,
         NUM_USER_ALIAS ,
         NULL ,
         _user_numerics ,
         NULL,
         NULL
    } ;
struct LAYER_DATA _usr1_parse =
    {
         CLASS_USER1 ,
         FLD_EXT_TYPE,
         "user1" ,
         "" ,
         0,
         0,
         NUM_USER_OPTS,
         1 ,
         NUM_USER_NUMERICS,
         NUM_USER_ALIAS ,
         NULL ,
         _user_numerics ,
         NULL,
         NULL
    } ;
struct LAYER_DATA _usr2_parse =
    {
         CLASS_USER2 ,
         FLD_EXT_TYPE,
         "user2" ,
         "" ,
         0,
         0,
         NUM_USER_OPTS,
         1 ,
         NUM_USER_NUMERICS,
         NUM_USER_ALIAS ,
         NULL ,
         _user_numerics ,
         NULL,
         NULL
    } ;
struct LAYER_DATA _usr3_parse =
    {
         CLASS_USER3 ,
         FLD_EXT_TYPE,
         "user3" ,
         "" ,
         0,
         0,
         NUM_USER_OPTS,
         1 ,
         NUM_USER_NUMERICS,
         NUM_USER_ALIAS ,
         NULL ,
         _user_numerics ,
         NULL,
         NULL
    } ;
struct LAYER_DATA _usr4_parse =
    {
         CLASS_USER4 ,
         FLD_EXT_TYPE,
         "user4" ,
         "" ,
         0,
         0,
         NUM_USER_OPTS,
         1 ,
         NUM_USER_NUMERICS,
         NUM_USER_ALIAS ,
         NULL ,
         _user_numerics ,
         NULL,
         NULL
    } ;
struct LAYER_DATA _usr5_parse =
    {
         CLASS_USER5 ,
         FLD_EXT_TYPE,
         "user5" ,
         "" ,
         0,
         0,
         NUM_USER_OPTS,
         1 ,
         NUM_USER_NUMERICS,
         NUM_USER_ALIAS ,
         NULL ,
         _user_numerics ,
         NULL,
         NULL
    } ;
struct LAYER_DATA _usr6_parse =
    {
         CLASS_USER6 ,
         FLD_EXT_TYPE,
         "user6" ,
         "" ,
         0,
         0,
         NUM_USER_OPTS,
         1 ,
         NUM_USER_NUMERICS,
         NUM_USER_ALIAS ,
         NULL ,
         _user_numerics ,
         NULL,
         NULL
    } ;
struct LAYER_DATA _usr7_parse =
    {
         CLASS_USER7 ,
         FLD_EXT_TYPE,
         "user7" ,
         "" ,
         0,
         0,
         NUM_USER_OPTS,
         1 ,
         NUM_USER_NUMERICS,
         NUM_USER_ALIAS ,
         NULL ,
         _user_numerics ,
         NULL,
         NULL
    } ;
struct LAYER_DATA _usr8_parse =
    {
         CLASS_USER8 ,
         FLD_EXT_TYPE,
         "user8" ,
         "" ,
         0,
         0,
         NUM_USER_OPTS,
         1 ,
         NUM_USER_NUMERICS,
         NUM_USER_ALIAS ,
         NULL ,
         _user_numerics ,
         NULL,
         NULL
    } ;
struct LAYER_DATA _usr9_parse =
    {
         CLASS_USER9 ,
         FLD_EXT_TYPE,
         "user9" ,
         "" ,
         0,
         0,
         NUM_USER_OPTS,
         1 ,
         NUM_USER_NUMERICS,
         NUM_USER_ALIAS ,
         NULL ,
         _user_numerics ,
         NULL,
         NULL
    } ;
