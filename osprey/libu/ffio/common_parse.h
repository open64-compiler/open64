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


/* USMID @(#) libu/ffio/common_parse.h	92.2	10/29/99 21:40:31 */

/*
 * This file contains some tables that are common and are used
 * by more than one layer.
 */
#ifndef _COMMONPARSE_H
#define _COMMONPARSE_H

#ifdef _UNICOS
#define MAX_NBUF_SIZE 0xfffffffffff
#elif (__mips)
#if _MIPS_SZLONG == 64
#define MAX_NBUF_SIZE 0xfffffffffffL
#else
#define MAX_NBUF_SIZE 0xfffffffffffLL
#endif
#elif defined(_LITTLE_ENDIAN)
#if defined(_LP64)
#define MAX_NBUF_SIZE 0xfffffffffffL
#else
#define MAX_NBUF_SIZE 0xfffffffffffLL
#endif
#else
#error "MAX NOT DEFINED"
#endif

struct LAYER_NUMERICS _nbuf_numerics[]  = {
   { 1, 0, 'b' , 1, MAX_NBUF_SIZE, 0,  "bufsize"   } , /*  number of blocks for
 buffer */
   { 1, 0, 'n' , 1, MAX_NBUF_SIZE, 0,  "num_buffers"   } , /*  number of blocks for
 buffer */
} ;

#define NUM_NBUF_NUMERICS (sizeof(_nbuf_numerics)/sizeof(struct LAYER_NUMERICS))



#ifdef _CRAY
#define MAX_MR_SDS_SIZE 0xfffffffffff
#elif (__mips)
#if _MIPS_SZLONG == 64
#define MAX_MR_SDS_SIZE 0xfffffffffffL
#else
#define MAX_MR_SDS_SIZE 0xfffffffffffLL
#endif
#elif defined(_LITTLE_ENDIAN)
#if defined(_LP64)
#define MAX_MR_SDS_SIZE 0xfffffffffffL
#else
#define MAX_MR_SDS_SIZE 0xfffffffffffLL
#endif
#else
#error "MAX NOT DEFINED"
#endif

struct LAYER_NUMERICS _mr_sds_numerics[]  = {
   { 1, 0, 'b', 0, MAX_MR_SDS_SIZE,  0, "start_size"} , /* initial allocation */
   { 1, 0, 'b', 0, MAX_MR_SDS_SIZE,  0, "max_size"  } , /* maximum allocation */
   { 1, 0, 'b', 0, MAX_MR_SDS_SIZE,  0, "inc_size"  }   /* allocation increments */
                     } ;
#define NUM_MR_SDS_NUMERICS (sizeof(_mr_sds_numerics)/sizeof(struct LAYER_NUMERICS))


struct LAYER_NUMERICS _rec_buf_numerics[]  = {
   1, 0, 'n' , 1 , 0xfffff  , 0 ,  "recsize" ,
   1, 0, 'n' , 1 , 0xfffff  , 0 ,  "mbs"
} ;

#define NUM_REC_BUF_NUMERICS sizeof (_rec_buf_numerics)/sizeof(struct LAYER_NUMERICS)

#endif
