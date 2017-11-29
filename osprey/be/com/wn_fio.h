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


/**
***		Fortran I/O Lowering Support
***		----------------------------
***
*** Description:
***
***	This interface describes all the declarations and data needed to
***     support Fortran I/O lowering.
***
*** Exported functions:
***
***	WN *lower_io_statement   perform I/O lowering
***
**/

#ifndef wnfio_INCLUDED
#define wnfio_INCLUDED "wn_fio.h"

#ifdef __cplusplus
extern "C" {
#endif

extern INT32 mp_io;

typedef struct dope_header {
    unsigned int        assoc     :1;   /* associated flag */
    unsigned int        ptr_alloc :1;   /* set if allocated by pointer */
    unsigned int        p_or_a    :2;   /* pointer or allocatable array. Use */
                                        /* enum ptrarray values.  */
    unsigned int        a_contig  :1;   /* array storage contiguous flag */
/*
 * PROPOSED CHANGE:
 *
 * Add version and coarray n_dim fields.  Update the n_dim to all
 * up to 255 dimensions although only 20 may be the actual MAXDIM.
 * The f95 standard is still only 7.
 *
 *  unsigned long       dv_versn  :6;
 *  unsigned long                 :21;
 *  unsigned long                 :8;
 *  unsigned long       n_codim   :8;
 *  unsigned long                 :8;
 *  unsigned long       n_dim     :8;
 */
    unsigned int                  :27;  /* pad for first 32 bits        */
    unsigned int                  :29;  /* pad for second 32-bits       */
    unsigned int        n_dim     :3;   /* number of dimensions */

} dope_header_type;

typedef struct f90_type {

    unsigned int                :32;     /* used for future development */
    unsigned int        type    :8;     /* type code */
    unsigned int        dpflag  :1;     /* set if declared double precision
                                         * or double complex */
    unsigned int kind_or_star   :3;     /* Set if KIND= or *n appears in the
                                         * variable declaration.  Values
                                         * are from enum dec_codes */
    unsigned int        int_len :12;    /* internal length in bits of iolist
                                         * entity. 8 for character data to
                                         * indicate size of each character */
    unsigned int        dec_len :8;     /* declared length in bytes for *n
                                         * or KIND value. Ignored if
                                         * kind_or_star==DVD_DEFAULT */
} f90_type_t;

typedef struct cilist_header {
        unsigned int    version :8;     /* contains CILIST_VERSION */
        unsigned int    uflag   :8;     /* type of unit identifier */
        unsigned int            :4;     /* unused */
        unsigned int    iostatflg:1;    /* iostat= present flag */
        unsigned int    eorflag :1;     /* eor= present flag */
        unsigned int    endflag :1;     /* end= present flag */
        unsigned int    errflag :1;     /* err= present flag */
        unsigned int            :2;     /* unused */
        unsigned int    advcode :3;     /* ADVANCE= specifier value     */
        unsigned int    edcode  :1;     /* 1 if ENCODE/DECODE flag */
        unsigned int    internal :1;    /* 1 if internal file */
                                        /* must be 1 if edcode is 1 */
        unsigned int    dflag   :1;     /* 1 if direct access */
        unsigned int    fmt     :8;     /* type of format (or list-directed) */
        unsigned int    stksize :8;     /* size in words of stack space */
                                        /* passed as 3rd arg to         */
                                        /* _FRF/_FWF/_FRU/_FWU          */
        unsigned int            :8;     /* unused */
        unsigned int    icount  :8;     /* size of struct control list in */
                                        /* words */
} cilist_header_type;

typedef struct {
        unsigned int    version :3;     /* contains IOLIST_VERSION */
        unsigned int            :27;    /* unused */

        /*
         * Iolist table entry bits indicate whether data transfer statement
         * contains more than one iolist table.  If iolfirst=iollast=1, then
         * table is entire iolist.  If iolfirst=iollast=0, then table is
         * middle iolist table.
         */

        unsigned int    iolfirst:1;     /* 1 if first IO item list for current*/                                        /* statment IO statement */
        unsigned int    iollast :1;     /* 1 if last IO item list for current */                                        /* statment IO statement */
        unsigned int    icount  :16;    /* number of iolist-items in this */
                                        /* IO item list.  If zero and it is */
                                        /* both first and last io list, there */                                        /* is no io list in statement */
        unsigned int    ioetsize:16;    /* number of words in the current */
                                        /* IO item list, including this */
                                        /* iolist_header */
                                        /* On SGI systems, in 32-bit mode */
                                        /* this is the number of 32-bit */
                                        /* words, and in 64-bit mode this is */
                                        /* the number of 64-bit words. */
} iolist_header_type ;

typedef struct {
        unsigned int    valtype :8;     /* type of iolist entry */
        unsigned int            :24;    /* unused */
        unsigned int            :16;    /* unused */
        unsigned int    ioentsize:16;   /* number of words of the current */
                                        /* iolist item, including this */
                                        /* ioentry_header */
                                        /* On SGI systems, in 32-bit mode */
                                        /* this is the number of 32-bit */
                                        /* words, and in 64-bit mode this is */
                                        /* the number of 64-bit words. */
} ioentry_header_type;

typedef struct {
        unsigned int    indflag :1;     /* 1 if indexed array */
        unsigned int    boundchk:1;     /* Array bounds checking flag */
                                        /* Not used for F90 release 1. */
                                        /* 0=no bounds checking on array */
                                        /* 1=bounds checking on array */

        unsigned int            :30;    /* pad to end of word */
        unsigned int            :32;

} ioarray_entry_type;

#ifdef __cplusplus
}
#endif

extern void Lower_IO_Init (void);

extern WN * lower_io_statement (WN *, LOWER_ACTIONS);



#endif
