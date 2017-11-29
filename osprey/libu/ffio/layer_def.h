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


/* USMID @(#) libu/ffio/layer_def.h	92.1	10/29/99 21:40:31 */
#ifndef _FF_LAYER_DEF
#define _FF_LAYER_DEF
/* Definitions of structures used by assign parsing routines */

/* Values for LAYER_DATA.type */
#define NO_TYPE 0
#define FLD_TYPE 1
#define FLD_EXT_TYPE 2
#define FSS_TYPE 3


/* _INFO1_STR1 and _INFO1_STR2 are used to put values for the */
/* spec_u.hdr.str1 and spec_u.hdr.str2 into the proper bits */

#define _INFO1_STR1(x) (x)
#define _INFO1_STR2(x) (x)

#ifdef _LITTLE_ENDIAN

/* the spec_u stuff which are 64 bit entities need to fill in
   the fields of the elements according to the endianness and
   the layout of the fields (see ffio.h)
*/
#define _STR1M (255)
#define _STR2M (15)
#else
#define _STR1M 0xff0000
#define _STR2M 0x0f000
#endif

extern struct LAYER_DATA * _find_layer_def() ;

/*
 * This describes non-numeric options (e.g., ".scr or .save"  for the mr 
 * layer). 
 */
struct LAYER_OPTS {
	unsigned  class:6  ;	/* FFIO Class - this is stored in */
				/* the spec_u.class_info structure */

	/* info1_mask, info1_val, info2_mask and info2_val are used */
	/* to determine what goes into fields info1 and info2 of */
	/* the  spec_u.class_info structure. (e.g., spec_u.class_info.info1 = */
	/*  (spec_u.class_info.info1 & ~info1_mask) | info1_val) */

	unsigned int info1_mask:24;	
	unsigned int info1_val:24;
	unsigned int info2_mask:32;
	unsigned int info2_val:32;

	/* exclusive_mask1 and exclusive_mask2 are used to determine */
	/* whether conflicting options have been specified. */
	/* For an option, if the spec_u info1 field already contains a bit */
	/* in exclusive_mask1 it is an error */

	unsigned int exclusive_mask1:24;	
	unsigned int exclusive_mask2:32;	

	int  num_mask ;	/* Indicates whether a numeric is required */
			/* Each layer (described by struct LAYER_DATA) */
			/* has num_numerics possible numeric options */
			/* if bit i is set in num_mask, then numeric option */
			/* i must be specified */
	char *name ;	/* name of the option */
     } ;

struct LAYER_ALIAS {
	char *name ;
	char *option_string ;
	char *numeric_string ;
	int  inuse ;
} ;

/*
 * This describes numeric options 
 */
struct LAYER_NUMERICS {
        unsigned check_min:1;
        unsigned check_max:1;
	char unit ;
#if defined(_CRAY) || defined(_MIPSEB) || defined(_LITTLE_ENDIAN)
	int64  min_value;
	int64  max_value;
	int64  def_value;
#else
	int  min_value;
	int  max_value;
	int  def_value;
#endif
	char *name ;
} ;

/*
 * This describes an assign -F option (e.g., "mr" or "cachea")
 */
struct LAYER_DATA {
	int	class_num ;	
	int	type ;	/* Used for layers that are expecting old forms of */
			/* the spec_u words */
	char	*name ; 
	char	*hard_coded_defaults; /* Defaults, as a string of options */
	/* non_zero_mask1 and non_zero_mask2 are used to check for */
	/* required options. (.e.g, if non_zero_mask1 is nonzero, then */
	/* (non_zero_mask1 & spec_u.class_info.info1) should not be 0 */
	int	non_zero_mask1:24;
	int	non_zero_mask2:32;
	int	num_opts ;	/* the number of non-numeric options */
	int	num_flag_words ;
	int	num_numerics ;	/* the number of numeric options */
	int	num_alias ;
	struct LAYER_OPTS *opts ;
	struct LAYER_NUMERICS *numerics ;
	struct LAYER_ALIAS *alias ;
	int     (*chkrtn) ();
} ;


#if !defined(SOFTIZE)
#define ZSOFT(TEXT)             _Pragma(#TEXT) ;
#define SOFTIZE_P(XLIST_P)      ZSOFT(_CRI soft (XLIST_P))
#endif

extern struct LAYER_DATA *_ffio_parse_tables[];

extern int _num_layer_tables;

/*
 * This structure describes assign -C and -N options 
 */
struct CVCHRT_DATA {
	int type;	/* Code for the type of conversion */
	char *name;
};

extern struct CVCHRT_DATA _cvch_parse_tables[];
extern int _num_cvch;

extern struct CVCHRT_DATA _cvrt_parse_tables[];
extern int _num_cvrt;

#endif
