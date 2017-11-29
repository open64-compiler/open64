/*
 * Copyright (C) 2010 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

/*
 * Copyright 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
 */

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


#ifndef ipa_trace_INCLUDED
#define ipa_trace_INCLUDED

/* Trace Options -- Please put them here, and not in arbitrary places */

#define IPA_TRACE_IPA                    1
#define IPA_TRACE_IPAA                   2
#define IPA_TRACE_DETAIL                 4
#define IPA_TRACE_CG                     8
#define IPA_TRACE_IPAA_SUMMARY          16
#define IPA_TRACE_STATS                 32
#define IPA_TRACE_ITERATOR              64
#define IPA_TRACE_SPLIT_COMMON         128
#define IPA_TRACE_SECTIONS             256
#define IPA_TRACE_CPROP_CLONING        512
#define IPA_TRACE_LNO_WRITE           1024
#define IPA_TRACE_SECTION_CORRECTNESS 2048
#define IPA_TRACE_PREOPT_IPL          4096
#define IPA_TRACE_MODREF              8192
#define IPA_TRACE_COMMON_CONST       16384
#define IPA_TRACE_RESHAPE            32768
#define IPA_TRACE_EXCOST             65536
#define IPA_TRACE_SIMPLIFY          131072
#define IPA_TRACE_TUNING           0x40000
#define IPA_TRACE_TUNING_NEW       0x80000
#define IPA_TRACE_ICALL_DEVIRTURAL 0x100000

#endif /* ipa_trace_INCLUDED */
