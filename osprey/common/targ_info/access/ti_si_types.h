/*
 * Copyright (C) 2010 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

/*
 * Copyright (C) 2007 PathScale, LLC.  All Rights Reserved.
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


/* See ti_si.h for detailed interface description.  */

#ifndef TI_SI_TYPES_H_INCLUDED
#define TI_SI_TYPES_H_INCLUDED
#ifdef __cplusplus
extern "C" {
#endif

typedef enum topcode TOPCODE;

#include "topcode.h"

/****************************************************************************
 ****************************************************************************/

typedef struct {
  mUINT64 dw[2];
} SI_BAD_II_SET;

enum { SI_BAD_II_SET_MAX=127 };

/****************************************************************************
 ****************************************************************************/

typedef UINT SI_RESOURCE_ID;

typedef struct {
  const char* name;
  SI_RESOURCE_ID id;
  mUINT8 avail_per_cycle;
  mUINT8 word_index;
  mUINT8 bit_index;
} SI_RESOURCE;

/****************************************************************************
 ****************************************************************************/

typedef mUINT64 SI_RESOURCE_ID_SET;


/****************************************************************************
 ****************************************************************************/

/* SI_RRW -- A resource reservation word */
typedef mUINT64 SI_RRW;

/****************************************************************************
 ****************************************************************************/

typedef struct {
  const char* name;
  mINT32 skew;
  mINT32 avail_per_cycle;
} SI_ISSUE_SLOT;

/****************************************************************************
 ****************************************************************************/

typedef struct {
  const SI_RESOURCE* resource;
  mINT32 total_used;
} SI_RESOURCE_TOTAL;

/****************************************************************************
 ****************************************************************************/

typedef const SI_RRW* SI_RR;

/****************************************************************************
 ****************************************************************************/
typedef UINT SI_ID;

typedef struct {
#ifdef Is_True_On
  const char* name;
#endif
  const mUINT8 *operand_access_times;
  const mUINT8 *result_available_times;
  mINT32 load_access_time;
  mINT32 last_issue_cycle;
  mINT32 store_available_time;
  SI_RR rr;
  SI_RR alter_rr;
  const SI_RESOURCE_ID_SET *resources_used;
  mUINT32 ii_info_size;
  const SI_RR *ii_rr;
  const SI_RESOURCE_ID_SET * const *ii_resources_used;
  SI_BAD_II_SET bad_iis;
  mINT32 valid_issue_slot_count;
  SI_ISSUE_SLOT * const *valid_issue_slots;
  mINT32 resource_total_vector_size;
  const SI_RESOURCE_TOTAL *resource_total_vector;
  mUINT8 write_write_interlock;
} SI;

typedef struct {
  const char *name;
  const int SI_issue_slot_count;
  const SI_ISSUE_SLOT * const * si_issue_slots;
  const int SI_ID_count;
  const int *SI_ID_si;
  const int *SI_top_si;
} SI_MACHINE;

/****************************************************************************
 ****************************************************************************/

#ifdef __cplusplus
}
#endif
#endif
