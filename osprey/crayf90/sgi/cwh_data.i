/*
 * Copyright 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
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

// -*-C++-*-
// Template stuff for sorting
#include <vector>
#include <algorithm>

static ST *orig_st; /* The symbol being initialized (for error reporting only) */
static ST *current_st; /* The symbol being initialized */
static TY_IDX current_ty;
static INT64 current_bytesize;
static INT32 current_pos; /* position in the array to initialize (0 based, byte indexed) */
static INT32 array_pos; /* position in the current array */
static BOOL is_struct_or_array; /* TRUE if initilaizing an array */
static INITO_IDX current_inito;

/* for pulling apart bases and offsets */
typedef struct {
  INT64 offset;
  ST *  base;
} b_and_o;

struct data_element_s {
  INT64 offset;
  INT64 initv_size;
  union {
    b_and_o  bo;
    struct {
      TCON_IDX tc;
      INT32    repeat_count;
    } tc_val;
  } val;
  UINT32 sequence_num:30;
  UINT32 is_b_and_o:1;
  UINT32 is_valid:1;
  
#ifdef DATA_DEBUG
  void print() {
    printf("Off: %12lld   TC: %d (x %d) %s\n",offset,val.tc_val.tc,val.tc_val.repeat_count,
	   is_valid ? "valid" : "invalid");
  }
#endif

  inline data_element_s(INT64 o, TCON_IDX tc, b_and_o *bo, INT64 isize, UINT32 seqnum) {
    offset=o; initv_size=isize; sequence_num = seqnum; is_valid = TRUE;
    if (bo) {
      is_b_and_o = TRUE;
      val.bo = *bo;
    } else {
      is_b_and_o = FALSE;
      val.tc_val.tc = tc;
      val.tc_val.repeat_count = 1;
    }
  }
  
  INITV_IDX create_initv()
  {
    if (is_b_and_o) {
      return (Irb_Init_Symoff(0, 0, 1, val.bo.base, val.bo.offset));
    } else {
      return (Irb_Init_Val(0, 0, val.tc_val.repeat_count, val.tc_val.tc));
    }
  }


  // Compare on offset first. If the offsets are the same, the larger
  // comes first (so that we get a more complete cover). If the sizes are the
  // same (the usual case, we'd expect) take the last one entered
  static bool comp(data_element_s a, data_element_s b) {
    if (a.offset != b.offset) return (a.offset < b.offset);
    if (a.initv_size != b.initv_size) return (b.initv_size < a.initv_size);
    return (b.sequence_num < a.sequence_num);
  }
};


typedef vector<data_element_s> DATA_ELEMENTS;

//
//  miscellaneous stuff needed for data intialization
//
//  st_inito - the INITO for a symbol
//  data_elements - the data elements of 
//
struct data_info_s {
  INITO_IDX st_inito;
  DATA_ELEMENTS data_elements;
  UINT32 sequence_num;
  
  data_info_s(ST *st) {st_inito = New_INITO(st); sequence_num=0;}
  ~data_info_s(void) {}
  
  void sort_data(ST *st) {
    BOOL message_issued = FALSE;
    sort(data_elements.begin(),data_elements.end(),data_element_s::comp);

    // Now pack together the data for multiple initv's, and flag errors
    INT32 vsize,i,last_index;
    INT64 pos=0;

    vsize = data_elements.size();
#ifdef DATA_DEBUG
    printf("======== before duplicate processing %s\n",ST_name(st));
    for (i=0; i < vsize; i++) {
      data_elements[i].print();
    }
#endif
    for (i=0,last_index=0; i < vsize; i++) {
      if (data_elements[i].offset < pos) {
	if (!message_issued) {
	  ErrMsg(EC_Multiple_Initialization,ST_name(st));
	  message_issued = TRUE;
	}
	data_elements[i].is_valid = FALSE;
	continue;
      }
      
      // Valid init, increment pos, check for packing
      if ((last_index != i) &&
	  (pos == data_elements[i].offset) &&
	  (!data_elements[last_index].is_b_and_o) &&
	  (!data_elements[i].is_b_and_o) &&
	  (data_elements[i].val.tc_val.tc == data_elements[last_index].val.tc_val.tc)) {
	// update repeat count of last_index, mark current as invalid
	data_elements[last_index].val.tc_val.repeat_count += 1;
	data_elements[last_index].initv_size += data_elements[i].initv_size;
	data_elements[i].is_valid = FALSE;
	pos += data_elements[i].initv_size;
      } else {
	// update the current position
	pos = data_elements[i].offset + data_elements[i].initv_size;
	last_index = i;
      }
    }
#ifdef DATA_DEBUG
    printf("======== after duplicate processing\n");
    for (i=0; i < vsize; i++) {
      data_elements[i].print();
    }
#endif
  }
  
  inline INITO_IDX Get_Inito(void) {return st_inito;}
  inline DATA_ELEMENTS *Get_Data_Elements(void) {return &data_elements;}

  inline void Reserve(INT64 num_to_add) {
     // To avoid a bad case in which we spend lots of time reserving tiny little
     // chunks, we reserve at least at 5% increase each time. 
     INT64 excess_capacity = data_elements.capacity() - data_elements.size();
     if (excess_capacity > num_to_add) return;

     INT64 new_size = num_to_add + data_elements.size();
     INT64 min_new_size = (INT64) (1.05 * data_elements.capacity());
     if (new_size < min_new_size) new_size = min_new_size;
     data_elements.reserve(new_size);
  }
  
  inline void Add_Data_Element(INT64 offset, TCON_IDX val, b_and_o *bo, INT64 isize) {
#ifdef DATA_DEBUG
    printf("Add at offset %lld: TCON = %d\n",offset,val);
#endif
    data_elements.push_back(data_element_s(offset,val,bo,isize,sequence_num++));
  }
};

typedef data_info_s *DATA_INFO;
	
static DATA_INFO current_data_info;

static void cwh_data_set_init_flag(ST * st, enum list_name list);


