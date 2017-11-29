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



#ifndef PATH_INTRINSIC_LIST_INCLUDED
#define PATH_INTRINSIC_LIST_INCLUDED

#include <ctype.h>

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

typedef struct {
  char *name_; /* Name of intrinsic */
  char added_; /* True if we should add, false if we should delete */
  char isfamily_; /* True if this is a family, not an individual intrinsic */
  } intrin_option_t;

extern int path_intrinsic_list_length();

extern intrin_option_t **path_intrinsic_list_list();

#ifdef __cplusplus
}

// Maintain a list of intrinsics to be added to the baseline or deleted
// from the baseline. We are responsible only for the list: the
// "enter_intrinsic_info" function in fe90/p_driver.c establishes the
// baseline list and adds to that list or deletes from it.
class intrinsic_list_t {

  static int list_len_;
  static int list_capacity_;
  static intrin_option_t **list_;

  public:

    // Add an intrinsic name to the list we maintain, marking whether it
    // should be added to or deleted from the baseline.
    static void add(char *name, int added);

    static int length() { return list_len_; }

    static intrin_option_t **list() { return list_; }
  };

extern intrinsic_list_t path_intrinsic_list;

#endif /* __cplusplus */

#endif /* PATH_INTRINSIC_LIST_INCLUDED */

