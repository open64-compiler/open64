/*
 * Copyright 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
 */

/*

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

*/

#include <alloca.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>

#include "path_intrinsic_list.h"

// Add an intrinsic name to the list we maintain, marking whether it
// should be added to or deleted from the baseline. We set "isfamily_" if it
// is uppercase, and then uppercase the entire name so that it sorts the same
// way as the table in intrin.h (relative to "_" and other intrinsics)
void intrinsic_list_t::add(char *name, int added) {
  if ((list_len_ + 1) >= list_capacity_) {
    intrin_option_t **new_list = new intrin_option_t *[list_capacity_ *= 2];
    memcpy(new_list, list_, list_len_ * sizeof *list_);
    delete[] list_;
    list_ = new_list;
    }
  intrin_option_t *new_option = list_[list_len_++] = new intrin_option_t();
  new_option->isfamily_ = (0 != isupper(*name));
  int name_len = strlen(name);
  new_option->name_ = (char *) malloc(name_len + 1);
  new_option->name_[name_len] = 0;
  for (int i = 0; i < name_len; i += 1) {
    new_option->name_[i] = toupper(name[i]);
    }
  new_option->added_ = added;
  }

int intrinsic_list_t::list_len_ = 0;
#define LIST_CAPACITY 16
int intrinsic_list_t::list_capacity_ = LIST_CAPACITY;
intrin_option_t **intrinsic_list_t::list_ =
  new intrin_option_t *[LIST_CAPACITY];

intrinsic_list_t path_intrinsic_list;

int path_intrinsic_list_length() { return path_intrinsic_list.length(); }

intrin_option_t **path_intrinsic_list_list() {
  return path_intrinsic_list.list(); }
