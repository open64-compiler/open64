/*
  Copyright (C) 2021 Xcalibyte (Shenzhen) Limited.

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

  http://www.xcalibyte.com

  For more information, see:
  http://github.com/open64-compiler/open64
  http://gitee.com/open64-compiler/open64

*/

#include "c2w_utils.h"

/* collection of all c++ intrinsic function name and intrinsic id mapping */

namespace wgen {

INTRINSIC
FindCXXStdIntrinsic(const char *fname) {

#include "c2w_intrn_cxx_std_map.inc"
#include "c2w_intrn_cxx_std_vector.inc"
#include "c2w_intrn_cxx_std_tie.inc"
#include "c2w_intrn_cxx_gnu_hash_map.inc"
#include "c2w_intrn_cxx_gnu_hash_multimap.inc"
#include "c2w_intrn_cxx_gnu_hash_multiset.inc"
#include "c2w_intrn_cxx_gnu_hash_set.inc"
#include "c2w_intrn_cxx_std_array.inc"
#include "c2w_intrn_cxx_std_deque.inc"
#include "c2w_intrn_cxx_std_list.inc"
#include "c2w_intrn_cxx_std_multimap.inc"
#include "c2w_intrn_cxx_std_multiset.inc"
#include "c2w_intrn_cxx_std_set.inc"
#include "c2w_intrn_cxx_std_unordered_map.inc"
#include "c2w_intrn_cxx_std_unordered_multimap.inc"
#include "c2w_intrn_cxx_std_unordered_multiset.inc"
#include "c2w_intrn_cxx_std_unordered_set.inc"
/* DO NOT EDIT THIS MARK */

  return INTRINSIC_NONE;
}

} // namespace wgen

