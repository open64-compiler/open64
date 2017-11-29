/*
 Copyright (C) 2010, Hewlett-Packard Development Company, L.P.
 All Rights Reserved.

 Open64 is free software; you can redistribute it and/or
 modify it under the terms of the GNU General Public License
 as published by the Free Software Foundation; either version 2
 of the License, or (at your option) any later version.

 Open64 is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU General Public License for more details.

 You should have received a copy of the GNU General Public License
 along with this program; if not, write to the Free Software
 Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 MA  02110-1301, USA.
*/
#ifndef __IPA_BE_READ_H__
#define __IPA_BE_READ_H__

#include "be_ipa_util.h"


#ifdef __cplusplus
extern "C" {
#endif

extern IPA_BE_SUMMARY CURRENT_BE_SUMMARY;

extern void
IPA_read_alias_summary (void* fhandle, PU_Info* pu_info_tree, MEM_POOL* pool);

#ifdef __cplusplus
}
#endif

#endif
