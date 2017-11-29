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
#ifndef __IPA_BE_WRITE_H__
#define __IPA_BE_WRITE_H__

#ifdef __cplusplus
extern "C" {
#endif

extern void
IPA_write_alias_summary (PU_Info* pu_info_tree, Output_File *fl);

extern void
IPA_irb_write_nystrom_alias_info(PU_Info*, Output_File *);

extern void
IPA_write_siloed_reference_summary (PU_Info* pu_info_tree, Output_File *fl);

#ifdef __cplusplus
}
#endif

#endif
