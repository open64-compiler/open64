/*
 * Copyright (C) 2012 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

/*
 Open64 is free software; you can redistribute it and/or modify it 
 under the terms of the GNU General Public License as published by 
 the Free Software Foundation; either version 2 of the License, 
 or (at your option) any later version.

 Open64 is distributed in the hope that it will be useful, but 
 WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU General Public License for more details.

 You should have received a copy of the GNU General Public License along 
 with this program; if not, write to the Free SoftwareFoundation, Inc., 
 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
*/

#ifndef CWH_TARG_INFO_INCLUDED
#define CWH_TARG_INFO_INCLUDED
#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */
/* Should return BOOL but the BOOL type is not available.
 */
#if defined(TARG_X8664)
extern int is_Target_ABI_n32(void);
#endif
#ifdef __cplusplus
}
#endif /* __cplusplus */
#endif /* CWH_TARG_INFO_INCLUDED */
