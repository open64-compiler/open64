
/*
*  Copyright (C) 2008-2009 Advanced Micro Devices, Inc. All Rights Reserved.
*
*  This file is part of libacml_mv.
*
*  libacml_mv is free software; you can redistribute it and/or
*  modify it under the terms of the GNU Lesser General Public
*  License as published by the Free Software Foundation; either
*  version 2.1 of the License, or (at your option) any later version.
*
*  libacml_mv is distributed in the hope that it will be useful,
*  but WITHOUT ANY WARRANTY; without even the implied warranty of
*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
*  Lesser General Public License for more details.
*
*  You should have received a copy of the GNU Lesser General Public
*  License along with libacml_mv.  If not, see
*  <http://www.gnu.org/licenses/>.
*
*/


#ifndef __LIBM_SPECIAL_H__
#define __LIBM_SPECIAL_H__

// exception status set
#define MXCSR_ES_INEXACT       0x00000020
#define MXCSR_ES_UNDERFLOW     0x00000010
#define MXCSR_ES_OVERFLOW      0x00000008
#define MXCSR_ES_DIVBYZERO     0x00000004
#define MXCSR_ES_INVALID       0x00000001

void __amd_handle_errorf(int type, int error, char *name,
                    float arg1, unsigned int arg1_is_snan,
                    float arg2, unsigned int arg2_is_snan,
                    float retval, unsigned int retval_is_snan);

void __amd_handle_error(int type, int error, char *name,
                   double arg1,
                   double arg2,
                   double retval);




#endif // __LIBM_SPECIAL_H__

