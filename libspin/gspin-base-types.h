/*
 * Copyright (C) 2006. QLogic Corporation. All Rights Reserved.
 */

/* */
/* */
/* Copyright 2006 PathScale, Inc. All Rights Reserved. */
/* */
/* This program is free software; you can redistribute it and/or modify it */
/* under the terms of version 2 of the GNU General Public License as */
/* published by the Free Software Foundation. */
/* */
/* This program is distributed in the hope that it would be useful, but */
/* WITHOUT ANY WARRANTY; without even the implied warranty of */
/* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. */
/* */
/* Further, this software is distributed without any warranty that it is */
/* free of the rightful claim of any third person regarding infringement */
/* or the like.  Any license provided herein, whether implied or */
/* otherwise, applies only to this software file.  Patent licenses, if */
/* any, provided herein do not apply to combinations of this program with */
/* other software, or any other product whatsoever. */
/* */
/* You should have received a copy of the GNU General Public License along */
/* with this program; if not, write the Free Software Foundation, Inc., 59 */
/* Temple Place - Suite 330, Boston MA 02111-1307, USA. */
/* */
                                                                                                                           
#ifndef __GPIN_BASE_TYPES_H__
#define __GPIN_BASE_TYPES_H__

#include <stdbool.h>

/* GSPIN wrappers over the basic types+ */

typedef void gs_void_t;
typedef char gs_char_t;
typedef unsigned char gs_unsigned_char_t;
typedef gs_char_t *gs_string_t;
typedef int gs_int_t;
typedef unsigned int gs_unsigned_t;
typedef unsigned int gs_count_t;
typedef bool gs_bool_t;
typedef long gs_long_t;
typedef unsigned long gs_unsigned_long_t;
typedef unsigned long long gs_unsigned_long_long_t;
typedef long long gs_long_long_t;
typedef long double gs_long_double_t;
typedef float gs_float_t;
typedef double gs_double_t;
#define gs_true  true
#define gs_false false

/* GSPIN wrappers over the basic types- */

#endif /* __GPIN_BASE_TYPES_H__ */
