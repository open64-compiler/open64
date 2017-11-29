/*
 * Copyright (C) 2008. PathScale, LLC. All Rights Reserved.
 */
/*
 * Copyright 2005, 2006 PathScale, Inc.  All Rights Reserved.
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of version 2 of the GNU General Public License as
 * published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it would be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  
 *
 * Further, this software is distributed without any warranty that it is
 * free of the rightful claim of any third person regarding infringement 
 * or the like.  Any license provided herein, whether implied or 
 * otherwise, applies only to this software file.  Patent licenses, if 
 * any, provided herein do not apply to combinations of this program with 
 * other software, or any other product whatsoever.  
 *
 * You should have received a copy of the GNU General Public License along
 * with this program; if not, write the Free Software Foundation, Inc., 59
 * Temple Place - Suite 330, Boston MA 02111-1307, USA.
 */

#ifndef DECORATE_UTILS_INCLUD
#define DECORATE_UTILS_INCLUD

extern void parse_decorate_script(const char *);

#if defined(__cplusplus)
extern "C" {
#endif /* __cplusplus */
const char *get_symbol_decoration(const char *);
void put_external_label(const char *, void *);
void *get_external_label(const char *);
#if defined(__cplusplus)
}
#endif /* __cplusplus */

#endif
