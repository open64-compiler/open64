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


#ifndef vstring_INCLUDED
#define vstring_INCLUDED
#ifdef __cplusplus
extern "C" {
#endif

/*
 *  String utilities that use varying length strings
 */

typedef struct {
	int len;
	int max;
	char *str;
} vstring;

/* only vstring.c should set the values in the vstring structure */
#define vstr_len(v)	((v).len+0)
#define vstr_str(v)	((v).str+0)

/*
 * For now, can only have one vstring buffer live at a time.
 * Someday should enhance this, but not needed now.
 */

/* must call vstr_{begin,end} around use of a vstring buffer */
extern vstring vstr_begin (int len);
extern void vstr_end (vstring v);

/* add char to vstring */
extern vstring vstr_append (vstring v, const char c);

/* add string to vstring */
extern vstring vstr_concat (vstring v, const char *s);

/* sprintf that reallocs space if needed */
extern int vstr_sprintf (vstring *v, int index, const char *format, ... /* args */);

#ifdef __cplusplus
}
#endif
#endif	/* vstring_INCLUDED */
