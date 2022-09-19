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


/*
 *  String utilities that use varying length strings
 */
#define USE_STANDARD_TYPES
#include <stdarg.h>
#include <malloc.h>
#include <stdio.h>
#include <string.h>
#include "vstring.h"
#include "errors.h"

#define MAX(a,b)        ((a>=b)?a:b)

#define vstr_max(v)	((v).max+0)
#define set_vstr_max(v,m)	(v).max = m
#define set_vstr_len(v,l)	(v).len = l
#define set_vstr_str(v,s)	(v).str = s

static vstring current_vstring = {0,0,NULL};

static void
Realloc_Vstring (vstring *v, int newlen)
{
	/* make sure realloc doesn't just add 1 char */
	newlen = MAX (newlen, vstr_max(*v)+80);
	set_vstr_max(*v, newlen);
	set_vstr_str(*v, (char*) realloc(vstr_str(*v), vstr_max(*v)));
}

/*
 * For now, can only have one vstring buffer live at a time.
 * Someday should enhance this, but not needed now.
 * To do that, we could keep freelist of vstrings, 
 * and reuse space that way.
 */

/* must call vstr_{begin,end} around use of a vstring buffer */

vstring
vstr_begin (int len) 
{
	if (vstr_max(current_vstring) == 0) {
		set_vstr_str(current_vstring, (char*) malloc(len));
		set_vstr_max(current_vstring, len);
	}
	else if (vstr_len(current_vstring) != 0) {
		DevWarn("vstr_begin before finishing old one?\n");
		set_vstr_len(current_vstring, 0);
	}
	if (vstr_max(current_vstring) < len) {
		Realloc_Vstring (&current_vstring, len);
	}
	return current_vstring;
}

/* frees space used by vstring buffer */
void
vstr_end (vstring v)
{
	set_vstr_len(v, 0);
	v.str[0] = '\0';
	current_vstring = v;	/* so reused later */
}

/* add char to vstring */
vstring
vstr_append (vstring v, const char c)
{
	if (vstr_len(v) + 1 > vstr_max(v)) {
		Realloc_Vstring (&v, vstr_len(v) + 1);
	}
	v.str[v.len] = c;
	v.len++;
	return v;
}

/* add string to vstring */
vstring
vstr_concat (vstring v, const char *s)
{
	int slen = strlen(s);
	if (vstr_len(v) + slen > vstr_max(v)) {
		Realloc_Vstring (&v, vstr_len(v) + slen);
	}
	/* may be nulls in vstr, so can't concat from beginning;
	 * instead just copy onto end of string. */
	strcpy(vstr_str(v)+vstr_len(v), s);
	set_vstr_len(v, vstr_len(v) + slen);
	return v;
}

/* 
 * sprintf that reallocs space if needed.
 * the string is formatted into the vstring v at index position.
 */
int
vstr_sprintf (vstring *v, int index, const char *format, ... /* args */)
{
	int len;
	va_list ap;
	char *p;
	len = strlen(format);
	va_start (ap, format);
	p = (char*) format;
	while (*p != '\0') {
		if (*p == '%') {
			p++;
			if (*p == '%') ;	/* ignore */
			else if (*p == 's') {
				len += strlen(va_arg(ap,char*));
			}
			else {
				/* numeric */
				va_arg(ap,int);
				len += 16;	/* max numeric size? */
			}
		}
		p++;
	}
	va_end(ap);
	if (len > vstr_max(*v)) {
		Realloc_Vstring (v, len);
	}
	va_start (ap, format);
	len = vsprintf(v->str+index, format, ap);
	set_vstr_len(*v, index + len);
	va_end(ap);
	if (vstr_len(*v) > vstr_max(*v)) 
		Fatal_Error("vstr_sprintf overflowed");
	return len;
}
