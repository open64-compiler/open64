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

The following is to allow dynamic arrays.
It calls your error function (or prints to stderr if you don't give
one) and abort()s on error.

You must set up the  base pointer and the element next and ''to use''
and the package updates them: thus these are local to you for your
easy reference.

The rest is held in the struct pointed to by aa_handle.
For each array:
	call aa_initialize once, setting things up.
		remember the handle passed back.
	Call aa_alloc as needed.
		It allocates space if necessary and returns
		a pointer to the next available array element.
		Note that the next call to aa_alloc may move
		the array, so

		       *use pointers only for short periods*.

		The only *safe* way to access these arrays is via
		indexes from the base pointer.
	

aa_discard() is probably completely unnecessary.
It frees up all memory allocated by aa_initialize() and aa_alloc() for the
given handle.

All three names are reserved for this package. And only those
3 (beyond ANSI C guaranteed functions).

*/
typedef struct aa_data_st *aa_handle;
aa_handle aa_initialize( char ** /* base */,
			long * /* next to use */,
			long * /* max allocated */,
			int    /* initial allocation */,
			int   /* element size */,
			int  /* change factor numerator */,
			int  /* change factor denominator */,
			void (* /* err func */)(int /* user array number */,
				  
				char * /* user string*/,
				int    /* aa error number */,
				char *  /* aa error string */),
			int   /* user aa array number */,
			char * /* user string */
			);

void *aa_alloc(aa_handle , long /* elements_to_allocate */ );

void aa_discard( aa_handle);

