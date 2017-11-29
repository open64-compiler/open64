/*
 * Copyright 2004 PathScale, Inc.  All Rights Reserved.
 */

/*

  Copyright (C) 2000, 2001 Silicon Graphics, Inc.  All Rights Reserved.

  This program is free software; you can redistribute it and/or modify it
  under the terms of version 2.1 of the GNU Lesser General Public License 
  as published by the Free Software Foundation.

  This program is distributed in the hope that it would be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  

  Further, this software is distributed without any warranty that it is
  free of the rightful claim of any third person regarding infringement 
  or the like.  Any license provided herein, whether implied or 
  otherwise, applies only to this software file.  Patent licenses, if
  any, provided herein do not apply to combinations of this program with 
  other software, or any other product whatsoever.  

  You should have received a copy of the GNU Lesser General Public 
  License along with this program; if not, write the Free Software 
  Foundation, Inc., 59 Temple Place - Suite 330, Boston MA 02111-1307, 
  USA.

  Contact information:  Silicon Graphics, Inc., 1600 Amphitheatre Pky,
  Mountain View, CA 94043, or:

  http://www.sgi.com

  For further information regarding this notice, see:

  http://oss.sgi.com/projects/GenInfo/NoticeExplan

*/



/*

Initializer and space-requester

*/
#include <stdio.h>
#if ! defined(BUILD_OS_DARWIN)
#include <malloc.h>
#endif /* defined(BUILD_OS_DARWIN) */
#include <string.h>
#include <stdlib.h>
#include "array_alloc.h"

#define AA_NOSTART      1 /* cannot alloc space for handle to point to */
#define AA_INITIAL      2 /* initial alloc amount not usable */
#define AA_EL_SIZE      3 /* element size < 1 */
#define AA_NUMERATOR    4 /* numerator for size change <= 1 */
#define AA_DENOMINATOR  5 /* denominator for size change < 1 */
#define AA_DISCARD      6 /* cannot discard: data trashed somehow */
#define AA_ALLOC1       7 /* inital word of alloc data trashed */
#define AA_ALLOC2       8 /* middle word of alloc data trashed */
#define AA_ALLOC3       9 /* final word of alloc data trashed */
#define AA_ELT_ALLOC   10 /* elements_to_allocate is <= 0 */
#define AA_ELT_ALLOC2  11 /* malloc failure */
#define AA_ELT_ALLOC3  12 /* realloc failure */
#define AA_ALLOC_NULL  13 /* null pointer to aa_alloc */

static char *aa_error[] = {
	"array_alloc no error.",
/* 1*/	"array_alloc Cannot malloc a struct fore aa_handle to point to",
/* 2*/	"array_alloc Initial allocation amount is not usable (<= 1)",
/* 3*/	"array_alloc Initial element size is not usable ( < 1 )",
/* 4*/	"array_alloc Initial numerator is not usable ( < 2)",
/* 5*/	"array_alloc Initial denominator is not usable (< 1 )",
/* 6*/	"array_alloc consistency checks fail in freeing memory",
/* 7*/	"array_alloc initial word of aa structure destroyed",
/* 8*/	"array_alloc middle word of aa structure destroyed",
/* 9*/	"array_alloc final word of aa structure destroyed",
/* 10*/	"array_alloc elements-to-alloc given <= 0",
/* 11*/	"array_alloc malloc failure",
/* 12*/	"array_alloc realloc failure",
/* 13 */ "array_alloc: null pointer passed to aa_alloc",
};

static void error(aa_handle,int);
#define ERROR(l,i)  error(l,i)


struct aa_data_st {
	struct aa_data_st * aa_handle_consistency1;
	char ** aa_base;
	long * aa_next_to_use;
	long * aa_max;
	long aa_initial_alloc;
	long aa_element_size;
	struct aa_data_st * aa_handle_consistency2;
	long aa_realloc_numerator;
	long aa_realloc_denominator;
	void (*aa_error_func) ( int, char *, int , char *);
	int aa_user_num;
	char * aa_user_string;
	struct aa_data_st * aa_handle_consistency3;
};


/*
	Initializataion, setup
*/

aa_handle aa_initialize( char **base,
	long * next_to_use,
	long *max_allocated,
	int initial_allocation,
	int element_size,
	int change_numerator,
	int change_denominator,
	void (*err_func)( int, char *, int, char *),
	int user_num,
	char *user_string)
{

	aa_handle l;

	l = (aa_handle) malloc( sizeof(struct aa_data_st));
	if(l == 0) 
		ERROR(l,AA_NOSTART);
	l->aa_handle_consistency1 =
	l->aa_handle_consistency2 =
	l->aa_handle_consistency3 = l;
	l->aa_base = base;
	*next_to_use = 0;
	l->aa_next_to_use = next_to_use;
	*max_allocated = 0;
	l->aa_max = max_allocated;
	l->aa_initial_alloc = initial_allocation;
	if(element_size < 1)
		ERROR(l,AA_EL_SIZE);
	l->aa_element_size = element_size;
	if(element_size < 1)
		ERROR(l,AA_INITIAL);
	l->aa_realloc_numerator = change_numerator;
	l->aa_realloc_denominator = change_denominator;
#ifdef DEBUG
fprintf(stderr,"dadebug init al %d elt size %d num %d den %d\n",initial_allocation,element_size,change_numerator, change_denominator);
#endif
	if(change_numerator < 2 )
		ERROR(l,AA_NUMERATOR);
	if(change_denominator < 1 || change_denominator >= change_numerator)
		ERROR(l,AA_DENOMINATOR);
	l->aa_error_func = err_func;
	l->aa_user_num = user_num;
	l->aa_user_string = user_string;

#ifdef DEBUG
fprintf(stderr,"dadebug new handle %#x user %s\n",l,user_string);
#endif
	return(l);
}


/*

	Ensure there is space for N elements.
	Return a pointer to the area made available.
	Update "next" integer.

*/
void *
aa_alloc(aa_handle lclaa, long elements_to_alloc)
{
	long count;
	long max;
	char *newbase;
	char *oldbase;
	long len;
	long newmax;
	long elsize;

#ifdef DEBUG
fprintf(stderr,"dadebug handle %#x count %d next to use %d\n",lclaa,elements_to_alloc,*(lclaa->aa_next_to_use));
#endif
	if(!lclaa)
		ERROR(lclaa,AA_ALLOC_NULL);
	if(elements_to_alloc <= 0)
		ERROR(lclaa,AA_ELT_ALLOC);
#ifdef AA_CONSISTENCY_CHECKS
	if(lclaa != lclaa->aa_handle_consistency1)
		ERROR(lclaa,AA_ALLOC1);
	if(lclaa != lclaa->aa_handle_consistency2)
		ERROR(lclaa,AA_ALLOC2);
	if(lclaa != lclaa->aa_handle_consistency3)
		ERROR(lclaa,AA_ALLOC3);
#endif
	count = *lclaa->aa_next_to_use;
	max = *lclaa->aa_max;
	elsize = lclaa->aa_element_size;

	if(( count + elements_to_alloc) < max) {
#ifdef DEBUG
fprintf(stderr," dadebug use next elt: %d elsize %d new next is %d\n",count,elsize,count + elements_to_alloc);
#endif
		*lclaa->aa_next_to_use  = count + elements_to_alloc;
		return((void *)( ((char *) *lclaa->aa_base)
			+ count * elsize));
	}

	/* We know we have to make an increase  now */

#ifdef DEBUG
fprintf(stderr," dadebug increase\n");
#endif
	oldbase = *lclaa->aa_base;
	if(oldbase == (char *)0) {
		/* first time  called with this handle */
		if(elements_to_alloc > lclaa->aa_initial_alloc) 
			max = elements_to_alloc + lclaa->aa_initial_alloc;
		else
			max = lclaa->aa_initial_alloc;

		len = max *elsize;

		newbase = (char *)malloc(len);
		if(newbase == 0)
			ERROR(lclaa,AA_ELT_ALLOC2);
		*(lclaa->aa_base) = newbase;
		*(lclaa->aa_next_to_use) = elements_to_alloc;
		*(lclaa->aa_max) = max;
#ifdef DEBUG
fprintf(stderr,"dadebug new basee calced. \n");
#endif
		return(newbase);
	}

	/* not first time: the realloc case*/

	newmax = max * lclaa->aa_realloc_numerator;
	newmax = newmax / lclaa->aa_realloc_denominator;
	if((count + elements_to_alloc) >= newmax) 
			newmax += elements_to_alloc;
	len = newmax *elsize;
	newbase = (char *)realloc(oldbase,len);
	if(newbase == 0)
		ERROR(lclaa,AA_ELT_ALLOC3);
	*(lclaa->aa_base) = newbase;
	*(lclaa->aa_next_to_use) = count + elements_to_alloc;
	*(lclaa->aa_max) = newmax;
#ifdef DEBUG
fprintf(stderr,"dadebug realloc base returned basee calced. \n");
#endif
	return( newbase + count*elsize);
}




/*

	Free up all memory
	Null pointer input does nothing.
*/

void
aa_discard(aa_handle lclaa)
{
	if(lclaa && lclaa ==lclaa->aa_handle_consistency1 &&
	   lclaa ==lclaa->aa_handle_consistency2 &&
	   lclaa ==lclaa->aa_handle_consistency3 ) {
		if(*(lclaa->aa_base)) 
			free(*(lclaa->aa_base));
		*(lclaa->aa_base) = 0;
		if(lclaa->aa_next_to_use)
			*(lclaa->aa_next_to_use) = 0;
		if(lclaa->aa_max)
			*(lclaa->aa_max) = 0;
		(void)memset(lclaa,0,sizeof(*lclaa));
		return;
	}
	ERROR(lclaa,AA_DISCARD);
}
static void error (aa_handle lclaa, int i)
{
	if(lclaa == 0) {
		(void)fprintf(stderr,"%s. aa_handle %p. error %d\n",
			aa_error[i],lclaa,i);
		abort();
	}
	if(lclaa->aa_error_func) {
		(*(lclaa->aa_error_func))(lclaa->aa_user_num,
			lclaa->aa_user_string,i,aa_error[i]);
	} else {
		(void)fprintf(stderr,"%s. aa_handle %p. error %d. Handle_info: %d %s\n",
			aa_error[i],
			lclaa,
			i,
			lclaa->aa_user_num,
			(lclaa->aa_user_string)? lclaa->aa_user_string: "");
	}
	abort();
}
