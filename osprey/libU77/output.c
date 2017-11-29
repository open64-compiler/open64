/*
 * Copyright 2004 PathScale, Inc.  All Rights Reserved.
 */

/*

  Copyright (C) 1999-2001, Silicon Graphics, Inc.  All Rights Reserved.

  This program is free software; you can redistribute it and/or modify it
  under the terms of version 2.1 of the GNU Lesser General Public License
  as published by the Free Software Foundation.

  This program is distributed in the hope that it would be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  Further, any
  license provided herein, whether implied or otherwise, is limited to 
  this program in accordance with the express provisions of the 
  GNU Lesser General Public License.  

  Patent licenses, if any, provided herein do not apply to combinations 
  of this program with other product or programs, or any other product 
  whatsoever.  This program is distributed without any warranty that the 
  program is delivered free of the rightful claim of any third person by 
  way of infringement or the like.  

  See the GNU Lesser General Public License for more details.

  You should have received a copy of the GNU General Public License along
  with this program; if not, write the Free Software Foundation, Inc., 59
  Temple Place - Suite 330, Boston MA 02111-1307, USA.

*/


#include "head.h"
#include <stdio.h>
#include <stdlib.h>
#if defined(BUILD_OS_DARWIN)
#include <stdlib.h>
#else /* defined(BUILD_OS_DARWIN) */
#include <malloc.h>
#endif /* defined(BUILD_OS_DARWIN) */
#include <string.h>
#include <sys/types.h>


int fnamesize = 0;

char *procn;  /* system call name */

/* getfilename: extract filename from procname entry
 */
extern char * 
getfilename (char *ptr)
{
  char *get = NULL;
  int i = -1;
  int last, j,k;
  int onsize;


if (ptr != NULL) {
	while ((++i < LINESIZE) && (ptr[i] != '\0'));

	/*
	 * skip back over ( 
	 */
	last = i - 2;

	while ((--i > -1) && (ptr[i] != ' ') && (ptr[i] != '*'));
	onsize = last -i +5;
	get = (char *) calloc(1, onsize);
	procn = (char *) calloc(1, last - i + 1);

	j = 0;
/*	get[j++] = 't';
 *	get[j++] = 'm';
 *	get[j++] = 'p';
 *	get[j++] = '/';
 */


	k = 0;
	while (i < last) {
		get[j++] = ptr[++i];
		procn[k++] = ptr[i];
	}

	/*
	 * if we had modification to proc name, do it here
	 */

	
	get[j++] = '_';
	get[j++] = '.';
	get[j++] = 'f';
	get[j++] = 'c';
	get[j++] = '\0';

	fnamesize = j ;
	if (onsize != fnamesize) {
	fprintf(stderr,"ERROR: bad internal file name size \n"); 
	fprintf(stderr,"      callocd: %d, realsize: %d\n",onsize,fnamesize);
	}
}

  return(get);
}
	
/* flushout:  use entry to create .c file calling
 *            system routine
 */

extern void
flushout (struct table *p)

{
  char *name;
  FILE *outfile;
  struct llist *lptr;
  int i,j;
  char *newvar1,*newvar2;
  int printed;

  name = getfilename(p->procname);

#ifdef DEBUG
  printf("\n\n name: %s\n",name);
  printf("entry: \n");
  printtable(p);
#endif


  outfile = fopen(name,"w");
  if (outfile == NULL) {
	fprintf(stderr,"ERROR: unable to open %s\n",name);
	exit(1);
  }


/* print includes
 */
  for (lptr = p->includs; lptr != NULL; lptr= lptr->nextl)
	  fprintf(outfile,"#include %s\n",lptr->entry);
  
/* print proc name
 */
  fprintf(outfile,"\n%s",p->procname);


/* print parameters in proc line 
 */
  for (i = 0; i< p->declnum; i++){
        fprintf(outfile," x%d",i);
	if (i !=  p->declnum-1)
		fprintf(outfile,",");
  }
  fprintf(outfile,")\n");

/* print var declarations
 */
  i = 0;
  for (lptr = p->decls; lptr != NULL; lptr= lptr->nextl){
	printed = FALSE;
	for (j=0; lptr->entry[j] != '\0'; j++){
		if (lptr->entry[j] == '[') {
			printed = TRUE;
			newvar1 = (char *) calloc(1,40);
			strncpy(newvar1,lptr->entry,j);
			newvar2 = (char *) calloc(1,40);
			strcpy(newvar2,&(lptr->entry[j]));
		   	fprintf(outfile,"%s x%d %s;\n",newvar1,i++,newvar2);
		}
	}
			
	   if (!printed) fprintf(outfile,"%s x%d;\n",lptr->entry,i++);
  }

/* print body
 */
  
  fprintf(outfile,"{\n");

 /* use procn, set by getfilename as system name called
 */
  fprintf(outfile,"\nreturn(%s(",procn);

  for (i=0; i< p->declnum; i++){
        fprintf(outfile," x%d",i);
	if (i !=  p->declnum-1)
		fprintf(outfile,",");
  }
  fprintf(outfile,"));\n}\n");


  if (fclose(outfile) != 0)
	perror(name);
}

