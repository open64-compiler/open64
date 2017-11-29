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


#pragma ident "@(#) libu/util/asntools.c	92.2	07/07/99 13:18:33"

#include <stdio.h>	
#include <stdlib.h>	
#include <ctype.h>	/* for isspace */
#ifndef	_ABSOFT
#include <malloc.h>
#endif
#include <unistd.h>
#include <errno.h>
#include <fcntl.h>
#include <string.h>
#include <cray/fortio.h>	/* for MXUNITSZ */
#include <cray/assign.h>
#include <sys/types.h>
#include <sys/stat.h>

extern long _mproc;

#define WHSPACE		" \t"		/* characters accepted as white space*/

/*
 *	Prototypes 
 */

static int
atabulate(char *np, int len, char *list[], int *count);

void
_ae_echoenv(char *shell, char *name, char *value);

/*
 *	_ae_externalize
 *	_ae_externalize_file
 *	_ae_externalize_env
 *
 *		Copies a local assign environment into the global assign
 *		environment file or to the process environment.  
 *
 *	Return value:
 *
 *		0 on normal exit, -1 on error (with errno set)
 */
int
_ae_externalize(
int			fromwhere,	/* AE_LIB, AE_ASSIGN, or AE_ASGCMD */
FILE			*gfile,		/* global assign environment file. */
assign_environment	*aep)
{
	if (gfile != NULL)
		return( _ae_externalize_file(gfile, aep) );
	else
		return( _ae_externalize_env(fromwhere, aep) );
}

int
_ae_externalize_file(
FILE			*gfile,		/* global assign environment file. */
assign_environment	*aep)
{
	register int	i;
	register int	ss;
	register long	bytecnt;
	assign_record	*arp;

	rewind(gfile);

	for (i = 0; i < aep->rec_cnt; i++) {

		arp	= &aep->ar[i];
		ss	= fprintf(gfile, "assign %s", arp->attr);

		if (ss == -1) {
			ERRET(ERAS_WRERR);
		}

		switch (arp->id.type) {
		case BYFILE:
		case BYGLOBAL:
		case BYPATTERN:
			ss	= fprintf(gfile, " %c:%s\n", arp->id.type,
							     arp->id.u.str);
			break;

		case BYUNIT:
			ss	= fprintf(gfile, " u:%ld\n", arp->id.u.unit);
			break;
		}

		if (ss == -1) {
			ERRET(ERAS_WRERR);
		}
	}

/*
 *	We truncate the file after the end of the data written.
 */
	bytecnt	= ftell(gfile);		/* get file size */

	if (fflush(gfile) == EOF) {
		ERRET(ERAS_WRERR);
	}

	ss	= lseek(fileno(gfile), bytecnt, SEEK_SET);

	if (ss == -1)
		return(-1);

#ifdef	_UNICOS
	ss	= trunc(fileno(gfile));
#else
	ss	= ftruncate(fileno(gfile), bytecnt);
#endif
	if (ss == -1)
		return(-1);
	
	return(0);
}

int
_ae_externalize_env(
int			fromwhere,	/* AE_LIB, AE_ASSIGN, or AE_ASGCMD */
assign_environment	*aep)
{
	char	*str;
	char	*str2;
	char	*thevar;	/* name of the environment variable */

	str	= _ae_build_envstring(aep);
	thevar	= _lae_get_assign_var_name();

	/*
 	 * If we are calling an assign library routine, then we store the
	 * assign options in a default environment variable if the FILENV
	 * environment variable is not set.
	 */
	if (thevar == NULL && fromwhere == AE_LIB) {
		static char	ebuf [	sizeof(ASSIGN_ENV_VAR) 
					+ sizeof("=$")
					+ sizeof(ASSIGN_OPT_VAR) 
					+ sizeof("\0") ];
		char		*ep = ebuf;
		static plock_t	lockvar;

		(void) strcpy(ebuf, "");
		(void) strcat(ebuf, ASSIGN_ENV_VAR);
		(void) strcat(ebuf, "=$");
		(void) strcat(ebuf, ASSIGN_OPT_VAR);

		if (putenv(ebuf) != 0) {	/* "FILENV=$_LIBASSIGNENV_" */
			free(str);
			errno	= FENOMEMY;
			return(-1);
		}

		thevar	= ASSIGN_OPT_VAR;
	}

	if (thevar == NULL) {
#ifdef	_UNICOS
		errno	= ERAS_FILENV;
#else
		errno	= ERAS_FILENVPOS;
#endif
		return(-1);
	}

	if (fromwhere == AE_LIB) {

		str2	= malloc(strlen(thevar) + 1 + strlen(str) + 1);

		if (str2 == NULL) {
			free(str);
			errno	= FENOMEMY;
			return(-1);
		}

		(void) strcpy(str2, thevar);
		(void) strcat(str2, "=");
		(void) strcat(str2, str);

		if (putenv(str2) != 0) {
			free(str);
			free(str2);
			errno	= FENOMEMY;
			return(-1);
		}
	}
	else {
		_ae_echoenv(getenv("SHELL"), thevar, str);
		printf("\n");
	}

	free(str);

	return(0);
}

#define ADD(S) {					\
	char		*sp;				\
	register int	len;				\
	sp	= S;					\
	len	= strlen(sp);				\
	if (rs_pos + len >= rs_size) {			\
		rs_size	= rs_size + GROWSIZE;		\
		rstring	= realloc(rstring, rs_size);	\
		if (rstring == NULL) {			\
			errno	= FENOMEMY;		\
			return(NULL);			\
		}					\
	}						\
	(void) strcpy(&rstring[rs_pos], sp);		\
	rs_pos	= rs_pos + len;				\
}

/*
 *	_ae_build_envstring
 *
 *		Build a string containing the new contents for the 
 *		process environment variable used to hold the assign
 *		environment records.
 *
 *	Return value:
 *
 *		Pointer to the constructed string.   NULL if an error
 *		occurred (errno is set to the error code).
 */
char *
_ae_build_envstring(assign_environment *aep)
{
#define	GROWSIZE	128
	register int	i;
	register int	ss;
	register int	rs_size;
	register int	rs_pos;
	char		*rstring;
	char		unitbuf[40]; 	/* must hold " u:maxint" */
	assign_record	*arp;
	extern	int	_Ae_asgcmd;

	rs_size	= GROWSIZE;
	rs_pos	= 0;
	rstring	= malloc(rs_size);

	if (rstring == NULL) {
		errno	= FENOMEMY;
		return(NULL);
	}

	for (i = 0; i < aep->rec_cnt; i++) {

		arp	= &aep->ar[i];

		ADD(DELIMSTR);

		if (_Ae_asgcmd) {
			ADD("asgcmd ");
		}
		else {
			ADD("assign ");
		}

		ADD(arp->attr);

		switch (arp->id.type) {

		case BYFILE:
			ADD(" f:");
			ADD(arp->id.u.str);
			break;

		case BYGLOBAL:
			ADD(" g:");
			ADD(arp->id.u.str);
			break;

		case BYPATTERN:
			ADD(" p:");
			ADD(arp->id.u.str);
			break;

		case BYUNIT:
			ss	= sprintf(unitbuf, " u:%ld", arp->id.u.unit);

			if (ss == -1) {
				free(rstring);
				errno	= ERAS_WRERR;
				return(NULL);
			}
			ADD(unitbuf);
			break;
		}
	}

	rstring[rs_pos]	= '\0';		/* null-terminate it */
	rstring	= realloc(rstring, rs_pos + 1);

	return(rstring);
}

/*
 * _ae_delete	Removes an ID from a local assign environment.
 *
 *		Returns 0
 */
_ae_delete(
assign_record		*arp,	/* pointer to assign record to be deleted */
assign_environment	*aep)
{
	register int	index;

	(void) _ae_dealloc_recflds(arp);

	index	= arp - aep->ar;

	if (aep->rec_cnt != index) {
		aep->ar[index]	= aep->ar[aep->rec_cnt - 1];
	}

	aep->rec_cnt	= aep->rec_cnt - 1;

	return(0);
}

/*
 * _ae_next	Returns the pointer to the next internal assign record in the 
 *		environment.  If "arprev" is NULL, then return 
 *		the first AR in the environment.
 *
 *		Return stats:  	1 on success, 
 *				0 on end of list,
 */
_ae_next(
assign_record		*prev,		/* pointer to prev pointer */
assign_record		**next,		/* output pointer to next record */
assign_environment	*aep)
{
	if (prev == NULL && aep->rec_cnt > 0) {
		*next	= aep->ar;
		return(1);
	}
	else if (prev != NULL && (++prev - aep->ar) < aep->rec_cnt) {
		*next	= prev;
		return(1);
	}

	return(0);
}
/*
 * _ae_eclipse	Add new attributes to an existing attributes string.
 *		The new attributes are not checked for correctness.
 *
 *	Return stats:  0 on success, -1 on error (with errno set).
 */
_ae_eclipse(
char	*attr1,			/* old attributes list */
int	len1,			/* length of attr1 */
char	*attr2,			/* new attributes which eclipse old */
int	len2,			/* length of attr2 */
char	**newarp)		/* (output) new null-terminated attr string */
				/* allocated with malloc(3). */
{
#define MAX_ATTR 40		/* maximum number of attributes per object */
	register short	eclipse;
	register int	i;
	register int	j;
	register int	len;
	register int	nind;
	char		*np, *p1;
	char		*list1[MAX_ATTR];
	char		*list2[MAX_ATTR];
	int		cnt1, cnt2;	/* indices into list1 and list2 */
	register char	newattr;

	np	= malloc(len1 + len2 + 2);

	if (np == NULL) {
		errno	= FENOMEMY;
		return(-1);
	}

	memcpy(np, attr1, len1);
	np[len1]	= '\0';
	memcpy(&np[len1+1], attr2, len2);
	np[len1+1+len2]	= '\0';		/* format: "attr1\0attr2\0"	*/

	/*
	 * Tabulate attributes in string attr1.
	 */
	p1	= np;

	if (atabulate(p1, len1, list1, &cnt1) == -1) {
		if (np != NULL)
			free(np);
		return(-1);
	}
	/*
	 * Tabulate attributes in string attr2.
	 */
	p1	= &np[len1+1];

	if (atabulate(p1, len2, list2, &cnt2) == -1) {
		if (np != NULL)
			free(np);
		return(-1);
	}

	/*
	 * The following code searches for attribute values in attr2 which
	 * will eclipse different values for the same attribute in attr1.
	 * Any new attributes are added at the end of list1.
	 */

	nind	= cnt1;

	for (i = 0; i < cnt2; i++) {
		eclipse	= 0;
		newattr	= list2[i][1];		/* second char is attr letter */

		for (j = 0; j < cnt1; j++) {
			if (list1[j][1] == newattr) {	/* option letter match*/
				list1[j]	= list2[i];
				eclipse		= 1;
			}
		}
		if (!eclipse) {		/* add new attribute */
			list1[nind++] = list2[i];
			if (nind > MAX_ATTR) {
				if (np != NULL)
					free(np);
				errno	= FEINTUNK;
				return(-1);
			}
		}
	}

	/*
	 * concatenate all attributes back into one string.  We know that the
	 * new string will be <= len1 + len2 + 1 characters long.
	 */
	p1	= malloc(len1 + len2 + 2);
	if (p1 == NULL) {
		if (np != NULL)
			free(np);
		errno	= FENOMEMY;
		return(-1);
	}

	*newarp	= p1;

	for (i = 0; i < nind; i++) {
		if (i != 0)
			*p1++	= ' ';	/* blanks between each attr */
		len	= strlen(list1[i]);
		memcpy(p1, list1[i], len);
		p1	= p1 + len;
	}

	*p1	= '\0';				/* terminate the string */

	if (np != NULL)
		free(np);

	return(0);
}

/*
 * atabulate - translates a string of options into a table of options.
 *	       This function will alter the contents of the buffer pointed to
 *	       by np.
 */
static int
atabulate(
char *np,	/* (input) string of options (NULL-terminated) */
int  len,	/* (input) length of np */
char *list[],	/* (output) array of pointers to the options */
int  *count)	/* (output) number of elements in list */

{
	register int	ind;
	char		*pp, *p2, *p3;

	ind	= 0;
	pp	= np;

	while (pp < &np[len]) {
		_AE_SKIPWHITE(pp);
		if (pp[0] == '\0')
			break;
		if (ind + 1 > MAX_ATTR) {
			errno	= FEINTUNK;
			return(-1);
		}
		list[ind++]	= pp;
		/* scan to end of option letter */
		p2		= strpbrk(pp, WHSPACE);

		if (p2 == NULL)
			p2	= &np[len];
		else {
			/*
			 * Scan to beginning of next option.
			 */
			p3	= p2;
			_AE_SKIPWHITE(p3);	/* p3 is at next token */
			if (p3[0] != '\0' && p3[0] != '-') {
				p2	= strpbrk(p3, WHSPACE);
				if (p2 == NULL)
					p2	= &np[len];
			}
		}

		p2[0]	= '\0';	/* delimit the end of this attribute */
		pp	= p2;

		if (pp < &np[len])
			pp	= pp + 1;
	}

	*count	= ind;

	return(0);
}

/*
 * _ae_printattrs	Prints current valid attributes with explanations.
 *			This routine is called by the assign command.
 */
void
_ae_printattrs(FILE *f)
{
	parse_info	*tabptr;

 	fprintf(f,
"Assign control options:\n");
 	fprintf(f, "\
  -I                    New assign options are added to existing options\n");
 	fprintf(f, "\
  -O                    New assign options replace existing options\n");
 	fprintf(f, "\
  -R                    Remove assign record or assign environment\n");
 	fprintf(f, "\
  -V                    View assign record or assign environment\n");
 	fprintf(f, "\
  -v                    Display the version of the assign command\n");

	fprintf(f,
"Assign attribute options:\n");

	tabptr	= &_Ae_option_parse_info[0];

        while (tabptr->optname != NULL) {
		if (tabptr->sup && tabptr->desc != NULL)
			fprintf(f, "%s\n", tabptr->desc);
		tabptr	= tabptr + 1;
	}

	return;
}

#define UNKNOWN	0	/* shell environment variable unknown or not set */
#define CSH	1	/* csh/tcsh style 'setenv NAME VAL' */
#define SH	2	/* sh/ksh style 'NAME=VAL; export NAME' */

void
_ae_echoenv(char *shell, char *name, char *value)
{
	register short	shtype;

	if (shell == NULL) 
		shtype	= UNKNOWN;
	else {
		register int	len;

		len	= strlen(shell);

		if (len >= 3 && strcmp(shell+len-3, "csh") == 0)
			shtype	= CSH;
		else if (len >= 2 && strcmp(shell+len-2, "sh") == 0)
			shtype	= SH;
		else
			shtype	= UNKNOWN;
	}

	switch (shtype) {

	case UNKNOWN:
	case SH:
		printf("%s='%s';export %s", name, value, name);
		break;

	case CSH:
		printf("setenv %s '%s'", name, value);
		break;
	}

	return;
}
