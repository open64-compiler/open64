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


#pragma ident "@(#) libu/ffio/asgrls.c	92.1	06/29/99 13:16:47"

#include <sys/param.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <memory.h>
#include <malloc.h>
#include "spec_parse.h"

#undef NUMARG
#define NUMARG()	_numargs() /* XMP CC 4.0 or better */

/*
 *  fchar and alias handling routines for libc and shell(s).
 */

#define FLDSEPC '\t'
#define FLDSEPC2 '#'
#define FLDSEPS "\t"
#define FLDSEPS2 "#"
#define FLDBOTH "#\t"
#define NTRYSEPC '\n'
#define NTRYSEPC2 '%'
#define NTRYSEPS "\n"
#define NTRYSEPS2 "%"
#define NTRYBOTH "%\n"

/*
 * search the aliases for the specified path
 * return pointer to the entry containing the name passed and the 'real' one.
 * Return 0 if not found.
 *	aliasptr is ptr to alias environment string
 *	path is name to be found in the alias entries
 */
char *
_g_alias(char *aliasptr, char *path)
	{
	char *cp, *cp2, *name, *last;

	cp = aliasptr;
	name = path;
	while (*cp)
		{
		last = cp;
		cp2 = name;
		if (*cp == *cp2)
			{
			/* Does the entire name match? */
			while (*cp == *cp2)
				{
				cp++;
				cp2++;
				}
			if ((*cp == FLDSEPC || *cp == FLDSEPC2) && *cp2 == '\0')
				return(last);
			}
		/* skip to the end of the entry */
		while(*cp && *cp != NTRYSEPC && *cp != NTRYSEPC2) cp++;
		if (*cp) cp++;
		}
	/* didn't find it... */
	return(0); /* NULL */
	}

#ifdef _CRAY
/*
 * _g_asg_entry()
 *	return a pointer to the fchar entry for the given file
 *	name.  Aliases are searched and resolved.  NULL is
 *	returned if no entry is found
 */
char *
_g_asg_entry(name, fcp, alp)
char *name;
char *fcp, *alp;
	{
	int na;
	char *fcharptr, *aliasptr;
	char *aliasntry, *entry;
	char lname[PATH_MAX];
	extern char _asg_fchar[];
	extern char _asg_alias[];
/*
 *	Search for file characteristics information
 */
	na = NUMARG();
	if (na < 2)
		fcharptr = getenv (_asg_fchar);
	else
		fcharptr = fcp;
	if (fcharptr == 0)
		return(0);
/*
 *	find aliases
 */
	if (na < 3)
		aliasptr = getenv (_asg_alias);
	else
		aliasptr = alp;
	if (aliasptr != 0)
		{
		if ((aliasntry = _g_alias(aliasptr, name)) != 0)
			{
			_cpyname(lname, _g_rname(aliasntry));
			}
		}
	else /* no aliases */
		strcpy(lname, name);

	entry = _g_fchar(fcharptr, lname);
	return(entry);
	}
#endif

/*
 * search the file characteristics for the specified path.
 * return pointer to beginning of entry for that file.  Aliases are NOT
 * searched.
 *	fcharptr- ptr to environment variable value
 *	path	- ptr to 'real' name whose characteristics are desired.
 */

char *
_g_fchar(char *fcharptr, char *path)
	{
	char *cp, *cp2, *last;

	cp = fcharptr;
	while (*cp)
		{
		last = cp;
		cp2 = path;
		if (*cp == *cp2)
			{
			/* Does the entire name match? */
			while (*cp == *cp2)
				{
				cp++;
				cp2++;
				}
			if ((*cp == FLDSEPC || *cp == FLDSEPC2) && *cp2 == '\0')
				return(last);
			}
		/* skip to the end of the entry */
		while(*cp && *cp != NTRYSEPC && *cp != NTRYSEPC2) cp++;
		if (*cp) cp++;
		}
	/* didn't find it... */
	return(0); /* NULL */
	}

/*
 * extract the file descriptor field from the given file
 * characteristics entry and return the integer value of it
 *	entry - ptr to file characteristics entry in environment variable
 */
_g_fchar_fd(char *entry)
	{
	int i;

	if (entry == 0) return(-1); /* just in case */
	/* Find first FLD separator */
	entry = strpbrk(entry, FLDBOTH) + 1;
	/* if two consecutive FLD seps, no fd */
	if ((*entry == FLDSEPC || *entry == FLDSEPC2)) return(-1);
	i = 0;
	while (*entry != FLDSEPC)
		{
		i *= 10;
		i += *entry - '0';
		entry++;
		}
	
	return(i);
	}

/*
 * extract the real name from an aliases entry
 * return char ptr to name
 */
char *
_g_rname(char *entry)
	{

	return(strpbrk(entry, FLDBOTH));
	}
/*
 * Get an option by name from the environment fchar
 */
char *
_g_fchar_opt(char *entry, char letter)
	{
	
/*
 *      The option string stored in the environment
 *      variable is in rigorous form. No unnecessary care is taken
 *      to do parsing
 */
	while(	*entry != '\0' &&
		(*entry != '-' || *(entry+1) != letter))
		{ 
		/* skip option letter and space separator */ 
		entry += 3;
		/* find trailing space */ 
		entry = strchr(entry, ' '); 
		if (entry == (char *)0)
			return((char *)0);
		/* skip space */ 
		entry++; 
		} 
/* 
 *      either the desired option, or EOS has been found 
 */
	if(*entry == '\0') return(0); /* NULL */

	/* skip option letter and space separator */ 
	entry += 3;
	return(entry);
	}
/*
 * extract the opt field from the given entry
 * return char ptr to string
 */
char *
_g_fchar_opts(char *entry)
	{
	entry = strpbrk(entry, FLDBOTH) + 1;
	entry = strpbrk(entry, FLDBOTH) + 1;
	/* if newline next, no opts */
	if (*entry == NTRYSEPC || *entry == NTRYSEPC2) return(0);
	return(entry);
	}

/*
 * _bld_fchar()
 * build file characteristics environment variable.
 */
char *
_bld_fchar(char *fcharptr, char *filename,int fd, char *optstring)
	{
	char *newfchar, *cp;
	int old_len;
	char cp2[16], *old_entry, *next_entry;

	if (fcharptr == (char *)0)
		fcharptr = "";
	old_entry = _g_fchar(fcharptr, filename);
	if (old_entry != (char *)0)
		{
		cp = strpbrk(old_entry, NTRYBOTH) + 1;
		next_entry = cp;
		old_len = cp - old_entry;
		}
	else
		{
		next_entry = "";
		old_len = 0;
		}


/*
 *	length of new fchars variable is
 *	old_total_length - old_entry + new_entry
 *		new_entry =  newname + fd + opt string + (two FLDs + '\n')
 *	In the calculation below the '15' represents 2 FLDs, newline terminator,
 *	and 12 places for the file descriptor number. (!)  I like to leave a
 *	little elbow room.
 */
	newfchar = malloc(strlen(fcharptr)+strlen(filename)+
			strlen(optstring) - old_len + 15);
	newfchar[0] = '\0';
/*
 *	Copy old FCHARS before old entry by same name
 */
	if (old_entry != (char *)0) /* if old entry exists */
		{
		if ((old_entry - fcharptr) > 0) /* if not FIRST entry */
			{
			memcpy(newfchar, fcharptr, old_entry - fcharptr);
			newfchar[old_entry - fcharptr] = '\0';
			}
		else
			/* if first entry, do nothing */
			;
		}
	else
		strcat(newfchar, fcharptr);
	strcat(newfchar, filename);	/* new filename */
	strcat(newfchar, FLDSEPS2);	/* field separator */

	if (fd >= 0)			/* add fd */
		sprintf(cp2, "%d", fd);
	else
		cp2[0] = '\0';

	strcat(newfchar, cp2);		/* file descriptor */
	strcat(newfchar, FLDSEPS2);
	strcat(newfchar, optstring);	/* opt string */
	strcat(newfchar, NTRYSEPS2);
	strcat(newfchar, next_entry);	/* Add old fchars after file */

	return(newfchar);
	}

/*
 * _bld_alias()
 * build file characteristics environment variable.
 */
char *
_bld_alias(char *aliasptr, char *alias, char *filename)
	{
	int old_len;
	char *newalias, *old_alias, *next_alias;
	char *cp;

	if (!aliasptr) aliasptr = "";
	old_alias = _g_alias(aliasptr, alias);
	if (old_alias != (char *)0)
		{
		cp = strpbrk(old_alias, NTRYBOTH) + 1;
		next_alias = cp;
		old_len = cp - old_alias;
		}
	else
		{
		next_alias = "";
		old_len = 0;
		}

/*
 * length is old + newname + rname + (FLD and term `0`)
 */
	newalias = malloc(strlen(aliasptr) + strlen(filename) +
			strlen(alias)-old_len+3);
/*
 *	Copy old ALIASES before old entry by same name
 */
	if (old_alias != (char *)0) /* if old alias exists */
		{
		if ((old_alias - aliasptr) > 0) /* if not FIRST entry */
			{
			memcpy(newalias, aliasptr, old_alias - aliasptr);
			newalias[old_alias - aliasptr] = '\0';
			}
		else
			/* nothing */
			;
		}
	else
		strcat(newalias, aliasptr);	/* old aliases */

	strcat(newalias, alias);	/* new alias */
	strcat(newalias, FLDSEPS2);	/* field separator */
	strcat(newalias, filename);	/* new filename */
	strcat(newalias, NTRYSEPS2);	/* entry separator */
	strcat(newalias, next_alias);	/* entries after old name */

	return(newalias);
	}
