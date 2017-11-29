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
 ****************************************************************************
 *				Makedepend
 ****************************************************************************
 *
 *	The is a compiler tool that generates and maintains Makefile
 *	dependency information in a file called a Makedepend file.
 *	The idea is that some dependencies change over time and the 
 *	maintainer of the source code may not recognize the changes
 *	but would still like to have the dependency information.
 *
 *	For example, a makefile will know about FOO.o depending
 *	upon FOO.c but it is much harder to write down the web
 *	of dependencies for every file included by FOO.c .
 *
 *	Any tool that links with libcmplrs can call routines from
 *	here to manage/update dependency information as it is discovered.
 *	The entry points are:
 *
 *		MDopen		-- begin collecting dependency info for 
 *				   one specified target and one tool.
 *		MDupdate	-- add new dependencies for target.
 *
 *		MDclose		-- process Makedepend file updating
 *				   with new dependency information.
 *
 *	All valid Makedepend lines look like:
 *
 *              target : dependency list #:tool\n
 *	   or
 *              target : dependency list \n
 *	   or
 *              blanks \n
 *	   or
 *              # comment \n
 *	
 ****************************************************************************
 *
 *	NOTE 1:	The Makedepend file is locked only during MDclose.  So 
 *		parallel tools do not interfere with each other except
 *		if they both call MDclose at the same time, in which case
 *		file locking serializes access to the Makedepend file.
 *
 *	NOTE 2: Locking over NFS is very often broken.  There is some
 *		belief that the way we do locking, here, is inherently
 *		unstable and will often be broken.  If I could figure
 *		out how to append/grow the Makedepend file while it is
 *		mmap-ed, without doing using this hack of re-opening 
 *		it for append access, we could make this more reliable.
 *
 ****************************************************************************
 */
#if ! (defined(linux) || defined(BUILD_OS_DARWIN))
#include <bstring.h>
#endif
#include <ctype.h>
#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include "make_depend.h"

/*
 *	Debugging macros
 */
#if  MD_DEBUG
#define	ENTER(s)  printf("ENTER: %s\n",s);
#define	EXIT(s)   printf("EXIT : %s\n",s);
#define	LOG(s)    printf s ;
#else
#define	ENTER(s)  
#define	EXIT(s)  
#define	LOG(s)  
#endif


/*
 * Compute initial number of free entries from table size, and initial table
 * size estimate from number of entries.
 */
#define	FREECOUNT(x)	((x) - (x) / 4)
#define	TABLESIZE(n)	((n) + (n) / 3)
#define	INITIALSIZE	(1 << 8)
#define INITSTABSIZE	(0x40000) 

/*
 * We assume that strings will never be longer than this.
 * The reason for making this assumption is so that we 
 * don't have to scan strings to find out their length
 * within the functin MDstrcpy.
 */
#define	MAXSTRINGLEN	512

char scratch[80];

typedef struct dependency Dependency;

struct dependency {
	char		*name;		/* file on which target depends */
	Dependency	*next;		/* hash collision linkage */
};

struct mdrule {
	char		*toolname;	/* name of language processor */
	char		*filename;	/* name of make-dependencies file */
	char		*target;	/* make rule left-hand side */
	void		(*error)(const char*,...); /* error reporting function */
	unsigned int	entries;	/* number of active entries */
	unsigned int	freecount;	/* number of free hash table entries */
	unsigned int	hashmask;	/* hash mask, i.e. (table size - 1) */
	Dependency	*table;		/* dynamically allocated hash table */
	char		*stab;		/* dynamically allocated string table*/
	char		*stabcur;	/* next free location in stab	*/
	char		*stabmax;	/* last byte of stab + 1	*/
};


typedef	struct	mdfile {
	int	f;		/* dependency file	*/
	const char*	filename;	/* filename		*/
	char*	base;		/* map base, if mapped	*/
	char*	limit;		/* map limit, if mapped	*/
	int	size;		/* map size		*/
	struct stat stb;	/* stat information	*/
	} MDfile_t;

/*
 * MDfile_init
 *
 * Initialize a variable of type MDfile.
 */
static
void
MDfile_init ( MDfile_t *m ) {
	m->f 		= 0;
	m->filename 	= "";
	m->base		= 0;
	m->limit	= 0;
	m->size		= 0;
	m->stb.st_size	= 0;
}
	

/*
 * MDtoolcmp
 *
 *
 * Compare a null terminated string (s) with a character array (tool)
 * holding the tool signature, terminated by blanks followed by a newline.
 *
 */
static
int	
MDtoolcmp ( const char *tool, const char *s, int slen ) {
	const char *t;
	int	toollen;
	
	t = strpbrk(tool," \n");	
	toollen = t-tool;

	if ( (t == 0) || (toollen != slen) )   {return -1;}

	return strncmp(tool,s,slen);
}


/*
 * MDtargcmp
 *
 *
 * Compare a null terminated string (s) with a character array (targ)
 * holding the target, terminated by blanks followed by a colon.
 *
 */
static
int	
MDtargcmp ( const char *targ, const char *s, int slen ) {
	const char *t;
	int	targlen;
	
	t = strpbrk(targ,": ");	
	targlen = t-targ;

	if ( (t == 0) || (targlen != slen) )   {return -1;}

	return strncmp(targ,s,slen);
}


/*
 * MDnewstab
 *
 * In order to avoid lots of calls to malloc we do a single malloc
 * of INITSTABSIZE.
 *
 * If the string table overflows, instead of doing something cleaver,
 * we just allocate another table and never free the first string
 * table.  This is not a big deal as typically, this operation is
 * done once at the very end of a compile or link and so it doesn't
 * really matter if we free the space or not.  
 *
 */
void
MDnewstab(MDhandle h) 
{
   h->stab	= (char *) malloc(INITSTABSIZE);
   h->stabcur	= h->stab;
   h->stabmax	= h->stab + INITSTABSIZE;
}

/*
 * MDstrcpy
 *
 * Copy a string into the stringtable, stab, and return pointer to the
 * copy of the string.  
 *
 * We make a copy of the string because the compilers (and other users
 * of this utility) can't guarantee that they don't touch the strings
 * that hold the dependency filenames passed into MDupdate.
 *
 */
char *
MDstrcpy(MDhandle h, const char * src ) 
{ 
	char  *dest;
	int	freespace;

	if ( src == NULL ) { return NULL; }

	/*
	 *  Will we overflow the table?  Get a new one if needed. 
	 *  Note that we are assuming that all passed in strings
	 *  are less than MAXSTRINGLEN bytes in length -- probably
	 *  not a valid assumption.
	 */
	freespace	= (h->stabmax - h->stabcur);
	if (freespace < MAXSTRINGLEN) {
		MDnewstab(h);
		freespace=(h->stabmax - h->stabcur);
	}

	/*
	 *  Make a copy of the string into the string table
	 */
	dest		= h->stabcur;
	h->stabcur	= (char *) memccpy(h->stabcur, src, '\0', freespace );
	if ( h->stabcur == NULL ) { 
		/* 
		 *  We're screwed if the string length is 
		 *  longer than the whole table 
		 */
		h->error("String table overflow in MDupdate\n", h->filename);
		exit(1);
	}

	return dest;
}


/*
 * Avoid accumulating common dependency suffixes (.h, .a, .o) in the least
 * significant bits of the hash function.  We don't worry about wasted time
 * accumulating the common directory prefixes $ROOT/usr/{include,lib}.
 */
static Dependency *
MDhash(MDhandle h, char *s)
{
	int len;
	char *t;
	unsigned int v, c;

	len = strlen(s);
	t = s + len - 2;
	if (len > 2 && *t == '.' && (t[1] == 'h' || t[1] == 'a' || t[1] == 'o'))
		*t = '\0';
	else
		t = 0;
	for (v = 0; (c = *s) != 0; s++)
		v += (v << 1) ^ c;
	if (t)
		*t = '.';
	return &h->table[v & h->hashmask];
}

/*
 * Look for a dependency in h, returning a pointer to a free hash table entry
 * if dependency is not found.  Probe linearly in case of collision, but link
 * colliding entries so future probes are faster.
 */
static Dependency *
MDlookup(MDhandle h, char *dependency)
{
	Dependency *d, *prevd;

	for (d = MDhash(h, dependency); d->name; d = d->next) {
		if (!strcmp(d->name, dependency))
			break;
		if (d->next == 0) {
			prevd = d;
			do {
				if (++d > &h->table[h->hashmask])
					d = &h->table[0];
			} while (d->name);
			prevd->next = d;
			break;
		}
	}
	return d;
}

/*
 * Grow the table by doubling its size (does not assume freecount is zero).
 */
static void
MDgrow(MDhandle h)
{
	int n, m;
	Dependency *table, *oldtable;
	Dependency *d, *e;

	n = h->hashmask + 1;
	m = 2 * n;
	table = (Dependency *) calloc(m, sizeof *h->table);
	if (table == 0) {
		h->error("can't reallocate memory for %s update", h->filename);
		exit(1);
	}

	h->freecount += FREECOUNT(n);
	h->hashmask = m - 1;
	oldtable = h->table;
	h->table = table;
	for (d = oldtable; --n >= 0; d++) {
		if (d->name == 0)
			continue;
		e = MDlookup(h, d->name);
		e->name = d->name;
	}
	free(oldtable);
}

/*
 * Allocate a table for INITIALSIZE entries.
 */
MDhandle
MDopen(const char *toolname, const char *filename, const char *target, void (*error)(const char*,...))
{
	MDhandle h;
	Dependency *table;

	h = (mdrule *) malloc(sizeof *h);
	MDnewstab(h); 
	table = (Dependency *) calloc(INITIALSIZE, sizeof *h->table);
	if (h == 0 || table == 0) {
		error("can't allocate memory for %s update", filename);
		if (h)
			free(h);
		if (table)
			free(table);
		return 0;
	}

	h->toolname = MDstrcpy(h,toolname);
	h->filename = MDstrcpy(h,filename);
	h->target   = MDstrcpy(h,target  );
	h->error = error;
	h->entries = 0;
	h->freecount = FREECOUNT(INITIALSIZE);
	h->hashmask = INITIALSIZE - 1;
	h->table = table;

	return h;
}

/*
 * Add a dependency to h's rule.
 */
void
MDupdate(MDhandle h, char *dependency)
{
	Dependency *d;

	if (h == 0)
		return;
	d = MDlookup(h, dependency);
	if (d->name)
		return;
	if (h->freecount == 0) {
		MDgrow(h);
		d = MDlookup(h, dependency);
	}

	d->name = MDstrcpy(h,dependency);
	--h->freecount;
	h->entries++;
}


/*
 *	MDparseline
 *
 *	Examines a line in a dependecy file, sets pointer to the 
 *	target, and tool, and returns a pointer to the next line in the file.
 *	Any errors return a NULL pointer.
 *
 *	[Note: these are NOT null terminated strings.]
 *
 *	All makedepend lines look like:
 *
 *              target : dependency list #:tool\n
 *	or
 *              target : dependency list \n
 *	or
 *              blanks \n
 *	or
 *              # comment \n
 *	
 *	And after successsful parsing, we return pointers as follows:
 *
 *		target : dependency list #:tool\n
 *              ^                          ^     ^
 *	targ ---+                          |     |
 *	tool ------------------------------+     |
 *	nextline --------------------------------+
 *                              
 *
 *		target : dependency list \n
 *              ^                          ^
 *	targ ---+                          |
 *	tool = NULL                        |
 *	nextline --------------------------+
 *                              
 *
 *              blanks \n
 *                       ^
 *	targ = NULL      |
 *	tool = NULL      |
 *	nextline --------+
 *                              
 *              # comment \n
 *                          ^
 *	targ = NULL         |
 *	tool = NULL         |
 *	nextline -----------+
 *                              
 *
 *	For any error, we return pointers as follows:
 *	targ = <invalid>
 *	tool = <invalid>
 *	nextline = NULL
 *
 *
 */
#define SKIP_BLANKS 	while ( next<limit && *next==' ' ) {next++;} 
#define SKIP_TO_EOL 	while ( next<limit && *next!='\n') {next++;} 
#define	IS_EOL		(*next=='\n')
#define	IS_EOF		(next>=limit)
#define	SCAN_FOR(c)	while ( next<limit && *next!=c && *next!='\n') {next++;}

static
char *
MDparseline ( char* line, char *limit, char **targ, char **tool ) {
	char *next;

	/* init */
	*targ	= 0;
	*tool	= 0;
	next	= line;

	/*
	 *----------------------------------------------------------------
	 *	Process blank and/or comment lines
	 *----------------------------------------------------------------
	 */

	/* skip leading blanks */
	SKIP_BLANKS; if ( IS_EOF ) { return 0; }

	/* if this is a blank line then return pointer to nextline */
	if ( IS_EOL ) { return ++next; }
		
	/* if this is a comment line, then return pointer to nextline */
	if ( *next == '#') { 
		SKIP_TO_EOL;
		if ( IS_EOF ) { return 0; }
		if ( IS_EOL ) { return ++next; }
	}
	
	/*
	 *----------------------------------------------------------------
	 *	Process actual dependency line
	 *----------------------------------------------------------------
	 */

	/* target is always the beginning of the line	*/
	*targ = next;

	/* scan for the ':' to find the dependency list */
	SCAN_FOR(':'); if ( *next != ':' ) return 0;

	/* skip ':' and trailing blanks */
	next++; SKIP_BLANKS;

	/* scan for the optional '#:' to find the tool signature */
	SCAN_FOR('#'); if ( IS_EOF ) { return 0; }

	/* If we got to the end of line, the tool signature was not present */
	if ( IS_EOL ) { return ++next;}

	/* we found '#' so the next character better be ':' */
	if ( *++next != ':' ) return 0;

	/* we found ':', so skip over it and any trailing blanks */
	next++;
	SKIP_BLANKS; if ( IS_EOF ) { return 0; }

	/* we should be pointing at the toolname now; if there isn't
	 * one specified, we'll treat it as if there was no tool
	 * signature at all
	 */
	if ( IS_EOL ) { return ++next; }

	/* we must of found the toolname */
	*tool = next;

	/* scan for newline after tool */
	SKIP_TO_EOL;
	if ( IS_EOF ) { return 0; }

	/* skip over '\n'  and return pointer to next line */
	return ++next;

}

/*
 *	MDclose 
 *
 *	This is where all the work is actually done...
 *	Open, Lock, and Map the dependency file.  Scan for this target and 
 *	"remove" it. Write the target and dependencies at the end of the file.
 *
 *	WARNING 1:
 *		Be extremely careful whenever you change the order
 *		in which this code maps/locks/closes the dependecy
 *		file; because it may change the locking behavior.
 *
 *	WARNING 2:
 *		File-locking on NFS is often broken, which can lead
 *		to corrupted makedepend files.  This should be fixable
 *		if I could figure out how to mmap the Makedepend file
 *		and grow it, without doing this goofy re-open for append.
 *
 *	WARNING 3:
 *		Make/pmake/smake   do not use file locking on include
 *		files.  Because of this, it is possible for make to
 *		read a dependency file in the middle of an update and
 *		so look corrupted.
 *	
 *
 */
#define ERR(x)	{ h->error x ; goto cleanup; }
void
MDclose(MDhandle h, char *target)
{
	struct flock l;
	MDfile_t md;		/* current makedepend file		*/
	FILE	*file;		/* current makedepend file		*/

	char	*line;		/* line to be parsed			*/
	char	*nextline;	/* next line to be parsed		*/
	char	*targ;		/* target of the line being parsed	*/
	char	*tool;		/* tool signature of line being parsed	*/
	Dependency *d;		/* dependency to write out		*/
	int	targetlen;	/* length of target string		*/
	int	toolnamelen;	/* length of toolname string		*/
	int	found=0;	/* found a target match			*/
	int	linelen;	/* length of current line		*/
	int	remaininglen;	/* remaining length of file		*/


	ENTER("MDclose");

	/*
	 **********************************************************************
	 *	Is there work to be done?  
	 *	If not, release resources and return.
	 **********************************************************************
	 */
	if (h == 0) return;
	if (h->entries == 0) goto relspace;

	/*
	 **********************************************************************
	 *	Decide which target to use.
	 **********************************************************************
	 */
	if (target == 0) target = h->target;
	targetlen = strlen(target);

	/*
	 **********************************************************************
	 *	Setup file handle information
	 **********************************************************************
	 */
	MDfile_init ( &md ); md.filename  = strdup ( h->filename );

	/*
	 **********************************************************************
	 *	Open, lock, and stat the dependency file.
	 **********************************************************************
	 */
	LOG(("Opening %s\n",md.filename));
	md.f = open(md.filename, O_CREAT|O_RDWR, 0666); 
	if ( md.f < 0 ) ERR(("open (%s): %s", md.filename, strerror(errno)));

	l.l_type = F_WRLCK;
	l.l_whence = SEEK_SET;
	l.l_start = l.l_len = 0;
	if ( fcntl(md.f, F_SETLKW, &l) < 0 ) 
		ERR(("fcntl (%s) : %s", md.filename, strerror(errno)));
	if ( fstat(md.f,&md.stb) < 0 )
		ERR(("fstat (%s) : %s", md.filename, strerror(errno)));
	LOG(("Opening %s successful\n",md.filename));

	/*
	 **********************************************************************
	 *	Map the dependency file at base if it is non-empty.
	 **********************************************************************
	 */
	LOG(("Mapping %s\n",md.filename));
	md.base = md.limit = 0;
	if ( 0 < md.stb.st_size ) {
		md.base = (char *) mmap	( 0, md.stb.st_size, PROT_READ|PROT_WRITE
				, MAP_SHARED, md.f, 0);
		if ((long) md.base < 0) 
			ERR(("mmap (%s): %s", md.filename, strerror(errno)));
		md.size  = md.stb.st_size;
		md.limit = md.base + md.size;
	}
	LOG(("Mapping %s successful \n",md.filename));

	/*
	 **********************************************************************
	 *	Scan the dependency file for the target  and tool signature. 
	 *	
	 **********************************************************************
	 */
	 nextline=0;
	 found=0;
	 toolnamelen = strlen(h->toolname);
	 for ( line=md.base; line<md.limit; line=nextline ) {
		nextline = MDparseline(line, md.limit, &targ, &tool); 
		if ( 0 == nextline ) ERR(("%s: damaged file", md.filename));

		/* if blank line then contine scanning */
		if ( 0 == targ ) continue;

		/* if it's our target and tool then stop the search */
		if ( 0 == MDtargcmp(targ,target,targetlen) ) {

			/* if no tool signature specified, then no match */
			if ( 0 == tool ) { continue; }

			/* is it the same tool signature? */
			if (0 == MDtoolcmp(tool,h->toolname,toolnamelen)) {
				found = 1;
				break;
			}
		}
	 }

	/*
	 **********************************************************************
	 *	If we found the target and tool signature, then remove it by 
	 *	"sliding" the rest of the file down over the record we are 
	 *	removing and then truncating the file.
	 *
	 *	target: dependency list#:tool\n target: dep.. \n
	 *	^                        ^      ^               ^
	 *	+- line                  +-tool +- nextline     +- limit
	 *
	 **********************************************************************
	 */
	if ( found ) {
		LOG(("Found target=%s\n",target));

		linelen		= nextline - line;
		remaininglen	= md.limit - nextline;
		memmove ( /* to */ line, /*from*/ nextline, remaininglen );

		LOG(("Truncating %s by 0x%08x bytes\n",md.filename,linelen));

		if ( 0 != ftruncate(md.f, (md.size-linelen) ) ) 
		   ERR(("ftruncate (%s): %s", md.filename, strerror(errno)));
	}	


	/*
	 **********************************************************************
	 *	Open the dependency file in append mode,
	 *	write new record to end of new dependency file,
	 *	and close the file.
	 **********************************************************************
	 */
	LOG(("Write final record to %s\n",md.filename));
	if ( 0 == (file = fdopen(md.f,"a"))) 
		ERR(("fdopen(%s): %s", md.filename, strerror(errno)));

	fprintf ( file, "%s:", target );
	for (d=&h->table[0]; d<= &h->table[h->hashmask]; d++) {
		if (d->name) { fprintf( file, " %s", d->name ); }
	}
	if ( 0 > fprintf( file, " #:%s\n", h->toolname ) ) 
		ERR(("fprintf(%s): %s", md.filename, strerror(errno)));

cleanup:
	if ( file 	!= 0		) { fclose(file); }
	if ( md.base	> (char*)0	) { munmap(md.base, md.stb.st_size ); }
	if ( md.f	> 0 		) { close(md.f); }

relspace:
	free(h->table);
	free(h->stab);
	free(h);
	EXIT("MDclose");
}

