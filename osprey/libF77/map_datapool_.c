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
** NAME
** 	map_datapool.c - mmap executable's datapool section to file
**
** C SYNOPSIS
**	int map_datapool_(const char* datapoolname, char* dpaddress, int* dplength )
**	
** FORTRAN SYNOPSIS
**	integer*4 map_datapool(datapool, dpaddress, dplength)
**	character*n datapool	! n is the number of characters in string
**	character*(*) dpaddress
**	integer*4 dplength
**	
**	
** DESCRIPTION
**	map_datapool will use mmap to map the specified datapool
**	for sharing the data 
**
**	map_datapool is called from the .init section of each datapool DSO
**
** SEE ALSO
**	ld(2), mmap(2)
*/

#include <fcntl.h>
#include <sys/mman.h>
#include <stdio.h>
#include <limits.h>
#include <sys/types.h>
#include <unistd.h>
#include <alloca.h>
 
#define MAX_DP	100
static int *count_ptr[MAX_DP];
static char fn_save[MAX_DP][PATH_MAX];
static int ndp = 0;

static    char *cpath;
static struct flock flk;
static	int ltype;
extern int errno;

int DEBUG_MAP_LOCALDATA = 0;		/* Set this to 1 to debug local data */

/*
** Fortran interface for f77_map_datapool_.
** The following arguments are used:
** dpname	= name of the datapool
** dp_address	= address of the first datapool item (not counting the filler)
** dp_length	= length of the datapool
*/
void
f77_map_datapool_(const char* dpname, char *dp_address, int dp_length)
{
    int length;
    char *getenv(), *dp_dir;
    int plen;
    int fd;
    int pagesize = sysconf(_SC_PAGESIZE);
    int plength, poff, nusers;
   
    length = strlen(dpname);
    if (dp_dir = getenv("DATAPOOL_DIR")) {
	plen = strlen(dp_dir);
        cpath = (char*)alloca(plen + length + 2);
	strcpy( cpath, dp_dir );
	if (dp_dir[plen-1] != '/') cpath[plen++] = '/';
    }
    else {
        cpath = (char*) alloca(length + 10);
	strcpy( cpath, "/usr/tmp/" );
	plen = 9;
    }
    *(cpath+plen++) = 'D';
    *(cpath+plen++) = 'P';
    *(cpath+plen++) = '_';
    strncpy(cpath+plen, dpname, length + 1);
    cpath[plen+length] = '\0';
    strcpy(fn_save[ndp], cpath);

    dp_address -= sizeof( int );	/* make sure there is space for the user count */
    poff = (long)dp_address % pagesize;
    dp_address -= poff;		/* adjust for page offset */
    plength = dp_length + poff + sizeof( int );

    if ((fd = open(cpath, O_RDWR | O_CREAT, 0666)) == -1)
    {
	fprintf(stderr,
		"map_localdata_: cannot open file %s for mmap\n", cpath );
	abort();
    }

    if (DEBUG_MAP_LOCALDATA)
        printf("mmap address = %x  , length = %d\n", dp_address, plength );
    if (mmap(dp_address, plength, PROT_READ | PROT_WRITE,
	    MAP_AUTOGROW | MAP_SHARED | MAP_FIXED,
	    fd, 0) == (caddr_t) -1)
    {
	fprintf(stderr,
		    "map_datapool: trouble sharing %s at address %X\n",
		    cpath, dp_address);
	perror("map_datapool (mmap)");
	fflush(stderr);
	close(fd);
	abort();
    }
    else
    {
	if (DEBUG_MAP_LOCALDATA)
	{
	    fprintf(stderr, "map_datapool: sharing file %s at address %X\n",
		    cpath, dp_address);
	    fflush(stderr);
	}
    }

    count_ptr[ndp] = (int *) dp_address;

    while ((nusers = _test_and_set( count_ptr[ ndp ], -1 )) < 0)
	;
    *count_ptr[ ndp++ ] = ++nusers;
}

/*
** Fortran interface for f77_map_datapool_pad_.
** The following arguments are used:
** dpname	= name of the datapool
** dp_address	= address of the first datapool item (not counting the filler)
** dp_length	= length of the datapool
** pagesize 	= page size specified by the user
*/
void
f77_map_datapool_pad_(const char* dpname, char *dp_address, int dp_length,
int pagesize)
{
    int length;
    char *getenv(), *dp_dir;
    int plen;
    int fd;
    int plength, poff, nusers;
   
    length = strlen(dpname);
    if (dp_dir = getenv("DATAPOOL_DIR")) {
	plen = strlen(dp_dir);
        cpath = (char*)alloca(plen + length + 2);
	strcpy( cpath, dp_dir );
	if (dp_dir[plen-1] != '/') cpath[plen++] = '/';
    }
    else {
        cpath = (char*) alloca(length + 10);
	strcpy( cpath, "/usr/tmp/" );
	plen = 9;
    }
    *(cpath+plen++) = 'D';
    *(cpath+plen++) = 'P';
    *(cpath+plen++) = '_';
    strncpy(cpath+plen, dpname, length + 1);
    cpath[plen+length] = '\0';
    strcpy(fn_save[ndp], cpath);

    dp_address -= sizeof( int );	/* make sure there is space for the user count */
    poff = (long)dp_address % pagesize;
    dp_address -= poff;		/* adjust for page offset */
    plength = dp_length + poff + sizeof( int );

    if ((fd = open(cpath, O_RDWR | O_CREAT, 0666)) == -1)
    {
	fprintf(stderr,
		"map_localdata_: cannot open file %s for mmap\n", cpath );
	abort();
    }

    if (DEBUG_MAP_LOCALDATA)
        printf("mmap address = %x  , length = %d\n", dp_address, plength );
    if (mmap(dp_address, plength, PROT_READ | PROT_WRITE,
	    MAP_AUTOGROW | MAP_SHARED | MAP_FIXED,
	    fd, 0) == (caddr_t) -1)
    {
	fprintf(stderr,
		    "map_datapool: trouble sharing %s at address %X\n",
		    cpath, dp_address);
	perror("map_datapool (mmap)");
	fflush(stderr);
	close(fd);
	abort();
    }
    else
    {
	if (DEBUG_MAP_LOCALDATA)
	{
	    fprintf(stderr, "map_datapool: sharing file %s at address %X\n",
		    cpath, dp_address);
	    fflush(stderr);
	}
    }

    count_ptr[ndp] = (int *) dp_address;

    while ((nusers = _test_and_set( count_ptr[ ndp ], -1 )) < 0)
	;
    *count_ptr[ ndp++ ] = ++nusers;
}


void
f77_unmap_datapool_()
{
    /* This function determines whether the mmap file in /usr/tmp/DP_name
    should be removed or not by examining the number of users currently
    accessing the file denoted in file /usr/tmp/DP_name.users.  If it
    is the last person accessing the file then it will clean up that
    /usr/tmp/DP_name* files.  Although the unmapping might not have
    the exact one-to-one correspondence to the mapping that does not
    really matter since at the end of the processes, ALL datapools that
    have been mapped will be unmapped and which one gets unmapped first
    does not matter.
    */
    int nusers;

    ndp--;
    while ((nusers = _test_and_set( count_ptr[ ndp ], -1 )) < 0)
	;
    *count_ptr[ ndp ] = --nusers;
    
    /* no more users delete this file */
    if (!nusers) {
        unlink( fn_save[ndp] );
    }
}
