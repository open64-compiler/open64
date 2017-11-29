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


/* $Header: /proj/osprey/CVS/open64/osprey1.0/libI77/ioutil.c,v 1.1.1.1 2005/10/21 19:00:00 marcel Exp $ */
/*	3.0 SID #	1.2	*/
#include <cmplrs/fio.h>
#include <mutex.h>
#include <cmplrs/f_errno.h>
#include <unistd.h>
#include <fcntl.h>
#include "iomode.h"
#include "util.h"
#include "fio_direct_io.h"
#include "bcompat.h"

static FILE *
get_ufd(ftnint luno, int *direct_unformatted)
{
	XINT64 loc;
	unit *u = find_luno(luno);
	FILE *fstat;

	if (!u) {
	    errno = F_ERUNIT;
	    return(NULL);
	}
	if (u->uacc == DIRECT && !u->ufmt) {
	    *direct_unformatted = 1;
            _fio_du_flush((int) u->ufd);
	} else {
	    *direct_unformatted = 0;
	    fflush(u->ufd);
	    if (!(u->uwrt & RW_FILE) && !u->ureadonly) {
	    /* 
	    reopen the file as read/write file so it won't be
	    reopened again and lose its lock
	    */
	    	loc = FTELL (u->ufd);
                /* obtain exclusive lock for special I/O operation */
                while (test_and_set( &io_lock, 1L ))
                  ;
                fstat = freopen (u->ufnm, "r+", u->ufd);
                if (fstat) {
		    FSEEK (u->ufd, loc, SEEK_SET);
		    u->uwrt |= RW_FILE;
                    io_lock = 0;
	        }
		else {
		    /* Have to restore the file to its original state
		    since the failed freopen() call would have messed
		    up the state of the file and it can't be used again
		    */
		    if (u->uwrt & WR_OP) {
                       /* obtain exclusive lock for special I/O operation */
                       fstat = freopen (u->ufnm, "a", u->ufd);
                       io_lock = 0;
                       if (fstat == 0) {
		            return(NULL);
		       }
		    } else {
                       /* obtain exclusive lock for special I/O operation */
                       fstat = freopen (u->ufnm, "r", u->ufd);
                       io_lock = 0;
                       if (fstat == 0)
		            return(NULL);
		    }
		    FSEEK (u->ufd, loc, SEEK_SET);
		}
	    }
	}
	return(u->ufd);
}


static __int32_t
get_fd(ftnint luno)
{
	FILE *ufd;
	int  direct_unformatted;

	if ((ufd = get_ufd(luno, &direct_unformatted)) == 0 && !direct_unformatted)
	    return(-errno);
	if (direct_unformatted)
	    return((int) ufd);
	else
	    return(fileno(ufd));
}


extern __int32_t 
fcntl_( __int32_t *fd, __int32_t *locktype, struct flock *fl)
{
	if (*locktype == F_DUPFD || *locktype ==F_SETFL ) {
	    if (fcntl( *fd, *locktype, * (int *) fl ))
		return(errno);
	}
	else if (fcntl( *fd, *locktype, fl ))
	    return(errno);
	return(0);
}


extern __int32_t 
lockf_( __int32_t *luno, __int32_t *func, size_t *size)
	/*
	**  This function assumes a 32-bit 'size' in all cases instead of
	**  a 'long' so it can have a fixed Fortran interface regardless
	**  of whether it is compiled with -64bit or not
	*/
{
	int fd;
	   /* will automatically promote *luno to XINT */
	if ((fd = get_fd(*luno)) < 0)
	    return( -fd );

	if (lockf( fd, *func, (long) *size ))
		return(errno);
	return(0);
}

#ifdef SIZEOF_LUNO_IS_64
extern __int32_t 
lockf64_( XINT *luno, __int32_t *func, size_t *size)
	/*
	**  This function assumes a 32-bit 'size' in all cases instead of
	**  a 'long' so it can have a fixed Fortran interface regardless
	**  of whether it is compiled with -64bit or not
	*/
{
	int fd;
	
	if ((fd = get_fd(*luno)) < 0)
	    return( -fd );

	if (lockf( fd, *func, (long) *size ))
		return(errno);
	return(0);
}
#endif

extern int
setbuf_( __int32_t *luno, char *buf )
{
	FILE *ufd;
	int  direct_unformatted;
	
	if ((ufd = get_ufd(*luno, &direct_unformatted)) == 0 && !direct_unformatted)
	    return( errno );
	/* direct unformatted files are already unbuffered */
	if (direct_unformatted) return (0);
	setbuf( ufd, buf );
	return(0);
}

#ifdef SIZEOF_LUNO_IS_64
extern int
setbuf64_( XINT *luno, char *buf )
{
	FILE *ufd;
	int  direct_unformatted;
	
	if ((ufd = get_ufd(*luno, &direct_unformatted)) == 0 && !direct_unformatted)
	    return( errno );
	/* direct unformatted files are already unbuffered */
	if (direct_unformatted) return (0);
	setbuf( ufd, buf );
	return(0);
}
#endif

extern int
setvbuf_( __int32_t *luno, char *buf, int *type, size_t *size)
{
	FILE *ufd;
	int  direct_unformatted;
	
	if ((ufd = get_ufd(*luno, &direct_unformatted)) == 0 && !direct_unformatted)
	    return( errno );
	/* direct unformatted files are already unbuffered */
	if (direct_unformatted)
	    return (0);
	if (setvbuf( ufd, buf, *type, *size ))
	    return(errno);
	return(0);
}

#ifdef SIZEOF_LUNO_IS_64
extern int
setvbuf64_( XINT *luno, char *buf, int *type, size_t *size)
{
	FILE *ufd;
	int  direct_unformatted;
	
	if ((ufd = get_ufd(*luno, &direct_unformatted)) == 0 && !direct_unformatted)
	    return( errno );
	/* direct unformatted files are already unbuffered */
	if (direct_unformatted)
	    return (0);
	if (setvbuf( ufd, buf, *type, *size ))
	    return(errno);
	return(0);
}
#endif

extern int
setbuffer_( __int32_t *luno, char *buf, int *size )
{
	FILE *ufd;
	int  direct_unformatted;
	
        if ((ufd = get_ufd(*luno, &direct_unformatted)) == 0 && !direct_unformatted)
            return( errno );
        /* direct unformatted files are already unbuffered */
        if (direct_unformatted) return (0);
        if (setbuffer( ufd, buf, *size ))
            return(errno);
        return(0);
}

#ifdef SIZEOF_LUNO_IS_64
extern int
setbuffer64_( XINT *luno, char *buf, int *size )
{
	FILE *ufd;
	int  direct_unformatted;
	
        if ((ufd = get_ufd(*luno, &direct_unformatted)) == 0 && !direct_unformatted)
            return( errno );
        /* direct unformatted files are already unbuffered */
        if (direct_unformatted) return (0);
        if (setbuffer( ufd, buf, *size ))
            return(errno);
        return(0);
}
#endif

extern int
setlinebuf_( __int32_t *luno )
{
	FILE *ufd;
	int  direct_unformatted;
	
        if ((ufd = get_ufd(*luno, &direct_unformatted)) == 0 && !direct_unformatted)
            return( errno );
        /* direct unformatted files are already unbuffered */
        if (direct_unformatted) return (0);
        if (setlinebuf( ufd ))
            return(errno);
	return(0);
}

#ifdef SIZEOF_LUNO_IS_64
extern int
setlinebuf64_( XINT *luno )
{
	FILE *ufd;
	int  direct_unformatted;
	
        if ((ufd = get_ufd(*luno, &direct_unformatted)) == 0 && !direct_unformatted)
            return( errno );
        /* direct unformatted files are already unbuffered */
        if (direct_unformatted) return (0);
        if (setlinebuf( ufd ))
            return(errno);
	return(0);
}
#endif

extern __int32_t
maplun_( int *luno )
{
	/* 
	** Returns integer file descriptor for a Fortran logical unit
	*/
	/*
	** Have to call get_fd() instead of find_luno() since
	** the file has to be set to "r+" mode so the user can
	** either read from or write to it
	*/
	return(get_fd(*luno));
}

#ifdef SIZEOF_LUNO_IS_64
extern __int32_t
maplun64_( XINT *luno )
{
	/* 
	** Returns integer file descriptor for a Fortran logical unit
	*/
	/*
	** Have to call get_fd() instead of find_luno() since
	** the file has to be set to "r+" mode so the user can
	** either read from or write to it
	*/
	return(get_fd(*luno));
}
#endif
