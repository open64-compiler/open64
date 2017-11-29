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



#include <stdio.h>
#include <stdlib.h>
#include <mutex.h>
#include <assert.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <string.h>
#include <errno.h>
#include <cmplrs/fio.h>
#include "fio_direct_io.h"
#include <cmplrs/f_errno.h>

typedef struct {
   /* length of the file when first opened */
   XINT64             open_l_file;	
   /* last byte address written to */
   XINT64             actual_l_file; /* true file length */
   XINT64             sector_l_file; /* written out length in sector multiple */
   unsigned long   mode;        /* mode of the file           */
   char           *buffer;	/* byte address of the buffer */
   XINT             l_buffer;	/* byte length of the buffer  */
   /* byte length of the data, relative to the start of the buffer */
   XINT             l_data;
   /* byte address of disk which corresponds to the start of the
    * buffer */
   XINT64             l_disk;
   int             flags;	/* flags, see below           */
   int             sector;      /* one disk sector in bytes   */
   long		   lseek;
   long		   position;
} file_info;

/* Underscores for macros.  Data structure in macros, in case the */
/* data structure changes. :^)                                    */

#define _Open_l_file  Direct[fd].open_l_file	/* file length when opened */
#define _Actual_l_file Direct[fd].actual_l_file	/* actual file length */
#define _Sector_l_file Direct[fd].sector_l_file
#define _Mode         Direct[fd].mode
#define _Buffer       Direct[fd].buffer
#define _L_buffer     Direct[fd].l_buffer
#define _L_data       Direct[fd].l_data
#define _L_disk       Direct[fd].l_disk
#define _Flags        Direct[fd].flags
#define _Sector       Direct[fd].sector
#define _Seek_pos     Direct[fd].lseek
#define _Current_pos  Direct[fd].position

#define SMALL_DATA_ITEM	512

static file_info       *Direct = NULL;
static int       	Direct_size = 0;

enum {
   NEW,
   OLD,
   UNKNOWN
};

enum {
   FALSE,
   TRUE
};

#define EMPTY_BIT       0x1
#define CLEAR_EMPTY_BIT {Direct[fd].flags &= (~EMPTY_BIT);}
#define SET_EMPTY_BIT   {Direct[fd].flags |= EMPTY_BIT;}
#define IS_EMPTY        (Direct[fd].flags &  EMPTY_BIT)

#define FLUSH_BIT       0x2
#define CLEAR_FLUSH_BIT {Direct[fd].flags &= (~FLUSH_BIT);}
#define SET_FLUSH_BIT   {Direct[fd].flags |= FLUSH_BIT;}
#define MUST_FLUSH      (Direct[fd].flags &  FLUSH_BIT)

#define DIRECT_BIT       0x4
#define CLEAR_DIRECT_BIT {Direct[fd].flags &= (~DIRECT_BIT);}
#define SET_DIRECT_BIT   {Direct[fd].flags |= DIRECT_BIT;}
#define IS_DIRECT        (Direct[fd].flags &  DIRECT_BIT)

#define max(a,b)        (((a)>(b))?(a):(b))
#define min(a,b)        (((a)<(b))?(a):(b))
#define DEFAULT   (64*1024)	/* default buffer size                       */

static int d_mem;
static int d_maxiosz;

#define MILLION   (1024*1024)
#define CEILING   MILLION       /* Current system limitation of 1Mbyte for   */
				/* read and writes of data                   */
#define CROSSOVER (MILLION/2)	/* buffer size where O_DIRECT should
				 * be used */
#define	LARGE_CHUNK	(64*1024)

/* Routines for Fortran Direct Unformatted I/O. */


XINT
get_buffer_size()
{
   int bufsiz;
   char           *p;		/* FORTRAN_BUFFER_SIZE string */

   bufsiz = DEFAULT;		/* default buffer size */
   if ((p = getenv ("FORTRAN_BUFFER_SIZE")) != NULL) {
      bufsiz = atol (p)*4; /* user specifies words! */
   }
   return (bufsiz);
}

/*PROCEDURE*****************************************************************/

int _fio_du_open(char *file_name, char *type, int dupopen, int dup_fd )

/***************************************************************************
*
* PURPOSE:
*    Opens a Fortran file for direct unformatted reads/writes.
*
* INPUT:
*    file_name, char *, name of the file to open
*    type, char *, type for open (from fopen)
*
* OUTPUT:
*    function return, file descriptor for open'ed file
*
* ASSUMPTIONS:
*    Assumes _fio_du_open called first, followed by any number of 
*    _fio_du_read/write calls, followed by a call to _fio_du_close.
*
*    The buffer length is decided with the following priority:
*       1) user specified buffer size on OPEN statement
*       2) environmental variable FORTRAN_BUFFER_SIZE
*       3) default
*
* HISTORY:
*    05/05/93 - LMS - Initial Coding.
*
*****************************************************************************/

{ /* BEGIN _fio_du_open */
   
#define MILLION   (1024*1024)
#define CEILING   MILLION       /* Current system limitation of 1Mbyte for   */
   /* read and writes of data                   */
   
   int             fd;		/* file descriptor            */
   struct dioattr  dioattr;	/* direct I/O attributes      */
   char           *buffer;	/* pointer to the I/O buffer  */
   XINT             l_buffer;	/* length of the buffer       */
   int             oflag;	/* oflag for open             */
   struct STAT_TAG     stat;        /* data structure for fstat   */
   
   /* 
    * Open the file in accord with the type. 
    */

   if ((strcmp (type, "r") == 0) || (strcmp (type, "rb") == 0)) {

      /* 
       * Don't use O_CREAT for read-only file since NFS doesn't like it
       * and since it doesn't allow the Fortran OPEN statement to return
       * an error when trying to open a non-existent read-only file.
       */

      oflag = O_RDONLY;

   } else if ((strcmp (type, "w") == 0) || (strcmp (type, "wb") == 0)) {
      oflag = O_WRONLY | O_CREAT;

   } else if ((strcmp (type, "a") == 0) || (strcmp (type, "ab") == 0)) {
      oflag = O_APPEND | O_CREAT;

   } else if ((strcmp (type, "r+") == 0) || (strcmp (type, "r+b") == 0)
	      || (strcmp (type, "rb+"))) {
      oflag = O_RDWR | O_CREAT;

   } else if ((strcmp (type, "w+") == 0) || (strcmp (type, "w+b") == 0)
	      || (strcmp (type, "wb+"))) {
      oflag = O_TRUNC | O_WRONLY | O_CREAT;

   } else if ((strcmp (type, "a+") == 0) || (strcmp (type, "a+b") == 0)
	      || (strcmp (type, "ab+"))) {
      oflag = O_APPEND | O_WRONLY | O_CREAT;

   } else {			/* bad type */
      return (-1);
   }

   l_buffer = get_buffer_size();	/* default buffer size */
   if (l_buffer >= CROSSOVER)  
      oflag |= O_DIRECT;

   /* obtain exclusive lock for special I/O operation */
   if (!dupopen) {
      fd = open (file_name, oflag, 0666);
      if (fd == -1) {

      /*
       * Try again without the O_DIRECT flag. An NFS mounted file
       * will not open with the O_DIRECT flag, so in this case
       * we remove the optimization.
       */
      
         oflag &= (~O_DIRECT);
         fd = open (file_name, oflag, 0666);
         if (fd == -1) return (fd);
      }
   }
   else
      fd = dup_fd;

   if (!Direct) {
     if ((Direct = malloc( sizeof(*Direct) * mxunit )) == NULL) {
       errno = 113;
       return(-1);
     }
     memset(Direct,0,(size_t)(sizeof(*Direct) * mxunit ));
     Direct_size = mxunit;
   }
   else if (fd >= Direct_size) {
     if (Direct_size < mxunit) {
       if ((Direct = realloc( Direct, sizeof(*Direct)* mxunit )) == NULL) {
         errno = 113;
         return(-1);
       }
       memset((Direct+Direct_size),0,(size_t)(sizeof(*Direct) * (mxunit - Direct_size)));
       Direct_size = mxunit;
     } else {
       /* This should never happen since map_luno() would be called
       ** first and mxunit would have been increased before we get here.
       ** But, just in case, this should still work.
       */
       if ((Direct = realloc( Direct, sizeof(*Direct)*(Direct_size += mxunit) )) == NULL) {
	 errno = 113;
	 return(-1);
       }
     }
  }

   /*
    * Determine the length and mode of the file.
    */

   if (FSTAT(fd, &stat) == -1) {
      /* obtain exclusive lock for special I/O operation */
      close (fd);
      return (-1);
   } else { 
      _Mode         = stat.st_mode;
      _Open_l_file  = stat.st_size;
      _Actual_l_file = stat.st_size;
      _Sector_l_file = 0;
      _Current_pos  = 0;
   }

   /* 
    * Quiry the buffer minimum/maximum size and boundary constraint. 
    */

   _Sector =  512;		/* default sector size */
   d_maxiosz = CROSSOVER;
   CLEAR_DIRECT_BIT;
   if (oflag & O_DIRECT) {
         if (fcntl (fd, F_DIOINFO, &dioattr) != -1) {
            /*
             * Determine the correct size of the buffer.
             */
            SET_DIRECT_BIT;
            assert(dioattr.d_miniosz > 0);	/* Buffer minimum.  */
            assert(dioattr.d_maxiosz > 0);	/* Buffer maximum.  */
   
            assert(dioattr.d_maxiosz>=dioattr.d_miniosz); 
            d_mem = dioattr.d_mem;
            _Sector = (int) dioattr.d_miniosz;
            d_maxiosz = (int) dioattr.d_maxiosz;
         }
   }
   if (_Sector > d_mem)
      d_mem = _Sector; 
   /* Limit the length of the buffer to d_maxiosz */
   if (l_buffer > d_maxiosz) l_buffer = d_maxiosz;
   if (l_buffer < _Sector) l_buffer = _Sector;
   
   /* Set the size of the buffer to multiple of _Sector. */

   if (oflag & O_DIRECT)
      l_buffer = (l_buffer / (int) _Sector) * (int) _Sector;

   /* Allocate a buffer on correct boundary. */

   if ((buffer = (char *) memalign (d_mem, l_buffer)) == NULL) {
      errno = 113;
      return(-1);
   }

   /* Fill out the information into the Direct array. */

   _Buffer = buffer;
   _L_buffer = l_buffer;
   SET_EMPTY_BIT;
   return (fd);
}				/* END _fio_du_open */

/*PROCEDURE*****************************************************************/

int _fio_du_write(unit *ftnunit, char *data, XINT n_data, XINT64 disk_loc, int fd)

/*************************************************************************
*
* PURPOSE:
*    Writes to a Fortran file for direct unformatted data.
*
* INPUT:
*    data, char *, pointer to the data to write
*    n_data, int, length in bytes of data
*    disk_loc, int, location on disk to write data
*    fd, int, file descriptor of file to write to 
*
* OUTPUT:
*    function return value, int, -1 if error, fd if OK
*
* ASSUMPTIONS:
*    Assumes _fio_du_open called first, followed by any number of
*    _fio_du_read/write calls, followed by a call to _fio_du_close.
*
* HISTORY:
*    05/05/93 - LMS - Initial Coding.
*
*****************************************************************************/

{ /* BEGIN _fio_du_write */

   XINT             n_data_to_move;
   char           *loc_in_buffer;
   XINT             sector_up;

   if (ftnunit->uacc != DIRECT) return(-1);
   if (!(_Mode & S_IWUSR)) return (-1); /* not user writable */

   if (disk_loc+n_data > _Actual_l_file)
      _Actual_l_file = disk_loc+n_data;

   while (n_data > 0) {
      if (IS_DIRECT) {
	 /* For large data which is correctly aligned for direct I/O
	 ** try another approach to get the performance level of C
	 */
	 int dirty_buff = !IS_EMPTY && (disk_loc < _L_disk + _L_data)
			&& (disk_loc + n_data >= _L_disk);

	 if (((long)data % d_mem) == 0 
	     && (disk_loc % _Sector) == 0
	     && (n_data >= CROSSOVER 
		 || (n_data >= LARGE_CHUNK && !dirty_buff))) 
	 {
	    int oddbytes;
	    if (dirty_buff) {
	       if (disk_loc >= _L_disk && disk_loc < _L_disk + _L_buffer) {
		  /* beginning of data is in buffer */
		  int ncopy = _L_disk + _L_buffer - disk_loc;
		  if (ncopy > n_data) 
		     ncopy = n_data;
		  memcpy( _Buffer + disk_loc - _L_disk, data, ncopy );
		  if (_L_data < disk_loc -_L_disk + ncopy)
		     _L_data = disk_loc - _L_disk + ncopy;
		  data += ncopy;
		  n_data -= ncopy;
		  disk_loc += ncopy;
		  SET_FLUSH_BIT;
	       }
	       else if (disk_loc + n_data >= _L_disk && disk_loc + n_data < _L_disk + _L_buffer) {
		  /* trailing end of data is in buffer */
		  int ncopy = disk_loc + n_data - _L_disk;
		  memcpy( _Buffer, data + _L_disk - disk_loc, ncopy );
		  if (_L_data < ncopy)
		     _L_data = ncopy;
		  n_data -= ncopy;
		  SET_FLUSH_BIT;
	       }
	       else {
	          SET_EMPTY_BIT;  /* buffer data is dirty and unusable */
	       }
            }
	    oddbytes = n_data % _Sector;
	    if (-1 == LSEEK (fd, disk_loc, SEEK_SET)) return (-1);
	    while (n_data >= d_maxiosz) {
	       if (d_maxiosz != write (fd, data, d_maxiosz))
		  return(-1);
	       data += d_maxiosz;
	       n_data -= d_maxiosz;
	       disk_loc += d_maxiosz;
	    }
	    if (n_data > oddbytes) {
	       int nwrite = n_data - oddbytes;
	       if (nwrite != write (fd, data, nwrite))
		  return(-1);
	       data += nwrite;
	       disk_loc += nwrite;
	    }
	    n_data = oddbytes;
	    _Current_pos = disk_loc;
	    continue;
	 }
         if (IS_EMPTY) {
	 /* For empty direct I/O file need to load the buffer with
	 ** data from the disk
	 */
	    _L_disk = (disk_loc / _Sector) * _Sector;
	    if (-1 == LSEEK (fd, _L_disk, SEEK_SET)) return (-1);
	    _L_data = read (fd, _Buffer, _L_buffer);
	    if (_L_data == -1) {
		/*
		 * Read failed, so lets try it again in "normal" mode,
		 * then switch back to O_DIRECT. This is needed to
                 * get around a bug in O_DIRECT where the last partial
		 * sector of a file isn't readable.
		 */

	        if (-1 == LSEEK (fd, _L_disk, SEEK_SET)) return (-1);
	        if (-1 == fcntl(fd, F_SETFL, (fcntl(fd, F_GETFL)&(~FDIRECT)))) 
		   return (-1);
	        if (-1 == (_L_data = read (fd, _Buffer, _L_buffer))) return(-1);
	        if (-1 == fcntl(fd, F_SETFL, fcntl(fd, F_GETFL) | FDIRECT)) 
		   return (-1);
            } else if (disk_loc > _Open_l_file) {
	       /* I don't know why there's no error from either lseek()
	       or read() when jumping beyond EOF and trying to read from
	       it.  Anyway, when that happens we need to make sure that the
	       buffer is zeroed out so garbages won't go into the file
	       */
	       n_data_to_move = disk_loc - (_L_data + _L_disk);
	       memset( _Buffer+_L_data, 0, n_data_to_move );
	    }
	    CLEAR_EMPTY_BIT;
         }
      
         /* 
          * Start of data is inside the buffer?  This algorithm is
          * inefficient in the case where the data overlaps the front of
          * the buffer, what happens is the buffer is moved, missing the
          * opportunity to write part of the data from the buffer.  Not
          * wrong, just slow. 
          */

         if ((disk_loc >= _L_disk) && (disk_loc < _L_disk + _L_buffer)) {
	    n_data_to_move = (_L_disk + _L_buffer) - disk_loc;
	    n_data_to_move = (n_data < n_data_to_move) ? n_data : n_data_to_move;
	    loc_in_buffer = _Buffer + (disk_loc - _L_disk);
	    memcpy (loc_in_buffer, data, n_data_to_move);
	    _L_data = (((disk_loc + n_data_to_move) - _L_disk) > _L_data) ?
	       (disk_loc + n_data_to_move) - _L_disk :
	       _L_data;
	    data += n_data_to_move;
	    n_data -= n_data_to_move;
	    disk_loc += n_data_to_move;
	    _Current_pos = disk_loc;
	    if (n_data == 0) {
	       CLEAR_EMPTY_BIT;
	       SET_FLUSH_BIT;
	    } else {
	       /* The buffer is full, flush it */
	       if (-1 == LSEEK (fd, _L_disk, SEEK_SET)) return (-1);
	       sector_up = ((_L_data + _Sector - 1) / _Sector) * _Sector;
	       if (sector_up != write (fd, _Buffer, sector_up)) return (-1);
	       while (n_data > _L_buffer) {
		  memcpy( _Buffer, data, _L_buffer);
		  if (_L_buffer != write (fd, _Buffer, _L_buffer)) return (-1);
		  n_data -= _L_buffer;
	    	  data += _L_buffer;
	          disk_loc += _L_buffer;
	       }
	       _Current_pos = disk_loc;
	       CLEAR_FLUSH_BIT;
	    }
         } else {			/* Move the buffer. */
            if (MUST_FLUSH) {
	       _fio_du_flush( fd );
            }
	    else
	       SET_EMPTY_BIT;
         }
      }
      else if (n_data > LARGE_CHUNK && (IS_EMPTY || ((disk_loc < _L_disk) || (disk_loc > _L_disk + _L_data)))) {
         /* flush the remaining data in the direct I/O buffer, if any */
         if (-1 == _fio_du_flush (fd)) return (-1);
         /* 
         ** Don't fuss around with the buffer, when the record size is big
         ** for non-direct I/O but write straight from the user buffer.
         */
         if (ftnunit->f77recpos == n_data)
            /* 
            ** At the beginning of a record, make sure the file pointer is at the
            ** correct position
            */
            if (-1 == LSEEK (fd, disk_loc, SEEK_SET)) return (-1);
         if (n_data != write (fd, data, n_data)) return (-1);
	 n_data = 0;
         SET_EMPTY_BIT;
         CLEAR_FLUSH_BIT;
      }
      else {
         /* For small data items we still buffer them but we couldn't 
         ** care less about the alignment nor to round everything up to
         ** _Sector blocks which would require unnecessary read()
         */
	 if (IS_EMPTY) {
	    /* copy the small data to the top of the buffer */
	    int nwrite = (n_data < _L_buffer) ? n_data : _L_buffer;
            memcpy (_Buffer, data, nwrite);
            _L_disk = disk_loc;
            _L_data = nwrite;
	    disk_loc += nwrite;
            _Current_pos = disk_loc;
	    n_data -= nwrite;
	    if (n_data != 0) {
	       /* buffer is already full; flush it */
               if (-1 == LSEEK (fd, _L_disk, SEEK_SET)) 
		  return (-1);
               if (_L_data != write (fd, _Buffer, _L_data)) 
		  return (-1);
               CLEAR_FLUSH_BIT;
	       SET_EMPTY_BIT;
	    } else {
	       SET_FLUSH_BIT;
	       CLEAR_EMPTY_BIT;
	    }
         } else if ((disk_loc >= _L_disk) && (disk_loc <= _L_disk + _L_data)) {
	    /* the next data to be written is within the range of valid data */
            n_data_to_move = (_L_disk + _L_buffer) - disk_loc;
            n_data_to_move = (n_data < n_data_to_move) ? n_data : n_data_to_move;
            loc_in_buffer = _Buffer + (disk_loc - _L_disk);
            memcpy (loc_in_buffer, data, n_data_to_move);
            _L_data = (((disk_loc + n_data_to_move) - _L_disk) > _L_data) ?
               (disk_loc + n_data_to_move) - _L_disk : _L_data;
            data += n_data_to_move;
            n_data -= n_data_to_move;
            disk_loc += n_data_to_move;
            _Current_pos = disk_loc;
	    if (n_data != 0) {
	       /* buffer is already full; flush it */
               if (-1 == LSEEK (fd, _L_disk, SEEK_SET)) 
		  return (-1);
               if (_L_data != write (fd, _Buffer, _L_data)) 
		  return (-1);
               CLEAR_FLUSH_BIT;
	       SET_EMPTY_BIT;
	    } else {
	       SET_FLUSH_BIT;
	       CLEAR_EMPTY_BIT;
	    }
         } else {                  /* Move the buffer. */
	    /* writing a diferent region.  Need to flush the current data
	    ** in buffer if any
	    */
            if (MUST_FLUSH) {
               if (-1 == LSEEK (fd, _L_disk, SEEK_SET)) return (-1);
               if (_L_data != write (fd, _Buffer, _L_data)) return (-1);
               CLEAR_FLUSH_BIT;
            }
            SET_EMPTY_BIT;
         }
      }
   }
   return (fd);
} /* END _fio_du_write */

/*PROCEDURE*****************************************************************/

int _fio_du_flush(fd)
   int             fd;

/*************************************************************************
*
* PURPOSE:
*    Flushed the data to a Fortran direct unformatted file.
*
* INPUT:
*    fd, int, file descriptor of file to write to
*
* OUTPUT:
*    function return value, int, -1 if error, fd if OK
*
* ASSUMPTIONS:
*    Assumes _fio_du_open called first, followed by any number of
*    _fio_du_read/write calls, followed by a call to _fio_du_close.
*
* HISTORY:
*    05/05/93 - LMS - Initial Coding.
*
*****************************************************************************/

{ /* BEGIN _fio_du_flush */

   XINT             sector_up;

   if (IS_EMPTY) return (fd);

   if (MUST_FLUSH) {
      if (!(_Mode & S_IWUSR)) return (-1); /* not user writable */
      if (-1 == LSEEK (fd, _L_disk, SEEK_SET))
	 return (-1);
      if (IS_DIRECT) {
         sector_up = ((_L_data + _Sector - 1) / _Sector) * _Sector;
         if (sector_up != write (fd, _Buffer, sector_up))
	    return (-1);
	 if (_L_disk + sector_up > _Actual_l_file) {
	    _Sector_l_file = _L_disk + sector_up;
	 }
      } else {
         if (_L_data != write (fd, _Buffer, _L_data))
            return (-1);
      }
      SET_EMPTY_BIT;
      CLEAR_FLUSH_BIT;
   }
   return (fd);

} /* END _fio_du_flush */

/*PROCEDURE*****************************************************************/

int _fio_du_read(unit *ftnunit, char *data, XINT n_data, XINT64 disk_loc, int fd)

/***************************************************************************
*
* PURPOSE:
*    Reads a Fortran file for direct unformatted data.
*
* INPUT:
*    data, char *, pointer to the data to read
*    n_data, int, length in bytes of data
*    disk_loc, int, location of disk to read from
*    fd, int, file descriptor
*
* OUTPUT:
*    function return value, int, -1 if error, fd if OK
*
* ASSUMPTIONS:
*    Assumes _fio_du_open called first, followed by any number of
*    _fio_du_read/write calls, followed by a call to _fio_du_close.
*
* HISTORY:
*    05/05/93 - LMS - Initial Coding.
*
*****************************************************************************/

{ /* BEGIN _fio_du_read */

   char            *loc_in_buffer;
   XINT             n_data_to_move;
   XINT             sector_up;

   if (ftnunit->uacc != DIRECT) {
      errno = F_ERNODIO;
      return(-1);
   }
   /*
    * Is the file readable? If not, error.
    */

   if (!(_Mode & S_IRUSR)) {
      errno = EBADF;
      return (-1);
    }

   /*
    * Is the request within the file? If not, error.
    */

   if ((disk_loc + n_data > _Open_l_file) &&
       (disk_loc + n_data > _Actual_l_file)) {
      errno = F_ERINVRECNO;
      return (-1);
   }

   while (n_data > 0) {
      if (IS_DIRECT) {
	 int hit_buff = !IS_EMPTY && (disk_loc >= _L_disk) && (disk_loc < _L_disk + _L_data);

	 /* 
	 ** Do direct read only if data is not already in the buffer
	 ** if it's large enough and if it satisfies all memory requirement
	 */
	 if (((long)data % d_mem) == 0 		/* data alignment */
	    && (disk_loc % _Sector) == 0	/* disk sector alignment */
	    && n_data >= LARGE_CHUNK /* reasonably large data */
	    && !hit_buff 	     /* why read if can do memcpy */
	    && (IS_EMPTY
	        || disk_loc != _L_disk + _L_data /* non-sequential read */
		|| n_data > _L_buffer/4)) /* very large chunk of data */
	 {
	    int oddbytes = n_data % _Sector;
	    int maxread; /* max. sectored bytes that can be read safely */

	    if ( !IS_EMPTY && (disk_loc + n_data) >= _L_disk && disk_loc < _L_disk)
		/* some trailing sectors may be in the buffer,
		** don't read them from the disk
		*/
		maxread = disk_loc - _L_disk;
	    else
		maxread = n_data - oddbytes;

	    n_data -= maxread;
	    if (-1 == LSEEK (fd, disk_loc, SEEK_SET)) return (-1);
            while (maxread >= d_maxiosz) {
               if (d_maxiosz != read (fd, data, d_maxiosz))
                  return(-1);
               data += d_maxiosz;
               maxread -= d_maxiosz;
               disk_loc += d_maxiosz;
            }
            if (maxread) {
               if (maxread != read (fd, data, maxread))
                  return(-1);
               data += maxread;
               disk_loc += maxread;
            }
	    _Current_pos = disk_loc;
	    continue;
	 }
         if (IS_EMPTY) {
	    if (disk_loc < _L_disk) {
	        int n_remain = ftnunit->url - ftnunit->f77recpos + n_data;
	        /* Trying to fit the requested data and the remaining record
		   at the end of the buffer in case some user might read the 
		   records backward
	        */
	        _L_disk = ((disk_loc+n_remain)/_Sector + 1) * _Sector - _L_buffer;
	        if (_L_disk < 0)
		   _L_disk = 0;
	        else if (disk_loc < _L_disk) 
	        /* when n_remain bigger than _L_buffer the we have no choice
		   but to position the currently requested data at the top
		      of the buffer
	        */
		   _L_disk = (disk_loc / _Sector) * _Sector;
	    } else
	           _L_disk = (disk_loc / _Sector) * _Sector;
	    if (-1 == LSEEK (fd, _L_disk, SEEK_SET)) return (-1);
	    if (-1 == (_L_data = read (fd, _Buffer, _L_buffer))) { 
		/*
		 * Read failed, so lets try it again in "normal" mode,
		 * then switch back to O_DIRECT.  This is needed to 
		 * get around a bug in O_DIRECT where the last partial
		 * sector of a file isn't readable.
		 */

	        if (-1 == LSEEK (fd, _L_disk, SEEK_SET)) return (-1);
		if (-1 == fcntl(fd, F_SETFL, (fcntl(fd, F_GETFL)&(~FDIRECT)))) 
		   return (-1);
	        if (-1 == (_L_data = read (fd, _Buffer, _L_buffer))) return(-1);
		if (-1 == fcntl(fd, F_SETFL, fcntl(fd, F_GETFL) | FDIRECT)) 
		   return (-1);
            }
	    CLEAR_EMPTY_BIT;

	    /* Check if the read is beyond the end of the file. */
   
	    if (_L_data < _L_buffer) {	/* end of the file */
	       if (disk_loc + n_data > _L_disk + _L_data) {
	          errno = -1;
	          return (-1);
	       }
	    }
         }
      
         /* 
          * Start of data is inside the buffer?  This algorithm is
          * inefficient in the case where the data overlaps the front of
          * the buffer, what happens is the buffer is moved, missing the
          * opportunity to read part of the data from the buffer.  Not
          * wrong, just slow. 
          */
   
         if ((disk_loc >= _L_disk) && (disk_loc < _L_disk + _L_buffer)) {
	    n_data_to_move = (_L_disk + _L_buffer) - disk_loc;
	    n_data_to_move = (n_data < n_data_to_move) ? n_data : n_data_to_move;
	    loc_in_buffer = _Buffer + (disk_loc - _L_disk);
	    memcpy (data, loc_in_buffer, n_data_to_move);
	    data     += n_data_to_move;
	    n_data   -= n_data_to_move;
	    disk_loc += n_data_to_move;
	    _Current_pos = disk_loc;
         } else {			/* Move the buffer. */
	    if (MUST_FLUSH) {
	       _fio_du_flush( fd );
	    }
	    else
	       SET_EMPTY_BIT;
         }
      /* End of direct I/O, all following cases are for non-direct I/O */
      } else if (n_data > _L_buffer) {
         /* reading very large data, just ignore the buffer and read into
	 ** the user data space directly
	 */
	 if (MUST_FLUSH) {
	    if (-1 == LSEEK (fd, _L_disk, SEEK_SET)) 
	       return (-1);
	    if (_L_data != write (fd, _Buffer, _L_data)) 
	       return (-1);
	    CLEAR_FLUSH_BIT;
	 }
	 if (-1 == LSEEK (fd, disk_loc, SEEK_SET))
	    return (-1);
	 if (n_data != read (fd, data, n_data))
	    return(-1);
	 n_data = 0;
      } else if (IS_EMPTY) {
	 /* if the buffer is empty then fill it up with data */
	 if (disk_loc < _L_disk) {
             int n_remain = ftnunit->url - ftnunit->f77recpos + n_data;
                /* Trying to fit the requested data and the remaining record
                   at the end of the buffer in case some user might read the
                   records backward
                */
             _L_disk = (disk_loc+n_remain) - _L_buffer;
             if (_L_disk < 0)
                _L_disk = 0;
             /* when n_remain bigger than _L_buffer the we have no choice
                but to position the currently requested data at the top
                   of the buffer
             */
             else if (disk_loc < _L_disk)
                _L_disk = disk_loc;
         } else
             _L_disk = disk_loc;
         if (-1 == LSEEK (fd, _L_disk, SEEK_SET)) 
            return (-1);
         if (-1 == (_L_data = read (fd, _Buffer, _L_buffer))) {
            errno = F_ERNODIO;
            return (-1);
         }
         CLEAR_EMPTY_BIT;
      } else if ((disk_loc >= _L_disk) && (disk_loc < _L_disk + _L_data)) {
         n_data_to_move = (_L_disk + _L_data) - disk_loc;
         n_data_to_move = (n_data < n_data_to_move) ? n_data : n_data_to_move;
         loc_in_buffer = _Buffer + (disk_loc - _L_disk);
         memcpy (data, loc_in_buffer, n_data_to_move);
         data += n_data_to_move;
         n_data -= n_data_to_move;
         disk_loc += n_data_to_move;
         _Current_pos = disk_loc;
      } else {
	 /* nothing in the buffer is usable, flush it to make it empty 
	 ** for a fresh buffer
	 */
         if (-1 == _fio_du_flush (fd)) return (-1);
	 SET_EMPTY_BIT;
      }
   }
   return (fd);

} /* END _fio_du_read */

/*PROCEDURE*****************************************************************/

int _fio_du_close(int fd)

/***************************************************************************
*
* PURPOSE:
*    Closes a Fortran file for direct unformatted reads/writes.
*
* INPUT:
*    fd, int, file descriptor 
*
* OUTPUT:
*    fd, int, file descriptor (if OK, -1 otherwise) 
*
* ASSUMPTIONS:
*    Assumes _fio_du_open called first, followed by any number of
*    _fio_du_read/write calls, followed by a call to _fio_du_close.
*
* HISTORY:
*    05/05/93 - LMS - Initial Coding.
*
*****************************************************************************/

{ /* BEGIN _fio_du_close */
   int istat;
   int lastpage;

   assert (fd >= 0);

   /* 
    * Flush the last buffer.
    */

   if (-1 == _fio_du_flush (fd)) return (-1);

   /*
    * Truncate the file if needed.   Note that the file could be extended 
    * even when no new records are added at the end.  If any of the 
    * records belong to the last buffer get updated and then the buffer 
    * gets flushed it will be extended to a multiple of the sector size.   
    * _Actual_l_file keeps track of the true file size but we need the
    * _Sector_l_file to check if more than the true file size has been
    * written out and so need to be truncated back to the true file size
    * when the file is closed..
    */

   if (_Sector_l_file > _Actual_l_file) {
      /* There are more bytes written out to the file than there are
      ** actual data due to the sector multiple requirement.   Has to
      ** truncate it to the right size
      */
      ftruncate (fd, _Actual_l_file);
   }


   /*
    * Deallocate the buffer and close the file.  
    */

   free (_Buffer);
   _Open_l_file  = 0; 
   _Actual_l_file = 0;
   _Sector_l_file = 0;
   _Mode         = 0;
   _Buffer       = 0;
   _L_buffer     = 0;
   _L_data       = 0;
   _L_disk       = 0;
   _Flags        = 0;

   /* obtain exclusive lock for special I/O operation */
   istat = close (fd);
   if (istat == -1) return (-1);
   return (fd);

} /* END _fio_du_close */


void
_fio_set_seek( int fd, long int offset, int from)
{
    if (from == 0) {
       _Seek_pos = offset;
    } else if (from == 1) {
       _Seek_pos = _Current_pos + offset;
    } else {
       _Seek_pos = _Actual_l_file + offset;
    }
    _Current_pos = _Seek_pos;
}

long
_fio_get_pos( int fd )
{
   return( _Current_pos );
}

void
_fio_seq_pos( int fd, unit *ftnunit )
{
   if (ftnunit->url != 1)
      if (_Seek_pos) {
         ftnunit->uirec = (_Seek_pos / ftnunit->url) + 1;
         ftnunit->f77recpos = _Seek_pos % ftnunit->url;
      } else
         ftnunit->uirec++;
   else if (_Seek_pos)
      ftnunit->uirec = _Seek_pos;
   /* after establishing the normal file positioning parameters, reset
   the _Seek_pos so we can ignore the seek operation after this
   */
   _Seek_pos = 0;
}

