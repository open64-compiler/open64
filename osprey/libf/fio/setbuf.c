/*
 * Copyright (C) 2006. QLogic Corporation. All Rights Reserved.
 */

#pragma ident "@(#) libf/fio/setbuf.c"

#include <errno.h>
#include <fcntl.h>
#include <liberrno.h>
#include <stddef.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <cray/nassert.h>
#include <cray/assign.h>
#include "fio.h"
#include "util/utildefs.h"
#include "ffio/spec_parse.h"

#if defined(BUILD_OS_DARWIN)
#else /* defined(BUILD_OS_DARWIN) */
/* For backward compatibility */
extern int setlinebuf_(_f_int *unit);
#pragma weak setlinebuf_ = _Setlinebuf
extern int setbuf_(_f_int *unit, char *buf, size_t size);
#pragma weak setbuf_ = _Setbuf
#endif /* defined(BUILD_OS_DARWIN) */

/*
 * unum		logical unit number
 * cfs		I/O state used in locking the file
 * return	unit * corresponding to that logical unit, having performed
 *		an implicit open if need be and locked the file. errno is
 *		0 unless an error occurred
 */
static unit *
setup(unum_t unum, struct fiostate *cfs) {
  unit *cup = 0;
  /* Lock the unit */
  errno = 0;
  STMT_BEGIN( unum, 0, T_RSF, NULL, cfs, cup);
  if (cup == NULL) {
    int stat;
    int errf = 0;
    cup = _imp_open(cfs, SEQ, FMT, unum, errf, &stat);
    errno = (0 == cup) ? stat : 0;
  }
  if (unum < 0 || !cup) {
    errno = FEIVUNIT;
  }
  return cup;
}

/*
 * Similar to C setlinebuf(3s)
 * u		logical unit
 * return	0 if no error; else errno (system or Fortran)
 */
int
_Setlinebuf(_f_int *u) {
   struct fiostate cfs;
   unit *cup = setup(*u, &cfs);
   int res = errno;
   if (0 == res) {
     switch (cup->ufs) {
       case FS_TEXT:
       case STD:
	 setlinebuf(cup->ufp.std);
	 res = errno;
	 break;
       default:
	 res = errno = FESTIOER;
         break;
      }
   }
   STMT_END( cup, TF_READ, NULL,  &cfs);
   return res;
}

/*
 * Similar to C setbuf(3s)
 * u		logical unit
 * buf		buffer to use: can pass %val(0) or a character(len=0)
 *		variable to disable buffering
 * size		character length which Fortran passes unbeknownst to user
 * return	0 if no error; else errno (system or Fortran)
 */
int
_Setbuf(_f_int *u, char *buf, size_t size) {
   struct fiostate cfs;
   unit *cup = setup(*u, &cfs);
   int res = errno;
   if (0 == res) {
     switch (cup->ufs) {
       case FS_TEXT:
       case STD:
	 if (buf && (0 == size)) {
	   buf = 0;
	 }
	 setbuffer(cup->ufp.std, buf, size);
	 res = errno;
	 break;
       default:
	 res = errno = FESTIOER;
         break;
      }
   }
   STMT_END( cup, TF_READ, NULL,  &cfs);
   return res;
}

