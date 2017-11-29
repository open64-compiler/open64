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



#ifndef _CMPLRS_LEB128_H
#define _CMPLRS_LEB128_H
/*

	LEB128.H

	reading and writing leb128 numbers.
	See the DWARF documentation for further information on leb numbers.
	No error indication is returned.

	The supplied buffer must be long enough to hold the leb number.
	For encode, 5 bytes is sufficient for 32 bit values and 10 bytes
	is sufficient for 64 bit values.

	The value returned is the number of bytes in the leb encoding.

	For improperly coded leb number (or one having more than
	the allowed number of leb digits with the most significant
	bit on in the input buffer) the
	leb buffer decode scan stops at 5 characters for 32 bit decodes 
	and stops at 10 characters for 64 bit decodes.  
	For such numbers the returned length will be 5  or 10 
	respectively, and no error indication is supplied.
	
	These routines are in libelfutil.a

*/
 


#ifdef __cplusplus
extern "C" {
#endif

#ifdef __MINGW32__
#include "sgidefs.h"
typedef __int32_t int32_t;
typedef __uint32_t u_int32_t;
typedef __int64_t int64_t;
typedef __uint64_t u_int64_t;
#endif /* __MINGW32__ */

#ifndef __GNUC__

extern int _leb128_unsigned_encode64(__uint64_t /*number*/, char* /*buffer*/);
extern int _leb128_signed_encode64(__int64_t /*number*/, char* /*buffer*/);

extern int _leb128_unsigned_encode32(__uint32_t /*number*/, char* /*buffer*/);
extern int _leb128_signed_encode32(__int32_t /*number*/, char* /*buffer*/);

extern int _leb128_unsigned_decode64(char* /*data*/, __uint64_t* /*value*/);
extern int _leb128_signed_decode64(char* /*data*/, __int64_t* /*value*/);

extern int _leb128_unsigned_decode32(char* /*data*/, __uint32_t* /*value*/);
extern int _leb128_signed_decode32(char* /*data*/, __int32_t* /*value*/);

#else

#include <sys/types.h>

extern int _leb128_unsigned_encode64(u_int64_t /*number*/, char* /*buffer*/);
extern int _leb128_signed_encode64(int64_t /*number*/, char* /*buffer*/);

extern int _leb128_unsigned_encode32(u_int32_t /*number*/, char* /*buffer*/);
extern int _leb128_signed_encode32(int32_t /*number*/, char* /*buffer*/);

extern int _leb128_unsigned_decode64(char* /*data*/, u_int64_t* /*value*/);
extern int _leb128_signed_decode64(char* /*data*/, int64_t* /*value*/);

extern int _leb128_unsigned_decode32(char* /*data*/, u_int32_t* /*value*/);
extern int _leb128_signed_decode32(char* /*data*/, int32_t* /*value*/);

#endif

#ifdef __cplusplus
}
#endif
#endif
