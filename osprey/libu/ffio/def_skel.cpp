
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

/* USMID @(#) libu/ffio/def_skel.cpp	92.0	10/08/98 14:57:41 */

*
*
* The following file contains a commented segldr(1) directive for each
* of the FFIO capabilities that are currently disabled in <fdcconfig.h>.
*
* For the layers that are already enabled, no segldr directive(s) will
* appear.  To disable these layers changes must be made to <fdcconfig.h>
* and the libraries and commands re-built.
*
* For each of those that are disabled in <fdcconfig.h>, a commented
* directive appears below.  For each class of routines so
* disabled, you should determine if the function
* performed by those routines is of sufficient interest to your users
* to require them to be loaded by default in all FORTRAN programs and
* other programs that use the FFIO interface. (see the man page
* for ffread(3))
*
* For each of these HARDREF directives, if the functionality will be used,
* simply remove the '***' from the beginning of the line to de-comment it.
*
* If the functionality will not be used, then the commented directive
* can be deleted.  The directives in this file will be read by
* segldr(1) every time it is invoked.
*
#define YES 1
#define NO 0
#include <fdcconfig.h>
#include "fxlist.h"

*
* SYSTEM, FD, CACHE, and COS blocking routines are not listed here.
* These classes must never be disabled.  They are used in default paths
* in the FORTRAN run-time libraries.
*
*
#if ! (ENABLE_IBM_LYR || ENABLE_VMS_LYR || ENABLE_NVE_LYR)
* F class routines
* These implement IBM, VMS and NOS/VE fixed-length record formats
*
***LOADIT(F_XLIST_P)
#endif

#if ! (ENABLE_IBM_LYR || ENABLE_VMS_LYR || ENABLE_NVE_LYR)
*
* V class routines
* These implement IBM, VMS and NOS/VE variable-length record formats
*
***LOADIT(V_XLIST_P)
#endif

#if ! (ENABLE_NVE_LYR || ENABLE_CRAY_LYR || ENABLE_UX_LYR || ENABLE_205_LYR)
*
* X class routines
* These routines implement the 'f77' style blocking common on UNIX FORTRAN
* implementations.  Also, Cyber205/ETA V format and NOS/VE V format.
*
***LOADIT(X_XLIST_P)
#endif

#if ! (ENABLE_BMX_LYR)
*
* BMX glue routines
* Handlers for on-line tape.
*
***LOADIT(TAPE_XLIST_P)
#endif

#if ! (ENABLE_CDC_LYR)
*
* CDC routines
* CDC 60-bit record conversion.  Cyber IW, CW, CS, and CZ formats.
*
***LOADIT(CDC_XLIST_P)
#endif

#if ! (ENABLE_SDS_LYR)
*
* SDS file residency handlers.
*
***LOADIT(SDS_XLIST_P)
#endif

#if ! (ENABLE_MR_LYR)
*
* MR routines
* Memory file residency handlers.
*
***LOADIT(MR_XLIST_P)
#endif

#if ! (ENABLE_TRC_LYR)
*
* TRACE routines
* debugging and performance tracing.  Not supported.
*
***LOADIT(TRC_XLIST_P)
#endif

#if ! (ENABLE_TEXT_LYR)
*
* TEXT routines
* Newline separated text files.
*
***LOADIT(TXT_XLIST_P)
#endif

#if ! (ENABLE_ER90B_LYR)
*
* ER90 byte-stream files
*
***LOADIT(ER90B_XLIST_P)
#endif

*
* USER routines are always soft and should never be 'enabled'
*
***LOADIT(USR_XLIST_P)

#if ! (ENABLE_SITE_LYR)
*
* SITE routines
*
***LOADIT(SITE_XLIST_P)
#endif

#if ! (ENABLE_BLX_LYR)
*
* BLX routines
* Blank compression handlers.  Used to handle COS, CTSS and ETA blank
* compression
*
***LOADIT(BLX_XLIST_P)
#endif

*----------------------------------------------------------------------
*
* _def_cos_bs is the default buffer size for COS-blocked files, which are
* used for sequential unformatted FORTRAN I/O.
*
SET=_def_cos_bs:48

*----------------------------------------------------------------------
*
* _def_cos_thrsh is the default point at which automatic double-buffering
* is activated.  COS-blocked files assigned buffers larger than this
* size are double buffered.  Smaller than this size, they are synchronously
* single-buffered.
*
SET=_def_cos_thrsh:64

*----------------------------------------------------------------------
*
* This is the default page size for the cache layer in 512-word blocks.
* When used by the FORTRAN I/O library, the page size is the minimum of
* of the record size ('RECL=' on OPEN) and this minimum page size.
*
SET=_def_cch_bufsiz:8

*----------------------------------------------------------------------
*
* This is the default number of 'pages' allocated for the cache layer.
*
SET=_def_cch_nbuf:4

*----------------------------------------------------------------------
*
* CRAY X/CEA systems only
*
* Default number of 512-word buffer pages for files assigned '-s bin'.
*
SET=_def_bin_bs:16

*----------------------------------------------------------------------
*
* CRAY X/CEA systems only
*
* Default number of 512-word buffer pages for files assigned '-s sbin'.
*
SET=_def_sbin_bs:8

*----------------------------------------------------------------------
*
* CRAY X/CEA systems only
*
* Default minimum buffer, or 'page' size, in bytes, for tape.  On model D
* systems, this is the default minimum buffer size.  On model E systems,
* it is also a minimum default buffer size, but more than one of
* these 'pages' is allocated to buffer multiple asynchronous requests.
* The default buffer size is chosen as the minimum of this default,
* or the 'mbs' of the tape, whichever is larger, rounded up to a
* multiple of 4096 bytes.
*
SET=_def_tape_bs:65536

*----------------------------------------------------------------------
*
* CRAY X/CEA systems only
*
* When running on a model-E system, this is the default maximum total buffer
* size.  When multiple 'pages' are allocated, the number of 'pages' is
* adjusted downward so that the total buffer size does not exceed this value.
*
SET=_def_tape_totbuf:262144

*----------------------------------------------------------------------
*
* CRAY X/CEA systems only
*
* _def_max_fn is the default number of named files that can be opened
* (i.e. 'WRITE('file') i,j,k') from a FORTRAN program at one time.
*
SET=_def_max_fn:60

*----------------------------------------------------------------------
*
* _def_stdio_bs is the default buffer size for C I/O buffered streams.
* The special value 0 means that the libraries will pick an appropriate value.  
*
SET=_def_stdio_bs:0
