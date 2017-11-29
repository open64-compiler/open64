/*
 * Copyright 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
 */

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


/* ====================================================================
 * ====================================================================
 *
 * Module: ipl_tlog.cxx
 * $Revision: 1.1.1.1 $
 * $Date: 2005/10/21 19:00:00 $
 * $Author: marcel $
 * $Source: /proj/osprey/CVS/open64/osprey1.0/ipa/local/ipl_tlog.cxx,v $
 *
 * Description: Defines tlog utilities for IPL, originated from OPT
 *
 * ====================================================================
 * ====================================================================
 */
// ====================================================================
// Usage:   -Wi,-tt1:18 -keep : generates file.tlog for IPL
// ====================================================================
#include <stdarg.h>
#include <stdio.h>
#include <strings.h>
#include <time.h>

#ifndef USE_STANDARD_TYPES
#define USE_STANDARD_TYPES
#endif
#include "defs.h"
#include "errors.h"
#include "tracing.h"
#include "srcpos.h"
#include "tlog.h"
#include "ipl_tlog.h"
const INT32 MAX_WARN_LEN = 1024;

static const char* tlog_phase = "IPL";

static BOOL
Ipl_tlog_trace( void )
{
  return Get_Trace ( TP_PTRACE1, TP_IPL );
}

static  
void Ipl_tlog2(const char *keyword, INT64 srcpos, const char *msg )
{
  // use the keyword as both the transformation name and keyword
  Generate_Tlog( tlog_phase, keyword, (SRCPOS)srcpos, keyword, msg, "","" );
}

static void 
Ipl_tlog( const char *keyword, INT64 srcpos, const char *fmt, va_list vp)
{
  char msg_buf[MAX_WARN_LEN];
  INT32 len;
  vsprintf(msg_buf, fmt, vp);     // if msg is too long, it might overrun the buf
  len = strlen(msg_buf);
  //  FmtAssert doesn't work here!
  if (len >= MAX_WARN_LEN) {
    fprintf(stderr, "Ipa_tlog message buffer too small.");
  }

  Ipl_tlog2( keyword, srcpos, msg_buf );
}




// ====================================================================
// TLOG external interface for reporting optimizations
// ====================================================================
extern "C" void 
Ipl_record_tlog(const char *keyword, SRCPOS srcpos, const char *fmt, ...)
{
  va_list ap; 
  va_start(ap, fmt);
  if ( ! Ipl_tlog_trace() ) {
    return;
  }
  Ipl_tlog(keyword, srcpos, fmt, ap);
  va_end(ap);
}
