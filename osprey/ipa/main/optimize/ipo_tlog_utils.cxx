/*
 * Copyright (C) 2006. QLogic Corporation. All Rights Reserved.
 */

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


//-*-c++-*-
/* ====================================================================
 * ====================================================================
 *
 * Module: ipo_tlog_utils.cxx
 * $Revision: 1.1.1.1 $
 * $Date: 2005/10/21 19:00:00 $
 * $Author: marcel $
 * $Source: /proj/osprey/CVS/open64/osprey1.0/ipa/main/optimize/ipo_tlog_utils.cxx,v $
 *
 * Description: Defines tlog utilities for IPA, originated from OPT
 *
 * ====================================================================
 * ====================================================================
 */

// ====================================================================
// Usage: -IPA -Wb,-tt1:20 -keep : generates a.out.tlog for IPO
//        -IPA -Wl,-tt1:19 -keep : generates a.out.tlog for IPA
//     	  -Wi,-tt1:17 -keep : generates file.tlog for STANDALONE INLINER
// ====================================================================
#ifdef _KEEP_RCS_ID
#define ipo_tlog_utils_CXX	"ipo_tlog_utils.cxx"
static char *rcs_id = 	ipo_tlog_utils_CXX"$Revision: 1.4 $";
#endif /* _KEEP_RCS_ID *///-*-c++-
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
#include "ipo_tlog_utils.h"

const INT32 MAX_WARN_LEN = 1024;

// ====================================================================
// Provide a variable-argument interface to the TLOG performance
// tracing routine.
// ====================================================================

static void Ipa_Inline_tlog2( const char *, INT64, char * );

static const char *tlog_phase = NULL;

static BOOL
Ipa_tlog_trace( void )
{
  return Get_Trace ( TP_PTRACE1, TP_PTRACE1_IPA );
}

static BOOL
Inline_tlog_trace( void )
{
  return Get_Trace ( TP_PTRACE1, TP_PTRACE1_INL );
}



static void 
Ipa_Inline_tlog( const char *keyword, INT64 srcpos, const char *fmt,va_list vp)
{
  char msg_buf[MAX_WARN_LEN];
  INT32 len;

  vsprintf(msg_buf, fmt, vp);     // if msg is too long, it might overrun the buf
  len = strlen(msg_buf);
  //  FmtAssert doesn't work here!
  if (len >= MAX_WARN_LEN) {
    fprintf(stderr, "Ipa_tlog message buffer too small.");
  }

  Ipa_Inline_tlog2( keyword, srcpos, msg_buf );
}

static  void Ipa_Inline_tlog2(const char *keyword, INT64 srcpos, char *msg )
{
  // use the keyword as both the transformation name and keyword
  Generate_Tlog( tlog_phase, keyword, (SRCPOS)srcpos, keyword, msg, "","" );
}

// ====================================================================
// TLOG external interface for reporting optimizations
// ====================================================================

extern "C" void
Set_ipa_tlog_phase(const INT32 i)
{
  if (i==PHASE_IPA)
    tlog_phase = "IPA";
  else
    tlog_phase = "INLINER";
}

#ifdef KEY
extern "C" PHASE_NAME
Get_ipa_tlog_phase(void)
{
  if (!tlog_phase)
  	return INVALID;	// invalid
  if (!strcmp(tlog_phase, "IPA"))
  	return PHASE_IPA;
  return PHASE_INLINER;
}
#endif

extern "C" void 
Ipa_tlog(const char *keyword, SRCPOS srcpos, const char *fmt, ...)
{
  va_list ap; 
  va_start(ap, fmt);
  // can accept either -tt1:1 or -tt1:
  if ( ! Ipa_tlog_trace() ) {
    return;
  }
  Ipa_Inline_tlog(keyword, srcpos, fmt, ap);
  va_end(ap);
}

extern "C" void 
Inline_tlog(const char *keyword, SRCPOS srcpos, const char *fmt, ...)
{
  va_list ap; 
  va_start(ap, fmt);
  // can accept either -tt1:1 or -tt1:
  if ( ! Inline_tlog_trace() ) {
    return;
  }
  Ipa_Inline_tlog(keyword, srcpos, fmt, ap);
  va_end(ap);
}

