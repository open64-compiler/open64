//-*-c++-*-
// ====================================================================
// ====================================================================
//
// Module: opt_config.h
// $Revision: 1.1.1.1 $
// $Date: 2005/10/21 19:00:00 $
// $Author: marcel $
// $Source: /proj/osprey/CVS/open64/osprey1.0/be/opt/opt_config.h,v $
//
// ====================================================================
//
// Copyright (C) 2000, 2001 Silicon Graphics, Inc.  All Rights Reserved.
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of version 2 of the GNU General Public License as
// published by the Free Software Foundation.
//
// This program is distributed in the hope that it would be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//
// Further, this software is distributed without any warranty that it
// is free of the rightful claim of any third person regarding
// infringement  or the like.  Any license provided herein, whether
// implied or otherwise, applies only to this software file.  Patent
// licenses, if any, provided herein do not apply to combinations of
// this program with other software, or any other product whatsoever.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write the Free Software Foundation,
// Inc., 59 Temple Place - Suite 330, Boston MA 02111-1307, USA.
//
// Contact information:  Silicon Graphics, Inc., 1600 Amphitheatre Pky,
// Mountain View, CA 94043, or:
//
// http://www.sgi.com
//
// For further information regarding this notice, see:
//
// http://oss.sgi.com/projects/GenInfo/NoticeExplan
//
// ====================================================================
// ====================================================================


#ifndef opt_config_INCLUDED
#define opt_config_INCLUDED             "opt_config.h"
#ifdef _KEEP_RCS_ID
static char *opt_configrcs_id = 	opt_config_INCLUDED"$Revision$";
#endif /* _KEEP_RCS_ID */

/* The -WOPT: group option flag variables are now in common/com: */
#include "config_wopt.h"

/*********************************************************************
 ***
 *** Anything that should not be seen outside of opt_config.c should
 *** be declared in this section.
 ***
 *********************************************************************
 */
#ifdef OPT_INTERNAL


/* used in yyy.c */

#define MAX_STACK_SIZE                          1024

#endif  /* OPT_INTERNAL */
/*********************************************************************
 ***
 *** Anything that may be seen outside of opt_config.c should
 *** be declared in this section.
 ***
 *********************************************************************
 */

#define STR_NONE "none"

#define INPUT_PATH_LENGTH 512

/* #define USE_LARGE_CODEREP 1 comment out to flush out any problem */

/* Default file	extensions: */
#define	IRB_FILE_EXTENSION ".B"	/* Binary WHIRL IR file */
#define	IRA_FILE_EXTENSION ".ir"/* Ascii  WHIRL IR file */
#define	ERR_FILE_EXTENSION ".e"	/* Error file */
#define	LST_FILE_EXTENSION ".l"	/* Listing file */
#define	TRC_FILE_EXTENSION ".t"	/* Trace file */
#define	ASM_FILE_EXTENSION ".s"	/* Assembly code file */
#define	OBJ_FILE_EXTENSION ".o"	/* Relocatable object file */
#define DSTDUMP_FILE_EXTENSION ".be.dst" /* DST dump-file extension */

/* The maximum profile level -- should probably be defined in a global
 * header file, but isn't.
 */
enum {MAX_PROFILE_LEVEL = 0 };

/* constants used by optimizer now */
enum { CFG_BB_TAB_SIZE = 10 };
enum { CFG_LAB_HASH_SIZE = 10 };
enum { CFG_ALTENTRY_TAB_SIZE = 10 };
enum { CFG_EARLYEXIT_TAB_SIZE = 10 };
enum { VAR_PHI_HASH_SIZE = 256 };
enum { CODE_HTABLE_SIZE = 9113 };	/* should be prime */
enum { CODE_ITABLE_SIZE = 1619 };	/* should be prime */
enum { EMITTER_COLOR_TAB_SIZE = 10 };
enum { RVI_CTAB_SIZE = 521 };		/* should be prime */
enum { LFTR_HASH_SIZE = 50 };		

#endif  /* opt_config_INCLUDED */

