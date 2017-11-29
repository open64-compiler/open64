/*
 * Copyright 2005, 2006 PathScale, Inc.  All Rights Reserved.
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

#ifdef KEY /* Bug 6121 */
#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

/* Note: update 'msg_severity_names' in messages.h if this enum is changed    */
/* Note: all message levels are output to CIF, only Comment .. Internal,      */
/*	 Log_Error, Log_Warning and Ansi are output to stderr		      */

enum	msg_severities	       {Comment,	Note,		Caution,
				Warning,	Error,		Internal,
				Vector,		Scalar,		Table,
				Ansi,		Log_Warning,	Inline,
				Info,		Tasking,	Limit,
				Log_Error,	Log_Summary,	F77_Ansi,
				Optimization,	Stream,		Unknown_Error };
typedef enum	msg_severities			msg_severities_type;

extern void PRINTMSG(int, int, msg_severities_type, int, ...);

#ifdef __cplusplus
}
#endif /* __cplusplus */
#endif /* KEY Bug 6121 */
