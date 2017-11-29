/* USMID @(#) clibinc/cray/nlcatmsg.h	92.2	06/04/99 14:08:09 */

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

#ifndef __NLCATMSG_H__
#define __NLCATMSG_H__

#ifdef __cplusplus
extern "C" {
#endif

/*
 * assume nl_types.h has been included.  Use it to pick up the
 * generic defines for the message cat
 */

/*
 * local versions since gnu has int_max for these.
 */
#ifndef NL_CATMSG_SETMAX
#define NL_CATMSG_SETMAX	1024
#endif
#ifndef NL_CATMSG_MSGMAX
#define NL_CATMSG_MSGMAX	32767
#endif
#ifndef NL_CATMSG_TEXTMAX
#define NL_CATMSG_TEXTMAX	2048
#endif

#ifndef NL_MAXPATHLEN
#define NL_MAXPATHLEN	1024
#endif
#ifndef NL_PATH
#define NL_PATH		"NLSPATH"
#endif
#ifndef NL_LANG
#define NL_LANG		"LANG"
#endif
#ifndef NL_DEF_LANG
#define NL_DEF_LANG	"english"
#endif
#ifndef NL_SETD
#define NL_SETD		1
#endif
#ifndef NL_MAX_OPENED
#define NL_MAX_OPENED	10
#endif

#ifndef NL_CAT_LOCALE
#define NL_CAT_LOCALE   1
#endif

/*
 * Default search pathname
 */
#ifndef DEF_NLSPATH
#define DEF_NLSPATH	"/usr/share/locale/%L/LC_MESSAGES/%N:/usr/share/locale/%L/Xopen/LC_MESSAGES/%N:/usr/share/locale/%L/LC_MESSAGES/%N.cat:/usr/share/locale/C/LC_MESSAGES/%N:/usr/share/locale/C/LC_MESSAGES/%N.cat"
#endif

/*
 * Default search path for the C locale only. Can still be overridden with NLSPATH.
 */
#ifndef _C_LOCALE_DEF_NLSPATH
#define _C_LOCALE_DEF_NLSPATH	"/usr/share/locale/C/LC_MESSAGES/%N:" \
				"/usr/share/locale/C/Xopen/LC_MESSAGES/%N:" \
				"/usr/share/locale/%L/LC_MESSAGES/%N.cat"
#endif

/* Default explanation and message set numbers */
#ifndef NL_EXPSET
#define NL_EXPSET       NL_SETD       /* set number for explanations */
#endif
#if defined(BUILD_OS_DARWIN)
#define NL_MSGSET       1
#else
#ifndef NL_MSGSET
#define NL_MSGSET       NL_SETD       /* set number for messages */
#endif
#endif

/* catmsgfmt formating information */

#define MSG_FORMAT	"MSG_FORMAT"
#define D_MSG_FORMAT	"%G-%N %C: %S %P\n  %M\n"

/* Internal catopen errors */
#define NL_ERR_MAXOPEN	-2      /* Too many catalog files are open.
				   See NL_MAX_OPENED */
#define NL_ERR_MAP	-3      /* The mmap(2) system call failed */
#define NL_ERR_MALLOC	-4      /* malloc(3C) failed */
#define NL_ERR_HEADER	-5      /* Message catalog header error */

/* Internal catgets and catgetmsg errors */
#define NL_ERR_ARGERR	-6      /* Bad catd argument */
#define NL_ERR_BADSET	-7      /* The set does not exist in the catalog. */
#define NL_ERR_NOMSG    -8      /* The message was not found. */
#define NL_ERR_BADTYPE  -9      /* The catalog type is unknown */

char *catgetmsg(nl_catd, int, int, char *, int);
char *catmsgfmt(const char *, const char *, int, const char *,
		const char *, char *, int, char *, char *);
int __catgetmsg_error_code(void);
char *_cat_name(char *, char *, int, int);
char *__cat_path_name(nl_catd);

#ifdef __cplusplus
}
#endif
#endif /* !__NLCATMSG_H__ */

