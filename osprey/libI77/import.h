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


#ifdef SHLIB
#ifdef _SVR4_SOURCE
#define __us_rsthread_stdio		(*_libI77___us_rsthread_stdio)
#else
#define _us_rsthread_stdio		(*_libI77__us_rsthread_stdio)
#endif
#define _sproced	(*_libI77__sproced)
#include <cmplrs/fio.h>

#define f77vms_flag_	(*_libI77_f77vms_flag_)
#define f77vfmt_com_	(*_libI77_f77vfmt_com_)
#define __mp_check_setup (*_libI77___mp_check_setup)


#define mkdatname	(*_libI77_mkdatname)
#define mkidxname	(*_libI77_mkidxname)
#define mklokname	(*_libI77_mklokname)

#ifdef _SVR4_SOURCE
#define __ctype		(*_libI77___ctype)
#define __filbuf		(*_libI77___filbuf)
#define __flsbuf		(*_libI77___flsbuf)
#define __iob		(*_libI77___iob)
#define __semgetc	(*_libI77___semgetc)
#define __semputc	(*_libI77___semputc)
#else
#define _ctype		(*_libI77__ctype)
#define _filbuf		(*_libI77__filbuf)
#define _flsbuf		(*_libI77__flsbuf)
#define _iob		(*_libI77__iob)
#define _semgetc	(*_libI77__semgetc)
#define _semputc	(*_libI77__semputc)
#endif
#define sys_errlist	(*_libI77_sys_errlist)
#define sys_nerr	(*_libI77_sys_nerr)

#define _cleanup	(*_libI77__cleanup)
#define __checktraps	(*_libI77___checktraps)
#define abort		(*_libI77_abort)
#define access		(*_libI77_access)
#define calloc		(*_libI77_calloc)
#define ecvt		(*_libI77_ecvt)
#define errno		(*_libI77_errno)
#define exit		(*_libI77_exit)
#define fclose		(*_libI77_fclose)
#define fcvt		(*_libI77_fcvt)
#define fflush		(*_libI77_fflush)
#define fgets		(*_libI77_fgets)
#define fopen		(*_libI77_fopen)
#define fp_class_d	(*_libI77_fp_class_d)
#define fprintf		(*_libI77_fprintf)
#define fread		(*_libI77_fread)
#define free		(*_libI77_free)
#define freopen		(*_libI77_freopen)
#define fscanf		(*_libI77_fscanf)
#define fseek		(*_libI77_fseek)
#define fstat		(*_libI77_fstat)
#define ftell		(*_libI77_ftell)
#define fwrite		(*_libI77_fwrite)
#define getenv		(*_libI77_getenv)
#define gets		(*_libI77_gets)
#define isatty		(*_libI77_isatty)
#define malloc		(*_libI77_malloc)
#define memcpy		(*_libI77_memcpy)
#define memset		(*_libI77_memset)
#define mktemp		(*_libI77_mktemp)
#define perror		(*_libI77_perror)
#define realloc		(*_libI77_realloc)
#define rewind		(*_libI77_rewind)
#define sprintf		(*_libI77_sprintf)
#define sscanf		(*_libI77_sscanf)
/*	Would mess up structure stat
#define stat		(*_libI77_stat)
*/
#define strcat		(*_libI77_strcat)
#define strcmp		(*_libI77_strcmp)
#define strcpy		(*_libI77_strcpy)
#define strlen		(*_libI77_strlen)
#define strncat		(*_libI77_strncat)
#define strncmp		(*_libI77_strncmp)
#define system		(*_libI77_system)
#define truncate	(*_libI77_truncate)
#define ungetc		(*_libI77_ungetc)
#define unlink		(*_libI77_unlink)
#ifdef _SVR4_SOURCE
#define clearerr	(*_libI77_clearerr)
#endif

extern	unsigned short (*_libI77_f77vms_flag_)[];
extern	vfmt_struct (*_libI77_f77vfmt_com_);
extern	int	(*_libI77___mp_check_setup)();

extern	int	(*_libI77_mkdatname)();
extern	int	(*_libI77_mkidxname)();
extern	int	(*_libI77_mklokname)();

#ifdef _SVR4_SOURCE
extern	char 	(*_libI77___ctype)[];
extern	int	(*_libI77___filbuf)();
extern	int	(*_libI77___flsbuf)();
extern	FILE 	(*_libI77___iob)[];
extern	int	(*_libI77___semgetc)();
extern	int	(*_libI77___semputc)();
#else
extern	char 	(*_libI77__ctype)[];
extern	int	(*_libI77__filbuf)();
extern	int	(*_libI77__flsbuf)();
extern	FILE 	(*_libI77__iob)[];
extern	int	(*_libI77__semgetc)();
extern	int	(*_libI77__semputc)();
#endif

extern	void *	(*_libI77__sproced);
#ifdef _SVR4_SOURCE
extern	int	(*_libI77___us_rsthread_stdio);
#else
extern	int	(*_libI77__us_rsthread_stdio);
#endif
extern	void	(*_libI77__cleanup)();
extern	int	(*_libI77___checktraps)();
extern	char *	(*_libI77_sys_errlist)[];
extern	int	(*_libI77_sys_nerr);


extern	int	(*_libI77_abort)();
extern	int	(*_libI77_access)();
extern	char *	(*_libI77_calloc)();
extern	char *	(*_libI77_ecvt)();
extern	int	(*_libI77_errno);
extern	void	(*_libI77_exit)();
extern	int	(*_libI77_fclose)();
extern	char *	(*_libI77_fcvt)();
extern	int	(*_libI77_fflush)();
extern	char *	(*_libI77_fgets)();
extern	FILE *	(*_libI77_fopen)();
extern	int	(*_libI77_fprintf)();
extern	int	(*_libI77_fread)();
extern	void	(*_libI77_free)();
extern	int	(*_libI77_freopen)();
extern	int	(*_libI77_fscanf)();
extern	int	(*_libI77_fseek)();
extern	int	(*_libI77_fstat)();
extern	int	(*_libI77_ftell)();
extern	int	(*_libI77_fwrite)();
extern	char *	(*_libI77_getenv)();
extern	char *	(*_libI77_gets)();
extern	int	(*_libI77_isatty)();
extern	char *	(*_libI77_malloc)();
extern	void	(*_libI77_memcpy)();
extern	void	(*_libI77_memset)();
extern	char *	(*_libI77_mktemp)();
extern	void	(*_libI77_perror)();
extern	char *	(*_libI77_realloc)();
extern	int	(*_libI77_rewind)();
extern	int	(*_libI77_sprintf)();
extern	int	(*_libI77_sscanf)();
extern	int	(*_libI77_stat)();
extern	char *	(*_libI77_strcat)();
extern	int	(*_libI77_strcmp)();
extern	char *	(*_libI77_strcpy)();
extern	int	(*_libI77_strlen)();
extern	char *	(*_libI77_strncat)();
extern	int	(*_libI77_strncmp)();
extern	int	(*_libI77_system)();
extern	int	(*_libI77_truncate)();
extern	int	(*_libI77_ungetc)();
extern	int	(*_libI77_unlink)();
#ifdef _SVR4_SOURCE
extern  void	(*_libI77_clearerr)();
#endif

#else

extern	unsigned short (f77vms_flag_)[];

#endif
