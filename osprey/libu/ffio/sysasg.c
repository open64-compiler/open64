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


#pragma ident "@(#) libu/ffio/sysasg.c	92.2	10/14/99 15:22:06"

#include <ctype.h>
#include <errno.h>
#include <ffio.h>
#include <fcntl.h>
#ifdef	_UNICOS
#include <infoblk.h>
#endif
#include <liberrno.h>
#include <malloc.h>
#include <memory.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#if	defined(_LITTLE_ENDIAN)
#include <unistd.h>
#endif
#include <sys/param.h>
#include <cray/fndc.h>
#include "layer_def.h"
#include "blxio.h"
#include "cchio.h"
#include "cdcio.h"
#include "cosio.h"
#include "evntio.h"
#include "fssio.h"
#include "gfio.h"
#include "globio.h"
#include "gvio.h"
#include "gxio.h"
#include "txtio.h"
#include "trcio.h"
#include "spec_parse.h"

#if	!defined(COMPAT_70)
# define COMPAT_70	1	/* 1 if must be compatible with 7.0 */
#endif


extern int	optind;
extern char	*optarg;

struct opt_s {
	union spec_u	*tspec;	/* pointer to array of FDC rec trans. spec words */
	int		cvrt;	/* numeric conversion type code */
	int		cvch;	/* character conversion type code */
};

/*
 *	Prototypes
 */
static int _eag_sys_asg(union spec_u *specs, const char *str , int limit, int warnmode,
        int errmode );
static int _crk_fchar_opts(struct opt_s *opts, char *optstr);
#ifdef _CRAY
static long _get_tok(char **icp);
#endif
static int _bld_fchar_opts(char *optstr, struct opt_s *opts);
int _parse_cvrt(const char *str, int warnmode);
int _parse_cvch(const char *str);


/*
 *	Helper functions
 */
static void
_asgwarning(char *s, ...)
{
	long	v1, v2, v3;
	va_list	args;

	va_start(args, s);
	v1	= va_arg(args, long);
	v2	= va_arg(args, long);
	v3	= va_arg(args, long);
	va_end(args);

	(void) fprintf(stderr, "warning: ");
	(void) fprintf(stderr, s, v1, v2, v3);
	(void) fputs("\n", stderr);
	return;
}

static void
_asgerror(char *s, ...)
{
	long	v1, v2, v3;
	va_list	args;

	va_start(args, s);
	v1	= va_arg(args, long);
	v2	= va_arg(args, long);
	v3	= va_arg(args, long);
	va_end(args);

	(void) fprintf(stderr, "asg: ");
	(void) fprintf(stderr, s, v1, v2, v3);
	(void) fputs("\n", stderr);
	return;
}

/*
 * _sys_asg( argc, argv, oldfchar, oldalias, newfchar, newalias, fdp)
 *
 *	argc - count of arguments
 *	argv - argument vector
 *	oldfchar - pointer to character string containing
 *		 characteristics environment variable.
 *	oldalias - pointer to character string containing
 *		 aliases environment variable.
 *	newfchar - returned pointer to character string containing
 *		 new characteristics environment variable.
 *	newalias - returned pointer to character string containing
 *		 new aliases environment variable.
 *	fdp - pointer to word that will be set to the file descriptor
 *		of the resulting file (if pre-opened...) ERR if none.
 *
 *	assign characteristics and aliases to files.  This is
 *	the low level routine called by both the shell(s) and the
 *	asg/ASG driver.  It parses the 'command' and opens files,
 *	allocates space, as appropriate.  Due to different
 *	conventions in the shells, the PUTENV call is not done.
 *	instead, the space is allocated on the heap, and a pointer
 *	to it is passed back to the caller.  This routine WILL
 *	allocate space for the NEW environment variables.  It is
 *	the responsibility of the caller to deallocate this space
 *	if it is not used.
 */

_sys_asg(int argc, char **argv, char *oldfchar, char *oldalias,
	char **newfchar, char **newalias, int *fdp)
	{
	static char	options[] = "C:F:N:";

	int	aliasflag;
	int	fd;
	int	forflag;
	int	cvrtflag;
	int	cvchflag;
	int	c;
	char	optstring[PATH_MAX];
	char	*oldopts, *forstr, *cvrtstr;
	char	*cvchstr;
	char	*alias;
	char	filename[PATH_MAX], *cp;
	char	*aliasentry, *fcharentry;

	struct opt_s	opt_struc;


#ifdef	DEBUG
	if (__fdctrace_enable < 0)
		{
		char	*estr;
		if ((estr = getenv("FDCTRACE")) == NULL)
			__fdctrace_enable	= 0;
		else
			__fdctrace_enable	= atol(estr);
		}
#endif

	optstring[0]	= '\0';

	fd		= -1;
	alias		= "";
	aliasflag	= _FALSE;
	forflag		= _FALSE;
	cvrtflag	= _FALSE;
	cvchflag	= _FALSE;
	optind		= 1;

	while ((c = getopt(argc, argv, options)) != EOF)
		{
		switch (c)
			{
			case 'a': /* alias */
				{
				aliasflag	= _TRUE;
				alias		= optarg;
				break;
				}
			case 'C': /* Foreign dataset Character conversion */
				{
				cvchflag	= _TRUE;
				cvchstr		= optarg;
				break;
				}
			case 'F': /* Foreign dataset type */
				{
				forflag		= _TRUE;
				forstr		= optarg;
				break;
				}
			case 'N': /* Foreign dataset Numeric conversion */
				{
				cvrtflag	= _TRUE;
				cvrtstr		= optarg;
				break;
				}
			case '?': /* illegal option */
				{
				return(1);
				}
			}
		}
	if (optind == argc)
		{
		_asgerror("need a file name");
		return(1);
		}
	if (optind < argc)
		(void)_cpyname(filename, argv[optind++]);
	if (optind < argc)
		{
		_asgerror("too many file names");
		return(1);
		}
/*
 *	we want to enforce a rule that an alias must always refer
 *	to a file
 */
	if (oldalias)
		{
		if (aliasentry	= _g_alias(oldalias, filename))
			{
			if (aliasflag)
				{
				_asgerror("cannot alias an alias");
				return(1);
				}
/*
 *			 file name is an alias, treat it same as real file name
 */
			_cpyname(filename, _g_rname(aliasentry));
			}

/*
 *		New alias assignment replaces old aliases.
 */
		if (aliasflag)
			{
			if (aliasentry	= _g_alias(oldalias, alias))
				{
				_asgerror("alias replacement deferred");
				return(1);
				}
			}
		}
	else	/* else no old alias */
		oldalias	= "";

	fcharentry	= NULL;

	if (oldfchar)
		{
		if ((cp = _g_alias(oldalias, filename)) != NULL)
			_cpyname(filename, cp);
		fcharentry	= _g_fchar(oldfchar, filename);
		}
	else
		oldfchar	= "";	/* set to NULL string */

	/* initialize the option structure */
	opt_struc.tspec	= NULL;
	opt_struc.cvrt	= 0;
	opt_struc.cvch	= 0;

	/* search for the old 'name' entry */
	if (fcharentry)
		{
		/* get old optstring and put in new entry */
		oldopts	= _g_fchar_opts(fcharentry);
/*
 *		The option string must be cracked and merged with the
 *		options specified.
 */
		if (_crk_fchar_opts(&opt_struc, oldopts) != 0)
			{
			_asgerror("Corrupted environment string");
			return(1);
			}
		}

/*
 *	Check for incompatible options
 */
	/*	No incompatable options yet... */

/*
 *	merge the old and new options
 */
	if (forflag)
		{
		int length;
		union spec_u specs[31];	/* limit of 10 layers, max 3 wds/lyr */
/*
 *		parse_forstr() parses the option string for record translation
 *		and returns the length in words of the spec array.
 *
 *		Arbitrary limit of 10 layers also keeps the token length under
 *		256 chars in FCHAR string (some buffers are only 256 chars).
 */
		length	= _parse_forstr(specs, forstr, 30, YES, _LELVL_MSG);

		if (length > 0)
			{
			if (opt_struc.tspec != NULL)
				free ((char *)opt_struc.tspec);
/*
 *			allocate space for spec words
 *			and copy to opt_struc
 */
			opt_struc.tspec	=
				(union spec_u *)malloc(length*sizeof(specs[0]));
			memcpy(opt_struc.tspec, specs, length*8);
			}
		else /* error */
			{
			_asgerror("bad record translation specification");
			return(1);
			}
		}
	if (cvchflag)
		{
		register int	res;

		res	= _parse_cvch(cvchstr);

		if (res < 0)
			{
			_asgerror("bad character conversion option");
			return(1);
			}
		opt_struc.cvch	= res;
		}
	if (cvrtflag)
		{
		register int	res;

		res	= _parse_cvrt(cvrtstr, YES);

		if (res < 0)
			{
			_asgerror("bad numeric conversion option");
			return(1);
			}
		opt_struc.cvrt	= res;
		}
/*
 *	Check to make sure that -N and -C are accompanied by a -F
 */
	if (opt_struc.cvch != 0 || opt_struc.cvrt != 0)
		if (opt_struc.tspec == NULL)
			{
			_asgerror("the -C and -N parameters must be accompanied by -F");
			exit(1);
			}
/*
 *	rebuild the fchar options from the modified structure
 */
	if (_bld_fchar_opts(optstring, &opt_struc) > PATH_MAX)
		_asgerror("Resulting spec is too long");

/*
 *	Make sure any memory 'malloc'ed is freed.  We are done with it
 *	now.
 */
	if (opt_struc.tspec != NULL) free (opt_struc.tspec);

#ifdef	DEBUG
	if (FDCTRACE(TRACE_ASG))
		{
		fprintf(stderr, "sysasg: oldfchar=<%s>, fn=%s, fd=%d, opt=%s\n",
			oldfchar, filename, fd, optstring);
		fprintf(stderr, "sysasg2: oldalias=<%s>, alias=%s, fn=%s\n",
			oldalias, alias, filename);
		}
#endif
	/* insert the file characteristics entry if not already there */

	*newfchar	= _bld_fchar(oldfchar, filename, fd, optstring);

	/* insert the ALIASES */
	if (aliasflag)
		*newalias	= _bld_alias(oldalias, alias, filename);
	else
		*newalias	= NULL;

	/* return the file descriptor */
	*fdp	= fd;

	return(0);
	}

/*
 * Crack the fchar options into a structure for manipulation.
 * Some shortcuts can be taken here as the string is generated
 * by the _bld_fchar_opts() routine, so we know that the first char is a '-'
 * and the separator is always a single space.
 */

static int
_crk_fchar_opts(struct opt_s *opts, char *optstr)
	{
	int		c, toklen, wl, i;
	long		tword;
	long		_hex2bin(char *S);
	char		token[256], *cp;
	union fddcvrt_u	fddcvrt;
	union spec_u	*words;

	if (optstr == NULL)
		optstr	= "";
	while (*optstr == '-')
		{
		optstr++;		/* skip '-' */
		c	= *optstr++;	/* pick up option letter */
		optstr++;		/* skip space */
		toklen	= _cpyname(token, optstr);
		optstr	+= toklen;
		cp	= token;
		switch (c)
			{
/*
 *			The C option word contains the version ID, the
 *			character conversion table choice and numeric
 *			conversion code choice.
 */
			case 'C': /* Foreign record translation spec */
				{
				/* check version and conversion word */
				fddcvrt.wword	= _hex2bin(cp);
				if(fddcvrt.fld.vers != ASG_VERSION)
					_asgerror("incompatible asg version");
				opts->cvrt	= fddcvrt.fld.cvrt;
				opts->cvch	= fddcvrt.fld.cvch;
				break;
				}

/*
 *			The FDC spec consists of
 *			one or more translation specification words, the last
 *			may be a physical layer word.
 */
			case 'F': /* Foreign record translation spec */
				{
				/* 17 = 16 hex digits + '+' */
				wl	= toklen / 17; /* number of words in spec */
				words	= (union spec_u *)malloc((wl+1)*sizeof(*words));
				i	= 0;
				do
					{
					tword	= _hex2bin(cp);
					words[i].wword	= tword;
					i++;
					cp	+= 17;	/* skip '+' too */
					} while(i < wl);
				words[i].wword	= 0;
				opts->tspec	= words;
				break;
				}
			default: /* illegal option */
				{
				return(ERR);
				}
			}
		if(*optstr == ' ') optstr++;	/* skip trailing space */
		}
	return(0);
	}


/*
 * Build the fchar options string from the structure
 * Option strings MUST have the following form:
 *	-x<space>[options]<space>
 * The trailing space is important.
 */
static int
_bld_fchar_opts(char *optstr, struct opt_s *opts)
	{
	long		*sp;
	char		*cp;
	union fddcvrt_u	fddcvrt;

	cp	= optstr;
	if (opts->cvrt != 0 || opts->cvch != 0)
		{
		/* -C for FOREIGN DATA conversion option */
		strcpy(cp, "-C ");
		cp	+= 3;
		/* build header word */
		fddcvrt.wword	= 0;
		fddcvrt.fld.vers	= ASG_VERSION;
		fddcvrt.fld.cvrt	= opts->cvrt;
		fddcvrt.fld.cvch	= opts->cvch;
		_bin2hex(cp, fddcvrt.wword);
		cp	+= 16;
		*cp	= ' ';
		cp++;
		}
	if (opts->tspec != NULL && opts->tspec->wword != 0)
		{
		/* -F for FOREIGN RECORD option */
		strcpy(cp, "-F ");
		cp	+= 3;
/*
 *		One word per FDC record spec
 */
		sp	= (long *)opts->tspec;
		while(*sp != 0)
			{
			_bin2hex(cp, *sp);
			sp++;
			cp	+= 16; /* 16 chars and '+' */
			*cp	= '+';
			cp++;
			}
		*cp	= ' ';
		cp++;
		}
	*cp	= '\0';
	/* return length of string produced */
	return(cp - optstr);
	}


/*
 * Goes through the options list and builds up the specification,
 * in the argument specs. Returns -1 on error. Otherwise returns
 * number of entries added to specs.
 */
static int
_eag_sys_asg(
union spec_u *specs,	/* The resulting encoded specification */
const char *str,	/* The options string. Input to the routine */
int limit,		/* The maximum size of specs. Input to the routine. */
int warnmode,		/* If not 0, print warning msg. Otherwise, be silent */
int errmode )		/* If not 0, print error msg. ERR is always returned */
			/* on error */
{
int index=0 , spec_level=0, status ;
int  num_added ;
char layer_options[256] ;
char layer_name[80] ;

	while(*str) {
		_get_next_token( &str, layer_name    , ".:,|", 1, 1,
			80, errmode );
		_get_next_token( &str, layer_options , ",|"  , 1, 1,
			256, errmode );

		status = _set_layer_options(
			layer_name,
			layer_options,
			specs+index,
			spec_level,
			limit,
			&num_added,
			warnmode,
			errmode);
		if( status != 0 )  {
			return(ERR);
		}

		spec_level++ ;

		index += num_added ;

		if( *str ) str++ ; /* skip past the , or | */

	}
	return(index) ;
}
/*
 * Parse the record translation options
 * the general format of this string is:
 *	class[.rtype[.subtype]][:rs[:mbs]][,...]
 *
 * examples:
 *	ibm.fb:80:8000,cos	request two layer conversion
 *				IBM FB records in COS blocking.
 *				RS=80, MBS=8000.
 *
 *	vms.D			one layer conversion of VMS D type records
 *				default RS and MBS are used.
 *
 *	cdc.cz.si		one layer, CDC CZ records, si tape format
 *
 *	input parameters:
 *		specs:	pointer to space to store the result specs in
 *			encoded form
 *		str:	ptr to string to parse
 *		limit:	number of words available in space at 'specs'
 *	     warnmode:	optional parameter.  if present and != 0, then
 *			parse_forstr will issue warning messages as necessary.
 *			The default is no issuance of warning messages.
 *	      errmode:  optional parameter.  if present, this is the error
 *			handling mode understood by the _lerror routine:
 *				_LELVL_MSG	- issue message and return
 *				_LELVL_RETURN	- just return (with err stat)
 *			_LELVL_MSG is the default.
 *	returns:
 *		ERR (-1) for error
 *		length of the resultant encoded spec, including the terminator
 */

#define MAX_NUM_PARM	16	/* maximum number of numeric layer parameters */
int
_parse_forstr(union spec_u *specs, const char *str, int limit, int warnmode, int errmode)
	{
	char	*class_cp;
	int	class;	/* class of record conversion */
	int	index;
	int	wmsgs;	/* 1 if warning messages should be printed */
	int	emsgs;	/* 1 if error messages should be printed */
	int	narg;

	extern struct	xtr_s *_recfm_tab[];


	wmsgs  = 0;
	if (warnmode != 0)
		wmsgs	= 1;

	emsgs	= 1;
	if ( errmode < _LELVL_MSG)
		emsgs	= 0;

	index	= 0;

	index = _eag_sys_asg(specs, str, limit, warnmode, errmode );

        if (index == ERR) 
		return(ERR);

	if (index >= limit) goto toomany;

	specs[index].wword	= 0;	/* zero terminator */
	index++;
	return(index);

toomany:
	if (emsgs)
		_asgerror( "too many record translation layers.");
	return(ERR);
	}

/*
 * Parse the numeric set selection option.  warnmode is an optional
 * parameter.  1 implies that warnings should be printed.  0 or omitted
 * implies no warnings printed.
 */
_parse_cvrt(const char *str, int warnmode)
	{
	int	narg;
	int	wmsgs;
        char	token[16];
	int	i;
#ifndef _CRAY
        narg = 2; 
	wmsgs	= warnmode;
#else
	NUMARG(narg);
	wmsgs	= 0;

	if (narg > 1)
		wmsgs	= warnmode;
#endif
        if (_get_next_token(&str, token, ":.,", 1, 1, 16, 0) == 0)
            return(ERR);
	if (*str != '\0') return(ERR);
	for (i = 0; i < _num_cvrt; i++) {
		if (strcmp(token, _cvrt_parse_tables[i].name)==0) {
			return(_cvrt_parse_tables[i].type);
		}
	}
	return(ERR);
	}


/*
 * Parse the character set selection option
 */
_parse_cvch(const char *str)
	{
        char token[16];
	int ret;
	int i;

        if (_get_next_token(&str, token, ":.,", 1, 1, 16, 0) == 0)
            return(ERR);
	if (*str != '\0') return(ERR);
	for (i = 0; i < _num_cvch; i++) {
		if (strcmp(token, _cvch_parse_tables[i].name)==0) {
			return(_cvch_parse_tables[i].type);
		}
	}
	return(ERR);
	}
/*
 * Applications has their own version of _get_ffio_rt_defaults.
 * See set_layer_options.c
 */
_get_ffio_rt_defaults(char *p, char *r, int n){
	return(0);
}
