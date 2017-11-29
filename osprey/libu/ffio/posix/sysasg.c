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


static char USMID[] = "@(#) libu/ffio/posix/sysasg.c	92.0	10/08/98 14:57:41";

#include <ctype.h>
#include <errno.h>
#include <ffio.h>
#include <fcntl.h>
#include <liberrno.h>
#if !defined(_ABSOFT)
#include <malloc.h>
#else
#include <stdlib.h>
#endif
#include <memory.h>
#include <stdio.h>
#include <string.h>

/*
 *	This is a SPARC-only version of this file.  The original
 *	sysasg.c file was determined to be quite dependent on a 64-bit
 *	word, and the SPARC version needs to handle only a few of the
 *	many file-types supported by CRAY.
 *	The major modifications are:
 *		1) Only handle the "system" and "f77" file types
 *		2) Remove any 64-bit dependencies for the supported types
 *		3) Rewrite non-portable code for the supported types
 *	 	   -esp. case statements using multi-character constants
 *		3) Remove numargs() usage
 */

#if !defined(_ABSOFT)
#include "../f77io.h"
#else
#include "f77io.h"
#endif

#if !defined(COMPAT_50)
# define COMPAT_50 1			/* 1 if must be compatible with 5.0 */
#endif
#if !defined(COMPAT_70)
# define COMPAT_70 1			/* 1 if must be compatible with 7.0 */
#endif

static _asgwarning(char *s);
static void _get_tok(char *resultstr, char **icp);

struct opt_s
        {
        union spec_u *tspec; /* pointer to array of FDC rec trans. spec words */
        int cvrt;       /* numeric conversion type code */
        int cvch;       /* character conversion type code */
        };
extern int optind;
extern char *optarg;

static char *Fopt_tab[NUM_PERRS+1] =
        {
        NULL,   /* not used */
        "class",
        "record format",
        "record subtype",
        "record size",
        "maximum block size",
        "minimum",
        "maximum",
        "increment",
        "specification"
        };

static
_asgwarn_num(s)
char *s;
	{
	char str[120];

	strcpy(str, s);
	strcat(str, " data conversion may not be enabled.");
	_asgwarning(str);
	}

static
_asgwarning(s)
char *s;
	{
	fprintf(stderr, "warning: ");
	fprintf(stderr,s);
	fputs("\n",stderr);
	}

static
_asgerror(s,v1,v2,v3)
char *s;
long v1, v2, v3;
	{
	fprintf(stderr, "asg: ");
	fprintf(stderr,s,v1,v2,v3);
	fputs("\n",stderr);
	}

/*
 * Crack the fchar options into a structure for manipulation.
 * Some shortcuts can be taken here as the string is generated
 * by the _bld_fchar_opts() routine, so we know that the first char is a '-'
 * and the separator is always a single space.
 */

static
_crk_fchar_opts(opts, optstr)
struct opt_s *opts;
char *optstr;
	{
	union fddcvrt_u fddcvrt;
	long tword;
	union spec_u *words;
	int c, toklen, wl, i;
	char token[256], *cp;
	long _hex2bin();

	if (optstr == NULL)
		optstr = "";
	while (*optstr == '-')
		{
		optstr++;		/* skip '-' */
		c = *optstr++;	/* pick up option letter */
		optstr++;		/* skip space */
		toklen = _cpyname(token, optstr);
		optstr += toklen;
		cp = token;
		switch (c)
			{
/*
 *			The C option word contains the version ID, the
 *			character conversion table choice and numeric
 *			conversion code choice.
 */
			case 'C': /* Foreign record translation spec */
				{
				/* check version  and conversion word */
				fddcvrt.wword = _hex2bin(cp);
				if(fddcvrt.fld.vers != ASG_VERSION)
					_asgerror("incompatible asg version",0,0,0);
				opts->cvrt = fddcvrt.fld.cvrt;
				opts->cvch = fddcvrt.fld.cvch;
				break;
				}

/*
 *			The FDC spec consists of
 *			one or more translation specification words, the last
 *			may be a  physical layer word.
 */
			case 'F': /* Foreign record translation spec */
				{
				/* 17 = 16 hex digits + '+' */
				wl = toklen / 17; /* number of words in spec */
				words = (union spec_u *)malloc((wl+1)*
							sizeof(*words));
				i = 0;
				do
					{
					tword = _hex2bin(cp);
					words[i].wword = tword;
					i++;
					cp += 17;	/* skip '+' too */
					} while(i < wl);
				words[i].wword = 0;
				opts->tspec = words;
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
static
_bld_fchar_opts(optstr, opts)
char *optstr;
struct opt_s *opts;
	{
	char *cp;
	long *sp;
	union fddcvrt_u fddcvrt;
	extern void _bin2hex();

	cp = optstr;
	if (opts->cvrt != 0 || opts->cvch != 0)
		{
		/* -C for  FOREIGN DATA conversion option */
		strcpy(cp, "-C ");
		cp += 3;
		/* build header word */
		fddcvrt.wword = 0;
		fddcvrt.fld.vers = ASG_VERSION;
		fddcvrt.fld.cvrt = opts->cvrt;
		fddcvrt.fld.cvch = opts->cvch;
		_bin2hex(cp, fddcvrt.wword);
		cp += 16;
		*cp = ' ';
		cp++;
		}
	if (opts->tspec != NULL && opts->tspec->wword != 0)
		{
		/* -F for  FOREIGN RECORD option */
		strcpy(cp, "-F ");
		cp += 3;
/*
 *		One word per FDC record spec
 */
		sp = (long *)opts->tspec;
		while(*sp != 0)
			{
			_bin2hex(cp, *sp);
			sp++;
			cp += 16; /* 16 chars and '+' */
			*cp = '+';
			cp++;
			}
		*cp = ' ';
		cp++;
		}
	*cp = '\0';
	/* return length of string produced */
	return(cp - optstr);
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
 *	      warnmode:	optional parameter.  if present and != 0, then
 *			parse_forstr will issue warning messages as necessary.
 *			The default is no issuance of warning messages.
 *	       errmode: optional parameter.  if present, this is the error
 *			handling mode understood by the _lerror routine:
 *				_LELVL_MSG	- issue message and return
 *				_LELVL_RETURN	- just return (with err stat)
 *			_LELVL_MSG is the default.
 *	returns:
 *		ERR (-1) for error
 *		length of the resultant encoded spec, including the terminator
 */

#define C_asgwarning(str) if (wmsgs) _asgwarning(str)

#define MAX_NUM_PARM	16	/* maximum number of numeric layer parameters */
int
_parse_forstr(specs, str, limit, warnmode, errmode)
union spec_u *specs;
char *str;
int limit;
int warnmode;
int errmode;
	{
	int z;
	char *class_cp, temp[9];
	int class;		/* class of record conversion */
	char pclass[9];		/* parsing class of record conversion */
	int rfc;		/* record format code */
	char prfc[9];		/* parse record format code */
	int rfc_subc;		/* subcode field */
	char prfc_subc[9];	/* parse subcode field */
	int n_paramv[MAX_NUM_PARM];
			/* validity indicators for numeric parameters */
	int n_param[MAX_NUM_PARM];
			/* array of numeric parameters */
	int par_count; 	/* number of numeric ffio layer parameters passed */
	int index;
	int err;
	int wmsgs;	/* 1 if warning messages should be printed */
	int emsgs;	/* 1 if error messages should be printed */
	int narg;
	int class_fd_num;
	extern void _get_tok();
	extern char *strpbrk();

	narg = 5;

	wmsgs = NO;
	if (warnmode != 0)
		wmsgs = YES;

	emsgs = YES;
	if (errmode < _LELVL_MSG)
		emsgs = NO;

	index = 0;
/*
 *	Loop for each comma-separated spec.
 */
	while(*str != '\0')
		{
		class_cp = str; /* save for error message */
		sprintf(pclass,"%s","");	/* parse class */
		sprintf(prfc,"%s","");	
		sprintf(prfc_subc,"%s","");

		/* find delimiter */
		_get_tok(pclass, &str);
		/* examine class  */
		if (*str == '.') {	/* record format */
			str++;
			_get_tok(prfc,&str);
		}

		if (*str == '.') {	/* sub format */
			str++;
			_get_tok(prfc_subc,&str);
		}

		par_count = 0;

		while (*str == ':') {
			char *newstr;
			if (par_count >= MAX_NUM_PARM) {
				err = FDC_PERR_BAD;	/* too many num params*/
				goto badspec;
			}
			str++;
			n_param [par_count] = strtol(str, &newstr, 10);
			n_paramv[par_count] = (str != newstr);
			par_count++;
			str = newstr;
		}

		if (*str != ',' && *str != '\0') {
			err = FDC_PERR_BAD;		/* unknown problem */
			goto badspec;
		}
		if (*str == ',')
			str++;	/* if comma, eat it... */
/*
 *		Now we have all of the tokens.  Interpret them...
 */
		class = 0;	/* class */
		rfc = 0;	/* record format code */
		rfc_subc = 0;	/* record format subcode */

/*
 *		Check to see which class we have...
 */
		if (strcmp(pclass,"null") == 0) {

			class = CLASS_NULL;

		} else if (strcmp(pclass,"system") == 0) {

			class = CLASS_SYSTEM;

		} else if (strcmp(pclass,"syscall") == 0) {

			class = CLASS_SYSCALL;

		} else if (strcmp(pclass,"user") == 0) {

			class = CLASS_USER;

		} else if (strcmp(pclass,"f77") == 0) {

			class = CLASS_F77;
			rfc = TR_UX_SUN;

		} else { 			/* unknown class */
			class = -1;
		}

/*
 *		See if format is legal...must be null
 */
		if (strcmp(prfc,"") != 0)  rfc = -1;

/*
 *		Make sure subformat was legal string.  Don't check too much
 *		now.  Leave checking for chknset().
 */
		if (strcmp(prfc_subc,"") != 0) rfc_subc = -1;
		
/*
 *		Check for valid combinations of options for each layer.
 *		Default rfc and rfc_subc values may be substituted.
 *		On return, handle any errors.
 */
		err = _chk_n_set( class, &rfc, &rfc_subc, par_count, n_param);
		if (err != 0)
			{
			char *cp;

			/* try to give the user some idea of why */
badspec:
			strncpy(temp, class_cp, 8);
			cp = strpbrk(temp, ",.:\0");
			if (cp == temp)
				temp[7] = '\0';
			else
				*cp = '\0';
			if (err < 0 || err > NUM_PERRS)
				{
				cp = "class:%s, bad -F parameter, err=%d";
				if (emsgs)
					_asgerror(cp,(long)temp,(long)err,0);
				}
			else 
				{
				cp = "class:%s, bad %s option on -F parameter";
				if (emsgs)
					_asgerror(cp,(long)temp,(long)Fopt_tab[err],0);
				}

			return(ERR);
			}
#ifdef DEBUG
		if (FDCTRACE(TRACE_ASG)) {
			int i;
			fprintf(stderr, "spec=%s, %d, %d, %d, (",
				pclass, class, rfc, rfc_subc);
			for (i=0 ; i<par_count; i++)
				fprintf(stderr, ", %c%d", n_paramv[i]?'v':'i',
					n_param[i]);
			fprintf(stderr, ")\n");
		}
#endif
/*
 * 		Set up one or more spec words for the next FFIO layer
 *		at the end of the list.
 */
		if (index >= limit) goto toomany;
		specs[index].wword	= 0; /* Whole word to zero */
		specs[index].hdr.ext	= 0; 
		specs[index].hdr.class	= class;
		specs[index].hdr.str1	= rfc;
		specs[index].hdr.str2	= rfc_subc;

		switch (class) {

		/*
		 *	The following case allows .str1.str2:num1:...:numN
		 *
		 *		num1..N		- up to 56 bits each
		 */
		default:
#if COMPAT_70
                        /*
                         * In UNICOS 7.0 and previous,  most of the layers
                         * used the "fld" spec_u format. Multiword specs were
                         * permitted.
                         */
                        if (par_count > 0)
                                specs[index].fld.recsize = n_param[0];
                        if (par_count > 1)
                                specs[index].fld.mbs     = n_param[1];
#endif
			index++;
			for (z=0; z<par_count; z++) {
				if (index >= limit) goto toomany;
				specs[index-1].info.ext	= 1; /* extend prev */
				specs[index].wword	= 0; /* zero the word */
				specs[index].info.ext	= 0;
				specs[index].info.valid	= n_paramv[z];
				specs[index].info.class	= class;
				specs[index].info.quan	= n_param[z];
				index++;
			}
			break;

		}
	}
/*
 *	Only get here if no error.  Put in the terminator, if there is room.
 */
	if (index >= limit) goto toomany;

	specs[index].wword = 0;	/* zero terminator */
	index++;
	return(index);

toomany:
	if (emsgs)
		_asgerror( "too many record translation layers.",0,0,0);
	return(ERR);
	}

/*
 * _get_tok() returns the next token as a left justified token
 * in a character string. The token is truncated to 8 chars, if longer.
 */
static void 
_get_tok(resultstr,icp)
char *resultstr;
char **icp;
	{
	extern char *strpbrk();
	char *cp, *cp2;
	int i, len;
	int maxchars = 8;

	cp = *icp;
	cp2 = strpbrk(cp, ":.,");
	if (cp2 == NULL)
		len = strlen(cp);
	else
		len = cp2 - cp;

	*icp = cp + len;

	cp2 = resultstr;
	sprintf(cp2,"%s","");

	if (len > maxchars)
		len = maxchars;	/* truncate to 8 characters */

	if (len > 0)
		for (i = 0 ; i < len ; i++)     /* convert to lower case */
			*cp2++ = tolower(cp[i]);

	*cp2 = '\0';
	}
