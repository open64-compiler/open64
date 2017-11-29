/*
 * Copyright 2005 PathScale, Inc.  All Rights Reserved.
 */

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


#pragma ident "@(#) libu/util/asnparse.c	92.4	08/02/99 10:41:11"

#include <ctype.h>
#include <errno.h>
#include <fcntl.h>
#include <fortio.h>
#ifndef	_ABSOFT
#include <malloc.h>
#endif
#include <stdlib.h>
#include <stddef.h>
#include <stdio.h>
#include <string.h>
#include <cray/assign.h>
#include <cray/fndc.h>
#include <cray/nassert.h>
#ifdef	_ABSOFT
#include "ac_sysdep.h"
#endif

/*****************************************************************************
 *	Adding new assign options.
 *
 *	Assign options parsing makes some assumptions which restrict your
 *	choices for spelling of new assign options.  These arise from
 *	issues surrounding the parsing of single-character options specified
 *	without spaces between the option and the option value, as in "-b20";
 *	as well as options which do not take an option value, as in "-ct".
 *	
 *	Backwards compatibility requires that we continue to support
 *	assign command syntax previously accepted, but steps have been taken
 *	to code command option parsing to allow multi-character options, for
 *	example "-o_parallel on".
 *
 *	When choosing the spelling of a new option, you need to:
 *
 *	 	1) Choose a name with a first letter which is not 
 *		   an existing single-character option which takes no 
 *		   option value or takes an option value other than signed
 *		   integers or "off" or "on" or "default".
 *		   These starting letters are off limits:
 *
 *			C D F I N O P R V a c d l s t v z
 *
 *		2) Choose a name whose second character is not a digit or
 *		   +/-.  These names are invalid:  "f77", "a-chew"
 *	
 *		3) Choose a name which is not a single letter followed by
 *		   "on", "off", or "default".  
 *		   These names are invalid: "won", "coff", "udefault".   
 *		   These names are OK: "wonder", "coffee", "udefaults",
 *		   (NOTE: the ProgEnv 3.0 release team has rejected use
 *		   of multi-character options.  We are trying to change 
 *		   this policy in the SGI common API working group.)
 *
 *		4) Define the option to take a value.  This simplifies
 *		   parsing.   If it is simply an on/off setting, define
 *		   the valid option values to be "on" and "off" for 
 *		   consistency.
 *
 *
 *	To add the new assign option, do the following:
 *
 *		1) Add two fields in the assign information structure in
 *		   <cray/assign.h>.  One field ends in "_flg" and one 
 *		   does not.
 *		2) If the option takes a distinct list of possible option
 *		   values, define an array of tok_list structures which
 *		   lists the possible option values.
 *		3) Add an entry in the _Ae_option_parse_info[] array of 
 *		   parse_info structures for the new option.  
 *		4) The third item in the DEFOPT macro is the name of the
 *		   option value parsing function.  If the option takes a an
 *		   integer value, list the "parse_nopt" function.  If the
 *		   option takes a distinct list of text strings, use the
 *		   "parse_topt" function.  Or you can add an option-specific 
 *		   parsing routine as well (like "parse_C" and others).
 * 
 *****************************************************************************/

/*
 *	_Ae_letters is a table which tells about letters used as 
 *	single-character assign options.  This table will not change
 *	if new assign options are always multi-letter options, a recommended
 *	practice. 
 *
 *	    'c' if it is an assign control option.
 *		(for example, -V and -R)
 *	    'n' if it is an assign attribute option without an option value
 *		(only -c and -t)
 *	    'v' if it is an assign attribute option which has an option value
 *		which is not an integer and which is not simply "on" or "off".
 *		(for example, -F and -s), 
 *	    ' ' if it is not an assign option, or if it is an assign attribute
 *		option which is a signed integer or simply "on" or "off".
 *		(for example, -h and -T), 
 */

char _Ae_letters[] = {
    /*   A    B    C    D    E    F    G    H    I    J    K    L    M	   */
 	' ', ' ', 'v', 'v', ' ', 'v', ' ', ' ', 'c', ' ', ' ', ' ', ' ',
    /*   N    O    P    Q    R    S    T    U    V    W    X    Y    Z	   */
 	'v', 'c', 'v', ' ', 'c', ' ', ' ', ' ', 'c', ' ', ' ', ' ', ' ',
    /*   [    \    ]    ^    _    ` 					   */
 	' ', ' ', ' ', ' ', ' ', ' ',
    /*   a    b    c    d    e    f    g    h    i    j    k    l    m	   */
 	'v', ' ', 'n', 'v', ' ', ' ', ' ', ' ', ' ', ' ', ' ', 'v', ' ',
    /*   n    o    p    q    r    s    t    u    v    w    x    y    z	   */
	' ', ' ', ' ', ' ', ' ', 'v', 'n', ' ', 'c', ' ', ' ', ' ', 'c',
};

/*
 *	If an option value "default" is asssigned with any option except
 *	-a, the effect is the same as if the corresponding option were not
 *	assigned.  This is convenient mostly if a combination of assign
 *	objects are being used.  For example, if "assign -P private p:%"
 *	and "assign -P default f:fort.1" are assigned the -P private option
 *	will take effect for all named files except fort.1.
 *
 *	In order to make the "default" option value most useful, it is 
 *	recommended that new assign options be defined to always take an
 *	option value.  The only attribute options to date which do not take
 *	option values are -c and -t.
 */
#define DFLT_STRING	"default"
#define DFLT_OPTVAL	(-2)
#define MAXOPTNAMELEN	30

/*
 *	Structures and typedefs.
 */

/*
 *	parsefunc is the typedef which describes the calling sequence
 *	for an option value parsing function.
 */ 

typedef int parsefunc(parse_info *tabptr, char *p, assign_info *ai,
		int warnmode, int errmode);

/*
 *	Prototypes.
 */

static parsefunc parse_nopt;
static parsefunc parse_topt;
static parsefunc parse_a;
static parsefunc parse_C;
static parsefunc parse_F;
static parsefunc parse_n;
static parsefunc parse_N;
static parsefunc parse_p;
static parsefunc parse_s;

static int _tok_match(char *str, tok_list *table);

/*
 *	Definitions for options with lists of valid option values.
 */

static tok_list d_tokens[] = {
	"skipbad",	AS_SKIPBAD, 
	"acptbad",	AS_ACPTBAD,
	NULL
};

static tok_list D_tokens[] = {
	"stdin",	0,
	"stdout",	1,
	"stderr",	2,
	NULL
};

static tok_list f_tokens[] = {
	"77",		AS_FORTRAN77,
	"90",		AS_FORTRAN90,
	"irixf77",	AS_IRIX_F77,
	"irixf90",	AS_IRIX_F90,
	NULL
};

static tok_list l_tokens[] = {
	"none",		(int)'n',
	"0",		(int)'l',
	"ldcache",	(int)'l',
	"1",		(int)'f',
	"full",		(int)'f',
	NULL
};

static tok_list P_tokens[] = {
	"thread",	AS_THREAD,
	"process",	AS_PROCESS,
	"team",		AS_TEAM,
	"private",	AS_PRIVATE,
	"global",	AS_GLOBAL,
	NULL
};

static tok_list s_tokens[] = {
	"text",		FS_TEXT,
	"cos",		FS_COS,
	"blocked",	FS_COS,
	"unblocked",	FS_UNBLOCKED,
	"u",		FS_U,
	"sbin",		STD,
	"bin",		FS_BIN,
	"tape",		FS_TAPE,
	"tape50",	FS_TAPE50,
	"bmx",		FS_TAPE,
	"bmx50",	FS_TAPE50,
	NULL
};

/*
 *	Several options are "on"/"off" options.
 */
static tok_list offon_tokens[] = {
	"off",		0,
	"on",		1,
	NULL
};

#ifdef KEY /* Bug 5921 */
/*
 * Default clears all flags in assign_info, but for some options (e.g. "-S"),
 * we want the default to be the same as "on". Use these with parse_topt to
 * accomplish that.
 */
static tok_list dflt_on_tokens[] = {
	"off",		1,
	"on",		0,
	NULL
};
#endif /* KEY Bug 5921 */

/*
 *	DEFOPT helps initialize the _Ae_option_parse_info[] table.
 */

#define	DEFOPT(qname, name, func, tokenlist, sup, desc)  { \
	qname, \
	offsetof(assign_info, name ## _flg), \
	offsetof(assign_info, name), \
	(func), \
	(tokenlist), \
	(sup), \
	(desc) }

/*
 *	Flags to indicate supported architectures.
 */

#ifdef _CRAY1
#define C1 1		/* PVP systems */
#else
#define C1 0
#endif

#ifdef _UNICOS
#define U 1		/* UNICOS (MAX/mk) systems */
#else
#define U 0
#endif

#ifdef _CRAYT3E		/* T3E systems */
#define E 1
#else
#define E 0
#endif

#ifdef __mips
#define MI 1		/* MIPS systems */
#else
#define MI 0
#endif

#if	defined(_LITTLE_ENDIAN) && defined(__sv2)
#define MI 1		/* LITTLE ENDIAN on sv2 is same as mips */
#define L 0		/* LITTLE ENDIAN systems */
#elif	defined(_LITTLE_ENDIAN) && !defined(__sv2)
#define MI 0		/* LITTLE ENDIAN without sv2 */
#define LI 1		/* LITTLE ENDIAN systems */
#else
#define LI 0		/* LITTLE ENDIAN systems */
#endif

/*
 *	T H E    T A B L E    O F    A S S I G N    O P T I O N S
 */

parse_info _Ae_option_parse_info[] = {
  /*
	 Option		  assign_info	  Option value	  List of      Supported
   	 name		  Field		  parse		  option       on this
					  routine	  values       system
  */
  DEFOPT("-a"		, a_actfil	, parse_a	, NULL		, 1 ,"\
  -a actname            Actual file name"),
  
  DEFOPT("-b"		, b_bufsiz	, parse_nopt	, NULL		, 1 ,"\
  -b bs                 Buffer size in 4096-byte blocks"),
  
  DEFOPT("-B"		, B_direct	, parse_topt	, offon_tokens	, MI ,"\
  -B on/off             Specify O_DIRECT on open(2) to bypass system cache"),
  
  DEFOPT("-c"		, c_contig	, NULL		, NULL		, U ,"\
  -c                    Contiguous space only"),
  
  DEFOPT("-C"		, C_chrcnv	, parse_C	, NULL		, 1 ,"\
  -C charcon            Foreign character conversion"),
  
  DEFOPT("-d"		, d_datrcv	, parse_topt	, d_tokens	, U ,"\
  -d bdr                Bad data recovery for online tape files"),
  
  DEFOPT("-D"		, D_fildes	, parse_topt	, D_tokens 	, 1 ,"\
  -D fildes             Specify connection to stdin, stdout, or stderr"),
  
  DEFOPT("-f"		, f_fortst	, parse_topt	, f_tokens	, 1 ,"\
  -f fortran_mode       Fortran compatibility mode: 77, 90, irixf77, or irixf90"),
  
  DEFOPT("-F"		, F_filter	, parse_F	, NULL		, 1 ,"\
  -F ffio               Flexible file I/O specification"),
  
  DEFOPT("-l"		, l_buflev	, parse_topt	, l_tokens	, U ,"\
  -l none/ldcache/full  Ldcache and system buffer cache utilization"),

  DEFOPT("-L"		, L_ldraw	, parse_topt	, offon_tokens	, C1,"\
  -L on/off             Specify O_LDRAW on open(2) to bypass ldcache"),
  
  DEFOPT("-m"		, m_multup	, parse_topt	, offon_tokens	, U ,"\
  -m on/off             Concurrent file access by multiple PEs or threads"),
  
  DEFOPT("-n"		, n_preall	, parse_n	, NULL		, U ,"\
  -n psz                Pre-allocation size in 4096-byte blocks"),
  
  DEFOPT("-N"		, N_datcnv	, parse_N	, NULL		, 1 ,"\
  -N numcon             Foreign data conversion"),
  
  DEFOPT("-p"		, pr_partit	, parse_p	, NULL		, U ,"\
  -p sparts             Disk stripe partition list"),
  
  DEFOPT("-P"		, P_ioscop	, parse_topt	, P_tokens	, U ,"\
  -P scope              Make a file global or local to a thread or PE"),
  
  DEFOPT("-q"		, q_ocblks	, parse_nopt	, NULL		, U ,"\
  -q ocblks             cblks value passed to open(2)"),

  DEFOPT("-r"		, r_raw		, parse_topt	, offon_tokens	, U ,"\
  -r on/off             Specify O_RAW on open(2) to bypass system cache"),
  
  DEFOPT("-s"		, s_fstrct	, parse_s	, NULL		, 1 ,"\
  -s ft                 File type: cos, sbin, tape, text, u, or unblocked"),
  
#ifdef KEY /* Bug 5921 */
  /*
   * Historically, "-S off" meant S_comsep == 0, which nonsensically meant to
   * use commas in list-directed output. We want "-S on" to be the default, so
   * "on" now sets S_comsep == 0 which now means not to use commas.
   */
  DEFOPT("-S"		, S_comsep	, parse_topt	, dflt_on_tokens, 1 ,"\
  -S on/off             Don't/do use comma as separator in list_directed output"),
#else /* KEY Bug 5921 */
  DEFOPT("-S"		, S_comsep	, parse_topt	, offon_tokens	, 1 ,"\
  -S on/off             Use comma as separator in list_directed output"),
#endif /* KEY Bug 5921 */

  DEFOPT("-t"		, t_tmpfil	, NULL		, NULL		, 1 ,"\
  -t                    Temporary (scratch) file"),
  
  DEFOPT("-T"		, T_utrunc	, parse_topt	, offon_tokens	, 1 ,"\
  -T on/off             File truncation after sequential write"),
  
  DEFOPT("-u"		, u_bufcnt	, parse_nopt	, NULL		, U ,"\
  -u bufcnt             Buffer count for multi-buffer file types"),
  
  DEFOPT("-U"		, U_unicoslist	, parse_topt	, offon_tokens	, 1 ,"\
  -U on/off             Produce unicos list_directed output form"),

  DEFOPT("-w"		, w_welfrm	, parse_topt	, offon_tokens	, U ,"\
  -w on/off             Specify O_WELLFORMED on open(2) for I/O size enforcement"),

  DEFOPT("-W"		, W_compwidth	, parse_topt	, offon_tokens	, 1 ,"\
  -W on/off             Produce compressed list_directed output fields"),

  DEFOPT("-x"		, x_parallel	, parse_topt	, offon_tokens	, E ,"\
  -x on/off             Specify O_PARALLEL on open(2)"),

#ifdef KEY /* Bug 5921 */
  /*
   * Historically, "-y off" meant y_reptcnt == 0, which nonsensically meant to
   * emit a repeat count in list-directed output. We want "-y on" to be the
   * default, so "on" now sets y_reptcnt == 0, which now means not to use a
   * repeat count.
   */
  DEFOPT("-y"		, y_reptcnt	, parse_topt	, dflt_on_tokens, 1 ,"\
  -y on/off             Don't/do produce repeat count for list_directed output"),
#else /* KEY Bug 5921 */
  DEFOPT("-y"		, y_reptcnt	, parse_topt	, offon_tokens	, 1 ,"\
  -y on/off             Produce repeat count for list_directed output"),
#endif /* KEY Bug 5921 */

  DEFOPT("-Y"		, Y_nl_skip	, parse_topt	, offon_tokens	, 1 ,"\
  -Y on/off             Skip namelist input record if group name mismatch"),
  DEFOPT("-Z"		, Z_neg_zero	, parse_topt	, offon_tokens	, 1 ,"\
  -Z on/off             Skip write of negative sign for -0.0 for formatted io"),

  NULL		/* mark the end of options */

};
	

/*
 * _ae_parse	Parses an assign record into assign info block format.
 *		"ai" receives the parsed information.
 *
 *	Return status:  0 on success, -1 on error (with errno set to error code)
 */
_ae_parse(
assign_obj_id	*aoidp,		/* assign object id */
char		*attr,		/* pointer to string of attributes */
int		attrlen,	/* length of string */
assign_info	*aip,
int		warnmode,	/* 1 if warnings should be printed */
int		errmode)	/* _LELVL_xxx error handling mode.  0 means   */
				/* return -1 with errno set to the error      */
				/* status.				      */
{
	int  i;
	char *copy;
	char *p, *last;
	char *newp, *p2;
	char c;
	int  ss;
	parse_info *tabptr;
	char curopt[MAXOPTNAMELEN];

	/*
 	 * Zero all assign attributes flags.
	 */ 
	(void) memset (aip, 0,
		offsetof(assign_info, flagpad) + sizeof(aip->flagpad));

	if (attr == NULL)
		return(0);		/* no attributes */

	/*
	 * Copy the attributes string into a local buffer.  The parsing 
	 * routines alter the string.
	 */
	copy = malloc(attrlen + 1);
	if (copy == NULL) {
		RETERR(errmode,FENOMEMY);
	}
	(void)strncpy(copy,attr,attrlen);
	copy[attrlen] = '\0';
	p = copy;

	last = p + strlen(p);		/* pointer to trailing null byte */

/*
 *	Each iteration of this loop processes one assign option and its
 *	option value.
 */
	
	while (p < last) {

		_AE_SKIPWHITE(p);

		if (*p == '\0')
			break;

		i = 0;
		while ( !isspace(*p) && *p != '\0')
			curopt[i++] = *p++;
		curopt[i] = '\0';

		if (curopt[0] != '-') {
			free(copy);
			RETERR1(errmode, ERAS_UNOPT, curopt);
		}

		/*
		 * Look up the current option in the table of known options.
		 */

		tabptr = &_Ae_option_parse_info[0];

		while (tabptr->optname != NULL) {
			if (strcmp(curopt, tabptr->optname) == 0)
				break;
			tabptr++;
		}

		if (tabptr->optname == NULL) {
			free(copy);
			RETERR1(errmode, ERAS_UNOPT, curopt);
		}

		/*
		 * If there is an option value, then find it and null 
		 * terminate it.
		 */

		_AE_SKIPWHITE(p);	/* skip space until option value */

		if (tabptr->pfunc == NULL) {
			newp = p;		/* no option value */
		}
		else {
			/*
			 * Options which have option values.
			 */
			p2 = strpbrk(p," \t");	/* scan to space or tab */
			if (p2 == NULL)
				p2 = last;
			else
				*p2 = '\0';	/* null terminate option value*/
			if (p == p2) {
				free(copy);
				RETERR(errmode, ERAS_NOOBJS);
			}
			newp = p2 + 1;
		}

		/*
		 * Parse the option value if there is one.
		 */

		ss = 0;
		if (tabptr->pfunc != NULL) {
			ss = ((parsefunc *)(tabptr->pfunc))(tabptr, p, aip,
							    warnmode, errmode);
		}
		else {	/* -c and -t do not have option values */
			if (strcmp("-c", tabptr->optname) == 0) {
				if (tabptr->sup)
					aip->c_contig_flg = ATTR_SET;
				else if (warnmode)
					_lwarn(WNAS_ANSUPY,tabptr->optname);
			}
			else if (strcmp("-t", tabptr->optname) == 0) {
				if (tabptr->sup)
					aip->t_tmpfil_flg = ATTR_SET;
				else if (warnmode)
					_lwarn(WNAS_ANSUPY,tabptr->optname);
			}
			else 
				assert (0);	/* deep weeds */
		}
		if (ss == -1) {
			free(copy);
			return(-1);
		}
		p = newp;
	}
		
	ss = _ae_check_attr(aip, warnmode, errmode);
	if (ss == -1) {
		free(copy);
		return(-1);
	}
	free(copy);
	return(0);
}

/*
 * The parse_X routines all return 0 on success, and -1 on error with
 * errno set to the error status.  An error results in program termination
 * if (errmode >= _LELVL_EXIT).
 */

/*
 * parse_topt - parses options with option values which are text strings.
 */
static int
parse_topt(
parse_info	*tabptr,	/* parse table entry for current option */
char 		*p,		/* pointer to string containing option value */
assign_info	*ai,		/* info block to receive parsed data */
int		warnmode,
int		errmode)
{
	int ret;

	ret = _tok_match(p, tabptr->tl);

	if (ret == -1)
		RETERR2(errmode, ERAS_OPTVAL2, p, tabptr->optname);

	if (! tabptr->sup) {
		if (warnmode)
			_lwarn(WNAS_ANSUPY, tabptr->optname);
		return(0);
	}

	*(char *)((char *)ai + tabptr->flg_off) =
		(ret == DFLT_OPTVAL) ? 0 : ATTR_SET;
	*(int *) ((char *)ai + tabptr->val_off) =
		(ret == DFLT_OPTVAL) ? 0 : ret;

	return(0);
}

/*
 * parse_nopt - parses options with integer option values.
 */

static int
parse_nopt(
parse_info	*tabptr,	/* parse table entry for current option */
char 		*p,		/* pointer to string containing value */
assign_info	*ai,		/* info block to receive parsed data */
int		warnmode,
int		errmode)
{
	char *endp;
	int num;
	int dflt_val = 0;

	num = strtol(p, &endp, DECIMAL);
	if (endp == p || *endp != '\0' || num <= 0) {
		if (strcmp(p, DFLT_STRING) == 0) 
			dflt_val = 1;
		else
			RETERR2(errmode, ERAS_OPTVAL2, p, tabptr->optname);
	}

	if (! tabptr->sup) {
		if (warnmode) 
			_lwarn(WNAS_ANSUPY, tabptr->optname);
		return(0);
	}

	*(char *)((char *)ai + tabptr->flg_off) = dflt_val ? 0 : ATTR_SET;
	*(int *) ((char *)ai + tabptr->val_off) = dflt_val ? 0 : num;

	return(0);
}


/*
 *	check_for_default returns 1 if the option value is DFLT_STRING,
 *	0 otherwise.
 */
static int
check_for_default(
char		*p,
aflg_t		*flg)
{
	if (strcmp(p, DFLT_STRING) == 0) {
		*flg = 0;
		return(1);
	}
	else
		return(0);
}

static
parse_a(
parse_info	*tabptr,	/* parse table entry for current option */
char 		*p,		/* pointer to string containing value */
assign_info	*ai,		/* info block where data is placed */
int		warnmode,
int		errmode)
{
	if (strcmp("SDS",p) == 0) {
		if (warnmode)
			_lwarn(WNAS_DEPASDS);
		ai->a_sdsfil_flg = ATTR_SET;
	}
	else {
		(void)strncpy(ai->a_actfil,p,PATH_MAX);
		ai->a_actfil_flg = ATTR_SET;
	}
	return(0);
}

static
parse_C(
parse_info	*tabptr,	/* parse table entry for current option */
char 		*p,		/* pointer to string containing value */
assign_info	*ai,		/* info block where data is placed */
int		warnmode,	
int		errmode)
{
	int  val;
	char *tp;

	if (check_for_default(p, &ai->C_chrcnv_flg))
		return(0);

	if (! tabptr->sup) {
		if (warnmode) 
			_lwarn(WNAS_ANSUPY, tabptr->optname);
		return(0);
	}
	
#if	NUMERIC_DATA_CONVERSION_ENABLED
	tp = strdup(p);
	if (tp == NULL) {
		RETERR(errmode, FENOMEMY);
	}
	val = _parse_cvch(tp);
	free(tp);
	if (val == -1) {
		RETERR2(errmode, ERAS_OPTVAL2,p, tabptr->optname);
	}
	ai->C_chrcnv = val;
	ai->C_chrcnv_flg = ATTR_SET;
#endif	/* NUMERIC_DATA_CONVERSION_ENABLED */

	return(0);
}

static
parse_F(
parse_info	*tabptr,	/* parse table entry for current option */
char 		*p,		/* pointer to string containing value */
assign_info	*ai,		/* info block where data is placed */
int		warnmode,	/* 1 if warnings should be printed */
int		errmode)	
{
#define HEXLEN 16	/* length of hex-encoded format */
	int parselen;

	if (check_for_default(p, &ai->F_filter_flg))
		return(0);

	if (! tabptr->sup) {
		if (warnmode) 
			_lwarn(WNAS_ANSUPY, tabptr->optname);
		return(0);
	}
	
	parselen = _parse_forstr(&ai->F_filter[0], p, MAX_FDC_SPEC,
			         warnmode, errmode);

	/*
	 * Until _parse_forstr can handle all errmodes by calling
	 * _lerror, we need to handle _LELVL_ABORT after the return
	 * from _parse_forstr.
	 */
	if (parselen == -1) {
		_lerror(errmode,0);	/* 0 suppresses message */
		errno = ERAS_OPTVAL1;
		return(-1);
	}
 	ai->F_filter_len = parselen;	/* length in words */
	ai->F_filter_flg = ATTR_SET;

	return(0);
}

static
parse_n(
parse_info	*tabptr,	/* parse table entry for current option */
char 		*p,		/* pointer to string containing value */
assign_info	*ai,		/* info block where data is placed */
int		warnmode,	
int		errmode)
{
	char *endp, *tmp;
	int num;

	if (check_for_default(p, &ai->n_stride_flg)) {
		ai->n_preall_flg = 0;
		return(0);
	}

	if (! tabptr->sup) {
		if (warnmode) 
			_lwarn(WNAS_ANSUPY, tabptr->optname);
		return(0);
	}

#ifdef	_UNICOS
	num = strtol(p, &endp, DECIMAL);
	if (endp == p || (*endp != '\0' && *endp != ':') || num <= 0) {
		RETERR2(errmode, ERAS_OPTVAL2, p, tabptr->optname);
	}
	ai->n_preall = num;
	ai->n_preall_flg = ATTR_SET;
	tmp = strchr(p,':');	/* check for -n x:y */
	if (tmp != NULL) {
		p = tmp+1;
		num = strtol(p, &endp, DECIMAL);
		if (endp == p || *endp != '\0') {
			RETERR2(errmode,ERAS_OPTVAL2, p, tabptr->optname);
		}
		ai->n_stride = num;
		ai->n_stride_flg = ATTR_SET;
	}
#endif	/* _UNICOS */

	return(0);
}

static
parse_N(
parse_info	*tabptr,	/* parse table entry for current option */
char 		*p,		/* pointer to string containing value */
assign_info	*ai,		/* info block where data is placed */
int		warnmode,	
int		errmode)
{
	int val;
	char *tp;

	if (check_for_default(p, &ai->N_datcnv_flg))
		return(0);

	if (! tabptr->sup) {
		if (warnmode) 
			_lwarn(WNAS_ANSUPY, tabptr->optname);
		return(0);
	}

#if	NUMERIC_DATA_CONVERSION_ENABLED

	tp = strdup(p);
	if (tp == NULL) {
		RETERR(errmode,FENOMEMY);
	}
	val = _parse_cvrt(tp, 1);
	free(tp);
	if (val == -1) {
		RETERR2(errmode, ERAS_OPTVAL2, p, tabptr->optname);
	}
	ai->N_datcnv = val;
	ai->N_datcnv_flg = ATTR_SET;
#endif	/* NUMERIC_DATA_CONVERSION_ENABLED */

	return(0);
}

static
parse_p(
parse_info	*tabptr,	/* parse table entry for current option */
char 		*p,		/* pointer to string containing value */
assign_info	*ai,		/* info block where data is placed */
int		warnmode,	
int		errmode)
{
	char *part, *next, *dash;
	long bit1, bit2;
	long fullmask, msk;

	if (check_for_default(p, &ai->pr_partit_flg))
		return(0);

	if (! tabptr->sup) {
		if (warnmode) 
			_lwarn(WNAS_ANSUPY, tabptr->optname);
		return(0);
	}

#ifdef	_UNICOS
	part = p;
	if (strspn(part,"0123456789-:") != strlen(part)) {
		RETERR1(errmode,ERAS_ATTRP,part);
	}

	next = part;
	fullmask = 0;
	while (next) {
		next = strchr(part,':');
		if (next != NULL) {
			*next = '\0';
			next++;
		}
		dash = strchr(part,'-');
		if (dash == NULL) {
			/*  A single partition is given  */
			bit1 = atol(part);
			if (bit1 < 0 || bit1 > 63) {
				RETERR1(errmode,ERAS_ATTRP,part);
			}
			bit2 = bit1;
		}
		else {
			*dash = '\0';
			dash++;
			bit1 = atol(part);
			if (bit1 < 0 || bit1 > 63) {
				RETERR1(errmode,ERAS_ATTRP,part);
			}
			bit2 = atol(dash);
			if (bit2 < 0 || bit2 > 63 || bit2 < bit1) {
				RETERR1(errmode,ERAS_ATTRP,dash);
			}
		}
		msk = (_mask(128-(bit2-bit1+1)))<<bit1;
		fullmask |= msk;
		part = next;
	}
	ai->pr_partit = fullmask;
	ai->pr_partit_flg = ATTR_SET;
#endif	/* _UNICOS */

	return(0);
}

static
parse_s(
parse_info	*tabptr,	/* parse table entry for current option */
char 		*p,		/* pointer to string containing value */
assign_info	*ai,		/* info block where data is placed */
int		warnmode,	
int		errmode)
{
	int val;
	int supported = 1;	/* set to 0 if file structure not supported */
	
	if (check_for_default(p, &ai->s_fstrct_flg))
		return(0);

	val = _tok_match(p,s_tokens);	/* code, or 0 if no match */

	switch (val) {

	case STD:
		val = STD;
		break;
#ifndef	_UNICOS
#if	!defined(__mips) && !defined(_LITTLE_ENDIAN)
	case FS_COS:
#endif
	case FS_TAPE:
	case FS_TAPE50:
		supported = 0;
		break;
#endif
	case DFLT_OPTVAL:
		break;			/* "default" */
	case -1:
		RETERR2(errmode, ERAS_OPTVAL2, p, tabptr->optname);
	}

	if (!supported) {
#if	defined(__mips)
		char *craysys = "MIPS";
#elif	defined(_CRAY1)
		char *craysys = "CRAY PVP";
#elif	defined(_CRAYMPP)
		char *craysys = "CRAY MPP";
#elif	defined(_SOLARIS)
		char *craysys = "Solaris";
#elif	defined(_ABSOFT)
		char *craysys = AC_RUN_HOST_SYS;
#elif	defined(_LITTLE_ENDIAN)
		char *craysys = "LITTLE ENDIAN";
#endif
		RETERR2(errmode,ERAS_FSNSUP,p,craysys);
	}

	if (supported && val != DFLT_OPTVAL) {
		ai->s_fstrct = val;
		ai->s_fstrct_flg = ATTR_SET;
	}
	else {
		ai->s_fstrct = 0;
		ai->s_fstrct_flg = 0;
	}
	return(0);
}

/*
 * _ae_check_attr checks for invalid combinations of attributes.
 */
_ae_check_attr(
assign_info	*ai,		/* info block where data is placed */
int		warnmode,	/* 1 if warnings should be printed */
int		errmode)	/* _LELVL_xxx error mode selected */
{

/*
 *	Check for invalid combinations of attributes.
 */
	if (ai->a_actfil_flg && ai->D_fildes_flg) {
		RETERR(errmode,ERAS_MIXAD);
	}

	if (ai->F_filter_flg && ai->s_fstrct_flg) {
		RETERR(errmode,ERAS_MIXFS);
	}

        if (ai->F_filter_flg &&
	    (   ai->b_bufsiz_flg	||
		ai->u_bufcnt_flg)) {

		RETERR(errmode,ERAS_MIXFO);
	}

	if (ai->c_contig_flg && !ai->n_preall_flg && warnmode) {
		RETERR(errmode,ERAS_COREQN);
	}

	/* -m on with -T on is not valid */
	if (ai->m_multup_flg && ai->m_multup == 1 &&
	    ai->T_utrunc_flg && ai->T_utrunc == 1) {
		RETERR(errmode,ERAS_MTRUNC);
	}

/*
 *	-l cannot be used with newer options -r or -L .
 *	Validate this and then copy then map -l to an equivalent
 *	-r/-L specification.
 */
	/* -r and -L should not be used with -l */
	if ((ai->r_raw_flg && ai->l_buflev_flg) ||
	    (ai->L_ldraw_flg && ai->l_buflev_flg) ) {
		RETERR(errmode, ERAS_LRAW);
	}

/*
 *	Map -l to the equivalent -r or -L option.
 */
	if (ai->l_buflev_flg) {
		ai->r_raw_flg = 1;
		ai->L_ldraw_flg = 1;
		switch (ai->l_buflev) {
			case 'n': ai->r_raw = 1; ai->L_ldraw = 1; break;
			case 'l': ai->r_raw = 1; ai->L_ldraw = 0; break;
			case 'f': ai->r_raw = 0; ai->L_ldraw = 0; break;
		}
	}
	


/*
 *	Issue warnings for some combinations of attributes.
 */
	if (warnmode) {
		if (ai->u_bufcnt_flg && ai->s_fstrct_flg
			&& ai->s_fstrct != FS_TAPE50
			&& ai->s_fstrct != FS_TAPE
			&& ai->s_fstrct != FS_UNBLOCKED)
			_lwarn(WNAS_UBMX);

		if (ai->n_stride_flg && ai->q_ocblks_flg)
			_lwarn(WNAS_QNSTRD, ai->q_ocblks, ai->n_stride);
	}

	return(0);
}

/*
 * _tok_match searches "table" for string "str".  If found, the corresponding
 * code in the table is returned.  If not found, -1 is returned.  If "default",
 * then DFLT_OPTVAL is returned.
 */ 
static
_tok_match(
char *str,
tok_list *table)
{
	tok_list	*tp;
	
	for (tp = table; tp->str != NULL; tp++) 
		if (strcmp(str,tp->str) == 0)
			return(tp->code);

	if (strcmp(str,DFLT_STRING) == 0)
		return(DFLT_OPTVAL);

	return(-1);
}

/*
 * _attr_save
 *
 *		Make a copy of all the ATTR_USED & ATTR_SET bits in an
 *		assign_info structure.
 */
void
_attr_copy( assign_info *aip, assign_info *copy )
{
	memcpy(copy, aip, AFLAGSIZE);
}

/*
 * _attr_used 	
 *
 *		Helps verify that user-specified assign attributes were 
 *		used.   
 *
 *		This routine is most helpful for verifying that certain
 *		attributes have been handled by FFIO.
 *
 *		Note that some attributes like -b/-l/-q/-u/-w are not
 *		verified this way because they might not apply to
 *		all files.   
 *
 * Return value
 *		0 on success.  -1 on error with errno set.
 */
int
_attr_used( assign_info *aip, char **attrstr )
{
	if (aip == NULL)
		return(0);
/*
 *	Verify that -m on has been handled.  -m off is the default and need 
 *	not be verified.
 */
 	if (aip->m_multup_flg && aip->m_multup) {
		if ((aip->m_multup_flg & ATTR_USED) == 0) {
			*attrstr = "-m on";
			goto nosup_error;
		}
	}

	return(0);

nosup_error:
	errno = FEANOSUP;
	return(-1);
}
