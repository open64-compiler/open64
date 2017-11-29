/*
 * Copyright 2005, 2006 PathScale, Inc.  All Rights Reserved.
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


#pragma ident "@(#) libu/util/asnlib.c	92.1	07/07/99 13:18:33"

#define _ISOC99_SOURCE 1

#include <stdio.h>
#include <fcntl.h>
#include <ctype.h>
#include <errno.h>
#ifndef	_ABSOFT
#include <malloc.h>
#ifdef KEY /* Bug 4260 */
#include <stdlib.h>	/* For getenv */
#endif /* KEY Bug 4260 */
#else
#include <stdlib.h>
#endif
#include <fortran.h>
#include <string.h>
#include <liberrno.h>
#include <fortran.h>
#include <cray/assign.h>

enum assign_modes {
	UPDATE_INCREMENT	= 1,	/* add attributes */
	UPDATE_REPLACE		= 2,	/* replace attributes */
	UPDATE_REMOVE		= 3,	/* remove an assign object */
	REMOVE_ALL		= 4,	/* remove all assign objects */
	VIEW_RECORD		= 5,	/* view one assign record */
	VIEW_ALL		= 6	/* view all assign records */
};

#ifndef	MAX
#define MAX(a,b)	((a)>(b)?(a):(b))
#endif

/*
 * ASSIGN
 *
 *	Usage:
 *
 *		CALL ASSIGN(CMD [, IER])
 *
 *		where
 *
 *			CMD is a fortran character variable which contains
 *			a valid assign command in the form acceptable
 *			to the shell, with these exceptions:
 *
 *				1) No shell variable substitution is
 *				   done.
 *
 *				2) A space is required between an option 
 *				   and the option value.
 *
 *			IER is an optional integer parameter on PVP and MPP
 *			systems (required on Solaris).  If supplied, 
 * 			it is assigned 0 on successful return.  Otherwise
 *			a positive error code is returned.
 */

#ifdef KEY /* Bug 8391 */
/* SGI historically provided this  function, but we don't want it to collide
 * with a user-coded Fortran procedure "assign" which will also emit "assign_"
 */
#if defined(BUILD_OS_DARWIN)
/* Mach-O doesn't support aliases */
extern void __assign(char *, _f_int *ier, int);
void assign_(char *cmdargs_ptr, _f_int *ier, int cmdargs_len) {
  __assign(cmdargs_ptr, ier, cmdargs_len);
}
#else /* defined(BUILD_OS_DARWIN) */
#pragma weak assign_ = __assign
extern void assign_(char *, _f_int *ier, int);
#endif /* defined(BUILD_OS_DARWIN) */
#endif  /* KEY Bug 8391 */

void
#ifdef	_UNICOS

ASSIGN(
	_fcd	cmdargs,	/* Fortran string containing assign request */
	_f_int	*ier)		/* optional error status variable */
{

#else

#ifdef KEY /* Bug 4260 */
__assign(
#else /* KEY Bug 4260 */
assign_(
#endif /* KEY Bug 4260 */
	char	*cmdargs_ptr,	/* Fortran string containing assign request */
	_f_int	*ier,		/* required error status variable. */
	int	cmdargs_len)	/* length of cmdargs string */
{
	_fcd	cmdargs	= _cptofcd(cmdargs_ptr, cmdargs_len);

#endif
	char 		*buf;
	char 		*opts;
	char 		*objtxt;
	int		len;
	assign_obj_id	aobj, *obptr;
	int		errmode;
	int		ier_valid;

#ifdef	_UNICOS
	ier_valid	= (_numargs() * sizeof(long) > sizeof(_fcd));
#else
	ier_valid	= (ier != NULL) ? 1 : 0;/* if NULL on SOLARIS */
#endif

	errmode	= ier_valid ? _LELVL_RETURN : _LELVL_ABORT;

#ifdef	_UNICOS
	if (_numargs() < 1) 
		_lerror(errmode, FEARGCNT, "ASSIGN", _numargs(), "1 or 2");
#endif

	len	= _fcdlen(cmdargs);
	buf	= malloc(len+1);

	if (buf == NULL) {
		errno	= FENOMEMY;
		goto assign_error;
	}

	memcpy(buf, _fcdtocp(cmdargs), len);	/* copy the string */

	buf[len]	= '\0';			/* NULL terminate it */

	if (_asndir_split(buf, &opts, &objtxt, 0) == -1)
		goto assign_error;

	if (*objtxt == '\0')
		obptr	= NULL;
	else {
		if (_lae_get_object(objtxt,&aobj) == -1) 
			goto assign_error;
		obptr	= &aobj;
	}
	
	if (_assign(opts,obptr,errmode) == -1)
		goto assign_error;

	free(buf);

	if (ier_valid)
		*ier	= 0;

	return;		/* normal return */

assign_error:

	if (buf != NULL)
		free(buf);

	_lerror(errmode,errno);		/* control never returns if	*/
					/* errmode == _LELVL_ABORT	*/

	if (ier_valid)
		*ier	= errno;

	return;		/* error return */
}

/*
 * ffassign() 	- C-callable function which is functionally 
 *		  equivalent to Fortran-callable ASSIGN().
 *
 * Return value:
 *	0 on success
 *	-1 on error, with errno set to the error code.
 */
int
ffassign(char *cmd)
{
	_f_int	ier;

#ifdef	_UNICOS
	ASSIGN (_cptofcd(cmd,strlen(cmd)), &ier);
#else
#ifdef KEY /* Bug 4260 */
	__assign(cmd, &ier, strlen(cmd));
#else /* KEY Bug 4260 */
	assign_(cmd, &ier, strlen(cmd));
#endif /* KEY Bug 4260 */
#endif
	if (ier != 0) {
		errno	= ier;
		return(-1);
	}

	return(0);
}

/*
 * ASNFILE	- assigns attributes to a file.
 *
 * CALL ASNUNIT(IUN, OPTIONS, ISTAT)
 *
 * Parameters
 *
 *	FNAME	(I) character file name
 *	OPTIONS (I) character assign options to be assigned to FNAME
 *	ISTAT	(O) integer status--0 on normal return, >0 error code on error.
 */
void
#ifdef	_UNICOS
ASNFILE(_fcd fname, _fcd options, _f_int *ier)
{
#else
asnfile_(char *fname_ptr, char *options_ptr, _f_int *ier, int fname_len,
	int options_len)
{
	_fcd fname	= _cptofcd(fname_ptr, fname_len);
	_fcd options	= _cptofcd(options_ptr, options_len);
#endif
	assign_obj_id	aobj;
	char		*cfn, *copts;
	int		ss;
	char		*pp;

	*ier	= 0;
	cfn	= NULL;
	copts	= NULL;

	cfn	= _fc_acopy(fname); /* copy _fcd string, trimming trailing blanks*/

	if (cfn == NULL) {
		*ier	= FENOMEMY;
		goto done;
	}

	pp	= strchr(cfn,' '); /* check for imbedded blanks in file name */

	if (pp != NULL) {
		*ier	= ERAS_FNBL;
		goto done;
	}

	copts	= _fc_acopy(options);

	if (copts == NULL) {
		*ier	= FENOMEMY;
		goto done;
	}

	aobj.u.str	= cfn;
	aobj.type	= BYFILE;

	ASN_DEBUG(("ASNFILE: calling _assign with <%s>\n",copts));

	if ((ss = _assign(copts,&aobj,_LELVL_RETURN)) == -1) {
		*ier	= errno;
		goto done;
	}

done:
	if (cfn != NULL)
		free(cfn);

	if (copts != NULL)
		free(copts);

	return;
}

/*
 * ASNUNIT	- assigns attributes to a Fortran unit.
 *
 *
 * CALL ASNUNIT(IUN, OPTIONS, ISTAT)
 *
 * Parameters
 *
 *	IUN	(I) integer unit number
 *	OPTIONS	(I) character assign options
 *	ISTAT	(O) integer status--0 on normal return, >0 error code on error.
 */
void
#ifdef	_UNICOS

ASNUNIT(_f_int *unum, _fcd options, _f_int *ier)
{

#else

asnunit_(_f_int *unum, char *options_ptr, _f_int *ier, int options_len)
{
	_fcd options = _cptofcd(options_ptr, options_len);

#endif
	assign_obj_id	aobj;
	char		*copts;
	int		len;
	int		ss;

	*ier	= 0;
	copts	= NULL;


	len	= _fcdlen(options);
	copts	= malloc(len+1);

	if (copts == NULL) {
		*ier	= FENOMEMY;
		goto done;
	}
	memcpy(copts,_fcdtocp(options),len);
	copts[len]	= '\0';

	aobj.u.unit	= *unum;
	aobj.type	= BYUNIT;

	if ((ss = _assign(copts,&aobj,_LELVL_RETURN)) == -1) {
		*ier	= errno;
		goto done;
	}

done:
	if (copts != NULL)
		free(copts);

	return;
}

/*
 *	CALL ASNRM(IER)
 *
 *	argument:
 *	
 *		IER		(output) assigned 0 on ok, or >0 on error 
 */
void
#ifdef	_UNICOS
ASNRM(
#else
asnrm_(
#endif
	_f_int	*ier)
{
	int	ss;

	*ier	= 0;

	if ((ss = _assign("-R",NULL,_LELVL_RETURN)) == -1) 
		*ier	= errno;

	return;
}

/*
 * ASNCTL routine - ASSIGN support routine
 */
void
#ifdef	_UNICOS

ASNCTL(
	_fcd	option,		
	_f_int	*value,	/* 1 indicates enable option. 0 indicates disable */
	_f_int	*ier)	/* returned error status, 0 if no error */
{

#else

asnctl_(
	char	*option_ptr,
	_f_int	*value,
	_f_int	*ier,
	int	option_len)
{
	_fcd option	= _cptofcd(option_ptr, option_len);

#endif
	int			status, ss, i;
	int			aes;
	char			atype, *s;
	FILE			*f;
	assign_environment	*aenvp;
	
	*ier	= 0;
	ASSIGN_LOCK();

	/*
	 * LOCAL - Push a new local assign environment onto stack _Ae_env_stack.
	 * The new environment receives a copy of the assign environment 
	 * immediately beneath it.  If the new local assign environment
	 * is the first local environment, then the global assign environment
	 * is copied with _ae_internalize.  The new assign environment becomes
	 * the active environment.
	 */
	if (_string_cmp("LOCAL", _fcdtocp(option), _fcdlen(option))) {

		if (*value != 1) {
			*ier	= ERAS_ASNCTL;
			goto ret;
		}

		if (_Ae_env_stack.size == 0) {
			/*
			 * Read the global assign information from the assign
			 * environment file into a new local environment at the
			 * top of _Ae_env_stack.
			 */
			f	= _gae_open('r', 'u', &status);
			if (status != 0) {
				*ier	= status;
				goto ret;
			}
			aenvp	= malloc(sizeof(assign_environment));
			if (aenvp == NULL) {
				*ier	= FENOMEMY;
				goto ret;
			}

			_Ae_env_stack.env	= aenvp;

			ss	= _ae_internalize(f, aenvp);

			if (ss == -1) {
				*ier	= errno;
				goto ret;
			}
			_gae_close(f);
		}
		else {
			aes	= _Ae_env_stack.size;
		
			aenvp	= (assign_environment *)
				realloc(_Ae_env_stack.env,
					(aes+1)*sizeof(assign_environment));
			if (aenvp == NULL) {
				*ier	= FENOMEMY;
				goto ret;
			}

			_Ae_env_stack.env	= aenvp;
			aenvp		= &_Ae_env_stack.env[aes];
			aenvp->rec_cnt	= (aenvp-1)->rec_cnt;
			aenvp->rec_lim	= (aenvp-1)->rec_lim;
			aenvp->ar	= NULL;

			if (aenvp->rec_lim > 0) {
				aenvp->ar	= malloc(aenvp->rec_lim *
							sizeof(assign_record));
				if (aenvp->ar == NULL) {
					*ier	= FENOMEMY;
					goto ret;
				}
			}
			for (i = 0; i < aenvp->rec_cnt; i++) {
			    aenvp->ar[i].id.type	= (aenvp-1)->ar[i].id.type;
			    atype	= (aenvp-1)->ar[i].id.type;

			    switch (atype) {

			    case BYUNIT:
				aenvp->ar[i].id.u.unit = (aenvp-1)->ar[i].id.u.unit;
				break;
			    default:
				s	= strdup((aenvp-1)->ar[i].id.u.str);
				if (s == NULL) {
				    *ier	= FENOMEMY;
				    goto ret;
				}
				aenvp->ar[i].id.u.str	= s;
				break;
			    }

			    if ((aenvp-1)->ar[i].attr != NULL) {
				s	= strdup((aenvp-1)->ar[i].attr);
				if (s == NULL) {
				    *ier	= FENOMEMY;
				    goto ret;
				}
				aenvp->ar[i].attr	= s;
			    }
			    else
				aenvp->ar[i].attr	= NULL;
			}
		}
		_Ae_env_stack.size += 1;
	
	}
	/*
	 * NEWLOCAL - Push an empty local assign environment onto
	 * stack _Ae_env_stack.  The new assign environment becomes
	 * the active environment.
	 */
	else if (_string_cmp("NEWLOCAL", _fcdtocp(option), _fcdlen(option))) {

		if (*value != 1) {
			*ier	= ERAS_ASNCTL;
			goto ret;
		}
		aes	= _Ae_env_stack.size;
		
		aenvp	= (assign_environment *)
			realloc(_Ae_env_stack.env,
				(aes+1)*sizeof(assign_environment));
		if (aenvp == NULL) {
			*ier	= FENOMEMY;
			goto ret;
		}
		_Ae_env_stack.env	= aenvp;
		(void)_ae_setupenv(&_Ae_env_stack.env[aes]);
		_Ae_env_stack.size += 1;

	}
	/*
	 * RESTORE - Pop the top assign environment from the stack of local
	 * assign environments.
	 */
	else if (_string_cmp("RESTORE", _fcdtocp(option), _fcdlen(option))) {

		if (*value != 1) {
			*ier	= ERAS_ASNCTL;
			goto ret;
		}

		if ((aes = _Ae_env_stack.size) == 0) {
			*ier	= ERAS_NOPOP;
			goto ret;
		}

		(void)_ae_dealloc_env(&_Ae_env_stack.env[aes-1]);
		_Ae_env_stack.env	= (assign_environment *)
			realloc(_Ae_env_stack.env,
				(aes-1)*sizeof(assign_environment));
		if (aes == 1)
			_Ae_env_stack.env	= NULL;
		_Ae_env_stack.size -= 1;
	}
	else {
		*ier	= ERAS_ASNCTL;
	}

ret:
	ASSIGN_UNLOCK();

	return;
}

/*
 * _assign 	Assigns Fortran file or unit attributes.  This routine
 *		is called by ASSIGN, ASNRM, ASNUNIT, and ASNFILE.
 *
 *		Return value: 0 if OK, -1 on error (with errno set to error)
 */
_assign(
char 		*opt_string,	/* string containing attribute and control */
				/* options */
assign_obj_id	*aop,		/* assign object, or NULL if none */
int		errmode)	/* error handling mode -- _LELVL_xxx */
{
	
	/*
	 * args includes all letters because any letter can be an attribute
	 * option.  "_ae_parse" validates attribute options.
	 */
	int	assign_mode;		/* Assign processing mode selected. */
					/* This is determined by I, O, R, V */
					/* options.	*/
	int	ss;
	char	*attr_string;
	char	open_mode;		/* 'r' or 'w' */
	char	res_mode;		/* 'f', 'e', or 'u' */
	assign_cntl	cntl;
	int	objcount;		/* number of assign objects (0 or 1) */

	attr_string	= NULL;
	/*
	 * _lae_process_opts mallocs a string which is freed later.
	 * "attr_string" is assigned a string (allocated with malloc())
	 * containing only attribute options. 
	 */
	ss	= _lae_process_opts(opt_string, &attr_string, &cntl);

	if (ss == -1) {
		if (attr_string != NULL)
			free(attr_string);
		return(-1);
	}

	ASN_DEBUG(("_assign: back from _lae_process_opts: <%s>\n",
		attr_string != NULL? attr_string: ""));

	objcount	= (aop == NULL) ? 0 : 1;
	ss	= _lae_assign_mode(AE_LIB, &cntl, objcount, &assign_mode, &open_mode,
			&res_mode);
	if (ss == -1) {
		if (attr_string != NULL)
			free(attr_string);
		return(-1);
	}

	ASN_DEBUG(("_assign: ASSIGN mode is %d open mode is %c\n",
			assign_mode,open_mode));

	ss	= _lae_do_assign(AE_LIB, assign_mode, open_mode, res_mode, aop,
			attr_string, 1,errmode);
	if (ss == -1) {
		if (attr_string != NULL)
			free(attr_string);
		return(-1);
	}

	if (attr_string != NULL)
		free(attr_string);

	return(0);
}

void
_lae_print_record(assign_record *arp)
{
	switch (arp->id.type) {
	default:
		printf("assign %s %c:%s\n", arp->attr, arp->id.type,
					arp->id.u.str);
		break;
	case BYUNIT:
		printf("assign %s u:%ld\n", arp->attr, arp->id.u.unit);
		break;
	}

	return;
}


/*
 * _lae_assign_mode	Compute the assign mode, set defaults, check for
 *			errors.  The assign mode tells if assign records
 *			are being updated or merely read.  If they are
 *			being updated, the type of update is indicated.
 *			Also compute the open mode for reading the 
 *			assign environment file.  The numberj of 
 *			assign objects "numobj" is validated against
 *			the assign mode.  
 *
 *			Returns: 0 on OK, -1 on error (with errno set).
 */
_lae_assign_mode(
	int		fromwhere,		/* AELIB or AECMD */
	assign_cntl	*cnp,
	int		numobj,
	int		*amode,
	char		*omode,
	char		*rmode)
{
	/*
	 * Check for invalid combinations of control options.
	 */
	if (cnp->I && cnp->O)
		ERRET(ERAS_INCOV);

	if (cnp->R && cnp->V)
		ERRET(ERAS_REMV);

	if (cnp->attrs && cnp->R)
		ERRET(ERAS_REMAT);

	if (cnp->attrs && cnp->V)
		ERRET(ERAS_VIEWAT);

/*
 *	Set up default options, and then establish the assign mode.
 */
	if (!cnp->I && !cnp->O)
		cnp->O	= 1;	/* -O is default */

	ASN_DEBUG(("_lae_assign_mode: O:%d R:%d V:%d I:%d attr:%d \n",
			cnp->O, cnp->R, cnp->V, cnp->I, cnp->attrs));

	if (cnp->R) 
		if (numobj == 0)
			*amode	= REMOVE_ALL;
		else 
			*amode	= UPDATE_REMOVE;
	else if (cnp->V) 
		if (numobj == 0)
			*amode	= VIEW_ALL;
		else
			*amode	= VIEW_RECORD;
	else if (cnp->I) 
		*amode	= UPDATE_INCREMENT;
	else if (cnp->O) 
		*amode	= UPDATE_REPLACE;
	else
		ERRET(FEINTUNK);	

/*
 *	Set the access mode used to open the assign environment.
 *		'r'	- read only
 *		'w'	- update
 */
	switch (*amode) {

		case UPDATE_INCREMENT:
		case UPDATE_REPLACE:
			if (numobj < 1)
				ERRET(ERAS_NOOBJS);
			*omode	= 'w';		/* update mode */
			break;

		case UPDATE_REMOVE:
		case REMOVE_ALL:
			*omode	= 'w';		/* update mode */
			break;

		case VIEW_RECORD:
		case VIEW_ALL:
			*omode	= 'r';		/* read only */
			break;

		default:
			ERRET(FEINTUNK);
	}

/*
 *	Set assign environment residency mode.  Always 'u'.
 */
	*rmode	= 'u';

	return(0);
}

/*
 * _lae_do_assign 	Performs the requested action.
 *
 *			Return value: 0 on OK, -1 on error (with errno set).
 *
 */
int
_lae_do_assign(
int			fromwhere,	/* AE_LIB, AE_ASSIGN, AE_ASGCMD */
int			assign_mode,	/* type of assign request */
char			open_mode,	/* type of environment access */
char			res_mode,	/* residency of env: 'f', 'e', 'u' */
assign_obj_id		*aoidp,
char			*attr_string,
int			optcheck,	/* 1 if options should be validated */
int			errmode)	/* error handling mode -- _LELVL_xxx */
{
#define DOAS_RET(val)	{ 		\
	if (val != 0)			\
		_lerror(errmode,val);	\
	reterr	= val; 			\
	goto done;			\
} 
	FILE *f;
	int  ss, found;
	int  reterr;
	int status;
	assign_info ainfo;
	assign_record *arp;
	char *combination;
	assign_environment	asnenv;
	assign_environment	*aenvp;

	f	= NULL;
	ASSIGN_LOCK();

	if (_Ae_env_stack.size > 0) {
		/*
		 * Local assign mode.  This is only utilized from 
		 * library calls to _assign().
		 */
		aenvp	= &_Ae_env_stack.env[_Ae_env_stack.size-1];
	}
	else {
		/*
		 * Non-local assign mode.
		 * Read in the assign records from the global file.
		 * This is skipped only if local assign is enabled.
	 	 * Concurrency protection is achieved through file locks.
		 */
		aenvp	= &asnenv;
		_ae_setupenv(aenvp);
		f	= _gae_open(open_mode, res_mode, &status);
		if (status != 0) {
			DOAS_RET(status);
		}

		ss	= _ae_internalize(f, aenvp);
		if (ss == -1) {
			DOAS_RET(errno);
		}
		if (open_mode == 'r') {
			_gae_close(f);  /* If no update close file now */
			f	= NULL;
		}
	}
	switch (assign_mode) {
	case UPDATE_INCREMENT:
	case UPDATE_REPLACE:
		found	= _ae_select(aoidp, &arp, aenvp);

		if (found && assign_mode == UPDATE_INCREMENT) {
			/* eclipse old attributes with new */
			ss	= _ae_eclipse(arp->attr, strlen(arp->attr),
			 		attr_string, strlen(attr_string),
			 		&combination);
			if (ss == -1) {
				DOAS_RET(FEINTUNK);
			}
			if (optcheck) {
				ss	= _ae_parse(aoidp, combination,
						strlen(combination),
						&ainfo, 1, errmode);
				if (ss == -1) {
					DOAS_RET(errno);
				}
			}
			free(arp->attr);
			arp->attr	= combination;
			break;
		}

		if (optcheck) {
			ss	= _ae_parse(aoidp, attr_string,
					strlen(attr_string), &ainfo, 1,
					errmode);
			if (ss == -1) {
				DOAS_RET(errno);
			}
		}

		if (found && assign_mode == UPDATE_REPLACE) {
			(void)_ae_delete(arp,aenvp);
		}

		ss	= _ae_insert(aoidp, attr_string, strlen(attr_string),
				aenvp);

		if (ss == -1) {
			DOAS_RET(errno);
		}
		break;

	case UPDATE_REMOVE:
		ss	= _ae_select(aoidp,&arp,aenvp);
		if (ss == 1) {
			(void)_ae_delete(arp,aenvp);	/* delete the record */
		}
		break;

	case REMOVE_ALL:
		(void)_ae_dealloc_env(aenvp);
		break;

	case VIEW_RECORD:
		found	= _ae_select(aoidp,&arp,aenvp);
		if (found) {
			_lae_print_record(arp);
		}
		break;

	case VIEW_ALL:
		if (_Ae_env_stack.size > 0) {
			printf("# LOCAL ASSIGN ATTRIBUTES\n");
		}
		else {
			char	*nam;
			nam	= _lae_get_assign_var_name();
			if (nam != NULL) {
				printf("# %s ENVIRONMENT VARIABLE=$%s\n",
					(_Ae_asgcmd ? "ASGCMD" : "ASSIGN"),
					nam);
			}
			else {

				nam	= _lae_get_assign_file_name(&reterr);
				if (reterr != 0) {
					DOAS_RET(FENOMEMY);
				}
			
				if (nam != NULL) {
					printf("# ASSIGN ENVIRONMENT FILE=%s\n",
						nam);
					free(nam);
				}
			}
		}
		arp	= NULL;
		while (_ae_next(arp,&arp,aenvp)) {
			_lae_print_record(arp);
		}
		break;
	}
	reterr	= 0;

done:
	if (_Ae_env_stack.size == 0) {
		if (reterr == 0) {
			/*
		 	 * Update the global assign environment if 
			 * changes were made and no errors occurred.
		 	 */
			if (open_mode == 'w') {
				ss	= _ae_externalize(fromwhere, f, aenvp);
				if (ss == -1) {
					DOAS_RET(errno);
				}
			}
		}
		(void)_ae_dealloc_env(aenvp);
	}
	/*
	 * Cleanup, then exit with proper error status set.
	 */
	if (f != NULL) {
		_gae_close(f); 
	}
	ASSIGN_UNLOCK();

	if (reterr) {
		errno	= reterr;
		return(-1);
	}

	return(0);
#undef DOAS_RET
}
/*
 * _lae_process_opts 	mallocs string "attr_string"
 * 			and assigns it a string containing only 
 * 			attribute options.
 *
 *			"opt_string" is the input string which contains
 *			attribute and control options.  Options which
 *			do not take an option value may be concatenated
 *			together (ie. -VvI).  And single-character options
 *			may be concatenated with their option values (ie.
 *			-b20).
 *
 *			The output "attr_string" string has options and
 *			option values separated by spaces.  And Control
 *			options I, O, R, V, v, and z are removed.
 */
int
_lae_process_opts(
char		*opt_string,
char		**attr_string,
assign_cntl	*cntlp)
{
	char	*np;
	char	*pr, *pe, *pw;
	int	len;
	int	toksiz;
	int	moreopts, optval;

	*attr_string	= NULL;
	cntlp->I	= 0;
	cntlp->O	= 0;
	cntlp->R	= 0;
	cntlp->V	= 0;
	cntlp->attrs	= 0;
	
/*
printf("_lae_process_opts input <%s>\n",opt_string);
*/
	if (opt_string == NULL)
		len	= 0;
	else
		/* Add an extra 4 chars in case "-cb 20" or "-tb 20".	*/
		/* Add an extra 52 chars in case "-b20" 		*/
		len	= strlen(opt_string) + 4 + 52;

	np	= malloc(len + 1);	/* one extra for trailing null */

	if (np == NULL) {
		ERRET(FENOMEMY);
	}

	*attr_string	= np;

	memset(np, 0, len+1);		/* for resiliency */

	if (opt_string == NULL)
		return(0);		/* no options at all */

	/*
	 * Delete control options I, O, R, and V from attributes string
	 * while watching for their occurrence.  The string modification
	 * is done in place.
	 *
	 *	pr = current option being read in opt_string
	 *	pe = pointer to whitespace after current token
	 *
 	 *	np = start of new string
	 *	pw = where options are being written in new string
	 */
	pr	= opt_string;
	_AE_SKIPWHITE(pr);
	pw	= np;

	while (*pr != '\0') {

		pe	= pr;
		while( (! isspace(*pe)) && *pe != '\0')
			pe++; 

		moreopts	= 1;
		optval		= 0;

		/*
 		 * pr should point to the start of an option.  If it does not
		 * then merely copy the token to let _ae_parse detect the
		 * error.
		 */
		if (*pr != '-') {
			moreopts	= 0;
			optval		= 1;
		}

		/* 
		 * This loop copiesy the zero or more attributes options from 
		 * "-xyz"  string.
		 */
		while (moreopts) {

		    /* assign control options */

		    if (_ae_opt_control(pr[1])) {
			switch (pr[1]) {
			    case 'I': cntlp->I = 1; break;
			    case 'O': cntlp->O = 1; break;
			    case 'R': cntlp->R = 1; break;
			    case 'V': cntlp->V = 1; break;
			    case 'v': cntlp->v = 1; break;
			    case 'z': cntlp->z = 1; break;
			}
			if (isspace(pr[2]) || pr[2] == '\0') {
			    moreopts	= 0;
			    pr	= pr + 2;	/* get past this option */
			}
			else 
			    pr	= pr + 1;
		    }

		    /* attribute options that don't take option values */
		    else if (_ae_opt_nooptval(pr[1])) {
			*pw++	= '-';
			*pw++	= pr[1];
			*pw++	= ' ';
			if (isspace(pr[2]) || pr[2] == '\0') {
			    moreopts	= 0;
			    pr	= pr + 2;	/* get past this option */
			}
			else 
			    pr	= pr + 1;
		    }

		    else if
			/* single character attribute options (-b, -C,...) */
			(_ae_opt_optval(pr[1]) ||

			/* or options with numeric option value or '-p :num' */
			isdigit(pr[2]) || pr[2] == '-' || pr[2] == '+' ||
			pr[2] == ':' ||

			/* or options with a the option value "default" */
			strncmp(&pr[2], "default", pe - &pr[2]) == 0 ||

			/* or options with a the option value "on" */
			strncmp(&pr[2],"on",MAX(pe-&pr[2],strlen("on"))) == 0 ||

			/* or options with a the option value "off" */
			strncmp(&pr[2],"off",MAX(pe-&pr[2],strlen("off")))==0) {

/*
printf("bi pediff %d\n",pe-&pr[2]);
printf("bi _ae_opt_optval %d\n",_ae_opt_optval(pr[1]));
printf("bi dig %d punct %d\n",isdigit(pr[2]), ispunct(pr[2]));
*/

			cntlp->attrs	= 1;
			*pw++		= '-';
			*pw++		= pr[1];
			*pw++		= ' ';
			moreopts	= 0;
			optval		= 1;
			pr		= pr + 2;/* point pr to option value */
		    }

		    /* attribute option delimited from its option value */
		    /* by whitespace.					*/
		    else {
			cntlp->attrs	= 1;
			/* copy option */
			while (!isspace(*pr) && *pr != '\0')
				*pw++	= *pr++;
			*pw++		= ' ';
			moreopts	= 0;
			optval		= 1;
		   }
		}	/* end while(moreopts) */

		/* 
		 * Now copy the option value 
		 */

		if (optval) {
			_AE_SKIPWHITE(pr);
			while (!isspace(*pr) && *pr != '\0')
				*pw++	= *pr++;
			*pw++	= ' ';
		}

		/*
 		 * Advance pr to next option.
		 */
		_AE_SKIPWHITE(pr);
	}

	/* Trim trailing spaces */
	while( pw > np && *(pw-1) == ' ')
		pw--;

	*pw	= '\0';		/* delimit the end of the string */
/*
printf("_lae_process_opts exit <%s>\n",*attr_string);
*/
	return(0);
}

#ifdef KEY /* Bug 4260 */
/* If byteswap options appeared on command line, front end will emit a strong
 * definition for this along with the Fortran main program. */
#pragma weak __io_byteswap_value
int __io_byteswap_value = IO_DEFAULT;

/* "assign" command options for big- and little-endian conversion */
static char *options[2] = {
  "-F f77.mips -N be",
  "-F f77.vax -N ia64",
  };

/* Must issue "assign" command for "g:su" and "g:du" (using "g:all" would
 * attempt to swap bytes on formatted files and give a runtime error) */
static char sequential_vs_direct[2] = "sd";

/*
 * Call this from _OPEN (so it is protected by the concurrency lock that
 * _OPEN sets) to initialize "assign" state based on  __io_byteswap_value; it
 * will execute on the first call, and return without doing anything on
 * subsequent calls.
 */
void
__io_byteswap() {

  /* Either no command-line option, or __io_byteswap has already executed. */
  if (IO_DEFAULT == __io_byteswap_value) {
    return;
  }

  /* Warn that FILENV overrides compile-time options */
  char *filenv = getenv(ASSIGN_ENV_VAR);
  if (filenv && *filenv) {
    _lwarn(FWFILENV);
  }

  else {

    int opt = -1;
    switch (__io_byteswap_value) {
      case IO_NATIVE:
	/* No swapping */
	break;
      case IO_SWAP:
  #     ifdef _LITTLE_ENDIAN
	 opt = 0;
  #     else
	 opt = 1;
  #     endif /* _LITTLE_ENDIAN */
	break;
      case IO_BIG_ENDIAN:
  #     ifdef _LITTLE_ENDIAN
	 opt = 0;
  #     endif /* _LITTLE_ENDIAN */
	break;
      case IO_LITTLE_ENDIAN:
  #     ifndef _LITTLE_ENDIAN
	 opt = 1;
  #     endif /* _LITTLE_ENDIAN */
	break;
    }
    if (-1 != opt) {
      int i;
      for (i = 0; i <= 1; i += 1) {
	int err;
	char buffer[256];
	snprintf(buffer, sizeof buffer, "assign %s g:%cu", options[opt],
	  sequential_vs_direct[i]);
	__assign(buffer, &err, strlen(buffer));
	if (err) {
	  _lwarn(err); /* Expect an "errno" value */
	}
      }
    }
  }

  /* Ensure that subsequent calls to __io_byteswap will do nothing. */
  __io_byteswap_value = IO_DEFAULT;
}
#endif /* KEY Bug 4260 */
