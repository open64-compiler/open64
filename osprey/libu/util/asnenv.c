/*
 * Copyright 2004, 2005 PathScale, Inc.  All Rights Reserved.
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


#pragma ident "@(#) libu/util/asnenv.c	92.2	07/07/99 13:18:33"

#include <ctype.h>
#include <errno.h>
#include <fcntl.h>
#include <liberrno.h>
#include <memory.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <cray/assign.h>
#include <cray/mtlock.h>
#include <cray/nassert.h>
#include <cray/portdefs.h>
#include <sys/types.h>
#include <sys/stat.h>
#ifdef  _ABSOFT
#include "ac_sysdep.h"
#else
#include <malloc.h>
#include <sys/unistd.h>
#endif

#ifdef	_UNICOS_MAX
#include <mpp/globals.h>
#endif

/*
 *	Data declarations
 */

#ifndef _CRAYMPP
DECL_LOCK(_Ae_assign_lock)
#else
DECL_MPP_LOCK(_Ae_assign_lock)
#endif

/*
 *	Prototypes
 */
static int _retry_open(char *path, int oflag, int mode);
static int _lae_get_format(char *line);
static int convert_hybrid(char *lp);

/*
 *	_Ae_env_stack is a stack of local assign environments.  The top assign
 *	environment in this stack is the currently active assign environment.
 *	When _Ae_env_stack.size == 0, the global assign environment is the
 *	active assign environment.
 */

aenv_stack	_Ae_env_stack;		/* Stack of local assign environments */

int	_Ae_assign_cmd	= 0;		/* set to 1 for assign command */
int	_Ae_asgcmd	= 0;		/* set to 1 for asgcmd command */

#define DUMMY	0
#define UMASK	0666			/* file creation mode for open(2) */

/*
 * _assign_asgcmd_info
 *	
 *	Searches the assign environment and the asgcmd environment for 
 *	assign options to be associated with the current file open request. 
 *
 *	Assign/asgcmd options associeated with the following assign
 *	objects are searched for:
 *
 *		- g:all if gamask contains ASN_G_ALL
 *		- Any other global assign (g:*) code indicated by gamask.
 *		- The unit number if passed in unum.
 *		- The file name if passed in fname.
 *
 *	The g:all options are searched and are overlaid with (in order)
 *	g:ff/aq/su/sf/du/df options, unit options, and file name options.
 *
 *	It is an error condition if assign attributes and asgcmd attributes 
 *	are found for this open.
 *
 *	Return value:	 1 if an entry for either the file or the unit
 * 			   was found, 
 *			 0 if not found,
 *			-1 on error (with errno set).
 */
int
_assign_asgcmd_info(
const char	*fname,         /* Requested file name (NULL if none) */
unum_t		unum,		/* Unit number (or -1 if none) */
int		gamask,		/* ASN_G_* masks */
assign_info	*aip,		/* Receives the data */
char		**atstr,	/* Receives a pointer to the attributes
				 * string on return.  The space is allocated
				 * with malloc(). If atstr == NULL, the
				 * attributes string is not returned. */
int		catcherr)	/* 0 if errors should result in a call to 
				 * _lerror.  != 0 if an error status should
				 * be returned in errno with ret value of -1 */

{
	int		ret, errn;
	char		*asg_entry;
	char		*fdstr, *cstr, *opts;
	union fddcvrt_u cword;
	union spec_u    *fdspec, *_dec_fdc_spec();
	extern char	*_g_asg_entry();
	extern char	*_g_fchar_opts();
	extern char	*_g_fchar_opt();
	int		asnflg, asgflg;
	int		errmode;
	char		*asgcmd_options, *assign_options, *ctmp;
	const char	*asgprefix = "(from asgcmd) ";

	errmode	= catcherr ? _LELVL_RETURN : _LELVL_ABORT;
	ret	= 0;
	errn	= 0;
	asgcmd_options	= NULL;
	assign_options	= NULL;

/*
 *	Search for assign environment entry for the file, unit, gamask.
 */
	asnflg	= _get_a_options(0, fname, unum, gamask, aip, &assign_options,
			errmode);
	if (asnflg == -1) 
		return(-1);
/*
 *	Search for asgcmd environment entry for the file.  We flag an error
 *	if the user is using assign and asgcmd for this file/unit.
 */

	asgflg		= 0;
	asg_entry	= NULL;

	if (fname != NULL) {
		asgflg	= _get_a_options(1, fname, unum, gamask, aip,
				&asgcmd_options, errmode);
		if (asgflg == -1) {
			errn	= errno;
			goto bye;
		}
		if (asgflg) {
			ctmp	= malloc(strlen(asgprefix) + 
						strlen(asgcmd_options) + 1);
			if (ctmp == NULL) {
				errn	= FENOMEMY;
				goto bye;
			}
			strcpy(ctmp,asgprefix);
			strcat(ctmp,asgcmd_options);
			free(asgcmd_options);
			asgcmd_options	= ctmp;
		}

		/*
		 * _get_a_options searched for asgcmd information in the new
		 * (UNICOS 8.0) environment variable "_ASG_ATTR".  
		 * asgcmd from UNICOS 7.0 (and prerelease 8.0) only set up
		 * environment variable "_ASG_FCHAR" with the old format.
		 *
		 * If "_ASG_ATTR" is not set, we use old-style inquiry of
		 * "_ASG_FCHAR" to maintain compatibility with 7.0 and previous
		 * versions of asgcmd.  This support is not absolutely 
		 * essential from the perspective of the compatibility 
		 * statement (it's backward, not forward, compatibility), but 
		 * it is convenient for testing of new libraries with old 
		 * versions of asgcmd.
		 * 
		 * This code which manipulates asg_entry could reasonably be 
		 * deleted in the CrayLibs 2.0 release.
		 */
#ifdef	_UNICOS
		if (! asgflg) {
			asg_entry	= _g_asg_entry(fname);
			if (asg_entry != NULL)
				asgflg	= 1;
		}
#endif

		if (asnflg && asgflg) {
			errno	= FEOPASGN;
			return(-1);
		}
	}

#ifdef	_UNICOS
	if (asg_entry != NULL) {
		/*
 		 * asg_entry points to a 7.0 version of the asgcmd info
		 * block.   Do special processing to parse it.
		 */
		(void) memset((char*)aip, 0, sizeof(assign_info));
		opts	= _g_fchar_opts(asg_entry);
		fdstr	= _g_fchar_opt(opts, 'F');
		cstr	= _g_fchar_opt(opts, 'C');
		if (cstr != NULL) {
			cword.wword	= _hex2bin(cstr);
			if (cword.fld.cvch != 0) {
				aip->C_chrcnv_flg	= 1;
				aip->C_chrcnv		= cword.fld.cvch;
			}

			if (cword.fld.cvrt != 0) {
				aip->N_datcnv_flg	= 1;
				aip->N_datcnv		= cword.fld.cvrt;
			}
		}
		if (fdstr != NULL) {
			/* decode the FDC specifications */
			aip->F_filter_flg	= 1;
			fdspec	= _dec_fdc_spec(fdstr);
			if (fdspec == NULL) {
				errno	= FENOMEMY;
				return(-1);
			}
                	(void)memcpy((char*)aip->F_filter, (char*)fdspec,
                        	     sizeof(long) * MAX_FDC_SPEC);
			aip->F_filter_len	= MAX_FDC_SPEC;
			free(fdspec);
		}

		asgcmd_options	= strdup("<asgcmd 7.0 options used>");
		if (asgcmd_options == NULL) {
			errn	= FENOMEMY;
			goto bye;
		}
	}
#endif	/* _UNICOS */

	if (asnflg || asgflg) {
		ret	= 1;		/* assign/asgcmd entry found */
		if (atstr != NULL)
			*atstr	= (assign_options != NULL) ?
				assign_options : asgcmd_options;
		else {
			if (assign_options != NULL) free(assign_options);
			if (asgcmd_options != NULL) free(asgcmd_options);
		}
	}

bye:
	if (errn != 0) {
		if (assign_options != NULL) free(assign_options);
		if (asgcmd_options != NULL) free(asgcmd_options);
		_lerror(errmode, errn);
		errno	= errn;
		ret	= -1;
	}

	return(ret);
	
}

/*
 * _more_options
 *
 *	Search for assign options for a particular assign object.  If they
 *	are found then merge them into the string at aop.   
 *
 *	Note special handling for BYPATTERN.
 *
 *	Return value:	 1 if the assign_object was found
 *			 0 if not found,
 *			-1 on error (with errno set).
 */
static int
_more_options(
assign_obj_id		*aoidp,
assign_environment	*aenvp,
char			**aop,		/* gets reassigned to a new malloc()ed
					 * string */
int			*msgprntp,
int			errmode)
{
	int ss;
	assign_record *arp;
	char *ctmp;

	if (aoidp->type == BYPATTERN) {
		ss	= _ae_match_pattern(aoidp->u.str, &arp, aenvp);
		if (ss > 1) {
			/*
			 * If more than one pattern matched (ignoring "p:%")
			 * we issue an error.  It is difficult to determine
			 * which pattern is more specific and hence which
			 * pattern's assign options should overlay the other's.
			 * So punt.
			 */
			errno	= ERAS_PATCONF;
			_lerror(errmode, errno, aoidp->u.str);
			*msgprntp	= 1;
			return(-1);
		}
	}
	else 
		ss	= _ae_select(aoidp, &arp, aenvp);

	if (ss == 1) {
		ss	= _ae_eclipse(*aop, strlen(*aop), arp->attr, 
				strlen(arp->attr), &ctmp);
		if (ss == -1)
			return(-1);
		free(*aop);
		*aop	= ctmp;	/* ctmp has been malloc()'ed */
		return(1);
	}
	return(0);
}
/*
 * _get_a_options
 *
 *	See description of _assign_asgcmd_info().  Does the same thing,
 *	except _get_a_options() searches only assign options or
 *	only asgcmd options.
 *
 *	Return value:	 1 if an entry for either the file or the unit
 * 			   was found, 
 *			 0 if not found,
 *			-1 on error (with errno set).
 */

int
_get_a_options(
int		ifasgcmd,	/* 1 if asgcmd options, 0 if assign */
const char	*fname,		/* File name (NULL if none) */
unum_t		unum,		/* Unit number (or -1 if none) */
int		gamask,		/* ASN_G_* masks (used if ! ifasgcmd) */
assign_info	*aip,		/* Receives the parsed assign information.
				 * If aip == NULL, this information is not
				 * returned. */
char		**atstr,	/* Receives a pointer to the attributes 
				 * string on return.  The space is allocated
				 * with malloc(). If atstr == NULL, the
				 * attributes string is not returned. */
int		errmode)	/* error handling mode _LELVL_xxx */

{
	assign_obj_id		aoid;
	char			*allopts;
	int			aes;
	int			ss;
	int			errn;		/* >0 if error encountered */
	int			msgprnt;	/* 1 after lerror is called */
	FILE 			*f;
	assign_environment	asnenv;
	assign_environment	*aenvp;
	int			ofound;		/* 1 if assign options found */
	int			warnmode = 0;	/* request no warning messages*/
						/* from _ae_parse */

	aenvp	= NULL;
	ofound	= 0;
	errn	= 0;
	msgprnt	= 0;
	f	= NULL;

	allopts	= strdup(" ");

	if (allopts == NULL) {
		errn	= FENOMEMY;
		goto bye;
	}

	ASSIGN_LOCK();

	if (ifasgcmd) {
		aenvp	= &asnenv;
		ss	= _ae_internalize_env(aenvp, ifasgcmd);
		if (ss == -1) {
			errn	= errno;
			goto bye;
		}
		goto internalized;
	}

	if ((aes = _Ae_env_stack.size) > 0) {
		/*
		 * Local assign mode.  This is utilized from 
		 * library calls to ASSIGN, ASNFILE, ASNUNIT, 
		 * and ASNRM when ASNCTL has been used to select
		 * local assign mode.
		 */
		aenvp	= &_Ae_env_stack.env[aes-1];
	}
	else {
		/*
		 * Non-local assign mode.
		 * Read in the assign records from the global file or
	 	 * the process environment.
 		 *
		 * This is skipped only if local assign is enabled.
		 */
		f	= _gae_open('r', 'u', &errn);
		if (errn != 0)
			goto bye;

		aenvp	= &asnenv;
		ss	= _ae_internalize(f, aenvp);
		if (ss == -1) {
			errn	= errno;
			goto bye;
		}

		_gae_close(f);  /* no update, so close file now */
		f	= NULL;
	}

internalized:

/*
 *	Start with assign options associated with g:all
 */
	if ((gamask & ASN_G_ALL) && !ifasgcmd) {
		aoid.type	= BYGLOBAL;
		aoid.u.str	= _ae_glob_name(ASN_G_ALL);
		ss	= _more_options(&aoid, aenvp, &allopts, &msgprnt, errmode);
		if (ss == -1) {
			errn	= errno;
			goto bye;
		}
		ofound	= ofound ? ofound : (ss == 1);
	}

/*
 *	Merge in assign options associated with g:sf/su/df/du/aq/ff/mpi.
 */
	if ((gamask & ~ASN_G_ALL) && !ifasgcmd) {
		aoid.type	= BYGLOBAL;
		aoid.u.str	= _ae_glob_name(gamask & ~ASN_G_ALL);
		ss	= _more_options(&aoid, aenvp, &allopts, &msgprnt, errmode);
		if (ss == -1) {
			errn	= errno;
			goto bye;
		}
		ofound	= ofound ? ofound : (ss == 1);
	}

/*
 *	Merge in assign options associated with the unit number.
 */
	if ((unum != -1) && !ifasgcmd) {
		aoid.type	= BYUNIT;
		aoid.u.unit	= unum;
		ss	= _more_options(&aoid, aenvp, &allopts, &msgprnt, errmode);
		if (ss == -1) {
			errn	= errno;
			goto bye;
		}
		ofound	= ofound ? ofound : (ss == 1);
	}

/*
 *	Merge in assign options associated with one file name pattern
 */
	if (fname != NULL) {
		aoid.type	= BYPATTERN;
		aoid.u.str	= (char*)fname;
		ss	= _more_options(&aoid, aenvp, &allopts, &msgprnt, errmode);
		if (ss == -1) {
			errn	= errno;
			goto bye;
		}
		ofound	= ofound ? ofound : (ss == 1);
	}
/*
 *	Last of all, merge in assign options associated with the file name.
 */
	if (fname != NULL) {
		aoid.type	= BYFILE;
		aoid.u.str	= (char*)fname;
		ss	= _more_options(&aoid, aenvp, &allopts, &msgprnt, errmode);
		if (ss == -1) {
			errn	= errno;
			goto bye;
		}
		ofound	= ofound ? ofound : (ss == 1);
	}
		
	if (ofound) {
		if (aip != NULL) {
			ss	= _ae_parse(	NULL,
					allopts,
					strlen(allopts),
					aip,
					warnmode, errmode);
			if (ss == -1) {
				errn	= errno;
				goto bye;
			}
		}
		if (atstr != NULL) {
			*atstr	= allopts;
			allopts	= NULL;		/* suppress free() later */
		}
	}


bye:
	if (ifasgcmd || _Ae_env_stack.size == 0) {
		/* Deallocate environment structure */
	 	_ae_dealloc_env(aenvp);
		_gae_close(f);  /* Close file if it has been left open*/
	}

	ASSIGN_UNLOCK();

	if (allopts != NULL) free(allopts);

	if (errn != 0) {
		if (! msgprnt)
			_lerror(errmode, errn);
		errno	= errn;
		return(-1);
	}

	return(ofound);
}

/*
 * _gae_open 
 *
 *	Locks and opens the global assign environment file.  
 *	_ae_global_lock must be called before _ae_internalize or 
 *	_ae_externalize may be called.
 *
 *	Return value:
 *
 *		FILE pointer	- if the assign environment is
 *				  file-resident.
 *		NULL		- if the assign environment is
 *				  enviroment-resident, or the
 *				  assign environment file does
 *				  not exist or is empty.
 */
FILE *
_gae_open(
char acc_mode,		/* 'r' for read-only mode, 'w' for update */
char res_mode,		/* 'f' for file-resident, 'e' for env-resident, 
			 * 'u' for unknown residency */
int *status)		/* Return status.  0 if OK, error status otherwise */
{
	int		estat;
	char		*fname;
	FILE		*fret;

	/* Assertions */
	assert ( acc_mode == 'r' || acc_mode == 'w' );
	assert ( res_mode == 'u' );		/* only 'u' is supported */

	fret	= NULL;
	*status	= 0;

	if (_Ae_asgcmd)
		return(NULL);		/* no env file for asgcmd */

	fname	= _lae_get_assign_file_name(&estat);
	if (estat) {
		*status	= FENOMEMY;
		goto dealloc_return;
	}

	if (fname != NULL)
		fret	= _unique_open(fname, acc_mode, status);

dealloc_return:
	if (fname != NULL)
		free(fname);
	return(fret);			/*  return file pointer to caller  */
}

/*
 * _gae_close
 *
 * 	Closes and unlocks the global assign environment file f.  No-op if
 *	f is NULL.
 */
void
_gae_close(FILE *f)
{
	if (f != NULL)
		_unique_close(f);            
}

/*
 * _lae_get_assign_file_name
 *
 *	Get the name of the assign environment file.  If FILENV and 
 *	TMPDIR are not set, return NULL.  
 *
 *	Return value:
 *
 *		pointer to an allocated copy of the file name.
 *		Use free() to deallocate this memory later. 
 *
 *	Side effect:
 *
 *		*estat is set to a nonzero value if memory could not
 *		be allocated.
 */
char *
_lae_get_assign_file_name(int *estat)
{
	int	siz;
	char	*fname;
	char	*tdir, *aname, *ctmp;

	*estat	= 0;

	fname	= getenv(ASSIGN_ENV_VAR);	/*  check for file environment  */

/*
 *	If FILENV is set, it has the following format:
 *
 *	   [asn_env_file_name][<DELIMSTR> ...]
 *
 *	if asn_env_file_name is preceded by a PROCENVFLAG character, the user 
 *	has indicated that all assign attributes are to be placed in the 
 *	process environment.
 */
	if ( fname != NULL ) {		
		if (fname[0] == PROCENVFLAG)
			return(NULL);		/* assign records in env var */

		ctmp	= strstr(fname, DELIMSTR);

		if (ctmp == NULL)
			siz	= strlen(fname);
		else
			siz	= ctmp - fname;

		if (siz == 0)
			goto default_asn_file;

		ctmp	= malloc(siz + 1);

		if (ctmp == NULL) {
			*estat	= -1;
			return(NULL);		/* no memory */
		}

		(void) memcpy(ctmp, fname, siz);
		ctmp[siz]	= '\0';		/* null-terminate it */
		return(ctmp);
	}

default_asn_file:

/*
 *	The file is "${TMPDIR}/.assign" (UNICOS systems only)
 */
#ifdef	_UNICOS
	tdir	= getenv("TMPDIR");
#else
	tdir	= NULL;
#endif	

	if (tdir == NULL) {
#ifdef	_UNICOS_MAX
		if (_MPP_MPPSIM == 1) {
			/* $TMPDIR is not set by default when using mppsim */
			/* For now, just put the assign file in ./.assign */
			tdir	= "./";
		}
		else
			return(NULL);		/* no assign env file */
		
#else
		return(NULL);			/* no assign env file */
#endif
	}

	aname	= "/.assign";
	siz	= strlen(aname) + strlen(tdir);

	if ((ctmp = malloc(siz+1)) == NULL) {
		*estat	= -1;
		return(NULL);			/* no memory */
	}

	(void) strcpy(ctmp, tdir);
	(void) strcat(ctmp, aname);
	return(ctmp);
}

/*
 * _lae_get_assign_var_name
 *
 *	Returns the name of the environment variable which
 *	holds the assign/asgcmd records.
 */
char *
_lae_get_assign_var_name(void)
{
	char	*p;
	if (_Ae_asgcmd)
		return(ASGCMD_ENV_VAR);

	p	= getenv(ASSIGN_ENV_VAR);

	if (p == NULL || p[0] != PROCENVFLAG)
		return(NULL);

	p++;			/* skip the PROCENVFLAG character */

	return(p);
}

/*
 * _unique_open
 *
 *	Open the assign environment file and set the file lock.
 *	When all programs accessing a file use _unique_open
 *	and _unique_close exclusive access is implemented. 
 *
 * 	Return value:
 *
 *		FILE pointer which receives the opened file pointer.  NULL is 
 *		assigned if mode=='r' and the file does not exist.
 */
FILE *
_unique_open(
char *fname,	/* file name 						  */
char mode,	/* mode of open: 'r' => read, 'w' => update 		  */
int  *ostat)	/* return status: 0 if OK, error code otherwise 	  */
{
#if	!defined(_ABSOFT) || (!defined(TARGET_MAC_OS) && !defined(TARGET_NT))
	struct flock    ll;
#endif
	struct stat	statbuf;
	char		*fotype;	/* type passed to fdopen */
	int		fd, ss;
	FILE		*stream;

	*ostat	= 0;

	if (mode == 'r') {
		fd	= _retry_open(fname, O_RDONLY, DUMMY);
		if (fd == -1) {
			switch(errno) {
			case ENOENT:			/* OK, no file exists */
				break;
			case EACCES:			/* no read permission */
#ifdef	_SOLARIS		
				/*
				 * BUG: When macrotasking, Solaris returns 
				 * EACCES instead of ENOENT from the second
				 * task to enter this code if the file does
				 * not exist.  For now we will accept EACCES 
			 	 * as possibly meaning that the assign 
				 *environment file does not exist.  This bug 
				 * was observeed in Solaris 5.3.
				 */
				break;
#endif
			case ENOTDIR:	
				*ostat	= ERAS_UNFILE;
				break;
			default:			/* system error */
				*ostat	= errno;
			}
			return(NULL);
		}
		if (fstat(fd, &statbuf) == -1) {
			(void) close(fd);
			*ostat	= ERAS_UNFILE;
			return(NULL);
		}

		/*
		 * If the file size is 0, then pretend the file doesn't
		 * exist.
		 */
		if (statbuf.st_size == 0) {
			(void) close(fd);
			return(NULL);
		}

		fotype	= "r";
	}
	else {
		fd	= _retry_open(fname, O_RDWR | O_CREAT, UMASK);
		if (fd == -1) {
			switch (errno) {
			case EACCES:
			case ENOTDIR:
				/* 
		 		 * Determine whether open failed due to 
				 * no read permission or no write permission.
			 	 */
				fd	= _retry_open(fname, O_RDONLY, DUMMY);
	
				if (fd == -1) {
					*ostat	= ERAS_UNFILE; /* no read perms*/
				}
				else {
					(void) close(fd);
					*ostat	= ERAS_WRERR; /* no write perms*/
				}
				break;
			default:
				*ostat	= errno;
			}
			return(NULL);
		}
		fotype	= "r+";
	}

	if ((stream = fdopen(fd, fotype)) == NULL) {
		(void) close(fd);
		*ostat	= FEINTUNK;		/* what happened? */
		return(NULL);
	}

#ifdef KEY /* Bug 6034 */
/* Eliminate locking of the FILENV file.
 *
 * The use of file locking on NFS on Fedora Core 3 triggers an apparent kernel
 * bug which causes the kernel to consume 50% of available cpu time. Since we
 * already ignore locking errors when NFS doesn't provide locks, and since
 * locking only prevents the 'assign' command from changing the FILENV file in
 * parallel with the execution of the "open" call (and does not preclude other
 * changes to that file, e.g. with a text editor), the locking doesn't seem
 * useful anyway.
 */ 
#else /* KEY Bug 6034 */

#if	!defined(_ABSOFT) || (!defined(TARGET_MAC_OS) && !defined(TARGET_NT))
	ll.l_whence	= 0; 
	ll.l_start	= 0; 
	ll.l_len	= 0; 
	ll.l_type	= (mode == 'r') ? F_RDLCK: F_WRLCK;

	/*
	 * fcntl fails with EINVAL if the assign environment
	 * file resides on an NFS file system.  In that case
	 * we allow assign processing to continue with
	 * no locking.
	 */
	do {		/* repeat system call if interrupted by a signal */
		ss	= fcntl(fd, F_SETLKW, &ll);
	} while (ss == -1 && errno == EINTR);

	if (ss == -1) {
#ifdef	_UNICOS_MAX
		/* if _MPP_MPPSIM == 1, then we are running on the */
		/* simulator in user virtual mode. The fcntl fails */
		/* in this mode */
		if ((_MPP_MPPSIM != 1) && (errno != EINVAL)) {
#else
#ifdef KEY /* Bug 4231 */
		/* SUSE9 Linux sets ENOLCK when NFS doesn't provide locks
		 * (or when the number of locks exceeds the limit) so we
		 * ignore that as well as INVAL, as suggested in the
		 * comment associated with the do/while/fcntl code above.
		 */
		if (errno != EINVAL && errno != ENOLCK) {
#else
		if (errno != EINVAL) {
#endif /* KEY Bug 4231 */
#endif
			(void) fclose(stream);
			*ostat	= errno;
			return(NULL);
		}
	}
#endif
#endif /* KEY Bug 6034 */
	return(stream);
}

/*
 * _unique_close 
 *
 *	Closes and unlocks a file opened with _unique_open.
 */
void
_unique_close(FILE *f)
{
	if ( f != NULL )
		(void) fclose(f);            
}

/*
 * _retry_open
 *
 *	Calls open repeatedly while EINTR status is returned.
 *	EINTR status is returned when a system call is interrupted
 *	by a signal.
 *
 *	Return value is the value returned by open(2), with errno
 *	set if open(2) returns -1.
 */
static int
_retry_open(char *path, int oflag, int mode)
{
	int	fd;

	do {
#if	defined(_ABSOFT) && defined(TARGET_MAC_OS)
		fd	= open(path, oflag);
#else
		fd	= open(path, oflag, mode);
#endif
	} while (fd == -1 && errno == EINTR);

	return(fd);
}

/*
 * _ae_setupenv
 *
 *	Initialize local assign environment.
 */
void
_ae_setupenv(assign_environment *aep)
{
	aep->rec_cnt	= 0;
	aep->rec_lim	= 0;
	aep->ar		= NULL;

	return;
}

/*
 * _ae_dealloc_env
 *
 *	Deallocates a local assign environment.   All storage pointed to by 
 *	aep is deallocated.  aep itself is not deallocated.  This is a no-op
 *	if aep is NULL.
 */
void 
_ae_dealloc_env(assign_environment *aep)
{
	register int	i;
	if (aep == NULL)
		return;	

	if (aep->ar != NULL) {
		for (i = 0 ; i < aep->rec_cnt; i++)
			_ae_dealloc_recflds(&aep->ar[i]);
		free(aep->ar);
		aep->ar = NULL;
	}

	aep->rec_cnt = 0;
	aep->rec_lim = 0;

	return;
}

/*
 * Possible assign file formats.
 */

enum formats {
	hybrid,		/* hybrid format for compatibility in 6.0 */
	unicos70	/* new format for UNICOS 7.0 and later */
};
	
/*
 * _ae_internalize
 *
 *	Copy and translates the global assign environment from a file or the 
 *	process environment into a local assign environment.  
 *
 *	Return value: 0 on normal return, -1 on error (with errno set).
 */
int
_ae_internalize(
FILE			*gfile,		/* global assign environment file, or */
					/* NULL if none exists. */
assign_environment	*aep)		/* local assign environment ptr */
{

	if (gfile != NULL)
		return( _ae_internalize_file(gfile, aep) );
	else
		return( _ae_internalize_env(aep, _Ae_asgcmd) );
}

/*
 * _ae_internalize_file
 *
 *	Copy and translates the global assign environment from a file into a 
 *	local assign environment.  
 *
 *	Return value: 0 on normal return, -1 on error (with errno set).
 */
int
_ae_internalize_file(
FILE			*gfile,		/* global assign environment file, or */
					/* NULL if none exists. */
assign_environment	*aep)		/* local assign environment ptr */
{
	int	lineno;
	int	arec_format;
	char	line[MAX_ASSIGN_LINE];
	char	*p;
	char	*attrp, *objp;
	int	len, ss;
	assign_obj_id aoid;

	_ae_setupenv(aep);

/*
 *	Each pass through the following loop processes one assign 
 *	record.  Each record is stored in the local assign environment.
 *
 *	Each record has the format:
 *		assign -a joe -b 20 -n 10:2 f:foo
 *		assign -a joe -b 20 -n 10:2 u:1
 */
	lineno = 0;

	while (fgets(line, MAX_ASSIGN_LINE, gfile) != NULL) {
		lineno	= lineno + 1;
		len	= strlen(line) - 1;	

		if (len == 0)
			continue;	/* skip 0 length line */
		if (line[len] != '\n')
			goto bad_format;
		line[len]	= '\0';	/* erase the newline at the end */

		/*
		 * Check whether the file is a hybrid format.
		 */
		arec_format	= _lae_get_format(line);

		if (arec_format == hybrid) {
			if (convert_hybrid(line) == -1)
				goto bad_format;
		}

		p	= line;
		_AE_SKIPWHITE(p);
		if (*p == '\0')
			continue;	/* skip line with length of 0 */

		if (*p == '#')
			continue;	/* skip lines which start w/ # */

		if (_asndir_split(line, &attrp, &objp, 0) == -1) /* get attrs */
			goto bad_format;

		if (_lae_get_object(objp, &aoid) == -1)	    /* get object */
			goto bad_format;

		ASN_DEBUG(("_ae_internalize: inserting attrs of len %d: %s\n",
				strlen(attrp), attrp));
		ss	= _ae_insert(&aoid, attrp, strlen(attrp), aep);
		if (ss == -1)
			return(-1);
	}

	if (ferror(gfile)) {
		errno	= ERAS_UNFILE;	/* error reading file */
		return(-1);
	}

	return(0);

bad_format:
	_lwarn(ERAS_BADRECF, lineno);
	return(0);
}

/*
 * _ae_internalize_env
 *
 *	Copy and translate the global assign environment from the 
 *	process environment into a local assign environment.  
 *
 *	Return value:
 *
 *		0 on normal return, -1 on error (with errno set).
 *
 *
 *	The string DELIMSTR precedes each assign_record.   The assign 
 *	records are stored in the form of the assign command which 
 *	generated them.  
 */
int
_ae_internalize_env(
assign_environment	*aep,		/* local assign environment ptr */
int			ifasgcmd)	/* 1 if asgcmd environment should
					 * be internalized.  0 if assign */
{
	int  dsize;	/* number of characters in DELIMSTR */
	int  recno;	/* number of records found */
	int  len;
	char *aes;
	char *thisar;	/* current assign record */
	char *nextar;	/* next assign record */
	char *p;
	char *thestr;	/* string containing assign/asgcmd records */
	char *attrp, *objp;
	assign_obj_id aoid;

	_ae_setupenv(aep);

/*
 *	For asgcmd, environment variable _ASG_ATTR contains the asgcmd records.
 */
	if (ifasgcmd) {
		thestr	= getenv(ASGCMD_ENV_VAR);

		if (thestr == NULL) 
			return(0);
	}

/*
 *	For assign, environment variable FILENV is a pointer to an environment 
 *	variable which contains the assign environment records.
 */
	else {
		thestr	= _lae_get_assign_var_name();
		if (thestr == NULL)
			return(0);

		if (thestr[0] == '\0') {
			errno	= ERAS_NULLVNAM;
			return(-1);
		}
		thestr	= getenv(thestr);
		if (thestr == NULL) 
			return(0);
	}

	aes	= strdup(thestr);	/* copy the assign environment string to
				 * a working copy, which will be altered
				 * as it is processed. */

	if (aes == NULL) {
		errno	= FENOMEMY;
		return(-1);
	}
/*
 *	Each loop iteration processes one assign record from the assign
 *	environment string.  The string has the following format:
 *
 */
	dsize	= strlen(DELIMSTR);

	recno	= 0;
	thisar	= strstr(aes, DELIMSTR);

	if (thisar != NULL)
		thisar	= thisar + dsize;

	while (thisar != NULL) {

		recno++;

		/*
		 * Look ahead for the next assign record now because
		 * _asndir_split will insert null characters in the 
		 * current assign record.
		 */
		nextar	= strstr(thisar, DELIMSTR);
		if (nextar != NULL)
			nextar	= nextar + dsize; /* advance past delimiter */

		p	= strstr(thisar, DELIMSTR);

		if (p == NULL)
			len	= strlen(thisar);
		else
			len	= p - thisar;

		thisar[len]	= '\0';		/* separate from next record */

		if (len == 0)
			continue;	/* skip 0 length line */

		/* get attributes and object text */
		if (_asndir_split(thisar, &attrp, &objp, ifasgcmd) == -1)
			goto bad_format;

		/* parse assign object text */
		if (_lae_get_object(objp, &aoid) == -1)
			goto bad_format;

		ASN_DEBUG(("_ae_internalize: inserting attrs of len %d: %s\n",
				strlen(attrp), attrp));

		/* insert the assign record into the assign environment */
		if (_ae_insert(&aoid, attrp, strlen(attrp), aep) == -1)
			return(-1);

		thisar	= nextar;
	}
	free(aes);
	return(0);

bad_format:
	_lwarn(ERAS_BADRECE, recno, thestr + (nextar-aes));
	free(aes);
	return(0);
}

/*
 * _lae_get_format  
 *
 *	Checks if an assign file entry of is an old format.
 *
 *	Return value: The format type of the file -- unicos70 or hybrid.
 */
static int
_lae_get_format(char *line)
{
	char	cc, *lp;
	int	llen;

	llen	= strlen(line);

	if (llen == 0)
		return(unicos70);

	lp	= line;

	do {
		cc	= *lp++;
	} while (cc != ':' && !isspace(cc) && cc != '\0');

	if (cc != ':') {
		/* First delimiter is white space => unicos70 format */
		return(unicos70);
	}

	return(hybrid);	
}

/*
 * convert_hybrid	
 *
 *	Converts a line from HYBRID format to unicos70.  The 
 *	conversion is done in place.
 *
 *	Returns -1 on error, 0 otherwise.
 *
 *	Hybrid format:
 *
 *	:a:b:c:d:e:f:g:h:i:j:k:l:m:n:o:p:	assign <attributes> <object>
 *
 *	The first part of the record is readable by 5.0 libraries.  The
 *	second part of the record duplicates the information with the unicos70
 *	format.
 *
 */
static int
convert_hybrid(
char *lp)	/* character buffer of size MAX_ASSIGN_LINE */
{
	char	*p, *pdest;

	p	= lp;
	/* scan for a white space followed by 'a' */
	while (*p != '\0' && !(isspace(*p) && *(p+1) == 'a'))
		p++;

	if (*p == '\0')
		return(-1);

	p++;

	/* cannot call strcpy because an overlapping copy may be needed */
	pdest	= lp;

	do {
		*pdest++	= *p;
	} while (*p++ != '\0');
	
	return(0);
}


/*
 * _ae_insert
 *
 *	Adds a record into the local assign environment.
 *
 *	Return value: 0 on success, -1 on failure (with errno set);
 */
int
_ae_insert(
assign_obj_id		*aoidp,
char			*attr,		/* attr string no terminating 0 */
					/* byte is necessary.		*/
int			attrlen,	/* length of attr string */
assign_environment	*aep)
{
#define LIMJUMP 10		/* number of assign_records to alloc */

	assign_record	   *arp;

	if (aep->rec_cnt >= aep->rec_lim) {
		if (aep->rec_lim == 0) {
			arp	= (assign_record *)
				malloc(LIMJUMP*sizeof(assign_record));
			if (arp == NULL)
				ERRET(FENOMEMY);
			aep->rec_lim	= aep->rec_lim + LIMJUMP;
		}
		else {
			arp	= (assign_record *)realloc(aep->ar,
			      (aep->rec_lim + LIMJUMP) * sizeof(assign_record));
			aep->rec_lim	= aep->rec_lim + LIMJUMP;
		}

		if (arp == NULL)
			ERRET(FENOMEMY);

		aep->ar	= arp;
	}
	
	arp	= &aep->ar[aep->rec_cnt];

	arp->id.type	= aoidp->type;

	switch (aoidp->type) {
	case BYUNIT:
		arp->id.u.unit	= aoidp->u.unit;
		break;
	default:
		if ((arp->id.u.str = malloc(strlen(aoidp->u.str) + 1))
		    == NULL) {
			ERRET(FENOMEMY);
		}
		(void)strcpy(arp->id.u.str, aoidp->u.str);
		break;
	}

	if ((arp->attr = malloc(attrlen + 1)) == NULL) {
		ERRET(FENOMEMY);
	}
	(void) strncpy(arp->attr, attr, attrlen);
	arp->attr[attrlen]	= '\0';

	aep->rec_cnt	= aep->rec_cnt + 1; /* increment this when all is OK */

	return(0);
}

/*
 * _ae_dealloc_recflds
 *
 *	Deallocates space pointed to by a particular assign
 *	record.  The record itself is not freed.
 */
void
_ae_dealloc_recflds(assign_record *arp)
{
	if (arp->id.type == BYFILE)
		free(arp->id.u.str);

	free(arp->attr);

	return;
}
			
/*
 * _ae_select
 *
 *	Finds a requested assign record.  Assigns "*arpp" 
 *	the pointer to the record on output.
 *
 *	Return status:	1 if found, 0 if not.
 */
int
_ae_select(
assign_obj_id		*aoidp,		/* id of assign record requested */
assign_record		**arpp,		/* pointer to pointer to assign rec  */
assign_environment	*aep)
{
	int	i, lim, found;
	assign_record *lar;
	char	type;

	type	= aoidp->type;

	lim	= aep->rec_cnt;
	lar	= aep->ar;
	found	= 0;

	for (i = 0 ; i < lim ; i++) {

		switch (type) {
		case BYUNIT:
			if ((lar[i].id.type == type) &&
		    	    lar[i].id.u.unit == aoidp->u.unit) {
				found	= 1;
				goto found_it;
			}
			break;
		default:
			if ((lar[i].id.type == type) &&
		    	    strcmp(lar[i].id.u.str, aoidp->u.str) == 0) {
				found	= 1;
				goto found_it;
			}
			break;
		}
	}

found_it:
	if (found) {
		*arpp	= &lar[i];
		return(1);
	}
	else {
		return(0);
	}
}

/*
 * _ae_match_pattern
 *
 *	Finds the assign record for a "patttern" (p:) assign object 
 *	which most closely matches fname.
 *
 * 	Return value:
 *
 *		The number of patterns matched.   arrp is assigned the pointer
 *		to the last pattern which matches.
 *
 *		The "p:%" pattern, which matches any name, is special.   If 
 *		this pattern and one or more other patterns all match, the
 *		"p:%" match is ignored--the return value is reduced by one
 *		and the returned assign record is for the latter of the
 *		remaining matches. 
 */
int
_ae_match_pattern(
const char		*fname,
assign_record		**arpp,		/* pointer to pointer to assign rec  */
assign_environment	*aep)
{
	int		i, lim, found;
	assign_record	*lar;
	assign_record	*percent_pat;
	assign_record	*match;

	lim		= aep->rec_cnt;
	lar		= aep->ar;
	percent_pat	= 0;
	found		= 0;

	for (i = 0 ; i < lim ; i++) {
		if (lar[i].id.type == BYPATTERN &&
	    	    _patmatch(fname, lar[i].id.u.str)) {
			if (strcmp("%", lar[i].id.u.str) == 0)
				percent_pat	= &lar[i];
			else {
				match		= &lar[i];
				found++;
			}
		}
	}

	if (found == 0 && percent_pat != NULL) {
		found	= found + 1;
		match	= percent_pat;
	}

	*arpp	= match;

	return(found);
}

/*
 * _asndir_split	
 *
 *	Extracts the assign options and assign object from
 *	a string (buf) containing an assign directive.
 *	of the form:
 *
 *		assign [options] [object]
 *
 *	NULLs are placed in the string just after the 
 *	assign options and the assign object in "buf".
 *
 *	A pointer to a NULL byte is returned in "*options" 
 *	or "*object" if no assign options or object are 
 *	present, respectively.
 *
 *	Return status: 0, or -1 on error with the error code
 *	in errno.
 */
int
_asndir_split(
char *buf,		/* buffer containing "assign -s pure f:foo"-type str */
char **options,		/* output pointer to options within buf */
char **object,		/* output pointer to assign object in buf */
int  ifasgcmd)		/* 1 iff asgcmd environment */
{
	char *p, *p2;
	char *opp;
	char *cmdname;

	*options	= "";
	*object		= "";

	p	= buf;
	_AE_SKIPWHITE(p);		/* advance past any white space */

	p2	= strpbrk(p, " \t");
	if (p2 != NULL)
		*p2++	= '\0';	   	/* put NULL at end of first word */
	else
		p2	= p + strlen(p);	/* stop at end of input string */

	cmdname	= ifasgcmd ? "asgcmd": "assign";

	if (strcmp(p, cmdname) != 0) {
		errno	= ERAS_DIRFMT;	/* no "assign"/"asgcmd" at beginning */
		return(-1);
	}

	p	= p2;
	_AE_SKIPWHITE(p);
	if (*p == '\0')
		return(0);		/* no options or assign object */

	/*
	 * The first nonwhite data is the start of the options
	 * list, or the start of the object if no attributes are
	 * present.
	 */
	opp	= p;

	/*
	 * Scan backwards from the end of the line for the assign
	 * object, which may be of one of these forms:
	 *
	 *	u:10
	 *	f:filename
	 *	p:filename_pattern
	 *	filename
	 */
	p	= p + strlen(p) - 1;		/* end of line string */
	while (isspace(*p)) {		/* skip trailing white*/
		p--;
	}
	*(p+1)	='\0';			/* trim trailing blanks */

	while (p >= opp && !isspace(*p)) { /* skip backwards over nonblanks */
		p--;
	}
	*p++	='\0';			/* put NULL at end of options */
	if (p != opp) {
		*options	= opp;		/* options and object were present */
		*object	= 	p;
	}
	else if (*p == '-')
		*options	= opp;	/* options were, object wasn't present*/
	else
		*object		= opp;	/* object was, options weren't present*/

	return(0);
}
/*
 * _lae_get_object
 *
 *	Parses a string containing an assign object specification.
 *
 *	Returns 0 on normal return, -1 on error with errno set
 *	to the error code.
 */
int
_lae_get_object(
char		*objtext,	/* input text string */
assign_obj_id	*aoidp)		/* output assign object */
{
	char	*endp;
	int	len;

	len	= strlen(objtext);

	if (len >= 2 && objtext[1] == ':') {
		aoidp->type	= objtext[0];
		objtext		= objtext + 2;
	}
	else if (len >= 1) {
		aoidp->type	= BYFILE;	/* default assign by file */
	}
	else {
		ERRET(ERAS_BADTYPE);		/* null string */
	}

	switch (aoidp->type) {
	case BYUNIT:
		aoidp->u.unit	= strtol(objtext, &endp, 10);

		if (endp == objtext || !(isspace(*endp) || *endp == '\0')) {
			ERRET(ERAS_BADUNIT);
		}
		break;
	default:
		if (strchr(objtext, ' ') != NULL) {
			ERRET(ERAS_FNBL);	/* blank in file name/pattern */
		}
		aoidp->u.str	= objtext;
		break;
	}
	return(0);
}

/*
 * _patmatch		
 *
 *	Determines if pattern pat matches string str.   Pat may
 *	contain text plus two special characters:
 *
 *		% - wildcard which matches any string of 0 
 *		    or more characters.
 *		_ - wildcard which matches any 1 character.
 *
 *	Algorithm:
 *
 *		A recursive parsing scheme attempts to map the pattern onto
 *		the input string.
 *
 *	Return value:
 *		1 if the pattern matches.
 *		0 if the pattern does not match.
 */

int
_patmatch(
const char	*str,
const char	*pat)
{
	int	len;
	char	*limit;


	if (*pat == '\0' && *str == '\0')
		return(1);				/* pattern matched */

	if (*pat == '\0')
		return(0);				/* match failed */

	if (*pat == '%') {
		pat++;

		if (*pat == '\0')
			return(1);			/* pattern matched */

		limit	= strpbrk(pat, "%_");

		if (limit == NULL)
			len	= strlen(pat);
		else
			len	= limit - pat;

		while (*str != '\0') {
			char	*mch;

			mch	= strnstrn(str, strlen(str), pat, len);

			if (mch == NULL)
				return(0);		/* match failed */

			if (_patmatch(mch + len, pat + len))
				return(1);		/* pattern matched */

			str	= mch + 1;
		}
	}
	else if (*pat == '_') {
		if (*str == '\0')
			return(0);			/* match failed */
		return(_patmatch(str + 1, pat + 1));
	}
	else {
		limit	= strpbrk(pat, "%_");
		if (limit == NULL)
			len	= strlen(pat);
		else
			len	= limit - pat;

		if (strncmp(str, pat, len) != 0)
			return(0);			/* match failed */
 
		return(_patmatch(str + len, pat + len));
	}

	return(0);					/* never reached */
}


struct {
	int		code;
	char		*name;
} _glob_obj_names[] = {
	ASN_G_ALL,		"all",
	ASN_G_SF,		"sf",
	ASN_G_SU,		"su",
	ASN_G_DF,		"df",
	ASN_G_DU,		"du",
	ASN_G_AQ,		"aq",
	ASN_G_FF,		"ff",
	ASN_G_MPI,		"mpi",
};

#define NUM_ASN_G	(sizeof(_glob_obj_names) / sizeof(_glob_obj_names[0])) 
	
/*
 * _ae_glob_name
 *
 *	Return the string which corresponds to an ASN_G_* value.
 */
char *
_ae_glob_name(int ga)
{
	register int	i;

	for (i = 0; i < NUM_ASN_G; i++)
		if (_glob_obj_names[i].code == ga)
			return(_glob_obj_names[i].name);

	_lerror(_LELVL_ABORT, FEINTUNK);	/* internal error */

	return(NULL);
}

/*
 * _ae_glob_code
 *
 *	Return the code which corresponds to a "g:xxx" string.
 */
int
_ae_glob_code(char *str)
{
	int	i;
	for (i = 0; i < NUM_ASN_G; i++)
		if (strcmp(str, _glob_obj_names[i].name) == 0)
			return(_glob_obj_names[i].code);
	
	return(0);
}


/*
 * SETOFLAG
 *
 *	Used in _ae_setoflags.
 */
#define SETOFLAG2(aflg, afld, aip, oflag, flagmask) {\
        if ((aip)->aflg) { \
                if ((aip)->afld) \
                        flagmask |= oflag; \
                else \
                        flagmask &= ~oflag; \
		(aip)->aflg |= ATTR_USED; \
        } \
}
 
#define SETOFLAG(asnfield, aip, oflag, flagmask)        \
        SETOFLAG2(asnfield ## _flg, asnfield, (aip), oflag, flagmask)
 

/*
 * _ae_setoflags
 *
 *	Convenience function called by all open routines to 
 *	set open(2) flags based on -B, -r, -x, -w, and -L options.
 */
void
_ae_setoflags(assign_info *aip, int *flagmask)
{
	assert(O_APPEND != 0);	/* this assertion verifes that fcntl.h 
				 * is included */

#ifdef	O_DIRECT
	SETOFLAG(B_direct, aip, O_DIRECT, *flagmask)
#endif	/* O_DIRECT */
#ifdef	O_RAW
	SETOFLAG(r_raw, aip, O_RAW, *flagmask)
#endif	/* O_RAW */
#ifdef	O_LDRAW
	SETOFLAG(L_ldraw, aip, O_LDRAW, *flagmask)
#endif	/* O_LDRAW */
#ifdef	O_PARALLEL
	SETOFLAG(x_parallel, aip, O_PARALLEL, *flagmask)
#endif	/* O_PARALLEL */
#ifdef	O_WELLFORMED
	SETOFLAG(w_welfrm, aip, O_WELLFORMED, *flagmask)
#endif	/* O_WELLFORMED */

	return;
}
