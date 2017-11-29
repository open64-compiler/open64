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

/* USMID @(#) clibinc/cray/assign.h	92.2	12/18/98 09:49:04 */

#ifndef _CRAY_ASSIGN_H
#define _CRAY_ASSIGN_H


#ifndef	_LIB_INTERNAL
#define	_LIB_INTERNAL 1		/* expose internal FFIO definitions */
#endif
#include <ffio.h>
#include <liberrno.h>
#include <stddef.h>
#include <stdio.h>
#include <cray/fortio.h>
#include <cray/mtlock.h>
#include <sys/param.h>


/************************************************************************
 *									*
 *	CONSTANTS 							*
 *									*
 ************************************************************************/


/*
 * Assign option values.
 */

#define AS_SKIPBAD	1		/* -d skipbad */ 
#define AS_ACPTBAD	2		/* -d acptbad */ 

#define AS_FORTRAN77	1		/* -f 77 */ 
#define AS_FORTRAN90	2		/* -f 90 */
#define AS_IRIX_F77	3		/* -f irixf77 */ 
#define AS_IRIX_F90	4		/* -f irixf90 */

#define AS_THREAD	1		/* -P thread */
#define AS_PROCESS	2		/* -P process */
#define AS_TEAM		3		/* -P team */
#define AS_PRIVATE	4		/* -P private */
#define AS_GLOBAL	5		/* -P global */

/*
 * Other constants.
 */

#define BYFILE		'f'
#define BYGLOBAL	'g'
#define BYPATTERN	'p'
#define BYUNIT		'u'

#define MAX_FDC_SPEC	128		/* max words in an FDC specification */

#define MAX_ASSIGN_LINE	5000		/* maximum number of characters in an */
					/* assign record.  This number must   */
					/* be comfortably greater than 	      */
					/* (PATH_MAX * 4 + MAX_FDC_SPEC)      */
					/* as long as the hybrid assign env   */
					/* file format is supported (through  */
					/* UNICOS 7.0?).  After that time,    */
					/* this constant may be reduced to    */
					/* (PATH_MAX * 2 + MAX_FDC_SPEC + pad)*/

#define	DECIMAL		10		/* for strtol */

#define AE_LIB		1		/* call from assign lib routines */
#define AE_ASSIGN	2		/* call from assign command */
#define AE_ASGCMD	3		/* call from asgcmd command */

/*
 *	Strings and characters
 */

#define ASSIGN_ENV_VAR	"FILENV"	/* pointer to assign environment */
#define ASSIGN_OPT_VAR	"_LIBASSIGNENV_"/* default asn environment repository */
#define ASGCMD_ENV_VAR	"_ASG_ATTR"	/* location of asgcmd attributes */
#define PROCENVFLAG	'$'		/* indicate an environment var name */
#define DELIMSTR	" # # "		/* delimiter in environment strings */

/*
 *	Mask bits which identify searches for g:all/sf/su/df/du/aq/ff/mpi
 *	assign objects.
 */

#define	ASN_G_ALL	 0001
#define	ASN_G_SF	 0002
#define	ASN_G_SU	 0004
#define	ASN_G_DF	 0010
#define	ASN_G_DU	 0020
#define	ASN_G_AQ	 0040
#define	ASN_G_FF	 0400
#define	ASN_G_MPI	01000

/************************************************************************
 *									*
 *	MACROS used by internal assign parsing 				*
 *									*
 ************************************************************************/

#define RETERR(_EH,_NUM)	{	\
	_lerror(_EH,_NUM);		\
	errno	= _NUM;			\
	return(-1);			\
}
#define RETERR1(_EH,_NUM,_D1)	{	\
	_lerror(_EH,_NUM,_D1);		\
	errno	= _NUM;			\
	return(-1);			\
}
#define RETERR2(_EH,_NUM,_D1,_D2) {	\
	_lerror(_EH,_NUM,_D1,_D2);	\
	errno	= _NUM;			\
	return(-1);			\
}
#define ERRET(ee) { errno = ee; return(-1); }

/*
 * ASSIGN_LOCK	protects multitasked programs when executing in sensitive
 *		areas in assign library routines.   ASSIGN_LOCK must be
 *		called whenever:
 *
 *		    (1)	- the assign environment file is open.  File updates
 *			  across unrelated processes are forced as 
 *			  single-threaded by file locking.   For tasks in
 *			  a multitasked group or multiple tasks/PEs on an MPP, 
 *			  file locking will not cause one task to wait for a 
 *			  lock held by another task.  So a multitasking lock 
 *			  is needed too.
 *
 *	(non-MPP)   (2)	- the local assign environment is being accessed.
 *			  The local assign environment is global to all tasks
 *			  and must be accessed by a single task at a time.
 *			  The local assign environment is enabled when ASNCTL 
 *			  is called with the 'LOCAL' or 'NEWLOCAL' options.
 *
 *	(non-MPP)   (3)	- fopen() or fclose() are called for the assign 
 *			  environment file.   This ensures that updates to
 *			  the __iob table are single threaded.  Note that
 *			  (1) implies (3).
 *
 * ASSIGN_UNLOCK Unlocks the assign library lock.
 */ 

#ifndef	_CRAYMPP

#define ASSIGN_LOCK()		MEM_LOCK(&_Ae_assign_lock)
#define ASSIGN_UNLOCK()		MEM_UNLOCK(&_Ae_assign_lock)

#else	/* _CRAYMPP */

#define ASSIGN_LOCK()		{ if (_num_pes() > 1) \
					MPP_LOCK(&_Ae_assign_lock); }
#define ASSIGN_UNLOCK()		{ if (_num_pes() > 1) \
					MPP_UNLOCK(&_Ae_assign_lock); }

#endif	/* _CRAYMPP */

/*
 * _AE_SKIPWHITE - skips spaces and tabs.
 */
#define _AE_SKIPWHITE(pp) { \
	register char	cc;			\
	cc	= *pp;				\
	while (isspace(cc) && cc != '\0')	\
		cc	= *++pp;		\
}


/*
 * _AE_NULLINFO - sets an assign_info structure to contain no attributes.
 */
#define _AE_NULLINFO(P)	((void)memset((char*)P,0,sizeof(assign_info)))

/*
 * Assign debugging macros.   Define DEBUG_ASSIGN to activate these macros.
 */
#ifdef	DEBUG_ASSIGN
#define ASN_DEBUG( ARGLIST )	printf ARGLIST
#else
#define ASN_DEBUG( ARGLIST )
#endif

/*
 * AFLAGSIZE	number of words in the flags part of the assign_info
 *		structure.  Includes flagpad to allow compatibility
 *		of mismatched libu and libf (in some cases).
 */
#define AFLAGSIZE ((offsetof(assign_info, flagpad) +			\
		   sizeof(((assign_info*) 0)->flagpad)) ) 

/*
 * _ae_opt_control	- returns 1 if a letter is a control option 
 *
 * _ae_opt_noptval	- returns 1 if a letter is an option which does
 *			  not take an option value 
 *
 * _ae_opt_optval	- returns 1 if a letter is an option which takes
 *			  an option value which is not an integer or
 *			  merely "off"/"on".
 */
#define _ae_opt_control(ch)  \
	((ch)>='A' && (ch)<='z' && _Ae_letters[(ch)-'A'] == 'c')

#define _ae_opt_nooptval(ch) \
	((ch)>='A' && (ch)<='z' && _Ae_letters[(ch)-'A'] == 'n')

#define _ae_opt_optval(ch) \
	((ch)>='A' && (ch)<='z' && _Ae_letters[(ch)-'A'] == 'v')


/************************************************************************
 *									*
 *	STRUCTURE DEFINITIONS 						*
 *									*
 ************************************************************************/

typedef struct {
	char	type;			/* BYUNIT	- unit		      */
					/* BYFILE	- file name 	      */
					/* BYGLOBAL	- g:XXX		      */
					/* BYPATTERN	- file name pattern   */
	union {
		long	unit;		/* for assign by unit */
		char	*str;		/* for other types of assign objects */
	} u;				
} assign_obj_id;

/*
 * assign_info	 	- this structure is used for returning assign
 *			  environment information (attributes) for a particular 
 *			  assign object to the user.
 *
 *			  The *_flg fields have the fillowing flags which
 *			  might be set:
 *
 *				ATTR_SET	- attribute is specified
 *				ATTR_USED	- attribute has been used
 *
 *			  The ATTR_SET bit is set during assign parsing.  The
 *			  ATTR_USED bit is for the convenience of library
 *			  open processing.
 */
enum asn_flags {
	ATTR_SET	= 1,
	ATTR_USED	= 2
};

typedef unsigned char aflg_t;

typedef struct assign_info_s {

	aflg_t	a_actfil_flg;	
	aflg_t	a_sdsfil_flg;	
	aflg_t	b_bufsiz_flg;	
	aflg_t	B_direct_flg;
	aflg_t	c_contig_flg;		
	aflg_t	C_chrcnv_flg;
	aflg_t	d_datrcv_flg;
	aflg_t	D_fildes_flg;
	aflg_t	f_fortst_flg;	
	aflg_t	F_filter_flg;	
	aflg_t	l_buflev_flg;
	aflg_t	L_ldraw_flg;
	aflg_t	m_multup_flg;
	aflg_t	n_preall_flg;
	aflg_t	n_stride_flg;
	aflg_t	N_datcnv_flg;
	aflg_t	o_UNUSED_flg;
	aflg_t 	P_ioscop_flg;
	aflg_t	q_ocblks_flg;
	aflg_t	pr_partit_flg;
	aflg_t	r_raw_flg;
	aflg_t	s_fstrct_flg;	
	aflg_t	S_comsep_flg;	
	aflg_t	t_tmpfil_flg;
	aflg_t	T_utrunc_flg;
	aflg_t	u_bufcnt_flg;
	aflg_t	U_unicoslist_flg;
	aflg_t	w_welfrm_flg;
	aflg_t	W_compwidth_flg;
	aflg_t	x_parallel_flg;
	aflg_t	y_reptcnt_flg;
	aflg_t	Y_nl_skip_flg;
	aflg_t	Z_neg_zero_flg;

	aflg_t	flagpad[9];	/* marker & pad for end of flags */

	char	a_actfil[PATH_MAX];	/* -a option.			      */
					/* Actual file opened when this	      */
					/* file or unit appears in an OPEN.   */
					/* -a SDS option.		      */
					/* Indicates SDS file.		      */

	int	b_bufsiz;		/* -b option.			      */
					/* Requested size of library buffer   */
					/* in 512-word blocks.		      */

	int	B_direct;		/* -B option			      */
					/* When == 1, set O_DIRECT on	      */
					/* open(2) system call.		      */

	int	c_contig;		/* -c option. (this field not used)   */

	int	C_chrcnv;		/* -C options.		   	      */
					/* Character conversion	code.	      */

	int	d_datrcv;		/* -d option.			      */
					/* AR_SKIPBAD or AR_ACPTBAD	      */
					/* Bad data recovery options.	      */

	int	D_fildes;		/* -D option			      */
					/* Connect to file descriptor	      */

	int	f_fortst;		/* Fortran 77 or 90 conformancy	      */

	union spec_u			/* -F option			      */
		F_filter[MAX_FDC_SPEC];	/* Binary filter specification.	      */
	int	F_filter_len;		/* Size of the filter spec (in words) */

	int	l_buflev;		/* -l option.			      */
					/* System buffering indicator.	      */
					/* 1 indicates to open file to use    */
					/* the system I/O buffers.   0 means  */
					/* open the file in RAW mode.	      */

	int	L_ldraw;		/* -L option.			      */
					/* When == 1, set O_LDRAW on	      */
					/* open(2) system call.		      */

	int	m_multup; 		/* -m option.			      */
					/* When == 1, a direct access file    */
					/* may suppress a truncation at       */
					/* logical size at close time.        */

	int	n_preall;		/* -n option.			      */
					/* Pre-allocation size in 512-word    */
					/* blocks.			      */

	int	n_stride;		/* -n option.			      */
					/* Pre-allocation stride accross      */
					/* stripe partitions.  Only available */
					/* on the YMP.			      */

	int	N_datcnv;		/* -N options.		              */
					/* Numeric conversion	code.	      */

	int	o_UNUSED;		/* no longer used		      */

	long	pr_partit;		/* -p option.			      */
					/* Bit mask indicating which	      */
					/* partitions of the file system to   */
					/* which the system should attempt    */
					/* to allocate the file.	      */

	int 	P_ioscop;		/* -P option.			      */
					/* When == 'p' Fortran units are      */
					/* private to a task.  When == 'g'    */
					/* Fortran units are global.	      */

	int	q_ocblks; 		/* -q option.			      */
					/* cblks value for open(2) sys call   */

	int	r_raw;			/* -r option.			      */
					/* When == 1, set O_RAW on	      */
					/* open(2) system call.		      */

	int	s_fstrct;		/* -s option.			      */
					/* File structure code.		      */

	int	S_comsep;		/* -S option.			      */
					/* Use comma as the list-directed     */
					/* output separator.  Default on      */
					/* UNICOS.			      */

	int	t_tmpfil;		/* -t option. (this field not used)   */

	int	T_utrunc; 		/* -T option.			      */
					/* When == 1, a sequential Fortran    */
					/* file is truncated after writes.    */

	int	u_bufcnt;		/* -u option.			      */
					/* Requested number of library	      */
					/* buffers to use for a direct-access */
					/* file. 			      */

	int	U_unicoslist;		/* -U option.			      */
					/* Produce UNICOS form of	      */
					/* list-directed output on irix.      */
					/* Default on UNICOS.		      */
					/* This includes -S, -W, and -y.      */

	int	w_welfrm;		/* -w option.			      */
					/* When == 1, set O_WELLFORMED on     */
					/* open(2) system call.		      */

	int	W_compwidth;		/* -W option.			      */
					/* Produce compressed width output    */
					/* for list-directed output on irix.  */
					/* Default on UNICOS.		      */

	int	x_parallel;		/* -x option.		      	      */
					/* When == 1, set O_PARALLEL on	      */
					/* open(2) system call.		      */

	int 	y_reptcnt;		/* -y option.			      */
					/* Produce repeat counts for	      */
					/* list-directed output on irix.      */
					/* Default on UNICOS.		      */
	int 	Y_nl_skip;		/* -Y option.			      */
					/* Skip unmatched namelist            */
					/* group name on input 		      */
					/* Default is off.		      */
	int 	Z_neg_zero;		/* -Z option.			      */
					/* When on, print -0.0 if neg zero    */
					/* When off, print 0.0 if neg zero    */

	int	pad[10]; 		/* Pad space for future expansion     */

} assign_info;

typedef struct {
	unsigned	I: 1;		/* -I option			     */
	unsigned	O: 1;		/* -O option			     */
	unsigned	R: 1;		/* -R option			     */
	unsigned	V: 1;		/* -V option			     */
	unsigned	v: 1;		/* -v option (command only)          */
	unsigned	z: 1;		/* -z option (command only)          */
	int		attrs;
} assign_cntl;

typedef struct {
	char	attrid; 		/* letter identifying the attribute   */
	char	*str;			/* attribute value (text string)      */
	char	*p;			/* parsed data (optional)	      */
} attribute;

/*
 * assign_record - this union is used to store assign records internally.
 */
typedef struct {
	assign_obj_id		id;	/* assign object identifier           */
	char			*attr;	/* pointer to list of attributes      */
} assign_record;

/*
 * assign_environment - a set of 0 or more assign_records.
 */
typedef struct {
	int		rec_cnt;	/* Number of assign records in the    */
					/* assign environment.		      */
	int		rec_lim;	/* Space allocated for assign         */
					/* records.		              */
	assign_record	*ar;		/* Pointer to list of assign_records  */
} assign_environment;

/*
 * aenv_stack - a stack of 0 or more assign_environments.
 */
typedef struct {
	assign_environment      *env;   /* contiguous list of env's */
	long                    size;   /* number of env's in stack */
} aenv_stack;


typedef struct {
	int		iotype;		/* AE_FORTIO, AE_FFIO */
	union {
		struct {
			char	access;		/* 's' or 'd' */
			char	form;		/* 'f' or 'u' */
		} fortio;
		/* other types can be added here */
	} u;
} aio_desc;

/*
 * tok_list 		- identifies a list of valid option values and the
 *      		  corresponding internal code value which corresponds 
 *			  to it.
 */
typedef struct tok_list_s {
        char *str;
        int  code;
} tok_list;
 
/*
 * opt_table 		- identifies the location in the assign info 
 *			  structure where data is stored for an assign option.
 *			  It also points to the tok_list for this option.
 */
typedef struct opt_table_s {
        int     flg_off;
        int     val_off;
        tok_list *tl;
} opt_table;


/*
 * parse_info 		- contains information for parsing all supported assign 
 *			  attribute options.
 */
typedef struct parse_info_s {
        char            *optname;       /* option name ("-b", "-T")	      */
        int             flg_off;        /* location of opt flag bit           */
        int             val_off;        /* location of opt value field        */
        int (*          pfunc)();       /* option value parsing function      */
        tok_list        *tl;            /* list of valid tokens if applicable */
        unsigned        sup: 1;         /* 1 if supported on this system      */
        char            *desc;          /* displayed usage string             */
} parse_info;

#ifdef KEY /* Bug 4260 */
typedef enum {
  IO_DEFAULT,		/* Implicit: no swapping, no warning about FILENV */
  IO_NATIVE,		/* Explicit: no swapping, warn about FILENV */
  IO_SWAP,		/* Files are opposite of native, whatever that is */
  IO_BIG_ENDIAN,	/* Files are big-endian */
  IO_LITTLE_ENDIAN	/* Files are little-endian */
} IO_BYTESWAP;
#endif /* KEY Bug 4260 */


/************************************************************************
 *									*
 *	EXTERNS AND PROTOTYPES						*
 *									*
 ************************************************************************/


extern aenv_stack	_Ae_env_stack;
extern int		_Ae_asgcmd;
extern int		_Ae_assign_cmd;
#ifndef _CRAYMPP
EXTERN_LOCK(_Ae_assign_lock)
#else
EXTERN_MPP_LOCK(_Ae_assign_lock)
#endif
extern char 		_Ae_letters[];
extern parse_info	_Ae_option_parse_info[];

extern char *_ae_build_envstring(assign_environment *aep);

extern void  _ae_dealloc_env(assign_environment *aep);

extern void  _ae_dealloc_recflds(assign_record *arp);

extern int   _ae_externalize(int fromwhere, FILE *gfile,
			assign_environment *ap);

extern int   _ae_externalize_file(FILE *gfile, assign_environment *ap);

extern int   _ae_externalize_env(int fromwhere, assign_environment *ap);

extern int   _ae_glob_code(char *str);

extern char *_ae_glob_str(int ga);

extern int   _ae_insert(assign_obj_id *aoidp, char *attr, int attrlen, 
			assign_environment *aep);

extern int   _ae_internalize(FILE *gfile, assign_environment *aep);

extern int   _ae_internalize_file(FILE *gfile, assign_environment *aep);

extern int   _ae_internalize_env(assign_environment *aep, int ifasgcmd);

extern int   _ae_match_pattern(const char *fname, assign_record **arpp,
			assign_environment *aep);

extern int   _ae_select(assign_obj_id *aoidp, assign_record **arpp,
			assign_environment *aep);

extern void  _ae_setupenv(assign_environment *aep);

extern char  *_ae_glob_name(int ga);

extern int   _asndir_split(char *buf, char **options, char **object,
			int ifasgcmd);

extern int   _assign(char *opt_string, assign_obj_id *aop, int errmode);

extern int   _assign_asgcmd_info(const char *fname, unum_t unum, int gamask,
			assign_info *aip, char **atstr, int catcherr);

extern FILE *_gae_open(char acc_mode, char res_mode, int *status);

extern void  _gae_close(FILE *f);

extern int   _get_a_options(int ifasgcmd, const char *fname, unum_t unum,
			int gamask, assign_info *aip, char **atstr,
			int errmode);

extern char *_lae_get_assign_file_name(int *estat);

extern char *_lae_get_assign_var_name(void);

extern int   _lae_get_object(char *objtext, assign_obj_id *aoidp);

extern int   _patmatch(const char *str, const char *pat);

extern void  _unique_close(FILE *f);

extern FILE *_unique_open(char *fname, char mode, int *ostat);

extern int   _lae_do_assign(int fromwhere, int assign_mode, char open_mode,
			char res_mode, assign_obj_id *aoidp,
			char *attr_string, int optcheck, int errmode);

extern int   _lae_assign_mode(int fromwhere, assign_cntl *cnp, int numobj,
			int *amode, char *omode, char *rmode);

extern int	_attrs_used(assign_info *aip, char **attrstr);

extern void	_attr_clear_used(assign_info *aip);

extern int	_ae_eclipse(char *attr1, int len1, char *attr2, int len2,
		char **newarp);

extern int	_ae_parse(assign_obj_id *aiodp, char *attr, int attrlen,
		assign_info *aip, int warnmode, int errmode);

extern int	_lae_process_opts(char *opt_string, char **attr_string, 
		assign_cntl *cntlp);

extern int	_ae_delete(assign_record *arp, assign_environment *aep);

extern void	_lae_print_record(assign_record *arp);

extern int	_ae_next(assign_record *prev, assign_record **next, 
		assign_environment *aep);

extern int	_ae_check_attr(assign_info *ai, int warnmode, int errmode);

extern void	_ae_setoflags(assign_info *aip, int *flagmask);

#ifdef KEY /* Bug 4260 */
extern void	__io_byteswap();
#endif /* KEY Bug 4260 */

#endif	/* !_CRAY_ASSIGN_H */
