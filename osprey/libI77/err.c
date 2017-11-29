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



/* $Header: /proj/osprey/CVS/open64/osprey1.0/libI77/err.c,v 1.1.1.1 2005/10/21 19:00:00 marcel Exp $ */
/*	3.0 SID #	1.3	*/
/* who	ref.	date		description		      		*/
/* AGC	    	9Jan87		core dump only if requested           	*/
/* AGC	    	9Jan87		don't try to print ftnunit info if it */
/*                              not available                         	*/
/* AGC	#950    8Apr87		Added error number 131 			*/
/* sjc	#1649   5Oct87		Made f77vms_flag_ an array 		*/
/* sjc	#1676	13Oct87		Initialize unreferenced globals to eliminate */
/*				noisy linker messages in -G 0 operation */
/* sjc  #1963	11Dec87		Dynamically allocate unit table		*/
/* bcn		22Nov88		Add mp_cleanup and mp_setup		*/
/* bcn		23Oct89		Remove mp_cleanup and mp_setup		*/
/* bcn		26Feb90		Add __mp_check_setup			*/

#include <cmplrs/fio.h>
#include <mutex.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <mutex.h>
#include <unistd.h>

#include "err.h"
#include "fmt.h"
#include "iomode.h"
#include "util.h"
#include "cmplrs/f_errno.h"
#include "idxio.h"

unit * Internal_File;
int fmt_check = 0;

#define STR(x) (x==NULL?"":x)

#define SHUTUP_G0 = 0		/* sjc #1676 */

#include "vmsflags.h"

/*error messages*/
static char    *f77F_err[] =
{
 "error in format",		/* 100 */
 "out of space for unit table",	/* 101 */
 "formatted io not allowed",	/* 102 */
 "unformatted io not allowed",	/* 103 */
 "direct io not allowed",	/* 104 */
 "sequential io not allowed",	/* 105 */
 "can't backspace file",	/* 106 */
 "null file name",		/* 107 */
 "can't stat file",		/* 108 */
 "file already connected",	/* 109 */
 "off end of record",		/* 110 */
 "truncation failed in endfile",/* 111 */
 "incomprehensible list input",	/* 112 */
 "out of free space",		/* 113 */
 "unit not connected",		/* 114 */
 "read unexpected character",	/* 115 */
 "blank logical input field",	/* 116 */
 "bad variable type",		/* 117 */
 "bad namelist name",		/* 118 */
 "variable not in namelist",	/* 119 */
 "no end record",		/* 120 */
 "namelist subscript out of range",	/* 121 */
 "negative repeat count",	/* 122 */
 "illegal operation for unit",	/* 123 */
 "off beginning of record",	/* 124 */
 "no * after repeat count",	/* 125 */
 "'new' file exists",		/* 126 */
 "can't find 'old' file",	/* 127 */
 "unknown system error",	/* 128 */
 "requires seek ability",	/* 129 */
 "illegal argument",		/* 130 */
 "duplicate key value on write",/* 131 */
 "indexed file not open",	/* 132 */
 "bad isam argument",		/* 133 */
 "bad key description",		/* 134 */
 "too many open indexed files",	/* 135 */
 "corrupted isam file",		/* 136 */
 "isam file not opened for exclusive access",	/* 137 */
 "record locked",		/* 138 */
 "key already exists",		/* 139 */
 "cannot delete primary key",	/* 140 */
 "beginning or end of file reached",	/* 141 */
 "cannot find requested record",/* 142 */
 "current record not defined",	/* 143 */
 "isam file is exclusively locked",	/* 144 */
 "filename too long",		/* 145 */
 "cannot create lock file",	/* 146 */
 "record too long",		/* 147 */
 "key structure does not match file structure",	/* 148 */
 "direct access on an indexed file not allowed",	/* 149 */
 "keyed access on a sequential file not allowed",	/* 150 */
 "keyed access on a relative file not allowed",	/* 151 */
 "append access on an indexed file not allowed",	/* 152 */
 "must specify record length",	/* 153 */
 "key field value type does not match key type",	/* 154 */
 "character key field value length too long",	/* 155 */
 "fixed record on sequential file not allowed",	/* 156 */
 "variable records allowed only on unformatted sequential file",	/* 157 */
 "stream records allowed only on formatted sequential file",	/* 158 */
 "maximum number of records in direct access file exceeded",	/* 159 */
 "attempt to write to a readonly file",	/* 160 */
 "must specify key descriptions",	/* 161 */
 "carriage control not allowed for unformatted units",	/* 162 */
 "indexed files only",		/* 163 */
 "cannot use on indexed file",	/* 164 */
 "cannot use on indexed or append file",	/* 165 */
 "error on closing file",	/* 166 */
 "invalid code in format specification",	/* 167 */
 "invalid record number in direct access file",	/* 168 */
 "cannot have endfile record on non-sequential file",	/* 169 */
 /* LHL 5/17/89 * fix "ftell" bug -- 4539 */
 "cannot position within current file",				/* 170 */
 "cannot have sequential records on direct access file",	/* 171 */
 "cannot find namelist in input file ",				/* 172 */
 "cannot read from stdout",				/* 173 */
 "cannot write to stdin",				/* 174 */
 "stat call failed in f77inode",			/* 175 */
#ifdef I90
	"illegal value for specifier",			/* 176 */
	"end-of-record condition occurs with PAD=NO",	/* 177 */
	"EOR= specifier requires ADVANCE=NO",		/* 178 */
	"SIZE= specifier requires ADVANCE=NO",		/* 179 */
	"attempt to read from a writeonly file",	/* 180 */
 "direct unformatted io not allowed",  			/* 181 */
 "cannot open a directory",	      			/* 182 */
 "subscript out of bounds",                         	/* 183 */
 "function not declared as varargs",                   	/* 184 */
 "internal error",                                 	/* 185 */
 "illegal input value"	,				/* 186 */
 "position specifier is allowed only for sequential files",	/* 187 */
 "position specifier has an illegal value",		/* 188 */
 "Memory exhausted",					/* 189 */
 "already ALLOCATED (see F90 6.3.1.1)",			/* 190 */
 "not currently ALLOCATED (see F90 6.3.3.1)",		/* 191 */
 "not currently ASSOCIATED (see F90 6.3.3.2)",		/* 192 */
 "not created by ALLOCATE (see F90 6.3.3.2)",		/* 193 */
 "cannot be DEALLOCATEd via a pointer (see F90 6.3.3.2)"	/* 194 */
 "cannot keep a file opened as a scratch file"		/* 195 */

#else
 "", /* these 5 spaces are reserved for libI90 */       /* 176 */
 "",                   					/* 177 */
 "",                                  			/* 178 */
 "",                                  			/* 179 */
 "",                                  			/* 180 */
 "direct unformatted io not allowed",  			/* 181 */
 "cannot open a directory",	      			/* 182 */
 "subscript out of bounds",                         	/* 183 */
 "function not declared as varargs",                   	/* 184 */
 "internal error"                                  	/* 185 */
 ,"illegal input value",				/* 186 */
 "", /* these 2 spaces are reserved for libI90 */       /* 187 */
 "",                   					/* 188 */
 "Memory exhausted",					/* 189 */
 "", /* these 5 spaces are reserved for fef90 codes */	/* 190 */
 "",							/* 191 */
 "",							/* 192 */
 "",							/* 193 */
 "",							/* 194 */
 "cannot keep a file opened as a scratch file",		/* 195 */
 "edit descriptor conflicts with the I/O item data type"/* 196 */
#endif
};

#define MAXERR (sizeof(f77F_err)/sizeof(char *)+F_ER)

static int      f77f_nerr = MAXERR + 1;
extern void     _cleanup(void);


void
f77fatal (unit *ftnunit, int n, char *s)
{
   char           *acc;
   char           *dumpflag;
   int             coredump = 0;

   if (n < 100 && n >= 0) {
      perror (s);		/* SYSDEP */
   } else if (n >= (int) MAXERR) {
      fprintf (stderr, "%s: illegal error number %d\n", s, n);
   } else if (n < 0) {
      fprintf (stderr, "%s: end of file %d\n", s, n);
   } else {
      fprintf (stderr, "%s: %s\n", s, f77F_err[n - 100]);
   }

   if (ftnunit) {		/* AGC 1/9/87 */
      switch (ftnunit->uacc) {
      case SEQUENTIAL:
      case APPEND:
	 acc = "sequential";
	 break;
      case DIRECT:
	 acc = "direct";
	 break;
      case KEYED:
	 acc = "indexed";
      default:
	 acc = "(null)";
      }
      fprintf (stderr, "apparent state: unit %d named %s\n",
	       ftnunit->luno, STR (ftnunit->ufnm));
      if (ftnunit->ufmt)
	 fprintf (stderr, "last format: %s\n", STR (ftnunit->f77fmtbuf));
      fprintf (stderr, "Unit %d is a %s %s %s file\n", ftnunit->luno,
	        acc, ftnunit->ufmt ? "formatted" : "unformatted",
	       !ftnunit->f77errlist.iciunit ? "external" : "internal");
   }
   iscleanup();

   /* fix bug 7580 the following is copied from s_abort.c in libF77 */

   if (dumpflag = getenv ("f77_dump_flag")) {
      coredump = up_low (*dumpflag) == 'y' ? 1 : 0;
   }
   if (coredump) {
      _cleanup ();
      abort ();			/* cause a core dump */
   } else {
      _cleanup ();
      fprintf (stderr, "*** Execution Terminated (%d) ***\n", n);
      exit (n);
   }

}


/*
FILE * debugfile;
*/

/*initialization routine*/
/* bron 22Aug94 - There is a significant problem here in that this
** routine calls other routines (i.e. map_luno) that themselves check
** the flag f77init.  For MP safe routines, f77init must be set *last*,
** so I invented some mechanism to avoid recursive calls to this routine.
** A better approach would be to alter the called routines so that they
** do not need to check the f77init flag.
*/
void
f_init (void)
{
   unit           *p;
   static unsigned long	  init_flag = 0;
   static pid_t  f_init_pid = 0;
   char *num, *v;

   if (test_and_set( &init_flag, 1L ) != 0) {
	/* If I accidently called this routine recursively, just return */
	if (getpid() == f_init_pid) return;

       /* Make sure everything got set up before returning */
       while (f77init == 0) {
           sginap(0);
       }
       return;
   }
   f_init_pid = getpid();

   if ((num = getenv ("FORTRAN_OPENED_UNITS")) != NULL)
      mxunit = atol(num) + 4; /* user specifies number of units! plus 
				4 default system units */
   else
       mxunit = INIT_MXUNIT;

   f77units =
      (unit *) calloc (mxunit, sizeof (unit));

   /* Set up unit zero as STDERR. */

   p = map_luno (0);
   p->ufd = stderr;
   p->luno = 0;
   p->uconn = 1;
   p->useek = f77canseek (stderr);
   p->uistty = isatty (fileno(stderr));
   p->ufmt = 1;
   p->ucc = CC_LIST;
   p->uwrt = WR_READY;
   p->ualias = p;

   /* Set up unit five as STDIN. */

   p = map_luno (5);
   p->ufd = stdin;
   p->luno = 5;
   p->uconn = 1;
   p->useek = f77canseek (stdin);
   p->uistty = isatty (fileno(stdin));
   p->ufmt = 1;
   p->ucc = CC_NONE;
   p->uwrt = 0;

   /* Set up unit six as STDOUT. */

   p = p->ualias = map_luno (6);
   p->ufd = stdout;
   p->luno = 6;
   p->uconn = 1;
   p->useek = f77canseek (stdout);
   p->uistty = isatty (fileno(stdout));
   p->ufmt = 1;
   p->ucc = (char) (f77vms_flag_[VMS_CC] ? CC_FORTRAN : CC_LIST);
   p->ucchar = '\0';
   p->uwrt = WR_READY;

   /* Use the next available slot for Internal file */
   p = map_luno(-1);
   Internal_File = p;
   p->uconn = 1;
   p->ufmt = 1;
   p->uwrt = WR_READY;

   if ((v = getenv ("FMT_CHECK")) != NULL) {
     if (*v == 'Y' || *v == 'y') {
       fmt_check = 1;
     }
   }

/* debugfile = fopen( "debugfile", "w" ); */
   /* This *must* be last */
   f77init = 1;
   /* Since this function should only be executed once, thsi flag should
   never be released.  Releasing this produces a small window where a
   thread sees f77init==0 and then execute this routines.  By the time it
   gets in here init_flag has already been set to 0 and so it can execute
   this again and destroyed all previous information set up in the old
   f77units
   init_flag = 0;
   */
}


int
f77canseek (FILE *f)
{
   struct stat     x;

   (void) fstat (fileno (f), &x);
   if (x.st_nlink > 0 /* pipe */ && !isatty (fileno (f))) {
      return (1);
   }
   return (0);
}


void
perror_ (char *s, int len)
{
   unit           *lu;
   char            buf[40];
   char           *mesg = s + len;

   while (len > 0 && *--mesg == ' ')
      len--;
   if (errno >= 0 && errno < sys_nerr)
      mesg = sys_errlist[errno];
   else if (errno >= F_ER && errno <= f77f_nerr)
      mesg = f77F_err[errno - F_ER];
   else {
      sprintf (buf, "%d: unknown error number", errno);
      mesg = buf;
   }
   lu = (unit *) map_luno (0);	/* AGC #701 2/17/87 */
   while (len-- > 0)
      putc (*s++, lu->ufd);
   fprintf (lu->ufd, ": %s\n", mesg);
}



void
gerror_ (char *s, int len)
{
   char           *mesg;

   if (errno >= F_ER && errno < f77f_nerr)
      mesg = f77F_err[errno - F_ER];
   else if (errno >= 0 && errno < sys_nerr)
      mesg = sys_errlist[errno];
   else
      mesg = "unknown error number";
   b_char (mesg, s, len);
}


void
strerror_ (int *errno, char *s, int len)
{
   char           *mesg;

   if (*errno >= F_ER && *errno < f77f_nerr)
      mesg = f77F_err[*errno - F_ER];
   else if (*errno >= 0 && *errno < sys_nerr)
      mesg = sys_errlist[*errno];
   else
      mesg = "unknown error number";
   b_char (mesg, s, len);
}
