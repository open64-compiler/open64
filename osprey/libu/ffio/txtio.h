/*
 * Copyright 2004 PathScale, Inc.  All Rights Reserved.
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


/* USMID @(#) libu/ffio/txtio.h	92.0	10/08/98 14:57:41 */


/*
 * TEXT class end-of-file recognition (disable|enable)
 */
#define TEXT_NL		1	/* NO WEOF allowed with text layer (default) */
#define TEXT_NL_WEOF	2	/* allows WEOF with text layer */
#define TEXT_205	3	/* CYBER 205 text file */
#define TEXT_CTSS	4	/* CTSS text file */
/*
 * Number of TEXT record types.
 */
#define NUM_TEXT_TYPES	4
/*
 *	We don't support other char lengths.
 */
#define CHAR_LEN 8
#ifdef MAX
#undef MAX
#endif
#define MAX(a,b) ((a) > (b) ? (a) : (b))

#define TEXT_MAGIC_MARKER 0x7e650aUL
/*
 * These are used to shift the eor_char and eof_mark to make
 * them left or right justified in a word.
 */
#if defined (_CRAY) || (_MIPS_SZLONG == 64)
#define CHAR_JUSTIFY 56
#define TEXTNL_JUSTIFY_SHIFT 40
#else
#define CHAR_JUSTIFY 24
#define TEXTNL_JUSTIFY_SHIFT 8
#endif

/*
 *	Private information to the text layer.
 *		Note: all of these are declared long, but eor_char and
 *		eof_mark are stored left-justified in the words.
 *		This allows the eor and eof marks to be between 8 and 64
 *		characters long (8 and 32 characters long on 32 bit systems),
 *		but means some funky code when calling
 *		routines like memchr(3). (There are, however, some places
 *		in the text layer where it assumes that an eor mark is
 *		only 8 bits long). The C compiler promotes char
 *		parameters to int when making calls, so a shift to right
 *		justify these is necessary when calling these routines.
 */
struct text_f
	{
	long	eor_char;	/* EOR character */
	long	eof_mark;	/* EOF mark, including any EOR. i.e. "~e\n" */
	int	eor_len;	/* length of EOR mark in bits */
	int	eof_len;	/* length of EOF mark in bits */
	};

extern _ffopen_t _txt_open(const char *name, int flags, mode_t mode, struct fdinfo *fio, union spec_u *spec, struct ffsw *stat, long cbits, int cblks, struct gl_o_inf *inf);

extern ssize_t _txt_read(struct fdinfo *fio, bitptr bufptr, size_t nbytes, struct ffsw *stat, int fulp, int *ubc);
extern int _txt_weof(struct fdinfo *fio, struct ffsw *stat);

extern int _txt_weod(struct fdinfo *fio, struct ffsw *stat);

extern ssize_t _txt_write(struct fdinfo *fio, bitptr bufptr, size_t nbytes,
struct ffsw *stat, int fulp, int *ubc);
