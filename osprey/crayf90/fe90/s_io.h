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



/* USMID:  "\n@(#)5.0_pl/headers/s_io.h	5.1	04/29/99 21:22:31\n" */


/****************************\
|* END, ERR, EOR list idxs. *|
\****************************/

static int		end_list_idx;
static int		err_list_idx;
static int		eor_list_idx;

static boolean		have_iostat;

/***********************************************\
|* flag for io stmt loop expansion.            *|
\***********************************************/

static	int		alt_return_branch_idx;
static	boolean		io_stmt_must_be_split;
static	boolean		three_call_model = FALSE;

/****************************\
|* global flag for s_io.c   *|
\****************************/

static boolean		list_directed;

/****************************************************************\
|* Interface table for IO stmt control list arguments.          *|
\****************************************************************/

extern	char		io_stmt_str[11][16];
extern	int		arg_idx_tbl[8][26];
extern	ciitem_tbl_type	ciitem_tbl[NUM_IO_STMT_TYPES];

/**************************************************************************\
|* extern to expr_semantics flags.                                        *|
\**************************************************************************/

extern boolean  namelist_illegal;

/**************************************************************************\
|* Variables global to s_io.c and p_io.c.                                 *|
\**************************************************************************/

extern boolean  is_namelist;
extern int      imp_do_var_list;
static int	namelist_descriptor_attr;

/***************************************************************************\
|* These tables give offsets for solaris open, close, buffer inquire tbls. *|
|* Actually, they are for any platform that uses two word fcd's.           *|
\***************************************************************************/

enum io_descript_entry  {	Buffer_Desc,
				Close_Desc,
				Inquire_Desc,
				Open_Desc};

typedef enum io_descript_entry	io_descriptor_type;

static int descriptor_size_tbl[4] = {8, 6, 42, 23};

static int offset_tbl[4][26] = {
	/* Buffer_Desc */
        	{1,	/* int             version;	*/
        	 1,	/* _f_int          *unit;	*/
        	 1,	/* _f_int          *recmode;	*/
        	 2,	/* gfptr_t         bloc;	*/
        	 2,	/* gfptr_t         eloc;	*/
        	 1	/* f90_type_t      *tiptr;	*/
                },

	/* Close_Desc */
        	{1,	/* int     version;	*/
        	 1,	/* _f_int  *unit;	*/
        	 1,	/* _f_int  *iostat;	*/
        	 1,	/* int     err;		*/
        	 2,	/* _fcd    status;	*/
		},


	/* Inquire_Desc */
		{1,	/* int     version;	*/
        	 1,	/* _f_int  *unit;	*/
        	 2,	/* _fcd    file;	*/
        	 1,	/* _f_int  *iostat;	*/
        	 1,	/* int     err;		*/
        	 1,	/* _f_log  *exist;	*/
        	 1,	/* _f_log  *opened;	*/
        	 1,	/* _f_int  *number;	*/
        	 1,	/* _f_log  *named;	*/
        	 2,	/* _fcd    name;	*/
        	 2,	/* _fcd    access;	*/
        	 2,	/* _fcd    sequential;	*/
        	 2,	/* _fcd    direct;	*/
        	 2,	/* _fcd    form;	*/
        	 2,	/* _fcd    formatted;	*/
        	 2,	/* _fcd    unformatted;	*/
        	 1,	/* _f_int  *recl;	*/
        	 1,	/* _f_int  *nextrec;	*/
        	 2,	/* _fcd    blank;	*/
        	 2,	/* _fcd    position;	*/
        	 2,	/* _fcd    action;	*/
        	 2,	/* _fcd    read;	*/
        	 2,	/* _fcd    write;	*/
        	 2,	/* _fcd    readwrite;	*/
        	 2,	/* _fcd    delim;	*/
        	 2	/* _fcd    pad;		*/
		},

	/* Open_Desc */
        	{1,	/* int     version;	*/
        	 1,	/* _f_int  *unit;	*/
        	 1,	/* _f_int  *iostat;	*/
        	 1,	/* int     err;		*/
        	 2,	/* _fcd    file;	*/
        	 2,	/* _fcd    status;	*/
        	 2,	/* _fcd    access;	*/
        	 2,	/* _fcd    form;	*/
        	 1,	/* _f_int  *recl;	*/
        	 2,	/* _fcd    blank;	*/
        	 2,	/* _fcd    position;	*/
        	 2,	/* _fcd    action;	*/
        	 2,	/* _fcd    delim;	*/
        	 2	/* _fcd    pad;		*/
		}};

