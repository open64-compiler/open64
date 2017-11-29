/*
 * Copyright 2004 PathScale, Inc.  All Rights Reserved.
 */

/*

  Copyright (C) 2000, 2001, Silicon Graphics, Inc.  All Rights Reserved.

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



#pragma ident "@(#) libf/fort/argchck.c	92.3	10/29/99 21:41:49"
#include <liberrno.h>
#include <stdio.h>
#include <string.h>

#if	defined(_UNICOS)
extern	int	_who_called_me();
#endif

/*
 *	_ARGCHCK is called by cft77 and cft90 compiled routines that
 *	were compiled with runtime argument checking on (-Ra).  It
 *	expects pointers to  two blocks of integer pointers which 
 *	describe the number and type of the actual and dummy arguments
 *	and the type of call being made (subroutine, real function,
 *	etc.).  With cft90, there are checks for kind type, array size,
 *	rank, character length, POINTER function reference, derived
 *	type, presence of nonoptional arguments, INTENT, and array
 *	versus scalar differences.  It matches number and call 
 *	type first and then if the number of arguments matches, the types
 *	of the arguments. It then issues appropriate messages with a 
 *	brief traceback. The descriptors that are the input parameters
 *	to this routine must have the following structure:
 *
 *	(argdes or dargdes) -> number of arguments(n)
 *				->			call type
 *				->			argument 1 type
 *				.
 *				.
 *				.
 *				.
 *				->			argument n type
 *	 optional --->		suppress message number 1
 *		   .		.
 *		   .		.
 *		   .		.
 *		   .		suppress message number m
 *		  --->		0
 *
 *	The call type and argument type are represented by integers
 *	which are described by the local array arg_msg (below).
 */

enum	arg_type_values {
			Null_Arg,                 
			Short_Integer_Arg,
			Long_Integer_Arg,
			Real_Arg,
			Double_Arg,
			Complex_Arg,
			Logical_Arg,
			Character_Arg,
			Pointer_Arg,
			Typeless_Arg,
			Character_Pointer_Arg,
			Label_Arg,
			Subroutine_Arg,
			Null_Function_Arg,
			Short_Integer_Function_Arg,
			Long_Integer_Function_Arg,
			Real_Function_Arg,
			Double_Function_Arg,
			Complex_Function_Arg,
			Logical_Function_Arg,
			Character_Function_Arg,
			Pointer_Function_Arg,
			Typeless_Function_Arg,
			Character_Pointer_Function_Arg,
			Subprogram_Arg,
			Derived_Type_Arg,
			Derived_Type_Function_Arg };

typedef enum	arg_type_values arg_type_type;

struct		arg_desc_header {
			unsigned int	seen_this	:1;
			unsigned int	f90_flag	:1;
			unsigned int	num_ck_only	:1;
			unsigned int	suppress_msg	:1;
			unsigned int	unused1		:4;
#if	defined(_UNICOS)
			unsigned int	unused2		:24;
			unsigned int	unused3		:8;
#endif
			unsigned int	arg_count	:24;
};

typedef struct	arg_desc_header arg_desc_header_type;

struct		arg_desc_node {
			long	arg_type;
			long	kind;
/* size is in terms of bytes for character, otherwise it's elements.
 * If it is assumed-size character, size is elements also.
 */
			long	size;
			long	char_len;
			long	rank;
			long	line;
			char	name[32];
#if	defined(_UNICOS)
			unsigned int	unused2			:32;
#endif
			unsigned int	unused1			:18;
			unsigned int	pgm_unknown		:1;
			unsigned int	pgm_unit		:1;
			unsigned int	ignore_tkr		:1;
			unsigned int	dope_vector		:1;
			unsigned int	pointer			:1;
			unsigned int	default_kind		:1;
			unsigned int	optional		:1;
			unsigned int	intent_out		:1;
			unsigned int	assumed_shape		:1;
			unsigned int	assumed_size_array	:1;
			unsigned int	assumed_size_char	:1;
			unsigned int	defineable		:1;
			unsigned int	array_element		:1;
			unsigned int	generic_call		:1;
			long		*derived_type_tbl;
};

typedef struct	arg_desc_node arg_desc_node_type;


#define SUBROUTINE_ATYPE 12
#define SUBPROGRAM_ATYPE 24
#define	MAX_ENT_LEN	32	/* Maximum entry point name length	*/

static char *(arg_msg[27]) = {
	"NULL",
	"SHORT INTEGER",
	"LONG INTEGER",
	"REAL",
	"DOUBLE",
	"COMPLEX",
	"LOGICAL",
	"CHARACTER",
	"POINTER",
	"TYPELESS",
	"CHARACTER POINTER",
	"LABEL",
	"SUBROUTINE",
	"NULL FUNCTION",
	"SHORT INTEGER FUNCTION",
	"LONG INTEGER FUNCTION",
	"REAL FUNCTION",
	"DOUBLE FUNCTION",
	"COMPLEX FUNCTION",
	"LOGICAL FUNCTION",
	"CHARACTER FUNCTION",
	"POINTER FUNCTION",
	"TYPELESS FUNCTION",
	"CHARACTER POINTER FUNCTION",
	"SUBPROGRAM",
	"DERIVED TYPE",
	"DERIVED TYPE FUNCTION" };

static int	_check_derived_type(long *, long *);

static int	_suppress_message(long);
static long	*_a_suppress_msg_list;
static long	*_d_suppress_msg_list;

#ifdef _UNICOS
#pragma _CRI duplicate _ARGCHCK as $ARGCHCK
#endif

static void	issue_msg();

int _ARGCHCK(long **argdes, long **dargdes)
{
	int	i, k, traced;
	char	name[MAX_ENT_LEN];
	char	callee[MAX_ENT_LEN];
	long 	lineno;
	arg_desc_header_type	*a_header;
	arg_desc_header_type	*d_header;
	arg_desc_node_type	*a_node;
	arg_desc_node_type	*d_node;
	int	arg_counts_match = 1;
	int	generic_call = 0;
	long	*long_ptr;

	traced		= 0;
	a_header	= (arg_desc_header_type *)argdes;
	d_header	= (arg_desc_header_type *)dargdes;

	_a_suppress_msg_list	= NULL;
	_d_suppress_msg_list	= NULL;

	argdes++;
	dargdes++;
	a_node		= (arg_desc_node_type *)*argdes;
	d_node		= (arg_desc_node_type *)*dargdes;

	if (a_header->f90_flag && d_header->f90_flag) {
		strcpy(callee, a_node->name);
		strcpy(name, d_node->name);
		lineno = a_node->line;
		traced = 1;
	}

	/* If sign bit set, we've seen this call before */

	if (a_header->seen_this == 0) {

	/* Set the sign bit of the first word so we don't do this call again. */

	a_header->seen_this = 1;

	if (a_header->f90_flag && a_header->suppress_msg) {
	   _a_suppress_msg_list	= (long *)argdes;
	   _a_suppress_msg_list += a_header->arg_count + 1;
	}

	if (d_header->f90_flag && d_header->suppress_msg) {
	   _d_suppress_msg_list	= (long *)dargdes;
	   _d_suppress_msg_list += d_header->arg_count + 1;
	}

	if (a_header->arg_count !=  d_header->arg_count) {

		/* counts do not match */
		issue_msg(FWNUMARG, a_header->arg_count,
			  d_header->arg_count, 0, &traced, callee, name,
			  &lineno);
		arg_counts_match	= 0;
	}

	if ((a_header->f90_flag && a_header->num_ck_only) ||
	    (d_header->f90_flag && d_header->num_ck_only)) {
			goto EXIT;
	}	

	/* check if call type matches */
	if (a_node->arg_type != d_node->arg_type) {
		issue_msg(FWFUNTYP, a_node->arg_type, d_node->arg_type,
			  0, &traced, callee, name, &lineno);
	} else if (a_header->f90_flag && d_header->f90_flag) {
		if (a_node->generic_call) {
			generic_call = 1;
		}

		/* other f90 checks on result information */
		if ((a_node->arg_type != Subroutine_Arg) &&
		    (a_node->arg_type != Subprogram_Arg)) {

			/* this is a function call */

			/* Check for a matching KIND type parameter */
			if (a_node->kind != d_node->kind) {
				issue_msg(FWFUNKTP, a_node->kind,
					  d_node->kind, 0, &traced,
					  callee, name, &lineno);
			}

			/* Check POINTER, RANK, and SIZE attributes */
			if (a_node->pointer && (!d_node->pointer)) {
				issue_msg(FWFUNNPT, 0, 0, 0, &traced,
					  callee, name, &lineno);
			} else if ((!a_node->pointer) &&
				     d_node->pointer) {
				issue_msg(FWFUNPTR, 0, 0, 0, &traced,
					  callee, name, &lineno);
			} else if (a_node->rank != d_node->rank) {
				issue_msg(FWFUNRNK, a_node->rank,
					  d_node->rank, 0, &traced,
					  callee, name, &lineno);
			} else if ((d_node->rank != 0 ) &&
				   (a_node->size != d_node->size)) {
				issue_msg(FWFUNSIZ, a_node->size,
					d_node->size, 0, &traced,
					callee, name, &lineno);
			}

			/* Check for matching character functions */
			if ((a_node->arg_type ==
			     Character_Function_Arg) &&
			    (a_node->char_len != d_node->char_len) &&
			    (!d_node->assumed_size_char)) {
				issue_msg(FWFUNCHL, a_node->char_len,
					  d_node->char_len, 0, &traced,
					  callee, name, &lineno);
				}

			/* Check for matching derived type functions */
			if (a_node->arg_type ==
			    Derived_Type_Function_Arg) {
				/* check derived type table */
				if (!_check_derived_type(
				     a_node->derived_type_tbl,
				     d_node->derived_type_tbl)) {
					issue_msg(FWFUNDVT, 0, 0, 0,
						  &traced, callee, name,
						  &lineno);
				}
			}
		}
	}

	if (arg_counts_match) {
		k	= a_header->arg_count;
		i	= 0;
		argdes++;
		dargdes++;
		a_node	= (arg_desc_node_type *)*argdes;
		d_node	= (arg_desc_node_type *)*dargdes;

		while (++i <= k) {

			if (a_node == NULL) {

				/* argument is not present, make sure
				 * the argument is optional.
				 */
				if ((!d_header->f90_flag) ||
				    (!d_node->optional)) {
					issue_msg(FWARGOPT, 0, 0,
						  i, &traced, callee,
						  name, &lineno);
				}
			} else if (a_node->pgm_unit && d_node->pgm_unit) {
				if (! a_node->pgm_unknown &&
				    ! d_node->pgm_unknown &&
				    a_node->arg_type != d_node->arg_type &&
				    ! d_node->ignore_tkr) {
					issue_msg(FWARGTYP,
						  a_node->arg_type,
						  d_node->arg_type, i,
						  &traced, callee, name,
						  &lineno);
				}
			} else if (a_node->arg_type != d_node->arg_type &&
				   ! d_node->ignore_tkr                 &&
				! (a_node->arg_type == Typeless_Arg &&
				  (d_node->arg_type == Short_Integer_Arg ||
				   d_node->arg_type == Long_Integer_Arg ||
				   d_node->arg_type == Real_Arg ||
				   d_node->arg_type == Double_Arg ||
				   d_node->arg_type == Complex_Arg ||
				   d_node->arg_type == Logical_Arg ||
				   d_node->arg_type == Character_Arg))) {

				if ((a_node->arg_type !=
				     Subprogram_Arg) &&
				   (d_node->arg_type != Subprogram_Arg)) {
					issue_msg(FWARGTYP,
						  a_node->arg_type,
						  d_node->arg_type, i,
						  &traced, callee, name,
						  &lineno);
				} else if ((a_node->arg_type ==
					    Subprogram_Arg) &&
					  (d_node->arg_type !=
					   Derived_Type_Function_Arg) &&
					  ((d_node->arg_type <
					    Subroutine_Arg) ||
					   (d_node->arg_type >
					    Subprogram_Arg))) {
					issue_msg(FWARGTYP,
						a_node->arg_type,
						d_node->arg_type,
						i, &traced, callee,
						name, &lineno);
				} else if ((d_node->arg_type ==
					    Subprogram_Arg) &&
					   (a_node->arg_type !=
					    Derived_Type_Function_Arg) &&
					   ((a_node->arg_type <
					     Subroutine_Arg) ||
					   (a_node->arg_type >
					     Subprogram_Arg))) {
					issue_msg(FWARGTYP,
						  a_node->arg_type,
						  d_node->arg_type,
						  i, &traced, callee,
						  name, &lineno);
				}
			} else if (a_header->f90_flag &&
				   d_header->f90_flag) {

				 /* do other f90 only checks. */
				
				if ((a_node->arg_type ==
				     Derived_Type_Arg) ||
				    (a_node->arg_type ==
				     Derived_Type_Function_Arg)) {

					/* check for derived type match */
					if (! d_node->ignore_tkr &&
					    !_check_derived_type(
						a_node->derived_type_tbl,
						d_node->derived_type_tbl)) {
						issue_msg(FWARGDVT, 0, 0,
						   i, &traced, callee,
						   name, &lineno);
					}
				} else if (a_node->arg_type != Typeless_Arg &&
					   a_node->kind != d_node->kind &&
					   ! d_node->ignore_tkr) {
					issue_msg(FWARGKTP, a_node->kind,
						  d_node->kind, i,
						  &traced, callee, name,
						  &lineno);
				}

				if (d_node->intent_out &&
				   (!a_node->defineable)) {
					issue_msg(FWARGOUT, 0, 0, i,
						   &traced, callee, name,
						   &lineno);
				}

				if (d_node->pointer) {
					if (!a_node->pointer) {
						issue_msg(FWARGPTR, 0, 0,
							 i, &traced,
							 callee, name,
							 &lineno);
				} else {
					if (a_node->rank !=
					    d_node->rank &&
					    ! d_node->ignore_tkr) {
						 issue_msg(FWARGRNK,
							  a_node->rank,
							  d_node->rank,
							  i, &traced,
							  callee, name,
							  &lineno);
					}
					if ((a_node->arg_type ==
					     Character_Arg) &&
					    ! d_node->ignore_tkr &&
					    (a_node->char_len !=
					     d_node->char_len)) {
						issue_msg(FWARGPCL,
							 a_node->char_len,
							 d_node->char_len,
							 i, &traced,
							 callee, name,
							 &lineno);
					}
				}
			} else if (d_node->assumed_shape) {
				if (!a_node->dope_vector) {
					issue_msg(FWARGASS, 0, 0, i,
						 &traced, callee, name,
						 &lineno);
				} else {
					if (a_node->rank !=
					    d_node->rank &&
					    ! d_node->ignore_tkr) {
						issue_msg(FWARGRNK,
							 a_node->rank,
							 d_node->rank,
							 i, &traced,
							 callee, name,
						 	 &lineno);
					}
					if ((a_node->arg_type ==
					     Character_Arg) &&
					    (!d_node->assumed_size_char) &&
					    (! d_node->ignore_tkr) &&
					    (a_node->char_len !=
					     d_node->char_len)) {
						issue_msg(FWARGPCL,
							 a_node->char_len,
							 d_node->char_len,
							 i, &traced,
							 callee, name,
						 	 &lineno);
					}
				}
			} else if ((d_node->rank == 0) &&
				   (a_node->rank != 0) &&
				   (! d_node->ignore_tkr)) {
					issue_msg(FWARGSCA, 0, 0, i,
						&traced, callee, name,
						&lineno);
			} else if (a_node->arg_type == Character_Arg &&
				   ! d_node->ignore_tkr) {
				/* must fit in total length (possible array) */

				if (d_node->rank == 0) {
					if (d_node->char_len >
					    a_node->char_len) {
						issue_msg(FWARGCHL,
							a_node->char_len,
							d_node->char_len,
							i, &traced,
							callee, name,
							&lineno);
					}
				} else if ((a_node->rank == 0) &&
					   (!a_node->array_element)) {
						issue_msg(FWARGARS, 0, 0,
							i, &traced,
							callee, name,
							&lineno);
				} else if ((!d_node->assumed_size_array) &&
					   (!a_node->assumed_size_array)) {
					if (d_node->size > a_node->size &&
					    ! a_node->array_element) {
						issue_msg(FWARGSIZ,
							a_node->size,
							d_node->size, i,
							&traced, callee,
							name, &lineno);
					}
				}
			} else if (d_node->rank > 0 &&
				   ! d_node->ignore_tkr) {
				if ((a_node->rank == 0) &&
				    (! a_node->array_element)) {
					issue_msg(FWARGARS, 0, 0, i,
						&traced, callee, name,
						 &lineno);
				} else if ((!d_node->assumed_size_array) &&
					  (!a_node->assumed_size_array) &&
					  (! a_node->array_element)  &&
					  (d_node->size > a_node->size)) {
					issue_msg(FWARGSIZ,
						a_node->size,
						d_node->size,
						i, &traced, callee,
						name, &lineno);
				}
			}
		}
		argdes++;
		dargdes++;
		a_node = (arg_desc_node_type *)*argdes;
		d_node = (arg_desc_node_type *)*dargdes;
		} /* while */
 	   } /* if (arg_counts_match) */
	}	/* if */

EXIT:
	return(0);
}


static void
issue_msg(which, nnum, dnum, indx, traced, nm1, nm2, lineno)
long	which, nnum, dnum, indx, *traced, *lineno;
char	*nm1, *nm2;
{
	int	l1, l2, temp;

	if (_suppress_message(which)) {
	   return;
	}

	if (*traced == 0) {

		*traced	= 1;

#if	defined(_UNICOS)
		l1	= _who_called_me(lineno, nm1, MAX_ENT_LEN, 3);
#else
		l1	= -1;
#endif

		if (l1 < 0) {		/* If there was an error */
			nm1[0]	= '?';
			nm1[1]	= '?';
			nm1[2]	= '?';
			l1	= 3;
		}

#if	defined(_UNICOS)
		l2	= _who_called_me(&temp, nm2, MAX_ENT_LEN, 2);
#else
		l2	= -1;
#endif

		if (l2 < 0) {		/* If there was an error */
			nm2[0]	= '?';
			nm2[1]	= '?';
			nm2[2]	= '?';
			l2	= 3;
		}

		/* Terminate names */

		nm1[l1]	= '\0';
		nm2[l2]	= '\0';

	} /* if traced == 0 */

	switch (which) {

		case FWNUMARG:
			(void) _fwarn(which, nm2, nm1, *lineno, nnum, dnum);
			break;

		case FWARGTYP:
			(void) _fwarn(which, nm2, nm1, *lineno, indx,
					arg_msg[nnum], arg_msg[dnum]);
			break;

		case FWFUNTYP:
			(void) _fwarn(which, nm2, nm1, *lineno, nm2,
					arg_msg[nnum], nm2, arg_msg[dnum]);
			break;

		case FWFUNKTP:
			(void) _fwarn(which, nm2, nm1, *lineno, nm2,
					nnum, nm2, dnum);
			break;

		case FWFUNRNK:
			(void) _fwarn(which, nm2, nm1, *lineno, nm2,
					nnum, nm2, dnum);
			break;

		case FWFUNSIZ:
			(void) _fwarn(which, nm2, nm1, *lineno, nm2,
					nnum, nm2, dnum);
			break;

		case FWFUNCHL:
			(void) _fwarn(which, nm2, nm1, *lineno, nm2,
					nnum, nm2, dnum);
			break;

		case FWFUNDVT:
			(void) _fwarn(which, nm2, nm1, *lineno);
			break;

		case FWFUNNPT:
			(void) _fwarn(which, nm2, nm1, *lineno, nm2);
			break;

		case FWFUNPTR:
			(void) _fwarn(which, nm2, nm1, *lineno, nm2);
			break;

		case FWARGOPT:
			(void) _fwarn(which, nm2, nm1, *lineno, indx);
			break;

		case FWARGKTP:
			(void) _fwarn(which, nm2, nm1, *lineno, indx,
					nnum, dnum);
			break;

		case FWARGDVT:
			(void) _fwarn(which, nm2, nm1, *lineno, indx);
			break;

		case FWARGOUT:
			(void) _fwarn(which, nm2, nm1, *lineno, indx);
			break;

		case FWARGPTR:
			(void) _fwarn(which, nm2, nm1, *lineno, indx);
			break;

		case FWARGRNK:
			(void) _fwarn(which, nm2, nm1, *lineno, indx,
					nnum, dnum);
			break;

		case FWARGPCL:
			(void) _fwarn(which, nm2, nm1, *lineno, indx,
					nnum, dnum);
			break;

		case FWARGASS:
			(void) _fwarn(which, nm2, nm1, *lineno, indx,
					indx);
			break;

		case FWARGSCA:
			(void) _fwarn(which, nm2, nm1, *lineno, indx,
					indx);
			break;

		case FWARGCHL:
			(void) _fwarn(which, nm2, nm1, *lineno, indx,
					nnum, dnum);
			break;

		case FWARGARS:
			(void) _fwarn(which, nm2, nm1, *lineno, indx,
					indx);
			break;

		case FWARGSIZ:
			(void) _fwarn(which, nm2, nm1, *lineno, indx,
					nnum, dnum);
			break;

		default:
			break;
	}

	return;
}

static int
_check_derived_type( long *a_struct,
		     long *d_struct)
{
	int	count;
	int	i;
	int	result = 1;

	if (a_struct[0] != d_struct[0])
		result = 0;
	else {
		count = a_struct[0];

		for (i = 1; i < count; i++) {
			if (a_struct[i] != d_struct[i]) {
				result = 0;
				break;
			}
		}
	}
	return(result);
}

static int
_suppress_message(long	message_number)

{
	int	i;
	int	result = 0;

	if (_a_suppress_msg_list != NULL) {
		i = 0;
		while (_a_suppress_msg_list[i] != 0) {
			if (_a_suppress_msg_list[i] == message_number) {
				result	= 1;
				break;
			}
			i++;
		}
	}

	if (result == 0 &&
	   _d_suppress_msg_list != NULL) {
		i = 0;
		while (_d_suppress_msg_list[i] != 0) {
			if (_d_suppress_msg_list[i] == message_number) {
				result	= 1;
				break;
			}
			i++;
		}
	}

	return(result);
}

