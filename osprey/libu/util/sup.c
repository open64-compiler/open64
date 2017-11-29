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


#pragma ident "@(#) libu/util/sup.c	92.1	07/07/99 13:18:33"

#include <fortran.h>
#include <cray/dopevec.h>

extern	short	_o_sup_flg_tab[DVTYPE_NTYPES];
extern	long	_o_sup_val_tab[DVTYPE_NTYPES];

#define	F_INDX	DVTYPE_REAL
#define	I_INDX	DVTYPE_INTEGER

void
FSUP(long *fvalue)
{
	_o_sup_val_tab[F_INDX]	= *fvalue;
	_o_sup_flg_tab[F_INDX]	= 1;

	return;
}

void
FSUPC(void)
{
	_o_sup_flg_tab[F_INDX]	= 0;

	return;
}

void
ISUP(long *ivalue)
{
	_o_sup_val_tab[I_INDX]	= *ivalue;
	_o_sup_flg_tab[I_INDX]	= 1;

	return;
}

void
ISUPC(void)
{
	_o_sup_flg_tab[I_INDX]	= 0;

	return;
}
