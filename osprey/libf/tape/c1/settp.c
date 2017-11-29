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


static char USMID[] = "@(#) libf/tape/c1/settp.c	92.0	10/08/98 14:30:10";

#include <errno.h>
#include <ffio.h>
#include <liberrno.h>
#include "fio.h"

#define ERET(err)					\
			{				\
			*istat = err;			\
			goto done;			\
			}
		
/*
 *	Set tape position
 */

void
SETTP(
_f_int *unump,
_f_int *nbs,
_f_int8 *nb,
_f_int *nvs,
_f_int *nv,
_f_int8 *vi,
_f_int *synch,
_f_int *istat)
{
	register int	n;
	int ss;
	_f_int8 adjustnb;
	unit *cup;
	struct ffp_settp_s pos;
	_f_int unum;
#define NBS_SHFT ((sizeof(*nbs)-sizeof(char))*8)
#define NVS_SHFT ((sizeof(*nvs)-sizeof(char))*8)
	STMT_BEGIN(*unump, 0, T_TAPE, NULL, NULL, cup);
/*
 *	If not connected, do an implicit open. 
 */
	unum = *unump;
	if (cup == NULL) {
		cup = _implicit_open(SEQ, UNF, unum, 1, &ss);
		if (cup == NULL)
			ERET(ss);
	}

	if (*nb != 0 ) {
		if (*nb < 0 ){
			ERET(FETAPNBN);
		}
		if ((char)(*nbs>>NBS_SHFT) == '-' ) {
			if ( (*nv != 0 ) || (*vi != 0 )) {
				ERET(FETAPCMB);
			}
		} else if ((char)(*nbs>>NBS_SHFT) == '+' ) {
			if ( ( *nv != 0 ) || ( *vi != 0 )) {
				ERET(FETAPCMB);
			}
		} else if ( (char)(*nbs>>NBS_SHFT) != ' ' ) {
			ERET(FETAPNBS);
		}
	}

	if ( *nv != 0 ) {
		if ( *vi != 0 ){
			ERET(FETAPCMB);
		}
		if (*nv < 0) {
			ERET(FETAPNVY);
		}
		if ((char) (*nvs>>NVS_SHFT) == ' ')
			pos.ffp_nvs_p = FP_TPOS_ABS;
		else if ((char) (*nvs>>NVS_SHFT) == '+')
			pos.ffp_nvs_p = FP_TPOS_FORW;
		else if ((char) (*nvs>>NVS_SHFT) == '-')
			pos.ffp_nvs_p = FP_TPOS_BACK;
		else
			ERET(FETAPNVS);
	}
	else {
		pos.ffp_nvs_p = FP_TPOS_ABS;
	}
	adjustnb = *nb;
	if ((char)(*nbs>>NBS_SHFT) == ' ')
		pos.ffp_nbs_p = FP_TPOS_ABS;
	else if ((char) (*nbs>>NBS_SHFT) == '+')
		pos.ffp_nbs_p = FP_TPOS_FORW;
	else if ((char) (*nbs>>NBS_SHFT) == '-')
		pos.ffp_nbs_p = FP_TPOS_BACK;
	else if (*nb == 0)
		pos.ffp_nbs_p = FP_TPOS_ABS;
	else
			ERET(FETAPCMB);

	cup->uwrt = 0;

	*istat = 0;
	switch(cup->ufs) {
		case FS_FDC:
			if ((cup->uend == LOGICAL_ENDFILE) &&
			   (cup->uspcproc == 0)){
				if (*nb > 0 && 
				(char) (*nbs>>NBS_SHFT) == '-'){
					adjustnb--;
				}
			}
			cup->uend = BEFORE_ENDFILE;
			pos.ffp_nb = adjustnb;
			pos.ffp_vi = *vi;
			pos.ffp_nv = *nv;
			n = XRCALL(cup->ufp.fdc, posrtn) cup->ufp.fdc, FP_SETTP,
				&pos, 0, &cup->uffsw);
			if (n < 0)
				*istat = cup->uffsw.sw_error;
			break;
		default:
			*istat = FECONNTP;
	}

done:
	STMT_END(cup, T_TAPE, NULL, NULL);
	return;
}
