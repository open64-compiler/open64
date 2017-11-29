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


#pragma ident "@(#) libu/ffio/fssovfl.c	92.1	06/29/99 13:16:47"
#include <stdio.h>
#include <string.h>
#include <malloc.h>
#include <ffio.h>
#include "fssio.h"

static void num2str();

#define ERBLD(buf, str)		buf = strlen(buf) + strcpy(buf, str);
#define EWRITE(str)						\
				{				\
				(void)fflush(stdout);		\
				write(2, str, strlen(str));	\
				}

/*
 * _fss_overflow()
 *	Handle fss overflow
 */
int
_fss_overflow(
struct fdinfo	*fio,
struct ffsw	*stat)
{
	int ret;
	int size;
	char *name;
	char er_buf[256], szstr[16], *cp;
	struct mr_f *mr_info;
	struct sds_f *sds_info;
	struct ffc_info_s locinfo;
/*
 *	If overflow is not permitted, return error.
 */
	if (fio->subtype == FSS_OPT_NOVFL)
		ERETURN(stat, FDC_ERR_FSSOVF, 0);

/*
 *	Check to make sure that file is overflowable
 *	If underlying layer is stream, non-transforming,
 *	non-truncating, and can seek, then is OK.
 */
	XRCALL(fio->fioptr, fcntlrtn) fio->fioptr, FC_GETINFO, &locinfo, stat);
	if (!CAN_OVERFLOW(locinfo))
		ERETURN(stat, FDC_ERR_BADOVF, 0);
/*
 *	Issue message to standard error stating overflow happened.
 */
	cp = er_buf;
	if (fio->class == CLASS_SDS) {
		ERBLD(cp, "SDS");
		sds_info = (struct sds_f *)fio->lyr_info;
		name = sds_info->name;
		size = sds_info->sdssize >> 3;
		sds_info->overflowed = YES;
	}
	else if (fio->class == CLASS_MR) {
		ERBLD(cp, "MR");
		mr_info = (struct mr_f *)fio->lyr_info;
		name = mr_info->name;
		size = mr_info->mrsize >> 3;
		mr_info->overflowed = YES;
	}
	else
		ERETURN(stat, FDC_ERR_INTERR, 0);
		
	ERBLD(cp, " overflow at ");
	num2str(szstr, 15, size);
	ERBLD(cp, szstr);
	ERBLD(cp, " bytes on file: ");
	ERBLD(cp, name);
	ERBLD(cp, "\n");
	EWRITE(er_buf);

	return(1);	/* did overflow... */
}


static void
num2str(str, len, num)
char *str;
int len;
int num;
{
	unsigned a, i, ct;
	int pr, d;

	pr = 0;
	ct = 0;
	a = num;
	for (i = 100000000000; i != 1; i /= 10) {
		if ((pr |= (d = a / i)))
			*str++ = d + '0';
		a %= i;
		ct++;
		if (ct >= len) {
			*str++ = '*';
			*str = '\0';
			return;
		}
	}
	*str++ = a + '0';
	*str = '\0';
}
