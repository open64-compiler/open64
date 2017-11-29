/*
 * Copyright 2004 PathScale, Inc.  All Rights Reserved.
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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#if !defined(__sparc__) && !defined(__mips)
# include <fortran.h>
#endif

#include "arith.h"

static int				pass = 0;
static int				fail = 0;

static AR_HOST_SINT64	result[4];

static void				check_ar_result();

static char				prevfname[8];
static int				prevflen;

#if defined(CRAY_TS_IEEE)
static AR_TYPE			INT_TYPE = AR_Int_64_S;
static AR_TYPE			FLOAT_64 = AR_Float_IEEE_NR_64;
static AR_TYPE			FLOAT_128 = AR_Float_IEEE_NR_128;
static AR_TYPE			COMPLEX_64 = AR_Complex_IEEE_NR_64;
static AR_TYPE			COMPLEX_128 = AR_Complex_IEEE_NR_128;
#elif _CRAY
static AR_TYPE			INT_TYPE = AR_Int_64_S;
static AR_TYPE			FLOAT_64 = AR_Float_Cray1_64;
static AR_TYPE			FLOAT_128 = AR_Float_Cray1_128;
static AR_TYPE			COMPLEX_64 = AR_Complex_Cray1_64;
static AR_TYPE			COMPLEX_128 = AR_Complex_Cray1_128;
#else
static AR_TYPE			INT_TYPE = AR_Int_64_S;
static AR_TYPE			FLOAT_64 = AR_Float_IEEE_NR_64;
static AR_TYPE			FLOAT_128 = AR_Float_IEEE_NR_128;
static AR_TYPE			COMPLEX_64 = AR_Complex_IEEE_NR_64;
static AR_TYPE			COMPLEX_128 = AR_Complex_IEEE_NR_128;
#endif


int main(int argc, char **argv)
{
	prevflen = 0;

#if defined(__sparc__) || defined(__mips)
	extern void test_native_();
	test_native_();
#else
	TEST_NATIVE();
#endif
	printf("Intrinsic test results:\n%6d passed\n%6d FAILED!!!\n",pass,fail);
	exit(fail);
}

#if defined(__sparc__) || defined(__mips)
void ar_strtod_(answer)
#else
void AR_STRTOD(answer)
#endif
double*	answer;
{
	char	num[32];

	double	dval;

	int		ierr;
	AR_TYPE	rtype;

	if(*answer >= 1.e6)
		sprintf(num,"%22.14e",*answer);
	else if(*answer >= 0.)
		sprintf(num,"%15.8f",*answer);
	else if(*answer >= -1.e6)
		sprintf(num,"%15.4f",*answer);
	else
		sprintf(num,"%25.16e",*answer);

	dval = strtod(num, 0);

	rtype = FLOAT_64;

	ierr = AR_convert_str_to_float((AR_DATA*)&result[0], &rtype, num);

	check_ar_result("STRTOD", strlen("STRTOD"), &result[0], ierr, &dval, 1);
}

#ifdef LD
#if defined(__sparc__) || defined(__mips)
void ar_strtold_(answer)
#else
void AR_STRTOLD(answer)
#endif
long double*	answer;
{
	char		num[33];

	long double	ldval;

	int			ierr;
	AR_TYPE		rtype;

#if defined(__sparc__) || defined(__mips)
	if(*answer >= 1.e6L)
		sprintf(num,"%30.22Le",*answer);
	else if(*answer >= 0.)
		sprintf(num,"%20.12Lf",*answer);
	else if(*answer >= -1.e6L)
		sprintf(num,"%22.10Lf",*answer);
	else
		sprintf(num,"%31.23Le",*answer);
	sscanf(num," %Lf", &ldval);
#else
	if(*answer >= 1.e6L)
		sprintf(num,"%30.22e",*answer);
	else if(*answer >= 0.)
		sprintf(num,"%20.12f",*answer);
	else if(*answer >= -1.e6L)
		sprintf(num,"%22.10f",*answer);
	else
		sprintf(num,"%31.23e",*answer);
	ldval = strtold(num, 0);
#endif

	rtype = FLOAT_128;

	ierr = AR_convert_str_to_float((AR_DATA*)&result[0], &rtype, num);

	check_ar_result("STRTOLD", strlen("STRTOLD"), &result[0], ierr, &ldval, 2);
}
#endif


#if defined(__sparc__) || defined(__mips)
void ar_intrin1_(func, opnd, answer, func_len)
char*				func;
AR_DATA*			opnd;
AR_DATA*			answer;
int					func_len;
#else
void AR_INTRIN1(func, opnd, answer)
_fcd				func;
AR_DATA*			opnd;
AR_DATA*			answer;
#endif
{
	int				ierr;
	int				n;
	AR_TYPE			rtype,otype,ptype;

#if defined(__sparc__) || defined(__mips)
	char			*fname = func;
	int				flen   = func_len;
#else
	char			*fname = _fcdtocp(func);
	int				flen   = _fcdlen(func);
#endif

	n = 1;

	if(strncmp(&fname[flen-3],"LOG",3) == 0) {
		if(fname[0] == 'A')
			rtype = FLOAT_64;
		else if(fname[0] == 'D') {
			rtype = FLOAT_128;
			n = 2;
		}
		else if(fname[1] == 'L') {
			rtype = COMPLEX_64;
			n = 2;
		}
		else {
			rtype = COMPLEX_128;
			n = 4;
		}
		ierr = AR_log((AR_DATA*)&result[0], &rtype, opnd, &rtype);
	}

	else if(strncmp(&fname[flen-3],"EXP",3) == 0) {
		if(fname[0] == 'E')
			rtype = FLOAT_64;
		else if(fname[0] == 'D') {
			rtype = FLOAT_128;
			n = 2;
		}
		else if(fname[1] == 'E') {
			rtype = COMPLEX_64;
			n = 2;
		}
		else {
			rtype = COMPLEX_128;
			n = 4;
		}
		ierr = AR_exp((AR_DATA*)&result[0], &rtype, opnd, &rtype);
	}

	else if(strncmp(&fname[flen-4],"SQRT",4) == 0) {
		if(fname[0] == 'S')
			rtype = FLOAT_64;
		else if(fname[0] == 'D') {
			rtype = FLOAT_128;
			n = 2;
		}
		else if(fname[1] == 'S') {
			rtype = COMPLEX_64;
			n = 2;
		}
		else {
			rtype = COMPLEX_128;
			n = 4;
		}
		ierr = AR_sqrt((AR_DATA*)&result[0], &rtype, opnd, &rtype);
	}
	
	else if(strncmp(&fname[flen-3],"ABS",3) == 0) {
		if(fname[1] == 'A') {
			rtype = FLOAT_64;
			otype = COMPLEX_64;
		}
		else {
			rtype = FLOAT_128;
			otype = COMPLEX_128;
			n = 2;
		}
		ierr = AR_cabs((AR_DATA*)&result[0], &rtype, opnd, &otype);
	}

	check_ar_result(fname, flen, &result[0], ierr, answer, n);
}

#if defined(__sparc__) || defined(__mips)
void ar_intrin2_(func, opnd1, opnd2, answer, func_len)
char*				func;
AR_DATA*			opnd1;
AR_DATA*			opnd2;
AR_DATA*			answer;
int					func_len;
#else
void AR_INTRIN2(func, opnd1, opnd2, answer)
_fcd				func;
AR_DATA*			opnd1;
AR_DATA*			opnd2;
AR_DATA*			answer;
#endif
{
	int				ierr;
	int				n;
	AR_TYPE			rtype,otype,ptype;
	AR_HOST_SINT64	base,power;

#if defined(__sparc__) || defined(__mips)
	char			*fname = func;
	int				flen   = func_len;
#else
	char			*fname = _fcdtocp(func);
	int				flen   = _fcdlen(func);
#endif

	n = 1;

	base = power = 0;
	if(strncmp(&fname[flen-3],"TOI",3)==0 ||
	   strncmp(&fname[flen-3],"TOR",3)==0) {
		if(fname[0] == 'I') {
#if _CRAY
			rtype = AR_Int_64_S;
			otype = AR_Int_64_S;
#else
			memcpy(((char*)&base)+4, ((char*)opnd1)+4, 4);
			memcpy(((char*)&power)+4, ((char*)opnd2)+4, 4);
			if((base>>31)  != 0 || (power>>31) != 0) {
				if((base>>31)  != 0)
					memset((char*)opnd1, 0xff, 4);
				if((power>>31) != 0)
					memset((char*)opnd2, 0xff, 4);
				rtype = AR_Int_64_S;
				otype = AR_Int_64_S;
			}
			else {
				rtype = AR_Int_32_S;
				otype = AR_Int_32_S;
			}
#endif
		}
		else if(fname[0] == 'R') {
			rtype = FLOAT_64;
			otype = FLOAT_64;
		}
		else if(fname[0] == 'D') {
			rtype = FLOAT_128;
			otype = FLOAT_128;
			n = 2;
		}
		else if(fname[1] == 'T') {
			rtype = COMPLEX_64;
			otype = COMPLEX_64;
			n = 2;
		}
		else {
			rtype = COMPLEX_128;
			otype = COMPLEX_128;
			n = 4;
		}
		if(fname[flen-1] == 'I') {
#if _CRAY
			ptype = AR_Int_64_S;
#else
			memcpy(((char*)&power)+4, ((char*)opnd2)+4, 4);
			if(otype == AR_Int_64_S || (power>>31) != 0) {
				if((power>>31) != 0)
					memset((char*)opnd2, 0xff, 4);
				ptype = AR_Int_64_S;
			}
			else
				ptype = AR_Int_32_S;
#endif
		}
		else
			ptype = FLOAT_64;
		ierr = AR_power((AR_DATA*)&result[0], &rtype, opnd1, &otype, opnd2,
						&ptype);
		if(base != 0) {
			memset((char*)opnd1, 0, 4);
			if(rtype == AR_Int_64_S && (power&1))
				memset((char*)&result[0], 0, 4);
		}
		if(power != 0)
			memset((char*)opnd2, 0, 4);
	}
	else {
		if(fname[0] == 'D') {
			rtype = FLOAT_128;
			otype = FLOAT_128;
			ptype = FLOAT_128;
			n = 2;
		}
		else if(fname[1] == 'T') {
			rtype = COMPLEX_64;
			otype = COMPLEX_64;
			ptype = COMPLEX_64;
			n = 2;
		}
		else {
			rtype = COMPLEX_128;
			otype = COMPLEX_128;
			ptype = COMPLEX_128;
			n = 4;
		}
		ierr = AR_power((AR_DATA*)&result[0], &rtype, opnd1, &otype, opnd2,
						&ptype);
	}
	
	check_ar_result(fname, flen, &result[0], ierr, answer, n);
}

static
void
check_ar_result(fname, flen, ar_result, ar_error, answer, rsize)
char			*fname;
int				flen;
AR_HOST_SINT64	*ar_result;
int				ar_error;
AR_HOST_SINT64	*answer;
int				rsize;
{
	int				i;
	int				ierr;
	AR_HOST_SINT64	xor;

	if(prevflen != flen && strncmp(prevfname, fname, flen) != 0) {
		prevflen = flen;
		strncpy(prevfname, fname, flen);
		printf("Testing %*.*s intrinsic\n", flen, flen, fname);
	}
	
	ierr = ar_error&(AR_STAT_OVERFLOW|AR_STAT_UNDEFINED|AR_STAT_INVALID_TYPE);
	
	for(xor=0, i=0; i<rsize; i++)
		xor |= (ar_result[i]^answer[i]);
	
	if((ierr & (AR_STAT_OVERFLOW|AR_STAT_UNDEFINED)) &&
	   ((answer[0]>>52)&0x7ff) == 0x7ff) ierr=0;
	
	if(ierr!=0 || xor!=0) {
		fprintf(stderr,
				"\n***** ERROR *** ERROR *** ERROR *** ERROR *****\n");
		fprintf(stderr,
				"   arith.a %*.*s result does not match expected result of",
				flen, flen, fname);
		for(i=0; i<rsize; i++)
			fprintf(stderr," %16.16llx",answer[i]);
		fprintf(stderr,"\n");
		if(ierr != 0)
			fprintf(stderr,
					"   The arith.a routine returned an error code = 0%o\n",
					ierr);
		else {
			fprintf(stderr,"   The arith.a routine returned a result of");
			for(i=0; i<rsize; i++)
				fprintf(stderr," %16.16llx",ar_result[i]);
			fprintf(stderr,"\n");
		}
		fail++;
	}
	else
		pass++;
	
}


static char USMID [] = "\n%Z%%M%	%I%	%G% %U%\n";
static char rcsid [] = "$Id: test_ar_intrin.c,v 1.1.1.1 2005/10/21 19:00:00 marcel Exp $";
