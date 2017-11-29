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
#include <fcntl.h>
#include <errno.h>
#include <sys/types.h>
#include <unistd.h>

#include <ar.h>
#if !defined(AR_HDR_SIZE)
  /*
   * Solaris doesn't define AR_HDR_SIZE.
   */
# define AR_HDR_SIZE sizeof(struct ar_hdr)
#endif

/************************ Cray MPP Intrinsic Simulator *********************/

/* Simulate MPP library intrinsics if this module is loaded */

#include "arith.internal.h"
#include "int64.h"

#if !defined(__mips)

int ar_rounding_modes = 1<<AR_ROUND_NEAREST;
int ar_underflow_modes = 1<<AR_UNDERFLOW_TO_PLUS_ZERO;

extern char AR_libmv2[];
extern char AR_version[];

#define IEEE_FLOAT_32		(UNROUNDED_TYPE(AR_Float_IEEE_NR_32))
#define IEEE_FLOAT_64		(UNROUNDED_TYPE(AR_Float_IEEE_NR_64))
#define IEEE_FLOAT_128		(UNROUNDED_TYPE(AR_Float_IEEE_NR_128))
#define IEEE_COMPLEX_32		(UNROUNDED_TYPE(AR_Complex_IEEE_NR_32))
#define IEEE_COMPLEX_64		(UNROUNDED_TYPE(AR_Complex_IEEE_NR_64))
#define IEEE_COMPLEX_128	(UNROUNDED_TYPE(AR_Complex_IEEE_NR_128))

#define T3D		10				/* T3D primary machine type */
#define T3E		12				/* T3E primary machine type */

static int ar_sim_version = 0;	/* Version of interface with data file */
static int ar_mach_type = T3D;	/* PMT of arith data file */

static AR_TYPE integer_64		= AR_Int_64_S;
static AR_TYPE ieee_float_32	= (AR_TYPE) IEEE_FLOAT_32;
static AR_TYPE ieee_float_64	= (AR_TYPE) IEEE_FLOAT_64;

#define	NOINTRIN	1	/* Unknown/unsupported intrinsic name */
#define ARITHERR	2	/* Arithmetic error detected by intrinsic */
#define EXTERROR	3	/* Unsupported external called by intrinsic */
#define SETERRNO	4	/* Set errno = ERANGE */
#define IFACEERR	5	/* Unsupported simulation interface */
#define HOSTEXT		6	/* Call host system external */
#define BADINSTR	7	/* Unsupported instructions in intrinsic */

#define MPP_STACK_SIZE	2048	/* # words to allocate for stack/heap space */
#define MPP_DSIB_SIZE	6		/* # words to allocate for DSIB space */
#define MPP_MAX_ARGS	8		/* Max number args allowed to any routine */

static unsigned char*	mpp_intrin=NULL;
static unsigned char*	code=NULL;
static unsigned char*	data=NULL;
static unsigned char*	stack=NULL;

static int D_roundmode = -1;	/* Dynamic rounding mode */

static AR_TYPE	D_float_32;
static AR_TYPE	D_float_64;
static AR_TYPE	D_float_128;
static AR_TYPE	D_complex_64;
static AR_TYPE	D_complex_128;

/* Simulated Alpha integer and floating registers */

static AR_INT_64	R[32];		/* Integer registers */
static AR_INT_64	F[32];		/* Floating registers */

#ifdef DEBUG

/* Disassembly variables */

int	ar_disasm = 0;		/* Turns on disassembly for debugging */

static char RN[32][5] = {
			"v0",		/* r0 */	/* return value */
			"t0",		/* r1 */	/* caller saved */
			"t1",		/* r2 */
			"t2",		/* r3 */
			"t3",		/* r4 */
			"t4",		/* r5 */
			"t5",		/* r6 */
			"t6",		/* r7 */
			"t7",		/* r8 */
			"s0",		/* r9 */	/* callee saved */
			"s1",		/* r10 */
			"s2",		/* r11 */
			"s3",		/* r12 */
			"s4",		/* r13 */
			"s5",		/* r14 */
			"fp",		/* r15 */	/* frame pointer */
			"a0",		/* r16 */	/* argument registers */
			"a1",		/* r17 */
			"a2",		/* r18 */
			"a3",		/* r19 */
			"a4",		/* r20 */
			"a5",		/* r21 */
			"t8",		/* r22 */	/* caller saved */
			"t9",		/* r23 */
			"t10",		/* r24 */
			"ci",		/* r25 */	/* call info register */
			"ra",		/* r26 */	/* return address */
			"t11",		/* r27 */	/* caller saved */
			"t12",		/* r28 */
			"t13",		/* r29 */
			"sp",		/* r30 */	/* stack pointer */
			"R31"		/* r31 */	/* wired to zero */
};

static char FN[32][6] = {
			"fv0",		/* f0 */	/* return value - FP */
			"fv1",		/* f1 */	/* Complex FP ret val */
			"fs0",		/* f2 */	/* callee saved */
			"fs1",		/* f3 */
			"fs2",		/* f4 */
			"fs3",		/* f5 */
			"fs4",		/* f6 */
			"fs5",		/* f7 */
			"fs6",		/* f8 */
			"fs7",		/* f9 */
			"ft0",		/* f10 */	/* caller saved */
			"ft1",		/* f11 */
			"ft2",		/* f12 */
			"ft3",		/* f13 */
			"ft4",		/* f14 */
			"ft5",		/* f15 */
			"fa0",		/* f16 */	/* argument registers */
			"fa1",		/* f17 */
			"fa2",		/* f18 */
			"fa3",		/* f19 */
			"fa4",		/* f20 */
			"fa5",		/* f21 */
			"ft6",		/* f22 */	/* caller saved */
			"ft7",		/* f23 */
			"ft8",		/* f24 */
			"ft9",		/* f25 */
			"ft10",		/* f26 */
			"ft11",		/* f27 */
			"ft12",		/* f28 */
			"ft13",		/* f29 */
			"ft14",		/* f30 */
			"F31"		/* f31 */	/* wired to fp zero */
};

#define DISASM0(fmt)		if(ar_disasm) fprintf(stderr,fmt)
#define DISASM1(fmt,x)		if(ar_disasm) fprintf(stderr,fmt,x)
#define DISASM2(fmt,x,y)	if(ar_disasm) fprintf(stderr,fmt,x,y)
#define DISASM3(fmt,x,y,z)	if(ar_disasm) fprintf(stderr,fmt,x,y,z)
#define DISASM4(fmt,x,y,z,v)	if(ar_disasm) fprintf(stderr,fmt,x,y,z,v)
#define DISASM5(fmt,x,y,z,v,w)	if(ar_disasm) fprintf(stderr,fmt,x,y,z,v,w)

#else

#define DISASM0(fmt)
#define DISASM1(fmt,x)
#define DISASM2(fmt,x,y)
#define DISASM3(fmt,x,y,z)
#define DISASM4(fmt,x,y,z,v)
#define DISASM5(fmt,x,y,z,v,w)

#endif

/* Define register names used in simulation interfaces */

#define v0	0
#define t1	2
#define s0	9
#define fp	15
#define a0	16
#define a1	17
#define a2	18
#define a3	19
#define a4	20
#define a5	21
#define ci	25
#define ret	26
#define sp	30
#define	zero	31

#define fv0	0
#define fv1	1
#define fa0	16
#define	fzero	31

#define MAX_EXT_ADDRS	16		/* Max # of EXT addresses passed */

static AR_HOST_SINT64	numargs = 0;

static int	n_external_addresses = 1;	/* Must be > 0 to be non-NULL */

/*
 * Segment identifiers for the mpp.  They are set in open_intrinsics_file.
 */
static unsigned code_segment_id;
static unsigned	data_segment_id;
static unsigned stack_segment_id;

/*
 * Segment types as defined in the aout_mpp.h include file.
 */
typedef enum {
        SEGT_DATA = 0,
        SEGT_TEXT = 1,          /* Only one segment can have this type  */
        SEGT_REGISTERS = 2,
        SEGT_STACK = 3,         /* Only one segment can have this type  */
        SEGT_EREGISTERS = 4,
        SEGT_BESUS = 5
} SegTypes;
 
static char*	external_address[MAX_EXT_ADDRS];
static long	external_length[MAX_EXT_ADDRS];

typedef union {
	AR_INT_64 ari64;
        long l64;
} i64_union;


/*
 * Local routines.
 */
static int  call_host_external(char* func);
static void	open_intrinsics_file(void);
static void	load_mpp_word(AR_INT_64 *result, long segment, long offset);
static void	store_mpp_word(long segment, long offset, AR_INT_64 *result);


int
ar_clear_sim_state(AR_TYPE resulttype)
{

#ifdef DEBUG
	if (getenv("AR_DISASM") != NULL) {
		ar_disasm = 1;
	}
#endif

	if(mpp_intrin == NULL)
		open_intrinsics_file();

	if(AR_CLASS(resulttype) == AR_CLASS_FLOAT &&
	   AR_FLOAT_FORMAT(resulttype) == AR_FLOAT_CRAY)
		ar_internal_error(2018, __FILE__, __LINE__);

	numargs = 0;				/* Clear # arguments counter */
	ZERO64(R[ci]);				/* Clear call info reg for next time */
	n_external_addresses = 1;	/* Forget all external addresses */

	D_roundmode   = ar_float_ieee_round_nearest;	/* Default rounding mode */
	D_float_32    = (AR_TYPE) IEEE_FLOAT_32;
	D_float_64    = (AR_TYPE) IEEE_FLOAT_64;
	D_float_128   = (AR_TYPE) IEEE_FLOAT_128;
	D_complex_64  = (AR_TYPE) IEEE_COMPLEX_64;
	D_complex_128 = (AR_TYPE) IEEE_COMPLEX_128;

	return AR_STAT_OK;
}

int
ar_ext_address(AR_INT_64* intaddr, const char* extaddr, int length)
{
	if(n_external_addresses == code_segment_id)
		external_address[n_external_addresses++] = NULL;

	if(n_external_addresses == data_segment_id)
		external_address[n_external_addresses++] = NULL;

	if(n_external_addresses == stack_segment_id)
		external_address[n_external_addresses++] = NULL;

	intaddr[0].part1 = intaddr[0].part3 = intaddr[0].part4 = 0;
	intaddr[0].part2 = n_external_addresses;

	if(n_external_addresses >= MAX_EXT_ADDRS)
		return AR_STAT_UNDEFINED;

	external_address[n_external_addresses] = (char*)extaddr;
	external_length[n_external_addresses++] = length;

	return AR_STAT_OK;
}

int
ar_pass_arg_address(const ar_data* arg, const AR_TYPE* argtype)
{
	int	iarg;
	int	arg_offset;
	int	arg_value_offset;

	numargs++;
	iarg = R[ci].part4>>8;		/* Current R[ci] has arg word index */
	R[ci].part4 += (1<<8);		/* Increment number of arguments */

	if(iarg >= MPP_MAX_ARGS)	/* Test for unsupported # of args */
		ar_internal_error(2014, __FILE__, __LINE__);

	/* Call-by-address must 1) copy the value of the argument into
	 * the address space known to the simulator and 2) store the address
	 * of this copied value into the argument list.  5*MPP_MAX_ARGS words
	 * are assumed to exist beyond the base of the stack for this purpose.
	 * These words emulate the argument setup at the top of a caller's
	 * stack frame on an MPP system.  The first MPP_MAX_ARGS are used to
	 * store arguments that are not passed in registers.  Then 4 words are
	 * available to store the value of each argument.
	 */

	/*
	 * Handle special case of a NULL pointer (no value).  If not NULL,
	 * Compute address of argument value (to be copied in switch below).
	 */

	if(arg == NULL)
		ZERO64(R[s0]);
	else {
		arg_value_offset = (MPP_STACK_SIZE + MPP_MAX_ARGS + (iarg * 4)) * 8;
		R[s0].part1 = 0;
		R[s0].part2 = stack_segment_id;
		R[s0].part3 = (arg_value_offset>>16);
		R[s0].part4 = arg_value_offset&0xffff;
	}

	/*
	 * If argument 1 through 6, store address into argument register.
	 * Otherwise, store it in the stack extension of the argument list.
	 */
	
	if(iarg < 6) {
		memcpy(&R[a0+iarg], &R[s0], 8);
		R[ci].part4++;		/* Increment # of register args */
	}
	else {
		arg_offset = MPP_STACK_SIZE + iarg;
		store_mpp_word(stack_segment_id, arg_offset, &R[s0]);
	}

	/* Define rounding for instructions with dynamic (/D) rounding mode */

	if(AR_CLASS(*argtype) == AR_CLASS_FLOAT) {
		D_roundmode   = ROUND_MODE(*argtype);
		D_float_32    = (AR_TYPE) (IEEE_FLOAT_32    | (D_roundmode<<3));
		D_float_64    = (AR_TYPE) (IEEE_FLOAT_64    | (D_roundmode<<3));
		D_float_128   = (AR_TYPE) (IEEE_FLOAT_128   | (D_roundmode<<3));
		D_complex_64  = (AR_TYPE) (IEEE_COMPLEX_64  | (D_roundmode<<3));
		D_complex_128 = (AR_TYPE) (IEEE_COMPLEX_128 | (D_roundmode<<3));
	}

	if(arg == NULL)			/* If no value, simply return */
		return AR_STAT_OK;

	/* Copy the value of the argument to stack slot reserved for it */

	switch (UNROUNDED_TYPE(*argtype)) {

	case IEEE_FLOAT_32:
		memcpy(&stack[arg_value_offset], (char*)(arg)+4, 4);
		break;

	case IEEE_FLOAT_64:
	case IEEE_COMPLEX_32:
		memcpy(&stack[arg_value_offset], (char*)(arg), 8);
		break;

	case IEEE_FLOAT_128:
	case IEEE_COMPLEX_64:
		memcpy(&stack[arg_value_offset], (char*)(arg), 16);
		break;


	case AR_Int_32_S:
		memcpy(&stack[arg_value_offset], (char*)(arg)+4, 4);
		break;

	case IEEE_COMPLEX_128:
		memcpy(&stack[arg_value_offset], (char*)(arg), 32);
		break;

	default:
		switch (*argtype) {

		case AR_Int_32_S:
			memcpy(&stack[arg_value_offset], (char*)(arg)+4, 4);
			break;

		case AR_Logical:
			if(arg->ar_i64.part1 || arg->ar_i64.part2 ||
			   arg->ar_i64.part3 || arg->ar_i64.part4)
				memcpy(&stack[arg_value_offset], (char*)&AR_const_true, 8);
			else
				memcpy(&stack[arg_value_offset], (char*)&AR_const_false, 8);
			break;

		case AR_Int_46_S:
		case AR_Int_64_S:
			memcpy(&stack[arg_value_offset], (char*)(arg), 8);
			break;

		default:
			return AR_STAT_INVALID_TYPE;
		}
	}

	return AR_STAT_OK;
}

int
ar_pass_ext_address(AR_INT_64 *extdesc, const char *addr, int nwords)
{
	int	iarg;
	int	arg_offset;
	int	arg_value_offset;

	int	status;

	numargs++;
	iarg = R[ci].part4>>8;		/* Current R[ci] has arg word index */
	R[ci].part4 += (1<<8);		/* Increment number of arguments */

	if(iarg >= MPP_MAX_ARGS)	/* Test for unsupported # of args */
		ar_internal_error(2014, __FILE__, __LINE__);

	/* Passing an external address must 1) obtain an external address
	 * descriptor for the address (from ar_ext_address) and 2) store this
	 * descriptor into the argument list.
	 */

	/*
	 * Handle special case of a NULL pointer (no value).  If not NULL,
	 * Compute address of argument value (to be copied in switch below).
	 */

	if(addr == NULL)
		ZERO64(R[s0]);
	else {
		status = ar_ext_address(&R[s0], addr, nwords);
		if(IS_ERROR_STATUS(status))
			return status;
	}

	/*
	 * If argument 1 through 6, store address into argument register.
	 * Otherwise, store it in the stack extension of the argument list.
	 */
	
	if(iarg < 6) {
		memcpy(&R[a0+iarg], &R[s0], 8);
		R[ci].part4++;		/* Increment # of register args */
	}
	else {
		arg_offset = MPP_STACK_SIZE + iarg;
		store_mpp_word(stack_segment_id, arg_offset, &R[s0]);
	}

	if(extdesc != NULL) {
		R[s0].part4 = 3;	/* Pass back word-to-addr shift count */
		memcpy(extdesc, &R[s0], 8);
	}

	return AR_STAT_OK;
}

int
ar_pass_fcd_address(const char* str, long lenstr)
{
	int	iarg;
	int	arg_offset;
	int	arg_value_offset;

	int	status;

	long	nbits;

	numargs++;
	iarg = R[ci].part4>>8;		/* Current R[ci] has arg word index */
	R[ci].part4 += (2<<8);		/* Increment number of arg words */

	if(iarg >= MPP_MAX_ARGS)	/* Test for unsupported # of args */
		ar_internal_error(2014, __FILE__, __LINE__);

	if(str == NULL)
		return AR_STAT_UNDEFINED;

	/* Get external address descriptor and store in first fcd word */
	nbits = lenstr<<3;
	status = ar_ext_address(&R[s0], str, (nbits+63)>>6);
	if(IS_ERROR_STATUS(status))
		return status;

	/* If argument 1 through 6, store 1st fcd word into argument register.
	 * Otherwise, store it in the stack extension of the argument list.
	 */
	
	if(iarg < 6) {
		memcpy(&R[a0+iarg], &R[s0], 8);
		R[ci].part4++;		/* Increment # of register args */
	}
	else {
		arg_offset = MPP_STACK_SIZE + iarg;
		store_mpp_word(stack_segment_id, arg_offset, &R[s0]);
	}

	/* Store length in second fcd word */
	R[s0].part1 = R[s0].part2 = 0;
	if(ar_mach_type == T3D) {
		R[s0].part3 = nbits>>16;		/* Length in bits */
		R[s0].part4 = nbits&0xffff;
	}
	else {
		R[s0].part3 = lenstr>>16;		/* Length in bytes */
		R[s0].part4 = lenstr&0xffff;
	}

	/* If argument 1 through 6, store 2nd fcd word into argument register.
	 * Otherwise, store it in the stack extension of the argument list.
	 */
	
	if((iarg+1) < 6) {
		memcpy(&R[a0+(iarg+1)], &R[s0], 8);
		R[ci].part4++;		/* Increment # of register args */
	}
	else {
		arg_offset = MPP_STACK_SIZE + (iarg + 1);
		store_mpp_word(stack_segment_id, arg_offset, &R[s0]);
	}

	return AR_STAT_OK;
}

int
ar_pass_arg_value(const ar_data* arg, const AR_TYPE* argtype)
{
	int		i;
	int		iarg;
	int		arg_offset;
	AR_IEEE_32	i32;		/* Only used for IEEE_COMPLEX_32 */
	int		status;

	numargs++;
	iarg = R[ci].part4>>8;		/* Current R[ci] has arg word index */
	R[ci].part4 += (1<<8);		/* Increment number of arg words */

	if(iarg >= MPP_MAX_ARGS)	/* Test for unsupported # of args */
		ar_internal_error(2014, __FILE__, __LINE__);

	/* Call-by-value must copy the value of the argument into either an
	 * argument register or into the stack extension of the argument list.
	 * The first MPP_MAX_ARGS words beyond the stack base are used as the
	 * extension of the argument list.  These words emulate the argument
	 * setup at the top of an MPP caller's stack frame.
	 */

	if(AR_CLASS(*argtype) == AR_CLASS_FLOAT) {
		D_roundmode   = ROUND_MODE(*argtype);
		D_float_32    = (AR_TYPE) (IEEE_FLOAT_32    | (D_roundmode<<3));
		D_float_64    = (AR_TYPE) (IEEE_FLOAT_64    | (D_roundmode<<3));
		D_float_128   = (AR_TYPE) (IEEE_FLOAT_128   | (D_roundmode<<3));
		D_complex_64  = (AR_TYPE) (IEEE_COMPLEX_64  | (D_roundmode<<3));
		D_complex_128 = (AR_TYPE) (IEEE_COMPLEX_128 | (D_roundmode<<3));
	}

	if(arg == NULL)			/* Test for NULL pointer (no value) */
		return AR_STAT_UNDEFINED;

	/*
	 * If argument 1 through 6, store value into argument register.
	 * Otherwise, store it in the stack extension of the argument list.
	 */

	switch (UNROUNDED_TYPE(*argtype)) {

	case IEEE_FLOAT_32:
		if(iarg >= 6) {
			arg_offset = (MPP_STACK_SIZE + iarg) * 8;
			return ar_convert_to_float((ar_data*)&stack[arg_offset],
									   &ieee_float_64,
									   arg, &ieee_float_32);
		}
		if(iarg == 5)
			R[ci].part4 |= 7;
		else
			R[ci].part4 = (R[ci].part4|(1<<(7-iarg))) + 1;
		return ar_convert_to_float((ar_data*)&F[fa0+iarg],
								   &ieee_float_64,
								   arg, &ieee_float_32);

	case IEEE_FLOAT_64:
		if(iarg >= 6) {
			arg_offset = (MPP_STACK_SIZE + iarg) * 8;
			memcpy(&stack[arg_offset], arg, 8);
			return AR_STAT_OK;
		}
		if(iarg == 5)
			R[ci].part4 |= 7;
		else
			R[ci].part4 = (R[ci].part4|(1<<(7-iarg))) + 1;
		memcpy(&F[fa0+iarg], arg, 8);
		return AR_STAT_OK;

	case IEEE_FLOAT_128:
	case IEEE_COMPLEX_64:
		R[ci].part4 += (1<<8);	/* Increment number of arg words */
		for(i=0; i<2; i++) {
			if((iarg+i) >= 6) {
				if((iarg+i) >= MPP_MAX_ARGS)
				    ar_internal_error(2014, __FILE__, __LINE__);
				arg_offset = (MPP_STACK_SIZE + (iarg + i)) * 8;
				memcpy(&stack[arg_offset], (AR_INT_64*)arg+i, 8);
			}
			else {
				if((iarg+i) == 5)
					R[ci].part4 |= 7;
				else
					R[ci].part4 = (R[ci].part4|(1<<(7-(iarg+i)))) + 1;
				memcpy(&F[fa0+(iarg+i)], (AR_INT_64*)arg+i, 8);
			}
		}
		return AR_STAT_OK;

	case IEEE_COMPLEX_32:
		R[ci].part4 += (1<<8);	/* Increment number of arg words */
		status = AR_STAT_OK;
		if(iarg >= (MPP_MAX_ARGS - 1))
			ar_internal_error(2014, __FILE__, __LINE__);

		i32.sign   = arg->ar_cplx_ieee32.rsign;
		i32.expo   = arg->ar_cplx_ieee32.rexpo;
		i32.coeff0 = arg->ar_cplx_ieee32.rcoeff0;
		i32.coeff1 = arg->ar_cplx_ieee32.rcoeff1;
		if(iarg >= 6) {
			arg_offset = (MPP_STACK_SIZE + iarg) * 8;
			status |= ar_convert_to_float((ar_data*)&stack[arg_offset],
										  &ieee_float_64,
										  &i32, &ieee_float_32);
		}
		else {
			if(iarg == 5)
				R[ci].part4 |= 7;
			else
				R[ci].part4 = (R[ci].part4|(1<<(7-iarg))) + 1;
			status |= ar_convert_to_float((ar_data*)&F[fa0+iarg],
										  &ieee_float_64,
										  &i32, &ieee_float_32);
		}

		i32.sign   = arg->ar_cplx_ieee32.isign;
		i32.expo   = arg->ar_cplx_ieee32.iexpo;
		i32.coeff0 = arg->ar_cplx_ieee32.icoeff0;
		i32.coeff1 = arg->ar_cplx_ieee32.icoeff1;
		iarg++;
		if(iarg >= 6) {
			if(iarg >= MPP_MAX_ARGS)
			    ar_internal_error(2014, __FILE__, __LINE__);
			arg_offset = (MPP_STACK_SIZE + iarg) * 8;
			status |= ar_convert_to_float((ar_data*)&stack[arg_offset],
										  &ieee_float_64,
										  &i32, &ieee_float_32);
		}
		else {
			if(iarg == 5)
				R[ci].part4 |= 7;
			else
				R[ci].part4 = (R[ci].part4|(1<<(7-iarg))) + 1;
			status |= ar_convert_to_float((ar_data*)&F[fa0+iarg],
										  &ieee_float_64,
										  &i32, &ieee_float_32);
		}

		return status;

	case IEEE_COMPLEX_128:
		R[ci].part4 += (1<<8);	/* Increment number of arg words */
		for(i=0; i<4; i++) {
			if((iarg+i) >= 6) {
				if((iarg+i) >= MPP_MAX_ARGS)
				    ar_internal_error(2014, __FILE__, __LINE__);
				arg_offset = (MPP_STACK_SIZE + (iarg + i)) * 8;
				memcpy(&stack[arg_offset], (AR_INT_64*)arg+i, 8);
			}
			else {
				if((iarg+i) == 5)
					R[ci].part4 |= 7;
				else
					R[ci].part4 = (R[ci].part4|(1<<(7-(iarg+i)))) + 1;
				memcpy(&F[fa0+(iarg+i)], (AR_INT_64*)arg+i, 8);
			}
		}
		return AR_STAT_OK;

	default:
		switch (*argtype) {

		case AR_Int_32_S:
		case AR_Int_46_S:
		case AR_Int_64_S:

			if(iarg >= 6) {
				arg_offset = (MPP_STACK_SIZE + iarg) * 8;
				return ar_convert_to_integral((ar_data*)&stack[arg_offset],
											  &integer_64,
											  arg, argtype);
			}
			R[ci].part4++;
			return ar_convert_to_integral((ar_data*)&R[a0+iarg],
										  &integer_64,
										  arg, argtype);

		case AR_Logical:
			if(arg->ar_i64.part1 || arg->ar_i64.part2 ||
			   arg->ar_i64.part3 || arg->ar_i64.part4)
				if(iarg >= 6) {
					arg_offset = (MPP_STACK_SIZE + iarg) * 8;
					memcpy(&stack[arg_offset],
					(char*)&AR_const_true, 8);
				}
				else {
					R[ci].part4++;
					memcpy(&R[a0+iarg],
					(char*)&AR_const_true, 8);
				}
			else
				if(iarg >= 6) {
					arg_offset = (MPP_STACK_SIZE + iarg) * 8;
					memcpy(&stack[arg_offset],
					(char*)&AR_const_false, 8);
				}
				else {
					R[ci].part4++;
					memcpy(&R[a0+iarg],
					(char*)&AR_const_false, 8);
				}
			break;

		default:
			return AR_STAT_INVALID_TYPE;
		}
	}
}


int
ar_put_real_address(AR_INT_64* extdesc)
{
	int	segment = extdesc->part2 & 0xff;

	if(segment >= n_external_addresses)
		return AR_STAT_UNDEFINED;

#if _CRAY
	extdesc->part1 = (((long long)external_address[segment]) >> 48) & 0xffff;
	extdesc->part2 = (((long long)external_address[segment]) >> 32) & 0xffff;
	extdesc->part3 = (((long long)external_address[segment]) >> 16) & 0xffff;
	extdesc->part4 =  ((long long)external_address[segment])        & 0xffff;
#else
	extdesc->part1 = 0;
	extdesc->part2 = 0;
	extdesc->part3 = (((long)external_address[segment]) >> 16) & 0xffff;
	extdesc->part4 =  ((long)external_address[segment])        & 0xffff;
#endif

	return AR_STAT_OK;	
}


int
ar_get_function_value(ar_data *result, AR_TYPE *resulttype)
{
	int	status;

	/*
	 * Return function value in result if register conventions known
	 * for the result type.  Otherwise, the function value is assumed
	 * to be returned through memory and the caller must retrieve it.
	 * Use ar_convert_to_* to set ZERO and NEGATIVE status flags.  Note
	 * that error flags such as AR_STAT_OVERFLOW are cleared (these
	 * would have been returned by the intrinsic function simulation
	 * performed by ar_sim if run-time evaluation would have produced
	 * them).  So effectively, this routine only returns the bit
	 * pattern of the result plus zero or negative status.
	 */

	switch (AR_CLASS(*resulttype)) {

	case AR_CLASS_INT:

		status = ar_convert_to_integral(result, resulttype,
				  (ar_data*)&R[v0], &integer_64);
		break;

	case AR_CLASS_FLOAT:

		switch(UNROUNDED_TYPE(*resulttype)) {

		case IEEE_FLOAT_32:
		case IEEE_FLOAT_64:
			status = ar_convert_to_float(result, resulttype,
					  (ar_data*)&F[fv0], &D_float_64);
			break;

		case IEEE_FLOAT_128:
			status = ar_convert_to_float(result, resulttype,
					  (ar_data*)&F[fv0], &D_float_128);
			break;

		case IEEE_COMPLEX_32:
		case IEEE_COMPLEX_64:
			status = ar_convert_to_complex(result, resulttype,
					  (ar_data*)&F[fv0], &D_complex_64);
			break;

		case IEEE_COMPLEX_128:
			status = ar_convert_to_complex(result, resulttype,
					  (ar_data*)&F[fv0], &D_complex_128);
			break;

		default:
			return AR_STAT_INVALID_TYPE;
		}
		break;

	default:
		return AR_STAT_INVALID_TYPE;
	}
	return status &~ AR_ERROR_STATUS;
}

static int
call_host_external(char* func)
{
	int		status = AR_STAT_UNDEFINED;

	if (strncmp(func, "malloc", 6) == 0) {
		long	length;
		char	*ptr1;

		length = (R[a0].part3<<16) | R[a0].part4;
		ptr1   = malloc(length);
		status = ar_ext_address(&R[v0], (const char*)ptr1, (length+7)>>3);
	}

	return status;
}

int
ar_sim(char* intrinsic)
{
	char*   name;
	char*	str;

	int	i,n;
	int	disp;
	int	sign;

	long	pc;
	unsigned long p1,p2;
	int	op;
	int	ra,rb,rc;
	int	func;
	long	rtc, rtcpc;
	i64_union iu64;

	long	offset;
	long	value;
	int	status;

	AR_INT_64	int64;
	AR_INT_64	reg64;

	AR_IEEE_64	*rr, *oa, *ob;

	AR_TYPE		ar_type;

    static AR_HOST_UINT64    SSIB[7] = {
										/* Various information           */
        (((AR_HOST_UINT64) 0x00000002) << 32) |
		 ((AR_HOST_UINT64) 0x00080528),

        0,                              /* Saved_R, saved_F              */
        0,                              /* Entry point address           */
        0,                              /* Language specific information */
        0,                              /* Machine specific information  */
										/* "ARITH" (my name)             */
		(((AR_HOST_UINT64) 0x41524954) << 32) |
		 ((AR_HOST_UINT64) 0x48000000),
        0								/* Call information word         */
	};

	/* Set up initial Alpha stack and register values */

	/*
	 * The following test verifies that at least one argument
	 * has been passed via ar_pass_arg_{value|address}.  This in turn
	 * guarantees that the arith data file has been opened and read into
	 * memory so that the simulation is ready to begin.
	 */

	if(R[ci].part4 == 0)
		ar_internal_error(2014, __FILE__, __LINE__);

	/*
	 * Define a simulated stack and set up a DSIB with the call information
	 * (CI) word when simulating for a T3E.  For the T3D, the CI word is
	 * currently in R[ci].  For the T3E, this must be modified somewhat
	 * and stored in the simulated SSIB.  Additionally, the SSIB index
	 * of the CI word must be put into R[ci], and simulated SSIB address
	 * must be stored in the simulated DSIB.  The simulated DSIB goes
	 * after the MPP_MAX_ARGS*5 words of simulated callee stack space
	 * in the stack area.
	 */

	R[sp].part1 = R[fp].part1 = 0;
	R[sp].part2 = R[fp].part2 = stack_segment_id;

	R[sp].part3 = (MPP_STACK_SIZE*8)>>16;
	R[sp].part4 = (MPP_STACK_SIZE*8)&0xffff;

	if(ar_mach_type == T3D) {
		R[fp].part3 = (MPP_STACK_SIZE*8)>>16;
		R[fp].part4 = (MPP_STACK_SIZE*8)&0xffff;
	}
	else {
        AR_INT_64       SSIB_ia;        /* SSIB internal address */

#		define T3E_DSIB_LOC    (MPP_STACK_SIZE + (MPP_MAX_ARGS * 5))

		R[fp].part3 = ((T3E_DSIB_LOC + MPP_DSIB_SIZE) * 8) >> 16;
		R[fp].part4 = ((T3E_DSIB_LOC + MPP_DSIB_SIZE) * 8)  & 0xffff;

		SSIB[6] = (numargs << 52) |
				  (((AR_HOST_UINT64) (R[ci].part4 >> 8)) << 32) |
				  (R[ci].part4 & 0xff);

		R[ci].part1 = R[ci].part2 = R[ci].part3 = 0;
		R[ci].part4 = 6;

		if (ar_ext_address(&SSIB_ia, (char*) &SSIB, (sizeof(SSIB) >> 3)) !=
			AR_STAT_OK) {
			ar_internal_error(2012, __FILE__, __LINE__);
		}
		store_mpp_word(stack_segment_id, (T3E_DSIB_LOC + 2), &SSIB_ia);

#		undef T3E_DSIB_LOC
	}

	ZERO64(R[ret]);				/* Force R[ret] = 0 */

	ZERO64(R[zero]);			/* Force R31 = 0 */
	ZERO64(F[fzero]);			/* Force F31 = 0. */

	/*
	 * At this point all necessary registers (except pc) contain values
	 * that correspond with being at the entry point of the called function.
	 */

	/*
	 * Pass my simulation interface identifier.  The logic within the
	 * mpp simulation file (arith data file) can then validate and/or
	 * translate to its interface.  This allows this routine and a
	 * newer mpp simulation file to still function correctly.  In
	 * practice, this permits older (archived?) products to still work
	 * with a newer default arith data file.
	 */

	R[s0].part1 = R[s0].part2 = R[s0].part3 = 0;
	R[s0].part4 = ar_sim_version;		/* Sim interface id */

	/*
	 * Store LJ'd, zero-filled, uppercase intrinsic name into R[v0]
	 * as part of the simulation interface with the mpp simulation file
	 * (arith data file).  The simulation is assumed to begin (at pc=0)
	 * with a lookup of this name followed by an unconditional branch to
	 * the associated entry point when found.  The simulation continues
	 * until either an error condition occurs or the pc becomes 0 again
	 * corresponding to the 0 forced into the return address register
	 * (R[ret]) above.
	 */

	name = (char*)&R[v0];
	for(i=0; i<8 && isalnum(intrinsic[i]); i++)
	  name[i] = toupper(intrinsic[i]);
	while(i < 8)
	  name[i++] = 0;

	pc = 0;
	rtcpc = 0;
	status = AR_STAT_OK;
	while(status == AR_STAT_OK) {

	  DISASM1("0x%6.6x\t", pc);
	  if(pc&4) {
#if _CRAY
	    p1 = *(((unsigned long*)code)+(pc>>3)) >> 48;
	    p2 = (*(((unsigned long*)code)+(pc>>3)) >> 32) & 0xffff;
#else
	    p1 = (code[pc-4]<<8) | code[pc-3];
	    p2 = (code[pc-2]<<8) | code[pc-1];
#endif
	  }
	  else {
#if _CRAY
	    p1 = (*(((unsigned long*)code)+(pc>>3)) >> 16) & 0xffff;
	    p2 = *(((unsigned long*)code)+(pc>>3)) & 0xffff;
#else
	    p1 = (code[pc+4]<<8) | code[pc+5];
	    p2 = (code[pc+6]<<8) | code[pc+7];
#endif
	  }
	  pc += 4;
	  op = p1>>10;
	  ra = (p1>>5)&037;
	  rb = p1&037;
	  func = (p2>>5)&03777;
	  rc = p2&037;

	  switch(op) {

	  case 0x00:
		status = R[v0].part1 | R[v0].part2 | R[v0].part3 | R[v0].part4;
		DISASM2("CALL_PAL(0x%7.7x)\t0x%4.4x\n", p1<<16 | p2, status);
		switch (status) {
		case IFACEERR:		/* Unsupported intrinsic interface */
		  ar_internal_error(2014, __FILE__, __LINE__);
		  status = AR_STAT_UNDEFINED;
		  break;
		case NOINTRIN:		/* Unsupported intrinsic name */
		  str=(char*)&R[v0];
		  PRINTMSG(0, 2015, Internal, 0, __FILE__, __LINE__, intrinsic, "");
		  status = AR_STAT_UNDEFINED;
		  break;
		case EXTERROR:		/* Unsupported EXT called by intrinsic */
		  ar_internal_error(2012, __FILE__, __LINE__);
		case ARITHERR:		/* Error detected by libm routine */
		  status = AR_STAT_UNDEFINED;
		  break;
		case SETERRNO:		/* Set errno = ERANGE */
		  errno = ERANGE;
		  status = AR_STAT_OVERFLOW;
		  pc = 0;
		  break;
		case HOSTEXT:
		  status = call_host_external((char*)&R[t1]);
	  	  pc = ((R[ret].part3<<16)|R[ret].part4) &~ 3;
		  break;
		default:
		  status = BADINSTR;
		}
	  	break;

	  case 0x19:
	  case 0x1b:
	  case 0x1d:
	  case 0x1e:
	  case 0x1f:
	  	status = BADINSTR;
		DISASM2("CALL_PAL(0x%7.7x)\t0x%4.4x\n", p1<<16 | p2, status);
	  	break;

	  case 0x01:
	  case 0x02:
	  case 0x03:
	  case 0x04:
	  case 0x05:
	  case 0x06:
	  case 0x07:
	  case 0x0a:
	  case 0x0c:
	  case 0x0d:
	  case 0x0e:
	  case 0x14:
	  case 0x1c:
	  	status = BADINSTR;
	  	DISASM5("%x.%x.%x.%x.%x\n", op, ra, rb, func, rc);
	  	break;

	  case 0x08:
	  	if(ra != zero) {
	  	  reg64.part4 = p2;
	  	  if(p2>>15)
	  	    reg64.part1 = reg64.part2 = reg64.part3 = 0xffff;
	  	  else
	  	    reg64.part1 = reg64.part2 = reg64.part3 = 0;
	  	  ADD64(R[ra], reg64, R[rb]);
	  	}
	  	DISASM5("LDA\t%s,0x%4.4x(%s)\t0x%8.8x %8.8x\n",
	  		RN[ra], p2, RN[rb],
	  		(R[ra].part1<<16) | R[ra].part2,
	  		(R[ra].part3<<16) | R[ra].part4);
	  	break;

	  case 0x09:
	  	if(ra != zero) {
	  	  reg64.part3 = p2;
	  	  reg64.part4 = 0;
	  	  if(p2>>15)
	  	    reg64.part1 = reg64.part2 = 0xffff;
	  	  else
	  	    reg64.part1 = reg64.part2 = 0;
	  	  ADD64(R[ra], reg64, R[rb]);
	  	}
	  	DISASM5("LDAH\t%s,0x%4.4x(%s)\t0x%8.8x %8.8x\n",
	  		RN[ra], p2, RN[rb],
	  		(R[ra].part1<<16) | R[ra].part2,
	  		(R[ra].part3<<16) | R[ra].part4);
	  	break;

	  case 0x0b:
	  	if(ra != zero) {
	  	  reg64.part4 = p2;
	  	  if(p2>>15)
	  	    reg64.part1 = reg64.part2 = reg64.part3 = 0xffff;
	  	  else
	  	    reg64.part1 = reg64.part2 = reg64.part3 = 0;
	  	  ADD64(reg64, reg64, R[rb]);
	  	  offset = ((reg64.part3<<16) | reg64.part4) >> 3;
	  	  load_mpp_word(&R[ra], reg64.part2&0xff, offset);
	  	}
	  	DISASM5("LDQ_U\t%s,0x%4.4x(%s)\t0x%8.8x %8.8x\n",
	  		RN[ra], p2, RN[rb],
	  		(R[ra].part1<<16) | R[ra].part2,
	  		(R[ra].part3<<16) | R[ra].part4);
	  	break;

	  case 0x0f:
	  	reg64.part4 = p2;
	  	if(p2>>15)
	  	  reg64.part1 = reg64.part2 = reg64.part3 = 0xffff;
	  	else
	  	  reg64.part1 = reg64.part2 = reg64.part3 = 0;
	  	ADD64(reg64, reg64, R[rb]);
	  	offset = ((reg64.part3<<16) | reg64.part4) >> 3;
	  	store_mpp_word(reg64.part2&0xff, offset, &R[ra]);
	  	DISASM3("STQ_U\t%s,0x%4.4x(%s)\n", RN[ra], p2, RN[rb]);
	  	break;

	  case 0x10:
	  	if(func&0x80) {
	  	  reg64.part1 = reg64.part2 = reg64.part3 = 0;
	  	  reg64.part4 = (rb<<3)|(func>>8);
	  	}
	  	else
	  	  COPY64(reg64, R[rb]);
	  	switch(func&0x7f) {
	  	case 0x00:
	  		DISASM0("ADDL\t");
	  		ADD64(R[rc], R[ra], reg64);
	  		if(R[rc].part3>>15)
	  		  R[rc].part1 = R[rc].part2 = 0xffff;
	  		else
	  		  R[rc].part1 = R[rc].part2 = 0;
	  		break;
	  	case 0x02:
	  		DISASM0("S4ADDL\t");
	  		COPY64(int64, R[ra]);
	  		SHLEFT64N(int64, 2);
	  		ADD64(R[rc], int64, reg64);
	  		if(R[rc].part3>>15)
	  		  R[rc].part1 = R[rc].part2 = 0xffff;
	  		else
	  		  R[rc].part1 = R[rc].part2 = 0;
	  		break;
	  	case 0x09:
	  		DISASM0("SUBL\t");
	  		NEG64(reg64);
	  		ADD64(R[rc], R[ra], reg64);
	  		if(R[rc].part3>>15)
	  		  R[rc].part1 = R[rc].part2 = 0xffff;
	  		else
	  		  R[rc].part1 = R[rc].part2 = 0;
	  		break;
	  	case 0x0b:
	  		DISASM0("S4SUBL\t");
	  		COPY64(int64, R[ra]);
	  		SHLEFT64N(int64, 2);
	  		NEG64(reg64);
	  		ADD64(R[rc], int64, reg64);
	  		if(R[rc].part3>>15)
	  		  R[rc].part1 = R[rc].part2 = 0xffff;
	  		else
	  		  R[rc].part1 = R[rc].part2 = 0;
	  		break;
	  	case 0x0f:
	  		DISASM0("CMPBGE\t");
	  		value = 0;
	  		if((R[ra].part1>>8) >= (reg64.part1>>8))
	  		  value |= 1<<7;
	  		if((R[ra].part1&0xff) >= (reg64.part1&0xff))
	  		  value |= 1<<6;
	  		if((R[ra].part2>>8) >= (reg64.part2>>8))
	  		  value |= 1<<5;
	  		if((R[ra].part2&0xff) >= (reg64.part2&0xff))
	  		  value |= 1<<4;
	  		if((R[ra].part3>>8) >= (reg64.part3>>8))
	  		  value |= 1<<3;
	  		if((R[ra].part3&0xff) >= (reg64.part3&0xff))
	  		  value |= 1<<2;
	  		if((R[ra].part4>>8) >= (reg64.part4>>8))
	  		  value |= 1<<1;
	  		if((R[ra].part4&0xff) >= (reg64.part4&0xff))
	  		  value |= 1;
	  		R[rc].part1 = R[rc].part2 = R[rc].part3 = 0;
	  		R[rc].part4 = value;
	  		break;
	  	case 0x12:
	  		DISASM0("S8ADDL\t");
	  		COPY64(int64, R[ra]);
	  		SHLEFT64N(int64, 3);
	  		ADD64(R[rc], int64, reg64);
	  		if(R[rc].part3>>15)
	  		  R[rc].part1 = R[rc].part2 = 0xffff;
	  		else
	  		  R[rc].part1 = R[rc].part2 = 0;
	  		break;
	  	case 0x1b:
	  		DISASM0("S8SUBL\t");
	  		COPY64(int64, R[ra]);
	  		SHLEFT64N(int64, 3);
	  		NEG64(reg64);
	  		ADD64(R[rc], int64, reg64);
	  		if(R[rc].part3>>15)
	  		  R[rc].part1 = R[rc].part2 = 0xffff;
	  		else
	  		  R[rc].part1 = R[rc].part2 = 0;
	  		break;
	  	case 0x1d:
	  		DISASM0("CMPULT\t");
	  		value = 0;
	  		if(R[ra].part1 < reg64.part1)
				value = 1;
	  		else if(R[ra].part1 == reg64.part1)
	  		  if(R[ra].part2 < reg64.part2)
				value = 1;
	  		  else if(R[ra].part2 == reg64.part2)
	  		    if(R[ra].part3 < reg64.part3)
				value = 1;
	  		    else if(R[ra].part3 == reg64.part3)
	  		      if(R[ra].part4 < reg64.part4)
				value = 1;
	  		R[rc].part1 = R[rc].part2 = R[rc].part3 = 0;
	  		R[rc].part4 = value;
	  		break;
	  	case 0x20:
	  		DISASM0("ADDQ\t");
	  		ADD64(R[rc], R[ra], reg64);
	  		break;
	  	case 0x22:
	  		DISASM0("S4ADDQ\t");
	  		COPY64(int64, R[ra]);
	  		SHLEFT64N(int64, 2);
	  		ADD64(R[rc], int64, reg64);
	  		break;
	  	case 0x29:
	  		DISASM0("SUBQ\t");
	  		NEG64(reg64);
	  		ADD64(R[rc], R[ra], reg64);
	  		break;
	  	case 0x2b:
	  		DISASM0("S4SUBQ\t");
	  		COPY64(int64, R[ra]);
	  		SHLEFT64N(int64, 2);
	  		NEG64(reg64);
	  		ADD64(R[rc], int64, reg64);
	  		break;
	  	case 0x2d:
	  		DISASM0("CMPEQ\t");
	  		value = 0;
	  		if(R[ra].part1 == reg64.part1)
	  		  if(R[ra].part2 == reg64.part2)
	  		    if(R[ra].part3 == reg64.part3)
	  		      if(R[ra].part4 == reg64.part4)
				value = 1;
	  		R[rc].part1 = R[rc].part2 = R[rc].part3 = 0;
	  		R[rc].part4 = value;
	  		break;
	  	case 0x32:
	  		DISASM0("S8ADDQ\t");
	  		COPY64(int64, R[ra]);
	  		SHLEFT64N(int64, 3);
	  		ADD64(R[rc], int64, reg64);
	  		break;
	  	case 0x3b:
	  		DISASM0("S8SUBQ\t");
	  		COPY64(int64, R[ra]);
	  		SHLEFT64N(int64, 3);
	  		NEG64(reg64);
	  		ADD64(R[rc], int64, reg64);
	  		break;
	  	case 0x3d:
	  		DISASM0("CMPULE\t");
	  		value = 0;
	  		if(R[ra].part1 < reg64.part1)
				value = 1;
	  		else if(R[ra].part1 == reg64.part1)
	  		  if(R[ra].part2 < reg64.part2)
				value = 1;
	  		  else if(R[ra].part2 == reg64.part2)
	  		    if(R[ra].part3 < reg64.part3)
				value = 1;
	  		    else if(R[ra].part3 == reg64.part3)
	  		      if(R[ra].part4 <= reg64.part4)
				value = 1;
	  		R[rc].part1 = R[rc].part2 = R[rc].part3 = 0;
	  		R[rc].part4 = value;
	  		break;
	  	case 0x4d:
	  		DISASM0("CMPLT\t");
	  		NEG64(reg64);
	  		ADD64(reg64, R[ra], reg64);
	  		R[rc].part1 = R[rc].part2 = R[rc].part3 = 0;
	  		R[rc].part4 = reg64.part1>>15;
	  		break;
	  	case 0x6d:
	  		DISASM0("CMPLE\t");
	  		NEG64(reg64);
	  		ADD64(reg64, R[ra], reg64);
	  		R[rc].part1 = R[rc].part2 = R[rc].part3 = 0;
	  		R[rc].part4 = reg64.part1>>15;
	  		if(R[rc].part4 == 0)
	  		  if((reg64.part1|reg64.part2|reg64.part3|reg64.part4) == 0)
	  		    R[rc].part4 = 1;
	  		break;
	  	default:
	  		status = BADINSTR;
	  		DISASM5("%x.%x.%x.%x.%x\n", op, ra, rb, func, rc);
	  	}
	  	if(func&0x80) {
		    DISASM5("%s,0x%2.2x,%s\t0x%8.8x %8.8x\n",
		    	RN[ra], (rb<<3)|(func>>8), RN[rc],
	  		(R[rc].part1<<16) | R[rc].part2,
	  		(R[rc].part3<<16) | R[rc].part4);
	  	}
	  	else {
		    DISASM5("%s,%s,%s\t0x%8.8x %8.8x\n",
		    	RN[ra], RN[rb], RN[rc],
	  		(R[rc].part1<<16) | R[rc].part2,
	  		(R[rc].part3<<16) | R[rc].part4);
	  	}
	  	break;

	  case 0x11:
	  	if(func&0x80) {
	  	  reg64.part1 = reg64.part2 = reg64.part3 = 0;
	  	  reg64.part4 = (rb<<3)|(func>>8);
	  	}
	  	else
	  	  COPY64(reg64, R[rb]);
	  	switch(func&0x7f) {
	  	case 0x00:
	  		DISASM0("AND\t");
	  		R[rc].part1 = R[ra].part1&reg64.part1;
	  		R[rc].part2 = R[ra].part2&reg64.part2;
	  		R[rc].part3 = R[ra].part3&reg64.part3;
	  		R[rc].part4 = R[ra].part4&reg64.part4;
	  		break;
	  	case 0x08:
	  		DISASM0("BIC\t");
	  		R[rc].part1 = R[ra].part1&~reg64.part1;
	  		R[rc].part2 = R[ra].part2&~reg64.part2;
	  		R[rc].part3 = R[ra].part3&~reg64.part3;
	  		R[rc].part4 = R[ra].part4&~reg64.part4;
	  		break;
	  	case 0x20:
	  		DISASM0("BIS\t");
	  		R[rc].part1 = R[ra].part1|reg64.part1;
	  		R[rc].part2 = R[ra].part2|reg64.part2;
	  		R[rc].part3 = R[ra].part3|reg64.part3;
	  		R[rc].part4 = R[ra].part4|reg64.part4;
	  		break;
	  	case 0x14:
	  		DISASM0("CMOVLBS\t");
	  		if(R[ra].part4&1)
	  		  COPY64(R[rc], reg64);
	  		break;
	  	case 0x16:
	  		DISASM0("CMOVLBC\t");
	  		if((R[ra].part4&1) == 0)
	  		  COPY64(R[rc], reg64);
	  		break;
	  	case 0x24:
	  		DISASM0("CMOVEQ\t");
	  		if((R[ra].part1|R[ra].part2|R[ra].part3|R[ra].part4) == 0)
	  		  COPY64(R[rc], reg64);
	  		break;
	  	case 0x26:
	  		DISASM0("CMOVNE\t");
	  		if((R[ra].part1|R[ra].part2|R[ra].part3|R[ra].part4) != 0)
	  		  COPY64(R[rc], reg64);
	  		break;
	  	case 0x28:
	  		DISASM0("ORNOT\t");
	  		R[rc].part1 = R[ra].part1|~reg64.part1;
	  		R[rc].part2 = R[ra].part2|~reg64.part2;
	  		R[rc].part3 = R[ra].part3|~reg64.part3;
	  		R[rc].part4 = R[ra].part4|~reg64.part4;
	  		break;
	  	case 0x40:
	  		DISASM0("XOR\t");
	  		R[rc].part1 = R[ra].part1^reg64.part1;
	  		R[rc].part2 = R[ra].part2^reg64.part2;
	  		R[rc].part3 = R[ra].part3^reg64.part3;
	  		R[rc].part4 = R[ra].part4^reg64.part4;
	  		break;
	  	case 0x44:
	  		DISASM0("CMOVLT\t");
	  		if(R[ra].part1>>15)
	  		  COPY64(R[rc], reg64);
	  		break;
	  	case 0x46:
	  		DISASM0("CMOVGE\t");
	  		if((R[ra].part1>>15) == 0)
	  		  COPY64(R[rc], reg64);
	  		break;
	  	case 0x48:
	  		DISASM0("EQV\t");
	  		R[rc].part1 = R[ra].part1^~reg64.part1;
	  		R[rc].part2 = R[ra].part2^~reg64.part2;
	  		R[rc].part3 = R[ra].part3^~reg64.part3;
	  		R[rc].part4 = R[ra].part4^~reg64.part4;
	  		break;
	  	case 0x64:
	  		DISASM0("CMOVLE\t");
	  		if(R[ra].part1>>15)
	  		  COPY64(R[rc], reg64);
	  		else if((R[ra].part1|R[ra].part2|R[ra].part3|R[ra].part4) == 0)
	  		  COPY64(R[rc], reg64);
	  		break;
	  	case 0x66:
	  		DISASM0("CMOVGT\t");
	  		if((R[ra].part1>>15) == 0)
	  		  if((R[ra].part1|R[ra].part2|R[ra].part3|R[ra].part4) != 0)
	  		    COPY64(R[rc], reg64);
	  		break;
	  	default:
	  		status = BADINSTR;
	  		DISASM5("%x.%x.%x.%x.%x\n", op, ra, rb, func, rc);
	  	}
		if(rc == zero)
			ZERO64(R[zero]);
	  	if(func&0x80) {
		    DISASM5("%s,0x%2.2x,%s\t0x%8.8x %8.8x\n",
		    	RN[ra], (rb<<3)|(func>>8), RN[rc],
	  		(R[rc].part1<<16) | R[rc].part2,
	  		(R[rc].part3<<16) | R[rc].part4);
	  	}
	  	else {
		    DISASM5("%s,%s,%s\t0x%8.8x %8.8x\n",
		    	RN[ra], RN[rb], RN[rc],
	  		(R[rc].part1<<16) | R[rc].part2,
	  		(R[rc].part3<<16) | R[rc].part4);
	  	}
	  	break;

	  case 0x12:
		value = (func&0x80) ? ((rb<<3)|(func>>8)) : R[rb].part4;
	  	switch(func&0x7f) {
	  	case 0x02:
	  		DISASM0("MSKBL\t");
			value = (ar_mach_type == T3D) ? (value&0x7) : ((value&0x7)^0x7);
	  		reg64.part1 = reg64.part2 = reg64.part3 = 0;
	  		reg64.part4 = 0xff;
	  		SHLEFT64N(reg64, value<<3);
	  		R[rc].part1 = R[ra].part1 &~ reg64.part1;
	  		R[rc].part2 = R[ra].part2 &~ reg64.part2;
	  		R[rc].part3 = R[ra].part3 &~ reg64.part3;
	  		R[rc].part4 = R[ra].part4 &~ reg64.part4;
	  		break;
	  	case 0x06:
	  		DISASM0("EXTBL\t");
			value = (ar_mach_type == T3D) ? (value&0x7) : ((value&0x7)^0x7);
	  		if(ra != rc)
	  		  COPY64(R[rc], R[ra]);
	  		SHRIGHT64N(R[rc], value<<3);
	  		R[rc].part1 = R[rc].part2 = R[rc].part3 = 0;
	  		R[rc].part4 &= 0xff;
	  		break;
	  	case 0x0b:
	  		DISASM0("INSBL\t");
			value = (ar_mach_type == T3D) ? (value&0x7) : ((value&0x7)^0x7);
	  		if(ra != rc)
	  		  COPY64(R[rc], R[ra]);
	  		R[rc].part1 = R[rc].part2 = R[rc].part3 = 0;
	  		R[rc].part4 &= 0xff;
	  		SHLEFT64N(R[rc], value<<3);
	  		break;
	  	case 0x12:
	  		DISASM0("MSKWL\t");
			value = (ar_mach_type == T3D) ? (value&0x7) : ((value&0x7)^0x7);
	  		reg64.part1 = reg64.part2 = reg64.part3 = 0;
	  		reg64.part4 = 0xffff;
	  		SHLEFT64N(reg64, value<<3);
	  		R[rc].part1 = R[ra].part1 &~ reg64.part1;
	  		R[rc].part2 = R[ra].part2 &~ reg64.part2;
	  		R[rc].part3 = R[ra].part3 &~ reg64.part3;
	  		R[rc].part4 = R[ra].part4 &~ reg64.part4;
	  		break;
	  	case 0x16:
	  		DISASM0("EXTWL\t");
			value = (ar_mach_type == T3D) ? (value&0x7) : ((value&0x7)^0x7);
	  		if(ra != rc)
	  		  COPY64(R[rc], R[ra]);
	  		SHRIGHT64N(R[rc], value<<3);
	  		R[rc].part1 = R[rc].part2 = R[rc].part3 = 0;
	  		break;
	  	case 0x1b:
	  		DISASM0("INSWL\t");
			value = (ar_mach_type == T3D) ? (value&0x7) : ((value&0x7)^0x7);
	  		if(ra != rc)
	  		  COPY64(R[rc], R[ra]);
	  		R[rc].part1 = R[rc].part2 = R[rc].part3 = 0;
	  		SHLEFT64N(R[rc], value<<3);
	  		break;
	  	case 0x22:
	  		DISASM0("MSKHL\t");
			value = (ar_mach_type == T3D) ? (value&0x7) : ((value&0x7)^0x7);
	  		reg64.part1 = reg64.part2 = 0;
	  		reg64.part3 = reg64.part4 = 0xffff;
	  		SHLEFT64N(reg64, value<<3);
	  		R[rc].part1 = R[ra].part1 &~ reg64.part1;
	  		R[rc].part2 = R[ra].part2 &~ reg64.part2;
	  		R[rc].part3 = R[ra].part3 &~ reg64.part3;
	  		R[rc].part4 = R[ra].part4 &~ reg64.part4;
	  		break;
	  	case 0x26:
	  		DISASM0("EXTLL\t");
			value = (ar_mach_type == T3D) ? (value&0x7) : ((value&0x7)^0x7);
	  		if(ra != rc)
	  		  COPY64(R[rc], R[ra]);
	  		SHRIGHT64N(R[rc], value<<3);
	  		R[rc].part1 = R[rc].part2 = 0;
	  		break;
	  	case 0x2b:
	  		DISASM0("INSLL\t");
			value = (ar_mach_type == T3D) ? (value&0x7) : ((value&0x7)^0x7);
	  		if(ra != rc)
	  		  COPY64(R[rc], R[ra]);
	  		R[rc].part1 = R[rc].part2 = 0;
	  		SHLEFT64N(R[rc], value<<3);
	  		break;
	  	case 0x30:
	  		DISASM0("ZAP\t");
	  		COPY64(R[rc], R[ra]);
	  		for(i=0; i<=7; i++)
	  		  if(value&(1<<i))
	  		    memset((char*)(&R[rc])+7-i, 0, 1);
	  		break;
	  	case 0x31:
	  		DISASM0("ZAPNOT\t");
	  		COPY64(R[rc], R[ra]);
	  		for(i=0; i<=7; i++)
	  		  if(!(value&(1<<i)))
	  		    memset((char*)(&R[rc])+7-i, 0, 1);
	  		break;
	  	case 0x32:
	  		DISASM0("MSKQL\t");
			value = (ar_mach_type == T3D) ? (value&0x7) : ((value&0x7)^0x7);
			reg64.part1 = reg64.part2 = reg64.part3 = reg64.part4 = 0xffff;
			SHLEFT64N(reg64, value<<3);
			R[rc].part1 = R[ra].part1 &~ reg64.part1;
			R[rc].part2 = R[ra].part2 &~ reg64.part2;
			R[rc].part3 = R[ra].part3 &~ reg64.part3;
			R[rc].part4 = R[ra].part4 &~ reg64.part4;
	  		break;
	  	case 0x34:
	  		DISASM0("SRL\t");
	  		if(ra != rc)
	  		  COPY64(R[rc], R[ra]);
	  		SHRIGHT64N(R[rc], value&077);
	  		break;
	  	case 0x36:
	  		DISASM0("EXTQL\t");
			value = (ar_mach_type == T3D) ? (value&0x7) : ((value&0x7)^0x7);
	  		if(ra != rc)
	  		  COPY64(R[rc], R[ra]);
	  		SHRIGHT64N(R[rc], value<<3);
	  		break;
	  	case 0x39:
	  		DISASM0("SLL\t");
	  		if(ra != rc)
	  		  COPY64(R[rc], R[ra]);
	  		SHLEFT64N(R[rc], value&077);
	  		break;
	  	case 0x3b:
	  		DISASM0("INSQL\t");
			value = (ar_mach_type == T3D) ? (value&0x7) : ((value&0x7)^0x7);
	  		if(ra != rc)
	  		  COPY64(R[rc], R[ra]);
	  		SHLEFT64N(R[rc], value<<3);
	  		break;
	  	case 0x3c:
	  		DISASM0("SRA\t");
	  		if(ra != rc)
	  		  COPY64(R[rc], R[ra]);
	  		if(R[rc].part1>>15) {
	  		  NOT64(R[rc]);
	  		  SHRIGHT64N(R[rc], value&077);
	  		  NOT64(R[rc]);
	  		}
	  		else
	  		  SHRIGHT64N(R[rc], value&077);
	  		break;
	  	case 0x52:
	  		DISASM0("MSKWH\t");
			value = (ar_mach_type == T3D) ? (value&0x7) : ((value&0x7)^0x7);
			reg64.part1 = reg64.part2 = reg64.part3 = 0;
			reg64.part4 = 0xffff;
			SHRIGHT64N(reg64, 64-(value<<3));
			R[rc].part1 = R[ra].part1 &~ reg64.part1;
			R[rc].part2 = R[ra].part2 &~ reg64.part2;
			R[rc].part3 = R[ra].part3 &~ reg64.part3;
			R[rc].part4 = R[ra].part4 &~ reg64.part4;
	  		break;
	  	case 0x57:
	  		DISASM0("INSWH\t");
			value = (ar_mach_type == T3D) ? (value&0x7) : ((value&0x7)^0x7);
	  		if(ra != rc)
	  		  COPY64(R[rc], R[ra]);
	  		R[rc].part1 = R[rc].part2 = R[rc].part3 = 0;
	  		SHRIGHT64N(R[rc], 64-(value<<3));
	  		break;
	  	case 0x5a:
	  		DISASM0("EXTWH\t");
			value = (ar_mach_type == T3D) ? (value&0x7) : ((value&0x7)^0x7);
	  		if(ra != rc)
	  		  COPY64(R[rc], R[ra]);
	  		SHLEFT64N(R[rc], (64-(value<<3))&077);
	  		R[rc].part2 = R[rc].part3 = R[rc].part4 = 0;
	  		break;
	  	case 0x62:
	  		DISASM0("MSKLH\t");
			value = (ar_mach_type == T3D) ? (value&0x7) : ((value&0x7)^0x7);
			reg64.part1 = reg64.part2 = 0;
			reg64.part3 = reg64.part4 = 0xffff;
			SHRIGHT64N(reg64, 64-(value<<3));
			R[rc].part1 = R[ra].part1 &~ reg64.part1;
			R[rc].part2 = R[ra].part2 &~ reg64.part2;
			R[rc].part3 = R[ra].part3 &~ reg64.part3;
			R[rc].part4 = R[ra].part4 &~ reg64.part4;
	  		break;
	  	case 0x67:
	  		DISASM0("INSLH\t");
			value = (ar_mach_type == T3D) ? (value&0x7) : ((value&0x7)^0x7);
	  		if(ra != rc)
	  		  COPY64(R[rc], R[ra]);
	  		R[rc].part1 = R[rc].part2 = 0;
	  		SHRIGHT64N(R[rc], 64-(value<<3));
	  		break;
	  	case 0x6a:
	  		DISASM0("EXTLH\t");
			value = (ar_mach_type == T3D) ? (value&0x7) : ((value&0x7)^0x7);
	  		if(ra != rc)
	  		  COPY64(R[rc], R[ra]);
	  		SHLEFT64N(R[rc], (64-(value<<3))&077);
	  		R[rc].part3 = R[rc].part4 = 0;
	  		break;
	  	case 0x72:
	  		DISASM0("MSKQH\t");
			value = (ar_mach_type == T3D) ? (value&0x7) : ((value&0x7)^0x7);
			reg64.part1 = reg64.part2 = reg64.part3 = reg64.part4 = 0xffff;
			SHRIGHT64N(reg64, 64-(value<<3));
			R[rc].part1 = R[ra].part1 &~ reg64.part1;
			R[rc].part2 = R[ra].part2 &~ reg64.part2;
			R[rc].part3 = R[ra].part3 &~ reg64.part3;
			R[rc].part4 = R[ra].part4 &~ reg64.part4;
	  		break;
	  	case 0x77:
	  		DISASM0("INSQH\t");
			value = (ar_mach_type == T3D) ? (value&0x7) : ((value&0x7)^0x7);
	  		if(ra != rc)
	  		  COPY64(R[rc], R[ra]);
	  		SHRIGHT64N(R[rc], 64-(value<<3));
	  		break;
	  	case 0x7a:
	  		DISASM0("EXTQH\t");
			value = (ar_mach_type == T3D) ? (value&0x7) : ((value&0x7)^0x7);
			if(ra != rc)
				COPY64(R[rc], R[ra]);
			SHLEFT64N(R[rc], (64-(value<<3))&077);
	  		break;
	  	default:
	  		status = BADINSTR;
	  		DISASM5("%x.%x.%x.%x.%x\n", op, ra, rb, func, rc);
	  	}
		if(rc == zero)
			ZERO64(R[zero]);
	  	if(func&0x80) {
		    DISASM5("%s,0x%2.2x,%s\t0x%8.8x %8.8x\n",
		    	RN[ra], (rb<<3)|(func>>8), RN[rc],
	  		(R[rc].part1<<16) | R[rc].part2,
	  		(R[rc].part3<<16) | R[rc].part4);
	  	}
	  	else {
		    DISASM5("%s,%s,%s\t0x%8.8x %8.8x\n",
		    	RN[ra], RN[rb], RN[rc],
	  		(R[rc].part1<<16) | R[rc].part2,
	  		(R[rc].part3<<16) | R[rc].part4);
	  	}
	  	break;

	  case 0x13:
	  	if(func&0x80) {
	  	  reg64.part1 = reg64.part2 = reg64.part3 = 0;
	  	  reg64.part4 = (rb<<3)|(func>>8);
	  	  sign = 0;
	  	}
	  	else
	  	  COPY64(reg64, R[rb]);
	  	COPY64(int64, R[ra]);
	  	ZERO64(R[rc]);
	  	switch(func&0x7f) {
	  	case 0x00:
	  		DISASM0("MULL\t");
	  		if(reg64.part1>>15) {
	  		  NEG64(reg64);
	  		  sign = 1;
	  		}
	  		else
	  		  sign = 0;
	  		if(int64.part1>>15) {
	  		  NEG64(int64);
	  		  sign ^= 1;
	  		}
	  		reg64.part1 = reg64.part2 = 0;
	  		int64.part1 = int64.part2 = 0;
	  		while(reg64.part3|reg64.part4) {
	  		  if(reg64.part4&1)
	  		    ADD64(R[rc],R[rc],int64);
	  		  SHLEFT64(int64);
	  		  SHRIGHT64(reg64);
	  		}
	  		R[rc].part1 = R[rc].part2 = 0;
	  		if(sign)
	  		  NEG64(R[rc]);
	  		break;
	  	case 0x20:
	  		DISASM0("MULQ\t");
	  		if(reg64.part1>>15) {
	  		  NEG64(reg64);
	  		  sign = 1;
	  		}
	  		else
	  		  sign = 0;
	  		if(int64.part1>>15) {
	  		  NEG64(int64);
	  		  sign ^= 1;
	  		}
	  		while(reg64.part1|reg64.part2|reg64.part3|reg64.part4) {
	  		  if(reg64.part4&1)
	  		    ADD64(R[rc],R[rc],int64);
	  		  SHLEFT64(int64);
	  		  SHRIGHT64(reg64);
	  		}
	  		if(sign)
	  		  NEG64(R[rc]);
	  		break;
	  	case 0x30:
	  		DISASM0("UMULH\t");
	  		n = 64;
	  		while(reg64.part1|reg64.part2|reg64.part3|reg64.part4) {
	  		  if(reg64.part4&1)
	  		    ADD64(R[rc],R[rc],int64);
	  		  SHRIGHT64(R[rc]);
	  		  SHRIGHT64(reg64);
	  		  n--;
	  		}
	  		SHRIGHT64N(R[rc], n);
	  		break;
	  	default:
	  		status = BADINSTR;
	  		DISASM5("%x.%x.%x.%x.%x\n", op, ra, rb, func, rc);
	  	}
		if(rc == zero)
			ZERO64(R[zero]);
	  	if(func&0x80) {
		    DISASM5("%s,0x%2.2x,%s\t0x%8.8x %8.8x\n",
		    	RN[ra], (rb<<3)|(func>>8), RN[rc],
	  		(R[rc].part1<<16) | R[rc].part2,
	  		(R[rc].part3<<16) | R[rc].part4);
	  	}
	  	else {
		    DISASM5("%s,%s,%s\t0x%8.8x %8.8x\n",
		    	RN[ra], RN[rb], RN[rc],
	  		(R[rc].part1<<16) | R[rc].part2,
	  		(R[rc].part3<<16) | R[rc].part4);
	  	}
	  	break;

	  case 0x15:
	  case 0x16:
	  	rr = (AR_IEEE_64*)&F[rc];
	  	oa = (AR_IEEE_64*)&F[ra];
	 	ob = (AR_IEEE_64*)&F[rb];
	  	switch(func) {
	  	case 0x00:
	  		DISASM0("ADDS/C\t");
	  		ar_type = AR_Float_IEEE_ZE_32;
	  		status = ar_ifadd64(rr, oa, ob, AR_ROUND_ZERO);
	  		status |= ar_convert_to_float((ar_data*)&reg64, &ar_type,
	  				     (ar_data*)&F[rc], &ieee_float_64);
	  		status |= ar_convert_to_float((ar_data*)&F[rc], &ieee_float_64,
	  				     (ar_data*)&reg64, &ieee_float_32);
			status &= AR_ERROR_STATUS;
	  		break;
	  	case 0x01:
	  		DISASM0("SUBS/C\t");
	  		ar_type = AR_Float_IEEE_ZE_32;
	  		status = ar_ifsub64(rr, oa, ob, AR_ROUND_ZERO);
	  		status |= ar_convert_to_float((ar_data*)&reg64, &ar_type,
	  				     (ar_data*)&F[rc], &ieee_float_64);
	  		status |= ar_convert_to_float((ar_data*)&F[rc], &ieee_float_64,
	  				     (ar_data*)&reg64, &ieee_float_32);
			status &= AR_ERROR_STATUS;
	  		break;
	  	case 0x02:
	  		DISASM0("MULS/C\t");
	  		ar_type = AR_Float_IEEE_ZE_32;
	  		status = ar_ifmul64(rr, oa, ob, AR_ROUND_ZERO);
	  		status |= ar_convert_to_float((ar_data*)&reg64, &ar_type,
	  				     (ar_data*)&F[rc], &ieee_float_64);
	  		status |= ar_convert_to_float((ar_data*)&F[rc], &ieee_float_64,
	  				     (ar_data*)&reg64, &ieee_float_32);
			status &= AR_ERROR_STATUS;
	  		break;
	  	case 0x03:
	  		DISASM0("DIVS/C\t");
	  		ar_type = AR_Float_IEEE_ZE_32;
	  		status = ar_ifdiv64(rr, oa, ob, AR_ROUND_ZERO);
	  		status |= ar_convert_to_float((ar_data*)&reg64, &ar_type,
	  				     (ar_data*)&F[rc], &ieee_float_64);
	  		status |= ar_convert_to_float((ar_data*)&F[rc], &ieee_float_64,
	  				     (ar_data*)&reg64, &ieee_float_32);
			status &= AR_ERROR_STATUS;
  			break;
	  	case 0x20:
	  		DISASM0("ADDT/C\t");
	  		status = ar_ifadd64(rr, oa, ob, AR_ROUND_ZERO)&AR_ERROR_STATUS;
	  		break;
	  	case 0x21:
	  		DISASM0("SUBT/C\t");
	  		status = ar_ifsub64(rr, oa, ob, AR_ROUND_ZERO)&AR_ERROR_STATUS;
	  		break;
	  	case 0x22:
	  		DISASM0("MULT/C\t");
	  		status = ar_ifmul64(rr, oa, ob, AR_ROUND_ZERO)&AR_ERROR_STATUS;
	  		break;
	  	case 0x23:
	  		DISASM0("DIVT/C\t");
	  		status = ar_ifdiv64(rr, oa, ob, AR_ROUND_ZERO)&AR_ERROR_STATUS;
	  		break;
	  	case 0x2c:
	  		DISASM0("CVTTS/C\t");
	  		ar_type = AR_Float_IEEE_ZE_32;
	  		status  = ar_convert_to_float((ar_data*)&reg64, &ar_type,
	  				     (ar_data*)&F[rb], &ieee_float_64);
	  		status |= ar_convert_to_float((ar_data*)&F[rc], &ieee_float_64,
	  				     (ar_data*)&reg64, &ieee_float_32);
			status &= AR_ERROR_STATUS;
	  		break;
	  	case 0x2f:
	  		DISASM0("CVTTQ/C\t");
	  		status = ar_ifix64((AR_INT_64*)rr, ob, 64, AR_ROUND_ZERO)&AR_ERROR_STATUS;
	  		break;
	  	case 0x3c:
	  		DISASM0("CVTQS/C\t");
	  		ar_type = AR_Float_IEEE_ZE_32;
	  		status = ar_iflt64(rr, (AR_INT_64*)ob, 0, AR_ROUND_ZERO);
	  		status |= ar_convert_to_float((ar_data*)&reg64, &ar_type,
	  				     (ar_data*)&F[rc], &ieee_float_64);
	  		status |= ar_convert_to_float((ar_data*)&F[rc], &ieee_float_64,
	  				     (ar_data*)&reg64, &ieee_float_32);
			status &= AR_ERROR_STATUS;
	  		break;
	  	case 0x3e:
	  		DISASM0("CVTQT/C\t");
	  		status = ar_iflt64(rr, (AR_INT_64*)ob, 0, AR_ROUND_ZERO)&AR_ERROR_STATUS;
	  		break;
	  	case 0x80:
	  		DISASM0("ADDS\t");
	  		ar_type = AR_Float_IEEE_NR_32;
	  		status = ar_ifadd64(rr, oa, ob, AR_ROUND_NEAREST);
	  		status |= ar_convert_to_float((ar_data*)&reg64, &ar_type,
	  				     (ar_data*)&F[rc], &ieee_float_64);
	  		status |= ar_convert_to_float((ar_data*)&F[rc], &ieee_float_64,
	  				     (ar_data*)&reg64, &ieee_float_32);
			status &= AR_ERROR_STATUS;
	  		break;
	  	case 0x81:
	  		DISASM0("SUBS\t");
	  		ar_type = AR_Float_IEEE_NR_32;
	  		status = ar_ifsub64(rr, oa, ob, AR_ROUND_NEAREST);
	  		status |= ar_convert_to_float((ar_data*)&reg64, &ar_type,
	  				     (ar_data*)&F[rc], &ieee_float_64);
	  		status |= ar_convert_to_float((ar_data*)&F[rc], &ieee_float_64,
	  				     (ar_data*)&reg64, &ieee_float_32);
			status &= AR_ERROR_STATUS;
	  		break;
	  	case 0x82:
	  		DISASM0("MULS\t");
	  		ar_type = AR_Float_IEEE_NR_32;
	  		status = ar_ifmul64(rr, oa, ob, AR_ROUND_NEAREST);
	  		status |= ar_convert_to_float((ar_data*)&reg64, &ar_type,
	  				     (ar_data*)&F[rc], &ieee_float_64);
	  		status |= ar_convert_to_float((ar_data*)&F[rc], &ieee_float_64,
	  				     (ar_data*)&reg64, &ieee_float_32);
			status &= AR_ERROR_STATUS;
	  		break;
	  	case 0x83:
	  		DISASM0("DIVS\t");
	  		ar_type = AR_Float_IEEE_NR_32;
	  		status = ar_ifdiv64(rr, oa, ob, AR_ROUND_NEAREST);
	  		status |= ar_convert_to_float((ar_data*)&reg64, &ar_type,
	  				     (ar_data*)&F[rc], &ieee_float_64);
	  		status |= ar_convert_to_float((ar_data*)&F[rc], &ieee_float_64,
	  				     (ar_data*)&reg64, &ieee_float_32);
			status &= AR_ERROR_STATUS;
  			break;
	  	case 0xa0:
	  		DISASM0("ADDT\t");
	  		status = ar_ifadd64(rr, oa, ob, AR_ROUND_NEAREST)&AR_ERROR_STATUS;
	  		break;
	  	case 0xa1:
	  		DISASM0("SUBT\t");
	  		status = ar_ifsub64(rr, oa, ob, AR_ROUND_NEAREST)&AR_ERROR_STATUS;
	  		break;
	  	case 0xa2:
	  		DISASM0("MULT\t");
	  		status = ar_ifmul64(rr, oa, ob, AR_ROUND_NEAREST)&AR_ERROR_STATUS;
	  		break;
	  	case 0xa3:
	  		DISASM0("DIVT\t");
	  		status = ar_ifdiv64(rr, oa, ob, AR_ROUND_NEAREST)&AR_ERROR_STATUS;
	  		break;
	  	case 0xa5:
	  		DISASM0("CMPTEQ\t");
	  		status = ar_ifcmp64((const AR_IEEE_64*)oa, (const AR_IEEE_64*)ob);
	  		F[rc].part2 = F[rc].part3 = F[rc].part4 = 0;
	  		if(status & AR_STAT_ZERO)
	  		  F[rc].part1 = 0x4000;
	  		else
	  		  F[rc].part1 = 0;
	  		status = AR_STAT_OK;
	  		break;
	  	case 0xa6:
	  		DISASM0("CMPTLT\t");
	  		status = ar_ifcmp64((const AR_IEEE_64*)oa, (const AR_IEEE_64*)ob);
	  		F[rc].part2 = F[rc].part3 = F[rc].part4 = 0;
	  		if(status & AR_STAT_NEGATIVE)
	  		  F[rc].part1 = 0x4000;
	  		else
	  		  F[rc].part1 = 0;
	  		status = AR_STAT_OK;
	  		break;
	  	case 0xa7:
	  		DISASM0("CMPTLE\t");
	  		status = ar_ifcmp64((const AR_IEEE_64*)oa, (const AR_IEEE_64*)ob);
	  		F[rc].part2 = F[rc].part3 = F[rc].part4 = 0;
	  		if(status & (AR_STAT_NEGATIVE|AR_STAT_ZERO))
	  		  F[rc].part1 = 0x4000;
	  		else
	  		  F[rc].part1 = 0;
	  		status = AR_STAT_OK;
	  		break;
	  	case 0xac:
	  		DISASM0("CVTTS\t");
	  		ar_type = AR_Float_IEEE_NR_32;
	  		status  = ar_convert_to_float((ar_data*)&reg64, &ar_type,
	  				     (ar_data*)&F[rb], &ieee_float_64);
	  		status |= ar_convert_to_float((ar_data*)&F[rc], &ieee_float_64,
	  				     (ar_data*)&reg64, &ieee_float_32);
			status &= AR_ERROR_STATUS;
	  		break;
	  	case 0xaf:
	  		DISASM0("CVTTQ\t");
	  		status = ar_ifix64((AR_INT_64*)rr, ob, 64, AR_ROUND_NEAREST)&AR_ERROR_STATUS;
	  		break;
	  	case 0xbc:
	  		DISASM0("CVTQS\t");
	  		ar_type = AR_Float_IEEE_NR_32;
	  		status = ar_iflt64(rr, (AR_INT_64*)ob, 0, AR_ROUND_NEAREST);
	  		status |= ar_convert_to_float((ar_data*)&reg64, &ar_type,
	  				     (ar_data*)&F[rc], &ieee_float_64);
	  		status |= ar_convert_to_float((ar_data*)&F[rc], &ieee_float_64,
	  				     (ar_data*)&reg64, &ieee_float_32);
			status &= AR_ERROR_STATUS;
	  		break;
	  	case 0xbe:
	  		DISASM0("CVTQT\t");
	  		status = ar_iflt64(rr, (AR_INT_64*)ob, 0, AR_ROUND_NEAREST)&AR_ERROR_STATUS;
	  		break;
	  	case 0xc0:
	  		DISASM0("ADDS/D\t");
	  		status = ar_ifadd64(rr, oa, ob, D_roundmode);
	  		status |= ar_convert_to_float((ar_data*)&reg64, &D_float_32,
	  				     (ar_data*)&F[rc], &ieee_float_64);
	  		status |= ar_convert_to_float((ar_data*)&F[rc], &ieee_float_64,
	  				     (ar_data*)&reg64, &ieee_float_32);
			status &= AR_ERROR_STATUS;
	  		break;
	  	case 0xc1:
	  		DISASM0("SUBS/D\t");
	  		status = ar_ifsub64(rr, oa, ob, D_roundmode);
	  		status |= ar_convert_to_float((ar_data*)&reg64, &D_float_32,
	  				     (ar_data*)&F[rc], &ieee_float_64);
	  		status |= ar_convert_to_float((ar_data*)&F[rc], &ieee_float_64,
	  				     (ar_data*)&reg64, &ieee_float_32);
			status &= AR_ERROR_STATUS;
	  		break;
	  	case 0xc2:
	  		DISASM0("MULS/D\t");
	  		status = ar_ifmul64(rr, oa, ob, D_roundmode);
	  		status |= ar_convert_to_float((ar_data*)&reg64, &D_float_32,
	  				     (ar_data*)&F[rc], &ieee_float_64);
	  		status |= ar_convert_to_float((ar_data*)&F[rc], &ieee_float_64,
	  				     (ar_data*)&reg64, &ieee_float_32);
			status &= AR_ERROR_STATUS;
	  		break;
	  	case 0xc3:
	  		DISASM0("DIVS/D\t");
	  		status = ar_ifdiv64(rr, oa, ob, D_roundmode);
	  		status |= ar_convert_to_float((ar_data*)&reg64, &D_float_32,
	  				     (ar_data*)&F[rc], &ieee_float_64);
	  		status |= ar_convert_to_float((ar_data*)&F[rc], &ieee_float_64,
	  				     (ar_data*)&reg64, &ieee_float_32);
			status &= AR_ERROR_STATUS;
  			break;
	  	case 0xe0:
	  		DISASM0("ADDT/D\t");
	  		status = ar_ifadd64(rr, oa, ob, D_roundmode)&AR_ERROR_STATUS;
	  		break;
	  	case 0xe1:
	  		DISASM0("SUBT/D\t");
	  		status = ar_ifsub64(rr, oa, ob, D_roundmode)&AR_ERROR_STATUS;
	  		break;
	  	case 0xe2:
	  		DISASM0("MULT/D\t");
	  		status = ar_ifmul64(rr, oa, ob, D_roundmode)&AR_ERROR_STATUS;
	  		break;
	  	case 0xe3:
	  		DISASM0("DIVT/D\t");
	  		status = ar_ifdiv64(rr, oa, ob, D_roundmode)&AR_ERROR_STATUS;
	  		break;
	  	case 0xec:
	  		DISASM0("CVTTS/D\t");
	  		status  = ar_convert_to_float((ar_data*)&reg64, &D_float_32,
	  				     (ar_data*)&F[rb], &ieee_float_64);
	  		status |= ar_convert_to_float((ar_data*)&F[rc], &ieee_float_64,
	  				     (ar_data*)&reg64, &ieee_float_32);
			status &= AR_ERROR_STATUS;
	  		break;
	  	case 0xef:
	  		DISASM0("CVTTQ/D\t");
	  		status = ar_ifix64((AR_INT_64*)rr, ob, 64, D_roundmode)&AR_ERROR_STATUS;
	  		break;
	  	case 0xfc:
	  		DISASM0("CVTQS/D\t");
	  		status = ar_iflt64(rr, (AR_INT_64*)ob, 0, D_roundmode);
	  		status |= ar_convert_to_float((ar_data*)&reg64, &D_float_32,
	  				     (ar_data*)&F[rc], &ieee_float_64);
	  		status |= ar_convert_to_float((ar_data*)&F[rc], &ieee_float_64,
	  				     (ar_data*)&reg64, &ieee_float_32);
			status &= AR_ERROR_STATUS;
	  		break;
	  	case 0xfe:
	  		DISASM0("CVTQT/D\t");
	  		status = ar_iflt64(rr, (AR_INT_64*)ob, 0, D_roundmode)&AR_ERROR_STATUS;
	  		break;
	  	default:
	  		status = BADINSTR;
	  		DISASM5("%x.%x.%x.%x.%x", op, ra, rb, func, rc);
	  	}
		if(rc == fzero)
			ZERO64(F[fzero]);
		DISASM5("%s,%s,%s\t0x%8.8x %8.8x\n",
		    	FN[ra], FN[rb], FN[rc],
	  		(F[rc].part1<<16) | F[rc].part2,
	  		(F[rc].part3<<16) | F[rc].part4);
	  	break;

	  case 0x17:
	  	switch(func) {
	  	case 0x10:
	  		DISASM0("CVTLQ\t");
	  		F[rc].part3 = (F[rb].part1&0xc000) |
	  			      ((F[rb].part1&0x7ff) << 3) |
	  			      (F[rb].part2 >> 13);
	  		F[rc].part4 = (F[rb].part2 << 3) |
	  			      (F[rb].part3 >> 13);
	  		if(F[rc].part3>>15)
	  		  F[rc].part1 = F[rc].part2 = 0xffff;
	  		else
	  		  F[rc].part1 = F[rc].part2 = 0;
			break;
	  	case 0x20:
	  		DISASM0("CPYS\t");
	  		sign = F[ra].part1&0x8000;
	  		COPY64(F[rc], F[rb]);
	  		if(ra != rb)
	  		  F[rc].part1 = (F[rc].part1&0x7fff) | sign;
	  		break;
	  	case 0x21:
	  		DISASM0("CPYSN\t");
	  		sign = 0x8000-(F[ra].part1&0x8000);
	  		COPY64(F[rc], F[rb]);
	  		F[rc].part1 = (F[rc].part1&0x7fff) | sign;
	  		break;
	  	case 0x22:
	  		DISASM0("CPYSE\t");
	  		sign = F[ra].part1&0xfff0;
	  		COPY64(F[rc], F[rb]);
	  		if(ra != rb)
	  		  F[rc].part1 = (F[rc].part1&0x000f) | sign;
	  		break;
	  	case 0x2a:
	  		DISASM0("FCMOVEQ\t");
	  		if(((F[ra].part1&0x7fff)|F[ra].part2|F[ra].part3|F[ra].part4) == 0)
	  		  COPY64(F[rc], F[rb]);
	  		break;
	  	case 0x2b:
	  		DISASM0("FCMOVNE\t");
	  		if(((F[ra].part1&0x7fff)|F[ra].part2|F[ra].part3|F[ra].part4) != 0)
	  		  COPY64(F[rc], F[rb]);
	  		break;
	  	case 0x2c:
	  		DISASM0("FCMOVLT\t");
	  		if(F[ra].part1&0x8000)
	  		  COPY64(F[rc], F[rb]);
	  		break;
	  	case 0x2d:
	  		DISASM0("FCMOVGE\t");
	  		if((F[ra].part1&0x8000) == 0)
	  		  COPY64(F[rc], F[rb]);
	  		break;
	  	case 0x2e:
	  		DISASM0("FCMOVLE\t");
	  		if((F[ra].part1&0x8000) ||
	  		   ((F[ra].part1&0x7fff)|F[ra].part2|F[ra].part3|F[ra].part4) == 0)
	  		  COPY64(F[rc], F[rb]);
	  		break;
	  	case 0x2f:
	  		DISASM0("FCMOVGT\t");
	  		if((F[ra].part1&0x8000) == 0 &&
	  		   ((F[ra].part1&0x7fff)|F[ra].part2|F[ra].part3|F[ra].part4) != 0)
	  		  COPY64(F[rc], F[rb]);
	  		break;
	  	case 0x30:
	  		DISASM0("CVTQL\t");
	  		F[rc].part1 = (F[rb].part3 & 0xc000) |
	  			     ((F[rb].part3 & 0x3fff) >> 3);
			F[rc].part2 = (F[rb].part3 << 13) |
				      (F[rb].part4 >> 3);
	  		F[rc].part3 = (F[rb].part4 << 13);
	  		F[rc].part4 = 0;
			break;
	  	default:
	  		status = BADINSTR;
	  		DISASM5("%x.%x.%x.%x.%x", op, ra, rb, func, rc);
	  	}
		if(rc == fzero)
			ZERO64(F[fzero]);
		DISASM5("%s,%s,%s\t0x%8.8x %8.8x\n",
		    	FN[ra], FN[rb], FN[rc],
	  		(F[rc].part1<<16) | F[rc].part2,
	  		(F[rc].part3<<16) | F[rc].part4);
	  	break;

	  case 0x18:
	  	switch(p2) {
	  	case 0x0:
 		  DISASM0("TRAPB\n");
	  	  break;
	  	case 0x4:
 		  DISASM0("MB\n");
	  	  break;
	  	case 0x8:
 		  DISASM2("FETCH\t%s,%s\n", RN[ra],RN[rb]);
	  	  break;
	  	case 0xa:
 		  DISASM2("FETCH_M\t%s,%s\n", RN[ra],RN[rb]);
	  	  break;
	  	case 0xc:
		  if(rtcpc < pc)
			rtc += (pc-rtcpc);
		  else
			rtc += (rtcpc-pc);
		  rtcpc = pc;
		  iu64.l64 = rtc;
		  COPY64(R[ra], iu64.ari64);
 		  DISASM2("RPCC\t%s\t0x%16x\n", RN[ra], rtc);
	  	  break;
	  	case 0xe:
 		  DISASM2("RC\t%s,%s\n", RN[ra], RN[rb]);
	  	  break;
	  	case 0xf:
 		  DISASM2("RS\t%s,%s\n", RN[ra], RN[rb]);
	  	  break;
	  	}
	  	break;

	  case 0x1a:
	  	switch(func>>9) {
	  	case 0:
	  	  DISASM2("JMP\t%s,%s\n",RN[ra],RN[rb]);
	  	  break;
	  	case 1:
	  	  DISASM2("JSR\t%s,%s\n",RN[ra],RN[rb]);
	  	  break;
	  	case 2:
	  	  DISASM2("RET\t%s,%s\n",RN[ra],RN[rb]);
	  	  break;
	  	case 3:
	  	  DISASM2("JSR_C\t,%s\n",RN[ra],RN[rb]);
	  	  break;
	  	}
	  	if(ra != zero) {
	  	  R[ra].part1 = R[ra].part2 = 0;
	  	  R[ra].part3 = pc>>16;
	  	  R[ra].part4 = pc&0xffff;
	  	}
	  	pc = ((R[rb].part3<<16)|R[rb].part4) &~ 3;
	  	if(pc == 0)		/* Check for end of simulation */
	  	  	return(status);
	  	break;

	  case 0x20:
	  	status = BADINSTR;
	  	DISASM3("LDF\t%s,0x%4.4x(%s)\n", RN[ra], p2, RN[rb]);
	  	break;

	  case 0x21:
	  	status = BADINSTR;
	  	DISASM3("LDG\t%s,0x%4.4x(%s)\n", RN[ra], p2, RN[rb]);
	  	break;

	  case 0x22:
	  	if(ra != fzero) {
	  	  reg64.part4 = p2;
	  	  if(p2>>15)
	  	    reg64.part1 = reg64.part2 = reg64.part3 = 0xffff;
	  	  else
	  	    reg64.part1 = reg64.part2 = reg64.part3 = 0;
	  	  ADD64(reg64, reg64, R[rb]);
	  	  offset = (reg64.part3<<16) | reg64.part4;
	  	  if(offset & 3)
	  	  	status = BADINSTR;
	  	  else {
	  	  	load_mpp_word(&reg64, reg64.part2&0xff, offset>>3);
	  	  	if(offset & 4) {
	  	  		int64.part3 = reg64.part3;
	  	  		int64.part4 = reg64.part4;
	  	  	}
	  	  	else {
	  	  		int64.part3 = reg64.part1;
	  	  		int64.part4 = reg64.part2;
	  	  	}
	  	  }
	  	  ar_convert_to_float((ar_data*)&F[ra], &ieee_float_64,
	  		   (ar_data*)&int64, &ieee_float_32);
	  	}
	  	DISASM5("LDS\t%s,0x%4.4x(%s)\t0x%8.8x %8.8x\n",
	  		FN[ra], p2, RN[rb],
	  		(F[ra].part1<<16) | F[ra].part2,
	  		(F[ra].part3<<16) | F[ra].part4);
	  	break;

	  case 0x23:
	  	if(ra != fzero) {
	  	  reg64.part4 = p2;
	  	  if(p2>>15)
	  	    reg64.part1 = reg64.part2 = reg64.part3 = 0xffff;
	  	  else
	  	    reg64.part1 = reg64.part2 = reg64.part3 = 0;
	  	  ADD64(reg64, reg64, R[rb]);
	  	  offset = (reg64.part3<<16) | reg64.part4;
	  	  if(offset & 7)
	  		status = BADINSTR;
	  	  else
	  		load_mpp_word(&F[ra], reg64.part2&0xff, offset>>3);
	  	}
	  	DISASM5("LDT\t%s,0x%4.4x(%s)\t0x%8.8x %8.8x\n",
	  		FN[ra], p2, RN[rb],
	  		(F[ra].part1<<16) | F[ra].part2,
	  		(F[ra].part3<<16) | F[ra].part4);
	  	break;

	  case 0x24:
	  	status = BADINSTR;
	  	DISASM3("STF\t%s,0x%4.4x(%s)\n", FN[ra], p2, RN[rb]);
	  	break;

	  case 0x25:
	  	status = BADINSTR;
	  	DISASM3("STG\t%s,0x%4.4x(%s)\n", FN[ra], p2, RN[rb]);
	  	break;

	  case 0x26:
	  	reg64.part4 = p2;
	  	if(p2>>15)
	  	  reg64.part1 = reg64.part2 = reg64.part3 = 0xffff;
	  	else
	  	  reg64.part1 = reg64.part2 = reg64.part3 = 0;
	  	ADD64(reg64, reg64, R[rb]);
	  	offset = (reg64.part3<<16) | reg64.part4;
	  	if(offset & 3)
	  		status = BADINSTR;
	  	else {
	  		n = reg64.part2&0xff;
	  		load_mpp_word(&reg64, n, offset>>3);
	  		ar_convert_to_float((ar_data*)&int64, &ieee_float_32,
	  			   (ar_data*)&F[ra], &ieee_float_64);
	  		if(offset & 4) {
	  			reg64.part3 = int64.part3;
	  			reg64.part4 = int64.part4;
	  		}
	  		else {
	  			reg64.part1 = int64.part3;
	  			reg64.part2 = int64.part4;
	  		}
	  		store_mpp_word(n, offset>>3, &reg64);
	  	}
	  	DISASM3("STS\t%s,0x%4.4x(%s)\n", FN[ra], p2, RN[rb]);
	  	break;

	  case 0x27:
	  	reg64.part4 = p2;
	  	if(p2>>15)
	  	  reg64.part1 = reg64.part2 = reg64.part3 = 0xffff;
	  	else
	  	  reg64.part1 = reg64.part2 = reg64.part3 = 0;
	  	ADD64(reg64, reg64, R[rb]);
	  	offset = (reg64.part3<<16) | reg64.part4;
	  	if(offset & 7)
	  		status = BADINSTR;
	  	else {
	  		store_mpp_word(reg64.part2&0xff, offset>>3, &F[ra]);
		}
	  	DISASM3("STT\t%s,0x%4.4x(%s)\n", FN[ra], p2, RN[rb]);
	  	break;

	  case 0x28:
	  	if(ra != zero) {
	  	  reg64.part4 = p2;
	  	  if(p2>>15)
	  	    reg64.part1 = reg64.part2 = reg64.part3 = 0xffff;
	  	  else
	  	    reg64.part1 = reg64.part2 = reg64.part3 = 0;
	  	  ADD64(reg64, reg64, R[rb]);
	  	  offset = (reg64.part3<<16) | reg64.part4;
	  	  if(offset & 3)
	  		status = BADINSTR;
	  	  else {
	  		load_mpp_word(&reg64, reg64.part2&0xff, offset>>3);
	  		if(offset & 4) {
	  			R[ra].part3 = reg64.part3;
	  			R[ra].part4 = reg64.part4;
	  		}
	  		else {
	  			R[ra].part3 = reg64.part1;
	  			R[ra].part4 = reg64.part2;
	  		}
	  	  }
	  	  if(R[ra].part3>>15)
	  	    R[ra].part1 = R[ra].part2 = 0xffff;
	  	  else
	  	    R[ra].part1 = R[ra].part2 = 0;
	  	}
	  	DISASM5("LDL\t%s,0x%4.4x(%s)\t0x%8.8x %8.8x\n",
	  		RN[ra], p2, RN[rb],
	  		(R[ra].part1<<16) | R[ra].part2,
	  		(R[ra].part3<<16) | R[ra].part4);
	  	break;

	  case 0x29:
	  	if(ra != zero) {
	  	  reg64.part4 = p2;
	  	  if(p2>>15)
	  	    reg64.part1 = reg64.part2 = reg64.part3 = 0xffff;
	  	  else
	  	    reg64.part1 = reg64.part2 = reg64.part3 = 0;
	  	  ADD64(reg64, reg64, R[rb]);
	  	  offset = (reg64.part3<<16) | reg64.part4;
	  	  if(offset & 7)
	  		status = BADINSTR;
	  	  else
	  		load_mpp_word(&R[ra], reg64.part2&0xff, offset>>3);
	  	}
	  	DISASM5("LDQ\t%s,0x%4.4x(%s)\t0x%8.8x %8.8x\n",
	  		RN[ra], p2, RN[rb],
	  		(R[ra].part1<<16) | R[ra].part2,
	  		(R[ra].part3<<16) | R[ra].part4);
	  	break;

	  case 0x2a:
	  	status = BADINSTR;
	  	DISASM3("LDL_L\t%s,0x%4.4x(%s)\n", RN[ra], p2, RN[rb]);
	  	break;

	  case 0x2b:
	  	status = BADINSTR;
	  	DISASM3("LDQ_L\t%s,0x%4.4x(%s)\n", RN[ra], p2, RN[rb]);
	  	break;

	  case 0x2c:
	  	reg64.part4 = p2;
	  	if(p2>>15)
	  	  reg64.part1 = reg64.part2 = reg64.part3 = 0xffff;
	  	else
	  	  reg64.part1 = reg64.part2 = reg64.part3 = 0;
	  	ADD64(reg64, reg64, R[rb]);
	  	offset = (reg64.part3<<16) | reg64.part4;
	  	if(offset & 3)
	  		status = BADINSTR;
	  	else {
	  		n = reg64.part2&0xff;
	  		load_mpp_word(&reg64, n, offset>>3);
	  		if(offset & 4) {
	  			reg64.part3 = R[ra].part3;
	  			reg64.part4 = R[ra].part4;
	  		}
	  		else {
	  			reg64.part1 = R[ra].part3;
	  			reg64.part2 = R[ra].part4;
	  		}
	  		store_mpp_word(n, offset>>3, &reg64);
	  	}
	  	DISASM3("STL\t%s,0x%4.4x(%s)\n", RN[ra], p2, RN[rb]);
	  	break;

	  case 0x2d:
	  	reg64.part4 = p2;
	  	if(p2>>15)
	  	  reg64.part1 = reg64.part2 = reg64.part3 = 0xffff;
	  	else
	  	  reg64.part1 = reg64.part2 = reg64.part3 = 0;
	  	ADD64(reg64, reg64, R[rb]);
	  	offset = (reg64.part3<<16) | reg64.part4;
	  	if(offset & 7)
	  		status = BADINSTR;
	  	else {
	  		store_mpp_word(reg64.part2&0xff, offset>>3, &R[ra]);
		}
	  	DISASM3("STQ\t%s,0x%4.4x(%s)\n", RN[ra], p2, RN[rb]);
	  	break;

	  case 0x2e:
	  	status = BADINSTR;
	  	DISASM3("STL_C\t%s,0x%4.4x(%s)\n", RN[ra], p2, RN[rb]);
	  	break;

	  case 0x2f:
	  	status = BADINSTR;
	  	DISASM3("STQ_C\t%s,0x%4.4x(%s)\n", RN[ra], p2, RN[rb]);
	  	break;

	  case 0x30:
	  case 0x34:
	  	disp = ((rb<<16)|p2)<<2;
	  	if(disp>>22)
	  	  disp |= (-1<<23);
	  	if(ra != zero) {
	  	  R[ra].part1 = R[ra].part2 = 0;
	  	  R[ra].part3 = pc>>16;
	  	  R[ra].part4 = pc&0xffff;
	  	}
	  	pc += disp;
	  	if(op == 0x30) {
	  		DISASM2("BR\t%s,0x%6.6x\n", RN[ra], (rb<<16)|p2);
	  	}
	  	else {
	  		DISASM2("BSR\t%s,0x%6.6x\n", RN[ra], (rb<<16)|p2);
	  	}
		if(pc == 0)		/* Check for end of simulation */
			return(status);
	  	break;

	  case 0x31:
	  	if(!((F[ra].part1&0x7fff)|F[ra].part2|F[ra].part3|F[ra].part4)) {
	  	  disp = ((rb<<16)|p2)<<2;
	  	  if(disp>>22)
	  	    disp |= (-1<<23);
		  pc += disp;
		}
	  	DISASM2("FBEQ\t%s,0x%6.6x\n", FN[ra], (rb<<16)|p2);
	  	break;

	  case 0x32:
	  	if(F[ra].part1>>15) {
	  	  disp = ((rb<<16)|p2)<<2;
	  	  if(disp>>22)
	  	    disp |= (-1<<23);
		  pc += disp;
		}
	  	DISASM2("FBLT\t%s,0x%6.6x\n", FN[ra], (rb<<16)|p2);
	  	break;

	  case 0x33:
	  	if((F[ra].part1>>15) ||
	  	   (F[ra].part1|F[ra].part2|F[ra].part3|F[ra].part4)==0) {
	  	  disp = ((rb<<16)|p2)<<2;
	  	  if(disp>>22)
	  	    disp |= (-1<<23);
		  pc += disp;
		}
	  	DISASM2("FBLE\t%s,0x%6.6x\n", FN[ra], (rb<<16)|p2);
	  	break;

	  case 0x35:
	  	if((F[ra].part1&0x7fff)|F[ra].part2|F[ra].part3|F[ra].part4) {
	  	  disp = ((rb<<16)|p2)<<2;
	  	  if(disp>>22)
	  	    disp |= (-1<<23);
		  pc += disp;
		}
	  	DISASM2("FBNE\t%s,0x%6.6x\n", FN[ra], (rb<<16)|p2);
	  	break;

	  case 0x36:
	  	if((F[ra].part1>>15) == 0) {
	  	  disp = ((rb<<16)|p2)<<2;
	  	  if(disp>>22)
	  	    disp |= (-1<<23);
		  pc += disp;
		}
	  	DISASM2("FBGE\t%s,0x%6.6x\n", FN[ra], (rb<<16)|p2);
	  	break;

	  case 0x37:
	  	if((F[ra].part1>>15)==0 &&
	  	   (F[ra].part1|F[ra].part2|F[ra].part3|F[ra].part4)!=0) {
	  	  disp = ((rb<<16)|p2)<<2;
	  	  if(disp>>22)
	  	    disp |= (-1<<23);
		  pc += disp;
		}
	  	DISASM2("FBGT\t%s,0x%6.6x\n", FN[ra], (rb<<16)|p2);
	  	break;

	  case 0x38:
	  	if((R[ra].part4&1) == 0) {
	  	  disp = ((rb<<16)|p2)<<2;
	  	  if(disp>>22)
	  	    disp |= (-1<<23);
		  pc += disp;
		}
	  	DISASM2("BLBC\t%s,0x%6.6x\n", RN[ra], (rb<<16)|p2);
	  	break;

	  case 0x39:
	  	if((R[ra].part1|R[ra].part2|R[ra].part3|R[ra].part4) == 0) {
	  	  disp = ((rb<<16)|p2)<<2;
	  	  if(disp>>22)
	  	    disp |= (-1<<23);
		  pc += disp;
		}
	  	DISASM2("BEQ\t%s,0x%6.6x\n", RN[ra], (rb<<16)|p2);
	  	break;

	  case 0x3a:
	  	if(R[ra].part1>>15) {
	  	  disp = ((rb<<16)|p2)<<2;
	  	  if(disp>>22)
	  	    disp |= (-1<<23);
		  pc += disp;
		}
	  	DISASM2("BLT\t%s,0x%6.6x\n", RN[ra], (rb<<16)|p2);
	  	break;

	  case 0x3b:
	  	if((R[ra].part1>>15) ||
	  	   (R[ra].part1|R[ra].part2|R[ra].part3|R[ra].part4)==0) {
	  	  disp = ((rb<<16)|p2)<<2;
	  	  if(disp>>22)
	  	    disp |= (-1<<23);
		  pc += disp;
		}
	  	DISASM2("BLE\t%s,0x%6.6x\n", RN[ra], (rb<<16)|p2);
	  	break;

	  case 0x3c:
	  	if(R[ra].part4&1) {
	  	  disp = ((rb<<16)|p2)<<2;
	  	  if(disp>>22)
	  	    disp |= (-1<<23);
		  pc += disp;
		}
	  	DISASM2("BLBS\t%s,0x%6.6x\n", RN[ra], (rb<<16)|p2);
	  	break;

	  case 0x3d:
	  	if((R[ra].part1|R[ra].part2|R[ra].part3|R[ra].part4) != 0) {
	  	  disp = ((rb<<16)|p2)<<2;
	  	  if(disp>>22)
	  	    disp |= (-1<<23);
		  pc += disp;
		}
	  	DISASM2("BNE\t%s,0x%6.6x\n", RN[ra], (rb<<16)|p2);
	  	break;

	  case 0x3e:
	  	if((R[ra].part1>>15) == 0) {
	  	  disp = ((rb<<16)|p2)<<2;
	  	  if(disp>>22)
	  	    disp |= (-1<<23);
		  pc += disp;
		}
	  	DISASM2("BGE\t%s,0x%6.6x\n", RN[ra], (rb<<16)|p2);
	  	break;

	  case 0x3f:
	  	if((R[ra].part1>>15)==0 &&
	  	   (R[ra].part1|R[ra].part2|R[ra].part3|R[ra].part4)!=0) {
	  	  disp = ((rb<<16)|p2)<<2;
	  	  if(disp>>22)
	  	    disp |= (-1<<23);
		  pc += disp;
		}
	  	DISASM2("BGT\t%s,0x%6.6x\n", RN[ra], (rb<<16)|p2);
	  	break;

	  default:
		status = BADINSTR;
		break;
	  }

	}

	if(status == BADINSTR)		/* Unsimulated instruction reached */
		ar_internal_error(2013, __FILE__, __LINE__);

	return status;
}

#define T3D_AHDR_SIZE	174*8	/* # bytes in T3D a.out file headers */
#define CLD_T3E_AHDR_SIZE	512*8	/* # bytes in T3E a.out file headers */

#if T3D_AHDR_SIZE < 5*MPP_MAX_ARGS*8
#error Insufficient space for T3D arguments
#elif CLD_T3E_AHDR_SIZE < 5*MPP_MAX_ARGS*8
#error Insufficient space for T3E arguments
#endif

static int	idf_fd;		/* Intrinsic data file file descriptor */
static long	idf_size;	/* Intrinsic data file size (in bytes) */
static long	text_size;	/* Size of text space loaded into memory */
static long	data_offset;	/* Offset in data file to data section */

static void
open_intrinsics_file(void)
{
	char	fname[FILENAME_MAX + 1];

	int	i;
	int	fd;
	int hdrsz;
	int	v;

	long	offset;

	char*	str;

	/* Find and open intrinsic evaluation data file */

    /* Use the arith data file from the CRAYLIBS path if it is set */

	str=(char*)getenv("CRAYLIBS");
	if(str != NULL) {
		strcpy(fname,str);
		v=strlen(fname);
		strcpy(&fname[v],"/arith");
		idf_fd=open(fname, O_RDONLY);
	}

    /* Use a default arith data file */

	else {
#if _CRAY
		strcpy(fname,"/opt/ctl/craylibs_m/craylibs_m/arith");
		v = 29;
		idf_fd=open(fname, O_RDONLY);
		if(idf_fd < 0) {
			 strcpy(fname,"/mpp/lib/arith");
			 v = 8;
			 idf_fd=open(fname, O_RDONLY);
		}
#else
		idf_fd = -1;
#endif
	}
	if(idf_fd < 0)
		ar_internal_error(2009, __FILE__, __LINE__);

	/* Load intrinsic evaluation data file into heap memory */

	idf_size=lseek(idf_fd,0,SEEK_END);
	if(idf_size > 0) {
		/*
		 * For total space, we need enough for the arith data file
		 * minus its header, plus MPP_STACK_SIZE words of stack,
		 * plus room for a fake DSIB, plus MPP_MAX_ARGS*5 words
		 * of fake callee stack space.   Some of this will end up
		 * being wasted, since the header gets thrown away, but
		 * that's alright.
		 */
		mpp_intrin=(unsigned char*)malloc(idf_size +
										  MPP_STACK_SIZE*8 +
										  6*8 +
										  MPP_MAX_ARGS*5*8);
		if(mpp_intrin == NULL)
			idf_size = -1;
		else {
			lseek(idf_fd,0,SEEK_SET);
			read(idf_fd,mpp_intrin,idf_size);

			/* Skip over T3D header string if it exists */
			i = (strcmp(mpp_intrin,"#!/mpp/bin/mppexec\n") == 0)? 32 : 0;
			if (i == 0) {
				/*
				 * Lack of the header string indicates this is a new style
				 * T3E data file (ie. one produced by cld).  mppldr puts
				 * the header string in for T3E object files, cld does not.
				 * The cld T3E headers are different than mppldr T3E headers.
				 * Basically, T3E headers from mppldr, and T3D headers from
				 * cld and mppldr are identical.  So, the header size is
				 * based on this.
				 */
				hdrsz = CLD_T3E_AHDR_SIZE;
			}
			else {
				hdrsz = T3D_AHDR_SIZE;
			}

			/* Determine if this is a valid T3D or a T3E arith data file */
			ar_mach_type = mpp_intrin[i+5];  /* t3x_exec_t.u_mag_t.pmt */

			/*
			 * MPP code, stack, and data segment identifiers from
			 * mpp_intrin[i+3].
			 */
			if ((ar_mach_type == T3D || ar_mach_type == T3E) &&
				mpp_intrin[i+6] == 1 && mpp_intrin[i+7] == 07) {
				/* magic = 0407? */
				if(ar_mach_type == T3D) {
					ar_sim_version = 0;
				}
				else {
					ar_sim_version = 1;
				}
				i += 64;
				while(i < hdrsz) {
					offset = (mpp_intrin[i+36]<<24) | (mpp_intrin[i+37]<<16) |
							 (mpp_intrin[i+38]<<8)  | (mpp_intrin[i+39]);
					/*
					 * Get the stack, code, and data segments using the
					 * segment type field of t3x_segment_t struct.
					 */
					switch (mpp_intrin[i+16] >> 4) {
					case SEGT_TEXT:
						if (code == NULL && mpp_intrin[i+3]) {
							code_segment_id = mpp_intrin[i+3];
							if (mpp_intrin[i+16] == 0x15 && offset==hdrsz) 
								code = mpp_intrin;
							else
								hdrsz = 0;
						}
						break;

					case SEGT_STACK:
						if (stack == NULL && mpp_intrin[i+3]) {
							stack_segment_id = mpp_intrin[i+3];
							text_size = idf_size-hdrsz;
							stack = mpp_intrin + (((text_size+7)>>3)<<3);
						}
					  break;

					case SEGT_DATA:
						if (data == NULL && mpp_intrin[i+3]) {
							data_segment_id = mpp_intrin[i+3];
							if (mpp_intrin[i+16] == 0x06)
								data = mpp_intrin+offset-hdrsz;
							else
								hdrsz = 0;
						}
						break;
					}

					/* add a.out header sizeof info here, ghg! */
					i += 10*8; /* 10 words in the t3x_segment_t struct */
				}

				/* do copy down of text over a.out header */
				if (stack != NULL) {
				  memcpy(mpp_intrin, mpp_intrin+hdrsz, text_size);
				}
			}
		}
	}

	close(idf_fd);

	/* Validate intrinsic evaluation data file contents and version info */

	if (code==NULL || data==NULL || stack==NULL)
		ar_internal_error(2010, __FILE__, __LINE__);

	i=text_size-1;
	str = (char*)strnstrn(mpp_intrin, text_size,
						  "@(#)MPP arith/libm_version ", 27);
	if(str != NULL)
		strcpy(AR_libmv2, str+14);
	else {
		while(idf_size>0 && (text_size-i)<32) {
			if(strncmp(&mpp_intrin[i], "libm_version", 12) == 0) {
				strncpy(AR_libmv2, &mpp_intrin[i], text_size-i);
				AR_libmv2[text_size-i] = '\0';
				if(strncmp(&mpp_intrin[i-13], "MPP arith", 9))
					idf_size=-1;
				break;
			}
			if(mpp_intrin[i] == '\n')
				mpp_intrin[i]='\0';
			i--;
		}
	}
	if(idf_size<0 || (text_size-i)>=32)
		ar_internal_error(2010, __FILE__, __LINE__);

	/*
	 * Validate probable libm.a version that will be used for loading.
	 *
	 * We gripe about the version number iff we can find libm_version.o
	 * in an archive, and we can find the version string, and it does
	 * not match.  If we can't find a libm.a, or if we have any trouble
	 * traversing it, or we don't find libm_version.o in it, or we don't
	 * find the version string, then we don't gripe.
	 */
	if (AR_libmv2[2] != 'x' && AR_libmv2[13+2] != 'x') {
		int				matched;		/* Version number matched?       */
		char			buf				/* To check ar(1) file magic str */
						[FILENAME_MAX];
		struct ar_hdr	arh;			/* An ar(1) archive header	     */
		off_t			fd_off;			/* Offset in archive file        */
		off_t			fd_fsize;		/* Size of an archive member     */
		off_t			fd_namelen;		/* Len of an archive member name */
		char		   *name_ptr;		/* Ptr to an archive member name */

		/*
		 * Open the libm archive that matches this arith data file.
		 */
		strcpy(&fname[v], "/libm.a");
		fd = open(fname, O_RDONLY);
		if (fd < 0) {
			strcpy(&fname[v], "/../libm.a");
			fd = open(fname, O_RDONLY);
		}

		/*
		 * If this is an ar(1) archive, then traverse it, looking
		 * for the "libm_version.o" file.
		 */
		if (fd >= 0 &&
			read(fd, buf, SARMAG) == SARMAG &&
			strncmp(buf, ARMAG, SARMAG) == 0) {

			matched = 1;
			fd_off = SARMAG;

			while (1) {
				/*
				 * If there are no more archive members, we're done.
				 */
				if (read(fd, &arh, AR_HDR_SIZE) != AR_HDR_SIZE) {
					fd_off = 0;
					break;
				}

				/*
				 * Check this archive filename.  If we've found the
				 * "libm_version.o" file, stop looking; otherwise,
				 * skip this and go on to the next one.
				 */
				if (strncmp(arh.ar_name, "#1/", 3) != 0) {
					name_ptr = arh.ar_name;
				}
				else {
					if (sscanf(&arh.ar_name[3], "%d", &fd_namelen) != 1 ||
						read(fd, buf, fd_namelen) != fd_namelen) {
						fd_off = 0;
						break;
					}
					name_ptr = buf;
				}

				if (strncmp(name_ptr, "libm_version.o", 14) == 0) {
					break;
				}

				if (sscanf(arh.ar_size, "%d", &fd_fsize) != 1) {
					fd_off = 0;
					break;
				}

				fd_off = (fd_off + AR_HDR_SIZE + fd_fsize + 1) & (~1);
				if (lseek(idf_fd, fd_off, SEEK_SET) != fd_off) {
					fd_off = 0;
					break;
				}
			}

			/*
			 * If we've found the "libm_version.o" file, then search
			 * in it for the version string.  It has to match, or we
			 * gripe, but it may be wildcarded with 'x' characters.
			 */
			if (fd_off != 0) {
				if (sscanf(arh.ar_size, "%d", &fd_fsize) != 1) {
					fd_off = 0;
				}
			}

			if (fd_off != 0) {
				if (strncmp(arh.ar_name, "#1/", 3) == 0) {
					fd_fsize -= fd_namelen;
				}

				if (fd_fsize > sizeof(buf)) {
					fd_fsize = sizeof(buf);
				}

				if (read(fd, buf, fd_fsize) == fd_fsize) {
					str = (char*)strnstrn(buf, (fd_fsize - 9),
										  "@(#)libm_version", 16);
					if (str != NULL) {
						str += 17;
						for (i = 0; matched && i < 8; i += 2)
						  if (str[i] != 'x' &&
							  AR_libmv2[13 + i] != 'x' &&
							  str[i] != AR_libmv2[13 + i])
							  matched = 0;

					}
				}
			}

			if (!matched) {
				PRINTMSG(0, 2016, Warning, 0, "MPP", "0", "", "");
			}
		}
	}

	ar_state_register.ar_float_format = AR_IEEE_FLOATING_POINT;
	ar_state_register.ar_128bit_format = AR_128BIT_EXTENDED_DOUBLE;
	ar_state_register.ar_rounding_mode = AR_ROUND_NEAREST;
	ar_state_register.ar_underflow_mode = AR_UNDERFLOW_TO_PLUS_ZERO;
}

static void
load_mpp_word(result, segment, offset)
AR_INT_64*	result;
long		segment;
long		offset;
{

	if (segment == data_segment_id) {
		memcpy(result, data+offset*8, 8);
		return;
	}

	else if (segment == stack_segment_id) {
		if(offset < (MPP_STACK_SIZE+MPP_MAX_ARGS*5+6)) {
			memcpy(result, stack+offset*8, 8);
			return;
		}
	}

	else {
		if(segment>=0 && segment < n_external_addresses &&
		   external_address[segment] != NULL) {
		    if(external_length[segment] > offset) {
				memcpy(result, external_address[segment]+offset*8, 8);
				return;
		    }
		    /* Allow up to 8 words of unusable slop to be loaded */
		    /* See first vector load in memchr.s where this is needed */
		    if(external_length[segment] > (offset-8)) {
				result->part1 = result->part2 = 0xf0f0;
				result->part3 = result->part4 = 0xf0f0;
				return;
		    }
		}
	}

	ar_internal_error(2012, __FILE__, __LINE__);
}

static void
store_mpp_word(segment, offset, result)
long		segment;
long		offset;
AR_INT_64*	result;
{

	if (segment == data_segment_id) {
		memcpy(data+offset*8, result, 8);
		return;
	}

	else if (segment == stack_segment_id) {
		if(offset < (MPP_STACK_SIZE+MPP_MAX_ARGS*5+6)) {
			memcpy(stack+offset*8, result, 8);
			return;
		}
	}

	else {
		if(segment < n_external_addresses &&
		   external_address[segment] != NULL) {
		    if(external_length[segment] >= offset) {
				memcpy(external_address[segment]+offset*8, result, 8);
				return;
		    }
		    /* Allow ORE (without actual store) iff value matches that
		     * returned by load_pvp_word above.
		     */
		    if(external_length[segment] > (offset-8) &&
		       result->part1==0xf0f0 && result->part2==0xf0f0 &&
		       result->part3==0xf0f0 && result->part4==0xf0f0) {
			return;
		    }
		}
	}

	ar_internal_error(2012, __FILE__, __LINE__);
}

#endif

static char USMID [] = "\n%Z%%M%	%I%	%G% %U%\n";
static char rcsid [] = "$Id: mpp_sim.c,v 1.1.1.1 2005/10/21 19:00:00 marcel Exp $";
