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

static const char USMID[] = "@(#)30/cray1_sim.c	30.0	03/18/98 12:06:00";


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#include <errno.h>
#include <sys/types.h>
#include <unistd.h>

/************************* Cray Intrinsic Simulator **********************/

/* Simulate Cray library intrinsics if this module is loaded */

/* Primary machine types (Triton_IEEE = T90 + ar_float_format == IEEE) */

#define YMP		7
#define C90		8
#define	T3D		10
#define	T90		11
#define	T3E		12

#include "arith.internal.h"
#include "int64.h"

#if !defined(__mips)

int ar_mach_type = 0;
int ar_rounding_modes = 0;
int ar_underflow_modes = 0;

extern char AR_libmv2[];
extern char AR_version[];

#define CRAY_FLOAT_64		(UNROUNDED_TYPE(AR_Float_Cray1_64))
#define CRAY_FLOAT_128		(UNROUNDED_TYPE(AR_Float_Cray1_128))
#define CRAY_COMPLEX_64 	(UNROUNDED_TYPE(AR_Complex_Cray1_64))
#define CRAY_COMPLEX_128	(UNROUNDED_TYPE(AR_Complex_Cray1_128))

#define IEEE_FLOAT_64		(UNROUNDED_TYPE(AR_Float_IEEE_NR_64))
#define IEEE_FLOAT_128		(UNROUNDED_TYPE(AR_Float_IEEE_NR_128))
#define IEEE_COMPLEX_64 	(UNROUNDED_TYPE(AR_Complex_IEEE_NR_64))
#define IEEE_COMPLEX_128	(UNROUNDED_TYPE(AR_Complex_IEEE_NR_128))

static AR_TYPE integer_64_s		= AR_Int_64_S;
static AR_TYPE integer_64_u		= AR_Int_64_U;
static AR_TYPE integer_128_s	= AR_Int_128_S;
static AR_TYPE integer_128_u	= AR_Int_128_U;

static AR_TYPE cray_float_64	= (AR_TYPE) CRAY_FLOAT_64;
static AR_TYPE cray_float_128	= (AR_TYPE) CRAY_FLOAT_128;
static AR_TYPE cray_complex_64	= (AR_TYPE) CRAY_COMPLEX_64;
static AR_TYPE cray_complex_128 = (AR_TYPE) CRAY_COMPLEX_128;

static AR_TYPE ieee_float_64	= (AR_TYPE) IEEE_FLOAT_64;
static AR_TYPE ieee_float_128	= (AR_TYPE) IEEE_FLOAT_128;
static AR_TYPE ieee_complex_64	= (AR_TYPE) IEEE_COMPLEX_64;
static AR_TYPE ieee_complex_128 = (AR_TYPE) IEEE_COMPLEX_128;

#if _CRAY
typedef unsigned long int64;
#else
typedef unsigned long long int64;
#endif

static int ar_sim_version = 0;		/* Simulation interface version id */

#define DATA			0			/* Implicit data segment identifier */
#define STACK			1			/* Stack segment identifier */

#define STACK_SIZE		20480		/* # words to allocate for stack space */

#define MAX_ARGS		8			/* Max # of arguments supported */
#define MAX_EXT_ADDRS	16			/* Max # of EXT addresses passed */

static unsigned char*	code=NULL;
static int64*			stack=NULL;

/* Define machine register and size variables */

static int		JSZP;				/* # parcels in a jump instruction */
static int		VSZ;				/* Max # words in vector register */

static int64	AMASK;				/* A-register size mask */
static int64	ASB;				/* A-register sign bit mask */

static int64	A[8];
static int64	S[8];

static int64	B[64];
static int64	T[64];

static int		SR0;				/* Status/mode Register 0 */

static int		VL; 				/* Current vector length register */
static int64	VM;
static int64	VM1;
static int64*	V;					/* V-registers allocated later */

/* Intrinsic function interface variables */

static int		numargs;
static int		numargwds;
static int		n_external_addresses;	/* Must be > 0 to be special */
static char*	external_address[MAX_EXT_ADDRS];
static long 	external_length[MAX_EXT_ADDRS];

/* Internal intrinsic-specified modes */

#define RMMASK			6			/* Masks RM[01] bits in SR0 */

#define INTERRUPT_INV	8
#define INTERRUPT_DIV	16
#define INTERRUPT_OVF	32
#define INTERRUPT_UNF	64
#define INTERRUPT_INX	128
#define INTERRUPT_INP	256

/* Debugging and disassembly variables */

#ifdef DEBUG
int ar_disasm    = 0;	/* =1 turns on disassembly output */
int ar_disasm_BT = 0;	/* =1 turns on B/T load display output */
int ar_disasm_V  = 0;	/* =1 turns on vector results display output */

#define DISASM0(fmt)    if(ar_disasm) fprintf(stderr,fmt); else
#define DISASM1(fmt,u)  if(ar_disasm) fprintf(stderr,fmt,u); else
#define DISASM2(fmt,u,v)  \
                        if(ar_disasm) fprintf(stderr,fmt,u,v); else
#define DISASM3(fmt,u,v,w)  \
                        if(ar_disasm) fprintf(stderr,fmt,u,v,w); else
#define DISASM4(fmt,u,v,w,x)  \
                        if(ar_disasm) fprintf(stderr,fmt,u,v,w,x); else
#define DISASM5(fmt,u,v,w,x,y)  \
                        if(ar_disasm) fprintf(stderr,fmt,u,v,w,x,y); else
#define DISASMB(b,c,ft) if (ar_disasm) {                                \
                            if (ar_disasm_BT) {                         \
                                for(i=0; i<(c); i++)                    \
                                    DISASM2("%s0x%*.*llx\n",            \
                                            ((i==0)?ft:"\t\t\t\t\t"),   \
                                            BVAL(((b)+i)&077));         \
                                if(i==0)                                \
                                    DISASM0("\n");                      \
                            } else {                                    \
                                DISASM0("\n");                          \
                            }                                           \
                        } else
#define DISASMT(t,c,ft) if (ar_disasm) {                                \
                            if (ar_disasm_BT) {                         \
                                for(i=0; i<(c); i++)                    \
                                    DISASM2("%s0x%16.16llx\n",          \
                                            ((i==0)?ft:"\t\t\t\t\t"),   \
                                            T[((t)+i)&077]);            \
                                if(i==0)                                \
                                    DISASM0("\n");                      \
                            } else {                                    \
                                DISASM0("\n");                          \
                            }                                           \
                        } else
#define DISASMV(v,ft)   if (ar_disasm) {                                \
                            if (ar_disasm_V) {                          \
                                for(i=0; i<VL; i++)                     \
                                    DISASM2("%s0x%16.16llx\n",          \
                                            ((i==0)?ft:"\t\t\t\t\t"),   \
                                            V[v*VSZ+i]);                \
                                if(VL==0)                               \
                                    DISASM0("\n");                      \
                            } else {                                    \
                                DISASM0("\n");                          \
                            }                                           \
                        } else

static int a_size;
#define set_a_size(s)   a_size = (s) >> 2
#define AVAL(i)         a_size, a_size, A[i]
#define BVAL(jk)        a_size, a_size, B[jk]

#else

#define DISASM0(fmt)
#define DISASM1(fmt,x)
#define DISASM2(fmt,x,y)
#define DISASM3(fmt,x,y,z)
#define DISASM4(fmt,x,y,z,v)
#define DISASM5(fmt,x,y,z,v,w)
#define DISASMB(b,c,ft)
#define DISASMT(t,c,ft)
#define DISASMV(v,ft)

#define set_a_size(s)

#endif

/* Internal prototype function specifications */

static void 	open_arith_file();
static int64	load_pvp_word(long vaddr);
static void 	store_pvp_word(long vaddr, int64 word);

int
ar_clear_sim_state(AR_TYPE resulttype)
{

#ifdef DEBUG
	if (getenv("AR_DISASM") != NULL) {
		ar_disasm = 1;
	}
	if (getenv("AR_DISASM_BT") != NULL) {
		ar_disasm_BT = 1;
	}
	if (getenv("AR_DISASM_V") != NULL) {
		ar_disasm_V = 1;
	}
#endif

	/* Load arith data file on first intrinsic called */

	if(code == NULL)
		open_arith_file();

	if(AR_CLASS(resulttype) == AR_CLASS_FLOAT) {
		if((AR_FLOAT_FORMAT(resulttype) == AR_FLOAT_CRAY &&
		    ar_state_register.ar_float_format != AR_CRAY_FLOATING_POINT) ||
		   (AR_FLOAT_FORMAT(resulttype) == AR_FLOAT_IEEE &&
		    ar_state_register.ar_float_format != AR_IEEE_FLOATING_POINT))
			ar_internal_error(2018, __FILE__, __LINE__);
	}

	numargwds = numargs = 0;		/* Set up next intrinsic simulation */
	n_external_addresses = 1;		/* Forget all external addresses */
	return AR_STAT_OK;
}

int
ar_ext_address(int64* intaddr, const char* extaddr, int length)
{
#if MAX_EXT_ADDRS >= DATA
	if(n_external_addresses == DATA)
		external_address[n_external_addresses++] = NULL;
#if MAX_EXT_ADDRS >= STACK
	if(n_external_addresses == STACK)
		external_address[n_external_addresses++] = NULL;
#endif
#endif
	*intaddr = n_external_addresses<<24;

	if(n_external_addresses >= MAX_EXT_ADDRS)
		return AR_STAT_UNDEFINED;

	external_address[n_external_addresses] = (char*)extaddr;
	external_length[n_external_addresses++] = length;

	return AR_STAT_OK;
}

int
ar_pass_arg_address(const ar_data* arg, const AR_TYPE* argtype)
{
	int iarg;
	int nbytes;
	int arg_offset;
	int arg_value_offset;

	iarg = numargwds;
	numargs++;
	numargwds++;				/* Increment number of arguments */

	if(iarg >= MAX_ARGS)	/* Test for unsupported # of args */
		ar_internal_error(2014, __FILE__, __LINE__);

	/* Call-by-address must 1) copy the value of the argument into
	 * the address space known to the simulator and 2) store the address
	 * of this copied value into the argument list.  5*MAX_ARGS words
	 * are assumed to exist beyond the base of the stack for this purpose.
	 * These words emulate the argument setup in a caller's stack frame.
	 * The first MAX_ARGS are used to store arguments
	 * (addresses) that are not passed in registers.  Then 4 words are
	 * available to store the value of each argument.
	 */

	/*
	 * Handle special case of a NULL pointer (no value).  If not NULL,
	 * Compute address of argument value (to be copied in switch below).
	 */

	if(arg == NULL)
		S[0] = 0;
	else {
		arg_value_offset = (STACK_SIZE+1+MAX_ARGS+iarg*4);
		S[0] = (STACK<<24) | arg_value_offset;
	}

	/*
	 * Store argument address in the stack extension of the argument list.
	 */

	arg_offset = (STACK<<24) | (STACK_SIZE+1+iarg);
	store_pvp_word(arg_offset, S[0]);

	if(arg == NULL) 		/* If no value, simply return */
		return AR_STAT_OK;

	/* Copy the value of the argument to stack slot reserved for it */

	switch (UNROUNDED_TYPE(*argtype)) {

	case CRAY_FLOAT_64:
	case IEEE_FLOAT_64:
		nbytes = 8;
		break;

	case CRAY_FLOAT_128:
	case CRAY_COMPLEX_64:
	case IEEE_FLOAT_128:
	case IEEE_COMPLEX_64:
		nbytes = 16;
		break;

	case CRAY_COMPLEX_128:
	case IEEE_COMPLEX_128:
		nbytes = 32;
		break;

	default:
		switch (*argtype) {

		case AR_Int_46_S:
		case AR_Int_64_S:
		case AR_Logical:
			nbytes = 8;
			break;

		default:
			return AR_STAT_INVALID_TYPE;
		}
	}

	memcpy((char*)&stack[arg_value_offset], (char*)(arg), nbytes);
	return AR_STAT_OK;
}

int
ar_pass_ext_address(int64 *extdesc, const char *addr, int nwords)
{
	int iarg;
	int arg_offset;
	int arg_value_offset;

	int status;

	iarg = numargwds;
	numargs++;
	numargwds++;			/* Increment number of arguments */

	if(iarg >= MAX_ARGS)	/* Test for unsupported # of args */
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
		S[0] = 0;
	else {
		status = ar_ext_address(&S[0], addr, nwords);
		if(IS_ERROR_STATUS(status))
			return status;
	}

	if(extdesc != NULL)
		*extdesc = S[0];

	/*
	 * Store argument address in the stack extension of the argument list.
	 */

	arg_offset = (STACK<<24) | (STACK_SIZE+1+iarg);
	store_pvp_word(arg_offset, S[0]);

	return AR_STAT_OK;
}

int
ar_pass_fcd_address(const char* str, long lenstr)
{
	int iarg;
	int arg_offset;
	int arg_value_offset;

	int status;

	long	nbits;

	iarg = numargwds;
	numargs++;
	numargwds++;			/* Increment number of arguments */

	if(iarg >= MAX_ARGS)	/* Test for unsupported # of args */
		ar_internal_error(2014, __FILE__, __LINE__);

	if(str == NULL)
		return AR_STAT_UNDEFINED;

	/* Store external address descriptor in fcd */
	nbits = lenstr<<3;
	status = ar_ext_address(&S[0], str, (lenstr+7)>>3);
	if(IS_ERROR_STATUS(status))
		return status;

	/*
	 * Store fcd in the stack extension of the argument list.
	 */

	arg_offset = (STACK<<24) | (STACK_SIZE+1+iarg);
	if(ar_mach_type == T90) {
		/* Store C pointer and bit length in fcd */
		store_pvp_word(arg_offset, S[0]);
		S[0] = nbits;
		numargwds++;
		store_pvp_word(arg_offset+1, S[0]);
	}
	else {
		/* Store bit offset and item length in fcd */
		S[0] |= (int64)nbits<<32;
		store_pvp_word(arg_offset, S[0]);
	}

	return AR_STAT_OK;
}

int
ar_pass_arg_value(const ar_data* arg, const AR_TYPE* argtype)
{
	int iarg;
	int nbytes;
	int arg_offset;
	int status;

	iarg = numargwds;
	numargs++;
	numargwds++;			/* Increment number of arguments */

	if(iarg >= MAX_ARGS)	/* Test for unsupported # of args */
		ar_internal_error(2014, __FILE__, __LINE__);

	/* Call-by-value must copy the value of the argument into an
	 * argument register.
	 */

	if(arg == NULL) 		/* Test for NULL pointer (no value) */
		return AR_STAT_UNDEFINED;

	switch (UNROUNDED_TYPE(*argtype)) {

	case CRAY_FLOAT_64:
	case IEEE_FLOAT_64:
		nbytes =  8;
		break;

	case CRAY_FLOAT_128:
	case CRAY_COMPLEX_64:
	case IEEE_FLOAT_128:
	case IEEE_COMPLEX_64:
		numargwds++;
		nbytes =  16;
		break;

	case CRAY_COMPLEX_128:
	case IEEE_COMPLEX_128:
		numargwds += 3;
		nbytes =  32;
		break;

	default:
		switch (*argtype) {

		case AR_Int_46_S:
		case AR_Int_64_S:
		case AR_Logical:
			nbytes =  8;
			break;

		default:
			return AR_STAT_INVALID_TYPE;
		}
	}

	memcpy(&S[1+iarg], (char*)arg, nbytes);
	return AR_STAT_OK;
}


int
ar_put_real_address(AR_INT_64* extdesc)
{
	int segment = extdesc->part3>>8;

	if(segment >= n_external_addresses)
		return AR_STAT_UNDEFINED;

	extdesc->part3 = ((long)external_address[segment]) >> 16;
	extdesc->part4 = ((long)external_address[segment]) & 0xffff;

	return AR_STAT_OK;
}


int
ar_get_function_value(ar_data *result, AR_TYPE *resulttype)
{
	int status;

	/*
	 * Return function value in result if register conventions known
	 * for the result type.  Otherwise, the function value is assumed
	 * to be returned through memory and the caller must retrieve it.
	 * Use AR_convert to set ZERO and NEGATIVE status flags.  Note
	 * that error flags such as AR_STAT_OVERFLOW are cleared (these
	 * would have been returned by the intrinsic function simulation
	 * performed by ar_sim if run-time evaluation would have produced
	 * them).  So effectively, this routine only returns the bit
	 * pattern of the result plus zero or negative status.
	 */

	switch (AR_CLASS(*resulttype)) {

	case AR_CLASS_INT:

		status = ar_convert_to_integral(result, resulttype,
				  (ar_data*)&S[1], resulttype);
		break;

	case AR_CLASS_FLOAT:

		switch(UNROUNDED_TYPE(*resulttype)) {

		case CRAY_FLOAT_64:
			status = ar_convert_to_float(result, resulttype,
					  (ar_data*)&S[1], &cray_float_64);
			break;

		case CRAY_FLOAT_128:
			status = ar_convert_to_float(result, resulttype,
					  (ar_data*)&S[1], &cray_float_128);
			break;

		case CRAY_COMPLEX_64:
			status = ar_convert_to_complex(result, resulttype,
					  (ar_data*)&S[1], &cray_complex_64);
			break;

		case CRAY_COMPLEX_128:
			status = ar_convert_to_complex(result, resulttype,
					  (ar_data*)&S[1], &cray_complex_128);
			break;

		case IEEE_FLOAT_64:
			status = ar_convert_to_float(result, resulttype,
					  (ar_data*)&S[1], &ieee_float_64);
			break;

		case IEEE_FLOAT_128:
			status = ar_convert_to_float(result, resulttype,
					  (ar_data*)&S[1], &ieee_float_128);
			break;

		case IEEE_COMPLEX_64:
			status = ar_convert_to_complex(result, resulttype,
					  (ar_data*)&S[1], &ieee_complex_64);
			break;

		case IEEE_COMPLEX_128:
			status = ar_convert_to_complex(result, resulttype,
					  (ar_data*)&S[1], &ieee_complex_128);
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
	char*	ptr1, ptr2;

	int status;

	long	length;

	if(strncmp(func, "malloc", 6) == 0) {
		length = S[1];
		ptr1 = malloc(length);
		status = ar_ext_address(&S[1], (const char*)ptr1, (length+7)>>3);
		return status;
	}

	return AR_STAT_UNDEFINED;
}

#define NOINTRIN	1	/* Unknown/unsupported intrinsic name */
#define ARITHERR	2	/* Arithmetic error detected by intrinsic */
#define EXTERROR	3	/* Unsupported external called by intrinsic */
#define SETERRNO	4	/* Set errno = ERANGE */
#define IFACEERR	5	/* Unsupported simulation interface */
#define HOSTEXT 	6	/* Call host system external */
#define UNIMPL_INST 7	/* Unimplemented instruction encountered */
#define UNIMPL_PRFX 8	/* Unimplemented prefixed instruction encountered */


#if _CRAY
#define getp1(pc) ( (*(((long*)code)+((pc)>>2))>>(48-(((pc)&3)<<4)) & 0xffff) )
#else
#define getp1(pc) ( (((int) code[(pc)*2])<<8) | code[(pc)*2+1] )
#endif

#define prefix_check(p)    if (p) { status = UNIMPL_PRFX; break; } else
        
static int
ar_check_status(int status, AR_IEEE_64* rr)
{
	if(!IS_ERROR_STATUS(status))
		return AR_STAT_OK;

	if((status & AR_STAT_OVERFLOW) && (SR0 & (INTERRUPT_OVF|INTERRUPT_DIV)))
		return status;

	if((status & AR_STAT_UNDEFINED) && (SR0 & INTERRUPT_INV))
		if(rr->expo > AR_IEEE64_MAX_EXPO &&
		   (rr->coeff0 | rr->coeff1 | rr->coeff2 | rr->coeff3))
			return status;

	return AR_STAT_OK;
}

int
ar_sim(char* intrinsic)
{
	char*	name;
	char*	str;

	int i,j,n;
	int status;

	int p1;
	int op;
	int I,J,K;
	int JK;
	int prefix;

	unsigned long	pc, inst_pc;
	AR_HOST_UINT64	wa;
	AR_HOST_UINT64	rtc, rtcpc;

	int64 zero;
	int64 one;
	int64 ones;
	int64 sb;
	int64 reg64;

	int64	*rr, *oj, *ok;

	int save_trunc_bits;

	AR_COMPARE_TYPE cmpres;

	static int ar_imul64u();

	/* Set up initial Cray stack and register values */

	/*
	 * The following test verifies that at least one argument
	 * has been passed via ar_pass_arg_{value|address}.  This in turn
	 * guarantees that the arith data file has been opened and read into
	 * memory so that the simulation is ready to begin.
	 */

	if(numargwds == 0)
		ar_internal_error(2014, __FILE__, __LINE__);

	S[0] = (numargs<<20) | numargwds;
	A[6] = (STACK<<24)|STACK_SIZE;		/* Argument list pointer */
	store_pvp_word((long)A[6], S[0]);

	VL = 0;
	B[000] = 0; 		/* Force return address = 0 */
	B[066] = (STACK<<24) | 0;	/* Define stack pointers */
	B[067] = B[066]+STACK_SIZE;

	/*
	 * At this point all necessary registers (except pc) contain values
	 * that correspond with being at the entry point of the called function.
	 */

	/*
	 * Pass my simulation interface identifier.  The logic within the
	 * pvp simulation file (arith data file) can then validate and/or
	 * translate to its interface.  This allows this routine and a
	 * newer pvp simulation file to still function correctly.  In
	 * practice, this permits older (archived?) products to still work
	 * with a newer default arith data file.
	 */

	B[065] = ar_sim_version;	/* Specify sim interface id */

	/*
	 * Store LJ'd, zero-filled, uppercase intrinsic name into S[7]
	 * as part of the simulation interface with the pvp simulation file
	 * (arith data file).  The simulation is assumed to begin (at pc=0)
	 * with a lookup of this name followed by an unconditional branch to
	 * the associated entry point when found.  The simulation continues
	 * until either an error condition occurs or the pc becomes 0 again
	 * corresponding to the 0 forced into the return address register
	 * B[000]) above.
	 */

	name = (char*)&S[7];
	for(i=0; i<8 && isalnum(intrinsic[i]); i++)
		name[i] = toupper(intrinsic[i]);
	while(i < 8)
		name[i++] = 0;

	/* User-specified truncation is not used when emulating library functions */

	save_trunc_bits = ar_state_register.ar_truncate_bits;	/* save trunc bits */
	ar_state_register.ar_truncate_bits = 0;
	zero = 0;
	one  = 1;
	ones = ~zero;
	sb   = one<<63;

	/*
	 * Simulate Cray intrinsic function.  Simulation normally ends with
	 * a J B00 (to word 0) and register S1,... containing the desired
	 * result.  An ERREXIT (opcode 000) also terminates simulation.  In
	 * this case if S0 = (NOINTRIN, ARITHERR, EXTERROR, IFACEERR, SETERRNO),
	 * actions are taken to handle the error condition.
	 */

	pc = 0;
	rtcpc = 0;
	prefix = 0;
	status = AR_STAT_OK;
	SR0 = (INTERRUPT_INV|INTERRUPT_DIV|INTERRUPT_OVF) |
		  (ar_state_register.ar_rounding_mode<<1);
	while(status == AR_STAT_OK) {

	  /* Decode next instruction */
	  DISASM2("0p%6.6o%c\t", pc>>2, 'a'+(pc&3));
	  p1 = getp1(pc); pc++;
	  op = p1>>9;
	  I  = (p1>>6)&7;
	  J  = (p1>>3)&7;
	  K  = p1&7;

	  switch(op) {
	  case 000:
		prefix_check(prefix);
		status = S[0];
		DISASM1("ERR\t\t\t0x%4.4x\n", status);
		switch (status) {
		case IFACEERR:		/* Unsupported simulation interface */
			ar_internal_error(2014, __FILE__, __LINE__);
			status = AR_STAT_UNDEFINED;
			break;
		case NOINTRIN:		/* Unsupported intrinsic name */
			PRINTMSG(0, 2015, Internal, 0, __FILE__, __LINE__, intrinsic, "");
			status = AR_STAT_UNDEFINED;
			break;
		case EXTERROR:		/* Unsupported EXT called by intrinsic */
			if(status == EXTERROR)
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
			status = call_host_external((char*)&S[7]);
			pc = B[00];
			break;
		default:
			ar_internal_error(2013, __FILE__, __LINE__);
			status = AR_STAT_UNDEFINED;
		}
		break;

	  case 001:
		prefix_check(prefix);
		if(!(I|J|K))
			DISASM0("PASS\n");
		else
			status = UNIMPL_INST;
		break;

	  case 002:
		prefix_check(prefix);
		if(I == 0) {			/* VL Ak */
			if(K == 0)
				VL = 1;
			else if((A[K]&(VSZ-1)) == 0)
				VL = VSZ;
			else
				VL = A[K]&(VSZ-1);
			DISASM2("VL\tA%1o\t\t%d\n", K, VL);
		}
		else if(I <= 2 && K > 0 &&
				ar_state_register.ar_float_format == AR_IEEE_FLOATING_POINT) {
			switch(K) {
			case 1:
				if(I == 1)
					SR0 |= INTERRUPT_INV;
				else
					SR0 &=~INTERRUPT_INV;
				break;
			case 2:
				if(I == 1)
					SR0 |= INTERRUPT_DIV;
				else
					SR0 &=~INTERRUPT_DIV;
				break;
			case 3:
				if(I == 1)
					SR0 |= INTERRUPT_OVF;
				else
					SR0 &=~INTERRUPT_OVF;
				break;
			case 4:
				if(I == 1)
					SR0 |= INTERRUPT_UNF;
				else
					SR0 &=~INTERRUPT_UNF;
				break;
			case 5:
				if(I == 1)
					SR0 |= INTERRUPT_INX;
				else
					SR0 &=~INTERRUPT_INX;
				break;
			case 6:
				if(I == 1)
					SR0 |= INTERRUPT_INP;
				else
					SR0 &=~INTERRUPT_INP;
				break;
			}
			DISASM2("%cFI\t%3.3s\n", 'F'-I, "INVDIVOVFUNFINXINP"+(K-1)*3);
		}
		else				/* Ignore all others */
			DISASM1("%6.6o\n", p1);
		break;

	  case 003:
		prefix_check(prefix);
		if(I == 0) {
			if(K < 4) { 		/* VM A/Sj */
				if(J == 0)
					VM = 0;
				else {
					switch(K) {
					case 0:
						VM = S[J];
						break;
					case 1:
						VM1 = S[J];
						break;
					case 2:
						VM = A[J];
						break;
					case 3:
						VM1 = A[J];
						break;
					}
				}
				DISASM4("VM%1o\t%c%1o\t\t0x%16.16llx\n", K&1, (K&2)?'A':'S', J,
						((K&1)?VM1:VM));
			}
			else if(ar_state_register.ar_float_format == AR_IEEE_FLOATING_POINT) {
				SR0 = (SR0 &~ RMMASK) | ((K<<1) & RMMASK);
				SR0 ^= ((SR0&04)>>1);		/* Flip rounding mode order */
				DISASM1("R%1.1sM\n", "NUZD"+(K&3));
			}
		}
		else				/* Ignore SM insts */
			DISASM1("%6.6o\n", p1);
		break;

	  case 004:
		prefix_check(prefix);
		status = UNIMPL_INST;
		break;

	  case 005:
		prefix_check(prefix);
		JK = p1&077;
		if(I <= 1) {
			pc = B[JK];
			DISASM3("J\tB%2.2o\t\t0p%6.6o%c\n", JK, pc>>2, 'a'+(pc&3));
			if(pc == 0) {			/* Check for end of simulation */
				ar_state_register.ar_truncate_bits = save_trunc_bits;
				return(status);
			}
		}
		else if((I == 4 && J == 0 && K == 0) ||
				(I == 5 && (J == 0 || J == 2 || J == 4) && K >= 1 && K <= 7)) {
			prefix = p1;
			DISASM1("%05o\n", prefix);
		}
		else {
			status = UNIMPL_INST;
		}
		break;

	  case 006:
		prefix_check(prefix);
		if(JSZP == 3 && I)
			status = UNIMPL_INST;
		else {
			pc = ((p1&0377)<<16) | getp1(pc);
			DISASM2("J\t0p%6.6o%c\n", pc>>2, 'a'+(pc&3));
		}
		break;

	  case 007:
		prefix_check(prefix);
		B[00] = pc+JSZP-1;
		pc = ((p1&0377)<<16) | getp1(pc);
		DISASM2("R\t0p%6.6o%c\n", pc>>2, 'a'+(pc&3));
		break;

	  case 010:
		prefix_check(prefix);
		inst_pc = ((p1&0377)<<16) | getp1(pc);
		if((A[0]&AMASK) == 0)
			pc = inst_pc;
		else
			pc += JSZP-1;
		DISASM2("JAZ\t0p%6.6o%c\n", inst_pc>>2, 'a'+(inst_pc&3));
		break;

	  case 011:
		prefix_check(prefix);
		inst_pc = ((p1&0377)<<16) | getp1(pc);
		if((A[0]&AMASK) != 0)
			pc = inst_pc;
		else
			pc += JSZP-1;
		DISASM2("JAN\t0p%6.6o%c\n", inst_pc>>2, 'a'+(inst_pc&3));
		break;

	  case 012:
		prefix_check(prefix);
		inst_pc = ((p1&0377)<<16) | getp1(pc);
		if((A[0]&ASB) == 0)
			pc = inst_pc;
		else
			pc += JSZP-1;
		DISASM2("JAP\t0p%6.6o%c\n", inst_pc>>2, 'a'+(inst_pc&3));
		break;

	  case 013:
		prefix_check(prefix);
		inst_pc = ((p1&0377)<<16) | getp1(pc);
		if((A[0]&ASB) != 0)
			pc = inst_pc;
		else
			pc += JSZP-1;
		DISASM2("JAM\t0p%6.6o%c\n", inst_pc>>2, 'a'+(inst_pc&3));
		break;

	  case 014:
		prefix_check(prefix);
		inst_pc = ((p1&0377)<<16) | getp1(pc);
		if(S[0])
			pc += JSZP-1;
		else
			pc = inst_pc;
		DISASM2("JSZ\t0p%6.6o%c\n", inst_pc>>2, 'a'+(inst_pc&3));
		break;

	  case 015:
		prefix_check(prefix);
		inst_pc = ((p1&0377)<<16) | getp1(pc);
		if(S[0])
			pc = inst_pc;
		else
			pc += JSZP-1;
		DISASM2("JSN\t0p%6.6o%c\n", inst_pc>>2, 'a'+(inst_pc&3));
		break;

	  case 016:
		prefix_check(prefix);
		inst_pc = ((p1&0377)<<16) | getp1(pc);
		if((S[0]>>63) == 0)
			pc = inst_pc;
		else
			pc += JSZP-1;
		DISASM2("JSP\t0p%6.6o%c\n", inst_pc>>2, 'a'+(inst_pc&3));
		break;

	  case 017:
		prefix_check(prefix);
		inst_pc = ((p1&0377)<<16) | getp1(pc);
		if((S[0]>>63) != 0)
			pc = inst_pc;
		else
			pc += JSZP-1;
		DISASM2("JSM\t0p%6.6o%c\n", inst_pc>>2, 'a'+(inst_pc&3));
		break;

	  case 020:
		prefix_check(prefix);
		wa = (getp1(pc) | (getp1(pc+1)<<16)); pc += 2;
		if(J == 0) {
			A[I] = wa;
			DISASM3("A%1o\t0x%8.8x\t0x%*.*llx\n", I, wa, AVAL(I));
		}
		else if(J == 2) {
			A[I] = (A[I]&~(int64)0xffffffff) | wa;
			DISASM4("A%1o\tA%1o:0x%8.8x\t0x%*.*llx\n", I, I, wa, AVAL(I));
		}
		else if(J == 4) {
			A[I] = (A[I]&(int64)0xffffffff) | ((int64)wa<<32);
			DISASM4("A%1o\t0x%8.8x:A%1o\t0x%*.*llx\n", I, wa, I, AVAL(I));
		}
		else
			status = UNIMPL_INST;
		break;

	  case 021:
		prefix_check(prefix);
		wa = (getp1(pc) | (getp1(pc+1)<<16)); pc += 2;
		A[I] = ~wa;
		DISASM3("A%1o\t#0x%8.8x\t0x%*.*llx\n", I, wa, AVAL(I));
		break;

	  case 022:
		prefix_check(prefix);
		A[I] = p1&077;
		DISASM3("A%1o\t0%2.2o\t\t0x%*.*llx\n", I, (long)A[I], AVAL(I));
		break;

	  case 023:
		prefix_check(prefix);
		if(K == 0) {
			A[I] = S[J];
			DISASM3("A%1o\tS%1o\t\t0x%*.*llx\n", I, J, AVAL(I));
		}
		else if(K == 1) {
			if(VL == 0)
				A[I] = VSZ;
			else
				A[I] = VL;
			DISASM2("A%1o\tVL\t\t%d\n", I, (long)A[I]);
		}
		else
			status = UNIMPL_INST;
		break;

	  case 024:
		prefix_check(prefix);
		JK = p1&077;
		A[I] = B[JK];
		DISASM3("A%1o\tB%2.2o\t\t0x%*.*llx\n", I, JK, AVAL(I));
		break;

	  case 025:
		prefix_check(prefix);
		JK = p1&077;
		B[JK] = A[I];
		DISASM3("B%2.2o\tA%1o\t\t0x%*.*llx\n", JK, I, BVAL(JK));
		break;

	  case 026:
		prefix_check(prefix);
		if(J) {
			if(K&2)
				reg64 = A[J];
			else
				reg64 = S[J];
			if(reg64) {
				for(i=0,n=0; i<64; i++) {
					if(reg64&1)
						n++;
					reg64 >>= 1;
				}
				if(K & 1)
					n &= 1;
			}
			else
				n = 0;
		}
		else
			n = 0;
		A[I] = n;
		if(K & 4)
			status = UNIMPL_INST;
		DISASM5("A%1o\t%c%c%1o\t\t%d\n", I, (K&1)?'Q':'P', (K&2)?'A':'S', J, (long)A[I]);
		break;

	  case 027:
		prefix_check(prefix);
		if(J) {
			if(K&1)
				reg64 = A[J];
			else
				reg64 = S[J];
			if(reg64) {
				for(n=0; n<64; n++)
					if(reg64>>(63-n)) break;
			}
			else
				n = 64;
			DISASM4("A%1o\tZ%c%1o\t\t%d\n", I, (K&1)?'A':'S', J, (long)A[I]);
		}
		else
			n = 64;
		A[I] = n;
		if(K & 6)
			status = UNIMPL_INST;
		break;

	  case 030:
		prefix_check(prefix);
		if(J==0 && K==0)
			A[I] = 1;
		else if(J == 0)
			A[I] = A[K];
		else if(K == 0)
			A[I] = (A[J]+1)&AMASK;
		else
			A[I] = (A[J]+A[K])&AMASK;
		DISASM4("A%1o\tA%1o+A%1o\t\t0x%*.*llx\n", I, J, K, AVAL(I));
		break;

	  case 031:
		prefix_check(prefix);
		if(J==0 && K==0)
			A[I] = AMASK;
		else if(J == 0)
			A[I] = (-A[K])&AMASK;
		else if(K == 0)
			A[I] = (A[J]-1)&AMASK;
		else
			A[I] = (A[J]-A[K])&AMASK;
		DISASM4("A%1o\tA%1o-A%1o\t\t0x%*.*llx\n", I, J, K, AVAL(I));
		break;

	  case 032:
		prefix_check(prefix);
		if(J == 0)
			A[I] = 0;
		else if(K == 0)
			A[I] = A[J];
		else
			A[I] = (A[J]*A[K])&AMASK;
		DISASM4("A%1o\tA%1o*A%1o\t\t0x%*.*llx\n", I, J, K, AVAL(I));
		break;

	  case 033:
		prefix_check(prefix);
		status = UNIMPL_INST;
		break;

	  case 034:
		prefix_check(prefix);
		JK = p1&077;
		wa = A[0];
		n = A[I];
		for(i=0; i<n; i++)
			B[(JK+i)&077] = load_pvp_word(wa+i) & AMASK;
		DISASM2("B%2.2o,A%1o\t0,A0", JK, I);
		DISASMB(JK, n, "\t\t");
		break;

	  case 035:
		prefix_check(prefix);
		JK = p1&077;
		wa = A[0];
		n = A[I];
		for(i=0; i<n; i++)
			store_pvp_word(wa+i, B[(JK+i)&077]);
		DISASM2("0,A0\tB%2.2o,A%1o\n", JK, I);
		break;

	  case 036:
		prefix_check(prefix);
		JK = p1&077;
		wa = A[0];
		n = A[I];
		for(i=0; i<n; i++)
			T[(JK+i)&077] = load_pvp_word(wa+i);
		DISASM2("T%2.2o,A%1o\t0,A0", JK, I);
		DISASMT(JK, n, "\t\t");
		break;

	  case 037:
		prefix_check(prefix);
		JK = p1&077;
		wa = A[0];
		n = A[I];
		for(i=0; i<n; i++)
			store_pvp_word(wa+i, T[(JK+i)&077]);
		DISASM2("0,A0\tT%2.2o,A%1o\n", JK, I);
		break;

	  case 040:
		prefix_check(prefix);
		wa = (getp1(pc) | (getp1(pc+1)<<16)); pc += 2;
		if(J == 0) {
			S[I] = wa;
			DISASM3("S%1o\t0x%8.8x\t0x%16.16llx\n", I, wa, S[I]);
		}
		else if(J == 2) {
			S[I] = (S[I]&~(int64)0xffffffff) | wa;
			DISASM4("S%1o\tS%1o:0x%8.8x\t0x%16.16llx\n", I, I, wa, S[I]);
		}
		else if(J == 4) {
			S[I] = (S[I]&(int64)0xffffffff) | ((int64)wa<<32);
			DISASM4("S%1o\t0x%8.8x:S%1o\t0x%16.16llx\n", I, wa, I, S[I]);
		}
		else
			status = UNIMPL_INST;
		break;

	  case 041:
		prefix_check(prefix);
		wa = (getp1(pc) | (getp1(pc+1)<<16)); pc += 2;
		S[I] = (((int64)0xffffffff)<<32) | ~wa;
		DISASM3("S%1o\t#0x%8.8x\t0x%16.16llx\n", I, wa, S[I]);
		break;

	  case 042:
		prefix_check(prefix != 0 && prefix != 05400);
		JK = p1&077;
		if (prefix == 0) {
			S[I] = ones>>JK;
			DISASM3("S%1o\t<0%2.2o\t\t0x%16.16llx\n", I, 64-JK, S[I]);
		}
		else {
			A[I] = ones>>JK;
			DISASM3("A%1o\t<0%2.2o\t\t0x%*.*llx\n", I, 64-JK, AVAL(I));
			prefix = 0;
		}
		break;

	  case 043:
		prefix_check(prefix != 0 && prefix != 05400);
		JK = p1&077;
		if (prefix == 0) {
			if(JK)
				S[I] = ones<<(64-JK);
			else
				S[I] = 0;
			DISASM3("S%1o\t>0%2.2o\t\t0x%16.16llx\n", I, JK, S[I]);
		}
		else {
			if(JK)
				A[I] = ones<<(64-JK);
			else
				A[I] = 0;
			DISASM3("A%1o\t>0%2.2o\t\t0x%*.*llx\n", I, JK, AVAL(I));
			prefix = 0;
		}
		break;

	  case 044:
		prefix_check(prefix != 0 && prefix != 05400);
		if (prefix == 0) {
			if(J == 0)
				S[I] = 0;
			else if(K == 0)
				S[I] = S[J]&sb;
			else
				S[I] = S[J]&S[K];
			DISASM4("S%1o\tS%1o&S%1o\t\t0x%16.16llx\n", I, J, K, S[I]);
		}
		else {
			if(J == 0)
				A[I] = 0;
			else if(K == 0)
				A[I] = A[J]&one;
			else
				A[I] = A[J]&A[K];
			DISASM4("A%1o\tA%1o&A%1o\t\t0x%*.*llx\n", I, J, K, AVAL(I));
			prefix = 0;
		}
		break;

	  case 045:
		prefix_check(prefix != 0 && prefix != 05400);
		if (prefix == 0) {
			if(J == 0)
				S[I] = 0;
			else if(K == 0)
				S[I] = S[J]&~sb;
			else
				S[I] = S[J]&~S[K];
			DISASM4("S%1o\tS%1o&#S%1o\t\t0x%16.16llx\n", I, J, K, S[I]);
		}
		else {
			if(J == 0)
				A[I] = 0;
			else if(K == 0)
				A[I] = A[J]&~one;
			else
				A[I] = A[J]&~A[K];
			DISASM4("A%1o\tA%1o&#A%1o\t\t0x%*.*llx\n", I, J, K, AVAL(I));
			prefix = 0;
		}
		break;

	  case 046:
		prefix_check(prefix != 0 && prefix != 05400);
		if (prefix == 0) {
			if(J == 0)
				if(K == 0)
					S[I] = sb;
				else
					S[I] = S[K];
			else if(K == 0)
				S[I] = S[J]^sb;
			else
				S[I] = S[J]^S[K];
			DISASM4("S%1o\tS%1o\\S%1o\t\t0x%16.16llx\n", I, J, K, S[I]);
		}
		else {
			if(J == 0)
				if(K == 0)
					A[I] = one;
				else
					A[I] = A[K];
			else if(K == 0)
				A[I] = A[J]^one;
			else
				A[I] = A[J]^A[K];
			DISASM4("A%1o\tA%1o\\A%1o\t\t0x%*.*llx\n", I, J, K, AVAL(I));
			prefix = 0;
		}
		break;

	  case 047:
		prefix_check(prefix != 0 && prefix != 05400);
		if (prefix == 0) {
			if(J == 0)
				if(K == 0)
					S[I] = ~sb;
				else
					S[I] = ~S[K];
			else if(K == 0)
				S[I] = ~S[J]^sb;
			else
				S[I] = ~S[J]^S[K];
			DISASM4("S%1o\t#S%1o\\S%1o\t\t0x%16.16llx\n", I, J, K, S[I]);
		}
		else {
			if(J == 0)
				if(K == 0)
					A[I] = ~one;
				else
					A[I] = ~A[K];
			else if(K == 0)
				A[I] = ~A[J]^one;
			else
				A[I] = ~A[J]^A[K];
			DISASM4("A%1o\t#A%1o\\A%1o\t\t0x%*.*llx\n", I, J, K, AVAL(I));
			prefix = 0;
		}
		break;

	  case 050:
		prefix_check(prefix != 0 && prefix != 05400);
		if (prefix == 0) {
			if(J == 0)
				if(K == 0)
					S[I] = S[I]&~sb;
				else
					S[I] = S[I]&~S[K];
			else if(K == 0)
				S[I] = (S[J]&sb) | (S[I]&~sb);
			else
				S[I] = (S[J]&S[K]) | (S[I]&~S[K]);
			DISASM4("S%1o\t|S%1o&S%1o\t\t0x%16.16llx\n", I, J, K, S[I]);
		}
		else {
			if(J == 0)
				if(K == 0)
					A[I] = A[I]&~one;
				else
					A[I] = A[I]&~A[K];
			else if(K == 0)
				A[I] = (A[J]&one) | (A[I]&~one);
			else
				A[I] = (A[J]&A[K]) | (A[I]&~A[K]);
			DISASM4("A%1o\t|A%1o&A%1o\t\t0x%*.*llx\n", I, J, K, AVAL(I));
			prefix = 0;
		}
		break;

	  case 051:
		prefix_check(prefix != 0 && prefix != 05400);
		if (prefix == 0) {
			if(J == 0)
				if(K == 0)
					S[I] = sb;
				else
					S[I] = S[K];
			else if(K == 0)
				S[I] = S[J]|sb;
			else
				S[I] = S[J]|S[K];
			DISASM4("S%1o\tS%1o|S%1o\t\t0x%16.16llx\n", I, J, K, S[I]);
		}
		else {
			if(J == 0)
				if(K == 0)
					A[I] = one;
				else
					A[I] = A[K];
			else if(K == 0)
				A[I] = A[J]|one;
			else
				A[I] = A[J]|A[K];
			DISASM4("A%1o\tA%1o|A%1o\t\t0x%*.*llx\n", I, J, K, AVAL(I));
			prefix = 0;
		}
		break;

	  case 052:
		prefix_check(prefix != 0 && prefix != 05400);
		if (prefix == 0) {
			S[0] = S[I] << (p1&077);
			DISASM3("S0\tS%1o<0%2.2o\t\t0x%16.16llx\n", I, p1&077, S[0]);
		}
		else {
			A[0] = A[I] << (p1&077);
			DISASM3("S0\tA%1o<0%2.2o\t\t0x%*.*llx\n", I, p1&077, AVAL(0));
			prefix = 0;
		}
		break;

	  case 053:
		prefix_check(prefix != 0 && prefix != 05400);
		if (prefix == 0) {
			if(p1&077)
				S[0] = S[I] >> (64-(p1&077));
			else
				S[0] = 0;
			DISASM3("S0\tS%1o>0%2.2o\t\t0x%16.16llx\n", I, 64-(p1&077), S[0]);
		}
		else {
			if(p1&077)
				A[0] = A[I] >> (64-(p1&077));
			else
				A[0] = 0;
			DISASM3("S0\tA%1o>0%2.2o\t\t0x%*.*llx\n", I, 64-(p1&077), AVAL(0));
			prefix = 0;
		}
		break;

	  case 054:
		prefix_check(prefix != 0 && prefix != 05400);
		if (prefix == 0) {
			S[I] <<= (p1&077);
			DISASM4("S%1o\tS%1o<0%2.2o\t\t0x%16.16llx\n", I, I, p1&077, S[I]);
		}
		else {
			A[I] <<= (p1&077);
			DISASM4("A%1o\tA%1o<0%2.2o\t\t0x%*.*llx\n", I, I, p1&077, AVAL(I));
			prefix = 0;
		}
		break;

	  case 055:
		prefix_check(prefix != 0 && prefix != 05400);
		if (prefix == 0) {
			if(p1&077)
				S[I] >>= (64-(p1&077));
			else
				S[I] = 0;
			DISASM4("S%1o\tS%1o>0%2.2o\t\t0x%16.16llx\n", I, I, 64-(p1&077),
					S[I]);
		}
		else {
			if(p1&077)
				A[I] >>= (64-(p1&077));
			else
				A[I] = 0;
			DISASM4("A%1o\tA%1o>0%2.2o\t\t0x%*.*llx\n", I, I, 64-(p1&077),
					AVAL(I));
			prefix = 0;
		}
		break;

	  case 056:
		prefix_check(prefix != 0 && prefix != 05400);
		if (prefix == 0) {
			if(J == 0)
				reg64 = 0;
			else
				reg64 = S[J];
			if(K == 0)
				n = 1;
			else {
				n = A[K];
				if(n >= 64) {
					if(n >= 128) {
						S[I] = 0;
						n = 0;
					}
					else {
						S[I] = reg64;
						reg64 = 0;
						n -= 64;
					}
				}
			}
			if(n)
				S[I] = (S[I]<<n) | (reg64>>(64-n));
			DISASM5("S%1o\tS%1o,S%1o<A%1o\t0x%16.16llx\n", I, I, J, K, S[I]);
		}
		else {
			if(J == 0)
				reg64 = 0;
			else
				reg64 = A[J];
			if(K == 0)
				n = 1;
			else {
				n = A[K];
				if(n >= 64) {
					if(n >= 128) {
						A[I] = 0;
						n = 0;
					}
					else {
						A[I] = reg64;
						reg64 = 0;
						n -= 64;
					}
				}
			}
			if(n)
				A[I] = (A[I]<<n) | (reg64>>(64-n));
			DISASM5("A%1o\tA%1o,A%1o<A%1o\t0x%*.*llx\n", I, I, J, K, AVAL(I));
			prefix = 0;
		}
		break;

	  case 057:
		prefix_check(prefix != 0 && prefix != 05400);
		if (prefix == 0) {
			if(J == 0)
				reg64 = 0;
			else
				reg64 = S[J];
			if(K == 0)
				n = 1;
			else {
				n = A[K];
				if(n >= 64) {
					if(n >= 128) {
						S[I] = 0;
						n = 0;
					}
					else {
						S[I] = reg64;
						reg64 = 0;
						n -= 64;
					}
				}
			}
			if(n)
				S[I] = (reg64<<(64-n)) | (S[I]>>n);
			DISASM5("S%1o\tS%1o,S%1o>A%1o\t0x%16.16llx\n", I, J, I, K, S[I]);
		}
		else {
			if(J == 0)
				reg64 = 0;
			else
				reg64 = A[J];
			if(K == 0)
				n = 1;
			else {
				n = A[K];
				if(n >= 64) {
					if(n >= 128) {
						A[I] = 0;
						n = 0;
					}
					else {
						A[I] = reg64;
						reg64 = 0;
						n -= 64;
					}
				}
			}
			if(n)
				A[I] = (reg64<<(64-n)) | (A[I]>>n);
			DISASM5("A%1o\tA%1o,A%1o>A%1o\t0x%*.*llx\n", I, J, I, K, AVAL(I));
			prefix = 0;
		}
		break;

	  case 060:
		prefix_check(prefix);
		if(J == 0)
			if(K == 0)
				S[I] = sb;
			else
				S[I] = S[K];
		else if(K == 0)
			S[I] = S[J] + sb;
		else
			S[I] = S[J] + S[K];
		DISASM4("S%1o\tS%1o+S%1o\t\t0x%16.16llx\n", I, J, K, S[I]);
		break;

	  case 061:
		prefix_check(prefix);
		if(J == 0)
			if(K == 0)
				S[I] = -sb;
			else
				S[I] = -S[K];
		else if(K == 0)
			S[I] = S[J] - sb;
		else
			S[I] = S[J] - S[K];
		DISASM4("S%1o\tS%1o-S%1o\t\t0x%16.16llx\n", I, J, K, S[I]);
		break;

	  case 062:
		prefix_check(prefix);
		if(K == 0)
			ok = &sb;
		else
			ok = &S[K];
		if(J == 0)
			oj = &zero;
		else
			oj = &S[J];
		rr = &S[I];
		if(ar_state_register.ar_float_format == AR_CRAY_FLOATING_POINT)
			status = ar_cfadd64((AR_CRAY_64*)rr,
								(AR_CRAY_64*)oj, (AR_CRAY_64*)ok);
		else {
			status = ar_ifadd64((AR_IEEE_64*)rr,
								(AR_IEEE_64*)oj, (AR_IEEE_64*)ok,
								SR0&RMMASK);
			if(IS_ERROR_STATUS(status))
				status = ar_check_status(status, (AR_IEEE_64*)rr);
		}
		status &= AR_ERROR_STATUS;
		DISASM4("S%1o\tS%1o+FS%1o\t\t0x%16.16llx\n", I, J, K, S[I]);
		break;

	  case 063:
		prefix_check(prefix);
		if(K == 0)
			ok = &sb;
		else
			ok = &S[K];
		if(J == 0)
			oj = &zero;
		else
			oj = &S[J];
		rr = &S[I];
		if(ar_state_register.ar_float_format == AR_CRAY_FLOATING_POINT)
			status = ar_cfsub64((AR_CRAY_64*)rr,
								(AR_CRAY_64*)oj, (AR_CRAY_64*)ok);
		else {
			status = ar_ifsub64((AR_IEEE_64*)rr,
								(AR_IEEE_64*)oj, (AR_IEEE_64*)ok,
								SR0&RMMASK);
			if(IS_ERROR_STATUS(status))
				status = ar_check_status(status, (AR_IEEE_64*)rr);
		}
		status &= AR_ERROR_STATUS;
		DISASM4("S%1o\tS%1o-FS%1o\t\t0x%16.16llx\n", I, J, K, S[I]);
		break;

	  case 064:
		prefix_check(prefix);
		if(K == 0)
			ok = &sb;
		else
			ok = &S[K];
		if(J == 0)
			oj = &zero;
		else
			oj = &S[J];
		rr = &S[I];
		if(ar_state_register.ar_float_format == AR_CRAY_FLOATING_POINT)
			status = ar_cfmul64((AR_CRAY_64*)rr,
								(AR_CRAY_64*)oj, (AR_CRAY_64*)ok,
								AR_UNROUNDED);
		else {
			status = ar_ifmul64((AR_IEEE_64*)rr,
								(AR_IEEE_64*)oj, (AR_IEEE_64*)ok,
								SR0&RMMASK);
			if(IS_ERROR_STATUS(status))
				status = ar_check_status(status, (AR_IEEE_64*)rr);
		}
		status &= AR_ERROR_STATUS;
		DISASM4("S%1o\tS%1o*FS%1o\t\t0x%16.16llx\n", I, J, K, S[I]);
		break;

	  case 065:
		prefix_check(prefix);
		if(K == 0)
			ok = &sb;
		else
			ok = &S[K];
		if(J == 0)
			oj = &zero;
		else
			oj = &S[J];
		rr = &S[I];
		if(ar_state_register.ar_float_format == AR_CRAY_FLOATING_POINT)
			status = UNIMPL_INST;
		else {
			/*
			 * Yes, it really is "Si Sk/FSj".
			 */
			status = ar_ifdiv64((AR_IEEE_64*)rr,
								(AR_IEEE_64*)ok, (AR_IEEE_64*)oj,
								SR0&RMMASK);
			status &= AR_ERROR_STATUS;
			if(status)
				status = ar_check_status(status, (AR_IEEE_64*)rr);
			DISASM4("S%1o\tS%1o/FS%1o\t\t0x%16.16llx\n", I, K, J, S[I]);
		}
		break;

	  case 066:
		prefix_check(prefix != 0 && prefix != 05400);
		if(K == 0)
			ok = &sb;
		else
			ok = &S[K];
		if(J == 0)
			oj = &zero;
		else
			oj = &S[J];
		rr = &S[I];
		if (prefix == 0) {
			if(ar_state_register.ar_float_format == AR_CRAY_FLOATING_POINT) {
				status = ar_cfmul64((AR_CRAY_64*)rr,
									(AR_CRAY_64*)oj, (AR_CRAY_64*)ok,
									AR_ROUNDED);
				status &= AR_ERROR_STATUS;
				DISASM4("S%1o\tS%1o*RS%1o\t\t0x%16.16llx\n", I, J, K, S[I]);
			}
			else {
				S[I] = *oj * *ok;
				DISASM4("S%1o\tS%1o*LS%1o\t\t0x%16.16llx\n", I, J, K, S[I]);
			}
		}
		else {
			status = ar_imul64u(rr, *oj, *ok);
			DISASM4("S%1o\tS%1o*US%1o\t\t0x%16.16llx\n", I, J, K, S[I]);
			prefix = 0;
		}
		break;

	  case 067:
		prefix_check(prefix);
		if(K == 0)
			ok = &sb;
		else
			ok = &S[K];
		if(J == 0)
			oj = &zero;
		else
			oj = &S[J];
		rr = &S[I];
		if(ar_state_register.ar_float_format == AR_CRAY_FLOATING_POINT) {
			status = ar_cfmul64((AR_CRAY_64*)rr,
								(AR_CRAY_64*)oj, (AR_CRAY_64*)ok,
								AR_RECIPROCAL_ITERATION);
			status &= AR_ERROR_STATUS;
			DISASM4("S%1o\tS%1o*IS%1o\t\t0x%16.16llx\n", I, J, K, S[I]);
		}
		else
			status = UNIMPL_INST;
		break;

	  case 070:
		prefix_check(prefix);
		if(J == 0)
			oj = &zero;
		else
			oj = &S[J];
		rr = &S[I];
		switch(K) {
		case 0:
			if(ar_state_register.ar_float_format == AR_CRAY_FLOATING_POINT) {
				status = ar_c1frecip((AR_CRAY_64*)rr, (AR_CRAY_64*)oj);
				DISASM3("S%1o\t/HS%1o\t\t0x%16.16llx\n", I, J, S[I]);
			}
			else {
				status = ar_isqrt64((AR_IEEE_64*)rr,
									(AR_IEEE_64*)oj, SR0&RMMASK);
				if(IS_ERROR_STATUS(status))
					status = ar_check_status(status, (AR_IEEE_64*)rr);
				DISASM3("S%1o\tSQRT,S%1o\t\t0x%16.16llx\n", I, J, S[I]);
			}
			break;
		case 1:
			reg64 = 0;
			for(i=0,j=0; i<VL; i++) {
				if((VM>>(63-i)) & 1)
					V[I*VSZ+j++] = reg64;
				if(J)
					reg64 += S[J];
			}
			DISASM2("V%1o\tCI,S%1o&VM\n", I, J);
			DISASMV(I, "\t\t");
			break;
		case 2:
			if(ar_state_register.ar_float_format == AR_IEEE_FLOATING_POINT) {
				status = ar_ifix64((AR_INT_64*)rr, (const AR_IEEE_64*)oj,
								   64, AR_ROUND_ZERO);
				if(IS_ERROR_STATUS(status))
					status = ar_check_status(status, (AR_IEEE_64*)rr);
				DISASM3("S%1o\tINT,S%1o\t\t0x%16.16llx\n", I, J, S[I]);
				break;
			}
		case 3:
			if(ar_state_register.ar_float_format == AR_IEEE_FLOATING_POINT) {
				status = ar_ifix64((AR_INT_64*)rr, (const AR_IEEE_64*)oj,
								   64, AR_ROUND_NEAREST);
				if(IS_ERROR_STATUS(status))
					status = ar_check_status(status, (AR_IEEE_64*)rr);
				DISASM3("S%1o\tRINT,S%1o\t\t0x%16.16llx\n", I, J, S[I]);
				break;
			}
		case 4:
			if(ar_state_register.ar_float_format == AR_IEEE_FLOATING_POINT) {
				status = ar_iflt64((AR_IEEE_64*)rr, (const AR_INT_64*)oj,
								   0, AR_ROUND_ZERO);
				if(IS_ERROR_STATUS(status))
					status = ar_check_status(status, (AR_IEEE_64*)rr);
				DISASM3("S%1o\tFLT,S%1o\t\t0x%16.16llx\n", I, J, S[I]);
				break;
			}
		case 5:
		case 6:
		case 7:
			status = UNIMPL_INST;
			break;
		}
		status &= AR_ERROR_STATUS;
		break;

	  case 071:
		prefix_check(prefix);
		switch(J) {
		case 0:
			S[I] = A[K];
			DISASM3("S%1o\tA%1o\t\t0x%16.16llx\n", I, K, S[I]);
			break;
		case 1:
		case 2:
			if(ar_state_register.ar_float_format == AR_CRAY_FLOATING_POINT) {
				if(ar_mach_type == T90)
					S[I] = A[K];
				else if(A[K] & ASB)
					S[I] = (AMASK<<32) | A[K];
				else
					S[I] = A[K];
				if(J == 1)
					DISASM3("S%1o\t+A%1o\t\t0x%16.16llx\n", I, K, S[I]);
				else {
					if(S[I]>>63)
					   S[I] = (S[I]^(ones>>1))+1;
					S[I] &= ~((int64)0x7fff<<48);
					S[I] |= ((int64)040060<<48);
					DISASM3("S%1o\t+FA%1o\t\t0x%16.16llx\n", I, K, S[I]);
				}
				break;
			}
		case 3:
			if(ar_state_register.ar_float_format == AR_CRAY_FLOATING_POINT) {
				S[I] = (int64)0400606<<45;
				DISASM1("S%1o\t0.6\n", I);
				break;
			}
		case 4:
			if(ar_state_register.ar_float_format == AR_CRAY_FLOATING_POINT) {
				S[I] = (int64)0400004<<45;
				DISASM1("S%1o\t0.4\n", I);
				break;
			}
		case 5:
			if(ar_state_register.ar_float_format == AR_CRAY_FLOATING_POINT) {
				S[I] = (int64)0400014<<45;
				DISASM1("S%1o\t1.\n", I);
				break;
			}
		case 6:
			if(ar_state_register.ar_float_format == AR_CRAY_FLOATING_POINT) {
				S[I] = (int64)0400024<<45;
				DISASM1("S%1o\t2.\n", I);
				break;
			}
		case 7:
			if(ar_state_register.ar_float_format == AR_CRAY_FLOATING_POINT) {
				S[I] = (int64)0400034<<45;
				DISASM1("S%1o\t4.\n", I);
				break;
			}
		default:
			status = UNIMPL_INST;
		}
		break;

	  case 072:
		prefix_check(prefix);
		if(J == 0 && K == 0) {
			if(rtcpc < pc)
				rtc += (pc-rtcpc);
			else
				rtc += (rtcpc-pc);
			rtcpc = pc;
			S[I] = rtc;
			DISASM1("S%1o\tRT\n", I);
		}
		else
			status = UNIMPL_INST;
		break;

	  case 073: 				/* Si VM */
		prefix_check(prefix != 0 && prefix != 05400);
		if (prefix == 0) {
			if(K == 0 && J <= 3) {
				switch(J) {
				case 0:
					S[I] = VM;
					break;
				case 1:
					S[I] = VM1;
					break;
				case 2:
					A[I] = VM;
					break;
				case 3:
					A[I] = VM1;
					break;
				}
				DISASM4("%c%1o\tVM%1o\t\t0x%16.16llx\n", (K&2)?'A':'S', I, K&1,
						(J<2)?S[I]:A[I]);
			}
			else if (J == 0 && K == 1) {
				S[I] = SR0 ^ ((SR0&04)>>1);		/* Flip rounding mode order */
				DISASM3("S%1o\tSR%1o\t\t0x%16.16llx\n", I, J, S[I]);
			}
			else if (J == 0 && K == 5) {
				SR0 = S[I] ;
				SR0 ^= ((SR0&04)>>1);			/* Flip rounding mode order */
				DISASM3("SR%1o\tS%1o\t\t0x%8.8x\n", J, I, SR0);
			}
			else
				status = UNIMPL_INST;
		}
		else if(J == 0 && K == 5) {
			SR0 = (SR0 &~ RMMASK) | S[I]&RMMASK;
			SR0 ^= ((SR0&04)>>1);				/* Flip rounding mode order */
			prefix = 0;
			DISASM1("SETRM\tS%1o\n", I);
		}
		else {
			status = UNIMPL_INST;
			prefix = 0;
		}
		break;

	  case 074:
		prefix_check(prefix);
		JK = p1&077;
		S[I] = T[JK];
		DISASM3("S%1o\tT%2.2o\t\t0x%16.16llx\n", I, JK, S[I]);
		break;

	  case 075:
		prefix_check(prefix);
		JK = p1&077;
		T[JK] = S[I];
		DISASM3("T%2.2o\tS%1o\t\t0x%16.16llx\n", JK, I, T[JK]);
		break;

	  case 076:
		prefix_check(prefix);
		if(K == 0)
			i=1;
		else
			i=A[K];
		S[I] = V[J*VSZ+i];
		DISASM4("S%1o\tV%1o,A%1o\t\t0x%16.16llx\n", I, J, K, S[I]);
		break;

	  case 077:
		prefix_check(prefix);
		if(K == 0)
			i=1;
		else
			i=A[K];
		if(J == 0)
			V[I*VSZ+i] = 0;
		else
			V[I*VSZ+i] = S[J];
		DISASM4("V%1o,A%1o\tS%1o\t\t0x%16.16llx\n", I, K, J, V[I*VSZ+i]);
		break;

	  case 0100:
	  case 0101:
	  case 0102:
	  case 0103:
	  case 0104:
	  case 0105:
	  case 0106:
	  case 0107:
		prefix_check(prefix);
		wa = (getp1(pc) | (getp1(pc+1)<<16)); pc += 2;
		if(J & 4) {
		  /* Triton and TS-IEEE only */
		        wa |= (((AR_HOST_UINT64) getp1(pc)) << 32);
			pc++;
		}
		if(op&07)
			A[I] = load_pvp_word(wa+(long)A[op&07]) & AMASK;
		else
			A[I] = load_pvp_word(wa) & AMASK;
		DISASM4("A%1o\t0x%6.6x,A%1o\t0x%*.*llx\n", I, wa, op&7, AVAL(I));
		break;

	  case 0110:
	  case 0111:
	  case 0112:
	  case 0113:
	  case 0114:
	  case 0115:
	  case 0116:
	  case 0117:
		prefix_check(prefix);
		wa = (getp1(pc) | (getp1(pc+1)<<16)); pc += 2;
		if(J & 4) {
		  /* Triton and TS-IEEE only */
			wa |= (((AR_HOST_UINT64) getp1(pc)) << 32);
			pc++;
		}
		if(op&07)
			store_pvp_word(wa+(long)A[op&07], A[I]);
		else
			store_pvp_word(wa, A[I]);
		DISASM3("0x%6.6x,A%1o  A%1o\n", wa, op&7, I);
		break;

	  case 0120:
	  case 0121:
	  case 0122:
	  case 0123:
	  case 0124:
	  case 0125:
	  case 0126:
	  case 0127:
		prefix_check(prefix);
		wa = (getp1(pc) | (getp1(pc+1)<<16)); pc += 2;
		if(J & 4) {
		  /* Triton and TS-IEEE only */
			wa |= (((AR_HOST_UINT64) getp1(pc)) << 32);
			pc++;
		}
		if(op&07)
			S[I] = load_pvp_word(wa+(long)A[op&07]);
		else
			S[I] = load_pvp_word(wa);
		DISASM4("S%1o\t0x%6.6x,A%1o\t0x%16.16llx\n", I, wa, op&7, S[I]);
		break;

	  case 0130:
	  case 0131:
	  case 0132:
	  case 0133:
	  case 0134:
	  case 0135:
	  case 0136:
	  case 0137:
		prefix_check(prefix);
		wa = (getp1(pc) | (getp1(pc+1)<<16)); pc += 2;
		if(J & 4) {
		  /* Triton and TS-IEEE only */
			wa |= (((AR_HOST_UINT64) getp1(pc)) << 32);
			pc++;
		}
		if(op&07)
			store_pvp_word(wa+(long)A[op&07], S[I]);
		else
			store_pvp_word(wa, S[I]);
		DISASM3("0x%6.6x,A%1o  S%1o\n", wa, op&7, I);
		break;

	  case 0140:				/* Vi Sj&Vk */
		prefix_check(prefix);
		if(J == 0)
			oj = &zero;
		else
			oj = &S[J];
		for(i=0; i<VL; i++)
			V[I*VSZ+i] = *oj & V[K*VSZ+i];
		DISASM3("V%1o\tS%1o&V%1o", I, J, K);
		DISASMV(I, "\t\t");
		break;

	  case 0141:				/* Vi Vj&Vk */
		prefix_check(prefix);
		for(i=0; i<VL; i++)
			V[I*VSZ+i] = V[J*VSZ+i] & V[K*VSZ+i];
		DISASM3("V%1o\tV%1o&V%1o", I, J, K);
		DISASMV(I, "\t\t");
		break;

	  case 0142:				/* Vi Sj!Vk */
		prefix_check(prefix);
		if(J == 0)
			oj = &zero;
		else
			oj = &S[J];
		for(i=0; i<VL; i++)
			V[I*VSZ+i] = *oj | V[K*VSZ+i];
		DISASM3("V%1o\tS%1o|V%1o", I, J, K);
		DISASMV(I, "\t\t");
		break;

	  case 0143:				/* Vi Vj!Vk */
		prefix_check(prefix);
		for(i=0; i<VL; i++)
			V[I*VSZ+i] = V[J*VSZ+i] | V[K*VSZ+i];
		DISASM3("V%1o\tV%1o|V%1o", I, J, K);
		DISASMV(I, "\t\t");
		break;

	  case 0144:				/* Vi Sj\Vk */
		prefix_check(prefix);
		if(J == 0)
			oj = &zero;
		else
			oj = &S[J];
		for(i=0; i<VL; i++)
			V[I*VSZ+i] = *oj ^ V[K*VSZ+i];
		DISASM3("V%1o\tS%1o\\V%1o", I, J, K);
		DISASMV(I, "\t\t");
		break;

	  case 0145:				/* Vi Vj\Vk */
		prefix_check(prefix);
		for(i=0; i<VL; i++)
			V[I*VSZ+i] = V[J*VSZ+i] ^ V[K*VSZ+i];
		DISASM3("V%1o\tV%1o^V%1o", I, J, K);
		DISASMV(I, "\t\t");
		break;

	  case 0146:				/* Vi Sj!Vk&VM */
		prefix_check(prefix);
		if(J == 0)
			oj = &zero;
		else
			oj = &S[J];
		for(i=0; i<VL; i++) {
			if(i < 64)
				n = (VM>>(63-i)) & 1;
			else
				n = (VM1>>(127-i)) & 1;
			if(n)
				V[I*VSZ+i] = *oj;
			else
				V[I*VSZ+i] = V[K*VSZ+i];
		}
		DISASM3("V%1o\tS%1o!V%1o&VM", I, J, K);
		DISASMV(I, "\t");
		break;

	  case 0147:				/* Vi Vj!Vk&VM */
		prefix_check(prefix);
		for(i=0; i<VL; i++) {
			if(i < 64)
				n = (VM>>(63-i)) & 1;
			else
				n = (VM1>>(127-i)) & 1;
			if(n)
				V[I*VSZ+i] = V[J*VSZ+i];
			else
				V[I*VSZ+i] = V[K*VSZ+i];
		}
		DISASM3("V%1o\tV%1o!V%1o&VM", I, J, K);
		DISASMV(I, "\t");
		break;

	  case 0150:				/* Vi Vj<Ak */
		prefix_check(prefix != 0 && prefix != 05400);
		if (prefix == 0) {
			if(K == 0) {
				for(i=0; i<VL; i++)
					V[I*VSZ+i] = V[J*VSZ+i]<<1;
			}
			else if(A[K] &~ (int64)077) {
				for(i=0; i<VL; i++)
					V[I*VSZ+i] = 0;
			}
			else {
				n = A[K];
				for(i=0; i<VL; i++)
					V[I*VSZ+i] = V[J*VSZ+i]<<n;
			}
			DISASM3("V%1o\tV%1o<A%1o", I, J, K);
			DISASMV(I, "\t\t");
		}
		else {
			if(K == 0) {
				for(i=0; i<VL; i++) {
					if(V[0*VSZ+i] &~ (int64)077)
						V[I*VSZ+i] = 0;
					else
						V[I*VSZ+i] = V[J*VSZ+i]<<V[0*VSZ+i];
				}
				DISASM2("V%1o\tV%1o<V0", I, J);
				DISASMV(I, "\t\t");
			}
			else {
				if(A[K] &~ (int64)077) {
					for(i=0; i<VL; i++)
						V[I*VSZ+i] = 0;
				}
				else {
					n = A[K];
					for(i=0; i<VL; i++)
						V[I*VSZ+i] = V[J*VSZ+i]<<n;
				}
				DISASM3("V%1o\tV%1o<A%1o", I, J, K);
				DISASMV(I, "\t\t");
			}
			prefix = 0;
		}
		break;

	  case 0151:				/* Vi Vj>Ak */
		prefix_check(prefix != 0 && prefix != 05400);
		if (prefix == 0) {
			if(K == 0) {
				for(i=0; i<VL; i++)
					V[I*VSZ+i] = V[J*VSZ+i]>>1;
			}
			else if(A[K] &~ (int64)077) {
				for(i=0; i<VL; i++)
					V[I*VSZ+i] = 0;
			}
			else {
				n = A[K];
				for(i=0; i<VL; i++)
					V[I*VSZ+i] = V[J*VSZ+i]>>n;
			}
			DISASM3("V%1o\tV%1o>A%1o", I, J, K);
			DISASMV(I, "\t\t");
		}
		else {
			if(K == 0) {
				for(i=0; i<VL; i++) {
					if(V[0*VSZ+i] &~ (int64)077)
						V[I*VSZ+i] = 0;
					else
						V[I*VSZ+i] = V[J*VSZ+i]>>V[0*VSZ+i];
				}
				DISASM2("V%1o\tV%1o>V0", I, J);
				DISASMV(I, "\t\t");
			}
			else {
				n = A[K];
				if(n &~ (int64)077) {
					for(i=0; i<VL; i++)
						V[I*VSZ+i] = 0;
				}
				else {
					for(i=0; i<VL; i++)
						V[I*VSZ+i] = V[J*VSZ+i]>>n;
				}
				DISASM3("V%1o\tV%1o>A%1o", I, J, K);
				DISASMV(I, "\t\t");
			}
			prefix = 0;
		}
		break;

	  case 0152:				/* Vi Vj,Vj<Ak */
		prefix_check(prefix != 0 && prefix != 05400);
		if (prefix == 0) {
			if(K == 0) {
				for(i=0; i<VL-1; i++)
					V[I*VSZ+i] = (V[J*VSZ+i]<<1) | (V[J*VSZ+i+1]>>63);
				V[I*VSZ+VL-1] = V[J*VSZ+VL-1]<<1;
			}
			else if(A[K] &~ (int64)0177) {
				for(i=0; i<VL; i++)
					V[I*VSZ+i] = 0;
			}
			else if(A[K] & (int64)0100) {
				n = A[K]&077;
				for(i=0; i<VL-1; i++)
					V[I*VSZ+i] = V[J*VSZ+i+1]<<n;
				V[I*VSZ+VL-1] = 0;
			}
			else {
				n = A[K];
				for(i=0; i<VL-1; i++)
					V[I*VSZ+i] = (V[J*VSZ+i]<<n) | (V[J*VSZ+i+1]>>(64-n));
				V[I*VSZ+VL-1] = V[J*VSZ+VL-1]<<n;
			}
			DISASM4("V%1o\tV%1o,V%1o<A%1o", I, J, J, K);
			DISASMV(I, "\t");
		}
		else {
			n = A[K]&077;
			for(i=n; i<VL; i++)
				V[I*VSZ+i-n] = V[J*VSZ+i];
			DISASM3("V%1o\tV%1o,A%1o", I, J, K);
			DISASMV(I, "\t\t");
			prefix = 0;
		}
		break;

	  case 0153:				/* Vi Vj,Vj>Ak */
		prefix_check(prefix != 0 && prefix != 05400);
		if (prefix == 0) {
			if(K == 0) {
				for(i=VL-1; i>0; i--)
					V[I*VSZ+i] = (V[J*VSZ+i]>>1) | (V[J*VSZ+i-1]<<63);
				V[I*VSZ+0] = V[J*VSZ+0]>>1;
			}
			else if(A[K] &~ (int64)0177) {
				for(i=0; i<VL; i++)
					V[I*VSZ+i] = 0;
			}
			else if(A[K] & (int64)0100) {
				n = A[K]&077;
				for(i=VL-1; i>0; i--)
					V[I*VSZ+i] = V[J*VSZ+i-1]>>n;
				V[I*VSZ+0] = 0;
			}
			else {
				n = A[K];
				for(i=VL-1; i>0; i--)
					V[I*VSZ+i] = (V[J*VSZ+i]>>n) | (V[J*VSZ+i-1]<<(64-n));
				V[I*VSZ+0] = V[J*VSZ+0]>>n;
			}
			DISASM4("V%1o\tV%1o,V%1o>A%1o", I, J, J, K);
			DISASMV(I, "\t");
		}
		else {
			if (K == 0) {
				i     = 0;
				j     = 0;
				n     = 0;
				reg64 = 1;
				for(n=0; n<64; j++, n++, reg64 <<= 1) {
					if ((reg64 & VM) != 0)
						V[I*VSZ+(i++)] = V[J*VSZ+j];
				}

				n     = 0;
				reg64 = 1;
				for(n=0; n<64; j++, n++, reg64 <<= 1) {
					if ((reg64 & VM1) != 0)
						V[I*VSZ+(i++)] = V[J*VSZ+j];
				}

				DISASM2("V%1o\tV%1o,[VM]", I, J);
				DISASMV(I, "\t\t");
			}
			else if (K == 1) {
				i     = 0;
				j     = 0;
				n     = 0;
				reg64 = 1;
				for(n=0; n<64; i++, n++, reg64 <<= 1) {
					if ((reg64 & VM) != 0)
						V[I*VSZ+i] = V[J*VSZ+(j++)];
				}

				n     = 0;
				reg64 = 1;
				for(n=0; n<64; i++, n++, reg64 <<= 1) {
					if ((reg64 & VM1) != 0)
						V[I*VSZ+i] = V[J*VSZ+(j++)];
				}

				DISASM2("V%1o,[VM]\tV%1o", I, J);
				DISASMV(I, "\t\t");
			}
			else
				status = UNIMPL_INST;
			prefix = 0;
		}
		break;

	  case 0154:				/* Vi Sj+Vk */
		prefix_check(prefix);
		if(J == 0)
			oj = &zero;
		else
			oj = &S[J];
		for(i=0; i<VL; i++)
			V[I*VSZ+i] = *oj + V[K*VSZ+i];
		DISASM3("V%1o\tS%1o+V%1o", I, J, K);
		DISASMV(I, "\t\t");
		break;

	  case 0155:				/* Vi Vj+Vk */
		prefix_check(prefix);
		for(i=0; i<VL; i++)
			V[I*VSZ+i] = V[J*VSZ+i] + V[K*VSZ+i];
		DISASM3("V%1o\tV%1o+V%1o", I, J, K);
		DISASMV(I, "\t\t");
		break;

	  case 0156:				/* Vi Sj-Vk */
		prefix_check(prefix);
		if(J == 0)
			oj = &zero;
		else
			oj = &S[J];
		for(i=0; i<VL; i++)
			V[I*VSZ+i] = *oj - V[K*VSZ+i];
		DISASM3("V%1o\tS%1o-V%1o", I, J, K);
		DISASMV(I, "\t\t");
		break;

	  case 0157:				/* Vi Vj-Vk */
		prefix_check(prefix);
		for(i=0; i<VL; i++)
			V[I*VSZ+i] = V[J*VSZ+i] - V[K*VSZ+i];
		DISASM3("V%1o\tV%1o-V%1o", I, J, K);
		DISASMV(I, "\t\t");
		break;

	  case 0160:				/* Vi Sj*FVk */
		prefix_check(prefix);
		if(J == 0)
			oj = &zero;
		else
			oj = &S[J];
		rr = &V[I*VSZ];
		ok = &V[K*VSZ];
		for(i=0; i<VL && status==AR_STAT_OK; i++) {
			if(ar_state_register.ar_float_format == AR_CRAY_FLOATING_POINT)
				status = ar_cfmul64((AR_CRAY_64*)rr,
									(AR_CRAY_64*)oj, (AR_CRAY_64*)ok,
									AR_UNROUNDED);
			else {
				status = ar_ifmul64((AR_IEEE_64*)rr,
									(AR_IEEE_64*)oj, (AR_IEEE_64*)ok,
									SR0&RMMASK);
				if(IS_ERROR_STATUS(status))
					status = ar_check_status(status, (AR_IEEE_64*)rr);
			}
			status &= AR_ERROR_STATUS;
			rr++;
			ok++;
		}
		DISASM3("V%1o\tS%1o*FV%1o", I, J, K);
		DISASMV(I, "\t\t");
		break;

	  case 0161:				/* Vi Vj*FVk */
		prefix_check(prefix);
		rr = &V[I*VSZ];
		oj = &V[J*VSZ];
		ok = &V[K*VSZ];
		for(i=0; i<VL && status==AR_STAT_OK; i++) {
			if(ar_state_register.ar_float_format == AR_CRAY_FLOATING_POINT)
				status = ar_cfmul64((AR_CRAY_64*)rr,
									(AR_CRAY_64*)oj, (AR_CRAY_64*)ok,
									AR_UNROUNDED);
			else {
				status = ar_ifmul64((AR_IEEE_64*)rr,
									(AR_IEEE_64*)oj, (AR_IEEE_64*)ok,
									SR0&RMMASK);
				if(IS_ERROR_STATUS(status))
					status = ar_check_status(status, (AR_IEEE_64*)rr);
			}
			status &= AR_ERROR_STATUS;
			rr++;
			oj++;
			ok++;
		}
		DISASM3("V%1o\tV%1o*FV%1o", I, J, K);
		DISASMV(I, "\t\t");
		break;

	  case 0162:
		prefix_check(prefix);
		if(ar_state_register.ar_float_format == AR_CRAY_FLOATING_POINT) {
			status = UNIMPL_INST;
			break;
		}
		if(J == 0)
			oj = &zero;
		else
			oj = &S[J];
		rr = &V[I*VSZ];
		ok = &V[K*VSZ];
		for(i=0; i<VL && status==AR_STAT_OK; i++) {
			status = ar_ifdiv64((AR_IEEE_64*)rr,
					    (AR_IEEE_64*)ok,
					    (AR_IEEE_64*)oj,
					    SR0&RMMASK);
			if(status)
				status = ar_check_status(status, (AR_IEEE_64*)rr);
			rr++;
			ok++;
		}
		DISASM3("V%1o\tS%1o/FV%1o", I, J, K);
		DISASMV(I, "\t\t");
		break;

	  case 0163:
		prefix_check(prefix);
		if(ar_state_register.ar_float_format == AR_CRAY_FLOATING_POINT) {
			status = UNIMPL_INST;
			break;
		}
		rr = &V[I*VSZ];
		oj = &V[J*VSZ];
		ok = &V[K*VSZ];
		for(i=0; i<VL && status==AR_STAT_OK; i++) {
			status = ar_ifdiv64((AR_IEEE_64*)rr,
					    (AR_IEEE_64*)ok,
					    (AR_IEEE_64*)oj,
					    SR0&RMMASK);
			status &= AR_ERROR_STATUS;
			if(status)
				status = ar_check_status(status, (AR_IEEE_64*)rr);
			rr++;
			oj++;
			ok++;
		}
		DISASM3("V%1o\tV%1o/FV%1o", I, J, K);
		DISASMV(I, "\t\t");
		break;

	  case 0164:				/* Vi Sj*RVk */
		prefix_check(prefix != 0 &&
					 !((prefix >= 05501 && prefix <= 05507) ||
					   (prefix >= 05521 && prefix <= 05527) ||
					   (prefix >= 05541 && prefix <= 05547)));
		if(J == 0)
			oj = &zero;
		else
			oj = &S[J];
		if (prefix == 0) {
			rr = &V[I*VSZ];
			ok = &V[K*VSZ];
			for(i=0; i<VL && status==AR_STAT_OK; i++) {
				status = ar_cfmul64((AR_CRAY_64*)rr,
									(AR_CRAY_64*)oj, (AR_CRAY_64*)ok,
									AR_ROUNDED);
				status &= AR_ERROR_STATUS;
				if(status)
					status = ar_check_status(status, (AR_IEEE_64*)rr);
				rr++;
				ok++;
			}
			DISASM3("V%1o\tS%1o*RV%1o", I, J, K);
			DISASMV(I, "\t\t");
		}
		else if (prefix >= 05501 && prefix <= 05507) {
			if(K == 0)
				ok = &sb;
			else
				ok = &S[K];
			cmpres = AR_compare((AR_DATA*)oj, &ieee_float_64,
								(AR_DATA*)ok, &ieee_float_64);
			switch(prefix) {
			case 05501:
				S[I] = (cmpres == AR_Compare_EQ) ? ones : zero;
				break;
			case 05502:
				S[I] = (cmpres != AR_Compare_EQ) ? ones : zero;
				break;
			case 05503:
				S[I] = (cmpres == AR_Compare_GT) ? ones : zero;
				break;
			case 05504:
				S[I] = (cmpres == AR_Compare_LT || cmpres == AR_Compare_EQ)
					   ? ones : zero;
				break;
			case 05505:
				S[I] = (cmpres == AR_Compare_LT) ? ones : zero;
				break;
			case 05506:
				S[I] = (cmpres == AR_Compare_GT || cmpres == AR_Compare_EQ)
					   ? ones : zero;
				break;
			case 05507:
				S[I] = (cmpres == AR_Compare_Unord) ? ones : zero;
				break;
			}
			DISASM5("S%1o\tS%1o,%2.2s,S%1o\t0x%16.16llx\n",
					I, J, "EQNEGTLELTGEUN"+(((prefix&07)-1)*2), K, S[I]);
			prefix = 0;
		}
		else if (prefix >= 05521 && prefix <= 05527) {
			VM = VM1 = 0;
			for(i=0; i<VL; i++) {
				cmpres = AR_compare((AR_DATA*)oj, &ieee_float_64,
									(AR_DATA*)&V[K*VSZ+i], &ieee_float_64);
				switch(prefix) {
				case 05521:
					if (cmpres == AR_Compare_EQ) {
						if(i < 64)
							VM |= ((int64)1<<(63-i));
						else
							VM1 |= ((int64)1<<(127-i));
					}
					break;
				case 05522:
					if (cmpres != AR_Compare_EQ) {
						if(i < 64)
							VM |= ((int64)1<<(63-i));
						else
							VM1 |= ((int64)1<<(127-i));
					}
					break;
				case 05523:
					if (cmpres == AR_Compare_GT) {
						if(i < 64)
							VM |= ((int64)1<<(63-i));
						else
							VM1 |= ((int64)1<<(127-i));
					}
					break;
				case 05524:
					if (cmpres == AR_Compare_LT || cmpres == AR_Compare_EQ) {
						if(i < 64)
							VM |= ((int64)1<<(63-i));
						else
							VM1 |= ((int64)1<<(127-i));
					}
					break;
				case 05525:
					if (cmpres == AR_Compare_LT) {
						if(i < 64)
							VM |= ((int64)1<<(63-i));
						else
							VM1 |= ((int64)1<<(127-i));
					}
					break;
				case 05526:
					if (cmpres == AR_Compare_GT || cmpres == AR_Compare_EQ) {
						if(i < 64)
							VM |= ((int64)1<<(63-i));
						else
							VM1 |= ((int64)1<<(127-i));
					}
					break;
				case 05527:
					if (cmpres == AR_Compare_Unord) {
						if(i < 64)
							VM |= ((int64)1<<(63-i));
						else
							VM1 |= ((int64)1<<(127-i));
					}
					break;
				}
			}
			DISASM5("VM\tS%1o,%2.2s,V%1o\t0x%16.16llx,0x%16.16llx\n",
					J, "EQNEGTLELTGEUN"+(((prefix&07)-1)*2), K, VM, VM1);
			prefix = 0;
		}
		else if (prefix >= 05541 && prefix <= 05547) {
			VM = VM1 = 0;
			for(i=0; i<VL; i++) {
				cmpres = AR_compare((AR_DATA*)&V[J*VSZ+i], &ieee_float_64,
									(AR_DATA*)&V[K*VSZ+i], &ieee_float_64);
				switch(prefix) {
				case 05541:
					if (cmpres == AR_Compare_EQ) {
						if(i < 64)
							VM |= ((int64)1<<(63-i));
						else
							VM1 |= ((int64)1<<(127-i));
					}
					break;
				case 05542:
					if (cmpres != AR_Compare_EQ) {
						if(i < 64)
							VM |= ((int64)1<<(63-i));
						else
							VM1 |= ((int64)1<<(127-i));
					}
					break;
				case 05543:
					if (cmpres == AR_Compare_GT) {
						if(i < 64)
							VM |= ((int64)1<<(63-i));
						else
							VM1 |= ((int64)1<<(127-i));
					}
					break;
				case 05544:
					if (cmpres == AR_Compare_LT || cmpres == AR_Compare_EQ) {
						if(i < 64)
							VM |= ((int64)1<<(63-i));
						else
							VM1 |= ((int64)1<<(127-i));
					}
					break;
				case 05545:
					if (cmpres == AR_Compare_LT) {
						if(i < 64)
							VM |= ((int64)1<<(63-i));
						else
							VM1 |= ((int64)1<<(127-i));
					}
					break;
				case 05546:
					if (cmpres == AR_Compare_GT || cmpres == AR_Compare_EQ) {
						if(i < 64)
							VM |= ((int64)1<<(63-i));
						else
							VM1 |= ((int64)1<<(127-i));
					}
					break;
				case 05547:
					if (cmpres == AR_Compare_Unord) {
						if(i < 64)
							VM |= ((int64)1<<(63-i));
						else
							VM1 |= ((int64)1<<(127-i));
					}
					break;
				}
			}
			DISASM5("VM\tV%1o,%2.2s,V%1o\t0x%16.16llx,0x%16.16llx\n",
					J, "EQNEGTLELTGEUN"+(((prefix&07)-1)*2), K, VM, VM1);
			prefix = 0;
		}
		break;

	  case 0165:				/* Vi Vj*[RLU]Vk */
		prefix_check(prefix != 0 && prefix != 05400);
		if(ar_state_register.ar_float_format == AR_IEEE_FLOATING_POINT) {
			if (prefix == 0) {
				for(i=0; i<VL; i++)
					V[I*VSZ+i] = V[J*VSZ+i] * V[K*VSZ+i];
				DISASM3("V%1o\tV%1o*LV%1o", I, J, K);
			}
			else {
				rr = &V[I*VSZ];
				oj = &V[J*VSZ];
				ok = &V[K*VSZ];
				for(i=0; status==AR_STAT_OK && i<VL; i++) {
					status = ar_imul64u(rr, *oj, *ok);
					rr++;
					oj++;
					ok++;
				}
				DISASM3("V%1o\tV%1o*UV%1o\n", I, J, K);
				prefix = 0;
			}
		}
		else {					/* Vi Vj*RVk */
			rr = &V[I*VSZ];
			oj = &V[J*VSZ];
			ok = &V[K*VSZ];
			for(i=0; i<VL && status==AR_STAT_OK; i++) {
				status = ar_cfmul64((AR_CRAY_64*)rr,
									(AR_CRAY_64*)oj, (AR_CRAY_64*)ok,
									AR_ROUNDED);
				status &= AR_ERROR_STATUS;
				rr++;
				oj++;
				ok++;
			}
			DISASM3("V%1o\tV%1o*RV%1o", I, J, K);
		}
		DISASMV(I, "\t\t");
		break;

	  case 0166:				/* Vi Sj*IVk */
		prefix_check(prefix != 0 && prefix != 05400);
		if(J == 0)
			oj = &zero;
		else
			oj = &S[J];
		rr = &V[I*VSZ];
		ok = &V[K*VSZ];
		if(ar_state_register.ar_float_format == AR_IEEE_FLOATING_POINT) {
			if (prefix == 0) {
				for(i=0; i<VL; i++)
					V[I*VSZ+i] = *oj * V[K*VSZ+i];
				DISASM3("V%1o\tS%1o*LV%1o", I, J, K);
			}
			else {
				for(i=0; status==AR_STAT_OK && i<VL; i++) {
					status = ar_imul64u(rr, *oj, *ok);
					rr++;
					ok++;
				}
				DISASM3("V%1o\tS%1o*UV%1o\n", I, J, K);
				prefix = 0;
			}
		}
		else {
			for(i=0; i<VL && status==AR_STAT_OK; i++) {
				status = ar_cfmul64((AR_CRAY_64*)rr,
									(AR_CRAY_64*)oj, (AR_CRAY_64*)ok,
									AR_RECIPROCAL_ITERATION);
				status &= AR_ERROR_STATUS;
				rr++;
				ok++;
			}
			DISASM3("V%1o\tS%1o*IV%1o", I, J, K);
		}
		DISASMV(I, "\t\t");
		break;

	  case 0167:				/* Vi Vj*IVk */
		prefix_check(prefix);
		rr = &V[I*VSZ];
		oj = &V[J*VSZ];
		if(ar_state_register.ar_float_format == AR_CRAY_FLOATING_POINT) {
			ok = &V[K*VSZ];
			for(i=0; i<VL && status==AR_STAT_OK; i++) {
				status = ar_cfmul64((AR_CRAY_64*)rr,
									(AR_CRAY_64*)oj, (AR_CRAY_64*)ok,
									AR_RECIPROCAL_ITERATION);
				rr++;
				oj++;
				ok++;
			}
			DISASM3("V%1o\tV%1o*RV%1o", I, J, K);
		}
		else if(K <= 2) {
			for(i=0; i<VL && status==AR_STAT_OK; i++) {
				switch(K) {
				case 0:
					status = ar_ifix64((AR_INT_64*)rr, (const AR_IEEE_64*)oj,
									   64, AR_ROUND_ZERO);
					break;
				case 1:
					status = ar_ifix64((AR_INT_64*)rr, (const AR_IEEE_64*)oj,
									   64, AR_ROUND_NEAREST);
					break;
				case 2:
					status = ar_iflt64((AR_IEEE_64*)rr, (const AR_INT_64*)oj,
									   0, AR_ROUND_ZERO);
					break;
				}
				if(IS_ERROR_STATUS(status))
					status = ar_check_status(status, (AR_IEEE_64*)rr);
				status &= AR_ERROR_STATUS;
				rr++;
				oj++;
			}
			DISASM3("V%1o\t%s,V%1o", I, (K==0)?"INT":(K==1)?"RINT":"FLT", J);
		}
		else
			status = UNIMPL_INST;
		DISASMV(I, "\t\t");
		break;

	  case 0170:				/* Vi Sj+FVk */
		prefix_check(prefix);
		if(J == 0)
			oj = &zero;
		else
			oj = &S[J];
		rr = &V[I*VSZ];
		ok = &V[K*VSZ];
		for(i=0; i<VL && status==AR_STAT_OK; i++) {
			if(ar_state_register.ar_float_format == AR_CRAY_FLOATING_POINT)
				status = ar_cfadd64((AR_CRAY_64*)rr,
									(AR_CRAY_64*)oj, (AR_CRAY_64*)ok);
			else {
				status = ar_ifadd64((AR_IEEE_64*)rr,
									(AR_IEEE_64*)oj, (AR_IEEE_64*)ok,
									SR0&RMMASK);
				if(IS_ERROR_STATUS(status))
					status = ar_check_status(status, (AR_IEEE_64*)rr);
			}
			status &= AR_ERROR_STATUS;
			rr++;
			ok++;
		}
		DISASM3("V%1o\tS%1o+FV%1o", I, J, K);
		DISASMV(I, "\t\t");
		break;

	  case 0171:				/* Vi Vj+FVk */
		prefix_check(prefix);
		rr = &V[I*VSZ];
		oj = &V[J*VSZ];
		ok = &V[K*VSZ];
		for(i=0; i<VL && status==AR_STAT_OK; i++) {
			if(ar_state_register.ar_float_format == AR_CRAY_FLOATING_POINT)
				status = ar_cfadd64((AR_CRAY_64*)rr,
									(AR_CRAY_64*)oj, (AR_CRAY_64*)ok);
			else {
				status = ar_ifadd64((AR_IEEE_64*)rr,
									(AR_IEEE_64*)oj, (AR_IEEE_64*)ok,
									SR0&RMMASK);
				if(IS_ERROR_STATUS(status))
					status = ar_check_status(status, (AR_IEEE_64*)rr);
			}
			status &= AR_ERROR_STATUS;
			rr++;
			oj++;
			ok++;
		}
		DISASM3("V%1o\tV%1o+FV%1o", I, J, K);
		DISASMV(I, "\t\t");
		break;

	  case 0172:				/* Vi Sj-FVk */
		prefix_check(prefix);
		if(J == 0)
			oj = &zero;
		else
			oj = &S[J];
		rr = &V[I*VSZ];
		ok = &V[K*VSZ];
		for(i=0; i<VL && status==AR_STAT_OK; i++) {
			if(ar_state_register.ar_float_format == AR_CRAY_FLOATING_POINT)
				status = ar_cfsub64((AR_CRAY_64*)rr,
									(AR_CRAY_64*)oj, (AR_CRAY_64*)ok);
			else {
				status = ar_ifsub64((AR_IEEE_64*)rr,
									(AR_IEEE_64*)oj, (AR_IEEE_64*)ok,
									SR0&RMMASK);
				if(IS_ERROR_STATUS(status))
					status = ar_check_status(status, (AR_IEEE_64*)rr);
			}
			status &= AR_ERROR_STATUS;
			rr++;
			ok++;
		}
		DISASM3("V%1o\tS%1o-FV%1o", I, J, K);
		DISASMV(I, "\t\t");
		break;

	  case 0173:				/* Vi Vj-FVk */
		prefix_check(prefix);
		rr = &V[I*VSZ];
		oj = &V[J*VSZ];
		ok = &V[K*VSZ];
		for(i=0; i<VL && status==AR_STAT_OK; i++) {
			if(ar_state_register.ar_float_format == AR_CRAY_FLOATING_POINT)
				status = ar_cfsub64((AR_CRAY_64*)rr,
									(AR_CRAY_64*)oj, (AR_CRAY_64*)ok);
			else {
				status = ar_ifsub64((AR_IEEE_64*)rr,
									(AR_IEEE_64*)oj, (AR_IEEE_64*)ok,
									SR0&RMMASK);
				if(IS_ERROR_STATUS(status))
					status = ar_check_status(status, (AR_IEEE_64*)rr);
			}
			status &= AR_ERROR_STATUS;
			rr++;
			oj++;
			ok++;
		}
		DISASM3("V%1o\tV%1o-FV%1o", I, J, K);
		DISASMV(I, "\t\t");
		break;

	  case 0174:
		prefix_check(prefix);
		if(K == 0) {			/* Vi /HVj */
			rr = &V[I*VSZ];
			oj = &V[J*VSZ];
			if(ar_state_register.ar_float_format == AR_CRAY_FLOATING_POINT) {
				for(i=0; i<VL && status==AR_STAT_OK; i++) {
					status = ar_c1frecip((AR_CRAY_64*)rr, (AR_CRAY_64*)oj);
					status &= AR_ERROR_STATUS;
					rr++;
					oj++;
				}
				DISASM2("V%1o\t/HV%1o", I, J);
			}
			else {
				for(i=0; i<VL && status==AR_STAT_OK; i++) {
					status = ar_sqrt((ar_data*)rr, &ieee_float_64,
									 (ar_data*)oj, &ieee_float_64);
					if(IS_ERROR_STATUS(status))
						status = ar_check_status(status, (AR_IEEE_64*)rr);
					status &= AR_ERROR_STATUS;
					rr++;
					oj++;
				}
				DISASM2("V%1o\tSQRT,V%1o", I, J);
			}
			DISASMV(I, "\t\t");
		}
		else if(K <= 3) {		/* Vi [PQZ]Vj */
			for(i=0; i<VL; i++) {
				switch(K) {
				case 1:
				case 2:
					for(j=0,n=0; j<64; j++)
						if((V[J*VSZ+i]>>j)&1)
							n++;
					if(K == 2)
						n &= 1;
					break;
				case 3:
					for(n=0; n<64; n++)
						if(V[J*VSZ+i]>>(63-n)) break;
					break;
				}
				V[I*VSZ+i] = n;
			}
			DISASM3("V%1o\t%cV%1o", I, (K==1)?'P':(K==2)?'Q':'Z', J);
			DISASMV(I, "\t\t");
		}
		else
			status = UNIMPL_INST;
		break;

	  case 0175:				/* [Vi,]VM Vj,[ZNPM] */
		prefix_check(prefix);
		VM = VM1 = 0;
		n = 0;
		if(K & 2) { 	/* [PM] */
			for(i=0; i<VL; i++) {
				if((1-(V[J*VSZ+i]>>63)) ^ (K&1)) {
					if(i < 64)
						VM |= ((int64)1<<(63-i));
					else
						VM1 |= ((int64)1<<(127-i));
					if(K & 4)
						V[I*VSZ+n++] = i;
				}
			}
		}
		else {			/* Else [ZN] */
			for(i=0; i<VL; i++) {
				if(!V[J*VSZ+i] ^ (K&1)) {
					if(i < 64)
						VM |= ((int64)1<<(63-i));
					else
						VM1 |= ((int64)1<<(127-i));
					if(K & 4)
						V[I*VSZ+n++] = i;
				}
			}
		}
		if(K < 4) {
			DISASM3("VM\tV%1o,%1.1s\t\t0x%16.16llx", J, "ZNPM"+(K&03), VM);
			if(VL > 64)
				DISASM1(" %16.16llx\n", VM1);
			else
				DISASM0("\n");
		}
		else {
			DISASM3("VM,V%1o\tV%1o,%1.1s", I, J, "ZNPM"+(K&03));
			if(VL > 64)
				DISASM2("\t\t0x%16.16llx %16.16llx\n", VM, VM1);
			else
				DISASM1("\t\t0x%16.16llx\n", VM);
			DISASMV(I, "\t\t\t\t\t");
		}
		break;

	  case 0176:
		prefix_check(prefix != 0 && prefix != 05400);
		if(prefix == 0 && J == 0) {
			if(K == 0)
				n = 1;
			else
				n = A[K];
			for(i=0; i<VL; i++)
				V[I*VSZ+i] = load_pvp_word((long)A[0]+i*n);
			DISASM2("V%1o\t,A0,A%1o", I, K);
		}
		else if(prefix == 0) {
			for(i=0; i<VL; i++)
				V[I*VSZ+i] = load_pvp_word((long)(A[0]+V[K*VSZ+i]));
			DISASM2("V%1o\t,A0,V%1o", I, K);
		}
		else {
			for(i=0; i<VL; i++) {
				V[I*VSZ+i] = load_pvp_word((long)(A[0]+V[K*VSZ+i]));
				V[J*VSZ+i] = load_pvp_word((long)(A[K]+V[K*VSZ+i]));
			}
			DISASM4("V%1o:V%1o\t,A0:A%1o,V%1o", I, J, K, K);
		}
		DISASMV(I, "\t\t");
		break;

	  case 0177:
		prefix_check(prefix);
		if(I == 0) {
			if(K == 0)
				n = 1;
			else
				n = A[K];
			for(i=0; i<VL; i++)
				store_pvp_word((long)A[0]+i*n, V[J*VSZ+i]);
			DISASM2(",A0,A%1o\tV%1o\n", K, J);
		}
		else {
			for(i=0; i<VL; i++)
				store_pvp_word((long)(A[0]+V[K*VSZ+i]), V[J*VSZ+i]);
			DISASM2(",A0,V%1o\tV%1o\n", K, J);
		}
		break;
	  }

	}

	ar_state_register.ar_truncate_bits = save_trunc_bits;

	if(status == UNIMPL_INST) {
		DISASM1("%6.6o\n", p1);
		ar_internal_error(2013, __FILE__, __LINE__);
		status = AR_STAT_UNDEFINED;
	}
	else if(status == UNIMPL_PRFX) {
		DISASM2("%6.6o %6.6o\n", prefix, p1);
		ar_internal_error(2013, __FILE__, __LINE__);
		status = AR_STAT_UNDEFINED;
	}

	return(status);
}

typedef union {
	struct {
		unsigned int p1: 32;
		unsigned int p2: 32;
	}     ui32;
	int64 ui64;
} u;
#define LO32(i)    ((int64) (((u*)&(i))->ui32.p2))
#define HI32(i)    ((int64) (((u*)&(i))->ui32.p1))

static int
ar_imul64u(int64* result, int64 opnd1, int64 opnd2)
{
	int64	reg64, sum64, tmp64;

	/*
	 * Special case "Si Sj*USk".  Eventually, with 128-bit ints,
	 * we'll be able to support this directly.  For now, we just
	 * do it by hand in 32-bit chunks:
	 *
	 *          a  b
	 *       x  c  d
	 *      --------
	 *         ad bd
	 *      ac bc
	 *      --------
	 *      ac+ad+bd
	 *        +bc
	 */
	sum64 = LO32(opnd1) * LO32(opnd2);		/* bd */
	reg64 = HI32(opnd1) * LO32(opnd2);		/* ad */
	tmp64 = LO32(opnd1) * HI32(opnd2);		/* bc */

	/* Sum from right propagating carry (in high 32 bits of sum64) */
	sum64 = LO32(reg64) + LO32(tmp64) + HI32(sum64);
	sum64 = HI32(reg64) + HI32(tmp64) + HI32(sum64);
	reg64 = HI32(opnd1) * HI32(opnd2);		/* ac */
	*result  = reg64 + sum64;

	return AR_STAT_OK;
}

#define CRAY_AHDR_SIZE	8	/* # words in cray a.out files */

#define PAGESZ		256		/* # words in data space pages read in */
#define PAGESHFT	8		/* Shift to get page # */

static int	idf_fd; 		/* Intrinsic data file file descriptor */
static long idf_size;		/* Intrinsic data file size (in bytes) */
static long text_size;		/* Size of text space */
static long data_size;		/* Size of data space */
static long data_offset;	/* Offset to data space */
static int	shared_text=0;	/* =1 for a shared text data file */
static long data_address;	/* Offset in data file to data section */

static int64** data_page;	/* Memory locations of data pages read in */
static int  ndata_pages;	/* Max # of data space pages */

static void
open_arith_file()
{
	int 	fd;
	int 	i,v;
	int 	ieee;

	int64	ahdr[CRAY_AHDR_SIZE];
	char*	craylibs;
	char*	str;
	char*	work;

	/* Find and open intrinsic evaluation data file */

	stack=(int64*)malloc((STACK_SIZE+1+5*MAX_ARGS)*8);
	craylibs=(char*)getenv("CRAYLIBS");
	work = (char*)stack;

	/* Use the arith data file at the CRAYLIBS path if it is set */

	if(craylibs != NULL) {
		strcpy(work, craylibs);
		v=strlen(work);
		strcpy(&work[v],"/arith");
		idf_fd=open(work, O_RDONLY);
	}

	/* Check default locations for a (native-only) arith data file */

#if _CRAY
	else {
		strcpy(work, "/opt/ctl/craylibs/craylibs/arith");
		v = 27;
		idf_fd=open(work, O_RDONLY);
		if(idf_fd < 0) {
			strcpy(work, "/lib/arith");
			v = 4;
			idf_fd=open(work, O_RDONLY);
		}
	}
#endif
	if(idf_fd < 0)
		ar_internal_error(2009, __FILE__, __LINE__);

	/* Load intrinsic evaluation data file into heap memory */

	idf_size=lseek(idf_fd, 0, SEEK_END);
	if(idf_size > 8*8) {
		lseek(idf_fd, 0, SEEK_SET);
		read(idf_fd, ahdr, CRAY_AHDR_SIZE*8);
		if((ahdr[0]>>32) == 0 && (ahdr[0]&0xffff) == 0407) {
			ar_mach_type = (ahdr[0]>>16)&0xff;
			ieee = (ahdr[0]>>29)&1;
			shared_text = (ahdr[0]>>31)&1;
			text_size = ahdr[1];
			data_size = ahdr[2];
			/* Check primary machine type */
			if(ar_mach_type >= YMP && ar_mach_type <= T3E) {
				if(ar_mach_type != 9)
					code = (unsigned char*)malloc(text_size*8);
				if(code == NULL)
					idf_size = -3;		/* Could not malloc text space */
			}
			else
				idf_size = -2;
		}
		else
			idf_size = -1;				/* Bad magic */
	}
	else
		idf_size = 0;

	/* Validate intrinsic evaluation data file contents and version info */

	if(idf_size > 0) {
		data_address = CRAY_AHDR_SIZE+text_size;
		data_offset = (1-shared_text)*text_size;
		read(idf_fd, code, text_size*8);
		str = (char*)strnstrn((char*)code, text_size*8,
			  "@(#)PVP arith/libmv2_version ", 29);
		if(str != NULL)
			strcpy(AR_libmv2, str+14);
		else {
			lseek(idf_fd, idf_size-64, SEEK_SET);
			read(idf_fd, stack, 64);
			v=63;
			while(v > 0) {
				if(strncmp((char*)&stack[v], "libmv2_version", 14) == 0) {
					strcpy(AR_libmv2, (char*)&stack[v]);
					if(strncmp((char*)&stack[v-13], "PVP arith", 9))
						idf_size=-1;
					break;
				}
				if(stack[v] == '\n')
					stack[v]='\0';
				v--;
			}
			if(v <= 0)
				idf_size = -1;
			v = 0;
		}
	}

	if(idf_size < 0)
	   ar_internal_error(2010, __FILE__, __LINE__);

	/* Validate probable libm.a version that will be used for loading */

	if(v > 0 && AR_libmv2[2] != 'x' && AR_libmv2[15+2] != 'x') {
		strcpy(&work[v], "/libm.a");
		fd=open(work, O_RDONLY);
		if(fd < 0) {
			strcpy(&work[v], "/../libm.a");
			fd=open(work, O_RDONLY);
		}
		if(fd >= 0) {
			read(fd, work, STACK_SIZE*8);
			close(fd);
			for(v=0; v<STACK_SIZE*8-27; v++)
				if(strncmp((char*)&work[v], "@(#)libmv2_version", 18) == 0) break;
			if(v < STACK_SIZE*8-27) {
				v+=19;
				for(i=0; i<8; i+=2)
					if(work[v+i]!='x' && AR_libmv2[15+i]!='x' &&
					   work[v+i]!=AR_libmv2[15+i])
						fd=-2;
			}
			else
				fd=-3;
		}

		/* If we are cross-compiling, libm.a need not exist */

		else {
#if _CRAY
#if _CRAYIEEE
			fd=-ieee;		/* Ok if cross-compiling to Cray FP system */
#else
			fd=1-ieee;		/* Ok if cross-compiling to IEEE FP system */
#endif
#else
			fd=0;			/* Ok since we are cross-compiling */
#endif
		}
		if(fd < 0)
			PRINTMSG(0, 2016, Logfile_Warning, 0, "YMP/C90", "0", "", "");
	}

	/* Define known numerical characteristics of each machine type */

	switch(ar_mach_type) {

	case YMP:
	case C90:
	case T90:
		if(ar_mach_type == T90 && ieee) {
			ar_state_register.ar_float_format = AR_IEEE_FLOATING_POINT;
			ar_state_register.ar_rounding_mode = AR_ROUND_NEAREST;
			ar_state_register.ar_underflow_mode = AR_UNDERFLOW_TO_PLUS_ZERO;
			ar_state_register.ar_denorms_trap = 0;
			ar_state_register.ar_128bit_format = AR_128BIT_EXTENDED_DOUBLE;
			ar_rounding_modes = (1<<AR_ROUND_NEAREST) |
								(1<<AR_ROUND_ZERO) |
								(1<<AR_ROUND_PLUS_INFINITY) |
								(1<<AR_ROUND_MINUS_INFINITY);
			ar_underflow_modes = 1<<AR_UNDERFLOW_TO_PLUS_ZERO;
		}
		else {
			ar_state_register.ar_float_format = AR_CRAY_FLOATING_POINT;
			ar_state_register.ar_rounding_mode = AR_ROUNDED;
			ar_state_register.ar_underflow_mode = AR_UNDERFLOW_TO_PLUS_ZERO;
			ar_state_register.ar_denorms_trap = 0;
			ar_state_register.ar_128bit_format = AR_128BIT_DOUBLE_DOUBLE;
			ar_rounding_modes = (1<<AR_ROUNDED) | (1<<AR_UNROUNDED);
			ar_underflow_modes = 1<<AR_UNDERFLOW_TO_PLUS_ZERO;
		}
		AMASK = 0xffffffff;
		ASB   = 0x80000000;
		set_a_size(32);
		if(ar_mach_type == YMP) {
			JSZP = 2;
			VSZ = 64;
		}
		else {
			JSZP = 3;
			VSZ = 128;
			if(ar_mach_type == T90) {
				AMASK  |= (AMASK<<32);
				ASB   <<= 32;
				set_a_size(64);
			}
		}
		break;

	case T3D:
		ar_state_register.ar_float_format = AR_IEEE_FLOATING_POINT;
		ar_state_register.ar_rounding_mode = AR_ROUND_NEAREST;
		ar_state_register.ar_underflow_mode = AR_UNDERFLOW_TO_PLUS_ZERO;
		ar_state_register.ar_denorms_trap = 1;
		ar_state_register.ar_128bit_format = AR_128BIT_EXTENDED_DOUBLE;
		ar_rounding_modes = (1<<AR_ROUND_NEAREST);
		ar_underflow_modes = 1<<AR_UNDERFLOW_TO_PLUS_ZERO;
		JSZP = VSZ = 0;
		break;

	case T3E:
		ar_state_register.ar_float_format = AR_IEEE_FLOATING_POINT;
		ar_state_register.ar_rounding_mode = AR_ROUND_NEAREST;
		ar_state_register.ar_underflow_mode = AR_UNDERFLOW_TO_PLUS_ZERO;
		ar_state_register.ar_denorms_trap = 1;
		ar_state_register.ar_128bit_format = AR_128BIT_EXTENDED_DOUBLE;
		ar_rounding_modes = (1<<AR_ROUND_NEAREST) |
							(1<<AR_ROUND_ZERO) |
							(1<<AR_ROUND_PLUS_INFINITY) |
							(1<<AR_ROUND_MINUS_INFINITY);
		ar_underflow_modes = 1<<AR_UNDERFLOW_TO_PLUS_ZERO;
		JSZP = VSZ = 0;
		break;
	}

	if(JSZP == 3 && text_size > 0x3fff) {
		/* Current text segment sizes are less than 64K parcels.  Therefore,
		 * C90/T90 instructions 006..017 have a zero 3rd parcel which is skipped
		 * in the instruction simulation in ar_sim.  When/if the text segment
		 * grows to more than 64K parcels, this test will cause a failure
		 * and the branch instruction simulation will have to start using the
		 * 3rd parcel.
		 */
		 ar_internal_error(2012, __FILE__, __LINE__);
	}

	/* Allocate data space page mapping array */
	/* Note: 1 extra page for uninitialized data */

	ndata_pages = ((data_size+PAGESZ-1)>>PAGESHFT)+1;
	data_page = (int64**)malloc(ndata_pages*8);
	for(i=0; i<ndata_pages; i++)
		data_page[i] = NULL;

	if(VSZ)
		V = (int64*)malloc(VSZ*8*8);	/* Allocate V-register space */
}

static int64
load_pvp_word(vaddr)
long	vaddr;
{
	int 	p;
	int 	n;
	long	disk_address;
	long	offset = vaddr&0xffffff;
	int 	segment = (vaddr>>24)&0xff;

	int64	result;

	switch (segment) {

	case DATA:
		offset -= data_offset;
		p = offset>>PAGESHFT;
		if(data_page[p] != NULL)
			return data_page[p][offset&(PAGESZ-1)];

		/* Check for load before store in uninitialized data space */
		if(p >= (ndata_pages-1))
			ar_internal_error(2012, __FILE__, __LINE__);

		/* Always allocate a full page for possible stores into uninit'd data */
		data_page[p] = (int64*)malloc(PAGESZ*8);

		disk_address = (data_address+(offset&~(PAGESZ-1)))*8;
		n = PAGESZ*8;
		if(n > (idf_size-disk_address)) {
			n = idf_size-disk_address;
			memset((char*)(data_page[p])+n, 0xfe, PAGESZ*8-n);
		}
		if(data_page[p] == NULL)
			ar_internal_error(2010, __FILE__, __LINE__);
		lseek(idf_fd, disk_address, SEEK_SET);
		if(read(idf_fd, data_page[p], n) != n)
			ar_internal_error(2010, __FILE__, __LINE__);

		return data_page[p][offset&(PAGESZ-1)];

	case STACK:
		if(offset < (STACK_SIZE+1+5*MAX_ARGS))
			return stack[offset];
		break;

	default:
		if(segment < n_external_addresses && external_address[segment] != NULL) {
			if(external_length[segment] > offset) {
				memcpy(&result, external_address[segment]+offset*8, 8);
				return result;
			}
			/* Allow up to 64 words of unusable slop to be loaded */
			/* See first vector load in memchr.s where this is needed */
			if(external_length[segment] > (offset-64)) {
				result = 0xf0f1f2f3;
				return result;
			}
		}

	}

	ar_internal_error(2012, __FILE__, __LINE__);
}

static void
store_pvp_word(vaddr, result)
long	vaddr;
int64	result;
{
	int 	p;
	long	offset = vaddr&0xffffff;
	int 	segment = (vaddr>>24)&0xff;

	switch (segment) {

	case DATA:
		offset -= data_offset;
		/* Allow store only beyond the end of initialized data space */
		if(offset < data_size)
			ar_internal_error(2012, __FILE__, __LINE__);

		p = offset>>PAGESHFT;
		/* If the last initialized data page hasn't been read, do dummy load */
		if(data_page[p] == NULL && p == (ndata_pages-2))
			load_pvp_word((DATA<<24) | (data_offset+(offset&~(PAGESZ-1))));

		if(data_page[p] != NULL) {
			data_page[p][offset&(PAGESZ-1)] = result;
			return;
		}

		/* Allow store only into 1st full page of uninitialized data space */
		if(p != (ndata_pages-1))
			ar_internal_error(2012, __FILE__, __LINE__);

		data_page[p] = (int64*)malloc(PAGESZ*8);
		memset((char*)data_page[p], 0xfe, PAGESZ*8);
		data_page[p][offset&(PAGESZ-1)] = result;
		return;

	case STACK:
		if(offset < (STACK_SIZE+1+5*MAX_ARGS)) {
			stack[offset] = result;
			return;
		}
		break;

	default:
		if(segment < n_external_addresses && external_address[segment] != NULL) {
			if(external_length[segment] > offset) {
				memcpy(external_address[segment]+offset*8, &result, 8);
				return;
			}
			/* Allow ORE (without actual store) iff value matches that
			 * returned by load_pvp_word above.
			 */
			if(external_length[segment] > (offset-64) && result == 0xf0f1f2f3)
				return;
		}
	}

	ar_internal_error(2012, __FILE__, __LINE__);
}

#endif
