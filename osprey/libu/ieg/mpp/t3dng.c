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


#pragma ident "@(#) libu/ieg/mpp/t3dng.c	92.1	06/25/99 14:59:36"
/*
 * This file contains routines which do nontrivial conversions
 * between IEEE workstations and Cray IEEE (IEG2CRI/CRI2IEG)
 * involving overflow, underflow (denormals), or size conversion.

 * Names are from"2"result"_"fff"T"rrr"_ONCRI", where:
 *	from   is the input to conversion: IEG or CRI (Cray IEEE)
 *	result is the result of conversion: IEG or CRI (Cray IEEE)
 *	fff    is the size in bits of items to convert,
 *	T      is the type: I (integer), R(real/double),
 *			L(logical), or C(complex),
 *	rrr    is the size in bits of items resulting
 * For complex, fff and rrr refer to the size of one real.

 * On overflow (input finite and result, before rounding, is too large)
 * increment external counter O@T3D.  Return signed infinity
 * (if real) or undefined value (if integer).
 * CRI2IEG rounds to infinity without overflow (consistent with CRI2CRY).

 * On underflow (rounded result between smallest real and zero):
 * Internal result: return signed zero or infinity;
 * External result: return denormal if possible, else signed 0.

 * Most of the 32-bit conversion routines here are T3x (not PVP)
 * because of pointers to short 32-bit.  The nonhardware 32/64 floating
 * conversion has been tested on the YMP and should be adaptable
 * to Cray T90/IEEE.  Integer 16/64 conversion should work on
 * both.  It does not use short.
 
 * CRY2CRI uses similar functions to convert single variables.
 * Since T3x inlining is not available yet, routines
 * here convert entire arrays instead.

 * Fairly long two part macros were used in an attempt to make
 * code common for real and complex conversions and
 * make routine conversion as straight line as possible, but
 * C moved the exceptions back in line.  T3x assembly
 * would be twice as fast as the current C code.
 * It's not known if current
 * performance is better than the simpler approach.
 */

unsigned long O@T3D;

#define TYPE32R float
#define TYPE64R double
#define TYPE32I short
#define TYPE64I long

#define	DIEEEMX	2047	/* IEEE 64-bit maximum biased exponent (03777)	*/
#define	DIEEEBS	1023	/* IEEE 64-bit exponent bias (01777)	*/
#define	DIEEEMN	52	/* IEEE 64-bit mantissa length 		*/
#define	DIEEEXP	11	/* IEEE 64-bit exponent length 		*/
#define DIEEESNMASK ( 1<<(DIEEEMN+DIEEEXP) )		/* sign mask  	*/
#define DIEEEXPMASK ( ( (1<<DIEEEXP)-1 )<<DIEEEMN )	/* exponent mask */

#define	SIEEEMX	255	/* IEEE 32-bit maximum biased exponent (0377)	*/
#define	SIEEEBS	127	/* IEEE 32-bit exponent bias (0177)	*/
#define	SIEEEMN	23	/* IEEE 32-bit mantissa length 		*/
#define	SIEEEXP	8	/* IEEE 32-bit exponent length 		*/
#define SIEEESNMASK ( 1<<(SIEEEMN+SIEEEXP) )		/* sign mask  	*/
#define SIEEEXPMASK ( ( (1<<SIEEEXP)-1 )<<SIEEEMN )	/* exponent mask */

#define	IEEENAN	01	/* IEEE NaN mantissa (arbitrary)	*/

#define	ROUND	1	/* 1: biased rounding, 0: no rounding	*/

/*
 *	_IEG2CRI_64R32_ONCRI	Translate IEEE REAL (64-bit) to T3D 32-bit
 *	Denormals are not created.
 *	No overflow on rounding up to infinity.
 */

#define IEG2CRI_64R32 \
{ \
	unsigned long	man, sign, newman; \
	register long	exp; \
	register short	noflow; \
 \
	*sp = (short) 0; \
	if( datum != 0 ) {      /* +0.0 */ \
 \
 		exp	= (datum+datum) >> (64-1-DIEEEXP); \
		noflow	= (exp == DIEEEMX); \
		exp	= exp - DIEEEBS; \
		man = datum << (1+DIEEEXP); \
		sign	= datum >> (64-1); \
 \
		sign	= sign << (SIEEEMN+SIEEEXP); \
		exp	= exp + SIEEEBS ; \
		*sp = (short) sign; \
		if ( exp > 0 ) {          /* -0.0, denormal--> signed 0.0 */ \
 \
		    if (  noflow ){ \
		        exp = SIEEEMX ; \
		        if ( man != 0) newman = IEEENAN ; \
		    } \
		    else { \
		        newman = man >> (64- SIEEEMN); \
		        if( ROUND !=0) newman +=  1&( man>>(64-1- SIEEEMN)); \
		        /* rounding overflow is silent (as in cry2cri) */ \
 \
		        if ( exp >= SIEEEMX ) { \
		            exp = SIEEEMX ; \
    		            if ( man != 0) newman = IEEENAN ; \
		            if( noflow ) O@T3D = O@T3D + 1; \
		        } \
		    } \
		    *sp	= sign + newman + ( exp<< SIEEEMN); \
		} \
	} \
} \
/* end macro */


void
_IEG2CRI_64R32(
unsigned long datum,
short *sp
)
{
	IEG2CRI_64R32
}


void
_IEG2CRI_64R32_ONCRI(
unsigned TYPE64I *iptr,
TYPE32I *sp,
int *numadd,
int *strideadd
)
{ 
	unsigned TYPE64I datum ;
	extern unsigned TYPE64I O@T3D ;
	TYPE64I i;
	for( i =0; i< *numadd ; i ++, iptr += 1, sp += *strideadd ){
		datum = *iptr ;
		IEG2CRI_64R32
	} /* end for loop */
} /* end routine */

void
_IEG2CRI_64C32_ONCRI(
unsigned TYPE64I *iptr,
TYPE32I *sp,
int *numadd,
int *strideadd
)
{ 
	unsigned TYPE64I datum ;
	TYPE64I i;
	for( i =0; i< * numadd ; i ++ ){
		datum	= * iptr ;
		IEG2CRI_64R32
		iptr	+= 1;
		datum	= * iptr ;
		iptr	+= 1;
		sp	+= 1;
		IEG2CRI_64R32
		sp	+= (* strideadd) -1 ;
	}
} /* end routine */


/*
 * Biased rounding when the input precision is larger.
 * newman is the mantissa, shifted right and truncated.
 * man is the mantissa, left justified.
 * lenman is the number of bits desired in the mantissa.
 * The rounded result is in newman.
 * Note that the result can be 1<<lenman, incrementing the exponent
 * when it is added in.
 */
#define ROUNDIT(newman,man,lenman) \
	newman += 1&(	((unsigned)man) >> (64-1-lenman)	); \
/* end macro */

/*
 * CRI-->IEG will produce a denormal if the resultant biased exp <=0 and
 * exp >= -DIEEEMN.  This is the only difference between it and
 * IEG-->CRI.
 *   in: 2^-126*1.fff, outexp=1,mant=fff  ---> <exp=1><mant=fff> 
 *   in: 2^-127*1.fff, outexp=0,mant=fff  ---> <exp=0><mant=1fff>
 *   in: 2^-128*1.fff, outexp=-1,mant=fff ---> <exp=0><mant=01fff>
 *   in: 2^-129*1.fff, outexp=-2,mant=fff ---> <exp=0><mant=001fff>
 *   in: 2^-149*1.fff, outexp=-22,mant=fff ---> <exp=0><mant=0..01>
 *   in: 2^-150*1.fff, outexp=-23,mant=fff ---> (rounds to 2^-149)
 */
#define CRI2IEG_64R32 \
{ \
	unsigned TYPE64I	man, manlo, sign, newman; \
	register TYPE64I	exp; \
	TYPE32I			noflow; \
\
	newman	= datum<<(64-DIEEEXP-DIEEEMN) ; /* shift off sign */ \
	man	= datum << (64-DIEEEMN) ;	/* shift mantissa to left */ \
	manlo	= 0 ; \
	sign	= datum >> (64-1) ; \
	exp	= newman >> (64-DIEEEXP) ; \
	noflow	= (exp < DIEEEMX) ; \
	exp	= exp +( SIEEEBS - DIEEEBS ) ;	/* - old bias + new bias */ \
\
	sign	= sign << (SIEEEMN+SIEEEXP) ; \
	/*  man, exp, and sign are now set for normal nonzero values */ \
\
	if (exp < SIEEEMX ) {		/*  else, inf or NaN */ \
		if (exp <= 0 ) {	/* if( 0.0 or possible denormal) */ \
			*sp = (TYPE32I) sign ;	/* store signed 0.0 */ \
			exp= - exp ; \
			if ( exp <= SIEEEMN ) {/* if denormal result */ \
				man = ( man>>1) | ( 1<<(64-1) ) ; \
				if (exp>0) man = man>>exp ; \
			} \
			else	 man	=0 ; 	/* underflow to signed 0.0 */ \
			exp	 = 0 ; 	/* exp=0 in any case */ \
		} \
	} \
	else{	/* Result is Infinity or NaN. Did conversion overflow? */ \
		if (  noflow ) {	/* noflow !=0: input finite */ \
			O@T3D	+= 1 ;	/* overflow count */ \
			man	= 0 ;	/* man of Inf is zero */ \
			} \
		else  	{	/* noflow==0: input is NaN or Inf */ \
			if ( man != 0) man = IEEENAN<<(64-SIEEEMN) ; \
		} \
		exp = SIEEEMX ; \
	} \
	newman = man >> (64- SIEEEMN); \
	ROUNDIT(newman,man,SIEEEMN) \
	/* Note: rounding overflow is silent (as in cry2cri) */ \
	*sp	= (TYPE32I)( sign + newman + ( exp<< SIEEEMN) ); \
} \
/* end macro */


/*
 *	_CRI2IEG_64R32	Translate T3D REAL (64-bit) to IEEE 32-bit
 *	Denormals, <sign><exp=0><mant=fff> is <sign>*fff*2^-126 are created.
 *	No overflow on rounding up to infinity.
 */

void
_CRI2IEG_64R32(unsigned long datum, short *sp) { CRI2IEG_64R32 }

/* cray2ieg( type 8 :: -0.18+309-->FFF0000000000000 )    */
/* cray2ieg( type 2 :: -0.34E+39->FF800000          )    */
/* cray2ieg( type 8 :: -0.36+309-->FFF0000000000000 )    */
/* cray2ieg( type 2 :: -0.68E+39->FF800000          )    */

/* cray2cri( type 2 :: -0.22-307-->8000000000000000 ) != won't create denorm */
/* cray2ieg( type 8 :: -0.22-307-->800FE00000000000 ) != denorm */

/* cray2cri( type 8 :: -0.12E-37->80800000          ) != won't create denorm */
/* cray2ieg( type 2 :: -0.12E-37->807F0000          ) != denorm */

/* cray2cri( type 2 :: -0.25-323-->8000000000000000 ) != won't create denorm */
/* cray2ieg( type 8 :: -0.25-323-->8000000000000001 ) != rounds up to denorm */

/* cray2cri( type 8 :: -0.70E-45->80000000          ) != */
/* cray2ieg( type 2 :: -0.70E-45->80000001          ) != */

/* cray2ieg( type 8 :: -0.25-323-->8000000000000000 ) too small */
#define EXPSMALLER 0
			/* 0 if new exponent contains all finite results.
			1 if denormals or overflow can result.
			32-->64:0,  64-->32:1
			*/
#undef ROUND
#define ROUND 0      /* 0: no rounding.  0 if 32-->64 */
                     /* 1: biased rounding.  1 if 64-->32 */

#define LLONG 64        /* Word length of long, bits */
#define IBIAS 0177      /* Exponent bias of IEEE IEG to be converted */
#define OBIAS 01777     /* Exponent bias of IEEE CRI result */
#define IINFX 0377      /* Exponent of Infinity:IEEE IEG to be converted */
#define OINFX 03777     /* Exponent of Infinity:IEEE CRI result */
#define ILEXP 8         /* Exponent length of IEEE to be converted */
#define OLEXP 11        /* Exponent length of IEEE CRI result */
#define ILMNT 23        /* Mantissa length of IEEE to be converted */
#define OLMNT 52        /* Mantissa length of IEEE CRI result */
#define ONANMANT 025252 /* NaN mantissa:IEEE CRI result, arbitrary */

#define PROCESS_FINITE_VALUE \
	manth = (manth >> ( LLONG - OLMNT )) \
		+( ibexp << OLMNT) ; /* () manth>> */ \
	if( ROUND != 0 ){ \
		manth = manth + ( manth >>( LLONG- OLMNT -1)&1); \
		ibexp = manth >>( OLMNT -1) ; }  /* ibexp */ \
	*optr = isnbit + manth ; /* finite, not denormal */ \
	/* if the new exponent is larger, no overflow occurs */ \
	/* and denormals cannot be generated */ \
	if( EXPSMALLER != 0 ){ \
		if (((long) ibexp) >0){ /* skip if denormal */ \
			if( ibexp >= OINFX ){ /* if Infinity */ \
				O@T3D += 1; /* overflow */ \
				*optr= isnbit+( OINFX<< OLMNT); \
				} \
			} \
		else{ *optr= isnbit; } \
		} \
/* end macro */

#define PROCESS_IEG_REAL \
{ /* local variable definitions */ \
unsigned TYPE64I isnbit , manth , ibexp , naninf ; \
extern unsigned TYPE64I O@T3D ;   \
ibexp = (* iptr )>> ILMNT ;             /*right justify and isolate exp */ \
ibexp = ibexp &( (1<< ILEXP )-1 ) ; \
isnbit = (* iptr )>>( ILMNT + ILEXP ) ; /* right justify sign */ \
isnbit = isnbit <<( OLEXP + OLMNT ) ;   /* position sign for result */ \
manth = (* iptr )<<( LLONG - ILMNT) ; \
* optr = isnbit ;                      /* store signed zero */ \
if( (* iptr )<<( LLONG - ILMNT - ILEXP) != 0) { /* done if exp=mantissa=0 */ \
	naninf= IINFX - ibexp ;        /* 0 if NaN or Infinity */ \
	if( ibexp !=  0){              /* skip if denormal */ \
		/* debug \
		printf( "\n ibexp:%x isnbit:%x manth:%x naninf:%x *optr:%x ", \
       	     	ibexp,     isnbit,     manth,     naninf,     *optr ); \
		*/ \
		ibexp += ( OBIAS - IBIAS ) ; \
		if( naninf != 0 ){   /* skip if NaN or Infinity */ \
			PROCESS_FINITE_VALUE \
			} \
		else{	/* NaN (mantissa !=0) or Infinity (mantissa 0) */ \
			isnbit += ( OINFX << OLMNT ) ; \
			if( manth==0 ){ *optr = isnbit ;} \
			else{ *optr = isnbit + ONANMANT ;} \
			} \
		} \
	else{ \
		int j; \
		j=_leadz(manth)+1 ;  /* denormal to convert */ \
		manth= manth<<j ; \
		ibexp= 1+(OBIAS-IBIAS)-j ; \
		PROCESS_FINITE_VALUE \
		} \
	} /* end if( nonzero ), zero already stored */ \
} /* end of local variable definitions */ \
/* end macro */


#define TWOPOWER30 1073741824.0 
#define TWOPOWERMINUS150	(1.0/(TWOPOWER30*TWOPOWER30*TWOPOWER30* \
				TWOPOWER30*TWOPOWER30));
#define DENORM32(x,y) \
{ \
	if( (x+x) == 0 ) y= x ;		/* zero, possibly signed */ \
	else { \
		y = ( (TYPE64R)(x+x) ) *TWOPOWERMINUS150; /* float(|x|) */ \
		if ( x < 0) y = -(y) ; \
	} \
} \
/* end macro */


/*************** real item **********************/
void _IEG2CRI_32R64_ONCRI(
	unsigned TYPE32I *iptr,  
	unsigned TYPE64I *optr,   
	int *numadd,
	int *strideadd
)
{
	TYPE64I i ;
	for( i=0; i< *numadd ; i ++, iptr += 1, optr += *strideadd ){
		PROCESS_IEG_REAL
	} /* end for loop */
} /* end routine */


/* This routine copies from iptr to optr, zeroing denormals as it goes. */
void _IEG2CRI_64R64_ONCRI(
	unsigned TYPE64I *iptr,  
	unsigned TYPE64I *optr,   
	int *numadd,
	int *strideadd
)
{
	TYPE64I i ;
	for( i=0; i< *numadd ; i ++, optr += *strideadd ){
		* optr	= iptr[i];
		if( ( iptr[i] & DIEEEXPMASK ) ==0 )
			* optr = iptr[i] & DIEEESNMASK ;
	}
} /* end routine */


/* This routine copies from iptr to optr, zeroing denormals as it goes. */
void _IEG2CRI_64C64_ONCRI(
	unsigned TYPE64I *iptr,  
	unsigned TYPE64I *optr,   
	int *numadd,
	int *strideadd
)
{
	TYPE64I i ;
	for( i=0; i< *numadd ; i ++, optr += *strideadd ){
		* optr	= iptr[2*i];
		if( ( iptr[2*i] & DIEEEXPMASK ) ==0 )
			* optr =	iptr[2*i] & DIEEESNMASK ;

		*( optr+1) = iptr[2*i+1];
		if( ( iptr[2*i+1] & DIEEEXPMASK ) ==0 )
			*( optr+1) = iptr[2*i+1] & DIEEESNMASK ;
	}
} /* end routine */


/* This routine copies from iptr to optr, assuming no denormals are present. */
void _CRI2IEG_64R64_ONCRI(
	unsigned TYPE64I *iptr,  
	unsigned TYPE64I *optr,   
	int *numadd,
	int *strideadd
)
{
	TYPE64I i ;
	for( i=0; i< *numadd ; i ++, optr += *strideadd ){
		* optr	= iptr[i];
	}
} /* end routine */


/* This routine copies from iptr to optr, assuming no denormals are present. */
void _CRI2IEG_64C64_ONCRI(
	unsigned TYPE64I *iptr,  
	unsigned TYPE64I *optr,   
	int *numadd,
	int *strideadd
)
{
	TYPE64I i ;
	for( i=0; i< *numadd ; i ++, iptr += *strideadd ){
		optr[2*i]	= * iptr;
		optr[2*i+1]	= * (iptr+1);
	}
} /* end routine */


/* This routine copies from iptr to optr, zeroing denormals as it goes. */
void _IEG2CRI_32R32_ONCRI(
	unsigned TYPE32I *iptr,  
	unsigned TYPE32I *optr,   
	int *numadd,
	int *strideadd
)
{
	TYPE64I i ;
	for( i=0; i< *numadd ; i ++, optr += *strideadd ){
		* optr	= iptr[i];
		if( ( iptr[i] & SIEEEXPMASK ) ==0 )
			* optr =	iptr[i] & SIEEESNMASK ;
	}
} /* end routine */


/* This routine copies from iptr to optr, zeroing denormals as it goes. */
void _IEG2CRI_32C32_ONCRI(
	unsigned TYPE32I *iptr,  
	unsigned TYPE32I *optr,   
	int *numadd,
	int *strideadd
)
{
	TYPE64I i ;
	for( i=0; i< *numadd ; i ++, optr += *strideadd ){
		* optr	= iptr[2*i];
		if( ( iptr[2*i] & SIEEEXPMASK ) ==0 )
			* optr =	iptr[2*i] & SIEEESNMASK ;

		*( optr+1) = iptr[2*i+1];
		if( ( iptr[2*i+1] & SIEEEXPMASK ) ==0 )
			*( optr+1) = iptr[2*i+1] & SIEEESNMASK ;
	}
} /* end routine */


/* This routine copies from iptr to optr, assuming no denormals are present. */
void _CRI2IEG_32R32_ONCRI(
	unsigned TYPE32I *iptr,  
	unsigned TYPE32I *optr,   
	int *numadd,
	int *strideadd
)
{
	TYPE64I i ;
	for( i=0; i< *numadd ; i ++, iptr += *strideadd ){
		optr[i]   = * iptr;
	}
} /* end routine */


/* This routine copies from iptr to optr, assuming no denormals are present. */
void _CRI2IEG_32C32_ONCRI(
	unsigned TYPE32I *iptr,  
	unsigned TYPE32I *optr,   
	int *numadd,
	int *strideadd
)
{
	TYPE64I i ;
	for( i=0; i< *numadd ; i ++, iptr += *strideadd ){
		optr[2*i]	= * iptr;
		optr[2*i+1]	= * (iptr+1);
	}
} /* end routine */


/*************** complex item (2 reals) *************/

void _IEG2CRI_32C64_ONCRI(
	unsigned TYPE32I * iptr,  
	unsigned TYPE64I * optr,   
	int * numadd,
	int * strideadd
)
{
	TYPE64I i ;
	for( i=0; i<*numadd; i ++ ){
		PROCESS_IEG_REAL
		iptr++ ;
		optr++ ;
		PROCESS_IEG_REAL
		iptr++ ;
		optr += (* strideadd)-1 ;
	} /* end for loop */
} /* end routine */

/*
 ************** logical item **********************
 */

void
_CRI2IEG_64L32_ONCRI(
unsigned TYPE64I *iptr,
unsigned TYPE32I *sp,
int *numadd,
int *strideadd
)
{ 
	TYPE64I i;
	for( i =0; i< * numadd ; i++, iptr += * strideadd ){
		sp[i] = ( (* iptr) != 0 ) ? 1 : 0 ;
	}
} /* end routine */


void
_IEG2CRI_32L64_ONCRI(
unsigned TYPE32I *iptr,
TYPE64I *sp,
int *numadd,
int *strideadd
)
{ 
	TYPE64I i;
	for( i =0; i< *numadd ; i ++, sp += *strideadd ){
		*sp = (iptr[i] != 0 ) ? 1 : 0 ;
	} /* end for loop */
} /* end routine */

/*
 *************** integer item **********************
 */
void _CRI2IEG_64I32_ONCRI(
	TYPE64I * iptr,   
	TYPE32I * optr,  
	int * numadd,
	int * strideadd
)
{
	TYPE64I i, datum, oflow ;
	oflow = 0 ;
	for( i =0; i< * numadd ; i ++ ){
		datum = * iptr ;
		iptr += * strideadd ;
		optr[i] = (unsigned TYPE32I) datum ;
		datum = datum>>(32-1) ;  /* sign extended right shift */
		oflow += ( datum > 0 ) + ( datum < (-1) ) ;
	} /* end for loop */
	O@T3D += oflow ;
} /* end routine */


void _IEG2CRI_32I64_ONCRI(
	TYPE32I *iptr,  
	TYPE64I *optr,   
	int *numadd,
	int * strideadd
)
{
	TYPE64I i ;
	for( i =0; i< *numadd ; i ++ ){
		* optr	=  * iptr ;
		iptr	+= 1 ;
		optr	+= * strideadd ;
	} /* end for loop */
} /* end routine */


#define INT16MASK ((1<<16)-1)
#define INT16MAX ((1<<15)-1)
#define INT16MIN (-(1<<15))

/*
        Convert NUM 64 bit integers from CRI(0::STRIDE) into IEG(0:).
        Values in IEG are 16 bits packed 4 per word, left justified.
        If the number of values to convert is not a multiple of 4,
        the rightmost bits in the last word of IEG are UNDEFINED.
        Increment overfow count O@T3D if value > 2^15-1 or < -2^15 .
	On overflow, the result is the truncated value.

	C call:
        _CRI2IEG_64I16_ONCRI(&CRI, &IEG, &NUM, &STRIDE)
	INTEGER*8 CRI(0:)	64-bit values to convert
	INTEGER*8 IEG(0:)	16-bit values, 4/word
	INTEGER*8 NUM   	positive number of values: NUM>0
	INTEGER*8 STRIDE	nonzero stride in CRI (may be <0)
	Call by address was chosen for numadd and strideadd
	so a name change makes it FORTRAN-callable.
*/

void
_CRI2IEG_64I16_ONCRI(
TYPE64I * ilptr,
TYPE64I * olptr,
int * numadd,
int * strideadd
)
{
	register TYPE64I	top16, bot16, oflow;
	int	i,j;
	oflow=	O@T3D ;
	top16=	0 ;
	for (i = 0; i <  * numadd ; i+=4) {
		for (j = 0; j < 4 ; j++) {
			if( (i+j) < * numadd ) bot16   = * ilptr ;
			ilptr   = ilptr + * strideadd ;
			oflow	+=  ( bot16 < INT16MIN) + ( bot16 > INT16MAX);
			top16	= ( top16 <<16 )|( bot16 & INT16MASK ) ;
		} /* end for */
		* olptr++	= top16 ;
	} /* end for */
	O@T3D = oflow ;
} /* end routine */

/*
        Convert NUM 16 bit integers from IEG(0:) into CRI(0::STRIDE).
        Values in IEG are 16 bits packed 4 per word, left justified.
        If the number of values to convert is not a multiple of 4,
        the rightmost bits in the last word of IEG are ignored.
        No overfow can occur.

	C call:
        _IEG2CRI_16I64_ONCRI(&IEG, &CRI, &NUM, &STRIDE)
	INTEGER*8 CRI(0:)	64-bit values
	INTEGER*8 IEG(0:)	16-bit values to convert, 4/word
	INTEGER*8 NUM   	positive number of values: NUM>0
	INTEGER*8 STRIDE	nonzero stride in CRI (may be <0)
	Call by address was chosen for numadd and strideadd
	so a name change makes it FORTRAN-callable.
*/

void
_IEG2CRI_16I64_ONCRI(
TYPE64I * ilptr,
TYPE64I * olptr,
int * numadd,
int * strideadd
)
{
	long i;
	register TYPE64I top16;
	for (i = 0; i < * numadd; i+=4) {
		top16   = * ilptr ;
		ilptr++;
		*olptr   = top16 >>48 ; /* sign extension */
		olptr   = olptr + (* strideadd) ;

		top16   = top16<<16 ;	/* left justify next value */
		if( (i+1) < * numadd ) *olptr   = top16 >>48 ; /* sign extend */
		olptr   = olptr + (* strideadd) ;

		top16   = top16<<16 ;	/* left justify next value */
		if( (i+2) < * numadd ) *olptr   = top16 >>48 ; /* sign extend */
		olptr   = olptr + (* strideadd) ;

		top16   = top16<<16 ;	/* left justify next value */
		if( (i+3) < * numadd ) *olptr   = top16 >>48 ; /* sign extend */
		olptr   = olptr + (* strideadd) ;
	}
} /* end routine */

/*
        Convert NUM 32 bit integers from CRI(0::STRIDE) into IEG(0:).
        Values in IEG are 16 bits packed 2 per word, left justified.
        If the number of values to convert is not a multiple of 2,
        the rightmost bits in the last word of IEG are UNDEFINED.
        Increment overfow count O@T3D if value > 2^15-1 or < -2^15 .
	On overflow, the result is the truncated value.
	This uses short, because it is only used with 32-bit containers.

	C call:
        _CRI2IEG_32I16_ONCRI(&CRI, &IEG, &NUM, &STRIDE)
	INTEGER*4 CRI(0:)	32-bit values to convert
	INTEGER*4 IEG(0:)	16-bit values, 2/word
	INTEGER*8 NUM   	positive number of values: NUM>0
	INTEGER*8 STRIDE	nonzero stride in CRI (may be <0)
	Call by address was chosen for numadd and strideadd
	so a name change makes it FORTRAN-callable.
*/

void
_CRI2IEG_32I16_ONCRI(
TYPE32I * isptr,
TYPE32I * osptr,
TYPE64I * numadd,
TYPE64I * strideadd
)
{
	long i, oflow;
	TYPE32I top16, bot16;
	oflow= O@T3D ;
	for (i = 0; i < ( (* numadd)>>1 ); i++) {
		top16   = * isptr ;
		isptr   = isptr + *strideadd ;
		bot16   = * isptr ;
		isptr   = isptr + *strideadd ;
		*osptr	= ( top16 <<16 )|( bot16 & INT16MASK ) ;
		osptr   += 1 ;
		oflow	+=  ( top16 < INT16MIN) + ( top16 > INT16MAX);
		oflow	+=  ( bot16 < INT16MIN) + ( bot16 > INT16MAX);
	} /* end for */
	if ( ((*numadd)&1) !=0) {
		* osptr	= (TYPE32I) * (isptr)<<16 ;
		oflow	+=  ( top16 < INT16MIN) + ( top16 > INT16MAX);
		/* if count is odd, the low 16 bits of
		   the last byte of the output are zeroed.
		*/
	} /* end if */
	O@T3D = oflow ;
} /* end routine */


/*
        Convert NUM 16 bit integers from IEG(0:) into CRI(0::STRIDE).
        Values in IEG are 16 bits packed 2 per short, left justified.
        If the number of values to convert is not a multiple of 2,
        the rightmost bits in the last word of IEG are ignored.
        No overfow can occur.

	C call:
        _IEG2CRI_16I32_ONCRI(&IEG, &CRI, &NUM, &STRIDE)
	INTEGER*4 CRI(0:)	32-bit values
	INTEGER*4 IEG(0:)	16-bit values to convert, 4/word
	INTEGER*8 NUM   	positive number of values: NUM>0
	INTEGER*8 STRIDE	nonzero stride in CRI (may be <0)
	Call by address was chosen for numadd and strideadd
	so a name change makes it FORTRAN-callable.
	This uses short, because it is only used with 32-bit containers.
*/

void
_IEG2CRI_16I32_ONCRI(
TYPE32I * isptr,
TYPE32I * osptr,
TYPE64I * numadd,
TYPE64I * strideadd
)
{
	long i;
	register TYPE32I top16;
	for (i = 0; i < * numadd; i+=2) {
		top16	= * isptr ;
		isptr++;
		*osptr   = top16 >>16 ;		/* sign extension */
		osptr   = osptr + *strideadd ;

		top16	= top16<<16 ;		/* left justify next value */
		if( (i+1) < * numadd) *osptr	= top16 >>16; /* sign extend */
		osptr   = osptr + *strideadd ;
	}
} /* end routine */


/*************** real item **********************/

/*
 * count is in items,
 * incr: stride for multiword items is now in words, else in items 
 */

void _CRI2IEG_64R32_ONCRI(
	unsigned TYPE64I *iptr,   
	unsigned TYPE32I *sp,  
	int *numadd,
	int *strideadd
)
{
	TYPE64I i ;
	unsigned TYPE64I datum ;
	extern unsigned TYPE64I O@T3D ;
	for( i=0; i< *numadd ; i ++, iptr += *strideadd, sp += 1 ){
		datum= * iptr ;
		CRI2IEG_64R32
	} /* end for loop */
} /* end routine */


/*************** complex item (2 reals) *************/

/*
 * count is in items,
 * incr: stride for multiword items is now in words, else in items 
 */

void _CRI2IEG_64C32_ONCRI(
	unsigned TYPE64I *iptr,   
	unsigned TYPE32I *sp,  
	int *numadd,
	int *strideadd
)
{
	TYPE64I i ;
	unsigned TYPE64I datum ;
	for( i=0; i<*numadd; i ++ ){
		datum= *iptr ;
		CRI2IEG_64R32
		sp++ ;
		datum= * (iptr +1) ;
		CRI2IEG_64R32
		iptr += * strideadd ;
		sp++ ;
	}
} /* end routine */
