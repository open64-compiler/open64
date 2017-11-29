/*
 * Copyright (c) 2000, Intel Corporation
 * All rights reserved.
 *
 * WARRANTY DISCLAIMER
 *
 * THESE MATERIALS ARE PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT 
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL INTEL OR ITS 
 * CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR 
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY
 * OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THESE
 * MATERIALS, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * Intel Corporation is the author of the Materials, and requests that all
 * problem reports or change requests be submitted to it directly at
 * http://developer.intel.com/opensource.
 */

/* Non MED section begin */

/* static char sccs_id[] = "@(#)sbv.h	1.1  08/02/96  13:01:46"; */

#ifndef _KAPI_BV_H_
#define _KAPI_BV_H_

#include <inttypes.h>
#include <stdio.h>

#ifdef __cplusplus
extern "C" {
#endif
/* Non MED section end */
/*---------------------------**
**                           **
** Bit Vector Library Macros **
**                           **
**                           **
**---------------------------*/


/* = = = = = = = = = = = = = = = = == = = = = = = = = = = = = */
/* ------------------- 32 bit stuff ------------------------- */
/* = = = = = = = = = = = = = = = = == = = = = = = = = = = = = */

typedef uint32_t bv32_t;         /* 32-bit long bit vector */

#define i2bv32( i ) ( 1 << (i) )

#define ZERO_bv32( bv )       \
    ((bv) = 0)

#define ONE_bv32(bv)	\
	((bv)=0xFFFFFFFF)

#define bv32ZEROS() ( 0 )

#define bv32ONES( len ) \
   ( ((len) == 0) ? 0 : ( ( (len) < 32 ) ? ( 1 << (len) ) - 1 : 0xFFFFFFFF ) )

#define bv32ONES_MASK( len, pos ) \
   ( bv32ONES(len) << ( pos ) )

#define bv32SHL( bv32Src, len ) \
   ( ((len)>=32) ? 0 : ( (bv32Src) << (len) ) )

#define bv32SHRL( bv32Src, len ) \
   ( ( (len)>=32) ? 0 : ( ( (uint32_t) (bv32Src) ) >> (len) ) )

#define bv32EXTRACTU( bv32Src, len, pos ) \
     ( bv32SHRL( (bv32Src), (pos) ) & bv32ONES(len) )


#define isbv32ZERO( bv ) \
   ( (bv) == 0 )

#define isbv32BITSET( bv, pos )              \
   ( ( (bv) & ( 1 << ((pos)) ) ) )


#define bv32AND( bv1, bv2 ) \
   ( (bv1) & (bv2) )

#define bv32OR( bv1, bv2 ) \
   ( (bv1) | (bv2) )

#define bv32XOR( bv1, bv2 ) \
   ( (bv1) ^ (bv2) )

#define bv32NOT( bvRes, bv ) \
   { (bvRes) = ~(bv); }


#define UNSETBIT_bv32( bv, pos ) {      \
       (bv) &= ~( 1 << (pos) );  } 

#define SETBIT_bv32( bv, pos ) {      \
       (bv) |= ( 1 << (pos) );  } 

#define COPY_bv32( bvTar, bvSrc ) {       \
   (bvTar) = (bvSrc);  }

extern char *bv322pch(  bv32_t  bv32,  char *pch );

/* = = = = = = = = = = = = = = = = == = = = = = = = = = = = = */
/* ------------------- 64 bit stuff ------------------------- */
/* = = = = = = = = = = = = = = = = == = = = = = = = = = = = = */

typedef struct _BV64_T {
   uint32_t i1, i2;   /* 64 bit long vector */
} bv64_t;


#define isbv64ZERO( bv ) \
   ( (bv.i1) == 0 && (bv.i2) == 0  )

#define isbv64BITSET( bv, pos )              \
   ( ( (pos) < 32 ) ? (                      \
       ( bv.i1 & ( 1 << ((pos)) ) ) ) : (    \
     ( ( bv.i2 & ( 1 << ((pos)-32) ) ) ) ) ) 


#define ZERO_bv64( bv )       \
   { (bv.i1) = 0; (bv.i2) = 0;  }

#define SETBIT_bv64( bv, pos ) {      \
   { if ( (pos) < 32 ) {               \
       bv.i1 |= ( 1 << (pos) );      \
   } else if ( (pos) < 64 ) { \
       bv.i2 |= ( 1 << ((pos)-32) ); \
   }  } }

#define COPY_bv64( bvTar, bvSrc ) {       \
   bvTar.i1 = bvSrc.i1;                 \
   bvTar.i2 = bvSrc.i2; }

#define ONES_bv64( bv64Tar, len ) \
   { \
      if ( (len) == 0 ) { \
         bv64Tar.i1 = 0; \
         bv64Tar.i2 = 0; \
      } if ( (len) <= 32 ) { \
         bv64Tar.i1 = bv32ONES( len ); \
         bv64Tar.i2 = 0; \
      } else { \
         bv64Tar.i1 = 0xFFFFFFFF; \
         bv64Tar.i2 = bv32ONES( (len)-32 ); \
      }  \
   }


#define OR_SELF_bv64( bvRes, bv1 ) \
   { (bvRes.i1) |= ( bv1.i1 );      \
   (bvRes.i2) |= ( bv1.i2 ); }

#define AND_SELF_bv64( bvRes, bv1 ) \
   { (bvRes.i1) &=  ( bv1.i1 );           \
   (bvRes.i2) &=  ( bv1.i2 ); }

#define OR_2_bv64( bvRes, bv1, bv2 )    \
   { (bvRes.i1) = (bv1.i1) | ( bv2.i1 ); \
   (bvRes.i2) = (bv1.i2) | ( bv2.i2 ); }

#define AND_2_bv64( bvRes, bv1, bv2 )   \
   { (bvRes.i1) = (bv1.i1) & ( bv2.i1 ); \
   (bvRes.i2) = (bv1.i2) & ( bv2.i2 ); }

#define OR_3SELF_bv64( bvRes, bv1, bv2, bv3 ) \
   { (bvRes.i1) |= (bv1.i1) | ( bv2.i1 ) | (bv3.i1); \
   (bvRes.i2) |= (bv1.i2) | ( bv2.i2 ) | (bv3.i2); }

#define AND_3SELF_bv64( bvRes, bv1, bv2, bv3 )        \
   { (bvRes.i1) &= (bv1.i1) & ( bv2.i1 ) & ( bv3.i1 ); \
   (bvRes.i2) &= (bv1.i2) & ( bv2.i2 ) & ( bv3.i2 ); }

#define NOT_bv64( bvRes, bv ) \
   { (bvRes.i1) = ~(bv.i1);    \
   (bvRes.i2) = ~(bv.i2); }

#define SHL_bv64( bv64Tar, bv64Src, len ) \
   { \
 \
      bv64Tar.i1 = bv32SHL( bv64Src.i1, (len) ); \
      bv64Tar.i2 = bv32SHL( bv64Src.i2, (len) ); \
 \
      /* compute high order len bits of i1 */ \
      if ( (len) > 32 ) { \
         bv64Tar.i2 |= bv32SHL( bv64Src.i1, (len)-32 ); \
      } else { \
         bv64Tar.i2 |= bv32EXTRACTU( bv64Src.i1, (len), 32-(len) ); \
      } \
   }


#define SHRL_bv64( bv64Tar, bv64Src, len ) \
   { \
      bv64Tar.i1 = bv32SHRL( bv64Src.i1, (len) ); \
      bv64Tar.i2 = bv32SHRL( bv64Src.i2, (len) ); \
 \
      /* extract low order bits of i2 */ \
      if ( (len) > 32 ) { \
         bv64Tar.i1 |= bv32SHRL( bv64Src.i2, (len)-32 ); \
      } else { \
         bv64Tar.i1 |= bv32SHL( bv32EXTRACTU( bv64Src.i2, (len), 0 ), 32-(len) ); \
      } \
   }

#define EXTRACTU_bv64( bv64Tar, bv64Src, len, pos ) \
   { \
     SHRL_bv64( bv64Tar, bv64Src, (pos) ) \
     bv64Tar.i1 &= bv32ONES( len ); \
     bv64Tar.i2 &= ( (len)>32 ) ? bv32ONES(64 - (len)) : 0; \
   }

extern char *bv642pch(  bv64_t  bv64,  char *pch );

/* = = = = = = = = = = = = = = = = == = = = = = = = = = = = = */
/* ------------------- 128 bit stuff ------------------------ */
/* = = = = = = = = = = = = = = = = == = = = = = = = = = = = = */

typedef struct _BV128_T {
   uint32_t i1, i2, i3, i4;   /* 128 bit long vector */
} bv128_t;


#define OR_SELF_bv128( bvRes, bv1 ) \
   { (bvRes.i1) |= ( bv1.i1 ); \
   (bvRes.i2) |= ( bv1.i2 ); \
   (bvRes.i3) |= ( bv1.i3 ); \
   (bvRes.i4) |= ( bv1.i4 ); }

#define AND_SELF_bv128( bvRes, bv1 ) \
  {  (bvRes.i1) &=  ( bv1.i1 ); \
   (bvRes.i2) &=  ( bv1.i2 ); \
   (bvRes.i3) &=  ( bv1.i3 ); \
   (bvRes.i4) &=  ( bv1.i4 ); }

#define OR_2_bv128( bvRes, bv1, bv2 )   \
   { (bvRes.i1) = (bv1.i1) | ( bv2.i1 ); \
   (bvRes.i2) = (bv1.i2) | ( bv2.i2 ); \
   (bvRes.i3) = (bv1.i3) | ( bv2.i3 ); \
   (bvRes.i4) = (bv1.i4) | ( bv2.i4 ); }

#define AND_2_bv128( bvRes, bv1, bv2 )  \
   { (bvRes.i1) = (bv1.i1) & ( bv2.i1 ); \
   (bvRes.i2) = (bv1.i2) & ( bv2.i2 ); \
   (bvRes.i3) = (bv1.i3) & ( bv2.i3 ); \
   (bvRes.i4) = (bv1.i4) & ( bv2.i4 ); }

#define OR_3SELF_bv128( bvRes, bv1, bv2, bv3 )      \
   { (bvRes.i1) |= (bv1.i1) | ( bv2.i1 ) | (bv3.i1); \
   (bvRes.i2) |= (bv1.i2) | ( bv2.i2 ) | (bv3.i2); \
   (bvRes.i3) |= (bv1.i3) | ( bv2.i3 ) | (bv3.i3); \
   (bvRes.i4) |= (bv1.i4) | ( bv2.i4 ) | (bv3.i4); }

#define AND_3SELF_bv128( bvRes, bv1, bv2, bv3 )       \
   { (bvRes.i1) &= (bv1.i1) & ( bv2.i1 ) & ( bv3.i1 ); \
   (bvRes.i2) &= (bv1.i2) & ( bv2.i2 ) & ( bv3.i2 ); \
   (bvRes.i3) &= (bv1.i3) & ( bv2.i3 ) & ( bv3.i3 ); \
   (bvRes.i4) &= (bv1.i4) & ( bv2.i4 ) & ( bv3.i4 ); }

#define NOT_bv128( bvRes, bv ) \
   { (bvRes.i1) = ~(bv.i1);     \
   (bvRes.i2) = ~(bv.i2);     \
   (bvRes.i3) = ~(bv.i3);     \
   (bvRes.i4) = ~(bv.i4); }
   
#define ZERO_bv128( bv )       \
   { (bv.i1) = 0; (bv.i2) = 0;  \
   (bv.i3) = 0; (bv.i4) = 0;  }

#define ONE_bv128(bv)	\
   { (bv.i1) = 0xFFFFFFFF; (bv.i2) = 0xFFFFFFFF;  \
   (bv.i3) = 0xFFFFFFFF; (bv.i4) = 0xFFFFFFFF;  }


#define isbv128ZERO( bv )             \
   (    (bv.i1) == 0 && (bv.i2) == 0  \
     && (bv.i3) == 0 && (bv.i4) == 0 )

#define isbv128BITSET( bv, pos )               \
   ( ( (pos) < 32 ) ? (                        \
       ( bv.i1 & ( 1 << ((pos)) ) ) ) : (      \
     ( (pos) < 64 ) ? (                        \
        ( bv.i2 & ( 1 << ((pos)-32) ) ) ) : (  \
     ( (pos) < 96 ) ? (                        \
        ( bv.i3 & ( 1 << ((pos)-64) ) ) ) :    \
     ( bv.i4 & ( 1 << ((pos)-96) ) ) ) ) )    

#define SETBIT_bv128( bv, pos )       \
   { if ( (pos) < 32 ) {               \
       bv.i1 |= ( 1 << (pos) );      \
   } else if ( (pos) < 64 ) {        \
       bv.i2 |= ( 1 << ((pos)-32) ); \
   } else if ( (pos) < 96 ) {        \
       bv.i3 |= ( 1 << ((pos)-64) ); \
   } else {                          \
       bv.i4 |= ( 1 << ((pos)-96) ); \
   } }

#define COPY_bv128( bvTar, bvSrc )       \
   { bvTar.i1 = bvSrc.i1;                 \
   bvTar.i2 = bvSrc.i2;                 \
   bvTar.i3 = bvSrc.i3;                 \
   bvTar.i4 = bvSrc.i4; }

extern char *bv1282pch( bv128_t bv128, char *pch );


/* = = = = = = = = = = = = = = = = == = = = = = = = = = = = = */
/* ------------------- Arbitrary  bit stuff ----------------- */
/* = = = = = = = = = = = = = = = = == = = = = = = = = = = = = */

typedef struct _BV_T {
   int n32Chunks;
   uint32_t *pint32Data;
} bv_t;

#define BYTE_LENGTHbv( pbv ) \
     ( 4 * (pbv)->n32Chunks )

#define BIT_LENGTHbv( pbv ) \
     ( 32 * (pbv)->n32Chunks )

#define isbvBITSET( pbv, pos )               \
   ( (pbv)->n32Chunks == 0 ? 0 : ((pbv)->pint32Data[ (pos) >> 5 ] &   ( 1 << ((pos) & 0x1F )) ) )

#define SETBIT_bv( pbv, pos )       \
   { (pbv)->pint32Data[ (pos) >> 5 ] |=  ( 1 << ((pos) & 0x1F) ); }

#define UNSETBIT_bv( pbv, pos )       \
   { (pbv)->pint32Data[ (pos) >> 5 ] &= ~( 1 << ((pos) & 0x1F) ); }

#define ONE_bv( pbv )       \
   {  \
     memset( (pbv)->pint32Data, 0xFFFFFFFF,  BYTE_LENGTHbv(pbv) );  \
   }

#define ZERO_bv( pbv )       \
   {  \
     memset( (pbv)->pint32Data, 0,  BYTE_LENGTHbv(pbv) );  \
   }

extern bv_t *pbvMake  ( int iLength,    int fOn );
extern bv_t *pbvDup( bv_t *pbv );
extern void bvInit ( bv_t *pbv, int iLength, int fOn );
extern void bvResize( bv_t *pbv, int iNewLength, int fOn );

extern int  fEqual_bv( bv_t *pbv1, bv_t *pbv2 );

extern bv_t *pbvOr ( bv_t *pbvTarget, bv_t *pbvSrc1, bv_t *pbvSrc2 );
extern bv_t *pbvAnd( bv_t *pbvTarget, bv_t *pbvSrc1, bv_t *pbvSrc2 );
extern bv_t *pbvXor( bv_t *pbvTarget, bv_t *pbvSrc1, bv_t *pbvSrc2 );

/* output routines */

/* Non MED section begin */
extern void Printbv( FILE *fp, bv_t *pbv );
/* Non MED section end */

extern void Free_pbv( bv_t *pbv );



#define u32ALLOCATED( length ) ( ( ((length-1) / 32) + 1 ))

/* Non MED section begin */
#ifdef __cplusplus
}
#endif

#endif
/* Non MED section end */
