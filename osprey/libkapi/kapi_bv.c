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


/* static char sccs_id[] = "@(#)sbv.c	1.1  08/02/96  13:01:36"; */

#include <assert.h>
#include <stdlib.h>
#include <memory.h>
#include "kapi_bv.h"
#pragma warning( disable : 4101 )


void
bvResize( bv_t *pbv, int iNewBitLength, int fOn )
{
   int bitstoset, newlen32, i, oldlen32;
   uint32_t val;
   bv32_t bv32;


   /* resizing to shorter never reallocs, but may change some bit values  */
   oldlen32 = pbv->n32Chunks;
   newlen32 = u32ALLOCATED( iNewBitLength );

   if ( fOn ) {
      val = 0xFFFFFFFF;
   } else {
      val = 0;
   }
   
   if ( newlen32 > oldlen32 ) {
      pbv->pint32Data = (uint32_t *)realloc( 
                         pbv->pint32Data, sizeof( uint32_t ) * newlen32 );
      pbv->n32Chunks = newlen32; 

      /* initialize new chunks */
      for ( i=oldlen32; i<newlen32; i++ ) {
         pbv->pint32Data[ i ] = val;
      }
   } 

   /* set all bits from iNewBitLength to pbv->n32Chunks*8 - 1 to fOn */

   /* initialize new whole chunks, if any */
   for ( i=newlen32; i<oldlen32; i++ ) {
      pbv->pint32Data[ i ] = val;
   }

   /* if it was an even boundary, can return */
   bitstoset = 32 - (iNewBitLength & 0x1F);
   if ( bitstoset == 32 ) {
      return; /* no change needed -- even boundary */
   }

   /* else set high order affected bits */
   bv32 = ~(( 1 << (32-bitstoset)) - 1);

   if ( fOn ) { /* turn on high order bits -- leave low order alone */
      pbv->pint32Data[ newlen32-1 ] = pbv->pint32Data[ newlen32-1 ] | bv32;
   } else {     /* turn off high order bits -- leave low order alone */
      pbv->pint32Data[ newlen32-1 ] = pbv->pint32Data[ newlen32-1 ] & ~bv32;
   }
}

int  
fEqual_bv( bv_t *pbv1, bv_t *pbv2 )
{
   int i, nChunks;
   bv_t *pbvLonger;

   /* this is less general that it could be */
   if ( pbv1->n32Chunks < pbv2->n32Chunks ) {
      nChunks = pbv1->n32Chunks;
      pbvLonger = pbv1;
   } else {
      nChunks = pbv2->n32Chunks;
      pbvLonger = pbv2;
   }

   /* now check to make sure first nChunks are equal */
   for ( i=0; i<nChunks; i++ ) {
      if ( pbv1->pint32Data[ i ] != pbv2->pint32Data[ i ] ) {
         return( 0 );
      }
   }

   return 1;
}

bv_t *
pbvDup( bv_t *pbv )
{
   int len, i;
   bv_t *pbvNew;


   pbvNew = (bv_t *)malloc( sizeof(bv_t) );
   pbvNew->n32Chunks = pbv->n32Chunks; 
   pbvNew->pint32Data = (uint32_t *)malloc( sizeof( uint32_t ) 
                        * pbv->n32Chunks );
   assert( pbvNew );

   memcpy( pbvNew->pint32Data, pbv->pint32Data, BYTE_LENGTHbv(pbv) );
   pbvNew->n32Chunks = pbv->n32Chunks;

   return( pbvNew );
}

void
bvInit( bv_t *pbv, int iLength, int fOn )
{
   int len, i;
   uint32_t ival;


   assert( iLength >= 0 );

   len = u32ALLOCATED( iLength );

   pbv->pint32Data = (uint32_t *)malloc( sizeof( uint32_t ) * len );
   assert( pbv->pint32Data );
   pbv->n32Chunks = len; 

   if ( fOn ) {
      ival = 0xFFFFFFFF; 
   } else {
      ival = 0x0;
   }

   for ( i=0; i<len; i++ ) {
      pbv->pint32Data[ i ] = ival;
   }
}

void
Free_pbv( bv_t *pbv )
{
   if ( NULL == pbv ) {
      return;
   }

   if ( pbv->pint32Data ) {
      free( pbv->pint32Data );
      pbv->pint32Data = NULL;
   }

   free( pbv );
}

bv_t *
pbvMake( int iLength, int fOn )
{
   int len, i;
   uint32_t ival;
   bv_t *pbv;


   assert( iLength > 0 );

   len = u32ALLOCATED( iLength );

   pbv = (bv_t *) malloc( sizeof( bv_t ) );
   assert( pbv );

   pbv->pint32Data = (uint32_t *)malloc( sizeof( uint32_t ) * len );
   assert( pbv->pint32Data );
   pbv->n32Chunks = len; 

   if ( fOn ) {
      ival = 0xFFFFFFFF; 
   } else {
      ival = 0x0;
   }

   for ( i=0; i<len; i++ ) {
      pbv->pint32Data[ i ] = ival;
   }

   return( pbv );
}

bv_t *
pbvOr( bv_t *pbvTarget, bv_t *pbvSrc1, bv_t *pbvSrc2 )
{
   int i, len;

   if ( pbvTarget->n32Chunks <= pbvSrc1->n32Chunks ) {
      len = pbvTarget->n32Chunks;
   } else {
      len = pbvSrc1->n32Chunks;
   }

   if ( pbvSrc1->n32Chunks < len ) {
      len = pbvSrc1->n32Chunks;
   } 

   for ( i=0; i<len; i++ ) {
      pbvTarget->pint32Data[ i ] = pbvSrc1->pint32Data[ i ] | pbvSrc2->pint32Data[ i ];
   }

   return( pbvTarget );
}

bv_t *
pbvAnd( bv_t *pbvTarget, bv_t *pbvSrc1, bv_t *pbvSrc2 )
{
   int i, len;

   if ( pbvTarget->n32Chunks <= pbvSrc1->n32Chunks ) {
      len = pbvTarget->n32Chunks;
   } else {
      len = pbvSrc1->n32Chunks;
   }

   if ( pbvSrc1->n32Chunks < len ) {
      len = pbvSrc1->n32Chunks;
   } 

   for ( i=0; i<len; i++ ) {
      pbvTarget->pint32Data[ i ] = pbvSrc1->pint32Data[ i ] & pbvSrc2->pint32Data[ i ];
   }
   return( pbvTarget );
}

bv_t *
pbvXor( bv_t *pbvTarget, bv_t *pbvSrc1, bv_t *pbvSrc2 )
{
   int i, len;

   if ( pbvTarget->n32Chunks <= pbvSrc1->n32Chunks ) {
      len = pbvTarget->n32Chunks;
   } else {
      len = pbvSrc1->n32Chunks;
   }

   if ( pbvSrc1->n32Chunks < len ) {
      len = pbvSrc1->n32Chunks;
   } 


   for ( i=0; i<len; i++ ) {
      pbvTarget->pint32Data[ i ] = pbvSrc1->pint32Data[ i ] ^ pbvSrc2->pint32Data[ i ];
   }
   return( pbvTarget );
}

void
Printbv( FILE *fp, bv_t *pbv )
{
   int b;
   int i;
   char *pchTemp;

   pchTemp = (char *)malloc( sizeof( char ) * (1 + pbv->n32Chunks * 32) );

   for ( i=0;i<pbv->n32Chunks * 32; i++ ) {
      b = ( 0 != isbvBITSET( pbv, i ) );
      pchTemp[ i ] = '0' + b;
   }
   pchTemp[ pbv->n32Chunks * 32 ] = '\0';

   fprintf( fp, "%s", pchTemp );
   free( pchTemp );
}

char *
bv322pch( bv32_t bv32, char *pch )
{
   int i, b, pos;
   uint32_t i32;

   i32 = bv32;
   pos = 0;
   for ( i=0; i<32; i++ ) {
      b = i32 & 1;
      i32 >>= 1;
      pch[ pos ] = '0'+b;
      pos++;
   }
   pch[ pos ] = '\0';

   return( pch );
}

char *
bv642pch( bv64_t bv64, char *pch )
{
   int i, pos, b;
   uint32_t i32;

   pos = 0;
   i32 = bv64.i1;
   for ( i=0; i<32; i++ ) {
      b = i32 & 1;
      i32 >>= 1;
      pch[ pos ] = '0'+b;
      pos++;
   }

   i32 = bv64.i2;
   for ( i=0; i<32; i++ ) {
      b = i32 & 1;
      i32 >>= 1;
      pch[ pos ] = '0'+b;
      pos++;
   }
   pch[ pos ] = '\0';

   return( pch );
}

char *
bv1282pch( bv128_t bv128, char *pch )
{
	return 0;
}

