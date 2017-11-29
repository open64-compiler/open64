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


/* static char sccs_id[] = "%W%  %G%  %U%"; */

#include <assert.h>
#include <stdio.h>
#include <string.h>

#include <malloc.h>
#include "kapi_parse.h"
#include "kapi_internal.h"
#include "kapi_util.h"

#define zSYM_HASH  512

static stn_t *pstnMake( char *pch, ity_t ity );
static int iHash_stn( char *pch );

stn_t *
kapi_pstnLookup_noadd( knobs_t *pknobs, char *pch )
{
   int idx, fcmp;
   stn_t *pstn, *pstnPrev;


   idx = iHash_stn( pch );

   pstnPrev = &(pknobs->dmpstn[ idx ] );
   pstn = pknobs->dmpstn[ idx ].pstnHashNext;

   /* find place in list with quick search */
   while ( pstn != NULL && pstn->pchName[0] < pch[0] ) { 
      pstnPrev = pstn;
      pstn = pstn->pstnHashNext;
   }

   /* find place in list with quick search */
   while ( pstn != NULL && ( (fcmp = strcmp( pstn->pchName, pch )) < 0 ) ) { 
      pstnPrev = pstn;
      pstn = pstn->pstnHashNext;
   }

   /* if not found */
   if ( pstn == NULL || fcmp > 0 ) {
      return( NULL );
   }

   /* if equal */
   if ( fcmp == 0 ) {
      return( pstn );
   }

   assert( 0 );
   return 0;
}

stn_t *
kapi_pstnLookup( knobs_t *pknobs, char *pch )
{
   int idx, fcmp;
   stn_t *pstn, *pstnPrev;

   idx = iHash_stn( pch );

   pstnPrev = &(pknobs->dmpstn[ idx ] );
   pstn = pknobs->dmpstn[ idx ].pstnHashNext;

   /* find place in list with quick search */
   while ( pstn != NULL && pstn->pchName[0] < pch[0] ) { 
      pstnPrev = pstn;
      pstn = pstn->pstnHashNext;
   }

   /* find place in list with quick search */
   while ( pstn != NULL && ( (fcmp = strcmp( pstn->pchName, pch )) < 0 ) ) { 
      pstnPrev = pstn;
      pstn = pstn->pstnHashNext;
   }

   /* if not found */
   if ( pstn == NULL || fcmp > 0 ) {
      stn_t *pstnNew;
      pstnNew = pstnMake( pch, ityUNKNOWN );

      pstnNew->pstnHashNext = pstn;
      pstnPrev->pstnHashNext = pstnNew;
      return( pstnNew );
   }

   /* if equal */
   if ( fcmp == 0 ) {
      return( pstn );
   }

   assert( 0 );
   return 0;
}

stn_t *
kapi_pstnDelete( knobs_t *pknobs, char *pchDelete )
{
   int idx;
   stn_t *pstn, *pstnPrev;

   idx = iHash_stn( pchDelete );

   pstnPrev = &(pknobs->dmpstn[ idx ] );
   pstn = pknobs->dmpstn[ idx ].pstnHashNext;

   /* find place in list with quick search */
   while ( pstn != NULL && strcmp( pstn->pchName, pchDelete ) ) { 
      pstnPrev = pstn;
      pstn = pstn->pstnHashNext;
   }

   /* delete it */
   if ( pstn ) {
      pstnPrev->pstnHashNext = pstn->pstnHashNext;
      pstn->pstnHashNext = NULL;
   }

   return( pstn );
}


#define EXPECT_IT( pch, _ity ) \
{  stn_t *pstnTMP__; \
   pstnTMP__ = kapi_pstnLookup( pknobs, (pch) ); \
   pstnTMP__->ity = (_ity); \
   pstnTMP__->fExpected = 1; }

#define RESERVE_IT( pch ) \
{  stn_t *pstnTMP__; \
   pstnTMP__ = kapi_pstnLookup( pknobs, (pch) ); \
   pstnTMP__->ity = ityTYPENAME; }


#define RESERVE_IT_TYPE( pch_, tty_ ) \
{  stn_t *pstnTMP__; \
   pstnTMP__ = kapi_pstnLookup( pknobs, (pch_) ); \
   pstnTMP__->ity = ityTYPENAME; \
   pstnTMP__->u.tfi.pstnIdent = (pstnTMP__); \
   pstnTMP__->u.tfi.tredefStatus = (tredefSTATUS_MARKED_NOREDEFINE); \
   pstnTMP__->u.tfi.tty = (tty_); }


void
kapi_InitSymbolTable( knobs_t *pknobs )
{
   int i;

   pknobs->nstn = zSYM_HASH;
   pknobs->dmpstn = (stn_t *) malloc( sizeof( stn_t ) * pknobs->nstn );

   for ( i=0; i<pknobs->nstn; i++ ) {
      pknobs->dmpstn[i].pstnHashNext = NULL;
   }



   RESERVE_IT( "ARRAY" );
   RESERVE_IT( "array" );
   RESERVE_IT( "BITMASK" );
   RESERVE_IT( "bitmask" );
   RESERVE_IT( "EXPECT" );
   RESERVE_IT( "expect" );
   RESERVE_IT( "ENUM" );
   RESERVE_IT( "enum" );
   RESERVE_IT( "VARIABLE" );
   RESERVE_IT( "variable" );
   RESERVE_IT( "TYPE" );
   RESERVE_IT( "type" );

   RESERVE_IT( "LIMIT" );
   RESERVE_IT( "limit" );
   RESERVE_IT( "USE" );
   RESERVE_IT( "use" );
   RESERVE_IT( "SKIP" );
   RESERVE_IT( "skip" );
   RESERVE_IT( "DEFAULT" );
   RESERVE_IT( "default" );
   RESERVE_IT( "OF" );
   RESERVE_IT( "of" );

   RESERVE_IT_TYPE( "INT", ttyINT );
   RESERVE_IT_TYPE( "int", ttyINT );
   RESERVE_IT_TYPE( "REAL", ttyREAL );
   RESERVE_IT_TYPE( "real", ttyREAL );
   RESERVE_IT_TYPE( "STRING", ttySTRING );
   RESERVE_IT_TYPE( "string", ttySTRING );
}


/* ----------------- local routines ----------------- */

static int
iHash_stn( char *pch )
{
   int i, l, sum, factor;

   sum = 0;
   factor = 1;
   l = strlen( pch );
   for ( i=0; i<l; i++ ) {
      if ( i & 1 ) {
         factor++;
      }
      sum += pch[ i ] * factor;
   }
   sum += pch[ 0 ] * l;

   return( sum % zSYM_HASH );
}

static stn_t *
pstnMake( char *pch, ity_t ity )
{
   stn_t *pstnTemp;

   pstnTemp = (stn_t *) malloc( sizeof( stn_t ) );

   pstnTemp->ity = ity;
   pstnTemp->fExpected = 0;
   pstnTemp->plimpListParse = NULL;
   pstnTemp->pchName = kapi_pchCopy( pch );
   pstnTemp->pstnNext = NULL;
   pstnTemp->pstnHashNext = NULL;
   pstnTemp->pstnTempNext = NULL;
   pstnTemp->fRHS = 0;

   return( pstnTemp );
}
