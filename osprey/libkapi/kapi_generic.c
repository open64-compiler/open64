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
#include <string.h>
#include "kapi_internal.h"
#include "kapi_parse.h"
#include "kapi_util.h"
#include "kapi_error.h"

int     
KAPI_VariableCardinality( void *pConfig, char *pchAttribute )
{
   stn_t *pstn;
   knobs_t *pknobs = pConfig;

   KAPI_error_attribute = 0;
   pstn = kapi_pstnLookup_noadd( pknobs, pchAttribute );
   if ( pstn == NULL || pstn->ity != ityVARNAME ) {
      KAPI_error_attribute = -1;
      return( -1 );
   }

   if ( pstn->u.vfi.ptfi->tty == ttyARRAY ) {
      if ( pstn->u.vfi.ptfi->ptfiArrayIndexType->tty == ttyENUM ) {
         return( pstn->u.vfi.ptfi->ptfiArrayIndexType->nEnumConst );
      } else {
         return( 1 );
      }
   } else {
      return( 1 );
   }
}

char *     
KAPI_GetEnumVariableName( void *pConfig, char *pchVariable, int idx )
{
   stn_t *pstn;
   valhdr_t valhdr;

   knobs_t *pknobs = pConfig;

   KAPI_error_attribute = 0;
   pstn = kapi_pstnLookup_noadd( pknobs, pchVariable );
   if ( pstn == NULL || pstn->ity != ityVARNAME ) {
      KAPI_error_attribute = -1;
      return( NULL );
   }

   /* check to see if this is a variable of type enum */
   if ( ! ( pstn->u.vfi.ptfi->tty == ttyENUM 
           ||
              ( pstn->u.vfi.ptfi->tty == ttyARRAY 
                &&
                pstn->u.vfi.ptfi->ptfiArrayEltType->tty == ttyENUM 
              )
         ) ) {
      KAPI_error_attribute = -1;
      return( NULL );
    }

   kapi_LookUpVariable_valhdr( pstn, &valhdr, idx );
   if ( valhdr.vals == valsUNSET ) {
      KAPI_error_attribute = -1;
      return( NULL );
   }

   return( kapi_pchCopy( valhdr.pvalnList->val.pch ) );
}

int     
KAPI_GetEnumVariable( void *pConfig, char *pchVariable, int idx )
{
   stn_t *pstn;
   valhdr_t valhdr;
   knobs_t *pknobs = pConfig;

   KAPI_error_attribute = 0;
   pstn = kapi_pstnLookup_noadd( pknobs, pchVariable );
   if ( pstn == NULL || pstn->ity != ityVARNAME ) {
      KAPI_error_attribute = -1;
      return( -1 );
   }

   /* check to see if this is a variable of type enum */
   if ( ! ( pstn->u.vfi.ptfi->tty == ttyENUM 
           ||
              ( pstn->u.vfi.ptfi->tty == ttyARRAY 
                &&
                pstn->u.vfi.ptfi->ptfiArrayEltType->tty == ttyENUM 
              )
         ) ) {
      KAPI_error_attribute = -1;
      return( -1 );
    }

   kapi_LookUpVariable_valhdr( pstn, &valhdr, idx );
   if ( valhdr.vals == valsUNSET ) {
      KAPI_error_attribute = -1;
      return( -1 );
   }

   return( pstnEnum2idx( kapi_pstnLookup( pknobs, valhdr.pvalnList->val.pch ) ) );
}

int     
KAPI_ArrayIndex( void *pConfig, char *pchVariable, char *pchIndex )
{
   stn_t *pstn;
   int iPos;
   knobs_t *pknobs = pConfig;

   KAPI_error_attribute = 0;
   pstn = kapi_pstnLookup_noadd( pknobs, pchVariable );
   if ( pstn == NULL || pstn->ity != ityVARNAME ) {
      KAPI_error_attribute = -1;
      return( -1 );
   }

   if ( pstn->u.vfi.ptfi->tty != ttyARRAY ) {
      KAPI_error_attribute = -1;
      return( -1 );
   }

   iPos = idxped4pstn( pstn, pchIndex );

   if ( iPos == -1 ) {
      KAPI_error_attribute = -1;
      return( -1 );
   }

   return( iPos );
}


int     
KAPI_GetIntegerVariable( void *pConfig, char *pchVariable, int idx )
{
   stn_t *pstn;
   valhdr_t valhdr;
   knobs_t *pknobs = pConfig;

   KAPI_error_attribute = 0;
   pstn = kapi_pstnLookup_noadd( pknobs, pchVariable );
   if ( pstn == NULL || pstn->ity != ityVARNAME ) {
      KAPI_error_attribute = -1;
      return( -1 );
   }

   if ( !( pstn->u.vfi.ptfi->tty == ttyINT 
           ||
              ( pstn->u.vfi.ptfi->tty == ttyARRAY 
                &&
                pstn->u.vfi.ptfi->ptfiArrayEltType->tty == ttyINT 
              )
         ) ) {
      KAPI_error_attribute = -1;
      return( -1 );
   }

   kapi_LookUpVariable_valhdr( pstn, &valhdr, idx );
   if ( valhdr.vals == valsUNSET ) {
      KAPI_error_attribute = -1;
      return( -1 );
   }

   return( valhdr.pvalnList->val.i );
}

bv_t *  
KAPI_GetBvVariable( void *pConfig, char *pchVariable, int idx )
{
   stn_t *pstn;
   valhdr_t valhdr;
   knobs_t *pknobs = pConfig;


   KAPI_error_attribute = 0;
   pstn = kapi_pstnLookup_noadd( pknobs, pchVariable );
   if ( pstn == NULL || pstn->ity != ityVARNAME ) {
      KAPI_error_attribute = -1;
      return( NULL );
   }

   if ( !( pstn->u.vfi.ptfi->tty == ttyBITMASK 
           ||
              ( pstn->u.vfi.ptfi->tty == ttyARRAY 
                &&
                pstn->u.vfi.ptfi->ptfiArrayEltType->tty == ttyBITMASK 
              )
         ) ) {
      KAPI_error_attribute = -1;
      return( NULL );
   }

   kapi_LookUpVariable_valhdr( pstn, &valhdr, idx );
   if ( valhdr.vals == valsUNSET ) {
      KAPI_error_attribute = -1;
      return( NULL );
   }

   return( pbvBuild4valhdr( pstn, &(valhdr) ) );
}

double  
KAPI_GetDoubleVariable( void *pConfig, char *pchVariable, int idx )
{
   stn_t *pstn;
   valhdr_t valhdr;
   knobs_t *pknobs = pConfig;

   KAPI_error_attribute = 0;
   pstn = kapi_pstnLookup_noadd( pknobs, pchVariable );
   if ( pstn == NULL || pstn->ity != ityVARNAME ) {
      KAPI_error_attribute = -1;
      return( -1.0 );
   }

   if ( !( pstn->u.vfi.ptfi->tty == ttyREAL 
           ||
              ( pstn->u.vfi.ptfi->tty == ttyARRAY 
                &&
                pstn->u.vfi.ptfi->ptfiArrayEltType->tty == ttyREAL 
              )
         ) ) {
      KAPI_error_attribute = -1;
      return( -1.0 );
   }

   kapi_LookUpVariable_valhdr( pstn, &valhdr, idx );
   if ( valhdr.vals == valsUNSET ) {
      KAPI_error_attribute = -1;
      return( -1.0 );
   }

   return( valhdr.pvalnList->val.r );
}

char *
KAPI_GetStringVariable( void *pConfig, char *pchVariable, int idx )
{
   stn_t *pstn;
   valhdr_t valhdr;

   knobs_t *pknobs = pConfig;

   KAPI_error_attribute = 0;
   pstn = kapi_pstnLookup_noadd( pknobs, pchVariable );
   if ( pstn == NULL || pstn->ity != ityVARNAME ) {
      KAPI_error_attribute = -1;
      return( NULL );
   }

   if ( !( pstn->u.vfi.ptfi->tty == ttySTRING 
           ||
              ( pstn->u.vfi.ptfi->tty == ttyARRAY 
                &&
                pstn->u.vfi.ptfi->ptfiArrayEltType->tty == ttySTRING 
              )
         ) ) {
      KAPI_error_attribute = -1;
      return( NULL );
   }

   kapi_LookUpVariable_valhdr( pstn, &valhdr, idx );
   if ( valhdr.vals == valsUNSET ) {
      KAPI_error_attribute = -1;
      return( NULL );
   }

   return( kapi_pchCopy( valhdr.pvalnList->val.pch ) );
}


/* --------------- attribute routines -------------------  */

int     
KAPI_count4attribute( void *pConfig, char *pchAttribute )
{
   stn_t *pstn;
   knobs_t *pknobs = pConfig;

   KAPI_error_attribute = 0;
   pstn = kapi_pstnLookup_noadd( pknobs, pchAttribute );
   if ( pstn == NULL || pstn->ity != ityATTRIBUTENAME ) {
      KAPI_error_attribute = -1;
      return( 0 );
   }

   return( pstn->u.afih.nAttr );
}

char *     
KAPI_attribute4index( void *pConfig, char *pchAttribute, int idx )
{
   stn_t *pstn;
   knobs_t *pknobs = pConfig;


   /* assume restructuring has happened */
   assert( pknobs->fRestructuredAttributes != 0 );

   KAPI_error_attribute = 0;
   pstn = kapi_pstnLookup_noadd( pknobs, pchAttribute );
   if ( pstn == NULL || pstn->ity != ityATTRIBUTENAME ) {
      KAPI_error_attribute = -1;
      return( NULL );
   }

   if ( idx >= pstn->u.afih.nAttr ) {
      KAPI_error_attribute = -1;
      return( NULL );
   }

   if (idx == 511)
    {
/*   printf ("Here!\n");*/
    }
    
    if ((pstn->u.afih.u.dmppch[ idx ])[0] == 0)
      {
/*      printf ("Here!\n");*/
      }
   return( kapi_pchCopy( pstn->u.afih.u.dmppch[ idx ] ) );
}

/* --------------- string conversion routines -------------------  */

int  
KAPI_EnumIndex( void *pConfig, char *pchType, char *pchEnumName )
{
   stn_t *pstnType, *pstnEnum;
   valn_t *pvalnEnum;
   int i;
   knobs_t *pknobs = pConfig;

   KAPI_error_attribute = 0;
   pstnType = kapi_pstnLookup_noadd( pknobs, pchType );
   if ( pstnType == NULL || pchEnumName == NULL ) {
      KAPI_error_attribute = -1;
      return( -1 );
   }


   if ( pstnType == NULL || pstnType->ity != ityTYPENAME ) {
      KAPI_error_attribute = -1;
      return( -1 );
   }

   /* KAPI 2.x compatibility mode */
   if ( 0 == strcmp( "port_t", pstnType->pchName ) ) {
	   kapi_Warning(0,0,"kapi 2.x port_t compatability not supported!");
	   return( -1 );
   }

   pvalnEnum = pstnType->u.tfi.pvalnEnums;
   for ( i=0; i < pstnType->u.tfi.nEnumConst; i++ ) {
      pstnEnum = kapi_pstnLookup_noadd( pknobs, pvalnEnum->val.pch );
      if ( pstnEnum != NULL 
           && 0 == strcmp( pstnEnum->pchName, pchEnumName ) ) { 
         return( i );
      }
      pvalnEnum = pvalnEnum->pvalnNext;
   }

   KAPI_error_attribute = -1;
   return( -1 );
}

int  
KAPI_EnumCardinality( void *pConfig, char *pchType )
{
   stn_t *pstn;
   knobs_t *pknobs = pConfig;

   KAPI_error_attribute = 0;
   pstn = kapi_pstnLookup_noadd( pknobs, pchType );
   if ( pstn == NULL || pstn->ity != ityTYPENAME ) {
      KAPI_error_attribute = -1;
      return( -1 );
   }

   /* KAPI 2.x compatibility */
   if ( 0 == strcmp( pstn->pchName, "port_t" ) ) {
	   kapi_Warning(0,0,"kapi 2.x port_t compatability not supported!");
	   return( -1 );
   } 

   return( pstn->u.tfi.nEnumConst );
}

char *  
KAPI_EnumName( void *pConfig, int enumconst, char *pchType )
{
   stn_t *pstnType;

   knobs_t *pknobs = pConfig;

   if ( enumconst < 0 ) {
      return( NULL );
   }

   pstnType = kapi_pstnLookup_noadd( pknobs, pchType );
   if ( pstnType == NULL || pstnType->ity != ityTYPENAME ) {
      return( NULL );
   }

   /* KAPI 2x compatibility not supported */ 

   if ( 0 == strcmp( "port_t", pchType ) ) {
	   kapi_Warning(0,0,"kapi 2.x port_t compatability not supported!");
   }
 
   return( kapi_pchCopy( kapi_enumname( pConfig, &(pstnType->u.tfi), enumconst ) ) );
}


/* ------------- Non-user visible routines here ------------- */


char *
kapi_enumname( knobs_t *pknobs, tfi_t *ptfi, int enumconst )
{
   int i;
   stn_t *pstnEnum;
   valn_t *pvalnEnum;

   pvalnEnum = ptfi->pvalnEnums;
   for ( i=0; i < enumconst; i++ ) {
      pvalnEnum = pvalnEnum->pvalnNext;
   }

   if ( pvalnEnum ) {
      pstnEnum = kapi_pstnLookup_noadd( pknobs, pvalnEnum->val.pch );
   } else {
      pstnEnum = NULL;
   }

   if ( pstnEnum ) {
      return( pstnEnum->pchName );
   } else {
      return( NULL );
   }
}

