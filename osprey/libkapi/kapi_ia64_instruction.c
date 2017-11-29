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
#include "kapi_util.h"
#include "kapi_error.h"


/* ---------- global variables ----------- */


/* -------------------  User visible routines  ------------------- */

int
KAPI_iidCount( void *pConfig )
{
   knobs_t *pknobs = pConfig;

   return( pknobs->ninstTable );
}

kapi_it_t
KAPI_iid2it( void *pConfig, kapi_iid_t iid, int iVariant )
{
   knobs_t *pknobs = pConfig;

   assert( iid < pknobs->ninstTable );
   return( pknobs->dmpinstTable[ iid ].it );
}

kapi_fu_t
KAPI_iid2fu( void *pConfig, kapi_iid_t iid, int iVariant )
{
   knobs_t *pknobs = pConfig;

   assert( iid < pknobs->ninstTable );
   return( pknobs->dmpinstTable[ iid ].fu );
}

char *
KAPI_iid2mnemonic( void *pConfig, kapi_iid_t iid, int iVariant )
{
   knobs_t *pknobs = pConfig;
   assert(pknobs->pfSaveHeaderFlags==NULL && "Names are not saved in header usage model");

   assert( iid < pknobs->ninstTable );
   return( pknobs->dmpinstTable[ iid ].pchMnemonic );
}

char *
KAPI_iid2uniqueName( void *pConfig, kapi_iid_t iid, int iVariant )
{
   knobs_t *pknobs = pConfig;
   assert(pknobs->pfSaveHeaderFlags==NULL && "Names are not saved in header usage model");

   assert( iid < pknobs->ninstTable );
   return( pknobs->dmpinstTable[ iid ].pchUniqueName );
}

kapi_iid_t
KAPI_uniqueName2iid( void *pConfig, char *pchName, int iVariant )
{
   knobs_t *pknobs = pConfig;
   int i;

   for ( i=0; i<pknobs->ninstTable; i++ ) {
      if ( 0 == strcmp( pchName, pknobs->dmpinstTable[ i ].pchUniqueName ) ) {
         return( i );
      }
   }
   return( -1 );
}


int
KAPI_oppGetSource( void *pConfig, kapi_fu_t fu, char *pchOppName )
{
   knobs_t *pknobs = pConfig;
   int i;
   /*assert((pknobs->pfSaveHeaderFlags==NULL) && "Strings are not saved in header usage model!\n");*/

   /* NOTE: There are maximum of maxDESTINATIONS destinations
      -- do not exceed this or the implementation will break */

   if ( fu >= pknobs->nfuinfoTable ) {
      return( -1 );
   }

   if ( pchOppName == NULL || 0 == strcmp( pchOppName, "primary" ) ) {
	  if (pknobs->dmpfuinfoTable[ fu ].bv32InfoBits & (1<<kapi_fu_info_no_primary_source) )
		   return -1;
      return( 0 );
   }

   for ( i=0; i<pknobs->dmpfuinfoTable[ fu ].cntSrc; i++ ) {
      if ( 0 == strcmp( pknobs->dmpfuinfoTable[ fu ].mppchSrcName[ i ], pchOppName ) ) {
         return( i ); 
      }
   }

   return( -1 );
}

int
KAPI_oppGetDest( void *pConfig, kapi_fu_t fu, char *pchOppName )
{
   knobs_t *pknobs = pConfig;
   int i;
/*   assert((pknobs->pfSaveHeaderFlags==NULL) && "Strings are not saved in header usage model!\n");*/

   /* NOTE: There are maximum of maxDESTINATIONS destinations
      -- do not exceed this or the implementation will break */

   if ( fu >= pknobs->nfuinfoTable ) {
      return( -1 );
   }

   if ( pchOppName == NULL || 0 == strcmp( pchOppName, "primary" ) ) {
	  if (pknobs->dmpfuinfoTable[ fu ].bv32InfoBits & (1<<kapi_fu_info_no_primary_destination) )
		   return -1;
      return( 0 );
   }

   for ( i=0; i<pknobs->dmpfuinfoTable[ fu ].cntDest; i++ ) {
      if ( 0 == strcmp( pknobs->dmpfuinfoTable[ fu ].mppchDestName[ i ], pchOppName ) ) {
         return( i ); 
      }
   }

   return( -1 );
}

int
KAPI_srcOppCount( void *pConfig, kapi_fu_t fuSrc )
{
   knobs_t *pknobs = pConfig;

   return( pknobs->dmpfuinfoTable[ fuSrc ].cntSrc ); 
}

int
KAPI_destOppCount( void *pConfig, kapi_fu_t fuDest )
{
   knobs_t *pknobs = pConfig;

   return( pknobs->dmpfuinfoTable[ fuDest ].cntDest ); 
}

char *
KAPI_srcOppName( void *pConfig, kapi_fu_t fuSrc, int opp )
{
   knobs_t *pknobs = pConfig;
   assert(pknobs->pfSaveHeaderFlags==NULL && "Names are not saved in header usage model");

   return( pknobs->dmpfuinfoTable[ fuSrc ].mppchSrcName[ opp ] ); 
}

char *
KAPI_destOppName( void *pConfig, kapi_fu_t fuDest, int opp )
{
   knobs_t *pknobs = pConfig;
   assert(pknobs->pfSaveHeaderFlags==NULL && "Names are not saved in header usage model");

   return( pknobs->dmpfuinfoTable[ fuDest ].mppchDestName[ opp ] ); 
}


int KAPI_GetOppIndex(void *pConfig, kapi_iid_t iid, 
					 int iIndex, kapi_operand_role_e iRole,
					 char *pchOppName)
{

	knobs_t *pknobs = pConfig;

	/* if name exists, get directly from fu */
	if (pchOppName != NULL)
	{
	   switch (iRole)
	   {
	   case kapi_op_role_dest:
		   return KAPI_oppGetDest(pConfig,KAPI_iid2fu(pConfig,iid,0),pchOppName);
	   case kapi_op_role_src:
		   return KAPI_oppGetSource(pConfig,KAPI_iid2fu(pConfig,iid,0),pchOppName);
	   default:
		   return -1;
	   }
	}

	assert(pknobs);

	switch (iRole)
	{
	   case kapi_op_role_dest:
		   assert(pknobs->dmpinstTable[iid].pExplicitOps);
		   if (type_single==pknobs->dmpinstTable[iid].pExplicitOps->iType)
		   {
			   return pknobs->dmpinstTable[iid].
				pExplicitOps->pIndexes[0].pOperand;
		   }
		   else
		   {
			   return pknobs->dmpinstTable[iid].
				pExplicitOps->pIndexes[0].pOperandList->pIndexes[0].pOperand;
		   }
	   case kapi_op_role_src:
		   assert(pknobs->dmpinstTable[iid].pExplicitOps);
		   if (type_single==pknobs->dmpinstTable[iid].pExplicitOps->iType)
		   {
			   return pknobs->dmpinstTable[iid].
				pExplicitOps->pIndexes[1].pOperand;
		   }
		   else
		   {
			   return pknobs->dmpinstTable[iid].
				pExplicitOps->pIndexes[0].pOperandList->pIndexes[1].pOperand;
		   }

	   default:
		   return -1;
	}
}
