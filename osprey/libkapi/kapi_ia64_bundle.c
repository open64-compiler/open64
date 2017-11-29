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


/* ---------- global variables ----------- */

int 
KAPI_SylCount_bid( void *pConfig, kapi_bid_t bid, kapi_syl_t syl )
{
   knobs_t *pknobs = pConfig;

   assert((pknobs->dmpbidinfoTable!=NULL) && "Syllable information not present!\n");
   return( pknobs->dmpbidinfoTable[ bid ].mpnsylAvail[ syl ] );
}

int 
KAPI_SbitPlacement_bid( void *pConfig, kapi_bid_t bid )
{
   knobs_t *pknobs = pConfig;

   assert((pknobs->dmpbidinfoTable!=NULL) && "Syllable information not present!\n");
   return( pknobs->dmpbidinfoTable[ bid ].isylSbit );
}

int 
KAPI_isReserved_bid( void *pConfig, kapi_bid_t bid )
{
   knobs_t *pknobs = pConfig;

   assert((pknobs->dmpbidinfoTable!=NULL) && "Syllable information not present!\n");
   return( pknobs->dmpbidinfoTable[ bid ].fReserved );
}
/* Get bundle name */
char *KAPI_bidName( void *pConfig, kapi_bid_t bid )
{
   knobs_t *pknobs = pConfig;

   assert((pknobs->dmpbidinfoTable!=NULL) && "Syllable information not present!\n");
   return (pknobs->dmpbidinfoTable[ bid ].pchBID);
}

void 
KAPI_SylOrder_bid( void *pConfig, kapi_bid_t bid, kapi_syl_t mpsyl[ 3 ] )
{
   knobs_t *pknobs = pConfig;

   assert((pknobs->dmpbidinfoTable!=NULL) && "Syllable information not present!\n");
   mpsyl[ 0 ] = pknobs->dmpbidinfoTable[ bid ].mpsylType[ 0 ];
   mpsyl[ 1 ] = pknobs->dmpbidinfoTable[ bid ].mpsylType[ 1 ];
   mpsyl[ 2 ] = pknobs->dmpbidinfoTable[ bid ].mpsylType[ 2 ];
}

void
KAPI_utCount_syl( void *pConfig, kapi_syl_t syl, kapi_ut_t mput[kapi_nUT] ) 
{
   kapi_ut_t ut;
   knobs_t *pknobs = pConfig;

   assert((pknobs->dmpsylinfoTable!=NULL) && "Syllable information not present!\n");
   for ( ut=kapi_utFIRST; ut<=kapi_utLAST; ut++ ) {
      mput[ ut ] = pknobs->dmpsylinfoTable[ syl ].mpnutNeeded[ ut ];
   }
}
