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
#include "kapi_bv.h"
#include "kapi_ia64.h"

#define pchNULL_PRINT( pch ) ( ( pch ) ? (pch) : "Null" )


/* ---------- global variables ----------- */


/* -------------------  User visible routines  ------------------- */

int
KAPI_BundleIssueWidth( void *pConfig, kapi_cluster_t cluster )
{
   knobs_t *pknobs = pConfig;
   assert ((pknobs->maxBundleIssue!=NULL) && "Bundle issue width information not presnt!\n");

   if ( cluster == -1 ) {
      int i, total;

      total = 0;
      for ( i=0; i<pknobs->nclr; i++ ) {
        total +=  pknobs->maxBundleIssue[ i ];
      }
      return( total );
   } else {
      return( pknobs->maxBundleIssue[ cluster ] );
   }
}

int 
KAPI_DisperseCount4syl( void *pConfig, kapi_syl_t syl )
{
   knobs_t *pknobs = pConfig;
   assert((pknobs->mpnut!=NULL) && "syllable information not present!\n");

   switch( syl ) {
      case kapi_sylI:
         return( pknobs->mpnut[ kapi_utI ] );       
      case kapi_sylF:
         return( pknobs->mpnut[ kapi_utF ] );       
      case kapi_sylM:
         return( pknobs->mpnut[ kapi_utM ] );       
      case kapi_sylB:
         return( pknobs->mpnut[ kapi_utB ] );       

      case kapi_sylL: /* SPECIAL CASE HACK - KLUDGE */ {
         int iclr, t;

         t = 0;
         for ( iclr=0; iclr<pknobs->nclr; iclr++ ) {
             t += pknobs->maxBundleIssue[ iclr ];
         }
         return( t );
      }
      default: 
         assert( 0 );
		 return 0;
   }
}

int 
KAPI_fuCount( void *pConfig )
{
   knobs_t *pknobs = pConfig;
   return( pknobs->nfuinfoTable );
}


/* translate fu enum to fu name, provided for ease of access */
char *
KAPI_fu2fuName( void *pConfig, kapi_fu_t fu, int iVariant )
{
   knobs_t *pknobs = (knobs_t *)pConfig;
   fuinfo_t *pfuinfo;
/*   assert(pknobs->pfSaveHeaderFlags==NULL && "Names are not saved in header usage model");*/

   assert(fu<pknobs->nfuinfoTable);
   pfuinfo = &(pknobs->dmpfuinfoTable[fu]); 

   assert(pfuinfo != NULL);
   assert(pfuinfo->fu == fu);
   assert(strcmp(KAPI_EnumName( pknobs, pfuinfo->fu, "fu_t" ),pfuinfo->pchName)==0);

   return (pfuinfo->pchName);
}

/* translate functional unit class name to kapi index, return -1 if not found */
int KAPI_fuName2fuIndex( void *pConfig, char *fuName )
{
   knobs_t *pknobs = (knobs_t *)pConfig;
   fuinfo_t *pfuinfo;
   int fu;

   for (fu=0;fu<pknobs->nfuinfoTable;fu++)
   {
	   pfuinfo = &(pknobs->dmpfuinfoTable[fu]); 
	   assert(pfuinfo != NULL);
	   if (strcmp(pfuinfo->pchName,fuName)==0)
		   return (pfuinfo->fu);
   }

   return (-1);
}

bv32_t KAPI_fuGetMiscInfo(void *pConfig, kapi_fu_t fu)
{
   knobs_t *pknobs = (knobs_t *)pConfig;
   fuinfo_t *pfuinfo;

   pfuinfo = &(pknobs->dmpfuinfoTable[fu]); 
   assert(pfuinfo != NULL);

   return pfuinfo->bv32InfoBits;

}

kapi_latency_type_t KAPI_fuGetLatencyType(void *pConfig, kapi_fu_t fu)
{
   knobs_t *pknobs = (knobs_t *)pConfig;
   fuinfo_t *pfuinfo;

   pfuinfo = &(pknobs->dmpfuinfoTable[fu]); 
   assert(pfuinfo != NULL);

   if (isbv32BITSET(pfuinfo->bv32InfoBits,kapi_fu_info_approximate_latency))
	   return kapi_latency_type_approximate;
   if (isbv32BITSET(pfuinfo->bv32InfoBits,kapi_fu_info_no_latency_info))
	   return kapi_latency_type_none;
   return kapi_latency_type_full;
}

int  
KAPI_clusterCount( void *pConfig )
{
   knobs_t *pknobs = pConfig;

   return( pknobs->nclr );
}


bv32_t
KAPI_cportMask4fu( void *pConfig, kapi_cluster_t cluster, kapi_fu_t fu )
{
   int i;
   cportinfo_t *pcportinfo;
   bv32_t bv32;
   knobs_t *pknobs = pConfig;

   bv32 = 0;
   KAPI_error_attribute = 0;
   if ( cluster == -1 ) {
      fprintf( stderr, "-1 argument to KAPI_cportMask4fu no longer supported\n" );
      fflush( stderr );
      assert( 0 );
   } else if ( cluster < pknobs->nclr ) {
      clr_t *pclr;

      pclr = &(pknobs->mpclrTable[ cluster ]);

      for ( i=0; i<pclr->ncports; i++ ) {
         pcportinfo = pclr->mppcportinfoTable[ i ];

         if ( isbvBITSET( &(pcportinfo->bvfuAllowed), fu ) ) {
            bv32 |= ( 1 << i );
         }
      }
   } else {
      KAPI_error_attribute = -1;
      bv32 = 0;
   }

   return( bv32 );
}

bv32_t
KAPI_cportMask4ut( void *pConfig, kapi_cluster_t cluster, kapi_ut_t ut )
{
   knobs_t *pknobs = pConfig;
   int i;
   cportinfo_t *pcportinfo;
   bv32_t bv32;

   bv32 = 0;
   if ( cluster == -1 ) {
      fprintf( stderr, "-1 argument to KAPI_cportMask4ut no longer supported\n" );
      fflush( stderr );
      assert( 0 );
   } else if ( cluster < pknobs->nclr ) {
      clr_t *pclr;

      pclr = &(pknobs->mpclrTable[ cluster ]);

      for ( i=0; i<pclr->ncports; i++ ) {
         pcportinfo = pclr->mppcportinfoTable[ i ]; 
         assert( pcportinfo );

         if ( pcportinfo->ut == ut) {
            bv32 |= ( 1 << i );
         }
      }
   } else {
      KAPI_error_attribute = -1;
      bv32 = 0;
   }

   return( bv32 );
}

int 
KAPI_cportCount( void *pConfig, kapi_cluster_t cluster )
{
   knobs_t *pknobs = pConfig;

   if ( cluster == -1 ) {
      return( pknobs->nports );
   } else if ( cluster >= pknobs->nclr ) {
      return( 0 );
   } else {
      return( pknobs->mpclrTable[ cluster ].ncports );
   }
}

int 
KAPI_cutportCount( void *pConfig, kapi_cluster_t cluster, kapi_ut_t utIn )
{
   knobs_t *pknobs = pConfig;
   int cnt;

   if ( cluster == -1 ) {
      cnt = pknobs->mpnut[ utIn ];
   } else if ( cluster >= pknobs->nclr ) {
      cnt = 0;
   } else {
      cnt = pknobs->mpclrTable[ cluster ].mpncutport[ utIn ];
   }

   return( cnt );
}

int
KAPI_cportCount4fu( void *pConfig, kapi_cluster_t cluster, kapi_fu_t fu )
{
   int port, cnt;
   cportinfo_t *pcportinfo;
   knobs_t *pknobs = pConfig;

   if ( cluster >= pknobs->nclr ) {
      return( 0 );
   } 
 
   assert( fu < pknobs->nfuinfoTable );

   cnt = 0;
   for ( port=0; port<pknobs->nports; port++ ) {
      pcportinfo = pknobs->mppportinfoTable[ port ];

      assert( pcportinfo && pcportinfo->bvfuAllowed.pint32Data );
      if ( isbvBITSET( &(pcportinfo->bvfuAllowed), fu ) ) {
         if ( cluster == -1 ||
              cluster == pknobs->port2cluster[ port ] ) {
            cnt++;
         } 
      }
   }

   return( cnt  );
}


int
KAPI_cportCount4ut( void *pConfig, kapi_cluster_t cluster, kapi_ut_t ut )
{
   int port, cnt;
   cportinfo_t *pcportinfo;
   knobs_t *pknobs = pConfig;

   if ( cluster >= pknobs->nclr ) {
      return( 0 );
   } 
 
   assert( ut < pknobs->nfuinfoTable );

   cnt = 0;
   for ( port=0; port<pknobs->nports; port++ ) {
      pcportinfo = pknobs->mppportinfoTable[ port ];

      assert( pcportinfo && pcportinfo->bvfuAllowed.pint32Data );
      if ( pcportinfo->ut==ut ) {
         if ( cluster == -1 ||
              cluster == pknobs->port2cluster[ port ] ) {
            cnt++;
         } 
      }
   }

   return( cnt  );
}


void  
KAPI_portInfo( void *pConfig, kapi_port_t port, 
               kapi_cluster_t *pcluster, kapi_cport_t *pcport,
               kapi_ut_t *put, kapi_cutport_t *pcutport )
{
   knobs_t *pknobs = pConfig;
   cportinfo_t *pcportTmp;

   assert(port != -1 && "Port must be specified!\n");
   assert((pknobs->mppportinfoTable!=NULL) && "port info not present!\n");

   KAPI_error_attribute = 0;

   pcportTmp = pknobs->mppportinfoTable[ port ];

   *pcluster = pknobs->port2cluster[ port ];
   *pcport = pcportTmp->cport;
   *put = pcportTmp->ut;
   *pcutport = pcportTmp->cutport;
}


void  
KAPI_cportInfo( void *pConfig, kapi_cluster_t cluster, kapi_cport_t cport,
                kapi_port_t *pport, kapi_ut_t *put, kapi_cutport_t *pcutport )
{
   knobs_t *pknobs = pConfig;
   cportinfo_t *pcport;
   clr_t *pclr;

   assert(cluster != -1 && "Cluster must be specified!\n");
   assert(cport   != -1 && "Cport must be specified!\n");
   assert((pknobs->mpclrTable!=NULL) && "cport info not present!\n");

   KAPI_error_attribute = 0;

   pclr = &(pknobs->mpclrTable[ cluster ]);

   pcport = pclr->mppcportinfoTable[ cport ];

   *pport    = pclr->cport2port[ cport ];
   *put      = pcport->ut;
   *pcutport = pcport->cutport;
}

void  
KAPI_cutportInfo( void *pConfig, kapi_cluster_t cluster, 
                  kapi_ut_t ut, kapi_cutport_t cutport,
                  kapi_port_t *pport, kapi_cport_t *pcport )
{
   knobs_t *pknobs = pConfig;
   kapi_cport_t cport;
   clr_t *pclr;

   assert(cluster != -1 && "Cluster must be specified!\n");
   assert(!((ut == -1 && cutport != -1) || (ut != -1 && cutport == -1))
			&& "ut and cutport must either both be specified or neither specified");

   assert((pknobs->mpclrTable!=NULL) && "cport info not present!\n");
   pclr = &(pknobs->mpclrTable[ cluster ]);

   assert((pclr->dmppcportinfoTable!=NULL) && "cutport info not present!\n");

   if (ut == -1 && cutport == -1)
   {
	   *pcport = -1; *pport = -1;
   }
   else
   {
	   cport = pclr->dmppcportinfoTable[ ut ][ cutport ].cport;

	   *pcport = cport;
	   *pport  = pclr->cport2port[ cport ];

	   KAPI_error_attribute = 0;
	}
}

/* -------------------------------------------------------------------- */
