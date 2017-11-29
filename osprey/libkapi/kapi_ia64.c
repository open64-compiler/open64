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

#include <stdlib.h>
#include "kapi_internal.h"
#include "kapi.h"
#include "kapi_ia64.h"
#include "kapi_util.h"
#include "kapi_error.h"
#include "kapi_parse.h"






#define pchNULL_PRINT( pch ) ( ( pch ) ? (pch) : "Null" )

static void ParseClusterDistance( char *pch, char **ppchClrSrc, 
                          char **ppchClrDest, char **ppchValue );
static void ParseLatency( char *pch, char **ppchFcSrc,  
                          char **ppchOppSrc, int *piLatency );
static void ParseBypass( char *pch, char **ppchFcSrc, char **ppchPortSrc, 
                        char **ppchOppSrc, 
                        char **ppchFcDest, char **ppchPortDest, char **ppchOppDest,
                        char **ppchLatency );
static void ParseIntraCluster( char *mpchTmp, 
                                  char **ppchClr, 
                       char **ppchFcSrc, char **ppchOppSrc, char **ppchCportSrc,
                       char **ppchFcDest, char **ppchOppDest, char **ppchCportDest,
                                       char **ppchLatency );
static void ParseInterCluster( char *mpchTmp, 
                                  char **ppchClrSrc, char **ppchFcSrc, 
                                  char **ppchOppSrc, char **ppchCportSrc,
                                  char **ppchClrDest, char **ppchFcDest, 
                                  char **ppchOppDest, char **ppchCportDest,
                                       char **ppchLatency );

static int  fOkProcessClusterDistance( knobs_t *pknobs, char *pchClrSrc,  
                         char *pchClrDest, char *pchValue,  
                         int *iclusterSrc, int *iclusterDest, int *ivalue );
static int  fOkProcessIntraCluster( knobs_t *pknobs, aby_t *pabyTmp, 
                           char *pchClr, 
                      char *pchFcSrc, char *pchOppSrc, char *pchCportSrc,
                      char *pchFcDest, char *pchOppDest, char *pchCportDest,
                              char *pchLatency );

static int  fOkProcessInterCluster( knobs_t *pknobs, eby_t *pebyTmp, 
                           char *pchClrSrc, char *pchFcSrc, 
                           char *pchOppSrc, char *pchCportSrc,
                           char *pchClrDest, char *pchFcDest, 
                           char *pchOppDest, char *pchCportDest,
                              char *pchLatency );


static void kapi_InitNumberOfClrNutCportPort( knobs_t *pknobs );
static void kapi_InitInstructionType( knobs_t *pknobs );
static void kapi_ProcessCportPortClr( knobs_t *pknobs );
static void kapi_SetupWidths( knobs_t *pknobs );
static void kapi_ProcessFu( knobs_t *pknobs );
static void kapi_ProcessSensitivity( knobs_t *pknobs );
static void kapi_ProcessCoreLatency( knobs_t *pknobs );
static void kapi_ProcessInterclusterLatency( knobs_t *pknobs );
static void kapi_ProcessIntraclusterLatency( knobs_t *pknobs );
static void kapi_ProcessClusterDistance( knobs_t *pknobs );
static void kapi_ProcessInstructionData( knobs_t *pknobs );
static void kapi_ProcessCacheData( knobs_t *pknobs);

static void DecodeInstructionInformation( knobs_t *pknobs, char *pch );

/* ---------- global variables ----------- */

int
kapi_fEnableIA64Calls( knobs_t *pknobs )
{
   /* ORDER MAY BE IMPORTANT! */
   /* check sym_t */ /* check bid_t */
   kapi_InitBidAndSyllable( pknobs );
   /* check ut_t */
   kapi_InitNumberOfClrNutCportPort( pknobs );
   /* check it_t */
   kapi_InitInstructionType( pknobs );
   /* set up cluster/port mappings */
   kapi_ProcessCportPortClr( pknobs );
   /* issue and bundle width */
   kapi_SetupWidths( pknobs );
   kapi_ProcessFu( pknobs );
   kapi_ProcessSensitivity( pknobs );
   kapi_ProcessCoreLatency( pknobs );
   kapi_ProcessClusterDistance( pknobs );
   kapi_ProcessInstructionData( pknobs );
   kapi_ProcessIntraclusterLatency( pknobs );
   kapi_ProcessInterclusterLatency( pknobs );
   kapi_ProcessCacheData( pknobs );
   if ( kapi_cntSemanticError > 0 || kapi_cntSyntaxError > 0 ) {
      return( 0 );
   } else {
      return( 1 );
   }
}

static void
kapi_ProcessCportPortClr( knobs_t *pknobs )
{
   int iport, icluster, icutport, nutTotal;
   kapi_ut_t ut;
   kapi_ut_t nutMAX[ kapi_nUT ];
  

   nutMAX[ kapi_utI ] = maxICPORTS;
   nutMAX[ kapi_utF ] = maxFCPORTS;
   nutMAX[ kapi_utB ] = maxBCPORTS;
   nutMAX[ kapi_utM ] = maxMCPORTS;

   /* setup pknobs->port2cluster[] by counting off ports */
   /* setup pknobs->mpclrTable[ ].cport2port[] */
   nutTotal = 0;
   for ( icluster=0; icluster<pknobs->nclr; icluster++ ) {
      clr_t *pclr;

      pclr = &(pknobs->mpclrTable[ icluster ]);
      for ( ut=kapi_utFIRST; ut<=kapi_utLAST; ut++ ) {
         if ( pclr->mpncutport[ ut ] > nutMAX[ ut ] ) {
            kapi_Error_i1( 0, 0, "KAPI Implementation maximum for %c units exceeded on cluster",
                 kapi_ut2ch( ut ) );
         }
         nutTotal += pclr->mpncutport[ ut ];
      }
   }
   if ( nutTotal > maxPORTS ) {
      kapi_Error_i1( 0, 0, "KAPI implementation maximum units (%d) exceeded",
           maxPORTS );
   }

   iport = 0;
   while ( iport < pknobs->nports ) {
      for ( icluster=0; icluster<pknobs->nclr; icluster++ ) {
         clr_t *pclr;
         kapi_cport_t icport;

         icport = 0;
         pclr = &(pknobs->mpclrTable[ icluster ]);
         for ( ut=kapi_utFIRST; ut<=kapi_utLAST; ut++ ) {
            if ( pclr->mpncutport[ ut ] > nutMAX[ ut ] ) {
               kapi_Error_i1( 0, 0, "KAPI Implementation maximum for %c units exceeded on cluster",
                    kapi_ut2ch( ut ) );
            }
            for ( icutport=0; icutport<pclr->mpncutport[ ut ]; icutport++ ) {

               pknobs->port2cluster[ iport ] = icluster;
               pclr->cport2port[ icport ] = iport;
               pclr->dmppcportinfoTable[ ut ][ icutport ].cport = icport;
               pclr->dmppcportinfoTable[ ut ][ icutport ].cutport = icutport;
               pclr->dmppcportinfoTable[ ut ][ icutport ].ut = ut;
               pclr->mppcportinfoTable[ icport ] 
                              = &(pclr->dmppcportinfoTable[ ut ][ icutport ]);
               iport++;   /* global port number */
               icport++;  /* local cport number */
            }
         }
      }
   } /* of iport loop */
}

static void
kapi_ProcessSensitivity( knobs_t *pknobs )
{
	int nfu, i, ndir;

	nfu = KAPI_EnumCardinality( pknobs, "fu_t");

    pknobs->dmpfudirTable = (int *)malloc( sizeof( int ) * nfu );

	ndir = KAPI_EnumCardinality( pknobs, "operand_direction_t");
    if ( KAPI_error_attribute ) {
      kapi_Error( kapi_cLine, 0, "'operand_direction_t' not defined" );
      return;
   }

	pknobs->fudirDestination = KAPI_EnumIndex( pknobs, "operand_direction_t", 
			                              "operand_direction_destination" );
	pknobs->fudirSource = KAPI_EnumIndex( pknobs, "operand_direction_t", 
			                              "operand_direction_source" );
	if ( pknobs->fudirSource != 0 ||
 	    pknobs->fudirDestination != 1 ) {
        kapi_Error_pch1( kapi_cLine, 0, "operand_direction_t must be defined to be enum (operand_direction_source, operand_direction_destination)", "" );
    }

	for ( i=0; i<nfu; i++ ) {
        pknobs->dmpfudirTable[ i ] = 
			   KAPI_GetEnumVariable(pknobs,"operand_direction", i);
	}
}

static void
kapi_SetupWidths( knobs_t *pknobs )
{
   int icluster;
   char mpchTmp[ 300 ];

   /* issue width */
   for ( icluster=0; icluster<pknobs->nclr; icluster++ ) {
      sprintf( mpchTmp, "cluster%dMaxBundleIssue", icluster );
      pknobs->maxBundleIssue[ icluster ] = 
                      KAPI_GetIntegerVariable( pknobs, mpchTmp, 0 );

      pknobs->maxInstructionIssue[ icluster ] = pknobs->maxBundleIssue[ icluster ] * nSYLBID;

      if ( KAPI_error_attribute ) {
         kapi_Error_pch1( kapi_cLine, 0, "'%s' must be defined of type int", mpchTmp );
      }
   }

}

static void 
kapi_InitInstructionType( knobs_t *pknobs )
{
   int nit, i, fError;

   nit = KAPI_EnumCardinality( pknobs, "it_t");
   if ( KAPI_error_attribute ) {
      kapi_Error( kapi_cLine, 0, "'it_t' not defined" );
      return;
   }

   if ( nit != kapi_nIT ) {
      kapi_Error_i1( kapi_cLine, 0, "'it_t' must currently have %d components", kapi_nIT );
      return;
   }

   fError = 0;
   for ( i=0; i<nit; i++ ) {
      char *pch;

      pch = KAPI_EnumName( pknobs, i, "it_t" );
      switch( i ) {
        case kapi_itA:
           if ( 0 != strcmp( pch, "itA" ) ) {
              fError = 1;
           }
           break;
        case kapi_itI:
           if ( 0 != strcmp( pch, "itI" ) ) {
              fError = 1;
           }
           break;
        case kapi_itF:
           if ( 0 != strcmp( pch, "itF" ) ) {
              fError = 1;
           }
           break;
        case kapi_itM:
           if ( 0 != strcmp( pch, "itM" ) ) {
              fError = 1;
           }
           break;
        case kapi_itBl:
           if ( 0 != strcmp( pch, "itBl" ) ) {
              fError = 1;
           }
           break;
        case kapi_itB:
           if ( 0 != strcmp( pch, "itB" ) ) {
              fError = 1;
           }
           break;
      }
   }

   if ( fError ) {
      kapi_Error( kapi_cLine, 0, "Order of 'it_t' enums is not correct" );
      return;
   }

   pknobs->nitinfoTable = nit;
   pknobs->dmpitinfoTable = (itinfo_t *)malloc( sizeof( itinfo_t ) * nit );

#define INIT_IT_ENTRY( _idxit, _pchit, _bv32syl ) \
   pknobs->dmpitinfoTable[ (_idxit) ].pchitName = (_pchit); \
   pknobs->dmpitinfoTable[ (_idxit) ].it = (_idxit); \
   pknobs->dmpitinfoTable[ (_idxit) ].bv32sylAllowed = _bv32syl;

   INIT_IT_ENTRY( kapi_itA,    "itA",   bv32sylI | bv32sylM );
   INIT_IT_ENTRY( kapi_itI,    "itI",   bv32sylI );
   INIT_IT_ENTRY( kapi_itM,    "itM",   bv32sylM );
   INIT_IT_ENTRY( kapi_itB,    "itB",   bv32sylB );
   INIT_IT_ENTRY( kapi_itBl,   "itBl",  bv32sylB );
   INIT_IT_ENTRY( kapi_itF,    "itF",   bv32sylF );
   INIT_IT_ENTRY( kapi_itL,    "itL",   bv32sylL );


   /* compute max avail information */
   pknobs->dmpitinfoTable[ kapi_itA ].maxAvail = 
         pknobs->mpnut[ kapi_utI ] +
         pknobs->mpnut[ kapi_utM ];
   pknobs->dmpitinfoTable[ kapi_itM ].maxAvail = 
         pknobs->mpnut[ kapi_utM ];
   pknobs->dmpitinfoTable[ kapi_itI ].maxAvail = 
         pknobs->mpnut[ kapi_utI ];
   pknobs->dmpitinfoTable[ kapi_itB ].maxAvail = 
         pknobs->mpnut[ kapi_utB ];
   pknobs->dmpitinfoTable[ kapi_itF ].maxAvail = 
         pknobs->mpnut[ kapi_utF ];

   /* these are faked using utF (number of bundles) */
   pknobs->dmpitinfoTable[ kapi_itBl ].maxAvail = 
         pknobs->mpnut[ kapi_utF ];
   pknobs->dmpitinfoTable[ kapi_itL ].maxAvail = 
         pknobs->mpnut[ kapi_utF ];
}


static void
kapi_InitNumberOfClrNutCportPort( knobs_t *pknobs )
{
   int i, nut, iclr, iport, cutport; /* nport ?*/
   int fError, nclr;
   kapi_ut_t ut;

   clr_t *pclr;

   nut = KAPI_EnumCardinality( pknobs, "ut_t" );
   if ( KAPI_error_attribute ) {
      kapi_Error( kapi_cLine, 0, "'ut_t' not defined or incorrect" );
      return;
   }
   if ( nut != kapi_nUT ) {
      kapi_Error( kapi_cLine, 0, "There must currently be 4 unit types defined" );
      return;
   }

   fError = 0;
   for ( i=0; i<nut; i++ ) {
      char *pch;

      pch = KAPI_EnumName( pknobs, i, "ut_t" );
      switch( i ) {
        case kapi_utI:
           if ( 0 != strcmp( pch, "utI" ) ) {
              fError = 1;
           }
           break;
        case kapi_utF:
           if ( 0 != strcmp( pch, "utF" ) ) {
              fError = 1;
           }
           break;
        case kapi_utM:
           if ( 0 != strcmp( pch, "utM" ) ) {
              fError = 1;
           }
           break;
        case kapi_utB:
           if ( 0 != strcmp( pch, "utB" ) ) {
              fError = 1;
           }
           break;
        default:
           assert( 0 );
      }
   }
   if ( fError ) {
      kapi_Error( kapi_cLine, 0, "Order of 'ut_t' enums is not correct" );
      return;
   }


   nclr = KAPI_GetIntegerVariable( pknobs, "NumberOfClusters", 0 );
   if ( nclr > maxCLR ) {
      kapi_Error_i1( kapi_cLine, 0, 
                "Kapi currently supports a maximum of %d clusters", nclr );
      return;
   } 
   if ( nclr > 0 ) {
      pknobs->nclr = nclr;
   }

   /* init globals */
   for ( i=0; i<nut; i++ ) {
      pknobs->mpnut[ i ] = 0;
   }
   pknobs->nports = 0;

   /* cannot read cluster 0 from variable to allow port_t compatibility */
   /*pclr = &(pknobs->mpclrTable[ 0 ]);
   for ( ut=kapi_utFIRST; ut<=kapi_utLAST; ut++ ) {
      pknobs->mpnut[ ut ] += pclr->mpncutport[ ut ];
      pknobs->nports += pclr->mpncutport[ ut ];
      pclr->ncports += pclr->mpncutport[ ut ];
   }*/

   for ( iclr=0; iclr<nclr; iclr++ ) {
      char mpchCluster[ 100 ];

      sprintf( mpchCluster, "cluster%dCutports", iclr );
      pclr = &(pknobs->mpclrTable[ iclr ]);

      if ( KAPI_VariableCardinality( pknobs, mpchCluster ) == -1 ) {
         kapi_Error_pch1( 0, 0, "%s must be defined", mpchCluster );
         continue;
      } 

      pclr->ncports = 0;

      for ( ut=kapi_utFIRST; ut<=kapi_utLAST; ut++ ) {
         pclr->mpncutport[ ut ] 
              = KAPI_GetIntegerVariable( pknobs, mpchCluster, ut );
         if ( pclr->mpncutport[ ut ] == -1 ) {
            kapi_Error_i1( kapi_cLine, 0, 
               "Number of units for cluster %d not fully specified", nclr );
            pknobs->nclr = iclr;
            pclr->mpncutport[ ut ] = 0;
         }
     
         /* update counts */
         pknobs->mpnut[ ut ] += pclr->mpncutport[ ut ];
         pknobs->nports += pclr->mpncutport[ ut ];
         pclr->ncports += pclr->mpncutport[ ut ];
      }
   }

   /* makes global port info point into cport copies */
   iport = 0;
   for ( iclr=0; iclr<pknobs->nclr; iclr++ ) {
      pclr = &(pknobs->mpclrTable[ iclr ]);
 
      for ( ut=kapi_utFIRST; ut<=kapi_utLAST; ut++ ) {
        for ( cutport=0;  
               cutport < pclr->mpncutport[ ut ];
               cutport++ ){
            pknobs->mppportinfoTable[ iport ] = 
               &(pclr->dmppcportinfoTable[ ut ][ cutport ]);
            iport++;
         }
      }
   }
}

char *
pchTranslateBypass2Intracluster( knobs_t *pknobs, char *pchIn )
{
   char *pchfuSrc, *pchPortSrc, *pchOppSrc, *pchValue; 
   char *pchfuDest, *pchPortDest, *pchOppDest;
   char mpchTmp[ 400 ];
   char mpchTmp2[ 400 ], *pchNew;

   (void)strncpy( (char *)mpchTmp, pchIn, 399 );

   ParseBypass( mpchTmp, 
                &pchfuSrc, &pchPortSrc, &pchOppSrc, 
                &pchfuDest, &pchPortDest, &pchOppDest, 
                &pchValue );
                         
   if ( pchOppSrc != NULL || pchOppDest != NULL ) {
      kapi_Error( kapi_cLine, 0, "Cannot translate KAPI2x BYPASS -- cannot handle operand specifications" );
   }

   mpchTmp2[ 0 ] = '\0';
   pchNew = strcat( mpchTmp2, pchfuSrc );
   if ( pchPortSrc != NULL ) {
      pchNew = strcat( pchNew, "/primary/c" );
      pchNew = strcat( pchNew, pchPortSrc );
   } 

   pchNew = strcat( pchNew, ":" );
   pchNew = strcat( pchNew, pchfuDest );

   if ( pchPortDest != NULL ) {
      pchNew = strcat( pchNew, "/primary/c" );
      pchNew = strcat( pchNew, pchPortDest );
   } 

   pchNew = strcat( pchNew, "=" );
   pchNew = strcat( pchNew, pchValue );
 
   return( strdup( pchNew ) );
}

static void
ParseBypass( char *pch, char **ppchFcSrc, char **ppchPortSrc, char **ppchOppSrc, 
                        char **ppchFcDest, char **ppchPortDest, char **ppchOppDest,
                        char **ppchLatency )
{
   char *pchSrc, *pchDest, *pchVal;

   /* ---- process RHS of string ( latency value ) ---- */

   pchVal = (char *)strchr( pch, '=' );
   if ( pchVal == NULL ) {   /* error */
      kapi_Error_pch1( -1, 0, "Malformed bypass attribute '%s' -- '=' required", pch );
      return;
   }
   *pchVal = '\0';   /* cut string in half */

   /* point to first character after '=' */
   *ppchLatency = pchVal + 1;


   /* ---- now process LHS of string ---- */
   
   pchDest = (char *)strchr( pch, ':' );
   if ( pchDest == NULL ) {   /* error */
      kapi_Error_pch1( -1, 0, "Malformed bypass attribute '%s' -- ':' required", pch );
      return;
   }
   *pchDest = '\0';
   pchDest = pchDest + 1;  /* same as for '=' */
   pchSrc = pch;

   /* initialize return values */
   *ppchFcSrc   = NULL;
   *ppchPortSrc = NULL;
   *ppchOppSrc  = NULL;
   *ppchFcDest   = NULL;
   *ppchPortDest = NULL;
   *ppchOppDest  = NULL;

   /* parse the source operand string */
   *ppchFcSrc   = strtok( pchSrc, " /" );
   if ( *ppchFcSrc ) {
      *ppchPortSrc = strtok( NULL, " /" );
      if ( *ppchPortSrc ) {
         *ppchOppSrc = strtok( NULL, " /" );
      }
   }

   /* parse the destination operand string */
   *ppchFcDest   = strtok( pchDest, "/" );
   if ( *ppchFcDest ) {
      *ppchPortDest = strtok( NULL, " /" );
      if ( *ppchPortDest ) {
         *ppchOppDest = strtok( NULL, " /" );
      }
   }
}

void
kapi_InitBidAndSyllable( knobs_t *pknobs )
{
   int i, nsyl, nbid;
   kapi_it_t it;
   kapi_bid_t bid;
   kapi_syl_t syl;
   int fError;


   nsyl = KAPI_EnumCardinality( pknobs, "syl_t" );

   if ( KAPI_error_attribute ) {
      kapi_Error( kapi_cLine, 0, "'syl_t' not defined according to EAS" );
   }

   if ( nsyl != 5 ) {
      kapi_Error( kapi_cLine, 0, "Current EAS requires syl_t to have 5 syllable types" );
      return;
   }

   fError = 0;
   for ( i=0; i<nsyl; i++ ) {
      char *pch;
      pch = KAPI_EnumName( pknobs, i, "syl_t" );
      if ( KAPI_error_attribute ) {
         kapi_Error_i1( kapi_cLine, 0, "Constant %d of 'syl_t' not defined", i );
      }

      switch( i ) {
        case kapi_sylL:
           if ( 0 != strcmp( pch, "sylL" ) ) {
              fError = 1;
           }
           break;
        case kapi_sylI:
           if ( 0 != strcmp( pch, "sylI" ) ) {
              fError = 1;
           }
           break;
        case kapi_sylF:
           if ( 0 != strcmp( pch, "sylF" ) ) {
              fError = 1;
           }
           break;
        case kapi_sylM:
           if ( 0 != strcmp( pch, "sylM" ) ) {
              fError = 1;
           }
           break;
        case kapi_sylB:
           if ( 0 != strcmp( pch, "sylB" ) ) {
              fError = 1;
           }
           break;
      }
   }
   if ( fError ) {
      kapi_Error( kapi_cLine, 0, "Order of 'syl_t' enums is not correct" );
      return;
   }

   pknobs->nsylinfoTable = nsyl;
   pknobs->dmpsylinfoTable = (sylinfo_t *)malloc( sizeof( sylinfo_t ) * nsyl );


   nbid = KAPI_EnumCardinality( pknobs, "bid_t");
   if ( KAPI_error_attribute ) {
      kapi_Error( kapi_cLine, 0, "'bid_t' not defined according to EAS" );
      return;
   }

   if ( nbid != kapi_nBID ) {
      kapi_Error( kapi_cLine, 0, "Current EAS requires bid_t to have 16 entries" );
      return;
   }

   pknobs->nbidinfoTable = nbid;
   pknobs->dmpbidinfoTable = (bidinfo_t *)malloc( sizeof( bidinfo_t ) * nbid );


   /* -------------- bundle information -------------- */

   pknobs->dmpbidinfoTable[ bidMII    ].pchBID = "bidMII";
   pknobs->dmpbidinfoTable[ bidMI_I    ].pchBID = "bidMI_I";
   pknobs->dmpbidinfoTable[ bidMLI    ].pchBID = "bidMLX";
   pknobs->dmpbidinfoTable[ bidRESERVED_3    ].pchBID = "bidRESERVED_3";
   pknobs->dmpbidinfoTable[ bidMMI    ].pchBID = "bidMMI";
   pknobs->dmpbidinfoTable[ bidM_MI    ].pchBID = "bidM_MI";
   pknobs->dmpbidinfoTable[ bidMFI    ].pchBID = "bidMFI";
   pknobs->dmpbidinfoTable[ bidMMF    ].pchBID = "bidMMF";
   pknobs->dmpbidinfoTable[ bidMIB    ].pchBID = "bidMIB";
   pknobs->dmpbidinfoTable[ bidMBB    ].pchBID = "bidMBB";
   pknobs->dmpbidinfoTable[ bidRESERVED_A    ].pchBID = "bidRESERVED_A";
   pknobs->dmpbidinfoTable[ bidBBB    ].pchBID = "bidBBB";
   pknobs->dmpbidinfoTable[ bidMMB    ].pchBID = "bidMMB";
   pknobs->dmpbidinfoTable[ bidRESERVED_D    ].pchBID = "bidRESERVED_D";
   pknobs->dmpbidinfoTable[ bidMFB    ].pchBID = "bidMFB";
   pknobs->dmpbidinfoTable[ bidRESERVED_F    ].pchBID = "bidRESERVED_F";

   for ( bid=kapi_bidFIRST; bid<=kapi_bidLAST; bid++ ) {
      char *pchBid;

      for ( syl=kapi_sylFIRST; syl<=kapi_sylLAST; syl++ ) {
         pknobs->dmpbidinfoTable[ bid ].mpnsylAvail[ syl ] = 0;
      }
      for ( it=kapi_itFIRST; it<=kapi_itLAST; it++ ) {
         pknobs->dmpbidinfoTable[ bid ].mpnitAvail[ it ] = 0;
      }
      pknobs->dmpbidinfoTable[ bid ].fReserved = 0;
      pknobs->dmpbidinfoTable[ bid ].isylSbit = 0;

      pchBid = KAPI_EnumName( pknobs, bid, "bid_t" );
      if ( 1 != KAPI_GetIntegerVariable( pknobs, "AllowAlternateBids", 0 ) ) {
         if ( strcmp( pchBid, pknobs->dmpbidinfoTable[ bid ].pchBID ) ) {
            kapi_Error_i1( kapi_cLine, 0,
               "Enumeration constant at position %d of 'bid_t' not defined according to EAS 2.5", bid );
         }
      }
   }

   pknobs->dmpbidinfoTable[ bidMII    ].mpnsylAvail[ kapi_sylI ] = 2;
   pknobs->dmpbidinfoTable[ bidMII    ].mpnsylAvail[ kapi_sylM ] = 1;

   pknobs->dmpbidinfoTable[ bidMII    ].mpsylType[ 0 ] = kapi_sylM;
   pknobs->dmpbidinfoTable[ bidMII    ].mpsylType[ 1 ] = kapi_sylI;
   pknobs->dmpbidinfoTable[ bidMII    ].mpsylType[ 2 ] = kapi_sylI;


   pknobs->dmpbidinfoTable[ bidMI_I   ].mpnsylAvail[ kapi_sylI ] = 2;
   pknobs->dmpbidinfoTable[ bidMI_I   ].mpnsylAvail[ kapi_sylM ] = 1;
   pknobs->dmpbidinfoTable[ bidMI_I   ].isylSbit = 2;

   pknobs->dmpbidinfoTable[ bidMI_I    ].mpsylType[ 0 ] = kapi_sylM;
   pknobs->dmpbidinfoTable[ bidMI_I    ].mpsylType[ 1 ] = kapi_sylI;
   pknobs->dmpbidinfoTable[ bidMI_I    ].mpsylType[ 2 ] = kapi_sylI;


   pknobs->dmpbidinfoTable[ bidMLI    ].mpnsylAvail[ kapi_sylM ] = 1;
   pknobs->dmpbidinfoTable[ bidMLI    ].mpnsylAvail[ kapi_sylL ] = 1;

   pknobs->dmpbidinfoTable[ bidMLI    ].mpsylType[ 0 ] = kapi_sylM;
   pknobs->dmpbidinfoTable[ bidMLI    ].mpsylType[ 1 ] = kapi_sylL;
   pknobs->dmpbidinfoTable[ bidMLI    ].mpsylType[ 2 ] = kapi_sylI;


   pknobs->dmpbidinfoTable[ bidRESERVED_3 ].fReserved = 1;
   pknobs->dmpbidinfoTable[ bidRESERVED_3 ].mpsylType[ 0 ] = kapi_sylI;
   pknobs->dmpbidinfoTable[ bidRESERVED_3 ].mpsylType[ 1 ] = kapi_sylI;
   pknobs->dmpbidinfoTable[ bidRESERVED_3 ].mpsylType[ 2 ] = kapi_sylI;


   pknobs->dmpbidinfoTable[ bidMMI    ].mpnsylAvail[ kapi_sylI ] = 1;
   pknobs->dmpbidinfoTable[ bidMMI    ].mpnsylAvail[ kapi_sylM ] = 2;

   pknobs->dmpbidinfoTable[ bidMMI    ].mpsylType[ 0 ] = kapi_sylM;
   pknobs->dmpbidinfoTable[ bidMMI    ].mpsylType[ 1 ] = kapi_sylM;
   pknobs->dmpbidinfoTable[ bidMMI    ].mpsylType[ 2 ] = kapi_sylI;


   pknobs->dmpbidinfoTable[ bidM_MI   ].mpnsylAvail[ kapi_sylI ] = 1;
   pknobs->dmpbidinfoTable[ bidM_MI   ].mpnsylAvail[ kapi_sylM ] = 2;
   pknobs->dmpbidinfoTable[ bidM_MI   ].isylSbit = 1;

   pknobs->dmpbidinfoTable[ bidM_MI    ].mpsylType[ 0 ] = kapi_sylM;
   pknobs->dmpbidinfoTable[ bidM_MI    ].mpsylType[ 1 ] = kapi_sylM;
   pknobs->dmpbidinfoTable[ bidM_MI    ].mpsylType[ 2 ] = kapi_sylI;


   pknobs->dmpbidinfoTable[ bidMFI    ].mpnsylAvail[ kapi_sylI ] = 1;
   pknobs->dmpbidinfoTable[ bidMFI    ].mpnsylAvail[ kapi_sylF ] = 1;
   pknobs->dmpbidinfoTable[ bidMFI    ].mpnsylAvail[ kapi_sylM ] = 1;

   pknobs->dmpbidinfoTable[ bidMFI    ].mpsylType[ 0 ] = kapi_sylM;
   pknobs->dmpbidinfoTable[ bidMFI    ].mpsylType[ 1 ] = kapi_sylF;
   pknobs->dmpbidinfoTable[ bidMFI    ].mpsylType[ 2 ] = kapi_sylI;


   pknobs->dmpbidinfoTable[ bidMMF    ].mpnsylAvail[ kapi_sylI ] = 0;
   pknobs->dmpbidinfoTable[ bidMMF    ].mpnsylAvail[ kapi_sylF ] = 1;
   pknobs->dmpbidinfoTable[ bidMMF    ].mpnsylAvail[ kapi_sylM ] = 2;

   pknobs->dmpbidinfoTable[ bidMMF    ].mpsylType[ 0 ] = kapi_sylM;
   pknobs->dmpbidinfoTable[ bidMMF    ].mpsylType[ 1 ] = kapi_sylM;
   pknobs->dmpbidinfoTable[ bidMMF    ].mpsylType[ 2 ] = kapi_sylF;


   pknobs->dmpbidinfoTable[ bidMIB    ].mpnsylAvail[ kapi_sylI ] = 1;
   pknobs->dmpbidinfoTable[ bidMIB    ].mpnsylAvail[ kapi_sylB ] = 1;
   pknobs->dmpbidinfoTable[ bidMIB    ].mpnsylAvail[ kapi_sylM ] = 1;

   pknobs->dmpbidinfoTable[ bidMIB  ].mpsylType[ 0 ] = kapi_sylM;
   pknobs->dmpbidinfoTable[ bidMIB  ].mpsylType[ 1 ] = kapi_sylI;
   pknobs->dmpbidinfoTable[ bidMIB  ].mpsylType[ 2 ] = kapi_sylB;


   pknobs->dmpbidinfoTable[ bidMBB    ].mpnsylAvail[ kapi_sylB ] = 2;
   pknobs->dmpbidinfoTable[ bidMBB    ].mpnsylAvail[ kapi_sylM ] = 1;

   pknobs->dmpbidinfoTable[ bidMBB  ].mpsylType[ 0 ] = kapi_sylM;
   pknobs->dmpbidinfoTable[ bidMBB  ].mpsylType[ 1 ] = kapi_sylB;
   pknobs->dmpbidinfoTable[ bidMBB  ].mpsylType[ 2 ] = kapi_sylB;


   pknobs->dmpbidinfoTable[ bidRESERVED_A ].fReserved = 1;

   pknobs->dmpbidinfoTable[ bidRESERVED_A  ].mpsylType[ 0 ] = kapi_sylI;
   pknobs->dmpbidinfoTable[ bidRESERVED_A  ].mpsylType[ 1 ] = kapi_sylI;
   pknobs->dmpbidinfoTable[ bidRESERVED_A  ].mpsylType[ 2 ] = kapi_sylI;


   pknobs->dmpbidinfoTable[ bidBBB  ].mpnsylAvail[ kapi_sylB ] = 3;

   pknobs->dmpbidinfoTable[ bidBBB  ].mpsylType[ 0 ] = kapi_sylB;
   pknobs->dmpbidinfoTable[ bidBBB  ].mpsylType[ 1 ] = kapi_sylB;
   pknobs->dmpbidinfoTable[ bidBBB  ].mpsylType[ 2 ] = kapi_sylB;


   pknobs->dmpbidinfoTable[ bidMMB    ].mpnsylAvail[ kapi_sylB ] = 1;
   pknobs->dmpbidinfoTable[ bidMMB    ].mpnsylAvail[ kapi_sylM ] = 2;

   pknobs->dmpbidinfoTable[ bidMMB  ].mpsylType[ 0 ] = kapi_sylM;
   pknobs->dmpbidinfoTable[ bidMMB  ].mpsylType[ 1 ] = kapi_sylM;
   pknobs->dmpbidinfoTable[ bidMMB  ].mpsylType[ 2 ] = kapi_sylB;


   pknobs->dmpbidinfoTable[ bidRESERVED_D ].fReserved = 1;

   pknobs->dmpbidinfoTable[ bidRESERVED_D  ].mpsylType[ 0 ] = kapi_sylI;
   pknobs->dmpbidinfoTable[ bidRESERVED_D  ].mpsylType[ 1 ] = kapi_sylI;
   pknobs->dmpbidinfoTable[ bidRESERVED_D  ].mpsylType[ 2 ] = kapi_sylI;


   pknobs->dmpbidinfoTable[ bidMFB    ].mpnsylAvail[ kapi_sylF ] = 1;
   pknobs->dmpbidinfoTable[ bidMFB    ].mpnsylAvail[ kapi_sylB ] = 1;
   pknobs->dmpbidinfoTable[ bidMFB    ].mpnsylAvail[ kapi_sylM ] = 1;

   pknobs->dmpbidinfoTable[ bidMFB  ].mpsylType[ 0 ] = kapi_sylM;
   pknobs->dmpbidinfoTable[ bidMFB  ].mpsylType[ 1 ] = kapi_sylF;
   pknobs->dmpbidinfoTable[ bidMFB  ].mpsylType[ 2 ] = kapi_sylB;


   pknobs->dmpbidinfoTable[ bidRESERVED_F ].fReserved = 1;

   pknobs->dmpbidinfoTable[ bidRESERVED_F  ].mpsylType[ 0 ] = kapi_sylI;
   pknobs->dmpbidinfoTable[ bidRESERVED_F  ].mpsylType[ 1 ] = kapi_sylI;
   pknobs->dmpbidinfoTable[ bidRESERVED_F  ].mpsylType[ 2 ] = kapi_sylI;


   for ( bid=kapi_bidFIRST; bid<=kapi_bidLAST; bid++ ) {
      pknobs->dmpbidinfoTable[ bid ].mpnitAvail[ kapi_itI ] =
                  pknobs->dmpbidinfoTable[ bid ].mpnsylAvail[ kapi_sylI ];
      pknobs->dmpbidinfoTable[ bid ].mpnitAvail[ kapi_itM ] =
                  pknobs->dmpbidinfoTable[ bid ].mpnsylAvail[ kapi_sylM ];
      pknobs->dmpbidinfoTable[ bid ].mpnitAvail[ kapi_itA ] =
                  pknobs->dmpbidinfoTable[ bid ].mpnsylAvail[ kapi_sylI ]
                  + pknobs->dmpbidinfoTable[ bid ].mpnsylAvail[ kapi_sylM ];
      pknobs->dmpbidinfoTable[ bid ].mpnitAvail[ kapi_itB ] =
                  pknobs->dmpbidinfoTable[ bid ].mpnsylAvail[ kapi_sylB ];
      pknobs->dmpbidinfoTable[ bid ].mpnitAvail[ kapi_itBl ] =
                  ( pknobs->dmpbidinfoTable[ bid ].mpnsylAvail[ kapi_sylB ] > 0 );
      pknobs->dmpbidinfoTable[ bid ].mpnitAvail[ kapi_itF ] =
                  pknobs->dmpbidinfoTable[ bid ].mpnsylAvail[ kapi_sylF ];
      pknobs->dmpbidinfoTable[ bid ].mpnitAvail[ kapi_itL ] =
                  pknobs->dmpbidinfoTable[ bid ].mpnsylAvail[ kapi_sylL ];
   }

   /* -------------- syllable information -------------- */

   pknobs->dmpsylinfoTable[ kapi_sylI ].mpnutNeeded[ kapi_utI ] = 1;
   pknobs->dmpsylinfoTable[ kapi_sylI ].mpnutNeeded[ kapi_utF ] = 0;
   pknobs->dmpsylinfoTable[ kapi_sylI ].mpnutNeeded[ kapi_utB ] = 0;
   pknobs->dmpsylinfoTable[ kapi_sylI ].mpnutNeeded[ kapi_utM ] = 0;
   pknobs->dmpsylinfoTable[ kapi_sylI ].itMajor = kapi_itA;

   pknobs->dmpsylinfoTable[ kapi_sylF ].mpnutNeeded[ kapi_utI ] = 0;
   pknobs->dmpsylinfoTable[ kapi_sylF ].mpnutNeeded[ kapi_utF ] = 1;
   pknobs->dmpsylinfoTable[ kapi_sylF ].mpnutNeeded[ kapi_utB ] = 0;
   pknobs->dmpsylinfoTable[ kapi_sylF ].mpnutNeeded[ kapi_utM ] = 0;
   pknobs->dmpsylinfoTable[ kapi_sylF ].itMajor = kapi_itF;

   pknobs->dmpsylinfoTable[ kapi_sylB ].mpnutNeeded[ kapi_utI ] = 0;
   pknobs->dmpsylinfoTable[ kapi_sylB ].mpnutNeeded[ kapi_utF ] = 0;
   pknobs->dmpsylinfoTable[ kapi_sylB ].mpnutNeeded[ kapi_utB ] = 1;
   pknobs->dmpsylinfoTable[ kapi_sylB ].mpnutNeeded[ kapi_utM ] = 0;
   pknobs->dmpsylinfoTable[ kapi_sylB ].itMajor = kapi_itB;

   pknobs->dmpsylinfoTable[ kapi_sylM ].mpnutNeeded[ kapi_utI ] = 0;
   pknobs->dmpsylinfoTable[ kapi_sylM ].mpnutNeeded[ kapi_utF ] = 0;
   pknobs->dmpsylinfoTable[ kapi_sylM ].mpnutNeeded[ kapi_utB ] = 0;
   pknobs->dmpsylinfoTable[ kapi_sylM ].mpnutNeeded[ kapi_utM ] = 1;
   pknobs->dmpsylinfoTable[ kapi_sylM ].itMajor = kapi_itA;

   pknobs->dmpsylinfoTable[ kapi_sylL ].mpnutNeeded[ kapi_utI ] = 1;
   pknobs->dmpsylinfoTable[ kapi_sylL ].mpnutNeeded[ kapi_utF ] = 1;
   pknobs->dmpsylinfoTable[ kapi_sylL ].mpnutNeeded[ kapi_utB ] = 0;
   pknobs->dmpsylinfoTable[ kapi_sylL ].mpnutNeeded[ kapi_utM ] = 0;
   pknobs->dmpsylinfoTable[ kapi_sylL ].itMajor = kapi_itL;
}

int kapi_CharCount(char c, char *pString)
{
	int count=0;
	while (*pString)
	{
		if (*pString==c)
			count++;
		pString++;
	}
	return count;
}

int kapi_ParseOpAttribute(knobs_t *pknobs, char *pAttribute, int *fuIdx, int *nOpNames, char **pOpNames)
{
	char *sName,*pWalker=pAttribute;
	int fPrimary,iOpIndex,i;
	int fNoPrimary=0;

	/* there can be only one ... */
	if (kapi_CharCount('*',pAttribute) > 1)
	{
		kapi_Error_pch1(0,0,"bad attribute, there can be only one primary operand\n\t\t \"%s\"!",pAttribute);
		*nOpNames=0;
		*fuIdx=0;
		return 0;
	}
	if (kapi_CharCount('*',pAttribute) == 0)
	{
		kapi_Warning_pch1(0,0,"No primary operand defined\n\t\t \"%s\"!",pAttribute);
		fNoPrimary=1;
	}

	/* find number of ops */
	*nOpNames = kapi_CharCount(',',pAttribute)+1;
	while ((*pWalker) && *pWalker!=':')
		pWalker++;

	/* get fu name and convert to index */
	sName=(char *)malloc( sizeof(char) * (pWalker - pAttribute +1) );
	strncpy(sName,pAttribute, pWalker - pAttribute);
	sName[pWalker - pAttribute]=0;
	*fuIdx = KAPI_EnumIndex( pknobs, "fu_t", sName );
	free(sName);
	if (*fuIdx<0)
	{
		kapi_Error_pch1(0,0,"bad attribute, or no such fu_t \"%s\"!",pAttribute);
	}
	pOpNames[0]="primary";
	if (fNoPrimary)
		iOpIndex=0;
	else
		iOpIndex=1;
	/* Now assign operand names */
	for (i=0, fPrimary=0; i < *nOpNames ; i++, fPrimary=0)
	{

		pAttribute=++pWalker;
		if (*pAttribute=='*')
		{
			pAttribute++;
			fPrimary=1;
		} 
		while ( (*pWalker)		  && 
				(*pWalker != ',') )
			pWalker++;

		/* Copy operand name */
		sName=(char *)malloc( sizeof(char) * (pWalker - pAttribute +1) );
		strncpy(sName,pAttribute, pWalker - pAttribute);
		sName[pWalker - pAttribute]=0;

		/* Primary operand is at place 0 */
		if (fPrimary)
			pOpNames[0]=sName;
		else
			pOpNames[iOpIndex++]=sName;
	}
	return fNoPrimary;
}

static void
kapi_ProcessFuOperands(knobs_t *pknobs )
{
	int nOpAttributes=0,i;
	char *pOpNames[maxSOURCES + maxDESTINATIONS];
	int nOpNames, fuIdx;
	char *pAttribute;

	/* for each "sources" attribute
		// pfu=table[fuidx]
		// fu.cntsrc=0
		// foreach source
			// fu.srcname[fu.cntsrc++]=sourceName; */
	nOpAttributes = KAPI_count4attribute( pknobs, "SOURCES" );
	if (nOpAttributes<=0)
	{
		kapi_Error(0,0,"SOURCES attribute not defined or is wrong!");
	}
	for (i=0;i<nOpAttributes;i++)
	{
		pAttribute=KAPI_attribute4index(pknobs,"SOURCES",i);
		if (kapi_ParseOpAttribute(pknobs,pAttribute, &fuIdx, &nOpNames, pOpNames))
		{ /* has no primary operand */
			pknobs->dmpfuinfoTable[fuIdx].bv32InfoBits |= (1<<kapi_fu_info_no_primary_source);
		} 
		pknobs->dmpfuinfoTable[fuIdx].cntSrc=nOpNames;
		while (--nOpNames>=0)
		{
			pknobs->dmpfuinfoTable[fuIdx].mppchSrcName[nOpNames] =
				pOpNames[nOpNames];
		}
	}
	/* for each "destinations" attribute
		// pfu=table[fuidx]
		// fu.cntdst=0
		// foreach dest
			// fu.srcname[fu.cntdst++]=sourceName; */
	nOpAttributes = KAPI_count4attribute( pknobs, "DESTINATIONS" );
	if (nOpAttributes<=0)
	{
		kapi_Error(0,0,"DESTINATIONS attribute not defined or is wrong!");
	}
	for (i=0;i<nOpAttributes;i++)
	{
		pAttribute=KAPI_attribute4index(pknobs,"DESTINATIONS",i);
		if (kapi_ParseOpAttribute(pknobs,pAttribute, &fuIdx, &nOpNames, pOpNames))
		{ /* has no primary operand */
			pknobs->dmpfuinfoTable[fuIdx].bv32InfoBits |= (1<<kapi_fu_info_no_primary_destination);
		} 
		pknobs->dmpfuinfoTable[fuIdx].cntDest=nOpNames;
		while (--nOpNames>=0)
		{
			pknobs->dmpfuinfoTable[fuIdx].mppchDestName[nOpNames] =
				pOpNames[nOpNames];
		}
	}
}

static void
kapi_ProcessFuInfoBits( knobs_t *pknobs, kapi_fu_t fu )
{
	bv_t *pbvTmp;
	bv32_t bv;

	pbvTmp = KAPI_GetBvVariable(pknobs,"fuInfoBits",fu);
	if (pbvTmp != NULL)
	{
		bv=pbvTmp->pint32Data[0];
		free(pbvTmp);
	} else
		bv = 0;

	pknobs->dmpfuinfoTable[fu].bv32InfoBits = bv;
}



static void
kapi_ProcessFu( knobs_t *pknobs )
{
	int i, j, nfu, clr; 

	for ( clr=0; clr<pknobs->nclr; clr++ ) {
	  clr_t *pclr;
	  char mpchCluster[ 40 ];
	  kapi_ut_t ut;
	  int cuport;

	  sprintf( mpchCluster, "cluster%dCportMask", clr );
	  pclr = &(pknobs->mpclrTable[ clr ]);

	  if ( KAPI_VariableCardinality( pknobs, mpchCluster ) == -1 ) {
		 kapi_Error_pch1( 0, 0, "%s must be defined", mpchCluster );
		 continue;
	  } 
	  for ( ut=kapi_utFIRST; ut<=kapi_utLAST; ut++ ) {
		 for ( cuport=0; cuport<pclr->mpncutport[ ut ]; cuport++ ) { 
			char mpchTmp[ 20 ];
			int idx;
			bv_t *pbv;

			sprintf( mpchTmp, "%%cport%c%d", kapi_ut2ch( ut ), cuport );

			idx = KAPI_ArrayIndex( pknobs, mpchCluster, mpchTmp );
			if ( idx != -1 ) {
			   pbv     = KAPI_GetBvVariable( pknobs, mpchCluster, idx );
			   pclr->dmppcportinfoTable[ ut ][ cuport ].bvfuAllowed = *pbv;
			} else {
			   kapi_Error_pch2( 0, 0, "Index %s for %s not defined", &(mpchTmp[1]), mpchCluster );
			}
		 }
	  }
	}

	for ( i=0; i<pknobs->nfuinfoTable; i++ ) {

	  pknobs->dmpfuinfoTable[i].iCoreLatency[ 0 ] =
			   KAPI_GetIntegerVariable( pknobs, "fuLatency", i );

	  if ( KAPI_error_attribute ) {
		 kapi_Error_i1( kapi_cLine, 0,  
				  "Retrieving value of fuLatency[ %d ]", i );
	  }
	}

	nfu = KAPI_EnumCardinality( pknobs, "fu_t");
	if ( KAPI_error_attribute ) {
	  kapi_Error( kapi_cLine, 0, "'fu_t' not defined or incorrect" );
	  return;
	}

	/* get primary latency information and initial bypasses */
	pknobs->nfuinfoTable = nfu;
	pknobs->dmpfuinfoTable = (fuinfo_t *)malloc( sizeof( fuinfo_t ) * nfu );

	for ( i=0; i<nfu; i++ ) {
		pknobs->dmpfuinfoTable[i].fu = i;
		pknobs->dmpfuinfoTable[i].pchName =
			kapi_pchCopy( KAPI_EnumName( pknobs, i, "fu_t" ) );
		pknobs->dmpfuinfoTable[i].cntDest = 1;
		pknobs->dmpfuinfoTable[i].cntSrc = 2;
		pknobs->dmpfuinfoTable[i].mppchDestName[0] = "primary";
		pknobs->dmpfuinfoTable[i].mppchSrcName[0] = "primary";
		pknobs->dmpfuinfoTable[i].mppchSrcName[1] = "predicate";
		for ( j=0;j<maxDESTINATIONS;j++ ) {
			pknobs->dmpfuinfoTable[i].iCoreLatency[j] = -1;
		}
		kapi_ProcessFuInfoBits(pknobs,i);
	}
	/* process special operands */
	kapi_ProcessFuOperands(pknobs);

}

static void
kapi_ProcessCoreLatency( knobs_t *pknobs )
{
   int i, nlatency;

   for ( i=0; i<pknobs->nfuinfoTable; i++ ) {
		int iLatency;

		iLatency=KAPI_GetIntegerVariable( pknobs, "fuLatency", i );

	   if (pknobs->dmpfuinfoTable[i].bv32InfoBits & (1<<kapi_fu_info_no_primary_destination))
	   {
		   if (iLatency != -1)
		   {
			   kapi_Error_pch1(0,0,"fuLatency invalid since fuClass %s has no primary destination operand",
				   pknobs->dmpfuinfoTable[i].pchName);
		   }
	   } else
	   {
			pknobs->dmpfuinfoTable[i].iCoreLatency[ 0 ] = iLatency;
	   }
               

      if ( KAPI_error_attribute ) {
         kapi_Error_i1( kapi_cLine, 0,  
                  "Retrieving value of fuLatency[ %d ]", i );
      }
   }

   nlatency = KAPI_count4attribute( pknobs, "LATENCY" );

   for ( i=0; i<nlatency; i++ ) {
      char *pch, *pchFcSrc, *pchOpp;
      char mpchTmp[ 400 ];
      int iLatency, opp;
      kapi_fu_t fu;


      pch = KAPI_attribute4index( pknobs, "LATENCY", i );

      (void)strncpy( (char *)mpchTmp, pch, 399 );

      ParseLatency( mpchTmp, &pchFcSrc, &pchOpp, &iLatency );

      fu = KAPI_EnumIndex( pknobs, "fu_t", pchFcSrc );
      if ( -1 == fu ) {
         /* ignore poorly formed entry */
         continue;
      } else {
		 if ( pknobs->dmpfudirTable[ fu ] == pknobs->fudirSource ) {
            opp = KAPI_oppGetSource( pknobs, fu, pchOpp );
	     } else {
            opp = KAPI_oppGetDest( pknobs, fu, pchOpp );
	     }
         if ( -1 == opp || pchOpp == NULL ) {
            /* ignore poorly formed entry */
			 kapi_Error_pch1(0,0,"Bad operand in attribute: %s",pch);
            continue;
         } else {
            pknobs->dmpfuinfoTable[ fu ].iCoreLatency[ opp ] = iLatency;
         }
      }
   }
}

static void
ParseClusterDistance( char *pch, char **ppchClrSrc, char **ppchClrDest, 
                      char **ppchValue )
{
   char *pchSrc, *pchValue;

   /* ---- process RHS of string ( latency value ) ---- */

   pchValue = (char *)strchr( pch, '=' );
   if ( pchValue == NULL ) {   /* error */
      kapi_Error_pch1( -1, 0, 
               "Malformed cluster distance attribute '%s' -- '=' required", 
               pch );
      return;
   }
   *pchValue = '\0';   /* cut string in half */

   /* point to first character after '=' */
   pchValue = pchValue + 1;

   *ppchValue = pchValue;


   /* ---- now process LHS of string ---- */
   
   pchSrc = pch;

   /* initialize return values */
   *ppchClrSrc   = NULL;
   *ppchClrSrc  = NULL;

   /* parse the source operand string */
   *ppchClrSrc   = strtok( pchSrc, " :" );
   if ( *ppchClrSrc ) {
      *ppchClrDest = strtok( NULL, " :" );
   }
}

static int
fOkProcessClusterDistance( knobs_t *pknobs, char *pchClrSrc,  
                         char *pchClrDest, char *pchValue,
                         int *piClrSrc, int *piClrDest, int *pivalue )
{
   int iclr;


   if ( strspn( pchValue, "0123456789-" ) != strlen( pchValue ) ) {
      kapi_Error_pch1( -1, 0,
                       "Badly formed cluster distance:  value '%s' not valid",
                       pchNULL_PRINT(pchValue) );
      return( 0 );
   }

   *pivalue = atoi( pchValue );


   iclr = clusterInterpretpchclr( pchClrSrc );
   if ( iclr < 0 ) {
      kapi_Error_pch1( -1, 0,
                       "Badly formed source cluster name:  cluster '%s' not valid",
                       pchNULL_PRINT(pchClrSrc) );
      return( 0 );
   }
   *piClrSrc = iclr;


   iclr = clusterInterpretpchclr( pchClrDest );
   if ( iclr < 0 ) {
      kapi_Error_pch1( -1, 0,
                       "Badly formed destination cluster:  cluster '%s' not valid",
                       pchNULL_PRINT(pchClrDest) );
      return( 0 );
   }
   *piClrDest = iclr;


   if ( *piClrSrc >= pknobs->nclr ) {
      /* silently ignore value */
      return( 0 );
   }

   if ( *piClrDest >= pknobs->nclr ) {
      /* silently ignore value */
      return( 0 );
   }


   return( 1 );
}

static void
kapi_ProcessClusterDistance( knobs_t *pknobs ) 
{
   int i, j, ncdist;

   /* initialize intercluster distances to zero */
   for ( i=0; i<maxCLR; i++ ) {
      for ( j=0; j<maxCLR; j++ ) {
         pknobs->mpclrTable[ i ].distClr[ j ] = 0;
      }
   }

   ncdist = KAPI_count4attribute( pknobs, "CLUSTERDISTANCE" );
   for ( i=0; i<ncdist; i++ ) {
      char *pch, *pchClrSrc, *pchClrDest, *pchValue;
      char mpchTmp[ 400 ];
      int ivalue, fOk, iclrSrc, iclrDest;


      pch = KAPI_attribute4index( pknobs, "CLUSTERDISTANCE", i );

      (void)strncpy( (char *)mpchTmp, pch, 399 );

      ParseClusterDistance( mpchTmp, &pchClrSrc, &pchClrDest, &pchValue );

      fOk = fOkProcessClusterDistance( pknobs, pchClrSrc, pchClrDest, pchValue,
                                      &iclrSrc, &iclrDest, &ivalue );
      if ( fOk ) {
         pknobs->mpclrTable[ iclrSrc ].distClr[ iclrDest ] = ivalue;
      }
   }
}

static void
ParseLatency( char *pch, char **ppchFcSrc, char **ppchOppSrc, int *piLatency )
{
   char *pchSrc, *pchVal, *pchLatency;

   /* ---- process RHS of string ( latency value ) ---- */

   pchVal = (char *)strchr( pch, '=' );
   if ( pchVal == NULL ) {   /* error */
      kapi_Error_pch1( -1, 0, "Malformed latency attribute '%s' -- '=' required", pch );
      return;
   }
   *pchVal = '\0';   /* cut string in half */

   /* point to first character after '=' */
   pchLatency = pchVal + 1;

   if ( strspn( pchLatency, "0123456789-" ) != strlen( pchLatency ) ) {
      kapi_Error_pch1( -1, 0, "Badly formed bypass:  latency value '%s' not valid", 
                              pchNULL_PRINT(pchLatency) );
      return;
   }
   *piLatency = atoi( pchLatency );


   /* ---- now process LHS of string ---- */
   
   pchSrc = pch;

   /* initialize return values */
   *ppchFcSrc   = NULL;
   *ppchOppSrc  = NULL;

   /* parse the source operand string */
   *ppchFcSrc   = strtok( pchSrc, " /" );
   if ( *ppchFcSrc ) {
      *ppchOppSrc = strtok( NULL, " /" );
   }
}

static void
ParseIntraCluster( char *pch_in, char **ppchClr, 
             char **ppchFcSrc, char **ppchOppSrc, char **ppchCportSrc,
             char **ppchFcDest, char **ppchOppDest, char **ppchCportDest,
                                       char **ppchValue )
{
   char *pchClr, *pchSrc, *pchDest, *pchVal;
   kapi_cutport_t cutport;

   /* ---- process RHS of string ( latency value ) ---- */

   pchVal = (char *)strchr( pch_in, '=' );
   if ( pchVal == NULL ) {   /* error */
      kapi_Error_pch1( -1, 0, "Malformed intracluster bypass '%s' -- '=' required", pch_in );
      return;
   }
   *pchVal = '\0';   /* cut string in half */

   /* point to first character after '=' */
   *ppchValue = pchVal + 1;


   /* ---- now process LHS of string ---- */
   
   pchDest = (char *)strchr( pch_in, ':' );
   if ( pchDest == NULL ) {   /* error */
      kapi_Error_pch1( -1, 0, "Malformed intracluster bypass '%s' -- ':' required", pch_in );
      return;
   }
   *pchDest = '\0';
   pchDest = pchDest + 1;  /* same as for '=' */

   /* see if there is a cluster spec */
   pchSrc = strchr( pch_in, '\\' );
   if ( pchSrc == NULL ) {
      pchSrc = pch_in;
      pchClr = NULL;
   } else {
      *pchSrc = '\0';
      pchSrc += 1;
      pchClr = pch_in;
   }

   /* initialize return values */
   if ( pchClr ) {
      *ppchClr   = pchClr;
   } else {
      *ppchClr   = NULL;
   }

   *ppchFcSrc   = NULL;
   *ppchOppSrc  = NULL;
   *ppchCportSrc = NULL;

   *ppchFcDest   = NULL;
   *ppchOppDest  = NULL;
   *ppchCportDest = NULL;


   /* parse the source operand string */
   *ppchFcSrc = strtok( pchSrc, " /" );
   if ( *ppchFcSrc ) {
      *ppchOppSrc = strtok( NULL, " /" );
      if ( *ppchOppSrc ) {
		 if (-1!=utInterpretCportName( *ppchOppSrc, &cutport )) /* no operand defined but cport was defined */
		 {
			*ppchCportSrc=*ppchOppSrc;
			*ppchOppSrc=NULL;
		 } else
			*ppchCportSrc = strtok( NULL, " /" );
         if ( *ppchCportSrc ) {
            if ( strtok( NULL, " /" ) ) {
               kapi_Error_pch1( -1, 0, 
                     "Malformed intracluster bypass too many '/' segments in LHS (%s)", 
                     pch_in );
               return;
            }
         }
      }
   }

   /* parse the destination operand string */
   *ppchFcDest = strtok( pchDest, " /" );
   if ( *ppchFcDest ) {
      *ppchOppDest = strtok( NULL, " /" );
      if ( *ppchOppDest ) {
		 if (-1!=utInterpretCportName( *ppchOppDest, &cutport )) /* no operand defined but cport was defined */
		 {
			*ppchCportDest=*ppchOppDest;
			*ppchOppDest=NULL;
		 } else
			*ppchCportDest = strtok( NULL, " /" );
         if ( *ppchCportDest ) {
            if ( strtok( NULL, " /" ) ) {
               kapi_Error_pch1( -1, 0, 
                     "Malformed intracluster bypass too many '/' segments in LHS (%s)", 
                     pch_in );
               return;
            }
         }
      }
   }

}

static void
ParseInterCluster( char *pch_in, char **ppchClrSrc, char **ppchFcSrc, 
                                  char **ppchOppSrc, char **ppchCportSrc,
                                  char **ppchClrDest, char **ppchFcDest, 
                                  char **ppchOppDest, char **ppchCportDest,
                                       char **ppchValue )
{
   char *pchSrc, *pchDest, *pchVal;

   /* ---- process RHS of string ( latency value ) ---- */

   pchVal = (char *)strchr( pch_in, '=' );
   if ( pchVal == NULL ) {   /* error */
      kapi_Error_pch1( -1, 0, "Malformed intercluster bypass '%s' -- '=' required", pch_in );
      return;
   }
   *pchVal = '\0';   /* cut string in half */

   /* point to first character after '=' */
   *ppchValue = pchVal + 1;


   /* ---- now process LHS of string ---- */
   
   pchDest = (char *)strchr( pch_in, ':' );
   if ( pchDest == NULL ) {   /* error */
      kapi_Error_pch1( -1, 0, "Malformed intercluster bypass '%s' -- ':' required", pch_in );
      return;
   }
   *pchDest = '\0';
   pchDest = pchDest + 1;  /* same as for '=' */
   pchSrc = pch_in;

   /* initialize return values */
   *ppchClrSrc   = NULL;
   *ppchFcSrc   = NULL;
   *ppchOppSrc  = NULL;
   *ppchCportSrc = NULL;

   *ppchClrDest   = NULL;
   *ppchFcDest   = NULL;
   *ppchOppDest  = NULL;
   *ppchCportDest = NULL;


   /* parse the source operand string */
   *ppchClrSrc   = strtok( pchSrc, " /" );
   if ( *ppchClrSrc ) {
      *ppchFcSrc = strtok( NULL, " /" );
      if ( *ppchFcSrc ) {
         *ppchOppSrc = strtok( NULL, " /" );
         if ( *ppchOppSrc ) {
            *ppchCportSrc = strtok( NULL, " /" );
         }
      }
   }

   /* parse the destination operand string */
   *ppchClrDest   = strtok( pchDest, " /" );
   if ( *ppchClrDest ) {
      *ppchFcDest = strtok( NULL, " /" );
      if ( *ppchFcDest ) {
         *ppchOppDest = strtok( NULL, " /" );
         if ( *ppchOppDest ) {
            *ppchCportDest = strtok( NULL, " /" );
         }
      }
   }

}

static int
fOkProcessIntraCluster( knobs_t *pknobs, aby_t *paby, 
                           char *pchClr, 
                   char *pchFcSrc, char *pchOppSrc, char *pchCportSrc,
                   char *pchFcDest, char *pchOppDest, char *pchCportDest,
                              char *pchValue )
{
   kapi_ut_t utDest, utSrc;

   /* lookup value - mandatory */
   if ( strspn( pchValue, "0123456789-" ) != strlen( pchValue ) ) {
      kapi_Error_pch1( -1, 0, "Badly formed intracluster bypass:  value '%s' not valid", 
                              pchNULL_PRINT(pchValue) );
      return( 0 );
   }
   paby->iValue = atoi( pchValue );
   

   /* lookup clusters - mandatory */
   if ( pchClr ) {
      paby->cluster = clusterInterpretpchclr( pchClr );
   } else { 
      paby->cluster = -1;
   }


   /* ignore, but don't issue error */
   if ( paby->cluster >= pknobs->nclr ) {
      return( 0 );
   }


   /* lookup functional unit classes - mandatory */

   paby->fuSrc = KAPI_EnumIndex( pknobs, "fu_t", pchFcSrc );
   paby->fuDest = KAPI_EnumIndex( pknobs, "fu_t", pchFcDest );
   if ( -1 == paby->fuSrc || paby->fuDest == -1 ) {
      kapi_Warning_pch2( 0, 0, "INTRACLUSTER dropped due to illegal fu %s -> %s\n", pchFcSrc, pchFcDest );
      return( 0 );
   }


   /* lookup operands - optionally present */

   paby->oppSrc = KAPI_oppGetDest( pknobs, paby->fuSrc, pchOppSrc );
   if ( -1 == paby->oppSrc ) {
	  kapi_Error_pch2( -1, 0, 
			  "Invalid destination operand (%s) for source instruction (%s)", 
			  pchOppSrc, KAPI_EnumName( pknobs, paby->fuSrc, "fu_t" ) );
	  return( 0 );
   }

   /* if operand is unspecified, port must also be unspecified */
   if ( pchOppSrc == NULL && pchCportSrc != NULL ) {
      kapi_Warning_pch1( -1, 0, 
            "INTRACLUSTER defined source port w/o source operand '%s'", 
            pchCportSrc );
	  /* return (0); if treated as error!!1 */
   }

   paby->oppDest = KAPI_oppGetSource( pknobs, paby->fuDest, pchOppDest );
   if ( -1 == paby->oppDest ) {
      kapi_Error_pch2( -1, 0, 
              "Invalid source operand (%s) for source instruction (%s)", 
              pchOppDest, KAPI_EnumName( pknobs, paby->fuDest, "fu_t" ) );
      return( 0 );
   }

   /* if operand is unspecified, port must also be unspecified */
   if ( pchOppDest == NULL && pchCportDest != NULL ) {
      kapi_Warning_pch1( -1, 0, 
           "INTRACLUSTER defined destination port w/o source operand '%s'", 
           pchCportDest );
	  /* return (0); if treated as error!!1 */
   }


   /* lookup ports - optionally present */
   if ( pchCportSrc != NULL ) {
      int cutport;

      utSrc = utInterpretCportName( pchCportSrc, &cutport );
      if ( -1 == utSrc ) {
         kapi_Error_pch1( -1, 0, 
                 "Invalid source cport '%s'", pchCportSrc );
         return( 0 );
      } else {
         paby->cutportSrc = cutport;
         paby->utSrc = utSrc;
      }
   } else {
      paby->cutportSrc = -1;
      paby->utSrc = -1;
   }

   if ( pchCportDest != NULL ) {
      int cutport;

      utDest = utInterpretCportName( pchCportDest, &cutport );
      if ( -1 == utDest ) {
         kapi_Error_pch1( -1, 0, 
                 "Invalid destination cport '%s'", pchCportDest );
         return( 0 );
      } else {
         paby->cutportDest = cutport;
         paby->utDest = utDest;
      }
   } else {
      paby->cutportDest = -1;
      paby->utDest = -1;
   }
   
   return( 1 );
}

static int
fOkProcessInterCluster( knobs_t *pknobs, eby_t *peby, 
                           char *pchClrSrc, char *pchFcSrc, 
                           char *pchOppSrc, char *pchCportSrc,
                           char *pchClrDest, char *pchFcDest, 
                           char *pchOppDest, char *pchCportDest,
                              char *pchValue )
{
   kapi_ut_t utDest, utSrc;

   /* lookup value - mandatory */
   if ( strspn( pchValue, "0123456789-" ) != strlen( pchValue ) ) {
      kapi_Error_pch1( -1, 0, "Badly formed intercluster bypass:  value '%s' not valid", 
                              pchNULL_PRINT(pchValue) );
      return 0;
   }
   peby->iValue = atoi( pchValue );
   

   /* lookup clusters - mandatory */
   peby->clusterSrc = clusterInterpretpchclr( pchClrSrc );
   peby->clusterDest = clusterInterpretpchclr( pchClrDest );

   if ( peby->clusterSrc < 0 || peby->clusterDest < 0 ) {
      kapi_Error( -1, 0, 
              "INTERCLUSTER source and destination instructions must have a cluster specified" );
      return( 0 );
   }

   if ( peby->clusterSrc == peby->clusterDest ) {
      kapi_Error( -1, 0, 
              "INTERCLUSTER source and destination instructions must have different clusters specified" );
      return( 0 );
   }

   /* ignore, but don't issue error */
   if ( peby->clusterSrc >= pknobs->nclr || peby->clusterDest >= pknobs->nclr ) {
      return( 0 );
   }


   /* lookup functional unit classes - mandatory */

   peby->fuSrc = KAPI_EnumIndex( pknobs, "fu_t", pchFcSrc );
   peby->fuDest = KAPI_EnumIndex( pknobs, "fu_t", pchFcDest );
   if ( -1 == peby->fuSrc || peby->fuDest == -1 ) {
      kapi_Warning_pch2( 0, 0, "INTERCLUSTER dropped due to illegal fu %s -> %s\n", pchFcSrc, pchFcDest );
      return( 0 );
   }


   /* lookup operands - optionally present */

   peby->oppSrc = KAPI_oppGetDest( pknobs, peby->fuSrc, pchOppSrc );
   if ( -1 == peby->oppSrc ) {
      kapi_Error_pch2( -1, 0, 
              "Invalid destination operand (%s) for source instruction (%s)", 
              pchOppSrc, KAPI_EnumName( pknobs, peby->fuSrc, "fu_t" ) );
      return( 0 );
   }

   /* if operand is unspecified, port must also be unspecified */
   if ( pchOppSrc == NULL && pchCportSrc != NULL ) {
      kapi_Error_pch1( -1, 0, 
            "INTERCLUSTER cannot specify source port w/o source operand '%s'", 
            pchCportSrc );
      return( 0 );
   }

   peby->oppDest = KAPI_oppGetSource( pknobs, peby->fuDest, pchOppDest );
   if ( -1 == peby->oppDest ) {
      kapi_Error_pch2( -1, 0, 
              "Invalid source register (%s) for source instruction (%s)", 
              pchOppDest, KAPI_EnumName( pknobs, peby->fuDest, "fu_t" ) );
      return( 0 );
   }

   /* if operand is unspecified, port must also be unspecified */
   if ( pchOppDest == NULL && pchCportDest != NULL ) {
      kapi_Error_pch1( -1, 0, 
           "INTERCLUSTER cannot specify destination port w/o source operand '%s'", 
           pchCportDest );
      return( 0 );
   }


   /* lookup ports - optionally present */
   if ( pchCportSrc != NULL ) {
      int cutportSrc;

      utSrc = utInterpretCportName( pchCportSrc, &cutportSrc );
      if ( -1 == utSrc ) {
         kapi_Error_pch1( -1, 0, 
                 "Invalid source port '%s'", pchCportSrc );
         return( 0 );
      } else {
         peby->cutportSrc = cutportSrc;
         peby->utSrc = utSrc;
      }
   } else {
      peby->cutportSrc = -1;
      peby->utSrc = -1;
   }

   if ( pchCportDest != NULL ) {
      int cutportDest;

      utDest = utInterpretCportName( pchCportDest, &cutportDest );
      if ( -1 == utDest ) {
         kapi_Error_pch1( -1, 0, 
                 "Invalid destination port '%s'", pchCportDest );
         return( 0 );
      } else {
         peby->cutportDest = cutportDest;
         peby->utDest = utDest;
      }
   } else {
      peby->cutportDest = -1;
      peby->utDest = -1;
   }
   
   return( 1 );
}

static void
kapi_ProcessInterclusterLatency( knobs_t *pknobs ) 
{
   int maxeby, idx, i;

   maxeby = KAPI_count4attribute( pknobs, "INTERCLUSTER" );

   if ( maxeby > 0 ) {
      pknobs->dmpebyInterTable = 
                (eby_t *)malloc( sizeof(eby_t)*maxeby );
   } else {
      pknobs->dmpebyInterTable = NULL;
      pknobs->nRawInter = 0;
      return;
   }

   idx = 0;
   for ( i=0; i<maxeby; i++ ) {
      char *pch;
      char mpchTmp[ 400 ], *pchLatency;
      char *pchClrSrc, *pchFcSrc, *pchOppSrc, *pchCportSrc,
           *pchClrDest, *pchFcDest, *pchOppDest, *pchCportDest;
      eby_t ebyTmp;
      int fOk;


      pch = KAPI_attribute4index( pknobs, "INTERCLUSTER", i );

      (void)strncpy( (char *)mpchTmp, pch, 399 );

      ParseInterCluster( mpchTmp, &pchClrSrc, &pchFcSrc, 
                                  &pchOppSrc, &pchCportSrc,
                                  &pchClrDest, &pchFcDest, 
                                  &pchOppDest, &pchCportDest,
                                       &pchLatency );

      fOk = fOkProcessInterCluster( pknobs, &ebyTmp, 
                           pchClrSrc, pchFcSrc, 
                           pchOppSrc, pchCportSrc,
                           pchClrDest, pchFcDest, 
                           pchOppDest, pchCportDest,
                              pchLatency );

      if ( fOk ) {
         pknobs->dmpebyInterTable[ idx ] = ebyTmp;
         idx++; 
      } else {
         kapi_Warning_pch1( 0, 0, "INTERCLUSTER dropped '%s'\n", pch );
      }
   }

   pknobs->nRawInter = idx;
}

static void
kapi_ProcessIntraclusterLatency( knobs_t *pknobs ) 
{
   int maxaby, idx, i;

   maxaby = KAPI_count4attribute( pknobs, "INTRACLUSTER" );

   if ( maxaby > 0 ) {
      pknobs->dmpabyIntraTable = 
                (aby_t *)malloc( sizeof(aby_t)*maxaby );
   } else {
      pknobs->dmpabyIntraTable = NULL;
      pknobs->nRawIntra = 0;
      return;
   }

   idx = 0;
   for ( i=0; i<maxaby; i++ ) {
      char *pch;
      char mpchTmp[ 400 ], *pchLatency;
      char *pchClr, *pchFcSrc, *pchOppSrc, *pchCportSrc,
           *pchFcDest, *pchOppDest, *pchCportDest;
      aby_t abyTmp;
      int fOk;


	  if (i==0x154)
		  pch=NULL;
      pch = KAPI_attribute4index( pknobs, "INTRACLUSTER", i );

      (void)strncpy( (char *)mpchTmp, pch, 399 );

      ParseIntraCluster( mpchTmp, &pchClr, 
                          &pchFcSrc, &pchOppSrc, &pchCportSrc,
                          &pchFcDest, &pchOppDest, &pchCportDest,
                                       &pchLatency );

      fOk = fOkProcessIntraCluster( pknobs, &abyTmp, pchClr, 
                         pchFcSrc, pchOppSrc, pchCportSrc,
                         pchFcDest, pchOppDest, pchCportDest,
                              pchLatency );


      if ( fOk ) {
         pknobs->dmpabyIntraTable[ idx ] = abyTmp;
         pknobs->dmpabyIntraTable[ idx ].pchEntry = pch;
         idx++; 
      } else {
         pknobs->dmpabyIntraTable[ idx ].pchEntry = NULL;
         kapi_Warning_pch1( 0, 0, "INTRACLUSTER dropped '%s'\n", pch );
      }
   }

   pknobs->nRawIntra = idx;
}


static int 
kapi_GetOpIndex(knobs_t *pknobs,char *pchOp,kapi_fu_t fu,int fRole)
{
	int iIndex;
	if (fRole==0) /* dest */
	{
		iIndex=KAPI_oppGetDest(pknobs,fu,pchOp);
	}
	if (fRole==1) /* src */
	{
		iIndex=KAPI_oppGetSource(pknobs,fu,pchOp);
	}
	if (iIndex==-1)
	kapi_Error_pch3(0,0,
		"Bad %s operand type (for fu %s), %s",
		(fRole) ? "source" : "destination",
		KAPI_fu2fuName(pknobs,fu,0),pchOp);
	return 0;
}

static void 
kapi_ParseSingleOp(knobs_t *pknobs, kapi_fu_t fu, char *pch, operand_match_t *pOp)
{
	char *pWalker=pch;
	int fNewOp=0;
	int fRole=99;
	char *pchOp=NULL;

	pOp->iType=type_single;
	pOp->pIndexes[0].pOperand=-1;
	pOp->pIndexes[1].pOperand=-1;
	while (*pWalker)
	{
		switch (*pWalker)
		{
		case '+':
			fRole=0; /* dest */
			fNewOp=1;
			*pWalker=0;
			break;
		case '-':
		case '?':
			fRole=1; /* src */
			fNewOp=1;
			*pWalker=0;
			break;
		case '*': /* ignore primary for the time being */
			pchOp++;
			break;
		default:
			break;
		}
		pWalker++;
		if (fNewOp && pchOp!=NULL)
		{
			pOp->pIndexes[fRole].pOperand=
				kapi_GetOpIndex(pknobs,pchOp,fu,fRole);
		}
		if (fNewOp)
		{
			fNewOp=0;
			pchOp=pWalker;
		}
	} 
	if (pchOp)
	{
		pOp->pIndexes[fRole].pOperand=
			kapi_GetOpIndex(pknobs,pchOp,fu,fRole);
	}
}

static void 
kapi_ParseInstOp(knobs_t *pknobs, kapi_fu_t fu,char *pch, operand_match_t *pOperandList)
{
	operand_match_t *pOpWalker;
	int i,nOperands=kapi_CharCount('/',pch)+1;
	char *pchOp;

	if (0==strcmp(pch,"none"))
		return;

	if (1==nOperands)
	{
		kapi_ParseSingleOp(pknobs,fu,pch,pOperandList);
	} else
	{
		pOperandList->iType=type_multiple;
		pOperandList->pIndexes[0].pOperandList = 
			(operand_match_t *) malloc(nOperands * sizeof(operand_match_t));
		pOpWalker=pOperandList->pIndexes[0].pOperandList;
		pchOp = strtok(pch,"/");
		for (i=0 ; (i<nOperands) && pchOp ; i++)
		{
			kapi_ParseSingleOp(pknobs,fu,pchOp,&pOpWalker[i]);
			pchOp=strtok(NULL,"/");
		}
	}
}

static void 
kapi_ParseInstOps(knobs_t *pknobs, kapi_fu_t fu,char *pch, 
				  operand_match_t **pExplicitOps, operand_match_t **pImplicitOps)
{
	operand_match_t *pOps;
	int nOps,iIndex;
	char *pchExplicits,*pchImplicits, *pWalker;
	char buffer[1024];

	pchExplicits = strchr(pch,':')+1;
	pWalker = strchr(pchExplicits,':');
	*pWalker++=0;
	pchImplicits=pWalker;

	/* handle explicits */
	nOps = kapi_CharCount(',',pchExplicits) + 1;
	pOps = (operand_match_t *)malloc(nOps * sizeof(operand_match_t));

	strncpy(buffer,pchExplicits,1024);
	pWalker=strtok(buffer,",");
	iIndex=0;
	while (pWalker)
	{
		kapi_ParseInstOp(pknobs,fu,pWalker,&pOps[iIndex++]);
		pWalker=strtok(NULL,",");
	}

	*pExplicitOps=pOps;

	/* handle Implicits */
	nOps = kapi_CharCount(',',pchImplicits) + 1;
	pOps = (operand_match_t *)malloc(nOps *  sizeof(operand_match_t));

	strncpy(buffer,pchImplicits,1024);
	pWalker=strtok(buffer,",");
	iIndex=0;
	while (pWalker)
	{
		kapi_ParseInstOp(pknobs,fu,pWalker,&pOps[iIndex++]);
		pWalker=strtok(NULL,",");
	}

	*pImplicitOps=pOps;

}

static void
DecodeInstructionInformation( knobs_t *pknobs, char *pch )
{
	char mpch[ 200 ];
	char *pchNumber, *pchUniqueName, *pchMnemonic, *pchit, *pchfu;
	kapi_it_t it;
	kapi_fu_t fu;
	int idx;
	operand_match_t *pExplicitOps=0,*pImplicitOps=0;
	char *pWalker;


	strncpy( mpch, pch, 199 );
	pchNumber = strtok( mpch, "," );
	pchUniqueName = strtok( NULL, "," );
	pchMnemonic = strtok( NULL, "," );
	pchit = strtok( NULL, "," );
	pchfu = strtok( NULL, "," );
	/* if instruction has operands information, fix fu string */
	if (pWalker=strchr(pchfu,':'))
	   *pWalker=0;
	idx = atoi( pchNumber );
	if (idx >= pknobs->ninstTable)
	{
	   char buffer[200]="";
	   sprintf(buffer,
		   "There are only %d instruction attributes,\n"
		   "\tbut there is an instruction with index %d!\n"
		   "\tAn instruction or more are missing!",pknobs->ninstTable,idx);
	   kapi_Error(kapi_cLine, 0, buffer);
	   return;
	}
	/* if we are overriding a former entry, need to update number of instructions */
	if (pknobs->dmpinstTable[ idx ].fu!=-1)
	{
	   pknobs->ninstTable--;
	   kapi_Warning_pch1( kapi_cLine,1, "Instruction %s override",pchNumber);
	}

	it = KAPI_EnumIndex( pknobs, "it_t", pchit );
	fu = KAPI_EnumIndex( pknobs, "fu_t", pchfu );

	if ( it == -1 ) {
		kapi_Error_pch1( kapi_cLine, 0, "%s not a valid instruction type", pchit );
	}
	if ( fu == -1 ) {
		kapi_Error_pch1( kapi_cLine, 0, "%s not a valid function class", pchfu );
	}

	/* if operand information exists */
	if (pWalker)
	{
		kapi_ParseInstOps(pknobs, fu , pch, 
						&pExplicitOps, &pImplicitOps);
	}

	pknobs->dmpinstTable[ idx ].fu = fu;
	pknobs->dmpinstTable[ idx ].it = it;
	pknobs->dmpinstTable[ idx ].pchfu = kapi_pchCopy( pchfu );
	pknobs->dmpinstTable[ idx ].pchit = kapi_pchCopy( pchit );
	pknobs->dmpinstTable[ idx ].pchMnemonic = kapi_pchCopy( pchMnemonic );
	pknobs->dmpinstTable[ idx ].pchUniqueName = kapi_pchCopy( pchUniqueName );

	pknobs->dmpinstTable[ idx ].pExplicitOps=pExplicitOps;
	pknobs->dmpinstTable[ idx ].pImplicitOps=pImplicitOps;

}

static void
kapi_ProcessInstructionData( knobs_t *pknobs )
{
   int niid, i;
   char *pch;

   niid = KAPI_count4attribute( pknobs, "instruction" );
   pknobs->dmpinstTable = NULL;
   pknobs->ninstTable = 0; 
   if ( KAPI_error_attribute ) {
      kapi_Error( kapi_cLine, 0, "Attribute 'instruction' not defined or incorrect" );
      return;
   } else if ( niid <= 10 ) {
      kapi_Error( kapi_cLine, 0, 
                  "Attribute 'instruction' not defined or incorrect" );
      return;
   } else {
      pknobs->ninstTable = niid; 
   }

   pknobs->dmpinstTable = (inst_t *)malloc( sizeof(inst_t) * niid );
   /* init instruction table */ 
   for ( i=0; i<niid; i++ ) 
	pknobs->dmpinstTable[i].fu=-1;
   assert( pknobs->dmpinstTable );
   for ( i=0; i<niid; i++ ) {
      pch = KAPI_attribute4index( pknobs, "instruction", i );
      DecodeInstructionInformation( pknobs, pch );
   }
}


kapi_cport_t
cport4utcport( knobs_t *pknobs, kapi_cluster_t cluster, kapi_ut_t ut, int cportut )
{
   return( pknobs->mpclrTable[ cluster ].dmppcportinfoTable[ ut ][ cportut ].cport );
}

static void kapi_ProcessCacheData( knobs_t *pknobs)
{
	int i,iType,iLevel,iPort;
	int iEnumIndex;
	int iCacheNames;
	char *pchTmp;
	bv_t *pbvTmp;
	char pchBuffer[100];
	char **levelNames[cache_type_enum_size];
	cache_t *pcacheTmp;
	iCacheNames=KAPI_EnumCardinality(pknobs,"cache_names_t");
	for (i=0;i<cache_type_enum_size;i++)
	{
		pknobs->nCacheLevels[i]=0;
	}
	if (iCacheNames<=0)
	{
      kapi_Warning( kapi_cLine, 0, 
                  "Attribute 'cache_names_t' not defined or incorrect" );
      return;
	}
	/* init cache levels */
	/* get values for cache levels */
	for (i=0;i<iCacheNames;i++)
	{
		pchTmp=KAPI_EnumName(pknobs,i,"cache_names_t");
		if (strchr(pchTmp,'I')!=NULL) {
			pknobs->nCacheLevels[cache_type_instruction]++;
		}
		else if (strchr(pchTmp,'D')!=NULL) {
			pknobs->nCacheLevels[cache_type_data]++;
		}
		else {
			pknobs->nCacheLevels[cache_type_unified]++;
		}
		free(pchTmp); 
	}
	for (i=0;i<cache_type_enum_size;i++)
	{
		levelNames[i]=(char **)malloc(sizeof(char *)*pknobs->nCacheLevels[i]);
		pknobs->nCacheLevels[i]=0;
	}
	/* get values for cache levels */
	for (i=0;i<iCacheNames;i++)
	{
		pchTmp=KAPI_EnumName(pknobs,i,"cache_names_t");
		if (strchr(pchTmp,'I')!=NULL) {
			levelNames[cache_type_instruction][pknobs->nCacheLevels[cache_type_instruction]]=pchTmp;
			pknobs->nCacheLevels[cache_type_instruction]++;
		}
		else if (strchr(pchTmp,'D')!=NULL) {
			levelNames[cache_type_data][pknobs->nCacheLevels[cache_type_data]]=pchTmp;
			pknobs->nCacheLevels[cache_type_data]++;
		}
		else {
			levelNames[cache_type_unified][pknobs->nCacheLevels[cache_type_unified]]=pchTmp;
			pknobs->nCacheLevels[cache_type_unified]++;
		}
	}
	/* init cache tables */
	for (i=0;i<cache_type_enum_size;i++)
	{
		pknobs->dmpcacheTable[i]=(cache_t *)malloc(sizeof(cache_t)*pknobs->nCacheLevels[i]);
	}
	/* get values for cache tables */
	for (iType=0;iType<cache_type_enum_size;iType++)
	{
		for (iLevel=0;iLevel<pknobs->nCacheLevels[iType];iLevel++)
		{
			iEnumIndex=KAPI_EnumIndex(pknobs,"cache_names_t",levelNames[iType][iLevel]);
			pcacheTmp=&(pknobs->dmpcacheTable[iType][iLevel]);
			assert(pcacheTmp!=NULL);
			pcacheTmp->nLines=KAPI_GetIntegerVariable(pknobs,"CACHE_Lines",iEnumIndex);
			pcacheTmp->nBytesLine=KAPI_GetIntegerVariable(pknobs,"CACHE_BytesPerLine",iEnumIndex);
			pcacheTmp->nWays=KAPI_GetIntegerVariable(pknobs,"CACHE_Ways",iEnumIndex);
			pcacheTmp->nCyclesRead=KAPI_GetIntegerVariable(pknobs,"CACHE_ReadLatency",iEnumIndex);
			pbvTmp=KAPI_GetBvVariable(pknobs,"CACHE_Content",iEnumIndex);
			if (pbvTmp==NULL)
			{
				*pchBuffer='\0';
				sprintf(pchBuffer+strlen(pchBuffer),"CACHE_Content[%s]",levelNames[iType][iLevel]);
				kapi_Error_pch1( kapi_cLine, 1, 
						  " %s should exists according to definition but is not defined.",pchBuffer );
				return;
			}
			pcacheTmp->bv32CacheContents=pbvTmp->pint32Data[0];
			free(pbvTmp);
			pcacheTmp->iWritePolicy=KAPI_GetEnumVariable(pknobs,"CACHE_PolicyWrite",iEnumIndex);
			pcacheTmp->iReplPolicy=KAPI_GetEnumVariable(pknobs,"CACHE_PolicyRepl",iEnumIndex);
			pcacheTmp->iAllocPolicy=KAPI_GetEnumVariable(pknobs,"CACHE_PolicyAlloc",iEnumIndex);
			pcacheTmp->nCachePorts=KAPI_GetIntegerVariable(pknobs,"CACHE_Ports",iEnumIndex);
			for (iPort=0;iPort<pcacheTmp->nCachePorts;iPort++)
			{
				/* typedef struct _CACHEPORT_T {
					   bv32_t  bv32AccessMode;
					} cacheport_t; */
				*pchBuffer='\0';
				sprintf(pchBuffer,"CACHE_Port%dAccessTypes",iPort);
				pbvTmp=KAPI_GetBvVariable(pknobs,pchBuffer,iEnumIndex);
				if (pbvTmp!=NULL) {
					pcacheTmp->cacheportInfo[iPort].bv32AccessMode=pbvTmp->pint32Data[0];
					free(pbvTmp);
				} else {
					  sprintf(pchBuffer+strlen(pchBuffer),"[%s]",levelNames[iType][iLevel]);
					  kapi_Error_pch1( kapi_cLine, 1, 
								  " %s should exists according to definition but is not defined.",pchBuffer );
					  return;
				}
			}
			/* set the access mode for the rest of the ports to 0 */
			for (;iPort<KAPI_MAX_CACHE_PORTS_IMPL;iPort++)
				pcacheTmp->cacheportInfo[iPort].bv32AccessMode=0;
				/* of port loop */
		} /* of level loop */
	} /* of type loop */
}

void *KAPI_ia64_Initialize(void *pConfig)
{
	  knobs_t *pknobs=(knobs_t *)pConfig;
      int fOk;

      fOk = kapi_fEnableIA64Calls( pknobs );
      if ( !fOk ) {
		  kapi_Error( -1, 0, "Unable to build structured information table" );
		  KAPI_error_attribute = 1;
		  return( NULL );
      } else
		  return pConfig;
}

