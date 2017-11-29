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
#include "kapi_error.h"


/* -------------------  User visible routines  ------------------- */

int
KAPI_CoreLatency( void *pConfig, kapi_fu_t fuProd, int opp )
{
   knobs_t *pknobs = pConfig;

   assert( pknobs->dmpfuinfoTable[ fuProd ].iCoreLatency[ opp ] >= 0 );
   return( pknobs->dmpfuinfoTable[ fuProd ].iCoreLatency[ opp ] );
}

int
KAPI_InterClusterBypass( void *pConfig,
             kapi_cluster_t clusterProd, kapi_fu_t fuProd,
                 int oppProd, kapi_ut_t utProd, kapi_cutport_t cutportProd,
             kapi_cluster_t clusterCons, kapi_fu_t fuCons,
                 int oppCons, kapi_ut_t utCons, kapi_cutport_t cutportCons )
{
   knobs_t *pknobs = pConfig;
   int i;

   /* temporary implementation */
   for ( i=0;i<pknobs->nRawInter;i++ ) {
      eby_t *peby;

      peby = &(pknobs->dmpebyInterTable[ i ]);

      if ( ( clusterProd != peby->clusterSrc && peby->clusterSrc != -1 ) ||
           ( clusterCons != peby->clusterDest && peby->clusterDest != -1 ) ) {
         continue;
      }

      if ( fuProd != peby->fuSrc ||
           fuCons != peby->fuDest ) {
         continue;
      }

      if ( oppProd != peby->oppSrc ||
           oppCons != peby->oppDest ) {
         continue;
      }

      if ( ( cutportProd != peby->cutportSrc && peby->cutportSrc != -1 ) ||
           ( cutportCons != peby->cutportDest && peby->cutportDest != -1 ) ) {
         continue;
      }

      return( peby->iValue );
   }

   /* default */
   return( 0 );
}

int
KAPI_IntraClusterBypass( void *pConfig,
             kapi_cluster_t cluster,
             kapi_fu_t fuProd, int oppProd, kapi_ut_t utProd, kapi_cutport_t
cutportProd,
             kapi_fu_t fuCons, int oppCons, kapi_ut_t utCons, kapi_cutport_t
cutportCons )
{
   knobs_t *pknobs = pConfig;
   int i;

   /* temporary implementation */
   for ( i=0;i<pknobs->nRawIntra;i++ ) {
      aby_t *paby;
      /* kapi_cport_t cportSrc, cportDest; */

      paby = &(pknobs->dmpabyIntraTable[ i ]);

      if ( ( cluster != paby->cluster && paby->cluster != -1 ) ) {
         continue;
      }

      if ( fuProd != paby->fuSrc ||
           fuCons != paby->fuDest ) {
         continue;
      }

      if ( oppProd != paby->oppSrc ||
           oppCons != paby->oppDest ) {
         continue;
      }

      if ( ( utProd != paby->utSrc && paby->utSrc != -1 ) ||
           ( utCons != paby->utDest && paby->utDest != -1 ) ) {
         continue;
      }

      if ( ( cutportProd != paby->cutportSrc && paby->cutportSrc != -1 ) ||
           ( cutportCons != paby->cutportDest && paby->cutportDest != -1 ) ) {
         continue;
      }


      return( paby->iValue );
   }

   /* default */
   return( 0 );
}

int
KAPI_ClusterDistance( void *pConfig,
             kapi_cluster_t clusterProd, kapi_cluster_t clusterCons )
{
   knobs_t *pknobs = pConfig;

   return( pknobs->mpclrTable[ clusterProd ].distClr[ clusterCons ] );
}

int
KAPI_TotalLatency( void *pConfig,
             kapi_cluster_t clusterProd, kapi_fu_t fuProd,
                 int oppProd, kapi_ut_t utProd, kapi_cutport_t cutportProd,
             kapi_cluster_t clusterCons, kapi_fu_t fuCons,
                 int oppCons, kapi_ut_t utCons, kapi_cutport_t cutportCons )
{
   int result;

   if ( clusterProd != clusterCons ) {
   /* Inter cluster */
      result = KAPI_InterClusterBypass( pConfig,
                                      clusterProd, fuProd, oppProd, utProd,
cutportProd,
                                      clusterCons, fuCons, oppCons, utCons,
cutportCons )
               + KAPI_CoreLatency( pConfig, fuProd, oppProd )
               + KAPI_ClusterDistance( pConfig, clusterProd, clusterCons );
   } else {
   /* Intra cluster */
      result = KAPI_IntraClusterBypass( pConfig, clusterProd, fuProd, oppProd,
                                           utProd, cutportProd,
                                           fuCons, oppCons, utCons, cutportCons
)
               + KAPI_CoreLatency( pConfig, fuProd, oppProd );
   }
   return( result );
}


papair_t *mppapairList = NULL;
pepair_t *mppepairList = NULL;


papair_t *
KAPI_IntraClusterBypassList( void *pConfig, kapi_cluster_t cluster,
                          kapi_fu_t fuSrc, int oppSrc, kapi_ut_t utSrc,
                                  kapi_cutport_t cutportSrc,
                          kapi_fu_t fuDest, int oppDest, kapi_ut_t utDest,
                                  kapi_cutport_t cutportDest,
                          int *pnbypass )
{
   knobs_t *pknobs = pConfig;
   aby_t *paby;
   int cnt, iaby;
   cnt = 0;

   /* Allocate space for a copy of the bypass list exactly once! */
   if (mppapairList == NULL)
   {
		mppapairList = (papair_t *)calloc(pknobs->nRawIntra, sizeof (papair_t));
		if (mppapairList == NULL)
		{
			fprintf(stderr, "Allocate copy of intra bypass list failed!\n");
			return NULL;
		}
   }

   for ( iaby=0; iaby<pknobs->nRawIntra; iaby++ ) {
       paby = &(pknobs->dmpabyIntraTable[ iaby ]);
       /* do clusters match? */
       if ( !( cluster == paby->cluster
            || cluster == -1 || paby->cluster == -1 ) ) {
           continue;
       }

       /* do fu's match? */
       if ( !( fuSrc == paby->fuSrc || fuSrc == -1 ) ) {
          continue;
       }

       if ( !( fuDest == paby->fuDest || fuDest == -1  ) ) {
          continue;
       }


       /* do opp's match? */
       if ( !( oppSrc == paby->oppSrc || oppSrc == -1 ) ) {
          continue;
       }

       if ( !( oppDest == paby->oppDest || oppDest == -1 ) ) {
          continue;
       }


       /* do cutports's match? */
       if (  !( cutportSrc == paby->cutportSrc
                || cutportSrc == -1 || paby->cutportSrc == -1 ) ) {
          continue;
       }

       if (  !( cutportDest == paby->cutportDest
                || cutportDest == -1 || paby->cutportDest == -1 ) ) {
          continue;
       }


       /* do ut's match? */
       if (  !( utSrc == paby->utSrc
                || utSrc == -1 || paby->utSrc == -1 ) ) {
          continue;
       }

       if (  !( utDest == paby->utDest
                || utDest == -1 || paby->utDest == -1 ) ) {
          continue;
       }

       mppapairList[ cnt ].cluster		= paby->cluster;
       mppapairList[ cnt ].cutportSrc	= paby->cutportSrc;
       mppapairList[ cnt ].cutportDest	= paby->cutportDest;
       mppapairList[ cnt ].utSrc		= paby->utSrc;
       mppapairList[ cnt ].utDest		= paby->utDest;
       mppapairList[ cnt ].fuSrc		= paby->fuSrc;
       mppapairList[ cnt ].fuDest		= paby->fuDest;
       mppapairList[ cnt ].oppSrc		= paby->oppSrc;
       mppapairList[ cnt ].oppDest		= paby->oppDest;
       mppapairList[ cnt ].iValue		= paby->iValue;

       cnt++;
   }

   *pnbypass = cnt;
   return( &(mppapairList[ 0 ]) );
}


pepair_t *
KAPI_InterClusterBypassList( void *pConfig,
                          kapi_cluster_t clusterSrc,
                          kapi_fu_t fuSrc, int oppSrc, kapi_ut_t utSrc,
                                     kapi_cutport_t cutportSrc,
                          kapi_cluster_t clusterDest,
                          kapi_fu_t fuDest, int oppDest, kapi_ut_t utDest,
                                     kapi_cutport_t cutportDest,
                          int *pnbypass)
{
   knobs_t *pknobs = pConfig;
   eby_t *peby;
   int cnt, ieby;


   cnt = 0;

   /* Allocate space for a copy of the bypass list exactly once! */
   if (mppepairList == NULL)
   {
		mppepairList = (pepair_t *)calloc(pknobs->nRawIntra, sizeof (pepair_t));
		if (mppepairList == NULL)
		{
			fprintf(stderr, "Allocate copy of inter bypass list failed!\n");
			return NULL;
		}
   }


   for ( ieby=0; ieby<pknobs->nRawInter; ieby++ ) {
       peby = &(pknobs->dmpebyInterTable[ ieby ]);

       /* do clusters match? */
       if ( !( clusterSrc == peby->clusterSrc
            || clusterSrc == -1 ) ) {
           continue;
       }

       if ( !( clusterDest == peby->clusterDest
            || clusterDest == -1 ) ) {
           continue;
       }

       /* do fu's match? */
       if ( !( fuSrc == peby->fuSrc || fuSrc == -1 ) ) {
          continue;
       }

       if ( !( fuDest == peby->fuDest || fuDest == -1  ) ) {
          continue;
       }


       /* do opp's match? */
       if ( !( oppSrc == peby->oppSrc || oppSrc == -1 ) ) {
          continue;
       }

       if ( !( oppDest == peby->oppDest || oppDest == -1 ) ) {
          continue;
       }


       /* do cutports's match? */
       if (  !( cutportSrc == peby->cutportSrc
                || cutportSrc == -1
                || peby->cutportSrc == -1 ) ) {
          continue;
       }

       if (  !( cutportDest == peby->cutportDest
                || cutportDest == -1
                || peby->cutportDest == -1 ) ) {
          continue;
       }


       /* do ut's match? */
       if (  !( utSrc == peby->utSrc
                || utSrc == -1
                || peby->utSrc == -1 ) ) {
          continue;
       }

       if (  !( utDest == peby->utDest
                || utDest == -1
                || peby->utDest == -1 ) ) {
          continue;
       }

       mppepairList[ cnt ].clusterSrc	= peby->clusterSrc;
       mppepairList[ cnt ].clusterDest	= peby->clusterDest;
       mppepairList[ cnt ].cutportSrc	= peby->cutportSrc;
       mppepairList[ cnt ].cutportDest	= peby->cutportDest;
       mppepairList[ cnt ].utSrc		= peby->utSrc;
       mppepairList[ cnt ].utDest		= peby->utDest;
       mppepairList[ cnt ].fuSrc		= peby->fuSrc;
       mppepairList[ cnt ].fuDest		= peby->fuDest;
       mppepairList[ cnt ].oppSrc		= peby->oppSrc;
       mppepairList[ cnt ].oppDest		= peby->cutportDest;
       mppepairList[ cnt ].iValue		= peby->utSrc;

       cnt++;
   }

   *pnbypass = cnt;
   return( &(mppepairList[ 0 ]) );
}

int
KAPI_MinIntraClusterTotalLatency( void *pConfig, kapi_cluster_t cluster,
        kapi_fu_t fuProd, int oppProd, kapi_fu_t fuCons, int oppCons )
{
   knobs_t *pknobs = pConfig;
   int latcore, minlatbypass, latbypass;
   kapi_ut_t utSrc, utDest;
   int oppProdTmp,oppConsTmp,oppProdStart,oppProdEnd,oppConsStart,oppConsEnd;


   minlatbypass = 10000;

   if (oppProd==-1)
   {
	   oppProdStart=0;
	   oppProdEnd=pknobs->dmpfuinfoTable[ fuProd ].cntDest;
   } else
   {
	   oppProdStart=oppProd;
	   oppProdEnd=oppProdStart+1;
   }

   if (oppCons==-1)
   {
	   oppConsStart=0;
	   oppConsEnd=pknobs->dmpfuinfoTable[ fuCons ].cntSrc;
   } else
   {
	   oppConsStart=oppCons;
	   oppConsEnd=oppConsStart+1;
   }
   for (oppProdTmp=oppProdStart;oppProdTmp<oppProdEnd;oppProdTmp++)
   {
	   for (oppConsTmp=oppConsStart;oppConsTmp<oppConsEnd;oppConsTmp++)
	   {
		   for ( utSrc=0; utSrc< kapi_nUT; utSrc++ ) {
			  for ( utDest=0; utDest< kapi_nUT; utDest++ ) {

				 latbypass = KAPI_IntraClusterBypass( pConfig,
					 cluster,
					 fuProd, oppProdTmp, utSrc, 0,
					 fuCons, oppConsTmp, utDest, 0 );

				 if ( latbypass < minlatbypass ) {
					minlatbypass = latbypass;
				 }
			  } /* for on utTar */
		   } /* for on utSrc */
	   } /* for on oppConsTmp */
   } /* for on oppProdTmp */

   latcore = KAPI_CoreLatency( pConfig, fuProd, oppProd );

   assert( minlatbypass != 10000 );
   return( latcore + minlatbypass );
}

/* maximum latency taking into account direct bypasses */
int
KAPI_MaxIntraClusterTotalLatency( void *pConfig, kapi_cluster_t cluster,
        kapi_fu_t fuProd, int oppProd, kapi_fu_t fuCons, int oppCons )
{
   knobs_t *pknobs = pConfig;
   int latcore, maxnlatbypass, latbypass;
   kapi_ut_t utSrc, utTar;
   int oppProdTmp,oppConsTmp,oppProdStart,oppProdEnd,oppConsStart,oppConsEnd;

   maxnlatbypass = -1;

   if (oppProd==-1)
   {
	   oppProdStart=0;
	   oppProdEnd=pknobs->dmpfuinfoTable[ fuProd ].cntDest;
   } else
   {
	   oppProdStart=oppProd;
	   oppProdEnd=oppProdStart+1;
   }

   if (oppCons==-1)
   {
	   oppConsStart=0;
	   oppConsEnd=pknobs->dmpfuinfoTable[ fuCons ].cntSrc;
   } else
   {
	   oppConsStart=oppCons;
	   oppConsEnd=oppConsStart+1;
   }
   for (oppProdTmp=oppProdStart;oppProdTmp<oppProdEnd;oppProdTmp++)
   {
	   for (oppConsTmp=oppConsStart;oppConsTmp<oppConsEnd;oppConsTmp++)
	   {
		   for ( utSrc=0; utSrc< kapi_nUT; utSrc++ ) {
			  for ( utTar=0; utTar< kapi_nUT; utTar++ ) {

				 latbypass = KAPI_IntraClusterBypass( pConfig,
					 cluster,
					 fuProd, oppProdTmp, utSrc, 0,
					 fuCons, oppConsTmp, utTar, 0 );

				 if ( latbypass > maxnlatbypass ) {
					maxnlatbypass = latbypass;
				 }
			  } /* for on utTar */
		   } /* for on utSrc */
	   } /* for on oppConsTmp */
   } /* for on oppProdTmp */

   latcore = KAPI_CoreLatency( pConfig, fuProd, oppProd );

   assert( maxnlatbypass != -1 );
   return( latcore + maxnlatbypass );
}


