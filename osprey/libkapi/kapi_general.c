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


#include <stdio.h>
#include <memory.h>
#include <malloc.h>
#include <assert.h>
#include <locale.h>
#include "kapi_internal.h"
#include "kapi_util.h"
#include "kapi_parse.h"
#include "kapi_error.h"
#include "kapi_symbol.h"
#include "kapi_ia64.h"

void KDebug_DumpKnobs( FILE *fp, knobs_t *pknobs, int nTab );
int kapi_fEnableIA64Calls( knobs_t *pknobs );


int   KAPI_error_attribute = 0;
int   kapi_fTrace_Msgs_On = 0;


void  kapi_ReadKnobsfile( FILE *fp, knobs_t *pknobs );
static int fCheckExpectedValues( knobs_t *pknobs );
static void RestructureAttributes( knobs_t *pknobs );
static void kapi_InitClusterPortDatastructures( knobs_t *pknobs );
static void kapi_free_ia64_mem(knobs_t *pknobs);
extern int yydebug;
void *
KAPI_Initialize( FILE *fpDelta, FILE *fpBaseline, char *pchToolname)
{
   knobs_t *pknobs;
   int fExpectedOk;
   char *pchFN;


   KAPI_error_attribute = 0;

   pknobs = (knobs_t *) malloc( sizeof( knobs_t ) );
   memset( pknobs, 0, sizeof( knobs_t ) );

   pknobs->pchToolname = kapi_pchCopy( pchToolname );
   pknobs->fRestructuredAttributes = 0;
   pknobs->fImplicitNone = 0;
   pknobs->nclr = 1;
   pknobs->pfSaveHeaderFlags=NULL;

   kapi_InitSymbolTable( pknobs );

   /* 
      Since these fields are updated on the fly while the
      knobsfile is parsed, we need to initialize them now.
    */
   kapi_InitClusterPortDatastructures( pknobs );

   kapi_ParseFile( pknobs, fpBaseline );
   if ( kapi_cntSemanticError > 0 || kapi_cntSyntaxError > 0 ) {
      KAPI_error_attribute = 1;
      return( NULL );
   }
#if YYDEBUG!=0
	yydebug=1;
#endif
   kapi_ParseFile( pknobs, fpDelta );

   /* if there was an error, exit */
   if ( kapi_cntSemanticError > 0 || kapi_cntSyntaxError > 0 ) {
      KAPI_error_attribute = 1;
      return( NULL );
   }

   /* 
      This routine updates the storage of several types of data
      so that it can be accessed more quickly.
   */
   RestructureAttributes( pknobs );


   fExpectedOk = fCheckExpectedValues( pknobs );
   if ( !fExpectedOk ) {
      KAPI_error_attribute = 1;
      return( NULL );
   }

   /* print KAPI tracing messages for YAMM trace mode */
   if ( 1 == KAPI_GetIntegerVariable( pknobs, "ScheduleTraceMessages", 0 ) ) {
      kapi_fTrace_Msgs_On = 1;
   }

   /* Dump knobs, if requested */
   pchFN = KAPI_GetStringVariable( pknobs, "DumpKnobsSummary", 0 );
   if ( NULL != pchFN ) {
      FILE *fp_local;

      fp_local = fopen( pchFN, "w" );
      if ( fp_local ) {
         KDebug_DumpKnobs( fp_local, pknobs, 0 );
         fclose( fp_local );
      }
   }


   return( pknobs );
}

void *
KAPI_Initialize2( char *pchDelta, char *pchBaseline, char *pchToolname )
{
   knobs_t *pknobs;
   FILE *fpDelta, *fpBaseline;


   if ( pchDelta != NULL ) {
      fpDelta = fopen( pchDelta, "r" );
   } else {
      fpDelta = NULL;
   }
   if ( pchBaseline != NULL ) {
      fpBaseline = fopen( pchBaseline, "r" );
   } else {
      fpBaseline = NULL;
   }

   if ( fpDelta == NULL && fpBaseline == NULL ) {
      printf("KAPI ERROR: Files '%s' and '%s' do not exist\n", pchDelta, pchBaseline );
      return( NULL );
   }

   pknobs = KAPI_Initialize( fpDelta, fpBaseline, pchToolname );

   if ( kapi_fTrace_Msgs_On  ) {
      fprintf( stderr, "+KAPI_Initialize2 < %s %s %s > %d\n", 
                      pchDelta ? pchDelta : "NULL", 
                      pchBaseline ? pchBaseline : "NULL", 
                      pchToolname,  (int)(long)pknobs );
   }

   if ( fpDelta ) {
      fclose( fpDelta );
   }
   if ( fpBaseline ) {
      fclose( fpBaseline );
   }

   return( pknobs );
}



void kapi_free_valn_string_list(valn_t *pvaln)
{
	valn_t *pvalnNext;
	  while (pvaln!=NULL)
	  {
			pvalnNext=pvaln->pvalnNext;
			free(pvaln->val.pch);
			free(pvaln);
			pvaln=pvalnNext;
	  }
}


static void kapi_free_stn_node(stn_t *pstn)
{
	ed_t *ped,*pedNext;
	limp_t *plimp,*plimpNext;
	switch (pstn->ity)
	{
	case ityVARNAME:
		ped=pstn->u.vfi.pedList;
		while (ped!=NULL)
		{
			pedNext=ped->pedNext;
			free(ped);
			ped=pedNext;
		}
	break;
	}
	plimp=pstn->plimpListParse;
	while (plimp!=NULL)
	{
		plimpNext=plimp->plimpNext;
		free(plimp);
		plimp=plimpNext;
	}
	if (pstn->pchName!=NULL) free(pstn->pchName);
}

void
KAPI_Finalize( void *pConfig )
{
   knobs_t *pknobs = pConfig;
   int i;
   stn_t *pstn,*pstnNext;

   if ( kapi_fTrace_Msgs_On  ) {
      fprintf( stderr, "+KAPI_Finalize < %d\n", (int)(long)pknobs );
   }
   if ( pConfig ) {
	  kapi_free_ia64_mem(pknobs);
	  /* need to free all symbol table info here ... */
	  kapi_free_valn_string_list(pknobs->pvalnTypeList);
	  kapi_free_valn_string_list(pknobs->pvalnVarList);
	  kapi_free_valn_string_list(pknobs->pvalnAttrList);

	  for (i=0;i<pknobs->nstn;i++)
	  {
		  pstn=pknobs->dmpstn[i].pstnHashNext;
		  while (pstn!=NULL)
		  {
			kapi_free_stn_node(pstn);
			pstnNext=pstn->pstnHashNext;
			free(pstn);
			pstn=pstnNext;
		  }
	  }
	  free(pknobs->dmpstn);
      free( pknobs ); 
   }
}


/* --------------------- Support routines ------------------------- */


static int
fCheckExpectedValues( knobs_t *pknobs )
{
   stn_t *pstnRun;
   int fPass;
   int i;

   fPass = 1;
   for ( i=0; i<pknobs->nstn; i++ ) {
      pstnRun = pknobs->dmpstn[ i ].pstnHashNext;
      while ( pstnRun ) {
         if ( pstnRun->fExpected ) {
             kapi_Error_pch2( -1, 0, "%s not found in knobsfile, expected to be declared as a %s", 
                              pstnRun->pchName, ity2pchname( pstnRun->ity ) );
             fPass = 0;
          }
          pstnRun = pstnRun->pstnHashNext;
      }
   }

   return( fPass );
}


static void
RestructureAttributes( knobs_t *pknobs )
{
   valn_t *pvalnRun;


   assert( pknobs->fRestructuredAttributes == 0 );


   /* restructure attributes */
      /* allocate arrays and copy list into array, 
         old nodes dropped on floor */

   /* traverse attributes */
   pvalnRun = pknobs->pvalnAttrList;
   while ( pvalnRun ) {
      stn_t *pstnTmp;
      afi_t *pafiList, *pafiRun;
      int idx;

      pstnTmp = kapi_pstnLookup_noadd( pknobs, pvalnRun->val.pch );
      assert( pstnTmp->ity == ityATTRIBUTENAME );

      /* grab the list */
      pafiList = pstnTmp->u.afih.u.pafi;

      /* malloc new space */
      pstnTmp->u.afih.u.dmppch = (char **)malloc( 
                              sizeof( char * ) * pstnTmp->u.afih.nAttr ); 

      /* copy linked list into array and reverse order */
      pafiRun = pafiList;
      idx = pstnTmp->u.afih.nAttr - 1;
      while ( pafiRun ) {
         pstnTmp->u.afih.u.dmppch[ idx ] = pafiRun->pchAttrVal;
         pafiRun = pafiRun->pafiNext;
         idx--;
      }

      pvalnRun = pvalnRun->pvalnNext;
   }


   pknobs->fRestructuredAttributes = 1;
}


static void
kapi_InitClusterPortDatastructures( knobs_t *pknobs )
{
   int clr;
	
   pknobs->nclr = 1;
   for ( clr=0;clr<maxCLR;clr++ ) {
       clr_t *pclr;

       pclr = &(pknobs->mpclrTable[ clr ]);
       pclr->idxclr = clr;

       pclr = &(pknobs->mpclrTable[ clr ]);
       pclr->dmppcportinfoTable[ kapi_utI ] 
                = (cportinfo_t *)malloc( sizeof(cportinfo_t)*maxICPORTS );
       memset( pclr->dmppcportinfoTable[ kapi_utI ], 0, 
               sizeof(cportinfo_t)*maxICPORTS );

       pclr->dmppcportinfoTable[ kapi_utM ] 
                = (cportinfo_t *)malloc( sizeof(cportinfo_t)*maxMCPORTS );
       memset( pclr->dmppcportinfoTable[ kapi_utM ], 0, 
               sizeof(cportinfo_t)*maxMCPORTS );

       pclr->dmppcportinfoTable[ kapi_utB ] 
                = (cportinfo_t *)malloc( sizeof(cportinfo_t)*maxBCPORTS );
       memset( pclr->dmppcportinfoTable[ kapi_utB ], 0, 
               sizeof(cportinfo_t)*maxBCPORTS );

       pclr->dmppcportinfoTable[ kapi_utF ] 
                = (cportinfo_t *)malloc( sizeof(cportinfo_t)*maxFCPORTS );
       memset( pclr->dmppcportinfoTable[ kapi_utF ], 0, 
               sizeof(cportinfo_t)*maxFCPORTS );

       pclr->mpncutport[ kapi_utI ] = -1;
       pclr->mpncutport[ kapi_utM ] = -1;
       pclr->mpncutport[ kapi_utF ] = -1;
       pclr->mpncutport[ kapi_utB ] = -1;
   }
}

/* This function will deallocate memory occupied by the 
   knobs_t structure if needed.
*/
static void kapi_free_ia64_mem(knobs_t *pknobs)
{
   int clr,i;
   /* if the structure was initialized from header,
   don't deallocate! */
   if (pknobs->pfSaveHeaderFlags!=NULL)
	   return; 
	
   for ( clr=0;clr<maxCLR;clr++ ) {
       clr_t *pclr;

       pclr = &(pknobs->mpclrTable[ clr ]);
       free(pclr->dmppcportinfoTable[ kapi_utI ]);
       free(pclr->dmppcportinfoTable[ kapi_utM ]);
       free(pclr->dmppcportinfoTable[ kapi_utB ]);
       free(pclr->dmppcportinfoTable[ kapi_utF ]);
   }
   free(pknobs->dmpitinfoTable);
   free(pknobs->dmpsylinfoTable);
   free(pknobs->dmpbidinfoTable);
   free(pknobs->dmpfuinfoTable);
   free(pknobs->dmpebyInterTable);
   free(pknobs->dmpabyIntraTable);
   free(pknobs->dmpinstTable);
	for (i=0;i<cache_type_enum_size;i++)
	{
		free(pknobs->dmpcacheTable[i]);
	}

}
