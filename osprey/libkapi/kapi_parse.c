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
#include <string.h>

#include <malloc.h>
#include <assert.h>
#include "kapi_parse.h"
#include "kapi_error.h"

#include "kapi_debug.h"
#include "kapi_internal.h"
#include "kapi_util.h"

tokval_t kapi_lval;
int      kapi_cLine;

extern   FILE *yyin;
extern   char *yytext;

knobs_t *kapi_pknobsCurrent;

static void AddEnums( stn_t *pstnType, valn_t *pvalnIdents, int fAllowRepeats );
static void AppendEnums( stn_t *pstnType, valn_t *pvalnIdents, int fAllowRepeats);

static int fMatch_valn_list( valn_t *pvalnList1, valn_t *pvalnList2, tty_t tty );

void pvalhdrCopy( valhdr_t *pvalhdrTarget, valhdr_t *pvalhdrSource );
int fFailCheckLimitValue( valhdr_t *pvalhdr, ed_t *ped );
int fFailCheckLimitValue2( valn_t *pvalnCheck, valhdr_t *pvalhdrLimit );

void SetVariableValueLimit( valhdr_t *pvalhdr, limp_t *plimpNew );
static int posInEnumList( char *pch, valn_t *pvalnEnums );
static int fFailCheck( valhdr_t *pvalhdrRHS, ed_t *ped, stn_t *pstnLHS, char * );
static int fCheckIdenticalVal( valhdr_t *pvalhdrRHS, ed_t *pedRHSVal );
static int fEqual_val( val_t *pval1, val_t *pval2, tty_t tty );

static void KAPI2X_checkPortTypes( knobs_t *pknobs, 
                         stn_t *pstnArray, char *, valhdr_t * );

static void KAPI2X_update_cluster0Cutports( knobs_t *pknobs, stn_t *pstnType );
static void KAPI3X_update_cluster0Cutports( knobs_t *pknobs, stn_t *pstnArray, 
                                            char *, valhdr_t * );
static void AddStringToAttr( stn_t *pstnAttr, char *pch );

void
kapi_ParseFile( knobs_t *pknobs, FILE *fp )
{
   if ( fp == NULL ) {
      return;
   }

   /* reset error counts */
   kapi_cntSemanticWarning = 0;
   kapi_cntSyntaxWarning = 0;
   kapi_cntSemanticError = 0;
   kapi_cntSyntaxError = 0;
   kapi_cLine = 1;
   kapi_pknobsCurrent = pknobs;
   yyin = fp;

   yyparse();
}

void
kapi_error()
{
   fprintf( stderr, "Error found line %4d\n", kapi_cLine );
}

void
pstnTypeInit( stn_t *pstnType )
{
   pstnType->u.tfi.tty = ttyENUM;
   pstnType->u.tfi.pvalnEnums = NULL;
   pstnType->u.tfi.pvalnPreviousEnums = NULL;
   pstnType->u.tfi.nEnumConst = 0;
   pstnType->u.tfi.pstnIdent = pstnType;
   pstnType->u.tfi.tredefStatus = tredefSTATUS_OK;

   pstnType->u.tfi.ptfiBitmaskBaseType = NULL;
   pstnType->u.tfi.ptfiArrayEltType = NULL;
   pstnType->u.tfi.ptfiArrayIndexType = NULL;
   pstnType->fRHS = 0;
}


/* save current enum list pointer, and use addenums
   to add at end of list */
static void
AppendEnums( stn_t *pstnType, valn_t *pvalnIdents, int fAllowRepeats )
{
   valn_t *pvalnOldList;
   valn_t *pvalnNewList;
   valn_t *pvalnRun;

   /* save current list */
   pvalnOldList=pstnType->u.tfi.pvalnEnums;
   
   /* Add the enums */
   AddEnums(pstnType, pvalnIdents, fAllowRepeats );

   pvalnNewList=pstnType->u.tfi.pvalnEnums;

   /* connect the new enum list with the old one */
   pvalnRun=pvalnOldList;
   /* goto end of old list */
   while (pvalnRun->pvalnNext!=NULL)
		pvalnRun = pvalnRun->pvalnNext;

   pvalnRun->pvalnNext=pvalnNewList;
   /* now put in the full list for the type */
   pstnType->u.tfi.pvalnEnums=pvalnOldList;
}


static void
AddEnums( stn_t *pstnType, valn_t *pvalnIdents, int fAllowRepeats )
{
   stn_t *pstnRun;
   valn_t *pvalnRun;

   pstnType->u.tfi.pvalnEnums = pvalnIdents;
   pvalnRun = pvalnIdents;
   while ( pvalnRun ) {
      pstnRun = kapi_pstnLookup( kapi_pknobsCurrent, pvalnRun->val.pch );
      if ( pstnRun->ity == ityUNKNOWN ||
           ( pstnRun->ity == ityENUMCONST && pstnRun->fExpected ) 
           || fAllowRepeats ) {
         pstnRun->ity = ityENUMCONST;
         pstnRun->fExpected = 0;
         pstnRun->u.efi.ptfiBaseType = &(pstnType->u.tfi);
         pstnType->u.tfi.nEnumConst++;
      } else {
         kapi_Error_pch1( kapi_cLine, 0, 
                          "Identifier '%s' already defined, cannot reuse as enum", 
                          pstnRun->pchName );
      }
      pvalnRun = pvalnRun->pvalnNext;
   }
}

static void
DefineType( stn_t *pstnType, valn_t *pvalnIdents, limp_t *plimpList )
{
   limp_t *plimp;

   pstnTypeInit( pstnType );
   pstnType->ity = ityTYPENAME;
   pstnType->fExpected = 0;  /* turn off Expected flag */
   pstnType->plimpListParse = plimpList;

   plimp = plimpLookup( plimpList, kapi_pknobsCurrent->pchToolname );
   if ( plimp ) {
      pstnType->u.tfi.tredefStatus = tredefSTATUS_MARKED_NOREDEFINE;
      if ( plimp->valhdrValues.vals != valsNOREDEFINE ) {
         kapi_Error( kapi_cLine, 0, "Limits on types can only be specified as 'noredefine'" );
      }
   }

   AddEnums( pstnType, pvalnIdents, 0 );
}

static valn_t *
pvalnRemoveEnums( stn_t *pstnType )
{
   /* stn_t *pstnDel; */
   valn_t *pvalnRun, *pvalnDelList;

   pvalnRun = pstnType->u.tfi.pvalnEnums;
   while ( pvalnRun ) {
      (void)kapi_pstnDelete( kapi_pknobsCurrent, pvalnRun->val.pch );

      /* update runner pointer */
      pvalnRun = pvalnRun->pvalnNext;
   }

   pvalnDelList = pstnType->u.tfi.pvalnEnums;
   pstnType->u.tfi.pvalnEnums = NULL;

   return( pvalnDelList );
}

int
fInEnumList( char *pch, valn_t *pvalnEnums )
{
   valn_t *pvalnRun;

#ifdef TRACE_REDEF
   fprintf( stderr, "\t   searching %s\n", pch );
#endif
   pvalnRun = pvalnEnums;
   while ( pvalnRun ) {
#ifdef TRACE_REDEF
      fprintf( stderr, "\t\tchecking %s\n", pvalnRun->val.pch );
#endif
      if ( ! strcmp( pvalnRun->val.pch, pch ) ) {
         return( 1 );
      }
      pvalnRun = pvalnRun->pvalnNext;
   }

   return( 0 );
}

static void
RedefineVariable_scalar( stn_t *pstnType, stn_t *pstnVar )
{
   vfi_t *pvfi;
   valn_t *pvaln;

   /*  
      Undefine values of scalar variables v : T when v's value
      is no longer defined. 
   */
#ifdef TRACE_REDEF
   fprintf( stderr, "Variable %s being examined for redefine\n",
            pstnVar->pchName );
   fprintf( stderr, "\tMatched type - scalar variable\n" );
#endif


   /* no values set or allocated */
   pvfi = &( pstnVar->u.vfi );
   if ( pvfi->pedList == NULL ) { 
      return;
   }

   /* if the value here is not in the enum list, undefine this variable */
   pvaln = pvfi->pedList->valhdrValue.pvalnList;
   if ( ! fInEnumList( pvaln->val.pch, pvfi->ptfi->pvalnEnums ) ) {
#ifdef TRACE_REDEF
      fprintf( stderr, "\tValue %s undefined\n", pvaln->val.pch );
#endif
      pvfi->pedList->valhdrValue.pvalnList = NULL;
      pvfi->pedList->valhdrValue.vals = valsUNSET;
   }
}

void
RedefineBitmask( valhdr_t *pvalhdr, tfi_t *ptfiBitmaskBaseType )
{
   valn_t *pvaln, *pvalnPrev;

   /* start with second element and search/delete */
   pvaln = pvalhdr->pvalnList->pvalnNext;
   pvalnPrev = pvalhdr->pvalnList; 
   while ( pvaln ) {
      if ( ! fInEnumList( pvaln->val.pch, 
                          ptfiBitmaskBaseType->pvalnEnums ) ) {
#ifdef TRACE_REDEF
         fprintf( stderr, "\tBit %s (%x) turned off\n", pvaln->val.pch, (int)pvaln );
#endif
         pvalnPrev->pvalnNext = pvaln->pvalnNext;
         pvaln = pvaln->pvalnNext;
      } else {
         pvalnPrev = pvaln;
         pvaln = pvaln->pvalnNext;
      }
   }

   /* now check the first element in the list */
   pvaln = pvalhdr->pvalnList;
   if ( ! fInEnumList( pvaln->val.pch, 
                       ptfiBitmaskBaseType->pvalnEnums ) ) {
#ifdef TRACE_REDEF
      fprintf( stderr, "\tBit %s turned off (1st)\n", pvaln->val.pch );
#endif
      pvalhdr->pvalnList = pvalhdr->pvalnList->pvalnNext;
      pvalhdr->vals = valsUNSET;
   }
}

static void
RedefineVariable_scalar_bitmask( stn_t *pstnType, stn_t *pstnVar )
{
   vfi_t *pvfi;
   valhdr_t *pvalhdr;
   valn_t *pvaln, *pvalnPrev;


   /*  4) Reset v : bitmasks( T ) for new mappings */
#ifdef TRACE_REDEF
   fprintf( stderr, "Variable %s being examined for redefine\n",
            pstnVar->pchName );
   fprintf( stderr, "\tMatched - bitmask base type - scalar\n" );
#endif


   /* no values set or allocated */
   pvfi = &( pstnVar->u.vfi );
   if ( pvfi->pedList == NULL ) { 
      return;
   }

   assert( pvfi->ptfi->tty == ttyBITMASK );
   pvalhdr = &( pvfi->pedList->valhdrValue );

   /* 
      Check to see if the values in the bitmask are still in the enum list. 
      If not, remove the value from the bitmask (sloppy - throw away memory). 
    */

   RedefineBitmask( pvalhdr, pvfi->ptfi->ptfiBitmaskBaseType );

   /* start with second element and search/delete */
   pvaln = pvalhdr->pvalnList->pvalnNext;
   pvalnPrev = pvalhdr->pvalnList; 
   while ( pvaln ) {
      if ( ! fInEnumList( pvaln->val.pch, 
                          pvfi->ptfi->ptfiBitmaskBaseType->pvalnEnums ) ) {
#ifdef TRACE_REDEF
         fprintf( stderr, "\tBit %s (%x) turned off\n", pvaln->val.pch, (int)pvaln );
#endif
         pvalnPrev->pvalnNext = pvaln->pvalnNext;
         pvaln = pvaln->pvalnNext;
         KDebug_printval( stderr, pvfi->ptfi, pvfi->pedList, 4 ); 
      } else {
         pvalnPrev = pvaln;
         pvaln = pvaln->pvalnNext;
      }
   }

   /* now check the first element in the list */
   pvaln = pvalhdr->pvalnList;
   if ( ! fInEnumList( pvaln->val.pch, 
                       pvfi->ptfi->ptfiBitmaskBaseType->pvalnEnums ) ) {
#ifdef TRACE_REDEF
      fprintf( stderr, "\tBit %s turned off (1st)\n", pvaln->val.pch );
#endif
      pvalhdr->pvalnList = pvalhdr->pvalnList->pvalnNext;
      pvalhdr->vals = valsUNSET;
   }
}

static void
RedefineVariable_array_elt( stn_t *pstnType, stn_t *pstnVar )
{
   ed_t *pedRun, *pedRunPrev;
   vfi_t *pvfi;


   /*  2) Remap V : array[ X ] of T so that:
          - undefine or reset default values of V[x] for which
            its value is not longer defined
   */

#ifdef TRACE_REDEF
   fprintf( stderr, "Variable %s being examined for redefine\n",
            pstnVar->pchName );
   fprintf( stderr, "\tMatched elt type - array variable\n" );
#endif


   /* no values set or allocated */
   pvfi = &( pstnVar->u.vfi );
   if ( pvfi->pedList == NULL ) { 
      return;
   }

   assert( pvfi->ptfi->tty == ttyARRAY );

   /* -- search all array indices that have defined values to
         see if their corresponding element has a value that
         is no longer defined by the base type -- */
   /* start with second element and search/delete */
   pedRun = pvfi->pedList->pedNext; 
   pedRunPrev = pvfi->pedList; 
   while ( pedRun ) {
      if ( ! fInEnumList( pedRun->valhdrValue.pvalnList->val.pch, 
                          pvfi->ptfi->ptfiArrayEltType->pvalnEnums ) ) {
#ifdef TRACE_REDEF
         fprintf( stderr, "\tElt (%s) at idx (%s) removed\n", 
                          pedRun->valhdrValue.pvalnList->val.pch,
                          pedRun->pchIndexName );
#endif
         pedRunPrev->pedNext = pedRun->pedNext;
         pedRun = pedRun->pedNext;
      } else {
         pedRunPrev = pedRun;
         pedRun = pedRun->pedNext;
      }
   }

   /* now check the first element in the list */
   pedRun = pvfi->pedList;
   if ( ! fInEnumList( pedRun->valhdrValue.pvalnList->val.pch, 
                       pvfi->ptfi->ptfiArrayEltType->pvalnEnums ) ) {
#ifdef TRACE_REDEF
      fprintf( stderr, "\tElt (%s) at idx (%s) removed\n", 
                          pedRun->valhdrValue.pvalnList->val.pch,
                          pedRun->pchIndexName );
#endif
      pvfi->pedList = pvfi->pedList->pedNext;
   }
}

static void
RedefineVariable_array_idx( stn_t *pstnType, stn_t *pstnVar )
{
   int fIndexAll;
   vfi_t *pvfi;
   ed_t *pedRun, *pedRunPrev;

   /*  1) Remap V : array[ T ] of X so that:
          - associative values are correct 
          - limit statements are correct
          - remove elements that no longer have corresponding elements
          - undefine or reset default values for new elements added
   */
#ifdef TRACE_REDEF
   fprintf( stderr, "Variable %s being examined for redefine\n",
            pstnVar->pchName );
   fprintf( stderr, "\tMatched idx type - array variable\n" );
#endif


   /* no values set or allocated */
   pvfi = &( pstnVar->u.vfi );
   if ( pvfi->pedList == NULL ) { 
      return;
   }

   assert( pvfi->ptfi->tty == ttyARRAY );

   /* -- search all array elements to see if there is a corresponding
         index.  If there isn't, remove the ped.  We could probably get away 
         without doing this since the index is removed from teh symbol 
         table, but I'm sure some other problem would bite me if I don't 
   */ 
   pedRun = pvfi->pedList->pedNext; 
   pedRunPrev = pvfi->pedList; 
   while ( pedRun ) {
 
      fIndexAll = ! strcmp( pedRun->pchIndexName, "*" );

      if ( !fIndexAll && ! fInEnumList( pedRun->pchIndexName, 
                          pvfi->ptfi->ptfiArrayIndexType->pvalnEnums ) ) {
#ifdef TRACE_REDEF
         fprintf( stderr, "\tIdx (%s) with elt ( ",
                          pedRun->pchIndexName,
                          pedRun->valhdrValue.pvalnList->val.pch );
         KDebug_printval( stderr, pvfi->ptfi->ptfiArrayEltType, pedRun, 0 ); 
         fprintf( stderr, " ) removed\n" );
#endif
         pedRunPrev->pedNext = pedRun->pedNext;
         pedRun = pedRun->pedNext;
      } else {
         pedRunPrev = pedRun;
         pedRun = pedRun->pedNext;
      }
   }

   /* now check the first element in the list */
   pedRun = pvfi->pedList;
   fIndexAll = ! strcmp( pedRun->pchIndexName, "*" );
   if ( !fIndexAll && ! fInEnumList( pedRun->pchIndexName,
                       pvfi->ptfi->ptfiArrayIndexType->pvalnEnums ) ) {
#ifdef TRACE_REDEF
      fprintf( stderr, "\tIdx (%s) with elt (%s) removed\n", 
                          pedRun->pchIndexName,
                          pedRun->valhdrValue.pvalnList->val.pch );
#endif
      pvfi->pedList = pvfi->pedList->pedNext;
   }
   
}

static void
RedefineVariable_array_elt_bitmask( stn_t *pstnType, stn_t *pstnVar )
{
   ed_t *pedRun;
   vfi_t *pvfi;

   /*  2) Remap V : array[ X ] of T so that:
          - undefine or reset default values of V[x] for which
            its value is not longer defined
   */

#ifdef TRACE_REDEF
   fprintf( stderr, "Variable %s being examined for redefine\n",
            pstnVar->pchName );
   fprintf( stderr, "\tMatched bitmask base type of elt - array variable\n" );
#endif


   /* no values set or allocated */
   pvfi = &( pstnVar->u.vfi );
   if ( pvfi->pedList == NULL ) { 
      return;
   }

   assert( pvfi->ptfi->tty == ttyARRAY );

   /* -- search all array indices that have defined values to
         see if their corresponding element has a value that
         has a bit set that is no longer defined by the base 
         bitmask type -- */
   pedRun = pvfi->pedList;
   while ( pedRun ) {
#ifdef TRACE_REDEF
      fprintf( stderr, "\tElt at idx (%s) being checked\n", 
                          pedRun->pchIndexName );
#endif
      RedefineBitmask( &(pedRun->valhdrValue), 
                       pvfi->ptfi->ptfiArrayEltType->ptfiBitmaskBaseType );
      pedRun = pedRun->pedNext;
   }
}

static void
RedefineVariable( stn_t *pstnType, valn_t *pvalnVar )
{
   stn_t *pstnVar;
   tfi_t *ptfiTmp;


   pstnVar = kapi_pstnLookup( kapi_pknobsCurrent, pvalnVar->val.pch ); 
   assert( pstnVar->ity == ityVARNAME );

   ptfiTmp = pstnVar->u.vfi.ptfi;
  
   switch ( pstnVar->u.vfi.ptfi->tty ) {
      case ttyARRAY: {
         tfi_t *ptfiTmpElt, *ptfiTmpIdx;

         ptfiTmpElt = ptfiTmp->ptfiArrayEltType;
         ptfiTmpIdx = ptfiTmp->ptfiArrayIndexType;

         if ( ptfiTmpElt->pstnIdent == pstnType ) {
            RedefineVariable_array_elt( pstnType, pstnVar );
         } 
         if ( ptfiTmpIdx->pstnIdent == pstnType ) {
            RedefineVariable_array_idx( pstnType, pstnVar );
         } 
         if ( ptfiTmpElt->tty == ttyBITMASK 
              && ptfiTmpElt->ptfiBitmaskBaseType->pstnIdent == pstnType ) {
            RedefineVariable_array_elt_bitmask( pstnType, pstnVar );
         } 
         break;
      }
      case ttyBITMASK: {
         if ( ptfiTmp->ptfiBitmaskBaseType->pstnIdent == pstnType ) {
            RedefineVariable_scalar_bitmask( pstnType, pstnVar );
         } 
         break;
      }
      default: {
         if ( ptfiTmp->pstnIdent == pstnType ) {
            RedefineVariable_scalar( pstnType, pstnVar );
         }
      }
   }

}


static void
RedefineType( stn_t *pstnType, valn_t *pvalnIdents, limp_t *plimpList )
{
   valn_t *pvalnVar;

   /* undefine all existing enum constants */
   pstnType->u.tfi.pvalnPreviousEnums = pvalnRemoveEnums( pstnType );
   pstnType->u.tfi.nEnumConst = 0;
   pstnType->u.tfi.tredefStatus = tredefSTATUS_ALREADY_REDEFINED;
 
   /* Redefine the type with the pstnIdents list */
   AddEnums( pstnType, pvalnIdents, 1 );

   /* Remove former enum names from symbol table */

   /* for each variable V affected by the change to type T ... */

   /* !! This can be put off until the end of the input files !! */

   pvalnVar = kapi_pknobsCurrent->pvalnVarList;
   while ( pvalnVar ) {
      RedefineVariable( pstnType, pvalnVar );
      pvalnVar = pvalnVar->pvalnNext;
   }
}

static int
fIdenticalEnumTypeDefinition( stn_t *pstnType, valhdr_t *pvalhdrIdents )
{
   return( fMatch_valn_list( pstnType->u.tfi.pvalnEnums, pvalhdrIdents->pvalnList, 
                             ttySTRING ) );
}


/* append enums at the end of a type enum that was already declared */
void kapi_ProcessAppendDecl( stn_t *pstnType, valhdr_t *pvalhdrIdents)
{
  valn_t *pvalnIdents;
   pvalnIdents = pvalhdrIdents->pvalnList;

  /* if new type then error (can only append to existing type */
   if ( (pstnType->ity != ityTYPENAME) ||
        ( pstnType->ity == ityTYPENAME && pstnType->fExpected ) ) {
          kapi_Error_pch1( kapi_cLine, 0, "Cannot append to undefined type '%s'.", pstnType->pchName );
		  return;
   }
   AppendEnums( pstnType, pvalnIdents, 0 );
   return;
}


void kapi_ProcessTypeDecl( stn_t *pstnType, valhdr_t *pvalhdrIdents, limp_t *plimpList )
{
   valn_t *pvalnIdents;
   /* limp_t *plimp; */
   valn_t *pvalnType;


   pvalnIdents = pvalhdrIdents->pvalnList;

   /* new type */
   if ( pstnType->ity == ityUNKNOWN ||
        ( pstnType->ity == ityTYPENAME && pstnType->fExpected ) ) {

      /* all of the idents must also be unknowns or error */
      DefineType( pstnType, pvalnIdents, plimpList );

      /* add to type list */
      pvalnType = pvalnMakeIdent( pstnType->pchName );

      pvalnType->pvalnNext = kapi_pknobsCurrent->pvalnTypeList;
      kapi_pknobsCurrent->pvalnTypeList = pvalnType;

   /* type redefinition */
   } else if ( pstnType->ity == ityTYPENAME ) {
       int fOk;

       fOk = fIdenticalEnumTypeDefinition( pstnType, pvalhdrIdents );

       /* 
          Note that if noredef is set or already redefined, then an 
          error is issued only if the values/types are not exactly 
          identical.  Also note that in these cases of exact redefinitions,
          the new definition is completely discarded.
       */
       switch ( pstnType->u.tfi.tredefStatus ) {
          case tredefSTATUS_OK: {
             RedefineType( pstnType, pvalnIdents, plimpList );
             break;
          }
          case tredefSTATUS_MARKED_NOREDEFINE: {
             if ( !fOk ) {
                 kapi_Error_pch1( kapi_cLine, 0, 
                          "Type %s marked as noredefine earlier, cannot define here", 
                           pstnType->pchName );
             }
             break;
          }
          case tredefSTATUS_ALREADY_REDEFINED: {
             if ( !fOk ) {
                kapi_Error_pch1( kapi_cLine, 0, 
                          "Type %s cannot be redefined due to earlier redefinition", 
                           pstnType->pchName );
             }
             break;
          }
          case tredefSTATUS_USED_IN_LIMIT: {
             if ( !fOk ) {
                kapi_Error_pch1( kapi_cLine, 0, 
                          "Type %s cannot be redefined -- its components were used in a limit statement earlier", 
                           pstnType->pchName );
             }
             break;
          }
          default: {
             assert( 0 );
          }
       }

   /* identifier already used -- maybe error */
   } else {
       kapi_Error_pch1( kapi_cLine, 0, "Identifier '%s' already defined, cannot reuse as typename", pstnType->pchName );
   }

   /* for KAPI2.x compatibility, rescan identifiers and compute
      number of units of each type and apply them to cluster 0 
      KAPI2X_update_cluster0Cutports( kapi_pknobsCurrent, pstnType ); */
   /*************** no compatibility supported!!! **************/
   if ( 0 == strcmp( pstnType->pchName, "port_t" ) ) {
	   kapi_Warning(0,0,"kapi 2.x port_t compatability not supported!");
   }
}
/* convert enum names to integer positions */
int
pstnEnum2idx( stn_t *pstnEnum )
{
   tfi_t *ptfiBaseType;
   valn_t *pvalnRun;
   int cnt;

   if ( pstnEnum == NULL || pstnEnum->pchName == NULL ) {
      return( -1 );
   }

   ptfiBaseType = pstnEnum->u.efi.ptfiBaseType;
   assert( ptfiBaseType->tty == ttyENUM );

   cnt = 0;
   pvalnRun = ptfiBaseType->pvalnEnums;
   while ( pvalnRun && strcmp( pstnEnum->pchName, pvalnRun->val.pch ) ) {
      cnt++;
      pvalnRun = pvalnRun->pvalnNext;
   }
   assert( pvalnRun );
   return( cnt );
}



void
kapi_ProcessAttrDecl( stn_t *pstnAttr, char *pchAttr )
{
   valn_t *pvalnAttr;

   /* new attr identifier */
   if ( pstnAttr->ity == ityUNKNOWN || 
        ( pstnAttr->ity == ityATTRIBUTENAME && pstnAttr->fExpected ) ) {
      pstnAttr->ity = ityATTRIBUTENAME;
      pstnAttr->fExpected = 0;
      pstnAttr->u.afih.u.pafi = NULL;
      pstnAttr->u.afih.nAttr = 0;
      AddStringToAttr( pstnAttr, pchAttr );

      /* add to attr list */
      pvalnAttr = pvalnMakeIdent( pstnAttr->pchName );
      pvalnAttr->pvalnNext = kapi_pknobsCurrent->pvalnAttrList;
      kapi_pknobsCurrent->pvalnAttrList = pvalnAttr;

   } else if ( pstnAttr->ity == ityATTRIBUTENAME ) {
      AddStringToAttr( pstnAttr, pchAttr );

   /* identifier already used -- error */
   } else {
      kapi_Error_pch1( kapi_cLine, 0, "%s already defined", pstnAttr->pchName );
   }
}

static int
fEqual_val( val_t *pval1, val_t *pval2, tty_t tty ) 
{
   switch( tty ) {
      case ttySTRING: {
         return( !strcmp( pval1->pch, pval2->pch ) );
         break;
      }
      case ttyINT: {
         return( pval1->i == pval2->i );
         break;
      }
      case ttyREAL: {
         return( pval1->r == pval2->r );
         break;
      }
      case ttyBITMASK: {
         return( fEqual_bv( &( pval1->bv ), &( pval2->bv ) ) );
         break;
      }
      default: {
         assert( 0 );
      }
   }
   return 0;
}

static int
fMatch_valn_list( valn_t *pvalnList1, valn_t *pvalnList2, tty_t tty )
{
    valn_t *pvalnRun1, *pvalnRun2;

    pvalnRun1 = pvalnList1;
    pvalnRun2 = pvalnList2;
    while ( pvalnRun1 != NULL && pvalnRun2 != NULL ) {
       if ( ! fEqual_val( &( pvalnRun1->val ), &( pvalnRun2->val ), tty ) ) {
          return( 0 ); 
       }

       pvalnRun1 = pvalnRun1->pvalnNext;
       pvalnRun2 = pvalnRun2->pvalnNext;
    }

    /* if they are not both null, fail */
    return( pvalnRun1 == pvalnRun2 );
}

static int 
fExactTypeMatch( tfi_t *ptfi1, tfi_t *ptfi2 )
{
#define RETURN_FAIL_IF( cond ) \
   if ( ( cond ) ) { \
      return( 0 ); \
   }

   RETURN_FAIL_IF( ptfi1->tty != ptfi2->tty );

   switch ( ptfi1->tty ) {
      case ttyENUM: {
         RETURN_FAIL_IF( ptfi1->nEnumConst != ptfi2->nEnumConst );
         return( fMatch_valn_list( ptfi1->pvalnEnums, ptfi2->pvalnEnums, ttySTRING ) );
         break;
      }
      case ttyARRAY: {
         int fOk;

         RETURN_FAIL_IF( ptfi1->tty != ptfi2->tty );
         fOk = fExactTypeMatch( ptfi1->ptfiArrayEltType, ptfi2->ptfiArrayEltType );
         return( fOk 
                 && fExactTypeMatch( ptfi1->ptfiArrayIndexType, 
                                     ptfi2->ptfiArrayIndexType ) );
         break;
      }
      case ttyBITMASK: {
         return( fExactTypeMatch( ptfi1->ptfiBitmaskBaseType, 
                                  ptfi2->ptfiBitmaskBaseType ) );
         break;
      }
 
      case ttyIDENT: {
         return( !strcmp( ptfi1->pstnIdent->pchName, ptfi2->pstnIdent->pchName ) );
         break;
      }

      case ttyINT:
      case ttySTRING: 
      case ttyREAL: {
         return( ptfi1->tty == ptfi2->tty );
         break;
      }

      default:  {
         assert( 0 );
         break;
      }
   }
   return 0;
}

static int
fIdenticalVarDecl( stn_t *pstnVar, tfi_t *ptfi )
{

   assert( pstnVar->ity == ityVARNAME );
   /* 
      Assume this var has already been defined once.

      In this case, the name must be the same already -- just check
      to make sure structure and contents of: 
  
         ptfi == pstnVar->u.vfi.ptfi;
   */

   return ( fExactTypeMatch( ptfi, pstnVar->u.vfi.ptfi ) );
}
  

void
kapi_ProcessVarDecl( stn_t *pstnVar, tfi_t *ptfi )
{
   valn_t *pvalnVar;

	/* first make sure this is a valid parameter */
	if (ptfi == NULL)
	{
	  kapi_Error(kapi_cLine,1,"Bad variable declaration!");
	  return;
	}

   /* new variable */
   if ( pstnVar->ity == ityUNKNOWN ||
        ( pstnVar->ity == ityVARNAME && pstnVar->fExpected == 1 ) ) {

      pstnVar->ity = ityVARNAME;
      pstnVar->fExpected = 0;
      pstnVar->u.vfi.ptfi = ptfi;

      /* add to var list */
      pvalnVar = pvalnMakeIdent( pstnVar->pchName );
      pvalnVar->pvalnNext = kapi_pknobsCurrent->pvalnVarList;
      kapi_pknobsCurrent->pvalnVarList = pvalnVar;


      /* compute number of elements */
      if ( ptfi->tty == ttyARRAY ) {
         assert( ptfi->ptfiArrayIndexType->pstnIdent->ity == ityTYPENAME );
         assert( ptfi->ptfiArrayIndexType->tty == ttyENUM || 
                 ptfi->ptfiArrayIndexType->tty == ttySTRING );
      }

      pstnVar->u.vfi.pedList = NULL;

   /* Var redefinition */
   } else if ( pstnVar->ity == ityVARNAME ) {
       int fOk;

       fOk = fIdenticalVarDecl( pstnVar, ptfi );
       
       /* 
          Note that if variable is already defined, then an 
          error is issued only if the values/types are not exactly 
          identical.  Also note that in these cases of exact redefinitions,
          the new definition is completely discarded.
       */
       if ( !fOk ) {
          kapi_Error_pch1( kapi_cLine, 0, "Variable %s already defined",  
                           pstnVar->pchName );
       }

   /* identifier already used -- maybe error */
   } else {
       kapi_Error_pch1( kapi_cLine, 0, "Identifier '%s' already defined, cannot reuse as variable name", pstnVar->pchName );
   }
}

void
TurnOnNoRedefine( tfi_t *ptfiBase, tredef_t tredef )
{
   if ( ptfiBase->tredefStatus == tredefSTATUS_OK ) {
      ptfiBase->tredefStatus = tredef;
   }
}

static int
iEnumUpperGetRange( valhdr_t *pvalhdr )
{
   valn_t *pvalnLower, *pvalnUpper;
   stn_t *pstnUpper;
   int iUpper;

   pvalnLower = pvalhdr->pvalnList;
   if ( pvalnLower == NULL || pvalnLower->pvalnNext == NULL ) {
               kapi_Error( kapi_cLine, 0, "Range specification bad" );
   }
   pvalnUpper = pvalnLower->pvalnNext;
            
   pstnUpper = kapi_pstnLookup( kapi_pknobsCurrent, pvalnUpper->val.pch );

   iUpper = pstnEnum2idx( pstnUpper );

   return( iUpper );
}

static int
iEnumLowerGetRange( valhdr_t *pvalhdr )
{
   valn_t *pvalnLower;
   stn_t *pstnLower;
   int iLower;

   pvalnLower = pvalhdr->pvalnList;
   if ( pvalnLower == NULL || pvalnLower->pvalnNext == NULL ) {
               kapi_Error( kapi_cLine, 0, "Range specification bad" );
   }
            
   pstnLower = kapi_pstnLookup( kapi_pknobsCurrent, pvalnLower->val.pch );

   iLower = pstnEnum2idx( pstnLower );

   return( iLower );
}

static int
iUpperGetRange( valhdr_t *pvalhdr )
{
   valn_t *pvalnLower, *pvalnUpper;
   /* int iUpper; */

   pvalnLower = pvalhdr->pvalnList;
   if ( pvalnLower == NULL || pvalnLower->pvalnNext == NULL ) {
               kapi_Error( kapi_cLine, 0, "Range specification bad" );
   }
   pvalnUpper = pvalnLower->pvalnNext;
            
   return( pvalnUpper->val.i );
}

static int
iLowerGetRange( valhdr_t *pvalhdr )
{
   valn_t *pvalnLower;
   /* int iLower; */

   pvalnLower = pvalhdr->pvalnList;
   if ( pvalnLower == NULL || pvalnLower->pvalnNext == NULL ) {
               kapi_Error( kapi_cLine, 0, "Range specification bad" );
   }
            
   return( pvalnLower->val.i );
}

void
ProcessNewLimits( stn_t *pstnLHSVar, limp_t *plimpNew )
{
   tfi_t *ptfiBase;
   vfi_t *pvfiLHS;
   int  fFailSublimit;


   pvfiLHS = &( pstnLHSVar->u.vfi );

   /* figure out the base type */
   if ( pvfiLHS->ptfi->tty == ttyARRAY ) {
      ptfiBase = pvfiLHS->ptfi->ptfiArrayEltType;
   } else {
      ptfiBase = pvfiLHS->ptfi;
   }

   /* 
      Convert idents to enums and check to 
      make sure they are enums 
   */
   if ( plimpNew->valhdrValues.tty == ttyIDENT ) {
      switch ( plimpNew->valhdrValues.vals ) {
         case valsLIST: {
            int fError;
            valn_t *pvalnRun;

            pvalnRun = plimpNew->valhdrValues.pvalnList;
            fError = 0;
            while ( pvalnRun ) {
               stn_t *pstnTmp;
          
               pstnTmp = kapi_pstnLookup_noadd( kapi_pknobsCurrent, pvalnRun->val.pch );
               if ( (!pstnTmp) || pstnTmp->ity != ityENUMCONST ) {
                  fError = 1;
               } else {
                  if ( pstnTmp->u.efi.ptfiBaseType != ptfiBase ) {
                     fError = 1;
                  }
               }
               pvalnRun = pvalnRun->pvalnNext;
            }

            if ( !fError ) {
               plimpNew->valhdrValues.tty = ttyENUM;
            } else {
               kapi_Error( kapi_cLine, 0, "Limit value expression not correct" );
            }
            break;
         }
         case valsRANGE: {
            int iUpper, iLower;

            iLower = iEnumLowerGetRange( &( plimpNew->valhdrValues) );
            iUpper = iEnumUpperGetRange( &( plimpNew->valhdrValues) );

            if ( iLower > iUpper ) {
               kapi_Error( kapi_cLine, 0, "Invalid range specification" );
            }
            plimpNew->valhdrValues.tty = ttyENUM;
            
            break;
         }
         case valsNOREDEFINE:
            break;
         case valsSCALAR:
            break;
         default:
            assert( 0 );
      } 
   } else if ( plimpNew->valhdrValues.tty == ttyINT
               && plimpNew->valhdrValues.vals == valsRANGE ) {
      valn_t *pvalnLower, *pvalnUpper;
      /* check integer ranges for consistency */

      pvalnLower = plimpNew->valhdrValues.pvalnList;
      if ( pvalnLower == NULL || pvalnLower->pvalnNext == NULL ) {
         kapi_Error( kapi_cLine, 0, "Range specification bad" );
      }
      pvalnUpper = pvalnLower->pvalnNext;

      if ( pvalnLower->val.i > pvalnUpper->val.i ) {
         kapi_Error( kapi_cLine, 0, "Invalid range specification" );
      }
   }

   /* bitmask types cannot be value limited */
   if ( ptfiBase->tty == ttyBITMASK && ! plimpNew->valhdrValues.fNoRedefine ) {
      kapi_Error_pch1( kapi_cLine, 0, 
                 "Cannot value-limit variables (%s) of type bitmask or array of bitmask", 
                 pstnLHSVar->pchName );
   } else {
      /* 
         Disallow redefinition of the base type of this variable since it is used
         in a limit statement.
      */
      TurnOnNoRedefine( ptfiBase, tredefSTATUS_USED_IN_LIMIT );
   }

   /* check to make sure type of values in limit match type of variable */
   if ( plimpNew->valhdrValues.tty != ptfiBase->tty 
        && plimpNew->valhdrValues.vals != valsNOREDEFINE ) {
      kapi_Error_pch1( kapi_cLine, 0, 
                 "Types in limit statment of '%s' do not match variable type", 
                 pstnLHSVar->pchName );
   }

   /* 
      If there were prior limits,
      check to make sure that this limit defines a subset of any 
      previous limit statement.
   */
   if ( pvfiLHS->pedList ) {
      /* 
          1) Integer list
          2) Enum list
          3) Integer range
          4) Enum range
          5) Real list
          6) String list
      */
      fFailSublimit = 0;
      if ( plimpNew->valhdrValues.vals == valsLIST ) {
          valn_t *pvalnRun;
   
          pvalnRun = plimpNew->valhdrValues.pvalnList;
          while ( (!fFailSublimit) && pvalnRun ) {
             fFailSublimit = fFailCheckLimitValue2( pvalnRun, 
                                                    &(pvfiLHS->pedList->valhdrLimit));
             pvalnRun = pvalnRun->pvalnNext;
          }
   
      } else if ( plimpNew->valhdrValues.vals == valsRANGE ) {
         int ival, iLower, iUpper;
         valn_t valnTmp;

         if ( plimpNew->valhdrValues.tty == ttyINT ) {
            iLower = iLowerGetRange( &(plimpNew->valhdrValues ) );
            iUpper = iUpperGetRange( &(plimpNew->valhdrValues ) );
         } else if ( plimpNew->valhdrValues.tty == ttyENUM) {
            iLower = iEnumLowerGetRange( &(plimpNew->valhdrValues ) );
            iUpper = iEnumUpperGetRange( &(plimpNew->valhdrValues ) );
         } else {
            assert( 0 );
         }
      
         valnTmp.pvalnNext = NULL;
         for ( ival=iLower; ival<=iUpper && !fFailSublimit; ival++ ) {
            valnTmp.val.i = ival;
            fFailSublimit = fFailCheckLimitValue2( &(valnTmp), 
                                                   &(pvfiLHS->pedList->valhdrLimit));
         }
      }

      if ( fFailSublimit ) {
         kapi_Error_pch1( kapi_cLine, 0, 
                         "Limits on variable '%s' must be a subset of previous limits",
                         pstnLHSVar->pchName );
      }
   }
}

void
CheckTypeConformance( vfi_t *pvfiLHS, char *pchIndex, int fIndexAll, valhdr_t *pvalhdrRHS )
{
   tfi_t *ptfiLHS;

   if ( pvfiLHS->ptfi->tty == ttyARRAY ) {
      stn_t *pstnIndex;

      ptfiLHS = pvfiLHS->ptfi->ptfiArrayEltType;
      if ( pchIndex != NULL && !fIndexAll ) {
         if ( pchIndex[ 0 ] != '%' ) {
            pstnIndex = kapi_pstnLookup_noadd( kapi_pknobsCurrent, pchIndex );

            if ( pstnIndex == NULL || pstnIndex->ity != ityENUMCONST ) {
               kapi_Error_pch1( kapi_cLine, 0, 
                     "Array index '%s' is not an enumerated type constant",
                     pchIndex );
            } else {
               if ( pstnIndex->u.efi.ptfiBaseType != pvfiLHS->ptfi->ptfiArrayIndexType ) {
                  kapi_Error_pch1( kapi_cLine, 0, 
                      "Array index '%s' is not the right type of enumeration constant",
                      pchIndex );
               }
            }
         }
      }
   } else {
      ptfiLHS = pvfiLHS->ptfi;
   }

   /* check type compatibility */
   if ( pvalhdrRHS->tty != ptfiLHS->tty ) {
      kapi_Error( kapi_cLine, 0, 
                  "LHS and RHS of assignment not type compatible" );
      return;
   } 
}

void
ProcessArrayAssign( vfi_t *pvfiLHS, stn_t *pstnLHSVar, char *pchIndex, 
                    int fIndexAll, valhdr_t *pvalhdrRHS, 
                    limp_t *plimpNew, int fIdenticalVal )
{
   if ( pchIndex == NULL ) {
      kapi_Error_pch1( kapi_cLine, 0, 
                 "Identifier %s declared as an array, but not indexed", 
                 pstnLHSVar->pchName );
      return;
   }

   assert( pvfiLHS->ptfi->ptfiArrayIndexType->tty == ttyENUM ||
           pvfiLHS->ptfi->ptfiArrayIndexType->tty == ttySTRING );

   if ( !fIdenticalVal ) {
      if ( fIndexAll ) {
         ed_t *pedRun;
         ed_t *pedGlobal;
   
         /* assign all values */
         pedRun = pvfiLHS->pedList;
         while ( pedRun ) {
            if ( pedRun->valhdrLimit.fNoRedefine ) {
               /* char *pchEnum; */
               tfi_t *ptfi;
   
               if ( !fIdenticalVal ) {
                  ptfi = pvfiLHS->ptfi->ptfiArrayIndexType;
                  kapi_Warning_pch2( kapi_cLine, 0, 
                       "%s[ %s ] cannot be redefined", 
                       pstnLHSVar->pchName, pedRun->pchIndexName );
               }
            } else {
   
               pvalhdrCopy( &(pedRun->valhdrValue), pvalhdrRHS );
               SetVariableValueLimit( &(pedRun->valhdrLimit), plimpNew );
            }
            pedRun = pedRun->pedNext;
         }
   
         pedGlobal = pedLookup4ped( pvfiLHS->pedList, "*" );
         
         /* set global default */
         if ( pedGlobal == NULL ) {
            pedGlobal = pedMake( "*", pvfiLHS->ptfi->ptfiArrayEltType );
            pedGlobal->pedNext = pvfiLHS->pedList;
            pvfiLHS->pedList = pedGlobal;
         }
   
         pvalhdrCopy( &(pedGlobal->valhdrValue), pvalhdrRHS );
   
      } else {
         ed_t *ped;
   
         /* assign the value */
         ped = pedLookup4pstn( pstnLHSVar, pchIndex );
         if ( ped == NULL ) {
            ped = pedMake( pchIndex,
                           pvfiLHS->ptfi->ptfiArrayEltType );
            ped->pedNext = pstnLHSVar->u.vfi.pedList;
            pstnLHSVar->u.vfi.pedList = ped;
   
         } 
   
         if ( ped->valhdrLimit.fNoRedefine ) {
            /* char *pchEnum; */
            tfi_t *ptfi;
   
            ptfi = pvfiLHS->ptfi->ptfiArrayIndexType;
            kapi_Error_pch2( kapi_cLine, 0, 
                       "%s[ %s ] cannot be redefined", 
                       pstnLHSVar->pchName, pchIndex  );
         } else {
            pvalhdrCopy( &(ped->valhdrValue), pvalhdrRHS );
            SetVariableValueLimit( &(ped->valhdrLimit), plimpNew );
         }
      }
   }


   if ( 0 == strcmp( pstnLHSVar->pchName, "PortTypes" ) ) {
	   kapi_Warning(0,0,"kapi 2.x port_t compatability not supported!");
   /* now interpret variables to allow for KAPI2x compatibility not supported!
      KAPI2X_checkPortTypes( kapi_pknobsCurrent, pstnLHSVar, 
                             pchIndex, pvalhdrRHS ); 
   } else if ( 0 == strcmp( pstnLHSVar->pchName, "cluster0Cutports" ) ) {
      KAPI3X_update_cluster0Cutports( kapi_pknobsCurrent, pstnLHSVar,
                             pchIndex, pvalhdrRHS );
   */} 
}

/* 
   Given a redefinition of port_t in KAPI2x, translate actions
   into those required for KAPI3X (cluster0Cutports)
 */
static void
KAPI2X_update_cluster0Cutports( knobs_t *pknobs, stn_t *pstnType )
{
   valn_t *pvalnRun;
   kapi_ut_t ut,utPrev;
   int iport;
   int mpnmaxcutport[ kapi_nUT ];

   /* clear out old port counts */
   pknobs->mpclrTable[ 0 ].ncports = 0;
   for ( ut=0;ut<kapi_nUT;ut++ ) {
      mpnmaxcutport[ ut ] = 0;
      pknobs->mpclrTable[ 0 ].mpncutport[ ut ] = 0;
   }

   /* run through and count up new ones */
   pvalnRun = pstnType->u.tfi.pvalnEnums;
   utPrev=-1;
   while ( pvalnRun ) {
      ut = utInterpretPortName( pvalnRun->val.pch, &iport );
	  if (ut<utPrev)
	  {
		  kapi_Warning_pch1(kapi_cLine, 0, 
               "KAPI 2.x declaration of port_t for enum %s does not follow ut_t order!", 
               pvalnRun->val.pch );
	  } else
		  utPrev=ut;

      if ( ut == -1 ) {
         kapi_Error_pch1( kapi_cLine, 0, 
               "Cannot translate KAPI 2.x declaration of port_t for enum %s", 
               pvalnRun->val.pch );
      } else {
         (pknobs->mpclrTable[ 0 ].mpncutport[ ut ])++;
         if ( mpnmaxcutport[ ut ] < iport ) {
            mpnmaxcutport[ ut ] = iport;
         }
      }
      pvalnRun = pvalnRun->pvalnNext;
   }

   for ( ut=0;ut<kapi_nUT;ut++ ) {
      /* this insures that all ports expected exist */
      if ( mpnmaxcutport[ ut ]+1 != pknobs->mpclrTable[ 0 ].mpncutport[ ut ] ) {
         kapi_Error( kapi_cLine, 0, 
               "Cannot translate KAPI 2.x declaration of port_t due to missing port" );
         
      }
   }
}

/* 
 * Given a cluster0Cutports assignment, update cluster information
 */
static void 
KAPI3X_update_cluster0Cutports( knobs_t *pknobs, stn_t *pstnArray, 
                                 char *pchIndex, valhdr_t *pvalhdrRHS )
{
   kapi_ut_t ut;
   int ntmp;
   kapi_cutport_t cutport;

   ut = KAPI_EnumIndex( pknobs, "ut_t", pchIndex );
 
   if ( ut == -1 ) {
      kapi_Error_pch1( kapi_cLine, 0, 
            "cluster0Cutports must be indexed by type ut_t. Found  %s", 
            pchIndex );
   } 

   if ( pvalhdrRHS->tty != ttyINT ) {
      kapi_Error_pch1( kapi_cLine, 0, 
            "cluster0Cutports[ %s ] requires integer values", 
            pchIndex );
   }

   ntmp = pvalhdrRHS->pvalnList->val.i;
   pknobs->mpclrTable[ 0 ].mpncutport[ ut ] = ntmp;

   for ( cutport=0; cutport<ntmp; cutport++ ) {
       cportinfo_t *pcportinfo;

       pcportinfo = &(pknobs->mpclrTable[ 0 ].dmppcportinfoTable[ ut ][ cutport ]);
       if ( BYTE_LENGTHbv( &(pcportinfo->bvfuAllowed ) ) ==  0) {
          bvResize( &(pcportinfo->bvfuAllowed), 256, 0 );
          ZERO_bv( &(pcportinfo->bvfuAllowed) );
       }
   }
}


static void 
KAPI2X_checkPortTypes( knobs_t *pknobs, stn_t *pstnArray, 
                     char *pchIndex, valhdr_t *pvalhdrRHS )
{
   kapi_ut_t utIndex, utRHS;
   int iport;

   if ( pvalhdrRHS->tty != ttyENUM ) {
      printf("ERROR -- RHS of PortTypes assignment not an Enum\n"); 
   }
   utRHS = KAPI_EnumIndex( pknobs, "ut_t", pvalhdrRHS->pvalnList->val.pch );
   utIndex = utInterpretPortName( pchIndex, &iport );

   if ( utRHS != utIndex ) {
      kapi_Error_pch1( kapi_cLine, 0, 
             "Error translating KAPI2x PortTypes[ %s ] assignment", 
             pchIndex );
   }
}


void
ProcessScalarAssign( vfi_t *pvfiLHS, stn_t *pstnLHSVar, char *pchIndex, 
                     int fIndexAll, valhdr_t *pvalhdrRHS, limp_t *plimpNew,
                     int fIdenticalVal )
{
   if ( pchIndex != NULL && pvfiLHS->ptfi->tty != ttyARRAY ) {
      kapi_Error_pch1( kapi_cLine, 0, "Identifier %s used as an array, but not declared as one", pstnLHSVar->pchName );
      return;
   }

   if ( !fIdenticalVal ) {
      if ( pvfiLHS->pedList == NULL ) {
         pvfiLHS->pedList = pedMake( "", pvfiLHS->ptfi );
      }
   
      if ( pvfiLHS->pedList
           && pvfiLHS->pedList->valhdrLimit.fNoRedefine ) {
          kapi_Error_pch1( kapi_cLine, 0, 
                           "%s cannot be redefined", 
                           pstnLHSVar->pchName );
      } else {
         pvalhdrCopy( &(pvfiLHS->pedList->valhdrValue), pvalhdrRHS );
         SetVariableValueLimit( &(pvfiLHS->pedList->valhdrLimit), plimpNew );
      }
   }
}

static int
fIdenticalValues( stn_t *pstnLHSVar, char *pchIndex, int fIndexAll, valhdr_t *pvalhdrRHS )
{
   int fIdentical;
   vfi_t *pvfiLHS;

   pvfiLHS = &(pstnLHSVar->u.vfi);
   if ( pvfiLHS->ptfi->tty == ttyARRAY ) {
      if ( fIndexAll ) {  /* all entries assigned */
         ed_t *pedRun;

         pedRun = pvfiLHS->pedList;

         fIdentical = 1;
         while ( pedRun && fIdentical ) {
            fIdentical = fCheckIdenticalVal( pvalhdrRHS, pedRun );
            pedRun = pedRun->pedNext;
         }

         /* 
            gross hack -- if no values were stored in this array yet, 
            then fIdentical should always be false 
         */
         if ( pvfiLHS->pedList == NULL ) {
            fIdentical = 0;
         }
      } else {               /* find one entry to check */
         ed_t *ped;

         ped = pedLookup4pstn( pstnLHSVar, pchIndex );
         fIdentical = fCheckIdenticalVal( pvalhdrRHS, ped );
      }
   } else { /* scalar */
      fIdentical = fCheckIdenticalVal( pvalhdrRHS, pvfiLHS->pedList );
   }

   return( fIdentical );
}



/* The valhdr hold a linked list of values and operators,
   (in reverse order of appearance).
   This list is computed as an expression 
   * Currently handles only integer expressions!
*/
void kapi_ProcessArithmeticAssignment( stn_t *pstnLHSVar, char *pchIndex, 
                        valhdr_t *pvalhdrRHS)
{ 
   int iVal=0;				/* holds the result of the expression */
   valn_t *pvalnExpression; /* pointer to the expression */
   arithmetic_op_t op;	    /* operands in the expression are put here */
   valhdr_t valhdrResult;

   assert(pvalhdrRHS!=NULL);
   assert(pstnLHSVar!=NULL);

   assert (pvalhdrRHS->tty==ttyEXPRESSION);
   assert (pvalhdrRHS->vals==valsLIST);

   /* init stage for expression eval */
   pvalnExpression=pvalhdrRHS->pvalnList;
   /* now reverse the list since the expression was parsed LTR */
   pvalnExpression=kapi_pvalnReverseTmp(pvalnExpression);
   /* and get the first value from the chain */
   iVal=pvalnExpression->val.i;



   while ((pvalnExpression=pvalnExpression->pvalnNext)!=NULL)
   {
	   /* The next values in the chain always consist of a pair of operand & value */
	    op=pvalnExpression->val.enu;
		pvalnExpression=pvalnExpression->pvalnNext;
		switch (op)
		{
		case opPLUS:
			iVal+=pvalnExpression->val.i;
			break;
		case opDIV:
			iVal/=pvalnExpression->val.i;
			break;
		default:
            kapi_Error_pch1( kapi_cLine, 0, 
            "Arithmetic expression error evaluating '%s' (unidentified operand)", 
            pstnLHSVar->pchName );
			break;
		}
   }

   /* now create an value header with result of expression and assign to LHS */
   valhdrResult.tty=ttyINT;
   valhdrResult.vals=valsSCALAR;
   valhdrResult.pvalnList=pvalnMakeInt(iVal);
   kapi_ProcessAssignment(pstnLHSVar,pchIndex,&valhdrResult,NULL);

} /* of kapi_ProcessArithmeticAssignment */


void
kapi_ProcessAssignment( stn_t *pstnLHSVar, char *pchIndex, 
                        valhdr_t *pvalhdrRHS, limp_t *plimpNewList )
{
   vfi_t *pvfiLHS;
   limp_t *plimpNew;
   valn_t *pvalnVar;
   int   fIndexAll, fIdentical;


   /* chck for global index */
   fIndexAll = pchIndex && ( ! strcmp( pchIndex, "*" ) );

   pvfiLHS = &( pstnLHSVar->u.vfi );

   if ( pstnLHSVar->ity == ityUNKNOWN 
        && kapi_pknobsCurrent->fImplicitNone ) {
      kapi_Error_pch1( kapi_cLine, 0, 
            "Undeclared variable used in assignment LHS '%s'", 
            pstnLHSVar->pchName );
       return;
   }



   if ( pstnLHSVar->ity == ityUNKNOWN ||
        ( pstnLHSVar->ity == ityVARNAME && pstnLHSVar->fExpected ) ) {
      pstnLHSVar->ity = ityVARNAME;
      pstnLHSVar->fExpected = 0;

      if ( pvalhdrRHS->tty == ttyINT
              || pvalhdrRHS->tty == ttyREAL 
              || pvalhdrRHS->tty == ttySTRING 
              || pvalhdrRHS->tty == ttyENUM ) {

         if ( pvalhdrRHS->tty == ttyENUM ) {
            stn_t *pstn;
            pstn = kapi_pstnLookup( kapi_pknobsCurrent, 
                                    pvalhdrRHS->pvalnList->val.pch );
            if ( pstn->ity == ityENUMCONST ) {
               pvfiLHS->ptfi = pstn->u.efi.ptfiBaseType;
            } else {
               kapi_Error_pch1( kapi_cLine, 0, 
                         "RHS enumerated value '%s' does not match LHS", 
                         pstn->pchName );
            }
         } else {
            pvfiLHS->ptfi = kapi_ptfiLookup( pvalhdrRHS->tty, NULL );
         }
         pvfiLHS->pedList = NULL;

         /* add to var list */
         pvalnVar = pvalnMakeIdent( pstnLHSVar->pchName );
         pvalnVar->pvalnNext = kapi_pknobsCurrent->pvalnVarList;
         kapi_pknobsCurrent->pvalnVarList = pvalnVar;
      } else {
         kapi_Error_pch1( kapi_cLine, 0, 
             "Identifier %s not declared ", 
                       pstnLHSVar->pchName );
         return;
      }
   }

   /* am I a variable being assigned? */
   if ( pstnLHSVar->ity != ityVARNAME ) {
      kapi_Error_pch1( kapi_cLine, 0, 
             "Identifier '%s' already defined, cannot reuse as variable name", 
                       pstnLHSVar->pchName );
      return;
   }

   /* 
      Check type conformance 
   */
   CheckTypeConformance( pvfiLHS, pchIndex, fIndexAll, pvalhdrRHS );

   fIdentical = fIdenticalValues( pstnLHSVar, pchIndex, fIndexAll, pvalhdrRHS );

   /* 
      Check existing limit statements relevant to this tool 
   */
   if ( !fIdentical ) {
	   /* First, warn if the value was used on RHS of some expression */

      if ( pvfiLHS->ptfi->tty == ttyARRAY ) {
         if ( fIndexAll ) {  /* all entries assigned */
            ed_t *pedRun;
   
            pedRun = pvfiLHS->pedList;
            while ( pedRun ) {
               (void)fFailCheck( pvalhdrRHS, pedRun, pstnLHSVar, pedRun->pchIndexName );
               pedRun = pedRun->pedNext;
            }
         } else {               /* find one entry to check */
            ed_t *ped;

            ped = pedLookup4pstn( pstnLHSVar, pchIndex );
			if ((NULL!=ped) && (ped->valhdrValue.fRHS))
				kapi_Warning_pch2( kapi_cLine, 0, "Variable '%s[%s]' was already used in an expression!\n", pstnLHSVar->pchName,pchIndex);
            (void)fFailCheck( pvalhdrRHS, ped, pstnLHSVar, pchIndex );
         }
      } else { /* scalar */
		 if (pstnLHSVar->fRHS)
			kapi_Warning_pch1( kapi_cLine, 0, "Variable '%s' was already used in an expression!\n", pstnLHSVar->pchName);
         (void)fFailCheck( pvalhdrRHS, pvfiLHS->pedList, pstnLHSVar, "" );
      }
   }

   /* 
      Check and process new limits (if any) placed on the assignment
   */
   plimpNew = plimpLookup( plimpNewList, kapi_pknobsCurrent->pchToolname );
   if ( plimpNew ) {
      ProcessNewLimits( pstnLHSVar, plimpNew );
   }

   /* 
      Process assignment based on type of variable 
   */
   if ( pvfiLHS->ptfi->tty == ttyARRAY ) {
      ProcessArrayAssign( pvfiLHS, pstnLHSVar, pchIndex, fIndexAll, 
                           pvalhdrRHS, plimpNew, fIdentical );
   } else {
      ProcessScalarAssign( pvfiLHS, pstnLHSVar, pchIndex, fIndexAll, 
                           pvalhdrRHS, plimpNew, fIdentical );
   }
}

static int
fCheckIdenticalVal( valhdr_t *pvalhdrRHS, ed_t *pedLHSVal )
{
   /* valhdr_t valhdrLHS; */

   if ( pedLHSVal == NULL ) { 
      return( 0 );
   }

   if ( pedLHSVal->valhdrValue.vals == valsSCALAR ) {
      tty_t ttyCheck;

      ttyCheck = pedLHSVal->valhdrValue.tty;
      /* 
         fEqual_val only uses 'primitive' C types - Enums are
         stored as Strings.
      */
      if ( ttyCheck == ttyENUM ) {
         ttyCheck = ttySTRING;
      } 
      if ( ttyCheck == ttyBITMASK ) {
         return( fMatch_valn_list( pedLHSVal->valhdrValue.pvalnList,
                                   pvalhdrRHS->pvalnList,
                                   ttySTRING ) );
      } else { 
         return( fEqual_val( &(pedLHSVal->valhdrValue.pvalnList->val),
                          &(pvalhdrRHS->pvalnList->val),
                          ttyCheck ) );
      }
   } else if ( pedLHSVal->valhdrValue.vals == valsUNSET ) {
      return( 0 );
   } else {
      assert( 0 );
   }
   assert( 0 );
   return 0;
}

static int
fFailCheck( valhdr_t *pvalhdrRHS, ed_t *pedLHSVal, stn_t *pstnLHSVar, char *pchIndex )
{
   int fFail;
   vfi_t *pvfiLHS;


   if ( pedLHSVal == NULL ) { 
      return( 0 );
   }
   pvfiLHS = &(pstnLHSVar->u.vfi);

   
   fFail = fFailCheckLimitValue( pvalhdrRHS, pedLHSVal );
   if ( fFail ) {
      char *pchError;

      switch( fFail ) {
         case 1: 
            pchError = "cannot be redefined";
            break;
         case 2:
            pchError = "redefined to unallowable value";
            break;
         default: 
            assert( 0 );
      }
      if ( pvfiLHS->ptfi->tty == ttyARRAY ) {
         /* char *pchEnum; */
         tfi_t *ptfi;

         ptfi = pvfiLHS->ptfi->ptfiArrayIndexType;
         kapi_Error_pch3( kapi_cLine, 0, 
                    "%s[ %s ] %s", 
                    pstnLHSVar->pchName, pchIndex, pchError );
      } else {
         kapi_Error_pch2( kapi_cLine, 0, 
                    "%s %s", 
                    pstnLHSVar->pchName, pchError );
      }
   }
   return( fFail );
}

char *
ity2pchname( ity_t ity )
{
   char *pchity;

   switch ( ity ) {
      case ityTYPENAME:
         pchity = "type name";
         break;
      case ityVARNAME:
         pchity = "variable name";
         break;
      case ityENUMCONST:
         pchity = "enumerated constant";
         break;
      case ityATTRIBUTENAME:
         pchity = "attribute name";
         break;
      default:
         assert( 0 );
   }
   return( pchity );
}

void
kapi_ProcessExpect( valhdr_t *pvalhdr, int typeexpect )
{
   stn_t *pstn;
   valn_t *pvalnRun;
   ity_t ityNew;

   switch ( typeexpect ) {
      case 0:  
         ityNew = ityTYPENAME;
         break;
      case 1:
         ityNew = ityVARNAME;
         break;
      case 2:
         ityNew = ityENUMCONST;
         break;
      case 3:
         ityNew = ityATTRIBUTENAME;
         break;
      default:
         assert( 0 );
   }

   pvalnRun = pvalhdr->pvalnList;
   while ( pvalnRun ) {
       pstn = kapi_pstnLookup( kapi_pknobsCurrent, pvalnRun->val.pch );
       if ( pstn->ity == ityUNKNOWN ) {
          pstn->fExpected = 1;
          pstn->ity = ityNew;
       } else if ( pstn->ity != ityNew ) {
          if ( pstn->fExpected ) {
              kapi_Error_pch2( kapi_cLine, 1, 
                           "Identifier %s expected to be a %s",
                           pstn->pchName, ity2pchname( pstn->ity ) );
          } else {
              kapi_Error_pch2( kapi_cLine, 1, 
                           "Identifier %s previously declared to be a %s",
                           pstn->pchName, ity2pchname( pstn->ity ) );
          }
       }

       pvalnRun = pvalnRun->pvalnNext;
   }
}

int
kapi_ProcessIndex( stn_t *pstnIndex )
{
    if ( pstnIndex->ity != ityENUMCONST ) {
       kapi_Error_pch1( kapi_cLine, 0, 
                 "Index value %s must be an enumerated constant or '*'", 
                 pstnIndex->pchName );
       return( -1 );
    }

    return( pstnEnum2idx( pstnIndex ) );
}

tfi_t *
kapi_ptfiConstructArray( tfi_t *ptfiIndexType, tfi_t *ptfiElt )
{
   tfi_t *ptfiTemp;

   ptfiTemp = (tfi_t *)malloc( sizeof( tfi_t ) );

   ptfiTemp->tty = ttyARRAY;

   ptfiTemp->ptfiArrayEltType = ptfiElt;
   ptfiTemp->ptfiArrayIndexType = ptfiIndexType;

   return( ptfiTemp );
}

tfi_t *
kapi_ptfiConstructBitmask( tfi_t *ptfiBaseType )
{
   tfi_t *ptfiTemp;

   ptfiTemp = (tfi_t *)malloc( sizeof( tfi_t ) );

   ptfiTemp->tty = ttyBITMASK;
   ptfiTemp->ptfiBitmaskBaseType = ptfiBaseType;

   return( ptfiTemp );
}

tfi_t *
kapi_ptfiCheckDeclaredType( stn_t *pstnName )
{
   if ( pstnName->ity == ityUNKNOWN ) {
      kapi_Error_pch1( kapi_cLine, 0, "Typename %s not declared", pstnName->pchName );
      return( NULL );
   }

   if ( pstnName->ity != ityTYPENAME ) {
      kapi_Error_pch1( kapi_cLine, 0, "Expected typename, found %s", pstnName->pchName );
      return( NULL );
   }

   return( &(pstnName->u.tfi) );
}

tfi_t *
kapi_ptfiLookup( tty_t tty, void *pvoid )
{
  stn_t *pstn;
  char *pchTypeName;

  switch( tty ) {
    case ttyINT:
       pchTypeName = "int";
       break;
    case ttyREAL:
       pchTypeName = "real";
       break;
    case ttySTRING:
       pchTypeName = "string";
       break;
    case ttyIDENT:
       pchTypeName = ((stn_t *)pvoid)->pchName;
       break;
    default:
       assert( 0 );
  }
  pstn = kapi_pstnLookup( kapi_pknobsCurrent, pchTypeName );
  assert( pstn );

  switch ( pstn->ity ) {
     case ityTYPENAME:
        return( &(pstn->u.tfi) );
     case ityVARNAME:
        return( pstn->u.vfi.ptfi );
     case ityENUMCONST:
        return( pstn->u.efi.ptfiBaseType );
     case ityRESERVED:
     case ityATTRIBUTENAME:
     case ityUNKNOWN:
     default: 
        assert( 0 );
  }
  return 0;
}

void
kapi_LookUpVariable_valhdr( stn_t *pstn, valhdr_t *pvalhdr, int iPos )
{
   ed_t *ped;

   assert( pstn->ity == ityVARNAME );

   if ( pstn->u.vfi.ptfi->tty == ttyARRAY ) {

      ped = pedGetIth( pstn, iPos );
      if ( ped ) {
         *pvalhdr = ped->valhdrValue;
      } else {
         ped = pedLookup4pstn( pstn, "*" );
         if ( ped ) {
            *pvalhdr = ped->valhdrValue;
         } else {
            pvalhdr->vals = valsUNSET;
         }
      }
   } else {
      if ( pstn->u.vfi.pedList ) {
         *pvalhdr = pstn->u.vfi.pedList->valhdrValue;
      } else {
         ped = pedLookup4pstn( pstn, "*" );
         if ( ped ) {
            *pvalhdr = ped->valhdrValue;
         } else {
            pvalhdr->vals = valsUNSET;
         }
      }
   }
}

void
kapi_LookUpIdent_valhdr( stn_t *pstn, valhdr_t *pvalhdr, int iPos )
{
   switch( pstn->ity ) {
      case ityVARNAME:
         if ( pstn->u.vfi.ptfi->tty == ttyARRAY ) {
            ed_t *ped;
 
            ped = pedGetIth( pstn, iPos );
            if ( ped ) {
               *pvalhdr = ped->valhdrValue;
            } else {
               pvalhdr->vals = valsUNSET;
            }
         } else {
            if ( pstn->u.vfi.pedList ) {
               *pvalhdr = pstn->u.vfi.pedList->valhdrValue;
            } else {
               pvalhdr->vals = valsUNSET;
            }
         }
         break;
      case ityENUMCONST:
         pvalhdr->vals = valsSCALAR;
         pvalhdr->tty = ttyENUM;
         pvalhdr->pvalnList = pvalnMakeEnum( pstn->pchName );
         break;
      default: 
         assert( 0 );
    }
}

valn_t *
kapi_pvalnReverseTmp( valn_t *pvalnList )
{
   valn_t *pvalnTempList = NULL;
   valn_t *pvalnTemp = NULL;

   while ( pvalnList ) {
      pvalnTemp = pvalnList;
      pvalnList = pvalnList->pvalnNext;

      pvalnTemp->pvalnNext = pvalnTempList;
      pvalnTempList = pvalnTemp;
   }
   pvalnList = pvalnTempList;

   return( pvalnList );
}

stn_t *
kapi_pstnReverseTmp( stn_t *pstnList )
{
   stn_t *pstnTempList = NULL;
   stn_t *pstnTemp = NULL;

   pstnTempList = NULL;
   pstnTemp = NULL;

   while ( pstnList ) {
      pstnTemp = pstnList;
      pstnList = pstnList->pstnTempNext;

      pstnTemp->pstnTempNext = pstnTempList;
      pstnTempList = pstnTemp;
   }
   pstnList = pstnTempList;

   return( pstnList );
}

/* 
   Later routines assume this list is in reverse order
   of appearance 
*/
static void
AddStringToAttr( stn_t *pstnAttr, char *pch )
{
   afi_t *pafiNew;

   pafiNew = (afi_t *)malloc( sizeof( afi_t ) );
   assert( pafiNew );

   pafiNew->pafiNext = pstnAttr->u.afih.u.pafi;
   pafiNew->pchAttrVal = kapi_pchCopy( pch );
   pstnAttr->u.afih.u.pafi = pafiNew;
   pstnAttr->u.afih.nAttr++;
}

void
ProcessBitmaskExpr( valhdr_t *pvalhdrBitmask, valhdr_t *pvalhdrBits )
{
   valn_t *pvalnRun;
   stn_t *pstnEnum;
   tfi_t *ptfiBitmaskBaseType;
   int fFirst;

   fFirst = 1;
   pvalhdrBitmask->tty = ttyBITMASK;
   pvalhdrBitmask->vals = valsSCALAR;
   if (pvalhdrBits)
		pvalhdrBitmask->pvalnList = pvalhdrBits->pvalnList;
   else
		pvalhdrBitmask->pvalnList = NULL;

   /* 
      Check to make sure all identifier names in bitmask list are 
      of same enum type and declared 
   */
   if ( pvalhdrBits && pvalhdrBits->tty != ttyIDENT ) {
      kapi_Error( kapi_cLine, 1, "List of enumeration constants expected in bitmask" );
   }
   pvalnRun = pvalhdrBitmask->pvalnList;
   while ( pvalnRun ) {
      pstnEnum = kapi_pstnLookup_noadd( kapi_pknobsCurrent, pvalnRun->val.pch );
      if ( pstnEnum == NULL ) {
         kapi_Error_pch1( kapi_cLine, 1, "Undeclared identifier '%s' in bitmask",
                          pvalnRun->val.pch );
      } else if ( pstnEnum->ity != ityENUMCONST ) {
         kapi_Error_pch1( kapi_cLine, 1, 
                          "Identifier '%s' is not an enumeration constant",
                          pstnEnum->pchName );
      } else {
         if ( fFirst ) {
            fFirst = 0;
            ptfiBitmaskBaseType = pstnEnum->u.efi.ptfiBaseType;
         } else if ( ptfiBitmaskBaseType != pstnEnum->u.efi.ptfiBaseType ) {
            kapi_Error_pch2( kapi_cLine, 1, 
                             "Enumeration constant '%s' in bitmask must %s",
                              pstnEnum->pchName,
                             "be of same base type as other enumeration constants" );
         }
      }
      pvalnRun = pvalnRun->pvalnNext;
   }
}

valn_t *
pvalnCopyList( valn_t *pvalnList )
{
   valn_t *pvalnTmp;

   if ( pvalnList ) { 
      pvalnTmp = (valn_t *)malloc( sizeof( valn_t ) );

      /* for pointer types, this is dangerous!!! */
      pvalnTmp->val = pvalnList->val;
      pvalnTmp->pvalnNext = pvalnCopyList( pvalnList->pvalnNext );
      return( pvalnTmp );
   } else {
      return( NULL );
   }
}


valn_t *
pvalnMakeEnum( char *pch )
{
   valn_t *pvalnTmp;

   pvalnTmp = (valn_t *)malloc( sizeof( valn_t ) );
   memset( pvalnTmp, 0, sizeof( valn_t ) );  /* appease purify */
   pvalnTmp->val.pch = kapi_pchCopy( pch );
   pvalnTmp->pvalnNext = NULL;

   return( pvalnTmp );
}

valn_t *
pvalnMakeString( char *pch )
{
   valn_t *pvalnTmp;

   pvalnTmp = (valn_t *)malloc( sizeof( valn_t ) );
   memset( pvalnTmp, 0, sizeof( valn_t ) );  /* appease purify */
   pvalnTmp->val.pch = kapi_pchCopy( pch );
   pvalnTmp->pvalnNext = NULL;

   return( pvalnTmp );
}

valn_t *
pvalnMakeInt( int i )
{
   valn_t *pvalnTmp;

   pvalnTmp = (valn_t *)malloc( sizeof( valn_t ) );
   memset( pvalnTmp, 0, sizeof( valn_t ) );  /* appease purify */
   pvalnTmp->val.i = i;
   pvalnTmp->pvalnNext = NULL;

   return( pvalnTmp );
}

valn_t *
pvalnMakeBitmask( void )
{
   valn_t *pvalnTmp;

   pvalnTmp = (valn_t *)malloc( sizeof( valn_t ) );
   memset( pvalnTmp, 0, sizeof( valn_t ) );  /* appease purify */
   pvalnTmp->pvalnNext = NULL;

   return( pvalnTmp );
}

valn_t *
pvalnMakeReal( double d )
{
   valn_t *pvalnTmp;

   pvalnTmp = (valn_t *)malloc( sizeof( valn_t ) );
   memset( pvalnTmp, 0, sizeof( valn_t ) );  /* appease purify */
   pvalnTmp->val.r = d;
   pvalnTmp->pvalnNext = NULL;

   return( pvalnTmp );
}


valn_t *

pvalnMakeArithmeticOp( char *pch )
{
   valn_t *pvalnTmp;
   pvalnTmp = (valn_t *)malloc( sizeof( valn_t ) );
   memset( pvalnTmp, 0, sizeof( valn_t ) );  /* appease purify */
   switch (*pch)
   {
   case '+':
		pvalnTmp->val.enu = opPLUS;
		break;
   case '/':
		pvalnTmp->val.enu = opDIV;
		break;
   default:
		pvalnTmp->val.enu = opNONE;
		break;
   }
   pvalnTmp->pvalnNext = NULL;
   return( pvalnTmp );

}




valhdr_t *
pvalhdrMake( tty_t tty, vals_t vals, valn_t *pvalnList )
{
   valhdr_t *pvalhdrTmp;

   pvalhdrTmp = (valhdr_t *)malloc( sizeof( valhdr_t ) );
   pvalhdrTmp->tty = tty;
   pvalhdrTmp->vals = vals;
   pvalhdrTmp->fNoRedefine = 0;
   pvalhdrTmp->fRHS = 0;
   if ( vals == valsNOREDEFINE ) {
      pvalhdrTmp->fNoRedefine = 1;
   }
   pvalhdrTmp->pvalnList = pvalnList;

   return( pvalhdrTmp );
}


valn_t *
pvalnMakeIdent( char *pch )
{
   valn_t *pvalnTmp;

   pvalnTmp = (valn_t *)malloc( sizeof( valn_t ) );
   pvalnTmp->val.pch = kapi_pchCopy( pch );
   pvalnTmp->pvalnNext = NULL;

   return( pvalnTmp );
}

limp_t *
plimpProcessLimitVariable( valhdr_t *pvalhdrTools, valhdr_t *pvalhdrValue )
{
   limp_t *plimpTmp;

   plimpTmp = (limp_t *)malloc( sizeof( limp_t ) );

   if ( pvalhdrTools == NULL ) {
      plimpTmp->pvalnNameList = NULL;
   } else {
      plimpTmp->pvalnNameList = pvalhdrTools->pvalnList;
   }

   plimpTmp->valhdrValues = *pvalhdrValue;
   plimpTmp->plimpNext = NULL;

   return( plimpTmp );
}

limp_t *
plimpProcessLimitType( valhdr_t *pvalhdrToolList )
{
   limp_t *plimpTmp;

   plimpTmp = (limp_t *)malloc( sizeof( limp_t ) );

   if ( pvalhdrToolList == NULL ) {
      plimpTmp->pvalnNameList = NULL;
   } else {
      plimpTmp->pvalnNameList = pvalhdrToolList->pvalnList;
   }

   plimpTmp->valhdrValues.tty = ttyUNKNOWN;
   plimpTmp->valhdrValues.vals = valsNOREDEFINE;
   plimpTmp->valhdrValues.pvalnList = NULL;
   plimpTmp->plimpNext = NULL;

   return( plimpTmp );
}


tfi_t *
ptfiProcessArrayTypeSpec( stn_t *pstnIndexType, tfi_t *ptfi )
{
   if ( pstnIndexType->ity == ityTYPENAME ) {
      return( kapi_ptfiConstructArray( &(pstnIndexType->u.tfi), ptfi ) );
   } else {
      kapi_Error_pch1( kapi_cLine, 1, "Index type (%s) specification bad", 
                       pstnIndexType->pchName );
      return( NULL );
   }
}

limp_t *
plimpLookup( limp_t *plimpList, char *pchToolname )
{
   limp_t *plimpRun, *plimpNull;
   valn_t *pvalnRun;
   int fNullFound;

   plimpRun = plimpList;
   fNullFound = 0;
   while ( plimpRun ) {
      pvalnRun = plimpRun->pvalnNameList;

      if ( pvalnRun == NULL ) {  /* empty <> */
         /* 
            Don't return yet as there might still be a specific
            limit clause for this tool 
         */
         fNullFound = 1;
         plimpNull = plimpRun;
      } else {                   /* search name list */
         while ( pvalnRun ) {
            if ( pchToolname == NULL 
                 || !strcmp( pchToolname, pvalnRun->val.pch ) ) {
               return( plimpRun );
            }
            pvalnRun = pvalnRun->pvalnNext;
         }
      }
      plimpRun = plimpRun->plimpNext;
   }

   if ( fNullFound ) { 
      return( plimpNull );
   }
   return( NULL );
}

void
pvalhdrCopy( valhdr_t *pvalhdrTarget, valhdr_t *pvalhdrSource )
{
   pvalhdrTarget->vals = pvalhdrSource->vals;
   pvalhdrTarget->tty = pvalhdrSource->tty;
   pvalhdrTarget->pvalnList = pvalnCopyList( pvalhdrSource->pvalnList );
}

int
fFailCheckLimitValue2( valn_t *pvalnCheck, valhdr_t *pvalhdrLimit )
{
   int fFail;

   /* is this value in the specified limit range */
   /* return 1 if yes, 0 if no */

   fFail = 0; 
   switch ( pvalhdrLimit->vals ) {
      case valsUNSET: {
         fFail = 0;
         break;
      }
      case valsNOREDEFINE: {
         fFail = 1;
         break;
      }
      case valsRANGE: {
         int iUpper, iLower, iValue;

         switch ( pvalhdrLimit->tty ) {
            case ttyENUM: {
               /* stn_t *pstnUpper, *pstnLower; */

               iValue = pvalnCheck->val.enu;
               iUpper = iEnumUpperGetRange( pvalhdrLimit );
               iLower = iEnumLowerGetRange( pvalhdrLimit );
               break;
            }
            case ttyINT: {
               iValue = pvalnCheck->val.i;
               iUpper = iUpperGetRange( pvalhdrLimit );
               iLower = iLowerGetRange( pvalhdrLimit );
               break;
            }
            default: { 
               assert( 0 );
            }
         }
         if ( iValue < iLower || iValue > iUpper ) {
            fFail = 2;
         }
         
         break;
      }
      case valsLIST: {
         int iListValue, iValue;
         valn_t *pvalnRun;
         char *pchValue, *pchListValue;
         double rValue, rListValue;

         fFail = 2;
         switch ( pvalhdrLimit->tty ) {
            case ttyENUM: {
               stn_t *pstnValue;

               pstnValue = kapi_pstnLookup( kapi_pknobsCurrent, 
                                pvalnCheck->val.pch );
               iValue = pstnEnum2idx( pstnValue );
               break;
            }
            case ttyINT: {
               iValue = pvalnCheck->val.i;
               break;
            }
            case ttySTRING: {
               pchValue = pvalnCheck->val.pch;
               break;
            }
            case ttyREAL: {
               rValue = pvalnCheck->val.r;
               break;
            }
            default: { 
               assert( 0 );
            }
         }

         pvalnRun = pvalhdrLimit->pvalnList;
         while ( pvalnRun ) {
            switch ( pvalhdrLimit->tty ) {
               case ttyENUM: {
                  stn_t *pstnListValue;

                  pstnListValue = kapi_pstnLookup( kapi_pknobsCurrent, 
                                              pvalnRun->val.pch );
                  iListValue = pstnEnum2idx( pstnListValue );

                  if ( iValue == iListValue ) {
                     fFail = 0;
                  }
                  break;
               }
               case ttyINT: {
                  iListValue = pvalnRun->val.i;
                  if ( iValue == iListValue ) {
                     fFail = 0;
                  }
                  break;
               }
               case ttySTRING: {
                  pchListValue = pvalnRun->val.pch;
                  if ( !strcmp( pchListValue, pchValue ) ) {
                     fFail = 0;
                  }
                  break;
               }
               case ttyREAL: {
                  rListValue = pvalnRun->val.r;
                  if ( rListValue == rValue ) {
                     fFail = 0;
                  }
                  break;
               }
               default: { 
                  assert( 0 );
               }
            }
            pvalnRun = pvalnRun->pvalnNext;
         }
         break;
      }
      case valsSCALAR:
      default: {
         assert( 0 );
      }
   }

   return( fFail );
}

int
fFailCheckLimitValue( valhdr_t *pvalhdrNew, ed_t *pedIn )
{
   int fFail;

   /* is this value in the specified limit range */
   /* return 1 if yes, 0 if no */

   if ( pedIn->valhdrLimit.fNoRedefine ) {
      return( 1 );
   } 

   fFail = 0; 
   switch ( pedIn->valhdrLimit.vals ) {
      case valsUNSET: {
         fFail = 0;
         break;
      }
      case valsNOREDEFINE: {
         fFail = 1;
         break;
      }
      case valsRANGE: {
         int iUpper, iLower, iValue;
         valn_t *pvalnLower, *pvalnUpper;

         pvalnLower = pedIn->valhdrLimit.pvalnList;
         pvalnUpper = pedIn->valhdrLimit.pvalnList->pvalnNext;
         switch ( pedIn->valhdrValue.tty ) {
            case ttyENUM: {
               stn_t *pstnUpper, *pstnLower;

               iValue = pvalhdrNew->pvalnList->val.enu;
               pstnUpper = kapi_pstnLookup( kapi_pknobsCurrent, pvalnUpper->val.pch );
               pstnLower = kapi_pstnLookup( kapi_pknobsCurrent, pvalnLower->val.pch );
               iUpper = pstnEnum2idx( pstnUpper );
               iLower = pstnEnum2idx( pstnLower );
               break;
            }
            case ttyINT: {
               iValue = pvalhdrNew->pvalnList->val.i;
               iUpper = pvalnUpper->val.i;
               iLower = pvalnLower->val.i;
               break;
            }
            default: { 
               assert( 0 );
            }
         }
         if ( iValue < iLower || iValue > iUpper ) {
            fFail = 2;
         }
         
         break;
      }
      case valsLIST: {
         int iListValue, iValue;
         valn_t *pvalnRun;
         char *pchValue, *pchListValue;
         double rValue, rListValue;

         fFail = 2;
         switch ( pedIn->valhdrValue.tty ) {
            case ttyENUM: {
               stn_t *pstnValue;

               pstnValue = kapi_pstnLookup( kapi_pknobsCurrent, 
                                pvalhdrNew->pvalnList->val.pch );
               iValue = pstnEnum2idx( pstnValue );
               break;
            }
            case ttyINT: {
               iValue = pvalhdrNew->pvalnList->val.i;
               break;
            }
            case ttySTRING: {
               pchValue = pvalhdrNew->pvalnList->val.pch;
               break;
            }
            case ttyREAL: {
               rValue = pvalhdrNew->pvalnList->val.r;
               break;
            }
            default: { 
               assert( 0 );
            }
         }

         pvalnRun = pedIn->valhdrLimit.pvalnList;
         while ( pvalnRun ) {
            switch ( pedIn->valhdrValue.tty ) {
               case ttyENUM: {
                  stn_t *pstnListValue;

                  pstnListValue = kapi_pstnLookup( kapi_pknobsCurrent, 
                                              pvalnRun->val.pch );
                  iListValue = pstnEnum2idx( pstnListValue );

                  if ( iValue == iListValue ) {
                     fFail = 0;
                  }
                  break;
               }
               case ttyINT: {
                  iListValue = pvalnRun->val.i;
                  if ( iValue == iListValue ) {
                     fFail = 0;
                  }
                  break;
               }
               case ttySTRING: {
                  pchListValue = pvalnRun->val.pch;
                  if ( !strcmp( pchListValue, pchValue ) ) {
                     fFail = 0;
                  }
                  break;
               }
               case ttyREAL: {
                  rListValue = pvalnRun->val.r;
                  if ( rListValue == rValue ) {
                     fFail = 0;
                  }
                  break;
               }
               default: { 
                  assert( 0 );
               }
            }
            pvalnRun = pvalnRun->pvalnNext;
         }
         break;
      }
      case valsSCALAR:
      default: {
         assert( 0 );
      }
   }

   return( fFail );
}

void
SetVariableValueLimit( valhdr_t *pvalhdr, limp_t *plimNew )
{
   /* no new limit restrictions */
   if ( plimNew == NULL ) {
      return;
   }

   /* 
      Could check at this point ot make sure that the person does not
      try to 'relimit' a variables value to something that allows 
      more values than the previously existing limit statement
   */
   /* cannot override a no-redefine */
   if ( pvalhdr->fNoRedefine ) {
      return;
   }

   *pvalhdr = plimNew->valhdrValues;
}

bv_t *
pbvBuild4valhdr( stn_t *pstnVar, valhdr_t *pvalhdr )
{
   int length, pos;
   bv_t *pbv;
   tfi_t *ptfiVar;
   valn_t *pvalnRun, *pvalnEnumList;

   ptfiVar = pstnVar->u.vfi.ptfi;

   if ( ptfiVar->tty == ttyBITMASK ) {
      length = ptfiVar->ptfiBitmaskBaseType->nEnumConst;
      pvalnEnumList = ptfiVar->ptfiBitmaskBaseType->pvalnEnums;
   } else if ( ptfiVar->tty == ttyARRAY &&
               ptfiVar->ptfiArrayEltType->tty == ttyBITMASK ) {
      tfi_t *ptfiBitmaskBaseType;

      ptfiBitmaskBaseType = ptfiVar->ptfiArrayEltType->ptfiBitmaskBaseType;

      length = ptfiBitmaskBaseType->nEnumConst;
      pvalnEnumList = ptfiBitmaskBaseType->pvalnEnums;

   } else {
      assert( 0 );
   }

   pbv = pbvMake( length, 0 );

   pvalnRun = pvalhdr->pvalnList;
   while ( pvalnRun ) {
 
      pos = posInEnumList( pvalnRun->val.pch, pvalnEnumList );
      assert( pos != -1 );
      SETBIT_bv( pbv, pos );

      pvalnRun = pvalnRun->pvalnNext;
   }

   return( pbv );
}

static int
posInEnumList( char *pch, valn_t *pvalnEnums )
{
   valn_t *pvalnRun;
   int cnt;

   cnt = 0;
   pvalnRun = pvalnEnums;
   while ( pvalnRun ) {
      if ( ! strcmp( pvalnRun->val.pch, pch ) ) {
         return( cnt );
      }
      cnt++;
      pvalnRun = pvalnRun->pvalnNext;
   }

   return( -1 );
}


valn_t *
pvalnGetIth( valn_t *pvalnIn, int iIndex )
{
   valn_t *pvalnRun;
   int i;

   /* find the index name corresponding to the ith index */
   pvalnRun = pvalnIn;
   for ( i=0; i<iIndex; i++ ) { 
      pvalnRun = pvalnRun->pvalnNext;
   }

   return( pvalnRun );
}

ed_t *
pedLookup4pstn( stn_t *pstnLHSVar, char *pchIndex )
{
   /* valn_t *pvalnRun; */
   ed_t *pedRun;

   assert( pstnLHSVar->u.vfi.ptfi->tty == ttyARRAY );

   pedRun = pstnLHSVar->u.vfi.pedList;
   while ( pedRun ) {
      if ( ! strcmp( pedRun->pchIndexName, pchIndex ) ) {
         return( pedRun );
      }
      pedRun = pedRun->pedNext;
   }
 
   return( NULL ) ;
}

int
idxped4pstn( stn_t *pstnLHSVar, char *pchIndex )
{
   /* valn_t *pvalnRun; */
   ed_t *pedRun;
   int iPos;

   assert( pstnLHSVar->u.vfi.ptfi->tty == ttyARRAY );

   iPos = 0;
   pedRun = pstnLHSVar->u.vfi.pedList;
   while ( pedRun ) {
      if ( ! strcmp( pedRun->pchIndexName, pchIndex ) ) {
         return( iPos );
      }
      iPos++;
      pedRun = pedRun->pedNext;
   }

   return( -1 ) ;
}


ed_t *
pedLookup4ped( ed_t *pedList, char *pchIndex )
{
   /* valn_t *pvalnRun; */
   ed_t *pedRun;

   pedRun = pedList;
   while ( pedRun ) {
      if ( ! strcmp( pedRun->pchIndexName, pchIndex ) ) {
         return( pedRun );
      }
      pedRun = pedRun->pedNext;
   }
 
   return( NULL ) ;
}


ed_t *
pedGetIth( stn_t *pstnLHSVar, int iIndex )
{
   valn_t *pvalnRun;
   ed_t *pedRun;

   assert( pstnLHSVar->u.vfi.ptfi->tty == ttyARRAY );

   if ( pstnLHSVar->u.vfi.ptfi->ptfiArrayIndexType->tty != ttySTRING ) {
      pvalnRun = pvalnGetIth( 
                   pstnLHSVar->u.vfi.ptfi->ptfiArrayIndexType->pvalnEnums, 
                   iIndex );
      assert( pvalnRun );
      /* 
         Now we have the name of the ith index, see if the array has a
         value at that location 
      */

      pedRun = pstnLHSVar->u.vfi.pedList;
      while ( pedRun ) {
         if ( ! strcmp( pedRun->pchIndexName, pvalnRun->val.pch ) ) {
            return( pedRun );
         }
         pedRun = pedRun->pedNext;
      }
   } else { /* was indexed by string, just find the physically iIndex elt */
      pedRun = pstnLHSVar->u.vfi.pedList;
      while ( iIndex > 0 && pedRun) {
         iIndex--;
         pedRun = pedRun->pedNext;
      }
      return( pedRun );
   }

   return( NULL ) ;
}

ed_t *
pedMake( char *pchIndexName, tfi_t *ptfi )
{
   ed_t *pedTmp;

   pedTmp = (ed_t *)malloc( sizeof(ed_t) );
   memset( pedTmp, 0, sizeof( ed_t ) );  /* appease purify */

   pedTmp->ptfi = ptfi;
   pedTmp->pedNext = NULL;
   pedTmp->pchIndexName = pchIndexName;
   pedTmp->valhdrLimit.vals = valsUNSET;
   pedTmp->valhdrLimit.fNoRedefine = 0;
   pedTmp->valhdrValue.vals = valsUNSET;
 
   return( pedTmp );
}

kapi_cluster_t
clusterInterpretpchclr( char *pchClr )
{
   char ch;

   assert( pchClr );
   if ( strncmp( pchClr, "cluster", 7 ) ) {
      return( -1 );
   }
   ch = pchClr[ 7 ];
   if ( ch > '9' || ch < '0' ) {
      return( -1 );
   }
   return( ch - '0' );
}

kapi_ut_t 
utInterpretCportName( char *pchPort, int *pn )
{
  if ( pchPort == NULL ) {
     return( -1 );
  }
  return( utInterpretPortName( pchPort+1, pn ) );
}

kapi_ut_t 
utInterpretPortName( char *pchPort, int *pn )
{
   char *pch;
   kapi_ut_t ut;

   if ( strncmp( pchPort, "port", 4 ) ) {
      return( -1 );
   }

   pch = &(pchPort[ 4 ]);

   switch (pch[ 0 ]) {
   case 'M':
      ut = kapi_utM;
      break;
   case 'I':
      ut = kapi_utI;
      break;
   case 'B':
      ut = kapi_utB;
      break;
   case 'F':
      ut = kapi_utF;
      break;
   default:
      return( -1 );
      break;
   }

   /* check for single digit port number */
   pch = &(pchPort[ 5 ]);
   if ( pch[0] > '9' || pch[0] < '0' ) {
      return( -1 );
   }

   pch = &(pchPort[ 6 ]);
   if ( pch[0] != '\0' ) {
      return( -1 );
   }

   /* convert port number to integer */
   *pn = pchPort[5] - '0';

   return( ut );
}

/* 
 *  This routine simulates the declaration:
 *     cluster0PortMask : array[ string ] of fu_t;
 */
void
Declare_cluster0CportMask( stn_t *pstn )
{
   tfi_t *ptfiEltType, *ptfiType;
   stn_t *pstnString, *pstnfu_t;

  
   pstnString = kapi_pstnLookup( kapi_pknobsCurrent, "string" );
   pstnfu_t = kapi_pstnLookup( kapi_pknobsCurrent, "fu_t" );

   if ( pstnfu_t->ity == ityTYPENAME
                && pstnfu_t->u.tfi.tty == ttyENUM ) {
      ptfiEltType = kapi_ptfiConstructBitmask( &(pstnfu_t->u.tfi) );
   } else {
      kapi_Error_pch1( kapi_cLine, 1,
                       "Base type (%s) of bitmask specification bad",
                       pstn->pchName );
      return;
   }

   ptfiType = ptfiProcessArrayTypeSpec( pstnString, ptfiEltType );

   kapi_ProcessVarDecl( pstn, ptfiType );
}
