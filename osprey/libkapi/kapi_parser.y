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

%{

/* static char sccs_id[] = "%W%  %G%  %U%"; */


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "kapi_internal.h"
#include "kapi_util.h"
#include "kapi_error.h"
#include "kapi_parse.h"

extern knobs_t *kapi_pknobsCurrent;
extern char yytext[];

static vfi_t vfi_gl;
%}


%token ENDMARKER  0

%token   tokARRAY
%token   tokBITMASK
%token   tokEXPECT
%token   tokENUM
%token   tokLIMIT
%token   tokUSE
%token   tokTYPE
%token   tokVARIABLE
%token   tokATTRIBUTE
%token   tokNOREDEFINE
%token   tokOF
%token   tokIMPLICIT
%token   tokNONE

%token   tokLCBRACK
%token   tokRCBRACK
%token   tokLABRACK
%token   tokRABRACK
%token   tokLSBRACK
%token   tokRSBRACK
%token   tokLPAREN
%token   tokRPAREN

%token  <pstnA> tokINT
%token  <pstnA> tokREAL
%token  <pstnA> tokSTRING

%token  <rValue>    tokREALCONST
%token  <iValue>    tokINTCONST
%token  <pch>  tokSTRINGCONST
   
%token   tokEQUALS
%token   tokCOMMA
%token   tokCOLON
%token   tokSEMICOLON
%token   tokCOLONEQUALS
%token   tokPLUSEQUALS
%token   tokSTAR
%token   tokDOT
%token   tokDDOT
%token   tokVERTBAR

%token   tokCOMMENT
  
%token   <pch> tokIDENT


%token   <pch> tokPLUS
%token   <pch> tokDIV
%token   tokAPPEND

/* Rule Types */
%type <pvalhdr> bitmaskexpr
%type <pvalhdr> rhs
%type <pvalhdr> arithmeticrhs
%type <pvaln> intrhs
%type <pvaln> arithmeticop
%type <pvaln> arithmeticrhsrest


%type <pch> lhsindexexp
%type <pch> lhsindex
%type <iValue> typeexpect

%type <ptfi> typespecclause
%type <ptfi> fulltypespec
%type <ptfi> simpletypespec
%type <plimp> limittypelist
%type <plimp> nlimittypelist
%type <plimp> limittype

%type <plimp>    limitassign
%type <plimp>    limitassignlist
%type <plimp>    nlimitassignlist

%type <pvalhdr> limitassignval

%type <pvalhdr> valuerange

%type <pvalhdr> valuelist
%type <pvalhdr> reallist
%type <pvalhdr> idlist
%type <pvalhdr> nidlist
%type <pvalhdr> intlist
%type <pvalhdr> stringlist
%type <pvalhdr> nstringlist

%type <pvaln> revstringlist
%type <pvaln> revintlist
%type <pvaln> revidlist
%type <pvaln> revreallist


%%

file:
	/* NULL */
	| stmtlist
	;

stmtlist:
	stmt
	| stmtlist stmt
	;

stmt:
	stmt0 tokSEMICOLON
	| stmt0 stmt0 {
            kapi_Error( kapi_cLine, 1, "Missing semicolon here or above" );
	}
	;

stmt0:
	typestmt
	| expectstmt
	| appendstmt
	| implicitstmt
	| assignstmt
	| arithmeticassignstmt
	| vardeclstmt
	| attrstmt
	| error {
            kapi_Error( kapi_cLine, 1, "Unknown error here or above" );
	}
	;


	

attrstmt:
	tokIDENT tokPLUSEQUALS tokSTRINGCONST {
	   stn_t *pstn;
           char *pchName, *pchString;

           /* 
              If this is an old KAPI2x BYPASS spec, translate it
              to an INTRACLUSTER bypass NO LONGER SUPPORTED
            */
           if ( 0 == strcmp( $1, "BYPASS" ) ) {
			  kapi_Warning(kapi_cLine,0,"kapi 2.x BYPASS compatability not supported!");
           } 

       pchName = $1;
       pchString = $3;
           
	   pstn = kapi_pstnLookup( kapi_pknobsCurrent, pchName );
           kapi_ProcessAttrDecl( pstn, pchString );
	   free(pchName);
	   free(pchString);
	}
	| tokIDENT tokSTRINGCONST {
            kapi_Error( kapi_cLine, 1, "Missing += or := in stmt" );
	}
	;

typestmt:
	tokTYPE tokIDENT tokEQUALS
	   tokENUM tokLPAREN idlist tokRPAREN
           nlimittypelist
	{
	   stn_t *pstn;

	   pstn = kapi_pstnLookup( kapi_pknobsCurrent, $2 );
           kapi_ProcessTypeDecl( pstn, $6, $8 ); 
	}
	;

expectstmt:
	tokEXPECT typeexpect idlist {
	   kapi_ProcessExpect( $3, $2 );
	} 
	;

appendstmt:
	tokAPPEND tokIDENT tokEQUALS
	   tokENUM tokLPAREN idlist tokRPAREN {
	   stn_t *pstn;

	   pstn = kapi_pstnLookup( kapi_pknobsCurrent, $2 );
           kapi_ProcessAppendDecl( pstn, $6); 
	}
	;

implicitstmt:
	tokIMPLICIT tokNONE {
	   kapi_pknobsCurrent->fImplicitNone = 1;
	} 
	;

typeexpect:
	tokTYPE { $$ = 0; }
	| tokVARIABLE { $$ = 1; }
	| tokENUM { $$ = 2; }
	| tokATTRIBUTE { $$ = 3; }
	;

assignstmt:
	tokIDENT lhsindexexp tokCOLONEQUALS rhs nlimitassignlist {
	   stn_t *pstn;
           char *pchIdent, *pchIndex, mpchIndex[ 200 ];
 
           if ( 0 == strcmp( $1, "MaxBundleIssue" ) ) {
              pchIdent = kapi_pchCopy( "cluster0MaxBundleIssue" );
              pchIndex = NULL;
           } else if ( 0 == strcmp( $1, "PortMask" ) ) {
              pchIdent = kapi_pchCopy( "cluster0CportMask" );
              sprintf( mpchIndex, "%%%c%s", 'c', $2 );
              pchIndex = kapi_pchCopy( mpchIndex );
           } else {
              pchIdent = $1;
              pchIndex = $2;
           }

	   pstn = kapi_pstnLookup( kapi_pknobsCurrent, pchIdent );
	   kapi_ProcessAssignment( pstn, pchIndex, $4, $5 );
	}
	;


arithmeticassignstmt:
	tokIDENT lhsindexexp tokCOLONEQUALS arithmeticrhs {
	   stn_t *pstn;
           char *pchIdent, *pchIndex, mpchIndex[ 200 ];
 
           if ( 0 == strcmp( $1, "MaxBundleIssue" ) ) {
              pchIdent = kapi_pchCopy( "cluster0MaxBundleIssue" );
              pchIndex = NULL;
           } else if ( 0 == strcmp( $1, "PortMask" ) ) {
              pchIdent = kapi_pchCopy( "cluster0CportMask" );
              sprintf( mpchIndex, "%%%c%s", 'c', $2 );
              pchIndex = kapi_pchCopy( mpchIndex );
           } else {
              pchIdent = $1;
              pchIndex = $2;
           }

	   pstn = kapi_pstnLookup( kapi_pknobsCurrent, pchIdent );
	   kapi_ProcessArithmeticAssignment( pstn, pchIndex, $4 );
	}
	;


lhsindexexp:
	tokLSBRACK lhsindex tokRSBRACK {
	   $$ = $2;
	}
	| /* NULL */ {
	   $$ = NULL;
    	}
	;

lhsindex:
	tokIDENT {
	   $$ = $1;
	}
	| tokSTAR {
 	   $$ = kapi_pchCopy( "*" );
	}
	| tokSTRINGCONST {
           char *pchTmp;
           
           pchTmp = (char *)malloc( sizeof(char)*strlen($1) + 2 );
 	   sprintf( pchTmp, "%%%s", $1 );
 	   $$ = pchTmp;
	}
	;

vardeclstmt:
	tokIDENT typespecclause {
	   stn_t *pstn;

           if ( 0 == strcmp( $1, "PortMask" ) ) {
	      pstn = kapi_pstnLookup( kapi_pknobsCurrent, "cluster0CportMask" );
              Declare_cluster0CportMask( pstn );
           } else {
   	      pstn = kapi_pstnLookup( kapi_pknobsCurrent, $1 );
	      kapi_ProcessVarDecl( pstn, $2 );
           }
	}
	;


typespecclause:
	tokCOLON fulltypespec {
	   $$ = $2;
	}
	| /* NULL */ {
 	   $$ = NULL;
	}
	;

fulltypespec:
	simpletypespec {
	   $$ = $1;
	}
	| tokARRAY tokLSBRACK tokIDENT tokRSBRACK tokOF simpletypespec {
	   stn_t *pstn;
 
	   pstn = kapi_pstnLookup( kapi_pknobsCurrent, $3 );
           $$ = ptfiProcessArrayTypeSpec( pstn, $6 );
 	}
	| tokARRAY tokLSBRACK tokSTRING tokRSBRACK tokOF simpletypespec {
           $$ = ptfiProcessArrayTypeSpec( $3, $6 );
 	}
	;

simpletypespec:
	tokSTRING {
           $$ = &($1->u.tfi);
	}
	| tokINT {
           $$ = &($1->u.tfi);
	}
	| tokREAL {
           $$ = &($1->u.tfi);
	}
	| tokIDENT {
	   stn_t *pstn;
 
	   pstn = kapi_pstnLookup( kapi_pknobsCurrent, $1 );
	   $$ = kapi_ptfiCheckDeclaredType( pstn );
	}
	| tokBITMASK tokLPAREN tokIDENT tokRPAREN {
	   stn_t *pstn;
 
	   pstn = kapi_pstnLookup( kapi_pknobsCurrent, $3 );
           if ( pstn->ity == ityTYPENAME 
                && pstn->u.tfi.tty == ttyENUM ) {
	      $$ = kapi_ptfiConstructBitmask( &(pstn->u.tfi) );
           } else {
              kapi_Error_pch1( kapi_cLine, 1, 
                               "Base type (%s) of bitmask specification bad", 
                                pstn->pchName );
              $$ = NULL;
	   }
	}
	;


nstringlist:
	/* NULL */ {
	  $$ = NULL;
	}
	| stringlist {
 	  $$ = $1;
 	}
	;

stringlist:
	revstringlist {
 	   $$ = pvalhdrMake( ttySTRING, valsLIST, kapi_pvalnReverseTmp( $1 ) );
	}
	;

revstringlist:
	tokSTRINGCONST {
	   $$ = pvalnMakeString( $1 );
	}
	|  revstringlist tokCOMMA tokSTRINGCONST {
           valn_t *pvaln;

	   pvaln = pvalnMakeString( $3 );
	   pvaln->pvalnNext = $1;
           $$ = pvaln;
	}
	;

reallist:
	revreallist {
 	   $$ = pvalhdrMake( ttyREAL, valsLIST, kapi_pvalnReverseTmp( $1 ) );
	}
	;


revreallist:
	tokREALCONST {
	   $$ = pvalnMakeReal( $1 );
	}
	| revreallist tokCOMMA tokREALCONST {
           valn_t *pvaln;

	   pvaln = pvalnMakeReal( $3 );
	   pvaln->pvalnNext = $1;
           $$ = pvaln;
	}
	;

nidlist:
	revidlist {
 	   $$ = pvalhdrMake( ttyIDENT, valsLIST, kapi_pvalnReverseTmp( $1 ) );
	}
	| /* NULL */ {
	   $$ = NULL;
	}
	;
idlist:
	revidlist {
 	   $$ = pvalhdrMake( ttyIDENT, valsLIST, kapi_pvalnReverseTmp( $1 ) );
	}
	;


revidlist:
	tokIDENT {
	   $$ = pvalnMakeString( $1 );
	}
	| revidlist tokCOMMA tokIDENT {
           valn_t *pvaln;

	   pvaln = pvalnMakeString( $3 );
	   pvaln->pvalnNext = $1;
           $$ = pvaln;
	}
	;

intlist:
	revintlist {
 	   $$ = pvalhdrMake( ttyINT, valsLIST, kapi_pvalnReverseTmp( $1 ) );
	}
	;

revintlist:
	tokINTCONST {
	   $$ = pvalnMakeInt( $1 );
	}
	| revintlist tokCOMMA tokINTCONST {
           valn_t *pvaln;

	   pvaln = pvalnMakeInt( $3 );
	   pvaln->pvalnNext = $1;
           $$ = pvaln;
	}
	;


rhs:
	tokSTRINGCONST {
           valhdr_t *pvalhdr;
 
           pvalhdr = (valhdr_t *)malloc( sizeof( valhdr_t ) );
           pvalhdr->pvalnList = pvalnMakeString( $1 );
           pvalhdr->tty = ttySTRING;
           pvalhdr->vals = valsSCALAR;
		   pvalhdr->fRHS=0;

	   $$ = pvalhdr;
	}
	| tokINTCONST {
           valhdr_t *pvalhdr;
 
           pvalhdr = (valhdr_t *)malloc( sizeof( valhdr_t ) );
           pvalhdr->pvalnList = pvalnMakeInt( $1 );
           pvalhdr->tty = ttyINT;
           pvalhdr->vals = valsSCALAR;
		   pvalhdr->fRHS=0;

	   $$ = pvalhdr;
	}
	| tokREALCONST {
           valhdr_t *pvalhdr;
 
           pvalhdr = (valhdr_t *)malloc( sizeof( valhdr_t ) );
           pvalhdr->pvalnList = pvalnMakeReal( $1 );
           pvalhdr->tty = ttyREAL;
           pvalhdr->vals = valsSCALAR;
		   pvalhdr->fRHS=0;

	   $$ = pvalhdr;
	}
	| bitmaskexpr {
	   $$ = $1;
	}
	| tokIDENT lhsindexexp {
           stn_t *pstn;

	   pstn = kapi_pstnLookup( kapi_pknobsCurrent, $1 );
           if ( pstn->ity == ityUNKNOWN ) {
              kapi_Error_pch1( kapi_cLine, 0, "'%s' unknown identifier", $1 );
	      $$ = NULL;
           } else {
              valhdr_t *pvalhdr;
    
              pvalhdr = (valhdr_t *)malloc( sizeof( valhdr_t ) );
			  pvalhdr->fRHS=1;
              pvalhdr->tty = ttyIDENT;

		    if ($2 != NULL ) /* if array, get item */  
			{
				ed_t *pedElement;
				pedElement=pedLookup4pstn(pstn,$2);
				pvalhdrCopy(pvalhdr,&pedElement->valhdrValue);
				pedElement->valhdrValue.fRHS=1;
			} else
			{
              kapi_LookUpIdent_valhdr( pstn, pvalhdr, 0 );
			}
   
   	      $$ = pvalhdr;
           }
	}
	;

arithmeticrhs:
	arithmeticrhsrest arithmeticop intrhs {
              valhdr_t *pvalhdr;
			  valn_t *pvalnOp, *pvalnExp, *pvalnVal;

              pvalhdr = (valhdr_t *)malloc( sizeof( valhdr_t ) );
              pvalhdr->tty = ttyEXPRESSION;
			  pvalhdr->vals = valsLIST;
			  pvalnVal=$3;
			  pvalnOp=$2;
			  pvalnExp=$1;
			  pvalnVal->pvalnNext=pvalnOp;
			  pvalnOp->pvalnNext=pvalnExp;

			  pvalhdr->pvalnList=pvalnVal;
       	      $$ = pvalhdr;
	}
	;

arithmeticrhsrest:
	intrhs { 
		$$=$1;
	}
	| arithmeticrhsrest arithmeticop intrhs {
			  valn_t *pvalnOp, *pvalnExp, *pvalnVal;

			  pvalnVal=$3;
			  pvalnOp=$2;
			  pvalnExp=$1;
			  pvalnVal->pvalnNext=pvalnOp;
			  pvalnOp->pvalnNext=pvalnExp;

       	      $$ = pvalnVal;
	}
	;


intrhs:
	tokINTCONST {
           valn_t *pvaln;
 
           pvaln = pvalnMakeInt( $1 );

	   $$ = pvaln;
	}
	| tokIDENT lhsindexexp {
           stn_t *pstn;

	   pstn = kapi_pstnLookup( kapi_pknobsCurrent, $1 );
           if ( pstn->ity == ityUNKNOWN ) {
              kapi_Error_pch1( kapi_cLine, 0, "'%s' unknown identifier", $1 );
	          $$ = NULL;
           } else 
		   {
              valhdr_t *pvalhdr;
			  pvalhdr = (valhdr_t *)malloc( sizeof( valhdr_t ) );

		    if (pstn->u.vfi.ptfi->tty == ttyARRAY) /* if array, get item */  
			{
				ed_t *pedElement;
				pedElement=pedLookup4pstn(pstn,$2);
				pvalhdrCopy(pvalhdr,&pedElement->valhdrValue);
				pedElement->valhdrValue.fRHS=1;
			} else
			{
              kapi_LookUpIdent_valhdr( pstn, pvalhdr, 0 );
			}

			  /* now pvalhdr holds the value for the variable */
			  if ((pvalhdr->tty!=ttyINT) || (pvalhdr->vals!=valsSCALAR)) {
                 kapi_Error_pch1( kapi_cLine, 0, "'%s' is non integer used in arithmetic expression", $1 );
				 $$ = NULL;
			  } else
			  {
				 valn_t *pvaln;
				 pvaln = pvalnMakeInt(pvalhdr->pvalnList->val.i);
				 /* mark value as used on RHS of equation */
				 pstn->fRHS=1;
   				 $$ = pvaln;
			  }
           }
	}
	;

arithmeticop:
	tokPLUS {
		valn_t *pvaln;
		pvaln=pvalnMakeArithmeticOp($1);

	   $$ = pvaln;
	}
	| tokDIV {
		valn_t *pvaln;
		pvaln=pvalnMakeArithmeticOp($1);

	   $$ = pvaln;
	}
	;

bitmaskexpr:
	tokBITMASK tokLPAREN nidlist tokRPAREN {
           valhdr_t *pvalhdr;
    
           pvalhdr = (valhdr_t *)malloc( sizeof( valhdr_t ) );
           ProcessBitmaskExpr( pvalhdr, $3 );
	   $$ = pvalhdr;
	}
	;

nlimitassignlist:
	limitassignlist  {
           $$ = $1;
	}
	| /* NULL */ {
           $$ = NULL;
 	}
	;

nlimittypelist:
	limittypelist { 
           $$ = $1; 
 	}
	| /* NULL */ { 
           $$ = NULL; 
        }
	;


limitassignlist:
	limitassign { 
           $1->plimpNext = NULL; 
           $$ = $1; 
	}
	| limitassignlist limitassign {
           limp_t *plimpTmp;
  
           plimpTmp = $2;
           plimpTmp->plimpNext = $1;
	   $$ = plimpTmp;
 	}
	;

limittypelist:
	limittype { 
           $1->plimpNext = NULL; 
           $$ = $1; 
	}
	| limittypelist limittype {
           limp_t *plimpTmp;
  
           plimpTmp = $2;
           plimpTmp->plimpNext = $1;
	   $$ = plimpTmp;
 	}
	;


limitassign:
	tokLIMIT tokLABRACK nstringlist tokRABRACK limitassignval {
  	   $$ = plimpProcessLimitVariable( $3, $5 );
        }
	;

limittype:
	tokLIMIT tokLABRACK nstringlist tokRABRACK tokNOREDEFINE {
  	   $$ = plimpProcessLimitType( $3 );
   	}
	;


limitassignval:
	tokNOREDEFINE {
	   $$ = pvalhdrMake( ttyUNKNOWN, valsNOREDEFINE, NULL );
	}
	| tokLPAREN valuelist tokRPAREN  {
	   $$ = $2;
	}
	| tokLPAREN valuerange tokRPAREN {
	   $$ = $2;
	}
	;

valuelist:
	idlist { $$ = $1; }
	| intlist { $$ = $1; }
	| reallist { $$ = $1; }
	| stringlist { $$ = $1; }
	;

valuerange:
	tokIDENT tokDDOT tokIDENT {
           valn_t *pvaln1, *pvaln2;
           stn_t *pstn1, *pstn2;

           /* create entries, but do not use pointers */
           pstn1 = kapi_pstnLookup( kapi_pknobsCurrent, $1 );
           pstn2 = kapi_pstnLookup( kapi_pknobsCurrent, $3 );

           pvaln1 = pvalnMakeIdent( $1 );
           pvaln2 = pvalnMakeIdent( $3 );
           pvaln1->pvalnNext = pvaln2;
           pvaln2->pvalnNext = NULL;
	   $$ = pvalhdrMake( ttyIDENT, valsRANGE, pvaln1 );
        }
	| tokINTCONST tokDDOT tokINTCONST {
           valn_t *pvaln1, *pvaln2;

           pvaln1 = pvalnMakeInt( $1 );
           pvaln2 = pvalnMakeInt( $3 );
           pvaln1->pvalnNext = pvaln2;
           pvaln2->pvalnNext = NULL;
	   $$ = pvalhdrMake( ttyINT, valsRANGE, pvaln1 );
        }
	;

%%

