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

#ifndef _KAPI_LEX_H_
#define _KAPI_LEX_H_

#include "kapi_bv.h"
#include "kapi_ia64.h"
#include "kapi_internal.h"

typedef int tok_t;

typedef enum _TYPE_TYPE {
   ttyINT,     /* stored as int in val_t */
   ttyREAL,    /* stored as double in val_t */
   ttySTRING,  /* stored as string in val_t */

   ttyENUM,    /* stored as int in val_t */

   ttyBITMASK,  /* compound type */
   ttyARRAY,    /* compound type */
   ttyEXPRESSION,    /* compound type */


   ttyIDENT,   /* stored as string in val_t */

   ttyUNKNOWN
} tty_t;

typedef enum _TREDEF_STATUS {
    tredefSTATUS_OK,
    tredefSTATUS_MARKED_NOREDEFINE,
    tredefSTATUS_ALREADY_REDEFINED,
    tredefSTATUS_USED_IN_LIMIT
} tredef_t;

typedef enum _IDENT_TYPE {
   ityENUMCONST,
   ityTYPENAME,
   ityVARNAME,
   ityATTRIBUTENAME,
   ityRESERVED,
   ityUNKNOWN
} ity_t;


typedef union _VALUE {
   int    i;
   double r;
   char  *pch;
   int    enu;
   bv_t   bv;
   struct _SYM_TABLE_NODE *pstn;
} val_t;

typedef struct _VALUE_NODE {
   struct _VALUE_NODE *pvalnNext;
   val_t  val;
} valn_t;

typedef enum _VALUE_STRUCTURE {
   valsUNSET,
   valsRANGE,
   valsLIST,
   valsSCALAR,
   valsNOREDEFINE
} vals_t;


typedef enum _OPERATOR_E {

	opNONE=0,
	opPLUS,
	opDIV
} arithmetic_op_t;


/* represents a single element value */
typedef struct _VALUE_NODE_HDR {  
   int     fNoRedefine; /* field not always used!! */
   int	   fRHS;		/* value was used on RHS of expression */
   vals_t  vals;        /* structure of elements */
   tty_t   tty;         /* type of elements */
   valn_t  *pvalnList;  /* values */
} valhdr_t;

typedef struct _LIMIT_PARSE_STRUCT {
   struct  _LIMIT_PARSE_STRUCT *plimpNext;
   valn_t   *pvalnNameList;    /* list of tool names - always strings */
   valhdr_t valhdrValues; 
} limp_t;

typedef struct _TYPE_FIELDS {
   tty_t tty;

   struct _SYM_TABLE_NODE *pstnIdent;         /* for named types */

   int   nEnumConst;
   int   tredefStatus;
   valn_t *pvalnEnums;
   valn_t *pvalnPreviousEnums;

   struct _TYPE_FIELDS *ptfiBitmaskBaseType;  /* for bitmasks */

   struct _TYPE_FIELDS *ptfiArrayEltType;     /* for arrays */
   struct _TYPE_FIELDS *ptfiArrayIndexType;   /* for arrays */
} tfi_t;

typedef struct _ELEMENT_DESCRIPTOR {
   struct _ELEMENT_DESCRIPTOR *pedNext;
   valhdr_t valhdrValue;   /* current value of this variable elt */
   valhdr_t valhdrLimit;   /* current limits on this variable elt */
   char     *pchIndexName;
   tfi_t    *ptfi;
} ed_t;

typedef struct _VAR_FIELDS {
   ed_t     *pedList;
   tfi_t    *ptfi;
} vfi_t;

typedef struct _ATTR_FIELD_HDR {
   int nAttr; 
   union {
      struct _ATTR_FIELDS *pafi;
      char **dmppch;
   } u;
} afih_t;

typedef struct _ATTR_FIELDS {
   struct _ATTR_FIELDS *pafiNext;
   char *pchAttrVal; 
} afi_t;

typedef struct _ENUM_FIELDS {
/*   valhdr_t valhdrIndex;    *//* always enu */
   tfi_t    *ptfiBaseType;
} efi_t;

typedef struct _SYM_TABLE_NODE {
   ity_t ity;
   int   fExpected;

   int	 fRHS;	/* for vars used on RHS of an expression */
   char  *pchName;
   union {
      tfi_t  tfi;
      vfi_t  vfi;
      efi_t  efi;
      afih_t afih;
   } u;
   limp_t  *plimpListParse;
   struct _SYM_TABLE_NODE *pstnNext; 
   struct _SYM_TABLE_NODE *pstnTempNext; 
   struct _SYM_TABLE_NODE *pstnHashNext; 
} stn_t;

typedef struct _INDEX_EXPR_TYPE {
   stn_t *pstn;
   int   ietType; /* 0 == none, 1 == pstn, 2 == all */
} iet_t;

typedef union _TOKVAL {
   int      iValue;
   double   rValue;

   char     *pch;
   stn_t    *pstnA;
   val_t    *pval;
   tfi_t    *ptfi;
   vfi_t    *pvfi;
   int	    idxVal;
   valhdr_t *pvalhdr;
   valn_t   *pvaln;
   limp_t   *plimp;
} tokval_t;

#define YYSTYPE tokval_t

/* ----------------------------------------- */

extern stn_t *kapi_pstnLookup( knobs_t *pknobs, char *pch );
extern stn_t *kapi_pstnLookup_noadd( knobs_t *pknobs, char *pch );
extern stn_t *kapi_pstnDelete( knobs_t *pknobs, char *pch );

extern void kapi_ProcessTypeDecl( stn_t *pstnTypeIdent, valhdr_t *pvalhdrList, 
                                  limp_t *plimp );
extern tfi_t *ptfiProcessArrayTypeSpec( stn_t *pstnIndexType, tfi_t *ptfi );
extern void kapi_ProcessVarDecl( stn_t *pstnVar, tfi_t *ptfi );
extern void kapi_ProcessAssignment( stn_t *pstnLHSVar, char *pchIndex, 
                                    valhdr_t *pvalhdrRHS, limp_t *plimpList );
extern int  kapi_ProcessIndex( stn_t *pstnIndex );
extern void kapi_ProcessExpect( valhdr_t *pvalhdr, int typeexpect );

extern tfi_t *kapi_ptfiConstructArray( tfi_t *ptfiIndexType, tfi_t *ptfiElt );
extern tfi_t *kapi_ptfiConstructBitmask( tfi_t *ptfiBaseType );
extern tfi_t *kapi_ptfiCheckDeclaredType( stn_t *pstnName );

extern tfi_t *kapi_ptfiLookup( tty_t tty, void *pvoid );

extern stn_t *kapi_pstnReverseTmp( stn_t *pstnList );
extern valn_t * kapi_pvalnReverseTmp( valn_t *pvalnList );

extern void ProcessBitmaskExpr( valhdr_t *pvalhdrBitmask, valhdr_t *pvalhdrBits );
extern char *ity2pchname( ity_t ity );

extern valn_t *pvalnMakeString( char *pch );
extern valn_t *pvalnMakeEnum( char *pch );
extern valn_t *pvalnMakeIdent( char *pch );

extern valn_t *pvalnMakeInt( int i );
extern valn_t *pvalnMakeReal( double d );
extern valn_t *pvalnMakeBitmask( void );

extern limp_t *plimpProcessLimitType( valhdr_t *pvalhdr );
extern limp_t *plimpProcessLimitVariable( valhdr_t *pvalhdrTools, valhdr_t *pvalhdr );

extern limp_t * plimpLookup( limp_t *plimpList, char *pchToolname );
extern valhdr_t *pvalhdrMake( tty_t tty, vals_t vals, valn_t *pvalnList );
extern char * kapi_enumname( knobs_t *pknobs, tfi_t *ptfi, int enumconst );

extern int pstnEnum2idx( stn_t *pstnEnum );

extern void kapi_LookUpIdent_valhdr(  stn_t *pstn, valhdr_t *pvalhdr, int iPos );

extern void kapi_LookUpIdent_val(  stn_t *pstn, val_t *pval, int iPos );
extern void kapi_LookUpVariable_val(  stn_t *pstn, val_t *pval, int iPos );

extern bv_t *pbvBuild4valhdr( stn_t *pstnVar, valhdr_t *pvalhdr );
extern ed_t *pedMake( char *pchIndexName, tfi_t *ptfi );
extern ed_t *pedLookup4pstn( stn_t *pstnLHSVar, char *pchIndex );
extern ed_t *pedLookup4ped( ed_t *ped, char *pchIndex );
extern int  idxped4pstn( stn_t *pstnLHSVar, char *pchIndex );

extern ed_t   *pedGetIth( stn_t *pstnLHSVar, int iIndex );
extern valn_t *pvalnGetIth( valn_t *pvalnIn, int iIndex );
extern char * pchTranslateBypass2Intracluster( knobs_t *pknobs, char *pchIn );

extern kapi_ut_t       utInterpretPortName( char *pchPort, int *piport );
extern kapi_ut_t       utInterpretCportName( char *pchPort, int *piport );
extern kapi_cluster_t  clusterInterpretpchclr( char *pchClr );


extern int yyparse();
extern void kapi_ParseFile( knobs_t *pknobs, FILE *fp );
extern void kapi_ProcessAttrDecl( stn_t *pstnAttr, char *pchAttr );
extern void Declare_cluster0CportMask( stn_t *pstn );
extern void kapi_LookUpVariable_valhdr( stn_t *pstn, valhdr_t *pvalhdr, int iPos );



extern void kapi_ProcessArithmeticAssignment( stn_t *pstnLHSVar, char *pchIndex, valhdr_t *pvalhdrRHS);
extern void kapi_ProcessAppendDecl( stn_t *pstnType, valhdr_t *pvalhdrIdents);
extern valn_t *pvalnMakeArithmeticOp( char *pch );
extern void pvalhdrCopy( valhdr_t *pvalhdrTarget, valhdr_t *pvalhdrSource );



#endif
