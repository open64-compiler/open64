/*
 * Copyright 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
 */

/*

  Copyright (C) 2000, 2001 Silicon Graphics, Inc.  All Rights Reserved.

  This program is free software; you can redistribute it and/or modify it
  under the terms of version 2 of the GNU General Public License as
  published by the Free Software Foundation.

  This program is distributed in the hope that it would be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  

  Further, this software is distributed without any warranty that it is
  free of the rightful claim of any third person regarding infringement 
  or the like.  Any license provided herein, whether implied or 
  otherwise, applies only to this software file.  Patent licenses, if 
  any, provided herein do not apply to combinations of this program with 
  other software, or any other product whatsoever.  

  You should have received a copy of the GNU General Public License along
  with this program; if not, write the Free Software Foundation, Inc., 59
  Temple Place - Suite 330, Boston MA 02111-1307, USA.

  Contact information:  Silicon Graphics, Inc., 1600 Amphitheatre Pky,
  Mountain View, CA 94043, or:

  http://www.sgi.com

  For further information regarding this notice, see:

  http://oss.sgi.com/projects/GenInfo/NoticeExplan

*/


/* ====================================================================
 * ====================================================================
 *
 * Module: wn2f_expr.c
 * $Revision: 1.15 $
 * $Date: 05/12/05 08:59:32-08:00 $
 * $Author: bos@eng-24.pathscale.com $
 * $Source: /scratch/mee/2.4-65/kpro64-pending/be/whirl2f/SCCS/s.wn2f_expr.cxx $
 *
 * Revision history:
 *  12-Apr-95 - Original Version
 *
 * Description:
 *
 *   Translate a WN expression subtree to Fortran by means of an inorder 
 *   recursive descent traversal of the WHIRL IR.  Note that the routines
 *   to handle statements and loads/stores are in separate source files.
 *   Recursive translation of WN nodes should only use WN2F_Translate()!
 *
 * ====================================================================
 * ====================================================================
 */

#ifdef _KEEP_RCS_ID
/*REFERENCED*/
static char *rcs_id = "$Source: /scratch/mee/2.4-65/kpro64-pending/be/whirl2f/SCCS/s.wn2f_expr.cxx $ $Revision: 1.15 $";
#endif

#include "whirl2f_common.h"
#include "PUinfo.h"          /* In be/whirl2c directory */
#include "tcon2f.h"
#include "wn2f.h"
#include "ty2f.h"
#include "st2f.h"
#include "wn2f_load_store.h"
#include "intrn_info.h"          /* INTR macros */

/*---- Fortran names for binary and unary arithmetic operations -------*/
/*---------------------------------------------------------------------*/


/* The builtin Fortran operations will begin with a special character
 * or an alphabetic character, where those beginning with an alphabetic
 * character will be applied like functions and all others will be
 * applied in usual infix format.  When a name begins with '_', it is
 * a whirl2f special symbol to be applied like a function.  It will
 * be implemented in a library made available to be linked in with 
 * compiled whirl2f code.
 */
#define WN2F_IS_ALPHABETIC(opc) \
   ((Opc_Fname[opc][0]>='a' && Opc_Fname[opc][0]<='z') || \
    (Opc_Fname[opc][0]>='A' && Opc_Fname[opc][0]<='Z') || \
    (Opc_Fname[opc][0]=='_'))

#define WN2F_IS_INFIX_OP(opc) \
   (Opc_Fname[opc]!=NULL && !WN2F_IS_ALPHABETIC(opc))

#define WN2F_IS_FUNCALL_OP(opc) \
   (Opc_Fname[opc]!=NULL && WN2F_IS_ALPHABETIC(opc))


/* Mapping from opcodes to Fortran names for arithmetic/logical 
 * operations.  An empty (NULL) name will occur for non-
 * arithmetic/logical opcodes, which must be handled by special
 * handler-functions.  This mapping is dynamically initialized,
 * based on Fname_Map[], in WN2F_Expr_Initialize().
 */
#define NUMBER_OF_OPCODES (OPCODE_LAST+1)
static const char *Opc_Fname[NUMBER_OF_OPCODES];
   

typedef struct Fname_PartialMap
{
   OPCODE      opc;
   const char *fname;
} FNAME_PARTIALMAP;

#define NUMBER_OF_FNAME_PARTIALMAPS \
   sizeof(Fname_Map) / sizeof(FNAME_PARTIALMAP)

static const FNAME_PARTIALMAP Fname_Map[] =
{
  {OPC_U8NEG, "-"},
  {OPC_FQNEG, "-"},
  {OPC_I8NEG, "-"},
  {OPC_U4NEG, "-"},
  {OPC_CQNEG, "-"},
  {OPC_F8NEG, "-"},
  {OPC_C8NEG, "-"},
  {OPC_I4NEG, "-"},
  {OPC_F4NEG, "-"},
  {OPC_C4NEG, "-"},
  {OPC_I4ABS, "ABS"},
  {OPC_F4ABS, "ABS"},
  {OPC_FQABS, "ABS"},
  {OPC_I8ABS, "ABS"},
  {OPC_F8ABS, "ABS"},
  {OPC_F4SQRT, "SQRT"},
  {OPC_C4SQRT, "SQRT"},
  {OPC_FQSQRT, "SQRT"},
  {OPC_CQSQRT, "SQRT"},
  {OPC_F8SQRT, "SQRT"},
  {OPC_C8SQRT, "SQRT"},
  {OPC_I4F4RND, "JNINT"},
  {OPC_I4FQRND, "JIQNNT"},
  {OPC_I4F8RND, "JIDNNT"},
  {OPC_U4F4RND, "JNINT"},
  {OPC_U4FQRND, "JIQNNT"},
  {OPC_U4F8RND, "JIDNNT"},
  {OPC_I8F4RND, "KNINT"},
  {OPC_I8FQRND, "KIQNNT"},
  {OPC_I8F8RND, "KIDNNT"},
  {OPC_U8F4RND, "KNINT"},
  {OPC_U8FQRND, "KIQNNT"},
  {OPC_U8F8RND, "KIDNNT"},
  {OPC_I4F4TRUNC, "JINT"},
  {OPC_I4FQTRUNC, "JIQINT"},
  {OPC_I4F8TRUNC, "JIDINT"},
  {OPC_U4F4TRUNC, "JINT"},
  {OPC_U4FQTRUNC, "JIQINT"},
  {OPC_U4F8TRUNC, "JIDINT"},
  {OPC_I8F4TRUNC, "KINT"},
  {OPC_I8FQTRUNC, "KIQINT"},
  {OPC_I8F8TRUNC, "KIDINT"},
  {OPC_U8F4TRUNC, "KINT"},
  {OPC_U8FQTRUNC, "KIQINT"},
  {OPC_U8F8TRUNC, "KIDINT"},
  {OPC_I4F4CEIL, "OPC_I4F4CEIL"},
  {OPC_I4FQCEIL, "OPC_I4FQCEIL"},
  {OPC_I4F8CEIL, "OPC_I4F8CEIL"},
  {OPC_I8F4CEIL, "OPC_I8F4CEIL"},
  {OPC_I8FQCEIL, "OPC_I8FQCEIL"},
  {OPC_I8F8CEIL, "OPC_I8F8CEIL"},
  {OPC_I4F4FLOOR, "OPC_I4F4FLOOR"},
  {OPC_I4FQFLOOR, "OPC_I4FQFLOOR"},
  {OPC_I4F8FLOOR, "OPC_I4F8FLOOR"},
  {OPC_I8F4FLOOR, "OPC_I8F4FLOOR"},
  {OPC_I8FQFLOOR, "OPC_I8FQFLOOR"},
  {OPC_I8F8FLOOR, "OPC_I8F8FLOOR"},
  {OPC_I4BNOT, "NOT"},
  {OPC_U8BNOT, "NOT"},
  {OPC_I8BNOT, "NOT"},
  {OPC_U4BNOT, "NOT"},
// >> WHIRL 0.30: replace OPC_LNOT by OPC_B and OPC_I4 variant
// TODO WHIRL 0.30: get rid of OPC_I4 variant
  {OPC_BLNOT, ".NOT."},
  {OPC_I4LNOT, ".NOT."},
// << WHIRL 0.30: replace OPC_LNOT by OPC_B and OPC_I4 variant
  {OPC_U8ADD, "+"},
  {OPC_FQADD, "+"},
  {OPC_I8ADD, "+"},
  {OPC_U4ADD, "+"},
  {OPC_CQADD, "+"},
  {OPC_F8ADD, "+"},
  {OPC_C8ADD, "+"},
  {OPC_I4ADD, "+"},
  {OPC_F4ADD, "+"},
  {OPC_C4ADD, "+"},
#ifdef TARG_X8664
  {OPC_V16I1ADD, "+"},
  {OPC_V16I2ADD, "+"},
  {OPC_V16I4ADD, "+"},
  {OPC_V16I8ADD, "+"},
  {OPC_V16F4ADD, "+"},
  {OPC_V16F8ADD, "+"},
  {OPC_V16C4ADD, "+"},
  {OPC_V16C8ADD, "+"},
  {OPC_V16I1SUB, "-"},
  {OPC_V16I2SUB, "-"},
  {OPC_V16I4SUB, "-"},
  {OPC_V16I8SUB, "-"},
  {OPC_V16F4SUB, "-"},
  {OPC_V16F8SUB, "-"},
  {OPC_V16C4SUB, "-"},
  {OPC_V16C8SUB, "-"},
  {OPC_V16F4MPY, "*"},
  {OPC_V16F4DIV, "/"},
  {OPC_V16F8MPY, "*"},
  {OPC_V16F8DIV, "/"},
  {OPC_V16C4MPY, "*"},
  {OPC_V16C4DIV, "/"},
  {OPC_V16C8MPY, "*"},
  {OPC_V16C8DIV, "/"},
  {OPC_V16F8F8REPLICA, "REPLICA"},
  {OPC_V16F4SQRT, "SQRT"},
  {OPC_V16F8SQRT, "SQRT"},
  {OPC_V16I1NEG, "-"},
  {OPC_V16I2NEG, "-"},
  {OPC_V16I4NEG, "-"},
  {OPC_V16I8NEG, "-"},
  {OPC_V16F4NEG, "-"},
  {OPC_V16F8NEG, "-"},
#endif /* TARG_X8664 */
  {OPC_U8SUB, "-"},
  {OPC_FQSUB, "-"},
  {OPC_I8SUB, "-"},
  {OPC_U4SUB, "-"},
  {OPC_CQSUB, "-"},
  {OPC_F8SUB, "-"},
  {OPC_C8SUB, "-"},
  {OPC_I4SUB, "-"},
  {OPC_F4SUB, "-"},
  {OPC_C4SUB, "-"},
  {OPC_U8MPY, "*"},
  {OPC_FQMPY, "*"},
  {OPC_I8MPY, "*"},
  {OPC_U4MPY, "*"},
  {OPC_CQMPY, "*"},
  {OPC_F8MPY, "*"},
  {OPC_C8MPY, "*"},
  {OPC_I4MPY, "*"},
  {OPC_F4MPY, "*"},
  {OPC_C4MPY, "*"},
  {OPC_U8DIV, "/"},
  {OPC_FQDIV, "/"},
  {OPC_I8DIV, "/"},
  {OPC_U4DIV, "/"},
  {OPC_CQDIV, "/"},
  {OPC_F8DIV, "/"},
  {OPC_C8DIV, "/"},
  {OPC_I4DIV, "/"},
  {OPC_F4DIV, "/"},
  {OPC_C4DIV, "/"},
  {OPC_I4MOD, "MOD"},
  {OPC_U8MOD, "MOD"},
  {OPC_I8MOD, "MOD"},
  {OPC_U8MOD, "MOD"},
  {OPC_U4MOD, "MOD"},
  {OPC_I4REM, "MOD"},
  {OPC_U8REM, "MOD"},
  {OPC_I8REM, "MOD"},
  {OPC_U4REM, "MOD"},
  {OPC_I4MAX, "MAX"},
  {OPC_U8MAX, "MAX"},
  {OPC_F4MAX, "MAX"},
  {OPC_FQMAX, "MAX"},
  {OPC_I8MAX, "MAX"},
  {OPC_U4MAX, "MAX"},
  {OPC_F8MAX, "MAX"},
  {OPC_I4MIN, "MIN"},
  {OPC_U8MIN, "MIN"},
  {OPC_F4MIN, "MIN"},
  {OPC_FQMIN, "MIN"},
  {OPC_I8MIN, "MIN"},
  {OPC_U4MIN, "MIN"},
  {OPC_F8MIN, "MIN"},
#ifdef TARG_X8664
  {OPC_V16F4MIN, "MIN"},
  {OPC_V16F8MIN, "MIN"},
  {OPC_V16F4MAX, "MAX"},
  {OPC_V16F8MAX, "MAX"},
  {OPC_V16I1MAX, "MAX"},
  {OPC_V16I2MAX, "MAX"},
  {OPC_V16I4MAX, "MAX"},
  {OPC_V16I1MIN, "MIN"},
  {OPC_V16I2MIN, "MIN"},
  {OPC_V16I4MIN, "MIN"},
#endif
  {OPC_I4BAND, "IAND"},
  {OPC_U8BAND, "IAND"},
  {OPC_I8BAND, "IAND"},
  {OPC_U4BAND, "IAND"},
  {OPC_I4BIOR, "IOR"},
  {OPC_U8BIOR, "IOR"},
  {OPC_I8BIOR, "IOR"},
  {OPC_U4BIOR, "IOR"},
  {OPC_I4BXOR, "IEOR"},
  {OPC_U8BXOR, "IEOR"},
  {OPC_I8BXOR, "IEOR"},
  {OPC_U4BXOR, "IEOR"},
// >> WHIRL 0.30: replaced OPC_{LAND,LIOR,CAND,CIOR} by OPC_B and OPC_I4 variants
// TODO WHIRL 0.30: get rid of OPC_I4 variants
  {OPC_BLAND, ".AND."},
  {OPC_I4LAND, ".AND."},
  {OPC_BLIOR, ".OR."},
  {OPC_I4LIOR, ".OR."},
  {OPC_BCAND, ".AND."},
  {OPC_I4CAND, ".AND."},
  {OPC_BCIOR, ".OR."},
  {OPC_I4CIOR, ".OR."},
// << WHIRL 0.30: replaced OPC_{LAND,LIOR,CAND,CIOR} by OPC_B and OPC_I4 variants
  {OPC_I4SHL, "ISHIFT"},
  {OPC_U8SHL, "ISHIFT"},
  {OPC_I8SHL, "ISHIFT"},
  {OPC_U4SHL, "ISHIFT"},
  {OPC_I4ASHR, "IASHR"},
  {OPC_U8ASHR, "IASHR"},
  {OPC_I8ASHR, "IASHR"},
  {OPC_U4ASHR, "IASHR"},
// >> WHIRL 0.30: replaced OPC_T1{EQ,NE,GT,GE,LT,LE} by OPC_B and OPC_I4 variants
// TODO WHIRL 0.30: get rid of OPC_I4 variants
  {OPC_BU8EQ, ".EQ."},
  {OPC_BFQEQ, ".EQ."},
  {OPC_BI8EQ, ".EQ."},
  {OPC_BU4EQ, ".EQ."},
  {OPC_BCQEQ, ".EQ."},
  {OPC_BF8EQ, ".EQ."},
  {OPC_BC8EQ, ".EQ."},
  {OPC_BI4EQ, ".EQ."},
  {OPC_BF4EQ, ".EQ."},
  {OPC_BC4EQ, ".EQ."},
  {OPC_BU8NE, ".NE."},
  {OPC_BFQNE, ".NE."},
  {OPC_BI8NE, ".NE."},
  {OPC_BU4NE, ".NE."},
  {OPC_BCQNE, ".NE."},
  {OPC_BF8NE, ".NE."},
  {OPC_BC8NE, ".NE."},
  {OPC_BI4NE, ".NE."},
  {OPC_BF4NE, ".NE."},
  {OPC_BC4NE, ".NE."},
  {OPC_BI4GT, ".GT."},
  {OPC_BU8GT, ".GT."},
  {OPC_BF4GT, ".GT."},
  {OPC_BFQGT, ".GT."},
  {OPC_BI8GT, ".GT."},
  {OPC_BU4GT, ".GT."},
  {OPC_BF8GT, ".GT."},
#ifdef TARG_X8664
  {OPC_V16I8V16F8GT, ".GT."},
  {OPC_V16I8V16F8LT, ".LT."},
  {OPC_V16I8V16F8GE, ".GE."},
  {OPC_V16I8V16F8LE, ".LE."},
  {OPC_V16I8V16F8NE, ".NE."},
  {OPC_V16I8V16F8EQ, ".EQ."},
#endif
  {OPC_BI4GE, ".GE."},
  {OPC_BU8GE, ".GE."},
  {OPC_BF4GE, ".GE."},
  {OPC_BFQGE, ".GE."},
  {OPC_BI8GE, ".GE."},
  {OPC_BU4GE, ".GE."},
  {OPC_BF8GE, ".GE."},
  {OPC_BI4LT, ".LT."},
  {OPC_BU8LT, ".LT."},
  {OPC_BF4LT, ".LT."},
  {OPC_BFQLT, ".LT."},
  {OPC_BI8LT, ".LT."},
  {OPC_BU4LT, ".LT."},
  {OPC_BF8LT, ".LT."},
  {OPC_BI4LE, ".LE."},
  {OPC_BU8LE, ".LE."},
  {OPC_BF4LE, ".LE."},
  {OPC_BFQLE, ".LE."},
  {OPC_BI8LE, ".LE."},
  {OPC_BU4LE, ".LE."},
  {OPC_BF8LE, ".LE."},
  {OPC_I4U8EQ, ".EQ."},
  {OPC_I4FQEQ, ".EQ."},
  {OPC_I4I8EQ, ".EQ."},
  {OPC_I4U4EQ, ".EQ."},
  {OPC_I4CQEQ, ".EQ."},
  {OPC_I4F8EQ, ".EQ."},
  {OPC_I4C8EQ, ".EQ."},
  {OPC_I4I4EQ, ".EQ."},
  {OPC_I4F4EQ, ".EQ."},
  {OPC_I4C4EQ, ".EQ."},
  {OPC_I8I4EQ, ".EQ."},
  {OPC_I8I4NE, ".NE."},
  {OPC_I4U8NE, ".NE."},
  {OPC_I4FQNE, ".NE."},
  {OPC_I4I8NE, ".NE."},
  {OPC_I4U4NE, ".NE."},
  {OPC_I4CQNE, ".NE."},
  {OPC_I4F8NE, ".NE."},
  {OPC_I4C8NE, ".NE."},
  {OPC_I4I4NE, ".NE."},
  {OPC_I4F4NE, ".NE."},
  {OPC_I4C4NE, ".NE."},
  {OPC_I4I4GT, ".GT."},
  {OPC_I4U8GT, ".GT."},
  {OPC_I4F4GT, ".GT."},
  {OPC_I4FQGT, ".GT."},
  {OPC_I4I8GT, ".GT."},
  {OPC_I4U4GT, ".GT."},
  {OPC_I4F8GT, ".GT."},
  {OPC_I4I4GE, ".GE."},
  {OPC_I4U8GE, ".GE."},
  {OPC_I4F4GE, ".GE."},
  {OPC_I4FQGE, ".GE."},
  {OPC_I4I8GE, ".GE."},
  {OPC_I4U4GE, ".GE."},
  {OPC_I4F8GE, ".GE."},
  {OPC_I4I4LT, ".LT."},
  {OPC_I4U8LT, ".LT."},
  {OPC_I4F4LT, ".LT."},
  {OPC_I4FQLT, ".LT."},
  {OPC_I4I8LT, ".LT."},
  {OPC_I4U4LT, ".LT."},
  {OPC_I4F8LT, ".LT."},
  {OPC_I4I4LE, ".LE."},
  {OPC_I4U8LE, ".LE."},
  {OPC_I4F4LE, ".LE."},
  {OPC_I4FQLE, ".LE."},
  {OPC_I4I8LE, ".LE."},
  {OPC_I4U4LE, ".LE."},
  {OPC_I4F8LE, ".LE."}
// << WHIRL 0.30: replaced OPC_T1{EQ,NE,GT,GE,LT,LE} by OPC_B and OPC_I4 variants
}; /* Fname_Map */


/*------------------------- Value Conversions -------------------------*/
/*---------------------------------------------------------------------*/

/* Create a mapping from a pair of MTYPEs to the Fortran intrinsic
 * or builtin operation that carries out the conversion.  NULL means
 * that either the conversion is redundant and can be ignored or there
 * is no way we can do it.
 */
static const char *Conv_Op[MTYPE_LAST+1][MTYPE_LAST+1];

typedef struct Conv_Op
{
   MTYPE       from, to;
   const char *name;
} CONV_OP;

#define NUMBER_OF_CONV_OPS sizeof(Conv_Op_Map)/sizeof(CONV_OP)

static const CONV_OP Conv_Op_Map[] = 
{
   /* from   |   to   |   op-name */

   /* Only consider conversion to ptr sized unsigned numbers 
    * valid in Fortran.
    */
   {MTYPE_I1, MTYPE_U4, "JZEXT"},
   {MTYPE_I2, MTYPE_U4, "JZEXT"},
   {MTYPE_I4, MTYPE_U4, "JZEXT"},
   {MTYPE_I8, MTYPE_U4, "JZEXT"},
   /*{MTYPE_U1, MTYPE_U4, ""},*/
   /*{MTYPE_U2, MTYPE_U4, ""},*/
   /*{MTYPE_U4, MTYPE_U4, ""},*/
   {MTYPE_U8, MTYPE_U4, "JZEXT"},

   {MTYPE_I1, MTYPE_U8, "KZEXT"},
   {MTYPE_I2, MTYPE_U8, "KZEXT"},
   {MTYPE_I4, MTYPE_U8, "KZEXT"},
   {MTYPE_I8, MTYPE_U8, "KZEXT"},
   /*{MTYPE_U1, MTYPE_U8, ""},*/
   /*{MTYPE_U2, MTYPE_U8, ""},*/
   /*{MTYPE_U4, MTYPE_U8, ""},*/
   /*{MTYPE_U8, MTYPE_U8, ""},*/

   /*{MTYPE_I1, MTYPE_I1, ""},*/
   {MTYPE_I2, MTYPE_I1, "INT1"},
   {MTYPE_I4, MTYPE_I1, "INT1"},
   {MTYPE_I8, MTYPE_I1, "INT1"},
   /*{MTYPE_U1, MTYPE_I1, ""},*/
   {MTYPE_U2, MTYPE_I1, "INT1"},
   {MTYPE_U4, MTYPE_I1, "INT1"},
   {MTYPE_U8, MTYPE_I1, "INT1"},
   {MTYPE_F4, MTYPE_I1, "INT1"},
   {MTYPE_F8, MTYPE_I1, "INT1"},
   {MTYPE_FQ, MTYPE_I1, "INT1"},

   {MTYPE_I1, MTYPE_I2, "INT2"},
   /*{MTYPE_I2, MTYPE_I2, ""},*/
   {MTYPE_I4, MTYPE_I2, "INT2"},
   {MTYPE_I8, MTYPE_I2, "INT2"},
   {MTYPE_U1, MTYPE_I2, "INT2"},
   /*{MTYPE_U2, MTYPE_I2, ""},*/
   {MTYPE_U4, MTYPE_I2, "INT2"},
   {MTYPE_U8, MTYPE_I2, "INT2"},
   {MTYPE_F4, MTYPE_I2, "INT2"},
   {MTYPE_F8, MTYPE_I2, "INT2"},
   {MTYPE_FQ, MTYPE_I2, "INT2"},

   {MTYPE_I1, MTYPE_I4, "INT4"},
   {MTYPE_I2, MTYPE_I4, "INT4"},
   /*{MTYPE_I4, MTYPE_I4, ""},*/
   {MTYPE_I8, MTYPE_I4, "INT4"},
   {MTYPE_U1, MTYPE_I4, "INT4"},
   {MTYPE_U2, MTYPE_I4, "INT4"},
   /*{MTYPE_U4, MTYPE_I4, ""},*/
   {MTYPE_U8, MTYPE_I4, "INT4"},
   {MTYPE_F4, MTYPE_I4, "INT4"},
   {MTYPE_F8, MTYPE_I4, "INT4"},
   {MTYPE_FQ, MTYPE_I4, "INT4"},

   {MTYPE_I1, MTYPE_I8, "INT8"},
   {MTYPE_I2, MTYPE_I8, "INT8"},
   {MTYPE_I4, MTYPE_I8, "INT8"},
   /*{MTYPE_I8, MTYPE_I8, ""},*/
   {MTYPE_U1, MTYPE_I8, "INT8"},
   {MTYPE_U2, MTYPE_I8, "INT8"},
   {MTYPE_U4, MTYPE_I8, "INT8"},
   /*{MTYPE_U8, MTYPE_I8, ""},*/
   {MTYPE_F4, MTYPE_I8, "INT8"},
   {MTYPE_F8, MTYPE_I8, "INT8"},
   {MTYPE_FQ, MTYPE_I8, "INT8"},

   {MTYPE_I1, MTYPE_F4, "REAL"},
   {MTYPE_I2, MTYPE_F4, "REAL"},
   {MTYPE_I4, MTYPE_F4, "REAL"},
   {MTYPE_I8, MTYPE_F4, "REAL"},
   {MTYPE_U1, MTYPE_F4, "REAL"},
   {MTYPE_U2, MTYPE_F4, "REAL"},
   {MTYPE_U4, MTYPE_F4, "REAL"},
   {MTYPE_U8, MTYPE_F4, "REAL"},
   /*{MTYPE_F4, MTYPE_F4, ""},*/
   {MTYPE_F8, MTYPE_F4, "REAL"},
   {MTYPE_FQ, MTYPE_F4, "REAL"},

   {MTYPE_I1, MTYPE_F8, "DBLE"},
   {MTYPE_I2, MTYPE_F8, "DBLE"},
   {MTYPE_I4, MTYPE_F8, "DBLE"},
   {MTYPE_I8, MTYPE_F8, "DBLE"},
   {MTYPE_U1, MTYPE_F8, "DBLE"},
   {MTYPE_U2, MTYPE_F8, "DBLE"},
   {MTYPE_U4, MTYPE_F8, "DBLE"},
   {MTYPE_U8, MTYPE_F8, "DBLE"},
   {MTYPE_F4, MTYPE_F8, "DBLE"},
   /*{MTYPE_F8, MTYPE_F8, ""},*/
   {MTYPE_FQ, MTYPE_F8, "DBLE"},

   {MTYPE_I1, MTYPE_FQ, "QEXT"},
   {MTYPE_I2, MTYPE_FQ, "QEXT"},
   {MTYPE_I4, MTYPE_FQ, "QEXT"},
   {MTYPE_I8, MTYPE_FQ, "QEXT"},
   {MTYPE_U1, MTYPE_FQ, "QEXT"},
   {MTYPE_U2, MTYPE_FQ, "QEXT"},
   {MTYPE_U4, MTYPE_FQ, "QEXT"},
   {MTYPE_U8, MTYPE_FQ, "QEXT"},
   {MTYPE_F4, MTYPE_FQ, "QEXT"},
   {MTYPE_F8, MTYPE_FQ, "QEXT"}
   /*{MTYPE_FQ, MTYPE_FQ, ""}*/
}; /* Conv_Op_Map */


static void
WN2F_Convert(TOKEN_BUFFER tokens,
	     MTYPE        from_mtype,
	     MTYPE        to_mtype)
{
   /* We emit a warning message for conversions not covered (TODO: put
    * this warning under a command-line option).  Converts the expression
    * in the given token-buffer to the given mtype.
    */
   Prepend_Token_Special(tokens, '(');
   if (Conv_Op[from_mtype][to_mtype] == NULL)
   {
      ASSERT_WARN(Conv_Op[from_mtype][to_mtype] != NULL,
		  (DIAG_W2F_UNEXPECTED_CVT, 
		   MTYPE_name(from_mtype), MTYPE_name(to_mtype), 
		   "WN2F_Convert"));
      Prepend_Token_String(tokens, "WN2F_Convert");
   }
   else
   {
      /* Note all these are intrinsics in the mongoose compiler and
       * need not be declared.
       */
      Prepend_Token_String(tokens, Conv_Op[from_mtype][to_mtype]);
   }
   Append_Token_Special(tokens, ')');
} /* WN2F_Convert */


/*------------------------- Utility Functions -------------------------*/
/*---------------------------------------------------------------------*/

static WN2F_STATUS
WN2F_Translate_Arithmetic_Operand(TOKEN_BUFFER tokens,
				  WN          *opnd, 
				  TY_IDX       assumed_ty, 
				  BOOL         call_by_value,
				  WN2F_CONTEXT context)
{
   /* Translate an operand to a function or built-in operator invocation,
    * based on whether the context indicates that we have call-by-value
    * or call-by-reference.  Also, the context indicates what type of
    * argument we expect.
    */
   
   /* TODO(?): Type promotion
    *
    * TY          *opnd_ty = WN_Tree_Type(opnd);
    * if (!WN2F_arithmetic_compatible_types(assumed_ty, opnd_ty))
    * {
    *    WN2F_prepend_cast(opnd, assumed_ty, context);
    * }
    */

   /* We do not handle substring expressions here, and assume any
    * such expression will be dispatched to a specialty routine
    * such as WN2F_Intr_Infix_SubstrExpr().
    */
   ASSERT_DBG_WARN(!TY_Is_Character_Reference(assumed_ty) &&
		   !TY_Is_Chararray_Reference(assumed_ty),
		   (DIAG_W2F_UNEXPECTED_SUBSTRING_REF, 
		    "WN2F_Translate_Arithmetic_Operand()"));
   
   if (!call_by_value)
   {
      WN2F_Offset_Memref(tokens, 
			 opnd,                   /* address expression */
			 assumed_ty,             /* address type */
			 TY_pointed(assumed_ty), /* object type */
			 0,                      /* offset from address */
			 context);
   }
   else
   {
      WN2F_translate(tokens, opnd, context);
   }
   
   return EMPTY_WN2F_STATUS;
} /* WN2F_Translate_Arithmetic_Operand */


static WN2F_STATUS
WN2F_Infix_Op(TOKEN_BUFFER tokens, 
	      OPCODE       opcode,
	      TY_IDX       result_ty,
	      WN          *wn0,
	      WN          *wn1,
	      WN2F_CONTEXT context)
{
   /* Infix Fortran operator.  Only string argument are passed by
    * reference; all other argument types are passed by value.
    */
   const BOOL   parenthesize = !WN2F_CONTEXT_no_parenthesis(context);
   const BOOL   binary_op = (wn0 != NULL);

   TY_IDX      wn0_ty;       /* Expected type of wn0 */
   TY_IDX      wn1_ty;       /* Expected type of wn1 */

   /* Ensure that subexpressions are parenthesized */
   reset_WN2F_CONTEXT_no_parenthesis(context);

   /* Get the expected types for the two operands, dependent on whether
    * or not we have a descriptor type.
    */
   if (OPCODE_desc(opcode) == MTYPE_V)
      wn0_ty = wn1_ty = result_ty;
   else
      wn0_ty = wn1_ty = Stab_Mtype_To_Ty(OPCODE_desc(opcode));

   if (parenthesize)
      Append_Token_Special(tokens, '(');

   /* First operand */
   if (binary_op)
      WN2F_Translate_Arithmetic_Operand(tokens, wn0, wn0_ty, 
					TRUE/*call-by-value*/,
					context);
   
   /* Operation */
   Append_Token_String(tokens, Opc_Fname[opcode]);

   /* Second operand, or only operand for unary operation */
   WN2F_Translate_Arithmetic_Operand(tokens, wn1, wn1_ty, 
				     TRUE/*call-by-value*/,
				     context);

   if (parenthesize)
      Append_Token_Special(tokens, ')');

   return EMPTY_WN2F_STATUS;
} /* WN2F_Infix_Op */

static WN2F_STATUS
WN2F_Funcall_Op(TOKEN_BUFFER tokens, 
		OPCODE       opcode,
		WN          *wn0, 
		WN          *wn1, 
		WN2F_CONTEXT context)
{
   /* Prefix Fortran operator.  Only string argument are passed by
    * reference; all other argument types are passed by value.
    */
   const BOOL   binary_op = (wn0 != NULL);

   TY_IDX const   rty = Stab_Mtype_To_Ty(OPCODE_rtype(opcode));
   TY_IDX         dty = Stab_Mtype_To_Ty(OPCODE_desc(opcode));

   /* If there is no descriptor type, assume the operands should be
    * of the same type as the result.  The assumed type of the argument
    * will be the dty.
    */
   if (TY_kind(dty) == KIND_VOID)
      dty = rty;
   
   Append_Token_String(tokens, Opc_Fname[opcode]);
   Append_Token_Special(tokens, '(');
   
   /* No need to parenthesize subexpressions */
   set_WN2F_CONTEXT_no_parenthesis(context);

   /* First operand */
   if (binary_op)
   {
      WN2F_Translate_Arithmetic_Operand(tokens, wn0, dty,
					TRUE/*call-by-value*/,
					context);
      Append_Token_Special(tokens, ',');
   }

   /* Second operand, or only operand for unary operation */
   WN2F_Translate_Arithmetic_Operand(tokens, wn1, dty,
				     TRUE/*call-by-value*/,
				     context);

   Append_Token_Special(tokens, ')');
   return EMPTY_WN2F_STATUS;
} /* WN2F_Funcall_Op */


static WN2F_STATUS 
WN2F_Intr_Funcall(TOKEN_BUFFER tokens, 
		  WN          *wn,
		  const char  *func_name,
		  INT          first_arg_idx,
		  INT          last_arg_idx,
		  BOOL         call_by_value,
		  WN2F_CONTEXT context)
{
   /* An intrinsic operator expression to be emitted with function
    * call syntax.  All arguments are passed by value or by reference,
    * i.e. we never have some arguments passed by value and some by 
    * reference, unless we have explicit INTR_OPC_ADRTMP or 
    * INTR_OPC_VALTMP argument expressions. Note that we also
    * handle substring arguments here. 
    */
   INT arg_idx, implicit_args, total_implicit_args;
   TY_IDX opnd_type;

   /* Determine the number of implicit arguments appended to the end
    * of the argument list (i.e. string lengths).
    */
   for (arg_idx = first_arg_idx, total_implicit_args = 0; 
	arg_idx <= last_arg_idx - total_implicit_args; 
	arg_idx++)
   {
      opnd_type = WN_Tree_Type(WN_kid(wn, arg_idx));
      if (TY_Is_Character_Reference(opnd_type) ||
	  TY_Is_Chararray_Reference(opnd_type))
      {
	 total_implicit_args++;
      }
   }

   /* Append the function-name */
   Append_Token_String(tokens, func_name);
   
   /* Append the argument list to the function reference, skipping
    * implicit character-string-length arguments assumed to be the
    * last ones in the list (see also ST2F_func_header()).
    */
   Append_Token_Special(tokens, '(');
   set_WN2F_CONTEXT_no_parenthesis(context);
   for (arg_idx = first_arg_idx, implicit_args = 0; 
	arg_idx <= last_arg_idx - implicit_args; 
	arg_idx++)
   {
      opnd_type = WN_Tree_Type(WN_kid(wn, arg_idx));

      if (TY_Is_Character_Reference(opnd_type) ||
	  TY_Is_Chararray_Reference(opnd_type))
      {
	 implicit_args++;
	 WN2F_String_Argument(tokens,
			      WN_kid(wn, arg_idx), /* string base */
			      WN_kid(wn, 
				     last_arg_idx - 
				     (total_implicit_args - 
				      implicit_args)), /* string length */
			      context);
      }
      else
	 WN2F_Translate_Arithmetic_Operand(tokens,
					   WN_kid(wn, arg_idx),
					   opnd_type,
					   call_by_value,
					   context);

      if ((arg_idx+implicit_args) < WN_kid_count(wn) - 1)
	 Append_Token_Special(tokens, ',');
   }
   Append_Token_Special(tokens, ')');

   /* TODO: See if we need to cast the resultant value */

   return EMPTY_WN2F_STATUS;
} /* WN2F_Intr_Funcall */


static WN2F_STATUS
WN2F_Intr_Infix(TOKEN_BUFFER tokens, 
		const char  *op_name,
		WN          *opnd0,   /* NULL for unary operation */
		WN          *opnd1,
		BOOL         call_by_value,
		WN2F_CONTEXT context)
{
   /* An intrinsic operator expression to be emitted with infix operator
    * syntax.  Note that we have already determined what the two arguments
    * are, and any implicit argument have already been ignored.
    */
   const BOOL parenthesize = !WN2F_CONTEXT_no_parenthesis(context);
   const BOOL binary_op = (opnd0 != NULL);

   /* Ensure that subexpressions are parenthesized */
   reset_WN2F_CONTEXT_no_parenthesis(context);
   
   if (parenthesize)
      Append_Token_Special(tokens, '(');

   if (binary_op)
      WN2F_Translate_Arithmetic_Operand(tokens,
					opnd0,
					WN_Tree_Type(opnd0),
					call_by_value,
					context);
   Append_Token_String(tokens, op_name);
   WN2F_Translate_Arithmetic_Operand(tokens,
				     opnd1,
				     WN_Tree_Type(opnd1),
				     call_by_value,
				     context);
   if (parenthesize)
      Append_Token_Special(tokens, ')');

   return EMPTY_WN2F_STATUS;
} /* WN2F_Intr_Infix */


static WN2F_STATUS
WN2F_Binary_Substr_Op(TOKEN_BUFFER tokens, 
		      WN          *op_wn,   /* Top-level expression */
		      const char  *op_name, /* The builtin operator */
		      WN2F_CONTEXT context)
{
   /* Similar to WN2F_Infix_Op, but we expect the arguments to be
    * string expressions.
    */
   const BOOL parenthesize = !WN2F_CONTEXT_no_parenthesis(context);

   /* Ensure that subexpressions are parenthesized */
   reset_WN2F_CONTEXT_no_parenthesis(context);
   
   if (parenthesize)
      Append_Token_Special(tokens, '(');

   WN2F_String_Argument(tokens,
			WN_kid(op_wn, 0), /* string base */
			WN_kid(op_wn, 2), /* string length */
			context);
   Append_Token_String(tokens, op_name);
   WN2F_String_Argument(tokens,
			WN_kid(op_wn, 1), /* string base */
			WN_kid(op_wn, 3), /* string length */
			context);
   if (parenthesize)
      Append_Token_Special(tokens, ')');

   return EMPTY_WN2F_STATUS;
} /* WN2F_Binary_Substr_Op */


/*------------------------- Exported Functions ------------------------*/
/*---------------------------------------------------------------------*/


void WN2F_Expr_initialize(void)
{
   INT  map;

   /* Reset the Opc_Fname array.  This has already been
    * implicitly done by declaring it as static:
    *
    *    OPCODE   opc;
    *    for (opc = 0; opc < NUMBER_OF_OPCODES; opc++)
    *       Opc_Fname[opc] = NULL;
    *
    * Initialize the Opc_Fname array:
    */
   for (map = 0; map < NUMBER_OF_FNAME_PARTIALMAPS; map++)
      Opc_Fname[Fname_Map[map].opc] = Fname_Map[map].fname;

   /* Initialize the Conv_Op array (default value is NULL) */
   for (map = 0; map < NUMBER_OF_CONV_OPS; map++)
      Conv_Op[Conv_Op_Map[map].from][Conv_Op_Map[map].to] = 
	 Conv_Op_Map[map].name;

} /* WN2F_Expr_initialize */


void WN2F_Expr_finalize(void)
{
   /* Nothing to do for now */
} /* WN2F_Expr_finalize */


WN2F_STATUS
WN2F_binaryop(TOKEN_BUFFER tokens, WN *wn, WN2F_CONTEXT context)
{
   ASSERT_DBG_FATAL(WN_kid_count(wn) == 2, 
		    (DIAG_W2F_UNEXPECTED_NUM_KIDS, 
		     WN_kid_count(wn), 2, WN_opc_name(wn)));

   if (WN2F_IS_INFIX_OP(WN_opcode(wn)))
      WN2F_Infix_Op(tokens,
		    WN_opcode(wn),
		    WN_Tree_Type(wn),
		    WN_kid0(wn),
		    WN_kid1(wn),
		    context);
   else if (WN2F_IS_FUNCALL_OP(WN_opcode(wn)))
      WN2F_Funcall_Op(tokens,
		      WN_opcode(wn),
		      WN_kid0(wn),
		      WN_kid1(wn),
		      context);
   else
      ASSERT_DBG_FATAL(FALSE, (DIAG_W2F_UNEXPECTED_OPC, "WN2F_binaryop"));

   return EMPTY_WN2F_STATUS;
} /* WN2F_binaryop */


WN2F_STATUS
WN2F_unaryop(TOKEN_BUFFER tokens, WN *wn, WN2F_CONTEXT context)
{
   ASSERT_DBG_FATAL(WN_kid_count(wn) == 1, 
		    (DIAG_W2F_UNEXPECTED_NUM_KIDS, 
		     WN_kid_count(wn), 1, WN_opc_name(wn)));

   if (WN2F_IS_INFIX_OP(WN_opcode(wn)))
      WN2F_Infix_Op(tokens,
		    WN_opcode(wn),
		    WN_Tree_Type(wn),
		    NULL, /* No first operand */
		    WN_kid0(wn),
		    context);
   else if (WN2F_IS_FUNCALL_OP(WN_opcode(wn)))
      WN2F_Funcall_Op(tokens,
		      WN_opcode(wn),
		      NULL, /* No first operand */
		      WN_kid0(wn),
		      context);
   else
      ASSERT_DBG_FATAL(FALSE, (DIAG_W2F_UNEXPECTED_OPC, "WN2F_binaryop"));

   return EMPTY_WN2F_STATUS;
} /* WN2F_unaryop */


WN2F_STATUS 
WN2F_intrinsic_op(TOKEN_BUFFER tokens, WN *wn, WN2F_CONTEXT context)
{
   /* An intrinsic operator expression.  Generate the call as is,
    * regardless how the return value is returned, since we know
    * the consumer of the value is the surrounding expression.  This
    * call is not related to the call-info generated by PUinfo.
    * Note that either all or none of the arguments are call-by-value.
    */
   INT   first_arg_idx, last_arg_idx;
   BOOL  by_value;

   ASSERT_DBG_FATAL(WN_opc_operator(wn) == OPR_INTRINSIC_OP, 
		    (DIAG_W2F_UNEXPECTED_OPC, "WN2F_intrinsic_op"));

   by_value = INTRN_by_value(WN_intrinsic(wn));
   last_arg_idx = WN_kid_count(wn) - 1;
   first_arg_idx = 0; /* Assume we never return to first argument */

   /* Switch on WN_intrinsic(wn) to handle builtin fortran opcodes.
    */
   switch (WN_intrinsic(wn))
   {      
   case INTRN_I4EXPEXPR: 
   case INTRN_I8EXPEXPR:
   case INTRN_F4EXPEXPR:
   case INTRN_F8EXPEXPR:
   case INTRN_FQEXPEXPR:
   case INTRN_C4EXPEXPR:
   case INTRN_C8EXPEXPR:
   case INTRN_CQEXPEXPR:
   case INTRN_F4I4EXPEXPR:
   case INTRN_F4I8EXPEXPR:
   case INTRN_F8I4EXPEXPR:
   case INTRN_F8I8EXPEXPR:
   case INTRN_FQI4EXPEXPR:
   case INTRN_FQI8EXPEXPR:
   case INTRN_C4I4EXPEXPR:
   case INTRN_C4I8EXPEXPR:
   case INTRN_C8I4EXPEXPR:
   case INTRN_C8I8EXPEXPR:
   case INTRN_CQI4EXPEXPR:
   case INTRN_CQI8EXPEXPR:
      WN2F_Intr_Infix(tokens, 
		      "**", WN_kid0(wn), WN_kid1(wn), by_value, context);
      break;
      
   case INTRN_CEQEXPR:
      WN2F_Binary_Substr_Op(tokens, wn, ".EQ.", context);
      break;
   case INTRN_CNEEXPR:
      WN2F_Binary_Substr_Op(tokens, wn, ".NE.", context);
      break;
   case INTRN_CGEEXPR:
      WN2F_Binary_Substr_Op(tokens, wn, ".GE.", context);
      break;
   case INTRN_CGTEXPR:
      WN2F_Binary_Substr_Op(tokens, wn, ".GT.", context);
      break;
   case INTRN_CLEEXPR:
      WN2F_Binary_Substr_Op(tokens, wn, ".LE.", context);
      break;
   case INTRN_CLTEXPR:
      WN2F_Binary_Substr_Op(tokens, wn, ".LT.", context);
      break;

   case INTRN_U4I1ADRTMP: 
   case INTRN_U4I2ADRTMP: 
   case INTRN_U4I4ADRTMP:
   case INTRN_U4I8ADRTMP: 
   case INTRN_U4F4ADRTMP: 
   case INTRN_U4F8ADRTMP: 
   case INTRN_U4FQADRTMP:
   case INTRN_U4C4ADRTMP: 
   case INTRN_U4C8ADRTMP:
   case INTRN_U4CQADRTMP:
   case INTRN_U4VADRTMP :
   case INTRN_U8I1ADRTMP:
   case INTRN_U8I2ADRTMP:
   case INTRN_U8I4ADRTMP:
   case INTRN_U8I8ADRTMP: 
   case INTRN_U8F4ADRTMP: 
   case INTRN_U8F8ADRTMP: 
   case INTRN_U8FQADRTMP:
   case INTRN_U8C4ADRTMP: 
   case INTRN_U8C8ADRTMP: 
   case INTRN_U8CQADRTMP:
   case INTRN_U8VADRTMP:
      /* Implicit call by reference.  Emit the dereferenced parameter.
       */
      WN2F_translate(tokens, WN_kid0(wn), context);
      break;

   case INTRN_I4VALTMP:
   case INTRN_I8VALTMP: 
   case INTRN_F4VALTMP: 
   case INTRN_F8VALTMP: 
   case INTRN_FQVALTMP:
   case INTRN_C4VALTMP: 
   case INTRN_C8VALTMP:
   case INTRN_CQVALTMP:
      /* Call-by-value.  Assume the context determines when it is
       * necessary to put a %val qualifier around the argument.
       */
      WN2F_translate(tokens, WN_kid0(wn), context);
      break;     
      
   default:
      WN2F_Intr_Funcall(tokens,
			wn,
			WN_intrinsic_name((INTRINSIC) WN_intrinsic(wn)),
			first_arg_idx,
			last_arg_idx,
			by_value,
			context);
      break;
   } /*switch*/

   /* TODO: See if we need to cast the resultant value.
    * TY * return_ty = 
    *         WN_intrinsic_return_ty(WN_opcode(wn), WN_intrinsic(wn));
    */

   return EMPTY_WN2F_STATUS;
} /* WN2F_intrinsic_op */


WN2F_STATUS 
WN2F_tas(TOKEN_BUFFER tokens, WN *wn, WN2F_CONTEXT context)
{
   ASSERT_DBG_FATAL(WN_opc_operator(wn) == OPR_TAS, 
		    (DIAG_W2F_UNEXPECTED_OPC, "WN2F_tas"));

   /* Just ignore TAS operators for now.  TODO: make sure this
    * is always ok.
    */
   return WN2F_translate(tokens, WN_kid0(wn), context);
} /* WN2F_tas */


WN2F_STATUS 
WN2F_select(TOKEN_BUFFER tokens, WN *wn, WN2F_CONTEXT context)
{
   /* SELECT is almost the same as the F90 MERGE intrinsic, 
      so I will output it that way for now */
   
   Append_Token_String(tokens, "MERGE");
   Append_Token_Special(tokens, '(');
   WN2F_translate(tokens, WN_kid1(wn), context);
   Append_Token_Special(tokens, ',');

   WN2F_translate(tokens, WN_kid2(wn), context);
   Append_Token_Special(tokens, ',');

   WN2F_translate(tokens, WN_kid0(wn), context);
   
   Append_Token_Special(tokens, ')');

   return EMPTY_WN2F_STATUS;
} /* WN2F_select */


WN2F_STATUS 
WN2F_cvt(TOKEN_BUFFER tokens, WN *wn, WN2F_CONTEXT context)
{
   TOKEN_BUFFER expr_tokens = New_Token_Buffer();

   ASSERT_DBG_FATAL(WN_opc_operator(wn) == OPR_CVT, 
		    (DIAG_W2F_UNEXPECTED_OPC, "WN2F_cvt"));

   WN2F_translate(expr_tokens, WN_kid0(wn), context);
   WN2F_Convert(expr_tokens, WN_opc_dtype(wn), WN_opc_rtype(wn));
   Append_And_Reclaim_Token_List(tokens, &expr_tokens);

   return EMPTY_WN2F_STATUS;
} /* WN2F_cvt */


WN2F_STATUS 
WN2F_cvtl(TOKEN_BUFFER tokens, WN *wn, WN2F_CONTEXT context)
{
   TY_IDX  rtype, dtype;
   TOKEN_BUFFER expr_tokens;

   ASSERT_DBG_FATAL(WN_opc_operator(wn) == OPR_CVTL, 
		    (DIAG_W2F_UNEXPECTED_OPC, "WN2F_cvtl"));

   dtype = WN_Tree_Type(WN_kid0(wn));
   rtype = WN_Tree_Type(wn);
   
   /* Only convert if it is necessary */
   if (Conv_Op[TY_mtype(dtype)][TY_mtype(rtype)] != NULL)
   {
      expr_tokens = New_Token_Buffer();
      WN2F_translate(expr_tokens, WN_kid0(wn), context);
      WN2F_Convert(expr_tokens, TY_mtype(dtype), TY_mtype(rtype));
      Append_And_Reclaim_Token_List(tokens, &expr_tokens);
   }
   else
   {
      WN2F_translate(tokens, WN_kid0(wn), context);
   }
   return EMPTY_WN2F_STATUS;
} /* WN2F_cvtl */


WN2F_STATUS 
WN2F_realpart(TOKEN_BUFFER tokens, WN *wn, WN2F_CONTEXT context)
{
   ASSERT_DBG_FATAL(WN_opc_operator(wn) == OPR_REALPART, 
		    (DIAG_W2F_UNEXPECTED_OPC, "WN2F_realpart"));
   
   switch (WN_opc_rtype(wn))
   {
   case MTYPE_F4:
      Append_Token_String(tokens, "REAL");
      break;
   case MTYPE_F8:
      Append_Token_String(tokens, "DBLE");
      break;
   case MTYPE_FQ:
      Append_Token_String(tokens, "QEXT");
      break;
   default:
      ASSERT_DBG_FATAL(FALSE, 
		       (DIAG_W2F_UNEXPECTED_BTYPE,
			MTYPE_name(WN_opc_rtype(wn)),
			"WN2F_realpart"));
      Append_Token_String(tokens, "WN2F_realpart");
      break;
   }
   Append_Token_Special(tokens, '(');
   WN2F_translate(tokens, WN_kid0(wn), context);
   Append_Token_Special(tokens, ')');
   
   return EMPTY_WN2F_STATUS;
} /* WN2F_realpart */


WN2F_STATUS 
WN2F_imagpart(TOKEN_BUFFER tokens, WN *wn, WN2F_CONTEXT context)
{
   ASSERT_DBG_FATAL(WN_opc_operator(wn) == OPR_IMAGPART, 
		    (DIAG_W2F_UNEXPECTED_OPC, "WN2F_imagpart"));
   
   switch (WN_opc_rtype(wn))
   {
   case MTYPE_F4:
      Append_Token_String(tokens, "AIMAG");
      break;
   case MTYPE_F8:
      Append_Token_String(tokens, "DIMAG");
      break;
   case MTYPE_FQ:
      Append_Token_String(tokens, "QIMAG");
      break;
   default:
      ASSERT_DBG_FATAL(FALSE, 
		       (DIAG_W2F_UNEXPECTED_BTYPE,
			MTYPE_name(WN_opc_rtype(wn)),
			"WN2F_imagpart"));
      Append_Token_String(tokens, "WN2F_imagpart");
      break;
   }
   Append_Token_Special(tokens, '(');
   WN2F_translate(tokens, WN_kid0(wn), context);
   Append_Token_Special(tokens, ')');
   
   return EMPTY_WN2F_STATUS;
} /* WN2F_imagpart */


WN2F_STATUS 
WN2F_paren(TOKEN_BUFFER tokens, WN *wn, WN2F_CONTEXT context)
{
   WN2F_STATUS status;
   
   ASSERT_DBG_FATAL(WN_opc_operator(wn) == OPR_PAREN, 
		    (DIAG_W2F_UNEXPECTED_OPC, "WN2F_paren"));

   Append_Token_Special(tokens, '(');
   status = WN2F_translate(tokens, WN_kid0(wn), context);
   Append_Token_Special(tokens, ')');

   return status;
} /* WN2F_paren */


WN2F_STATUS 
WN2F_complex(TOKEN_BUFFER tokens, WN *wn, WN2F_CONTEXT context)
{
   ASSERT_DBG_FATAL(WN_opc_operator(wn) == OPR_COMPLEX, 
		    (DIAG_W2F_UNEXPECTED_OPC, "WN2F_complex"));

   switch (WN_opc_rtype(wn))
   {
   case MTYPE_C4:
      Append_Token_String(tokens, "CMPLX");
      break;
   case MTYPE_C8:
      Append_Token_String(tokens, "DCMPLX");
      break;
   case MTYPE_CQ:
      Append_Token_String(tokens, "QCMPLX");
      break;
   default:
      ASSERT_DBG_FATAL(FALSE, 
		       (DIAG_W2F_UNEXPECTED_BTYPE,
			MTYPE_name(WN_opc_rtype(wn)),
			"WN2F_complex"));
      Append_Token_String(tokens, "WN2F_complex");
      break;
   }
   /* No need to parenthesize subexpressions */
   set_WN2F_CONTEXT_no_parenthesis(context);

   Append_Token_Special(tokens, '('); /* getting real part */
   (void)WN2F_translate(tokens, WN_kid0(wn), context);
   Append_Token_Special(tokens, ','); /* getting imaginary part */
   (void)WN2F_translate(tokens, WN_kid1(wn), context);
   Append_Token_Special(tokens, ')');

   return EMPTY_WN2F_STATUS;
} /* WN2F_complex */


WN2F_STATUS
WN2F_ceil(TOKEN_BUFFER tokens, WN *wn, WN2F_CONTEXT context)
{
   ASSERT_DBG_FATAL(WN_opc_operator(wn) == OPR_CEIL, 
		    (DIAG_W2F_UNEXPECTED_OPC, "WN2F_ceil"));
   ASSERT_DBG_FATAL(WN_kid_count(wn) == 1, 
		    (DIAG_W2F_UNEXPECTED_NUM_KIDS, 
		     WN_kid_count(wn), 1, WN_opc_name(wn)));

   /* Special handling for opcodes that do not have an intrinsic
    * counterpart in compiler versions < v7.00. TODO: define this one.
    */
   ASSERT_DBG_WARN(!W2F_Ansi_Format, 
		   (DIAG_W2F_UNEXPECTED_OPC, "WN2F_ceil"));

   WN2F_Funcall_Op(tokens,
		   WN_opcode(wn),
		   NULL,
		   WN_kid0(wn),
		   context);

   return EMPTY_WN2F_STATUS;
} /* WN2F_ceil */


WN2F_STATUS
WN2F_floor(TOKEN_BUFFER tokens, WN *wn, WN2F_CONTEXT context)
{
   ASSERT_DBG_FATAL(WN_opc_operator(wn) == OPR_FLOOR, 
		    (DIAG_W2F_UNEXPECTED_OPC, "WN2F_floor"));
   ASSERT_DBG_FATAL(WN_kid_count(wn) == 1, 
		    (DIAG_W2F_UNEXPECTED_NUM_KIDS, 
		     WN_kid_count(wn), 1, WN_opc_name(wn)));

   /* Special handling for opcodes that do not have an intrinsic
    * counterpart in compiler versions < v7.00. TODO: define this one.
    */
   ASSERT_DBG_WARN(!W2F_Ansi_Format, 
		   (DIAG_W2F_UNEXPECTED_OPC, "WN2F_floor"));

   WN2F_Funcall_Op(tokens,
		   WN_opcode(wn),
		   NULL,
		   WN_kid0(wn),
		   context);

   return EMPTY_WN2F_STATUS;
} /* WN2F_floor */


WN2F_STATUS
WN2F_ashr(TOKEN_BUFFER tokens, WN *wn, WN2F_CONTEXT context)
{
   TY_IDX const rty = Stab_Mtype_To_Ty(WN_rtype(wn));

   ASSERT_DBG_FATAL(WN_kid_count(wn) == 2, 
		    (DIAG_W2F_UNEXPECTED_NUM_KIDS, 
		     WN_kid_count(wn), 2, WN_opc_name(wn)));

   if (W2F_Ansi_Format)
   {
      /* Special handling for opcodes that do not have an intrinsic
       * counterpart in compiler versions < v7.00.
       */
      switch (WN_opcode(wn))
      {
      case OPC_I4ASHR:
	 Append_Token_String(tokens, "I4ASHR");
	 break;
      case OPC_U8ASHR:
	 Append_Token_String(tokens, "U8ASHR");
	 break;
      case OPC_I8ASHR:
	 Append_Token_String(tokens, "I8ASHR");
	 break;
      case OPC_U4ASHR:
	 Append_Token_String(tokens, "I4ASHR");
	 break;
      default:
	 ASSERT_DBG_FATAL(FALSE, 
			  (DIAG_W2F_UNEXPECTED_OPC, "WN2F_ashr"));
	 break;
      }

      /* No need to parenthesize subexpressions */
      set_WN2F_CONTEXT_no_parenthesis(context);

      Append_Token_Special(tokens, '(');
      WN2F_Translate_Arithmetic_Operand(tokens, WN_kid0(wn), rty,
					TRUE, /* call-by-value */
					context);
      Append_Token_Special(tokens, ',');
      WN2F_Translate_Arithmetic_Operand(tokens, WN_kid1(wn), rty,
					TRUE, /* call-by-value */
					context);

      Append_Token_Special(tokens, ')');
   }
   else
   {
      /* Has an intrinsic counterpart in compiler versions >= v7.00.
       */
      ASSERT_DBG_FATAL(WN_opc_operator(wn) == OPR_ASHR, 
		       (DIAG_W2F_UNEXPECTED_OPC, "WN2F_ashr"));
      WN2F_Funcall_Op(tokens,
		      WN_opcode(wn),
		      WN_kid0(wn),
		      WN_kid1(wn),
		      context);
   }

   return EMPTY_WN2F_STATUS;
} /* WN2F_ashr */


WN2F_STATUS 
WN2F_lshr(TOKEN_BUFFER tokens, WN *wn, WN2F_CONTEXT context)
{
   TY_IDX const result_ty = Stab_Mtype_To_Ty(WN_opc_rtype(wn));
   
   ASSERT_DBG_FATAL(WN_opc_operator(wn) == OPR_LSHR, 
		    (DIAG_W2F_UNEXPECTED_OPC, "WN2F_lshr"));

   /* No need to parenthesize subexpressions */
   set_WN2F_CONTEXT_no_parenthesis(context);

   Append_Token_String(tokens, "ISHIFT");
   Append_Token_Special(tokens, '(');
   WN2F_Translate_Arithmetic_Operand(tokens, WN_kid(wn,0), result_ty,
				     !TY_Is_Character_Reference(result_ty),
				     context);
   Append_Token_Special(tokens, ',');
   Append_Token_Special(tokens, '-');
   Append_Token_Special(tokens, '(');
   WN2F_Translate_Arithmetic_Operand(tokens, WN_kid(wn,1), result_ty,
				     !TY_Is_Character_Reference(result_ty),
				     context);
   Append_Token_Special(tokens, ')');
   Append_Token_Special(tokens, ')');

   return  EMPTY_WN2F_STATUS;
} /* WN2F_lshr */


WN2F_STATUS 
WN2F_bnor(TOKEN_BUFFER tokens, WN *wn, WN2F_CONTEXT context)
{
   TY_IDX const result_ty = Stab_Mtype_To_Ty(WN_opc_rtype(wn));
   
   ASSERT_DBG_FATAL(WN_opc_operator(wn) == OPR_BNOR, 
		    (DIAG_W2F_UNEXPECTED_OPC, "WN2F_bnor"));

   /* No need to parenthesize subexpressions */
   set_WN2F_CONTEXT_no_parenthesis(context);

   Append_Token_String(tokens, "NOT");
   Append_Token_Special(tokens, '(');
   Append_Token_String(tokens, "IOR");
   Append_Token_Special(tokens, '(');
   WN2F_Translate_Arithmetic_Operand(tokens, WN_kid(wn,0), result_ty,
				     !TY_Is_Character_Reference(result_ty),
				     context);
   Append_Token_Special(tokens, ',');
   WN2F_Translate_Arithmetic_Operand(tokens, WN_kid(wn,1), result_ty,
				     !TY_Is_Character_Reference(result_ty),
				     context);
   Append_Token_Special(tokens, ')');
   Append_Token_Special(tokens, ')');

   return  EMPTY_WN2F_STATUS;
} /* WN2F_bnor */


WN2F_STATUS 
WN2F_recip(TOKEN_BUFFER tokens, WN *wn, WN2F_CONTEXT context)
{
   TY_IDX const result_ty = Stab_Mtype_To_Ty(WN_opc_rtype(wn));
   
   ASSERT_DBG_FATAL(WN_opc_operator(wn) == OPR_RECIP, 
		    (DIAG_W2F_UNEXPECTED_OPC, "WN2F_recip"));

   Append_Token_Special(tokens, '(');
   if (TY_mtype(result_ty) == MTYPE_FQ || TY_mtype(result_ty) == MTYPE_CQ)
      Append_Token_String(tokens, "1Q00");
   else if (TY_mtype(result_ty) == MTYPE_F8 || TY_mtype(result_ty) == MTYPE_C8)
      Append_Token_String(tokens, "1D00");
   else
      Append_Token_String(tokens, "1E00");

   Append_Token_Special(tokens, '/');
   WN2F_Translate_Arithmetic_Operand(tokens, WN_kid(wn,0), result_ty,
				     !TY_Is_Character_Reference(result_ty),
				     context);
   Append_Token_Special(tokens, ')');

   return EMPTY_WN2F_STATUS;
} /* WN2F_recip */

WN2F_STATUS 
WN2F_rsqrt(TOKEN_BUFFER tokens, WN *wn, WN2F_CONTEXT context)
{
   TY_IDX const result_ty = Stab_Mtype_To_Ty(WN_opc_rtype(wn));
   
   ASSERT_DBG_FATAL(WN_opc_operator(wn) == OPR_RSQRT, 
		    (DIAG_W2F_UNEXPECTED_OPC, "WN2F_rsqrt"));

   Append_Token_Special(tokens, '(');
   Append_Token_String(tokens, "1.0");
   Append_Token_Special(tokens, '/');
   Append_Token_String(tokens, "SQRT");
   Append_Token_Special(tokens, '(');
   set_WN2F_CONTEXT_no_parenthesis(context);
   WN2F_Translate_Arithmetic_Operand(tokens, WN_kid(wn,0), result_ty,
				     !TY_Is_Character_Reference(result_ty),
				     context);
   Append_Token_Special(tokens, ')');
   Append_Token_Special(tokens, ')');

   return EMPTY_WN2F_STATUS;
} /* WN2F_rsqrt */


WN2F_STATUS 
WN2F_madd(TOKEN_BUFFER tokens, WN *wn, WN2F_CONTEXT context)
{
   TY_IDX const result_ty = Stab_Mtype_To_Ty(WN_opc_rtype(wn));
   
   ASSERT_DBG_FATAL(WN_opc_operator(wn) == OPR_MADD, 
		    (DIAG_W2F_UNEXPECTED_OPC, "WN2F_madd"));

   Append_Token_Special(tokens, '(');
   WN2F_Translate_Arithmetic_Operand(tokens, WN_kid(wn,1), result_ty,
				     !TY_Is_Character_Reference(result_ty),
				     context);
   Append_Token_Special(tokens, '*');
   WN2F_Translate_Arithmetic_Operand(tokens, WN_kid(wn,2), result_ty,
				     !TY_Is_Character_Reference(result_ty),
				     context);
   Append_Token_Special(tokens, '+');
   WN2F_Translate_Arithmetic_Operand(tokens, WN_kid(wn, 0), result_ty,
				     !TY_Is_Character_Reference(result_ty),
				     context);
   Append_Token_Special(tokens, ')');

   return  EMPTY_WN2F_STATUS;
} /* WN2F_madd */


WN2F_STATUS 
WN2F_msub(TOKEN_BUFFER tokens, WN *wn, WN2F_CONTEXT context)
{
   TY_IDX const result_ty = Stab_Mtype_To_Ty(WN_opc_rtype(wn));
   
   ASSERT_DBG_FATAL(WN_opc_operator(wn) == OPR_MSUB, 
		    (DIAG_W2F_UNEXPECTED_OPC, "WN2F_msub"));

   Append_Token_Special(tokens, '(');
   WN2F_Translate_Arithmetic_Operand(tokens, WN_kid(wn,1), result_ty,
				     !TY_Is_Character_Reference(result_ty),
				     context);
   Append_Token_Special(tokens, '*');
   WN2F_Translate_Arithmetic_Operand(tokens, WN_kid(wn,2), result_ty,
				     !TY_Is_Character_Reference(result_ty),
				     context);
   Append_Token_Special(tokens, '-');
   WN2F_Translate_Arithmetic_Operand(tokens, WN_kid(wn, 0), result_ty,
				     !TY_Is_Character_Reference(result_ty),
				     context);
   Append_Token_Special(tokens, ')');

   return  EMPTY_WN2F_STATUS;
} /* WN2F_msub */


WN2F_STATUS 
WN2F_nmadd(TOKEN_BUFFER tokens, WN *wn, WN2F_CONTEXT context)
{
   TY_IDX const result_ty = Stab_Mtype_To_Ty(WN_opc_rtype(wn));

   ASSERT_DBG_FATAL(WN_opc_operator(wn) == OPR_NMADD, 
		    (DIAG_W2F_UNEXPECTED_OPC, "WN2F_nmadd"));

   Append_Token_Special(tokens, '-');
   Append_Token_Special(tokens, '(');
   WN2F_Translate_Arithmetic_Operand(tokens, WN_kid(wn,1), result_ty,
				     !TY_Is_Character_Reference(result_ty),
				     context);
   Append_Token_Special(tokens, '*');
   WN2F_Translate_Arithmetic_Operand(tokens, WN_kid(wn,2), result_ty,
				     !TY_Is_Character_Reference(result_ty),
				     context);
   Append_Token_Special(tokens, '+');
   WN2F_Translate_Arithmetic_Operand(tokens, WN_kid(wn, 0), result_ty,
				     !TY_Is_Character_Reference(result_ty),
				     context);
   Append_Token_Special(tokens, ')');

   return  EMPTY_WN2F_STATUS;
} /* WN2F_nmadd */


WN2F_STATUS 
WN2F_nmsub(TOKEN_BUFFER tokens, WN *wn, WN2F_CONTEXT context)
{
   TY_IDX const result_ty = Stab_Mtype_To_Ty(WN_opc_rtype(wn));
   
   ASSERT_DBG_FATAL(WN_opc_operator(wn) == OPR_NMSUB, 
		    (DIAG_W2F_UNEXPECTED_OPC, "WN2F_nmsub"));

   Append_Token_Special(tokens, '-');
   Append_Token_Special(tokens, '(');
   WN2F_Translate_Arithmetic_Operand(tokens, WN_kid(wn,1), result_ty,
				     !TY_Is_Character_Reference(result_ty),
				     context);
   Append_Token_Special(tokens, '*');
   WN2F_Translate_Arithmetic_Operand(tokens, WN_kid(wn,2), result_ty,
				     !TY_Is_Character_Reference(result_ty),
				     context);
   Append_Token_Special(tokens, '-');
   WN2F_Translate_Arithmetic_Operand(tokens, WN_kid(wn, 0), result_ty,
				     !TY_Is_Character_Reference(result_ty),
				     context);
   Append_Token_Special(tokens, ')');

   return  EMPTY_WN2F_STATUS;
} /* WN2F_nmsub */


WN2F_STATUS 
WN2F_const(TOKEN_BUFFER tokens, WN *wn, WN2F_CONTEXT context)
{
   ASSERT_DBG_FATAL(WN_opc_operator(wn) == OPR_CONST, 
		    (DIAG_W2F_UNEXPECTED_OPC, "WN2F_const"));

   TCON2F_translate(tokens,
		    STC_val(WN_st(wn)),
		    (TY_is_logical(ST_type(WN_st(wn))) ||
		     WN2F_CONTEXT_is_logical_arg(context)));

   return EMPTY_WN2F_STATUS;
} /* WN2F_const */


WN2F_STATUS 
WN2F_intconst(TOKEN_BUFFER tokens, WN *wn, WN2F_CONTEXT context)
{
   ASSERT_DBG_FATAL(WN_opc_operator(wn) == OPR_INTCONST, 
		    (DIAG_W2F_UNEXPECTED_OPC, "WN2F_intconst"));

   TCON2F_translate(tokens,
		    Host_To_Targ(WN_opc_rtype(wn), WN_const_val(wn)),
		    WN2F_CONTEXT_is_logical_arg(context));

   return EMPTY_WN2F_STATUS;
} /* WN2F_intconst */


WN2F_STATUS 
WN2F_eq(TOKEN_BUFFER tokens, WN *wn, WN2F_CONTEXT context)
{
   /* Try to reduce "bool .EQ. 0" to simply ".NOT. (bool)", whenever possible.
    */

   ASSERT_DBG_FATAL(WN_opc_operator(wn) == OPR_EQ, 
		    (DIAG_W2F_UNEXPECTED_OPC, "WN2F_eq"));

   if (WN_opc_operator(WN_kid0(wn)) == OPR_INTCONST &&
       WN_const_val(WN_kid0(wn)) == 0LL             &&
       (OPCODE_is_boolean(WN_opcode(WN_kid1(wn))) ||
	TY_is_logical(WN_Tree_Type(WN_kid1(wn)))))
   {
      Append_Token_String(tokens, ".NOT.");
      Append_Token_Special(tokens, '(');
      set_WN2F_CONTEXT_no_parenthesis(context);
      WN2F_translate(tokens, WN_kid1(wn), context);
      Append_Token_Special(tokens, ')');
   }
   else if (WN_opc_operator(WN_kid1(wn)) == OPR_INTCONST &&
	    WN_const_val(WN_kid1(wn)) == 0LL             &&
	    (OPCODE_is_boolean(WN_opcode(WN_kid0(wn))) ||
	     TY_is_logical(WN_Tree_Type(WN_kid0(wn)))))
   {
      Append_Token_String(tokens, ".NOT.");
      Append_Token_Special(tokens, '(');
      set_WN2F_CONTEXT_no_parenthesis(context);
      WN2F_translate(tokens, WN_kid0(wn), context);
      Append_Token_Special(tokens, ')');
   }
   else
   {
      WN2F_binaryop(tokens, wn, context);
   }
   
   return EMPTY_WN2F_STATUS;
} /* WN2F_eq */


WN2F_STATUS 
WN2F_ne(TOKEN_BUFFER tokens, WN *wn, WN2F_CONTEXT context)
{
   /* Try to reduce "bool .NE. 0" to simply "bool", whenever possible.
    */

   ASSERT_DBG_FATAL(WN_opc_operator(wn) == OPR_NE, 
		    (DIAG_W2F_UNEXPECTED_OPC, "WN2F_ne"));

   if (WN_opc_operator(WN_kid0(wn)) == OPR_INTCONST &&
       WN_const_val(WN_kid0(wn)) == 0LL             &&
       (OPCODE_is_boolean(WN_opcode(WN_kid1(wn))) ||
	TY_is_logical(WN_Tree_Type(WN_kid1(wn)))))
   {
      WN2F_translate(tokens, WN_kid1(wn), context);
   }
   else if (WN_opc_operator(WN_kid1(wn)) == OPR_INTCONST &&
	    WN_const_val(WN_kid1(wn)) == 0LL             &&
	    (OPCODE_is_boolean(WN_opcode(WN_kid0(wn))) ||
	     TY_is_logical(WN_Tree_Type(WN_kid0(wn)))))
   {
      WN2F_translate(tokens, WN_kid0(wn), context);
   }
   else
   {
      WN2F_binaryop(tokens, wn, context);
   }
   
   return EMPTY_WN2F_STATUS;
} /* WN2F_ne */



WN2F_STATUS 
WN2F_parm(TOKEN_BUFFER tokens, WN *wn, WN2F_CONTEXT context)
{
   /* TODO: handle opcode parms properly, i.e. take some advantage
    * of the information provided in this packaging of argument 
    * expressions.  For now, just skip these nodes.
    */
   ASSERT_DBG_FATAL(WN_opc_operator(wn) == OPR_PARM, 
		    (DIAG_W2F_UNEXPECTED_OPC, "WN2F_parm"));

   return WN2F_translate(tokens, WN_kid0(wn), context);
} /* WN2F_parm */


/*---------------- Memory allocation ops -------------------*/

WN2F_STATUS 
WN2F_alloca(TOKEN_BUFFER tokens, WN *wn, WN2F_CONTEXT context)
{
   ASSERT_DBG_FATAL(WN_operator(wn) == OPR_ALLOCA, 
		    (DIAG_W2F_UNEXPECTED_OPC, "WN2F_alloca"));


   Append_Token_String(tokens,"OPR_ALLOCA");
   Append_Token_Special(tokens,'(');
   WN2F_translate(tokens,WN_kid0(wn),context);
   Append_Token_Special(tokens,')');

   return EMPTY_WN2F_STATUS;
} /* WN2F_alloca */


WN2F_STATUS 
WN2F_dealloca(TOKEN_BUFFER tokens, WN *wn, WN2F_CONTEXT context)
{
  INT16 n,i;

   ASSERT_DBG_FATAL(WN_operator(wn) == OPR_DEALLOCA, 
		    (DIAG_W2F_UNEXPECTED_OPC, "WN2F_dealloca"));

   n = WN_kid_count(wn);

   WN2F_Stmt_Newline(tokens, NULL/*label*/, WN_linenum(wn), context);
   Append_Token_String(tokens,"CALL OPR_DEALLOCA");
   Append_Token_Special(tokens,'(');

   i = 0 ;
   while (i < n)
   {
     WN2F_translate(tokens,WN_kid(wn,i),context);
     if (++i < n)
       Append_Token_Special(tokens,',');
   }

   Append_Token_Special(tokens,')');
   return EMPTY_WN2F_STATUS;
} /* WN2F_dealloca */

