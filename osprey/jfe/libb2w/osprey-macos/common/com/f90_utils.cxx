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


#ifdef USE_PCH
#include "common_com_pch.h"
#endif /* USE_PCH */
#pragma hdrstop
#include "defs.h"
#include "wn.h"
#include "wn_util.h"

#include "f90_utils.h"


/* Returns TRUE if the intrinsic is one of the F90 transformational intrinsics
 */

BOOL F90_Is_Transformational(INT32 intrinsic)
{
   switch (intrinsic) {
    case INTRN_MATMUL:
    case INTRN_SPREAD:
    case INTRN_RESHAPE:
    case INTRN_TRANSPOSE:
    case INTRN_ALL:
    case INTRN_ANY:
    case INTRN_COUNT:
    case INTRN_PRODUCT:
    case INTRN_SUM:
    case INTRN_EOSHIFT:
    case INTRN_MAXVAL:
    case INTRN_MINVAL:
    case INTRN_MAXLOC:
    case INTRN_MINLOC:
    case INTRN_CSHIFT:
    case INTRN_DOT_PRODUCT:
    case INTRN_PACK:
    case INTRN_UNPACK:
      return(TRUE);
    default:
      return (FALSE);
   }
}

/* Returns TRUE if the intrinsic is one of the F90 Character intrinsics
 */
BOOL F90_Is_Char_Intrinsic(INT32 intr)
{
   switch(intr) {
    case INTRN_CASSIGNSTMT:
    case INTRN_CONCATEXPR:
    case INTRN_ADJUSTL:
    case INTRN_ADJUSTR:
    case INTRN_CEQEXPR:
    case INTRN_CNEEXPR:
    case INTRN_CGEEXPR:
    case INTRN_CGTEXPR:
    case INTRN_CLEEXPR:
    case INTRN_CLTEXPR:
    case INTRN_I4CLEN:
    case INTRN_I4CINDEX:
    case INTRN_CLGE:
    case INTRN_CLGT:
    case INTRN_CLLE:
    case INTRN_CLLT:
    case INTRN_LENTRIM:
    case INTRN_F90INDEX:
    case INTRN_SCAN:
    case INTRN_VERIFY:
      return (TRUE);
    default:
      return (FALSE);
   }
}



/*================================================================ 
 * Check to see if a node represents a DIM= 
 * argument. Return 0 if no DIM= is present, otherwise
 * return the value of DIM
 *================================================================/
 */
INT F90_Get_Dim(WN *dim_wn)
{
   OPERATOR opr;
   opr = WN_operator(dim_wn);
   if (WN_opcode(dim_wn) == OPC_VPARM) {
      return (0);
   } else if (opr == OPR_PARM) {
      return (F90_Get_Dim(WN_kid0(dim_wn)));
   } else if (opr == OPR_INTCONST) {
      return (WN_const_val(dim_wn));
   } else {
      return (0);
   }
}





/*================================================================
 *
 * Utility routine for sizing a tree
 * returns TRUE if the tree is not scalar, and ndim and sizes are
 * set to the number of dimensions and the sizes of each. sizes is a copy
 * of WHIRL in the tree, so it should be deleted to save space afterward.
 *================================================================
 */

BOOL F90_Size_Walk(WN *expr, INT *ndim, WN **sizes) 
{
   WN *temp;
   WN *temp_sizes[MAX_NDIM];
   INT child_ndim;
   INT i,j,dim,numkids;
   BOOL sized;
   
   *ndim = 0;
   sized = FALSE;

   switch (WN_operator(expr)) {
    case OPR_COMMA:
       /* Size the things in the block */
       temp = WN_first(WN_kid0(expr));
       while (temp) {
	  sized = F90_Size_Walk(temp,ndim,sizes);
	  if (sized) return (TRUE);
	  temp = WN_next(temp);
       }
       return (FALSE);
       
    case OPR_RCOMMA:
       /* Size the things in the block */
       temp = WN_first(WN_kid1(expr));
       while (temp) {
	  sized = F90_Size_Walk(temp,ndim,sizes);
	  if (sized) return (TRUE);
	  temp = WN_next(temp);
       }
       return (FALSE);

    case OPR_ARRAYEXP:
      numkids = WN_kid_count(expr);
      if (numkids != 1) {
	 *ndim = numkids-1;
	 for (i = 1; i < numkids; i++) {
	    sizes[i-1] = WN_COPY_Tree(WN_kid(expr,i));
	 }
	 return (TRUE);
      }
      /* Just size the child */
      break;

    case OPR_ARRSECTION:
      numkids = (WN_kid_count(expr) - 1)/2;
      j = 0;
      for (i = 1; i <= numkids; i++) {
	 if (F90_Size_Walk(WN_kid(expr,i+numkids),&child_ndim,temp_sizes)) {
	    sizes[j] = temp_sizes[0];
	    j += 1;
	 }
      }
      *ndim = j;
      return (j != 0);

    case OPR_TRIPLET:
      *ndim = 1;
      sizes[0] = WN_COPY_Tree(WN_kid2(expr));
      return (TRUE);
      
    case OPR_INTRINSIC_OP:
      switch (WN_intrinsic(expr)) {
       case INTRN_SPREAD:
	 sized = F90_Size_Walk(WN_kid0(expr),&child_ndim,temp_sizes);
	 if (!sized) {
	    /* Scalar expression */
	    child_ndim = 0;
	 }
	 dim = child_ndim - F90_Get_Dim(WN_kid1(expr)) + 1;
	 for (i=0,j=0; i <= child_ndim; i++) {
	    if (i == dim) {
	       sizes[i] = WN_COPY_Tree(WN_kid0(WN_kid2(expr)));
	    } else {
	       sizes[i] = temp_sizes[j++];
	    }
	 }
	 *ndim = child_ndim + 1;
	 return (TRUE);

       case INTRN_TRANSPOSE:
	 (void) F90_Size_Walk(WN_kid0(expr),ndim,sizes);
	 temp = sizes[0];
	 sizes[0] = sizes[1];
	 sizes[1] = temp;
	 return (TRUE);


       case INTRN_MATMUL:
	 {
	   INT dim1,dim2;
	   WN *size1[2],*size2[2];
	   F90_Size_Walk(WN_kid0(expr),&dim1,size1);
	   F90_Size_Walk(WN_kid1(expr),&dim2,size2);
	   
	   if (dim1 == 1) {
	     // Vector-matrix
	     *ndim = 1;
	     sizes[0] = size2[0];
	     WN_DELETE_Tree(size1[0]);
	     WN_DELETE_Tree(size2[1]);
	   } else if (dim2 == 1) {
	     // Matrix vector
	     *ndim = 1;
	     sizes[0] = size1[1];
	     WN_DELETE_Tree(size1[0]);
	     WN_DELETE_Tree(size2[0]);
	   } else {
	     Is_True(dim1==2 && dim2 == 2,("Bad MATMUL"));
	     // Matrix-Matrix
	     *ndim = 2;
	     sizes[1] = size1[1];
	     sizes[0] = size2[0];
	     WN_DELETE_Tree(size1[0]);
	     WN_DELETE_Tree(size2[1]);
	   }
	   return (TRUE);
	 }

       case INTRN_ALL:
       case INTRN_ANY:
       case INTRN_COUNT:
       case INTRN_PRODUCT:
       case INTRN_SUM:
       case INTRN_MAXVAL:
       case INTRN_MINVAL:
	 temp = WN_kid1(expr);
	 dim = F90_Get_Dim(WN_kid1(expr));
	 if (dim==0) {
	    /* Scalar reduction */
	    *ndim = 0;
	    return (FALSE);
	 } else {
	    sized = F90_Size_Walk(WN_kid0(expr),&child_ndim,temp_sizes);
	    if (dim == 1 && child_ndim == 1) {
	       /* This is really a scalar reduction */
	       *ndim = 0;
	    } else {
	       dim = child_ndim - dim;
	       for (i=0,j=0; i < child_ndim; i++) {
		  if (i != dim) {
		     sizes[j++] = temp_sizes[i];
		  } else {
		     WN_DELETE_Tree(temp_sizes[i]);
		  }
	       }
	       *ndim = child_ndim - 1;
	    }
	    return (*ndim != 0);
	 }
	 
	 
       case INTRN_MAXLOC:
       case INTRN_MINLOC:
	 temp = WN_kid1(expr);
	 dim = F90_Get_Dim(WN_kid1(expr));
	 if (dim==0) {
	    /* Scalar reduction */
	    *ndim = 1;
	    (void) F90_Size_Walk(WN_kid0(expr),&child_ndim,temp_sizes);
	    sizes[0] = WN_Intconst(MTYPE_I4,child_ndim);
	    return (TRUE);
	 } else {
	    sized = F90_Size_Walk(WN_kid0(expr),&child_ndim,temp_sizes);
	    dim = child_ndim - dim;
	    for (i=0,j=0; i < child_ndim; i++) {
	       if (i != dim) {
		  sizes[j++] = temp_sizes[i];
	       } else {
		  WN_DELETE_Tree(temp_sizes[i]);
	       }
	    }
	    *ndim = child_ndim - 1;
	    return (*ndim != 0);
	 }
	 
       case INTRN_EOSHIFT:
       case INTRN_CSHIFT:
	 return (F90_Size_Walk(WN_kid0(expr),ndim,sizes));

       case INTRN_PACK:
	 /* We should only get here if VECTOR exists */
	 return (F90_Size_Walk(WN_kid2(expr),ndim,sizes));

       case INTRN_UNPACK:
	 return (F90_Size_Walk(WN_kid1(expr),ndim,sizes));

       default:
	 break;
      }
    default:
      break;
   }

   /* Size all the children, looking for a sizable object */
   sized = FALSE;
   numkids = WN_kid_count(expr);
   for (i=0; i < numkids; i++) {
      sized = F90_Size_Walk(WN_kid(expr,i),ndim,sizes);
      if (sized) break;
   }
   return (sized);
}   

/*================================================================
 *
 * Utility routine for getting the rank of a tree
 * returns 0 for scalar, otherwise the rank of the TREE.
 * Works by calling F90_Size_Walk and cleaning up afterward.
 *================================================================
 */

INT F90_Rank_Walk(WN * tree)
{
  INT rank;
  INT i;
  WN *temp_sizes[MAX_NDIM];
  (void) F90_Size_Walk(tree,&rank,temp_sizes);
  
  for (i=0; i < rank; i++) {
    WN_DELETE_Tree(temp_sizes[i]);
  }

  return(rank);
}

  

/* 
 * F90_Wrap_ARREXP - Wrap an ARREXP node about an expression
 *
 * WN * F90_Lower_Wrap_ARREXP(WN * expr)
 * expr(input) - The expression to be wrapped
 *
 * returns an ARREXP node with all the sizes on it, or else
 * returns the expression if it's scalar.
 */
WN * F90_Wrap_ARREXP(WN * expr)
{
   WN * r;
   INT i;
   INT ndim;
   WN *sizes[MAX_NDIM];
   BOOL is_array_valued;
   TYPE_ID ty;

   if (WN_operator(expr) == OPR_TRIPLET) {
      /* Special case: TRIPLETS don't ever get ARRAYEXPs on top of them */
      return (expr);
   }
   is_array_valued = F90_Size_Walk(expr, &ndim, sizes);
   if (is_array_valued) {
     ty = WN_rtype(expr);
     switch (ty) {
      case MTYPE_I1:
      case MTYPE_I2:
      case MTYPE_B:
	ty = MTYPE_I4;
	break;
      case MTYPE_U1:
      case MTYPE_U2:
	ty = MTYPE_U4;
	break;
      default:
	break;
     }
     r = WN_Create(OPCODE_make_op(OPR_ARRAYEXP,ty,MTYPE_V),
		   ndim+1);
     WN_kid0(r) = expr;
     for (i=0; i<ndim; i++) {
       WN_kid(r,i+1) = sizes[i];
     }
   } else {
      r = expr;
   }
   return (r);
}


/*================================================================
 *
 * F90_wrap_cvtl 
 *
 * Wrap a CVTL around an expressions for small types
 *
 *==================================================================
 */

WN * 
F90_wrap_cvtl(WN * wn, TYPE_ID ty)
{
  WN *r;
  
  switch (ty) {
   case MTYPE_I1:
     r = WN_CreateCvtl(OPC_I4CVTL,8,wn);
     break;
   case MTYPE_I2:
     r = WN_CreateCvtl(OPC_I4CVTL,16,wn);
     break;
   case MTYPE_U1:
     r = WN_CreateCvtl(OPC_U4CVTL,8,wn);
     break;
   case MTYPE_U2:
     r = WN_CreateCvtl(OPC_U4CVTL,16,wn);
     break;
   default:
     r = wn;
  }
  
  return (r);
}
