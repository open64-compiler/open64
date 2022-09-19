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


#ifndef quadop_INCLUDED
#define quadop_INCLUDED
#ifdef __cplusplus
extern "C" {
#endif
/* ====================================================================
 * ====================================================================
 *
 * Module: quadop.h
 * $Revision: 1.1.1.1 $
 * $Date: 2005/10/21 19:00:00 $
 * $Author: marcel $
 * $Source: /proj/osprey/CVS/open64/osprey1.0/common/com/quadop.h,v $
 *
 * Revision history:
 *  15-Nov-92 - Original Version
 *
 * Description:
 *
 * This file defines a 16 byte type to represent 16 byte floats.  If
 * the host C compiler supports such type, that type can be used.  If
 * not, it needs to be implemented if needed.
 *
 * There are three possible situations:
 *
 * 1) If target compiler does not want to implement separate 16 byte
 *    float (either because the target machine does not have such
 *    operations or for some other reason), it does not matter what
 *    the operations defined below do.  They will never be used.
 *
 *    md_ctarg.h and config.h will respectively define 
 *    CTARG_NEEDS_QUAD_OPS and TARG_NEEDS_QUAD_OPS to FALSE. Somewhere
 *    in tran.c we will check that they are defined to same values.
 *
 *    In ctarg.h, size and alignment of long_double will be set to be 
 *    same as those of double.  In tran2.c type long_double will be
 *    mapped to BETYPE_R8 because its size is 8. In float_pt.c,
 *    separate quad routines will not be compiled in, and will never
 *    use QUADOP macros.
 *
 *    In f77fe/aux_stab.c,Get_Btype will not accept real*16 or
 *    complex*32.  In scan1.c 'Q' constnats will be treated same as
 *    'D' constants.
 *
 *    In targ_const.c QUADOP defines will not be used. 
 *
 * 2) If the target compiler wants to implement 16 byte float, but
 *    host compiler does not support them.
 *
 *    The macros defined in this file will be used to do quad word
 *    operations.  These macros must define type QUAD_TYPE to a 16
 *    byte type.  Operations must be done by routines that can
 *    implement 16 byte floats. In writing such routines is not
 *    practical, (particularly in cross compilers), they can be
 *    written to actually do double precision operations, as long as 
 *    the QUAD_TYPE ise defined to be a 16 byte struct.
 *
 *    md_ctarg.h and config.h will respectively define 
 *    CTARG_NEEDS_QUAD_OPS and TARG_NEEDS_QUAD_OPS to TRUE.  Somewhere
 *    in tran.c we will check that they are defined to same values.
 *
 *    Size and alignment of long-double type of C will be defined in 
 *    md_ctarg.h.  In tran2.c type long_double will be mapped to
 *    BETYPE_R16 because its size is 8.  In float_pt.c, separate quad
 *    routines will be compiled in, and will use QUADOP macros.
 *
 *    In f77fe/aux_stab.c, Get_Btype will accept real*16 or
 *    complex*32.  In scan1.c 'Q' constnats will become TK_SQUAD_CONST.
 *
 *    In targ_const.c QUADOP defines will be used. 
 *
 * 3) If the target compiler wants to implement 16 byte float, and the
 *    host compiler supports them.
 *
 *    Everything similar to 2) above, except that QUAD_TYPE can be
 *    defined to be long_double of host and macros can be defined in
 *    terms of  native operations of host.
 *
 * ====================================================================
 * ====================================================================
 */


#ifdef _KEEP_RCS_ID
static char *quadop_rcs_id = "$Source$ $Revision$";
#endif /* _KEEP_RCS_ID */

extern QUAD_TYPE QUADOP_SQRT(QUAD_TYPE);
extern INT QUADOP_EQ(QUAD_TYPE, QUAD_TYPE);
extern INT QUADOP_NE(QUAD_TYPE, QUAD_TYPE);
extern INT QUADOP_GE(QUAD_TYPE, QUAD_TYPE);
extern INT QUADOP_GT(QUAD_TYPE, QUAD_TYPE);
extern INT QUADOP_LE(QUAD_TYPE, QUAD_TYPE);
extern INT QUADOP_LT(QUAD_TYPE, QUAD_TYPE);
extern QUAD_TYPE QUADOP_ADD(QUAD_TYPE,QUAD_TYPE) ;
extern QUAD_TYPE QUADOP_SUB(QUAD_TYPE,QUAD_TYPE) ;
extern QUAD_TYPE QUADOP_MPY(QUAD_TYPE,QUAD_TYPE) ;
extern QUAD_TYPE QUADOP_DIV(QUAD_TYPE,QUAD_TYPE) ;
extern QUAD_TYPE QUADOP_NEGATE(QUAD_TYPE);
extern double QUADOP_QTOD (QUAD_TYPE);
extern float QUADOP_QTOS (QUAD_TYPE);
extern INT QUADOP_QTOI (QUAD_TYPE);
extern UINT QUADOP_QTOU (QUAD_TYPE);
extern QUAD_TYPE QUADOP_UTOQ (UINT);
extern QUAD_TYPE QUADOP_STOQ (float);
extern QUAD_TYPE QUADOP_DTOQ (double);
extern QUAD_TYPE QUADOP_ITOQ (INT);

extern QUAD_TYPE My_Atoq(char *);

#ifdef __cplusplus
}
#endif
#endif /* quadop_INCLUDED */
