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
 *  Module: tn_prop.h
 *  $Revision: 1.1 $
 *  $Date: 2005/07/27 02:17:56 $
 *  $Author: kevinlo $
 *  $Source: /depot/CVSROOT/javi/src/sw/cmplr/common/util/x_prop.h,v $
 *
 *  Revision history:
 *   13-Dec-91 - Original Version
 *
 *  Synopsis:
 *
 *      This module is a template for lightweight sets that support
 *      the operations: add-member, delete-member, is-member, and
 *      destructive union.  (More could be added.)  A new set type is
 *      created from a base type by defining some perameter macros and
 *      including the template files.  The sets are static in the
 *      sense that their universe is fixed at the time of their
 *      creation.  (This is part of the source of their lightweight.)
 *
 *  Instructions for use:
 *
 *      This file is only a template.  See MTP_ROOT/bin/gen_x_prop for
 *      instructions on how to create instantiations based on specific
 *      types.
 *  
 *  Interface:  Given a base_type_name XT and a prefix X:
 *
 *      Prefixes: X_PROP -   for X properties
 *
 *      Exported types:
 *
 *          typedef UINT32/UINT64 X_PROP
 *
 *              Only accessable through the functions defined below.
 *
 *      Exported functions:
 *
 *          X_PROP *X_PROP_Create(
 *              INT32     universe_size,
 *		MEM_POOL *pool
 *          )
 *
 *              Creates a new PREFIX_PROP which is initially FALSE of
 *              all (currently existing) elements.
 *
 *          void X_PROP_Set(
 *              X_PROP *prop,
 *              XT      x
 *          )
 *
 *              Makes 'prop' true of 'x'.
 *
 *          void X_PROP_Reset(
 *              X_PROP *prop,
 *              XT      x
 *          )
 *
 *            Makes 'prop' false of 'x'.
 *
 *        BOOL X_PROP_Get(
 *            X_PROP *prop,
 *            XT      x
 *        )
 *
 *            Returns TRUE or FALSE as the 'prop' applies to 'x'.
 *
 *        void X_PROP_UnionD(
 *            x_PROP *prop0,
 *            x_PROP *prop1
 *        )
 *
 *            Destructively adds the elements of 'prop1' into 'prop0'.
 *
 *        BOOL X_PROP_Intersection_Is_NonEmpty(
 *            x_PROP *prop0,
 *            x_PROP *prop1
 *        )
 *
 *            Returns TRUE iff 'prop0', 'prop1' have an element in
 *            common. 
 *
 * ====================================================================
 * ====================================================================
 */




#ifdef __cplusplus
extern "C" {
#endif



static const char _X_RCS_ID_[] = "$Source: /depot/CVSROOT/javi/src/sw/cmplr/common/util/x_prop.h,v $ $Revision: 1.1 $";

#if defined(_MIPS_ISA) && _MIPS_ISA >= 3
typedef UINT64 _X_PROP_TYPE_;
#define _X_PROP_TYPE_SIZE_LOG2_ 6
#else
typedef UINT32 _X_PROP_TYPE_;
#define _X_PROP_TYPE_SIZE_LOG2_ 5
#endif
#define _X_PROP_TYPE_SIZE_ (sizeof(_X_PROP_TYPE_) * 8)

typedef _X_BASE_TYPE_ _X_PROP_LOCAL_BASE_TYPE_;

extern _X_PROP_TYPE_ *
_X_PROP_CREATE_(
  INT32     universe_size,
  MEM_POOL *pool
);

extern void
_X_PROP_SET_(
  _X_PROP_TYPE_            *prop,
  _X_PROP_LOCAL_BASE_TYPE_  x
);

extern void
_X_PROP_RESET_(
  _X_PROP_TYPE_            *prop,
  _X_PROP_LOCAL_BASE_TYPE_  x
);

extern BOOL
_X_PROP_GET_(
  _X_PROP_TYPE_            *prop,
  _X_PROP_LOCAL_BASE_TYPE_  x
);

extern void
_X_PROP_UNIOND_(
  _X_PROP_TYPE_ *prop0,
  _X_PROP_TYPE_ *prop1
);

extern BOOL
_X_PROP_INTERSECTION_IS_NONEMPTY_(
  _X_PROP_TYPE_ *prop0,
  _X_PROP_TYPE_ *prop1
);

#ifdef __cplusplus
}
#endif
