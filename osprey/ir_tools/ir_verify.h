/*

  Copyright (C) 2018 Xcalibyte (Shenzhen) Limited.

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

 */

/* ====================================================================
 * ====================================================================
 *
 * Module: ir_verify.h
 * $Revision: 1.1.1.1 $
 * $Date: 2018/11/15 19:00:00 $
 * $Author: QING $
 *
 * Revision history:
 *  15-Nov-18 - Original Version
 *
 * Description:
 *  Verify whirl tree
 *
 * ====================================================================
 * ====================================================================
 */
#ifndef ir_verify_INCLUDED
#define ir_verify_INCLUDED

#include "wn.h"
extern void WN_verify(WN *wn, WN *parent);

extern void WN_verify_varName_line(char * input_file);
#endif
