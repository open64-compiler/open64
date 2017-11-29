//-*-c++-*-
// ====================================================================
// ====================================================================
//
// Module: opt_combine.h
// $Revision: 1.1.1.1 $
// $Date: 2005/10/21 19:00:00 $
// $Author: marcel $
// $Source: /proj/osprey/CVS/open64/osprey1.0/be/opt/opt_combine.h,v $
//
// Revision history:
//  15-SEP-94 shin - Original Version
//
// ====================================================================
//
// Copyright (C) 2000, 2001 Silicon Graphics, Inc.  All Rights Reserved.
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of version 2 of the GNU General Public License as
// published by the Free Software Foundation.
//
// This program is distributed in the hope that it would be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//
// Further, this software is distributed without any warranty that it
// is free of the rightful claim of any third person regarding
// infringement  or the like.  Any license provided herein, whether
// implied or otherwise, applies only to this software file.  Patent
// licenses, if any, provided herein do not apply to combinations of
// this program with other software, or any other product whatsoever.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write the Free Software Foundation,
// Inc., 59 Temple Place - Suite 330, Boston MA 02111-1307, USA.
//
// Contact information:  Silicon Graphics, Inc., 1600 Amphitheatre Pky,
// Mountain View, CA 94043, or:
//
// http://www.sgi.com
//
// For further information regarding this notice, see:
//
// http://oss.sgi.com/projects/GenInfo/NoticeExplan
//
// ====================================================================
//
// Description:  Interface to routines for special handling of
//               combined operations
//
// ====================================================================
// ====================================================================


#ifndef opt_combine_INCLUDED
#define opt_combine_INCLUDED	"opt_combine.h"

// main driver for the code to try to combine operations, which may
// later be split up.  This should just deal with cases in which we
// simplify/change expressions, and later at emit time, we may undo
// the change.  Any real simplifications should be done in the 
// simplifier.
extern BOOL Combine_Operations( WN *old_wn, WN **new_wn );

// main driver for the code to try to un-combine operations which may
// have been created by Combine_Operations.
// Think of this as a targeted simplifier.
extern BOOL Uncombine_Operations( WN *old_wn, WN **new_wn );

#endif  // opt_combine_INCLUDED
