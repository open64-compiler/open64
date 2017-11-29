/* 
 * Copyright 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of version 2 of the GNU General Public License as
 * published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it would be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  
 *
 * Further, this software is distributed without any warranty that it is
 * free of the rightful claim of any third person regarding infringement 
 * or the like.  Any license provided herein, whether implied or 
 * otherwise, applies only to this software file.  Patent licenses, if 
 * any, provided herein do not apply to combinations of this program with 
 * other software, or any other product whatsoever.  
 *
 * You should have received a copy of the GNU General Public License along
 * with this program; if not, write the Free Software Foundation, Inc., 59
 * Temple Place - Suite 330, Boston MA 02111-1307, USA.
 *
 * File modified June 20, 2003 by PathScale, Inc. to update Open64 C/C++ 
 * front-ends to GNU 3.2.2 release.
 */

/* WFE == WHIRL Front End */
/* translate gnu stmt trees to whirl */

#ifndef wn_pragma_INCLUDED
#define wn_pragma_INCLUDED

#ifdef KEY
#ifdef __cplusplus
extern "C" {
#endif 
void WFE_Expand_Pragma (tree);
void WFE_Expand_Freq_Hint (tree exp);
#ifdef __cplusplus
}
#endif
#endif // KEY
#endif
