/* -*- c++ -*-
 *
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
 */

class IPA_BUILTIN {
private:
  PU_Info *_pu_info;	// the function created by IPA
  INTRINSIC _intrinsic;	// id of the intrinsic that the builtin replaces

public:
  IPA_BUILTIN (PU_Info *pu_info, INTRINSIC intrinsic) {
    _pu_info = pu_info;
    _intrinsic = intrinsic;
  }
  PU_Info *Get_PU_Info() {return _pu_info;}
  INTRINSIC Get_Intrinsic() {return _intrinsic;}
};

extern std::vector<IPA_BUILTIN*> IPA_builtins_list;

extern void IPA_Create_Builtins();
extern void IPA_Rename_Builtins(IPA_NODE *);
