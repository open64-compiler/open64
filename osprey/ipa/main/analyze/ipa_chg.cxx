/*
 * Copyright (C) 2009-2010 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

/*
 *
 * Copyright (C) 2006, 2007, Tsinghua University.  All Rights Reserved.
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
 * For further information regarding this notice, see:
 * http://hpc.cs.tsinghua.edu.cn
 *
 */


#include "ipa_chg.h"

IPA_CLASS_HIERARCHY *IPA_Class_Hierarchy;

IPA_CLASS_HIERARCHY::IPA_CLASS_HIERARCHY() {
}

IPA_CLASS_HIERARCHY::~IPA_CLASS_HIERARCHY() {
    
}

INT
IPA_CLASS_HIERARCHY::Get_Num_Base_Classes(TY_INDEX tyi) {
    CLASS_RELATIONSHIP::const_iterator node = baseclass.find(tyi);
    if (node != baseclass.end())
        return node->second.size();
    return 0;
}

INT
IPA_CLASS_HIERARCHY::Get_Num_Sub_Classes(TY_INDEX tyi) {
    CLASS_RELATIONSHIP::const_iterator node = subclass.find(tyi);
    if (node != subclass.end())
        return node->second.size();
    return 0;
}

IPA_CLASS_HIERARCHY::CLASS_RELATIONSHIP::iterator IPA_CLASS_HIERARCHY::Get_Begin_Base_Classes()
{
    return baseclass.begin();
}

IPA_CLASS_HIERARCHY::CLASS_RELATIONSHIP::iterator IPA_CLASS_HIERARCHY::Get_End_Base_Classes()
{
    return baseclass.end();
}

IPA_CLASS_HIERARCHY::CLASS_RELATIONSHIP::iterator IPA_CLASS_HIERARCHY::Get_Begin_Sub_Classes()
{
    return subclass.begin();
}

IPA_CLASS_HIERARCHY::CLASS_RELATIONSHIP::iterator IPA_CLASS_HIERARCHY::Get_End_Sub_Classes()
{
    return subclass.end();
}

TY_INDEX
IPA_CLASS_HIERARCHY::Get_Base_Class(TY_INDEX tyi, INT index) {
    CLASS_RELATIONSHIP::const_iterator node = baseclass.find(tyi);
    if (node != baseclass.end())
        return node->second[index];
    return 0;
}

TY_INDEX
IPA_CLASS_HIERARCHY::Get_Sub_Class(TY_INDEX tyi, INT index) {
    CLASS_RELATIONSHIP::const_iterator node = subclass.find(tyi);
    if (node != subclass.end())
        return node->second[index];
    return 0;
}

void
IPA_CLASS_HIERARCHY::Add_Base_Class(TY_INDEX tyi, TY_INDEX base) {
    _ty_idx_list list;
    if (baseclass.find(tyi) != baseclass.end()) { 
        baseclass[tyi].push_back(base);
    }
    else {
        _ty_idx_list list;
        list.push_back(base);
        baseclass[tyi] = list;
    }  
    return;
}

void
IPA_CLASS_HIERARCHY::Add_Sub_Class(TY_INDEX tyi, TY_INDEX sub) {
    if (subclass.find(tyi) != subclass.end()) {
        subclass[tyi].push_back(sub);
    }
    else {
        _ty_idx_list list;
        list.push_back(sub);
        subclass[tyi] = list;
    }
    return;
}

BOOL
IPA_CLASS_HIERARCHY::Is_Sub_Class(TY_INDEX tyi, TY_INDEX sub) {
    if (subclass.find(tyi) != subclass.end()) {
        _ty_idx_list &list = subclass[tyi];
	for (size_t i = 0; i < list.size(); i++)
	    if (list[i] == sub)
                return TRUE;
    }
    return FALSE;
}

BOOL
IPA_CLASS_HIERARCHY::Is_Ancestor(TY_INDEX ancestor, TY_INDEX descendant) {
    if (ancestor == descendant || Is_Sub_Class(ancestor, descendant))
        return TRUE;
    for (UINT i=0; i<Get_Num_Sub_Classes(ancestor); i++) {
        TY_INDEX sub = Get_Sub_Class(ancestor, i);
        if (Is_Ancestor(sub, descendant))
            return TRUE;
    }
    return FALSE;
}

void 
IPA_CLASS_HIERARCHY::Add_Virtual_Base(TY_INDEX tyi, TY_INDEX base)
{
    _ty_idx_list list;
    if (virtual_bases.find(tyi) != virtual_bases.end()) { 
        virtual_bases[tyi].push_back(base);
    }
    else {
        _ty_idx_list list;
        list.push_back(base);
        virtual_bases[tyi] = list;
    }  
    return;
}

BOOL
IPA_CLASS_HIERARCHY::Is_Virtual_Base(TY_INDEX tyi, TY_INDEX base)
{
    if (virtual_bases.find(tyi) != virtual_bases.end()) {
        _ty_idx_list &list = virtual_bases[tyi];
	for (size_t i = 0; i < list.size(); i++)
	    if (list[i] == base)
                return TRUE;
    }
    return FALSE;
}

void IPA_CLASS_HIERARCHY::Get_Sub_Class_Hierarchy (TY_INDEX declared_class,
    hash_set<TY_INDEX>& targets) {
    targets.insert(declared_class);
    if (IPA_Class_Hierarchy->
        Get_Num_Sub_Classes(declared_class) == 0)
        return;
    for (UINT scls = 0; 
         scls < IPA_Class_Hierarchy->Get_Num_Sub_Classes(declared_class);
         ++scls) {
        TY_INDEX sub = IPA_Class_Hierarchy->Get_Sub_Class(declared_class, scls);
        Get_Sub_Class_Hierarchy (sub, targets);
    }
}

int IPA_CLASS_HIERARCHY::Num_Sub_Class_In_Hierarchy(TY_INDEX declared_class) {
    hash_set<TY_INDEX> subclses;
    Get_Sub_Class_Hierarchy (declared_class, subclses);
    return subclses.size();
}

IPA_CLASS_HIERARCHY*
Build_Class_Hierarchy() {

    IPA_CLASS_HIERARCHY *chg = CXX_NEW(IPA_CLASS_HIERARCHY, Malloc_Mem_Pool);
    UINT32 num_type = TY_Table_Size();

    // for every type in Ty_tab
    for (UINT32 i = 1; i < num_type; i++) {
	TY &ty = Ty_tab[i];    
        if (Is_Structure_Type(ty) && !TY_is_union(ty) && ty.Fld() > 0) {
            // find every base class
            FLD_IDX fld_idx = ty.Fld();
            do {
                FLD_HANDLE fld(fld_idx);
                if (FLD_is_base_class(fld)) {
                    TY_INDEX base = TY_IDX_index(FLD_type(fld));
                    chg->Add_Base_Class(i, base);
                    chg->Add_Sub_Class(base, i);
                    if (FLD_is_virtual(fld))
                       chg->Add_Virtual_Base(i, base);
                }
                if (FLD_last_field(fld))
                    break;
                fld_idx++;
            } while (1);
        }
    }
    return chg;
}

/*
 * Get the offset of the ancestor class in the class sub 
 */
size_t
IPA_CLASS_HIERARCHY::Get_Ancestor_Offset(TY_INDEX sub, TY_INDEX anc) {
    if (sub == anc)
        return 0;
    TY &ty = Ty_tab[sub];
    if (Is_Structure_Type(ty) && !TY_is_union(ty) && ty.Fld() > 0) {
        // search for each base class
        FLD_IDX fld_idx = ty.Fld();
        do {
            FLD_HANDLE fld(fld_idx);
            if (FLD_is_base_class(fld)) {
                TY_INDEX current_base = TY_IDX_index(FLD_type(fld));
                // if anc(current_base) is direct base class of sub
                if (current_base == anc) 
                    return FLD_ofst(fld);
                // otherwise, search current_base recursively
                size_t ofst = Get_Ancestor_Offset(current_base, anc);
                if (ofst != BASE_CLASS_NOT_FOUND) 
                    return FLD_ofst(fld) + ofst;
            }
            if (FLD_last_field(fld))
                break;
            fld_idx++;
        } while (1); 
    }
    return BASE_CLASS_NOT_FOUND;
}

