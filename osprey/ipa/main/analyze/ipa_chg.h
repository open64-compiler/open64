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

#ifndef cxx_ipa_chg_INCLUDED
#define cxx_ipa_chg_INCLUDED

#include "symtab.h"
#include "ipc_symtab_merge.h"
#include <vector>
#include <ext/hash_map>
#include <ext/hash_set>

using __gnu_cxx::hash_map;
using __gnu_cxx::hash_set;

#define BASE_CLASS_NOT_FOUND (size_t)-1

class IPA_CLASS_HIERARCHY {

public:

    typedef vector <TY_INDEX> _ty_idx_list;
    typedef hash_map <TY_INDEX, _ty_idx_list> CLASS_RELATIONSHIP; 
	
    IPA_CLASS_HIERARCHY();
    ~IPA_CLASS_HIERARCHY();

    INT Get_Num_Base_Classes(TY_INDEX tyi);
    INT Get_Num_Sub_Classes(TY_INDEX tyi);

    CLASS_RELATIONSHIP::iterator Get_Begin_Base_Classes();
    CLASS_RELATIONSHIP::iterator Get_End_Base_Classes();
    CLASS_RELATIONSHIP::iterator Get_Begin_Sub_Classes();
    CLASS_RELATIONSHIP::iterator Get_End_Sub_Classes();
    CLASS_RELATIONSHIP::iterator Get_Begin_Virtual_Base();
    CLASS_RELATIONSHIP::iterator Get_End_Virtual_Base();

    TY_INDEX Get_Base_Class(TY_INDEX tyi, INT index); 
    TY_INDEX Get_Sub_Class(TY_INDEX tyi, INT index);

    void Add_Base_Class(TY_INDEX tyi, TY_INDEX base);
    void Add_Sub_Class(TY_INDEX tyi, TY_INDEX sub);

    BOOL Is_Sub_Class(TY_INDEX tyi, TY_INDEX sub);
    BOOL Is_Ancestor(TY_INDEX ancestor, TY_INDEX descendant); 

    void Add_Virtual_Base(TY_INDEX tyi, TY_INDEX base);
    BOOL Is_Virtual_Base(TY_INDEX tyi, TY_INDEX base);

    size_t Get_Ancestor_Offset(TY_INDEX sub, TY_INDEX anc);

    void Get_Sub_Class_Hierarchy (TY_INDEX declared_class,
        hash_set<TY_INDEX>& targets);

    int Num_Sub_Class_In_Hierarchy(TY_INDEX declared_class);

    void Print_IPA_CLASS_HIERARCHY() {
      FILE *_debug = fopen("class_hierarchy.trace", "w");
      fprintf(_debug, 
          "Baseclass_relationship_size:%zu; Subclass_relationship_size:%zu\n", 
          baseclass.size(), subclass.size());
      Print_helper(baseclass,_debug,true); 
      Print_helper(subclass,_debug,false); 
      fclose(_debug);
    }


    void Print_helper(CLASS_RELATIONSHIP myhier, FILE *_debug, bool for_bases) {
      CLASS_RELATIONSHIP::iterator _iterator;
      if (for_bases)
          fprintf(_debug, "basetypes\n");
      else
          fprintf(_debug, "subtypes\n");
      fprintf(_debug, "in type ids\n");
      for (_iterator = myhier.begin(); 
          _iterator != myhier.end();
          ++_iterator) {
        fprintf(_debug, "%d:", _iterator->first);
        _ty_idx_list::iterator _viterator;
        _ty_idx_list _vect = _iterator->second;
        for (_viterator = _vect.begin();
            _viterator != _vect.end();
            ++_viterator) {
          const char *vir = 
             (for_bases && 
              Is_Virtual_Base(_iterator->first, *_viterator)) ?
              " virtual " : " ";
          fprintf (_debug, "%s%d", vir, *_viterator);
        }
        fprintf (_debug, "\n");
      }
      fprintf(_debug, "in dump_type style\n");
      for (_iterator = myhier.begin(); 
          _iterator != myhier.end();
          ++_iterator) {
        fprintf(_debug, "\n[Type_idx %d] ", _iterator->first);
        Ty_tab[_iterator->first].Print(_debug);
        _ty_idx_list::iterator _viterator;
        _ty_idx_list _vect = _iterator->second;
        for (_viterator = _vect.begin();
            _viterator != _vect.end();
            ++_viterator) {
          const char *vir = 
             (for_bases && 
              Is_Virtual_Base(_iterator->first, *_viterator)) ?
              " virtual " : " ";
          fprintf (_debug, "+++++++ %s [type_idx %d] %s", 
                for_bases ? "base class" : "derived class",
                *_viterator, vir);
          Ty_tab[*_viterator].Print(_debug);
          fprintf (_debug, "+++++++\n");
        }
      }
    }

private:

    CLASS_RELATIONSHIP baseclass;
    CLASS_RELATIONSHIP subclass;
    CLASS_RELATIONSHIP virtual_bases;
};

// Global class hierarchy graph
extern IPA_CLASS_HIERARCHY* IPA_Class_Hierarchy;

// Build the global class hierarchy graph
extern IPA_CLASS_HIERARCHY* Build_Class_Hierarchy();

#endif
