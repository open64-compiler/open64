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

#ifndef cxx_ipa_devirtual_INCLUDED
#define cxx_ipa_devirtual_INCLUDED

/*
 * Main function of devirtualization in IPA phase
 */
extern void
IPA_devirtualization(); // earlier version

extern void
IPA_Fast_Static_Analysis_VF ();

#include <list>
#include <ext/hash_set>
#include <ext/hash_map>
#include <string>
#include <map>
using std::list;
using std::map;
using __gnu_cxx::hash_map;
using __gnu_cxx::hash_set;

#define INVALID_VIRTUAL_FUNCTION_COUNT 0xffffffff

class Virtual_Function {
    TY_INDEX class_ty_index;				//indicate in which class this virtual function is defined
    ST_IDX st_idx;							//the st_idx of this virtual function
    size_t offset;							//the offset of this virtual function in vtable
    vector< Virtual_Function*> overriders;		//the overrides of this virtual function
public:
    Virtual_Function(TY_INDEX _class_ty_index, ST_IDX _st_idx, size_t _offset):
        class_ty_index(_class_ty_index),
        st_idx(_st_idx),
        offset(_offset)
    {}
    Virtual_Function(const Virtual_Function& vf)
    {
        class_ty_index = vf.class_ty_index;
        st_idx = vf.st_idx;
        offset = vf.offset;
        overriders = vf.overriders;
    }
    TY_INDEX Get_class_ty_index()
    {
        return class_ty_index;
    }
    ST_IDX Get_st_idx()
    {
        return st_idx;
    }
    size_t Get_offset()
    {
        return offset;
    }
    UINT Candidate_count()
    {
        int count = 0;
        if(strcmp(ST_name(st_idx), "__cxa_pure_virtual") != 0)
            count++;
		
        hash_set<ST_IDX> st_idx_set;
        for(UINT i = 0; i < overriders.size(); i++)
        {
            //a virtual function could be declared as pure virtual function by its
            //sub class, so there might be multiple __cxa_pure_virtual taken as
            //overrider.
            if(strcmp(ST_name(overriders[i]->Get_st_idx()), "__cxa_pure_virtual") == 0)
                continue;
            Is_True(st_idx_set.find(overriders[i]->Get_st_idx()) == st_idx_set.end()
                , ("Overriders should not have duplication"));
            st_idx_set.insert(overriders[i]->Get_st_idx());
            count++;
        }

        return count;
    }
    ST_IDX Get_The_Only_Candidate()
    {
        Is_True(Candidate_count() == 1, ("There are multiple candidates"));

        if(strcmp(ST_name(st_idx), "__cxa_pure_virtual") != 0)
            return st_idx;
        else
        {
            for(UINT i = 0; i < overriders.size(); i++)
            {
                //a virtual function could be declared as pure virtual function by its
                //sub class, so there might be multiple __cxa_pure_virtual taken as
                //overrider.
                if(strcmp(ST_name(overriders[i]->Get_st_idx()), "__cxa_pure_virtual") != 0)
                    return overriders[i]->Get_st_idx();;
            }
        }
    }
    UINT32 Overriders_count()
    {
        return overriders.size();
    }
    Virtual_Function* Get_Overrider(UINT32 index)
    {
        Is_True(index < overriders.size(), ("Invalid index of overrider"));

        return overriders[index];
    }
    void Add_Overriders(Virtual_Function* p_vf)
    {
        if(st_idx != p_vf->Get_st_idx())
        {
            overriders.push_back(p_vf);
        }
        for(UINT32 i = 0; i < p_vf->Overriders_count(); i++)
        {
            overriders.push_back(p_vf->Get_Overrider(i));
        }
    }
    void Print(FILE *fp)
    {
        fprintf(fp, "\tVF:%s   offset:%d   overriders: \n"
            , ST_name(st_idx), offset);
        for(UINT32 i = 0; i < overriders.size(); i++)
        {
            fprintf(fp, "\t\tClass:%s   VF:%s\n"
                , TY_name(Ty_tab[overriders[i]->Get_class_ty_index()]), ST_name(overriders[i]->Get_st_idx()));
        }
    }
};

class Virtual_Functions {
    TY_INDEX class_ty_index;
    ST_IDX vtable_st_idx;				//the vtable st_idx
    UINT32 vtable_offset;				//the offset of the actual vtable in the vtable struct
    BOOL usable;						//it this class is a secondary base class, usable would be set FALSE
    BOOL propagated;					//indicate whether the overridden info is propagated over this class
    vector < Virtual_Function > _vf_list;	//the virtual functions of this class
public:
    Virtual_Functions(): 
        class_ty_index(0),
        vtable_st_idx(0),
        vtable_offset(0),
        usable(TRUE),
        propagated(FALSE)
    {}		
    BOOL Is_Usable() {
        return usable;
    }
    void Set_Usable() {
        usable = TRUE;
    }
    void Set_Unusable() {
        usable = FALSE;
    }
    BOOL Is_Propagated()
    {
        return propagated;
    }
    void Set_Propagated() {
        propagated = TRUE;
    }
    TY_INDEX Get_class_ty_index()
    {
        return class_ty_index;
    }
    void Set_class_ty_index(TY_INDEX idx) {
        class_ty_index = idx;
    }
    ST_IDX Get_vtable_st_idx()
    {
        return vtable_st_idx;
    }
    void Set_vtable_st_idx(ST_IDX idx) {
        vtable_st_idx = idx;
    }
    UINT32 Get_vtable_offset()
    {
        return vtable_offset;
    }
    void Set_vtable_offset(UINT offset) {
        vtable_offset = offset;
    }
    UINT Get_Count() {
        return _vf_list.size();
    }
    BOOL Out_Of_Range(size_t offset)
    {
        if(offset / Pointer_Size < _vf_list.size())
            return FALSE;
        return TRUE;
    }
    Virtual_Function* Get_Virtual_Function_by_Index(UINT32 index)
    {
        Is_True(index < _vf_list.size(), ("Index exceeds the size of virtual function list"));
        return &_vf_list[index];
    }
    Virtual_Function* Get_Virtual_Function_by_Offset(size_t offset)
    {
        return Get_Virtual_Function_by_Index(offset / Pointer_Size);
    }
    void Add_Overriders(int index, Virtual_Function* p_vf)
    {
       Is_True(p_vf && p_vf->Get_class_ty_index() != 0, ("Invalid overrider type index"));
        _vf_list[index].Add_Overriders(p_vf);
    }
    void Print(FILE *fp)
    {
        fprintf(fp, "Class:%s   Vtab:%s  Offset:%d  Usable:%s  Propagated:%s\n"
            , TY_name(Ty_tab[class_ty_index]), ST_name(vtable_st_idx), vtable_offset
            ,usable ? "true" : "false", propagated ? "true" : "false");
        for(int i = 0; i < _vf_list.size(); i++)
        {
            _vf_list[i].Print(fp);
        }
    }
    void Add_Virtual_Function(Virtual_Function& vf)
    {
       Is_True(Get_class_ty_index() != 0 && vf.Get_class_ty_index() != 0, ("Invalid class_ty_index"));
        _vf_list.push_back(vf);        
    }
};

struct DevirCallsiteInfo
{
   char *caller;
   int lineno;
   int callsite_id;
   DevirCallsiteInfo (char *str, int ln, int id): lineno(ln), callsite_id(id) {
     caller = strdup(str);
   }
   DevirCallsiteInfo (DevirCallsiteInfo &that) {
       caller = strdup(that.caller);
       lineno = that.lineno;
       callsite_id = that. callsite_id;
   }

   ~DevirCallsiteInfo () { free(caller); }
};

class IPA_VIRTUAL_FUNCTION_TRANSFORM {
    public:
    struct eqstr {
        bool operator()(const char* s1, const char* s2) const
          { return strcmp(s1, s2) == 0; }
      };

    typedef hash_map<const char*, vector<DevirCallsiteInfo *> * ,__gnu_cxx::hash<const char*>, eqstr >
         DevirCallsiteMap;

// After analysis, an object of the following type is available
// for each transformable virtual function       
    class VIRTUAL_FUNCTION_CANDIDATE {
        public:
            // Replace icall with call
            BOOL Single_Callee;
// The virtual function's call site summary
            SUMMARY_CALLSITE *Virtual_Call_Site;
// Dummy call site summary that will now be set to fun_st_idx
            SUMMARY_CALLSITE *Dummy_Call_Site;
// Caller node containing the virtual function call site
            IPA_NODE *Caller;
// The virtual table of the instance attached to callee
            WN* Virtual_Table;
            ST_IDX Virtual_Table_st_idx;
// The offset of virtual table
            UINT32 Offset;
// The ST_IDX of the resolved function contained in 
// above WN* irtual_table
            ST_IDX Transform_Function_ST_IDX;
    };
// mapping NODE_INDEX indices against 
// a list of transform candidates 
    hash_map <NODE_INDEX, list<VIRTUAL_FUNCTION_CANDIDATE> > 
        Node_Transform_Lists;

// optimization controlling 
    bool Class_Hierarchy_Analysis, Class_Type_Analysis;
// transformation initializing and finalizing
    void Initialize_Virtual_Function_Transform ();
    void Finalize_Virtual_Function_Transform ();

// fixup
    void Fixup_Virtual_Function_Callsites ();
    void Fixup_Virtual_Function_Callsites_Per_Node (IPA_NODE *method);

// prepare for optimization by obtaining constructors
// and building class hierarchy graph
    void Prepare_Virtual_Function_Transform ();
    
// main function for transform 
    void Transform_Virtual_Functions ();
    
// analyze and transform
    void Transform_Virtual_Functions_Per_Node_ORIG (IPA_NODE* method);
    void Transform_Virtual_Functions_Per_Node_NEW (IPA_NODE* method);

    void Apply_Virtual_Function_Transform (VIRTUAL_FUNCTION_CANDIDATE);

// collect constructor calls from call graph
    hash_set <TY_INDEX> Constructed_Types;
    hash_map <TY_INDEX, PU_IDX> Constructor_Map;
    TY_INDEX Get_Constructor_Type (SUMMARY_SYMBOL *func_sym);
    void Identify_Constructors ();

// A mapping between PUs and NODE_INDEX indices. 
// The other way mapping is available directly 
// but this is not   
    hash_map <PU_IDX, NODE_INDEX> pu_node_index_map;
    void Build_PU_NODE_INDEX_Map();

// utils
    int Get_Callsite_Count (IPA_NODE* method);

    typedef hash_map <WN_MAP_ID, WN *> WN_IPA_MAP;
    hash_map <NODE_INDEX, WN_IPA_MAP> Node_Virtual_Function_Whirl_Map;
    void Build_Virtual_Function_Whirl_Map (
            IPA_NODE* method, 
            WN_IPA_MAP& a_wn_ipa_map);

    hash_set<TY_INDEX> Identify_Instances_From_Subclass_Hierarchy (
            TY_INDEX declared_class);

    void Identify_Virtual_Function (
            hash_set<TY_INDEX> constructed_types_set, 
            SUMMARY_CALLSITE *callsite,
            VIRTUAL_FUNCTION_CANDIDATE& vcand);

    void Locate_Virtual_Function_In_Virtual_Table (
            IPA_NODE *constructor,
            WN* vtab, size_t func_offset, 
            VIRTUAL_FUNCTION_CANDIDATE& vcand);

// debug 
    bool Enable_Debug;
    FILE *Virtual_Whirls;
    FILE *Transformed_Whirls;
    FILE *Callsite_Dump_file;

    void Dump_Constructors ();
    void Dump_Callsite( IPA_NODE *method,
                        SUMMARY_CALLSITE* callsite,
                        WN_IPA_MAP& wn_map,
                        bool devirtualized,
                        ST_IDX callee_st_idx);
    void Read_Callsite();
    DevirCallsiteInfo * Find_Callsite(IPA_NODE *method,
                                      SUMMARY_CALLSITE* callsite,
                                      WN_IPA_MAP& wn_map);

    typedef hash_map <NODE_INDEX, hash_map <WN_MAP_ID, hash_set<TY_INDEX> > > VIRTUAL_FUNCTION_DEBUG_DATA;
    VIRTUAL_FUNCTION_DEBUG_DATA Transform_Debug_Data;
    void Dump_Virtual_Function_Transform_Candidates ();
    hash_map <NODE_INDEX, IPA_NODE*> Optimized_Methods_By_NODE_INDEX;

// statistics
    bool Enable_Statistics;
    int Num_VFs_Count;
    int Class_Hierarchy_Transform_Count;
    int Class_Instance_Transform_Count;
    void Print_Statistics ();
    void Miss_Hit_Profile ();
    void Histogram_Statistics ();
    hash_map <int, int > Num_Instances;
    hash_map <int, int > Class_Hierarchy_Depth;
    hash_map <int, list<std::string> > Miss_Hit_Tag;
    void Update_Class_Hierarchy_Depth (int index);
    void Update_Instances (int index);

// profiling
    bool Enable_Profile;
    ST_IDX Miss_ST_IDX, Hit_ST_IDX;

// virtual function overridden map
    typedef hash_map <TY_INDEX, Virtual_Functions > CLASS_VF_MAP;
    CLASS_VF_MAP class_vf_map;
    // build up virtual function overridden map
    void Build_Virtual_Function_Overridden_Map();
    void Record_Virtual_Functions_for_Class();
    void Mark_Secondary_Bases_Unusable();
    void Mark_Self_And_Ancestor_Unusable(TY_INDEX declared_class);
    void Print_Overridden_Map(FILE *fp);
    UINT32 Candidate_Count(TY_INDEX declared_class, size_t offset);
    void Propagate_Overridden_Info();
    void Propagate_Overridden_Info_For_Class(TY_INDEX declared_class);
    ST_IDX Get_The_Only_Candidate(TY_INDEX declared_class, size_t offset);

};

#endif

