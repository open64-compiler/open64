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
 */


#ifndef OUTPUT_FUNC_START_PROFILER_INCLUDE
#define OUTPUT_FUNC_START_PROFILER_INCLUDE


#include "wn.h"
#include "pu_info.h"
#include "DaVinci.h"
#include "cxx_template.h"
#include "symtab.h"
#include "wn_map.h"
#include "mempool.h"		//it includes "defs.h"
#include "wn_util.h"
#include "symtab_utils.h"
#include "lwn_util.h"		//WN_Parentize()
#include "ir_reader.h"
#include "tracing.h"
#include "data_layout.h"	//Allocate_Object()

class OUTPUT_FUNC_START_PROFILER_MEM {
protected:
    MEM_POOL _mem_pool;

    OUTPUT_FUNC_START_PROFILER_MEM (void) {
        MEM_POOL_Initialize (&_mem_pool, "OUTPUT_FUNC_START_PROFILER_MEM", TRUE);
        MEM_POOL_Push(&_mem_pool);
    };   
    
    ~OUTPUT_FUNC_START_PROFILER_MEM (void) {
        MEM_POOL_Pop (&_mem_pool);   
        MEM_POOL_Delete (&_mem_pool);
    };
};


class OUTPUT_FUNC_START_PROFILER : public OUTPUT_FUNC_START_PROFILER_MEM {
private:
    const char* _file_name;
    PU_Info** _pu_tree_p;

    static const char* _prefix;
    static const char* _init_proc;
    static const char* _lpbx_0;
   
    ST* _func_st;
    ST* _lpbx_st;
    WN* _func_entry;
    WN* _func_body;
    PU_Info* _pu_info;
    SYMTAB_IDX _func_level;
    char* _func_name;

    char* Construct_Func_Name(const char *name);
public:

    void Set_file_name(const char* file) { _file_name = file; }
    void Set_pu_tree(PU_Info** p) { _pu_tree_p = p; }

    static const char* Get_prefix() { return _prefix; }

    OUTPUT_FUNC_START_PROFILER (const char* src_file_name = NULL, PU_Info** pu_tree_p = NULL);
    ~OUTPUT_FUNC_START_PROFILER (void) {}
            
    void Generate_Func_Start_Profiler_PU(void);
    void Fill_In_Func_Body(void);
};

extern OUTPUT_FUNC_START_PROFILER Output_Func_Start_Profiler;

#endif


