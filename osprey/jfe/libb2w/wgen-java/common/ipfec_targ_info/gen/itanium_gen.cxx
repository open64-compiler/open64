/*
  Copyright (C) 2000-2003, Intel Corporation
  All rights reserved.
  
  Redistribution and use in source and binary forms, with or without modification,
  are permitted provided that the following conditions are met:
  
  Redistributions of source code must retain the above copyright notice, this list
  of conditions and the following disclaimer. 
  
  Redistributions in binary form must reproduce the above copyright notice, this list
  of conditions and the following disclaimer in the documentation and/or other materials
  provided with the distribution. 

  Neither the name of the owner nor the names of its contributors may be used to endorse or
  promote products derived from this software without specific prior written permission. 

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR
  IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
  FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE CONTRIBUTORS BE LIABLE FOR
  ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
  NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
  BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

//-*-c++-*-

//*********************************************************************
//
// Module: itanium_gen.cxx
// $Date: 2005/10/21 19:00:00 $
// $Author: marcel $
// $Source: /proj/osprey/CVS/open64/osprey1.0/common/ipfec_targ_info/gen/itanium_gen.cxx,v $
//
// Description:
//   Generate the definition of itanium machine information and schedule infor.
//
//*********************************************************************

#include "itanium_gen.h"
#include "ekapi_itanium.h"

void Print_Resource(FILE *c_file)
{
    RESOURCE::Output_All(c_file);
}

void Print_SI(void *pknobs, FILE *c_file)
{
    int fu_count,fu_index;   
   
    fu_count = KAPI_fuCount(pknobs);    
    for (fu_index=0; fu_index<fu_count; fu_index++)
    {
        SCHE_INFO *si = new SCHE_INFO(pknobs, fu_index);
        si->Output_SI(pknobs, c_file);
    }   
    // Some instruction is dummy , that implemented by others instruction 
    // In fact it is intergrated form of others' instructions;
    // Such as break, chk.s;
    
    // dummy instruction need not issue resource
    SCHE_INFO *si_unknown = new SCHE_INFO(pknobs, "Dummy instructions" );  
    si_unknown->Output_SI(pknobs, c_file);
    
    // Dummy function class need issue resource;
    SCHE_INFO *si_dummy = new SCHE_INFO(pknobs, "dummy" );
    si_dummy->Req_Issue_Resource();  
    si_dummy->Output_SI(pknobs, c_file);
    
    SCHE_INFO::Output_SI_ID(c_file);
}    

void Print_Top_Si(void *pknobs, FILE *c_file)
{
    SCHE_INFO::Output_OP_SI(pknobs, c_file);
}

void Itanium_Generator(void *pknobs, GEN_MODE mode, MACHINE_TYPE type)
{
    FILE *c_file;
    int issue_width, max_slot, bundle_width;
    int issue_res_id, sem_res_id;
    
    if (type == MCK_TYPE)
        Init_Module_Files(mode, "itanium_mck",&c_file);
    else
        Init_Module_Files(mode, "itanium",&c_file);
    
    fprintf(c_file, "#include \"ti_si.h\"\n");
   
    bundle_width = KAPI_BundleIssueWidth(pknobs, -1);
    max_slot = EKAPI_GetMaxSlot(pknobs);
    issue_width = bundle_width * max_slot;
    
    EKAPI_ClearResource();
    // Set up resource list in our ways;
    // The first must be issue resource;
    
    EKAPI_CreatResource("issue", issue_width, 1);
    EKAPI_CreatResource("sem", 1);

    if (type == MCK_TYPE) { // Mckinley 
    EKAPI_MapResource(pknobs, "integer_or_memory", 0, 
                      kapi_utI, 0,
                      kapi_utI, 1,
                      kapi_utM, 0,
                      kapi_utM, 1,
                      kapi_utM, 2,
                      kapi_utM, 3, -1);
    EKAPI_MapResource(pknobs, "memory", 0,
                      kapi_utM, 0,
                      kapi_utM, 1,
                      kapi_utM, 2,
                      kapi_utM, 3, -1);
    EKAPI_MapResource(pknobs, "memory_ld", 0,
                      kapi_utM, 0,
                      kapi_utM, 1, -1);
    EKAPI_MapResource(pknobs, "memory_st", 0,
                      kapi_utM, 2,
                      kapi_utM, 3, -1);
    EKAPI_MapResource(pknobs, "memory0", 0,
                      kapi_utM, 0, -1);
    EKAPI_MapResource(pknobs, "memory2", 0,
                      kapi_utM, 2, -1);

    } else {/* Itanium */
    EKAPI_MapResource(pknobs, "integer_or_memory", 0, 
                      kapi_utI, 0,
                      kapi_utI, 1,
                      kapi_utM, 0,
                      kapi_utM, 1, -1);
    EKAPI_MapResource(pknobs, "memory", 0,
                      kapi_utM, 0,
                      kapi_utM, 1, -1);
    EKAPI_MapResource(pknobs, "memory0", 0,
                      kapi_utM, 0, -1);
    }
    EKAPI_MapResource(pknobs, "floating-point", 0,
                      kapi_utF, 0,
                      kapi_utF, 1, -1); 
    EKAPI_MapResource(pknobs, "floating-point0", 0,
                      kapi_utF, 0, -1);                  
    EKAPI_MapResource(pknobs, "integer", 0,
                      kapi_utI, 0,
                      kapi_utI, 1, -1); 
    EKAPI_MapResource(pknobs, "integer0", 0,
                      kapi_utI, 0, -1);                      
    EKAPI_MapResource(pknobs, "branch", 0,
                      kapi_utB, 0,
                      kapi_utB, 1,
                      kapi_utB, 2, -1); 
    EKAPI_MapResource(pknobs, "B0_or_B1", 0,
                      kapi_utB, 0,
                      kapi_utB, 1, -1);
    EKAPI_MapResource(pknobs, "B0_or_B2", 0,
                      kapi_utB, 0,
                      kapi_utB, 2, -1);
    EKAPI_MapResource(pknobs, "B2", 0,
                      kapi_utB, 2, -1);
    
    
   // EKAPI_MapResource(pknobs, "");
    Print_Resource(c_file);
    // Print information of resource from the list variable resource_list;
    
    Print_SI(pknobs, c_file);
    // Print schedule infoemation for each fuction class.
    
    Print_Top_Si(pknobs, c_file);
    // print topcode's schedule infor address;
    
    fclose(c_file);   
    
}
