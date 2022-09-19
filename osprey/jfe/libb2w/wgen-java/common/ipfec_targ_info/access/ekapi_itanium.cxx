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
//=============================================================================
//
//  Module : ekapi_itanium.cxx
//  $Date  : $
//  $Author: marcel $
//  $Source: /proj/osprey/CVS/open64/osprey1.0/common/ipfec_targ_info/access/ekapi_itanium.cxx,v $
//
//  Description:
//  ============
//  hold all functions of itanium information in access layer of IPFEC.
//  and encapsulate KAPI first layer opcode of access layer.
//  Used for generate hardware specific scheduling information.
//=============================================================================


#include <list>
#include <map>
#include <stdarg.h>
#include "ekapi_ia64.h"
#include "ekapi_util.h"
#include "ekapi_itanium.h"

//////////////////////////////////////////////////////////////////////////
// VARNAME class
//////////////////////////////////////////////////////////////////////////

int VARNAME::count = 0;

VARNAME::VARNAME() {
    sprintf(varname,"&varname%d",count++);
}

VARNAME::VARNAME(char* prefix) {
    assert(strlen(prefix) <= 10);
    sprintf(varname,"&%s%d",prefix,count++);
}

char* VARNAME::Addr_Of_Vname() {
        return varname;
}

char* VARNAME::Vname() {
        return varname + 1;
}

//////////////////////////////////////////////////////////////////////////
//  RESOURCE CLASS
//////////////////////////////////////////////////////////////////////////



int  RESOURCE::total = 0;
int  RESOURCE::shift_count_total =0;
std::map <int,RESOURCE*> RESOURCE::resources;

// Initial function
RESOURCE::RESOURCE(char *name)
 :varname("resource"),name(name),id(total++),count(0)
{
    resources[id] = this;
}

// Get Resource by given an id;
RESOURCE* RESOURCE::Get(int i)
{
    assert(total > 0 && i >= 0 && i < total);
    return resources[i];
}


void RESOURCE::Update_Res(int res_count, bv32_t res_mask, int is_issue)
/////////////////////////////////////////////////////
// Given count and mask to update the class resource
////////////////////////////////////////////////////
{    
    count = res_count;
    mask  = res_mask;
    issue_res = is_issue;
    shift_count = shift_count_total;
    Calculate_Field_Word();
    Calculate_Field_Shift_Total();    
}
/////////////////////////////////////
void RESOURCE::Calculate_Field_Word()
/////////////////////////////////////
//  Calculate the number of bits for my field and set <field_width>
//  accordingly.
/////////////////////////////////////
{
    int i;
    Is_True(count > 0,("count in class resource is lower than 0"));
    word = 0;   
}


/////////////////////////////////////////
void RESOURCE::Calculate_Field_Shift_Total()
/////////////////////////////////////////
//  Caculate shift count for my field and 
//  Set <shift count total> accordingly
/////////////////////////////////////////
{
    int i;
    Is_True(count > 0,("count in class resource is lower than 0"));
    
    for ( i = 31 ; i >= 0 ; --i ) 
    {
         if ((1 << i) & count) {
             shift_count_total += i + 2;
             field_width = i + 2;
             break;
         }
    }         
}

/////////////////////////////////////
void RESOURCE::Output( FILE* fd )
/////////////////////////////////////
// Allocate my field in the resource reservation table.
/////////////////////////////////////
{
    fprintf(fd,"SI_RESOURCE %s = {\"%s\",%d,%d,%d,%d};\n",
               varname.Vname(),
               name,
               id,
               count,
               word,
               shift_count);
}

void RESOURCE::Output_All( FILE* fd )
{
    int i, _width, _max;
    UINT64 init_rrw=0;
    UINT64 over_rrw=0;

    for ( i = 0; i < total; ++i )
        resources[i]->Output(fd);

    fprintf(fd,"const int SI_resource_count = %d;\n",total);
    fprintf(fd,"SI_RESOURCE * const SI_resources[] = {");

    bool is_first = true;
    for ( i = 0; i < total; ++i ) {  
        fprintf(fd,"\n  %s",resources[i]->varname.Addr_Of_Vname());
        if (i!=total) fprintf(fd, ",");
    
        // Caculate initial rrw and over use mask
        _width = resources[i]->field_width;
        _max   = 1ULL << (_width-1);
    
        init_rrw |= (UINT64)(_max - resources[i]->count - 1) << resources[i]->shift_count;
        over_rrw |= (UINT64)(_max) << resources[i]->shift_count;
    }
    fprintf(fd,"\n};\n");
  
    // Print RRW SI_RRW_initializer and SI_RRW_overuse_mask;
    fprintf(fd, "const SI_RRW SI_RRW_initializer = 0x%llxLL;\n", init_rrw);
    fprintf(fd, "const SI_RRW SI_RRW_overuse_mask = 0x%llxLL;\n", over_rrw);
    fprintf(fd, "const int SI_issue_slot_count = %d;\n", 0);
    fprintf(fd, "SI_ISSUE_SLOT * const SI_issue_slots[1] = {0};\n\n");
  
}
////////////////////////////////////////////////////////////////////////////
// SCHE_INFO CLASS
///////////////////////////////////////////////////////////////////////////

int  SCHE_INFO::total = 0;
std::map <int,SCHE_INFO*> SCHE_INFO::fusi_list; // map fuid to schedule infor name;

SCHE_INFO::SCHE_INFO(void *pknobs, int fuid):
si_name("varname"), res_req_name("res_req"), total_res_name("varname"),id_set_name("varname"), 
res_req_num(0), id(total++), fu_id(fuid), res_req_cycle(1), use_res_sem(0)
{
    bv32_t mask;
    char *buf;
    int i;
    int sem_res_id = 1;
    
    mask = KAPI_cportMask4fu(pknobs, 0, fuid);
    buf = KAPI_EnumName(pknobs, fuid, "fu_t");
    name = strdup(buf+strlen("fu"));
    
    free(buf);
    
    Caculate_Field(mask);
    fusi_list[id] = this;
    
    // sem process;
    if (use_res_sem) {
        int latency = KAPI_CoreLatency(pknobs, fu_id, 0);  
        res_req_cycle = latency;
        for (i=1; i<latency; i++)
        {
             rrw[i] =  1ULL << RESOURCE::Get(sem_res_id)->Shift_Count();
             rrw_id[i] =  1ULL << sem_res_id; 
        }
    }    
}

SCHE_INFO::SCHE_INFO(void *pknobs, char *funame):
si_name("varname"), res_req_name("res_req"), total_res_name("varname"),id_set_name("varname"), 
res_req_num(0), id(total++), fu_id(-1), res_req_cycle(1)
{
    bv32_t mask;    
    mask = 0;   
    name = funame;
    
    // Caculate_Field(mask);
    fusi_list[id] = this;
}
void SCHE_INFO::Req_Issue_Resource()
{
    FmtAssert(RESOURCE::Get(0)->Is_Issue_Res(), 
                  ("The first source must be issue resource"));
    res_req_id[0] = 0;
    res_req_num++;
    rrw_id[res_req_cycle - 1] |= 1;
    rrw[res_req_cycle - 1]    |= 1;    
}

void SCHE_INFO::Caculate_Field(bv32_t cport_mask)
{
    int index;
    RESOURCE* res; 
    
    
    rrw[res_req_cycle - 1]=0;
    rrw_id[res_req_cycle - 1]=0;        
    
    // Find the required resource
    for (index=0; index < RESOURCE::Total() ; index++)
    {
        res = RESOURCE::Get(index);
        if ( ((res->CportMask() & cport_mask) == cport_mask) 
             && (cport_mask != 0)) {
            rrw_id[res_req_cycle - 1] |= 1ULL << index;
            rrw[res_req_cycle - 1]   |= 1ULL << res->Shift_Count();
            res_req_id[res_req_num+1] = index;
            res_req_num++;
            issue = 1;
        }
        
        // If function unit class is SEM, we will process the unusual 
        // resource SEM for semphone resource must last all cycle.
        // That is other instruction must stop to wait sem instruction
        // finished;
        if ((strcmp(name, "SEM")==0) && 
            (strcasecmp(res->Name(), "SEM")==0))  {
            rrw_id[res_req_cycle - 1] |= 1ULL << index;
            rrw[res_req_cycle - 1]    |= 1ULL << res->Shift_Count();
            res_req_id[res_req_num+1] = index;
            res_req_num++;   
            use_res_sem = 1;                  
        }        
    }    
    
    // issue resource process
    // If it will use a fu, then need a issue resource
    // Default the first resource issue;   
     
    if (issue) {        
        FmtAssert(RESOURCE::Get(0)->Is_Issue_Res(), 
                  ("The first source must be issue resource"));
        res_req_id[0] = 0;
        res_req_num++;
        rrw_id[res_req_cycle - 1] |= 1;
        rrw[res_req_cycle - 1]    |= 1;        
    }
}

void SCHE_INFO::Output_SI(void *pknobs, FILE *fd)
{
    int i;
    int max_src, max_dest;
    int load_access_time=0;
    int store_avail_time=0;
    
    VARNAME* operand_latency_info = new VARNAME("latency");
    VARNAME* result_latency_info  = new VARNAME("latency");
    
    max_src  = EKAPI_GetSrcOpndsMax(pknobs);
    max_dest = EKAPI_GetDestOpndsMax(pknobs);
    
    fprintf(fd, "/* Instruction group %s */\n", name);
    fprintf(fd, "static const SI_RRW %s[] = {\n", res_req_name.Vname());
    
    
    if (res_req_num != 0) {        
        fprintf(fd, "  %d", res_req_cycle);
        for(i=0; i<res_req_cycle; i++)
        {
            fprintf(fd, ",\n  0x%llxLL", rrw[i]);
        }
        
        fprintf(fd, "\n};\n");        
        
        fprintf(fd, "static const SI_RESOURCE_ID_SET %s[] = {\n", id_set_name.Vname());        
        for(i=0; i<res_req_cycle; i++)
        {
            fprintf(fd, "  0x%llxLL", rrw_id[i]);
            if (i != res_req_cycle -1) fprintf(fd, ",\n");
        }
        
        fprintf(fd, "\n};\n\n");
        fprintf(fd, "static SI_RESOURCE_TOTAL %s[] = {\n", total_res_name.Vname());
        for (i=0; i<res_req_num; i++)
        {
            fprintf(fd, "  {%s,1}/* %s */", 
                    RESOURCE::Get(res_req_id[i])->Addr_Of_Vname(),
                    RESOURCE::Get(res_req_id[i])->Name()
                    );
            if (i != res_req_num-1 )fprintf(fd, ",\n");
        }   
        fprintf(fd, "\n};\n\n");
    }
    else {
        fprintf(fd, "  0");
        fprintf(fd, "};\n");
    }
    
    // Print latency source and destine   
    fprintf(fd, "static const mUINT8 %s[] = {", operand_latency_info->Vname());
    for (i=0; i<max_src; i++)
    {
        fprintf(fd, "%d", 0);
        if (i != max_src-1) fprintf(fd, ",");
    }
    fprintf(fd, "};\n");
    
    fprintf(fd, "static const mUINT8 %s[] = {", result_latency_info->Vname());
    for (i=0; i<max_dest; i++)
    {
        if (fu_id >= 0) {
            fprintf(fd, "%d", KAPI_CoreLatency(pknobs, fu_id, 0));
        }
        else { fprintf(fd, "%d", 0);}
        if (i != max_dest-1) fprintf(fd, ",");
    }
    fprintf(fd, "};\n");
    
    // Special process to some function class
    // For example: LD function unit must give value to load access time;
    //              STR function unit must give value to store available time;
    if (strstr(name, "LD")) {
        load_access_time = KAPI_CoreLatency(pknobs, fu_id, 0);
    }
    if (strcmp(name, "ST")==0 || strcmp(name, "STF")==0) {
        store_avail_time = KAPI_CoreLatency(pknobs, fu_id, 0);
    }
        

    // print SI struct required resource      
    fprintf(fd, "static SI %s = {\n", si_name.Vname());
    fprintf(fd,"  \"%s\",\n",name);
    fprintf(fd,"  %-15d, /* id */\n",id);
    fprintf(fd,"  %-15s, /* operand latency */\n",
               operand_latency_info->Vname());
    fprintf(fd,"  %-15s, /* result latency */\n",
               result_latency_info->Vname());
    fprintf(fd,"  %-15d, /* load access time */\n",
               load_access_time);
    fprintf(fd,"  %-15d, /* last issue cycle */\n",
               0);
    fprintf(fd,"  %-15d, /* store available time */\n",
               store_avail_time);
    fprintf(fd,"  %-15s, /* resource requirement */\n",
               res_req_name.Vname());
    if (res_req_num != 0) {           
        fprintf(fd,"  %-15s, /* res id used set vec */\n",
                   id_set_name.Vname());
    }
    else {
        fprintf(fd,"  %-15s, /* res id used set vec */\n",
                   "0");
    }
    
    fprintf(fd,"  %-15d, /* II info size */\n",
               0);
    fprintf(fd,"  %-15s, /* II resource requirement vec */\n",
                "0");
    fprintf(fd,"  %-15s, /* II res id used set vec */\n",
                "0");
    fprintf(fd,"  {{");
    
    for ( i = 0; i <2; ++i ) {
        fprintf(fd, "0x%d", 0);
        if ( i < 1) fprintf(fd, ",");
    }
    fprintf(fd, "}}    , /* Bad IIs */\n");
    fprintf(fd,"  %-15d, /* valid issue slots vec size */\n",
                0);
    fprintf(fd,"  %-15s, /* valid issue slots vec */\n",
               "0");
    fprintf(fd,"  %-15d, /* resource count vec size */\n",
                 res_req_num);
                 
    if (res_req_num != 0) {           
        fprintf(fd,"  %-15s, /* resource count vec */\n",
                 total_res_name.Vname());
    }
    else {
        fprintf(fd,"  %-15s, /* resource count vec */\n",
                 "0");
    }
    
    fprintf(fd,"  %-15s  /* write-write interlock */\n",
                 "0");
    fprintf(fd,"};\n");   
    
}
void SCHE_INFO::Output_SI_ID(FILE *fd)
//  Output all the SI address;
{
    int i;
    fprintf(fd, "SI * const SI_ID_si[] = {");
    for ( i = 0; i < total; ++i ) 
    {  
        fprintf(fd,"\n  %s",fusi_list[i]->si_name.Addr_Of_Vname());
        if (i!=total) fprintf(fd, ",");
    }
    
    fprintf(fd,"\n};\n");
    fprintf(fd,"const int SI_ID_count = %d;\n\n", total);  
  
}

void SCHE_INFO::Output_OP_SI( void *pknobs, FILE *fd )
// Print opcode's corresponding schedule information
// opcount is op's count;
{
    int i;
    int opcount, fuid;
    char *funame;
    
    opcount = EKAPI_OpCount(pknobs);
    
    fprintf(fd, "SI * const SI_top_si[%d] = {", opcount);
    for ( i = 0; i < opcount; ++i ) 
    {  
        fuid    = EKAPI_Op2FuIndex(pknobs, i);             
        if (fuid>=0) {
            fprintf(fd,"\n  %-12s /* %s */",
                    fusi_list[fuid]->si_name.Addr_Of_Vname(),
                    EKAPI_OpName4id(pknobs, i)
                    );
        }
        else {
            // Use map to find the SI id; then use fusi_list[id] to get 
            // SI Infor.
            char *funame = EKAPI_Op2Fu(pknobs, i);
            if (strcmp(funame, "fuUNKNOWN") == 0) {
                fprintf(fd, "\n  %-12s /* %s */", 
                        fusi_list[total-2]->si_name.Addr_Of_Vname(),
                        EKAPI_OpName4id(pknobs, i));
            }
            else {
                fprintf(fd, "\n  %-12s /* %s */", 
                    fusi_list[total-1]->si_name.Addr_Of_Vname(),
                    EKAPI_OpName4id(pknobs, i));
            }             
        }
        if (i!=opcount) fprintf(fd, ",");
    }
    
    fprintf(fd,"\n};\n");
}

////////////////////////////////////////////////////////////
// Interface for Generator independent with class
////////////////////////////////////////////////////////////

void EKAPI_MapResource(void *pknobs, char *name, kapi_cluster_t cluster, ...)
// pknobs: pointer init by KAPI_Initialize() and KAPI_ia64_Initialize();
// name is resource name for document
// cluster: specify cluster ous use , 0 is the first cluster, -1 is all cluster
// Other : pair of ut_t and cutport;
//
// Because our resource  definition is not consist with KAPI's cport
// This function can be used to map our resource to KAPI's cport
// the mask is cport mask;
{
    va_list ap;    
    kapi_cluster_t clust;
    kapi_ut_t ut;
    kapi_port_t pport;
    kapi_cutport_t cutport;
    kapi_cport_t cport=0;
    int count = 0;
    bv32_t mask = 0;
    
    RESOURCE *current_resource = new RESOURCE(name);

    va_start(ap,cluster);
    while ( ( (ut = static_cast<kapi_ut_t>(va_arg(ap,int))) != -1 ) &&
            ( (cutport = va_arg(ap,int)) != -1) )
    {
        count++;
        KAPI_cutportInfo(pknobs, cluster, ut, cutport, &pport, &cport);
        //cport = 0;
        mask |= 1 << cport;        
    }
    
    current_resource->Update_Res(count, mask);
    
    va_end(ap);
}

void EKAPI_CreatResource(char *name, int count, int is_issue)
// Because some resource are leakage in KAPI. but we plan to 
// consider them as resource, so we will use creat resource;
{
    RESOURCE *current_resource = new RESOURCE(name); 
    current_resource->Update_Res(count, 0, is_issue);
}
void EKAPI_ClearResource(void)
{
    char *name="clear";
    RESOURCE *current_resource = new RESOURCE(name); 
    current_resource->Clear();
    VARNAME* varname_clear  = new VARNAME(name);
    varname_clear->Clear();
    SCHE_INFO* si_clear = new SCHE_INFO();
    si_clear->Clear();
}
