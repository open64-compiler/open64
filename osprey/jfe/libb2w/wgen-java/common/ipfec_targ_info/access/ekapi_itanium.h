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
//  Module : ekpi_itanium.h
//  $Date  : $
//  $Author: marcel $
//  $Source: /proj/osprey/CVS/open64/osprey1.0/common/ipfec_targ_info/access/ekapi_itanium.h,v $
//
//  Description:
//  ============
//   hold all functions of itanium information in access layer of IPFEC.
//   and encapsulate KAPI first layer opcode of access layer.
//   Used for generate hardware specific scheduling information.
//=============================================================================

#ifndef EKAPI_ITANIUM_INCLUDED
#define EKAPI_ITANIUM_INCLUDED

#ifdef __cplusplus
extern "C" {
#endif



#ifdef __cplusplus
}
#endif

#include <list>
#include <map>
#include <stdarg.h>

/////////////////////////////////////
class VARNAME {
/////////////////////////////////////
public:
    VARNAME();
    // Generate a unique name for a variable.  Don't care for the prefix.
    VARNAME(char* prefix);
    // Generate a unique name for a variable.  Force to use a particular prefix.
    char* Vname();
    // Return the name.  This is the name under which the object is defined.
    char* Addr_Of_Vname();
    // Return a pointer to the named object.
    void Clear() { count=0; };
    // Clear all name object.
 
private:
    char varname[20];       // Where to keep the name.  
    static int count;     // the count of name For generating the unique names.
};

class RESOURCE{

public:
    RESOURCE(char *name); //initial funciton
    
    static RESOURCE* Get(int id);
    
    void Update_Res(int res_count, bv32_t res_mask, int is_issue = 0);
    // Mappping the resource of pro64 to the specified set of 
    // cluster/ut/cutport. the parameter ended by -1;
    
 
    char* Addr_Of_Vname() { return varname.Addr_Of_Vname(); }
    // Return name of pointer to this resource object (in generated code).
    
    unsigned int Count() const { return count; }
    // How may members?
    
    int Word() const { return word; }
    // Index of word in resource reservation table.  (We have sort of allowed
    // there to be more than one, but this is probably not fully working.)
    
    int Id() const { return id; }
    // Unique ID of resource.  Index into table of pointers to resources in the
    // generated file.
    
    unsigned int Shift_Count() const { return shift_count; }
    // Bit index of the field in the resource reservation word.
    
    static void Output_All( FILE* fd );
    // Write out all the resource info to <fd>.
    
    bv32_t CportMask() const { return mask; }
    // cport mask 
    
    static int Total()  {return total; }
    
    bool Is_Issue_Res() {return issue_res;}
    
    int Width() const {return field_width;}
    
    char const *Name() {return name;}

    void Clear() { total = 0; shift_count_total=0; resources.clear();};
private:
    char const *name;  // For documentation and debugging
    VARNAME varname;       // Generated symbolic name;
    int count;         // Available per cycle;
  
    int word;          // Which word in the table?
    int field_width;   // How wide the field?
    int shift_count;   // How much to shift (starting pos of the low
                       //   order bit
                        
    int issue_res;         // indicate it is issue resource, default is 0;                        
                           
    static int shift_count_total; // holds all resource before this shift count;
    
    const int id;      // Unique identical
    static int total;  // Total number of different RESs (not the the
                       //   total of their counts, 1 for each RES)
    
    static std::map <int,RESOURCE*> resources;
                       // Map of all resources, ordered by their Id's
    
    bv32_t mask;       // cport mask;
    
    void Calculate_Field_Word();
    void Calculate_Field_Shift_Total();

    static void Calculate_Fields();
    // Calculate fields for all resources.  This can only be done at the very
    // end becaue we may not know for sure that there are no multiple resources
    // until then.

    void Output( FILE* fd ); //output each resource information
    
};

class SCHE_INFO{

public:
    void Output_SI(void *pknobs, FILE *fd); // Output Schedule information for this SI;
    static void Output_SI_ID(FILE *fd ); // Output all SI name address;
    static void Output_OP_SI( void *pknobs, FILE *fd ); // Output all Op's SI address;
    SCHE_INFO(void *pknobs, int fuid);    // Initialize according function class id;
    SCHE_INFO(void *pknobs, char *funame); // Initialize according name self defined
    SCHE_INFO(void){}; //Initialize a object;
    void Req_Issue_Resource();            // Add Issue Resource to require list
    void Clear() { total=0; fusi_list.clear();}; // Clear all SI information   

private:    
    int id;          // Id
    static int total;    // Total of schedule information
    int fu_id;        // According function index of KAPI;
    char *name;      // Name for document
    int res_req_num; // Required resource number;
    
    int res_req_cycle; // always is require resource in first cycle, but SEM instrucion
                       // nust need require resource sem in all its cycle;
    std::map <int, INT64> rrw;       // Required resource word arranged by each resource occupy 
                     // specified width according shift_count;
                     
    std::map <int, INT64> rrw_id;    // Required resource flag ranged by id order;
                     // each resource occupy one bit;
                   
    std::map <int, int> res_req_id; // Holds the required resource id;
       
    BOOL issue;       // Need issue or not
    
    BOOL use_res_sem; // use res sem resource or not;
    
    VARNAME res_req_name; // Name of res req struct name in target file;
    
    VARNAME id_set_name;  // Name of id set which keeps rrw_id in target file;
    
    VARNAME total_res_name; // Name of total_res which keeps all address of resource
                          // required in target file;
                          
    VARNAME si_name;     // Name of SI struct;     
    
    static std::map <int,SCHE_INFO*> fusi_list; // mapping of function class id and si 
    
    void Caculate_Field(bv32_t cport_mask);
      
};

#endif
