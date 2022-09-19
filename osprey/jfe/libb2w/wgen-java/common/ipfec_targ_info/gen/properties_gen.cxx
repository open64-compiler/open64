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
// Module: opcode_gen.cxx
// $Date: 2005/10/21 19:00:00 $
// $Author: marcel $
// $Source: /proj/osprey/CVS/open64/osprey1.0/common/ipfec_targ_info/gen/properties_gen.cxx,v $
//
// Description:
//   Generate the definition of isa properties and definition
//
//*********************************************************************

#include "properties_gen.h"

static const char * const description[] = {
" /* ====================================================================\n\
 * ====================================================================\n\
 *\n\
 * Description:\n\
 *\n\
 *   A description of the properties (attributes) for the instructions\n\
 *   in the ISA. The description exports the following:\n\
 *\n\
 *   BOOL TOP_is_xxx(TOP topcode)\n\
 *       Return true/false if 'topcode' has/does-not-have the property\n\
 *       'xxx'.\n\
 *\n\
 * ====================================================================\n\
 * ====================================================================\n\
 */", NULL};
 
void Properties_Generator(void *pknobs, GEN_MODE mode)
{
    int prop_index, prop_count, false_prop_count;
    int op_index, op_count;
    FILE *c_file, *h_file, *export_file;
    char *prop_name;
    UINT64 prop_value;
    char suffix_UINT64[10] = "ULL";
    
    #ifdef TARG_WIN
        strcpy(suffix_UINT64, "mUI64");
    #endif 
    Init_Module_Files(mode, "targ_isa_properties", &c_file, &h_file, &export_file);
    Emit_Header(h_file, "targ_isa_properties", description);
    fprintf(c_file, "#include \"targ_isa_properties.h\"\n");
    
    fprintf(h_file, "extern const mUINT64 ISA_PROPERTIES_flags[];\n\n");
    fprintf(c_file, "\nconst mUINT64 ISA_PROPERTIES_flags[] = {\n");
    fprintf(export_file, "ISA_PROPERTIES_flags\n");
    
    prop_count = EKAPI_OppCount(pknobs);
    // Generate prop macro define to hfile;
    for(prop_index=0; prop_index<prop_count; prop_index++ )
    {
        char *buf = EKAPI_Oppid2Name(pknobs, prop_index);
        Is_True(buf, ("op prop name return NULL"));
        
        if (strstr(buf, "OPP_")) {
            prop_name =buf + strlen("OPP_");
        }
        prop_value = 1ULL << prop_index;
        fprintf(h_file, "#define PROP_%-16s 0x%llx%s\n",
                prop_name,
                prop_value,
                suffix_UINT64
               );
        free(buf);
    }
    fprintf(h_file, "\n\n");
    
    // Generate prop macro define TOP_is_XXX function to hfile
    for(prop_index=0; prop_index<prop_count; prop_index++ )
    {
        char *buf = EKAPI_Oppid2Name(pknobs, prop_index);
        Is_True(buf, ("op properties name return NULL"));
        
        if (strstr(buf, "OPP_")) {
            prop_name =buf + strlen("OPP_");
        }
        
        fprintf(h_file, "#define TOP_is_%s(t)	 (ISA_PROPERTIES_flags[(INT)t] & PROP_%s)\n",
                prop_name,
                prop_name            
                );
       
        free(buf);                               
    }
    fprintf(h_file, "\n");
        
    // Generate flase prop macro function
    false_prop_count = EKAPI_FalseOppCount(pknobs);
    for(prop_index=0; prop_index<false_prop_count; prop_index++ )
    {
        char *buf = EKAPI_FalseOppid2Name(pknobs, prop_index);
        Is_True(buf, ("op flase properties name return NULL"));
        
        if (strstr(buf, "OPP_")) {
            prop_name =buf + strlen("OPP_");
        }
        
        fprintf(h_file, "#define TOP_is_%s(t)	 (FALSE)\n",
                prop_name              
                );
        free(buf);                               
    }
 
    
    // Generate properties flag for each op to cfile
    op_count = EKAPI_OpCount(pknobs);
    for (op_index=0; op_index<op_count; op_index++)
    {
        char comment[200]="";
        UINT64 flag = EKAPI_OppMask4op(pknobs, op_index);
        
        // construct comment for print into cfile
        sprintf(comment, "/* %s:", EKAPI_OpName4id(pknobs, op_index));
        for(prop_index=0; prop_index<prop_count; prop_index++)
        {
         
            if (flag & (1 << prop_index)) {
                char *buf = EKAPI_Oppid2Name(pknobs, prop_index);
                Is_True(buf, ("Op name return NULL"));
                
                if (strstr(buf, "OPP_")) {
                    prop_name =buf + strlen("OPP_");
                }
                strcat(comment, " ");
                strcat(comment, prop_name);
                free(buf);
            }
            
        }    
        strcat(comment," */" );
        fprintf(c_file,"  0x%016llx%s, %s\n", flag, suffix_UINT64, comment);
   }
    fprintf(c_file, "};\n");
    
    Emit_Tailer(h_file);
    Close_Module_Files(mode, &c_file, &h_file, &export_file);
}
