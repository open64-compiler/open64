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
//  Module : ekapi_register.cxx
//  $Date  : $
//  $Author: marcel $
//  $Source: /proj/osprey/CVS/open64/osprey1.0/common/ipfec_targ_info/ipfec_targ_gen.cxx,v $
//
//  Description:
//  ============
//  Main function to generate all target file
//=============================================================================

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>


#include "ekapi_ia64.h" //kapi library and access layer of MD

#include "issue_port_gen.h"
#include "opcode_gen.h"
#include "subset_gen.h"
#include "properties_gen.h"
#include "operands_gen.h"
#include "enums_gen.h"
#include "lits_gen.h"
#include "bundle_gen.h"
#include "reg_gen.h"
#include "itanium_gen.h"
#include "bypass_gen.h"
#include "proc_gen_new.h"
#include "proc_properties_gen_new.h"
#include "cache_info_gen.h"

void Generate_MM(char *knobf, char *eknobf, MACHINE_TYPE type)
{
    FILE *fp1, *fp2;
    void *pknobs;

    fp1  = fopen( knobf, "r" );
    fp2  = fopen( eknobf, "r" );
    if ((fp1 == NULL)||(fp2 == NULL))
    {
        printf("ERROR: can't open files %s and %s\n", knobf, eknobf);
        exit(-1);
    } 

    // Initialize
    pknobs = KAPI_Initialize(fp2,fp1,NULL);
    pknobs = KAPI_ia64_Initialize(pknobs);
    if (fp2 != NULL) fclose(fp2);
    if (fp1 != NULL) fclose(fp1);

    if (type == MCK_TYPE) {
        Issue_Port_Generator(pknobs, GEN_MODE_FILE, type);
        Itanium_Generator(pknobs, GEN_MODE_FILE, type);
        Bypass_Generator(pknobs, GEN_MODE_FILE, type);
        Cache_Info_Generator(pknobs, GEN_MODE_FILE, type);
    } else {

        // Generate Tables
        Opcode_Generator(pknobs, GEN_MODE_FILE);
        Issue_Port_Generator(pknobs, GEN_MODE_FILE);
        Subset_Generator(pknobs, GEN_MODE_FILE);
        Properties_Generator(pknobs, GEN_MODE_FILE);
        Enums_Generator(pknobs, GEN_MODE_FILE);
        Lits_Generator(pknobs, GEN_MODE_FILE);
        Bundle_Generator(pknobs, GEN_MODE_FILE);
        Register_Generator(pknobs, GEN_MODE_FILE);
        Operands_Generator(pknobs, GEN_MODE_FILE);
        Itanium_Generator(pknobs, GEN_MODE_FILE);
        Bypass_Generator(pknobs, GEN_MODE_FILE);
        Proc_Generator(pknobs, GEN_MODE_FILE);
        Proc_Properties_Generator(pknobs, GEN_MODE_FILE);
        Cache_Info_Generator(pknobs, GEN_MODE_FILE);
        
    }
    //  release memory
    KAPI_Finalize(pknobs);
}

int main( int argc, char *argv[] )
{
    char src1[200],src2[200];
    int i;
 
    
    // Read arguments from command line 
    if ( argc < 2 ) {
        strcpy( src1,"v26-itanium-41-external.knb" );
        strcpy( src2,"v11-itanium-extra.knb");
    }
    else {
    	strcpy( src1, argv[1] );
    	strcpy( src2, argv[2] );
    }
    Generate_MM(src1, src2, ITM_TYPE);
 
    printf("Use Mckinley knobsfile to generate machine model!\n");
    strcpy( src1,"mckinley_knobsfile.knb" );
    strcpy( src2,"v12-itanium-extra.knb");
    Generate_MM(src1, src2, MCK_TYPE);


    printf("Leaving creat machine model\n"); 
}

