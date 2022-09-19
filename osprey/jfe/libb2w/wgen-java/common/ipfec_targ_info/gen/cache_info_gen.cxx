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
//  Module : cache_info_gen.cxx
//  $Date  : $
//  $Author: marcel $
//  $Source: /proj/osprey/CVS/open64/osprey1.0/common/ipfec_targ_info/gen/cache_info_gen.cxx,v $
//
//  Description:
//  ============
//  Generate cache information
//=============================================================================
/////////////////////////////////////////
//
//  Generate Cache information for using in compiler runtime.
//  To answer question of cache size, cache latency
//
//
#include <stdio.h>
#include <string.h>
#include "ekapi_ia64.h" //our access layer of MD and KAPI headfile
#include "assert.h"
#include "cache_info_gen.h"


static const char * const description[] = {"\
/* ====================================================================\n\
 * ====================================================================\n\
 *\n\
 * Description:\n\
 */", NULL};

void Cache_Info_Generator(void *pknobs, GEN_MODE mode, MACHINE_TYPE type)
{
    FILE *fpc, *fph, *fp_export;
    char fname[512] = "targ_cache_info";
    int i,j,k,count;
    kapi_cache_t *cache_t=NULL;
    
    
    if (type == MCK_TYPE) 
        Init_Module_Files(mode, "targ_cache_info_mck", &fpc, &fph, &fp_export, 1);
    else
        Init_Module_Files(mode, "targ_cache_info", &fpc, &fph, &fp_export, 1);
    Emit_Header(fph, "targ_cache_info", description, 1);

    fprintf(fpc, "#include \"%s.h\" \n\n", fname);
    
    fprintf(fph, "extern INT ");
    char **cache_names = (char **)malloc(EKAPI_CacheNameCount(pknobs) * sizeof(char*));
    for (i=0; i< EKAPI_CacheNameCount(pknobs); i++) {
       char *buf; 
       buf = EKAPI_CacheName(pknobs,i);
       fprintf(fph,"CACHE_%s, ", buf); 
       fprintf(fpc, "INT CACHE_%s = %d;\n", buf, i);
       cache_names[i]=buf;
    }
    fprintf(fpc, "INT CACHE_MAX = %d;\n", i);
    fprintf(fph, " CACHE_MAX;\n");
    
    // dump struct to h file
    fprintf(fph, "typedef struct{\n");
    fprintf(fph, "   INT lines;\n");
    fprintf(fph, "   INT linesize;\n");
    fprintf(fph, "   INT size;\n");
    fprintf(fph, "   INT ways;\n");
    fprintf(fph, "   INT ports;\n");
    fprintf(fph, "   INT readcycle;\n");
    fprintf(fph, "} CACHE_INFO;\n");

    // Dump function to h file
    fprintf(fph, "extern const CACHE_INFO cache_info[];\n\n");
    fprintf(fph, "inline INT Cache_Line_Size(INT idx)\n{\n"
                 "  return cache_info[idx].linesize;\n}\n");
    
    fprintf(fph, "inline INT Cache_Size(INT idx)\n{\n"
                 "  return cache_info[idx].size;\n}\n");
    
    fprintf(fph, "inline INT Cache_Lines(INT idx)\n{\n"
                 "  return cache_info[idx].lines;\n}\n");
    
    fprintf(fph, "inline INT Cache_Read_Cycle(INT idx)\n{\n"
                 "  return cache_info[idx].readcycle;\n}\n");

    fprintf(fph, "inline INT Cache_Ways(INT idx)\n{\n"
                 "  return cache_info[idx].ways;\n}\n");
    
    fprintf(fph, "inline INT Cache_Ports(INT idx)\n{\n"
                 "  return cache_info[idx].ports;\n}\n");
    
    // dump INT to c file;
    // dump structure to c file
    count = i; j=0;
    fprintf(fpc, "\nconst CACHE_INFO cache_info[%d] = {\n",count);
    for (i=0; i< count; i++)
    {
        if (strstr(cache_names[i], "I")) {
            cache_t = KAPI_CacheHierarcy(pknobs, i, KAPI_CACHE_TYPE_INSTRUCTION);
            j++;
        }else if(strstr(cache_names[i], "D")) {
            cache_t = KAPI_CacheHierarcy(pknobs, i-j, KAPI_CACHE_TYPE_DATA);
        }else{
            cache_t = KAPI_CacheHierarcy(pknobs, i-j, KAPI_CACHE_TYPE_DATA);//use it to call
        }
       
        if (cache_t) {
            // dump info of a structure
            fprintf(fpc, "    { %d, %d, %d, %d, %d, %d},",
                cache_t->nLines,
                cache_t->nBytesLine,
                cache_t->nBytesLine * cache_t->nLines,
                cache_t->nWays,
                cache_t->nCachePorts,
                cache_t->nCyclesRead);
        } else {
            fprintf(fpc, "{ 0, 0, 0, 0, 0 , 0}");
        }    
        
        fprintf(fpc, "/* %s */\n", cache_names[i]);
    } 
    free(cache_names);
    
    fprintf(fpc,"};\n");
    Emit_Tailer(fph, 1);
    Close_Module_Files(mode, &fpc, &fph, &fp_export);
}//end of Issue_Port_Generator


