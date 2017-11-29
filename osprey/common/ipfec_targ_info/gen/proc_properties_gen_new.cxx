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
// Module: proc_properties_gen_new.cxx
// $Date: 2005/10/21 19:00:00 $
// $Author: marcel $
// $Source: /proj/osprey/CVS/open64/osprey1.0/common/ipfec_targ_info/gen/proc_properties_gen_new.cxx,v $
//
// Description:
//   Generate the definition of proc and it's properties;
//
//*********************************************************************

#include "proc_properties_gen_new.h"

static const char * const description[]= {"\
/* ====================================================================\n\
 * ====================================================================\n\
 *\n\
 * Description:\n\
 *\n\
 *   A description of the properties (attributes) for the processors\n\
 *   in the PROC. The description exports the following:\n\
 *\n\
 *   BOOL PROC_xxx(void)\n\
 *       Return true/false if PROCESSOR_Value has/does-not-have the\n\
 *       property 'xxx'.\n\
 *\n\
 * ====================================================================\n\
 * ====================================================================\n\
 */", NULL};

void Proc_Properties_Generator(void *pknobs, GEN_MODE mode)
{
	FILE *c_file, *h_file, *export_file;
	int op_index;

	Init_Module_Files(mode, "targ_proc_properties", &c_file, &h_file, &export_file);
	Emit_Header(h_file, "targ_proc_properties", description);
	fprintf(c_file, "#include \"targ_proc_properties.h\"\n\n");

    fprintf(h_file, "#include \"targ_proc.h\"\n\n");
	fprintf(c_file, "const mUINT8 PROC_PROPERTIES_flags[] = {\n");
	char * buf = EKAPI_ProcessName(pknobs);
	fprintf(c_file, "  0x%02x, /* %s: */\n", 0, buf);
	fprintf(c_file, "  0x00  /* UNDEFINED */\n};\n");	
	fprintf(h_file, 
            "extern const mUINT8 PROC_PROPERTIES_flags[];\n\n\n"
            "#define PROC_has_branch_delay_slot() (0)\n"
            "#define PROC_has_same_cycle_branch_shadow() (0)\n"
            "#define PROC_is_out_of_order() (0)\n"
            "#define PROC_is_superscalar() (1)\n"
            "#define PROC_has_bundles() (1)\n"
            "#define PROC_has_delayed_exception() (1)\n"
            "#define PROC_has_fast_recip() (0)\n");
	fprintf(export_file, "PROC_PROPERTIES_flags\n");

	Emit_Tailer(h_file);
	Close_Module_Files(mode, &c_file, &h_file, &export_file);
}

