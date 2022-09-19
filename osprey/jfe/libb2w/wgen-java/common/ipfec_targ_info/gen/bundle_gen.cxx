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
// Module: bundle_gen.cxx
// $Date: 2005/10/21 19:00:00 $
// $Author: marcel $
// $Source: /proj/osprey/CVS/open64/osprey1.0/common/ipfec_targ_info/gen/bundle_gen.cxx,v $
//
// Description:
//   Generate the definition of opcode and opcode name.
//
//*********************************************************************

#include "bundle_gen.h"
#include "ekapi_util.h"

static const char* const description[]= {"\
#include \"topcode.h\"\n\
/* ====================================================================\n\
 * ====================================================================\n\
 *" ,
" * Description:\n\
 *\n\
 *   A description of the bundling properties. The interface is\n\
 *   divided into two pieces: scheduling, and packing. The scheduling\n\
 *   interface exports the following:\n\
 *\n\
 *   const INT ISA_MAX_SLOTS\n\
 *       An integer constant that indicates the maximum number of\n\
 *       slots in a bundle.\n\
 *" ,
" *   const INT ISA_TAG_SHIFT\n\
 *       Maximum number of bits required to encode all the execution\n\
 *       property types.\n\
 *\n\
 *   typedef mUINTxx ISA_EXEC_UNIT_PROPERTY\n\
 *       A single-bit mask of representing an execution unit.\n\
 *\n\
 *       The names have the form ISA_EXEC_PROPERTY_xxx\n\
 *       where 'xxx' is replaced with the EXEC_UNIT_PROPERTY name.\n\
 *" ,
" *   typedef (enum) ISA_EXEC_UNIT\n\
 *       An enumeration of the execution units.\n\
 *\n\
 *       The names have the form ISA_EXEC_xxx\n\
 *       where 'xxx' is replaced with the EXEC_UNIT_PROPERTY name.\n\
 *\n\
 *       The values of ISA_EXEC_UNIT and ISA_EXEC_UNIT_PROPERTY are\n\
 *       related in that the bit-mask value of an ISA_EXEC_UNIT_PROPERTY\n\
 *       is equal to 2**ISA_EXEC_UNIT.\n\
 *" ,
" *   const INT ISA_EXEC_MAX\n\
 *       The highest value ISA_EXEC_UNIT value.\n\
 *\n\
 *   BOOL ISA_EXEC_PROPERTY_is_xxx(TOP t) \n\
 *       Returns TRUE if EXEC_PROPERTY_is_xxx matches <t>'s property.\n\
 *\n\
 *   ISA_EXEC_UNIT_PROPERTY ISA_EXEC_Unit_Prop(TOP topcode)\n\
 *       Returns exec_unit_property for the instruction specified\n\
 *       by <topcode>.\n\
 *" ,
" *   ISA_BUNDLE_INFO ISA_EXEC_Bundle_Info(INT index)\n\
 *       Return isa_bundle_info specified by <index>. \n\
 *\n\
 *   ISA_EXEC_UNIT_PROPERTY ISA_EXEC_Slot_Prop(INT bundle, INT slot_index)\n\
 *       Return exec_unit_property for the slot position <slot_index>\n\
 *       in <bundle>.\n\
 *\n\
 *   UINT64 ISA_EXEC_Slot_Mask(INT bundle)\n\
 *       Return slot_mask for <bundle>.\n\
 *" ,
" *   BOOL ISA_EXEC_Stop(INT bundle, INT slot_index)\n\
 *       Return stop bit for the slot position <slot_index> in <bundle>.\n\
 *\n\
 *   ISA_EXEC_UNIT ISA_EXEC_Unit(INT bundle, INT slot_index)\n\
 *       Return the execution unit slot position <slot_index> in <bundle>.\n\
 *\n\
 *   UINT32 ISA_EXEC_Stop_Mask(INT bundle)\n\
 *       Return stop_mask for <bundle>.\n\
 *" ,
" *   const char *ISA_EXEC_Name(INT bundle)\n\
 *       Return the name for <bundle>.\n\
 *\n\
 *   const char *ISA_EXEC_AsmName(INT bundle)\n\
 *       Return the assembly language name for <bundle>.\n\
 *\n\
 *   BOOL ISA_EXEC_Stop_Before(INT bundle)\n\
 *       Return stop bit (splite isssue) before <bundle>.\n\
 *" ,
" *   BOOL ISA_EXEC_Stop_After(INT bundle)\n\
 *       Return stop bit (splite isssue) after <bundle>.\n\
 *\n\
 * ====================================================================\n\
 *\n\
 *   The packing interface exports the following:\n\
 *" ,
" *   typedef ISA_BUNDLE\n\
 *       A type large enough to hold a bundle. This type will always\n\
 *       be a struct containing an array of either 32-, or 64-bit\n\
 *       unsigned integers.\n\
 *\n\
 *   typedef (enum) ISA_BUNDLE_PACK_COMP\n\
 *       An enumeration of the bundle components to be packed.\n\
 *\n\
 *   const INT ISA_BUNDLE_PACK_COMP_MAX\n\
 *       The maximum number of components to be packed for a bundle.\n\
 *" ,
" *   typedef (struct) ISA_BUNDLE_PACK_INFO\n\
 *       Describes how a the components of a bundle are packed.\n\
 *       The contents are private.\n\
 *\n\
 *   const ISA_BUNDLE_PACK_INFO *ISA_BUNDLE_Pack_Info(void)\n\
 *       Returns a pointer to the first packing component.\n\
 *       Increment the returned pointer to access any additional packing\n\
 *       components for the bundle. A component of ISA_PACK_COMP_end\n\
 *       marks the end.\n\
 *\n\
 *   INT ISA_BUNDLE_PACK_INFO_Comp(const ISA_BUNDLE_PACK_INFO *info)\n\
 *       Identifies the bundle component to be packed.\n\
 *" , 
" *   INT ISA_BUNDLE_PACK_INFO_Index(const ISA_BUNDLE_PACK_INFO *info)\n\
 *       The index of the bundle word containing the component.\n\
 *\n\
 *       ISA_BUNDLE_PACK_INFO_Index is meaningless for ISA_BUNDLE_PACK_COMP_end.\n\
 *\n\
 *   INT ISA_BUNDLE_PACK_INFO_CompPos(const ISA_BUNDLE_PACK_INFO *info)\n\
 *       The offset, in bits, to the start of the component in the\n\
 *       component value.\n\
 *" ,
" *       ISA_BUNDLE_PACK_INFO_CompPos is meaningless for ISA_BUNDLE_PACK_COMP_end.\n\
 *\n\
 *   INT ISA_BUNDLE_PACK_INFO_BundlePos(const ISA_BUNDLE_PACK_INFO *info)\n\
 *       The offset, in bits, to the start of the component in the\n\
 *       bundle word.\n\
 *\n\
 *       ISA_BUNDLE_PACK_INFO_BundlePos is meaningless for ISA_BUNDLE_PACK_COMP_end.\n\
 *" ,
" *   UINT64 ISA_BUNDLE_PACK_INFO_Mask(const ISA_BUNDLE_PACK_INFO *info)\n\
 *       A bit mask that is as wide as the bundle component being\n\
 *       packed. The mask is shifted to match the field in the\n\
 *       bundle word.\n\
 *\n\
 *       ISA_BUNDLE_PACK_INFO_Mask is meaningless for ISA_BUNDLE_PACK_COMP_end.\n\
 *" ,
" *   INT ISA_BUNDLE_Pack_Info_Index(ISA_BUNDLE_PACK_COMP comp)\n\
 *       Index into bundle packing info array (see ISA_BUNDLE_Pack_Info)\n\
 *       to the start of the info for the component <comp>. If there\n\
 *       is no packing info for <comp>, the index is for the 'end'\n\
 *       component.\n\
 *\n\
 * ====================================================================\n\
 * ====================================================================\n\
 */",
NULL };


static const char bundle_info_struct[]="\
typedef struct {\n\
  const char *name;\n\
  const char *asm_name;\n\
  int slot_count;\n\
  ISA_EXEC_UNIT_PROPERTY slot[%d];\n\
  mBOOL stop[%d];\n\
  mUINT8 unit[%d];\n\
  mUINT8 pack_code;\n\
  mUINT8 stop_mask;\n\
  mUINT64 slot_mask;\n\
  mBOOL stop_bf;\n\
  mBOOL stop_af;\n\
} ISA_BUNDLE_INFO;\n\n";

static const char* const bundle_info_query[]={ 
"\n\
inline ISA_EXEC_UNIT_PROPERTY ISA_EXEC_Unit_Prop(TOP topcode)\n\
{\n\
  return ISA_EXEC_unit_prop[(INT)topcode];\n\
}\n\
" ,
"inline ISA_BUNDLE_INFO ISA_EXEC_Bundle_Info(INT index)\n\
{\n\
  extern const ISA_BUNDLE_INFO ISA_BUNDLE_info[];\n\
  return ISA_BUNDLE_info[index];\n\
}\n\
" ,
"inline ISA_EXEC_UNIT_PROPERTY ISA_EXEC_Slot_Prop(INT bundle, INT slot_index)\n\
{\n\
  extern const ISA_BUNDLE_INFO ISA_BUNDLE_info[];\n\
  const ISA_BUNDLE_INFO *info = ISA_BUNDLE_info + bundle;\n\
  return info->slot[slot_index];\n\
}\n\
" ,
"inline UINT64 ISA_EXEC_Slot_Mask(INT bundle)\n\
{\n\
  extern const ISA_BUNDLE_INFO ISA_BUNDLE_info[];\n\
  const ISA_BUNDLE_INFO *info = ISA_BUNDLE_info + bundle;\n\
  return info->slot_mask;\n\
}\n\
" ,
"inline BOOL ISA_EXEC_Stop(INT bundle, INT slot_index)\n\
{\n\
  extern const ISA_BUNDLE_INFO ISA_BUNDLE_info[];\n\
  const ISA_BUNDLE_INFO *info = ISA_BUNDLE_info + bundle;\n\
  return info->stop[slot_index];\n\
}\n\
" ,
"inline ISA_EXEC_UNIT ISA_EXEC_Unit(INT bundle, INT slot_index)\n\
{\n\
  extern const ISA_BUNDLE_INFO ISA_BUNDLE_info[];\n\
  const ISA_BUNDLE_INFO *info = ISA_BUNDLE_info + bundle;\n\
  return (ISA_EXEC_UNIT)info->unit[slot_index];\n\
}\n\
" ,
"inline UINT32 ISA_EXEC_Stop_Mask(INT bundle)\n\
{\n\
  extern const ISA_BUNDLE_INFO ISA_BUNDLE_info[];\n\
  const ISA_BUNDLE_INFO *info = ISA_BUNDLE_info + bundle;\n\
  return info->stop_mask;\n\
}\n\
" ,
"inline const char * ISA_EXEC_Name(INT bundle)\n\
{\n\
  extern const ISA_BUNDLE_INFO ISA_BUNDLE_info[];\n\
  const ISA_BUNDLE_INFO *info = ISA_BUNDLE_info + bundle;\n\
  return info->name;\n\
}\n\
" ,
"inline const char * ISA_EXEC_AsmName(INT bundle)\n\
{\n\
  extern const ISA_BUNDLE_INFO ISA_BUNDLE_info[];\n\
  const ISA_BUNDLE_INFO *info = ISA_BUNDLE_info + bundle;\n\
  return info->asm_name;\n\
}\n\
" ,
"// Add two function for special slpit issue\n\
inline BOOL ISA_EXEC_Stop_Before(INT bundle)\n\
{\n\
  extern const ISA_BUNDLE_INFO ISA_BUNDLE_info[];\n\
  const ISA_BUNDLE_INFO *info = ISA_BUNDLE_info + bundle;\n\
  return info->stop_bf;\n\
}\n\
" ,
"inline BOOL ISA_EXEC_Stop_After(INT bundle)\n\
{\n\
  extern const ISA_BUNDLE_INFO ISA_BUNDLE_info[];\n\
  const ISA_BUNDLE_INFO *info = ISA_BUNDLE_info + bundle;\n\
  return info->stop_af;\n\
}\n\
", NULL };

static const char bundle_pack_name[]="\
typedef enum {\n\
  ISA_BUNDLE_PACK_COMP_end       = 0,  /* End of list marker */\n\
  ISA_BUNDLE_PACK_COMP_stop      = 1,  /* Stop bit */\n\
  ISA_BUNDLE_PACK_COMP_template  = 2,  /* Template */\n\
  ISA_BUNDLE_PACK_COMP_slot      = 3,  /* SLOT+n => slot n */\n\
  ISA_BUNDLE_PACK_COMP_MAX       = %d   /* Last component */\n\
} ISA_BUNDLE_PACK_COMP;\n\
\n";

static const char bundle_pack_info[]=
"typedef struct {\n\
  mUINT8 comp;\n\
  mUINT8 index;\n\
  mUINT8 comp_pos;\n\
  mUINT8 bundle_pos;\n\
  UINT64 mask;\n\
} ISA_BUNDLE_PACK_INFO;\n\
\n\
inline const ISA_BUNDLE_PACK_INFO *ISA_BUNDLE_Pack_Info(void)\n\
{\n\
  extern const ISA_BUNDLE_PACK_INFO ISA_BUNDLE_pack_info[];\n\
  return ISA_BUNDLE_pack_info;\n\
}\n\
\n\
inline INT ISA_BUNDLE_PACK_INFO_Comp(const ISA_BUNDLE_PACK_INFO *info)\n\
{\n\
  return info->comp;\n\
}\n\
\n\
inline INT ISA_BUNDLE_PACK_INFO_Index(const ISA_BUNDLE_PACK_INFO *info)\n\
{\n\
  return info->index;\n\
}\n\
\n\
inline INT ISA_BUNDLE_PACK_INFO_CompPos(const ISA_BUNDLE_PACK_INFO *info)\n\
{\n\
  return info->comp_pos;\n\
}\n\
\n\
inline INT ISA_BUNDLE_PACK_INFO_BundlePos(const ISA_BUNDLE_PACK_INFO *info)\n\
{\n\
  return info->bundle_pos;\n\
}\n\
\n\
inline UINT64 ISA_BUNDLE_PACK_INFO_Mask(const ISA_BUNDLE_PACK_INFO *info)\n\
{\n\
  return info->mask;\n\
}\n\
\n\
inline INT ISA_BUNDLE_Pack_Info_Index(ISA_BUNDLE_PACK_COMP comp)\n\
{\n\
  extern const mUINT8 ISA_BUNDLE_pack_info_index[6];\n\
  return ISA_BUNDLE_pack_info_index[(INT)comp];\n\
}\n\
\n";

void Bundle_Generator(void *pknobs, GEN_MODE mode)
{
  FILE *c_file, *h_file, *export_file;
  int index;

  Init_Module_Files(mode, "targ_isa_bundle", &c_file, &h_file, &export_file);
  Emit_Header(h_file, "targ_isa_bundle", description);
  fprintf(c_file, "#include \"targ_isa_bundle.h\"\n\n\n");

  fprintf(h_file, "\n#define ISA_MAX_SLOTS (%d)\n", EKAPI_GetMaxSlot(pknobs));
  fprintf(h_file, "#define ISA_TAG_SHIFT (12)\n"); //Hard code 12 bit per slot
  fprintf(h_file, "\ntypedef mUINT8 ISA_EXEC_UNIT_PROPERTY;\n\n");

  for (index=0; index<EKAPI_ExecUnitCount(pknobs); index++){
    fprintf(h_file, "#define ISA_EXEC_PROPERTY_%-15s (%#x)\n",
            EKAPI_ExecUnitName(pknobs, index), (unsigned int)(1ULL << index));
  }

  fprintf(h_file, "\ntypedef enum {\n");
  for (index=0; index<EKAPI_ExecUnitCount(pknobs); index++){
    fprintf(h_file, "  ISA_EXEC_%-15s = %d,\n",
            EKAPI_ExecUnitName(pknobs, index), index);
  }
  fprintf(h_file, "  ISA_EXEC_MAX             = %d\n} ISA_EXEC_UNIT;\n\n",
          index-1);

  // Emit declaration and definition of bundle info
  fprintf(export_file, "ISA_BUNDLE_info\n");
  fprintf(h_file, bundle_info_struct, EKAPI_GetMaxSlot(pknobs),
          EKAPI_GetMaxSlot(pknobs), EKAPI_GetMaxSlot(pknobs));
  fprintf(c_file, "const ISA_BUNDLE_INFO ISA_BUNDLE_info[] = {\n");
  for (index=0; index<EKAPI_TemplateCount(pknobs); index++){
    int i;
    fprintf(c_file, " {\n    ");
    char * name = EKAPI_TemplateName(pknobs,index);
    fprintf(c_file, "\"%s\",    ", name);
    for (i=0; i<(10-strlen(name)); i++){
      fprintf(c_file, " ");
    }
    if (KAPI_isReserved_bid(pknobs,index)){
      fprintf(c_file, "\".%#x\",", index);
    }
    else{
      fprintf(c_file, "\"%s\",", EKAPI_TemplateAsmName(pknobs,index));
    }
    fprintf(c_file, "     %d,\n", EKAPI_GetMaxSlot(pknobs));

    int * slots = (int*)malloc(EKAPI_GetMaxSlot(pknobs)*sizeof(int));
    FmtAssert(slots, ("Memory Allocation Failure!\n"));
    EKAPI_BundleType4bid(pknobs, index, slots);
    fprintf(c_file, "    {");
    int slot_mask = 0;
    for (i=0; i<EKAPI_GetMaxSlot(pknobs); i++){
      fprintf(c_file, " %2d /* %7s */,", 1 <<slots[i],
              EKAPI_ExecUnitName(pknobs, slots[i]));
      slot_mask = slot_mask <<12;
      slot_mask |= 1 << slots[i];
    }
    fprintf(c_file, " },\n    {");

    int stop_mask = 0;
    for (i=0; i<EKAPI_GetMaxSlot(pknobs); i++){
      stop_mask = stop_mask << 1;
      if ((i+1)==KAPI_SbitPlacement_bid(pknobs,index)){
        fprintf(c_file, "  TRUE,");
        stop_mask |= 1;
      }
      else{
        fprintf(c_file, " FALSE,");
      }
    }
    fprintf(c_file, " },\n    {");

    for (i=0; i<EKAPI_GetMaxSlot(pknobs); i++){
      char *unit_name = EKAPI_ExecUnitName(pknobs, slots[i]);
      if (strcmp(unit_name, "B2_Unit")==0){
        fprintf(c_file, " ISA_EXEC_B_Unit,");
      }
      else{
        fprintf(c_file, " ISA_EXEC_%s,", unit_name );
      }
    }
    fprintf(c_file, " },\n    ");

    fprintf(c_file, "%2d, 0x%x, 0x%09x,", index, stop_mask, slot_mask);
    if (EKAPI_Split_Before_Bundle(pknobs, index)) 
    	fprintf(c_file, " TRUE,");
    else 
    	fprintf(c_file, " FALSE,");
    if (EKAPI_Split_After_Bundle(pknobs, index)) 
    	fprintf(c_file, " TRUE");
    else 
    	fprintf(c_file, " FALSE");
    fprintf(c_file, "\n  },\n");
    free(slots);;
  }
  fprintf(c_file, "  {\n\
    \"template_MAX\", \"\", -1,\n\
    { -1 /* ??????? */, -1 /* ??????? */, -1 /* ??????? */,},\n\
    { FALSE, FALSE, FALSE,},\n\
    -1, 0x0, 0x000000000, FALSE, FALSE\n  }\n};\n\n");
  // End emit init of bundle info



  fprintf(h_file, "#define ISA_MAX_BUNDLES %d\n\n",
          EKAPI_TemplateCount(pknobs));


  // Begin Emit declaration and init of ISA_EXEC_unit_prop
  fprintf(export_file, "ISA_EXEC_unit_prop\n");
  fprintf(h_file,
          "extern const ISA_EXEC_UNIT_PROPERTY ISA_EXEC_unit_prop[];\n\n");
  fprintf(c_file,
          "const ISA_EXEC_UNIT_PROPERTY ISA_EXEC_unit_prop[%d] = {\n",
          EKAPI_OpCount(pknobs));
  for (index=0; index<EKAPI_OpCount(pknobs); index++){
    bv32_t mask = EKAPI_EunMask4op(pknobs, index);
    fprintf(c_file, "%5d,  /* (null): ", mask);
    for (int i=0; i<EKAPI_ExecUnitCount(pknobs); i++){
      if (mask & (1<<i)){
        fprintf(c_file," %s", EKAPI_ExecUnitName(pknobs, i));
      }
    }
    fprintf(c_file, " */\n");
  }
  fprintf(c_file, "};\n\n");
  // End Emit declaration and init of ISA_EXEC_unit_prop

  for (index=0; index<EKAPI_ExecUnitCount(pknobs); index++){
    fprintf(h_file, "#define EXEC_PROPERTY_is_%s(t)\t \
(ISA_EXEC_unit_prop[(INT)t] & ISA_EXEC_PROPERTY_%s)\n",
            EKAPI_ExecUnitName(pknobs, index),
            EKAPI_ExecUnitName(pknobs, index));
  }
  for (int i=0; bundle_info_query[i] != NULL; i++)
      fprintf(h_file, "%s\n", bundle_info_query[i]);

  // Calculate the space needed to emulate a bundle
  int b_width = EKAPI_GetBundleWidth(pknobs);
  int number = b_width / 64;
  if (b_width%64){
    number++;
  }
  fprintf(h_file,
          "typedef struct {\n  mUINT64 word[%d];\n} ISA_BUNDLE;\n\n", number);

  // Begin emit declaration and init of bundle pack info
  fprintf(export_file, "ISA_BUNDLE_pack_info\nISA_BUNDLE_pack_info_index\n");

  fprintf(h_file, bundle_pack_name, EKAPI_BundleCompCount(pknobs));
  fprintf(h_file, bundle_pack_info);
  fprintf(c_file, "const ISA_BUNDLE_PACK_INFO ISA_BUNDLE_pack_info[] = {\n");
  int start_bit=0;
  int start_pos=0;
  int width=0;
  int pos = 0;
  int *positions = (int*)malloc(EKAPI_BundleCompCount(pknobs)*sizeof(int));
  FmtAssert(positions, ("Memory Allocation Failure!\n"));
  memset( positions, 0, EKAPI_BundleCompCount(pknobs)*sizeof(int));

  for (index=0; index<EKAPI_BundleCompCount(pknobs); index++, pos++){
    char *bundle_comp_name = EKAPI_BundleCompName(pknobs, index);
    if (positions[index]==0){
      positions[index]=pos;
    }

    // here, slot_0 slot_1 should be convert to slot+0, slot+1..
    char *p = strstr(bundle_comp_name, "slot_");
    if (p){
      p[4]='+';
    }
    fprintf(c_file, "  { ISA_BUNDLE_PACK_%-13s , %2d, %2d, %2d,",
            bundle_comp_name, start_pos, width, start_bit);
    width = EKAPI_BundleCompWidth(pknobs,index) - width;
    UINT64 mask = 0;
    for (int i=0; i<width; i++){
      mask = mask << 1ULL;
      mask |= 1ULL;
    }
    mask = mask << start_bit;
    fprintf(c_file, " %#016llxLL },",
            mask );

    char *comment = strdup(bundle_comp_name);
    char *cp = comment+5; // skip COMP_
    if (strncmp(cp, "slot", 4)==0){
      cp[4]=cp[5];
      cp[5]='\0';
    }
    cp = StrUpper(cp);
    fprintf(c_file, "  /* %s */\n", cp);
    free(comment);

    if (start_bit+width > 64){
      // We need a new 64 bit int here
      width = 64-start_bit;
      start_pos++;
      start_bit=0;
      index--;
    }
    else{
      start_bit += width;
      width=0;
    }
    free(bundle_comp_name);
  }
  fprintf(c_file, "  { ISA_BUNDLE_PACK_COMP_end      , -1, -1, -1,\
                 -1 },  /* END */\n};\n\n");
  fprintf(c_file, "const mUINT8 ISA_BUNDLE_pack_info_index[%d] = {\n",
          pos);
  fprintf(c_file, "   %d, /* ISA_BUNDLE_PACK_COMP_end */\n", pos);
  for (index=0; index<EKAPI_BundleCompCount(pknobs); index++){
    char *comp_name = EKAPI_BundleCompName(pknobs, index);
    // here, slot_0 slot_1 should be convert to slot+0, slot+1..
    char *p = strstr(comp_name, "slot_");
    if (p){
      p[4]='+';
    }
    fprintf(c_file, "   %d, /* ISA_BUNDLE_PACK_%s */\n",
            positions[index], comp_name);
  }
  free(positions);
  fprintf(c_file, "};\n");
  // End emit declaration and init of bundle pack info

  Emit_Tailer(h_file);
  Close_Module_Files(mode, &c_file, &h_file, &export_file);
}




