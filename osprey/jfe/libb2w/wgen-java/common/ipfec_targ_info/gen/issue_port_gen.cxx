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
//  Module : issue_port_gen.cxx
//  $Date  : $
//  $Author: marcel $
//  $Source: /proj/osprey/CVS/open64/osprey1.0/common/ipfec_targ_info/gen/issue_port_gen.cxx,v $
//
//  Description:
//  ============
//  Generate issue port function
//=============================================================================
/////////////////////////////////////////
//
//  Generate Issue port information for using in compiler runtime.
//  To answer question of which ports will be used by specified op.
//
//
#include <stdio.h>
#include "issue_port_gen.h"
#include "ekapi_ia64.h" //our access layer of MD and KAPI headfile
#include "assert.h"

static const char * const description[] = {"\
/* ====================================================================\n\
 * ====================================================================\n\
 *\n\
 * Description:\n\
 */", NULL};

void Issue_Port_Generator(void *pknobs, GEN_MODE mode, MACHINE_TYPE type)
{
    FILE *fpc, *fph, *fp_export;
    char fname[512] = "targ_issue_port";
    char bufname[512];
    char *buf;
    int i,j,k,count;
    int ipcount;
    int value=0;
    kapi_fu_t fu; //kapi type
   
    if (type == MCK_TYPE) 
        Init_Module_Files(mode, "targ_issue_port_mck", &fpc, &fph, &fp_export, 1);
    else
        Init_Module_Files(mode, "targ_issue_port", &fpc, &fph, &fp_export, 1);
    Emit_Header(fph, "targ_issue_port", description, 1);

    fprintf(fph, "#include \"topcode.h\" \n");
    fprintf(fph, "#define ISA_MAX_ISSUE_BUNDLES (%d) "
                 "// Machine width in bunldes\n",
                  KAPI_BundleIssueWidth(pknobs,0));
    fprintf(fpc, "#include \"stdio.h\" \n");
    fprintf(fpc, "#include \"%s.h\" \n", fname);

    fprintf(fph, "typedef INT ISSUE_PORT;\n");
    fprintf(fph, "extern ISSUE_PORT ");
    // dump enum to c file
    for (i=0 ; i< KAPI_EnumCardinality(pknobs, "ut_t"); i++)
    {
        buf  = KAPI_EnumName(pknobs, i, "ut_t");
        buf += strlen("ut") ; //Skip prefix "ut"
        for (j=0; j< KAPI_cportCount4ut( pknobs, 0, (kapi_ut_t)i ); j++)
        {
             
             fprintf(fpc, "ISSUE_PORT  ip_%s%d = %d;\n", buf, j, value);
             fprintf(fph, "ip_%s%d, ", buf, j);
             value++;
        }
    }
    fprintf(fpc, "ISSUE_PORT  ip_invalid = %d; \n ISSUE_PORT ip_number = %d;\n", value, value);
    fprintf(fph, "ip_invalid, ip_number;\n");
 
    fprintf(fpc, "char *issue_port_name[] = {\n");
    for (i=0 ; i< KAPI_EnumCardinality(pknobs, "ut_t"); i++)
    {
        buf  = KAPI_EnumName(pknobs, i, "ut_t");
        buf += strlen("ut") ; //Skip prefix "ut"
        for (j=0; j< KAPI_cportCount4ut( pknobs, 0, (kapi_ut_t)i ); j++)
        {
             fprintf(fpc, "  \"%s%d\",\n", buf, j);
             value++;
        }
    }
    fprintf(fpc, "  \"invalid\" \n};\n\n");

    fprintf(fph, "extern char *issue_port_name[];\n");
    fprintf(fph, "inline char *Issue_Port_Name( const ISSUE_PORT ip)\n"
              "{ return issue_port_name[ip];}\n\n");

    fprintf(fph, "extern ISSUE_PORT issue_port_seq[];\n\n");
    fprintf(fpc, "ISSUE_PORT issue_port_seq[]={\n");

    int m_idx = -1;
    int i_idx = -1;
    for (i=0 ; i< KAPI_EnumCardinality(pknobs, "ut_t"); i++)
    {
        buf  = KAPI_EnumName(pknobs, i, "ut_t");
        buf += strlen("ut") ; //Skip prefix "ut"
        
        // We prefer I unit to M unit in Itanium2 microscheduling 
        if (type == MCK_TYPE && *buf == 'M' && i_idx == -1) { 
            m_idx = i;
            continue;
        }
        for (j=0; j< KAPI_cportCount4ut( pknobs, 0, (kapi_ut_t)i ); j++)
        {
             int k = j;
             if (*buf=='B'){
                 k = KAPI_cportCount4ut( pknobs, 0, (kapi_ut_t)i ) - j - 1;
             }
             fprintf(fpc, "  ip_%s%d,\n", buf, k);
        }
        
        if (type == MCK_TYPE && *buf == 'I' ) { 
            i_idx = i;
            if ( m_idx != -1) {
                *buf = 'M';
                for (j=0; j< KAPI_cportCount4ut( pknobs, 0, (kapi_ut_t)m_idx ); j++) 
                     fprintf(fpc, "  ip_%s%d,\n", buf, j);
            }
        }
    }
    fprintf(fpc, "  ip_invalid\n};\n");

    fprintf(fph, "class PORT_SET{\n"
              "   INT body;\n\n"
	      "public:\n"
              "   PORT_SET() : body(0) {}\n"
              "   PORT_SET(INT b): body(b) {}\n"
              "   PORT_SET(const PORT_SET& p): body(p.body) {}\n\n"
              "   PORT_SET& operator=(PORT_SET p){ body=p.body; return *this; }\n\n"
              "   PORT_SET operator+(PORT_SET p) const\n"
              "       { return PORT_SET(body|p.body); }\n"
              "   PORT_SET operator&(PORT_SET p) const\n"
              "       { return PORT_SET(body&p.body); }\n"
              "   PORT_SET operator+(ISSUE_PORT p) const\n"
              "       { return PORT_SET(body|(1<<p)); }\n\n"
              "   PORT_SET operator-(PORT_SET p) const\n"
              "       { return PORT_SET(body&~(p.body)); }\n"
              "   PORT_SET operator-(ISSUE_PORT p) const\n"
              "       { return PORT_SET(body&~(1<<p));}\n\n"
              "   operator INT() const{return body; }"
              "   INT Body() const{ return body; }\n"
              "   BOOL In(ISSUE_PORT p) const { return ((1<<p)&body)!=0;}\n"
              "   BOOL Is_Subset_Of(PORT_SET p) const { return (~body)|(p.body);}\n"
              "   void Print(FILE *fp, char *str);\n"
              "   ISSUE_PORT First_IP();\n"
              "   ISSUE_PORT Last_IP();\n"
              "   INT Count();\n"
              "};\n\n" );

	// Print M, I, F, B PORT SET
    fprintf(fph, "extern PORT_SET ");
    fprintf(fpc, 
              "void PORT_SET::Print(FILE *fp, char *str) {\n"
              "    BOOL first=true;\n"
              "    for (INT i=0; i<ip_invalid; i++)\n"
              "        if (body & 1<<i) {\n"
              "            if (!first) fprintf(fp, str); /* print string to seperate*/\n"
              "            fprintf(fp, Issue_Port_Name((ISSUE_PORT)i));\n"
              "            first = false;\n"
              "        }\n"
              "}\n"
              "ISSUE_PORT PORT_SET::First_IP() {\n"
              "    for (INT i=0; i<ip_invalid; i++)\n"
              "        if (body & 1<<i) return (ISSUE_PORT)i;\n"
              "    return ip_invalid;\n"
              "}\n"
              "ISSUE_PORT PORT_SET::Last_IP() {\n"
              "    for (INT i=ip_invalid; i>=0; i--)\n"
              "        if (body & 1<<i) return (ISSUE_PORT)i;\n"
              "    return ip_invalid;\n"
              "}\n"
              "INT PORT_SET::Count() {\n"
              "    INT count = 0;\n"
              "    for (INT i=ip_invalid; i>=0; i--)\n"
              "        if (body & 1<<i) count++;\n"
              "    return count;\n"
              "}\n");
    j = 0; /*keeps shift number*/
    for (i=0 ; i< KAPI_EnumCardinality(pknobs, "ut_t"); i++)
    {
        buf  = KAPI_EnumName(pknobs, i, "ut_t");
        buf += strlen("ut") ; //Skip prefix "ut"
        fprintf(fph, "%s_PORTS", buf);
        int sum = KAPI_cportCount4ut( pknobs, 0, (kapi_ut_t)i );
        long ports_set = ((1 << sum) - 1) << j; 
        fprintf(fpc, "PORT_SET %s_PORTS(%ld);\n", buf, ports_set);
        j += sum;
        if (i != KAPI_EnumCardinality(pknobs, "ut_t")-1) {
		    fprintf(fph, ", ");
        }
        else {
		    fprintf(fph, ";\n");
        }
    }
 
    fprintf(fph, "extern const INT op_issue_port[];\n"
              "inline const PORT_SET TSI_Issue_Ports(TOP top)\n"
              "{\n"
              "    switch(top){\n"
              "    case TOP_mov_t_ar_r:\n"
              "        return (PORT_SET)( op_issue_port[TOP_mov_t_ar_r_i]\n"
              "                         | op_issue_port[TOP_mov_t_ar_r_m] );\n"
              "    case TOP_mov_t_ar_i:\n"
              "        return (PORT_SET)( op_issue_port[TOP_mov_t_ar_i_i]\n"
              "                         | op_issue_port[TOP_mov_t_ar_i_m] );\n"
              "    case TOP_mov_f_ar:\n"
              "        return (PORT_SET)( op_issue_port[TOP_mov_f_ar_i]\n"
              "                         | op_issue_port[TOP_mov_f_ar_m] );\n"
              "    case TOP_break:\n"
              "        return (PORT_SET)( op_issue_port[TOP_break_i]\n"
              "                         | op_issue_port[TOP_break_m]\n"
              "                         | op_issue_port[TOP_break_b]\n"
              "                         | op_issue_port[TOP_break_f] );\n"
              "    case TOP_chk_s:\n"
              "        return (PORT_SET)( op_issue_port[TOP_chk_s_i]\n"
              "                         | op_issue_port[TOP_chk_s_m] );\n"
              "    default:\n"
              "        return (PORT_SET)(op_issue_port[top]);\n"
              "    }\n"
              "}\n\n");

    count = EKAPI_OpCount(pknobs);
    fprintf(fpc, "\nconst INT op_issue_port[%d] = {\n", count);
    for(i=0; i<count; i++)
    {
        buf = EKAPI_Op2Fu(pknobs, i);
            
        if ( (strcmp(buf, "fuUNKNOWN") == 0)  ||
               (strcmp(buf, "fuDUMMY") ==0) )
        {
            fu = -2;
        }
        else
        {
            //If not hold this function class ,it will return -1
            fu  = KAPI_EnumIndex(pknobs, "fu_t", buf);
        }
        switch (fu)
        {
        case -1:   printf("UNKNOWN fu [%s] line %d\n", buf, i); break;
        case -2:   fprintf(fpc, "    %3d", 0); break;
        default:
            {
                fprintf(fpc, "    %3ld", (long)KAPI_cportMask4fu(pknobs, 0, fu));
                break;
            }
        }
        if (i != count-1)  {
            fprintf(fpc, ",  /* %-20s", EKAPI_OpName4id(pknobs, i));
            ipcount = 0;

            //  print the name of issue port to file as comment
            for (j=0 ; j< KAPI_EnumCardinality(pknobs, "ut_t"); j++)
            {
                buf  = KAPI_EnumName(pknobs, j, "ut_t");
                buf += strlen("ut") ; //Skip prefix "ut"
                for (k=0; k< KAPI_cportCount4ut( pknobs, 0, (kapi_ut_t)j );k++)
                {
                    value = 1ULL << ipcount;
                    ipcount++;
                    //  printf("%d, %d\n", value, KAPI_cportMask4fu(pknobs, 0, fu) );
                    if (value & KAPI_cportMask4fu(pknobs, 0, fu)) {
                        fprintf(fpc, " ip_%s%d ", buf, k );
                    }
                }
            }
            fprintf(fpc, "*/\n");
        }
    }

    fprintf(fpc, "\n};\n");
    // dump processor version which used by ptn_table
    if (type == MCK_TYPE)
        fprintf(fpc, "INT PROCESSOR_Version=2;\n");
    else
        fprintf(fpc, "INT PROCESSOR_Version=1;\n");
    fprintf(fph, "extern INT PROCESSOR_Version;\n");
    Emit_Tailer(fph, 1);
    Close_Module_Files(mode, &fpc, &fph, &fp_export);
}//end of Issue_Port_Generator


