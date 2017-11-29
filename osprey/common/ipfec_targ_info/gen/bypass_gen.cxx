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
// Module: bypass_gen.cxx
// $Date: 2005/10/21 19:00:00 $
// $Author: marcel $
// $Source: /proj/osprey/CVS/open64/osprey1.0/common/ipfec_targ_info/gen/bypass_gen.cxx,v $
//
// Description:
//   Generate the definition of bypass for getting correct latency between
//    two op and its operand;
//*********************************************************************

#include "bypass_gen.h"
#include <map>
#include <list>

//
typedef struct SUC_FU_BYPASS {    
    kapi_fu_t suc_fu;   // Successive function class in bypass info
    BOOL use_eunit;     // Whether consider execute unit type of preceding
                        // op or not? 1 is consider, 0 is not;
    int eun_id;         // If use_eunit is true , this field keeps eut type
    int use_opns;      // Indicate the successive function class should
                        // note which operations; 
                        // the first bit means hold address register or not;
                        // the second bit means predicate register;
    int bypass_val;     // bypass value;
};

typedef struct bypass_care_fu {
    kapi_fu_t pre_fu;
    SUC_FU_BYPASS *succ_bypass;
}BYPASS_CARE_FU;
static BOOL first_in_pre_case = true;
static BOOL first_in_suc_case = true;
static BOOL last_in_suc_case = false;

static SUC_FU_BYPASS cur_state;
static std::map <int, char*> oddfunc_pair;
static std::list < BYPASS_CARE_FU > care_fu_bypasslist;
static std::list < BYPASS_CARE_FU >::iterator cf_iter;

// Function:
void Print_OddLatency(void *pknobs, FILE *c_file);
void Print_All_Bypass(void *pknobs, FILE *h_file, FILE *c_file);



////////////////////////////////////////////////////////////////
//  Use EKAP_OddLatencyList to get list of odd latency;
//  And Parse them to creat many odd latency function;
//  Print into c_file;

inline char *Chop_fu(char *funame)
{ 
  return funame? (funame+strlen("fu")): NULL;
}
void Print_OddLatency(void *pknobs, FILE *c_file)
{
    int i, count;
    int listnum;
    int rcid, rid;
 
    count = KAPI_fuCount(pknobs);
    
    for(i=0; i<count; i++)
    {
        ODD_LATENCY *ol = EKAPI_OddLatencyList(pknobs, i, &listnum);
        if (ol != NULL) {
            if (listnum != 0) {/*have odd latency*/
                
                
                BOOL has_odd_func = 0;/*Indicate need oddlatency function or not*/
                for(int j=0; j<listnum; j++)
		{
                    Line2Dot(ol[j].opndname);
                    if (EKAPI_RegInfo4Name(pknobs, ol[j].opndname, &rcid, &rid))
                    {
		        has_odd_func = 1;
                    }
                    if (strcmp(ol[j].opndname, "addr") == 0) {
                    }
               
                }
               
                if (has_odd_func) {
                    char *func_name;
                    func_name = (char *)malloc(100*sizeof(char));
                    sprintf(func_name, "Odd_Latency_%s", 
                            Chop_fu(KAPI_fu2fuName(pknobs, i, 0))
                           );
                        
                    //////////////////////////////////////////
                    //  Save pair of fuid and function name;
                    oddfunc_pair[i] = func_name;

                    //  Print odd function body;
                    fprintf(c_file, "static void\n%s(TOP pred_code, TOP succ_code, INT src_reg, "
                                    "INT dst_reg, INT *latency)\n{\n", 
                            func_name
                           ); 
                    if(strstr(func_name, "Odd_Latency_TO")!=NULL) {
                        fprintf(c_file, "INT reg = dst_reg;");
                    }
                    else { 
                         fprintf(c_file, "INT reg = src_reg;");
                    } 
                    BOOL find =0;
                    int cur_latency = ol[0].oddlatency;
                    for(int j=0; j<listnum; j++)
		    {
                    
		        //printf("%s, %d\n" , ol[j].opndname, ol[j].oddlatency);                    
		        Line2Dot(ol[j].opndname);
                        if (EKAPI_RegInfo4Name(pknobs, ol[j].opndname, &rcid, &rid))
			{
                        
                           // Print switch case sentence for special latency;
			    if (!find) {
			        fprintf(c_file, "switch (reg) {\n");                            
			    }
			    if ((cur_latency == ol[j].oddlatency)) {
                                fprintf(c_file, "  case %d:\t// %s\n", rid, ol[j].opndname);
                                if (j == listnum-1) 
                                    fprintf(c_file, "    *latency = %d;\n     break;\n", cur_latency);
			    }
			    else {
			        fprintf(c_file, "    *latency = %d;\n     break;\n", cur_latency);
			        fprintf(c_file, "  case %d:\t// %s\n", rid, ol[j].opndname);
			        cur_latency = ol[j].oddlatency;
			        if (j == listnum-1) 
                                    fprintf(c_file, "    *latency = %d;\n     break;\n", cur_latency);
                            
			    }
                        
			    find = 1;                  
			}

                    }/* end of for */
                    if (find) 
		        fprintf(c_file, "}\n"); /*case end bracket*/
                    fprintf(c_file, "}\n\n"); // Print separetor end of function
                }/* has_odd_func is true*/
	    }/*listnum > 0*/
        }/* end of odd latency is not null*/
    }/* for fuid */
}

//  Print bypass preceding case head. Contains switch;
void Print_Bypass_Pre_Case(void *pknobs, FILE *c_file, int pre_i)
{
    char *name = KAPI_fu2fuName(pknobs, pre_i, 0);
    if (name == NULL) {
        Is_True(false, ("KNOB FILE::fu %d not exited.", pre_i));
        return;
    }
    if (first_in_pre_case) {
        fprintf(c_file, "  switch (pred_class) {\n");
        first_in_pre_case = 0;
    }
    
    fprintf(c_file, "\tcase SIC_%s:\n", Chop_fu(name));
}

//  Print bypass successing case head. Contains switch;
void Print_Bypass_Suc_Case(void *pknobs, FILE *c_file, int suc_i)
{
    char *name = KAPI_fu2fuName(pknobs, suc_i, 0);
    if (name == NULL) {
        Is_True(false, ("KNOB FILE::fu %d not exited.", suc_i));
        return;
    }
    if (first_in_suc_case) {
        fprintf(c_file, "\t\tswitch (succ_class) {\n");
        first_in_suc_case = 0;
    }
    
    fprintf(c_file, "\t\t  case SIC_%s:\n", Chop_fu(name));
    
}
void print_bypass_struct(void *pknobs, FILE *c_file,SUC_FU_BYPASS *bypass, BOOL fu_adjust=false)
{
  /* first unset valeu , so you must initial value*/
    if ((cur_state.bypass_val == -1))
	{   
        cur_state.bypass_val = bypass->bypass_val;
        cur_state.use_opns   = bypass->use_opns;
        cur_state.use_eunit  = bypass->use_eunit;
        cur_state.eun_id     = bypass->eun_id;
	}
    if ((cur_state.bypass_val != bypass->bypass_val)
       ||(bypass->use_opns != cur_state.use_opns) 
        ||(cur_state.use_eunit != bypass->use_eunit)
        ||(last_in_suc_case)) {
            
         if (cur_state.use_opns > 0) {
              
              if (cur_state.use_opns & 0x1)
                  fprintf(c_file, "\t\t\tif (%s", 
                         "TOP_Find_Operand_Use(succ_code, OU_base) == opnd");
              if (cur_state.use_opns & 0x2)
                  fprintf(c_file, "\t\t\tif (%s",
                         "TOP_Find_Operand_Use(succ_code, OU_predicate) == opnd");
         } 
         // determine execute unit need or not;
         if (cur_state.use_eunit == 1){
       	      if (cur_state.eun_id == kapi_utI) {
                    if (cur_state.use_opns > 0) 
	                fprintf(c_file, "\n\t\t\t    && ");
                    else 
	                fprintf(c_file, "\t\t\tif (");
                    if (!fu_adjust)
                        fprintf(c_file, "EXEC_PROPERTY_is_%s(pred_code) ", "I_Unit"); 
                    else 
                        fprintf(c_file, "!ports.Is_Subset_Of((PORT_SET)3) ");
              }
	      if (cur_state.eun_id == kapi_utM) {
                    if (cur_state.use_opns > 0) 
	                fprintf(c_file, "\n\t\t\t    && ");
                    else 
	                fprintf(c_file, "\t\t\tif (");
                    if (!fu_adjust)
                        fprintf(c_file, "EXEC_PROPERTY_is_%s(pred_code) ", "M_Unit");
                    else 
                        fprintf(c_file, "!ports.Is_Subset_Of((PORT_SET)12) ");
              }
         }
         if (cur_state.use_opns >0 || cur_state.use_eunit == 1)
             fprintf(c_file, ")\n");
 
         if (!fu_adjust)
             fprintf(c_file,"\t\t\t  *latency += %d;\n", cur_state.bypass_val);
         else  
             fprintf(c_file,"\t\t\t  *adjust = -%d;\n", cur_state.bypass_val);

         fprintf(c_file, "\t\t\tbreak;\n");
         if (!last_in_suc_case) {
             cur_state.bypass_val = bypass->bypass_val;
             cur_state.use_opns   = bypass->use_opns;
             cur_state.use_eunit  = bypass->use_eunit;
             cur_state.eun_id     = bypass->eun_id;          
             Print_Bypass_Suc_Case(pknobs, c_file, bypass->suc_fu);
         }
    }        
}
void Print_All_Bypass(void *pknobs, FILE *h_file, FILE *c_file)
{
    int pre_i;
    SUC_FU_BYPASS *suc_bypass;
    int count = KAPI_fuCount(pknobs);
    
    // Print bypass function :TARG_Adjust_Latency
    //  Head of this function
    fprintf(h_file, 
            "extern void TARG_Adjust_Latency(TOP pred_code, TOP succ_code,\n"
            "                  INT src_reg, INT dst_reg, INT opnd, INT *latency, \n"
            "                  BOOL pred_is_chk=false, BOOL succ_is_chk=false);\n");
 
    fprintf(c_file, 
            "/* =============================================================\n"
            " *\n"
            " * TARG_Adjust_Latency\n"
            " *\n"
            " * See interface description\n"
            " *\n"
            " * =============================================================\n"
            " */\n");
    fprintf(c_file, "void\n"
            "TARG_Adjust_Latency(TOP pred_code, TOP succ_code, INT src_reg, INT dst_reg,\n" 
            "                     INT opnd, INT *latency, BOOL pred_is_chk, BOOL succ_is_chk)\n"
            );
    fprintf(c_file, "{\n");
     fprintf(c_file, "  const SCHED_INFO_CLASS pred_class = TARG_Sched_Info_Class(pred_code, pred_is_chk);\n"
                    "  const SCHED_INFO_CLASS succ_class = TARG_Sched_Info_Class(succ_code, succ_is_chk);\n"
            );
 
            
    int pa_num=0;
    for (pre_i=0; pre_i<count; pre_i++)
    {
        int cluster = -1;
        int no_set = -1;
        first_in_suc_case = 1;
        last_in_suc_case = 0;
        cur_state.use_opns = -1; /*init cur_state*/
        cur_state.bypass_val = -1;
        cur_state.use_eunit = 0;
       
        
        if (oddfunc_pair[pre_i] != NULL) {                
             // Print first switch case into c_file
            // Update var [first_in_pre_case] and [first_in_suc_case];
            Print_Bypass_Pre_Case(pknobs, c_file, pre_i);
            
            // Print odd function name;
            fprintf(c_file, "\t\t\t// From-%s register latency is determined\n", 
                    oddfunc_pair[pre_i]);
            fprintf(c_file, "\t\t\t// by source register\n");
            fprintf(c_file, "\t\t\t%s(pred_code, succ_code, src_reg, dst_reg, latency);\n", 
                        oddfunc_pair[pre_i]);
        }
        else {
            Print_Bypass_Pre_Case(pknobs, c_file, pre_i);
        } 
            
        for (int suc_i=0; suc_i<count; suc_i++)        
        {
            papair_t *pa = KAPI_IntraClusterBypassList(
                                     pknobs, cluster, pre_i, 
                                     -1, (kapi_ut_t)-1, -1, 
                                     suc_i, -1, (kapi_ut_t)-1, -1, 
                                     &pa_num);
            suc_bypass = (SUC_FU_BYPASS *)malloc(pa_num * sizeof(SUC_FU_BYPASS));
            
            for(int pa_i=0; pa_i < pa_num; pa_i++)
            {
                // Check precede op's unit type is needed or not?
              
                suc_bypass[pa_i].use_opns = 0; /*init*/ 
                suc_bypass[pa_i].use_eunit = 0;
                suc_bypass[pa_i].eun_id = -1;

                if (pa[pa_i].utSrc>=0) {
                    suc_bypass[pa_i].use_eunit = 1;
                    suc_bypass[pa_i].eun_id = pa[pa_i].utSrc;
                     
                }

                char *opnd_name = KAPI_srcOppName(pknobs, suc_i, pa[pa_i].oppDest);
                if (opnd_name != NULL) {
                    if( strcmp(opnd_name, "addr")==0){
                        suc_bypass[pa_i].use_opns = 1;  

                    }
                    if( strcmp(opnd_name, "predicate")==0){
                        suc_bypass[pa_i].use_opns = 2;
                    }
                }
                suc_bypass[pa_i].bypass_val = pa[pa_i].iValue;
                suc_bypass[pa_i].suc_fu = suc_i;

                // record all latency should care for function unit;
                if (suc_bypass[pa_i].use_eunit == 1 &&
                    suc_bypass[pa_i].bypass_val >0 &&
                    pa_i==0 ) { // only get the first
                    BYPASS_CARE_FU cf_bypass;
                    cf_bypass.pre_fu = pre_i;
                    cf_bypass.succ_bypass = (SUC_FU_BYPASS *)malloc(sizeof(SUC_FU_BYPASS));

                    // copy succ_bypass[pa_i] to one item of list
                    cf_bypass.succ_bypass->suc_fu = suc_bypass[pa_i].suc_fu;
                    cf_bypass.succ_bypass->use_opns = suc_bypass[pa_i].use_opns;
                    cf_bypass.succ_bypass->use_eunit = suc_bypass[pa_i].use_eunit;
                    cf_bypass.succ_bypass->eun_id = suc_bypass[pa_i].eun_id;
                    cf_bypass.succ_bypass->bypass_val = suc_bypass[pa_i].bypass_val;
                    care_fu_bypasslist.push_back(cf_bypass);
                }
            }
            if (pa != NULL) {
             //   free(pa);
                pa = NULL;
            }
            
            last_in_suc_case = (suc_i == count-1) ? 1:0;
            // Print part;    
            
            // Print first switch case into c_file
            // Update var [first_in_pre_case] and [first_in_suc_case];
            if (pa_num > 0) {                
                
                /////////////////////////////////////////////////////
                // Print bypass except not equal to current bypass state
			    // if not equal, will  go to print_bypass_struct()
                // if noe equal but it is beginning , always print it;
                if ( ((cur_state.bypass_val == suc_bypass->bypass_val)
                      &&(cur_state.use_opns == suc_bypass->use_opns)
                      &&(cur_state.use_eunit == suc_bypass->use_eunit))
                    ||(cur_state.bypass_val == -1))
                    Print_Bypass_Suc_Case(pknobs, c_file, suc_i);
                print_bypass_struct(pknobs, c_file, suc_bypass);
            }
            
            if ((last_in_suc_case)&&(cur_state.bypass_val != -1)) {
                print_bypass_struct(pknobs, c_file, suc_bypass);               
            }
            
            free(suc_bypass);
        }/* iter of suc fuction class*/
        if (!first_in_suc_case) {
            fprintf(c_file, "\t\t}/*suc fu end*/\n");
        }
        fprintf(c_file, "\t\tbreak;\n");
        if (pre_i == count-1) {
	  fprintf(c_file, "\t}/*End of swtich of preceding fu*/\n\n");        
        }
        
    }/* all pred fu */

    // Some special case;

    fprintf(c_file,
           "\n  // TOP_alloc only cannot place one group with flushrs,loadrs,br.call,br1.call\n"
           "  // br.ia,br.ret,clrrrb,cover,rfi;\n"
//           "  if (pred_code==TOP_alloc && OP_def_use_stack_regs(succ_op)) {\n"
//           "       *latency = 0;\n"
//           "  }\n"
           "  if (pred_code  == TOP_alloc &&\n"
           "     (succ_code == TOP_flushrs ||\n"
           "     succ_code == TOP_br_cexit||\n"
           "     succ_code == TOP_br_ctop ||\n"
           "     succ_code == TOP_br_wexit ||\n"
           "     succ_code == TOP_br_wtop ||\n"
           "     succ_code == TOP_br_call ||\n"
           "     succ_code == TOP_br_ia ||\n"
           "     succ_code == TOP_br_ret ||\n"
           "     succ_code == TOP_clrrrb ||\n"
           "     succ_code == TOP_clrrrb_pr ||\n"
           "     succ_code == TOP_cover ||\n"
           "     succ_code == TOP_rfi ))\n"
           "       *latency = 1;\n");

    fprintf(c_file, "}/* End of Adjust Latency*/\n\n");

    // clean memory
    oddfunc_pair.clear ();

}/* end of Print_Bypass() */

void Print_FU_Class(void *pknobs, FILE *h_file, FILE *c_file)
{   
    int count = KAPI_fuCount(pknobs); 
    fprintf(h_file, "/*Function class of op*/\n");
    fprintf(h_file, "typedef enum {\n");
    for(int i=0; i<count; i++)
    {
        char *funame = KAPI_fu2fuName(pknobs, i, 0);
        fprintf(h_file, "  SIC_%s,\n",Chop_fu(funame));
    }
    fprintf(h_file, "  SIC_DUMMY,\n"); /* Two add SIC for requirment*/
    fprintf(h_file, "  SIC_UNKNOWN\n");
    fprintf(h_file, "} SCHED_INFO_CLASS;\n\n");

    // Print function which return function class for each op
    fprintf(h_file, "extern SCHED_INFO_CLASS TARG_Sched_Info_Class(TOP top, BOOL is_chk=false);\n");
    fprintf(c_file, "SCHED_INFO_CLASS TARG_Sched_Info_Class(TOP top, BOOL is_chk)\n{\n");
    
    fprintf(c_file, "  switch (top) {\n");
    int op_count = EKAPI_OpCount(pknobs);
    char *cur_funame = EKAPI_Op2Fu(pknobs, 0);
    for (int i=0; i<op_count; i++)
    {
        char *funame = EKAPI_Op2Fu(pknobs, i); 
        char *op_name = EKAPI_OpName4id(pknobs, i);        
        Dot2Line(op_name);
      
        if (strcmp(cur_funame, funame)==0) {               
	    fprintf(c_file, "  case TOP_%s:\n", op_name);
        }
        else {
	    BOOL special_ld_op = 0;
	    // Special process for LD op      
       	    if (strcmp(cur_funame,"fuLD")==0) {
	        fprintf(c_file, "    return is_chk ? SIC_%s : SIC_%s;\n",
			"CLD",
		        Chop_fu(cur_funame) );
                special_ld_op = 1;
	    }
            if (strcmp(cur_funame,"fuFLD")==0) {
	        fprintf(c_file, "    return is_chk ? SIC_%s : SIC_%s;\n",
			"FCLD",
		        Chop_fu(cur_funame) );
                special_ld_op = 1;
            }
            if (strcmp(cur_funame,"fuFLDP")==0)
	    {
	        fprintf(c_file, "    return is_chk ? SIC_%s : SIC_%s;\n",
			"FCLD",
		        Chop_fu(cur_funame) );
                special_ld_op = 1;
	    }

            if (!special_ld_op)
	        fprintf(c_file, "    return SIC_%s;\n", Chop_fu(cur_funame));
	    fprintf(c_file, "  case TOP_%s:\n", op_name);
            strcpy(cur_funame,funame);                     
        }
        free(funame);
    }
    fprintf(c_file, "    return SIC_%s;\n", Chop_fu(cur_funame));
    fprintf(c_file, "}\n\n");
    free(cur_funame);

    fprintf(c_file, 
            "  FmtAssert(FALSE, (\"no scheduling class for %s \", TOP_Name(top)));\n",
            "%s");
    fprintf(c_file,"  /*NOTREACHED*/\n");
    fprintf(c_file, "}\n");

}
void Print_Bypass_Care_FU(void *pknobs, FILE *h_file, FILE *c_file)
{
    int old_pre_fu=-1;
    int count=0;
    first_in_pre_case = 0;

    fprintf(h_file, "extern void "
            "TARG_Adjust_Latency_FU(TOP pred_code, TOP succ_code, struct PORT_SET ports, INT *adjust);\n"
            );
    fprintf(c_file, "void\n"
            "TARG_Adjust_Latency_FU(TOP pred_code, TOP *succ_code, struct PORT_SET ports, INT *adjust)\n"
            );
    fprintf(c_file, "{\n");
    fprintf(c_file, 
                    "  const SCHED_INFO_CLASS pred_class = TARG_Sched_Info_Class(pred_code);\n"
                    "  const SCHED_INFO_CLASS succ_class = TARG_Sched_Info_Class(succ_code);\n"
            );
    for (cf_iter = care_fu_bypasslist.begin(); cf_iter != care_fu_bypasslist.end(); ++cf_iter)
    {
        count++;
        if (old_pre_fu != cf_iter->pre_fu) {
            cur_state.use_opns = -1; /*init cur_state*/
            cur_state.bypass_val = -1;
            cur_state.use_eunit = 0;
            last_in_suc_case = 0;
            first_in_pre_case = 1;

            if (old_pre_fu >= 0) {
               fprintf(c_file, "\t\t}/*suc fu end*/\n");
            }
            Print_Bypass_Pre_Case(pknobs, c_file, cf_iter->pre_fu); 
            old_pre_fu = cf_iter->pre_fu;
        }else {
            last_in_suc_case = (count == (care_fu_bypasslist.size()-1))? 1 : 0; 
            first_in_pre_case = 0;
        }
        if ( ((cur_state.bypass_val == cf_iter->succ_bypass->bypass_val)
               &&(cur_state.use_opns == cf_iter->succ_bypass->use_opns)
               &&(cur_state.use_eunit == cf_iter->succ_bypass->use_eunit))
               ||(cur_state.bypass_val == -1)) 
               Print_Bypass_Suc_Case(pknobs, c_file, cf_iter->succ_bypass->suc_fu);
        if (!last_in_suc_case)
           print_bypass_struct(pknobs, c_file, cf_iter->succ_bypass, true);
         
    }
    if (care_fu_bypasslist.size() >0 ) {
        last_in_suc_case = 1;
        print_bypass_struct(pknobs, c_file, &cur_state, true);
        fprintf(c_file, "\t\t}/*suc fu end*/\n");
        fprintf(c_file, "\t}/*pre fu end*/\n");
    }
    // release memory
    for (cf_iter = care_fu_bypasslist.begin(); cf_iter != care_fu_bypasslist.end(); ++cf_iter)
    {
        free(cf_iter->succ_bypass);
    }
    fprintf(c_file, "}\n/*function end*/"); 
}

void Bypass_Generator(void *pknobs, GEN_MODE mode, MACHINE_TYPE type)
{
    FILE *c_file, *h_file, *export_file;
    int index;
    char * description[]= {
"/***************************************************************************\n"
"* Description:\n"
"*	void TARG_Adjust_Latency(TOP pred_code, TOP succ_code,\n"
"*                               INT src_reg, INT dst_reg, INT *latency,\n"
"*                               BOOL pred_is_chk, BOOL succ_is_chk)\n"
"*	  Makes any target-specific latency adjustments that may be\n"
"*	  required between <pred_code> and <succ_code>.\n"
"*/\n", NULL};
 
    first_in_pre_case = true;
    first_in_suc_case = true;
    last_in_suc_case = false;

    if (type == MCK_TYPE) 
        Init_Module_Files(mode, "targ_bypass_mck", &c_file, &h_file, &export_file, 1);
    else 
        Init_Module_Files(mode, "targ_bypass", &c_file, &h_file, &export_file, 1);
    Emit_Header(h_file, "targ_bypass", description, 1);

    fprintf(c_file, "#include \"errors.h\"\n");
    fprintf(c_file, "#include \"targ_isa_lits.h\"\n");
    fprintf(c_file, "#include \"targ_isa_bundle.h\"\n");
    fprintf(c_file, "#include \"targ_isa_registers.h\"\n");
    fprintf(c_file, "#include \"targ_issue_port.h\"\n");
    fprintf(c_file, "#include \"targ_isa_operands.h\"\n");

    // for SIC definition diff, But we can use the new targ bypass
    // head file which generated for mckinley 
        fprintf(c_file, "#include \"targ_bypass_mck.h\"\n\n\n"); 
      
    Print_FU_Class(pknobs, h_file, c_file);
    Print_OddLatency(pknobs, c_file);
    Print_All_Bypass(pknobs, h_file, c_file);
    
     
    Emit_Tailer(h_file, 1);
    Close_Module_Files(mode, &c_file, &h_file, &export_file);
    
}


