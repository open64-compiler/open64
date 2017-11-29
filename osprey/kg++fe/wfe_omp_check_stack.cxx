/*
 * Copyright 2005, 2006 PathScale, Inc.  All Rights Reserved.
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of version 2 of the GNU General Public License as
 * published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it would be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 *
 * Further, this software is distributed without any warranty that it is
 * free of the rightful claim of any third person regarding infringement
 * or the like.  Any license provided herein, whether implied or
 * otherwise, applies only to this software file.  Patent licenses, if
 * any, provided herein do not apply to combinations of this program with
 * other software, or any other product whatsoever.
 *
 * You should have received a copy of the GNU General Public License along
 * with this program; if not, write the Free Software Foundation, Inc., 59
 * Temple Place - Suite 330, Boston MA 02111-1307, USA.
 */

#include "wn.h"
#include "wn_util.h"
#include "wfe_omp_check_stack.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

extern BOOL Trace_Omp;

namespace {  // Bug 13884: Keep "dirname" out of global namespace
  char dirname[80];
}

static char* WFE_omp_name(WFE_CHECK_KIND kind)
{
   switch(kind)
   {
     case wfe_omp_parallel:
       sprintf(dirname,"OMP PARALLEL directive");
       break;	
     case wfe_omp_for:
       sprintf(dirname,"OMP FOR directive");
       break;	 	
     case wfe_omp_single:
       sprintf(dirname,"OMP SINGLE directive");
       break;					
     case wfe_omp_sections:
       sprintf(dirname,"OMP SECTIONS directive");
       break;	
     case wfe_omp_parallel_sections:
       sprintf(dirname,"OMP PARALLEL SECTIONS directive" );
       break;	
     case wfe_omp_parallel_for:
       sprintf(dirname,"OMP PARALLEL FOR direcitve");
       break;
     case wfe_omp_critical:
       sprintf(dirname,"OMP CRITICAL directive");
       break;					
     default: sprintf(dirname,"OTHER DIRECTIVES ");
   }
   return dirname;
}

// init a check stack
void WFE_CS_Init()
{    
  omp_check_size  =  OMP_CHECK_STACK_SIZE;
  
  omp_check_stack =  (CHECK_STMT *) malloc (sizeof (CHECK_STMT) *
                                           omp_check_size );
  omp_check_sp    =  omp_check_stack - 1;

  omp_check_last =  omp_check_stack + omp_check_size - 1;

  omp_check_num = 0;  
}

//push a omp stmt kind into check stack
void extern WFE_CS_push( WFE_CHECK_KIND kind, int lnum,int fnum)
{
  int new_stack_size;

  if (omp_check_sp == omp_check_last)
  {
    new_stack_size = ENLARGE(omp_check_size);
    omp_check_stack = 
      (CHECK_STMT *) realloc (omp_check_stack, new_stack_size * sizeof(CHECK_STMT));
    omp_check_sp=omp_check_stack + omp_check_size - 1;
    omp_check_size=new_stack_size;
    omp_check_last= omp_check_stack + omp_check_size -1;
  }
  omp_check_sp++;

  bzero (omp_check_sp, sizeof(CHECK_STMT));

  omp_check_sp->kind=kind;
  omp_check_sp->linenum=lnum;
  omp_check_sp->filenum=fnum;
  
  omp_check_num++;
   
  if (Trace_Omp)
    printf("Pushing %s ok! Stack now: %d element\n",
            WFE_omp_name (omp_check_sp->kind), omp_check_num);
}

//pop a omp stmt from the check stack
extern WFE_CHECK_KIND WFE_CS_pop(WFE_CHECK_KIND kind)
{
  FmtAssert (omp_check_sp >= omp_check_stack,
             ("no more entries on stack in function WFE_cs_Pop"));

  FmtAssert (omp_check_sp->kind == kind,
             ("mismatch in statements:expect %d,got %d\n", kind, omp_check_sp->kind ) );

  if (Trace_Omp)
    printf("Poping %s ok! Stack now: %d element\n",
           WFE_omp_name (omp_check_sp->kind), omp_check_num-1);
  omp_check_sp--;
  omp_check_num--;
  
  return kind;
}

//get the top CHECK_STMT in the check stack
extern CHECK_STMT*  WFE_CS_top(void)
{
  
  FmtAssert (omp_check_sp >= omp_check_stack,
             ("no more entries on stack in function WFE_CS_Top"));

  return omp_check_sp;
}

//get the enclosing CHECK_STMT of the current top of the stack
extern CHECK_STMT *WFE_CS_enclose(void)
{
  FmtAssert (omp_check_sp-1 >= omp_check_stack,
             ("no entry on stack in function WFE_CS_enclose"));
  return omp_check_sp-1;
}

//set top CHECK_STMT's prag pointer 
extern void WFE_Set_Prag(WN* omp_prag)
{
  FmtAssert (omp_check_sp >= omp_check_stack,
             ("no entry on stack in function WFE_Set_Prag"));
  omp_check_sp->wn_prag=omp_prag;
}
//set name for critical directive

extern void WFE_Set_Nameflag(char* name)
{
  FmtAssert (omp_check_sp >= omp_check_stack,
             ("no entry on stack in function WFE_Set_Intflag"));
  omp_check_sp->name=name;

}

//set clause flag for top omp stmt
extern void WFE_Set_Cflag(WFE_CLAUSE_KIND flag)
{
  FmtAssert (omp_check_sp >= omp_check_stack,
             ("no entriy on stack in function WFE_Set_Cflag"));

  omp_check_sp->cflag |=flag;
}

//set line number and file number for the top omp stmt
extern void WFE_Set_LFnum(CHECK_STMT* cs,int lnum,int fnum)
{

  FmtAssert (omp_check_sp >= omp_check_stack,
             ("no entriy on stack in function WFE_Set_LFnum"));

  cs->linenum=lnum;
  cs->filenum=fnum;
}

// set region node
extern void WFE_Set_Region (WN * region)
{
  FmtAssert (omp_check_sp >= omp_check_stack,
             ("no entry on stack in function WFE_Set_Region"));
  omp_check_sp->region = region;
}

//check the clause flag of given CHECK_STMT with the given flag
extern bool WFE_Check_Cflag(CHECK_STMT* cs, WFE_CLAUSE_KIND flag)
{
  if(flag & cs->cflag)
    return true; 
  else return false;
}

//search stmt of given kind from inside to outside, return the depth
extern int WFE_CS_Find(WFE_CHECK_KIND kind)  
{
  int count=0;
  CHECK_STMT *temp;

  for(temp=omp_check_sp;temp>=omp_check_stack;temp--)
  {
    count++;
    if (temp->kind == kind)
      return (omp_check_num - count);
  }
  return -1;
}

//search stmt of given kind from inside to outside, return the find
extern CHECK_STMT* WFE_CS_Find_Rtn(WFE_CHECK_KIND kind)
{    
  CHECK_STMT* temp;
  int i;

  for(temp=omp_check_sp;temp>=omp_check_stack;temp--)
  {
    if(temp->kind == kind)
      return temp;
  }
  return NULL;   
}

//search stmt of given kind of nesting depth 
extern int WFE_CS_Find_depth(WFE_CHECK_KIND kind,int depth)
{
  int count=0;
  int find=0, rval=-1;
  CHECK_STMT* temp;

  for(temp=omp_check_sp;temp>=omp_check_stack;temp--)
  {
    count++;
    if(temp->kind == kind)
    {
      rval=omp_check_num - count;
      find++;
    }
    if (find == depth)
      return rval;	
  }
  return -1;;  
}

//search the stmt of the given kind with the given flag, return depth
extern int WFE_CS_Find_Cflag(WFE_CHECK_KIND kind,WFE_CLAUSE_KIND flag)
{	
	int rval=-1,count=0;
	CHECK_STMT* temp;
	
	for(temp=omp_check_sp;temp>=omp_check_stack;temp--)
	{
	    count++;
    	if(temp->kind==kind&&(temp->cflag&flag))
    	{	 
    		  rval=omp_check_num-count;
    		  break;
    	}
     }
	
	return rval;
}

//find stmt of the given kind with the given name, especially for critical direcitive
extern int WFE_CS_Find_fgname(WFE_CHECK_KIND kind ,char* name)
{
	//printf("check same name: kind is %d,with the name %s\n",kind,name);
	int rval=-1, count=0;
	CHECK_STMT* temp;

	for(temp=omp_check_sp;temp>=omp_check_stack;temp--)
	{
	    count++;
    	if(temp->kind == kind && temp->name && !strcmp(temp->name, name))
    	{	 
    		  rval=omp_check_num-count;
    		  break;
    	}
     }
	
	return rval;
	
}

//judge if the top stmt is of the given kind
extern bool WFE_is_top(WFE_CHECK_KIND kind)
{
	if(omp_check_sp->kind==kind)
		return true;
	else return false;
}

//judge if the top_next stmt is of the given type
extern bool WFE_is_top_next(WFE_CHECK_KIND kind)
{
	if((omp_check_sp-1)->kind==kind)
		return true;	
	else return false;
}

//judge if arg1 and arg2 are bind the arg3
extern bool WFE_bind_to_same(WFE_CHECK_KIND sub1,WFE_CHECK_KIND sub2,WFE_CHECK_KIND bind)
{
	int i;
	bool rval=false;
	int find1=-1,find2=-1,find3=-1,tmp=-1;
	
	find1=WFE_CS_Find(sub1);
	find2=WFE_CS_Find(sub2);
	find3=WFE_CS_Find(bind);
	
	if(find1<0||find2<0||find3<0)  // one or more of the three kind are not in stack
	   rval=false;
	else if(find1==find2)   //sub1 and sub2 are same kind
	{  
		if(WFE_CS_Find_depth(sub1,2)>-1)
		{
		   tmp=WFE_CS_Find_depth(sub1,2);
		   if(find3<find2&&find3<tmp) 
		     rval=true;		  
		}
		else rval=false;     //only one sub1 in stack
	}
	
	else if(find3<find1&&find3<find2)  //three different kind;
	{
	   rval=true;	
	}
	else rval=false;
	
	return rval;
	
}

extern void WFE_add_pragma_to_enclosing_regions (WN_PRAGMA_ID id, ST * st)
{
  WN_VECTOR regions;

  for (CHECK_STMT * iter = omp_check_sp;
       iter >= omp_check_stack; iter--)
  {
    WN * reg = iter->region;
    if (reg && WN_operator (reg) == OPR_REGION &&
        WN_region_kind(reg) == REGION_KIND_MP)
     regions.insert (regions.end(), reg);
  }

  // bug 5149: set pragma_compiler_generated flag
  Add_Pragma_To_MP_Regions (&regions, id, st, 0, 0, TRUE);
}

