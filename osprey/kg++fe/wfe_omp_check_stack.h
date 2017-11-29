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

#ifndef _GCCFE_WFE_OMP_CHECK_STACK_H_
#define _GCCFE_WFE_OMP_CHECK_STACK_H_

typedef enum {
   wfe_unknown,
   wfe_cscf,
   wfe_other_ompscope,
   wfe_omp_parallel,
   wfe_omp_for,
   wfe_omp_single,
   wfe_omp_sections,
   wfe_omp_parallel_sections,
   wfe_omp_parallel_for,
   wfe_omp_section,
   wfe_omp_master,
   wfe_omp_critical,
   wfe_omp_ordered,
   wfe_omp_barrier,
   wfe_omp_flush,
   wfe_omp_atomic,
   wfe_omp_threadprivate
}  WFE_CHECK_KIND;
typedef enum{
   clause_none          =0x0 ,
   clause_default        =0x1 ,
   clause_firstprivate    =0x2 ,
   clause_if             =0x4 ,     
   clause_lastprivate    =0x8 ,
   clause_copyin         =0x10 ,
   clause_copyprivate    =0x20,
   clause_ordered        =0x40,
   clause_private        =0x80,
   clause_reduction      =0x100,
   clause_schedule      =0x200,
   clause_shared        =0x400   

}WFE_CLAUSE_KIND;

typedef struct check_stmt
{
   WFE_CHECK_KIND kind;     
   char* name;        // for critical directive
   int cflag;
   int linenum;         
   int filenum;
   WN *wn_prag;
   WN *region;        // for a stack of regions    
}  CHECK_STMT;

static CHECK_STMT *omp_check_stack; //check stack     
static CHECK_STMT *omp_check_sp;   
static CHECK_STMT *omp_check_last;  //point to the last one of the check stack
static int omp_check_size;      //size of the check stack now
static int omp_check_num;      //number of stmt in check stack

#define ENLARGE(x) (x + (x >> 1))
#define OMP_CHECK_STACK_SIZE 32

extern  void WFE_CS_Init();
extern void WFE_CS_push( WFE_CHECK_KIND kind,int lnum, int fnum);
extern void SEtd(SRCPOS srcpos);
extern CHECK_STMT *WFE_CS_top(void);
extern CHECK_STMT *WFE_CS_enclose(void);
extern WFE_CHECK_KIND WFE_CS_pop(WFE_CHECK_KIND kind);
extern void WFE_Set_Prag(WN *omp_prag); 
extern void WFE_Set_Nameflag(char* name);
extern void WFE_Set_Cflag(WFE_CLAUSE_KIND flag);
extern void WFE_Set_LFnum(CHECK_STMT *cs,int lnum,int fnum);
extern bool WFE_Check_Cflag(CHECK_STMT* cs, WFE_CLAUSE_KIND flag);
extern void WFE_Set_Region (WN *);

//search from the inside level to outside level, return inmost find level
extern int WFE_CS_Find(WFE_CHECK_KIND kind); 
extern CHECK_STMT* WFE_CS_Find_Rtn(WFE_CHECK_KIND kind);
extern int WFE_CS_Find_Cflag(WFE_CHECK_KIND kind,WFE_CLAUSE_KIND flag);
extern int WFE_CS_Find_fgname(WFE_CHECK_KIND kind,char* name); 
//utilities:
//search in the name

extern bool WFE_is_top(WFE_CHECK_KIND kind);
extern bool WFE_is_top_next(WFE_CHECK_KIND kind);
//test if sub1 and sub2 bind to the same bindkind:
extern bool WFE_bind_to_same(WFE_CHECK_KIND sub1,WFE_CHECK_KIND sub2,WFE_CHECK_KIND bind);

#endif
