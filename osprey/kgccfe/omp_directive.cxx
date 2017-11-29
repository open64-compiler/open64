/*
 * Copyright 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
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

#include "defs.h"
#include "glob.h"
#include "config.h"
#include "wn.h"
#include "wn_util.h"

#include "gnu_config.h"
#include "system.h"

#include "srcpos.h"
#include "tree.h"

#include "wfe_expr.h"
#include "wfe_misc.h"
#include "omp_types.h"
#include "omp_directive.h"
#include "wfe_omp_directives.h"
#include "wfe_omp_check_stack.h"

#include <stdio.h>
#include "errors.h"
#include "const.h"
 
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

/* Concatenate two chains of nodes (chained through TREE_CHAIN)
   by modifying the last node in chain 1 to point to chain 2.
   This is the Lisp primitive `nconc'.  */

tree
chainon (tree op1, tree op2)
{

  if (op1)
    {
      register tree t1;
      
      for (t1 = op1; TREE_CHAIN (t1); t1 = TREE_CHAIN (t1));
      
      TREE_CHAIN (t1) = op2;
      return op1;
    }
  else return op2;
}


// the following two functions only for debugging

void
print_tree (FILE *file, tree node)
{
  print_node_brief (file, "", node, 0);

  fprintf (file, "\n");
}


void
print_node_brief (FILE *file, const char *prefix, tree node, int indent)
{
  char class1;

  if (node == 0)
    return;

  class1 = TREE_CODE_CLASS (TREE_CODE (node));

  /* Always print the slot this node is in, and its code, address and
     name if any.  */
  if (indent > 0)
    fprintf (file, " ");
  fprintf (file, "%s <%s ", prefix, tree_code_name[(int) TREE_CODE (node)]);
  fprintf (file, HOST_PTR_PRINTF, (char *) node);

  if (class1 == 'd')
    {
      if (DECL_NAME (node))
	fprintf (file, " %s", IDENTIFIER_POINTER (DECL_NAME (node)));
    }
  else if (class1 == 't')
    {
      if (TYPE_NAME (node))
	{
	  if (TREE_CODE (TYPE_NAME (node)) == IDENTIFIER_NODE)
	    fprintf (file, " %s", IDENTIFIER_POINTER (TYPE_NAME (node)));
	  else if (TREE_CODE (TYPE_NAME (node)) == TYPE_DECL
		   && DECL_NAME (TYPE_NAME (node)))
	    fprintf (file, " %s",
		     IDENTIFIER_POINTER (DECL_NAME (TYPE_NAME (node))));
	}
    }
  if (TREE_CODE (node) == IDENTIFIER_NODE)
    fprintf (file, " %s", IDENTIFIER_POINTER (node));

  fprintf (file, ">");

  if (TREE_CODE (node) == TREE_LIST)
  	{
	  print_node_brief (file, "purpose", TREE_PURPOSE (node), indent + 4);
	  print_node_brief (file, "value", TREE_VALUE (node), indent + 4);
	  print_node_brief (file, "chain", TREE_CHAIN (node), indent + 4);
  	}
}

/*
 * return a previously created ST associated with a var-decl.
 */

inline ST *
Get_Pre_ST (tree name)
{
   if (name!=NULL)
      return WN_st( WFE_Expand_Expr(name));
   else
   	{
   	  SRCPOS srcpos = Get_Srcpos();
      WFE_CS_push(wfe_omp_master,SRCPOS_linenum(srcpos), SRCPOS_filenum(srcpos));

      Fail_FmtAssertion("Undeclared variable name at line %d file %d \n",
      	             SRCPOS_linenum(srcpos), SRCPOS_filenum(srcpos));
   	  return NULL;
   	}
} 

extern "C"{


/////////// prepare clause  ///////

void prepare_com_clause ( tree  clause_tree, ST_list * * clause_st )
{
     register tree t = NULL;
     ST *st = NULL;
     ST_list *stlist = NULL;

     for (t = clause_tree; t; t = TREE_CHAIN (t))
       {      
         st = Get_Pre_ST (TREE_VALUE(t));
       	 stlist = (struct ST_list *) malloc(sizeof(struct ST_list));
         stlist->st = st ;
         stlist->next = *clause_st;
         * clause_st = stlist;
       }

}

struct reduction_list 
{
  struct reduction node;
  struct reduction_list *next;
};

void prepare_reduction_clause 
	( struct reduction_list *reduction_clause_list, WN_list * * reduction_clause_wn )
{
     register tree t = NULL;
     ST *st = NULL;
     WN *wn = NULL;
     WN_list *wnlist = NULL;

     while (reduction_clause_list)
       {   
         OPERATOR op_code;
         switch(reduction_clause_list->node.reduction_op) {
             case REDUCTION_OPR_BAND:  //'&'
                op_code = OPR_BAND;
                break;
             case REDUCTION_OPR_BIOR:  //"|"
                op_code = OPR_BIOR;
                break;
             case REDUCTION_OPR_BXOR:  //'^'
                op_code = OPR_BXOR;
                break;
             case REDUCTION_OPR_ADD:   //'+'
                op_code = OPR_ADD;
                break;
             case REDUCTION_OPR_MPY:   //'*'
                op_code = OPR_MPY;
                break;
             case REDUCTION_OPR_SUB:   //'-'
                op_code = OPR_SUB;
                break;
             case REDUCTION_OPR_CAND:  //ANDAND
                op_code = OPR_CAND;
                break;
             case REDUCTION_OPR_CIOR:  //OROR
                op_code = OPR_CIOR;
                break;
           }
         
           for (t=reduction_clause_list->node.var_list; t; t = TREE_CHAIN (t))
              {
                st = Get_Pre_ST (TREE_VALUE(t));
                wn = WN_CreatePragma(WN_PRAGMA_REDUCTION, st, 0, op_code);
                WN_set_pragma_omp(wn);
       	        wnlist = (struct WN_list *) malloc(sizeof(struct WN_list));
                wnlist->wn = wn;
                wnlist->next = *reduction_clause_wn;
                *reduction_clause_wn = wnlist;

              }
          
           reduction_clause_list= reduction_clause_list->next;
          
         }

}


///////// parallel directive ////////

void
check_parallel_directive( struct parallel_clause_list * clause_list )
{
     struct parallel_clause_list *cl = NULL;
     int count_if = 0, count_num_threads = 0, count_default = 0;
     
	 for (cl = clause_list; cl != NULL; cl = cl->next)
       {
          if ( cl->type == p_if ) count_if++;
          if ( cl->type == p_num_threads ) count_num_threads++;
          if ( cl->type == p_default ) count_default++;
       }
	 
     if ( count_if > 1 || count_num_threads > 1 || count_default > 1)
       {
          if ( count_if > 1)
          	{
          	   printf ("Too many IF clausees.\n");
          	}
          if ( count_num_threads > 1)
          	{
          	   printf ("Too many NUM_THREADS clausees.\n");
          	}
          if ( count_default > 1)
          	{
          	   printf ("Too many DEFAULT clausees.\n");
          	}
          
          SRCPOS srcpos = Get_Srcpos();
          Fail_FmtAssertion ("Invalid syntax in the #PRAGMA OMP PARALLEL directive at line: %d, file number: %d.!\n", 
                      SRCPOS_linenum(srcpos), SRCPOS_filenum(srcpos));
       }
}

void
expand_start_parallel (struct parallel_clause_list * clause_list)
 {
     struct Parallel_clause_wn_type * parallel_clause_wn;
     
     tree  if_clause_tree = NULL, num_threads_clause_tree = NULL;
     tree  private_clause_tree = NULL, firstprivate_clause_tree = NULL;
     tree  shared_clause_tree = NULL, copyin_clause_tree = NULL;
     struct reduction_list  *reduction_clause_list = NULL;
     enum default_type default_clause_value = no_default;

     WN   *if_clause_wn = NULL, *num_threads_clause_wn =NULL ;

     struct parallel_clause_list *cl = NULL;
     struct reduction_list *rl = NULL;
     
     ST_list *private_clause_st = NULL;
     ST_list *firstprivate_clause_st = NULL;
     ST_list *shared_clause_st = NULL;
     ST_list *copyin_clause_st = NULL;
     WN_list *reduction_clause_wn = NULL;

     check_parallel_directive(clause_list); 

     for (cl = clause_list; cl != NULL; cl = cl->next)
       {
          if ( cl->type == p_if ) {
              if_clause_tree = cl->node.expr_no_commas;
              break;
          };
       };


     for (cl = clause_list; cl != NULL; cl = cl->next)
       {
          if ( cl->type == p_num_threads ) {
              num_threads_clause_tree = cl->node.expr_no_commas;
              break;
          };
      };


     for (cl = clause_list; cl != NULL; cl = cl->next)
       {
          if ( cl->type == p_private ) {
              private_clause_tree = chainon (private_clause_tree, cl->node.var_list);
          };
       };


     for (cl = clause_list; cl != NULL; cl = cl->next)
       {
          if ( cl->type == p_firstprivate ) {
             firstprivate_clause_tree = 
             chainon (firstprivate_clause_tree, cl->node.var_list);
          };
       };


     for (cl = clause_list; cl != NULL; cl = cl->next)
       {
          if ( cl->type == p_shared ) {
             shared_clause_tree = chainon (shared_clause_tree, cl->node.var_list);
          };
       };


     for (cl = clause_list; cl != NULL; cl = cl->next)
       {
          if ( cl->type == p_copyin ) {
             copyin_clause_tree = chainon (copyin_clause_tree, cl->node.var_list);
          };
       };


     for (cl = clause_list; cl != NULL; cl = cl->next)
       {
          if ( cl->type == p_reduction ) {
          	 rl = (struct reduction_list *) malloc(sizeof(struct reduction_list));
             rl->node = cl->node.reduction_node;
             rl->next = reduction_clause_list;
             reduction_clause_list = rl;
          };
       };


     for (cl = clause_list; cl != NULL; cl = cl->next)
       {
          if ( cl->type == p_default) {
             default_clause_value = cl->node.defaulttype;
             break;
          };
       };

     if (if_clause_tree) 
     {
     	 if_clause_wn = WFE_Expand_Expr (if_clause_tree);
	 TYPE_ID type = WN_rtype (if_clause_wn);
	 WN * val = (MTYPE_is_integral (type)) ? WN_Intconst (type, 0) :
	                                         WN_Floatconst (type, 0);
	 if_clause_wn = WN_NE (WN_rtype (if_clause_wn),
	                       val, if_clause_wn);
     }
     
     if (num_threads_clause_tree) 
    	 num_threads_clause_wn = WFE_Expand_Expr (num_threads_clause_tree);

     prepare_com_clause ( private_clause_tree, &private_clause_st );
     
     prepare_com_clause ( firstprivate_clause_tree, &firstprivate_clause_st );

     prepare_com_clause ( shared_clause_tree, &shared_clause_st );

     prepare_com_clause ( copyin_clause_tree, &copyin_clause_st );

     prepare_reduction_clause ( reduction_clause_list, &reduction_clause_wn );    

     parallel_clause_wn = (struct Parallel_clause_wn_type *) malloc(sizeof(Parallel_clause_wn_type)); 

     parallel_clause_wn-> if_clause = if_clause_wn;
     parallel_clause_wn-> num_threads_clause = num_threads_clause_wn;
     parallel_clause_wn-> private_clause = private_clause_st;
     parallel_clause_wn-> firstprivate_clause = firstprivate_clause_st;
     parallel_clause_wn-> shared_clause = shared_clause_st;
     parallel_clause_wn-> copyin_clause = copyin_clause_st;
     parallel_clause_wn-> reduction_clause = reduction_clause_wn;
     parallel_clause_wn-> default_clause = default_clause_value;

     WFE_expand_start_parallel (parallel_clause_wn);

     free(parallel_clause_wn);

}

void
expand_end_parallel ( )
{
     WFE_expand_end_parallel ();
}


///////// for directive ////////

void
check_for_directive(struct for_clause_list * clause_list )
{
     struct for_clause_list *cl = NULL;
     int count_schedule = 0, count_ordered = 0, count_nowait = 0;
     
	 for (cl = clause_list; cl != NULL; cl = cl->next)
       {
          if ( cl->type == f_schedule_1 || cl->type == f_schedule_2 ) count_schedule++;
          if ( cl->type == f_ordered ) count_ordered++;
          if ( cl->type == f_nowait ) count_nowait++;
       }
	 
     if ( count_schedule > 1 || count_ordered > 1 || count_nowait > 1)
       {
          if ( count_schedule > 1)
          	{
          	   printf ("Too many SCHEDULE clausees.\n");
          	}
          if ( count_ordered > 1)
          	{
          	   printf ("Too many ORDERED clausees.\n");
          	}
          if ( count_nowait > 1)
          	{
          	   printf ("Too many NOWAIT clausees.\n");
          	}

          
          SRCPOS srcpos = Get_Srcpos();
          Fail_FmtAssertion ("Invalid syntax in the #PRAGMA OMP FOR directive at line: %d, file number: %d.!\n", 
                      SRCPOS_linenum(srcpos), SRCPOS_filenum(srcpos));
       }
}


void
expand_start_for (struct for_clause_list * clause_list)
 {

     struct For_clause_wn_type * for_clause_wn;
 
     tree  private_clause_tree = NULL;
     tree  firstprivate_clause_tree = NULL;
     tree  lastprivate_clause_tree = NULL;
     struct reduction_list * reduction_clause_list = NULL;
     bool  ordered_clause_value = false;
     enum schedule_kind_type  schedule_1_clasue_value = SK_NONE;
     struct schedule_2  schedule_2_clause_value; 
     schedule_2_clause_value.schedule_kind = SK_NONE;
     schedule_2_clause_value.chunk_size = NULL;
     bool  nowait_clause_value = false;

     register tree t = NULL;
     struct for_clause_list *cl = NULL;
     struct reduction_list *rl = NULL;
     ST *st = NULL;
     WN *wn = NULL;


     ST_list *private_clause_st = NULL;
     ST_list *firstprivate_clause_st = NULL;
     ST_list *lastprivate_clause_st = NULL;
     WN_list *reduction_clause_wn = NULL;
     struct  schedule_2_wn  schedule_2_clause_wn;
     schedule_2_clause_wn.schedule_2_kind = SK_NONE;
     schedule_2_clause_wn.chunk_size_wn = NULL;


     ST_list *stlist = NULL;
     WN_list *wnlist = NULL;

     check_for_directive(clause_list);        


     for (cl = clause_list; cl != NULL; cl = cl->next)
       {
          if ( cl->type == f_private ) {
              private_clause_tree = chainon (private_clause_tree, cl->node.var_list);
          };
       };


     for (cl = clause_list; cl != NULL; cl = cl->next)
       {
          if ( cl->type == f_firstprivate ) {
             firstprivate_clause_tree = 
             chainon (firstprivate_clause_tree, cl->node.var_list);
          };
       };


     for (cl = clause_list; cl != NULL; cl = cl->next)
       {
          if ( cl->type == f_lastprivate ) {
             lastprivate_clause_tree = 
             chainon (lastprivate_clause_tree, cl->node.var_list);
          };
       };
 
     
     for (cl = clause_list; cl != NULL; cl = cl->next)
       {
          if ( cl->type == f_reduction ) {
          	 rl = (struct reduction_list *) malloc(sizeof(struct reduction_list));
             rl->node = cl->node.reduction_node;
             rl->next = reduction_clause_list;
             reduction_clause_list = rl;
          };
       };
     
     
     for (cl = clause_list; cl != NULL; cl = cl->next)
       {
          if ( cl->type == f_ordered) {
             ordered_clause_value = true;
             break;
          };
       };
  
     
     for (cl = clause_list; cl != NULL; cl = cl->next)
       {
          if ( cl->type == f_schedule_1) {
             schedule_1_clasue_value = cl->node.schedule_kind;
             break;
          };
       };
  
     
     for (cl = clause_list; cl != NULL; cl = cl->next)
       {
          if ( cl->type == f_schedule_2) {
             schedule_2_clause_value.schedule_kind = cl->node.schedule_node.schedule_kind;
             schedule_2_clause_value.chunk_size = cl->node.schedule_node.chunk_size;
             break;
          };
       };


     for (cl = clause_list; cl != NULL; cl = cl->next)
       {
          if ( cl->type == f_nowait) {
             nowait_clause_value = true;
             break;
          };
       };
  
     prepare_com_clause ( private_clause_tree, &private_clause_st );
     
     prepare_com_clause ( firstprivate_clause_tree, &firstprivate_clause_st );

     prepare_com_clause ( lastprivate_clause_tree, &lastprivate_clause_st );

     
     prepare_reduction_clause ( reduction_clause_list, &reduction_clause_wn );    

     if (schedule_2_clause_value.schedule_kind != SK_NONE)
     {
        schedule_2_clause_wn.schedule_2_kind = 
            schedule_2_clause_value.schedule_kind;

        WN *wn =  WFE_Expand_Expr(schedule_2_clause_value.chunk_size);

        schedule_2_clause_wn.chunk_size_wn = wn;
     }


     for_clause_wn = (struct For_clause_wn_type *) malloc(sizeof(For_clause_wn_type)); 

     for_clause_wn-> private_clause = private_clause_st;
     for_clause_wn-> firstprivate_clause = firstprivate_clause_st;
     for_clause_wn-> lastprivate_clause = lastprivate_clause_st;
     for_clause_wn-> reduction_clause = reduction_clause_wn;
     for_clause_wn-> ordered_clause = ordered_clause_value;
     for_clause_wn-> schedule_1_clause = schedule_1_clasue_value;
     for_clause_wn-> schedule_2_clause = schedule_2_clause_wn;
     for_clause_wn-> nowait_clause = nowait_clause_value;

     WFE_expand_start_for (for_clause_wn);

     free(for_clause_wn);

 }

void
expand_end_for ( )
{
     WFE_expand_end_for ();
}


///////// sections directive ////////


void
check_sections_directive( struct sections_clause_list * clause_list )
{
     struct sections_clause_list *cl = NULL;
     int count_nowait = 0;
     
	 for (cl = clause_list; cl != NULL; cl = cl->next)
       {
          if ( cl->type == sections_nowait ) count_nowait++;
       }
	 
     if ( count_nowait > 1)
       {
          printf ("Too many NOWAIT clausees.\n");
          
          SRCPOS srcpos = Get_Srcpos();
          Fail_FmtAssertion ("Invalid syntax in the #PRAGMA OMP SECTIONS directive at line: %d, file number: %d.!\n", 
                      SRCPOS_linenum(srcpos), SRCPOS_filenum(srcpos));
       }
}

void
expand_start_sections (struct sections_clause_list * clause_list)
 {
     struct Sections_clause_wn_type * sections_clause_wn = NULL;
     
     tree  private_clause_tree = NULL;
     tree  firstprivate_clause_tree = NULL;
     tree  lastprivate_clause_tree = NULL;
     
     struct reduction_list * reduction_clause_list = NULL;
     bool  nowait_clause_value = false;

     register tree t = NULL;
     struct sections_clause_list *cl = NULL;
     struct reduction_list *rl = NULL;
     ST *st = NULL;
     WN *wn = NULL;


     ST_list *private_clause_st = NULL;
     ST_list *firstprivate_clause_st = NULL;
     ST_list *lastprivate_clause_st = NULL;
     WN_list *reduction_clause_wn = NULL;

     ST_list *stlist = NULL;
     WN_list *wnlist = NULL;


     check_sections_directive(clause_list);        
     

     for (cl = clause_list; cl != NULL; cl = cl->next)
       {
          if ( cl->type == sections_private ) {
              private_clause_tree = chainon (private_clause_tree, cl->node.var_list);
          };
       };


     for (cl = clause_list; cl != NULL; cl = cl->next)
       {
          if ( cl->type == sections_firstprivate ) {
             firstprivate_clause_tree = 
             chainon (firstprivate_clause_tree, cl->node.var_list);
          };
       };


     for (cl = clause_list; cl != NULL; cl = cl->next)
       {
          if ( cl->type == sections_lastprivate ) {
             lastprivate_clause_tree = 
             chainon (lastprivate_clause_tree, cl->node.var_list);
          };
       };
 
     
     for (cl = clause_list; cl != NULL; cl = cl->next)
       {
          if ( cl->type == sections_reduction ) {
          	 rl = (struct reduction_list *) malloc(sizeof(struct reduction_list));
             rl->node = cl->node.reduction_node;
             rl->next = reduction_clause_list;
             reduction_clause_list = rl;
          };
       };


     for (cl = clause_list; cl != NULL; cl = cl->next)
       {
          if ( cl->type == sections_nowait) {
             nowait_clause_value = true;
             break;
          };
       };
  
     prepare_com_clause ( private_clause_tree, &private_clause_st );
     
     prepare_com_clause ( firstprivate_clause_tree, &firstprivate_clause_st );

     prepare_com_clause ( lastprivate_clause_tree, &lastprivate_clause_st );
     
     prepare_reduction_clause ( reduction_clause_list, &reduction_clause_wn );    



     sections_clause_wn = (struct Sections_clause_wn_type *) malloc(sizeof(Sections_clause_wn_type)); 

     sections_clause_wn-> private_clause = private_clause_st;
     sections_clause_wn-> firstprivate_clause = firstprivate_clause_st;
     sections_clause_wn-> lastprivate_clause = lastprivate_clause_st;
     sections_clause_wn-> reduction_clause = reduction_clause_wn;
     sections_clause_wn-> nowait_clause = nowait_clause_value;

     WFE_expand_start_sections (sections_clause_wn);

     free(sections_clause_wn);

 }

void
expand_end_sections ( )
{
     WFE_expand_end_sections ();
}


///////// section directive ////////


void expand_start_section ( )
{
     WFE_expand_start_section ();
}

void expand_end_section ( )
{
     WFE_expand_end_section ();
}

///////// single directive ////////

#ifdef TARG_SL2 //fork_joint
/* following code is used to handle sl2 fork_joint and we put our implementation in openmp framework */ 
void
expand_start_sl2_sections (bool is_minor_thread)
 {
     WFE_expand_start_sl2_sections (is_minor_thread);
 }

void
expand_end_sl2_sections ( )
{
     WFE_expand_end_sl2_sections ();
}

void expand_start_sl2_section (bool is_minor_thread)
{
     WFE_expand_start_sl2_section (is_minor_thread);
}

void expand_end_sl2_section ( )
{
     WFE_expand_end_sl2_section ();
}
#endif 


void
check_single_directive( struct single_clause_list * clause_list )
{
     struct single_clause_list *cl = NULL;
     int count_copyprivate = 0, count_nowait = 0;
     
	 for (cl = clause_list; cl != NULL; cl = cl->next)
       {
          if ( cl->type == single_copyprivate ) count_copyprivate++;
          if ( cl->type == single_nowait ) count_nowait++;
       }
	 
     if ( count_nowait > 1 || (count_copyprivate != 0 && count_nowait != 0) )
       {
          if ( count_copyprivate != 0 && count_nowait != 0 )
          	{
          	   printf ("The copyprivate clause must not be used with the nowait clause.\n");
          	}
          if ( count_nowait > 1)
          	{
          	   printf ("Too many NOWAIT clausees.\n");
          	}

          SRCPOS srcpos = Get_Srcpos();
          Fail_FmtAssertion ("Invalid syntax in the #PRAGMA OMP SINGLE directive at line: %d, file number: %d.!\n", 
                      SRCPOS_linenum(srcpos), SRCPOS_filenum(srcpos));
       }
}

void
expand_start_single (struct single_clause_list * clause_list)
 {
     struct Single_clause_wn_type * single_clause_wn;
     
     tree  private_clause_tree = NULL;
     tree  firstprivate_clause_tree = NULL;
     tree  copyprivate_clause_tree = NULL;
     bool  nowait_clause_value = false;

     register tree t = NULL;
     struct single_clause_list *cl = NULL;
     ST *st = NULL;
     WN *wn = NULL;


     ST_list *private_clause_st = NULL;
     ST_list *firstprivate_clause_st = NULL;
     ST_list *copyprivate_clause_st = NULL;

     ST_list *stlist = NULL;
     WN_list *wnlist = NULL;

     check_single_directive(clause_list);    
     

     for (cl = clause_list; cl != NULL; cl = cl->next)
       {
          if ( cl->type == single_private ) {
              private_clause_tree = chainon (private_clause_tree, cl->node.var_list);
          };
       };


     for (cl = clause_list; cl != NULL; cl = cl->next)
       {
          if ( cl->type == single_firstprivate ) {
             firstprivate_clause_tree = 
             chainon (firstprivate_clause_tree, cl->node.var_list);
          };
       };


     for (cl = clause_list; cl != NULL; cl = cl->next)
       {
          if ( cl->type == single_copyprivate ) {
             copyprivate_clause_tree = 
             chainon (copyprivate_clause_tree, cl->node.var_list);
          };
       };


     for (cl = clause_list; cl != NULL; cl = cl->next)
       {
          if ( cl->type == single_nowait) {
             nowait_clause_value = true;
             break;
          };
       };

     prepare_com_clause ( private_clause_tree, &private_clause_st );
     
     prepare_com_clause ( firstprivate_clause_tree, &firstprivate_clause_st );

     prepare_com_clause ( copyprivate_clause_tree, &copyprivate_clause_st );
     
     single_clause_wn = (struct Single_clause_wn_type *) malloc(sizeof(Single_clause_wn_type)); 

     single_clause_wn-> private_clause = private_clause_st;
     single_clause_wn-> firstprivate_clause = firstprivate_clause_st;
     single_clause_wn-> copyprivate_clause = copyprivate_clause_st;
     single_clause_wn-> nowait_clause = nowait_clause_value;

     WFE_expand_start_single (single_clause_wn);

     free(single_clause_wn);

}

void expand_end_single( )
{
     WFE_expand_end_single ();
}


///////// parallel for directive ////////


void
check_parallel_for_directive( struct parallel_for_clause_list * clause_list )
{
     struct parallel_for_clause_list *cl = NULL;
     int count_if = 0, count_num_threads = 0, count_default = 0;
     int count_schedule = 0, count_ordered = 0;
     
	 for (cl = clause_list; cl != NULL; cl = cl->next)
       {
          if ( cl->type == p_for_if ) count_if++;
          if ( cl->type == p_for_num_threads ) count_num_threads++;
          if ( cl->type == p_for_default ) count_default++;
          if ( cl->type == p_for_schedule_1 || cl->type == p_for_schedule_2 ) count_schedule++;
          if ( cl->type == p_for_ordered ) count_ordered++;
       }
	 
     if ( count_if > 1 || count_num_threads > 1 || count_default >1
     	 ||count_schedule > 1 || count_ordered > 1 )
       {
          if ( count_if > 1)
          	{
          	   printf ("Too many IF clausees.\n");
          	}
          if ( count_num_threads > 1)
          	{
          	   printf ("Too many NUM_THREADS clausees.\n");
          	}
          if ( count_default > 1)
          	{
          	   printf ("Too many DEFAULT clausees.\n");
          	}
          if ( count_schedule > 1)
          	{
          	   printf ("Too many SCHEDULE clausees.\n");
          	}
          if ( count_ordered > 1)
          	{
          	   printf ("Too many ORDERED clausees.\n");
          	}
          
          SRCPOS srcpos = Get_Srcpos();
          Fail_FmtAssertion ("Invalid syntax in the #PRAGMA OMP PARALLEL FOR directive at line: %d, file number: %d.!\n", 
                      SRCPOS_linenum(srcpos), SRCPOS_filenum(srcpos));
       }

}


void
expand_start_parallel_for (struct parallel_for_clause_list * clause_list)
 {
     struct Parallel_for_clause_wn_type * parallel_for_clause_wn;
     
     tree  if_clause_tree = NULL, num_threads_clause_tree = NULL;
     tree  private_clause_tree = NULL, firstprivate_clause_tree = NULL;
     tree  shared_clause_tree = NULL, copyin_clause_tree = NULL;
     tree  lastprivate_clause_tree = NULL;
     struct reduction_list  *reduction_clause_list = NULL;
     enum default_type default_clause_value = no_default;
     bool  ordered_clause_value = false;
     enum schedule_kind_type  schedule_1_clasue_value = SK_NONE;
     struct schedule_2  schedule_2_clause_value; 
     schedule_2_clause_value.schedule_kind = SK_NONE;
     schedule_2_clause_value.chunk_size = NULL;

     WN   *if_clause_wn = NULL, *num_threads_clause_wn =NULL ;

     struct parallel_for_clause_list *cl = NULL;
     struct reduction_list *rl = NULL;
     
     ST_list *private_clause_st = NULL;
     ST_list *firstprivate_clause_st = NULL;
     ST_list *shared_clause_st = NULL;
     ST_list *copyin_clause_st = NULL;
     WN_list *reduction_clause_wn = NULL;
     ST_list *lastprivate_clause_st = NULL;
     struct  schedule_2_wn  schedule_2_clause_wn;
     schedule_2_clause_wn.schedule_2_kind = SK_NONE;
     schedule_2_clause_wn.chunk_size_wn = NULL;

     check_parallel_for_directive(clause_list);     
     

     for (cl = clause_list; cl != NULL; cl = cl->next)
       {
          if ( cl->type == p_for_if ) {
              if_clause_tree = cl->node.expr_no_commas;
              break;
          };
       };


     for (cl = clause_list; cl != NULL; cl = cl->next)
       {
          if ( cl->type == p_for_num_threads ) {
              num_threads_clause_tree = cl->node.expr_no_commas;
              break;
          };
     };


     for (cl = clause_list; cl != NULL; cl = cl->next)
       {
          if ( cl->type == p_for_private ) {
              private_clause_tree = chainon (private_clause_tree, cl->node.var_list);
          };
       };


     for (cl = clause_list; cl != NULL; cl = cl->next)
       {
          if ( cl->type == p_for_firstprivate ) {
             firstprivate_clause_tree = 
             chainon (firstprivate_clause_tree, cl->node.var_list);
          };
       };


     for (cl = clause_list; cl != NULL; cl = cl->next)
       {
          if ( cl->type == p_for_lastprivate ) {
             lastprivate_clause_tree = 
             chainon (lastprivate_clause_tree, cl->node.var_list);
          };
       };


     for (cl = clause_list; cl != NULL; cl = cl->next)
       {
          if ( cl->type == p_for_shared ) {
             shared_clause_tree = chainon (shared_clause_tree, cl->node.var_list);
          };
       };


     for (cl = clause_list; cl != NULL; cl = cl->next)
       {
          if ( cl->type == p_for_copyin ) {
             copyin_clause_tree = chainon (copyin_clause_tree, cl->node.var_list);
          };
       };


     for (cl = clause_list; cl != NULL; cl = cl->next)
       {
          if ( cl->type == p_for_reduction ) {
          	 rl = (struct reduction_list *) malloc(sizeof(struct reduction_list));
             rl->node = cl->node.reduction_node;
             rl->next = reduction_clause_list;
             reduction_clause_list = rl;
          };
       };


     for (cl = clause_list; cl != NULL; cl = cl->next)
       {
          if ( cl->type == p_for_default) {
             default_clause_value = cl->node.defaulttype;
             break;
          };
       };

     
     for (cl = clause_list; cl != NULL; cl = cl->next)
       {
          if ( cl->type == p_for_ordered) {
             ordered_clause_value = true;
             break;
          };
       };
  
     
     for (cl = clause_list; cl != NULL; cl = cl->next)
       {
          if ( cl->type == p_for_schedule_1) {
             schedule_1_clasue_value = cl->node.schedule_kind;
             break;
          };
       };
  
     
     for (cl = clause_list; cl != NULL; cl = cl->next)
       {
          if ( cl->type == p_for_schedule_2) {
             schedule_2_clause_value.schedule_kind = cl->node.schedule_node.schedule_kind;
             schedule_2_clause_value.chunk_size = cl->node.schedule_node.chunk_size;
             break;
          };
       };
  
     if (if_clause_tree) 
     {
     	 if_clause_wn = WFE_Expand_Expr (if_clause_tree);
	 TYPE_ID type = WN_rtype (if_clause_wn);
	 WN * val = (MTYPE_is_integral (type)) ? WN_Intconst (type, 0) :
	                                         WN_Floatconst (type, 0);
	 if_clause_wn = WN_NE (WN_rtype (if_clause_wn),
	                       val, if_clause_wn);
     }
     
     if (num_threads_clause_tree) 
    	 num_threads_clause_wn = WFE_Expand_Expr (num_threads_clause_tree);

     prepare_com_clause ( private_clause_tree, &private_clause_st );
     
     prepare_com_clause ( firstprivate_clause_tree, &firstprivate_clause_st );

     prepare_com_clause ( shared_clause_tree, &shared_clause_st );

     prepare_com_clause ( copyin_clause_tree, &copyin_clause_st );

     prepare_reduction_clause ( reduction_clause_list, &reduction_clause_wn );    

     prepare_com_clause ( lastprivate_clause_tree, &lastprivate_clause_st );

     if (schedule_2_clause_value.schedule_kind != SK_NONE)
     {
        schedule_2_clause_wn.schedule_2_kind = 
            schedule_2_clause_value.schedule_kind;
        WN *wn = WFE_Expand_Expr(schedule_2_clause_value.chunk_size);
        schedule_2_clause_wn.chunk_size_wn = wn;
     }

     parallel_for_clause_wn = 
     	(struct Parallel_for_clause_wn_type *) malloc(sizeof(Parallel_for_clause_wn_type)); 

     parallel_for_clause_wn-> if_clause = if_clause_wn;
     parallel_for_clause_wn-> num_threads_clause = num_threads_clause_wn;
     parallel_for_clause_wn-> private_clause = private_clause_st;
     parallel_for_clause_wn-> firstprivate_clause = firstprivate_clause_st;
     parallel_for_clause_wn-> shared_clause = shared_clause_st;
     parallel_for_clause_wn-> copyin_clause = copyin_clause_st;
     parallel_for_clause_wn-> reduction_clause = reduction_clause_wn;
     parallel_for_clause_wn-> default_clause = default_clause_value;
     parallel_for_clause_wn-> lastprivate_clause = lastprivate_clause_st;
     parallel_for_clause_wn-> ordered_clause = ordered_clause_value;
     parallel_for_clause_wn-> schedule_1_clause = schedule_1_clasue_value;
     parallel_for_clause_wn-> schedule_2_clause = schedule_2_clause_wn;

     WFE_expand_start_parallel_for (parallel_for_clause_wn);

     free(parallel_for_clause_wn);

 }

void
expand_end_parallel_for ( )
{
     WFE_expand_end_parallel_for ();
}


///////// parallel sections directive ////////

void
check_parallel_sections_directive( struct parallel_sections_clause_list * clause_list )
{
     struct parallel_sections_clause_list *cl = NULL;
     int count_if = 0, count_num_threads = 0, count_default = 0;
     
	 for (cl = clause_list; cl != NULL; cl = cl->next)
       {
          if ( cl->type == p_sections_if ) count_if++;
          if ( cl->type == p_sections_num_threads ) count_num_threads++;
          if ( cl->type == p_sections_default ) count_default++;
       }
	 
     if ( count_if > 1 || count_num_threads > 1 || count_default > 1 )
       {
          if ( count_if > 1)
          	{
          	   printf ("Too many IF clausees.\n");
          	}
          if ( count_num_threads > 1)
          	{
          	   printf ("Too many NUM_THREADS clausees.\n");
          	}
          if ( count_default > 1)
          	{
          	   printf ("Too many DEFAULT clausees.\n");
          	}
          
          SRCPOS srcpos = Get_Srcpos();
          Fail_FmtAssertion ("Invalid syntax in the #PRAGMA OMP PARALLEL SECTIONS directive at line: %d, file number: %d.!\n", 
                      SRCPOS_linenum(srcpos), SRCPOS_filenum(srcpos));
       }

}

void
expand_start_parallel_sections( struct parallel_sections_clause_list * clause_list )
{
     struct Parallel_sections_clause_wn_type * parallel_sections_clause_wn;
     
     tree  if_clause_tree = NULL, num_threads_clause_tree = NULL;
     tree  private_clause_tree = NULL, firstprivate_clause_tree = NULL;
     tree  shared_clause_tree = NULL, copyin_clause_tree = NULL;
     tree  lastprivate_clause_tree = NULL;
     struct reduction_list  *reduction_clause_list = NULL;
     enum default_type default_clause_value = no_default;

     WN   *if_clause_wn = NULL, *num_threads_clause_wn =NULL ;

     struct parallel_sections_clause_list *cl = NULL;
     struct reduction_list *rl = NULL;
     
     ST_list *private_clause_st = NULL;
     ST_list *firstprivate_clause_st = NULL;
     ST_list *lastprivate_clause_st = NULL;
     ST_list *shared_clause_st = NULL;
     ST_list *copyin_clause_st = NULL;
     WN_list *reduction_clause_wn = NULL;

     check_parallel_sections_directive(clause_list);      


     for (cl = clause_list; cl != NULL; cl = cl->next)
       {
          if ( cl->type == p_sections_if ) {
              if_clause_tree = cl->node.expr_no_commas;
              break;
          };
       };


     for (cl = clause_list; cl != NULL; cl = cl->next)
       {
          if ( cl->type == p_sections_num_threads ) {
              num_threads_clause_tree = cl->node.expr_no_commas;
              break;
          };
     };


     for (cl = clause_list; cl != NULL; cl = cl->next)
       {
          if ( cl->type == p_sections_private ) {
              private_clause_tree = chainon (private_clause_tree, cl->node.var_list);
          };
       };


     for (cl = clause_list; cl != NULL; cl = cl->next)
       {
          if ( cl->type == p_sections_firstprivate ) {
             firstprivate_clause_tree = 
             chainon (firstprivate_clause_tree, cl->node.var_list);
          };
       };


     for (cl = clause_list; cl != NULL; cl = cl->next)
       {
          if ( cl->type == p_sections_lastprivate ) {
             lastprivate_clause_tree = 
             chainon (lastprivate_clause_tree, cl->node.var_list);
          };
       };


     for (cl = clause_list; cl != NULL; cl = cl->next)
       {
          if ( cl->type == p_sections_shared ) {
             shared_clause_tree = chainon (shared_clause_tree, cl->node.var_list);
          };
       };


     for (cl = clause_list; cl != NULL; cl = cl->next)
       {
          if ( cl->type == p_sections_copyin ) {
             copyin_clause_tree = chainon (copyin_clause_tree, cl->node.var_list);
          };
       };


     for (cl = clause_list; cl != NULL; cl = cl->next)
       {
          if ( cl->type == p_sections_reduction ) {
          	 rl = (struct reduction_list *) malloc(sizeof(struct reduction_list));
             rl->node = cl->node.reduction_node;
             rl->next = reduction_clause_list;
             reduction_clause_list = rl;
          };
       };


     for (cl = clause_list; cl != NULL; cl = cl->next)
       {
          if ( cl->type == p_sections_default) {
             default_clause_value = cl->node.defaulttype;
             break;
          };
       };

  
     if (if_clause_tree) 
     {
     	 if_clause_wn = WFE_Expand_Expr (if_clause_tree);
	 TYPE_ID type = WN_rtype (if_clause_wn);
	 WN * val = (MTYPE_is_integral (type)) ? WN_Intconst (type, 0) :
	                                         WN_Floatconst (type, 0);
	 if_clause_wn = WN_NE (WN_rtype (if_clause_wn),
	                       val, if_clause_wn);
     }
     
     if (num_threads_clause_tree) 
    	 num_threads_clause_wn = WFE_Expand_Expr (num_threads_clause_tree);

     prepare_com_clause ( private_clause_tree, &private_clause_st );
     
     prepare_com_clause ( firstprivate_clause_tree, &firstprivate_clause_st );

     prepare_com_clause ( shared_clause_tree, &shared_clause_st );

     prepare_com_clause ( copyin_clause_tree, &copyin_clause_st );

     prepare_reduction_clause ( reduction_clause_list, &reduction_clause_wn );    

     prepare_com_clause ( lastprivate_clause_tree, &lastprivate_clause_st );

     parallel_sections_clause_wn = 
     	(struct Parallel_sections_clause_wn_type *) malloc(sizeof(Parallel_sections_clause_wn_type)); 

     parallel_sections_clause_wn-> if_clause = if_clause_wn;
     parallel_sections_clause_wn-> num_threads_clause = num_threads_clause_wn;
     parallel_sections_clause_wn-> private_clause = private_clause_st;
     parallel_sections_clause_wn-> firstprivate_clause = firstprivate_clause_st;
     parallel_sections_clause_wn-> shared_clause = shared_clause_st;
     parallel_sections_clause_wn-> copyin_clause = copyin_clause_st;
     parallel_sections_clause_wn-> reduction_clause = reduction_clause_wn;
     parallel_sections_clause_wn-> default_clause = default_clause_value;
     parallel_sections_clause_wn-> lastprivate_clause = lastprivate_clause_st;

     WFE_expand_start_parallel_sections (parallel_sections_clause_wn);

     free(parallel_sections_clause_wn);

}

void
expand_end_parallel_sections( )
{
    WFE_expand_end_parallel_sections ();
}


///////// master directive ////////


void expand_start_master ( )
{
     WFE_expand_start_master ();
}

void expand_end_master ( )
{
     WFE_expand_end_master ();
}


///////// critical directive ////////


void expand_start_critical( tree region_phrase )
{
    char *critical_name = NULL;
    ST *st = NULL;        
    TCON           tcon;
    TY_IDX ty;

    if (region_phrase)
      {
         critical_name = IDENTIFIER_POINTER (region_phrase);
         tcon = Host_To_Targ_String ( MTYPE_STRING, critical_name, strlen(critical_name));
         st = Gen_String_Sym (&tcon, MTYPE_To_TY(MTYPE_STRING), FALSE );
      }
    if (Trace_Omp)
      printf("critical name is %s \n",critical_name);
    WFE_expand_start_critical ( st,critical_name );
}
 
void expand_end_critical (  )
{
      WFE_expand_end_critical ( );
}


///////// atomaic directive ////////


void check_atomic_expression ( tree atomic_expression )
{
    bool valid_for_expr = true;
    enum tree_code code, code1;
    
    code = TREE_CODE (atomic_expression);

    if (code != MODIFY_EXPR && code != PREDECREMENT_EXPR &&
        code != PREINCREMENT_EXPR && code != POSTDECREMENT_EXPR &&
	code != POSTINCREMENT_EXPR)
    {
	printf (" No such increment operation is permitted.\n");
	valid_for_expr = false;		
    }
    else {
        code1 = TREE_CODE (TREE_OPERAND (atomic_expression, 1));
	if (code1 == NOP_EXPR)
	  code1 = TREE_CODE (TREE_OPERAND (TREE_OPERAND (atomic_expression, 1), 0));
        if (code == MODIFY_EXPR && code1 != PLUS_EXPR &&
	    code1 != MINUS_EXPR && code1 != MULT_EXPR &&
	    code1 != RDIV_EXPR && code1 != BIT_AND_EXPR &&
	    code1 != BIT_XOR_EXPR && code1 != BIT_IOR_EXPR &&
	    code1 != LSHIFT_EXPR && code1 != RSHIFT_EXPR &&
	    code1 != TRUNC_DIV_EXPR)
           {
               printf (" No such modifying operation is supported.\n");
               valid_for_expr = false;		
           }
    }

   if ( !valid_for_expr) 
   	{
   	  SRCPOS srcpos = Get_Srcpos();
      Fail_FmtAssertion ("Invalid atomic_expression in an ATOMIC directive!  at line: %d, file number: %d.!\n", 
                      SRCPOS_linenum(srcpos), SRCPOS_filenum(srcpos));
   	}  
}


void  expand_start_atomic ()   
{
    WFE_expand_start_atomic ();
}

void  expand_end_atomic ()   
{
    WFE_expand_end_atomic ();
}


///////// ordered directive ////////


void expand_start_ordered( )
{
    WFE_expand_start_ordered ( );
} 

void expand_end_ordered ( )
{
    WFE_expand_end_ordered ( );
}


///////// barrier directive ////////


void  expand_barrier ()         
{
     WFE_expand_barrier ();
}


///////// flush directive ////////


void  expand_flush ( tree flush_variables)         
{
    register tree t = NULL;
    WN_list *flush_wn_list = NULL;
    WN_list *wnlist = NULL;

    for (t = flush_variables; t; t = TREE_CHAIN (t))
    { 
       WN *wn = WFE_Expand_Expr(TREE_VALUE(t));
       wnlist = (struct WN_list *) malloc(sizeof(struct WN_list));
       wnlist->wn = WN_CopyNode (wn) ;
       wnlist->next = flush_wn_list;
       flush_wn_list = wnlist;
    }

    WFE_expand_flush (flush_wn_list);
    
}


///////// threadprivate directive ////////


void  expand_threadprivate ( tree threadprivate_variables)         
{
    register tree t = NULL;
    ST_list *threadprivate_st = NULL;
    ST_list *stlist = NULL;
    WN *wn;
    ST *st;
    for (t = threadprivate_variables; t; t = TREE_CHAIN (t))
    { 
       st = Get_Pre_ST (TREE_VALUE(t));
       stlist = (struct ST_list *) malloc(sizeof(struct ST_list));
       stlist->st = st ;
       stlist->next = threadprivate_st;
       threadprivate_st = stlist;
    }

    WFE_expand_threadprivate (threadprivate_st);
}


///////// do loop expander ////////

int  check_do_loop_for(tree init_expr, tree logical_expr, tree incr_expr)
{

    bool valid_for_expr = true;
    enum tree_code code, code1;
    
    WN *index, *start, *end, *step;
    
	WN *wn_tmp,  * lcv , * incv;

	TYPE_ID lcv_t;

    code = TREE_CODE (init_expr);

    if (code != MODIFY_EXPR) 
    	{
	      valid_for_expr = false;		
    	}
    else 
    	{
           lcv = WFE_Expand_Expr (TREE_OPERAND (init_expr, 0));
           lcv_t = TY_mtype(ST_type(WN_st(lcv)));

           if (lcv_t != MTYPE_I4 && lcv_t != MTYPE_I8 && lcv_t != MTYPE_I2
           	          && lcv_t != MTYPE_I1)
           	{         
   	             valid_for_expr = false;		
           	}
        }
    
    code = TREE_CODE (logical_expr);

    if (valid_for_expr && (code != LT_EXPR) && (code != LE_EXPR) && (code != GT_EXPR) && (code != GE_EXPR)) 
    	{
   	       valid_for_expr = false;		
    	}
    
    
    code = TREE_CODE (incr_expr);

    if (valid_for_expr && (code != MODIFY_EXPR) && (code != PREDECREMENT_EXPR) && (code != PREINCREMENT_EXPR) 
           && (code != POSTDECREMENT_EXPR) && (code != POSTINCREMENT_EXPR)) 
    	{
   	       valid_for_expr = false;		
    	}
    else {
           incv = WFE_Expand_Expr (TREE_OPERAND (incr_expr, 0));
           if (WN_st_idx(lcv)!=WN_st_idx(incv))
           	{	     
   	           valid_for_expr = false;		
           	}
           else 
           	{
               code1 = TREE_CODE (TREE_OPERAND (incr_expr, 1));
               if ((code == MODIFY_EXPR) && (code1 != PLUS_EXPR) && (code1 != MINUS_EXPR))
               	{
   	                valid_for_expr = false;		
               	}
             
           	}
    }

   if (valid_for_expr) 
   	{
         return true;
   	}
   else
   	{
   	     return false;
   	}  


}
void expand_start_do_loop (tree init_expr, tree logical_expr, tree incr_expr, struct nesting * nest)
{
    bool valid_for_expr = true;
    enum tree_code code, code1;
    tree temp; 
    
    WN *index, *start, *end, *step;
    
	WN *wn_tmp,  * lcv , * incv;
    WN *wn,*wn1;
	WN *tdecl;
	ST *tst;
	ST_IDX stlcv;

	TYPE_ID lcv_t;

    code = TREE_CODE (init_expr);

 
    
    if (code != MODIFY_EXPR&&code!=VAR_DECL) 
    	{
		   printf ("Invalid init_expr in a FOR statement! \n");
   	       valid_for_expr = false;
   	       WFE_Stmt_Push (WN_CreateBlock (), wfe_stmk_comma, Get_Srcpos());
    	}
    else 
    	{
    	   
    	 if(code==VAR_DECL)
         	{
  			  tst=DECL_ST(init_expr);
   		      index = WN_CreateIdname(0,tst);
              WFE_Stmt_Push (WN_CreateBlock (), wfe_stmk_comma, Get_Srcpos());
              
            //temp= build_modify_expr (DECL_NAME(init_expr), NOP_EXPR, DECL_INITIAL(init_expr));
             wn1 = WFE_Expand_Expr (DECL_INITIAL(init_expr)); // r.h.s.
#ifdef TARG_SL
	         wn_tmp = WFE_Lhs_Of_Modify_Expr(MODIFY_EXPR, init_expr, NULL, FALSE, 
#else
	         wn_tmp = WFE_Lhs_Of_Modify_Expr(MODIFY_EXPR, init_expr, FALSE, 
#endif
				     0, 0, 0, FALSE, wn1, 0, FALSE, FALSE);
             //wn_tmp = WFE_Expand_Expr (temp); 
   		     wn_tmp = WFE_Stmt_Pop (wfe_stmk_comma);
  	          start = WN_COPY_Tree( WN_first( wn_tmp ));
		         WN_DELETE_Tree( wn_tmp );
  			 
    	  }
          else
          {
          	lcv = WFE_Expand_Expr (TREE_OPERAND (init_expr, 0));
           lcv_t = TY_mtype(ST_type(WN_st(lcv)));

           if (lcv_t != MTYPE_I4 && lcv_t != MTYPE_I8 && lcv_t != MTYPE_I2
           	          && lcv_t != MTYPE_I1)
           	{
		         printf ("Invalid induction variable type in init_expr in a FOR statement! \n");
   	             valid_for_expr = false;		
           	}
           else
           	  {
    	         index = WN_CreateIdname(0,WN_st_idx(lcv));
    	   
                 WFE_Stmt_Push (WN_CreateBlock (), wfe_stmk_comma, Get_Srcpos());
                
                 
                 wn_tmp = WFE_Expand_Expr (init_expr); 
                 
/*                 
                 TYPE_ID from_type = WN_rtype(wn_tmp);     

                 if (from_type != lcv_t) 
                 	{
                 	   printf ("Type inconsistence in initialization of the loop variable! \n");
                 	   valid_for_expr = false;	   
                 	}
*/
	             wn_tmp = WFE_Stmt_Pop (wfe_stmk_comma);
		         start = WN_COPY_Tree( WN_first( wn_tmp ));
		         WN_DELETE_Tree( wn_tmp );
		      } 
          }
       }
    code = TREE_CODE (logical_expr);
    if (valid_for_expr && (code != LT_EXPR) && (code != LE_EXPR) && (code != GT_EXPR) && (code != GE_EXPR)) 
    	{
	       printf ("Invalid logical_expr in a FOR statement! \
	   	      The logical operators can only be <=, <, > or >= \n");
   	       valid_for_expr = false;		
    	}
    else {
           end = WFE_Expand_Expr (logical_expr); 
          
        }
    
    code = TREE_CODE (incr_expr);

    if (valid_for_expr && (code != MODIFY_EXPR) && (code != PREDECREMENT_EXPR) && (code != PREINCREMENT_EXPR) 
           && (code != POSTDECREMENT_EXPR) && (code != POSTINCREMENT_EXPR)) 
    	{
		   printf ("Invalid incr_expr in a FOR statement! \n");
   	       valid_for_expr = false;		
    	}
    else {
           incv = WFE_Expand_Expr (TREE_OPERAND (incr_expr, 0));
           if(TREE_CODE(init_expr)==VAR_DECL)
           	 stlcv=ST_st_idx(tst);
           else stlcv=WN_st_idx(lcv);
           if (stlcv!=WN_st_idx(incv))
           	{
		       printf ("Invalid incr_expr in a FOR statement!  \
		       	      No induction variable to be modified.\n");
   	           valid_for_expr = false;		
           	}
           else 
           	{
               code1 = TREE_CODE (TREE_OPERAND (incr_expr, 1));
               if ((code == MODIFY_EXPR) && (code1 != PLUS_EXPR) && (code1 != MINUS_EXPR))
               	{
		            printf ("Invalid incr_expr in a FOR statement!  \
		       	           No such increment operation is permitted.\n");
   	                valid_for_expr = false;		
		            
               	}
               else
               	{
                   WFE_Stmt_Push (WN_CreateBlock (), wfe_stmk_comma, Get_Srcpos());
                   WFE_Expand_Expr (incr_expr, FALSE); 
                   wn_tmp = WFE_Stmt_Pop (wfe_stmk_comma);
   	               step = WN_COPY_Tree( WN_first( wn_tmp ));
	               WN_DELETE_Tree(  wn_tmp );
               	}
           	}
    }
   if (valid_for_expr) 
   	{
           WFE_expand_start_do_loop (index, start, end, step, nest);
   	}
   else
   	{
   	  SRCPOS srcpos = Get_Srcpos();
      Fail_FmtAssertion ("Invalid syntax in the FOR statement at line: %d, file number: %d.!\n", 
                      SRCPOS_linenum(srcpos), SRCPOS_filenum(srcpos));
   	}
}


void expand_end_do_loop (struct nesting * nest)
{
    WFE_expand_end_do_loop(nest);
}

///////// build clause list for PARALLEL directive ////////// 


struct parallel_clause_list *
chain_parallel_list_on (struct parallel_clause_list * pclause_list, struct parallel_clause_list * pclause)
{
    struct parallel_clause_list *pcl;
    if (pclause_list) 
    	{
            for (pcl = pclause_list; pcl->next != NULL; pcl = pcl->next);
	        pcl->next = pclause;
            return pclause_list;
    	}
    else  return pclause;

 }

// This flag is used to determine the context we are in, inside a omp
// pragma stmt. OpenMP keywords can be the names of variables for example
// in variable-list. So if we have seen the '(' before a variable-list
// (seen_omp_paren == TRUE), and have not seen the ')' after the variable-list,
// then don't treat the var-names as OpenMP keywords.
extern bool seen_omp_paren;

struct parallel_clause_list *
build_parallel_clause_list (tree t, 
                            parallel_clause_type p_type, 
			    default_type d_type,
			    reduction_op_type red_op)
{
  // mark the end of the current clause
  seen_omp_paren = FALSE;

  parallel_clause_list * result = (parallel_clause_list *) malloc (sizeof (parallel_clause_list));

  result->type = p_type;
  result->next = NULL;
  switch (p_type)
  {
    case p_if:
    case p_num_threads:
      result->node.expr_no_commas = t;
      break;
    case p_private:
    case p_firstprivate:
    case p_shared:
    case p_copyin:
      result->node.var_list = t;
      break;
    case p_default:
      result->node.defaulttype = d_type;
      break;
    case p_reduction:
      result->node.reduction_node.reduction_op = red_op;
      result->node.reduction_node.var_list = t;
      break;
    default:
      Fail_FmtAssertion ("Unexpected parallel-clause-type");
  }

  return result;
}

///////// build clause list for FOR directive ////////// 


struct for_clause_list *
chain_for_list_on (struct for_clause_list * fclause_list, struct for_clause_list * fclause)
{
	struct for_clause_list *fcl;
    if (fclause_list) 
    	{
            for (fcl = fclause_list; fcl->next != NULL; fcl = fcl->next);
	        fcl->next = fclause;
            return fclause_list;
    	}
    else  return fclause;
}

struct for_clause_list *
build_for_clause_list (tree t, for_clause_type f_type, schedule_kind_type s_kind, reduction_op_type red_op)
{
  // mark the end of the current clause
  seen_omp_paren = FALSE;

  for_clause_list * result = (for_clause_list *) malloc (sizeof (for_clause_list));

  result->type = f_type;
  result->next = NULL;
  
  switch (f_type)
  {
    case f_private:
    case f_firstprivate:
    case f_lastprivate:
      result->node.var_list = t;
      break;
    case f_schedule_1:
      result->node.schedule_kind = s_kind;
      break;
    case f_schedule_2:
      result->node.schedule_node.chunk_size = t;
      result->node.schedule_node.schedule_kind = s_kind;
      break;
    case f_reduction:
      result->node.reduction_node.var_list = t;
      result->node.reduction_node.reduction_op = red_op;
      break;
    case f_ordered:
    case f_nowait:
      result->node.ordered_nowait = 0;
      break;
    default:
      Fail_FmtAssertion ("Unexpected for-clause-type");
  }
  return result;
}

///////// build clause list for SECTIONS directive ////////// 

struct sections_clause_list *
chain_sections_list_on (struct sections_clause_list * sclause_list, 
                           struct sections_clause_list * sclause)
{
	struct sections_clause_list *scl;
    if (sclause_list) 
    	{
            for (scl = sclause_list; scl->next != NULL; scl = scl->next);
	        scl->next = sclause;
            return sclause_list;
    	}
    else  return sclause;
}

struct sections_clause_list *
build_sections_clause_list (tree t, sections_clause_type s_type, reduction_op_type red_op)
{
  // mark the end of the current clause
  seen_omp_paren = FALSE;

  sections_clause_list * result = (sections_clause_list *) malloc (sizeof (sections_clause_list));

  result->type = s_type;
  result->next = NULL;

  switch (s_type)
  {
    case sections_private:
    case sections_firstprivate:
    case sections_lastprivate:
      result->node.var_list = t;
      break;
    case sections_reduction:
      result->node.reduction_node.reduction_op = red_op;
      result->node.reduction_node.var_list = t;
      break;
    case sections_nowait:
      result->node.nowait = 0;
      break;
    default:
      Fail_FmtAssertion ("unexpected sections-clause-type");
  }

  return result;
}

///////// build clause list for SINGLE directive ////////// 


struct single_clause_list *
chain_single_list_on 
    (struct single_clause_list * sclause_list, struct single_clause_list * sclause)
{
	struct single_clause_list *scl;
    if (sclause_list) 
    	{
            for (scl = sclause_list; scl->next != NULL; scl = scl->next);
	        scl->next = sclause;
            return sclause_list;
    	}
    else  return sclause;
}

struct single_clause_list *
build_single_clause_list (tree t, single_clause_type s_type)
{
  // mark the end of the current clause
  seen_omp_paren = FALSE;

  struct single_clause_list * result = (single_clause_list *) malloc (sizeof (single_clause_list));

  result->type = s_type;
  result->next = NULL;

  switch (s_type)
  {
    case single_private:
    case single_firstprivate:
    case single_copyprivate:
      result->node.var_list = t;
      break;
    case single_nowait:
      result->node.nowait = 0;
      break;
    default:
      Fail_FmtAssertion ("Unexpected single-clause-type");
  }

  return result;
}

///////// build clause list for PARALLEL FOR directive ////////// 

struct parallel_for_clause_list *
chain_parallel_for_list_on 
    (struct parallel_for_clause_list * pfclause_list, struct parallel_for_clause_list * pfclause)
{
	struct parallel_for_clause_list *pfcl;
    if (pfclause_list) 
    	{
            for (pfcl = pfclause_list; pfcl->next != NULL; pfcl = pfcl->next);
	        pfcl->next = pfclause;
            return pfclause_list;
    	}
    else return pfclause;
}

struct parallel_for_clause_list *
build_parallel_for_clause_list (tree t, 
                                parallel_for_clause_type p_type, 
				default_type d_type, 
				schedule_kind_type s_kind, 
				reduction_op_type red_op)
{
  // mark the end of the current clause
  seen_omp_paren = FALSE;

  struct parallel_for_clause_list * result = (parallel_for_clause_list *) malloc (sizeof (parallel_for_clause_list));

  result->type = p_type;
  result->next = NULL;

  switch (p_type)
  {
    case p_for_if:
    case p_for_num_threads:
      result->node.expr_no_commas = t;
      break;
    case p_for_copyprivate:
    case p_for_shared:
    case p_for_copyin:
    case p_for_private:
    case p_for_firstprivate:
    case p_for_lastprivate:
      result->node.var_list = t;
      break;
    case p_for_default:
      result->node.defaulttype = d_type;
      break;
    case p_for_schedule_1:
      result->node.schedule_kind = s_kind;
      break;
    case p_for_schedule_2:
      result->node.schedule_node.schedule_kind = s_kind;
      result->node.schedule_node.chunk_size = t;
      break;
    case p_for_reduction:
      result->node.reduction_node.reduction_op = red_op;
      result->node.reduction_node.var_list = t;
      break;
    case p_for_ordered:
      result->node.ordered = 0;
      break;
    default:
      Fail_FmtAssertion ("Unexpected parallel-for-clause-type");
  }

  return result;
}

///////// build clause list for PARALLEL SECTIONS directive ////////// 


struct parallel_sections_clause_list *
chain_parallel_sections_list_on
    (struct parallel_sections_clause_list * psclause_list, struct parallel_sections_clause_list * psclause)
{
	struct parallel_sections_clause_list *pscl;
    if (psclause_list) 
    	{
            for (pscl = psclause_list; pscl->next != NULL; pscl = pscl->next);
	        pscl->next = psclause;
            return psclause_list;
    	}
    else  return psclause;
}

struct parallel_sections_clause_list *
build_parallel_sections_clause_list (tree t, parallel_sections_clause_type p_type, default_type d_type, reduction_op_type red_op)
{
  // mark the end of the current clause
  seen_omp_paren = FALSE;

  parallel_sections_clause_list * result = (parallel_sections_clause_list *) malloc (sizeof (parallel_sections_clause_list));

  result->type = p_type;
  result->next = NULL;

  switch (p_type)
  {
    case p_sections_if:
    case p_sections_num_threads:
      result->node.expr_no_commas = t;
      break;
    case p_sections_private:
    case p_sections_copyprivate:
    case p_sections_firstprivate:
    case p_sections_lastprivate:
    case p_sections_shared:
    case p_sections_copyin:
      result->node.var_list = t;
      break;
    case p_sections_default:
      result->node.defaulttype = d_type;
      break;
    case p_sections_reduction:
      result->node.reduction_node.reduction_op = red_op;
      result->node.reduction_node.var_list = t;
      break;
    default:
      Fail_FmtAssertion ("Unexpected parallel-sections-clause-type");
  }

  return result;
}

}//extern "C"
