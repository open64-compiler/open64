/*
 * Copyright 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
 */

/*

  Copyright (C) 2000, 2001 Silicon Graphics, Inc.  All Rights Reserved.

  This program is free software; you can redistribute it and/or modify it
  under the terms of version 2 of the GNU General Public License as
  published by the Free Software Foundation.

  This program is distributed in the hope that it would be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  

  Further, this software is distributed without any warranty that it is
  free of the rightful claim of any third person regarding infringement 
  or the like.  Any license provided herein, whether implied or 
  otherwise, applies only to this software file.  Patent licenses, if 
  any, provided herein do not apply to combinations of this program with 
  other software, or any other product whatsoever.  

  You should have received a copy of the GNU General Public License along
  with this program; if not, write the Free Software Foundation, Inc., 59
  Temple Place - Suite 330, Boston MA 02111-1307, USA.

  Contact information:  Silicon Graphics, Inc., 1600 Amphitheatre Pky,
  Mountain View, CA 94043, or:

  http://www.sgi.com

  For further information regarding this notice, see:

  http://oss.sgi.com/projects/GenInfo/NoticeExplan

*/


/* -*-Mode: c++;-*- (Tell emacs to use c++ mode) */
/* ====================================================================
 *
 * Module: DaVinci.h
 * $Revision: 1.6 $
 * $Date: 05/12/05 08:59:12-08:00 $
 * $Author: bos@eng-24.pathscale.com $
 * $Source: /scratch/mee/2.4-65/kpro64-pending/be/com/SCCS/s.com_whirlview.cxx $
 * ====================================================================
 */

#include <stdio.h>
#include <string.h>
#include <vector>

#include "defs.h"
#include "errors.h"
#include "srcpos.h"
#include "opcode.h"
#include "wn.h"		/* Whirl Node descriptor */
#include "wn_simp.h"    /* Whirl simplifier (can be stubs) */

// #include "stab.h"
// #include "const.h"
// #include "targ_const.h"
// #include "strtab.h"
// #include "wio.h"
// #include "wintrinsic.h"
// #include "wn_pragmas.h"

#include "DaVinci.h"
#include "wb_util.h"
#include "com_whirlview.h"
#include "fb_whirl.h"

#include "vcg.h"

#define MAXEXPR 5000

static MEM_POOL DV_wv_mempool;
static BOOL     DV_wv_mempool_init = FALSE;
static DaVinci *DV      = NULL;
static WN      *Func_wn = NULL;

static void draw_expr(WN *);
static void draw_stmt(WN *);


// Memory allocation for VCG graphs
static MEM_POOL VCG_pool;
static BOOL VCG_pool_init = FALSE; 
static int vcg_node_count = 0;

static char *
id_str(WN *wn)
{
  static char dv_id[64];
  INT         len;

 if ( wn ) {
   // don't need unique labels for display - node_id is unique.
   len = sprintf( dv_id, "%d:%d",
		  OPCODE_mapcat(WN_opcode(wn)), WN_map_id(wn) );
 } else {
   strcpy( dv_id, "NULL-WN" );
 }

 if ( Cur_PU_Feedback ) {
   FB_FREQ freq = Cur_PU_Feedback->Query_total_out( wn );
   freq.Sprintf( dv_id + len );
 }

 return dv_id;
}

static void
draw_expr(WN *wn)

{
  NODE_TYPE nt;
  EDGE_TYPE et;
  INT       i;

  DV->Node_Begin( NODE_ID(wn), id_str(wn), nt );

  for (i = 0; i < WN_kid_count(wn); i++) {
    WN *wn2 = WN_kid(wn,i);
    DV->Out_Edge( EDGE_ID(wn, wn2), et, NODE_ID(wn2) );
  }
  DV->Node_End();

  for (i = 0; i < WN_kid_count(wn); i++) {
    draw_expr( WN_kid(wn,i) );
  }
}

static void
draw_stmt(WN *wn, BOOL show_expr)
{
  NODE_TYPE nt;
  EDGE_TYPE et;

  vector<WN*> kids;

  switch (WN_opcode(wn)) {
  case OPC_BLOCK: {
    WN *wn2 = WN_first(wn);
    while (wn2) {
      kids.push_back( wn2 );
      wn2 = WN_next(wn2);
    }
    break;
  }
  case OPC_IF:
    nt.Shape( NS_RHOMBUS );
    if ( WN_then(wn) ) {
      kids.push_back( WN_then(wn) );  // THEN.
    }
    if ( show_expr ) {
      kids.push_back( WN_if_test(wn) ); // condition-expr.
    }
    if ( WN_else(wn) ) {
      kids.push_back( WN_else(wn) );  // ELSE.
    }
    break;

  case OPC_DO_LOOP:
    nt.Shape( NS_ELLIPSE );
    if ( show_expr ) kids.push_back( WN_index(wn) );   // INDX-expr.
    kids.push_back( WN_start(wn) );   // INIT.
    if ( show_expr ) kids.push_back( WN_end(wn) );     // COMP-expr.
    kids.push_back( WN_do_body(wn) ); // BODY.
    kids.push_back( WN_step(wn) );    // INCR.
    break;

  case OPC_COMPGOTO:
    if ( show_expr ) kids.push_back( WN_kid(wn,0) );   // SWCH-expr.
    kids.push_back( WN_kid(wn,1) );   // JMPS.
    if ( WN_kid_count(wn) > 2 ) {
      kids.push_back( WN_kid(wn,2) ); // DFLT.
    }
    break;

  case OPC_XGOTO:
    if ( show_expr ) kids.push_back( WN_kid(wn,0) );   // SWCH-expr.
    kids.push_back( WN_kid(wn,1) );   // JMPS.
    break;

  default: 
    {
      for (INT i = 0; i < WN_kid_count(wn); ++i) {
	WN* wn2 = WN_kid(wn,i);
	FmtAssert(wn2, ("Null kid in draw_stmt"));
	OPCODE opc2 = WN_opcode(wn2);
	if ( show_expr && OPCODE_is_expression(opc2) ) {
	  kids.push_back( wn2 );
	} else if ( OPCODE_is_stmt(opc2) || OPCODE_is_scf(opc2) ) {
	  kids.push_back( wn2 );
	}
      }
    }
  }


  if ( Cur_PU_Feedback ) {
    FB_FREQ freq = Cur_PU_Feedback->Query_total_out( wn );
    if ( freq.Known() ) {
      nt.Color( "wheat1" );
    }
  }

  DV->Node_Begin( NODE_ID(wn), id_str(wn), nt );

  vector<WN*>::iterator wn_iter;

  for (wn_iter = kids.begin(); wn_iter != kids.end(); ++wn_iter) {
    WN *wn2 = *wn_iter;

    DV->Out_Edge( EDGE_ID(wn,wn2), et, NODE_ID(wn2) );
  }
  DV->Node_End();

  for (wn_iter = kids.begin(); wn_iter != kids.end(); ++wn_iter) {
    WN *wn2 = *wn_iter;

    draw_stmt( wn2, show_expr );
  }
}

static void
draw_whirl_tree(WN *wn, BOOL show_expr)
{
  DV->Graph_Begin();

  if ( OPCODE_is_expression(WN_opcode(wn)) ) {
    draw_expr( wn );
  } else if ( OPCODE_is_stmt(WN_opcode(wn))
	      || OPCODE_is_scf(WN_opcode(wn)) ) {
    draw_stmt( wn, show_expr );
  } else {
    FmtAssert( FALSE, ("opcode of unknown type") );
  }
  DV->Graph_End();
}

class Callback : public DaVinci_Callback {
  WN *node_sel;  // last node selected (or NULL).
public:
  virtual void Node_Select(const INT n_ids, const NODE_ID id_array[]);
  virtual void Edge_Select(const EDGE_ID& id);
  virtual void Menu_Select(const char *menu_id);
};

void
Callback::Node_Select(const INT n_ids, const NODE_ID id_array[])
{
  char buf[ MAXEXPR ]; // more: better storage management.

  for (INT i = 0; i < n_ids; ++i) {
    WN *wn      = (WN *)id_array[i];
    WN *head_wn = ( Func_wn ? Func_wn : wn );
    INT end = WB_Dump_Whirl_Expr(head_wn, wn, buf, 0);
    buf[end] = '\0';
    printf("%p: %s\n", wn, buf);
  }
  node_sel = (n_ids > 0 ? (WN *)id_array[n_ids - 1] : NULL);
}

void
Callback::Edge_Select(const EDGE_ID& id)
{
  EDGE_TYPE et;

  et.Color( "red" );
  DV->Change_Attr( id, et );  // just to ack edge select event ..
}

void
Callback::Menu_Select(const char *menu_id)
{
  if ( strcmp( menu_id, "EXPAND" ) == 0 && node_sel ) {
    printf("selecting EXPAND for node %s\n", id_str(node_sel));
    // MORE: use graph update capability to expand tree.
    //       need to track which nodes have already been plotted.
  }
}


static MENU_INFO DV_Menu[] = {
  { "EXPAND", "Expand Subtree", true, 0, NULL }
  // more: add other useful queries here.
};
#define N_DV_MENU   ( sizeof(DV_Menu) / sizeof(DV_Menu[0]) )

// more: consider du_mgr ..?

void
dV_view_whirl(WN *wn, const char *title, BOOL show_expr, FILE *trace_fp)
{
  if ( ! DaVinci::enabled(true) ) return;

  DaVinci  dv(&DV_wv_mempool, trace_fp);
  Func_wn = (WN_operator(wn) == OPR_FUNC_ENTRY ? wn : NULL);

  const char *trace_fname = getenv("DV_TRACE_FILE");
  bool        local_trace = false;

  if ( trace_fp == NULL && trace_fname ) {
    if ( (trace_fp = fopen(trace_fname, "w")) != NULL ) {
      local_trace = true;
    } else {
      fprintf(stderr, "DV_TRACE_FILE not writeable\n");
      perror( trace_fname );
    }
  }
  FmtAssert( DV == NULL, ("dV_view_fb_cfg: DV is null"));
  if ( ! DV_wv_mempool_init ) {
    MEM_POOL_Initialize(&DV_wv_mempool, "DV_wv_mempool", FALSE);
    DV_wv_mempool_init = TRUE;
  }

  DV=&dv;
  const char *window_title = (title ? title : "com_whirlview tree display");
  DV->Title( window_title  );
  draw_whirl_tree( wn, show_expr );
  DV->Menu_Create( N_DV_MENU, DV_Menu );

  Callback callback;
  DV->Event_Loop( &callback );

  DV      = NULL;
  Func_wn = NULL;

  if ( local_trace ) {
    (void)fclose( trace_fp );
  }
}

#include <sstream>
#include <string>
using namespace std; 

// method to dump the vcg graph of a function.

#define NEW_VCG(x)  CXX_NEW(x,&VCG_pool)

// external methods 

extern char *image_st(MEM_POOL *pool, ST_IDX st_idx);
extern char *image_ty(MEM_POOL *pool, ST_IDX st_idx);
extern char *image_wn(MEM_POOL *pool, WN *wn);
extern char *image_stmt(MEM_POOL *pool, WN *wn);
extern char *image_WN_TREE_stmt(MEM_POOL *pool, WN *wn);
extern char *image_expr(MEM_POOL *pool, WN *wn);
extern char *image_WN_TREE_expr(MEM_POOL *pool, WN *wn);
extern char *image_lineno(MEM_POOL *pool, WN *wn);

extern void help_image_lineno(stringstream &ss, WN *wn);
extern void help_image_st (stringstream &ss, ST_IDX st_idx);
extern void help_image_ty(stringstream &ss, TY_IDX ty);
extern void help_image_expr(stringstream &ss, WN * wn, INT indent);
extern void help_image_wn(stringstream &ss, WN *wn, INT indent);
extern void help_image_stmt(stringstream &ss, WN * wn, INT indent);
extern void help_WN_TREE_image_stmt(stringstream &ss, WN *wn, int indent);
extern void help_WN_TREE_image_expr(stringstream &ss, WN * wn, INT indent);

VCGNode *vcg_whirl_tree(VCGGraph &vcg, WN *wn);
static VCGNode *vcg_stmt(VCGGraph &vcg, WN *wn);

static 
const char *
get_unique_name(char *prefix)
{
  stringstream ss;
  if (prefix)
    ss << prefix;
  ss << vcg_node_count;
  vcg_node_count++;
  char *str  = (char *) MEM_POOL_Alloc(&VCG_pool, strlen(ss.str().data()) + 1);
  
  // copy string into mempool.
  strcpy(str, ss.str().data()); 
  return str; 
}
  

static
const char *lineno(WN *wn, char *suffix = NULL)
{ 
  stringstream ss;
  ss << "line ";
  help_image_lineno(ss,wn);
  if (suffix)
    ss << suffix;
  char *str  = (char *) MEM_POOL_Alloc(&VCG_pool, strlen(ss.str().data()) + 1);
  // copy string into mempool.
  strcpy(str, ss.str().data()); 
  return str; 
} 

static 
const char *node_title(WN *wn)
{ 
  return image_wn(&VCG_pool, wn);
}

static 
const char *node_name(WN *wn, char *prefix = NULL)
{ 
  stringstream ss;

  WN *func_wn = (WN_operator(wn) == OPR_FUNC_ENTRY ? wn : NULL);
  if (func_wn)
  { 
     return  image_st(&VCG_pool, WN_entry_name(func_wn));
  } 

  if (WN_opcode(wn) == OPC_LABEL)
  { 
      ss << "L";
      ss << WN_label_number(wn);
      char *str  = (char *) MEM_POOL_Alloc(&VCG_pool, strlen(ss.str().data()) + 1);
  
      // copy string into mempool.
      strcpy(str, ss.str().data()); 
      return str; 
  }

  if (prefix)
    ss << prefix;
  switch (WN_opcode(wn)) {
    case OPC_BLOCK: 
      ss << "B ";
      break;
    case OPC_IF:
      ss << "IF ";
      break;
    case OPC_DO_WHILE:
      ss << "DO WHILE";
      break;;
    case OPC_WHILE_DO:
      ss << "WHILE DO";
      break;
    case OPC_DO_LOOP:
      ss << "DO_LOOP";
      break;
    case OPC_COMPGOTO:
      ss << "COMPGOTO ";
      break;
    case OPC_XGOTO:
      ss << "XGOTO ";
      break;
    default:
      if (OPCODE_is_stmt(WN_opcode(wn)))
         ss << "S ";
      else if (OPCODE_is_expression(WN_opcode(wn)))
         ss << "E ";
      else
         ss << "WN ";
      break;
  }

  ss << id_str(wn);
  char *str  = (char *) MEM_POOL_Alloc(&VCG_pool, strlen(ss.str().data()) + 1);
  
  // copy string into mempool.
  strcpy(str, ss.str().data()); 
  return str; 
}


// return info associated with a loop, such as index variable, init, step and compare expressions.

char *image_loopinfo(WN *wn)
{ 
  stringstream ss;

  if (WN_opcode(wn) == OPC_DO_LOOP)
  { 

     ss << "INDEX:\n";
     help_image_expr(ss, WN_index(wn),1);
     ss << "INIT:\n";
     help_image_stmt(ss, WN_start(wn),1);
     ss << "COMP:";
     help_image_expr(ss, WN_end(wn), 1);
     ss << "INCR:";
     help_image_stmt(ss, WN_step(wn),1);
     if ( WN_do_loop_info(wn) != NULL ) {
       help_image_stmt(ss,WN_do_loop_info(wn), 1);
     }     
  }
  else
  { 
     ss << "not a loop";
  } 
  
      
  char *str  = (char *) MEM_POOL_Alloc(&VCG_pool, strlen(ss.str().data()) + 1);
  
  // copy string into mempool.
  strcpy(str, ss.str().data()); 
  return str; 
} 

static
const char *get_last_node_title(VCGNode *node, WN *wn)
{ 
  if (wn == NULL) 
    return node->title(); 
  switch(WN_opcode(wn))
  { 
    case OPC_BLOCK: 
      return node_name(wn, "END");
    case OPC_IF:
      return node_name(wn, "ENDIF");
    case OPC_DO_LOOP:
      return node_name(wn, "ENDLOOP");
  default:
    if (OPCODE_is_scf(WN_opcode(wn)))
    { 
      return node_name(wn, "ENDSCF");
    } 
    return node->title(); 
  } 
} 
char *image_funcinfo(WN *wn)
{ 
  WN *func_wn = (WN_operator(wn) == OPR_FUNC_ENTRY ? wn : NULL);
  if (!func_wn) return "null";
  stringstream ss; 
  ss << "FUNC NAME: ";
  help_image_st(ss, WN_entry_name(func_wn));
  ss << endl; 
  INT num_formals = WN_num_formals(func_wn);
  if (num_formals > 0)
  { 
     ss << "FORMALS: ";
     INT i; 
     for (i = 0; i < num_formals; i++) { 
        help_image_wn(ss, WN_formal(func_wn, i), 0);
        ss << " ";
     }
     ss << endl;
  }
  ss << "PRAGMAS: ";
  help_image_wn(ss,WN_func_pragmas(func_wn),1);
  ss << endl;

  char *str  = (char *) MEM_POOL_Alloc(&VCG_pool, strlen(ss.str().data()) + 1);
  
  // copy string into mempool.
  strcpy(str, ss.str().data()); 
  return str; 
} 


extern BOOL follow_st;
void
vcg_wn(WN *wn, char *filename)
{
  MEM_POOL_Initialize(&VCG_pool, "VCG_mempool", FALSE);
  VCG_pool_init = TRUE;
  follow_st = TRUE; 
  WN *func_wn = (WN_operator(wn) == OPR_FUNC_ENTRY ? wn : NULL);
  VCGGraph vcg("VCG of whirl");
  vcg.infoName(1, "Whirl IR");

  if (!func_wn)
  {

     vcg_whirl_tree( vcg, wn);
     FILE *vcgfile = fopen(filename, "w");
     Is_True(vcgfile != NULL, ("Couldn't open vcgfile for writing"));
     vcg.emit(vcgfile);
     fclose(vcgfile);
  }
  else
  {
     const char *name = node_name(wn);
     VCGNode *funcnode = NEW_VCG(VCGNode(name, node_title(wn),Ellipse));
     vcg.addNode(*funcnode);
     char *funcinfo = image_funcinfo(wn);
     funcnode->info(1,funcinfo);
     VCGNode *body = vcg_whirl_tree(vcg, WN_func_body(wn));
     VCGEdge *edge = NEW_VCG(VCGEdge(funcnode->title(), body->title()));
     edge->label("BODY");
     vcg.addEdge(*edge);
     FILE *vcgfile = fopen(filename, "w");
     Is_True(vcgfile != NULL, ("Couldn't open vcgfile for writing"));
     vcg.emit(vcgfile);
     fclose(vcgfile);
  }
  MEM_POOL_Delete(&VCG_pool); 
}

VCGNode *
vcg_whirl_tree(VCGGraph &vcg, WN *wn)
{ 
  if ( OPCODE_is_expression(WN_opcode(wn)) ) {
    Is_True( FALSE, ("expr vcg not supported ") );
    return NULL; 
  } else if ( OPCODE_is_stmt(WN_opcode(wn))
	      || OPCODE_is_scf(WN_opcode(wn)) ) {
    return vcg_stmt( vcg, wn);
  } else {
    Is_True( FALSE, ("opcode of unknown type") );
  }
  return NULL;
} 

BOOL
is_uncond_branch(WN *wn)
{ 
  return (wn && (WN_opcode(wn) == OPC_GOTO));
} 

static
VCGNode *
vcg_stmt(VCGGraph &vcg, WN *wn)
{
  vector<WN*> kids;
  stringstream ss; 
  switch (WN_opcode(wn)) {
  case OPC_BLOCK: {
    const char *vcg_blk_name = get_unique_name("BLOCK ");
    VCGGraph *vcg_blk = NEW_VCG(VCGGraph(vcg_blk_name));
    vcg_blk->color(LightGreen);
    vcg_blk->textColor(Blue);
    const char *name = node_name(wn, "BEGIN");
    VCGNode *blk = NEW_VCG(VCGNode(name, "BEGIN BLOCK", Ellipse));
    vcg.addSubGraph(*vcg_blk);
    vcg_blk->addNode(*blk);
    WN *wn2 = WN_first(wn);
    VCGNode *prev = blk;
    WN *prev_wn = NULL; 
    while (wn2) {
      VCGNode *node = vcg_stmt(*vcg_blk,wn2);
      if (!prev_wn || !is_uncond_branch(prev_wn))
      {
        VCGEdge *edge = NEW_VCG(VCGEdge(get_last_node_title(prev, prev_wn),
                                      node->title()));
        vcg_blk->addEdge(*edge);
      }
      prev_wn = wn2;
      wn2 = WN_next(wn2);
      prev = node;
    }
    const char *endname = node_name(wn, "END");
    VCGNode *endblk = NEW_VCG(VCGNode(endname, "END BLOCK", Ellipse));
    vcg_blk->addNode(*endblk);
    VCGEdge *edge = NEW_VCG(VCGEdge(prev->title(),endblk->title()));
    vcg_blk->addEdge(*edge);
    return blk; 
    break;
  }
  case OPC_IF:
  {
    // create a new subgraph for the IF_ELSE tree and
    // keep it folded. 

    const char *if_title = get_unique_name("IF-THEN-ELSE ");
    VCGGraph *if_vcg = NEW_VCG(VCGGraph(if_title));
    if_vcg->color(LightGreen);
    if_vcg->textColor(Blue);
    vcg.addSubGraph(*if_vcg);
    const char *name = node_name(wn, "IF");

    VCGNode *ifn = NEW_VCG(VCGNode(name, node_title(wn),Rhombus));
    if_vcg->addNode(*ifn);
    const char *end = node_name(wn, "ENDIF");
    VCGNode *endifn = NEW_VCG(VCGNode(end, "end if",Rhombus));
    if_vcg->addNode(*endifn);
    if ( WN_then(wn) ) {
      VCGNode *then_vcg = vcg_stmt(*if_vcg, WN_then(wn));
      const char *then_last_title = get_last_node_title(then_vcg, WN_then(wn));
      VCGEdge *edge = NEW_VCG(VCGEdge(ifn->title(),then_vcg->title()));
      if_vcg->addEdge(*edge);
      edge->label("then");
      // add an edge between then block and endif
      VCGEdge *edge2 = NEW_VCG(VCGEdge(then_last_title,endifn->title()));
      if_vcg->addEdge(*edge2);
    }
    if ( WN_else(wn) ) {
      VCGNode *else_vcg = vcg_stmt(*if_vcg, WN_else(wn));
      const char *else_last_title = get_last_node_title(else_vcg, WN_else(wn));
      VCGEdge *edge = NEW_VCG(VCGEdge(ifn->title(),else_vcg->title()));
      if_vcg->addEdge(*edge);
      edge->label("else");
      // add an edge between else block and endif
      VCGEdge *edge2 = NEW_VCG(VCGEdge(else_last_title,endifn->title()));
      if_vcg->addEdge(*edge2);
    }
    else
    {
      // add an edge between if block and endif
      VCGEdge *edge2 = NEW_VCG(VCGEdge(ifn->title(),endifn->title()));
      if_vcg->addEdge(*edge2);
      edge2->label("fall-thru");
    }

    // Let us add the test information to the if block.
    char *str = image_expr(&VCG_pool, WN_if_test(wn));
    ifn->info(1,str);
    return ifn; 
    break;
  }

  case OPC_LABEL:
  case OPR_LABEL:
  { 
     const char *name = node_name(wn);
     VCGNode *label = NEW_VCG(VCGNode(name));
     vcg.addNode(*label);
     return label;
  } 
  case OPC_TRUEBR:
  case OPC_FALSEBR:
  case OPR_TRUEBR:
  case OPR_FALSEBR:
  case OPC_GOTO:
  {
     const char *name = node_name(wn, "BR");
     VCGNode *br = NEW_VCG(VCGNode(name, node_title(wn), Rhombus));
     if (WN_opcode(wn) != OPC_GOTO)
     { 
       char *test_expr = image_expr(&VCG_pool, WN_kid(wn,0)); 
       br->info(1,test_expr);
     }
     vcg.addNode(*br);
     char str[32];
     sprintf(str, "L%d", WN_label_number(wn));
     char *mem  = (char *) MEM_POOL_Alloc(&VCG_pool, strlen(str) + 1);
     // copy string into mempool.
     strcpy(mem, str); 

     VCGEdge *edge = NEW_VCG(VCGEdge(br->title(), mem));
     vcg.addEdge(*edge);
     if (WN_opcode(wn) == OPC_TRUEBR)
         edge->label("true");
     if (WN_opcode(wn) == OPC_FALSEBR)
         edge->label("false");
       return br;
     break;
  } 

  case OPC_DO_LOOP:
  { 
    const char *loop_title = get_unique_name("LOOP SUBGRAPH ");
    VCGGraph *loop_vcg = NEW_VCG(VCGGraph(loop_title));
    loop_vcg->color(LightGreen);
    loop_vcg->textColor(Blue);
    vcg.addSubGraph(*loop_vcg);
    const char *name = node_name(wn, "LOOP");
    VCGNode *loop = NEW_VCG(VCGNode(name, "LOOP", Ellipse));
    loop_vcg->addNode(*loop);
    const char *endname = node_name(wn, "ENDLOOP");
    VCGNode *endloop = NEW_VCG(VCGNode(endname, "END LOOP", Ellipse));
    loop_vcg->addNode(*endloop);
    VCGNode *body = vcg_stmt(*loop_vcg, WN_do_body(wn));     
    char *data = image_loopinfo(wn);
    VCGEdge *edge = NEW_VCG(VCGEdge(loop->title(),body->title()));
    edge->label("loop-body");
    loop_vcg->addEdge(*edge);
    loop->info(1, data);
    VCGEdge *endedge = NEW_VCG(VCGEdge(get_last_node_title(body, WN_do_body(wn)), endloop->title()));
    loop_vcg->addEdge(*endedge);
    return loop;
    break;
  }
  case OPC_COMPGOTO: 
  { 
     const char *name = node_name(wn);
     VCGNode *compgoto = NEW_VCG(VCGNode(name, node_title(wn), Rhombus));
     vcg.addNode(*compgoto);
     char *data = image_wn(&VCG_pool, WN_kid(wn,0)); 
     compgoto->info(1, data);
     VCGNode *target = vcg_stmt(vcg, WN_kid(wn,1));
     VCGEdge *edge = NEW_VCG(VCGEdge(compgoto->title(), target->title()));
     edge->label("goto-target");
     if ( WN_kid_count(wn) > 2 ) {
        VCGNode *deftarget = vcg_stmt(vcg, WN_kid(wn,2));
        VCGEdge *edge = NEW_VCG(VCGEdge(compgoto->title(), deftarget->title()));
        edge->label("default-target");
        vcg.addEdge(*edge);
     }
     return compgoto;
     break;
  }
  case OPC_XGOTO:
  { 
    const char *name = node_name(wn);
    VCGNode *xgoto = NEW_VCG(VCGNode(name, node_title(wn), Rhombus));
    vcg.addNode(*xgoto);
    char *data = image_wn(&VCG_pool, WN_kid(wn,0) );   
    VCGNode *target = vcg_stmt(vcg, WN_kid(wn,1));
    VCGEdge *edge = NEW_VCG(VCGEdge(xgoto->title(), target->title()));
    edge->label("goto-target");
    return xgoto;
    break;
  }

  default: 
  {
    if (OPCODE_is_stmt(WN_opcode(wn)))
    { 
      const char *name = node_name(wn);
      VCGNode *vcgnode = NEW_VCG(VCGNode(name, node_title(wn)));
      vcg.addNode(*vcgnode);
      char *data = image_stmt(&VCG_pool, wn);
      vcgnode->info(1,data);
      return vcgnode; 
    }

    if (OPCODE_is_scf(WN_opcode(wn)))
    {
      const char *scfname = node_name(wn, "SCF");
      const char *scf_title = image_wn(&VCG_pool,wn);
      VCGGraph *scf_vcg = NEW_VCG(VCGGraph(scf_title));
      scf_vcg->color(LightGreen);
      scf_vcg->textColor(Blue);
      vcg.addSubGraph(*scf_vcg);
      VCGNode *vcgnode = NEW_VCG(VCGNode(scfname, scf_title, Ellipse));
      scf_vcg->addNode(*vcgnode);
      const char *endscfname = node_name(wn, "ENDSCF"); 
      VCGNode *endnode = NEW_VCG(VCGNode(endscfname, "END SCF", Ellipse));
      scf_vcg->addNode(*endnode);

      vcgnode->backGroundColor(Green);
      endnode->backGroundColor(Green); 

      stringstream ss;
      help_image_wn(ss,wn,0);
      OPCODE opc2;
      INT i;
      for (i = 0; i < WN_kid_count(wn); i++) {
        WN *wn2 = WN_kid(wn,i);
        if (wn2) {
	    opc2 = WN_opcode(wn2);
	    if (opc2 == 0) {
	      ss << "### error: WN opcode 0\n";
	    } else if (OPCODE_is_expression(opc2)) {
	      help_image_expr(ss,wn2, 1);
	    }
	    else if (OPCODE_is_stmt(opc2) || OPCODE_is_scf(opc2)) {

		VCGNode *stmt_vcg = vcg_stmt(*scf_vcg,wn2);
                // add an edge from wn to stmt.
                VCGEdge *edge = NEW_VCG(VCGEdge(vcgnode->title(), stmt_vcg->title()));
                edge->label("kid");
                // add an edge from last node of wn2 to the scf end node. 
                VCGEdge *endedge = NEW_VCG(VCGEdge(get_last_node_title(stmt_vcg,wn2), endnode->title()));
                scf_vcg->addEdge(*edge);
                scf_vcg->addEdge(*endedge);
	    }
	}
      }

      // attach the expr string. 
      char *str  = (char *) MEM_POOL_Alloc(&VCG_pool, strlen(ss.str().data()) + 1);
      // copy string into mempool.
      strcpy(str, ss.str().data()); 
      vcgnode->info(1,str);
      return vcgnode; 
    }
	
  }
  }
}
