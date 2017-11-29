/*
 * Copyright (C) 2010 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

/*
 * Copyright 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
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



/* ====================================================================
 *
 * Module: ipl_summary_print.cxx
 * $Source: /proj/osprey/CVS/open64/osprey1.0/ipa/local/ipl_summary_print.cxx,v $
 *
 * Description:
 *	all the print functions for summary info.
 *
 * ====================================================================
 * ====================================================================
 */

#include <stdint.h>
#if defined(BUILD_OS_DARWIN)
#include <darwin_elf.h>                // ipl_summary.h needs it
#else /* defined(BUILD_OS_DARWIN) */
#include <elf.h>                // ipl_summary.h needs it
#endif /* defined(BUILD_OS_DARWIN) */

#include "defs.h"
#include "strtab.h"             // Current_Strtab
#include "ipl_summary.h"        // SUMMARY_* classes
#include "ipa_be_summary.h"     // SUMMARY_CONSTRAINT_* classes
#include "ipc_symtab_merge.h"   // IPC_GLOBAL_IDX_MAP
#include "ipl_tlog.h"


SUMMARY_SYMBOL *Ipl_Summary_Symbol;
const INT MODREF_MAX_STRING_LENGTH = 1000;
char    Modref_Buf[MODREF_MAX_STRING_LENGTH];
BOOL    IPA_Trace_Mod_Ref = FALSE;  /* Trace log for Mod_Ref */                


// SUMMARY_PROCEDURE:  Print / Trace
void
SUMMARY_PROCEDURE::Print (FILE *fp, INT32 id) const
{
    fprintf (fp, "PROCEDURE[%d] bb_count=%d, stmt_count=%d, %d formals (%d),"
	     " state=0x%x", id, Get_bb_count(), Get_stmt_count(),
	     Get_formal_count(), Get_formal_index (), Get_state() ); 

    if ( Is_may_inline() )
	fputs (", may inline", fp);

    if ( Is_must_inline() )
	fputs (", must inline", fp);

    if ( Is_no_inline() )
	fputs (", no inline", fp);

    if ( Is_varargs() )
	fputs (", varargs", fp);

    if ( Is_alt_entry() )
	fputs (", alternate entry", fp);

    if ( Has_alt_entry() )
	fprintf ( fp, ", has %d alternate entries", Get_altentry_count() );

    if ( Has_pstatic() )
	fputs (", has local statics", fp);

    if (Is_direct_mod_ref() )
	fputs (", has direct mods and refs", fp);

    if (Is_exc_inline() )
      fputs (", has exceptions", fp);

    if (Is_no_delete() )
      fputs (", no delete", fp);

    if (Is_block_data() )
      fputs (", block data", fp);

    if (Has_addr_taken_reset() )
      fputs (", addr_taken reset", fp);

    if (Has_PU_freq() )
      fputs (", PU freq reset", fp);

    if (Has_formal_pragma() )
      fputs (", formal pragma", fp);

    if (Has_parallel_pragma() )
      fputs (", parallel pragma", fp);

    if (Has_parallel_region_pragma() )
      fputs (", parallel region pragma", fp);

    if (Has_fstatic() )
      fputs (", fstatic", fp);

    if (Has_unknown_calls() )
      fputs (", unknown calls", fp);

    if (Has_incomplete_array_info () )
      fputs (", incomplete array info", fp);

    if (Has_mp_needs_lno() )
      fputs (", mp needs lno", fp);

#ifdef KEY
    if (Has_pragma_side_effect() )
#else
    if (Has_side_effect() )
#endif
      fputs (", has side effect pragmas", fp);

    if (Has_messy_regions() )
      fputs (", has messy array regions", fp);

    if (Has_unstructured_cflow())
      fputs (", has unstructured cflow \n", fp);

    fprintf ( fp, "\n\t\t%d globals (%d), %d callsites (%d), %d ctrl-dep (%d), %d array-cfg(%d)",
	     Get_global_count(), Get_global_index(), Get_callsite_count(),
	     Get_callsite_index(), Get_ctrl_dep_count (),
	     Get_ctrl_dep_index (), Get_array_section_count(), Get_array_section_index()); 

    fprintf ( fp, "\n\t\t%d common_count (%d) feedback (%d)\n\t\t",
	     Get_common_count(), Get_common_index(), Get_feedback_index());

    fprintf(fp, "VALUE[%d:%d] EXPR[%d:%d]\n\t\t", Get_ex_value_index(), 
      Get_ex_value_count(), Get_ex_expr_index(), Get_ex_expr_count());

    if ( Ipl_Summary_Symbol ) {
	Ipl_Summary_Symbol[Get_symbol_index()].Print (fp);
    }

    // For the Nystrom alias analyzer
    fprintf(fp, "constraint graph nodes count: %d idx: %d\n",
            Get_constraint_graph_nodes_count(),
            Get_constraint_graph_nodes_idx());
    fprintf(fp, "constraint graph edges count: %d idx: %d\n",
            Get_constraint_graph_edges_count(),
            Get_constraint_graph_edges_idx());
    fprintf(fp, "constraint graph stinfos count: %d idx: %d\n",
            Get_constraint_graph_stinfos_count(),
            Get_constraint_graph_stinfos_idx());
    fprintf(fp, "constraint graph callsites count: %d idx: %d\n",
            Get_constraint_graph_callsites_count(),
            Get_constraint_graph_callsites_idx());
    fprintf(fp, "constraint graph node ids count: %d idx: %d\n",
            Get_constraint_graph_node_ids_count(),
            Get_constraint_graph_node_ids_idx());
    fprintf(fp, "constraint graph formal parm count: %d idx: %d\n",
            Get_constraint_graph_formal_parm_count(),
            Get_constraint_graph_formal_parm_idx());
    fprintf(fp, "constraint graph formal ret count: %d idx: %d\n",
            Get_constraint_graph_formal_ret_count(),
            Get_constraint_graph_formal_ret_idx());
} // SUMMARY_PROCEDURE::Print


void
SUMMARY_PROCEDURE::Trace ( INT32 id ) const
{
    Print ( TFile, id );
}

void 
SUMMARY_PROCEDURE::Print_array (FILE* fp, INT32 size ) const
{
    INT i;
    
    fprintf ( fp, "%sStart procedure array\n%s", SBar, SBar );
    for ( i=0; i<size; ++i )
	this[i].Print ( fp, i );
    fprintf ( fp, "%sEnd procedure array\n%s", SBar, SBar );
}


void
SUMMARY_PROCEDURE::Trace_array ( INT32 size ) const
{
    Print_array ( TFile, size );
}



void
SUMMARY_FEEDBACK::Print (FILE *f) const
{
    fprintf (f, "cycle = ");
    Get_cycle_count().Print(f);
    fprintf (f, ", freq. = ");
    Get_frequency_count().Print(f);
    fprintf (f, ", bb_count = %d, stmt_count = %d\n",
	     Get_effective_bb_count (), Get_effective_stmt_count ());
} // SUMMARY_FEEDBACK::Print


void
SUMMARY_FEEDBACK::Print_array (FILE *f, INT32 size) const
{
    fprintf (f, "%sStart feedback array\n%s", SBar, SBar);
    for (INT i = 0; i < size; i++) {
	fprintf (f, "FEEDBACK[%d]: ", i);
	this[i].Print (f);
    }
    fprintf (f, "%sEnd feedback array\n%s", SBar, SBar );
    
} // SUMMARY_FEEDBACK::Print_array 

// ====================================================================
//
// SUMMARY_CALLSITE:: Print / Trace
//
// Trace a callsite node.
//
// ====================================================================

void
SUMMARY_CALLSITE::Print (FILE* f) const
{
    fprintf (f, "%d actuals (@%d), map_id = %d%s",
	     Get_param_count(), Get_actual_index(),
	     Get_map_id(), Is_func_ptr() ? ", FPTR" : "" );

    if (Is_must_inline())
	fputs ("pragma inline \n", f);
    if (Is_no_inline())
	fputs ("pragma no inline \n", f);
    if (Is_intrinsic ()) {
	fputs (", intrinsic\n", f);
    } else if (Is_func_ptr ())
	fprintf (f, ": VALUE[%d]\n", Get_value_index ());
    else if (Ipl_Summary_Symbol) {
	fputs (": ", f);
	Ipl_Summary_Symbol[Get_symbol_index()].Print (f);
    }
    if (_constraint_graph_callsite_id != 0)
      fprintf (f, " cg callsite_id: %d\n", _constraint_graph_callsite_id);
}

// ====================================================================

void
SUMMARY_CALLSITE::Trace () const
{
    Print ( TFile );
}

// ====================================================================
//
// SUMMARY_CALLSITE:: Print_array / Trace_array
//
// Trace the array of callsite nodes starting with this of the given
// length.
//
// ====================================================================

void
SUMMARY_CALLSITE::Print_array (FILE* f, INT32 size) const
{
    INT i;

    fprintf ( f, "%sStart callsite array\n%s", SBar, SBar );
    for ( i=0; i<size; ++i ) {
	fprintf ( f, "callsite[%d] : ", i );
	this[i].Print ( f );
    }
    fprintf ( f, "%sEnd callsite array\n%s", SBar, SBar );
}

// ====================================================================

void
SUMMARY_CALLSITE::Trace_array ( INT32 size ) const
{
    Print_array ( TFile, size );
}



//-----------------------------------------------------------
// Trace formal nodes
//-----------------------------------------------------------

void
SUMMARY_FORMAL::Print ( FILE* fp ) const
{
    fprintf ( fp, "pos [%d]: ", Get_position() );
    fprintf ( fp, "REGION[%d] ", Get_region_index());
    fprintf ( fp, "MTYPE(%s) ", MTYPE_name(Get_machine_type()));
    fprintf ( fp, "ty_idx = 0x%x ", Get_ty());
    if (Is_ref_parm ())
	fputs ("by_reference ", fp);
    if (Is_var_dim_array ())
	fputs ("var_dim_array ", fp);
    if (Ipl_Summary_Symbol)
        Ipl_Summary_Symbol[Get_symbol_index()].Print (fp);
}

//-----------------------------------------------------------

void
SUMMARY_FORMAL::Trace ( void ) const
{
    Print ( TFile );
}

//-----------------------------------------------------------

void
SUMMARY_FORMAL::Print_array (FILE* fp, INT32 size) const
{
    INT i;

    fprintf ( fp, "%sStart formal array\n%s", SBar, SBar );
    for ( i=0; i<size; ++i ) {
	fprintf ( fp, "FORMAL[%d]: ", i );
	this[i].Print ( fp );
  }
  fprintf ( fp, "%sEnd formal array\n%s", SBar, SBar );
}

//-----------------------------------------------------------

void
SUMMARY_FORMAL::Trace_array ( INT32 size ) const
{
    Print_array ( TFile, size );
}


const char *
SUMMARY_ACTUAL::Pass_type_name (void) const
{
    switch (Get_pass_type()) {
    case PASS_UNKNOWN:
	return "PASS_UNKNOWN";
    case PASS_LDID:
	return "PASS_LDID";
    case PASS_LOAD:
	return "PASS_LOAD";
    case PASS_MLOAD:
	return "PASS_MLOAD";
    case PASS_LDA:
	return "PASS_LDA";
    case PASS_ARRAY_SECTION:
        return "PASS_ARRAY_SECTION"; 
    }
    return 0;
} // SUMMARY_ACTUAL::Pass_type_name


void
SUMMARY_ACTUAL::Print (FILE *f, INT32 id) const
{
    fprintf (f, "ACTUAL[%d]: ", id);
    if (Ipl_Summary_Symbol && Get_symbol_index () != -1)
	Ipl_Summary_Symbol[Get_symbol_index()].Print (f);

    if (Is_value_parm())
	fprintf (f, " value_parm, ");

    fprintf (f, " ty_idx = 0x%x, ", Get_ty());

    const char *p = Pass_type_name ();
    if (p)
	fprintf (f, " %s, ", p);
    else
	fprintf (f, " PASS_%d, ", Get_pass_type());
    if (Get_pass_type() == PASS_ARRAY_SECTION)
      fprintf (f, " Section Idx =  %d", Get_index());
    else
      fprintf (f, " Scalar Idx =  %d", Get_index());
    fprintf (f, " Value Idx = %d \n", Get_value_index ());
	
} // SUMMARY_ACTUAL::Print

void
SUMMARY_ACTUAL::Print_array (FILE *f, INT32 size) const
{
    fprintf (f, "%sStart actual array\n%s", SBar, SBar);
    for (INT i = 0; i < size; i++)
	this[i].Print (f, i);
    fprintf (f, "%sEnd actual array\n%s", SBar, SBar );
    
} // SUMMARY_ACTUAL::Print_array



const char *
SUMMARY_VALUE::Const_type_name (void) const
{
    switch (Get_const_type()) {
    case VALUE_UNKNOWN:
	return "VALUE_UNKNOWN";
    case VALUE_INT_CONST:
	return "VALUE_INT_CONST";
    case VALUE_TWO_CONSTS:
	return "VALUE_TWO_CONSTS";
    case VALUE_CONST:
	return "VALUE_CONST";
    case VALUE_FORMAL:
	return "VALUE_FORMAL";
    case VALUE_GLOBAL:
	return "VALUE_GLOBAL";
    case VALUE_SYMBOL:
	return "VALUE_SYMBOL";
    case VALUE_EXPR:
	return "VALUE_EXPR";
    case VALUE_PHI:
	return "VALUE_PHI";
    case VALUE_CHI:
	return "VALUE_CHI";
    case VALUE_NOT_CONST:
	return "VALUE_NOT_CONST";
    }
    return 0;
} // SUMMARY_VALUE::Const_type_name


void
SUMMARY_VALUE::Print_const_value (FILE *f, const SUMMARY_SYMBOL* symbol) const
{ 
    if (Is_addr_of ()) {
	if (!Is_convertible_to_global ())
	    fputs ("stack ", f);
	fputs ("address of ", f);
    }
	
    switch (Get_const_type()) {

    case VALUE_UNKNOWN:
	fputs ("*UNKNOWN*", f);
	return;

    case VALUE_INT_CONST:
	fprintf (f, "%lld", Get_int_const_value ());
	return;

    case VALUE_TWO_CONSTS:
	fprintf (f, "0x%x or 0x%x", Get_first_of_two_values(),
		 Get_second_of_two_values());
	return;

    case VALUE_CONST:
	{
            const ST& st = St_Table[Get_const_st_idx ()];
	    if (ST_sym_class (st) == CLASS_CONST)
		fputs (Targ_Print (NULL, Tcon_Table[ST_tcon (st)]), f);
	    else {
		fprintf (f, "GLOBAL %s", ST_name (st));
	    }
	}
	return;

    case VALUE_FORMAL:
	fprintf (f, "FORMAL[%d]", Get_formal_index());
	return;

    case VALUE_GLOBAL:
	fputs ("GLOBAL ", f);
        if (Is_global_st_idx()) { 
	  fprintf(f, "ST_IDX = %d", Get_global_st_idx ());
        } else { 
	  if (symbol)
	      symbol[Get_global_index()].Print (f);
	  else {
	      if (Get_global_index () != -1)
		  fprintf (f, "index = %d", Get_global_index ());
	      else
		  fprintf (f, "ST_IDX = 0x%x", Get_global_st_idx ());
	  }
        } 
	return;

    case VALUE_SYMBOL:
	if (symbol)
	    symbol[Get_symbol_index()].Print (f);
	else
	    fprintf (f, "Symbol index = %d", Get_symbol_index ());
	return;

    case VALUE_EXPR:
	fprintf (f, "EXPR[%d]", Get_expr_index());
	return;

    case VALUE_PHI:
	fprintf (f, "PHI[%d]", Get_phi_index());
	return;

    case VALUE_CHI:
	fprintf (f, "CHI[%d]", Get_chi_index ());
	return;
	
    case VALUE_NOT_CONST:
	fputc ('?', f);
	return;

    }
    
    fputs ("*print function missing*", f);
	
} // SUMMARY_VALUE::Print_const_value


void
SUMMARY_VALUE::Print (FILE *f, INT32 id) const
{
    const char *p;
    
    fprintf (f, "VALUE[%d] : ", id);
    p = Const_type_name ();
    if (p)
	fprintf (f, "%s, ", p);
    else
	fprintf (f, "VALUE_%d, ", Get_const_type());

    fprintf (f, "type = %s, ", Get_mtype() == MTYPE_UNKNOWN ? "?" :
	     MTYPE_name (Get_mtype()));
    if (Is_addr_of ()) 
	fprintf (f, "target type = %s, ", Target_mtype() == MTYPE_UNKNOWN ?
		 "?" : MTYPE_name (Target_mtype()));
    fputs ("value = ", f);
    

    Print_const_value (f, Ipl_Summary_Symbol);

    fputc ('\n', f);
    
} // SUMMARY_VALUE::Print


void
SUMMARY_VALUE::Print_array (FILE *f, INT32 size) const
{
    fprintf (f, "%sStart value array\n%s", SBar, SBar);
    for (INT i = 0; i < size; i++)
	this[i].Print (f, i);
    fprintf (f, "%sEnd value array\n%s", SBar, SBar );
    
} // SUMMARY_VALUE::Print_array


void
SUMMARY_CHI::Print (FILE *f) const
{
    fprintf (f, "CALLSITE[%d]: ", Get_call_index ());

    switch (_type) {
    case CHI_VALUE:
	fprintf (f, "VALUE[%d] ", Get_node_index ());
	break;
    case CHI_PHI:
	fprintf (f, "PHI[%d] ", Get_node_index ());
	break;
    case CHI_EXPR:
	fprintf (f, "EXPR[%d] ", Get_node_index ());
	break;
    case CHI_CHI:
	fprintf (f, "CHI[%d] ", Get_node_index ());
	break;
    }

    if (Ipl_Summary_Symbol && _symbol_index != -1)
	Ipl_Summary_Symbol[_symbol_index].Print (f);
    else
	fputc ('\n', f);
	  
} // SUMMARY_CHI::Print_node


void
SUMMARY_CHI::Print_array (FILE *f, INT32 size) const
{
    fprintf (f, "%sStart chi array\n%s", SBar, SBar);
    for (INT i = 0; i < size; i++) {
	fprintf (f, "CHI[%d]: ", i);
	this[i].Print (f);
    }
    fprintf (f, "%sEnd chi array\n%s", SBar, SBar );
} // SUMMARY_CHI::Print_array

void
SUMMARY_EXPR::Print_node (FILE *f, INT kid) const
{
    if (Is_expr_value (kid))
	fprintf (f, " VALUE[%d]", Get_node_index (kid));
    else if (Is_expr_phi (kid))
	fprintf (f, " PHI[%d]", Get_node_index (kid));
    else if (Is_expr_expr (kid))
	fprintf (f, " EXPR[%d]", Get_node_index (kid));
    else if (Is_expr_chi (kid))
	fprintf (f, " CHI[%d]", Get_node_index (kid));

} // SUMMARY_EXPR::Print_node


void
SUMMARY_EXPR::Print (FILE *f) const
{
    if (Is_expr_unknown ()) {
	fputs ("unknown\n", f);
	return;
    }

    fputs (Get_opcode() == OPCODE_UNKNOWN 
	? "<UNKNOWN>" : OPCODE_name (Get_opcode ()), f);

    if (Has_const_operand ()) {
	if (Get_kid () == 0) {
	    Print_node (f);
	    if (OPCODE_nkids (Get_opcode ()) == 2)
		fprintf (f, " 0x%llx", Get_const_value ());
	} else {
	    fprintf (f, " 0x%llx", Get_const_value ());
	    Print_node (f);
	}
    } else {
	// both operands are not constant
	Print_node (f, 0);
	Print_node (f, 1);
    }

    if (Is_trip_count())
      fprintf(f, " <TRIP COUNT> ");

    fprintf (f, " return type = %s\n", Get_mtype () == MTYPE_UNKNOWN ?
	     "?" : MTYPE_name (Get_mtype ()));
} // SUMMARY_EXPR::Print


void
SUMMARY_EXPR::Print_array (FILE *f, INT32 size) const
{
    fprintf (f, "%sStart expr array\n%s", SBar, SBar);
    for (INT i = 0; i < size; i++) {
	fprintf (f, "EXPR[%d]: ", i);
	this[i].Print (f);
    }
    fprintf (f, "%sEnd expr array\n%s", SBar, SBar );
    
} // SUMMARY_EXPR::Print_array

// print functions for SUMMARY_PHI
void
SUMMARY_PHI::Print (FILE *f) const
{
    for (INT i = 0; i < 2; i++) {
	switch (kids[i]._type) {
	case PHI_UNKNOWN:
	    fprintf (f, "UNKNOWN ");
	    break;
	case PHI_VALUE:
	    fprintf (f, "VALUE[%d] ", Get_node_index (i));
	    break;
	case PHI_EXPR:
	    fprintf (f, "EXPR[%d] ", Get_node_index (i));
	    break;
	case PHI_PHI:
	    fprintf (f, "PHI[%d] ", Get_node_index (i));
	    break;
	case PHI_CHI:
	    fprintf (f, "CHI[%d] ", Get_node_index (i));
	    break;
	}

	fprintf (f, "(if CTRL_DEP[%d] is %s) ", Get_ctrl_dep_index (i),
		 Get_branch (i) ? "true" : "false");
    }

    fputc ('\n', f);
    
} // SUMMARY_PHI::Print


void
SUMMARY_PHI::Print_array (FILE *f, INT32 size) const
{
    fprintf (f, "%sStart phi array\n%s", SBar, SBar);
    for (INT i = 0; i < size; i++) {
	fprintf (f, "PHI[%d]: ", i);
	this[i].Print (f);
    }
    fprintf (f, "%sEnd phi array\n%s", SBar, SBar );
    
} // SUMMARY_PHI::Print_array

// print functions for SUMMARY_STMT

void
SUMMARY_STMT::Print (FILE *f) const
{
    switch (_stmt_type) {
    case STMT_EXPR:
	fprintf (f, "EXPR[%d]\n", Get_expr_index ());
	break;
    case STMT_VAR:
	fprintf(f, "STMT_VAR \n");
        if (Ipl_Summary_Symbol)
	  Ipl_Summary_Symbol[Get_var_index()].Print (f);
	break;
    case STMT_CALL:
	fprintf (f, "CALLSITE[%d]\n", Get_call_index ());
	break;
    case STMT_CD:
	fprintf (f, "CTRL-DEP[%d]\n", Get_cond_index ());
	break;
    case STMT_ARRAY_REF:
	fprintf (f, "ARRAY map_id = %d\n", Get_array_ref_map_id ());
	break;
    case STMT_STID:
	fprintf (f, "GLOBAL-STID[%d]\n", Get_stid_index());
	break;
      default:
	fprintf(f, "UNKNOWN STMT TYPE\n");
	break;
      }
} // SUMMARY_STMT::Print


void
SUMMARY_STMT::Print_array (FILE *f, INT32 size) const
{
    fprintf (f, "%sStart stmt array\n%s", SBar, SBar);
    for (INT i = 0; i < size; i++) {
	fprintf (f, "STMT[%d]: ", i);
	this[i].Print (f);
    }
    fprintf (f, "%sEnd stmt array\n%s", SBar, SBar );
} // SUMMARY_STMT::Print_array



// print functions for summary_control_dependence
void
SUMMARY_CONTROL_DEPENDENCE::Print (FILE *f) const
{
    INT i;

    if (Is_entry ()) {
	fprintf (f, "Entry point:\n");
	fprintf (f, "\tStmts, count = %d: ", Get_true_count ());
    } if (Is_if_stmt ()) {
	fprintf (f, "IF: map_id = %d, EXPR[%d]\n", Get_map_id (),
		 Get_expr_index ());
	fprintf (f, "\tTRUE stmts, count = %d: ", Get_true_count ());
    } else if (Is_do_loop ()) {
	fprintf (f, "DO LOOP[%d]: map_id = %d\n", Get_do_loop_index (),
		 Get_map_id ());
	fprintf (f, "\tLoop Stmts, count = %d: ", Get_true_count ());
    }

    for (i = Get_true_stmt_index ();
	 i < Get_true_stmt_index () + Get_true_count ();
	 i++)

	fprintf (f, "STMT[%d] ", i);
    fputc ('\n', f);

    if (Is_if_stmt ()) {
	fprintf (f, "\tFALSE stmts, count = %d: ", Get_false_count ());

	for (i = Get_false_stmt_index ();
	     i < Get_false_stmt_index () + Get_false_count ();
	     i++)
	    
	    fprintf (f, "STMT[%d] ", i);

	fputc ('\n', f);
    }

} // SUMMARY_CONTROL_DEPENDENCE::Print

void
SUMMARY_CONTROL_DEPENDENCE::Print_array (FILE *f, INT32 size) const
{
    fprintf (f, "%sStart control dependence array\n%s", SBar, SBar);
    for (INT i = 0; i < size; i++) {
	fprintf (f, "CTRL-DEP[%d]: ", i);
	this[i].Print (f);
    }
    fprintf (f, "%sEnd control dependence array\n%s", SBar, SBar );
    
} // SUMMARY_CONTROL_DEPENDENCE::Print_array 

// ====================================================================
// ====================================================================
//
// SUMMARY_SYMBOL::Get_Name
//
// Get the name of a symbol.
//
// WARNING:  NULL is returned for constants or the situations described
// below.
//
// WARNING:  The globals Ipl_Summary_Symbol and Current_Strtab must
// be set to the appropriate symbol summary and string table for this
// to produce a valid name.
//
// ====================================================================
// ====================================================================

const char *
SUMMARY_SYMBOL::Get_Name ( void ) const
{
    // Sort out impossible situations:
    if (Ipl_Summary_Symbol == NULL)
	return "";
    return ST_name (St_idx ());
}
// ====================================================================
// ====================================================================
//
// SUMMARY_SYMBOL::Print
// SUMMARY_SYMBOL::Trace
// SUMMARY_SYMBOL::Print_array
// SUMMARY_SYMBOL::Trace_array
//
// Trace information in one or more symbol.
//
// WARNING:  The globals Ipl_Summary_Symbol and Current_Strtab must
// be set to the appropriate symbol summary and string table for these
// to work.
//
// ====================================================================
// ====================================================================

void
SUMMARY_SYMBOL::Print(FILE *fp, 
		      INT idx,
		      char* symbol_name,
		      char* function_name) const
{
  INT32 id = -1;

  // Attempt to determine the index, and print it:
  if ((Ipl_Summary_Symbol != NULL ) && (idx == -1)) {
      id = this - Ipl_Summary_Symbol;
  }

  if (idx == -1)
    fprintf ( fp, "SYMBOL[%d] ", id );
  else 
    fprintf( fp, "SYMBOL[F%d] ", Get_findex());

  INT ss = 0; 
  if (symbol_name != NULL && function_name != NULL) { 
    ss = sprintf(Modref_Buf, " \"%s\":\"%s\"", symbol_name, function_name);
  } else if (ST_IDX_level(St_idx()) == GLOBAL_SYMTAB) {
    ss = sprintf(Modref_Buf, " \"%s\"", ST_name(St_idx()));
  } 
  sprintf(&Modref_Buf[ss], 
    " type = %s, %sLOCAL, %sSTATIC, %sPASSED(common), %s",
    Get_btype () == MTYPE_UNKNOWN ? "?" : MTYPE_name (Get_btype()),
    Is_local() ? "" : "non-",                      
    Is_static() ? "" : "non-",
    Is_parm() ? "" : "not " ,
    Is_common() ? "COMMON, ": "");
  fprintf (fp, "%s", Modref_Buf);

  if ( IPA_Trace_Mod_Ref ) {
    Ipl_record_tlog("MOD_REF", 0, "%s", Modref_Buf);
  }

  INT32 cc = 0;
  if (Is_common_block()) {   
      fputs (" CBLK", fp);  
      cc += sprintf(Modref_Buf + cc," COMMON_BLOCK");
  }
  if (Is_function()) {   
      fputs (" FUNC", fp);  
      cc += sprintf(Modref_Buf + cc," FUNCTION");
  }
  if (Is_imod()) {   
      fputs (" IMOD", fp);  
      cc += sprintf(Modref_Buf + cc," IMOD");
  }
  if (Is_dmod()) {	
      fputs (" DMOD", fp);  
      cc += sprintf(Modref_Buf + cc," DMOD");
  }
  if (Is_iref()) {	
      fputs (" IREF", fp);  
      cc += sprintf(Modref_Buf + cc," IREF");
  }
  if (Is_dref()) {	
      fputs (" DREF", fp);  
      cc += sprintf(Modref_Buf + cc," DREF");
  }
  if (Is_ikill()) {	
      fputs (" IKILL", fp); 
      cc += sprintf(Modref_Buf + cc," IKILL");
  }
  if (Is_dkill()) {	
      fputs (" DKILL", fp); 
      cc += sprintf(Modref_Buf + cc," DKILL");
  }
  if (Is_parm()) {	
      fputs (" PARM", fp);  
      cc += sprintf(Modref_Buf + cc," PARM");
  }

  if (Is_formal()) {
      cc += sprintf (Modref_Buf+cc, " FORMAL[%d]", Get_findex ());
      fprintf (fp, " FORMAL[%d]", Get_findex ());
  }
  if (Is_cref()) {	
     fputs (" CREF", fp);
      cc += sprintf(Modref_Buf+cc, " CREF");
  }
  if (Is_cdref_preg_only() ) {
      fputs (" CDREF-PREG-ONLY",fp);
      cc += sprintf(Modref_Buf+cc, " CDREF-PREG-ONLY");
  }
  if (Is_addr_saved() || Is_addr_f90_target() || Is_addr_passed()) {
      const char *sep = "";
      fputs (" ADDR_TAKEN_AND_", fp );
      cc += sprintf( Modref_Buf+cc, " ADDR_TAKEN_AND_ ");
      if (Is_addr_saved()) {
	  fprintf ( fp, "%sSAVED", sep );
	      cc += sprintf( Modref_Buf+cc, "%sSAVED", sep);
	  sep = "|";
      }
      if (Is_addr_f90_target()) {
	  fprintf ( fp, "%sIS_F90_TARGET", sep );
	      cc += sprintf( Modref_Buf+cc, "%sIS_F90_TARGET", sep);
	  sep = "|";
      }
      if (Is_addr_passed()) {
	  fprintf ( fp, "%sPASSED", sep );
	      cc += sprintf( Modref_Buf+cc, "%sPASSED", sep);
      }
  }
  if (Used_in_array_section()) {
      fputs(" USED_IN_ARRAY_SECTION", fp);
      cc += sprintf( Modref_Buf+cc, " USED_IN_ARRAY_SECTION" );
      }
  if (Common_io_no_pad()) {
      fputs(" I/O", fp);
      cc += sprintf( Modref_Buf+cc, " I/O:NO_PAD" );
      }
  if (Common_read_no_cprop()) {
      fputs(" I/O", fp);
      cc += sprintf( Modref_Buf+cc, " NO_CPROP" );
      }
  fputc ('\n', fp );
      if ( IPA_Trace_Mod_Ref )
      Ipl_record_tlog("MOD_REF", 0, "%s", Modref_Buf);
}

// ====================================================================

void
SUMMARY_SYMBOL::Trace () const
{
    Print ( TFile );
}

// ====================================================================
// Trace an array of symbol offset nodes.
// ====================================================================

void
SUMMARY_SYMBOL::Print_array (FILE* fp, INT32 size, 
			     DYN_ARRAY<char*>* symbol_names, 
			     DYN_ARRAY<char*>* function_names) const
{
    INT32 i;

    fprintf ( fp, "%sStart symbol array\n%s", SBar, SBar );
    for ( i = 0; i< size; ++i ) {
	this[i].Print ( fp, 
                        -1, 
                        symbol_names ? (*symbol_names)[i] : NULL, 
                        function_names ? (*function_names)[i] : NULL);
    }
    fprintf ( fp, "%sEnd symbol array\n%s", SBar, SBar );
}

// ====================================================================

void
SUMMARY_SYMBOL::Trace_array ( INT32 size ) const
{
    Print_array ( TFile, size );
}



//-----------------------------------------------------------
// Trace one entry of the global offset node
//-----------------------------------------------------------

void
SUMMARY_GLOBAL::Print ( FILE *fp ) const
{
  if (Ipl_Summary_Symbol) {
    SUMMARY_SYMBOL *sym = &(Ipl_Summary_Symbol[Get_symbol_index()]);
    
    fprintf ( fp, "GLOBAL %s (refs=%d,  mods=%d)",
	     sym->Get_Name (), Get_refcount(), Get_modcount() );

    if ( Is_imod() )	fprintf ( fp, " IMOD");
    if ( Is_dmod() )	fprintf ( fp, " DMOD");
    if ( Is_iref() )	fprintf ( fp, " IREF");
    if ( Is_dref() )	fprintf ( fp, " DREF");
    if ( Is_aref() )	fprintf ( fp, " AREF");
    if ( Is_ikill() )	fprintf ( fp, " IKILL");
    if ( Is_dkill() )	fprintf ( fp, " DKILL");
    if ( sym->Is_cref() )	fprintf ( fp, " CREF");
    if ( sym->Is_cmod() )	fprintf ( fp, " CMOD");
  }

  fprintf ( fp, "\n" );
}

//-----------------------------------------------------------

void
SUMMARY_GLOBAL::Trace ( void ) const
{
  Print ( TFile );
}

//-----------------------------------------------------------
// Trace the global offset node array
//-----------------------------------------------------------

void
SUMMARY_GLOBAL::Print_array (FILE* fp, INT32 size ) const
{
    INT i;

    fprintf ( fp, "%sStart global array\n%s", SBar, SBar );
    for ( i = 0; i< size; ++i ) {
	fprintf ( fp, "GLOBAL[%d]: ", i );
	this[i].Print ( fp );
    }
    fprintf ( fp, "%sEnd global array\n%s", SBar, SBar );
}

//-----------------------------------------------------------

void
SUMMARY_GLOBAL::Trace_array ( INT32 size ) const
{
    Print_array ( TFile, size );
}




//-----------------------------------------------------------
// Trace the summary common array
//-----------------------------------------------------------
void
SUMMARY_COMMON::Print ( FILE *fp, INT32 id ) const
{
    fprintf ( fp, "COMMON[%d]:  ", id );

    if ( Is_initialized() )
	fprintf ( fp, ", initialized" );

    if ( Has_bad_equiv() )
	fprintf ( fp, ", bad equivalence " );

    if ( Has_bad_split_equiv() )
	fprintf ( fp, ", bad split equivalence " );
    else
	fprintf ( fp, ", not bad split equivalence ");

    if ( Has_array_constants() )
      fprintf ( fp, ", array constants " );
    else
      fprintf ( fp, ", no array constants ");

    if ( Has_equivalences() )
	fprintf ( fp, ", equivalences " );
    else
	fprintf ( fp, ", no equivalences " );

    if (Has_unstructured_cflow())
	fprintf( fp, ", unstructured cflow ");
    else
	fprintf( fp, ", no unstructured cflow ");

    fprintf(fp, "\n");

    fprintf ( fp, ", common shape index = %d, common shape count = %d \n",
	     Get_common_shape_index(), Get_common_shape_count() );

    if ( Ipl_Summary_Symbol ) {
      Ipl_Summary_Symbol[Get_symbol_index()].Print ( fp );
    }
} // SUMMARY_COMMON::Print

//-----------------------------------------------------------

void
SUMMARY_COMMON::Trace ( INT32 id ) const
{
    Print ( TFile, id );
}


void 
SUMMARY_COMMON::Print_array (FILE* fp, INT32 size ) const
{
    INT i;

    fprintf ( fp, "%sStart common array\n%s", SBar, SBar );
    for ( i=0; i<size; ++i ) {
	this[i].Print ( fp, i );
    }
    fprintf ( fp, "%sEnd common array \n%s", SBar, SBar );
}

//-----------------------------------------------------------
// Trace the summary stid array
//-----------------------------------------------------------
void
SUMMARY_STID::Print ( FILE *fp, INT32 id ) const
{
    fprintf ( fp, "GLOBAL_STID[%d]: ", id );
    fprintf ( fp, "symbol index = %d, value index = %d",
	     Get_symbol_index(), Get_value_index() );
    if ( Is_always_executed() )
      fprintf ( fp, ", always executed");
    if ( Is_array_assignment() ) {
      fprintf ( fp, ", array assignment");
      if ( Has_constant_subscript() )
        fprintf ( fp, ", constant subscript = %d", Get_array_subscript());
      else
        fprintf ( fp, ", non-constant subscript");
    }
    fprintf(fp, "\n");
    if ( Ipl_Summary_Symbol && Get_symbol_index() != -1 ) 
      Ipl_Summary_Symbol[Get_symbol_index()].Print ( fp );
} // SUMMARY_STID::Print


void
SUMMARY_STID::Trace ( INT32 id ) const
{
    Print ( TFile, id );
}


void 
SUMMARY_STID::Print_array (FILE* fp, INT32 size ) const
{
    INT i;

    fprintf ( fp, "%sStart STID COMMON array\n%s", SBar, SBar );
    for ( i=0; i<size; ++i ) {
	this[i].Print ( fp, i );
    }
    fprintf ( fp, "%sEnd STID COMMON array \n%s", SBar, SBar );
}

void
SUMMARY_STID::Trace_array ( INT32 size ) const
{
    Print_array ( TFile, size );
}



void
SUMMARY_COMMON_SHAPE::Print ( FILE *fp, INT32 id ) const
{
  fprintf ( fp, "COMMON_SHAPE[%d]:  ", id ); 

    if ( Is_kind_array() )	
	fprintf ( fp, " array" );

    if ( Is_kind_scalar() ) {
      fprintf ( fp, " scalar, element size = %d, offset = %lld, symbol_id = %d   \n", Get_element_size(), Get_offset(), Get_symbol_index());
    }

    if ( Is_symbolic_bounds() )
	fprintf ( fp, ", symbolic bounds \n" );

    if (Is_kind_array() && (!Is_symbolic_bounds()))
	fprintf (fp,
		 ", upper = %d, lower = %d, stride = %d, element_size= %d, symbol_id = %d \n",Get_upper(), Get_lower(), Get_stride(),Get_element_size(), Get_symbol_index() );
} // SUMMARY_COMMON_SHAPE::Print


//-----------------------------------------------------------

void
SUMMARY_COMMON_SHAPE::Trace ( INT32 id ) const
{
    Print ( TFile, id );
}

void 
SUMMARY_COMMON_SHAPE::Print_array (FILE* fp, INT32 size ) const
{
    INT i;

    fprintf ( fp, "%sStart common shape array\n%s", SBar, SBar );
    for ( i=0; i<size; ++i ) {
	this[i].Print ( fp, i );
    }
    fprintf ( fp, "%sEnd common shape array \n%s", SBar, SBar );
}

//-----------------------------------------------------------

void
SUMMARY_COMMON_SHAPE::Trace_array ( INT32 size ) const
{
    Print_array ( TFile, size );
}
/*-----------------------------------------------------------*/
/*reorder*/
void
SUMMARY_STRUCT_ACCESS::Print ( FILE *fp, INT32 id ) const
{
	UINT i;
  fprintf ( fp, "FLD ACCESS [%d]: name:%s  ", id,Get_ty_name() ); 
	for(i=0; i<max_hot_num;i++){
		if(Get_hot_fld_id(i)==0) /*fld_id=1,2,...*/
			break;
      	fprintf ( fp, " field_id= %d, count = %lld  \n", Get_hot_fld_id(i),Get_hot_fld(i));
	}
} // SUMMARY_STRUCT_ACCESS::Print
void
SUMMARY_STRUCT_ACCESS::Trace_array ( INT32 size ) const
{
    Print_array ( TFile, size );
}
void
SUMMARY_STRUCT_ACCESS::Print_array (FILE* fp, INT32 size ) const
{
    INT i;

    fprintf ( fp, "%sStart fld access array\n%s", SBar, SBar );
    for ( i=0; i<size; ++i ) {
	this[i].Print ( fp, i );
    }
    fprintf ( fp, "%sEnd fld access array \n%s", SBar, SBar );
}
void
SUMMARY_STRUCT_ACCESS::Trace ( INT32 id ) const
{
    Print ( TFile, id );
}

#ifdef KEY
void
SUMMARY_TY_INFO::Print ( FILE *fp ) const
{
  fprintf ( fp, "TYPE [%d]: ", Get_ty() ); 
  if (Is_ty_no_split()) fprintf ( fp, "no_split " );

  fprintf ( fp, "\n");
} // SUMMARY_TY_INFO::Print

void
SUMMARY_TY_INFO::Print_array (FILE* fp, INT32 size ) const
{
  fprintf ( fp, "%sStart type array\n%s", SBar, SBar );
  for ( INT i=0; i<size; ++i ) {
    this[i].Print ( fp );
  }
  fprintf ( fp, "%sEnd type array \n%s", SBar, SBar );
}

void
SUMMARY_TY_INFO::Trace_array ( INT32 size ) const
{
  Print_array ( TFile, size );
}

void
SUMMARY_TY_INFO::Trace ( void ) const
{
  Print ( TFile );
}
#endif

// Constraint graph summary for Nystrom Alias Analyzer
void
SUMMARY_CONSTRAINT_GRAPH_NODE::Print( FILE *fp) const
{
  fprintf(fp, "CGNODE %d st: %lld offset: %d flags: 0x%x\n",
          _cgNodeId, _cg_st_idx, _offset, _flags);
  fprintf(fp, "_numBitsPtsGBL: %d idx: %d\n", _numBitsPtsGBL, _pointsToGBLIdx);
  fprintf(fp, "_numBitsPtsHZ: %d idx: %d\n", _numBitsPtsHZ, _pointsToHZIdx);
  fprintf(fp, "_numBitsPtsDN: %d idx: %d\n", _numBitsPtsDN, _pointsToDNIdx);
} 

void
SUMMARY_CONSTRAINT_GRAPH_NODE::Print_array(FILE* fp, INT32 size) const
{
  fprintf(fp, "%sStart cg node array\n%s", SBar, SBar);
  for (INT i = 0; i < size; ++i) {
    this[i].Print(fp);
    fprintf(fp, "\n");
  }
  fprintf( fp, "%sEnd cg node array \n%s", SBar, SBar);
}

void
SUMMARY_CONSTRAINT_GRAPH_NODE::Trace_array(INT32 size) const
{
  Print_array(TFile, size);
}

void
SUMMARY_CONSTRAINT_GRAPH_NODE::Trace(void) const
{
  Print (TFile);
}

void
SUMMARY_CONSTRAINT_GRAPH_EDGE::Print( FILE *fp) const
{
  fprintf(fp, "src %d dest: %d etype: %d qual: %d sizeOrSkew: %d", 
          _srcId, _destId, _etype, _qual, _sizeOrSkew);
} 

void
SUMMARY_CONSTRAINT_GRAPH_EDGE::Print_array(FILE* fp, INT32 size) const
{
  fprintf(fp, "%sStart cg edge array\n%s", SBar, SBar);
  for (INT i = 0; i < size; ++i) {
    this[i].Print(fp);
    fprintf(fp, "\n");
  }
  fprintf( fp, "%sEnd cg edge array \n%s", SBar, SBar);
}

void
SUMMARY_CONSTRAINT_GRAPH_EDGE::Trace_array(INT32 size) const
{
  Print_array(TFile, size);
}

void
SUMMARY_CONSTRAINT_GRAPH_EDGE::Trace(void) const
{
  Print (TFile);
}

void
SUMMARY_CONSTRAINT_GRAPH_STINFO::Print( FILE *fp) const
{
  fprintf(fp, "st_idx %lld varsize: %lld modulus: %d firstoffset: %d", 
          _cg_st_idx, _varSize, _modulus, _firstOffset);
} 

void
SUMMARY_CONSTRAINT_GRAPH_STINFO::Print_array(FILE* fp, INT32 size) const
{
  fprintf(fp, "%sStart cg stinfo array\n%s", SBar, SBar);
  for (INT i = 0; i < size; ++i) {
    this[i].Print(fp);
    fprintf(fp, "\n");
  }
  fprintf( fp, "%sEnd cg stinfo array \n%s", SBar, SBar);
}

void
SUMMARY_CONSTRAINT_GRAPH_STINFO::Trace_array(INT32 size) const
{
  Print_array(TFile, size);
}

void
SUMMARY_CONSTRAINT_GRAPH_STINFO::Trace(void) const
{
  Print (TFile);
}

void
SUMMARY_CONSTRAINT_GRAPH_CALLSITE::Print( FILE *fp) const
{
  fprintf(fp, "id %d numParms: %d paramNodesIdx: %d return: %d ", 
          _id, _numParms, _paramNodesIdx, _return);
} 

void
SUMMARY_CONSTRAINT_GRAPH_CALLSITE::Print_array(FILE* fp, INT32 size) const
{
  fprintf(fp, "%sStart cg callsite array\n%s", SBar, SBar);
  for (INT i = 0; i < size; ++i) {
    this[i].Print(fp);
  }
  fprintf( fp, "%sEnd cg callsite array \n%s", SBar, SBar);
}

void
SUMMARY_CONSTRAINT_GRAPH_CALLSITE::Trace_array(INT32 size) const
{
  Print_array(TFile, size);
}

void
SUMMARY_CONSTRAINT_GRAPH_CALLSITE::Trace(void) const
{
  Print (TFile);
}

void
SUMMARY_CONSTRAINT_GRAPH_MODRANGE::Print( FILE *fp) const
{
  fprintf(fp, "start %d end: %d mod: %d child: %d next: %d", 
          _startOffset, _endOffset, _modulus, _childIdx, _nextIdx);
} 

void
SUMMARY_CONSTRAINT_GRAPH_MODRANGE::Print_array(FILE* fp, INT32 size) const
{
  fprintf(fp, "%sStart cg modrange array\n%s", SBar, SBar);
  for (INT i = 0; i < size; ++i) {
    this[i].Print(fp);
    fprintf(fp, "\n");
  }
  fprintf( fp, "%sEnd cg modrange array \n%s", SBar, SBar);
}

void
SUMMARY_CONSTRAINT_GRAPH_MODRANGE::Trace_array(INT32 size) const
{
  Print_array(TFile, size);
}

void
SUMMARY_CONSTRAINT_GRAPH_MODRANGE::Trace(void) const
{
  Print (TFile);
}

