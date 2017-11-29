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


/* -*-Mode: c++;-*- (Tell emacs to use c++ mode) */

#include <stdint.h>
#if defined(BUILD_OS_DARWIN)
#include <darwin_elf.h>                        // Elf64_Word
#else /* defined(BUILD_OS_DARWIN) */
#include <elf.h>                        // Elf64_Word
#endif /* defined(BUILD_OS_DARWIN) */
#include <sys/elf_whirl.h>              // WT_IPA_SUMMARY
#include <sys/types.h>                  // ir_bwrite.h needs it

#include "defs.h"
#include "wn.h"                         // ir_bwrite.h needs it
#include "pu_info.h"                    // ir_bwrite.h needs it
#include "ir_bwrite.h"                  // Output_File
#include "ir_bcom.h"                    // ir_b_save_buf
#include "ipl_main.h"                   // Do_Par, DoPreopt, ...
#include "ipl_summary.h"		// SUMMARY_* classes
#include "ipl_summarize.h"              // SUMMARY class
#include "ipl_array_bread_write.h"      // ARRAY_SUMMARY_OUTPUT
#include "ipl_bread_write.h"            // C linkage


extern mUINT8 Optlevel;                 // from ipl_main.cxx


#define SUMMARY_HEADER_ADDR(offset) \
    ((SUMMARY_FILE_HEADER*)(fl->map_addr + offset))

#define HEADER_ADDR(offset) \
 ((Elf64_Word*)(fl->map_addr + offset))

static SUMMARY_FILE_HEADER header;

// if we are not computing array section information, atleast
// fill in the necessary header fields
static void
init_array_size_fields(SUMMARY_FILE_HEADER *header_addr)
{
  header_addr->Set_cfg_node_size(0);
  header_addr->Set_regions_array_size(0);
  header_addr->Set_projected_region_size(0);
  header_addr->Set_projected_array_size(0);
  header_addr->Set_term_array_size(0);
  header_addr->Set_ivar_size(0);
  header_addr->Set_loopinfo_size(0);
  header_addr->Set_scalar_node_size(0);
}

void
IPA_irb_write_summary(Output_File *fl)
{
    INT size, offset, idx, header_loc;
    INT offset_sym, offset_proc, offset_feedback, offset_call;
    INT offset_formal, offset_actual, offset_value, offset_expr;
    INT offset_phi, offset_chi, offset_global;
    INT offset_stmt, offset_ctrl_dep, offset_global_stid;
    INT cur_sec_disp, offset_common, offset_common_shape;
    INT offset_struct_access;
#ifdef KEY
    INT offset_ty_info = 0;
#endif
    // Constraint graph summary for Nystrom Alias Analyzer
    INT offset_cg_nodes = 0;
    INT offset_cg_edges = 0;
    INT offset_cg_stinfos = 0;
    INT offset_cg_callsites = 0;
    INT offset_cg_node_ids = 0;
    INT offset_cg_modranges = 0;

    Elf64_Word temp;
    offset_sym = offset_proc = offset_feedback = offset_call = 0;
    offset_formal = offset_actual = offset_value = offset_expr = 0;
    offset_phi = offset_chi = offset_stmt = offset_ctrl_dep = 0;
    offset_global = 0; offset_common = 0; offset_common_shape = 0;
    offset_global_stid = 0;offset_struct_access=0;

    cur_sec_disp = fl->file_size;

    // store the offset of the header structure in this field
    header_loc = (INT) ir_b_save_buf(&temp, sizeof(Elf64_Word),
				     sizeof(INT64),0,fl);
    

    if (Summary->Has_symbol_entry ()) {
	size = (Summary->Get_symbol_idx () + 1) * sizeof(SUMMARY_SYMBOL);

	offset_sym  = (INT) ir_b_save_buf (Summary->Get_symbol (0),
					    size, sizeof(INT64), 0, fl);
	
	offset_sym = offset_sym - cur_sec_disp;
    }

    if (Summary->Has_value_entry ()) {
	size = (Summary->Get_value_idx() + 1) * sizeof(SUMMARY_VALUE);

	offset_value = (INT) ir_b_save_buf (Summary->Get_value (0),
					    size, sizeof(INT64), 0, fl);

	offset_value = offset_value - cur_sec_disp;
    }

    if (Summary->Has_procedure_entry ()) {
	size = (Summary->Get_procedure_idx () + 1) *
	    sizeof(SUMMARY_PROCEDURE);

	offset_proc  = (INT) ir_b_save_buf (Summary->Get_procedure (0),
					    size, sizeof(INT64), 0, fl);

	offset_proc = offset_proc - cur_sec_disp;
    }

    if (Summary->Has_feedback_entry ()) {
	size = (Summary->Get_feedback_idx () + 1) * sizeof(SUMMARY_FEEDBACK);

	offset_feedback  = (INT) ir_b_save_buf (Summary->Get_feedback (0),
						size, sizeof(INT64), 0, fl);

	offset_feedback = offset_feedback - cur_sec_disp;
    }

    if (Summary->Has_callsite_entry ()) {
	size = (Summary->Get_callsite_idx() + 1) * sizeof(SUMMARY_CALLSITE);

	offset_call = (INT) ir_b_save_buf (Summary->Get_callsite (0),
					   size, sizeof(INT64), 0, fl);

	offset_call = offset_call - cur_sec_disp;
    }

    if (Summary->Has_actual_entry ()) {
	size = (Summary->Get_actual_idx() + 1) * sizeof(SUMMARY_ACTUAL);

	offset_actual = (INT) ir_b_save_buf (Summary->Get_actual (0),
					     size, sizeof(INT64), 0, fl);

	offset_actual = offset_actual - cur_sec_disp;
    }

    if (Summary->Has_expr_entry ()) {
	size = (Summary->Get_expr_idx() + 1) * sizeof(SUMMARY_EXPR);

	offset_expr = (INT) ir_b_save_buf (Summary->Get_expr (0),
					   size, sizeof(INT64), 0, fl);

	offset_expr = offset_expr - cur_sec_disp;
    }

    if (Summary->Has_phi_entry ()) {
	size = (Summary->Get_phi_idx() + 1) * sizeof(SUMMARY_PHI);

	offset_phi = (INT) ir_b_save_buf (Summary->Get_phi (0),
					  size, sizeof(INT64), 0, fl);

	offset_phi = offset_phi - cur_sec_disp;
    }

    if (Summary->Has_chi_entry ()) {
	size = (Summary->Get_chi_idx() + 1) * sizeof(SUMMARY_CHI);

	offset_chi = (INT) ir_b_save_buf (Summary->Get_chi (0),
					  size, sizeof(INT64), 0, fl);

	offset_chi = offset_chi - cur_sec_disp;
    }

    if (Summary->Has_stmt_entry ()) {
	size = (Summary->Get_stmt_idx() + 1) * sizeof(SUMMARY_STMT);

	offset_stmt = (INT) ir_b_save_buf (Summary->Get_stmt (0),
					   size, sizeof(INT64), 0, fl);

	offset_stmt = offset_stmt - cur_sec_disp;
    }

    if (Summary->Has_ctrl_dep_entry ()) {
	size = (Summary->Get_ctrl_dep_idx() + 1) *
	    sizeof(SUMMARY_CONTROL_DEPENDENCE);

	offset_ctrl_dep = (INT) ir_b_save_buf (Summary->Get_ctrl_dep (0),
					   size, sizeof(INT64), 0, fl);

	offset_ctrl_dep = offset_ctrl_dep - cur_sec_disp;
    }

    if (Summary->Has_formal_entry ()) {
	size = (Summary->Get_formal_idx () + 1) * sizeof(SUMMARY_FORMAL);
	
	offset_formal = (INT) ir_b_save_buf (Summary->Get_formal (0),
					     size, sizeof(INT64), 0, fl);

	offset_formal = offset_formal - cur_sec_disp;
    } 

    if (Summary->Has_global_entry ()) {
	size = (Summary->Get_global_idx () + 1) * sizeof(SUMMARY_GLOBAL);
	
	offset_global = (INT) ir_b_save_buf (Summary->Get_global (0),
					     size, sizeof(INT64), 0, fl);

	offset_global = offset_global - cur_sec_disp;
    } 



    if (Summary->Has_common_entry ()) {
	size = (Summary->Get_common_idx () + 1) * sizeof(SUMMARY_COMMON);
	
	offset_common = (INT) ir_b_save_buf (Summary->Get_common (0),
					     size, sizeof(INT64), 0, fl);

	offset_common = offset_common - cur_sec_disp;
    } 

    if (Summary->Has_common_shape_entry ()) {
	size = (Summary->Get_common_shape_idx () + 1) *
	    sizeof(SUMMARY_COMMON_SHAPE);
	
	offset_common_shape = (INT)
	    ir_b_save_buf (Summary->Get_common_shape (0), size,
			   sizeof(INT64), 0, fl); 

	offset_common_shape = offset_common_shape - cur_sec_disp;
    } 


    if (Summary->Has_global_stid_entry ()) {
      size = (Summary->Get_global_stid_idx () + 1) * sizeof(SUMMARY_STID);
      
      offset_global_stid = (INT) ir_b_save_buf (Summary->Get_global_stid (0),
						size, sizeof(INT64), 0, fl);
      
      offset_global_stid = offset_global_stid - cur_sec_disp;
    } 
    if (Summary->Has_struct_access_entry ()) {
      size = (Summary->Get_struct_access_idx () + 1) * sizeof(SUMMARY_STRUCT_ACCESS);
      
      offset_struct_access = (INT) ir_b_save_buf (Summary->Get_struct_access (0),
						size, sizeof(INT64), 0, fl);
      
      offset_struct_access = offset_struct_access - cur_sec_disp;
    } 
#ifdef KEY
    if (Summary->Has_ty_info_entry ()) {
      size = (Summary->Get_ty_info_idx () + 1) * sizeof(SUMMARY_TY_INFO);

      offset_ty_info = (INT) ir_b_save_buf (Summary->Get_ty_info (0),
                                            size, sizeof(INT64), 0, fl);

      offset_ty_info = offset_ty_info - cur_sec_disp;
    } 
#endif

    // Constraint graph specific data for Nystrom Alias Analyzer
    if (Summary->Has_constraint_graph_nodes()) {
      
      // Dump the list of constraint graph nodes
      size = (Summary->Get_constraint_graph_nodes_idx() + 1) * 
               sizeof(SUMMARY_CONSTRAINT_GRAPH_NODE);
      offset_cg_nodes = 
        (INT)ir_b_save_buf(Summary->Get_constraint_graph_node(0),
                           size, sizeof(INT64), 0, fl);
      offset_cg_nodes = offset_cg_nodes - cur_sec_disp;

      FmtAssert(Summary->Has_constraint_graph_stinfos(), 
                ("CGNodes should have StInfos"));

      // Dump the list of constraint graph stinfos
      size = (Summary->Get_constraint_graph_stinfos_idx() + 1) * 
               sizeof(SUMMARY_CONSTRAINT_GRAPH_STINFO);
      offset_cg_stinfos = 
        (INT)ir_b_save_buf(Summary->Get_constraint_graph_stinfo(0),
                           size, sizeof(INT64), 0, fl);
      offset_cg_stinfos = offset_cg_stinfos - cur_sec_disp;
    }

    if (Summary->Has_constraint_graph_edges()) {
      // Dump the list of constraint graph edges
      size = (Summary->Get_constraint_graph_edges_idx() + 1) * 
               sizeof(SUMMARY_CONSTRAINT_GRAPH_EDGE);
      offset_cg_edges = 
        (INT)ir_b_save_buf(Summary->Get_constraint_graph_edge(0),
                           size, sizeof(INT64), 0, fl);
      offset_cg_edges = offset_cg_edges - cur_sec_disp;
    }

    if (Summary->Has_constraint_graph_callsites()) {
      // Dump the list of constraint graph callsites
      size = (Summary->Get_constraint_graph_callsites_idx() + 1) * 
               sizeof(SUMMARY_CONSTRAINT_GRAPH_CALLSITE);
      offset_cg_callsites = 
        (INT)ir_b_save_buf(Summary->Get_constraint_graph_callsite(0),
                           size, sizeof(INT64), 0, fl);
      offset_cg_callsites = offset_cg_callsites - cur_sec_disp;
    }

    if (Summary->Has_constraint_graph_node_ids()) {
      // Dump the list of constraint graph ids that are in the pts-to-sets
      // of the corresponding nodes
      size = (Summary->Get_constraint_graph_node_ids_idx() + 1) * 
             sizeof(UINT32);
      offset_cg_node_ids = 
        (INT)ir_b_save_buf(Summary->Get_constraint_graph_node_id(0),
                           size, sizeof(INT64), 0, fl);
      offset_cg_node_ids = offset_cg_node_ids - cur_sec_disp;
    }

    if (Summary->Has_constraint_graph_modranges()) {
      // Dump the list of constraint graph modranges
      size = (Summary->Get_constraint_graph_modranges_idx() + 1) * 
               sizeof(SUMMARY_CONSTRAINT_GRAPH_MODRANGE);
      offset_cg_modranges = 
        (INT)ir_b_save_buf(Summary->Get_constraint_graph_modrange(0),
                           size, sizeof(INT64), 0, fl);
      offset_cg_modranges = offset_cg_modranges - cur_sec_disp;
    }
 
    if (Do_Par)
     Array_Summary_Output->Write_summary(fl, cur_sec_disp);
    
    offset = (INT)ir_b_save_buf(&header, sizeof(SUMMARY_FILE_HEADER),
				sizeof(INT64), 0, fl);

    
    *(HEADER_ADDR(header_loc)) = offset - cur_sec_disp;
    SUMMARY_FILE_HEADER *header_addr = SUMMARY_HEADER_ADDR(offset);

    if (Do_Par) {
      Array_Summary_Output->Update_array_sect_header(header_addr);
      header_addr->Set_AutoPar();
    }
    else {
      init_array_size_fields(header_addr);
    }

    // use opt level to represent that splitting is needed. So set
    // it to 3 if Optlevel=3 or Reorg_Common = ON 
    // if Reorg_Common is off and Optlevel is 3 then don't set it to 3
    if ((Optlevel == 3 && Do_Split_Commons) || (Do_Split_Commons_Set))
      Optlevel = 3;
    else
      Optlevel = 1;

    header_addr->Set_opt_level(Optlevel);
    header_addr->Set_version_number(IPA_SUMMARY_REVISION);
    header_addr->Set_minor_version_number(IPA_SUMMARY_MINOR_REVISION);
    header_addr->Set_symbol_offset(offset_sym);
    header_addr->Set_proc_offset(offset_proc);
    header_addr->Set_feedback_offset(offset_feedback);
    header_addr->Set_callsite_offset(offset_call);
    header_addr->Set_actual_offset(offset_actual);
    header_addr->Set_value_offset(offset_value);
    header_addr->Set_expr_offset(offset_expr);
    header_addr->Set_phi_offset(offset_phi);
    header_addr->Set_chi_offset(offset_chi);
    header_addr->Set_stmt_offset(offset_stmt);
    header_addr->Set_ctrl_dep_offset(offset_ctrl_dep);
    header_addr->Set_formal_offset(offset_formal);
    header_addr->Set_global_offset(offset_global);
    header_addr->Set_common_offset(offset_common);
    header_addr->Set_common_shape_offset(offset_common_shape);
    header_addr->Set_global_stid_offset(offset_global_stid);
    header_addr->Set_struct_access_offset(offset_struct_access);
#ifdef KEY
    header_addr->Set_ty_info_offset(offset_ty_info);
#endif
    // Constraint graph summary for Nystrom Alias Analyzer
    header_addr->Set_constraint_graph_nodes_offset(offset_cg_nodes);
    header_addr->Set_constraint_graph_edges_offset(offset_cg_edges);
    header_addr->Set_constraint_graph_stinfos_offset(offset_cg_stinfos);
    header_addr->Set_constraint_graph_callsites_offset(offset_cg_callsites);
    header_addr->Set_constraint_graph_node_ids_offset(offset_cg_node_ids);
    header_addr->Set_constraint_graph_modranges_offset(offset_cg_modranges);

    header_addr->Set_symbol_size(Summary->Get_symbol_idx() + 1);
    header_addr->Set_proc_size(Summary->Get_procedure_idx() + 1);
    header_addr->Set_feedback_size(Summary->Get_feedback_idx() + 1);
    header_addr->Set_callsite_size(Summary->Get_callsite_idx() + 1);
    header_addr->Set_actual_size(Summary->Get_actual_idx() + 1);
    header_addr->Set_value_size(Summary->Get_value_idx() + 1);
    header_addr->Set_expr_size(Summary->Get_expr_idx() + 1);
    header_addr->Set_phi_size(Summary->Get_phi_idx() + 1);
    header_addr->Set_chi_size(Summary->Get_chi_idx() + 1);
    header_addr->Set_stmt_size(Summary->Get_stmt_idx() + 1);
    header_addr->Set_ctrl_dep_size(Summary->Get_ctrl_dep_idx() + 1);
    header_addr->Set_formal_size(Summary->Get_formal_idx() + 1);
    header_addr->Set_global_size(Summary->Get_global_idx() + 1);
    header_addr->Set_common_size(Summary->Get_common_idx() + 1);
    header_addr->Set_common_shape_size(Summary->Get_common_shape_idx() + 1);
    header_addr->Set_global_stid_size(Summary->Get_global_stid_idx() + 1);
 	header_addr->Set_struct_access_size(Summary->Get_struct_access_idx() + 1);
#ifdef KEY
    header_addr->Set_ty_info_size(Summary->Get_ty_info_idx() + 1);
#endif
    // Constraint graph summary for Nystrom Alias Analyzer
    header_addr->Set_constraint_graph_nodes_size(Summary->Get_constraint_graph_nodes_idx() + 1);
    header_addr->Set_constraint_graph_edges_size(Summary->Get_constraint_graph_edges_idx() + 1);
    header_addr->Set_constraint_graph_stinfos_size(Summary->Get_constraint_graph_stinfos_idx() + 1);
    header_addr->Set_constraint_graph_callsites_size(Summary->Get_constraint_graph_callsites_idx() + 1);
    header_addr->Set_constraint_graph_node_ids_size(Summary->Get_constraint_graph_node_ids_idx() + 1);
    header_addr->Set_constraint_graph_modranges_size(Summary->Get_constraint_graph_modranges_idx() + 1);
 
    header_addr->Set_symbol_entry_size(sizeof(SUMMARY_SYMBOL));
    header_addr->Set_proc_entry_size(sizeof(SUMMARY_PROCEDURE));
    header_addr->Set_feedback_entry_size(sizeof(SUMMARY_FEEDBACK));
    header_addr->Set_callsite_entry_size(sizeof(SUMMARY_CALLSITE));
    header_addr->Set_actual_entry_size(sizeof(SUMMARY_ACTUAL));
    header_addr->Set_value_entry_size(sizeof(SUMMARY_VALUE));
    header_addr->Set_expr_entry_size(sizeof(SUMMARY_EXPR));
    header_addr->Set_phi_entry_size(sizeof(SUMMARY_PHI));
    header_addr->Set_chi_entry_size(sizeof(SUMMARY_CHI));
    header_addr->Set_stmt_entry_size(sizeof(SUMMARY_STMT));
    header_addr->Set_ctrl_dep_entry_size(sizeof(SUMMARY_CONTROL_DEPENDENCE));
    header_addr->Set_formal_entry_size(sizeof(SUMMARY_FORMAL));
    header_addr->Set_global_entry_size(sizeof(SUMMARY_GLOBAL));
    header_addr->Set_common_entry_size(sizeof(SUMMARY_COMMON));
    header_addr->Set_common_shape_entry_size(sizeof(SUMMARY_COMMON_SHAPE));
    header_addr->Set_common_shape_entry_size(sizeof(SUMMARY_COMMON_SHAPE));
    header_addr->Set_global_stid_entry_size(sizeof(SUMMARY_STID));
    header_addr->Set_struct_access_entry_size(sizeof(SUMMARY_STRUCT_ACCESS));
#ifdef KEY
    header_addr->Set_ty_info_entry_size(sizeof(SUMMARY_TY_INFO));
#endif
    // Constraint graph summary for Nystrom Alias Analyzer
    header_addr->Set_constraint_graph_nodes_entry_size(sizeof(SUMMARY_CONSTRAINT_GRAPH_NODE));
    header_addr->Set_constraint_graph_edges_entry_size(sizeof(SUMMARY_CONSTRAINT_GRAPH_EDGE));
    header_addr->Set_constraint_graph_stinfos_entry_size(sizeof(SUMMARY_CONSTRAINT_GRAPH_STINFO));
    header_addr->Set_constraint_graph_callsites_entry_size(sizeof(SUMMARY_CONSTRAINT_GRAPH_CALLSITE));
    header_addr->Set_constraint_graph_node_ids_entry_size(sizeof(UINT32));
    header_addr->Set_constraint_graph_modranges_entry_size(sizeof(SUMMARY_CONSTRAINT_GRAPH_MODRANGE));
}


void
IPA_Trace_Summary_Section (FILE *f,		// File to trace to
			   const void *sbase,
			   DYN_ARRAY<char*>* symbol_names, 
			   DYN_ARRAY<char*>* function_names)	
			     // Handle for section
{
    SUMMARY_FILE_HEADER *file_header;
    SUMMARY_SYMBOL *sym_array;
    SUMMARY_PROCEDURE  *proc_array;
    SUMMARY_FEEDBACK *feedback_array;
    SUMMARY_CALLSITE *callsite_array;
    SUMMARY_VALUE *value_array;
    SUMMARY_EXPR *expr_array;
    SUMMARY_PHI *phi_array;
    SUMMARY_CHI *chi_array;
    SUMMARY_STMT *stmt_array;
    SUMMARY_CONTROL_DEPENDENCE *ctrl_dep_array;
    SUMMARY_FORMAL *formal_array;
    SUMMARY_ACTUAL *actual_array;
    SUMMARY_GLOBAL *global_array;
    SUMMARY_STID *global_stid_array;
    SUMMARY_COMMON *common_array;
    SUMMARY_COMMON_SHAPE *common_shape_array;
    SUMMARY_STRUCT_ACCESS * struct_access_array;
#ifdef KEY
    SUMMARY_TY_INFO * ty_info_array;
#endif
    SUMMARY_CONSTRAINT_GRAPH_NODE     *cg_nodes_array;
    SUMMARY_CONSTRAINT_GRAPH_EDGE     *cg_edges_array;
    SUMMARY_CONSTRAINT_GRAPH_STINFO   *cg_stinfos_array;
    SUMMARY_CONSTRAINT_GRAPH_CALLSITE *cg_callsites_array;
    SUMMARY_CONSTRAINT_GRAPH_MODRANGE *cg_modranges_array;
    UINT32 *cg_node_ids_array;

    ARRAY_SUMMARY_OUTPUT array_summary(Malloc_Mem_Pool);

    const char *section_base = (char *)sbase;

    Elf64_Word* offset = (Elf64_Word*)section_base;

    file_header = (SUMMARY_FILE_HEADER*)(section_base + *offset);
    
    if (file_header == 0)
	return;

    fprintf ( (FILE *)f, "IPA SUMMARY REVISION -- %d.%d \n\n", 
		file_header->Get_version_number(), file_header->Get_minor_version_number() );

    fprintf ( (FILE*)f, "OPT LEVEL-- O%d \n", file_header->Get_opt_level() );

    fprintf (f, "      Summary type        offset             size\n\n");

    const char * const format = "%-24s 0x%06x   0x%06x * %-5d\t= 0x%08x\n";

    if (file_header->Get_symbol_size () != 0)
	fprintf (f, format, "SYMBOL",
		 file_header->Get_symbol_offset (),
		 file_header->Get_symbol_entry_size (),
		 file_header->Get_symbol_size(),
		 file_header->Get_symbol_entry_size () *
		 file_header->Get_symbol_size ());
    
    if (file_header->Get_proc_size ())
	fprintf (f, format, "PROCEDURE",
		 file_header->Get_proc_offset (),
		 file_header->Get_proc_entry_size (),
		 file_header->Get_proc_size(),
		 file_header->Get_proc_entry_size () *
		 file_header->Get_proc_size ());
    
    if (file_header->Get_feedback_size ())
	fprintf (f, format, "FEEDBACK",
		 file_header->Get_feedback_offset (),
		 file_header->Get_feedback_entry_size (),
		 file_header->Get_feedback_size(),
		 file_header->Get_feedback_entry_size () *
		 file_header->Get_feedback_size ());
    
    if (file_header->Get_callsite_size ())
	fprintf (f, format, "CALLSITE",
		 file_header->Get_callsite_offset (),
		 file_header->Get_callsite_entry_size (),
		 file_header->Get_callsite_size(),
		 file_header->Get_callsite_entry_size () *
		 file_header->Get_callsite_size ());
    
    if (file_header->Get_stmt_size ())
	fprintf (f, format, "STMT",
		 file_header->Get_stmt_offset (),
		 file_header->Get_stmt_entry_size (),
		 file_header->Get_stmt_size(),
		 file_header->Get_stmt_entry_size () *
		 file_header->Get_stmt_size ());
    
    if (file_header->Get_ctrl_dep_size ())
	fprintf (f, format, "CTRL_DEP",
		 file_header->Get_ctrl_dep_offset (),
		 file_header->Get_ctrl_dep_entry_size (),
		 file_header->Get_ctrl_dep_size(),
		 file_header->Get_ctrl_dep_entry_size () *
		 file_header->Get_ctrl_dep_size ());
    
    if (file_header->Get_formal_size ())
	fprintf (f, format, "FORMAL",
		 file_header->Get_formal_offset (),
		 file_header->Get_formal_entry_size (),
		 file_header->Get_formal_size(),
		 file_header->Get_formal_entry_size () *
		 file_header->Get_formal_size ());
    
    if (file_header->Get_actual_size ())
	fprintf (f, format, "ACTUAL",
		 file_header->Get_actual_offset (),
		 file_header->Get_actual_entry_size (),
		 file_header->Get_actual_size(),
		 file_header->Get_actual_entry_size () *
		 file_header->Get_actual_size ());
    
    if (file_header->Get_value_size ())
	fprintf (f, format, "VALUE",
		 file_header->Get_value_offset (),
		 file_header->Get_value_entry_size (),
		 file_header->Get_value_size(),
		 file_header->Get_value_entry_size () *
		 file_header->Get_value_size ());
    
    if (file_header->Get_expr_size ())
	fprintf (f, format, "EXPR",
		 file_header->Get_expr_offset (),
		 file_header->Get_expr_entry_size (),
		 file_header->Get_expr_size(),
		 file_header->Get_expr_entry_size () *
		 file_header->Get_expr_size ());
    
    if (file_header->Get_phi_size ())
	fprintf (f, format, "PHI",
		 file_header->Get_phi_offset (),
		 file_header->Get_phi_entry_size (),
		 file_header->Get_phi_size(),
		 file_header->Get_phi_entry_size () *
		 file_header->Get_phi_size ());
    
    if (file_header->Get_chi_size ())
	fprintf (f, format, "CHI",
		 file_header->Get_chi_offset (),
		 file_header->Get_chi_entry_size (),
		 file_header->Get_chi_size(),
		 file_header->Get_chi_entry_size () *
		 file_header->Get_chi_size ());
    
    if (file_header->Get_global_size ())
	fprintf (f, format, "GLOBAL",
		 file_header->Get_global_offset (),
		 file_header->Get_global_entry_size (),
		 file_header->Get_global_size(),
		 file_header->Get_global_entry_size () *
		 file_header->Get_global_size ());
    
    if (file_header->Get_common_size ())
	fprintf (f, format, "COMMON",
		 file_header->Get_common_offset (),
		 file_header->Get_common_entry_size (),
		 file_header->Get_common_size(),
		 file_header->Get_common_entry_size () *
		 file_header->Get_common_size ());
    
    if (file_header->Get_common_shape_size ())
	fprintf (f, format, "COMMON_SHAPE",
		 file_header->Get_common_shape_offset (),
		 file_header->Get_common_shape_entry_size (),
		 file_header->Get_common_shape_size(),
		 file_header->Get_common_shape_entry_size () *
		 file_header->Get_common_shape_size ());

    if (file_header->Get_global_stid_size ())
	fprintf (f, format, "GLOBAL_STID",
		 file_header->Get_global_stid_offset (),
		 file_header->Get_global_stid_entry_size (),
		 file_header->Get_global_stid_size(),
		 file_header->Get_global_stid_entry_size () *
		 file_header->Get_global_stid_size ());
    
    if (file_header->Get_scalar_node_size ())
	fprintf (f, format, "SCALAR_NODE",
		 file_header->Get_scalar_offset (),
		 file_header->Get_scalar_node_entry_size (),
		 file_header->Get_scalar_node_size(),
		 file_header->Get_scalar_node_entry_size () *
		 file_header->Get_scalar_node_size ());
    
    if (file_header->Get_cfg_node_size ())
	fprintf (f, format, "CFG_NODE",
		 file_header->Get_cfg_node_offset (),
		 file_header->Get_cfg_node_entry_size (),
		 file_header->Get_cfg_node_size(),
		 file_header->Get_cfg_node_entry_size () *
		 file_header->Get_cfg_node_size ());
    
    if (file_header->Get_regions_array_size ())
	fprintf (f, format, "REGIONS_ARRAY",
		 file_header->Get_regions_array_offset (),
		 file_header->Get_regions_array_entry_size (),
		 file_header->Get_regions_array_size(),
		 file_header->Get_regions_array_entry_size () *
		 file_header->Get_regions_array_size ());
    
    if (file_header->Get_projected_region_size ())
	fprintf (f, format, "PROJECTED_REGION",
		 file_header->Get_projected_region_offset (),
		 file_header->Get_projected_region_entry_size (),
		 file_header->Get_projected_region_size(),
		 file_header->Get_projected_region_entry_size () *
		 file_header->Get_projected_region_size ());
    
    if (file_header->Get_projected_array_size ())
	fprintf (f, format, "PROJECTED_ARRAY",
		 file_header->Get_projected_array_offset (),
		 file_header->Get_projected_array_entry_size (),
		 file_header->Get_projected_array_size(),
		 file_header->Get_projected_array_entry_size () *
		 file_header->Get_projected_array_size ());
    
    if (file_header->Get_term_array_size ())
	fprintf (f, format, "TERM_ARRAY",
		 file_header->Get_term_array_offset (),
		 file_header->Get_term_array_entry_size (),
		 file_header->Get_term_array_size(),
		 file_header->Get_term_array_entry_size () *
		 file_header->Get_term_array_size ());
    
    if (file_header->Get_ivar_size ())
	fprintf (f, format, "IVAR",
		 file_header->Get_ivar_offset (),
		 file_header->Get_ivar_entry_size (),
		 file_header->Get_ivar_size(),
		 file_header->Get_ivar_entry_size () *
		 file_header->Get_ivar_size ());
    
    if (file_header->Get_loopinfo_size ())
	fprintf (f, format, "LOOPINFO",
		 file_header->Get_loopinfo_offset (),
		 file_header->Get_loopinfo_entry_size (),
		 file_header->Get_loopinfo_size(),
		 file_header->Get_loopinfo_entry_size () *
		 file_header->Get_loopinfo_size ());
    if (file_header->Get_struct_access_size () != 0)
	fprintf (f, format, "FLD_ACCESS",
		 file_header->Get_struct_access_offset (),
		 file_header->Get_struct_access_entry_size (),
		 file_header->Get_struct_access_size(),
		 file_header->Get_struct_access_entry_size () *
		 file_header->Get_struct_access_size ());
#ifdef KEY
    if (file_header->Get_ty_info_size () != 0)
	fprintf (f, format, "TY_INFO",
		 file_header->Get_ty_info_offset (),
		 file_header->Get_ty_info_entry_size (),
		 file_header->Get_ty_info_size(),
		 file_header->Get_ty_info_entry_size () *
		 file_header->Get_ty_info_size ());
#endif
    // Constraint graph summary for Nystrom Alias Analyzer
    if (file_header->Get_constraint_graph_nodes_size () != 0)
	fprintf (f, format, "CG NODE",
		 file_header->Get_constraint_graph_nodes_offset (),
		 file_header->Get_constraint_graph_nodes_entry_size (),
		 file_header->Get_constraint_graph_nodes_size(),
		 file_header->Get_constraint_graph_nodes_entry_size () *
		 file_header->Get_constraint_graph_nodes_size ());

    if (file_header->Get_constraint_graph_edges_size () != 0)
	fprintf (f, format, "CG EDGES",
		 file_header->Get_constraint_graph_edges_offset (),
		 file_header->Get_constraint_graph_edges_entry_size (),
		 file_header->Get_constraint_graph_edges_size(),
		 file_header->Get_constraint_graph_edges_entry_size () *
		 file_header->Get_constraint_graph_edges_size ());

    if (file_header->Get_constraint_graph_stinfos_size () != 0)
	fprintf (f, format, "CG STINFOS",
		 file_header->Get_constraint_graph_stinfos_offset (),
		 file_header->Get_constraint_graph_stinfos_entry_size (),
		 file_header->Get_constraint_graph_stinfos_size(),
		 file_header->Get_constraint_graph_stinfos_entry_size () *
		 file_header->Get_constraint_graph_stinfos_size ());

    if (file_header->Get_constraint_graph_callsites_size () != 0)
	fprintf (f, format, "CG CALLSITES",
		 file_header->Get_constraint_graph_callsites_offset (),
		 file_header->Get_constraint_graph_callsites_entry_size (),
		 file_header->Get_constraint_graph_callsites_size(),
		 file_header->Get_constraint_graph_callsites_entry_size () *
		 file_header->Get_constraint_graph_callsites_size ());

    if (file_header->Get_constraint_graph_node_ids_size () != 0)
	fprintf (f, format, "CGNODE IDS",
		 file_header->Get_constraint_graph_node_ids_offset (),
		 file_header->Get_constraint_graph_node_ids_entry_size (),
		 file_header->Get_constraint_graph_node_ids_size(),
		 file_header->Get_constraint_graph_node_ids_entry_size () *
		 file_header->Get_constraint_graph_node_ids_size ());

    if (file_header->Get_constraint_graph_modranges_size () != 0)
	fprintf (f, format, "CG MODRANGES",
		 file_header->Get_constraint_graph_modranges_offset (),
		 file_header->Get_constraint_graph_modranges_entry_size (),
		 file_header->Get_constraint_graph_modranges_size(),
		 file_header->Get_constraint_graph_modranges_entry_size () *
		 file_header->Get_constraint_graph_modranges_size ());
    
    if (file_header->Get_symbol_size() != 0) {
	sym_array = (SUMMARY_SYMBOL *)
	    (section_base + file_header->Get_symbol_offset());
	Ipl_Summary_Symbol = sym_array;
	sym_array->Print_array ( f, file_header->Get_symbol_size(),
          symbol_names, function_names );

    }

    if (file_header->Get_proc_size() != 0) {
	proc_array =  (SUMMARY_PROCEDURE *)
	    (section_base + file_header->Get_proc_offset());
	proc_array->Print_array ( f, file_header->Get_proc_size() );
    }

    if (file_header->Get_feedback_size() != 0) {
	feedback_array =  (SUMMARY_FEEDBACK *)
	    (section_base + file_header->Get_feedback_offset());
	feedback_array->Print_array ( f, file_header->Get_feedback_size() );
    }

    if (file_header->Get_callsite_size() != 0) {
	callsite_array = (SUMMARY_CALLSITE*)
	    (section_base + file_header->Get_callsite_offset()); 
	callsite_array->Print_array (f, file_header->Get_callsite_size());
    }

    if (file_header->Get_actual_size() != 0) {
	actual_array = (SUMMARY_ACTUAL *)
	    (section_base + file_header->Get_actual_offset());  
	actual_array->Print_array ( f, file_header->Get_actual_size() );
    }

    if (file_header->Get_value_size() != 0) {
	value_array = (SUMMARY_VALUE *) (section_base +
					 file_header->Get_value_offset());  
	value_array->Print_array ( f, file_header->Get_value_size() );
    }

    if (file_header->Get_expr_size() != 0) {
	expr_array = (SUMMARY_EXPR *) (section_base +
				       file_header->Get_expr_offset());  
	expr_array->Print_array ( f, file_header->Get_expr_size() );
    }

    if (file_header->Get_phi_size() != 0) {
	phi_array = (SUMMARY_PHI *) (section_base +
				     file_header->Get_phi_offset());  
	phi_array->Print_array ( f, file_header->Get_phi_size() );
    }

    if (file_header->Get_chi_size() != 0) {
	chi_array = (SUMMARY_CHI *) (section_base +
				     file_header->Get_chi_offset());  
	chi_array->Print_array ( f, file_header->Get_chi_size() );
    }

    if (file_header->Get_stmt_size() != 0) {
	stmt_array = (SUMMARY_STMT *) (section_base +
				       file_header->Get_stmt_offset());  
	stmt_array->Print_array ( f, file_header->Get_stmt_size() );
    }

    if (file_header->Get_ctrl_dep_size() != 0) {
	ctrl_dep_array = (SUMMARY_CONTROL_DEPENDENCE *)
	    (section_base + file_header->Get_ctrl_dep_offset());  
	ctrl_dep_array->Print_array ( f, file_header->Get_ctrl_dep_size() );
    }

    if (file_header->Get_formal_size() != 0) {
	formal_array = (SUMMARY_FORMAL *) (section_base +
					   file_header->Get_formal_offset()); 
	formal_array->Print_array ( f, file_header->Get_formal_size() );
    }

    if (file_header->Get_global_size() != 0) {
	global_array = (SUMMARY_GLOBAL *)
	    (section_base + file_header->Get_global_offset());
	global_array->Print_array ( f, file_header->Get_global_size() );
    }

    if (file_header->Get_global_stid_size() != 0) {
      global_stid_array = (SUMMARY_STID *)
	(section_base + file_header->Get_global_stid_offset());
      global_stid_array->Print_array ( f, file_header->Get_global_stid_size() );
    }

    if (file_header->Get_common_size() != 0) {
	common_array = (SUMMARY_COMMON *)
	    (section_base + file_header->Get_common_offset());
	common_array->Print_array ( f, file_header->Get_common_size() );
    }

    if (file_header->Get_common_shape_size() != 0) {
	common_shape_array = (SUMMARY_COMMON_SHAPE *)
	    (section_base + file_header->Get_common_shape_offset());
	common_shape_array->Print_array(f,
					file_header->Get_common_shape_size() );

    }
    if (file_header->Get_struct_access_size() != 0) {
	struct_access_array =  (SUMMARY_STRUCT_ACCESS *)
	    (section_base + file_header->Get_struct_access_offset());
	struct_access_array->Print_array ( f, file_header->Get_struct_access_size() );
    }    
#ifdef KEY
    if (file_header->Get_ty_info_size() != 0) {
	ty_info_array =  (SUMMARY_TY_INFO *)
	    (section_base + file_header->Get_ty_info_offset());
	ty_info_array->Print_array ( f, file_header->Get_ty_info_size() );
    }
#endif
    // Constraint graph summary for Nystrom Alias Analyzer
    if (file_header->Get_constraint_graph_nodes_size() != 0) {
	cg_nodes_array =  (SUMMARY_CONSTRAINT_GRAPH_NODE *)
	    (section_base + file_header->Get_constraint_graph_nodes_offset());
	cg_nodes_array->Print_array ( f, file_header->Get_constraint_graph_nodes_size() );
    }
    if (file_header->Get_constraint_graph_edges_size() != 0) {
	cg_edges_array =  (SUMMARY_CONSTRAINT_GRAPH_EDGE *)
	    (section_base + file_header->Get_constraint_graph_edges_offset());
	cg_edges_array->Print_array ( f, file_header->Get_constraint_graph_edges_size() );
    }
    if (file_header->Get_constraint_graph_stinfos_size() != 0) {
	cg_stinfos_array =  (SUMMARY_CONSTRAINT_GRAPH_STINFO *)
	    (section_base + file_header->Get_constraint_graph_stinfos_offset());
	cg_stinfos_array->Print_array ( f, file_header->Get_constraint_graph_stinfos_size() );
    }
    if (file_header->Get_constraint_graph_callsites_size() != 0) {
	cg_callsites_array =  (SUMMARY_CONSTRAINT_GRAPH_CALLSITE *)
	    (section_base + file_header->Get_constraint_graph_callsites_offset());
	cg_callsites_array->Print_array ( f, file_header->Get_constraint_graph_callsites_size() );
    }
    if (file_header->Get_constraint_graph_node_ids_size() != 0) {
	cg_node_ids_array =  (UINT32 *)
           (section_base + file_header->Get_constraint_graph_node_ids_offset());
        for (INT i=0; i<file_header->Get_constraint_graph_node_ids_size(); ++i) {
          fprintf(f, " %d ",  cg_node_ids_array[i]);
        }
    }
    if (file_header->Get_constraint_graph_modranges_size() != 0) {
	cg_modranges_array =  (SUMMARY_CONSTRAINT_GRAPH_MODRANGE *)
          (section_base + file_header->Get_constraint_graph_modranges_offset());
	cg_modranges_array->Print_array ( f, file_header->Get_constraint_graph_modranges_size() );
    }
    
    array_summary.Trace(f, sbase);
}



//---------------------------------------------------------------
// Higher-level interface for ipl to trace summary info.
//---------------------------------------------------------------

void
IPA_Trace_Summary_File (FILE *f, 
		        Output_File *fl, 
			BOOL verbose,
			DYN_ARRAY<char*>* symbol_names,
			DYN_ARRAY<char*>* function_names)
{
  Section *section = NULL;
  INT i;

  // Find the IPA summary section
  for (i = 0; i < fl->num_of_section; i++) {
    if ((fl->section_list[i].shdr.sh_info == WT_IPA_SUMMARY) &&
        (strcmp (fl->section_list[i].name, MIPS_WHIRL_SUMMARY) == 0)) {
      section = fl->section_list + i;
      break;
    }
  }

  if ( verbose ) {
    fprintf ( (FILE *)f, "TRACING SUMMARY INFORMATION \n" );
  }

  if (section == NULL) {
    fprintf ( (FILE *)f, "--- Empty summary section ---\n" );
  }
  else {
    char *summary_base = (char *)(fl->map_addr + section->shdr.sh_offset);
    IPA_Trace_Summary_Section ( f, summary_base, symbol_names, function_names );
  }
}


// ----------------------------------------------------------------
// Print summary info arrays: similar to IPA_Trace_Summary_Section, 
// but has simpler interface and doesn't print header info.
// ----------------------------------------------------------------
template<>
void 
SUMMARIZE<IPL>::Trace(FILE* fp)
{
  if (Has_symbol_entry()) {
    Ipl_Summary_Symbol = Get_symbol(0);
    Ipl_Summary_Symbol->Print_array(fp, Get_symbol_idx()+1);
  }

  if (Has_procedure_entry())
    Get_procedure(0)->Print_array(fp, Get_procedure_idx()+1);
  
  if (Has_callsite_entry())
    Get_callsite(0)->Print_array(fp, Get_callsite_idx()+1);

  if (Has_feedback_entry())
    Get_feedback(0)->Print_array(fp, Get_feedback_idx()+1);

  if (Has_actual_entry())
    Get_actual(0)->Print_array(fp, Get_actual_idx()+1);
  
  if (Has_value_entry())
    Get_value(0)->Print_array(fp, Get_value_idx()+1);

  if (Has_expr_entry())
    Get_expr(0)->Print_array(fp, Get_expr_idx()+1);
  
  if (Has_phi_entry())
    Get_phi(0)->Print_array(fp, Get_phi_idx()+1);

  if (Has_chi_entry())
    Get_chi(0)->Print_array(fp, Get_chi_idx()+1);
  
  if (Has_stmt_entry())
    Get_stmt(0)->Print_array(fp, Get_stmt_idx()+1);

  if (Has_ctrl_dep_entry())
    Get_ctrl_dep(0)->Print_array(fp, Get_ctrl_dep_idx()+1);

  if (Has_formal_entry())
    Get_formal(0)->Print_array(fp, Get_formal_idx()+1);
  
  if (Has_global_entry())
    Get_global(0)->Print_array(fp, Get_global_idx()+1);

  if (Has_global_stid_entry())
    Get_global_stid(0)->Print_array(fp, Get_global_stid_idx()+1);

  if (Has_common_entry())
    Get_common(0)->Print_array(fp, Get_common_idx()+1);

  if (Has_common_shape_entry())
    Get_common_shape(0)->Print_array(fp, Get_common_shape_idx()+1);
  if (Has_struct_access_entry())
    Get_struct_access(0)->Print_array(fp, Get_struct_access_idx()+1);
#ifdef KEY
  if (Has_ty_info_entry())
    Get_ty_info(0)->Print_array(fp, Get_ty_info_idx()+1);
#endif
  // Constraint graph summary for Nystrom Alias Analyzer
  if (Has_constraint_graph_nodes()) {
    Get_constraint_graph_node(0)->Print_array(fp, 
                                         Get_constraint_graph_nodes_idx()+1);
  }
  if (Has_constraint_graph_edges()) {
    Get_constraint_graph_edge(0)->Print_array(fp, 
                                         Get_constraint_graph_edges_idx()+1);
  }
  if (Has_constraint_graph_stinfos()) {
    Get_constraint_graph_stinfo(0)->Print_array(fp, 
                                         Get_constraint_graph_stinfos_idx()+1);
  }
  if (Has_constraint_graph_callsites()) {
    Get_constraint_graph_callsite(0)->Print_array(fp, 
                                         Get_constraint_graph_callsites_idx()+1);
  }
  if (Has_constraint_graph_node_ids()) {
    fprintf (fp, "%sStart cgnode node ids array\n%s", SBar, SBar);
    for (INT i=0; i<(Get_constraint_graph_node_ids_idx()+1); ++i) {
      fprintf(fp, " %d ",  _constraint_graph_node_ids[i]);
    }
    fprintf ( fp, "\n%sEnd cgnode node ids array \n%s", SBar, SBar );
  }
  if (Has_constraint_graph_modranges()) {
    Get_constraint_graph_modrange(0)->Print_array(fp, 
                                      Get_constraint_graph_modranges_idx()+1);
  }
}


