/*
 * Copyright (C) 2009 Advanced Micro Devices, Inc.  All Rights Reserved.
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


#ifndef ir_reader_INCLUDED
#define ir_reader_INCLUDED
/* ====================================================================
 * ====================================================================
 *
 * Module: ir_reader.h
 * $Revision: 1.1.1.1 $
 * $Date: 2005/10/21 19:00:00 $
 * $Author: marcel $
 * $Source: /proj/osprey/CVS/open64/osprey1.0/common/com/ir_reader.h,v $
 *
 * Revision history:
 *  31-AUG-94 - Original Version
 *
 * Description:
 *
 * ====================================================================
 * ====================================================================
 */


#ifdef _KEEP_RCS_ID
static char *opt_irrcs_id = "$Source: /proj/osprey/CVS/open64/osprey1.0/common/com/ir_reader.h,v $ $Revision: 1.1.1.1 $";
#endif /* _KEEP_RCS_ID */

extern void IR_reader_init(void);
extern void IR_reader_finish(void);
extern FILE *IR_open(char *filename);
extern void IR_close(void);
extern BOOL IR_open_output(char *filename);
extern void IR_close_output(void);
extern WN * IR_get_func(void);
extern void IR_put_func(WN * wn, FILE *f);
extern void IR_Dwarf_Gen_File_Table (BOOL dump_filenames);
extern void IR_Srcpos_Filename (SRCPOS srcpos,         /* in */
				const char **fname,    /* out */
				const char **dirname); /* out */

/* for debugging */
struct ALIAS_MANAGER;    /* forward declaration */

extern void Check_for_IR_Dump(INT phase, WN *pu, const char *phase_name);
extern void Check_for_IR_Dump_Before_Phase(INT phase, WN *pu, const char *phase_name);

extern void dump_wn(WN *wn);
extern void dump_tree(WN *wn);
extern void dump_region_tree(WN *wn);
extern void dump_wn_no_st(WN *wn);
extern void fdump_wn_no_st(FILE *fp,WN *wn);
extern void dump_tree_no_st(WN *wn);
extern void fdump_tree_with_alias(FILE *fp, const WN *wn, WN_MAP map, const struct ALIAS_MANAGER *);
extern void fdump_tree_with_freq(FILE *fp, const WN *wn, WN_MAP map);
/* dump an WN node to a file other than stdout */
extern void fdump_wn(FILE *f, WN *wn);
/* dump a tree to a file other than stdout */
extern void fdump_tree(FILE *f, WN *wn);
/* dump a tree to a file other than stdout, and do not print STs */
extern void fdump_tree_no_st(FILE *f, WN *wn);
/* dump a tree to a file, with region info */
extern void fdump_region_tree(FILE *f, WN *wn);
#ifdef BACK_END
extern void fdump_dep_tree( FILE *f, WN *wn, struct ALIAS_MANAGER *alias);
#endif 
/* dump a tree that contains regions of OPs */
extern "C" { void CG_Dump_Region(FILE *f, WN *); }

/* whether to dump the map info; defaults to FALSE */
extern BOOL IR_dump_map_info;
/* whether to dump the region OPs info; defaults to FALSE */
extern BOOL IR_dump_region;
/* whether to dump the line numbers; defaults to FALSE */
extern BOOL IR_dump_line_numbers;

/* whether to dump in prefix order or postfix order */
extern BOOL IR_set_dump_order( BOOL dump_prefix);

/* enable dumping frequency */
extern void enable_tree_freq_display(void);
/* disable dumping frequency */
extern void disable_tree_freq_display(void);

/* WN_TREE related dump functions */
extern void WN_TREE_put_func(WN * , FILE *);
extern void WN_TREE_dump_tree(WN *wn);
/* dump a tree to a file other than stdout */
extern void WN_TREE_fdump_tree(FILE *f, WN *wn);

#endif /* ir_reader_INCLUDED */

