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

#ifndef ipfec_defs_INCLUDED
#define ipfec_defs_INCLUDED

/*
 * Tracing flags:  -ttPHASE-NUMBER:FLAG
 */

/* TP_IPFEC (phase-number) */
#define TT_IPFEC_GRAPHIC        0x0001

/* dump flags for region formation */
#define TT_RGN_TREE_DUMP 	0x0001 /* dump region tree */
#define TT_RGN_CFG_DUMP         0x0002 /* dump regional cfg */
#define TT_RGN_SUMMERY          0x0004 /* print summery info  */
#define TT_RGN_DETAILED         0x0008 /* print detailed info */
#define TT_RGN_DEBUG            0x0010
#define TT_RGN_UPDATE_DEBUG     0x0020
#define TT_RGN_VERIFY_DEBUG     0x0040
#define TT_RGN_UTIL_DEBUG       0x0080

/* dump flags for if_conversion*/
#define  TT_IF_CONV_SUMMARY     0x0010
#define  TT_IF_CONV_DETAILED	0x0020
#define  TT_IF_CONV_GRAPHIC     0x0040

/* dump flags for prdb*/
#define  TT_PRDB_VERBOSE        0x0001
#define  TT_PRDB_APP            0x0002 /*for PRDB testing dump */

/* dump flags for profiling*/
#define  TT_PROF_FEEDBACK_DUMP  0x0001
#define  TT_PROF_CFG            0x0002 /* print cfg */
#define  TT_VALUE_PROF          0x0004 /* value profile*/

/* Dump flags for instruction scheduling */
#define DUMP_IR                 0x0001 /* print IR         */
#define DUMP_DAG                0x0002 /* print DAG        */
#define DUMP_CAND               0x0004 /* print candidates */
#define SUMMARY_DUMP            0x0010 /* summary info     */
#define VERBOSE_DUMP            0x0020 /* verbose dump     */
#define DRAW_GLBL_CFG           0x0100 /* draw global CFG  */
#define DRAW_RGNL_CFG           0x0200 /* draw local CFG   */
#define DRAW_LOCAL_DAG          0x0400 /* draw local DAG   */
#define DRAW_RGNL_DAG           0x0800 /* draw regional DAG   */
#define TEST_SPEC               0x1000 /* do speculative code motion if possible */
#define DUMP_DELAY_SLOT         0x2000 /* dump code motion about delay slot*/
/* Dump flags for recovery block generation */
#define TT_RBG_DRAW_GLBL_CFG    0x0001 /* draw global CFG  */


/* Visualization flags for daVinci */
#define CFG_LABEL_VT_FLAG       0x0100 /* dump edge label           */
#define DAG_BR_VT_FLAG          0x0200 /* dump branch dependence    */

#endif
