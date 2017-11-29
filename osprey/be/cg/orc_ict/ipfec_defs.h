/*
  Copyright (C) 2000-2003, Institute of Computing Technology, Chinese Academy of Sciences
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
// Module: ipfec_defs.h
// $Date: 2005/10/21 19:00:00 $
// $Author: marcel $
// $Source: /proj/osprey/CVS/open64/osprey1.0/be/cg/orc_ict/ipfec_defs.h,v $
//
// Description:
//
// Interface to ORC Instruction Scheduler.
//
//*********************************************************************

#ifndef ipfec_defs_INCLUDED
#define ipfec_defs_INCLUDED

/*
 * Tracing flags:  -ttPHASE-NUMBER:FLAG
 */

/* TP_IPFEC (phase-number) */
#define TT_IPFEC_GRAPHIC        0x0001

/* dump flags for region formation */
#define TT_RGN_TREE_DUMP 	      0x0001 /* dump region tree */
#define TT_RGN_CFG_DUMP         0x0002 /* dump regional cfg */
#define TT_RGN_SUMMERY          0x0004 /* print summery info  */
#define TT_RGN_DETAILED         0x0008 /* print detailed info */
#define TT_RGN_DEBUG            0x0010
#define TT_RGN_UPDATE_DEBUG     0x0020
#define TT_RGN_VERIFY_DEBUG     0x0040
#define TT_RGN_UTIL_DEBUG       0x0080

/* dump flags for if_conversion*/
#define  TT_IF_CONV_SUMMARY     0x0010
#define  TT_IF_CONV_DETAILED	  0x0020
#define  TT_IF_CONV_GRAPHIC     0x0040

/* dump flags for prdb*/
#define  TT_PRDB_VERBOSE        0x0001
#define  TT_PRDB_APP              0x0002 /*for PRDB testing dump */

/* dump flags for profiling*/
#define  TT_PROF_FEEDBACK_DUMP   0x0001
#define  TT_PROF_CFG             0x0002 /* print cfg */
#define  TT_VALUE_PROF           0x0004 /* value profile*/

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

/* Dump flags for recovery block generation */
#define TT_RBG_DRAW_GLBL_CFG    0x0001 /* draw global CFG  */


/* Visualization flags for daVinci */
#define CFG_LABEL_VT_FLAG       0x0100 /* dump edge label           */
#define DAG_BR_VT_FLAG          0x0200 /* dump branch dependence    */

#endif
