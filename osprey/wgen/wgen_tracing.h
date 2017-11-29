/* 
 * Copyright 2007 Google, Inc.  All Rights Reserved.
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of version 2 of the GNU General Public License as
 * published by the Free Software Foundation; either version 2, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it would be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  
 *
 * You should have received a copy of the GNU General Public License along
 * with this program; if not, write the Free Software Foundation, Inc., 59
 * Temple Place - Suite 330, Boston MA 02111-1307, USA.
 *
 */

/************************************************************
 * Tracing framework for wgen.
 * Outputs GIMPLE/WHIRL nodes to the trace file as
 * they are read/created.
 ************************************************************/

#ifndef wgen_tracing_INCLUDED
#define wgen_tracing_INCLUDED

#include "gspin-tree.h"
#include "opcode.h"

// TRACE_EXPAND_GS should be called at the top-level in a function which processes
// a GIMPLE node.  The macro is a bit of hackery to indent the lines according to the
// node's depth in the GIMPLE tree with minimal code changes to the wgen_XXX
// files.  It declares a WGEN_TRACE object whose constructor increments the static member
// gimple_depth (used for indenting) and prints out a GIMPLE node.  The destructor decrements
// gimple_depth.  So the number of WGEN_TRACE objects = WGEN_Expand recursion depth = indent amount
// should nicely track the current depth in GIMPLE tree depth.
// 
// Caveats:
//   -- macro declares an object.  Cannot be used twice in the same context.
#define TRACE_EXPAND_GS(n) WGEN_TRACE __tmp_wgen_tr(n)

// Intercept all WN_Create calls (declared in wn.h) and wrap them in
// WGEN_Trace_wn which outputs the WHIRL node to the trace file
#define WN_CreateStid(args...) WGEN_Trace_wn(WN_CreateStid(args))
#define WN_CreateBlock(args...) WGEN_Trace_wn(WN_CreateBlock(args))
#define WN_CreateDO(args...) WGEN_Trace_wn(WN_CreateDO(args))
#define WN_CreateDoWhile(args...) WGEN_Trace_wn(WN_CreateDoWhile(args))
#define WN_CreateWhileDo(args...) WGEN_Trace_wn(WN_CreateWhileDo(args))
#define WN_CreateIf(args...) WGEN_Trace_wn(WN_CreateIf(args))
#define WN_CreateEntry(args...) WGEN_Trace_wn(WN_CreateEntry (args))
#define WN_CreateRegion(args...) WGEN_Trace_wn(WN_CreateRegion(args))
#define WN_CreateRegionExit(args...) WGEN_Trace_wn(WN_CreateRegionExit (args))
#define WN_CreateGoto(args...) WGEN_Trace_wn(WN_CreateGoto(args))
#define WN_CreateGotoOuterBlock(args...) WGEN_Trace_wn(WN_CreateGotoOuterBlock (args))
#define WN_CreateAgoto(args...) WGEN_Trace_wn(WN_CreateAgoto(args))
#define WN_CreateAltentry(args...) WGEN_Trace_wn(WN_CreateAltentry(args))
#define WN_CreateTruebr(args...) WGEN_Trace_wn(WN_CreateTruebr(args))
#define WN_CreateFalsebr(args...) WGEN_Trace_wn(WN_CreateFalsebr(args))
#define WN_CreateReturn(args...) WGEN_Trace_wn(WN_CreateReturn(args))
#define WN_CreateReturn_Val(args...) WGEN_Trace_wn(WN_CreateReturn_Val (args))
#define WN_CreateLabel(args...) WGEN_Trace_wn(WN_CreateLabel(args))
#define WN_CreateCompgoto(args...) WGEN_Trace_wn(WN_CreateCompgoto(args))
#define WN_CreateSwitch(args...) WGEN_Trace_wn(WN_CreateSwitch(args))
#define WN_CreateCasegoto(args...) WGEN_Trace_wn(WN_CreateCasegoto(args))
#define WN_CreateXgoto(args...) WGEN_Trace_wn(WN_CreateXgoto (args))
#define WN_CreateIstore(args...) WGEN_Trace_wn(WN_CreateIstore(args))
#define WN_CreateIstorex(args...) WGEN_Trace_wn(WN_CreateIstorex(args))
#define WN_CreateMstore(args...) WGEN_Trace_wn(WN_CreateMstore(args))
#define WN_CreateStid(args...) WGEN_Trace_wn(WN_CreateStid(args))
#define WN_CreatePrefetch(args...) WGEN_Trace_wn(WN_CreatePrefetch(args))
#define WN_CreatePrefetchx(args...) WGEN_Trace_wn(WN_CreatePrefetchx(args))
#define WN_CreateIo(args...) WGEN_Trace_wn(WN_CreateIo(args))
#define WN_CreateIoItem0(args...) WGEN_Trace_wn(WN_CreateIoItem0(args))
#define WN_CreateIoItem1(args...) WGEN_Trace_wn(WN_CreateIoItem1(args))
#define WN_CreateIoItem2(args...) WGEN_Trace_wn(WN_CreateIoItem2(args))
#define WN_CreateIoItem3(args...) WGEN_Trace_wn(WN_CreateIoItem3(args))
#define WN_CreateIoItemN(args...) WGEN_Trace_wn(WN_CreateIoItemN(args))
#define WN_CreateEval(args...) WGEN_Trace_wn(WN_CreateEval(args))
#define WN_CreatePragma(args...) WGEN_Trace_wn(WN_CreatePragma(args))
#define WN_CreatePragma(args...) WGEN_Trace_wn(WN_CreatePragma(args))
#define WN_CreateXpragma(args...) WGEN_Trace_wn(WN_CreateXpragma(args))
#define WN_CreateExp0(args...) WGEN_Trace_wn(WN_CreateExp0(args))
#define WN_CreateExp1(args...) WGEN_Trace_wn(WN_CreateExp1(args))
#define WN_CreateExp2(args...) WGEN_Trace_wn(WN_CreateExp2(args))
#define WN_CreateExp3(args...) WGEN_Trace_wn(WN_CreateExp3(args))
#define WN_CreateIload(args...) WGEN_Trace_wn(WN_CreateIload(args))
#define WN_CreateIloadx(args...) WGEN_Trace_wn(WN_CreateIloadx(args))
#define WN_CreateMload(args...) WGEN_Trace_wn(WN_CreateMload(args))
#define WN_CreateLdid(args...) WGEN_Trace_wn(WN_CreateLdid(args))
#define WN_CreateLda(args...) WGEN_Trace_wn(WN_CreateLda(args))
#define WN_CreateIlda(args...) WGEN_Trace_wn(WN_CreateIlda(args))
#define WN_CreateLdaLabel(args...) WGEN_Trace_wn(WN_CreateLdaLabel(args))
#define WN_CreateIdname(args...) WGEN_Trace_wn(WN_CreateIdname(args))
#define WN_CreateConst(args...) WGEN_Trace_wn(WN_CreateConst (args))
#define WN_CreateIntconst(args...) WGEN_Trace_wn(WN_CreateIntconst(args))
#define WN_CreateCvtl(args...) WGEN_Trace_wn(WN_CreateCvtl(args))
#define WN_Create_Intrinsic(args...) WGEN_Trace_wn(WN_Create_Intrinsic(args))
#define WN_CreateParm(args...) WGEN_Trace_wn(WN_CreateParm(args))
#define WN_CreateComma(args...) WGEN_Trace_wn(WN_CreateComma(args))
#define WN_CreateRcomma(args...) WGEN_Trace_wn(WN_CreateRcomma(args))
#define WN_CreateComment(args...) WGEN_Trace_wn(WN_CreateComment (args))
#define WN_CreateAsm_Stmt(args...) WGEN_Trace_wn(WN_CreateAsm_Stmt (args))
#define WN_CreateAsm_Input(args...) WGEN_Trace_wn(WN_CreateAsm_Input (args))
#define WN_CreateLoopInfo(args...) WGEN_Trace_wn(WN_CreateLoopInfo (args))
#define WN_CreateExcScopeBegin(args...) WGEN_Trace_wn(WN_CreateExcScopeBegin(args))
#define WN_CreateExcScopeEnd(args...) WGEN_Trace_wn(WN_CreateExcScopeEnd(args))
#define WN_CreateBarrier(args...) WGEN_Trace_wn(WN_CreateBarrier (args))
#define WN_CreateTrap(args...) WGEN_Trace_wn(WN_CreateTrap (args))
#define WN_CreateAssert(args...) WGEN_Trace_wn(WN_CreateAssert (args))

WN* WGEN_Trace_wn(WN *);

class WGEN_TRACE
{
 public:
  static int gimple_depth;
  WGEN_TRACE(gs_t n) {
    trace_gs(n);
    gimple_depth++;
  }
  ~WGEN_TRACE() {
    gimple_depth--;
  }
  void trace_gs(gs_t n);
};

#endif
