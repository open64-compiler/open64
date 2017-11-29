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


// ====================================================================
// ====================================================================
//
// Module: ipa_df.h
//
// Revision history:
//  19-Sep-95 - Housekeeping
//
// Description:
//
// This package provides a generic interface for solving dataflow
// problems over the call graph.
//
// ====================================================================
// ====================================================================

#ifndef cxx_ipa_df_INCLUDED
#define cxx_ipa_df_INCLUDED

/* specify the direction of the dataflow problem */
enum DF_DIRECTION {
  FORWARD, 
  BACKWARD
};


/* every dataflow problem must first initialize this structure          */
/* note, the dataflow solver will initialize the entry and exit nodes   */
/* and the depth first ordering of vertices is initialized on a demand  */
/* driven fashion. That way it is created only once if several dataflow */
/* problems are solved on the same graph                                */
 
struct DFS {
  DF_DIRECTION di;    /* data flow direction                         */
  NODE_INDEX entry, exit; /* entry and exit nodes that connect the entire graph */
  DFN *d;             /* depth first ordering of vertices            */
  INT change;         /* change field, determine if dataflow problem */
                      /* has settled                                 */

/* array of in annotations, one for each node in the graph, it is    */
/* used to hold the result of the meet operation                     */
  void* *in;

/* array of out annotations, one for each node in the graph, it is   */
/* used to hold the result of the trans operation                    */
  void* *out;

};


/* access macros */

#define DFS_di(df)       ((df)->di)
#define DFS_d(df)        ((df)->d)
#define DFS_entry(df)    ((df)->entry)
#define DFS_exit(df)     ((df)->exit)
#define DFS_in(df)       ((df)->in)
#define DFS_out(df)      ((df)->out)
#define DFS_in_i(df,i)   ((df)->in[i])
#define DFS_out_i(df,i)  ((df)->out[i])
#define DFS_change(df)   ((df)->change)


class IPA_NODE;
class IPA_EDGE;

class IPA_DATA_FLOW
{
 private:

  DF_DIRECTION d;
  DFS *df;

  void iterative_solver(DFS*);
  void dataflow(DFS*);

 protected:

  MEM_POOL *m;

 public:

  IPA_DATA_FLOW (DF_DIRECTION df, MEM_POOL *m);

  IPA_NODE* Get_caller(IPA_EDGE *callee);
  IPA_NODE* Get_callee(IPA_EDGE *callee);
  IPA_NODE* Clone(IPA_NODE *n);

  void Init();
  void Print(FILE* fp);

  virtual void Solve();
  virtual void* Meet(void* in,void* vertex, INT *change);
  virtual void* Trans(void* in, void* out, void* vertex, INT *change);
  virtual void InitializeNode(void*) {};
  virtual void Print_entry(FILE *fp, void* out, void* n);
  virtual void PostProcessIO(void *){};
};

#endif /* cxx_ipa_df_INCLUDED */
