/*
 * Copyright 2005-2008 NVIDIA Corporation.  All rights reserved.
 */

#ifndef opt_lab_INCLUDED
#define opt_lab_INCLUDED
#include "opt_base.h"

class WN;

// This class is for removing unreferenced labels in a program unit
// This is needed for some configurations that generate C using whirl2c 
// and where host compilers emit warnings on unreferenced labels.
//
// The basic idea of this phase is to mark all labels that are referenced
// in goto and other control transfer nodes, and then remove all labels that
// were not marked, kind of like garbage collection.
//
class LabelOpt {
  MAP       * _label_map;
  MEM_POOL    _label_mem_pool;

private:

  void Set_Mark(INT32 num);
  BOOL Get_Mark(INT32 num);
  void Mark_Referenced_Labels(WN * wn);
  void Remove_Unmarked_Labels(WN * wn);

public:

  LabelOpt();
  ~LabelOpt();

  // call this on the function unit to remove any unreferenced labels
  //
  static void Remove_Unreferenced_Labels(WN * wn);
};

#endif /* opt_lab_INCLUDED */
