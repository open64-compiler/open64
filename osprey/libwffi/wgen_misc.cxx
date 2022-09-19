/*
 * Copyright (C) 2021 Xcalibyte (Shenzhen) Limited.
 */

// ==================================================================
// misc variables or functions to fix unresolved symbol problem
// in loading wffi shared object
// ==================================================================

#include "defs.h"

BOOL Run_vsaopt = FALSE;
INT  Debug_Level = 0;

extern "C" const char *
Host_Format_Parm(INT kind, MEM_PTR parm)
{
  return "";
}

extern "C" void
Signal_Cleanup(INT sig)
{
}
