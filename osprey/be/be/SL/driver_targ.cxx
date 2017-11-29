/*
 * Copyright (C) 2010 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

/********************************************************************\
|*                                                                  *|   
|*  Copyright (c) 2006 by SimpLight Nanoelectronics.                *|
|*  All rights reserved                                             *|
|*                                                                  *|
|*  This program is free software; you can redistribute it and/or   *|
|*  modify it under the terms of the GNU General Public License as  *|
|*  published by the Free Software Foundation; either version 2,    *|
|*  or (at your option) any later version.                          *|
|*                                                                  *|
\********************************************************************/

#include "defs.h"
#include "errors.h"
#include "config.h"
#include "config_targ.h"
#include "ti_init.h"

void Initialize_Targ_Info(void)
{
  ABI_PROPERTIES_ABI abi;
  ISA_SUBSET isa;
  PROCESSOR proc;

  switch (Target_ABI) {
  case ABI_N32:
    abi = ABI_PROPERTIES_ABI_n32;
    break;
  case ABI_N64:
    abi = ABI_PROPERTIES_ABI_n64;
    break;
  default:
    FmtAssert(FALSE, ("targinfo doesn't handle abi: %d\n", Target_ABI));
    /*NOTREACHED*/
  }

  switch (Target_ISA) {
  case TARGET_ISA_Mips64:
    isa = ISA_SUBSET_MIPS4;
    break;
  case TARGET_ISA_M4:
    isa = ISA_SUBSET_MIPS4;
    break;
  case TARGET_ISA_SL1:
    isa = ISA_SUBSET_SL1;
    break;
  case TARGET_ISA_SL2:
    isa = ISA_SUBSET_SL2;
    break;
  case TARGET_ISA_SL5:
    isa = ISA_SUBSET_SL5;
    break;
  default:
    FmtAssert(FALSE, ("targinfo doesn't handle isa: %s\n", Isa_Name(Target_ISA)));
    /*NOTREACHED*/
  }
  switch (Target) {
#ifdef TARG_SL
  case TARGET_sl1_pcore:
    proc = PROCESSOR_sl1_pcore;
    break;
  case TARGET_sl1_dsp:
    proc = PROCESSOR_sl1_dsp;
    break;
  case TARGET_sl2_pcore:
    proc = PROCESSOR_sl2_pcore;
    break;
  case TARGET_sl2_mcore:
    proc = PROCESSOR_sl2_mcore;
    break;
  case TARGET_sl5:
    proc = PROCESSOR_sl5;
    break;
#else

  case TARGET_R10K:
    proc = PROCESSOR_r10000;
    break;
  case TARGET_sb1:
    proc = PROCESSOR_sb1;
    break;
#endif
  default:
    FmtAssert(FALSE, ("targinfo doesn't handle target: %s\n", Targ_Name(Target)));
    /*NOTREACHED*/
  }

  TI_Initialize(abi, isa, proc);
}
