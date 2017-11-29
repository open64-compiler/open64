/*
  proc_si.cxx: scheduling info generator program

  Copyright (C) 2010 Advanced Micro Devices, Inc.  All Rights Reserved.

  Open64 is free software; you can redistribute it and/or modify it
  under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License,
  or (at your option) any later version.

  Open64 is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
  02110-1301, USA.

*/
#include "si_gen.h"

extern void Generate_SL1_PCore(void);
extern void Generate_SL1_DSP(void);
extern void Generate_SL2_PCore(void);
extern void Generate_SL2_MCore(void);
extern void Generate_SL5(void);

int main (int argc, char *argv[])
{
  int current_slot = 0;

  Targ_SI();
  Generate_SL1_PCore();
  Generate_SL1_DSP();
  Generate_SL2_PCore();
  Generate_SL2_MCore();
  Generate_SL5();
  Targ_SI_Done("targ_si");

  return 0;
}
