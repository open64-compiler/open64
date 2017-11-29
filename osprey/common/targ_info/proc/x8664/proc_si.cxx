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

extern void Generate_Opteron(void);
extern void Generate_Barcelona(void);
extern void Generate_Core(void);
extern void Generate_EM64T(void);
extern void Generate_Orochi(void);
extern void Generate_Wolfdale(void);

int main (int argc, char *argv[])
{
  int current_slot = 0;

  Targ_SI();
  Generate_Opteron();
  Generate_Barcelona();
  Generate_Core();
  Generate_EM64T();
  Generate_Orochi();
  Generate_Wolfdale();
  Targ_SI_Done("targ_si");

  return 0;
}
