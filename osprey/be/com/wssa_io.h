/*

  Copyright (C) 2010, Hewlett-Packard Development Company, L.P. All Rights Reserved.

  Open64 is free software; you can redistribute it and/or
  modify it under the terms of the GNU General Public License
  as published by the Free Software Foundation; either version 2
  of the License, or (at your option) any later version.

  Open64 is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
  MA  02110-1301, USA.

*/

//====================================================================
//
// Module: wssa_io.h
//
// Revision history:
//  Nov-10 - Original Version
//
// Description:
//  Interface of I/O for WHIRL SSA
//
// Exported classes:
//  WSSA::WHIRL_SSA_IO
//
// SEE ALSO:
//  be/com/wssa_mgr.h (WHIRL_SSA_MANAGER)
//
//====================================================================

#ifndef wssa_io_INCLUDED
#define wssa_io_INCLUDED

#include "wssa_mgr.h"
#include "pu_info.h"
#include "ir_bwrite.h"
#include "ir_bcom.h"

namespace WSSA {

class WHIRL_SSA_IO
{
private:
  WHIRL_SSA_MANAGER* _mgr;

public:
  WHIRL_SSA_IO(WHIRL_SSA_MANAGER* mgr) : _mgr(mgr) {}

  // write ssa into file
  void Write_To_Output_File(Output_File *fl);

  // read ssa from file
  void Read_SSA_From_File(char *base);

};

}
#endif  /* wssa_io_INCLUDED */
