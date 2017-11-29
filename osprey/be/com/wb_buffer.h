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


#ifndef wb_buffer_INCLUDED
#define wb_buffer_INCLUDED "wb_buffer.h"

class WN;

const INT WB_BUFFER_MAX = 132; 

enum WB_SKIP_CLASS {
  WB_SKIP_NONE, 
  WB_SKIP_ALPHANUMERIC, 
  WB_SKIP_NUMERIC, 
  WB_SKIP_HEX,
};

class WB_BUFFER { 
  char _buffer[WB_BUFFER_MAX]; 
  INT _buffer_start; 
public: 
  WB_BUFFER() { _buffer_start = 0; _buffer[0] = '\0'; } 
  void Reset_Buffer() { _buffer_start = 0; } 
  void Load_Buffer(); 
  void Load_Buffer(const char s[]); 
  void Scan_Blanks_And_Tabs(); 
  void Skip_To_Separator(WB_SKIP_CLASS skip_type); 
  void Load_Loop(WN** wn_loop);
  void Load_UINT32(UINT32* int_value);
  void Load_mINT32(mINT32* value);
  void Load_mINT64(mINT64* value);
  void Load_Integer(INT* int_value); 
  void Load_Double(double* value);
  void Load_Boolean(BOOL* bool_value, BOOL default_present = FALSE,
    BOOL default_value = FALSE);
  void Skip_Chars(INT int_value); 
  char Scan_Character(); 
  void Pushback_Character(); 
  void Scan_Integer(INT* int_value); 
  void Scan_Unsigned(UINT32* int_value); 
  void Scan_HexInteger(INT* int_value); 
  void Scan_Alphanumeric(char s[]); 
  BOOL Is(char ch) {return _buffer[_buffer_start] == ch;};
  BOOL Is_Integer();  
  char Get_Command(); 
}; 

#endif /* wb_buffer_INCLUDED */ 
