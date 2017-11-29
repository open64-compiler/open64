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



#ifdef USE_PCH
#include "be_com_pch.h"
#endif /* USE_PCH */
#pragma hdrstop
#include <sys/types.h>
#if defined(BUILD_OS_DARWIN)
#include "darwin_elf.h"
#else /* defined(BUILD_OS_DARWIN) */
#include <elf.h>
#endif /* defined(BUILD_OS_DARWIN) */
#include <ctype.h>
#include "wn.h"
#include <stdio.h> 
#include "wb_buffer.h" 

WB_BUFFER WB_buffer; 

//-----------------------------------------------------------------------
// NAME: WB_BUFFER::Load_Buffer
// FUNCTION: Load the '_buffer' up to the next newline character with
//   input from 'stdin'.
//-----------------------------------------------------------------------

void WB_BUFFER::Load_Buffer()
{
  INT i;
  for (i = 0; ; i++) {
    _buffer[i] = fgetc(stdin);
    if (_buffer[i] == '\n')
      break;
  }
  _buffer_start = 0;
}

//-----------------------------------------------------------------------
// NAME: WB_BUFFER::Load_Buffer
// FUNCTION: Load the '_buffer' up to the next newline character with
//   input from 'stdin'.
//-----------------------------------------------------------------------

void WB_BUFFER::Load_Buffer(const char s[])
{
  INT i;
  for (i = 0; s[i] != '\0'; i++) {
    _buffer[i] = s[i];
  }
  _buffer[i] = '\n';   
  _buffer_start = 0; 
}

//-----------------------------------------------------------------------
// NAME: WB_BUFFER::Scan_Blanks_And_Tabs
// FUNCTION: In the '_buffer' advance '_buffer_start' past all blanks and
//   tabs.
//-----------------------------------------------------------------------

void WB_BUFFER::Scan_Blanks_And_Tabs()
{
  char ch;
  do {
    ch = _buffer[_buffer_start++];
  } while (ch == ' ' || ch == '\t');
  _buffer_start--;
}

//-----------------------------------------------------------------------
// NAME: WB_BUFFER::Skip_To_Separator
// FUNCTION: In the '_buffer' advance '_buffer_start' past all blanks and
//   tabs.
//-----------------------------------------------------------------------

void WB_BUFFER::Skip_To_Separator(WB_SKIP_CLASS skip_type)
{
  switch (skip_type) {
  case WB_SKIP_ALPHANUMERIC:
    while (_buffer[_buffer_start] != ' ' &&  _buffer[_buffer_start] != '\t'
        && _buffer[_buffer_start] != ';' && _buffer[_buffer_start] != '\n')
      _buffer_start++;
    break;
  case WB_SKIP_NUMERIC:
    while (isdigit(_buffer[_buffer_start]))
      _buffer_start++;
    break;
  case WB_SKIP_HEX:
    while (isxdigit(_buffer[_buffer_start]))
      _buffer_start++;
    break;
  default:
    while (_buffer[_buffer_start] != ' ' &&  _buffer[_buffer_start] != '\t'
        && _buffer[_buffer_start] != ';' && _buffer[_buffer_start] != '\n')
      _buffer_start++;
  }
  if (_buffer[_buffer_start] == '\n')
    return;
  while (_buffer[_buffer_start] == ' ' || _buffer[_buffer_start] == '\t'
      || _buffer[_buffer_start] == ';')
    _buffer_start++;
}

//-----------------------------------------------------------------------
// NAME: WB_BUFFER::Load_Loop
// FUNCTION: Query the user for a loop and place the result at '*wn_loop'.
//-----------------------------------------------------------------------

void WB_BUFFER::Load_Loop(WN** wn_loop)
{
  Load_Buffer();
  Scan_Blanks_And_Tabs();
  sscanf(_buffer + _buffer_start, "0x%p", wn_loop);
  _buffer_start += 2;
  Skip_To_Separator(WB_SKIP_HEX);
}

//-----------------------------------------------------------------------
// NAME: WB_BUFFER::Load_UINT32
// FUNCTION: Query the user for an UINT32 and place the result at '*int_value'.
//-----------------------------------------------------------------------

void WB_BUFFER::Load_UINT32(UINT32* int_value)
{
  Load_Buffer();
  Scan_Blanks_And_Tabs();
  sscanf(_buffer + _buffer_start, "%d", int_value);
  Skip_To_Separator(WB_SKIP_NUMERIC);
}

//-----------------------------------------------------------------------
// NAME: WB_BUFFER::Load_mINT32
// FUNCTION: Query the user for a mINT32 and place the result at '*value'.
//-----------------------------------------------------------------------

void WB_BUFFER::Load_mINT32(mINT32* value)
{
  Load_Buffer();
  Scan_Blanks_And_Tabs();
  sscanf(_buffer + _buffer_start, "%d", value);
  Skip_To_Separator(WB_SKIP_NUMERIC);
}

//-----------------------------------------------------------------------
// NAME: WB_BUFFER::Load_mINT64
// FUNCTION: Query the user for a mINT64 and place the result at '*value'.
//-----------------------------------------------------------------------

void WB_BUFFER::Load_mINT64(mINT64* value)
{
  Load_Buffer();
  Scan_Blanks_And_Tabs();
  sscanf(_buffer + _buffer_start, "%lld", value);
  Skip_To_Separator(WB_SKIP_NUMERIC);
}

//-----------------------------------------------------------------------
// NAME: WB_BUFFER::Skip_Chars
// FUNCTION: Skip adhead in the '_buffer' 'count' characters. 
//-----------------------------------------------------------------------

void WB_BUFFER::Skip_Chars(INT count)
{ 
  _buffer_start += count; 
} 

//-----------------------------------------------------------------------
// NAME: WB_BUFFER::Scan_Integer
// FUNCTION: Scan for an Integer and place the result at '*int_value'.
//-----------------------------------------------------------------------

void WB_BUFFER::Scan_Integer(INT* int_value)
{
  Scan_Blanks_And_Tabs();
  sscanf(_buffer + _buffer_start, "%d", int_value);
  Skip_To_Separator(WB_SKIP_NUMERIC);
}

//-----------------------------------------------------------------------
// NAME: WB_BUFFER::Scan_Unsigned
// FUNCTION: Scan for an unsigned and place the result at '*int_value'.
//-----------------------------------------------------------------------

void WB_BUFFER::Scan_Unsigned(UINT32* int_value)
{
  Scan_Blanks_And_Tabs();
  sscanf(_buffer + _buffer_start, "%d", int_value);
  Skip_To_Separator(WB_SKIP_NUMERIC);
}

//-----------------------------------------------------------------------
// NAME: WB_BUFFER::Scan_HexInteger
// FUNCTION: Scan for an Integer and place the result at '*int_value'.
//-----------------------------------------------------------------------

void WB_BUFFER::Scan_HexInteger(INT* int_value)
{
  Scan_Blanks_And_Tabs();
  sscanf(_buffer + _buffer_start, "0x%x", int_value);
  _buffer_start += 2;
  Skip_To_Separator(WB_SKIP_HEX);
}

//-----------------------------------------------------------------------
// NAME: WB_BUFFER::Scan_Alphanumeric
// FUNCTION: Scan for an alphanumeric string and place the result in 's'.
//-----------------------------------------------------------------------

void WB_BUFFER::Scan_Alphanumeric(char s[])
{
  Scan_Blanks_And_Tabs();
  sscanf(_buffer + _buffer_start, "%s", s);
  INT i;
  for (i = 0; s[i] != '\0'; i++)
    if (s[i] == '\n' || s[i] == ';')
      break;
  s[i] = '\0';
  Skip_To_Separator(WB_SKIP_ALPHANUMERIC);
}

//-----------------------------------------------------------------------
// NAME: WB_BUFFER::Scan_Character
// FUNCTION: Scan and return the next character.  
//-----------------------------------------------------------------------

char WB_BUFFER::Scan_Character()
{
  char ch = _buffer[_buffer_start++];
  return ch; 
}

//-----------------------------------------------------------------------
// NAME: WB_BUFFER::Pushback_Character
// FUNCTION: Retract the start pointer, effectively pushing back the last 
//   last character read out of the buffer.  
//-----------------------------------------------------------------------

void WB_BUFFER::Pushback_Character() 
{ 
  _buffer_start--;
} 

//-----------------------------------------------------------------------
// NAME: WB_BUFFER::Load_Integer
// FUNCTION: Query the user for an Integer and place the result at '*int_value'.//-----------------------------------------------------------------------

void WB_BUFFER::Load_Integer(INT* int_value)
{
  Load_Buffer();
  Scan_Blanks_And_Tabs();
  sscanf(_buffer + _buffer_start, "%d", int_value);
  Skip_To_Separator(WB_SKIP_NUMERIC);
}

//-----------------------------------------------------------------------
// NAME: WB_BUFFER::Load_Double
// FUNCTION: Query the user for a Double and place the result at '*value'.
//-----------------------------------------------------------------------

void WB_BUFFER::Load_Double(double* value)
{
  Load_Buffer();
  Scan_Blanks_And_Tabs();
  sscanf(_buffer + _buffer_start, "%lg", value);
  Skip_To_Separator(WB_SKIP_NUMERIC);
}

//-----------------------------------------------------------------------
// NAME: WB_BUFFER::Load_Boolean
// FUNCTION: Query the user for a boolean value, and place the result at
//   'bool_value'. If 'default_present' is TRUE, a default value (either
//   'Y' or 'N' should be assumed, even when the user gives no response.
//   When 'default_present' is TRUE, 'default_value' should be set to TRUE
//   if the assumed default value is 'TRUE', and to 'FALSE' if the assumed
//   value is FALSE.
//-----------------------------------------------------------------------

void WB_BUFFER::Load_Boolean(BOOL* bool_value, 
			     BOOL default_present,
                             BOOL default_value)
{
  Load_Buffer();
  Scan_Blanks_And_Tabs();
  if (!default_present) {
    if (_buffer[_buffer_start] == 'Y' || _buffer[_buffer_start] == 'y')
      *bool_value = TRUE;
    if (_buffer[_buffer_start] == 'N' || _buffer[_buffer_start] == 'n')
      *bool_value = FALSE;
    return;
  }
  if (default_value == TRUE) {
    *bool_value = TRUE;
    if (_buffer[_buffer_start] == 'N' || _buffer[_buffer_start] == 'n')
      *bool_value = FALSE;
  } else {
    *bool_value = FALSE;
    if (_buffer[_buffer_start] == 'Y' || _buffer[_buffer_start] == 'y')
      *bool_value = TRUE;
  }
}

//-----------------------------------------------------------------------
// NAME: WB_BUFFER::Get_Command
// FUNCTION: Using the 'buffer' scan past spaces, tabs, and semicolons to
//   get to the next character, and return it as the character represen-
//   tation of the next command.
//-----------------------------------------------------------------------

char WB_BUFFER::Get_Command()
{
  INT i = _buffer_start;
  while (_buffer[i] == ' ' || _buffer[i] == '\t' || _buffer[i] == ';') i++;
  _buffer_start = i + 1;
  return _buffer[i];
}


//-----------------------------------------------------------------------
// NAME: WB_BUFFER::Is_Integer 
// FUNCTION: Returns TRUE if the next item in the buffer is an integer, 
//   otherwise returns FALSE. 
//-----------------------------------------------------------------------

BOOL WB_BUFFER::Is_Integer()
{ 
  INT start_index 
    = (_buffer[_buffer_start] == '+' || _buffer[_buffer_start] == '-') 
    ? _buffer_start + 1 : _buffer_start;
  return isdigit(_buffer[start_index]); 
} 
