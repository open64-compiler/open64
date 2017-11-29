/*
 * Copyright 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of version 2 of the GNU General Public License as
 * published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it would be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  
 *
 * Further, this software is distributed without any warranty that it is
 * free of the rightful claim of any third person regarding infringement 
 * or the like.  Any license provided herein, whether implied or 
 * otherwise, applies only to this software file.  Patent licenses, if 
 * any, provided herein do not apply to combinations of this program with 
 * other software, or any other product whatsoever.  
 *
 * You should have received a copy of the GNU General Public License along
 * with this program; if not, write the Free Software Foundation, Inc., 59
 * Temple Place - Suite 330, Boston MA 02111-1307, USA.
 */

#include <defs.h>
#include <iostream>
#include <fstream>
#include <string.h>
#include <ext/hash_map>
#include <stdlib.h>

#include "mempool.h"
#include "cxx_memory.h"
#include "errors.h"
#include "f2c_abi_utils.h"

extern "C" {
#include "../fe90/printmsg.h"
}

static CALLEE_HTABLE spec_table;
static BOOL script_init = FALSE;
extern MEM_POOL *FE_Mempool;
using namespace std;

static void Make_Mangled_Name(char *dst, const char *src)
{
  const char *ptr;
  int underscores;

  bzero (dst, sizeof(char)*strlen(dst));
  underscores = 1;
  for (ptr = src; ptr && *ptr && *ptr != ' ' ; ptr++){
    *dst++ = towlower(*ptr) ;
    if (*ptr == '_')
      underscores++;
  }
  while (underscores--)
    *dst ++ = '_';
  *dst = '\0';
}

//--------------------------------------------------------
// NAME: Parse_inline_script
// FUNCTION: Parse the context sensitive inlining specification file.
//           The inlining records are stored in the two-level hash-map
//           defined in "inline_script_parser.h"
//--------------------------------------------------------
static void Parse_script(const char *script_name)
{
  // Assumption: the maximum line length should not exceed 1024 bytes
  char line_buffer[1024], *line_iterator;
  ifstream infile; 
  char *callee_func;
  char callee_key[1024], *key_temp;
  static int count = 1;

  infile.open(script_name, ifstream::in);

  strcpy(callee_key, "");
#ifdef KEY /* Bug 6121 */
  if (!infile.good()) {
    PRINTMSG(0, 1676, Log_Error, 0, script_name, strerror(errno));
    return;
  }
#else
  FmtAssert((infile.good()), ("Ff2c abi script parsing error: can't open the ff2c abi description file"));
#endif /* KEY Bug 6121 */

  // Read in the file line by line,
  // Perform syntax analysis based on the keywords of call-site type specification
  while (infile.good()) {
    infile.getline(line_buffer, 1024);
    line_iterator = line_buffer;

    // Filter out the blank-space in line header
    while (*line_iterator == ' ')
      line_iterator++;
    callee_func = strtok(line_iterator, " ");
    if (!callee_func)
      continue;
    Make_Mangled_Name(callee_key, callee_func);
//    strcpy(callee_key, callee_func);
    key_temp = TYPE_MEM_POOL_ALLOC_N(char, FE_Mempool, sizeof(char)*(strlen(callee_key)+1));
    strcpy(key_temp, callee_key);
    spec_table[key_temp] = count++;
  } 
  infile.close();
}

//--------------------------------------------------------
// NAME: Check_Inline_Script
// FUNCTION: The interface function for inquiry into the inlining record
//--------------------------------------------------------
extern char *F2C_ABI_filename;
extern "C" int Check_FF2C_Script(const char *callee_key, int mangled)
{
  char name_string[1024];
  const char *name_ptr;

  if (!F2C_ABI_filename) 
    return FALSE;
  // If being queired for the first time, parse the inline script file, and set up inlining record
  if (script_init == FALSE) {
    Parse_script (F2C_ABI_filename);
    script_init = TRUE;
  }

  if (!mangled){
    Make_Mangled_Name(name_string, callee_key);
    name_ptr = name_string;
  }
  else
    name_ptr = callee_key;

  // Check if the hash map contains the element being inquired
  if (spec_table.find(name_ptr) == spec_table.end()) 
    return FALSE;
  return TRUE;
}
