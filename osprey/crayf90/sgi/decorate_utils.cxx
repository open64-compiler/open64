/*
 * Copyright (C) 2008. PathScale, LLC. All Rights Reserved.
 */
/*
 * Copyright 2005, 2006 PathScale, Inc.  All Rights Reserved.
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

typedef __gnu_cxx::hash_map<const char*, const char *, __gnu_cxx::hash<const char*>,  eqstr> KEY_TARGET_HTABLE;
static KEY_TARGET_HTABLE spec_table;
typedef __gnu_cxx::hash_map<const char*, void*, __gnu_cxx::hash<const char*>,  eqstr> char_to_void_htable;
static char_to_void_htable binding_labels;
extern MEM_POOL *FE_Mempool;
using namespace std;

//--------------------------------------------------------
// Parse "-fdecorate path" file. Each line contains two blank- or tab-delimited
// tokens. The first is taken as a Fortran external identifier. The second
// is the linker symbol to use in place of that identifier. Abbreviations
// are accepted in place of the second token: "0" acts like "-fnounderscoring",
// "1" acts like "-funderscoring", and "2" acts like "-underscoring
// -fsecond-underscore".
//--------------------------------------------------------
void parse_decorate_script(const char *script_name)
{
  // Assumption: the maximum line length should not exceed 1024 bytes
  char line_buffer[1024], *line_iterator;
  ifstream infile; 
  char *key, *key_temp, *target, *target_temp;

  infile.open(script_name, ifstream::in);

  if (!infile.good()) {
    PRINTMSG(0, 1676, Log_Error, 0, script_name, strerror(errno));
    return;
  }

  // Read in the file line by line. First blank-delimited token is
  // Fortran symbol, second is desired linker symbol.
  while (infile.good()) {
    infile.getline(line_buffer, 1024);
    line_iterator = line_buffer;

    // Filter out the blank-space in line header
    while (*line_iterator == ' ')
      line_iterator++;
    key = strtok(line_iterator, " \t");
    if (!key)
      continue;		// Ignore blank line
    key_temp = new char[sizeof(char)*(strlen(key)+1)];
    strcpy(key_temp, key);
    target = strtok(NULL, " \t");

    int abbrev = 0;	// Append no underscores to target
    // Treat missing 2nd token like "0" abbreviation
    if (0 == target || '0' == *target && 0 == target[1]) {
      target = key;	// Target is same as key
      }
    else if ('1' == *target && 0 == target[1]) {
      target = key;	// Target is same as key, but
      abbrev = 1;	// append one underscore
      }
    else if ('2' == *target && 0 == target[1]) {
      target = key;	// Target is same as key, but
      abbrev = 2;	// append one or two underscores
      }

    // Allocate space for copy of target plus underscores, if any
    target_temp = new char[sizeof(char)*(strlen(target)+abbrev+1)];

    switch (abbrev) {
      case 0:
	strcpy(target_temp, target);
        break;
      case 1:
        strcat(strcpy(target_temp, target), "_");
        break;
      case 2:
        strcat(strcpy(target_temp, target),
	  (strchr(target, '_') ?  "__" : "_"));
	break;
      }

    spec_table[key_temp] = target_temp;
  } 
  infile.close();
}

//--------------------------------------------------------
// If the "-fdecorate path" option has told us to map Fortran symbol "key"
// onto a particular string, return that string; otherwise, return null.
//--------------------------------------------------------
extern "C" const char *get_symbol_decoration(const char *key)
{
  return (spec_table.find(key) == spec_table.end()) ?
    0 :
    spec_table[key];
}

//--------------------------------------------------------
// Map language binding label "key" onto global_attr_idx
//--------------------------------------------------------
extern "C" void put_external_label(const char *key, void *info)
{
  char *key_temp = new char[::strlen(key) + 1];
  ::strcpy(key_temp, key);
  binding_labels[key_temp] = info;
}

//--------------------------------------------------------
// Return global_attr_idx associated with "key", or 0 if there is no mapping
//--------------------------------------------------------
extern "C" void *get_external_label(const char *key)
{
  return (binding_labels.find(key) == binding_labels.end()) ?
    0 :
    binding_labels[key];
}
