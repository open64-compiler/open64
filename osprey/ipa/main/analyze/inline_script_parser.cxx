/*
 * Copyright 2003, 2004 PathScale, Inc.  All Rights Reserved.
 */

/*
  Copyright (C) 2000-2003, Intel Corporation
  All rights reserved.

  Redistribution and use in source and binary forms, with or without modification,
  are permitted provided that the following conditions are met:

  Redistributions of source code must retain the above copyright notice, this list
  of conditions and the following disclaimer.

  Redistributions in binary form must reproduce the above copyright notice, this list
  of conditions and the following disclaimer in the documentation and/or other materials
  provided with the distribution.

  Neither the name of the owner nor the names of its contributors may be used to endorse or
  promote products derived from this software without specific prior written permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR
  IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
  FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE CONTRIBUTORS BE LIABLE FOR
  ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
  NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
  BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/          

#include <defs.h>
#include <iostream>
#include <fstream>
#include <string.h>
#include <hash_map>
#include <stdlib.h>

#include "inline_script_parser.h"

const char *inline_action_log = "Inline_Action.verify";
const char *inline_script_log = "Inline_Script.verify";

static CALLER_HTABLE inline_spec_table;

static BOOL inline_script_init = FALSE;

//--------------------------------------------------------
// NAME: ISP_Fix_Filename
// FUNCTION: Filter out the suffix in file names,
//           or names of functions declared with "local"
//--------------------------------------------------------
void ISP_Fix_Filename(char *name)
{
  char *p = strchr(name, '.');
  if (p != NULL) *p='\0';	
}

//--------------------------------------------------------
// NAME: Parse_inline_script
// FUNCTION: Parse the context sensitive inlining specification file.
//           The inlining records are stored in the two-level hash-map
//           defined in "inline_script_parser.h"
//--------------------------------------------------------
static void Parse_inline_script(const char *script_name, MEM_POOL *parser_pool)
{
  // Assumption: the maximum line length should not exceed 1024 bytes
  char line_buffer[1024], *line_iterator;
  ifstream infile; 
  char *callsite_ln, *callee_file, *callee_func;
  char cur_caller_key[1024], callee_key[1024], *key_temp;
  char *cur_caller_file, *cur_caller_func; 
  CALLEE_HTABLE *cur_callee_htable;
  CALLEE_INFO callee_info;
  int outer_nest = 0, inner_nest = 0;

  strcpy(cur_caller_key, "");
  strcpy(callee_key, "");

  infile.open(script_name, ifstream::in);
  FmtAssert((infile.good()), ("Inline script parsing error: can't open the inlining description file"));

#ifdef Enable_ISP_Verify
  FILE *inline_script = fopen(inline_script_log, "a+");
  FmtAssert((inline_script != NULL), ("Inline script parsing error: can't open file"));
#endif

  // Read in the file line by line,
  // Perform syntax analysis based on the keywords of call-site type specification
  while (infile.good()) {
    infile.getline(line_buffer, 1024);
    line_iterator = line_buffer;

    // Filter out the blank-space in line header
    while (*line_iterator == ' ')
    	line_iterator++;

    // The line starting with "COMPILE" represents a caller function
    if (strncmp(line_iterator, "COMPILE", 7) == 0) {
      FmtAssert((outer_nest == 0),
                ("Inline script parsing error: nested parentheses don't match"));
      outer_nest++;	

      strtok(line_iterator, "\"");
      cur_caller_file = strtok(NULL, "\"");
      cur_caller_func = strtok(NULL, ",");

      FmtAssert((cur_caller_file != NULL && cur_caller_func != NULL),
                ("Inline script parsing error: incorrect input format for caller specification"));
      // Assemble the caller key
      ISP_Fix_Filename(cur_caller_file);
      ISP_Fix_Filename(cur_caller_func);
      strcat(cur_caller_key, cur_caller_file); 
      strcat(cur_caller_key, cur_caller_func);

      // Store the caller record in hash map
      key_temp = TYPE_MEM_POOL_ALLOC_N(char, parser_pool, sizeof(char)*(strlen(cur_caller_key)+1));
      strcpy(key_temp, cur_caller_key);   	     	
      cur_callee_htable = &inline_spec_table[key_temp];

#ifdef ISP_DEBUG
cout << "Inline script parsing info : added caller_key " << cur_caller_key << endl;
#endif     	      	
    }
    // The line starting with "INLINE" represents a callee function which should be inlined
    else if (strncmp(line_iterator, "INLINE", 6) == 0) {
      inner_nest++;

      strtok(line_iterator, "(");
      callsite_ln = strtok(NULL, ",");
      strtok(NULL, "\"");
      callee_file = strtok(NULL, "\"");
      char *patch_strtok = strtok(NULL, ",");
      callee_func = strtok(patch_strtok, ")");
      
      FmtAssert((callsite_ln != NULL && callee_file != NULL && callee_func != NULL),
      	        ("Inline script parsing error: incorrect input format for callee specification"));
      // Assemble the callee key
      ISP_Fix_Filename(callee_file);
      ISP_Fix_Filename(callee_func);
      strcat(callee_key, callsite_ln);
      strcat(callee_key, callee_file);
      strcat(callee_key, callee_func);
#ifdef Enable_ISP_Verify
      fprintf(inline_script, "[%s] inlined into [%s]\n",
              callee_key, cur_caller_key);
#endif
      callee_info.attrib = INLINE;
      callee_info.nested_inline = NULL;
      // Store the callee record in hash map		
      key_temp = TYPE_MEM_POOL_ALLOC_N(char, parser_pool, sizeof(char)*(strlen(callee_key)+1));
      strcpy(key_temp, callee_key);
      (*cur_callee_htable)[key_temp] = callee_info;

#ifdef ISP_DEBUG
cout << "Inline script parsing info : added INLINE type callee_key " << callee_key << endl;
#endif
      callsite_ln = NULL; 
      callee_file = NULL;
      callee_func = NULL;
      strcpy(callee_key, "");
    }
    // clean up for caller record when matching the right parenthesis
    else if (*line_iterator == '}') {
      if(outer_nest == 1 && inner_nest > 0) {
        inner_nest--;
      }
      else if(outer_nest == 1 && inner_nest == 0) {
      	outer_nest--;
      	strcpy(cur_caller_key, "");
      }
      else {
        FmtAssert(0,("Inline script parsing error: nested parentheses don't match"));
      } 	
    }
    // The line starting with "CALL" represents a callee function which should not be inlined
    else if (strncmp(line_iterator, "CALL", 4) == 0) {
      strtok(line_iterator, "(");
      callsite_ln = strtok(NULL, ",");
      strtok(NULL, "\"");
      callee_file = strtok(NULL, "\"");
      char *patch_strtok = strtok(NULL, ",");
      callee_func = strtok(patch_strtok, ")");
      
      FmtAssert((callsite_ln != NULL && callee_file != NULL && callee_func != NULL),
      	        ("Inline script parsing error : invalid input format for callee specification"));
      // Assemble the callee key
      ISP_Fix_Filename(callee_file);
      ISP_Fix_Filename(callee_func);
      strcat(callee_key, callsite_ln);
      strcat(callee_key, callee_file);
      strcat(callee_key, callee_func);

      callee_info.attrib = CALL;
      callee_info.nested_inline = NULL;
      // Store the callee record in hash map
      key_temp = TYPE_MEM_POOL_ALLOC_N(char, parser_pool, sizeof(char)*(strlen(callee_key)+1));
      strcpy(key_temp, callee_key);
      (*cur_callee_htable)[key_temp] = callee_info;

#ifdef ISP_DEBUG
cout << "Inline script parsing info : added CALL type callee_key " << callee_key << endl;
#endif
      callsite_ln = NULL;
      callee_file = NULL;
      callee_func = NULL;
      strcpy(callee_key, "");
   	}
    else {
      // Default action
    }    	    	
  } 
#ifdef Enable_ISP_Verify
  fclose(inline_script);
#endif
  infile.close();
}

//--------------------------------------------------------
// NAME: Check_Inline_Script
// FUNCTION: The interface function for inquiry into the inlining record
//--------------------------------------------------------
BOOL Check_Inline_Script(const char *script_name, const char *caller_key, const char *callee_key, MEM_POOL* parser_pool)
{
#ifdef ISP_DEBUG
cout << "Inline script check info: analyze call-pair (" << caller_key << " --> " << callee_key << ")" << endl;
#endif

  // If being queired for the first time, parse the inline script file, and set up inlining record
  if (inline_script_init == FALSE) {
  	Parse_inline_script (script_name, parser_pool);
  	inline_script_init = TRUE;
  }

  // Check if the hash map contains the element being inquired
  if (inline_spec_table.find(caller_key) == inline_spec_table.end()) {
#ifdef ISP_DEBUG
    fprintf(stderr, "  ...... check failed: caller \"%s\" not found in inline description file %s\n",
    	   caller_key, script_name);
#endif
    return FALSE;
  } else if ((inline_spec_table[caller_key]).find(callee_key) == (inline_spec_table[caller_key]).end()) {
#ifdef ISP_DEBUG
    fprintf(stderr, "  ...... check failed: callee \"%s\" not found in inline description file %s\n",
    	   callee_key, script_name);
#endif
    return FALSE;
  }
  else {
    if ((inline_spec_table[caller_key])[callee_key].attrib == INLINE) {
      (inline_spec_table[caller_key])[callee_key].process_flag += 1;
#ifdef ISP_DEBUG
      fprintf(stderr, "  ...... check successful: the call-site is inlined\n");
#endif
      return TRUE;
    }
    else {
#ifdef ISP_DEBUG
      fprintf(stderr, "  ...... check failed: the call-site is not inlined (type = %d)\n",
    	      (inline_spec_table[caller_key])[callee_key].attrib);
#endif
      return FALSE;
    }
  } 
}

//--------------------------------------------------------
// NAME: Verify_Inline_Script
// FUNCTION: Provide detailed debug information to help
//           verify if the inlining results are valid
//--------------------------------------------------------
void Verify_Inline_Script(void)
{
  CALLER_HTABLE::iterator caller_it;
  CALLEE_HTABLE::iterator callee_it;
  int total_inline_count = 0;

#ifdef ISP_DEBUG
  fprintf(stderr, "### Inline script verify info ### \n");
#endif
#ifdef Enable_ISP_Verify
  FILE *inline_action = fopen(inline_action_log, "a+");
  FmtAssert((inline_action != NULL), ("Inline script parsing error: can't open file"));
#endif
  for(caller_it = inline_spec_table.begin(); caller_it != inline_spec_table.end(); caller_it++) {
    for(callee_it = (*caller_it).second.begin(); callee_it != (*caller_it).second.end(); callee_it++) {
      if((*callee_it).second.attrib == INLINE)
        total_inline_count++;

      if( ((*callee_it).second.attrib == INLINE) && ((*callee_it).second.process_flag == 0)) {
#ifdef ISP_DEBUG
        fprintf(stderr, "  ...... callsite is specified but not processed [%s] --> [%s] \n",
                (*caller_it).first, (*callee_it).first);
#endif
#ifdef Enable_ISP_Verify
        fprintf(inline_action, "[%s] inlined into [%s]\n",
                (*callee_it).first, (*caller_it).first);
#endif
      }
#ifdef ISP_DEBUG
      else if(((*callee_it).second.attrib == INLINE) && ((*callee_it).second.process_flag > 1))
        fprintf(stderr, "  ...... callsite was queried %d times [%s] --> [%s] \n",
                (*callee_it).second.process_flag, (*caller_it).first, (*callee_it).first);
#endif
    }
  }
#ifdef Enable_ISP_Verify
  fprintf(inline_action, "(The above call-sites are actually not processed!)\n");
  fclose(inline_action);
#endif
#ifdef ISP_DEBUG
  fprintf(stderr, "  ...... The total number of inlined call-sites in the description file is %d \n",
          total_inline_count);
#endif
}
