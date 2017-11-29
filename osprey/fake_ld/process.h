/*
 * Copyright 2003, 2004 PathScale, Inc.  All Rights Reserved.
 */

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

#ifndef __PROCESS_H__
#define __PROCESS_H__

#ifdef __cplusplus
extern "C" {
#endif

extern string tmpdir;

extern void dump_argv (string *);

extern int do_compile (string *);

extern void ld_kill_compilation (int);

extern void add_to_tmp_file_list (string);

extern void cleanup_all_files (void);

extern string make_temp_file (string, char);

extern int create_tmpdir ( int tracing );

extern string create_unique_file (string, char);

extern string create_tmp_file (string);

extern string *get_command_line (an_object_file_ptr, string, string, int *);

extern int make_link (const string, const string);

extern string ld_compile (an_object_file_ptr);

extern char * always_demangle(char *, char );

extern char *__Release_ID;

#ifdef __cplusplus
} /* Close extern "C" */
#endif

#endif /* __PROCESS_H__ */
