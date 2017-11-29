/*
 * Copyright 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
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


#ifndef __IPC_LINK_H__
#define __IPC_LINK_H__

#ifdef __cplusplus
extern "C" {
#endif 

extern void
ipa_init_link_line (int argc, char **argv);

extern void
ipa_add_link_flag (const char *str);

// modify link flag for mixed archive: replace -labc with /path/libabc.a
extern void
ipa_modify_link_flag (char *lname, char *fname);


extern void
ipa_add_comma_list (const char* name);

extern void
ipa_compose_comma_list (const char* name);

#ifdef _LD_IPA_INTERFACE

#pragma weak ipa_init_link_line
#pragma weak ipa_add_link_flag
#pragma weak ipa_modify_link_flag
#pragma weak ipa_add_comma_list
#pragma weak ipa_compose_comma_list

#endif /* _LD_IPA_INTERFACE */

#ifdef __cplusplus
}
#endif


#ifndef _LD_IPA_INTERFACE

/* These are NOT exported to ld */

#include <vector>

typedef std::vector<const char*> ARGV;

extern void
ipa_insert_whirl_obj_marker ();

extern ARGV *
ipa_link_line_argv (const ARGV* output_files,
                    const char* dir, const char* symtab_file);

// extern void process_cord_obj_list (FILE *);
#endif /* _LD_IPA_INTERFACE */


#endif /* __IPC_LINK_H__ */
