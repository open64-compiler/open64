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



#include <stdio.h>
#include <sys/types.h>
#include "defs.h"
#include "srcpos.h"
#include "glob.h"	/* for Tlog_File */
#include "errors.h"	/* for FmtAssert */

static char* dummy_word="(null)";

/* see interface in tlog.h */

extern void Generate_Tlog(
  char*		phase_name,
  char* 	trans_name,
  SRCPOS	srcpos,
  char*		keyword,
  char*		input_string,
  char*		output_string,
  char*		aux_info_string
)
{
  if (Tlog_File==NULL)
    return;

  FmtAssert(phase_name!=NULL,("Null phase name !!"));
  FmtAssert(trans_name!=NULL,("Null transformation name !!"));

  if (keyword[0]=='\0')
    keyword=dummy_word;

  fprintf(Tlog_File, "\n%s %s %llu %s\n",
    phase_name, trans_name, srcpos, keyword);
  fprintf(Tlog_File, "{ %s }\n", input_string);
  fprintf(Tlog_File, "{ %s }\n", output_string);
  fprintf(Tlog_File, "{ %s }\n", aux_info_string);
}


