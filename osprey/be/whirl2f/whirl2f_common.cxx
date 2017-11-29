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


/* ====================================================================
 * ====================================================================
 *
 * Module: whirl2f_common.c
 * $Revision: 1.1 $
 * $Date: 2005/07/27 02:13:43 $
 * $Author: kevinlo $
 * $Source: /depot/CVSROOT/javi/src/sw/cmplr/be/whirl2f/whirl2f_common.cxx,v $
 *
 * Revision history:
 *  17-Apr-95 - Original Version
 *
 * Description:
 *
 *   See whirl2f_common.h for comments on this file.
 *
 * ====================================================================
 * ====================================================================
 */

#include <ctype.h>
#include "whirl2f_common.h"
#include "PUinfo.h"

/*----------- Functions and state for identifier naming -----------*/

const char * 
WHIRL2F_make_valid_name(const char *name, BOOL allow_dot )
{
   /* If name==NULL, then return NULL;  otherwise, if a valid name,
    * then keep it unaltered; otherwise, construct a valid name
    * in a new Name_Buf by removing invalid characters (never return
    * NULL for this case)
    */
   const char *return_name = name;
   char       *new_name;
   INT         name_idx;
   
   if (name != NULL)
   {
      /* See if we need to construct a new name. First skip valid
       * characters (alphanumeric, '_', or '$', beginning with
       * an alphabetic or '_' character).
       */
      if (isalpha(name[0]) || name[0] == '_')
      {
	 /* See if there are any invalid characters */
	 for (name_idx = 1;
	      (isalnum(name[name_idx]) || 
	       name[name_idx] == '_'   || 
	       name[name_idx] == '$');
	      name_idx++);
      }
      else /* Incorrect first character */
      {
	 /* Skip to first alphabetic or '_' character */
	 name_idx = 0;
	 while (name[name_idx] != '\0' && 
		!isalpha(name[name_idx]) && name[name_idx] != '_')
	    name += 1;
	 return_name = name;
      }
      
      
      /* Did we find an invalid character? */
      if (name[name_idx] != '\0')
      {
	 /* Need to construct a new name.  This should be relatively rare */
	 new_name = strcpy(Get_Name_Buf_Slot(strlen(name) + 1), name);
	 return_name = new_name;
	 while (*name != '\0')
	 {
	   if ((isalnum(*name)) || 
	       (*name == '_' )  || 
	       (*name == '$' )  || 
	       (*name == '.' && allow_dot))
	   {
	       *new_name++ = *name;
	   }
	   name++;     /* Skip character (may be invalid) */
	 }
	 *new_name = '\0';
      }
   }
   return return_name;
} /* WHIRL2F_make_valid_name */


void 
WHIRL2F_Append_Comment(TOKEN_BUFFER  tokens,
		       const char   *cmt,
		       UINT          prefix_lines,
		       UINT          suffix_lines)
{
   /* Purpose: Adds prefix_lines+1+postfix_lines number of comment-lines
    * to the given token buffer, where the given comment-string (if non-
    * NULL) will be insterted on a separate line between the prefix and 
    * postifix at the current identation.
    */   
   if (cmt == NULL || cmt[0] == '\0')
   {
      Append_F77_Comment_Newline(tokens,
				 prefix_lines + suffix_lines + 1, 
				 FALSE/*ident_last_line*/);
   }
   else
   {
      Append_F77_Comment_Newline(tokens, 
				 prefix_lines+1, 
				 TRUE/*ident_last_line*/);
      Append_Token_String(tokens, cmt);
      Append_F77_Comment_Newline(tokens, 
				 suffix_lines, 
				 FALSE/*ident_last_line*/);
   }
} /* WHIRL2F_Append_Comment */


void 
WHIRL2F_Parenthesize(TOKEN_BUFFER tokens)
{
   Prepend_Token_Special(tokens, '(');
   Append_Token_Special(tokens, ')');
} /* WHIRL2F_Parenthesize */







