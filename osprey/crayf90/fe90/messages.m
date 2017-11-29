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


/* USMID:  "\n@(#)5.0_pl/macros/messages.m	5.1	04/29/99 21:22:31\n" */
 

/*****************\
|* MISCELLANEOUS *|
\*****************/


/********************\
|* SIZES AND LIMITS *|
\********************/

/* EXPANDED_MSG_SIZE   This is the maximum length a message can be once all   */
/*                     arguments have been expanded.  The value was chosen    */
/*                     strictly on the basis of "it was large enough".        */
/* FINAL_MSG_SIZE      The maximum total length of a message when all the     */
/*                     parts are added together.			      */
/* MAX_ERR_LIMIT       The default maximum number of messages that will be    */
/*                     issued.  The compiler keeps going after the limit is   */
/*                     exceeded but no more messages appear.                  */
/* MAX_HDR_SIZE        The maximum length of the program/file/line/column     */
/*                     info output with a message.  Again, the value was      */
/*                     chosen because it was "big enough".                    */
/* MAX_MSG             The largest message number allowed.		      */
/* MAX_FE_MSG          The largest message number allocated to the front-end. */
/* MAX_FOLDER_MSG      The largest message number allocated to the folder.    */
/* ORIG_MSG_SIZE       Enforced by psm.  That is, this is the message length  */
/*                     limit used by psm when a new message is entered.       */
/* LAST_MSG_QUEUE_SIZE This is the size of the queue used to hold information */
/*                     about already issued messages.  It prevents duplicate  */
/*                     messages from being issued when reset_lex is called.   */

# define EXPANDED_MSG_SIZE	500
# define FINAL_MSG_SIZE		(MAX_HDR_SIZE + EXPANDED_MSG_SIZE) 
# define LAST_MSG_QUEUE_SIZE	5
# define MAX_ERR_LIMIT		100 
# define MAX_HDR_SIZE		(MAX_FILE_NAME_SIZE + MAX_FILE_NAME_SIZE)
# define MAX_MSG		9999 
# define MAX_FE_MSG		1999 
# define MAX_FOLDER_MSG		2099
# define ORIG_MSG_SIZE		256		


/******************************\
|* OBJECT REPLACEMENT STRINGS *|
\******************************/


/***********************************\
|* CONDITIONAL REPLACEMENT STRINGS *|
\***********************************/


/***********************************************\
|* STATEMENT/FUNCTION-LIKE REPLACEMENT STRINGS *|
\***********************************************/
