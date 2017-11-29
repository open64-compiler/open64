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


#ifndef NOTES_INCLUDED
#define NOTES_INCLUDED

/* =======================================================================
 * =======================================================================
 *
 *  Module: note.h
 *  $Revision: 1.2 $
 *  $Date: 02/11/07 23:41:27-00:00 $
 *  $Author: fchow@keyresearch.com $
 *  $Source: /scratch/mee/2.4-65/kpro64-pending/be/cg/SCCS/s.note.h $
 *
 *  Revision comments:
 *
 *  27-Feb-1994 - Initial version
 *
 *  Description:
 *  ============
 *
 *  Allows the compiler to place notes to the user in the instruction
 *  stream.  These are useful for various phases of the compiler to add
 *  information that will be  emitted with a .s file so that users can
 *  understand why we did things.
 *
 *  This is structured so as to allow flexibility in exactly what we do
 *  with the basic information.  For example, we may want to include it in
 *  some more compact form in the .o files as well.  It will probably be
 *  useful to look at be/cg/ls_neon_sign.c for an example of what a client
 *  of this be like.
 *
 *  The central object defined by this module is:
 *
 *      TYPE NOTE
 *
 *          a structural type with two fields:
 *
 *              NOTE_HANDLER  handler
 *
 *                  A function pointer that can interpret the
 *                  _info field in various ways.
 *
 *              NOTE_info *info
 *
 *                  The basic information associated with the comment.
 *
 *  Each NOTE refers to an NOTE_HANDLER:
 *
 *      TYPE (void)(*NOTE_HANDLER) (
 *          NOTE_ACTION action,
 *          NOTE_INFO  *info,
 *          FILE       *file
 *      )
 *
 *          A function pointer capable of interpreting a particular 'info'
 *          in various ways depending on the value of 'action'.  Every
 *          NOTE_HANDLER must be able to perform all the possible
 *          NOTE_ACTIONs (see below).  'file' is provided as an output for
 *          printing.  More arguments are possible here in the future,
 *          e.g., a char * as a destination for formatting the comment.
 *
 *  and a NOTE_INFO:
 *
 *      TYPE struct note_info NOTE_INFO
 *
 *          Each module containing NOTE_HANDLER functions should
 *          define (LOCALLY) 'struct note_info' for its own convienience.
 *
 *  Each NOTE_HANDLER function must be able to respond to all the
 *  different NOTE_ACTIONs:
 *
 *      TYPE NOTE_ACTION
 *
 *          An enumerated type with the following members:
 *
 *              NOTE_PRINT_TO_FILE
 *
 *                  Format the comment in a user readable format to the
 *                  'file' argument.
 *
 *              NOTE_PRINT_TO_ANL_FILE
 *
 *                  Format the comment in a format suitable for the ANL file
 *		    to the 'file' argument.
 *
 *              NOTE_PRINT_HANDLER_NAME_TO_FILE
 *
 *                  Format the name of the comment handler to the 'file'
 *                  argument.  This is partucularly useful for debugging
 *                  when we may want to find out just what kind of comment
 *                  we are looking at.
 *
 *
 *  Adding NOTEs to a BB
 *  ====================
 *
 *  Once you have defined your handler functions and _INFO structures, this
 *  is quite easy.  You can add a NOTE as an annotation to a BB with:
 *
 *      void NOTE_Add_To_BB(
 *          BB*        *bb,
 *          NOTE_HANDLER handler,
 *          NOTE_INFO   *info
 *      )
 *
 *          Create a new NOTE and add it to the given BB as an annotation.
 *	    The new NOTE is added below any NOTEs already present in the BB.
 *
 *
 *  Acting on NOTEs in a BB
 *  =======================
 *
 *      void NOTE_BB_Act(
 *          BB          *bb,
 *          NOTE_ACTION action,
 *          FILE        *file
 *      )
 *
 *	    For all the NOTEs in the 'bb' perform the given 'action',
 *	    possibly printing to the 'file' argument.
 *
 *  Retrieving NOTEs in a BB
 *  ========================
 *
 *	NOTE_INFO *NOTE_Retrieve_Note_For_Handler (
 *	    BB *bb,
 *	    NOTE_HANDLER handler
 *	)
 *
 *	    If there is a NOTE for the <handler> return it, otherwise 
 *	    return NULL.
 *
 * =======================================================================
 * ======================================================================= */

#ifdef _KEEP_RCS_ID
static char *notes_rcs_id = "$Source: /scratch/mee/2.4-65/kpro64-pending/be/cg/SCCS/s.note.h $ $Revision: 1.2 $";
#endif /* _KEEP_RCS_ID */

typedef enum {
  NOTE_PRINT_TO_FILE,
  NOTE_PRINT_TO_ANL_FILE,
  NOTE_PRINT_HANDLER_NAME_TO_FILE
} NOTE_ACTION;

typedef struct note_info NOTE_INFO;

typedef void (*NOTE_HANDLER)(NOTE_ACTION,NOTE_INFO*,FILE*);

typedef struct note {
  NOTE_HANDLER   handler;
  NOTE_INFO     *info;
} NOTE;

#define NOTE_handler(x)      ((x)->handler)
#define NOTE_info(x)         ((x)->info)

#define NOTE_ACT(x,action,file)                                  \
  (NOTE_handler(x)((action),(NOTE_info(x)),(file)))

extern void
NOTE_Add_To_BB(
  BB         *bb,
  NOTE_HANDLER handler,
  NOTE_INFO   *info
);


extern void
NOTE_BB_Act(
    BB          *bb,
    NOTE_ACTION  action,
    FILE        *file
);

extern NOTE_INFO *
NOTE_Retrieve_Note_For_Handler(
    BB		*bb,
    NOTE_HANDLER handler
);

#endif /* NOTES_INCLUDED */
