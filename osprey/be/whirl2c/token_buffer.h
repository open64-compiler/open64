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


#ifndef token_buffer_INCLUDED
#define token_buffer_INCLUDED
/* ====================================================================
 * ====================================================================
 *
 * Module: token_buffer.h
 * $Revision: 1.2 $
 * $Date: 02/11/07 23:41:59-00:00 $
 * $Author: fchow@keyresearch.com $
 * $Source: /scratch/mee/2.4-65/kpro64-pending/be/whirl2c/SCCS/s.token_buffer.h $
 *
 * Revision history:
 *  07-Oct-94 - Original Version
 *
 * Description:
 *
 *     The internals of a TOKEN_BUFFER is hidden within this package,
 *     and we export a TOKEN_BUFFER as an abstract data type (AST).
 *     To create and initialize a new TOKEN_BUFFER, call 
 *     New_Token_Buffer().  The facilities to manipulate TOKEN_BUFFER
 *     objects are grouped as follows in this file:
 *
 *        1) The AST itself (TOKEN_BUFFER) and a means for creating
 *           a new instance of a TOKEN_BUFFER.
 *
 *        2) Inquiries about a TOKEN_BUFFER.
 *
 *        3) Initialization and finalization of this subsystem, as
 *           well as facilities for freeing up unused memory.
 *
 *        4) Changing the indentation.
 *
 *        5) Prepending and appending new tokens to the token-list
 *           maintained by a TOKEN_BUFFER.
 *
 *        6) Fortran specific layout functions.
 *
 *        7) Writing a token-list to file, and reclaiming (not
 *           freeing up memory) a TOKEN_BUFFER.
 *
 *     Most of the formatting must be done explicitly by the user
 *     in terms of item 3).  Separators will be automatically
 *     inserted between tokens, as follows:
 *
 *        + Two string tokens are always separated by a space.
 *    
 *        + A string token and a binary/tertiary special operator 
 *          character (i.e. one of: '+', '-', '*', '/', '&', '|',
 *          '%', '=', '?', '<', '>', or ':') are always separated
 *          by a space.  Note that to write C code, "->" should be 
 *          split into special tokens to get "a->b".
 *
 *        + A '}', ')' or ']' special character, followed by a binary 
 *          or tertiary special operator character or a string token,
 *          are always separated by a space.
 *
 *        + A ';' or ',' and any following token except separator 
 *          tokens, are always separated by a space.
 *
 *        + When a limit has been set on the maximum line-length, then
 *          a line may be split anywhere except in the middle of the
 *          characters belonging to a token.  NB: For this reason, try
 *          to use SPECIAL tokens whenever possible and allways put
 *          character string constants in a single token.
 *
 *     There is no limit to the number of token buffers that may be 
 *     created, other than those imposed by memory availablity on the
 *     host system.
 *
 * The opening and closing of files must be taken care of in the
 * environment using this module.  This way we can support a uniform
 * model of token-sequences (TOKEN_BUFFER) without further 
 * consideration of how many output files are being used.
 * ====================================================================
 * ====================================================================
 */


/* Before using any of the facilities provided through this package, the 
 * token buffer subsystem must be initialized.  To free up memory used
 * by all token buffers, the sybsystem must be terminated.  
 *
 * The FORMAT_KIND sets up a default for how many characters may appear 
 * on an output-line (unlimited for FREE_FORMAT and F77_TAB_FORMAT;
 * 72 characters for F77_ANSI_FORMAT.  This default can be reset
 * with a call to Set_Maximum_Linelength subsequent to the call to
 * Initialize_Token_Buffer().  The FORMAT_KIND also determines how lines
 * are continued when the maximum line size is exceeded.  Note that
 * Set_Maximum_Linelength() will reinstate the default maximum linelength,
 * as is determoned by the FORMAT_KIND, when given a zero argument.
 *
 * WARNING: We assume the srcpos_map_file is open when the tokens written
 * to file included any Srcpos_Map token.  We do not currently employ
 * any error-checking mechanism to ensure that this is so and gracefully
 * terminate otherwise.
 * ----------------------------------------------------------------------*/
typedef enum Format_Kind 
{
   FREE_FORMAT = 0,
   F77_TAB_FORMAT = 1,
   F77_ANSI_FORMAT = 2, 
   NUM_FORMAT_KINDS = F77_ANSI_FORMAT
} FORMAT_KIND;

extern void Initialize_Token_Buffer(FORMAT_KIND output_format);
extern void Terminate_Token_Buffer(FILE *srcpos_map_file);

extern void Set_Maximum_Linelength(UINT32 max_linelength);
extern BOOL Has_Maximum_Linelength(void);
extern UINT32 Get_Maximum_Linelength(void);


/* The AST and a means for creating new objects of the type.
 * A token_buffer declaration should always be initialized to
 * NULL or a New_Token_Buffer() prior to any use.
 * ----------------------------------------------------------*/
typedef struct Token_Buffer *TOKEN_BUFFER;
extern TOKEN_BUFFER New_Token_Buffer(void);
extern void Reclaim_Token_Buffer(TOKEN_BUFFER *tokens);


/* Some means for inquiring about a token buffer instance.
 * -------------------------------------------------------*/
extern BOOL Is_Empty_Token_Buffer(TOKEN_BUFFER tokens);
extern BOOL Identical_Token_Lists(TOKEN_BUFFER tokens1, 
				  TOKEN_BUFFER tokens2);


/* Free up buffers that are no longer in use.  Buffers are
   reclaimed by adding them to a free-list.  This routine
   will free up all buffers on the free-list.
   -------------------------------------------------------*/
extern void Free_Token_Buffer_Memory(void);


/* Change indentation for the next call to Append_Indented_Newline()
 * or prepend_indented_newline().  The default unit of indentation is
 * 2 spaces, but this can be changed with set_indentation_step().
 * The indentation can be reset to column 1 with a call to 
 * Zero_Indentation().
 *  ---------------------------------------------------------------*/
extern UINT Current_Indentation(void);
extern void Set_Current_Indentation(UINT ident);
extern void Set_Indentation_Step(UINT num_spaces);
extern void Increment_Indentation(void);
extern void Decrement_Indentation(void);
extern void Zero_Indentation(void);


/* Tokens may be appended to the end (right-hand-side) of the 
 * current token-list or prepended to the beginning (left-hand
 * side) of the current token-list.  We only support the most 
 * basic forms of tokens, these being string constants and 
 * special characters, where separators other than newlines
 * (space characters) are automatically inserted.  The chars
 * in a "const char *" argument is copied when adding a 
 * token-string to a TOKEN_BUFFER.
 * ------------------------------------------------------------*/
extern void Append_Indented_Newline(TOKEN_BUFFER tokens, UINT num_lines);
extern void Append_Token_String(TOKEN_BUFFER tokens, const char *string);
extern void Append_Token_Special(TOKEN_BUFFER tokens, char special);
extern void Append_And_Copy_Token_List(TOKEN_BUFFER result_tokens, 
				       TOKEN_BUFFER copy_tokens);
extern void Append_And_Reclaim_Token_List(TOKEN_BUFFER result_tokens, 
					  TOKEN_BUFFER *reclaim_tokens);

extern void Prepend_Indented_Newline(TOKEN_BUFFER tokens, UINT num_lines);
extern void Prepend_Token_String(TOKEN_BUFFER tokens, const char *string);
extern void Prepend_Token_Special(TOKEN_BUFFER tokens, char special);
extern void Prepend_And_Copy_Token_List(TOKEN_BUFFER result_tokens, 
					TOKEN_BUFFER copy_tokens);
extern void Prepend_And_Reclaim_Token_List(TOKEN_BUFFER result_tokens, 
					   TOKEN_BUFFER *reclaim_tokens);


/* Fortran specific layout functions.  Do not call these function
 * in modes other than F77_TAB_FORMAT and F77_ANSI_FORMAT:
 *
 *    + Append_F77_Indented_Newline() will add a newline character 
 *      followed by an indentation conformant with Fortran rules 
 *      (label==NULL implies "no label") to the given token buffer.
 *
 *    + Append_F77_Indented_Continuation will ass a newline character,
 *      add a continuation marker for the next line and ident it.
 *
 *    + Append_F77_Comment_Newline() will add a sequence of empty
 *      comment lines, following the present line, to the token-list.
 *      The last line will be indented according to the 
 *      Current_Indentation(), if so desired.  The last line may also
 *      be given the "*$*" directive prefix before the indentation,
 *      if so desired.
 *
 *    + Append_F77_Directive_Newline() will add a comment line
 *      to the token-list, after ending the current line.
 *      The new line will contain the directive prefix (e.g. "C*$*").
 *
 *    + Append_F77_Sequence_No() should only be called for 
 *      F77_ANSI_FORMAT and will append spaces to fill the current
 *      line up to column 73 followed by a sequence number in columns 
 *      73-80.
 * ------------------------------------------------------------------*/
extern void Append_F77_Indented_Newline(TOKEN_BUFFER tokens,
					UINT         num_lines,
					const char  *label);
extern void Prepend_F77_Indented_Newline(TOKEN_BUFFER tokens,
					 UINT         num_lines, 
					 const char  *label);

extern void Append_F77_Indented_Continuation(TOKEN_BUFFER tokens);
extern void Prepend_F77_Indented_Continuation(TOKEN_BUFFER tokens);

extern void Append_F77_Comment_Newline(TOKEN_BUFFER tokens, 
				       UINT         num_lines,
				       BOOL         indent_last_line);
extern void Prepend_F77_Comment_Newline(TOKEN_BUFFER tokens, 
					UINT         num_lines,
					BOOL         indent_last_line);

extern void Append_F77_Directive_Newline(TOKEN_BUFFER tokens, 
					 const char  *directive_prefix);
extern void Prepend_F77_Directive_Newline(TOKEN_BUFFER tokens, 
					  const char  *directive_prefix);

extern void Append_F77_Sequence_No(TOKEN_BUFFER tokens, 
				   const char  *seq_no);
extern void Prepend_F77_Sequence_No(TOKEN_BUFFER tokens, 
				    const char  *seq_no);


/* The following routines are used to create tokens representing
 * source position mappings and/or directives.
 *
 * The srcpos mappings relate the "current" location in the output 
 * file, after having written all tokens up to this point, to the 
 * given SRCPOS in the original file. These mappings are inserted
 * into a separate file (see comments about Write_And_Reclaim_Tokens()
 * and Terminate_Token_Buffer()).
 *
 * Each srcpos directive represents a newline followed by a "#line"
 * directive.  The newline to follow the directive must be explicitly
 * added by means of one of the Indented_Newline() routines described
 * above.  These diretives are inserted inline into the generated
 * source code.
 * -----------------------------------------------------------------*/
extern void Append_Srcpos_Map(TOKEN_BUFFER tokens, SRCPOS srcpos);
extern void Append_Srcpos_Directive(TOKEN_BUFFER tokens, SRCPOS srcpos);

extern void Prepend_Srcpos_Map(TOKEN_BUFFER tokens, SRCPOS srcpos);
extern void Prepend_Srcpos_Directive(TOKEN_BUFFER tokens, SRCPOS srcpos);



/* Tokens representing a partial text-line, a full text-line, or 
 * multiple text-lines, may be converted to text strings and written 
 * to the output file (ofile) in left-to-right order.  Once the 
 * TOKEN_BUFFER has been written to file, the TOKEN_BUFFER will be 
 * reclaimed by this subsystem (for reuse) and "*tokens" will be set
 * to NULL.  When srcpos_map_file==NULL, the advances in the output-
 * file current location will be ignored as far as the Srcpos_Map is 
 * concerned.
 *
 * WARNING: We assume the ofile is open for writing without checking
 * that this is so!  Similarly, we assume the srcpos_map_file is
 * open when the tokens may include a Srcpos_Map token.
 *
 * An alternative is provided for writing a token buffer into
 * a string buffer, by means of Str_Write_And_Reclaim_Tokens().
 * No Srcpos_Map is maintained for such output.
 * -----------------------------------------------------------------*/
extern void Write_And_Reclaim_Tokens(FILE         *ofile, 
				     FILE         *srcpos_map_file,
				     TOKEN_BUFFER *tokens);

extern void Str_Write_And_Reclaim_Tokens(char         *strbuf,
					 UINT32        buflen,
					 TOKEN_BUFFER *tokens);


/* Write the given string to the output-file.  The increment in
 * line/column number for the output-file will be ignored as
 * far as the Srcpos_Map is concerned, unless srcpos_map_file!=NULL.
 * -----------------------------------------------------------------*/
void Write_String(FILE *ofile, FILE *srcpos_map_file, const char *str);


#endif /* token_buffer_INCLUDED */


