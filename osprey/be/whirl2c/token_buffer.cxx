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
 * Module: token_buffer.c
 * $Revision: 1.2 $
 * $Date: 02/11/07 23:41:59-00:00 $
 * $Author: fchow@keyresearch.com $
 * $Source: /scratch/mee/2.4-65/kpro64-pending/be/whirl2c/SCCS/s.token_buffer.cxx $
 *
 * Revision history:
 *  07-Oct-94 - Original Version
 *
 * Description:
 *
 *    This package implements the most basic form of token buffer,
 *    where we only distinguish between separators, special character
 *    string tokens, and source position directives.  
 *
 *    The purpose of a token_buffer is two-fold:  It writes token 
 *    sequences to an output-file, which is opened as a side-effect
 *    of initializing this module.  Secondly it provides facilities
 *    for buffering an ordered sequence of tokens.
 *
 *    We maintain a free-list (buffer_free_list) for reusing buffers.
 *    There are really three kinds of buffers:
 *
 *        TOKEN_BUFFER:  Contains information about a buffer and has its
 *                       own private string and token buffer area.  This
 *                       is the "abstract data type" exported by this
 *                       package.
 *
 *        tokens: A memory pool allocated for a given TOKEN_BUFFER,
 *                which contains all its tokens.  The token sequence
 *                will index into this memory pool of tokens.
 *
 *        strings: A memory pool allocated for a given TOKEN_BUFFER,
 *                 which contains all its character strings.  Each
 *                 token will index into this pool of characters.
 *
 *    The buffers and memory pools are never freed up from memory,
 *    unless an explicit request is recieved to do so, and they are 
 *    instead kept on a free_list.
 *
 *    The exported routines are described in token_buffer.h, and are
 *    implemented in terms of various static utility routines and
 *    variables local to this module "body".  The definitions are
 *    in the following order:
 *
 *       1) Macro definitions, data types, and local state.
 *
 *       2) Local utility routines.
 *
 *       3) Implementation of each of the exported routines.
 *
 * WARNING:
 *
 *    This module has been implemented very carefully without any
 *    interspersed assertion checking, so extreme care should be
 *    exercised in any modification to this module.  It is essential
 *    that the implementation of the token-buffer mechanism be as
 *    efficient as is possible.
 *
 * ====================================================================
 * ====================================================================
 */

#ifdef _KEEP_RCS_ID
static char *rcs_id = "$Source: /scratch/mee/2.4-65/kpro64-pending/be/whirl2c/SCCS/s.token_buffer.cxx $ $Revision: 1.2 $";
#endif /* _KEEP_RCS_ID */

#include <stdio.h>
#include <string.h>
#include "defs.h"
#include "srcpos.h"
#include "token_buffer.h"
#include "errors.h"
#include "mempool.h"
#include "wn.h"
#include "ir_reader.h"


/*------------------ macros, types, and local state -------------------*/
/*---------------------------------------------------------------------*/

/* Disallow any use of strcpy, since it potentially dangerous.  We operate
 * with strings that are not NULL-character terminated in the tokens, and
 * as such space is not allocated for such NULL-characters and a strcpy
 * may write a NULL character beyond the end of an allocated buffer.
 */
#define strcpy DO_NOT_USE_STRCPY

/* Macros for allocating, reallocating and freeing up memory.
 */
#define TB_TYPE_ALLOC_N(type, count)\
   TYPE_MEM_POOL_ALLOC_N(type, Malloc_Mem_Pool, count)

#define TB_TYPE_REALLOC_N(type, old_ptr, old_count, new_count)\
   TYPE_MEM_POOL_REALLOC_N(type, Malloc_Mem_Pool, old_ptr,\
			   old_count, new_count)

#define TB_FREE(ptr)\
   MEM_POOL_FREE(Malloc_Mem_Pool, ptr)

/* Commonly used character constants */
#define SPACE ' '
#define NEWLINE '\n'
#define COMMA ','
#define SEMICOLON ';'
#define LEFT_PAREN '('
#define RIGHT_PAREN ')'
#define LEFT_BRACKET '['
#define RIGHT_BRACKET ']'
#define LEFT_BRACE '{'
#define RIGHT_BRACE '}'
#define PLUS '+'
#define MINUS '-' 
#define MULTIPLY '*'
#define DIVIDE '/'
#define BITAND '&'
#define BITOR '|'
#define MODULUS '%'
#define EQUAL '='
#define QUESTION_MARK '?'
#define COLON ':'
#define LESS_THAN '<'
#define LARGER_THAN '>'
#define NOT '!'
#define BIT_COMPLEMENT '~'
#define FORTRAN_COMMENT_CHAR 'C'

/* The buffer structure and its index types */
typedef mUINT32 STRING_IDX;
typedef mUINT32 TOKEN_IDX;
#define MAX_STRING_IDX (STRING_IDX)0x7fffffffU /* max string buffer size */
#define MAX_TOKEN_IDX  (TOKEN_IDX)0x7fffffffU  /* max token buffer size */
#define NO_STRING_IDX (STRING_IDX)0xffffffffU  /* Invalid string buffer index*/
#define NO_TOKEN_IDX  (TOKEN_IDX)0xffffffffU   /* Invalid token buffer index*/
#define INIT_STRING_BUFFER_SIZE 1024
#define INIT_TOKEN_BUFFER_SIZE  512

typedef enum Token_Kind
{
   STRING_TOKEN = 0,
   SPECIAL_TOKEN = 1,
   SEPARATOR_TOKEN = 2,
   DIRECTIVE_TOKEN = 3,
   F77_SEQNO_TOKEN = 4,
   SRCPOS_MAP_TOKEN = 5,
   SRCPOS_DIRECTIVE_TOKEN = 6
} TOKEN_KIND;

typedef struct String_Value
{
   STRING_IDX size; /* String size */
   union
   {
      char       ch[sizeof(STRING_IDX)]; /* SPECIAL_TOKEN and short strings */
      STRING_IDX idx;                    /* ... any longer char strings */
   } string;
} STRING_VALUE;

typedef union Token_Value
{
   SRCPOS       srcpos;  /* SRCPOS value (64 bits) */
   STRING_VALUE str_val; /* string values (64 bits) */
} TOKEN_VALUE;

typedef struct Token
{
   TOKEN_KIND  kind;  /* (32 bits) */
   TOKEN_IDX   next;  /* The next token in a token-sequence (32 bits) */
   TOKEN_VALUE value; /* The characters for the token (64 bits) */
} TOKEN;

#define TOKEN_kind(t) (t)->kind
#define TOKEN_next(t) (t)->next
#define TOKEN_srcpos(t) (t)->value.srcpos
#define TOKEN_char(t) (t)->value.str_val.string.ch[0]
#define TOKEN_short_string(t) (t)->value.str_val.string.ch
#define TOKEN_string_idx(t) (t)->value.str_val.string.idx
#define TOKEN_string_size(t) (t)->value.str_val.size

#define TOKEN_is_short_string(t) (TOKEN_string_size(t) <= sizeof(STRING_IDX))
#define TOKEN_is_string(t) \
   (TOKEN_kind(t) == STRING_TOKEN || \
    TOKEN_kind(t) == SPECIAL_TOKEN || \
    TOKEN_kind(t) == SEPARATOR_TOKEN || \
    TOKEN_kind(t) == DIRECTIVE_TOKEN || \
    TOKEN_kind(t) == F77_SEQNO_TOKEN)


typedef struct Token_Sequence
{
   TOKEN_IDX  first;      /* First token in the token-sequence */
   TOKEN_IDX  last;       /* Last token in the token-sequence */
} TOKEN_SEQUENCE;

struct Token_Buffer
{
   char          *strings;
   TOKEN         *tokens;
   STRING_IDX     chars_allocated;  /* Number of characters allocated */
   STRING_IDX     chars_used;       /* Number of characters used */
   TOKEN_IDX      tokens_allocated; /* Number of tokens allocated */
   TOKEN_IDX      tokens_used;      /* Number of tokens used */
   TOKEN_SEQUENCE token_list;       /* The token-sequence */
   TOKEN_BUFFER   next;             /* Maintains free-list of buffers */
};

/* Given that the token is of a kind that has an associated char-string,
 * this macro will return the address to this string (currently, only
 * SRCPOS tokens do not have a char-string, while SPECIAL_CHAR tokens
 * are represented as a string with one char and can alternatively be
 * accessed with the TOKEN_char(a_token) macro call).  There is no
 * assertion checking on this access to the string.
 */
#define TOKEN_BUFFER_get_char_string(buf, a_token)\
   (TOKEN_is_short_string(a_token)? \
    TOKEN_short_string(a_token) : \
    &buf->strings[TOKEN_string_idx(a_token)])

/* Local state variables */
static TOKEN_BUFFER buffer_free_list = NULL;


#define WRITE_BUFFER_SIZE 256
#define INVALID_SPLIT_PT -1
static char write_buffer[WRITE_BUFFER_SIZE+1];
static INT32 write_buffer_next = 0; /* Need to access the previous idx as -1 */
static INT32 last_split_pt = INVALID_SPLIT_PT; /* To split too long lines */


#define MAX_INDENTATION 40
#define MAX_INDENTATION_STEP 10
static UINT32 indentation_increment = 2; /* Ident by 2 spaces at a time */
static INT32  current_indentation = 0;   /* Never to exceed MAX_INDENTATION */
static INT32  requested_indentation = 0; /* May exceed MAX_INDENTATION */


/* How do we format the output characters? */
#define USE_UNLIMITED_LINE_LENGTH (Max_Line_Length == 0)
static UINT32 Max_Line_Length = 0; /* Unlimited line-length */
static FORMAT_KIND Output_Format = FREE_FORMAT;


/* What is the current position in the output file, taking into
 * account the line currently being buffered for output.
 */
static UINT32 Current_Output_Col = 1;
static UINT32 Current_Output_Line = 1;


/* What is the maxumum file-number encountered in any SRCPOS_MAP token.
 */
static UINT32 Max_Srcpos_Map_Filenum = 0;

/* What is the default maximum line lengths for the various formatting
 * kinds that we support.
 */
static UINT32 Default_Max_Line_Length[NUM_FORMAT_KINDS+1] =
{
   0,  /* FREE_FORMAT: no limit */
   0,  /* F77_TAB_FORMAT: no limit */
   72  /* F77_ANSI_FORMAT: limited to 72 characters */
};

/* Fortran directives are somewhat special in that they enforce a 72
 * character limit on line-lengths.
 */
#define MAX_F77_DIRECTIVE_PREFIX_SIZE 24
static BOOL   Inside_F77_Directive = FALSE;
static UINT32 Max_Line_Length_Outside_F77_Directive;
static char   F77_Directive_Continuation[MAX_F77_DIRECTIVE_PREFIX_SIZE+1];


/*--------------- routines for debugging a token buffer ---------------*/
/*---------------------------------------------------------------------*/

#define DBGOUT stderr   /* debugging output file */

void
dbg_tokens(TOKEN_BUFFER buf, BOOL with_token_name)
{
   const char *str;
   STRING_IDX  c;
   TOKEN_IDX   t;
   TOKEN      *a_token;
   USRCPOS     usrcpos;
   
   for (t = buf->token_list.first; t != NO_TOKEN_IDX; t = TOKEN_next(a_token))
   {
      a_token = &buf->tokens[t];
      
      switch (TOKEN_kind(a_token))
      {
      case F77_SEQNO_TOKEN:
	 if (with_token_name)
	    fputs("F77_SEQNO_TOKEN(", DBGOUT);
	 str = TOKEN_BUFFER_get_char_string(buf, a_token);
	 for (c = 0; c < TOKEN_string_size(a_token); c++)
	    fputc(str[c], DBGOUT);
	 break;

      case STRING_TOKEN:
	 if (with_token_name)
	    fputs("STRING_TOKEN(", DBGOUT);
	 str = TOKEN_BUFFER_get_char_string(buf, a_token);
	 for (c = 0; c < TOKEN_string_size(a_token); c++)
	    fputc(str[c], DBGOUT);
	 break;

      case SEPARATOR_TOKEN:
	 if (with_token_name)
	    fputs("SEPARATOR_TOKEN(", DBGOUT);
	 str = TOKEN_BUFFER_get_char_string(buf, a_token);
	 for (c = 0; c < TOKEN_string_size(a_token); c++)
	    fputc(str[c], DBGOUT);
	 break;

      case DIRECTIVE_TOKEN:
	 if (with_token_name)
	    fputs("DIRECTIVE_TOKEN(", DBGOUT);
	 str = TOKEN_BUFFER_get_char_string(buf, a_token);
	 for (c = 0; c < TOKEN_string_size(a_token); c++)
	    fputc(str[c], DBGOUT);
	 break;

      case SPECIAL_TOKEN:
	 if (with_token_name)
	    fputs("SPECIAL_TOKEN(", DBGOUT);
	 fputc(TOKEN_char(a_token), DBGOUT);
	 break;

      case SRCPOS_MAP_TOKEN:
	 USRCPOS_srcpos(usrcpos) = TOKEN_srcpos(a_token);
	 fprintf(DBGOUT,
		 "SRCPOS_MAP(%d, %d, %d)",
		 USRCPOS_column(usrcpos), 
		 USRCPOS_linenum(usrcpos), 
		 USRCPOS_filenum(usrcpos));
	 break;
	 
      case SRCPOS_DIRECTIVE_TOKEN:
	 USRCPOS_srcpos(usrcpos) = TOKEN_srcpos(a_token);
	 fprintf(DBGOUT,
		 "SRCPOS_DIRECTIVE(%d, %d, %d)",
		 USRCPOS_column(usrcpos), 
		 USRCPOS_linenum(usrcpos), 
		 USRCPOS_filenum(usrcpos));
	 break;

      default:
	 Is_True(FALSE, ("Attempt to write invalid token kind"));
	 break;
      }
      if (with_token_name)
	 fputs(")\n", DBGOUT);
   }
   if (!with_token_name)
      fputs("\n", DBGOUT);
} /* dbg_tokens */


/*------------------ general purpose hidden routines ------------------*/
/*---------------------------------------------------------------------*/

/* Determine whether we have exceeded the line-length limit and have
 * a place to insert a split-point.
 */
#define CAN_SPLIT_LINE (last_split_pt != INVALID_SPLIT_PT)
#define NEED_TO_SPLIT_LINE \
   (!USE_UNLIMITED_LINE_LENGTH && Current_Output_Col > Max_Line_Length)

#ifdef BUILD_WHIRL2F
#define is_binary_or_tertiary_op(c) \
   (c==PLUS          || \
    c==MINUS         || \
    c==MULTIPLY      || \
    c==DIVIDE        || \
    c==BITAND        || \
    c==BITOR         || \
    c==EQUAL         || \
    c==NOT           || \
    c==QUESTION_MARK || \
    c==COLON         || \
    c==LESS_THAN     || \
    c==LARGER_THAN)
#else
#define is_binary_or_tertiary_op(c) \
   (c==PLUS          || \
    c==MINUS         || \
    c==MULTIPLY      || \
    c==DIVIDE        || \
    c==BITAND        || \
    c==BITOR         || \
    c==MODULUS       || \
    c==EQUAL         || \
    c==NOT           || \
    c==QUESTION_MARK || \
    c==COLON         || \
    c==LESS_THAN     || \
    c==LARGER_THAN)
#endif

#define is_begin_grouping(c) \
   (c==LEFT_PAREN    || \
    c==LEFT_BRACKET  || \
    c==LEFT_BRACE)

#define is_end_grouping(c) \
   (c==RIGHT_PAREN   || \
    c==RIGHT_BRACKET || \
    c==RIGHT_BRACE)


/* Determine whether or not c2 is a unary operator, assuming the 
 * previous character c1 is also a special character.
 */
#define is_unary_op(c1, c2) \
   ((is_binary_or_tertiary_op(c1) || \
     is_begin_grouping(c1)        || \
     is_comma_or_semicolon(c1))     && \
    is_binary_or_tertiary_op(c2)    && \
    c1 != c2                        && \
    c2 != EQUAL                     && \
    !is_end_grouping(c1)            && \
    !(c1==MINUS && c2==LARGER_THAN))   /* Field selection */


#define is_comma_or_semicolon(c) (c==COMMA || c==SEMICOLON)


static TOKEN_IDX 
get_new_tokens(TOKEN_BUFFER buf, INT number_of_tokens)
{
   /* Allocates number_of_tokens, with successive indices
    * starting at the returned idx.
    */
   const TOKEN_IDX return_idx = buf->tokens_used;
   TOKEN_IDX       max_tokens = buf->tokens_allocated;

   buf->tokens_used += number_of_tokens;
   Is_True(buf->tokens_used < MAX_TOKEN_IDX, ("Too many tokens!"));
   if (buf->tokens_used > max_tokens)
   {
      /* Need to reallocate tokens; double size if the number of tokens
       * is less than 8K, otherwise increment the number of tokens by 8K.
       */
      if (max_tokens < 0x2000)
	 do {
	    max_tokens *= 2;
	 } while (buf->tokens_used > max_tokens);
      else
	 do {
	    max_tokens += 0x2000;
	 } while (buf->tokens_used > max_tokens);
      
      buf->tokens =
	 TB_TYPE_REALLOC_N(TOKEN,                 /* type */
			   buf->tokens,           /* old ptr */
			   buf->tokens_allocated, /* old count */
			   max_tokens);           /* new count */
      buf->tokens_allocated = max_tokens;
   }
   return (return_idx);
} /* get_new_tokens */


static STRING_IDX 
get_new_string(TOKEN_BUFFER buf, INT16 stringsize)
{
   /* Allocates number_of_char, with successive indices
    * starting at the returned idx.
    */
   const STRING_IDX return_idx = buf->chars_used;
   STRING_IDX       max_chars = buf->chars_allocated;

   buf->chars_used += stringsize;
   Is_True(buf->chars_used < MAX_STRING_IDX, ("Too many output characters!"));
   if (buf->chars_used > max_chars)
   {
      /* Need to reallocate string-buffer; double size if the buffer is 
       * less than 32K in size, otherwise increment size by 32K.
       */
      if (max_chars < 0x8000)
	 do {
	    max_chars *= 2;
	 } while (buf->chars_used > max_chars);
      else
	 do {
	    max_chars += 0x8000;
	 } while (buf->chars_used > max_chars);
      
      buf->strings =
	 TB_TYPE_REALLOC_N(char,                 /* type */
			   buf->strings,         /* old ptr */
			   buf->chars_allocated, /* old count */
			   max_chars);           /* new count */
      buf->chars_allocated = max_chars;
   }
   return return_idx;
} /* get_new_string */


static char *
Allocate_Token_String(TOKEN_BUFFER buf, TOKEN *a_token, STRING_IDX str_size)
{
   char *str;
   
   TOKEN_string_size(a_token) = str_size;
   if (TOKEN_is_short_string(a_token))
   {
      str = TOKEN_short_string(a_token);
   }
   else
   {
      TOKEN_string_idx(a_token) = 
	 get_new_string(buf, TOKEN_string_size(a_token));
      str = TOKEN_BUFFER_get_char_string(buf, a_token);
   }
   return str;
} /* Allocate_Token_String */


static void
free_buffer_list(TOKEN_BUFFER free_list)
{
   TOKEN_BUFFER to_be_freed, next = free_list;

   while (next != NULL)
   {
      TB_FREE(next->strings);
      TB_FREE(next->tokens);
      to_be_freed = next;
      next = next->next;
      TB_FREE(to_be_freed);
   }
} /* free_buffer_list */


static TOKEN_IDX
indented_newline_token(TOKEN_BUFFER buffer, 
		       UINT         num_lines,
		       BOOL         continuation,
		       const char  *label)
{
   /* Emit num_lines '\n' characters, followed by the Fortran
    * formatting offset and label, followed by the continuation
    * mark and the indentation amount.  Note that the indentation
    * for labels of more than 5 characters in F77_TAB_FORMAT will
    * be larger than expected (however, this should be very rare).
    */
   char      *str;
   STRING_IDX char_idx;
   INT32      label_size = (label==NULL? 0 : strlen(label));
   INT32      prefix_size;
   TOKEN_IDX  new_token_idx = get_new_tokens(buffer, 1);
   TOKEN     *a_token = &buffer->tokens[new_token_idx];

   Is_True(label_size < 6, ("Too large label at beginning for Fortran line"));

   /* Allocate a string for this SEPERATOR token */
   if (Output_Format == FREE_FORMAT)
      prefix_size = 0;
   else if (Output_Format == F77_TAB_FORMAT)
      prefix_size = label_size + (continuation? 2 : 1);
   else if (Output_Format == F77_ANSI_FORMAT)
      prefix_size = 6;

   /* Initiate the token */
   TOKEN_kind(a_token) = SEPARATOR_TOKEN;
   TOKEN_next(a_token) = NO_TOKEN_IDX;
   str = Allocate_Token_String(buffer, a_token, 
			       prefix_size + current_indentation + num_lines);

   /* Add the newline ('\n') characters to the string and 
    * advance char_idx one past the last newline character.
    */
   for (char_idx = 0; char_idx < num_lines; char_idx++)
      str[char_idx] = NEWLINE;

   /* Add the Fortran specific label (char_idx==num_lines) */
   if (label_size > 0)
   {
      (void)strncpy(&str[char_idx], &label[0], label_size);
      char_idx += label_size;
   }
      
   /* Add the fortran specific layout prefix and continuation marks
    * (char_idx==new_string_idx+num_lines+label_size)
    */
   if (Output_Format == F77_TAB_FORMAT)
   {
      str[char_idx++] = '\t';
      if (continuation)
	 str[char_idx++] = '1';
   }
   else if (Output_Format == F77_ANSI_FORMAT && continuation)
   {
      while (char_idx < num_lines + prefix_size - 1)
	 str[char_idx++] = SPACE;
      str[char_idx++] = '>';
   }

   /* Add the indentation (char_idx==new_string_idx+num_lines+prefix_size) */
   while (char_idx < num_lines + prefix_size + current_indentation)
      str[char_idx++] = SPACE;

   return new_token_idx;
} /* indented_newline_token */


static TOKEN_IDX
F77_comment_line_token(TOKEN_BUFFER buffer,
		       UINT         num_lines,
		       const char  *comment_prefix,
		       BOOL         indent_last_line)
{
   /* Emit num_lines '\n' characters, each followed by the
    * comment_prefix in column one.  Use the current Indent the last
    * line when "indent==TRUE".  Must be in one of the Fortran 
    * formatting modes.  With no indentation of the last line, we do 
    * not even insert the tab character or standard fomatting space
    * characters.
    */
   const INT32 comment_prefix_size = strlen(comment_prefix);
   char        indentation_str[MAX_INDENTATION+50]; /* E.g. "C$\t   " */
   INT32       indentation_size;
   INT32       num_spaces, lines;
   char       *str;
   TOKEN_IDX   new_token_idx = get_new_tokens(buffer, 1);
   TOKEN      *a_token = &buffer->tokens[new_token_idx];

   /* Get the string used to indent the last line (preceding actual comment).
    */
   strncpy(indentation_str, comment_prefix, comment_prefix_size);
   indentation_size = comment_prefix_size;
   if (indent_last_line)
   {
      /* Assume the comment_prefix is smaller than the F77 tab or space
       * prefix.
       */
      if (Output_Format == F77_TAB_FORMAT)
      {
	 indentation_str[indentation_size++] = '\t';
      }
      else /* Output_Format == F77_ANSI_FORMAT */
      {
	 while (indentation_size < 6)
	    indentation_str[indentation_size++] = SPACE;
      }
      for (num_spaces = 0; num_spaces < current_indentation; num_spaces++)
      {
	 indentation_str[indentation_size++] = SPACE;
      }
   } /* if indent last line */

   /* Allocate a string for this SEPERATOR token and initiate it */
   TOKEN_kind(a_token) = SEPARATOR_TOKEN;
   TOKEN_next(a_token) = NO_TOKEN_IDX;
   str = Allocate_Token_String(buffer, a_token,
			       num_lines + /* '\n' */
			       (num_lines - 1)*comment_prefix_size +
			       indentation_size);

   /* Add the newline ("\n<comment_prefix>") characters to the string */
   for (lines = 1; lines < num_lines; lines++)
   {
      *str++ = NEWLINE;
      str = strncpy(str, comment_prefix, comment_prefix_size);
      str += comment_prefix_size;
   }
   *str++ = NEWLINE;

   /* Add the last line indentation */
   str = strncpy(str, indentation_str, indentation_size);

   return new_token_idx;
} /* F77_comment_line_token */


static TOKEN_IDX
F77_directive_line_token(TOKEN_BUFFER buffer,
			 const char  *directive_prefix)
{
   /* Emit a Fortran directive, which must be specially tokenized since
    * it must NEVER exceed a line-length of 72 characters (there is no
    * tab formatting mode for directives).  The directive will be preceeded
    * by a newline character.  Note that all subsequent tokens, up till
    * the next explicit NEWLINE character, apply to this token.
    */
   const INT32 directive_prefix_size = strlen(directive_prefix);
   char       *str;
   TOKEN_IDX   new_token_idx = get_new_tokens(buffer, 1);
   TOKEN      *a_token = &buffer->tokens[new_token_idx];

   /* Allocate a string for this DIRECTIVE token and initiate it.
    */
   TOKEN_kind(a_token) = DIRECTIVE_TOKEN;
   TOKEN_next(a_token) = NO_TOKEN_IDX;
   str = Allocate_Token_String(buffer, a_token, 
			       1/*'\n'*/ + directive_prefix_size);

   /* Insert the new ("\n<diretcive_prefix>") characters into the token.
    */
   *str++ = NEWLINE;
   str = strncpy(str, directive_prefix, directive_prefix_size);

   return new_token_idx;
} /* F77_directive_line_token */


static TOKEN_IDX
string_token(TOKEN_BUFFER buffer, const char *string)
{
   char      *str;
   TOKEN_IDX  new_token_idx = get_new_tokens(buffer, 1);
   TOKEN     *a_token = &buffer->tokens[new_token_idx];

   /* Initiate the token */
   TOKEN_kind(a_token) = STRING_TOKEN;
   TOKEN_next(a_token) = NO_TOKEN_IDX;
   str = Allocate_Token_String(buffer, a_token, strlen(string));

   /* Initiate the string for this token */
   (void)strncpy(str, string, TOKEN_string_size(a_token));

   return new_token_idx;
} /* string_token */


static TOKEN_IDX
f77_seqno_token(TOKEN_BUFFER buffer, const char *seqno)
{
   TOKEN_IDX idx = string_token(buffer, seqno);
   TOKEN_kind(&buffer->tokens[idx]) = F77_SEQNO_TOKEN;
   return idx;
} /* f77_seqno_token */


static TOKEN_IDX
special_char_token(TOKEN_BUFFER buffer, char special)
{
   const TOKEN_IDX  new_token_idx = get_new_tokens(buffer, 1);
   TOKEN           *a_token = &buffer->tokens[new_token_idx];

   /* Initiate the token */
   TOKEN_kind(a_token) = SPECIAL_TOKEN;
   TOKEN_next(a_token) = NO_TOKEN_IDX;
   TOKEN_char(a_token) = special;
   TOKEN_string_size(a_token) = 1;

   return new_token_idx;
}


static TOKEN_IDX
Srcpos_Map_Token(TOKEN_BUFFER buffer, SRCPOS srcpos)
{
   const TOKEN_IDX  new_token_idx = get_new_tokens(buffer, 1);
   TOKEN           *a_token = &buffer->tokens[new_token_idx];

   /* Initiate the token */
   TOKEN_kind(a_token) = SRCPOS_MAP_TOKEN;
   TOKEN_next(a_token) = NO_TOKEN_IDX;
   TOKEN_srcpos(a_token) = srcpos;

   return new_token_idx;
} /* Srcpos_Map_Token */


static TOKEN_IDX
Srcpos_Directive_Token(TOKEN_BUFFER buffer, SRCPOS srcpos)
{
   const TOKEN_IDX  new_token_idx = get_new_tokens(buffer, 1);
   TOKEN           *a_token = &buffer->tokens[new_token_idx];

   /* Initiate the token */
   TOKEN_kind(a_token) = SRCPOS_DIRECTIVE_TOKEN;
   TOKEN_next(a_token) = NO_TOKEN_IDX;
   TOKEN_srcpos(a_token) = srcpos;

   return new_token_idx;
} /* Srcpos_Directive_Token */


static TOKEN_SEQUENCE
copy_token_list(TOKEN_BUFFER to_buffer, TOKEN_BUFFER from_buffer)
{
   /* Copy the tokens from from_buffer into to_buffer, and
    * return a pointer to the first and last tokens of the 
    * resultant token list without appending or prepending 
    * it to the existing tokens in the to_buffer.  The next
    * pointers for the token_sequence will all be correct.
    */
   TOKEN_IDX       token_offset, from_idx;
   STRING_IDX      char_offset, char_idx;
   TOKEN          *new_token;
   TOKEN_SEQUENCE  token_list;

   if (from_buffer->tokens_used == 0)
   {
      token_list.first = NO_TOKEN_IDX;
      token_list.last = NO_TOKEN_IDX;
   }
   else
   {
      /* We take advantage of the fact that token_lists and characters
       * will be allocated in consecutive regions of memory, and we
       * therefore allocate all we need immediately.
       */
      token_offset = get_new_tokens(to_buffer, from_buffer->tokens_used);
      char_offset = get_new_string(to_buffer, from_buffer->chars_used);
      token_list.first = from_buffer->token_list.first + token_offset;
      token_list.last = from_buffer->token_list.last + token_offset;

      /* Copy the string into the target buffer.
       */
      for (char_idx = 0; char_idx < from_buffer->chars_used; char_idx++)
	 to_buffer->strings[char_offset + char_idx] = 
	    from_buffer->strings[char_idx];
      
      /* Copy the tokens into the new token list, and update the string
       * indices and next indices accordingly.
       */
      for (from_idx = 0; from_idx < from_buffer->tokens_used; from_idx++)
      {
	 new_token = &to_buffer->tokens[token_offset + from_idx];
	 *new_token = from_buffer->tokens[from_idx]; /* copy token */
	 TOKEN_next(new_token) += token_offset;      /* correct next idx */

	 /* correct string idx, if relevant */
	 if (TOKEN_is_string(new_token) && !TOKEN_is_short_string(new_token))
	    TOKEN_string_idx(new_token) += char_offset;
      }

      /* Correct the next index for the last token in the copied sequence */
      to_buffer->tokens[token_list.last].next = NO_TOKEN_IDX;
   }
   return token_list;
} /* copy_token_list */


static void
append_token_list(TOKEN_BUFFER buffer, TOKEN_SEQUENCE token_list)
{
   Is_True(token_list.first != NO_TOKEN_IDX, 
	   ("Cannot append empty token_sequence"));
   if (buffer->token_list.first == NO_TOKEN_IDX)
      buffer->token_list.first = token_list.first;
   else
      TOKEN_next(&buffer->tokens[buffer->token_list.last]) = token_list.first;
   buffer->token_list.last = token_list.last;
} /* append_token_list */


static void
prepend_token_list(TOKEN_BUFFER buffer, TOKEN_SEQUENCE token_list)
{
   Is_True(token_list.first != NO_TOKEN_IDX, 
	   ("Cannot prepend empty token_sequence"));
   if (buffer->token_list.last == NO_TOKEN_IDX)
      buffer->token_list.last = token_list.last;
   else
      TOKEN_next(&buffer->tokens[token_list.last]) = buffer->token_list.first;
   buffer->token_list.first = token_list.first;
} /* prepend_token_list */


static void
write_into_string_buf(const char *from, 
		      UINT        from_size,
		      char      **into, 
		      UINT       *into_size)
{
   /* Write "from_size" characters into "into" and update
    * the "into" buffer and the "into_size" to refer to the
    * remaining part of the buffer.
    */
   if (into_size && (from_size >= *into_size))
   {
      fprintf(stderr, 
	      "ERROR: -flist/-clist string-buffer overflow in"
	      "write_into_string_buf() !");
      from_size = *into_size - 1;
   }
   if ((from_size > 0) && into)
   {
      strncpy(*into, from, from_size);
      *into = *into + from_size;
      *into_size -= from_size;
   }
} /* write_into_string_buf */


static void
flush_write_buffer(FILE *ofile, char **buffer, UINT *buflen)
{
   /* NOTE: THIS SHOULD BE THE ONLY PLACE WHERE TOKENS ARE WRITTEN
    * TO FILE!
    *
    * Write buffer contents to file, but not beyond the 
    * last_split_pt.
    */
   UINT32 buffer_idx;
   char   saved_ch;
   
   if (write_buffer_next > 0)
   {
      /* If there is a splitpoint somewhere in the buffer, but not at
       * the very beginning of the buffer and not at the very end of
       * the buffer, then emit characters up till that point and 
       * restore the remaining characters into the buffer; 
       * otherwise, write out the whole buffer.
       */
      if (last_split_pt > 0 && last_split_pt < write_buffer_next)
      {
	 saved_ch = write_buffer[last_split_pt]; /* save splitpoint */
	 write_buffer[last_split_pt] = '\0';
	 if (ofile != NULL)
	    fputs(&write_buffer[0], ofile);
	 else
	    write_into_string_buf(
	       &write_buffer[0], last_split_pt, buffer, buflen);
	 
	 /* Restore the characters following and including the last_split_pt
	  * into the buffer.
	  */
	 write_buffer[0] = saved_ch;             /* restore splitpoint */
	 for (buffer_idx = 1; 
	      buffer_idx+last_split_pt < write_buffer_next;
	      buffer_idx++)
	 {
	    write_buffer[buffer_idx] = write_buffer[buffer_idx+last_split_pt];
	 }
	 last_split_pt = 0;
	 write_buffer_next = buffer_idx;
      }
      else
      {
	 write_buffer[write_buffer_next] = '\0';
	 if (ofile != NULL)
	    fputs(&write_buffer[0], ofile);
	 else 
	    write_into_string_buf(
	       &write_buffer[0], write_buffer_next, buffer, buflen);
	 if (last_split_pt == write_buffer_next)
	    last_split_pt = 0;
	 else
	    last_split_pt = INVALID_SPLIT_PT;
	 write_buffer_next = 0;
      }
   }
} /* flush_write_buffer */


static void Output_Character(FILE *ofile, char **strbuf, UINT *strlen, char c);

static void
Split_The_Current_Output_Line(FILE *ofile,  /* NULL when strbuf!=NULL */
			      char **strbuf, /* NULL when ofile!=NULL */
			      UINT  *strlen) /* Relevant for strbuf!=NULL */
{
   UINT32      idx;
   UINT32      num_chars_after_split;
   const char *continuation_prefix;
   char        tmp_buffer[WRITE_BUFFER_SIZE+1];
   const BOOL  is_inside_directive = Inside_F77_Directive;

   /* Copy the characters following the split-point into a temporary
    * buffer, and set buffer_idx.
    */
   for (idx = last_split_pt; idx < write_buffer_next; idx++)
   {
      tmp_buffer[idx - last_split_pt] = write_buffer[idx];
   }
   num_chars_after_split = write_buffer_next - last_split_pt;
      
   /* Determine the continuation prefix for this line-split */
   if (is_inside_directive)
   {
      continuation_prefix = &F77_Directive_Continuation[0];
      Inside_F77_Directive = FALSE;
   }
   else if (Output_Format == FREE_FORMAT)
      continuation_prefix = "\\\n"; /* backslash followed by newline */
   else if (Output_Format == F77_TAB_FORMAT)
      continuation_prefix = "\n\t1 "; /* newline followed by tab and '1' */
   else /* if (Output_Format == F77_ANSI_FORMAT) */
      continuation_prefix = "\n     > "; /* newline followed by 5 spaces */

   /* Reset the buffer to only account for characters up to the
    * current split-point, then add back in the remaining characters
    * after having added in the split-point itself.
    */
   write_buffer_next = last_split_pt;
   last_split_pt = INVALID_SPLIT_PT;
   Current_Output_Col -= num_chars_after_split;
   Inside_F77_Directive = FALSE;
   for (idx = 0; continuation_prefix[idx] != '\0'; idx++)
      Output_Character(ofile, strbuf, strlen, continuation_prefix[idx]);
   for (idx = 0; idx < num_chars_after_split; idx++)
      Output_Character(ofile, strbuf, strlen, tmp_buffer[idx]);

   Inside_F77_Directive = is_inside_directive;
} /* Split_The_Current_Output_Line */


static void
Output_Character(FILE  *ofile,  /* NULL when strbuf!=NULL */
		 char **strbuf, /* NULL when ofile!=NULL */
		 UINT  *strlen, /* Only relevant when strbuf!=NULL */
		 char   c)
{
   /* Write the buffer to file once it is full */
   if (write_buffer_next+1 >= WRITE_BUFFER_SIZE)
      flush_write_buffer(ofile, strbuf, strlen);

   /* Add this character to file */
   write_buffer[write_buffer_next++] = c;

   /* Keep track of the current positioning in the output file */
   if (c == '\n')
   {
      Current_Output_Col = 1;
      Current_Output_Line++;
      last_split_pt = INVALID_SPLIT_PT;
      if (Inside_F77_Directive)
      {
	 Inside_F77_Directive = FALSE;
         Max_Line_Length = Max_Line_Length_Outside_F77_Directive;
      }
   }
   else
   {
      Current_Output_Col++;
   }
   
   if (NEED_TO_SPLIT_LINE && CAN_SPLIT_LINE)
      Split_The_Current_Output_Line(ofile, strbuf, strlen);

} /* Output_Character */


static void 
Output_Srcpos_Map(FILE *mapfile, SRCPOS srcpos)
{
   /* Output the current source position, relative to the
    * Current_Output_Line and the Current_Output_Col in
    * a Lisp like predicate form.  If this token is
    * used in conjunction with a SRCPOS_DIRECTIVE token,
    * it should typically follow the indented newline token
    * that follows the SRCPOS_DIRECTIVE token to get the 
    * Current_Output_Line and Current_Output_Col right.
    * NOTE that this token is assumed output to a file 
    * different from the regular output-file, and hence
    * we do not use the Output_Character() function.
    */
   INT32 status;
   USRCPOS usrcpos;
   
   if (srcpos != 0)
   {
      USRCPOS_srcpos(usrcpos) = srcpos;

      /* Get the maximum file-number, such we later can write out the
       * whole file-table.
       */
      if (USRCPOS_filenum(usrcpos) > Max_Srcpos_Map_Filenum)
	 Max_Srcpos_Map_Filenum = USRCPOS_filenum(usrcpos);
      
      status = fprintf(mapfile, 
                       " ((%u %u) (%u %u %u))\n",
                       Current_Output_Line, 
                       Current_Output_Col,
                       USRCPOS_filenum(usrcpos),
                       USRCPOS_linenum(usrcpos),
                       USRCPOS_column(usrcpos));

      Is_True(status >= 0, ("Output error to srcpos mapping file"));
   }
} /* Output_Srcpos_Map */


static void 
Output_Srcpos_Directive(FILE  *ofile, 
			char **strbuf, 
			UINT  *strlen, 
			SRCPOS srcpos)
{
   /* Output the current source position in the form
    * of a "#line" preprocessing directive.  Since this
    * directive applies to everything that follows, it
    * implicitly causes a newline to precede it.  The
    * implicit newline should be known to users of this
    * module and means this token should precede the
    * indented newline that typically precedes a stmt.
    */
   char        Srcpos_Directive[1024];
   const char *fname;
   const char *dirname;
   STRING_IDX  ch_idx;
   USRCPOS     usrcpos;
   
   if (srcpos != 0)
   {
      IR_Srcpos_Filename(srcpos, &fname, &dirname);
      USRCPOS_srcpos(usrcpos) = srcpos;
      if (fname != NULL && dirname != NULL)
      {
	 sprintf(Srcpos_Directive,
		 "\n#line %d \"%s/%s\"",
		 USRCPOS_linenum(usrcpos), dirname, fname);
      }
      else if (fname != NULL)
      {
	 sprintf(Srcpos_Directive, 
		 "\n#line %d \"%s\"", USRCPOS_linenum(usrcpos), fname);
      }
      else
      {
	 sprintf(Srcpos_Directive, "\n#line %d",  USRCPOS_linenum(usrcpos));
      }

      /* Write the directive to file.
       */
      for (ch_idx = 0; Srcpos_Directive[ch_idx] != '\0'; ch_idx++)
	 Output_Character(ofile, strbuf, strlen, Srcpos_Directive[ch_idx]);
   }
} /* Output_Srcpos_Directive */


static void 
write_token(FILE        *ofile,   /* NULL if strbuf!=NULL */
	    char       **strbuf,  /* NULL if ofile!=NULL */
	    UINT        *strlen,  /* Only relevant when strbuf!=NULL */
	    TOKEN_BUFFER buffer, 
	    TOKEN_IDX    this_token)
{
   const char *str;
   STRING_IDX  ch_idx;
   TOKEN      *a_token = &buffer->tokens[this_token];
   
   Is_True(this_token != NO_TOKEN_IDX, ("Cannot write non-existent token"));
   
   switch (TOKEN_kind(a_token))
   {
   case F77_SEQNO_TOKEN:
      /* Fill up the current line to column 73, where the sequence number
       * will begin, before entering the sequence number.
       */
      for (ch_idx = Current_Output_Col; ch_idx < 73; ch_idx++)
	 Output_Character(ofile, strbuf, strlen, SPACE);

      str = TOKEN_BUFFER_get_char_string(buffer, a_token);
      for (ch_idx = 0; ch_idx < TOKEN_string_size(a_token); ch_idx++)
	 Output_Character(ofile, strbuf, strlen, str[ch_idx]);
      break;

   case STRING_TOKEN:
      str = TOKEN_BUFFER_get_char_string(buffer, a_token);
      for (ch_idx = 0; ch_idx < TOKEN_string_size(a_token); ch_idx++)
	 Output_Character(ofile, strbuf, strlen, str[ch_idx]);
      last_split_pt = write_buffer_next;
      break;

   case SEPARATOR_TOKEN:
      str = TOKEN_BUFFER_get_char_string(buffer, a_token);
      for (ch_idx = 0; ch_idx < TOKEN_string_size(a_token); ch_idx++)
	 Output_Character(ofile, strbuf, strlen, str[ch_idx]);
      break;

   case DIRECTIVE_TOKEN:
      /* Output the diretive prefix.
       */
      str = TOKEN_BUFFER_get_char_string(buffer, a_token);
      for (ch_idx = 0; ch_idx < TOKEN_string_size(a_token); ch_idx++)
	 Output_Character(ofile, strbuf, strlen, str[ch_idx]);

      /* Note the fact that subsequent tokens apply to a Fortran directive
       * and that lines should be limited to 72 characters.
       */
      Is_True(TOKEN_string_size(a_token) < MAX_F77_DIRECTIVE_PREFIX_SIZE-2,
	      ("Too large directive prefix (max = %d)", 
	      MAX_F77_DIRECTIVE_PREFIX_SIZE-2));

      Inside_F77_Directive = TRUE;
      Max_Line_Length_Outside_F77_Directive = Max_Line_Length;
      Max_Line_Length = 72;
      strncpy(&F77_Directive_Continuation[0], &str[0],
	      TOKEN_string_size(a_token));
      F77_Directive_Continuation[TOKEN_string_size(a_token)] = '&';
      F77_Directive_Continuation[TOKEN_string_size(a_token)+1] = ' ';
      F77_Directive_Continuation[TOKEN_string_size(a_token)+2] = '\0';
      break;


   case SPECIAL_TOKEN:
      Output_Character(ofile, strbuf, strlen, TOKEN_char(a_token));
      last_split_pt = write_buffer_next;
      break;

   case SRCPOS_MAP_TOKEN:
      Is_True(ofile != NULL, ("Cannot source position mapping to file"));
      Output_Srcpos_Map(ofile, TOKEN_srcpos(a_token));
      break;

   case SRCPOS_DIRECTIVE_TOKEN:
      Output_Srcpos_Directive(ofile, strbuf, strlen, TOKEN_srcpos(a_token));
      break;

   default:
      Is_True(FALSE, ("Attempt to write non-existent token"));
      break;
   }
} /* write_token */


static void 
write_separator(FILE        *ofile,  /* NULL when strbuf!=NULL */
		char       **strbuf, /* NULL when ofile!=NULL */
		UINT        *strlen, /* Only relevant for strbuf!=NULL */
		TOKEN_BUFFER buffer, 
		TOKEN_IDX    idx1, 
		TOKEN_IDX    idx2)
{
   BOOL          separate;
   static UINT16 previous_token_kind = SEPARATOR_TOKEN; 
   static char   previous_token_ch = '\0';   

   if (idx1 == NO_TOKEN_IDX || idx2 == NO_TOKEN_IDX)
   {
      previous_token_kind = SEPARATOR_TOKEN;
      previous_token_ch = '\0';
      separate = FALSE;
   }
   else
   {
      TOKEN       *a_token1 = &buffer->tokens[idx1];
      TOKEN       *a_token2 = &buffer->tokens[idx2];
      const UINT16 kind1 = TOKEN_kind(a_token1);
      const UINT16 kind2 = TOKEN_kind(a_token2);
      char         ch1 = (kind1 == SPECIAL_TOKEN? TOKEN_char(a_token1) : 'a');
      char         ch2 = (kind2 == SPECIAL_TOKEN? TOKEN_char(a_token2) : 'a');
      
      if (kind1 == SEPARATOR_TOKEN        ||
	  kind1 == DIRECTIVE_TOKEN        ||
	  kind1 == SRCPOS_DIRECTIVE_TOKEN ||
	  kind1 == SRCPOS_MAP_TOKEN       ||
	  kind2 == SEPARATOR_TOKEN        ||
	  kind2 == DIRECTIVE_TOKEN        ||
	  kind2 == SRCPOS_DIRECTIVE_TOKEN ||
	  kind2 == SRCPOS_MAP_TOKEN)
	 separate = FALSE;
      else if (kind1 == STRING_TOKEN && kind2 == STRING_TOKEN)
	 separate = TRUE;
      else if (kind1 == STRING_TOKEN && kind2 == SPECIAL_TOKEN)
	 separate = (is_binary_or_tertiary_op(ch2) || ch2 == LEFT_BRACE);
      else if (kind1 == SPECIAL_TOKEN && kind2 == STRING_TOKEN)
	 separate = ((is_binary_or_tertiary_op(ch1) &&
		      !(previous_token_kind == SPECIAL_TOKEN &&
			is_unary_op(previous_token_ch, ch1))) ||
		     is_end_grouping(ch1)                     ||
		     is_comma_or_semicolon(ch1));
      else if (kind1 == SPECIAL_TOKEN && kind2 == SPECIAL_TOKEN)
	 separate = (is_comma_or_semicolon(ch1)             ||
		     (is_binary_or_tertiary_op(ch1) && 
		      is_begin_grouping(ch2) &&
		      !is_unary_op(previous_token_ch, ch1)) ||
		     (is_end_grouping(ch1) &&
		      is_binary_or_tertiary_op(ch2))        ||
		     (!is_begin_grouping(ch1) &&
		      is_unary_op(ch1, ch2)));
      else
	 Is_True(FALSE, ("Illegal token to separate"));

      previous_token_kind = kind1;
      previous_token_ch = ch1;
   }
   if (separate)
      Output_Character(ofile, strbuf, strlen, SPACE);
} /* write_separator */

static void 
write_F77_separator(FILE        *ofile,  /* NULL when strbuf!=NULL */
		    char       **strbuf, /* NULL when ofile!=NULL */
		    UINT        *strlen, /* Relevant for strbuf!=NULL */
		    TOKEN_BUFFER buffer, 
		    TOKEN_IDX    idx1, 
		    TOKEN_IDX    idx2)
{
   /* This can be targeted to Fortran requirements whenever necessary.
    * For now, simply call the FREE_FORMAT separator.
    */
   write_separator(ofile, strbuf, strlen, buffer, idx1, idx2);
} /* write_F77_separator */


static TOKEN_IDX
Skip_Srcpos_Map(FILE *srcpos_map_file, TOKEN_BUFFER buf, TOKEN_IDX token_idx)
{
   TOKEN_IDX return_idx;
   
   if (token_idx == NO_TOKEN_IDX ||
       TOKEN_kind(&buf->tokens[token_idx]) != SRCPOS_MAP_TOKEN)
   {
      return_idx = token_idx;
   }
   else /* TOKEN_kind == SRCPOS_MAP_TOKEN */
   {
      write_token(srcpos_map_file, NULL, 0, buf, token_idx);
      return_idx = TOKEN_next(&buf->tokens[token_idx]);
   }
   return return_idx;
} /* Skip_Srcpos_Map */


static TOKEN_IDX
Str_Skip_Srcpos_Map(TOKEN_BUFFER buf, TOKEN_IDX token_idx)
{
   TOKEN_IDX return_idx;
   
   if (token_idx == NO_TOKEN_IDX ||
       TOKEN_kind(&buf->tokens[token_idx]) != SRCPOS_MAP_TOKEN)
   {
      return_idx = token_idx;
   }
   else /* TOKEN_kind == SRCPOS_MAP_TOKEN */
   {
      return_idx = TOKEN_next(&buf->tokens[token_idx]);
   }
   return return_idx;
} /* Str_Skip_Srcpos_Map */

      
static void 
Write_Srcpos_File_Map_Table(FILE *srcpos_map_file)
{
   UINT32      filenum;
   const char *fname;
   const char *dirname;
   USRCPOS     usrcpos;
   INT32       status;

   fprintf(srcpos_map_file, "(SRCPOS-FILEMAP\n");
   for (filenum = 1; filenum <= Max_Srcpos_Map_Filenum; filenum++)
   {
      USRCPOS_filenum(usrcpos) = filenum;
      IR_Srcpos_Filename(USRCPOS_srcpos(usrcpos), &fname, &dirname);
      if (fname != NULL && dirname != NULL)
      {
	 status = fprintf(srcpos_map_file, 
			  " (%u \"%s/%s\")\n",
			  filenum, dirname, fname);
	 Is_True(status >= 0, ("Output error to srcpos mapping file"));
      }
      else if (fname != NULL)
      {
	 status = fprintf(srcpos_map_file, 
			  " (%u \"%s\")\n",
			  filenum, fname);
	 Is_True(status >= 0, ("Output error to srcpos mapping file"));
      }
   } /* for */
   fprintf(srcpos_map_file, ")\n");
} /* Write_Srcpos_File_Map_Table */


/*------------------------- exported routines -------------------------*/
/*---------------------------------------------------------------------*/

void
Initialize_Token_Buffer(FORMAT_KIND output_format)
{
   Output_Format = output_format;
   Max_Line_Length = Default_Max_Line_Length[output_format];

   write_buffer_next = 0;
   last_split_pt = INVALID_SPLIT_PT;

   Current_Output_Col = 1;
   Current_Output_Line = 1;
   Max_Srcpos_Map_Filenum = 0;
} /* Initialize_Token_Buffer */


void
Terminate_Token_Buffer(FILE *srcpos_map_file)
{
   /* Free up all malloced space! */
   free_buffer_list(buffer_free_list);

   /* Emit the file-number to file-name mapping table */
   if (Max_Srcpos_Map_Filenum > 0)
      Write_Srcpos_File_Map_Table(srcpos_map_file);

} /* Terminate_Token_Buffer */


void
Set_Maximum_Linelength(UINT32 max_linelength)
{
   if (max_linelength == 0) 
      Max_Line_Length = Default_Max_Line_Length[Output_Format];
   else
      Max_Line_Length = max_linelength;
} /* Set_Maximum_Linelength */


BOOL
HAS_Maximum_Linelength(void)
{
   return Max_Line_Length==0;
} /* Get_Maximum_Linelength */


UINT32
Get_Maximum_Linelength(void)
{
   return Max_Line_Length;
} /* Get_Maximum_Linelength */


void 
Free_Token_Buffer_Memory(void)
{
   free_buffer_list(buffer_free_list);
} /* free_token_buffer_memory */


TOKEN_BUFFER 
New_Token_Buffer(void)
{
   TOKEN_BUFFER new_buffer;
   
   if (buffer_free_list != NULL)
   {
      new_buffer = buffer_free_list;
      buffer_free_list = new_buffer->next;
   }
   else
   {
      new_buffer = 
	 TB_TYPE_ALLOC_N(struct Token_Buffer, 1);
      new_buffer->chars_allocated = INIT_STRING_BUFFER_SIZE;
      new_buffer->strings = 
	 TB_TYPE_ALLOC_N(char, new_buffer->chars_allocated);
      new_buffer->tokens_allocated = INIT_TOKEN_BUFFER_SIZE;
      new_buffer->tokens = 
	 TB_TYPE_ALLOC_N(TOKEN, new_buffer->tokens_allocated);
   }
   new_buffer->chars_used = 0;
   new_buffer->tokens_used = 0;
   new_buffer->token_list.first = NO_TOKEN_IDX;
   new_buffer->token_list.last = NO_TOKEN_IDX;
   new_buffer->next = NULL;

   return new_buffer;
} /* New_Token_Buffer */


void 
Reclaim_Token_Buffer(TOKEN_BUFFER *tokens)
{
   (*tokens)->next = buffer_free_list;
   buffer_free_list = *tokens;
   *tokens = NULL;
} /* Reclaim_Token_Buffer */


BOOL 
Is_Empty_Token_Buffer(TOKEN_BUFFER tokens)
{
   Is_True(tokens != NULL, ("Invalid TOKEN_BUFFER in Is_Empty_Token_Buffer()"));
   return (tokens->tokens_used == 0);
} /* Is_Empty_Token_Buffer */


BOOL 
Identical_Token_Lists(TOKEN_BUFFER tokens1, 
		      TOKEN_BUFFER tokens2)
{
   BOOL        identical = (tokens1 == NULL && tokens2 == NULL);
   TOKEN_IDX   token_idx;
   TOKEN      *a_token1;
   TOKEN      *a_token2;
   const char *str1, *str2;

   if (!identical)
   {
      identical = (tokens1 != NULL && tokens2 != NULL &&
		   tokens1->tokens_used == tokens2->tokens_used);
   
      for (token_idx = 0; 
	   identical && (token_idx < tokens1->tokens_used); 
	   token_idx++)
      {
	 a_token1 = &tokens1->tokens[token_idx];
	 a_token2 = &tokens2->tokens[token_idx];
	 identical = TOKEN_kind(a_token1) == TOKEN_kind(a_token2);
	 if (identical)
	 {
	    switch (TOKEN_kind(a_token1))
	    {
	    case F77_SEQNO_TOKEN:
	    case STRING_TOKEN:
	    case SEPARATOR_TOKEN:
	    case DIRECTIVE_TOKEN:
	       str2 = TOKEN_BUFFER_get_char_string(tokens1, a_token1);
	       str1 = TOKEN_BUFFER_get_char_string(tokens2, a_token2);
	       identical = 
		  ((TOKEN_string_size(a_token1) == 
		    TOKEN_string_size(a_token2)) &&
		   (strncmp(str1, str2, TOKEN_string_size(a_token1)) == 0));
	       break;

	    case SPECIAL_TOKEN:
	       identical = TOKEN_char(a_token1) == TOKEN_char(a_token2);
	       break;

	    case SRCPOS_MAP_TOKEN:
	    case SRCPOS_DIRECTIVE_TOKEN:
	       identical = TOKEN_srcpos(a_token1) == TOKEN_srcpos(a_token2);
	       break;

	    default:
	       Is_True(FALSE, ("Attempt to access invalid token kind"));
	       break;
	    } /*switch*/
	 } /*if*/
      } /*for*/
   } /*if*/

   return identical;
} /* Identical_Token_Lists */


UINT 
Current_Indentation(void)
{
   return requested_indentation;
} /* Current_Indentation */


void
Set_Current_Indentation(UINT indent)
{
   if (indent > MAX_INDENTATION)
   {
      requested_indentation = (INT32)indent;
      current_indentation = MAX_INDENTATION;
   }
   else
      requested_indentation = current_indentation = (INT32)indent;
} /* Set_Current_Indentation */


void 
Set_Indentation_Step(UINT num_spaces)
{
   if (num_spaces > MAX_INDENTATION_STEP)
      indentation_increment = MAX_INDENTATION_STEP;
   else
      indentation_increment = num_spaces;
} /* Set_Indentation_Step */


void 
Increment_Indentation(void)
{
   requested_indentation += indentation_increment;
   if (requested_indentation > MAX_INDENTATION)
      current_indentation = MAX_INDENTATION;
   else
      current_indentation = requested_indentation;
} /* Increment_Indentation */


void 
Decrement_Indentation(void)
{
   requested_indentation -= indentation_increment;
   if (requested_indentation < current_indentation)
   {
      if (requested_indentation < 0)
	 requested_indentation = current_indentation = 0;
      else
	 current_indentation = requested_indentation;
   }
} /* Decrement_Indentation */


void 
Zero_Indentation(void)
{
    requested_indentation = current_indentation = 0;
} /* Zero_Indentation */


void 
Append_Indented_Newline(TOKEN_BUFFER tokens, UINT num_lines)
{
   TOKEN_SEQUENCE token_list;

   Is_True(tokens != NULL, 
	   ("Invalid TOKEN_BUFFER in Append_Indented_Newline()"));
   token_list.first = indented_newline_token(tokens, num_lines, FALSE, NULL);
   token_list.last = token_list.first;
   append_token_list(tokens, token_list);
}


void 
Append_Token_String(TOKEN_BUFFER tokens, const char *string)
{
   TOKEN_SEQUENCE token_list;

   Is_True(tokens != NULL, 
	   ("Invalid TOKEN_BUFFER in Append_Token_String()"));
   if (string != NULL && string[0] != '\0')
   {
      token_list.first = string_token(tokens, string);
      token_list.last = token_list.first;
      append_token_list(tokens, token_list);
   }
}


void 
Append_Token_Special(TOKEN_BUFFER tokens, char special)
{
   TOKEN_SEQUENCE token_list;

   Is_True(tokens != NULL, 
	   ("Invalid TOKEN_BUFFER in Append_Token_Special()"));
   token_list.first = special_char_token(tokens, special);
   token_list.last = token_list.first;
   append_token_list(tokens, token_list);
}


void 
Append_And_Copy_Token_List(TOKEN_BUFFER result_tokens, 
			   TOKEN_BUFFER copy_tokens)
{
   TOKEN_SEQUENCE token_list;

   Is_True(result_tokens != NULL, 
	   ("Invalid TOKEN_BUFFER in Append_And_Copy_Token_List()"));
   token_list = copy_token_list(result_tokens, copy_tokens);
   if (token_list.first != NO_TOKEN_IDX)
      append_token_list(result_tokens, token_list);
}


void 
Append_And_Reclaim_Token_List(TOKEN_BUFFER result_tokens, 
			      TOKEN_BUFFER *reclaim_tokens)
{
   Append_And_Copy_Token_List(result_tokens, *reclaim_tokens);
   Reclaim_Token_Buffer(reclaim_tokens);
}


void 
Prepend_Indented_Newline(TOKEN_BUFFER tokens, UINT num_lines)
{
   TOKEN_SEQUENCE token_list;

   Is_True(tokens != NULL, 
	   ("Invalid TOKEN_BUFFER in Prepend_Indented_Newline()"));
   token_list.first = indented_newline_token(tokens, num_lines, FALSE, NULL);
   token_list.last = token_list.first;
   prepend_token_list(tokens, token_list);
}


void 
Prepend_Token_String(TOKEN_BUFFER tokens, const char *string)
{
   TOKEN_SEQUENCE token_list;

   Is_True(tokens != NULL, 
	   ("Invalid TOKEN_BUFFER in Prepend_Token_String()"));
   if (string != NULL && string[0] != '\0')
   {
      token_list.first = string_token(tokens, string);
      token_list.last = token_list.first;
      prepend_token_list(tokens, token_list);
   }
}


void 
Prepend_Token_Special(TOKEN_BUFFER tokens, char special)
{
   TOKEN_SEQUENCE token_list;

   Is_True(tokens != NULL, 
	   ("Invalid TOKEN_BUFFER in Prepend_Token_Special()"));
   token_list.first = special_char_token(tokens, special);
   token_list.last = token_list.first;
   prepend_token_list(tokens, token_list);
}


void 
Prepend_And_Copy_Token_List(TOKEN_BUFFER result_tokens, 
			    TOKEN_BUFFER copy_tokens)
{
   TOKEN_SEQUENCE token_list;

   Is_True(result_tokens != NULL, 
	   ("Invalid TOKEN_BUFFER in Prepend_And_Copy_Token_List()"));
   token_list = copy_token_list(result_tokens, copy_tokens);
   if (token_list.first != NO_TOKEN_IDX)
      prepend_token_list(result_tokens, token_list);
}

void 
Prepend_And_Reclaim_Token_List(TOKEN_BUFFER result_tokens, 
			       TOKEN_BUFFER *reclaim_tokens)
{
   Prepend_And_Copy_Token_List(result_tokens, *reclaim_tokens);
   Reclaim_Token_Buffer(reclaim_tokens);
}


void 
Append_F77_Indented_Newline(TOKEN_BUFFER tokens,
			    UINT         num_lines, 
			    const char  *label)
{
   TOKEN_SEQUENCE token_list;

   Is_True(tokens != NULL && 
	   (Output_Format == F77_TAB_FORMAT || 
	    Output_Format == F77_ANSI_FORMAT), 
	   ("Invalid TOKEN_BUFFER in Append_F77_Indented_Newline()"));
   token_list.first = indented_newline_token(tokens, num_lines, FALSE, label);
   token_list.last = token_list.first;
   append_token_list(tokens, token_list);
} /* Append_F77_Indented_Newline */


void 
Prepend_F77_Indented_Newline(TOKEN_BUFFER tokens, 
			    UINT         num_lines, 
			    const char  *label)
{
   TOKEN_SEQUENCE token_list;

   Is_True(tokens != NULL && 
	   (Output_Format == F77_TAB_FORMAT || 
	    Output_Format == F77_ANSI_FORMAT), 
	   ("Invalid TOKEN_BUFFER in Prepend_F77_Indented_Newline()"));
   token_list.first = indented_newline_token(tokens, num_lines, FALSE, label);
   token_list.last = token_list.first;
   prepend_token_list(tokens, token_list);
} /* Prepend_F77_Indented_Newline */


void 
Append_F77_Indented_Continuation(TOKEN_BUFFER tokens)
{
   TOKEN_SEQUENCE token_list;

   Is_True(tokens != NULL && 
	   (Output_Format == F77_TAB_FORMAT || 
	    Output_Format == F77_ANSI_FORMAT), 
	   ("Invalid TOKEN_BUFFER in Append_F77_Indented_Newline()"));
   token_list.first = indented_newline_token(tokens, 1, TRUE, NULL);
   token_list.last = token_list.first;
   append_token_list(tokens, token_list);
} /* Append_F77_Indented_Continuation */


void 
Prepend_F77_Indented_Continuation(TOKEN_BUFFER tokens)
{
   TOKEN_SEQUENCE token_list;

   Is_True(tokens != NULL && 
	   (Output_Format == F77_TAB_FORMAT || 
	    Output_Format == F77_ANSI_FORMAT), 
	   ("Invalid TOKEN_BUFFER in Prepend_F77_Indented_Continuation()"));
   token_list.first = indented_newline_token(tokens, 1, TRUE, NULL);
   token_list.last = token_list.first;
   prepend_token_list(tokens, token_list);
} /* Prepend_F77_Indented_Newline */


void 
Append_F77_Comment_Newline(TOKEN_BUFFER tokens,
			   UINT         num_lines,
			   BOOL         indent_last_line)
{
   TOKEN_SEQUENCE token_list;

   Is_True(tokens != NULL && 
	   (Output_Format == F77_TAB_FORMAT || 
	    Output_Format == F77_ANSI_FORMAT), 
	   ("Invalid TOKEN_BUFFER in Append_F77_Commented_Newline()"));
   token_list.first = 
      F77_comment_line_token(tokens, num_lines, "C", indent_last_line);
   token_list.last = token_list.first;
   append_token_list(tokens, token_list);
} /* Append_F77_Comment_Newline */


void 
Prepend_F77_Comment_Newline(TOKEN_BUFFER tokens, 
			    UINT         num_lines,
			    BOOL         indent_last_line)
{
   TOKEN_SEQUENCE token_list;

   Is_True(tokens != NULL && 
	   (Output_Format == F77_TAB_FORMAT || 
	    Output_Format == F77_ANSI_FORMAT), 
	   ("Invalid TOKEN_BUFFER in Prepend_F77_Commented_Newline()"));
   token_list.first = 
      F77_comment_line_token(tokens, num_lines, "C", indent_last_line);
   token_list.last = token_list.first;
   prepend_token_list(tokens, token_list);
} /* Prepend_F77_Comment_Newline */


void 
Append_F77_Directive_Newline(TOKEN_BUFFER tokens,
			     const char  *directive_prefix)
{
   TOKEN_SEQUENCE token_list;

   Is_True(tokens != NULL && 
	   (Output_Format == F77_TAB_FORMAT || 
	    Output_Format == F77_ANSI_FORMAT), 
	   ("Invalid TOKEN_BUFFER in Append_F77_Directive_Newline()"));
   token_list.first = F77_directive_line_token(tokens, directive_prefix);
   token_list.last = token_list.first;
   append_token_list(tokens, token_list);
} /* Append_F77_Directive_Newline */


void 
Prepend_F77_Directive_Newline(TOKEN_BUFFER tokens, 
			      const char  *directive_prefix)
{
   TOKEN_SEQUENCE token_list;

   Is_True(tokens != NULL && 
	   (Output_Format == F77_TAB_FORMAT || 
	    Output_Format == F77_ANSI_FORMAT), 
	   ("Invalid TOKEN_BUFFER in Prepend_F77_Directive_Newline()"));
   token_list.first = F77_directive_line_token(tokens, directive_prefix);
   token_list.last = token_list.first;
   prepend_token_list(tokens, token_list);
} /* Prepend_F77_Directive_Newline */


void
Append_F77_Sequence_No(TOKEN_BUFFER tokens, 
		       const char  *seq_no)
{
   TOKEN_SEQUENCE token_list;

   Is_True(tokens != NULL && Output_Format == F77_TAB_FORMAT, 
	   ("Invalid TOKEN_BUFFER in Append_F77_Sequence_No()"));
   if (seq_no != NULL && seq_no[0] != '\0')
   {
      token_list.first = f77_seqno_token(tokens, seq_no);
      token_list.last = token_list.first;
      append_token_list(tokens, token_list);
   }
} /* Append_F77_Sequence_No */


void
Prepend_F77_Sequence_No(TOKEN_BUFFER tokens, 
			const char  *seq_no)
{
   TOKEN_SEQUENCE token_list;

   Is_True(tokens != NULL && Output_Format == F77_TAB_FORMAT, 
	   ("Invalid TOKEN_BUFFER in Append_F77_Sequence_No()"));
   if (seq_no != NULL && seq_no[0] != '\0')
   {
      token_list.first = f77_seqno_token(tokens, seq_no);
      token_list.last = token_list.first;
      prepend_token_list(tokens, token_list);
   }
} /* Prepend_F77_Sequence_No */


void 
Append_Srcpos_Map(TOKEN_BUFFER tokens, SRCPOS srcpos)
{
   TOKEN_SEQUENCE token_list;

   Is_True(tokens != NULL, 
	   ("Invalid TOKEN_BUFFER in Append_Srcpos_Map()"));
   token_list.first = Srcpos_Map_Token(tokens, srcpos);
   token_list.last = token_list.first;
   append_token_list(tokens, token_list);
} /* Append_Srcpos_Map */


void 
Append_Srcpos_Directive(TOKEN_BUFFER tokens, SRCPOS srcpos)
{
   TOKEN_SEQUENCE token_list;

   Is_True(tokens != NULL, 
	   ("Invalid TOKEN_BUFFER in Append_Srcpos_Directive()"));
   token_list.first = Srcpos_Directive_Token(tokens, srcpos);
   token_list.last = token_list.first;
   append_token_list(tokens, token_list);
} /* Append_Srcpos_Directive */


void 
Prepend_Srcpos_Map(TOKEN_BUFFER tokens, SRCPOS srcpos)
{
   TOKEN_SEQUENCE token_list;

   Is_True(tokens != NULL,
	   ("Invalid TOKEN_BUFFER in Prepend_Srcpos_Map()"));
   token_list.first = Srcpos_Map_Token(tokens, srcpos);
   token_list.last = token_list.first;
   prepend_token_list(tokens, token_list);
} /* Prepend_Srcpos_Map */


void 
Prepend_Srcpos_Directive(TOKEN_BUFFER tokens, SRCPOS srcpos)
{
   TOKEN_SEQUENCE token_list;

   Is_True(tokens != NULL, 
	   ("Invalid TOKEN_BUFFER in Prepend_Srcpos_Directive()"));
   token_list.first = Srcpos_Directive_Token(tokens, srcpos);
   token_list.last = token_list.first;
   prepend_token_list(tokens, token_list);
} /* Prepend_Srcpos_Directive */


void 
Write_And_Reclaim_Tokens(FILE         *ofile,
			 FILE         *srcpos_map_file, 
			 TOKEN_BUFFER *tokens)
{
   TOKEN_IDX this_token, next_token;
   UINT32    saved_output_col, saved_output_line;

   Is_True(tokens != NULL,
	   ("Invalid TOKEN_BUFFER in Write_And_Reclaim_Tokens()"));
   
   if (srcpos_map_file == NULL)
   {
      /* Save the current source and column numbers and restore them
       * at the end.  These tokens do not apply to any srcpos map.
       */
      saved_output_line = Current_Output_Line;
      saved_output_col = Current_Output_Col;
   }
      
   /* Write tokens to the ofile, and all SRCPOS_MAP_TOKENs to 
    * the srcpos_map_file.
    */
   this_token = Skip_Srcpos_Map(srcpos_map_file, 
				*tokens,
				(*tokens)->token_list.first);
   while (this_token != NO_TOKEN_IDX)
   {
      write_token(ofile, NULL, 0, *tokens, this_token);
      next_token = Skip_Srcpos_Map(srcpos_map_file, 
				   *tokens,
				   TOKEN_next(&(*tokens)->tokens[this_token]));
      if (Output_Format == FREE_FORMAT)
	 write_separator(ofile, NULL, 0, *tokens, this_token, next_token);
      else
	 write_F77_separator(ofile, NULL, 0, *tokens, this_token, next_token);
      this_token = next_token;
   }
   Reclaim_Token_Buffer(tokens);

   /* Write out remaining characters, since the same buffer is used
    * to write to different files! 
    */
   flush_write_buffer(ofile, NULL, 0);

   if (srcpos_map_file == NULL)
   {
      /* Restore the current source and column numbers .
       */
      Current_Output_Line = saved_output_line;
      Current_Output_Col = saved_output_col;
   }
} /* Write_And_Reclaim_Tokens */


void 
Str_Write_And_Reclaim_Tokens(char         *strbuf,
			     UINT32        buflen,
			     TOKEN_BUFFER *tokens)
{
   TOKEN_IDX this_token, next_token;
   UINT32    saved_output_col, saved_output_line;

   Is_True(tokens != NULL,
	   ("Invalid TOKEN_BUFFER in Str_Write_And_Reclaim_Tokens()"));
   
   /* Save the current source and column numbers and restore them
    * at the end.  These tokens do not apply to any srcpos map.
    */
   saved_output_line = Current_Output_Line;
   saved_output_col = Current_Output_Col;

   /* Write tokens to the string, but skip all SRCPOS_MAP_TOKENs */
   this_token = Str_Skip_Srcpos_Map(*tokens, (*tokens)->token_list.first);
   while (this_token != NO_TOKEN_IDX)
   {
      write_token(NULL, &strbuf, &buflen, *tokens, this_token);
      next_token = 
	 Str_Skip_Srcpos_Map(
	    *tokens, TOKEN_next(&(*tokens)->tokens[this_token]));

      if (Output_Format == FREE_FORMAT)
	 write_separator(
	    NULL, &strbuf, &buflen, *tokens, this_token, next_token);
      else
	 write_F77_separator(
	    NULL, &strbuf, &buflen, *tokens, this_token, next_token);

      this_token = next_token;
   }
   Reclaim_Token_Buffer(tokens);

   /* Write out remaining characters and terminate the string. 
    */
   flush_write_buffer(NULL, &strbuf, &buflen);
   Is_True(buflen > 0, ("String buffer overflow!"));
   *strbuf = '\0'; /* Terminate the output string */

   /* Restore the current source and column numbers .
    */
   Current_Output_Line = saved_output_line;
   Current_Output_Col = saved_output_col;
} /* Str_Write_And_Reclaim_Tokens */


void 
Write_String(FILE *ofile, FILE *srcpos_map_file, const char *str)
{
   INT32 str_idx;
   
   if (srcpos_map_file != NULL)
   {
      for (str_idx = 0; str[str_idx] != '\0'; str_idx++)
	 if (str[str_idx] == '\n')
	    Current_Output_Line++;
      Current_Output_Col += str_idx;
   }
   fputs(str, ofile);
} /* Write_String */
