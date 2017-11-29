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
 * Module: w2cf_symtab.c
 * $Revision: 1.11 $
 * $Date: 2006/10/09 20:11:01 $
 * $Author: wychen $
 * $Source: /var/local/cvs/compilers/open64/osprey1.0/be/whirl2c/w2cf_symtab.cxx,v $
 *
 * Revision history:
 *  07-Oct-95 - Original Version
 *
 * Description:
 *
 *   The primary aim of this module is to provide an interface through
 *   which we can maintain a stack of symbol-tables used to disambiguate
 *   names.  To ensure the same naming of the same symbol in two 
 *   independent sessions with this module, the sequence of names 
 *   entered prior to that symbol must be identical for the two
 *   sessions.
 *
 *   The central idea here is that symbols are disambiguated by adding
 *   a numerical suffix to them.  For this reason, numerical suffices
 *   must be removed from names before hashing into the symbol-table.
 *   This way we only need to search through the bucket into which we
 *   land to find an unambigeous name (with added numerical suffix) for
 *   a given symbol.  The names without their disambiguating numerical 
 *   suffices are termed "base_names" in all subsequent comments in
 *   this file.
 *
 *   The datastructure maintained here consists of a stack of hash-
 *   tables, each hash-table bucket contains exactly one element for
 *   each different base_name that hash to that bucket.  Each bucket
 *   entry is a list of entries for symbols with the same base_name,
 *   sorted on the disambiguating numerical suffix.  We impose a hard-
 *   coded limit to the number of characters that may occur in a 
 *   base-name, where this limit may be overridden by a smaller limit
 *   by a user of this module.
 *
 * ====================================================================
 * ====================================================================
 */
#ifdef _KEEP_RCS_ID
static char *rcs_id = "$Source: /var/local/cvs/compilers/open64/osprey1.0/be/whirl2c/w2cf_symtab.cxx,v $ $Revision: 1.11 $";

#endif /* _KEEP_RCS_ID */

#include <ctype.h>
#include <string.h>

#ifdef BUILD_WHIRL2C
#include "whirl2c_common.h"
#define W2FC_Valid_Name(x,b) WHIRL2C_make_valid_c_name(x)
#else /* BUILD_WHIRL2F */
#include "whirl2f_common.h"
#define W2FC_Valid_Name(x,b) WHIRL2F_make_valid_name(x,b)
#endif /* BUILD_WHIRL2C */

#include "mempool.h"
#include "w2cf_symtab.h"


/*------------ Typedefs for the symtab datastructure ------------------*
 *---------------------------------------------------------------------*/

#define MAX_LABEL_NUMBER 99999U

typedef UINT32 W2CF_STR_IDX;
typedef struct W2CF_Stringbuf W2CF_STRINGBUF;
struct W2CF_Stringbuf
{
   W2CF_STR_IDX size;      /* The size of the allocated string buffer */
   W2CF_STR_IDX next_char; /* The next character available for allocation */
   char        *chars;     /* The allocated string buffer */
}; /* W2CF_Stringbuf */
#define W2CF_STRINGBUF_size(s) (s).size
#define W2CF_STRINGBUF_next_char(s) (s).next_char
#define W2CF_STRINGBUF_chars(s) (s).chars


typedef struct W2CF_Symtab W2CF_SYMTAB;
typedef struct W2CF_Symtab_Stack 
{
   W2CF_SYMTAB *top;    /* The top of the stack (deepest nested symtab) */
   W2CF_SYMTAB *bot;    /* The outermost (global) symtab */
} W2CF_SYMTAB_STACK;
#define W2CF_SYMTAB_STACK_top(s) (s).top
#define W2CF_SYMTAB_STACK_bot(s) (s).bot


typedef struct W2CF_Symhdr W2CF_SYMHDR;
struct W2CF_Symtab
{
   UINT32         unique_label; /* Unique label number for this scope */
   INT32          size;     /* Number of elements in allocated hash_tbl */
   W2CF_SYMHDR  **hash_tbl; /* The allocated buckets of symbol headers */
   W2CF_STRINGBUF strbuf;   /* A string buffer for this symbol table */
   W2CF_SYMTAB   *up;       /* The symtab at an outer level of nesting */
   W2CF_SYMTAB   *down;     /* The symtab at a deeper level of nesting */
}; /* W2CF_Symtab */
#define W2CF_SYMTAB_unique_label(s) (s)->unique_label
#define W2CF_SYMTAB_size(s) (s)->size
#define W2CF_SYMTAB_hash_tbl(s) (s)->hash_tbl
#define W2CF_SYMTAB_strbuf(s) (s)->strbuf
#define W2CF_SYMTAB_up(s) (s)->up
#define W2CF_SYMTAB_down(s) (s)->down
#define W2CF_SYMTAB_next(s) (s)->down /* For free-list */


typedef struct W2CF_Symbol W2CF_SYMBOL;
struct W2CF_Symhdr
{
   UINT64       hashval;       /* The hashvalue for the basename */
   INT32        next_symid;    /* The smallest available symid available */
   W2CF_STR_IDX basename;      /* The basename index */
   W2CF_SYMBOL *symbol;        /* A list of symbols with this "name" */
   W2CF_SYMHDR *next;          /* The next symbol header in this bucket */
}; /* W2CF_Symhdr */
#define W2CF_SYMHDR_hashval(s) (s)->hashval
#define W2CF_SYMHDR_next_symid(s) (s)->next_symid
#define W2CF_SYMHDR_basename(s) (s)->basename
#define W2CF_SYMHDR_symbol(s) (s)->symbol
#define W2CF_SYMHDR_next(s) (s)->next


typedef enum W2CF_Symbol_Kind
{
   SYMKIND_UNKNOWN,     
   SYMKIND_UNIQUE,
   SYMKIND_FLD,
   SYMKIND_FLD_POINTEE,
   SYMKIND_TY,
   SYMKIND_ST,
   SYMKIND_ST_POINTEE,
   SYMKIND_TEMPVAR,
   SYMKIND_PREG
} W2CF_SYMBOL_KIND;


struct W2CF_Symbol
{
   W2CF_SYMBOL_KIND kind;  /* The kind of symbol */
   INT32            symid; /* A unique id number for this symbol */
   W2CF_STR_IDX     name;  /* The full symbol name, including symid suffix */
   W2CF_SYMBOL     *next;  /* The next symbol with the same basename */
   union
   {
      FLD_IDX    fld;        /* SYMKIND_FLD */
      FLD_IDX    fld_ptr;    /* SYMKIND_FLD_POINTEE */
      TY_IDX     ty;         /* SYMKIND_TY */
      const ST  *st;         /* SYMKIND_ST */
      const ST  *st_ptr;     /* SYMKIND_ST_POINTEE  */
      INT32      tempvar_id; /* SYMKIND_TEMPVAR */
      struct
      {
	 TY_IDX   preg_ty;

	 PREG_NUM preg_num;
      } preg;           /* SYMKIND_PREG */
   } attr;
}; /* W2CF_Symbol */
#define W2CF_SYMBOL_kind(s) (s)->kind
#define W2CF_SYMBOL_symid(s) (s)->symid
#define W2CF_SYMBOL_name(s) (s)->name
#define W2CF_SYMBOL_next(s) (s)->next
#define W2CF_SYMBOL_attr(s) (s)->attr
#define W2CF_SYMBOL_fld(s) (s)->attr.fld
#define W2CF_SYMBOL_fld_ptr(s) (s)->attr.fld_ptr
#define W2CF_SYMBOL_ty(s) (s)->attr.ty
#define W2CF_SYMBOL_st(s) (s)->attr.st
#define W2CF_SYMBOL_st_ptr(s) (s)->attr.st_ptr
#define W2CF_SYMBOL_tempvar_id(s) (s)->attr.tempvar_id


#define W2CF_SYMBOL_preg_ty(s) (s)->attr.preg.preg_ty
#define W2CF_SYMBOL_preg_num(s) (s)->attr.preg.preg_num

/* Some macros used to interpret symbol symids
 */
#define W2CF_INVALID_SYMID -2
#define W2CF_FIRST_VALID_SYMID -1
#define W2CF_NOSUFFIX_SYMID W2CF_FIRST_VALID_SYMID


static void W2CF_Dump_Symbol(W2CF_SYMBOL *sym,W2CF_SYMTAB *symtab=NULL);
static void W2CF_Dump_Symtab(W2CF_SYMTAB *symtab) ;
static void W2CF_Dump_Symhdr(W2CF_SYMHDR *symhdr,W2CF_SYMTAB *symtab=NULL) ;

/*---------- The symtab state and some generalized macros -------------*
 *---------------------------------------------------------------------*/


/* Define some limits.
 */
#define INITIAL_STRING_BUFFER_SIZE 1024
#define INITIAL_SYMTAB_SIZE 1024


/* Define some helper macros.
 */
#define W2CF_SYMTAB_strbuf_size(s) \
   W2CF_STRINGBUF_size(W2CF_SYMTAB_strbuf(s))
#define W2CF_SYMTAB_strbuf_next(s) \
   W2CF_STRINGBUF_next_char(W2CF_SYMTAB_strbuf(s))
#define W2CF_SYMTAB_strbuf_chars(s) \
   W2CF_STRINGBUF_chars(W2CF_SYMTAB_strbuf(s))

#define W2CF_SYMHDR_basename_string(symtab, symhdr) \
   &W2CF_SYMTAB_strbuf_chars(symtab)[W2CF_SYMHDR_basename(symhdr)]
#define W2CF_SYMBOL_name_string(symtab, symbol) \
   &W2CF_SYMTAB_strbuf_chars(symtab)[W2CF_SYMBOL_name(symbol)]


/* Define the local state.
 */
static W2CF_SYMTAB_STACK  Symtab_Stack = {NULL, NULL};
static W2CF_SYMTAB       *Symtab_Free_List = NULL;
static W2CF_SYMHDR       *Symhdr_Free_List = NULL;
static W2CF_SYMBOL       *Symbol_Free_List = NULL;

static const char W2CF_Anonymous_Fld[] = "fld";
static const char W2CF_Anonymous_Ty[] = "ty";
static const char W2CF_Anonymous_St[] = "anon";
static const char W2CF_Anonymous_Tempvar[] = "_tmp";
static const char W2CF_Anonymous_Preg[] = "reg";


/*----------------- Character string manipulation ---------------------*
 *---------------------------------------------------------------------*/

static W2CF_STR_IDX
W2CF_Symtab_Alloc_Chars(W2CF_SYMTAB *symtab, UINT32 size)
{
   /* Allocate a character string of the given size in the given
    * symbol table.  PRECONDITION: size > 0!
    */
   UINT32 new_bufsize = W2CF_SYMTAB_strbuf_size(symtab);
   UINT32 next_char = W2CF_SYMTAB_strbuf_next(symtab);

   /* First see if we overflow the string buffer, and if so (re)allocate
    * it such that a character string of desired size can be allocated.
    */
   while (new_bufsize < next_char + size)
      new_bufsize += new_bufsize/2 + INITIAL_STRING_BUFFER_SIZE;

   if (W2CF_SYMTAB_strbuf_size(symtab) == 0)
   {
      W2CF_SYMTAB_strbuf_chars(symtab) = TYPE_ALLOC_N(char, new_bufsize);
   }
   else if (new_bufsize > W2CF_SYMTAB_strbuf_size(symtab))
   {
      W2CF_SYMTAB_strbuf_chars(symtab) = 
	 TYPE_REALLOC_N(char, 
			W2CF_SYMTAB_strbuf_chars(symtab), 
			W2CF_SYMTAB_strbuf_size(symtab), 
			new_bufsize);
   }
   W2CF_SYMTAB_strbuf_size(symtab) = new_bufsize;
   W2CF_SYMTAB_strbuf_next(symtab) += size;

   return next_char;
} /* W2CF_Symtab_Alloc_Chars */


static void
W2CF_Get_Basename(const char *original_name, char *basename, INT32 *sym_id)
{
   /* This just cuts the original name down to size, removes any
    * numeric suffix, and puts the resultant name into the basename
    * and the numeric suffix into sym_id.  When there is no numeric 
    * suffix, sym_id is returned as W2CF_INVALID_SYMID.
    */
#define MAX_NUMERIC_SUFFIX_SIZE 8 /* fits into a UINT32 */

   const char *valid_name = W2FC_Valid_Name(original_name,WN2F_F90_pu);
   INT32       name_size, suffix_size;
   UINT32      numeric_suffix = 0;
   UINT32      suffix_exponent;
   
   /* Copy the valid characters into the buffer */
   for (name_size = 0; valid_name[name_size] != '\0'; name_size++)
      basename[name_size] = valid_name[name_size];

   /* Remove the numeric suffix and record its value */
   for (suffix_size = 0, name_size--, suffix_exponent = 1; 
	(name_size >= 0                         && 
	 suffix_size <= MAX_NUMERIC_SUFFIX_SIZE && 
	 isdigit(basename[name_size]));
	suffix_size++, name_size--, suffix_exponent *= 10)
   {
      numeric_suffix += suffix_exponent*(basename[name_size] - '0');
   }
   while (name_size >= 0 && isdigit(basename[name_size]))
      name_size--; /* Remove redundant suffix digits */
   while (basename[name_size+1] == '0' && suffix_size > 0)
   {
      suffix_size--;
      name_size++; /* Append back in zeros preceeding the numeric suffix */
   }
   
   basename[name_size+1] = '\0';
   if (suffix_size > 0)
      *sym_id = numeric_suffix;
   else
      *sym_id = W2CF_INVALID_SYMID;
} /* W2CF_Get_Basename */


static const char *
W2CF_Get_Ftn_St_Name(const ST *st, const char *original_name)
{ 
   const char *extern_name;

#ifdef BUILD_WHIRL2F
   char *name_ptr;

   if (Stab_External_Linkage(st) && 
       !Stab_Is_Based_At_Common_Or_Equivalence(st) &&
       !(ST_sym_class(st) == CLASS_VAR && ST_is_namelist(st)))
   {
      /* Here we deal with a curiosity of the Fortran naming scheme for
       * external names:
       *
       *    + If the name ends with a '_', the name was without the '_'
       *      in the original Fortran source.
       *
       *    + If the name ends without a '_', the name was with a '$'
       *      suffix in the original Fortran source.
       *
       *    + Unless the external name was a namelist variable, then even
       *      though there isn't a trailing '_', don't emit a '$'.
       */
      extern_name = name_ptr =
	 strcpy(Get_Name_Buf_Slot(strlen(original_name)+2), original_name);
      
      /* Get the last character */
      while (name_ptr[1] != '\0')
	 name_ptr++;
       
      /* Correct the name-suffix */
      if (extern_name[0] != '_'  && name_ptr[0] == '_')
      {
	 if (name_ptr[-1] == '_')
	    name_ptr[-1] = '\0';
	 else
	    name_ptr[0] = '\0';
      }
      else if (!WN2F_F90_pu)
      {
	 name_ptr[1] = '$';
	 name_ptr[2] = '\0';
      }
   }
   else /* Not an external variable */
#endif /* BUILD_WHIRL2F */
      extern_name = original_name;

   return extern_name;
   
} /* W2CF_Get_Ftn_St_Name */


/*-------------- Hidden functions for symtab manipulation -------------*
 *---------------------------------------------------------------------*/


static BOOL
W2CF_Avoid_Suffix(W2CF_SYMBOL *symbol )
{
  /* some globals eg: COMMONs, functions,  should get the same name */
  /* if they appear in the global symbol table more than once. They */
  /* may have been created by different PUs                         */

  BOOL avoid = FALSE;

#ifdef BUILD_WHIRL2F
  if (W2CF_SYMBOL_kind(symbol) == SYMKIND_ST) 
    {
      const ST * st = W2CF_SYMBOL_st(symbol);

      if (Stab_Is_Common_Block(st))
	avoid = TRUE;
    }
#endif

  // shouldn't suffixs  be avoided for TY entries???
    if (W2CF_SYMBOL_kind(symbol) == SYMKIND_TY) {
      avoid = TRUE;
    }

  return avoid;
}

static BOOL
W2CF_Identical_Symkinds(W2CF_SYMBOL *sym1, W2CF_SYMBOL *sym2)
{
   BOOL identical = W2CF_SYMBOL_kind(sym1) == W2CF_SYMBOL_kind(sym2);
   
   if (identical/*kinds*/)
   {
      switch (W2CF_SYMBOL_kind(sym1))
      {
      case SYMKIND_UNIQUE:
	 identical = FALSE;
	 break;
      case SYMKIND_FLD:
	 identical = W2CF_SYMBOL_fld(sym1) == W2CF_SYMBOL_fld(sym2);
	 break;
      case SYMKIND_FLD_POINTEE:
	 identical = W2CF_SYMBOL_fld_ptr(sym1) == W2CF_SYMBOL_fld_ptr(sym2);
	 break;
      case SYMKIND_TY:
	 identical = W2CF_SYMBOL_ty(sym1) == W2CF_SYMBOL_ty(sym2);
	 break;
      case SYMKIND_ST:
	 identical = W2CF_SYMBOL_st(sym1) == W2CF_SYMBOL_st(sym2);
	 break;
      case SYMKIND_ST_POINTEE:
	 identical = W2CF_SYMBOL_st_ptr(sym1) == W2CF_SYMBOL_st_ptr(sym2);
	 break;
      case SYMKIND_TEMPVAR:
	 identical = 
	    W2CF_SYMBOL_tempvar_id(sym1) == W2CF_SYMBOL_tempvar_id(sym2);
	 break;
      case SYMKIND_PREG:
	 identical = 
	    (W2CF_SYMBOL_preg_num(sym1) == W2CF_SYMBOL_preg_num(sym2) &&
	     W2CF_SYMBOL_preg_ty(sym1) == W2CF_SYMBOL_preg_ty(sym2));
	 break;
      default:
	 Is_True(FALSE, ("Illegal W2CF_SYMKIND"));
	 break;
      } /*switch*/
   } /*if*/

   return identical;
   
} /* W2CF_Identical_Symkinds */


static void
W2CF_Insert_Symbol(W2CF_SYMHDR *symhdr, 
		   W2CF_SYMBOL *symbol)
{
   /* Insert the symbol into the symbol-list associated with the given
    * symbol header.  The symbols are in order of increasing symids.
    * If the symbol has an INVALID or already used symid, then we create
    * a new one for it and update the symbol accordingly.
    */
   W2CF_SYMBOL *before_sym, *after_sym;
   INT32        symid;
   INT32        next_symid;

   /* The "next_symid" is a lower bound on new symids, so adjust
    * the symid accordingly.
    */
   symid = W2CF_SYMBOL_symid(symbol);
   if (symid < W2CF_SYMHDR_next_symid(symhdr))
      symid = W2CF_SYMHDR_next_symid(symhdr);

   /* Find the insertion point for this new symbol. 
    */
   for ((before_sym = W2CF_SYMHDR_symbol(symhdr), after_sym = NULL);
	before_sym != NULL && W2CF_SYMBOL_symid(before_sym) < symid;
	(after_sym = before_sym, before_sym = W2CF_SYMBOL_next(after_sym)));
   
   if (before_sym != NULL && W2CF_SYMBOL_symid(before_sym) == symid)
   {
      /* Another symbol was found with the same symid, which means
       * the symid cannot have been the next_symid.  Instead of
       * using a user-defined symid, use the next_symid. Note that
       * next_symid is guaranteed not to clash with any symbols
       * in this or any outer symbol-table.
       */
      symid = W2CF_SYMHDR_next_symid(symhdr);
      for ((before_sym = W2CF_SYMHDR_symbol(symhdr), after_sym = NULL);
	   before_sym != NULL && W2CF_SYMBOL_symid(before_sym) < symid;
	   (after_sym = before_sym, before_sym = W2CF_SYMBOL_next(after_sym)));
   }

   /* Update the symid and insert the symbol.
    */
   W2CF_SYMBOL_symid(symbol) = symid;
   W2CF_SYMBOL_next(symbol) = before_sym;
   if (after_sym == NULL)
      W2CF_SYMHDR_symbol(symhdr) = symbol;
   else
      W2CF_SYMBOL_next(after_sym) = symbol;
   
   /* Update next_symid if necessary.  Note that the next
    * next_symid always is larger than or equal to its former
    * value.
    */
   if (symid == W2CF_SYMHDR_next_symid(symhdr))
   {
      next_symid = symid + 1;
      for (before_sym = W2CF_SYMHDR_symbol(symhdr);
	   before_sym != NULL && W2CF_SYMBOL_symid(before_sym) <= next_symid;
	   before_sym = W2CF_SYMBOL_next(before_sym))
      {
	 if (W2CF_SYMBOL_symid(before_sym) == next_symid)
	    next_symid++;
      }
      W2CF_SYMHDR_next_symid(symhdr) = next_symid;
   }

} /* W2CF_Insert_Symbol */


static W2CF_SYMHDR *
W2CF_Search_Symhdr(W2CF_SYMTAB *symtab, const char *basename)
{
   /* Return NULL when there is no symhdr with the given basename
    * in the given symtab.  We make no assumptions about any ordering
    * of symhdrs hashing to the same symbol table entry. 
    */
   const UINT64 hashval = Get_Hash_Value_For_Name(basename);
   const UINT32 hashidx = Name_Hash_Idx(hashval, W2CF_SYMTAB_size(symtab));
   W2CF_SYMHDR *symhdr;
   
   /* Get the matching symhdr if one exists */
   for (symhdr = W2CF_SYMTAB_hash_tbl(symtab)[hashidx];
	(symhdr != NULL && 
	 (hashval != W2CF_SYMHDR_hashval(symhdr) ||
	  strcmp(basename, W2CF_SYMHDR_basename_string(symtab, symhdr)) != 0));
	symhdr = W2CF_SYMHDR_next(symhdr));

   return symhdr;

} /* W2CF_Search_Symhdr */


static W2CF_SYMBOL *
W2CF_Search_Symbol(W2CF_SYMHDR *symhdr, W2CF_SYMBOL *match_symbol)
{
   /* Return NULL when there is no symbol matching the symkind and
    * associated attributes of the given match_symbol.
    */
   W2CF_SYMBOL *symbol;
   
   /* Get the matching symbol, if one exists */
   for (symbol = W2CF_SYMHDR_symbol(symhdr);
	symbol != NULL && !W2CF_Identical_Symkinds(symbol, match_symbol);
	symbol = W2CF_SYMBOL_next(symbol));
   
   return symbol;
} /* W2CF_Search_Symbol */


static W2CF_SYMHDR *
W2CF_Create_Symhdr(W2CF_SYMTAB *symtab, const char *basename)
{
   /* Create a symbol header with the given basename.  The symtab will 
    * be updated to account for the new symbol header.  It is assumed
    * that no other symhdr exists in the symtab with the same basename.
    */
   const UINT64 hashval = Get_Hash_Value_For_Name(basename);
   const UINT32 hashidx = Name_Hash_Idx(hashval, W2CF_SYMTAB_size(symtab));
   W2CF_SYMHDR *symhdr = TYPE_ALLOC_N(W2CF_SYMHDR, 1);
   W2CF_SYMTAB *symtab2;
   W2CF_SYMHDR *symhdr2;
   W2CF_SYMBOL *symbol2;
   
   /* Initiate the new symhdr */
   W2CF_SYMHDR_next(symhdr) = W2CF_SYMTAB_hash_tbl(symtab)[hashidx];
   W2CF_SYMHDR_next_symid(symhdr) = W2CF_FIRST_VALID_SYMID;
   W2CF_SYMHDR_symbol(symhdr) = 0;
   W2CF_SYMHDR_symbol(symhdr) = 0;
   W2CF_SYMHDR_hashval(symhdr) = hashval;
   W2CF_SYMHDR_basename(symhdr) = 
      W2CF_Symtab_Alloc_Chars(symtab, strlen(basename)+1);
   
   (void)strcpy(W2CF_SYMHDR_basename_string(symtab, symhdr), basename);

   /* Insert the symhdr into the hash-table */
   W2CF_SYMTAB_hash_tbl(symtab)[hashidx] = symhdr;

   /* Ensure the the next_symid is outside the range of symids
    * in outer scope symbol-tables.
    */
   symtab2 = W2CF_SYMTAB_up(symtab);
   while (symtab2 != NULL)
   {
      symhdr2 = W2CF_Search_Symhdr(symtab2, basename);
      if (symhdr2 != NULL)
      {
	 symbol2 = W2CF_SYMHDR_symbol(symhdr2);
	 if (symbol2 != NULL)
	 {
	    /* Walk to the last symbol in the list (largest symid) */
	    while (W2CF_SYMBOL_next(symbol2) != NULL) 
	       symbol2 = W2CF_SYMBOL_next(symbol2);

	    /* Set the symbol ids for this symbol to begin beyond
	     * the range symids in any outer scopes!
	     */
	    if (W2CF_SYMBOL_symid(symbol2) >= W2CF_SYMHDR_next_symid(symhdr))
	        W2CF_SYMHDR_next_symid(symhdr) = W2CF_SYMBOL_symid(symbol2)+1;
	 }
      }
      symtab2 = W2CF_SYMTAB_up(symtab2);
   } /* while more symtabs */

   return symhdr;
} /* W2CF_Create_Symhdr */


static W2CF_SYMBOL *
W2CF_Create_Symbol(W2CF_SYMTAB *symtab, 
		   W2CF_SYMHDR *symhdr, 
		   W2CF_SYMBOL *match_symbol)
{
   /* Create a symbol based at the given symhdr and with the symid,
    * symkind and associated attributes as specified by the match_
    * symbol.  If the symid is W2CF_INVALID_SYMID, then invent a 
    * new symid.
    */
   W2CF_SYMBOL *symbol = TYPE_ALLOC_N(W2CF_SYMBOL, 1);
   char        *symname;

   W2CF_SYMBOL_kind(symbol) = W2CF_SYMBOL_kind(match_symbol);
   W2CF_SYMBOL_attr(symbol) = W2CF_SYMBOL_attr(match_symbol);
   W2CF_SYMBOL_symid(symbol) = W2CF_SYMBOL_symid(match_symbol);

   /* Insert the symbol under the given symbol header, possibly 
    * changing the symid if it is already in use or is invalid.
    */
   W2CF_Insert_Symbol(symhdr, symbol);

   /* The symbol name will be constructed from the basename and the
    * the symbol identifier.
    */

   if (W2CF_SYMBOL_symid(symbol) == W2CF_NOSUFFIX_SYMID)
   {
      W2CF_SYMBOL_name(symbol) = W2CF_SYMHDR_basename(symhdr);
   }
   else
   {
     symname = W2CF_SYMHDR_basename_string(symtab, symhdr);

     if (!W2CF_Avoid_Suffix(symbol))
     {
       symname = Get_Name_Buf_Slot(strlen(symname) + 32);
       sprintf(symname, "%s%d",
               W2CF_SYMHDR_basename_string(symtab, symhdr), 
               W2CF_SYMBOL_symid(symbol));
     }
     W2CF_SYMBOL_name(symbol) = W2CF_Symtab_Alloc_Chars(symtab, strlen(symname)+1);
     (void)strcpy(W2CF_SYMBOL_name_string(symtab, symbol), symname);
   }

   //   W2CF_Dump_Symhdr(symhdr,symtab);

   return symbol;

} /* W2CF_Create_Symbol */


static void
W2CF_Get_Symbol(W2CF_SYMTAB **found_symtab,
		W2CF_SYMHDR **found_symhdr,
		W2CF_SYMBOL **found_symbol,
		W2CF_SYMBOL  *match_symbol,
		const char   *basename)
{
   /* This function searches for a symbol with the given basename and
    * symbol attributes and, if found, returns it; otherwise it inserts
    * a new symbol table entry and returns that.  The W2CF_SYMBOL_kind()
    * and W2CF_SYMBOL_attr() must have been set appropriately in 
    * match_symbol.
    */
   W2CF_SYMTAB *symtab;
   W2CF_SYMHDR *symhdr = NULL;
   W2CF_SYMBOL *symbol = NULL;
   W2CF_SYMHDR *top_symhdr;
   
   /* First, check to see if the given symbol is represented somewhere
    * on the top of the symbol-table stack.
    */
   symtab = W2CF_SYMTAB_STACK_top(Symtab_Stack);
   top_symhdr = W2CF_Search_Symhdr(symtab, basename);
   if (top_symhdr != NULL)
      symbol = W2CF_Search_Symbol(top_symhdr, match_symbol);

   /* If the given symbol cannot be found at the top of the stack,
    * then search through outer levels of scope-nesting.
    */
   symtab = W2CF_SYMTAB_up(symtab);
   while (symtab != NULL && symbol == NULL)
   {
      symhdr = W2CF_Search_Symhdr(symtab, basename);
      if (symhdr != NULL)
	 symbol = W2CF_Search_Symbol(symhdr, match_symbol);
      symtab = W2CF_SYMTAB_up(symtab);
   }

   /* Restore the symtab to where the symbol is (or will be) found.
    */
   if (symtab != NULL)
      symtab = W2CF_SYMTAB_down(symtab);            /* Found local symbol */
   else if (symbol != NULL)
      symtab = W2CF_SYMTAB_STACK_bot(Symtab_Stack); /* Found global symbol */
   else
      symtab = W2CF_SYMTAB_STACK_top(Symtab_Stack); /* Did not find symbol */

   /* If no symbol was found, then create one.  Symtab is already set to
    * the most deeply nested one.
    */
   if (symbol == NULL)
   {
      if (top_symhdr == NULL)
	 symhdr = W2CF_Create_Symhdr(symtab, basename);
      else
	 symhdr = top_symhdr;
      symbol = W2CF_Create_Symbol(symtab, symhdr, match_symbol);
   }
   *found_symtab = symtab;
   *found_symhdr = symhdr;
   *found_symbol = symbol;
} /* W2CF_Get_Symbol */


/*------------ Exported functions for symtab manipulation -------------*
 *---------------------------------------------------------------------*/


void
W2CF_Symtab_Push(void)
{
   /* Create a new symbol-table entry and push it onto the stack */
   W2CF_SYMTAB *symtab;
   INT32        hash_idx;

   if (Symtab_Free_List == NULL)
   {
      symtab = TYPE_ALLOC_N(W2CF_SYMTAB, 1);
      W2CF_SYMTAB_strbuf_size(symtab) = 0;
      W2CF_SYMTAB_strbuf_next(symtab) = 0;
      W2CF_SYMTAB_strbuf_chars(symtab) = NULL;
      W2CF_SYMTAB_hash_tbl(symtab) = 
	 TYPE_ALLOC_N(W2CF_SYMHDR*, INITIAL_SYMTAB_SIZE);
      W2CF_SYMTAB_size(symtab) = INITIAL_SYMTAB_SIZE;
      for (hash_idx = 0; hash_idx < INITIAL_SYMTAB_SIZE; hash_idx++)
	 W2CF_SYMTAB_hash_tbl(symtab)[hash_idx] = NULL;
   }
   else
   {
      /* Reuse the symbol-table entry and its string buffer */
      symtab = Symtab_Free_List;
      W2CF_SYMTAB_strbuf_next(symtab) = 0;
      Symtab_Free_List = W2CF_SYMTAB_down(Symtab_Free_List);
   }
   W2CF_SYMTAB_unique_label(symtab) = MAX_LABEL_NUMBER;
   W2CF_SYMTAB_down(symtab) = NULL;
   W2CF_SYMTAB_up(symtab) = W2CF_SYMTAB_STACK_top(Symtab_Stack);
   if (W2CF_SYMTAB_STACK_top(Symtab_Stack) != NULL)
      W2CF_SYMTAB_down(W2CF_SYMTAB_STACK_top(Symtab_Stack)) = symtab;

   W2CF_SYMTAB_STACK_top(Symtab_Stack) = symtab;
   if (W2CF_SYMTAB_STACK_bot(Symtab_Stack) == NULL)
      W2CF_SYMTAB_STACK_bot(Symtab_Stack) = symtab;

} /* W2CF_Symtab_Push */



void
W2CF_Symtab_Pop(void)
{
   /* Put the SYMTAB and its SYMHDRs and the SYMBOLs onto free-lists.
    * The string-buffer and the hash-table belongs to the SYMTAB, 
    * even when the SYMTAB is on the free-list.  To free up all of 
    * this from memory, instead of putting it on free-list, call:
    *
    *   W2CF_Symtab_Terminate()
    */
   W2CF_SYMTAB *symtab = W2CF_SYMTAB_STACK_top(Symtab_Stack);
   W2CF_SYMHDR *symhdr;
   W2CF_SYMBOL *symbol;
   INT32        tbl_idx;

   /* The new top of the stack is the symbol-table above the current
    * one in the scope-nesting.
    */
   W2CF_SYMTAB_STACK_top(Symtab_Stack) = W2CF_SYMTAB_up(symtab);

   /* The new top of the stack has no symbol table at a deeper level
    * of scope-nesting.
    */
   if (W2CF_SYMTAB_STACK_top(Symtab_Stack) != NULL)
      W2CF_SYMTAB_down(W2CF_SYMTAB_STACK_top(Symtab_Stack)) = NULL;
   else
      W2CF_SYMTAB_STACK_bot(Symtab_Stack) = NULL;

   /* Add the popped symtab to the free-list 
    */
   W2CF_SYMTAB_next(symtab) = Symtab_Free_List;
   Symtab_Free_List = symtab;

   /* Next put the symhdrs and the symbols on free lists 
    */
   for (tbl_idx = 0; tbl_idx < W2CF_SYMTAB_size(symtab); tbl_idx++)
   {
      symhdr = W2CF_SYMTAB_hash_tbl(symtab)[tbl_idx];
      while (W2CF_SYMTAB_hash_tbl(symtab)[tbl_idx] != NULL)
      {
	 symhdr = W2CF_SYMTAB_hash_tbl(symtab)[tbl_idx];
	 W2CF_SYMTAB_hash_tbl(symtab)[tbl_idx] = W2CF_SYMHDR_next(symhdr);
	 W2CF_SYMHDR_next(symhdr) = Symhdr_Free_List;
	 Symhdr_Free_List = symhdr;
	 while (W2CF_SYMHDR_symbol(symhdr) != NULL)
	 {
	    symbol = W2CF_SYMHDR_symbol(symhdr);
	    W2CF_SYMHDR_symbol(symhdr) = W2CF_SYMBOL_next(symbol);
	    W2CF_SYMBOL_next(symbol) = Symbol_Free_List;
	    Symbol_Free_List = symbol;
	 } /* while there are no more symbols under this symhdr */
      } /* while there are no more symhdrs in the bucket */
   } /* for each element in the hash-table */
} /* W2CF_Symtab_Pop */


const char * 
W2CF_Symtab_Nameof_St(const ST *st)
{
   const char  *valid_name = NULL ;
   char        *symname;
   INT32        symid;
   W2CF_SYMTAB *symtab;
   W2CF_SYMHDR *symhdr;
   W2CF_SYMBOL *symbol;
   W2CF_SYMBOL  match_symbol;
   
   /* Get the valid version of the name in basename form and its numeric
    * suffix (symid).  Create a name-buffer large enough to hold the
    * name appended to the suffix (hence the "+32").
    */
   
   if (ST_sym_class(st) != CLASS_CONST) 
	   valid_name = W2FC_Valid_Name(ST_name(st),WN2F_F90_pu && !ST_is_temp_var(st));

   if (valid_name == NULL || valid_name[0] == '\0')
   {
      valid_name = W2CF_Anonymous_St;
   }
   else
   {
      valid_name = W2CF_Get_Ftn_St_Name(st, valid_name);
   }

   // don't think there's any reason to rename function names
   if (ST_sym_class(st) == CLASS_FUNC) {
     return valid_name;
   }

   symname = Get_Name_Buf_Slot(strlen(valid_name) + 32);
   W2CF_Get_Basename(valid_name, symname, &symid);

   /* Get the associated symbol entry (with a possibly modified symid).
    */
   W2CF_SYMBOL_symid(&match_symbol) = symid;
   W2CF_SYMBOL_kind(&match_symbol)  = SYMKIND_ST;
   W2CF_SYMBOL_st(&match_symbol)    = st;
   W2CF_Get_Symbol(&symtab, &symhdr, &symbol, &match_symbol, symname);

   //fprintf(stderr, "%s %s\n", W2CF_SYMBOL_name_string(symtab, symbol), ST_name(st));

   char * name =  W2CF_SYMBOL_name_string(symtab, symbol);
   if (!ST_is_temp_var(st) && strcmp(name, ST_name(st)) != 0) {
     // An user variable's name has been mangled by whirl2c.  To avoid conflicts with other user variables,
     // we add a "_w2c_" prefix to it.
     // Note that this is safe because the content of buf has been copied (via Append_Token_String)
     // before Nameof_St is invoked the next time
     static char buf[256];
     BZERO(buf, sizeof(buf));
     strcpy(buf, "_w2c_");
     strncat(buf, W2CF_SYMBOL_name_string(symtab, symbol), 248);
     return buf;
   }
   /* Return the resultant disambiguated name */
   // return W2CF_SYMBOL_name_string(symtab, symbol);
   return name;
   
} /* W2CF_Symtab_Nameof_St */


const char * 
W2CF_Symtab_Nameof_St_Pointee(const ST *st)
{
   const char  *pointee_name;
   char        *symname;
   INT32        symid;
   W2CF_SYMTAB *symtab;
   W2CF_SYMHDR *symhdr;
   W2CF_SYMBOL *symbol;
   W2CF_SYMBOL  match_symbol;
   
   /* Get the valid version of the name in basename form and its numeric
    * suffix (symid).  Create a name-buffer large enough to hold the
    * name appended to the suffix (hence the "+32").
    */
   pointee_name = Concat2_Strings("deref_", W2CF_Symtab_Nameof_St(st));
   symname = Get_Name_Buf_Slot(strlen(pointee_name) + 32);
   W2CF_Get_Basename(pointee_name, symname, &symid);
   
   /* Get the associated symbol entry (with a possibly modified symid).
    */
   W2CF_SYMBOL_symid(&match_symbol)  = symid;
   W2CF_SYMBOL_kind(&match_symbol)   = SYMKIND_ST_POINTEE;
   W2CF_SYMBOL_st_ptr(&match_symbol) = st;
   W2CF_Get_Symbol(&symtab, &symhdr, &symbol, &match_symbol, symname);
   
   /* Return the resultant disambiguated name */
   return W2CF_SYMBOL_name_string(symtab, symbol);
   
} /* W2CF_Symtab_Nameof_St_Pointee */


const char * 
W2CF_Symtab_Nameof_Ty(TY_IDX ty)
{
   const char  *valid_name;
   char        *symname;
   INT32        symid;
   W2CF_SYMTAB *symtab;
   W2CF_SYMHDR *symhdr;
   W2CF_SYMBOL *symbol;
   W2CF_SYMBOL  match_symbol;
   
   /* Get the valid version of the name in basename form and its numeric
    * suffix (symid).  Create a name-buffer large enough to hold the
    * name appended to the suffix (hence the "+32").
    */
   valid_name = W2FC_Valid_Name(TY_name(ty),FALSE);
   if (valid_name == NULL || valid_name[0] == '\0')
   {
      valid_name = W2CF_Anonymous_Ty;
   }
   symname = Get_Name_Buf_Slot(strlen(valid_name) + 32);
   W2CF_Get_Basename(valid_name, symname, &symid);
   
   /* Get the associated symbol entry (with a possibly modified symid).
    */
   W2CF_SYMBOL_symid(&match_symbol) = symid;
   W2CF_SYMBOL_kind(&match_symbol)  = SYMKIND_TY;
   W2CF_SYMBOL_ty(&match_symbol)    = ty;
   W2CF_Get_Symbol(&symtab, &symhdr, &symbol, &match_symbol, symname);

   // for structs, we output the valid_name directly
   //(so different structs will have different names)
   if (TY_kind(ty) == KIND_STRUCT) {
     return valid_name;
   }
   
   /* Return the resultant disambiguated name */
   return W2CF_SYMBOL_name_string(symtab, symbol);
   
} /* W2CF_Symtab_Nameof_Ty */


const char * 
W2CF_Symtab_Nameof_Fld(FLD_HANDLE fld)
{
   const char  *valid_name;
   char        *symname;
   INT32        symid;
   W2CF_SYMTAB *symtab;
   W2CF_SYMHDR *symhdr;
   W2CF_SYMBOL *symbol;
   W2CF_SYMBOL  match_symbol;
   
   /* Get the valid version of the name in basename form and its numeric
    * suffix (symid).  Create a name-buffer large enough to hold the
    * name appended to the suffix (hence the "+32").
    */
   valid_name = W2FC_Valid_Name(FLD_name(fld),FALSE);
   if (valid_name == NULL || valid_name[0] == '\0')
   {
      valid_name = W2CF_Anonymous_Fld;
   }
   symname = Get_Name_Buf_Slot(strlen(valid_name) + 32);
   W2CF_Get_Basename(valid_name, symname, &symid);

     return valid_name;
   // is this really necessary? I think there's no need to rename fields in a struct
   //(How could you possibly get conflicts?)

   /* Get the associated symbol entry (with a possibly modified symid).
    */
   W2CF_SYMBOL_symid(&match_symbol) = symid;
   W2CF_SYMBOL_kind(&match_symbol)  = SYMKIND_FLD;
   W2CF_SYMBOL_fld(&match_symbol)   = fld.Idx ();
   W2CF_Get_Symbol(&symtab, &symhdr, &symbol, &match_symbol, symname);
   
   /* Return the resultant disambiguated name */
   return W2CF_SYMBOL_name_string(symtab, symbol);
   
} /* W2CF_Symtab_Nameof_Fld */


const char *
W2CF_Symtab_Nameof_Fld_Pointee(FLD_HANDLE fld)
{
   const char  *pointee_name;
   char        *symname;
   INT32        symid;
   W2CF_SYMTAB *symtab;
   W2CF_SYMHDR *symhdr;
   W2CF_SYMBOL *symbol;
   W2CF_SYMBOL  match_symbol;
   
   /* Get the valid version of the name in basename form and its numeric
    * suffix (symid).  Create a name-buffer large enough to hold the
    * name appended to the suffix (hence the "+32").
    */
   pointee_name = Concat2_Strings("deref_", W2CF_Symtab_Nameof_Fld(fld));
   symname = Get_Name_Buf_Slot(strlen(pointee_name) + 32);
   W2CF_Get_Basename(pointee_name, symname, &symid);
   
   /* Get the associated symbol entry (with a possibly modified symid).
    */
   W2CF_SYMBOL_symid(&match_symbol)   = symid;
   W2CF_SYMBOL_kind(&match_symbol)    = SYMKIND_FLD_POINTEE;
   W2CF_SYMBOL_fld_ptr(&match_symbol) = fld.Idx ();
   W2CF_Get_Symbol(&symtab, &symhdr, &symbol, &match_symbol, symname);
   
   /* Return the resultant disambiguated name */
   return W2CF_SYMBOL_name_string(symtab, symbol);
   
} /* W2CF_Symtab_Nameof_Fld_Pointee */


const char *
W2CF_Symtab_Nameof_Tempvar(INT32 tempvar_id)
{
   char        *symname;
   char        *tmpvarname;
   INT32        symid;
   W2CF_SYMTAB *symtab;
   W2CF_SYMHDR *symhdr;
   W2CF_SYMBOL *symbol;
   W2CF_SYMBOL  match_symbol;
   
   /* Get the valid version of the name in basename form and its numeric
    * suffix (symid).  Create a name-buffer large enough to hold the
    * name appended to the suffix (hence the "+32").
    */
   tmpvarname = Get_Name_Buf_Slot(strlen(W2CF_Anonymous_Tempvar) + 32);
   sprintf(tmpvarname, "%s%d", W2CF_Anonymous_Tempvar, tempvar_id);
   symname = Get_Name_Buf_Slot(strlen(tmpvarname) + 32);
   W2CF_Get_Basename(tmpvarname, symname, &symid);
   
   /* Get the associated symbol entry (with a possibly modified symid).
    */
   W2CF_SYMBOL_symid(&match_symbol)      = symid;
   W2CF_SYMBOL_kind(&match_symbol)       = SYMKIND_TEMPVAR;
   W2CF_SYMBOL_tempvar_id(&match_symbol) = tempvar_id;
   W2CF_Get_Symbol(&symtab, &symhdr, &symbol, &match_symbol, symname);
   
   /* Return the resultant disambiguated name */
   return W2CF_SYMBOL_name_string(symtab, symbol);
   
} /* W2CF_Symtab_Nameof_Tempvar */


const char *
W2CF_Symtab_Nameof_Preg(const TY_IDX preg_ty, PREG_NUM preg_num)
{
   const char  *valid_name;
   char        *symname;
   INT32        symid;
   W2CF_SYMTAB *symtab;
   W2CF_SYMHDR *symhdr;
   W2CF_SYMBOL *symbol;
   W2CF_SYMBOL  match_symbol;
   
   /* Get the valid version of the name in basename form and its numeric
    * suffix (symid).  Create a name-buffer large enough to hold the
    * name appended to the suffix (hence the "+32").
    */
   valid_name = (preg_num > Last_Dedicated_Preg_Offset) 
      ? Preg_Name(preg_num) : NULL; 
   char buffer[64];
   if (valid_name == NULL && preg_ty == 0) {
     sprintf(buffer, "reg%d", preg_num);
     valid_name = buffer;
   }
   valid_name = W2FC_Valid_Name(valid_name,FALSE);
   if (valid_name == NULL || valid_name[0] == '\0')
   {
      symname = Get_Name_Buf_Slot(strlen(W2CF_Anonymous_Preg) + 32);
      sprintf(symname, "%s%d", W2CF_Anonymous_Preg, preg_num);
      valid_name = symname;
   }

   symname = Get_Name_Buf_Slot(strlen(valid_name) + 32);
   W2CF_Get_Basename(valid_name, symname, &symid);

   /* Get the associated symbol entry (with a possibly modified symid).
    */
   W2CF_SYMBOL_symid(&match_symbol)    = symid;
   W2CF_SYMBOL_kind(&match_symbol)     = SYMKIND_PREG;
   W2CF_SYMBOL_preg_ty(&match_symbol)  = preg_ty;
   W2CF_SYMBOL_preg_num(&match_symbol) = preg_num;
   W2CF_Get_Symbol(&symtab, &symhdr, &symbol, &match_symbol, symname);

   static char buf[256];
   BZERO(buf, sizeof(buf));
   strcpy(buf, "_w2c_");
   strncat(buf, W2CF_SYMBOL_name_string(symtab, symbol), 248);
   
   /* Return the resultant disambiguated name */
   // return W2CF_SYMBOL_name_string(symtab, symbol);
   return buf;
   
} /* W2CF_Symtab_Nameof_Preg */


const char *
W2CF_Symtab_Unique_Name(const char *name)
{
   const char  *valid_name;
   char        *unique_name;
   INT32        symid;
   W2CF_SYMTAB *symtab;
   W2CF_SYMHDR *symhdr;
   W2CF_SYMBOL *symbol;
   W2CF_SYMBOL  match_symbol;
   
   /* Get the valid version of the name in basename form and its numeric
    * suffix (symid).  Create a name-buffer large enough to hold the
    * name appended to the suffix (hence the "+32").
    */
   valid_name = W2FC_Valid_Name(name,WN2F_F90_pu);
   if (valid_name == NULL || valid_name[0] == '\0')
      valid_name = W2CF_Anonymous_St;
   unique_name = Get_Name_Buf_Slot(strlen(valid_name) + 32);
   W2CF_Get_Basename(valid_name, unique_name, &symid);
   
   /* Get the associated symbol entry (with a possibly modified symid).
    */
   W2CF_SYMBOL_symid(&match_symbol)    = symid;
   W2CF_SYMBOL_kind(&match_symbol)     = SYMKIND_UNIQUE;
   W2CF_Get_Symbol(&symtab, &symhdr, &symbol, &match_symbol, unique_name);
   
   /* Return the resultant disambiguated name */
   return W2CF_SYMBOL_name_string(symtab, symbol);
   
} /* W2CF_Symtab_Unique_Name */


UINT32 
W2CF_Symtab_Unique_Label(void)
{
   return W2CF_SYMTAB_unique_label(W2CF_SYMTAB_STACK_top(Symtab_Stack))--;
} /* W2CF_Symtab_Unique_Label */


void
W2CF_Symtab_Free(void)
{
   /* Free up all symbol-table objects allocated, but not currently
    * in use.
    */
   W2CF_SYMTAB *symtab;
   W2CF_SYMHDR *symhdr;
   W2CF_SYMBOL *symbol;

   while (Symtab_Free_List != NULL)
   {
      symtab = Symtab_Free_List;
      Symtab_Free_List = W2CF_SYMTAB_next(symtab);
      FREE(W2CF_SYMTAB_hash_tbl(symtab));
      if (W2CF_SYMTAB_strbuf_chars(symtab) != NULL)
	 FREE(W2CF_SYMTAB_strbuf_chars(symtab));
      FREE(symtab);
   }
   
   while (Symhdr_Free_List != NULL)
   {
      symhdr = Symhdr_Free_List;
      Symhdr_Free_List = W2CF_SYMHDR_next(symhdr);
      FREE(symhdr);
   }
   
   while (Symbol_Free_List != NULL)
   {
      symbol = Symbol_Free_List;
      Symbol_Free_List = W2CF_SYMBOL_next(symbol);
      FREE(symbol);
   }
} /* W2CF_Symtab_Free */


void
W2CF_Symtab_Terminate(void)
{   
   /* Pop all elements off the symbol table stack, thereby putting
    * all SYMTABs, all SYMHDRs, and all SYMBOLs on their respective
    * free-lists.
    */
   while (W2CF_SYMTAB_STACK_top(Symtab_Stack) != NULL)
      W2CF_Symtab_Pop();

   W2CF_Symtab_Free();
} /* W2CF_Symtab_Terminate */


/*---------- Dump routines --------------------------------------------*/
/*---------------------------------------------------------------------*/

static void 
W2CF_Dump_Symhdr(W2CF_SYMHDR *symhdr,W2CF_SYMTAB *symtab)
{

  if (symhdr != 0) 
  {
    printf ("symhdr: 0x%p, hashval=0x%llx, next_symid=%d, next_symhdr=0x%p \n",
            symhdr,
	    W2CF_SYMHDR_hashval(symhdr),
	    W2CF_SYMHDR_next_symid(symhdr),
	    W2CF_SYMHDR_next(symhdr));

    if (symtab != NULL)
      printf ("    basename: %s \n",W2CF_SYMHDR_basename_string(symtab, symhdr)); 
    
    W2CF_SYMBOL *sym = W2CF_SYMHDR_symbol(symhdr);
    while (sym != NULL) 
    {
      W2CF_Dump_Symbol(sym,NULL);
      sym = W2CF_SYMBOL_next(sym);
    }
  }
}

static void 
W2CF_Dump_Symtab(W2CF_SYMTAB *symtab)
{
  if (symtab != 0) 
  {
    printf ("symtab: 0x%p, label=%d, num=%d, hash_tbl=0x%p, up=0x%p, down=0x%p next=0x%p\n",  
            symtab,
	    W2CF_SYMTAB_unique_label(symtab),
            W2CF_SYMTAB_size(symtab),
            W2CF_SYMTAB_hash_tbl(symtab),
	    W2CF_SYMTAB_up(symtab),
	    W2CF_SYMTAB_down(symtab),
	    W2CF_SYMTAB_next(symtab));

    W2CF_STRINGBUF str = W2CF_SYMTAB_strbuf(symtab);

    printf ("         strbuf: size=%d, next=0x%x chars=0x%p\n",
	    W2CF_STRINGBUF_size(str), 
	    W2CF_STRINGBUF_next_char(str),
	    W2CF_STRINGBUF_chars(str));

    char * p = W2CF_STRINGBUF_chars(str);
    char * e = p + W2CF_STRINGBUF_next_char(str);

    while (p < e) 
    {
      printf("            %s\n",p);
      p += strlen(p) + 1;
    }
  }
}

static void 
W2CF_Dump_Symbol(W2CF_SYMBOL *sym,W2CF_SYMTAB *symtab)
{
  if (sym != NULL) 
  {
    const char * hdr = "          ";

    W2CF_STR_IDX nm = W2CF_SYMBOL_name(sym);   

    printf ("    symbol: 0x%p, id=%d, next=0x%p, str_idx 0x%d",
	    sym,
	    W2CF_SYMBOL_symid(sym),
	    W2CF_SYMBOL_next(sym),
	    nm);

    if (symtab != NULL) 
    {
      printf (", name %s",W2CF_SYMBOL_name_string(symtab,sym));
    }
    printf(" \n");

   switch(W2CF_SYMBOL_kind(sym))
   {
   case SYMKIND_UNIQUE:
     printf("%s unique",hdr);
     break;
   case SYMKIND_FLD:
   {
     FLD_HANDLE fld(W2CF_SYMBOL_fld(sym));
     printf("%s FLD 0x%x %s",hdr,W2CF_SYMBOL_fld(sym),FLD_name(fld));
     break; 
   }
   case SYMKIND_TY:
   {
     TY_IDX ty = W2CF_SYMBOL_ty(sym);
     printf("%s TY 0x%x %s",hdr,TY_IDX_index(ty),TY_name(ty));
     break;
   }
   case SYMKIND_ST:
   {
     const ST * st = W2CF_SYMBOL_st(sym);
     printf("%s ST 0x%p %s",hdr,st,ST_name(st));
     break;
   }
   case SYMKIND_PREG:
   {
     TY_IDX  ty = W2CF_SYMBOL_preg_ty(sym);  
     printf("%s PREG num 0x%x ty 0x%x %s",hdr,W2CF_SYMBOL_preg_num(sym),TY_IDX_index(ty),TY_name(ty));
     break;
   }
   default:
     printf("???");
     break;
   }
   printf("\n") ;
  }
}

