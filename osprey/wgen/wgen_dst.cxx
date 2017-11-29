/*
 * Copyright (C) 2009-2010 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

/*
 * Copyright (C) 2007 Pathscale, LLC. All Rights Reserved.
 */

/*
 * Copyright (C) 2006, 2007. QLogic Corporation. All Rights Reserved.
 */

/* 
   Copyright 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
   File modified June 20, 2003 by PathScale, Inc. to update Open64 C/C++ 
   front-ends to GNU 3.2.2 release.
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


/* ====================================================================
 * ====================================================================
 *
 * Module: dst.c
 * $Revision: 1.72 $
 * $Date: 05/12/05 09:00:09-08:00 $
 * $Author: bos@eng-24.pathscale.com $
 * $Revision: 1.72 $
 * $Date: 05/12/05 09:00:09-08:00 $
 * $Author: bos@eng-24.pathscale.com $
 * $Source: kg++fe/SCCS/s.wfe_dst.cxx $
 *
 * Revision history:
 *  01-May-93 - Original Version
 *
 * Description:
 *  Implements generation of dwarf debugging information be means of
 *  ../common/com/dwarf_DST_producer.h.  The information is generated
 *  by starting with the outermost scope (il_header.primary_scope) and
 *  handling nested scopes as they are encountered. 
 *
 *  Note how we mark nodes that have no DST_INFO_IDX associated with
 *  them as visited by means of a static file-scope variable 
 *  (e.g. void_is_visited).  This is necessary when we want to mark
 *  a type of edg node as visited, without a ASSOC_DST field.
 * ====================================================================
 * ====================================================================
 */

static char *source_file = __FILE__;
#ifdef _KEEP_RCS_ID
static char *rcs_id = "$Source: kg++fe/SCCS/s.wfe_dst.cxx $ $Revision: 1.72 $";
#endif /* _KEEP_RCS_ID */

extern "C"{
#include "gspin-wgen-interface.h"
}

#if defined(BUILD_OS_DARWIN)
#include <limits.h>
#else /* defined(BUILD_OS_DARWIN) */
#include <values.h>
#endif /* defined(BUILD_OS_DARWIN) */

#include "defs.h"
#include "glob.h"
#include "config.h"
#include "dwarf_DST_producer.h"
#include "dwarf_DST_dump.h"
#include "file_util.h"  /* From common/util */
#include "srcpos.h"
#include "symtab.h"

#include "wgen_misc.h"
#include "wgen_dst.h"
#include "wgen_expr.h"
#include "wgen_spin_symbol.h"

#include <sys/types.h>
#include <sys/stat.h>   /* For accessing file statistics (stat()) */
#include <sys/param.h>  /* For MAXHOSTNAMELEN */
#include <unistd.h>     /* for gethostname() and getcwd() */
#include <string>
#include <vector>
#include <map>
#ifdef KEY
#include "stamp.h"	/* For INCLUDE_STAMP */
#include "demangle.h"
extern "C" char *cplus_demangle (const char *, int);
#endif


/*
extern FILE *tree_dump_file; //for debugging only
*/

static BOOL dst_initialized = FALSE;


#ifdef KEY
static char *cwd_buffer = NULL;
static char *current_working_dir = NULL;
static char *current_host_dir = NULL;
#else
#define MAX_CWD_CHARS (256 - (MAXHOSTNAMELEN+1))
static char  cwd_buffer[MAX_CWD_CHARS+MAXHOSTNAMELEN+1];
static char *current_working_dir = &cwd_buffer[0];
static char *current_host_dir = &cwd_buffer[0];
#endif


// A file-global current scope is not useful, as with gcc we see
// declarations out of context and must derive right context.
//static DST_INFO_IDX current_scope_idx = DST_INVALID_INIT;

static DST_INFO_IDX comp_unit_idx = DST_INVALID_INIT;	// Compilation unit

static void DST_enter_file (char *, UINT);

static UINT last_dir_num = 0;
static UINT current_dir = 1;
static UINT last_file_num = 0;
UINT current_file = 1;

static DST_INFO_IDX DST_Create_var(ST *var_st, gs_t decl);
static DST_INFO_IDX DST_Create_Parmvar(ST *var_st, gs_t decl);
static DST_INFO_IDX DST_Create_type(ST *typ_decl, gs_t decl);
static void DST_enter_param_vars(gs_t fndecl,DST_INFO_IDX parent,gs_t parameter_list,int is_abstract_root, int is_declaration_only);


static std::vector< std::pair< char *, UINT > > dir_dst_list;
typedef std::map< std::string, DST_INFO_IDX > DST_Type_Map;
static DST_Type_Map basetypes;

#ifdef KEY
static DST_accessibility
get_dwarf_access (gs_t t)
{
  unsigned int flag0, flag1, code;
  flag0 = gs_dwarf_access_flag_0(t);
  flag1 = gs_dwarf_access_flag_1(t);
  code = 2 * flag1 + flag0;
  switch (code) {
    case 1: return DW_ACCESS_public;
    case 2: return DW_ACCESS_protected;
    case 3: return DW_ACCESS_private;
  }
  return 0;
}

// Returns true if type_tree has a DECL_ORIGINAL_TYPE, which implies this
// node is a typedef.
static inline BOOL is_typedef (gs_t type_tree)
{
  gs_t tname = gs_type_name (type_tree);
  return (tname && gs_tree_code (tname) == GS_TYPE_DECL &&
          gs_decl_original_type (tname));
}
#endif

// Use DECL_CONTEXT or TYPE_CONTEXT to get
// the  index of the current applicable scope
// for the thing indicated.
// We already got to TYPE_CONTEXT/DECL_CONTEXT on the input
//   The TYPE_CONTEXT for any sort of type which could have a name or
//    which could have named members (e.g. tagged types in C/C++) will
//    point to the node which represents the scope of the given type, or
//    will be NULL_TREE if the type has "file scope".  For most types, this
//    will point to a BLOCK node or a FUNCTION_DECL node, but it could also
//    point to a FUNCTION_TYPE node (for types whose scope is limited to the
//    formal parameter list of some function type specification) or it
//    could point to a RECORD_TYPE, UNION_TYPE or QUAL_UNION_TYPE node
//    (for C++ "member" types).
//   DECL_CONTEXT points to the node representing the context in which
//    this declaration has its scope.  For FIELD_DECLs, this is the
//    RECORD_TYPE, UNION_TYPE, or QUAL_UNION_TYPE node that the field
//    is a member of.  For VAR_DECL, PARM_DECL, FUNCTION_DECL, LABEL_DECL,
//    and CONST_DECL nodes, this points to either the FUNCTION_DECL for the
//    containing function, the RECORD_TYPE or UNION_TYPE for the containing
//    type, or NULL_TREE if the given decl has "file scope".


static DST_INFO_IDX 
DST_get_context(gs_t intree)
{
    gs_t ltree = intree;
    DST_INFO_IDX l_dst_idx = DST_INVALID_INIT;
    bool continue_looping = true;

    while(ltree && continue_looping) {
	continue_looping = false;
	switch(gs_tree_code(ltree)) {
	case GS_BLOCK:
	    // TODO: unclear when this will happen, as yet
	    DevWarn("Unhandled BLOCK scope of decl/var");
	    return comp_unit_idx;
	case GS_FUNCTION_DECL:
	    // This is a normal case!
	    l_dst_idx = DECL_DST_IDX(ltree);
	    if (DST_IS_NULL(l_dst_idx)) {
		DevWarn("forward reference to subprogram!"
			" assuming global context\n");
		return comp_unit_idx;
	    }
	    return l_dst_idx;
	case GS_RECORD_TYPE:
	case GS_UNION_TYPE:
	case GS_QUAL_UNION_TYPE:
	    ltree = gs_type_context(ltree);
	    continue;
	case GS_FUNCTION_TYPE:
	    DevWarn("Unhandled FUNCTION_TYPE scope of decl/var/type");
	    return comp_unit_idx;
	case GS_REFERENCE_TYPE:
	    // cannot find our context from here
	    return comp_unit_idx;
	case GS_NAMESPACE_DECL:
	case GS_TRANSLATION_UNIT_DECL:
	    // I see these a lot: catching here to avoid default DevWarn
	    ltree = gs_decl_context(ltree);
	    continue;
	default:
	    DevWarn("Unhandled scope of tree code %d", gs_tree_code(ltree));
	    // Best guess for general types and decls
	    if (gs_tree_code_class(ltree) == GS_TCC_DECLARATION) {
		ltree = gs_decl_context(ltree);
		continue_looping = true;
		continue;
	    } else if (gs_tree_code_class(ltree) == GS_TCC_TYPE) {
		ltree = gs_type_context(ltree);
		continue_looping = true;
		continue;
	    }
	    // else: cannot find our context from here
	    return comp_unit_idx;
	}
    }
    // This is the normal case for most things.
    return comp_unit_idx;
}


// get the directory path dst info.
// if already exists, return existing info, else append to list.
static UINT
Get_Dir_Dst_Info (char *name)
{
        std::vector< std::pair < char*, UINT > >::iterator found;
	// assume linear search is okay cause list will be small?
        for (found = dir_dst_list.begin(); 
		found != dir_dst_list.end(); 
		++found)
        {
		if (strcmp ((*found).first, name) == 0) {
			return (*found).second;
		}
	}
	// not found, so append path to dst list
#ifdef KEY
	// We have to create a new home for name because memory
	// will be freed once the caller exits. 
	char *new_name = (char *)malloc((strlen(name)+1)*sizeof(char));
	strcpy(new_name, name);
	name = new_name;
#endif
	dir_dst_list.push_back (std::make_pair (name, ++last_dir_num));
	DST_mk_include_dir (name);
	return last_dir_num;
}

static std::vector< std::pair< char *, UINT > > file_dst_list;

// get the file dst info.
// if already exists, return existing info, else append to list.
static UINT
Get_File_Dst_Info (char *name, UINT dir)
{
#ifdef KEY
        if (name[0] == '\0') // empty file name from g++ 3.4
          return last_file_num;
#endif
        std::vector< std::pair < char*, UINT > >::iterator found;
	// assume linear search is okay cause list will be small?
        for (found = file_dst_list.begin(); 
		found != file_dst_list.end(); 
		++found)
        {
		if (strcmp ((*found).first, name) == 0) {
			return (*found).second;
		}
	}
	// not found, so append file to dst list
#ifdef KEY
	// We have to create a new home for name because memory
	// will be freed once the caller exits. 
	char *new_name = (char *)malloc((strlen(name)+1)*sizeof(char));
	strcpy(new_name, name);
	name = new_name;
#endif
	file_dst_list.push_back (std::make_pair (name, ++last_file_num));
	DST_enter_file (name, dir);
	return last_file_num;
}


// drops path prefix in string 
static char *
drop_path (char *s)
{
        char *tail;
        tail = strrchr (s, '/');
        if (tail == NULL) {
                return s;       // no path prefix 
        } else {
                tail++;         // skip the slash 
                return tail;    // points inside s, not new string! 
        }
}

static void
DST_enter_file (char *file_name, UINT dir)
{
        UINT64 file_size = 0;
        UINT64 fmod_time = 0;
        struct stat fstat;
        if (stat(file_name, &fstat) == 0) {
                // File was found, so set to non-zero values 
                file_size = (UINT64)fstat.st_size;
                fmod_time = (UINT64)fstat.st_mtime;
        }
        DST_mk_file_name(
                file_name,
                dir,
                file_size,
                fmod_time);
}

/* Given the set of options passed into the front-end, string
 * together the ones of interest for debugging and return
 * the resultant string.  The options of interest depends on 
 * the level of debugging.  The caller should free the malloced
 * string once it is no longer needed.
 */
static char *
DST_get_command_line_options(INT32 num_copts, 
			     char *copts[])
{
  INT32	    i, 
            strlength = 0;
  INT32     num_opts = 0;
  char    **selected_opt;
  INT32    *opt_size;
  char     *rtrn, *cp;
  char      ch;
  INT32     record_option;

  selected_opt = (char **)malloc(sizeof(char*) * num_copts);
  opt_size = (INT32 *)malloc(sizeof(INT32) * num_copts);
  
  for (i = 1; i <= num_copts; i++)
  {
     if (copts[i] != NULL && copts[i][0] == '-')
     {
	ch = copts[i][1];  /* Next flag character */
	if (Debug_Level <= 0)
	   /* No debugging */
	   record_option = (ch == 'g' || /* Debugging option */
			    ch == 'O');  /* Optimization level */
	else
	   /* Full debugging */
	   record_option = (ch == 'D' || /* Macro symbol definition */
			    ch == 'g' || /* Debugging option */
			    ch == 'I' || /* Search path for #include files */
			    ch == 'O' || /* Optimization level */
			    ch == 'U');  /* Macro symbol undefined */
	if (record_option)
	{
	   opt_size[num_opts] = strlen(copts[i]) + 1; /* Arg + space/null */
	   selected_opt[num_opts] = copts[i];
	   strlength += opt_size[num_opts];
	   num_opts += 1;
	}
     }
  }
  
  if (strlength == 0)
  {
     rtrn = (char *)calloc(1, 1); /* An empty string */
  }
  else
  {
     rtrn = (char *)malloc(strlength);
     cp = rtrn;

     /* Append the selected options to the string (rtrn) */
     for (i = 0; i < num_opts; i++)
	if (opt_size[i] > 0)
	{
	   cp = strcpy(cp, selected_opt[i]) + opt_size[i];
	   cp[-1] = ' '; /* Space character */
	}
     cp[-1] = '\0'; /* Terminating null character */
  }
  
  free(selected_opt);
  free(opt_size);
  return rtrn;
} /* DST_get_command_line_options */

static char *
Get_Name (gs_t node)
{
  static char buf[64];


  if (node == NULL) {
		buf[0] = 0;
                return buf;
  }
  char *name = buf;
  buf[0] = 0;

#define DANAME(d) ((gs_tree_code_class(d) == GS_TCC_DECLARATION)? \
((gs_decl_name(d))?gs_identifier_pointer(gs_decl_name(d)):"?"):\
 "?2")


  gs_tree_code_class_t tc_class = gs_tree_code_class(node);

  if (tc_class == GS_TCC_DECLARATION)
  {
      if (gs_decl_name (node)) {
        name = gs_identifier_pointer (gs_decl_name (node));
      }
  }
  else if (tc_class == GS_TCC_TYPE)
  {
      if (gs_type_name (node))
        {
          if (gs_tree_code (gs_type_name (node)) == GS_IDENTIFIER_NODE)
            name =  gs_identifier_pointer (gs_type_name (node));
          else if (gs_tree_code (gs_type_name (node)) == GS_TYPE_DECL
                   && gs_decl_name (gs_type_name (node)))
            name = gs_identifier_pointer (gs_decl_name (gs_type_name (node)));
	  
        } 
      else if (gs_tree_code(node) == GS_INTEGER_TYPE) { // bug 11848
	if (gs_type_unsigned(node))
	  strcpy(name, "unsigned ");
	else strcpy(name, "signed ");
	if (strcmp(gs_type_mode(node), "QI") == 0)
	  strcat(name, "char");
	else if (strcmp(gs_type_mode(node), "HI") == 0)
	  strcat(name, "short");
	else if (strcmp(gs_type_mode(node), "SI") == 0)
	  strcat(name, "int");
	else if (strcmp(gs_type_mode(node), "DI") == 0)
	  strcat(name, "long long");
      }else if (gs_tree_code(node) == GS_COMPLEX_TYPE){
            if (strcmp(gs_type_mode(node), "SC") == 0)
               strcpy(name, "complex float");
            else if(strcmp(gs_type_mode(node), "DC") == 0)
               strcpy(name, "complex double");
            //bug 12960: XC(160bits) and TC(256bits) have the same name             
            else if(strcmp(gs_type_mode(node), "XC") == 0 ||
                    strcmp(gs_type_mode(node), "TC") == 0) 
               strcpy(name, "complex long double");            
      }else if(gs_tree_code(node) == GS_REAL_TYPE){
            if (strcmp(gs_type_mode(node), "SF") == 0)
               strcpy(name, "float");
            else if(strcmp(gs_type_mode(node), "DF") == 0)
               strcpy(name, "double");
            //bug 12960: XF(80bits) and TF(128bits) have the same name
            else if(strcmp(gs_type_mode(node), "XF") == 0 ||
                    strcmp(gs_type_mode(node), "TF") == 0 )
               strcpy(name, "long double");
     }
  } else {
  }
  return name;
}

// static data member of class
// Since there will be a global somewhere with this,
// we need to avoid creating ST information. 
// ST information
// the DECL dst record.
//
static void
DST_enter_static_data_mem(gs_t  parent_tree,
                DST_INFO_IDX parent_idx,
                TY_IDX parent_ty_idx,
                gs_t field)
{
	/* cannot be a bit field */
    int isbit = 0;
    DST_INFO_IDX field_idx = DST_INVALID_INIT;

    char * linkage_name = 
  		gs_identifier_pointer(gs_decl_assembler_name (field));
    char * mem_name = Get_Name(field);


    gs_t ftype = gs_tree_type(field);


    // We don't want ST entries created for this class decl.
    ST * st = 0; // Do not do Get here Get_ST(field);
    ST_IDX base =  ST_IDX_ZERO; //  Nothere ST_st_idx(st);


    DST_INFO_IDX fidx = Create_DST_type_For_Tree(ftype,base,parent_ty_idx);

    USRCPOS src;
    // For now, the source location appears bogus
    // (or at least odd) for files other than the base
    // file, so lets leave it out. Temporarily.
    //USRCPOS_srcpos(src) = Get_Srcpos();
#ifdef KEY
    USRCPOS_srcpos(src) = Get_Srcpos();
#else
    USRCPOS_clear(src);
#endif // KEY


    field_idx = DST_mk_variable(
        src,         // srcpos
        mem_name,  // user typed name, not mangled
        fidx,        // user typed type name here (typedef type perhaps).
        0,           // offset (fortran uses non zero )
        base, // underlying type here, not typedef.
        DST_INVALID_IDX,  // abstract origin
        TRUE,          // is_declaration=  decl only
        FALSE,         // is_automatic ?
        FALSE,         // is_external ?
        FALSE  );      // is_artificial ?

    DECL_DST_FIELD_IDX(field) = field_idx;
#ifdef KEY
    // Bug 4829 - do not enter variables without a name.
    if (mem_name != NULL && *mem_name != '\0')
#endif
    DST_append_child(parent_idx,field_idx);

    DST_INFO_IDX varidx = DECL_DST_IDX(field);
    DECL_DST_SPECIFICATION_IDX(field) = field_idx;

#ifdef KEY
    if(mem_name && linkage_name && strcmp(mem_name, linkage_name)) {
       DST_add_linkage_name_to_variable(field_idx, linkage_name);
    }
#else
    // FIXME: need a data version of this.
    //if(mem_name && linkage_name && strcmp(mem_name, linkage_name)) {
    //   DST_add_linkage_name_to_subprogram(field_idx, linkage_name);
    //}
#endif
    DECL_DST_SPECIFICATION_IDX(field) = field_idx;

    return ;
}

// Called for member functions, whether static member funcs
// or non-static member funcs.
// Here we add the member func to the class decl.
// If the function has arguments and the first argument's name
// is "this" it is a non-static member function. Otherwise it
// is a static member function.
// We don't want types added here to be remembered:
// not DST and not ST information. As this is not a definition point.
// (for some functions it can be, but we don't yet handle that)
// 
#ifndef KEY
static void
#else
void
#endif
DST_enter_member_function( gs_t parent_tree,
		DST_INFO_IDX parent_idx,
                TY_IDX parent_ty_idx,
		gs_t fndecl)
{
    USRCPOS src;
    USRCPOS_srcpos(src) = Get_Srcpos();
    DST_INFO_IDX dst = DST_INVALID_INIT;
    DST_INFO_IDX ret_dst = DST_INVALID_IDX;
                                     
    DST_INFO_IDX current_scope_idx = parent_idx;

    

#ifndef KEY
    gs_t resdecl = gs_decl_result(fndecl);
    gs_t restype = 0;
    if( resdecl) {
	   restype = gs_tree_type(resdecl);
    }
#else
    // Another case of Bug 1510 - get the result type from the function type 
    // declaration rather than the result declaration type.
    gs_t restype = 0;
    if (gs_tree_type(fndecl))
      restype = gs_tree_type(gs_tree_type(fndecl));    
#endif
    if(restype) {
	 TY_IDX itx = Get_TY(restype);
	 ret_dst = TYPE_DST_IDX(restype);
    }

    BOOL is_prototyped = TRUE;

#ifdef KEY
    // bug 1736.  Why is the name of the operator omitted?  Let's not do that
    char *basename = gs_identifier_pointer (gs_decl_name (fndecl));
#else
    char * basename = 
	gs_identifier_opname_p(gs_decl_name(fndecl))? 0 :
	gs_identifier_pointer (gs_decl_name (fndecl));
#endif
    char * linkage_name = 
	gs_identifier_pointer(gs_decl_assembler_name (fndecl));
#ifdef KEY
    // Bug 3846
    if (gs_identifier_opname_p (gs_decl_name(fndecl)) && 
	gs_identifier_typename_p (gs_decl_name(fndecl))) {
      basename = cplus_demangle(linkage_name, DMGL_PARAMS | DMGL_ANSI | 
                                              DMGL_TYPES);
      if (basename) {
	basename = strstr(basename, "operator ");
	FmtAssert(basename, ("NYI"));
      } else {
	// Bug 4788 has a weird mangled name which cannot be demangled using
	// c++filt; g++ also generates the same linkage name (mangled name) for
	// that symbol. However, g++ is able to get back the demangled name 
	// somehow. For now, leave this operator name as such.
	// c++filt version 3.3 has this problem but version 3.4 seems to have
	// fixed this. Since, there are lot of changes to 
	// kgnu_common/libiberty/cp-demangle.c, we will wait for the front-end
	// upgrade to 3.4 which will fix this automatically.
	DevWarn(
	 "encountered a mangled name that can not be demangled using c++filt");
	basename = gs_identifier_pointer (gs_decl_name (fndecl));
      }
    }
#endif


    gs_t ftype = gs_tree_type(fndecl);

    int is_abstract_root = 0; // Simply a decl here, not abs. root.

    // is_declaration TRUE  as this is function declared in class,
    // not a definition or abstract root.
    TY_IDX base =  Get_TY(ftype);


    BOOL is_external = TRUE;
    DST_virtuality  virtuality = 
		 gs_decl_pure_virtual_p(fndecl)?
			 DW_VIRTUALITY_pure_virtual
		 : gs_decl_virtual_p(fndecl)?
			DW_VIRTUALITY_virtual : DW_VIRTUALITY_none;
    // FIX virtuality.

    DST_inline inlin = 0; 
		//DECL_PENDING_INLINE_P(fndecl)?
		//	DW_INL_inlined: DW_INL_not_inlined;

    
    //FIX vtable elem loc
    DST_vtable_elem_location vtable_elem_location =  0;

    dst = DST_mk_subprogram(
        src,			// srcpos
        basename,
        ret_dst,        	// return type
        DST_INVALID_IDX,        // Index to alias for weak is set later
        0,              // index to fe routine for st_idx
        inlin,                  // dwarf inline code.
        virtuality,     	// applies to C++, dwarf virt code
        vtable_elem_location,   // vtable_elem_location (vtable slot #
				// as emitted in MIPS, something else
				// by gcc for ia64 )
        TRUE,         // is_declaration
        is_prototyped,           // always true for C++
#ifdef KEY
        gs_decl_artificial (fndecl),
#endif
        is_external );  // is_external? (has external linkage)

    // producer routines thinks we will set pc to fe ptr initially
    DST_RESET_assoc_fe (DST_INFO_flag(DST_INFO_IDX_TO_PTR(dst)));

    DST_append_child (current_scope_idx, dst);



    DECL_DST_FIELD_IDX(fndecl) = dst;

    // Now we create the argument info itself, relying
    // on the is_prototyped flag above to let us know if
    // we really should do this.
    if(is_prototyped) {
       gs_t parms = gs_decl_arguments(fndecl);
       if(!parms) {
          // no arguments: int y(); for example in C++.
       } else {
	   // This kills be with mem func. FIX
	   DST_enter_param_vars(fndecl, 
		dst,
		parms,
		is_abstract_root,
		/* is_declaration_only */ 1);
       }

    }
    if(basename && linkage_name && strcmp(basename, linkage_name)) {
       DST_add_linkage_name_to_subprogram(dst, linkage_name);
    }
    DECL_DST_SPECIFICATION_IDX(fndecl) = dst;
}

        
static void
DST_enter_normal_field(gs_t  parent_tree,
		DST_INFO_IDX parent_idx,
		TY_IDX parent_ty_idx,
		gs_t field)
{
    char isbit = 0; 
    if ( ! gs_decl_bit_field(field)
#ifdef KEY // bug 10478, 11590: from kgccfe
           && gs_decl_size(field)
#endif
           && gs_get_integer_value(gs_decl_size(field)) > 0
           && gs_get_integer_value(gs_decl_size(field))
           != (TY_size(Get_TY(gs_tree_type(field)))
                                        * BITSPERBYTE) )
    {
           // for some reason gnu doesn't set bit field
           // when have bit-field of standard size
           // (e.g. int f: 16;).  But we need it set
           // so we know how to pack it, because
           // otherwise the field type is wrong.
	   // already warned
           //DevWarn(field size %d does not match type size %d,
           //                            gs_decl_size(field),
           ////                           TY_size(Get_TY(gs_tree_type(field)))
           //                                   * BITSPERBYTE );

	   isbit = 1;
    }
    if (gs_decl_bit_field(field)) {
	   isbit = 1;
    }
    DST_INFO_IDX field_idx = DST_INVALID_INIT;
    char *field_name = Get_Name((field));

#ifdef KEY
    // bug 1718
    if ((field_name == NULL || field_name[0] == '\0' ) &&
	/* bug 3847 - fields that are anonymous unions should be emitted */ 
	gs_tree_code(gs_tree_type(field)) != GS_UNION_TYPE) {
        return ;
    }
#endif

    gs_t ftype = gs_tree_type(field);


    TY_IDX base = Get_TY(ftype);

    DST_INFO_IDX fidx = Create_DST_type_For_Tree(ftype,base,parent_ty_idx);

    USRCPOS src;
    // For now, the source location appears bogus
    // (or at least odd) for files other than the base
    // file, so lets leave it out. Temporarily.
    //USRCPOS_srcpos(src) = Get_Srcpos();
#ifdef KEY
    USRCPOS_srcpos(src) = Get_Srcpos();
#else
    USRCPOS_clear(src);
#endif // KEY

    INT bitoff = gs_get_integer_value(gs_decl_field_bit_offset(field));
#ifndef KEY
INT fld_offset_bytes = bitoff / BITSPERBYTE;
#else
// Bug 1271 
INT fld_offset_bytes = gs_get_integer_value(gs_decl_field_offset(field)) + 
  bitoff / BITSPERBYTE; 
#endif 
 gs_t type_size = gs_type_size(ftype);
UINT align = gs_type_align(ftype)/BITSPERBYTE;
    INT tsize;
    if (type_size == NULL) {
#ifndef KEY //bug 10478, 11590: from kgccfe
          // incomplete structs have 0 size
          Fail_FmtAssertion("DST_enter_normal_field: type_size NULL ");
#endif
          tsize = 0;
    }
    else {
          if (gs_tree_code(type_size) != GS_INTEGER_CST) {
            if (gs_tree_code(type_size) == GS_ARRAY_TYPE) {
              tsize = 0;
            }
            else {
              tsize = -1;
            }
          }
          else
            tsize = gs_get_integer_value(type_size) / BITSPERBYTE;
    }

    if(isbit == 0) {
        //  currentcontainer = -1 ;                    // invalidate bitfield calculation
#ifndef KEY
	  field_idx = DST_mk_member(
		src,
		field_name,
		fidx, // field type	
		fld_offset_bytes, // field offset in bytes
			0, // no container (size zero)
			0, // bit_offset= no offset into container
		        0, // bit_size= no bitfield size
	       FALSE, // is_bitfield= not a bitfield
	       FALSE, // is_static= not a static member
	       FALSE, // is_declaration= 
	       FALSE); // is_artificial = no
#else
	  int accessibility = get_dwarf_access(field);
	  field_idx = DST_mk_member(
		src,
		field_name,
		fidx, // field type	
		fld_offset_bytes, // field offset in bytes
			0, // no container (size zero)
			0, // bit_offset= no offset into container
		        0, // bit_size= no bitfield size
	       FALSE, // is_bitfield= not a bitfield
	       FALSE, // is_static= not a static member
	       FALSE, // is_declaration= 
	       FALSE, // is_artificial = no
	       accessibility); // accessibility = public/private/protected
#endif
			
    } else {
	  if(tsize == 0) {
	   Fail_FmtAssertion("bit field type size 0!");
	   return;
	  }
	  UINT container_off = fld_offset_bytes - (fld_offset_bytes%align);

//***********************************************************
//Bug 8890 : modify the calculation for DW_AT_bit_offset
//        (1) claculate offset into the container
//        (2) adjust for little endian,
//***********************************************************
#ifndef KEY
          UINT into_cont_off = bitoff - (container_off*BITSPERBYTE);
#else
          UINT into_cont_off = bitoff - ((container_off%16)*BITSPERBYTE);
          // (1) yes, we mod 16 here because bitoff will wrap around at 16 bytes
          //     this is essential set the new base for bitoff

          // (2) adjust for little endian
          if (Target_Byte_Sex != BIG_ENDIAN) {
             into_cont_off =  tsize*BITSPERBYTE - into_cont_off;   // reset current offset
             into_cont_off -= gs_get_integer_value (gs_decl_size(field)); // start at MSB
            }
#endif

#ifndef KEY
	  field_idx = DST_mk_member(
			src,
			field_name,
			fidx	,      // field type	
                        fld_offset_bytes,    // container offset in bytes
			tsize,         // container size, bytes
                        into_cont_off, // offset into 
					// container, bits

                        gs_get_integer_value(gs_decl_size(field)), // bitfield size
                        TRUE, // a bitfield
                        FALSE, // not a static member
                        FALSE, // Only TRUE for C++?
                        FALSE); // artificial (no)
#else
	  int accessibility = get_dwarf_access(field);
	  field_idx = DST_mk_member(
			src,
			field_name,
			fidx	,      // field type	
                        container_off,    // container offset in bytes
			tsize,         // container size, bytes
                        into_cont_off, // offset into 
					// container, bits

                        gs_get_integer_value(gs_decl_size(field)), // bitfield size
                        TRUE, // a bitfield
                        FALSE, // not a static member
                        FALSE, // Only TRUE for C++?
                        FALSE, // artificial (no)
			accessibility); // accessibility = public/private/protected
#endif
    }
    DST_append_child(parent_idx,field_idx);


	
    return ;
}
static void
DST_enter_struct_union_members(gs_t parent_tree, 
	DST_INFO_IDX parent_idx  )
{ 
    DST_INFO_IDX dst_idx = DST_INVALID_INIT;

    TY_IDX parent_ty_idx = Get_TY(parent_tree);
    
    //if(TREE_CODE_CLASS(TREE_CODE(parent_tree)) != 'd') {
     //   DevWarn("DST_enter_struct_union_members input not 't' but %c",
      //          TREE_CODE_CLASS(TREE_CODE(parent_tree)));
   // }

    gs_t field = gs_type_fields(parent_tree);

//    int currentoffset = 0 ;
//    int currentcontainer = -1 ;
                                                                                                                      
    for( ; field ; field = gs_tree_chain(field) )
    { 
	if(gs_tree_code(field) == GS_FIELD_DECL) {
	   DST_enter_normal_field( parent_tree,parent_idx,
		parent_ty_idx,field);
	} else if(gs_tree_code(field) == GS_VAR_DECL) {
		// Here create static class data mem decls
		// These cannot be definitions.
	   DST_enter_static_data_mem( parent_tree,parent_idx,
		parent_ty_idx,field);
        } else if (gs_tree_code(field) == GS_FUNCTION_DECL) {
		// Cannot happen.
		DevWarn("Impossible FUNCTION DECL member of class!\n");
	}  else {
	  // RECORD_TYPE is class itself, apparently,
	  // and can appear here.
        }
    }

#ifdef KEY
    // Bug 3533 - Expand all member functions of classes inside ::std namespace
    // here (I don't know how to get the member functions of the classes
    // contained in ::std namespace from gxx_emitted_decl in wfe_decl.cxx).
    // Bug 10483 and 13050: Check for NULL context.
    // Bug 3533: Use type context, not decl context.
    gs_t context = gs_type_context(parent_tree);
    if (context == NULL ||
	gs_tree_code(context) != GS_TYPE_DECL ||
	!gs_decl_context(context) ||
	gs_tree_code(gs_decl_context(context)) != GS_NAMESPACE_DECL ||
	!gs_decl_name(gs_decl_context(context)))
      return;
#endif
    // member functions
    gs_t methods = gs_type_methods(parent_tree);
    for  ( ; methods != NULL; methods = gs_tree_chain(methods)) {
	if(gs_tree_code(methods) == GS_FUNCTION_DECL ) {
        // g++ seems to put the artificial ones in the output too for some reason
        // in particular, operator= is there.  We do want to omit the __base_ctor stuff
        // though
#ifndef KEY
	   if ( gs_decl_artificial(methods)) {
#else
           if (gs_identifier_ctor_or_dtor_p(gs_decl_name(methods))) {
#endif
	     // compiler generated methods are not interesting.
	     // We want only ones user coded.
	     continue;	   
	   } else {

	     DST_enter_member_function( parent_tree,parent_idx,
		parent_ty_idx,methods);
	   }
	}
    }

    return;
}


// We have a struct/union. Create a DST record
// and enter it.
static DST_INFO_IDX
DST_enter_struct_union(gs_t type_tree, TY_IDX ttidx  , TY_IDX idx, 
		INT tsize)
{ 
#ifdef KEY
    // Get the unqualified gcc type.  Bug 3969.
    if (lang_cplus) // bug 10483
      type_tree = gs_type_main_variant(type_tree);
#endif

    DST_INFO_IDX dst_idx  = TYPE_DST_IDX(type_tree);

    DST_INFO_IDX current_scope_idx;

#ifdef KEY
    // Want the immediately enclosing scope.  Bug 4168.  (Do this for other
    // types besides records and unions too?  For now, limit to nested
    // records/unions.)
    if (gs_type_context(type_tree) &&
	(gs_tree_code(type_tree) == GS_RECORD_TYPE ||
	 gs_tree_code(type_tree) == GS_UNION_TYPE) &&
	(gs_tree_code(gs_type_context(type_tree)) == GS_RECORD_TYPE ||
	 gs_tree_code(gs_type_context(type_tree)) == GS_UNION_TYPE)) {
      gs_t type_context = gs_type_context(type_tree);
      current_scope_idx = TYPE_DST_IDX(type_context);
      if (DST_IS_NULL(current_scope_idx)) {
	// TODO: Using TY_IDX_ZERO as the 3rd arg is valid only if type is not
	// being forward declared.  Verify if that's correct.
	Create_DST_type_For_Tree(type_context, TYPE_TY_IDX(type_context),
				 TY_IDX_ZERO);
	current_scope_idx = TYPE_DST_IDX(type_context);
      }
      Is_True(!DST_IS_NULL(current_scope_idx),
	      ("DST_enter_struct_union: invalid current scope index\n"));
    } else
#endif
    current_scope_idx =
         DST_get_context(gs_type_context(type_tree));

    if(DST_IS_NULL(dst_idx)) {

	// not yet created, so create it


	// Deal with scope here (FIX)
        // in case scope of decl is different from scope of ref
	//
        USRCPOS src;
        // For now, the source location appears bogus
        // (or at least odd) for files other than the base
        // file, so lets leave it out. Temporarily.
        //USRCPOS_srcpos(src) = Get_Srcpos();
#ifdef KEY
    USRCPOS_srcpos(src) = Get_Srcpos();
#else
        USRCPOS_clear(src);
#endif // KEY

	char *name = Get_Name(type_tree);

#ifdef KEY
        // bug 1718
        if (name == NULL || name[0] == '\0') {
            return dst_idx ;
        }
#endif

	if(gs_tree_code(type_tree) == GS_RECORD_TYPE) {
	   dst_idx = DST_mk_structure_type(src,
		  name , // struct tag name
		  tsize,
		  DST_INVALID_IDX, // not inlined
		   gs_type_fields(type_tree)== 0   // 1 if incomplete
		   );
	} else if (gs_tree_code(type_tree) == GS_UNION_TYPE) {
	   dst_idx = DST_mk_union_type(src,
		  name  , // union tag name
		  tsize,
		  DST_INVALID_IDX, // not inlined
		   gs_type_fields(type_tree)== 0   // arg 1 if incomplete
		   );
	} else {
	  // no DST_enter_struct_union_members(type_tree,dst_idx);
          // leave as DST_IS_NULL
          return dst_idx;
	}
	DST_append_child(current_scope_idx,dst_idx);

	// set this now so we will not infinite loop
	// if this has ptr to itself inside.
        TYPE_DST_IDX(type_tree) = dst_idx;


  	// Do the base classes
        INT32 offset = 0;
        INT32 anonymous_fields = 0;
	gs_t type_binfo, basetypes;
        if ((type_binfo = gs_type_binfo(type_tree)) != NULL &&
	     (basetypes = gs_binfo_base_binfos(type_binfo)) != NULL) {
	  gs_t list;
	  for (list = basetypes; gs_code(list) != EMPTY; 
	       list = gs_operand(list, 1)) {
                    gs_t binfo = gs_operand(list, 0);

                    gs_t basetype = gs_binfo_type(binfo);

		    int virtuality = DW_VIRTUALITY_none;
		    if(gs_binfo_virtual_p(binfo)) {
			// Don't know how to tell if
			// it is pure_virtual.  FIX
			virtuality = DW_VIRTUALITY_virtual;
		    }
#ifdef KEY
		    // For inheritance, accessibility is private unless
		    // otherwise mentioned - bug 3041.
		    int accessibility = get_dwarf_access(binfo);
		    if (accessibility != DW_ACCESS_public &&
			accessibility != DW_ACCESS_protected) {
		      DW_ACCESS_private;
		    }
#endif
                    offset = Roundup (offset,
                                    gs_type_align(basetype) / BITSPERBYTE);

		    TY_IDX itx =  TYPE_TY_IDX(basetype);
		    DST_INFO_IDX bcidx =
                       Create_DST_type_For_Tree (basetype,itx, idx);

#ifndef KEY
		    // There is no way to pass in DW_ACCESS_* here.
		    // That is am omission.  FIX
		    DST_INFO_IDX inhx = 
		      DST_mk_inheritance(src,
				bcidx,
			        virtuality,
				offset);
#else
		    DST_INFO_IDX inhx = 
		      DST_mk_inheritance(src,
				bcidx,
			        virtuality,
				// Bug 1737 - handle virtual base classes
				virtuality ?  //bug 11555: gs_tree_int_cst_low -> gs_get_integer_value
				    -((int)gs_get_integer_value(gs_binfo_vptr_field(binfo))):
				    offset, 
				accessibility);
#endif

		    DST_append_child(dst_idx,inhx);
//---------------------------------------------------------------
//bug 12948: advance "offset" only when non-empty and non-virtual
//---------------------------------------------------------------
#ifdef KEY
                    if (!is_empty_base_class(basetype) &&
#else
                    if (!is_empty_base_class(basetype) ||
#endif
                        !gs_binfo_virtual_p(binfo)) {

                      //FLD_Init (fld, Save_Str(Get_Name(0)),
                       //         Get_TY(basetype) , offset);
                      offset += Type_Size_Without_Vbases (basetype);
                    }

          }
        }


	// now can do the members of our type.
	DST_enter_struct_union_members(type_tree,dst_idx);

    }

    return  dst_idx;
}


// We have a enum. 
// and enter it.
static DST_INFO_IDX
DST_enter_enum(gs_t type_tree, TY_IDX ttidx  , TY_IDX idx, 
		INT tsize)
{ 
   DST_INFO_IDX dst_idx = 
       TYPE_DST_IDX(type_tree);

   if(gs_tree_code_class(type_tree) != GS_TCC_TYPE) {
        DevWarn("DST_enter_enum input not TCC_TYPE but %c",
                gs_tree_code_class(type_tree));
   }
   DST_INFO_IDX current_scope_idx =
        DST_get_context(gs_type_context(type_tree));


   if(DST_IS_NULL(dst_idx)) {
      DST_INFO_IDX t_dst_idx = DST_INVALID_INIT;
      USRCPOS src;
      // For now, the source location appears bogus
      // (or at least odd) for files other than the base
      // file, so lets leave it out. Temporarily.
      //USRCPOS_srcpos(src) = Get_Srcpos();
#ifdef KEY
    USRCPOS_srcpos(src) = Get_Srcpos();
#else
      USRCPOS_clear(src);
#endif // KEY
      char *name1 = Get_Name(type_tree);
      gs_t enum_entry = gs_type_values(type_tree);
      DST_size_t e_tsize =  tsize;
      t_dst_idx = DST_mk_enumeration_type( src,
                           name1,
                           e_tsize, // Type size.
                           DST_INVALID_IDX, // Not inlined.
                           (enum_entry==NULL)); // Pass non-zero 
						// if incomplete.
      DST_append_child(current_scope_idx,t_dst_idx);

      TYPE_DST_IDX(type_tree) = t_dst_idx;


      DST_CONST_VALUE enumerator;
      if(tsize == 8) {
	   DST_CONST_VALUE_form(enumerator) =  DST_FORM_DATA8;
      } else if (tsize == 4) {
	   DST_CONST_VALUE_form(enumerator) =  DST_FORM_DATA4;
      } else {
	   // ???
	   DST_CONST_VALUE_form(enumerator) =  DST_FORM_DATA4;
	   DevWarn("Unexpected type size  %d in enumerator",
		(int)tsize);
      }

      for( ; enum_entry; enum_entry = gs_tree_chain(enum_entry) ) {
         USRCPOS src;
         // For now, the source location appears bogus
         // (or at least odd) for files other than the base
         // file, so lets leave it out. Temporarily.
         //USRCPOS_srcpos(src) = Get_Srcpos();
#ifdef KEY
    USRCPOS_srcpos(src) = Get_Srcpos();
#else
         USRCPOS_clear(src);
#endif // KEY
         char *name2 = 
		  gs_identifier_pointer(gs_tree_purpose(enum_entry));

         if (tsize == 8) {
	   DST_CONST_VALUE_form_data8(enumerator) = 
	      gs_get_integer_value(gs_tree_value(enum_entry));
         } else {
	   DST_CONST_VALUE_form_data4(enumerator) = 
		gs_get_integer_value(gs_tree_value(enum_entry));
         }

	 DST_INFO_IDX ed = DST_mk_enumerator(src,
				name2,
				enumerator);
	 DST_append_child(t_dst_idx,ed);
	
      }


#ifdef KEY
      // Bug 1334 
      // set dst_idx (was NULL) => the reason we do all that we do
      dst_idx = t_dst_idx;
#endif
    }
    return dst_idx;
}


#ifdef KEY
typedef struct type_trans {
	DST_size_t size;
	char *name;
	DST_ATE_encoding encoding;
} type_trans;

static DST_INFO_IDX base_types[MTYPE_LAST+5] =  
{
DST_INVALID_INIT,DST_INVALID_INIT,DST_INVALID_INIT,DST_INVALID_INIT,	
DST_INVALID_INIT,DST_INVALID_INIT,DST_INVALID_INIT,DST_INVALID_INIT,	
DST_INVALID_INIT,DST_INVALID_INIT,DST_INVALID_INIT,DST_INVALID_INIT,	
DST_INVALID_INIT,DST_INVALID_INIT,DST_INVALID_INIT,DST_INVALID_INIT,
DST_INVALID_INIT,DST_INVALID_INIT,DST_INVALID_INIT,DST_INVALID_INIT,
DST_INVALID_INIT,DST_INVALID_INIT,DST_INVALID_INIT,DST_INVALID_INIT,
DST_INVALID_INIT
} ;
	
static type_trans ate_types[] = {
 4, "BAD",       0,		
 4, "UNK",       0,                     /* bit */
 1, "INTEGER_1", DW_ATE_signed,		/* MTYPE_I1  */
 2, "INTEGER_2", DW_ATE_signed,		/* MTYPE_I2  */
 4, "INTEGER_4", DW_ATE_signed,		/* MTYPE_I4  */
 8, "INTEGER_8", DW_ATE_signed,		/* MTYPE_I8  */
 1, "INTEGER*1", DW_ATE_unsigned,	/* MTYPE_U1  */
 2, "INTEGER*2", DW_ATE_unsigned,	/* MTYPE_U2  */
 4, "INTEGER*4", DW_ATE_unsigned,	/* MTYPE_U4  */
 8, "INTEGER*8", DW_ATE_unsigned,	/* MTYPE_U8  */
 4, "REAL_4",    DW_ATE_float,		/* MTYPE_F4  */
 8, "REAL_8",    DW_ATE_float,		/* MTYPE_F8  */
#if defined(TARG_IA64) || defined(TARG_X8664)
 16,"REAL_10",   DW_ATE_float,          /* MTYPE_F10 */
#else
 10,"UNK",       DW_ATE_float,		/* MTYPE_F10 */
#endif
 16,"REAL_16",   DW_ATE_float,		/* MTYPE_F16 */
 1 ,"CHAR" ,     DW_ATE_signed_char,    /* MTYPE_STR */
 16,"REAL_16",   DW_ATE_float,		/* MTYPE_FQ  */
 1, "UNK",       DW_ATE_unsigned_char,	/* MTYPE_M   */		
 8, "COMPLEX_4", DW_ATE_complex_float,	/* MTYPE_C4  */
 16,"COMPLEX_8", DW_ATE_complex_float,	/* MTYPE_C8  */
 32,"COMPLEX_16",DW_ATE_complex_float,	/* MTYPE_CQ  */
 1, "VOID",      0,                     /* MTYPE_V   */
#ifdef TARG_IA64
 1, "UNK",       0,                     /* MTYPE_BS  */
 4, "ADDRESS_4", DW_ATE_unsigned,       /* MTYPE_A4  */
 8, "ADDRESS_8", DW_ATE_unsigned,       /* MTYPE_A8  */
#else
 1, "LOGICAL_1", DW_ATE_boolean,	
 2, "LOGICAL_2", DW_ATE_boolean,	
 4, "LOGICAL_4", DW_ATE_boolean,	
#endif
#if defined(TARG_IA64) || defined(TARG_X8664)
 32,"COMPLEX_10",DW_ATE_complex_float,  /* MTYPE_C10 */
#else
 8, "LOGICAL_8", DW_ATE_boolean,	
#endif
} ;

/*===================================================
 *
 * DST_create_basetype
 *
 * Given a SCALAR ty, returns the corresponding DST
 * basetype for its typeid. Appends it to compilation 
 * unit to avoid duplication.
 *
 *===================================================
*/
static DST_INFO_IDX
DST_create_basetype (TY_IDX ty)
{
  TYPE_ID bt ;
  DST_INFO_IDX i ;

  bt = TY_mtype(ty);

  if (bt == MTYPE_V) return(DST_INVALID_IDX);

  if (TY_is_logical(Ty_Table[ty]))
    bt = bt -MTYPE_I1 + MTYPE_V + 1 ;

  if (!DST_IS_NULL(base_types[bt]))
    return base_types[bt];

  i = DST_mk_basetype(ate_types[bt].name,
		      ate_types[bt].encoding, 
		      ate_types[bt].size);

  base_types[bt] = i;
  DST_append_child(comp_unit_idx,i);
  return i;
}

// We have a subrange
// enter it.
static DST_INFO_IDX 
DST_enter_subrange_type (ARB_HANDLE ar) 
{
  DST_INFO_IDX i ;
  DST_cval_ref lb,ub;
  DST_flag     const_lb,const_ub ;
  BOOL         extent = FALSE ;
  USRCPOS src;
  USRCPOS_clear(src);
  DST_INFO_IDX type;

  const_lb = ARB_const_lbnd(ar) ;
  const_ub = ARB_const_ubnd(ar) ;

  
  if (const_lb)
    lb.cval = ARB_lbnd_val(ar) ;
  else {
    ST* var_st = &St_Table[ARB_lbnd_var(ar)];
    type = DST_create_basetype(ST_type(var_st));
    lb.ref = DST_mk_variable(
			     src,                    // srcpos
			     ST_name(var_st),
			     type,    
			     0,  
			     ST_st_idx(var_st), 
			     DST_INVALID_IDX,        
			     FALSE,                  // is_declaration
			     ST_sclass(var_st) == SCLASS_AUTO,
			     FALSE,  // is_external
			     FALSE  ); // is_artificial
#ifdef KEY
    // Bug 4829 - do not enter variables without a name.
    if (ST_name(var_st) != NULL && *ST_name(var_st) != '\0')
#endif
    DST_append_child(comp_unit_idx,lb.ref);
  } 

  if (const_ub)
    ub.cval = ARB_ubnd_val(ar) ;
  else {
    ST* var_st = &St_Table[ARB_ubnd_var(ar)];
    type = DST_create_basetype(ST_type(var_st));
    ub.ref = DST_mk_variable(
			     src,                    // srcpos
			     ST_name(var_st),
			     type,    
			     0,  
			     ST_st_idx(var_st), 
			     DST_INVALID_IDX,        
			     FALSE,                  // is_declaration
			     ST_sclass(var_st) == SCLASS_AUTO,
			     FALSE,  // is_external
			     FALSE  ); // is_artificial
#ifdef KEY
    // Bug 4829 - do not enter variables without a name.
    if (ST_name(var_st) != NULL && *ST_name(var_st) != '\0')
#endif
    DST_append_child(comp_unit_idx,ub.ref);
  }

  i = DST_mk_subrange_type(const_lb,
			   lb, 
			   const_ub,
			   ub);

  if (extent) 
    DST_SET_count(DST_INFO_flag(DST_INFO_IDX_TO_PTR(i))) ;

  return i;  
}

#endif /* KEY */
// We have an array
// enter it.
static DST_INFO_IDX
DST_enter_array_type(gs_t type_tree, TY_IDX ttidx  , TY_IDX idx,INT tsize)
{ 
   DST_INFO_IDX dst_idx = TYPE_DST_IDX(type_tree);

   if(gs_tree_code_class(type_tree) != GS_TCC_TYPE) {
        DevWarn("DST_enter_array_type input not TCC_TYPE but %c",
                gs_tree_code_class(type_tree));
   }

   if(DST_IS_NULL(dst_idx)) {

      USRCPOS src;
      // For now, the source location appears bogus
      // (or at least odd) for files other than the base
      // file, so lets leave it out. Temporarily.
      //USRCPOS_srcpos(src) = Get_Srcpos();
#ifdef KEY
    USRCPOS_srcpos(src) = Get_Srcpos();
#else
      USRCPOS_clear(src);
#endif // KEY

      //if tsize == 0, is incomplete array

      gs_t elt_tree = gs_tree_type(type_tree);
      TY_IDX itx = TYPE_TY_IDX(elt_tree);

      // not created yet, so create
      DST_INFO_IDX inner_dst  =
                    Create_DST_type_For_Tree (elt_tree,itx, idx);

#ifndef KEY
      dst_idx = DST_mk_array_type( src,
                            0, // name ?. Nope array types not named: no tag
				// in  C/C++
                           inner_dst, // element type DST_INFO_IDX
                           tsize, // type size
                           DST_INVALID_IDX, // not inlined
                           (tsize == 0)); // pass non-zero if incomplete.
#else
      // We follow the Fortran-style DW_TAG_array_type declaration whereby
      // we would have a DW_TAG_subrange_type declaration for each dimension.
      // Without this, the type information seems to be clobbered for arrays.
      dst_idx = DST_mk_array_type( src,
                            0, // name ?. Nope array types not named: no tag
				// in  C/C++
                           inner_dst, // element type DST_INFO_IDX
                           0, 
                           DST_INVALID_IDX, // not inlined
                           TRUE); 
      TY& tt = Ty_Table[ttidx];
      ARB_HANDLE arb = TY_arb(ttidx);
      DST_INFO_IDX d;
      if ( TY_kind (tt) != KIND_INVALID ) {
	for (INT index = TY_AR_ndims(ttidx) - 1; index >= 0; index--) {
	  // For C++, we may have declarations like "extern int a[]"
	  // Fix bug 322
	  if (!ARB_ubnd_var(arb[index]))
	    break;	
	  // Fix bug 383
	  if (!ARB_const_ubnd(arb[index])) { 
	    ST* var_st = &St_Table[ARB_ubnd_var(arb[index])];
	    if (ST_sclass(var_st) == SCLASS_AUTO)
	      break;
	  }
	  d = DST_enter_subrange_type(arb[index]);
	  DST_append_child(dst_idx,d);
	}
      }
#endif
      DST_append_child(comp_unit_idx,dst_idx);

      TYPE_DST_IDX(type_tree) = dst_idx;
    }
    return dst_idx;
}

// Given a tree with
// POINTER_TYPE, type is OFFSET_TYPE
// we know this is a pointer_to_member tree.
// And we know debug_level >= 2 
// And we know we've not constructed it yet
// (see the caller code)
// What we want to construct here is
// DW_TAG_ptr_to_member_type
// DW_AT_type (of the type of the offset_type)
// DW_AT_containing_type ( of the type of the
//    ttree base-type record-type
static  DST_INFO_IDX
DST_construct_pointer_to_member(gs_t type_tree)
{
    gs_t ttree = gs_tree_type(type_tree);
    FmtAssert(gs_tree_code(type_tree) == GS_POINTER_TYPE
                        && gs_tree_code(ttree) == GS_OFFSET_TYPE,
                          ("DST_construct_pointer_to_member:"
			   "invalid incoming arguments "));
    USRCPOS src;
    // For now, the source location appears bogus
    // (or at least odd) for files other than the base
    // file, so lets leave it out. Temporarily.
    //USRCPOS_srcpos(src) = Get_Srcpos();
    DST_INFO_IDX error_idx = DST_INVALID_INIT;


#ifdef KEY
    USRCPOS_srcpos(src) = Get_Srcpos();
#else
    USRCPOS_clear(src);
#endif // KEY

    char *name1 = 0;

//bug 11554: use is_typedef to check whether or not it is a typedef type
#ifdef KEY
    if(is_typedef(type_tree) == FALSE){
      // not a typedef type.
      name1 =  Get_Name(gs_tree_type(type_tree));
    } else {
      // is a typedef type
      gs_t tname = gs_type_name (type_tree);
      name1 = Get_Name(gs_decl_original_type(tname));
    }
#else
    if(gs_decl_original_type(type_tree)== 0) {
      // not a typedef type.
      name1 =  Get_Name(gs_tree_type(type_tree));
    } else {
      // is a typedef type
      name1 = Get_Name(gs_decl_original_type(type_tree));
    }
#endif

    gs_t member_type = gs_tree_type(ttree);
    if( gs_tree_code(member_type) == GS_ERROR_MARK) {
	return error_idx;
    }
    if(gs_tree_code_class(ttree) != GS_TCC_TYPE) {
	DevWarn("Unexpected tree shape1: pointer_to_member %c\n",
		gs_tree_code_class(ttree));
    }
    TY_IDX midx = Get_TY(member_type);
    TYPE_TY_IDX(member_type) = midx;
    TY_IDX orig_idx = 0;

    DST_INFO_IDX mdst =
                    Create_DST_type_For_Tree(member_type,
                        midx,
			orig_idx);
    TYPE_DST_IDX(type_tree) = mdst;


    gs_t base_type = gs_type_offset_basetype(ttree);
    if( gs_tree_code(base_type) == GS_ERROR_MARK) {
	return error_idx;
    }
    if(gs_tree_code_class(base_type) != GS_TCC_TYPE) {
	DevWarn("Unexpected tree shape2: pointer_to_member %c\n",
		gs_tree_code_class(base_type));
    }
    TY_IDX container_idx = Get_TY(base_type);


    DST_INFO_IDX container_dst = Create_DST_type_For_Tree(
			base_type,
                        container_idx,
			orig_idx);
    TYPE_DST_IDX(type_tree) = container_dst;
			

    DST_INFO_IDX lidx =
	DST_mk_ptr_to_member_type(src,
		  name1, //name, I expect it to be empty 
		  mdst	, //type of member
		  container_dst); //type of class
    return lidx;

}

      /*--------------------------------------------------
       * Visible routines for creating the DST information
       *--------------------------------------------------*/

// This is in parallel to Create_TY_For_Tree() in 
// tree_symtab.cxx  and must be kept so.
// We cannot use the TY tree as too much information
// is lost that debuggers depend on.
// type_tree_in is void * so 
//
// idx is non-zero only for RECORD and UNION, 
// when there is forward declaration.
//
// ttidx is the type tree Create_TY_For_Tree just created
// for type_tree

DST_INFO_IDX
Create_DST_type_For_Tree (gs_t type_tree, TY_IDX ttidx  , TY_IDX idx, bool ignoreconst, bool ignorevolatile)
{
    
    DST_INFO_IDX dst_idx = DST_INVALID_INIT;

    if(gs_tree_code_class(type_tree) != GS_TCC_TYPE) {
	DevWarn("Create_DST_type_For_Tree input not TCC_TYPE but %c",
		gs_tree_code_class(type_tree));
	return dst_idx;

    }
	 


    // for typedefs get the information from the base type
    if (gs_type_name(type_tree)) {
	    
	if(  idx == 0  &&
	    (gs_tree_code(type_tree) == GS_RECORD_TYPE ||
	     gs_tree_code(type_tree) == GS_UNION_TYPE) &&
	     gs_tree_code(gs_type_name(type_tree)) == GS_TYPE_DECL &&
	     gs_type_main_variant(type_tree) != type_tree) {
		idx = Get_TY (gs_type_main_variant(type_tree));

#ifndef KEY
		// The following code always a return an invalid DST_IDX. This 
		// causes the back-end to skip DW_AT_type for any variable 
		// declared to be of a user-defined type (which is a typedef 
		// of a base type).

		//if (TYPE_READONLY(type_tree))
		//	Set_TY_is_const (idx);
		//if (TYPE_VOLATILE(type_tree))
		//	Set_TY_is_volatile (idx);
		// restrict qualifier not supported by gcc
		//TYPE_TY_IDX(type_tree) = idx;
		//return idx;
		// FIX	      
		//hack so rest of gnu need know nothing of DST 

		return dst_idx;
#endif
       } else {
//
       }
    }
    DST_INFO_IDX current_scope_idx = comp_unit_idx;
    if(gs_type_context(type_tree)) {
	current_scope_idx = DST_get_context(gs_type_context(type_tree));
    }

    char *name1 = Get_Name(type_tree);

    TYPE_ID mtype;
    INT tsize;
    BOOL variable_size = FALSE;
    gs_t type_size = gs_type_size(type_tree);
    UINT align = gs_type_align(type_tree) / BITSPERBYTE;
    if (type_size == NULL) {
		// In a typedef'd type 
		// incomplete structs have 0 size
		FmtAssert( gs_tree_code(type_tree) == GS_ARRAY_TYPE 
			|| gs_tree_code(type_tree) == GS_UNION_TYPE
			|| gs_tree_code(type_tree) == GS_RECORD_TYPE
			|| gs_tree_code(type_tree) == GS_ENUMERAL_TYPE
			|| gs_tree_code(type_tree) == GS_VOID_TYPE
			|| gs_tree_code(type_tree) == GS_LANG_TYPE,
			  ("Create_DST_type_For_Tree: type_size NULL for non ARRAY/RECORD"));
		tsize = 0;
   }
   else {
		if (gs_tree_code(type_size) != GS_INTEGER_CST) {
			if (gs_tree_code(type_tree) == GS_ARRAY_TYPE) {
				tsize = 0;
			}
			else {
				tsize = -1;
			}
			variable_size = TRUE;
		}
		else
			tsize = gs_get_integer_value(type_size) / BITSPERBYTE;
   }

   // why is this simpler than the one in kgccfe/wfe_dst.cxx???
   if (!ignoreconst && gs_type_readonly (type_tree)) {
       dst_idx = TYPE_DST_IDX(type_tree);
       if(DST_IS_NULL(dst_idx)) {
         TY_IDX itx = TYPE_TY_IDX(type_tree);
         DST_INFO_IDX unqual_dst = Create_DST_type_For_Tree (type_tree,itx, idx, true, false);
         dst_idx = DST_mk_const_type (unqual_dst) ;
         DST_append_child(current_scope_idx,dst_idx);
         TYPE_DST_IDX(type_tree) = dst_idx;
       }
       return dst_idx ;
   }

   if (!ignorevolatile && gs_type_volatile (type_tree)) {
       dst_idx = TYPE_DST_IDX(type_tree);
       if(DST_IS_NULL(dst_idx)) {
         TY_IDX itx = TYPE_TY_IDX(type_tree);
         DST_INFO_IDX unqual_dst = Create_DST_type_For_Tree (type_tree,itx, idx, true, true);
         dst_idx = DST_mk_volatile_type (unqual_dst) ;
         DST_append_child(current_scope_idx,dst_idx);
         TYPE_DST_IDX(type_tree) = dst_idx;
       }
       return dst_idx ;
   }

   int encoding = 0;
   switch (gs_tree_code(type_tree)) {
   case GS_VOID_TYPE:
   case GS_LANG_TYPE:
		//idx = MTYPE_To_TY (MTYPE_V);	// use predefined type
		break;
   case GS_BOOLEAN_TYPE:
		{
		encoding = DW_ATE_boolean;
		goto common_basetypes;
		}
   case GS_INTEGER_TYPE:
		{
		// enter base type
#ifdef KEY
		// (copied over from fix for bug 3962 in kgccfe/wfe_dst.cxx).
		// Bug 4326 - GNU produces INTEGER_TYPE node
		// even for CHARACTER_TYPE from gnu/stor_layout.c
		// and then fixes it up during Dwarf emission in
		// gnu/dwarf2out.c (base_type_die). We should do
		// the same. 
		if (tsize == 1 && 
		    (strcmp(name1, "char") == 0 ||
		     strcmp(name1, "unsigned char") == 0)) {
		  if (gs_decl_unsigned(type_tree)) {
		    encoding = DW_ATE_unsigned_char;
		  } else {
		    encoding = DW_ATE_signed_char;
		  }
		  goto common_basetypes;		    
		}		  
#endif
		if (gs_decl_unsigned(type_tree)) {
		 encoding = DW_ATE_unsigned;
		} else {
		 encoding = DW_ATE_signed;
		}
		goto common_basetypes;
		}
     case GS_CHAR_TYPE:
		{
		// enter base type
		if (gs_decl_unsigned(type_tree)) {
		 encoding = DW_ATE_unsigned_char;
		} else {
		 encoding = DW_ATE_signed_char;
		}
		goto common_basetypes;
		}
     case GS_ENUMERAL_TYPE:
		{
#ifdef KEY
                // Handle typedefs for struct/union
                if (is_typedef (type_tree))
                  dst_idx = DST_Create_type ((ST*)NULL, gs_type_name (type_tree));
                else
#endif
		dst_idx = DST_enter_enum(type_tree,ttidx,idx,
                        tsize);

		}
		break;
     case GS_REAL_TYPE:
		{
		// enter base type
		encoding = DW_ATE_float;
		goto common_basetypes;
		}
    case GS_COMPLEX_TYPE:
                // enter base type
                encoding = DW_ATE_complex_float;
    //============
    common_basetypes:
		{
		FmtAssert(name1 != 0 && strlen(name1) != 0,
		   ("name of base type empty, cannot make DST entry!"));

                std::string names(name1);
                DST_Type_Map::iterator p =
                        basetypes.find(names);
                if(p != basetypes.end()) {
                        DST_INFO_IDX t = (*p).second;
                        return t;
                } else {
#ifdef KEY
                       // Handle typedefs for common basetypes
                       if (is_typedef (type_tree))
                         dst_idx = DST_Create_type ((ST*)NULL,
                                                    gs_type_name (type_tree));
                       else
#endif
                       {
                       dst_idx = DST_mk_basetype(
                                name1,encoding,tsize);
                       basetypes[names] = dst_idx;
		       DST_append_child(comp_unit_idx,dst_idx);
                       }
                }

                }

		break;
    case GS_REFERENCE_TYPE:
	       {
                dst_idx = TYPE_DST_IDX(type_tree);
		if(DST_IS_NULL(dst_idx)) {

		  gs_t ttree = gs_tree_type(type_tree);
	          TY_IDX itx = TYPE_TY_IDX(ttree);
	          DST_INFO_IDX inner_dst =
		    Create_DST_type_For_Tree (ttree,itx, idx);


		  // not created yet, so create
		  dst_idx = DST_mk_reference_type(
		       	inner_dst,    // type ptd to
			DW_ADDR_none, // no address class
			tsize);
                                
                  DST_append_child(current_scope_idx,dst_idx);

		  TYPE_DST_IDX(type_tree) = dst_idx;
		}
               }
		break;
    case GS_POINTER_TYPE:
#ifdef KEY
               // Handle typedefs for pointer types
               if (is_typedef (type_tree))
                 dst_idx = DST_Create_type ((ST*)NULL, gs_type_name (type_tree));
               else
#endif
	       {
                dst_idx =  TYPE_DST_IDX(type_tree);
                if(DST_IS_NULL(dst_idx)) {

		  gs_t ttree = gs_tree_type(type_tree);
		  if(gs_tree_code(ttree) == GS_OFFSET_TYPE) {
		     dst_idx = DST_construct_pointer_to_member(type_tree);
		  } else {

	              TY_IDX itx = TYPE_TY_IDX(ttree);
	              DST_INFO_IDX inner_dst =
		         Create_DST_type_For_Tree (ttree,itx, idx);

                      // not created yet, so create


		      dst_idx = DST_mk_pointer_type(
		       	inner_dst,    // type ptd to
			DW_ADDR_none, // no address class
			tsize);
		  }
                                
                  DST_append_child(current_scope_idx,dst_idx);

		  TYPE_DST_IDX(type_tree) = dst_idx;
                 }
               }
	       break;
    case GS_OFFSET_TYPE:

		//  Do nothing. We should not get here.
		break;

    case GS_ARRAY_TYPE:
		
	       {
                dst_idx = DST_enter_array_type(type_tree, 
			ttidx, idx, tsize);
	       }
	       break;
    case GS_RECORD_TYPE:
    case GS_UNION_TYPE:
		{
#ifdef KEY
                // Handle typedefs for struct/union
                if (is_typedef (type_tree))
                  dst_idx = DST_Create_type ((ST*)NULL, gs_type_name (type_tree));
                else
#endif
		dst_idx = DST_enter_struct_union(type_tree,ttidx,idx,
			tsize);
		}
		break;
    case GS_METHOD_TYPE:
		{
		//DevWarn ("Encountered METHOD_TYPE at line %d", lineno);
		}
		break;

    case GS_FUNCTION_TYPE:
		{	// new scope for local vars
		} // end FUNCTION_TYPE scope
		break;
#ifdef TARG_X8664
    case GS_VECTOR_TYPE:
		{
		  // GNU's debug representation for vectors.
		  // See gen_array_type_die() for details.
		  type_tree = gs_tree_type (gs_type_fields
		               (gs_type_debug_representation_type (type_tree)));
		  ttidx = Get_TY (type_tree);
		  dst_idx = DST_enter_array_type(type_tree, ttidx, idx, tsize);
		  DST_SET_GNU_vector(DST_INFO_flag(DST_INFO_IDX_TO_PTR(dst_idx)));
		}
		break;
#endif // TARG_X8664
    default:

		FmtAssert(FALSE, ("Create_DST_type_For_Tree: unexpected tree_type"));
    }
    //if (TYPE_READONLY(type_tree))
//		Set_TY_is_const (idx);
 //   if (TYPE_VOLATILE(type_tree))
//		Set_TY_is_volatile (idx);
    // restrict qualifier not supported by gcc
 //   TYPE_TY_IDX(type_tree) = idx;
	
    
    return dst_idx;
}

extern DST_INFO_IDX
Create_DST_decl_For_Tree(
        gs_t decl, ST* var_st)
{



  DST_INFO_IDX cur_idx = DECL_DST_IDX(decl);
  if(!DST_IS_NULL(cur_idx)) {

	// Already processed. Do not redo!
	return cur_idx;
  }

  DST_INFO_IDX dst_idx = DST_INVALID_INIT;

  //if (gs_tree_code(decl) != GS_VAR_DECL) return var_idx;

  if (gs_decl_ignored_p(decl))    {
  	return cur_idx;
  }


  // if tests  for locals and returns if local 
  // if (gs_decl_context(decl) != 0)  return var_idx;


  // If this is simply an extern decl (not an instance)
  // (and the context is not an inner lexical block,
  // where we could be hiding an outer decl with the same name so
  // need a decl here) 
  // then we can just do nothing. 
  // externs get debug info at the point of def.
  if (gs_tree_code(decl) == GS_VAR_DECL && 
		 gs_decl_external(decl) && !gs_decl_common(decl)) {
      return cur_idx;
  }

#ifndef KEY
  // For now ignore plain declarations?
  // till we get more working
  if (gs_tree_code(decl) == GS_VAR_DECL && (!gs_tree_static(decl)
		 && !gs_decl_common(decl))) {
	return cur_idx ;
  }
#endif // !KEY

  // is something that we want to put in DST
  // (a var defined in this file, or a type).
  // The following eliminates locals 
  //if(!TREE_PERMANENT(decl)) {
  //	// !in permanent obstack 
  //	return cur_idx ;
  // }

  int tcode = gs_tree_code(decl);
  switch(tcode) {
  case GS_VAR_DECL: {
      //Get_ST(decl);
      dst_idx = DST_Create_var(var_st,decl);
      }
      break;
  case GS_TYPE_DECL: {
      //Get_ST(decl);
      dst_idx = DST_Create_type(var_st,decl);
      }
      break;
  case GS_PARM_DECL: {
      //Get_ST(decl);
      dst_idx = DST_Create_Parmvar(var_st,decl);
      }
      break;
  case GS_FUNCTION_DECL: {
      dst_idx = DST_Create_Subprogram(var_st,decl);
      }
      break;
  default: {
      }
      break;
  }
  return dst_idx;
}



static DST_INFO_IDX
DST_Create_type(ST *typ_decl, gs_t decl)
{
    USRCPOS src;
    // For now, the source location appears bogus
    // (or at least odd) for files other than the base
    // file, so lets leave it out. Temporarily.
    //USRCPOS_srcpos(src) = Get_Srcpos();


#ifdef KEY
    USRCPOS_srcpos(src) = Get_Srcpos();
#else
    USRCPOS_clear(src);
#endif // KEY
    DST_INFO_IDX dst_idx = DST_INVALID_INIT;
    



    char *name1 = 0; 
    if(gs_decl_original_type(decl)== 0) {
      // not a typedef type.
      name1 =  Get_Name(gs_tree_type(decl));
    } else {
      // is a typedef type
#ifdef KEY
      // Yes, this is a typedef, and so get THAT typename, not the
      // original typename
      name1 = Get_Name(gs_tree_type(decl));
#else
      name1 = Get_Name(gs_decl_original_type(decl));
#endif
    }
  
    // FIX look in various contexts to find known types ?
    // It is not true base types that are the problem, it
    // is typedefs creating 'new types'.
    std::string names(name1);
    DST_Type_Map::iterator p =
                        basetypes.find(names);
    if(p != basetypes.end()) {
                        //Yep, already known.
        DST_INFO_IDX t = (*p).second;
        // hack so rest of gnu need know nothing of DST
        return t;
    } 

    DST_INFO_IDX current_scope_idx =
         DST_get_context(gs_decl_context(decl));

    // Nope, something new. make a typedef entry.
    // First, ensure underlying type is set up.
#ifdef KEY
    // Same as DECL_RESULT, but this looks to be the right macro
    gs_t undt = gs_decl_original_type(decl);
#else
    gs_t undt = DECL_RESULT(decl);
#endif
    TY_IDX base;

    if(!undt) {
	DevWarn ("DST no result type for typedef decl: impossible");
	return DST_INVALID_IDX;
    } 
    // Do for side effect of creating DST for base type.
    // ie, in typedef int a, ensure int is there.
    base = Get_TY(undt);
    DST_INFO_IDX dst = 
	Create_DST_type_For_Tree(undt,base,
		/* struct/union fwd decl TY_IDX=*/ 0);

    dst_idx = DST_mk_typedef( src,
                          name1, // new type name we are defining
                          dst, // type of typedef
                          DST_INVALID_IDX);
    DST_append_child(current_scope_idx,dst_idx);
    // and add the new type to our base types map
    basetypes[names] = dst_idx;
#ifdef KEY
    TYPE_DST_IDX(decl) = dst_idx;	// bug 6247
#else
    TYPE_DST_IDX(undt) = dst_idx;
#endif

    return dst_idx;
}

static DST_INFO_IDX
DST_Create_Parmvar(ST *var_st, gs_t param)
{
    USRCPOS src;
    // For now, the source location appears bogus
    // (or at least odd) for files other than the base
    // file, so lets leave it out. Temporarily.
    //USRCPOS_srcpos(src) = Get_Srcpos();

#ifdef KEY
    USRCPOS_srcpos(src) = Get_Srcpos();
#else
    USRCPOS_clear(src);
#endif // KEY


    DST_INFO_IDX type_idx = DST_INVALID_INIT;
#ifdef KEY //************************************************************
           //bug 11589: The var_st is the just created st for this param.
           //           Don't know why we have to get (may create) a new one.
           //           
           //NOTE:      Using Get_ST here will cause an infinite
           //           loop in compiling for VLA because of the DECL_ST
           //           set_DECL_ST does not match.
           //************************************************************
    ST * st = var_st?var_st:Get_ST(param);//As a side effect,
#else
    ST * st = Get_ST(param); // As a side effect,
#endif
                //parameters and the types are set in ST.
		// and in the DST
    gs_t type = gs_tree_type(param);

    TY_IDX ty_idx = Get_TY(type); 

    DST_INFO_IDX dtype = DECL_DST_IDX(param);
    return dtype;
}

// Look thru the members of myrecord, looking for
// static vars. If we find one with that
// linkage name, return the DST_INFO_IDX.
// Return the DST_INVALID_INIT value if such a member
// not found.
//
static DST_INFO_IDX
DST_find_class_member(char * linkage_name_in, gs_t myrecord)
{
    DST_INFO_IDX return_member_dst = DST_INVALID_INIT;
    if(!linkage_name_in)
		return return_member_dst;
    gs_t field = gs_type_fields(myrecord);
    for( ; field ; field = gs_tree_chain(field) )
    {
        if(gs_tree_code(field) == GS_VAR_DECL) {

             char * linkage_name =
                        gs_identifier_pointer(gs_decl_assembler_name (field));
	     if(linkage_name && 
			(strcmp(linkage_name,  linkage_name_in) == 0)) {
		return_member_dst =  DECL_DST_FIELD_IDX(field);
                return return_member_dst;
	     }
        }
    }
    gs_t methods = gs_type_methods(myrecord);
    for  ( ; methods != NULL; methods = gs_tree_chain(methods)) {
        if(gs_tree_code(methods) == GS_FUNCTION_DECL ) {

	   // Skip if function was never translated by GCC into RTL and so was
	   // never needed.
	   if (gs_operand(methods, GS_FLAGS) == NULL)
	     continue;

           if ( gs_decl_artificial(methods)) {
             // compiler generated methods are not interesting.
             // We want only ones user coded.
             continue;
           } else {
	     char * linkage_name = 
		   gs_identifier_pointer(gs_decl_assembler_name (methods));
	     if(linkage_name &&
			(strcmp(linkage_name,  linkage_name_in) == 0)) {
		return_member_dst =  DECL_DST_FIELD_IDX(methods);
                return return_member_dst;
	     }
           }
        }
    }


    return return_member_dst;
}

static DST_INFO_IDX
DST_Create_var(ST *var_st, gs_t decl)
{
    USRCPOS src;
    // For now, the source location appears bogus
    // (or at least odd) for files other than the base
    // file, so lets leave it out. Temporarily.
    //USRCPOS_srcpos(src) = Get_Srcpos();



    int is_external = gs_tree_public(decl); // true if var has external linkage
    int external_decl = gs_decl_external(decl); // true if var undefined
    gs_t context = gs_decl_context(decl);

    // If it is external, 
    // then unless it hides some local it need not
    // be in the dwarf at all.
    if(external_decl) {
     if( context && gs_tree_code(context) == GS_RECORD_TYPE) {
	// Is C++ class function mention, not a def.
	DST_INFO_IDX no_info = DST_INVALID_INIT;
	return no_info;
     }
     // FIXME: does not remove other externals...

    }
    char *field_name = Get_Name(decl);

    int class_var_found_member = 0;
    DST_INFO_IDX class_var_idx = DST_INVALID_INIT;
    if (lang_cplus) { // wgen
#ifdef KEY
    char *linkage_name = "";	
    if (!gs_decl_artificial (decl) && gs_decl_name(decl) 
        && gs_decl_assembler_name_set_p(decl) 
	&& gs_decl_assembler_name(decl) != NULL // bug 12666
       )
      linkage_name = gs_identifier_pointer(gs_decl_assembler_name (decl));
#else
    char *linkage_name = 	
		gs_identifier_pointer(gs_decl_assembler_name (decl));
#endif // KEY

    if(context && gs_tree_code(context) == GS_RECORD_TYPE) {
	/*look for  static data member decl*/
	class_var_idx =
		DST_find_class_member(linkage_name, context);
	if(!DST_IS_NULL(class_var_idx)) {
		class_var_found_member = 1;
		field_name = 0; // we will use DW_AT_specification
		   //to get name in debugger
		   //So do not assign name here.
	}
	// Field name should now have the class name and ::
	// prepended, per dwarf2
	// Save_Str2 can glob 2 pieces together.
	char *classname = Get_Name(context);
	if(classname && !class_var_found_member) {
	   int len = strlen(classname);
	   char* newname = new char[len+5 
			/* 5 makes room for :: and null char */];

	   // no 0 check: let it crash to get signal handler msg.

	   strcpy(newname,classname);
	   strcpy(newname+len,"::");
	   if(field_name) {
	    // We do not need the string in a stable storage
	    // area, but this is a convenient way to
	    // concatenate strings and avoid a memory leak.
	    field_name = Index_To_Str(Save_Str2(newname,field_name));
	   }

	   delete [] newname;
	}

    }
    }

#ifdef KEY
    USRCPOS_srcpos(src) = Get_Srcpos();
#else
    USRCPOS_clear(src);
#endif // KEY
    DST_INFO_IDX dst = DST_INVALID_INIT;

    DST_INFO_IDX type = TYPE_DST_IDX(gs_tree_type(decl));


#ifdef KEY
    // Bug 1717 - for anonymous unions like
    //      union {
    //       int member1;
    //       char member2;
    //      };
    // a variable without a name used to be created.
    // According to the standard, there could be assignment
    // statements like "member1 = 123" and the compiler is
    // supposed to emit the correct locations as if the 
    // union members are all declared as individual variables 
    // with identical address. Here, we catch all these variables
    // and generate new entries for all members in the anonymous union as
    // if they are individual variables.
    if (field_name && strcmp(field_name, "") == 0 &&
	gs_tree_code(gs_tree_type(decl)) == GS_UNION_TYPE &&
	gs_type_fields(gs_tree_type(decl)) &&
	// exposed by bug 3531 - treat types with fields that are
	// template declarations the usual way; because we don't care
	// about them right now and we don't know how to handle them yet.
	gs_tree_code(gs_type_fields(gs_tree_type(decl))) != GS_TEMPLATE_DECL) {
      gs_t field = gs_type_fields(gs_tree_type(decl));
      DST_INFO_IDX current_scope_idx =
	DST_get_context(gs_decl_context(decl));
      for( ; field ; field = gs_tree_chain(field) )
      { 
	field_name = Get_Name(field);
	if (gs_tree_code(field) == GS_FIELD_DECL ||
	    gs_tree_code(field) == GS_VAR_DECL) {
	  dst = DST_mk_variable(src,                    // srcpos
				field_name,
				TYPE_DST_IDX(gs_tree_type(field)),
				0,  // offset (fortran uses non zero )
				ST_st_idx(var_st), // underlying type here, not typedef.
				DST_INVALID_IDX,        // abstract origin
				external_decl,          // is_declaration
				FALSE,                  // is_automatic
				is_external,  // is_external
				FALSE  ); // is_artificial	
	  // Bug 4829 - do not enter variables without a name.
	  if (field_name != NULL && *field_name != '\0')
	    DST_append_child (current_scope_idx, dst);	
	} 
	// Bug 3889 - CONST_DECL, FUNCTION_DECL, and TYPE_DECL  are skipped..
      }	
      return dst;
    }	
#endif
    dst = DST_mk_variable(
        src,                    // srcpos
        field_name,
        type,    // user typed type name here (typedef type perhaps).
	0,  // offset (fortran uses non zero )
        ST_st_idx(var_st), // underlying type here, not typedef.
        DST_INVALID_IDX,        // abstract origin
        external_decl,          // is_declaration
        FALSE,                  // is_automatic
        is_external,  // is_external
	FALSE  ); // is_artificial

    DST_INFO_IDX current_scope_idx =
         DST_get_context(gs_decl_context(decl));


#ifdef KEY
    // because all our variables have a linkage name, we don't want to do this
    //if(linkage_name && linkage_name[0] != '\0' && field_name && strcmp(linkage_name,field_name)) {
       //// we have a genuine linkage name.
      //DST_add_linkage_name_to_variable(dst, linkage_name);
    //}
#else
    // At present, DST producer in common/com does not
    // allow setting linkage name

    //if(linkage_name && basename && strcmp(linkage_name,basename)) {
      // we have a genuine linkage name.
     // DST_add_linkage_name_to_subprogram(dst, linkage_name);
    //}
#endif



    // If this is a def of a static var member, want DW_AT_specification
    // added to point to class mem
    if(class_var_found_member) {
       DST_add_specification_to_variable(dst, class_var_idx);
    }


#ifdef KEY
    // Bug 4829 - do not enter variables without a name.
    if (field_name != NULL && *field_name != '\0')
#endif
    DST_append_child (current_scope_idx, dst);
    return dst;

}

// For each parameter decl,
// create a record and attach to the function
// it belongs to.
// is_declaration_only stuff does NOT work: it was
// for member functions, which we don't deal with. 
// see DST_enter_member_function() comments
// 
static void
DST_enter_param_vars(gs_t fndecl,
  DST_INFO_IDX parent_idx,
  gs_t parameter_list,
  int is_abstract_root,
  int is_declaration_only)
{
    USRCPOS src;
    USRCPOS_srcpos(src) = Get_Srcpos();

   
    gs_t pdecl = parameter_list;

    for(  ; pdecl; pdecl = gs_tree_chain(pdecl)) {
#ifdef KEY
      // Bug 4443 - Due to the change in the front-end, PARM_DECL nodes 
      // are converted to INDIRECT_REF node now (pass param by invisible
      // reference in wfe_decl.cxx).
      if(gs_tree_code(pdecl) == GS_INDIRECT_REF) {
	_gs_code(pdecl, GS_PARM_DECL);

        DST_INFO_IDX param_idx = DST_INVALID_INIT;
        DST_INFO_IDX type_idx = DST_INVALID_INIT;
        BOOL is_artificial = gs_decl_artificial(pdecl);
	int decl_to_be_restored = 0;
	int type_to_be_restored = 0;
	gs_t type = gs_tree_type(pdecl);

	
	DST_INFO_IDX save_type_idx = TYPE_DST_IDX(type);
	DST_INFO_IDX save_decl_idx = DECL_DST_IDX(pdecl);

	ST *st = 0;
	if(!is_declaration_only) {
	  st = Get_ST(gs_tree_operand(pdecl, 0)); // As a side effect,
		// parameters and the types are set in ST
		// and dst
		// and we do not want ST set up in this case.
	}
	


        TY_IDX ty_idx = Get_TY(type);
	

	type_idx = TYPE_DST_IDX(type);
	
	char *name = Get_Name(pdecl);
	
	DST_INFO_IDX initinfo = DST_INVALID_IDX;
        //tree initl = DECL_INITIAL(pdecl);
	//if(initl) {  FIXME: optional params
		//  Get_TY(initl); // As a side effect,
			// set types, etc
	  //initinfo = DECL_DST_IDX(initl);
	//}

	ST_IDX loc = (is_abstract_root || is_declaration_only)? 
		ST_IDX_ZERO: ST_st_idx(st);
	DST_INFO_IDX aroot = DST_INVALID_IDX;
	if(!is_abstract_root && !is_declaration_only) {
	  //. get the abstract root idx if it exists
	  aroot = DECL_DST_ABSTRACT_ROOT_IDX(pdecl);
	}


	param_idx = DST_mk_formal_parameter(
		src,
		name,
		type_idx,
		loc, // So backend can get location.
			// For a formal in abstract root
			// or a plain declaration (no def)
			// there is no location.
		aroot, // If inlined, and this
			// is concrete instance,pass idx of formal
			// in the abstract root
		initinfo, // C++ default value (of optional param)
		0?TRUE:FALSE, // true if C++ optional param // FIXME
	        FALSE, // DW_AT_variable_parameter not set in C++.
		is_artificial,
		is_declaration_only);           // bug 1735: this is totally bogus but is needed because
                                                // the backend be/cg/cgdrawf.cxx tries to put a location
                                                // attribute into the tag if it's not a declaration.

       // producer routines thinks we will set pc to fe ptr initially
       DST_RESET_assoc_fe (DST_INFO_flag(DST_INFO_IDX_TO_PTR(param_idx)));

	// The abstract root and
	// each concrete root have
	// distinct values.
	// The concrete root values need not be recorded, and
	// they will differ (no one unique value anyway) based
        // off the same pdecl node (????). FIXME 
	if(is_abstract_root) {
	   DECL_DST_ABSTRACT_ROOT_IDX(pdecl) = param_idx;
	} 
	if (!is_declaration_only && !is_abstract_root) {
	    DECL_DST_IDX(pdecl) = param_idx;
	}

	DST_append_child(parent_idx,param_idx);	

	DST_SET_deref(DST_INFO_flag( DST_INFO_IDX_TO_PTR(param_idx)));

	_gs_code(pdecl, GS_INDIRECT_REF);
      } else
#endif
      if(gs_tree_code_class(pdecl) != GS_TCC_DECLARATION) {
	DevWarn("parameter node not decl! tree node code %d ",
		gs_tree_code(pdecl));
      } else {
        DST_INFO_IDX param_idx = DST_INVALID_INIT;
        DST_INFO_IDX type_idx = DST_INVALID_INIT;
        BOOL is_artificial = gs_decl_artificial(pdecl);
	int decl_to_be_restored = 0;
	int type_to_be_restored = 0;
	gs_t type = gs_tree_type(pdecl);

	
	DST_INFO_IDX save_type_idx = TYPE_DST_IDX(type);
	DST_INFO_IDX save_decl_idx = DECL_DST_IDX(pdecl);

	ST *st = 0;
	if(!is_declaration_only) {
	  st = Get_ST(pdecl); // As a side effect,
		// parameters and the types are set in ST
		// and dst
		// and we do not want ST set up in this case.

	}
	


        TY_IDX ty_idx = Get_TY(type);


	type_idx = TYPE_DST_IDX(type);

	char *name = Get_Name(pdecl);

	DST_INFO_IDX initinfo = DST_INVALID_IDX;
        //tree initl = DECL_INITIAL(pdecl);
	//if(initl) {  FIXME: optional params
		//  Get_TY(initl); // As a side effect,
			// set types, etc
	  //initinfo = DECL_DST_IDX(initl);
	//}

	ST_IDX loc = (is_abstract_root || is_declaration_only)? 
		ST_IDX_ZERO: ST_st_idx(st);
	DST_INFO_IDX aroot = DST_INVALID_IDX;
	if(!is_abstract_root && !is_declaration_only) {
	  //. get the abstract root idx if it exists
	  aroot = DECL_DST_ABSTRACT_ROOT_IDX(pdecl);
	}


	param_idx = DST_mk_formal_parameter(
		src,
		name,
		type_idx,
		loc, // So backend can get location.
			// For a formal in abstract root
			// or a plain declaration (no def)
			// there is no location.
		aroot, // If inlined, and this
			// is concrete instance,pass idx of formal
			// in the abstract root
		initinfo, // C++ default value (of optional param)
		0?TRUE:FALSE, // true if C++ optional param // FIXME
	        FALSE, // DW_AT_variable_parameter not set in C++.
		is_artificial,
		is_declaration_only);           // bug 1735: this is totally bogus but is needed because
                                                // the backend be/cg/cgdrawf.cxx tries to put a location
                                                // attribute into the tag if it's not a declaration.

       // producer routines thinks we will set pc to fe ptr initially
       DST_RESET_assoc_fe (DST_INFO_flag(DST_INFO_IDX_TO_PTR(param_idx)));

	// The abstract root and
	// each concrete root have
	// distinct values.
	// The concrete root values need not be recorded, and
	// they will differ (no one unique value anyway) based
        // off the same pdecl node (????). FIXME 
	if(is_abstract_root) {
	   DECL_DST_ABSTRACT_ROOT_IDX(pdecl) = param_idx;
	} 
	if (!is_declaration_only && !is_abstract_root) {
	    DECL_DST_IDX(pdecl) = param_idx;
	}

	DST_append_child(parent_idx,param_idx);
      }
    }
}

//
// fndecl can be 0 if this is an asm stmt treated as a function, 
// rather than being a true function.
DST_INFO_IDX
DST_Create_Subprogram (ST *func_st, gs_t fndecl)
{
    USRCPOS src;
    DST_inline inlin;
    USRCPOS_srcpos(src) = Get_Srcpos();
    DST_INFO_IDX dst = DST_INVALID_INIT;
    DST_INFO_IDX ret_dst = DST_INVALID_IDX;

    DST_INFO_IDX current_scope_idx = fndecl ? 
	(DST_get_context(gs_decl_context(fndecl))): 
	comp_unit_idx;

     
    BOOL is_prototyped = FALSE;

    
    if(Debug_Level >= 2 && fndecl) {

	// Bug 1510 - get the result type from the function type declaration
	// rather than the result declaration type.
	gs_t restype = 0;
	if (gs_tree_type(fndecl))
	  restype = gs_tree_type(gs_tree_type(fndecl)); //bug 12820: a level of indirect

       if(restype) {
	 TY_IDX itx = Get_TY(restype);
	 ret_dst = TYPE_DST_IDX(restype); //bug 12820: return type should be set
	}

        gs_t type = gs_tree_type(fndecl);
	if(type) {
	//  tree arg_types = TYPE_ARG_TYPES(type);
	//  if(arg_types) {
		is_prototyped = TRUE;
	 // }
	}
    }


    char * basename;
    if (fndecl)
	basename = gs_identifier_pointer (gs_decl_name (fndecl));
    else
        basename = ST_name(func_st);
    char * linkage_name = ST_name(func_st);
#ifdef KEY
    // Bug 3846
    if (Debug_Level > 0 && fndecl &&
	gs_identifier_opname_p (gs_decl_name(fndecl)) && 
	gs_identifier_typename_p (gs_decl_name(fndecl))) {
      basename = cplus_demangle(linkage_name, DMGL_PARAMS | DMGL_ANSI | 
                                              DMGL_TYPES);
      if (basename) {
	basename = strstr(basename, "operator ");
	FmtAssert(basename, ("NYI"));
      } else {
	// Bug 4788 has a weird mangled name which cannot be demangled using
	// c++filt; g++ also generates the same linkage name (mangled name) for
	// that symbol. However, g++ is able to get back the demangled name 
	// somehow. For now, leave this operator name as such.
	// c++filt version 3.3 has this problem but version 3.4 seems to have
	// fixed this. Since, there are lot of changes to 
	// kgnu_common/libiberty/cp-demangle.c, we will wait for the front-end
	// upgrade to 3.4 which will fix this automatically.
	DevWarn(
         "encountered a mangled name that can not be demangled using c++filt");
	basename = gs_identifier_pointer (gs_decl_name (fndecl));
      }
    }
#endif

    char * funcname = basename;
    int is_abstract_root = 0;
    DST_INFO_IDX class_func_idx = DST_INVALID_INIT;
    int class_func_found_member = 0;
#ifdef KEY
    gs_t context = NULL;
    if (fndecl) context = gs_decl_context(fndecl);
#else
    gs_t context = gs_decl_context(fndecl);
#endif
    if(context && gs_tree_code(context) == GS_RECORD_TYPE) {
        /*look for  static data member decl*/
        class_func_idx =
                DST_find_class_member(linkage_name, context);
        if(!DST_IS_NULL(class_func_idx)) {
                class_func_found_member = 1;
                funcname = 0; // we will use DW_AT_specification
                   //to get name in debugger
                   //So do not assign name here.
 		   // and no src position: leave that to member
		 USRCPOS_clear(src);
        }
        // Field name should now have the class name and ::
        // prepended, per dwarf2
        // Save_Str2 can glob 2 pieces together.
        char *classname = Get_Name(context);
        if(classname && !class_func_found_member) {
           int len = strlen(classname);
           char* newname = new char[len+5
                        /* 5 makes room for :: and null char */];

           // no 0 check: let it crash to get signal handler msg.

           strcpy(newname,classname);
           strcpy(newname+len,"::");
           if(funcname) {
            // We do not need the string in a stable storage
            // area, but this is a convenient way to
            // concatenate strings and avoid a memory leak.
            funcname = Index_To_Str(Save_Str2(newname,funcname));
           }

           delete [] newname;
        }
    }

    linkage_name = ST_name(func_st);
    ST_IDX fstidx = ST_st_idx(func_st);
    if((Opt_Level >=2) && (fndecl))
      inlin = gs_decl_inline(fndecl)?DW_INL_inlined:DW_INL_not_inlined;
    else
      inlin = DW_INL_not_inlined;

    dst = DST_mk_subprogram(
        src,			// srcpos
        funcname,
        ret_dst,        	// return type
        DST_INVALID_IDX,        // Index to alias for weak is set later
        fstidx,                 // index to fe routine for st_idx
        inlin,                  // applies to C++
        DW_VIRTUALITY_none,     // applies to C++
        0,                      // vtable_elem_location
        FALSE,                  // is_declaration
#ifdef KEY
        FALSE,                  // is_artificial
#endif
        is_prototyped,           // 
        ! ST_is_export_local(func_st) );  // is_external
    // producer routines think we will set pc to fe ptr initially
    DST_RESET_assoc_fe (DST_INFO_flag(DST_INFO_IDX_TO_PTR(dst)));
    DST_append_child (current_scope_idx, dst);

    if(class_func_found_member) {
       DST_add_specification_to_subprogram(dst, class_func_idx);
    }

    if(linkage_name && funcname && strcmp(linkage_name,funcname)) {
      // we have a genuine linkage name.
      DST_add_linkage_name_to_subprogram(dst, linkage_name);
    }
    if( !DST_IS_NULL(DECL_DST_ABSTRACT_ROOT_IDX(fndecl))) {
	// FIXME need to record abstract root!
	DevWarn("Concrete function instance has abstract root! %s\n",
		basename);
    }


    if(fndecl) {
	DECL_DST_IDX(fndecl) =  dst;
    }

    // Now we create the argument info itself, relying
    // on the is_prototyped flag above to let us know if
    // we really should do this.
#ifndef KEY
    if(is_prototyped) {
#else
    // For function declarations like
    //    foo (i) int i; { ... }
    // isprotyped will be false but, we still want to generate DST info.
    if (fndecl) {
#endif /* KEY */
       gs_t parms = gs_decl_arguments(fndecl);
       if(!parms) {
	  // Normal function, no paramaters: in C++ this means
          // it has no arguments at all.
	  // And no dwarf should be emitted.
          // DevWarn("impossible arg decls -- is %s empty?",basename?basename:"");
       } else {
	   DST_enter_param_vars(fndecl, 
			dst,
			parms, 
			/* abstract_root = */ 0,
			/* declaration only = */ 0  );
       }

    }

    return dst;
}

DST_INFO_IDX
DST_Get_Comp_Unit (void)
{
   return comp_unit_idx;
}



//***********************************************************************
// Look in "lexical.h" at push_input_stack() to find out about directories
// and search paths.
//***********************************************************************
void
DST_build(int num_copts, /* Number of options passed to fec(c) */
	  char *copts[]) /* The array of option strings passed to fec(c) */
{
   char         *src_path, *comp_info;
#ifdef KEY
   char         *cur_dir = Get_Current_Working_Directory();

   current_working_dir = current_host_dir = cwd_buffer =
                      (char *) malloc (strlen(cur_dir) + MAXHOSTNAMELEN + 10);
#endif

   dst_initialized = TRUE;

   /* Initiate the memory system */
   DST_Init (NULL, 0);
   /* Enter the file-name as the first one in the file_list 
    * (DW_AT_name => src_path).  In the case that src_path should not
    * be an absolute path, we only need to eliminate the call to 
    * Make_Absolute_Path and we will get a path relative to the cwd.
    */

   if (Orig_Src_File_Name != NULL)
   {
     src_path = Orig_Src_File_Name;
   }

   /* Get the DW_AT_comp_dir attribute (current_host_dir) */
   if (Debug_Level > 0)
   {
      int host_name_length = 0;
      
      current_host_dir = &cwd_buffer[0];
      if (gethostname(current_host_dir, MAXHOSTNAMELEN) == 0)
      {
	 /* Host name is ok */
	 host_name_length = strlen(current_host_dir);
	 if(strchr(current_host_dir,'.')) {
	    // If hostname is already a FQDN (fully qualified
	    // domain name) don't add the domain again...
	    // Somehow.
	 } else {
	   current_host_dir[host_name_length] = '.';
	   if (getdomainname(&current_host_dir[host_name_length+1], 
			   MAXHOSTNAMELEN-host_name_length) == 0)
	   {
	    /* Domain name is ok */
	    host_name_length += strlen(&current_host_dir[host_name_length]);
	   }
         }

      }
      current_host_dir[host_name_length++] = ':';  /* Prefix cwd with ':' */
      current_working_dir = &cwd_buffer[host_name_length];
   }
   else /* No debugging */
   {
      current_host_dir = NULL;
      current_working_dir = &cwd_buffer[0];
   }
#ifdef KEY
   strcpy(current_working_dir, cur_dir);
#else
   strcpy(current_working_dir, Get_Current_Working_Directory());
#endif
   if (current_working_dir == NULL) {
      perror("getcwd");
      exit(2);
   }

   /* Get the AT_producer attribute! */
#ifndef KEY
   comp_info = DST_get_command_line_options(num_copts, copts);
#else
   comp_info = (char *)malloc(sizeof(char)*100);
   strcpy(comp_info, lang_cplus? "openCC ": "opencc ");
   if (INCLUDE_STAMP)
     strcat(comp_info, INCLUDE_STAMP);
#endif

   {
      // bug 12576: If available, use the original source file name.
      char * dump_base_name = Orig_Src_File_Name ? Orig_Src_File_Name :
                                                   Src_File_Name;
      comp_unit_idx = DST_mk_compile_unit(Last_Pathname_Component(dump_base_name),
					  current_host_dir,
					  comp_info, 
				lang_cplus ? DW_LANG_C_plus_plus : DW_LANG_C89,
					  DW_ID_case_sensitive);
   }

   free(comp_info);

   WGEN_Set_Line_And_File (0, Orig_Src_File_Name);
}


static const char *current_file_name = NULL;

void
WGEN_Set_Line_And_File (UINT line, const char* f, bool check)
{
	if (!dst_initialized) return;

        //bug 8895, 10632: check whether necessary to update
        //NOTE: we don't check for function entry related caller
        if(f == NULL) return;
        if(check && current_file_name)
         if(strcmp(current_file_name, f)==0)
           return;
        current_file_name = f;

        // We aren't really modifying f, this is just so we can
        // call legacy code.
        char* file = const_cast<char*>(f);

	// split file into directory path and file name
	char *dir;
	char *file_name = drop_path(file);;
	char * buf = (char *) alloca (strlen(file) + 1);
	if (file_name == file) {
		// no path
		dir = current_working_dir;
	}
	else if (strncmp(file, "./", 2) == 0) {
		// current dir
		dir = current_working_dir;
	}
	else {
		// copy specified path
		strcpy (buf, file);
		dir = drop_path(buf);	// points inside string, after slash
		--dir;			// points before slash
		*dir = '\0';		// terminate path string
		dir = buf;
	}

	current_dir = Get_Dir_Dst_Info (dir);
	current_file = Get_File_Dst_Info (file_name, current_dir);
}

#ifdef KEY
void WGEN_Macro_Define (UINT line, const char *buffer)
{
  DST_mk_macr(line, (char *)buffer, 1 /* DW_MACINFO_DEFINE */);
  return;
}

void WGEN_Macro_Undef (UINT line, const char *buffer)
{
  DST_mk_macr(line, (char *)buffer, 2 /* DW_MACINFO_UNDEF */);
  return;
}

void WGEN_Macro_Start_File (UINT line, UINT file)
{
  DST_mk_macr_start_file(line, file);
}

void WGEN_Macro_End_File (void)
{
  DST_mk_macr_end_file();
}
#endif
