/*

  Copyright (C) 2010, Hewlett-Packard Development Company, L.P. All Rights Reserved.

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
 * Module: flags.h
 * $Revision: 1.1 $
 * $Date: 2005/07/27 02:17:57 $
 * $Author: kevinlo $
 * $Source: /depot/CVSROOT/javi/src/sw/cmplr/common/util/flags.h,v $
 *
 * Revision history:
 *  08-Sep-89 - Original Version
 *  01-Feb-91 - Copied for TP/Muse
 *  23-May-93 - Added group support
 *  29-Jan-94 - Removed last non-generic info to config.h
 *
 * Description:
 *
 * External interface to the command line processing utilities for the
 * Ragnarok compiler.
 *
 * NOTE:  This interface should only be visible to modules involved in
 * configuration and other initialization.  More widely-visible
 * configuration options belong in config.h and related files.  See
 * config.h for a more complete discussion.
 *
 *
 * Exported types:
 *
 *	OPTION_GROUP
 *	    Describes a command-line "option group", a set of related
 *	    options with their own specification namespace.  Fields
 *	    are accessed by:
 *	     char *OGROUP_name(grp)	Name (e.g., "OPT") of the group.
 *					These need to be chosen carefully
 *					so they don't conflict with other
 *					command-line option names (e.g.,
 *					can't start with L or I without
 *					restricting directory names that
 *					can be specified with -L/-I).
 *					By convention, hierarchical group
 *					names consist of uppercase names
 *					separated by underscores.  (e.g.,
 *					CG_SWP provides SoftWare Pipelining
 *					options for Code Generation.)
 *	     OGROUP_DESC *OGROUP_options(grp)
 *					Array of option descriptors (see
 *					below), terminated by one with kind
 *					OVK_COUNT, describing the options
 *					provided in the group.  These exist
 *					within their own namespace, so names
 *					do not have to be chosen as carefully
 *					as the option group name.
 *	     char OGROUP_separator(grp)	Character used to separate options
 *					within the group (usually ':').
 *	     char OGROUP_valmarker(grp)	Character used to separate option
 *					names from values within the group
 *					(usually '=').
 *	     char *OGROUP_description(grp)
 *					One-line description of group.
 *
 *	OPTION_DESC
 *	    Describes an option with an option group.  Fields are accessed
 *	    by:
 *	     OPTION_KIND ODESC_kind(od)	Kind of option, as given below.
 *	     OPTION_VISIBILITY ODESC_visibility(od)
 *					Visibility of option, below.
 *	     char *ODESC_name(od)	Full verbose name of option.
 *	     char *ODESC_abbrev(od)	Shortest acceptable prefix allowed
 *					for abbreviating option.  MUST be
 *					a prefix of the full name.  If prefix
 *					of more than one opt name in group,
 *					ambiguity is resolved in favor of
 *					first appearance in option array.
 *					NULL means full option name must be
 *					used.  Empty string ("") means any
 *					non-ambiguous prefix allowed as an
 *					abbreviation.  Abbreviations that
 *					are not prefixes of the full option
 *					name should be implemented with their
 *					own OPTION_DESC (sharing the same
 *					variable).
 *	     INT64 ODESC_def_val(od)	Default value for numeric options.
 *	     INT64 ODESC_min_val(od)	Minimum value for numeric options.
 *	     INT64 ODESC_max_val(od)	Maximum value for numeric options.
 *	     void *variable		Pointer to variable to be set if
 *					option is given.
 *	     BOOL *specified		Pointer to Boolean value to set if
 *					option is given, or NULL if no such
 *					notification is necessary.  This is
 *					useful for seeing if options were
 *					explicitly set on the command line.
 *	     char *ODESC_description(od)
 *					One-line description of option.
 *	    The "variable" and "specified" are not accessible after the
 *	    initialization of the data structures.
 *
 *	OPTION_KIND
 *	    Enumerated type specifying the various types of options within
 *	    an option group.  Valid values are:
 *	     OVK_NONE		Option takes no value; its appearance
 *				will simply set the variable to TRUE
 *	     OVK_BOOL		Option takes Boolean value, defaulting
 *				to TRUE if no value given.  Valid values
 *				are (case-insignificant) "NO", "YES",
 *				"TRUE", "FALSE", "ON", "OFF", and "0".
 *	     OVK_INT32		Option takes INT32 value, defaulting
 *				to the specified default if no value
 *				given or if out of range.
 *	     OVK_INT64		Option takes INT64 value, defaulting
 *				to the specified default if no value
 *				given or if out of range.
 *	     OVK_UINT32		Option takes UINT32 value, defaulting
 *				to the specified default if no value
 *				given or if out of range.
 *	     OVK_UINT64		Option takes UINT64 value, defaulting
 *				to the specified default if no value
 *				given or if out of range.
 *	     OVK_NAME		Option takes string value, defaulting to
 *				the empty string ("") if no value given.
 *	     OVK_SELF		Option takes string value, defaulting to
 *				the option name if no value given.  This
 *				may be useful when you want several
 *				options to set the same variable.
 *	     OVK_LIST		Option takes list value, where the list
 *				contains the option names and values.
 *	     OVK_OBSOLETE	Option is obsolete.
 *	     OVK_UNIMPLEMENTED	Option is unimplemented.
 *	     OVK_REPLACED	Option is obsolete, replaced by another,
 *				named by its ODESC_variable field.
 *       OVK_ENUM       Options takes string value, which should be 
 *              one of the names defined for a enum, defaulting to
 *              the specified default value. It also has a default
 *              set value, which is used when a "" string is passed
 *              to the option.
 *	     OVK_COUNT		Dummy value used to mark end of option
 *				descriptor array.
 *	    TODO: 64-bit option values not yet implemented.
 *
 *	OPTION_VISIBILITY
 *	    Enumerated type specifying the visibility (to users) of
 *	    options within an option group.  Valid values are:
 *	     OVK_VISIBLE	Users will see option on full listing.
 *				Such options should have a valid
 *				ODESC_description field, and should
 *				appear in documentation.
 *	     OVK_SHY		Users will see option on listing only
 *				if they've set it or it's been
 *				implicitly set by one of their options
 *				(e.g. Ofast).
 *	     OVK_INTERNAL	This option does not appear on user
 *				listings.
 *
 *
 * Exported functions:
 *
 *	void Initialize_Option_Groups ( OPTION_GROUP *og )
 *	    Initialize auxiliary internal information for the given
 *	    array of option groups.  Will be called automatically by
 *	    the a call to Process_Command_Line_Group, but if some
 *	    options have their initial static defaults modified by
 *	    other means before that, calling this explicitly earlier
 *	    will properly identify those as modified on listings.
 *
 *	void Set_Option_Internal ( OPTION_GROUP *ogroup, const char *name );
 *	    Set the given option in the given group internal, meaning
 *	    that it won't appear on user listings.  If the option name
 *	    is NULL, the entire group is set internal.  This must be
 *	    called after initialization.
 *
 *	BOOL Process_Command_Line_Group ( char *flag, OPTION_GROUP *og )
 *	    Attempt to interpret <flag> as a command-line option group.
 *          <flag> should point just past the initial "-".  If <flag>
 *	    does not name an option group (as defined in og or Option_Groups,
 *	    see below), FALSE is returned.  Otherwise processes the group,
 *          setting the appropriate switches as specified in the group
 *	    desciption, possibly signalling errors or warnings for options
 *	    incorrectly specified.  Returns TRUE whenever <flags> named
 *	    a valid group.
 *
 *	void Print_Option_Group ( FILE *tf, OPTION_GROUP *og, const char *pfx,
 *				  BOOL internal, BOOL full, BOOL update)
 *	    Print the current settings of the 'og' flags to 'tf'.
 *	    Start lines with pfx (for comments in assembly source file
 *	    output, for example).  Identify internal use (tracing) vs.
 *	    customer output (listing files).  For internal use, 'full'
 *	    causes all options to be printed, not just set/modified
 *	    ones.  If 'update' is TRUE, internal set/modified flags are
 *	    cleared and those options won't be printed next time
 *	    (unless set again).
 *
 *	void Trace_Option_Group ( FILE *tf, OPTION_GROUP *og, BOOL full)
 *	    Trace the current settings of the 'og' flags to 'tf'.
 *
 *	Print_Option_Groups / Trace_Option_Groups
 *	    Same as the singular forms above, but print array of groups.
 *
 *	OPTION_GROUP *Get_Command_Line_Group ( OPTION_GROUP *og, const char *name )
 *	    Given an option group array and a group name, return a
 *	    pointer to the array element which has the name (or NULL).
 *
 *	INT32 Get_Numeric_Flag (
 *		char **cp,
 *		UINT32 min,
 *		UINT32 max,
 *		UINT32 def,
 *		char *flag)
 *	    Process a numeric flag from the command line.  <flag> is a
 *	    pointer to the whole flag as given on the command line.
 *	    <cp> points to a pointer to a position within <flag> at
 *	    which the numeric value is expected.  On exit,
 *	    point past the numeric value (if any).  <min> and <max>
 *	    give the bounds for the expected value, while <def> gives
 *	    the default value.  If either no numeric value is given,
 *	    or the value is outside the required range, the default is
 *	    returned (and a warning issued if outside the range); 
 *	    otherwise the number specified by the flag is returned.
 *
 *
 * Exported variables:
 *
 *      OPTION_GROUP Common_Option_Groups[]
 *	    List of option group descriptors common to all phases, terminated 
 *	    by a NULL entry.  Each phase can have its own list of other groups.
 *	    Statically initialized in config.c.  Option groups are modified
 *	    by changing this initial value.  After initialization, should
 *	    only be accessed by Process_Command_Line_Group.
 *
 *	char Cmdname[]
 *	    The compiler invocation command.
 *	    
 *
 * SEE ALSO:
 *
 *	com/config.h	General configuration options.
 *	com/controls.h	Flag/pragma-based compiler control options.
 *
 * ====================================================================
 * ====================================================================
 */

#ifndef flags_INCLUDED
#define flags_INCLUDED

#ifdef _KEEP_RCS_ID
static char *flags_rcs_id = "$Source: /depot/CVSROOT/javi/src/sw/cmplr/common/util/flags.h,v $ $Revision: 1.1 $";
#endif /* _KEEP_RCS_ID */

#ifdef __cplusplus
extern "C" {
#endif

extern char Cmdname[];

/* Interpret a numeric string such as '356' or '256': */
extern INT64 Get_Numeric_Flag (
  char **cp,	/* String to decode and advance */
  INT64 min,	/* Minimum valid value */
  INT64 max,	/* Maximum valid value */
  INT64 def,	/* Default value if none present */
  char *flag	/* Option string (for error messages) */
);

/* Interpret a string such as '356K' or '256g' as a numeric value: */
extern BOOL Atoi_KMG (	/* Returns whether string was valid */
  const char* s,	/* String to interpret */
  INT64* val,		/* Result of interpretation */
  BOOL suffix_required	/* Is a suffix required?  (Must be [kKmMgG]) */
);


/* Define the option kinds: */
typedef enum {
  OVK_INVALID = 0,
  OVK_NONE,	/* Option never takes a value */
  OVK_BOOL,	/* boolean value */
  OVK_INT32,	/* 32-bit integer value */
  OVK_INT64,	/* 64-bit integer value */
  OVK_UINT32,	/* 32-bit unsigned integer value */
  OVK_UINT64,	/* 64-bit unsigned integer value */
  OVK_NAME,	/* string value, defaulting to "" */
  OVK_SELF,	/* string value, defaulting to option name */
  OVK_LIST,	/* list of option name/value pairs */
  OVK_OBSOLETE,	/* Option is obsolete */
  OVK_OLD_COUNT,/* Used to be COUNT. Here to avoid a revision mismatch.
		 * Remove the reference in flags.c at some future time,
		 * later remove this with another if needed.
		 */
  OVK_REPLACED,	/* Option is obsolete, replaced by another */
  OVK_UNIMPLEMENTED,	/* Option is unimplemented */
  OVK_ENUM,
  
  OVK_COUNT=63	/* end of list marker */
} OPTION_KIND;

/* Define the option visibility: */
typedef enum {
  OV_VISIBLE,	/* Option freely visible to users */
  OV_SHY,	/* Option listed only if user explicitly sets it */
  OV_INTERNAL	/* Internal option never listed for users */
} OPTION_VISIBILITY;

/* Define the list returned for OVK_LIST: */
typedef struct option_list {
  struct option_list	*next;
  const char		*opt;
  char			*val;
} OPTION_LIST;

#define OLIST_next(o)	((o)->next)
#define OLIST_opt(o)	((o)->opt)
#define OLIST_val(o)	((o)->val)

/* Define an option descriptor: */
typedef struct option_desc {
  mINT8		kind;
  mINT8		visibility;
  BOOL		can_change_by_pragma; /* options pragma */
  const char *	name;
  const char *	abbrev;
  INT64		def_val;
  INT64		min_val;
  INT64		max_val;
  void *	variable;
  void *	aux;
  const char *	description;
} OPTION_DESC;

#define ODESC_kind(o)		((o)->kind)
#define ODESC_visibility(o)	((o)->visibility)
#define ODESC_can_change_by_pragma(o)	((o)->can_change_by_pragma)
#define ODESC_name(o)		((o)->name)
#define ODESC_abbrev(o)		((o)->abbrev)
#define ODESC_def_val(o)	((o)->def_val)
#define ODESC_min_val(o)	((o)->min_val)
#define ODESC_max_val(o)	((o)->max_val)
#define ODESC_variable(o)	((o)->variable)
#define ODESC_description(o)	((o)->description)

/* Define an option group descriptor: */
typedef struct option_group {
  const char *	name;		/* Group name */
  char	        separator;	/* Separator between sub-options */
  char		valmarker;	/* ... between option name and value */
  OPTION_DESC *	options;	/* Array of option descriptors */
  void *	aux;		/* Auxiliary info for internal use */
  char *	description;	/* Short description of group */
} OPTION_GROUP;

#define OGROUP_name(o)		((o)->name)
#define OGROUP_options(o)	((o)->options)
#define OGROUP_separator(o)	((o)->separator)
#define OGROUP_valmarker(o)	((o)->valmarker)
#define OGROUP_description(o)	((o)->description)

/* The option groups common to all compiler components: */
extern OPTION_GROUP Common_Option_Groups[];

/* Initialize auxiliary info for an array of OPTION_GROUPs: */
extern void Initialize_Option_Groups ( OPTION_GROUP *ogroups );

/* Set the given option in the given group internal, meaning that it
 * won't appear on user listings.  If the option name is NULL, the
 * entire group is set internal.
 */
extern void Set_Option_Internal ( OPTION_GROUP *ogroup, const char *name );

/* Process the given option group: */
extern BOOL Process_Command_Line_Group (
  char *flag,
  OPTION_GROUP *opt_groups );

/* Print/trace the settings for the given option group: */
extern void Print_Option_Group (
  FILE *tf,			/* Listing/trace file */
  OPTION_GROUP *opt_group,	/* Which group? */
  const char *prefix,			/* Prefix for output lines (comment) */
  BOOL internal,		/* Internal trace or user listing? */
  BOOL full,			/* All options or only set options? */
  BOOL update );		/* Update set/mod flags after list? */
extern void Trace_Option_Group (
  FILE *tf,			/* Trace file */
  OPTION_GROUP *opt_group,	/* Which group? */
  BOOL full );			/* All options or only set options? */

/* Same as the above, but print an array of groups: */
extern void Print_Option_Groups (
  FILE *tf,			/* Listing/trace file */
  OPTION_GROUP *opt_group,	/* Group array */
  const char *prefix,			/* Prefix for output lines (comment) */
  BOOL internal,		/* Internal trace or user listing? */
  BOOL full,			/* All options or only set options? */
  BOOL update );		/* Update set/mod flags after list? */
extern void Trace_Option_Groups (
  FILE *tf,			/* Trace file */
  OPTION_GROUP *opt_group,	/* Group array */
  BOOL full );			/* All options or only set options? */

/* Get a group from an array, given its name: */
extern OPTION_GROUP *Get_Command_Line_Group (
  OPTION_GROUP *og,
  const char *name );

extern void Trace_Command_Line_Group(FILE *, OPTION_GROUP *);

extern void Save_or_restore_options(char *, INT32, BOOL);

typedef enum 
{       
    OPT_invalid = -1,                                      
    OPT_common_first = 0,
    OPT_enable       = OPT_common_first,
    OPT_disable,
    OPT_dump_before,
    OPT_dump_after,
    OPT_trace,
    OPT_stats,
    OPT_skip_b,
    OPT_skip_a,
    OPT_common_last,
    OPT_component_first = OPT_common_last

} O64_COMMON_OPTION;

typedef enum
{
    TRACE_none = 0,
    TRACE_info,
    TRACE_minimal,
    TRACE_medium,
    TRACE_maximal
} TRACE_OPTION_KIND;

typedef enum
{
    DUMP_none =0,
    DUMP_ir,
    DUMP_cfg,
    DUMP_ssa,
    DUMP_vcg,
    DUMP_maximal
} DUMP_KIND;

typedef enum
{
    COMPONENT_invalid = -1,
    COMPONENT_first   =  0,
    COMPONENT_driver  = COMPONENT_first,
    COMPONENT_cg_gpo,
    COMPONENT_last

} O64_COMPONENT;

#ifdef __cplusplus
}
#endif
#endif /* flags_INCLUDED */
