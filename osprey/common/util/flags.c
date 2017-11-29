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
 * Module: flags.c
 * $Revision: 1.1 $
 * $Date: 2005/07/27 02:17:57 $
 * $Author: kevinlo $
 * $Source: /depot/CVSROOT/javi/src/sw/cmplr/common/util/flags.c,v $
 *
 * Revision history:
 *  17-Jun-91 - Original Version
 *
 * Description:
 *
 * Utilities for processing the input command line options.
 *
 * ====================================================================
 * ====================================================================
 */

#ifdef _KEEP_RCS_ID
static const char source_file[] = __FILE__;
static const char rcs_id[] = "$Source: /depot/CVSROOT/javi/src/sw/cmplr/common/util/flags.c,v $ $Revision: 1.1 $";
#endif

#include <string.h>
#include "defs.h"
#include "flags.h"
#include "erglob.h"
#include "tracing.h"		/* Is_Trace */
#include "config.h"		/* Opt_Level */

extern char *SBar;	/* Line of dashes from tracing.h */

/* ====================================================================
 *
 * Get_Numeric_Flag
 *
 * Read a numeric flag from the flag word passed via *cp.  Compare it
 * to the given bounds.  If it is out of bounds, generate a warning and
 * set it to the given default.  If no numeric digits are present, use
 * the given default.  Update *cp to point past the numeric value.
 *
 * ====================================================================
 */

INT64
Get_Numeric_Flag (
  char   **cp,	/* Pointer to the flag pointer */
  INT64   min,	/* Bounds and default for flag */
  INT64   max,
  INT64   def,
  char    *flag	/* Pointer to containing flag word */
)
{
  INT64 val = 0;
  INT64 radix = 10;
  char c;
  BOOL done = FALSE;
  BOOL negate = FALSE;

  /* First check for a number: */
  c = **cp;
  if (c == '-') {
      c = *++(*cp);
      negate = TRUE;
  }
  if ( c < '0' || c > '9' ) return def;

  /* Next get the radix: */
  if ( c == '0' ) {
    c = *++(*cp);
    if ( c == 'x' || c == 'X' ) {
      c = *++(*cp);
      radix = 16;
    } else radix = 8;
  }

  /* Finally get the number: */
  while ( ! done ) {
    switch ( c ) {
	case '8':
	case '9': if ( radix == 8 ) {
		    ErrMsg ( EC_Flag_Digit, c, flag );
		    return def;
		  }
	  	  /* no break */
	case '0':
	case '1':
	case '2':
	case '3':
	case '4':
	case '5':
	case '6':
	case '7': val = val*radix + (c-'0');
		  break;
	case 'a':
	case 'b':
	case 'c':
	case 'd':
	case 'e':
	case 'f': if ( radix != 16 ) done = TRUE;
		  else val = val*radix + 10 + (c-'a');
		  break;
	case 'A':
	case 'B':
	case 'C':
	case 'D':
	case 'E':
	case 'F': if ( radix != 16 ) done = TRUE;
		  else val = val*radix + 10 + (c-'A');
		  break;
	default:  done = TRUE;
	  	  break;
    }
    if ( ! done ) c = *++(*cp);
  }

  if (negate) val = -val;

  /* Check value against bounds: */
  if ( val >= min && val <= max ) return val;
  ErrMsg ( EC_Flag_Range, val, min, max, flag );
  return def;
}

/* ====================================================================
 *
 * Atoi_KMG
 *
 * Return TRUE if string s contains a valid numerical representation,
 * with suffix 'k' or 'K' meaning kilo, 'm' or 'M' meaning mega, and
 * 'g' or 'G' meaning giga (e.g. bytes).  If suffix_required is true,
 * the string must end in k,K,m,M or g,G -- otherwise it's optional
 * (but value 0 is always legal).
 *
 * Returns the interpreted value of the string in val, and a BOOL
 * result indicating whether a valid value was found.
 *
 * ====================================================================
 */

BOOL
Atoi_KMG ( const char* s, INT64* val, BOOL suffix_required )
{
  BOOL  seen_digit = FALSE;
  INT64 v = 0;

  for (;;) {
    char  c;
    switch (c = *s++) {
     case 'k':
     case 'K':
      *val = v * 1024;
      return seen_digit;
     case 'm':
     case 'M':
      *val = v * (1024 * 1024);
      return seen_digit;
     case 'g':
     case 'G':
      *val = v * (1024 * 1024 * 1024);
      return seen_digit;
     default:
      if (c >= '0' && c <= '9') {
        v = v*10 + c - '0';
        seen_digit = TRUE;
      } else {
        *val = v;
        return seen_digit && (!suffix_required || v == 0);
      }
    }
  }
}

/* ====================================================================
 *
 * Option descriptor and group descriptor auxiliary information
 *
 * The descriptors used in client code to identify group options are
 * augmented here internally to allow general-purpose printing for
 * trace or listing purposes.
 *
 * ====================================================================
 */

/* Upon initialization, we augment the option descriptor with a
 * collection of status information.  A pointer to this structure, once
 * created, replaces the user's "specified" pointer in the original
 * structure, which moves to the auxiliary struct.
 */
#define ODESC_orig_specified(o)	((o)->aux)
#define ODESC_aux(o)		((ODESC_AUX *)((o)->aux))
#define Set_ODESC_aux(o,a)	(((o)->aux)=(ODESC_AUX *)(a))

typedef union optval {
  UINT64 i;
  void  *p;
} OPTVAL;

typedef union optvaltypes {
  BOOL   b;
  UINT32 ui32;
  INT32  i32;
  UINT64 ui64;
  INT64  i64;
  void  *p;
} OPTVALTYPES;

typedef struct odesc_aux {
  INT16 flags;		/* Various status flags */
  BOOL	*specified;	/* The option has been specified (user's) */
  OPTION_DESC *primary;	/* Primary option for ODK_LIST option */
  OPTVAL orig;		/* Original value */
  OPTVAL last;		/* Last value */
} ODESC_AUX;

#define ODF_SET_USER	0x01	/* Set since a user print */
#define ODF_SET_INT	0x02	/* Set since a internal print */
#define ODF_SET		0x03	/* Set (for setting only) */
#define ODF_MOD_USER	0x04	/* Modified since a user print */
#define ODF_MOD_INT	0x08	/* Modified since a internal print */
#define ODF_MOD		0x0c	/* Modified (for setting only) */
#define ODF_PRINT	0x10	/* Print (because set/modified) */
#define ODF_PRINTED	0x20	/* Printed (for ODK_LIST options) */

#define ODA_flags(o)		((o)->flags)
#define ODA_specified(o)	((o)->specified)
#define ODESC_specified(o)	ODA_specified(ODESC_aux(o))
#define ODA_primary(o)		((o)->primary)
#define ODESC_primary(o)	ODA_primary(ODESC_aux(o))
#define ODA_orig(o)		((o)->orig)
#define ODA_orig_i(o)		((o)->orig.i)
#define ODA_orig_p(o)		((o)->orig.p)
#define ODA_last(o)		((o)->last)
#define ODA_last_i(o)		((o)->last.i)
#define ODA_last_p(o)		((o)->last.p)
#define ODA_set_user(o)		(ODA_flags(o) & ODF_SET_USER)
#define Set_ODA_set_user(o)	(ODA_flags(o) |= ODF_SET_USER)
#define Reset_ODA_set_user(o)	(ODA_flags(o) &= ~ODF_SET_USER)
#define ODA_set_int(o)		(ODA_flags(o) & ODF_SET_INT)
#define Set_ODA_set_int(o)	(ODA_flags(o) |= ODF_SET_INT)
#define Reset_ODA_set_int(o)	(ODA_flags(o) &= ~ODF_SET_INT)
#define Set_ODA_set(o)		(ODA_flags(o) |= ODF_SET)
#define ODA_mod_user(o)		(ODA_flags(o) & ODF_MOD_USER)
#define Set_ODA_mod_user(o)	(ODA_flags(o) |= ODF_MOD_USER)
#define Reset_ODA_mod_user(o)	(ODA_flags(o) &= ~ODF_MOD_USER)
#define ODA_mod_int(o)		(ODA_flags(o) & ODF_MOD_INT)
#define Set_ODA_mod_int(o)	(ODA_flags(o) |= ODF_MOD_INT)
#define Reset_ODA_mod_int(o)	(ODA_flags(o) &= ~ODF_MOD_INT)
#define Set_ODA_mod(o)		(ODA_flags(o) |= ODF_MOD)
#define ODA_print(o)		(ODA_flags(o) & ODF_PRINT)
#define Set_ODA_print(o)	(ODA_flags(o) |= ODF_PRINT)
#define Reset_ODA_print(o)	(ODA_flags(o) &= ~ODF_PRINT)
#define ODA_printed(o)		(ODA_flags(o) & ODF_PRINTED)
#define Set_ODA_printed(o)	(ODA_flags(o) |= ODF_PRINTED)
#define Reset_ODA_printed(o)	(ODA_flags(o) &= ~ODF_PRINTED)

/* Similarly, we augment the group descriptor with some auxiliary
 * information, including the ODESC auxiliary array:
 */
#define OGROUP_aux(o)		((OGROUP_AUX *)((o)->aux))
#define Set_OGROUP_aux(o,a)	(((o)->aux)=(OGROUP_AUX *)(a))

typedef struct ogroup_aux {
  mINT16 flags;		/* Status flags */
  mINT16 count;		/* Number of options */
  ODESC_AUX *odesc_aux;	/* Auxiliary option descriptor array */
} OGROUP_AUX;

#define OGF_SET		0x01	/* Something has been set */
#define OGF_INTERNAL	0x02	/* Entire group is internal */

#define OGA_flags(o)		((o)->flags)
#define OGA_count(o)		((o)->count)
#define OGA_odesc_aux(o)	((o)->odesc_aux)
#define OGA_set(o)		(OGA_flags(o) & OGF_SET)
#define Set_OGA_set(o)		(OGA_flags(o) |= OGF_SET)
#define Reset_OGA_set(o)	(OGA_flags(o) &= ~OGF_SET)
#define OGA_internal(o)		(OGA_flags(o) & OGF_INTERNAL)
#define Set_OGA_internal(o)	(OGA_flags(o) |= OGF_INTERNAL)
#define Reset_OGA_internal(o)	(OGA_flags(o) &= ~OGF_INTERNAL)

/* ====================================================================
 * OVK_Scalar_Kind
 * return TRUE if OPTION_KIND is a scalar.
 * ====================================================================*/
static BOOL
OVK_Scalar_Kind(OPTION_KIND o)
{
  return (o == OVK_NONE || o == OVK_BOOL || o == OVK_INT32
          || o == OVK_UINT32 || o == OVK_INT64 || o == OVK_UINT64);
}

/* ====================================================================
 * OVK_Pointer_Kind
 * return TRUE if OPTION_KIND is a pointer.
 * ====================================================================*/
static BOOL
OVK_Pointer_Kind(OPTION_KIND o)
{
  return (o == OVK_SELF || o == OVK_NAME || o == OVK_LIST);
}

/* ====================================================================
 * Get_OVK_Size
 * return size of the different OPTION_KIND's.
 * ====================================================================*/
static INT
Get_OVK_Size(OPTION_KIND o)
{
  switch (o) {
    case OVK_NONE:
    case OVK_BOOL:
      return sizeof(BOOL);
    case OVK_INT32:
      return sizeof(INT32);
    case OVK_UINT32:
      return sizeof(UINT32);
    case OVK_INT64:
      return sizeof(INT64);
    case OVK_UINT64:
      return sizeof(UINT64);
    case OVK_NAME:
    case OVK_SELF:
      return sizeof(char *);
    case OVK_LIST:
      return sizeof(OPTION_LIST *);
    default: /* INVALID, OBSOLETE, REPLACED, UNIMPLEMENTED */
      return 0;
  }
}

/* ====================================================================
 * Get_OVK_UINT64_Val
 * return UINT64 value for pointer to OVK variable of type o.
 * ====================================================================*/
static UINT64
Get_OVK_UINT64_Val(void *p, OPTION_KIND o)
{
  char b[1024];
  OPTVALTYPES v;

  if (Get_OVK_Size(o) > 0)
    memmove(&v, p, Get_OVK_Size(o));

  switch (o) {
    case OVK_NONE:
    case OVK_BOOL:
      return (UINT64) v.b;
    case OVK_INT32:
      return (UINT64) v.i32;
    case OVK_UINT32:
      return (UINT64) v.ui32;
    case OVK_INT64:
      return (UINT64) v.i64;
    case OVK_UINT64:
      return v.ui64;
    default: /* INVALID, OBSOLETE, REPLACED, UNIMPLEMENTED */
      return 0;
  }
}

/* ====================================================================
 * Get_OVK_Pointer_Val
 * return pointer value for pointer to OVK variable of type o.
 * ====================================================================*/
static void *
Get_OVK_Pointer_Val(void *p, OPTION_KIND o)
{
  OPTVALTYPES v;
  if (OVK_Pointer_Kind(o)) {
    memmove(&v, p, Get_OVK_Size(o));
    return (void *) v.p;
  }
  else
    return NULL;
}

/* ====================================================================
 * Copy_option
 * underlying routine to copy odesc's to and from memory
 * returns number of bytes copied
 * ====================================================================*/
static INT
Copy_option(OPTION_DESC *odesc, char *container, BOOL save)
{
  void *var = ODESC_variable(odesc);
  INT size = Get_OVK_Size(ODESC_kind(odesc));

  Is_True(ODESC_can_change_by_pragma(odesc),
	  ("Copy_option, trying to copy option that cannot change"));

  if (size > 0) {
    if (save)
      memmove(container, var, size);
    else /* restore */
      memmove(var, container, size);
  }

  return size;
}


/* ====================================================================
 *
 * Duplicate_Value
 *
 * Given an option descriptor, duplicate the current value of the
 * associated user variable into the given container.  The container
 * passed must be 64 bits in size.  Pointers are simply replicated
 * (i.e. not their pointees), and are stored in the initial part of the
 * container.  Values smaller than 64 bits are converted to 64 bits for
 * storage in the container.
 *
 * NOTE: this routine is different from Copy_option in that it puts
 * the values in the OPTVAL container and so sign extends.
 * ====================================================================*/
static void
Duplicate_Value ( OPTION_DESC *odesc, OPTVAL *container )
{
  void *var = ODESC_variable(odesc);

  if (OVK_Scalar_Kind(ODESC_kind(odesc)))
    container->i = Get_OVK_UINT64_Val(var, ODESC_kind(odesc));
  else if (OVK_Pointer_Kind(ODESC_kind(odesc)))
    container->p = Get_OVK_Pointer_Val(var, ODESC_kind(odesc));
}


/* ====================================================================
 *
 * Initialize_Option_Group
 *
 * Given an option group descriptor, initialize all the auxiliary
 * information associated with it and its option descriptors.
 *
 * This routine should be called after any of the component options are
 * initialized (which is normally static and therefore not an issue),
 * and before we start reading options to make sure that the defaults
 * that we collect are valid.  However, we simply call it upon any
 * check of the group to make sure that everything works with existing
 * code.
 *
 * ====================================================================
 */

static void
Initialize_Option_Group ( OPTION_GROUP *ogroup )
{
  OGROUP_AUX *ogaux;
  INT16 count, i;
  OPTION_DESC *odesc;
  ODESC_AUX *odaux;

  /* If we've already done it, never mind: */
  if ( OGROUP_aux(ogroup) != NULL ) return;

  /* Count the descriptors in the group, including terminator: */
  count = 1;
  for ( odesc = OGROUP_options(ogroup);
	ODESC_kind(odesc) != OVK_COUNT
     && ODESC_kind(odesc) != OVK_OLD_COUNT;
	++odesc )
  {
    ++count;
  }

  /* Allocate the auxiliary descriptors: */
  ogaux = (OGROUP_AUX *) calloc ( 1, sizeof (OGROUP_AUX) );
  if ( ogaux == NULL ) {
    ErrMsg ( EC_No_Mem, "Initialize_Option_Group: OGROUP_aux" );
  }
  Set_OGROUP_aux ( ogroup, ogaux );
  odaux = (ODESC_AUX *) calloc ( count, sizeof (ODESC_AUX) );
  if ( odaux == NULL ) {
    ErrMsg ( EC_No_Mem, "Initialize_Option_Group: ODESC_aux" );
  }
  OGA_odesc_aux(ogaux) = odaux;
  OGA_count(ogaux) = count-1;	/* Not including terminator */

  /* Initialize the auxiliary option descriptors: */
  for ( i = 0, odesc = OGROUP_options(ogroup);
	i < count;
	++i, ++odesc, ++odaux )
  {
    ODA_specified(odaux) = ODESC_orig_specified(odesc);
    Set_ODESC_aux ( odesc, odaux );
    Duplicate_Value ( odesc, &ODA_orig(odaux) );
    ODA_last(odaux) = ODA_orig(odaux);
  }

  /* Special initialization for OVK_LIST options.  Several such options
   * may use the same variable.  So we make the ODA_primary field of
   * the first one's aux descriptor point to its ODESC, and then we
   * use the same aux descriptor for all the others.  Exception: if
   * the secondary has a different override variable than the primary,
   * they can't share since that moves to the auxiliary descriptor.
   *
   * Do the same for OVK_NAME and OVK_SELF options, taking care of
   * situations like -TARG:isa=mips3 and -TARG:mips3 using the same
   * variable.
   */
  for ( odesc = OGROUP_options(ogroup);
	ODESC_kind(odesc) != OVK_COUNT
     && ODESC_kind(odesc) != OVK_OLD_COUNT;
	++odesc )
  {
    if ( ODA_primary ( ODESC_aux(odesc) ) == NULL
      && ( ODESC_kind(odesc) == OVK_LIST
	|| ODESC_kind(odesc) == OVK_NAME
	|| ODESC_kind(odesc) == OVK_SELF ) )
    {
      OPTION_DESC *sdesc;

      ODA_primary ( ODESC_aux(odesc) ) = odesc;
      for ( sdesc = odesc+1;
	    ODESC_kind(sdesc) != OVK_COUNT
	 && ODESC_kind(sdesc) != OVK_OLD_COUNT;
	    ++sdesc )
      {
	if ( ODESC_variable(sdesc) == ODESC_variable(odesc)
	  && ODESC_specified(sdesc) == ODESC_specified(odesc) )
	{
	  Set_ODESC_aux ( sdesc, ODESC_aux(odesc) );
	}
      }
    }
  }
}

/* ====================================================================
 *
 * Set_Option_Internal
 *
 * Set the given option in the given group internal, meaning that it
 * won't appear on user listings.  If the option name is NULL, the
 * entire group is set internal.
 *
 * ====================================================================
 */

void
Set_Option_Internal ( OPTION_GROUP *ogroup, const char *name )
{
  if ( name == NULL ) {
    Set_OGA_internal ( OGROUP_aux (ogroup) );
    return;
  } else {
    OPTION_DESC *o;

    for ( o = OGROUP_options(ogroup);
	  ODESC_kind(o) != OVK_COUNT
       && ODESC_kind(o) != OVK_OLD_COUNT;
	  ++o )
    {
      if ( strcasecmp ( name, ODESC_name(o) ) == 0 ) {
	ODESC_visibility(o) = OV_INTERNAL;
      }
    }
  }
}

/* ====================================================================
 *
 * Initialize_Option_Groups
 *
 * Given an option group descriptor array, initialize all the auxiliary
 * information associated with the groups in the array.
 *
 * This routine should be called after any of the component options are
 * initialized (which is normally static and therefore not an issue),
 * and before we start reading options to make sure that the defaults
 * that we collect are valid.  However, we simply call it upon any
 * check of the group to make sure that everything works with existing
 * code.
 *
 * ====================================================================
 */

void
Initialize_Option_Groups ( OPTION_GROUP *ogroups )
{
  OPTION_GROUP *group;

  for (group = ogroups; group && OGROUP_options(group); group++) {
    Initialize_Option_Group ( group );

    /* We may terminate with an null name list of dummy options: */
    if ( OGROUP_name(group) == NULL ) break;
  }
}

/* ====================================================================
 *
 * Update_Scalar_Value
 *
 * Given an option descriptor for a scalar option type, update the
 * current value to that given.  First identify whether this is a
 * modified value (relative to ODA_last) and if so mark the option
 * modified and reset ODA_last.  Then mark the option set.  Finally,
 * replace the option value.
 *
 * WARNING:  The caller is responsible for verifying that the option
 * being updated is one of the scalar kinds.
 *
 * ====================================================================
 */

static void
Update_Scalar_Value ( OPTION_DESC *odesc, UINT64 val )
{
  void *var = ODESC_variable(odesc);
  ODESC_AUX *aux = ODESC_aux(odesc);
  OPTVALTYPES v;

  if ( val != ODA_last_i(aux) ) {
    Set_ODA_mod(aux);
    ODA_last_i(aux) = val;
  }
  Set_ODA_set(aux);

  switch ( ODESC_kind(odesc) ) {
    case OVK_NONE:
    case OVK_BOOL:
      v.b = (BOOL)val;
      break;
    case OVK_INT32:
      v.i32 = (INT32)val;
      break;
    case OVK_UINT32:
      v.ui32 = (UINT32)val;
      break;
    case OVK_INT64:
      v.i64 = (UINT64)val;
      break;
    case OVK_UINT64:
      v.ui64 = (UINT64)val;
      break;
    default:
      break;
  }
  if (Get_OVK_Size(ODESC_kind(odesc)) > 0) 
    memmove(var, &v, Get_OVK_Size(ODESC_kind(odesc)));
}

/* ====================================================================
 *
 * Update_Pointer_Value
 *
 * Given an option descriptor for a pointer option type, update the
 * current value to that given.  First identify whether this is a
 * modified value (relative to ODA_last) and if so mark the option
 * modified and reset ODA_last.  Then mark the option set.  Finally,
 * replace the option value.
 *
 * WARNING:  The caller is responsible for verifying that the option
 * being updated is one of the pointer kinds.
 *
 * ====================================================================
 */

static void
Update_Pointer_Value ( OPTION_DESC *odesc, void *val )
{
  void **var = (void **)ODESC_variable(odesc);
  ODESC_AUX *aux = ODESC_aux(odesc);

  if ( val != ODA_last_p(aux) ) {
    Set_ODA_mod(aux);
    ODA_last_p(aux) = val;
  }
  Set_ODA_set(aux);
  *var = val;
}

/* ====================================================================
 *
 * Process_Command_Line_Group
 *
 * See header file flags.h for description.
 *
 * ====================================================================
 */

BOOL
Process_Command_Line_Group (char *flag, OPTION_GROUP *opt_groups)
{
    OPTION_GROUP *group;
    char *option, *copy, *cp, *next_cp;
    size_t tidx;
    char sep[3];
    BOOL hasval;
    char *val = NULL;
    BOOL bval;
    INT64 ival;
    OPTION_DESC *odesc, *found, *first_found;
    OPTION_LIST *olist, *ol;

    /* Just in case: */
    Initialize_Option_Groups ( opt_groups );

    /* See whether flag names an option group */
    option = NULL;
    for (group = opt_groups; group && OGROUP_name(group); group++) {
	register INT group_name_len;

	if (flag[0] != *OGROUP_name(group))
	    continue;
      
	group_name_len = strlen(OGROUP_name(group));
      
	if (strncmp(flag, OGROUP_name(group), group_name_len) == 0 &&
	    flag[group_name_len] == OGROUP_separator(group)) {
	    /* We have a match.  Point to first option char
	     * and quit searching.
	     */
	    option = flag + group_name_len + 1;
	    break;
	}
    }

    /* Return failure indication if flag doesn't name an option group */
    if (option == NULL)
	return FALSE;

    cp = copy = strdup ( option );	/* Make a permanent, modifiable copy */
    next_cp = cp;

    /* Set up a separator string: */
    sep[0] = OGROUP_valmarker(group);
    sep[1] = OGROUP_separator(group);
    sep[2] = 0;

#ifdef Is_True_On
    /* Configuration check: make sure each non-NULL abbreviation is
     * actual prefix of option name.  Otherwise, we'll never match.
     */
    for ( odesc=OGROUP_options(group);
	  ODESC_kind(odesc) != OVK_COUNT
       && ODESC_kind(odesc) != OVK_OLD_COUNT;
	  odesc++)
    {
      const char *abbrev = ODESC_abbrev(odesc);
      const char *name = ODESC_name(odesc);
      Is_True ( abbrev == NULL
	     || strncmp(abbrev, name, strlen(abbrev)) == 0,
		( "Option group (%s) configuration error: "
		  "'%s' not prefix of '%s'",
		  OGROUP_name(group), abbrev, name ) );
    }
#endif

    while ( next_cp != NULL ) {
        char this_flag[256];

	cp = next_cp;

	/* Find end of name: */
	tidx = strcspn ( cp, sep );
	next_cp = ( cp[tidx] == 0 ) ? NULL : cp+tidx+1;

	/* If it's a value, extract it: */
	hasval = ( cp[tidx] == OGROUP_valmarker(group) );
	cp[tidx] = 0;
	if ( hasval ) {
	    val = next_cp;
	    /* Don't consider valmarker this time: */
	    tidx = strcspn ( val, &sep[1] );
	    next_cp = ( val[tidx] == 0 ) ? NULL : val+tidx+1;
	    val[tidx] = 0;
	}

	/* Construct string containing only the current flag,
	 * with group name for context.  This lets us emit
	 * clearer error messages.
	 */
	sprintf(this_flag, "%.50s%c%.100s", OGROUP_name(group),
		OGROUP_separator(group), cp);
	if (hasval)
	  sprintf(this_flag+strlen(this_flag), "%c%.100s",
		  OGROUP_valmarker(group), val);

	/* Find the option descriptor: */
	first_found = found = NULL;
	for ( odesc = OGROUP_options(group);
	      !found
	   && ODESC_kind(odesc) != OVK_COUNT
	   && ODESC_kind(odesc) != OVK_OLD_COUNT;
	     odesc++ )
	{
	    /* Matching rules.  If abbreviation is:
	     * (1) NULL - only whole name matches.
	     * (2) Empty string - any non-ambiguous prefix of name matches.
	     * (3) Any other prefix of name - anything between abbrev and whole
	     *     name matches.
	     */
	    if (!ODESC_abbrev(odesc)) {
		/* Rule (1) */
		if (strcasecmp(cp, ODESC_name(odesc)) == 0) found = odesc;
	    } else {
		INT32 alen = strlen(ODESC_abbrev(odesc));
		INT32 clen = strlen(cp);
		if (alen == 0)  {
		    /*
		     * Rule (2).  If matching, start ambiguity check
		     * by setting first_found, unless we're already
		     * checking for ambiguity.
		     */
		    if (strncasecmp(cp, ODESC_name(odesc), clen) == 0) {
			if (!first_found) first_found = odesc;
			else found = odesc;
		    }
		} else {
		    /* Rule (3) */
		    if (strncasecmp(ODESC_abbrev(odesc), cp, alen) == 0 &&
			strncasecmp(cp, ODESC_name(odesc), clen) == 0)
			found = odesc;
		}
	    }
	}

	/* Finish ambiguity check */
	if (first_found) {
	    if (found) {
		ErrMsg(EC_Ambig_In_Grp, cp, OGROUP_name(group), this_flag);
		continue;
	    }
	    found = first_found;
	}

	if ( found == NULL ) {
	    /* Complain: */
	    ErrMsg ( EC_Not_In_Grp, cp, OGROUP_name(group), this_flag );
	} else {
	    ODESC_AUX *aux = ODESC_aux(found);

	    /* Set specified flag if given */
	    if ( ODA_specified(aux) ) *ODA_specified(aux) = TRUE;

	    switch ( ODESC_kind(found) ) {
	    case OVK_NONE:
		if ( hasval ) {
		    ErrMsg (EC_Inv_Grp_Val, cp, OGROUP_name(group), val,
			    this_flag);
		}
		Update_Scalar_Value ( found, (UINT64)TRUE );
		break;

	    case OVK_OBSOLETE:
		ErrMsg ( EC_Obsolete_Opt, this_flag );
		break;

	    case OVK_REPLACED:
		ErrMsg ( EC_Replaced_Opt, this_flag,
			 (char *)ODESC_variable(found) );
		break;

	    case OVK_UNIMPLEMENTED:
		ErrMsg ( EC_Unimp_Opt, this_flag );
		break;

	    case OVK_BOOL:
		if ( hasval ) {
		    bval = ( strcasecmp ( val, "ON" ) == 0
			  || strcasecmp ( val, "TRUE" ) == 0
			  || strcasecmp ( val, "YES" ) == 0
			  || ( strcasecmp ( val, "OFF" ) != 0
			    && strcasecmp ( val, "FALSE" ) != 0
			    && strcasecmp ( val, "NO" ) != 0
			    && strcmp ( val, "0" ) != 0 ) );
		} else {
		    bval = TRUE;
		}
		Update_Scalar_Value ( found, (UINT64)bval );
		break;

	    case OVK_INT32:
	    case OVK_INT64:
	    case OVK_UINT64:
	    case OVK_UINT32:
		if ( ! hasval ) {
		    ival = ODESC_def_val(found);
		} else {
		    char *tval = val;
		    if ((*val < '0' || *val > '9') && *val != '-')
		      ErrMsg(EC_Flag_Int_Expected, this_flag);
		    ival = Get_Numeric_Flag ( &tval,
					     ODESC_min_val(found),
					     ODESC_max_val(found),
					     ODESC_def_val(found),
					     this_flag );
		}
		Update_Scalar_Value ( found, (UINT64)ival );
		break;


	    case OVK_NAME:
		if ( ! hasval ) {
		    val = "";
		}
		Update_Pointer_Value ( found, strdup (val) );
		break;

	    case OVK_SELF:
		if ( ! hasval ) {
		    val = cp;
		}
		Update_Pointer_Value ( found, strdup (val) );
		break;

	    case OVK_LIST:
		olist =
		    (OPTION_LIST *) calloc ( sizeof(OPTION_LIST), 1 );
		OLIST_val(olist) = val ? strdup ( val ) : NULL;
		OLIST_opt(olist) = ODESC_name(found);
		ol = *(OPTION_LIST **)ODESC_variable(found);

		if ( ol == NULL ) {
		  Update_Pointer_Value ( found, olist );

		} else {
		  while ( OLIST_next(ol) != NULL ) ol = OLIST_next(ol);
		  OLIST_next(ol) = olist;

		  /* This won't actually change the apparent value of
		   * ODA_last, since the head of the list isn't
		   * changing.  So, just mark it modified and set:
		   */
		  Set_ODA_mod ( aux );
		  Set_ODA_set ( aux );
		}
		break;

	    default:
		break;
	    }
	}
    }

    free(copy);
    /* Return success indicator */
    return TRUE;
} /* Process_Command_Line_Group */

/* ====================================================================
 *
 * Modified_Option
 *
 * Given an option descriptor, determine whether the option has been
 * modified since the ODA_last value was set, i.e. without using
 * Process_Command_Line_Group.
 *
 * WARNING:  Because we want calls to this routine to be repeatable, we
 * don't update ODA_last.  Therefore, multiple calls to the Print/Trace
 * routines may print this option even though it doesn't change between
 * them.
 *
 * ====================================================================
 */

static BOOL
Modified_Option ( OPTION_DESC *odesc )
{
  void *var = ODESC_variable(odesc);
  ODESC_AUX *aux = ODESC_aux(odesc);
  UINT64 cur;

  if (OVK_Scalar_Kind(ODESC_kind(odesc)))
    return (Get_OVK_UINT64_Val(var, ODESC_kind(odesc)) != ODA_last_i(aux));
  else if (OVK_Pointer_Kind(ODESC_kind(odesc)))
    return (Get_OVK_Pointer_Val(var, ODESC_kind(odesc)) != ODA_last_p(aux));
  else
    return FALSE;
}

/* ====================================================================
 *
 * Print_Option_Group
 *
 * Given an option group descriptor, print the options in the group
 * to the given file.  Each line of output is prefixed by "prefix",
 * to allow commenting for emission in assembly sources, for instance.
 * The set and mod flags are updated appropriately if "update" is TRUE;
 * this allows them to be left unchanged if the information is to be
 * printed to two files (e.g. listing and assembly).
 *
 * Which options are printed depends on the "internal" and "full"
 * parameters, the OGA_internal flag of the group, and the
 * ODESC_visibility value for the option, as indicated by the
 * following:
 *   internal == TRUE:
 *	full == TRUE:	print all options
 *	full == FALSE:	print only options modified since last internal
 *			listing
 *   internal == FALSE: (print no options if OGA_internal is set)
 *	full == TRUE:	print OV_VISIBLE and modified OV_SHY options
 *	full == FALSE:	print only OV_VISIBLE or OV_SHY options
 *			modified since last non-internal listing
 *
 * ====================================================================
 */

void
Print_Option_Group ( FILE *tf, OPTION_GROUP *og, const char *prefix,
		     BOOL internal, BOOL full, BOOL update )
{
  OPTION_DESC *desc = OGROUP_options(og);
  ODESC_AUX *aux;
  OPTION_LIST **this;
  OPTION_LIST *list;
  BOOL option_set = FALSE;
  BOOL visible_option = FALSE;
  char *bar = SBar + 12;	/* shorten it a bit */

  /* Don't print internal groups in user mode: */
  if ( !internal && OGA_internal ( OGROUP_aux(og) ) ) return;

  /* Clear the ODA_PRINTED flags and check for set options: */
  for ( desc = OGROUP_options(og);
	ODESC_kind(desc) != OVK_COUNT
     && ODESC_kind(desc) != OVK_OLD_COUNT;
	desc++ )
  {
    aux = ODESC_aux(desc);
    Reset_ODA_printed ( aux );
    if ( ( internal
	&& ( ODA_set_int(aux) || Modified_Option(desc) ) )
      || ( ! internal
	&& ODESC_visibility(desc) != OV_INTERNAL
	&& ( ODA_set_user(aux) || Modified_Option(desc) ) ) )
    {
      Set_ODA_print(aux);
      option_set = TRUE;
    } else {
      Reset_ODA_print(aux);
      if ( ODESC_visibility(desc) != OV_INTERNAL ) {
	visible_option = TRUE;
      }
    }
  }

  /* Is there anything to print? */
  if ( ! ( option_set || full ) ) return;
  if ( ! ( option_set || internal || visible_option ) ) return;

  /* Print a header: */
  fprintf ( tf, "\n%s%s%s -%s Option Group\n",
	    prefix, bar, prefix, OGROUP_name(og) );
  if ( OGROUP_description(og) != NULL ) {
    fprintf ( tf, "%s\t%s\n", prefix, OGROUP_description(og) );
  }
  fprintf ( tf, "%s%s", prefix, bar );

  for ( desc = OGROUP_options(og);
	ODESC_kind(desc) != OVK_COUNT
     && ODESC_kind(desc) != OVK_OLD_COUNT;
	desc++ )
  {
    char mchar;

    aux = ODESC_aux(desc);
    if ( ! ODA_print(aux) && ! full ) continue;
    if ( ! internal && ODESC_visibility(desc) == OV_INTERNAL ) continue;

    /* Mark modified options with '#', set options with '=': */
    if ( ( ! internal && (ODA_mod_user(aux) || Modified_Option(desc) ) )
      || ( internal && (ODA_mod_int(aux) || Modified_Option(desc) ) ) )
    {
      mchar = '#';
    } else if ( ( ! internal && ODA_set_user(aux) )
	     || (   internal && ODA_set_int(aux) ) )
    {
      mchar = '=';
    } else {
      mchar = ' ';
    }

    fprintf ( tf, "%s%c %-20s= ", prefix, mchar, ODESC_name(desc));

    switch (ODESC_kind(desc)) {
    case OVK_BOOL:
	fprintf ( tf, "%s", *((BOOL *) ODESC_variable(desc)) ?
		 "ON" : "OFF");
	break;
    case OVK_INT32:
	fprintf ( tf, "%d", *((INT32 *) ODESC_variable(desc)));
	break;
    case OVK_INT64:
	fprintf ( tf, "%lld", *((INT64 *) ODESC_variable(desc)));
	break;
    case OVK_UINT32:
	fprintf ( tf, "%u", *((UINT32 *) ODESC_variable(desc)));
	break;
    case OVK_UINT64:
	fprintf ( tf, "%llu", *((UINT64 *) ODESC_variable(desc)));
	break;

    case OVK_NAME:
    case OVK_SELF:
	/* These are likely to be repeasted, e.g. for
	 * -TARG:isa=mips3 and -TARG:mips3.  So check whether this
	 * is the primary, and just refer to it from secondaries:
	 */
	if ( ODESC_primary(desc) == desc
	  || ODESC_primary(desc) == NULL  )
	{
#ifdef KEY /* bug 12020: The compiler may change the fprintf to fputs,
	      which cannot handle null. */
	  char ** var = (char **) ODESC_variable(desc);
	  if ( *var != NULL )
	    fprintf ( tf, "%s", *var);
#else
	  fprintf ( tf, "%s", *((char **) ODESC_variable(desc)));
#endif
	} else {
	  fprintf ( tf, " (See '%s' above)",
		    ODESC_name(ODESC_primary(desc)) );
	}
	break;

    case OVK_LIST:
	/* Lists are likely to be repeated for multiple options.
	 * If the primary isn't this option, we have that situation,
	 * and we simply refer to the primary:
	 */
	if ( ODESC_primary(desc) == desc
	  || ODESC_primary(desc) == NULL  )
	{
	  char sep = ' ';

	  this = (OPTION_LIST **)ODESC_variable(desc);
	  for ( list = *this; list != NULL; list = OLIST_next(list) ) {
	    fprintf ( tf, "%c%s%c%s", sep, OLIST_opt(list),
		      OGROUP_valmarker(og), OLIST_val(list) );
	    sep = OGROUP_separator(og);
	  }
	} else {
	  fprintf ( tf, " (See '%s' above)",
		    ODESC_name(ODESC_primary(desc)) );
	}
	break;

    default:
	break;
    }
    fprintf ( tf, "\n" );

    if ( ODESC_description(desc) != NULL ) {
      fprintf ( tf, "%s                        (%s)\n",
		prefix, ODESC_description(desc) );
    }
  }

  fprintf ( tf, "%s%s\n", prefix, bar );

  if ( update ) {
    for ( desc = OGROUP_options(og);
	  ODESC_kind(desc) != OVK_COUNT
       && ODESC_kind(desc) != OVK_OLD_COUNT;
	  desc++ )
    {
      aux = ODESC_aux(desc);
      if ( internal ) {
	Reset_ODA_set_int(aux);
	Reset_ODA_mod_int(aux);
      } else {
	Reset_ODA_set_user(aux);
	Reset_ODA_mod_user(aux);
      }
    }
  }
}

/* ====================================================================
 *
 * Trace_Option_Group / Trace_Command_Line_Group
 *
 * Given a command line group descriptor, trace the options in the
 * group to the given file, i.e. do Print_Command_Line_Group with
 * internal=TRUE and prefix="".  Trace_Command_Line_Group is present
 * only for backwards compatibility.
 *
 * ====================================================================
 */

void
Trace_Option_Group ( FILE *tf, OPTION_GROUP *og, BOOL full )
{
  Print_Option_Group ( tf, og, "", TRUE, full, TRUE );
}

/* ================================================================= */

void
Trace_Command_Line_Group ( FILE *tf, OPTION_GROUP *og )
{
  Print_Option_Group ( tf, og, "", TRUE, FALSE, TRUE );
}

/* ====================================================================
 *
 * Print_Option_Groups / Trace_Option_Groups
 *
 * Same as the singular forms above, but print all groups in an array.
 *
 * ====================================================================
 */

void
Print_Option_Groups ( FILE *tf, OPTION_GROUP *og, const char *prefix,
		      BOOL internal, BOOL full, BOOL update )
{
  while ( OGROUP_name(og) != NULL ) {
    Print_Option_Group ( tf, og, prefix, internal, full, update );
    ++og;
  }
}

/* ================================================================= */

void
Trace_Option_Groups ( FILE *tf, OPTION_GROUP *og, BOOL full )
{
  Print_Option_Groups ( tf, og, "", TRUE, full, TRUE );
}

/* ====================================================================
 *
 * Get_Command_Line_Group
 *
 * Given an array of option groups and a group name, return a pointer
 * to the option group with the given name, or NULL.  The array must
 * be terminated by a NULL name if the requested name is not present.
 *
 * ====================================================================
 */

OPTION_GROUP *
Get_Command_Line_Group ( OPTION_GROUP *og, const char *name )
{
  INT32 i;

  for ( i = 0; OGROUP_name(og+i) != NULL; i++ ) {
    if ( strcmp ( OGROUP_name(og+i), name ) == 0 ) return og+i;
  }
  return NULL;
}


/* ====================================================================
 * handle individual option inside Save_or_restore_options
 * ====================================================================*/
#define individual_option(opt, str) \
    ODESC_kind(&odesc) = OVK_INT32; \
    ODESC_variable(&odesc) = (void *)&opt; \
    ODESC_can_change_by_pragma(&odesc) = TRUE; \
    incr = Copy_option(&odesc, (char *)(memory+offset), save); \
    offset += incr; \
    Is_Trace(trace, (TFile, "  %s %d: %s (%d bytes)\n", \
		     save ? "saving" : "restoring", offset, str, incr));


/* ====================================================================
 * Save__or_restore_options
 * save or restore all options to the memory given
 * keep track of how much memory is used and check against size
 *
 * NOTE: this same routine to used to save and restore the options,
 * 	 this way they will always be in the same order.
 * ====================================================================*/
void Save_or_restore_options(char *memory, INT32 size, BOOL save)
{
  OPTION_GROUP *ogroups = Common_Option_Groups;
  OPTION_GROUP *group;
  BOOL trace = Get_Trace(TP_MISC, 0x80); /* full command line tracing */
  INT32 incr, offset = 0;

  Is_Trace(trace, (TFile, "Save_or_restore_options: %s called\n",
	  save ? "save" : "restore"));

  /*----------------------------------------------------------------------*/
  /* go through all option groups */
  for (group = ogroups; group && OGROUP_options(group) && OGROUP_name(group);
       group++) {
    OPTION_DESC *odesc;
    OGROUP_AUX *ogaux = OGROUP_aux(group);
    ODESC_AUX *odaux = OGA_odesc_aux(ogaux);
    INT16 i, count = OGA_count(ogaux);

    Is_Trace(trace, (TFile, "group = %s\n", OGROUP_name(group)));

    /* Iterate through the descriptors in the group */
    for (i=0, odesc=OGROUP_options(group); i<count; i++, odesc++, odaux++) {
      /* only save/restore the ones that can change */
      if (ODESC_can_change_by_pragma(odesc)) {
	incr = Copy_option(odesc, (char *)(memory+offset), save);
	Is_Trace(trace, (TFile, "  %s %d: %s (%d bytes)\n",
			 save ? "saving" : "restoring",
			 offset, ODESC_name(odesc), incr));
	offset += incr;
#ifdef Is_True_On
	Is_True(offset < size,
		("OPTIONS_SIZE in options_stack.h needs to be bigger\n"));
#endif      
      }
    } /* for (i=0, odesc=OGROUP_options(group); i<count; ... */

  } /* for (group = ogroups; group && OGROUP_options(group) ... */

  /*----------------------------------------------------------------------*/
  /* now go through the options not in the groups */
  { OPTION_DESC odesc;

    Is_Trace(trace, (TFile, "group = Individual Options\n"));

    /* Add more options here, individual_option is a macro defined above */
    individual_option(Opt_Level, "Opt_Level");
  }

  /*----------------------------------------------------------------------*/
  Is_Trace(trace, (TFile, "max offset = %d\n", offset));
}
