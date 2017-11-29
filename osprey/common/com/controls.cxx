/*
 * Copyright 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
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
 * Module: controls.c
 * $Revision: 1.1.1.1 $
 * $Date: 2005/10/21 19:00:00 $
 * $Author: marcel $
 * $Source: /proj/osprey/CVS/open64/osprey1.0/common/com/controls.cxx,v $
 *
 * Revision history:
 *  12-Sep-90 - Original Version
 *  17-Jun-91 - Integrated Josie version.
 *
 * Description:
 *
 * A general package for uniform access to compiler controls which may
 * be set via command line flags or pragmas.  It is separate from
 * flags.c to allow linking with the driver (or other processes)
 * without getting all of the stuff in flags.c.
 *
 * TODO:  This file has not gotten much attention during integration.
 * We need to go over the option list and other stuff to determine
 * what is really useful for Muse.
 *
 * TODO: 
 * - Check that routine scope controls aren't modified within routines.
 *	WORKS for f77. Not for C
 * - Pop once loop level controls in C and f77
 * - Pop once routine level controls in edgcfe
 *
 * ====================================================================
 * ====================================================================
 */


#include "defs.h"
#include "controls.h"
#include "config.h"
#include "erglob.h"
#include "glob.h"
#include "tracing.h"

/* To loop over all controls: */
#define FOR_ALL_CONTROLS(i) for (i=CONTROL_FIRST; i<CONTROL_LAST; i++)

typedef struct str_list {
  const char* item;
  struct str_list *next;
} STR_LIST;

#define STRLIST_item(x)	(x)->item

#include "targ_ctrl.h"

/* Define a structure describing the supported controls: */
typedef struct {
  const char    *name;
  CONTROL index;   /* used to verify that the table is in order */
  INT16   flags;
  INTPS first_def; /* first default */
  INTPS sec_def;   /* second default */
  INTPS min_val,   /* minimum value for int_values controls. */
        max_val;   /* max value */
  INTPS cur_val;   /* current value */
  INTPS prev_val;  /* previous value. To be popped after one application */
} CONTROL_INFO;

#define CI_HAS_AA_VAL		0x0001 /* has val that can not be changed  */
#define CI_HAS_ONCE_VAL		0x0002 /* has val that expires in one use.
					* Replace by prev_val after one use */
#define CI_USER_SPECIFIED_IMPL	0x0004 /* Implicitly specified by user,
					* by -O flag, for example */
#define CI_USER_SPECIFIED_EXPL	0x0008 /* Explicitly specified by user,
					* by -Asched=0, for example */
#define CI_USER_SPECIFIED (CI_USER_SPECIFIED_IMPL|CI_USER_SPECIFIED_EXPL)
#define CI_NAMELIST_TYPE	0x0010 /* Control type is namelist
					* (as opposed to integer valued) */
#define CI_CAN_CHANGE		0x0020 /* Can be respecified in cor */
#define CI_SCOPE		0x0f00 /* Scope of this control. One of: */
# define CI_SCOPE_LINE		0x0100 /*	per source line	*/
# define CI_SCOPE_LOOP		0x0200 /*	per loop	*/
# define CI_SCOPE_ROUTINE	0x0300 /*	per routine	*/
# define CI_SCOPE_FILE		0x0400 /*	per source file	*/
# define CI_SCOPE_COMPILATION	0x0500 /*	per compilation	*/
#define CI_HAS_CHANGED		0x1000 /* Changed by a pragma */

const char *ci_int_type_message = "Control %s expects integer values";
const char *ci_nlist_type_message = "Control %s expects namelist values";

#define IS_INT_TYPED(ci) (Is_True(((ci)->flags&CI_NAMELIST_TYPE)==0,\
			 (ci_int_type_message, ci->name)),(ci))
#define IS_NLIST_TYPED(ci) (Is_True(((ci)->flags&CI_NAMELIST_TYPE),\
			   (ci_nlist_type_message, ci->name)),(ci))

#define CI_int(ci,f)   (IS_INT_TYPED(ci))->f
#define CI_nlist(ci,f) ((STR_LIST*)(IS_NLIST_TYPED(ci))->f)
#define Set_CI_int(ci,f,v)   (IS_INT_TYPED(ci))->f=(v)
#define Set_CI_nlist(ci,f,v) (IS_NLIST_TYPED(ci)->f)=((INTPS)(v))

#define CI_can_change(ci)		((ci)->flags & CI_CAN_CHANGE)
#define CI_scope(ci)			((ci)->flags & CI_SCOPE)

#define CI_has_changed(ci)		((ci)->flags & CI_HAS_CHANGED)
#define Set_CI_has_changed(ci)		((ci)->flags |= CI_HAS_CHANGED)

#define CI_has_AA_val(ci)		((ci)->flags & CI_HAS_AA_VAL)
#define Set_CI_has_AA_val(ci)		((ci)->flags |= CI_HAS_AA_VAL)
#define Reset_CI_has_AA_val(ci)		((ci)->flags &= ~CI_HAS_AA_VAL)

#define CI_has_once_val(ci)		((ci)->flags & CI_HAS_ONCE_VAL)
#define Set_CI_has_once_val(ci)		((ci)->flags |= CI_HAS_ONCE_VAL)
#define Reset_CI_has_once_val(ci)	((ci)->flags &= ~CI_HAS_ONCE_VAL)

#define CI_user_specified(ci)		((ci)->flags & CI_USER_SPECIFIED)
#define Set_CI_user_specified(ci)	((ci)->flags |= CI_USER_SPECIFIED)
#define Reset_CI_user_specified(ci)	((ci)->flags &= ~CI_USER_SPECIFIED)

#define CI_user_specified_impl(ci)	 ((ci)->flags & CI_USER_SPECIFIED_IMPL)
#define Set_CI_user_specified_impl(ci)	 ((ci)->flags |= CI_USER_SPECIFIED_IMPL)
#define Reset_CI_user_specified_impl(ci) ((ci)->flags &= ~CI_USER_SPECIFIED_IMPL)

#define CI_user_specified_expl(ci)	 ((ci)->flags & CI_USER_SPECIFIED_EXPL)
#define Set_CI_user_specified_expl(ci)	 ((ci)->flags |= CI_USER_SPECIFIED_EXPL)
#define Reset_CI_user_specified_expl(ci) ((ci)->flags &= ~CI_USER_SPECIFIED_EXPL)

#define CI_is_int_type(ci)		(((ci)->flags & CI_NAMELIST_TYPE) == 0)
#define CI_is_nlist_type(ci)		((ci)->flags & CI_NAMELIST_TYPE)


/* TRUE when inside a routine. Used to check that routine scope
 * controls are not modified inside routines.
 */
static BOOL Inside_A_Routine = FALSE;

/* ====================================================================
 *
 * Control error reporting
 *
 * ====================================================================
 */

/* Should we report pragma errors?  Yes in FE, maybe in BE: */
BOOL Diag_On_Pragmas 
#ifdef FRONT_END
			= TRUE
#else /* ! FRONT_END */
			= TRUE
#endif
;

/* Should we report control errors?
 * Yes initially; later Diag_On_Pragmas:
 */
static BOOL Diag_Controls = TRUE;
#define Report_Error	if (Diag_Controls) ErrMsg

/* ====================================================================
 *
 *		Beginning of section on control names
 *
 * ====================================================================
 */

static STR_LIST ccv0 = {"cckr",     NULL};
static STR_LIST ccv1 = {"xansi",   &ccv0};
static STR_LIST ccv2 = {"cplus",   &ccv1};
static STR_LIST ccv  = {"ansi",    &ccv2};

static STR_LIST icv0 = {"signed",  NULL};
static STR_LIST icv  = {"unsigned",&icv0};

static STR_LIST ocv0 = {"svr4", NULL};
static STR_LIST ocv  = {"svr3", &ocv0};

#define N CI_NAMELIST_TYPE
#define MI 0x7fffffff
#define H CI_CAN_CHANGE
#define P CI_SCOPE_LOOP
#define L CI_SCOPE_LINE		
#define R CI_SCOPE_ROUTINE
#define F CI_SCOPE_FILE	
#define C CI_SCOPE_COMPILATION

CONTROL_INFO Aflag_Tbl[] = {
/* name	 	 idx		flags fdef sdef min max */
  {NULL        , CONTROL_MIN_CONTROL},
  {"acir"      , CONTROL_ACIR       , F, 1,   2, 0, 63},
  {"alias"     , CONTROL_ALIAS      , R, 1,   4, 0,  4},
  {"alndcl"    , CONTROL_ALNDCL     , R, 0,   1, 1, -1}, /* not decided yet. */
  {"alnref"    , CONTROL_ALNREF     , R, 0, 128, 0,255},
  {"alnstd"    , CONTROL_ALNSTD     , R, 0,   1, 0,  1},
  {"argoverlap", CONTROL_ARGOVERLAP ,N|R,0,   0, 0,  0},
  {"c"         , CONTROL_C          ,N|F,0,   0, (INTPS)&ccv},
  {"callmod"   , CONTROL_CALLMOD    , R, 0,   2, 0,  2},
  {"case"      , CONTROL_CASE       , F, 0,   1, 0,  1},
  {"char"      , CONTROL_CHAR       ,N|F,0,   0, (INTPS)&icv},
  {"chkargs"   , CONTROL_CHKARGS    , R, 0,   1, 0,  1},
  {"chkrec"    , CONTROL_CHKREC     , R, 0,   1, 0,  1},
  {"chksub"    , CONTROL_CHKSUB     , L, 0,   1, 0,  1},
  {"constp"    , CONTROL_CONSTP     ,H|R,0,   2, 0,  2},
  {"comname"   , CONTROL_COMNAME    , F, 1,   1, 0,  1},
  {"copyp"     , CONTROL_COPYP      ,H|R,0,   2, 0,  2},
  {"defargoverlap",CONTROL_DEFARGOVERLAP,L,0, 0, 0,  2},
  {"deffunc"   , CONTROL_DEFFUNC    , L, 1,   2, 0,  2},
  {"defkeepargs",CONTROL_DEFKEEPARGS, L, 1,   2, 0,  2},
  {"deflib"    , CONTROL_DEFLIB     , L, 2,   0, 0,  2},
  {"defnewmem" , CONTROL_DEFNEWMEM  , L, 1,   2, 0,  2},
  {"defrec"    , CONTROL_DEFREC     , L, 1,   2, 0,  2},
  {"defsef"    , CONTROL_DEFSEF     , L, 1,   2, 0,  2},
  {"defsrc"    , CONTROL_DEFSRC     , L, 0,   2, 0,  2},
  {"defvol"    , CONTROL_DEFVOL     , L, 0,   2, 0,  2},
  {"diag"      , CONTROL_DIAG       , L, 1,   2, 0,  2},
  {"dline"     , CONTROL_DLINE      , L, 0,   1, 0,  1},
  {"domain"    , CONTROL_DOMAIN     , R, 1,   1, 0,  1},
  {"exits"     , CONTROL_EXITS      ,N|R,0,   0, 0,  0},
  {"fblank"    , CONTROL_FBLANK     , L, 0,   1, 0,  1},
  {"fcm"       , CONTROL_FCM        ,H|R,0,   1, 0,  2},
  {"fcols"     , CONTROL_FCOLS      , L,72,   0, 0, MI},
  {"feral"     , CONTROL_FERAL      ,N|L,0,   0, 0,  0},
  {"flow"      , CONTROL_FLOW       , R, 0,   1, 0,  1},
  {"fp"        , CONTROL_FP         , R, 0,   2, 0,  2},
  {"ftab"      , CONTROL_FTAB       , L, 1,   2, 0,  2},
  {"func"      , CONTROL_FUNC       ,N|L,0,   0, 0,  0},
  {"g"         , CONTROL_G          , F, 0,   2, 0,  3},
  {"inline"    , CONTROL_INLINE     ,N|L,0,   0, 0,  0},
  {"keepargs"  , CONTROL_KEEPARGS   ,N|L,0,   0, 0,  0},
  {"leaf"      , CONTROL_LEAF       , L, 0,   0, 0,  1},
  {"map"       , CONTROL_MAP        , F, 0,   1, 0,  1},
  {"memlimit"  , CONTROL_MEMLIMIT   ,H|F,0,   0, 0, MI},
  {"newmem"    , CONTROL_NEWMEM     ,N|L,0,   0, 0,  0},
  {"noargoverlap",CONTROL_NOARGOVERLAP,N|L,0, 0, 0,  0},
  {"nofunc"    , CONTROL_NOFUNC     ,N|L,0,   0, 0,  0},
  {"noinline"  , CONTROL_NOINLINE   ,N|L,0,   0, 0,  0},
  {"nokeepargs", CONTROL_NOKEEPARGS ,N|L,0,   0, 0,  0},
  {"nonewmem"  , CONTROL_NONEWMEM   ,N|L,0,   0, 0,  0},
  {"norec"     , CONTROL_NOREC      ,N|L,0,   0, 0,  0},
  {"nosef"     , CONTROL_NOSEF      ,N|L,0,   0, 0,  0},
  {"novol"     , CONTROL_NOVOL      ,N|L,0,   0, 0,  0},
  {"onetrip"   , CONTROL_ONETRIP    , P, 0,   1, 0,  1},
  {"oform"     , CONTROL_OFORM      ,N|C,0,   0, (INTPS) &ocv},
  {"mopt"      , CONTROL_MOPT       ,H|R,1,   3, 0,  3},
  {"prof"      , CONTROL_PROF       , R, 0,   1, 0,  1},
  {"ptrvol"    , CONTROL_PTRVOL     ,N|L,0,   0, 0,  0},
  {"quit"      , CONTROL_QUIT       , F, 0,   1, 0,  2},
  {"real"      , CONTROL_REAL       , F, 0,   0, 0,  8},
  {"recursive" , CONTROL_RECURSIVE  ,N|L,0,   0, 0,  0},
  {"reg"       , CONTROL_REG        , R, 0,   3, 0,  3},
  {"retpts"    , CONTROL_RETPTS     , R, 0,   1, 0,  1},
  {"save"      , CONTROL_SAVE       , R, 0,   1, 0,  1},
  {"sched"     , CONTROL_SCHED      ,H|R,0,   1, 0,  1},
  {"sef"       , CONTROL_SEF        ,N|L,0,   0, 0,  0},
  {"stddiag"   , CONTROL_STDDIAG    , L, 0,   1, 0,  2},
  {"tame"      , CONTROL_TAME       ,N|L,0,   0, 0,  0},
  {"targ"      , CONTROL_TARG       ,N|C,TARG_FIRST_DEF,   TARG_SECOND_DEF, (INTPS) &Possible_Targets},
  {"unroll"    , CONTROL_UNROLL     , P, 0,   1, 0, MI},
  {"unrollexact",CONTROL_UNROLLEXACT, P, 0,   1, 0,  1},
  {"volatile"  , CONTROL_VOLATILE   ,N|L,0,   0, 0,  0},
  {"whole"     , CONTROL_WHOLE      , C, 0,   1, 0,  1},
  {"wild"      , CONTROL_WILD       ,N|L,0,   0, 0,  0},
  {"xref"      , CONTROL_XREF       , F, 0,   1, 0,  1},

  /* New Josie/92 controls: */
  {"ivrep"     , CONTROL_IVREP      ,H|R,0,   1, 0,  1},
  {"xopt"      , CONTROL_XOPT       , R, 0,   4, 0,  5},

  {NULL        , CONTROL_MAX_CONTROL}
};
#undef H 
#undef P 
#undef L 
#undef R 
#undef F 
#undef C 
#undef MI
#undef N

/* TO CHECK: 1) integer valued have NULL as possible vals.
 *	     2) integer valued have max_val > min_val,
 *		except when max_val=0 and min_val= -1.
 *	     3) Non integer valued have "*" or a list of comma separated names.
 *	     4) that first and sec def are in range.
 *	     4) index is in order
 */

/* ====================================================================
 *
 * Control group table:  We keep the name, min and max values, the
 * second defaults (there are no first defaults of groups) and an array
 * of char strings, whose members represent the expansion of that group
 * in terms of controls.  When we get a group specification, we index
 * into this array and pass the corresponding string back to the
 * routine that handles controls.
 *
 * ====================================================================
 */

typedef struct o_gr_exp {
  const char *name;
  const char *val;
} O_GR_EXP;

typedef struct {
  const char    *name;
  INT16   flags, sec_def, min_val, max_val;
  O_GR_EXP   *expansion;
} CONTROL_GROUP_INFO;

#define CGI_IS_INT_TYPE       0x0001
#define CGI_is_int_type(c)		((((c)->flags) & CGI_IS_INT_TYPE) != 0)

static O_GR_EXP o_group_expansion[] = {
  /*0*/ { "no-opt",
	  "callmod=0,constp=0,copyp=0,domain=1,flow=0,fcm=0,"
	  "alias=0,mopt=0,reg=0,sched=0,unroll=0,whole=0" },
  /*1*/ { "local-opt",
	  "callmod=0,constp=0,copyp=0,domain=1,flow=0,fcm=0,"
	  "alias=1,mopt=1,reg=0,sched=0,unroll=0,whole=0" },
  /*2*/ { "global-opt",
	  "callmod=1,constp=2,copyp=2,domain=1,flow=1,fcm=1,"
	  "alias=3,mopt=3,reg=1,sched=1,unroll=0,whole=0" },
  /*3*/ { "swp-opt",
	  "callmod=1,constp=2,copyp=2,domain=1,flow=1,fcm=1,"
	  "alias=3,mopt=3,reg=1,sched=1,unroll=0,whole=0" },
};

static O_GR_EXP f_group_expansion[] = {
/* 0 */ {"classic", "fcols=72,ftab=0,fblank=1" },
/* 1 */ {"svs72"  , "fcols=72,ftab=0,fblank=0" },
/* 2 */ {"svs120" , "fcols=120,ftab=0,fblank=1" },
/* 3 */ {"normal" , "fcols=72,ftab=1,fblank=0" },
/* 4 */ {"vax72"  , "fcols=72,ftab=1,fblank=1" },
/* 5 */ {"vax132" , "fcols=132,ftab=1,fblank=1" },
/* 6 */ {"mips72" , "fcols=72,ftab=2,fblank=0" },
/* 7 */ {"unix72" , "fcols=72,ftab=2,fblank=1" },
/* 8 */ {"unix"   , "fcols=0,ftab=2,fblank=1" }
};

static CONTROL_GROUP_INFO Control_Group_Tbl[] = {
  {"OPT",  CGI_IS_INT_TYPE,	2, 0, 3, o_group_expansion},
  {"FORT", 0,			8, 0, 0, f_group_expansion},
  { NULL,  0,			0, 0, 0, 0,            }
};

static STR_LIST *make_nlist(char *name, STR_LIST *next)
{
  STR_LIST *r = (STR_LIST *) Src_Alloc(sizeof(STR_LIST));
  r->item = strcpy((char *)Src_Alloc(strlen(name)+1),name);
  r->next = next;
  return r;
}

#define IS_ID_CHAR(c) (((c)>='0'&&(c)<='9')||((c)=='-')||(nlist_ctrl&&\
	(((c)>='a'&&(c)<='z')||((c)=='_')||((c)>='A'&&(c)<='Z')||((c)=='$'))))

#define ERRORS_FOUND    1
#define NO_ERRORS_FOUND 0

static INT store_ctrl(char *, STR_LIST *, INT);

/* ====================================================================
 *
 * Given a name return TRUE if it is the name of an int typed control
 * or group.  Return FALSE if it is not name of any control or group.
 * The return value is used during parsing to determine whether to look
 * for a digit sequence or an arbitrary string as value.  As
 * implemented, the name given by user is searched twice, once in this 
 * routine and once later in store_ctrl.  Fixing this will require
 * maintaining and passing more info between this routine,
 * Process_Ctrl_Opt and store_ctrl.
 *
 * ====================================================================
 */

static BOOL
is_nlist_typed ( char *name )
{
  if (name[0] >= 'A' && name[0] <= 'Z') {
    CONTROL_GROUP_INFO *cgi;
    for (cgi = Control_Group_Tbl; cgi->name; cgi++) 
      if (name[0] == cgi->name[0]) 
	return !(CGI_is_int_type(cgi));
  } else {
    CONTROL_INFO *a;
    INT i;
    for ( i=CONTROL_FIRST,a=Aflag_Tbl+(INT)i; i<CONTROL_LAST; i++,a++) 
      if (a->name && strcmp(name, a->name) == 0)
	return CI_is_nlist_type(a);
  }
  return FALSE;
}

/* ====================================================================
 *
 * Process_Control_Opt
 *
 * Process a control spec save_a, and store in current value of
 * controls.  "flags" is a mask of HCO values defined above.  Return 1
 * if errors were found, 0 if no errors were found.
 *
 * ====================================================================
 */

INT
Process_Control_Opt ( const char *save_a, INT flags )
{
  char *name, ch, *s, *a;
  INT nlist_ctrl, found_lpar;
  STR_LIST *nl;

  a = strcpy((char *)Src_Alloc(strlen(save_a)+1), save_a);
  while (1) {
    name = a;
    ch = a[0];
    if (ch >= 'A' && ch <= 'Z') {
      /* ctrl group name */
      a++;
    } else if (ch >= 'a' && ch <= 'z') {
      a++;
      while ((ch = a[0]) && ch >= 'a' && ch <= 'z')
	a++;
    } else {
      Report_Error ( EC_Ctrl_Syntax, save_a );
      return ERRORS_FOUND;
    }
    name = strncpy((char *)Src_Alloc(a-name+1), name, a-name);
    if (a[0] == '\0') {
      /* last ctrl with no val */
      return store_ctrl(name, NULL, flags);
    }
    /* search_ctrl: return CONTROL_MIN_CONTROL if search failed.
     * We treat it as a namelist control in order to avoid spurious
     * syntax error messages. We do give unknown control error later.
     */
    nlist_ctrl = is_nlist_typed(name);
    if (a[0] != '=') {
      if (a[0] == ',') {
	*a = '\0'; 
	/* non-last ctrl with no val */
	if (store_ctrl(name, NULL, flags))
	  return ERRORS_FOUND;
	a++;
	continue;
      }
    } else {
      a[0] = '\0';
      a++;
    }
    /* now we expect a value. It can be parenthesised or not */
    if (a[0] == '(' /*)*/) {
      a[0] = '\0';
      found_lpar = 1;
      a++;
    } else
      found_lpar = 0;
    nl = NULL;
    while (1) {
      INT ef;
      s = a;
      if (!IS_ID_CHAR(a[0])) {
	Report_Error ( EC_Ctrl_Syntax, save_a );
	return ERRORS_FOUND;
      }
      while ((ch = a[0]) && IS_ID_CHAR(ch)) a++;
      if (ch == '\0') {
	return store_ctrl(name, make_nlist(s, nl), flags);
      }
      if (found_lpar && ch == ',') {
	a[0] = '\0';
	a++;
	nl = make_nlist(s, nl);
	continue;
      }
      ch = a[0]; a[0] = '\0'; 
      ef = store_ctrl(name, make_nlist(s, nl), flags); 
      a[0] = ch;
      if (ef) return ERRORS_FOUND;
      break;
    }
    if ((a[0] == /*(*/')') != found_lpar) {
      Report_Error ( EC_Ctrl_Paren, save_a );
      return ERRORS_FOUND;
    }
    if (a[0] == /*(*/')')
      *a++ = '\0';
    if (a[0] == ',')
      *a++ = '\0';
    if (a[0] == '\0')
      break;
  }
  return NO_ERRORS_FOUND;
}

/* ====================================================================
 *
 * same_name_lists
 *
 * Determine whether two name-lists are identical.
 *
 * ====================================================================
 */

static BOOL
same_name_lists ( STR_LIST *a, STR_LIST *b )
{
  STR_LIST *p;
  INT16 ac, bc;
  for (ac =0 , p = a; p; p = p->next) ac++;
  for (bc =0 , p = b; p; p = p->next) bc++;
  if (ac != bc)
    return FALSE;
  while (a) {
    BOOL found = FALSE;
    const char *ai = a->item;
    for (p = b; p; p = p->next) 
      if (strcmp(p->item, ai) == 0) {
	found = TRUE;
	break;
      }
    if (!found)
      return FALSE;
    a = a->next;
  }
  return TRUE;
}

/* ====================================================================
 *
 * push_cur_val
 *
 * Save current value as prev_val.  Set once flag.
 *
 * ====================================================================
 */

static void
push_cur_val ( CONTROL_INFO *a )
{
  Set_CI_has_once_val(a);
  a->prev_val = a->cur_val;
}

/* ====================================================================
 *
 * store_ctrl
 *
 * Store a new value for a control in the current value field.
 *
 * ====================================================================
 */

#define CI_name(a) ((a)->name)

#define debugging FALSE
#define dprintf	if (debugging) printf

static INT
store_ctrl ( char *name, STR_LIST *name_list, INT flags )
{
  CONTROL_INFO *a;
  BOOL ok_int;
  INT32 int_val;

  if ( debugging ) {
    STR_LIST *nl = name_list;
    printf("store_ctrl: %s ", name);
      while (nl) {
	printf("%s,", nl->item);
	nl = nl->next;
      }
    printf("\n");
  }

  /* various possibilities for values of name_list are:
   *  NULL: can be value of int typed or namelist typed.
   *	    Use second default value.
   *  single, a valid integer number: can be value of int typed or
   *	    namelist typed.
   *  single, but not valid number: can be namelist only.
   *  multiple: can be namelist only.
   */
  int_val = 0;	/* unnecessary except to avoid warning msg */
  if (name_list == NULL) {
    ok_int = TRUE;
  } else if (name_list->next == NULL) {
    const char *v = name_list->item;
    if (*v == '-') v++;
    else if (*v == '+') v++;
    while (v[0] >= '0' && v[0] <= '9') v++;
    ok_int = v[0] == '\0';
    if (ok_int) int_val = atoi(name_list->item);
  } else {
    ok_int = FALSE;
  }
  

  if (name[0] >= 'A' && name[0] <= 'Z') {
    CONTROL_GROUP_INFO *cgi;
    Is_True(name[1] == '\0', ("Multiple character group name ?"));
    for (cgi = Control_Group_Tbl; cgi->name; cgi++) {
      if (name[0] == cgi->name[0]) {
	INT v;
	if (CGI_is_int_type(cgi)) {
	  if (!ok_int) {
	    Report_Error ( EC_Ctrl_Integer, cgi->name );
	    return ERRORS_FOUND;
	  }
	  v = name_list ? int_val : cgi->sec_def;
	  if (v < cgi->min_val || v > cgi->max_val) {
	    Report_Error ( EC_Group_Range, v, cgi->name,
			   cgi->min_val, cgi->max_val);
	    return ERRORS_FOUND;
	  }
	} else {
	  /* namelist value is always present. Even if user types
	   * number, it could be a single valued namelist
	   */
	  if (name_list == NULL)
	    v = cgi->sec_def;
	  else {
	    O_GR_EXP *o;
	    if (name_list->next) {
	      Report_Error ( EC_Group_Mult, cgi->name );
	      return ERRORS_FOUND;
	    }
	    v = 0;
	    for (o = cgi->expansion; o->val; o++,v++)
	      if (strcmp(name_list->item, o->name) == 0) 
		break;
	    if (o == NULL) {
	      Report_Error (EC_Inv_Ctrl_Val, name_list->item, cgi->name);
	      return ERRORS_FOUND;
	    }
	  }
	}
	return Process_Control_Opt(cgi->expansion[v].val, flags | HCO_IMPLICIT);
      }
    }
    Report_Error ( EC_Unrec_Group, name );
    return ERRORS_FOUND;
  }

  for (INT i=CONTROL_FIRST; i<CONTROL_LAST; i++ ) {
    a = &Aflag_Tbl[i];

    if (a->name && strcmp(name, a->name) == 0) {
      BOOL changed;
      if ((flags & HCO_ONCE) && CI_scope(a) == CI_SCOPE_LOOP)
	Report_Error ( EC_Unimp_Once, a->name );

      if (CI_is_nlist_type(a)) {
	STR_LIST *p;

	dprintf ( "  %s: namelist value\n", a->name );
	p = CI_nlist(a,min_val);
	if (name_list == NULL)
	  name_list = CI_nlist(a, sec_def);
	if (p) {
	  /* control takes a singleton, and only one of few possible values. 
	     like CONTROL_C CONTROL_TARG, etc  */
	  while (p) {
	    if (strcmp(p->item,name_list->item) == 0)
	      break;
	    p = p->next;
	  }
	  if (p == NULL) {
	    Report_Error ( EC_Inv_Ctrl_Val, name_list->item, a->name );
	    return ERRORS_FOUND;
	  }
	}
	if (same_name_lists(name_list, CI_nlist(a, cur_val)))
	  changed = FALSE;
	else {
	  if (CI_has_AA_val(a)) {
	    Report_Error ( EC_Change_AA, CI_name(a) );
	  }
	  else {
	    if (flags & HCO_ONCE) 
	      push_cur_val(a);
            Set_CI_nlist(a, cur_val, name_list);
	    changed = TRUE;
	  }
	}

      } else /* CI_is_int_type(a) */ {
	INT v;
	if (!ok_int) {
	  Report_Error ( EC_Ctrl_Numeric, a->name );
	  return ERRORS_FOUND;
	}
	v = (name_list) ? int_val : CI_int(a, sec_def);
	dprintf ( "  %s: integer value %d (current %ld)\n",
		  a->name, v, a->cur_val );
	if (v < CI_int(a, min_val) || v > CI_int(a, max_val)) {
	  Report_Error ( EC_Ctrl_Range, v, a->name,
			 CI_int(a,min_val), CI_int(a,max_val));
	  return ERRORS_FOUND;
	}
	if ( v != a->cur_val ) {
	  if (CI_has_AA_val(a)) {
	    Report_Error ( EC_Change_AA, CI_name(a) );
	  } else {
	    if (flags & HCO_ONCE) push_cur_val(a);
	    changed = TRUE;
	    dprintf ( "  %s: %d (was %ld)\n", a->name, v, a->cur_val );
            Set_CI_int (a, cur_val, v);
	  }
	} else {
	  changed = FALSE;
	  dprintf ( "  %s: unchanged\n", a->name );
	}
      }
      /* has changed flag is set whenever a control is changed via a
       * pragma.  This tells that the new value needs to be written to
       * .B file.
       */
      if (changed && (flags & HCO_PRAGMA)) {
	if (CI_scope(a) >= CI_SCOPE_FILE) {
	  Report_Error ( EC_File_Scope, CI_name(a) );
	  return ERRORS_FOUND;
	} else if (CI_scope(a) == CI_SCOPE_ROUTINE && Inside_A_Routine) {
	  Report_Error ( EC_Routine_Scope, CI_name(a) );
	  return ERRORS_FOUND;
	}
	Set_CI_has_changed(a);
      }
      if (flags & HCO_AAVAL)
	Set_CI_has_AA_val(a);
      if (CI_user_specified_expl(a) && changed && (flags & HCO_PRAGMA) == 0)
	/* We issue a warning whenever an explicit setting of a flag
	 * is overridden.  This will catch situations like -Asched=0 -O,
	 * where -O silently overrides -Asched=0.  However, we do it
	 * only for command line args, because it is difficult to handle
	 * in pragma processing.  Pragmas cause changes to settings
	 * several times, once for each pass, and that can trigger this
	 * message.  We might later want to give similar warnings for
	 * pragmas, but that will require more work.
	 */
	Report_Error ( EC_Override, a->name,
		       (flags & HCO_IMPLICIT) ? "implicit flag"
					      : "another explicit setting");
      if (flags & HCO_IMPLICIT)
        Set_CI_user_specified_impl(a);
      else
        Set_CI_user_specified_expl(a);
      return NO_ERRORS_FOUND;
    }
  }
  Report_Error ( EC_Unimp_Actrl, name );
  return ERRORS_FOUND;
}

/* TO CHECK: 1) integer valued have max_val > min_val, 
		except when max_val=0 and min_val= -1
	     4) index is in order */
#define CI_allowed_vals(a) CI_nlist(a,min_val)
void
Init_Controls_Tbl ( void )
{
  CONTROL_INFO *a;
  BOOL trace = Get_Trace ( TP_MISC, 1 );

  for (INT i=CONTROL_FIRST; i<CONTROL_LAST; i++ ) {
    a = &Aflag_Tbl[i];
    if (a->name == NULL)
      break;
    Is_True(a->index == i,
      ("Aflag_Tbl index mismatch: i=%1d a->index=%1d(%s)", i, a->index, a->name));
    if (CI_is_int_type(a)) {
      if (a->max_val <= a->min_val) {
	Is_True(a->max_val == -1 && a->min_val == 1,
		("inconsistent min_val and max_val of %s: %1d %1d", 
		a->name, a->min_val, a->max_val));
      } else {
        Is_True(a->first_def >= a->min_val && a->first_def <= a->max_val,
	  ("inconsistent first_def(%1d) of %s: %1d..%1d", 
		a->first_def,a->name, a->min_val, a->max_val));
        Is_True(a->sec_def >= a->min_val && a->sec_def <= a->max_val,
	  ("inconsistent sec_def(%1d) of %s: %1d..%1d", 
		a->first_def,a->name, a->min_val, a->max_val));
      }
    } else {
      /* In the initialized table first and sec def are given as numbers
       * which are indices in the chain of allowed_vals.  Convert that
       * to actual STR_LIST:
       */
      STR_LIST *v, *v1; INT sc;
      v = CI_allowed_vals(a); 
      if (v) {
        sc = a->sec_def;
        while (sc-- > 0) v = v->next;
        v1 = (STR_LIST *) calloc(1, sizeof(STR_LIST));
        v1->item = v->item;
        Set_CI_nlist(a, sec_def, v1);
        v = CI_allowed_vals(a); 
        sc = a->first_def;
        while (sc-- > 0) v = v->next;
          v1 = (STR_LIST *) calloc(1, sizeof(STR_LIST));
        v1->item = v->item;
        Set_CI_nlist(a, first_def, v1);
      } /* else there are no fixed allowed_vals. */
    }
    a->cur_val = a->first_def;
  }

  if ( trace ) {
    fprintf ( TFile, "\nInit_Controls_Tbl:\n" );
    Print_Controls ( TFile, "<init>", TRUE );
  }
}

#ifndef DRIVER

typedef struct {
  INT32 value[(INT)CONTROL_MAX_CONTROL];
} CTRL_VAL_SET;

CTRL_VAL_SET routine_top_values;
/* in cor we need to remember the command line values of controls
   because the order of processing of routines is not necessarily
   same as that in front end. Hence, at the top of every routine
   in cor we restore the cmd_line values and then apply the increments
   applicable to that routine, independent of any other routine.
   */
CTRL_VAL_SET cmd_line_values;

static void
save_ctrl_val_set ( CTRL_VAL_SET *s )
{
  INT i;
  for (i=CONTROL_FIRST; i<CONTROL_MAX_CONTROL; i++)
    s->value[i] = Aflag_Tbl[i].cur_val;
}

static void
restore_ctrl_val_set ( CTRL_VAL_SET *r )
{
  INT i;
  for (i=CONTROL_FIRST; i<CONTROL_MAX_CONTROL; i++)
    Aflag_Tbl[i].cur_val = r->value[i];
}


void
Save_Routine_Top_Ctrls ( void )
{
  BOOL trace = Get_Trace ( TP_MISC, 1 );

  save_ctrl_val_set(&routine_top_values);
  Inside_A_Routine = TRUE;

  if ( trace ) {
    fprintf ( TFile, "\nSave_Routine_Top_Ctrls:\n" );
    Print_Controls ( TFile, "<SRTC>", TRUE );
  }
}

void
Restore_Routine_Top_Ctrls ( void )
{
  BOOL trace = Get_Trace ( TP_MISC, 1 );

  restore_ctrl_val_set(&routine_top_values);
  Inside_A_Routine = FALSE;

  if ( trace ) {
    fprintf ( TFile, "\nRestore_Routine_Top_Ctrls:\n" );
    Print_Controls ( TFile, "<RRTC>", TRUE );
  }
}

void
Restore_Cmd_Line_Ctrls ( void )
{
  BOOL trace = Get_Trace ( TP_MISC, 1 );

  restore_ctrl_val_set(&cmd_line_values);

  if ( trace ) {
    fprintf ( TFile, "\nRestore_Cmd_Line_Ctrls:\n" );
    Print_Controls ( TFile, "<RCLC>", TRUE );
  }
}

void
Apply_Controls ( void )
{
  INT i;
  CONTROL_INFO *a;
  INT32 control_ival;

  save_ctrl_val_set(&cmd_line_values);

  for ( i=CONTROL_FIRST,a=Aflag_Tbl+(INT)i; i<CONTROL_LAST; i++,a++) {
    if (CI_user_specified(a)) {
      switch (a->index) {
	case CONTROL_C:
	case CONTROL_CHAR:
	case CONTROL_ALIAS:
	case CONTROL_CASE:
	case CONTROL_CALLMOD:
	case CONTROL_DEFVOL:
	case CONTROL_DLINE:
	case CONTROL_MAP:
	case CONTROL_UNROLL:
	case CONTROL_UNROLLEXACT:
	  break;

	case CONTROL_ALNREF:
	  Allow_Word_Aligned_Doubles = FALSE;
	  control_ival = Get_Int_Ctrl_Val(CONTROL_ALNREF);
	  if ( control_ival == 128 ) {
	    /* allow doubles to be aligned on only 4-byte boundaries */
	    Allow_Word_Aligned_Doubles = TRUE;
	  } else {
	    Report_Error ( EC_Unimp_Align, a->name, control_ival );
	  }
	  break;

	case CONTROL_DIAG:
	  if (a->cur_val == 0)
	    Min_Error_Severity = ES_ERROR;
	  break;

	default:
	  /* Report_Error ( EC_Unimp_Ctrl, a->name ); */
	  break;
      }
    }
  }

  /* After this, report errors/warnings iff Diag_On_Pragmas is set: */
  Diag_Controls = Diag_On_Pragmas;
}

/*--------------------------------------------------------------
 * routine to copy values of routine scope controls to internal
 * flags that are used by compiler. TODO: We need to make sure 
 * that this routine is called after any pragma's or directives 
 * applicable to a routine have been applied.
 * TODO: Get rid of this routine. Use control values directly
 *------------------------------------------------------------*/

void
Apply_Routine_Scope_Controls ( void )
{
  Symbolic_Debug_Mode = SDM_NONE;
  switch (Get_Int_Ctrl_Val(CONTROL_FP)) {
    case 2:
      Symbolic_Debug_Mode |= SDM_USE_FP;
      /* fall through */
    case 1:
      Symbolic_Debug_Mode |= SDM_GEN_FP;
  }
  if (Get_Int_Ctrl_Val(CONTROL_G))
    Symbolic_Debug_Mode |= (SDM_LINE|SDM_SYMBOL);
  Max_Symbolic_Debug_Mode = Symbolic_Debug_Mode;
}
#endif

INT32
Get_Int_Ctrl_Val ( CONTROL a )
{
#ifdef Is_True_On
  Is_True((Aflag_Tbl[a].flags & CI_NAMELIST_TYPE) == 0,
	  ("Control %s does not have integral value", Aflag_Tbl[a].name));
#endif
  return CI_int((Aflag_Tbl+ (INT)a),cur_val);
}

const char *
Get_Name_Ctrl_Val ( CONTROL a )
{
#ifdef Is_True_On
  Is_True(Aflag_Tbl[a].flags & CI_NAMELIST_TYPE, 
	  ("Control %s does not have name-list value", Aflag_Tbl[a].name));
#endif
  return STRLIST_item(CI_nlist((Aflag_Tbl+ (INT)a),cur_val));
}

/* ====================================================================
 *
 * Pop_Controls / Pop_Once_Line_Controls
 *
 * Pop ONCE values of any controls of a given level.
 *
 * The second is a special entry-point for C, to avoid including
 * flags.h in lexical.c.  (Josie comment -- OBSOLETE.)
 *
 * ====================================================================
 */

void
Pop_Controls ( INT32 level )
{
  CONTROL_INFO *a;
  INT i;
  
  for ( i=CONTROL_FIRST,a=Aflag_Tbl+(INT)i; i<CONTROL_LAST; i++,a++) 
    if (CI_scope(a) == level && CI_has_once_val(a)) {
      Reset_CI_has_once_val(a);
      Aflag_Tbl[i].cur_val = Aflag_Tbl[i].prev_val;
    }
}

/* ================================================================= */

void
Pop_Once_Line_Controls ( void )
{
  Pop_Controls(CI_SCOPE_LINE);
}



/* ====================================================================
 *
 * Process_Pragma
 *
 * Process a pragma, entering it in the control table.
 *
 * ====================================================================
 */

INT
Process_Pragma ( char *x )
{
  INT flags = HCO_PRAGMA;
  /*printf("PROCESS PRAGMA: %s\n", x);*/
  INT rv = 0;

  while (x[0] == ' ' || x[0] == '\t') x++;
  if (x[0] == '%' && (x[1]=='o' || x[1]=='O') && (x[2]=='n' || x[2]=='N') && 
     (x[3] == 'c' || x[3] == 'C') && (x[4] == 'e' || x[4] == 'E')) {
     flags = HCO_PRAGMA | HCO_ONCE;
     x += 5;
  } else 
     flags = HCO_PRAGMA;

  /* pick substrings and call Process_Control_Opt for them */
  while (1) {
    const char *r;
    char ch;
    while (x[0] == ' ' || x[0] == '\t') x++;
    if (x[0] == '\0') return rv;
    r = x;
    while (x[0] != '\0' && x[0] != ' ' && x[0] != '\t') x++;
    ch = x[0];
    x[0] = '\0';
    rv |= Process_Control_Opt(r, flags);
    if (ch == '\0') return rv;
    x[0] = ch;
  }
}

/* ====================================================================
 *
 * Print_Controls
 *
 * Print the current values of the controls to the given file.  Start
 * each output line with the given tag string.  Only print controls
 * with their default values if "def" is TRUE.
 *
 * ====================================================================
 */

void
Print_Controls ( FILE *fp, const char *tag, BOOL def )
{
  CONTROL_INFO *a;
  BOOL defaulted;

  for (INT i=CONTROL_FIRST; i<CONTROL_LAST; i++ ) {
    a = &Aflag_Tbl[i];
    defaulted = (a->cur_val == a->first_def);
    if ( def || !defaulted ) {
      fprintf(fp, "%s %s%s = ", tag, defaulted ? "*" : " ", a->name);
      if ( CI_is_int_type(a) )
	fprintf ( fp, "%ld\n", a->cur_val );
      else {
	STR_LIST *s = CI_nlist(a, cur_val);
	while (s) {
	  fprintf ( fp, " %s%s", s->item, s->next ? ",":"" );
	  s = s->next;
	}
	fprintf ( fp, "\n" );
      }
    }
  }
  
}

/* handle the interation between -g and -O */
void Fix_g_O( void )
{
#ifdef TARG_IA64
  if (Debug_Level == 2 || Debug_Level == 1) {
#else
    if (Debug_Level >= 2) {
#endif
    if (Opt_Level > 0) {
#ifdef FRONT_END
      ErrMsg(EC_Fix_g_O);
#endif
      Opt_Level = 0;
    }

    Set_CI_int((Aflag_Tbl+CONTROL_CALLMOD),cur_val, 0);
    Set_CI_int((Aflag_Tbl+CONTROL_CONSTP),cur_val, 0);
    Set_CI_int((Aflag_Tbl+CONTROL_COPYP),cur_val, 0);
    Set_CI_int((Aflag_Tbl+CONTROL_DOMAIN),cur_val, 1);
    Set_CI_int((Aflag_Tbl+CONTROL_FLOW),cur_val, 0);
    Set_CI_int((Aflag_Tbl+CONTROL_FCM),cur_val, 0);
    Set_CI_int((Aflag_Tbl+CONTROL_ALIAS),cur_val, 0);
    Set_CI_int((Aflag_Tbl+CONTROL_MOPT),cur_val, 0);
    Set_CI_int((Aflag_Tbl+CONTROL_REG),cur_val, 0);
    Set_CI_int((Aflag_Tbl+CONTROL_SCHED),cur_val, 0);
    Set_CI_int((Aflag_Tbl+CONTROL_XOPT),cur_val, 0);
  }
}
