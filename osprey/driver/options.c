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


#include <malloc.h>
#include "phases.h"
#include "options.h"
#include "string_utils.h"
#include "errors.h"
#include "option_names.h"
#include "option_seen.h"

/*
 * Routines for handling the list of predefined options,
 * plus any user-defined options.
 */

/* use static array to keep predefined list of options */

typedef struct option_list_rec {
	int info_index;		/* index into option_info */
	char *name;
	struct option_list_rec *next;
} option_list_t;

typedef struct index_list_rec {
	int info_index;
	struct index_list_rec *next;
} index_list_t;

typedef struct option_info_rec {
	mask_t valid_langs;
	mask_t valid_phases;
	option_list_t *implies;
	char *name;
	char *help_msg;
} option_info_t;

/* the options array will be indexed by the option flag values */
static option_info_t *options;
int max_options = LAST_PREDEFINED_OPTION+100;

/* mark last option in options array;
 * last_option can be more than last_predefined_option,
 * cause implicit options are added by things like -W and -D
 */
static int last_option = LAST_PREDEFINED_OPTION;

/* return name */
char *
get_option_name (int flag)
{
	/* derived options may only have implied name */
	if (options[flag].name == NULL && options[flag].implies != NULL) {
		return options[flag].implies->name;
	} else {
		return options[flag].name;
	}
}

/* return help msg */
char *
get_option_help (int flag)
{
	return options[flag].help_msg;
}


/* whether option should have a blank space in it, e.g. -o foo */
boolean 
option_has_blank (int flag)
{
	return (options[flag].implies != NULL
           && has_blank(options[flag].implies->name));
}

/* set / add language for option */
void
set_language_for_option (int flag, languages_t l)
{
  options[flag].valid_langs = get_language_mask(l);
}

void
add_language_for_option (int flag, languages_t l)
{
  options[flag].valid_langs |= get_language_mask(l);
}

/* whether the option is valid for the language */
boolean 
option_matches_language (int flag, languages_t l)
{
  return (is_matching_language (options[flag].valid_langs, l));
}

/* Is the option internal? */
boolean 
is_internal_option (int flag)
{
  return (is_matching_language (options[flag].valid_langs, L_internal));
}

/* Set the option internal */
void 
set_internal_option (int flag)
{
  add_language_for_option ( flag, L_internal );
}

/* whether the option is valid for the phase */
boolean 
option_matches_phase (int flag, phases_t p)
{
	return (is_matching_phase (options[flag].valid_phases, p));
}

/* add phase to list of valid phases for option */
void
add_phase_for_option(int flag, phases_t p)
{
                options[flag].valid_phases |= get_phase_mask(p);
}

/* keep multi options for a phase */
void
keep_phase_for_option(int flag, char *opt)
{
                options[flag].implies->name = opt;
}

/* remove phase to list of valid phases for option */
void
remove_phase_for_option(int flag, phases_t p)
{
                options[flag].valid_phases &= ~(get_phase_mask(p));
}

/* double the size of the options list */
static void
double_max_options (void)
{
	max_options *= 2;
	options = (option_info_t *) realloc((char*)options, max_options*sizeof(option_info_t));
	double_max_option_seen();	/* synchronize other structures */
}

/* add new user-defined option */
int 
add_new_option (char *arg)
{
	option_list_t *p;
	if (last_option >= max_options) {
		double_max_options();
	}
	p = (option_list_t*)malloc(sizeof(option_list_t));
	p->name = string_copy(arg);
	p->info_index = last_option;	/* point to self */
	p->next = NULL;
	options[last_option].implies = p;
	options[last_option].name = NULL;
	options[last_option].help_msg = NULL;
	options[last_option].valid_langs = get_language_mask(L_ALL);
	options[last_option].valid_phases = get_phase_mask(P_NONE);
	last_option++;
	return last_option-1;
}

/*
 * For options with arbitrary argument, we have the option prefix in 
 * our predefined table/list of options.
 * Now we want to create a new derived entry for the exact option string,
 * which will point back to the prefix entry.  
 */
int 
add_derived_option (int parent, char *arg)
{
	option_list_t *pi;
	option_list_t *ni;
	char* loc;
	int new = add_new_option (arg);	/* arg is overwritten later */
	if (loc = strstr(arg, "roundoff")) {
          /* hack -- if roundoff has been specified, pass it to pfa/pca */
          roundoff = loc[9]; /* point to number in roundoff=n */
	}
	/* copy parent info to new entry */
	options[new].valid_langs = options[parent].valid_langs;
	options[new].valid_phases = options[parent].valid_phases;
	/* copy parent implies info to new entry */
	pi = options[parent].implies;
	ni = options[new].implies;
	/* info_index points to parent or parent's implied */
	ni->info_index = parent;
	while (pi != NULL) {
		ni->info_index = pi->info_index;
		ni->name = expand_template_string (pi->name, arg);
		pi = pi->next;
		if (pi != NULL) {
			/* create new implies item */
			ni->next = (option_list_t*)malloc(sizeof(option_list_t));
			ni = ni->next;
		}
	}
	ni->next = NULL;
	return new;
}

/* is option derived? */
boolean 
is_derived_option (int flag)
{
	return (options[flag].name == NULL);
}

/* return base parent */
int 
get_derived_parent (int flag)
{
	return options[flag].implies->info_index;
}

/*
 * iterator routines 
 * (should use FOREACH macros rather than individual routines)
 */
/* iterate through all known options.
 * iterate in reverse order because table is reverse alphabetical
 * #define FOREACH_OPTION(i)       \
 *       for (i = first_option(); !no_more_options(); i = next_option())
 */
static int current_option;

int 
first_option (void)
{
	current_option = last_option-1;
	return current_option;
}

int 
next_option (void)
{
	current_option--;
	return current_option;
}

boolean 
no_more_options (void)
{
	return (current_option == 0);
}


/*
 * iterate through implied options for a particular option.
 * #define FOREACH_IMPLIED_OPTION(i,o)	\
 *	for (i = first_implied_option(o); !no_more_implied_options(o); i = next_implied_option(o))
 */	
static option_list_t *current_implied;

int 
first_implied_option (int flag)
{
	current_implied = options[flag].implies;
	if (current_implied == NULL)
		return O_Unrecognized;
	else
		return current_implied->info_index;
}

int 
next_implied_option (int flag)
{
	current_implied = current_implied->next;
	if (current_implied == NULL)
		return O_Unrecognized;
	else
		return current_implied->info_index;
}

boolean 
no_more_implied_options (int flag)
{
	return (current_implied == NULL);
}

/* return name of current implied string;
 * MUST BE INSIDE IMPLIED ITERATOR when calling this routine! */
char *
get_current_implied_name (void)
{
	return (current_implied->name);
}

void
dump_option (int flag)
{
	option_list_t *pi = options[flag].implies;
	printf("dump option %d", flag);
	if (options[flag].name != NULL) {
		printf(" (%s)", options[flag].name);
	}
	printf("\n");
	printf("\tlangs = %llx, phases = %llx\n", options[flag].valid_langs, 
		options[flag].valid_phases);
	if (pi != NULL) {
		printf("\timplies:");
		while (pi != NULL) {
			printf("  %d(%s)", pi->info_index, pi->name);
			pi = pi->next;
		}
	}
	printf("\n");
}

#include "init_options.i"
