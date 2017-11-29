/*
 * Copyright (C) 2009 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

/*
 * Copyright 2004 PathScale, Inc.  All Rights Reserved.
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
#include <stdio.h>
#include "option_seen.h"
#include "options.h"
#include "option_names.h"
#include "errors.h"

/*
 * option_seen keeps track of whether an option has been seen,
 * and the order in which options are seen. 
 */

/*
 * We have two arrays which point into each other.
 * The option_is_seen array has a value for each option flag
 * that says whether the option was seen, and if so, points
 * into the order_options_seen array.
 * The order_options_seen array is an ordered list of options 
 * that are seen, where the index is the order and the value 
 * points back to the option_is_seen array.
 * In order to allow prepending to the order seen, 
 * we make the order_options_seen array be a linked list.
 */
typedef struct int_list_rec {
	int value;
	struct int_list_rec *next;
} int_list, *int_list_p;
typedef struct linked_list_rec {
	int_list *head;
	int_list *tail;
} linked_list;
static linked_list order_options_seen; 
static int_list_p *option_is_seen;

static int_list* IMPLICITLY_SEEN;
#define NOT_SEEN NULL

static int_list* current_seen;	/* for iterator */

/* init the option_seen list */
void 
init_option_seen (void)
{
	int i;
	option_is_seen = (int_list_p*) malloc(max_options*sizeof(int_list_p));
	for (i = 0; i < max_options; i++) {
		option_is_seen[i] = NOT_SEEN;
	}
	/* make dummy entry for implicitly seen */
	order_options_seen.head = (int_list*)malloc(sizeof(int_list));
	order_options_seen.head->value = -1;
	order_options_seen.head->next = NULL;
	order_options_seen.tail = order_options_seen.head;
	IMPLICITLY_SEEN = order_options_seen.head;
	current_seen = 0;	/* better not be iterating here */
}

/* double the number of options available in our structures */
void
double_max_option_seen (void)
{
	/* we assume that max_options var has already been doubled */
	int base;
	int i;
	int_list_p *new_list = (int_list_p*) realloc ((char*)option_is_seen, 
		max_options*sizeof(int_list_p));
	if (new_list == NULL) {
		internal_error("Out of memory allocating %d options_seen",
			max_options);
	}
	option_is_seen = new_list;
	base = max_options/2;

	current_seen = 0;	/* better not be iterating here */
	for (i = base; i < max_options; i++) {
		option_is_seen[i] = NOT_SEEN;
	}
}

/* whether option flag was seen or not */
boolean 
option_was_seen (int optflag)
{
	return (option_is_seen[optflag] != NOT_SEEN);
}

/* whether option flag was implicitly seen or not */
boolean 
option_was_implicitly_seen (int optflag)
{
	return (option_is_seen[optflag] == IMPLICITLY_SEEN);
}

/* set option as seen */
void 
add_option_seen (int optflag)
{
	/* create cross-reference between option_is_seen and 
	 * order_options_seen for new option */
	int_list *p;
	p = (int_list*)malloc(sizeof(int_list));
	p->value = optflag;
	p->next = NULL;
	order_options_seen.tail->next = p;
	order_options_seen.tail = p;
	option_is_seen[optflag] = p; 
}

/* set option as seen, add to beginning of ordered list */
void 
prepend_option_seen (int optflag)
{
	/* create cross-reference between option_is_seen and 
	 * order_options_seen for new option */
	int_list *p;
	p = (int_list*)malloc(sizeof(int_list));
	p->value = optflag;
	/* keep implicit option at head, if first option then change tail */ 
	p->next = order_options_seen.head->next;
	order_options_seen.head->next = p;
	if (order_options_seen.head == order_options_seen.tail) {
		order_options_seen.tail = p;
	} 
	option_is_seen[optflag] = p; 
}

/* set option as implicitly seen */
void 
add_option_implicitly_seen (int optflag)
{
	/* just mark flag as being implicitly seen;
	 * don't put in order_option_seen array. */
	option_is_seen[optflag] = IMPLICITLY_SEEN; 
}

/* replace old option seen with new value, but in same place */
void
replace_option_seen (int old_optflag, int new_optflag)
{
	int_list* p = option_is_seen[old_optflag];
	option_is_seen[old_optflag] = NOT_SEEN;
	option_is_seen[new_optflag] = p;
	p->value = new_optflag;
}

static void
delete_order_option (int_list *old)
{
	int_list *p;
	/* assume that can't remove initial implicit option */
	for (p = order_options_seen.head; p != NULL; p = p->next) {
		if (p->next == old) {
			/* skip old */
			p->next = old->next;
			/* don't set next to NULL, as may need it 
			 * for current_seen */
			/* old->next = NULL; */
			if (p->next == NULL) {
				/* must be tail */
				order_options_seen.tail = p;
			}
			return;
		}
	}
	internal_error("couldn't unset option %d", old->value);
}

/* set option as unseen */
void
set_option_unseen (int optflag)
{
	int_list *p;
	p = option_is_seen[optflag];
	option_is_seen[optflag] = NOT_SEEN;
	if (p && p != IMPLICITLY_SEEN) delete_order_option(p);
}

/* iterator routines, e.g.:
 * for (i = first_option_seen(); more_option_seen(); i = next_option_seen())
 */

int 
first_option_seen (void)
{
	current_seen = order_options_seen.head->next;	/* skip implicit */
	if (current_seen == NULL) {
		return 0;
	} else {
		return current_seen->value;
	}
}

int 
next_option_seen (void)
{
	current_seen = current_seen->next;
	if (current_seen == NULL) {
		return 0;
	} else {
		return current_seen->value;
	}
}

boolean 
more_option_seen (void)
{
	return (current_seen != NULL);
}

/* return true if this option was repeated later */
/* Warning:  this routine only works if inside an iterator */
boolean
current_option_seen_later (int optflag)
{
	/* if was seen later, then option_is_seen will point to later use */
	return (current_seen != option_is_seen[optflag]);
}

/* set current option as unseen */
/* Warning:  this routine only works if inside an iterator */
void
set_current_option_unseen (void)
{
	delete_order_option(current_seen);
}

void
dump_option_seen (void)
{
	int_list *p;
	printf("order_options_seen:\n");
	for (p = order_options_seen.head; p != NULL; p = p->next) {
		printf("flag %d\n", p->value);
	}
}
