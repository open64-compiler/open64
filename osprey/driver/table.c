/*
 * Copyright (C) 2009-2010 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

/*
 * Copyright 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
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


#include <stdio.h>
#include <string.h>
#include <malloc.h>
#include <ctype.h>
#include <stdlib.h>
#include <assert.h>
#include "basic.h"
#include "lang_defs.h"
#include "string_utils.h"
#include "errors.h"

#ifdef _WIN32
#define LL_FORMAT "I64"
#else
#define LL_FORMAT "ll"
#endif

#define EMPTY(x)	(x[0] == NIL)
#define SPACE		" 	"
#define DQUOTE		"\""
#define DQUOTE_SPACE	"\" 	"
#define COMMA		","
#define RPAREN		")"
#define FIELD_DIVIDER	'|'
#define OPTIONS 1

/* dummy for option referenced in lang_defs.c */
int external_gcc = 0;

typedef enum {
	normal, 
	oneletter, 		/* single-letter option */
	oneletter_space, 	/* single-letter followed by space and value */
	multiletter_space, 	/* >1 letters followed by space and value */
	complicated,		/* too complicated, parse by hand */
	needs_string,		/* option needs string arg */
	needs_string_or_dash,	/* same as needs_string but also allow '-' */
	needs_directory,	/* option needs directory arg */
	needs_directory_or_null, /* option takes directory arg; if no dir arg
				    is seen, ignore the option altogether */
	needs_decimal		/* option needs decimal arg */
} syntax_t; 

#define MAX_OPTION_LENGTH 48
#define MAX_NAME_LENGTH MAX_OPTION_LENGTH
#define MAX_ACTION_LENGTH 256

typedef struct option_list_rec {
	int info_index;		/* index into option_info */
	char name[MAX_OPTION_LENGTH];
	struct option_list_rec *next;
} option_list_t;

typedef struct index_list_rec {
	int info_index;
	boolean negated;
	struct index_list_rec *next;
} index_list_t;

typedef struct opt_info_rec {
	syntax_t syntax;
	int num_letters;	/* for multiletter_space */
	boolean toggle;		/* has toggle action */
	boolean internal;	/* not available to user */
	mask_t languages;
	mask_t phases;
	option_list_t *implies;
	char name[MAX_OPTION_LENGTH];
	char flag[MAX_OPTION_LENGTH];
	char action[MAX_ACTION_LENGTH];
	char *help;
} option_info_t;
#define MAX_OPTIONS 1000
static option_info_t options[MAX_OPTIONS];
static int num_options = 0;

static char implicit_vars[MAX_OPTIONS][MAX_NAME_LENGTH];
static int n_ivars = 0;

/*
 * table_for_phase means we don't need lang and phase masks,
 * because only have options for one phase, and can assume they are correct.
 */
static boolean table_for_phase = FALSE;

static const mask_t ALL_LANGS = -1;
static const mask_t ALL_PHASES = -1;
#define UNDEFINED	-1

#define index_last_char(s)	(strlen(s)-1)

static int
find_name(char *s)
{
	int i;
	size_t len;
	for (i = 0; i < num_options; i++) {
		switch(options[i].syntax) {
		case needs_decimal:
			/* must be name followed by digit or %d */
			len = strlen(options[i].name);
			if ((strncmp(s, options[i].name, len) == 0)
			    && (isdigit(s[len]) 
				|| (s[len] == '%' && s[len+1] == 'd')
				|| (s[len] == ' ' && s[len+1] == '%' && s[len+2] == 'd')
				) )
			{
				return i;
			}
			break;
		case needs_string_or_dash:
		case needs_string:
		case needs_directory:
		case needs_directory_or_null:
			len = strlen(options[i].name);
			if ((strncmp(s, options[i].name, len) == 0)
			    /* isgraph includes underscore and % */
			    && (isgraph(s[len])
				|| (s[len] == ' ' && isgraph(s[len+1])) ) )
			{
				return i;
			}
			break;
		default:
			if (strcmp(s, options[i].name) == 0)
				return i;
			break;
		}
	}
	internal_error("didn't find name %s", s);
	return UNDEFINED;	/* not found */
}

static int
get_line (char *line)
{
	char c;
	char *p = line;
	while (1) {
		c = (char) getchar();
		if (c == (char) EOF) return EOF;
		else if (c == '\n') {
			*p = NIL;
			return 0;
		} else {
			*p = c;
			p++;
		}
	}
}

static syntax_t
set_option_name(char *name, char *s, int *num_letters)
{
	syntax_t f = normal;
	char *pn = name;
	char *ps = s;
	*num_letters = 0;
	while (*ps != NIL) {
		if (*ps == '*') {
			if (*(ps+1) == NIL)
				f = complicated;
			else if (ps == s+2)	/* e.g. -B* */
				f = oneletter_space;
			else {
				f = multiletter_space;
				*num_letters = (ps - s - 1);
			}
		} else if (*ps == '%') {
			switch (*(ps+1)) {
			case 's':
				f = needs_string;
				break;
			case '-':
				f = needs_string_or_dash;
				break;
			case 'd':
			case 'x':
				f = needs_decimal;
				break;
			case 'D':
				f = needs_directory;
				if (*(ps+2) == '?') {
				  ps++;
				  f = needs_directory_or_null;
				}
				break;
			}
			ps++;
		} else {
			*pn = *ps;
			pn++;
		}
		ps++;
	}
	*pn = NIL;
	return f;
}

/* convert -foo option name into O_foo flag name */
static void
set_flag_name(option_info_t *opt)
{
	char *pn = opt->name;
	char *pf = opt->flag;
	*pf++ = 'O';
	while (*pn != NIL) {
		if (*pn == ' ') pn++;
		if (*pn == '!') pn++;
		if (*pn == '.') pn++;
		if (*pn == '-') *pf = '_';
		else if (*pn == '=') *pf = 'Q';
		else if (*pn == '#') *pf = 'P';
		else if (*pn == ',') *pf = 'C';
		else if (*pn == ':') *pf = '_';
		else if (*pn == '+') *pf = '_';
		else *pf = *pn;
		pf++;
		pn++;
	}
	switch (opt->syntax) {
	case needs_directory:
	case needs_decimal:
	case complicated:
	case needs_directory_or_null:
		/* add trailing __ to distinguish with no-arg case */
		/* use two _ to distinguish from -I- case */
		*pf = '_';
		pf++;
		*pf = '_';
		pf++;
		break;
	}
	*pf = NIL;
}

/* store src into tgt unless src == special */
static void
store_string(char *tgt, char *src, char *special)
{
	char *p;
	if (strcmp(src, special) == 0) {
		*tgt = NIL;
	} else if (*src != '{') {
		strcpy(tgt, src);
	} else {
		/* string surrounded by { }, can have white-spaces */
		strcpy(tgt, src);
		strcat(tgt, " ");
		p = strtok(NULL, "}"); 
		strcat(tgt, p);
		strcat(tgt, "}");
	}
}

static option_list_t *
create_option_list (char **sp, int index)
{
	/* have to return current pointer in sp after strtoks */
	char *s = *sp;
	option_list_t *olist;
	option_list_t *olist_head;
	s = strtok(NULL, DQUOTE_SPACE); 
	*sp = s;
	if (s == NULL) {
		return NULL;
	} else if (*s == FIELD_DIVIDER) {
		return NULL;
	} else {
		olist = (option_list_t *) malloc(sizeof(option_list_t)); 
		olist->next = NULL;
		olist_head = olist;
		if (strcmp(s, "self") == 0) {
			strcpy(olist->name, options[index].name);
			olist->info_index = index;
		} else {
			strcpy(olist->name, s);
			olist->info_index = UNDEFINED;
		}
	}
	s = strtok(NULL, DQUOTE_SPACE); 
	while (s != NULL && *s != FIELD_DIVIDER) {
		if (s[0] != '-') {
			/* append to previous item, with a space */
			strcat(olist->name, " ");
			strcat(olist->name, s);
		} else {
			olist = (option_list_t *) malloc(sizeof(option_list_t));
			strcpy(olist->name, s);
			olist->info_index = UNDEFINED;
			/* prepend, so in reverse order */
			olist->next = olist_head;
			olist_head = olist;
		}
		s = strtok(NULL, DQUOTE_SPACE); 
	}
	*sp = s;
	return olist_head;
}

static char *
list_to_string (option_list_t *list)
{
	option_list_t *p;
	char *buf;
	if (list == NULL) {
		return NULL;
	} else {
		buf = (char *) malloc(80);
		strcpy(buf, list->name);
	}
	for (p = list->next; p != NULL; p = p->next) {
		strcat(buf, " ");
		strcat(buf, p->name);
	}
	return buf;
}

static mask_t
create_lang_field (char *s)
{
	char *p;
	mask_t bitfield = 0;
	for (p = s; *p != NIL; p++) {
		if (*p == ',') continue;
		bitfield |= get_language_mask(get_language(*p));
	}
	return bitfield;
}

static mask_t
create_phase_field (char *s)
{
	char *p;
	mask_t bitfield = 0;
	for (p = s; *p != NIL; p++) {
		if (*p == ',') continue;
		bitfield |= get_phase_mask(get_phase(*p));
	}
	return bitfield;
}

static char *
create_help_msg (char *s)
{
	if (*s != FIELD_DIVIDER) 
		internal_error("| help msg expected: %s", s);
	s = strtok(NULL, DQUOTE);
	if (s != NULL && !isspace(*s)) 
		internal_error("| help msg expected: %s", s);
	s = strtok(NULL, DQUOTE);
	if (s == NULL) return NULL;
	else return string_copy(s);
}

#define INCREMENT_INDEX(i,max)\
	i++;\
	if (i >= max) internal_error("INDEX OVERFLOWED %d", max);

/* String comparison, in *reverse* alphabetical order. 
   Comparison function passed to qsort. */
static int reverse_strcmp(const void* x, const void* y) {
  const char * const*x1 = x;
  const char * const*y1 = y;

  return strcmp(*y1, *x1);
}

/*
 * Find options that have the same mangled name.
 */
option_info_t *find_by_flag(char *name, int last)
{
	int i;

	for (i = 0; i < last; i++) {
		if (strcmp(options[i].flag, name) == 0)
			return &options[i];
	}
	return NULL;
}

/*
 * Compare options for approximate equality.
 */
static int compare_options(option_info_t *a, option_info_t *b)
{
	option_list_t *oa, *ob;
	
	if (a->languages != b->languages)
		return 0;
	if (a->phases != b->phases)
		return 0;
	if (a->phases != b->phases)
		return 0;
	if (strcmp(a->action, b->action) != 0)
		return 0;

	for (oa = a->implies, ob = b->implies;; oa = oa->next, ob = ob->next) {
		if ((oa != NULL) ^ (ob != NULL))
			return 0;
		if (strcmp(oa->name, ob->name) != 0)
			return 0;
	}
	
	return 1;
}

/*
 * Check repeated definitions of a flag, to ensure they are all
 * identical.
 */
static void check_dups(void)
{
	int i;
	
	for (i = 1; i < num_options; i++) {
		option_info_t *opt = find_by_flag(options[i].flag, i);
		if (opt == NULL)
			continue;
		if (!compare_options(&options[i], opt)) {
			fprintf(stderr, "Error: %s and %s are not identical\n",
				options[i].name, opt->name);
			exit(1);
		}
	}
}

/*
 * Read table from stdin, without assuming options section is sorted.
 */
static void read_table(void) 
{
	char line[512];
	char line2[512];
        char *option_lines[MAX_OPTIONS];
        int option_line_count = 0;
	char *p;
	int section = 0;
	int i = num_options;
        int j;

	strcpy(options[i].flag, "O_Unrecognized");
	options[i].internal = TRUE;
	strcpy(options[i].name, "");
	options[i].implies = NULL;
	i++;

        option_line_count = 0;
        while (get_line(line) != EOF) {
            char* s;

	    if (EMPTY(line)) 
              continue;
	    else if (line[0] == '%')
              continue;
	    else if (line[0] == ' ' && line[1] == NIL)
              continue;
	    else if (line[0] == '\t')
                internal_error("unexpected line (help msg?): %s", line);
	    else if (line[0] != '-' && line[0] != 'I')
                internal_error("unexpected line: [%s]", line);

	    if (get_line(line2) == EOF)
                internal_error("unexpected EOF for line2");

            s = malloc(strlen(line) + strlen(line2) + 4);
            if (s == NULL)
              internal_error("memory allocation failed");
	    sprintf(s, "%s %c %s", line, FIELD_DIVIDER, line2);

            option_lines[option_line_count] = s;
            INCREMENT_INDEX(option_line_count, MAX_OPTIONS);
        }
         
        qsort(option_lines, option_line_count, sizeof(char*),
              reverse_strcmp);

        /* Process the option lines */
        for (j = 0; j < option_line_count; ++j) {
          /* <name> <action> <langs> <phases> <implies> */
            p = strtok(option_lines[j], SPACE);
            if (*p == 'I') {
              options[i].internal = TRUE;
              p++;
            } else
              options[i].internal = FALSE;
            if (*p != '-') 
              internal_error("MISSING - : %s", p);
            options[i].syntax = 
              set_option_name(options[i].name, p, &options[i].num_letters);
            if (options[i].syntax == normal 
                && strlen(options[i].name) == 2)
              options[i].syntax = oneletter;
            set_flag_name(&options[i]);
            p = strtok(NULL, SPACE); 
            store_string(options[i].action, p, ";");
            if (strncmp(p, "toggle", 6) == 0)
              options[i].toggle = TRUE;
            else
              options[i].toggle = FALSE;
            if (!table_for_phase) {
              p = strtok(NULL, SPACE); 
              options[i].languages = create_lang_field(p);
              if ( options[i].internal ) {
                options[i].languages |=
                  get_language_mask ( L_internal );
              }
              p = strtok(NULL, SPACE); 
              options[i].phases = create_phase_field(p);
            }
            options[i].implies = create_option_list(&p,i);
            options[i].help = create_help_msg(p);
            INCREMENT_INDEX(i, MAX_OPTIONS);
        }

	check_dups();
	

        num_options = i;
}

static void
fill_option_info (void)
{
	int i;
	option_list_t *p;
	index_list_t *pc;
	char *s;
	char buffer[MAX_OPTION_LENGTH];
	for (i = 0; i < num_options; i++) {
		p = options[i].implies;
		while (p != NULL) {
		    if (p->info_index == UNDEFINED) {
		    	p->info_index = find_name(p->name);
		    }
		    p = p->next;
		}
	}
}

static void
dump_option_info (void)
{
	int i;
	for (i = 0; i < num_options; i++) {
		if (!EMPTY(options[i].action))
			printf("%s: action: %s\n", options[i].name, 
				options[i].action);
	}
}

static void
add_implicit_var (char *s)
{
	int i;
	for (i = 0; i < n_ivars; i++) {
		if (strcmp(s, implicit_vars[i]) == 0) {
			/* already in list */
			return;
		}
	}
	strcpy(implicit_vars[n_ivars], s);
	n_ivars++;
}

static FILE *
begin_file (char *name)
{
	FILE *f;
	f = fopen(name, "w");
	fprintf(f, "/* THIS FILE IS AUTOMATICALLY GENERATED BY table */\n\n");
	return f;
}

static void
write_option_names (void)
{
	int i;
	char *ivar;
	buffer_t buffer;
	FILE *f;
	f = begin_file("option_names.h");
	for (i = 0; i < num_options; i++) {
		if (!find_by_flag(options[i].flag, i))
			fprintf(f, "#define %s %d\n", options[i].flag, i);
		if (options[i].toggle) {
			/* get var after "toggle(&" */
			// Handle multiple toggles such as:
			//   toggle(&var1,10);toggle(&var2,11)
			char *p = options[i].action;
			while ((p = strstr(p, "toggle(&")) != NULL) {
			  p = p + 8;
			  strcpy(buffer, p);
			  ivar = strtok(buffer,COMMA);
			  add_implicit_var(ivar);
			}
		}
	}
	fprintf(f, "#define LAST_PREDEFINED_OPTION %d\n", num_options);
	fprintf(f, "extern int max_options;\n\n");
	for (i = 0; i < n_ivars; i++) {
		fprintf(f, "extern int %s;\n", implicit_vars[i]);
	}
	fclose(f);
}

static void
write_init_options (void)
{
	int i;
	option_list_t *q;
	FILE *f;
	f = begin_file("init_options.i");
	fprintf(f, "#include \"option_names.h\"\n");
	fprintf(f, "#include \"opt_actions.h\"\n\n");
	for (i = 0; i < n_ivars; i++) {
		fprintf(f, "int %s = UNDEFINED;\n", implicit_vars[i]);
	}
	fprintf(f, "\n");
	fprintf(f, "static void\n");
	if (table_for_phase) {
		fprintf(f, "create_option_info (int flag, char *name, char *help_msg)\n");
		fprintf(f, "{\n");
	} else {
		fprintf(f, "create_option_info (int flag, mask_t lang_mask, mask_t phase_mask, char *name, char *help_msg)\n");
		fprintf(f, "{\n");
		fprintf(f, "\toptions[flag].valid_langs = lang_mask;\n");
		fprintf(f, "\toptions[flag].valid_phases = phase_mask;\n");
	}
	fprintf(f, "\toptions[flag].implies = NULL;\n");
	fprintf(f, "\toptions[flag].name = string_copy(name);\n");
	fprintf(f, "\toptions[flag].help_msg = string_copy(help_msg);\n");
	fprintf(f, "}\n\n");
	fprintf(f, "static void\n");
	fprintf(f, "create_implies_item (int key, int index, char *name)\n");
	fprintf(f, "{\n");
	fprintf(f, "\toption_list_t *p;\n");
	fprintf(f, "\tp = (option_list_t *) malloc(sizeof(option_list_t));\n");
	fprintf(f, "\tp->info_index = index;\n");
	fprintf(f, "\tp->name = string_copy(name);\n");
	fprintf(f, "\tp->next = options[key].implies;\n");
	fprintf(f, "\toptions[key].implies = p;\n");
	fprintf(f, "}\n\n");
	fprintf(f, "\nextern void\n");
	fprintf(f, "init_options (void)\n");
	fprintf(f, "{\n");
	fprintf(f, "\toptions = (option_info_t *) malloc(max_options*sizeof(option_info_t));\n");

	for (i = 0; i < num_options; i++) {
		if (find_by_flag(options[i].flag, i))
			continue;
		if (table_for_phase) {
		    fprintf(f, "\tcreate_option_info(%d,\"%s\",",
			    i, options[i].name);
		} else {
		    if (sizeof(mask_t)>4)
		     {
		     fprintf(f, "\tcreate_option_info("
			    "%d,%#" LL_FORMAT "xLL,%#" LL_FORMAT "xLL,\"%s\",",
			    i, options[i].languages, options[i].phases, options[i].name);
		     }
		    else
		     {
		     fprintf(f, "\tcreate_option_info("
			    "%d,%#" LL_FORMAT "x,%#" LL_FORMAT "x,\"%s\",",
			    i, options[i].languages, options[i].phases, options[i].name);
		     }
		}
		if (options[i].help == NULL)
			fprintf(f, "NULL);\n");
		else 
			fprintf(f, "\"%s\");\n", options[i].help);
		q = options[i].implies;
		while (q != NULL) {
			/* we write reversed lists back in reverse order,
			 * so now in correct order */
			fprintf(f, "\tcreate_implies_item(%s, %d, \"%s\");\n",
				options[i].flag, q->info_index, q->name);
			q = q->next;
		}
	}
	fprintf(f, "}\n\n");
	fclose(f);
}

static void
write_get_option (void)
{
	int i, j;
	FILE *f;
	f = begin_file("get_option.i");
	fprintf(f, "/* return option index */\n");
	fprintf(f, "extern int\n");
	fprintf(f, "get_option(int *argi, char *argv[])\n");
	fprintf(f, "{\n");
	fprintf(f, "/* don't reset optargs, as may contain needed info */\n");
	fprintf(f, "optargd = 0;\n");
	fprintf(f, "switch (argv[*argi][optindex]) {\n");
	i = 0;
	while (i < num_options) {
		if (options[i].internal) {
			/* skip */
			i++;
			continue;
		}
		if (find_by_flag(options[i].flag, i))
			continue;
		if (options[i].name[1] == '\0')	/* special case "-" */
			fprintf(f, "case '\\0':\n");
		else
			fprintf(f, "case '%c':\n", options[i].name[1]);
		while (1) {
		    switch (options[i].syntax) {
		    case normal:
		    	fprintf(f, "\tif (strcmp(argv[*argi],\"%s\") == 0) {\n",
				options[i].name);
			fprintf(f, "\t\tget_next_arg(argi);\n");
			fprintf(f, "\t\treturn %s;\n", options[i].flag);
			fprintf(f, "\t\t/* NOTREACHED */\n");
			fprintf(f, "\t}\n");
			break;
		    case oneletter:
			fprintf(f, "\tget_next_char(argv, argi);\n");
			fprintf(f, "\treturn %s;\n", options[i].flag);
			fprintf(f, "\t/* NOTREACHED */\n");
			break;
		    case oneletter_space:
		    case multiletter_space:
			if (options[i].syntax == multiletter_space) {
				char *temp = string_copy(options[i].name);
				int nil_index = options[i].num_letters+1;
				temp[nil_index] = NIL;
				fprintf(f, "\tif ((strncmp(argv[*argi],\"%s\",%d) == 0)\n",
					temp, nil_index);
				fprintf(f, "\t&& strcmp(next_string_after(\"%s\",argv,argi),\"%s\") == 0) {\n",
				    temp, options[i].name+nil_index);
				fprintf(f, "\t\tend_option(argv, argi, %d);\n", nil_index);
			} else {
				fprintf(f, "\tif (strcmp(next_string(argv,argi), \"%s\") == 0) {\n",
					options[i].name+2);
			}
			fprintf(f, "\t\toptargs = get_optarg(argv,argi);\n");
			fprintf(f, "\t\tget_next_arg(argi);\n");
			fprintf(f, "\t\treturn %s;\n", options[i].flag);
			fprintf(f, "\t\t/* NOTREACHED */\n");
			fprintf(f, "\t}\n");
			break;
		    case needs_string_or_dash:
		    case needs_string:
		    case needs_directory:
		    case needs_decimal:
		    case needs_directory_or_null:
			/* require this to be a separate arg */
			fprintf(f, "\tif (!is_new_arg) break;\n");
			/* for cases where base is > 1 letter,
			 * check that whole string matches */
			if (strlen(options[i].name) > 2) {
				fprintf(f, "\tif (strncmp(argv[*argi],\"%s\",%ld) == 0) {\n",
				    options[i].name, (long)strlen(options[i].name));
				fprintf(f, "\t\tend_option(argv, argi, %ld);\n",
					(long)strlen(options[i].name));
			} /* else -<single-letter> */
			if (options[i].syntax == needs_decimal) {
				fprintf(f, "\tif (is_decimal(next_string(argv,argi))) {\n");
			} else if (options[i].syntax == needs_directory
				   || options[i].syntax == needs_directory_or_null
				   ) {
			  fprintf(f, "\tif (!is_directory(next_string(argv,argi)) && fullwarn)\n");
			  fprintf(f, "\t\twarning(\"%%s is not a directory\", next_string(argv,argi));\n");
			  fprintf(f, "\tif (strcmp(next_string(argv,argi),\"-default_options\")) {\n" );

			}
			fprintf(f, "\t\toptargs = get_optarg(argv, argi);\n");
			if (options[i].syntax == needs_decimal) {
				fprintf(f, "\t\toptargd = atoi(optargs);\n");
			}
			fprintf(f, "\t\tget_next_arg(argi);\n");
			if (options[i].syntax == needs_string_or_dash)
			  fprintf(f, "\t\treturn add_string_option_or_dash(%s,optargs);\n", 
				  options[i].flag);
			else if (options[i].syntax == needs_directory)
			  fprintf(f, "\t\treturn add_any_string_option(%s,optargs);\n", 
				  options[i].flag);
			else
			  fprintf(f, "\t\treturn add_string_option(%s,optargs);\n", 
				  options[i].flag);
			fprintf(f, "\t\t/* NOTREACHED */\n");
			if (options[i].syntax != needs_string
			    && options[i].syntax != needs_string_or_dash
					) {
				fprintf(f, "\t}\n");
			}
			if (strlen(options[i].name) > 2) {
				fprintf(f, "\t}\n");
			}
			break;
		    case complicated:
			/* for cases where base is > 1 letter,
			 * check that whole string matches */
			if (strlen(options[i].name) > 2) {
				fprintf(f, "\tif (strncmp(argv[*argi],\"%s\",%ld) == 0) {\n",
				    options[i].name, (long)strlen(options[i].name));
			}
			fprintf(f, "\t\treturn parse_%s_option(argv, argi);\n",
				options[i].name+1);
			fprintf(f, "\t\t/* NOTREACHED */\n");
			if (strlen(options[i].name) > 2) {
				fprintf(f, "\t}\n");
			}
			break;
		    }
		    if (i+1 < num_options 
			&& options[i+1].name[1] == options[i].name[1])
		    { 
			/* stay within the case stm */
			i++;
		    } else {
			break;	/* goto next case */
		    }
		}
		fprintf(f, "\tbreak;\n");
		i++;
	}
	fprintf(f, "}\n");
	fprintf(f, "optargs = current_string(argv,argi);\n");
	fprintf(f, "get_next_arg(argi);\n");
	fprintf(f, "return O_Unrecognized;\n");
	fprintf(f, "}\n");

	/* now write alias routine */
	fprintf(f, "/* if alias, return real option index */\n");
	fprintf(f, "extern int\n");
	fprintf(f, "get_real_option_if_aliased(int flag)\n");
	fprintf(f, "{\n");
	fprintf(f, "  switch (flag) {\n");
	for (i = 0; i < num_options; i++) {
	    /* alias is option with:
	     * no action, one implies, all langs, no phases.
	     */
	    if (options[i].languages == get_language_mask(L_ALL)
		&& options[i].phases == 0
		&& EMPTY(options[i].action)
		&& options[i].implies != NULL
		&& options[i].implies->next == NULL)
	    {
		j = options[i].implies->info_index;
		if (options[i].syntax == normal
		 || options[i].syntax == oneletter)
		{
			if (options[j].syntax == normal) {
				fprintf(f, "\tcase %s: return %s;\n", 
					options[i].flag, 
					options[j].flag);
			}
			else if (options[j].syntax == needs_string ||
				 options[j].syntax == needs_string_or_dash) {
				fprintf(f, "\tcase %s: {\n", options[i].flag);
				fprintf(f, "\t\toptargs = \"%s\";\n",
					options[i].implies->name 
					  + strlen(options[j].name));
				fprintf(f, "\t\treturn add_string_option(%s,optargs);\n",
					options[j].flag);
				fprintf(f, "\t\t}\n");
			}
			else continue;	/* don't know how to handle */
		}
		else if (options[i].syntax == options[j].syntax) {
			/* will catch this on derived case */
			fprintf(f, "\tcase %s: return %s;\n", 
				options[i].flag, 
				options[j].flag);
		}
		else continue;	/* don't know how to handle */
	    }
	}
	fprintf(f, "\tdefault: return flag;\n");
	fprintf(f, "  }\n");
	fprintf(f, "}\n");
	fclose(f);
}

static void
write_opt_action (void)
{
	int i;
	buffer_t buf;
	char *tvar;
	char *tval;
	FILE *f;
	f = begin_file("opt_action.i");
	fprintf(f, "#include \"string_utils.h\"\n\n");
	fprintf(f, "#include \"get_options.h\"\n\n");
	fprintf(f, "#include \"lib_phase_dir.h\"\n\n");
	fprintf(f, "/* do action associated with option */\n");
	fprintf(f, "extern void\n");
	fprintf(f, "opt_action (int optflag)\n");
	fprintf(f, "{\n");
	fprintf(f, "switch (optflag) {\n");

	for (i = 0; i < num_options; i++) {
		if (!options[i].internal &&
		    //options[i].syntax != combo &&
		    !EMPTY(options[i].action) &&
		    find_by_flag(options[i].flag, i) == NULL) {
			fprintf(f, "case %s:\n", options[i].flag);
			fprintf(f, "\t%s\n", options[i].action);
			fprintf(f, "\tbreak;\n");
		}
	}
	fprintf(f, "}\n");
	fprintf(f, "}\n\n");
	fprintf(f, "/* return whether to mark this flag as unseen */\n");
	fprintf(f, "extern boolean\n");
	fprintf(f, "flag_is_superceded (int optflag)\n");
	fprintf(f, "{\n");
        fprintf(f, "switch (optflag) {\n");
	for (i = 0; i < num_options; i++) {
		if (options[i].toggle) {
			if (find_by_flag(options[i].flag, i))
				continue;

			fprintf(f, "case %s:\n", options[i].flag);
			/* get var after "toggle(&" */
			strcpy(buf, options[i].action+8);
			tvar = strtok(buf,COMMA);
			/* get value after "toggle(&<var>," */
			tval = strtok(NULL,RPAREN);
			fprintf(f, "\tif (%s != %s) return TRUE;\n", tvar, tval);
			fprintf(f, "\tbreak;\n");
		}
	}
        fprintf(f, "}\n");
        fprintf(f, "return FALSE;\n");
	fprintf(f, "}\n\n");
	fclose(f);
}


static void
write_implicits (void)
{
	int i;
	FILE *f;
	f = fopen("implicits.c", "w");
	fprintf(f, "/* THIS FILE IS AUTOMATICALLY GENERATED BY table */\n\n");
	fprintf(f, "#include \"string_utils.h\"\n\n");
	fprintf(f, "#include \"options.h\"\n");
	fprintf(f, "#include \"option_seen.h\"\n");
	fprintf(f, "#include \"option_names.h\"\n");
	fprintf(f, "#include \"opt_actions.h\"\n");
	fprintf(f, "#include \"get_options.h\"\n\n");
	fprintf(f, "#include \"errors.h\"\n");
	fprintf(f, "extern void\n");
	fprintf(f, "toggle_implicits (void)\n");
	fprintf(f, "{\n");
	for (i = 0; i < num_options; i++) {
		if (options[i].toggle) {
			if (strstr(options[i].action, "optarg") != NULL) {
				/* don't want to toggle if toggle value is
				 * an optarg that is only valid at parsing */
				continue;
			}
			fprintf(f, "\tif (option_was_implicitly_seen(%s)) {\n",
				options[i].flag);
			fprintf(f, "\t\toption_name = get_option_name(%s);\n",
				options[i].flag);
			fprintf(f, "\t\t%s\n", options[i].action);
			fprintf(f, "\t}\n");
		}
	}
	fprintf(f, "}\n\n");
	fclose(f);
}

int
main (int argc, char *argv[])
{
	boolean debug = FALSE;
	program_name = argv[0];
	if (argc == 2 && strcmp(argv[1], "-phase") == 0) {
		table_for_phase = TRUE;
	} 
	read_table();
	fill_option_info();
	if (debug) dump_option_info();
	/* create files from info */
	write_option_names();
	write_init_options();
	write_get_option();
	write_opt_action();
	write_implicits();
	return(error_status);
}
