/*
 * Copyright 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
 */

#ifndef __IPA_CMDLINE_H__
#define __IPA_CMDLINE_H__


extern int arg_count;			    /* argument count */
extern char **arg_vector;		    /* argument vector */
extern char **environ_vars;		    /* list of environment variables */

extern int ipa_argc;
extern char **ipa_argv;
extern unsigned int max_gpa_size;
extern bfd_boolean is_ipa;
extern unsigned int used_gp_area;

extern bfd_boolean
ipa_search_command_line(int, char **, char **);



#endif /* __IPA_CMDLINE_H__ */
