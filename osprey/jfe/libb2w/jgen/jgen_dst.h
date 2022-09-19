#include <sys/types.h>
#include <sys/stat.h>
#include <sys/param.h>

#include <string>
#include <vector>
#include <map>

#ifdef NO_SUCE_CCC

extern "C" char *cplus_demangle (const char *, int);


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


#endif