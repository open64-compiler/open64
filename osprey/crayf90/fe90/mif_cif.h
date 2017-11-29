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



/* USMID:  "\n@(#)3.0_pl/headers/mif_cif.h	3.2	12/20/96 16:58:53\n" */


# include <stdio.h>


# define EOI                  '\036'              /* End_Of_Item  (^)         */
# define EOR                  '\n'                /* End_Of_Record            */


/* CIF record [2].							      */

extern  void  Cif_Cifhdr_Rec(FILE *,		/* Compiler Information File. */
			     int,               /* Language code.	      */
			     char *, 		/* Compiler version.	      */
			     char *, 		/* Date of CIF creation in    */
						/* mm/dd/yy form.	      */
			     char *, 		/* Time of CIF creation in    */
						/* hh:mm:ss form.	      */
			     char *, 		/* Group id.		      */
			     int,		/* Message catalog file id.   */
			     char *, 		/* Host CPU name; for         */
						/* example, "hot".            */
			     char *); 		/* Host CPU type; for	      */
						/* example, "CRAY-C90".       */


/* CDIR$ record [5].							      */

extern  void  Cif_Cdir_Rec(FILE *,            	/* Compiler Information File. */
			   int,			/* Directive type.	      */
                           int,			/* File id.         	      */
                           int,			/* File line number.	      */
                           int,    		/* Column number.	      */
                           int,			/* Number of COPY variables.  */
                                                /* (for END MASTER only)      */
                           long *);		/* An array of COPY variable  */
						/* symbol ids.		      */


/* File Name record [7].						      */

extern  void  Cif_File_Rec(FILE *,		/* Compiler Information File. */
			   char *,      	/* Fully expanded file name.  */
                           int, 		/* File id.		      */
			   char *);		/* User-specified file name.  */


/* INCLUDE record [9].							      */

extern  void  Cif_Include_Rec(FILE *,		/* Compiler Information File. */
			      int,		/* Parent file id.	      */
                              int,		/* Current file line number.  */
                              int,		/* Column number.	      */
                              int);       	/* INCLUDE file id.	      */


/* Message record [11].							      */

extern  void  Cif_Message_Rec(FILE *,		/* Compiler Information File. */
			      int,		/* Message severity.	      */
			      int,		/* Message number.	      */
                              int,		/* File id.         	      */
                              int,		/* Global line number.	      */
                              int,	    	/* Column number.	      */
                              int,		/* File line number.	      */
                              int,		/* Number of message inserts. */
                              char **,          /* An array of message	      */
						/* inserts (strings). 	      */
                              char *,           /* Procedure name.            */
                              int,              /* Relative message order.    */
                              int,              /* Flags.                     */
                              int);             /* Physical file id.          */


/* Source File record [14].  						      */

extern  void  Cif_Srcfile_Rec(FILE *,	        /* Compiler Information File. */
			      int,		/* File id.         	      */
                              int);		/* Source form.		      */
                                                /* (0 = fixed; 1 = free)      */
    

/* Summary record [15].							      */

extern  void  Cif_Summary_Rec(FILE *,		/* Compiler Information File. */
			      char *,		/* Compiler release level.    */
                              char *,		/* Compiler generation date.  */
                              char *,		/* Compiler generation time.  */
                              char *,		/* Compilation time in the    */
     						/* form hh:mm:ss.zzz          */
                              long,		/* Max field length.	      */
			      int,		/* Total number of source     */
						/* lines compiled.	      */
                              int,		/* Code size.		      */
                              int);		/* Data size.		      */

/*  QUESTION:  Does Max field length need to be a long?  Should code size     */
/*             and data sizes also be longs?				      */


/* DOSHARED CDIR$ record [16].						      */

extern  void  Cif_Doshared_Cdir_Rec(FILE *,	/* Compiler Information File. */
				    int,	/* Doshared type.	      */
                                    int,	/* Random.		      */
                                    int,	/* File id.		      */
                                    int,	/* File line number.	      */
                                    int,	/* Column number.	      */
                                    char *,	/* M value.  		      */
                                    int,	/* M file id.		      */
                                    int,	/* M file line number.	      */
                                    int,	/* M column number.	      */
                                    int,	/* Number of control vars.    */
				    long *);    /* An array of control	      */
						/* variable symbol ids.       */

/*   Note:  This record is not yet output by CF90 and may have a different    */
/*          form depending on what happens to CRAFT.			      */


/* Unit record [17].							      */

extern  void   Cif_Unit_Rec(FILE *,		/* Compiler Information File. */
			   char *,		/* Compilation unit name.     */
       			   int,			/* File id.		      */
      			   int,			/* File line number.	      */
                           int);		/* Column number.	      */


/* End Unit record [18].						      */

extern  void  Cif_Endunit_Rec(FILE *,		/* Compiler Information File. */
		              char *,		/* Compilation unit name.     */
       		  	      int,		/* File id.		      */
      			      int,		/* File line number.	      */
                              int);		/* Column number.	      */


/* Usage record [19].							      */

extern  void  Cif_Usage_Rec(FILE *,             /* Compiler Information File. */
			    int,		/* Symbol id.		      */
    		  	    int,		/* File id.		      */
      			    int,		/* File line number.	      */
                            int, 		/* Column number.	      */
                            int, 		/* Usage code.	  	      */
			    int,		/* Number of other symbol ids.*/
			    long *);		/* An array of component      */
						/* symbol ids.      	      */
			    

/* Enable/Disable Options record [21].					      */

extern  void  Cif_EDopts_Rec(FILE *,   	        /* Compiler Information File. */
			     int);	        /* Word with bit flags set.   */


/* Machine Characteristics record [22].					      */

extern  void  Cif_Mach_Char_Rec(FILE *,         /* Compiler Information File. */
			        char *, 	/* CPU type.		      */
				long,		/* Memory speed.	      */
				long,		/* Memory size.		      */
				int,		/* Characteristics (bit flags)*/
				long,		/* Number of memory banks.    */
				long,		/* Number of CPUs.	      */
				long,		/* Instruction buffer size.   */
				long, 		/* Clock period.	      */
				long,		/* Num cluster reg sets.      */
				long, 		/* Bank busy time.	      */
			        int);		/* Word bit length.	      */

/*    Note:  Most of these fields are meaningful only for PVP machines.       */
/*           See the libcif or Fortran CIF documentation for the fields that  */
/*           are expected to contain values for other machines such as MPPs   */
/*           and SPARCs.						      */


/* Statement Type record [25].						      */

extern  void  Cif_Stmt_Type_Rec(FILE *,         /* Compiler Information File. */
				int,		/* Statement type.	      */
    		  	        int,		/* File id.		      */
      			        int,		/* File line number.	      */
                                int,            /* Column number.             */
                                int);           /* Statement number.          */
      			        

/* MPP Geometry record [26].						      */

/*   Note:  This record is not yet output by CF90 and may have a different    */
/*          form depending on what happens to CRAFT.			      */
/*									      */
/*          It will also need a declaration of the following type because a   */
/*          number of these can be passed:				      */

struct  Cif_geometry_dim_entry 
		     {
		       int	distribution; 	/* Dimension distribution.    */
		       char    *weight;		/* Weight.		      */
		       int      weight_file_id;	/* Weight file id.	      */
      		       int      weight_line_number;
						/* Weight file line number.   */
                       int      weight_column_number;
						/* Weight column number.      */
		       char    *block_size;	/* Block size.		      */
    		       int      block_size_file_id;
						/* Block size file id.	      */
    		       int      block_size_line_number;
						/* Block size file line num.  */
                       int 	block_size_column_number;
						/* Block size column number.  */
                     };

typedef struct  Cif_geometry_dim_entry   Cif_geometry_dim;


extern  void  Cif_Mpp_Geometry_Rec(FILE *,      /* Compiler Information File. */
				   char *,	/* Geometry name.	      */
				   int,		/* Geometry symbol id.        */
				   int,		/* Number of dimensions.      */
				   Cif_geometry_dim *);
						/* An array of geometry	      */
						/* dimension structures.      */
					


/* Continuation Line record [27].					      */

extern  void  Cif_Continuation_Rec(FILE *,      /* Compiler Information File. */
			           int,  	/* Continuation type.	      */
    		  	           int,		/* File id.		      */
      			           int,		/* File line number.	      */
                                   int); 	/* Column number.	      */
				        
/*    Note:  The front-end does not seem to be producing these yet.           */


/* Call Site record [28].						      */

extern  void  Cif_F90_Callsite_Rec(FILE *,      /* Compiler Information File. */
			           int,		/* Symbol id.		      */
			           int,		/* Scope id.		      */
	  	                   int,		/* File id.		      */
      		                   int,		/* File line number.	      */
                                   int,      	/* Column number.	      */
                                   int,  	/* Specific proc symbol id.   */
			           int,		/* Max number of actual args. */
			           char **,     /* An array of actual arg     */
						/* symbol ids.                */
			           int  *);	/* An array of actual arg     */
						/* ranks.		      */


/*        See the Fortran CIF documentation for why the symbol id in this     */
/*        case must be a character string (see "arg symbol id" description).  */


/* Common Block record [29].						      */

extern  void  Cif_F90_Comblk_Rec(FILE *,        /* Compiler Information File. */
			         char *,	/* Common block name.	      */
			         int,		/* Symbol id.		      */
			         int,		/* Scope id.		      */
	  	                 int,		/* Storage class.	      */
      		                 int,		/* Module symbol id.	      */
                                 int,		/* Byte length of common blk. */
   			         int);	        /* Distribution.	      */
				   

/* Named Constant record [30].						      */

extern  void  Cif_F90_Const_Rec(FILE *,	        /* Compiler Information File. */
				int,		/* Symbol id.		      */
				int,		/* Scope id.		      */
				int,		/* Scalar/Aggregate.          */
				char *,		/* Value.		      */
	  	                int,		/* File id.		      */
      		                int,		/* File line number (start).  */
                                int,  		/* Column number (start).     */
      		                int,		/* File line number (end).    */
                                int); 		/* Column number (end).       */
				     

/* Entry Point record [31].						      */

extern  void  Cif_F90_Entry_Rec(FILE *,       /* Compiler Information File. */
			        char *,		/* Entry point name.	      */
				int,		/* Entry point symbol id.     */
				int,		/* Scope id.		      */
				int,		/* Program unit type.	      */
				int, 		/* Procedure type.	      */
				int, 		/* A word containing bit      */
     						/* flags (attributes).	      */
			        int,		/* Result symbol id (if       */
						/* function).		      */
				int,		/* Module symbol id (if entry */
						/* point is a module proc).   */
				int,		/* Number of dummy arguments. */
						/* (only in definition form   */
						/* of this record).	      */
				long *);	/* Array of dummy argument    */
						/* symbol ids.		      */


/* Loop Definition record [32].						      */

extern  void  Cif_F90_Loop_Rec(FILE *,          /* Compiler Information File. */
			       int,		/* Scope id.		      */
			       int,		/* Loop type.		      */
			       int,		/* Start file id.	      */
			       int, 		/* Start line number.	      */
			       int,		/* Start column number.	      */
			       int,		/* End file id.		      */
			       int, 		/* End line number.	      */
			       int,		/* End column number.	      */
			       int,		/* DO-variable symbol id.     */
			       int, 		/* Loop term. label symbol id.*/
			       int, 		/* Construct name symbol id.  */
			       int);		/* Loop end stmt number.      */
	

/* Derived Type record [33].						      */

extern  void  Cif_F90_Derived_Type_Rec(FILE *,  /* Compiler Information File. */
				       char *,	/* Derived type name.	      */
				       int,	/* Symbol id.		      */
				       int, 	/* Scope id.		      */
				       int,	/* Derived type id.	      */
				       int,	/* Attribute flags.	      */
				       int,	/* Number of components.      */
				       long *,	/* Array of component symbol  */
						/* ids.			      */
				       int);    /* Symbol id of the module in */
        					/* which the derived type is  */
                                                /* defined; 0 otherwise.      */


/* Label record [34].							      */

extern  void  Cif_F90_Label_Rec(FILE *,      	/* Compiler Information File. */
			        char *,		/* Statement label or         */
						/* construct name.	      */
   		  	        int,		/* Symbol id.		      */
			        int,		/* Scope id.		      */
			        int);		/* Label type.		      */


/* Namelist record [35].						      */

extern  void  Cif_F90_Namelist_Rec(FILE *,      /* Compiler Information File. */
			           char *,	/* Namelist group name.	      */
			           int,		/* Symbol id.		      */
			           int,		/* Scope id.		      */
			           int,		/* Module symbol id (if the   */
						/* namelist group is defined  */
						/* in a module).	      */
			           int,		/* Number of group members.   */
			           long *);	/* An array of member symbol  */
						/* ids.			      */


/* Object record [36].							      */

/*    Note:  The Distribution and Geometry Id fields are not currently being  */
/*           output by the front-end.  These fields may change depending on   */
/*           the final CRAFT design.					      */

/* The bound information must be in character form because a bound can be an  */
/* expression which is represented by the character "E".  		      */

struct Cif_f90_dim_entry { char  *lower_bound;
                           char  *upper_bound;
                         };

typedef struct Cif_f90_dim_entry Cif_f90_dim;
 

extern  void  Cif_F90_Object_Rec
                     (FILE *,	              /* Compiler Information File.   */
	              char *,		      /* Object name.		      */
		      int,		      /* Symbol id.		      */
		      int,		      /* Scope id.		      */
		      int,		      /* Data type.		      */
		      int,		      /* Symbol class.		      */
		      int,		      /* Storage class.		      */
		      int,		      /* Storage symbol id.	      */
		      int,		      /* Offset.		      */
		      int,		      /* Attribute flags.	      */
		      int,		      /* Derived type id (if object   */
					      /* is a component).	      */
		      char *,		      /* Character length (if object  */
					      /* is of type char).	      */
		      int,		      /* Number of dimensions (0 if   */
					      /* object is scalar).           */
		      int,		      /* Array type (only if object   */
					      /* is an array).		      */
		      Cif_f90_dim *,          /* An array of bound info       */
					      /* structures.		      */
		      int,		      /* Distribution (only if object */
				 	      /* is an array).		      */
		      int,		      /* Geometry id (only if object  */
					      /* is an array). 		      */
		      int);		      /* Pointer symbol id (only if   */
					      /* object is a CRI pointee).  */


/* Miscellaneous Compiler Options record [37].				      */

extern  void  Cif_F90_Misc_Opts_Rec
                                (FILE *,	/* Compiler Information File. */
				 int,		/* -i option (integer length) */
						/* representation.	      */
				 int,		/* -m option (message level). */
				 int,		/* -V option (compilation     */
						/* info) enabled/disabled.    */
				 int,		/* -t option (truncation)     */
						/* enabled/disabled.          */
				 int,   	/* -t option (truncation)     */
						/* value.		      */
				 int,		/* Number of message numbers  */
						/* that are disabled.	      */
				 long *, 	/* An array of disabled       */
						/* message numbers.	      */
				 int,   	/* Number of CDIR$s/CMIC$s    */
						/* that are disabled.	      */
				 char **,	/* An array of CDIR$/CMIC$    */
 						/* names that are disabled.   */
				 char *,	/* File name of the .o being  */
						/* produced.		      */
				 char *,	/* -S option: (CAL) file name.*/
				 char *,	/* Inline file name.	      */
				 char *, 	/* Compiler Information File  */
					 	/* name.		      */
				 int,    	/* -C option mask (bit flags).*/
				 int, 	 	/* -N option value.	      */
				 int,	 	/* Number of -I options.      */
				 char **, 	/* Array of -I path names.    */
				 int,  	 	/* Number of -p options.      */
				 char **, 	/* Array of -p path names.    */
				 int,  	 	/* Source form flag.	      */
				 int);   	/* Run-time option mask (bit  */
					 	/* flags).		      */

/*       Note:  Since we've added many new options, surely this record        */
/*              will need to be expanded.				      */


/* Optimization Options record [38].					      */


struct Cif_f90_level_opt_entry {
                                int   option;   /* Option flag.	      */
                                int   level;    /* The optimization level.  */
                               };

typedef struct  Cif_f90_level_opt_entry  Cif_f90_level_opt;


extern  void  Cif_F90_Opt_Opts_Rec
		       (FILE *,                 /* Compiler Information File. */
                        int,		        /* Optimization options mask  */
					        /* (bit flags).		      */
			int,		        /* Number of level fields.    */
			Cif_f90_level_opt *);   /* An array of optimization   */
						/* level structures.	      */


/* Begin Scope record [39].						      */

extern  void  Cif_F90_Begin_Scope_Rec(FILE *,   /* Compiler Information File. */
				      int,	/* Scope id.		      */
				      int,	/* Symbol id.		      */
				      int,	/* File id.		      */
				      int,	/* File line number.	      */
				      int,	/* Column number.	      */
				      int,	/* Scope type.		      */
				      int,	/* Nesting level.	      */
			 	      int);	/* Parent scope id.	      */


/* End Scope record [40].						      */

extern  void  Cif_F90_End_Scope_Rec(FILE *,     /* Compiler Information File. */
				    int,	/* Scope id.		      */
				    int,	/* File id.		      */
			            int,	/* File line number.	      */
				    int,	/* Column number.	      */
				    int);	/* Scope in error flag.	      */


/* Scope Information record [41].					      */

extern  void  Cif_F90_Scope_Info_Rec(FILE *,    /* Compiler Information File. */
				     int,	/* Scope id.                  */
                                     int,       /* Miscellaneous attributes.  */
                                     int,       /* Number of alternate        */
						/* entry points.	      */
                                     long *);	/* An array of alternate      */
						/* entry point symbol ids.    */

/* Use Module record [42].						      */

extern  void  Cif_F90_Use_Module_Rec(FILE *,    /* Compiler Information File. */
				     int,	/* Module symbol id.	      */
				     int,	/* Module file id.	      */
				     int);	/* Module reference flag.     */


/* Rename record [43].							      */

extern  void  Cif_F90_Rename_Rec(FILE *,        /* Compiler Information File. */
			         int, 		/* Scope id.		      */
			         char *,	/* Name in module.	      */
			         int,		/* Name-in-module symbol id.  */
			         int,		/* Module symbol id.	      */
			         char *,	/* Original name.	      */
			         int,		/* Original module symbol id. */
			         long);		/* Local name symbol id.      */


/* Interface Block record [44].						      */

extern  void  Cif_F90_Int_Block_Rec(FILE *,   /* Compiler Information File. */
				    char *,	/* Interface block "name".    */
				    int,	/* Symbol id.		      */
				    int,	/* Scope id.		      */
				    int,	/* Interface block type.      */
				    int,	/* Attribute mask.	      */
				    int,	/* Number of interfaces.      */
				    long *, 	/* An array of specific proc  */
						/* symbol ids.		      */
			            int);       /* Symbol id of the module in */
        					/* which the interface block  */
                                                /* defined; 0 otherwise.      */


/* Command Line Options record [70].					      */

extern  void  Cif_Orig_Cmd(FILE *,		/* Compiler Information File. */
			   char *);		/* Options.		      */

		
/* Source Position record [86].						      */

extern  void  Cif_Src_Pos_Rec(FILE *,		/* Compiler Information File. */
			      int,		/* What the record is         */
						/* representing.	      */
       			      int, 	 	/* Source position id.        */
			      int,		/* Parent source position id. */
			      int,		/* Start line number.         */
			      int,		/* Start column number.       */
			      int,		/* File id.		      */
			      int,		/* End line number.	      */
			      int,		/* End column number.	      */
			      int);		/* Macro symbol id.	      */



/* Error recovery procedure.  Must be provided by the caller of these CIF     */
/* output routines.							      */

extern void     Cif_Error (void);



/* ************************************************************************** */
/*									      */
/*         ***  TEMPORARY DEFINITION UNTIL INTEGRATION INTO LIBCIF  ***       */
/*									      */
/* ************************************************************************** */

#define  CIF_SOURCE_POSITION	86

