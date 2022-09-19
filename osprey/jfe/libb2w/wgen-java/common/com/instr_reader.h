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


//-*-c++-*-
// ====================================================================
// ====================================================================
//
// Module: instr_reader.h
// $Revision: 1.1.1.1 $
// $Date: 2005/10/21 19:00:00 $
// $Author: marcel $
// $Source: /proj/osprey/CVS/open64/osprey1.0/common/com/instr_reader.h,v $
//
// Revision history:
//  10-Aug-98 - Original Version
//
// Description:
//
// ====================================================================
// ====================================================================

#ifndef instr_reader_INCLUDED
#define instr_reader_INCLUDED

#include <stdio.h>
#include "profile_com.h"
#include "fb_info.h"
#ifndef _BUILD_INSTR
#include "mempool_allocator.h"
#include "cxx_memory.h"
#endif

#ifdef _USE_PROFILE_ERROR_ROUTINES

// For standalone instr_reader and libinstr use the error 
// reporting mechanism of libinstr.
# include "profile_errors.h"

#else

// When the backend uses instr_reader, it needs to use its own
// error reporting routines.
#include "errors.h"
#define profile_error(arg1, arg2) Fatal_Error(arg1, arg2)

#endif /* _USE_PROFILE_ERROR_ROUTINES */


#define FSEEK(_fd, _position, whence, _error_message, _error_arg) \
     if (fseek(_fd, _position, whence) != 0) \
           profile_error(_error_message, _error_arg);

#define FREAD(_buffer, _size, _nitems, _fp, _error_message, _error_arg) \
        if (fread((void *)_buffer, _size, _nitems, _fp) != _nitems) \
           profile_error(_error_message, _error_arg);

#ifdef _BUILD_INSTR

// we can't use the definitions in fb_info.h because we don't have mempool
// in the standalone version
typedef vector<FB_Info_Invoke>	FB_Invoke_Vector;
typedef vector<FB_Info_Branch>	FB_Branch_Vector;
typedef vector<FB_Info_Loop>	FB_Loop_Vector;
typedef vector<FB_Info_Circuit> FB_Circuit_Vector;
typedef vector<FB_Info_Call>	FB_Call_Vector;
typedef vector<FB_Info_Icall>	FB_Icall_Vector;
typedef vector<FB_Info_Switch>	FB_Switch_Vector;
typedef vector<FB_Info_Edge>    FB_Edge_Vector;
typedef vector<FB_Info_Value>   FB_Value_Vector;
#ifdef KEY
typedef vector<FB_Info_Value_FP_Bin>   FB_Value_FP_Bin_Vector;
#endif
#endif

struct PU_Profile_Handle
{
    FB_Invoke_Vector	Invoke_Profile_Table;
    FB_Branch_Vector	Branch_Profile_Table;
    FB_Switch_Vector	Switch_Profile_Table;
    FB_Switch_Vector	Compgoto_Profile_Table;
    FB_Loop_Vector	Loop_Profile_Table;
    FB_Circuit_Vector	Short_Circuit_Profile_Table;
    FB_Call_Vector	Call_Profile_Table;
    FB_Icall_Vector	Icall_Profile_Table;
    FB_Edge_Vector      Edge_Profile_Table;
    FB_Value_Vector     Value_Profile_Table;
#ifdef KEY
    FB_Value_FP_Bin_Vector  Value_FP_Bin_Profile_Table;
#endif
    FB_Value_Vector     Stride_Profile_Table; 
  
    INT32 checksum;
    
    char *pu_name;
#ifdef KEY
    // feedback filename from which this profile has been obtained
    char *fb_name;
#endif
    INT32 pu_size;
    UINT64 runtime_fun_address;

#ifdef _BUILD_INSTR

    PU_Profile_Handle (char* pname = NULL, INT32 c_sum = 0) :
	pu_name (pname),
	checksum (c_sum) {

	if (pname) {
	    pu_name = new char[strlen(pname) + 1];
	    strcpy (pu_name, pname);
	}
    }
    
    ~PU_Profile_Handle () { delete [] pu_name; }

#else // _BUILD_INSTR
    PU_Profile_Handle (char *pname = NULL, INT32 c_sum = 0,
		       MEM_POOL* pool = MEM_pu_nz_pool_ptr) :
	pu_name (pname),
#ifdef KEY
	fb_name (NULL),
#endif
	checksum (c_sum),
	Invoke_Profile_Table (pool),
	Branch_Profile_Table (pool),
	Switch_Profile_Table (pool),
	Compgoto_Profile_Table (pool),
	Loop_Profile_Table (pool),
	Short_Circuit_Profile_Table (pool),
	Call_Profile_Table (pool) {

	if ( pname ) {
	    pu_name = (char *) MEM_POOL_Alloc (pool, strlen (pname) + 1);
	    strcpy(pu_name, pname);
	}
  }
  /*
    PU_Profile_Handle (char *pname = NULL, INT32 c_sum = 0,
		       MEM_POOL* pool = MEM_pu_nz_pool_ptr) :
	pu_name (pname),
	checksum (c_sum),
	Edge_Profile_Table (pool) {
	  if ( pname ) {
	    pu_name = (char *) MEM_POOL_Alloc (pool, strlen (pname) + 1);
	    strcpy(pu_name, pname);
	}
    }
	*/	       
  
    ~PU_Profile_Handle()
    {
#ifdef KEY
      free (fb_name);
#endif
    }

#endif // _BUILD_INSTR
   FB_Invoke_Vector& Get_Invoke_Table () {
	return Invoke_Profile_Table;
    }

    FB_Branch_Vector& Get_Branch_Table () {
	return Branch_Profile_Table;
    }

    FB_Switch_Vector& Get_Switch_Table () {
	return Switch_Profile_Table;
    }
    
    FB_Switch_Vector& Get_Compgoto_Table () {
	return Compgoto_Profile_Table;
    }

    FB_Loop_Vector& Get_Loop_Table () {
	return Loop_Profile_Table;
    }

    FB_Circuit_Vector& Get_Short_Circuit_Table () {
	return Short_Circuit_Profile_Table;
    }

    FB_Call_Vector& Get_Call_Table () {
	return Call_Profile_Table;
    }
  
    FB_Icall_Vector& Get_Icall_Table () {
	return Icall_Profile_Table;
    }

    FB_Edge_Vector& Get_Edge_Table() {
      return Edge_Profile_Table;
    }

    FB_Value_Vector& Get_Value_Table() {
       return Value_Profile_Table;
    }

#ifdef KEY
    FB_Value_FP_Bin_Vector& Get_Value_FP_Bin_Table() {
       return Value_FP_Bin_Profile_Table;
    }
#endif

    FB_Value_Vector& Get_Stride_Table() {
       return Stride_Profile_Table;
    }

};

typedef PU_Profile_Handle * PU_PROFILE_HANDLE;

extern void Get_File_Header(FILE *fp, char *fname, Fb_Hdr *fb_hdr);

extern void Get_Pu_Hdr_Table(FILE *fp, char *fname, Fb_Hdr& fb_hdr, 
			     Pu_Hdr *pu_hdr_table);

extern void Get_Str_Table(FILE *fp, char *fname, Fb_Hdr& fb_hdr, 
			  char *str_table);

extern void read_invoke_profile(  PU_PROFILE_HANDLE pu_handle, 
				  Pu_Hdr& pu_hdr_entry,
				  long pu_ofst, FILE *fp, char *fname);

extern void read_branch_profile(  PU_PROFILE_HANDLE pu_handle, 
				  Pu_Hdr& pu_hdr_entry,
				  long pu_ofst, FILE *fp, char *fname);

extern void read_switch_profile(  PU_PROFILE_HANDLE pu_handle, 
				  Pu_Hdr& pu_hdr_entry,
				  long pu_ofst, FILE *fp, char *fname);

extern void read_cgoto_profile(   PU_PROFILE_HANDLE pu_handle, 
				  Pu_Hdr& pu_hdr_entry,
				  long pu_ofst, FILE *fp, char *fname);

extern void read_loop_profile(    PU_PROFILE_HANDLE pu_handle, 
				  Pu_Hdr& pu_hdr_entry,
				  long pu_ofst, FILE *fp, char *fname);

extern void read_scircuit_profile(PU_PROFILE_HANDLE pu_handle, 
				  Pu_Hdr& pu_hdr_entry,
                                  long pu_ofst, FILE *fp, char *fname);

extern void read_call_profile(    PU_PROFILE_HANDLE pu_handle, 
				  Pu_Hdr& pu_hdr_entry,
				  long pu_ofst, FILE *fp, char *fname);

extern void read_icall_profile(    PU_PROFILE_HANDLE pu_handle, 
				  Pu_Hdr& pu_hdr_entry,
				  long pu_ofst, FILE *fp, char *fname);

extern void read_edge_profile(      PU_PROFILE_HANDLE pu_handle, 
          Pu_Hdr& pu_hdr_entry,
          long pu_ofst, FILE *fp, char *fname);

extern void read_value_profile(PU_PROFILE_HANDLE pu_handle, Pu_Hdr& pu_hdr_entry,
			       long pu_ofst, FILE *fp, char *fname);

#ifdef KEY
extern void read_value_fp_bin_profile(PU_PROFILE_HANDLE pu_handle, 
				      Pu_Hdr& pu_hdr_entry,
				      long pu_ofst, FILE *fp, char *fname);
#endif

extern void read_stride_profile(PU_PROFILE_HANDLE pu_handle, Pu_Hdr& pu_hdr_entry,
				long pu_ofst, FILE *fp, char *fname);
          
#ifndef _BUILD_INSTR

struct Fb_File_Info {
    char *name;
    FILE *fp;
    Fb_Hdr fb_hdr;
    Pu_Hdr *pu_hdr_table;
    char *str_table;

    Fb_File_Info() {}
    Fb_File_Info(char *nm, FILE *fptr, Fb_Hdr fhdr, Pu_Hdr *ptbl, char *stbl) :
		 name(nm), fp(fptr), fb_hdr(fhdr), pu_hdr_table(ptbl), 
		 str_table(stbl)
    {
#ifdef KEY
	if ( nm ) {
	    name = (char *) malloc (strlen (nm) + 1);
	    strcpy(name, nm);
	}
#endif
    }
    ~Fb_File_Info()
    {
#ifdef KEY
	free (name);
#endif
    }
};

extern BOOL Feedback_Enabled[PROFILE_PHASE_LAST];
typedef vector<Fb_File_Info*> Fb_File_Info_Vector;
typedef Fb_File_Info_Vector::iterator File_Info_Iterator;
extern Fb_File_Info_Vector Feedback_File_Info[PROFILE_PHASE_LAST];

// Prototypes for routines in instr_reader.cxx

extern void Process_Feedback_File(char *fb_name);

extern void Close_Feedback_Files();

typedef vector<PU_PROFILE_HANDLE, mempool_allocator<PU_PROFILE_HANDLE> >
	PU_PROFILE_HANDLES;
typedef PU_PROFILE_HANDLES::iterator PU_PROFILE_ITERATOR;

extern PU_PROFILE_HANDLES
Get_PU_Profile (char *pu_name, char *src_fname,
		Fb_File_Info_Vector& file_info_vector);

extern PU_PROFILE_HANDLE Get_PU_Profile(char *pu_name, char *src_fname,
					FILE *fp, char *fb_fname, 
					Fb_Hdr& fb_hdr, Pu_Hdr *pu_hdr_table, 
					char *str_table); 
					
// added by dxq
extern PU_PROFILE_HANDLES
Get_CG_PU_Profile (char* srcfile_pu_name,Fb_File_Info_Vector& file_info_vector);

extern PU_PROFILE_HANDLE Get_CG_PU_Profile(char* srcfile_pu_name,
					FILE *fp, char *fb_fname, 
					Fb_Hdr& fb_hdr, Pu_Hdr *pu_hdr_table, char* str_table); 
					
extern PU_PROFILE_HANDLES
Get_CG_PU_Value_Profile (char* srcfile_pu_name, Fb_File_Info_Vector& file_info_vector);
extern PU_PROFILE_HANDLE
Get_CG_PU_Value_Profile(char* srcfile_pu_name,  FILE* fp, char *fb_fname, Fb_Hdr& fb_hdr, Pu_Hdr *pu_hdr_table, char* str_table);

extern PROFILE_PHASE Get_Phase_Num(Fb_Hdr& fb_hdr);

extern char * Get_PU_Name(PU_PROFILE_HANDLE pu_handle);

extern INT32 Get_PU_Checksum(PU_PROFILE_HANDLE pu_handle);

extern size_t Get_Invoke_Table_Size(PU_PROFILE_HANDLE pu_handle);

extern FB_Info_Invoke& Get_Invoke_Profile(PU_PROFILE_HANDLE pu_handle,
					  INT32 id);

extern size_t Get_Branch_Table_Size(PU_PROFILE_HANDLE pu_handle);

extern FB_Info_Branch& Get_Branch_Profile(PU_PROFILE_HANDLE pu_handle,
					  INT32 id);

extern size_t Get_Switch_Table_Size(PU_PROFILE_HANDLE pu_handle);

extern FB_Info_Switch& Get_Switch_Profile(PU_PROFILE_HANDLE pu_handle,
					  INT32 id);

extern size_t Get_Compgoto_Table_Size(PU_PROFILE_HANDLE pu_handle);

extern FB_Info_Switch& Get_Compgoto_Profile(PU_PROFILE_HANDLE pu_handle,
					    INT32 id);

#ifdef KEY
extern size_t Get_Value_Table_Size(PU_PROFILE_HANDLE pu_handle);

extern FB_Info_Value& Get_Value_Profile(PU_PROFILE_HANDLE pu_handle,
					INT32 id);

extern size_t Get_Value_FP_Bin_Table_Size(PU_PROFILE_HANDLE pu_handle);

extern FB_Info_Value_FP_Bin& Get_Value_FP_Bin_Profile(PU_PROFILE_HANDLE pu_handle,
						      INT32 id);
#endif

extern size_t Get_Loop_Table_Size(PU_PROFILE_HANDLE pu_handle);

extern FB_Info_Loop& Get_Loop_Profile(PU_PROFILE_HANDLE pu_handle, INT32 id);

extern size_t Get_Short_Circuit_Table_Size(PU_PROFILE_HANDLE pu_handle);

extern FB_Info_Circuit& Get_Short_Circuit_Profile(PU_PROFILE_HANDLE pu_handle, 
						  INT32 id); 

extern size_t Get_Call_Table_Size(PU_PROFILE_HANDLE pu_handle);

extern FB_Info_Call& Get_Call_Profile(PU_PROFILE_HANDLE pu_handle, INT32 id);

extern FB_Info_Icall& Get_Icall_Profile(PU_PROFILE_HANDLE pu_handle, INT32 id);

extern size_t Get_Edge_Table_Size(PU_PROFILE_HANDLE pu_handle);

extern FB_Info_Edge& Get_Edge_Profile(PU_PROFILE_HANDLE pu_handle, INT32 id);
extern FB_Info_Value& Get_Value_Profile(PU_PROFILE_HANDLE pu_handle, INT32 id);
#ifdef KEY
extern FB_Info_Value_FP_Bin& Get_Value_FP_Bin_Profile(PU_PROFILE_HANDLE pu_handle, 
						      INT32 id);
#endif
extern FB_Info_Value& Get_Stride_Profile(PU_PROFILE_HANDLE pu_handle, INT32 id);


#endif // _BUILD_INSTR

#endif /* instr_reader_INCLUDED */
