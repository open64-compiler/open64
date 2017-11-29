//-*-c++-*-

/*
 * Copyright 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
 */

// ====================================================================
// ====================================================================
//
// Module: dump.cxx
// $Revision: 1.1.1.1 $
// $Date: 2005/10/21 19:00:00 $
// $Author: marcel $
// $Source: /proj/osprey/CVS/open64/osprey1.0/instrumentation/libinstr/dump.cxx,v $
//
// ====================================================================
//
// Copyright (C) 2000, 2001 Silicon Graphics, Inc.  All Rights Reserved.
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of version 2 of the GNU General Public License as
// published by the Free Software Foundation.
//
// This program is distributed in the hope that it would be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//
// Further, this software is distributed without any warranty that it
// is free of the rightful claim of any third person regarding
// infringement  or the like.  Any license provided herein, whether
// implied or otherwise, applies only to this software file.  Patent
// licenses, if any, provided herein do not apply to combinations of
// this program with other software, or any other product whatsoever.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write the Free Software Foundation,
// Inc., 59 Temple Place - Suite 330, Boston MA 02111-1307, USA.
//
// Contact information:  Silicon Graphics, Inc., 1600 Amphitheatre Pky,
// Mountain View, CA 94043, or:
//
// http://www.sgi.com
//
// For further information regarding this notice, see:
//
// http://oss.sgi.com/projects/GenInfo/NoticeExplan
//
// ====================================================================
//
// Description:
//
// Write to a binary file all of the frequency counts obtained during
// this run of this program.
//
// ====================================================================
// ====================================================================


#include <stdio.h>
#include <algorithm>
#include <vector>

using std::vector;

#include "profile.h"
#include "profile_errors.h"

#define ERRORS_INCLUDED			// force errors.h not included by
					// fb_info.h
#define Is_True(x,y)			// Is_True not available at runtime
#define DevWarn(x)			// DevWarn not available at runtimex
#include "fb_info.h"

#include "dump.h"


// ====================================================================


static char* ERR_POS = "Error in positioning within %s";
static char* ERR_WRITE = "Error in writing to %s";


// round up "offset" as specified by "alignment"
static inline ULONG
align (ULONG offset, UINT32 alignment)
{
    --alignment;
    return ((offset + alignment) & ~alignment);
}


static inline void
realign_file (FILE* fp, ULONG& offset, UINT32 alignment, char* fname)
{
    if (offset % alignment == 0)
	return;

    ULONG new_offset = align (offset, alignment);
    FSEEK (fp, new_offset - offset, SEEK_CUR, ERR_POS, fname);
    offset = new_offset;
}


// Just before finish, dump all the profile information

void 
Dump_all(FILE *fp, char *output_filename)
{
  HASH_MAP::iterator i;

  vector<Pu_Hdr> Pu_Hdr_Table;
  vector<char *> Str_Table;

  Fb_Hdr fb_hdr;

  strcpy ((char *) fb_hdr.fb_ident, INSTR_MAG);
  fb_hdr.fb_version = INSTR_CURRENT;
  fb_hdr.fb_pu_hdr_ent_size = sizeof(Pu_Hdr);
  fb_hdr.phase_num = Instrumentation_Phase_Num();
  fb_hdr.fb_profile_offset = align (sizeof(Fb_Hdr), sizeof(mUINT64));
  // Leave space for Fb_Hdr

  FSEEK(fp, fb_hdr.fb_profile_offset, SEEK_SET, ERR_POS, output_filename);

  // Dump profile info for all PU

  for( i = PU_Profile_Handle_Table.begin();
       i != PU_Profile_Handle_Table.end(); i++) {
    PU_PROFILE_HANDLE pu_handle = (*i).second;
    Dump_PU_Profile(fp, pu_handle, output_filename, Pu_Hdr_Table, Str_Table);
  }

  // Now attach the PU header table

  Dump_Fb_File_Pu_Table(fp, output_filename, Pu_Hdr_Table, fb_hdr);

  // Attach the string table 

  Dump_Fb_File_Str_Table(fp, output_filename, Str_Table, fb_hdr);

  // Go put the Fb_Hdr at the top of the feedback file

  Dump_Fb_File_Header(fp, output_filename,fb_hdr);
}


void
Dump_Fb_File_Header(FILE *fp, char *output_filename, Fb_Hdr& fb_hdr) 
{
  // Rewind the file
  FSEEK(fp,0,SEEK_SET, ERR_POS, output_filename);
  
  // Write the feedback header
  FWRITE(&fb_hdr, sizeof(Fb_Hdr), 1, fp, ERR_WRITE, output_filename);

  // Reset the file position to EOF
  FSEEK(fp,0,SEEK_END, ERR_POS, output_filename);
}


namespace {

    // local templates used only by Dump_PU_Profile

    static void
    Convert_Invoke_Profile (vector<FB_Info_Invoke>& dest,
			    const Invoke_Profile_Vector& src) {
	dest.reserve (src.size ());
	
	for (Invoke_Profile_Vector::const_iterator first (src.begin ());
	     first != src.end (); ++first) {
	    
	    dest.push_back (FB_Info_Invoke (FB_FREQ (first->invoke_count)));
	}
    }


    static void
    Convert_Branch_Profile (vector<FB_Info_Branch>& dest,
			    const Branch_Profile_Vector& src) {
	dest.reserve (src.size ());
	
	for (Branch_Profile_Vector::const_iterator first (src.begin ());
	     first != src.end (); ++first) {

	    dest.push_back (FB_Info_Branch (FB_FREQ (first->taken),
					    FB_FREQ (first->not_taken)));
	}
    }

#ifdef KEY
    static void Convert_Value_Profile( vector<FB_Info_Value>& dest,
				       const Value_Profile_Vector& src )
    {
      dest.reserve (src.size ());
	
      for( Value_Profile_Vector::const_iterator first(src.begin ());
	   first != src.end (); ++first ){
	FB_Info_Value info( first->num_values,
			    first->exe_counter,
			    first->value,
			    first->freq );
	dest.push_back( info );
      }
    }

    static void Convert_Value_FP_Bin_Profile( 
			   vector<FB_Info_Value_FP_Bin>& dest,
			   const Value_FP_Bin_Profile_Vector& src )
    {
      dest.reserve (src.size ());
	
      for( Value_FP_Bin_Profile_Vector::const_iterator first(src.begin ());
	   first != src.end (); ++first ){
	FB_Info_Value_FP_Bin info( first->exe_counter,
				   first->zopnd0,
				   first->zopnd1,
				   first->uopnd0,
				   first->uopnd1 );
	dest.push_back( info );
      }
    }
#endif

    template <class T>
    static void
    Convert_Switch_Profile (FB_Info_Switch& dest, T& src) {

	dest.freq_targets.reserve (src.size ());

	for (typename T::value_type::const_iterator
		 first (src.Get_Targets_Profile ().begin ());
	     first != src.Get_Targets_Profile ().end (); ++first) {

	    dest.freq_targets.push_back (FB_FREQ (*first));
	}
    }


    static void
    Convert_Loop_Profile (vector<FB_Info_Loop>& dest,
			  const Loop_Profile_Vector& src) {
	dest.reserve (src.size ());

	for (Loop_Profile_Vector::const_iterator first (src.begin ());
	     first != src.end (); ++first) {

	  // Did the very last pass through the loop have zero trip count?
	  INT64 zero_trips = first->num_zero_trips;
	  if ( first->invocation_count > 0 && first->last_trip_count == 0 ) {
	    zero_trips++;
	  }

	  const INT64 positive = first->invocation_count - zero_trips;
	  const INT64 back = first->total_trip_count - first->invocation_count + zero_trips;
	dest.push_back(FB_Info_Loop(FB_FREQ (zero_trips),
				    FB_FREQ (positive),
				    FB_FREQ (FB_FREQ_TYPE_UNKNOWN),
				    FB_FREQ (back)));
      }
    }

    static void
    Convert_Short_Circuit_Profile (vector<FB_Info_Circuit>& dest,
				   const Short_Circuit_Profile_Vector& src) {
	dest.reserve (src.size ());

	for (Short_Circuit_Profile_Vector::const_iterator first (src.begin ());
	     first != src.end (); ++first) {

	    dest.push_back
		(FB_Info_Circuit (FB_FREQ (FB_FREQ_TYPE_UNKNOWN),
				  FB_FREQ (first->right_taken_count),
				  FB_FREQ (first->neither_taken_count)));
	}
    }

    static void
    Convert_Call_Profile (vector<FB_Info_Call>& dest,
			  const Call_Profile_Vector& src) {
	dest.reserve (src.size ());

	for (Call_Profile_Vector::const_iterator first (src.begin ());
	     first != src.end (); ++first) {

	    dest.push_back
		(FB_Info_Call (FB_FREQ (first->entry_count),
			       FB_FREQ (first->exit_count),
			       first->entry_count == first->exit_count));
	}
    }


    struct POSITION {
	ULONG offset;
	INT32 num_entries;

	POSITION () {}
	POSITION (ULONG ofst, INT32 num) : offset (ofst), num_entries (num) {}
    };

    template <class T>
    POSITION
    Dump_PU_Profile (FILE *fp, ULONG& offset, const T& profile, char *fname) {

	realign_file (fp, offset, sizeof(mINT64), fname);

	POSITION pos (offset, profile.size ());

	FWRITE (&(profile.front ()), sizeof(typename T::value_type),
		profile.size (), fp, ERR_WRITE, fname);

	offset += profile.size () * sizeof(typename T::value_type);
	return pos;
    }


    template <class T>
    POSITION
    Dump_PU_Switch_Profile (FILE *fp, ULONG& offset, T& profile,
			    ULONG& target_offset, char *fname) {

	realign_file (fp, offset, sizeof(mINT64), fname);

	POSITION pos (offset, profile.size ());

	typename T::iterator first (profile.begin ());

	while (first != profile.end ()) {

	    FB_Info_Switch fb_info;
	    Convert_Switch_Profile (fb_info, *first);

	    FWRITE (&(fb_info.freq_targets.front ()), sizeof(FB_FREQ),
		    fb_info.freq_targets.size (), fp, ERR_WRITE, fname);

	    offset += sizeof(INT64) * fb_info.freq_targets.size ();

	    ++first;
	}

	target_offset = offset;

	first = profile.begin ();
	while (first != profile.end ()) {
	    INT32 num_targs = first->Get_Targets_Profile ().size ();
	    FWRITE (&num_targs, sizeof(INT32), 1, fp, ERR_WRITE, fname);
	    ++first;
	}

	offset += profile.size () * sizeof(INT32);
	return pos;
    }

}  // end namespace


// Given a PU handle, dump all the profile information that
// has been gathered for that PU.

void
Dump_PU_Profile(FILE *fp, PU_PROFILE_HANDLE pu_handle, char * fname,
		vector<Pu_Hdr> & Pu_Hdr_Table, vector<char *> & Str_Table)
{
  static ULONG Str_Offset;
  static ULONG PU_Offset;

  // Update the string Table
  Str_Table.push_back(pu_handle->pu_name);

  // double word align the PU_Offset
  realign_file (fp, PU_Offset, sizeof(mINT64), fname);
  
  Pu_Hdr pu_hdr;

  pu_hdr.pu_checksum = pu_handle->checksum;
  pu_hdr.pu_size = pu_handle->pu_size;
  pu_hdr.runtime_fun_address = pu_handle->runtime_fun_address;
  pu_hdr.pu_name_index = Str_Offset;
  pu_hdr.pu_file_offset = PU_Offset;

  ULONG offset = 0;

  POSITION pos;

  {
      vector<FB_Info_Invoke> fb_info;
      Convert_Invoke_Profile (fb_info, pu_handle->Get_Invoke_Table ());
      pos = Dump_PU_Profile (fp, offset, fb_info, fname);
      pu_hdr.pu_inv_offset = pos.offset;
      pu_hdr.pu_num_inv_entries = pos.num_entries;
  }

  {
      vector<FB_Info_Branch> fb_info;
      Convert_Branch_Profile (fb_info, pu_handle->Get_Branch_Table ());
      pos = Dump_PU_Profile (fp, offset, fb_info, fname);
      pu_hdr.pu_br_offset = pos.offset;
      pu_hdr.pu_num_br_entries = pos.num_entries;
  }

  pos = Dump_PU_Switch_Profile (fp, offset, pu_handle->Get_Switch_Table (),
				pu_hdr.pu_switch_target_offset, fname);
  pu_hdr.pu_switch_offset = pos.offset;
  pu_hdr.pu_num_switch_entries = pos.num_entries;

  pos = Dump_PU_Switch_Profile (fp, offset, pu_handle->Get_Compgoto_Table (),
				pu_hdr.pu_cgoto_target_offset, fname);
  pu_hdr.pu_cgoto_offset = pos.offset;
  pu_hdr.pu_num_cgoto_entries = pos.num_entries;

  {
      vector<FB_Info_Loop> fb_info;
      Convert_Loop_Profile (fb_info, pu_handle->Get_Loop_Table ());
      pos = Dump_PU_Profile (fp, offset, fb_info, fname);
      pu_hdr.pu_loop_offset = pos.offset;
      pu_hdr.pu_num_loop_entries = pos.num_entries;
  }

  {
      vector<FB_Info_Circuit> fb_info;
      Convert_Short_Circuit_Profile (fb_info,
				     pu_handle->Get_Short_Circuit_Table ());
      pos = Dump_PU_Profile (fp, offset, fb_info, fname); 
      pu_hdr.pu_scircuit_offset = pos.offset;
      pu_hdr.pu_num_scircuit_entries = pos.num_entries;
  }

  {
      vector<FB_Info_Call> fb_info;
      Convert_Call_Profile (fb_info, pu_handle->Get_Call_Table ());
      pos = Dump_PU_Profile (fp, offset, fb_info, fname);
      pu_hdr.pu_call_offset = pos.offset;
      pu_hdr.pu_num_call_entries = pos.num_entries;
  }

#ifdef KEY
  {
    vector<FB_Info_Value> fb_info;
    Convert_Value_Profile( fb_info, pu_handle->Get_Value_Table() );
    pos = Dump_PU_Profile( fp, offset, fb_info, fname );
    pu_hdr.pu_value_offset = pos.offset;
    pu_hdr.pu_num_value_entries = pos.num_entries;
  }

  {
    vector<FB_Info_Value_FP_Bin> fb_info;
    Convert_Value_FP_Bin_Profile( fb_info, 
				  pu_handle->Get_Value_FP_Bin_Table() );
    pos = Dump_PU_Profile( fp, offset, fb_info, fname );
    pu_hdr.pu_value_fp_bin_offset = pos.offset;
    pu_hdr.pu_num_value_fp_bin_entries = pos.num_entries;
  }
#endif

  {
      pos = Dump_PU_Profile (fp, offset, pu_handle->Get_Icall_Table (), fname);
      pu_hdr.pu_icall_offset = pos.offset;
      pu_hdr.pu_num_icall_entries = pos.num_entries;
  }

  // Enter the PU header
  Pu_Hdr_Table.push_back(pu_hdr);

  Str_Offset += strlen(pu_handle->pu_name) + 1;
  PU_Offset += (offset - pu_hdr.pu_inv_offset);
}


// Write out the PU header table

void 
Dump_Fb_File_Pu_Table(FILE *fp, char *fname, vector<Pu_Hdr>& Pu_Hdr_Table,
		      Fb_Hdr& fb_hdr)
{
  ULONG offset = ftell(fp);
  realign_file (fp, offset, 
#if defined(__GNUC__)
  __alignof__(Pu_Hdr),
#else
  __builtin_alignof(Pu_Hdr), 
#endif
  fname);

  fb_hdr.fb_pu_hdr_offset = offset;
  fb_hdr.fb_pu_hdr_num = Pu_Hdr_Table.size();
 
  for (size_t i = 0; i < Pu_Hdr_Table.size(); i++) {
    Pu_Hdr & pu_hdr_entry = Pu_Hdr_Table[i];
    FWRITE(&pu_hdr_entry, sizeof(Pu_Hdr), 1, fp, ERR_WRITE, fname);
  }
}


// Write out the string table

void 
Dump_Fb_File_Str_Table(FILE *fp, char *fname, vector<char *>& Str_Table,
		       Fb_Hdr& fb_hdr)
{
  ULONG table_size = 0;

  fb_hdr.fb_str_table_offset = ftell(fp);

  for (size_t i = 0; i < Str_Table.size(); i++) {
    char *pu_name = Str_Table[i];
    FWRITE(pu_name, strlen(pu_name) + 1, 1, fp, ERR_WRITE, fname);

    table_size += strlen(pu_name) + 1;
  }

  fb_hdr.fb_str_table_size = table_size;
}
