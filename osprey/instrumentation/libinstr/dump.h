//-*-c++-*-
// ====================================================================
// ====================================================================
//
// Module: dump.h
// $Revision: 1.1.1.1 $
// $Date: 2005/10/21 19:00:00 $
// $Author: marcel $
// $Source: /proj/osprey/CVS/open64/osprey1.0/instrumentation/libinstr/dump.h,v $
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

#ifndef dump_INCLUDED
#define dump_INCLUDED

extern void Dump_all(FILE *fp, char *output_filename);
extern void Dump_Fb_File_Header(FILE *fp, char *output_filename,
				Fb_Hdr& fb_hdr);

extern void
Dump_PU_Profile(FILE *fp, PU_PROFILE_HANDLE pu_handle, char * fname,
                vector<Pu_Hdr> & Pu_Hdr_Table, vector<char *> & Str_Table);

extern ULONG Dump_PU_Invoke_Profile(       FILE *fp, PU_PROFILE_HANDLE handle,
					   char *fname, INT32 *num);
extern ULONG Dump_PU_Branch_Profile(       FILE *fp, PU_PROFILE_HANDLE handle,
					   char *fname, INT32 *num);
extern ULONG Dump_PU_Switch_Profile(       FILE *fp, PU_PROFILE_HANDLE handle,
					   char *fname, INT32 *num);
extern ULONG Dump_PU_Compgoto_Profile(     FILE *fp, PU_PROFILE_HANDLE handle,
					   char *fname, INT32 *num);
extern ULONG Dump_PU_Loop_Profile(         FILE *fp, PU_PROFILE_HANDLE handle,
					   char *fname, INT32 *num);
extern ULONG Dump_PU_Short_Circuit_Profile(FILE *fp, PU_PROFILE_HANDLE handle,
					   char *fname, INT32 *num);
extern ULONG Dump_PU_Call_Profile(         FILE *fp, PU_PROFILE_HANDLE handle,
					   char *fname, INT32 *num);

extern void Dump_Fb_File_Pu_Table(FILE *fp, char *fname,
				  vector<Pu_Hdr>& Pu_Hdr_Table,
				  Fb_Hdr& fb_hdr);

extern void Dump_Fb_File_Str_Table(FILE *fp, char *fname,
				   vector<char *>& Str_Table, Fb_Hdr& fb_hdr);



#define FWRITE(_buffer, _size, _nitems, _fp, _error_message, _error_arg) \
	if (fwrite((void *)_buffer, _size, _nitems, _fp) != _nitems) \
	   profile_error(_error_message, _error_arg);

#define FSEEK(_fd, _position, whence, _error_message, _error_arg) \
     if (fseek(_fd, _position, whence) != 0) \
	   profile_error(_error_message, _error_arg);

#endif /* dump_INCLUDED */
