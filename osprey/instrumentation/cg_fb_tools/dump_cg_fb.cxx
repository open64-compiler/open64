/*
  Copyright (c) 2001, Institute of Computing Technology, Chinese Academy of Sciences
  All rights reserved.

  Redistribution and use in source and binary forms, with or without modification,
  are permitted provided that the following conditions are met:

  Redistributions of source code must retain the above copyright notice, this list
  of conditions and the following disclaimer.

  Redistributions in binary form must reproduce the above copyright notice, this list
  of conditions and the following disclaimer in the documentation and/or other materials
  provided with the distribution.

  Neither the name of the owner nor the names of its contributors may be used to endorse or
  promote products derived from this software without specific prior written permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR
  IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
  FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE CONTRIBUTORS BE LIABLE FOR
  ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
  NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
  BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

//-*-c++-*-

#include <unistd.h>
#include <sys/types.h>  
#include <sys/stat.h>  
#include <fcntl.h>
#include <sys/mman.h>         // for  mmap() 
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <time.h>
#include <vector>
#include <ext/hash_map>
using namespace std;

#include "cg_instru_lib.h"

static Fb_Hdr file_header; 
static void  _dump_File_Header( FILE * fp )
{
  if (fread( &file_header, sizeof(Fb_Hdr), 1, fp) != 1)
  {
  	fprintf(stderr, "Error when reading file header\n");
  	exit(-1);
  }
  file_header.Print(stdout);
}

static Pu_Hdr * pu_headers = NULL;
static void  _dump_Pu_Headers( FILE * fp  )
{
  pu_headers = (Pu_Hdr *)malloc(sizeof(Pu_Hdr)*file_header.fb_pu_hdr_num);
  if (!pu_headers)
  {
  	fprintf(stderr,"Failed to malloc mem for pu_headers1\n");
  	exit(-1);
  }
  if (fread(pu_headers, sizeof(Pu_Hdr), file_header.fb_pu_hdr_num, fp) != file_header.fb_pu_hdr_num)
  {
  	fprintf(stderr, "Error reading pu_headers\n");
  	exit(-1);
  }

  for (int i=0; i<file_header.fb_pu_hdr_num; i++)
  {
  	pu_headers[i].Print(stdout, i);
  }
}

char * str_table = NULL;
static void  _dump_Str_Header( FILE * fp )
{
  str_table = (char *) malloc (file_header.fb_str_table_size + 2);
  if (!str_table)
  {
  	fprintf(stderr, "failed to malloc mem for str_table\n");
  	exit(-1);
  }
  if (fread(str_table, sizeof(char), file_header.fb_str_table_size, fp) != file_header.fb_str_table_size)
  {
  	fprintf(stderr, "Error while reading str table!\n");
  	exit(-1);
  }
  str_table[file_header.fb_str_table_size] = 0;
  fprintf(stdout,"\n************   Str table    **************\n");
  for (int i=0; i<file_header.fb_pu_hdr_num; i++)
  {
  	fprintf(stdout, "No %d : %s \n", i, str_table+pu_headers[i].pu_name_index);
  }
}

static void  _dump_pu_data( FILE * fp )
{
  for (int i=0; i<file_header.fb_pu_hdr_num; i++)
  {
  	fprintf(stdout, "\n*********** PU Data No %d ************\n", i );
  	fprintf(stdout, "*********** edge profile info: ************\n");
	FB_FREQ fb_freq;
  	for (int j=0;j<pu_headers[i].pu_num_edge_entries; j++)
  	{
  	  if (fread(&fb_freq, sizeof(FB_FREQ), 1, fp) != 1)
  	  {
  	    fprintf(stderr, "Error while reading fb_freq\n");
  	    exit(-1);
  	  }
  	  fprintf(stdout, "_type = %d   |  _value = %f \n", fb_freq._type, fb_freq._value);
  	}
  	fprintf(stdout, "*********** value profile info: ************\n");
	FB_TNV fb_tnv;
	fseek(fp,pu_headers[i].pu_value_offset,SEEK_SET);
  	for (int j=0; j<pu_headers[i].pu_instr_count ; j++)
  	{
  	  if (fread(&fb_tnv, sizeof(FB_TNV), 1, fp) != 1)
  	  {
  	    fprintf(stderr, "Error while reading fb_tnv\n");
  	    exit(-1);
  	  }
  	  
  	  fb_tnv.Print(stdout);
  	}
   long pu_ofst=file_header.fb_profile_offset + pu_headers[i].pu_file_offset;
  fprintf(stdout, "*********** stride profile info: ************\n");
  fseek(fp,pu_ofst+pu_headers[i].pu_stride_offset,SEEK_SET);
  	for (int j=0; j<pu_headers[i].pu_ld_count ; j++)
  	{
  	  if (fread(&fb_tnv, sizeof(FB_TNV), 1, fp) != 1)
  	  {
  	    fprintf(stderr, "Error while reading fb_tnv\n");
  	    exit(-1);
  	  }
  	 
  	  fb_tnv.Print(stdout);
  	}
  }
}

int main(int argc, char * argv[])
{
	if ( argc < 2)
	{
		fprintf(stderr, "USAGE: %s fb_file_name\n", argv[0]);
		exit(-1);
	}
	FILE * fpin;
	if (!(fpin = fopen(argv[1], "rb")) ) 
	{
	  fprintf(stderr, "Error to open file %s\n", argv[1]);
	  exit(-1);
	}
	fprintf(stdout, "Start to dump data from feedback file: %s\n", argv[1]);
	_dump_File_Header(fpin);
	_dump_Pu_Headers(fpin);
	_dump_Str_Header(fpin);
	_dump_pu_data(fpin);
	fprintf(stdout,"************   End of dump   **************\n\n");

	fclose(fpin);
}

	
