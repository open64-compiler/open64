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


typedef long ADDR;

#define C_FIDENT 8
#define COUNTSMAG0    'p'
#define COUNTSMAG1    'M'
#define COUNTSMAG2    'o'
#define COUNTSMAG3    '0'
#define COUNTSMAG4    '~'
#define COUNTSMAG5    '?'
#define COUNTSMAG6    '>'
#define COUNTSMAG7    '&'

#define C_VERSION     1

typedef struct {
  unsigned char c_ident[C_FIDENT];
  int           c_entry;
  short         c_version;
  short         c_dummy1;
} Counts_hdr;

typedef struct {
  ADDR    caller;
  ADDR    callee;
  int     caller_name_idx;
  int     callee_name_idx;
  int     count;
} counts_entry;

struct counts_desc {
  ADDR    caller;
  ADDR    callee;
  struct counts_desc *next;
  int     count;
};



typedef enum {
  ER_FATAL,
  ER_WARNING,
  ER_INFO,
  ER_ERROR,
  ER_VERBOSE,
  ER_MSG,
} error_number;

typedef enum {
  CMP_LESS,
  CMP_SAME,
  CMP_MORE,
} cmp_status;
  

extern void ir_prof_error(int, char *, char *);
