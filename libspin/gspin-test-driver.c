/*
  Copyright (C) 2006. QLogic Corporation. All Rights Reserved.

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
 */

#include <stdio.h>
#include <stdlib.h>

#include "gspin-tree.h"
#include "gspin-io.h"

extern gs_void_t gs_dump_file (gs_t t, FILE *f) ;

int
main (int argc, char *argv[])
{
  gs_t p;
  char *file_name, *outfile_name;
  FILE *f;

  if (argc != 2) {
    printf("usage: gspin <spin_file>\n");
    printf("       \"gspin foo.spin\" will dump to \"foo.spin.txt\".\n");
    exit(1);
  }

  file_name = argv[1];
  outfile_name = (char *) malloc(strlen(file_name) + 10);
  sprintf(outfile_name, "%s.txt", file_name);

  f = fopen(outfile_name, "w");
  if (f == NULL) {
    printf("gspin: cannot write to %s\n", outfile_name);
  }

  p = gs_read_file (argv[1]);
  gs_dump_file (p, f);
  // printf ("struct gspin size: %d (bytes)\n", sizeof (struct gspin));

  fclose(f);

  return 0;
}
