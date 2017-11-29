/*
 * Copyright (C) 2006. QLogic Corporation. All Rights Reserved.
 */

////////////////////////////////////////////////////////////////////////////////
//
// Copyright 2006 PathScale, Inc. All Rights Reserved.
//
// This program is free software; you can redistribute it and/or modify it
// under the terms of version 2 of the GNU General Public License as
// published by the Free Software Foundation.
//
// This program is distributed in the hope that it would be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//
// Further, this software is distributed without any warranty that it is
// free of the rightful claim of any third person regarding infringement
// or the like.  Any license provided herein, whether implied or
// otherwise, applies only to this software file.  Patent licenses, if
// any, provided herein do not apply to combinations of this program with
// other software, or any other product whatsoever.
//
// You should have received a copy of the GNU General Public License along
// with this program; if not, write the Free Software Foundation, Inc., 59
// Temple Place - Suite 330, Boston MA 02111-1307, USA.
////////////////////////////////////////////////////////////////////////////////

#include <stdio.h>
#include <stdlib.h>

#include "gspin-base-types.h"
#include "gspin-assert.h"

static void
gs_assert_failure (gs_string_t file, gs_string_t function, gs_count_t line,
                   const gs_string_t message)
{
  fprintf(stderr, "Assertion failure in file: %s, function: %s, line: %d.\n",
	  file, function, line);
  fprintf (stderr, "%s\n", message);
}

void gs_assert(bool condition, 
                     gs_string_t file, 
                     gs_string_t function, 
                     gs_count_t line, 
                     const gs_string_t message)
{
  if (condition == gs_true)
    return;
  gs_assert_failure(file, function, line, message);
}
