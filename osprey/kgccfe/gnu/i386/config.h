/*
 * Copyright 2003, 2004 PathScale, Inc.  All Rights Reserved.
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of version 2 of the GNU General Public License as
 * published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it would be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  
 *
 * Further, this software is distributed without any warranty that it is
 * free of the rightful claim of any third person regarding infringement 
 * or the like.  Any license provided herein, whether implied or 
 * otherwise, applies only to this software file.  Patent licenses, if 
 * any, provided herein do not apply to combinations of this program with 
 * other software, or any other product whatsoever.  
 *
 * You should have received a copy of the GNU General Public License along
 * with this program; if not, write the Free Software Foundation, Inc., 59
 * Temple Place - Suite 330, Boston MA 02111-1307, USA.
 */

#define TARGET_CPU_DEFAULT (TARGET_CPU_DEFAULT_athlon_sse)
#include "auto-host.h"
#ifdef IN_GCC
/* Provide three core typedefs used by everything, if we are compiling
   GCC.  These used to be found in rtl.h and tree.h, but this is no
   longer practical.  Providing these here rather that system.h allows
   the typedefs to be used everywhere within GCC. */
struct rtx_def;
typedef struct rtx_def *rtx;
struct rtvec_def;
typedef struct rtvec_def *rtvec;
union tree_node;
typedef union tree_node *tree;
#endif
#define GTY(x)
#ifdef IN_GCC
# include "ansidecl.h"
# include "i386/biarch64.h"
# include "i386/i386.h"
# include "i386/unix.h"
#if defined(BUILD_OS_DARWIN)
# include "svr4.h"
# include "darwin.h"
# include "i386/x86-64.h"
#else /* defined(BUILD_OS_DARWIN) */
# include "i386/att.h"
# include "dbxelf.h"
# include "elfos.h"
# include "svr4.h"
# include "linux.h"
# include "i386/x86-64.h"
# include "i386/linux64.h"
#endif /* defined(BUILD_OS_DARWIN) */
# include "defaults.h"
#endif
#ifndef POSIX
# define POSIX
#endif
#ifndef GENERATOR_FILE
# include "insn-constants.h"
# include "insn-flags.h"
#endif
