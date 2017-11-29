/*
 * Copyright 2003, 2004 PathScale, Inc.  All Rights Reserved.
 */

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
#ifdef IN_GCC
# include "ansidecl.h"
# include "i386/biarch64.h"
# include "i386/i386.h"
# include "i386/att.h"
# include "dbxelf.h"
# include "elfos.h"
# include "svr4.h"
# include "linux.h"
# include "i386/x86-64.h"
# include "i386/linux64.h"
# include "defaults.h"
#endif
#ifndef POSIX
# define POSIX
#endif
