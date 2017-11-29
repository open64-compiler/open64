/*
 * Copyright 2004 PathScale, Inc.  All Rights Reserved.
 */

/*

  Copyright (C) 2000, 2001 Silicon Graphics, Inc.  All Rights Reserved.

  This program is free software; you can redistribute it and/or modify it
  under the terms of version 2.1 of the GNU Lesser General Public License 
  as published by the Free Software Foundation.

  This program is distributed in the hope that it would be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  

  Further, this software is distributed without any warranty that it is
  free of the rightful claim of any third person regarding infringement 
  or the like.  Any license provided herein, whether implied or 
  otherwise, applies only to this software file.  Patent licenses, if
  any, provided herein do not apply to combinations of this program with 
  other software, or any other product whatsoever.  

  You should have received a copy of the GNU Lesser General Public 
  License along with this program; if not, write the Free Software 
  Foundation, Inc., 59 Temple Place - Suite 330, Boston MA 02111-1307, 
  USA.

  Contact information:  Silicon Graphics, Inc., 1600 Amphitheatre Pky,
  Mountain View, CA 94043, or:

  http://www.sgi.com

  For further information regarding this notice, see:

  http://oss.sgi.com/projects/GenInfo/NoticeExplan

*/


#pragma ident "@(#) libu/ffio/parse_data.c	92.7	11/16/99 15:10:31"
#include <stdlib.h>
#include <string.h>
#include <ffio.h>
#include <cray/fndc.h>
#include "layer_def.h"

#include "fd_parse.h"
#include "f77_parse.h"
#include "null_parse.h"
#include "sysio.h"
#include "sys_parse.h"
#include "syscall_parse.h"
#include "user_parse.h"
#include "text_parse.h"
#include "cca_parse.h"
#include "cos_parse.h"
#include "cch_parse.h"
#include "bufa_parse.h"
#include "global_parse.h"
#include "event_parse.h"
#include "tmf_parse.h"
#include "ibm_parse.h"
#include "vms_parse.h"

#ifdef	_CRAY
#include "blx_parse.h"
#include "bmx_parse.h"
#include "c205_parse.h"
#include "cdc_parse.h"
#include "cmp_parse.h"
#include "er90_parse.h"
#include "error_parse.h"
#include "mr_parse.h"
#include "nosve_parse.h"
#include "sds_parse.h"
#include "trace_parse.h"
#endif

struct LAYER_DATA *_ffio_parse_tables[] = {
   0, /* end has no parse table */
   &_USR0_parse,
   &_USR1_parse,
   &_USR2_parse,
   &_USR3_parse,
   &_USR4_parse,
   &_USR5_parse,
   &_USR6_parse,
   &_USR7_parse,
   &_USR8_parse,
   &_USR9_parse,
   &_syscall_data,
   &_system_data,
   &_f77_data,
   &_null_data,
   &_user_parse,
   &_site_data,
   &_text_data,
   &_cca_data ,
   &_cos_data,
   &_cch_data ,
   &_bufa_data,
   &_fd_data,
#if	defined(_CRAY) || defined(__mips)
   &_global_data,
#endif
#ifdef	_CRAY
   &_blocked_data,
   &_tape_data,
   &_cdc_data,
   &_sds_data,
   &_mr_data,
   &_trace_data,
   &_error_data,

   &_blx_data,
   &_blankx_data,

   &_er90b_data ,
   &_cmp_data ,
#endif /* _CRAY */
#if	defined(_CRAY) || defined(__mips)
   &_ibm_data,
   &_vms_data,
   &_event_data,
	/* User cannot specify lock layer */
#endif /* _CRAY or __mips */
#ifdef	_CRAY
   &_bmx_data,

   &_nosve_data,
   &_c205_data,

   &_stdin_data,
   &_stdout_data,
   &_stderr_data,
#endif
#ifdef	__mips
   &_tmf_data,
#endif
   &_usr0_parse ,
   &_usr1_parse ,
   &_usr2_parse ,
   &_usr3_parse ,
   &_usr4_parse ,
   &_usr5_parse ,
   &_usr6_parse ,
   &_usr7_parse ,
   &_usr8_parse ,
   &_usr9_parse ,
   } ;

int _num_layer_tables= (sizeof(_ffio_parse_tables)/sizeof(struct LAYER_DATA *));

struct CVCHRT_DATA _cvch_parse_tables[] =  {
    { CS_ASCII,  "ascii"  },
    { CS_EBCDIC, "ebcdic" },
    { CS_CDC,    "cdc"    },
};

int _num_cvch = (sizeof(_cvch_parse_tables)/sizeof(struct CVCHRT_DATA ));

struct CVCHRT_DATA _cvrt_parse_tables[] = {
    { NCV_NONE,  "none"    },
    { NCV_USER,  "user"    },
    { NCV_SITE,  "site"    },
    { NCV_CRAY,  "cray"    },
    { NCV_IEG,   "ieee"    },
    { NCV_IEG,   "ieee_32" },
#ifndef	__mips
#ifndef	_CRAYMPP
    { NCV_IBM,   "ibm"     },
    { NCV_IBD,   "ibm_dp"  },
    { NCV_IEL,   "ieee_64" },
    { NCV_IED,   "ieee_dp" },
#endif
#if	!defined(_CRAYMPP) && !defined(_CRAYIEEE)
    { NCV_CDC,   "cdc"     },
    { NCV_VMS,   "vax"     },
    { NCV_VAD,   "vax_dp"  },
    { NCV_VMS,   "vms"     },
    { NCV_VAD,   "vms_dp"  },
    { NCV_NVE,   "nosve"   },
    { NCV_MIPS,  "mips"    },
    { NCV_MIPS,  "bigendian" },
    { NCV_MIPS,  "be"      },
    { NCV_205,   "c205"    },
    { NCV_205,   "eta"     },
    { NCV_IEU,   "ultrix"  },
    { NCV_IEU,   "ieee_le" },
    { NCV_IUD,   "ultrix_dp" },
    { NCV_IUD,   "ieee_le_dp" },
#endif
#if	(defined(_CRAY1) && !defined(_CRAYIEEE)) || defined(_CRAYMPP)
    { NCV_T3D,   "t3d"     },
    { NCV_T3D,   "t3e"     },
#endif
#ifdef  KEY
    { NCV_IA,    "ia32"    },
    { NCV_IA,    "ia64"    },
    { NCV_MIPS,  "bigendian" },
    { NCV_MIPS,  "be"      },
#endif /* KEY */
#else	/* !__mips */
    { NCV_IEL,   "ieee_64" },
    { NCV_IEU,   "ieee_le" },
    { NCV_MIPS,  "mips"    },
    { NCV_MIPS,  "bigendian" },
    { NCV_MIPS,  "be"      },
    { NCV_VMS,   "vax"     },
    { NCV_VMS,   "vms"     },
    { NCV_IBM,   "ibm"     },
    { NCV_IA,    "ia32"    },
    { NCV_IA,    "ia64"    },
#endif	/* !__mips */
};

int _num_cvrt = (sizeof(_cvrt_parse_tables)/sizeof(struct CVCHRT_DATA ));

/* */
