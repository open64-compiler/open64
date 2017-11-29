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


// 
// all system headers included directly from LNO
//

#include <alloca.h>
#include <assert.h>
#include <ctype.h>
#include <limits.h>
#include <math.h>
#include <stdarg.h>
#include <sys/elf_whirl.h>
#include <elf.h>
#include <sys/types.h>

//
// lnopt_main.h is included almost everywhere 
//

#include "lnopt_main.h"
//          wn.h" 
//            defs.h 
//            opcode.h 
//              opcode_core.h
//                opcode_gen_core.h
//                  mtypes.h
//              errors.h
//                errdesc.h
//              opcode_gen.h
//            mempool.h 
//            srcpos.h 
//            wn_core.h 
//              stab.h 
//                symtab.h
//                  <slist.h>
//                    <algobase.h> ...
//                    <alloc.h> ...
//                  segmented_array.h
//                  cmplr_segmented_array.h
//                  symtab_idx.h
//                  targ_const.h
//                  strtab.h
//                    mempool_allocator.h
//                  irbdata_defs.h
//                  symtab_[defs|verify|access|utils|compatible].h
//              irbdata.h
//            wn_map.h 
//            wio.h 
//              wutil.h 
//                wintrinsic.h 
//                  wintrinsic90.h 
//            wn_pragma.h
//          config_cache.h"
//          config_lno.h"
//          access_main.h
//            cxx_base.h
//            cxx_template.h
//            access_vector.h
//          tracing.h
//          if_info.h
//          dep_graph.h
//            cxx_graph.h
//            graph_template.h
//            dep.h
//              dvector.h
//            cxx_hash.h

// 
// other frequently included LNO headers
//

#include "ara.h"  
//          ara_region.h
//            soe.h
//              mat.h
//                frac.h
//              infin.h
//          ara_loop.h
//            lnoutils.h 
//              region_util.h
//              name.h
//            snl.h
//              reduc.h
//              snl_utils.h
//              snl_nest.h
//              snl_trans.h
//              snl_gen.h
//              snl_inv.h
//              snl_deps.h
//              snl_dist.h
//              snl_test.h
//            parallel.h
//              pu_info.h
//              sxlist.h
//              sdlist.h

#include "fiz_fuse.h" 
//          ff_utils.h
//            lwn_util.h
//              wn_util.h
//            btree.h
//            lno_bv.h

#include "lego_pragma.h" 
//          lego_util.h
//          opt_du.h
//          opt_alias_interface.h

#include "config.h" 
//          config_host.h
//          config_targ.h

