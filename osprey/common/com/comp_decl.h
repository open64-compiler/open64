/*

  Copyright (C) 2010, Hewlett-Packard Development Company, L.P. All Rights Reserved.

  Open64 is free software; you can redistribute it and/or
  modify it under the terms of the GNU General Public License
  as published by the Free Software Foundation; either version 2
  of the License, or (at your option) any later version.

  Open64 is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
  MA  02110-1301, USA.

*/

/* ====================================================================
 *
 * Module: comp_decl.h
 *
 * Revision history:
 *  Oct-10 - Original Version
 *
 * Description:
 *  Define the base O64_Component class to support phase componentization.
 *
 * Exported classes:
 *	O64_Component
 *
 * SEE ALSO:
 *  be/com/comp_driver.h (OptionDescriptor, O64_Driver)
 *
 * ====================================================================
 */

#ifndef comp_decl_INCLUDED 
#define comp_decl_INCLUDED

#include "defs.h"
#include "flags.h"
#include "comp_driver.h"
#include "mempool.h"
#include "wn.h"

class O64_Component
{
protected:

    O64_COMPONENT       _CurrentComponent;
    O64_Driver*         _Driver;
    MEM_POOL*           _LocalMemPool; /* local memory pool */
    O64_Option*         _CurrentOption; /* option handling */

    bool                _enable;
    bool                _disable;
    bool                _canBeDisabled;
    bool                _doStats;
    bool                _doTrace;   /* whether to print trace information */
    TRACE_OPTION_KIND   _TraceKind;

    bool                _invalidCFG; /* CFG needs to be invalidated after this opt */ 
    bool                _invalidSSA; /* SSA needs to be invalidated after this opt */

    void                StartEndMessage_(bool);
    virtual void        ProcessDumpOptions_(DUMP_KIND);
    
    bool                LookupDisableable_(INT32 comp);
    bool                ComponentDisableable_(O64_COMPONENT comp);  

public:

    O64_Component(O64_COMPONENT component);
    ~O64_Component();

    O64_Option*         GetCurrentOption () { return _CurrentOption; }

};

// Declare the public interface for each component
extern INT32 Current_PU_Count();

#endif /* comp_decl_INCLUDED */
