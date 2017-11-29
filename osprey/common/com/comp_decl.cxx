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
 * Module: comp_decl.cxx
 *
 * Revision history:
 *  Oct-10 - Original Version
 *
 * Description:
 *  Implementation for O64_Component 
 *
 * ====================================================================
 */

#include "comp_decl.h"
#include "opt_wn.h"
#include "glob.h"

O64_Component::O64_Component(O64_COMPONENT component)
{
    _CurrentComponent = component;
    _canBeDisabled = ComponentDisableable_(_CurrentComponent);
    _Driver = O64_Driver::GetInstance();
    if (!_Driver->GetComponentDescriptorList()->IsComponentRegistered(component)) {
        _enable = false;
        _disable = true;
        return;
    }    

    _CurrentOption = _Driver->GetCurrentOption();


    if (Current_PU_Count() < _CurrentOption->GetUIntOption(_CurrentComponent, OPT_skip_b) ||
        Current_PU_Count() > _CurrentOption->GetUIntOption(_CurrentComponent, OPT_skip_a))
    {
        _enable = false;
        _disable = true;
        return;
    }    
        
    _enable = _CurrentOption->GetBoolOption(_CurrentComponent, OPT_enable);
    _disable = _CurrentOption->GetBoolOption(_CurrentComponent, OPT_disable);

    if (_disable) return;

    _LocalMemPool = _Driver->GetLocalMemPool();
    MEM_POOL_Push(_LocalMemPool);
    
    _doStats   = _CurrentOption->GetBoolOption(_CurrentComponent, OPT_stats);
    _TraceKind = _CurrentOption->GetEnumOption<TRACE_OPTION_KIND>(_CurrentComponent, OPT_trace);
    _doTrace   = (_TraceKind != TRACE_none);

    ProcessDumpOptions_(
        _CurrentOption->GetEnumOption<DUMP_KIND>(_CurrentComponent, OPT_dump_before));

    // driver level trace
    StartEndMessage_(true);
}

O64_Component::~O64_Component()
{
    if (Current_PU_Count() < _CurrentOption->GetUIntOption(_CurrentComponent, OPT_skip_b) ||
        Current_PU_Count() > _CurrentOption->GetUIntOption(_CurrentComponent, OPT_skip_a))
        return;

    if (_disable) return;

    ProcessDumpOptions_(
         _CurrentOption->GetEnumOption<DUMP_KIND>(_CurrentComponent, OPT_dump_after));
            
    // driver-level trace
    StartEndMessage_(false);

    MEM_POOL_Pop(_LocalMemPool);

    // TODO: invalidate data structure (e.g., cfg) when they are ready

}

void
O64_Component::StartEndMessage_(bool isStart)
{
    if (_Driver->GetTraceKind() != TRACE_info) return;

    fprintf(TFile, "[DRIVER]%s %-10s",
            isStart ? "starting" : "finished",
            _CurrentOption->GetComponentName_(_CurrentComponent));
            
    if (isStart) 
        fprintf(TFile, " for \"%s\" ", Cur_PU_Name);
                
    fprintf(TFile, "\n");
}

void
O64_Component::ProcessDumpOptions_(DUMP_KIND dumpKind)
{
    switch(dumpKind) {
    case DUMP_none:
        return;
    case DUMP_ir:
        if (_Driver->GetCurrentWN())
          fdump_tree(TFile, _Driver->GetCurrentWN());
        break;
    case DUMP_cfg:
        break;
    case DUMP_ssa:
        break;
    case DUMP_maximal:
        break;
    default:
        break;
    }    
}

static O64_COMPONENT NonDisableAbles[] = 
{
    COMPONENT_driver,
    COMPONENT_invalid  /* sentinel */
};

bool 
O64_Component::LookupDisableable_(INT32 comp)
{
    int index = 0;
    
    while(NonDisableAbles[index] != COMPONENT_invalid)
    {   
        if (NonDisableAbles[index] == comp)
            return false;
        ++index;
    }
    return true;
}

bool 
O64_Component::ComponentDisableable_(O64_COMPONENT comp)
{   
    // prepare lookup table (only once)
    //
    static bool tableInitialized = false;
    static bool table[COMPONENT_last];
    
    if (!tableInitialized)
    {  
        Is_True(COMPONENT_first == 0, ("COMPONENT_first should be zero"));
 
        for (INT32 comp = COMPONENT_first; comp < COMPONENT_last; ++comp)
            table[comp] = LookupDisableable_(comp);
        
        tableInitialized = true;
    }
    
    // lookup
    return table[comp];
}


