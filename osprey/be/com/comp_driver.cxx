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
 * Module: comp_driver.cxx
 *
 * Revision history:
 *  Oct-10 - Original Version
 *
 * Description:
 *  O64_Driver and O64_Option class implementation
 * 
 * ====================================================================
 */

#include "comp_driver.h"
#include "errors.h"
#include "mempool.h"
#include "cxx_memory.h"
#include "erglob.h"

O64_Driver * O64_Driver::_theInstance = NULL;

// initialize the driver
static O64_DriverInitializer initialize_driver;

// driver's component descriptor
const O64_ComponentDescriptor O64_Driver::ComponentDescriptor =
{
    O64_COMPONENT_DESC("O64 driver", "DRIVER", OptionDescriptors)
};

// driver's option descriptors
const O64_OptionDescriptor O64_Driver::OptionDescriptors[] =
{
    // O64_OPTION_DESC(id, description, name, abbreviation, kind, visibility, changed by pragma, 
    // default value, minimal value, maximal value)

    O64_OPTION_DESC(OPT_driver_tstats, "compile time statistics",
                "tstats", "tstats", OVK_NONE, OV_INTERNAL, false, false, 0, 0),
    O64_OPTION_DESC(OPT_driver_mstats, "memory usage statistics",
                "mstats", "mstats", OVK_NONE, OV_INTERNAL, false, false, 0, 0),
    O64_OPTION_DESC(OPT_driver_last, "end marker", 0, 0, OVK_INVALID, OV_INTERNAL, false, 0, 0, 0)
};

// register the driver component descriptor to list
static O64_ComponentInitializer driver_init(
    COMPONENT_driver, &O64_Driver::ComponentDescriptor);


// =======================================================================
// O64_Driver Implementation
// =======================================================================

O64_Driver::O64_Driver()
    :_CurrentWN(NULL), _CompOptionArgc(0)
{
    MEM_POOL_Initialize(&_DriverPool, "DriverPool", FALSE);
    MEM_POOL_Initialize(&_LocalPool, "LocalMemPool", FALSE);

    MEM_POOL_Push(&_DriverPool);

    _CompOptionArgv = (char **)
        MEM_POOL_Alloc(&_DriverPool, sizeof(char*)*MaxOptionNum); 
    _ComponentDescriptorList.InitCompDescList(&_DriverPool);    
}

O64_Driver::~O64_Driver()
{
    MEM_POOL_Pop(&_DriverPool);
    MEM_POOL_Delete(&_DriverPool);
    MEM_POOL_Delete(&_LocalPool);
}

O64_Driver*
O64_Driver::GetInstance()
{
    if (_theInstance) return _theInstance;

    _theInstance = new O64_Driver();
    return _theInstance;
        
}

void
O64_Driver::Destroy()
{
    if (_theInstance) delete _theInstance;
    _theInstance = NULL;

}


// register the component descriptor into the list maintained in O64_Driver
void
O64_Driver::RegisterComponent(O64_COMPONENT component, const O64_ComponentDescriptor* desc)
{
    Is_True((COMPONENT_first <= component) && 
            (component < COMPONENT_last) && desc, 
            ("RegisterComponent: invalid component"));

    _ComponentDescriptorList.SetComponentDescriptor(component, desc); 
    _ComponentDescriptorList.SetComponentRegistered(component);   
    _ComponentDescriptorList.SetNumOfRegisteredComponents(
        _ComponentDescriptorList.GetNumOfRegisteredComponents() + 1);

}

// Save the componentization related commands into a string list 
// in O64_Driver. The commands will be processed once all the modules are
// loaded and thus all the components are registered.
bool
O64_Driver::SaveComponentOption(const char * argv)
{
    char * temp_str;
    if (!STRNCMP(argv, "-COMP-", STRLEN("-COMP-"))) {
        temp_str = STRDUP(argv);
        temp_str = temp_str + STRLEN("-COMP-");
        _CompOptionArgv[_CompOptionArgc] = (char *) 
            MEM_POOL_Alloc(&_DriverPool, sizeof(char)*(STRLEN(temp_str)+1));
        STRCPY(_CompOptionArgv[_CompOptionArgc], temp_str);
        _CompOptionArgc++;
        FmtAssert(_CompOptionArgc <= MaxOptionNum, 
            ("too large option string"));
        return true; 
    }
    return false;
}

// Create O64_Option object to store all registed components' option (if not exist)
// Process the component-based command option 
void
O64_Driver::ProcessComponentOption()
{
    FmtAssert(_NumRegisteredComponents <= COMPONENT_last,
        ("[O64_Driver] Component Registration Error"));

    if (_CurrentOption == NULL) {
        _CurrentOption = CXX_NEW(O64_Option, &_DriverPool);
        Is_True(_CurrentOption, ("[O64_Driver] Null CurrentOption"));
    }

    for (INT i = 0; i < _CompOptionArgc; i++) {
        if (!_CurrentOption->ProcessComponentOption_(_CompOptionArgv[i])) 
            ErrMsg (EC_Unknown_Flag, _CompOptionArgv[i][0], _CompOptionArgv[i]);
    }
}

TRACE_OPTION_KIND
O64_Driver::GetTraceKind()
{
    return _CurrentOption->GetEnumOption<TRACE_OPTION_KIND>(COMPONENT_driver, OPT_trace);
}

BOOL
O64_Driver::TimeStats()
{
    return _CurrentOption->GetBoolOption(COMPONENT_driver, OPT_driver_tstats);
}

BOOL
O64_Driver::MemStats()
{
    return  _CurrentOption->GetBoolOption(COMPONENT_driver, OPT_driver_mstats);
}    

// =======================================================================
// O64_Option Implementation
// =======================================================================

// names for trace option (OVK_ENUM)
// keep in sync with TRACE_OPTION_KIND definition in common/util/flags.h
const char * TraceOptionNames[] =
{
    "none",
    "info",
    "min",
    "med",
    "max"
};

// names for dump option (OVK_ENUM)
// keep in sync with DUMP_KIND definition in common/util/flags.h
const char * DumpOptionNames[] =
{
    "none",
    "ir",
    "cfg",
    "ssa",
    "vcg",
    "max"
};

// option descriptors for common options
static const O64_OptionDescriptor 
CommonOptionDescriptors[OPT_common_last+1] =
{
    // O64_OPTION_DESC(id, description, name, abbreviation, kind, visibility, changed by pragma, 
    // default value, minimal value, maximal value)
        
    O64_OPTION_DESC(OPT_enable, "enable the component",
                "ENABLE",  "enable", OVK_NONE, OV_INTERNAL, false, false, 0, 0),
    O64_OPTION_DESC(OPT_disable, "disable the component",
                "DISABLE", "disable", OVK_NONE, OV_INTERNAL, false, false, 0, 0),
                
    // O64_ENUM_OPTION_DESC(id, description, name, abbreviation, visibility, changed by pragma,
    // default value, minimal value, maximal value, default set value, names for each enum).
    // default value is when the option is not specified;
    // default set value is when the option is specified with "".
    // for example, if dump_before is not specified, the following option is DUMP_none;
    // if -COMP:db is specified, this option is DUMP_ir;
    // if -COMP:db=ir is specified, this option is DUMP_ir; 
    // if -COMP:db=cfg is specified, this option is DUMP_cfg;
    
    O64_ENUM_OPTION_DESC(OPT_dump_before, "dump [ir|cfg|ssa|vcg|max] before the component",
                "dump_before", "db",OV_INTERNAL, false, DUMP_none, DUMP_none, DUMP_maximal,
                DUMP_ir, DumpOptionNames),
    O64_ENUM_OPTION_DESC(OPT_dump_after, "dump [ir|cfg|ssa|vcg|max] after the component",
                "dump_after", "da",  OV_INTERNAL, false, DUMP_none, DUMP_none, DUMP_maximal,
                DUMP_ir, DumpOptionNames),
    O64_ENUM_OPTION_DESC(OPT_trace, "dump out trace information [info|min|med|max]", 
                 "TRACE", "trace", OV_INTERNAL, false, TRACE_none, TRACE_none, TRACE_maximal,
                 TRACE_maximal, TraceOptionNames),
    O64_OPTION_DESC(OPT_stats, "gather and print component statistics",
                "STATS", "stats", OVK_NONE, OV_INTERNAL, false, false, 0, 0),
    O64_OPTION_DESC(OPT_skip_b, "skip the function unit before",
                "skip_b", "skip_b", OVK_UINT32, OV_INTERNAL, false, 0, 0, UINT32_MAX),
    O64_OPTION_DESC(OPT_skip_a, "skip the function unit after",
                "skip_a", "skip_a", OVK_UINT32, OV_INTERNAL, false, UINT32_MAX, 0, UINT32_MAX),
    O64_OPTION_DESC(OPT_common_last, "end marker", 0, 0, OVK_INVALID, OV_INTERNAL, false, 0, 0, 0)
};

O64_Option::O64_Option()
    :_CommonOptionDescriptors(CommonOptionDescriptors)
{

    _ComponentDescriptorList = O64_Driver::GetInstance()->GetComponentDescriptorList();
    _MemPool = O64_Driver::GetInstance()->GetDriverMemPool();

    _Options = (_Value **) MEM_POOL_Alloc(_MemPool, 
      sizeof(_Value*) *GetNumOfComponents_());
    _OptionsSet = (BOOL **) MEM_POOL_Alloc(_MemPool, 
      sizeof(BOOL*) *GetNumOfComponents_());    

    for (UINT32 component = 0; component < GetNumOfComponents_(); ++component)
    {
        // if the component not registered, we could not find its option
        // descriptors
        if (!_ComponentDescriptorList->IsComponentRegistered(component))
            continue;

        UINT32 option;
        for (option = 0; GetOptionName_(component, option); ++option);

        
        _Options[component] = (_Value *) MEM_POOL_Alloc(_MemPool, 
          sizeof(_Value) * option);
        _OptionsSet[component] = (BOOL*) MEM_POOL_Alloc(_MemPool, 
          sizeof(BOOL) * option);      

        for (option = 0; GetOptionName_(component, option); ++option)
        {
            Is_True(GetOptionDescriptor_(component, option)->OptionId == option,
                ("Inconsistent option declaration"));
            SetDefaultOption_(component, option);
        }
    } 
    
}

O64_Option::~O64_Option()
{
    for (INT32 component = 0; component < GetNumOfComponents_(); ++component)
    {
        if (!_ComponentDescriptorList->IsComponentRegistered(component))
            continue;

        for (INT32 option = 0; GetOptionName_(component, option); ++option)
        {
            // free the strings that are generated by STRDUP
            if (GetOptionKind_(component, option) == OVK_NAME ||
                GetOptionKind_(component, option) == OVK_SELF)
                if (_Options[component][option]._StringVal)
                    free(_Options[component][option]._StringVal);
         }           

        MEM_POOL_FREE(_MemPool, _Options[component]);
        MEM_POOL_FREE(_MemPool, _OptionsSet[component]);
    }

    MEM_POOL_FREE(_MemPool, _Options);
    MEM_POOL_FREE(_MemPool, _OptionsSet);
}

//
// Process component-option
// "-component-name:option-list"
//
bool
O64_Option::ProcessComponentOption_(char * component_options)
{
    if (!component_options) return true;

    char * option_dup, * option_start;

    option_start = component_options;
    option_dup = STRDUP(option_start);

    if (!STRCASECMP("OPTIONS", option_dup)) {
        PrintOptionsUsage_();
        return true;
    }

    option_start = STRTOK(option_dup, ":");
    if (option_start == NULL) return false;

    for (INT32 component = 0; component <= GetNumOfComponents_(); ++component) {
        if (!_ComponentDescriptorList->IsComponentRegistered(component)) {
            continue;
        }    
        if (!STRCASECMP(GetComponentName_(component), option_start)) {
            option_start = STRTOK(NULL, ":");
            if (ProcessOptionList_(component, option_start))
                return true;
        }
    }
    return false;
};

// Process a list of options 
// option['+'option]*
//
bool
O64_Option::ProcessOptionList_(INT32 component, char* options)
{
    if (!options) return true;

    bool return_value = true;

    char *option_dup, *option_start;
    
    option_dup = STRDUP(options);
    option_start = STRTOK(option_dup, "+");
    while (option_start != NULL) {
        if (!STRCASECMP("OPTIONS", option_start)) {
            PrintOptionsUsage_(component);
        }
        else {
            if (!ProcessOption_(component, option_start))
                return_value = false;
        }
        option_start = STRTOK(NULL, "+");
    }    
    return return_value;
}

// Process a option
// option_name[=option_value]
bool
O64_Option::ProcessOption_(INT32 component, char *option_str)
{
    if (!option_str) return true;

    char *option_dup, *option_start;
    char *option_name, *option_value;

    option_dup = STRDUP(option_str);
    option_start = STRCHR(option_dup, '=');
    if (!option_start) {
        option_name = option_dup;
        option_value = NULL;
    }
    else {
        option_value = option_start+1;
        *option_start = '\0';
        option_name = option_dup;
    }
    
    bool option_found = false;
    for (INT32 option = 0; GetOptionName_(component, option); ++option) {
        if (!STRCASECMP(GetOptionName_(component, option), option_name) ||
            !STRCASECMP(GetOptionAbbrev_(component, option), option_name)) {
            if (SetOptionValue_(component, option, option_value))
                option_found = true; 
            break;  
        }       
    }       
    if (!option_found) {
        return false;
    }
    return true;
}

// set up the default value for options
//
bool
O64_Option::SetOptionValue_(INT32 component, INT32 option, char * option_value)
{
    INT32 int32_value;
    INT64 int64_value;
    UINT32 uint32_value;
    UINT64 uint64_value;

    switch (GetOptionKind_(component, option)) {
    case OVK_NONE:
        Is_True(!option_value, ("OVK_NONE should have no value"));
        _Options[component][option]._BoolVal = true;
        break;
    case OVK_BOOL:
        Is_True(option_value, ("OVK_BOOL should have value"));
        if (!STRCASECMP(option_value, "ON") ||
            !STRCASECMP(option_value, "YES") ||
            !STRCASECMP(option_value, "TRUE"))
            _Options[component][option]._BoolVal = true;
        else if (!STRCASECMP(option_value, "OFF") ||
                !STRCASECMP(option_value, "NO")  ||
                !STRCASECMP(option_value, "FALSE"))
            _Options[component][option]._BoolVal = false;
        else
        {
            Is_True(false,
                ("wrong option value \"%s\", use ON|OFF or YES|NO or TRUE|FALSE", 
                  option_value));
            return false;
        }
        break;
    case OVK_INT32:
        Is_True(option_value, ("OVK_INT32 should have value"));
        int32_value = (INT32) STRTOL(option_value, (char**) NULL, 0);
        Is_True((GetMinVal_(component, option) <= int32_value) &&
                (GetMaxVal_(component, option) >= int32_value), ("OVK_INT32 out of range"));
        _Options[component][option]._IntVal = int32_value;
        break;
    case OVK_INT64:
        Is_True(option_value, ("OVK_INT64 should have value"));
        int64_value = (INT64) STRTOL(option_value, (char**) NULL, 0);
        Is_True((GetMinVal_(component, option) <= int64_value) &&
                (GetMaxVal_(component, option) >= int64_value), ("OVK_INT64 out of range"));
        _Options[component][option]._IntVal = int64_value;
        break;
    case OVK_UINT32:
        Is_True(option_value, ("OVK_UINT32 should have value"));
        uint32_value = (UINT32) STRTOL(option_value, (char**) NULL, 0);
        Is_True((GetMinVal_(component, option) <= uint32_value) &&
                (GetMaxVal_(component, option) >= uint32_value), ("OVK_UINT32 out of range"));
        _Options[component][option]._UIntVal = uint32_value;
        break;
    case OVK_UINT64:
        Is_True(option_value, ("OVK_UINT64 should have value"));
        uint64_value = (UINT64) STRTOL(option_value, (char**) NULL, 0);
        Is_True((GetMinVal_(component, option) <= uint64_value) &&
                (GetMaxVal_(component, option) >= uint64_value), ("OVK_UINT64 out of range"));
        _Options[component][option]._UIntVal = uint64_value;
        break;
    case OVK_NAME:
    case OVK_SELF:
         Is_True(option_value, ("OVK_NAME should have value"));
         _Options[component][option]._StringVal = STRDUP(option_value);
         break;      
    case OVK_LIST:
        break;
    case OVK_ENUM:
       if (option_value)
       {
            const INT32 from = GetMinVal_(component, option);
            const INT32 to = GetMaxVal_(component, option);
            const char * const * names = GetValueNames_ (component, option);
            INT32 enumval;
            for (enumval = from; enumval <= to; ++enumval)
                if (!STRCASECMP(option_value, names[enumval - from])) {
                    _Options[component][option]._EnumVal = enumval;
                    break;
                }
            if (enumval > to)
            {
                Is_True(false, ("Unrecognized enumerator option %s", option_value));
                return false;
            }
        }
        else
            _Options[component][option]._EnumVal = GetDefaultSetVal_(component, option);
        break;
    default:  
        Is_True(false, ("Unimplemented option kind"));
        return false;
    }
    
    //set the _OptionSet (i.e., the value is from command line)
    _OptionsSet[component][option] = TRUE;
    return true;
}

// the print name for each option kind
//
static const char *
KindName(OPTION_KIND kind)
{
    // Keep this array in sync with the OPTION_KIND in flags.h
    static const char * const kind_name[OVK_ENUM - OVK_INVALID+1] = {
        "",
        "OVK_NONE",
        "OVK_BOOL",
        "OVK_INT32",
        "OVK_INT64",
        "OVK_UINT32",
        "OVK_UINT64",
        "OVK_NAME",
        "OVK_SELF",
        "OVK_LIST",
        "OVK_OBSOLETE",
        "OVK_OLD_COUNT",
        "OVK_REPLACED",
        "OVK_UNIMPLEMENTED",
        "OVK_ENUM"
    };

    return ((kind >= OVK_INVALID ) && (kind <= OVK_ENUM))
           ? kind_name[kind]
           : "???";
}

// print out the option usage
//
void
O64_Option::PrintOptionsUsage_()
{
    // print the usage for common options
    PrintOptionsUsage_(GetNumOfComponents_());

    for (INT32 component = 0; component < GetNumOfComponents_(); ++component)
        PrintOptionsUsage_(component);
}

void
O64_Option::PrintOptionsUsage_(INT32 component)
{
    bool common = (component == GetNumOfComponents_());

    INT32 first = common ?  OPT_common_first : OPT_component_first;
    INT32 last = common ? OPT_common_last : INT32_MAX;

    fprintf(stderr,  "[%s]", GetComponentName_(component));
    fprintf(stderr, " %s",
            common ? "Common options" : GetComponentDescriptor_(component)->CompDesc);
    fprintf(stderr, "\n");

    if (common) component = 0;
    const char * name, * abbr;
    int max_name_len = 1, max_abbr_len=1;
    for (INT32 option = first; (option < last) && (name = GetOptionName_(component, option)); 
        ++option)
    {
        abbr = GetOptionAbbrev_(component, option);
        size_t name_len = STRLEN(name);
        size_t abbr_len = STRLEN(abbr);    
        if (name_len > max_name_len)
            max_name_len = (int)name_len;
        if (abbr_len > max_abbr_len)
            max_abbr_len = (int) abbr_len;
    }
    for (INT32 option = first; (option < last) && (name = GetOptionName_(component, option)); 
        ++option)
    {
        abbr = GetOptionAbbrev_(component, option);
        fprintf(stderr, "  o %-*s", max_name_len, name);
        fprintf(stderr, " : %-*s", max_abbr_len, abbr);    
        fprintf(stderr, " : %-6s : %s",
                KindName(GetOptionKind_(component, option)),
                GetOptionDescriptor_(component, option)->OptionDesc);
        fprintf(stderr, "\n");
    }
}

// =======================================================================
// O64_ComponentInitializer Implementation
// =======================================================================

O64_ComponentInitializer::O64_ComponentInitializer(
    O64_COMPONENT component, const O64_ComponentDescriptor* comp_descr)
{
    O64_Driver::GetInstance()->RegisterComponent(component, comp_descr);
}
