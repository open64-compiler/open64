#ifndef gpo_INCLUDED
#define gpo_INCLUDED

#include "comp_decl.h"
#include "cg_cfg.h"
#include "cgssa_core.h"
#include "cgssa_update.h"

using namespace CFG_UTIL;
using namespace CGSSA_NAME;

class CG_GPO: public O64_Component
{
public:
    // options for CG_GPO component
    // using the new component-based infrastructure
    enum OPTION
    {
        CG_GPO_first = OPT_component_first,
        CG_GPO_allow_overlap = CG_GPO_first,
        CG_GPO_last
    };

    static const O64_ComponentDescriptor ComponentDescriptor;
    static const O64_OptionDescriptor
        OptionDescriptors[CG_GPO_last - CG_GPO_first + 1];

    // to specify where GPO is invoked
    enum GPO_PHASE
    {
      GPO_Before_RA,
      GPO_After_RA
    };

    CG_GPO(GPO_PHASE phase);
    ~CG_GPO();

private:

    CG_CFG*         _CurrentCGCFG;   
    CGSSA*          _CurrentCGSSA;
    CGSSA_UPDATER * _CurrentCGSSA_Updater;

    GPO_PHASE   _CurrentGPOPhase;

    BOOL        _AllowOverlap;
    void        Perform_();

    void        ProcessDumpOptions_(DUMP_KIND);

public:
    GPO_PHASE   CurrentGPOPhase() { return _CurrentGPOPhase; }
    CG_CFG*     CurrentCGCFG()    { return _CurrentCGCFG; }
    CGSSA*      CurrentCGSSA()    { return _CurrentCGSSA; }
    CGSSA_UPDATER * CurrentCGSSA_Updater() { return _CurrentCGSSA_Updater; }

    BOOL        AllowOverlap()    { return _AllowOverlap; }

};    


// declare the public interface for GPO optimization
extern void CG_PerformGPO(CG_GPO::GPO_PHASE);
extern void draw_vcg_flow_graph(const char* fname);

#endif /* gpo_INCLUDED */
