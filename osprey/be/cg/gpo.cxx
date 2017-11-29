

#include "gpo.h"
#include "cxx_memory.h"

void
CG_PerformGPO(CG_GPO::GPO_PHASE phase)
{
	CG_GPO cg_gpo(phase);
}

const O64_ComponentDescriptor CG_GPO::ComponentDescriptor =
{
	O64_COMPONENT_DESC("Global Peephole Optimizer", "GPO", OptionDescriptors)
};

const O64_OptionDescriptor CG_GPO::OptionDescriptors[] =
{
  O64_OPTION_DESC(CG_GPO_allow_overlap, "allow overlapping live range in GPO",
    "allow_overlap", "overlap", OVK_BOOL, OV_INTERNAL, false, 0, 0, 0),

  O64_OPTION_DESC(CG_GPO_last, "end marker", 0, 0,
		OVK_INVALID, OV_INTERNAL, false, 0, 0, 0)

};

static O64_ComponentInitializer cg_gpo_init(
    COMPONENT_cg_gpo, &CG_GPO::ComponentDescriptor);

CG_GPO::CG_GPO(CG_GPO::GPO_PHASE phase)
    :O64_Component(COMPONENT_cg_gpo), _CurrentGPOPhase(phase),
    _CurrentCGCFG(NULL), _CurrentCGSSA(NULL)
{
    // disable by default
    if (!_enable) return;

    _AllowOverlap = _CurrentOption->GetBoolOption(_CurrentComponent, 
      CG_GPO_allow_overlap);

    ProcessDumpOptions_(_CurrentOption->GetEnumOption<DUMP_KIND>(
      _CurrentComponent, OPT_dump_before));

    Perform_();
}

CG_GPO::~CG_GPO()
{
    if (!_enable) return;

    ProcessDumpOptions_(_CurrentOption->GetEnumOption<DUMP_KIND>(
      _CurrentComponent, OPT_dump_after));
}

void
CG_GPO::Perform_()
{
    _CurrentCGCFG = CXX_NEW(CG_CFG(_LocalMemPool), _LocalMemPool);
    _CurrentCGCFG->Build();
    
    DOM_BUILDER<CG_CFG> dom_bld(*_CurrentCGCFG);
    dom_bld.Build_DOM();
    dom_bld.Build_PDOM();

    DF_BUILDER<CG_CFG> df_bld(*_CurrentCGCFG);
    df_bld.Build_DF();
    df_bld.Build_CD();

    _CurrentCGSSA = CXX_NEW(CGSSA(_CurrentCGCFG, *_LocalMemPool), 
      _LocalMemPool);
    _CurrentCGSSA->Build();

    _CurrentCGSSA_Updater = CXX_NEW(CGSSA_UPDATER(_CurrentCGSSA, _CurrentCGCFG),
      _LocalMemPool);

    if (AllowOverlap())
      _CurrentCGSSA->Leave();

}

void
CG_GPO::ProcessDumpOptions_(DUMP_KIND dumpKind) 
{
  switch(dumpKind) {
  case DUMP_none:
    return;
  case DUMP_ir:
    Print_All_BBs();
    break;
  case DUMP_cfg:
    if (CurrentCGCFG())
      CurrentCGCFG()->Print(TFile, DUMP_ACFG);
    break;
  case DUMP_ssa:
    if (CurrentCGSSA())
      CurrentCGSSA()->Print(TFile);
    break;
  case DUMP_vcg:
  {
    char * proc_name = STRDUP(Get_Procedure_Name());
    STRCAT(proc_name, ".vcg");
    draw_vcg_flow_graph(proc_name);
    break;
  }  
  case DUMP_maximal:
    break;
  default:
    break;
  }  
}
