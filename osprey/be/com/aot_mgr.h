/*
 * Copyright (C) 2021 Xcalibyte (Shenzhen) Limited.
 */

//-*-c++-*-
// ====================================================================
// ====================================================================
//
// Module: aot_mgr.h
//
// ====================================================================
//


#ifndef aot_mgr_INCLUDED
#define aot_mgr_INCLUDED

#include <vector>
#include "defs.h"
#include "tracing.h"
#include "errors.h"
#include "whirl_comp_base.h"
#include "timing.h"
#include "pu_info.h"

// Macro defines
#define AOT_PHASE_NAME "AOT"
#define Last_Dedicated_Preg_Offset_X8664    48

// class forward declartion
class IPA_AOT_EXECUTOR;
class BE_AOT_EXECUTOR;
class IPA_CALL_GRAPH;
class IPA_NODE;
class AOT_COMP_DRIVER;
class WN;

// Enums 
typedef enum {
  TT_AOT_FLOW    = 0x00000001,
  TT_AOT_PTN     = 0x00000002,
  TT_AOT_LAST    = TT_AOT_PTN,
  TT_AOT_ALL     = 0xffffffff,
} AOT_TRACE_FLAGS;

typedef std::vector<const char *> STR_ARR;

class AOT_COMP_DRIVER
{
private:
  CXX_MEM_POOL        _pool;
  BOOL                _keep;
  const char         *_phase;
  const char         *_output_file;
  const char         *_workdir;
  const char         *_uwm_driver_path;
  const char         *_nat_driver_path;
  const char         *_linker_path;
  STR_ARR             _uwm_inputs;
  STR_ARR             _uwm_options;
  STR_ARR             _nat_inputs;
  STR_ARR             _nat_options;
  STR_ARR             _symtab_options;

  AOT_COMP_DRIVER(const AOT_COMP_DRIVER&);              // REQUIRED UNDEFINED UNWANTED methods
  AOT_COMP_DRIVER& operator = (const AOT_COMP_DRIVER&); // REQUIRED UNDEFINED UNWANTED methods
public:
  AOT_COMP_DRIVER(void) :  _pool("AOT_COMP_POOL", FALSE), _phase(AOT_PHASE_NAME), _keep(FALSE) {}

  MEM_POOL      *Mem_pool(void)                  { return _pool();                 }
  BOOL           Keep(void) const                { return _keep;                   }
  const char    *Get_phase(void) const           { return _phase;                  }
  const char    *Get_workdir(void) const         { return _workdir;                }
  const char    *Get_output(void) const          { return _output_file;            }
  const char    *Get_nat_driver(void) const      { return _nat_driver_path;        }
  const char    *Get_uwm_driver(void) const      { return _uwm_driver_path;        }
  const char    *Get_linker(void) const          { return _linker_path;            }
  const STR_ARR &Get_nat_files(void) const       { return _nat_inputs;             }
  const STR_ARR &Get_uwm_files(void) const       { return _uwm_inputs;             }
  const STR_ARR &Get_nat_opt(void) const         { return _nat_options;            }
  const STR_ARR &Get_uwm_opt(void) const         { return _uwm_options;            }
  const STR_ARR &Get_symtab_opt(void) const      { return _symtab_options;         }
  void           Set_keep(BOOL v)                { _keep = v;                      }
  void           Set_workdir(const char* dir)    { _workdir = dir;                 }
  void           Set_phase(const char* name)     { _phase = name;                  }
  void           Set_output(const char *output)  { _output_file = output;          }
  void           Set_uwm_driver(const char *path){ _uwm_driver_path = path;        }
  void           Set_nat_driver(const char *path){ _nat_driver_path = path;        }
  void           Set_linker(const char *path)    { _linker_path = path;            }
  void           Add_uwm_file(const char *f)     { _uwm_inputs.push_back(f);       }
  void           Add_nat_file(const char *f)     { _nat_inputs.push_back(f);       }
  void           Add_uwm_option(const char *opt) { _uwm_options.push_back(opt);    }
  void           Add_nat_option(const char *opt) { _nat_options.push_back(opt);    }
  void           Add_symtab_option(const char *o){ _symtab_options.push_back(o);   }
  template<typename T>
  void           Initialize(T *);
  char*          Gen_makefile();
  BOOL           Verify(void);
};

// =============================================================================
//
//  class AOT_MGR manage aot code generation process
//  AOT_MGR can be triggered in IPA/IPSA/BE
//  The general flow is:
//  Do_ptn()->Do_emit()->Do_comp()
//
// =============================================================================
template<typename AOT_EXECUTOR, typename FUNC_NODE>
class AOT_MGR
{
  typedef std::vector<FUNC_NODE *> FUNC_VEC;
  typedef typename std::vector<FUNC_NODE *>::iterator FUNC_VEC_ITER;
private:
  BOOL                     _tracing;
  INT32                    _flags;
  SKIPLIST                *_skip_list;
  AOT_EXECUTOR            &_executor;
  FUNC_VEC                 _nat_funcs;
  FUNC_VEC                 _uwm_funcs;

  AOT_MGR(const AOT_MGR&);              // REQUIRED UNDEFINED UNWANTED methods
  AOT_MGR& operator = (const AOT_MGR&); // REQUIRED UNDEFINED UNWANTED methods
public:
  AOT_MGR(AOT_EXECUTOR &exec): _executor(exec)
                                     { _tracing = Get_Trace(TP_AOT, TT_AOT_FLOW); }
  BOOL Tracing(void)                 { return _tracing; }
  BOOL Do_flush(void)                { return _executor.Do_flush(); };
  const char* Fname(FUNC_NODE *f)    { return _executor.Fname(f);   };
  void Adjust_preg_num(FUNC_NODE *f) { _executor.Adjust_preg_num(f); }

  void Push_native(FUNC_NODE *node)  { _nat_funcs.push_back(node); }
  void Push_uwasm(FUNC_NODE *node)   { _uwm_funcs.push_back(node); }

  // helper functions
  BOOL
  Do_ptn(void)
  {
    Set_Error_Phase("AOT_PTN");
    Start_Timer(T_AOT_PTN);
    Is_Trace(Tracing(), (TFile, "%sAOT_PTN BEGIN:\n", DBar));

    _executor.Do_ptn(&_nat_funcs, &_uwm_funcs);

    Is_Trace_cmd(Tracing(), Print(TFile));
    Is_Trace(Tracing(), (TFile, "AOT_PTN END:\n"));
    Stop_Timer(T_AOT_PTN);
  }

  BOOL
  Do_emit(void)
  {
    Set_Error_Phase("AOT_EMIT");
    FUNC_VEC_ITER nat_it;
    for(nat_it = _nat_funcs.begin(); nat_it != _nat_funcs.end(); nat_it++) {
      Is_Trace(Tracing(), (TFile, "AOT_EMIT PU: %s\n", Fname(*nat_it)));
      // adjust uwasm preg num to native preg
      Adjust_preg_num(*nat_it);
      (*nat_it)->Write_PU();
    }
    Do_flush();
    FUNC_VEC_ITER uwm_it;
    for(uwm_it = _uwm_funcs.begin(); uwm_it != _uwm_funcs.end(); uwm_it++) {
      Is_Trace(Tracing(), (TFile, "AOT_EMIT PU: %s\n", Fname(*uwm_it)));
      (*uwm_it)->Write_PU();
    }
    Do_flush();
  }

  BOOL
  Do_comp(void)
  {
    Set_Error_Phase("AOT_COMP");
    Start_Timer(T_AOT_COMP);
    Is_Trace(Tracing(), (TFile, "%sAOT_COMP BEGIN:\n", DBar));

    AOT_COMP_DRIVER aot_comp;
    WHIRL_COMP_BASE<AOT_COMP_DRIVER> comp_driver(aot_comp);
    comp_driver.Compile(this);

    Is_Trace(Tracing(), (TFile, "AOT_COMP END:\n"));
    Stop_Timer(T_AOT_COMP);
  }

  void Print(FILE *fp)
  {
    fprintf(fp, "Native funcs:\n");
    FUNC_VEC_ITER nat_it;
    for(nat_it = _nat_funcs.begin(); nat_it != _nat_funcs.end(); nat_it++) {
      fprintf(fp, "  %s\n", Fname(*nat_it));
    }

    fprintf(fp, "Uwasm funcs:\n");
    FUNC_VEC_ITER uwm_it;
    for(uwm_it = _uwm_funcs.begin(); uwm_it != _uwm_funcs.end(); uwm_it++) {
      fprintf(fp, "  %s\n", Fname(*uwm_it));
    }
  }
};

typedef AOT_MGR<IPA_AOT_EXECUTOR, IPA_NODE> IPA_AOT_MGR;
typedef AOT_MGR<BE_AOT_EXECUTOR, PU_Info> BE_AOT_MGR;
typedef std::vector<IPA_NODE *> IPA_FUNC_VEC;
typedef std::vector<PU_Info *> PU_VEC;

class IPA_AOT_EXECUTOR
{
private:
  IPA_CALL_GRAPH  *_call_graph;

  IPA_AOT_EXECUTOR(const IPA_AOT_EXECUTOR&);              // REQUIRED UNDEFINED UNWANTED methods
  IPA_AOT_EXECUTOR& operator = (const IPA_AOT_EXECUTOR&); // REQUIRED UNDEFINED UNWANTED methods
public:
  IPA_AOT_EXECUTOR(IPA_CALL_GRAPH *cg) : _call_graph(cg) {}
  IPA_CALL_GRAPH *Get_call_graph() { return _call_graph; }

  BOOL Do_ptn(IPA_FUNC_VEC *nat_funcs, IPA_FUNC_VEC *uwm_funcs);
  BOOL Do_flush();
  void Mark_native_for_cycle();
  void Adjust_preg_num(IPA_NODE *node);
  const char* Fname(IPA_NODE *node);
};

class BE_AOT_EXECUTOR
{
private:

  BE_AOT_EXECUTOR(const BE_AOT_EXECUTOR&);              // REQUIRED UNDEFINED UNWANTED methods
  BE_AOT_EXECUTOR& operator = (const BE_AOT_EXECUTOR&); // REQUIRED UNDEFINED UNWANTED methods
public:
  BE_AOT_EXECUTOR() {}
  BOOL Do_ptn(PU_VEC *nat_funcs, PU_VEC *uwm_funcs);
  BOOL Do_flush();
  void Adjust_preg_num(PU_Info *pu_info);
  const char* Fname(PU_Info *node);

private:
  void Identify_native_code(PU_Info *pu_info);
  void Insert_native_region(WN *parent, WN *wn);
  void Transform_native_regions(PU_Info *pu_info);
};

extern void Adjust_tree_preg_num(WN *tree);
extern void Adjust_stmt_preg_num(WN *stmt);
#endif
