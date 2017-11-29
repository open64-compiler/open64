
#ifndef opt_lclsc_INCLUDED
#define opt_lclsc_INCLUDED "opt_lclsc.h"

#include "opt_bb.h"           //BB_LIST_CONTAINER
#include "cxx_memory.h"       //CXX_NEW* && MEM_POOL
#include "opt_cfg.h"          //CFG
#include "opt_sym.h"          //OPT_STAB
#include "opt_mu_chi.h"
#include "opt_htable.h"       //CODEREP && STMTREP
#include "errors.h"            //Is_True DevWarn
#include "defs.h"             //IDTYPE
#include "vector"		//vector
#include "map"		//map
#include "bb_node_set.h"
#include "idx_32_set.h"
#include "opt_alias_rule.h"
#include "opt_points_to.h"
#include "stblock.h"

using namespace std;

class LCLSC_NODE{
private:
  ST* _st; //bit position assigned to variable
  BB_NODE_SET *_live_range;  //set of bbs in live range of the variable
public:
  LCLSC_NODE(ST *st, CFG *cfg, MEM_POOL *pool)
  	{
  	_st=st;
	_live_range=CXX_NEW(BB_NODE_SET(cfg->Total_bb_count(), cfg, pool, BBNS_EMPTY), pool);
	}
  ~LCLSC_NODE()   	{}
 ST* Get_st()  	{return _st;}
  BB_NODE_SET * Get_lr(void)   	{return _live_range;}
  void Print( FILE *fp=stderr)
  	{
        fprintf(fp, "st: %s ", ST_name(Get_st()));
        fprintf(fp, "live range:");
        Get_lr()->Print(fp);
        fprintf(fp, "\n");
        return;
       }
};

typedef map<ST*, IDX_32> STBP_MAP;
typedef vector<LCLSC_NODE*> LCLSC_NTAB;

class LOCAL_CLSC{
private:
  CFG *_cfg;
  OPT_STAB *_opt_stab;
  STBP_MAP _bp_map;  //map if aux_id -> bit position
  IDX_32 _bs_size;
  LCLSC_NTAB _clsc_ntab;
  MEM_POOL _pool;
  BOOL _tracing;

  CFG* Cfg() 	 {return _cfg;}
  OPT_STAB* Opt_stab()  	{return _opt_stab;}
  MEM_POOL *Pool()  	{return &_pool;}
  BOOL Tracing()  	{return _tracing;}
  IDX_32 Bs_size()  	{return _bs_size;}

  IDX_32 Get_bitpos(AUX_ID aux_id) 
  	{
          AUX_STAB_ENTRY *sym=Opt_stab()->Aux_stab_entry(aux_id);
          if(sym!=NULL) {
            ST *st=sym->St();
            if(st!=NULL && ST_class(st)==CLASS_PREG) {
              WN *preg_home=Preg_Home(sym->St_ofst());
              if(preg_home!=NULL && WN_has_sym(preg_home)) {
		st=WN_st(preg_home);	
              }
            }

            return Get_bitpos(st);
          }
          return ILLEGAL_BP;
	}

  IDX_32 Get_bitpos(ST *st)
  	{
  	  if(st!=NULL) {
            if(_bp_map.count(st)) {
              return _bp_map[st];
            }
  	  }
	  return ILLEGAL_BP;  	
  	}
 
  LCLSC_NODE * Add_node(ST *st, CFG *cfg)
	{
	  LCLSC_NODE *node=CXX_NEW(LCLSC_NODE(st, cfg, Pool()), Pool());
	  _clsc_ntab.push_back(node);
          return node;
	}
  
  BOOL LR_overlapped(BB_NODE_SET *lr0, BB_NODE_SET *lr1, CFG *cfg);
  void Init_bp_map(OPT_STAB *opt_stab);
  BOOL Aux_id_in_bs(AUX_ID aux_id, IDX_32_SET *bs);
  BOOL Add_aux_id_to_bs(AUX_ID aux_id, IDX_32_SET *bs);
  void Get_aux_id_by_alias(AUX_ID aux_id, AUX_ID_LIST *alist);
  void Collect_def_by_chi_list(CHI_LIST *chi_list, IDX_32_SET *appear_set);
  AUX_ID_LIST *Get_use_by_mu_node(MU_NODE *mnode);
  void Collect_use_by_mu_node(MU_NODE *mnode, IDX_32_SET *upwd_set, IDX_32_SET *appear_set, IDX_32_SET *def_set);
  void Collect_use_rec(CODEREP *cr, IDX_32_SET *upwd_set, IDX_32_SET *appear_set, IDX_32_SET *def_set);
  void Collect_def(CODEREP *cr, IDX_32_SET *appear_set, IDX_32_SET *def_set);
  void Collect_local_refs(CFG *cfg);
  void Calculate_liveness(CFG *cfg);
  void Get_lr(CFG *cfg);
  void Update_cr_alias(POINTS_TO *pt0, POINTS_TO *pt1, CODEREP *cr, OPT_STAB *opt_stab);
  void Update_alias(ST *st0, ST *st1, CFG *cfg, OPT_STAB *opt_stab);
  void Perform_clsc(CFG *cfg);

public:
  LOCAL_CLSC(CFG *cfg, OPT_STAB *opt_stab)
  	{
  	_cfg=cfg; 
 	_opt_stab=opt_stab;
	_tracing=Get_Trace( TP_WOPT2, LCLSC_TRACE_FLAG); 
	}
  
  ~ LOCAL_CLSC()  	{  	}
  void Do_local_clsc();
};
#endif
