#include "opt_lclsc.h"

  //calculate the total bits needed for all the local variables and
  //initialize the _bp_map between aux_id of local variabls and bit position
void
LOCAL_CLSC::Init_bp_map(OPT_STAB *opt_stab)
{
  IDX_32 nbits=0;
  AUX_ID i;
  AUX_STAB_ITER aux_stab_iter(opt_stab);
  FOR_ALL_NODE(i, aux_stab_iter, Init()) {
    AUX_STAB_ENTRY *sym = opt_stab->Aux_stab_entry(i);
    ST *st=sym->St();
    if(st!=NULL && ST_sclass(st)==SCLASS_AUTO) {
        if(!_bp_map.count(st))  {
          (_bp_map)[st]=nbits;
	  nbits++;
        }
    }
  }
  _bs_size=nbits;

  if(Tracing()) {
    fprintf(TFile, "dump bitpos of symbals: \n");
    STBP_MAP::iterator mitr=_bp_map.begin();
    STBP_MAP::iterator mitrn=_bp_map.end();
    while(mitr!=mitrn) {
      ST* st=(*mitr).first;
      IDX_32 bp=(*mitr).second;
      fprintf(TFile, "st: %s,  bp: %d\n", ST_name(st), bp);
      mitr++;
    }
  }

  return;
}


BOOL
LOCAL_CLSC::Aux_id_in_bs(AUX_ID aux_id, IDX_32_SET *bs)
{
  IDX_32 bitpos=Get_bitpos(aux_id);
  if(bitpos!=ILLEGAL_BP) {
    if(bs->MemberP(bitpos))
      return TRUE;
  }
  return FALSE;     
}

BOOL
LOCAL_CLSC:: Add_aux_id_to_bs(AUX_ID aux_id, IDX_32_SET *bs)
{
  IDX_32 bitpos=Get_bitpos(aux_id);
  if(bitpos!=ILLEGAL_BP) {
    bs->Union1D(bitpos);
    return TRUE;
  }
  else
    return FALSE;
}

void
LOCAL_CLSC::Get_aux_id_by_alias(AUX_ID aux_id, AUX_ID_LIST *alist)
{
  const BS *alias_set = Opt_stab()->Indirect(); // all scalars + virtuals
  for (AUX_ID idx = BS_Choose( alias_set );
        idx != (AUX_ID) BS_CHOOSE_FAILURE;
        idx = BS_Choose_Next ( alias_set, idx )) {

        if (Opt_stab()->Rule()->Aliased_Memop(Opt_stab()->Aux_stab_entry(aux_id)->Points_to(), 
		    Opt_stab()->Aux_stab_entry(idx)->Points_to())) 
          alist->New_aux_id_node(idx, Pool());
  }
  return;
}

void
LOCAL_CLSC::Collect_def_by_chi_list(CHI_LIST *chi_list, IDX_32_SET *appear_set)
{
  AUX_ID_LIST *alist=CXX_NEW(AUX_ID_LIST, Pool());
  alist->Clear();
  CHI_NODE *cnode;
  CHI_LIST_ITER chi_iter;
  FOR_ALL_NODE(cnode, chi_iter, Init(chi_list)) {
    if(cnode->Live()) {
      AUX_ID aux_id=cnode->Aux_id();
      alist->New_aux_id_node(aux_id, Pool());
      //deal with virtual variable
      AUX_STAB_ENTRY *sym=Opt_stab()->Aux_stab_entry(aux_id);
      if(sym->Is_virtual() && sym->Aux_id_list()!=NULL)
          Get_aux_id_by_alias(aux_id, alist);        
    }
  }

  if(!alist->Is_Empty()) {
    AUX_ID_LIST_ITER alist_iter;
    AUX_ID_NODE *a_node;
    FOR_ALL_ELEM(a_node, alist_iter, Init(alist)) {
      AUX_ID aux_id=a_node->Aux_id();
      Add_aux_id_to_bs(aux_id, appear_set);
    }          
  }

  CXX_DELETE(alist, Pool());		  
  return;
}

void
LOCAL_CLSC::Collect_def(CODEREP *cr, IDX_32_SET *appear_set, IDX_32_SET *def_set)
{
  AUX_ID aux_id=cr->Aux_id();
  AUX_STAB_ENTRY *sym=Opt_stab()->Aux_stab_entry(aux_id);
  if(sym!=NULL && sym->St()!=NULL) {
    TY_IDX ty=ST_type(sym->St());
    if(TY_kind(ty)!=KIND_ARRAY && TY_kind(ty)!=KIND_STRUCT) {
      Add_aux_id_to_bs(aux_id, def_set);
      return;
    }
  }
  Add_aux_id_to_bs(aux_id, appear_set);
  return;
}

AUX_ID_LIST *
LOCAL_CLSC::Get_use_by_mu_node(MU_NODE *mnode)
{
  AUX_ID_LIST *alist=CXX_NEW(AUX_ID_LIST, Pool());
  alist->Clear();
 
  AUX_ID aux_id=mnode->Aux_id();
  alist->New_aux_id_node(aux_id, Pool());
  
  AUX_STAB_ENTRY *sym=Opt_stab()->Aux_stab_entry(aux_id);
  if(sym->Is_virtual()  && sym->Aux_id_list()!=NULL)
      Get_aux_id_by_alias(aux_id, alist);        

  return alist;
}


void
LOCAL_CLSC::Collect_use_by_mu_node(MU_NODE *mnode, IDX_32_SET *upwd_set, IDX_32_SET *appear_set, IDX_32_SET *def_set)
{
  AUX_ID_LIST *aux_id_list=Get_use_by_mu_node(mnode);
  if(!aux_id_list->Is_Empty()) {
    AUX_ID_LIST_ITER aux_id_list_iter;
    AUX_ID_NODE *aux_id_node;
    FOR_ALL_ELEM(aux_id_node, aux_id_list_iter, Init(aux_id_list)) {
      AUX_ID aux_id=aux_id_node->Aux_id();
      if(!Aux_id_in_bs(aux_id, def_set)) {
        Add_aux_id_to_bs(aux_id, upwd_set);
      } 
      else { 
        Add_aux_id_to_bs(aux_id, appear_set);
      }
    }
  }
  CXX_DELETE(aux_id_list, Pool());
  return;
}

void
LOCAL_CLSC::Collect_use_rec(CODEREP *cr, IDX_32_SET *upwd_set, IDX_32_SET *appear_set, IDX_32_SET *def_set)
{
  if(cr->Kind() == CK_VAR){
      AUX_ID aux_id=cr->Aux_id();
      if(!Aux_id_in_bs(aux_id, def_set))
        Add_aux_id_to_bs(aux_id, upwd_set);
      else
        Add_aux_id_to_bs(aux_id, appear_set);
  }
  else if(cr->Kind() == CK_IVAR){
    if(cr->Ilod_base())
      Collect_use_rec(cr->Ilod_base(), upwd_set, appear_set, def_set);

    MU_NODE *mnode=cr->Ivar_mu_node();    
    if(mnode!=NULL)
      Collect_use_by_mu_node(mnode, upwd_set, appear_set, def_set); 
  } else if(cr->Kind()==CK_OP) {
      //if  there is a call in parameter, the algorithm will fail, but in wopt, we assume this will not happen
      for (INT i=0; i< cr->Kid_count(); i++) {
        Collect_use_rec(cr->Opnd(i), upwd_set, appear_set, def_set);
      }
  }
  return;
}

  //initialize the bs, then collect local info for each bb:  
  // 1) must def, saved in _loc_def
  // 2) upward exposed use, saved in _loc_upwd
  // 3) all the other appears (non-upwd uses, may def, mu, chi...) will be saved in _loc_appear
void
LOCAL_CLSC::Collect_local_refs(CFG *cfg)
{
  CFG_ITER cfg_iter(cfg);
  BB_NODE *bb;
  FOR_ALL_NODE(bb, cfg_iter, Init()){
    bb->Set_loc_appear( CXX_NEW(IDX_32_SET(Bs_size(), Pool(), OPTS_FALSE), Pool()));
    bb->Set_loc_def( CXX_NEW(IDX_32_SET(Bs_size(), Pool(), OPTS_FALSE), Pool()));
    bb->Set_loc_upwd( CXX_NEW(IDX_32_SET(Bs_size(), Pool(), OPTS_FALSE), Pool()));
    bb->Set_live_out( CXX_NEW(IDX_32_SET(Bs_size(), Pool(), OPTS_FALSE), Pool()));
    bb->Set_live_at_exit( CXX_NEW(IDX_32_SET(Bs_size(), Pool(), OPTS_FALSE), Pool()));     //we use _live_at_exit to save live_in set for bb
	
    STMT_LIST *stmt_list = bb->Stmtlist();
    STMTREP_ITER  stmt_iter(stmt_list);
    STMTREP      *stmtrep;
    FOR_ALL_NODE(stmtrep, stmt_iter, Init()){

      //calculate upward exposed use and all the uses
      if(stmtrep->Rhs())
        Collect_use_rec(stmtrep->Rhs(), bb->Loc_upwd(), bb->Loc_appear(),bb->Loc_def()); 

      OPERATOR opr=stmtrep->Opr();
      if(opr==OPR_ISTORE || opr==OPR_ISTOREX || opr==OPR_STBITS || opr==OPR_MSTORE)
        Collect_use_rec(stmtrep->Lhs()->Istr_base(), bb->Loc_upwd(), bb->Loc_appear(), bb->Loc_def());

      if(stmtrep->Has_mu()) {
        MU_LIST *mu_list=stmtrep->Mu_list();
        if(mu_list!=NULL)  {
          MU_NODE *mnode;
          MU_LIST_ITER mu_iter;
          FOR_ALL_NODE(mnode, mu_iter, Init(mu_list))  {
            Collect_use_by_mu_node(mnode, bb->Loc_upwd(), bb->Loc_appear(), bb->Loc_def());
          }
        }
      }

      if(stmtrep->Opr()==OPR_STID) {
         Collect_def(stmtrep->Lhs(), bb->Loc_appear(), bb->Loc_def());
      }

      if(stmtrep->Has_chi()) {
        CHI_LIST *chi_list=stmtrep->Chi_list(); 
        //chi_list may be empty for STID,           
        FmtAssert((chi_list!=NULL||stmtrep->Opr()!=OPR_ISTORE), 
			("LOCAL_CLSC::Collect_local_refs: empty chi_list for istore!\n"));
        if(chi_list!=NULL) {
          Collect_def_by_chi_list(chi_list, bb->Loc_appear());
        }
      }
    }

    bb->Loc_appear()->UnionD(bb->Loc_def());
    bb->Loc_appear()->UnionD(bb->Loc_upwd());

    if(Tracing()) {
      fprintf(TFile, "dump local references for bb %d: \n", bb->Id());
      fprintf( TFile, "kill def: ");
      bb->Loc_def()->Print(TFile);
      fprintf( TFile, "\n" );
      fprintf( TFile, " upwd: ");
      bb->Loc_upwd()->Print(TFile);
      fprintf( TFile, "\n" );      
      fprintf( TFile, "appear: ");
      bb->Loc_appear()->Print(TFile);
      fprintf( TFile, "\n" );
    }
  }
  return;
}

  //calculate data flow equation, backward
  // 1) initialize:  _live_out(exit_bb) = empty
  // 2) propagation equation: _live_out(bb) = union of _live_in(succs)
  // 						_live_in(bb) = live_out(bb) - _loc_def(bb) + _loc_upwd(bb)  
  //						In the above equation, the order must be kept.
  // 3) live range calculation equation:
  //						liveness(var) = union of bbs in which var is in _live_in, _live_out,  _loc_appear
void
LOCAL_CLSC::Calculate_liveness(CFG *cfg)
{
  BOOL change=TRUE;
  IDX_32_SET save_lo_set(Bs_size(), Pool(), OPTS_FALSE);
  IDX_32_SET save_lae_set(Bs_size(), Pool(), OPTS_FALSE);
  
  while(change) {
    change=FALSE;

    BB_NODE *bb;
    POBB_ITER cfg_iter(cfg);
    FOR_ALL_ELEM(bb, cfg_iter, Init()) {
      save_lo_set.CopyD(bb->Live_out());
      save_lae_set.CopyD(bb->Live_at_exit());
   
      BB_NODE *succ;
      BB_LIST_ITER bb_succ_iter;
      FOR_ALL_ELEM( succ, bb_succ_iter, Init(bb->Succ()) )     
        bb->Live_out()->UnionD(succ->Live_at_exit());

      bb->Live_at_exit()->UnionD(bb->Live_out());
      bb->Live_at_exit()->DifferenceD(bb->Loc_def());
      bb->Live_at_exit()->UnionD(bb->Loc_upwd());

      if((!save_lo_set.EqualP( bb->Live_out() )) || (!save_lae_set.EqualP(bb->Live_at_exit())))
        change=TRUE;
    }
  }

  if(Tracing()) {
    fprintf(TFile, "dump liveness:\n");
    BB_NODE *bb;
    POBB_ITER cfg_iter(cfg);
    FOR_ALL_ELEM(bb, cfg_iter, Init()) {
      fprintf(TFile, "for bb %d: \n", bb->Id());
      fprintf( TFile, "live in: ");
      bb->Live_at_exit()->Print(TFile);
      fprintf( TFile, "\n" );     
      fprintf( TFile, "live out: ");
      bb->Live_out()->Print(TFile);
      fprintf( TFile, "\n" );     
      fprintf(TFile, "appear:");
      bb->Loc_appear()->Print(TFile);
      fprintf( TFile, "\n" );
    }
  }

  return;
}

  //calculate live ranges for each local variable
  // live_range(var) = union of bbs in which var was included in  _live_in(bb) or live_out(bb) or appear
void
LOCAL_CLSC::Get_lr(CFG *cfg)
{
  //create LOCAL CLSC candidate
  STBP_MAP::iterator mitr=_bp_map.begin();
  STBP_MAP::iterator mitrn=_bp_map.end();
  while(mitr!=mitrn) {
    ST* st=(*mitr).first;
    IDX_32 bp=(*mitr).second;
    Add_node(st,cfg);
    mitr++;
  }
  //calculate live range
  DFSBB_ITER cfg_iter(cfg);
  BB_NODE *bb;
  FOR_ALL_ELEM(bb, cfg_iter, Init()){  
    if (bb->Kind() != BB_ENTRY) {
      IDX_32_SET *bs=CXX_NEW(IDX_32_SET(Bs_size(), Pool(), OPTS_FALSE), Pool());

      bs->UnionD(bb->Loc_appear());
      bs->UnionD(bb->Live_at_exit());
      bs->UnionD(bb->Live_out());
 
      LCLSC_NTAB::iterator vitr=_clsc_ntab.begin();
      LCLSC_NTAB::iterator vitrn=_clsc_ntab.end();
      while(vitr!=vitrn) {
        LCLSC_NODE *node=*vitr;
        IDX_32 bp=Get_bitpos(node->Get_st());
        if(bs->MemberP(bp))
          node->Get_lr()->Union1D(bb);
        vitr++;
      }
    }
  }

  if(Tracing()) {
    fprintf(TFile, "dump live range:\n");
    fprintf(TFile, "before remove fake entry live range\n");
    LCLSC_NTAB::iterator vitr=_clsc_ntab.begin();
    LCLSC_NTAB::iterator vitrn=_clsc_ntab.end();
    while(vitr!=vitrn) {
      LCLSC_NODE *node=*vitr;
      node->Print(TFile);
      vitr++;
    }    
  }

  //shrink to remove the fake live range before the first appearance of st
  BOOL change=TRUE;
  IDX_32_SET save_appear_set(Bs_size(), Pool(), OPTS_FALSE);
  while(change) {
    change=FALSE;

    FOR_ALL_ELEM(bb, cfg_iter, Init()) {
      if(bb->Kind() == BB_ENTRY) {
        bb->Loc_appear()->ClearD();
      }

      save_appear_set.CopyD(bb->Loc_appear());

      BB_NODE *pred;
      BB_LIST_ITER bb_pred_iter;
      FOR_ALL_ELEM( pred, bb_pred_iter, Init(bb->Pred()) )
        bb->Loc_appear()->UnionD(pred->Loc_appear());

      if((!save_appear_set.EqualP( bb->Loc_appear() )))
        change=TRUE;
    }
  }

  FOR_ALL_ELEM(bb, cfg_iter, Init()){
      LCLSC_NTAB::iterator vitr=_clsc_ntab.begin();
      LCLSC_NTAB::iterator vitrn=_clsc_ntab.end();
      while(vitr!=vitrn) {
        LCLSC_NODE *node=*vitr;
        IDX_32 bp=Get_bitpos(node->Get_st());
        if(!bb->Loc_appear()->MemberP(bp))
          node->Get_lr()->Difference1D(bb);
        vitr++;
      }
  }
  


  //reset bs in bb node of cfg to NULL, they will be freed after mem pool pop

  FOR_ALL_ELEM(bb, cfg_iter, Init()){
    bb->Set_loc_def( NULL );
    bb->Set_loc_upwd( NULL );
    bb->Set_loc_appear(NULL);
    bb->Set_live_at_exit(NULL);
    bb->Set_live_out(NULL);
  }

  if(Tracing()) {
    fprintf(TFile, "after remove fake entry live range\n");
    LCLSC_NTAB::iterator vitr=_clsc_ntab.begin();
    LCLSC_NTAB::iterator vitrn=_clsc_ntab.end();
    while(vitr!=vitrn) {
      LCLSC_NODE *node=*vitr;
      node->Print(TFile);
      vitr++;
    }    
  }
  return;
}

BOOL 
LOCAL_CLSC::LR_overlapped(BB_NODE_SET *lr0, BB_NODE_SET *lr1, CFG *cfg)
{
  if(lr0==lr1)
    return TRUE;
  
  BB_NODE_SET *bs0=CXX_NEW(BB_NODE_SET(cfg->Total_bb_count(), cfg, Pool(), BBNS_EMPTY), Pool());
  BB_NODE_SET *bs1=CXX_NEW(BB_NODE_SET(cfg->Total_bb_count(), cfg, Pool(), BBNS_EMPTY), Pool());
  bs0->CopyD(lr0);
  bs1->CopyD(lr1);
  if(bs0->IntersectionD(bs1)->EmptyP())
    return FALSE;
  else 
    return TRUE;
}

void
LOCAL_CLSC::Update_cr_alias(POINTS_TO *pt0, POINTS_TO *pt1, CODEREP *cr, OPT_STAB *opt_stab)
{
  CODEKIND ck=cr->Kind();
  if(ck==CK_IVAR || ck==CK_VAR) {
    POINTS_TO *pt=cr->Points_to(opt_stab);
    if(pt!=NULL) {
      BOOL aliased=FALSE;
      if(opt_stab->Rule()->Aliased_Memop(pt,pt0)) {
        pt->Meet(pt1, NULL);
      }

      if(opt_stab->Rule()->Aliased_Memop(pt,pt1)) {
        pt->Meet(pt0, NULL);
      }
    }

    if(ck==CK_IVAR) 
      if(cr->Ilod_base())
        Update_cr_alias(pt0, pt1, cr->Ilod_base(), opt_stab);
    
  }else if(ck==CK_OP) {
      for (INT i=0; i< cr->Kid_count(); i++) 
        Update_cr_alias(pt0, pt1, cr->Opnd(i), opt_stab);
  }
  
  return;
}

void
LOCAL_CLSC::Update_alias(ST *st0, ST *st1, CFG *cfg, OPT_STAB *opt_stab)
{
  //update the points_to
  //the st_group need not to be updated since the alias info has been updated?
  POINTS_TO pt0, pt1;
  TY_IDX ty0 = ST_type (st0);
  pt0.Analyze_ST(st0, 0, TY_size(ty0), 0, 0, ty0, TRUE/*has equiv*/);
  pt0.Set_alias_class(PESSIMISTIC_AC_ID);
  
  TY_IDX ty1 = ST_type (st1);
  pt1.Analyze_ST(st1, 0, TY_size(ty1), 0, 0, ty1, TRUE/*has equiv*/);
  pt1.Set_alias_class(PESSIMISTIC_AC_ID);
  
  AUX_ID i;
  AUX_STAB_ITER aux_stab_iter(opt_stab);
  FOR_ALL_NODE(i, aux_stab_iter, Init()) {
  
    AUX_STAB_ENTRY *sym = opt_stab->Aux_stab_entry(i);
    ST *st=sym->St();
    if(st==st0) {
      POINTS_TO *pt=sym->Points_to();
      pt->Meet(&pt1, NULL);    
    }
    else if(st==st1) {
      POINTS_TO *pt=sym->Points_to();
      pt->Meet(&pt0, NULL);    
    }
  }

  //update the points_to
  CFG_ITER cfg_iter(cfg);
  BB_NODE *bb;
  FOR_ALL_NODE(bb, cfg_iter, Init()){
  
    STMT_LIST *stmt_list = bb->Stmtlist();
    STMTREP_ITER  stmt_iter(stmt_list);
    STMTREP      *stmtrep;
    FOR_ALL_NODE(stmtrep, stmt_iter, Init()){

      OPERATOR opr=stmtrep->Opr();
      if(opr==OPR_ISTORE || opr==OPR_ISTBITS || opr==OPR_ISTOREX || opr==OPR_MSTORE ) {
        Update_cr_alias(&pt0, &pt1, stmtrep->Lhs(), opt_stab);
      }
          
      if(stmtrep->Rhs()) {
        Update_cr_alias(&pt0, &pt1, stmtrep->Rhs(), opt_stab);
      }
          
      if(OPERATOR_is_store(opr))  {
        POINTS_TO *pt=stmtrep->Lhs()->Points_to(opt_stab);
        if(opt_stab->Rule()->Aliased_Memop(pt,&pt0))
          pt->Meet(&pt1, NULL);
        if(opt_stab->Rule()->Aliased_Memop(pt,&pt1))
          pt->Meet(&pt0, NULL);
      }
    }
  }

  return;
}

void
LOCAL_CLSC::Perform_clsc(CFG *cfg)
{

  typedef map<LCLSC_NODE*, BB_NODE_SET*> LR_MAP;
  typedef vector<LCLSC_NODE*> LN_CLASS;
  typedef map<LCLSC_NODE*, LN_CLASS*> LS_MAP;

  LR_MAP lr_map;  //the map from local variable to live range
  LS_MAP lc_map;  //the map from local variable to the set of variables sharing the same stack location

  //initialize lr_map and lc_map
  for(LCLSC_NTAB::iterator vitr0=_clsc_ntab.begin(); vitr0!=_clsc_ntab.end();vitr0++) {
    LCLSC_NODE *node=*vitr0;
    if(node->Get_lr()->EmptyP()) 
      continue;

    BB_NODE_SET *lcr=CXX_NEW(BB_NODE_SET(cfg->Total_bb_count(), cfg, Pool(), BBNS_EMPTY), Pool());
    lcr->CopyD(node->Get_lr());
    lr_map[node]=lcr;

    LN_CLASS *lnc=new LN_CLASS;
    lnc->push_back(node);
    lc_map[node]=lnc;
  }

  //perform coalescing  
  for(LCLSC_NTAB::iterator vitr0=_clsc_ntab.begin(); vitr0!=_clsc_ntab.end();vitr0++)  {
    LCLSC_NODE *node0=*vitr0;
    BB_NODE_SET *lr0=node0->Get_lr();
    if(lr0->EmptyP()) 
      continue;

    BB_NODE_SET *lcr0=lr_map[node0];

    for(LCLSC_NTAB::iterator vitr1=vitr0+1; vitr1!=_clsc_ntab.end();vitr1++) {
      LCLSC_NODE *node1=*vitr1;
      BB_NODE_SET *lr1=node1->Get_lr();
      if(lr1->EmptyP()) 
        continue;

      BB_NODE_SET *lcr1=lr_map[node1];

      if(!LR_overlapped(lcr0, lcr1,cfg)) {
        ST *st0=node0->Get_st();
        ST* st1=node1->Get_st();
        TY_IDX ty0=ST_type(st0);
        TY_IDX ty1=ST_type(st1);

        if(TY_size(ty0)==TY_size(ty1)) {
//        if(ty0==ty1) {

          if(Tracing()) {
            fprintf(TFile, "union %s and %s, size reduction: %lld\n", ST_name(st0),ST_name(st1), TY_size(ty0));
          }

          //create common st base block for st0 and st1
          St_Block_Union(st0, st1);

          //merge live range and update lr_map
          lcr0->UnionD(lcr1);
          lr_map[node1]=lcr0;

          //update alias info for all the symble related, first st0 and st1, then st0 class and st1 class
          Update_alias(st0, st1,Cfg(), Opt_stab());
          
          LN_CLASS *lnc0=lc_map[node0];
          LN_CLASS *lnc1=lc_map[node1];
          FmtAssert(lnc0!=lnc1, ("LOCAL_CLSC::Perform_clse, merged two node have the same class"));
          for(LN_CLASS::iterator lnitr0=lnc0->begin();lnitr0!=lnc0->end();lnitr0++) {
            for(LN_CLASS::iterator lnitr1=lnc1->begin();lnitr1!=lnc1->end();lnitr1++) {
              LCLSC_NODE *ln0=*lnitr0;
              LCLSC_NODE *ln1=*lnitr1;
              FmtAssert(ln0!=ln1, ("LOCAL_CLSC::Perform_clsc, merged two set have common member"));
              Update_alias(ln0->Get_st(), ln1->Get_st(), cfg, Opt_stab());
            }
          }

          //update ls_map
          lnc0->insert(lnc0->end(), lnc1->begin(), lnc1->end());
          lc_map[node1]=lnc0;
        }	
      }
    }
  }
  return;
}

void
LOCAL_CLSC:: Do_local_clsc()
{
  if(Tracing()) {
    fprintf(TFile, "%sBefore LOCAL CLSC\n%s", DBar, DBar);
    Cfg()->Print(TFile);
  }
  OPT_POOL_Initialize(Pool(), "LOCAL CLSC  pool", FALSE, LCLSC_TRACE_FLAG);
  OPT_POOL_Push( Pool(), LCLSC_TRACE_FLAG);
  //calculate the total bits needed for all the local variables and
  //initialize the _bp_map between aux_id of local variabls and bit position
  Init_bp_map(Opt_stab());

  //initialize the bs, then collect local info for each bb:  
  Collect_local_refs(Cfg());

  //calculate data flow equation, backward
  Calculate_liveness(Cfg());

  //calculate live ranges for each local variable
  Get_lr(Cfg());

  //union the basic st of two vars whose live ranges are not overlap and fix alias info
  Perform_clsc(Cfg());

  OPT_POOL_Pop( Pool(), LCLSC_TRACE_FLAG );
  OPT_POOL_Delete( Pool(), LCLSC_TRACE_FLAG);
  
  if(Tracing()) {
    fprintf(TFile, "%sAfter LOCAL CLSC\n%s", DBar, DBar);
    Cfg()->Print(TFile);
  }

  return;
}
