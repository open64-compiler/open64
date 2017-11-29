/*
 * Copyright (C) 2008-2009 Advanced Micro Devices, Inc.  All Rights Reserved.
 */
// This program is free software; you can redistribute it and/or modify
// it under the terms of version 2 of the GNU General Public License as
// published by the Free Software Foundation.
//
// This program is distributed in the hope that it would be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//
// Further, this software is distributed without any warranty that it
// is free of the rightful claim of any third person regarding
// infringement  or the like.  Any license provided herein, whether
// implied or otherwise, applies only to this software file.  Patent
// licenses, if any, provided herein do not apply to combinations of
// this program with other software, or any other product whatsoever.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write the Free Software Foundation,
// Inc., 59 Temple Place - Suite 330, Boston MA 02111-1307, USA.

#ifndef opt_reasso_INCLUDED
#define opt_reasso_INCLUDED "opt_reasso.h"

#include "id_map.h"
#include "defs.h"
#include "opt_defs.h"
#include "glob.h"
#include "opt_bb.h"
#include "opt_cfg.h"
#include "opt_main.h"
#include "opt_sym.h"
#include "opt_wn.h"
#include "opt_base.h"
#include "config.h"
#include "ir_reader.h"

#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <map>
#include <set>
#include <vector>
#include <list>
#include <algorithm>
#include <cassert>
using namespace std;

namespace REASSO {
struct nary_exp;
struct wcexp;

WN* canon_sub_fn (WN *mytree);

enum nary_ir_qualifier {AWN, NARY};
struct nary_exp {
    nary_ir_qualifier narytyp;
    WN* awn;
    OPERATOR aop;
    OPCODE aopc;
    vector<nary_exp> anary;

    nary_exp (nary_ir_qualifier qual,
            OPERATOR op,
            OPCODE opc, 
            WN* wN, 
            vector<nary_exp>& anar) :
            narytyp(qual),
            aop(op),
            aopc(opc), 
            awn(wN), 
            anary(anar) {}

    nary_exp (nary_ir_qualifier qual,
            OPERATOR op,
            OPCODE opc) :
            narytyp(qual),
            aop(op),
            aopc(opc) {}

    nary_exp (const nary_exp& b) {
        narytyp = b.narytyp;
        awn = b.awn;
        aop = b.aop;
        aopc = b.aopc;
        anary = b.anary;
    }
};

bool WHIRL_has_term (WN* intre, AUX_ID astidx);
WN* WHIRL_get_term (WN* intre, AUX_ID astidx);
WN* fold_neg_WHIRL (WN* intre);
WN* WHIRL_of_nary_exp (const nary_exp& anary_exp);
nary_exp binary_to_nary (WN* atree);

struct term_struct {
    vector<nary_exp> termvec;
    int current_id;
};

enum wcexp_ir_qualifier {WTERM, WNODE};

struct wcexp;
struct wcexp {
    wcexp_ir_qualifier wcexptyp;
    OPERATOR aop;
    OPCODE aopc;
    int count;
    WN* wterm;
    vector<wcexp> wnode;
    wcexp (wcexp_ir_qualifier qual,
            OPERATOR op, 
            OPCODE opc,
            int coun) : 
            wcexptyp(qual),
            aop(op), 
            aopc(opc), 
            count(coun) {}
    wcexp (const wcexp& b) {
        wcexptyp = b.wcexptyp;
        wterm = b.wterm;
        aop = b.aop;
        aopc = b.aopc;
        count = b.count;
        wnode = b.wnode;
    }
};

WN* WHIRL_of_wcexp (const wcexp& wcx);

struct exp_struct {
    vector<nary_exp> term_based_explis;
    vector<wcexp> factorized_term_explis;
    vector<vector<wcexp> > subexpl;
    vector<vector<int> > costvecs;
    vector<nary_exp> selected_subexps;
    vector<int> selected_costs;
    int current_expr_id;
};

struct reasso_struct {
    exp_struct expressions;
    vector<nary_exp> canonicalized_input_trees;
    map<int, WN*> map_stm_wn;
    int cse_start_id;
    vector<nary_exp> excised_subexps;
    vector<int> excised_costs;
    vector<pair<AUX_ID, WN*> > excised_subexps_stids;
    vector<pair<AUX_ID, WN*> > excised_subexps_stids_comp;
    OPT_STAB * optstab;
    reasso_struct (int term_start, 
            vector<nary_exp>& canons, 
            map<int, WN*>& mystmnumwn, 
            OPT_STAB* optsta) 
        : cse_start_id (term_start),
        canonicalized_input_trees(canons), 
        map_stm_wn (mystmnumwn), 
        optstab (optsta) {}
};


nary_exp make_nary_term (WN* ald);
void make_nary_term (WN* ald, nary_exp& myret);
}

#endif /*opt_reasso_INCLUDED*/
