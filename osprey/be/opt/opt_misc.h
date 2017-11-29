/*
 * Copyright (C) 2009 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

//-*-c++-*-

// ====================================================================
// ====================================================================
//
// Copyright (C) 2007, University of Delaware, Hewlett-Packard Company,
// All Rights Reserved.
//
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
//
// ====================================================================
//
//  Description:
// ==============
//    Any prototype, definition that does not fall into any category 
// are clustered here 
//
//  o. Analyze_pu_attr: Reveal more __attribute__(()) semantic 
//       This function should be called after :
//         - the CFG is constucted properly all kind of memory 
//           disambiguation is done (so that the analysis can 
//           take advantage of these info)
//       and before:
//         - HSSA construction 
//
// ====================================================================
#ifndef opt_misc_INCLUDED
#define opt_misc_INCLUDED

#include "be_util.h"
#include "opt_alias_rule.h"
#include "opt_main.h"  // for COMP_UNIT
#include "opt_mu_chi.h"
#include "opt_htable.h"
#include "opt_sym.h"
#include "opt_cfg.h"

void Analyze_pu_attr (OPT_STAB* opt_stab, ST* pu_st);

class UselessStoreElimination
{
friend class CFG;

private:

    COMP_UNIT* _comp_unit;
    CFG* _cfg;
    OPT_STAB* _opt_stab;
    const ALIAS_RULE* _alias_rule;
    CODEMAP* _htable;

    typedef struct {
        CODEREP * def;
        BB_NODE * def_defbb;
        STMTREP * def_stmt;
        CODEREP * def_copy;
        BOOL def_inc;
        BOOL def_const;
        INT def_const_val;
        BB_NODE * clear_bb;
    } Candidate;

    enum Store_Type {
        Inc_Store = 1,
        Const_Store = 2,
        Other_Store =3
    };
    
    Candidate _candidate;
    INT _opt_count;
    BOOL _tracing;

    void Candidate_Clear()
    {
        _candidate.def = NULL;
        _candidate.def_stmt = NULL;
        _candidate.def_defbb = NULL;
        _candidate.def_copy = NULL;
        _candidate.def_inc = FALSE;
        _candidate.def_const = FALSE;
        _candidate.def_const_val = 0;
        _candidate.clear_bb = NULL;
    };

    void Traverse_Loops(BB_LOOP*);
    STMTREP* Is_Applicable_Innerloop(BB_LOOP*);
    BOOL Is_Freed_in_BB(BB_NODE*, CODEREP*);
    BOOL Aliased_with_CR(CODEREP*, POINTS_TO*);
    BOOL Aliased_with_base(CODEREP*, POINTS_TO*);
    BOOL Check_Uses_Defs_in_Loop(BB_LOOP* , STMTREP*, BB_NODE*, POINTS_TO*, BOOL);
    BOOL Check_Uses_Defs_Coderep(STMTREP*, CODEREP*, POINTS_TO*, BOOL);
    BOOL Check_First_Def_Coderep(STMTREP*, BB_NODE *, CODEREP*, POINTS_TO*, BOOL);
    BOOL Call_Can_be_Ignore(STMTREP*, POINTS_TO*);
    BOOL Is_Def_Candidate(CODEREP*);
    BOOL Same_as_first_def(CODEREP*);
    BOOL Have_Increment_Def(BB_LOOP*, BB_NODE*, POINTS_TO*);
    Store_Type Istore_Inc_Const_Other(STMTREP *, INT *);
    BOOL Perform_transform(BB_LOOP*);

public:
    UselessStoreElimination(COMP_UNIT*);
    ~UselessStoreElimination(void) {};
    void Perform_Useless_Store_Elimination();
};


#endif //opt_misc_INCLUDED
