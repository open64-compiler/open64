/*
 * Copyright (C) 2010 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

/* Placeholder for license */

#include "tracing.h"
#include "glob.h"
#include "be_util.h"
#include "bb_node_set.h"
#include "opt_htable.h"
#include "opt_main.h"
#include "opt_ssa.h"

#include "opt_peel_unroll.h"
#include "opt_lmv.h"

#define opt_peel_unroll_helper_INCLUDED
    #include "opt_peel_unroll_helper.h"
#undef opt_peel_unroll_helper_INCLUDED

//////////////////////////////////////////////////////////////////////
//
//          Implementation of LOOP_CONSTRUCT 
//
//////////////////////////////////////////////////////////////////////
//
void
LOOP_CONSTRUCT::Init_all_flags (void) {
    _is_do_loop = 
    _is_in_do_loop_shape =
    _is_normalized = 
    _has_const_stride = 0;
}

LOOP_CONSTRUCT::LOOP_CONSTRUCT (CFG* cfg, BB_LOOP* loop) :
    _cfg(cfg), _loop(loop) {
    
    Init_all_flags ();

    _is_do_loop = _loop->Header()->Kind () == BB_DOHEAD;

    _stride = 0;
    _unique_exit_br = NULL;

    if (!_loop->Well_formed ())
        return;

    // determine the unique exit-br
    //
    if (!_loop->Exit_early ()) {
        if (loop->End ()) {
            _unique_exit_br = loop->End()->Branch_stmtrep ();
        }
    }

    // init other fields
    _iv_lower_bound = _iv_upper_bound = NULL;
    _upbound_adj = 0;

    _iv_def_phi = NULL;
    _iv_init_stmt = _iv_incr_stmt = NULL;
    _stride = 0;

    // See if the loop can be promoted into DO-loop. If yes, consider it 
    // as DO-loop.
    //
    Check_if_in_do_loop_shape ();
}

// Return the PHI that defines IV in loop header 
//
PHI_NODE*
LOOP_CONSTRUCT::Lookup_iv_phi (void) {
       
    if (_iv_def_phi)
        return _iv_def_phi;
    
    if (!_loop->Iv())
        return NULL;
        
    AUX_ID iv_id = _loop->Iv()->Aux_id();

    PHI_LIST_ITER iter;
    PHI_NODE* phi;
    FOR_ALL_ELEM (phi, iter, Init (_loop->Header()->Phi_list())) {
        if (phi->Aux_id () == iv_id)
            return _iv_def_phi = phi;
    }

    return NULL;
}

// Look up the *unique* statement that advance IV by stride.
//
STMTREP* 
LOOP_CONSTRUCT::Lookup_iv_incr_stmt (void) {
    
    if (_iv_incr_stmt)
        return _iv_incr_stmt;

    PHI_NODE* phi = Lookup_iv_phi ();
    if (phi == NULL)
        return NULL;

    CODEREP* opnd = phi->OPND (_loop->Loopback_pred_num ());
    if (!opnd || 
        opnd->Is_flag_set (CF_DEF_BY_PHI) ||
        opnd->Is_flag_set (CF_DEF_BY_CHI)) {
        // This should not happen! (FIXME)
        //
        return NULL;
    }

    // Make sure:
    //  - the RHS is in the form of "IV + stride", and 
    //  - it is the only definiton to IV
    //
    if (STMTREP* iv_inc_stmt = opnd->Defstmt ()) {
        CODEREP* rhs = iv_inc_stmt->Rhs (); 
        rhs = Strip_cvt (rhs); 
        
        if (rhs->Kind () != CK_OP) return NULL;
        if (rhs->Opr () != OPR_ADD && rhs->Opr() != OPR_SUB)
            return NULL; 
        
        CODEREP* iv_def_by_phi = phi->RESULT (); 

        CODEREP* opnd0 = Strip_cvt (rhs->Opnd(0));
        CODEREP* opnd1 = Strip_cvt (rhs->Opnd(1));
        if (opnd0 == iv_def_by_phi || opnd1 == iv_def_by_phi) {
            return _iv_incr_stmt = iv_inc_stmt;
        }
    }

    return NULL;
}

// Return the statement the IV init statement right before the loop is 
// is entered.
//
STMTREP* 
LOOP_CONSTRUCT::Lookup_iv_init_stmt (void) {

    if (_iv_init_stmt)
        return _iv_init_stmt;

    PHI_NODE* phi = Lookup_iv_phi ();
    if (phi == NULL)
        return NULL;

    CODEREP* opnd = phi->OPND (_loop->Preheader_pred_num ());
    if (!opnd || 
        opnd->Is_flag_set (CF_DEF_BY_PHI) ||
        opnd->Is_flag_set (CF_DEF_BY_CHI)) {
        // This should not happen! (FIXME)
        //
        return NULL;
    }

    return _iv_init_stmt = opnd->Defstmt ();
}

// Check to see if the given loop is DO-loop shape.
//
void
LOOP_CONSTRUCT::Check_if_in_do_loop_shape (void) {

    _is_in_do_loop_shape = 0;
    _is_normalized = 0;
    _stride = 0;
    _has_const_stride = 0;
    _iv_lower_bound = 
    _iv_upper_bound = NULL;
    _upbound_adj = 0;

    if (!Check_if_in_do_loop_shape_helper ()) {
        // invalidate relevant data members
        //
        _is_in_do_loop_shape = 0;
        _is_normalized = 0;
        _stride = 0;
        _has_const_stride = 0;
        _iv_lower_bound = 
        _iv_upper_bound = NULL;
        _upbound_adj = 0;
    }
}

// Helper function of Check_if_in_do_loop_shape()
BOOL
LOOP_CONSTRUCT::Check_if_in_do_loop_shape_helper (void) {

    if (!_loop->Well_formed() || _loop->Exit_early () || 
        !_loop->Iv() || !_unique_exit_br) {
        return FALSE;
    }

    BB_NODE* loop_header = _loop->Header();
    STMTREP* exit_br = _unique_exit_br;

    // step 1: check to see if loop header is almost empty except 
    //   for label and loop-brk-branch.
    //
    {
        STMTREP_ITER iter (loop_header->Stmtlist());
        STMTREP *stmt;
        FOR_ALL_NODE (stmt, iter, Init ()) {
            if (stmt->Op() == OPC_LABEL)
                continue;
            
            if (!_loop->Test_at_entry () || stmt != exit_br)
                return FALSE;
        }
    }

    // step 2: check to see if loop-brk-branch is conditional branch whose
    //   outcome is determined by comparision between IV and a loop-invar.
    //
    if (exit_br->Op () != OPC_TRUEBR && exit_br->Op() != OPC_FALSEBR)
        return FALSE;;

    CODEREP *cmp = exit_br->Rhs ();
    if (cmp->Kind() != CK_OP || !OPCODE_is_compare (cmp->Op())) 
        return FALSE;

    // check to see if the comparision is in the form of 
    // "IV {<,<=,>,>=} upbound".
    // TODO: handle the cases like "up-bound <cmp> IV".
    // 
    if (cmp->Opr () != OPR_LE && cmp->Opr () != OPR_LT) {
        return FALSE;
    }

    CODEREP* iv = Strip_cvt (cmp->Opnd (0));
    if (iv->Kind () != CK_VAR || iv->Aux_id () != _loop->Iv()->Aux_id ()) {
        return FALSE;
    }

    // step 3: get IV upper bound
    //
    CODEREP* upbound = _iv_upper_bound = Strip_cvt (cmp->Opnd (1));
    if (!_loop->Invariant_cr (upbound))
        return FALSE;
          
    // get 4: get IV lower bound
    STMTREP *init_stmt = Lookup_iv_init_stmt ();
    STMTREP *incr_stmt = Lookup_iv_incr_stmt ();

    if (!init_stmt || !incr_stmt)
        return FALSE;

    // step 4: get IV lower bound
    //
    _iv_lower_bound = Strip_cvt (init_stmt->Rhs ());

    // step 5: get stride
    //
    CODEREP* incr_expr = incr_stmt->Rhs ();
    if (incr_expr->Opr() == OPR_ADD) {
        CODEREP* stride = Strip_cvt (incr_expr->Opnd(1));
        if (stride->Kind () == CK_VAR && 
            stride->Aux_id() == _loop->Iv()->Aux_id()) {
            stride = Strip_cvt (incr_expr->Opnd(0));
        }

        if (stride->Kind () != CK_CONST)
            return FALSE;
        
        _has_const_stride = 1;
        _stride = stride->Const_val ();
        if (_stride <= 0) {
            // TODO: handle count-down case.
            return FALSE;
        }
    } else {
        // TODO: handle OPR_SUB.
        return FALSE;
    }

    // step 6: determine up-bound adjustment: 
    //   - cmp='<' && stride > 0, adjusted by 1 (stride)
    //   - cmp='<=' && stride > 0, adjusted by 0 (stride)
    //   - otherwise, not handled 
    //
    _upbound_adj = (cmp->Opr() == OPR_LT) ? 1 : 0;

    // step 7: see if this loop is normalized
    //
    if (_stride == 1 && _iv_lower_bound && 
        _iv_lower_bound->Kind() == CK_CONST && 
        _iv_lower_bound->Const_val () == 0) {
       _is_normalized = 1; 
    }

    _is_in_do_loop_shape = 1;

    return TRUE;
}


// Return true iff the nest is a simple nest or has <depth> level 
// simple nest. The concept of "Simple nest" steals from SNL in LNO.
//
BOOL
LOOP_CONSTRUCT::Is_simple_nesting (UINT32 depth) const {

    BB_LOOP* loop = _loop->Child ();
    for (INT32 i = 1; loop && i < (INT32)depth; i++) {
        if (loop->Next () != NULL) {
            return FALSE;
        } 
        loop = loop->Child ();
    }

    return TRUE;
}

BOOL
LOOP_CONSTRUCT::Check_if_clonable (void) {
    BOOL allow_clone_calls = _cfg->Allow_clone_calls ();
    BB_NODE_SET_ITER iter;
    BB_NODE* blk;
    FOR_ALL_ELEM (blk, iter, Init (_loop->True_body_set())) {
        if (!blk->Clonable (TRUE, NULL, allow_clone_calls))
            return FALSE;
    }
    return TRUE;
}

//////////////////////////////////////////////////////////////////////
//
//          Implementation of MULTIVER_PLUGIN
//
//////////////////////////////////////////////////////////////////////
//
const LPU_PLUGIN::PEEL_UNROLL_BENEFIT 
    LPU_PLUGIN::NO_BENEFIT = 0;

const LPU_PLUGIN::PEEL_UNROLL_BENEFIT 
    LPU_PLUGIN::MAX_BENEFIT = 100;

const INT32 MULTIVER_PLUGIN::_small_trip_count_limit = 3;

MULTIVER_PLUGIN::MULTIVER_PLUGIN 
    (PEEL_UNROLL_DRIVER_IMPL& dr, MEM_POOL* mp, BB_LOOP* loop):
    LPU_PLUGIN (dr, mp, loop) {

    _loop_header = NULL;
    _unrool_factor = 0;
    _inner_trip_count = 0;
    _outer_trip_count = 0;
    _allow_clone_call_bb_saved = Get_cfg()->Allow_clone_calls ();
    Get_cfg()->Set_allow_clone_calls (TRUE);
}

MULTIVER_PLUGIN::~MULTIVER_PLUGIN (void) {
    // restore original CFG settings
    //
    Get_cfg ()->Set_allow_clone_calls (_allow_clone_call_bb_saved);
}

inline BOOL
MULTIVER_PLUGIN::Is_scalar_ld (CODEREP* cr) const {
    return cr->Kind () == CK_VAR && !cr->Bit_field_valid ();
}

// Analyze <ld>'s definitions. Check to see if the definition of 
// given load match folowing idiom. If yes, upon return, fill in <def_pattern> 
// the content of def. i.e. the rhs of stmt1, else fill in <def_pattern> invalid 
// content (def_pattern.nonconst_addend = NULL).
//
//  x = expr +/- <const> // stmt1
//  ...
//  if (cond) x = 
//
//    = ..x.. (the ld fed to this func)
// 
void
MULTIVER_PLUGIN::Get_def_pattern 
    (CODEREP* ld, DEF_PATTERN& def_pattern) const {
    
    def_pattern.nonconst_addend = NULL;
    def_pattern.const_addend = 0;
    def_pattern.dom_def = NULL;
    def_pattern.cond_def = NULL;

    if (!ld->Is_flag_set (CF_DEF_BY_PHI))
        return;

    PHI_NODE* phi = ld->Defphi ();        

    // should have 2 operands
    //
    if (phi->Size () != 2) {
        return;
    }

    CODEREP* opnd1 = phi->OPND(0);
    if (opnd1->Is_flag_set (CF_DEF_BY_PHI) || 
        opnd1->Is_flag_set (CF_DEF_BY_CHI)) {
        return;
    }

    CODEREP* opnd2 = phi->OPND(1);
    if (opnd2->Is_flag_set (CF_DEF_BY_PHI) || 
        opnd2->Is_flag_set (CF_DEF_BY_CHI)) {
        return;
    }

    STMTREP* stmt1 = opnd1->Defstmt ();
    STMTREP* stmt2 = opnd2->Defstmt ();

    if (!stmt1 || !stmt2)
        return;

    BB_NODE* bb1 = stmt1->Bb ();
    BB_NODE* bb2 = stmt2->Bb ();
    
    if (bb2->Dominates (bb1)) {
        BB_NODE* b = bb2; bb2=bb1; bb1=b;    
        STMTREP* s = stmt2; stmt2=stmt1; stmt1=s;
    } else if (!bb1->Dominates (bb2)) {
        return;
    }
   
    if (stmt1->Opr () != OPR_STID)
        return;

    // ought to be in the form of "expr + <const>" or "expr - <const>"
    //
    CODEREP* rhs = stmt1->Rhs ();
    if (rhs->Kind () == CK_OP) {
        if (rhs->Opr () == OPR_ADD || rhs->Opr () == OPR_SUB) {
            CODEREP* nonconst_kid;
            CODEREP* const_kid; 

            if (rhs->Opnd(0)->Kind () == CK_CONST) {
                const_kid = rhs->Opnd(0);
                nonconst_kid = rhs->Opnd(1);
            } else if (rhs->Opnd(1)->Kind () == CK_CONST) {
                const_kid = rhs->Opnd(1);
                nonconst_kid = rhs->Opnd(0);
            } else
                return;

            if (Is_scalar_ld (nonconst_kid)) {
                // fill in the pattern
                //
                def_pattern.nonconst_addend = nonconst_kid;
                def_pattern.const_addend = const_kid->Const_val ();
                def_pattern.dom_def = stmt1;
                def_pattern.cond_def = stmt2;
            }
        }
    }
}

// See if upbound is an expr like x2-x1, which could be a small number but
// for x1/x2's conditional def.
//
// If it is so, fill in <def_pattern1> and <def_pattern2> for x1 and x2, 
// respectivey, and return TRUE.
//
// An example is shown bellow:
//
//    x1=expr-<small-const>; // stmt1
//    x2=expr+<small-const>; // stmt2
//    ...
//    x1 is conditionally modified
//    x2 is conditionally modified
//    DO iv=0,x2-x1
//     ...
//    ENDDO
//  
BOOL
MULTIVER_PLUGIN::Trip_count_could_be_small
    (LOOP_CONSTRUCT& loop, 
     DEF_PATTERN& def_pattern1, DEF_PATTERN& def_pattern2, INT32& tc) const {

    CODEREP* upper_bound = loop.Get_iv_upper_bound ();
    if (upper_bound->Kind() != CK_OP || 
        upper_bound->Opr () != OPR_SUB ||
        !MTYPE_is_integral (upper_bound->Dtyp())) {
        return FALSE;
    }

    CODEREP* x2 = upper_bound->Opnd(0);
    CODEREP* x1 = upper_bound->Opnd(1);

    // check to see if <opnd1> share the same pattern as <x2> in above snippet
    //
    Get_def_pattern (x1, def_pattern1);
    Get_def_pattern (x2, def_pattern2);

    if (def_pattern1.nonconst_addend == NULL ||
        def_pattern1.nonconst_addend != def_pattern2.nonconst_addend) {
        // if they don't have common non-const part, we are not able to 
        // compute their diff statically.
        //
        return FALSE;
    }
        
    tc = def_pattern2.const_addend - def_pattern1.const_addend;
    STMTREP* br = loop.Get_unique_exit_br ();
    if (br->Rhs()->Opr() == OPR_LE) { tc += 1; }

    if (tc > _small_trip_count_limit) {
        return FALSE;
    }

    return TRUE;
}

// helper function of Unrolling_will_bring_benefit().
//
static BOOL
Has_use (CODEREP* expr, CODEREP* var) {
    
    if (expr->Kind() == CK_OP) {
        Is_True (var->Kind () == CK_VAR, ("not var"));
        IDTYPE aux_id = var->Aux_id ();
        for (INT32 i = 0; i < expr->Kid_count(); i++) {
            CODEREP* kid = expr->Opnd (i);
            if (kid->Kind () == CK_VAR) {
                if (kid->Aux_id () == aux_id)
                    return TRUE;
            } else if (kid->Kind () == CK_OP) {
                if (Has_use (kid, var))
                    return TRUE;
            }
        }
    }
    return FALSE;
}

// Check to see if unrolling will bring some benefit.
//
BOOL
MULTIVER_PLUGIN::Unrolling_will_bring_benefit 
    (LOOP_CONSTRUCT& outer, LOOP_CONSTRUCT& inner) const {

    // Idiom 1: 
    //   if iv is used in branch, unrolling will remove some conditions.
    //  
    BB_LOOP* iloop =  inner.Get_loop ();
    BB_NODE* header = iloop->Header();
    BB_NODE* body = header->Next (); 
    
    if (STMTREP* br = body->Branch_stmtrep ()) {
        if (OPERATOR_has_label (br->Opr ()) && 
            (Has_use (br->Rhs (), iloop->Iv()) || 
             Has_use (br->Rhs(), outer.Get_loop()->Iv()))) {
            return TRUE;
        }
    }

    return FALSE; 
}

// return TRUE iff this opt is applicable to given loop, and is supposed 
// to gain some improvement.
//
BOOL
MULTIVER_PLUGIN::Exam_applicability (void) {

    _benefit = NO_BENEFIT;

    LOOP_CONSTRUCT nest (Get_cfg(), _loop);
    LOOP_CONSTRUCT& outer = nest;

    BOOL tr = Is_trace_on ();
    char tr_char = _driver.Get_trace_leading_char ();
    if (tr) {
        SRCPOS srcpos = _loop->Header ()->Linenum ();
        fprintf (TFile, "%cMulti-ver fully-unroller is examining this "
                        "loop (file#:%d line:%d) :\n",
                 tr_char,
                 SRCPOS_filenum (srcpos), 
                 SRCPOS_linenum (srcpos));
        outer.Print (TFile);
    }

    BOOL applicable = TRUE;
    const char* msg = NULL;

    // step 1: Check the outer loop first, it should be outmost, 
    //   normalized DO-loop
    // 
    if (!outer.Is_do_loop () && !outer.Is_in_do_loop_shape ()) {
        applicable = FALSE; msg = "outer isn't DO-loop";
    } else if (_loop->Parent () == NULL) {
        applicable = FALSE; msg = "outer must have nesting loop"; 
    } else if (!outer.Is_normalized ()) {
        applicable = FALSE; msg = "outer isn't normalized";  
    } else if (!outer.Check_if_clonable ()) {
        applicable = FALSE; msg = "outer isn't clonable";
    }

    if (!applicable) {
        if (tr) { fprintf (TFile, "%c%s\n", tr_char, msg); }
        return FALSE;
    }

    // step 2: check to see if it is a simply nested double-loop 
    //
    BB_LOOP* inner_loop = nest.Get_loop ()->Child ();
    if (!inner_loop || inner_loop->Child () || !nest.Is_simple_nesting(2)) {
        if (tr) { fprintf (TFile, "%cNot a simply nested loop\n", tr_char); }
        return FALSE;
    }

    // step 3: inner loop must be nomalized and clonable
    //
    LOOP_CONSTRUCT inner (Get_cfg(), inner_loop);

    if (!inner.Is_do_loop () && !inner.Is_in_do_loop_shape ()) {
        if (tr) { fprintf (TFile, "%cInnner isn't DO-loop", tr_char); }
        return FALSE;
    } else if (!inner.Is_normalized ()) {
        if (tr) { fprintf (TFile, "%cInnner isn't normalized\n", tr_char); }
        return FALSE;
    } else if (!inner.Check_if_clonable ()) {
        if (tr) { fprintf (TFile, "%cInnner loop isn't clonable\n", tr_char); }
        return FALSE;
    }

    // step 4: see if the trip-count could be a very small integer
    // 
    if (!Trip_count_could_be_small 
            (outer, _x1_def_pattern, _x2_def_pattern, _outer_trip_count) ||
        !Trip_count_could_be_small 
            (inner, _y1_def_pattern, _y2_def_pattern, _inner_trip_count)) {

        if (tr) {
            fprintf (TFile, "%ctrip-count may not be a small number\n", tr_char);
        }
        return FALSE;
    }

    // step 5: see if unrolling will bring some benefit.
    //
    if (!Unrolling_will_bring_benefit (outer, inner)) {
        if (tr) {
            fprintf (TFile, "%cUnrolling does not bring benefit\n", tr_char);
        }
        return FALSE;
    }

    if (tr) {
        fprintf (TFile, 
                 "%cApplicable! inner/outer fully-unroll factor = %d/%d \n",
                 tr_char, _inner_trip_count, _outer_trip_count);
    }
    
    _benefit = MAX_BENEFIT;
    return TRUE;
}


// Helper function of Gen_multi_ver_predicate().
// Return the store stamtent in the form the "lhs_var = rhs_val". 
//
// The version <ver> is actually immaterial in term of correctness (because 
// version structures have already been destroied after HSSA).
// However, using distinct version will make IR readable.
//
STMTREP*
MULTIVER_PLUGIN::Gen_stid_stmt 
    (AUX_ID lhs_var, UINT32 rhs_val, VER_ID ver) {
    
    COMP_UNIT* cu = _driver.Get_comp_unit ();
    AUX_STAB_ENTRY* var_aux_st = cu->Opt_stab()->Aux_stab_entry (lhs_var);
    CODEMAP* htab = cu->Htable();  

    // step 1: generate LHS 
    //
    MTYPE mtype = var_aux_st->Mtype ();
    CODEREP* lhs = htab->Add_def (lhs_var, ver, NULL, mtype, mtype, 
                                  var_aux_st->St_ofst(), 
                                  MTYPE_TO_TY_array[mtype], 
                                  0/*no field */, TRUE /* is store */);

    // step 2: generate RHS.
    //
    CODEREP* rhs = Alloc_stack_cr (0);
    rhs->Init_const (mtype, rhs_val);
    rhs = htab->Rehash (rhs);

    STMTREP* stmt = CXX_NEW (STMTREP, htab->Mem_pool ());
    stmt->Init (lhs, rhs, OPCODE_make_op(OPR_STID, MTYPE_V, lhs->Dsctyp ()));
    stmt->Set_chi_list (NULL);

    lhs->Set_defstmt (stmt);

    return stmt;
}

// Insert <stmt> right before <before>
//
void
MULTIVER_PLUGIN::Insert_before (STMTREP* stmt, STMTREP* before) {
    
    before->Bb()->Insert_stmtrep_before (stmt, before);
    stmt->Set_live_stmt ();
    stmt->Set_linenum (before->Linenum ());
}

// Helper function of Perform_transformantion(). This function is to generate 
// a multi-versioning predicate dynamically determining which loop-version is 
// going to kick in: orignal one or multi-versioned instance.
//
// It also insert some "mult_ver = {true|false}" stmts. Please check the 
// comment at the beginning of MULTIVER_PLUGIN declaration.
//
CODEREP*
MULTIVER_PLUGIN::Gen_multi_ver_predicate (void) {

    COMP_UNIT* cu = _driver.Get_comp_unit ();

    BOOL trace = Is_trace_on ();
    const char trace_leading_char = _driver.Get_trace_leading_char ();
    VER_ID ver_id = 0; // just to make IR readable

    // step 1: create a boolean PREG for variable "no_mult_ver"
    //
    AUX_ID aux_id = cu->Opt_stab()->Create_preg (MTYPE_U4, "no_multi_ver");
    if (trace) {
        fprintf (TFile, "%cCreate symbol with AUX_ID=%d\n", 
                 trace_leading_char, aux_id);
    }

    STMTREP* init_stmt = Gen_stid_stmt (aux_id, 0, ++ver_id);
    Insert_before (init_stmt, _x1_def_pattern.dom_def);
    if (trace) {
        fprintf (TFile, "%cInsert init statement in BB:%d\n", 
                 trace_leading_char, 
                 _x1_def_pattern.dom_def->Bb()->Id ());
    }

    // step 2: Add this aux-id to entry-chi. This is useful only when dead phi
    //   is accidently introduced. In this case, one of the opnd of the dead phi
    //   is defined by the entry-chi.
    //  
    Add_new_auxid_to_entry_chis (aux_id, cu->Cfg (), cu->Htable(), 
                                 cu->Opt_stab ());

    STMTREP* insert_points[] = {
        _x1_def_pattern.cond_def,
        _x2_def_pattern.cond_def,
        _y1_def_pattern.cond_def,
        _y2_def_pattern.cond_def
    };

    // step 3: insert reset statement into the block where <x1|x2|y1|y2> is 
    //   conditionaly defined
    //
    for (INT32 i = 0; 
         i < (INT32)(sizeof(insert_points)/sizeof(insert_points[0])); 
         i++) {
        STMTREP* reset_stmt = Gen_stid_stmt (aux_id, 1, ++ver_id);
        STMTREP* insert_point = insert_points[i];

        Insert_before (reset_stmt, insert_point);
        if (trace) {
            fprintf (TFile, "%cInsert reset statement in BB:%d\n", 
                     trace_leading_char, insert_point->Bb()->Id());
        }
    }

    return init_stmt->Lhs(); 
}

//  Please check the comment to MULTIVER_PLUGIN for original loop.
//  This function is to create s1, s2, s3 and s4 and it must be called after 
//  the loop is cloned.
//
//    bool multi_ver = true; *
//    if (x1<0) x1=0;
//    if (y1<0) y1=0;
//    if (x2>mapmaxx) {x2=mapmaxx; multi_ver=false; *}
//    if (y2>mapmaxy) {y2=mapmaxy; multi_ver=false; *}
//  
//    if (multi_ver) {
//       x1 = t1; * // s1
//       x2 = t2; * // s2
//       y1 = t3; * // s3
//       y4 = t4; * // s4
//       for (y=0; y<=2, y++)
//         for (x=0; x<=2; x++) 
//           ... body ...
//    } else {
//          ... original double-loop
//    }
//
void
MULTIVER_PLUGIN::Propagate_original_val (LMV_CFG_ADAPTOR& adaptor) {

    CFG* cfg = Get_cfg ();
    BB_LOOP* cloned_loop = adaptor.Cloned_loop ();
    
    // step 1: create an empty block and insert it right before the 
    //    preheader of cloned loop.
    //
    BB_NODE* preheader = cloned_loop->Preheader ();
    BB_NODE* cond_blk = preheader->Nth_pred(0);
    BB_NODE* new_blk;
    new_blk = cfg->Add_bb_to_edge (cond_blk, preheader);
    
    if (BB_IFINFO* ifinfo = cond_blk->Ifinfo ()) {
        if (ifinfo->Then () == preheader)
            ifinfo->Set_then (new_blk);
        else if (ifinfo->Else() == preheader)
            ifinfo->Set_else (new_blk);
    }

    STMTREP* def_stmt[] = { _x1_def_pattern.dom_def, 
                            _x2_def_pattern.dom_def,
                            _y1_def_pattern.dom_def,
                            _y2_def_pattern.dom_def };

    for (INT32 i = 0; i < (INT32)(sizeof(def_stmt)/sizeof(def_stmt[0])); i++) {

        STMTREP* def = def_stmt[i];
        MTYPE mtype = def->Rhs()->Dtyp ();

        // step 2: create a symbols
        //
        COMP_UNIT* cu = _driver.Get_comp_unit ();
        AUX_ID save_id = cu->Opt_stab()->Create_preg (mtype);
        AUX_STAB_ENTRY* var_aux_st = cu->Opt_stab()->Aux_stab_entry (save_id);
        CODEMAP* htab = cu->Htable();

        // step 2: create statement "save = <def>'s rhs".
        //
        CODEREP* save_cr = htab->Add_def (save_id, 1, NULL, mtype, mtype, 
                                          var_aux_st->St_ofst(), 
                                          MTYPE_TO_TY_array[mtype], 
                                          0 /* no field */,
                                          TRUE /* is store */);

        STMTREP* stmt = CXX_NEW (STMTREP, htab->Mem_pool ());
        stmt->Init (save_cr, def->Rhs(), 
                    OPCODE_make_op(OPR_STID, MTYPE_V, mtype));
        stmt->Set_chi_list (NULL);
        save_cr->Set_defstmt (stmt);

        // step 3: Insert the new stmt right before <def>
        //
        Insert_before (stmt, def);

        // step 4: generate "x1|x2|y1|y2 = save"
        //
        stmt = CXX_NEW (STMTREP, htab->Mem_pool ());
        stmt->Init (def->Lhs(), save_cr, 
                    OPCODE_make_op (OPR_STID, MTYPE_V, mtype));
        
        stmt->Set_live_stmt ();
        stmt->Set_linenum (def->Linenum ());

        new_blk->Prepend_stmtrep (stmt);
    }
}

void
MULTIVER_PLUGIN::Perform_transformantion (void) {
    
    if (Is_trace_on ()) {
        fprintf (TFile, "%cPerforming transformation ...\n",
                 _driver.Get_trace_leading_char ());
    }

    // step 1: do multi-versioning
    //
    CFG* cfg = _driver.Get_cfg ();
    CODEREP* predicate = Gen_multi_ver_predicate ();

    LMV_CFG_ADAPTOR adaptor (_mp, cfg, Is_trace_on (), _loop, predicate);

    _driver.Get_cfg()->LMV_clone_loop (&adaptor);
    Propagate_original_val (adaptor);

    // step 2: fully unroll inner loop
    //
    cfg->Invalidate_loops();
    cfg->Invalidate_and_update_aux_info (TRUE);
    cfg->Analyze_loops();

    BB_LOOP* cloned_loop = adaptor.Cloned_loop ();
    BB_LOOP* inner_loop = cloned_loop->Child ();
    {
        BOOL tr = Is_trace_on ();
        LOOP_UNROLL_UTIL inner_unroller (Get_comp_unit (), inner_loop, tr);
        inner_unroller.Fully_unroll (_inner_trip_count);
        Is_True (cloned_loop->Child () == NULL, ("inner loop is not destroied"));
    }

    // step 3: fully unroll outer loop
    //
    cfg->Invalidate_loops();
    cfg->Invalidate_and_update_aux_info (TRUE);
    cfg->Analyze_loops();
    {
        BOOL tr = Is_trace_on ();
        LOOP_UNROLL_UTIL outer_unroller (Get_comp_unit (), cloned_loop, tr);
        outer_unroller.Fully_unroll (_outer_trip_count);
    }

    // step 4: At this point, SSA form is in mess. Rebuild SSA by renaming.
    //
    cfg->Invalidate_loops();
    cfg->Invalidate_and_update_aux_info (TRUE);
    cfg->Analyze_loops();

    extern void Rename_CODEMAP(COMP_UNIT*);
    Rename_CODEMAP (_driver.Get_comp_unit ());

    // step 5: Mark the loops in the nest are processed.
    // 
    _driver.Set_processed (_loop); 
    _driver.Set_processed (inner_loop);
}

//////////////////////////////////////////////////////////////////////
//
//    Implementation of PEEL_UNROLL_DRIVER_IMPL
//
//////////////////////////////////////////////////////////////////////
//
const char PEEL_UNROLL_DRIVER_IMPL::_trace_leading_char = '>';

LOOP_PEEL_UNROLL_DRIVER::LOOP_PEEL_UNROLL_DRIVER 
    (COMP_UNIT* comp_unit, LPU_OPTS opts, LPU_TRACE_LEVEL tr)
    : _comp_unit(comp_unit), _tr_level(tr), _enable_opts(opts) {
}

PEEL_UNROLL_DRIVER_IMPL::PEEL_UNROLL_DRIVER_IMPL 
    (COMP_UNIT* cu, LPU_OPTS opts, LPU_TRACE_LEVEL tr):
        LOOP_PEEL_UNROLL_DRIVER (cu, opts, tr) {
    
    MEM_POOL_Initialize (&_mp, "loop peeling and unrolling", FALSE);
	MEM_POOL_Push (&_mp);
}

PEEL_UNROLL_DRIVER_IMPL::~PEEL_UNROLL_DRIVER_IMPL (void) {
	MEM_POOL_Pop (&_mp);
	MEM_POOL_Delete (&_mp);
}

// Process given *INDIVIDUAL* loop (i.e. not nest), but "Plug-in" may choose 
// to optimize the loop nest as a whole. In either case, any processed loop, 
// regardless being optimized or not, should be marked by calling 
// Set_processed().
//
// return true iff any change was made on the loop. 
//
BOOL
PEEL_UNROLL_DRIVER_IMPL::Process_loop (BB_LOOP* cur_loop) {

    // step 1: Go through all "plug-in"s on current loop nest, estimating 
    //   performance improvement. Pick up the winner which yield the best 
    //   performance. 
    //

    PEEL_UNROLL_DRIVER_IMPL* driver = static_cast<PEEL_UNROLL_DRIVER_IMPL*>(this);

    // bunch of plug-ins...
    //
    MULTIVER_PLUGIN do_loop_multi_ver(*driver, &_mp, cur_loop);
    DUMMY_PLUGIN dummy (*driver, &_mp, cur_loop);
    LPU_PLUGIN* best_opt = NULL; 

    BOOL change = FALSE;

    LPU_PLUGIN::PEEL_UNROLL_BENEFIT benefit;
    benefit = LPU_PLUGIN::NO_BENEFIT;

    if (Is_enabled (LPU_OPT_MV_FULLY_UNROLL)) {
        // NOTE: declaring Exam_....() and Get_estima...() virtual would 
        //   certinaly ease coding, but it would incur cost as these two 
        //   func are called frequently
        //
        if (do_loop_multi_ver.Exam_applicability ()) {
            LPU_PLUGIN::PEEL_UNROLL_BENEFIT t;
            t = do_loop_multi_ver.Get_estimated_benefit ();
            if (t > benefit) {
                benefit = t;
                best_opt = static_cast<LPU_PLUGIN*>(&do_loop_multi_ver);
            }
        }
    }

#if 0
    // This snippet has no real use. It is just to illustrate how to extend 
    // PEEL_UNROLL_DRIVER_IMPL  by adding a "plug-in".
    //
    if (dummy.Exam_applicability ()) {
        PEEL_UNROLL_BENEFIT t = dummy.Get_estimated_benefit ();
        if (t > benefit) {
            benefit = t;
            best_opt = static_cast<LPU_PLUGIN*>(&dummy);
        }
    }
#endif

    // step 2: if winner exist, perform transformation
    //
    if (best_opt) {
        change = TRUE;
        best_opt->Perform_transformantion ();
    }

    Set_processed (cur_loop);

    return change;
}

// Helper function of PEEL_UNROLL_DRIVER_IMPL::Perform_peeling_or_unroll(void).
// It walks the from outer to inner along the loop hiearchy. The order matters
// as some opt may choose to optimize a loop-nest instead of a individual loop.
// 
BOOL
PEEL_UNROLL_DRIVER_IMPL::Perform_peeling_or_unroll_rec (BB_LOOP* loop_nests) {
    
    Is_True (!is_processed (loop_nests), 
             ("loop nest has already been processed"));

    // Save the sibling loop nest. current <loop_nests> may be destroied 
    // after it is processed.
    //
    BB_LOOP* sibling = static_cast<BB_LOOP*> (loop_nests->Next ());
    BB_LOOP* inner = static_cast<BB_LOOP*> (loop_nests->Child ());


    // Step 1: process current individual loop, plug-in optimizer has freedom
    //    to optimize the nest as a whole.
    //
    BOOL change = Process_loop (loop_nests);

    // step 2: Walk this loop nest from outer loop to inner loop. 
    //   We don't like to walk the nest in opposite order, as some opt 
    //   need to process a nest as a whole. If this order it awkward to   
    //   some "plug-in"s, it is up to these "plug-in"s to walk from 
    //   inner to outer, marking processed loops by calling is_processed()
    //   accordingly. 
    //
    if (inner && !is_processed (inner)) {
        // HINT: inner loop may already been processed along with 
        //   current loop, depending on implementation of "plug-in"s.
        //
        if (Perform_peeling_or_unroll_rec (inner))
            change = TRUE;
    }
    
    // step 3: take care the rest sibling nests
    //
    if (sibling && Perform_peeling_or_unroll_rec (sibling))
        change = TRUE;

    return change;
}

BOOL
PEEL_UNROLL_DRIVER_IMPL::Perform_peeling_or_unroll (void) {

    BB_LOOP* loop_hierachy = Get_cfg()->Loops ();
    if (!loop_hierachy)
        return FALSE;

    const char* title = "loop peeling & unrolling";
    BOOL trace_on = (_tr_level != LOOP_PEEL_UNROLL_DRIVER::LPU_NONE); 

    if (trace_on) {
        fprintf (TFile , "\n%s    Before %s \n%s", DBar, title, DBar);
        Get_cfg()->Print (TFile); 
        fprintf (TFile, "%s\n", DBar);
    }

    BOOL change = Perform_peeling_or_unroll_rec (loop_hierachy);
    
    if (trace_on) {
        fprintf (TFile , "\n%s    After %s\n%s", DBar, title, DBar);
        Get_cfg()->Print (TFile); 
        fprintf (TFile, "%s\n", DBar);
    }

    if (change) {
        extern void Rename_CODEMAP (COMP_UNIT*);
        Rename_CODEMAP (Get_comp_unit ());
    }

    return change;
}

BOOL
LOOP_PEEL_UNROLL_DRIVER::Perform_peeling_or_unroll (void) {

    PEEL_UNROLL_DRIVER_IMPL driver (_comp_unit, _enable_opts, _tr_level);
    return driver.Perform_peeling_or_unroll ();
}

/////////////////////////////////////////////////////////////////
//
//          Implementation of LOOP_UNROLL_UTIL 
//
/////////////////////////////////////////////////////////////////
//
#ifdef Is_True_On
void
LOOP_UNROLL_UTIL::Verify_merge_blk (const BB_NODE* candidate) {

    BOOL is_empty = TRUE;

    if (candidate->Kind () != BB_GOTO) {
        is_empty = FALSE;
    } else {
        STMTREP_CONST_ITER iter (candidate->Stmtlist());
        const STMTREP *stmt;
        FOR_ALL_NODE (stmt, iter, Init ()) {
            OPERATOR opr = stmt->Opr ();
            switch (opr) {
            case OPR_LABEL:
            case OPR_GOTO:
            case OPR_PRAGMA:
                break;
            default:
                is_empty = TRUE;
            }
            if (is_empty) { break; }
        }
    }

    Is_True (is_empty && candidate->Pred()->Len () == 1, 
             ("BB %d is not merge block"));
}
#endif


LOOP_UNROLL_UTIL::LOOP_UNROLL_UTIL 
    (COMP_UNIT* comp_unit, BB_LOOP* loop, BOOL trace): 
    _cu(comp_unit), _loop(loop) {

    Is_True (loop->Well_formed (), ("loop is not well formed"));

    _factor = 0;

    _is_fully_unroll = 0;
    _unrolled = FALSE;
    _trace = trace;

    // Collect blocks of original loop, and put them in <_bbs_of_orig_loop>.
    // The only reason for doing so is that BB_NODE_SET is awkward.
    //
    {
        BB_NODE_SET_ITER iter;
        BB_NODE* blk;
        FOR_ALL_ELEM (blk, iter, Init (loop->True_body_set())) {
            _bbs_of_orig_loop.push_back (blk);
        }
    }

    // Determine source block of *UNIQUE* backedge. The "uniqueness" is guaranteed
    // by BB_LOOP::Well_formed().
    //
    {
        BB_NODE* pred;
        BB_LIST_ITER iter;
        FOR_ALL_ELEM (pred, iter, Init (_loop->Header ()->Pred ())) {
            if (::Is_backedge (pred, _loop->Header ())) {
                _backedge_src = pred;
                break;
            }
        }
    }
    Is_True (_backedge_src, ("no backedge"));

    // Collect all merge-blocks, i.e. those blocks that are not part of loop body,
    // but one of its pred is.
    //
    if (BB_NODE* merge = _loop->Merge ()) {
        // loop has unique merge block
        //
        Verify_merge_blk (merge);
        _merge_blks.push_back (merge);
        _bbs_of_orig_loop.push_back (merge);
    } else {
        for (BB_NODE_LIST::iterator iter = _bbs_of_orig_loop.begin (),
             iter_end = _bbs_of_orig_loop.end (); 
             iter != iter_end; iter++) {
            
            BB_NODE* succ;
            BB_LIST_ITER succ_iter;
            FOR_ALL_ELEM (succ, succ_iter, Init ((*iter)->Succ ())) {
                if (!_loop->True_body_set()->MemberP (succ)) {
                    Verify_merge_blk (succ);
                    _merge_blks.push_back (succ);
                    _bbs_of_orig_loop.push_back (succ);
                }
            }
        }
    }

    // NOTE: gcc 4.2.* will emit kinda "ambiguous != operator" complains when compiling 
    //  following assignment.
    //
    //   _all_blks = _bbs_of_orig_loop;
    // 
    // For a quick workaround, I replace this concise assgnment with unwieldy loop.
    // TODO: restore it back when following statements are removed from 
    //   opt_cfg_trans.h
    //
    // > #ifdef KEY // fix g++ 3.2 problems
    // > template <class C>
    // > bool operator!=(C x, C y) { return !(x == y); }
    //
    _all_blks.clear ();
    for (BB_NODE_LIST::const_iterator iter = _bbs_of_orig_loop.begin (),
         iter_e = _bbs_of_orig_loop.end (); iter != iter_e; iter++) {
        _all_blks.push_back (*iter);
    }
}

// Establish map between labels of original loop and <clone_idx>-th cloned 
// loop body.
//
inline void
LOOP_UNROLL_UTIL::Map_cloned_label (LAB_ID src, LAB_ID cloned, 
                  UNROLL_IDX clone_idx) {

    Verify_body_copy_idx (clone_idx);
    _lab_id_map_vect[clone_idx][src] = cloned;
}

inline LOOP_UNROLL_UTIL::LAB_ID
LOOP_UNROLL_UTIL::Get_cloned_label (LAB_ID src, UNROLL_IDX clone_idx) const {

    Verify_body_copy_idx (clone_idx);

    const LAB_ID_MAP_VECT& map_vect = _lab_id_map_vect;
    const LAB_ID_MAP& lab_id_map = map_vect[clone_idx];

    LAB_ID_MAP::const_iterator iter = lab_id_map.find (src);
    return iter != lab_id_map.end() ? (*iter).second : (LAB_ID)0;
}

// Establish map between BB_NODEs of original loop and <clone_idx>-th cloned 
// loop body.
//
inline void
LOOP_UNROLL_UTIL::Map_cloned_bb (const BB_NODE* src, const BB_NODE* cloned, 
        UNROLL_IDX clone_idx) {

    Verify_body_copy_idx (clone_idx);
    _bb_map_vect[clone_idx][src] = cloned;
}

inline BB_NODE*
LOOP_UNROLL_UTIL::Get_cloned_bb (const BB_NODE* src, UNROLL_IDX clone_idx) const {

    Verify_body_copy_idx (clone_idx);

    const BB_NODE_MAP& bb_node_map = _bb_map_vect[clone_idx];

    BB_NODE_MAP::const_iterator iter = bb_node_map.find (src);
    return (iter != bb_node_map.end ()) ? 
            const_cast<BB_NODE*>((*iter).second) : NULL;
}

BB_NODE*
LOOP_UNROLL_UTIL::Clone_block (const BB_NODE* src, UNROLL_IDX clone_idx) {

    CFG* cfg = Get_cfg ();

    BB_NODE* clone = cfg->Create_and_allocate_bb (src->Kind()); 
    cfg->Clone_bb (src->Id(), clone->Id(), FALSE); 

    clone->Set_loopdepth (src->Loopdepth());
    clone->Set_rid_id (src->Rid_id());
    clone->Set_flag (src->Flag());
    clone->Set_linenum (src->Linenum());

    // Blindly copy the frequency for now. Later on we will adjust freq of all 
    // blocks by calling Adjust_bb_frequncy() 
    //
    clone->Set_freq (src->Freq()); 

    // current DCE implementation would be unhappy if phi-list is NULL
    //
    clone->Set_phi_list (CXX_NEW(PHI_LIST(clone), cfg->Mem_pool()));

    Map_cloned_bb (src, clone, clone_idx);

    if (src->Labnam() != 0) {
        clone->Add_label (cfg);
        Map_cloned_label (src->Labnam(), clone->Labnam(), clone_idx);

        BB_NODE* t = const_cast<BB_NODE*>(src);
        if (t->Label_stmtrep ()) {
            clone->Add_label_stmtrep (cfg->Mem_pool ());
        }
    }
  
    _all_blks.push_back (clone);

    if (OPT_FEEDBACK* fb = cfg->Feedback ()) {
        fb->Add_node (clone->Id ());
    }

    return clone;
}

// Helper function of Clone_loop_body(). The input <cloned_bbs> is the set
// of blocks of cloned loop body. This function is to copy code layout 
// as much as possible from original loop.  In any event, the blocks of 
// cloned loop are linked in a single doubly-linked list (via prev/next fields
// of BB_NODE) whose header and tail is also returned via *<first> and *<last>
// respectively.
//
void
LOOP_UNROLL_UTIL::Copy_layout (const BB_NODE_LIST& cloned_bbs, 
                               UNROLL_IDX clone_idx, 
                               BB_NODE** first, BB_NODE** last) {

    Verify_body_copy_idx (clone_idx);

    // step 1: copy the prev/next link
    //
    for (BB_NODE_LIST::const_iterator iter = _bbs_of_orig_loop.begin (),
         iter_end = _bbs_of_orig_loop.end (); iter != iter_end; iter++) {

        BB_NODE* src = *iter;
        BB_NODE* cloned = Get_cloned_bb (src, clone_idx);

        BB_NODE* src_next = static_cast<BB_NODE*> (src->Next ());
        BB_NODE* cloned_next = Get_cloned_bb (src_next, clone_idx);
        
        if (cloned_next) {
            cloned->Set_next (cloned_next);
            cloned_next->Set_prev (cloned);
        }
    }

    // step 2: In any event, chain all cloned blocks in a single doubly-linked
    //   list. The only reason for doing so is to ease rest operations.
    //
    
    // first, figure out all blocks having no prev-field.
    BB_NODE_LIST no_prev, no_next; 
    for (BB_NODE_LIST::const_iterator iter = cloned_bbs.begin (), 
         iter_end = cloned_bbs.end (); iter != iter_end; iter++) {

        BB_NODE* bb = *iter;
        if (bb->Next () == NULL) no_next.push_back (bb); 
        if (bb->Prev () == NULL) no_prev.push_back (bb);
    }

    Is_True (no_prev.size () == no_next.size (), ("internal inconsistency"));

    BB_NODE* bb_first, *bb_last;
    bb_first = bb_last = no_prev.front ();
    
    while (bb_last->Next()) { bb_last = static_cast<BB_NODE*>(bb_last->Next ()); }
    no_prev.pop_front (); 
    
    while (!no_prev.empty ()) {
        BB_NODE* new_head = no_prev.front ();
        no_prev.pop_front ();
        
        // splice two lists into one
        //
        bb_last->Set_next (new_head);
        new_head->Set_prev (bb_last);

        while (bb_last->Next()) {
            bb_last = static_cast<BB_NODE*>(bb_last->Next ()); 
        }
    }

    *first = bb_first;
    *last = bb_last;
    
    Is_True (bb_first->Prev() == NULL && bb_last->Next() == NULL, 
             ("internal inconsistency"));

    if (_trace) {
        fprintf (TFile, "The prev/next link is (first %d, last %d): ", 
                        bb_first->Id(), bb_last->Id ());
        for (BB_NODE* t = bb_first; t != NULL; t = t->Next()) {
            fprintf (TFile, "%d,", t->Id());
        }
        fprintf (TFile, "\n");
    }
}

// Clone all edges emanating from loop body blocks except backedge.
//
void
LOOP_UNROLL_UTIL::Clone_edges (const BB_NODE_LIST& cloned_bbs, 
                               UNROLL_IDX clone_idx) {

    // To ensure the frequency of cloned edges has 1/_factor of the 
    // corresponding edges of original loop.
    //
    float freq_scale = 1.0f / (_factor - clone_idx);

    CFG* cfg = Get_cfg ();

    for (BB_NODE_LIST::const_iterator iter = _bbs_of_orig_loop.begin (),
         iter_end = _bbs_of_orig_loop.end ();
         iter != iter_end; 
         iter++) {

        BB_NODE* blk = *iter;   
        BB_NODE* succ;
        BB_LIST_ITER succ_iter;
        FOR_ALL_ELEM (succ, succ_iter, Init (blk->Succ ())) {

            if (::Is_backedge (blk, succ))
                continue;

            BB_NODE* cloned_blk = Get_cloned_bb (blk, clone_idx);
            BB_NODE* cloned_succ = Get_cloned_bb (succ, clone_idx);
            if (cloned_succ == NULL) {

                // <blk> is merge-block which is actually not part of loop
                // body. We put them in <_bbs_of_orig_loop> in order to handle
                // merge-block uniformaly with other body blocks. However, 
                // the edges emanating from merge-blocks should be handled 
                // specially.
                //
                // Suppose original merge-block is <m>, and its unique target
                // is <t>, and cloned merge-block, is <c>, we need to connect
                // <t> and <c>.
                //
                Is_True (find (_merge_blks.begin (), _merge_blks.end(), blk) !=
                         _merge_blks.end (),
                         ("BB %d should be a merge block", blk->Id ()));

                cloned_succ = succ;
                
                // If <succ> has multiple predecessor, make sure it has a 
                // PHI_LIST -- downstream optimizers take for granted that 
                // a joint node should has pre-allocated phi-list.
                //
                if (succ->Phi_list () == NULL) {
                    PHI_LIST* t = CXX_NEW (PHI_LIST(succ), cfg->Mem_pool());
                    succ->Set_phi_list (t);
                }
            }

            cfg->Connect_predsucc (cloned_blk, cloned_succ);

            // maintain edge frequency
            //
            if (OPT_FEEDBACK* fb = cfg->Feedback ()) {
                fb->Clone_edge (blk->Id(), succ->Id(), 
                                cloned_blk->Id(), cloned_succ->Id(), 
                                freq_scale);
            }
        }
    }

    // Merge blocks need special handling. Suppose <m> is one of the orignial 
    // merge block, and <m>'s unique succ is <n>. <m> may not have goto-
    // statement, as it may fall-through to <n>. However, we alreay append a 
    // goto statement to <m>'s clone. So, we need to update the branch lable
    // of <m>'s clone.
    //
    for (BB_NODE_LIST::iterator iter = _merge_blks.begin ();
         iter != _merge_blks.end (); iter++) {

        BB_NODE* merge = *iter; 
        BB_NODE* succ = merge->Nth_succ (0);

        Prepend_label_stmt (succ);
        STMTREP* goto_stmt = Get_cloned_bb (merge, clone_idx)->Branch_stmtrep ();
        if (goto_stmt && OPERATOR_has_label (goto_stmt->Opr ())) {
            goto_stmt->Set_label_number (succ->Labnam ());
        }
    }
}

void
LOOP_UNROLL_UTIL::Clone_BB_IFINFO (UNROLL_IDX clone_idx) {

    for (BB_NODE_LIST::iterator iter = _bbs_of_orig_loop.begin (), 
         iter_end = _bbs_of_orig_loop.end ();
         iter != iter_end; 
         iter++) {

        BB_NODE* orig = *iter;
        if (BB_IFINFO* ifinfo = orig->Ifinfo ()) {
            BB_NODE* cond_blk = Get_cloned_bb (ifinfo->Cond(), clone_idx);
            BB_NODE* then_blk = Get_cloned_bb (ifinfo->Then(), clone_idx);
            BB_NODE* else_blk = Get_cloned_bb (ifinfo->Else(), clone_idx);
            BB_NODE* merge_blk = Get_cloned_bb (ifinfo->Merge(), clone_idx);

            BB_IFINFO* cloned_ifinfo = 
                CXX_NEW (BB_IFINFO(ifinfo->Thenloc(),
                                   ifinfo->Elseloc(),
                                   cond_blk,
                                   then_blk, 
                                   else_blk,
                                   merge_blk), Get_cfg()->Mem_pool());
            Get_cloned_bb (orig, clone_idx)->Set_ifinfo (cloned_ifinfo);
        }
    }
}

// This function is to update target of cloned branch statement. 
// The branch statements were earlier copied verbatim from original branches.
// After label-mapping is completely established, their targets need to be 
// updated to point to cloned blocks.
//
void
LOOP_UNROLL_UTIL::Update_branch_target (UNROLL_IDX clone_idx) {

    for (BB_NODE_LIST::iterator iter = _bbs_of_orig_loop.begin (), 
         iter_end = _bbs_of_orig_loop.end ();
         iter != iter_end; 
         iter++) {
        
        BB_NODE* orig_blk = *iter;
        STMTREP* orig_br_stmt = orig_blk->Branch_stmtrep ();
        if (!orig_br_stmt || !OPERATOR_has_label (orig_br_stmt->Opr ()))
            continue;

        BB_NODE* cloned_blk = Get_cloned_bb (orig_blk, clone_idx);
        STMTREP* cloned_br_stmt = cloned_blk->Branch_stmtrep ();
        
        LAB_ID orig_lab_id = orig_br_stmt->Label_number ();
        LAB_ID new_lab_id = Get_cloned_label (orig_lab_id, clone_idx); 
        Is_True (new_lab_id != 0, ("Label-%d is not mapped", orig_lab_id));

        cloned_br_stmt->Set_label_number (new_lab_id);
    }
}

STMTREP*
LOOP_UNROLL_UTIL::Append_goto_stmt (BB_NODE* bb) {

    Is_True (bb->Kind () == BB_GOTO, ("block is not of kind BB_GOTO")); 

    STMTREP* br = bb->Branch_stmtrep ();
    if (!br) {
        br = CXX_NEW (STMTREP (OPC_GOTO), Get_cfg()->Mem_pool());
        bb->Append_stmtrep (br);
    } else {
        Is_True (OPERATOR_has_label (br->Opr()), ("BB is ended by call"));
    }

    return br;
}

STMTREP*
LOOP_UNROLL_UTIL::Prepend_label_stmt (BB_NODE* bb) {
    if (STMTREP* lab = bb->Label_stmtrep ()) {
        return lab;    
    }

    Alloc_lab (bb);
    bb->Add_label_stmtrep (Get_cfg()->Mem_pool ());

    return bb->Label_stmtrep ();
}

LOOP_UNROLL_UTIL::LAB_ID
LOOP_UNROLL_UTIL::Alloc_lab (BB_NODE* bb) {
    LAB_ID lab_id = bb->Labnam ();
    if (lab_id == 0) {
        bb->Set_labnam (lab_id = Get_cfg()->Alloc_label ());
    }
    return lab_id;
}

// Make <clone_idx>-th cloning of original loop body.
//
void
LOOP_UNROLL_UTIL::Clone_loop_body (UNROLL_IDX clone_idx) {

    if (_trace) {
        fprintf (TFile, "Begin %d-th unrolling\n", clone_idx+1);
        fprintf (TFile, "Src/cloned block mapping: ");
    }

    std::list<BB_NODE*> cloned_bbs;

    // step 1: Copy individual block of original loop body
    //
    for (BB_NODE_LIST::iterator iter = _bbs_of_orig_loop.begin (),
         iter_end = _bbs_of_orig_loop.end ();
         iter != iter_end; iter++) {

        BB_NODE* src = *iter;
        BB_NODE* clone = Clone_block (src, clone_idx);
        cloned_bbs.push_back (clone);
        
        if (_trace) {
            fprintf (TFile, "%d->%d, ", src->Id(), clone->Id());
        }
    }
    if (_trace) { fprintf (TFile, "\n"); }

    // step 2: Insert label and goto statement in cloned "merge" blocks.
    //    This is just to workaround WN-emitter problem -- it may not emit 
    //    an "empty" block even if the the block has label.
    //
    for (BB_NODE_LIST::iterator iter = _merge_blks.begin ();
         iter != _merge_blks.end (); iter++) {

        BB_NODE* clone = Get_cloned_bb (*iter, clone_idx);
        Append_goto_stmt (clone);
        Prepend_label_stmt (clone);
    }

    // Likewise, we need to insert label stmt to eschew WN-emitter's stupidity.
    //
    {
        BB_NODE* clone = Get_cloned_bb (_loop->Header (), clone_idx);
        Prepend_label_stmt (clone);
    }

    // step 3: Try to copy block layout as well.
    //
    BB_NODE* first_bb, *last_bb;
    Copy_layout (cloned_bbs, clone_idx, &first_bb, &last_bb);
    _bb_nodes_pair_vect[clone_idx] = std::make_pair(first_bb, last_bb);

    // step 4: clone all edges except backedges
    //
    Clone_edges (cloned_bbs, clone_idx);

    // step 5: update target of cloned branch statements
    //
    Update_branch_target (clone_idx);

    // step 6: update BB_IFINFO (it does not depend on previous two steps)
    //
    Clone_BB_IFINFO (clone_idx);
}

inline void
LOOP_UNROLL_UTIL::Connect_predsucc 
    (BB_NODE* pred, BB_NODE* succ, BOOL change_pred_br_target) {

    Get_cfg()->Connect_predsucc (pred, succ); 
    if (change_pred_br_target) {
        STMTREP* br = pred->Branch_stmtrep();
        Is_True (br && OPERATOR_has_label (br->Opr()), ("No branch"));

        br->Set_label_number (Alloc_lab (succ));
    }
}

// This function is to connnect adjacent unrolled bodies, create new 
// backedge, and do other misc things.
//
void
LOOP_UNROLL_UTIL::Hookup_cloned_bodies (void) {
    
    CFG* cfg = Get_cfg ();
    OPT_FEEDBACK* fb = cfg->Feedback ();

#ifdef Is_True_On
    // This should be guaranted by Split_backedge().
    //
    STMTREP* br = _backedge_src->Branch_stmtrep ();
    Is_True (br && OPERATOR_has_label(br->Opr()) &&
             (br->Label_number () == _loop->Header()->Labnam ()),
             ("loopback's target is not loop header"));
#endif

    // step 1: Remove backedge of original loop to prevent it from being cloned.
    //
    cfg->DisConnect_predsucc (Get_backedge_src (), _loop->Header ());
    
    // step 2: Redirect backedge. Suppose original loop's backedge is "(src, dest)". 
    // This step is to connect "clone(dest, <i>-th)" with "clone(src, <i+1>-th)", 
    // where "cloned(bb, idx)" is the cloned block of original block bb in 
    //  <idx>-th unrolled body.
    //
    //  We also link the prev/next of STMTREP.
    //
    BB_NODE* backedge_src = Get_backedge_src ();
    BB_NODE* backedge_sink = Get_cloned_bb (_loop->Header(), 0); 
    Connect_predsucc (backedge_src, backedge_sink, TRUE);
    if (fb) {
        fb->Move_edge_dest (backedge_src->Id(), _loop->Header()->Id(), 
                            backedge_sink->Id());
    }

    if (backedge_src->Ifinfo ())
        backedge_src->Set_ifinfo (NULL);

    for (UNROLL_IDX i = 1; i < _factor - 1; i++) {
        BB_NODE* cloned_header = Get_cloned_bb (_loop->Header(), i);
        BB_NODE* cloned_be_src = Get_cloned_bb (Get_backedge_src (), i - 1);

        Connect_predsucc (cloned_be_src, cloned_header, TRUE);
        if (fb) {
            float scale = 1.0f / (_factor + i - 1);
            fb->Clone_edge (backedge_src->Id(), backedge_sink->Id(), 
                            cloned_be_src->Id(), cloned_header->Id(), 
                            scale);
        }

        // Splice STMTREP lists of adjacent unrolled loop bodies into 
        // one list.
        //
        BB_NODE_PAIR prev_pair = _bb_nodes_pair_vect[i-1];
        BB_NODE_PAIR this_pair = _bb_nodes_pair_vect[i];
        prev_pair.second->Set_next (this_pair.first);
        this_pair.first->Set_prev (prev_pair.second);
    }

    // step 3: Deal with the last unrolled copy. 
    //  Suppose original loop looks like following:
    //
    //      bb_1: if (!condition) jump to bb_100 (out of loop)
    //      ...other blocks of loop body...
    //      bb_10 goto bb_1
    //
    //   If it is not fully unrolling, the clone-of-bb_10-of-last-unrolled-body 
    //  will hook to bb1 as a new backedge; otherwise, the 
    //  clone-of-bb_10-of-last-unrolled-body will connect to bb_100.
    //
    BB_NODE* backedge_src_clone = Get_cloned_bb (Get_backedge_src (), _factor-2);
    if (!_is_fully_unroll) {
        // generate new backedge 
        //
        Connect_predsucc (backedge_src_clone, _loop->Header (), TRUE);
        if (fb) {
            fb->Clone_edge (backedge_src->Id(), backedge_sink->Id(), 
                            backedge_src_clone->Id(), 
                            _loop->Header()->Id(), 0.5f);
        }
    } else {
        BB_NODE* loop_end = _loop->End ();
        Is_True (loop_end->Succ()->Len () == 2, 
                 ("supposed to be ended by a cond-br"));

        BB_NODE* succ = loop_end->Nth_succ (1);
        if (_loop->True_body_set()->MemberP (succ))
            succ = loop_end->Nth_succ (0);
        
        // <succ> must be merge block
        Is_True (find (_merge_blks.begin (), _merge_blks.end(), succ) !=
                 _merge_blks.end (),
                 ("BB%d must be a merge block", succ->Id()));
        Is_True (succ->Succ()->Len() == 1, ("merge block has multiple succ"));
        succ = succ->Nth_succ (0);
        if (succ->Phi_list () == NULL) {
            succ->Set_phi_list (CXX_NEW(PHI_LIST(succ), cfg->Mem_pool()));
        }
        Connect_predsucc (backedge_src_clone, succ, TRUE);
        if (fb) {
            fb->Clone_edge (backedge_src->Id(), backedge_sink->Id(), 
                            backedge_src_clone->Id(), succ->Id(), 0.5f);
        }
    }

    // step 4: Wedge the STMTREP list of unrolled loop body into 
    //   existing STMTREP list of current PU.
    //
    
    // looking for a good place to wedge in ...
    BB_NODE* point = _loop->Header (); 
    for (; _loop->True_body_set()->MemberP (point->Next ()); 
        point = point->Next ()) {
    }
    
    BB_NODE* first_blk = _bb_nodes_pair_vect[0].first;
    BB_NODE* last_blk = _bb_nodes_pair_vect[_factor-2].second;

    last_blk->Set_next (point->Next ());
    if (point->Next ()) {
        point->Next ()->Set_prev (last_blk);
    }
    point->Set_next (first_blk);
    first_blk->Set_prev (point);
}

void
LOOP_UNROLL_UTIL::Remove_flags_about_loop (BB_NODE* blk) {

    BOOL change = FALSE;

    switch (blk->Kind ()) {
    case BB_GOTO:
    case BB_LOGIF:
    case BB_VARGOTO:
    case BB_IO:
        break;

    case BB_ENTRY:
    case BB_EXIT:
    case BB_REGIONSTART:
    case BB_REGIONEXIT:
    case BB_SUMMARY:
        Is_True (FALSE, ("should not come across such blocks"));

    case BB_DOSTART:
    case BB_DOEND:
    case BB_DOSTEP:
    case BB_DOHEAD:
    case BB_DOTAIL:
    case BB_REPEATBODY:
    case BB_REPEATEND:
    case BB_WHILEEND:
        change = TRUE;
        break;

    default:
        Is_True (FALSE, ("unknown bb kind"));
    }

    if (change) {
        INT32 succ_num = blk->Succ()->Len ();
        if (succ_num == 1)
            blk->Set_kind (BB_GOTO);
        else if (succ_num == 2)
            blk->Set_kind (BB_LOGIF);
        else {
            Is_True (FALSE, ("Something must be wrong"));
        }
    }
}

// Helper func of fully-unrolling. As its name suggest it funtion is to remove
// the loop construct after fully-unrolling.
//
void
LOOP_UNROLL_UTIL::Destroy_loop_construct (void) {
    
    for (BB_NODE_LIST::iterator iter = _all_blks.begin (), 
         iter_end = _all_blks.end (); iter != iter_end; iter++) {
        
        BB_NODE* blk = *iter;

        // step 1: remove loop-related flags associated with the BB_LOOP
        //
        Remove_flags_about_loop (blk);

        // step 2: adjust the BB_LOOP associated with some blocks of 
        //   fully-unrolled loop to reflect their current nesting status.
        //
        BB_LOOP* enclosing_Loop = blk->Loop ();

        if (!enclosing_Loop)
            continue;

        if (enclosing_Loop == _loop) {
            blk->Set_loop (enclosing_Loop->Parent ());
        }

        blk->Set_loopdepth (blk->Loopdepth() - 1);
    }

    // step 3: Remove the BB_LOOP from BB_LOOP hierarchy
    //
    Get_cfg()->Remove_loop_construct (_loop);
    _loop = NULL; // avoid dangling pointer
}

// Change blocks' freq to be 1/unroll-factor of original frequency
//
void
LOOP_UNROLL_UTIL::Adjust_bb_frequncy (void) {

    for (BB_NODE_LIST::iterator iter = _all_blks.begin (), 
         iter_end = _all_blks.end (); iter != iter_end; iter++) {

        BB_NODE* blk = *iter;
        blk->Set_freq (blk->Freq () / _factor);
    }
}

#ifdef Is_True_On
void
LOOP_UNROLL_UTIL::Verify (void) {

    for (BB_NODE_LIST::const_iterator iter = _all_blks.begin (),
         iter_end = _all_blks.end (); iter != iter_end; iter++) {
        
        BB_NODE* bb = *iter;
        if (bb->Kind() == BB_VARGOTO)
            continue;

        // make sure branch-target lable is non-zero
        //
        STMTREP* br = bb->Branch_stmtrep ();
        if (br && OPERATOR_has_label (br->Opr ())) {
            INT32 lab_id = br->Label_number ();
            Is_True (lab_id != 0, 
                     ("Invalid target label of branch in BB%d", bb->Id ()));
            
            // make sure it is consistent with the lab-id of targeting blk
            //
            BOOL match_succ_lab = FALSE;
            if (bb->Nth_succ(0)->Labnam() == lab_id)
                match_succ_lab = TRUE;
            else if (bb->Succ()->Len() == 2 && 
                     bb->Nth_succ(1)->Labnam() == lab_id) {
                match_succ_lab = TRUE;
            }
            Is_True (match_succ_lab, 
                     ("BB%d does not has succ with label %d", bb->Id(), lab_id));
        }

        // make sure the prev/next is non-null.
        Is_True (bb->Prev() && bb->Next(), 
                 ("prev/next chain is broken at BB%d", bb->Id ()));

        if (BB_IFINFO* ifinfo = bb->Ifinfo ()) {
            if (BB_NODE* then_clause = ifinfo->Then ()) {
                Is_True (bb->Succ()->Contains (then_clause),    
                         ("incorrect IFIFO"));
            }

            if (BB_NODE* else_clause = ifinfo->Else ()) {
                Is_True (bb->Succ()->Contains (else_clause), 
                         ("incorrect IFINFO"));
            }
        }
    }
}
#endif


// Make sure loop-back block has a branch whose target is loop-header.
// We would othewise has great difficulty in gluing adjacent unrolled 
// bodies together. If look-back doesn't satisfy this criteria, the back-edge
// will be split by an block with only a goto-header-statement.
//
void
LOOP_UNROLL_UTIL::Split_backedge (void) {
    
    INT32 header_lab_num = _loop->Header()->Labnam ();

    STMTREP* br = _backedge_src->Branch_stmtrep ();
    if (!br) {
        STMTREP* goto_br = Append_goto_stmt (_backedge_src);
        goto_br->Set_label_number (header_lab_num);
    } else if (!OPERATOR_has_label (br->Opr ()) ||
               br->Label_number () != header_lab_num) {
        BB_NODE* bb = Get_cfg()->Add_bb_to_edge (_backedge_src, _loop->Header());
        STMTREP* goto_br = Append_goto_stmt (bb);
        goto_br->Set_label_number (header_lab_num);
        
        _backedge_src = bb;
        _loop->True_body_set()->Union1D (bb); 
        _all_blks.push_back (bb);
        _bbs_of_orig_loop.push_back (bb);
        _loop->Set_loopback (bb);
    }
}

void
LOOP_UNROLL_UTIL::Unroll (UINT32 factor) {
    
    Is_True (_is_fully_unroll, ("non-fully-unrolling is not yet supported"));
    Is_True (!_unrolled, ("cannot unrolled twice"));
    Is_True (factor > 1, ("Invalid unrolling factor"));

    _factor = factor;

    // step 1: set up maps properly
    //
    _lab_id_map_vect.resize (_factor - 1);
    _bb_map_vect.resize (_factor - 1);
    _bb_nodes_pair_vect.resize (_factor - 1);

    // step 2: bridge loop-back block and loop header with a goto-block if 
    //  necessary.
    //
    Split_backedge ();

    // step 3: clone body by factor-1 times.
    //
    for (UNROLL_IDX unroll_idx = 0; unroll_idx < factor - 1; unroll_idx++) {
        Clone_loop_body (unroll_idx);
    }
    
    // step 4: hook up cloned bodies and original loop body
    //
    Hookup_cloned_bodies ();

    // step 5: In case of fully-unroll, insert label and goto statements in 
    //   merge blocks of original blocks ---- just to avoid WN-emiter's 
    //   stupidity.
    //
    if (_is_fully_unroll) {
        for (BB_NODE_LIST::iterator iter = _merge_blks.begin (),
             iter_end = _merge_blks.end (); 
             iter != iter_end; iter++) {

            BB_NODE* merge = *iter;
            Prepend_label_stmt (merge);
            if (merge->Nth_succ (0) != merge->Next ()) {
                STMTREP* goto_stmt = Append_goto_stmt (merge);
                LAB_ID lab = Alloc_lab (merge->Nth_succ (0));
                goto_stmt->Set_label_number (lab);
            }
        }

        // Likewise, we need to insert label to header to sidestep 
        // emitter's problem.
        Prepend_label_stmt (_loop->Header ());
    }

    // step 6: update bb frequency
    //
    Adjust_bb_frequncy ();

    _unrolled = TRUE;
}

void
LOOP_UNROLL_UTIL::Fully_unroll (UINT32 trip_count) {

    _is_fully_unroll = TRUE;

    Unroll (trip_count);
    Destroy_loop_construct ();

    _unrolled = TRUE;

    Verify ();
}

/////////////////////////////////////////////////////////////////
//
//   Debugging as well as other "cold" functions go here
//
/////////////////////////////////////////////////////////////////
//
void
LOOP_CONSTRUCT::Print (FILE* f) const {
    
    INT32 indent_width = 2; // indent 2 blank

    fprintf (f, "%*c", indent_width, ' ');

    if (_loop->Header()) 
        fprintf (f, "Header BB:%d, ", _loop->Header()->Id());

    if (_loop->Preheader ())
        fprintf (f, "Preheader BB:%d, ", _loop->Preheader()->Id());

    if (_loop->End ())
        fprintf (f, "End BB:%d, ", _loop->End()->Id());

    if (_loop->Body ())
        fprintf (f, "First body BB:%d, ", _loop->Body()->Id());
    
    if (_loop->Tail ())
        fprintf (f, "Tail BB:%d, ", _loop->Tail()->Id());

    if (_loop->Loopback ())
        fprintf (f, "Loopback BB:%d, ", _loop->Loopback()->Id());

    if (_loop->Test_at_entry ())
        fprintf (f, "test at entry, ");
    else if (_loop->Test_at_exit ())
        fprintf (f, "test at exit, ");

    if (Is_do_loop () || Is_in_do_loop_shape ())
        fprintf (f, "DO loop, ");

    if (Is_normalized ())
        fprintf (f, "normalized, ");

    if (Is_const_strided ())
        fprintf (f, "stride=%d, ", Get_const_stride ());

    if (Get_iv_lower_bound ())
        fprintf (f, "IV lower bound CR:%p, ", Get_iv_lower_bound ());

    if (Get_iv_upper_bound ())
        fprintf (f, "IV upper bound CR:%p, ", Get_iv_upper_bound ());

    // print body 
    //
    fprintf (f, "\n%*cLoop body:", indent_width, ' ');
    BB_NODE_SET* body = _loop->True_body_set ();
    if (body) {
        fprintf (f, "{");
        BB_NODE_SET_ITER iter;
        BB_NODE* node;
        FOR_ALL_ELEM (node, iter, Init (body)) {
            fprintf (f, "%d,", node->Id());
        }
        fprintf (f, "}");
    } else {
        fprintf (f, "empty");
    }

    fprintf (f, "\n");
}
