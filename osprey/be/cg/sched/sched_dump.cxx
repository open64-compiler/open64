/* -*-Mode: c++;-*- (Tell emacs to use c++ mode) */

/*
 *  Copyright (C) 2000-2003, Intel Corporation
 *  All rights reserved.
 *  
 *  Redistribution and use in source and binary forms, with or without modification,
 *  are permitted provided that the following conditions are met:
 *  
 *  Redistributions of source code must retain the above copyright notice, this list
 *  of conditions and the following disclaimer. 
 *  
 *  Redistributions in binary form must reproduce the above copyright notice, this list
 *  of conditions and the following disclaimer in the documentation and/or other materials
 *  provided with the distribution. 
 *
 *  Neither the name of the owner nor the names of its contributors may be used to endorse or
 *  promote products derived from this software without specific prior written permission. 
 *
 *  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR
 *  IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
 *  FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE CONTRIBUTORS BE LIABLE FOR
 *  ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 *  NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
 *  BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 *  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 *  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 */

 /* =========================================================================
  * =========================================================================
  * 
  * Module: sched_dump.h
  *
  * $Revision: 1.1 $
  * $Date: 2005/12/30 01:50:22 $
  * $Author: weitang $
  * $Source: /depot/CVSROOT/javi/src/sw/cmplr/be/cg/sched/sched_dump.cxx,v $
  *
  * Description:
  * ============
  * 
  * Most of dump-routines implemented in this file. since dump routines 
  * are rarely used. We concentrate them in one place to reduce the working-
  * set of our compiler
  *
  * =========================================================================
  * =========================================================================
  */
#include "tracing.h"
#include "scheduler.h"
#include "stdarg.h"
#include "sched_rgn_info.h"
#include "sched_cflow.h"

#define DUMP_OP_SUMMARY(o,f)  { fprintf ((f), "[OP%3d][BB%3d][%-7s]", \
                                OP_map_idx((o)), BB_id(OP_bb((o))), \
                                TOP_Name(OP_code(o))); }        

#define BEGIN_WITH_NEW_LINE(f) { fprintf (f,"\n") ; } 

char * sched_dump_indent = "   " ; 
char * caption_l_bar = "=========>" ; 
char * caption_r_bar = "<=========" ;

    /* each line of dumpped content do not exceed this very much
     */
enum { LINE_MAX_WIDTH = 75, };

/* ===============================================================
 *
 *      Dump a individual CANDIDATE 
 * 
 * ==============================================================
 */
void
CANDIDATE :: Dump (FILE *f, BOOL verbose,
                      FAVOR_DELAY_HEUR * heur) {

    if (!Op()) {
        fprintf (f,"CANDIDATE(%p)'s corresponding OP is NULL", this);
        return ;
    }

    if (verbose) {
        DUMP_OP_SUMMARY(Op(),f) ; 
    }

    /* We should print out the cached _e_time field, the up-to-date
     * e-time is print by 'Dump_OP_Heur_Info()'
     */
    fprintf (f, " %-9s %s ", 
                 spec_text[_spec_type],
                 Is_P_Ready () ? "PREADY" : "MREADY");

    /* print the heuristic stuff associated with this candidate
     */
    if (heur) {
       heur->Dump_OP_Heur_Info (_op,f,FALSE);
    }
}

    /* ==============================================================
     *
     * tagRGN_INFO :: Dump 
     * 
     * Dump the status of RGN_INFO 
     * 
     * =============================================================
     */
void
RGN_INFO :: Dump (FILE * f) {

    if (!rgn) {
        fprintf (f,"REGION:(NULL)\n");
        return ;
    }

    char * s = NULL ;

    switch (skip_reason) {
    case SKIP_RGN_NONE:
        s = "(Need Scheduling)" ;
        break ;

    case SKIP_RGN_IMPROPER:
        s = "(Improper Region)" ;
        break ;

    case SKIP_RGN_NON_SEME:
        s = "(Non-SEME Region)" ;
        break ;

    case SKIP_RGN_CRITICAL_EDGE:
        s = "(Critical Edge present)";
        break ;

    case SKIP_RGN_DEBUG:
        s = "(Skip scheduling for debugging purpose)";
        break ;

    case SKIP_RGN_NO_FURTHER_OPT:
        s = "(Need no further opt)";
        break ;
    
    default:
        FmtAssert (FALSE,
            ("Unknown SKIP_SCHED_RGN_REASON (%d)", skip_reason));
    }

    fprintf (f, " %s, %s\n", s, 
                 in_abnormal_loop ? "In abnormal loop" : "");
    
    if (summary) { 
        summary->Dump (); 
    } else { 
        fprintf (f," summary : NULL") ; 
    }

    fprintf (f, "\n");
}


/* ==========================================================
 *
 *      Dump a whole candidate list
 *
 * =========================================================
 */
void
CAND_LIST :: Dump (FILE * f, BOOL verbose, FAVOR_DELAY_HEUR * heur) {

    if (verbose) {
        fprintf (f, "%sCANDIDATE LIST %d IN TOTAL "
                    "(%d TRIED)%s\n", 
             caption_l_bar,
             _total, _tried_cand_num,
             caption_r_bar) ;
    } else {
        BEGIN_WITH_NEW_LINE (f);
    }

    for (CAND_LIST_ITER iter(this) ; !iter.done(); iter.step ()) {

        CANDIDATE* cand = iter.cur ();

            /* we should distinguish each candidate, if its dumpped
             * text is terse, it may be illegible.
             */
        cand->Dump (f, TRUE, heur);
        fprintf (f,Cand_Has_Been_Tried (cand) ? "  TRIED":" NONTRY");

        BEGIN_WITH_NEW_LINE (f);
    }
}

#ifdef Is_True_On

void
CAND_LIST :: gdb_dump (FAVOR_DELAY_HEUR * heur) {
    Dump (stderr, TRUE, heur);    
}

#endif


void
SRC_BB_MGR::Dump (FILE *f) {

    if (!_targ) return ;
        
    fprintf (f, "SOURCE BBs OF BB:%d\n", BB_id(_targ));

    for (SRC_BB_INFO_ITER iter = _src_info_vect.begin () ; 
        iter != _src_info_vect.end() ; iter++) {

        SRC_BB_INFO* info = *iter ;
        fprintf (f, "\t====> SRC BB:%3d cutting set:", 
                 BB_id(info->Source_BB ()));
        
        BB_VECTOR* bbv = info->Get_Cutting_Set ();
        for (BB_VECTOR_ITER cs_iter = bbv->begin() ; 
             cs_iter != bbv->end() ; cs_iter ++) {
            fprintf (f,"BB:%3d ", BB_id(*cs_iter));
        }

        bbv = info->Move_Across_Or_Around_BBs ();
        fprintf (f, "\n\t    BBs between src BB and cutting-set:");
        for (BB_VECTOR_ITER tmp_iter = bbv->begin() ;
             tmp_iter != bbv->end() ; 
             tmp_iter++) {

            fprintf (f,"BB:%3d ", BB_id(*tmp_iter));

        }

        fprintf (f, "\n\t    Nested REGIONs between src BB and cutting-set:");
        REGION_VECTOR* rv = info->Move_Across_Or_Around_Nested_Rgns ();
        for (REGION_VECTOR_ITER rgn_iter = rv->begin ();
             rgn_iter != rv->end ();
             rgn_iter ++) {

            fprintf (f, "RGN:%3d ", (*rgn_iter)->Id());

        }

        fprintf (f,"\n");
    }

}

#ifdef Is_True_On

void
SRC_BB_MGR :: gdb_dump (void) {
    Dump (stderr);
}

#endif 

    /* =======================================================
     *
     *   Dump SCHED_BB_ANNOT structure's content 
     *
     * =======================================================
     */
void
SCHED_BB_ANNOT::Dump (FILE *f) {

    fprintf (f,"\n===========BB:%d===========\n", BB_id(_bb));

    if (!_1st_append_op)
        fprintf (f, "no appended op,") ; 
    else 
        fprintf (f, "1st appended op[%d],", OP_map_idx(_1st_append_op));


    if (!_last_prepend_op) 
        fprintf (f, "no prepend op,") ;
    else
        fprintf (f, "last prepend op[%d],", OP_map_idx(_last_prepend_op));

    fprintf (f, "\n");

    OP * op ;
    SCHED_OP_ANNOT * op_annot ; 

    FOR_ALL_BB_OPs (_bb, op) {
        op_annot = Get_OP_Annot(op) ;
        if (!op_annot) fprintf (f, "[OP:%d] NULL\n", OP_map_idx(op));
        else op_annot->Dump(f);
    }

}

     /* =======================================================
      *
      *   Dump SCHED_RGN_ANNOT structure's status 
      * 
      * =======================================================
      */
void
SCHED_RGN_ANNOT::Dump (FILE *f) {
    fprintf (f, "RGN %d\n", _rgn->Id()); 
}

    /* =====================================================
     *
     *   Dump SCHED_OP_ANNOT structure's status
     *
     * =====================================================
     */
void
SCHED_OP_ANNOT::Dump(FILE* f) {
    
    fprintf (f, 
        "OP[%d] %s, original home BB BB:%d, current home BB BB:%d, ext-flag:%#X\n", 
                 OP_map_idx(_op), TOP_Name(OP_code(_op)),
                 BB_id (_org_home_bb), BB_id(OP_bb(_op)),
                 _ext_flags);
}

    /* ====================================================
     *
     *  Dump the content of CFG_NODE_MAP
     *
     * ===================================================
     */
void
CFG_NODE_MAP :: Dump (FILE *f) {

    char buf[LINE_MAX_WIDTH * 2], *next;
    
    if (_bb_map_vect.size()) {
        
        next = &buf[0]; *next = '\0';
        BOOL first_elem = TRUE;

        for (_PAIR_VECT_ITER iter = _bb_map_vect.begin () ;
             iter != _bb_map_vect.end ();
             iter ++) {

            next += sprintf (next, "%s<BB:%d,%p>", 
                                    first_elem ? "" : ", ",
                                    (*iter).node_id, 
                                    (*iter).value);

            if (next - &buf[0] >= LINE_MAX_WIDTH) {
                fprintf (f,"%s\n", buf);
                next = &buf[0];
            }
            
            first_elem = FALSE;
        }
        fprintf (f,"%s\n", buf);
    }

    if (_rgn_map_vect.size()) {

        next = &buf[0]; *next = '\0';
        BOOL first_elem = TRUE;

        for (_PAIR_VECT_ITER iter = _rgn_map_vect.begin () ;
             iter != _bb_map_vect.end ();
             iter ++) {

            next += sprintf (next, "%s<RGN:%d,%p>", 
                                    first_elem ? "" : ",",
                                    (*iter).node_id, 
                                    (*iter).value);

            if (next - &buf[0] >= LINE_MAX_WIDTH) {
                fprintf (f, "%s\n", buf);
                next = &buf[0];
            }

            first_elem = FALSE;
        }

        fprintf (f, "%s\n", buf);
    }
}

#ifdef Is_True_On

void
CFG_NODE_MAP :: gdb_dump (void) {
    Dump (stderr) ; fflush (stderr); 
}

#endif

    /* ========================================================
     * 
     *   Dump a paticular OP_HEUR_INFO structure
     * 
     * =======================================================
     */
void
FAVOR_DELAY_HEUR::tagOP_HEUR_INFO::Dump (FILE *f,BOOL verbose) {

    if (verbose) {
        DUMP_OP_SUMMARY(_op,f);
    }

    fprintf (f," FOUT %2d E_TIM %3d ISSUE_PORT_# %2d DELAY %f",
                _fan_out, _e_time, 
                _issuable_port_num,_delay);
}

   /* ========================================================
    *
    *    Dump heuristic information associated with 
    *    the given OP
    *
    * ========================================================
    */
void
FAVOR_DELAY_HEUR :: Dump_OP_Heur_Info 
    (OP * op, FILE *f, BOOL verbose) {
   
    OP_HEUR_INFO * info = Get_OP_Heur_Info (op);
    if (info) {
        info->Dump (f, verbose);
    } else {
        if (verbose) DUMP_OP_SUMMARY(op,f);
        fprintf (f, " (NULL)");
    }
}

    /* =======================================================
     *
     *   Dump heuristic information associated with OPs 
     *   reside in <bb>
     * 
     * =======================================================
     */
void
FAVOR_DELAY_HEUR :: Dump_OP_Heur_Info 
    (BB * bb, FILE *f,BOOL verbose) {

    if (verbose) {
        fprintf (f,"%sHeuritics Infor OPs Reside in BB:%d%s\n", 
             caption_l_bar,
             BB_id(bb),
             caption_r_bar);
    }

    OP * op ;
    FOR_ALL_BB_OPs(bb, op) {

        /* It is make no sense to dump nop's heuristic (actually
         * there is no heuristic stuff bound to nop.
         */
        if (OP_noop(op)) continue ;

        Dump_OP_Heur_Info (op,f,TRUE);
        BEGIN_WITH_NEW_LINE (f);
    }
}


 /* ===============================================================
  *
  *    Dump the status of class FAVOR_DELAY_HEUR
  *
  * ==============================================================
  */
void
FAVOR_DELAY_HEUR::Dump (FILE * f,BOOL verbose) {

    if (verbose) {
        fprintf (f,"%s FAVOR_DELAY_HEUR status %s\n",
                    caption_l_bar, caption_r_bar);
    }

    if (_bb_scope) {

       Dump_OP_Heur_Info (_bb_scope, f, verbose);

    } else {

        for (TOPOLOGICAL_REGIONAL_CFG_ITER 
             iter (_rgn_scope -> Regional_Cfg()) ; iter != 0 ; ++ iter) {
            
            if ((*iter)->Is_Region ()) continue ;
            BB * bb = (*iter)->BB_Node ();
            if (BB_entry(bb) || BB_exit(bb)) { continue ; } ;

            Dump_OP_Heur_Info (bb, f,verbose);

        } /* end of for-loop */
    }
}

#ifdef Is_True_On

void
FAVOR_DELAY_HEUR::gdb_dump (void) {
    Dump (stderr, TRUE);
}

#endif  /* Is_True_On */

    /* ==========================================================
     *
     *     Trace candidate selection process 
     *
     * =========================================================
     */
void
FAVOR_DELAY_HEUR::Enable_Trace_Cand_Sel_Process (FILE* f) {

    _trace_cand_sel = TRUE;
    _trace_file = f ;
    
}

void
FAVOR_DELAY_HEUR :: Disable_Trace_Cand_Sel_Process (void) {
    
    _trace_cand_sel = FALSE;

    if (_trace_file) {
       _trace_file = NULL ;
    }
}


void 
FAVOR_DELAY_HEUR :: Trace_Cand_Sel_Process (const char * fmt, ...) {

    Is_True (_trace_cand_sel, 
            ("We have not enable tracing candidate selection process yet!"));

    va_list ap ;

    va_start(ap, fmt);
    vfprintf (_trace_file, fmt, ap);
    va_end(ap);
}





 /* =====================================================
  * =====================================================
  * 
  *  Dumping routines used directly by SCHEDULER
  *
  * =====================================================
  * =====================================================
  */


 /* =====================================================
  *
  *   Dump verbose information for a given OP 
  * 
  * =====================================================
  */
void
SCHEDULER::Dump_OP_Verbose_Info (OP* op, FILE * f) {

        /* 1. print the <op> itself 
         */
    DUMP_OP_SUMMARY(op,f);
    BEGIN_WITH_NEW_LINE(f);

    Set_Trace_File_internal (f);
    Print_OP_No_SrcLine (op);
    Set_Trace_File_internal (TFile);

    if (OP_Scheduled (op)) {
       fprintf (f,"ISSUE CYCLE: %3d ", OP_scycle(op));  
    }

        /*2. dump heuristic info */
    _heur_mgr.Dump_OP_Heur_Info (op, f,FALSE);
    
        /*3. dump its associated CADIDATE structure if any
         */
    CANDIDATE* cand = _cand_mgr.Get_Candidate (op) ;
    if (cand) { cand->Dump (f,FALSE); }

        /* ??? this should be put into cg_dep_graph.cxx */

    static char *arc_txt[] = {
        "REGIN", "REGOUT", "REGANTI", "MEMIN", "MEMOUT", "MEMANTI", 
        "MEMVOL", "MEMREAD", "SPILLIN", "PREFIN", "PREFOUT", "PREBR", 
        "POSTBR", "SCC", "PRECHK", "POSTCHK", "CTLSPEC", "MISC"
    };

        /*4. dump predecessors */
    fprintf (f, "\nPREDECESSORS:\n");

    for (ARC_LIST* arcs = OP_preds(op); 
         arcs != NULL; arcs = ARC_LIST_rest(arcs)) {

        ARC *arc = ARC_LIST_first(arcs) ;
        OP * pred = ARC_pred(arc) ;

        DUMP_OP_SUMMARY(pred,f);
        fprintf (f, "%-8s LAT %2d %s %-5s ", 
		       	    arc_txt[ARC_kind(arc)], 
                    ARC_latency(arc),
			        ARC_is_dotted(arc)   ? "Dotted" : "Strict", 
			        ARC_is_definite(arc) ? "Def" : "Indef");
        
        _heur_mgr.Dump_OP_Heur_Info (pred,f,FALSE); 
        if (OP_Scheduled(pred)) { fprintf (f," SCH") ; }
        BEGIN_WITH_NEW_LINE(f);
    }

        /*5. dump succs 
         */
    fprintf (f,"\nSuccessors:\n") ; 
    for (ARC_LIST* arcs = OP_succs(op); 
         arcs != NULL; arcs = ARC_LIST_rest(arcs)) {
        
        ARC * arc = ARC_LIST_first(arcs) ;
        OP  * succ = ARC_succ(arc);

        DUMP_OP_SUMMARY(succ,f);
        fprintf (f, "%-9s LATENCY %3d %-8s %s ", 
		       	    arc_txt[ARC_kind(arc)], 
                    ARC_latency(arc),
			        ARC_is_dotted(arc)   ? "Dotted" : "Strict", 
			        ARC_is_definite(arc) ? "Def" : "Indef");

        _heur_mgr.Dump_OP_Heur_Info (succ,f,FALSE); 
        if (OP_Scheduled(succ)) { fprintf (f," SCH") ; }

        BEGIN_WITH_NEW_LINE(f);
    }
}


/* ====================================================================
 *
 * SCHEDULER::Dump_IR (void)
 * 
 * We does not encourage dumping IR in this way. use SCHED_Dump_IR 
 * instead. 
 * 
 * ===================================================================
 */
void
SCHEDULER :: Dump_IR (FILE *f) {

    FmtAssert (FALSE,
        ("Please Use 'SCHED_Dump_IR' to dump IR instead!"));
}

/* ===================================================================
 *
 *  SCHEDULER::Dump_DAG ()
 * 
 * dump the dependence DAG into file <f>   
 * 
 * ==================================================================
 */
void
SCHEDULER :: Dump_DAG (FILE *f) {

    if (_global) {

        for (TOPOLOGICAL_REGIONAL_CFG_ITER cfg_iter(_region->Regional_Cfg()); 
             cfg_iter != 0; 
             ++cfg_iter) {

            if ((*cfg_iter)->Is_Region ())  continue ;

            BB * b = (*cfg_iter)->BB_Node ();
            if (BB_entry(b) || BB_exit(b))  continue ;

            CG_DEP_Trace_Graph(b);
        } /* end of for(TOPOLOGICAL...) */

    } else { 

        CG_DEP_Trace_Graph(_target_bb);

    }
}


/* ====================================================================
 *
 *  SCHEDULER::Dump ()
 * 
 *  Dump IR, candidate list, DAG etc.
 *
 * ===================================================================
 */
void
SCHEDULER::Dump (FILE *f) {
    
    FmtAssert (FALSE,
        ("SCHEDULER::Dump () has yet implemented!"));
}


    /* ===============================================================
     *
     *   SCHED_Dump_IR 
     *
     *  dump IR of OPs in current code motion scope 
     * 
     *  prepass:    before or after register allocation
     *  bef_sched:  indicate whether IR is dumpped before 
     *              code motion occurs or after it 
     *  gcm:        global or local code motion 
     *  f:          it is self-descriptive
     *
     * ===============================================================
     */
void
SCHED_Dump_IR (BOOL prepass, BOOL bef_sched, BOOL gcm,FILE *f) {

    FILE * ftmp = TFile ;
    Set_Trace_File_internal (f);

    /* print header */
    fprintf (f, "\n%s\n IR %s ORC %s %s CODE MOTION (BEGIN)\n\n%s\n",
                DBar,  
                bef_sched ? "before"  : "after", 
                prepass   ? "PREPASS" : "POSTPASS", 
                gcm       ? "GLOBAL"  : "LOCAL",
                DBar);

 
    for (BB* bb = REGION_First_BB; bb; bb = BB_next(bb)) {
        Print_BB_No_Srclines (bb);

        /* extra empty line to ease the navigation in vi */
        fprintf (f, "\n");
    }
 
    fprintf (f, "\n%s\n IR %s ORC %s %s CODE MOTION (END)\n\n%s\n",
                DBar,  
                bef_sched ? "before"  : "after", 
                prepass   ? "PREPASS" : "POSTPASS", 
                gcm       ? "GLOBAL"  : "LOCAL",
                DBar);

    Set_Trace_File_internal (ftmp); 
}


    /* ==========================================================
     * ==========================================================
     *
     *      Tracing routines of sched_rgn_info.cxx package 
     *
     * ==========================================================
     * ==========================================================
     */

void
RGN_SUMMARY::Dump (FILE * f, BOOL verbose) {

    if (verbose) {
        fprintf (f, "REGION %d", _rgn->Id());
    }
    
    if (!_summary_is_valid) {
        fprintf (f, "Invalid summary of REGION:%d\n", 
                    _rgn->Id());
        return ;
    }

    fprintf (f,"killed defs:\n");
    TN_SET_Print (_killed_def,f);
}

void
REGION_INFO_MGR :: Dump (FILE *f, BOOL verbose) {

    for (INNERMOST_REGION_FIRST_ITER iter(_rgn_tree);
         iter != 0 ; ++iter) {
    
        fprintf (f,SBar);
        RGN_INFO * rgn_info = Get_Rgn_Info (*iter) ;
        rgn_info->Dump (f) ;
        fprintf (f, "%s\n", SBar);
    }
}

#ifdef Is_True_On 
void
RGN_SUMMARY::gdb_dump (void) {
    Dump (stderr, TRUE);
}
#endif


    /* =======================================================
     * =======================================================
     *
     * 
     *      Traceing routines of EXEC_PATH, EXEC_PATH_SET,
     *      and EXEC_PATH_MGR.
     *
     * ======================================================
     * ======================================================
     */

    /* dump the content of EXEC_PATH 
     */
void
EXEC_PATH :: Dump (FILE *f) {
    
        /* format e.g :
         *  path:5 {
         *       3,4,r5,7, 9
         *  }
         */
    fprintf (f, "path:%d{\n", Id ());
   
    char buf[LINE_MAX_WIDTH * 2];
    char *p = buf; *p = '\0';

    BOOL first_elem = TRUE;

    for (PATH_NODE_INFO_VECT_ITER iter = _path_node_seq.begin () ;
         iter != _path_node_seq.end ();
         iter ++) {

        PATH_NODE_INFO ni = *iter;
        
        if (!first_elem) { *p++ = ','; } else { first_elem = FALSE; }

        if (ni.Node()->Is_Region ()) {
            p += sprintf (p, "r%d(%f)", 
                          ni.Node()->Region_Node()->Id(),
                          ni.Prob_From_Root ());
        } else {
            p += sprintf (p, "%d(%f)", 
                          BB_id(ni.Node()->BB_Node()),
                          ni.Prob_From_Root ());
        }

        if (p - &buf[0] >= LINE_MAX_WIDTH) {
            fprintf (f,"%s",buf); p = &buf[0]; *p = '\0';
            fprintf (f, "\n");
        }
    }

    fprintf (f, "%s\n}\n", buf);
}

    /* =======================================================
     *
     * EXEC_PATH_SET::Dump
     * 
     * Dump the content of EXEC_PATH_SET. 
     * <dump_path_id_range> is the supporting routine to Dump.
     *
     * =======================================================
     */
static void
dump_path_id_range 
    (FILE *f, EXEC_PATH_ID begin, EXEC_PATH_ID end, BOOL* first_range) {
   
    if (!*first_range) { fprintf (f,","); }
    if (end == begin) { fprintf (f, "%d", begin); }
    else if (end > begin) {
        fprintf (f, "%d-%d", begin, end);
    } else if (end < begin) {
        FmtAssert (FALSE, 
            ("<begin>(%d) is expected no greater than <end>(%d)",
             begin, end));
    }
    
    * first_range = FALSE ;
}

void
EXEC_PATH_SET :: Dump (FILE *f) {
   
    BOOL first_range = TRUE;
    fprintf (f,"{ ");
    
    EXEC_PATH_ID  range_begin = First_Path_Id ();

    if (EXEC_PATH_ID_IS_INVALID(range_begin)) {
        fprintf (f,"}\n");
        return ;
    }

    EXEC_PATH_ID pid,last_pid=range_begin;

    while (!EXEC_PATH_ID_IS_INVALID(pid = Next_Path_Id (last_pid))) {

        if (pid != last_pid + 1) {
            /* continguity stop here 
             */
            dump_path_id_range (f, range_begin, last_pid,&first_range);
            range_begin = pid;
        }
        last_pid = pid;
    }

    dump_path_id_range (f, range_begin, last_pid, &first_range);

    fprintf (f,"}\n");
}

    /* dump the content of EXEC_PATH_MGR 
     */
void
EXEC_PATH_MGR :: Dump (FILE *f) {
    
    if (!_region) {  return ; };

    fprintf (f, "EXEC PATH INFO OF REGION:%d\n", _region->Id());
    
    for (EXEC_PATH_VECT_ITER iter = _pathv.begin () ;
         iter != _pathv.end ();
         iter ++) {
        (*iter)->Dump (f); 
    }

    fprintf (f,"%s", DBar);
}

#ifdef Is_True_On 

void
EXEC_PATH :: gdb_dump (void) {
    Dump (stderr); fflush (stderr);
}

void
EXEC_PATH_SET :: gdb_dump (void) {
    Dump (stderr); fflush (stderr);
}

void
EXEC_PATH_MGR :: gdb_dump (void) {
    Dump (stderr); fflush (stderr);
}

#endif /* Is_True_On */
