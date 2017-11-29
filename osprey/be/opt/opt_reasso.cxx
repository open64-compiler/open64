/*
 * Copyright (C) 2008-2010 Advanced Micro Devices, Inc.  All Rights Reserved.
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

// opt_reasso.cxx/.h
// A redundancy elimination scheme that takes advantage of the 
// associative property is introduced.
// 
// Motivating examples:
// Open64's PRE or GVN passes are currently not taking 
// advantage of the associative property. 
// The following example gets optimized by GVN or PRE. 
// D = .
// G = .
// I = 5*G*D
// J = 5*G
// Subexpression 5*G is eliminated leaving 
// only two multiplies in the assembly.
// However, when the same example is rewritten 
// with a different association, it does not get optimized. 
// D = .
// G = .
// I = 5*G*D
// J = 5*D
// Neither does the following rewrite get optimized.
// D = .
// G = .
// I = 5*G*D
// J = G*D
// An earlier canonicalization led to a expression 
// tree that looks like this (D*(5*G)) all the time. 
// Only 5*G will ever be optimized.
// 
// These examples show that we can use reassociation 
// in open64. Reassociation can be enabled with a 
// subexpression elimination pass that is aware of the 
// Math laws for associative arithmetic and then uses these 
// laws to guide CSE. 
// We not only need reassociation for term 
// based NARY trees (like the above example) but also for 
// trees that also include trees in them, i.e. non-terminal 
// based NARY trees, like (A*B+C*D).
// 
// We worked on a recursive formulation that works from 
// term based and goes up into non-term based trees that 
// contain them. We have now implemented a nary-reassociation 
// pass. 
// 
// Our intention is to build a standard recursive algorithm 
// (using dynamic programming) that selects a subexpression 
// elimination candidate by using the number of operations that 
// get eliminated as a cost metric. Most of the challenges have 
// been in getting an NARY representation of the associative 
// input WHIRL trees, which are in the form of binary trees 
// for OPR_ADD, OPR_MPY and unary trees for OPR_NEGs and leaf 
// WHIRLs for OPR_LDID and OPR_CONSTs. We rewrite WHIRL into 
// newly introduced NARY data structures, nary_exp and 
// wcexp and work on these NARY forms.
// The ideas here are heavily influenced by or borrowed from 
// these two works:
// #1:
// Dennis J. Frailey,
// "Expression optimization using unary complement operators"
// @inproceedings{808485,
// author = {Frailey, Dennis J.},
// title = {Expression optimization using unary complement operators},
// booktitle = {Proceedings of a symposium on Compiler optimization},
// year = {1970},
// pages = {67--85},
// location = {Urbana-Champaign, Illinois},
// doi = {http://doi.acm.org/10.1145/800028.808485},
// publisher = {ACM},
// address = {New York, NY, USA},
// }
// #2:
// Keith Cooper, et al
// "Redundancy elimination revisited"
// @inproceedings{1454120,
// author = {Cooper, Keith and Eckhardt, Jason and Kennedy, Ken},
// title = {Redundancy elimination revisited},
// booktitle = {PACT '08: Proceedings of the 17th international conference 
// on Parallel architectures and compilation techniques},
// year = {2008},
// isbn = {978-1-60558-282-5},
// pages = {12--21},
// location = {Toronto, Ontario, Canada},
// doi = {http://doi.acm.org/10.1145/1454115.1454120},
// publisher = {ACM},
// address = {New York, NY, USA},
// }
// 
// We have added comments in this file, opt_reasso.cxx, that 
// should answer a number of questions. Please contact via 
// appropriate forum with other questions. 
// 
// Placement of pass:
// It is placed after Dead store elimination after
// SSA but before CODEREP. This pass can only operates on 
// SSA WHIRL, not on CODEREPs or on non-SSA WHIRL.
// 
// Enabling the pass:
// The pass is always on, but can be disabled by unsetting 
// a flag: WOPT_Enable_Reassociation_CSE. However it is only 
// active if Roundoff_Level is greater than ROUNDOFF_ASSOC, 
// which is the default case with -Ofast. Finally, it is only 
// applied during the MAINOPT_PHASE.
// 
// Debug: Traces can be obtained by using the REASSO_DUMP_FLAG 
// and REASSO_DUMP_FLAG_DEBUG suboptions to ttWOPT2 
// (-Wb,-tt26,0x2000, -Wb,-tt26,0x4000).
// 
// Restrictions:
// 1: Pass works at basic block level only. Maybe that is 
// a reasonable choice considering the algorithmic 
// complexity of the solution. It should be straightforward
// to port it to work on function level with a CFG traversal
// similar to GVN's 
// The algorithmic complexity is around O(n^2). 
// 2: Then it applies redundancy elimination only to 
// associative trees (not really a restriction). 
// 3: Also associative trees should only use LDIDs and CONSTs 
// as leaves (array expressions via OPR_ILOADs are not 
// allowed in this version). 
// 4: Also, it is legal only if the variables in the 
// basic block are used in a SSA like way. 
// 5: Also we only allow statements like OPR_STIDs, 
// OPR_ISTOREs, OPR_COMMENTs, OPR_PRAGMAs, OPR_LABEL 
// statements in the basic block. This restriction is also 
// set aside for future work. 
// 
// However, even with these restrictions we found that 
// many redundancies were eliminated.
// 
// The following are main points from these restrictions: 
// pass does not work on array accesses, pass is not 
// valid if scalars are written to more than once 
// in a basicblock or if a def follows a use in a basic block. 
// 

#include "opt_reasso.h"

namespace REASSO {
MEM_POOL cse_name_pool;
bool operator== (const nary_exp& x, const nary_exp& y) {
    if (x.narytyp != y.narytyp) 
        return false;
    if (x.aop != y.aop)
        return false;
    if (x.aopc != y.aopc)
        return false;
    if (x.anary.size() != y.anary.size()) 
        return false;
    
    if (x.narytyp == AWN && y.narytyp == AWN) {
        if (WN_operator(x.awn) == OPR_LDID 
            && WN_operator(y.awn) == OPR_LDID) {
            if (WN_ver(x.awn) == WN_ver(y.awn)) {
                    return true;
                } else {
                    return false;
                }
        } else if(WN_operator(x.awn) == OPR_INTCONST
            && WN_operator(y.awn) == OPR_INTCONST) {
            if (WN_const_val(x.awn) == WN_const_val(y.awn)) {
                return true;
            } else {
                return false;
            }
        } else if (WN_operator(x.awn) == OPR_CONST &&
                WN_operator(y.awn) == OPR_CONST) {
            if (WN_st(x.awn) == WN_st(y.awn)) {
                return true;
            } else {
                return false;                
            }
        } else {
            return false;
        }
    } else if (x.narytyp == NARY && y.narytyp == NARY) {
        if (x.aop == y.aop) {
            vector<nary_exp>::iterator xit;
            vector<nary_exp> myx = x.anary;
            vector<nary_exp> myy = y.anary;
            if (myx.size() != myy.size()) {
                return false;
            }
            for (xit = myx.begin(); xit != myx.end(); ++xit) {
                vector<nary_exp>::iterator myyit;
                myyit = find (myy.begin(), myy.end(), (*xit));
                if (myyit == myy.end()) {
                    return false;
                } else {
                    // remove what we found. This is meant to 
                    // solve the duplicate term issue.
                    myy.erase(myyit);
                }
            }
            return true;
        } else {
            return false;
        }
    }
    else {
        return false;
    }
}

bool operator< (const nary_exp& x, const nary_exp& y) {
    // This is a general kind ordering, 
    // not very accurate
    if (x.narytyp == AWN) 
        return true;
    else if (y.narytyp == AWN)
        return true;
    if (x.narytyp == NARY) {
        if (x.aop == OPR_NEG)    
            if (x.anary[0].narytyp == AWN)
                return true;
    } else if (y.narytyp == NARY) {
        if (y.aop == OPR_NEG ) {
            if (y.anary[0].narytyp == AWN) {
                return true;
            }
        }
    }
    return false;
}

bool operator== (const wcexp& x, const wcexp& y) {
    // we know that there are no duplicates in wcexps 
    if (x.wcexptyp != y.wcexptyp) 
        return false;

    if (x.count != y.count) {
        return false;
    }

    if (x.aop != y.aop) {
        return false;
    } 
    
    if (x.aopc != y.aopc) {
        return false;
    }

    if (x.wnode.size() != y.wnode.size()) {
        return false;
    }

    if (x.wcexptyp == WTERM && y.wcexptyp == WTERM) {
        if (x.aop == OPR_LDID && y.aop == OPR_LDID) {
            if (WN_ver(x.wterm) == WN_ver(y.wterm)) {
                return true;
            } else {
                return false;
            }
        } else if(x.aop == OPR_INTCONST && y.aop == OPR_INTCONST) {
            if (WN_const_val(x.wterm) == WN_const_val(y.wterm)) {
                return true;
            } else {
                return false;
            }
        } else if (x.aop == OPR_CONST && y.aop == OPR_CONST) {
            if (WN_st(x.wterm) == WN_st(y.wterm)) {
               return true;
            } else {
                return false;
            }
        } else {
            return false;
        }
    } else if (x.wcexptyp == WNODE && y.wcexptyp == WNODE) {
        if (x.aop == y.aop) {
            vector<wcexp>::iterator xit;
            vector<wcexp> myx = x.wnode;
            vector<wcexp> myy = y.wnode;

            if (myx.size() != myy.size()) {
                return false;
            }

            for (xit = myx.begin(); xit != myx.end(); ++xit) {
                vector<wcexp>::iterator myyit;
                myyit = find (myy.begin(), myy.end(), (*xit));
                if (myyit == myy.end()) {
                    return false;
                }
            }
            return true;
        } else {
            return false;
        }
    } else {
        return false;
    }
}

void nary_assert (nary_exp an) {
    if (an.narytyp == AWN) {
        if (an.anary.size() > 0) {
            FmtAssert(FALSE, ("Found nary type nary_exp which type is WHIRL"));
        } 
    } else {
        if (an.aop == OPR_NEG) {
            if (an.anary.size() != 1) {
                FmtAssert(FALSE, ("Found more than one operand in a NARY neg type"));
            }
        } else {
            if (an.anary.size() <= 1) {
                FmtAssert(FALSE, ("Found unsupported number of operands in NARY type"));
            }
        }
    }
}

// This does the opposite of create_wcexp 
// by returning a nary_exp for an input wcexp.
nary_exp convert_wcexp_to_nary (const wcexp& wcx) {
    if (wcx.wcexptyp == WTERM) {
        nary_exp ncx = make_nary_term (wcx.wterm);
        nary_assert(ncx);
        return ncx;
    } else {
        nary_exp ncx(NARY, wcx.aop, wcx.aopc);
        vector<wcexp>::const_iterator wcxit;
        for (wcxit = wcx.wnode.begin();
             wcxit != wcx.wnode.end();
             ++wcxit) {
            if (((*wcxit).aop == OPR_NEG) && 
                    ((*wcxit).wnode[0].wcexptyp == WTERM)) {
                for (int i = 0; i < (*wcxit).wnode[0].count; ++i) {
                        nary_exp anewna (NARY, (*wcxit).aop, (*wcxit).aopc);
                        anewna.anary.push_back(
                            convert_wcexp_to_nary ((*wcxit).wnode[0]));
                        ncx.anary.push_back(anewna);
                }
            } else {
                for (int i = 0; i < (*wcxit).count; ++i) {
                    ncx.anary.push_back(
                            convert_wcexp_to_nary (*wcxit));
                }
            }
        }
        nary_assert(ncx);
        return ncx;
    }
}

WN* WHIRL_of_wcexp (const wcexp& wcx) {
    nary_exp nexp = convert_wcexp_to_nary (wcx);
    return WHIRL_of_nary_exp (nexp);
}

// The difference between nary_exp and 
// wcexp is that each wcexp uses a count 
// variable to keep count of the number 
// of duplicate expressions contained 
// in it. Consequently wcexp contains 
// no duplicates but nary_exps can 
// contain duplicates. So they basically are 
// synonymous. For a given task, for example, 
// proving that a given nary_exp is a subexpression 
// of another given nary_exp, there is  
// a simplicity in implementation if we  
// use wcexp instead of nary_exp. 
// create_wcexp creates a wcexp for a given
// nary_exp by implicitly invoking operator== 
// via STL find algorithm to handle
// duplicates.
wcexp create_wcexp (const nary_exp& ctree) {
    if (ctree.narytyp == NARY) {
        wcexp ret(WNODE, ctree.aop,
                ctree.aopc, 1);
        for (vector<nary_exp>::const_iterator ctit 
            = ctree.anary.begin();
            ctit != ctree.anary.end();
            ++ctit) {
            wcexp temp = create_wcexp(*ctit);
            vector<wcexp>::iterator retit;
            retit = find (ret.wnode.begin(), 
                    ret.wnode.end(), 
                    temp);
            if (retit == ret.wnode.end()) {
                ret.wnode.push_back(temp);
            } else {
                if ((*retit).wcexptyp == WNODE &&  
                        (*retit).aop == OPR_NEG) {
                    if ((*retit).wnode[0].wcexptyp == WTERM) {
                        wcexp anewwx (WTERM, (*retit).wnode[0].aop,
                                (*retit).wnode[0].aopc, 
                                (*retit).wnode[0].count+1);
                        anewwx.wterm = (*retit).wnode[0].wterm;
                        (*retit).wnode[0] = anewwx;
                    } else {
                        const char *myasserts = 
                            "Unsupported neg expression in create_wcexp, expecting NEG operations only on terms";
                        FmtAssert (FALSE, (myasserts)); 
                    }
                } else {
                    (*retit).count = (*retit).count + 1;
                }
            }
        }
        if (ctree != convert_wcexp_to_nary(ret)) {
            FmtAssert(FALSE, ("Error in create_wcexp from NARY type nary_exp"));
        }
        return ret;
    } else {
        wcexp myret(WTERM, ctree.aop,
                ctree.aopc, 1);
        myret.wterm = ctree.awn;
        if (ctree != convert_wcexp_to_nary(myret)) {
            FmtAssert(FALSE, ("Error in create_wcexp from TERM type nary_exp"));
        }
        return myret;
    }
}

// utility function that pushes a term based nary
// expression into term_based_explis, and 
// creates wcexp for this nary_exp and pushes that into
// factorized_term_explis.
void add_exp (nary_exp cxp, reasso_struct& rrec) {
    rrec.expressions.term_based_explis.push_back (cxp);
    rrec.expressions.factorized_term_explis.push_back (
            create_wcexp(cxp));
    rrec.expressions.current_expr_id = 
        rrec.expressions.current_expr_id + 1;
}

// This function is the predicate that a given nary_exp
// needs to satisfy to be called term_based_expression, IOW,
// the nary_exp is checked to see if it contains only terms.
bool all_term_based (const nary_exp& atree) {
    if (atree.narytyp == AWN) {
        return true;
    } else if (atree.narytyp == NARY) {
        if (atree.aop == OPR_NEG) {
            if (atree.anary[0].narytyp == AWN) {
                return true;
            } else {
                return false;
            }
        } else {
            for (vector<nary_exp>::const_iterator nvit = 
                    atree.anary.begin();
                 nvit != atree.anary.end();
                 ++nvit) {
                if ((*nvit).narytyp == NARY) {
                    // Only allow neg based NARY trees through
                    // however even these need to be termbased
                    if ((*nvit).aop == OPR_NEG) {
                        if ((*nvit).anary[0].narytyp == NARY) {
                            return false;
                        }
                    } else {
                        return false;
                    }
                }
            }
            return true;
        }
    }
    FmtAssert(FALSE, ("Unknown NARY type for nary_exp"));
}

// Mutually recursive functions recurse_def and 
// make_entry work on a given nary_exp and collect
// all term_based_expressions from the nary_exp and its
// children.
void recurse_def (nary_exp atree, reasso_struct& rrec);
void make_entry (nary_exp atree, reasso_struct& rrec) {
    if (all_term_based(atree) == true) {
        if (atree.narytyp == NARY) { 
            add_exp (atree, rrec);
        }
    } else {
        recurse_def (atree, rrec);
    }
}

void recurse_def (nary_exp atree, reasso_struct& rrec) {
    if (atree.narytyp == NARY) {
        for(vector<nary_exp>::iterator nait = 
                atree.anary.begin();
             nait != atree.anary.end(); ++nait) {
            make_entry (*nait, rrec);
        }
    }
}

// Iterates over the canonicalized associate trees 
// in reasso_struct and adds term based expressions
// extracted from them to 
// reasso_struct.term_based_explis 
void collect_term_based_expressions (reasso_struct& rrec) {
    vector<nary_exp> naries = rrec.canonicalized_input_trees;
    for (vector<nary_exp>::iterator nait = naries.begin();
         nait != naries.end(); ++nait) {
        make_entry (*nait, rrec);
    }
}

// If a and b are both terms or if a and b are 
// both constants, we check if one is the 
// same as the other. if they are the same 
// we insert either a or b into return vector, 
// depending on which has the smallest count.
// Now two LDIDs can be compared for equal
// by using WN_ver on them. For constants,
// I use two solutions that I picked from RVI 
// pass. If the constant is a INTCONSTANT,
// we can compare the WN_const_val of the two WHIRL nodes.
// If the constant is another kind of constant, 
// we can compare their WN_st.
vector<wcexp> subexp_terms (const wcexp& a, const wcexp& b) {
    vector<wcexp> subt;
    if (((a.wcexptyp == WTERM && b.wcexptyp == WTERM) &&
        (a.aop == OPR_LDID && b.aop == OPR_LDID) &&
        (WN_ver(a.wterm) == WN_ver(b.wterm))) ||
        ((a.wcexptyp == WTERM && b.wcexptyp == WTERM) &&
        (a.aop == OPR_INTCONST && b.aop == OPR_INTCONST) &&
        (WN_const_val(a.wterm) == WN_const_val(b.wterm))) || 
        ((a.wcexptyp == WTERM && b.wcexptyp == WTERM) &&
        (a.aop == OPR_CONST && b.aop == OPR_CONST) &&
        (WN_st(a.wterm) == WN_st(b.wterm)))) {
        if (a.count <= b.count) 
            subt.push_back(a);
        else 
            subt.push_back(b);
    } else if ((a.wcexptyp == WNODE && b.wcexptyp == WNODE) &&
         (a.aop == OPR_NEG && b.aop == OPR_NEG) &&
         (((a.wnode[0].wcexptyp == WTERM && 
                b.wnode[0].wcexptyp == WTERM) &&
         (a.wnode[0].aop == OPR_LDID && 
                b.wnode[0].aop == OPR_LDID) && 
         (WN_ver(a.wnode[0].wterm) == 
               WN_ver(b.wnode[0].wterm))) ||
         ((a.wnode[0].wcexptyp == WTERM && 
           b.wnode[0].wcexptyp == WTERM) &&
         (a.wnode[0].aop == OPR_INTCONST && b.wnode[0].aop == OPR_INTCONST) &&
         (WN_const_val(a.wnode[0].wterm) == WN_const_val(b.wnode[0].wterm))) || 
         ((a.wnode[0].wcexptyp == WTERM && b.wnode[0].wcexptyp == WTERM) &&
         (a.wnode[0].aop == OPR_CONST && b.wnode[0].aop == OPR_CONST) &&
         (WN_st(a.wnode[0].wterm) == WN_st(b.wnode[0].wterm))))) {
        if (a.wnode[0].count <= b.wnode[0].count)
            subt.push_back(a);
        else 
            subt.push_back(b);
    }
    return subt;
}

// Read comments on common_exps and 
// subexp_terms to get an idea of 
// operand_comparisons
void operand_comparisons (
        const wcexp& basewc, const vector<wcexp>& waN1, 
        const vector<wcexp>& waN2, vector<wcexp>& myspli) {
    vector<wcexp>::const_iterator waN1_it;
    vector<wcexp>::const_iterator waN2_it;
    vector<vector<wcexp> > subexpl;
    wcexp anewx(WNODE, basewc.aop, basewc.aopc, 1);
    for (waN1_it = waN1.begin();
         waN1_it != waN1.end();
         ++waN1_it) {
        for (waN2_it = waN2.begin();
             waN2_it != waN2.end(); 
             ++waN2_it) {
            vector<wcexp> splt =
            subexp_terms (*waN1_it,*waN2_it);
            if (splt.size() == 1) {
                anewx.wnode.push_back(splt[0]);
            } else if (splt.size() > 1) {
                const char *myassertval = 
                     "Unexpected size, >1 for results wcexp in operand_comparisons";
                FmtAssert(FALSE, (myassertval));
            }
        }
    }

    if (anewx.wnode.size() > 1) {
        myspli.push_back(anewx);
    }
}

// In order to prove that a nary_exp, c, shares a 
// subexpression with nary_exp, d, we first need 
// to ensure that they are the same 
// type of operations. Then we 
// need to examine every term in c 
// for membership with d. Then we can 
// put the common terms in a new wcexp
// to get the common subexpression between
// c and d.
// This is precisely what common_exps and 
// common_exps_complement do. 
// Each term pair from c and d are compared using 
// operand_comparisons function.
// The only difference between common_exps_complement
// and common_exps is that common_exps_complement
// complements the first argument and then
// checks for subexpressions with the 
// second argument. But common_exps compares them directly. 
// The use of vector<wcexp>& myspli
// can confuse the reader. I actually need a 
// polymorphic holder that can hold 
// either an "empty" value or a 
// single wcexp. In the future I will 
// come up with a template that can 
// serve that purpose. For now, the assertion
// that size() <= 1 guarantee what we what.
void common_exps(const wcexp& c, const wcexp& d, 
        vector<wcexp>& myspli) {
    if (c.wcexptyp == WNODE && d.wcexptyp == WNODE) {
        if ((c.aop == OPR_ADD && d.aop == OPR_ADD) || 
            (c.aop == OPR_MPY && d.aop == OPR_MPY)) {
            vector<wcexp> myp;
            operand_comparisons (c, c.wnode, d.wnode, myp);
            if (myp.size() == 1) {
                myspli = myp;
            } else if (myp.size() > 1) {
                const char *myassertval = 
                    "Unexpected size, >1 for results wcexp in common_exps";
                FmtAssert(FALSE, (myassertval));
            }
        }
    }
}

wcexp complement_wcexp (const wcexp& ay) {
    if (ay.wcexptyp == WTERM) {
        WN* rewn = WN_Unary(OPR_NEG,
                        OPCODE_rtype(ay.aopc), 
                        ay.wterm);
        wcexp retwc(WNODE, WN_operator(rewn), 
                WN_opcode(rewn), ay.count);
        retwc.wterm = rewn;
        retwc.wnode.push_back(ay);
        return retwc;       
    } else {
        if (ay.aop == OPR_NEG) {
            return ay.wnode[0];
        } else {
            if (ay.aop == OPR_ADD) {
                wcexp retwc(WNODE, ay.aop, ay.aopc, ay.count);
                vector<wcexp>::const_iterator rwc_it;
                for (rwc_it = ay.wnode.begin();
                     rwc_it != ay.wnode.end();
                     ++rwc_it) {
                    retwc.wnode.push_back(
                            complement_wcexp(*rwc_it));
                }
                return retwc;
            } else {
                WN* awter = WHIRL_of_wcexp (ay);
                WN* rewn = WN_Unary(OPR_NEG,
                        WN_rtype(awter), awter);
                wcexp retwc(WNODE, WN_operator(rewn),
                        WN_opcode(rewn), ay.count);
                retwc.wnode.push_back(ay);
                retwc.wterm = rewn;
                return retwc;
            }
        }
    }
    FmtAssert(FALSE, ("Unknown NARY type for nary_exp"));
}

void common_exps_complement (const wcexp& c, const wcexp& d, 
        vector<wcexp>& myspli) {
    wcexp newc = complement_wcexp (c);
    if (newc.wcexptyp == WTERM) {
        return;
    } else if ((newc.aop == OPR_NEG) &&
        (newc.wnode[0].wcexptyp == WTERM)) {
        return;
    } else if (newc.wcexptyp == WNODE && 
            d.wcexptyp == WNODE) {
        if ((newc.aop == OPR_ADD && d.aop == OPR_ADD) || 
            (newc.aop == OPR_MPY && d.aop == OPR_MPY)) {
            vector<wcexp> myp;
            operand_comparisons (newc, 
                    newc.wnode, 
                    d.wnode, 
                    myp);
            if (myp.size() == 1) {
                myspli.push_back(myp[0]);
                return;
            } else {
                return;
            }
        } else {
            return;
        }
    } else {
        return;
    }
    FmtAssert(FALSE, ("Unknown input type for common_exps_complement"));
}

// The following functions, find_subexps_for_one_exp
// and find_subexps_for_one_exp_complement
// are used to collect all subexpressions 
// between the first argument and the 
// vector containing term based expressions
// starting from a given index to the end 
// of the vector. They use the common_exps
// function that iteratively a vector containing 
// common terms between the given expression and 
// the currently examined expression in the 
// term expression vector.
void find_subexps_for_one_exp (const wcexp h, 
        const vector<wcexp>& ftvec, 
        int where, vector<wcexp>& myinsw) {
    if (where >= ftvec.size()) {
        return;
    } 
    for (int i = where; i< ftvec.size(); ++i) {
        vector<wcexp> mysplices;
        common_exps (h,ftvec[i], mysplices);
        if (mysplices.size() == 1) {
            myinsw.push_back(mysplices[0]);
        } else  if (mysplices.size() != 0) {
            const char *myassertval = 
            "Unexpected size, >1 for results wcexp in find_subexps_for_one_exp";
            FmtAssert(FALSE, (myassertval));
        }
    }
}

// See comment on find_subexps_for_one_exp
void find_subexps_for_one_exp_complement (const wcexp& h, 
        const vector<wcexp>& ftvec, 
        int where, 
        vector<wcexp>& myinsw) {
    if (where >= ftvec.size()) {
        return;
    } 
    for (int i = where; i< ftvec.size(); ++i) {
        vector<wcexp> mysplices;
        common_exps_complement (h, ftvec[i], mysplices);
        if (mysplices.size() == 1) {
            myinsw.push_back(mysplices[0]);
        } else if (mysplices.size() != 0) {
            const char *myassertval = 
            "Unexpected size, >1 for results wcexp in find_subexps_for_one_exp_complement";
            FmtAssert(FALSE, (myassertval));
        }
    }
}

// Basically, we start from the 0th element
// of term_based_explis, and find if it 
// shares subexpressions with term expressions 
// from 1st element to end of vector. 
// Then we start the same process from 1st element  
// and collect the subexpression that it shares with
// elements at index 2 to the end of the vector.
// This is done via the find_subexps_for_one_exp
// function. There is the possibility that duplicates 
// are collected. For ADD trees, this function also collects 
// expressions which share a negated subexpression 
// containing terms from the start expression.
// This is done via the find_subexps_for_one_exp_complement
// function. As negates in MPYs are fixed via factorize_mul_trees 
// such complemented subexps for MPYs are caught by 
// find_subexps_for_one_exp itself.
// This function defines the algorithmic complexity of 
// this reassociation enabled redundancy elimination. 
void find_subexps_across_exps (const vector<wcexp>& ftvec, 
        vector<vector<wcexp> >& subexpl) {
    for (int i = 0; i < ftvec.size(); ++i) {
        vector<wcexp> myinsw;
        find_subexps_for_one_exp (ftvec[i], 
                ftvec, i+1, myinsw);
        if (myinsw.size() > 0) {
            subexpl.push_back(myinsw);
        }

        if (ftvec[i].aop == OPR_ADD) {
            vector<wcexp> myinsw_c;
            find_subexps_for_one_exp_complement (
                    ftvec[i], ftvec, i+1, myinsw_c);
            if (myinsw_c.size() > 0) {
                subexpl.push_back(myinsw_c);
            }
        }
    }
}

// Once a term_based_explis is available, 
// collect_common_subexpressions is called to 
// collect all subexpressions out of them into 
// reasso_struct::subexpl
void collect_common_subexpressions (reasso_struct& rrec) {
    vector<vector<wcexp> > subexpl;
    find_subexps_across_exps (
            rrec.expressions.factorized_term_explis,
            subexpl);
    if (subexpl.size() > 0) {
        rrec.expressions.subexpl = subexpl;
    }
}

bool contained_in_wcexp (const wcexp& a1, const wcexp& b1) {
    if (a1.wcexptyp == WTERM) {
        return false;
    } else if (a1.wcexptyp == WNODE &&
          a1.aop == OPR_NEG &&
          a1.wnode[0].wcexptyp == WTERM) {
        return false;
    } else if (a1.wcexptyp == WNODE &&
          b1.wcexptyp == WNODE) {
        if (a1.wnode.size() > b1.wnode.size()) {
            return false;
        } else {
            vector<wcexp>::const_iterator a1it;
            vector<wcexp>::const_iterator b1it;
            if ((a1.aop == OPR_ADD &&
                 b1.aop == OPR_ADD) ||
                (a1.aop == OPR_MPY &&
                 b1.aop == OPR_MPY)) {
                for (a1it = a1.wnode.begin();
                     a1it != a1.wnode.end();
                     ++a1it) {
                    bool term_found = false;
                    for (b1it = b1.wnode.begin(); 
                         b1it != b1.wnode.end();
                         ++b1it) {
                        if ((*a1it).count <= (*b1it).count) {
                            wcexp anewal = (*a1it);
                            anewal.count = (*b1it).count;
                            if (anewal == (*b1it)) {
                                term_found = true;
                                break;
                            }
                        }
                    }
                    if (term_found == false) {
                        return false;
                    } else {
                        return true;
                    }
                }
                return true;
            } else {
                return false;
            }
        }
    } else {
        return false;
    }
}

void enumerate_subexps (const vector<wcexp>& aexps,
     vector<int>& costvec) {
    vector<wcexp>::const_iterator aexpsit;
    int which;

    which = 0;
    for (aexpsit = aexps.begin(); 
         aexpsit != aexps.end();
         ++aexpsit) {
        if ((*aexpsit).wcexptyp == WNODE) {
            costvec[which] = 
                costvec[which]*((*aexpsit).wnode.size()-1);
        } else {
            FmtAssert(FALSE, 
                    ("Unexpected type in wcexp in emunerate_subexps"));
        }
        ++which;
    }

    which = 0;
    for (aexpsit = aexps.begin(); 
         aexpsit != aexps.end();
         ++aexpsit) {
        vector<wcexp>::const_iterator tlexpsit;
        int tlwhich = which+1;
        tlexpsit = aexpsit;
        ++tlexpsit;
        for (; tlexpsit != aexps.end();
             ++tlexpsit) {
            if (contained_in_wcexp(*aexpsit,*tlexpsit) 
                    == true) {
                costvec[tlwhich] += costvec[which];
            } else if (contained_in_wcexp(
                       complement_wcexp(*aexpsit),
                       *tlexpsit) == true) {
                costvec[tlwhich] += costvec[which];
            } else if (contained_in_wcexp(*tlexpsit, *aexpsit)
                    == true) {
                costvec[which] += costvec[tlwhich];
            } else if (contained_in_wcexp(
                        complement_wcexp(*tlexpsit),
                        *aexpsit) == true) {
                costvec[which] += costvec[tlwhich];
            }
            ++tlwhich;
        }
        ++which;
    }
}

// Summarize_subexps checks each vector in 
// subexpl and computes a cost vector for each.
void summarize_subexps (const vector<vector<wcexp> >& subexpl,
        vector<vector<int> >& costvecs) {
    vector<vector<wcexp> >::const_iterator subeit;
    int whic = 0;
    for(subeit = subexpl.begin(); 
        subeit != subexpl.end();
        ++subeit) {
        vector<int> mycwhic;
        for (int i = 0; i < (*subeit).size(); ++i) {
            mycwhic.push_back(1);
        }
        enumerate_subexps(*subeit,mycwhic);
        costvecs.push_back(mycwhic);
    }
}

bool contained_in_nary(const nary_exp& what, const nary_exp& where) {
    if (what.aop != where.aop) {
        return false;
    } 

    if (what.aopc != where.aopc) {
        return false;
    }

    if (what.narytyp != where.narytyp) {
        return false;
    }

    if (what.narytyp == AWN || where.narytyp == AWN) {
        return false;
    }

    if (what.anary.size() > where.anary.size()) {
        return false;
    } 
    
    vector<nary_exp>::const_iterator wanit;

    vector<nary_exp> anwhere = where.anary;
    for (wanit = what.anary.begin(); 
         wanit != what.anary.end();
         ++wanit) {
        vector<nary_exp>::iterator whereis = 
            (find(anwhere.begin(), anwhere.end(),
                    *wanit));
        if (whereis == anwhere.end()) {
            return false;
        } 
        anwhere.erase(whereis);
    }
    return true;
}

nary_exp
fixpoint_eliminate_subexp_from_nary (
        const nary_exp& from,
        const nary_exp& what,
        const nary_exp& cseterm, 
        bool& change,
        int itercount) {
    bool reasso_do_the_debug = false;
    if (from.narytyp == AWN) {
        return from;
    } else if (from.narytyp == NARY) {
        if (contained_in_nary(what, from) == true) {
            vector<nary_exp>::const_iterator contains_it;
            vector<nary_exp> nout(from.anary);
            for (contains_it = what.anary.begin();
                 contains_it != what.anary.end();
                 ++contains_it) {
                vector<nary_exp>::iterator whichre = find(
                        nout.begin(), nout.end(), *contains_it);
                if (whichre == nout.end()) {
                    FmtAssert (FALSE, ("Expecting term, but it is not found in nary_exp"));
                }
                nout.erase(whichre); // removes only one
            }
            change = true;
            if (nout.size() == 0) {
                nary_exp anewcseterm(AWN, cseterm.aop, cseterm.aopc);
                anewcseterm.awn = cseterm.awn;
                return anewcseterm;
            } else {
                nary_exp outnary (NARY, from.aop, from.aopc);
                nary_exp anewcseterm(AWN, cseterm.aop, cseterm.aopc);
                anewcseterm.awn = cseterm.awn;
                nout.push_back(anewcseterm);
                outnary.anary = nout;
                return fixpoint_eliminate_subexp_from_nary (
                        outnary, what, cseterm, change,itercount+1);
            }
        } else {
            return from;
        }
    }
    FmtAssert(FALSE, ("Unknown NARY type for nary_exp"));
}

// elimination of a subexpression, arg#2 "nary_exp what",
// in arg#1, "nary_exp from" proceeds by first 
// proving that what is contained in from, 
// and then by using a fixpoint elimination scheme.
// Fixpoint is introduced here as there maybe more 
// than one occurance of what in from, which can 
// happen in some situations.
nary_exp eliminate_subexp_from_nary (
        const nary_exp& from, const nary_exp& what, 
        const nary_exp& cseterm, bool& change) {
    bool reasso_do_the_debug = false;
    if (from.narytyp == AWN) {
        return from;
    } else if (from.narytyp == NARY) {
        if ((from.aop == what.aop) &&
            (contained_in_nary(what, from) == true)) {
            bool othchange = false;
            nary_exp fixsub = 
                fixpoint_eliminate_subexp_from_nary (
                    from, what, cseterm, othchange,0);
            if (othchange == true) {
                change = true;
            }
            if (fixsub.narytyp == AWN) {
                return fixsub;
            } else {
                bool mychange = false;
                vector<nary_exp>::const_iterator rec_sub_it;
                vector<nary_exp> resultvec;
                for (rec_sub_it = fixsub.anary.begin();
                     rec_sub_it != fixsub.anary.end();
                     ++rec_sub_it) {
                    resultvec.push_back (
                            eliminate_subexp_from_nary(
                                *rec_sub_it, what, cseterm, 
                                mychange));
                }
                if (mychange == true) {
                    change = true;
                }
                FmtAssert((resultvec.size() > 0), 
                  ("Nonzero sized nary vector expected after elimination of subexps"));
                nary_exp result (NARY, from.aop, from.aopc);
                result.anary = resultvec;
                return result; 
            }
        } else {
            bool mychange = false;
            vector<nary_exp>::const_iterator rec_sub_it;
            vector<nary_exp> resultvec;
            for (rec_sub_it = from.anary.begin();
                 rec_sub_it != from.anary.end();
                 ++rec_sub_it) {
                resultvec.push_back (
                            eliminate_subexp_from_nary(
                                *rec_sub_it, what, cseterm, 
                                mychange));
            }
            if (mychange == true) {
                change = true;
            }
            FmtAssert((resultvec.size() > 0), 
              ("Nonzero sized nary vector expected after elimination of subexps"));
            nary_exp result (NARY, from.aop, from.aopc);
            result.anary = resultvec;
            return result; 
        }
    }
    FmtAssert(FALSE, ("Unknown NARY type for nary_exp"));
}

// argument #2, called "what" is eliminated from the 
// canonicalized trees in argument#1 reasso_struct& rrec.
// We create a new LDID preg, which
// will replace occurances of "nary_exp what" in all
// of rrec's canonicalized_input_trees. We maintain
// this preg and the replaced tree "nary_exp what", 
// in WHIRL form as a pair which is stored in 
// rrec's excised_subexps_stids vector.
// Later, after do_reassociation puts the 
// changed canonicalized trees back into the basic block, 
// it inserts STIDs before 
// the first LDID occurance of the preg, in the newly written BB's
// WHIRL. For this purpose the excised_subexps_stids is maintained.
void eliminate_subexp (reasso_struct& rrec, 
        nary_exp what, bool& change) {
    vector<nary_exp>::iterator nari_it;
    vector<nary_exp> new_canonicalized_input_trees;
    rrec.cse_start_id = rrec.cse_start_id + 1;
    int ci = 0;
    AUX_STAB_ENTRY *psym;
    ostringstream cseostr;
    cseostr << "__reasso__temp__" << rrec.cse_start_id;
    string csename(cseostr.str());
    what.awn = WHIRL_of_nary_exp(what);
    TYPE_ID arg_type_id = WN_rtype(what.awn);
#ifdef KEY            
    AUX_ID tmp_preg = rrec.optstab->Create_preg(arg_type_id, 
                            csename.c_str());
#else
    AUX_ID tmp_preg = rrec.optstab->Create_preg(arg_type_id);
#endif
    TY_IDX arg_type = ST_type(rrec.optstab->St(tmp_preg));
    WN *ldid = WN_CreateLdid(OPR_LDID, 
                    WN_rtype(what.awn), 
                    WN_rtype(what.awn),
		    rrec.optstab->St_ofst(tmp_preg), 
		    ST_st_idx(rrec.optstab->St(tmp_preg)), 
                    arg_type);
    psym = rrec.optstab->Aux_stab_entry(tmp_preg);
    psym->Set_stack(CXX_NEW(STACK<AUX_ID>(
                    &cse_name_pool), &cse_name_pool));
    VER_ID du = rrec.optstab->Gen_name(tmp_preg);
    WN_set_ver(ldid,rrec.optstab->Get_name(tmp_preg));
    nary_exp cseterm(AWN, WN_operator(ldid), WN_opcode(ldid));
    cseterm.awn = ldid;
    pair<VER_ID, WN*> apair;
    apair.first = WN_ver(ldid);
    apair.second = what.awn;

    ostringstream cse_comp_ostr;
    cse_comp_ostr << "__reasso__temp_comp__" << rrec.cse_start_id;
    string cse_comp_name(cse_comp_ostr.str());
#ifdef KEY
    AUX_ID tmp_preg_comp = rrec.optstab->Create_preg(arg_type_id, 
                            cse_comp_name.c_str());
#else
    AUX_ID tmp_preg_comp = rrec.optstab->Create_preg(arg_type_id);
#endif
    arg_type = ST_type(rrec.optstab->St(tmp_preg_comp));
    WN *ldid_comp = WN_CreateLdid(OPR_LDID, 
                    WN_rtype(what.awn), 
                    WN_rtype(what.awn),
		    rrec.optstab->St_ofst(tmp_preg_comp), 
		    ST_st_idx(rrec.optstab->St(tmp_preg_comp)), 
                    arg_type);
    psym = rrec.optstab->Aux_stab_entry(tmp_preg_comp);
    psym->Set_stack(CXX_NEW(STACK<AUX_ID>(
                    &cse_name_pool), &cse_name_pool));
    du = rrec.optstab->Gen_name(tmp_preg_comp);
    WN_set_ver(ldid_comp,rrec.optstab->Get_name(tmp_preg_comp));

    nary_exp cse_comp_term(AWN, WN_operator(ldid_comp), WN_opcode(ldid_comp));
    cse_comp_term.awn = ldid_comp;
    pair<VER_ID, WN*> apair_comp;
    apair_comp.first = WN_ver(ldid_comp);
    WN* acop = ldid;
    apair_comp.second = fold_neg_WHIRL(
            WN_Unary(OPR_NEG, WN_rtype(ldid), acop));

    bool mychange = false;
    for (nari_it = rrec.canonicalized_input_trees.begin();
         nari_it != rrec.canonicalized_input_trees.end();
         ++nari_it) {
        bool localchange = false; 
        nary_exp newca = eliminate_subexp_from_nary (
                    *nari_it, what, cseterm, localchange);

        if (localchange == true) {
            change = true;
            mychange = true;
            new_canonicalized_input_trees.push_back(newca);
        } else {
            nary_exp newwhat = convert_wcexp_to_nary(
                complement_wcexp(create_wcexp(what)));
            nary_exp newca_comp = eliminate_subexp_from_nary (
                    *nari_it, newwhat, cse_comp_term, localchange);
            if (localchange == true) {
                change = true;
                mychange = true;
            }
            new_canonicalized_input_trees.push_back(newca_comp);
        }
        ++ci;
    }
    if (mychange == true) {
        rrec.excised_subexps_stids_comp.push_back(apair_comp);
        rrec.excised_subexps_stids.push_back(apair);
    } else {
        WN_DELETE_Tree (ldid);
        WN_DELETE_Tree (ldid_comp);
    }
    rrec.canonicalized_input_trees = new_canonicalized_input_trees;
}

// Here from each subexpl vector a CSE with the highest cost is picked. 
// We maintain the selected subexpression in reasso_struct::selected_subexps. 
// Then we eliminate the selected subexpression.
// We only allow a subexpression to be selected if it does not already have
// membership in selected_subexps. Thus we guarantee that 
// a CSE is removed only if it is the hottest of the lot. Also 
// if a selection was already eliminated, we do not look to eliminate it,
// and pick up another subexpression to eliminate.
// elimination is done by using the eliminate_subexp function
// which takes as argument the CSE to be removed, in nary_exp form, which is
// argument #2, called "which" in this function.  
void eliminate_subexps (reasso_struct& rrec, bool& change) {
    vector<vector<wcexp> >::iterator vvwexpit;
    int cost = 0;
    pair<int,int> selected_wcexp;
    selected_wcexp.first = -1;
    selected_wcexp.second = -1;
    for (int outi = 0; 
         outi < rrec.expressions.subexpl.size(); 
         ++outi) {
        for (int ini = 0; 
             ini < rrec.expressions.subexpl[outi].size();
             ++ini) {
            if (rrec.expressions.costvecs[outi][ini] > cost) {
                nary_exp newca = convert_wcexp_to_nary (
                            rrec.expressions.subexpl[outi][ini]);
                if (find (rrec.expressions.selected_subexps.begin(), 
                        rrec.expressions.selected_subexps.end(), newca) 
                        == rrec.expressions.selected_subexps.end()) {
                    cost = rrec.expressions.costvecs[outi][ini];
                    selected_wcexp.first = outi;
                    selected_wcexp.second = ini;
                } 
            }
        }
    }

    if (selected_wcexp.first != -1 
        && selected_wcexp.second != -1) {
        nary_exp which = convert_wcexp_to_nary (
                    rrec.expressions.subexpl
                        [selected_wcexp.first]
                            [selected_wcexp.second]);
        rrec.expressions.selected_subexps.push_back(which);
        rrec.expressions.selected_costs.push_back(
                    rrec.expressions.costvecs
                        [selected_wcexp.first]
                            [selected_wcexp.second]);
        bool localchange = false;
        eliminate_subexp (rrec, which, localchange);
        if (localchange == true) {
            change = localchange;
        }

        if (change == true) {
            rrec.excised_subexps.push_back(which);
            rrec.excised_costs.push_back(
                rrec.expressions.costvecs
                    [selected_wcexp.first]
                        [selected_wcexp.second]);
        }
    }
}

// This function is the backend of this 
// reassociative redundancy removal pass
void do_reassociation(reasso_struct& reas_st, 
        BB_NODE* bb, bool& useful_work) {
    bool reasso_do_the_debug = false;
    // collect term based expressions from 
    // expressions like A*D+B*C. i.e. collects
    // A*D, B*C into reasso_struct.expressions.
    // term_based_explis
    collect_term_based_expressions (reas_st);

    // collect_common_subexpressions from 
    // term_based_explis. Put into reasso_struct.expressions.subexpl
    collect_common_subexpressions (reas_st);

    // Arrange subexps in decreasing order according 
    // to the benefit in removing them
    summarize_subexps (reas_st.expressions.subexpl,
                       reas_st.expressions.costvecs);
    if (Get_Trace( TP_WOPT2, REASSO_DUMP_FLAG_DEBUG)) {
        fprintf (TFile, "cost vector\n");
        for (int i = 0; i < reas_st.expressions.costvecs.size();
                ++i) {
            for (int j = 0; j < reas_st.expressions.costvecs[i].size();
                    ++j) {
                fprintf (TFile, "%d ", reas_st.expressions.costvecs[i][j]);
            }
            fprintf (TFile, "\n");
        }
        fprintf (TFile, "\n");
    }

    // iteratively remove costliest to cheapest subexps
    bool change = false;
    int iter = 0;
    do {
        change = false;
        eliminate_subexps (reas_st, change);
        if (change == true) {
            useful_work = true;
        }
        iter++;
    } while (change == true);

    // if there were any subexpression elimination,
    // useful_work will be set to true
    if (useful_work == true) {
        // Change the kid of associative expression 
        // hosting STIDs. Use the newly derived
        // canonical trees instead of what was there.
       for (int ci = 0; 
             ci < reas_st.canonicalized_input_trees.size();
             ++ci) {
            WN* in_parent = reas_st.map_stm_wn[ci];
            WN_kid0(in_parent) = WHIRL_of_nary_exp (
                    reas_st.canonicalized_input_trees[ci]);
       }

        // Rewrite original basic block by including stids for each 
        // cse ldid pregs. Each cse uses a new ldid 
        // preg. These pregs are stored along with the 
        // cse strand as pairs of <VER_ID,WN*> in
        // excised_subexps_stids and excised_subexps_stids_comp.
        // It is necessary that we first iterate over 
        // excised_subexps_stids_comp and then
        // excised_subexps_stids.
        for (vector<pair<VER_ID,WN*> >::iterator mymait = 
                reas_st.excised_subexps_stids_comp.begin();
             mymait != reas_st.excised_subexps_stids_comp.end();
             ++mymait) {
            STMT_ITER stmt_iter;
            WN *wn = NULL;
            FOR_ALL_ELEM (wn, stmt_iter,
                    Init(bb->Firststmt(), bb->Laststmt())) {
                if (WHIRL_has_term(wn, (*mymait).first)
                        == true) {
                    WN* awn = (*mymait).second;
                    WN * theldid = WHIRL_get_term (wn, 
                            (*mymait).first);
                    TYPE_ID arg_type_id = OPCODE_desc(WN_opcode(theldid));
                    AUX_ID tmp_preg = reas_st.optstab->Ver_stab_entry(
                            WN_ver(theldid))->Aux_id();
                    WN *stid = WN_CreateStid(OPR_STID, MTYPE_V,
                            arg_type_id, reas_st.optstab->St_ofst(tmp_preg), 
			    ST_st_idx(reas_st.optstab->St(tmp_preg)),
			    Be_Type_Tbl(arg_type_id), awn);
                    WN_set_ver(stid, WN_ver(theldid));

                    if (reas_st.optstab->Ver_stab_entry(WN_ver(wn))->Real_use()) {
                        reas_st.optstab->Ver_stab_entry(WN_ver(stid))->Set_Real_use();
                    } 

                    if (reas_st.optstab->Ver_stab_entry(WN_ver(wn))->Any_use()) {
                        reas_st.optstab->Ver_stab_entry(WN_ver(stid))->Set_Any_use();
                    }

                    if (reas_st.optstab->Ver_stab_entry(WN_ver(wn))->Zero_vers()) {
                        reas_st.optstab->Ver_stab_entry(WN_ver(stid))->Set_Zero_vers();
                    }
                    bb->Insert_wn_before(stid, wn);
                    goto there1;
                }
            }
there1:  ;      
        }
        for (vector<pair<VER_ID,WN*> >::iterator mymait = 
                reas_st.excised_subexps_stids.begin();
             mymait != reas_st.excised_subexps_stids.end();
             ++mymait) {
            STMT_ITER stmt_iter;
            WN *wn = NULL;
            FOR_ALL_ELEM (wn, stmt_iter,
                    Init(bb->Firststmt(), bb->Laststmt())) {
                if (WHIRL_has_term(wn, (*mymait).first)
                        == true) {
                    WN* awn = (*mymait).second; 
                    WN * theldid = WHIRL_get_term (wn, 
                            (*mymait).first);
                    TYPE_ID arg_type_id = WN_rtype(theldid);
                    AUX_ID tmp_preg 
                        = reas_st.optstab->Ver_stab_entry(WN_ver(theldid))->Aux_id();
                    WN *stid = WN_CreateStid(OPR_STID, MTYPE_V,
                            arg_type_id, reas_st.optstab->St_ofst(tmp_preg), 
			    ST_st_idx(reas_st.optstab->St(tmp_preg)),
			    Be_Type_Tbl(arg_type_id), awn);
                    WN_set_ver(stid, WN_ver(theldid));

                    if (reas_st.optstab->Ver_stab_entry(WN_ver(wn))->Real_use()) {
                        reas_st.optstab->Ver_stab_entry(WN_ver(stid))->Set_Real_use();
                    } 

                    if (reas_st.optstab->Ver_stab_entry(WN_ver(wn))->Any_use()) {
                        reas_st.optstab->Ver_stab_entry(WN_ver(stid))->Set_Any_use();
                    }

                    if (reas_st.optstab->Ver_stab_entry(WN_ver(wn))->Zero_vers()) {
                        reas_st.optstab->Ver_stab_entry(WN_ver(stid))->Set_Zero_vers();
                    }

                    bb->Insert_wn_before(stid, wn);
                    goto there;
                }
            }
there:   ;         
        }
    }
}

// helper function used by WHIRL_of_nary_exp
WN* WHIRL_of_nary_exp_vector (
        OPERATOR yaop,
        const vector<nary_exp>& nary_exp_vec, int where) {
    int length = nary_exp_vec.size();
    if (where < 1 || where >= length) {
        FmtAssert (FALSE, 
            ("Unexpected size of nary_vector in WHIRL_of_nary_exp_vector"));
    }

    if (where == length-1) {
        return WHIRL_of_nary_exp(nary_exp_vec[where]);
    } else {
        WN *outnod = WN_Binary(
                        yaop,
                        (OPCODE_rtype(
                                nary_exp_vec[where].aopc)),
                        WHIRL_of_nary_exp(nary_exp_vec[where]),
                        WHIRL_of_nary_exp_vector(
                            yaop,
                            nary_exp_vec, 
                            where+1));
        return fold_neg_WHIRL(outnod);
    }
}

// Converts a nary_exp to WN*.  Reintroduces
// SUBs into the code that were converted into
// ADD-NEG form due to canonicalization.
// 
WN* WHIRL_of_nary_exp (const nary_exp& anary_exp) {
    nary_assert(anary_exp);
    if (anary_exp.narytyp == AWN) {
        return anary_exp.awn;
    } else {
        if (anary_exp.aop == OPR_NEG) {
            WN* anunwn = WN_Unary(
                         OPR_NEG, 
                         OPCODE_rtype(anary_exp.aopc),
                         WHIRL_of_nary_exp(anary_exp.anary[0]));
                return anunwn;
        } else if (anary_exp.anary.size() > 1) {
            if (anary_exp.aop == OPR_MPY) {
                WN * anmulwn = WN_Binary(OPR_MPY,
                            OPCODE_rtype(anary_exp.aopc),
                            WHIRL_of_nary_exp(anary_exp.anary[0]),
                            WHIRL_of_nary_exp_vector(
                                OPR_MPY,
                                anary_exp.anary, 1)); 
                return anmulwn;
            } else if (anary_exp.aop == OPR_ADD) {
                WN * anaddwn = WN_Binary(OPR_ADD,
                            OPCODE_rtype(anary_exp.aopc),
                            WHIRL_of_nary_exp(anary_exp.anary[0]),
                            WHIRL_of_nary_exp_vector(
                                OPR_ADD,
                                anary_exp.anary, 1)); 
                return fold_neg_WHIRL(anaddwn);
            }
        }
        FmtAssert (FALSE, 
            ("Unexpected nary_exp in WHIRL_of_nary_exp"));
    }
}

nary_exp make_nary_term (WN* ald) {
    nary_exp myret(AWN, WN_operator(ald), WN_opcode(ald));
    myret.awn = ald;
    return myret;
}

// This function is almost like its sister function 
// prod_to_nary, however, it differs in where we go
// the first time we enter this function. 
// We enter WN_operator(atree) == OPR_ADD condition
// for sum_to_nary, at the first time. But we 
// entered WN_operator(atree) == OPR_MPY. 
// Code is identical except for this difference. 
void sum_to_nary (WN* atree, 
        vector<nary_exp>& outli) {
    if ((WN_operator(atree) == OPR_LDID) || 
        (WN_operator(atree) == OPR_CONST)) {
        outli.push_back(make_nary_term(atree));
        return;
    } else if (WN_operator(atree) == OPR_NEG) {
        outli.push_back(binary_to_nary(atree));
        return;
    } else if (WN_operator(atree) == OPR_ADD) {
        // real work here
        sum_to_nary(WN_kid0(atree),outli);
        sum_to_nary(WN_kid1(atree),outli);
        return;
    } else if (WN_operator(atree) == OPR_MPY) {
        outli.push_back(binary_to_nary(atree));
        return;
    } else {
        FmtAssert (FALSE, 
            ("Unexpected WHIRL in sum_to_nary"));
    }
}

// This is a recursive function that is first
// entered with a OPR_MPY WHIRL tree. Then it calls itself
// on the OPR_MPY's kid0 and dispatches kid0 for nary_exp 
// creation of kid0 via binary_to_nary. 
// Then it calls itself with the OPR_MPY's
// kid1 and dispatches kid1 for nary_exp creation 
// via binary_to_nary. 
void prod_to_nary(WN* atree, 
        vector<nary_exp>& outli) {
    if ((WN_operator(atree) == OPR_LDID) || 
        (WN_operator(atree) == OPR_CONST)) {
        outli.push_back(make_nary_term(atree));
        return;
    } else if (WN_operator(atree) == OPR_NEG) {
        outli.push_back(binary_to_nary(atree));
        return;
    } else if (WN_operator(atree) == OPR_MPY) {
        // real work here
        prod_to_nary(WN_kid0(atree),outli);
        prod_to_nary(WN_kid1(atree),outli);
        return;
    } else if (WN_operator(atree) == OPR_ADD) {
        outli.push_back(binary_to_nary(atree));
        return;
    } else {
        FmtAssert (FALSE, 
            ("Unexpected WHIRL in prod_to_nary"));
    }
}

// Top level dispatcher for converting 
// a WHIRL tree into nary_exp.
// If the operation in the argument 
// tree is an OPR_MPY, this launches
// prod_to_nary with this argument.
// Else if the the argument is an OPR_ADD,
// this launches sum_to_nary with this 
// argument. Else it figures out that the 
// argument is a term
// and returns a nary_exp terminal that 
// contains the argument.
// The name binary_to_nary was a bad 
// choice as this also dispatches unary
// trees, (near future work to refactor function name).
nary_exp 
binary_to_nary (WN* atree) {
    if(WN_operator(atree) == OPR_MPY) {
        // convert Binop(MPY
        nary_exp myret (NARY, WN_operator(atree), 
                WN_opcode(atree));
        prod_to_nary(atree, myret.anary); 
        return myret;
    } else if (WN_operator(atree) == OPR_ADD) {
        // convert Binop(ADD
        nary_exp myret (NARY, 
                WN_operator(atree), 
                WN_opcode(atree));
        sum_to_nary(atree, myret.anary); 
        return myret;
    } else if (WN_operator(atree) == OPR_NEG) {
        // convert Unaryop(NEG
        nary_exp myret(NARY, 
                WN_operator(atree), 
                WN_opcode(atree));
        myret.anary.push_back(
                binary_to_nary(WN_kid0(atree)));
        return myret;
    } else if ((WN_operator(atree) == OPR_LDID) ||
         (WN_operator(atree) == OPR_CONST)) {
        nary_exp myret = make_nary_term(atree);
        return myret;
    }
    FmtAssert (FALSE, 
        ("Unexpected WHIRL in binary_to_nary"));
}

// Given a WHIRL tree, we obtain the corresponding 
// nary_exp using this driver function, make_nary_tree.
// Ensure that the tree is an associative WHIRL tree 
// using the is_associate_tree function that operates 
// on WHIRL. Our formulation only includes ADDs and MPYs
// as operations that are associative too. 
// There are others too, for example, ==, MAX, MIN etc.
// 
nary_exp 
make_nary_tree (WN* atree) {
    return binary_to_nary(atree);
}

// utility function used only in factorize_mul_trees (in 
// this work)
int count_negates (const vector<nary_exp>& anary_vec) {
    int ret = 0;
    for (vector<nary_exp>::const_iterator vit = 
            anary_vec.begin();
         vit != anary_vec.end(); ++vit) {
        nary_exp vitex = *vit;
        if (vitex.narytyp == NARY 
                && vitex.aop == OPR_NEG) {
            ++ret;
        }
    }
    return ret;
}

// utility function used only in factorize_mul_trees (in 
// this work)
vector<nary_exp> clear_signs (const vector<nary_exp>& anaryv) {
    vector<nary_exp>::const_iterator vit;
    vector<nary_exp> outvec;
    for (vit = anaryv.begin();
        vit != anaryv.end();
        ++vit) {
        nary_exp vitexp = *vit;
        if (vitexp.aop == OPR_NEG) {
            outvec.push_back(vitexp.anary[0]);
        } else {
            outvec.push_back(vitexp);
        }
    }
    return outvec;
}

// utility function used only in factorize_mul_trees (in 
// this work)
nary_exp
unary_sub_fold (const nary_exp& mytree) {
    if(mytree.narytyp == NARY) {
        if (mytree.aop == OPR_NEG) {
            nary_exp neged = mytree.anary[0];
            if (neged.narytyp == NARY) {
                if (neged.aop == OPR_NEG) {
                    nary_exp outexp =  
                        unary_sub_fold (neged.anary[0]);
                    return outexp;
                }
            }
        }
    }

    return mytree;
}

// factorize_mul_trees is a recursive function that 
// optimizes NEGs in MPY nary expressions. 
// for example: (-A)*(-B) = A*B
// (-A)*(-B)*(-C) = -(A*B)
nary_exp 
factorize_mul_trees (nary_exp atree) {
    if(atree.narytyp == AWN) {
        return atree;
    } else {
        if (atree.aop == OPR_NEG) {
            nary_exp outexp = unary_sub_fold (atree);
            return outexp;
        } else if (atree.aop == OPR_MPY) {
            int negatives = count_negates (atree.anary);
            if (negatives % 2 == 0) {
                // even
                nary_exp outnary(NARY, atree.aop,
                        atree.aopc);
                outnary.anary = clear_signs (atree.anary);
                return outnary;
            } else {
                // odd

                nary_exp outnary(NARY, atree.aop,
                        atree.aopc);
                
                outnary.anary= clear_signs(atree.anary);

                OPCODE aopC =  OPCODE_make_op (
                        OPR_NEG, OPCODE_rtype(atree.aopc),
                        OPCODE_desc(atree.aopc));
                nary_exp neged_nary(NARY, OPR_NEG, aopC);
                neged_nary.anary.push_back(outnary); 
                return neged_nary;
            }
        } else if (atree.aop == OPR_ADD) {
            vector<nary_exp>::const_iterator vit; 
            vector<nary_exp> outvec;
            for (vit = atree.anary.begin(); 
                vit != atree.anary.end(); 
                ++vit) {
                outvec.push_back(
                        factorize_mul_trees(
                            *vit));
            }
            nary_exp outnary(NARY, atree.aop,
                    atree.aopc);
            outnary.anary = outvec;
            return outnary;
        }
        FmtAssert (FALSE, ("Unexpected nary_typ in factorize_mul_trees"));
    }
}

// Given a vector of nary expressions, 
// oplis, extract non-terminal nary-exps in oplis
// into nterms, and extract terminal nary-exps into terms.
// The definition of terminal nary-expression is 
// expanded to include negates of terms. For example,
// -A is also considered to be a terminal, 
// if A is a nary-exp terminal.
void extract_term_based (
        const vector<nary_exp>& oplis,
        vector<nary_exp>& nterms,
        vector<nary_exp>& terms) {
    vector<nary_exp>::const_iterator opl_it;
    for(opl_it = oplis.begin();
        opl_it != oplis.end();
        ++opl_it) {
        nary_exp anaryexp = *opl_it;
        if (anaryexp.narytyp == AWN) {
            terms.push_back(anaryexp);
        } else if (anaryexp.narytyp == NARY) {
            if (anaryexp.aop == OPR_NEG && 
                anaryexp.anary[0].narytyp == AWN) {
                terms.push_back(anaryexp);
            } else {
                nterms.push_back(anaryexp);
            }
        }
    }
}

// Mutually recursive functions, reorg_vec and
// group_terms_nonterms accomplish the grouping
// need, where we work to synthesize term only 
// nary expressions out of the input nary expression, 
// and rewrite the input nary expression to contain
// newly created term only nary expression.
nary_exp group_terms_nonterms (const nary_exp& atree);
vector<nary_exp> reorg_vec (const vector<nary_exp>& anterms) {
    vector<nary_exp> outvec;
    vector<nary_exp>::const_iterator anterm_it;
    for (anterm_it = anterms.begin();
        anterm_it != anterms.end();
        ++anterm_it) {
        nary_exp outnary = 
            group_terms_nonterms(*anterm_it);
        
        outvec.push_back(outnary);
    }
    return outvec;
}

// See comments related to reorg_vec for help
// on this function.
nary_exp group_terms_nonterms (const nary_exp& atree) {
    if (atree.narytyp == AWN) {
        return atree;
    } else {
        int invsize = atree.anary.size();
        if ((atree.aop == OPR_NEG) 
             && (atree.anary[0].narytyp == AWN)) {
            return atree;
        } else {
            vector<nary_exp> outvec;
            vector<nary_exp> terms;
            vector<nary_exp> nterms;
            extract_term_based (atree.anary, 
                    nterms, terms);
            nterms = reorg_vec(nterms);
            int numn = nterms.size();
            int numt = terms.size();
            if (numn < 1) {
                return atree;   
            } else {
                if (numt < 2) {
                    return atree;
                } else {
                    nary_exp term_exp (NARY, atree.aop, 
                            atree.aopc, atree.awn, terms);
                    nary_exp nterm_exp(NARY, atree.aop, 
                            atree.aopc, atree.awn, nterms);
                    nterm_exp.anary.push_back(term_exp);
                    return nterm_exp;
                }
            }
        }
    }
}

void find_terms (const nary_exp& atree, list<nary_exp>& trec) {
    if (atree.narytyp == AWN) {
        if (WN_operator(atree.awn) == OPR_LDID) {
            if (find(trec.begin(), trec.end(), atree) 
                    == trec.end()) {
                trec.push_back(atree);
            }
        } else if (WN_operator(atree.awn) == OPR_CONST) {
            if (find(trec.begin(), trec.end(), atree) 
                    == trec.end()) {
                trec.push_back(atree);
            }
        } else {
            FmtAssert (FALSE, ("Unexpected term type in nary_exp"));
        }
    } else {
        for (vector<nary_exp>::const_iterator vit = 
                atree.anary.begin();
             vit != atree.anary.end(); ++vit) {   
            find_terms(*vit, trec);
        }
    }
}

bool 
contains_volatile_term (OPT_STAB* opt_stab, WN* in_tree) {
    // we find volatile terms recursively
    switch(WN_operator(in_tree)) {
        case OPR_ADD:
        case OPR_SUB:
        case OPR_MPY:
            return 
                (contains_volatile_term (opt_stab, WN_kid0(in_tree)) || 
                 contains_volatile_term (opt_stab, WN_kid1(in_tree)));
        case OPR_NEG:
            return 
                contains_volatile_term (opt_stab, WN_kid0(in_tree)); 
        case OPR_LDID:
            {
                if (opt_stab->Du_is_volatile(WN_ver(in_tree))) 
                    return true;
                else 
                    return false;
            }
        case OPR_CONST: 
            return false;
        default:
            return false;
    }
    FmtAssert (FALSE, 
        ("Unexpected transfer of control in contains_volatile_term"));
}

bool 
is_associate_tree (WN* in_tree) {
    // we also need to check if 
    // types are same all over.
    switch(WN_operator(in_tree)) {
        case OPR_ADD:
        case OPR_SUB:
        case OPR_MPY:
            return 
                (is_associate_tree(WN_kid0(in_tree)) && 
                 is_associate_tree(WN_kid1(in_tree)));
        case OPR_NEG:
            return 
                is_associate_tree(WN_kid0(in_tree)); 
        case OPR_LDID:
        case OPR_CONST: 
            return true;
        default:
            return false;
    }
    FmtAssert (FALSE, 
        ("Unexpected transfer of control in is_associate_tree"));
}

WN* fold_neg_WHIRL (WN* intre) {
    // This is mainly for undoing 
    // bad effects of canonicalization. As a result of 
    // canonicalization, A-B becomes A+ (-B). 
    // We fold ADD with a NEGs into SUBs
    // to avoid performance penalty.
    // Assume that (-A)+ (-B) is input.
    // both inputs are OPR_NEGs:
    // (orkid0,orkid1) = A,B become
    // A+B and negate becomes -(A+B).
    // if (-A)+B is input.
    // Thus orkid0,orkid1 = A, B
    // and result is B-A
    // if (A) + (-B) is input,
    // the result is A-B
    // First contention is that the 
    // SUB cases are irrelevant in here 
    // Need to revisit this here.
    // OK, we are mostly calling this function during nary to 
    // WHIRL. As we build trees from each nary leaf,
    // we are automatically folding all the way to the top.
    // Thus, the recursive calls are not required for 
    // orkid0 and orkid1... local decisions 
    // away to the top. However, they are anyway 
    // enabled. 
    if (WN_operator(intre) == OPR_ADD) {
        if ((WN_operator(WN_kid0(intre)) == OPR_NEG)
            && (WN_operator(WN_kid1(intre)) == OPR_NEG)) {
            // (-A)+(-B)
            WN* orkid0 = WN_kid0(WN_kid0(intre)); // A
            WN* orkid1 = WN_kid0(WN_kid1(intre)); // B
            orkid0 = fold_neg_WHIRL(orkid0);
            orkid1 = fold_neg_WHIRL(orkid1);
            WN* rettr = WN_Binary(
                            OPR_ADD, 
                            WN_rtype(intre),
                            orkid0,
                            orkid1); // A+B
            rettr = fold_neg_WHIRL(rettr); // simplify A+B
            rettr = WN_Unary(
                    OPR_NEG,
                    WN_rtype(intre),
                    rettr); // -(A+B)
            return rettr;
        } else if (WN_operator(WN_kid0(intre)) == OPR_NEG) {
            // (-A) + B
            WN* orkid0 = WN_kid0(WN_kid0(intre)); // A
            WN* orkid1 = WN_kid1(intre); // B
            orkid0 = fold_neg_WHIRL(orkid0);
            orkid1 = fold_neg_WHIRL(orkid1);
            WN* rettr = WN_Binary(
                            OPR_SUB, 
                            WN_rtype(intre),
                            orkid1,
                            orkid0); // B - A
            return fold_neg_WHIRL(rettr);
        } else if (WN_operator(WN_kid1(intre)) == OPR_NEG) {
            // A + (-B)
            WN* orkid0 = WN_kid0(intre); // A
            WN* orkid1 = WN_kid0(WN_kid1(intre)); // B
            orkid0 = fold_neg_WHIRL(orkid0);
            orkid1 = fold_neg_WHIRL(orkid1);
            WN* rettr = WN_Binary(
                            OPR_SUB, 
                            WN_rtype(intre),
                            orkid0,
                            orkid1); // A - B
            return fold_neg_WHIRL(rettr); 
        } else {
            return intre;
        }
    } else if (WN_operator(intre) == OPR_SUB) {
        if (WN_operator(WN_kid1(intre)) == OPR_NEG) {
            //  - (-B) 
            WN* orkid0 = WN_kid0(intre); // Dont care
            WN* orkid1 = WN_kid0(WN_kid1(intre)); // B
            orkid0 = fold_neg_WHIRL(orkid0);
            orkid1 = fold_neg_WHIRL(orkid1);
            WN* rettr = WN_Binary(
                            OPR_ADD,
                            WN_rtype(intre),
                            orkid0,
                            orkid1); // Dont care + B
            return fold_neg_WHIRL(rettr);
        } else if (WN_operator(WN_kid0(intre)) == OPR_NEG) {
            // (-A) - 
            // No reason to believe that this 
            // actually simplifies a given expression any further
            WN* orkid0 = WN_kid0(WN_kid0(intre)); // A
            WN* orkid1 = WN_kid1(intre); // Dont care
            orkid0 = fold_neg_WHIRL(orkid0);
            orkid1 = fold_neg_WHIRL(orkid1);
            WN* rettr = WN_Binary(
                            OPR_ADD, 
                            WN_rtype(intre),
                            orkid0,
                            orkid1); // A + Dont care
            rettr = fold_neg_WHIRL(rettr);
            rettr = WN_Unary(
                    OPR_NEG,
                    WN_rtype(intre),
                    rettr);  // - (A + Dont care)
            return rettr;
        } else {
            return intre;
        }
    } else {
        return intre;
    }
}

WN* fold_tree (WN *mytree) {
    // During canonicalization of A-B into A+ (-B),
    // it is possible that we end up with 
    // NEG(NEG(... terms like -(-B) and so on.
    // fold_tree folds out such code.
    // fold tree also drives canonicalization 
    // for OPR_NEG based trees by dispatching its 
    // kid with canon_sub_fn.
    if (WN_operator(mytree) == OPR_LDID) {
        return mytree;
    } else if (WN_operator(mytree) == OPR_NEG ) {
        WN* wnk0 = WN_kid0(mytree);
        if (WN_operator(wnk0)== OPR_NEG) {
            WN* outtr = fold_tree (
                    canon_sub_fn (WN_kid0(wnk0)));
            return outtr;
        } else if (WN_operator(wnk0) == OPR_SUB) {
            WN* wnk0k0 = WN_kid0(wnk0);
            WN* wnk0k1 = WN_kid1(wnk0);
            WN* myswapforsubs = 
                WN_Binary(
                     OPR_ADD, 
                     WN_rtype(wnk0),
                     fold_tree(
                          WN_Unary(
                               OPR_NEG, 
                               WN_rtype(wnk0k0),
                               fold_tree (
                                   canon_sub_fn (wnk0k0)))),
                     fold_tree( canon_sub_fn(wnk0k1))); 
            return myswapforsubs;
        } else if (WN_operator(wnk0) == OPR_ADD) {
            WN* wnk0k0 = WN_kid0(wnk0);
            WN* wnk0k1 = WN_kid1(wnk0);
            WN *myaddfold = 
                WN_Binary(
                    OPR_ADD, 
                    WN_rtype(wnk0),
                    fold_tree(
                        WN_Unary(
                             OPR_NEG,
                             WN_rtype(wnk0k0),
                             fold_tree (canon_sub_fn (wnk0k0)))),
                    fold_tree(
                        WN_Unary(
                             OPR_NEG,
                             WN_rtype(wnk0k1),
                             fold_tree (canon_sub_fn (wnk0k1)))));
            return myaddfold;
        } else if (WN_operator(wnk0) == OPR_MPY) {
            WN* wnk0k0 = WN_kid0(wnk0);
            WN* wnk0k1 = WN_kid1(wnk0);
            WN *mymulfold = 
                WN_Unary(
                  OPR_NEG, 
                  WN_rtype(wnk0),
                  fold_tree(
                     WN_Binary(
                         OPR_MPY, 
                         WN_rtype(wnk0),
                         fold_tree (canon_sub_fn (wnk0k0)),
                         fold_tree (canon_sub_fn (wnk0k1)))));
            return mymulfold;
        } else {
            return mytree;
        } // if (WN_operator(wnk0)== OPR_NEG) 
    } //else if (WN_operator(mytree) == OPR_NEG ) 
    else {
        return mytree;
    }
}

WN* canon_subtract_tree (WN* mytree) {
    if (WN_operator(mytree) == OPR_SUB) {
        // A-B = A + (-B)
        WN* wnl = canon_sub_fn (WN_kid0(mytree));
        WN* wnr = canon_sub_fn (
                    WN_Unary(
                        OPR_NEG, 
                        WN_rtype(WN_kid1(mytree)),
                        WN_kid1(mytree)));
        WN* aftercano = WN_Binary(
                            OPR_ADD, 
                            WN_rtype(mytree),
                            wnl,
                            wnr);
        return aftercano;
    } // if(WN_operator(mytree) == OPR_SUB) 
    else if (WN_operator(mytree) == OPR_ADD) {
        WN* wnl = WN_kid0(mytree);
        wnl = canon_sub_fn (WN_Unary(
                                OPR_NEG,
                                WN_rtype(wnl),
                                wnl));
        WN* wnr = WN_kid1(mytree);
        wnr = canon_sub_fn (WN_Unary(
                                OPR_NEG,
                                WN_rtype(wnr),
                                wnr));
        WN* aftercano = WN_Binary(
                            OPR_ADD,
                            WN_rtype(mytree),
                            wnl,
                            wnr);
        return aftercano;
    }// else if (WN_operator(mytree) == OPR_ADD) 
    else if (WN_operator(mytree) == OPR_MPY) {
        WN *aftercano = 
            WN_Unary (
                 OPR_NEG,
                 WN_rtype(mytree),
                 WN_Binary (
                      OPR_MPY,
                      WN_rtype(mytree),
                      canon_sub_fn(WN_kid0(mytree)),
                      canon_sub_fn(WN_kid1(mytree))));
        return aftercano;
    } else {
        return mytree;
    }
}

WN* canon_sub_fn (WN *mytree) {
    WN* myfold = fold_tree (mytree);
    if (WN_operator(mytree) == OPR_LDID) {
        return myfold;
    } else if (WN_operator(mytree) == OPR_NEG) {
        return myfold;
    } else if (WN_operator(mytree) == OPR_SUB) {
        WN* canon_sub_whi = canon_subtract_tree (myfold);
        return canon_sub_whi;
    } else if ((WN_operator(mytree) == OPR_ADD) || 
            (WN_operator(mytree) == OPR_MPY)) {
        WN *canon_rest = WN_Binary(
                            WN_operator(mytree), 
                            WN_rtype(mytree),
                            canon_sub_fn(WN_kid0(mytree)),
                            canon_sub_fn(WN_kid1(mytree)));
        return canon_rest;
    } else {
        return myfold;
    }
}

WN* 
build_associate_trees_collect_terms (
        OPT_STAB* opt_stab,
        WN* in_parent,
        WN* in_tree, 
        set<VER_ID>& termset, 
        vector<nary_exp>& assovec,
        map<int,WN*>& map_stm_wn) {
    if(is_associate_tree(in_tree) == true) {
        if (contains_volatile_term (opt_stab, in_tree) == true) {
            // we do not reassociate trees that contain
            // volatile terms in them
            return in_tree;
        }
        
        if (WN_operator(in_tree) != OPR_LDID
                && WN_operator(in_tree) != OPR_CONST) {
            WN* ctree;
            // There are two parts to reassociative 
            // redundancy elimination.  
            // This is the canonicalization stage
            // or the "front-end".
            // Here we collect associative trees for 
            // processing by the "backend".
            // The backend is do_reassociation, which 
            // does the main work.
            // During canonicalization, input 
            // goes from WHIRL to WHIRL to nary_exp.
            // We first convert SUB expressions into 
            // ADD and NEG based form. The
            // canon_sub_fn function takes a WHIRL 
            // returns another WHIRL. 
            // For example, 
            // A - B  is converted to A + (-B).
            ctree = canon_sub_fn(in_tree);

            // the rewritten WHIRL is converted to the 
            // nary_exp form.
            nary_exp mynary = make_nary_tree(ctree);

            // factorize_mul_trees optimizes NEGs in MUL nary expressions. 
            // for example: (-A)*(-B) = A*B
            // (-A)*(-B)*(-C) = -(A*B)
            nary_exp myfact = factorize_mul_trees(mynary);

            // group_terms_nonterms rewrites nary expressions
            // that contain terms (LDIDs, CONSTs) and nonterms
            // (other nary expressions). It mainly replaces all terms 
            // from an nary expression with "new term only" nary 
            // expression. For example: an nary ADD expression 
            // A+B+C*D+E*F, which contains the following 4 kids:
            // {A,B,C*D,E*F} will become (A+B)+C*D+E*F which is
            // an nary ADD expression that contains 3 kids, viz.,
            // {A+B, D*E, E*F}
            // If narys are written like this, harnessing term 
            // only expressions from a given nary tree is easier.
            nary_exp mygrouped = group_terms_nonterms(myfact);
            list<nary_exp> terms;
            find_terms (mygrouped, terms);
            for (list<nary_exp>::iterator 
             snait = terms.begin(); snait != terms.end(); ++snait) {
                // termset is used in the main routine
                // toplvl_bb
                if (WN_operator ((*snait).awn) == OPR_LDID) {
                    termset.insert(WN_ver((*snait).awn));
                }
            }
            // push canonicalized expressions into 
            // assovec for processing by do_reassociation
            assovec.push_back(mygrouped);
            WN* wn_nary = WHIRL_of_nary_exp(mygrouped);
            map_stm_wn[assovec.size()-1] = in_parent;
            // we return the WHIRL of the canonicalized
            // expression as output. This can be used by toplvl_bb 
            // to replace the original associative tree in the BB.  
            // This helps when we are debugging the front-end alone.
            return wn_nary;
        } else {
            return in_tree;
        }
    } else {
        return in_tree;
    }
}

bool WHIRL_has_term (WN* intre, VER_ID astidx) {
    // finds out if the given intre contains the
    // astidx
    if (is_associate_tree (intre) == true) {
        if ( WN_operator(intre) == OPR_ADD || 
            WN_operator(intre) == OPR_SUB ||     
            WN_operator(intre) == OPR_MPY ) {
            return WHIRL_has_term (WN_kid0(intre), astidx) ||
                WHIRL_has_term (WN_kid1(intre), astidx);
        } else if (WN_operator(intre) == OPR_NEG) {
            return WHIRL_has_term (WN_kid0(intre), astidx);
        } else if (WN_operator(intre) == OPR_LDID) {
            if (WN_ver(intre) == astidx) {
                return true;
            } else {
                return false;
            }
        } else {
            return false;
        }
    } else {
        if (WN_operator(intre) == OPR_STID || 
            WN_operator(intre) == OPR_ISTORE) {
            return WHIRL_has_term(WN_kid0(intre), astidx);
        } else {
            return false;
        }
    }
}

WN* WHIRL_get_term (WN* intre, VER_ID astidx) {
    // utility function that finds the first LDID 
    // that contains the astidx
    if (is_associate_tree (intre) == true) {
        if ( WN_operator(intre) == OPR_ADD || 
            WN_operator(intre) == OPR_SUB ||     
            WN_operator(intre) == OPR_MPY ) {
            WN* ret1 = WHIRL_get_term (WN_kid0(intre), astidx);
            if (ret1 == NULL) {
                return WHIRL_get_term (WN_kid1(intre), astidx);
            } else {
                return ret1;
            }
        } else if (WN_operator(intre) == OPR_NEG) {
            return WHIRL_get_term (WN_kid0(intre), astidx);
        } else if (WN_operator(intre) == OPR_LDID) {
            if (WN_ver(intre) == astidx) {
                return intre;
            } else {
                return NULL;
            }
        } else {
            return NULL;
        }
    } else {
        if (WN_operator(intre) == OPR_STID || 
            WN_operator(intre) == OPR_ISTORE) {
            return WHIRL_get_term(WN_kid0(intre), astidx);
        } else {
            return NULL;
        }
    }
}

void
toplvl_bb (CFG* acfg, BB_NODE *bb, OPT_STAB * optstab) {
    bool reasso_do_the_debug = false;
    int startcsenum = 220;
    bool useful_work = false;
    int num_iter = 0;
    do {
        map<VER_ID,WN*> whirlmap;
        vector<nary_exp> assovec;
        set<VER_ID> termset;
        // this is the variable that is used to 
        // detect a change for this fix point algorithm
        useful_work = false;
        STMT_ITER stmt_iter;
        WN *wn = NULL;
        map <int, WN*> map_stm_wn;
        FOR_ALL_ELEM (wn, stmt_iter, 
                 Init(bb->Firststmt(), bb->Laststmt())) {
            // I need to ensure that scalars are 
            // used in SSA kind of way. That is, I need to 
            // ensure that same variables 
            // are written to only once inside the 
            // basic block, if they are written to in the 
            // first place. This is a (major?) restriction
            // given that we are compiling C/FORTRAN code 
            // (Issue marked for future work).
            // Secondly when the work extends to array 
            // accesses, we will need to involve the 
            // alias manager. I have assumed that 
            // scalars do not alias with 
            // array accesses, which should be  
            // accurate.
            if ((WN_operator(wn) == OPR_STID)) {
                if (!optstab->Du_any_use(WN_ver(wn)))
                    continue;
                if (optstab->Du_is_volatile(WN_ver(wn)) == true) 
                    continue;
                if (
#ifdef TARG_X8664
                    WN_desc(wn) != MTYPE_V16F8 
                        && WN_desc(wn) != MTYPE_V16F4 &&
#endif
                        WN_desc(wn) != MTYPE_F8 
                        && WN_desc(wn) != MTYPE_F4) 
                    continue;
                if(whirlmap.find(WN_ver(wn)) 
                            == whirlmap.end()) {
                    if (termset.find(WN_ver(wn)) == termset.end()) {
                        // collect WHIRL for reassociation only if 
                        // STID writes to unseen terms.
                        // i.e. we require no anti/output dependencies 
                        // on the scalar variables
                        whirlmap[WN_ver(wn)] = WN_kid0(wn);
                        termset.insert(WN_ver(wn));
                        WN_kid0(wn) = build_associate_trees_collect_terms (
                                optstab, wn, WN_kid0(wn), termset, 
                                assovec, map_stm_wn);
                    } else {
                        // If we do a value numbering on 
                        // the basic block first, we can
                        // probably avoid this situation due to 
                        // scalar name reuse. Ideally,
                        // if we could get code into SSA
                        // and then apply reassociation
                        // we can extend the range of code this 
                        // pass acts over. For now, we
                        //bail out because term is reused.
                        FmtAssert(FALSE, 
                            ("Found a scalar name resue, expecting SSA semantics"));
                    }
                } else {
                    // we also need to bail out from here 
                    // because we do not 
                    // support rewrites to known variables
                    FmtAssert(FALSE, 
                        ("Found a scalar name resue, expecting SSA semantics"));
                }
            } else if (WN_operator(wn) == OPR_ISTORE) {
                // we assume that any istore does not write to scalar
                // variables. i am being very conservative by not 
                // even taking scalar associative content from 
                // kid0 of ISTOREs, and other likewise stms 
                // like address computes from 
                // ILOADs/ISTOREs/PREFETCHes etc which are mostly 
                // associative trees. Other examples are WHIRLs 
                // like CVTL,TRUNC, which may contain an associative tree in them. 
                // Basically if a given WHIRL is not associative, it 
                // does not mean that its kids are not. 
                // Marked for future work.
                continue;
            } else if (WN_operator(wn) == OPR_COMMENT || 
                    WN_operator(wn) == OPR_PREFETCH ||
                    WN_operator(wn) == OPR_XPRAGMA ||
                    WN_operator(wn) == OPR_PRAGMA ||
                    WN_operator(wn) == OPR_LABEL) {
                continue;
            } else if (WN_operator(wn) == OPR_GOTO ||
                    WN_operator(wn) == OPR_TRUEBR || 
                    WN_operator(wn) == OPR_FALSEBR ) {
                goto here;
            } else {
                // conservatively skip basicblock if there are 
                // other types of statements. Marked for future work.
                return;        
            } 
        }
here:    
        if (assovec.size() > 1) {
            reasso_struct reas_st(startcsenum, 
                    assovec, 
                    map_stm_wn, 
                    optstab);
            reas_st.canonicalized_input_trees = assovec;
            // reassociation's main driver, sets useful_work 
            // to true if it does any redundancy elimination
            do_reassociation(reas_st, bb, useful_work);
            assovec.clear();
            termset.clear();
            whirlmap.clear();
            startcsenum = reas_st.cse_start_id;
        }
        ++num_iter;
        // 
        // setting a top limit for the number of
        // times we fix redundancies in a basic block.
        // 
        if (num_iter > 30) {
            break;
        }
    } while (useful_work == true);
}

void
toplvl_cfg (CFG* acfg, OPT_STAB * optstab) {
    OPT_POOL_Initialize(&cse_name_pool, 
                    "Reassociation name pool", FALSE, REASSO_DUMP_FLAG_DEBUG);
    OPT_POOL_Push(&cse_name_pool, REASSO_DUMP_FLAG_DEBUG);
    CFG_ITER cfg_iter;
    BB_NODE *bb;
    if (Get_Trace( TP_WOPT2, REASSO_DUMP_FLAG)) {
        fprintf (TFile, 
        "-------Before redundancy elimination with reassociation--------\n"); 
        acfg->Print(TFile);
    }

    FOR_ALL_ELEM (bb, cfg_iter, Init(acfg)) {
        toplvl_bb (acfg, bb, optstab);
    }

    OPT_POOL_Pop(&cse_name_pool, REASSO_DUMP_FLAG_DEBUG);
    OPT_POOL_Delete(&cse_name_pool, REASSO_DUMP_FLAG_DEBUG);
    if (Get_Trace( TP_WOPT2, REASSO_DUMP_FLAG)) {
        fprintf (TFile, 
        "-------After redundancy elimination with reassociation--------\n"); 
        acfg->Print(TFile);
    }
}
}

void COMP_UNIT::Do_reasso(void) {
    if (WOPT_Enable_Reassociation_CSE == TRUE) {
        if (Roundoff_Level >= ROUNDOFF_ASSOC) {
            REASSO::toplvl_cfg (Cfg(), Opt_stab());
        }
    }
}

