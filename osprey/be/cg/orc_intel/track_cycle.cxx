/*
  Copyright (C) 2000-2003, Intel Corporation
  All rights reserved.
  
  Redistribution and use in source and binary forms, with or without modification,
  are permitted provided that the following conditions are met:
  
  Redistributions of source code must retain the above copyright notice, this list
  of conditions and the following disclaimer. 
  
  Redistributions in binary form must reproduce the above copyright notice, this list
  of conditions and the following disclaimer in the documentation and/or other materials
  provided with the distribution. 

  Neither the name of the owner nor the names of its contributors may be used to endorse or
  promote products derived from this software without specific prior written permission. 

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR
  IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
  FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE CONTRIBUTORS BE LIABLE FOR
  ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
  NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
  BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

//-*-c++-*-
/* ====================================================================
 *
 * Module: track_cycle.cxx
 * $Revision: 1.1.1.1 $
 * $Date: 2005/10/21 19:00:00 $
 * $Author: marcel $
 * $Source: /proj/osprey/CVS/open64/osprey1.0/be/cg/orc_intel/track_cycle.cxx,v $
 *
 * Description:
 *
 * We need to do cycle count about an BB for performance comparision, while
 * due to oversubscription in current pro64, the cycle count gives far
 * less value than it really costs.
 *
 * Here we track the usage of each function unit and machine width to
 * increase the value of cycle count
 *
 * ====================================================================
 * ====================================================================
 */

#include <vector>

#include "defs.h"
#include "errors.h"
#include "tn.h"
#include "op.h"
#include "op_map.h"
#include "cg_dep_graph.h"
#include "cgtarget.h"

#include "ipfec_options.h"
#include "cggrp_ptn.h"
#include "targ_isa_bundle.h"
#include "targ_issue_port.h"

// ==================================================================
//
//   Class Name: TRACK_CYCLE
//
//   Base Class: <none>
//
//   Derived Class: <none>
//
//   Class Description: 
//      Each CYCLE_STATE object tracks the usage of every issue port
//      and issue window to identify over subscription done in
//      bundling
//
//   Note:
//
// ==================================================================


class TRACK_CYCLE {
    INT             _cur_bundle;
    PATTERN_TYPE    _ptn;
    std::vector <mBOOL>  _occupied;
    INT             _split;
    const DISPERSAL_TARG  *_ports;

    void _Clear()
    {
        _cur_bundle = 0;
	_ptn.start_in_bundle = FALSE;
	_ptn.end_in_bundle = FALSE;
	for (int i=0; i<ISA_MAX_ISSUE_BUNDLES; i++)
	    _ptn.bundle[i] = ISA_MAX_BUNDLES;

        _occupied.clear();
	for (int j=0; j<ip_invalid; j++)
	    _occupied.push_back(FALSE);
	_ports = NULL;
    }
public:
    TRACK_CYCLE()  { _Clear(); }
    void Next_Cycle()   { _Clear(); _split++; }
    void Next_Bundle(int template_index);
    INT  Split(INT template_index, UINT stop_mask, BOOL &extra_split);
};


void TRACK_CYCLE::Next_Bundle(INT template_index)
{
    if (ISA_EXEC_Stop_Before(template_index)){
      if (!((_cur_bundle==0)&&(_ptn[_cur_bundle]==ISA_MAX_BUNDLES))){
        // The current cycle is NOT empty
        Next_Cycle();
      }
    }
    if (_ptn[_cur_bundle]!=ISA_MAX_BUNDLES){
        if (_cur_bundle < ISA_MAX_ISSUE_BUNDLES-1){
	    _cur_bundle ++;
	}
	else{
	    Next_Cycle();
	}
    }
    _ptn.bundle[_cur_bundle] = template_index;
    _ports = dispersal_table.Query(&_ptn);
    Is_True(_ports, ("Illegal group pattern in emit phase!"));
}

INT TRACK_CYCLE::Split(INT template_index, UINT stop_mask, BOOL &extra_split)
{
    _split = 0;
    extra_split = FALSE;
    Next_Bundle(template_index);

    for (INT slot=0; slot<ISA_MAX_SLOTS; slot++){
        ISSUE_PORT issue_port = _ports->Port(_cur_bundle, slot);
	if ((_occupied[issue_port])||(issue_port == ip_invalid)){
	    Next_Cycle();
	    Next_Bundle(template_index); // This bundle not finished
	    issue_port = _ports->Port(_cur_bundle, slot);
	    extra_split = TRUE;
	}
	_occupied[issue_port] = TRUE;
        if ( (ISA_EXEC_Stop(template_index, slot))
	     ||(stop_mask & (1<<ISA_MAX_SLOTS-1-slot)) ){
	    Next_Cycle();
	    if (slot < ISA_MAX_SLOTS-1){ // Stop bit in bundle
	        Next_Bundle(template_index); // This bundle not finished
	    }
	}
        else if (ISA_EXEC_Stop_After(template_index)&&(slot==ISA_MAX_SLOTS-1)){
            Next_Cycle();
	    extra_split = TRUE;
        }
    }
    return _split;
}

TRACK_CYCLE track;

INT Track_Split(INT template_index, UINT stop_mask, BOOL &extra)
{
    return track.Split( template_index, stop_mask, extra);
}

