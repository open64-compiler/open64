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

#ifndef cggrp_ptn_INCLUDED
#define cggrp_ptn_INCLUDED

#include "defs.h"
#include "errors.h"
#include "targ_issue_port.h"
#include "targ_isa_bundle.h"

// ==================================================================
//
//  Module :  cggrp_ptn.h
//  $Date  : $
//  $Author: marcel $
//  $Source: /proj/osprey/CVS/open64/osprey1.0/common/ipfec_targ_info/msched/cggrp_ptn.h,v $
//
//  Description:
//  ============
//
//  This file contains the interface which is used for micro scheduler
//  to query the offline pattern table.
//
//  - PTN_table[bv] (where bv is a bit vector representing a issue
//    ports request) will return all the valid patterns that satisfy
//    this request -- a table line.
//  - a_table_line[i] the i-th pattern in this line
//
//  Note:
//  This table is initialized on offline, and should NEVER be modified
//  when compiler runs.
//
// ==================================================================


struct PATTERN_TYPE{
    mBOOL  start_in_bundle; // Pattern do NOT start with bundle boundery 
                            // But start from bundle stop bit in middle
    mBOOL  end_in_bundle;   // Pattern do NOT end with bundle boundery
                            // But end with bundle stop bit in middle
    mINT16 bundle[ISA_MAX_ISSUE_BUNDLES];

    const mINT16 operator[](INT i) const
    {
        Is_True((i<ISA_MAX_ISSUE_BUNDLES),
            ("Exceed max slot in accessing a pattern."));
        return bundle[i];
    }
    void Dump(FILE *f = stderr)
    {
        fprintf(f,"%d %d ", start_in_bundle, end_in_bundle);
        for( INT i=0; i<ISA_MAX_ISSUE_BUNDLES; i++)
        {
            fprintf(f, "%s ", ISA_EXEC_Name(bundle[i]));
        }
        fprintf(f, "\n");
    }
};

struct PTN_TABLE_LINE{
    INT size;
    PATTERN_TYPE *ptns;

     PATTERN_TYPE& operator[](INT i) const
    {
        Is_True((i<size), ("Invalid access of PTN table."));
        return ptns[i];
    }

    void Dump(FILE *f = stderr) ;

#ifdef Is_True_On 
    void gdb_dump (void);
#endif /* Is_True_On */
};

enum PTN_LINE_KIND {invalid_PTN_TABLE_entry = -1 };

struct PTN_TABLE_TYPE{
    INT *map;
    PTN_TABLE_LINE *body;


    const BOOL Is_Valid(INT i) const
    {
        return (map[i]!=invalid_PTN_TABLE_entry);
    }

    const PTN_TABLE_LINE& operator[](INT i) const
    {
        Is_True(Is_Valid(i), ("Access non-existing pattern %d.", i) );
        return body[map[i]];
    }
};

extern const PTN_TABLE_TYPE PTN_table;

///////////////////////////////////////////////////////////////////
// We still need a table: dispersal_targ_table;
//
// By using PTN_table, we can find a valid group pattern for a given
// issue ports assignment. After we got the pattern, we need to known
// to the port of each slot issued. Then we can sort the operations in
// this cycle and insert noops properly.
/////////////////////////////////////////////////////////////////////
struct DISPERSAL_TARG{
    PORT_SET  port[ISA_MAX_SLOTS*ISA_MAX_ISSUE_BUNDLES];
    PORT_SET  operator[](INT i) const { return port[i]; }
    PORT_SET  Ports( INT bundle, INT slot) const
    { return (*this)[bundle*ISA_MAX_SLOTS+slot]; }
    ISSUE_PORT  Port( INT bundle, INT slot) const
    { return ((*this)[bundle*ISA_MAX_SLOTS+slot]).First_IP(); }
};

struct DISPERSAL_TARG_TABLE{
    UINT             size;
    DISPERSAL_TARG   *body;

    const DISPERSAL_TARG *Query(const PATTERN_TYPE * ptn) const
    {
        UINT index = (*ptn)[ISA_MAX_ISSUE_BUNDLES-1] % ISA_MAX_BUNDLES;
        for (INT i=ISA_MAX_ISSUE_BUNDLES-2; i>=0; i--){
            index *= ISA_MAX_BUNDLES;
            index += (*ptn)[i] % ISA_MAX_BUNDLES;
        }
        Is_True( index < size, ("Invalid pattern in query dispersal table!"));
        return &(body[index]);
    }
};

extern const DISPERSAL_TARG_TABLE dispersal_table;

#endif // cggrp_ptn_INCLUDED
// End of file

