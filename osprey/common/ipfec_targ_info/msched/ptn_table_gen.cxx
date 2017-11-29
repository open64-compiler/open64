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

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <memory.h>

#include <vector>
#include "targ_isa_bundle.h" 
#include "targ_issue_port.h"
#include "targ_proc.h"

/*=============================================================================
 *=============================================================================
 *
 *  Module :  ptn_table_gen.cxx
 *  $Date  : $
 *  $Author: marcel $
 *  $Source: /proj/osprey/CVS/open64/osprey1.0/common/ipfec_targ_info/msched/ptn_table_gen.cxx,v $
 *
 *  Description:
 *  ============
 * 
 *  This is the offline model of the micro-scheduler.
 * 
 *  In this model, we try to generate a table for the online model
 *  to query. Every line of the table is identified by a possible
 *  function unit occupation (bit vector), and the content of the
 *  line should be all the valid the group patterns that can satisfy
 *  the resource's request represented by this bit vector.
 *
 *  And we need to dump the table into a C++ source file so that
 *  it can be used in our compiler
 *=============================================================================
 *=============================================================================*/

/******************************************************************
 * It seems that we can NOT use the error report machanism in pro64
 * compiler. So define ourselves.
 ******************************************************************/

# define FmtAssert(Cond,ParmList) \
    ( Cond ? (void) 1 : \
    ( fprintf(stderr, "Assertion Failed at %s:%d ", __FILE__, __LINE__), \
      fprintf(stderr, ParmList), \
      fprintf(stderr, "\n"), \
      assert(false) ) )

#define Is_True FmtAssert

static ISSUE_PORT Find_Issue_Port( INT already_reserved,
         ISA_EXEC_UNIT slot_kind,
         mINT16 bundle_sequence,
         mINT8 slot_sequence);

/*******************************************************************
 * struct PATTERN_TYPE
 *
 * Data structure is used to describe a group pattern.
 *******************************************************************/
struct PATTERN_TYPE{
    mBOOL start_in_bundle; /* Pattern do NOT start with a bundle boundery*/
    mBOOL end_in_bundle;   /* Pattern do NOT end with a bundle boundery */
    mINT16 bundle[ISA_MAX_ISSUE_BUNDLES];

    /* Constructors */
    PATTERN_TYPE(void) : start_in_bundle(FALSE), end_in_bundle(FALSE)
    {
        for (mINT16 i=0; i<ISA_MAX_ISSUE_BUNDLES; i++){
            bundle[i] = ISA_MAX_BUNDLES;
        }
    }

    PATTERN_TYPE( const PATTERN_TYPE& ptn)
    {
        start_in_bundle = ptn.start_in_bundle;
        end_in_bundle = ptn.end_in_bundle;
        for (INT i=0; i< ISA_MAX_ISSUE_BUNDLES; i++)
            bundle[i] = ptn.bundle[i];
    }

    INT  Count_Slot(void);
    BOOL Has_Multbranch_Template (void);

    PATTERN_TYPE& operator=(const PATTERN_TYPE& ptn);
    BOOL operator == (const PATTERN_TYPE ptn)  const ;
    mINT16& operator[](const INT i)  { return bundle[i]; }
};

PATTERN_TYPE& PATTERN_TYPE::operator=(const PATTERN_TYPE& ptn)
{
    start_in_bundle = ptn.start_in_bundle;
    end_in_bundle = ptn.end_in_bundle;
    for (INT i=0; i< ISA_MAX_ISSUE_BUNDLES; i++)
        bundle[i] = ptn.bundle[i];
    return *this;
}

BOOL PATTERN_TYPE::operator == (const PATTERN_TYPE ptn) const
{
    for (INT i=0; i< ISA_MAX_ISSUE_BUNDLES; i++){
        if (bundle[i] != ptn.bundle[i])
            return false;
    }
    return ((start_in_bundle == ptn.start_in_bundle)
            && (end_in_bundle == ptn.end_in_bundle));
}

INT PATTERN_TYPE::Count_Slot(void)
{
    INT count=0;/*count of slot available*/
    BOOL first_stop_meet = FALSE;

    for (INT i=0; i<ISA_MAX_ISSUE_BUNDLES; i++)
    {
        if (bundle[i] == ISA_MAX_BUNDLES) break;
        for (INT j=0; j<ISA_MAX_SLOTS; j++)
        {
            if (start_in_bundle&&!first_stop_meet){
                if (ISA_EXEC_Stop((INT)bundle[i], j) ){
                    first_stop_meet = TRUE;
                }
                continue;
            }
            count++;
            if (end_in_bundle && ISA_EXEC_Stop((INT)bundle[i], j)){
                return count;
            }
        }

    }
    return count;
}


BOOL
PATTERN_TYPE::Has_Multbranch_Template (void) {

    for (INT i=0; i<ISA_MAX_ISSUE_BUNDLES; i++) {

        if (bundle[i] == ISA_MAX_BUNDLES) return FALSE;

        int b_slot_num = 0;

        for (INT slot = 0; slot < ISA_MAX_SLOTS; slot++) {
            ISA_EXEC_UNIT_PROPERTY prop =
                ISA_EXEC_Slot_Prop (bundle[i],slot);

            if (prop & ISA_EXEC_PROPERTY_B_Unit) {
                ++ b_slot_num ; continue;
            }

            if (prop & ISA_EXEC_PROPERTY_B2_Unit) {
                ++ b_slot_num;
            }
        }

        if (b_slot_num >= 2) { return TRUE ; }
    }

    return FALSE;
}

PATTERN_TYPE empty_ptn;

typedef std::vector<PATTERN_TYPE> PTN_TABLE_LINE;

typedef std::vector<PATTERN_TYPE> PTN_VECTOR;

INT PTN_TABLE_SIZE=(1<<ip_number);

PTN_TABLE_LINE* PTN_table; 
INT max_ptn_line_size = 0;

/*******************************************************************
 * MCK difference
 *    logic dispersal rule for M_UNIT
 ******************************************************************/
void MCK_Dispersal_Rule(PORT_SET *old_ports, ISA_EXEC_UNIT slot_type)
{
   if ( PROCESSOR_Version == 2 ) {
        // Because Mckinley has intelligent dispersal logic
        // each slot can be dispersaled to evert M UNIT
        // pos indicates has available issue port
        if (slot_type == ISA_EXEC_M_Unit) 
            *old_ports = M_PORTS; 
   }
}
/********************************************************************
 * Dump method below, to dump the offline table into a C++
 * file so that we can use it in our compiler.
 ******************************************************************/

void Print_PTN( PATTERN_TYPE ptn, FILE *cxxfile)
{
    mINT16 bundle;

    /* Print init data */
    fprintf(cxxfile, "{ %d, %d, {",
        ptn.start_in_bundle, ptn.end_in_bundle);
    for (bundle=0; bundle<ISA_MAX_ISSUE_BUNDLES; bundle++){
        if (bundle!=0)
            fprintf(cxxfile, ",");
        fprintf(cxxfile, " %d", ptn[bundle]);
    }
    fprintf(cxxfile, "} }");

    /* Print Comments */
    fprintf(cxxfile, "\t/*");
    if (ptn.start_in_bundle)
        fprintf(cxxfile, "->");
    for (bundle=0; bundle<ISA_MAX_ISSUE_BUNDLES; bundle++){
        fprintf(cxxfile, "%s ",ISA_EXEC_Name(ptn[bundle]));
    }
    /* Print dispersal destination of each slot, for testing purpose.*/
    INT ip_issued=0;
    ISSUE_PORT pos = ip_invalid;
    for (bundle=0; bundle<ISA_MAX_ISSUE_BUNDLES; bundle++){
        for (INT slot=0; slot <ISA_MAX_SLOTS; slot++){
            ISA_EXEC_UNIT slot_type = ISA_EXEC_Unit(ptn[bundle]%ISA_MAX_BUNDLES, slot); 
            pos = Find_Issue_Port( ip_issued, slot_type , bundle, slot);
            /*  fprintf(cxxfile, "%s ",Issue_Port_Name(pos)); */

            PORT_SET ports;
            ports = ports + pos;
            // only print port number ,should print name of issue port
            // PORT_SET should provide a funtion to print all issue port it holds.
            MCK_Dispersal_Rule(&ports, slot_type);
            if(ptn[bundle] < ISA_MAX_BUNDLES) {
               ports.Print(cxxfile, "|");
               fprintf(cxxfile, " "); 
            }
            ip_issued |= 1 << pos;
            if (ISA_EXEC_Stop(ptn[bundle], slot))
                ip_issued = 0;
        }
    }

    fprintf(cxxfile, "*/");
}

void Print_Table_Line( PTN_TABLE_LINE table_line,
          INT request_bv, FILE *cxxfile)
{
    fprintf(cxxfile, "/* ================================================================\n");
    fprintf(cxxfile, " * Request bit vector: %d ", request_bv);

    INT bv = request_bv;
    for (INT ip=0; ip<ip_number; ip++){
        if (bv&1)
            fprintf(cxxfile, "%s ", Issue_Port_Name((const ISSUE_PORT)ip));
        bv >>= 1;
    }
    fprintf(cxxfile, "*/\n");
    if (table_line.size()==0){
        fprintf(cxxfile, "\n");
        return;
    }

    fprintf(cxxfile, "PATTERN_TYPE ptn%d[] = {", request_bv);
    for (UINT i=0; i<table_line.size(); i++){
        if (i!=0)
            fprintf(cxxfile, ",");
        fprintf(cxxfile, "\n\t");
        Print_PTN( table_line[i], cxxfile );
    }
    fprintf(cxxfile, "\n};\n\n");
}


/*
 * We want to purge those table lines with no valid patterns, so we
 * need a map for we can NOT use the bit vector as the index;
 */
void Gen_Table_Map_n_Body(FILE *cxxfile)
{
    INT index;
    INT value = 0;

    fprintf(cxxfile, "INT PTN_table_map[] = {\n\t");
    for (index=0; index < PTN_TABLE_SIZE; index++){
        if (index != 0)
            fprintf(cxxfile, ",\n\t");
        if (PTN_table[index].size() == 0)
            fprintf(cxxfile, "invalid_PTN_TABLE_entry");
        else{
            fprintf(cxxfile, "%d", value++);
        }
        fprintf(cxxfile, "/* index %d */", index);
    }
    fprintf(cxxfile, "\n};\n\n");

    value = 0;
    fprintf(cxxfile, "PTN_TABLE_LINE PTN_table_body[] = {\n\t");
    BOOL first = true;
    for (index=0; index < PTN_TABLE_SIZE; index++){
        if (PTN_table[index].size() != 0){
            if (!first)
                fprintf(cxxfile, ",\n\t");
            first = false;
            fprintf(cxxfile, "{%d, ptn%d} /* position %d */",
                    (INT)PTN_table[index].size(), index, value++);
        }
    }
    fprintf(cxxfile, "\n};\n\n");

}

void Print_PTN_Table_Def(FILE *cxxfile)
{
    fprintf(cxxfile, "const PTN_TABLE_TYPE PTN_table = {\n");
    fprintf(cxxfile, "\tPTN_table_map, PTN_table_body");
    fprintf(cxxfile, "\n};\n\n");
}

/*
 * Dump methods above, used to dump the offline table into a C++
 * file so that we can use it in our compiler.
 *******************************************************************/


/*
 * This function care about dispersal rule, subjected to change between
 * generations.
 *
 * Given a group pattern, we need this function to find out the
 * destination issue port of each slot.
 *
 * Parameters:
 * already_reserved: bit vector of function units occupied already
 * slot_kind:        the kind of the slot, M, I, B, F, L, X
 * bundle_sequence:  this slot is in the ?th bundle of the group, start with 0
 * slot_sequence:    this slot is the ?th slot of the bundle, start with 0;
 *
 * Return Value
 * return the issue port that the op in this slot should be issued to
 */

static ISSUE_PORT Find_Issue_Port( INT already_reserved,
         ISA_EXEC_UNIT slot_kind,
         mINT16 bundle_sequence,
         mINT8 slot_sequence)
{
    ISSUE_PORT pos = ip_invalid;

    Is_True( bundle_sequence<ISA_MAX_ISSUE_BUNDLES,
        ("Exceed Max Bundle Number!"));
    Is_True( slot_sequence < ISA_MAX_SLOTS, ("Exceed Max Slots!") );

    switch (slot_kind){
    case ISA_EXEC_M_Unit:
        for (ISSUE_PORT p=ip_M0; M_PORTS.In(p); p=(ISSUE_PORT)(p+1)){
            if (!( (1<<p) & already_reserved) ){ /* not occupied */
                pos = p;
                break;
            }
        }
        break;
    case ISA_EXEC_I_Unit:
X_Unit:
        if ( (slot_sequence==2) && (bundle_sequence >=1) )
        {   /* Third slot in the later bundle */
            if (!((1<<(ip_I0+bundle_sequence)) & already_reserved) ){
                /* not occupied */
                pos = (ISSUE_PORT)(ip_I0+bundle_sequence);
            }
            break;
        }
        for (ISSUE_PORT p=ip_I0; I_PORTS.In(p); p=(ISSUE_PORT)(p+1)){
            if (!( (1<<p) & already_reserved) ){ /* not occupied */
                pos = p;
                break;
            }
        }
        break;
    case ISA_EXEC_B_Unit:
        if ((slot_sequence==0) && !((1<<ip_B0)&already_reserved))
            /* First slot, B0 not occupied */
            pos = ip_B0;
        else if ((slot_sequence==1) && !( (1<<ip_B1) & already_reserved))
            /* Second slot */
            pos = ip_B1;
        else if ( (slot_sequence==2) && !( (1<<ip_B2) & already_reserved) )
            // Third slot
            pos = ip_B2;
        break;
    case ISA_EXEC_L_Unit:
        // I really hate the "goto" here. But the bundle talble in
        // Pro64 does NOT distinguish "L" and "X" slot. MLX will be
        // mistaken as MLL in its table.
        if (slot_sequence == 2)
            goto X_Unit;
    case ISA_EXEC_F_Unit:
        // First bundle to f0; second bundle to f1
        if (!((1<<(ip_F0+bundle_sequence)) & already_reserved) ){
            // not occupied
            pos = (ISSUE_PORT)(ip_F0+bundle_sequence);
        }
        break;
    case ISA_EXEC_R_Unit:
        break;
    default:
        Is_True(false, ("Unknow slot type in template table!"));
        break;
    }
    return pos;
}




/*
 * We need to sort the patterns in every line of the PTN_table, put
 * ones with single bunldle firstly, then the others. And in the ones with
 * single bundle, put those using compressed templates firstly.
 * Further more, we need to pub those do not start with a bundle
 * boundery firstly.
 *
 * The data structure below is for this purpose of sorting. 
 */
class PTN_SORT_BUF;

class PTN_SORT_BUF{

protected:
    enum BUCKET_TYPE{
        partial_in_both_end =0,
        partial_in_front,
        partial_in_back,
        complete_bundles,
        kinds_of_bucket
    };

    /* buckets to contain different kind of patterns */
    PTN_VECTOR _ptns[ISA_MAX_ISSUE_BUNDLES * kinds_of_bucket];

public:
    /* Clear all this buffer. */
    void Clear(void);

    /* Insert a pattern. Choose a bucket according to its kind. */
    void Insert(PATTERN_TYPE ptn, mINT16 num_bundles);

    /* Output all the bucket into dest, usually a table line. */
    void Output(PTN_VECTOR * dest);
};

void PTN_SORT_BUF::Clear(void)
{
    for (int i=0; i<ISA_MAX_ISSUE_BUNDLES * kinds_of_bucket; i++){
        _ptns[i].clear();
    }
}

void PTN_SORT_BUF::Insert(PATTERN_TYPE ptn, mINT16 num_bundles)
{
    PTN_VECTOR::iterator iter;
    INT index = num_bundles * kinds_of_bucket;

    if (ptn.start_in_bundle){
        if (ptn.end_in_bundle)
            index += partial_in_both_end;
        else index += partial_in_front;
    }
    else{
        if (ptn.end_in_bundle)
            index += partial_in_back;
        else index += complete_bundles;
    }

    /* Sort the sequence of ptn by the number of slot occupied; ascending*/  
    for(iter=_ptns[index].begin(); iter!=_ptns[index].end(); iter++)
    {
        INT delta = ptn.Count_Slot () - iter->Count_Slot ();
        if (delta < 0) { 
            break;
        } else if (delta == 0) {
            if (iter->Has_Multbranch_Template ()) {
               break;
            }
        }
    }
    _ptns[index].insert(iter, ptn);
}

void PTN_SORT_BUF::Output(PTN_VECTOR * dest)
{

    for (UINT i=0; i<ISA_MAX_ISSUE_BUNDLES * kinds_of_bucket; i++){
        for (UINT j=0; j<_ptns[i].size(); j++){
            dest->push_back(_ptns[i][j]);
        }
    }
}

PTN_SORT_BUF pattern_buffer;

void Find_PTN(INT request_bv, 
              INT satisfied_bv,
              PATTERN_TYPE cur_ptn,
              mINT16 cur_bdSeq);


void Gen_PTN_Table(FILE *cxxfile)
{
    INT bv; // bit vector represents occupation of func units
    for (bv=1; bv < (PTN_TABLE_SIZE); bv++) {
        pattern_buffer.Clear();

        Find_PTN(bv, 0, empty_ptn, 0);

        pattern_buffer.Output(&PTN_table[bv]);
        if (PTN_table[bv].size()>max_ptn_line_size)
            max_ptn_line_size = PTN_table[bv].size();

        Print_Table_Line( PTN_table[bv], bv, cxxfile);
    }
    Gen_Table_Map_n_Body(cxxfile);
    Print_PTN_Table_Def(cxxfile);
}
INT bv_count(INT bv)
{
   INT count = 0;
   for(INT i=0; i<ip_invalid; i++) {
       if (bv & 1 << i) count++; 
   }
//   printf("bv= %x, count = %d\n", bv, count);
   return count;
}
/*
 * This function answer whether ptn's satisfied bit vector cover 
 * the reserved word or not. In Itanium, it is easy to just do and
 * operation, But in Mckinley, we should consider the intellectual 
 * of M_unit dispersal rule. M_unit in any slot can be issued to any
 * M issue ports. We must count the number of M_Unit is suitable.
 */
BOOL bv_is_cover(INT satisfied_bv, INT request_bv) 
{
    if ( PROCESSOR_Version == 2 ) {
        BOOL is_cover =  !(~(satisfied_bv | M_PORTS.Body()) & request_bv);
        if (is_cover) {
            /* count number of memory unit */
            INT s_count = bv_count(satisfied_bv & M_PORTS.Body());
            INT r_count = bv_count(request_bv & M_PORTS.Body());
            if(s_count >= r_count) return TRUE;
           
        }
        return FALSE;
    } else {
         return !(~satisfied_bv & request_bv); 
    }
}
/*
 * This function is used to traverse every possible group pattern, when we
 * find that this pattern can cover all the issue ports reserved in
 * the bit vector, out put it into the corresponding buffer.
 *
 * One issue is about traversing: If the number of bundles issue in a
 * cycle is a variable, how can I just use nested loops (When we 
 * use nested loops, We need to modify our code when the machine 
 * width change from 2 to 4.)? So recursively traverse templates' table.
 *
 * Parameters
 * - request_bv: function units requested by current state, trying to
 *   find patterns that cover it.
 * - satisfied_bv: function units covered by selected templates. If
 *   satisfied_bv covers request_bv, done!
 * - cur_ptn:  partial group patterns selected by this and previous
 *   iteration of Find_PTN.
 * - cur_bdSeq: we are currently seeking for the ?th bundle of the
 *   group pattern. Start with 0
 */
void Find_PTN(INT request_bv, INT satisfied_bv,
              PATTERN_TYPE cur_ptn, mINT16 cur_bdSeq)
{
    /*
     * When we see a group pattern with a compressed template,
     * which part of the slots should we use?
     */
    static BOOL useTail; /*TRUE: Use slots after the stop bit */

    Is_True( cur_bdSeq < ISA_MAX_ISSUE_BUNDLES,
        ("Exceed max bundle when finding pattern.") );

    if (bv_is_cover(satisfied_bv, request_bv))
        return;  /* satisfied_bv covers request_bv */

    for (mINT16 template_index=0; template_index<ISA_MAX_BUNDLES; template_index++)
    {
        /* Just traverse the templates' table of MD */
        if (cur_bdSeq == 0)
            useTail = false; /* we start with a brand new pattern */
    else if (ISA_EXEC_Stop_Before(template_index)){
        // Some templates has stop bit before them
        // They must be the first bundle
        continue;
    }

        BOOL success = true;
        BOOL stop_bit_encountered = false;
        INT  bv_buffer = 0;

        for (INT slot_index=0; slot_index < ISA_MAX_SLOTS; slot_index++){

            ISSUE_PORT pos = Find_Issue_Port(satisfied_bv | bv_buffer,
                             ISA_EXEC_Unit(template_index, slot_index),
                            cur_bdSeq, slot_index);

            if (pos == ip_invalid){
                success = false;
                break; /* try the next template; */
            }
            bv_buffer |= 1 << pos;
             
            if (ISA_EXEC_Stop(template_index, slot_index))
            {
                /* stop bit in a compressed template encountered */
                stop_bit_encountered = true; 

                if (bv_is_cover(bv_buffer|satisfied_bv, request_bv)){
                    /*
                     * all the resources request fulfilled 
                     * A valid group pattern found, with compressed
                     * template at the tail.
                     */
                    cur_ptn[cur_bdSeq] = template_index;
                    cur_ptn.end_in_bundle = true;
                    cur_ptn.start_in_bundle = useTail;

                    /* Output cur_ptn to sort buffer */
                    pattern_buffer.Insert(cur_ptn, cur_bdSeq);

                    cur_ptn[cur_bdSeq] = ISA_MAX_BUNDLES;
                    cur_ptn.end_in_bundle = false;
                    cur_ptn.start_in_bundle = false;
                    /*
                     * fall through to see whether the slots after
                     * the stop bit can still be used
                     */
                }
                if (cur_bdSeq != 0) /* this is NOT the first bundle */
                    /* We can NOT use the following slots */
                    break; 

                /*
                 * Go on checking the slots after the stop bit.
                 * There may be a valid pattern.
                 */
                Is_True( (satisfied_bv == 0) && (cur_ptn == empty_ptn),
                    ("Failed to clear issue ports when finding patterns."));
                bv_buffer = 0;
                stop_bit_encountered = 0;
                useTail = true;
            } /* if stop bit encountered */
        } /* for each slot */

        if (stop_bit_encountered)
            continue; /* try next template */

        if (success){
            cur_ptn[cur_bdSeq] = template_index;
            INT saved = satisfied_bv; /* save satisfied_bv */
            satisfied_bv |= bv_buffer;

            if (bv_is_cover(satisfied_bv,request_bv)){
                /* satisfied_bv fully cover request_bv
                 * a valid group pattern successfully found! */
                cur_ptn.start_in_bundle = useTail;
                cur_ptn.end_in_bundle = false;
                pattern_buffer.Insert(cur_ptn, cur_bdSeq);
            }
            else{
                if ( (cur_bdSeq < ISA_MAX_ISSUE_BUNDLES-1)
            && (!ISA_EXEC_Stop_After(template_index)) ){
                    /* Not reached Number of bundles per cycle
             * && NO split issue after current template
                     * Go on try more bundle */
                    Find_PTN(request_bv, satisfied_bv, cur_ptn, cur_bdSeq+1);
        }
            }

            cur_ptn[cur_bdSeq] = ISA_MAX_BUNDLES;
            cur_ptn.end_in_bundle = false;
            cur_ptn.start_in_bundle = false;
            satisfied_bv = saved;
        } /* if (success) */
        /* continue to test next template */
    } /* for each template */
}

/********************************************************************
 * We still need a table: dispersal_targ_table;
 *
 * Using PTN_table, we can find a valid group pattern for a given
 * issue ports assignment. After we get the pattern, we need to know
 * which port each slot should be issued. Then we can sort the operations
 * in this cycle and insert noops properly.
 ********************************************************************/
struct DISPERSAL_TARG{
    PORT_SET  port[ISA_MAX_SLOTS*ISA_MAX_ISSUE_BUNDLES];
    PORT_SET& operator[](int i) { return port[i]; }
};

class DISPERSAL_TARG_TABLE{
protected:
    UINT            _size;
    DISPERSAL_TARG  *_body;

public:
    /* Constructor and Destroctors */
    DISPERSAL_TARG_TABLE(void);
    ~DISPERSAL_TARG_TABLE(void);

    /* Generate the whole table. */
    void Gen_Table(void);
    /* Dump the table to a file as the form of array init */
    void Dump(FILE * dest_file)  const; 
};

DISPERSAL_TARG_TABLE::DISPERSAL_TARG_TABLE(void)
{
    _body = NULL;
    _size = 1;
    for (int i=0; i<ISA_MAX_ISSUE_BUNDLES; i++){
        _size = _size * ISA_MAX_BUNDLES;
    }
    /* It should be ok to use malloc here for it is in offline. */
    _body = (DISPERSAL_TARG *) malloc(_size * sizeof(DISPERSAL_TARG));
    FmtAssert(_body, ("Not enough memory for dispersal table!"));
}

DISPERSAL_TARG_TABLE::~DISPERSAL_TARG_TABLE(void)
{
    free(_body);
    _body = NULL;
    _size = 0;
}

void DISPERSAL_TARG_TABLE::Gen_Table(void)
{
    UINT index;
    for (index = 0; index <_size; index++){
        UINT ptn_index = index;
        UINT template_index = ptn_index % ISA_MAX_BUNDLES;
        INT  fu_issued = 0;
        UINT sl = 0;
        for (INT bundle=0; bundle<ISA_MAX_ISSUE_BUNDLES; bundle++){
            for (INT slot=0; slot <ISA_MAX_SLOTS; slot++){
                ISSUE_PORT pos= Find_Issue_Port( fu_issued,
                    ISA_EXEC_Unit(template_index, slot), bundle, slot);

                // convert ISSUE_PORT to PORT_SET;
                PORT_SET ports;
                ports = ports + pos;
                MCK_Dispersal_Rule(&ports, ISA_EXEC_Unit(template_index, slot));
                _body[index][sl] = ports;

                 fu_issued |= 1<< pos;
                if (ISA_EXEC_Stop(template_index, slot))
                    fu_issued = 0;
                sl++;
            }
            ptn_index /= ISA_MAX_BUNDLES;
            template_index = ptn_index % ISA_MAX_BUNDLES;
        }
    }
}


void DISPERSAL_TARG_TABLE::Dump(FILE * dest_file) const
{
    fprintf(dest_file, "UINT            dispersal_table_size = %d;\n",
            _size);
    fprintf(dest_file, "DISPERSAL_TARG  dispersal_table_body[]={\n");

    for (UINT i=0; i<_size; i++){
        if (i!=0)
            fprintf(dest_file, ",\n");
        fprintf(dest_file, "    {");
        for (int j=0; j<ISA_MAX_SLOTS*ISA_MAX_ISSUE_BUNDLES; j++){
            if (j!=0)
                fprintf(dest_file, ",");

            // dump all issue port holded by ports set
            fprintf(dest_file, " 0x%x", _body[i][j].Body());
        }
        fprintf(dest_file, "}");

        /* Printing comment, telling what kind of pattern it is */
        fprintf(dest_file, "/* ");
        UINT ptn_index = i;
        UINT template_index = ptn_index % ISA_MAX_BUNDLES;
        for (INT bundle=0; bundle<ISA_MAX_ISSUE_BUNDLES; bundle++){
            fprintf(dest_file, "%s ",ISA_EXEC_Name(template_index));
            ptn_index /= ISA_MAX_BUNDLES;
            if (ptn_index == ISA_MAX_BUNDLES) {
                /* only bundle group */
                break;
            }
            template_index = ptn_index % ISA_MAX_BUNDLES;
        }
        fprintf(dest_file, "*/");
    }

    fprintf(dest_file, "\n};\n\n");
    fprintf(dest_file, "const DISPERSAL_TARG_TABLE dispersal_table =\n");
    fprintf(dest_file, "    { dispersal_table_size, dispersal_table_body };\n\n");
    fprintf(dest_file, "INT MAX_PTN_TABLE_LINE_SIZE = %d; \n", max_ptn_line_size);
}
/********************************************************************
 * End definition of dispersal_targ_table
 ********************************************************************/

void Dump_Headerfile(FILE *headerfile)
{
    fprintf(headerfile,
"//-*-c++-*-\n"
"\n"
"#ifndef cggrp_ptn_table_INCLUDED\n"
"#define cggrp_ptn_table_INCLUDED\n"
"\n"
"#include \"cggrp_ptn.h\"\n\n"
    );

    fprintf(headerfile, "extern INT MAX_PTN_TABLE_LINE_SIZE;\n");

    fprintf(headerfile,
"\n#endif // cggrp_ptn_table_INCLUDED\n"
"// End of file\n"
    );
}
void Init_Memory()
{
   PTN_table = (PTN_TABLE_LINE *)malloc(sizeof(PTN_TABLE_LINE) * PTN_TABLE_SIZE); 
}
void Free_Memory()
{
   free(PTN_table);
}
int main(void)
{
    char *OFFLINE_TABLE_FILE_NAME ="cggrp_ptn_table";
    

    char buf[1000];
    FILE *cxxfile = NULL;
    FILE *hfile = NULL;

    if (PROCESSOR_Version == 2) /* itanium2 */
       sprintf(buf, "%s_mck.cxx", OFFLINE_TABLE_FILE_NAME);
    else 
       sprintf(buf, "%s.cxx", OFFLINE_TABLE_FILE_NAME);

    cxxfile = fopen(buf, "wt");
    Is_True(cxxfile, ("Can not open offline table file!"));

    fprintf(cxxfile, "//-*-c++-*-\n");
    fprintf(cxxfile, "#include \"defs.h\"\n");
    fprintf(cxxfile, "#include \"errors.h\"\n");
    fprintf(cxxfile, "#include \"targ_isa_bundle.h\"\n");
    fprintf(cxxfile, "#include \"%s.h\"\n", OFFLINE_TABLE_FILE_NAME);

    Init_Memory();
    Gen_PTN_Table(cxxfile);
    DISPERSAL_TARG_TABLE dispersal_table;
    dispersal_table.Gen_Table();
    dispersal_table.Dump(cxxfile);

    fprintf(cxxfile, "/* End of Generated Offline Table Definition.*/\n");
    fclose(cxxfile);

    if (PROCESSOR_Version == 2) /* itanium2 */
       sprintf(buf, "%s_mck.h", OFFLINE_TABLE_FILE_NAME);
    else 
       sprintf(buf, "%s.h", OFFLINE_TABLE_FILE_NAME);

    hfile = fopen(buf, "wt");
    Is_True(hfile, ("Can not open offline table file!"));

    Dump_Headerfile(hfile);
    fclose(hfile);
    Free_Memory();
    return 0;
}

