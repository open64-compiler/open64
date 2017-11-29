/*
 * Copyright (C) 2010 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

/* placeholder for license */

#ifndef opt_peel_unroll_INCLUDED
#define opt_peel_unroll_INCLUDED

//////////////////////////////////////////////////////////////////////////
//
// LOOP_PEEL_UNROLL_DRIVER is the driver of loop peeling/unrolling, the real 
// implemetation is done by LOOP_PEEL_UNROLL_IMPL which is derived from this 
// class. This derivation prevents the gory implementation details from 
// cluttering header files, and therefore ensure that the header file are 
// concise and self-explanatory.
//
//  LOOP_PEEL_UNROLL_IMPL itself will work with bunch of "plug-in"s, each 
// serving for individual optimization. It is possible that multiple
// "plug-in"s compete for one loop/loop-nest. In that case, the driver will 
// arbitrate by picking up the one having declared benefit.
//
//////////////////////////////////////////////////////////////////////////
//
class LOOP_PEEL_UNROLL_DRIVER {
public:
    typedef enum {
        LPU_NONE,
        LPU_CONCISE,
        LPU_MEDIUM,
        LPU_DETAILED,
    } LPU_TRACE_LEVEL;

    // n-th entry should have value 2**n insteand <n>.
    typedef enum {
        LPU_OPT_NONE = 0,
        LPU_OPT_MV_FULLY_UNROLL = 1,
    } LPU_OPTS;

    LOOP_PEEL_UNROLL_DRIVER (COMP_UNIT*, LPU_OPTS opts, 
                             LPU_TRACE_LEVEL tr=LPU_NONE);
    ~LOOP_PEEL_UNROLL_DRIVER (void) {};

    // accessors 
    //
    COMP_UNIT* Get_comp_unit (void) const { return _comp_unit; }
    LPU_TRACE_LEVEL Get_trace_level (void) const { return _tr_level; }

    // return FALSE iff no change is made
    //
    BOOL Perform_peeling_or_unroll (void);

protected:
    COMP_UNIT* _comp_unit;
    LPU_TRACE_LEVEL _tr_level;
    LPU_OPTS _enable_opts;
};

#endif /*opt_peel_unroll_INCLUDED*/
