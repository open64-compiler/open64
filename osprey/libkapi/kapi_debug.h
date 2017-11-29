/*
 * Copyright (c) 2000, Intel Corporation
 * All rights reserved.
 *
 * WARRANTY DISCLAIMER
 *
 * THESE MATERIALS ARE PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT 
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL INTEL OR ITS 
 * CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR 
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY
 * OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THESE
 * MATERIALS, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * Intel Corporation is the author of the Materials, and requests that all
 * problem reports or change requests be submitted to it directly at
 * http://developer.intel.com/opensource.
 */


/* static char sccs_id[] = "%W%  %G%  %U%"; */


#ifndef _KAPI_DEBUG_H_
#define _KAPI_DEBUG_H_

#include <stdio.h>

#include "kapi_internal.h"

#include "kapi_parse.h"

#define bv32PRINT_LEVEL_DEP_LIST   0x0001

extern void kapi_indent_line( FILE *fp, int nTab );
extern void KDebug_DumpInterClusterBypass( FILE *fp, knobs_t *pknobs );
extern void KDebug_DumpClusterDistances( FILE *fp, knobs_t *pknobs );
extern void KDebug_DumpIntraClusterBypass( FILE *fp, knobs_t *pknobs );
extern void KDebug_DumpLatencies( FILE *fp, knobs_t *pknobs );
extern void KDebug_DumpTotalLatency( FILE *fp, knobs_t *pknobs );
extern void KDebug_printval( FILE *fp, tfi_t *ptfi, ed_t *pedIn, int nTab );




extern void KDebug_DumpMachineDescription( FILE *fp, knobs_t *pknobs, int nTab );
extern void KDebug_DumpLatencyTable( FILE *fp, knobs_t *pknobs, int iLevel );
extern void KDebug_DumpFUTable( FILE *fp, knobs_t *pknobs, int iLevel );
extern void KDebug_DumpPortTable( FILE *fp, knobs_t *pknobs, int iLevel );
extern void KDebug_DumpITTable( FILE *fp, knobs_t *pknobs, int iLevel );
extern void KDebug_DumpS2SLatencyTable( FILE *fp, knobs_t *pknobs, int iLevel );
extern void KDebug_DumpInstTable( FILE *fp, knobs_t *pknobs, int nTab );

extern void KDebug_TestBypassValues( FILE *fp, knobs_t *pknobs );

extern void KDebug_Dumpmpit( FILE *fp, knobs_t *pknobs, int mpnitAvail[] );

#endif

