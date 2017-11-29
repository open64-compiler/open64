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

#ifndef _KMAPI_INTERNAL_H
#define _KMAPI_INTERNAL_H

#include "kapi_internal.h"
#include "kmapi.h"


#ifndef FALSE
#define FALSE 0
#endif
#ifndef TRUE
#define TRUE 1
#endif

typedef struct _KMAPI_FULIST_T {
	kmapi_ResourceMap_t bvResourceMap;
	struct _KMAPI_FULIST_T *pNext;
} kmapi_fulist_t;


typedef struct _KMAPI_EXCEPTION_DATA_T {
	kapi_bid_t bid;
	bv128_t bvfuClasses;
	kmapi_fulist_t *pfuList;
} kmapi_exception_data_t;

typedef struct _KMAPI_EXCEPTION_T {
	kmapi_exception_data_t Details;
	struct _KMAPI_EXCEPTION_T *pNext;
} kmapi_exception_t;

typedef struct _BUNDLE_FUS_T {
	bv128_t bvSlots[kmapi_BUNDLE_WIDTH];
	struct _BUNDLE_FUS_T *pNext;
} bundle_fus_t;

typedef struct _KMAPI_SPLIT_RULE_T {
	kapi_bid_t bid;
	bv32_t bvSplits;
	bundle_fus_t *pSplitExeptions;
} kmapi_split_rule_t;

typedef enum {
	ALLOCATE_INVALID=0,
	ALLOCATE_FOR_ITANIUM
} kmapi_allocation_scheme_e;

typedef struct _KMAPI_KNOBS_T {
	knobs_t *pKnobs;
	int cBinfoLists;					/* number of items in BinfoLists */
	kmapi_fulist_t **dm3pfuBinfoLists;	/* pointers to mapping options per place/bid/slot */
	int cOptions;					/* number of options */
	kmapi_fulist_t **dm3pOptions;	/* lists of possible options per place/slot/syl */
	int cFuclassLists;					/* number of Fuclass lists */
	kmapi_fulist_t **dmpfuFuclassLists; /* preparation for McKinley */
	int cExceptions;					/* number of exceptions */
	kmapi_exception_t **dm3pExceptions;  /* execeptions to mapping rules per place/slot/syl */
	int cSplitRules;					/* number of split issue rules */
	kmapi_split_rule_t *dmpSplitRules;	/* split issue rules */
	int iTotalResources; /* total resources that kmapi can handle, currently 32 */
	kmapi_allocation_scheme_e iScheme;
	bv32_t *dmpMiscFuInfo;
	/* other info */
} kmapi_knobs_t;

/* init functions */
kmapi_result kmapi_init_split_issue_data(kmapi_knobs_t *pKMnobs);

/* saver functions & info */
extern int nBundlesPerCycle;

#define MAX_OPTIONS 64

typedef enum {
	KMAPI_FU_IS_LONG=0
} fu_misc_e;

#endif /* _KMAPI_INTERNAL_H */

