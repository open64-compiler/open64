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

#include <assert.h>
#include <string.h>

#include <stdlib.h>
#include "kapi_internal.h"
#include "kapi.h"
#include "kapi_ia64.h"
#include "kapi_error.h"




int KAPI_nCacheHierarchyLevels(void *pConfig, kapi_cache_types_e cachetype )
{
	knobs_t *pknobs=(knobs_t *)pConfig;
	if (cachetype>=cache_type_enum_size)
		return -1;
	assert((pknobs->nCacheLevels!=NULL) && "Cache content not present!\n");
	return (pknobs->nCacheLevels[cachetype]+pknobs->nCacheLevels[cache_type_unified]);
}

/* return cache struct for specific type/level */
kapi_cache_t *KAPI_CacheHierarcy(void *pConfig, int level, kapi_cache_types_e cachetype )
{
	int iPort;
	knobs_t *pknobs=(knobs_t *)pConfig;
	kapi_cache_t *pcacheTmp=(kapi_cache_t *)malloc(sizeof(kapi_cache_t));
	cache_t *pcache;
	/* find out which type we are talking about, and if data exists for it */
	assert((pknobs->nCacheLevels!=NULL) && "Cache content not present!\n");

	if ((cachetype!=cache_type_data) 
		&& (cachetype!=cache_type_instruction))	
		return NULL; /* illegal cache content */

	if (level>=pknobs->nCacheLevels[cachetype]) {
		level-=pknobs->nCacheLevels[cachetype];
		if (level>pknobs->nCacheLevels[cache_type_unified])
			return NULL; /* illegal cache level */
		else		
			pcache=&(pknobs->dmpcacheTable[cache_type_unified][level]);
	}
	else 
		pcache=&(pknobs->dmpcacheTable[cachetype][level]);
	/* now get data */
	pcacheTmp->nLines=pcache->nLines;
	pcacheTmp->nBytesLine=pcache->nBytesLine;
	pcacheTmp->nWays=pcache->nWays;
	pcacheTmp->bv32CacheContents=pcache->bv32CacheContents;
	pcacheTmp->iWritePolicy=pcache->iWritePolicy;
	pcacheTmp->iReplPolicy=pcache->iReplPolicy;
	pcacheTmp->iAllocPolicy=pcache->iAllocPolicy;
	pcacheTmp->nCachePorts=pcache->nCachePorts;
	pcacheTmp->nCyclesRead=pcache->nCyclesRead;
	for (iPort=0;iPort<pcacheTmp->nCachePorts;iPort++)
		pcacheTmp->cacheportInfo[iPort].bv32AccessMode=pcache->cacheportInfo[iPort].bv32AccessMode;
	return pcacheTmp;
}

