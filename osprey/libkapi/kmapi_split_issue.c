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

#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include "kmapi.h"
#include "kmapi_internal.h"
#include "kmapi_error.h"



bundle_fus_t *bundle_fus_AddNode(bundle_fus_t **ppbfu)
{
	int i;
	bundle_fus_t *pbfu=(bundle_fus_t *)malloc(sizeof(bundle_fus_t));
	for (i=0;i<kmapi_BUNDLE_WIDTH;i++)
		ZERO_bv128(pbfu->bvSlots[i]);
	if (*ppbfu!=NULL)
		pbfu->pNext=(*ppbfu);
	else /* first node */
		pbfu->pNext=NULL;
	*ppbfu=pbfu;
	return pbfu;
}

/* Split issue format:
BID:T0:T1:<fu(fff,fff)>/<none>
where BID is bundle template, 
T is yes or no, T0 indicating split before, T1 indicating split after
f is either * for any or a specific fuClass, indicating exception to the rule, or none
  to indicate no execptions.
Currently only one syllable can contain exception.
Sample: 
"bidMMB:no:yes:fu(* / * /fuBRP,* / * /fuNOP)"
*/
kmapi_result kmapi_parse_split(kmapi_knobs_t *pKMnobs,char *pchSplitRule,kapi_bid_t *pbid)
{
	char *pchWalker;
	char *pchRule;
	char *pchBid;
	char *pchTmp;
	int iSlot=0;
	kapi_bid_t bid;
	bundle_fus_t *pbfu;
	kapi_fu_t fu;
	int fEND=FALSE;

	pchRule=strdup(pchSplitRule);
	pchBid=pchWalker=pchRule;
	pchWalker=strchr(pchWalker,':');
	*pchWalker='\0';
	bid=KAPI_EnumIndex(pKMnobs->pKnobs,"bid_t",pchBid);
	if (bid==-1)
	{
		kmapi_Error_pch1(0,"no such bundle template %s",pchBid);
		return kmapi_failure;
	}
	pchTmp=++pchWalker;
	pchWalker=strchr(pchWalker,':');
	*pchWalker='\0';
	if (strstr(pchTmp,"yes")!=NULL)
		pKMnobs->dmpSplitRules[bid].bvSplits|=kmapi_implicit_break_before;
	else 
	{
		if (strstr(pchTmp,"no")==NULL)
		{
			kmapi_Error_pch1(0,"wrong format for split issue rule: %s",pchSplitRule);
			return kmapi_failure;
		}
	}
			
	pchTmp=++pchWalker;
	pchWalker=strchr(pchWalker,':');
	*pchWalker='\0';
	if (strstr(pchTmp,"yes")!=NULL)
		pKMnobs->dmpSplitRules[bid].bvSplits|=kmapi_implicit_break_after;
	else 
	{
		if (strstr(pchTmp,"no")==NULL)
		{
			kmapi_Error_pch1(0,"wrong format for split issue rule: %s",pchSplitRule);
			return kmapi_failure;
		}
	}
	pchWalker++;
	if (strstr(pchWalker,"none")!=NULL)
		return kmapi_success;
	pchWalker=strchr(pchWalker,'(');
	pchTmp=++pchWalker;
	pbfu=bundle_fus_AddNode(&(pKMnobs->dmpSplitRules[bid].pSplitExeptions));
	while (!fEND)
	{
		pchWalker++;
		switch (*pchWalker)
		{
		case ')':
			fEND=TRUE;
		case '/':
			*pchWalker='\0';
			if (*pchTmp!='*')
			{
				fu=KAPI_EnumIndex(pKMnobs->pKnobs,"fu_t",pchTmp);
				if (fu==-1) {
					kmapi_Error_pch2(0,"No such fu %s in rule %s",pchTmp,pchSplitRule);
				}
				SETBIT_bv128(pbfu->bvSlots[iSlot],fu);
			}
			pchTmp=++pchWalker;
			iSlot++;
			break;
		case ',':
			*pchWalker='\0';
			fu=KAPI_EnumIndex(pKMnobs->pKnobs,"fu_t",pchTmp);
			SETBIT_bv128(pbfu->bvSlots[iSlot],fu);
			pchTmp=++pchWalker;
			iSlot=0;
			pbfu=bundle_fus_AddNode(&(pKMnobs->dmpSplitRules[bid].pSplitExeptions));
			break;
		default:
			break;
		}
	}
	return kmapi_success;
}

kmapi_result kmapi_init_split_issue_data(kmapi_knobs_t *pKMnobs)
{
	int i;
	int nSplits;
	kapi_bid_t bid;
	int nBid;

	nSplits=KAPI_count4attribute(pKMnobs->pKnobs,"SPLIT_ISSUE");
	nBid=KAPI_EnumCardinality(pKMnobs->pKnobs,"bid_t");
	pKMnobs->dmpSplitRules=(kmapi_split_rule_t *)malloc(sizeof(kmapi_split_rule_t)*nBid);
	for (i=0;i<nBid;i++)
	{
		pKMnobs->dmpSplitRules[i].bid=i;
		pKMnobs->dmpSplitRules[i].bvSplits=0;
		pKMnobs->dmpSplitRules[i].pSplitExeptions=NULL;
	}
	for (i=0;i<nSplits;i++)
		if (kmapi_success!=kmapi_parse_split(pKMnobs,KAPI_attribute4index(pKMnobs->pKnobs,"SPLIT_ISSUE",i),&bid))
			return kmapi_failure;
	pKMnobs->cSplitRules=nSplits;
	return kmapi_success;
}


kmapi_implicit_break_t  KMAPI_ImplicitBreakType(void *pConfig,kapi_bid_t bid, kapi_fu_t *mpfu)
{
	kmapi_knobs_t *pKMnobs=(kmapi_knobs_t *)pConfig;
	kmapi_split_rule_t splitRule=pKMnobs->dmpSplitRules[bid];
	bundle_fus_t *pException=splitRule.pSplitExeptions;
	int i;

	assert(pKMnobs!=NULL);

	while (pException!=NULL)
	{
		for (i=0;i<kmapi_BUNDLE_WIDTH;i++)
			if (isbv128BITSET(pException->bvSlots[i],mpfu[i]))
				return kmapi_no_implicit_break;
		pException=pException->pNext;
	}

	return splitRule.bvSplits;

}
