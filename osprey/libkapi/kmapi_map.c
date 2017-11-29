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
#include <stdio.h>
#include <string.h>
#include <assert.h>

//#include "kmapi.h"
#include "kmapi_internal.h"
#include "kmapi_error.h"

/* Globals */
int nBundlesPerCycle;


/* Resource Map creation and handling */
/**************************************/
kmapi_pResourceMap_t KMAPI_CreateResourceMap(void *pConfig)
{
	kmapi_knobs_t *pKMnobs=(kmapi_knobs_t *)pConfig;
	if (pKMnobs->iTotalResources>32)
		return NULL;
	return ((kmapi_pResourceMap_t)malloc(sizeof(bv32_t)));
}

void KMAPI_DestroyResourceMap(kmapi_pResourceMap_t mpMap)
{
	assert(mpMap!=NULL);
	free(mpMap);
}

void KMAPI_SetAllResources(kmapi_pResourceMap_t mpMap, kmapi_resource_status_e status)
{
	assert(mpMap!=NULL);
	if (status==kmapi_resource_free)
		ZERO_bv32(*mpMap);
	if (status==kmapi_resource_occupied)
		ONE_bv32(*mpMap);
}

kmapi_result KMAPI_SetResource(kmapi_pResourceMap_t mpMap, kmapi_resource_t iResource, kmapi_resource_status_e status)
{
	assert(mpMap!=NULL);
	if (iResource<0)
		return kmapi_failure;
	if (status==kmapi_resource_occupied)
	{
		SETBIT_bv32(*mpMap,iResource);
		return kmapi_success;
	}
	if (status==kmapi_resource_free)
	{
		UNSETBIT_bv32(*mpMap,iResource);
		return kmapi_success;
	}
	return kmapi_failure;
}

kmapi_result KMAPI_SetResourceMap(kmapi_pResourceMap_t mpMapDst, kmapi_pResourceMap_t mpMapSrc, kmapi_resource_status_e status)
{
	bv32_t bvTmp;
	if (status==kmapi_resource_occupied)
	{
		*mpMapDst=bv32OR(*mpMapDst,*mpMapSrc);
		return kmapi_success;
	}
	if (status==kmapi_resource_free)
	{
		bv32NOT(bvTmp,bv32AND(*mpMapDst,*mpMapSrc))
		*mpMapDst=bv32AND( bvTmp ,*mpMapDst);
		return kmapi_success;
	}
	return kmapi_failure;
}

kmapi_resource_status_e KMAPI_ResourceStatus(kmapi_pResourceMap_t mpMap, kmapi_resource_t iResource)
{
	assert(mpMap!=NULL);
	if (iResource<0)
		return kmapi_resource_invalid;
	if (isbv32BITSET(*mpMap,iResource))
		return kmapi_resource_occupied;
	else 
		return kmapi_resource_free;
}


/* Getting a resource list */
/**********************************************/
/* macros to access the 3d arrays holding the mapping info */
#define PBS(m_place,m_bid,m_slot) \
	(((m_place) * kapi_nBID * kmapi_BUNDLE_WIDTH)+((m_bid)*kmapi_BUNDLE_WIDTH)+(m_slot))
#define PSL(m_place,m_slot,m_syl) \
	(((m_place) * kmapi_BUNDLE_WIDTH * kapi_nSYL)+((m_slot)*kapi_nSYL)+(m_syl))

kmapi_fulist_t *kmapi_GetBundleResourceList(kmapi_knobs_t *pKMnobs,
											kmapi_bundle_info_t *pbinfo)
{
	return (pKMnobs->dm3pfuBinfoLists[PBS(pbinfo->iPlace,pbinfo->bid,pbinfo->iSlot)]);
}

kmapi_fulist_t *kmapi_GetFuResourceList(kmapi_knobs_t *pKMnobs,
											kapi_fu_t fuClass)
{
	return (pKMnobs->dmpfuFuclassLists[fuClass]);
}

/* Get kapi information for kmapi resource
	Return Values:
	kmapi_success if information updated.
	kmapi_invalid_input if invalid resource or configuration. */



/* Check if specific bundle,slot,fuclass have specific resources
   for allocation */
kmapi_fulist_t *kmapi_AllocateException(kmapi_knobs_t *pKMnobs,
								  kmapi_bundle_info_t *pbinfo,
								  kapi_fu_t fuClass)
{
	bv128_t bvFuClasses;
	kmapi_exception_t *pException=NULL;
	kapi_syl_t mpTmpSyls[ nSYLBID ];

	KAPI_SylOrder_bid(pKMnobs->pKnobs,pbinfo->bid,mpTmpSyls);

	pException=(pKMnobs->dm3pExceptions[PSL(pbinfo->iPlace,pbinfo->iSlot,mpTmpSyls[pbinfo->iSlot])]);
	while (pException!=NULL)
	{
		COPY_bv128(bvFuClasses,pException->Details.bvfuClasses);
		if ((pException->Details.bid==pbinfo->bid) && (isbv128BITSET(bvFuClasses,fuClass)))
			return pException->Details.pfuList;
		pException=pException->pNext;
	}
	return NULL;
}



/************************/
/* Allocation functions */
/************************/


kmapi_resource_t kmapi_getunset_resource(kmapi_pResourceMap_t mpMap)
{
	int i;
	for (i=0;i<32;i++)
	{
		if (isbv32BITSET(*mpMap,i))
		{
			UNSETBIT_bv32(*mpMap,i);
			return i;
		}
	}
	return -1;
}

kmapi_resource_t KMAPI_GetFirstAllocatedMapResource(kmapi_pResourceMap_t mpMap)
{
	int i=0;
	for (;i<32;i++)
	{
		if (isbv32BITSET(*mpMap,i))
		{
			return i;
		}
	}
	return -1;
}

void KMAPI_CopyResourceMap(kmapi_pResourceMap_t mpMapDst, kmapi_ResourceMap_t MapSrc)
{
	*mpMapDst=MapSrc;
}

int KMAPI_IsResourceMapClear(bv32_t *pbvMap)
{
	if isbv32ZERO(*pbvMap)
		return TRUE;
	return FALSE;
}

kmapi_resource_t KMAPI_GetNextAllocatedMapResource(bv32_t *pbvMap, kmapi_resource_t iPrevResource)
{
	int i=iPrevResource+1;

	if (iPrevResource<0)
		return -3;
	if (KMAPI_IsResourceMapClear(pbvMap))
		return -1;
	for (;i<32;i++)
	{
		if (isbv32BITSET(*pbvMap,i))
		{
			return i;
		}
	}
	return -2;
}
/* Allocation for Intel(R) Itanium(TM) processor:
	Use bundle info to find allowed resources.
	Scan all allowed resources to find a free resource that can service fuClass.
	If a matching resource was found, return sucess and update pAllocatedResource
	else return fail. */

kmapi_result  kmapi_AllocateForItanium(void *pConfig,
												  kmapi_pResourceMap_t mpMap, 
												  kmapi_bundle_info_t *pbinfo,
												  kapi_fu_t fuClass, 
												  kmapi_pResourceMap_t pAllocatedResources)
{
	kmapi_knobs_t *pKMnobs=(kmapi_knobs_t *)pConfig;
	kmapi_fulist_t *pOccupy=NULL;
	kmapi_resource_t tmpRes=-2;
	bv32_t bvTmp,bvTmpMap;

	pOccupy=kmapi_AllocateException(pKMnobs,pbinfo,fuClass);
	if (pOccupy==NULL)
		pOccupy=kmapi_GetBundleResourceList(pKMnobs,pbinfo);


	while (pOccupy!=NULL)
	{
		kapi_cluster_t clr;
		kapi_ut_t ut;
		kapi_cport_t cport;
		kapi_cutport_t cutport;
		kapi_port_t port;

		/* find free resources that can be used for allocation. */
		/* pOccupy->bvResourceMap holds needes resources,		*/
		/* mpMap has has bit set for each occupied resoucre		*/
		if (!isbv32ZERO(bv32AND(*mpMap,pOccupy->bvResourceMap)))
			pOccupy=pOccupy->pNext;
		else 
		{ /*  can the port found be used by the fu ?*/
			/* tmpRes comes out as -1 if search ended, and all fu's 
			could be used, and as -2 if even one fu could not be used */
			COPY_bv32(bvTmpMap,pOccupy->bvResourceMap);
			while ((tmpRes=kmapi_getunset_resource(&bvTmpMap))!=-1)
			{
				ZERO_bv32(bvTmp);
				KMAPI_ResourceInfo(pConfig,tmpRes,&port,&clr,&cport,&ut,&cutport);
				SETBIT_bv32(bvTmp,cport);
				/* if resource found can be used, check next */
				if (!(isbv32ZERO(bv32AND(KAPI_cportMask4fu(pKMnobs->pKnobs,clr,fuClass),bvTmp))))
					continue;
				else /* else keep searching */
				{
					tmpRes=-2;
					break; /* out of the while on resource validity check per map */
				}
			} /* end of while on resources in map */
			/* Allocation was successful? */
			if (tmpRes!=-2)
			{
				break; /* out of while on resource maps */
			} else /* if could not allocate, try next */
				pOccupy=pOccupy->pNext;
		} /* end of else */
	} /* end of while on pOccupy */
	/* if allocation was successful */
	if ((pOccupy!=NULL) && (tmpRes!=-2)) 
	{
		(*pAllocatedResources)=pOccupy->bvResourceMap;
		return kmapi_success;
	} 

	return kmapi_failure;

}


/*	This function uses pConfig to determine allocation scheme,
	and activates the correct mapping function.
	If allocation succeeded return value is kmapi_success.
	If allocation failed return value is kmapi_failure.
	If no allocation scheme found for the configuration defined 
	return value is kmapi_invalid_input.
*/
kmapi_result  KMAPI_MapInstructionToPort(void *pConfig,
												  kmapi_pResourceMap_t mpMap, 
												  kmapi_bundle_info_t *pbinfo, 
												  kapi_iid_t iid, 
												  kmapi_pResourceMap_t pAllocatedResource)
{
	kapi_fu_t fuClass;
	int dummy=0;
	kmapi_knobs_t *pKMnobs=(kmapi_knobs_t *)pConfig;

	/* sanity */
	assert(pConfig!=NULL);
	assert(pAllocatedResource!=NULL);
	assert(mpMap!=NULL);


	fuClass=KAPI_iid2fu(pKMnobs->pKnobs,iid,dummy);

	/* activate selected funtion */
	if (pKMnobs->iScheme==ALLOCATE_FOR_ITANIUM) {
		assert(pbinfo!=NULL);
		return kmapi_AllocateForItanium(pConfig,mpMap,pbinfo,fuClass,pAllocatedResource);
	} else
	{
		return kmapi_invalid_input;
	}
	return kmapi_invalid_input;

}


/* Kapi interaction (Initializations and conversion routines) */
/**************************************************************/

void fulist_AddNewNode(kmapi_fulist_t **pfulist,kmapi_ResourceMap_t res)
{
	kmapi_fulist_t *plistWalker=NULL;
	kmapi_fulist_t *NewNode=NULL;
	/* create new node */
	NewNode=(kmapi_fulist_t *)malloc(sizeof(kmapi_resource_t));
	NewNode->pNext=NULL;
	COPY_bv32(NewNode->bvResourceMap,res);

	/* add at end since order is important. 
	If list becomes to long, will need to add tail pointer */
	if (*pfulist!=NULL)
	{
		plistWalker=*pfulist;
		while (plistWalker->pNext!=NULL)
			plistWalker=plistWalker->pNext;
		plistWalker->pNext=NewNode;
	} else
		*pfulist=NewNode;
}

/* parse resource, return value is addr to end, iRes is resource */
/* clusterX\utY/cportYZ											 */
/* currently mapping is X*16+(kapi_ut_t)Y*4+Z								 */
char *kmapi_parse_resource(char *pchResourceStart, kmapi_resource_t *iRes)
{
	int iTmp=0;
	int iHash=0;
	char *pchWalker=pchResourceStart;
	if (pchWalker==strstr(pchWalker,"cluster"))
	{
		pchWalker+=strlen("cluster");
		if (  ((iTmp=((*pchWalker)-'0')) >= 0) && (iTmp < 2)  )
		{
			pchWalker++;
			iHash=16*iTmp;
			if (pchWalker==strstr(pchWalker,"\\ut"))
			{
				pchWalker+=strlen("\\ut");
				switch(*pchWalker)
				{
				case 'I':
					iTmp=kapi_utI;
					break;
				case 'M':
					iTmp=kapi_utM;
					break;
				case 'F':
					iTmp=kapi_utF;
					break;
				case 'B':
					iTmp=kapi_utB;
					break;
				default:
					kmapi_Error_pch1(1,"Wrong resource definition for mapping: %s",pchResourceStart);
				}
				pchWalker++;
				iHash+=4*iTmp;
				if (pchWalker==strstr(pchWalker,"/cport"))
				{
					pchWalker+=strlen("/cportY"); /* skip the ut descriptor */
					iTmp=((*pchWalker)-'0');
					if ( (iTmp >= 0) && (iTmp <= 4)  )
					{
						*iRes=iHash+iTmp;
						pchWalker++;
						return (pchWalker);
					} else 
						kmapi_Error_pch1(1,"Could not create resource definition hash for: %s",pchResourceStart);
						/* cport number if */
				} /* cport if */
			} /* ut if */
		} /* cluster number if */
	}  /* cluster if */
	kmapi_Error_pch1(1,"Wrong resource definition for mapping: %s",pchResourceStart);
	return NULL;
}



char *kmapi_create_resource_list(char *pchResList,kmapi_fulist_t **ppfulist)
{
	char *pchWalker=pchResList;
	kmapi_ResourceMap_t bvMap;
	kmapi_resource_t iRes;
	pchWalker=(char *)strchr(pchWalker,'(');
	if (pchWalker==NULL)
		return NULL;
	pchWalker++;
	KMAPI_SetAllResources(&bvMap,kmapi_resource_free);
	/* now create list of possible resources */
	{
		while (*pchWalker != ')')
		{
			/* hash resource to kmapi resource */
			pchWalker=kmapi_parse_resource(pchWalker,&iRes);
			KMAPI_SetResource(&bvMap,iRes,kmapi_resource_occupied);

			/* pchWalker++; */
			switch (*pchWalker)
			{
			case ',': 
				pchWalker++;
			case ')': 
				fulist_AddNewNode(ppfulist,bvMap);
				KMAPI_SetAllResources(&bvMap,kmapi_resource_free);
				break;
			case '+':
				pchWalker++;
				break;
			default:
				kmapi_Error_pch1(1,"Wrong resource definition for mapping: %s",pchWalker);
				return NULL;
			} /* of switch */
		} /* of while */
	} /* of list creation*/
	return pchWalker;
}

kmapi_result kmapi_parse_option(kmapi_knobs_t *pKMnobs,char *pchOption, int *pPlace, int *pSlot, 
						kapi_syl_t *pSyl, kmapi_fulist_t **ppfulist)
{
	char *pchTmp=NULL;
	char *pchWalker=NULL;
	kmapi_fulist_t *pfulist=NULL;
	int iSlot=0,iPlace=0;

	pchTmp=strdup(pchOption);
	/* tmp now of the form:
	(xxx,xxx)=(T,T....) where x is - or syllable, and T is port or "rule" */
	pchWalker=strchr(pchTmp,'(');
	if (pchWalker==NULL)
		return kmapi_failure;
	/* first find place and syl type */
	pchWalker++;
	while (*pchWalker != ')')
	{
		switch (*pchWalker)
		{
		case 'I':
			*pSyl=kapi_sylI;
			*pSlot=iSlot;
			*pPlace=iPlace;
			break;
		case 'M':
			*pSyl=kapi_sylM;
			*pSlot=iSlot;
			*pPlace=iPlace;
			break;
		case 'F':
			*pSyl=kapi_sylF;
			*pSlot=iSlot;
			*pPlace=iPlace;
			break;
		case 'B':
			*pSyl=kapi_sylB;
			*pSlot=iSlot;
			*pPlace=iPlace;
			break;
		case 'L':
			*pSyl=kapi_sylL;
			*pSlot=iSlot;
			*pPlace=iPlace;
			break;
		case '-':
			iSlot++;
			break;
		case ',':
			iSlot=0;
			iPlace++;
			break;
		default:
			/*assumed space*/
			kmapi_Warning("Option format may be incorrect");
		}
		pchWalker++;
	}
	pchWalker=kmapi_create_resource_list(pchWalker,&pfulist);
	if (pchWalker==NULL)
		return kmapi_failure;
	*ppfulist=pfulist;


	/* cleanup */
	free(pchTmp);
	return kmapi_success;
}

/* Intel(R) Itanium(TM) processor exeption format (b1,b2[..])/(yyy,yyy[..])/(f1,f2...)=(p1,p2...) where:
      - b1 is the first template, b2 is the second template "ANY" for
        any.
      - one of the y is the syllable affected by this exeption, the     
        other must all be -.
      - fi are the fu classes for which the exeption is valid, or "ANY" 
        for any.
      - pi are the available ports for this specific set of conditions, 
        overriding the dispersal option.
*/

kmapi_exception_t *kmapi_MakeException()
{
	kmapi_exception_t *pException=(kmapi_exception_t *)malloc(sizeof(kmapi_exception_t));
	pException->pNext=NULL;
	ZERO_bv128(pException->Details.bvfuClasses);
	pException->Details.pfuList=NULL;
	pException->Details.bid=-1;
	return pException;

}

kmapi_result kmapi_parse_exception(kmapi_knobs_t *pKMnobs,char *pchException,int *pPlace,kapi_bid_t *pBid,int *pSlot,kapi_syl_t *pSyl,kmapi_exception_t **pResult)
{
	char *pchWalker;
	char *pchTmp;
	char *pchBid;
	char *pchFu;
	int fEND=FALSE;
	int iPlace=0,fUnique=1,tmpSlot=0;
	kapi_bid_t bid=-1;
	kapi_fu_t fu=-1;
	kmapi_exception_t *pException=kmapi_MakeException();


	pchTmp=strdup(pchException);
	pchWalker=strchr(pchTmp,'(');
	pchWalker++;
	pchBid=pchWalker;
	fEND=FALSE;

	/* find bid & place */
	while (!fEND)
	{
		pchWalker++;
		switch (*pchWalker)
		{
		case ')':
			fEND=TRUE;
		case ',':
			*pchWalker='\0';
			if (strstr(pchBid,"ANY")==NULL)
			{
				if (bid!=-1)
				{
					kmapi_Error_pch1(0,"Can't handle more then one specific bundlein exeption: %s",pchException);
					return kmapi_failure;
				}
				bid=KAPI_EnumIndex(pKMnobs->pKnobs,"bid_t",pchBid);
				if (bid==-1) {
					kmapi_Error_pch1(0,"Unrecognized bid: %s",pchBid);
					return kmapi_failure;
				}
				pException->Details.bid=bid;
			} 
			pchBid=++pchWalker;
			break;
		default:
			break;
		}
	} /* of file to find bid */
	/* find place/slot/syl */
	pchWalker=strchr(pchWalker,'(');
	pchWalker++;
	*pSyl=kapi_nSYL;
	*pSlot=-1;
	while (*pchWalker!=')')
	{

		switch (*pchWalker)
		{
		case 'I':
			*pSyl=kapi_sylI;
			fUnique--;
			break;
		case 'M':
			*pSyl=kapi_sylM;
			fUnique--;
			break;
		case 'F':
			*pSyl=kapi_sylF;
			fUnique--;
			break;
		case 'B':
			*pSyl=kapi_sylB;
			fUnique--;
			break;
		case 'L':
			*pSyl=kapi_sylL;
			fUnique--;
			break;
		case '-':
			tmpSlot++;
			break;
		case ',':
			iPlace++;
			tmpSlot=0;
			break;
		}
		if (*pSyl!=kapi_nSYL) /* found right syl */
		{
			if (fUnique<0) 
			{
				kmapi_Error_pch1(0,"Can't have more then one syl affected per exception: %s",pchException);
				return kmapi_failure;
			} 
			if (*pSlot==-1) /* first time after finding syl */
			{
				*pSlot=tmpSlot;
				*pPlace=iPlace;
			}
		}
		pchWalker++;
	} /* found slot */
	/* find fu's affected */
	pchWalker=strchr(pchWalker,'(');
	pchWalker++;
	pchFu=pchWalker;
	fEND=FALSE;
	while (!fEND)
	{
		pchWalker++;
		switch (*pchWalker)
		{
		case ')':
			fEND=TRUE;
		case ',':
			*pchWalker='\0';
			if (strstr(pchFu,"ANY")!=NULL)
			{
				ONE_bv128(pException->Details.bvfuClasses);
			}
			else 
			{
				fu=KAPI_EnumIndex(pKMnobs->pKnobs,"fu_t",pchFu);
				if (fu==-1) {
					kmapi_Error_pch1(0,"Unrecognized fu: %s",pchFu);
					return kmapi_failure;
				}
				SETBIT_bv128(pException->Details.bvfuClasses,fu);
			}
			pchFu=++pchWalker;
		}
	} /* found fus affected */
	/* get resource list */
	/* at this stage, pchwalker: "=(...)". need to get rid of "=".
	   ~ for better reliability, check that = is truly the next char. */
	pchWalker++;
	pchWalker=kmapi_create_resource_list(pchWalker,&pException->Details.pfuList);
	if (pchWalker==NULL)
		return kmapi_failure;
	
	free(pchTmp);

	/* success: set return values */
	*pResult=pException;
	*pBid=bid;

	return kmapi_success;
}


kmapi_result kmapi_process_options_table(kmapi_knobs_t *pKMnobs, kmapi_fulist_t **dm3pOptions)
{
	int nOptions,i;
	char *pchOption;
	kapi_syl_t syl;
	int place,slot;
	kmapi_fulist_t *pfulist;
	
	nOptions=KAPI_count4attribute(pKMnobs->pKnobs,"DISPERSAL_OPTION");
	for (i=0;i<nOptions;i++)
	{
		pchOption=KAPI_attribute4index(pKMnobs->pKnobs,"DISPERSAL_OPTION",i);
		if (kmapi_parse_option(pKMnobs,pchOption,&place,&slot,&syl,&pfulist)!=kmapi_success)
			return kmapi_failure;
		dm3pOptions[PSL(place,slot,syl)]=pfulist;
	}
	return kmapi_success;
}

kmapi_result kmapi_process_exceptions_table(kmapi_knobs_t *pKMnobs)
{
	int nExceptions,i;
	char *pchException;
	kapi_bid_t bid;
	int place,slot;
	kapi_syl_t syl;
	kmapi_exception_t *pException=NULL;;
	
	nExceptions=KAPI_count4attribute(pKMnobs->pKnobs,"DISPERSAL_EXCEPTION");
	for (i=0;i<nExceptions;i++)
	{
		pchException=KAPI_attribute4index(pKMnobs->pKnobs,"DISPERSAL_EXCEPTION",i);
		if(kmapi_parse_exception(pKMnobs,pchException,&place,&bid,&slot,&syl,&pException)!=kmapi_success)
			return kmapi_failure;
		if (NULL == pKMnobs->dm3pExceptions[PSL(place,slot,syl)])
			pKMnobs->dm3pExceptions[PSL(place,slot,syl)]=pException;
		else 
			pKMnobs->dm3pExceptions[PSL(place,slot,syl)]->pNext=pException;
	}
	return kmapi_success;
}


/* initialize the fu lists according to bundle info from knobs info */
kmapi_result kmapi_init_fulist_tables(kmapi_knobs_t *pKMnobs)
{
	int place,bid,slot;
	kapi_syl_t mpTmpSyls[ nSYLBID ];
	kmapi_fulist_t **dm3pOptions;

	pKMnobs->cOptions=(nBundlesPerCycle * kmapi_BUNDLE_WIDTH * kapi_nSYL);
	dm3pOptions=(kmapi_fulist_t **)malloc(sizeof(kmapi_fulist_t *) * pKMnobs->cOptions);
	memset(dm3pOptions,0,(sizeof(kmapi_fulist_t *) * pKMnobs->cOptions));
	if (kmapi_process_options_table(pKMnobs,dm3pOptions)!=kmapi_success)
		return kmapi_failure;

	pKMnobs->cExceptions=nBundlesPerCycle * kmapi_BUNDLE_WIDTH * kapi_nSYL;
	pKMnobs->dm3pExceptions=(kmapi_exception_t **)malloc(sizeof(kmapi_exception_t *) * pKMnobs->cExceptions);
	memset(pKMnobs->dm3pExceptions,0,(sizeof(kmapi_exception_t *) * pKMnobs->cExceptions));
	if (kmapi_process_exceptions_table(pKMnobs)!=kmapi_success)
		return kmapi_failure;

	for (place=0;place<nBundlesPerCycle;place++)
		for (bid=kapi_bidFIRST;bid<kapi_nBID;bid++) {
			KAPI_SylOrder_bid(pKMnobs->pKnobs,bid,mpTmpSyls);
			for (slot=0;slot<kmapi_BUNDLE_WIDTH;slot++){
				pKMnobs->dm3pfuBinfoLists[PBS(place,bid,slot)]=
					dm3pOptions[PSL(place,slot,mpTmpSyls[slot])];
			} /* of slot */
		} /* of bid */

	pKMnobs->dm3pOptions=dm3pOptions;
	return kmapi_success;

}


kmapi_result kmapi_init_misc_fu_info(kmapi_knobs_t *pKMnobs)
{

	bv_t *pbv;
	int fu;
	int nFu=KAPI_EnumCardinality(pKMnobs->pKnobs,"fu_t");

	for (fu=0;fu<nFu;fu++)
	{
		pbv=KAPI_GetBvVariable(pKMnobs->pKnobs,"MISC_FU_INFO",fu);
		if (pbv!=NULL)
			pKMnobs->dmpMiscFuInfo[fu]=*(pbv->pint32Data);
		else
			pKMnobs->dmpMiscFuInfo[fu]=0;
	}
	return kmapi_success;
}

void *KMAPI_initialize(void *pIA64Config)
{
	kmapi_knobs_t *pKMnobs=(kmapi_knobs_t *)malloc(sizeof(kmapi_knobs_t));
	char *pchMachine;

	nBundlesPerCycle=KAPI_BundleIssueWidth(pIA64Config,-1);

	memset(pKMnobs,0,sizeof(kmapi_knobs_t));

	pKMnobs->pKnobs=(knobs_t *)pIA64Config;
	pKMnobs->iTotalResources=32;
	if ( KAPI_EnumCardinality(pIA64Config,"fu_t") > 128 )
	{
		kmapi_Error(0,"This version of kmapi supports only up to 128 fu's: %s");
		free(pKMnobs);
		return NULL;
	}
	/* 3d list of [place of bundle in issue group][bundle type][slot] */
	pKMnobs->cBinfoLists=(nBundlesPerCycle * kapi_nBID * kmapi_BUNDLE_WIDTH);
	pKMnobs->dm3pfuBinfoLists=(kmapi_fulist_t **)malloc(sizeof(kmapi_fulist_t *) * pKMnobs->cBinfoLists);
	pKMnobs->dmpMiscFuInfo=(bv32_t *)malloc(sizeof(bv32_t)*(KAPI_EnumCardinality(pIA64Config,"fu_t")+1));
	memset(pKMnobs->dm3pfuBinfoLists,0,(sizeof(kmapi_fulist_t *) * pKMnobs->cBinfoLists));
	if ((kmapi_init_fulist_tables(pKMnobs)!=kmapi_success) ||
		(kmapi_init_split_issue_data(pKMnobs)!=kmapi_success) ||
		(kmapi_init_misc_fu_info(pKMnobs)!=kmapi_success) ||
		(kmapi_err_counter!=0) )
	{
		KMAPI_finalize(pKMnobs);
		return NULL;
	}
	pchMachine=KAPI_GetStringVariable(pIA64Config,"UARCH",0);
	/* choose allocation funtion */
	if (strstr(pchMachine,"ITANIUM")!=NULL) {
		pKMnobs->iScheme=ALLOCATE_FOR_ITANIUM;
	} else
	{
		pKMnobs->iScheme=ALLOCATE_INVALID;
	}
	return ((void *)(pKMnobs));
}

void KMAPI_finalize(void *pKmapiConfig)
{
	kmapi_knobs_t *pKMnobs=(kmapi_knobs_t *)pKmapiConfig;

	if (pKMnobs==NULL)
		return;

	if (pKMnobs->dm3pfuBinfoLists!=NULL)
		free(pKMnobs->dm3pfuBinfoLists);
	if (pKMnobs->dm3pOptions!=NULL)
	{
		/*~ need to free resources lists as well */
		free(pKMnobs->dm3pOptions);
	}
	if (pKMnobs->dm3pExceptions!=NULL)
	{
		/*~ need to free resource lists as well */
		free(pKMnobs->dm3pExceptions);
	}
	if (pKMnobs->dmpSplitRules!=NULL)
	{
		/*~ need to free all fulists allocated for exceptions */
		free(pKMnobs->dmpSplitRules);
	}

	if (pKMnobs->dmpMiscFuInfo!=NULL)
	{
		free(pKMnobs->dmpMiscFuInfo);
	}

	kmapi_err_counter=0;
	free(pKMnobs);
}

kmapi_result KMAPI_ResourceInfo(void *pConfig,kmapi_resource_t iResource,
								 kapi_port_t *pport,
								 kapi_cluster_t *pclr,kapi_cport_t *pcport,
								 kapi_ut_t *put,kapi_cutport_t *pcutport)
{
	kmapi_knobs_t *pKMnobs=(kmapi_knobs_t *)pConfig;

	/* sanity */
	assert(pConfig!=NULL);

	/* init */
	if ((iResource<0) || (iResource>(pKMnobs->iTotalResources-1)))
		return kmapi_invalid_input;

	/* get info */
	if (pKMnobs->iScheme==ALLOCATE_FOR_ITANIUM) {
		*pclr=iResource/16;
		iResource-=16*(*pclr);
		*put=iResource/4;
		iResource-=4*(*put);
		*pcutport=iResource;
		KAPI_cutportInfo(pKMnobs->pKnobs,*pclr,*put,*pcutport,pport,pcport);
		return kmapi_success;
	}
	return kmapi_invalid_input;
}


kmapi_result KMAPI_AllocationOptions4fu(void *pConfig, kapi_fu_t fu, kapi_it_t it, kmapi_allocation_option_t *pOptions)
{
	int i,j,nOptions=0;
	cportinfo_t *pcportinfo;
	bv32_t bv32=0,bvUsedForLong;
	bv32_t bvArray[MAX_OPTIONS];
	kapi_cluster_t clrArray[MAX_OPTIONS];
	kapi_cluster_t cluster=0;
	kmapi_knobs_t *pKMnobs=(kmapi_knobs_t *)pConfig;
	knobs_t *pknobs = pKMnobs->pKnobs;
	itinfo_t *pitinfo;
	kapi_syl_t syl;
	int nUtNeeded=0;
	kapi_ut_t ut;
	bv32_t utMask=0, utMaskTmp; /* utMask holds the required unit types mask */


	if (NULL==pConfig)
		return kmapi_failure;
	if (NULL==pOptions)
		return kmapi_failure;

	pitinfo = &(pknobs->dmpitinfoTable[it]);
	if (NULL==pitinfo)
		return kmapi_failure;
	/* check the number of unit types required for specific instruction type */
	for ( syl=kapi_sylFIRST;syl<=kapi_sylLAST;syl++ ) 
	{
		/* first check if the syllable is valid */
		if (!( pitinfo->bv32sylAllowed & ( 1 << syl ) ) )
			continue;
		utMask=0;
		utMaskTmp=0;
		nUtNeeded=0;
		cluster=0;
		/* find out which unit types are needed */
		for (ut=0;ut<kapi_nUT;ut++)
		{
			 nUtNeeded+=pknobs->dmpsylinfoTable[ syl ].mpnutNeeded[ ut ];
			 if (pknobs->dmpsylinfoTable[ syl ].mpnutNeeded[ ut ]>0)
				SETBIT_bv32(utMask,ut);
		}
		 /* find options for this syllable */
		while ( cluster < pknobs->nclr ) 
		{
			clr_t *pclr;

			bvUsedForLong=0;
			pclr = &(pknobs->mpclrTable[ cluster ]);


			for ( i=0; i<pclr->ncports; i++ ) 
			{
				pcportinfo = pclr->mppcportinfoTable[ i ];

				/* *** this implementation assumes that no more then one ut of 
					   a type will be needed for any specific syl!!! 
					   (i.e possible one b, one f, one i but impossible 2 i */
				if (1==nUtNeeded)
				{ /* if only one unit needed */
					if ( isbvBITSET( &(pcportinfo->bvfuAllowed), fu ) &&
						 isbv32BITSET(utMask,(pcportinfo->ut))			) 
					{
						bvArray[nOptions]=( 1 << i );
						clrArray[nOptions]=cluster;
						nOptions++;
					}
				} /* end if only one unit needed */ 
				else
				{ /* each option includes more then one unit */

					/* check if port was already used for another option */
					if (isbv32BITSET(bvUsedForLong,i))
						continue;

					if ( isbvBITSET( &(pcportinfo->bvfuAllowed), fu ) &&
						 isbv32BITSET(utMask,pcportinfo->ut)			) 
					{
					/* found first needed, mark and look for the rest! */
						utMaskTmp=utMask; /* utMaskTmp holds the required unit types */
						bvArray[nOptions]=( 1 << i );
						clrArray[nOptions]=cluster;
						SETBIT_bv32(bvUsedForLong,i);
						UNSETBIT_bv32(utMaskTmp,pcportinfo->ut);
					} else
						continue;
					
					 /* while not all unit types satisfied, keep looking */
					for (j=0;
						 (j<pclr->ncports) && (utMaskTmp!=0);
						 j++)
					{
						/* check if port was already used for another option */
						if (isbv32BITSET(bvUsedForLong,j))
							continue;
						pcportinfo = pclr->mppcportinfoTable[ j ];
						/* if unit required and allowed for fu, 
						   mark as used, and mark unit type satisfied */
						if (isbv32BITSET(utMaskTmp,pcportinfo->ut) &&
							isbvBITSET( &(pcportinfo->bvfuAllowed), fu) )
						{
							bv32 = (1<<j);
							if ((bvUsedForLong & bv32)==0) /* not used yet */
							{
								bvUsedForLong|=bv32; /* Mark port used */
								bvArray[nOptions]|=bv32;
								/* mark unit type satisfied */
								UNSETBIT_bv32(utMaskTmp,pcportinfo->ut);
							}
						}
					}
					nOptions++;
				} /* end if more then one unit type needed */
			} /* for loop on cports */
			cluster++;
		} /* while loop on clusters */
	} /* for loop on syllables */


	pOptions->nOptions=nOptions;
	pOptions->dmpOptions=(bv32_t *)malloc(nOptions*sizeof(bv32_t));
	pOptions->dmpClrID=(bv32_t *)malloc(nOptions*sizeof(kapi_cluster_t));

	while (--nOptions>=0)
	{
		pOptions->dmpOptions[nOptions]=bvArray[nOptions];
		pOptions->dmpClrID[nOptions]=clrArray[nOptions];
	}

	return kmapi_success;
}

kmapi_result KMAPI_ClearAllocationOptions(kmapi_allocation_option_t *pOptions)
{
	assert(pOptions->dmpClrID!=NULL);
		free(pOptions->dmpClrID);
	assert(pOptions->dmpOptions!=NULL);
		free(pOptions->dmpOptions);
	pOptions->nOptions=0;
	return kmapi_success;
}

kmapi_result KMAPI_nAllocationOptions4syl(void *pConfig, kapi_syl_t syl, int *iOptions)
{
	bv32_t bv32=0;
	kapi_cluster_t cluster=0;
	kmapi_knobs_t *pKMnobs=(kmapi_knobs_t *)pConfig;
	knobs_t *pknobs = pKMnobs->pKnobs;
	int nUtNeeded=0;
	kapi_ut_t ut;

	if (NULL==pConfig)
		return kmapi_failure;
	if (NULL==iOptions)
		return kmapi_failure;

	/* init Options to max */
	*iOptions=0x0ffff;

	/* check the number of unit types required for specific syl,
	   and take the min from available / needed */
	for (ut=0;ut<kapi_nUT;ut++)
	{
		assert(pknobs->mpnut[ut] > 0);
		nUtNeeded=pknobs->dmpsylinfoTable[ syl ].mpnutNeeded[ ut ];
		if ( (nUtNeeded > 0)									&&  
			 ((*iOptions) > (pknobs->mpnut[ut] / nUtNeeded ))	)
			*iOptions=pknobs->mpnut[ut] / nUtNeeded;
	}

	return kmapi_success;
}

kmapi_Resource_Array_t *KMAPI_GetRawOptions(void *pKmapiConfig, int iIssueSlot, kapi_syl_t iSyl)
{
#define MAX_RESOURCE_OPTIONS	10 
	kmapi_knobs_t *pKMnobs=(kmapi_knobs_t *)pKmapiConfig;
	int iPlace;
	int iSlot;
	kmapi_fulist_t *pOptions;
	kmapi_Resource_Array_t *pResourceArray=(kmapi_Resource_Array_t *) 
		malloc(sizeof(kmapi_Resource_Array_t));

	int nOptions=0;
	kmapi_ResourceMap_t *pResources=(kmapi_ResourceMap_t *) 
		malloc(sizeof(kmapi_ResourceMap_t) * MAX_RESOURCE_OPTIONS);

	assert(pKmapiConfig!=NULL);
	
	iPlace=(iIssueSlot >=3 ? 1 : 0);
	iSlot=iIssueSlot % 3;

	pOptions=pKMnobs->dm3pOptions[PSL(iPlace,iSlot,iSyl)];
	while (pOptions!=NULL)
	{
		pResources[nOptions]=pOptions->bvResourceMap;
		nOptions++;
		pOptions=pOptions->pNext;
	}
	
	pResourceArray->nResources=nOptions;
	pResourceArray->dmResourceMaps=(kmapi_ResourceMap_t *)malloc( nOptions * sizeof(kmapi_ResourceMap_t) );
	while (--nOptions>=0)
	{
		pResourceArray->dmResourceMaps[nOptions]=pResources[nOptions];
	}
	
	return pResourceArray;
}
