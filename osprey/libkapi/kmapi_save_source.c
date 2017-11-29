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
#include "kmapi_internal.h"
#define pknobs pKMnobs
#include "kapi_saver_utils.h"



/*
Save a list of:
typedef struct _KMAPI_FULIST_T {
	kmapi_ResourceMap_t bvResourceMap;
	struct _KMAPI_FULIST_T *pNextFu;
} kmapi_fulist_t;
Return success or fail.
*/

kmapi_result kmapi_save_as_header_FuList_struct(FILE *fp, kmapi_knobs_t *pKMnobs ,kmapi_fulist_t *pResList, char *pchName, FILE *fpTables)
{
	static kmapi_save_as_header_FuList_struct_num=0;
	char *MyName;
	char pchTmp[200];
	FILE *fpTmp=fp;
	kmapi_fulist_t *pWalker=pResList;
	int iIndex=0;

	/* Don't bother with empty lists */
	if (pResList==NULL)
	{
		ADD_NULL;
		ADD_NEWLINE;
		return kmapi_success;
	}

	/* init names */
	kmapi_save_as_header_FuList_struct_num++;
	*pchTmp='\0';
	sprintf(pchTmp,"fu_list_%d",kmapi_save_as_header_FuList_struct_num);
	MyName=make_string_attach(pchName,pchTmp);

	/* save list as an array, with each pointer pointing to the next entry. */
	fpTmp=fpTables;
	ADD_STRUCT_ARRAY_HEADER("kmapi_fulist_t",MyName);
	while (pWalker!=NULL)
	{
		fprintf(fpTmp,"{%#x,",pWalker->bvResourceMap);
		if (pWalker->pNext!=NULL)
			fprintf(fpTmp,"&%s[%d]},\n",MyName,++iIndex);
		else 
			fprintf(fpTmp,"NULL}\n");
		pWalker=pWalker->pNext;
	}
	END_ARRAY;

	fpTmp=fp;
	fprintf(fpTmp,"%s\n",MyName);

	return kmapi_success;
}


/*
typedef struct _KMAPI_EXCEPTION_DATA_T {
	bv128_t bvfuClasses;
	kmapi_fulist_t *pfuList;
} kmapi_exception_data_t;
*/
kmapi_result kmapi_save_as_header_Exception_Data_struct(FILE *fp,kmapi_knobs_t *pKMnobs,kmapi_exception_data_t *pExceptionDetails,char *pchName,FILE *fpTables)
{
	static kmapi_save_as_header_Exception_Data_struct=0;
	char *MyName;
	char pchTmp[200];
	FILE *fpTmp=fp;

	kmapi_save_as_header_Exception_Data_struct++;
	*pchTmp='\0';
	sprintf(pchTmp,"exception_data_%d",kmapi_save_as_header_Exception_Data_struct);
	MyName=make_string_attach(pchName,pchTmp);

	KAPI_save_as_header_int_list(fpTmp,pExceptionDetails->bid);
	/* save the bv128 - 4 ints */
	KAPI_save_as_header_bv128(fpTmp,&(pExceptionDetails->bvfuClasses));
	ADD_COMMA;
	kmapi_save_as_header_FuList_struct(fpTmp,pKMnobs,pExceptionDetails->pfuList,MyName,fpTables);


	return kmapi_success;
}

/*
typedef struct _KMAPI_EXCEPTION_T {
	kmapi_exception_data_t Details;
	struct _KMAPI_EXCEPTION_T *pNext;
} kmapi_exception_t;
*/

kmapi_result kmapi_save_as_header_ExceptionList_struct(FILE *fp, kmapi_knobs_t *pKMnobs ,kmapi_exception_t *pExceptionList, char *pchName, FILE *fpTables)
{
	static kmapi_save_as_header_ExceptionList_struct_num=0;
	char *MyName;
	char pchTmp[200];
	FILE *fpTmp=tmpfile();
	kmapi_exception_t *pWalker=pExceptionList;
	int iIndex=0;

	/* Don't bother with empty lists */
	if (pExceptionList==NULL)
	{
		fclose(fpTmp);
		fpTmp=fp;
		ADD_NULL;
		ADD_NEWLINE;
		return kmapi_success;
	}

	/* init names */
	kmapi_save_as_header_ExceptionList_struct_num++;
	*pchTmp='\0';
	sprintf(pchTmp,"exception_list_%d",kmapi_save_as_header_ExceptionList_struct_num);
	MyName=make_string_attach(pchName,pchTmp);

	/* save list as an array, with each pointer pointing to the next entry. */
	ADD_STRUCT_ARRAY_HEADER("kmapi_exception_t",MyName);
	while (pWalker!=NULL)
	{
		START_STRUCT;
			START_STRUCT;
			kmapi_save_as_header_Exception_Data_struct(fpTmp,pKMnobs,&(pWalker->Details),MyName,fpTables);
			CLOSE_STRUCT;
			ADD_COMMA;
		if (pWalker->pNext!=NULL)
			fprintf(fpTmp,"&%s[%d]},\n",MyName,++iIndex);
		else 
			fprintf(fpTmp,"NULL}\n");

		pWalker=pWalker->pNext;
	}
	END_ARRAY;

	append_tmp(fpTables, fpTmp);

	fpTmp=fp;
	fprintf(fpTmp,"%s\n",MyName);

	return kmapi_success;
}


/*typedef struct _BUNDLE_FUS_T {
	bv128_t bvSlots[kmapi_BUNDLE_WIDTH];
	struct _BUNDLE_FUS_T *pNext;
} bundle_fus_t;
*/
kmapi_result kmapi_save_as_header_bundle_fu_list(FILE *fp,kmapi_knobs_t *pKMnobs, bundle_fus_t *pbfu, char *pchName,FILE *fpTables)
{
	static int kmapi_save_as_header_bundle_fu_list_num=0;
	char *MyName;
	char pchTmp[200];
	FILE *fpTmp=fp;
	int iIndex=0;
	bundle_fus_t *pWalker=pbfu;
	int i;


	/* Don't bother with empty lists */
	if (pbfu==NULL)
	{
		ADD_NULL;
		ADD_NEWLINE;
		return kmapi_success;
	}

	kmapi_save_as_header_bundle_fu_list_num++;
	*pchTmp='\0';
	sprintf(pchTmp,"bundle_fulist_%d",kmapi_save_as_header_bundle_fu_list_num);
	MyName=make_string_attach(pchName,pchTmp);

	/* save list as an array, with each pointer pointing to the next entry. */
	fpTmp=fpTables;
	ADD_STRUCT_ARRAY_HEADER("bundle_fus_t",MyName);
	while (pWalker!=NULL)
	{
		START_STRUCT;
			START_ARRAY;
				for (i=0;i<kmapi_BUNDLE_WIDTH;i++) 
				{
					KAPI_save_as_header_bv128(fpTmp,&(pWalker->bvSlots[i]));
					if (i<(kmapi_BUNDLE_WIDTH-1)) ADD_COMMA;
				}
			CLOSE_ARRAY;
			ADD_COMMA;
			if (pWalker->pNext!=NULL)
				fprintf(fpTmp,"&%s[%d]},\n",MyName,++iIndex);
			else 
				fprintf(fpTmp,"NULL\n}");

		pWalker=pWalker->pNext;
	}
	END_ARRAY;

	fpTmp=fp;
	fprintf(fpTmp,"%s\n",MyName);

	return kmapi_success;
}


/* typedef struct _KMAPI_SPLIT_RULE_T {
	kapi_bid_t bid;
	bv32_t bvSplits;
	bundle_fus_t *pSplitExeptions;
} kmapi_split_rule_t;
*/
kmapi_result kmapi_save_as_header_SplitRule_struct(FILE *fp, kmapi_knobs_t *pKMnobs ,kmapi_split_rule_t *pRule, char *pchName, FILE *fpTables)
{
	static int kmapi_save_as_header_SplitRule_struct_num=0;
	char *MyName;
	char pchTmp[200];
	FILE *fpTmp=fp;

	kmapi_save_as_header_SplitRule_struct_num++;
	*pchTmp='\0';
	sprintf(pchTmp,"Split_Rule_%d",kmapi_save_as_header_SplitRule_struct_num);
	MyName=make_string_attach(pchName,pchTmp);
	
	KAPI_save_as_header_int_list(fp,pRule->bid);
	KAPI_save_as_header_int_list(fp,pRule->bvSplits);
	kmapi_save_as_header_bundle_fu_list(fp, pKMnobs, pRule->pSplitExeptions, MyName, fpTables);

	return kmapi_success;
}

kmapi_result KMAPI_save_as_header_allinfo_struct(FILE *fp,kmapi_knobs_t *pKMnobs, char *pchKmapiName, char *pchKapiName, FILE *fpTables)
{
	FILE *fpTmp=tmpfile();
	char *pchBinfoName;
	char *pchOptionsName;
	char *pchExceptionName;
	char *pchFuclassListsName;
	char *pchSplitRulesName;
	char *pchMiscFuInfoName;

	pchBinfoName=make_string_attach(pchKmapiName,"Allocation_Option_Table_By_PBS");
	pchOptionsName=make_string_attach(pchKmapiName,"Allocation_Option_Table_By_PSL");
	pchExceptionName=make_string_attach(pchKmapiName,"Allocation_Exceptions_Table_By_PSL");
	pchSplitRulesName=make_string_attach(pchKmapiName,"Split_Rules_Table");
	pchMiscFuInfoName=make_string_attach(pchKmapiName,"Misc_Fu_Info_array");
	pchFuclassListsName="NULL";


	ADD_PTR_STRUCT_ARRAY("kmapi_fulist_t *",pchBinfoName,pKMnobs->cBinfoLists,
		pKMnobs->dm3pfuBinfoLists,kmapi_save_as_header_FuList_struct);
	ADD_PTR_STRUCT_ARRAY("kmapi_fulist_t *",pchOptionsName,pKMnobs->cOptions,
		pKMnobs->dm3pOptions,kmapi_save_as_header_FuList_struct);
	ADD_PTR_STRUCT_ARRAY("kmapi_exception_t *",pchExceptionName, pKMnobs->cExceptions,
		pKMnobs->dm3pExceptions,kmapi_save_as_header_ExceptionList_struct);
	ADD_STRUCT_ARRAY("kmapi_split_rule_t",pchSplitRulesName,KAPI_EnumCardinality(pKMnobs->pKnobs,"bid_t"),
		pKMnobs->dmpSplitRules,kmapi_save_as_header_SplitRule_struct);

	ADD_STRUCT_ARRAY_HEADER("bv32_t",pchMiscFuInfoName);
	KAPI_save_as_header_bv32_array(fpTmp,pKMnobs->dmpMiscFuInfo,(KAPI_EnumCardinality(pKMnobs->pKnobs,"fu_t")+1));
	END_ARRAY;
	


    append_tmp(fpTables,fpTmp); /* tables containing lists */

	if (pchKapiName!=NULL)
	{
		KAPI_save_as_header_all_IA64_info(fpTables,pKMnobs->pKnobs,pchKapiName);
	} else
		pchKapiName="NULL";

	/* now save the structure */
	fpTmp=fp;

	KAPI_save_as_header_name_list(fpTmp,pchKapiName);
	KAPI_save_as_header_int_list(fpTmp,pKMnobs->cBinfoLists);
	KAPI_save_as_header_name_list(fpTmp,pchBinfoName);
	KAPI_save_as_header_int_list(fpTmp,pKMnobs->cOptions);
	KAPI_save_as_header_name_list(fpTmp,pchOptionsName);
	KAPI_save_as_header_int_list(fpTmp,pKMnobs->cFuclassLists);
	KAPI_save_as_header_name_list(fpTmp,pchFuclassListsName);
	KAPI_save_as_header_int_list(fpTmp,pKMnobs->cExceptions);
	KAPI_save_as_header_name_list(fpTmp,pchExceptionName);
	KAPI_save_as_header_int_list(fpTmp,pKMnobs->cSplitRules);
	KAPI_save_as_header_name_list(fpTmp,pchSplitRulesName);
	KAPI_save_as_header_int_list(fpTmp,pKMnobs->iTotalResources);
	KAPI_save_as_header_int_list(fpTmp,pKMnobs->iScheme);
	ADD_NEWLINE;
	KAPI_save_as_header_name(fpTmp,pchMiscFuInfoName);
	

	return kmapi_success;

}


kmapi_result kmapi_save_as_header_allinfo_saver( FILE *fp, void *pConfig , char *pchKmapiName, char *pchKapiName, int *piReserved)
{
	kmapi_knobs_t *pKMnobs=(kmapi_knobs_t *)pConfig;
	FILE *fpTmp=fp;
	FILE *fpTmpTablesFile=NULL;
	char pchVersion[200];
	char pchHeader[]="#include \"kmapi_internal.h\"\n";

	/* sanity */
	if (NULL==fp) return kmapi_invalid_input;
	if (NULL==pConfig) 
	{
		ADD_COMMENT("no valid configuration\n");
		return kmapi_invalid_input;
	}
	if (NULL==pchKmapiName)
	{
		ADD_COMMENT("no name for saving kmapi structures\n");
		return kmapi_invalid_input;
	}
	/* prepare tmp files */
	fpTmp=tmpfile();
	fpTmpTablesFile=tmpfile();
	if ((NULL==fpTmp) || (NULL==fpTmpTablesFile))
		return kmapi_internal_error;
	/* save the struct */
	ADD_EXTERN_STRUCT_HEADER("kmapi_knobs_t",pchKmapiName);
	KMAPI_save_as_header_allinfo_struct( fpTmp, pKMnobs , pchKmapiName, pchKapiName, fpTmpTablesFile);
	END_STRUCT;
	/* concat the tmp files and close them */
	fwrite(pchHeader,sizeof(char),strlen(pchHeader),fp);
	append_tmp(fp,fpTmpTablesFile);
	append_tmp(fp,fpTmp);
	/* save version for checks ...*/
	sprintf(pchVersion,"double %s_kmapi_version = %#f\n;",pchKmapiName,KMAPI_GetInternalVersion());

	return kmapi_success;
}



kmapi_result KMAPI_save_as_header_all_kmapi_info( FILE *fp, void *pConfig , char *pchKmapiInfoName, char *pchKapiInfoName)
{
	int iRes;
	int i;

	/* save */
	iRes=kmapi_save_as_header_allinfo_saver(fp,pConfig,pchKmapiInfoName,pchKapiInfoName,&i);

	/* cleanup and return */
	return iRes;
}

kmapi_result KMAPI_fEnableKmapiCalls_from_header(void **pConfig, void *pKMAPI_HeaderConfig, void *pKAPI_IA64_HeaderConfig, int iReserved)
{
	kmapi_knobs_t *pKMnobs;

	if (NULL==pKMAPI_HeaderConfig)
		return kmapi_invalid_input;
	pKMnobs=(kmapi_knobs_t *)pKMAPI_HeaderConfig;
	if (NULL!=pKAPI_IA64_HeaderConfig)
		pKMnobs->pKnobs=(knobs_t *)pKAPI_IA64_HeaderConfig;
	
	if (NULL==pKMnobs->pKnobs)
	{ /* if the knobs info was not initialized by either header - return error */
		return kmapi_invalid_input;
	}
	*pConfig=pKMnobs;
	return kmapi_success;

}

kmapi_result KMAPI_fDisableKmapiCalls_from_header(void **pConfig)
{
	*pConfig=NULL;
	return kmapi_success;
}
