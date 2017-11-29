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

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include <malloc.h>
#include "kapi_internal.h"
#include "kapi_parse.h"
#include "kapi_util.h"
#include "kapi_error.h"
#include "kapi_save_source_ia64.h"


/**********************************************************
Various utilities for saving (adding commas, int->str etc.)
***********************************************************/
/* global for reference in multiple functions */
char *FieldNames[enum_LAST_KAPI_FIELD];


/* Macros & globals */
/*-----------*/

#define API_VER_STR "4_0"


#include "kapi_saver_utils.h"



/***************************************************************
ia64 information saving functions
****************************************************************/


void KAPI_save_as_header_MachineDescription_struct( FILE *fp, knobs_t *pknobs ,char *pchName, FILE *fpTables)
{
   int  i;
   char *myName;
   char pchTmp[200];
   FILE *fpTmp=tmpfile();
   char *mpCacheNames[cache_type_enum_size];
   /* Sanity */
	assert(NULL!=fp);
	assert(NULL!=pknobs);
	assert(NULL!=pchName);


   /* Assign Names */
	myName=make_string_attach(pchName,API_VER_STR);
	FieldNames[enum_mpclrTable]=make_string_attach(myName,"cluster_info");
	FieldNames[enum_pfSaveHeaderFlags]=make_string_attach(myName,"saved_fields_info");
	FieldNames[enum_dmpfuinfoTable]=make_string_attach(myName,"fu_info");
	FieldNames[enum_dmpebyInterTable]=make_string_attach(myName,"intercluster_bypass_info");
	FieldNames[enum_dmpabyIntraTable]=make_string_attach(myName,"intracluster_bypass_info");
	FieldNames[enum_mppportinfoTable]=make_string_attach(myName,"port_info");
	FieldNames[enum_dmpinstTable]=make_string_attach(myName,"instruction_info");
	FieldNames[enum_dmpitinfoTable]=make_string_attach(myName,"it_info");
	FieldNames[enum_dmpbidinfoTable]=make_string_attach(myName,"bundle_info");
	FieldNames[enum_dmpsylinfoTable]=make_string_attach(myName,"syl_info");
	FieldNames[enum_dmpcacheTable]=make_string_attach(myName,"cache_info");



    /* Save tables */
	/* ----------- */
	/* tables will be saved in fpTables */

	ADD_STRUCT_ARRAY_ON_FLAG(pknobs->pfSaveHeaderFlags[enum_pfSaveHeaderFlags],"knobs_fields_enum_t", FieldNames[enum_pfSaveHeaderFlags],enum_LAST_KAPI_FIELD,
		pknobs->pfSaveHeaderFlags,KAPI_save_as_header_saver_flags);
	ADD_PTR_STRUCT_ARRAY_ON_FLAG(pknobs->pfSaveHeaderFlags[enum_mppportinfoTable],"cportinfo_t", FieldNames[enum_mppportinfoTable],pknobs->nports,
		pknobs->mppportinfoTable,KAPI_save_as_header_PortInfo_struct);
	ADD_STRUCT_ARRAY_ON_FLAG(pknobs->pfSaveHeaderFlags[enum_dmpebyInterTable],"eby_t", FieldNames[enum_dmpebyInterTable],pknobs->nRawInter,
		pknobs->dmpebyInterTable,KAPI_save_as_header_InterClusterBypassInfo_struct);
	ADD_STRUCT_ARRAY_ON_FLAG(pknobs->pfSaveHeaderFlags[enum_dmpabyIntraTable],"aby_t", FieldNames[enum_dmpabyIntraTable],pknobs->nRawIntra,
		pknobs->dmpabyIntraTable,KAPI_save_as_header_IntraClusterBypassInfo_struct);
	ADD_STRUCT_ARRAY_ON_FLAG(pknobs->pfSaveHeaderFlags[enum_dmpfuinfoTable],"fuinfo_t", FieldNames[enum_dmpfuinfoTable],pknobs->nfuinfoTable,
		pknobs->dmpfuinfoTable,KAPI_save_as_header_FunctionalUnitInfo_struct);
	ADD_STRUCT_ARRAY_ON_FLAG(pknobs->pfSaveHeaderFlags[enum_dmpinstTable],"inst_t", FieldNames[enum_dmpinstTable],pknobs->ninstTable,
		pknobs->dmpinstTable,KAPI_save_as_header_InstructionInfo_struct);
	ADD_STRUCT_ARRAY_ON_FLAG(pknobs->pfSaveHeaderFlags[enum_dmpitinfoTable],"itinfo_t", FieldNames[enum_dmpitinfoTable],pknobs->nitinfoTable,
		pknobs->dmpitinfoTable,KAPI_save_as_header_InstructionTypeInfo_struct);
	ADD_STRUCT_ARRAY_ON_FLAG(pknobs->pfSaveHeaderFlags[enum_dmpbidinfoTable],"bidinfo_t", FieldNames[enum_dmpbidinfoTable],pknobs->nbidinfoTable,
		pknobs->dmpbidinfoTable,KAPI_save_as_header_BundleInfo_struct);
	ADD_STRUCT_ARRAY_ON_FLAG(pknobs->pfSaveHeaderFlags[enum_dmpsylinfoTable],"sylinfo_t", FieldNames[enum_dmpsylinfoTable],pknobs->nsylinfoTable,
		pknobs->dmpsylinfoTable,KAPI_save_as_header_SyllableInfo_struct);
	for (i=0;i<cache_type_enum_size;i++){
		*pchTmp='\0';
		sprintf(pchTmp,"%d",i);
		mpCacheNames[i]=make_string_attach(FieldNames[enum_dmpcacheTable],pchTmp);
		ADD_STRUCT_ARRAY_ON_FLAG(pknobs->pfSaveHeaderFlags[enum_dmpcacheTable],"cache_t",mpCacheNames[i] ,pknobs->nCacheLevels[i],
			pknobs->dmpcacheTable[i],KAPI_save_as_header_Cache_struct);
	}

   /* Save structure */
   /* -------------- */
   /* First append all the tables in the temporary file to the tables file */
   append_tmp(fpTables,fpTmp);
   /* structures will be saved in fp */	
   fpTmp=fp;

   ADD_COMMENT("General info, always saved");
   KAPI_save_as_header_name_list(fp,FieldNames[enum_pfSaveHeaderFlags]);
   ADD_COMMENT("Bundle issue width info");
   KAPI_save_as_header_int_array_list(fp,pknobs->maxBundleIssue,maxCLR);
   ADD_COMMENT("Instruction issue width info");
   KAPI_save_as_header_int_array_list(fp,pknobs->maxInstructionIssue,maxCLR);
   ADD_COMMENT("misc parsing info, never saved (fImplicitNone,cntLinesDeltaFile,cntLinesBaselineFile");
   for (i=0;i<3;i++)
	KAPI_save_as_header_int_list(fp,0);
   ADD_COMMENT("Tool:");
   KAPI_save_as_header_String_list(fp,pknobs->pchToolname);
   ADD_NEWLINE;

   /* Cluster info */
   ADD_COMMENT("nClusters");
   KAPI_save_as_header_int_list(fp,pknobs->nclr);
   ADD_NEWLINE;
   ADD_COMMENT("Cluster Info:");
   ADD_NEWLINE;
   START_ARRAY;
   SAVE_STRUCT_ARRAY(maxCLR,pknobs->mpclrTable,
	   KAPI_save_as_header_ClusterInfo_struct,FieldNames[enum_mpclrTable]);
   CLOSE_ARRAY;
   ADD_COMMA;
   ADD_NEWLINE;
   ADD_COMMENT("Intercluster bypass info");
   ADD_COMMENT("Entries,Table Name:");
   KAPI_save_as_header_int_list(fp,pknobs->nRawInter);
   KAPI_save_as_header_name_list(fp,FieldNames[enum_dmpebyInterTable]);
   ADD_NEWLINE;
   ADD_COMMENT("IntraCluster bypass info");
   ADD_COMMENT("Entries,Table Name:");
   KAPI_save_as_header_int_list(fp,pknobs->nRawIntra);
   KAPI_save_as_header_name_list(fp,FieldNames[enum_dmpabyIntraTable]);
   ADD_NEWLINE;
   /* Ports info (global) */
   ADD_COMMENT("port info");
   ADD_COMMENT("global:");
   KAPI_save_as_header_int_list(fp,pknobs->nports);
   START_STRUCT;
   for (i=0;i<(maxPORTS);i++)
   {
	   *pchTmp='\0';
	   if ((NULL==pknobs->mppportinfoTable[i]) || (0==pknobs->pfSaveHeaderFlags[enum_mppportinfoTable]))
		   sprintf(pchTmp,"NULL");
	   else
		   sprintf(pchTmp,"&(%s[%d])",FieldNames[enum_mppportinfoTable],i);
	   KAPI_save_as_header_name(fp,pchTmp);
	   if (i<(maxPORTS-1)) ADD_COMMA;
   }
   CLOSE_STRUCT;
   ADD_COMMA;
   ADD_NEWLINE;
   ADD_COMMENT("port to cluster mapping");
   KAPI_save_as_header_int_array_list(fp,pknobs->port2cluster,32);
   ADD_NEWLINE;
   ADD_COMMENT("global unit type information (number of units of each type)");
   KAPI_save_as_header_int_array_list(fp,pknobs->mpnut,kapi_nUT);
   ADD_NEWLINE;
   /*fu's & instructions information */
   ADD_COMMENT("fu info");
   ADD_COMMENT("Entries,Table Name:");
   KAPI_save_as_header_int_list(fp,pknobs->nfuinfoTable);
   KAPI_save_as_header_name_list(fp,FieldNames[enum_dmpfuinfoTable]);
   ADD_NEWLINE;
   ADD_COMMENT("instruction information");
   ADD_COMMENT("Entries,Table Name:");
   KAPI_save_as_header_int_list(fp,pknobs->ninstTable);
   KAPI_save_as_header_name_list(fp,FieldNames[enum_dmpinstTable]);
   ADD_NEWLINE;
   ADD_COMMENT("instruction type information");
   ADD_COMMENT("Entries,Table Name:");
   KAPI_save_as_header_int_list(fp,pknobs->nitinfoTable);
   KAPI_save_as_header_name_list(fp,FieldNames[enum_dmpitinfoTable]);
   ADD_NEWLINE;
   ADD_COMMENT("bundle information");
   ADD_COMMENT("Entries,Table Name:");
   KAPI_save_as_header_int_list(fp,pknobs->nbidinfoTable);
   KAPI_save_as_header_name_list(fp,FieldNames[enum_dmpbidinfoTable]);
   ADD_NEWLINE;
   ADD_COMMENT("syllable information");
   ADD_COMMENT("Entries,Table Name:");
   KAPI_save_as_header_int_list(fp,pknobs->nsylinfoTable);
   KAPI_save_as_header_name_list(fp,FieldNames[enum_dmpsylinfoTable]);
   ADD_NEWLINE;
   ADD_COMMENT("cache info:");
   KAPI_save_as_header_int_array_list(fp,pknobs->nCacheLevels,cache_type_enum_size);
   START_ARRAY;
   for (i=0;i<cache_type_enum_size;i++) {
	   KAPI_save_as_header_name(fp,mpCacheNames[i]);
	   if (i<cache_type_enum_size-1) ADD_COMMA;
   }
   CLOSE_ARRAY;
   ADD_COMMA;


   /* symbol table information, never saved */
/*  struct _SYM_TABLE_NODE *dmpstn; 
	int nstn;
	int fRestructuredAttributes;
	struct _VALUE_NODE *pvalnTypeList;   
	struct _VALUE_NODE *pvalnVarList;    
	struct _VALUE_NODE *pvalnAttrList;   
*/
   KAPI_save_as_header_name_list(fp,"NULL");
   KAPI_save_as_header_int_list(fp,0);
   KAPI_save_as_header_int_list(fp,0);
   KAPI_save_as_header_name_list(fp,"NULL");
   KAPI_save_as_header_name_list(fp,"NULL");
   KAPI_save_as_header_name(fp,"NULL");
   ADD_NEWLINE;
   /* cleanup */
	free(myName);
	free(FieldNames[enum_mpclrTable]);
	free(FieldNames[enum_pfSaveHeaderFlags]);
	free(FieldNames[enum_dmpfuinfoTable]);
	free(FieldNames[enum_dmpebyInterTable]);
	free(FieldNames[enum_dmpabyIntraTable]);
	free(FieldNames[enum_mppportinfoTable]);
	free(FieldNames[enum_dmpinstTable]);
	free(FieldNames[enum_dmpitinfoTable]);
	free(FieldNames[enum_dmpbidinfoTable]);
	free(FieldNames[enum_dmpsylinfoTable]);
	for (i=0;i<cache_type_enum_size;i++)
		free(mpCacheNames[i]);

}

void KAPI_save_as_header_saver_flags(FILE *fp,knobs_t *pknobs,int *pFlag, char *pchName, FILE *fpTables)
{
	FILE *fpTmp=fp;
	KAPI_save_as_header_int(fp,*pFlag);
}

void KAPI_save_as_header_ClusterInfo_struct(FILE *fp,knobs_t *pknobs,clr_t *pclr,char *pchName, FILE *fpTables)
{
	int i;
	FILE *fpTmp=tmpfile();
	char *myName;
	char *PortTableNames[kapi_nUT];
	char pchTmp[200];
    kapi_ut_t ut;
    kapi_cutport_t icutport;


	/* Sanity */
	/* Init Names */
	*pchTmp='\0';
	sprintf(pchTmp,"%d",pclr->idxclr);
	myName=make_string_attach(pchName,pchTmp);
	if (0!=pknobs->pfSaveHeaderFlags[enum_mpclrTable])
	{
		for (i=0;i<kapi_nUT;i++)
		{
			*pchTmp='\0';
			sprintf(pchTmp,"ut%d",i);
			PortTableNames[i]=make_string_attach(myName,pchTmp);
		}
		/* Save Related Tables */
		for (i=0;i<(kapi_nUT);i++)
		{
			if (pclr->mpncutport[i]>0) {
				ADD_STRUCT_ARRAY("cportinfo_t", PortTableNames[i],pclr->mpncutport[i],
					pclr->dmppcportinfoTable[i],KAPI_save_as_header_PortInfo_struct);
			} else
				ADD_NULL_PTR("cportinfo_t", PortTableNames[i]);
		}
	}

	/* Save Struct */
   append_tmp(fpTables,fpTmp);
   fpTmp=fp;

   if (0==pknobs->pfSaveHeaderFlags[enum_mpclrTable])
	   ADD_COMMENT("Dummy entry for fixed sized array");
   /* cluster number */
	ADD_COMMENT("id:");
	KAPI_save_as_header_int_list(fp,pclr->idxclr);
   /* number of ports in cluster and ports info*/
	ADD_COMMENT("ncports");
	KAPI_save_as_header_int_list(fp,pclr->ncports);
   /* 1d array of pointers to structures */
	ADD_COMMENT("ports:");
	START_ARRAY;
    for (i=0;i<(maxCPORTS);i++)
	{
	   *pchTmp='\0';

	   if ((NULL==pclr->mppcportinfoTable[i]) || (0==pknobs->pfSaveHeaderFlags[enum_mpclrTable]))
		   sprintf(pchTmp,"NULL");
	   else
	   {
		   ut=pclr->mppcportinfoTable[i]->ut;
		   icutport=pclr->mppcportinfoTable[i]->cutport;
		   sprintf(pchTmp,"&(%s[%d])",PortTableNames[ut],icutport);
	   }
	   KAPI_save_as_header_name(fp,pchTmp);
	   if (i<(maxCPORTS-1)) ADD_COMMA;
	}
	CLOSE_ARRAY;
    ADD_COMMA;
	ADD_NEWLINE;
   /* 2d array of cport structures, length of 2nd D in mpncutport */
	START_ARRAY;
    for (i=0;i<(kapi_nUT);i++)
	 {
	   *pchTmp='\0';
	   if (NULL==pclr->dmppcportinfoTable[i] || (0==pknobs->pfSaveHeaderFlags[enum_mpclrTable]))
		   sprintf(pchTmp,"NULL");
	   else
		   sprintf(pchTmp,"%s",PortTableNames[i]);
	   KAPI_save_as_header_name(fp,pchTmp);
	   if (i<(kapi_nUT-1)) ADD_COMMA;
	 }
	CLOSE_ARRAY;
    ADD_COMMA;
	ADD_COMMENT("size of 2nd D for the 2D array above");
	KAPI_save_as_header_int_array_list(fp,pclr->mpncutport,kapi_nUT);
   /* cport to global port mapping */
    KAPI_save_as_header_int_array_list(fp,pclr->cport2port,32);
   /* distance to other clr */
    KAPI_save_as_header_int_array(fp,pclr->distClr,maxCLR);
	ADD_NEWLINE;
	/* cleanup */
	for (i=0;i<kapi_nUT;i++)
		free(PortTableNames[i]);
	free(myName);
}

void KAPI_save_as_header_InterClusterBypassInfo_struct(FILE *fp,knobs_t *pknobs,eby_t *peby,char *pchName,FILE *fpTables)
{
	FILE *fpTmp=fp;
	/* Inter Cluster Bypass Info: */
	ADD_COMMENT("clrSrc");
	KAPI_save_as_header_int_list(fp,peby->clusterSrc);
	ADD_COMMENT("clrDest");
	KAPI_save_as_header_int_list(fp,peby->clusterDest);
	ADD_COMMENT("fuSrc");
	KAPI_save_as_header_int_list(fp,peby->fuSrc);
	ADD_COMMENT("fuDest");
	KAPI_save_as_header_int_list(fp,peby->fuDest);
	ADD_COMMENT("cutportSrc");
	KAPI_save_as_header_int_list(fp,peby->cutportSrc);
	ADD_COMMENT("cutportDest");
	KAPI_save_as_header_int_list(fp,peby->cutportDest);
	ADD_COMMENT("utSrc");
	KAPI_save_as_header_int_list(fp,peby->utSrc);
	ADD_COMMENT("utDest");
	KAPI_save_as_header_int_list(fp,peby->utDest);
	ADD_COMMENT("oppSrc");
	KAPI_save_as_header_int_list(fp,peby->oppSrc);
	ADD_COMMENT("oppDest");
	KAPI_save_as_header_int_list(fp,peby->oppDest);
	ADD_COMMENT("Value");
	KAPI_save_as_header_int(fp,peby->iValue);
	ADD_NEWLINE;
}

void KAPI_save_as_header_IntraClusterBypassInfo_struct(FILE *fp,knobs_t *pknobs,aby_t *paby, char *pchName,FILE *fpTables)
{
	FILE *fpTmp=fp;
	/* Intra Cluster Bypass Info: */
	ADD_COMMENT("clr:");
	KAPI_save_as_header_int_list(fp,paby->cluster);
	ADD_COMMENT("fuSrc");
	KAPI_save_as_header_int_list(fp,paby->fuSrc);
	ADD_COMMENT("fuDest");
	KAPI_save_as_header_int_list(fp,paby->fuDest);
	ADD_COMMENT("cutportSrc");
	KAPI_save_as_header_int_list(fp,paby->cutportSrc);
	ADD_COMMENT("cutportDest");
	KAPI_save_as_header_int_list(fp,paby->cutportDest);
	ADD_COMMENT("utSrc");
	KAPI_save_as_header_int_list(fp,paby->utSrc);
	ADD_COMMENT("utDest");
	KAPI_save_as_header_int_list(fp,paby->utDest);
	ADD_COMMENT("oppSrc");
	KAPI_save_as_header_int_list(fp,paby->oppSrc);
	ADD_COMMENT("oppDest");
	KAPI_save_as_header_int_list(fp,paby->oppDest);
	ADD_COMMENT("Value");
	KAPI_save_as_header_int_list(fp,paby->iValue);
	ADD_COMMENT("Entry");
    KAPI_save_as_header_String(fp,paby->pchEntry);
	ADD_NEWLINE;
}

void KAPI_save_as_header_PortInfo_struct(FILE *fp,knobs_t *pknobs,cportinfo_t *pcport,char *pchName,FILE *fpTables)
{
	FILE *fpTmp=fpTables;
	char *myName=NULL;

	fpTmp=fp;
	ADD_COMMENT("port(ut,cp,cut):");
	KAPI_save_as_header_int_list(fp,pcport->ut);
	KAPI_save_as_header_int_list(fp,pcport->cport);
	KAPI_save_as_header_int_list(fp,pcport->cutport);
	ADD_COMMENT("bv-fu:");
	SAVE_STRUCT(KAPI_save_as_header_BitVector_struct(fp,
		&(pcport->bvfuAllowed),
		myName,fpTables));
}

void KAPI_save_as_header_FunctionalUnitInfo_struct(FILE *fp,knobs_t *pknobs,fuinfo_t *pfut,char *pchName,FILE *fpTables)
{
	FILE *fpTmp=fp;
	char comment[50];
	sprintf(comment,"fu: %s",pfut->pchName);
	ADD_COMMENT(comment);
	KAPI_save_as_header_int_list(fp,pfut->fu);
	ADD_COMMENT("core latencies");
	KAPI_save_as_header_int_array_list(fp,pfut->iCoreLatency,maxDESTINATIONS);
	ADD_COMMENT("ndests & dest names");
	KAPI_save_as_header_int_list(fp,pfut->cntDest);
	KAPI_save_as_header_String_array_list_always(fp,pfut->mppchDestName,pfut->cntDest);
	ADD_COMMENT("nsrcs & src names");
	KAPI_save_as_header_int_list(fp,pfut->cntSrc);
	KAPI_save_as_header_String_array_list_always(fp,pfut->mppchSrcName,pfut->cntSrc);
	KAPI_save_as_header_bv32_list(fp,pfut->bv32InfoBits);
    KAPI_save_as_header_String(fp,pfut->pchName);
}

void KAPI_save_as_header_InstructionInfo_struct(FILE *fp,knobs_t *pknobs,inst_t *pinst,char *pchName,FILE *fpTables)
{
	FILE *fpTmp=fp;
#ifdef ADD_INSTRUCTION_STRINGS
    KAPI_save_as_header_String_list_always(fp,pinst->pchUniqueName);
    KAPI_save_as_header_String_list_always(fp,pinst->pchMnemonic);
#else
    KAPI_save_as_header_String_list(fp,pinst->pchUniqueName);
    KAPI_save_as_header_String_list(fp,pinst->pchMnemonic);
#endif
    /* function unit class character representation */
	KAPI_save_as_header_String_list(fp,pinst->pchfu);
	/* it character representation */
    KAPI_save_as_header_String_list(fp,pinst->pchit);
	/* instruction type */
	ADD_COMMENT("it:");
	KAPI_save_as_header_int_list(fp,pinst->it);
	/* function unit restrictions/byp/latency */
	ADD_COMMENT("fu:");
    KAPI_save_as_header_int(fp,pinst->fu);
	ADD_NEWLINE;
}

void KAPI_save_as_header_InstructionTypeInfo_struct(FILE *fp,knobs_t *pknobs,itinfo_t *pit,char *pchName,FILE *fpTables)
{
	FILE *fpTmp=fp;
	char *myName=NULL;
	ADD_COMMENT("it:");
	KAPI_save_as_header_int_list(fp,pit->it);
	ADD_COMMENT("Available:");
	KAPI_save_as_header_int_list(fp,pit->maxAvail);
    KAPI_save_as_header_String_list(fp,pit->pchitName);
	ADD_COMMENT("syllables allowed");
	/* bit vector 32bit == int! */
	KAPI_save_as_header_bv32(fp,pit->bv32sylAllowed);
	ADD_NEWLINE;
}

void KAPI_save_as_header_BundleInfo_struct(FILE *fp,knobs_t *pknobs,bidinfo_t *pbid,char *pchName,FILE *fpTables)
{
	FILE *fpTmp=fp;
	/* syllables comprising this bundle type */
	ADD_COMMENT("syl types:");
	KAPI_save_as_header_int_array_list(fp,(int *)(pbid->mpsylType),nSYLBID);
	/* number of syllables of each type */
	ADD_COMMENT("available syls:");
	KAPI_save_as_header_int_array_list(fp,pbid->mpnsylAvail,kapi_nSYL);
	/* number of instructions of each type possible */
	ADD_COMMENT("available it's:");
	KAPI_save_as_header_int_array_list(fp,pbid->mpnitAvail,kapi_nIT);
	/* is this a reserved bid (flag) */
	ADD_COMMENT("flag:");
	KAPI_save_as_header_int_list(fp,pbid->fReserved);
	/* 0 == no sbit, 1, 2 legal values */
	ADD_COMMENT("stop bit slot:");
	KAPI_save_as_header_int_list(fp,pbid->isylSbit);
    /* string rep of expected bid */
    KAPI_save_as_header_String(fp,pbid->pchBID);
	ADD_NEWLINE;
}

void KAPI_save_as_header_SyllableInfo_struct(FILE *fp,knobs_t *pknobs,sylinfo_t *psyl, char *pchName,FILE *fpTables)
{
	FILE *fpTmp=fp;
	/* number of syllables of each type */
	ADD_COMMENT("uts needed:");
	KAPI_save_as_header_int_array_list(fp,psyl->mpnutNeeded,kapi_nUT);
	ADD_COMMENT("it:");
	KAPI_save_as_header_int(fp,psyl->itMajor);
}

void KAPI_save_as_header_CachePortInfo_struct( FILE *fp, knobs_t *pknobs, cacheport_t *pcaport , char *pchName, FILE *fpTables)
{
	FILE *fpTmp=fp;

	KAPI_save_as_header_bv32(fp,pcaport->bv32AccessMode);
}

void KAPI_save_as_header_Cache_struct( FILE *fp, knobs_t *pknobs, cache_t *pcache , char *pchName, FILE *fpTables)
{
	FILE *fpTmp;
	char *myName;
	char pchTmp[200];
	int i;
	static int struct_num=0;
	
	/* init names */
	
	*pchTmp='\0';
	struct_num++;
	sprintf(pchTmp,"N_%d",struct_num);
	myName=make_string_attach(pchName,pchTmp);
	/* save struct */
	fpTmp=fp;
	ADD_COMMENT("nLines");
	KAPI_save_as_header_int_list(fp,pcache->nLines);
	ADD_COMMENT("nBytesLine");
	KAPI_save_as_header_int_list(fp,pcache->nBytesLine);
	ADD_COMMENT("nWays");
	KAPI_save_as_header_int_list(fp,pcache->nWays);
	ADD_NEWLINE;
	ADD_COMMENT("nCachePorts,portinfo array");
	KAPI_save_as_header_int_list(fp,pcache->nCachePorts);
	START_ARRAY;
    for (i=0;i<(KAPI_MAX_CACHE_PORTS_IMPL);i++)
	 {
		KAPI_save_as_header_CachePortInfo_struct(fp,pknobs,&(pcache->cacheportInfo[i]),myName,fpTables);	   
		if (i<(KAPI_MAX_CACHE_PORTS_IMPL-1)) ADD_COMMA;
	 }
	CLOSE_ARRAY;
	ADD_COMMA;
	ADD_COMMENT("iWritePolicy");
	KAPI_save_as_header_int_list(fp,pcache->iWritePolicy);
	ADD_COMMENT("iReplPolicy");
	KAPI_save_as_header_int_list(fp,pcache->iReplPolicy);
	ADD_COMMENT("iAllocPolicy");
	KAPI_save_as_header_int(fp,pcache->iAllocPolicy);
	ADD_NEWLINE;
}

int KAPI_save_as_header_MachineDescription_saver( FILE *fp, void *pConfig , char *pchName, int *pfSaveFlags)
{
	knobs_t *pknobs=(knobs_t *)pConfig;
	FILE *fpTmp=fp;
	FILE *fpTmpTablesFile=NULL;
	char pchVersion[200];
	char pchHeader[]="#include \"kapi_internal.h\"\n";

	/* sanity */
	if (NULL==fp) return -1;
	if (NULL==pConfig) 
	{
		ADD_COMMENT("no valid configuration\n");
		return -1;
	}
	if (NULL==pchName)
	{
		ADD_COMMENT("no name for saving ia64 structures\n");
		return -1;
	}
	/* prepare tmp files */
	fpTmp=tmpfile();//tmpfile();
	fpTmpTablesFile=tmpfile();//tmpfile();
	if ((NULL==fpTmp) || (NULL==fpTmpTablesFile))
		return -2;
	/* save the struct */
	pknobs->pfSaveHeaderFlags=pfSaveFlags;
	ADD_EXTERN_STRUCT_HEADER("knobs_t",pchName);
	KAPI_save_as_header_MachineDescription_struct( fpTmp, pknobs , pchName, fpTmpTablesFile);
	END_STRUCT;
	pknobs->pfSaveHeaderFlags=NULL;
	/* concat the tmp files and close them */
	fwrite(pchHeader,sizeof(char),strlen(pchHeader),fp);
	append_tmp(fp,fpTmpTablesFile);
	append_tmp(fp,fpTmp);
	/* save version for checks ...*/
	sprintf(pchVersion,"double %s_kapi_ia64_version = %#f\n;",pchName,KAPI_ia64_GetInternalVersion());
	return 0;
}

int KAPI_save_as_header_all_IA64_info( FILE *fp, void *pConfig , char *pchName)
{
	int pfSaveFlags[enum_LAST_KAPI_FIELD];
	int iRes;
	int i;

	memset(pfSaveFlags,0,sizeof(int)*enum_LAST_KAPI_FIELD);
	/* set flags for all fields that need saving */
	for(i=0;i<enum_LAST_KAPI_FIELD;i++)
		pfSaveFlags[i]=1;
	/* save */
	iRes=KAPI_save_as_header_MachineDescription_saver(fp,pConfig,pchName,pfSaveFlags);
	/* free flags */
	return iRes;
}

int KAPI_save_as_header_latency_all_info( FILE *fp, void *pConfig , char *pchName)
{
	int pfSaveFlags[enum_LAST_KAPI_FIELD];
	int iRes;
	int i;

	memset(pfSaveFlags,0,sizeof(int)*enum_LAST_KAPI_FIELD);
	pfSaveFlags[0]=1;

	/* set flags for all fields that need saving */
	for(i=enum_nclr;i<enum_dmpfuinfoTable;i++)
		pfSaveFlags[i]=1;
	/* save */
	iRes=KAPI_save_as_header_MachineDescription_saver(fp,pConfig,pchName,pfSaveFlags);
	return iRes;
}
int KAPI_save_as_header_latency_core_info( FILE *fp, void *pConfig , char *pchName)
{
	int pfSaveFlags[enum_LAST_KAPI_FIELD];
	int iRes;
	int i;

	memset(pfSaveFlags,0,sizeof(int)*enum_LAST_KAPI_FIELD);
	/* set flags for all fields that need saving */
	for(i=enum_nfuinfoTable;i<enum_dmpfuinfoTable;i++)
		pfSaveFlags[i]=1;
	/* save */
	iRes=KAPI_save_as_header_MachineDescription_saver(fp,pConfig,pchName,pfSaveFlags);
	return iRes;
}
int  KAPI_save_as_header_cluster_all_info( FILE *fp, void *pConfig , char *pchName)
{
	int pfSaveFlags[enum_LAST_KAPI_FIELD];
	int iRes;
	int i;

	/* set flags for all fields that need saving */
	memset(pfSaveFlags,0,sizeof(int)*enum_LAST_KAPI_FIELD);
	for(i=enum_nclr;i<enum_port2cluster;i++)
		pfSaveFlags[i]=1;
	/* save */
	iRes=KAPI_save_as_header_MachineDescription_saver(fp,pConfig,pchName,pfSaveFlags);
	return iRes;
}
int	KAPI_save_as_header_cluster_distance_info( FILE *fp, void *pConfig , char *pchName)
{
	int pfSaveFlags[enum_LAST_KAPI_FIELD];
	int iRes;
	int i;

	memset(pfSaveFlags,0,sizeof(int)*enum_LAST_KAPI_FIELD);
	/* set flags for all fields that need saving */
	for(i=enum_nclr;i<enum_mpclrTable;i++)
		pfSaveFlags[i]=1;
	/* save */
	iRes=KAPI_save_as_header_MachineDescription_saver(fp,pConfig,pchName,pfSaveFlags);
	return iRes;
}
int KAPI_save_as_header_cluster_intracluster_latency_info( FILE *fp, void *pConfig , char *pchName)
{
	int pfSaveFlags[enum_LAST_KAPI_FIELD];
	int iRes;
	int i;

	memset(pfSaveFlags,0,sizeof(int)*enum_LAST_KAPI_FIELD);
	/* set flags for all fields that need saving */
	for(i=enum_nclr;i<enum_mpclrTable;i++)
		pfSaveFlags[i]=1;
	for(i=enum_nRawIntra;i<enum_dmpabyIntraTable;i++)
		pfSaveFlags[i]=1;
	/* save */
	iRes=KAPI_save_as_header_MachineDescription_saver(fp,pConfig,pchName,pfSaveFlags);
	return iRes;
}
int KAPI_save_as_header_cluster_intercluster_latency_info( FILE *fp, void *pConfig , char *pchName)
{
	int pfSaveFlags[enum_LAST_KAPI_FIELD];
	int iRes;
	int i;

	memset(pfSaveFlags,0,sizeof(int)*enum_LAST_KAPI_FIELD);
	/* set flags for all fields that need saving */
	for(i=enum_nclr;i<enum_mpclrTable;i++)
		pfSaveFlags[i]=1;
	for(i=enum_nRawInter;i<enum_dmpebyInterTable;i++)
		pfSaveFlags[i]=1;
	/* save */
	iRes=KAPI_save_as_header_MachineDescription_saver(fp,pConfig,pchName,pfSaveFlags);
	return iRes;
}
int KAPI_save_as_header_functional_units_info_info( FILE *fp, void *pConfig , char *pchName)
{
	int pfSaveFlags[enum_LAST_KAPI_FIELD];
	int iRes;
	int i;

	memset(pfSaveFlags,0,sizeof(int)*enum_LAST_KAPI_FIELD);
	/* set flags for all fields that need saving */
	for(i=enum_mpnut;i<enum_dmpfuinfoTable;i++)
		pfSaveFlags[i]=1;
	/* save */
	iRes=KAPI_save_as_header_MachineDescription_saver(fp,pConfig,pchName,pfSaveFlags);
	return iRes;
}  
int KAPI_save_as_header_instruction_all_info( FILE *fp, void *pConfig , char *pchName)
{
	int pfSaveFlags[enum_LAST_KAPI_FIELD];
	int iRes;
	int i;

	memset(pfSaveFlags,0,sizeof(int)*enum_LAST_KAPI_FIELD);
	/* set flags for all fields that need saving */
	for(i=enum_ninstTable;i<enum_dmpsylinfoTable;i++)
		pfSaveFlags[i]=1;
	/* save */
	iRes=KAPI_save_as_header_MachineDescription_saver(fp,pConfig,pchName,pfSaveFlags);
	return iRes;
}  
int KAPI_save_as_header_instruction_type_info( FILE *fp, void *pConfig , char *pchName)
{
	int pfSaveFlags[enum_LAST_KAPI_FIELD];
	int iRes;
	int i;

	memset(pfSaveFlags,0,sizeof(int)*enum_LAST_KAPI_FIELD);
	/* set flags for all fields that need saving */
	for(i=enum_nitinfoTable;i<enum_dmpitinfoTable;i++)
		pfSaveFlags[i]=1;
	/* save */
	iRes=KAPI_save_as_header_MachineDescription_saver(fp,pConfig,pchName,pfSaveFlags);
	return iRes;
}  
int KAPI_save_as_header_byd_n_syl_info( FILE *fp, void *pConfig , char *pchName)
{
	int pfSaveFlags[enum_LAST_KAPI_FIELD];
	int iRes;
	int i;

	memset(pfSaveFlags,0,sizeof(int)*enum_LAST_KAPI_FIELD);
	/* set flags for all fields that need saving */
	for(i=enum_nbidinfoTable;i<enum_dmpsylinfoTable;i++)
		pfSaveFlags[i]=1;
	/* save */
	iRes=KAPI_save_as_header_MachineDescription_saver(fp,pConfig,pchName,pfSaveFlags);
	return iRes;
}  
int KAPI_fEnableIA64call_from_header(void **pConfig, void *pHeaderConfig, int iReserved)
{
	if (NULL==pHeaderConfig)
		return -1;
	*pConfig=(knobs_t *)pHeaderConfig;
	return 0;
}

