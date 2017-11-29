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
#include <assert.h>
#include <string.h>
#include <malloc.h>
#include "kapi_internal.h"
#include "kapi_parse.h"
#include "kapi_util.h"
#include "kapi_error.h"
#include "kapi_dumpbin.h"

void KDumpBin_DumpMachineDescription( FILE *fp, knobs_t *pknobs )
{
   int  i;
   if (NULL==fp) return;
   /* number of clusters */
   fwrite(&(pknobs->nclr),sizeof(int),1,fp);
   /* Bundle issue width info */
   fwrite(pknobs->maxBundleIssue,sizeof(int),(pknobs->nclr),fp);
   /* Instruction issue width info */
   fwrite(pknobs->maxInstructionIssue,sizeof(int),(pknobs->nclr),fp);
   /* Cluster info */
   for (i=0;i<(pknobs->nclr);i++)
	KDumpBin_DumpClusterInfo(fp,&(pknobs->mpclrTable[i]));
   /* InterCluster bypass info */
   fwrite(&(pknobs->nRawInter),sizeof(int),1,fp);
   for (i=0;i<(pknobs->nRawInter);i++)
	KDumpBin_DumpInterClusterBypassInfo(fp,&(pknobs->dmpebyInterTable[i]));
   /* IntraCluster bypass info */
   fwrite(&(pknobs->nRawIntra),sizeof(int),1,fp);
   for (i=0;i<(pknobs->nRawIntra);i++)
	KDumpBin_DumpIntraClusterBypassInfo(fp,&(pknobs->dmpabyIntraTable[i]));
   /* Ports info (global) */
   fwrite(&(pknobs->nports),sizeof(int),1,fp);
   for (i=0;i<(pknobs->nports);i++)
	KDumpBin_DumpPortInfo(fp,pknobs->mppportinfoTable[i]);
   /* port to cluster mapping */
   fwrite(pknobs->port2cluster,sizeof(int),32,fp);
   /* global unit type information */
   fwrite(pknobs->mpnut,sizeof(int),kapi_nUT,fp);
   /* functional unit information */
   fwrite(&(pknobs->nfuinfoTable),sizeof(int),1,fp);
   for (i=0;i<(pknobs->nfuinfoTable);i++)
	KDumpBin_DumpFunctionalUnitInfo(fp,&(pknobs->dmpfuinfoTable[i]));
   /* instruction information */
   fwrite(&(pknobs->ninstTable),sizeof(int),1,fp);
   for (i=0;i<(pknobs->ninstTable);i++)
	KDumpBin_DumpInstructionInfo(fp,&(pknobs->dmpinstTable[i]));
   /* instruction type information */
   fwrite(&(pknobs->nitinfoTable),sizeof(int),1,fp);
   for (i=0;i<(pknobs->nitinfoTable);i++)
	KDumpBin_DumpInstructionTypeInfo(fp,&(pknobs->dmpitinfoTable[i]));
   /* bundle information by bundle id */
   fwrite(&(pknobs->nbidinfoTable),sizeof(int),1,fp);
   for (i=0;i<(pknobs->nbidinfoTable);i++)
	KDumpBin_DumpBundleInfo(fp,&(pknobs->dmpbidinfoTable[i]));
   /* syllable information */
   fwrite(&(pknobs->nsylinfoTable),sizeof(int),1,fp);
   for (i=0;i<(pknobs->nsylinfoTable);i++)
	KDumpBin_DumpSyllableInfo(fp,&(pknobs->dmpsylinfoTable[i]));
   /* symbol table information */
   fwrite(&(pknobs->nstn),sizeof(int),1,fp);
   for (i=0;i<(pknobs->nstn);i++)
	KDumpBin_DumpSymbolTableNodeInfo(fp,&(pknobs->dmpstn[i]));
   /* attribute structured data flag*/
   fwrite(&(pknobs->fRestructuredAttributes),sizeof(int),1,fp);
   /* misc lists */
   KDumpBin_DumpValueNodeListInfo(fp,pknobs->pvalnTypeList,ttySTRING);
   KDumpBin_DumpValueNodeListInfo(fp,pknobs->pvalnVarList,ttySTRING);
   KDumpBin_DumpValueNodeListInfo(fp,pknobs->pvalnAttrList,ttySTRING);
}


void KDumpBin_DumpString(FILE *fp,char *pch)
{
	int iLength=strlen(pch);
	/* save length of string */
	fwrite(&iLength,sizeof(int),1,fp);
	/* save the string itself */
	fwrite(pch,sizeof(char),iLength,fp);
}

void KDumpBin_LoadString(FILE *fp,char **ppch)
{
	int iLength;
	/* load length of string */
	fread(&iLength,sizeof(int),1,fp);
	/* save the string itself */
	*ppch=(char *)(malloc(sizeof(char)*iLength));
	fread(*ppch,sizeof(char),iLength,fp);
}

void KDumpBin_DumpBitVector(FILE *fp,bv_t *pbv)
{
   fwrite(&(pbv->n32Chunks),sizeof(int),1,fp);
   fwrite(pbv->pint32Data,sizeof(uint32_t),pbv->n32Chunks,fp);
}

void KDumpBin_LoadBitVector(FILE *fp,bv_t **pbv)
{
   int iByteLength;
   fread(&(iByteLength),sizeof(int),1,fp);
   *pbv=pbvMake(iByteLength*32,0);
   (*pbv)->pint32Data=(uint32_t *)(malloc(sizeof(uint32_t)*(iByteLength)));
   fread((*pbv)->pint32Data,sizeof(uint32_t),iByteLength,fp);
}

void KDumpBin_DumpClusterInfo(FILE *fp,clr_t *pclr)
{
	int i,j;
   /* cluster number */
   fwrite(&(pclr->idxclr),sizeof(int),1,fp);
   /* number of ports in cluster and ports info*/
   fwrite(&(pclr->ncports),sizeof(int),1,fp);
   /* 1d array of pointers to structures */
   for (i=0;i<(maxCPORTS);i++)
	KDumpBin_DumpPortInfo(fp,pclr->mppcportinfoTable[i]);
   /* 2d array of cport structures, length of 2nd D in mpncutport */
   fwrite(pclr->mpncutport,sizeof(int),kapi_nUT,fp);
   for (i=0;i<(kapi_nUT);i++)
	for (j=0;j<(pclr->mpncutport[i]);j++)
	  KDumpBin_DumpPortInfo(fp,&(pclr->dmppcportinfoTable[i][j]));
   /* cport to global port mapping */
   fwrite(pclr->cport2port,sizeof(int),32,fp);
   /* distance to other clr */
   fwrite(pclr->distClr,sizeof(int),maxCLR,fp);
}

void KDumpBin_DumpInterClusterBypassInfo(FILE *fp,eby_t *peby)
{
#ifndef FAST_SAVE
   fwrite(&(peby->clusterSrc),sizeof(kapi_cluster_t),1,fp);
   fwrite(&(peby->clusterTar),sizeof(kapi_cluster_t),1,fp);
   fwrite(&(peby->fuSrc),sizeof(kapi_fu_t),1,fp);
   fwrite(&(peby->fuTar),sizeof(kapi_fu_t),1,fp);
   fwrite(&(peby->cutportSrc),sizeof(kapi_cutport_t),1,fp);
   fwrite(&(peby->cutportTar),sizeof(kapi_cutport_t),1,fp);
   fwrite(&(peby->utSrc),sizeof(kapi_ut_t),1,fp);
   fwrite(&(peby->utTar),sizeof(kapi_ut_t),1,fp);
   fwrite(&(peby->oppSrc),sizeof(int),1,fp);
   fwrite(&(peby->oppTar),sizeof(int),1,fp);
   fwrite(&(peby->iValue),sizeof(int),1,fp);
#else
   fwrite(&(peby->clusterSrc),sizeof(int),11,fp);
#endif
}

void KDumpBin_DumpIntraClusterBypassInfo(FILE *fp,aby_t *paby)
{
#ifndef FAST_SAVE
   fwrite(&(paby->cluster),sizeof(kapi_cluster_t),1,fp);
   fwrite(&(paby->fuSrc),sizeof(kapi_fu_t),1,fp);
   fwrite(&(paby->fuTar),sizeof(kapi_fu_t),1,fp);
   fwrite(&(paby->cutportSrc),sizeof(kapi_cutport_t),1,fp);
   fwrite(&(paby->cutportTar),sizeof(kapi_cutport_t),1,fp);
   fwrite(&(paby->utSrc),sizeof(kapi_ut_t),1,fp);
   fwrite(&(paby->utTar),sizeof(kapi_ut_t),1,fp);
   fwrite(&(paby->oppSrc),sizeof(int),1,fp);
   fwrite(&(paby->oppTar),sizeof(int),1,fp);
   fwrite(&(paby->iValue),sizeof(int),1,fp);
#else
   fwrite(&(peby->cluster),sizeof(int),10,fp);
#endif
   KDumpBin_DumpString(fp,paby->pchEntry);
}

void KDumpBin_DumpPortInfo(FILE *fp,cportinfo_t *pcport)
{
    fwrite(&(pcport->ut),sizeof(kapi_ut_t),1,fp);
    fwrite(&(pcport->cport),sizeof(kapi_cport_t),1,fp);
    fwrite(&(pcport->cutport),sizeof(kapi_cutport_t),1,fp);
	KDumpBin_DumpBitVector(fp,&(pcport->bvfuAllowed));
}

void KDumpBin_DumpFunctionalUnitInfo(FILE *fp,fuinfo_t *pfut)
{
	int i;
    fwrite(&(pfut->fu),sizeof(kapi_fu_t),1,fp);
	fwrite(pfut->iCoreLatency,sizeof(int),maxDESTINATIONS,fp);
    for (i=0;i<(pfut->cntDest);i++)
 	 KDumpBin_DumpString(fp,pfut->mppchDestName[i]);
    for (i=0;i<(pfut->cntSrc);i++)
 	 KDumpBin_DumpString(fp,pfut->mppchSrcName[i]);
    KDumpBin_DumpString(fp,pfut->pchName);
}

void KDumpBin_DumpInstructionInfo(FILE *fp,inst_t *pinst)
{
    KDumpBin_DumpString(fp,pinst->pchUniqueName);
    KDumpBin_DumpString(fp,pinst->pchMnemonic);
    /* function unit class character representation */
	KDumpBin_DumpString(fp,pinst->pchfu);
	/* it character representation */
    KDumpBin_DumpString(fp,pinst->pchit);
	/* instruction type */
    fwrite(&(pinst->it),sizeof(kapi_it_t),1,fp);
	/* function unit restrictions/byp/latency */
    fwrite(&(pinst->fu),sizeof(kapi_fu_t),1,fp);
}

void KDumpBin_DumpInstructionTypeInfo(FILE *fp,itinfo_t *pit)
{
    fwrite(&(pit->it),sizeof(kapi_it_t),1,fp);
    fwrite(&(pit->maxAvail),sizeof(int),1,fp);
    KDumpBin_DumpString(fp,pit->pchitName);
	fwrite(&(pit->bv32sylAllowed), sizeof(bv32_t),1,fp);
}

void KDumpBin_DumpBundleInfo(FILE *fp,bidinfo_t *pbid)
{
	/* syllables comprising this bundle type */
    fwrite(pbid->mpsylType,sizeof(kapi_syl_t),nSYLBID,fp);
	/* number of syllables of each type */
    fwrite(pbid->mpnsylAvail,sizeof(int),kapi_nSYL,fp);
	/* number of instructions of each type possible */
    fwrite(pbid->mpnitAvail,sizeof(int),kapi_nIT,fp);
	/* is this a reserved bid (flag) */
    fwrite(&(pbid->fReserved),sizeof(int),1,fp);
	/* 0 == no sbit, 1, 2 legal values */
    fwrite(&(pbid->isylSbit),sizeof(int),1,fp);
    /* string rep of expected bid */
    KDumpBin_DumpString(fp,pbid->pchBID);
}

void KDumpBin_DumpSyllableInfo(FILE *fp,sylinfo_t *psyl)
{
	/* number of syllables of each type */
    fwrite(psyl->mpnutNeeded,sizeof(int),kapi_nUT,fp);
    fwrite(&(psyl->itMajor),sizeof(kapi_it_t),1,fp);
}

void KDumpBin_DumpSymbolTableNodeInfo(FILE *fp,stn_t *pstn)
{

    fwrite(&(pstn->fExpected),sizeof(int),1,fp);
    KDumpBin_DumpString(fp,pstn->pchName);
    fwrite(&(pstn->ity),sizeof(ity_t),1,fp);
	/* ity determines the type of value in the union */
	switch (pstn->ity)
	{
	case ityTYPENAME:
		KDumpBin_REF_DumpTypeFieldInfo(fp, &(pstn->u.tfi), pstn);
	case ityVARNAME:
		KDumpBin_REF_DumpVarFieldInfo(fp, &(pstn->u.tfi), pstn);

	}
}
