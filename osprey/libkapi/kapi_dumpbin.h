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

/* string handling */
void KDumpBin_DumpString(FILE *fp,char *pString);
void KDumpBin_LoadString(FILE *fp,char **pString);
void KDumpBin_DumpBitVector(FILE *fp,bv_t *pbv);
void KDumpBin_LoadBitVector(FILE *fp,bv_t **pbv);
/* Dump the whole knobs info */
void KDumpBin_DumpMachineDescription( FILE *fp, knobs_t *pknobs );
/* Functions used by KDumpBin_DumpMachineDescription */
/*****************************************************/
	/* Dump info for a cluster */
	void KDumpBin_DumpClusterInfo(FILE *fp,clr_t *pclr);
	/* Dump inter cluster bypass info for each reprocessed intercluster */
	void KDumpBin_DumpInterClusterBypassInfo(FILE *fp,eby_t *peby);
	/* Dump intra cluster bypass info for each reprocessed intracluster */
	void KDumpBin_DumpIntraClusterBypassInfo(FILE *fp,aby_t *paby);
	/* Dump port information for a CPORT */
	void KDumpBin_DumpPortInfo(FILE *fp,cportinfo_t *pcport);
	/* Dump functional unit info for a functional unit */
	void KDumpBin_DumpFunctionalUnitInfo(FILE *fp,fuinfo_t *pfut);
	/* Dump instruction info for an instruction */
	void KDumpBin_DumpInstructionInfo(FILE *fp,inst_t *pinst);
	/* Dump instruction type info */
	/*  for instruction types ...
	   kapi_itA  = 0,
	   kapi_itM  = 1,
	   kapi_itB  = 2,
	   kapi_itBl = 3,
	   kapi_itI  = 4,
	   kapi_itF  = 5,
	   kapi_itL  = 6 */
	void KDumpBin_DumpInstructionTypeInfo(FILE *fp,itinfo_t *pit);
	/* Dump bundle info */
	void KDumpBin_DumpBundleInfo(FILE *fp,bidinfo_t *pbid);
	/* Dump syllable info */
	void KDumpBin_DumpSyllableInfo(FILE *fp,sylinfo_t *psyl);
	/* Dump symbol Table */
	void KDumpBin_DumpSymbolTableNodeInfo(FILE *fp,stn_t *pstn);
	 /* Save type field info, assuming that the symbol table */
	 /*  info is a back pointer or NULL						 */
	 void KDumpBin_REF_DumpTypeFieldInfo(FILE *fp, tfi_t *ptfi, stn_t *pstn);
	/* Dump a list of values according to value type */
	void KDumpBin_DumpValueNodeListInfo(FILE *fp,valn_t *pvaln,tty_t ttyType);
