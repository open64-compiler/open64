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

#ifndef _kapi_save_source_h
#define _kapi_save_source_h

/*  
	fp: where to save the struct
	pknobs: pknobs pointer, for reference
	other ptr: struct to save
	pchName: name of struct, for reference / adding new tables
	fpTables: where to save tables related to the struct
*/


extern void KAPI_save_as_header_MachineDescription_struct( FILE *fp, knobs_t *pknobs , char *pchName, FILE *fpTables);
extern void KAPI_save_as_header_ClusterInfo_struct(FILE *fp,knobs_t *pknobs,clr_t *pclr,char *pchName, FILE *fpTables);
extern void KAPI_save_as_header_saver_flags(FILE *fp,knobs_t *pknobs,int *pFlag, char *pchName, FILE *fpTables);
extern void KAPI_save_as_header_InterClusterBypassInfo_struct(FILE *fp,knobs_t *pknobs, eby_t *peby, char *pchName, FILE *fpTables);
extern void KAPI_save_as_header_IntraClusterBypassInfo_struct(FILE *fp,knobs_t *pknobs,aby_t *paby, char *pchName, FILE *fpTables);
extern void KAPI_save_as_header_PortInfo_struct(FILE *fp,knobs_t *pknobs,cportinfo_t *pcport,char *pchName, FILE *fpTables);
extern void KAPI_save_as_header_InstructionInfo_struct(FILE *fp,knobs_t *pknobs,inst_t *pinst,char *pchName, FILE *fpTables);
extern void KAPI_save_as_header_InstructionTypeInfo_struct(FILE *fp,knobs_t *pknobs,itinfo_t *pit,char *pchName, FILE *fpTables);
extern void KAPI_save_as_header_BundleInfo_struct(FILE *fp,knobs_t *pknobs,bidinfo_t *pbid,char *pchName, FILE *fpTables);
extern void KAPI_save_as_header_SyllableInfo_struct(FILE *fp,knobs_t *pknobs,sylinfo_t *psyl, char *pchName, FILE *fpTables);
extern void KAPI_save_as_header_FunctionalUnitInfo_struct(FILE *fp,knobs_t *pknobs,fuinfo_t *pfut,char *pchName, FILE *fpTables);
extern void KAPI_save_as_header_Cache_struct( FILE *fp, knobs_t *pknobs, cache_t *pcache , char *pchName, FILE *fpTables);

#endif /* _kapi_save_source_h */
