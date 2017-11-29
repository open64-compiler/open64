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

#ifndef _KAPI_SAVER_UTILS_H
#define _KAPI_SAVER_UTILS_H

#include <string.h>
#include "kapi_bv.h"

static int _kapi_radix=10;
static int kapi_saver_bv_counter=0;


#define ADD_COMMA fwrite(",",sizeof(char),1,fpTmp)
#define ADD_NEWLINE fwrite("\n",sizeof(char),strlen("\n"),fpTmp)
#define ADD_NULL fwrite("NULL",sizeof(char),strlen("NULL"),fpTmp)
#define START_ARRAY fwrite("{",sizeof(char),1,fpTmp)
#define START_STRUCT START_ARRAY
#define CLOSE_ARRAY fwrite("}\n",sizeof(char),strlen("}\n"),fpTmp)
#define CLOSE_STRUCT fwrite("}",sizeof(char),1,fpTmp)
#define ADD_SEMICOLON fwrite(";",sizeof(char),1,fpTmp)
#define END_STRUCT fwrite("};",sizeof(char),2,fpTmp)
#define END_ARRAY fwrite("};\n",sizeof(char),strlen("};\n"),fpTmp)

#define ADD_COMMENT(_comment) \
{														\
	char *str=malloc(sizeof(char)*(20+strlen(_comment)));\
	sprintf(str,"/* %s */",_comment);					\
	fwrite(str,sizeof(char),strlen(str),fpTmp);			\
	free(str);											\
}

#define ADD_STRUCT_HEADER(_stype,_sname) \
{																		\
	char *str=malloc(sizeof(char)*(20+strlen(_stype)+strlen(_sname)));	\
	sprintf(str,"static %s %s = {",_stype,_sname);								\
	fwrite(str,sizeof(char),strlen(str),fpTmp);							\
	free(str);															\
}

#define ADD_EXTERN_STRUCT_HEADER(_stype,_sname) \
{																		\
	char *str=malloc(sizeof(char)*(20+strlen(_stype)+strlen(_sname)));	\
	sprintf(str,"%s %s = {",_stype,_sname);								\
	fwrite(str,sizeof(char),strlen(str),fpTmp);							\
	free(str);															\
}

#define ADD_STRUCT_ARRAY_HEADER(_type_name,_array_name) \
{																					\
	char *str;																		\
	str=(char *)malloc(sizeof(char)*(20+strlen(_type_name)+strlen(_array_name)));	\
	sprintf(str,"static %s %s[] = {\n",_type_name,_array_name);								\
	fwrite(str,sizeof(char),strlen(str),fpTmp);										\
	free(str);															\
}
#define ADD_NULL_PTR(_type_name,_var_name) \
{																					\
	char *str;																		\
	str=(char *)malloc(sizeof(char)*(20+strlen(_type_name)+strlen(_var_name)));		\
	sprintf(str,"#define %s NULL\n",_var_name);										\
	fwrite(str,sizeof(char),strlen(str),fpTmp);										\
	free(str);															\
}
/*OLD ver ??? sprintf(str,"static %s *%s = NULL;\n",_type_name,_var_name);					\*/


#define SAVE_STRUCT_ARRAY(_nsItems,_assptr,_ssaver_func,_ssave_name) \
	SAVE_PTR_STRUCT_ARRAY(_nsItems,&_assptr,_ssaver_func,_ssave_name) 


#define SAVE_PTR_STRUCT_ARRAY(_nItems,_asptr,_saver_func,_save_name) \
{														\
	int _counter;										\
	char _pchIdx[20];									\
	if (_nItems>0)										\
	{													\
		for (_counter=0;_counter<(_nItems-1);_counter++)	\
		{													\
			sprintf(_pchIdx,"[%d]",_counter);				\
			ADD_COMMENT(_pchIdx);							\
			START_STRUCT;									\
			(_saver_func)(fpTmp,pknobs,_asptr[_counter],_save_name,fpTables);\
			CLOSE_STRUCT;									\
			ADD_COMMA;										\
			*_pchIdx='\0';									\
		}													\
		sprintf(_pchIdx,"[%d]",_counter);					\
		ADD_COMMENT(_pchIdx);								\
			START_STRUCT;									\
		(_saver_func)(fpTmp,pknobs,_asptr[_counter],_save_name,fpTables);	\
			CLOSE_STRUCT;									\
	}																		\
}

#define ADD_STRUCT_ARRAY(_array_type_str,_array_name,_nElements,_array_ptr,_saver_function) \
{																						\
	ADD_STRUCT_ARRAY_HEADER(_array_type_str,_array_name);								\
	SAVE_STRUCT_ARRAY(_nElements,_array_ptr,_saver_function,_array_name);				\
	END_STRUCT;																			\
	ADD_NEWLINE;										\
}
#define ADD_STRUCT_ARRAY_ON_FLAG(_flag,_array_type_str,_array_name,_nElements,_array_ptr,_saver_function) \
{																							\
	if ((_flag!=0) && (_nElements>0))														\
	{																						\
		ADD_STRUCT_ARRAY_HEADER(_array_type_str,_array_name);								\
		SAVE_STRUCT_ARRAY(_nElements,_array_ptr,_saver_function,_array_name);				\
		END_STRUCT;																			\
	} else																					\
		ADD_NULL_PTR(_array_type_str,_array_name);											\
	ADD_NEWLINE;																			\
}

#define ADD_PTR_STRUCT_ARRAY(_array_type_str,_array_name,_nElements,_array_ptr,_saver_function) \
{																						\
	ADD_STRUCT_ARRAY_HEADER(_array_type_str,_array_name);								\
	SAVE_PTR_STRUCT_ARRAY(_nElements,_array_ptr,_saver_function,_array_name);			\
	END_STRUCT;																			\
	ADD_NEWLINE;										\
}
#define ADD_PTR_STRUCT_ARRAY_ON_FLAG(_flag,_array_type_str,_array_name,_nElements,_array_ptr,_saver_function) \
{																						\
	if ((_flag!=0) && (_nElements>0))																			\
	{																						\
		ADD_STRUCT_ARRAY_HEADER(_array_type_str,_array_name);								\
		SAVE_PTR_STRUCT_ARRAY(_nElements,_array_ptr,_saver_function,_array_name);			\
		END_STRUCT;																			\
	} else																					\
		ADD_NULL_PTR(_array_type_str,_array_name);											\
	ADD_NEWLINE;										\
}

#define SAVE_STRUCT(_struct_function) \
	{	START_STRUCT;		\
		(_struct_function);	\
		CLOSE_STRUCT;		\
		ADD_NEWLINE;		\
	}

#define SAVE_STRUCT_LIST(_struct_function) \
	{	SAVE_STRUCT(_struct_function);	\
		ADD_COMMA;						\
	}


/* Functions */
/* --------- */
static char *make_string_attach(char *pchBase,char *pchExt)
{
	char *str=(char *)malloc(sizeof(char)*(20+strlen(pchBase)+strlen(pchExt)));
	sprintf(str,"%s_%s",pchBase ,pchExt);
	return str;
}

static void append_tmp(FILE *fpTarget, FILE *fpTmp)
{
	char ch;
	fputc(EOF,fpTmp);
	fflush(fpTmp);
	rewind(fpTmp);
	while ((ch=fgetc(fpTmp))!=EOF)
	{
		fputc(ch,fpTarget);
	}
	fclose(fpTmp);
}

static void KAPI_save_as_header_int(FILE *fp, int i)
{
	char buffer[20];
	FILE *fpTmp=fp;
	if (_kapi_radix==16)
		sprintf(buffer,"%#x",i);
	else
		sprintf(buffer,"%d",i);
   fwrite(buffer,sizeof(char),strlen(buffer),fp);
}

static void KAPI_save_as_header_uint(FILE *fp, unsigned int i)
{
	char buffer[20];
	FILE *fpTmp=fp;
	if (_kapi_radix==16)
		sprintf(buffer,"%#x",i);
	else
		sprintf(buffer,"%d",i);
   fwrite(buffer,sizeof(char),strlen(buffer),fp);
}

static void KAPI_save_as_header_int_list(FILE *fp, int i)
{
	FILE *fpTmp=fp;
   KAPI_save_as_header_int(fp,i);
   ADD_COMMA;
}
static void KAPI_save_as_header_uint_list(FILE *fp, int i)
{
	FILE *fpTmp=fp;
   KAPI_save_as_header_uint(fp,i);
   ADD_COMMA;
}

static void KAPI_save_as_header_int_array(FILE *fp,int *pint,int number_of_elements) 
{															
	int i;													
	FILE *fpTmp=fp;
   START_ARRAY;												
   for (i=0;i<number_of_elements-1;i++)									
	   KAPI_save_as_header_int_list(fp,pint[i]);				
   KAPI_save_as_header_int(fp,pint[i]);						
   CLOSE_ARRAY;												
}
static void KAPI_save_as_header_uint_array(FILE *fp,unsigned long *pint,int number_of_elements) 
{															
	int i;													
	FILE *fpTmp=fp;
   START_ARRAY;												
   for (i=0;i<number_of_elements-1;i++)									
	   KAPI_save_as_header_uint_list(fp,pint[i]);				
   KAPI_save_as_header_uint(fp,pint[i]);						
   CLOSE_ARRAY;												
}

static void KAPI_save_as_header_int_array_list(FILE *fp,int *pint,int number_of_elements) 
{		
	FILE *fpTmp=fp;
	KAPI_save_as_header_int_array(fp,pint,number_of_elements);
	ADD_COMMA;
}
static void KAPI_save_as_header_String(FILE *fp,char *pch)
{
	FILE *fpTmp=fp;
	fwrite("NULL/*",sizeof(char),strlen("NULL/*"),fp);
	fwrite(pch,sizeof(char),strlen(pch),fp);
	fwrite("*/",sizeof(char),strlen("*/"),fp);
}
static void KAPI_save_as_header_String_list(FILE *fp,char *pch)
{
	FILE *fpTmp=fp;
	KAPI_save_as_header_String(fp,pch);
	ADD_COMMA;
}
static void KAPI_save_as_header_String_always(FILE *fp,char *pch)
{
	FILE *fpTmp=fp;
	fwrite("\"",sizeof(char),strlen("\""),fp);
	fwrite(pch,sizeof(char),strlen(pch),fp);
	fwrite("\"",sizeof(char),strlen("\""),fp);
}
static void KAPI_save_as_header_String_list_always(FILE *fp,char *pch)
{
	FILE *fpTmp=fp;
	KAPI_save_as_header_String_always(fp,pch);
	ADD_COMMA;
}
static void KAPI_save_as_header_Enum(FILE *fp,char *pch)
{
	FILE *fpTmp=fp;
	fwrite(pch,sizeof(char),strlen(pch),fp);
}
static void KAPI_save_as_header_name(FILE *fp,char *pch)
{
	FILE *fpTmp=fp;
	fwrite(pch,sizeof(char),strlen(pch),fp);
}
static void KAPI_save_as_header_Enum_list(FILE *fp,char *pch)
{
	FILE *fpTmp=fp;
	KAPI_save_as_header_Enum(fp,pch);
	ADD_COMMA;
}
static void KAPI_save_as_header_name_list(FILE *fp,char *pch)
{
	FILE *fpTmp=fp;
	KAPI_save_as_header_name(fp,pch);
	ADD_COMMA;
}

static void KAPI_save_as_header_String_array(FILE *fp,char **ppch,int number_of_elements) 
{															
	int i;													
	FILE *fpTmp=fp;
   START_ARRAY;												
   for (i=0;i<number_of_elements-1;i++)									
	   KAPI_save_as_header_String_list(fp,ppch[i]);				
   KAPI_save_as_header_String(fp,ppch[i]);						
   CLOSE_ARRAY;												
}

static void KAPI_save_as_header_String_array_list(FILE *fp,char **ppch,int number_of_elements) 
{
	FILE *fpTmp=fp;
	KAPI_save_as_header_String_array(fp,ppch,number_of_elements);
	ADD_COMMA;
}

static void KAPI_save_as_header_String_array_always(FILE *fp,char **ppch,int number_of_elements) 
{															
	int i;													
	FILE *fpTmp=fp;
   START_ARRAY;												
   for (i=0;i<number_of_elements-1;i++)									
	   KAPI_save_as_header_String_list_always(fp,ppch[i]);				
   KAPI_save_as_header_String_always(fp,ppch[i]);						
   CLOSE_ARRAY;												
}

static void KAPI_save_as_header_String_array_list_always(FILE *fp,char **ppch,int number_of_elements) 
{
	FILE *fpTmp=fp;
	KAPI_save_as_header_String_array_always(fp,ppch,number_of_elements);
	ADD_COMMA;
}

static void KAPI_save_as_header_bv128(FILE *fp,bv128_t *pbv)
{
	fprintf(fp,"{%#x,%#x,%#x,%#x}",pbv->i1,pbv->i2,pbv->i3,pbv->i4);
}

static void KAPI_save_as_header_BitVector_struct(FILE *fp,bv_t *pbv, char *pchName, FILE *fpTables)
{
	FILE *fpTmp=fpTables;
	char *myName=NULL;
	char *vectorName=NULL;
	int tmp_radix=_kapi_radix;
	char pchNum[20];
	/* init vector name */
	kapi_saver_bv_counter++;
	sprintf(pchNum,"%d",kapi_saver_bv_counter);
	myName=make_string_attach("_saver_bv",pchNum);
	vectorName=(char *)malloc(sizeof(char)*(20+strlen("unsigned long [] =")+strlen(myName)));
	*vectorName='\0';
	sprintf(vectorName,"unsigned long %s[] = ",myName);

	/* save vector */
	fwrite(vectorName,sizeof(char),strlen(vectorName),fpTmp);
     _kapi_radix=16;
     KAPI_save_as_header_uint_array(fpTmp,pbv->pint32Data,pbv->n32Chunks);
     _kapi_radix=tmp_radix;
	ADD_SEMICOLON;
	ADD_NEWLINE;

	/* save struct */
	fpTmp=fp;

    KAPI_save_as_header_int_list(fpTmp,pbv->n32Chunks);
	KAPI_save_as_header_name(fpTmp,myName);
	free(myName);
	free(vectorName);
}


static void KAPI_save_as_header_bv32(FILE *fp,bv32_t bv)
{
	int tmp_radix=_kapi_radix;
	_kapi_radix=16;
	KAPI_save_as_header_uint(fp,bv);
	_kapi_radix=tmp_radix;
}
static void KAPI_save_as_header_bv32_list(FILE *fp,bv32_t bv)
{
	FILE *fpTmp=fp;
	KAPI_save_as_header_bv32(fp,bv);
	ADD_COMMA;
}
static void KAPI_save_as_header_bv32_array(FILE *fp,int *pint,int number_of_elements) 
{															
	int i;													
	FILE *fpTmp=fp;
   for (i=0;i<number_of_elements-1;i++)									
	   KAPI_save_as_header_bv32_list(fp,pint[i]);				
   KAPI_save_as_header_bv32(fp,pint[i]);						
}


#endif /* _KAPI_SAVER_UTILS_H */
