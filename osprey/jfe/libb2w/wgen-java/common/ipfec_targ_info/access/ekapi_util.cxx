/*
  Copyright (C) 2000-2003, Intel Corporation
  All rights reserved.
  
  Redistribution and use in source and binary forms, with or without modification,
  are permitted provided that the following conditions are met:
  
  Redistributions of source code must retain the above copyright notice, this list
  of conditions and the following disclaimer. 
  
  Redistributions in binary form must reproduce the above copyright notice, this list
  of conditions and the following disclaimer in the documentation and/or other materials
  provided with the distribution. 

  Neither the name of the owner nor the names of its contributors may be used to endorse or
  promote products derived from this software without specific prior written permission. 

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR
  IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
  FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE CONTRIBUTORS BE LIABLE FOR
  ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
  NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
  BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

//-*-c++-*-
//=============================================================================
//
//  Module : ekapi_util.cxx
//  $Date  : $
//  $Author: marcel $
//  $Source: /proj/osprey/CVS/open64/osprey1.0/common/ipfec_targ_info/access/ekapi_util.cxx,v $
//
//  Description:
//  ============
//  hold all common functions used in access layer of IPFEC.
//=============================================================================
#include <string.h>
#include "ekapi_util.h"

//trim the space from beginning and tailer.
char *StrTrim(char *str)
{
    int length;
    if (str != NULL)
    {
        length = strlen(str);
        while (str[0] == ' ') 
        {
            str = str +1;
            length = length-1;
        }
        while (str[length-1] == ' ') 
        {
            str[length-1] = '\0';
            length --; 
        }
    }
    return str;
}// end of strtrim

// convert integer to string and add prefix '%'
// limitation length 100.
char *itos(int num)
{
    char str[100],result[100];
    int i,j,count = 0;
    if ( num == 0 ) {
        return strdup("%0");
    }
    while (num != 0)
    {
        j = num % 10;
        num = (num - j)/10;
        str[count] = j + '0';
        count++;
        if (count>=100) break;
    }
    j = 1;
    result[0] = '%';
    for(i=count-1; i>=0; i--)
    {
        result[j] = str[i];
        j++;
    }
    result[j] = '\0';
    
    return strdup(result);
}// end of itos()

// Conver string into all upper case
char *StrUpper(char *src)
{
    char *p = src;
    for (;*p;p++){
        if ((*p>='a')&&(*p<='z')){
          *p += 'A'-'a';
        }
    }
    return src;
}// end of StrUpper()

// Conver string into all lower case
char *StrLower(char *src)
{
    char *p = src;
    for (;*p;p++){
        if ((*p>='A')&&(*p<='Z')){
          *p -= 'A'-'a';
        }
    }
    return src;
}// end of StrLower()

// Remove all blank, underscore from string
char *StrClean(char *src)
{
    char *begin = src;
    char *dest = src;
    for (;*src;src++){
        if ((*src!=' ')&&(*src!='_')){
            *dest = *src;
            dest++;
        }
    }
    *dest = '\0';
    return begin;
}// end of StrClean()
