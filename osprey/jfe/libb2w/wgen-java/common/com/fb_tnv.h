/*
 * Copyright 2003, 2004 PathScale, Inc.  All Rights Reserved.
 */

/*
  Copyright (C) 2000-2003, Institute of Computing Technology, Chinese Academy of Sciences
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
 
#ifndef fb_tnv_INCLUDED
#define fb_tnv_INCLUDED

#include "defs.h"

#define FB_TNV_SIZE  10

struct FB_TNV{
  UINT32 _id; // instruction id.
  INT32 _flag; //0 for integer type, 1 for float type.
  //Note: for "float" type, we just put it here as an "integer". Because they have same size.
  UINT64 _address; //record previous address
  UINT64 _exec_counter; // how many times does this instruction executed.
  UINT64 _clear_counter;
  UINT64 _sample_counter; //do stride profile sample 
  UINT64 _stride_steps;
  UINT64 _zero_std_counter;
  UINT64 _values[FB_TNV_SIZE]; //top 10 values. 
  UINT64 _counters[FB_TNV_SIZE]; //counters for top 10 values.

  FB_TNV():_address(0),_sample_counter(0),_zero_std_counter(0),_stride_steps(0){}
    
  void Print( FILE *fp ) const {
    fprintf(fp, "id(%u), exec_counter(%llu), flag(%d), zero_std_counter(%llu), (values(counters)=( %lld(%llu), %lld(%llu), %lld(%llu), %lld(%llu), %lld(%llu), %lld(%llu), %lld(%llu), %lld(%llu), %lld(%llu), %lld(%llu) ) )\n",
	    _id, _exec_counter, _flag, _zero_std_counter,
	    _values[0], _counters[0] , _values[1], _counters[1], 
	    _values[2], _counters[2], _values[3], _counters[3], _values[4], _counters[4], 
	    _values[5], _counters[5], _values[6], _counters[6], _values[7], _counters[7], 
	    _values[8], _counters[8], _values[9], _counters[9]  
	    );
  }
};



#endif
