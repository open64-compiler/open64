/*

  Copyright (C) 2000, 2001 Silicon Graphics, Inc.  All Rights Reserved.

  This program is free software; you can redistribute it and/or modify it
  under the terms of version 2 of the GNU General Public License as
  published by the Free Software Foundation.

  This program is distributed in the hope that it would be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  

  Further, this software is distributed without any warranty that it is
  free of the rightful claim of any third person regarding infringement 
  or the like.  Any license provided herein, whether implied or 
  otherwise, applies only to this software file.  Patent licenses, if 
  any, provided herein do not apply to combinations of this program with 
  other software, or any other product whatsoever.  

  You should have received a copy of the GNU General Public License along
  with this program; if not, write the Free Software Foundation, Inc., 59
  Temple Place - Suite 330, Boston MA 02111-1307, USA.

  Contact information:  Silicon Graphics, Inc., 1600 Amphitheatre Pky,
  Mountain View, CA 94043, or:

  http://www.sgi.com

  For further information regarding this notice, see:

  http://oss.sgi.com/projects/GenInfo/NoticeExplan

*/

#include <stdio.h>

#include "arith.h"

main() {
	AR_HOST_SINT64	ans[4];
	AR_HOST_SINT64	arg[8];
	AR_HOST_SINT64	result[4];
	AR_HOST_SINT64	xor;

	AR_TYPE		rtype,opndtype,ptype;

	char		line[256];
	char		prevfunc[9];
	char		func[9];
	char*		linep;
	char*		num;

	int		i,l,n;
	int		ierr;
	int		pass,okerrs,fail;

	int		rsize;

	FILE		*file;

	pass=okerrs=fail=0;

	file = fopen("results/mpp_sim_data","r");
	if(file == NULL) {
	  fprintf(stderr,"Could not open results/mpp_sim_data\n");
	  exit(0);
	}

	prevfunc[0]=0;
	while(fgets(line,256,file) != NULL) {
	  i=l=0;
	  while(line[l] != '(')
	    func[i++]=line[l++];
	  func[i]=0;
	  if(strcmp(func,"STOP") == 0) exit(0);
	  if(strcmp(func,prevfunc)) {
	    printf("Testing %s intrinsic\n",func);
	    strcpy(prevfunc,func);
	  }
	  l++;
	  num=NULL;
	  if(func[0]=='C') {
	    if(func[1]=='D')
	      if(strcmp(func,"CDABS") != 0)
	        rsize=4;
	      else
	        rsize=2;
	    else if(strcmp(func,"CABS")!=0 && strcmp(func,"COS")!=0)
	      rsize=2;
	    else
	      rsize=1;
	  }
	  else if(func[0]=='D')
	    rsize=2;
	  else if(strcmp(func,"STRTOF") == 0) {
	    rsize=1;
	    num=line+l;
	  }
	  else if(strcmp(func,"STRTOD") == 0) {
	    rsize=1;
	    num=line+l;
	  }
	  else if(strcmp(func,"STRTOLD") == 0) {
	    rsize=2;
	    num=line+l;
	  }
	  else
	    rsize=1;
	  n=0;
	  while(line[l] != ')') {
	    if(num == NULL) {
	      if(line[l]=='C') {
	        if(line[++l]=='D')
	          { n+=4; l++; }
	        else
	          n+=2;
	      }
	      else if(line[l++]=='D')
	        n+=2;
	      else
	        n+=1;
	      if(line[l]==',')
	        l++;
	      else if(line[l]!=')') {
	        fprintf(stderr,"Invalid input on line:\n   %s\n",line);
	        n=0;
	        break;
	      }
	    }
	    else {
	      n=1;
	      l++;
	    }
	  }
	  if(n == 0) continue;
	  linep=&line[++l];
	  if(num == NULL) {
	    for(i=0;i<n;i++)
	      get_hex_input(&linep, &arg[i]);
	  }
	  else {
	    line[l-1] = 0;
	    memcpy((char*)(&arg[0])+8-sizeof(char*), &num, sizeof(char*));
	  }
	  for(i=0;i<rsize;i++)
	    get_hex_input(&linep, &ans[i]);
	  l = strlen(func);
	  if(strncmp(func,"STRTO",5) == 0) {
	    if(func[5] == 'F')
	      rtype = AR_Float_IEEE_NR_32;
	    else if(func[5] == 'D')
	      rtype = AR_Float_IEEE_NR_64;
	    else
	      rtype = AR_Float_IEEE_NR_128;
	    ierr=AR_convert_str_to_float((AR_DATA*)&result[0], &rtype, num);
	    num[strlen(num)] = ')';
	  }
	  else if(strncmp(&func[l-3],"LOG",3) == 0) {
	    if(func[0] == 'A')
	      rtype = AR_Float_IEEE_NR_64;
	    else if(func[0] == 'D')
	      rtype = AR_Float_IEEE_NR_128;
	    else if(func[0] == 'H')
	      rtype = AR_Float_IEEE_NR_32;
	    else if(func[1] == 'L')
	      rtype = AR_Complex_IEEE_NR_64;
	    else
	      rtype = AR_Complex_IEEE_NR_128;
	    ierr = AR_log((AR_DATA*)&result[0], &rtype, (AR_DATA*)&arg[0], &rtype);
	  }
	  else if(strncmp(&func[l-3],"EXP",3) == 0) {
	    if(func[0] == 'E')
	      rtype = AR_Float_IEEE_NR_64;
	    else if(func[0] == 'D')
	      rtype = AR_Float_IEEE_NR_128;
	    else if(func[0] == 'H')
	      rtype = AR_Float_IEEE_NR_32;
	    else if(func[1] == 'E')
	      rtype = AR_Complex_IEEE_NR_64;
	    else
	      rtype = AR_Complex_IEEE_NR_128;
	    ierr = AR_exp((AR_DATA*)&result[0], &rtype, (AR_DATA*)&arg[0], &rtype);
	    if(func[0] == 'C')		/* Fix order-of-operation diff in cexp */
	      if((ierr&AR_STAT_UNDEFINED) && ans[0]==AR_STAT_OVERFLOW)
	        ierr = (ierr^AR_STAT_UNDEFINED) | AR_STAT_OVERFLOW;
	      else if((ierr&AR_STAT_OVERFLOW) && ans[0]==AR_STAT_UNDEFINED)
	        ierr = (ierr^AR_STAT_OVERFLOW) | AR_STAT_UNDEFINED;
	  }
	  else if(strncmp(&func[l-4],"SQRT",4) == 0) {
	    if(func[0] == 'S')
	      rtype = AR_Float_IEEE_NR_64;
	    else if(func[0] == 'D')
	      rtype = AR_Float_IEEE_NR_128;
	    else if(func[0] == 'H')
	      rtype = AR_Float_IEEE_NR_32;
	    else if(func[1] == 'S')
	      rtype = AR_Complex_IEEE_NR_64;
	    else
	      rtype = AR_Complex_IEEE_NR_128;
	    ierr = AR_sqrt((AR_DATA*)&result[0], &rtype, (AR_DATA*)&arg[0], &rtype);
	  }
	  else if(strncmp(&func[l-3],"ABS",3) == 0) {
	    if(func[1] == 'A') {
	      rtype = AR_Float_IEEE_NR_64;
	      opndtype = AR_Complex_IEEE_NR_64;
	    }
	    else {
	      rtype = AR_Float_IEEE_NR_128;
	      opndtype = AR_Complex_IEEE_NR_128;
	    }
	    ierr = AR_cabs((AR_DATA*)&result[0], &rtype, (AR_DATA*)&arg[0], &opndtype);
	  }
	  else if(strncmp(&func[l-3],"TOI",3)==0 ||
	  	  strncmp(&func[l-3],"TOR",3)==0) {
	    if(func[0] == 'I') {
	      rtype = AR_Int_64_S;
	      opndtype = AR_Int_64_S;
	    }
	    else if(func[0] == 'R') {
	      rtype = AR_Float_IEEE_NR_64;
	      opndtype = AR_Float_IEEE_NR_64;
	    }
	    else if(func[0] == 'H') {
	      rtype = AR_Float_IEEE_NR_32;
	      opndtype = AR_Float_IEEE_NR_32;
	    }
	    else if(func[0] == 'D') {
	      rtype = AR_Float_IEEE_NR_128;
	      opndtype = AR_Float_IEEE_NR_128;
	    }
	    else if(func[1] == 'T') {
	      rtype = AR_Complex_IEEE_NR_64;
	      opndtype = AR_Complex_IEEE_NR_64;
	    }
	    else {
	      rtype = AR_Complex_IEEE_NR_128;
	      opndtype = AR_Complex_IEEE_NR_128;
	    }
	    if(func[l-1] == 'I')
	      ptype = AR_Int_64_S;
	    else
	      ptype = AR_Float_IEEE_NR_64;
	    ierr = AR_power((AR_DATA*)&result[0], &rtype, (AR_DATA*)&arg[0], &opndtype, (AR_DATA*)&arg[n-1], &ptype);
	  }
	  else if(strncmp(&func[0],"MODULO",6)==0 ) {
	    if(func[6] == 'I')
	      rtype = AR_Int_32_S;
	    else if(func[6] == 'J')
	      rtype = AR_Int_64_S;
	    else if(func[6] == 'F')
	      rtype = AR_Float_IEEE_NR_32;
	    else if(func[6] == 'S')
	      rtype = AR_Float_IEEE_NR_64;
	    ierr = AR_Modulo((AR_DATA*)&result[0], &rtype, (AR_DATA*)&arg[0], &rtype, (AR_DATA*)&arg[1], &rtype);
	  }
	  else if(strncmp(&func[0],"SELREALK",8)==0 ) {
	    rtype = AR_Int_64_S;
	    if(arg[0] < 0 && arg[1] < 0)
	      ierr = AR_selected_real_kind((AR_DATA*)&result[0], &rtype, (AR_DATA*)NULL, &rtype, (AR_DATA*)NULL, &rtype);
	    else if(arg[0] < 0)
	      ierr = AR_selected_real_kind((AR_DATA*)&result[0], &rtype, (AR_DATA*)NULL, &rtype, (AR_DATA*)&arg[1], &rtype);
	    else if(arg[1] < 0)
	      ierr = AR_selected_real_kind((AR_DATA*)&result[0], &rtype, (AR_DATA*)&arg[0], &rtype, (AR_DATA*)NULL, &rtype);
	    else
	      ierr = AR_selected_real_kind((AR_DATA*)&result[0], &rtype, (AR_DATA*)&arg[0], &rtype, (AR_DATA*)&arg[1], &rtype);
	  }
	  else {
	    if(func[0] == 'D') {
	      rtype = AR_Float_IEEE_NR_128;
	      opndtype = AR_Float_IEEE_NR_128;
	      ptype = AR_Float_IEEE_NR_128;
	    }
	    else if(func[1] == 'T') {
	      rtype = AR_Complex_IEEE_NR_64;
	      opndtype = AR_Complex_IEEE_NR_64;
	      ptype = AR_Complex_IEEE_NR_64;
	    }
	    else {
	      rtype = AR_Complex_IEEE_NR_128;
	      opndtype = AR_Complex_IEEE_NR_128;
	      ptype = AR_Complex_IEEE_NR_128;
	    }
	    ierr = AR_power((AR_DATA*)&result[0], &rtype, (AR_DATA*)&arg[0], &opndtype, (AR_DATA*)&arg[n>>1], &ptype);
	  }
	  ierr &= (AR_STAT_OVERFLOW|AR_STAT_UNDERFLOW|AR_STAT_UNDEFINED|AR_STAT_INVALID_TYPE);
	  if(rsize > 1 && (result[0]>>48) == 0x7fff)
	     xor = result[0]^ans[0];
	  else
	     for(xor=0, i=0; i<rsize; i++)
	        xor |= (result[i]^ans[i]);
	  if(ierr!=0 || xor!=0) {
	    if(ierr!=0 && ans[0]==ierr)
	      okerrs++;
	    else {
	      fprintf(stderr,"\n***** ERROR *** ERROR *** ERROR *** ERROR *****\n");
	      fprintf(stderr,"   Intrinsic result does not match expected result on input line:\n   %s\n",line);
	      if(ierr != 0)
	        fprintf(stderr,"   The intrinsic returned an error code = 0%o\n",ierr);
	      else {
	        fprintf(stderr,"   The intrinsic returned a result of:\n   ");
	        for(i=0; i<rsize; i++)
	          fprintf(stderr," %16.16llx",result[i]);
	        fprintf(stderr,"\n");
	      }
	      fail++;
	    }
	  }
	  else
	    pass++;
	}

	printf("Intrinsic test results:\n%6d passed\n%6d passed with expected error condition\n%6d FAILED!!!\n",pass,okerrs,fail);
	exit(fail);

}

get_hex_input(str, arg)
char**	str;
AR_HOST_SINT64 *arg;
{
	char*	line;

	int	h1,h2,h3,h4;

	line = *str;

	while(*line == ' ') line++;

	h1=(*line>='A')?*line-'A'+10:*line-'0'; line++;
	h2=(*line>='A')?*line-'A'+10:*line-'0'; line++;
	h3=(*line>='A')?*line-'A'+10:*line-'0'; line++;
	h4=(*line>='A')?*line-'A'+10:*line-'0'; line++;
	*arg  = (AR_HOST_SINT64)((h1<<12) | (h2<<8) | (h3<<4) | h4)<<16;

	h1=(*line>='A')?*line-'A'+10:*line-'0'; line++;
	h2=(*line>='A')?*line-'A'+10:*line-'0'; line++;
	h3=(*line>='A')?*line-'A'+10:*line-'0'; line++;
	h4=(*line>='A')?*line-'A'+10:*line-'0'; line++;
	*arg |= (AR_HOST_SINT64)((h1<<12) | (h2<<8) | (h3<<4) | h4);

	if((*line>='0' && *line<='9') ||
	   (*line>='A' && *line<='F')) {
	  *arg <<= 32;
	  h1=(*line>='A')?*line-'A'+10:*line-'0'; line++;
	  h2=(*line>='A')?*line-'A'+10:*line-'0'; line++;
	  h3=(*line>='A')?*line-'A'+10:*line-'0'; line++;
	  h4=(*line>='A')?*line-'A'+10:*line-'0'; line++;
	  *arg |= (AR_HOST_SINT64)((h1<<12) | (h2<<8) | (h3<<4) | h4)<<16;

	  h1=(*line>='A')?*line-'A'+10:*line-'0'; line++;
	  h2=(*line>='A')?*line-'A'+10:*line-'0'; line++;
	  h3=(*line>='A')?*line-'A'+10:*line-'0'; line++;
	  h4=(*line>='A')?*line-'A'+10:*line-'0'; line++;
	  *arg |= (AR_HOST_SINT64)((h1<<12) | (h2<<8) | (h3<<4) | h4);
	}

	*str = line;
}


static char USMID [] = "\n%Z%%M%	%I%	%G% %U%\n";
static char rcsid [] = "$Id: test_mpp_sim.c,v 1.1.1.1 2005/10/21 19:00:00 marcel Exp $";
