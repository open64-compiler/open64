/*

  Copyright (C) 2000, 2001 Silicon Graphics, Inc.  All Rights Reserved.

  This program is free software; you can redistribute it and/or modify it
  under the terms of version 2.1 of the GNU Lesser General Public License 
  as published by the Free Software Foundation.

  This program is distributed in the hope that it would be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  

  Further, this software is distributed without any warranty that it is
  free of the rightful claim of any third person regarding infringement 
  or the like.  Any license provided herein, whether implied or 
  otherwise, applies only to this software file.  Patent licenses, if
  any, provided herein do not apply to combinations of this program with 
  other software, or any other product whatsoever.  

  You should have received a copy of the GNU Lesser General Public 
  License along with this program; if not, write the Free Software 
  Foundation, Inc., 59 Temple Place - Suite 330, Boston MA 02111-1307, 
  USA.

  Contact information:  Silicon Graphics, Inc., 1600 Amphitheatre Pky,
  Mountain View, CA 94043, or:

  http://www.sgi.com

  For further information regarding this notice, see:

  http://oss.sgi.com/projects/GenInfo/NoticeExplan

*/



#include <stdio.h>
#include <errno.h>
#include <signal.h>
#include <setjmp.h>

#define EBP     3
#define ESP     4
#define EIP     5

void unwind_frame(struct sigcontext *scp) {
	unsigned long ip, sp, bp;

	/* restore IP, SP, BP */
	ip = scp->eip;
	sp = scp->esp;
	bp = scp->ebp;
	if (0 == bp) {
		scp->eip = 0;
		return;
	}
	scp->eip = *(unsigned long *)(bp + 4);
	scp->esp = bp;
	scp->ebp = *(unsigned long *)bp;
}

void unwind_stack(void) {
	struct sigcontext sc;
	jmp_buf env;

	/* get current context */
	if (0 != setjmp(env)) {
		perror("setjmp() failed");
		exit(-1);
	}

	/* set up (partial) sigcontext */
	sc.ebp = (unsigned long)env->__jmpbuf[EBP];
	sc.esp = (unsigned long)env->__jmpbuf[ESP];
	sc.eip = (unsigned long)env->__jmpbuf[EIP];

	/* jump current frame */
	unwind_frame(&sc);

	/* stack back trace */
	while (sc.eip != 0) {
		fprintf(stdout,
			"pc=0x%lx sp=0x%lx bp=0x%lx\n", sc.eip, sc.esp, sc.ebp);
		unwind_frame(&sc);
	}
}
