/*
 * Copyright 2003, 2004 PathScale, Inc.  All Rights Reserved.
 */

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


#include <errno.h>		    /* for sys_errlist */
#include <stdio.h>		    /* for stderr */
#include <fcntl.h>
#include <libgen.h>		    /* for basename() */
#include <sys/stat.h>
#include "ir_prof.h"
static char *thisfile = __FILE__;


static int fd_counts;   /* counts file handle */

#define FALSE 0
#define TRUE  1
#define NUM_COUNTS_ENTRY 4097
#define SAMPLE_SIZE 15

static counts_entry *counts_array;
static long   num_counts;
static long   sizeof_counts_array = NUM_COUNTS_ENTRY;

typedef struct {
    unsigned *phash;   /* the hash */
    unsigned *chain;   /* real symbol chain */
    char     *strtab;
    unsigned  nchain;  /* # of syms in chain */
    unsigned  size;    /* number of buckets */
    unsigned  nsym;    /* max # of syms in chain */
    unsigned  strsize;
    unsigned  nchars;
} HASH_STRUCT;

#define DEF_STRSIZE 3
#define DEF_NBUCKETS (NUM_COUNTS_ENTRY-1)
#define DEF_NSYM (NUM_COUNTS_ENTRY-1)
#define UNDEF -1
static HASH_STRUCT h_tab;

extern void hashinit(void);



extern void *atexit();

extern int
file_exists (char *path)
{
  int st;
  struct stat sbuf;
  st = stat(path, &sbuf);
  if (st == -1 && (errno == ENOENT || errno == ENOTDIR))
    return FALSE;
  else
    return TRUE;
}


static void
usage (char *progname)
{
  fprintf (stderr, "Usage: %s [-v] <Binary IR>\n", progname);
  fprintf (stderr, "\t-v option will print out verbose info\n");
  fprintf (stderr, "\tall sizes are in bytes\n");
  exit (1);
}


#define PR_ASSERT(EX, p)      if (!(EX)) ir_prof_error(ER_FATAL, p, "Ir_pr")
#define MALLOC(nbytes)        malloc((size_t)(nbytes))
#define REALLOC(ptr, size)    realloc((void *)(ptr), (size_t)(size))

void 
ir_prof_dump(void)
{
  int i;
 
  printf("Edge Counts Dump: (%d), total = %d\n", num_counts, SAMPLE_SIZE);
  for (i=0; i < num_counts; i++) {
    printf("\t0x%x : 0x%x\t%d\t%s(%d):\t%s(%d)\n", 
	   counts_array[i].caller, counts_array[i].callee, counts_array[i].count,
	   (h_tab.strtab + counts_array[i].caller_name_idx),counts_array[i].caller_name_idx,
	   (h_tab.strtab + counts_array[i].callee_name_idx),counts_array[i].callee_name_idx);
  }
}


void 
ir_prof_error(int type, char *fmt, char *fname)
{
    fprintf (stderr, "%s: ", "Ir_prof");
    fprintf(stderr, fmt, fname);
    fputc('\n', stderr);
    fflush(stderr);
    switch (type) {
    ER_FATAL:
      exit(1);
    ER_WARNING:
    ER_INFO:
    ER_ERROR:
    ER_VERBOSE:
    ER_MSG:
      return;
    }
}


void
ir_prof_start(void)
{
  char *errmsg;

  counts_array = (counts_entry *)(MALLOC(NUM_COUNTS_ENTRY * sizeof(counts_entry)));
  PR_ASSERT(counts_array, "ir_prof_start: malloc of counts_entry returns null");
  num_counts = 0;
  hashinit();
  return;
}


void 
ir_prof_dump_counts(int fd_counts)
{
  Counts_hdr hdr;

  PR_ASSERT(fd_counts, "ir_prof_dump_counts: fd_counts is null");

  hdr.c_ident[0] = COUNTSMAG0;
  hdr.c_ident[1] = COUNTSMAG1;
  hdr.c_ident[2] = COUNTSMAG2;
  hdr.c_ident[3] = COUNTSMAG3;
  hdr.c_ident[4] = COUNTSMAG4;
  hdr.c_ident[5] = COUNTSMAG5;
  hdr.c_ident[6] = COUNTSMAG6;
  hdr.c_ident[7] = COUNTSMAG7;
  hdr.c_entry = num_counts;
  hdr.c_version = C_VERSION;
  hdr.c_dummy1 = 0;
#ifdef _64BIT
  hdr.c_dummy2 = 0;
  hdr.c_dummy3 = 0;
#endif
  write(fd_counts, (char *)&hdr, sizeof(hdr));
  write(fd_counts, (char *)counts_array, sizeof(counts_entry)*num_counts);
  write(fd_counts, h_tab.strtab, h_tab.nchars);
}


void
cgt_fini(void)
{
  char *errmsg;
  char *name = "junk.prof";
  int file_exists = 0;
  struct stat statbuf;
  int res, fd_counts;

  if (num_counts == 0)
    return;

  res = stat(name,&statbuf);
  if (res < 0) {
    if(errno == ENOENT) {
      /* file does not exist */
      if ((fd_counts = open(name, O_RDWR|O_CREAT, 0555)) < 0) {
	errmsg = strerror(errno);
	ir_prof_error(ER_FATAL, "Unable to open addr_counts file: %s", name);
      }
    } else {
      errmsg = strerror(errno);
      ir_prof_error(ER_FATAL, "Unable to open addr_counts file: %s", name);
    }
  } 
  else { /* file exists */
    file_exists = 1;
    if ((fd_counts = open(name, O_RDWR|O_TRUNC, 0555)) == 0) {
      errmsg = strerror(errno);
      ir_prof_error(ER_FATAL, "Existing addr_counts file open error: %s", name);
    }
  }

  /* sort and flush counts */
  (void)ir_prof_dump();
  ir_prof_dump_counts(fd_counts);
  /* close the file. */
  close (fd_counts);
}


void 
ir_prof_init(void)
{
  void (*pf)() = cgt_fini;
  num_counts = 0;
  atexit(pf);
}


void hashinit()
{
    unsigned *p;

    h_tab.nchain = 0;
    h_tab.strsize = DEF_STRSIZE;

    if ((p = (unsigned *)malloc(h_tab.strsize)) == (unsigned *)0)
	ir_prof_error(ER_FATAL, "Malloc error in %s\n", "malloc strtab");
    h_tab.strtab = (char *)p;
    p[0] = '\0';     /* first entry in strtab is a null */
    h_tab.nchars = 1;

    if ((p = (unsigned *)malloc(DEF_NBUCKETS * sizeof(unsigned))) == (unsigned *)0)
	ir_prof_error(ER_FATAL, "Malloc error in %s\n", "malloc hash");
    memset((char *)p, -1, DEF_NBUCKETS * sizeof(unsigned));
    h_tab.phash = p;
    h_tab.size = DEF_NBUCKETS;
   
    if ((p = (unsigned *)malloc(DEF_NSYM *sizeof(unsigned))) == (unsigned *)0)
	ir_prof_error(ER_FATAL, "Malloc error in %s\n", "hashtab");
    bzero (p, DEF_NSYM * sizeof(unsigned));
    h_tab.chain = p;
    h_tab.nsym = DEF_NSYM;
    return ;
}


void
expand_htab()
{
    unsigned *pnew;
    int nsym = h_tab.nsym * 2;

    if ((pnew = (unsigned *)realloc(h_tab.chain, nsym*sizeof(unsigned))) == 0)
	ir_prof_error(ER_FATAL, "Realloc error when %s", "expanding hash");
    h_tab.chain = pnew;
    bzero (&pnew[h_tab.nsym], h_tab.nsym * sizeof(unsigned));
    h_tab.nsym = nsym;
}


void
expand_strtab(int i)
{
  char *pnew;

  h_tab.strsize = 2 * (h_tab.strsize + i);
  if ((pnew = (char *)realloc(h_tab.strtab, h_tab.strsize)) == 0)
    ir_prof_error(ER_FATAL, "Realloc error when %s", "expanding strtab");
  h_tab.strtab = pnew;
}


int
enter_str(char *name)
{
  int i, ret;
  char *p = h_tab.strtab;

  if (name == 0 || name[0] == '\0')
    return 0;

  i = strlen(name) + 1;
  if ((i + h_tab.nchars) > h_tab.strsize) 
    expand_strtab(i);

  memcpy(h_tab.strtab + h_tab.nchars, name, i);
  ret = h_tab.nchars;
  h_tab.nchars += i;
  return ret;
}


static void
hash_entry(ADDR caller, ADDR callee, char *caller_name, char *callee_name) 
{
  unsigned *pchain = h_tab.chain;
  int y;
  int i = h_tab.nchain;

  /* the hash uses caller and callee address only, this will cause */
  /* duplication in the string table when either one of them is */
  /* different. I am ignoring that for now */
  int hash = ((caller + callee) & 0x0FFFFFFFF) % h_tab.size;

/* printf("caller %x callee %x hash %d\n",caller, callee, hash);   */
  if ((y = h_tab.phash[hash]) == UNDEF) {
    if (h_tab.nchain == h_tab.nsym) {
      expand_htab();
      pchain = h_tab.chain;
    }
    pchain[i] = UNDEF;

    counts_array[i].count = 1;
    counts_array[i].caller = caller;
    counts_array[i].callee = callee;
    counts_array[i].caller_name_idx = enter_str(caller_name);
    counts_array[i].callee_name_idx = enter_str(callee_name);

    h_tab.phash[hash] = h_tab.nchain++;
    num_counts++;
/* printf("not found and new\n"); */
    return;
  }

  while (y != UNDEF) {
    if (counts_array[y].caller == caller &&
	counts_array[y].callee == callee) {
      counts_array[y].count++;
/* printf("found and count %d\n", counts_array[y].count); */
      return;
    }
    hash = y;
    y = pchain[y];
  }

  /* not found */
  pchain[hash] = h_tab.nchain;
  if (i == h_tab.nsym) {
    expand_htab();
    pchain = h_tab.chain;
  }
/* printf("not found and old hash\n"); */
  pchain[i] = UNDEF;

  counts_array[i].caller = caller;
  counts_array[i].callee = callee;
  counts_array[i].caller_name_idx = enter_str(caller_name);
  counts_array[i].callee_name_idx = enter_str(callee_name);
  counts_array[i].count = 1;
  
  h_tab.nchain++;
  num_counts++;
  return;
}


void 
__profile_call(ADDR caller, ADDR callee, char *caller_name, char *callee_name)
{
#ifdef _DEBUG
  return;
#else
  if (num_counts == 0)
    ir_prof_start();

  if (num_counts == sizeof_counts_array) {
    sizeof_counts_array *= 2;
    counts_array = (counts_entry *)REALLOC(
		    counts_array, sizeof_counts_array);
    PR_ASSERT(counts_array, "realloc of counts_array failed");
  }

  hash_entry(caller, callee, caller_name, callee_name);
#endif
}



#ifdef _MAIN_DEBUG


struct counts_desc_char {
  char caller[9];
  char callee[9];
  int caller_idx;
  int callee_idx;
};

static char *str[6] = {
"", "scan", "foo", "a", "longgggggggggggg", "new"};
static struct counts_desc_char debugi[SAMPLE_SIZE] = {
{"1000129c","1000129c",1, 1},
{"1000133c","1000129c",2, 1},
{"100012bc","1000129c",3, 1},
{"1000127c","1000127c",4, 4},
{"100012bc","100012ac",3, 0},
{"1000127c","1000129c",4, 1},
{"1000133c","1000129c",2, 1},
{"1000129c","1000127c",1, 4},
{"100012bc","1000129c",3, 1},
{"10002340","1000129c",5, 1},
{"1000127c","1000127c",4, 4},
{"1000127c","1000127c",4, 4},
{"100012bc","1000133c",3, 2},
{"1000133c","10002340",2, 5},
{"1000133c","1000127c",2, 4},
};
static counts_entry debug[SAMPLE_SIZE];


long h2d(char *p)
{
  int i;
  long val = 0;
  long ret = 0;

  for (i=0; i < 8; i++) {
    if (p[i] < '0' || p[i] > '9') {
      val = p[i] - 'a' + 10;
    }
    else
      val = p[i] - '0';

    ret = ret * 16 + val;
  }
  return ret;
}


int 
debug_init(void)
{
  int i;
  for (i=0; i < SAMPLE_SIZE; i++) {
    debug[i].caller = h2d(debugi[i].caller);
    debug[i].callee = h2d(debugi[i].callee);
    debug[i].caller_name_idx = debugi[i].caller_idx;
    debug[i].callee_name_idx = debugi[i].callee_idx;
  }
  return SAMPLE_SIZE;
}


void
main (int argc, char *argv[])
{
    register char *progname = "ir_prof";
    int binarg = 1;
    int verbose = FALSE;
    int j, i;

    progname = (argv[0]);

    if (argc == 2) {
      if (strcmp(argv[1], "-h") == 0)
	usage(progname);
      exit(0);
    }
    j = debug_init();
    for (i = 0; i < j; i++) {
      __profile_call(debug[i].caller, debug[i].callee, str[debug[i].caller_name_idx],
		     str[debug[i].callee_name_idx]);
    }
    ir_prof_dump();
    cgt_fini();

    exit (0);
} /* main */
#endif

