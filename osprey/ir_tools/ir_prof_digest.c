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


#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <stdio.h>
#include <fcntl.h>
#include <errno.h>

#include <elf_abi.h>
#include <elf_mips.h>
#include <sys/types.h>
#include <sys/stat.h>

#include <sys/mman.h>
#include "ir_prof.h"
#include "ir_graph_util.h"

#define MALLOC(nbytes) \
    malloc((size_t)(nbytes))

#define REALLOC(ptr, size) \
    realloc((void *)(ptr), (size_t)(size))

char *raw_bits;
struct stat statb;
int fd_counts, fd;

#define TRUE 1
#define FALSE 0
#define EEOF -1
#define NOT_FOUND -1

#define FILE_OTHER	    0
#define FILE_EXECUTABLE	    1
#define FILE_RELOCATABLE    2
#define FILE_ARCHIVE	    3

#define ST_NONE	    0
#define ST_NAME	    1
#define ST_BIND	    2
#define ST_OTHER    3
#define ST_TYPE	    4
#define ST_SHNDX    5


static char *infile	= NULL;
static char *tool_name	= NULL;
static char *sect_name = NULL;
static int   dump_flag = 0;
static int   callgraph_flag = 0;
static int   sort_flag = 0;
static int   verbose_flag = 0;
static void *dummy;
static int   num_argc = 0;

static unsigned warn_flag = FALSE;
static unsigned sect_index = 0;
static unsigned targ32_flag = 0;
static counts_entry *carray;

typedef struct {
  char *name;
  ADDR size;
} proc_info;

static proc_info *parray;
static int        num_proc;

Elf64_Ehdr *p_elf_header;
Elf64_Shdr *p_sect_header;

__uint64_t    g_text_start;
__uint64_t    g_text_end;
__uint64_t    g_text_size;
__uint64_t    g_textlo_index;

#define ELF_TARG_32() (p_elf_header->e_ident[EI_CLASS] == ELFCLASS32)

#define ASSERT(EX, p)      if (!(EX)) sysfatal(p)


void usage()
{

    fprintf(stderr, "%s: Usage is: %s options... countsfile\n       options are:\n", tool_name, tool_name);
    fprintf(stderr, "\t-ebinaryname\n");
}


void
warn(char * const s, ...)
{
va_list ap;
FILE *stream;

  warn_flag = TRUE ;
	stream = stderr;
	fprintf(stream, "%s: WARNING: ", tool_name);

  va_start(ap, s);
  _doprnt(s, ap, stream);
  fprintf(stream, " [%s]", infile);
  fputs(".\n", stream);
  fflush(stream);
}


void sysfatal(char * const s, ...)
{
    va_list ap;
    FILE *stream;

    stream = stderr;

    int err_num = errno;
    errno = 0;

    char *err_str = strerror(err_num);
    
    if (err_num == EEOF) {
	fprintf(stream, "%s: %s, ", tool_name, "unexpected end of file");
    }
    else if (errno == 0) {
	fprintf(stream, "%s: %s, ", tool_name, strerror(errno));
    }
    else {
	fprintf(stream, "%s: error code %d, ", tool_name, err_num);
    }

    va_start(ap, s);
    _doprnt(s, ap, stream);
    fprintf(stream, " [%s]", infile);
    fputs(".\n", stream);

    exit(1);
}


void command_line (int argc, char **argv)
{
  register char **av, **ave;
  char *addrs_name = NULL;
  char *counts_name = NULL;
  int index = 1;
  
  num_argc = argc;
  for (av = &argv[1], ave = argv + argc; av != ave; index++) {
    register char *arg = *av++;
    if (arg[0] == '-') {
      register char *name = &arg[1];
      
      if (strcmp(name, "h") == 0) {
	usage();
	exit(1);
      }
      else if (name[0] == 'e') {
	infile = &arg[2];
	num_argc--;
      }
      else if (name[0] == 'd') {
	dump_flag = 1;
	num_argc--;
      }
      else if (name[0] == 'g') {
	callgraph_flag = 1;
	num_argc--;
      }
      else if (name[0] == 's') {
	sort_flag = 1;
	num_argc--;
      }
      else if (name[0] == 'v') {
	verbose_flag = 1;
	num_argc--;
      }
    }
    
  }
}


char *
open_file(char * const objname)
{
    char *raw_bits;
    int fd;

    fd = open(objname, O_RDWR, 0755);
    if (fd < 0)
	return 0;

    if (fstat(fd, &statb) != 0)
	return 0;

    if ((void *)(raw_bits = 
	(caddr_t)mmap(	0, 
			statb.st_size, 
			PROT_WRITE, 
			MAP_SHARED,
			fd,
			0)) == (void *)-1) {
        raw_bits = (char *)malloc (statb.st_size);
        if (!raw_bits)
	    return 0;

        if (read(fd, raw_bits, statb.st_size) != statb.st_size)
	    return 0;
    }
    return raw_bits;

} /* obj_open */


void close_file(char *raw_bits)
{
extern long *_end;
unsigned long	new_offset;

		/*
		 * Free malloc'd tables and sections.
		 */
    free(p_elf_header);
    free(p_sect_header);

    new_offset = (unsigned long)raw_bits;
    if (munmap ((caddr_t)raw_bits,statb.st_size) == -1)
	free(raw_bits);

    close(fd);

}


Elf64_Ehdr *
make_elfheader(char *raw_bits)
{
Elf64_Ehdr *p_ehdr, *tmpp_ehdr;

		/*
		 * Get target address size
		 */
    tmpp_ehdr = (Elf64_Ehdr *)raw_bits;
    targ32_flag = (tmpp_ehdr->e_ident[EI_CLASS] == ELFCLASS32);
    
    p_ehdr = (Elf64_Ehdr *)malloc(sizeof(Elf64_Ehdr));
    if (!(p_ehdr)) {
	sysfatal("malloc unable to allocate %d bytes",
		    sizeof(Elf64_Ehdr)); 
    }

    if (targ32_flag) {
	register unsigned i;
	Elf32_Ehdr *p32_ehdr = (Elf32_Ehdr *)raw_bits;
	
	for (i = 0; i<EI_NIDENT; i++) {
	    p_ehdr->e_ident[i] = p32_ehdr->e_ident[i];
	}
	p_ehdr->e_type	    = (Elf64_Half)p32_ehdr->e_type;
	p_ehdr->e_machine   = (Elf64_Half)p32_ehdr->e_machine;
	p_ehdr->e_version   = (Elf64_Word)p32_ehdr->e_version;
	p_ehdr->e_entry	    = (Elf64_Addr)p32_ehdr->e_entry;
	p_ehdr->e_phoff	    = (Elf64_Off )p32_ehdr->e_phoff;
	p_ehdr->e_shoff	    = (Elf64_Off )p32_ehdr->e_shoff;
	p_ehdr->e_flags	    = (Elf64_Word)p32_ehdr->e_flags;
	p_ehdr->e_ehsize    = (Elf64_Half)p32_ehdr->e_ehsize;
	p_ehdr->e_phentsize = (Elf64_Half)p32_ehdr->e_phentsize;
	p_ehdr->e_phnum	    = (Elf64_Half)p32_ehdr->e_phnum;
	p_ehdr->e_shentsize = (Elf64_Half)p32_ehdr->e_shentsize;
	p_ehdr->e_shnum	    = (Elf64_Half)p32_ehdr->e_shnum;
	p_ehdr->e_shstrndx  = (Elf64_Half)p32_ehdr->e_shstrndx;
    }
    else {
	Elf64_Ehdr *p64_ehdr = (Elf64_Ehdr *)raw_bits;
	memcpy((void *)p_ehdr, p64_ehdr, sizeof(Elf64_Ehdr));
    }
    
    return(p_ehdr);
}


Elf64_Shdr *
make_sectheader(char *raw_bits)
{
Elf64_Shdr *p_shdr;
unsigned sect_count;

    if (targ32_flag) {
	unsigned i;
	Elf32_Ehdr *p32_ehdr = (Elf32_Ehdr *)raw_bits;
	Elf32_Shdr *p32_shdr  = (Elf32_Shdr *)(raw_bits+p32_ehdr->e_shoff);
	
	sect_count = p32_ehdr->e_shnum;
	
	p_shdr = (Elf64_Shdr *)malloc(sizeof(Elf64_Shdr) * sect_count);
	if (!(p_shdr)) {
	    sysfatal("malloc unable to allocate %d bytes",
		sizeof(Elf64_Phdr) * sect_count); 
	}

	for (i = 0; i < sect_count; i++) {
	    p_shdr[i].sh_name	    = (Elf64_Word)p32_shdr[i].sh_name;
	    p_shdr[i].sh_type	    = (Elf64_Word)p32_shdr[i].sh_type;
	    p_shdr[i].sh_flags	    = (Elf64_Xword)p32_shdr[i].sh_flags;
	    p_shdr[i].sh_addr	    = (Elf64_Addr)p32_shdr[i].sh_addr;
	    p_shdr[i].sh_offset	    = (Elf64_Off)p32_shdr[i].sh_offset;
	    p_shdr[i].sh_size	    = (Elf64_Xword)p32_shdr[i].sh_size;
	    p_shdr[i].sh_link	    = (Elf64_Word)p32_shdr[i].sh_link;
	    p_shdr[i].sh_info	    = (Elf64_Word)p32_shdr[i].sh_info;
	    p_shdr[i].sh_addralign  = (Elf64_Xword)p32_shdr[i].sh_addralign;
	    p_shdr[i].sh_entsize    = (Elf64_Xword)p32_shdr[i].sh_entsize;
	}
    }
    else {
	Elf64_Ehdr *p64_ehdr = (Elf64_Ehdr *)raw_bits;
	Elf64_Shdr *p64_shdr  = (Elf64_Shdr *)(raw_bits+p64_ehdr->e_shoff);

	sect_count = p64_ehdr->e_shnum;
	
	p_shdr = (Elf64_Shdr *)malloc(sizeof(Elf64_Shdr) * sect_count);
	if (!(p_shdr)) {
	    sysfatal("malloc unable to allocate %d bytes",
		sizeof(Elf64_Shdr) * sect_count); 
	}

	memcpy((void *)p_shdr, p64_shdr, sizeof(Elf64_Shdr) * sect_count);
    }
    
    return(p_shdr);
}


int
is_elf_file(char *raw_bits, char *fname)
{
  Elf32_Ehdr *p_ehdr;
  int ret = 0;

  p_ehdr = (Elf32_Ehdr *)raw_bits;
  if (p_ehdr->e_ident[EI_MAG0] != ELFMAG0 ||
      p_ehdr->e_ident[EI_MAG1] != ELFMAG1 ||
      p_ehdr->e_ident[EI_MAG2] != ELFMAG2 ||
      p_ehdr->e_ident[EI_MAG3] != ELFMAG3) {
    return(FALSE);
  }
  
  if (p_ehdr->e_type == ET_REL)
    sysfatal( "%s not an executable", fname);
  
  targ32_flag = (p_ehdr->e_ident[EI_CLASS] == ELFCLASS32);
  
  return FILE_EXECUTABLE;
}


unsigned
find_section(char *raw_bits, char *name, unsigned type, unsigned type_only)
{
Elf64_Shdr *p_shdr, *p_section;
char *string_tab, *string;
unsigned offset;
unsigned i;

		/*
		 * Find the section.
		 */
    p_section = &p_sect_header[p_elf_header->e_shstrndx];
    string_tab = (char *)(raw_bits+p_section->sh_offset);

    for (i = 0; i < p_elf_header->e_shnum; i++) {
	if (p_sect_header[i].sh_type == type) {

	    if (type_only)
		return(i);

	    if (strcmp(string_tab+p_sect_header[i].sh_name, name) == 0) {
		return(i);
	    }
	}
    }
    
    return 0;
}


int
verify_counts_file(char *bits, char *fname)
{
  Counts_hdr *p;

  p = (Counts_hdr *)bits;

  if (p->c_ident[0] != COUNTSMAG0 ||
      p->c_ident[1] != COUNTSMAG1 ||
      p->c_ident[2] != COUNTSMAG2 ||
      p->c_ident[3] != COUNTSMAG3 ||
      p->c_ident[4] != COUNTSMAG4 ||
      p->c_ident[5] != COUNTSMAG5 ||
      p->c_ident[6] != COUNTSMAG6 ||
      p->c_ident[7] != COUNTSMAG7)
    sysfatal("Not a counts-file:%s\n",fname);

  if (p->c_version != C_VERSION)
    sysfatal("Counts-file version %d not supported\n",p->c_version);

  return p->c_entry;
}


void
close_counts_file(char *fname)
{
  close(fd_counts); 
}


counts_entry *
open_counts_file(char *fname, long *num_entry, char **count_str)
{
  char *counts_bits;

  ASSERT(fname, "openning file with null string");

  fd_counts = open(fname, 0, 0755);

  if (fd_counts < 0)
    sysfatal("Cannot open counts-file: %s\n",fname);

  if (fstat(fd_counts, &statb) != 0)
    sysfatal("Cannot fstat counts-file: %s\n",fname);
	
  if ((void *)(counts_bits = 
	       (caddr_t)mmap(	0, 
			statb.st_size, 
			PROT_WRITE, 
			MAP_SHARED,
			fd_counts,
			0)) == (void *)-1) {
    counts_bits = (char *)MALLOC (statb.st_size);
    if (!counts_bits)
      sysfatal("Cannot malloc counts-file size: %x\n",statb.st_size);
    
    if (read(fd_counts, counts_bits, statb.st_size) != statb.st_size)
      sysfatal("Cannot read counts-file: %s\n",fname);
   
  }
  *num_entry = verify_counts_file(counts_bits, fname);
  *count_str = (char *)counts_bits + sizeof(Counts_hdr) + (*num_entry * sizeof(counts_entry));

  return (counts_entry *)((char *)counts_bits+sizeof(Counts_hdr));
}


char *
find_and_get_str(ADDR addr, Elf64_Sym *symtab, int symcount, int start, char *strtab)
{
  unsigned int ilow, ihigh, ihalf;
  
  ilow = start;
  ihigh = symcount-1;
  ihalf = (ilow + ihigh) / 2;
  
  while (ilow < ihigh) {
    if (addr < symtab[ihalf].st_value)
      ihigh = ihalf;
    else if (addr > symtab[ihalf].st_value) {
      ilow = ihalf;
      if (ilow == ihigh - 1) {
	if (addr == symtab[ihigh].st_value)
	  return (char *)(strtab + symtab[ihigh].st_name);
	break;
      }
    }
    else {
      return (char *)(strtab + symtab[ihalf].st_name);
    }
    ihalf = (ilow + ihigh) / 2;
  }
  warn("address %x has no symbol", addr);
  return 0;
}


char *make_name(ADDR addr)
{
  char *tmp = MALLOC(12);

  ASSERT(tmp, "Cannot malloc string to manufacture name");

  sprintf(tmp, "_0x%x", addr);
  return tmp;
}


char *
get_name(ADDR mem, int idx, Elf64_Sym *symtab, int symcount, int symstart, char *strtab, char *count_strtab)
{
  char *name1;

  if (idx == 0) {
    if ((name1 = find_and_get_str(mem, symtab, symcount, symstart, strtab)) == 0) {
      /* manufacture a name */
      name1 = make_name(mem);
    }
  }
  else 
    name1 = count_strtab + idx;
  return name1;
}


void 
put_rec(counts_entry *pc, int idx)
{
  
  ASSERT(carray, "carray never initialized"); 
  carray[idx].count = pc->count;
  carray[idx].caller = pc->caller;
  carray[idx].callee = pc->callee;
  carray[idx].caller_name_idx = pc->caller_name_idx;
  carray[idx].callee_name_idx = pc->callee_name_idx;
}


int c_cmp(counts_entry *p1, counts_entry *p2)
{
  if (p1->count > p2->count)
    return -1;
  if (p1->count == p2->count)
    return 0;
  return 1;
}

typedef struct {
  char *name;
  ADDR a;
  VINDEX  idx;
} unique_name;

typedef struct {
  int caller;
  int callee;
  EINDEX edge_idx;
} map2unique;

static unique_name *pun;
static map2unique *c;   /* this gives the map from the counts array to the vertices in the graph */
/* 
   this is really quick and dirty, I am doing a linear search 
   from beginning and check for same name, same address 
*/
int
add_unique_name(char *name, ADDR a, unique_name *pun, int size)
{
  int i;
  char *p;

  for (i = 0; i < size; i++) {
    if (strcmp(pun[i].name, name) == 0) {
      if (pun[i].a == a)
	return i;
    }
  }
  
  p = MALLOC(strlen(name) + 1);
  ASSERT(p, "cannot malloc string for unique name");
  strcpy(p, name);

  pun[size].a = a;
  pun[size].name = p;
  return NOT_FOUND;
}


int
pr_dyn_count(EINDEX idx, long num_entry)
{
  long i, j;
  
  for (i=0; i < num_entry; i++) {
    if (c[i].edge_idx == idx) {
      ADDR caller, callee;

      caller = pun[c[i].caller].a;
      callee = pun[c[i].callee].a;
      
      for (j = 0; j < num_entry; j++) {
	if (carray[j].caller == caller && carray[j].callee == callee)
	  return j;
      }
    }
  }
  ASSERT(0, "edge_index not found");
  return 0;
}


extern ADDR get_proc_size();
static char ptmp[] = " (???)";

void
pr_vertex_name(int idx)
{
  ADDR s;

  printf("%s", pun[idx].name);

  if (verbose_flag) {
    s = get_proc_size(pun[idx].name);
    if (s) {
      printf(" (0x%x)", s);
    }
    else {
      printf(ptmp);
    }
  }
}


void
pr_count(long *p)
{
  printf("%d",*p);
}


void
build_call_graph(counts_entry *pc, long num_entry, Elf64_Sym *symtab, int symcount, int symstart, char *strtab, char *count_strtab)
{
  int i, count, j;
  long k;
  char *name;
  GRAPH *graph;

  count = 0;

  pun = MALLOC(num_entry * sizeof(unique_name));
  ASSERT(pun, "cannot malloc unique struct");

  c = MALLOC(num_entry * sizeof(map2unique));
  ASSERT(c, "cannot malloc map2unique array");
  
  for (i = 0; i < num_entry; i++) {
    name = get_name(pc[i].caller, pc[i].caller_name_idx, symtab, symcount, symstart, strtab, count_strtab);
    j = add_unique_name(name, pc[i].caller, pun, count);
    if (j == NOT_FOUND) {
      c[i].caller = count;
      count++;
    }
    else
      c[i].caller = j;

    name = get_name(pc[i].callee, pc[i].callee_name_idx, symtab, symcount, symstart, strtab, count_strtab);
    j = add_unique_name(name, pc[i].callee, pun, count);
    if (j == NOT_FOUND) {
      c[i].callee = count;
      count++;
    }
    else
      c[i].callee = j;
  }

  graph = build_graph_u(DEF_VERTEX_SIZE, DEF_EDGE_SIZE, dummy);
  for (i = 0; i < count; i++) {
    VINDEX idx;

    idx = add_vertex(graph, (void *)&pun[i]);
#ifdef _DEBUG
printf("add_vertex: %s (%d)\n",pun[i].name, idx);
#endif
    pun[i].idx = idx;
  }

  for (i = 0; i < num_entry; i++) {
    c[i].edge_idx = add_edge(graph, pun[c[i].caller].idx, pun[c[i].callee].idx, &pun[i]);
#ifdef _DEBUG
printf("add_edge: %s, --> %s (%d)\n",pun[c[i].caller].name, pun[c[i].callee].name, c[i].edge_idx);
#endif
   
    k = pr_dyn_count(c[i].edge_idx, num_entry);
    set_edge_u(graph, c[i].edge_idx, &carray[k].count);

    if (GRAPH_root(graph)  == INVALID_VINDEX )
      if (strcmp(pun[c[i].caller].name, "main") == 0)
	GRAPH_root(graph) = pun[c[i].caller].idx;
  }

{
  DFN *d;
  d = Depth_First_Ordering(graph, dummy);
  Print_DFN(d, graph, (void (*)())pr_vertex_name, pr_count);
}

}


void
process_counts(char *fname, Elf64_Sym *symtab, int symcount, int symstart, char *strtab)
{
  long num_entry;
  counts_entry *pc;
  long i, id1, id2;
  char *name1, *name2;
  char *count_strtab;

  pc = open_counts_file(fname, &num_entry, &count_strtab);

  if (!dump_flag) {
    if (carray == 0) {
      if ((carray = (counts_entry *)calloc(num_entry, sizeof(counts_entry))) == 0) {
	ASSERT(0, "Cannot allocate temporary array");
      }
    }
  }

  for (i = 0; i < num_entry; i++) {
    if (dump_flag) {
      name1 = get_name(pc[i].caller, pc[i].caller_name_idx, symtab, symcount, symstart, strtab, count_strtab);
      name2 = get_name(pc[i].callee, pc[i].callee_name_idx, symtab, symcount, symstart, strtab, count_strtab);
      printf("%s\t%s\t%d\n", name1, name2, pc[i].count);
    }
    else
      put_rec(&pc[i], i);
  }

  if (sort_flag) {
    char *v;

#define NOT_VISITED 1

    v = MALLOC(num_entry);
    ASSERT(v, "fail to malloc visited array of carray");
    memset(v, NOT_VISITED, num_entry);

    qsort(carray, num_entry, sizeof(counts_entry),  (int (*) ()) c_cmp);

    for (i = 0; i < num_entry; i++) {

      if (v[i]) {
	name1 = get_name(carray[i].caller, carray[i].caller_name_idx, symtab, symcount, symstart, strtab, count_strtab);
	name2 = get_name(carray[i].callee, carray[i].callee_name_idx, symtab, symcount, symstart, strtab, count_strtab);
	printf("%s\t%s\t%d\n",name1, name2, carray[i].count);
	
	if (verbose_flag) {
	  int j, k;
	  long total;
	  
	  total = 0;
	  for (j = i+1; j < num_entry; j++) {
	    if (carray[i].caller == carray[j].caller) {
	      if (total == 0) {
		for (k = 0; k < strlen(name1); k++) {
		  printf(" ");
		}
		printf("\t ");
	      }
	      	      
	      name2 =  get_name(carray[j].callee, carray[j].callee_name_idx, symtab, symcount, symstart, strtab, count_strtab);
	      printf(" %s/%d", name2, carray[j].count);
	      
	      total += carray[j].count;
	      v[j] = !NOT_VISITED;
	    }
	  }
	  if (total != 0) 
	    printf("  total - %d\n", total + carray[i].count);
	}
      }
    }
  }

  if (callgraph_flag) {
    build_call_graph(pc, num_entry, symtab, symcount, symstart, strtab, count_strtab);
  }
}


Elf64_Sym *
make_symtab64(char *orig, int count)
{
  Elf64_Sym *new;
  int i;

  if (ELF_TARG_32()) {
    Elf32_Sym *porig = (Elf32_Sym *)orig;

    new = MALLOC(count * sizeof (Elf64_Sym));
    ASSERT(new, "cannot malloc symtab64");

    for (i = 0; i < count; i++) {
      new[i].st_name = (Elf64_Word)porig[i].st_name;
      new[i].st_info = (Elf64_Byte)porig[i].st_info;
      new[i].st_other = (Elf64_Byte)porig[i].st_other;
      new[i].st_shndx = (Elf64_Section)porig[i].st_shndx;
      new[i].st_value = (Elf64_Addr)porig[i].st_value;
      new[i].st_size = (Elf64_Xword)porig[i].st_size;
    }
    return new;
  }
  else
    return (Elf64_Sym *)orig;
}


void
get_dynamic_rec(int index, char *dynamic, Elf64_Dyn *dyn)
{
  Elf64_Dyn *p = (Elf64_Dyn *)dynamic;
  if (ELF_TARG_32()) {
    Elf32_Dyn *p32 = (Elf32_Dyn *)dynamic;
    dyn->d_tag = p32[index].d_tag;
    dyn->d_un.d_val = p32[index].d_un.d_val;
  }
  else {
    dyn->d_tag = p[index].d_tag;
    dyn->d_un.d_val = p[index].d_un.d_val;
    return;
  }
}


ADDR
get_proc_size(char *name)
{
  int i;

  if (parray == 0)
    return 0;

  for (i = 0; i < num_proc; i++) {
    if (strcmp(name, parray[i].name) == 0)
      return (parray[i].size);
  }
  return 0;
}


/* I assume that section names are made up of .text{proc name} */
void
put_proc_name(char *sect_name, ADDR size)
{
  char *name;

  if (sect_name == 0 || *sect_name == '\0')
    return;

  if (strncmp(sect_name, ".text", 5) == 0) {
    if (strlen(sect_name) == 5)
      return;

    parray[num_proc].size = size;
    parray[num_proc].name = sect_name + 5;
    num_proc++;
  }
  return;
}


main(int argc, char **argv)
{
  unsigned index;
  int type;
  int i, sym_count, got_index;
  int start;
  Elf64_Sym *sym_tab;
  char *str_tab, *shstr_tab;
  char *raw_bits;
  int symindex;
  int strindex, dynindex, shstrindex;
  char *orig_tab, *dynamic;
  Elf64_Dyn e64_dyn;
  Elf32_Ehdr *p_ehdr;
    

  tool_name = argv[0];
  
  if (argc < 3) {
    usage();
    exit(1);
  }
  
  if (num_argc > 2) {
    sysfatal("Will only handle 1 counts-file for now");
  }
  
  command_line(argc, argv);
  if (infile) {
    if ((raw_bits = open_file(infile)) == 0)
      sysfatal("Cannot open file %s ", infile);
    
    if ((type = is_elf_file(raw_bits, infile)) == FILE_OTHER) 
      sysfatal("%s not an elf file", infile);
    
    
    p_elf_header = make_elfheader(raw_bits);
    p_sect_header = make_sectheader(raw_bits);
    
    symindex = find_section(raw_bits, ".dynsym", SHT_DYNSYM, FALSE);
    orig_tab = (char *)(raw_bits+p_sect_header[symindex].sh_offset);
    sym_count = p_sect_header[symindex].sh_size/p_sect_header[symindex].sh_entsize;
    /* normalize the symbol table */
    sym_tab = make_symtab64(orig_tab, sym_count);
    
    strindex = find_section(raw_bits, ".dynstr", SHT_STRTAB, FALSE);
    str_tab = (char *)(raw_bits+p_sect_header[strindex].sh_offset);
    
    dynindex = find_section(raw_bits, ".dynamic", SHT_DYNAMIC, FALSE);
    dynamic = (char *)(raw_bits+p_sect_header[dynindex].sh_offset);

    i = 0;
    do {
      get_dynamic_rec(i++, dynamic, &e64_dyn);
      if (e64_dyn.d_tag == DT_MIPS_GOTSYM) {
	got_index = e64_dyn.d_un.d_val;
	break;
      }
    } while (e64_dyn.d_tag != DT_NULL);
    
    if (got_index == 0)
      sysfatal("Got index of executable is 0");
  }
  else 
    sysfatal("No executable specified");

  if (verbose_flag) {
    int i, j;
    Elf64_Shdr *p_section = &p_sect_header[p_elf_header->e_shstrndx];

    parray = MALLOC(sizeof(proc_info) * p_elf_header->e_shnum);
    ASSERT(parray, "cannot malloc proc array");

    for (i = 0; i < p_elf_header->e_shnum; i++) {
      if (p_sect_header[i].sh_type == SHT_PROGBITS)
	put_proc_name((char *)(raw_bits+p_section->sh_offset + p_sect_header[i].sh_name), p_sect_header[i].sh_size);
    }
  }
  
  for (i = 1; i < argc; i++) {
    if (argv[i][0] == '-') 
      continue;
    process_counts(argv[i], sym_tab, sym_count, got_index, str_tab);
    close_counts_file(argv[i]);
  }
}

