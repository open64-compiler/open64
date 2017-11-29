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



#ident "$Revision: 1.1.1.1 $"

#include <sys/types.h>
#include <sys/syssgi.h>
#include <sys/stat.h>
#include <sys/mman.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <limits.h>
#include <stdio.h>
#include <elf_abi.h>
#include <elf_mips.h>
#include <string.h>
#include <errno.h>
#include <cmplrs/elfmap.h>

#define MAX_PAGE_SIZE 0x00010000
#define OLDVM 
#define TWOMEG 0x200000
#define TWOMEGMASK 0xffe00000

#define DPRINTF if (debug) printf
#ifdef DEBUG
static int debug = 1;
#else
static int debug = 0;
#endif

/* Cleanup states */
#define INITIAL_STATE 0
/* This state left open in case I want to allocate my environment,
 * instead of having it on the stack.
 */
#define FILE_OPEN 2
#define EH_ALLOC 3
#define EH_SUCCESS 4
#define PH_ALLOC 5
#define PH_SUCCESS 6
#define ISMAP_ALLOC 7
#define SEG_MAPPED 8
#define MAP_SUCCESS 9

/* TRUE AND FALSE */
#define TRUE 1
#define FALSE 0

/* Maximum number of loadable segments. */
#define MAX_LOAD_SEGS 20

/* TYPES *****************************************************/

typedef struct {
  int state; /* Keeps track of how far we are. */
  int infd;
  void *retval;
  int *isMapped;
  int options;
  int isPIC;
} em_environ;

typedef struct {
  void *mm_addr;
  int  mm_len;
  int  mm_prot;
  int  mm_flags;
  off_t mm_off;
  int  mm_filesz;
} mm_vector;


/* PROTOTYPES ************************************************/
static Elf32_Ehdr *getEhdr(em_environ *);
static int validEhdr(Elf32_Ehdr *);
static Elf32_Phdr *getPhdr(Elf32_Ehdr *, em_environ *);
static void cleanup(em_environ *, Elf32_Ehdr *, Elf32_Phdr *);
extern void error(char * fmt, ... );

/* UNUSED PROTOTYPES *****************************************/

/**************************************************************
*
*  _elfunmap
*  elfunmap
*
*  This function unmaps a segment of an ELF executable from memory.  It
*  is passed the address (addr) and the length of the region (len)
*  and returns the address of the segment if successful, NULL
*  otherwise.
*
***************************************************************/

Elf32_Addr
_elfunmap(Elf32_Addr addr, int len) {
  return(elfunmap(addr,len));
}

Elf32_Addr
elfunmap(Elf32_Addr addr, int len) {
  Elf32_Addr map_addr = addr&~(getpagesize()-1);
  Elf32_Word diff = addr - map_addr;

  /* unmap the segment */
  if (munmap((void *)map_addr,len+diff) < 0) {
    perror("elfunmap:");
    addr = (Elf32_Addr) -1;
  }
  return( addr );
}


/***************************************************************
*
*  _elfmap
*  elfmap
*
*  This function takes a pathname (path) and a bit string
*  (opts) as input, verifies that the path name points to
*  an ELF executable or dynamic object, and maps that
*  object into memory.
*
*  Output: This function returns the base address of the file
*  as mapped.  It returns _EM_ERROR otherwise.
*
****************************************************************/

Elf32_Addr
_elfmap(char *path, int infd, int opts) {
  
  return(elfmap(path,infd,opts));
}

Elf32_Addr
elfmap(char *path, int infd, int opts) {

  int numloadseg = 0, i;
  Elf32_Ehdr *pEhdr;
  Elf32_Phdr *pPhdr;
  Elf32_Phdr *pPload=0, *ptr_first_text_page=0, *pPtemp;
  em_environ e;
  void *baseaddr;
  long map_diff;


  if (!infd && !path) {
    error("No file specified.");
    return( _EM_ERROR );
  }

  if (!infd) {
    /* Open file */
    DPRINTF("elfmap: opening file %s, opt = 0x%x\n", path, opts);
    e.state = INITIAL_STATE;

    if ((infd = open(path, O_RDONLY)) == -1) {
      if (!(_EM_SILENT&opts)) perror("elfmap");
      cleanup(&e, 0, 0);
      return(_EM_ERROR);
    }
  } 

  e.state = FILE_OPEN;
  e.infd = infd;
  e.options = opts;

  /* Read and verify Elf Header */
  DPRINTF("elfmap: reading ELF header.\n");
  pEhdr = getEhdr(&e);
  if (e.state != EH_SUCCESS) {
    cleanup(&e, pEhdr, 0);
    return(_EM_ERROR);
  }

  /* Read program header */
  DPRINTF("elfmap: reading program header\n");
  pPhdr = getPhdr(pEhdr, &e);
  if (e.state != PH_SUCCESS) {
    error("Couldn't read Program header in %s.", path);
    cleanup(&e, pEhdr, pPhdr);
    return( _EM_ERROR );
  }

#ifndef OLD_SYSCALL
  if ((pPload = (Elf32_Phdr *)malloc(pEhdr->e_phnum * sizeof(Elf32_Phdr))) == NULL)
    error ("elfmap: cannot malloc for pPload");
  
  for (i = 0; i<pEhdr->e_phnum; i++) {
    if (pPhdr[i].p_type == PT_LOAD) {
	memcpy(&pPload[numloadseg], &pPhdr[i], sizeof(Elf32_Phdr));
	numloadseg++;
    }
  }

  if (numloadseg == 0) {
    error("elfmap: no PT_LOAD segment found in %s.", path);
    cleanup(&e, pEhdr, pPhdr);
    return(_EM_ERROR);
  }
      
  baseaddr = (void *) syssgi(SGI_ELFMAP, e.infd, pPload, numloadseg);
  if ((long)baseaddr < 0) {
    error("elfmap: couldn't map file %s -- error code %d.", path, errno);
    cleanup(&e, pEhdr, pPhdr);
    return(_EM_ERROR);
  } 
  /* success! */
  /* Now we need to find the real ELF header.  This is complicated by the
   * fact that it ain't necessarily at the beginning of the first segment,
   * but rather is at the beginning of the segment which starts at offset
   * 0 in the file.  If no such offset, punt.
   */
  map_diff = (long) baseaddr - (long)pPload->p_vaddr;
  
  pPtemp = pPload;
  for (i = 0; i < numloadseg; i++) {
      if ((ptr_first_text_page == 0) || (pPtemp->p_offset < ptr_first_text_page->p_offset)) {
	 if (pPtemp->p_flags & (PF_X))	/* text segment */
	     ptr_first_text_page = pPtemp;
      }
      pPtemp++;
  }

  baseaddr = (void *)((ptr_first_text_page->p_vaddr) + 
		map_diff - ptr_first_text_page->p_offset);
  free(pPload);
#else
    
  DPRINTF("elfmap: allocating isMapped array\n");
  e.isMapped = (int *) calloc(pEhdr->e_phnum, sizeof(int));
  if (e.isMapped == NULL) {
    cleanup(&e, pEhdr, pPhdr);
    return( _EM_ERROR );
  }
  e.state = ISMAP_ALLOC;

  /* Map the segments */
  DPRINTF("elfmap: mapping segments\n");
  mapSegs(pEhdr, pPhdr, &e);
  if (e.state != MAP_SUCCESS) {
    baseaddr = (void *) _EM_ERROR;
  } else {
    baseaddr = e.retval;
  }
#endif
  cleanup(&e, pEhdr, pPhdr);
  return((Elf32_Addr) baseaddr);
}

/*******************************************************************
 *
 *  getEhdr
 *
 *  This function reads in the ELF header for an ELF file pointed
 *  to by infd.  It will allocate space and verify the size of
 *  the header as well.  All other validation is done by 
 *  validEhdr().  The progress state is EH_SUCCESS if we are
 *  completely successful. Error returns have e->state set to
 *  other values.  This is so we can still return the address
 *  of pEhdr so that it can be freed in one place.
 *
 ******************************************************************/
#ifdef RLD_ELFMAP
extern char *hbuffer;
extern size_t hbuffersize, hbsize;
#else
#define HBUFSIZE 512
static char hbuffer[HBUFSIZE];
static size_t hbuffersize = HBUFSIZE, hbsize = HBUFSIZE;
#endif
static int ehdr_malloced;
static int phdr_malloced;

static Elf32_Ehdr
*getEhdr(em_environ *e) 
{
  Elf32_Ehdr *pEhdr = 0;
  size_t rsize = hbuffersize;
  /* Try to read elfheader and program header in one fell swoop */
  if (read(e->infd, (void *) hbuffer, rsize) == rsize ) {

      pEhdr = (Elf32_Ehdr *) hbuffer;
      ehdr_malloced = 0;
      
  } else {
      /* That didn't work, so try reading ELF header only */
      rsize = sizeof(Elf32_Ehdr);

      /* Allocate space */
      if (rsize > hbuffersize) {
	  pEhdr = (Elf32_Ehdr *) malloc(rsize);
	  hbsize = 0;
	  ehdr_malloced = 1;
      } else {
	  pEhdr = (Elf32_Ehdr *) hbuffer;
	  hbsize = rsize;
	  ehdr_malloced = 0;
      }
	 
      if (pEhdr == NULL) {
	  error("elfmap: out of memory.");
	  DPRINTF("Couldn't allocate pEhdr");
	  return(NULL);
      }
      e->state=EH_ALLOC;

      /* Read header */
      if(read(e->infd, (void *) pEhdr, rsize) != rsize) {
	  error("elfmap: Unexpected EOF");
	  return(pEhdr);
      }
  }

  /* Check size? DEBUGGING ONLY */
  if (rsize < pEhdr->e_ehsize) {
    rsize = pEhdr->e_ehsize;
    DPRINTF("***Size mismatch - Reallocating\n");
    pEhdr = (Elf32_Ehdr *) realloc(pEhdr,rsize);
    if(read(e->infd, (void *) pEhdr, rsize) != rsize) {
      error("elfmap: Unexpected EOF");
      cleanup(e, pEhdr, NULL);
      return(pEhdr);
    }
  }

  /* Validate header */
  
  if (validEhdr(pEhdr)) {
    e->state = EH_SUCCESS;
  }

  /* Check to see if file contains position independent code. (PIC) */  
  e->isPIC = (int)pEhdr->e_flags & EF_MIPS_PIC;

  return(pEhdr);
}



/**************************************************************
 *
 *  validEhdr
 *
 *  This function checks an ELF header verify that this
 *  is an appropriate file for dynamic loading.  It
 *  returns TRUE if the file is valid, FALSE otherwise.
 *
 **************************************************************/

static int
validEhdr(Elf32_Ehdr *pEhdr) 
{
  unsigned char *ident;

  /* Check ELF magic */
  ident = pEhdr->e_ident;
  if ( (ident[EI_MAG0] != 0x7f) || (ident[EI_MAG1] != 'E') 
      || (ident[EI_MAG2] != 'L') || (ident[EI_MAG3] != 'F') ) {
    error("elfmap: not an ELF file.");
    return(FALSE);
  }

  /* Check Endian */
  /* According to the Mips ABI supplement p. 4-1, this
   * is the correct value.  I wonder how they handle
   * little endian.???
   */
  if (ident[EI_DATA] != ELFDATA2MSB) {
    error("elfmap: object is opposite Endian from me.");
    return(FALSE);
  }

  /* Check file class */
  if (ident[EI_CLASS] != ELFCLASS32) {
    error("elfmap: object has invalid class,");
    return(FALSE);
  }
  /* Check machine type */
  if ((pEhdr->e_machine != EM_NONE) && (pEhdr->e_machine != EM_MIPS)) {
    error("elfmap: object is not for this machine.");
    return(FALSE);
  }

  /* Check ofile type */
  if ((pEhdr->e_type != ET_EXEC) && (pEhdr->e_type != ET_DYN)) {
    error("elfmap: object is not an executable or dynamic object.");
    return(FALSE);
  }

  if ((pEhdr->e_flags & EF_MIPS_ABI2) == EF_MIPS_ABI2) {
    error("elfmap: object should NOT be a ABI2 -- new ABI 32bit object.");
    return(FALSE);
  }

  return(TRUE);
}


/*********************************************************************
 *
 *  getPhdr
 *
 *  This function is passed a file descriptor (infd) and an
 *  ELF header.  It allocates space for and reads a program header.
 *  and returns a pointer to it.  It returns NULL if there is
 *  a problem. 
 *
 ********************************************************************/

static Elf32_Phdr 
*getPhdr(Elf32_Ehdr *pE, em_environ *e) 
{
  Elf32_Phdr *pPhdr;
  unsigned size;

  /* allocate space for headers */
  size = pE->e_phentsize * pE->e_phnum;
  if (hbsize >= pE->e_phoff + size) {
      /* We picked it up in hbuffer, find it now */
      pPhdr = (Elf32_Phdr *)(hbuffer + pE->e_phoff);
      phdr_malloced = 0;
  } else {
      /* didn't get it in first read, pick it up individually. */
      pPhdr = (Elf32_Phdr *) calloc((size_t) pE->e_phentsize, pE->e_phnum);
      phdr_malloced = 1;
      if (pPhdr == NULL) {
	  error("elfmap: cant allocate space for program header.");
	  return(NULL);
      }
      e->state = PH_ALLOC;

      /* read in headers */
      if (lseek(e->infd, (off_t)pE->e_phoff, SEEK_SET) 
	  != (off_t)pE->e_phoff) {
	  error("elfmap: lseek failed.");
	  return(NULL);
      }

      if (read(e->infd, (void *)pPhdr, size) != size) {
	  error("elfmap: read of program header failed.");
	  return(NULL);
      }

  }
  e->state = PH_SUCCESS;
  return (pPhdr);
}

#ifdef OLD_SYSCALL
/********************************************************************
 *
 *  mapSegs(infd,pPhdr, pEhdr, pEnv)
 *
 *  This function does the actual mapping of segments into memory.
 *  Inputs are a file descriptor and pointers to the 
 *  Program Header (pPhdr), the Elf Header (pEhdr),
 *  and the state structure (pEnv) which will be used to
 *  keep track of which segments have been mapped.
 *
 *  TODO We will need to alter this 1) to keep track of which
 *  areas of memory are mapped, and 2) to attempt to dynamically
 *  relocate an object, should it's intended target pages be
 *  already occupied.  This will require checking for pic code
 *  in the ELF Header.
 *
 ******************************************************************/

static void
mapSegs(Elf32_Ehdr *pEhdr, Elf32_Phdr *pPhdr, em_environ *e) {

  int phIndex;
  unsigned pgsz;
  unsigned pgmsk;
  mm_vector mmargs[MAX_LOAD_SEGS];
  int cur_load_seg=0;
  void *vaddr;
  off_t offset;
  int len;
  int flags;
  int prot;
  unsigned minaddr = 0x7fffffff;
  unsigned maxaddr = 0;

  pgsz = getpagesize();
  pgmsk = pgsz - 1;

  /* for each segment record */
  for (phIndex=0; phIndex < pEhdr->e_phnum; phIndex++) {
    
    /* Check type if loadable */
    if (pPhdr[phIndex].p_type != PT_LOAD) continue;

    /* Check for overlap */
    /* TODO implement this */

    /* Calculate addresses to the pagesize */
    offset = (off_t) pPhdr[phIndex].p_offset;
    vaddr = (void *) pPhdr[phIndex].p_vaddr;
    if((len = pPhdr[phIndex].p_memsz) < pPhdr[phIndex].p_filesz) {
      error("elfmap: loadable segment memsz is smaller than filesz.");
      return;
    }
    
    /* update extent */
    if ((unsigned)vaddr < minaddr) {
      minaddr = (unsigned) vaddr;
    }
    if ((unsigned)vaddr+pPhdr[phIndex].p_memsz > maxaddr) {
      maxaddr = (unsigned) vaddr+pPhdr[phIndex].p_memsz;
    }

    prot = 0;
    prot |= ((pPhdr[phIndex].p_flags & PF_R) ? PROT_READ : 0);
    prot |= ((pPhdr[phIndex].p_flags & PF_W) ? PROT_WRITE : 0);
    prot |= ((pPhdr[phIndex].p_flags & PF_X) ? PROT_EXECUTE : 0);
    
    /* This is a workaround so we can debug mapped text under
     * 4.0.1  Now commented out
     */
    if ((e->options & _EM_DEBUG_TEXT) && (prot & PROT_EXECUTE)) {
      prot |= PROT_WRITE;
    }

    /* We want to decide where stuff goes, not the OS. */
    flags = MAP_FIXED;

    /* Deciding whether a segment is to be shared or not is 
     * problematic.  For now, what we will do is map anything
     * which is writable as private, and everything else as shared.
     * This is consistent with _EM_DEBUG_TEXT handling.
     */

#ifdef _DELTA_C_PLUS_PLUS
    flags |= MAP_PRIVATE;
#else
    flags |= ((prot & PROT_WRITE) ? MAP_PRIVATE : MAP_SHARED);
#endif
#ifdef OLDVM
    if (len > pPhdr[phIndex].p_filesz) {
      flags |= MAP_AUTOGROW;
      len = (len + TWOMEG) & TWOMEGMASK;
    }
#else
    /* Under the new VM system, we will mmap from /dev/zero to
     * make up the difference. 
     */
/*
    flags |= ((len > pPhdr[phIndex].p_filesz) ? MAP_AUTOGROW : 0);
*/
#endif

    mmargs[cur_load_seg].mm_addr = vaddr;
    mmargs[cur_load_seg].mm_len = len;
    mmargs[cur_load_seg].mm_prot = prot;
    mmargs[cur_load_seg].mm_flags = flags;
    mmargs[cur_load_seg].mm_off = offset;
    mmargs[cur_load_seg].mm_filesz = pPhdr[phIndex].p_filesz;

    if (cur_load_seg++ == MAX_LOAD_SEGS) {
      fatal("elfmap: too many loadable segs");
    }

  }
  /* Map it. */
  if(domap(mmargs, cur_load_seg, minaddr, maxaddr, e)) {
    e->state = MAP_SUCCESS;
  }
}

/*****************************************************************
 *
 *  domap(args, min, max, e)
 *
 *  This routine takes an array of requests for mapping
 *  and maps them into memory.  First, we try to put things
 *  at the location they think they go.  After that, we
 *  put them where the system wants, provided they contain PIC code.
 *  args is an array of requests.
 *  count is the number of requests.
 *  min is the smallest address specified in the ofile.
 *  max is the largest address specified in the ofile.
 *  e is a pointer to the environment struct.
 *
 *****************************************************************/


static int 
domap(mm_vector *args, 
	  int count,
	  unsigned min, 
	  unsigned max, 
	  em_environ *e) {

  void *newaddr;
  int  newoffset;

  /* First, try specified addresses. */
  if(trymap(args, count, 0, e)) {
    e->state = MAP_SUCCESS;
    return(TRUE);
  }

  /* we were unsuccessful in mapping at target address. */
  if (!e->isPIC) {
    /* we're hosed.  Can't map at QS address,
     * and can't be moved.  Bail out.
     */
    error("elfmap: Non-PIC file cant be mapped.");
    return(FALSE);
  }

  /* Original address didn't work.  So lets
   * let the system find a place to put it. 
   */
  {
    int zfd;
    int zflags = MAP_PRIVATE;
    int zprot = PROT_READ;
    
    zfd = open("/dev/zero", O_RDONLY);
    newaddr = mmap((void *)0, max-min, zprot, zflags, zfd, 0);
    close(zfd);
    if ((int)newaddr == -1) {
      error("elfmap: can't map space for object.");
      return(FALSE);
    }
    munmap(newaddr,max-min);
  }

  /* Ok, got the base address, now I need to convert that to
   * an offset and retry.
   */
    
  newoffset = (int)newaddr-min;
  if (trymap(args, count, newoffset, e)) {
    e->state = MAP_SUCCESS;
    return(TRUE);
  }
  
  return(FALSE);
}

/****************************************************************
 *
 * trymap(args, numargs, offset, e)
 *
 * trymap takes an array (args) of size numargs and tries to map
 * the infile into memory at the location determined by adding
 * offset to each virtual address.  Success returns TRUE, failure FALSE.
 *
 ****************************************************************/

static int 
trymap(mm_vector *args, int numargs, int offset, em_environ *e) {
  int i;

  for (i=0; i< numargs; i++) {
    void *curaddr;
    
    curaddr = (void *)((int)args[i].mm_addr + offset);

    /* check to see if something's there. */
    if(addrInUse(curaddr)) break;

    if (mmap(curaddr, args[i].mm_len, args[i].mm_prot, 
	     args[i].mm_flags, e->infd, args[i].mm_off) != curaddr) {
      break;
    }
    
    /* Do we need to zero-fill? */
    if ( args[i].mm_len > args[i].mm_filesz ) {
      void *bss_start = (void *)((int) curaddr + args[i].mm_filesz);
      int bss_len = args[i].mm_len - args[i].mm_filesz;
      bzero(bss_start, bss_len);
    }


    /* Careful examination of rld.c reveals that 
     * it expects elfmap to return a pointer
     * to the ELF Header.  This also makes
     * the most sense, since you can figure out where
     * everything else is from that.
     */

    if (args[i].mm_off == 0) e->retval = curaddr;
  }

  if (i == numargs) {
    /* success! */
    return(TRUE);
  }

  /* Otherwise, unmap any segs that did get mapped. */
  while (i > 0) {
    void *curaddr;
    i--;
    curaddr = (void *)((int)args[i].mm_addr + offset);
    munmap(curaddr, args[i].mm_len);
  }
    
  return(FALSE);
}


/*****************************************************************
 *
 * addrInUse(addr)
 *
 * This routine checks to see if the address specified
 * is currently mapped into the user's address space.
 *
 *****************************************************************/

static int
addrInUse(void *addr) {
  unsigned len;

  len = getpagesize();

  if (mpin(addr, len) == -1) {
    return(FALSE);
  } else {
    munpin(addr, len);
    return(TRUE);
  }
}
#endif /* OLD_SYSCALL */

/*****************************************************************
 *
 * cleanup(e, pE, pP)
 *
 * This routine cleans up stuff, unmaps segments, etc.
 * It is called for both a normal and an exceptional exit.
 *
 *****************************************************************/

static void
cleanup(em_environ *e, Elf32_Ehdr *pE, Elf32_Phdr *pP) 
{

  int i;

  switch(e->state) {
    /******************************************************
     * 
     *  WARNING!   WARNING!   WARNING!
     *
     *  This is a fall-through switch!!!
     *  The further we got through mapping, the more there
     *  is to clean up, and the higher we jump into this
     *  switch.  There are intentionally no breaks in this
     *  switch, except for after case INITIAL_STATE.
     *  
     *******************************************************/

  case SEG_MAPPED: 
    /* Given the future semantics of mmap -- namely
     * that mmapping on top of something clobbers it --
     * it seems excessive to unmap stuff here. But
     * I'll do it anyway. jlg
     */
    for (i = 0; i < pE->e_phnum; i++) {
      if (e->isMapped[i]) {
	elfunmap((Elf32_Addr) pP[i].p_vaddr, (int) pP[i].p_memsz);
      }
    }

  case MAP_SUCCESS:
  case ISMAP_ALLOC:
    free(e->isMapped);

  case PH_SUCCESS:
  case PH_ALLOC:
    if (phdr_malloced) free(pP);

  case EH_SUCCESS:
  case EH_ALLOC:
    if (ehdr_malloced) free(pE);

  case FILE_OPEN:
    close(e->infd);

  case INITIAL_STATE:
    /* nothing to do */
    break;

  default:
    printf("Bogus state in elfmap:cleanup: %d\n",e->state);
    exit(10);
  }
}
  
  
