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


#pragma ident "@(#) libu/util/segrstuf.c	92.1	07/07/99 13:18:33"


#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/fs/prfcntl.h>

/* --- Global symbolic constants --- */
#define ERROR	-1		/* system call error condition */
#define OK	-2		/* general okay condition */
#define OFF	 0		/* flag disabled state */
#define ON	 1		/* flag enabled state */

/* --- /$SEGRES/ header information structure --- */
struct seghdr {
   long len;			/* length of common block */

   unsigned dbg:      1; 	/* debug mode flag */
   unsigned sfcopy:   1;	/* scratch file copy mode flag */
   unsigned segsplit: 1;	/* XMP/YMP dual segment structure */
   unsigned mrcopy:   1;	/* memory resident copy mode flag */
   unsigned :        54;	/* unused */
   unsigned vers:     6;	/* $SEGRES version id; */

   long xfer;			/* user main entrance address */
   long preset;			/* bss area preset value */

   unsigned nslt:    32;	/* number of SLT entries */
   unsigned slt:     32;	/* base address of SLT entries */

   unsigned nsdt:    32;	/* number of SDT entries */
   unsigned sdt:     32;	/* base address of SDT entries */

   unsigned njtbl:   32;	/* number of JTBL entries */
   unsigned jtbl:    32;	/* base address of JTBL entries */

};
 
/* --- Segment Linkage Table (SLT) structure --- */
struct slt {
   unsigned sdtp:    32;	/* SDT entry pointer */
   unsigned addr:    32;        /* target routine entrance address */
};

/* --- Segment Description Table (SDT) structure --- */
struct sdt {
   char name[8];		/* segment name */

   unsigned res:      1;	/* segment in memory flag */
   unsigned save:     1;	/* save segment to file flag */
   unsigned :        30;	/* unused */
   unsigned level:   16;	/* tree level of segment */
   unsigned acount:  16;	/* count of active calls to segment */

   unsigned succp:   32;	/* SDT ptr to current successor seg */
   unsigned predp:   32;	/* SDT pointer to predecessor segment */

   unsigned tlen:    32;	/* segment text section length */
   unsigned tla:     32;	/* segment text area base address */
 
   unsigned dlen:    32;	/* segment data section length */
   unsigned dla:     32;	/* segment data area base address */

   unsigned blen:    32;	/* segment bss section length */
   unsigned zlen:    32;	/* segment zeroset section length*/

   long unused;			/* unused */
   long tpos;			/* text section file position */
};

typedef long *MEMPTR;		/* segment section memory pointer */
typedef struct sdt *SDTPTR;	/* SDT entry pointer */
typedef struct slt *SLTPTR;	/* SLT entry pointer */

/* --- Common shared data --- */
static struct seghdr *hdrptr;	/* pointer to seghdr */
static SLTPTR saveslt;		/* pointer to current slt entry */
static long segrsum;		/* /$SEGRES/ checksum value */
static char *sname;		/* pointer to scratch file name */
static int xfile;		/* executable file descriptor */
static int sfile;		/* scratch file descriptor */
static char parcel[] = "abcd";	/* parcel address suffixes */

/* --- Error messages --- */
#define   ERRSTAT   0
#define   FIRSTERR  1
#define   BADVERS   1
#define   BADSUM    2
#define   NOPATH    3
#define   OVERWRITE 4
#define   SYSFIRST  5
#define   OPENXF    5
#define   OPENSF    6
#define   POSXF     7
#define   POSSF     8
#define   READXF    9
#define   READSF    10
#define   WRITESF   11
#define   MEMRES    12
#define   LASTERR   12
 
static char *errmsg[] = { 
/* ERRSTAT*/	"segres: illegal $SEGRES status\n",
/* BADVERS*/	"segres: incompatible versions of SEGLDR and $SEGRES\n",
/* BADSUM*/	"segres: /$SEGRES/ destroyed\n",
/* NOPATH*/	"segres: can't find PATH variable\n",
/* OVERWRITE*/	"segres: segment          overwrites segment         \n",
/* OPENXF*/	"segres: can't open executable file\n",
/* OPENSF*/	"segres: can't open scratch file\n",
/* POSXF*/	"segres: error positioning executable file\n",
/* POSSF*/	"segres: error positioning scratch file\n",
/* READXF*/	"segres: error reading executable file\n",
/* READSF*/	"segres: error reading scratch file\n",
/* WRITESF*/	"segres: error writing scratch file\n",
/* MEMRES*/	"segres: error allocating memory for memory resident copy\n",
};
#define OVSEG1 16	/* char offset for first OVERWRITE seg name */
#define OVSEG2 44       /* char offset for second OVERWRITE seg name */

/*----------------------------------------------------------------------
*  checksum:   Compute /$SEGRES/ value
*
*  Procedure description:
*     Get /$SEGRES/ length
*     Clear checksum value
*     DO for each word in common block
*        Add word to checksum value
*     ENDDO
*     RETURN
*
*  Inputs:
*     none
*
*  Global data:
*     hdrptr = pointer to $SEGRES header
*
*  Outputs:
*     Function value = checksum value
*
*---------------------------------------------------------------------*/

static long checksum ()
{

   long cvalue;			/* running checksum value*/
   long length;			/* $SEGRES block length */
   MEMPTR segptr;		/* pointer to $SEGRES block */

   cvalue = 0;
   segptr = (MEMPTR)hdrptr;
   for (length = hdrptr->len ; length > 0 ; length--) {
      cvalue = cvalue ^ *segptr++;
   }
   return (cvalue);

}

/*----------------------------------------------------------------------
*  _seginit:  $SEGRES initialization routine
*
*  Called from $SEGRES during program startup.
* 
*  Procedure description:
*    Check /$SEGRES/ data to insure match between SEGLDR and $SEGRES.
*    Open the executable file.
*    IF memory resident copy mode is specified THEN
*       Allocate memory in data space for the copy.
*       Copy the program segments from the executable file to memory.
*    ELSEIF scratch file copy mode is specified THEN
*       Create the scratch file.
*       Copy the program segments to the scratch file.
*    ENDIF
*    IF preset value not 0 THEN
*       Fill root segment bss area with preset value
*    ENDIF
*    IF debug mode THEN
*       Compute /$SEGRES/ initial checksum value.
*    ENDIF
*    RETURN
* 
*  Inputs:
*    hptr = pointer to seghdr (/$SEGRES/ common block header)
*
*  Outputs:
*    hdrptr = static global variable assigned the value of hptr for 
*             use on subsequent segment loading steps.
*
*---------------------------------------------------------------------*/

_seginit (hptr)
struct seghdr *hptr;
{
   SDTPTR sdtptr;		/* SDT root segment pointer */
   int status;			/* local status value */
   char* mrcopy_env_val;

   hdrptr = hptr;
   status = OK;

   /* 
    * mrcopy can be set on or off, either by directive at link time
    * or by * environment variable at run time.
    */
   mrcopy_env_val = getenv("MRCOPY");
   if (mrcopy_env_val != NULL) {
      if (strcmp(mrcopy_env_val, "ON") == 0 ||
          strcmp(mrcopy_env_val, "on") == 0) {
         hptr->mrcopy = ON;
      }
      else if (strcmp(mrcopy_env_val, "OFF") == 0 ||
               strcmp(mrcopy_env_val, "off") == 0) {
         hptr->mrcopy = OFF;
      }
      else {
         fprintf(stderr, 
           "segres: MRCOPY has an invalid value of '%s'.\n", mrcopy_env_val);
      }
   }

   if ( hptr->len>>16 > 0 || hptr->vers < 1 || hptr->vers > 2) {
      status = BADVERS;
   }
   if (status == OK) {
      status = openxqt();
      if (status == OK) {
         if (hptr->mrcopy == ON) {
            status = mrcopysegs();
         }
         else if (hptr->sfcopy == ON) {
            status = openscr();
            if (status == OK) {
               status = sfcopysegs();
            }
         }
         if (status == OK) {
            if (hptr->preset != 0) {
               sdtptr = (SDTPTR)hptr->sdt;
               fillmem((MEMPTR)(sdtptr->dla+sdtptr->dlen), sdtptr->blen,
                    hptr->preset);
            }
            if (status == OK) {
               if (hptr->dbg == ON) {
                  /* I don't know how to make the checksum meaningful since
                   * hptr will change between calls to checksum. */
                  segrsum = checksum();
               }
            }
         }
      }
   }

   if (status != OK) {
      doerror(status);
   }
}

/*----------------------------------------------------------------------
*  openxqt:   Open the executable file
*  
*  Procedure description:
*     IF kernel level is 6.0 or greater THEN
*        Get own process id.
*        Open process via /proc.
*        Open executable file via ioctl.
*     ENDIF
*     IF /proc open not done or unsuccessful THEN
*        Get argv(0) value.
*        IF executable file name starts with "/", "./", "../" THEN
*           Open file name as given.
*        ELSE
*           Get PATH environment variable.
*           DO for each name in PATH
*              Concatenate PATH name and file name.
*              IF file exists, has correct permissions, and is regular file
*              THEN
*                 Open file.
*                 BREAK loop.
*              ENDIF
*           ENDDO.
*        ENDIF
*     ENDIF
*     RETURN
*
*  Outputs:
*     Function value = open status
*     xfile = executable file descriptor
*
*---------------------------------------------------------------------*/

extern int ZQRLSLVL;		/* kernel release level */
static openxqt ()
{
   extern char **_argv;		/* pointer to cmd line args */

   /* --- /proc data --- */
   char procbuff[24];		/* /proc name buffer */
   int procfd;			/* process file descriptor */
   long lxfile;			/* file descriptor of executable file */
   extern int getpid();		/* get process id */

   /* --- path data --- */
   char *arg0;                  /* pointer to command name */
   char *path;                  /* pointer to PATH name */
   char *xname;			/* pointer to executable file name */
   char *savex;                 /* xname save pointer */
   struct stat sbuf;            /* stat structure */
   static char xfilname[64];    /* executable file name build buffer */
   extern char *getenv();	/* get environment var. routine (lib) */


   /* --- if 6.0 kernel and after, try /proc open --- */

   xfile = ERROR;
   if (ZQRLSLVL >= 6000) {
      sprintf (procbuff, "/proc/%d", getpid());
      procfd = open (procbuff, O_RDONLY);
      if (procfd >= 0) {
         if (ioctl (procfd, PFCOPENT, &lxfile) >= 0)
            xfile = (int)lxfile;
         close (procfd);
      }
   }

   /* --- if /proc open not done/unsuccesful, try path variable method --- */

   if (xfile == ERROR) {
      arg0 = _argv[0];
      if (arg0[0] == '/' ||
          arg0[0] == '.' && arg0[1] == '/' ||
          arg0[0] == '.' && arg0[1] == '.' && arg0[2] == '/') {
         xname = arg0;
         xfile = open (xname, O_RDONLY);
      }
      else {
         if ((path = getenv ("PATH")) == NULL)
            return (NOPATH);
         else {
            while (*path != '\0') {
               xname = xfilname;
               savex = xname;
               arg0 = _argv[0];
               while (*path != ':' && *path != '\0')
                  *xname++ = *path++;
               if (xname != savex)
                  *xname++ = '/';
               while ((*xname++ = *arg0++) != '\0')
                  ; 
               xname = savex;
               if ( access (xname, 5) == 0) {
                  stat (xname, &sbuf);
                  if ((sbuf.st_mode & S_IFMT) == S_IFREG) {
                    xfile = open (xname, O_RDONLY);
                    break;
                  }
               }   
               if (*path == ':')
                  path++;
            }
         }
      } 
   }
   if (xfile == ERROR)
      return (OPENXF);
   else
      return (OK);
}

/*----------------------------------------------------------------------
*  openscr:   Create the scratch file
*  
*  Procedure description:
*     Create temporary file name.
*     Open the file.
*     Unlink the file (in case of errors).
*     RETURN
*
*  Inputs:
*     none
*
*  Outputs:
*     Function value = creation status
*     sfile = scratch file descriptor
*     sname = pointer to scratch file name
*
*---------------------------------------------------------------------*/

static openscr ()
{
   extern char *tempnam();	/* temporary name creation (lib) */

   if ((sname = tempnam(NULL, "SEG.")) != NULL) {
      if ((sfile = open(sname, O_RDWR|O_CREAT|O_RAW, 0600)) != ERROR) {
         if ((unlink(sname)) != ERROR)
            return (OK);
      }
   }
   return (OPENSF);
}

/*----------------------------------------------------------------------
*  sfcopysegs:   Move program segments from exec file to scratch file
*  
*  Procedure description:
*     Get copy buffer address after end of root segment.
*     DO WHILE for each non-root segment
*        Position executable file to first non-root segment.
*        Read in text/data sections from executable file.
*        Get current position in scratch file and save as text position.
*        Write out text section.
*        Get current position in scratch file and save as data position.
*        IF segment save mode THEN
*           Set output length to data + bss + zeroset section lengths.
*           Fill bss and zeroset sections.
*        ELSE
*           Set output length to data section length.
*        ENDIF
*        Write out data area.
*        Get next SDT entry.
*     ENDDO
*     RETURN
*
*  Inputs:
*    none
*
*  Global data:
*    hdrptr = pointer to $SEGRES header
*    sfile = file descriptor of scratch file
*    xfile = file descriptor of executable file
*
*  Outputs:
*    Function value = copy status
*
*---------------------------------------------------------------------*/

static sfcopysegs ()
{
   SDTPTR sdtptr;		/* SDT entry pointer */
   long length;			/* Segment length value */
   long nsdt;			/* number of SDT entries */

   sdtptr = (SDTPTR)hdrptr->sdt;
   for (nsdt = hdrptr->nsdt - 1 ; nsdt > 0 ; nsdt--) {
      sdtptr++;
      if ( lseek (xfile, (long)sdtptr->tpos, 0) == ERROR) 
         return(POSXF);
      
      if (read(xfile,(char *)sdtptr->tla,sizeof(long)*(sdtptr->tlen)) !=
	  (sizeof(long)*(sdtptr->tlen))) {
	return (READXF);
      }
      if (read(xfile,(char *)sdtptr->dla,sizeof(long)*(sdtptr->dlen)) !=
	  (sizeof(long)*(sdtptr->dlen))) {
	return (READXF);
      }

      sdtptr->tpos = lseek (sfile, (long)0, 1);

      if (sdtptr->save == ON) {
         fillmem( (MEMPTR)(sdtptr->dla + sdtptr->dlen),
                 sdtptr->blen, hdrptr->preset );
         fillmem( (MEMPTR)( sdtptr->dla + sdtptr->dlen +
                  sdtptr->blen), sdtptr->zlen, 0);
         length = sizeof(long) * (sdtptr->dlen + sdtptr->blen + sdtptr->zlen);
      }
      else
         length = sizeof(long) * (sdtptr->dlen);

      if (write(sfile,(char *)sdtptr->tla,(sizeof(long)*(sdtptr->tlen))) != 
	  (sizeof(long)*(sdtptr->tlen))) {
	return (WRITESF);
      }
      if ( write (sfile, (char*)sdtptr->dla, length) != length)
         return (WRITESF);
   }
   return (OK);

}

/*----------------------------------------------------------------------
*  mrcopysegs:   Move program segments from exec file to memory resident
*                (data space) copy.  This is useful primarily for YMP/J90 
*                architectures where code space is limited but data space
*                is considerably larger.
*  
*  Procedure description:
*     Determine the amount of memory needed for all non-root segments.
*     malloc space for the memory resident copy of the non-root segments.
*     DO WHILE for each non-root segment
*        Position executable file to first non-root segment.
*        Read in text/data sections from executable file into the mem res copy.
*        Save the mem res text address.
*        Save the mem res data address.
*        Set the mem res address for the next segment to 
*                              base + data + bss + zeroset section lengths.
*        Fill bss and zeroset sections.
*        Get next SDT entry.
*     ENDDO
*     RETURN
*
*  Inputs:
*    none
*
*  Global data:
*    hdrptr = pointer to $SEGRES header
*    xfile = file descriptor of executable file
*
*  Outputs:
*    Function value = copy status
*
*---------------------------------------------------------------------*/

static mrcopysegs ()
{
   SDTPTR sdtptr;		/* SDT entry pointer */
   long length;			/* Segment length value */
   long nsdt;			/* number of SDT entries */
   long *memres;                /* pointer into memory res copy of segments */

   sdtptr = (SDTPTR)hdrptr->sdt;
   length = 0;

   for (nsdt = hdrptr->nsdt - 1 ; nsdt > 0 ; nsdt--) {
      sdtptr++;
      if (sdtptr->save == ON) {
         length = length + sdtptr->tlen + sdtptr->dlen + sdtptr->blen +
                  sdtptr->zlen;
      }
      else {
         length = length + sdtptr->tlen + sdtptr->dlen;
      }
   }

   memres = (long*)(malloc(sizeof(long) * length));
   if (memres == NULL) {
      return (MEMRES);
   }

   sdtptr = (SDTPTR)hdrptr->sdt;
   for (nsdt = hdrptr->nsdt - 1 ; nsdt > 0 ; nsdt--) {
      sdtptr++;
      if ( lseek (xfile, (long)sdtptr->tpos, 0) == ERROR) 
         return(POSXF);
      
      sdtptr->tpos = (long)memres;
      length = sizeof(long)*(sdtptr->tlen);
      if (read(xfile,(char *)memres,length) != length) {
	return (READXF);
      }

      memres += sdtptr->tlen;
      length = sizeof(long)*(sdtptr->dlen);
      if (read(xfile,(char *)memres,length) != length) {
	return (READXF);
      }

      memres += sdtptr->dlen;
      if (sdtptr->save == ON) {
         fillmem(memres, sdtptr->blen, hdrptr->preset );
         memres += sdtptr->blen;
         fillmem(memres, sdtptr->zlen, 0);
         memres += sdtptr->zlen;
      }

   }
   return (OK);

}

/*----------------------------------------------------------------------
*  fillmem:   Flush an area of memory with a value
*
*  Procedure description:
*     Put value in every word
*
*  Inputs:
*     fillptr = pointer to area to fill
*     fillwrds = number of words to fill
*     fillval = value to fill
*
*---------------------------------------------------------------------*/

static fillmem (fillptr, fillwrds, fillval)

MEMPTR fillptr;	
long fillwrds;
long fillval;

{
   while (fillwrds-- > 0)
      *fillptr++ = fillval;

}

/*----------------------------------------------------------------------
*  _segchkr:   Segment residency checker
* 
*  Called from $SEGCALL each time there is a function call into a segment.
*
*  Procedure description:
*    IF debug mode on and /$SEGRES/ checksum not okay THEN
*       Print message and quit.
*    ENDIF
*    Get SDT pointer.
*    IF destination segment not in memory THEN
*       CALL loadseg to load it.
*    ENDIF
*    Get target routine entrance address
*    RETURN
*
*  Inputs:
*     sltptr = pointer to SLT entry for this interception
*
*  Global data:
*     hdrptr = pointer to $SEGRES header (set previously by _seginit)
*
*  Outputs:
*     function value = target routine entrance address
*
*---------------------------------------------------------------------*/

_segchkr (sltptr)
SLTPTR sltptr;
{

   register SDTPTR sdtptr;	/* pointer to SDT entries */
   register int status;		/* local status value */

   /* saveslt is not multi-tasking safe but it is only used for error 
    * processing so does it matter? */
   saveslt = sltptr;
   if (hdrptr->dbg == ON && checksum() != segrsum) {
      status = BADSUM;
   }
   else {
      sdtptr = (SDTPTR)sltptr->sdtp;
#pragma _CRI guard
      /* The incrementing and decrementing of the activity count must be
       * protected when tasking.  Use the same semaphore here as in 
       * segres.s, that is, use GRDLK = SM05. */
      sdtptr->acount++;
#pragma _CRI endguard
      if (sdtptr->res == ON)
         status = OK;
      else {
         status = loadseg (sdtptr);
      }
   }
   if (status != OK)
      doerror (status);
   return (sltptr->addr);
}

/*----------------------------------------------------------------------
*  loadseg:   Segment save/load routine.
* 
*  Procedure description:
*     Find nearest predecessor of target segment that's in memory.
*     DO for each successor of resident segment.
*        Mark segment as non-resident.
*        IF segment reference count is not zero THEN
*           (NOTE: reference count is decremented in $SEGCALL)
*           RETURN overwritten segment status
*        ENDIF
*        IF segment save flag on THEN
*           CALL writeseg to write segment out to scratch file or to
*           memory resident copy.
*        ENDIF
*     ENDDO
*     DO for target segment and each non-resident predecessor
*        CALL readseg to read segment into memory.
*        Mark segment as resident.
*        Link successor pointer to previous segment.
*     ENDDO
*     CALL checksum to compute new /$SEGRES/ checksum.
*     RETURN
*
*  Inputs:
*     loadsdt = pointer to sdt entry for segment to load
*
*  Global data:
*     hdrptr = pointer to /$SEGRES/ header block
*
*  Outputs:
*     function value = load operations status
*
*---------------------------------------------------------------------*/

static loadseg (loadsdt)
SDTPTR loadsdt;
{

   SDTPTR sdtptr;		/* SDT pointer */
   SDTPTR succp;		/* SDT successor pointer */
   int scnt;			/* segment name count */
   int status;			/* local status value */
   char *mptr;			/* char ptr to OVERWRITE message */
   char *sptr;                  /* char ptr to segment names */

   /* Move up the chain to the lowest resident segment. */

   for ( sdtptr = (SDTPTR)loadsdt->predp ; sdtptr->res == OFF ;
      sdtptr = (SDTPTR)sdtptr->predp )
   {
      ;
   }

   /* Move back down the chain to mark segments as no longer resident
    * and optionally save what will be over written. */

   for ( sdtptr = (SDTPTR)sdtptr->succp ; sdtptr != 0 ;
      sdtptr = (SDTPTR)sdtptr->succp )
   {
      sdtptr->res = OFF;
      if (sdtptr->acount != 0) {
         mptr = errmsg[OVERWRITE] + OVSEG1;
         sptr = loadsdt->name;
         for (scnt = 0 ; scnt < 8 && *sptr != '0' ; scnt++)
             *mptr++ = *sptr++;
         mptr = errmsg[OVERWRITE] + OVSEG2;
         sptr = sdtptr->name;
         for (scnt = 0 ; scnt < 8 && *sptr != '0' ; scnt++)
             *mptr++ = *sptr++;
         return (OVERWRITE);
      }
      if (sdtptr->save == ON) {
         if ((status = writeseg(sdtptr)) != OK)
            return (status);
      }
   }

   /* Move back up the chain to read in the new segments. */

   succp = 0;
   for ( sdtptr = loadsdt ; sdtptr->res == OFF ; sdtptr =
      (SDTPTR)sdtptr->predp )
   {
      if ((status = readseg(sdtptr)) != OK)
         return (status);
      sdtptr->res = ON;
      sdtptr->succp = (int)succp;
      succp = sdtptr;
   }
   sdtptr->succp = (int)succp;
   if (hdrptr->dbg == ON) {
      segrsum = checksum();
   }
   return (OK);
}

/*----------------------------------------------------------------------
* 
*  readseg:   Load a segment from either the executable, the scratch file,
*             or the memory resident copy.
* 
*  Procedure description:
*    IF mrcopy mode THEN
*       Copy text into text area.
*       Copy data into data area.
*    ELSE IF sfcopy mode THEN
*       Position scratch file to segment text section beginning.
*       IF split segment mode THEN
*          Read text section from scratch file into text area.
*          IF save mode THEN
*             Read data/bss/zeroset sections from scratch file into
*               data area.
*          ELSE
*             Read data section from scratch file into data area.
*             Fill bss and zeroset sections in data area.
*          ENDIF
*       ELSE
*          IF save mode THEN
*             Read text/data/bss/zeroset sections from file into
*                memory.
*          ELSE
*             Read text/data sections from scratch file into memory.
*             Fill bss and zeroset sections.
*          ENDIF
*       ENDIF
*    ELSE
*       Position executable file to segment text section beginning.
*       IF split segment mode THEN
*          Read text section from executable file into memory.
*          Read data section from executable file into memory.
*       ELSE
*          Read text/data sections from executable file into memory.
*       ENDIF
*       Fill bss and zeroset sections.
*    ENDIF
*    RETURN
*
*  Inputs:
*     sdtptr = pointer to SDT entry for segment to load.
*
*  Global data:
*     hdrptr = pointer to $SEGRES header.
*     sfile = file descriptor for scratch file
*     xfile = file descriptor for executable file
*
*  Outputs:
*     function value = segment read status
*
*---------------------------------------------------------------------*/

static readseg (sdtptr)
SDTPTR sdtptr;
{

   long length;			/* segment length */
   int status;			/* local status value */
   long *memres;                /* pointer into memory res copy of segments */

   status = OK;

   /* The mrcopy option takes precedence over the sfcopy option.  If the
    * save option was specified, that forces at least sfcopy. */

   if (hdrptr->mrcopy == ON) {
      if (sdtptr->save == ON) {
         memres = (long*)sdtptr->tpos;
         length = sizeof(long)*(sdtptr->tlen);
         memcpy((char*)sdtptr->tla, (char*)memres, length);
         memres += sdtptr->tlen;
         length = sizeof(long)*(sdtptr->dlen + sdtptr->blen + sdtptr->zlen);
         memcpy((char*)sdtptr->dla, (char*)memres, length);
      }
      else {
         memres = (long*)sdtptr->tpos;
         length = sizeof(long)*(sdtptr->tlen);
         memcpy((char*)sdtptr->tla, (char*)memres, length);
         memres += sdtptr->tlen;
         length = sizeof(long)*(sdtptr->dlen);
         memcpy((char*)sdtptr->dla, (char*)memres, length);
         fillmem((MEMPTR)(sdtptr->dla+sdtptr->dlen),
                         sdtptr->blen, hdrptr->preset);
         fillmem((MEMPTR)(sdtptr->dla+sdtptr->dlen+
                         sdtptr->blen), sdtptr->zlen, 0);
      }
   }
   else if (hdrptr->sfcopy == ON) {
      if ( lseek (sfile, sdtptr->tpos, 0) == ERROR)
          status = POSSF;
      else {
         if (hdrptr->segsplit == ON) {
            length = sizeof(long) * sdtptr->tlen;
            if ( read (sfile, (char*)sdtptr->tla, length) != length)
               status = READSF;
            else {
               if (sdtptr->save == ON) {
                  length = sizeof(long) * (sdtptr->dlen + sdtptr->blen +
                                sdtptr->zlen);
                  if (read(sfile, (char*)sdtptr->dla, length) != length)
                     status = READSF;
               }
               else {
                  length = sizeof(long) * sdtptr->dlen;
                  if (read(sfile, (char*)sdtptr->dla, length) != length)
                     status = READSF;
                  else {
                     fillmem( (MEMPTR)(sdtptr->dla+sdtptr->dlen),
                         sdtptr->blen, hdrptr->preset);
                     fillmem( (MEMPTR)(sdtptr->dla+sdtptr->dlen+
                         sdtptr->blen), sdtptr->zlen, 0);
                  }
               }
            }
         }
         else {
            if (sdtptr->save == ON) {
               length = sizeof(long) * (sdtptr->tlen + sdtptr->dlen +
                             sdtptr->blen + sdtptr->zlen);
               if ( read (sfile, (char*)sdtptr->tla, length) != length)
                  status = READSF;
            }
            else {
               length = sizeof(long) * (sdtptr->tlen + sdtptr->dlen);
               if ( read (sfile, (char*)sdtptr->tla, length) != length)
                  status = READSF; 
               else { 
                  fillmem( (MEMPTR)(sdtptr->dla+sdtptr->dlen), 
                      sdtptr->blen, hdrptr->preset); 
                  fillmem( (MEMPTR)(sdtptr->dla+sdtptr->dlen+ 
                      sdtptr->blen), sdtptr->zlen, 0); 
               }
            }
         }
      }
   }
   else {
      if ( lseek (xfile, sdtptr->tpos, 0) == ERROR)
         status = POSXF;
      else {
         if (hdrptr->segsplit == ON) { 
            length = sizeof(long) * sdtptr->tlen;
            if ( read (xfile, (char*)sdtptr->tla, length) != length)
               status = READXF;
            else {
               length = sizeof(long) * sdtptr->dlen;
               if ( read(xfile, (char*)sdtptr->dla, length) != length)
                  status = READXF;
            }
         }
         else {
            length = sizeof(long) * (sdtptr->tlen + sdtptr->dlen);
            if ( read(xfile, (char*)sdtptr->tla, length) != length)
               status = READXF;
         }
         if (status == OK) {
            length = sdtptr->dla + sdtptr->dlen;
            fillmem( (MEMPTR)length, sdtptr->blen, hdrptr->preset);
            length = length + sdtptr->blen;
            fillmem( (MEMPTR)length, sdtptr->zlen, 0);
         }
      }
   }
   return (status);
}

/*----------------------------------------------------------------------
* 
*  writeseg:   Write a segment out to the scratch file or to the memory 
*              resident copy.  This is called only if 'save' is on.
* 
*  Procedure description:
*     IF mrcopy THEN
*        Copy text section to data space.
*        Copy data/bss/zeroset sections to data space.
*     ELSE
*        Position scratch file to segment text section.
*        IF split segment mode THEN
*           Write out text section to scratch file.
*           Write out data/bss/zeroset sections to scratch file.
*        ELSE
*           Write out text/data/bss/zeroset sections.
*        ENDIF
*     ENDIF
*     RETURN
*
*  Inputs:
*     sdtptr = pointer to SDT entry for segment to write out
*
*  Global data:
*     hdrptr = pointer to $SEGRES header
*     sfile = file descriptor for scratch file
*
*  Outputs:
*     function value = segment write status
*
*---------------------------------------------------------------------*/

static writeseg (sdtptr)
SDTPTR sdtptr;
{

   long length;			/* segment length to write out */
   int status;			/* local status value */
   long *memres;                /* pointer into memory res copy of segments */

   status = OK;
   if (hdrptr->mrcopy == ON) {
      memres = (long*)sdtptr->tpos;
      length = sizeof(long)*(sdtptr->tlen);
      memcpy((char*)memres, (char*)sdtptr->tla, length);
      memres += sdtptr->tlen;
      length = sizeof(long)*(sdtptr->dlen + sdtptr->blen + sdtptr->zlen);
      memcpy((char*)memres, (char*)sdtptr->dla, length);
   }

   else if ( lseek (sfile, sdtptr->tpos, 0) == ERROR) 
      status = POSSF;
   else {
      if (hdrptr->segsplit == ON) {
         length = sizeof(long) * sdtptr->tlen;
         if ( write (sfile, (char*)sdtptr->tla, length) != length)
            status = WRITESF;
         else {
            length = sizeof(long) * (sdtptr->dlen + sdtptr->blen + sdtptr->zlen);
            if ( write (sfile, (char*)sdtptr->dla, length) != length)
               status = WRITESF;
         }
      }
      else {
         length = sizeof(long) * (sdtptr->tlen + sdtptr->dlen + sdtptr->blen +
                       sdtptr->zlen);
         if (length != 0) {
            if ( write (sfile, (char*)sdtptr->tla, length) != length)
               status = WRITESF;
         }
      }
   }
   return (status);

}

/*----------------------------------------------------------------------
*  doerror:  $SEGRES error message processor
*
*  Procedure description:
*  IF operating system related error THEN
*     CALL perror to system error message
*  ENDIF
*  Print out $SEGRES message
*
*  Inputs:
*     status = $SEGRES status value
*
*  Global data:
*     errmsg = error message table
*
*---------------------------------------------------------------------*/

static doerror (status)
int status;
{

   int cnt;			/* character counter */	
   SDTPTR sdtptr;		/* SDT pointer */

   if (status < FIRSTERR || status > LASTERR)
      status = ERRSTAT;
   if (status >= SYSFIRST)
      perror ("segres");
   fprintf (stderr, errmsg[status] );
   if (status == OVERWRITE) {
      fprintf (stderr, "segres: routine at address %o%c in overwriting segment would be called\n",
          saveslt->addr>>2, parcel[saveslt->addr&&3]);
      fprintf (stderr, "segres: active segment list - ");
      for ( sdtptr = (SDTPTR)hdrptr->sdt ; sdtptr != 0 ;
            sdtptr = (SDTPTR)sdtptr->succp )
      {
         for (cnt = 0; cnt < 8; cnt++) {
            if (sdtptr->name[cnt] == '\0' || sdtptr->name[cnt] == ' ')
               break;
            fputc (sdtptr->name[cnt], stderr);
         }
         if (sdtptr->succp != 0)
            fputs (", ", stderr);
      }
      fputc ('\n', stderr);
   }
   if (status == OVERWRITE || status == BADSUM)
      $TRBK ();
   exit (1);

}

/*----------------------------------------------------------------------
*  SG00RCLR:  $SEGRES segment residence clearing routine.  Called by
*             the 'ldovl' overly routine to insure reloading segments.
*
*   Procedure description:
*      Locate named segment in SDT.
*      Clear resident bit in SDT entry.
*      DO for each sucessor segment
*         Clear resident bit in SDT entry.
*      ENDDO
*      RETURN
*
*   Inputs:
*      segname = pointer to ljzf segment name
*
*---------------------------------------------------------------------*/

SG00RCLR (segname)
long *segname;
{

   char *nptr;			/* character to segname */
   int nsdt;			/* number of SDT entries */
   SDTPTR sdtptr;		/* SDT pointer */

   nptr = (char*)segname;
   sdtptr = (SDTPTR)hdrptr->sdt;
   for (nsdt = hdrptr->nsdt - 1 ; nsdt > 0 ; nsdt--) {
      sdtptr++;
      if (strncmp (sdtptr->name, nptr, 8) == 0) {
         sdtptr->res = OFF;
         for ( sdtptr = (SDTPTR)sdtptr->succp ; sdtptr != 0 ;
               sdtptr = (SDTPTR)sdtptr->succp)
            sdtptr->res = OFF;
         return (0);
      }
   }
   return (ERROR);
}

/*----------------------------------------------------------------------
* _seg_owclear:  user callable routine to clear segment use count.  This
*                routine must be called by the user if setjmp/longjmp is 
*                used in the program and longjmp causes a return to setjmp
*                across segment boundaries.  Otherwise, _segchkr will detect
*                a segment overwrite violation.
*
* Procedure description:
*      Look for resident segment that contains input address.
*      IF not found THEN
*         Return error.
*      ELSE
*         DO for each successor segment
*            Clear access count.
*         ENDDO
*      ENDIF
*---------------------------------------------------------------------*/
seg_owclear (funcaddr)
long funcaddr;
{
   int nsdt;			/* number of SDT entries */
   SDTPTR sdtptr;		/* SDT pointer */

   sdtptr = (SDTPTR)hdrptr->sdt;
   funcaddr = funcaddr >> 2;	/* convert from parcel to word */
   for (nsdt = hdrptr->nsdt-1; nsdt > 0 ; nsdt--) {
       if (sdtptr->res == ON &&
           funcaddr > sdtptr->tla && funcaddr < (sdtptr->tla + sdtptr->tlen))
       {
          sdtptr = (SDTPTR)sdtptr->succp;
          while (sdtptr != 0) {
             sdtptr->acount = 0;
             sdtptr = (SDTPTR)sdtptr->succp;
          }
          return 0;
       }
       sdtptr++;
   }
   return 1;
}
