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


static char USMID[] = "@(#) libu/ffio/c1/sdsopen.c	92.0	10/08/98 14:57:41";

#include <fcntl.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <malloc.h>
#include <errno.h>
#include <ffio.h>
#include <string.h>
#include "../fssio.h"
#include "../fxlist.h"

DECLARE(SDS_XLIST);
struct xtr_s SDS_XLIST_P = { SDS_XLIST };
#define MAX(a,b) ((a) > (b) ? (a) : (b))

/*
 * SDS open routine.
 * Open a file that is destined to be resident on SDS.
 * There are 3 options for disk access and permanence
 *	TR_FSS_SAVE - load and save to disk on every open/close
 *	TR_FSS_SCR - Don't touch the disk. and deallocate on close
 *	TR_FSS_LCL - A mix of save and scratch.  The data is kept in the
 *			SDS when the file is closed, and re-used when
 *			re-opned.  However, the data is not flushed to
 *			disk when the file is closed.  This allows files
 *			to be repeatedly opened and closed without the
 *			cost of the SAVE option, and without data loss.
 *			(not implemented)
 */

extern union spec_u *_fss_cpy_spc();

int
_sds_open(
const char	*name,
int		flags,
int		mode,
struct fdinfo	*fio,
union spec_u	*spec,
struct ffsw	*stat,
long		cbits,
int		cblks,
struct gl_o_inf *oinf)
{
	char *ptr;
	int nextfio;
	int bs, ret;
	long mininc;
	struct fdinfo *nfioptr;
	union spec_u *nspec;
	struct sds_f *sds_info;
	struct ffsw clstat;
	struct stat statbuf;

/*
 *	Allocate private storage
 */
	sds_info = (struct sds_f *) calloc(sizeof(struct sds_f),1);
	if (sds_info == NULL) goto nomem;
	fio->lyr_info = (char *)sds_info;
	sds_info->bdfd = -1;	/* also used as a flag */
	sds_info->name = strdup(name);
	if (sds_info->name == NULL) goto badret;

/*
 *	Internally, both blksize and recsize are in bits!
 */
	bs = 4096 * 8;		/* one sector buffer */
	fio->_ffbufsiz = bs; /* bit size of buffer, 1 sector */
	ptr = malloc((bs >> 3)+16);
	if (ptr == NULL) goto nomem;
	SET_BPTR(fio->_base, CPTR2BP(ptr));
	fio->rwflag = POSITIN;
	sds_info->sdsdirty = 0;
	sds_info->overflowed = NO;
	sds_info->ovoff = 0;
	fio->segbits = 0;
	fio->_cnt = 0;
	fio->_ptr = fio->_base;
/*
 *	Open the lower layers
 */
	nspec = spec;
	NEXT_SPEC(nspec);


	if (fio->rtype != TR_FSS_SCR || fio->subtype != FSS_OPT_NOVFL) {
		int write_only = 0;
		int llflags = flags;
/*
 *		The lower level file must be readable to allow loading into
 *		memory.
 */	 
		if ((llflags & O_ACCMODE) == O_WRONLY) {
			write_only = 1;
			llflags &= ~O_ACCMODE;
			llflags |= O_RDWR;
		}
		nextfio = _ffopen(name, llflags, mode, nspec, stat, cbits,
			cblks, NULL, oinf);
/*
 *		If write_only and file has no read permissions, then it
 *		may be opened O_WRONLY.  But it better be empty, because if not
 *		_sds_load will get an error later.
 */
		if (nextfio < 0 && write_only) {
			llflags = flags;
			nextfio = _ffopen(name, llflags, mode, nspec, stat,
				cbits, cblks, NULL, oinf);
		}
		if (nextfio < 0) goto badret;
		ret = XRCALL((struct fdinfo *)nextfio, fcntlrtn) 
			(struct fdinfo *)nextfio, FC_STAT, &statbuf, stat);
		if (ret < 0) goto close_badret;

		sds_info->dsk_blksize = statbuf.st_blksize;
	}
	else {
		nextfio = 0;
		sds_info->sdseof = 0;
	}

	nfioptr = (struct fdinfo *)nextfio;
	fio->fioptr = nfioptr;  /* must do this early for _sds_load() */

/*
 *	Set up min, max allocation in SDS and min increment
 */

	sds_info->sdsklik = MAX(SDSMINKLIK, sds_info->dsk_blksize/SDSBLKBY);
	sds_info->minsize = ROUNDD60(spec->fss.min, sds_info);
	sds_info->maxsize = ROUNDD60((spec+1)->info.quan, sds_info);
	if (sds_info->maxsize == 0)
		sds_info->maxsize = ROUNDD60(0x0fffffffffff, sds_info); /* whole bunch */
	mininc = (spec+2)->info.quan;
	if (mininc == 0) mininc = SDSMININC;
	mininc = ROUNDD60(mininc, sds_info);
	sds_info->mininc = mininc;
	if (sds_info->minsize == 0) {
		if (mininc > sds_info->maxsize) mininc = sds_info->maxsize;
		sds_info->minsize = mininc;
	}
	if (fio->rtype == 0)
		fio->rtype = TR_FSS_SAVE;	/* default */
	if (fio->rtype == TR_FSS_LCL) {
		ERETURN(stat, FDC_ERR_INTERR, 0);	/* disable local */
#if FSS_LOCAL
/*
 *		If local, don't bother to continue down the chain
 *		Just save away some info in case we need it later.
 *		See if the file has a previous life..
 *              This code is currently inactivated, and really
 *              needs to be reviewed before it is activated.
 */
		nextfio = 0;

		extern struct local_s *_fss_fndloc();

		locs = _fss_fndloc(name);
		file_exists = 1;
		if (locs != NULL) { /* found it */
			if (file_exists) {
				sds_info->sdsalo = locs->sdsalo;
				sds_info->sdsalnum = locs->alnum;
				sds_info->sdssize = locs->size;
				sds_info->sdseof = locs->eof;
				free(locs);
			}
			else {
				/* Do we really want to do the sds_freeall? */
				sds_freeall(sds_info, stat); 
				sds_info->sdseof = 0;
				/* llopen it */
			}
		}
		else { /* did not find it  */
			sds_info->sdseof = 0;
			/* llopen it to create file */
		}
#endif
	}
	else if (fio->rtype == TR_FSS_SCR) {
/*
 *		If the O_CREAT flag is not set, fail.  By definition, the
 *		file does not exist.
 */
		if ((flags & O_CREAT) != O_CREAT) {
			_sds_clfree(fio);
			ERETURN(stat, ENOENT, 0);
		}
/*
 *		This unlinks the underlying file and tells underlying layers
 *		that this is a scratch file. 
 */
		ret = XRCALL(fio,fcntlrtn) fio, FC_SCRATCH,
			&sds_info->scrtch_flgs, stat);
		if (ret == ERR) goto close_badret;
	}
	else if (fio->rtype == TR_FSS_SAVE) {
		ptr = malloc(BLKS2BYTES(SECBSZ));
		SET_BPTR(sds_info->locbuf, CPTR2BP(ptr));
		if (BPTR2CP(sds_info->locbuf) == NULL) goto nomem;
/*
 *		Load the file into the SDS buffer if its size is nonzero.
 */
		ret = _sds_load(fio, sds_info,
			name, flags, mode, cbits, cblks, &statbuf, stat);
		if (ret == ERR) goto close_badret;
	}

	DUMP_IOB(fio); /* debugging only */
	return(nextfio);

nomem:
	ERETURN(stat, FDC_ERR_NOMEM, 0);

close_badret:
	if (nfioptr != NULL)
		(void) XRCALL(nfioptr,closertn) nfioptr, &clstat);
badret:
	_sds_clfree(fio);
	return(ERR);
}

/*
 *	Load up the data from the file
 */

#define BDOPSTR "%s: BD open of %s, flags=0x%x, mode=%x, cbits=%x, returns 0x%x\n"

static
_sds_load(
struct fdinfo	*fio,
struct sds_f	*sds_info,
char		*name,
int		flags,
int		mode,
int		cbits,
int		cblks,
struct stat	*statbuf,
struct ffsw	*ffstat)
{
	struct ffsw locstat;
	struct ffc_info_s ffci;
	int i;
	int ret;
	int ovret;
	int xfer;
	int incr;
	int nb;
	int got;
	int ubc;
	int done;
	int cursize;
	int evnblks;
	int bdfd;
	int loadsize;	/* in bits */
	int blksize;	/* in bytes */
	int regular_file;
	struct sds_alo_s *alop;
	struct fdinfo *llfio;

	llfio = fio->fioptr;
	locstat.sw_error = 0;
	XRCALL(llfio, fcntlrtn) llfio, FC_GETINFO, &ffci, ffstat);
/*
 *	See if the underlying file has the appropriate characteristics to use
 *	the back-door to the SSD.  It must be a regular file, and the SYSTEM
 *	layer must be the next lower level layer.
 */
	regular_file = S_ISREG(statbuf->st_mode) &&	/* regular file and */
			CAN_OVERFLOW(ffci);		/* pure stream */
	done = L_ISMORE;
/*
 *	If we can, use the backdoor..
 */
	blksize = sds_info->dsk_blksize;
	sds_info->sds_gran = 1;
/* 
 *	If the file is zero-length, then don't bother with any of this
 *	loading stuff. Note that loadsize is only valid for regular files.
 */
	loadsize = statbuf->st_size << 3;	/* in bits */
/*
 *	Calculate even number of blocks to load
 *	If it is a regular file, has a blocksize that is a power
 *	of two, Grow the file to its required size.
 */
	evnblks = 0;
	nb = 0;
	if (regular_file && _popcnt(blksize) == 1) {
/*
 *		Set up the granularity even if there is nothing to load
 */
		sds_info->sds_gran = blksize/SDSBLKBY;
/*
 *		No need to check SDS size, we *know* this is the first try.
 */
		ovret = 0;
		if (loadsize > 0) {
			ovret = _sds_grow(fio, loadsize, ffstat);
			if (ovret < 0) return(ERR);
			evnblks = loadsize >> 3;	/* back to bytes */
			if (sds_info->sdssize < loadsize)
				evnblks = sds_info->sdssize >> 3;
			evnblks = evnblks & ~(blksize - 1);
		}
/*
 *		Get the back door file descriptor now.  We might need it
 *		when flushing.
 *		If the back door open fails, then do the regular way.
 *		Note that the file *must* always exist at this
 *		point
 *
 *		The lower level file must be readable to allow loading into
 *		memory.
 */	 

		bdfd = -1;
		if (llfio->class == CLASS_SYSCALL) {
			int write_only = 0;
			int llflags = flags;

			if ((llflags & O_ACCMODE) == O_WRONLY) {
				write_only = 1;
				llflags &= ~O_ACCMODE;
				llflags |= O_RDWR;
			}
	
			bdfd = open(name, llflags | O_SSD, mode, cbits, 0);
			FSSTRACE(_xrt_putf(BDOPSTR, "sdsload", name, llflags,
				mode, cbits, bdfd));

			/*
			 * Retry with write-only permissions.
			 */
			if (bdfd == -1 && write_only) {
				llflags = flags;
				bdfd = open(name, llflags | O_SSD, mode, cbits,
					cblks);
				FSSTRACE(_xrt_putf(BDOPSTR, "sdsload", name,
					llflags, mode, cbits, bdfd));
			}
		}
/*
 *		Even if the open() call failed, the returned (-1)
 *		value is still appropriate to store
 */
		sds_info->bdfd = bdfd;	/* Save it away.. */
/*
 *		If worthwhile, use the back-door to read it up.
 *		Due to overflow complications, ONLY do back door if
 *		SYSCALL is our lower level layer.
 */
		if (bdfd >= 0 && evnblks != 0) {
			i = 0;
			alop = sds_info->sdsalo;
			while (nb < evnblks) {
				xfer = BLKS2BYTES(alop[i].size);
				if (xfer > (evnblks - nb))
					xfer = evnblks - nb;
/*
 *				Note that if the I/O fails, we assume that it
 *				due to something benign, and just continue.
 *				This is most likely due to the SDS allocations
 *				not being well formed.  i.e. on a DD-60,
 *				an SDS allocation of 3 sectors would cause an
 *				error here.
 */
				BDREAD(ret,bdfd,BLKS2WORDS(alop[i].base),xfer);
				if (ret < 0)
					break;	/* assume benign error */
				i++;
				nb += ret;	/* total up bytes read */
			}
			sds_info->sdseof = nb << 3;
			ret = XRCALL(fio, seekrtn) fio, nb, 0, &locstat);
			if (ret < 0) goto load_error;
			ret = XRCALL(llfio, seekrtn) llfio, nb, 0, &locstat);
			if (ret < 0) goto load_error;
			if (sds_info->sdseof == loadsize)
				done = L_GOTALL;
/*
 *			If we overflowed, and the back door operation filled
 *			all of the allocated SDS space, there is no point
 *			in continuing.
 *			Note that 'ovret' is from the _sds_grow() call above.
 *			This check is only here for efficiency.  It is
 *			not necessary.
 */
			if (ovret > 0 && sds_info->sdseof == sds_info->sdssize)
				done = L_OVFLWD;

		}
	}

/*
 *	Can't use backdoor, or not even multiple of well-formed
 *	requests.  Loop doing reads and copy to SDS.
 */
	while(done == L_ISMORE) {
		ubc = 0;
/* 
 *		nb will be = min(loc buffer size, max_sds_size - cursize)
 */
		nb = BLKS2BYTES(SECBSZ);
		if ((sds_info->sdseof + (nb<<3)) >
		    BLKS2BITS(sds_info->maxsize)) {
			nb = (BLKS2BITS(sds_info->maxsize) -
					sds_info->sdseof) >> 3;
			done = L_OVFLWD;
		}
/*
 *		Read up some data
 */
		got = XRCALL(llfio, readrtn) llfio,
			sds_info->locbuf, nb, &locstat, PARTIAL, &ubc);
		if(locstat.sw_stat == FFEOD) done = L_GOTALL;
		if (got < 0) goto load_error;
/*
 *		Grow the SDS allocation *before* writing to SDS.  There is
 *		no point in overflowing right back to disk!
 */
		cursize = sds_info->sdssize;
		incr = got << 3;
		if (cursize < (sds_info->sdseof + incr)) {
/*
 *			Grow by enough to accomodate request, no more.
 */
			incr = (sds_info->sdseof + incr) - cursize;
			ret = _sds_grow(fio, incr, &locstat);
			if (ret < 0)
				goto load_error;
			else if (ret > 0) {	/* overflowed 'cuz grow failed*/
/*
 *				available room is new SDS size minus
 *				current EOF
 */
				got = (sds_info->sdssize - sds_info->sdseof) >> 3;
				done = L_OVFLWD; /* Quit after this write. */
			}
		}
/*
 *		Copy it to SDS
 */
		if (got > 0) {
			ret = XRCALL(fio, writertn) fio,
				sds_info->locbuf, got, ffstat, PARTIAL, &ubc);
			if (ret < 0) return(ERR);
		}
	}
/*
 *	See if there was more data that would not fit.  If so, then
 *	check if overflow is legal, if so, set the EOF to what it
 *	*really* is.  Overflow appropriately.

 *
 *	Note that the stat fields are reliable only for regular files.
 *	If not a regular file, take a chance on one more read.  If it so
 *	happens to be an EOD or EOF/EOD, then all is OK.  Anything else is
 *	an error.  Overflow to a non-regular, non-stream file is forbidden.
 */
	if (regular_file) {
		if (sds_info->sdseof < loadsize) {
			ret = _fss_overflow(fio, &locstat);
			if (ret < 0) goto load_error;
			sds_info->sdseof = statbuf->st_size << 3;
		}
	}
	else {
		got = XRCALL(llfio, readrtn) llfio,
			sds_info->locbuf, nb, &locstat, PARTIAL, &ubc);
		if (got < 0) goto load_error;
		if (got > 0) ERETURN(ffstat, FDC_ERR_BADOVF, 0);
		if(locstat.sw_stat == FFEOF) {
			got = XRCALL(llfio, readrtn) llfio,
				sds_info->locbuf, nb, &locstat, PARTIAL, &ubc);
			if (got < 0) goto load_error;
			if (got > 0) ERETURN(ffstat, FDC_ERR_BADOVF, 0);
		}
		if(locstat.sw_stat != FFEOD)
			ERETURN(ffstat, FDC_ERR_NOSUP, 0);
		/* if return is EOF/EOD or EOD, then exact size is OK. */
	}
	ret = XRCALL(fio, seekrtn) fio, 0, 0, ffstat);
/*
 *	SDS is not really dirty at this point, but since we used the write
 *	routine to put the data in SDS, it thinks it is.  Simply
 *	Clear the flag.
 */
	sds_info->sdsdirty = NO;
	return(ret);

load_error:
	ERETURN(ffstat, locstat.sw_error, 0);
}

/*
 *	Set up spec list for emulation of "assign -a SDS -n NNN -s u file"
 *	This routine allows the removal of an include across directories
 *	in libio.  It returns a pointer to a spec_u obtained from malloc().
 *	It is the responsibility of the caller to free it.
 */

union spec_u *
_A_SDS_(spec, size, room)
union spec_u *spec;
int size;
int room;
{
	if (room < 4) return ((union spec_u *)NULL);	/* need 4 spec words */

	spec[0].fld.ext		= 1;
	spec[0].fld.class	= CLASS_SDS;
	spec[0].fld.recfmt	= TR_FSS_SCR;
	spec[0].fld.subtype	= FSS_OPT_NOVFL;
	spec[0].fss.min		= size;		/* init alloc */

	spec[1].info.ext	= 1;
	spec[1].info.class	= CLASS_SDS;
	spec[1].info.quan	= size;		/* max alloc */

	spec[2].info.ext	= 0;
	spec[2].info.class	= CLASS_SDS;
	spec[2].info.quan	= 0;		/* increment */

	spec[3].wword		= 0;
	return(spec);
}

#if FSS_LOCAL

static struct local_s *_sds_loclst = NULL;

/*
 * save a local file structure on the save list
 *	This does not work right with overflow...
 */
int
_fss_savloc(name, base, size, eof, stat)
char *name;
long base, size, eof;
struct ffsw *stat;
{
	struct local_s *ptr;

	ptr = (struct local_s *)malloc(sizeof(struct local_s) + strlen(name)+1);
	if (ptr == NULL) ERETURN(stat, FDC_ERR_NOMEM, 0);
	ptr->link = _sds_loclst;
	_sds_loclst = ptr;
	strcpy(ptr->name, name);
	ptr->base = base;
	ptr->size = size;
	ptr->eof = eof;
	return(0);
}

/*
 * find a local file on the linked list
 */
struct local_s *
_fss_fndloc(name)
char *name;
{
	struct local_s *ptr, **lastptr;

	ptr = _sds_loclst;
	lastptr = &_sds_loclst;
	while (ptr != NULL) {
		if (strcmp(ptr->name, name) == 0) {
			*lastptr = ptr->link;	/* unlink it from chain */
			return(ptr);
		}
		ptr = ptr->link;
		lastptr = &ptr->link;
	}
	return(NULL);
}
#endif
