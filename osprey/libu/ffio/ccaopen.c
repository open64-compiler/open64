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


#pragma ident "@(#) libu/ffio/ccaopen.c	92.5	10/29/99 21:40:31"


#include <errno.h>
#include <fcntl.h>
#include <ffio.h>
#include <malloc.h>
#include <stdio.h>
#include <stddef.h>
#include <stdlib.h>
#include <cray/assign.h>
#include <cray/mtlock.h>
#include <cray/nassert.h>
#include <sys/stat.h>
#include <sys/types.h>
#ifdef	_UNICOS_MAX
#include <mpp/globals.h>
#endif
#include "ccaio.h"
#include "cchio.h"
#include "fxlist.h"
#include "fflock.h"

DECLARE(CACHEA_XLIST);
struct xtr_s CACHEA_XLIST_P = { CACHEA_XLIST };

struct shared_cache_tab _CCA_scache[NUM_SHARED_CACHE];
plock_t _CCA_scache_lock[2];

FILE *_GL_cca_logptr;

/*
 * 	Open routine for "cachea" layer.
 *
 *	Returns: pointer to fdinfo block from next lower layer on success.
 *		 -1 if an error occurred.
 */

_ffopen_t
_cca_open(
	const char	*name,
	int		oflags,
	mode_t		mode,
	struct fdinfo	*fio,
	union spec_u	*spec,
	struct ffsw	*stat,
	long		cbits,
	int		cblks,
	struct gl_o_inf *oinf)		/* global open information for this file */
{
	register short	create_cache_buffers;
	register short	create_shared_info;
	register short	free_buf = FALSE; /* set to FALSE because of shared cache */
	register short	locked = 0;
	register short	o_direct = 0;	
	register int	alloc_bytes;
	register int	blks_per_pg;
	register int	blks_per_sect = 0;
	register int	cache_number;
	register int	cca_flags;
	register int	errn = 0;	/* Error code */
	register int	extra;
	register int	fdfio = -1;
	register int	file_number;
	register int	i;
	register int	nb;		/* number of buffers */
	register int	num_advance_pages;
	register int	orig_oflags;
	register int	ret;
	register int	sect_per_pg;
	register int	sectmsk_siz;
	int		isvalid;
	register int64	bs;		/* size of each buffer in bits */
	char		*s;
	char		*storage;
	bitptr		bptr;
	_ffopen_t	nextfio;
	struct fdinfo	*nfioptr;
	union spec_u	*nspec;
	struct cca_f	*cca_info;
	struct cca_buf	*cbufs;
	struct stat	fstat;
	struct ffsw	clstat;
	struct ffc_info_s layer_info;

#ifdef	__mips
	struct dioattr	dio;
#endif

	if ( _GL_cca_logptr == NULL ) 
	  _GL_cca_logptr = stderr;

	orig_oflags	= oflags;

	fio->lyr_info	= NULL;	/* set to NULL for the benefit of _cca_clfree */

/*
 *	Decide whether a shared cache will be used.   
 */
	cache_number	= _ff_nparm_getv(spec, 4, &isvalid);

	if ( cache_number != 0 ) { /* A shared cache is to be used */
	    if (cache_number < 0 || cache_number >= NUM_SHARED_CACHE) {
		errn	= FDC_ERR_INTERR;
		goto err_ret;
	    }
	    SCACHE_LOCK(_CCA_scache_lock);
	    locked	= 1;
	    if (MULTI_ON) {
		int nextcache;
		nspec	= spec;
		NEXT_SPEC(nspec);
		while (!_ffclass_sys_interface(nspec->fld.class)) {
			if (nspec->fld.class == CLASS_CACHEA) {
#if	defined(__mips) || defined(_LITTLE_ENDIAN)
				/* On Irix systems, locking is not sufficient */
				/* to support shared stacked caches. Detect */
				/* this an issue an error */
				if (_ff_nparm_getv(nspec, 4, &isvalid) != 0) {
					errn	= FDC_ERR_NOSTCA;
					goto err_unlock;			
				}
#else
				/* On Unicos systems, we can allow shared */
				/* stacked caches, but their order must not */
				/* be mixed between files, otherwise it could */
				/* lead to deadlock. Detect this and issue */
				/* an error. E.g., the following is an error: */
				/* assign -F cachea::::4,cachea::::2  */
				/* assign -F cachea::::2,cachea::::4  */
				if ((nextcache = _ff_nparm_getv(nspec, 4, &isvalid)) != 0) {
					if ((nextcache == cache_number) ||
					    (_check_scache(nextcache, cache_number) !=
					     0)) {
						errn	= FDC_ERR_BADSHC;
						goto err_unlock;			
					}
				}
#endif
			}
			NEXT_SPEC(nspec);
		}
	    }
 	    /*
	     * Check if the shared cache needs to be allocated.
	     */
	    if ( _CCA_scache[cache_number].file_count == 0 ) {
		void	*v;

		v	= calloc(MAX_FILES_PER_SHARED_CACHE + 1,
				sizeof(struct cca_f *));

		if (v == NULL) {
		    errn	= FDC_ERR_NOMEM;
		    goto err_unlock;
		}

		_CCA_scache[cache_number].cca_info	= v;

		v	= calloc( 1, sizeof(struct cca_f) );

		if (v == NULL) {
		    free(_CCA_scache[cache_number].cca_info);
		    errn	= FDC_ERR_NOMEM;
		    goto err_unlock;
		}

		_CCA_scache[cache_number].cca_info[0]	= v;

		file_number		= 1;
		_CCA_scache[cache_number].file_count	= 1;
		create_cache_buffers	= TRUE;
		create_shared_info	= TRUE;
	    }
	    else {	/* the shared cache is already started */

		/* search for empty cca_info slot in _CCA_scache */
		file_number	= -1;

		for (i = 1; i < MAX_FILES_PER_SHARED_CACHE; i++) {
		   if ( _CCA_scache[cache_number].cca_info[i] == NULL ) {
		      file_number	= i;
		      _CCA_scache[cache_number].file_count++;
		      break;
		   }

		}
		if ( file_number == -1 ) {
		   fprintf(_GL_cca_logptr,
"Max files limit(%d) exceeded for shared cache %d.  Will use private cache\n",
		           MAX_FILES_PER_SHARED_CACHE,cache_number);
		   cache_number	= 0;
	   	   SCACHE_UNLOCK(_CCA_scache_lock);
		   locked	= 0;
		}
		create_cache_buffers	= FALSE;
		create_shared_info  	= FALSE;
	   }
	}

	if ( cache_number == 0 ) { /* A private cache is to be used */
	     create_cache_buffers	= TRUE;
	     create_shared_info		= FALSE;
	     file_number		= 1;
	}

	cca_info	= (struct cca_f *)calloc(1, sizeof(struct cca_f) );

	if (cca_info == NULL) {
	   errn	= FDC_ERR_NOMEM;
	   goto err_unlock_free;
	}

	fio->lyr_info		= cca_info;
	cca_info->thisfio	= fio;

	cca_info->shared_cache	= cache_number;
	cca_info->file_number	= file_number;

	oflags		= oflags | O_RAW;	/* We believe that bypassing
						   system cache enhances
						   performance in most cases. */

	cca_flags	= 0;

/*
 *	Get values from the FFIO spec.
 */

	if ( create_cache_buffers ) {
	   bs	= _ff_nparm_getv(spec, 1, &isvalid) * BITPBLOCK;
#ifdef	_UNICOS_MAX
	   if (_MPP_MPPSIM > 0) {
		/* Running on simulator in user virtual mode. Simulator can */
		/* not handle large reads/writes, so restrict the size */
		if (bs == 0)
		   bs	= CCA_DEF_SIMBUFSIZ * BITPBLOCK;
	   }
	   else
#endif
	   if (bs == 0) bs	= CCA_DEF_BUFSIZ * BITPBLOCK;

	   if (bs < 0 || bs >= CCA_MAX_BBUFSIZ) {
	   	   errn	= FDC_ERR_BUFSIZ;
		   goto err_unlock_free;
	   }
   
	   nb	= _ff_nparm_getv(spec, 2, &isvalid);
	   if (!isvalid)
		nb	= CCA_DEF_NBUF;	/* set default number of buffers */

	   if (nb <= 0) {
		   errn	= FDC_ERR_NBUF0;
		   goto err_unlock_free;
	   }
	}

	num_advance_pages	= _ff_nparm_getv(spec, 3, &isvalid);

	cca_info->nbufs		= nb;
	cca_info->bufs		= NULL;
	cca_info->max_lead	= num_advance_pages;

	cca_info->bypass	= FALSE;
#ifdef	_CCA_DIAGS
	cca_info->read_hit	= 0;
	cca_info->read_miss	= 0;
	cca_info->write_hit	= 0;
	cca_info->write_miss	= 0;
#endif
	cca_info->adv_start_count	= 0;
	cca_info->adv_used_count	= 0;
	cca_info->max_count		= 0;
	cca_info->chain_start_count	= 0;
	cca_info->read_hide_time	= 0;

	(void) strcpy( cca_info->file_name, name );

	cca_info->cache_pages_not_preread	= 0;
	cca_info->cache_pages_synced_for_read	= 0;
	cca_info->cache_pages_synced_for_ill_formed_write = 0;
	cca_info->partial_pages_written		= 0;
	cca_info->unsynced_pages_read		= 0;

	cca_info->exempts_issued	= 0;
	cca_info->exempts_failed	= 0;
	cca_info->unknown_recalls	= 0;
	cca_info->optflags.unused	= FALSE; /* initially, no special options*/
	cca_info->optflags.sds		= FALSE;
	cca_info->optflags.dump		= FALSE;
	cca_info->optflags.diags	= FALSE;
	cca_info->optflags.full_diags	= FALSE;
	cca_info->optflags.pw		= TRUE;
	cca_info->optflags.prw		= FALSE;
	cca_info->optflags.wb		= TRUE;
	cca_info->optflags.hldwb	= FALSE;
	cca_info->optflags.scr		= FALSE;
	cca_info->optflags.bpons	= FALSE;
	cca_info->optflags.do_listio	= FALSE;
	cca_info->optflags.do_sylistio	= FALSE;

#ifdef	__mips
	if (oflags & O_DIRECT) {
		o_direct	= 1;
	}
#endif
/*
 *	Set flag if -m on is assigned.
 *	Set flag if -w on is assigned. We only care about this for
 *	cachea.ssd files.
 */
	if (oinf->aip != NULL) {
		if (oinf->aip->m_multup_flg && oinf->aip->m_multup) {
			cca_info->is_multup	= 1;
			oinf->aip->m_multup_flg	= oinf->aip->m_multup_flg |
							ATTR_USED;
		}
		if (oinf->aip->w_welfrm_flg && oinf->aip->w_welfrm) {
			cca_info->is_welfrm	= 1;
		}
#ifdef	__mips
		if (oinf->aip->B_direct_flg )
			if (oinf->aip->B_direct)
				o_direct	= 1;
			else	
				o_direct	= 0;
#endif
	}

	if ( oflags & O_RDWR ) {
	   cca_info->optflags.no_read	= FALSE;
	   cca_info->optflags.no_write	= FALSE;
	}
	else if ( oflags & O_WRONLY ) {
	   cca_info->optflags.no_read	= TRUE;
	   cca_info->optflags.no_write	= FALSE;
	   oflags			= oflags & ~O_WRONLY;
	   oflags			= oflags | O_RDWR;
	}
	else {	/* implied O_RDONLY */
	   cca_info->optflags.no_read	= FALSE;
	   cca_info->optflags.no_write	= TRUE;
	}

	cca_info->bytes_read_from_cca	= 0;
	cca_info->bytes_written_to_cca	= 0;
	cca_info->bytes_read_by_cca	= 0;
	cca_info->bytes_written_by_cca	= 0;

	cca_info->output_units	= cca_flags & (CCA_flags_mult | CCA_flags_units);

	if ( (CCA_flags_DIAGS_PART  & cca_flags) == CCA_flags_DIAGS_PART ) {
	    cca_info->optflags.diags	= TRUE;
	}
	if ( (CCA_flags_DIAGS_FULL  & cca_flags) == CCA_flags_DIAGS_FULL ) {
	    cca_info->optflags.diags	= TRUE;
	    cca_info->optflags.full_diags	= TRUE;
	}

#ifdef	SDS_SUPPORTED
	if (spec->fld.recfmt == TR_CCH_SDS)
		cca_info->optflags.sds	= TRUE;
#endif

	if ( cca_flags & CCA_flags_HOLD_WRITE_BEHIND )
	    cca_info->optflags.hldwb	= TRUE;

	if ( cca_flags & CCA_flags_WRITE_BEHIND )
	    cca_info->optflags.wb	= TRUE;

	if ( cca_flags & CCA_flags_PARTIAL_WRITES )
	    cca_info->optflags.pw	= TRUE;

	if ( cca_flags & CCA_flags_SCRATCH )
	    cca_info->optflags.scr	= TRUE;


	if ( ( cca_flags & CCA_flags_PREREAD_WRITES  ) ||
	     (cca_info->optflags.pw == FALSE) )
	   cca_info->optflags.prw	= TRUE;

	if ( oflags & O_RDONLY )	/* if read only, no write behind */
		cca_info->optflags.wb	= FALSE;

	if ( oflags & O_WRONLY )	/* if write only, no read ahead */
		cca_info->max_lead	= 0;

#if	defined(_CRAY1) || defined(_CRAYMPP)
	if ( create_cache_buffers ) {
	   if (cca_info->optflags.sds)
	   	   oflags	= oflags | O_SSD; /* request I/O betw disk and SDS */
	}
	else {
	   if (_CCA_scache[cache_number].cca_info[0]->optflags.sds)
		   oflags	= oflags | O_SSD; /* request I/O betw disk and SDS */
	}
#endif

/*
 *	Get the FFIO spec for the next lower layer.
 */
	nspec	= spec;
	NEXT_SPEC(nspec);

/*
 *	Open the layers below this one.
 */
	if ( cca_info->bypass ) {
	   oflags	= orig_oflags;
	   _cca_clfree(fio, fio->fioptr, TRUE);
	   nextfio	= _ffopen(name, oflags, mode, nspec, stat, cbits, cblks,
				fio, oinf);
	   return(nextfio);
	}
	else {
	   nextfio	= _ffopen(name, oflags, mode, nspec, stat, cbits, 
				cblks, NULL, oinf );
	    if (nextfio == _FFOPEN_ERR) {
		/*
		 * Retry with the oflags originally passed.
		 */
		oflags	= orig_oflags;
	    	nextfio	= _ffopen(name, oflags, mode, nspec, stat, cbits, 
			          cblks, NULL, oinf );
	    }
	}

	if (nextfio == _FFOPEN_ERR) {
		errn	= stat->sw_error;
		goto err_unlock_free;
	}

	nfioptr	= (struct fdinfo *)nextfio;

#ifdef	__mips
	if (o_direct) {
		register int64	bsbyt;

		bsbyt = bs / 8;

		/* determine buffer size requirements for O_DIRECT */

		ret	= XRCALL(nfioptr,fcntlrtn) nfioptr, FC_DIOINFO, &dio, stat);
		if (ret == ERR) {
			errn	= stat->sw_error;
			goto close_badret;
		}
		/* Adjust the size of the buffers for O_DIRECT's requirements */
		if (bsbyt % dio.d_miniosz != 0) {
			/* we need to write in these units. */
			bsbyt	= bsbyt - bsbyt%dio.d_miniosz;
		}
		if (bsbyt < dio.d_miniosz) {
			bsbyt	= dio.d_miniosz;
		}
		else if (bsbyt > dio.d_maxiosz) {
			bsbyt	= dio.d_maxiosz;
		}
		if (bsbyt % dio.d_mem != 0) {
			errn	= FDC_ERR_BUFSIZ;
			goto err_unlock_free;
		}
		bs	= bsbyt * 8;
		/* We must write in units of d_miniosz */
		blks_per_sect	= dio.d_miniosz/(NUM_BITS_PER_BLOCK/8);
	}
#endif
	if ( create_cache_buffers ) {
/*
 *	Set the number of blocks per sector to 1.   This number determines
 *	the smallest granularity of a page which can be dirty without forcing
 *	a page pre-read.   We should track this value separately from the
 *	true sector size in the future.   
 */

	   if (blks_per_sect == 0)
	   	blks_per_sect	= 1;  /* _get_sector_size( name ); */
	   if ( cache_number != 0 && blks_per_sect < 4  && !o_direct)
		blks_per_sect	= 4;

	   blks_per_pg		= bs / NUM_BITS_PER_BLOCK;
	   cca_info->bits_per_sect	= blks_per_sect * NUM_BITS_PER_BLOCK;

	   extra		= blks_per_pg % blks_per_sect;

	   if ( extra != 0 ) {
	      blks_per_pg	= blks_per_pg + (blks_per_sect - extra);
	      bs		= blks_per_pg * NUM_BITS_PER_BLOCK;
	   }
	   sect_per_pg		= blks_per_pg/blks_per_sect;
	   cca_info->byte_per_pg	= blks_per_pg * BYTPBLOCK;
	}

	cca_info->bsize		= bs;
	cca_info->blks_per_pg	= blks_per_pg;
	cca_info->sect_per_pg	= sect_per_pg;

/*
 *	Allocate the buffer control blocks contiguously.
 */
   if ( create_cache_buffers ) {
	if ( ( cca_info->async_tracker	= _cca_add_trackers( 10 ) ) == NULL ) {
	    fprintf(_GL_cca_logptr,"cca : Unable to allocate space for async trackers\n");
	}
	if ((cca_info->bufs	= calloc(nb*(sizeof(struct cca_buf)),1)) == NULL) {
	    errn	= FDC_ERR_NOMEM;
	    goto close_badret;
	}

	sectmsk_siz	= (sect_per_pg + sizeof(int64)*8 - 1)/(sizeof(int64)*8);
	cca_info->dirty_sectwds	= sectmsk_siz;

/*
 *	Add up the storage requirements for a bunch of info fields and 
 *	allocate them all at once. (Should probably make this a struct?)
 */
	alloc_bytes	= 0;
	alloc_bytes    += sectmsk_siz*sizeof(int64);    /* check words */
	alloc_bytes    += nb*sectmsk_siz*sizeof(int64); /* unsynced sector bits */
	alloc_bytes    += nb*sectmsk_siz*sizeof(int64); /* valid sector bits */
	alloc_bytes    += nb*sectmsk_siz*sizeof(int64); /* sector reuse bits */
	alloc_bytes    += sizeof(int);                /* shared chronometer */
	alloc_bytes    += sizeof(int);                /* shared spilled */
	alloc_bytes    += sizeof(plock_t);		   /* cache_lock */

	storage		= malloc(alloc_bytes);

	if (storage == NULL ) {
	   errn	= FDC_ERR_NOMEM;
	   goto close_badret;
	}

	cca_info->dirty_sectors_check	= (int64 *)storage;
	storage		= storage + (sectmsk_siz * sizeof(int64));

	for (i = 0; i < sect_per_pg; i++)
		_SETBIT(i, cca_info->dirty_sectors_check);

	for (i = 0; i < nb; i++) {
	    register int	j;

	    cca_info->bufs[i].index		= i;
	    cca_info->bufs[i].file_page.all	= NULL_FILE_PAGE;
	    cca_info->bufs[i].prev_cubuf	= NULL;
	    cca_info->bufs[i].prev_file_page.all = -1;
	    cca_info->bufs[i].eligible		= FALSE;
	    cca_info->bufs[i].protected		= FALSE;
	    cca_info->bufs[i].direction		= -99999;
	    cca_info->bufs[i].pending_asyncs	= 0;
	    cca_info->bufs[i].chain_position	= 0;
	    cca_info->bufs[i].exempt_count	= 0;
	    CLRFFSTAT( cca_info->bufs[i].sw );
	    cca_info->bufs[i].sw.inuse		= NULL;
	    cca_info->bufs[i].sw.rw_mode	= 0;
	    cca_info->bufs[i].sw.file_page.all	= 0;
	    cca_info->bufs[i].sw.llfio		= NULL;

	    cca_info->bufs[i].unsynced_sectors	= (int64 *)storage;
	    storage +=  sectmsk_siz*sizeof(int64);
	    cca_info->bufs[i].valid_sectors = (int64*)storage;
	    storage +=  sectmsk_siz*sizeof(int64);
	    cca_info->bufs[i].sector_used   = (int64*)storage;
	    storage +=  sectmsk_siz*sizeof(int64);
	    for (j = 0; j < sectmsk_siz; j++) {
		cca_info->bufs[i].unsynced_sectors[j] = 0;
		cca_info->bufs[i].valid_sectors[j] = 0;
		cca_info->bufs[i].sector_used[j] = 0;
	    }
	}

	cca_info->chronometer = (int *) storage;
	*(cca_info->chronometer) = 0;
	storage += sizeof(int);

	cca_info->spilled  = (int *) storage;
	*(cca_info->spilled ) = FALSE;
	storage += sizeof(int);

	cca_info->cache_lock  = (plock_t *)storage;
	INITIALIZE_LOCK(*(plock_t *)(cca_info->cache_lock));
	storage += sizeof(plock_t);

/*
 *	Allocate the page buffers contiguously.  They are allocated in 
 *	memory or in SDS.
 */

#ifdef	SDS_SUPPORTED
	if (cca_info->optflags.sds) {
		int oblk, istat;
		oblk = sdsalloc(nb * BITS2BLOCKS(bs), &istat);
		if (oblk == ERR || istat == ERR) {
			_SETERROR(stat, FENOSDSP, 0);
			/* Set cca_info->bufs[0].buf to a special */
			/* value so cca_clfree will not try to free it */
			cbufs = cca_info->bufs;
			cbufs[0].buf = -1;
		        errn = FENOSDSP;
			goto close_badret;
		}
		s = (char *)NULL + oblk * BYTPBLOCK;	/* byte pointer */
	}
	else
#endif
#ifdef	__mips
		if (o_direct) {
			if ((s = memalign(dio.d_mem, nb * BITS2BYTES(bs))) == NULL) {
				errn = FDC_ERR_NOMEM;
				goto close_badret;
			}
		}
		else
#endif
		if ((s = malloc(nb * BITS2BYTES(bs))) == NULL) {
		        errn = FDC_ERR_NOMEM;
			goto close_badret;
		}

			
	SET_BPTR(bptr, CPTR2BP(s));

	cbufs = cca_info->bufs;

	for (i = 0; i < nb; i++) {
		SET_BPTR(cbufs[i].buf, bptr);	/* page buffer bit address */
		SET_BPTR(bptr,(INC_BPTR(bptr,bs)));
	}
   }   /*  end of create_cache_buffers */

	if ( create_shared_info )
	   *_CCA_scache[cache_number].cca_info[0] = *cca_info;

	if ( cache_number != 0 ) {  /* create link from shared to local */
	   _CCA_scache[cache_number].cca_info[file_number] = cca_info;
	   if (MULTI_ON)
	     cca_info->is_shrdlck = 1;
	}
/*
 *	Get information about the underlying layer.
 */
	ret = XRCALL(nfioptr,fcntlrtn) nfioptr, FC_STAT, &fstat, stat);

	if (ret == ERR) {
		errn = stat->sw_error;
		goto close_badret;
	}

	ret = XRCALL(nfioptr,fcntlrtn) nfioptr, FC_GETINFO, &layer_info, stat);

	if (ret == ERR) {
		errn = stat->sw_error;
		goto close_badret;
	}

	if ( layer_info.ffc_flags & FFC_CANLISTIO )
		cca_info->optflags.do_listio = TRUE;
	if ( layer_info.ffc_flags & FFC_CANSYLISTIO )
		cca_info->optflags.do_sylistio = TRUE;
	cca_info->nextfio = (struct fdinfo *)nextfio;
	
/*
 *	Store the size of the underlying file.   For block special files,
 *	the size is unknown.  Assume infinite size until kernel is fixed to
 *	cause stat system call to return the proper size.
 */
	if (S_ISBLK(fstat.st_mode) && fstat.st_size == 0) {
#if	defined(_mips)

                cca_info->feof = LONGLONG_MAX;	/* assume infinite size */
#else
                cca_info->feof = LONG_MAX;	/* assume infinite size */
#endif
                cca_info->is_blkspec = 1;	/* flag block special file */
        }
	else
		cca_info->feof = fstat.st_size << 3;	/* size in bits */
	cca_info->st_blksize = fstat.st_blksize;

#if	defined(_CRAY1) || defined(_CRAYMPP)
#ifdef	SDS_SUPPORTED
	if (cca_info->optflags.sds ) {
		/*
		 * If SDS cache is requested, the underlying layer must support
		 * O_SSD because data transferral between this layer and below
		 * is by backdoor reads and writes.
		 */
		if ( nfioptr->class != CLASS_SYSTEM && nfioptr->class != CLASS_SYSCALL )
		{
		   if ((layer_info.ffc_flags & FFC_BCKDOOR) == 0) {
		           _SETERROR(stat, FDC_ERR_BCKDOOR, 0);
		           errn = FDC_ERR_BCKDOOR;
		           goto close_badret;
		   }
		}
		if (cca_info->is_welfrm) {
			if (cca_info->feof & (BITPBLOCK -1 )) {
				_SETERROR(stat, FDC_ERR_OPNGRAN, 0);
				errn = FDC_ERR_OPNGRAN;
				goto close_badret;
			}
		}
		else {
			/*
			 * Open the front door file descriptor, which will be 
			 * used for non-wellformed requests.
			 */
			fdfio = _ffopen(name, oflags & (~O_SSD),
			     mode, nspec, stat, cbits, cblks, NULL, oinf );
		        if (fdfio == _FFOPEN_ERR) {
		    		/*
				 * Retry with the oflags originally passed.
				 */
				oflags  = orig_oflags;
			    	fdfio = _ffopen(name, oflags & (~O_SSD), mode, 
			           nspec, stat, cbits, cblks, NULL, oinf );
			    	if (fdfio == _FFOPEN_ERR) {
			           goto close_badret;
				   errn = stat->sw_error;
				}
		        }
		}
		cca_info->frontdoorfio = (struct fdinfo *)fdfio;
	}
#endif
#endif

/*
 *	Initialize the logical size of the file and the current file position.
 */
	cca_info->fsize = cca_info->feof;	/* logical end of file */
	cca_info->cpos  = 0;			/* positioned at start of file*/
	cca_info->cubuf = NULL;			/* no buffer is assigned yet */ 

/*
 *	If a shared cache has been initialized, then copy in the description of
 *	the shared cache.
 */
	if ( create_cache_buffers == FALSE ) {
	    struct shared_cache_tab *sct;
	    sct = &_CCA_scache[cache_number];
	    cca_info->bufs		= sct->cca_info[0]->bufs;
	    cca_info->chronometer	= sct->cca_info[0]->chronometer;
	    cca_info->spilled		= sct->cca_info[0]->spilled;
	    cca_info->cache_lock	= sct->cca_info[0]->cache_lock;

	    cca_info->dirty_sectors_check = 
		sct->cca_info[0]->dirty_sectors_check;
	    cca_info->dirty_sectwds	= sct->cca_info[0]->dirty_sectwds;
	    cca_info->async_tracker	= sct->cca_info[0]->async_tracker;

	    /* If any layer has diags, then the shared layer has diags */
	    if ( cca_info->optflags.diags )
		sct->cca_info[0]->optflags.diags = TRUE;

	    /* Ensure that the buffers will be handled the same */
	    cca_info->optflags.sds	= sct->cca_info[0]->optflags.sds;
	    cca_info->nbufs		= sct->cca_info[0]->nbufs;
	    cca_info->bsize		= sct->cca_info[0]->bsize;
	    cca_info->byte_per_pg	= sct->cca_info[0]->byte_per_pg;
	    cca_info->blks_per_pg	= sct->cca_info[0]->blks_per_pg;
	    cca_info->sect_per_pg	= sct->cca_info[0]->sect_per_pg;
	    cca_info->bits_per_sect	= sct->cca_info[0]->bits_per_sect;
	    cca_info->bypass		= sct->cca_info[0]->bypass;
	}

	if (cca_info->max_lead > (cca_info->nbufs-1) )
	    cca_info->max_lead = (cca_info->nbufs-1);

	cca_info->child_class = ((struct fdinfo *)(nextfio))->class;

	if (MULTI_ON && cache_number) {
	    /* Tell _ffopen to use our lock for shared cache */
	    fio->lock_word = cca_info->cache_lock;
	}
	if (cache_number != 0) {
		if (locked)
			SCACHE_UNLOCK(_CCA_scache_lock);
	}
/*
 *	Here is the normal return point.  Before we go, let's make some
 *	assertions.
 */
#ifdef	_UNICOS
	assert ( _popcnt(cca_info->bits_per_sect) == 1 ); /* must be power of 2*/
#endif
	return(nextfio);			/* normal return */

#ifdef	NOT_IMPLEMENTED
nomem_bypass:
	if ( CCA_flags_BYPASS_ON_NO_SPACE & cca_flags ) {
	   fprintf(_GL_cca_logptr,"Attempting to bypass cca layer : Not enough space for cache pages\n");
	   cca_info->bypass = TRUE;
	   if ( create_shared_info ) {
	      create_shared_info = FALSE;
	      _CCA_scache[cache_number].file_count = 0;
	      cca_info->shared_cache = 0;
	   }
		/* I've changed the order of operations in this code. */
		/* This won't work anymore - needs changes. Fortunately, */
		/* we could never execute this anyway. */
	   goto continue_bypass;
	}
#endif

/*
 * These error-handling routines assume the error code in is errn.
 * This prevents the clean-up routines from overwriting the error
 * code with a subsequent--and probably less descriptive--error.
 */

err_unlock_free:
	if ( cache_number != 0 ) {
		if (_CCA_scache[cache_number].file_count == 1) {
			free(_CCA_scache[cache_number].cca_info[0]);
			free(_CCA_scache[cache_number].cca_info);
			free_buf = TRUE;
		}
		else
			free_buf = FALSE;
		_CCA_scache[cache_number].file_count--;
	}

err_unlock:
	if ( cache_number != 0 && locked) 
	   SCACHE_UNLOCK(_CCA_scache_lock);

err_ret:
	if ( cache_number == 0 ) 
	   free_buf = TRUE;
	_cca_clfree(fio, fio->fioptr, free_buf);
	_SETERROR(stat, errn, 0)
	return(_FFOPEN_ERR);

close_badret:
	if ( cache_number != 0 && locked) 
	   SCACHE_UNLOCK(_CCA_scache_lock);
	(void) XRCALL(nfioptr,closertn) nfioptr, &clstat);
	if (errn == 0) /* If--for some reason--no error code is set */
		errn = clstat.sw_error;
	free(nfioptr);
#ifdef	SDS_SUPPORTED
	if ((cca_info->optflags.sds) && (fdfio >= 0)) {
	   (void) XRCALL(cca_info->frontdoorfio,closertn)
               cca_info->frontdoorfio, &clstat);
	}
#endif
	if (errn == 0) /* If--for some reason--no error code is set */
		errn = clstat.sw_error;
	goto err_unlock_free;
}

struct cca_async_tracker *
_cca_add_trackers( int num_to_add )
{
	register int	i;
	struct cca_async_tracker *first;
	struct cca_async_tracker *this_tracker;

	first = calloc( 1, sizeof(struct cca_async_tracker));

	if ( first == NULL )
	    return(NULL);

	this_tracker = first;

	for (i = 0; i < num_to_add - 1; i++) {
	    this_tracker->mode         = CCA_TRACKER_FREE;
	    this_tracker->next_tracker =
		calloc( 1, sizeof(struct cca_async_tracker));
	    if (this_tracker->next_tracker == NULL )
		return(NULL);
	    this_tracker = this_tracker->next_tracker;
	}

	this_tracker->mode         = CCA_TRACKER_FREE;
	this_tracker->next_tracker = NULL;

	return( first );
}

/*
 * Returns -1 if there is conflict,
 * Returns 0 if OK
 */
_check_scache(int lowercache,int uppercache)
{
	register int	i;
	struct cca_f	*cptr;
	struct fdinfo	*fptr;

	if (_CCA_scache[lowercache].file_count != 0) {
	    for (i = 0; i < MAX_FILES_PER_SHARED_CACHE; i++) {
		cptr = _CCA_scache[lowercache].cca_info[i];
		if (cptr != NULL) {
			/* Now we need to proceed down the chain, */
			/* looking for cachea layers that have */
			/* shared cache. If the shared cache */
			/* number == uppercache, it is an error */
			fptr = cptr->nextfio;
			while (fptr != NULL) {
				if (fptr->class == CLASS_CACHEA) {
				    cptr = (struct cca_f *)fptr->lyr_info;
				    if (cptr->shared_cache == uppercache)
					return(-1);
				}
				fptr = fptr->fioptr;
			}
		}
	    }
	}
	return(0);
}
