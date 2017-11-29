/*
 * Copyright 2005, 2006 PathScale, Inc.  All Rights Reserved.
 */

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


#pragma ident "@(#) libu/ffio/globopen.c	92.1	06/29/99 13:16:47"

#include <assert.h>
#include <ffio.h>
#include <string.h>
#include <cray/assign.h>
#include "globio.h"
#include "fxlist.h"
#if defined(BUILD_OS_DARWIN)
#include <sys/types.h>
#include <sys/stat.h>
#endif /* defined(BUILD_OS_DARWIN) */

DECLARE(GLOBAL_XLIST);

struct xtr_s GLOBAL_XLIST_P = { GLOBAL_XLIST };

/*
 *      Open routine for "global" layer on IRIX systems.
 *
 *      Returns: pointer to fdinfo block from next lower layer on success.
 *               -1 if an error occurred.
 */

_ffopen_t
_glob_open(
const char	*name,
int		flags,
mode_t		mode,
struct fdinfo	*fio,
union spec_u	*spec,
struct ffsw	*stat,
long		cbits,
int		cblks,
struct gl_o_inf *oinf)
{
	struct fdinfo	*nfioptr;
	union spec_u	*nspec;
	struct glob_f	*glob_info;
	struct stat	fstat;
	struct ffsw	clstat;
#ifdef KEY /* Bug 1678 */
        struct stat statbuf;
#else /* KEY Bug 1678 */
        struct ffc_stat_s statbuf;
#endif /* KEY Bug 1678 */
	_ffopen_t	nextfio;
	register int	bsblks;
	register int	errn;
	register int	groupsz;	/* # of processes which access this file */
	register int	npages;
	register int	oflags;
	register int	ret;
        size_t		symd_glob_info;
        size_t		symd_pagedesc;
        size_t		symd_pgtablcks;
        size_t		symd_wakebm;
        size_t		symd_cachepages;
	va_list		ap;
	register short	ll_is_open = 0;
	int		isvalid;
	struct gliocoms *coms;
	size_t		pgsize_bytes;
	size_t		sa_len;
	size_t		sa_off;
	size_t		blen;
	assign_info	*aip = oinf->aip;
	glio_group_t	*gg;
	glio_arena_t	*arp;

	coms	= NULL;
	arp	= NULL;
	errn	= 0;
	/*
	 * Read up default buffer sizes, etc. from the environment.
	 */

	_glob_io_init();

	/*
	 * Now set up the shared memory arena which will contain library
	 * I/O cache buffers and other control structures.
	 *
	 * The first numeric parameter on the spec is the buffer size in 
	 * 512 word blocks.  The first global file opened determines the
	 * buffer size for all subsequent global files.
	 */

        if (_glio_curgroup.grtype == GR_DEFAULT)
		_glio_group_init(&_glio_curgroup);

	groupsz	= _glio_curgroup.groupsz;

	/*
	 * SHMEM groups require MPT 1.2 or higher because the code relies
	 * on _shmem_barrier_all and shmalloc.
	 */
        if (_glio_curgroup.grtype == GR_SHMEM && groupsz > 1) {
        	if (shmalloc == NULL || _shmem_barrier_all == NULL) {
			if (_my_pe() == 0) {
				fprintf(stderr,"%s:\n\
    Global files used in SHMEM programs require libsma.so from MPT 1.2 or\n\
    later.  The version of libsma you are currently using is from the MPT\n\
    1.1 release.\n", GLOBERRMSG);
				abort();
			}
		}
	}

	bsblks	= _ff_nparm_getv(spec, 1, &isvalid);

	if (!isvalid) 
		bsblks	= _par_vars.pgsize;

	pgsize_bytes	= bsblks * YMPBLOCK;

	/*
	 * 	The second numeric parameter on the spec is the number of 
	 *	buffers per PE.
	 */
	npages	= _ff_nparm_getv(spec, 2, &isvalid);

	if (!isvalid) 
		npages	= _par_vars.pepages;

        /*
         * Symmetric memory is allocated from the symmetric arena buffer.
	 * Compute the total space needed while allowing for padding 
	 * between data areas.
         */

	sa_off		= (size_t) _glio_padmem( (void *) NULL );

	/* for glob_info */

	symd_glob_info	= sa_off;
	sa_len		= sizeof(*glob_info);
	sa_off		= (size_t)_glio_padmem( (void *)(sa_off + sa_len) );

	/* for glob_info->ppages */

	symd_pagedesc	= sa_off;
	sa_len		= npages * sizeof(struct par_page_description);
	sa_off		= (size_t)_glio_padmem( (void *)(sa_off + sa_len) );

	/* for glob_info->ppage_table_locks */

        symd_pgtablcks	= sa_off;
	sa_len		= groupsz * sizeof(long);
	sa_off		= (size_t)_glio_padmem( (void *)(sa_off + sa_len) );

	/* for glob_info->wakeup_bitmap */

        symd_wakebm	= sa_off;
	sa_len		=  (groupsz/BITPW + 1) * sizeof(long long);
	sa_off		= (size_t)_glio_padmem( (void *)(sa_off + sa_len) );

	/* for glob_info->cchpages */

        symd_cachepages	= sa_off;
	sa_len		= npages * (STRMPAD + pgsize_bytes);
	sa_off		= (size_t)_glio_padmem( (void *)(sa_off + sa_len) );

	arp		= _glio_arena_create(&_glio_curgroup, sa_off);

	if (arp == NULL) {
		errn	= FDC_ERR_NOMEM;
		goto badret;
	}

	gg		= &arp->grp;		/* point to the updated copy */

	_glio_barrier(arp);

	/*
	 * Now that the arena is allocated, assign specific addresses
	 * to the various symmetric memory areas.
	 */

	glob_info	= (void *)
			((char *)arp->mybase + symd_glob_info);

        glob_info->ppages = (void *) 
			((char *)arp->mybase + symd_pagedesc);

        glob_info->ppage_table_locks = (void *)
			((char *)arp->mybase + symd_pgtablcks);

        glob_info->wakeup_bitmap = (void *)
			((char *)arp->mybase + symd_wakebm);

        glob_info->cchpages = (void *) 
			((char *)arp->mybase + symd_cachepages);

	coms			= &glob_info->coms;
	coms->errn		= 0;	/* initialize error flag */
	glob_info->arp		= arp;
	glob_info->myrank	= arp->grp.myrank;
	glob_info->groupsz	= groupsz;
        fio->lyr_info		= (char *)glob_info;
	glob_info->user_oflags	= flags;
	glob_info->pgsize	= bsblks;
	glob_info->pgsize_bytes	= glob_info->pgsize * YMPBLOCK;
	glob_info->pepages	= npages;
	glob_info->pgpad	= STRMPAD;
	glob_info->fstate_owner	= gg->groupsz / 2; /* arbitrary owner */

	/*
	 *	Allocate the cache page buffers and control tables.
	 */

        blen			= ((gg->groupsz / BITPW) + 1) * sizeof(long long);
        glob_info->lwakeup_bitmap = _glio_malloc(glob_info, blen);
        (void) memset((void *) glob_info->lwakeup_bitmap, 0, blen);

        blen			= npages * sizeof(struct par_page_description);
        glob_info->myppages	= _glio_malloc(glob_info, blen);

        blen			= pgsize_bytes;
        glob_info->my_iobuf	= _glio_malloc(glob_info, blen);
        (void) memset(glob_info->my_iobuf, 0, blen);

	/*
	 *	Get values from the FFIO spec.   The .globpos/.privpos option 
	 * 	specifies if the file position pointer is private to all PEs
	 *	(the default) or globally shared by all PEs.
	 */
	if (spec->fld.recfmt == TR_GLOB_GLOBPOS) 
		glob_info->globpos	= 1;

	/*
	 *	Set up oflags and more...
	 */

	oflags	= flags;
	
	if (flags & O_APPEND)	/* Should return FDC_ERR_OAPPEND */
		_par_abort(arp, "par_open: does not support O_APPEND");
 
	if ((flags & O_WRONLY) || (flags & O_APPEND)) {
		flags	= flags & ~(O_WRONLY | O_APPEND);
		/* when writing, we may have to pre-read pages in the file */
		flags	= flags | O_RDWR;
	}

#ifdef	O_PLACE
	if (flags & O_CREAT)
		flags	= flags | O_PLACE;	/* needed for cbits/cblks */
#endif

#ifdef	O_RAW
	/*
	 * Open O_RAW always - this is more consistent with default FFIO
         * behavior.
         */

	flags	= flags | O_RAW;
#endif

	ret	= 0;

/*
 *      Get the FFIO spec for the next lower layer.
 */
        nspec	= spec;
        NEXT_SPEC(nspec);

/*
 *      Check to see if any of the PEs have a -n option to handle.
 */

        coms->glob_preall_flg	= 0;

        _glio_barrier(arp);

        if (aip != NULL && aip->n_preall_flg &&
                (aip->n_preall_flg & ATTR_USED) == 0) {
                /*
                 * There is a race to see which PE is the last to be
                 * put in glob_preall_pe on PE 0.  The last one put there
                 * will be the PE that ends up doing the preallocation.
                 * This choice is arbitrary.
                 *
                 * We assume that either all PEs have the same -n option
                 * or else the user has only specified the -n option on
                 * one of the PEs.   The former case is preferred over
                 * the latter.
                 */
                _glio_shmem_int_p(arp, &coms->glob_preall_flg, 1, 0);
                _glio_shmem_int_p(arp, &coms->glob_preall_pe, gg->myrank, 0);

                /*
                 * We set the ATTR_USED bit so that lower level layers do
                 * not also do preallocation.
                 */
                aip->n_preall_flg |= ATTR_USED;
        }

        _glio_barrier(arp);

/*
 *      Open the layers below this one.
 */
        nextfio	= _ffopen(name, flags, mode, nspec, stat, cbits, cblks, NULL,
                        oinf);

        if (nextfio == _FFOPEN_ERR) 
		coms->errn	= stat->sw_error;
	else
		ll_is_open	= 1;

	if (_par_broadcast_error(arp, &coms->errn) != 0)
		goto badret;

        nfioptr	= (struct fdinfo *)nextfio;

#ifdef	_CRAY
	/*
	 *      Now do the file preallocation implied by -n.
	 */
        _glio_shmem_int_get(arp, &coms->glob_preall_flg, &coms->glob_preall_flg,
		1, 0);
        _glio_shmem_int_get(arp, &coms->glob_preall_pe, &coms->glob_preall_pe,
		1, 0);
        if (coms->glob_preall_flg && gg->myrank == coms->glob_preall_pe) {
                struct ffc_info_s	layer_info;

                ret	= XRCALL(nfioptr, fcntlrtn)
                                nfioptr, FC_GETINFO, &layer_info, stat);
                if (ret == ERR) {
                        coms->errn	= stat->sw_error;
                        goto past_getinfo;
                }

                if (layer_info.ffc_fd >= 0) {
                        /*
                         * Clear the ATTR_USED bit so that _prealloc will go
                         * ahead and do the preallocation.
                         */
                        aip->n_preall_flg &= ~ATTR_USED;
                        if (_prealloc(layer_info.ffc_fd, 1, aip, NULL) == -1) {
                                coms->errn	= errno;
                                goto past_getinfo;
                        }
                }
        }
#endif	/* _CRAY */

past_getinfo:
        if (_par_broadcast_error(arp, &coms->errn) != 0)
                goto badret;

	/*
	 * Assume the layer beneath is the system layer for now.
	 */

	if (nfioptr->class != CLASS_SYSCALL && nfioptr->class != CLASS_SYSTEM)
		coms->errn	= FDC_ERR_GLSYS;

	if (_par_broadcast_error(arp, &coms->errn) != 0)
		goto badret;

	glob_info->sysfd	= nfioptr->realfd;

	/*
	 * Get information about the underlying file.  This need only be
	 * done on PE 0. The info is stored to the shared fdata table.
	 */

	glob_info->fname	= _glio_strdup(glob_info, name);

	if (glob_info->fname == NULL)
		coms->errn	= FDC_ERR_NOMEM;

	if (_par_broadcast_error(arp, &coms->errn) != 0) 
		goto badret;

	glob_info->oflags	= oflags;

	glob_info->file_off	= 0;

	/*
	 * Make a stat call to find various info about the file.
	 */
        ret	= XRCALL(nfioptr, fcntlrtn) nfioptr, FC_STAT, &statbuf, stat);

       	if (ret == ERR) 
		coms->errn	= stat->sw_error;

	if (_par_broadcast_error(arp, &coms->errn) != 0) 
		goto badret;

	glob_info->inode	= statbuf.st_ino;
	glob_info->device	= statbuf.st_dev;

	if (gg->myrank == 0) {
		/*
		 * Send out the device and inode number to all PEs.
		 */
		coms->inode	= glob_info->inode;
		coms->device	= glob_info->device;
	}

	_par_bcast0(arp, &coms->inode, sizeof(coms->inode));
	_par_bcast0(arp, &coms->device, sizeof(coms->device));

	/*
	 *	Verify that the same file has been opened on all PEs.
	 */

	if (   coms->inode != glob_info->inode 
	    || coms->device != glob_info->device )
		coms->errn	= FDC_ERR_GLMFILE;

	if (_par_broadcast_error(arp, &coms->errn) != 0) 
		goto badret;

	/*
	 * The PE who owns the file state structure initiializes it.
	 */

	_glio_barrier(arp);

	if (gg->myrank == glob_info->fstate_owner) {
		par_file_data_t	*fptr;

		fptr			= &glob_info->fstate;
		fptr->size		= statbuf.st_size;
		fptr->bytes_on_disk	= fptr->size;

		if (_par_vars.do_tracing)
			_par_write_open_trace(fio, oflags, name);
	}

        if (_par_broadcast_error(arp, &coms->errn) != 0)
		goto badret;

        return(nextfio);

badret:
	if (ll_is_open)
        	(void) XRCALL(nfioptr, closertn) nfioptr, &clstat);

	if (coms != NULL) {
		errn	= coms->errn;
		_glob_clfree(fio);		/* collective operation */
	}

	if (arp != NULL)
		_glio_arena_destroy(arp);

	_FFOPEN_ERETURN(stat, errn, 0);
}

/*
 *	Broadcast one word from PE 0 to all other PEs.
 *
 *	This is a collective routine.
 */
void
_par_bcast0(
	glio_arena_t	*arp,
	void	*ptr,		/* pointer to quadword-aligned address */
	int	size)		/* size in bytes */
{
	_glio_barrier(arp);

	if (arp->grp.myrank != 0)
		_glio_shmem_getmem(arp, ptr, ptr, size, 0);

	_glio_barrier(arp);

	return;
}

/*
 *	Consolidate error codes across all PEs.  If any PE has an error
 *	code then all PEs get it.   If more than one PE have nonzero error 
 *	codes, we arbitrate by using the error code on the PE with lowest
 *	PE number.
 */

int
_par_broadcast_error(glio_arena_t *arp, int *errad)
{
	register int	ipord;

	_glio_barrier(arp);

	for (ipord = 0; ipord < arp->grp.groupsz; ipord++) {
		int	remote_errn;

		_glio_shmem_getmem(arp, &remote_errn, errad, sizeof(*errad),
			ipord);
		if (remote_errn != 0) {
			*errad	= remote_errn;
			break;
		}
	}

	_glio_barrier(arp);

	return(*errad);	
}
