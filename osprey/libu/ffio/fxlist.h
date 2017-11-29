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


/* USMID @(#) libu/ffio/fxlist.h	92.3	10/15/99 10:22:38 */

#if	defined(__mips) || defined(_LITTLE_ENDIAN)
#define FUNDECL(open, read, reada, readc, write, writea, writec, close,	\
		flush, weof, weod, seek, bksp, pos, listio, fcntl)	\
									\
		extern _ffopen_t open(const char *, int, mode_t, 	\
			struct fdinfo *, union spec_u *, struct ffsw *, \
			long, int, struct gl_o_inf *);			\
		extern ssize_t	read(struct fdinfo *, bitptr, size_t,	\
			struct ffsw *, int, int *);			\
		extern ssize_t	reada(struct fdinfo *, bitptr, size_t,	\
			struct ffsw *, int, int *);			\
		extern ssize_t	readc(struct fdinfo *, bitptr, size_t,	\
			struct ffsw *, int );			\
		extern ssize_t	write(struct fdinfo *, bitptr, size_t,	\
			struct ffsw *, int, int *);			\
		extern ssize_t	writea(struct fdinfo *, bitptr, size_t,	\
			struct ffsw *, int, int *);			\
		extern ssize_t	writec(struct fdinfo *, bitptr, size_t,	\
			struct ffsw *, int );			\
		extern int	close(struct fdinfo *, struct ffsw *),	\
				flush(struct fdinfo *, struct ffsw *),	\
				weof(struct fdinfo *, struct ffsw *),	\
				weod(struct fdinfo *, struct ffsw *);	\
		extern _ffseek_t seek(struct fdinfo *, off_t, int, struct ffsw *),\
		 		pos(struct fdinfo *, int, void *, int, struct ffsw *);			\
		extern int	bksp(struct fdinfo *, struct ffsw *),	\
		  listio(), 						\
		  fcntl(struct fdinfo *, int, void *, struct ffsw *)
#else
#define FUNDECL(open, read, reada, readc, write, writea, writec, close,	\
		flush, weof, weod, seek, bksp, pos, listio, fcntl)	\
									\
		extern _ffopen_t open();				\
		extern ssize_t	read();					\
		extern ssize_t 	reada();	 			\
		extern ssize_t	readc();				\
		extern ssize_t  write();				\
		extern ssize_t	writea(); 				\
		extern ssize_t  writec();				\
		extern int	close(),			\
				flush(), weof(), weod(); 		\
		extern _ffseek_t  seek(), pos();			\
		extern int	bksp(),  listio(), fcntl()
#endif
#define DECLARE(XLIST)		FUNDECL(XLIST)

#ifdef	_UNICOS
#define ZSOFT(TEXT)		_Pragma(#TEXT) ;
#else
#define ZSOFT(TEXT)		
#endif
/*
 * These declare and 'softize' pointers to the function pointers
 */
#define DECLARE_P(XLIST_P)	extern struct xtr_s XLIST_P ;

#define SOFTIZE_P(XLIST_P)	ZSOFT(_CRI soft (XLIST_P))

/*
 * Use this macro to generate segldr(1) directives.
 */

#define LOADIT(XLIST_P)		HARDREF=XLIST_P

/*
 * Each of the following lists represents the entry points for one
 * of the layers.
 */
     /*	open		read		reada		readc
	write		writea		writec		close
	flush		weof		weod		seek
	bksp		pos		listio		fcntl	*/

#define  END_XLIST_P	_end_ffvect
#define  END_XLIST							\
	_gsys_open,	_ff_err_rw,	_ff_err_rw,	_ff_err_rwc,	\
	_ff_err_rw,	_ff_err_rw,	_ff_err_rwc,	_ff_err2,	\
	_ff_err2,	_ff_err2,	_ff_err2,	_ff_err_seek,	\
	_ff_err2,	_ff_err_pos,	_ff_err_listio,	_ff_err_fcntl
#define SYSCALL_XLIST_P	_syscall_ffvect

#ifdef	_UNICOS
#define SYSCALL_XLIST							\
	_sys_open,	_sys_read,	_sys_reada,	_ff_readc,	\
	_sys_write,	_sys_writea,	_ff_writec,	_sys_close,	\
	_sys_flush,	_ff_err2,	_sys_trunc,	_sys_lseek,	\
	_ff_err2,	_ff_pos,	_sys_listio,	_sys_fcntl
#else
#if	defined(__mips)
#define SYSCALL_XLIST							\
	_sys_open,	_sys_read,	_sys_reada,	_ff_readc,	\
	_sys_write,	_sys_writea,	_ff_writec,	_sys_close,	\
	_sys_flush,	_ff_err2,	_sys_trunc,	_sys_lseek,	\
	_ff_err2,	_ff_pos,	_sys_listio,	_sys_fcntl
#elif	defined(_LITTLE_ENDIAN)
#define SYSCALL_XLIST							\
	_sys_open,	_sys_read,	_sys_read,	_ff_readc,	\
	_sys_write,	_sys_write,	_ff_writec,	_sys_close,	\
	_sys_flush,	_ff_err2,	_sys_trunc,	_sys_lseek,	\
	_ff_err2,	_ff_pos,	_sys_listio,	_sys_fcntl
#else
#define SYSCALL_XLIST							\
	_sys_open,	_sys_read,	_ff_err,	_ff_readc,	\
	_sys_write,	_ff_err,	_ff_writec,	_sys_close,	\
	_sys_flush,	_ff_err2,	_sys_trunc,	_sys_lseek,	\
	_ff_err2,	_ff_pos,	_sys_listio,	_sys_fcntl
#endif
#endif
#define NULL_XLIST_P	_null_ffvect
#define NULL_XLIST							\
	_ff_err_open,	_ff_err_rw,	_ff_err_rw,	_ff_err_rwc,	\
	_ff_err_rw,	_ff_err_rw,	_ff_err_rwc,	_ff_err2,	\
	_ff_err2,	_ff_err2,	_ff_err2,	_ff_err_seek,	\
	_ff_err2,	_ff_err_pos,	_ff_err_listio,	_ff_err_fcntl
#define SYSTEM_XLIST_P	_system_ffvect
#define SYSTEM_XLIST							\
	_gsys_open,	_ff_err_rw,	_ff_err_rw,	_ff_err_rwc,	\
	_ff_err_rw,	_ff_err_rw,	_ff_err_rwc,	_ff_err2,	\
	_ff_err2,	_ff_err2,	_ff_err2,	_ff_err_seek,	\
	_ff_err2,	_ff_err_pos,	_ff_err_listio,	_ff_err_fcntl

#define COS_XLIST_P	_cos_ffvect
#define COS_XLIST							\
	_cos_open,	_cos_read,	_cos_read,	_ff_readc,	\
	_cos_write,	_cos_write,	_ff_writec,	_cos_close,	\
	_ff_noop,	_cos_weof,	_cos_weod,	_cos_seek,	\
	_cos_bksp,	_cos_pos,	_ff_err_listio,	_cos_fcntl

#define TAPE_XLIST_P	_tape_ffvect
#define TAPE_XLIST							\
	_bmx_open,	_bmx_read,	_bmx_read,	_ff_readc,	\
	_bmx_write,	_bmx_write,	_ff_writec,	_bmx_close,	\
	_bmx_flush,	_bmx_weof,	_bmx_weod,	_bmx_seek,	\
	_bmx_bksp,	_bmx_pos,	_ff_err_listio,	_bmx_fcntl

#define F_XLIST_P	_f_ffvect
#define F_XLIST								\
	_gen_fopen,	_gen_fread,	_gen_fread,	_ff_readc,	\
	_gen_fwrite,	_gen_fwrite,	_ff_writec,	_ff_close,	\
	_ff_flush,	_ff_weof,	_gen_fweod,	_ff_seek,	\
	_ff_err2,	_ff_pos,	_ff_err_listio,	_gen_ffcntl

#define V_XLIST_P	_v_ffvect
#define V_XLIST								\
	_gen_vopen,	_gen_vread,	_gen_vread,	_ff_readc,	\
	_gen_vwrite,	_gen_vwrite,	_ff_writec,	_ff_close,	\
	_gen_vflush,	_gen_vweof,	_gen_vweod,	_gen_vseek,	\
	_ff_err2,	_ff_pos,	_ff_err_listio,	_gen_vfcntl

#define TXT_XLIST_P	_txt_ffvect
#define TXT_XLIST							\
	_txt_open,	_txt_read,	_txt_read,	_ff_readc,	\
	_txt_write,	_txt_write,	_ff_writec,	_ff_close,	\
	_ff_flush,	_txt_weof,	_txt_weod,	_txt_seek,	\
	_txt_bksp,	_ff_pos,	_ff_err_listio,	_txt_fcntl

#define X_XLIST_P	_x_ffvect
#define X_XLIST								\
	_gen_xopen,	_gen_xread,	_gen_xread,	_ff_readc,	\
	_gen_xwrite,	_gen_xwrite,	_ff_writec,	_ff_close,	\
	_gen_xflush,	_gen_xweof,	_gen_xweod,	_gen_xseek,	\
	_gen_xbksp,	_ff_pos,	_ff_err_listio,	_gen_xfcntl

#define CDC_XLIST_P	_cdc_ffvect
#define CDC_XLIST							\
	_cdc_open,	_cdc_read,	_cdc_read,	_ff_readc,	\
	_cdc_write,	_cdc_write,	_ff_writec,	_cdc_close,	\
	_cdc_flush,	_cdc_weof,	_cdc_weod,	_cdc_seek,	\
	_ff_err2,	_ff_pos,	_ff_err_listio,	_cdc_fcntl

#define SDS_XLIST_P	_sds_ffvect
#define SDS_XLIST							\
	_sds_open,	_sds_read,	_sds_reada,	_ff_readc,	\
	_sds_write,	_sds_writea,	_ff_writec,	_sds_close,	\
	_sds_flush,	_ff_err2,	_sds_weod,	_sds_seek,	\
	_ff_err2,	_ff_pos,	_ff_err_listio,	_fss_fcntl

#define MR_XLIST_P	_mr_ffvect
#define MR_XLIST							\
	_mr_open,	_mr_read,	_mr_reada,	_ff_readc,	\
	_mr_write,	_mr_writea,	_ff_writec,	_mr_close,	\
	_ff_noop,	_ff_err2,	_mr_weod,	_mr_seek,	\
	_ff_err2,	_ff_pos,	_ff_err_listio,	_fss_fcntl

#define TRC_XLIST_P	_trc_ffvect
#define TRC_XLIST							\
	_trc_open,	_trc_read,	_trc_reada,	_trc_readc,	\
	_trc_write,	_trc_writea,	_trc_writec,	_trc_close,	\
	_trc_flush,	_trc_weof,	_trc_weod,	_trc_seek,	\
	_trc_bksp,	_trc_pos,	_trc_listio,	_trc_fcntl

#define USR_XLIST_P	_usr_ffvect
#define USR_XLIST							\
	_usr_open,	_usr_read,	_usr_reada,	_usr_readc,	\
	_usr_write,	_usr_writea,	_usr_writec,	_usr_close,	\
	_usr_flush,	_usr_weof,	_usr_weod,	_usr_seek,	\
	_usr_bksp,	_usr_pos,	_usr_listio,	_usr_fcntl

#define SITE_XLIST_P	_site_ffvect
#define SITE_XLIST							\
	_site_open,	_site_read,	_site_reada,	_site_readc,	\
	_site_write,	_site_writea,	_site_writec,	_site_close,	\
	_site_flush,	_site_weof,	_site_weod,	_site_seek,	\
	_site_bksp,	_site_pos,	_ff_err_listio,	_site_fcntl

#define ERR_XLIST_P	_err_ffvect
#define ERR_XLIST							\
	_ff_err_open,	_ff_err_rw,	_ff_err_rw,	_ff_err_rwc,	\
	_ff_err_rw,	_ff_err_rw,	_ff_err_rwc,	_ff_err2,	\
	_ff_err2,	_ff_err2,	_ff_err2,	_ff_err_seek,	\
	_ff_err2,	_ff_err_pos,	_ff_err_listio,	_ff_err_fcntl

#define FD_XLIST_P	_fd_ffvect
#define FD_XLIST							\
	_fd_open,	_ff_err_rw,	_ff_err_rw,	_ff_err_rwc,	\
	_ff_err_rw,	_ff_err_rw,	_ff_err_rwc,	_ff_err2,	\
	_ff_err2,	_ff_err2,	_ff_err2,	_ff_err_seek,	\
	_ff_err2,	_ff_err_pos,	_ff_err_listio,	_ff_err_fcntl

#define BLX_XLIST_P	_blx_ffvect
#define BLX_XLIST							\
	_blx_open,	_blx_read,	_blx_read,	_ff_readc,	\
	_blx_write,	_blx_write,	_ff_writec,	_ff_close,	\
	_ff_noop,	_ff_err2,	_blx_weod,	_blx_seek,	\
	_ff_err2,	_ff_pos,	_ff_err_listio,	_blx_fcntl

#define CCH_XLIST_P	_cch_ffvect
#if	defined(_LITTLE_ENDIAN)
#define CCH_XLIST							\
	_cch_open,	_cch_read,	_cch_read,	_ff_readc,	\
	_cch_write,	_cch_write,	_ff_writec,	_cch_close,	\
	_cch_flush,	_ff_err2,	_cch_weod,	_cch_seek,	\
	_ff_err2,	_ff_pos,	_ff_err_listio,	_cch_fcntl
#else
#define CCH_XLIST							\
	_cch_open,	_cch_read,	_cch_reada,	_ff_readc,	\
	_cch_write,	_cch_writea,	_ff_writec,	_cch_close,	\
	_cch_flush,	_ff_err2,	_cch_weod,	_cch_seek,	\
	_ff_err2,	_ff_pos,	_ff_err_listio,	_cch_fcntl
#endif

#define ER90B_XLIST_P	_er90b_ffvect
#define ER90B_XLIST							\
	_er90b_open,	_er90b_read,	_er90b_reada,	_ff_readc,	\
	_er90b_write,	_er90b_writea,	_ff_writec,	_er90b_close,	\
	_ff_noop,	_ff_err,	_er90b_weod,	_er90b_lseek,	\
	_ff_err2,	_er90b_pos,	_ff_err_listio,	_er90b_fcntl

#define BUFA_XLIST_P	_bufa_ffvect
#define BUFA_XLIST							\
	_sqb_open,	_sqb_read,	_sqb_read,	_ff_readc,	\
	_sqb_write,	_sqb_write,	_ff_writec,	_sqb_close,	\
	_sqb_flush,	_sqb_weof,	_sqb_weod,	_sqb_seek,	\
	_ff_err2,	_sqb_pos,	_ff_err_listio,	_sqb_fcntl

#define CACHEA_XLIST_P	_cca_ffvect
#if	defined(__mips) || defined(_LITTLE_ENDIAN)
#define CACHEA_XLIST							\
	_cca_open,	_cca_read,	_cca_read,	_ff_readc,	\
	_cca_write,	_cca_write,	_ff_writec,	_cca_close,	\
	_cca_flush,	_ff_err2,	_cca_weod,	_cca_seek,	\
	_ff_err2,	_ff_pos,	_ff_err_listio,	_cca_fcntl
#else
#define CACHEA_XLIST							\
	_cca_open,	_cca_read,	_cca_reada,	_ff_readc,	\
	_cca_write,	_cca_writea,	_ff_writec,	_cca_close,	\
	_cca_flush,	_ff_err2,	_cca_weod,	_cca_seek,	\
	_ff_err2,	_ff_pos,	_cca_listio,	_cca_fcntl
#endif

#define EVENT_XLIST_P	_evnt_ffvect
#define EVENT_XLIST							\
        _evnt_open, 	_evnt_read, 	_evnt_reada, 	_evnt_readc,	\
        _evnt_write, 	_evnt_writea, 	_evnt_writec, 	_evnt_close,	\
        _evnt_flush, 	_evnt_weof, 	_evnt_weod, 	_evnt_seek,	\
        _evnt_bksp, 	_ff_pos, 	_evnt_listio, 	_evnt_fcntl
#if	defined(__mips)
#define LOCK_XLIST_P	_lock_ffvect
#define LOCK_XLIST							\
        _ff_err, 	_lock_read, 	_lock_reada, 	_lock_readc,	\
        _lock_write, 	_lock_writea, 	_lock_writec, 	_lock_close,	\
        _lock_flush, 	_lock_weof, 	_lock_weod, 	_lock_seek,	\
        _lock_bksp, 	_lock_pos, 	_ff_err_listio,	_lock_fcntl
#elif	defined(_LITTLE_ENDIAN)
#define LOCK_XLIST_P	_lock_ffvect
#define LOCK_XLIST							\
        _ff_err, 	_lock_read, 	_lock_read, 	_lock_readc,	\
        _lock_write, 	_lock_write, 	_lock_writec, 	_lock_close,	\
        _lock_flush, 	_lock_weof, 	_lock_weod, 	_lock_seek,	\
        _lock_bksp, 	_lock_pos, 	_ff_err_listio,	_lock_fcntl
#else
#define LOCK_XLIST_P	_lock_ffvect
#define LOCK_XLIST							\
        _ff_err, 	_lock_read, 	_lock_reada, 	_lock_readc,	\
        _lock_write, 	_lock_writea, 	_lock_writec, 	_lock_close,	\
        _lock_flush, 	_lock_weof, 	_lock_weod, 	_lock_seek,	\
        _lock_bksp, 	_lock_pos, 	_lock_listio, 	_lock_fcntl
#endif
#define GLOBAL_XLIST_P	_glob_ffvect
#define GLOBAL_XLIST							\
        _glob_open, 	_glob_read, 	_glob_reada, 	_ff_readc,	\
        _glob_write, 	_glob_writea, 	_ff_writec, 	_glob_close,	\
        _glob_flush, 	_ff_err2, 	_glob_weod, 	_glob_seek,	\
        _ff_err2, 	_ff_pos, 	_ff_err_listio,	_glob_fcntl

#define F77_XLIST_P	_f77_ffvect
#define F77_XLIST							\
	_f77_xopen,	_f77_xread,	_f77_xread,	_ff_readc,	\
	_f77_xwrite,	_f77_xwrite,	_ff_writec,	_f77_close,	\
	_f77_xflush,	_f77_xweof,	_f77_xweod,	_f77_xseek,	\
	_f77_xbksp,	_ff_pos,	_ff_err_listio,	_f77_xfcntl

#define TMF_XLIST_P	_tmf_ffvect
#define TMF_XLIST							\
	_tmf_open,	_tmf_read,	_tmf_read,	_ff_readc,	\
	_tmf_write,	_tmf_write,	_ff_writec,	_tmf_close,	\
	_tmf_flush,	_tmf_weof,	_tmf_weod,	_tmf_seek,	\
	_tmf_bksp,	_tmf_pos,	_ff_err_listio,	_tmf_fcntl

#define CMP_XLIST_P	_cmp_ffvect
#define CMP_XLIST							\
	_cmp_open,	_cmp_read,	_cmp_read,	_ff_readc,	\
	_cmp_write,	_cmp_write,	_ff_writec,	_cmp_close,	\
	_cmp_flush,	_cmp_weof,	_cmp_weod,	_cmp_seek,	\
	_ff_err2,	_cmp_pos,	_ff_err_listio,	_cmp_fcntl

/*
 * Stubs for user layers 0-9
 */

#define USR0_XLIST_P	_usr0_ffvect
#define USR1_XLIST_P	_usr1_ffvect
#define USR2_XLIST_P	_usr2_ffvect
#define USR3_XLIST_P	_usr3_ffvect
#define USR4_XLIST_P	_usr4_ffvect
#define USR5_XLIST_P	_usr5_ffvect
#define USR6_XLIST_P	_usr6_ffvect
#define USR7_XLIST_P	_usr7_ffvect
#define USR8_XLIST_P	_usr8_ffvect
#define USR9_XLIST_P	_usr9_ffvect
