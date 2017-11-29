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


/* USMID @(#) libu/ffio/cmpio.h	92.2	06/23/99 10:35:47 */
 

#ifndef __CMPIO_H_
#define __CMPIO_H_

#include <stdio.h>

#ifdef _CRAY
        typedef long                    int_64;
        typedef unsigned long           uint_64;
#else
        typedef long long               int_64;
        typedef unsigned long long      uint_64;
#endif

#define	TRUE	 1
#define FALSE	 0

#define CMP_FILEID              "compress"
#define CMP_FILEMAGIC           "12345666"

#define DEFAULT_SEGSZ           0x8000          /* 32K bytes */
#define MIN_SEGSZ               0x200           /* 512  bytes */
#define MAX_SEGSZ               0x800000        /* 8388608 bytes */

#define LZW_STRING		"LZW"
#define UNCMP_BUFFER_NUM	10
 
#define DEBUG_LVL0		0		/* No Debug    */
#define DEBUG_LVL1		1		/* (de)compress check */
#define DEBUG_LVL2		2		/* Info/Stats  */
#define DEBUG_LVL3		3		/* Enter/Leave */
#define DEBUG_LVL4		4		/* Detail info */
#define DEBUG_LVL_MAX		DEBUG_LVL4

#define TRC_INFO	if (cinfo->DebugLvl > DEBUG_LVL1) _TraceLog
#define TRC_ENTER	if (cinfo->DebugLvl > DEBUG_LVL2) _TraceEnter(rtnName, cinfo)
#define TRC_LEAVE	if (cinfo->DebugLvl > DEBUG_LVL2) _TraceLeave(rtnName, cinfo)
#define TRC_LOG		if (cinfo->DebugLvl > DEBUG_LVL3) _TraceLog
#define TRC_IT		_TraceLog

/*
 * Compressed file format: Data Segment Descriptor (DataSegD) (5 64-bit words)
 */
struct DataSegD {
	char	SegmentID[8];		/* Segmente descriptor ID	   */
        uint_64 CmpLength:32,           /* Length of compressed segment    */
                Length:32;              /* Actual uncompressed length      */
        uint_64 FileAddr;               /* Byte address in compressed dump */
                                        /* file where data segment begins  */
        uint_64 NextDataSegD;           /* Byte address in compressed dump */
                                        /* file where the next DataSegD    */
                                        /* structure resides (need not be  */
                                        /* set).                           */
        uint_64 Compressed:1,        	/* Flags indicate whether the data */
                Unused:63;              /* segment is compressed or not    */
};

/*
 * Compressed file format: Header/Trailer structure (32 64-bit words)
 */
typedef union {
        struct CmpFileHdr {
                char    FileID[8];      /* Compressed file id (characters)   */
                char    FileMagic[8];   /* Compressed file id (ASCII)        */
                char    Date[8];        /* File date                         */
                char    Time[8];        /* File time                         */
                char    TransType[8];  	/* Transformation Type               */
                uint_64 DataSegSz;      /* Original Segment size             */
                uint_64 FirstDataSegD;  /* Byte address of first DataSegD    */
                uint_64 Reserved[25];   /* Reserved for future expansion     */
        } CmpFileHdr;
 
        struct CmpFileTrl {
                char    FileID[8];      /* Compressed file id (characters)   */
                char    FileMagic[8];   /* Compressed file id (ASCII)        */
                char    Date[8];        /* File date                         */
                char    Time[8];        /* File time                         */
                uint_64 DataSegDAddr;   /* Byte address in compressed file   */
                                        /* where DataSegD cache list begins  */
                uint_64 NumDataSegD;    /* Total number of DataSegDs written */
                uint_64 ActualFileSz;   /* Actual size of compressed file    */
                uint_64 Reserved[25];   /* Reserved for future expansion     */
        } CmpFileTrl;
} CmpFile;

#define UNCMP_BUFFERS	5

/*
 * The 'UnCmpSeg' structure contains an uncompressed buffer and all the
 * information about which segment descriptor
 */
typedef struct UnCmpSeg_st {
	int	 LUcount;		/* Least Used count, 0 if unused     */
	char	*Buffer;		/* Uncompressed data buffer          */
	int	 SegNum;		/* Index into DSegD cache	     */
} UnCmpSeg;

/*
 * The 'cmp_lyr' structure is passed as the 'lyr_info' field of 
 * the 'fdinfo' structure in 'ffio.h.' This structure contains elements
 * that need to be accessed by multiple 'cmp' layer routines, and are 
 * not already provided for in the 'fdinfo' structure. 
 */
typedef struct cmp_st {
	/*
	 * These are used both for writing and
	 * reading from a compressed file.
	 */
	int	OpnFlags;		/* Are we reading or writing?	     */
	char	Trans[8];		/* Compression algorithm	     */
	int	DebugLvl;		/* Level of debug information 	     */
	int     DSegSize;		/* Data Segment size		     */	
	int	InitHeader;		/* Have we initialized header	     */
	CmpFile Header;			/* Stores header information 	     */
	CmpFile	Trailer;		/* Stores trailer information	     */
	struct  DataSegD *DSegDCache;   /* Pointer to dSegD cache            */
	int	NumDSegD;		/* Number of Data Segment Desc's     */
	int	CurrPos;		/* Compressed file position on write */
					/* User file position on read        */
	int	DbgOpenCnt;		/* ID used for each opened file	     */

	/*
	 * These are only used when writing a 
         * compressed file.
	 */
	int 	OrigFileSz;		/* Original file size		     */
	int	WroteHeader;		/* Have we written the header info   */
	char   *DSegBuf;		/* Stores current data segment	     */
	int	BytesInBuf;		/* Bytes stored for current segment  */
					/* which haven't been written yet    */
	int     Writea;			/* Doing an asynchronous write?	     */

	/*
	 * These are only used when reading 
  	 * from a compressed file.
	 */
	int	  Hit, Miss;		/* Hits and misses for segment cache */
	int	  FileSz;		/* Uncompressed file size	     */
	int	  DSegCacheSize;	/* Uncompressed data seg cache size  */
	int	  Reada;		/* Doing an asynchronous read? 	     */
	UnCmpSeg *UnCmpSegList;		/* Uncompressed segment cache        */
} cmp_lyr;
	
#define h_FileID        CmpFileHdr.FileID
#define h_FileMagic     CmpFileHdr.FileMagic
#define h_Date          CmpFileHdr.Date
#define h_Time          CmpFileHdr.Time
#define h_DataSegSz     CmpFileHdr.DataSegSz
#define h_TransType     CmpFileHdr.TransType
#define h_FirstDataSegD CmpFileHdr.FirstDataSegD
 
#define t_FileID        CmpFileTrl.FileID
#define t_FileMagic     CmpFileTrl.FileMagic
#define t_Date          CmpFileTrl.Date
#define t_Time          CmpFileTrl.Time
#define t_DataSegDAddr  CmpFileTrl.DataSegDAddr
#define t_NumDataSegD   CmpFileTrl.NumDataSegD
#define t_ActualFileSz  CmpFileTrl.ActualFileSz

extern void  _TraceEnter(char *rtnName, cmp_lyr *cinfo);
extern void  _TraceLeave(char *rtnName, cmp_lyr *cinfo);
extern void  _TraceLog(char *fmt, ...);
extern void  _free_cmp_lyr(cmp_lyr *cinfo);
extern void  _initHeader(cmp_lyr *cinfo);
extern void  _CloseTraceFile(cmp_lyr *cinfo);
extern int   _OpenTraceFile(struct ffsw *stat, cmp_lyr *cinfo);
extern char *_findInCache(struct fdinfo *llfio, struct ffsw *stat, \
			cmp_lyr *cinfo, int segNum);
extern char *_unCmpDSeg(struct fdinfo *llfio, struct ffsw *stat, \
			struct DataSegD dSegD, cmp_lyr *cinfo);
extern int   _lz_compress(cmp_lyr *cinfo, char *buf, int nread, \
			char **outbuf, int *out_size);
extern int   _lz_decompress(cmp_lyr *cinfo, char *inbuf, int nread, \
			int outsize, char **outbuf);
extern int   _writeDSeg(struct fdinfo *llfio, struct ffsw *stat, int fulp, \
			int *ubc, cmp_lyr *cinfo);
extern int   _writeTrailer(struct fdinfo *llfio, struct ffsw *stat, int fulp, \
			int *ubc, cmp_lyr *cinfo);

#endif /* __CMPIO_H_ */
