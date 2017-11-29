C
C
C  Copyright (C) 2000, 2001 Silicon Graphics, Inc.  All Rights Reserved.
C
C  This program is free software; you can redistribute it and/or modify it
C  under the terms of version 2.1 of the GNU Lesser General Public License 
C  as published by the Free Software Foundation.
C
C  This program is distributed in the hope that it would be useful, but
C  WITHOUT ANY WARRANTY; without even the implied warranty of
C  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  
C
C  Further, this software is distributed without any warranty that it is
C  free of the rightful claim of any third person regarding infringement 
C  or the like.  Any license provided herein, whether implied or 
C  otherwise, applies only to this software file.  Patent licenses, if
C  any, provided herein do not apply to combinations of this program with 
C  other software, or any other product whatsoever.  
C
C  You should have received a copy of the GNU Lesser General Public 
C  License along with this program; if not, write the Free Software 
C  Foundation, Inc., 59 Temple Place - Suite 330, Boston MA 02111-1307, 
C  USA.
C
C  Contact information:  Silicon Graphics, Inc., 1600 Amphitheatre Pky,
C  Mountain View, CA 94043, or:
C
C  http://www.sgi.com
C
C  For further information regarding this notice, see:
C
C  http://oss.sgi.com/projects/GenInfo/NoticeExplan
C
C


      subroutine opendr(unit,index,length,it,ierrf)
CDIR$ ID "@(#) libu/waio/dr.f	92.0	10/08/98 14:57:41"
      implicit none

cc       READDR/WRITDR -  cray direct random i/o
c
c        1. description:
c
c         READDR/WRITDR is a "direct" version of READMS/WRITMS.
c         This means that all i/o goes directly to/from the user
c         area from/to the disk without going through a buffer
c         in high user memory.  All record lengths are rounded up
c         to the next multiple of 512 since the disk can only be
c         addressed in even 512 word blocks.
c
c        2. impact on user code:
c
c         On WRITDR calls we just write extra user data to disk
c         if record size is less than multiple of 512.
c
c         For READDR calls involving an exact multiple of 512 words,
c         a single read request is made.  For READDR calls involving
c         fewer than 512 words, one read request is made synchronously,
c         reading the data into an auxiliary buffer; the data is
c         then moved to its final destination.  For READDR calls
c         involving more than 512 words and yet not a multiple of
c         512 words, two requests are made.  The first request reads
c         the final block into an auxiliary buffer from which it
c         is moved to the last part of the final destination.  The
c         second request is for the appropriate number of full blocks;
c         this request is made either synchronously or asynchronously,
c         depending on the current mode of the file.
c
c         If a user wishes to try these routines instead of
c         READMS/WRITMS and its paging scheme, simply add a CFT
c         subroutine at the end of fortran code to intercept
c         READMS/WRITMS calls and change to READDR/WRITDR calls.
c         The users READMS/WRITMS routines will be called rather
c         than the system ones and these will call the direct
c         rather than the word addressable ones.
c
c        3. performance considerations.
c
c         Since each user call results in either a direct disk
c         read or direct disk write the performance may be quite
c         different from buffering i/o schemes (like READMS/
c         WRITMS).  User jobs that do a lot of random rewrites
c         of large records may see a big performance jump because
c         WRITDR adjusts all records so they start and stop on
c         disk boundarys.  A rewrite of a record is then just
c         an overlaying on disk of new data and can never involve
c         reading old data from disk and merging new and old data
c         together and rewriting to disk.  Also on large records
c         the word addressable routines will not stream data but
c         will have to initiate as many reads/writes as necessary
c         to satisfy the user request based on the pages used.
c         thus if a user issues a 100 pru read (51200 words)
c         and has a 10 pru buffer it will take 10 read requests
c         to the system to satisfy.  A READDR call for a 100
c         pru read will make just one system call since i/o goes
c         directly to the user array.  Users however who have
c         very small records or sharply varying record sizes
c         may do better with READMS/WRITMS since the data will
c         be packed together with no pru pad.  Users who also
c         have random accessing that tends to zero in on one
c         large area of the file and reuse and update the data
c         several times may also do better on READMS/WRITMS with
c         large number of page buffers.
c
c        4. installation cautions.
c
c         Both the READMS/WRITMS routines and the READDR/WRITDR
c         routines make calls to the same C routines to make
c         system i/o calls. The calls to these routines use
c         indexes into tables rather than names or offsets. This
c         speeds up the code but care must be exercised in the
c         way the three pieces of code are installed.  The scheme
c         is this.  READMS/WRITMS has a default limit of G@DSNMAXW
c         active datasets and READDR/WRITDR has a default limit of
c         G@DSNMAXD active datasets.  READMS/WRITMS uses index
c         numbers 1 to G@DSNMAXW when calling the CAL routines
c         and READDR/WRITDR uses indexs G@DSNMAXW+1 to
c         G@DSNMAXW+G@DSNMAXD.  The underlying table space is
c         allocated dynamically based on these values.
c
c        5. intermixing READMS/WRITMS and READDR/WRITDR.
c
c         A user can intermix both READMS/WRITMS and READDR/WRITDR
c         datasets in the same program.  Care must be taken of course
c         to not use the same file in both packages at the same time
c         because this will produce unpredictable results.
c         Creating a file with READMS/WRITMS
c         and trying to read it with READDR/WRITDR is also a no no
c         because the indexes in the directory that READMS/WRITMS
c         builds are not on pru boundarys so the whole scheme falls
c         apart.  Files created by READDR/WRITDR can be read by READMS/
c         WRITMS but any updating by a WRITMS call could potentially
c         foul the indexes and is not recommended.
c
c
c         features:
c
c          1. named or numbered index types.
c          2. three write/rewrite options.
c                a. rewrite record in place.
c                b. write/rewrite record at eoi.
c                c. rewrite record in place if it fits, at eoi if it
c                   does not fit.
c          3. subindexs supported.
c          4. dataset unit numbers or hollerith names supported.
c          5. record index saved at the end of the dataset.
c          6. variable length records.
c          7. asynchronous access addedd.
c
c          method:
c
c            dataset must be opened by a call to opendr.  opendr checks
c          to see if the dataset exists by a call to the system to
c          obtain the last block number of the dataset.  if the dataset
c          exists then it should have an index and opendr will then read
c          the index and put it in the users index array. if the dataset
c          does not exist the index array will be zeroed out and the
c          first pru on the dataset is blocked out to save the index
c          address at close time.
c
c            accessing the dataset is then done thru 'READDR' and
c          'WRITDR' calls.  'closdr' must be called at job end if
c          the dataset has been written on to insure that the index
c          will be updated to the correct values.  if the dataset
c          is read only 'closdr' will not update the index.
c          since the i/o goes directly from the disk to the user
c          all the records are rounded up to the next multiple of
c          512. when doing synchronous i/o the area that may be
c          clobbered by the longer reads is saved out and restored
c          when the read finishes but on asynchronous calls this
c          is not done.
c
c            the READDR/WRITDR routines make calls to another package
c          to do direct disk pru type i/o.  the dataset format is
c          that of an unblocked/random dataset. the first block is
c          left empty when the file is created by opendr and when closdr
c          is called it updates the first six words with the index
c          variables and rewrites the first pru.
c
c
c          DRIO File Versions
c         
c            Three versions of DRIO files are supported.  
c
c                  Version       Description
c
c                     0          Old DRIO file.
c
c                                This file created by pre-3.0 CrayLibs.  All 
c                                records are less than 2**22 words long. 
c                                Checksum is computed from 
c                                words 1-4.  (When a version 0 numbered 
c                                index DRIO file is later reopened for 
c                                update, new records must be less than 
c                                2**22 words long.  This restriction will
c                                be lifted in a later release, probably 
c                                CrayLibs 4.0).
c
c                     1          New DRIO file with no long records.
c
c                                File created by big-record-enabled version
c                                of libu (CrayLibs 3.0.x).  All records 
c                                are less than 2**22 words long.  These 
c                                files can be read by older versions of
c                                libu.  When a version 1 DRIO file is updated,
c                                records 2**22 words long and longer are 
c                                allowed, in which case the file is promoted
c                                to a version 2 DRIO file.
c
c                     2          New DRIO file with long records.
c
c                                File has or had a record at least 2**22 words 
c                                long.  Word 8 of the file describes the 
c                                format of a record index entry.  Version 2
c                                files cannot be read by pre CrayLibs 3.0.x
c                                libraries.  A file checksum error is generated
c                                in this case.
c                                
c
c          Version 0 and 1 DRIO files have numbered indexes with the high 
c          order 22 bits containing the record length in words and the 
c          remaining 42 bits containing 1-based word offset of the record 
c          within the file.  Version 2 DRIO files split up the 64 bits in 
c          the index entry differently.  closdr generates a version 1 DRIO 
c          file whenever possible so that programs built with prior releases 
c          of libu can still read new data files.   
c          or openms try to open the file.
c
c
c          word 1 -- word address of the index record     --
c               2 -- length of the index record           --
c               3 -- date of last update                  --
c               4 -- time of last update                  --
c               5 -- chksum of wds 1-4 (plus 1 if vers>=2)--
c               6 -- 1 if created by DRIO, 0 if MSIO      --
c               7 -- DRIO file version number: 0, 1, or 2 --
c               8 -- bit length of record location field  --
c                    within an entry in the master index   
c                    if version >= 2; else 0.                 
c               : --                                      --
c               : --                                      --
c             512 --             null                     --
c             513 --        record one starts here        --
c               : --                                      --
c               : --                                      --
c             550 --        record one ends               --
c                 --        pad added                     --
c            1025 --        record two begins             --
c               : --                                      --
c               : --                                      --
c               : --                                      --
c               : --                                      --
c               : --                                      --
c            1750 --        record two ends               --
c               : --                                      --
c            4097 --        record n begins               --
c               : --                                      --
c            5188 --        record n ends                 --
c                 --        record pad.                   --
c            5633 --        index record begins           --
c               : --                                      --
c        5189+n-1 --        index record ends             --
c
c
c
cc        tables pointers variables etc.
c
c         variable *G@DSNMAXD* sets the maximum number of active datasets.
c         if the released value of 20 is inadequate then the value
c         can be changed either in the CAL routine in libu that defines
c         _dsnmax_d and _dsnmax_w, or it can be changed with a SET=
c         segldr directive (SET=_dsnmax_d:35 will raise the number
c         of DRIO datasets to 35)
c
c
c         fit - file information table.
c
c         contains information pertinent to READDR/WRITDR files.  the following
c         FIT_* constants define fields in each FIT table entry.
c

            integer    FIT_NAME            ! left justified file name
            parameter (FIT_NAME      =  1)

            integer    FIT_CINDADDR        ! address of current index array
            parameter (FIT_CINDADDR  =  2)

            integer    FIT_CINDTYPE        ! current index type:
            parameter (FIT_CINDTYPE  =  3) ! 1= named. 0= numbered.

            integer    FIT_CINDLENG        ! current index length
            parameter (FIT_CINDLENG  =  4)    

            integer    FIT_CINDLOCFLD      ! bit size of record location
            parameter (FIT_CINDLOCFLD=  5) ! field of current index entry

            integer    FIT_ACCTIME         ! access time in clock periods
            parameter (FIT_ACCTIME   =  6)

            integer    FIT_EOIWRITES       ! number of writes to eoi
            parameter (FIT_EOIWRITES =  7)

            integer    FIT_REWRITES        ! number of rewrites in place
            parameter (FIT_REWRITES  =  8)

            integer    FIT_MAXREC          ! maximum record size
            parameter (FIT_MAXREC    =  9)

            integer    FIT_MINREC          ! minimum record size
            parameter (FIT_MINREC    = 10)

            integer    FIT_SEQREADS        ! number of sequential reads
            parameter (FIT_SEQREADS  = 11)

            integer    FIT_SEQWRITES       ! number of sequential writes
            parameter (FIT_SEQWRITES = 12)

            integer    FIT_READS           ! total number of reads done
            parameter (FIT_READS     = 13)

            integer    FIT_WDSMOVED        ! total words moved to/from buffer
            parameter (FIT_WDSMOVED  = 14)

            integer    FIT_LASTWRITE       ! 1 if last op was a write
            parameter (FIT_LASTWRITE = 15)

            integer    FIT_PREVREC         ! previous record number
            parameter (FIT_PREVREC   = 16)

            integer    FIT_MINDADDR        ! address of master index array
            parameter (FIT_MINDADDR  = 17)

            integer    FIT_MINDTYPE        ! type of master index array
            parameter (FIT_MINDTYPE  = 18) ! 1= named. 0= numbered

            integer    FIT_MINDLENG        ! length of master index array
            parameter (FIT_MINDLENG  = 19) 

            integer    FIT_MINDLOCFLD      ! bit size of record location
            parameter (FIT_MINDLOCFLD= 20) ! field of master index entry

            integer    FIT_EOI             ! word address of eoi of file
            parameter (FIT_EOI       = 21)

            integer    FIT_WRFLAG          ! 1 if any write was done
            parameter (FIT_WRFLAG    = 22)

            integer    FIT_ASYNC           ! 0 if sync. 1 if asynchronous
            parameter (FIT_ASYNC     = 23)

            integer    FIT_VERS            ! file version from word 7
            parameter (FIT_VERS      = 24) 

            integer    FIT_BIGOK           ! 1 if reclen >= 2**22 is OK
            parameter (FIT_BIGOK     = 25) 

            integer    FITMAX              ! word size of FIT table
            parameter (FITMAX        = 25)


cdir$ eject
cc        opendr entry
c
c         call opendr(unit,index,length,it,ierrf)
c
c            where :   *unit*   =  file number or hollerith name to open
c                      *index*  =  array to hold file index.
c                      *length* =  length of index array.
c                      *it*     =  0 if numbered style index and synch.
c                      *it*     =  1 if named style index and synch.
c                      *it*     =  2 if numbered style index and asynch.
c                      *it*     =  3 if named style index and asynch.
c                      *ierrf*  =  optional error status return
c                                  if parameter passed, error status
c                                  will be returned to the user.  if
c                                  ierrf is not used program will abort.
c
c                                  on input, if ierrf > 0 only error
c                                  number returned, no logfile message
c                                  posted.
c
c                                  on return, if ierrf < 0, error has
c                                  been encountered.
c
c                on return index(1) thru index(length) = 0
c                if file is null, otherwise index(1) thru
c                index(length) will contain the indexs for file.
c                if the dataset opened exists the index will be
c                copied from the disk directly to the user index
c                array.  if the index length is not a multiple
c                of 512 the data that will be clobbered by the
c                record pad is read out and restored after the
c                indexs are read from disk.  the user does not
c                have to be sure his index size is a multiple of
c                512 for either asynchronous or synchronous methods.
c
c                error messages posted or returned to user:
c
c                 illegal unit number or name             ierrf = -1
c                 index length .le. 0                     ierrf = -2
c                 maximum number of datasets exceeded     ierrf = -3
c                 index length .gt. user length           ierrf = -4
c                 user index .gt. dataset index length    ierrf = -5
c                 index word address .le. zero            ierrf = -11
c                 index length .le. zero                  ierrf = -12
c                 checksum error on dataset               ierrf = -13
c                 unit already opened by opendr           ierrf = -14
c                 file created by WRITMS not WRITDR       ierrf = -20
c                 could not allocate memory               ierrf = -21
c                 unsupported DRIO file version           ierrf = -22
c
c
c         flow.
c
c         1. if illegal unit name or number - abort or return error.
c         2. check if file already open. if so abort or return error.
c         3. check if max number of datasets exceeded. if so abort
c            or return error code to user.
c         4. check index length. if .le. zero - abort or return error.
c         5. save index type, length, sync/async flag, and address.
c         6. zero the index array.
c         7. set a large minimum record so true minimum will be found.
c         8. open the file by a call to cal i/o routine.
c         9. if file does not exist blank out the first pru on the
c            disk and set the eoi address to word 513. return.
c        10. read the first pru from the disk.  .
c        11. the five words read from the dataset are checked for
c            errors. the first word is the file index word address,
c            this is checked to be sure it is not .le. zero.  the
c            second word contains the file index length, this is also
c            checked for .le. zero.  then the first four words are
c            checksumed and compared to the value in word five. next
c            the index length from the user is compared to the one
c            read from the file. lastly word six is checked to be
c            sure that this dataset was not created by WRITMS rather
c            than WRITDR.
c        12. check user index word count to be sure it is a multiple
c            of 512. if not copy out data that will be overwritten
c            by the index read.
c        13. read in the index.  if data was copied out above restore
c            it to the user array.  set the eoi address to the beginning
c            address of the index.  return.
c
c

      external   abort

c     Constants

      integer    WORDSZ
      parameter (WORDSZ=64)       ! word size in bits

      integer    DISKBLK
      parameter (DISKBLK=512)      ! file block size in words

      integer    signbit
      parameter (signbit=1000000000000000000000b)

      integer    DEF_INDLOCFLD
      parameter (DEF_INDLOCFLD=42)


c     Variables

      real       avat
      external   chkunit
      external   closewa
      external   drio0dsn
      character*8 drio0dsn
      character*8 ermsg
      integer    G@DSNMAXW
      common    /G@DSNMAXW/G@DSNMAXW
      integer    G@DSNMAXD
      common    /G@DSNMAXD/G@DSNMAXD
      logical    eoi
      integer    fit(FITMAX,1)
      integer    fit2(1)
      integer    fullblks
      integer    i
      integer    I@CLKTCK           
      integer    iabort             !abort flag, set to 1 for abort
      integer    iaddr
      integer    iasync
      integer    icheck
      integer    ichk
      integer    filechksum
      integer    idn
      integer    idxusr(1)
      integer    ieoi
      integer    ieoia
      integer    ierr
      integer    ierrf
      integer    ifilem
      logical    iflag
      integer    ifwa(1)
      integer    iloff
      integer    ilinc
      integer    imaxr
      integer    iminr
      integer    ind
      integer    index(1)
      integer    indexl
      integer    iread
      integer    irwip
      integer    isc(DISKBLK)        ! disk block buffer
      integer    isc2(DISKBLK)       ! disk block buffer
      integer    iseqr
      integer    iseqw
      integer    a@isrcheq          ! MPP does not have isrcheq yet
      integer    istat
      integer    isync
      integer    it
      integer    itime              ! time required for I/O operation
      integer    itold
      integer    itotw
      integer    it0                ! temporary used in calculating 'iti
      integer    it1                ! temporary used in calculating 'iti
      integer    iupdat1
      integer    iweoi
      integer    iwrit
      integer    j
      integer    k                  ! Local copy of record name
      integer    kk                 ! User's record name parameter
      integer    leng
      integer    length
      integer    lra
      integer    maxrec
      integer    maxreclen
      integer    mess
      integer    minrec
      logical    message
      integer    msg
      integer    rln		    ! rounded nnn
      integer    nn
      integer    nnn
      external   openwa
      integer    r                  ! Local copy of rewrite control flag
      external   readwa
      external   remark
      integer    residue
      integer    rnb
      integer    rlenbits
      integer    rlocbits
      external   rnb
      integer    rr                 ! User's rewrite flag parameter
      integer    s
      integer    sector
      real       tota
      integer    unit
      integer    unitn              ! internal unit number
      integer    unitnn             ! internal unit offset by *G@DSNMAXW*
      external   wdset
      external   writewa
      external   wunit

      pointer (fitptr,fit)
      pointer (fitptr2,fit2)
      pointer (idxptr,idxusr)
      data fitptr/0/

c     Statement Functions

      integer maskr
      maskr(i) = mask(128-i)


c     convert user supplied thing to ascii name

      iabort = 1
      message = .true.
      if (numarg().eq.5) then
          iabort = 0
          message = ierrf .le. 0
          ierrf = 0
      endif

c     Make sure that fit gets allocated

      if (fitptr .eq. 0) then
        call hpalloc(fitptr, FITMAX*G@DSNMAXD, ierr, 0)
        if(ierr.ne.0) then
          fitptr=0
          if (message) then
            assign 9320 to msg
            write(102,msg)ierr          ! Unable to allocate memory
          endif
          if (iabort.eq.1) call abort
          ierrf = -21
          return
        endif
        fitptr2 = fitptr
        do 5 i=1,FITMAX*G@DSNMAXD
          fit2(i) = 0
    5   continue
      endif

      call wdset(unit,idn)

      if ((idn.eq.0) .or. (idn.eq.1)) then
          if (message) then
              assign 9300 to msg
              write(102,msg)        ! Illegal unit number or name
          endif
          if (iabort.eq.1) call abort
          ierrf = -1
          return
      endif

      do 10 i = 1,G@DSNMAXD
          if (fit(FIT_NAME,i).eq.idn) then
              if (message) then
                  assign 9180 to msg ! Unit already opened
                  ermsg = drio0dsn(fit(FIT_NAME,i))
                  write(102,msg)ermsg
              endif
              if (iabort.eq.1) call abort
              ierrf = -14
              return
          endif

          if (fit(FIT_NAME,i).eq.0) then
              unitn = i

c             zero out the FIT table for this file

              do k = 1,FITMAX
                 fit(k,unitn) = 0
              enddo

              fit(FIT_NAME,unitn) = idn
              unitnn = i + G@DSNMAXW
              go to 20
          endif

   10 continue

c     All available *fit* slots are filled.  Abort with
c     message 'DR003 - MAXIMUM NUMBER OF DATASETS EXCEEDED'

      if (message) then
        assign 9310 to msg
        write(102,msg)
      endif
      if (iabort.eq.1) call abort
      ierrf = -3
      return
   20 continue
cdir$ eject
      indexl = length
      if (length.le.0) then
          if (message) then
              assign 9050 to msg ! index length .le. 0
              ermsg=drio0dsn(fit(FIT_NAME,unitn))
              write(102,msg)ermsg
          endif
          if (iabort.eq.1) call abort
          ierrf = -2
          return
      endif

c     now save index type, index length, and index address as the current index

      fit(FIT_CINDTYPE,unitn) = it .and. 1
      fit(FIT_CINDLENG,unitn)  = indexl
      fit(FIT_CINDADDR,unitn) = loc(index)
      fit(FIT_CINDLOCFLD,unitn) = DEF_INDLOCFLD           ! default value


c     this is also the master index

      fit(FIT_MINDTYPE,unitn) = fit(FIT_CINDTYPE,unitn)
      fit(FIT_MINDLENG,unitn) = fit(FIT_CINDLENG,unitn)
      fit(FIT_MINDADDR,unitn) = fit(FIT_CINDADDR,unitn)
      fit(FIT_MINDLOCFLD,unitn) = fit(FIT_CINDLOCFLD,unitn) 

c     zero the user index array

      do i = 1,indexl
          index(i) = 0
      enddo

c     set a large minimum record so we find the true min.

      fit(FIT_ASYNC,unitn) = shiftr(it,1)
      fit(FIT_MINREC,unitn) = 777777777777777777777b

c     zero the isc() array

      isc = 0

c     open the dataset and see if it has information on it.

c     'Iaddr' is not set or used if the size parameter is zero.
      call openwa(idn,unitnn,ieoi,iaddr,0,sector)
      if (message) then
        assign 9270 to mess
        ermsg=drio0dsn(fit(FIT_NAME,unitn))
        write(102,mess)ermsg,ieoi
      endif
      fit(FIT_BIGOK,unitn) = 1
      if (ieoi.le.0) then

c         new file.  blank out the first sector, set the eoi address, and 
c         then return.

          fit(FIT_VERS,unitn) = 1            ! new files start as version 1
          fit(FIT_BIGOK,unitn) = 1           ! big records OK on new files

          call writewa(unitnn,isc,1,DISKBLK,0)
          sector = max(DISKBLK, sector)
          fit(FIT_EOI,unitn) = sector+1
          return
      endif

c     file exists. now read in the first pru which contains the
c     pru address of the index, the length of the index, the
c     date of the last update, the time of the last update, a
c     checksum of the first 4 words, and a flag indicating if
c     this file was created by WRITMS or WRITDR.

      call readwa(unitnn,isc,1,DISKBLK,0)
c
      leng = indexl

c     if no error than file exists.  read in index if everything checks.

      fit(FIT_VERS,unitn) = isc(7)

      fit(FIT_BIGOK,unitn) = 1           ! big records are OK on new files
c******
c******
c     NOTE: At approximately CrayLibs 4.0, remove the following assignment
c     of fit(FIT_BIGOK,unitn) to 0.  At that release, about 18 months after
c     the first release of the extensible DRIO file format, it will be OK
c     to update old DRIO files with longer records.  Prior to that time,
c     old DRIO files will not permit records >= 2**22 words in length.
c     This was requested by EDF.

      if (fit(FIT_VERS,unitn) .eq. 0) then
        fit(FIT_BIGOK,unitn) = 0	! "big" is >= 2**22 words
      endif
c******
c******

      if (isc(1).le.0) then
          if (message) then
              assign 9150 to msg ! index word address <= 0
              ermsg = drio0dsn(fit(FIT_NAME,unitn))
              write(102,msg)ermsg
              assign 9210 to msg
              write(102,msg)ermsg
          endif
          if (iabort.eq.1) call abort
          ierrf = -11
          return
      endif

      if (isc(2).le.0) then
          if (message) then
              assign 9160 to msg ! index length <= 0
              ermsg = drio0dsn(fit(FIT_NAME,unitn))
              write(102,msg)ermsg
              assign 9210 to msg
              write(102,msg)ermsg
          endif
          if (iabort.eq.1) call abort
          ierrf = -12
          return
      endif

c     check the checksum

      filechksum = isc(5)

      ichk = 0
      do i = 1,4
          ichk = ichk + shiftr(isc(i),16)
      enddo
      if (fit(FIT_VERS,unitn).ge.2) then
          ichk = ichk + 1
      endif

      if (ichk.ne.filechksum) then
          if (message) then
              assign 9170 to msg ! checksum error on dataset
              ermsg = drio0dsn(fit(FIT_NAME,unitn))
              write(102,msg)ermsg
              assign 9210 to msg
              write (102,msg)ermsg
          endif
          if (iabort.eq.1) call abort
          ierrf = -13
          return
      endif

c     check for an unsupported DRIO file version

      if (fit(FIT_VERS,unitn) .gt. 2) then
          if (message) then
              write (102,9172) drio0dsn(fit(FIT_NAME,unitn))
              write (102,9173) fit(FIT_VERS,unitn)
          endif
          if (iabort.eq.1) call abort
          ierrf = -22
          return
      endif


      if (isc(2).gt.indexl) then
          if (message) then
c             'dataset index length > user index length'
              ermsg = drio0dsn(fit(FIT_NAME,unitn))
              assign 9060 to msg
              write (102,msg)ermsg
              assign 9070 to msg
              write(102,msg)isc(2),length
          endif
          if (iabort.eq.0) ierrf = -4
      endif

      if (indexl.gt.isc(2)) then
          if (message) then
c             'user index length .gt. dataset index length'
              assign 9080 to msg
              ermsg = drio0dsn(fit(FIT_NAME,unitn))
              write(102,msg)ermsg
              assign 9070 to msg
              write(102,msg)isc(2),length
          endif
          if (iabort.eq.0) ierrf = -5
c         set the length to the shorter length from the dataset.
          leng = isc(2)
      endif

      if (isc(6).eq.0) then

c         error ' dataset created by READMS/WRITMS not READDR/WRITDR'
c         this is unsafe condition because the indexes created by
c         READMS/WRITMS wont line up on pru boundarys as is required
c         by READDR/WRITDR.

          if (message) then
              assign 9260 to msg
              ermsg = drio0dsn(fit(FIT_NAME,unitn))
              write(102,msg)ermsg
          endif
          if (iabort.eq.1) call abort
          ierrf = -20
          return
      endif

c     read the actual index in here.

      if (message) then
        assign 9220 to msg
        write(102,msg)isc(3),isc(4),isc(2)+isc(1),isc(2)
      endif
c     calculate the number of full blocks
      fullblks = leng/DISKBLK
c     check for a residue
      residue = leng - (fullblks * DISKBLK)
c     open the dataset for queued i/o
      if (residue .eq. 0) then
        call readwa(unitnn,index,isc(1),fullblks*DISKBLK,0)
      else
        if (fullblks .gt. 0) then
          call readwa(unitnn,index,isc(1),fullblks*DISKBLK,0)
        endif
        call readwa(unitnn,isc2(1),isc(1)+fullblks*DISKBLK,DISKBLK,0)
        j = fullblks * DISKBLK
        do 60 i = 1,residue
          index(j+i) = isc2(i)
   60   continue
      endif

c     read the bit size of the record location field in each index entry.
c
c                   |<------------------- 64 bits ------------------>|
c                   |                |                               |
c                   | record length  |       record location         |
c                   |                |                               |
c                   |                |<- fit(FIT_CINDLOCFLD,unitn) ->|
c

      if (fit(FIT_VERS,unitn).ge.2) then
          fit(FIT_CINDLOCFLD,unitn) = isc(8)
          fit(FIT_MINDLOCFLD,unitn) = isc(8)
      endif

      fit(FIT_EOI,unitn) = isc(1) 
      fit(FIT_WRFLAG,unitn) = 0
      return

cdir$ eject
c     c    stindr entry.
c
c     change the index to a subindex.
c
c     call stindr(unit,index,length,it,ierrf)
c
c     where:   unit = unit number or name to change.
c             index = new index address.
c            length = length of index array.
c               it  =  0 if numbered style index and synch.
c               it  =  1 if named style index and synch.
c               it  =  2 if numbered style index and asynch.
c               it  =  3 if named style index and asynch.
c            ierrf  =  optional error status return
c                      if parameter passed, error status
c                      will be returned to the user.  if
c                      ierrf is not used program will abort.
c
c                      on input, if ierrf > 0 only error
c                      number returned, no logfile message
c                      posted.
c
c                      on return, if ierrf < 0, error has
c                      been encountered.
c
c        error messages:
c
c           illegal unit number or name            ierrf = -1
c           opendr not called                      ierrf = -15
c           stindr call cannot change index type  ierrf = -16
c
c
c     flow:
c
c     1.  check unit name/number. if error abort.
c     2.  if unit not opened by opendr abort.
c     3.  check if new type matches old type. if not abort.
c     4.  if first subindex call move master index save area.
c     5.  save new index info as primary index info.
c     6.  exit.
c

      entry stindr(unit,index,length,it,ierrf)

      iabort = 1
      message = .true.
      if (numarg().eq.5) then
          iabort = 0
          message = ierrf .le. 0
          ierrf = 0
      endif

      call wdset(unit,idn)
      if (idn.eq.0) then
          if (message) then
            assign 9300 to msg
            write(102,msg)
          endif
          if (iabort.eq.1) call abort
          ierrf = -1
          return
      endif

      unitn = 0
      do 80 i = 1,G@DSNMAXD
          if (fit(FIT_NAME,i).eq.idn) then
              unitn = i
              unitnn = i + G@DSNMAXW
          endif
   80 continue

      if (unitn.eq.0) then
          if (message) then
              ermsg = drio0dsn(fit(FIT_NAME,unitn))
              assign 9190 to msg ! OPENDR not called
              write(102,msg)ermsg
          endif
          if (iabort.eq.1) call abort
          ierrf = -15
          return
      endif

      itold = fit(FIT_CINDTYPE,unitn)
      if (itold.ne.mod(it,2)) then
          if (message) then
              ermsg = drio0dsn(fit(FIT_NAME,unitn))
              assign 9200 to msg ! Cannot change index type
              write(102,msg)ermsg
          endif
          if (iabort.eq.1) call abort
          ierrf = -16
          return
      endif

      fit(FIT_CINDTYPE,unitn) = it .and. 1
      fit(FIT_CINDLENG,unitn)  = length
      fit(FIT_CINDADDR,unitn) = loc(index)
      if (fit(FIT_CINDADDR,unitn) .eq. fit(FIT_MINDADDR,unitn)) then
          fit(FIT_CINDLOCFLD,unitn) = fit(FIT_MINDLOCFLD,unitn)
      else
          fit(FIT_CINDLOCFLD,unitn) = DEF_INDLOCFLD    ! subindex always has 
                                                       ! default bit location
      endif

c     now set new sync/async flag.

      fit(FIT_ASYNC,unitn) = shiftr(it,1)
      return
cdir$ eject
c     c    checkdr/waitdr entry.
c
c     checkdr.
c
c     check the status of file transfer.
c
c     call checkdr(unit,istat,ierrf)
c
c     where:   unit = unit number or name to check.
c             istat = status passed back to user.
c                     if istat = 0 there is no file activity.
c                     if istat = 1 there is file activity.
c                     (no recall is done this is a check.)
c            ierrf  =  optional error status return
c                      if parameter passed, error status
c                      will be returned to the user.  if
c                      ierrf is not used program will abort.
c
c                      on input, if ierrf > 0 only error
c                      number returned, no logfile message
c                      posted.
c
c                      on return, if ierrf < 0, error has
c                      been encountered.
c
c        error messages:
c
c           illegal unit number or name       ierrf = -1
c           opendr non called                 ierrf = -15
c
c
c     waitdr.
c
c     wait for i/o quiet on file.
c
c     call waitdr(unit,istat,ierrf)
c
c     where:   unit = unit number or name to wait for i/o quiet.
c             istat = status passed back to user.
c                     if istat = 0 there was no file error.
c                     if istat = 1 there was an error.
c            ierrf  = optional error status return
c                     if parameter passed, error status
c                     will be returned to the user.  if
c                     ierrf is not used program will abort.
c
c                     on input, if ierrf > 0 only error
c                     number returned, no logfile message
c                     posted.
c
c                     on return, if ierrf < 0, error has
c                     been encountered.
c
c        error messages:
c
c
c           illegal unit number or name       ierrf = -1
c           opendr non called                 ierrf = -15
c
c     flow:
c
c     1.  record entry. check unit name/number. if error abort.
c     2.  if unit not opened by opendr abort.
c     3.  check file activity and set istat accordingly.
c     4.  exit.
c

      entry checkdr(unit,istat,ierrf)

      icheck = 1
      go to 90

      entry waitdr(unit,istat,ierrf)

      icheck = 0
   90 continue
      iabort = 1
      message = .true.
      if (numarg().eq.3) then
          iabort = 0
          message = ierrf .le. 0
          ierrf = 0
      endif

      call wdset(unit,idn)
      if (idn.eq.0) then
          if (message) then
            assign 9300 to msg
            write(102,msg)
          endif
          if (iabort.eq.1) call abort
          ierrf = -1
          return
      endif

      unitn = 0
      do 100 i = 1,G@DSNMAXD
          if (fit(FIT_NAME,i).eq.idn) then
              unitn = i
              unitnn = i + G@DSNMAXW
          endif
  100 continue
      if (unitn.eq.0) then
          if (message) then
              assign 9190 to msg
              write(102,msg)idn
          endif
          if (iabort.eq.1) call abort
          ierrf = -15
          return
      endif

      istat = 0
      if (icheck.eq.0) then

c         call the wait routine to wait for i/o quiet.

          call wunit(unitnn)

      else

c         now call cal routine to see if file is active.

          call chkunit(unitnn,j)
          if (j.eq.0) istat = 1
      endif

      return
cdir$ eject
cc    asyncdr/syncdr entry.
c
c     asyncdr.
c
c     set the access mode to asynchronous.
c
c     call asyncdr(unit,ierrf)
c
c     where:   unit = unit number or name to change.
c             ierrf = optional error status return
c                     if parameter passed, error status
c                     will be returned to the user.  if
c                     ierrf is not used program will abort.
c
c                     on input, if ierrf > 0 only error
c                     number returned, no logfile message
c                     posted.
c
c                     on return, if ierrf < 0, error has
c                     been encountered.
c
c        error messages:
c
c
c           illegal unit number or name       ierrf = -1
c           opendr non called                 ierrf = -15
c
c     syncdr.
c
c     set the access mode to synchronous.
c
c     call syncdr(unit,ierrf)
c
c     where:   unit = unit number or name to change.
c             ierrf = optional error status return
c                     if parameter passed, error status
c                     will be returned to the user.  if
c                     ierrf is not used program will abort.
c
c                     on input, if ierrf > 0 only error
c                     number returned, no logfile message
c                     posted.
c
c                     on return, if ierrf < 0, error has
c                     been encountered.
c
c        error messages:
c
c           illegal unit number or name       ierrf = -1
c           opendr non called                 ierrf = -15
c
c     flow:
c
c     1.  record entry. check unit name/number. if error abort.
c     2.  if unit not opened by opendr abort.
c     3.  set new mode in the fit table.
c     4.  exit.
c

      entry asyncdr(unit,ierrf)

      iasync = 1
      go to 110

      entry syncdr(unit,ierrf)

      iasync = 0
  110 continue
      iabort = 1
      message = .true.
      if (numarg().eq.2) then
          iabort = 0
          message = ierrf .le. 0
          ierrf = 0
      endif

      call wdset(unit,idn)
      if (idn.eq.0) then
          if (message) then
            assign 9300 to msg
            write(102,msg)
          endif
          if (iabort.eq.1) call abort
          ierrf = -1
          return
      endif

      unitn = 0
      do 120 i = 1,G@DSNMAXD
          if (fit(FIT_NAME,i).eq.idn) then
              unitn = i
              unitnn = i + G@DSNMAXW
          endif
  120 continue

      if (unitn.eq.0) then
          if (message) then
              ermsg = drio0dsn(fit(FIT_NAME,unitn))
              assign 9190 to msg
              write(102,msg)ermsg
          endif
          if (iabort.eq.1) call abort
          ierrf = -15
          return
      endif

c     now set new status in the fit table.

      fit(FIT_ASYNC,unitn) = iasync
      return
cdir$ eject
cc    WRITDR entry
c
c      call WRITDR(unit,ifwa,nnn,kk,rr,s,ierrf)
c
c      where:   unit = unit number 1 to 99, or dataset name.
c               ifwa = source array to write from.
c                nnn = number of words to write. this will be
c                      rounded up to next multiple of 512 and
c                      this will be the new record size saved
c                      in the index area.  thus if a user writes
c                      a 5 word record and later rewrites with
c                      a record 10 words long this can be done
c                      in place because WRITDR really called it
c                      a 512 word record.
c                 kk = record number or 64 bit named record.
c                 rr = rewrite control flag.
c                      if rr = 0 - rewrite the record at eoi.(default)
c                      if rr = 1 - rewrite the record in place if it
c                                  fits. abort if it does not fit.
c                      if rr =-1 - rewrite the record in place if it
c                                  fits, or rewrite it at eoi if it
c                                  does not fit.
c                  s = subindex flag.  (deferred implementation)
c
c              ierrf =  optional error status return
c                       if parameter passed, error status
c                       will be returned to the user.  if
c                       ierrf is not used program will abort.
c
c                       on input, if ierrf > 0 only error
c                       number returned, no logfile message
c                       posted.
c
c                       on return, if ierrf < 0, error has
c                       been encountered.
c
c           illegal unit number or name             ierrf = -1
c           illegal named index                     ierrf = -6
c           record index array full                 ierrf = -7
c           index number .gt. maximum               ierrf = -8
c           rewrite record exceeds original         ierrf = -9
c           opendr not called                       ierrf = -15
c           index offset .le. 0                     ierrf = -19
c
c     flow:
c
c     1. round word count up to next multiple of DISKBLK.      .
c        if illegal dataset name or opendr not called abort.
c     2. set write = .true. for this unit. the index will be updated
c        when closdr is called for this dataset.
c     3. compute index address for this dataset.
c     4. check if this is a named or numbered index dataset.
c     5. if numbered skip to 12.
c     6. named index.  if index = +0 or -0  -  abort.
c     7. search index array for match. if found set length and
c        record address.  go to 11.
c     8. if zero entry found we are at the end of the index entries,
c        so put name here and skip to 11.
c     9. loop back to 7 if not to end.
c     10. index array overflow. -  abort.
c     11. skip to 14
c     12. if index number .le. 0 - abort. if record number .gt. maximum,
c        index array overflow - abort.
c     13. set word address and record length from index array.
c     14. if rewrite flag .eq. -1 and record length less than or equal
c        the old record length set flag to rewrite in place.  if record
c        does not fit set rewrite to eoi flag.
c     15. if rewrite flag .eq. 1 set the rewrite in place flag and then
c        check if the record will fit. if not issue msg and abort.
c     16. if rewrite flag .ne. 1 or rewrite flag .ne. -1, set rewrite
c        flag to 0 (the default) to signal write to eoi.
c     17. if eoi flag true (default value) then write at eoi update
c        the index entry and increase the eoi address for this dataset.
c     18. if eoi flag false then rewrite the record in place.
c     19. update file statistics and then return.
c


      entry WRITDR(unit,ifwa,nnn,kk,rr,s,ierrf)

      iabort = 1
      message = .true.
      if (numarg().eq.7) then
          iabort = 0
          message = ierrf .le. 0
          ierrf = 0
      endif

      itime = 0
      call wdset(unit,idn)
      if (idn.eq.0) then
          if (message) then
              assign 9300 to msg
              write(102,msg)
          endif
          if (iabort.eq.1) call abort
          ierrf = -1
          return
      endif

c     find unitn

      unitn = 0
      do i = 1,G@DSNMAXD
          if (fit(FIT_NAME,i).eq.idn) then
              unitn = i
              unitnn = i + G@DSNMAXW
          endif
      enddo

c     Round number of words up to multiple of *DISKBLK*

      rln = ((nnn+(DISKBLK-1))/DISKBLK)*DISKBLK

c     named indexes have len/loc in the even words of the index

      if (fit(FIT_CINDTYPE,unitn) .eq. 1) then
           iloff = 2
           ilinc = 2
      else
           iloff = 1
           ilinc = 1
      endif
      idxptr = fit(FIT_CINDADDR,unitn)

c     Check for record too long

      rlenbits = WORDSZ - fit(FIT_CINDLOCFLD,unitn)
      rlocbits = fit(FIT_CINDLOCFLD,unitn)

      maxreclen = shiftl(1, rlenbits) - 1

      if (rln .gt. maxreclen) then
        if (fit(FIT_CINDADDR,unitn) .eq. fit(FIT_MINDADDR,unitn) .and.
     &    fit(FIT_BIGOK,unitn) .eq. 1) then

c         if the master index is being used,
c         dynamically reconfigure the index format to allow the larger record
c         size.   note that a record length beyond 2**55-1 is impossible because
c         that would leave fewer than 9 bits for the record location field.
c
c         compute the number of bits to store the record location "rlocbits"
c         such that the current EOI fits.  Then compute the number of
c         bits needed to store the record length "rlenbits".  Verify that
c         rlocbits + rlenbits <= 64.

          ieoi = fit(FIT_EOI,unitn) 

c         find a lower bound for rec location bit field size

          rlocbits = 9                   ! minimum bits for rec location
          do while (shiftl(1,rlocbits)-1.lt.ieoi .and. rlocbits.lt.64)
              rlocbits = rlocbits + 1
          enddo

c         grow rec length bit field size until current reclen fits.

          do while (rln.gt.maxreclen .and. rlenbits.le.WORDSZ-rlocbits) 
	      rlenbits = rlenbits + 1
              maxreclen = shiftl(1, rlenbits) - 1
          enddo

c         final rec location bit field stays as large as possible while
c         still accomidating the growing rec size bit field

          rlocbits = WORDSZ-rlenbits
        endif
      endif

      if (rln .gt. maxreclen) then
          if (message) then
              write (102, *) 
     &'DR026 - THE ROUNDED RECORD SIZE OF ', rln,
     &' WORDS EXCEEDS THE MAXIMUM'
              write (102, *)
     &'        POSSIBLE SIZE OF 2**',rlenbits, '-1 WORDS.'

	      if (fit(FIT_BIGOK,unitn) .eq. 0) then
                  write (102, *) 
     &'        THIS HARD LIMIT CANNOT BE EXCEEDED FOR VERSION ',
     &  fit(FIT_VERS,unitn),' DRIO FILES.'
               endif

          endif
          if (iabort.eq.1) call abort
          ierrf = -26                ! requested record length exceeds max
          return
      endif

      if (rlocbits .ne.  fit(FIT_CINDLOCFLD,unitn)) then

c       if the index must be reconfigured,
c       check for any location field in the index which could not be 
c       compressed to the smaller number of bits (rlocbits) now allowed in
c       each index entry

        do i=iloff, fit(FIT_CINDLENG, unitn), ilinc
            lra = idxusr(i) .and. maskr(fit(FIT_CINDLOCFLD,unitn))
	    if (lra .ge. shiftl(1,rlocbits)) then
              if (message) then
                write (102, *) 
     &'DR027 - THE ROUNDED RECORD SIZE OF ',rln, ' WORDS EXCEEDS THE'
                write (102, *) 
     &'        MAXIMUM POSSIBLE FOR A FILE OF THIS SIZE.  THE MAXIMUM'
                write (102, *) 
     &'        RECORD SIZE FOR THIS FILE IS 2**',rlenbits,'-1 WORDS.'
              endif
              if (iabort.eq.1) call abort
              ierrf = -27                ! requested record length exceeds max
              return
            endif
        enddo

c       adjust the user index array to the new index format

        do i=iloff, fit(FIT_CINDLENG, unitn), ilinc
                leng = shiftr(idxusr(i),fit(FIT_CINDLOCFLD,unitn))
                lra = idxusr(i) .and. maskr(fit(FIT_CINDLOCFLD,unitn))
                idxusr(i) = shiftl(leng,WORDSZ-rlenbits) .or. lra
        enddo
        fit(FIT_CINDLOCFLD,unitn) = WORDSZ - rlenbits
        fit(FIT_MINDLOCFLD,unitn) = WORDSZ - rlenbits
        fit(FIT_VERS,unitn) = 2             ! promote it to version 2
      endif

      if (unitn.eq.0) then
          if (message) then
              assign 9190 to msg ! not opened
              write(102,msg)idn
          endif
          if (iabort.eq.1) call abort
          ierrf = -15
          return
      endif

c     set write flag so index will be rewritten at closedr time

      fit(FIT_WRFLAG,unitn) = 1
      k = kk ! Record identifier
      r = 0
      if (numarg() .ge. 5) r = rr
      indexl = fit(FIT_CINDLENG,unitn)
      ieoi = fit(FIT_EOI,unitn) 
      isync = fit(FIT_ASYNC,unitn)

c     How will file be referenced (name or number?)

      if (fit(FIT_CINDTYPE,unitn) .eq. 1) then

c         file will be referenced thru named index.
c         start of the named index code.

          if (k.eq.0) then
              if (message) then
                  ermsg = drio0dsn(fit(FIT_NAME,unitn))
                  assign 9090 to msg ! illegal named index
                  write(102,msg)ermsg
              endif
              if (iabort.eq.1) call abort
              ierrf = -6
              return
          endif

c         search index array for a matching name.

          j = indexl/2
          i = a@isrcheq(j,idxusr(1),2,k)
          if (i.gt.j) then
c           no match was found, look for a free slot
            i = a@isrcheq(j,idxusr(1),2,0)
            if (i.gt.j) then
c             name table overflow
              if (message) then
                ermsg = drio0dsn(fit(FIT_NAME,unitn))
                assign 9100 to msg
                write(102,msg)ermsg
              endif
              if (iabort.eq.1) call abort
              ierrf = -7
              return
            else
              it0 = 2*i - 1
              idxusr(it0) = k
              idxusr(it0+1) = 0
            endif
          endif
          i = 2*i - 1
          k = i + 1
          lra = idxusr(k) .and. maskr(fit(FIT_CINDLOCFLD,unitn))
          leng = shiftr(idxusr(k),fit(FIT_CINDLOCFLD,unitn))

c         end of named index code
      else
c         start of code for numbered index random file.

          if (k.le.0) then
              if (message) then
                  ermsg = drio0dsn(fit(FIT_NAME,unitn))
                  assign 9250 to msg
                  write(102,msg)ermsg
              endif
              if (iabort.eq.1) call abort
              ierrf = -19
              return
          endif

          if (k.gt.indexl) then
              if (message) then
                  ermsg = drio0dsn(fit(FIT_NAME,unitn))
                  assign 9110 to msg
                  write(102,msg)ermsg
              endif
              if (iabort.eq.1) call abort
              ierrf = -8
              return
          endif

          lra = idxusr(k) .and. maskr(fit(FIT_CINDLOCFLD,unitn))
          leng = shiftr(idxusr(k),fit(FIT_CINDLOCFLD,unitn))

c         end of numbered index code
      endif

c     now evaluate the rewrite in place paramater.

      if (r.eq.-1) then
c         if r = -1 we will rewrite in place if record fits, otherwise
c         write to eoi.
          eoi = rln .gt. leng
      else if (r.eq.1) then
c         if r = 1, rewrite in place if it fits, abort if it does not.
          eoi = .FALSE.
          if (rln.gt.leng) then
              if (message) then
                  ermsg = drio0dsn(fit(FIT_NAME,unitn))
                  assign 9120 to msg
                  write(102,msg)ermsg
                  assign 9130 to msg
                  write(102,msg)leng,rln
              endif
              if (iabort.eq.1) call abort
              ierrf = -9
              return
          endif
      else
        eoi = .TRUE.
      endif

      if (rln.le.0) then
          if (message) then
              ermsg = drio0dsn(fit(FIT_NAME,unitn))
              assign 9240 to msg
              write(102,msg)ermsg,k
          endif
          if (iabort.eq.1) call abort
          ierrf = -18
          return
      endif

      fullblks = nnn/DISKBLK
      residue  = nnn - fullblks*DISKBLK
      if (eoi) then

c         write the record at *eoi*

          iupdat1 = shiftl(1,28)
          if (ieoi.le.0) then
              if (message) then
                  ermsg = drio0dsn(fit(FIT_NAME,unitn))
                  assign 9230 to msg
                  write(102,msg)ermsg,k
              endif
              if (iabort.eq.1) call abort
              ierrf = -17
              return
          endif

          if (residue .eq. 0) then
            it1 = irtc()
            call writewa(unitnn,ifwa,ieoi,rln,isync)
            itime = irtc() - it1
          else
C           One must write out the stuff in proper order to avoid
C           a security violation.
            if (fullblks .gt. 0) then
              it1 = irtc()
              call writewa(unitnn,ifwa,ieoi,fullblks*DISKBLK,0)
              itime = irtc()  -  it1
            endif
            it1 = irtc()
            do 140 i = 1,residue
              isc2(i) = ifwa(fullblks*DISKBLK+i)
  140       continue
            call writewa(unitnn,isc2,ieoi+fullblks*DISKBLK,DISKBLK,0)
            itime = itime + (irtc() - it1)
          endif

c         update user's index

	  if (ieoi .gt. 2**fit(FIT_CINDLOCFLD,unitn) - 1) then
              if (message) then
                  ermsg = drio0dsn(fit(FIT_NAME,unitn))
                  assign 9230 to msg
                  write(102,msg)ermsg,k
              endif
              if (iabort.eq.1) call abort
              ierrf = -17
              return
          endif
          idxusr(k) = (shiftl(rln,fit(FIT_CINDLOCFLD,unitn))) + ieoi 
          ieoi = ieoi + rln
          fit(FIT_EOI,unitn) = ieoi
      else
c         rewrite in place.
          iupdat1 = shiftl(1,46)
          if (lra.le.0) then
              if (message) then
                  ermsg = drio0dsn(fit(FIT_NAME,unitn))
                  assign 9230 to msg
                  write(102,msg)ermsg,k
              endif
              if (iabort.eq.1) call abort
              ierrf = -17
              return
          endif
          if (residue .eq. 0) then
            it1 = irtc()
            call writewa(unitnn,ifwa,lra,rln,isync)
            itime = irtc() - it1
          else
C           When we rewrite in place, it is OK to write out the rump
C           first.  This allows the longer part (full blocks) to be
C           written out asynchronously.
            it1 = irtc()
            do 150 i = 1,residue
              isc2(i) = ifwa(fullblks*DISKBLK+i)
  150       continue
            call writewa(unitnn,isc2,lra+fullblks*DISKBLK,DISKBLK,0)
            itime = irtc() - it1
            if (fullblks .gt. 0) then
              it1 = irtc()
              call writewa(unitnn,ifwa,lra,fullblks*DISKBLK,isync)
              itime = itime + (irtc() - it1)
            endif
          endif
      endif

c     update statistics

      maxrec = fit(FIT_MAXREC,unitn) 
      minrec = fit(FIT_MINREC,unitn) 
      if (rln.gt.maxrec) fit(FIT_MAXREC,unitn) = rln
      if (rln.lt.maxrec) fit(FIT_MINREC,unitn) = rln
      fit(FIT_ACCTIME,unitn) = fit(FIT_ACCTIME,unitn) + itime
      fit(FIT_REWRITES,unitn) = fit(FIT_REWRITES,unitn) + 1
      if (fit(FIT_LASTWRITE,unitn).eq.1) then
          if (fit(FIT_PREVREC,unitn)+1.eq.k)  then
              fit(FIT_SEQWRITES,unitn) = fit(FIT_SEQWRITES,unitn) + 1
          endif
      endif

      fit(FIT_WDSMOVED,unitn) = fit(FIT_WDSMOVED,unitn) + rln
      fit(FIT_PREVREC,unitn) = k 
      fit(FIT_LASTWRITE,unitn) = 1
      return

cdir$ eject
cc    READDR entry here.
c
c     call READDR(unit,ifwa,nnn,kk,ierrf)
c
c     where:  unit = unit number from 1 to 99, or dataset name.
c             ifwa = array to receive data.
c              nnn = number of words to read.if not a multiple
c                    of DISKBLK it will be rounded up to next highest
c                    number that will be a DISKBLK factor.  if user
c                    is in asynchronous mode and record size is
c                    not a multiple of DISKBLK user data will be over
c                    written and not restored.  if this is suspected
c                    to cause problems the dataset can be switched
c                    to synchronous mode for reads and data will be
c                    copied out and restored after the read.
c               kk = record number or named record to read.
c
c              ierrf =  optional error status return
c                       if parameter passed, error status
c                       will be returned to the user.  if
c                       ierrf is not used program will abort.
c
c                       on input, if ierrf > 0 only error
c                       number returned, no logfile message
c                       posted.
c
c                       on return, if ierrf < 0, error has
c                       been encountered.
c
c           illegal unit number or name             ierrf = -1
c           illegal named index                     ierrf = -6
c           record index array full                 ierrf = -7
c           index number .gt. maximum               ierrf = -8
c           rewrite record exceeds original         ierrf = -9
c           named record not found                  ierrf = -10
c           opendr not called                       ierrf = -15
c           index entry .le. 0                      ierrf = -17
c           record length .le. 0                    ierrf = -18
c           index offset .le. 0                     ierrf = -19
c
c
c     flow:
c
c     1. round record size up to next multiple of DISKBLK if necessary.
c        if illegal dataset name or opendr not called abort.
c     2. compute index array address and type for this unit.
c     3. if numbered index dataset skip to 10.
c     4. if index name is 0  -  abort.
c     5. search index array for a matching name.
c     6. if match set record length and word address. skip to 9.
c     7. loop back to 6 until index limit.
c     8. name not found in index array  -  abort.
c     9. skip to 12.
c     10. dataset will be referenced by numbered index.  if index.le. 0
c        or index .gt. limit as set at openms time  -  abort.
c     11. set record length and word address from index array.
c     12. abort if either record length or word address is zero.
c     13. if requested length is .gt. record length only transfer
c        the actual record length.
c     14. if transfer length is greater than user requested length
c        and this is a synchronous call save the data that will
c        be clobbered in a scratch array.
c     15. read in the record from the dataset.
c     16. if synchronous and data saved restore the data to user array.
c     17. update the file statistics and return.
c
c


      entry READDR(unit,ifwa,nnn,kk,ierrf)

      iabort = 1
      message = .true.
      if (numarg().eq.5) then
          iabort = 0
          message = ierrf .le. 0
          ierrf = 0
      endif

      itime = 0
      call wdset(unit,idn)
      if ((idn.eq.0) .or. (idn.eq.1)) then
          if (message) then
              assign 9300 to msg
              write(102,msg)
          endif
          if (iabort.eq.1) call abort
          ierrf = -1
          return
      endif

      unitn = 0
      do 170 i = 1,G@DSNMAXD
          if (fit(FIT_NAME,i).eq.idn) then
              unitn = i
              unitnn = i + G@DSNMAXW
          endif
  170 continue
      if (unitn.eq.0) then
          if (message) then
              assign 9190 to msg
              write(102,msg)idn
          endif
          if (iabort.eq.1) call abort
          ierrf = -15
          return
      endif

c     Round number of words up to multiple of *DISKBLK*

      rln = ((nnn+(DISKBLK-1))/DISKBLK)*DISKBLK

      k = kk
      idxptr = fit(FIT_CINDADDR,unitn)
      indexl = fit(FIT_CINDLENG,unitn)
      isync = fit(FIT_ASYNC,unitn)
      if (fit(FIT_CINDTYPE,unitn) .eq. 1) then

c         file will be referenced thru named index.

          if (k.eq.0) then
              if (message) then
                  ermsg = drio0dsn(fit(FIT_NAME,unitn))
                  assign 9090 to msg
                  write(102,msg)ermsg
              endif
              if (iabort.eq.1) call abort
              ierrf = -6
              return
          endif

c         search the index array for a matching name.

          do 190 i = 1,indexl,2
              if (idxusr(i).eq.k) then
                  lra = idxusr(i+1) .and.
     &                        maskr(fit(FIT_CINDLOCFLD,unitn))
                  leng = shiftr(idxusr(i+1),fit(FIT_CINDLOCFLD,unitn))
                  k = shiftr((i+1),1)
                  go to 200
              endif
  190     continue

c         here we are at the end of the array and no match  - issue
c         error  ' named record not found in index array '

          if (message) then
              assign 9140 to msg
              write(102,msg)k,idn
          endif
          if (iabort.eq.1) call abort
          ierrf = -10
          return
  200     continue

      else

c         file will be referenced thru numbered index.

          if (k.le.0) then
              if (message) then
                  ermsg = drio0dsn(fit(FIT_NAME,unitn))
                  assign 9250 to msg
                  write(102,msg)ermsg
              endif
              if (iabort.eq.1) call abort
              ierrf = -19
              return
          endif

          if (k.gt.indexl) then
              if (message) then
                  ermsg = drio0dsn(fit(FIT_NAME,unitn))
                  assign 9110 to msg
                  write(102,msg)ermsg
              endif
              if (iabort.eq.1) call abort
              ierrf = -8
              return
          endif

          lra = idxusr(k) .and. maskr(fit(FIT_CINDLOCFLD,unitn))
          leng = shiftr(idxusr(k),fit(FIT_CINDLOCFLD,unitn))
     
      endif

      if (lra.le.0) then
          if (message) then
              ermsg = drio0dsn(fit(FIT_NAME,unitn))
              assign 9230 to msg
              write(102,msg)ermsg,k
          endif
          if (iabort.eq.1) call abort
          ierrf = -17
          return
      endif

      nn = min0(rln,leng)
      if (nn.le.0) then
          if (message) then
              ermsg = drio0dsn(fit(FIT_NAME,unitn))
              assign 9240 to msg
              write(102,msg)ermsg,k
          endif
          if (iabort.eq.1) call abort
          ierrf = -18
          return
      endif

c     *nnn* is the user's buffer size
c     *leng* is the record length from the file and is a multiple of DISKBLK
      j = min0(nnn,leng)
      fullblks = j/DISKBLK
      residue  = j - fullblks*DISKBLK
      if (residue .eq. 0) then
        it1 = irtc()
        call readwa(unitnn,ifwa,lra,fullblks*DISKBLK,isync)
        itime = irtc() - it1
      else
        it1 = irtc()
        call readwa(unitnn,isc2,lra+fullblks*DISKBLK,DISKBLK,0)
        j = fullblks*DISKBLK
        do 210 i = 1,residue
          ifwa(i+j) = isc2(i)
  210   continue
        itime = irtc() - it1
        if (fullblks .gt. 0) then
          it1 = irtc()
          call readwa(unitnn,ifwa,lra,fullblks*DISKBLK,isync)
          itime = itime + irtc() - it1
        endif
      endif

      maxrec = fit(FIT_MAXREC,unitn) 
      minrec = fit(FIT_MINREC,unitn) 
      if (rln.gt.maxrec) fit(FIT_MAXREC,unitn) = nn
      if (rln.lt.minrec) fit(FIT_MINREC,unitn) = nn
      fit(FIT_ACCTIME,unitn) = fit(FIT_ACCTIME,unitn) + itime
      if (fit(FIT_LASTWRITE,unitn).eq.0) then
          if (fit(FIT_PREVREC,unitn)+1.eq.k)  then
             fit(FIT_SEQREADS,unitn) = fit(FIT_SEQREADS,unitn) + 1
          endif
      endif

      fit(FIT_READS,unitn) = fit(FIT_READS,unitn) + 1
      fit(FIT_WDSMOVED,unitn) = fit(FIT_WDSMOVED,unitn) + rln
      fit(FIT_PREVREC,unitn) = k
      fit(FIT_LASTWRITE,unitn) = 0
      return
cdir$ eject
cc    closdr entry
c
c     call closdr(unit,ierrf)
c
c     where:  unit = unit number from 0 to 99 or file name to close.
c
c            ierrf =  optional error status return
c                     if parameter passed, error status
c                     will be returned to the user.  if
c                     ierrf is not used program will abort.
c
c                     on input, if ierrf > 0 only error
c                     number returned, no logfile message
c                     posted.
c
c                     on return, if ierrf < 0, error has
c                     been encountered.
c
c        error messages:
c
c           illegal unit number or name       ierrf = -1
c           opendr non called                 ierrf = -15
c
c
c     flow:
c
c     1. if bad unit number or name -  abort.
c        if dataset not opened by call to opendr - abort.
c     2. if dataset never written on skip to 6.
c     3. compute eoi word address, the index array address, and
c        the length of the index array for this dataset.
c     4. rewrite the first siz words of the dataset.  the first
c        word will contain the word address of the index array,
c        and the second word will contain the length of the index.
c        word 3 will have todays date, word 4 will have the time,
c        word 5 will have a checksum of first 4 words, and word
c        6 will have a non zero value to indicate file created
c        by WRITDR.
c     5. write the index array to the eoi word address.
c     6. call closewa.  write statistics record on file $stats, zero
c        out the file name in the fit and return.
c

      entry closdr(unit,ierrf)

      iabort = 1
      message = .true.
      if (numarg().eq.2) then
          iabort = 0
          message = ierrf .le. 0
      endif

      call wdset(unit,idn)
      if (idn.eq.0) then
          if (message) then
              assign 9300 to msg
              write(102,msg)
          endif
          if (iabort.eq.1) call abort
          ierrf = -1
          return
      endif

      unitn = 0
      do 230 i = 1,G@DSNMAXD
          if (fit(FIT_NAME,i).eq.idn) then
              unitn = i
              unitnn = i + G@DSNMAXW
          endif
  230 continue

      if (unitn.eq.0) then
          if (message) then
              assign 9190 to msg
              write(102,msg)idn
          endif
          if (iabort.eq.1) call abort
          ierrf = -15
          return
      endif

      it1 = 0

c     zero the isc() and isc2() arrays

      isc = 0
      isc2 = 0

      if (fit(FIT_WRFLAG,unitn) .ne. 0) then

c         dataset was written on so we must update the index.
c         first build the six words words in the first pru.


          ieoia  = fit(FIT_EOI,unitn)
          idxptr = fit(FIT_MINDADDR,unitn)
          isc(1) = ieoia
          indexl = fit(FIT_MINDLENG,unitn)
          isc(2) = indexl
          isc(3) = date()
          isc(4) = clock()
          isc(6) = 1                ! indicate DRIO file
          isc(7) = fit(FIT_VERS,unitn)

          ichk = 0
          do i = 1,4
              ichk = ichk + shiftr(isc(i),16)
          enddo
          if (fit(FIT_VERS,unitn).ge.2) then
               isc(5) = ichk + 1
               isc(8) = fit(FIT_MINDLOCFLD,unitn)
          else
               isc(5) = ichk 
          endif

c         now rewrite the first pru containing flags on the dataset.

          it0 = irtc()
          call writewa(unitnn,isc,1,DISKBLK,0)
          it1 = it1 + irtc() - it0

c         now write the index at the eoi address. after making sure
c         that the count is a multiple of DISKBLK.

          fullblks = indexl/DISKBLK
          residue  = indexl - fullblks*DISKBLK
          it0 = irtc()
          if (residue .eq. 0) then
            call writewa(unitnn,idxusr(1),ieoia,DISKBLK*fullblks,0)
          else
            do i = 1, residue
              isc2(i) = idxusr((fullblks*DISKBLK)+i)
            enddo
            if (fullblks .gt. 0) then
              call writewa(unitnn,idxusr(1),ieoia,DISKBLK*fullblks,0)
            endif
            call writewa(unitnn,isc2,ieoia+fullblks*DISKBLK,DISKBLK,0)
          endif
          it1 = it1 + (irtc() - it0)
          fit(FIT_EOI,unitn) = 0
          fit(FIT_WRFLAG,unitn) = 0
      endif

      it0 = irtc()
      call closewa(unitnn)
      it1 = it1 + irtc() - it0
      fit(FIT_ACCTIME,unitn) = fit(FIT_ACCTIME,unitn) + it1
      fit(FIT_NAME,unitn) = 0

      tota = fit(FIT_ACCTIME,unitn)/float(I@CLKTCK())

      irwip = fit(FIT_REWRITES,unitn)
      iweoi = fit(FIT_EOIWRITES,unitn)
      imaxr = fit(FIT_MAXREC,unitn)
      iwrit = irwip + iweoi

      iseqr = fit(FIT_SEQREADS,unitn)
      iseqw = fit(FIT_SEQWRITES,unitn)
      iminr = fit(FIT_MINREC,unitn)

      iread = fit(FIT_READS,unitn)
      itotw = fit(FIT_WDSMOVED,unitn) 

      avat = 0.0
      if ((iwrit+iread).ne.0) avat = tota/ (iwrit+iread)*1000.0
      if ((iread+iwrit).eq.0) iminr = 0
      write ('$STATS',9000) idn
      write ('$STATS',9010) (iwrit+iread),iread,iwrit
      write ('$STATS',9020) iseqr,iseqw,irwip,iweoi
      write ('$STATS',9030) itotw,iminr,imaxr
      write ('$STATS',9040) tota,avat
      return

 9000 format ('1','  READDR/WRITDR SUMMARY FOR DATASET ',a8)
 9010 format ('0','  TOTAL ACCESSES =    ',i12,'  READS =             ',
     +       i8,'  WRITES =            ',i6)
 9020 format ('0','  SEQUENTIAL READS =  ',i12,
     +       '  SEQUENTIAL WRITES = 'i8,'  REWRITES IN PLACE = ',i6,
     +       ' WRITES TO EOI = ',i6)
 9030 format ('0','  TOTAL WORDS MOVED = ',i12,'  MINIMUM RECORD  =   ',
     +       i8,'  MAXIMUM RECORD  = ',i8)
 9040 format ('0','  TOTAL ACCESS TIME = ',f12.3,'  SECONDS.  AVERAGE ',
     +       'ACCESS TIME =  ',f10.3,' MILLISECONDS ')
 9050 format ('DR004 - INDEX LENGTH .LE. 0 ON UNIT = ',a8)
 9060 format ('DR010 - INDEX LENGTH .GT. USER INDEX LENGTH ON UNIT =',
     $         a8)
 9070 format ('DR011 - DATASET INDEX LENGTH = ',i6,' USER LENGTH = ',i6)
 9080 format ('DR012 - USER INDEX LENGTH .GT. INDEX LENGTH ON UNIT=',a8)
 9090 format ('DR015 - ILLEGAL NAMED INDEX ON UNIT = ',a8)
 9100 format ('DR016 - RECORD INDEX ARRAY FULL ON UNIT = ',a8)
 9110 format ('DR017 - INDEX NUMBER .GT. MAXIMUM ON UNIT = ',a8)
 9120 format ('DR018 - REWRITE RECORD EXCEEDS ORIGINAL ON UNIT =',a8)
 9130 format ('DR019 - OLD RECORD SIZE = ',i8,'  NEW RECORD SIZE = ',i8)
 9140 format ('DR020 - NAMED RECORD NOT FOUND, NAME = ',a8,' UNIT = ',a8
     $       )
 9150 format ('DR006 - INDEX WORD ADDRESS .LE. 0 UNIT = ',a8)
 9160 format ('DR007 - INDEX LENGTH .LE. 0 ON UNIT = ',a8)
 9170 format ('DR009 - CHECKSUM ERROR ON UNIT = ',a8)
 9172 format ('DR022 - DRIO FILE VERSION ERROR ON UNIT = ',a8)
 9173 format ('DR022 - DRIO FILE VERSION ',i2,' NOT SUPPORTED')
 9180 format ('DR002 - DATASET ALREADY OPEN,  UNIT = ',a8)
 9190 format ('DR013 - OPENDR NOT CALLED ON UNIT = ',a8)
 9200 format ('DR014 - STINDR CALL CANNOT CHANGE INDEX TYPE,  UNIT = ',
     $         a8)
 9210 format ('DR008 - DATASET PROBABLY NOT CREATED BY READDR/WRITDR ',
     $         a8)
 9220 format (' LAST UPDATE ',a8,' TIME ',a8,' FILE LGTH= ',i8,' INDX ',
     +       'LG= ',i6)
 9230 format ('DR021 - INDEX ENTRY LE 0,  UNIT= ',a8,' INDEX NUMBER = ',
     $          i6)
 9240 format ('DR022 - RECORD LENGTH .LE. 0 ON UNIT = ',a8,
     $        ' INDEX RECORD NUMBER = ',i6)
 9250 format ('DR023 - INDEX NUMBER .LE. 0 ON UNIT = ',a8)
 9260 format ('DR024 - DATASET CREATED BY WRITMS, NOT WRITDR ',a8)
 9270 format ('DR005 - READDR/WRITDR FILE OPENED. FILE= ',a8,' EOI = ',
     $         i6)
 9300 format ('DR001 - ILLEGAL UNIT NUMBER OR NAME')
 9310 format ('DR003 - MAXIMUM NUMBER OF DATASETS EXCEEDED ')
 9320 format ('DR025 - NO AVAILABLE MEMORY, ERROR ',I5,'.')
      end

c     drio0dsn
c     Translate a unit number or Hollerith into a character string suitable
c     for printing.

      character*8 function drio0dsn(idsn)
      integer idsn
      integer i
      integer rnb

      if (0 .le. idsn .and. idsn .le. 99) then
        write(drio0dsn, 10) idsn
   10   format(i3)
      else
        write(drio0dsn, 20) idsn
   20   format(a8)
      endif
      return
      end
