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

      subroutine atx@ep(iatxcsn)
CDIR$ ID "@(#) libu/multi/atx.F	92.3	05/19/99 14:19:43"
cdir$ stack
cdir$ novector
      implicit integer(a-z)

c.... Communications blocks with the fmp instrumentation

      common /ut@comm/ ut@subr,ut@slave,iatxrloc,ut@trips,
     .                 ut@sched,ut@case,ut@label,ut@savelast,
     .                 ut@guard,ut@param,ut@wait

      parameter(MAXCPUS=64)
      parameter(MAXCS=16)

      common /atx@com/ iatxmxcp,iatxcpu
      common /atx@com/ iatxpst,iatxbp,iatxep
      common /atx@com/ iatxbcs(MAXCPUS,MAXCS)
      common /atx@com/ iatxtop(MAXCPUS,MAXCS)
      common /atx@com/ iatxbot(MAXCPUS,MAXCS)
      common /atx@com/ iatxitc(MAXCPUS,MAXCS)
      common /atx@com/ iatxitr(MAXCS)
      common /atx@com/ iatxdo(MAXCS),iatxtype(MAXCS)
      common /atx@com/ iatxsavl(MAXCS),iatxgard(MAXCS)
      common /atx@com/ iatxparm(MAXCS)
      common /atx@com/ iatxwt(MAXCPUS,MAXCS)

      common /g@tsbptr/ itsbptrs(64)
      integer reconnct(64)
      integer taskcps(64)
      pointer(iptsb,itsb(1))

      integer*8 pr,loop

c.... Indexes into the pr (parallel region) structure

      parameter(SUBNAME=1)
      parameter(SLVNAME=2)
      parameter(Loop1Ptr=3)
      parameter(PSTPtr=4)
      parameter(mnxBPT=5)
      parameter(totBPT=6)
      parameter(mnxSAT=7)
      parameter(totSAT=8)
      parameter(mnxWST=9)
      parameter(totWST=10)
      parameter(NUDNcpus=11)
      parameter(Ncs=12)
      parameter(PRSZ=12)

c.... Indexes into the pst (preceding serial time) structure

      parameter(minPST=1)
      parameter(maxPST=2)
      parameter(totPST=3)
c     parameter(PSTPtr=4)
      parameter(PSTNAME=5)
      parameter(PSTNx=6)
      parameter(PSTSZ=6)

c.... Indexes into the loop (parallel loop) structure

c     parameter(N*T*=1..MAXCPUS)
c     parameter(mnxT*=MAXCPUS+1..2*MAXCPUS)
      parameter(NuTu=2*MAXCPUS+1)
      parameter(mnxTu=2*MAXCPUS+2)
      parameter(mnTuIOT=2*MAXCPUS+3)
      parameter(NuIter=2*MAXCPUS+4)
      parameter(mnxNuItr=2*MAXCPUS+5)
      parameter(NcIter=2*MAXCPUS+6)
      parameter(mnxNcItr=2*MAXCPUS+7)
      parameter(LoopPtr=2*MAXCPUS+8)
      parameter(mnxIOT=2*MAXCPUS+9)
      parameter(totIOT=2*MAXCPUS+10)
      parameter(mnxWT=2*MAXCPUS+11)
      parameter(totWT=2*MAXCPUS+12)
      parameter(mnxLST=2*MAXCPUS+13)
      parameter(totLST=2*MAXCPUS+14)
      parameter(mnxILT=2*MAXCPUS+15)
      parameter(totILT=2*MAXCPUS+16)
      parameter(mnxBCT=2*MAXCPUS+17)
      parameter(totBCT=2*MAXCPUS+18)
      parameter(mnxPDT=2*MAXCPUS+19)
      parameter(totPDT=2*MAXCPUS+20)
      parameter(LoopFlgs=2*MAXCPUS+21)
      parameter(LOOPSZ=2*MAXCPUS+21)

      parameter(MAXTIME=17777777777777B)
      parameter(MXTIME=37777777777B)
      parameter(MASKHWR=37777777777B)

      parameter(MAXTRACE=2000)

      parameter(IATXSZ=MAXCPUS*MAXCS)

c.... Type values

      parameter(CASE=0)
      parameter(CHNKSZ=1)
      parameter(GUIDED=2)
      parameter(NCHNKS=3)
      parameter(SINGLE=4)
      parameter(VECTOR=5)

c.... Flag values

      parameter(VALID=1)
      parameter(GUARDED=2)
      parameter(ENDCASE=4)
      parameter(SAVELAST=8)

      parameter(ANAMINC=256)
      parameter(APTRINC=64)
      pointer(ipatxptr,atxptr(1))
      pointer(ipatxnam,atxnam(1))
      pointer(ipatxpto,atxptold(1))

      pointer(ippr,pr(1))
      pointer(ipprv,prv(1))
      pointer(ippst,pst(1))

      pointer(iploop,loop(1))
      pointer(ipname,name(1))

      external atx@quit
      character*80 atxfile

      integer ipuname
      character*15 sysname,release
      character*8 garded,savlast
      equivalence (isysname, sysname)
      equivalence (irelease, release)
      equivalence (igarded, garded)
      equivalence (isavlast, savlast)

      equivalence (iippr, ippr)
      equivalence (iipatxnam, ipatxnam)
      equivalence (iipatxptr, ipatxptr)

      real second,timef,tsecnd
      real t,tatxbp,tatxep,clkrate,xx

cdir$ vfunction utqgn,utqegn
      external utqep
      external atx@slave

      INTERFACE

        SUBROUTINE atx@write(unit, fmt, a1, a2, a3, a4, a5, a6, a7)
          INTEGER           :: unit, a1
          INTEGER, OPTIONAL :: a2, a3, a4, a5, a6, a7
          character*120 fmt
        END SUBROUTINE

      END INTERFACE

      integer atx@startpr
      integer atxargs(0:1)
      save atxargs
      external atx@checkstrm
      integer atxstream

      dimension bcs(MAXCPUS)
      dimension top(MAXCPUS)
      dimension bot(MAXCPUS)
      dimension itc(MAXCPUS)
      dimension wsw(MAXCPUS)

      data machtype/'CRAY-YMP'/

      data ut@param/1/

      data iatxcpu/1/
      data iatxbcs/IATXSZ*0/
      data iatxtop/IATXSZ*0/
      data iatxbot/IATXSZ*0/
      data iatxitc/IATXSZ*0/
      data iatxitr/MAXCS*0/
      data iatxdo/MAXCS*0/
      data iatxtype/MAXCS*CASE/
      data iatxsavl/MAXCS*0/
      data iatxgard/MAXCS*0/
      data iatxparm/MAXCS*1/
      data iatxwt/IATXSZ*0/

      data iatxsub/0/
      data iatxslv/0/
      data iatxprv/0/

      data bpmin/600/
      data maxvl/64/
      data iatxibp/0/
      data maxbp/0/
      data npr/0/
      data ipatxptr/0/
      data atxptrsz/0/
      data ipatxnam/0/
      data atxnamsz/0/
      data maxnames/0/

      data maxcpu/1/
      data maxmem/0/

      data clkrate/0./
      data itsstart/0/

      data atxtrace/0/

      data atxon/0/
      data ign/0/
      data numgd/59/
      data ispread/1/
      data nbpx/0/
      data nunitask/0/
      data unitask/0/
      data iutqbp/0/
      data iatxncs/0/
      data iatxcsx/0/
      data atxsched/1/

      data nworks/0/
      data noworks/0/

      data tdisconp/0/
      data tdiscons/0/
      data ndisconp/0/
      data ndiscons/0/

      data ncstot/0/

      save itsovhd,atxpstsv,atxbpsv,atxonsv,outerpst,uttripsv
      save initlpst,finalpst

c.... Statement functions for pr (parallel region) array referencing

      setNcs(numcs)=or(and(pr(Ncs),compl(MASKHWR)),numcs)
      getNcs()=and(pr(Ncs),MASKHWR)

      setPtrp(ptr)=(ptr)
      getPtrp()=pr(Loop1Ptr)

      mnxp(i,ival)=or(shiftl(min0(0+shiftr(pr(i),32),ival),32),
     .                      max0(0+and(pr(i),MASKHWR),ival))
      setmnxp(imin,imax)=or(shiftl(imin,32),imax)
      getminp(i)=shiftr(pr(i),32)
      getmaxp(i)=and(pr(i),MASKHWR)

      incp(i,it)=pr(i)+shiftl(1,40)+it
      getavgp(i)=(and(pr(i),17777777777777B)+shiftr(pr(i),41))/
     .            max0(1,0+shiftr(pr(i),40))
      getNtotp(i)=shift(pr(i),40)

      incNUD()=pr(NUDNcpus)+shiftl(1,40)
      getNUD()=shiftr(pr(NUDNcpus),40)

      getNcpus()=and(pr(NUDNcpus),MASKHWR)
      incNcpus(n)=pr(NUDNcpus)+n

c.... Statement functions for preceding serial time structure references

      incNx()=pst(PSTNx)+1
      getNx()=pst(PSTNx)

      getpstPtr()=pst(PSTPtr)
      getpstName()=pst(PSTNAME)

c.... Statement functions for loop (parallel loop) array referencing
c.... NOTE: (8/15/94) low 32-bits of LoopFlgs not used now.

      setDo(do)=or(shiftl(do,40),and(loop(LoopFlgs),17777777777777B))
      getDo()=shiftr(loop(LoopFlgs),40)

      setF(i)=or(loop(LoopFlgs),shiftl(i,36))
      getF(i)=and(shiftr(loop(LoopFlgs),36),i)

      setType(i)=or(loop(LoopFlgs),shiftl(and(i,17B),32))
      getType()=and(shiftr(loop(LoopFlgs),32),17B)

      setParm(i)=or(and(loop(LoopFlgs),compl(MASKHWR)),and(i,MASKHWR))
      getParm()=and(loop(LoopFlgs),MASKHWR)

      setPtrl(ptr)=ptr
      getPtrl()=loop(LoopPtr)

      mnxl(i,ival)=or(shiftl(min0(0+shiftr(loop(i),32),ival),32),
     .                      max0(0+and(loop(i),MASKHWR),ival))
      setmnxl(imin,imax)=or(shiftl(imin,32),imax)
      getminl(i)=shiftr(loop(i),32)
      getmaxl(i)=and(loop(i),MASKHWR)

      incl(i,it)=loop(i)+shiftl(1,40)+it
      getavgl(i)=(and(loop(i),17777777777777B)+shiftr(loop(i),41))/
     .            max0(1,0+shiftr(loop(i),40))
      getNtotl(i)=shiftr(loop(i),40)

      NiTi(n,it)=or(and(loop(n),compl(17777777777777B)),it)+shiftl(1,40)
      getNi(n)=shiftr(loop(n),40)
      getTi(n)=and(loop(n),17777777777777B)
c
c.... Check to see if this program is multi-streaming.  If it is, abort
c.... because streaming and tasking cannot be done at the same time.
c
c.... Call a C routine to check for the presence of entry point $STREAM$ON
c.... on SV1 only.  If the entry point is zero, streaming is being done.
c.... Otherwise, streaming is not being done.
c
      atxstream = 1
      call atx@checkstrm(atxstream)
      if (atxstream .eq. 0) then
         print *, 'Fatal ATEXPERT Error--Multi-streaming programs ',
     .            'are not supported'
         call atx@abt(1)
      endif
c
c.... Try to detect a disconnect via tsecnd calls by the master task
c.... Td=approximate disconnect time
c
      tatxep=tsecnd()
      irtatxep=irtc()

cdir$ suppress atxon
      ign=utqgn(numgd,ign)			! Protect the updating of atxon
      if(atxon.le.0) then
         if(atxon.eq.0) return
         atxon=atxon+1
         if(atxon.eq.0) then
            atxon=atxonsv
            iatxbp=atxbpsv
            outerpst=atxpstsv
            ut@trips=uttripsv
         endif
         ign=utqegn(numgd,ign)
         return
      endif
      atxon=0
      ign=utqegn(numgd,ign)

      nconnect=0
      do i=1,MAXCPUS
         iptsb=itsbptrs(i)
         if(iptsb.ne.0) then
            reconnct(i)=itsb(1)
            taskcps(i)=itsb(2)
            nconnect=i
         else
            reconnct(i)=0
            taskcps(i)=0
         endif
      enddo

c.... Clear unitasking mode if set by atxbp earlier

      if(iutqbp.eq.1) then
         iutqbp=0
         call utqep
      endif

c.... Check for instrumentation errors

      if(iatxncs.eq.0 .and. iatxcpu.gt.MAXCPUS) then
         print *,'Fatal ATEXPERT Error--# cpus=',iatxcpu,' max is ',
     .           MAXCPUS
         call atx@abt(1)
      endif

c.... Check for unitasking measurements;  Note that unitasking may have
c.... occurred regardless whether ATEXPERT forced it or not

      if(iatxncs.ne.0) then
         unitask=1
         iatxcpu=1
         iatxcsx=mod(iatxncs-1,MAXCS)+1
         iatxtop(1,iatxcsx)=iatxbp
         iatxbot(1,iatxcsx)=iatxep
         iatxgard(iatxcsx)=ut@guard
         if(atxsched.eq.0) then
            iatxgard(iatxcsx)=or(iatxgard(iatxcsx),2)   ! Mark end of CASE struct
         endif
      else
         unitask=0
         iatxncs=iatxcsn
         iatxgard(mod(iatxncs-1,MAXCS)+1)=ut@guard
      endif
      if(iatxncs.lt.0) then
         print *,'Fatal ATEXPERT Error--# parallel loops=',iatxncs,
     .     ' minimum is 1.'
         call atx@abt(1)
      endif

c.... Check for a disconnect during this PR;
c.... Adjust times if at very beginning or very end

      Td=(iatxep-iatxbp)-(tatxep*clkrate-100-itsstart)
      if(Td.lt.1000) then
         Td=0
      else
         if((irtatxep-iatxep).gt.Td) then
            Td=0
         elseif(unitask.eq.1) then
            if((iatxep-iatxbp-1000).gt.Td) then
               iatxbcs(1,iatxcsx)=-Td
            else
               Td=0
            endif
         else
            ndisconp=ndisconp+1
            tdisconp=tdisconp+Td
         endif
      endif

c.... Store final timings and load first control structure into /atx@com/

      call atx@cp(-iatxncs)
      call atx@cs(1,iatxbcs,iatxtop,iatxbot,iatxitc,iatxwt,
     .            iatxitr,iatxdo,iatxtype,iatxsavl,iatxgard,iatxparm)

c.... Check for ATEXPERT trace--put out raw timing data

      if(atxtrace.lt.0) then
         ipname=iatxslv
         if(name(2).ne.'        ') then
            i=or(and(name(1),compl(MASKHWR)),
     .           shiftr(name(2),32))
            call atx@tr(outerpst,1,'BP ',i)
         else
            call atx@tr(outerpst,1,'BP ',name(1))
         endif
         mintim=iatxbp
         if(unitask.eq.1) then
            bot(1)=iatxtop(1,1)-int(bpmin*0.1)
            do ics=1,iatxncs
               lastbot=bot(1)
               call atx@cs(ics,bcs,top,bot,itc,wsw,
     .                     iter,donm,type,savl,gard,parm)
               if(ics.eq.iatxncs) bot(1)=iatxep
               call atx@tr(top(1)-lastbot,1,'TOP',type+ics*10)
               call atx@tr(bot(1)-top(1) ,1,'BOT',iter)
            enddo
            mintim=bot(1)
         elseif(iatxncs.ne.0) then
            mintim=iatxbp
            mint=mintim
            minics=1
            maxics=1

2           n=0
            itype=0
            ntopbot=0
            ics=minics
3           continue
               call atx@cs(ics,bcs,top,bot,itc,wsw,
     .                     iter,donm,type,savl,gard,parm)
               do i=1,iatxcpu
                  if(bcs(i).gt.mintim) then
                     if(n.eq.0 .or. bcs(i).lt.mint) then
                        n=i
                        nc=ics
                        mint=bcs(i)
                        ntype=type
                        itype=1
                     elseif(bcs(i).eq.mint) then
                        bcs(i)=bcs(i)+1
                     endif
                  endif
                  if(top(i).gt.mintim) then
                     ntopbot=ntopbot+1
                     if(n.eq.0 .or. top(i).lt.mint) then
                        n=i
                        nc=ics
                        mint=top(i)
                        ntype=type
                        niter=itc(i)
                        itype=2
                     else
                        if(top(i).eq.mint) top(i)=top(i)+1
                     endif
                  endif
                  if(bot(i).gt.mintim) then
                     ntopbot=ntopbot+1
                     if(n.eq.0 .or. bot(i).lt.mint) then
                        n=i
                        nc=ics
                        mint=bot(i)
                        ntype=type
                        niter=iter
                        itype=3
                     else
                        if(bot(i).eq.mint) bot(i)=bot(i)+1
                     endif
                  endif
               enddo
               if(maxics.gt.minics .and. ics.eq.minics .and. n.eq.0)
     .            minics=minics+1
            ics=ics+1
            if(itype.le.1 .and. ics.le.maxics) goto 3

            if(n.ne.0) then
               if(itype.eq.1) then
                  call atx@tr(mint-mintim,n,'BCS',ntype+nc*10)
               elseif(itype.eq.2) then
                  call atx@tr(mint-mintim,n,'TOP',niter)
               elseif(itype.eq.3) then
                  call atx@tr(mint-mintim,n,'BOT',niter)
               endif
               if(ntopbot.le.1) maxics=min0(maxics+1,iatxncs)
               mintim=mint
               goto 2
            endif

         endif
         call atx@tr(iatxep-mintim,1,'EP ',0)
         atxtrace=atxtrace+1
         if(atxtrace.ge.0) then
            atxtrace=0
            call atx@tr(0,0,'END',0)
         endif
      endif

c.... Allocate and initialize a PR structure if necessary

      if(and(atxptr(iatxibp),MASKHWR).eq.0) then
         call hpalloc(ippr,PRSZ,ierr,0)
         if(ierr.ne.0) then
            print *,'Fatal ATEXPERT Error--Could not allocate PR space'
            call atx@abt(1)
         endif
         atxptr(iatxibp)=or(atxptr(iatxibp),ippr)
         maxmem=max0(maxmem,iippr+PRSZ-1)
         npr=npr+1

c.... Move the master and slave names into the names heap space

c....... Determine # words in each name first

         ipname=iatxslv
         i=0
         nsw=0
         mask=0
         do while(mask.eq.0)
            nsw=nsw+1
            i=i+1
            mask=shiftl(377b,56)
            do while(and(name(i),mask).ne.0)
               mask=shiftr(mask,8)
            enddo
         enddo
         if(shiftl(mask,8).eq.0) nsw=nsw-1

         ipname=iatxsub
         i=0
         nmw=0
         mask=0
         do while(mask.eq.0)
            nmw=nmw+1
            i=i+1
            mask=shiftl(377b,56)
            do while(and(name(i),mask).ne.0)
               mask=shiftr(mask,8)
            enddo
         enddo
         if(shiftl(mask,8).eq.0) nmw=nmw-1

c....... See if master name already exists for another parallel region

         do i=1,atxnamsz
            j=1
            do while(atxnam(i+j-1).eq.name(j))
               if(j.eq.nmw) then
                  pr(SUBNAME)=or(shiftl(nmw,32),i)
                  nmw=0
                  goto 10
               endif
               j=j+1
            enddo
         enddo

10       continue

c....... Allocate more names heap space if necessary

         if((atxnamsz+nmw+nsw).ge.maxnames) then
            oldsz=atxnamsz
            ipatxpto=ipatxnam
            maxnames=atxnamsz+ANAMINC
            call hpalloc(ipatxnam,maxnames,ierr,0)
            if(ierr.ne.0) then
              print *,'Fatal ATEXPERT Error--',
     .                'Could not allocate name space'
              call atx@abt(1)
            endif
            maxmem=max0(maxmem,iipatxnam+maxnames-1)
            do i=1,oldsz
               atxnam(i)=atxptold(i)
            enddo
            if(ipatxpto.ne.0) call hpdeallc(ipatxpto,ierr,0)
         endif

	 if(atxnamsz.eq.0) then
	    atxnam(1)='$START'
	    atxnamsz=1
	    iatxprv=or(shiftl(1,32),1)
	 endif

         ipname=iatxsub
         if(nmw.ne.0) then
            do i=1,nmw
               atxnam(atxnamsz+i)=name(i)
            enddo
            pr(SUBNAME)=or(shiftl(nmw,32),atxnamsz+1)
            atxnamsz=atxnamsz+nmw
         endif

         ipname=iatxslv
         do i=1,nsw
            atxnam(atxnamsz+i)=name(i)
         enddo
         pr(SLVNAME)=or(shiftl(nsw,32),atxnamsz+1)
         atxnamsz=atxnamsz+nsw

         pr(Loop1Ptr)=0
         pr(PSTPtr)=0
         pr(mnxBPT)=setmnxp(MXTIME,0)
         pr(totBPT)=0
         pr(mnxSAT)=setmnxp(MXTIME,0)
         pr(totSAT)=0
         pr(mnxWST)=setmnxp(MXTIME,0)
         pr(totWST)=0
         pr(NUDNcpus)=0
         pr(Ncs)=0

c....... Allocate and initialize LOOP structure for each parallel loop in PR

         donm1=-1
         ic=0
         do 40 ics=1,iatxncs
            call atx@cs(ics,bcs,top,bot,itc,wsw,
     .                  iter,donm,type,savl,gard,parm)
            if(type.eq.VECTOR) parm=maxvl
            if(ics.eq.1) then
               ic=ic+1
               call atx@mklp(iptr)
               maxmem=max0(maxmem,iptr+LOOPSZ-1)
               pr(Loop1Ptr)=setPtrp(iptr)
               donm1=donm
            else
               if(donm.eq.donm1) goto 41
               iploop=getPtrp()
               dowhile(donm.ne.getDo() .and. getPtrl().ne.0)
                  iploop=getPtrl()
                  if(iploop.lt.0 .or. iploop.gt.maxmem) then
                     print *,'Fatal ATEXPERT Error--Out-of-bounds loop',
     .                       ' pointer: ',iploop
                     call atx@abt(1)
                  endif
               enddo
               if(getDo().eq.donm) goto 40
               ic=ic+1
               call atx@mklp(iptr)
               maxmem=max0(maxmem,iptr+LOOPSZ-1)
               loop(LoopPtr)=setPtrl(iptr)
            endif
            iploop=iptr
            loop(LoopFlgs)=setDo(donm)
            loop(LoopFlgs)=setType(type)
            loop(LoopFlgs)=setParm(parm)
            if(savl.eq.1) loop(LoopFlgs)=setF(SAVELAST)
            if(and(gard,1).eq.1) loop(LoopFlgs)=setF(GUARDED)
            if(and(gard,2).eq.2) loop(LoopFlgs)=setF(ENDCASE)
40       continue

41       pr(Ncs)=setNcs(ic)

      else

c....... Pick up pointer to structure previously allocated

         ippr=and(atxptr(iatxibp),MASKHWR)

      endif

c.... Count # of executions of PR and tally preceding serial time

      nbpx=nbpx+1
      ipprv=ippr
      ippst=pr(PSTPtr)
      do while(ippst.ne.0 .and. getpstName().ne.iatxprv)
         ipprv=ippst
         ippst=getpstPtr()
      enddo
      if(ippst.eq.0) then
         call hpalloc(ippst,PSTSZ,ierr,0)
         if(ierr.ne.0) then
           print *,'Fatal ATEXPERT Error--Could not allocate PST space'
           call atx@abt(1)
         endif
         prv(PSTPtr)=or(prv(PSTPtr),ippst)
         pst(minPST)=MXTIME
         pst(maxPST)=0
         pst(totPST)=0
         pst(PSTPtr)=0
         pst(PSTNx)=0
         pst(PSTNAME)=iatxprv
      endif
      iatxprv=pr(SLVNAME)
      pst(PSTNx)=incNx()
      ipstim=max0(1,outerpst)
      pst(minPST)=min0(pst(minPST),ipstim)
      pst(maxPST)=max0(pst(maxPST),ipstim)
      pst(totPST)=pst(totPST)+ipstim
c
c.... Note that it's possible to begin an extended parallel region outside a
c.... loop, but that loop has zero trips.  Hence, iatxncs will be zero since
c.... the cmic$ do parallels inside are never executed.
c
      if(iatxncs.eq.0) goto 2000
c
c.... Unitasking timing here
c
      if(unitask.eq.1) then
         nunitask=nunitask+1

c....... Update unitask timings for each parallel loop

         iploop=0
         do ics=1,iatxncs
            call atx@cs(ics,bcs,top,bot,itc,wsw,
     .                  Nuitr,donm,type,savl,gard,parm)
            if(type.eq.VECTOR) parm=maxvl
            niter=0

c.......... Loop here finds the correct (or adds a) control structure associated
c.......... with donm. Handles case of alternate paths through parallel regions.

280         continue
               if(iploop.gt.0) iploop=getPtrl()
               if(iploop.le.0) iploop=getPtrp()
               if(iploop.lt.0 .or. iploop.gt.maxmem) then
                  print *,'Fatal ATEXPERT Error--Out-of-bounds loop',
     .                    ' pointer: ',iploop
                  call atx@abt(1)
               endif
               if(iploop.ne.0) then
                  donm1=getDo()
               else
                  donm1=0
               endif
            if(donm.ne.0 .and. donm1.ne.donm) then
               niter=niter+1
               if(niter.lt.iatxncs) goto 280
               if(donm1.ne.0) then
                  if(getPtrl().ne.0) goto 280
               endif
               call atx@mklp(iptr)
               maxmem=max0(maxmem,iptr+LOOPSZ-1)
               if(donm1.eq.0) then
                  pr(Loop1Ptr)=setPtrp(iptr)
               else
                  loop(LoopPtr)=setPtrl(iptr)
               endif
               iploop=iptr
               loop(LoopFlgs)=setDo(donm)
               loop(LoopFlgs)=setType(type)
               loop(LoopFlgs)=setParm(parm)
               if(savl.eq.1) loop(LoopFlgs)=setF(SAVELAST)
               if(and(gard,1).eq.1) loop(LoopFlgs)=setF(GUARDED)
               if(and(gard,2).eq.2) loop(LoopFlgs)=setF(ENDCASE)
               pr(Ncs)=setNcs(getNcs()+1)
            endif
            if(type.eq.CASE) Nuitr=1
            if(type.eq.GUIDED .or. type.eq.VECTOR) then
               niter=(Nuitr+parm-1)/parm
            else
               niter=Nuitr
            endif
            if(Nuitr.gt.0) then

c............. bcs contains Tu disconnection time if any

               if(bcs(1).lt.0) then
                  top(1)=top(1)-bcs(1)
                  ndisconp=ndisconp+1
                  tdisconp=tdisconp-bcs(1)
               endif

c............. Store maximum iterations, minimum Tu per (vector) iter

               Tuiter=int((bot(1)-top(1))/niter+0.5)
               loop(NuTu)=incl(NuTu,Tuiter)
               loop(mnxTu)=mnxl(mnxTu,Tuiter)
               niteru=getmaxl(mnxNuItr)
               if(niteru.lt.Nuitr) then
                  loop(mnTuIOT)=Tuiter
                  loop(mnxIOT)=setmnxl(getminl(mnxIOT),MXTIME)
                  if(niteru.lt.3) then
                     loop(mnxTu)=setmnxl(Tuiter,Tuiter)
                  endif
               elseif(niteru.eq.Nuitr) then
                  Tuitr = loop(mnTuIOT)
                  loop(mnTuIOT)=min0(Tuitr,Tuiter)
               endif
               loop(NuIter)=incl(NuIter,Nuitr)
               loop(mnxNuItr)=mnxl(mnxNuItr,Nuitr)
               if(wsw(1).ne.0) then
                  Tw=bpmin*0.3
                  loop(totWT)=incl(totWT,Tw)
                  loop(mnxWT)=mnxl(mnxWT,Tw)
               endif

            else

               loop(NuTu)=incl(NuTu,0)

            endif

         enddo

         goto 2000

      endif


c                <<<<  Handle multiprocessing here >>>>

      if(Td.gt.0 .and. (iatxbcs(1,1)-iatxbp-bpmin).gt.Td) then
         minbcs=iatxbcs(1,1)
         do icpu=2,iatxcpu
            if(iatxbcs(icpu,1).ne.0) then
               minbcs=min0(minbcs,iatxbcs(icpu,1))
            endif
         enddo
         if(minbcs.gt.iatxbcs(1,1)) then
            iatxbp=iatxbp+Td
            Td=0
         endif
      endif

c.... Tally begin parallel (BP) times

      ibptim=iatxbcs(1,1)-iatxbp
      pr(mnxBPT)=mnxp(mnxBPT,ibptim)
      if(Td.eq.0 .or. ibptim.lt.1000) then
         pr(totBPT)=incp(totBPT,ibptim)
         bpmin=min0(bpmin,ibptim)
      endif

c.... Find 1st slave arrival (SA) time and number of entering cpus (mxcpu)

      minarrv=irtc()
      mxcpu=1
      do icpu=2,iatxcpu
         if(iatxbcs(icpu,1).ne.0) then
            minarrv=min0(minarrv,iatxbcs(icpu,1))
            mxcpu=icpu
         endif
      enddo

      ncwrk=mxcpu
      nworks=nworks+mxcpu*iatxncs
      ncstot=ncstot+iatxncs
      pr(NUDNcpus)=incNcpus(mxcpu)
      iploop=getPtrp()

c.... If we've suffered a disconnect and already have a valid result, then
c....    ignore this sample

      if(Td.ne.0) then
         if(getF(VALID).ne.0) then
            if(getNi(mxcpu).ge.3) then
               pr(NUDNcpus)=incNUD()
               goto 2000
            endif
         endif
         ivalid=0
      else
         ivalid=1
      endif

c.... Tally slave arrival time if at least 1 slave participated

      if(mxcpu.ge.2) then
         maxcpu=max0(maxcpu,mxcpu)
         satim=max0(bpmin,min0(MXTIME,minarrv-iatxbp))
         if(ivalid.eq.1 .or. satim.lt.100*bpmin) then
            pr(mnxSAT)=mnxp(mnxSAT,satim)
            pr(totSAT)=incp(totSAT,satim)
         endif
      else
         satim=MXTIME
      endif

c.... Tally info for each parallel loop in the PR

      iploop=0
      nTdLoops=0
      Tcitr=0
      maxbot=0
      maxbots=iatxep
      mincasbc=iatxep
      type=-1
      bot(1)=iatxbp
      nxttally=1
      do 1990 ics=1,iatxncs
         lasttype=type
         lastbot1=bot(1)
         call atx@cs(ics,bcs,top,bot,itc,wsw,
     .               niteru,donm,type,savl,gard,parm)
         if(type.eq.VECTOR) parm=maxvl
         niter=0

c....... Loop here finds the correct (or adds a) control structure associated
c....... with donm. Handles case of alternate paths through parallel regions.

1080     continue
            if(iploop.gt.0) iploop=getPtrl()
            if(iploop.le.0) iploop=getPtrp()
            if(iploop.lt.0 .or. iploop.gt.maxmem) then
               print *,'Fatal ATEXPERT Error--Out-of-bounds loop',
     .                 ' pointer: ',iploop
               call atx@abt(1)
            endif
            if(iploop.ne.0) then
               donm1=getDo()
            else
               donm1=0
            endif
         if(donm.ne.0 .and. donm1.ne.donm) then
            niter=niter+1
            if(niter.lt.iatxncs) goto 1080
            if(donm1.ne.0) then
               if(getPtrl().ne.0) goto 1080
            endif
            call atx@mklp(iptr)
            maxmem=max0(maxmem,iptr+LOOPSZ-1)
            if(donm1.eq.0) then
               pr(Loop1Ptr)=setPtrp(iptr)
            else
               loop(LoopPtr)=setPtrl(iptr)
            endif
            iploop=iptr
            loop(LoopFlgs)=setDo(donm)
            loop(LoopFlgs)=setType(type)
            loop(LoopFlgs)=setParm(parm)
            if(savl.eq.1) loop(LoopFlgs)=setF(SAVELAST)
            if(and(gard,1).eq.1) loop(LoopFlgs)=setF(GUARDED)
            if(and(gard,2).eq.2) loop(LoopFlgs)=setF(ENDCASE)
            pr(Ncs)=setNcs(getNcs()+1)
         endif

c....... Check for inter-loop disconnect

         if(Td.ne.0 .and. type.ne.CASE .and. lasttype.ne.CASE) then
            if(bcs(1).ne.0 .and. (bcs(1)-maxbots).gt.10*bpmin) then
               Td=max0(0,Td-(bcs(1)-lastbot1))
               nxttally=0
            endif
         endif

c....... Set flag for inter-loop timing tallies

         iltally=nxttally
         if(ics.ne.1 .and. mod(ics,MAXCS).eq.1) then
            iltally=0
         endif
         nxttally=1

         maxbotsv=maxbots
         maxbots=0

c....... Handle parallel CASE constructs here

         if(type.eq.CASE) then

c.......... Deal with disconnects if possible

            if(Td.gt.1000 .and. top(1).ne.0) then
               if((top(1)-bcs(1)).gt.Td) Td=0
            endif

c.......... Summarize CASE loop here
c.......... Note:  It is possible for a cpu to skip past all CASEs--abort the
c..........        timing analysis in this case (bcs(icpu)=0).

            ncwrkd=0
            mintops=0
            maxtops=0
            ncpu=0
            minbcs=maxbotsv
            do icpu=1,mxcpu
               if(bcs(icpu).eq.0) goto 2000
               if(top(icpu).ne.0) then
                  ncpu=icpu
                  maxbots=max0(maxbots,top(icpu))
               else
                  maxbots=max0(maxbots,bcs(icpu))
               endif
               if(lasttype.ne.CASE) then
                  mincasbc=min0(mincasbc,bcs(icpu))
                  if(icpu.gt.2) then
                     if(mintops.eq.0) then
                        mintops=bcs(icpu)
                     else
                        maxtops=bcs(icpu)
                        ncwrkd=ncwrkd+1
                     endif
                  endif
               endif
            enddo
            if(ncpu.eq.0) goto 2000

            ncwrk=1
            niteru=1
            niterc=1
            bcstot=top(ncpu)-mincasbc
            minbot=0
            maxbot=0

            mint=top(ncpu)
            if(ics.eq.iatxncs) then
               maxt=iatxep
            else
               call atx@cs(ics+1,bcs,top,bot,itc,wsw,
     .                     iter,donm,type,savl,gard,parm)
               if(type.eq.VECTOR) parm=maxvl
               if(bcs(ncpu).eq.0) then
                  maxt=iatxep
               else
                  maxt=bcs(ncpu)
               endif
            endif
            if(getNi(1).ge.3) then
               do i=1,nconnect
                  if(reconnct(i).gt.mint .and. reconnct(i).lt.maxt)
     .               goto 1990
               enddo
            endif
            Tctot=maxt-mint
            maxt=Tctot
            mint=0

         else

c.......... First analyze for disconnections and/or late reconnects
c.......... Gather min/max arrival times and work time/iteration

            minTcitr=irtc()
            minbcs=iatxep
            maxbcs=0
            maxbot=0
            ncwrk=0
            do icpu=1,mxcpu
               if(itc(icpu).ne.0) then
                  minbcs=min0(minbcs,bcs(icpu))
                  maxbcs=max0(maxbcs,bcs(icpu))
                  ncwrk=ncwrk+1
                  maxbot=max0(maxbot,bot(icpu))
                  Tcitr=(bot(icpu)-top(icpu)+
     .                   itc(icpu)-1)/itc(icpu)
                  minTcitr=min0(minTcitr,Tcitr)
               endif
            enddo

c.......... Test for disconnect if master is only working cpu on this loop

            if(ncwrk.eq.1) then
               if(itc(1).ne.0 .and. Td.ne.0) then

c................ Could this loop have had a disconnect?

                  mintim=1.25*niteru
                  if((bot(1)-top(1)-Td).gt.mintim) then
                     nTdLoops=nTdLoops+1

c................... Yes, if there are previous samples to compare against,
c................... check if removing Td will bring Tuitr close to them

                     if(ics.eq.iatxncs.and.nTdLoops.eq.1) then
                        if(ics.eq.1) iatxbp=iatxbp+Td
                        bcs(1)=bcs(1)+Td
                        top(1)=top(1)+Td
                        Td=0
                     elseif(getNi(NuTu).ge.2) then
                        if(type.eq.GUIDED .or. type.eq.VECTOR) then
                           niter=(niteru+parm-1)/parm
                        else
                           niter=niteru
                        endif
                        Tuiter=getminl(mnxTu)
                        dtu=int((bot(1)-top(1)-Td)/niter+0.5)-Tuiter
                        if(10*iabs(dtu).lt.Tuiter) then
                           if(ics.eq.1) iatxbp=iatxbp+Td
                           bcs(1)=bcs(1)+Td
                           top(1)=top(1)+Td
                           Td=0
                        endif

c................... Is this the only possible loop with a disconnect?

                     elseif(nTdLoops.eq.1) then
                        if((iatxep-bot(1)-
     .                   max0(2*bpmin,mintim)*(iatxncs-ics)).lt.Td) then
                         if(ics.eq.1) iatxbp=iatxbp+Td
                         bcs(1)=bcs(1)+Td
                         top(1)=top(1)+Td
                         Td=0
                        endif
                     endif
                     if(Td.eq.0) nxttally=0
                  endif
               endif
               goto 1500
            endif

            maxt=minbcs+max0(2*bpmin*mxcpu,minTcitr)

c.......... Skip loop types with non-uniform time per concurrent iteration

            if(type.eq.GUIDED .or. type.eq.VECTOR .or.
     .                             getmaxl(mnxWT).ne.0) then
               if(maxbcs.ge.maxt) iltally=0
               goto 1500
            endif

c.......... Remove cpus that were disconnected/reconnected; tally their iters;
c.......... calculate average work/iter in other cpus

c.......... Tally number of cpus which were reconnected during top..bot interval

            idiscon=0
            maxbcs=maxt
            do i=1,nconnect
               if(reconnct(i).gt.maxbcs .and. reconnct(i).lt.maxbot)
     .            idiscon=idiscon+1
            enddo
            if(idiscon.eq.0) goto 1500

c.......... If all were disconnected, assume large granularity work in which
c.......... case disconnect time has a minimal effect--remove one cpu.

            if(idiscon.eq.ncwrk) then
               if(GetNi(ncwrk-1).ge.3) then
                  maxbots=maxbot
                  goto 1990
               endif
               idiscon=1
            endif
            ivalid=0

c.......... Eliminate those cpus which started iterations after maxbcs

            nditers=0
            ncwrk=mxcpu
            do icpu=1,mxcpu
               if(itc(icpu).ne.0) then
                  if(bcs(icpu).gt.maxbcs) then
                     nditers=nditers+itc(icpu)
                     top(icpu)=0
                     bot(icpu)=0
                     itc(icpu)=0
                     idiscon=idiscon-1
                     ncwrk=ncwrk-1
                  endif
               else
                  ncwrk=ncwrk-1
               endif
            enddo

c.......... Eliminate the idiscon number of cpus with the largest times/iteration

            do while(idiscon.gt.0)
               ncpu=0
               maxTcitr=0
               do icpu=1,mxcpu
                  if(itc(icpu).ne.0) then
                     Tcitr=(bot(icpu)-top(icpu)+
     .                      itc(icpu)-1)/itc(icpu)
                     if(maxTcitr.lt.Tcitr) then
                        maxTcitr=Tcitr
                        ncpu=icpu
                     endif
                  endif
               enddo
               nditers=nditers+itc(ncpu)
               top(ncpu)=0
               bot(ncpu)=0
               itc(ncpu)=0
               ncwrk=ncwrk-1
               idiscon=idiscon-1
            enddo

c.......... Compute the average time per iteration for connected cpus

            tottc=0
            totws=0
            nciters=0
            do icpu=1,mxcpu
               if(itc(icpu).ne.0) then
                  Tcitr=(bot(icpu)-top(icpu)+
     .                   itc(icpu)-1)/itc(icpu)
                  tottc=tottc+bot(icpu)-top(icpu)
                  if(wsw(icpu).gt.0.5*ncwrk*minTcitr*itc(icpu)) then
                     wsw(icpu)=minTcitr*itc(icpu)
                  endif
                  totws=totws+wsw(icpu)
                  nciters=nciters+itc(icpu)
                  ncpu=icpu
               endif
            enddo
            if(ncwrk.eq.0) then
               pr(NUDNcpus)=incNUD()
               goto 2000
            endif
            if(nciters.eq.0) goto 1500

            avgTcitr=(tottc+nciters-1)/nciters
            avgws=(totws+nciters-1)/nciters

c.......... Redistribute work to cpus that were connected the whole time
c.......... Assume uniform concurrent iteration size; do a block dist 1st

            nciters=nditers/ncwrk
            if(nciters.ne.0) then
               inctime=nciters*avgTcitr
               incws=nciters*avgws
               nditers=nditers-nciters*ncwrk
               do icpu=1,mxcpu
                  if(top(icpu).ne.0) then
                     itc(icpu)=itc(icpu)+nciters
                     wsw(icpu)=wsw(icpu)+incws
                     bot(icpu)=bot(icpu)+inctime
                     if(bot(icpu).gt.maxbot) then
                        maxbot=bot(icpu)
                        if(ics.eq.iatxncs) iatxep=iatxep+avgTcitr
                     endif
                  endif
               enddo
            endif

c.......... Dist the remaining iterations to those cpus with min bottom times

            do iter=1,nditers
               do icpu=1,mxcpu
                  if(top(icpu).ne.0) then
                     if(bot(icpu).lt.bot(ncpu)) ncpu=icpu
                  endif
               enddo
               itc(ncpu)=itc(ncpu)+1
               wsw(ncpu)=wsw(ncpu)+avgws
               bot(ncpu)=bot(ncpu)+avgTcitr
               if(bot(ncpu).gt.maxbot) then
                  maxbot=bot(ncpu)
                  if(ics.eq.iatxncs) iatxep=iatxep+avgTcitr
               endif
            enddo

c.......... Set flag to reflect the redist of work; skips interloop tallies

            nxttally=0

c.......... Check master for a distinguishable disconnects before/after loop

1500        continue
            if(Td.ne.0 .and. bcs(1).ne.0) then

c............. If the master did not participate in the last CS, adjust Td 

               if(top(1).eq.0) then

                  minbcs=min0(bcs(1),minbcs)
                  if(ics.eq.iatxncs) then
                     Td=Td-(iatxep-minbcs)
                  else
                     Td=Td-(maxbot-minbcs)
                  endif
                  Td=max0(0,Td)

               else

c................ Check master for a distinguishable preceding gap

                  if((top(1)-bcs(1)).gt.10*bpmin) then
                     Td=max0(0,Td-(top(1)-bcs(1)-2*bpmin))
                  endif

               endif

            endif

c.......... Initialize loop summarization variables (non-CASE loop)

            mincasbc=iatxep

            niterc=0
            bcstot=0
            mint=iatxep
            if(ics.eq.1) mint=iatxbp
            maxt=0
            Tctot=0
            totws=0
            ncwrk=0
            ncwrkd=0
            minbcs=irtc()
            mintops=0
            maxtops=0
            minbot=iatxep
            maxbot=0

c.......... Summarize this parallel loop

            do icpu=1,mxcpu
               if(bcs(icpu).ne.0) then
                  mint=min0(mint,bcs(icpu))
                  minbcs=min0(minbcs,bcs(icpu))
                  if(top(icpu).ne.0) then
                     niterc=niterc+itc(icpu)
                     ncwrk=ncwrk+1
                     bcstot=bcstot+top(icpu)-bcs(icpu)
                     Tctot=Tctot+bot(icpu)-top(icpu)
                     maxt=max0(maxt,bot(icpu))
                     totws=totws+wsw(icpu)
                     if(mintops.eq.0) then
                        if(icpu.ne.1 .or. ics.ne.1) then
                           mintops=top(icpu)
                        endif
                     else
                        mintops=min0(mintops,top(icpu))
                        ncwrkd=ncwrkd+1
                     endif
                     maxtops=max0(maxtops,top(icpu))
                     minbot=min0(minbot,bot(icpu))
                     maxbot=max0(maxbot,bot(icpu))
                  endif
               endif
            enddo

            maxbots=max0(maxbots,maxbot)
            noworks=noworks+(mxcpu-ncwrk)

         endif

c....... Determine whether to tally loop overhead info

         if(ncwrk.eq.0 .or. niterc.eq.0) goto 1990

         tally=1
         Nimaxcpu=getNi(maxcpu)
         if(ncwrk.eq.maxcpu) then
            if(Nimaxcpu.eq.0) then
               loop(mnxIOT)=setmnxl(getminl(mnxIOT),MXTIME)
            endif
         elseif(Nimaxcpu.ne.0) then
            tally=0
         endif

c....... Increment Ni(ncwrk); store tot,min,max Ti(ncwrk) per (vector) iteration

         if(type.eq.GUIDED .or. type.eq.VECTOR) then
            niter=(niteru+parm-1)/parm
         else
            niter=niteru
         endif

         Tcitr=int((maxt-mint)/niter+0.5)
         loop(ncwrk)=incl(ncwrk,Tcitr)
         loop(MAXCPUS+ncwrk)=mnxl(MAXCPUS+ncwrk,Tcitr)
         if(ivalid.eq.1) then
            loop(LoopFlgs)=setF(VALID)
         endif

c....... Estimate iteration overhead (IO+WT) per iteration with 1 and max cpus

         if(type.ne.CASE .and. niteru.eq.getmaxl(mnxNuItr)) then
            Tu=niter*loop(mnTuIOT)
            Tio=max0(0,Tctot-Tu)
            if(Tio.lt.10*Tu*ncwrk .or. totws.gt.0) then
              Tio=Tio/niter
              if(Tio.eq.0 .and. totws.gt.0) Tio=0.1*bpmin
              Tw=min0(totws/niterc,Tio)
              Tio=Tio-Tw
              maxTw=max0(Tw,int(0.4*bpmin))
              minTw=min0(Tw,min0(int(0.2*bpmin),getminl(mnxWT)))
              mint=min0(getminl(mnxIOT),Tio)
              loop(totIOT)=incl(totIOT,Tio)
              loop(totWT)=incl(totWT,Tw)
              if(Nimaxcpu.eq.0) then
                loop(mnxIOT)=setmnxl(mint,Tio)
                loop(mnxWT)=setmnxl(minTw,maxTw)
              elseif(ncwrk.eq.maxcpu) then
                loop(mnxIOT)=setmnxl(mint,min0(Tio,getmaxl(mnxIOT)))
                loop(mnxWT)=setmnxl(minTw,min0(maxTw,getmaxl(mnxWT)))
              else
                loop(mnxIOT)=setmnxl(mint,getmaxl(mnxIOT))
                loop(mnxWT)=setmnxl(minTw,getmaxl(mnxWT))
              endif
            endif
         endif

c....... Tally number of concurrent iterations

         loop(NcIter)=incl(NcIter,niterc)
         loop(mnxNcItr)=mnxl(mnxNcItr,niterc)

         if(tally.eq.0 .or. iltally.eq.0) goto 1990

c....... Tally inter-loop (IL) time

         if(ics.ne.1) then
            Til=minbcs-maxbotsv
            if(Til.gt.0) then
               loop(mnxILT)=mnxl(mnxILT,Til)
               loop(totILT)=incl(totILT,Til)
            endif
         endif

c....... Tally begin control structure (BC) time

         Tbc=bcstot/ncwrk
         if(Tbc.gt.0) then
            loop(mnxBCT)=mnxl(mnxBCT,Tbc)
            loop(totBCT)=incl(totBCT,Tbc)
         endif

c....... Tally processor delta (PD) time--the time between arriving cpus

         if(ncwrkd.ne.0) then
            Tpd=(maxtops-mintops)/ncwrkd
            if(Tpd.gt.0) then
               loop(mnxPDT)=mnxl(mnxPDT,Tpd)
               loop(totPDT)=incl(totPDT,Tpd)
            endif
         endif

c....... Tally loop synchronzation (LS) time

         if(iatxncs.ne.1) then
            Tls=maxbot-minbot
            if(Tls.gt.0) then
               loop(mnxLST)=mnxl(mnxLST,Tls)
               loop(totLST)=incl(totLST,Tls)
            endif
         endif

1990  continue

c.... Tally time from last bottom to end parallel as master wait slave time

      if(ncwrk.ge.2) then
         Tws=iatxep-maxbot
         if(Tws.gt.0 .and. Tws.lt.satim*2) then
            pr(mnxWST)=mnxp(mnxWST,Tws)
            pr(totWST)=incp(totWST,Tws)
         endif
      endif

c.... Check master for distinguishable ending disconnect

      if(Td.ne.0 .and. (iatxep-maxbots).gt.10*bpmin) then
         Td=max0(0,Td-(iatxep-bot(1)))
      endif

c.... If Td not fully identified, then increment disconnect counter

      if(Td.ge.1000) pr(NUDNcpus)=incNUD()

2000  continue

c.... Initialize /ATX@COM/ for the next PR

      do ics=1,min0(MAXCS,max0(1,iatxncs))
         iatxdo(ics)=0
         iatxtype(ics)=CASE
         iatxsavl(ics)=0
         iatxgard(ics)=0
         iatxparm(ics)=1
         iatxitr(ics)=0
         do icpu=1,iatxcpu
            iatxbcs(icpu,ics)=0
            iatxtop(icpu,ics)=0
            iatxbot(icpu,ics)=0
            iatxitc(icpu,ics)=0
            iatxwt(icpu,ics)=0
         enddo
      enddo
      iatxitc(MAXCPUS,MAXCS)=0       ! Clear possible store of iatxitr(0)
      iatxcpu=1
      iatxbp=0
      iatxep=0
      iatxncs=0
      iatxcsx=0
      ut@param=1
      itsstart=tsecnd()*clkrate+50+itsovhd

      return

      entry atx@bp

c.... Initialize ATEXPERT timing of a PR; Determine whether to force unitasking

      it0=irtc()
      tatxbp=tsecnd()
      it1=irtc()

c.... Check for higher level ATEXPERT instrumentation; note that multiple tasks
c.... (slave tasks) can be entering atx@bp at lower levels of instrumentation.

cdir$ suppress atxon
      ign=utqgn(numgd,ign)			! Protect the updating of atxon
      if(atxon.ne.0) then
         if(atxon.gt.0) then
            atxonsv=atxon
            atxon=-1
            atxpstsv=outerpst
            atxbpsv=iatxbp
            uttripsv=ut@trips
            if(iutqbp.eq.0) then	! If I have not forced unitasking yet
               atxargs(0)=1		!   Do it now to avoid atx@com clobbering
               atxargs(1)=loc@(maxmem)
               iutqbp = atx@startpr(atx@slave,atxargs)
            endif
         else
            atxon=atxon-1		! -(# of lower level tasks calling atx@bp)
         endif
         ign=utqegn(numgd,ign)
         return
      endif
      ign=utqegn(numgd,ign)

c.... Higher level parallel execution is not occurring; therefore use ATEXPERT
c.... instrumentation at this lower level

      outerpst=iatxpst

c.... Save pointers to the highest level routine names

      iatxsub=ut@subr
      iatxslv=ut@slave

c.... Find PR structure for this call to atx@bp

      n=0
      do while (n.lt.maxbp)
         n=n+1
         if(iatxibp.gt.maxbp) iatxibp=1
         if(shiftr(atxptr(iatxibp),32).eq.iatxrloc)  goto 3050
         iatxibp=iatxibp+1
      enddo

c.... Allocate a PR structure for this call

c.... Increment the size of atxptr if PR# > current size

      if(atxptrsz.ge.maxbp) then
         oldsz=atxptrsz
         ipatxpto=ipatxptr
         atxptrsz=((maxbp+1)/APTRINC+1)*APTRINC
         call hpalloc(ipatxptr,atxptrsz,ierr,0)
         if(ierr.ne.0) then
           print *,'Fatal ATEXPERT Error--',
     .              'Could not allocate pointer space'
           call atx@abt(1)
         endif
         maxmem=max0(maxmem,iipatxptr+atxptrsz-1)
         do i=1,oldsz
            atxptr(i)=atxptold(i)
         enddo
         do i=oldsz+1,atxptrsz
            atxptr(i)=0
         enddo
         if(ipatxpto.ne.0) call hpdeallc(ipatxpto,ierr,0)

c....... ATEXPERT initialization here

         if(oldsz.eq.0) then
            machtype=ihost_pmt@()
            if(machtype.eq.'CRAY-2') then
               bpmin=600
            elseif(machtype.eq.'CRAY-XMP') then
               bpmin=200
            elseif(machtype.eq.'CRAY-YMP') then
               bpmin=250
            elseif(machtype.eq.'CRAY-C90') then
               bpmin=275
               maxvl=128
            elseif(machtype.eq.'CRAY-TS') then
               bpmin=300
               maxvl=128
            endif
            clkrate=1.0/(ihost_clkpico@()*1.e-12)
            outerpst=int(tatxbp*clkrate)
            initlpst=outerpst
            xx=timef()
            ito=irtc()
            xx=tsecnd()
            itsovhd=irtc()-ito
            call atexit(atx@quit)
         endif
      endif

      maxbp=maxbp+1
      iatxibp=maxbp
      atxptr(iatxibp)=shiftl(iatxrloc,32)

3050  atxon=iatxibp

      ut@guard=0
      ut@wait=0
      atxsched=1
      iatxcpu=1

c.... Find minimum TSECND overhead

      itsovhd=min0(itsovhd,it1-it0)

c.... Check for a disconnect during the preceding serial time (outerpst)

      Td=outerpst-(tatxbp*clkrate-itsstart)
      if(Td.gt.1000) then
         if(maxbp.ne.0) then
            if(outerpst.gt.Td) outerpst=outerpst-Td
            ndiscons=ndiscons+1
            tdiscons=tdiscons+Td
         endif
      endif

c.... Determine whether to force unitasking or not

      unitask=0
      iatxmxcp=MAXCPUS
      if(and(atxptr(iatxibp),MASKHWR).eq.0) then

c....... Force unitasking for the 1st execution of a PR

         unitask=1

      else

         ippr=and(atxptr(iatxibp),MASKHWR)
         iploop=getPtrp()
         Nu=getNi(NuTu)

c....... Force unitasking on first sample

         if(Nu.eq.0) then

            unitask=1

c.......... Force T1 timing next to get basic parallel code timings and
c.......... generated code differences particularly for wait/send loops.

         elseif(getNi(1).eq.0) then

            iatxmxcp=1

c.......... Unitask if < 8 unitask samples and # of samples > 2**Nu

         elseif(Nu.lt.8) then

            Ns=Nu
            do icpu=1,maxcpu
               Ns=Ns+getNi(icpu)
            enddo
            if(shiftr(Ns,Nu).ne.0) then
               unitask=1
            elseif(Ns.gt.10 .and. getNi(1).lt.3) then
               iatxmxcp=1
            endif

c.......... Random unitasking

         else if(and(irtc(),37B).eq.0) then

            unitask=1

         else

c.......... Spread sampling across 1..maxcpu's

            ispread=ispread-1
            if(ispread.eq.0) then
               ispread=2
               do iatxmxcp=1,maxcpu
                  if(getNi(iatxmxcp).lt.3) goto 3310
               enddo
               iatxmxcp=MAXCPUS
3310           continue
            endif

         endif

      endif

c.... Force unitasking by making a dummy call to utqbp (inside atx@startpr)

      if(unitask.eq.1) then
         atxargs(0)=1
         atxargs(1)=loc@(maxmem)
         iutqbp = atx@startpr(atx@slave,atxargs)
      endif

c.... Get the master task's total cpu time at the start of the PR

      itsstart=tsecnd()*clkrate+50+itsovhd

      return

      entry atx@ut
      tatxep=tsecnd()
      irtatxep=irtc()
      if(atxon.le.0) return
      if(iatxncs.ge.1) then
         iatxtop(1,iatxcsx)=iatxbp
         iatxbot(1,iatxcsx)=iatxep
         Td=(iatxep-iatxbp)-(tatxep*clkrate-100-itsstart)
         if(Td.ge.1000) then
            if((irtatxep-iatxep).lt.Td) then
               if((iatxep-iatxbp-1000).gt.Td) then
                  iatxbcs(1,iatxcsx)=-Td
               endif
            endif
         endif
         iatxgard(iatxcsx)=ut@guard
         if(atxsched.eq.0 .and. (ut@sched+ut@case).ne.0) then
            iatxgard(iatxcsx)=or(iatxgard(iatxcsx),2)   ! Mark end of CASE struct
         endif
         if(iatxcsx.eq.MAXCS) then
            call atx@cp(iatxncs)
            iatxcsx=0
         endif
      endif
      iatxcsx=iatxcsx+1
      iatxncs=iatxncs+1
      iatxitr(iatxcsx)=ut@trips
      iatxdo(iatxcsx)=ut@label
      iatxtype(iatxcsx)=ut@sched
      iatxparm(iatxcsx)=ut@param
      iatxsavl(iatxcsx)=ut@savelast
      iatxwt(1,iatxcsx)=ut@wait
      ut@guard=0
      ut@wait=0
      atxsched=ut@sched
      ut@case=0
      itsstart=tsecnd()*clkrate+50+itsovhd
      return

      entry atx@tron(nbptrace)
      if(numarg().eq.0) then
         atxtrace=-MAXTRACE
      else
         atxtrace=-nbptrace
      endif
      return

      entry atx@troff
      if(atxtrace.lt.0) call atx@tr(0,0,'OFF',0)
      atxtrace=0
      return

      entry atx@exit
      finalpst=irtc()-iatxpst
      tatxbp=tsecnd()

c.... Subtract disconnect time since last parallel region ended

      Td=finalpst-(tatxbp*clkrate-itsstart)
      if(Td.gt.1000) then
         if(maxbp.ne.0) then
            if(finalpst.gt.Td) finalpst=finalpst-Td
            ndiscons=ndiscons+1
            tdiscons=tdiscons+Td
         endif
      endif

      if(iutqbp.eq.1) call utqep

c.... Determine name and create output file

      atxfile='ATXFILE'
      call pxfgetenv(atxfile, 7, atxfile, lenname, ierr)
      if(ierr.ne.0 .or. lenname.eq.0) then
         atxfile='atx.raw'
      endif

      call asnctl('NEWLOCAL',1,ierr)
      open(66,file=atxfile)

c.... Get the system name and release level for this run

      irelease = '        '
      isysname = '        '
      call pxfstructcreate('utsname',ipuname,ierr)
      call pxfuname(ipuname,ierr)
      if(ierr.ne.0) then
         release='8.3'
         sysname='sn4000'
      else
         lenname=0
         call pxfstrget(ipuname, 'sysname', sysname, lenname, ierr)
         lenname=0
         call pxfstrget(ipuname, 'release', release, lenname, ierr)
      endif
      do i=1,15
         if(sysname(i:i).eq.char(00)) sysname(i:i)=' '
         if(release(i:i).eq.char(00)) release(i:i)=' '
      enddo
      call pxfstructfree(ipuname,ierr)

c.... Print out statistical info from this run

      write(66,10700)
10700 format('VS ATEXPERT 9.0  05/19/99  92.3')
      t=timef()*0.001
      itf=t
      itf3=1000*(t-itf)
      t=second()
      is=t
      is3=1000*(t-is)
      call atx@write(66,'(3hDT ,2a9,i10,1h.,i3.3,i10,1h.,i3.3)',
     .                   date(),clock(),itf,itf3,is,is3)
      call atx@write(66,'(3hSY ,a8,1x,a8,1x,a8)',isysname,irelease,
     .                   machtype)
      if(ncstot.le.0) then
         xx=1.0
      else
         xx=float(nworks)/ncstot
      endif
      ncpu=xx
      ncpu2=100*(xx-ncpu)
      xx=maxmem*1.e-6
      mxm=xx
      mxm2=100*(xx-mxm)
      call atx@write(66,'(3hMX ,i2,i4,1h.,i2.2,i4,1h.,i2.2)',
     .              maxcpu,ncpu,ncpu2,mxm,mxm2)
      call atx@write(66,'(3hDS ,2i14)',
     .              ndiscons,tdiscons/max0(1,ndiscons))
      call atx@write(66,'(3hDP ,2i14)',
     .              ndisconp,tdisconp/max0(1,ndisconp))
      call atx@write(66,'(3hNP ,i6,2i14)',npr,nbpx,nunitask)

c.... Print all nonzero data for all PRs and LOOPs

      tumin=bpmin/10
      timin=bpmin+tumin
      do ipr=1,maxbp
         if(and(atxptr(ipr),MASKHWR).ne.0) then

            ippr=and(atxptr(ipr),MASKHWR)
            ippst=pr(PSTPtr)
            nx=0
            do while(ippst.ne.0)
               nx=nx+getNx()
               ippst=getpstPtr()
            enddo

c.......... Print PR info

            call atx@write(66,'PR ',
     .                  pr(SUBNAME)+iipatxnam-1,pr(SLVNAME)+iipatxnam-1,
     .                  nx,getNcs(),getNcpus(),getNUD())
            ippst=pr(PSTPtr)
            do while(ippst.ne.0)
               call atx@write(66,'PS ',pst(PSTNAME)+iipatxnam-1,
     .            getNx(),pst(minPST),pst(maxPST),pst(totPST)/getNx())
               ippst=getpstPtr()
            enddo
            if(getNtotp(totBPT).ne.0) then
               call atx@write(66,'(3hBP ,3i14)',getminp(mnxBPT),
     .                       getmaxp(mnxBPT),getavgp(totBPT))
            endif

            if(getNtotp(totSAT).ne.0) then
               call atx@write(66,'(3hSA ,3i14)',getminp(mnxSAT),
     .                       getmaxp(mnxSAT),getavgp(totSAT))
            endif

            iploop=getPtrp()
            if(iploop.eq.0) goto 10085

c.......... Print LOOP info

10000       continue

               Nu=getNi(NuTu)
               Ni=0
               do icpu=1,maxcpu
                  Ni=Ni+getNi(icpu)
               enddo
               niteru=getmaxl(mnxNuItr)
               if(niteru.eq.0) then
                 niteru=getmaxl(mnxNcItr)
               endif
               if(getType().eq.GUIDED) then
                  parm=getParm()
                  niter=(niteru+parm-1)/parm
               elseif(getType().eq.VECTOR) then
                  niter=(niteru+maxvl-1)/maxvl
               else
                  niter=niteru
               endif
               igarded='        '
               isavlast='        '
               if(getF(GUARDED).ne.0) garded='GUARDED'
               if(getF(SAVELAST).ne.0) savlast='SAVELAST'
               if(getType().eq.GUIDED) then
                  call atx@write(66,'(3hDO ,i5,8h GUIDED ,i12,2a9)',
     .                             getDo(),getParm(),igarded,isavlast)
               elseif(getType().eq.CASE) then
                  if(getF(ENDCASE).eq.0) then
                     call atx@write(66,'(3hDO ,i5,6h CASE ,2a9)',
     .                             getDo(),igarded,isavlast)
                  else
                     call atx@write(66,'(3hDO ,i5,9h ENDCASE ,2a9)',
     .                             getDo(),igarded,isavlast)
                  endif
               elseif(getType().eq.VECTOR) then
                  if(getF(GUARDED).eq.0) then
                     call atx@write(66,'(3hDO ,i5,8h VECTOR ,a8)',
     .                             getDo(),isavlast)
                  else
                     call atx@write(66,'(3hDO ,i5,8h VECRED ,a8)',
     .                             getDo(),isavlast)
                  endif
               else
                  call atx@write(66,'(3hDO ,i5,1x,2a9)',
     .                          getDo(),igarded,isavlast)
               endif
               if(getType().ne.CASE) then
                  if(Nu.gt.0)
     .              call atx@write(66,'(3hUI ,3i14)',getminl(mnxNuItr),
     .                             getmaxl(mnxNuItr),getavgl(NuIter))
                  if(Ni.gt.0)
     .              call atx@write(66,'(3hCI ,3i14)',getminl(mnxNcItr),
     .                             getmaxl(mnxNcItr),getavgl(NcIter))
               endif
               if(getNtotl(totILT).ne.0) then
                  call atx@write(66,'(3hIL ,3i14)',getminl(mnxILT),
     .                          getmaxl(mnxILT),getavgl(totILT))
               endif

               if(getNtotl(totBCT).ne.0) then
                  call atx@write(66,'(3hBC ,3i14)',getminl(mnxBCT),
     .                          getmaxl(mnxBCT),getavgl(totBCT))
               endif

               if(getNtotl(totPDT).ne.0) then
                  call atx@write(66,'(3hPD ,3i14)',getminl(mnxPDT),
     .                          getmaxl(mnxPDT),getavgl(totPDT))
               endif

               if(getNtotl(totIOT).ne.0) then
                  mint=getminl(mnxIOT)
                  maxt=getmaxl(mnxIOT)
                  avgt=getavgl(totIOT)
                  if(mint.eq.MXTIME) then
                     if(maxt.eq.MXTIME) then
                        mint=getavgl(totIOT)
                        maxt=mint
                     else
                        mint=maxt
                     endif
                  elseif(maxt.eq.MXTIME) then
                     maxt=mint
                  endif
                  call atx@write(66,'(3hIO ,3i14)',mint,maxt,avgt)
               endif

               if(getNtotl(totWT).ne.0) then
                  mint=getminl(mnxWT)
                  if(mint.ne.0) then
                     avgt=getavgl(totWT)
                     maxt=getmaxl(mnxWT)
                     if(mint.eq.MXTIME) then
                        if(maxt.eq.MXTIME) then
                           mint=getavgl(totWT)
                           maxt=mint
                        else
                           mint=maxt
                        endif
                     elseif(maxt.eq.MXTIME) then
                        maxt=mint
                     endif
                     call atx@write(66,'(3hWT ,3i14)',mint,maxt,avgt)
                  endif
               endif

               if(getNtotl(totLST).ne.0) then
                  call atx@write(66,'(3hLS ,3i14)',getminl(mnxLST),
     .                          getmaxl(mnxLST),getavgl(totLST))
               endif

               if(getPtrl().eq.0) then
                  Tws=getminp(mnxWST)
                  if(Tws.eq.MXTIME) then
                     Tws=0
                  else
                     Tws=Tws*nx/(Nu+Ni)
                  endif
               else
                  Tws=0
               endif

c............. Print out min max avg times

               if(Nu.ne.0) then
                  minTu=max0(tumin,getminl(mnxTu)*niter)
                  maxTu=max0(tumin,getmaxl(mnxTu)*niter)
                  avgTu=max0(tumin,getTi(NuTu)*niter/Nu)
                  call atx@write(66,'(3hTU ,i9,3i14)',Nu,
     .                           minTu,maxTu,avgTu)
               endif
               do icpu=1,maxcpu
                  Ni=getNi(icpu)
                  if(Ni.ne.0) then
                    minTi=max0(timin,niter*getminl(MAXCPUS+icpu))+Tws
                    maxTi=max0(timin,niter*getmaxl(MAXCPUS+icpu))+Tws
                    avgTi=max0(timin,getTi(icpu)*niter/Ni)+Tws
                    if(icpu.lt.10) then
                       call atx@write(66,'(1hT,i1,i10,3i14)',icpu,Ni,
     .                                minTi,maxTi,avgTi)
                     else
                       call atx@write(66,'(1hT,i2,i10,3i14)',icpu,Ni,
     .                                minTi,maxTi,avgTi)
                    endif
                  endif
               enddo

               iploop=getPtrl()

            if(iploop.ne.0) goto 10000

c.......... Print last PR info (Tws)

10085       if(getNtotp(totWST).ne.0) then
               call atx@write(66,'(3hWS ,3i14)',getminp(mnxWST),
     .                       getmaxp(mnxWST),getavgp(totWST))
            endif

         endif

      enddo

c.... Print ending serial time

      call atx@write(66,'ES ',iatxprv+iipatxnam-1,finalpst)

c.... Terminate user's code here

      close(66)

      return
            
      end
      subroutine atx@mklp(iplpptr)
      implicit integer(a-z)
      parameter(MAXCPUS=64)
      parameter(MXTIME=37777777777B)

c.... Indexes into the loop (parallel loop) array

c     parameter(N*T*=1..MAXCPUS)
c     parameter(mnxT*=MAXCPUS+1..2*MAXCPUS)
      parameter(NuTu=2*MAXCPUS+1)
      parameter(mnxTu=2*MAXCPUS+2)
      parameter(mnTuIOT=2*MAXCPUS+3)
      parameter(NuIter=2*MAXCPUS+4)
      parameter(mnxNuItr=2*MAXCPUS+5)
      parameter(NcIter=2*MAXCPUS+6)
      parameter(mnxNcItr=2*MAXCPUS+7)
      parameter(LoopPtr=2*MAXCPUS+8)
      parameter(mnxIOT=2*MAXCPUS+9)
      parameter(totIOT=2*MAXCPUS+10)
      parameter(mnxWT=2*MAXCPUS+11)
      parameter(totWT=2*MAXCPUS+12)
      parameter(mnxLST=2*MAXCPUS+13)
      parameter(totLST=2*MAXCPUS+14)
      parameter(mnxILT=2*MAXCPUS+15)
      parameter(totILT=2*MAXCPUS+16)
      parameter(mnxBCT=2*MAXCPUS+17)
      parameter(totBCT=2*MAXCPUS+18)
      parameter(mnxPDT=2*MAXCPUS+19)
      parameter(totPDT=2*MAXCPUS+20)
      parameter(LoopFlgs=2*MAXCPUS+21)
      parameter(LOOPSZ=2*MAXCPUS+21)

      pointer(iploop,loop(1))

      setmnxl(imin,imax)=or(shiftl(imin,32),imax)

c.... Allocate and initialize a new LOOP structure

      call hpalloc(iploop,LOOPSZ,ierr,0)
      if(ierr.ne.0) then
         print *,'Fatal ATEXPERT Error--Could not allocate LOOP space'
         call atx@abt(1)
      endif

      do icpu=1,MAXCPUS
         loop(icpu)=0
         loop(MAXCPUS+icpu)=setmnxl(MXTIME,0)
      enddo

      loop(NuTu)=0
      loop(mnxTu)=setmnxl(MXTIME,0)
      loop(mnTuIOT)=MXTIME
      loop(NuIter)=0
      loop(mnxNuItr)=setmnxl(0,0)
      loop(NcIter)=0
      loop(mnxNcItr)=setmnxl(0,0)
      loop(LoopPtr)=0
      loop(mnxIOT)=setmnxl(MXTIME,MXTIME)
      loop(totIOT)=0
      loop(mnxWT)=setmnxl(MXTIME,MXTIME)
      loop(totWT)=0
      loop(mnxLST)=setmnxl(MXTIME,0)
      loop(totLST)=0
      loop(mnxILT)=setmnxl(MXTIME,0)
      loop(totILT)=0
      loop(mnxBCT)=setmnxl(MXTIME,0)
      loop(totBCT)=0
      loop(mnxPDT)=setmnxl(MXTIME,0)
      loop(totPDT)=0
      loop(LoopFlgs)=0

      iplpptr=iploop
      return

      end
      integer function atx@startpr(slavfunc,slavargs)
cdir$ vfunction utqbp
      logical   utqbp
      if(utqbp(loc@(slavfunc),loc@(slavargs),-10000)) then

c....... Call succeeded--no higher level auto/microtasking existed

         atx@startpr=1
      else
         atx@startpr=0
      endif
      return
      end
      subroutine atx@slave(maxmem)
cdir$ stack
cdir$ vfunction utqgn,utqegn
      integer utqgn,utqegn
      integer iptr
      data ign/0/
      data numgd/59/
      ign=utqgn(numgd,ign)
      iptr=loc(ign)
      maxmem=max0(maxmem,iptr)
      ign=utqegn(numgd,ign)
      return
      end
      subroutine atx@write(unit,fmt,a1,a2,a3,a4,a5,a6,a7)
      implicit integer(a-z)
      integer unit,a1,a2,a3,a4,a5,a6,a7
      character*120 fmt
      dimension iary(7)
      character*640 line
      dimension iline(80)
      equivalence(line,iline)
      pointer(ipname,name(1))
      OPTIONAL :: a2, a3, a4, a5, a6, a7

      if(fmt(1:3).eq.'PR ') then
         line='PR      '
         ipname=and(a1,37777777777B)
         nmw=shiftr(a1,32)
         do i=1,nmw
            iline(1+i)=name(i)
         enddo
         ipname=and(a2,37777777777B)
         nsw=shiftr(a2,32)
         do i=1,nsw
            iline(2+nmw+i)=name(i)
         enddo
         blank='        '
         mask=shiftl(377B,56)
         do i=64,8,-8
            if(and(iline(1+nmw),mask).eq.000B)
     .         iline(1+nmw)=or(shiftl(shiftr(iline(1+nmw),i),i),
     .                         shiftr(blank,64-i))
            if(and(iline(2+nmw+nsw),mask).eq.000B)
     .         iline(2+nmw+nsw)=or(shiftl(shiftr(iline(2+nmw+nsw),i),i),
     .                         shiftr(blank,64-i))
            mask=shiftr(mask,8)
         enddo
         ix=(2+nmw+nsw)*8+1
         write(line(ix:ix+4*14),'(4i14)') a3,a4,a5,a6
         maxlen=ix+4*14-1
      elseif(fmt(1:3).eq.'PS ') then
         write(line(1:48),'(3hPS ,3i14,3x)') a3,a4,a5
         ipname=and(a1,37777777777B)
         nsw=shiftr(a1,32)
         do i=1,nsw
            iline(6+i)=name(i)
         enddo
         blank='        '
         mask=shiftl(377B,56)
         do i=64,8,-8
            if(and(iline(6+nsw),mask).eq.000B)
     .         iline(6+nsw)=or(shiftl(shiftr(iline(6+nsw),i),i),
     .                         shiftr(blank,64-i))
            mask=shiftr(mask,8)
         enddo
         ix=(6+nsw)*8+1
         maxlen=ix+14
         write(line(ix:maxlen),'(1x,i14)') a2
      elseif(fmt(1:3).eq.'ES ') then
         write(line(1:24),'(3hES ,i14,7x)') a2
         ipname=and(a1,37777777777B)
         nsw=shiftr(a1,32)
         do i=1,nsw
            iline(3+i)=name(i)
         enddo
         blank='        '
         mask=shiftl(377B,56)
         do i=64,8,-8
            if(and(iline(3+nsw),mask).eq.000B)
     .         iline(3+nsw)=or(shiftl(shiftr(iline(3+nsw),i),i),
     .                         shiftr(blank,64-i))
            mask=shiftr(mask,8)
         enddo
         maxlen=(3+nsw)*8
      else
         nargs=1
         iary(1)=a1
         if(PRESENT(a2)) then
            iary(2)=a2
            nargs = nargs + 1
         endif
         if(PRESENT(a3)) then
            iary(3)=a3
            nargs = nargs + 1
         endif
         if(PRESENT(a4)) then
            iary(4)=a4
            nargs = nargs + 1
         endif
         if(PRESENT(a5)) then
            iary(5)=a5
            nargs = nargs + 1
         endif
         if(PRESENT(a6)) then
            iary(6)=a6
            nargs = nargs + 1
         endif
         if(PRESENT(a7)) then
            iary(7)=a7
            nargs = nargs + 1
         endif
         write(line,fmt) (iary(i),i=1,nargs)
         maxlen=80
      endif

      j=1
      nblank=0
      do i=1,maxlen
         if(line(i:i).ne.' ') then
            line(j:j)=line(i:i)
            j=j+1
            nblank=0
         else
            nblank=nblank+1
            if(nblank.eq.1) then
               line(j:j)=' '
               j=j+1
            else
               if(nblank.gt.32) goto 20
            endif
         endif
      enddo

20    continue

      write(unit,'(640a1)') (line(i:i),i=1,j-1)
      return
      end
      subroutine atx@tr(it,icpu,type,idata)
      implicit integer(a-z)
      character*3 type
      character*3 event
      equivalence (ievent, event)
      parameter(TRSZ=4*2048)

      pointer(iptrace,trace(1))
      save iptrace

      data ixtrace/TRSZ/

      ievent = '        '
      event = type

      if(ixtrace.ge.TRSZ .or. event.eq.'END' .or. event.eq.'OFF') then
         if(ixtrace.eq.TRSZ) then
            call hpalloc(iptrace,TRSZ,ierr,0)
               if(ierr.ne.0) then
               print *,'Fatal ATEXPERT Error--',
     .                 'Could not allocate trace buffer'
               call atx@abt(1)
            endif
         else
            do i=1,ixtrace-4,4
               if(trace(i+2).eq.'BP ') then
                  write(6,'(i12,8h  1 BP  ,a8)') trace(i),trace(i+3)
               elseif(trace(i+2).eq.'EP ') then
                  write(6,'(i12,6h  1 EP)') trace(i)
               elseif(trace(i+2).eq.'BCS') then
                  write(6,'(i12,i3,4h BCS,i8)') trace(i),trace(i+1),
     .               trace(i+3)
               else
                  write(6,'(i12,i3,1x,a3,i8)') trace(i),trace(i+1),
     .               trace(i+2),trace(i+3)
               endif
            enddo
            if(event.ne.'END' .and. event.ne.'OFF')
     .		write(6,'(15hATX TRACE BREAK)')
         endif
         ixtrace=1
         if(event.eq.'END' .or. event.eq.'OFF') then
            if(event.eq.'OFF') then
               write(6,'(13hATX TRACE OFF)')
            else
               write(6,'(25hATX MAXIMUM TRACE REACHED)')
            endif
            return
         endif
      endif

      trace(ixtrace  )=it
      trace(ixtrace+1)=icpu
      trace(ixtrace+2)=ievent
      trace(ixtrace+3)=idata
      ixtrace=ixtrace+4

      return
      end
      subroutine atx@cs(ics,bcs,top,bot,itc,wsw,
     .                  iter,donm,type,savl,gard,parm)
cdir$ stack
cdir$ vfunction utqgn,utqegn
      implicit integer(a-z)

      parameter(MAXCPUS=64)
      parameter(MAXCS=16)
      parameter(CSSZ=5*MAXCPUS+6)
      parameter(INCCS=5*MAXCPUS*MAXCS+6*MAXCS)

c.... Parameters for remapping from old MAXCPUS=16 value

      parameter(OLDMAXCPUS=16)
      parameter(OLDMAXSZ=OLDMAXCPUS*MAXCS)

      integer bcs(MAXCPUS)
      integer top(MAXCPUS)
      integer bot(MAXCPUS)
      integer itc(MAXCPUS)
      integer wsw(MAXCPUS)
      integer iter,donm,type,savl,gard,parm

      common /ut@comm/ ut@subr,ut@slave,iatxrloc,ut@trips,
     .                 ut@sched,ut@case,ut@label,ut@savelast,
     .                 ut@guard,ut@param,ut@wait
      
      common /atx@com/ iatxmxcp,iatxcpu
      common /atx@com/ iatxpst,iatxbp,iatxep
      common /atx@com/ iatxbcs(MAXCPUS,MAXCS)
      common /atx@com/ iatxtop(MAXCPUS,MAXCS)
      common /atx@com/ iatxbot(MAXCPUS,MAXCS)
      common /atx@com/ iatxitc(MAXCPUS,MAXCS)
      common /atx@com/ iatxitr(MAXCS)
      common /atx@com/ iatxdo(MAXCS),iatxtype(MAXCS)
      common /atx@com/ iatxsavl(MAXCS),iatxgard(MAXCS)
      common /atx@com/ iatxparm(MAXCS)
      common /atx@com/ iatxwt(MAXCPUS,MAXCS)

      dimension ibcs(INCCS)
      equivalence(ibcs,iatxbcs)

c.... Equivalences for remapping from old MAXCPUS=16 value

      dimension ibcs16(OLDMAXCPUS, MAXCS)
      dimension itop16(OLDMAXCPUS, MAXCS)
      dimension ibot16(OLDMAXCPUS, MAXCS)
      dimension iitc16(OLDMAXCPUS, MAXCS)
      dimension iitr16 (MAXCS)
      dimension ido16  (MAXCS)
      dimension itype16(MAXCS)
      dimension isavl16(MAXCS)
      dimension igard16(MAXCS)
      dimension iparm16(MAXCS)
      dimension iwt16 (OLDMAXCPUS, MAXCS)

      equivalence(ibcs16(1,1), ibcs(1+0*OLDMAXSZ))
      equivalence(itop16(1,1), ibcs(1+1*OLDMAXSZ))
      equivalence(ibot16(1,1), ibcs(1+2*OLDMAXSZ))
      equivalence(iitc16(1,1), ibcs(1+3*OLDMAXSZ))
      equivalence(iitr16 (1),  ibcs(1+4*OLDMAXSZ+0*MAXCS))
      equivalence(ido16  (1),  ibcs(1+4*OLDMAXSZ+1*MAXCS))
      equivalence(itype16(1),  ibcs(1+4*OLDMAXSZ+2*MAXCS))
      equivalence(isavl16(1),  ibcs(1+4*OLDMAXSZ+3*MAXCS))
      equivalence(igard16(1),  ibcs(1+4*OLDMAXSZ+4*MAXCS))
      equivalence(iparm16(1),  ibcs(1+4*OLDMAXSZ+5*MAXCS))
      equivalence(iwt16 (1,1), ibcs(1+4*OLDMAXSZ+6*MAXCS))

      pointer(ipatxcs,atxcs(1))
      pointer(ipatxcso,atxcso(1))

      data atxcssz/0/
      data nxtstore/MAXCS/
      data ipatxcs/0/
      data iatxncs/0/
      data ign/0/
      data numgd/59/

c.... Return stored measurements from control structure ics

      if(ics.le.iatxncs) then

         if(iatxncs.le.MAXCS) then

            iter=iatxitr(ics)
            donm=iatxdo(ics)
            type=iatxtype(ics)
            savl=iatxsavl(ics)
            gard=iatxgard(ics)
            parm=iatxparm(ics)
            do icpu=1,iatxcpu
               bcs(icpu)=iatxbcs(icpu,ics)
               top(icpu)=iatxtop(icpu,ics)
               bot(icpu)=iatxbot(icpu,ics)
               itc(icpu)=iatxitc(icpu,ics)
               wsw(icpu)=iatxwt(icpu,ics)
            enddo

         else
         
            icsbk=((ics-1)/MAXCS)*INCCS
            ic=mod(ics-1,MAXCS)
            iter=atxcs(icsbk+4*MAXCPUS*MAXCS+ic+1)
            donm=atxcs(icsbk+4*MAXCPUS*MAXCS+ic+1+1*MAXCS)
            type=atxcs(icsbk+4*MAXCPUS*MAXCS+ic+1+2*MAXCS)
            savl=atxcs(icsbk+4*MAXCPUS*MAXCS+ic+1+3*MAXCS)
            gard=atxcs(icsbk+4*MAXCPUS*MAXCS+ic+1+4*MAXCS)
            parm=atxcs(icsbk+4*MAXCPUS*MAXCS+ic+1+5*MAXCS)
            do icpu=1,iatxcpu
               bcs(icpu)=atxcs(icsbk+ic*MAXCPUS+icpu)
               top(icpu)=atxcs(icsbk+ic*MAXCPUS+icpu+1*MAXCPUS*MAXCS)
               bot(icpu)=atxcs(icsbk+ic*MAXCPUS+icpu+2*MAXCPUS*MAXCS)
               itc(icpu)=atxcs(icsbk+ic*MAXCPUS+icpu+3*MAXCPUS*MAXCS)
               wsw(icpu)=atxcs(icsbk+ic*MAXCPUS+icpu+4*MAXCPUS*MAXCS+
     .                                                       6*MAXCS)
            enddo

         endif

c....... Fix overlap into itc by a store into atx@itr(0)

         if(ics.eq.MAXCS) then
            if(itc(MAXCPUS).ne.0) then
               if(top(MAXCPUS).eq.0) itc(MAXCPUS)=0
            endif
         endif

         return

      endif

      iter=0
      donm=0
      type=-1
      savl=0
      gard=0
      parm=1
      do icpu=1,iatxcpu
         bcs(icpu)=iatxep
         top(icpu)=iatxep
         bot(icpu)=iatxep
         itc(icpu)=0
         wsw(icpu)=0
      enddo
      return

      entry atx@cp(ics)

c.... If called from atx@ep, ics=-iatxncs;
c.... Else ics=current total # control structures

      if(ics.lt.0) then
         iatxncs=-ics
         if(iatxncs.le.MAXCS) return
      else
cdir$ suppress nxtstore
         ign=utqgn(numgd,ign)
         if(nxtstore.ge.(ics+MAXCS)) then
            ign=utqegn(numgd,ign)
            return
         endif
         iatxgard(MAXCS)=ut@guard
         if(iatxbcs(1,MAXCS).eq.0) iatxwt(1,MAXCS)=ut@wait
      endif

c.... The 1st cpu here executes this code and does the copy.

c.... If called using a 16-cpu interface, remap to the new 64-cpu arrays

      if(iatxdo(1).eq.0) then
         if(ics.lt.0) then
           mxics=and(-ics-1,MAXCS-1)+1
         else
           mxics=MAXCS
         endif
         do ic=1,mxics
           do icpu=1,OLDMAXCPUS
             iatxwt(icpu,ic)=iwt16(icpu,ic)
           enddo
           iatxitr (ic)=iitr16(ic)
           iatxdo  (ic)=ido16(ic)
           iatxtype(ic)=itype16(ic)
           iatxsavl(ic)=isavl16(ic)
           iatxgard(ic)=igard16(ic)
           iatxparm(ic)=iparm16(ic)
         enddo
         do ic=1,mxics
           do icpu=1,OLDMAXCPUS
             iatxitc(icpu,ic)=iitc16(icpu,ic)
             iatxbot(icpu,ic)=ibot16(icpu,ic)
             iatxtop(icpu,ic)=itop16(icpu,ic)
           enddo
         enddo
         do ic=mxics,1,-1
           do icpu=OLDMAXCPUS,1,-1
             iatxbcs(icpu,ic)=ibcs16(icpu,ic)
           enddo
         enddo
         do ic=mxics+1,MAXCS
           iatxitr (ic)=0
           iatxdo  (ic)=0
           iatxtype(ic)=0
           iatxsavl(ic)=0
           iatxgard(ic)=0
           iatxparm(ic)=0
           do icpu=1,OLDMAXCPUS
             iatxbcs(icpu,ic)=0
             iatxtop(icpu,ic)=0
             iatxbot(icpu,ic)=0
             iatxitc(icpu,ic)=0
             iatxwt (icpu,ic)=0
           enddo
         enddo
      endif

c.... Copy /atx@com/ arrays into dynamic storage every MAXCS control structures

c.... Allocate more space if necessary

      if(nxtstore*CSSZ.gt.atxcssz) then
         ipatxcso=ipatxcs
         oldsz=atxcssz
         atxcssz=atxcssz+INCCS
         call hpalloc(ipatxcs,atxcssz,ierr,0)
         if(ierr.ne.0) then
            print *,'Fatal ATEXPERT Error--',
     .              'Could not allocate control structure space'
            call atx@abt(1)
         endif
         do i=1,oldsz
            atxcs(i)=atxcso(i)
         enddo
         call hpdeallc(ipatxcso,ierr,0)
      endif

c.... Copy /atx@com/ here

      i=(nxtstore-MAXCS)*CSSZ
      do ic=1,INCCS
         atxcs(i+ic)=ibcs(ic)
      enddo

      if(ics.lt.0) then
         do icpu=1,iatxcpu
            if(atxcs(icpu).eq.0 .and. atxcs(i+icpu).ne.0) then
               nc=2
cdir$ novsearch
               do while(atxcs(icpu+(nc-1)*INCCS).eq.0)
                  nc=nc+1
               enddo
               atxcs(icpu)=atxcs(icpu+(nc-1)*INCCS)-10*(nc-1)*MAXCS
            endif
         enddo
         nxtstore=MAXCS
      else
         do ic=1,INCCS-MAXCS
            ibcs(ic)=0
         enddo
         do ic=1,MAXCS
            iatxparm(ic)=1
         enddo
         nxtstore=nxtstore+MAXCS
         ign=utqegn(numgd,ign)
      endif

      return
      end
      subroutine atx@quit()
      call atx@exit
      return
      end
      subroutine atx@abt(n)
      call atx@troff
      n=1/n
      stop 'ATX ERR'
      end
