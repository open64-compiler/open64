!FFLAGS:-O3
!This is the testcase of bug #561
!This testcase is extracted from 481.wrf@spec2006

subroutine bar(i,j,k,l,m,n,o,p)
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: i,j,k,l,m,n,o,p
endsubroutine bar

subroutine foo(kts,kte,LTOP, NSTEP, &
     OMG,FXM, &
     QLPA,QIPA,QRPA,QSPA, &
     DETLQ,DETIC,RAINFB,SNOWFB &
     )
  IMPLICIT NONE
  INTEGER, INTENT(IN   ) :: kts,kte,LTOP,NSTEP
  REAL, DIMENSION( kts:kte+1 ) , INTENT(IN   ) :: OMG    
  REAL, DIMENSION( kts:kte ), INTENT(IN   )  :: FXM,DETLQ,DETIC,RAINFB,SNOWFB
  REAL, DIMENSION( kts:kte ), INTENT(INOUT   )  :: QLPA,QIPA,QRPA,QSPA
  INTEGER :: NK,NTC
  REAL, DIMENSION( kts:kte ) :: QLFXIN,QIFXIN,QRFXIN,QSFXIN,QLFXOUT,QIFXOUT,QRFXOUT,QSFXOUT
  
  DO NTC=1,NSTEP
     DO NK=1,LTOP
        QLFXIN(NK)=0.
        QLFXOUT(NK)=0.
        QIFXIN(NK)=0.
        QIFXOUT(NK)=0.
        QRFXIN(NK)=0.
        QRFXOUT(NK)=0.
        QSFXIN(NK)=0.
        QSFXOUT(NK)=0.
     ENDDO
     DO NK=2,LTOP
        IF(OMG(NK).LE.0.)THEN
           QLFXIN(NK)=-FXM(NK)*QLPA(NK-1)
           QIFXIN(NK)=-FXM(NK)*QIPA(NK-1)
           QRFXIN(NK)=-FXM(NK)*QRPA(NK-1)
           QSFXIN(NK)=-FXM(NK)*QSPA(NK-1)
           QLFXOUT(NK-1)=QLFXOUT(NK-1)+QLFXIN(NK)
           QIFXOUT(NK-1)=QIFXOUT(NK-1)+QIFXIN(NK)
           QRFXOUT(NK-1)=QRFXOUT(NK-1)+QRFXIN(NK)
           QSFXOUT(NK-1)=QSFXOUT(NK-1)+QSFXIN(NK)
        ELSE
           QLFXOUT(NK)=FXM(NK)*QLPA(NK)
           QIFXOUT(NK)=FXM(NK)*QIPA(NK)
           QRFXOUT(NK)=FXM(NK)*QRPA(NK)
           QSFXOUT(NK)=FXM(NK)*QSPA(NK)
           QLFXIN(NK-1)=QLFXIN(NK-1)+QLFXOUT(NK)
           QIFXIN(NK-1)=QIFXIN(NK-1)+QIFXOUT(NK)
           QRFXIN(NK-1)=QRFXIN(NK-1)+QRFXOUT(NK)
           QSFXIN(NK-1)=QSFXIN(NK-1)+QSFXOUT(NK)
        ENDIF
     ENDDO
     DO NK=1,LTOP
        QLPA(NK)=QLPA(NK)+(QLFXIN(NK)+DETLQ(NK)-QLFXOUT(NK))
        QIPA(NK)=QIPA(NK)+(QIFXIN(NK)+DETIC(NK)-QIFXOUT(NK))
        QRPA(NK)=QRPA(NK)+(QRFXIN(NK)-QRFXOUT(NK)+RAINFB(NK))
        QSPA(NK)=QSPA(NK)+(QSFXIN(NK)-QSFXOUT(NK)+SNOWFB(NK))
     ENDDO
  ENDDO
  call bar2()
  call  bar(1,2,3,4,5,6,7,8)
end subroutine foo

subroutine bar2()
  write(*,*) "hello"
end subroutine bar2

program aa
  IMPLICIT NONE
  INTEGER :: i,kts,kte,LTOP,NSTEP
  REAL, DIMENSION( 1:3 ) :: OMG    
  REAL, DIMENSION( 1:2 )  :: FXM,DETLQ,DETIC,RAINFB,SNOWFB,EMSD
  REAL, DIMENSION( 1:2 )  :: QLPA,QIPA,QRPA,QSPA
  REAL:: DTIME
  
  kts=1
  kte=2
  LTOP=2
  NSTEP=2
  do i=kts,kte
     OMG(i) = 0.0 + i*10
     fxm(i) = 0.1 + i*10
     detlq(i) = 0.3 + i*10
     qlpa(i) = 0.5 + i*10
     qIPA(i) = 0.6 + i*10
     qrpa(i) = 0.7 + i*10
     qspa(i) = 0.8 + i*10
     detlq(i) = 0.9 + i*10
     detic(i) = 1.0 + i*10
     rainfb(i) = 1.1 + i*10
     snowfb(i) = 1.2 + i*10
  enddo

  omg(kte+1) = 9999.0

  write (*,*) qlpa
  call foo(kts,kte,LTOP, NSTEP, &
     OMG,FXM, &
     QLPA,QIPA,QRPA,QSPA, &
     DETLQ,DETIC,RAINFB,SNOWFB &
     )
  write (*,*) qlpa
end program aa
