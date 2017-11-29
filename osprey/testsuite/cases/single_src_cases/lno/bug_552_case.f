!FFLAGS:-O3
!This is the testcase of bug #552
!This testcase is extracted from 454.calculix@spec2006
      subroutine e_c3d(co,s,mattyp)
      implicit none
      integer mattyp,j,k,l,i,m,n

      real*8 co(3,*),s(60,60),w(3,3),anisox(3,3,3,3),vo(3,3),weight

      if(mattyp.eq.1) then
         do j=1,2
            do k=1,2
               do i=1,2
                  s(1,j)=s(1,j) + weight
                  do m=1,2
                     do n=1,2
                        s(1,j)=s(1,j)
     &                       +anisox(m,k,n,i)
     &                       *w(k,i)*vo(1,m)*vo(j,n)
     &                       *weight
                     enddo
                  enddo
               enddo
            enddo
         enddo
      endif
      return
      end

