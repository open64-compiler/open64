!FFLAGS:-O3
!This is the testcase of bug #549
!This testcase is extracted from 410.bwaves@spec2006
      subroutine shell(nx,ny,nz,conf)
      implicit none
      integer nx,ny,nz,ni,n,i,j,k,conf
      real*8 q(5,nx,ny,nz), t6, t7
      if (conf.EQ.0) then
            do j=ny/2-1,ny/2+1
               do i=nx/2-1,nx/2+1
                 q(1,i,j,1)=2
              enddo
           enddo
      else
         do k=1,nz
            do j=1,ny
               t7=j-ny
               do i=1,nx
                  t6= i-nx  + t7 + 8
                  if (t6.LE. 1.0) then
                     q(1,i,j,k)=1
                     q(2,i,j,k)=2
                     q(3,i,j,k)=3
                     q(4,i,j,k)=4
                     q(5,i,j,k)=5
                  endif
               enddo
            enddo
         enddo
      endif
      call jacobian(q,nx,ny,nz)
      return
      end

      subroutine jacobian(q,nx,ny,nz)
      implicit none
      integer nx,ny,nz
      real*8 q(5,nx,ny,nz)
      write(*, *) q(1,1,1,1)
      return
      end

      program test
      implicit none
      call shell(10,10,10,10)
      end 


      
