!CFLAGS:-O2 -IPA
!This is the testcase of bug #458
!This testcase is extracted from 465.tonto@spec2006

module ycwu_module
implicit none

public rysfun_
interface rysfun_
        module procedure rysfun
end interface

contains

subroutine rysfun(ff)
!
integer(kind=kind(1))  :: m
real(kind=kind(1.0d0)), dimension(:), intent(out) :: ff

m = 4

do
if (m<1) exit
ff(m)=1
m=m-1
end do

end subroutine
end module


program ycwu_test

use ycwu_module
real(kind=kind(1.0d0)), dimension(13) :: ff

call rysfun_(ff)
end
