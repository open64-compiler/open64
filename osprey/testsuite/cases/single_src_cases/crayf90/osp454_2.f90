PROGRAM t454
  TYPE t
    COMPLEX, DIMENSION(1)  :: z
  END TYPE t
  TYPE(t) :: s0 = t( RESHAPE((/(6.0, 7.0)/), (/1/)) )
  COMPLEX, DIMENSION(1) :: C
  C = s0%z
  WRITE (*, "(2F6.3)" ) C
END
!{ dg-output "6.000 7.000" }
