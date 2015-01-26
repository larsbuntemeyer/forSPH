program bsp
  implicit none
  
  type :: koord
    integer              :: id
    real, dimension( 3 ) :: x
  end type koord
  
  type( koord ) :: k = koord( 12, (/ 0.0, 1.5, 20.5 /) )
  write( *, * ) k

  k = koord( 13, (/ 5.5, 0.0, 0.5 /) )
  write( *, * ) k
  
! Ausgabe:
!   12   0.0   1.5   20.5
!   13   5.5  0.0   0.5
end program bsp
