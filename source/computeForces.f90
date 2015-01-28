

subroutine computeForces

use sph, only: P,N=>number_of_particles

implicit none

real    :: dist,dist3
real, dimension(3) :: rji
integer :: i,j,k

do i=1,N
  do k=1,3
    P(i)%acc(k) = 0.0
  enddo
enddo

do i=1,N
  do j=i+1,N
    dist = 0.0
    dist3 = 0.0
    do k=1,3
      rji(k) = P(j)%pos(k) - P(i)%pos(k)
      dist   = dist + rji(k)**2
    enddo
    dist3 = dist*sqrt(dist)
    do k=1,3
      P(i)%acc(k) = P(i)%acc(k) + P(j)%mass*rji(k)/dist3
      P(j)%acc(k) = P(j)%acc(k) - P(i)%mass*rji(k)/dist3
    enddo
  enddo
enddo

end subroutine computeForces

