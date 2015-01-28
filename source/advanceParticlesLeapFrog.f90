
subroutine advanceParticlesLeapFrog(dt)

use sph, only: P,N=>number_of_particles

implicit none

real, intent(in) :: dt
integer          :: i,k
logical,save     :: init=.true.

if(init) then
   call computeForces
   init=.false.
endif

do i=1,N
  do k=1,3
     P(i)%vel(k) = P(i)%vel(k) + 0.5*dt*P(i)%acc(k) 
     P(i)%pos(k) = P(i)%pos(k) + 1.0*dt*P(i)%vel(k)
  enddo
enddo

call computeForces

do i=1,N
  do k=1,3
     P(i)%vel(k) = P(i)%vel(k) + 0.5*dt*P(i)%acc(k) 
  enddo
enddo

end subroutine advanceParticlesLeapFrog
