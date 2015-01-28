module Particles

implicit none

type :: Particle
  real, dimension(3) :: pos,vel,acc
  real               :: mass=1.0
end type Particle

integer :: N = 1

type(Particle), allocatable, dimension(:) :: P

contains


subroutine advanceParticlesForwardEuler(dt)

implicit none

real, intent(in) :: dt
integer          :: i,k

call computeForces

do i=1,N
  do k=1,3
     P(i)%pos(k) = P(i)%pos(k) + dt*P(i)%vel(k)
     P(i)%vel(k) = P(i)%vel(k) + dt*P(i)%acc(k) 
  enddo
enddo

end subroutine advanceParticlesForwardEuler


subroutine advanceParticlesLeapFrog(dt)

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


subroutine computeForces

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


real function totalEnergy()

real :: E_kin,E_pot,dist
real, dimension(3) :: rji
integer :: i,j,k
  
  E_kin = 0.0
  E_pot = 0.0

!  do i=1,N
!    E_kin = E_kin + 0.5*(P(i)%vel(1)**2+P(i)%vel(2)**2+P(i)%vel(3)**2)
!    E_pot = E_pot + (-1.0)/sqrt(P(i)%pos(1)**2+P(i)%pos(2)**2+P(i)%pos(3)**2)
!  enddo 

  do i=1,N
    do j=i+1,N
      dist = 0.0
      do k=1,3
        rji(k) = P(j)%pos(k) - P(i)%pos(k)
        dist = dist + rji(k)**2
      enddo
      dist = sqrt(dist)    
      E_pot = E_pot - (P(j)%mass**2)/dist
    enddo
    E_kin = E_kin + 0.5*P(i)%mass*(P(i)%vel(1)**2+P(i)%vel(2)**2+P(i)%vel(3)**2)
  enddo 

  totalEnergy = E_kin + E_pot

end function totalEnergy


end module Particles


!-----------------------------------------------------------
program forSPH

use Particles

implicit none

real, parameter    :: dt_out=0.01,pi=2.*asin(1.0)
integer, parameter :: steps=1000
integer, parameter :: io = 1
integer :: n_step, stat
real    :: time=0.0,dt=0.001,t_end=10.0,time_out=dt_out
real    :: E_begin, E_end
character(80)    :: filename="output.dat"

print*, 'Enter timestep: '
read(*,*) dt
print*, 'Duration of the run: '
read(*,*) t_end

!call setupCircle(6)
call setupEight

open(io, file=filename, iostat=stat)
write(io,*) P(1)%pos(1),P(1)%pos(2),P(1)%pos(3)

E_begin = totalEnergy()

do while(time<t_end)
  call advanceParticlesLeapFrog(dt)
  !call advanceParticlesForwardEuler(dt)
  if(time>time_out) then 
     write(io,*) P(1)%pos(1),P(1)%pos(2),P(1)%pos(3)
     time_out = time_out+dt_out
  endif
  time = time+dt
enddo

E_end = totalEnergy()

write(*,*) 'E_begin',E_begin
write(*,*) 'E_end',E_end
write(*,*) 'Error', (E_begin-E_end)/E_begin

close(io)

contains

subroutine setupCircle(nParticles)

implicit none

integer, intent(in) :: nParticles
integer :: i
real    :: phi,v_abs

allocate(P(nParticles))
N = nParticles

do i=1,N
  phi = (i-1)*2.0*pi/N
  P(i)%pos(1) = cos(phi)
  P(i)%pos(2) = sin(phi)
  P(i)%pos(3) = 0.0
  P(i)%acc(1) = 0.0
  P(i)%acc(2) = 0.0
  P(i)%acc(3) = 0.0
  P(i)%vel(1) = 0.0
  P(i)%vel(2) = 0.0
  P(i)%vel(3) = 0.0
enddo

call computeForces

v_abs = sqrt(-P(1)%acc(1))

do i=1,N
  phi = (i-1)*2.0*pi/N
  P(i)%vel(1) = -v_abs*sin(phi)
  P(i)%vel(2) =  v_abs*cos(phi)
  P(i)%vel(3) = 0.0
enddo

P(1)%vel(1) = P(1)%vel(1)*1.0001

end subroutine setupCircle


subroutine setupEight()

implicit none

N = 3
allocate(P(N))

  P(1)%pos(1) =  0.9700436 
  P(1)%pos(2) = -0.24308753
  P(1)%pos(3) =  0.0
  P(1)%vel(1) =  0.466203685
  P(1)%vel(2) =  0.43236573
  P(1)%vel(3) =  0.0

  P(2)%pos(1) = -P(1)%pos(1)
  P(2)%pos(2) = -P(1)%pos(2)
  P(2)%pos(3) = -P(1)%pos(3)
  P(2)%vel(1) =  P(1)%vel(1)
  P(2)%vel(2) =  P(1)%vel(2)
  P(2)%vel(3) =  P(1)%vel(3)

  P(3)%pos(1) =  0.0 
  P(3)%pos(2) =  0.0 
  P(3)%pos(3) =  0.0 
  P(3)%vel(1) = -2.0*P(1)%vel(1)
  P(3)%vel(2) = -2.0*P(1)%vel(2)
  P(3)%vel(3) = -2.0*P(1)%vel(3)

end subroutine setupEight

subroutine setupOneParticle

implicit none

N = 1

allocate(P(N))

P(1)%pos(1) = 1.0
P(1)%pos(2) = 0.0
P(1)%pos(3) = 0.0
P(1)%acc(1) = 0.0
P(1)%acc(2) = 0.0
P(1)%acc(3) = 0.0
P(1)%vel(1) = 0.0
P(1)%vel(2) = 0.5
P(1)%vel(3) = 0.0

end subroutine setupOneParticle

end program forSPH
