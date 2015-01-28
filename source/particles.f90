module particles

implicit none

type :: Particle
  real, dimension(3) :: pos,vel,acc
  real               :: mass=1.0
end type Particle

end module particles
