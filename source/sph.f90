module sph

use particles

implicit none

integer :: number_of_particles

type(Particle), allocatable, dimension(:) :: P

end module sph
