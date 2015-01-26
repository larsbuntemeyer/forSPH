!
!
!
module RuntimeParameters
!
   implicit none
!
   public :: RuntimeParameters_init
!
!  time
!
   integer          , save :: n_max
   double precision , save :: t_max
!
!  more
!
   double precision        :: xmin,xmax
   double precision        :: ymin,ymax
   double precision        :: zmin,zmax
!
   double precision        :: cfl
   double precision        :: dtmin
   double precision        :: dtmax
   double precision        :: dtini
!
   double precision        :: gamma
   double precision        :: mu_mol
!
   character(80)           :: fl
!
contains
!
!
!
subroutine RuntimeParameters_init
   !
   write(*,*) '----- Runtime_parameters_init -----'
   write(*,*) 'initializing runtime parameters'
   !
   xmin=0.d0
   ymin=0.d0
   zmin=0.d0
   xmax=1.d0
   ymax=0.d0
   zmax=0.d0
   !
   cfl = 0.8
   dtmin = 1.d-10
   dtmax = 1.d0
   dtini = 1.d-10
   n_max = 30 
   gamma = 5.d0/3.d0
   mu_mol = 1.0
   !
   fl  = 'superbee'
   !
   write(*,*) '----- Runtime_parameters_init done-'
   !
end subroutine RuntimeParameters_init
!
end module RuntimeParameters
