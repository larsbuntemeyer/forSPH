!
!
!
module Hydro
   !
   use Grid
   use Database
   !
   implicit none
   !
   !
contains
   !
   !
   !
   subroutine Hydro_init
      !
      implicit none
      !
      write(*,*) '----- Hydro_init ------------------'
      write(*,*) 'Hydro is not doing anything yet,   '
      write(*,*) 'because i have to admit, i was lazy'
      write(*,*) '----- Hydro_init done -------------'
      !
   end subroutine Hydro_init
   !
   !
   !
   subroutine Hydro_solve(dt)
      !
      implicit none
      !
      double precision, intent(in) :: dt
      !
      if(ndim>1) then
         write(*,*) '------------------------'
         write(*,*) 'ERROR in Hydro_solve:'
         write(*,*) 'only 1D possilbe!'
         write(*,*) '------------------------'
         stop
      endif
      !
   end subroutine Hydro_solve
   !
end module Hydro
