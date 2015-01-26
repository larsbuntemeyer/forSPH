!
!
!
module Driver
   !
   use RuntimeParameters
   use Grid
   use Database
   use Hydro
   use Simulation
   use Io
   use Eos
   !
   implicit none
   !
   public  :: Driver_init, Driver_evolve,   &
              Driver_finish
   private :: get_cfl_timestep
   !
contains
!
!
!
subroutine Driver_init
   !
   implicit none
   !
   write(*,*) '----- Driver_init_nerd-------------'
   write(*,*) 'driver parameters:'
   write(*,*) 'n_max:', n_max
   write(*,*) 't_max:', t_max
   !
   call RuntimeParameters_init
   call Grid_init
   call Database_init
   call Hydro_init
   call Io_init
   call Simulation_init_domain
   call Eos_gamma 
   !
   write(*,*) '----- Driver_init_nerd done--------'
   !
end subroutine Driver_init
!
!
!
subroutine Driver_evolve
   !
   implicit none
   !
   integer          :: step
   double precision :: current_time
   double precision :: dt
   !
   write(*,*) '----- Driver_evolve_nerd ----------'
   !
   !
   current_time = 0.d0
   dt = dtini
   !
   do step=1,n_max
      !
      dt = get_cfl_timestep()
      !
      current_time = current_time + dt  
      !
      call Hydro_solve(dt) 
      call Eos_gamma 
      !
      write(*,'(I5,2D18.4)') step,current_time,dt
      !
   enddo
   !
   write(*,*) '----- Driver_evolve_nerd done -----'
   !
end subroutine Driver_evolve
!
!
!
subroutine Driver_finish
   !
   implicit none
   !
   write(*,*) '----- Driver_finish_nerd ----------'
   write(*,*) 'writing to file...'
   !
   call Io_write_to_file
   !
   write(*,*) 'writing to file... done'
   write(*,*) '----- Driver_finish_nerd done------'
   !
end subroutine Driver_finish
!
!
!
double precision function get_cfl_timestep()
   !
   implicit none
   !
   double precision :: dt 
   double precision :: vmax
   !
   vmax = maxval(abs(u))
   if(ndim.ge.2) vmax = max(vmax,maxval(abs(v)))
   if(ndim.ge.3) vmax = max(vmax,maxval(abs(w)))
   !
   if(vmax>0.d0) then
      dt = cfl*dx/vmax
   else
      dt = dtmax
   endif
   !
   if(dt.lt.dtmin) then
      write(*,*) 'WARNING: cfl timestep is less than minimum timestep'
      write(*,*) 'using dtmin'
      dt = dtmin
   endif
   !
   get_cfl_timestep = dt
   !
end function get_cfl_timestep
!
end module Driver
