!
!
!
module Eos  
   !
   use Grid 
   use Database
   use RuntimeParameters
   use PhysicalConstants 
   !
   implicit none
   !
   public :: Eos_gamma
   !
   integer :: i,j,k
   !
   contains 
   !
   !
   !
   subroutine Eos_gamma
      !
      implicit none
      !
      do i=1+nguard,nx+nguard+1
         do j=1+k2d*nguard,ny+k2d*(nguard+1)
            do k=1+k3d*nguard,nz+k3d*(nguard+1)
               !
               !pres(i,j,k) = eint(i,j,k)*dens(i,j,k)*(gamma-1.d0)      
               eint(i,j,k) = pres(i,j,k)/dens(i,j,k)/(gamma-1.d0)      
               temp(i,j,k) = eint(i,j,k)*mu_mol*(gamma-1.d0)/gas_constant      
               !
            enddo  
         enddo  
      enddo  
      !       
   end subroutine Eos_gamma 
   !
end module Eos
!
