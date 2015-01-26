!
!
!
module Grid
   !
   use RuntimeParameters
   !
   implicit none
   !
   public :: Grid_init
   !
   integer,          save :: ndim
   integer,          save :: nx,ny,nz
   integer,          save :: k2d,k3d 
   integer,          save :: nguard
   integer,          save :: ib,jb,kb
   integer,          save :: ie,je,ke
   integer,          save :: ibg,jbg,kbg
   integer,          save :: ieg,jeg,keg
   double precision, save :: dx,dy,dz 
   !
   !  The cell coordinates
   !  c: center coordinate of the cell
   !  l: left cell-face coordinate
   !  r: right cell-face coordinate
   !
   double precision, dimension(:),      allocatable :: xcCoord
   double precision, dimension(:),      allocatable :: ycCoord
   double precision, dimension(:),      allocatable :: zcCoord
   double precision, dimension(:),      allocatable :: xlCoord
   double precision, dimension(:),      allocatable :: ylCoord
   double precision, dimension(:),      allocatable :: zlCoord
   double precision, dimension(:),      allocatable :: xrCoord
   double precision, dimension(:),      allocatable :: yrCoord
   double precision, dimension(:),      allocatable :: zrCoord
   !
contains
!
!
!
subroutine Grid_init
   ! 
   implicit none
   !
   integer        :: i,j,k
   !
   ndim    = 1
   nx      = 100
   ny      = 1
   nz      = 1
   k2d     = 0
   k3d     = 0
   nguard  = 2
   !
   ! Precalculate loop indices as following:
   ! -----------------------------------------------------------------------------------------------
   !  ibg|              |    ib    |                 |     ie    |                   |     ieg     |
   ! -----------------------------------------------------------------------------------------------
   ! 
   ! | 1 | ... | nguard | nguard+1 | ... | ... | ... | nguard+nx | nguard+nx+1 | ... | 2*nguard+nx |
   ! -----------------------------------------------------------------------------------------------
   ! |   guard cells    |             valid cells                |          guard cells            |
   ! 
   !
   ! loop ranges including guardcells
   !
   ibg     = 1
   ieg     = nx+2*nguard 
   jbg     = 1
   jeg     = ny+2*k2d*nguard 
   kbg     = 1
   keg     = nz+2*k3d*nguard 
   !
   ! loop ranges excluding guardcells
   !
   ib      = nguard+1
   ie      = nx+nguard
   jb      = k2d*nguard+1
   je      = ny+k2d*nguard
   kb      = k3d*nguard+1
   ke      = nz+k3d*nguard
   !
   !
   !
   write(*,*) '----- Grid_init -------------------'
   write(*,*) 'grid parameters:'
   write(*,*) 'ndim:', ndim
   write(*,*) 'nx,ny,nz:', nx,ny,nz
   write(*,*) 'nguard:', nguard
   write(*,*) 'ib,jb,kb:', ib,jb,kb
   write(*,*) 'ie,je,ke:', ie,je,ke
   write(*,*) 'ibg,jbg,kbg:', ibg,jbg,kbg
   write(*,*) 'ieg,jeg,keg:', ieg,jeg,keg
   !
   !  allocate coordinate fields
   !
   allocate(xcCoord(ibg:ieg))
   allocate(ycCoord(jbg:jeg))
   allocate(zcCoord(kbg:keg))
   allocate(xlCoord(ibg:ieg))
   allocate(ylCoord(jbg:jeg))
   allocate(zlCoord(kbg:keg))
   allocate(xrCoord(ibg:ieg))
   allocate(yrCoord(jbg:jeg))
   allocate(zrCoord(kbg:keg))
   !
   !  inititialize the grid
   !
   if(xmax-xmin<0.d0) then
      write(*,*) 'ERROR in Grind_init:' 
      write(*,*) 'xmax < xmin' 
   endif
   if(ndim>1.and.ymax-ymin<0.d0) then
      write(*,*) 'ERROR in Grind_init:' 
      write(*,*) 'ymax < ymin' 
   endif
   if(ndim==3.and.zmax-zmin<0.d0) then
      write(*,*) 'ERROR in Grind_init:' 
      write(*,*) 'zmax < zmin' 
   endif
   !
   dx = (xmax-xmin)/nx
   dy = (ymax-ymin)/ny
   dz = (zmax-zmin)/nz
   !
   write(*,*) 'dx:', dx
   write(*,*) 'dy:', dy
   write(*,*) 'dz:', dz
   !
   write(*,*) 'initializing grid-coordinates'
   !
   do i=1,nx+2*nguard
      do j=1,ny+2*k2d*nguard
         do k=1,nz+2*k3d*nguard
            !
            ! The cell center-coordinates
            !
            xcCoord(i) = xmin - 1.d0*nguard*dx + 0.5d0*dx + (i-1)*dx
            ycCoord(j) = ymin - 1.d0*nguard*dy + 0.5d0*dy + (j-1)*dy
            zcCoord(k) = zmin - 1.d0*nguard*dz + 0.5d0*dz + (k-1)*dz
            !
            ! The left cell-face coordinates
            !
            xlCoord(i) = xmin - 1.d0*nguard*dx + (i-1)*dx
            ylCoord(j) = ymin - 1.d0*nguard*dy + (j-1)*dy
            zlCoord(k) = zmin - 1.d0*nguard*dz + (k-1)*dz
            !
            ! The right cell-face coordinates
            !
            xrCoord(i) = xmin - 1.d0*nguard*dx  + i*dx
            yrCoord(j) = ymin - 1.d0*nguard*dy + j*dy
            zrCoord(k) = zmin - 1.d0*nguard*dz + k*dz
            !
         enddo
      enddo
   enddo
   !
   write(*,*) '----- Grid_init done --------------'
   !
end subroutine Grid_init
!
end module Grid
