
      real  :: SizeX,SizeY
      real  :: f0,beta
      real  :: x0,y0,dx,dy
      real  :: i,j

      ! Additional vars that are needed
      real  :: L2


      SizeX = 320.0e3  !! Domain size in x-direction [m]
      SizeY = 320.0e3  !! Domain size in y-direction [m]

      f0 = 1.0e-4      !! Coriolis, [1/s]
      beta = 0

      dx = SizeX/gnx   !! grid size in x-direction
      dy = SizeX/gny

# ifdef MPI
      x0=dx*dble(iSW_corn)             ! Coordinates of south-west
      y0=dy*dble(jSW_corn)             ! corner of MPI subdomain
# else
      x0=0. ; y0=0.
# endif

      do j=-1,ny+2          ! Extended ranges for x,y arrays
        do i=-1,nx+2
          xr(i,j)=x0+dx*(dble(i)-0.5D0)
          yr(i,j)=y0+dy*(dble(j)-0.5D0)

          pm(i,j)=1./dx
          pn(i,j)=1./dy
        enddo
      enddo


      x0=SizeX/2.   ! Define center of the domain
      y0=SizeY/2.
      do j=-1,ny+2          ! Extended ranges for x,y arrays
        do i=-1,nx+2
          f(i,j)=f0+beta*( yr(i,j)-y0 )
# if defined NONTRAD_COR
!         feta(i,j) = f0*cos(pi/4)
!         fxi(i,j)  = f0*sin(pi/4)
# endif
        enddo
      enddo

      L2 = 4.0e4**2
      do j=-1,ny+2
        do i=-1,nx+2
          h(i,j)=5000-4500*exp( -( (xr(i,j)-x0)**2+(yr(i,j)-y0)**2)/L2)
        enddo
      enddo

# ifdef MASKING
      do j=-1,ny+2
        do i=-1,nx+2
          ! default is water
          rmask(i,j) = 1
        enddo
      enddo
# endif