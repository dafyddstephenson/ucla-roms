      module dimensions

      ! ========================================
      ! Contains global and subdomain dimensions
      ! ========================================

      ! coded by Jeroen Molemaker
      use param, only: llm, mmm, n, lm, mm, np_eta, np_xi
      use pio_roms, only: pio_i0, pio_i1, pio_j0, pio_j1

      implicit none
      private

#include "cppdefs.opt"

      integer,parameter, public :: gnx = LLm
      integer,parameter, public :: gny = MMm
      integer,parameter, public ::  nz = N

      integer,parameter, public :: npx = np_xi
      integer,parameter, public :: npy = np_eta

      integer,parameter, public ::  bf = 2 ! allocation buffer size


      integer,public            :: nx   ! Can vary between subdomains so not compile time parameters
      integer,public            :: ny

      integer,public :: i0,i1,j0,j1     ! accounts for physical boundary buffers
      integer, public ::  xi_rho, xi_u, eta_rho, eta_v
      integer,public :: inode,jnode     ! location of subdomain in the grid
      integer,public :: x0,x1,y0,y1

# ifdef ANA_GRID
      logical,parameter,public :: analytical_grid = .true.
# else
      logical,parameter,public :: analytical_grid = .false.
# endif

      public :: init_dimensions
      character(len=256), public :: grdname
      character(len=:), allocatable, public :: grdname_full
      contains

! ----------------------------------------------------------------------
      subroutine init_dimensions  ![
      implicit none
      ! need to do this here because Lm, Mm change in mpi_setup and are not known at compile time.

c$$$      nx = Lm
c$$$      ny = Mm
      Lm = nx
      Mm = ny

      i0=1; i1=nx; j0=1; j1=ny

      if (inode==0)        i0=0
      if (inode==NP_XI-1)  i1=nx+1
      if (jnode==0)        j0=0
      if (jnode==NP_ETA-1) j1=ny+1

#ifdef PIO
      x0 = i0-pio_i0
      x1 = i1+pio_i1
      y0 = j0-pio_j0
      y1 = j1+pio_j1
#else
      x0 = i0
      x1 = i1
      y0 = j0
      y1 = j1
#endif

      end subroutine init_dimensions  !]

! ----------------------------------------------------------------------

      end module dimensions
