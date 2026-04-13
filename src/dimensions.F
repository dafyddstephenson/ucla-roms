      module dimensions

#include "cppdefs.opt"
      ! ========================================
      ! Contains global and subdomain dimensions
      ! ========================================

      ! coded by Jeroen Molemaker
      use param, only: llm, mmm, n, lm, mm, np_eta, np_xi
      use insert_node_mod, only: insert_node
#ifdef PARALLEL_IO
      use pio_roms, only: use_pio,pio_i0, pio_i1, pio_j0, pio_j1
#endif

      implicit none
      private

!#include "cppdefs.opt"

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
      integer,public :: x0,x1,y0,y1,x_,y_

# ifdef ANA_GRID
      logical,parameter,public :: analytical_grid = .true.
# else
      logical,parameter,public :: analytical_grid = .false.
# endif

      public :: init_dimensions, read_grid_nml
      character(len=10) :: module_name = "dimensions"
      character(len=256), public :: grdname = "roms_grid.nc"
      namelist /GRID_SETTINGS/ grdname
      contains

!     ----------------------------------------------------------------------
      subroutine read_grid_nml
      use param, only: mynode, nsize
      use error_handling_mod, only: error_log
      use namelist_mod, only: open_namelist_file
      use utils_mod, only: lenstr
!     Read the "GRID_SETTINGS" section of the namelist file
      integer ::  namelist_unit, ios, lstr
      integer :: testunit
c$$$      character(len=256) :: fname
      character(len=20) :: sr_name = "read_grid_nml"
      character(len=512) :: msg = ""
      ! Read namelist
      call open_namelist_file(namelist_unit)
      rewind(namelist_unit)

      read (unit=namelist_unit, nml=GRID_SETTINGS, iostat=ios, iomsg=msg)

      if (ios /= 0) then
         call error_log%raise_global(
     &   context = module_name//'/'//sr_name,
     &   info='could not read GRID_SETTINGS'
     &        //' section of namelist file: '
     &        //trim(msg)
     &      )
      end if


      lstr = lenstr(grdname)

# if defined(MPI) && defined(PARALLEL_FILES) && !defined(PARALLEL_IO)
      call insert_node(grdname, lstr, mynode, nsize)
# endif
      open(newunit=testunit, file=trim(grdname), status='old',
     &     iostat=ios)
      if (ios /= 0) then
         call error_log%raise_from_rank(
     &        info="Cannot find input file '" // trim(grdname) // "'",
     &        context=module_name)
         return
      end if
      close(testunit)

      mpi_master_only write(*,'(1x,2A)')
     &      'grid file: ', trim(grdname)

      close(namelist_unit)

      end subroutine read_grid_nml

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

#ifdef PARALLEL_IO
      if (use_pio) then
        x_ = 1-pio_i0
        x0 = i0-pio_i0
        x1 = i1+pio_i1
        y_ = 1-pio_j0
        y0 = j0-pio_j0
        y1 = j1+pio_j1
      else
#else
      x_ = 1
      x0 = i0
      x1 = i1
      y_ = 1
      y0 = j0
      y1 = j1
#endif

#ifdef PARALLEL_IO
      endif
#endif

      end subroutine init_dimensions  !]

! ----------------------------------------------------------------------

      end module dimensions
