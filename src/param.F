      module param

      ! Master module containing key roms variables used throughout the code
! and by other modules. Direct copy of param.h into module format.
#include "cppdefs.opt"
#ifdef MPI
      use mpi_f08, only: mpi_comm
#endif
      implicit none

! DevinD: normally this was above param.opt in subroutines but since compiled alone need it here

! Need this here above param.opt for BGC to work:
      integer, parameter :: itemp=1,isalt=2

#include "param.opt"
      integer, public :: NP_XI = 1
      integer, public :: NP_ETA = 1
      integer, public :: Lm, Mm
      namelist /PARAM_SETTINGS/ NP_XI, NP_ETA
! Array dimensions and bounds of the used portions of sub-arrays:

#ifdef MPI
      integer :: nnodes,nsize

      integer mynode,  iSW_corn, jSW_corn,
     &     iwest, ieast, jsouth, jnorth
      type(mpi_comm) :: ocean_grid_comm
#endif

! Derived dimension parameters, number of tracers and tracer
! identification indices:

!     integer, parameter :: padd_X=(Lm_old+2)/2-(Lm_old+1)/2,
!    &                      padd_E=(Mm_old+2)/2-(Mm_old+1)/2
      integer, parameter :: padd_X= 0,padd_E= 0

# ifdef EW_PERIODIC
      logical,parameter :: ew_periodic =.true.
# else
      logical,parameter :: ew_periodic =.false.
# endif
# ifdef NS_PERIODIC
      logical,parameter :: ns_periodic =.true.
# else
      logical,parameter :: ns_periodic =.false.
# endif

# ifdef OBC_WEST
      logical,parameter :: obc_west =.true.
# else
      logical,parameter :: obc_west =.false.
# endif
# ifdef OBC_EAST
      logical,parameter :: obc_east =.true.
# else
      logical,parameter :: obc_east =.false.
# endif
# ifdef OBC_NORTH
      logical,parameter :: obc_north=.true.
# else
      logical,parameter :: obc_north=.false.
# endif
# ifdef OBC_SOUTH
      logical,parameter :: obc_south=.true.
# else
      logical,parameter :: obc_south=.false.
# endif

      contains

      subroutine read_nml_param

!     Read the "FRC_OUTPUT_SETTINGS" section of the namelist file
      integer ::  namelist_unit, ios
      character(len=20) :: sr_name = "read_nml_frc_output"
      character(len=512) :: msg = ""
      character(len=13) :: namelist_fname = "namelist.nml"
! Read namelist

      open (newunit=namelist_unit, file=namelist_fname, status="old",
     &     action="read", iostat=ios)
      if (ios/=0) then
         write(*,*) "ERROR [param.F]: could not open namelist file"
         error stop
      end if
      rewind(namelist_unit)

      read (unit=namelist_unit, nml=PARAM_SETTINGS, iostat=ios, iomsg=msg)

      if (ios /= 0) then
         write(*,*) "ERROR [param.F]: could not read section ",
     &        "PARAM_SETTINGS of namelist file"
      end if
      close(namelist_unit)
#ifdef MPI
      Lm=(LLm+NP_XI-1)/NP_XI
      Mm=(MMm+NP_ETA-1)/NP_ETA
      if (mynode==0) then
         print *, "OI NP_XI = ", np_xi
         print *, "OI NP_ETA = ", np_eta
         print *, "OI Lm = ", Lm
         print *, "OI Mm = ", Mm
      end if
#else
      Lm = LLm
      Mm = MMm
#endif


      end subroutine read_nml_param


      end module
