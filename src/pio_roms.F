      module pio_roms
#include "pio_config.h"
#include "cppdefs.opt"

#ifdef PIO
      use pio, only : PIO_init, PIO_rearr_subset, iosystem_desc_t, file_desc_t
      use pio, only : PIO_finalize, PIO_noerr, PIO_iotype_netcdf, PIO_createfile
      use pio, only : PIO_int, PIO_real, PIO_double, var_desc_t, PIO_redef, PIO_def_dim, PIO_def_var, PIO_enddef
      use pio, only : PIO_closefile, io_desc_t, PIO_initdecomp, PIO_write_darray
      use pio, only : PIO_freedecomp, PIO_clobber, PIO_read_darray, PIO_syncfile, PIO_OFFSET_KIND
      use pio, only : PIO_nowrite, PIO_openfile
      use pio_nf, only : PIO_inq_varid
      use mpi

      use param
      use hidden_mpi_vars
      implicit none

      private

      public :: initialize_pio

!! This class contains the data and functions for reading input files.

      type, public :: pioReader

        !> @brief Rank of processor running the code.
        integer :: myRank
        !> @brief Number of processors participating in MPI communicator.
        integer :: ntasks
        !> @brief Number of processors performing I/O.
        integer :: niotasks
        !> @brief Stride in the mpi rank between io tasks.
        integer :: stride
        !> @brief Number of aggregator.
        integer :: numAggregator
        !> @brief Start index of I/O processors.
        integer :: optBase
        !> @brief The ParallelIO system set up by @ref PIO_init.
        type(iosystem_desc_t) :: pioIoSystem
        !> @brief Contains data identifying the file.
        type(file_desc_t)     :: pioFileDesc
        !> @brief An io descriptor handle that is generated in @ref PIO_initdecomp.
        type(io_desc_t)       :: iodesc_2D_r
        type(io_desc_t)       :: iodesc_3D_r
        !> @brief Specifies the flavor of netCDF output.
        integer               :: iotype
        !> @brief The length of the dimension of the netCDF variable.
        integer, dimension(2) :: dimLen_2D_r
        integer, dimension(2) :: dimLen_2D_u
        integer, dimension(2) :: dimLen_2D_v
        integer, dimension(3) :: dimLen_3D_r
        integer, dimension(3) :: dimLen_3D_u
        integer, dimension(3) :: dimLen_3D_v

        integer :: xi_rho, eta_rho
        integer :: xi_u, eta_v
        integer :: N
        integer :: xi_start, eta_start, s_start

        !> @brief Array describing the decomposition of the data.
        integer, allocatable  :: fcompdof_2D_r(:)
        integer, allocatable  :: fcompdof_2D_u(:)
        integer, allocatable  :: fcompdof_2D_v(:)

        integer, allocatable  :: fcompdof_3D_r(:)
        integer, allocatable  :: fcompdof_3D_u(:)
        integer, allocatable  :: fcompdof_3D_v(:)

      contains

        !! Initialize the ParallelIO library. Also allocate
        !! memory to read data from the netCDF file.
        procedure,  public  :: init

        !! This subroutine creates the decomposition.
        procedure,  public  :: createDecomp

        !! This subroutine reads the data array from the netCDF input file.
        procedure,  public  :: ncread
        procedure,  public  :: ncread3

      end type pioReader

!! WRITER
!        !> @brief The netCDF dimension ID.
!        integer, dimension(2) :: pioDimId
!        !> @brief 1-based index of start of this processors data in full data array.
!        integer, dimension(2) :: fstart
!        !> @brief Size of data array for this processor.
!        integer, dimension(2) :: fend
!        !> @brief Number of elements handled by each processor.
!        integer, dimension(2) :: fcount
!
!        !> @brief Create netCDF output file.
!        !! This subroutine creates the netCDF output file for the example.
!        procedure,  public  :: createFile
!
!        !> @brief Define the netCDF metadata.
!        !! This subroutine defines the netCDF dimension and variable used
!        !! in the output file.
!        procedure,  public  :: defineVar
!
!        !> @brief Write the sample data to the output file.
!        !! This subroutine writes the sample data array to the netCDF
!        !! output file.
!        procedure,  public  :: writeVar
!
!        !> @brief Close the netCDF output file.
!        !! This subroutine closes the output file used by this example.
!        procedure,  public  :: closeFile
!
!        !> @brief Clean up resources.
!        !! This subroutine cleans up resources used in the example. The
!        !! ParallelIO and MPI libraries are finalized, and memory
!        !! allocated in this example program is freed.
!        procedure,  public  :: cleanUp
!
!        !> @brief Handle errors.
!        !! This subroutine is called if there is an error.
!        procedure,  private :: errorHandle


      type(pioReader), public :: pio_r

      contains

! ----------------------------------------------------------------------
!! Instantiate all the PIO Example class objects.

      subroutine initialize_pio

      implicit none

      call pio_r%init()
      call pio_r%createDecomp()

!      call pio_class2d_rho%init()
!      call pio_class2d_rho%createDecomp()

!      call pio_class2d_rho%init()
!      call pio_class2d_rho%createDecomp()

!      call pio_class2d_rho%init()
!      call pio_class2d_rho%createDecomp()

!      call pio_class2d_rho%init()
!      call pio_class2d_rho%createDecomp()

!      call pio_class2d_rho%init()
!      call pio_class2d_rho%createDecomp()

!      call pioExInst%closeFile()

      end subroutine initialize_pio

! ----------------------------------------------------------------------
!! Initialize the MPI and ParallelIO libraries. Also allocate
!! memory to write and read the sample data to the netCDF file.

      subroutine init(this)

        implicit none

        class(pioReader), intent(inout) :: this

        integer :: ierr,i,j,k
        integer :: xi_start, eta_start
        integer :: wbuf, tmp_idx

        integer tile

#include "compute_tile_bounds.h"

        ! Set up PIO for this object

        this%stride        = 1  ! 128
        this%numAggregator = 0
        this%optBase       = 1
        this%iotype        = PIO_iotype_netcdf !pnetcdf

        this%dimLen_2D_r(1)     = LLm+2
        this%dimLen_2D_r(2)     = MMm+2
        this%dimLen_2D_u(1)     = LLm+1
        this%dimLen_2D_u(2)     = MMm+2
        this%dimLen_2D_v(1)     = LLm+2
        this%dimLen_2D_v(2)     = MMm+1

        this%dimLen_3D_r(1)     = LLm+2
        this%dimLen_3D_r(2)     = MMm+2
        this%dimLen_3D_r(3)     = N
        this%dimLen_3D_u(1)     = LLm+1
        this%dimLen_3D_u(2)     = MMm+2
        this%dimLen_3D_u(3)     = N
        this%dimLen_3D_v(1)     = LLm+2
        this%dimLen_3D_v(2)     = MMm+1
        this%dimLen_3D_v(3)     = N

        this%niotasks = this%ntasks ! keep things simple - 1 iotask per MPI process

        call PIO_init(this%myRank,       ! MPI rank
     &       MPI_COMM_WORLD,             ! MPI communicator
     &       this%niotasks,              ! Number of iotasks (ntasks/stride)
     &       this%numAggregator,         ! number of aggregators to use
     &       this%stride,                ! stride
     &       PIO_rearr_subset,           ! do not use any form of rearrangement (can be BOX or SUBSET)
     &       this%pioIoSystem,           ! iosystem
     &       base=this%optBase)          ! base (optional argument)

        allocate(this%fcompdof_2D_r( this%xi_rho * this%eta_rho * N ))
        allocate(this%fcompdof_2D_u( this%xi_u   * this%eta_rho * N ))
        allocate(this%fcompdof_2D_v( this%xi_rho * this%eta_v   * N ))
        allocate(this%fcompdof_3D_r( this%xi_rho * this%eta_rho * N ))
        allocate(this%fcompdof_3D_u( this%xi_u   * this%eta_rho * N ))
        allocate(this%fcompdof_3D_v( this%xi_rho * this%eta_v   * N ))

        do j=1,this%eta_rho
          do i=1,this%xi_rho
            wbuf = (this%eta_start - 1 + j - 1) * this%dimLen_2D_r(1) + (this%xi_start - 1 + i)
            tmp_idx = (j - 1) * this%xi_rho + i
            this%fcompdof_2D_r(tmp_idx) = wbuf
          end do
        end do

        do k=1,N
          do j=1,this%eta_rho
            do i=1,this%xi_rho
              wbuf = (this%s_start - 1 + k - 1) * (this%dimLen_3D_r(1)*this%dimLen_3D_r(2)) +
     &               (this%eta_start - 1 + j - 1) * this%dimLen_2D_r(1) + (this%xi_start - 1 + i)
              tmp_idx = (k - 1) * ( this%xi_rho * this%eta_rho) + (j - 1) * this%xi_rho + i
              this%fcompdof_3D_r(tmp_idx) = wbuf
            end do
          end do
        end do

      end subroutine init
! ----------------------------------------------------------------------
      subroutine createDecomp(this)

        implicit none

        class(pioReader), intent(inout) :: this

        call PIO_initdecomp(this%pioIoSystem, PIO_double, this%dimLen_2D_r, this%fcompdof_2D_r,
     &       this%iodesc_2D_r)
        call PIO_initdecomp(this%pioIoSystem, PIO_double, this%dimLen_3D_r, this%fcompdof_3D_r,
     &       this%iodesc_3D_r)

      end subroutine createDecomp
! ----------------------------------------------------------------------
      subroutine ncread(this, fname, varName, arr, ierr)

        implicit none

        class(pioReader), intent(inout) :: this
        character(len=*) :: fname
        character(len=*) :: varName
        real,dimension(:,:),intent(inout)   :: arr ! array to be filled

        type(var_desc_t) :: varId
        integer :: ierr

        ierr = PIO_openfile(this%pioIoSystem, this%pioFileDesc, this%iotype, trim(fname))

        ierr = PIO_inq_varid(this%pioFileDesc, trim(varName), varId)

        call PIO_read_darray(this%pioFileDesc, varId, this%iodesc_2D_r,
     &  arr, ierr)

      end subroutine ncread

! ----------------------------------------------------------------------
      subroutine ncread3(this, fname, varName, arr, ierr)

        implicit none

        class(pioReader), intent(inout) :: this
        character(len=*) :: fname
        character(len=*) :: varName
        real,dimension(:,:),intent(inout)   :: arr ! array to be filled

        type(var_desc_t) :: varId
        integer :: ierr

        ierr = PIO_openfile(this%pioIoSystem, this%pioFileDesc, this%iotype, trim(fname))

        ierr = PIO_inq_varid(this%pioFileDesc, trim(varName), varId)

        call PIO_read_darray(this%pioFileDesc, varId, this%iodesc_3D_r,
     &  arr, ierr)

      end subroutine ncread3

! ----------------------------------------------------------------------
!      subroutine createFile(this)
!
!        implicit none
!
!        class(pioExampleClass), intent(inout) :: this
!
!        integer :: retVal
!
!        character(len=255) :: fname = "outputfile.nc"
!
!        retVal = PIO_createfile(this%pioIoSystem, this%pioFileDesc, this%iotype, fname, PIO_clobber)
!
!        call this%errorHandle("PIO: Could not create file", retVal)
!
!      end subroutine createFile
! ----------------------------------------------------------------------
!      subroutine defineVar(this)
!
!        implicit none
!
!        class(pioExampleClass), intent(inout) :: this
!
!        type(var_desc_t) :: varId
!        integer :: retVal
!
!        retVal = PIO_def_dim(this%pioFileDesc, 'x', fLEN , this%pioDimId(1))
!        call this%errorHandle("Could not define dimension x", retVal)
!
!        retVal = PIO_def_dim(this%pioFileDesc, 'y', fLEN , this%pioDimId(2))
!        call this%errorHandle("Could not define dimension x", retVal)
!
!        retVal = PIO_def_var(this%pioFileDesc, 'f', PIO_real, (/this%pioDimId(1), this%pioDimId(2)/), varId)
!        call this%errorHandle("Could not define variable foo", retVal)
!
!        retVal = PIO_enddef(this%pioFileDesc)
!        call this%errorHandle("Could not end define mode", retVal)
!
!      end subroutine defineVar
! ----------------------------------------------------------------------
!      subroutine writeVar(this)
!
!      implicit none
!
!        class(pioExampleClass), intent(inout) :: this
!
!        integer :: retVal
!
!        call PIO_write_darray(this%pioFileDesc, this%varId, this%iodescNCells,
!     &  this%f(this%fstart(1):this%fend(1),this%fstart(2):this%fend(2)), retVal)
!
!        call this%errorHandle("Could not write foo", retVal)
!        call PIO_syncfile(this%pioFileDesc)
!
!      end subroutine writeVar
! ----------------------------------------------------------------------
!      subroutine closeFile(this)
!
!        implicit none
!
!        class(pioExampleClass), intent(inout) :: this
!
!        call PIO_closefile(this%pioFileDesc)
!
!      end subroutine closeFile
! ----------------------------------------------------------------------
!      subroutine cleanUp(this)
!
!        implicit none
!
!        class(pioExampleClass), intent(inout) :: this
!
!        integer :: ierr
!
!        deallocate(this%fcompdof)
!        deallocate(this%f)
!
!        call PIO_freedecomp(this%pioIoSystem, this%iodescNCells)
!        call PIO_finalize(this%pioIoSystem, ierr)
!        call MPI_Finalize(ierr)
!
!      end subroutine cleanUp
! ----------------------------------------------------------------------
!      subroutine errorHandle(this, errMsg, retVal)
!
!        implicit none
!
!        class(pioExampleClass), intent(inout) :: this
!        character(len=*),       intent(in)    :: errMsg
!        integer,                intent(in)    :: retVal
!        integer :: lretval
!        if (retVal .ne. PIO_NOERR) then
!            write(*,*) retVal,errMsg
!            call PIO_closefile(this%pioFileDesc)
!            call mpi_abort(MPI_COMM_WORLD,retVal, lretval)
!        end if
!
!      end subroutine errorHandle
! ----------------------------------------------------------------------

#endif ! PIO

      end module pio_roms
