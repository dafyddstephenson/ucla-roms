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
      use roms_read_write
      implicit none

      private

      !> @brief Rank of processor running the code.
      integer, public :: pio_myRank
      !> @brief Number of processors participating in MPI communicator.
      integer, public :: pio_ntasks
      !> @brief Number of processors performing I/O.
      integer :: pio_niotasks
      !> @brief Stride in the mpi rank between io tasks.
      integer :: pio_stride
      !> @brief Number of aggregator.
      integer :: pio_numAggregator
      !> @brief Start index of I/O processors.
      integer :: pio_optBase
      !> @brief The ParallelIO system set up by @ref PIO_init.
      type(iosystem_desc_t) :: pio_IoSystem
      !> @brief Contains data identifying the file.
      type(file_desc_t)     :: pio_FileDesc
      !> @brief An io descriptor handle that is generated in @ref PIO_initdecomp.
      type(io_desc_t)       :: pio_desc_2D_r
      type(io_desc_t)       :: pio_desc_3D_r
      !> @brief Specifies the flavor of netCDF output.
      integer               :: pio_type
      !> @brief The length of the dimension of the netCDF variable.
      integer, dimension(2) :: pio_dimLen_2D_r
      integer, dimension(2) :: pio_dimLen_2D_u
      integer, dimension(2) :: pio_dimLen_2D_v
      integer, dimension(3) :: pio_dimLen_3D_r
      integer, dimension(3) :: pio_dimLen_3D_u
      integer, dimension(3) :: pio_dimLen_3D_v

      !integer :: xi_rho, pio_eta_rho
      !integer :: xi_u, eta_v
      !integer :: N
      integer, public :: pio_xi_start, pio_eta_start, pio_s_start

      !> @brief Array describing the decomposition of the data.
      integer, allocatable  :: pio_fcompdof_2D_r(:)
      integer, allocatable  :: pio_fcompdof_2D_u(:)
      integer, allocatable  :: pio_fcompdof_2D_v(:)

      integer, allocatable  :: pio_fcompdof_3D_r(:)
      integer, allocatable  :: pio_fcompdof_3D_u(:)
      integer, allocatable  :: pio_fcompdof_3D_v(:)

      !! Initialize the ParallelIO library. Also allocate
      !! memory to read data from the netCDF file.
      public  :: pio_initialize

      !! This subroutine creates the decomposition.
      public  :: pio_createDecomp

      !! This subroutine reads the data array from the netCDF input file.
      public  :: pio_ncread
!      public  :: pio_ncread3

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

      contains

! ----------------------------------------------------------------------
!! Initialize the MPI and ParallelIO libraries. Also allocate
!! memory to write and read the sample data to the netCDF file.

      subroutine pio_initialize

        implicit none

        integer :: ierr,i,j,k
        integer :: wbuf, tmp_idx

        integer tile

#include "compute_tile_bounds.h"

        ! Set up PIO for this object

        pio_stride        = 1  ! 128
        pio_numAggregator = 0
        pio_optBase       = 1
        pio_type        = PIO_iotype_netcdf !pnetcdf

        pio_dimLen_2D_r(1)     = LLm+2
        pio_dimLen_2D_r(2)     = MMm+2
!        pio_dimLen_2D_u(1)     = LLm+1
!        pio_dimLen_2D_u(2)     = MMm+2
!        pio_dimLen_2D_v(1)     = LLm+2
!        pio_dimLen_2D_v(2)     = MMm+1

!        pio_dimLen_3D_r(1)     = LLm+2
!        pio_dimLen_3D_r(2)     = MMm+2
!        pio_dimLen_3D_r(3)     = N
!        pio_dimLen_3D_u(1)     = LLm+1
!        pio_dimLen_3D_u(2)     = MMm+2
!        pio_dimLen_3D_u(3)     = N
!        pio_dimLen_3D_v(1)     = LLm+2
!        pio_dimLen_3D_v(2)     = MMm+1
!        pio_dimLen_3D_v(3)     = N

        pio_niotasks = pio_ntasks ! keep things simple - 1 iotask per MPI process

        call PIO_init(pio_myRank,       ! MPI rank
     &       MPI_COMM_WORLD,             ! MPI communicator
     &       pio_niotasks,              ! Number of iotasks (ntasks/stride)
     &       pio_numAggregator,         ! number of aggregators to use
     &       pio_stride,                ! stride
     &       PIO_rearr_subset,           ! do not use any form of rearrangement (can be BOX or SUBSET)
     &       pio_IoSystem,           ! iosystem
     &       base=pio_optBase)          ! base (optional argument)

        allocate(pio_fcompdof_2D_r( xi_rho * eta_rho ))
!        allocate(pio_fcompdof_2D_u( xi_u   * eta_rho ))
!        allocate(pio_fcompdof_2D_v( xi_rho * eta_v   ))
!        allocate(pio_fcompdof_3D_r( xi_rho * eta_rho * N ))
!        allocate(pio_fcompdof_3D_u( xi_u   * eta_rho * N ))
!        allocate(pio_fcompdof_3D_v( xi_rho * eta_v   * N ))

        do j=1,eta_rho
          do i=1,xi_rho
            wbuf = (pio_eta_start - 1 + j - 1) * pio_dimLen_2D_r(1) + (pio_xi_start - 1 + i)
            tmp_idx = (j - 1) * xi_rho + i
            pio_fcompdof_2D_r(tmp_idx) = wbuf
          end do
        end do

!        do k=1,N
!          do j=1,eta_rho
!            do i=1,xi_rho
!              wbuf = (pio_s_start - 1 + k - 1) * (pio_dimLen_3D_r(1)*pio_dimLen_3D_r(2)) +
!     &               (pio_eta_start - 1 + j - 1) * pio_dimLen_2D_r(1) + (pio_xi_start - 1 + i)
!              tmp_idx = (k - 1) * ( xi_rho * eta_rho) + (j - 1) * xi_rho + i
!              pio_fcompdof_3D_r(tmp_idx) = wbuf
!            end do
!          end do
!        end do

      call pio_createDecomp()

      end subroutine pio_initialize
! ----------------------------------------------------------------------
      subroutine pio_createDecomp()

        implicit none

        call PIO_initdecomp(pio_IoSystem, PIO_double, pio_dimLen_2D_r, pio_fcompdof_2D_r,
     &       pio_desc_2D_r)
!        call PIO_initdecomp(pio_IoSystem, PIO_double, pio_dimLen_3D_r, pio_fcompdof_3D_r,
!     &       pio_desc_3D_r)

      end subroutine pio_createDecomp
! ----------------------------------------------------------------------
      subroutine pio_ncread(fname, varName, arr)

        implicit none

        character(len=*) :: fname
        character(len=*) :: varName
        real,dimension(:,:),intent(inout)   :: arr ! array to be filled

        type(var_desc_t) :: varId
        integer :: ierr

        ierr = PIO_openfile(pio_IoSystem, pio_FileDesc, pio_type, trim(fname))

        ierr = PIO_inq_varid(pio_FileDesc, trim(varName), varId)

        call PIO_read_darray(pio_FileDesc, varId, pio_desc_2D_r,
     &  arr, ierr)

      end subroutine pio_ncread

! ----------------------------------------------------------------------
!      subroutine pio_ncread3(fname, varName, arr)
!
!        implicit none
!
!        character(len=*) :: fname
!        character(len=*) :: varName
!        real,dimension(:,:),intent(inout)   :: arr ! array to be filled
!
!        type(var_desc_t) :: varId
!        integer :: ierr
!
!        ierr = PIO_openfile(pio_IoSystem, pio_FileDesc, pio_type, trim(fname))
!
!        ierr = PIO_inq_varid(pio_FileDesc, trim(varName), varId)
!
!        call PIO_read_darray(pio_FileDesc, varId, pio_desc_3D_r,
!     &  arr, ierr)
!
!      end subroutine pio_ncread3

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
