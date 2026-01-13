      module error_handling
!     Routines for checking state and handling errors

#include "cppdefs.opt"
#ifdef MPI
      use mpi_f08, only: mpi_comm_world
#endif
      use iso_fortran_env, only: error_unit
      use param, only: mynode

      implicit none
      private

      public :: handle_configuration_error
      public :: handle_netcdf_status
      public :: handle_local_error

      contains

      subroutine handle_configuration_error(context, info)
      implicit none
      integer :: ierr, msglen, total, leftpad, rightpad
      character(len=*), intent(in) :: context
      character(len=*), intent(in), optional :: info
      character(len=5120) :: final_message
#ifdef MPI
      if (mynode==0) then
#endif
      write(error_unit, '(A)') repeat('!', 34) // " ROMS ERROR " //
     &     repeat('!',34)
#ifdef MPI
      end if
#endif
      ! For context line, want to pad left and right of message so things are aligned:
      msglen  = len_trim(context)
      total   = 80
      leftpad  = (total - msglen - 2) / 2 ! integer division = floor
      rightpad = total - msglen - 2 - leftpad ! whatever remains
#ifdef MPI
      if (mynode==0) then
#endif
      write(error_unit,'(A)') repeat('!', leftpad) //
     &     ' ' // trim(context) // ' ' // repeat('!',rightpad)
      if (present(info)) write(error_unit,'(A)') trim(info)
      write(error_unit,'(A)') repeat('!', total)
#ifdef MPI
      flush(error_unit)
      end if

      call MPI_Abort(MPI_COMM_WORLD, mynode, ierr)
      call sleep(30) ! prevent any further outputs from being printed
#else
      error stop
#endif
      end subroutine handle_configuration_error

      subroutine handle_local_error(context, info, tile, i, j, k)
      implicit none
      integer, intent(in) :: i,j,tile
      integer, intent(in), optional :: k
      integer :: ierr, msglen, total, leftpad, rightpad
      character(len=*), intent(in) :: context
      character(len=*), intent(in), optional :: info
      character(len=200) :: loc_info
      character(len=5120) :: final_message
#ifdef MPI
      if (mynode==tile) then
#endif
      write(error_unit, '(A)') repeat('!', 34) // " ROMS ERROR " //
     &     repeat('!',34)
#ifdef MPI
      end if
#endif
! For context line, want to pad left and right of message so things are aligned:
      msglen  = len_trim(context)
      total   = 80
      leftpad  = (total - msglen - 2) / 2 ! integer division = floor
      rightpad = total - msglen - 2 - leftpad ! whatever remains
#ifdef MPI
      if (mynode==tile) then
#endif
         write(error_unit,'(A)') repeat('!', leftpad) //
     &        ' ' // trim(context) // ' ' // repeat('!',rightpad)
         if (present(info)) write(error_unit,'(A)') trim(info)
         write(error_unit,'(A)') repeat('!', total)
         write(loc_info, '(A,I0,A,I0,A,I0)') " Location: tile=", tile,
     &        ", i=",i,", j=",j
         if (present(k)) then
            write(loc_info,('(2A,I0)')) loc_info, ", k=",k
         end if
         write(error_unit,'(A)') loc_info
         write(error_unit,'(A)') repeat('!', total)
#ifdef MPI
         flush(error_unit)
      end if
      call MPI_Abort(MPI_COMM_WORLD, mynode, ierr)
      call sleep(30) ! prevent any further outputs from being printed
#else
      error stop
#endif
      end subroutine handle_local_error

      subroutine handle_netcdf_status(netcdf_status, context, info)

#ifdef MPI
      use mpi_f08, only: MPI_COMM_WORLD, MPI_INTEGER, MPI_SUM
#endif
      use netcdf, only: nf90_noerr, nf90_strerror
      use iso_fortran_env, only: error_unit
      use param, only: mynode

      implicit none

      integer, intent(in) :: netcdf_status
      character(len=*), intent(in) :: context
      character(len=*), intent(in), optional :: info

      integer :: local_fail, nfail, nprocs, ierr
      integer, allocatable :: fail_flags(:)

!--------------------------------------------------
!     1. Local detection (all ranks)
!--------------------------------------------------

      local_fail = merge(1, 0, netcdf_status /= nf90_noerr)

#ifdef MPI
!--------------------------------------------------
!     2. Global classification (all ranks, collective)
!--------------------------------------------------

      call MPI_Comm_size(MPI_COMM_WORLD, nprocs, ierr)

      call MPI_Allreduce(local_fail, nfail, 1, MPI_INTEGER, MPI_SUM,
     &     MPI_COMM_WORLD, ierr)

      if (nfail == 0) return    ! no error anywhere

!--------------------------------------------------
!     3. Gather failing ranks (all ranks participate)
!--------------------------------------------------

      if (mynode == 0) allocate(fail_flags(nprocs))

      call MPI_Gather(local_fail, 1, MPI_INTEGER,
     &     fail_flags, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierr)

!--------------------------------------------------
!     4. Reporting (rank 0 only)
!--------------------------------------------------

      if (mynode == 0) then
         write(error_unit, '(A)') repeat('!', 33) //
     &        " NETCDF ERROR " // repeat('!',33)

         write(error_unit,'(A)') trim(context)
         if (present(info)) write(error_unit,'(A)') trim(info)

         write(error_unit,'(A)') repeat('!', 80)

         if (nfail == nprocs) then
            write(error_unit,'(A)') 'All ranks reported a NetCDF error.'
         else
            write(error_unit,'(A)') 'NetCDF error reported on ranks:'
            do ierr = 0, nprocs-1
               if (fail_flags(ierr+1) == 1) then
                  write(error_unit,'(I0)') ierr
               end if
            end do
         end if

         write(error_unit,'(A,I0)') 'nf90 status code: ', netcdf_status
         write(error_unit,'(2A)') 'nf90 error message: ',
     &        nf90_strerror(netcdf_status)

         write(error_unit,'(A)') repeat('!', 80)
         flush(error_unit)
      end if

!--------------------------------------------------
!     5. Termination (all ranks)
!--------------------------------------------------

      call MPI_Abort(MPI_COMM_WORLD, 1, ierr)
      call sleep(30)

#else
!--------------------------------------------------
!     Serial case
!--------------------------------------------------

      if (netcdf_status /= nf90_noerr) then
         write(error_unit, '(A)') 'NETCDF ERROR'
         write(error_unit, '(A)') trim(context)
         if (present(info)) write(error_unit,'(A)') trim(info)
         write(error_unit, '(A)') nf90_strerror(netcdf_status)
         error stop
      end if
#endif

      end subroutine handle_netcdf_status

      end module error_handling
