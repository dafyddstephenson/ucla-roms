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

      end module error_handling
