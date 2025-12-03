      module keyword_registry

!-----------------------------------------------------------------------
!     MODULE: keyword_registry
!
!     DESCRIPTION:
!     Provides a registry of keywords used in a runtime settings file
!
!     Includes types and procedures to register keywords (e.g. 'title')
!     and keyword handlers (subroutines used to extract associated
!     parameters from the runtime settings file) and look those keywords
!     up
!
!     SEE ALSO: read_inp_mod.F
!
!     AUTHOR: Dafydd Stephenson
!     DATE: 2025-12-03
!-----------------------------------------------------------------------

      use param

      implicit none

      integer, parameter :: MAX_NUM_KEYWORDS = 200 ! hardcoded upper bound

      type :: keyword_entry
!-----------------------------------------------------------------------
!     TYPE: keyword_entry
!     DESCRIPTION:
!     Represents a single legal keyword in the input file along with
!     its bookkeeping info and the procedure bound to handle it.
!
!     FIELDS:
!       name      - keyword string as it appears in the input
!       expected  - expected number of occurrences in settings file (0,1)
!       found     - occurrences actually encountered during parse
!       handler   - procedure pointer to the routine that processes
!                   this keyword (implements keyword_handler)
!-----------------------------------------------------------------------
         character(len=:), allocatable :: name
         integer :: expected = 1 ! expected occurrences (0 or 1)
         integer :: found    = 0 ! occurrences seen in file
         procedure(keyword_handler), pointer, nopass :: handler => null()
      end type keyword_entry

      abstract interface
!-----------------------------------------------------------------------
!     ABSTRACT INTERFACE: keyword_handler
!     DESCRIPTION:
!     Prototype for all keyword handler routines. Each handler is
!     responsible for reading/processing the keyword’s data from the
!     specified unit and reporting any errors through ierr.
!
!     ARGUMENTS:
!       unit  - input unit positioned at the keyword’s data
!       ierr  - incremental error flag
!-----------------------------------------------------------------------
         subroutine keyword_handler(unit, ierr)
            integer, intent(in) :: unit
            integer, intent(inout) :: ierr
         end subroutine keyword_handler
      end interface

      type(keyword_entry), allocatable :: registry(:)
      integer :: kw_idx = 0

      contains

      subroutine init_registry()
!-----------------------------------------------------------------------
!     SUBROUTINE: init_registry
!     DESCRIPTION:
!     Allocate the keyword registry wtih a fixed number of entries
!-----------------------------------------------------------------------
         if (.not.allocated(registry)) then
            allocate(registry(MAX_NUM_KEYWORDS))
         end if
         kw_idx = 0
      end subroutine init_registry

      subroutine register_keyword(name, expected, handler)
!-----------------------------------------------------------------------
!     SUBROUTINE: register_keyword
!     DESCRIPTION:
!     Add a keyword to the registry along with its expected number of
!     occurences and a pointer to its handler routine
!-----------------------------------------------------------------------
         character(len=*), intent(in) :: name
         integer, intent(in), optional :: expected
         procedure(keyword_handler), optional :: handler

         if (kw_idx >= MAX_NUM_KEYWORDS) then
            if (mynode==0) write(*,*)
     &           "ERROR: MAX_NUM_KEYWORDS exceeded in register_keyword"
            error stop
         end if
         kw_idx = kw_idx + 1

         registry(kw_idx)%name = trim(name)
         registry(kw_idx)%found = 0

         if (present(expected)) registry(kw_idx)%expected = expected
         if (present(handler)) registry(kw_idx)%handler => handler

      end subroutine register_keyword

      function lookup_keyword(name) result(idx)
!-----------------------------------------------------------------------
!     INTEGER FUNCTION: lookup_keyword
!     DESCRIPTION:
!     Given a keyword name, look up that keyword in the registry and
!     provide its index
!-----------------------------------------------------------------------
         character(len=*), intent(in) :: name
         integer :: idx, i
         idx = 0
         do i = 1, kw_idx
            if (trim(registry(i)%name) == trim(name)) then
               idx = i
               return
            end if
         end do
      end function lookup_keyword

      subroutine report_keyword_mismatch(ierr)
!-----------------------------------------------------------------------
!     SUBROUTINE: report_keyword_mismatch
!     DESCRIPTION:
!     Increments the error counter and prints a message if a keyword has
!     not been marked as found in the registry an equal number of times
!     as `expected`
!-----------------------------------------------------------------------
         integer, intent(inout) :: ierr
         integer :: i

         do i = 1, kw_idx
            if (registry(i)%found /= registry(i)%expected) then
               if (mynode==0) write(*,'(A," expected=",I2," found=",I2)')
     &              "Keyword "//trim(registry(i)%name)//" mismatch: ",
     &              registry(i)%expected, registry(i)%found
               ierr = ierr + 1
            end if
         end do
      end subroutine report_keyword_mismatch

      end module keyword_registry
