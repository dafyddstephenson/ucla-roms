      module keyword_registry

      use param

      implicit none

      integer, parameter :: MAX_NUM_KEYWORDS = 200 ! hardcoded upper bound

      type :: keyword_entry
         character(len=:), allocatable :: name
         integer :: expected = 1 ! expected occurrences (0 or 1)
         integer :: found    = 0 ! occurrences seen in file
         procedure(keyword_handler), pointer, nopass :: handler => null()
      end type keyword_entry

      abstract interface
         subroutine keyword_handler(unit, ierr)
            integer, intent(in) :: unit
            integer, intent(inout) :: ierr
         end subroutine keyword_handler
      end interface

      type(keyword_entry), allocatable :: registry(:)
      integer :: kw_idx = 0

      contains

      subroutine init_registry()
         if (.not.allocated(registry)) then
            allocate(registry(MAX_NUM_KEYWORDS))
         end if
         kw_idx = 0
      end subroutine init_registry

      subroutine register_keyword(name, expected, handler)
         character(len=*), intent(in) :: name
         integer, intent(in), optional :: expected
         procedure(keyword_handler), pointer, optional :: handler

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
