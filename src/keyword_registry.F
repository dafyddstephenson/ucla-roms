      module keyword_registry

      use param

      implicit none

      integer, parameter :: MAX_KW = 200 ! hardcoded upper bound

      type :: keyword_entry
      character(len=:), allocatable :: name
      integer :: expected = 1   ! expected occurrences (0 or 1)
      integer :: found    = 0   ! occurrences seen in file
      end type keyword_entry

      type(keyword_entry), allocatable :: registry(:)
      integer :: n_keywords = 0

      contains

      subroutine init_registry()
      if (.not.allocated(registry)) then
         allocate(registry(MAX_KW))
      end if
      n_keywords = 0
      end subroutine init_registry

      subroutine register_keyword(name, expected)
      character(len=*), intent(in) :: name
      integer, intent(in), optional :: expected

      if (n_keywords >= MAX_KW) then
         if (mynode==0) write(*,*)
     &        "ERROR: MAX_KW exceeded in register_keyword"
         stop
      end if

      n_keywords = n_keywords + 1
      registry(n_keywords)%name = trim(name)
      if (present(expected)) registry(n_keywords)%expected = expected
      registry(n_keywords)%found = 0
      end subroutine register_keyword

      function lookup_keyword(name) result(idx)
      character(len=*), intent(in) :: name
      integer :: idx, i
      idx = 0
      do i = 1, n_keywords
         if (trim(registry(i)%name) == trim(name)) then
            idx = i
            return
         end if
      end do
      end function lookup_keyword

      subroutine report_missing_or_extra(ierr)
      integer, intent(inout) :: ierr
      integer :: i

      do i = 1, n_keywords
         if (registry(i)%found /= registry(i)%expected) then
            if (mynode==0) write(*,'(A," expected=",I2," found=",I2)')
     &           "Keyword "//trim(registry(i)%name)//" mismatch: ",
     &           registry(i)%expected, registry(i)%found
            ierr = ierr + 1
         end if
      end do
      end subroutine report_missing_or_extra

      end module keyword_registry
