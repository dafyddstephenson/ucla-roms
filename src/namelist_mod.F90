module namelist_mod
  !-----------------------------------------------------------------------
  !     MODULE: namelist_mod
  !
  !     DESCRIPTION:
  !     This module handles generic namelist-related management, such
  !     as opening the namelist file for reading in other modules.
  !
  !     AUTHOR: Dafydd Stephenson
  !     DATE: 2026-04-10
  !-----------------------------------------------------------------------

  implicit none
  private

  public :: open_namelist_file

  character(len=13) :: module_name = "namelist_mod"
contains

  subroutine open_namelist_file(namelist_unit)
    !-----------------------------------------------------------------------
    !     SUBROUTINE: open_namelist_file
    !     DESCRIPTION:
    !     Opens the namelist file for reading using a new unit,
    !     returning the unit to the caller so they can close the file after
    !     read
    !-----------------------------------------------------------------------
    use error_handling_mod, only: error_log

    implicit none

    integer, intent(out)  :: namelist_unit
    character(len=12) :: nml_file = "namelist.nml"
    integer :: ios
    character(len=1024) :: error_info
    character(len=19) :: sr_name ="open_namelist_file"

    ! Open the namelist file
    open (newunit=namelist_unit, file=nml_file, status="old", &
         action="read", iostat=ios)

    ! Raise error in case of failure
    if (ios /= 0) then
       write (error_info,*) "could not open namelist file ", trim(nml_file)
       call error_log%raise_global( &
            context=module_name//"/"//sr_name, &
            info=error_info)
    end if
  end subroutine open_namelist_file



  end module namelist_mod
