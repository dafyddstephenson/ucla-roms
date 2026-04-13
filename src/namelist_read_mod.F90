module namelist_read_mod
#include "cppdefs.opt"
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

  public :: read_namelists

  character(len=18) :: module_name = "namelist_read_mod"
contains

  subroutine read_namelists
#ifndef ANA_GRID
    use dimensions, only: read_grid_nml
#endif

    implicit none

#ifndef ANA_GRID
    call read_grid_nml
#endif

  end subroutine read_namelists

  end module namelist_read_mod
