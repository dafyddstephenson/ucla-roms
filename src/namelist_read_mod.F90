module namelist_read_mod
#include "cppdefs.opt"
  !-----------------------------------------------------------------------
  !     MODULE: namelist_read_mod
  !
  !     DESCRIPTION:
  !     This module centralizes namelist read calls from various modules
  !     so they can be called in line with model initialization
  !
  !     AUTHOR: Dafydd Stephenson
  !     DATE: 2026-04-13
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
#if defined(MARBL) || defined(BIOLOGY_BEC2)
    use bgc_shared_vars, only: read_bgc_nml
#endif
#if defined MARBL && defined CDR_FORCING
    use cdr_frc, only: read_cdr_frc_nml
#endif
#if defined MARBL && defined MARBL_DIAGS && defined CDR_FORCING
    use cdr_output, only: read_cdr_output_nml
#endif
#ifdef DIAGNOSTICS
    use diagnostics, only: read_diagnostics_nml
#endif
    use basic_output, only: read_basic_output_nml
    use calc_pflx_mod, only: read_pflx_nml
    use extract_data, only: read_extract_nml
    use frc_output, only: read_frc_output_nml
    use particles, only: read_particles_nml
    use pipe_frc, only: read_pipe_nml
    use random_output, only: read_random_nml
    use river_frc, only: read_river_nml
    use sponge_tune, only: read_sponge_tune_nml
    use surf_flux, only: read_surf_flx_nml
    use tides, only: read_tides_nml
#if defined MARBL && defined MARBL_DIAGS && defined UPSCALING
    use upscale_output, only: read_upscale_nml
#endif
    use zslice_output, only: read_zslice_nml

    implicit none

#ifndef ANA_GRID
    call read_grid_nml
#endif
#if defined(MARBL) || defined(BIOLOGY_BEC2)
    call read_bgc_nml
#endif
#if defined MARBL && defined CDR_FORCING
    call read_cdr_frc_nml
#endif
#ifdef DIAGNOSTICS
    call read_diagnostics_nml
#endif
    call read_basic_output_nml
    call read_pflx_nml
#if defined MARBL && defined MARBL_DIAGS && defined CDR_FORCING
    call read_cdr_output_nml
#endif
    call read_extract_nml
    call read_frc_output_nml
    call read_particles_nml
    call read_pipe_nml
    call read_random_nml
    call read_river_nml
    call read_sponge_tune_nml
    call read_surf_flx_nml
    call read_tides_nml
#if defined MARBL && defined MARBL_DIAGS && defined UPSCALING
    call read_upscale_nml
#endif
    call read_zslice_nml

  end subroutine read_namelists

  end module namelist_read_mod
