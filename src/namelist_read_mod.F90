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
    use dimensions, only: read_nml_grid
#endif
#if defined(MARBL) || defined(BIOLOGY_BEC2)
    use bgc_shared_vars, only: read_nml_bgc
#endif
#if defined MARBL && defined CDR_FORCING
    use cdr_frc, only: read_nml_cdr_frc
#endif
#if defined MARBL && defined MARBL_DIAGS && defined CDR_FORCING
    use cdr_output, only: read_cdr_output_nml
#endif
#ifdef DIAGNOSTICS
    use diagnostics, only: read_nml_diagnostics
#endif
    use basic_output, only: read_nml_basic_output
    use calc_pflx_mod, only: read_nml_pflx
    use extract_data, only: read_nml_extract
    use frc_output, only: read_nml_frc_output
    use particles, only: read_nml_particles
    use pipe_frc, only: read_nml_pipe
    use random_output, only: read_nml_random
    use river_frc, only: read_nml_river
    use sponge_tune, only: read_nml_sponge_tune
    use surf_flux, only: read_nml_surf_flx
    use tides, only: read_nml_tides
#if defined MARBL && defined MARBL_DIAGS && defined UPSCALING
    use upscale_output, only: read_nml_upscale
#endif
    use zslice_output, only: read_nml_zslice
#if defined(SOLVE3D) && !defined(NONLIN_EOS)
    use eos_vars, only: read_nml_lin_rho_eos
#endif

    implicit none

#ifndef ANA_GRID
    call read_nml_grid
#endif
#if defined(MARBL) || defined(BIOLOGY_BEC2)
    call read_nml_bgc
#endif
#if defined MARBL && defined CDR_FORCING
    call read_nml_cdr_frc
#endif
#ifdef DIAGNOSTICS
    call read_nml_diagnostics
#endif
    call read_nml_basic_output
    call read_nml_pflx
#if defined MARBL && defined MARBL_DIAGS && defined CDR_FORCING
    call read_cdr_output_nml
#endif
    call read_nml_extract
    call read_nml_frc_output
    call read_nml_particles
    call read_nml_pipe
    call read_nml_random
    call read_nml_river
    call read_nml_sponge_tune
    call read_nml_surf_flx
    call read_nml_tides
#if defined MARBL && defined MARBL_DIAGS && defined UPSCALING
    call read_nml_upscale
#endif
    call read_nml_zslice
#if defined(SOLVE3D) && !defined(NONLIN_EOS)
    call read_nml_lin_rho_eos
#endif

  end subroutine read_namelists

  end module namelist_read_mod
