module compile_time_switches
  implicit none
  public
  !================================================================================
  !                               BIOGEOCHEMISTRY
  !================================================================================
  ! Formerly in `bgc.opt`. Require MARBL or BIOLOGY_BEC2 cpp keys to take effect
  !================================================================================
  logical,parameter :: wrt_bgc_his = .false.     ! t/f to write tracer history file
  logical,parameter :: wrt_bgc_avg = .false.     ! t/f to write tracer averages file
  logical,parameter :: wrt_bgc_dia_his = .false. ! t/f to write diagnostic history file
  logical,parameter :: wrt_bgc_dia_avg = .false. ! t/f to write diagnostic averages file
  logical,parameter :: interp_bgc_frc = .true.   ! interpolate forcing from coarser grid

  !================================================================================
  !                                BULK FORCING
  !================================================================================
  ! Formerly in `bulk_frc.opt`
  !================================================================================
  logical,parameter :: interp_bulk_frc = .true. ! Interpolate forcing from coarser grid
  logical           :: check_bulk_frc_units = .false. ! Check units in forcing file

  !================================================================================
  !                       PRESSURE FLUX CALCULATION
  !================================================================================
  ! Formerly in `calc_pflx.opt`
  !================================================================================
  logical, parameter  :: calc_pflx     = .true.   ! Baroclinic pressure fluxes

  !================================================================================
  !                    CARBON DIOXIDE REMOVAL FORCING
  !================================================================================
  ! Formerly in `cdr_frc.opt`. Require CDR_FORCING cpp key to take effect
  !================================================================================
  logical,parameter :: cdr_source  = .false.  ! on/off switch for CDR forcing


  ! CDR forcing type
  !-----------------
  logical,public :: forcing_depth_profiles = .false. ! T if this file contains depth profiles
  logical,public :: forcing_3d             = .false. ! T if this file contains 3d forcing
  logical,public :: forcing_parameterized  = .false. ! T if forcing with idealized Gaussian profiles

  ! Linear time interpolation between records (T), or step changes (F)
  logical,parameter :: time_interpolation = .false.

  ! Prevent errors by relocating single-point CDR forcing locations on land to water
  logical,parameter :: relocate_to_wet_pts = .false.

  ! Flux type
  !----------
  ! Volume flux & tracer concentration (T) or tracer flux (F):
  logical,parameter,public :: cdr_volume  = .false.

  !================================================================================
  !                      CARBON DIOXIDE REMOVAL OUTPUT FILES
  !================================================================================
  ! Formerly in `cdr_output.opt` or `cstar_output.opt`
  ! Require CDR_OUTPUT cpp key to take effect.
  !================================================================================
  logical,parameter :: do_cdr_output   = .false. ! on/off switch for CDR-specific output
  logical,parameter :: wrt_cdr_avg   = .true. ! Write averages (T) or snapshots(F)
  ! Write averages over calendar months:
  logical,parameter :: cdr_monthly_averages = .false.

  !================================================================================
  !                               DIAGNOSTICS
  !================================================================================
  ! Formerly in `diag.opt` (stdout diags)  and `diagnostics.opt` (netcdf diags)
  ! NetCDF diagnostics require DIAGNOSTICS cppkey to take effect
  !================================================================================
  logical, parameter         :: diag_avg      = .true.  ! averages (T) or snapshots (F)
  logical, parameter, public :: diag_uv       = .false. ! Momentum diagnostics
  logical, parameter, public :: diag_trc      = .false. ! Selected tracers diagnostics

  ! Change STDOUT diagnostics stream for model testing:
  logical, parameter :: code_check_mode = .true.

  !================================================================================
  !                        NESTED BOUNDARY FILE GENERATION
  !================================================================================
  ! Formerly in `extract_data.opt`
  !================================================================================
  logical,parameter,public :: do_extract = .false. ! on/off switch for generation

  !================================================================================
  !                               FLUX FORCING
  !================================================================================
  ! Formerly in `flux_frc.opt`
  !================================================================================
  logical,parameter,public :: interp_flux_frc = .true. ! Interpolate from coarser grid

  !================================================================================
  !                           INCLUDE FORCING AS OUTPUT
  !================================================================================
  ! Formerly in `frc_output.opt`
  !================================================================================
  logical,public,parameter :: wrt_frc = .false.    ! forcing output on/off switch
  logical,parameter        :: wrt_frc_avg = .false.! averages(T) or snapshots (F)

  !================================================================================
  !                           NETCDF READ/WRITE
  !================================================================================
  ! Formerly in `nc_read_write.opt`
  !================================================================================
  logical,parameter  :: nccreate_shuffle=.true. ! Shuffle on for extra conmpression

  !================================================================================
  !                        BASIC MODEL NETCDF OUTPUT CONTROLS
  !================================================================================
  ! Formerly in `ocean_vars.opt`
  logical,parameter :: wrt_file_his = .true.   ! Write output as snapshots
  logical,parameter :: wrt_file_avg = .false.  ! Write output as averages
  logical,parameter :: wrt_file_rst = .false.  ! Write model restart files

  ! Write restarts at start of calendar month (overrides output_period_rst):
  logical,parameter :: monthly_restarts = .false.   ! This overrides output_period

  ! Control output variables (snapshots/history file):
  logical,parameter :: wrt_Z =.true.,    & ! Include `zeta`
                       wrt_Ub=.true.,    & ! Include `ubar`
                       wrt_Vb=.true.,    & ! Include `vbar`
                       wrt_U=.true.,     & ! Include `u`
                       wrt_V=.true.,     & ! Include `v`
                       wrt_R=.false.,    & ! Include `rho`
                       wrt_O=.false.,    & ! Include `omega`
                       wrt_W=.false.,    & ! Include `w`
                       wrt_Akv=.false.,  & ! Include `Akv`
                       wrt_Akt=.false.,  & ! Include `Akt`
                       wrt_Aks=.false.,  & ! Include `Aks`
                       wrt_Hbls=.false., & ! Include `hbls`
                       wrt_Hbbl=.false.    ! Include `hbbl`

  ! Control output variables (averages file):
  logical,parameter :: wrt_avg_Z =.true.,   & ! Include `zeta`
                       wrt_avg_Ub=.true.,   & ! Inlucde `ubar`
                       wrt_avg_Vb=.true.,   & ! Include `vbar`
                       wrt_avg_U=.true.,    & ! Include `u`
                       wrt_avg_V=.true.,    & ! Include `v`
                       wrt_avg_R=.true.,    & ! Include `rho`
                       wrt_avg_O=.true.,    & ! Include `omega`
                       wrt_avg_W=.true.,    & ! Include `w`
                       wrt_avg_Akv=.true. , & ! Include `Akv`
                       wrt_avg_Akt=.true.,  & ! Include `Akt`
                       wrt_avg_Aks=.true.,  & ! Include `Aks`
                       wrt_avg_Hbls=.true., & ! Include `hbls`
                       wrt_avg_Hbbl=.true.    ! Include `hbbl`

  !================================================================================
  !                           LAGRANGIAN PARTICLES
  !================================================================================
  ! Formerly in `particles.opt`
  !================================================================================
  logical,parameter :: floats = .false.   ! on/off switch for particles
  logical,parameter :: full_seed = .true. ! seed with constant density


  !================================================================================
  !                                PIPE INPUT
  !================================================================================
  ! Formerly in `pipe_frc.opt`
  !================================================================================
  logical,parameter,public :: pipe_source  = .true. ! on/off switch for pipe input
  logical,parameter        :: p_analytical = .true. ! Specify inputs analytically

  !================================================================================
  !                               CUSTOM OUTPUT
  !================================================================================
  ! Formerly in `random.opt`
  !================================================================================
  logical,public,parameter :: do_random = .false. ! on/off switch for custom output

  !================================================================================
  !                               RIVER INPUT
  !================================================================================
  ! Formerly in `river_frc.opt`
  !================================================================================
  logical,parameter,public :: river_source = .false. ! on/off switch for river input
  logical,parameter    :: river_analytical = .false. ! Specify inputs analytically

  !================================================================================
  !                              SPONGE TUNING
  !================================================================================
  !Formerly in `sponge_tune.opt`
  !================================================================================
  logical,parameter  :: ub_tune = .false. ! on/off switch for sponge tuning
  logical,parameter  :: wrt_sponge=.true. ! output sponge to file
  logical,parameter  :: spn_avg = .true.  ! write as averages (T) or snapshots (F)

  !================================================================================
  !                             SURFACE FLUX OUTPUT
  !================================================================================
  ! Formerly in `surf_flx.opt`
  !================================================================================
  logical,parameter :: wrt_smflx = .false. ! output surface momentum flux
  logical,parameter :: wrt_stflx = .false. ! output surface tracer flux
  logical,parameter :: wrt_swflx = .false. ! output surface water flux (P-E)
  logical,parameter :: sflx_avg  = .false. ! write averaged sflx data

  !================================================================================
  !                               TIDAL FORCING
  !================================================================================
  ! Formerly in `tides.opt`
  !================================================================================
  logical,parameter :: bry_tides=.false. ! Barotropic (TPXO) tides at the boundary
  logical,parameter :: pot_tides=.false. ! Surface tidal potential
  logical,parameter :: ana_tides=.false. ! Specify tides analytically
  !================================================================================
  !                               TRACER OUTPUT
  !================================================================================
  ! Formerly in `tracers.opt`
  !================================================================================
  logical,parameter :: wrt_temp = .true.     ! Write temperature
  logical,parameter :: wrt_salt = .true.     ! Write salinity
  logical,parameter :: wrt_temp_dia = .true. ! Write temperature diagnostics
  logical,parameter :: wrt_salt_dia = .true. ! Write salinity diagnostics

  !================================================================================
  !                                CDR UPSCALING
  !================================================================================
  ! Formerly in `upscale_output.opt`
  !================================================================================
  logical,parameter :: do_upscale = .false. ! on/off switch for upscaling

  !================================================================================
  !                          OUTPUT ON Z LEVELS
  !================================================================================
  ! Formerly in `zslice_output.opt`
  !================================================================================
  logical,parameter :: do_zslice  = .false. ! on/off switch for sigma-z conversion
  logical,parameter :: zslice_avg = .false. ! output averages (T) or snapshots (T)
  logical,parameter :: wrt_T_zsl  = .false., &  ! Include tracers
                       wrt_U_zsl  = .true.,  &  ! Include u-velocity
                       wrt_V_zsl  = .true.      ! Include v-velocity


  !================================================================================
  !                           ERROR HANDLING
  !================================================================================
  ! GATHER_ERRORS_ON_MAIN_RANK ensures MPI synchronization before abort.
  ! This can add runtime bottlenecks due to the use of MPI collectives,
  ! But guarantees all ranks are able to report, and groups identical errors
  ! spanning multiple ranks for more human-readable output:
  logical,parameter :: gather_errors_on_main_rank = .false.



end module compile_time_switches

