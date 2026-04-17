module compile_time_switches
  implicit none
  public
  !================================================================================
  !                                BULK FORCING
  !================================================================================
  ! Formerly in `bulk_frc.opt`
  !================================================================================
  logical,parameter :: interp_bulk_frc = .false. ! Interpolate forcing from coarser grid
  logical           :: check_bulk_frc_units = .false. ! Check units in forcing file


  !================================================================================
  !                               FLUX FORCING
  !================================================================================
  ! Formerly in `flux_frc.opt`
  !================================================================================
  logical,parameter,public :: interp_flux_frc = .false. ! Interpolate from coarser grid

  !================================================================================
  !                           NETCDF READ/WRITE
  !================================================================================
  ! Formerly in `nc_read_write.opt`
  !================================================================================
  logical,parameter  :: nccreate_shuffle=.true. ! Shuffle on for extra conmpression


  !================================================================================
  !                           LAGRANGIAN PARTICLES
  !================================================================================
  ! Formerly in `particles.opt`
  !================================================================================
  logical,parameter :: floats = .false.   ! on/off switch for particles
  logical,parameter :: full_seed = .true. ! seed with constant density

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
  !                           ERROR HANDLING
  !================================================================================
  ! GATHER_ERRORS_ON_MAIN_RANK ensures MPI synchronization before abort.
  ! This can add runtime bottlenecks due to the use of MPI collectives,
  ! But guarantees all ranks are able to report, and groups identical errors
  ! spanning multiple ranks for more human-readable output:
  logical,parameter :: gather_errors_on_main_rank = .false.



end module compile_time_switches

