module compile_time_switches
  implicit none
  public
! bgc.opt (requires MARBL or BIOLOGY_BEC2, and MARBL_DIAGS or BEC2_DIAG for diagnostics)
  logical,parameter :: wrt_bgc_his = .true. ! t/f to write module history file
  logical,parameter :: wrt_bgc_avg = .true. ! t/f to write module averages file
  logical,parameter :: wrt_bgc_dia_his = .true. ! t/f to write module history file
  logical,parameter :: wrt_bgc_dia_avg = .true.         ! t/f to write module history file
  ! prev wrt_his, wrt_avg, wrt_his_dia, wrt_avg_dia

  !calc_pflx.opt
  logical, parameter  :: calc_pflx     = .true.   ! Baroclinic pressure fluxes

  !cdr.opt (requires CDR_FORCING)
  logical,parameter :: cdr_source  = .false.  ! Should be false if not using CDR forcing


  ! SET ONLY ONE OF THE FOLLOWING FORCING TYPES TO BE TRUE ***********************************
  logical,public :: forcing_depth_profiles = .false.     ! T if this file contains depth profiles
  logical,public :: forcing_3d             = .false.     ! T if this file contains 3d forcing
  logical,public :: forcing_parameterized  = .false.      ! T if forcing with idealized Gaussian profiles

  ! INTERPOLATE FORCING IN TIME, OR NO?
  logical :: time_interpolation = .false.   ! T if using time interpolation. If T, forcing is linearly
  ! interpolated in time between consecutive records; otherwise,
  ! forcing is held constant until the next record is reached

  ! RELOCATE CDR FORCING TO NEAREST WET POINT?
  logical :: relocate_to_wet_pts = .false.  ! If false, any depth profile or single-point parameterized
  ! forcing that is centered on land will cause the model to
  ! error out.  If true, the forcing will be relocated or
  ! recentered to the nearest wet point.

  ! PARAMETERIZED VERTICAL PROFILES ********************************
  logical,parameter,public :: cdr_volume  = .false.  ! Set to .false. if you want a tracer flux but no added water.
  ! If .true., read in volume flux and tracer concentration, and
  ! multiply together to get tracer flux.
  ! Set to false if you want to read in a vertical profile(s).

  ! cdr_output.opt
  logical,parameter :: wrt_cdr   = .false.
  logical,parameter :: wrt_cdr_avg   = .false.  ! NOTE: For most applications .true. is recommended
  logical,parameter :: cdr_monthly_averages = .false. ! This overrides output_period

  !diag.opt
  logical, parameter :: code_check_mode =.true.

  !diagnostics.opt
  logical, parameter         :: diag_avg      = .true.    ! compute history (=F) or averages (=T)
  logical, parameter, public :: diag_uv       = .false.   ! Momentum diagnostics
  logical, parameter, public :: diag_trc      = .false.   ! Selected tracers diagnostics

  !extract_data.opt
  logical,parameter,public :: do_extract = .false.

  !frc_output.opt
  logical,public,parameter :: wrt_frc = .false.
  logical,parameter        :: wrt_frc_avg   = .false.

  !nc_read_write.opt
  logical,parameter  :: nccreate_shuffle=.true.  ! Shuffle on for extra conmpression

  !ocean_vars.opt
  logical,parameter :: wrt_file_rst      = .false.     ! t/f to write module history file
  logical,parameter :: monthly_restarts = .false.      ! This overrides output_period
  logical,parameter :: wrt_file_his      = .true.     ! t/f to write module history file
  logical,parameter :: wrt_file_avg      = .false.     ! t/f to write module averages file

  logical,parameter :: wrt_Z =.true., &
                       wrt_Ub=.true., &
                       wrt_Vb=.true., &
                       wrt_U=.true., &
                       wrt_V=.true., &
                       wrt_R=.false., &
                       wrt_O=.false., &
                       wrt_W=.false., &
                       wrt_Akv=.false., &
                       wrt_Akt=.false., &
                       wrt_Aks=.false., &
                       wrt_Hbls=.false., &
                       wrt_Hbbl=.false.

  logical,parameter :: wrt_avg_Z =.true., &
                       wrt_avg_Ub=.true., &
                       wrt_avg_Vb=.true., &
                       wrt_avg_U=.true., &
                       wrt_avg_V=.true., &
                       wrt_avg_R=.true., &
                       wrt_avg_O=.true., &
                       wrt_avg_W=.true., &
                       wrt_avg_Akv=.true., &
                       wrt_avg_Akt=.true., &
                       wrt_avg_Aks=.true., &
                       wrt_avg_Hbls=.true., &
                       wrt_avg_Hbbl=.true.

  ! particles.opt
  logical,parameter :: floats = .false.
  logical :: full_seed = .true.           ! seed with constant density

  !pipe_frc.opt
  logical,parameter,public :: pipe_source  = .false.      ! use pipe forcing
  logical,parameter        :: p_analytical = .false.      ! analytical forcing true

  !random_output.opt
  logical,public,parameter :: do_random = .false.

end module compile_time_switches

