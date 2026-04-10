module compile_time_switches
#include "cppdefs.opt"
  implicit none
  public
! bgc.opt
#if defined(MARBL) || defined(BIOLOGY_BEC2)
  logical,parameter :: wrt_bgc_his = .false. ! t/f to write module history file
  logical,parameter :: wrt_bgc_avg = .false. ! t/f to write module averages file

#if defined(BEC2_DIAG) || defined(MARBL_DIAGS)
  logical,parameter :: wrt_bgc_dia_his = .false. ! t/f to write module history file
  logical,parameter :: wrt_bgc_dia_avg = .false.         ! t/f to write module history file
#endif
  ! prev wrt_his, wrt_avg, wrt_his_dia, wrt_avg_dia
#endif

end module compile_time_switches

