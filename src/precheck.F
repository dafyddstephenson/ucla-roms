      module precheck

      ! A place to conduct runtime checking of inputs and settings

#include "cppdefs.opt"

!      use ocean_vars, only: wrt_file_rst, output_period_rst
!      use extract_data, only: do_extract, extract_period, extract_nrpf
      use error_handling_mod, only: error_log
      use netcdf, only: nf90_double

      implicit none
      private

#include "ocean_vars.opt"
#include "extract_data.opt"

      character(len=8) :: module_name="precheck"

      public do_precheck

      contains
!---------------------------------------------------
      subroutine do_precheck

      implicit none

      call extract_data_precheck

      end subroutine do_precheck
!---------------------------------------------------
      subroutine extract_data_precheck

      implicit none

      character(len=21) :: sr_name = "extract_data_precheck"
      character(len=1024) :: error_info
      real :: extract_newfile_freq

      if (wrt_file_rst .and. do_extract) then !1
      extract_newfile_freq = extract_nrpf * extract_period

      if (mod(output_period_rst,extract_newfile_freq) /= 0) then
       write(error_info,*) "Extract data frequency = ", extract_newfile_freq,
     & ". Restart freuency = ", output_period_rst, ". The frequency of ",
     & "writing the extract_data file must evenly divide the restart ",
     & "frequency (this prevents writing partial extract_data files)."
        call error_log%raise_from_rank(
     &   context=module_name//"/"//sr_name,
     &   info=error_info)
        call error_log%abort_check()
      endif

      endif !1

      end subroutine extract_data_precheck
!---------------------------------------------------
      end module precheck
