      module precheck

      ! A place to conduct runtime checking of inputs and settings

#include "cppdefs.opt"
      use compile_time_switches, only: do_extract, wrt_file_rst
      use error_handling_mod, only: error_log
      use netcdf, only: nf90_double
      use bgc_shared_vars, only: bgc_precheck
      use extract_data, only:  extract_data_precheck
      use basic_output, only:  output_period_rst

      implicit none
      private

      character(len=8) :: module_name="precheck"

      public do_precheck

      contains
!---------------------------------------------------
      subroutine do_precheck

      implicit none

      call extract_data_precheck
      call bgc_precheck

      end subroutine do_precheck
!---------------------------------------------------
      end module precheck
