      module add_git_hash_mod

      implicit none
      private

      public :: add_git_hash
      character(len=41), public  :: git_hash= "xxx"

      contains

      subroutine add_git_hash(ncid)

      use netcdf, only: nf90_global, nf90_noerr, nf90_put_att
      use error_handling_mod, only: error_log

      implicit none
      integer :: ncid,ierr

      ierr=nf90_put_att(ncid, nf90_global, 'git_version',git_hash)
      call error_log%check_netcdf_status(netcdf_status=ierr,
     &     info="unable to add git hash attribute to netcdf file",
     &     context="add_git_hash")
      end subroutine add_git_hash


      end module add_git_hash_mod
