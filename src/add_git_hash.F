      subroutine add_git_hash(ncid)

      use roms_read_write, only: git_hash
      use nc_read_write, only: handle_ierr
      use netcdf, only: nf90_global, nf90_noerr, nf90_put_att

      implicit none
      integer :: ncid,ierr

      ierr=nf90_put_att(ncid, nf90_global, 'git_version',git_hash)
      if (ierr/=nf90_noerr)
     &    call handle_ierr(ierr,'add_git hash: ')


      end subroutine add_git_hash

