      module instant_output
      ! Collection of random variables for output

#include "cppdefs.opt"

      use nc_read_write, only: ncwrite
      use netcdf, only:
     &     nf90_noerr, nf90_write, nf90_open, nf90_def_dim,
     &     nf90_def_var, nf90_put_att, nf90_close, nf90_double
      use roms_read_write, only: create_file
      implicit none

      private

      character(len=13) :: module_name = "instant_output"

      interface wrt_instant
        module procedure  wrt_instant_2D, wrt_instant_3D
      end interface

      ! Public functions
      public wrt_instant


      contains
!---------------------------------------------------------
      subroutine def_vars_instant(ncid, varname, ndim, dimsize)  ![
      implicit none

      ! input
      integer,intent(in) :: ncid
      character(len=*),intent(in) :: varname
      integer,intent(in) :: ndim
      integer,intent(in),dimension(:) :: dimsize

      ! local
      integer :: i, ierr, varid, did, xtype
      character(len=1) :: istr
      integer,allocatable,dimension(:) :: dimid

      integer,parameter :: default_prec = nf90_double

      xtype = default_prec
      allocate(dimid(ndim))

!      varid = nccreate(ncid,'zeta',(/dn_xr,dn_yr,dn_tm/),(/xi_rho,eta_rho,0/))

      do i = 1,ndim                                        ! get dimension ids. Create if needed.
        write(istr, '(I0)') i
        ierr=nf90_def_dim(ncid,'dim' // istr ,dimsize(i),did)
        dimid(i) = did
      enddo

      ierr=nf90_def_var(ncid,varname,xtype,dimid,varid,
     &                  deflate_level=1, shuffle=.true.)

      end subroutine def_vars_instant  !]
!----------------------------------------------------------------------
      subroutine wrt_instant_2D(var, varname, kill) ![
      ! Call wrt_random after completion of the time-step
      ! (After step3d_uv2)
      implicit none

      ! input
      real,intent(in),dimension(:,:) :: var
      character(len=*),intent(in)    :: varname
      logical, optional :: kill

      character(len=10) :: sr_name = "wrt_instant"
      ! local
      integer,dimension(2)   :: varsize
      character(len=99),save :: fname
      integer                :: ncid,ierr

      varsize = shape(var)

      call create_file('_' // varname,fname)
      ierr=nf90_open(fname,nf90_write,ncid)
      call def_vars_instant(ncid,varname,2,varsize)

      ! always add time
      call ncwrite(ncid,varname ,var(:,:),(/1,1/))

      ierr=nf90_close (ncid)

      if (present(kill)) then
        call MPI_Finalize (ierr)
        stop
      endif

      end subroutine wrt_instant_2D !]
!----------------------------------------------------------------------
      subroutine wrt_instant_3D(var, varname, kill) ![
      implicit none

      ! input
      real,intent(in),dimension(:,:,:) :: var
      character(len=*),intent(in)    :: varname
      logical, optional :: kill

      character(len=10) :: sr_name = "wrt_instant"
      ! local
      integer,dimension(3)   :: varsize
      character(len=99),save :: fname
      integer                :: ncid,ierr

      varsize = shape(var)

      call create_file('_' // varname,fname)
      ierr=nf90_open(fname,nf90_write,ncid)
      call def_vars_instant(ncid,varname,3,varsize)

      ! always add time
      call ncwrite(ncid,varname ,var(:,:,:),(/1,1,1/))

      ierr=nf90_close (ncid)

      if (present(kill)) then
        call MPI_Finalize (ierr)
        stop
      endif

      end subroutine wrt_instant_3D !]
!----------------------------------------------------------------------

      end module instant_output
