module error_handling_mod
#include "cppdefs.opt"

  use timers, only: stop_timers
  use param, only: mynode, nnodes, ocean_grid_comm
  use utils_mod, only: replace_string
#ifdef MPI
  use mpi_f08, only: MPI_Gather, MPI_Gatherv, MPI_INTEGER, MPI_CHARACTER, MPI_MAX, MPI_Allreduce, MPI_Barrier, MPI_Abort
#endif

  implicit none
  private
  save

  !==============================
  ! Public symbols
  !==============================
  public :: error_log_type, error_log
  public :: LOG_LEVEL_ERROR, LOG_LEVEL_WARNING

  !==============================
  ! Levels
  !==============================
  integer, parameter :: LOG_LEVEL_ERROR   = 1
  integer, parameter :: LOG_LEVEL_WARNING = 2   ! TODO

  !==============================
  ! Scopes (internal)
  !==============================
  integer, parameter :: SCOPE_GLOBAL = 0
  integer, parameter :: SCOPE_RANK   = 1
  integer, parameter :: SCOPE_POINT  = 2

  !--------------------------------------------------------------------------------
  ! DERIVED TYPES
  !--------------------------------------------------------------------------------

  !==============================
  !    Log entry
  !==============================
  type :: error_log_entry_type
    integer :: level
    integer :: scope
    integer :: rank = -1
    integer :: i = -1, j = -1, k = -1
    character(len=:), allocatable :: context
    character(len=:), allocatable :: info
    type(error_log_entry_type), pointer :: next => null()
#ifdef MPI
  contains
    procedure, private :: serialize => serialize_log_entry
#endif
  end type error_log_entry_type

  !==============================
  !    Log entry group
  !==============================
  ! Used to group errors with same content, different rank/point
  type :: error_log_entry_group_id_type
     integer :: rank = -1
     integer :: i = -1, j = -1, k = -1
     integer :: count = 0
  end type error_log_entry_group_id_type

  type :: error_log_entry_group_type
     integer :: level
     integer :: scope
     character(len=:), allocatable :: context
     character(len=:), allocatable :: info
     type(error_log_entry_group_id_type), allocatable :: id(:)
     type(error_log_entry_group_type), pointer :: next => null()
  end type error_log_entry_group_type

  !==============================
  !    Error log object
  !==============================
  type :: error_log_type
    logical :: abort_requested = .false.
    type(error_log_entry_type), pointer :: head => null()
    type(error_log_entry_type), pointer :: tail => null()
  contains
    procedure :: raise_global
    procedure :: raise_from_rank
    procedure :: raise_from_point
    procedure :: check_netcdf_status
    procedure :: abort_check
    procedure, private :: raise_internal
    procedure, private :: append_entry => append_entry_to_log
#ifdef MPI
    procedure, private :: serialize => serialize_log
#endif
  end type error_log_type

  !==============================
  !    Global error log instance
  !==============================
  type(error_log_type) :: error_log

contains
  !=========================================================
  !    Public API
  !=========================================================

  subroutine raise_global(this, context, info, level)
    class(error_log_type), intent(inout) :: this
    character(len=*), intent(in)         :: context, info
    integer, intent(in), optional        :: level
    integer                              :: used_level

    if (present(level)) then
       used_level = level
    else
       used_level = LOG_LEVEL_ERROR
    end if

    call this%raise_internal( &
      scope   = SCOPE_GLOBAL, &
      level   = used_level, &
      context = context, &
      info = info )
  end subroutine raise_global


  subroutine raise_from_rank(this, context, info, level)
    use param, only: mynode
    class(error_log_type), intent(inout) :: this
    character(len=*), intent(in)         :: context, info
    integer, intent(in), optional        :: level
    integer                              :: used_level

    if (present(level)) then
       used_level = level
    else
       used_level = LOG_LEVEL_ERROR
    end if

    call this%raise_internal( &
      scope   = SCOPE_RANK, &
      level   = used_level, &
      context = context, &
      info = info, &
      rank    = mynode )
  end subroutine raise_from_rank


  subroutine raise_from_point(this, i, j, k, context, info, level)
    use param, only: mynode
    class(error_log_type), intent(inout) :: this
    integer, intent(in)                  :: i, j
    integer, intent(in), optional        :: k
    character(len=*), intent(in)         :: context, info
    integer, intent(in), optional        :: level
    integer                              :: used_level, used_k

    if (present(k)) then
       used_k = k
    else
       used_k = -1
    end if

    if (present(level)) then
       used_level = level
    else
       used_level = LOG_LEVEL_ERROR
    end if

    call this%raise_internal( &
      scope   = SCOPE_POINT, &
      level   = used_level, &
      context = context, &
      info = info, &
      rank    = mynode, &
      i       = i, &
      j       = j, &
      k       = used_k)
  end subroutine raise_from_point

  subroutine check_netcdf_status(this, netcdf_status, context, info)
    use param, only: mynode
    use netcdf, only: nf90_noerr, nf90_strerror
    class(error_log_type), intent(inout) :: this
    integer, intent(in) :: netcdf_status
    character(len=*), intent(in) :: context
    character(len=*), intent(in), optional :: info

    character(len=:), allocatable :: final_info
    character(len=32) :: status_str

    if (netcdf_status == nf90_noerr) return

    write(status_str,'(I0)') netcdf_status

    final_info = ''
    if (present(info)) final_info = trim(info)//new_line('A')

    final_info = final_info // &
         'NF90 STATUS CODE: ' // trim(status_str) // new_line('A') // &
         'NF90 ERROR MESSAGE: ' // trim(nf90_strerror(netcdf_status))
    call this%raise_from_rank(context, final_info, level=LOG_LEVEL_ERROR)
  end subroutine check_netcdf_status

  subroutine abort_check(this)
    use param, only: ocean_grid_comm, mynode
    class(error_log_type), intent(inout) :: this
    type(error_log_entry_group_type), pointer :: groups

#ifdef MPI
    type(error_log_type) :: global_log
    integer :: local_abort, global_abort
    character(len=:), allocatable :: local_serialized_log, global_serialized_log

    local_abort = merge(1, 0, this%abort_requested)
    call MPI_Allreduce(local_abort, global_abort, 1, MPI_INTEGER, MPI_MAX, ocean_grid_comm)

    if (global_abort == 0) return

    call this%serialize(local_serialized_log)
    call gather_serialized_error_logs_on_primary_rank(local_serialized_log, global_serialized_log)

    if (mynode == 0) then
       call deserialize_log(global_serialized_log, global_log)
       call group_error_log_entries(global_log, groups)
       call print_error_log_entry_groups(groups)
    end if

    call stop_timers()
    call MPI_Barrier(ocean_grid_comm)
    call MPI_Abort(ocean_grid_comm, 1)
    call sleep(30) ! stop further output leaking through
#else
    if (this%abort_requested) then
       call group_error_log_entries(this, groups)
       call print_error_log_entry_groups(groups)
       call stop_timers()
       error stop
    end if
#endif
  end subroutine abort_check

  !=========================================================
  !   Internal procedures
  !=========================================================

  subroutine raise_internal(this, scope, level, context, info, rank, i, j, k)
    class(error_log_type), intent(inout) :: this
    integer, intent(in)                  :: scope, level
    character(len=*), intent(in)         :: context, info
    integer, intent(in), optional        :: rank, i, j, k

    type(error_log_entry_type), pointer :: error_entry

    if (level == LOG_LEVEL_ERROR) this%abort_requested = .true.

    allocate(error_entry)
    error_entry%scope   = scope
    error_entry%level   = level
    error_entry%context = trim(context)
    error_entry%info = trim(info)

    if (present(rank)) error_entry%rank = rank
    if (present(i))    error_entry%i = i
    if (present(j))    error_entry%j = j
    if (present(k) .and. k>-1) then
       error_entry%k = k
    end if

    if (associated(this%tail)) then
      this%tail%next => error_entry
    else
      this%head => error_entry
    end if
    this%tail => error_entry

  end subroutine raise_internal

  subroutine append_entry_to_log(this, e)
    class(error_log_type), intent(inout) :: this
    type(error_log_entry_type), intent(in) :: e
    type(error_log_entry_type), pointer :: p

    allocate(p)
    p = e
    p%next => null()

    if (associated(this%tail)) then
       this%tail%next => p
    else
       this%head => p
    end if
    this%tail => p
  end subroutine append_entry_to_log


  !--------------------------------------------------------------------------------
  ! GROUPING ENTRIES AND PRINTING
  !--------------------------------------------------------------------------------

  subroutine group_error_log_entries(log, groups)
    type(error_log_type), intent(in) :: log
    type(error_log_entry_group_type), pointer :: groups

    type(error_log_entry_type), pointer :: e
    type(error_log_entry_group_type), pointer :: g
    type(error_log_entry_group_id_type), allocatable :: tmp(:)
    integer :: n, p
    logical :: found_group, found_id

    groups => null()
    e => log%head

    do while (associated(e))

       !#########################################
       ! Find existing group (same error content)
       !#########################################
       g => groups
       found_group = .false.
       do while (associated(g))
          if ( e%level   == g%level   .and. &
               e%scope   == g%scope   .and. &
               e%context == g%context .and. &
               e%info    == g%info ) then
             found_group = .true.
             exit
          end if
          g => g%next
       end do

       !#########################################
       ! Create new group if needed
       !#########################################
       if (.not. found_group) then
          allocate(g)
          g%level   = e%level
          g%scope   = e%scope
          g%context = e%context
          g%info    = e%info
          allocate(g%id(0))
          g%next => groups
          groups => g
       end if

       !#########################################
       ! Find existing id inside the group
       !#########################################
       found_id = .false.
       do p = 1, size(g%id)
          if ( g%id(p)%rank == e%rank .and. &
               g%id(p)%i    == e%i    .and. &
               g%id(p)%j    == e%j    .and. &
               g%id(p)%k    == e%k ) then
             g%id(p)%count = g%id(p)%count + 1
             found_id = .true.
             exit
          end if
       end do

       !#########################################
       ! Append new ID if needed
       !#########################################
       if (.not. found_id) then
          n = size(g%id)
          allocate(tmp(n+1))
          if (n > 0) tmp(1:n) = g%id

          tmp(n+1)%rank  = e%rank
          tmp(n+1)%i     = e%i
          tmp(n+1)%j     = e%j
          tmp(n+1)%k     = e%k
          tmp(n+1)%count = 1

          call move_alloc(tmp, g%id)
       end if

       e => e%next
    end do
  end subroutine group_error_log_entries


  subroutine print_error_log_entry_groups(groups)
    use iso_fortran_env, only: error_unit
    type(error_log_entry_group_type), pointer :: groups
    type(error_log_entry_group_type), pointer :: g
    integer :: n_locs, i, total_raises

    g => groups
    do while (associated(g))

       n_locs = size(g%id)
       total_raises = sum(g%id(:)%count)

       !#########################################
       ! Header
       !#########################################

       select case (g%scope)
       case (SCOPE_GLOBAL)
          write(error_unit,*) &
               'ERROR [global] [', trim(g%context), ']:'
       case (SCOPE_RANK)
          write(error_unit,'(A,I0,A,I0,A,A,A)') &
               'ERROR [raised ',total_raises,' time(s) across ', n_locs, ' rank(s)] [', trim(g%context), ']:'
       case (SCOPE_POINT)
          write(error_unit,'(A,I0,A,I0,A,A,A)') &
               'ERROR [raised ',total_raises,' time(s) across ', n_locs, ' point(s)] [', trim(g%context), ']:'
       end select

       !#########################################
       ! Message body
       !#########################################

       write(error_unit,*) trim(g%info)

       !#########################################
       ! IDs
       !#########################################

       select case (g%scope)

       case (SCOPE_RANK)
          write(error_unit,'(A)', advance='no') 'RANKS: '
          do i = 1, n_locs
             write(error_unit,'(I0)', advance='no') g%id(i)%rank
             if (i < n_locs) write(error_unit,'(A)', advance='no') ', '
          end do
          write(error_unit,*)

       case (SCOPE_POINT)
          do i = 1, n_locs
             write(error_unit,*) &
                  '  rank ', g%id(i)%rank, &
                  ' (i,j,k)=(', g%id(i)%i, ',', &
                  g%id(i)%j, ',', &
                  g%id(i)%k, ')'
          end do

       end select

       write(error_unit,*)  ! blank line between groups
       g => g%next
    end do
  end subroutine print_error_log_entry_groups

!--------------------------------------------------------------------------------
! MPI-SPECIFIC SUBROUTINES
!--------------------------------------------------------------------------------
#ifdef MPI

  subroutine gather_serialized_error_logs_on_primary_rank( &
       local_serialized_log, global_serialized_log)
    character(len=*), intent(in) :: local_serialized_log
    character(len=:), allocatable, intent(out) :: global_serialized_log

    integer ::  i
    integer :: local_len, total_len
    integer, allocatable :: recvcounts(:), displs(:)

    local_len = len(local_serialized_log)

    allocate(recvcounts(nnodes), displs(nnodes))

    call MPI_Gather(local_len, 1, MPI_INTEGER, &
         recvcounts, 1, MPI_INTEGER, &
         0, ocean_grid_comm)

    if (mynode == 0) then
       displs(1) = 0
       do i = 2, nnodes
          displs(i) = displs(i-1) + recvcounts(i-1)
       end do
       total_len = sum(recvcounts)
       allocate(character(len=total_len) :: global_serialized_log)
    end if

    call MPI_Gatherv(local_serialized_log, local_len, MPI_CHARACTER, &
         global_serialized_log, recvcounts, displs, MPI_CHARACTER, &
         0, ocean_grid_comm)

    deallocate(recvcounts, displs)
  end subroutine gather_serialized_error_logs_on_primary_rank

  subroutine serialize_log(this, buffer)
    class(error_log_type), intent(in) :: this
    character(len=:), allocatable, intent(out) :: buffer

    type(error_log_entry_type), pointer :: e
    character(len=:), allocatable :: line

    buffer = ''
    e => this%head

    do while (associated(e))
       call e%serialize(line)
       buffer = buffer // line // new_line('A')
       e => e%next
    end do
  end subroutine serialize_log

  subroutine deserialize_log(serialized_log, deserialized_log)
    character(len=*), intent(in) :: serialized_log
    type(error_log_type), intent(out) :: deserialized_log

    integer :: pos, next
    type(error_log_entry_type) :: deserialized_log_entry
    character(len=:), allocatable :: serialized_log_entry

    pos = 1
    do
       next = index(serialized_log(pos:), new_line('A'))
       if (next == 0) exit

       serialized_log_entry = serialized_log(pos:pos+next-2)
       pos = pos + next

       call deserialize_log_entry(serialized_log_entry,deserialized_log_entry)

       deserialized_log_entry%next => null()
       call deserialized_log%append_entry(deserialized_log_entry)
    end do
  end subroutine deserialize_log

  subroutine serialize_log_entry(this, s)
    class(error_log_entry_type), intent(in) :: this
    character(len=:), allocatable, intent(out) :: s
    character(len=:), allocatable :: safe_info
    character(len=1024) :: tmp

    safe_info = this%info
    safe_info = replace_string(safe_info, new_line('A'), '\n')

    write(tmp, '(A,I0,A,I0,A,I0,A,I0,A,I0,A,I0,A,A,A,A)') &
         'L=',  this%level, &
         '|S=', this%scope, &
         '|R=', this%rank, &
         '|I=', this%i, &
         '|J=', this%j, &
         '|K=', this%k, &
         '|C=', trim(this%context), &
         '|M=', trim(safe_info)

    s = trim(tmp)
  end subroutine serialize_log_entry

  subroutine deserialize_log_entry(serialized_entry, deserialized_entry)!level, scope, rank, i, j, k, context, info)
    character(len=*), intent(in) :: serialized_entry
    ! integer, intent(out) :: level, scope, rank, i, j, k
    ! character(len=:), allocatable, intent(out) :: context, info
    character(len=:), allocatable :: entry_info
    type(error_log_entry_type), intent(out) :: deserialized_entry

    call extract_int(serialized_entry, 'L=', deserialized_entry%level)
    call extract_int(serialized_entry, 'S=', deserialized_entry%scope)
    call extract_int(serialized_entry, 'R=', deserialized_entry%rank)
    call extract_int(serialized_entry, 'I=', deserialized_entry%i)
    call extract_int(serialized_entry, 'J=', deserialized_entry%j)
    call extract_int(serialized_entry, 'K=', deserialized_entry%k)
    call extract_str(serialized_entry, 'C=', deserialized_entry%context)
    call extract_str(serialized_entry, 'M=', entry_info)
    entry_info = replace_string(entry_info, '\n', new_line('A'))

    deserialized_entry%info = entry_info

  contains

  subroutine extract_int(line, key, val)
    character(len=*), intent(in) :: line, key
    integer, intent(out) :: val
    integer :: p1, p2

    p1 = index(line, key) + len(key)
    p2 = index(line(p1:), '|')
    if (p2 == 0) then
       read(line(p1:), *) val
    else
       read(line(p1:p1+p2-2), *) val
    end if
  end subroutine extract_int

  subroutine extract_str(line, key, val)
    character(len=*), intent(in) :: line, key
    character(len=:), allocatable, intent(out) :: val
    integer :: p1, p2

    p1 = index(line, key) + len(key)
    p2 = index(line(p1:), '|')
    if (p2 == 0) then
       val = line(p1:)
    else
       val = line(p1:p1+p2-2)
    end if
  end subroutine extract_str

  end subroutine deserialize_log_entry

#endif /* MPI */

end module error_handling_mod
