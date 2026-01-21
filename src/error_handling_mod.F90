module error_handling_mod
  use mpi_f08, only: MPI_Gather, MPI_Gatherv, MPI_comm, MPI_INTEGER, MPI_CHARACTER, MPI_MAX, MPI_Allreduce, MPI_Barrier, MPI_Abort
  use timers, only: stop_timers
  use param, only: mynode, nnodes
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

  !==============================
  ! Log entry
  !==============================
  type :: error_entry_type
    integer :: level
    integer :: scope
    integer :: rank = -1
    integer :: i = -1, j = -1, k = -1
    character(len=:), allocatable :: context
    character(len=:), allocatable :: info
    type(error_entry_type), pointer :: next => null()
  end type error_entry_type

  !==============================
  ! Log entry group
  !==============================
  ! Used to group errors with same content, different rank/point
  type :: provenance_type
     integer :: rank = -1
     integer :: i = -1, j = -1, k = -1
     integer :: count = 0
  end type provenance_type

  type :: error_group_type
     integer :: level
     integer :: scope
     character(len=:), allocatable :: context
     character(len=:), allocatable :: info
     type(provenance_type), allocatable :: prov(:)
     type(error_group_type), pointer :: next => null()
  end type error_group_type

  !==============================
  ! Error log object
  !==============================
  type :: error_log_type
    logical :: abort_requested = .false.
    type(error_entry_type), pointer :: head => null()
    type(error_entry_type), pointer :: tail => null()
  contains
    procedure :: raise_global
    procedure :: raise_from_rank
    procedure :: raise_from_point
    procedure :: check_netcdf_status
    procedure :: abort_check
    procedure, private :: raise_internal
    procedure, private :: append_entry
  end type error_log_type

  !==============================
  ! ROMS error log
  !==============================
  type(error_log_type) :: error_log

contains
  function replace_string(str, old, new) result(out)
    character(len=*), intent(in) :: str
    character(len=*), intent(in) :: old
    character(len=*), intent(in) :: new
    character(len=:), allocatable :: out

    integer :: pos, start
    out = ''
    start = 1

    do
       pos = index(str(start:), old)
       if (pos == 0) exit

       out = out // str(start:start+pos-2) // new
       start = start + pos - 1 + len(old)
    end do

    out = out // str(start:)
  end function replace_string
  !=========================================================
  ! Public API
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

  !=========================================================
  ! Internal implementation
  !=========================================================

  subroutine raise_internal(this, scope, level, context, info, rank, i, j, k)
    class(error_log_type), intent(inout) :: this
    integer, intent(in)                  :: scope, level
    character(len=*), intent(in)         :: context, info
    integer, intent(in), optional        :: rank, i, j, k

    type(error_entry_type), pointer :: error_entry

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

  subroutine append_entry(this, e)
    class(error_log_type), intent(inout) :: this
    type(error_entry_type), intent(in) :: e
    type(error_entry_type), pointer :: p

    allocate(p)
    p = e
    p%next => null()

    if (associated(this%tail)) then
       this%tail%next => p
    else
       this%head => p
    end if
    this%tail => p
  end subroutine append_entry
  !=========================================================
  ! Collective abort handling
  !=========================================================

  subroutine serialize_entry(e, s)
    type(error_entry_type), intent(in) :: e
    character(len=:), allocatable, intent(out) :: s
    character(len=:), allocatable :: safe_info
    character(len=1024) :: tmp

    safe_info = e%info
    safe_info = replace_string(safe_info, new_line('A'), '\n')

    write(tmp, '(A,I0,A,I0,A,I0,A,I0,A,I0,A,I0,A,A,A,A)') &
         'L=', e%level, &
         '|S=', e%scope, &
         '|R=', e%rank, &
         '|I=', e%i, &
         '|J=', e%j, &
         '|K=', e%k, &
         '|C=', trim(e%context), &
         '|M=', trim(safe_info)

    s = trim(tmp)
  end subroutine serialize_entry

  subroutine serialize_local_log(this, buffer)
    class(error_log_type), intent(in) :: this
    character(len=:), allocatable, intent(out) :: buffer

    type(error_entry_type), pointer :: e
    character(len=:), allocatable :: line

    buffer = ''
    e => this%head

    do while (associated(e))
       call serialize_entry(e, line)
       buffer = buffer // line // new_line('A')
       e => e%next
    end do
  end subroutine serialize_local_log

  subroutine gather_all_error_buffers(local_buf, global_buf, comm)
    character(len=*), intent(in) :: local_buf
    character(len=:), allocatable, intent(out) :: global_buf
    type(MPI_Comm), intent(in) :: comm

    integer ::  i
    integer :: local_len, total_len
    integer, allocatable :: recvcounts(:), displs(:)

    local_len = len(local_buf)

    allocate(recvcounts(nnodes), displs(nnodes))

    call MPI_Gather(local_len, 1, MPI_INTEGER, &
         recvcounts, 1, MPI_INTEGER, &
         0, comm)

    if (mynode == 0) then
       displs(1) = 0
       do i = 2, nnodes
          displs(i) = displs(i-1) + recvcounts(i-1)
       end do
       total_len = sum(recvcounts)
       allocate(character(len=total_len) :: global_buf)
    end if

    call MPI_Gatherv(local_buf, local_len, MPI_CHARACTER, &
         global_buf, recvcounts, displs, MPI_CHARACTER, &
         0, comm)

    deallocate(recvcounts, displs)
  end subroutine gather_all_error_buffers

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

  subroutine parse_line(line, level, scope, rank, i, j, k, context, info)
    character(len=*), intent(in) :: line
    integer, intent(out) :: level, scope, rank, i, j, k
    character(len=:), allocatable, intent(out) :: context, info

    call extract_int(line, 'L=', level)
    call extract_int(line, 'S=', scope)
    call extract_int(line, 'R=', rank)
    call extract_int(line, 'I=', i)
    call extract_int(line, 'J=', j)
    call extract_int(line, 'K=', k)
    call extract_str(line, 'C=', context)
    call extract_str(line, 'M=', info)
    info = replace_string(info, '\n', new_line('A'))
  end subroutine parse_line

  logical function same_group(level, scope, context, info, g)
    integer, intent(in) :: level, scope
    character(len=*), intent(in) :: context, info
    type(error_group_type), intent(in) :: g

    same_group = &
         level == g%level .and. &
         scope == g%scope .and. &
         context == g%context .and. &
         info == g%info
  end function same_group

  subroutine deserialize_log(buffer, log)
    character(len=*), intent(in) :: buffer
    type(error_log_type), intent(out) :: log

    integer :: pos, next
    type(error_entry_type) :: e
    character(len=:), allocatable :: line

    pos = 1
    do
       next = index(buffer(pos:), new_line('A'))
       if (next == 0) exit

       line = buffer(pos:pos+next-2)
       pos = pos + next

       call parse_line( &
            line, &
            e%level, e%scope, e%rank, e%i, e%j, e%k, &
            e%context, e%info )

       e%next => null()
       call log%append_entry(e)
    end do
  end subroutine deserialize_log

  subroutine group_errors(log, groups)
    type(error_log_type), intent(in) :: log
    type(error_group_type), pointer :: groups

    type(error_entry_type), pointer :: e
    type(error_group_type), pointer :: g
    type(provenance_type), allocatable :: tmp(:)
    integer :: n, p
    logical :: found_group, found_prov

    groups => null()
    e => log%head

    do while (associated(e))

       !==================================================
       ! Find existing group (same error content)
       !==================================================
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

       !==================================================
       ! Create new group if needed
       !==================================================
       if (.not. found_group) then
          allocate(g)
          g%level   = e%level
          g%scope   = e%scope
          g%context = e%context
          g%info    = e%info
          allocate(g%prov(0))
          g%next => groups
          groups => g
       end if

       !==================================================
       ! Find existing provenance inside the group
       !==================================================
       found_prov = .false.
       do p = 1, size(g%prov)
          if ( g%prov(p)%rank == e%rank .and. &
               g%prov(p)%i    == e%i    .and. &
               g%prov(p)%j    == e%j    .and. &
               g%prov(p)%k    == e%k ) then
             g%prov(p)%count = g%prov(p)%count + 1
             found_prov = .true.
             exit
          end if
       end do

       !==================================================
       ! Append new provenance if needed
       !==================================================
       if (.not. found_prov) then
          n = size(g%prov)
          allocate(tmp(n+1))
          if (n > 0) tmp(1:n) = g%prov

          tmp(n+1)%rank  = e%rank
          tmp(n+1)%i     = e%i
          tmp(n+1)%j     = e%j
          tmp(n+1)%k     = e%k
          tmp(n+1)%count = 1

          call move_alloc(tmp, g%prov)
       end if

       e => e%next
    end do
  end subroutine group_errors

  subroutine print_groups(groups)
    use iso_fortran_env, only: error_unit
    type(error_group_type), pointer :: groups
    type(error_group_type), pointer :: g
    integer :: n_locs, i, total_raises

    g => groups
    do while (associated(g))

       n_locs = size(g%prov)
       total_raises = sum(g%prov(:)%count)

       !==================================================
       ! Header
       !==================================================

       select case (g%scope)
       case (SCOPE_GLOBAL)
          write(error_unit,*) &
               'ERROR [global] [', trim(g%context), ']:'
       case (SCOPE_RANK)
          write(error_unit,'(A,I0,A,I0,A,A,A)') &
               'ERROR [raised ',total_raises,' times across ', n_locs, ' ranks] [', trim(g%context), ']:'
       case (SCOPE_POINT)
          write(error_unit,'(A,I0,A,I0,A,A,A)') &
               'ERROR [raised ',total_raises,' times across ', n_locs, ' points] [', trim(g%context), ']:'
       end select

       !==================================================
       ! Message body
       !==================================================

       write(error_unit,*) trim(g%info)

       !==================================================
       ! Provenance
       !==================================================

       select case (g%scope)

       case (SCOPE_RANK)
          write(error_unit,'(A)', advance='no') 'RANKS: '
          do i = 1, n_locs
             write(error_unit,'(I0)', advance='no') g%prov(i)%rank
             if (i < n_locs) write(error_unit,'(A)', advance='no') ', '
          end do
          write(error_unit,*)

       case (SCOPE_POINT)
          do i = 1, n_locs
             write(error_unit,*) &
                  '  rank ', g%prov(i)%rank, &
                  ' (i,j,k)=(', g%prov(i)%i, ',', &
                  g%prov(i)%j, ',', &
                  g%prov(i)%k, ')'
          end do

       end select

       write(error_unit,*)  ! blank line between groups
       g => g%next
    end do
  end subroutine print_groups

  subroutine abort_check(this)
    use param, only: ocean_grid_comm, mynode
    class(error_log_type), intent(inout) :: this

    type(error_log_type) :: global_log
    type(error_group_type), pointer :: groups
    integer :: local_abort, global_abort
    character(len=:), allocatable :: local_buf, global_buf

    local_abort = merge(1, 0, this%abort_requested)
    call MPI_Allreduce(local_abort, global_abort, 1, MPI_INTEGER, MPI_MAX, ocean_grid_comm)

    if (global_abort == 0) return

    call serialize_local_log(this, local_buf)
    call gather_all_error_buffers(local_buf, global_buf, ocean_grid_comm)

    if (mynode == 0) then
       call deserialize_log(global_buf, global_log)
       call group_errors(global_log, groups)
       call print_groups(groups)
    end if

    call stop_timers()
    call MPI_Barrier(ocean_grid_comm)
    call MPI_Abort(ocean_grid_comm, 1)
    call sleep(30) ! stop further output leaking through
  end subroutine abort_check

end module error_handling_mod
