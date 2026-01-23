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
  !    Public API (error_log_type)
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
    class(error_log_type), intent(inout) :: this
    type(error_log_entry_group_type), allocatable :: grouped_error_log_entries(:)

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
       call group_error_log_entries(global_log, grouped_error_log_entries)
       call print_error_log_entry_groups(grouped_error_log_entries)
    end if

    call stop_timers()
    call MPI_Barrier(ocean_grid_comm)
    call MPI_Abort(ocean_grid_comm, 1)
    call sleep(30) ! stop further output leaking through
#else
    if (this%abort_requested) then
       call group_error_log_entries(this, grouped_error_log_entries)
       call print_error_log_entry_groups(grouped_error_log_entries)
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

  subroutine append_entry_to_log(this, entry_to_append)
    class(error_log_type), intent(inout) :: this
    type(error_log_entry_type), intent(in) :: entry_to_append
    type(error_log_entry_type), pointer :: appended_entry

    ! Allocate new log entry
    allocate(appended_entry)
    ! Copy contents into newly allocated entry
    appended_entry = entry_to_append
    ! Clear any previous associations copied in from entry_to_append
    appended_entry%next => null()

    ! Redefine indexing of the updated log
    if (associated(this%tail)) then
       this%tail%next => appended_entry
    else
       this%head => appended_entry
    end if
    this%tail => appended_entry

  end subroutine append_entry_to_log


  !--------------------------------------------------------------------------------
  ! GROUPING ENTRIES AND PRINTING
  !--------------------------------------------------------------------------------

  subroutine group_error_log_entries(log, grouped_log_entries)
    type(error_log_type), intent(in) :: log
    type(error_log_entry_group_type), allocatable, intent(out) :: grouped_log_entries(:)
    type(error_log_entry_type), pointer :: entry_to_group
    integer :: i, found_group_idx, new_idx, n_groups
    logical :: group_found, id_found
    type(error_log_entry_group_id_type), allocatable :: tmp_id(:)
    type(error_log_entry_group_type), allocatable :: tmp_grouped_log_entries(:)

    ! Start with no groups
    allocate(grouped_log_entries(0))
    n_groups = 0

    entry_to_group => log%head

    do while (associated(entry_to_group))

       !#########################################
       ! Find matching group
       !#########################################
       group_found=.false.
       do i = 1, n_groups
          if ( entry_to_group%level   == grouped_log_entries(i)%level   .and. &
               entry_to_group%scope   == grouped_log_entries(i)%scope   .and. &
               entry_to_group%context == grouped_log_entries(i)%context .and. &
               entry_to_group%info    == grouped_log_entries(i)%info ) then
             group_found = .true.
             found_group_idx = i
             exit
          end if
       end do

       !#########################################
       ! Create new group if needed
       !#########################################
       if (.not. group_found) then
          allocate(tmp_grouped_log_entries(n_groups+1))
          if (n_groups > 0) then
             tmp_grouped_log_entries(1:n_groups) = grouped_log_entries
          end if

          new_idx = n_groups + 1
          tmp_grouped_log_entries(new_idx)%level   = entry_to_group%level
          tmp_grouped_log_entries(new_idx)%scope   = entry_to_group%scope
          tmp_grouped_log_entries(new_idx)%context = entry_to_group%context
          tmp_grouped_log_entries(new_idx)%info    = entry_to_group%info

          allocate(tmp_grouped_log_entries(new_idx)%id(0))

          call move_alloc(tmp_grouped_log_entries, grouped_log_entries)
          found_group_idx = new_idx
          n_groups = n_groups+1
       end if

       associate (found_group => grouped_log_entries(found_group_idx))
       !#########################################
       ! Find matching ID within group
       !#########################################
         id_found=.false.
         do i = 1, size(found_group%id)
            if ( found_group%id(i)%rank == entry_to_group%rank .and. &
                 found_group%id(i)%i    == entry_to_group%i    .and. &
                 found_group%id(i)%j    == entry_to_group%j    .and. &
                 found_group%id(i)%k    == entry_to_group%k ) then

               id_found=.true.
               found_group%id(i)%count = found_group%id(i)%count + 1
               exit

            end if
         end do

         !#########################################
         ! Append new ID if needed
         !#########################################

         if (.not. id_found) then
            allocate(tmp_id(size(found_group%id) + 1))

            if (size(found_group%id) > 0) tmp_id(1:size(found_group%id)) = found_group%id

            new_idx = size(tmp_id)
            tmp_id(new_idx)%rank  = entry_to_group%rank
            tmp_id(new_idx)%i     = entry_to_group%i
            tmp_id(new_idx)%j     = entry_to_group%j
            tmp_id(new_idx)%k     = entry_to_group%k
            tmp_id(new_idx)%count = 1

            call move_alloc(tmp_id, found_group%id)

         end if
       end associate
       entry_to_group => entry_to_group%next

    end do

  end subroutine group_error_log_entries

  subroutine print_error_log_entry_groups(groups)
    use iso_fortran_env, only: error_unit
    type(error_log_entry_group_type), intent(in) :: groups(:)

    integer :: g, i, n_locs, total_raises

    do g = 1, size(groups)

       n_locs = size(groups(g)%id)
       total_raises = sum(groups(g)%id(:)%count)

       select case (groups(g)%scope)
       case (SCOPE_GLOBAL)
          write(error_unit,*) &
               'ERROR [global] [', trim(groups(g)%context), ']:'
       case (SCOPE_RANK)
          write(error_unit,'(A,I0,A,I0,A,A,A)') &
               'ERROR [raised ',total_raises,' time(s) across ', n_locs, ' rank(s)] [', &
               trim(groups(g)%context), ']:'
       case (SCOPE_POINT)
          write(error_unit,'(A,I0,A,I0,A,A,A)') &
               'ERROR [raised ',total_raises,' time(s) across ', n_locs, ' point(s)] [', &
               trim(groups(g)%context), ']:'
       end select

       write(error_unit,*) trim(groups(g)%info)

       select case (groups(g)%scope)
       case (SCOPE_RANK)
          write(error_unit,'(A)', advance='no') 'RANKS: '
          do i = 1, n_locs
             write(error_unit,'(I0)', advance='no') groups(g)%id(i)%rank
             if (i < n_locs) write(error_unit,'(A)', advance='no') ', '
          end do
          write(error_unit,*)

       case (SCOPE_POINT)
          do i = 1, n_locs
             write(error_unit,*) &
                  '  rank ', groups(g)%id(i)%rank, &
                  ' (i,j,k)=(', groups(g)%id(i)%i, ',', &
                  groups(g)%id(i)%j, ',', &
                  groups(g)%id(i)%k, ')'
          end do
       end select

       write(error_unit,*)
    end do
  end subroutine print_error_log_entry_groups

#ifdef MPI
!--------------------------------------------------------------------------------
! MPI-SPECIFIC SUBROUTINES
!--------------------------------------------------------------------------------

  subroutine gather_serialized_error_logs_on_primary_rank( &
       local_serialized_log, global_serialized_log)
    character(len=*), intent(in) :: local_serialized_log
    character(len=:), allocatable, intent(out) :: global_serialized_log

    integer ::  i
    integer :: local_len, total_len
    integer, allocatable :: gathered_local_lens(:), rank_start_positions(:)

    !#########################################
    ! Create a global serialized log on rank 0
    !#########################################
    ! Collect each rank's local log length on
    ! rank 0, then determine the total length
    ! of the overall log string, and the
    ! starting position/index of each rank's
    ! contribution

    ! Length of serialized log string on this rank
    local_len = len(local_serialized_log)

    ! Allocate vars to track how many characters each rank will send
    ! to 0 and where rank 'i's contribution starts in the overall log:
    allocate(gathered_local_lens(nnodes))
    allocate(rank_start_positions(nnodes))

    ! Gather array of all local lengths on rank 0
    call MPI_Gather( &
         sendbuf  = local_len, &
         sendcount= 1, &
         sendtype = MPI_INTEGER, &
         recvbuf  = gathered_local_lens, &
         recvcount= 1, &
         recvtype = MPI_INTEGER, &
         root     = 0, &
         comm     = ocean_grid_comm )

    ! Now on primary rank, determine where to insert each rank's
    ! contribution to the global log:
    if (mynode == 0) then
       rank_start_positions(1) = 0
       do i = 2, nnodes
          rank_start_positions(i) = rank_start_positions(i-1) + gathered_local_lens(i-1)
       end do

       ! ... and allocate the global log:
       total_len = sum(gathered_local_lens)
       allocate(character(len=total_len) :: global_serialized_log)
    end if

    !#########################################
    ! Populate the global serialized log
    !#########################################
    !
    ! Now we have an allocated fixed-length string for the global log, we can populate it
    ! from each local log using a variable-size gather with our calculated indices
    call MPI_Gatherv( &
         sendbuf  = local_serialized_log, &
         sendcount= local_len, &
         sendtype = MPI_CHARACTER, &
         recvbuf  = global_serialized_log, &
         recvcounts = gathered_local_lens, &
         displs  = rank_start_positions, &
         recvtype = MPI_CHARACTER, &
         root     = 0, &
         comm     = ocean_grid_comm )

    deallocate(gathered_local_lens, rank_start_positions)

  end subroutine gather_serialized_error_logs_on_primary_rank

  subroutine serialize_log(this, serialized_log)
    class(error_log_type), intent(in) :: this
    character(len=:), allocatable, intent(out) :: serialized_log

    type(error_log_entry_type), pointer :: entry_to_serialize
    character(len=:), allocatable :: serialized_entry

    serialized_log = ''
    entry_to_serialize => this%head

    do while (associated(entry_to_serialize))
       call entry_to_serialize%serialize(serialized_entry)
       serialized_log = serialized_log // serialized_entry // new_line('A')
       entry_to_serialize => entry_to_serialize%next
    end do
  end subroutine serialize_log

  subroutine deserialize_log(serialized_log, deserialized_log)
    character(len=*), intent(in) :: serialized_log
    type(error_log_type), intent(out) :: deserialized_log

    integer :: pos, entry_len
    type(error_log_entry_type) :: deserialized_log_entry
    character(len=:), allocatable :: serialized_log_entry

    pos = 1
    do
       ! Find index of end of entry starting at current position:
       entry_len = index(serialized_log(pos:), new_line('A'))
       if (entry_len == 0) exit

       ! Extract entry and deserialize
       serialized_log_entry = serialized_log(pos:pos+entry_len-2)
       pos = pos + entry_len

       call deserialize_log_entry(serialized_log_entry,deserialized_log_entry)

       deserialized_log_entry%next => null()

       ! Append deserialized entry to output log
       call deserialized_log%append_entry(deserialized_log_entry)
    end do
  end subroutine deserialize_log

  subroutine serialize_log_entry(this, serialized_log_entry)
    class(error_log_entry_type), intent(in) :: this
    character(len=:), allocatable, intent(out) :: serialized_log_entry
    character(len=:), allocatable :: info_no_newlines
    ! Fixed-length temporary to hold the serialized log entry
    character(len=1024) :: tmp

    info_no_newlines = this%info
    info_no_newlines = replace_string(info_no_newlines, new_line('A'), '\n')

    write(tmp, '(A,I0,A,I0,A,I0,A,I0,A,I0,A,I0,A,A,A,A)') &
         'L=',  this%level, &
         '|S=', this%scope, &
         '|R=', this%rank, &
         '|I=', this%i, &
         '|J=', this%j, &
         '|K=', this%k, &
         '|C=', trim(this%context), &
         '|M=', trim(info_no_newlines)

    serialized_log_entry = trim(tmp)

  end subroutine serialize_log_entry

  subroutine deserialize_log_entry(serialized_entry, deserialized_entry)
    character(len=*), intent(in) :: serialized_entry
    character(len=:), allocatable :: entry_info
    type(error_log_entry_type), intent(out) :: deserialized_entry

    ! Extract integer fields (level,scope,rank,i,j,k) from serialzied entry:
    call extract_int(serialized_entry, 'L=', deserialized_entry%level)
    call extract_int(serialized_entry, 'S=', deserialized_entry%scope)
    call extract_int(serialized_entry, 'R=', deserialized_entry%rank)
    call extract_int(serialized_entry, 'I=', deserialized_entry%i)
    call extract_int(serialized_entry, 'J=', deserialized_entry%j)
    call extract_int(serialized_entry, 'K=', deserialized_entry%k)
    ! Extract character fields (context, info) from serialized_entry:
    call extract_str(serialized_entry, 'C=', deserialized_entry%context)
    call extract_str(serialized_entry, 'M=', entry_info)
    ! Replace placeholder character with true newline in 'info'
    entry_info = replace_string(entry_info, '\n', new_line('A'))
    deserialized_entry%info = entry_info

  contains

  subroutine extract_int(serialized_entry, key, val)
    character(len=*), intent(in) :: serialized_entry, key
    integer, intent(out) :: val
    integer :: p1, p2

    ! Start position and end position of integer to extract:
    p1 = index(serialized_entry, key) + len(key)
    p2 = index(serialized_entry(p1:), '|')

    ! If int is final entry in line, read to end of line
    if (p2 == 0) then
       read(serialized_entry(p1:), *) val
    else
       read(serialized_entry(p1:p1+p2-2), *) val
    end if
  end subroutine extract_int

  subroutine extract_str(line, key, val)
    character(len=*), intent(in) :: line, key
    character(len=:), allocatable, intent(out) :: val
    integer :: p1, p2
    ! Start position and end position of char to extract:
    p1 = index(line, key) + len(key)
    p2 = index(line(p1:), '|')
    ! If char is final entry in line, read to end of line
    if (p2 == 0) then
       val = line(p1:)
    else
       val = line(p1:p1+p2-2)
    end if
  end subroutine extract_str

  end subroutine deserialize_log_entry

#endif /* MPI */

end module error_handling_mod
