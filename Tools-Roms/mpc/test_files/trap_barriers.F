!        1         2         3         4         5         6
!23456789012345678901234567890123456789012345678901234567890
      PROGRAM TEST_TRAP_BARRIERS
      IMPLICIT NONE
      INTEGER I

      PRINT *, 'BEGIN TRAP_BARRIERS TEST'

!======================================================================
! VALID BARRIERS — MPC WILL TRAP ALL OF THESE
!======================================================================

C$OMP BARRIER
      PRINT *, 'after barrier 1'

!$OMP    BARRIER
      PRINT *, 'after barrier 2'

c$omp    barrier
      PRINT *, 'after barrier 3'

!$OMP      BARRIER
      PRINT *, 'after barrier 4'

C$OMP    barrier      ! MPC allows comments after keyword
      PRINT *, 'after barrier 5'

!$OMPbarrier          ! NO SPACE REQUIRED — MPC MATCHES SUBSTRING
      PRINT *, 'after barrier 6'

!$OMP BARRIER nowait  ! EXTRA TOKENS ALLOWED — STILL A MATCH
      PRINT *, 'after barrier 7'

!$OMP    BARRIER,anything_here   ! MPC IGNORES TRAILING GARBAGE
      PRINT *, 'after barrier 8'

!======================================================================
! INVALID BARRIERS — MPC WILL *NOT* MATCH THESE
!======================================================================

 ! $OMP BARRIER       ! column 1 is NOT C,c,!    → invalid
      PRINT *, 'invalid 1'

!!$OMP BARRIER        ! column 2 is '!' not '$'  → invalid
      PRINT *, 'invalid 2'

! $OMP BARRIER        ! space before $           → invalid
      PRINT *, 'invalid 3'

C $OMP BARRIER        ! space before $           → invalid
      PRINT *, 'invalid 4'

      CALL OMP_BARRIER()  ! not an OpenMP directive
      PRINT *, 'invalid 5'

!======================================================================
! ONE MORE VALID BARRIER (counter should now be 9)
!======================================================================

!$OMP BARRIER
      PRINT *, 'after barrier 9'

      PRINT *, 'END TRAP_BARRIERS TEST'
      END PROGRAM TEST_TRAP_BARRIERS
