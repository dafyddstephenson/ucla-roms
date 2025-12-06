      PROGRAM TEST_TRAP_BARRIERS
      IMPLICIT NONE
      INTEGER(kind=4) I
      PRINT *, 'BEGIN TRAP_BARRIERS TEST'
C$    call sync_trap(1)
C$OMP BARRIER
      PRINT *, 'after barrier 1'
C$    call sync_trap(2)
!$OMP    BARRIER
      PRINT *, 'after barrier 2'
C$    call sync_trap(3)
c$omp    barrier
      PRINT *, 'after barrier 3'
C$    call sync_trap(4)
!$OMP      BARRIER
      PRINT *, 'after barrier 4'
C$    call sync_trap(5)
C$OMP    barrier
      PRINT *, 'after barrier 5'
C$    call sync_trap(6)
!$OMPbarrier
      PRINT *, 'after barrier 6'
C$    call sync_trap(7)
!$OMP BARRIER nowait
      PRINT *, 'after barrier 7'
C$    call sync_trap(8)
!$OMP    BARRIER,anything_here
      PRINT *, 'after barrier 8'
      PRINT *, 'invalid 1'
!
      PRINT *, 'invalid 2'
! $OMP BARRIER
      PRINT *, 'invalid 3'
C $OMP BARRIER
      PRINT *, 'invalid 4'
      CALL OMP_BARRIER()
      PRINT *, 'invalid 5'
C$    call sync_trap(9)
!$OMP BARRIER
      PRINT *, 'after barrier 9'
      PRINT *, 'END TRAP_BARRIERS TEST'
      END PROGRAM TEST_TRAP_BARRIERS
