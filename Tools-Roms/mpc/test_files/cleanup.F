      PROGRAM TEST_CLEANUP
      IMPLICIT NONE

! blank lines should be removed


      PRINT *, 'cleanup start'

C this is a comment – removed by MPC
c this too
* also removed
! normal F90 comment removed

C$OMP PARALLEL      ! should be KEPT
c$omp    do         ! kept (OpenMP sentinel)
!$OMP DO            ! kept

      PRINT *, 'cleanup middle'

      ! Trailing comment removed after !

      PRINT *, 'cleanup end'
      END PROGRAM TEST_CLEANUP
