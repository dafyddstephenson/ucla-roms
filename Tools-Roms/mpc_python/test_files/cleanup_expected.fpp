      PROGRAM TEST_CLEANUP
      IMPLICIT NONE
      PRINT *, 'cleanup start'
C$OMP PARALLEL
c$omp    do
!$OMP DO
      PRINT *, 'cleanup middle'
      PRINT *, 'cleanup end'
      END PROGRAM TEST_CLEANUP
