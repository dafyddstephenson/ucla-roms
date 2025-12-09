      PROGRAM TEST_DOUBLE_CONST
      IMPLICIT NONE

! VALID — MUST be converted to double
      A = 1.
      B = .1
      C = 9.81
      D = .5e-8
      E = 1.2e+14
      F = 3.e-2
      G = 4.e+2
      H = 7.E-3

! RUTGERS suffixes — converted if RUTGERS switch set
      I = 1.0_r8
      J = 2.5_e8+3

! INVALID — MUST NOT be converted
      10 FORMAT(F8.4)        ! inside FORMAT
      PRINT *, 1.EQ. I       ! logical operator
      PRINT *, 2.D0          ! already double
      PRINT *, 1._8          ! kind syntax
      PRINT *, '1.23'        ! inside quotes

! Dot but no number — ignore
      X = .foo
      Y = a.b

      PRINT *, 'done'
      END PROGRAM TEST_DOUBLE_CONST
