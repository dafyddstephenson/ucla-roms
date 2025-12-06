      PROGRAM TEST_DOUBLE_CONST
      IMPLICIT NONE
      A = 1._8
      B = .1_8
      C = 9.81_8
      D = .5D-8
      E = 1.2D+14
      F = 3.D-2
      G = 4.D+2
      H = 7.D-3
      I = 1.0_r8
      J = 2.5_e8+3
      10 FORMAT(F8.4)
      PRINT *, 1.EQ. I
      PRINT *, 2.D0
      PRINT *, 1._8
      PRINT *, '1.23'
      X = .foo
      Y = a.b
      PRINT *, 'done'
      END PROGRAM TEST_DOUBLE_CONST
