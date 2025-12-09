      PROGRAM TEST_QUOTES
      IMPLICIT NONE
      PRINT *, 'hello'
      PRINT *, "world"
      PRINT *, 'has ! inside but ok'
      PRINT *, "double "" quote OK"

      ### ERROR: Unmatched quote on line  11

      PRINT *, 'this is bad

      ### ERROR: Unmatched quote on line  12

      PRINT *, "also bad

      ### ERROR: Unmatched quote on line  13

      PRINT *, 'nested " not closed
      PRINT *, "ok again"
      END PROGRAM TEST_QUOTES
