      PROGRAM TEST_QUOTES
      IMPLICIT NONE

! valid cases
      PRINT *, 'hello'
      PRINT *, "world"
      PRINT *, 'has ! inside but ok'
      PRINT *, "double "" quote OK"

! invalid — MUST trigger unmatched-quote error
      PRINT *, 'this is bad
      PRINT *, "also bad
      PRINT *, 'nested " not closed

! mixing
      PRINT *, "ok again"

      END PROGRAM TEST_QUOTES
