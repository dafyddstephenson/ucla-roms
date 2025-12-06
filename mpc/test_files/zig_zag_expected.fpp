      PROGRAM TEST_ZIGZAG
      IMPLICIT NONE
      INTEGER(kind=4) tile
      PRINT *, 'BEGIN ZIGZAG'
      do tile = my_first,my_last,+1
      end do
      do   tile   =  my_last,my_first,-1
      end do
      do tile=my_first,my_last,+1
      end do
      do tiles = my_first, my_last
      end do
      do tile = myRange_first, myRange_last
      end do
      do tile = foo, bar
      end do
      do tile = my_last,my_first,-1
      end do
      PRINT *, 'END ZIGZAG'
      END PROGRAM TEST_ZIGZAG
