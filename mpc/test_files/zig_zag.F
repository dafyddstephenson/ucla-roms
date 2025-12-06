      PROGRAM TEST_ZIGZAG
      IMPLICIT NONE
      INTEGER tile

      PRINT *, 'BEGIN ZIGZAG'

! VALID literal matches
      do tile = my_first, my_last
      end do

      do   tile   =  my_first, my_last
      end do

      do tile=my_first,my_last
      end do

! NOT valid
      do tiles = my_first, my_last   ! wrong var name
      end do

      do tile = myRange_first, myRange_last ! wrong prefix
      end do

      do tile = foo, bar            ! wrong prefix
      end do

! VALID again
      do tile = my_something, my_anything
      end do

      PRINT *, 'END ZIGZAG'
      END PROGRAM TEST_ZIGZAG
