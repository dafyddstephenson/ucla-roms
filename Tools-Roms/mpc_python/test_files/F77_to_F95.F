      PROGRAM TEST_F77_TO_F95
      IMPLICIT NONE
! REALS
! VALID — must be promoted
      REAL A
      real b
      ReAl c

! NOT promoted — already sized
      REAL*4 d
      REAL*16 e
      REAL(KIND=4) f
      REAL(kind=8) g

! Attributes attached
      REAL :: x, y
      REAL POINTER :: p
      REAL DIMENSION(10) :: arr

! Continued declaration lines
      REAL
     &    z

      PRINT *, 'done'
! INTEGERS
! VALID — promote default INTEGER to INTEGER*4
      INTEGER A
      integer b
      InTeGeR c

! NOT promoted
      INTEGER*2 small
      INTEGER*8 big
      INTEGER(KIND=2) k2
      INTEGER(KIND=8) k8

! Integer with attributes
      INTEGER :: x, y
      INTEGER DIMENSION(3) :: arr
      INTEGER
     &    cont

      PRINT *, 'done'

      END PROGRAM TEST_F77_TO_F95
