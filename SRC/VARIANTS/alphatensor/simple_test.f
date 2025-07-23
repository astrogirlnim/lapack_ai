      PROGRAM SIMPLE_TEST
*
*     Simple test to check our AlphaTensor implementation
*
      IMPLICIT NONE
*
*     .. Local Scalars ..
      INTEGER I, J
      DOUBLE PRECISION ALPHA, BETA
      DOUBLE PRECISION A(4,4), B(4,4), C(4,4)
*
*     Initialize test matrices (same as Python reference)
      DATA ((A(I,J), J=1,4), I=1,4) /
     +  1.0D0,  2.0D0,  3.0D0,  4.0D0,
     +  5.0D0,  6.0D0,  7.0D0,  8.0D0,
     +  9.0D0, 10.0D0, 11.0D0, 12.0D0,
     + 13.0D0, 14.0D0, 15.0D0, 16.0D0 /
*
      DATA ((B(I,J), J=1,4), I=1,4) /
     +  1.0D0, 1.0D0, 1.0D0, 1.0D0,
     +  2.0D0, 2.0D0, 2.0D0, 2.0D0,
     +  3.0D0, 3.0D0, 3.0D0, 3.0D0,
     +  4.0D0, 4.0D0, 4.0D0, 4.0D0 /
*
*     Initialize C matrix
      DO I = 1, 4
          DO J = 1, 4
              C(I,J) = 0.0D0
          END DO
      END DO
*
*     Set ALPHA=1, BETA=0 for simple test
      ALPHA = 1.0D0
      BETA = 0.0D0
*
      WRITE(*,*) 'Input matrices:'
      WRITE(*,*) 'A matrix:'
      DO I = 1, 4
          WRITE(*,'(4F8.1)') (A(I,J), J=1,4)
      END DO
*
      WRITE(*,*) 'B matrix:'
      DO I = 1, 4
          WRITE(*,'(4F8.1)') (B(I,J), J=1,4)
      END DO
*
*     Call our AlphaTensor implementation
      CALL DGEMM_ALPHATENSOR_CORRECT(ALPHA, A, 4, B, 4, BETA, C, 4)
*
      WRITE(*,*) 'Result C matrix:'
      DO I = 1, 4
          WRITE(*,'(4F15.1)') (C(I,J), J=1,4)
      END DO
*
      END PROGRAM SIMPLE_TEST
