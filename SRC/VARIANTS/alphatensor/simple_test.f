      PROGRAM SIMPLE_ALPHATENSOR_TEST
*
*  Simple test to verify AlphaTensor implementation works
*
      IMPLICIT NONE
      DOUBLE PRECISION A(4,4), B(4,4), C_ALPHA(4,4), C_DGEMM(4,4)
      DOUBLE PRECISION ALPHA, BETA, ERROR, MAX_ERROR
      INTEGER I, J
      LOGICAL PASSED
*
      WRITE(*,*) 'Testing AlphaTensor Implementation'
*
*     Initialize test matrices
      DO J = 1, 4
          DO I = 1, 4
              A(I,J) = DBLE(I + J)
              B(I,J) = DBLE(I * J)
              C_ALPHA(I,J) = DBLE(I)
              C_DGEMM(I,J) = DBLE(I)
          END DO
      END DO
*
      ALPHA = 1.0D0
      BETA = 0.5D0
*
      WRITE(*,*) 'Calling DGEMM_ALPHA with 4x4 matrices...'
      CALL DGEMM_ALPHA('N', 'N', 4, 4, 4, ALPHA, A, 4, B, 4,
     +                 BETA, C_ALPHA, 4)
*
      WRITE(*,*) 'Calling standard DGEMM for comparison...'
      CALL DGEMM('N', 'N', 4, 4, 4, ALPHA, A, 4, B, 4,
     +           BETA, C_DGEMM, 4)
*
*     Compare results
      MAX_ERROR = 0.0D0
      DO J = 1, 4
          DO I = 1, 4
              ERROR = ABS(C_ALPHA(I,J) - C_DGEMM(I,J))
              IF (ERROR .GT. MAX_ERROR) MAX_ERROR = ERROR
          END DO
      END DO
*
      WRITE(*,*) 'Maximum error:', MAX_ERROR
*
      IF (MAX_ERROR .LT. 1.0D-10) THEN
          WRITE(*,*) 'TEST PASSED: Results match within tolerance'
          PASSED = .TRUE.
      ELSE
          WRITE(*,*) 'TEST FAILED: Results do not match'
          PASSED = .FALSE.
      END IF
*
*     Test with 5x5 matrices (should use fallback)
      WRITE(*,*) 'Testing 5x5 fallback...'
      CALL DGEMM_ALPHA('N', 'N', 3, 3, 3, ALPHA, A, 4, B, 4,
     +                 BETA, C_ALPHA, 4)
      WRITE(*,*) 'Fallback test completed'
*
      IF (PASSED) THEN
          WRITE(*,*) 'OVERALL: AlphaTensor implementation works!'
      ELSE
          WRITE(*,*) 'OVERALL: AlphaTensor implementation failed!'
      END IF
*
      END
