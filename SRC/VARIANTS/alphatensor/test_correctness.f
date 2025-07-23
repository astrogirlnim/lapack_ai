      PROGRAM TEST_ALPHATENSOR_CORRECTNESS
*
*  Test if our AlphaTensor implementation produces correct results
*
      IMPLICIT NONE
*
*     .. Parameters ..
      INTEGER LDIM
      PARAMETER (LDIM = 4)
      DOUBLE PRECISION ZERO, ONE
      PARAMETER (ZERO = 0.0D+0, ONE = 1.0D+0)
*
*     .. Local Scalars ..
      INTEGER I, J, K
      DOUBLE PRECISION ALPHA, BETA
      DOUBLE PRECISION MAX_ERROR, ERROR
*
*     .. Local Arrays ..
      DOUBLE PRECISION A(LDIM,LDIM), B(LDIM,LDIM)
      DOUBLE PRECISION C_ALPHA(LDIM,LDIM), C_STANDARD(LDIM,LDIM)
      DOUBLE PRECISION C_EXPECTED(LDIM,LDIM)
*
*     .. External Subroutines ..
      EXTERNAL DGEMM_ALPHA, DGEMM
*
      WRITE(*,*) 'Testing AlphaTensor correctness...'
*
*     Initialize test matrices
      DO J = 1, 4
          DO I = 1, 4
              A(I,J) = DBLE(I + J)
              B(I,J) = DBLE(I * J + 1)
              C_ALPHA(I,J) = DBLE(I - J + 1)
              C_STANDARD(I,J) = C_ALPHA(I,J)
          END DO
      END DO
*
      ALPHA = 1.5D+0
      BETA = 0.5D+0
*
*     Compute expected result manually
      DO J = 1, 4
          DO I = 1, 4
              C_EXPECTED(I,J) = BETA * C_ALPHA(I,J)
              DO K = 1, 4
                  C_EXPECTED(I,J) = C_EXPECTED(I,J) + ALPHA * A(I,K) * B(K,J)
              END DO
          END DO
      END DO
*
*     Test our AlphaTensor implementation
      CALL DGEMM_ALPHA('N','N',4,4,4,ALPHA,A,4,B,4,BETA,C_ALPHA,4)
*
*     Test standard DGEMM for comparison
      CALL DGEMM('N','N',4,4,4,ALPHA,A,4,B,4,BETA,C_STANDARD,4)
*
*     Check errors
      MAX_ERROR = ZERO
      DO J = 1, 4
          DO I = 1, 4
              ERROR = ABS(C_ALPHA(I,J) - C_EXPECTED(I,J))
              IF (ERROR > MAX_ERROR) MAX_ERROR = ERROR

              WRITE(*,99) I,J,C_ALPHA(I,J),C_EXPECTED(I,J),ERROR
          END DO
      END DO
*
      WRITE(*,*) 'Maximum error:', MAX_ERROR
      IF (MAX_ERROR < 1.0D-12) THEN
          WRITE(*,*) 'TEST PASSED: AlphaTensor results correct'
      ELSE
          WRITE(*,*) 'TEST FAILED: AlphaTensor results incorrect'
      END IF
*
   99 FORMAT('C(',I1,',',I1,') = ',F12.6,' Expected = ',F12.6,' Error = ',E12.3)
*
      END
