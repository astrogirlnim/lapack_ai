      PROGRAM DEBUG_TEST
*     =====================================================================
*     DEBUG TEST FOR SELECTIVE MIXED-PRECISION
*     =====================================================================

      IMPLICIT NONE

*     .. Parameters ..
      DOUBLE PRECISION, PARAMETER :: ZERO = 0.0D+0, ONE = 1.0D+0
*     ..
*     .. Local Scalars ..
      INTEGER M, N, K, LDA, LDB, LDC, I, J
      DOUBLE PRECISION ALPHA, BETA, ERROR, MAX_ERROR
      CHARACTER*1 TRANSA, TRANSB
*     ..
*     .. Local Arrays ..
      DOUBLE PRECISION A(20,20), B(20,20), C_ALPHA(20,20)
      DOUBLE PRECISION C_DGEMM(20,20)
*     ..
*     .. External Subroutines ..
      EXTERNAL DGEMM_ALPHA, DGEMM

      WRITE(*,*) 'SELECTIVE MIXED-PRECISION DEBUG TEST'

*     Test 1: 4x4 Direct (should use mixed-precision)
      WRITE(*,*)
      WRITE(*,*) 'TEST 1: 4x4 Direct (Mixed-Precision Path)'
      M = 4
      N = 4
      K = 4
      LDA = 4
      LDB = 4
      LDC = 4
      ALPHA = 1.5D0
      BETA = 0.5D0
      TRANSA = 'N'
      TRANSB = 'N'

*     Simple test matrices
      DO J = 1, 4
          DO I = 1, 4
              A(I,J) = DBLE(I + J)
              B(I,J) = DBLE(I * J)
              C_ALPHA(I,J) = DBLE(I - J)
              C_DGEMM(I,J) = C_ALPHA(I,J)
          END DO
      END DO

      CALL DGEMM_ALPHA(TRANSA, TRANSB, M, N, K, ALPHA, A, LDA,
     +                 B, LDB, BETA, C_ALPHA, LDC)
      CALL DGEMM(TRANSA, TRANSB, M, N, K, ALPHA, A, LDA,
     +           B, LDB, BETA, C_DGEMM, LDC)

      MAX_ERROR = ZERO
      DO J = 1, 4
          DO I = 1, 4
              ERROR = ABS(C_ALPHA(I,J) - C_DGEMM(I,J))
              IF (ERROR .GT. MAX_ERROR) MAX_ERROR = ERROR
          END DO
      END DO
      WRITE(*,*) '  Error: ', MAX_ERROR

*     Test 2: 6x6 (should fallback to standard DGEMM)
      WRITE(*,*)
      WRITE(*,*) 'TEST 2: 6x6 Fallback (Standard DGEMM Path)'
      M = 6
      N = 6
      K = 6
      LDA = 6
      LDB = 6
      LDC = 6

      DO J = 1, 6
          DO I = 1, 6
              A(I,J) = DBLE(I + J) * 0.5D0
              B(I,J) = DBLE(I * J) * 0.3D0
              C_ALPHA(I,J) = DBLE(I - J) * 0.1D0
              C_DGEMM(I,J) = C_ALPHA(I,J)
          END DO
      END DO

      CALL DGEMM_ALPHA(TRANSA, TRANSB, M, N, K, ALPHA, A, LDA,
     +                 B, LDB, BETA, C_ALPHA, LDC)
      CALL DGEMM(TRANSA, TRANSB, M, N, K, ALPHA, A, LDA,
     +           B, LDB, BETA, C_DGEMM, LDC)

      MAX_ERROR = ZERO
      DO J = 1, 6
          DO I = 1, 6
              ERROR = ABS(C_ALPHA(I,J) - C_DGEMM(I,J))
              IF (ERROR .GT. MAX_ERROR) MAX_ERROR = ERROR
          END DO
      END DO
      WRITE(*,*) '  Error: ', MAX_ERROR

*     Test 3: 8x8 (should use Strassen-AlphaTensor)
      WRITE(*,*)
      WRITE(*,*) 'TEST 3: 8x8 Strassen (Full DOUBLE Precision)'
      M = 8
      N = 8
      K = 8
      LDA = 20
      LDB = 20
      LDC = 20

      DO J = 1, 8
          DO I = 1, 8
              A(I,J) = DBLE(MOD(I*J + 17, 19) - 9) * 0.7D0
              B(I,J) = DBLE(MOD(I + J*3 + 11, 23) - 11) * 0.9D0
              C_ALPHA(I,J) = DBLE(MOD(I*2 + J + 7, 13) - 6) * 0.5D0
              C_DGEMM(I,J) = C_ALPHA(I,J)
          END DO
      END DO

      CALL DGEMM_ALPHA(TRANSA, TRANSB, M, N, K, ALPHA, A, LDA,
     +                 B, LDB, BETA, C_ALPHA, LDC)
      CALL DGEMM(TRANSA, TRANSB, M, N, K, ALPHA, A, LDA,
     +           B, LDB, BETA, C_DGEMM, LDC)

      MAX_ERROR = ZERO
      DO J = 1, 8
          DO I = 1, 8
              ERROR = ABS(C_ALPHA(I,J) - C_DGEMM(I,J))
              IF (ERROR .GT. MAX_ERROR) MAX_ERROR = ERROR
          END DO
      END DO
      WRITE(*,*) '  Error: ', MAX_ERROR

      WRITE(*,*)
      WRITE(*,*) 'DEBUG TEST COMPLETE'

      END
