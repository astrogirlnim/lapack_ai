      PROGRAM MIXED_PRECISION_TEST
*     =====================================================================
*     COMPREHENSIVE MIXED-PRECISION ALPHATENSOR TEST
*     =====================================================================

      IMPLICIT NONE

*     .. Parameters ..
      INTEGER, PARAMETER :: NMAX = 20
      DOUBLE PRECISION, PARAMETER :: ZERO = 0.0D+0, ONE = 1.0D+0
      DOUBLE PRECISION, PARAMETER :: TOLERANCE = 1.0D-12
*     ..
*     .. Local Scalars ..
      INTEGER M, N, K, LDA, LDB, LDC, I, J
      DOUBLE PRECISION ALPHA, BETA, ERROR, MAX_ERROR
      CHARACTER*1 TRANSA, TRANSB
*     ..
*     .. Local Arrays ..
      DOUBLE PRECISION A(NMAX,NMAX), B(NMAX,NMAX), C_ALPHA(NMAX,NMAX)
      DOUBLE PRECISION C_DGEMM(NMAX,NMAX)
*     ..
*     .. External Subroutines ..
      EXTERNAL DGEMM_ALPHA, DGEMM

      WRITE(*,*) 'PHASE 8.6 APPROACH 3: MIXED-PRECISION TEST'
      WRITE(*,*) 'Testing REAL intermediate + DOUBLE accumulation'

*     Test 1: 4x4 Direct AlphaTensor (Mixed-Precision)
      WRITE(*,*)
      WRITE(*,*) 'TEST 1: 4x4 Direct AlphaTensor Mixed-Precision'
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

*     Initialize test matrices
      DO J = 1, 4
          DO I = 1, 4
              A(I,J) = DBLE(I + J - 1)
              B(I,J) = DBLE(I * J) * 0.5D0
              C_ALPHA(I,J) = DBLE(I - J) * 0.1D0
              C_DGEMM(I,J) = C_ALPHA(I,J)
          END DO
      END DO

*     Test mixed-precision AlphaTensor
      CALL DGEMM_ALPHA(TRANSA, TRANSB, M, N, K, ALPHA, A, LDA,
     +                 B, LDB, BETA, C_ALPHA, LDC)

*     Test standard DGEMM for comparison
      CALL DGEMM(TRANSA, TRANSB, M, N, K, ALPHA, A, LDA,
     +           B, LDB, BETA, C_DGEMM, LDC)

*     Calculate accuracy
      MAX_ERROR = ZERO
      DO J = 1, 4
          DO I = 1, 4
              ERROR = ABS(C_ALPHA(I,J) - C_DGEMM(I,J))
              IF (ERROR .GT. MAX_ERROR) MAX_ERROR = ERROR
          END DO
      END DO

      WRITE(*,*) '  Maximum error vs DGEMM: ', MAX_ERROR
      IF (MAX_ERROR .LT. TOLERANCE) THEN
          WRITE(*,*) '  PASSED: Accuracy within tolerance'
      ELSE
          WRITE(*,*) '  FAILED: Accuracy exceeds tolerance'
      END IF

*     Test 2: 8x8 Strassen-AlphaTensor Hybrid
      WRITE(*,*)
      WRITE(*,*) 'TEST 2: 8x8 Strassen-AlphaTensor Hybrid'
      M = 8
      N = 8
      K = 8
      LDA = 20
      LDB = 20
      LDC = 20

*     Initialize larger test matrices with working pattern
      DO J = 1, 8
          DO I = 1, 8
              A(I,J) = DBLE(MOD(I*J + 17, 19) - 9) * 0.7D0
              B(I,J) = DBLE(MOD(I + J*3 + 11, 23) - 11) * 0.9D0
              C_ALPHA(I,J) = DBLE(MOD(I*2 + J + 7, 13) - 6) * 0.5D0
              C_DGEMM(I,J) = C_ALPHA(I,J)
          END DO
      END DO

*     Test hybrid algorithm
      CALL DGEMM_ALPHA(TRANSA, TRANSB, M, N, K, ALPHA, A, LDA,
     +                 B, LDB, BETA, C_ALPHA, LDC)

*     Test standard DGEMM for comparison
      CALL DGEMM(TRANSA, TRANSB, M, N, K, ALPHA, A, LDA,
     +           B, LDB, BETA, C_DGEMM, LDC)

*     Calculate accuracy
      MAX_ERROR = ZERO
      DO J = 1, 8
          DO I = 1, 8
              ERROR = ABS(C_ALPHA(I,J) - C_DGEMM(I,J))
              IF (ERROR .GT. MAX_ERROR) MAX_ERROR = ERROR
          END DO
      END DO

      WRITE(*,*) '  Maximum error vs DGEMM: ', MAX_ERROR
      IF (MAX_ERROR .LT. TOLERANCE) THEN
          WRITE(*,*) '  PASSED: Accuracy within tolerance'
      ELSE
          WRITE(*,*) '  FAILED: Accuracy exceeds tolerance'
      END IF

*     Test 3: 16x16 Block-wise AlphaTensor
      WRITE(*,*)
      WRITE(*,*) 'TEST 3: 16x16 Block-wise AlphaTensor'
      M = 16
      N = 16
      K = 16
      LDA = 20
      LDB = 20
      LDC = 20

*     Initialize block-wise test matrices with working pattern
      DO J = 1, 16
          DO I = 1, 16
              A(I,J) = DBLE(MOD(I*J + 13, 17) - 8) * 0.6D0
              B(I,J) = DBLE(MOD(I + J*2 + 9, 19) - 9) * 0.8D0
              C_ALPHA(I,J) = DBLE(MOD(I*3 + J + 5, 11) - 5) * 0.4D0
              C_DGEMM(I,J) = C_ALPHA(I,J)
          END DO
      END DO

*     Test block-wise algorithm
      CALL DGEMM_ALPHA(TRANSA, TRANSB, M, N, K, ALPHA, A, LDA,
     +                 B, LDB, BETA, C_ALPHA, LDC)

*     Test standard DGEMM for comparison
      CALL DGEMM(TRANSA, TRANSB, M, N, K, ALPHA, A, LDA,
     +           B, LDB, BETA, C_DGEMM, LDC)

*     Calculate accuracy
      MAX_ERROR = ZERO
      DO J = 1, 16
          DO I = 1, 16
              ERROR = ABS(C_ALPHA(I,J) - C_DGEMM(I,J))
              IF (ERROR .GT. MAX_ERROR) MAX_ERROR = ERROR
          END DO
      END DO

      WRITE(*,*) '  Maximum error vs DGEMM: ', MAX_ERROR
      IF (MAX_ERROR .LT. TOLERANCE) THEN
          WRITE(*,*) '  PASSED: Accuracy within tolerance'
      ELSE
          WRITE(*,*) '  FAILED: Accuracy exceeds tolerance'
      END IF

      WRITE(*,*)
      WRITE(*,*) 'MIXED-PRECISION TEST COMPLETE'
      WRITE(*,*) 'Implementation ready for performance testing'

      END
