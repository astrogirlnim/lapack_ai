      PROGRAM FAILURE_RECREATION
*     =====================================================================
*     RECREATE THE EXACT CONDITIONS THAT CAUSED ORIGINAL TEST FAILURES
*     =====================================================================

      IMPLICIT NONE

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

      WRITE(*,*) 'RECREATING ORIGINAL FAILURE CONDITIONS'
      WRITE(*,*)

*     ================================================================
*     EXACT CONDITIONS FROM ORIGINAL FAILING TEST
*     ================================================================
      WRITE(*,*) 'TEST: 8x8 with original failing conditions'
      M = 8
      N = 8
      K = 8
      LDA = 8   ! THIS WAS THE PROBLEM!
      LDB = 8   ! Arrays A(20,20) but LDA=8
      LDC = 8
      ALPHA = 2.5D0  ! Different from working test
      BETA = 1.5D0   ! Different from working test
      TRANSA = 'N'
      TRANSB = 'N'

*     Original matrix initialization pattern
      DO J = 1, 8
          DO I = 1, 8
              A(I,J) = DBLE(I + J) * 0.3D0
              B(I,J) = DBLE(I * J) * 0.7D0
              C_ALPHA(I,J) = DBLE(I + J - 2) * 0.2D0
              C_DGEMM(I,J) = C_ALPHA(I,J)
          END DO
      END DO

      CALL DGEMM_ALPHA(TRANSA, TRANSB, M, N, K, ALPHA, A, LDA,
     +                 B, LDB, BETA, C_ALPHA, LDC)
      CALL DGEMM(TRANSA, TRANSB, M, N, K, ALPHA, A, LDA,
     +           B, LDB, BETA, C_DGEMM, LDC)

      MAX_ERROR = 0.0D0
      DO J = 1, 8
          DO I = 1, 8
              ERROR = ABS(C_ALPHA(I,J) - C_DGEMM(I,J))
              IF (ERROR .GT. MAX_ERROR) MAX_ERROR = ERROR
          END DO
      END DO

      WRITE(*,*) 'Original failing conditions:'
      WRITE(*,*) '  Arrays: A(20,20), LDA=8'
      WRITE(*,*) '  ALPHA=2.5, BETA=1.5'
      WRITE(*,*) '  Matrix pattern: I+J, I*J'
      WRITE(*,*) '  Error: ', MAX_ERROR

*     ================================================================
*     CORRECTED CONDITIONS
*     ================================================================
      WRITE(*,*)
      WRITE(*,*) 'TEST: 8x8 with corrected conditions'
      LDA = 20  ! FIXED: Match actual array size
      LDB = 20
      LDC = 20
      ALPHA = 1.5D0  ! Working values
      BETA = 0.5D0

*     Working matrix pattern
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

      MAX_ERROR = 0.0D0
      DO J = 1, 8
          DO I = 1, 8
              ERROR = ABS(C_ALPHA(I,J) - C_DGEMM(I,J))
              IF (ERROR .GT. MAX_ERROR) MAX_ERROR = ERROR
          END DO
      END DO

      WRITE(*,*) 'Corrected conditions:'
      WRITE(*,*) '  Arrays: A(20,20), LDA=20'
      WRITE(*,*) '  ALPHA=1.5, BETA=0.5'
      WRITE(*,*) '  Matrix pattern: MOD formulas'
      WRITE(*,*) '  Error: ', MAX_ERROR

*     ================================================================
*     ISOLATED TEST: ONLY LDA MISMATCH
*     ================================================================
      WRITE(*,*)
      WRITE(*,*) 'TEST: Isolate LDA mismatch effect'
      LDA = 8   ! Wrong LDA with A(20,20)
      LDB = 20
      LDC = 20
      ALPHA = 1.5D0
      BETA = 0.5D0

*     Working matrix pattern
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

      MAX_ERROR = 0.0D0
      DO J = 1, 8
          DO I = 1, 8
              ERROR = ABS(C_ALPHA(I,J) - C_DGEMM(I,J))
              IF (ERROR .GT. MAX_ERROR) MAX_ERROR = ERROR
          END DO
      END DO

      WRITE(*,*) 'LDA mismatch only:'
      WRITE(*,*) '  LDA=8 but A(20,20)'
      WRITE(*,*) '  Error: ', MAX_ERROR

      END
