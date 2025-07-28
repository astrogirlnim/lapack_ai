      PROGRAM ROBUSTNESS_TEST
*     =====================================================================
*     COMPREHENSIVE ROBUSTNESS TEST FOR ALPHATENSOR IMPLEMENTATION
*     =====================================================================
*     Tests implementation under various conditions to identify real bugs
*     vs. test setup issues
*     =====================================================================

      IMPLICIT NONE

*     .. Parameters ..
      INTEGER, PARAMETER :: NMAX = 50
      DOUBLE PRECISION, PARAMETER :: ZERO = 0.0D+0, ONE = 1.0D+0
      DOUBLE PRECISION, PARAMETER :: TOLERANCE = 1.0D-10
*     ..
*     .. Local Scalars ..
      INTEGER M, N, K, LDA, LDB, LDC, I, J, TEST_NUM, FAILURES
      DOUBLE PRECISION ALPHA, BETA, ERROR, MAX_ERROR
      CHARACTER*1 TRANSA, TRANSB
      LOGICAL PASSED
*     ..
*     .. Local Arrays ..
      DOUBLE PRECISION A(NMAX,NMAX), B(NMAX,NMAX), C_ALPHA(NMAX,NMAX)
      DOUBLE PRECISION C_DGEMM(NMAX,NMAX)
*     ..
*     .. External Subroutines ..
      EXTERNAL DGEMM_ALPHA, DGEMM

      WRITE(*,*) 'COMPREHENSIVE ROBUSTNESS TEST FOR ALPHATENSOR'
      WRITE(*,*) 'Testing various conditions to identify real bugs'
      WRITE(*,*)

      TEST_NUM = 0
      FAILURES = 0
      TRANSA = 'N'
      TRANSB = 'N'

*     ================================================================
*     TEST GROUP 1: 4x4 with various leading dimensions
*     ================================================================
      WRITE(*,*) 'GROUP 1: 4x4 with various leading dimensions'
      M = 4
      N = 4
      K = 4
      ALPHA = 1.5D0
      BETA = 0.5D0

      DO LDA = 4, 10
          DO LDB = 4, 10
              DO LDC = 4, 10
                  TEST_NUM = TEST_NUM + 1

*                 Initialize matrices
                  DO J = 1, 4
                      DO I = 1, 4
                          A(I,J) = DBLE(I + J - 1) * 0.5D0
                          B(I,J) = DBLE(I * J) * 0.3D0
                          C_ALPHA(I,J) = DBLE(I - J) * 0.2D0
                          C_DGEMM(I,J) = C_ALPHA(I,J)
                      END DO
                  END DO

                  CALL DGEMM_ALPHA(TRANSA, TRANSB, M, N, K, ALPHA, A,
     +                             LDA, B, LDB, BETA, C_ALPHA, LDC)
                  CALL DGEMM(TRANSA, TRANSB, M, N, K, ALPHA, A, LDA,
     +                       B, LDB, BETA, C_DGEMM, LDC)

                  MAX_ERROR = ZERO
                  DO J = 1, 4
                      DO I = 1, 4
                          ERROR = ABS(C_ALPHA(I,J) - C_DGEMM(I,J))
                          IF (ERROR .GT. MAX_ERROR) MAX_ERROR = ERROR
                      END DO
                  END DO

                  PASSED = (MAX_ERROR .LT. TOLERANCE)
                  IF (.NOT. PASSED) THEN
                      FAILURES = FAILURES + 1
                      WRITE(*,*) '  FAIL: LDA=', LDA, ' LDB=', LDB,
     +                           ' LDC=', LDC, ' Error=', MAX_ERROR
                  END IF
              END DO
          END DO
      END DO

*     ================================================================
*     TEST GROUP 2: 8x8 with various conditions
*     ================================================================
      WRITE(*,*) 'GROUP 2: 8x8 Strassen with various conditions'
      M = 8
      N = 8
      K = 8

*     Test 2a: Various leading dimensions
      DO LDA = 8, 12
          DO LDB = 8, 12
              DO LDC = 8, 12
                  TEST_NUM = TEST_NUM + 1

*                 Initialize matrices (working pattern)
                  DO J = 1, 8
                      DO I = 1, 8
                          A(I,J) = DBLE(MOD(I*J + 17, 19) - 9) * 0.7D0
                          B(I,J) = DBLE(MOD(I + J*3 + 11, 23) - 11)
     +                             * 0.9D0
                          C_ALPHA(I,J) = DBLE(MOD(I*2 + J + 7, 13) - 6)
     +                                   * 0.5D0
                          C_DGEMM(I,J) = C_ALPHA(I,J)
                      END DO
                  END DO

                  CALL DGEMM_ALPHA(TRANSA, TRANSB, M, N, K, ALPHA, A,
     +                             LDA, B, LDB, BETA, C_ALPHA, LDC)
                  CALL DGEMM(TRANSA, TRANSB, M, N, K, ALPHA, A, LDA,
     +                       B, LDB, BETA, C_DGEMM, LDC)

                  MAX_ERROR = ZERO
                  DO J = 1, 8
                      DO I = 1, 8
                          ERROR = ABS(C_ALPHA(I,J) - C_DGEMM(I,J))
                          IF (ERROR .GT. MAX_ERROR) MAX_ERROR = ERROR
                      END DO
                  END DO

                  PASSED = (MAX_ERROR .LT. TOLERANCE)
                  IF (.NOT. PASSED) THEN
                      FAILURES = FAILURES + 1
                      WRITE(*,*) '  FAIL 8x8: LDA=', LDA, ' LDB=', LDB,
     +                           ' LDC=', LDC, ' Error=', MAX_ERROR
                  END IF
              END DO
          END DO
      END DO

*     Test 2b: Various matrix patterns
      LDA = 20
      LDB = 20
      LDC = 20

*     Simple pattern
      TEST_NUM = TEST_NUM + 1
      DO J = 1, 8
          DO I = 1, 8
              A(I,J) = DBLE(I + J) * 0.2D0
              B(I,J) = DBLE(I * J) * 0.4D0
              C_ALPHA(I,J) = DBLE(I - J) * 0.1D0
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

      PASSED = (MAX_ERROR .LT. TOLERANCE)
      IF (.NOT. PASSED) THEN
          FAILURES = FAILURES + 1
          WRITE(*,*) '  FAIL 8x8 simple pattern: Error=', MAX_ERROR
      END IF

*     Test 2c: Various ALPHA/BETA values
      DO ALPHA = 0.5D0, 3.0D0, 0.5D0
          DO BETA = 0.0D0, 2.0D0, 0.5D0
              TEST_NUM = TEST_NUM + 1

              DO J = 1, 8
                  DO I = 1, 8
                      A(I,J) = DBLE(I + J) * 0.3D0
                      B(I,J) = DBLE(I * J) * 0.6D0
                      C_ALPHA(I,J) = DBLE(I - J + 4) * 0.2D0
                      C_DGEMM(I,J) = C_ALPHA(I,J)
                  END DO
              END DO

              CALL DGEMM_ALPHA(TRANSA, TRANSB, M, N, K, ALPHA, A, LDA,
     +                         B, LDB, BETA, C_ALPHA, LDC)
              CALL DGEMM(TRANSA, TRANSB, M, N, K, ALPHA, A, LDA,
     +                   B, LDB, BETA, C_DGEMM, LDC)

              MAX_ERROR = ZERO
              DO J = 1, 8
                  DO I = 1, 8
                      ERROR = ABS(C_ALPHA(I,J) - C_DGEMM(I,J))
                      IF (ERROR .GT. MAX_ERROR) MAX_ERROR = ERROR
                  END DO
              END DO

              PASSED = (MAX_ERROR .LT. TOLERANCE)
              IF (.NOT. PASSED) THEN
                  FAILURES = FAILURES + 1
                  WRITE(*,*) '  FAIL 8x8 ALPHA=', ALPHA, ' BETA=', BETA,
     +                       ' Error=', MAX_ERROR
              END IF
          END DO
      END DO

*     ================================================================
*     SUMMARY
*     ================================================================
      WRITE(*,*)
      WRITE(*,*) 'ROBUSTNESS TEST SUMMARY'
      WRITE(*,*) 'Total tests run: ', TEST_NUM
      WRITE(*,*) 'Tests failed: ', FAILURES
      WRITE(*,*) 'Success rate: ',
     +           DBLE(TEST_NUM - FAILURES) / DBLE(TEST_NUM) * 100.0D0,
     +           '%'

      IF (FAILURES .EQ. 0) THEN
          WRITE(*,*) 'IMPLEMENTATION IS ROBUST'
      ELSE
          WRITE(*,*) 'IMPLEMENTATION HAS SYSTEMATIC ISSUES'
      END IF

      END
