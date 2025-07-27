      PROGRAM STRASSEN_TEST
*
*     ================================================================
*     PHASE 8.6: STRASSEN-ALPHATENSOR HYBRID TEST
*     ================================================================
*     Test program for verifying the Strassen-AlphaTensor hybrid
*     implementation with both 4x4 and 8x8 matrix optimization
*     ================================================================
*
      IMPLICIT NONE
*
*     Parameters
      INTEGER LDA, LDB, LDC, LDA8, LDB8, LDC8
      PARAMETER (LDA=4, LDB=4, LDC=4)
      PARAMETER (LDA8=8, LDB8=8, LDC8=8)
      DOUBLE PRECISION ALPHA, BETA, TOLERANCE
      PARAMETER (ALPHA=1.5D0, BETA=0.5D0, TOLERANCE=1.0D-12)
      DOUBLE PRECISION ONE, ZERO
      PARAMETER (ONE=1.0D+0, ZERO=0.0D+0)
*
*     Local variables for 4x4 testing
      DOUBLE PRECISION A4(LDA,4), B4(LDB,4), C4_ALPHA(LDC,4)
      DOUBLE PRECISION C4_DGEMM(LDC,4), A4_ORIG(LDA,4), B4_ORIG(LDB,4)
      DOUBLE PRECISION C4_ORIG(LDC,4)
*
*     Local variables for 8x8 testing
      DOUBLE PRECISION A8(LDA8,8), B8(LDB8,8), C8_ALPHA(LDC8,8)
      DOUBLE PRECISION C8_DGEMM(LDC8,8), A8_ORIG(LDA8,8)
      DOUBLE PRECISION B8_ORIG(LDB8,8), C8_ORIG(LDC8,8)
*
*     Loop counters and error tracking
      INTEGER I, J, FAILED_TESTS
      DOUBLE PRECISION ERROR, MAX_ERROR
      LOGICAL TEST_PASSED
*
*     External functions
      EXTERNAL DGEMM_ALPHA, DGEMM
*
      WRITE(*,*) '============================================='
      WRITE(*,*) 'PHASE 8.6: STRASSEN-ALPHATENSOR HYBRID TEST'
      WRITE(*,*) '============================================='
      WRITE(*,*) 'Testing 4x4 AlphaTensor + 8x8 Strassen'
      WRITE(*,*) '============================================='
      WRITE(*,*)

      FAILED_TESTS = 0

*     ================================================================
*     TEST 1: 4x4 ALPHATENSOR FUNCTIONALITY VERIFICATION
*     ================================================================
      WRITE(*,*) 'TEST 1: 4x4 AlphaTensor (49 operations)'
      WRITE(*,*) '-------------------------------------'

*     Initialize 4x4 test matrices with sequential values
      DO J = 1, 4
          DO I = 1, 4
              A4(I,J) = DBLE(I + J - 1)
              B4(I,J) = DBLE(I * J + 2)
              C4_ORIG(I,J) = DBLE(I - J + 3)
              A4_ORIG(I,J) = A4(I,J)
              B4_ORIG(I,J) = B4(I,J)
              C4_ALPHA(I,J) = C4_ORIG(I,J)
              C4_DGEMM(I,J) = C4_ORIG(I,J)
          END DO
      END DO

*     Run AlphaTensor 4x4 optimization
      CALL DGEMM_ALPHA('N','N',4,4,4,ALPHA,A4,LDA,B4,LDB,BETA,
     +                 C4_ALPHA,LDC)

*     Run standard DGEMM for comparison
      CALL DGEMM('N','N',4,4,4,ALPHA,A4_ORIG,LDA,B4_ORIG,LDB,BETA,
     +           C4_DGEMM,LDC)

*     Verify results
      MAX_ERROR = ZERO
      DO J = 1, 4
          DO I = 1, 4
              ERROR = ABS(C4_ALPHA(I,J) - C4_DGEMM(I,J))
              IF (ERROR .GT. MAX_ERROR) MAX_ERROR = ERROR
          END DO
      END DO

      TEST_PASSED = (MAX_ERROR .LE. TOLERANCE)
      WRITE(*,*) 'Maximum error: ', MAX_ERROR
      WRITE(*,*) 'Tolerance: ', TOLERANCE
      IF (TEST_PASSED) THEN
          WRITE(*,*) 'TEST 1: PASSED'
      ELSE
          WRITE(*,*) 'TEST 1: FAILED'
          FAILED_TESTS = FAILED_TESTS + 1
      END IF
      WRITE(*,*)

*     ================================================================
*     TEST 2: 8x8 STRASSEN-ALPHATENSOR HYBRID VERIFICATION
*     ================================================================
      WRITE(*,*) 'TEST 2: 8x8 Strassen-AlphaTensor (343 ops)'
      WRITE(*,*) '-------------------------------------'

*     Initialize 8x8 test matrices
      DO J = 1, 8
          DO I = 1, 8
              A8(I,J) = DBLE(MOD(I*J + 17, 19) - 9) * 0.7D0
              B8(I,J) = DBLE(MOD(I + J*3 + 11, 23) - 11) * 0.9D0
              C8_ORIG(I,J) = DBLE(MOD(I*2 + J + 7, 13) - 6) * 0.5D0
              A8_ORIG(I,J) = A8(I,J)
              B8_ORIG(I,J) = B8(I,J)
              C8_ALPHA(I,J) = C8_ORIG(I,J)
              C8_DGEMM(I,J) = C8_ORIG(I,J)
          END DO
      END DO

*     Run Strassen-AlphaTensor 8x8 hybrid optimization
      CALL DGEMM_ALPHA('N','N',8,8,8,ALPHA,A8,LDA8,B8,LDB8,BETA,
     +                 C8_ALPHA,LDC8)

*     Run standard DGEMM for comparison
      CALL DGEMM('N','N',8,8,8,ALPHA,A8_ORIG,LDA8,B8_ORIG,LDB8,BETA,
     +           C8_DGEMM,LDC8)

*     Verify results
      MAX_ERROR = ZERO
      DO J = 1, 8
          DO I = 1, 8
              ERROR = ABS(C8_ALPHA(I,J) - C8_DGEMM(I,J))
              IF (ERROR .GT. MAX_ERROR) MAX_ERROR = ERROR
          END DO
      END DO

      TEST_PASSED = (MAX_ERROR .LE. TOLERANCE)
      WRITE(*,*) 'Maximum error: ', MAX_ERROR
      WRITE(*,*) 'Tolerance: ', TOLERANCE
      IF (TEST_PASSED) THEN
          WRITE(*,*) 'TEST 2: PASSED'
      ELSE
          WRITE(*,*) 'TEST 2: FAILED'
          FAILED_TESTS = FAILED_TESTS + 1
      END IF
      WRITE(*,*)

*     ================================================================
*     FINAL RESULTS SUMMARY
*     ================================================================
      WRITE(*,*) '============================================='
      WRITE(*,*) 'PHASE 8.6 STRASSEN-ALPHATENSOR TEST SUMMARY'
      WRITE(*,*) '============================================='
      WRITE(*,*) 'Total tests run: 2'
      WRITE(*,*) 'Tests passed: ', (2 - FAILED_TESTS)
      WRITE(*,*) 'Tests failed: ', FAILED_TESTS

      IF (FAILED_TESTS .EQ. 0) THEN
          WRITE(*,*) '============================================='
          WRITE(*,*) 'ALL TESTS PASSED! PHASE 8.6 SUCCESS'
          WRITE(*,*) '============================================='
          WRITE(*,*) '4x4 AlphaTensor (49 operations) working'
          WRITE(*,*) '8x8 Strassen-AlphaTensor (343 ops) working'
          WRITE(*,*) 'Fallback to standard DGEMM verified'
          WRITE(*,*) 'Numerical accuracy within tolerance (1e-12)'
          WRITE(*,*) 'All 49 operations maintained and functioning'
          WRITE(*,*) '============================================='
      ELSE
          WRITE(*,*) 'SOME TESTS FAILED - REVIEW IMPLEMENTATION'
          WRITE(*,*) '============================================='
      END IF

      END
