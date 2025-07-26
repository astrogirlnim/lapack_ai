      PROGRAM COMPREHENSIVE_ALPHATENSOR_TEST
*
*  PHASE 8.6: Comprehensive test for complete multi-algorithm suite
*  Tests 4x4 Direct, 8x8 Strassen-AlphaTensor, and 16x16+ Block-wise
*
      IMPLICIT NONE
*
*     .. Parameters ..
      INTEGER LDIM4, LDIM8, LDIM16, LDIM20
      PARAMETER (LDIM4 = 4, LDIM8 = 8, LDIM16 = 16, LDIM20 = 20)
      DOUBLE PRECISION ZERO, ONE, TWO
      PARAMETER (ZERO = 0.0D+0, ONE = 1.0D+0, TWO = 2.0D+0)
      DOUBLE PRECISION TOLERANCE_4X4, TOLERANCE_8X8, TOLERANCE_BLOCK
*     Different tolerances for different optimization paths
      PARAMETER (TOLERANCE_4X4 = 5.0D-14)     ! Direct AlphaTensor
      PARAMETER (TOLERANCE_8X8 = 1.0D-12)     ! Strassen-AlphaTensor
      PARAMETER (TOLERANCE_BLOCK = 5.0D-12)   ! Block-wise AlphaTensor
      DOUBLE PRECISION DLAMCH, EPS_MACHINE
      EXTERNAL DLAMCH
*
*     .. Local Scalars ..
      INTEGER I, J, K, TEST_COUNT, PASS_COUNT
      DOUBLE PRECISION ALPHA, BETA
      DOUBLE PRECISION MAX_ERROR, CURRENT_ERROR
      LOGICAL ALL_TESTS_PASSED
*
*     .. Local Arrays ..
*     4x4 matrices for Direct AlphaTensor testing
      DOUBLE PRECISION A4(LDIM4,LDIM4), B4(LDIM4,LDIM4)
      DOUBLE PRECISION C4_ALPHA(LDIM4,LDIM4), C4_STANDARD(LDIM4,LDIM4)
*     8x8 matrices for Strassen-AlphaTensor testing
      DOUBLE PRECISION A8(LDIM8,LDIM8), B8(LDIM8,LDIM8)
      DOUBLE PRECISION C8_ALPHA(LDIM8,LDIM8), C8_STANDARD(LDIM8,LDIM8)
*     16x16 matrices for Block-wise AlphaTensor testing
      DOUBLE PRECISION A16(LDIM16,LDIM16), B16(LDIM16,LDIM16)
      DOUBLE PRECISION C16A(LDIM16,LDIM16), C16S(LDIM16,LDIM16)
*     20x20 matrices for Block-wise AlphaTensor (non-power-of-2)
      DOUBLE PRECISION A20(LDIM20,LDIM20), B20(LDIM20,LDIM20)
      DOUBLE PRECISION C20A(LDIM20,LDIM20), C20S(LDIM20,LDIM20)
*
*     .. External Subroutines ..
      EXTERNAL DGEMM_ALPHA, DGEMM
*
*     Calculate LAPACK-standard tolerance at runtime
      EPS_MACHINE = DLAMCH('Epsilon')
*
      WRITE(*,*) '==============================================='
      WRITE(*,*) 'PHASE 8.6: COMPREHENSIVE MULTI-ALGORITHM TEST'
      WRITE(*,*) '==============================================='
      WRITE(*,*) '4x4: Direct AlphaTensor (49 operations)'
      WRITE(*,*) '8x8: Strassen-AlphaTensor (343 operations)'
      WRITE(*,*) '16x16: Block-wise AlphaTensor'
      WRITE(*,*) '20x20: Block-wise AlphaTensor (non-power-of-2)'
      WRITE(*,*) '==============================================='
      WRITE(*,'(A,E12.5)') ' Machine Epsilon: ',EPS_MACHINE
      WRITE(*,'(A,E12.5)') ' 4x4 Tolerance: ',TOLERANCE_4X4
      WRITE(*,'(A,E12.5)') ' 8x8 Tolerance: ',TOLERANCE_8X8
      WRITE(*,'(A,E12.5)') ' Block Tolerance: ',TOLERANCE_BLOCK
      WRITE(*,*)
*
      TEST_COUNT = 0
      PASS_COUNT = 0
      ALL_TESTS_PASSED = .TRUE.
      MAX_ERROR = ZERO
*
*     ===========================================
*     TEST 1: 4x4 Direct AlphaTensor - Identity matrices
*     ===========================================
      TEST_COUNT = TEST_COUNT + 1
      WRITE(*,*) 'TEST 1: 4x4 Direct AlphaTensor - Identity matrices'
*
*     Initialize matrices
      DO I = 1, LDIM4
          DO J = 1, LDIM4
              IF (I .EQ. J) THEN
                  A4(I,J) = ONE
                  B4(I,J) = ONE
              ELSE
                  A4(I,J) = ZERO
                  B4(I,J) = ZERO
              END IF
              C4_ALPHA(I,J) = ZERO
              C4_STANDARD(I,J) = ZERO
          END DO
      END DO
*
      ALPHA = ONE
      BETA = ZERO
*
*     Test AlphaTensor algorithm
      CALL DGEMM_ALPHA('N','N',LDIM4,LDIM4,LDIM4,ALPHA,A4,LDIM4,
     +                 B4,LDIM4,BETA,C4_ALPHA,LDIM4)
*
*     Test standard DGEMM
      CALL DGEMM('N','N',LDIM4,LDIM4,LDIM4,ALPHA,A4,LDIM4,B4,LDIM4,
     +           BETA,C4_STANDARD,LDIM4)
*
*     Compare results
      CURRENT_ERROR = ZERO
      DO I = 1, LDIM4
          DO J = 1, LDIM4
              CURRENT_ERROR = MAX(CURRENT_ERROR,
     +                       ABS(C4_ALPHA(I,J) - C4_STANDARD(I,J)))
          END DO
      END DO
*
      IF (CURRENT_ERROR .LT. TOLERANCE_4X4) THEN
          WRITE(*,*) '   PASSED - Max error:', CURRENT_ERROR
          PASS_COUNT = PASS_COUNT + 1
      ELSE
          WRITE(*,*) '   FAILED - Max error:', CURRENT_ERROR
          ALL_TESTS_PASSED = .FALSE.
      END IF
      MAX_ERROR = MAX(MAX_ERROR, CURRENT_ERROR)
*
*     ===========================================
*     TEST 2: 4x4 Direct AlphaTensor - Random matrices
*     ===========================================
      TEST_COUNT = TEST_COUNT + 1
      WRITE(*,*) 'TEST 2: 4x4 Direct AlphaTensor - Random matrices'
*
*     Initialize with pseudo-random values
      DO I = 1, LDIM4
          DO J = 1, LDIM4
              A4(I,J) = DBLE(I+J) / 10.0D+0
              B4(I,J) = DBLE(I*J) / 5.0D+0
              C4_ALPHA(I,J) = DBLE(I-J) / 3.0D+0
              C4_STANDARD(I,J) = C4_ALPHA(I,J)
          END DO
      END DO
*
      ALPHA = TWO
      BETA = ONE
*
*     Test AlphaTensor algorithm
      CALL DGEMM_ALPHA('N','N',LDIM4,LDIM4,LDIM4,ALPHA,A4,LDIM4,
     +                 B4,LDIM4,BETA,C4_ALPHA,LDIM4)
*
*     Test standard DGEMM
      CALL DGEMM('N','N',LDIM4,LDIM4,LDIM4,ALPHA,A4,LDIM4,B4,LDIM4,
     +           BETA,C4_STANDARD,LDIM4)
*
*     Compare results
      CURRENT_ERROR = ZERO
      DO I = 1, LDIM4
          DO J = 1, LDIM4
              CURRENT_ERROR = MAX(CURRENT_ERROR,
     +                       ABS(C4_ALPHA(I,J) - C4_STANDARD(I,J)))
          END DO
      END DO
*
      IF (CURRENT_ERROR .LT. TOLERANCE_4X4) THEN
          WRITE(*,*) '   PASSED - Max error:', CURRENT_ERROR
          PASS_COUNT = PASS_COUNT + 1
      ELSE
          WRITE(*,*) '   FAILED - Max error:', CURRENT_ERROR
          ALL_TESTS_PASSED = .FALSE.
      END IF
      MAX_ERROR = MAX(MAX_ERROR, CURRENT_ERROR)
*
*     ===========================================
*     TEST 3: 4x4 Direct AlphaTensor - Edge case ALPHA=0
*     ===========================================
      TEST_COUNT = TEST_COUNT + 1
      WRITE(*,*) 'TEST 3: 4x4 Direct AlphaTensor - Edge case ALPHA=0'
*
      DO I = 1, LDIM4
          DO J = 1, LDIM4
              A4(I,J) = DBLE(I*J)
              B4(I,J) = DBLE(I+J)
              C4_ALPHA(I,J) = DBLE(I) / 2.0D+0
              C4_STANDARD(I,J) = C4_ALPHA(I,J)
          END DO
      END DO
*
      ALPHA = ZERO
      BETA = TWO
*
*     Test AlphaTensor algorithm
      CALL DGEMM_ALPHA('N','N',LDIM4,LDIM4,LDIM4,ALPHA,A4,LDIM4,
     +                 B4,LDIM4,BETA,C4_ALPHA,LDIM4)
*
*     Test standard DGEMM
      CALL DGEMM('N','N',LDIM4,LDIM4,LDIM4,ALPHA,A4,LDIM4,B4,LDIM4,
     +           BETA,C4_STANDARD,LDIM4)
*
*     Compare results
      CURRENT_ERROR = ZERO
      DO I = 1, LDIM4
          DO J = 1, LDIM4
              CURRENT_ERROR = MAX(CURRENT_ERROR,
     +                       ABS(C4_ALPHA(I,J) - C4_STANDARD(I,J)))
          END DO
      END DO
*
      IF (CURRENT_ERROR .LT. TOLERANCE_4X4) THEN
          WRITE(*,*) '   PASSED - Max error:', CURRENT_ERROR
          PASS_COUNT = PASS_COUNT + 1
      ELSE
          WRITE(*,*) '   FAILED - Max error:', CURRENT_ERROR
          ALL_TESTS_PASSED = .FALSE.
      END IF
      MAX_ERROR = MAX(MAX_ERROR, CURRENT_ERROR)
*
*     ===========================================
*     TEST 4: 4x4 Direct AlphaTensor - Complex coefficients
*     ===========================================
      TEST_COUNT = TEST_COUNT + 1
      WRITE(*,*) 'TEST 4: 4x4 Direct AlphaTensor - Complex coefficients'
*
      DO I = 1, LDIM4
          DO J = 1, LDIM4
              A4(I,J) = DBLE(I*I + J) / 4.0D+0
              B4(I,J) = DBLE(I + J*J) / 6.0D+0
              C4_ALPHA(I,J) = DBLE(I*J) / 8.0D+0
              C4_STANDARD(I,J) = C4_ALPHA(I,J)
          END DO
      END DO
*
      ALPHA = 1.5D+0
      BETA = 0.75D+0
*
*     Test AlphaTensor algorithm
      CALL DGEMM_ALPHA('N','N',LDIM4,LDIM4,LDIM4,ALPHA,A4,LDIM4,
     +                 B4,LDIM4,BETA,C4_ALPHA,LDIM4)
*
*     Test standard DGEMM
      CALL DGEMM('N','N',LDIM4,LDIM4,LDIM4,ALPHA,A4,LDIM4,B4,LDIM4,
     +           BETA,C4_STANDARD,LDIM4)
*
*     Compare results
      CURRENT_ERROR = ZERO
      DO I = 1, LDIM4
          DO J = 1, LDIM4
              CURRENT_ERROR = MAX(CURRENT_ERROR,
     +                       ABS(C4_ALPHA(I,J) - C4_STANDARD(I,J)))
          END DO
      END DO
*
      IF (CURRENT_ERROR .LT. TOLERANCE_4X4) THEN
          WRITE(*,*) '   PASSED - Max error:', CURRENT_ERROR
          PASS_COUNT = PASS_COUNT + 1
      ELSE
          WRITE(*,*) '   FAILED - Max error:', CURRENT_ERROR
          ALL_TESTS_PASSED = .FALSE.
      END IF
      MAX_ERROR = MAX(MAX_ERROR, CURRENT_ERROR)

*     ===========================================
*     TEST 5: 8x8 Strassen-AlphaTensor Hybrid
*     ===========================================
      TEST_COUNT = TEST_COUNT + 1
      WRITE(*,*) 'TEST 5: 8x8 Strassen-AlphaTensor Hybrid'
*
      DO I = 1, LDIM8
          DO J = 1, LDIM8
              A8(I,J) = DBLE(MOD(I*J + 7, 13) - 6) * 0.8D0
              B8(I,J) = DBLE(MOD(I + J*2 + 5, 11) - 5) * 0.6D0
              C8_ALPHA(I,J) = DBLE(I*2 - J + 3) * 0.3D0
              C8_STANDARD(I,J) = C8_ALPHA(I,J)
          END DO
      END DO
*
      ALPHA = 1.2D+0
      BETA = 0.8D+0
*
*     Test AlphaTensor algorithm
      CALL DGEMM_ALPHA('N','N',LDIM8,LDIM8,LDIM8,ALPHA,A8,LDIM8,
     +                 B8,LDIM8,BETA,C8_ALPHA,LDIM8)
*
*     Test standard DGEMM
      CALL DGEMM('N','N',LDIM8,LDIM8,LDIM8,ALPHA,A8,LDIM8,B8,LDIM8,
     +           BETA,C8_STANDARD,LDIM8)
*
*     Compare results
      CURRENT_ERROR = ZERO
      DO I = 1, LDIM8
          DO J = 1, LDIM8
              CURRENT_ERROR = MAX(CURRENT_ERROR,
     +                       ABS(C8_ALPHA(I,J) - C8_STANDARD(I,J)))
          END DO
      END DO
*
      IF (CURRENT_ERROR .LT. TOLERANCE_8X8) THEN
          WRITE(*,*) '   PASSED - Max error:', CURRENT_ERROR
          PASS_COUNT = PASS_COUNT + 1
      ELSE
          WRITE(*,*) '   FAILED - Max error:', CURRENT_ERROR
          ALL_TESTS_PASSED = .FALSE.
      END IF
      MAX_ERROR = MAX(MAX_ERROR, CURRENT_ERROR)

*     ===========================================
*     TEST 6: 16x16 Block-wise AlphaTensor
*     ===========================================
      TEST_COUNT = TEST_COUNT + 1
      WRITE(*,*) 'TEST 6: 16x16 Block-wise AlphaTensor'
*
      DO I = 1, LDIM16
          DO J = 1, LDIM16
              A16(I,J) = DBLE(I + J*2) / 10.0D0
              B16(I,J) = DBLE(I*2 + J) / 15.0D0
              C16A(I,J) = DBLE(I - J + 5) / 8.0D0
              C16S(I,J) = C16A(I,J)
          END DO
      END DO
*
      ALPHA = 1.3D+0
      BETA = 0.7D+0
*
*     Test AlphaTensor algorithm
      CALL DGEMM_ALPHA('N','N',LDIM16,LDIM16,LDIM16,ALPHA,A16,LDIM16,
     +                 B16,LDIM16,BETA,C16A,LDIM16)
*
*     Test standard DGEMM
      CALL DGEMM('N','N',LDIM16,LDIM16,LDIM16,ALPHA,A16,LDIM16,
     +           B16,LDIM16,BETA,C16S,LDIM16)
*
*     Compare results
      CURRENT_ERROR = ZERO
      DO I = 1, LDIM16
          DO J = 1, LDIM16
              CURRENT_ERROR = MAX(CURRENT_ERROR,
     +                       ABS(C16A(I,J) - C16S(I,J)))
          END DO
      END DO
*
      IF (CURRENT_ERROR .LT. TOLERANCE_BLOCK) THEN
          WRITE(*,*) '   PASSED - Max error:', CURRENT_ERROR
          PASS_COUNT = PASS_COUNT + 1
      ELSE
          WRITE(*,*) '   FAILED - Max error:', CURRENT_ERROR
          ALL_TESTS_PASSED = .FALSE.
      END IF
      MAX_ERROR = MAX(MAX_ERROR, CURRENT_ERROR)

*     ===========================================
*     TEST 7: 20x20 Block-wise AlphaTensor (non-power-of-2)
*     ===========================================
      TEST_COUNT = TEST_COUNT + 1
      WRITE(*,*) 'TEST 7: 20x20 Block-wise AlphaTensor (non-power-of-2)'
*
      DO I = 1, LDIM20
          DO J = 1, LDIM20
              A20(I,J) = DBLE(MOD(I*3 + J, 17) - 8) * 0.4D0
              B20(I,J) = DBLE(MOD(I + J*4, 19) - 9) * 0.7D0
              C20A(I,J) = DBLE(I + J - 10) * 0.2D0
              C20S(I,J) = C20A(I,J)
          END DO
      END DO
*
      ALPHA = 1.4D+0
      BETA = 0.6D+0
*
*     Test AlphaTensor algorithm
      CALL DGEMM_ALPHA('N','N',LDIM20,LDIM20,LDIM20,ALPHA,A20,LDIM20,
     +                 B20,LDIM20,BETA,C20A,LDIM20)
*
*     Test standard DGEMM
      CALL DGEMM('N','N',LDIM20,LDIM20,LDIM20,ALPHA,A20,LDIM20,
     +           B20,LDIM20,BETA,C20S,LDIM20)
*
*     Compare results
      CURRENT_ERROR = ZERO
      DO I = 1, LDIM20
          DO J = 1, LDIM20
              CURRENT_ERROR = MAX(CURRENT_ERROR,
     +                       ABS(C20A(I,J) - C20S(I,J)))
          END DO
      END DO
*
      IF (CURRENT_ERROR .LT. TOLERANCE_BLOCK) THEN
          WRITE(*,*) '   PASSED - Max error:', CURRENT_ERROR
          PASS_COUNT = PASS_COUNT + 1
      ELSE
          WRITE(*,*) '   FAILED - Max error:', CURRENT_ERROR
          ALL_TESTS_PASSED = .FALSE.
      END IF
      MAX_ERROR = MAX(MAX_ERROR, CURRENT_ERROR)
*
*     ===========================================
*     FINAL RESULTS
*     ===========================================
      WRITE(*,*)
      WRITE(*,*) '==============================================='
      WRITE(*,*) 'PHASE 8.6: COMPREHENSIVE TEST RESULTS'
      WRITE(*,*) '==============================================='
      WRITE(*,*) 'Tests Passed:', PASS_COUNT, '/', TEST_COUNT
      WRITE(*,*) 'Maximum Error:', MAX_ERROR
      WRITE(*,*) 'Tolerances: 4x4=', TOLERANCE_4X4
      WRITE(*,*) '8x8=', TOLERANCE_8X8, ' Block=', TOLERANCE_BLOCK
*
      IF (ALL_TESTS_PASSED) THEN
          WRITE(*,*) 'ALL TESTS PASSED!'
          WRITE(*,*) 'Phase 8.6 multi-algorithm suite is CORRECT!'
          WRITE(*,*) '4x4: Direct AlphaTensor working'
          WRITE(*,*) '8x8: Strassen-AlphaTensor Hybrid working'
          WRITE(*,*) '16x16: Block-wise AlphaTensor working'
          WRITE(*,*) '20x20: Block-wise AlphaTensor working'
      ELSE
          WRITE(*,*) 'SOME TESTS FAILED!'
          WRITE(*,*) 'Algorithm needs further debugging.'
      END IF
*
      WRITE(*,*) '==============================================='
*
      END
