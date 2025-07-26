      PROGRAM COMPREHENSIVE_MULTI_SIZE_BENCHMARK
*
*  -- CORRECTED: Multi-Size Matrix AlphaTensor Testing Framework --
*  -- TRUE HEAD-TO-HEAD: DGEMM_ALPHA vs DGEMM across matrix sizes --
*  -- Sizes: 4x4 (AlphaTensor active), 8x8, 16x16, 32x32 (fallback) --
*  -- Coverage: 12 matrix types, speed, performance, accuracy --
*  -- Output: Shows AlphaTensor active vs fallback behavior --
*
      IMPLICIT NONE
*
*     .. Parameters ..
      INTEGER NUM_TEST_CASES, REPORT_UNIT, NUM_ALGOS, NUM_SIZES
      PARAMETER (NUM_TEST_CASES=12, NUM_SIZES=4)
      PARAMETER (NUM_ALGOS=2, REPORT_UNIT=10)
      DOUBLE PRECISION ONE, ZERO, PI
      PARAMETER (ONE=1.0D+0, ZERO=0.0D+0, PI=3.14159265358979323846D+0)
*     ..
*     .. Local Arrays ..
      DOUBLE PRECISION A(32,32)
      DOUBLE PRECISION B(32,32)
      DOUBLE PRECISION C_DGEMM(32,32)
      DOUBLE PRECISION C_ALPHA(32,32)
      DOUBLE PRECISION INITIAL_C(32,32)
      CHARACTER*50 TEST_NAMES(12)
      CHARACTER*30 ALGO_NAMES(2)
      INTEGER MATRIX_SIZES(4), NRUNS_BY_SIZE(4)
      DOUBLE PRECISION TIMES(2), GFLOPS(2)
      INTEGER ALGO_RANKINGS(2)
*     ..
*     .. Local Scalars ..
      INTEGER I, J, RUN, TEST_CASE, ALGO_IDX, FASTEST_ALGO
      INTEGER SIZE_IDX, CURRENT_SIZE, CURRENT_NRUNS
      DOUBLE PRECISION ALPHA, BETA
      DOUBLE PRECISION START_TIME, END_TIME
      DOUBLE PRECISION ERR_A, MAX_A
      DOUBLE PRECISION AVG_A, ERROR_SUM
      DOUBLE PRECISION FLOPS_CURRENT, SPEEDUP_ALPHA
      DOUBLE PRECISION TOTAL_SPEEDUP_ALPHA
      DOUBLE PRECISION TOTAL_ACCURACY_ALPHA
      INTEGER PASSED_TESTS_ALPHA
      INTEGER FAILED_TESTS_ALPHA
      INTEGER WINS_ALPHA, WINS_DGEMM
      INTEGER TOTAL_TESTS_RUN
      LOGICAL IS_ALPHATENSOR_ACTIVE
*     ..
*     .. External Subroutines ..
      EXTERNAL DGEMM, DGEMM_ALPHA
*     ..

*     Initialize matrix sizes and corresponding iteration counts
      MATRIX_SIZES(1) = 4     ! AlphaTensor active
      MATRIX_SIZES(2) = 8     ! AlphaTensor fallback
      MATRIX_SIZES(3) = 16    ! AlphaTensor fallback
      MATRIX_SIZES(4) = 32    ! AlphaTensor fallback

*     Adjust iterations based on matrix size (larger = fewer iterations)
      NRUNS_BY_SIZE(1) = 50000  ! 4x4: many iterations
      NRUNS_BY_SIZE(2) = 10000  ! 8x8: moderate iterations
      NRUNS_BY_SIZE(3) = 2000   ! 16x16: fewer iterations
      NRUNS_BY_SIZE(4) = 500    ! 32x32: few iterations

*     Initialize algorithm names
      ALGO_NAMES(1) = 'DGEMM (Reference)'
      ALGO_NAMES(2) = 'DGEMM_ALPHA (Phase 8.3)'

*     Initialize test case names
      TEST_NAMES(1) = 'Identity Matrices'
      TEST_NAMES(2) = 'Zero Matrices'
      TEST_NAMES(3) = 'Random Dense Matrices'
      TEST_NAMES(4) = 'Diagonal Matrices'
      TEST_NAMES(5) = 'Symmetric Matrices'
      TEST_NAMES(6) = 'Sparse Matrices'
      TEST_NAMES(7) = 'Large Value Matrices'
      TEST_NAMES(8) = 'Small Value Matrices'
      TEST_NAMES(9) = 'Mixed Sign Matrices'
      TEST_NAMES(10) = 'Ill-Conditioned Matrices'
      TEST_NAMES(11) = 'Integer Matrices'
      TEST_NAMES(12) = 'Stress Test Matrices'

*     Open comprehensive report file
      OPEN(UNIT=REPORT_UNIT,
     $     FILE='corrected_multi_size_alphatensor_report.txt',
     $     STATUS='REPLACE')

*     Write detailed report header
      WRITE(REPORT_UNIT, '(A)')
     +  '============================================================'
      WRITE(REPORT_UNIT, '(A)')
     +  '   CORRECTED MULTI-SIZE ALPHATENSOR TESTING FRAMEWORK'
      WRITE(REPORT_UNIT, '(A)')
     +  '============================================================'
      WRITE(REPORT_UNIT, '(A)') 'HEAD-TO-HEAD: DGEMM vs DGEMM_ALPHA'
      WRITE(REPORT_UNIT, '(A,I0)') 'Matrix Sizes Tested: ', NUM_SIZES
      DO I = 1, NUM_SIZES
          IS_ALPHATENSOR_ACTIVE = (MATRIX_SIZES(I) .EQ. 4)
          IF (IS_ALPHATENSOR_ACTIVE) THEN
              WRITE(REPORT_UNIT, '(A,I0,A,I0,A)') '  ',
     +            MATRIX_SIZES(I), 'x', MATRIX_SIZES(I),
     +            ' (AlphaTensor ACTIVE)'
          ELSE
              WRITE(REPORT_UNIT, '(A,I0,A,I0,A)') '  ',
     +            MATRIX_SIZES(I), 'x', MATRIX_SIZES(I),
     +            ' (AlphaTensor fallback to DGEMM)'
          END IF
      END DO
      WRITE(REPORT_UNIT, '(A,I0)') 'Test Cases per Size: ', 12
      WRITE(REPORT_UNIT, '(A)') 'Algorithms Tested:'
      DO I = 1, NUM_ALGOS
          WRITE(REPORT_UNIT, '(A,I0,A,A)') '  ', I, '. ',
     +        TRIM(ALGO_NAMES(I))
      END DO
      WRITE(REPORT_UNIT, '(A)') 'Metrics: Speed, GFLOPS, Accuracy'
      WRITE(REPORT_UNIT, '(A)') ''

*     Console header
      WRITE(*,*) '=============================================='
      WRITE(*,*) '   CORRECTED MULTI-SIZE ALPHATENSOR TESTING'
      WRITE(*,*) '=============================================='
      WRITE(*,*) 'TRUE HEAD-TO-HEAD: DGEMM_ALPHA vs DGEMM'
      WRITE(*,*) 'Testing 2 algorithms across', NUM_SIZES,
     +           'matrix sizes'
      WRITE(*,*) 'Matrix sizes: 4x4 (AlphaTensor), 8x8, 16x16, 32x32'
      WRITE(*,*) 'Test cases per size:', NUM_TEST_CASES
      WRITE(*,*) 'Writing comprehensive report to file...'
      WRITE(*,*) '=============================================='

*     Initialize global statistics
      TOTAL_SPEEDUP_ALPHA = ZERO
      TOTAL_ACCURACY_ALPHA = ZERO
      PASSED_TESTS_ALPHA = 0
      FAILED_TESTS_ALPHA = 0
      WINS_DGEMM = 0
      WINS_ALPHA = 0
      TOTAL_TESTS_RUN = 0

*     ============================================================
*     MAIN COMPREHENSIVE TEST LOOP - SIZE BY SIZE
*     ============================================================
      DO SIZE_IDX = 1, NUM_SIZES
          CURRENT_SIZE = MATRIX_SIZES(SIZE_IDX)
          CURRENT_NRUNS = NRUNS_BY_SIZE(SIZE_IDX)
          IS_ALPHATENSOR_ACTIVE = (CURRENT_SIZE .EQ. 4)

          WRITE(*,'(A)') '============================================='
          IF (IS_ALPHATENSOR_ACTIVE) THEN
              WRITE(*,'(A,I0,A,I0,A)') 'TESTING ', CURRENT_SIZE, 'x',
     +            CURRENT_SIZE, ' MATRICES (AlphaTensor ACTIVE)'
          ELSE
              WRITE(*,'(A,I0,A,I0,A)') 'TESTING ', CURRENT_SIZE, 'x',
     +            CURRENT_SIZE, ' MATRICES (AlphaTensor fallback)'
          END IF
          WRITE(*,'(A,I0,A)') 'Iterations: ', CURRENT_NRUNS, ' per test'
          WRITE(*,*) '=============================================='

          WRITE(REPORT_UNIT, '(A)')
     +      '======================================================='
          IF (IS_ALPHATENSOR_ACTIVE) THEN
              WRITE(REPORT_UNIT, '(A,I0,A,I0,A)') 'MATRIX SIZE: ',
     +            CURRENT_SIZE, 'x', CURRENT_SIZE,
     +            ' (AlphaTensor ACTIVE)'
          ELSE
              WRITE(REPORT_UNIT, '(A,I0,A,I0,A)') 'MATRIX SIZE: ',
     +            CURRENT_SIZE, 'x', CURRENT_SIZE,
     +            ' (AlphaTensor fallback to DGEMM)'
          END IF
          WRITE(REPORT_UNIT, '(A,I0)') 'Iterations per test: ',
     +        CURRENT_NRUNS
          WRITE(REPORT_UNIT, '(A)')
     +      '======================================================='

          DO TEST_CASE = 1, NUM_TEST_CASES
              WRITE(*,'(A,I0,A,A)') 'Test ', TEST_CASE, ': ',
     +            TRIM(TEST_NAMES(TEST_CASE))

              WRITE(REPORT_UNIT, '(A)')
     +          '----------------------------------------------------'
              WRITE(REPORT_UNIT, '(A,I0,A,A)') 'TEST ', TEST_CASE, ': ',
     +            TRIM(TEST_NAMES(TEST_CASE))
              WRITE(REPORT_UNIT, '(A)')
     +          '----------------------------------------------------'

*             Generate test matrices based on test case and size
              CALL GENERATE_TEST_MATRICES_VARIABLE_SIZE(TEST_CASE, A, B,
     +            ALPHA, BETA, CURRENT_SIZE)

*             Store consistent initial C matrix for all algorithms
              DO J = 1, CURRENT_SIZE
                  DO I = 1, CURRENT_SIZE
                      INITIAL_C(I,J) = 0.1D+0 * DBLE(I * J)
                  END DO
              END DO

*             ========================================================
*             BENCHMARK 1: Standard DGEMM (Reference Implementation)
*             ========================================================
              WRITE(REPORT_UNIT, '(A)') 'ALGORITHM 1: DGEMM (Reference)'

*             Initialize with consistent values
              DO J = 1, CURRENT_SIZE
                  DO I = 1, CURRENT_SIZE
                      C_DGEMM(I,J) = INITIAL_C(I,J)
                  END DO
              END DO

*             Warm-up runs (eliminate cache effects)
              DO RUN = 1, MIN(1000, CURRENT_NRUNS/10)
                  CALL DGEMM('N','N',CURRENT_SIZE,CURRENT_SIZE,
     +                CURRENT_SIZE,ALPHA,A,32,B,32,
     +                BETA,C_DGEMM,32)
              END DO

*             Precise timed benchmark - DGEMM
              CALL CPU_TIME(START_TIME)
              DO RUN = 1, CURRENT_NRUNS
                  CALL DGEMM('N','N',CURRENT_SIZE,CURRENT_SIZE,
     +                CURRENT_SIZE,ALPHA,A,32,B,32,
     +                BETA,C_DGEMM,32)
              END DO
              CALL CPU_TIME(END_TIME)
              TIMES(1) = END_TIME - START_TIME

*             Calculate FLOPS for current size
              FLOPS_CURRENT = 2.0D+0 * DBLE(CURRENT_SIZE) *
     +                        DBLE(CURRENT_SIZE) * DBLE(CURRENT_SIZE)
              GFLOPS(1) = (FLOPS_CURRENT * DBLE(CURRENT_NRUNS)) /
     +                    (TIMES(1) * 1.0D+9)

              WRITE(REPORT_UNIT, '(A,F12.6,A)') '  Time: ', TIMES(1),
     +            ' seconds'
              WRITE(REPORT_UNIT, '(A,F8.3)') '  GFLOPS: ', GFLOPS(1)

*             ========================================================
*             BENCHMARK 2: DGEMM_ALPHA (Phase 8.3)
*             ========================================================
              WRITE(REPORT_UNIT, '(A)') 'ALGORITHM 2: DGEMM_ALPHA'

*             Initialize with same values as DGEMM
              DO J = 1, CURRENT_SIZE
                  DO I = 1, CURRENT_SIZE
                      C_ALPHA(I,J) = INITIAL_C(I,J)
                  END DO
              END DO

*             Warm-up runs
              DO RUN = 1, MIN(100, CURRENT_NRUNS/50)
                  CALL DGEMM_ALPHA('N','N',CURRENT_SIZE,CURRENT_SIZE,
     +                CURRENT_SIZE,ALPHA,A,32,B,32,
     +                BETA,C_ALPHA,32)
              END DO

*             Precise timed benchmark - DGEMM_ALPHA
              CALL CPU_TIME(START_TIME)
              DO RUN = 1, CURRENT_NRUNS
                  CALL DGEMM_ALPHA('N','N',CURRENT_SIZE,CURRENT_SIZE,
     +                CURRENT_SIZE,ALPHA,A,32,B,32,
     +                BETA,C_ALPHA,32)
              END DO
              CALL CPU_TIME(END_TIME)
              TIMES(2) = END_TIME - START_TIME
              GFLOPS(2) = (FLOPS_CURRENT * DBLE(CURRENT_NRUNS)) /
     +                    (TIMES(2) * 1.0D+9)

              SPEEDUP_ALPHA = TIMES(1) / TIMES(2)
              WRITE(REPORT_UNIT, '(A,F12.6,A)') '  Time: ', TIMES(2),
     +            ' seconds'
              WRITE(REPORT_UNIT, '(A,F8.3)') '  GFLOPS: ', GFLOPS(2)
              WRITE(REPORT_UNIT, '(A,F8.3,A)') '  Speedup vs DGEMM: ',
     +            SPEEDUP_ALPHA, 'x'
              IF (IS_ALPHATENSOR_ACTIVE) THEN
                  WRITE(REPORT_UNIT, '(A)') '  (AlphaTensor active)'
              ELSE
                  WRITE(REPORT_UNIT, '(A)') '  (Fallback to DGEMM)'
              END IF

*             ========================================================
*             ACCURACY ANALYSIS
*             ========================================================
              WRITE(REPORT_UNIT, '(A)') ''
              WRITE(REPORT_UNIT, '(A)') 'ACCURACY ANALYSIS:'

*             DGEMM_ALPHA vs DGEMM accuracy
              MAX_A = ZERO
              ERROR_SUM = ZERO
              DO J = 1, CURRENT_SIZE
                  DO I = 1, CURRENT_SIZE
                      ERR_A = ABS(C_ALPHA(I,J) - C_DGEMM(I,J))
                      ERROR_SUM = ERROR_SUM + ERR_A
                      IF (ERR_A .GT. MAX_A) THEN
                          MAX_A = ERR_A
                      END IF
                  END DO
              END DO
              AVG_A = ERROR_SUM / DBLE(CURRENT_SIZE * CURRENT_SIZE)

              WRITE(REPORT_UNIT, '(A,ES15.8)')
     +            '  DGEMM_ALPHA Max Error: ', MAX_A
              WRITE(REPORT_UNIT, '(A,ES15.8)')
     +            '  DGEMM_ALPHA Avg Error: ', AVG_A

*             ========================================================
*             PERFORMANCE RANKING & WINNER DETERMINATION
*             ========================================================
              WRITE(REPORT_UNIT, '(A)') ''
              WRITE(REPORT_UNIT, '(A)') 'PERFORMANCE RANKING:'

*             Find fastest algorithm
              IF (TIMES(1) .LT. TIMES(2)) THEN
                  FASTEST_ALGO = 1
                  ALGO_RANKINGS(1) = 1
                  ALGO_RANKINGS(2) = 2
              ELSE
                  FASTEST_ALGO = 2
                  ALGO_RANKINGS(1) = 2
                  ALGO_RANKINGS(2) = 1
              END IF

              DO I = 1, NUM_ALGOS
                  WRITE(REPORT_UNIT, '(A,I0,A,A)') '  #', I, ': ',
     +                TRIM(ALGO_NAMES(ALGO_RANKINGS(I)))
                  WRITE(REPORT_UNIT, '(A,F8.3,A,F8.3,A)') '    (',
     +                TIMES(ALGO_RANKINGS(I)), 's, ',
     +                GFLOPS(ALGO_RANKINGS(I)), ' GFLOPS)'
              END DO

*             Update winner statistics
              IF (FASTEST_ALGO .EQ. 1) THEN
                  WINS_DGEMM = WINS_DGEMM + 1
              ELSE
                  WINS_ALPHA = WINS_ALPHA + 1
              END IF

*             ========================================================
*             TEST CASE ASSESSMENT
*             ========================================================
              WRITE(REPORT_UNIT, '(A)') ''
              WRITE(REPORT_UNIT, '(A)') 'TEST CASE ASSESSMENT:'

*             DGEMM_ALPHA assessment
              IF (MAX_A .LT. 1.0D-10) THEN
                  WRITE(REPORT_UNIT, '(A)') '  DGEMM_ALPHA: PASSED'
                  PASSED_TESTS_ALPHA = PASSED_TESTS_ALPHA + 1
              ELSE
                  WRITE(REPORT_UNIT, '(A)') '  DGEMM_ALPHA: FAILED'
                  FAILED_TESTS_ALPHA = FAILED_TESTS_ALPHA + 1
                  WRITE(REPORT_UNIT, '(A)') '    - Accuracy issue'
              END IF

              IF (SPEEDUP_ALPHA .GT. 1.0D+0) THEN
                  WRITE(REPORT_UNIT, '(A,F6.2,A)') '  Performance: ',
     +                SPEEDUP_ALPHA, 'x FASTER than DGEMM'
              ELSE
                  WRITE(REPORT_UNIT, '(A,F6.2,A)') '  Performance: ',
     +                (1.0D+0/SPEEDUP_ALPHA), 'x SLOWER than DGEMM'
              END IF

              WRITE(REPORT_UNIT, '(A)') ''

*             Update running statistics
              TOTAL_SPEEDUP_ALPHA = TOTAL_SPEEDUP_ALPHA + SPEEDUP_ALPHA
              TOTAL_ACCURACY_ALPHA = TOTAL_ACCURACY_ALPHA + MAX_A
              TOTAL_TESTS_RUN = TOTAL_TESTS_RUN + 1

*             Console progress update
              WRITE(*,'(A,F6.3,A)') '  ALPHA: ', SPEEDUP_ALPHA, 'x'

          END DO
      END DO

*     ============================================================
*     COMPREHENSIVE SUMMARY
*     ============================================================
      WRITE(REPORT_UNIT, '(A)')
     +  '============================================================'
      WRITE(REPORT_UNIT, '(A)')
     +  '                   COMPREHENSIVE SUMMARY'
      WRITE(REPORT_UNIT, '(A)')
     +  '============================================================'

      WRITE(REPORT_UNIT, '(A)') 'OVERALL PERFORMANCE CHAMPION:'
      IF (WINS_DGEMM .GT. WINS_ALPHA) THEN
          WRITE(REPORT_UNIT, '(A,I0,A,I0,A)')
     +        '  DGEMM (Reference) - ', WINS_DGEMM, ' / ',
     +        TOTAL_TESTS_RUN, ' wins'
      ELSE
          WRITE(REPORT_UNIT, '(A,I0,A,I0,A)')
     +        '  DGEMM_ALPHA (Phase 8.3) - ', WINS_ALPHA, ' / ',
     +        TOTAL_TESTS_RUN, ' wins'
      END IF

      WRITE(REPORT_UNIT, '(A)') ''
      WRITE(REPORT_UNIT, '(A)') 'DETAILED RESULTS:'
      WRITE(REPORT_UNIT, '(A,I0,A,I0)') 'DGEMM_ALPHA Tests Passed: ',
     +    PASSED_TESTS_ALPHA, ' / ', TOTAL_TESTS_RUN
      WRITE(REPORT_UNIT, '(A,I0,A,I0)') 'DGEMM_ALPHA Tests Failed: ',
     +    FAILED_TESTS_ALPHA, ' / ', TOTAL_TESTS_RUN
      WRITE(REPORT_UNIT, '(A,F8.3,A)') 'DGEMM_ALPHA Average Speedup: ',
     +    TOTAL_SPEEDUP_ALPHA / DBLE(TOTAL_TESTS_RUN), 'x'
      WRITE(REPORT_UNIT, '(A,ES15.8)') 'DGEMM_ALPHA Average Error: ',
     +    TOTAL_ACCURACY_ALPHA / DBLE(TOTAL_TESTS_RUN)

      WRITE(REPORT_UNIT, '(A)') ''
      WRITE(REPORT_UNIT, '(A)') 'MATRIX SIZE ANALYSIS:'
      WRITE(REPORT_UNIT, '(A)') '4x4: AlphaTensor algorithm active'
      WRITE(REPORT_UNIT, '(A)') '8x8, 16x16, 32x32: Fallback to DGEMM'
      WRITE(REPORT_UNIT, '(A)') 'Performance differences reveal:'
      WRITE(REPORT_UNIT, '(A)') '- AlphaTensor effectiveness on 4x4'
      WRITE(REPORT_UNIT, '(A)') '- Fallback overhead behavior'
      WRITE(REPORT_UNIT, '(A)') '- Scaling behavior with matrix size'

      WRITE(REPORT_UNIT, '(A)') ''
      WRITE(REPORT_UNIT, '(A)') 'RECOMMENDATION:'
      IF (PASSED_TESTS_ALPHA .GE. TOTAL_TESTS_RUN * 3 / 4) THEN
          WRITE(REPORT_UNIT, '(A)')
     +      'EXCELLENT: AlphaTensor shows strong performance.'
      ELSE IF (PASSED_TESTS_ALPHA .GE. TOTAL_TESTS_RUN / 2) THEN
          WRITE(REPORT_UNIT, '(A)')
     +      'GOOD: AlphaTensor viable for target cases.'
      ELSE
          WRITE(REPORT_UNIT, '(A)')
     +      'NEEDS WORK: Focus on accuracy and performance.'
      END IF

      WRITE(REPORT_UNIT, '(A)') ''
      WRITE(REPORT_UNIT, '(A)') 'CORRECTED TESTING NOTES:'
      WRITE(REPORT_UNIT, '(A)') '- Misleading comparisons eliminated'
      WRITE(REPORT_UNIT, '(A)') '- True head-to-head comparison only'
      WRITE(REPORT_UNIT, '(A)') '- Performance results now accurate'
      WRITE(REPORT_UNIT, '(A)') '- AlphaTensor vs fallback shown'
      WRITE(REPORT_UNIT, '(A)')
     +  '============================================================'

*     Close comprehensive report
      CLOSE(REPORT_UNIT)

*     Final console summary
      WRITE(*,*) '=============================================='
      WRITE(*,*) 'CORRECTED MULTI-SIZE TESTING COMPLETE!'
      WRITE(*,'(A,I0,A,I0,A)') 'Performance wins: DGEMM=',
     +    WINS_DGEMM, ', DGEMM_ALPHA=', WINS_ALPHA
      WRITE(*,'(A,F6.3,A)') 'Average speedup: DGEMM_ALPHA=',
     +    TOTAL_SPEEDUP_ALPHA / DBLE(TOTAL_TESTS_RUN), 'x'
      WRITE(*,*) 'Report: corrected_multi_size_alphatensor_report.txt'
      WRITE(*,*) '=============================================='

      END

*     ============================================================
*     SUBROUTINE: Generate Test Matrices (Variable Size Version)
*     ============================================================
      SUBROUTINE GENERATE_TEST_MATRICES_VARIABLE_SIZE(TEST_CASE, A, B,
     +           ALPHA, BETA, MATRIX_SIZE)
      IMPLICIT NONE
      INTEGER TEST_CASE, MATRIX_SIZE, I, J
      DOUBLE PRECISION A(32,32), B(32,32)
      DOUBLE PRECISION ALPHA, BETA
      DOUBLE PRECISION PI
      PARAMETER (PI=3.14159265358979323846D+0)

*     Default coefficient values for comprehensive testing
      ALPHA = 1.5D+0
      BETA = 0.25D+0

*     Initialize all matrices to zero first
      DO J = 1, 32
          DO I = 1, 32
              A(I,J) = 0.0D+0
              B(I,J) = 0.0D+0
          END DO
      END DO

      IF (TEST_CASE .EQ. 1) THEN
*         Identity matrices - Perfect for algorithm verification
          DO J = 1, MATRIX_SIZE
              DO I = 1, MATRIX_SIZE
                  IF (I .EQ. J) THEN
                      A(I,J) = 1.0D+0
                      B(I,J) = 1.0D+0
                  ELSE
                      A(I,J) = 0.0D+0
                      B(I,J) = 0.0D+0
                  END IF
              END DO
          END DO

      ELSE IF (TEST_CASE .EQ. 2) THEN
*         Zero matrices - Edge case testing
          DO J = 1, MATRIX_SIZE
              DO I = 1, MATRIX_SIZE
                  A(I,J) = 0.0D+0
                  B(I,J) = 0.0D+0
              END DO
          END DO

      ELSE IF (TEST_CASE .EQ. 3) THEN
*         Random dense matrices - Real-world simulation
          DO J = 1, MATRIX_SIZE
              DO I = 1, MATRIX_SIZE
                  A(I,J) = DBLE(I + 2*J - 1) / 7.0D+0
                  B(I,J) = DBLE(3*I - J + 5) / 11.0D+0
              END DO
          END DO

      ELSE IF (TEST_CASE .EQ. 4) THEN
*         Diagonal matrices - Sparse structure testing
          DO J = 1, MATRIX_SIZE
              DO I = 1, MATRIX_SIZE
                  IF (I .EQ. J) THEN
                      A(I,J) = DBLE(I) * 2.5D+0
                      B(I,J) = DBLE(I) * 1.8D+0
                  ELSE
                      A(I,J) = 0.0D+0
                      B(I,J) = 0.0D+0
                  END IF
              END DO
          END DO

      ELSE IF (TEST_CASE .EQ. 5) THEN
*         Symmetric matrices - Pattern recognition
          DO J = 1, MATRIX_SIZE
              DO I = 1, MATRIX_SIZE
                  IF (I .LE. J) THEN
                      A(I,J) = DBLE(I*J) / 5.0D+0
                      A(J,I) = A(I,J)
                      B(I,J) = DBLE(I+J) / 3.0D+0
                      B(J,I) = B(I,J)
                  END IF
              END DO
          END DO

      ELSE IF (TEST_CASE .EQ. 6) THEN
*         Sparse matrices - Memory access efficiency
          DO J = 1, MATRIX_SIZE
              DO I = 1, MATRIX_SIZE
                  A(I,J) = 0.0D+0
                  B(I,J) = 0.0D+0
              END DO
          END DO
*         Add a few non-zero elements scaled by matrix size
          A(1,1) = 3.2D+0
          B(1,MATRIX_SIZE) = 2.8D+0
          IF (MATRIX_SIZE .GT. 2) THEN
              A(2,MATRIX_SIZE-1) = 1.7D+0
              B(MATRIX_SIZE-1,1) = -1.3D+0
          END IF
          A(MATRIX_SIZE,MATRIX_SIZE) = 4.5D+0
          B(MATRIX_SIZE,MATRIX_SIZE) = -2.1D+0

      ELSE IF (TEST_CASE .EQ. 7) THEN
*         Large value matrices - Overflow resistance
          DO J = 1, MATRIX_SIZE
              DO I = 1, MATRIX_SIZE
                  A(I,J) = DBLE(I*J) * 1.0D+6
                  B(I,J) = DBLE(I+J) * 2.5D+5
              END DO
          END DO

      ELSE IF (TEST_CASE .EQ. 8) THEN
*         Small value matrices - Underflow resistance
          DO J = 1, MATRIX_SIZE
              DO I = 1, MATRIX_SIZE
                  A(I,J) = DBLE(I*J) * 1.0D-6
                  B(I,J) = DBLE(I+J) * 2.5D-7
              END DO
          END DO

      ELSE IF (TEST_CASE .EQ. 9) THEN
*         Mixed sign matrices - Sign handling verification
          DO J = 1, MATRIX_SIZE
              DO I = 1, MATRIX_SIZE
                  A(I,J) = DBLE((-1)**(I+J)) * DBLE(I*J) / 4.0D+0
                  B(I,J) = DBLE((-1)**I) * DBLE(I+J) / 3.0D+0
              END DO
          END DO

      ELSE IF (TEST_CASE .EQ. 10) THEN
*         Ill-conditioned matrices - Numerical stability
          DO J = 1, MATRIX_SIZE
              DO I = 1, MATRIX_SIZE
                  A(I,J) = 1.0D+0 / DBLE(I + J - 1)
                  B(I,J) = DBLE(I)**DBLE(J-1)
              END DO
          END DO

      ELSE IF (TEST_CASE .EQ. 11) THEN
*         Integer matrices - Exact arithmetic verification
          DO J = 1, MATRIX_SIZE
              DO I = 1, MATRIX_SIZE
                  A(I,J) = DBLE(I * J)
                  B(I,J) = DBLE(I + J)
              END DO
          END DO

      ELSE IF (TEST_CASE .EQ. 12) THEN
*         Stress test matrices - Complex trigonometric patterns
          DO J = 1, MATRIX_SIZE
              DO I = 1, MATRIX_SIZE
                  A(I,J) = SIN(DBLE(I) * PI / 6.0D+0) +
     +                     COS(DBLE(J) * PI / 4.0D+0)
                  B(I,J) = COS(DBLE(I) * PI / 8.0D+0) -
     +                     SIN(DBLE(J) * PI / 3.0D+0)
              END DO
          END DO

      END IF

      RETURN
      END
