      PROGRAM COMPREHENSIVE_4X4_BENCHMARK
*
*  -- Ultimate 4x4 Matrix AlphaTensor Testing Framework --
*  -- Tests: All 3 algorithms (DGEMM, DGEMM_ALPHA, DGEMM_ALPHA_OPTIMIZED) --
*  -- Coverage: 12 matrix types, speed, performance, accuracy comparisons --
*  -- Output: Detailed report with algorithm rankings and recommendations --
*
      IMPLICIT NONE
*
*     .. Parameters ..
      INTEGER NRUNS, MATRIX_SIZE, NUM_TEST_CASES, REPORT_UNIT, NUM_ALGOS
      PARAMETER (NRUNS=50000, MATRIX_SIZE=4, NUM_TEST_CASES=12)
      PARAMETER (NUM_ALGOS=3, REPORT_UNIT=10)
      DOUBLE PRECISION ONE, ZERO, PI
      PARAMETER (ONE=1.0D+0, ZERO=0.0D+0, PI=3.14159265358979323846D+0)
*     ..
*     .. Local Arrays ..
      DOUBLE PRECISION A(MATRIX_SIZE,MATRIX_SIZE)
      DOUBLE PRECISION B(MATRIX_SIZE,MATRIX_SIZE)
      DOUBLE PRECISION C_DGEMM(MATRIX_SIZE,MATRIX_SIZE)
      DOUBLE PRECISION C_ALPHA(MATRIX_SIZE,MATRIX_SIZE)
      DOUBLE PRECISION C_OPT(MATRIX_SIZE,MATRIX_SIZE)
      DOUBLE PRECISION INITIAL_C(MATRIX_SIZE,MATRIX_SIZE)
      CHARACTER*50 TEST_NAMES(NUM_TEST_CASES)
      CHARACTER*30 ALGO_NAMES(NUM_ALGOS)
      DOUBLE PRECISION TIMES(NUM_ALGOS), GFLOPS(NUM_ALGOS)
      INTEGER ALGO_RANKINGS(NUM_ALGOS)
*     ..
*     .. Local Scalars ..
      INTEGER I, J, RUN, TEST_CASE, ALGO_IDX, FASTEST_ALGO
      DOUBLE PRECISION ALPHA, BETA
      DOUBLE PRECISION START_TIME, END_TIME
      DOUBLE PRECISION ERR_A, ERR_O, MAX_A, MAX_O
      DOUBLE PRECISION AVG_A, AVG_O, ERROR_SUM
      DOUBLE PRECISION FLOPS_4X4, SPEEDUP_ALPHA, SPEEDUP_OPT
      DOUBLE PRECISION TOTAL_SPEEDUP_ALPHA, TOTAL_SPEEDUP_OPT
      DOUBLE PRECISION TOTAL_ACCURACY_ALPHA, TOTAL_ACCURACY_OPT
      INTEGER PASSED_TESTS_ALPHA, PASSED_TESTS_OPT
      INTEGER FAILED_TESTS_ALPHA, FAILED_TESTS_OPT
      INTEGER WINS_ALPHA, WINS_OPT, WINS_DGEMM
*     ..
*     .. External Subroutines ..
      EXTERNAL DGEMM, DGEMM_ALPHA, DGEMM_ALPHA_OPTIMIZED
*     ..

*     Initialize algorithm names
      ALGO_NAMES(1) = 'DGEMM (Reference)'
      ALGO_NAMES(2) = 'DGEMM_ALPHA (AlphaTensor)'
      ALGO_NAMES(3) = 'DGEMM_ALPHA_OPTIMIZED'

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
     $     FILE='ultimate_4x4_alphatensor_report.txt',
     $     STATUS='REPLACE')

*     Write detailed report header
      WRITE(REPORT_UNIT, '(A)')
     +  '=============================================================='
      WRITE(REPORT_UNIT, '(A)')
     +  '    ULTIMATE 4x4 ALPHATENSOR TESTING FRAMEWORK'
      WRITE(REPORT_UNIT, '(A)')
     +  '=============================================================='
      WRITE(REPORT_UNIT, '(A,I0)') 'Total Test Cases: ', NUM_TEST_CASES
      WRITE(REPORT_UNIT, '(A,I0)') 'Iterations per test: ', NRUNS
      WRITE(REPORT_UNIT, '(A)') 'Algorithms Tested:'
      DO I = 1, NUM_ALGOS
          WRITE(REPORT_UNIT, '(A,I0,A,A)') '  ', I, '. ',
     +        TRIM(ALGO_NAMES(I))
      END DO
      WRITE(REPORT_UNIT, '(A)') 'Metrics: Speed, GFLOPS, Accuracy'
      WRITE(REPORT_UNIT, '(A)') ''

*     Console header
      WRITE(*,*) '=================================================='
      WRITE(*,*) '    ULTIMATE 4x4 ALPHATENSOR TESTING FRAMEWORK'
      WRITE(*,*) '=================================================='
      WRITE(*,*) 'Testing 3 algorithms across', NUM_TEST_CASES,
     +           'test cases'
      WRITE(*,*) 'Each test:', NRUNS, 'iterations for precise timing'
      WRITE(*,*) 'Writing comprehensive report to file...'
      WRITE(*,*) '=================================================='

*     Initialize global statistics
      FLOPS_4X4 = 2.0D+0 * MATRIX_SIZE * MATRIX_SIZE * MATRIX_SIZE
      TOTAL_SPEEDUP_ALPHA = ZERO
      TOTAL_SPEEDUP_OPT = ZERO
      TOTAL_ACCURACY_ALPHA = ZERO
      TOTAL_ACCURACY_OPT = ZERO
      PASSED_TESTS_ALPHA = 0
      PASSED_TESTS_OPT = 0
      FAILED_TESTS_ALPHA = 0
      FAILED_TESTS_OPT = 0
      WINS_DGEMM = 0
      WINS_ALPHA = 0
      WINS_OPT = 0

*     ============================================================
*     MAIN COMPREHENSIVE TEST LOOP
*     ============================================================
      DO TEST_CASE = 1, NUM_TEST_CASES
          WRITE(*,'(A,I0,A,A)') 'Test ', TEST_CASE, ': ',
     +        TRIM(TEST_NAMES(TEST_CASE))

          WRITE(REPORT_UNIT, '(A)')
     +      '------------------------------------------------------'
          WRITE(REPORT_UNIT, '(A,I0,A,A)') 'TEST ', TEST_CASE, ': ',
     +        TRIM(TEST_NAMES(TEST_CASE))
          WRITE(REPORT_UNIT, '(A)')
     +      '------------------------------------------------------'

*         Generate test matrices based on test case
          CALL GENERATE_TEST_MATRICES(TEST_CASE, A, B, ALPHA, BETA)

*         Store consistent initial C matrix for all algorithms
          DO J = 1, MATRIX_SIZE
              DO I = 1, MATRIX_SIZE
                  INITIAL_C(I,J) = 0.1D+0 * DBLE(I * J)
              END DO
          END DO

*         ============================================================
*         BENCHMARK 1: Standard DGEMM (Reference Implementation)
*         ============================================================
          WRITE(REPORT_UNIT, '(A)') 'ALGORITHM 1: DGEMM (Reference)'

*         Initialize with consistent values
          DO J = 1, MATRIX_SIZE
              DO I = 1, MATRIX_SIZE
                  C_DGEMM(I,J) = INITIAL_C(I,J)
              END DO
          END DO

*         Warm-up runs (eliminate cache effects)
          DO RUN = 1, 1000
              CALL DGEMM('N','N',4,4,4,ALPHA,A,4,B,4,BETA,C_DGEMM,4)
          END DO

*         Precise timed benchmark - DGEMM
          CALL CPU_TIME(START_TIME)
          DO RUN = 1, NRUNS
              CALL DGEMM('N','N',4,4,4,ALPHA,A,4,B,4,BETA,C_DGEMM,4)
          END DO
          CALL CPU_TIME(END_TIME)
          TIMES(1) = END_TIME - START_TIME
          GFLOPS(1) = (FLOPS_4X4 * DBLE(NRUNS)) / (TIMES(1) * 1.0D+9)

          WRITE(REPORT_UNIT, '(A,F12.6,A)') '  Time: ', TIMES(1),
     +        ' seconds'
          WRITE(REPORT_UNIT, '(A,F8.3)') '  GFLOPS: ', GFLOPS(1)

*         ============================================================
*         BENCHMARK 2: DGEMM_ALPHA (AlphaTensor with Logging)
*         ============================================================
          WRITE(REPORT_UNIT, '(A)') 'ALGORITHM 2: DGEMM_ALPHA'

*         Initialize with same values as DGEMM
          DO J = 1, MATRIX_SIZE
              DO I = 1, MATRIX_SIZE
                  C_ALPHA(I,J) = INITIAL_C(I,J)
              END DO
          END DO

*         Warm-up runs
          DO RUN = 1, 1000
              CALL DGEMM_ALPHA('N','N',4,4,4,ALPHA,A,4,B,4,
     $                         BETA,C_ALPHA,4)
          END DO

*         Precise timed benchmark - DGEMM_ALPHA
          CALL CPU_TIME(START_TIME)
          DO RUN = 1, NRUNS
              CALL DGEMM_ALPHA('N','N',4,4,4,ALPHA,A,4,B,4,
     $                         BETA,C_ALPHA,4)
          END DO
          CALL CPU_TIME(END_TIME)
          TIMES(2) = END_TIME - START_TIME
          GFLOPS(2) = (FLOPS_4X4 * DBLE(NRUNS)) / (TIMES(2) * 1.0D+9)

          SPEEDUP_ALPHA = TIMES(1) / TIMES(2)
          WRITE(REPORT_UNIT, '(A,F12.6,A)') '  Time: ', TIMES(2),
     +        ' seconds'
          WRITE(REPORT_UNIT, '(A,F8.3)') '  GFLOPS: ', GFLOPS(2)
          WRITE(REPORT_UNIT, '(A,F8.3,A)') '  Speedup vs DGEMM: ',
     +        SPEEDUP_ALPHA, 'x'

*         ============================================================
*         BENCHMARK 3: DGEMM_ALPHA_OPTIMIZED (Performance Optimized)
*         ============================================================
          WRITE(REPORT_UNIT, '(A)') 'ALGORITHM 3: DGEMM_ALPHA_OPTIMIZED'

*         Initialize with same values as others
          DO J = 1, MATRIX_SIZE
              DO I = 1, MATRIX_SIZE
                  C_OPT(I,J) = INITIAL_C(I,J)
              END DO
          END DO

*         Warm-up runs
          DO RUN = 1, 1000
              CALL DGEMM_ALPHA_OPTIMIZED('N','N',4,4,4,ALPHA,A,4,B,4,
     $                                   BETA,C_OPT,4)
          END DO

*         Precise timed benchmark - DGEMM_ALPHA_OPTIMIZED
          CALL CPU_TIME(START_TIME)
          DO RUN = 1, NRUNS
              CALL DGEMM_ALPHA_OPTIMIZED('N','N',4,4,4,ALPHA,A,4,B,4,
     $                                   BETA,C_OPT,4)
          END DO
          CALL CPU_TIME(END_TIME)
          TIMES(3) = END_TIME - START_TIME
          GFLOPS(3) = (FLOPS_4X4 * DBLE(NRUNS)) / (TIMES(3) * 1.0D+9)

          SPEEDUP_OPT = TIMES(1) / TIMES(3)
          WRITE(REPORT_UNIT, '(A,F12.6,A)') '  Time: ', TIMES(3),
     +        ' seconds'
          WRITE(REPORT_UNIT, '(A,F8.3)') '  GFLOPS: ', GFLOPS(3)
          WRITE(REPORT_UNIT, '(A,F8.3,A)') '  Speedup vs DGEMM: ',
     +        SPEEDUP_OPT, 'x'

*         ============================================================
*         COMPREHENSIVE ACCURACY ANALYSIS
*         ============================================================
          WRITE(REPORT_UNIT, '(A)') ''
          WRITE(REPORT_UNIT, '(A)') 'ACCURACY ANALYSIS:'

*         DGEMM_ALPHA vs DGEMM accuracy
          MAX_A = ZERO
          ERROR_SUM = ZERO
          DO J = 1, MATRIX_SIZE
              DO I = 1, MATRIX_SIZE
                  ERR_A = ABS(C_ALPHA(I,J) - C_DGEMM(I,J))
                  ERROR_SUM = ERROR_SUM + ERR_A
                  IF (ERR_A .GT. MAX_A) THEN
                      MAX_A = ERR_A
                  END IF
              END DO
          END DO
          AVG_A = ERROR_SUM / DBLE(MATRIX_SIZE * MATRIX_SIZE)

*         DGEMM_ALPHA_OPTIMIZED vs DGEMM accuracy
          MAX_O = ZERO
          ERROR_SUM = ZERO
          DO J = 1, MATRIX_SIZE
              DO I = 1, MATRIX_SIZE
                  ERR_O = ABS(C_OPT(I,J) - C_DGEMM(I,J))
                  ERROR_SUM = ERROR_SUM + ERR_O
                  IF (ERR_O .GT. MAX_O) THEN
                      MAX_O = ERR_O
                  END IF
              END DO
          END DO
          AVG_O = ERROR_SUM / DBLE(MATRIX_SIZE * MATRIX_SIZE)

          WRITE(REPORT_UNIT, '(A,ES15.8)')
     +        '  DGEMM_ALPHA Max Error: ', MAX_A
          WRITE(REPORT_UNIT, '(A,ES15.8)')
     +        '  DGEMM_ALPHA Avg Error: ', AVG_A
          WRITE(REPORT_UNIT, '(A,ES15.8)')
     +        '  DGEMM_ALPHA_OPT Max Error: ', MAX_O
          WRITE(REPORT_UNIT, '(A,ES15.8)')
     +        '  DGEMM_ALPHA_OPT Avg Error: ', AVG_O

*         ============================================================
*         PERFORMANCE RANKING & WINNER DETERMINATION
*         ============================================================
          WRITE(REPORT_UNIT, '(A)') ''
          WRITE(REPORT_UNIT, '(A)') 'PERFORMANCE RANKING:'

*         Find fastest algorithm
          FASTEST_ALGO = 1
          DO I = 2, NUM_ALGOS
              IF (TIMES(I) .LT. TIMES(FASTEST_ALGO)) THEN
                  FASTEST_ALGO = I
              END IF
          END DO

*         Sort algorithms by performance (simple ranking)
          DO I = 1, NUM_ALGOS
              ALGO_RANKINGS(I) = I
          END DO
          IF (TIMES(2) .LT. TIMES(1) .AND. TIMES(2) .LT. TIMES(3)) THEN
              ALGO_RANKINGS(1) = 2
              IF (TIMES(1) .LT. TIMES(3)) THEN
                  ALGO_RANKINGS(2) = 1
                  ALGO_RANKINGS(3) = 3
              ELSE
                  ALGO_RANKINGS(2) = 3
                  ALGO_RANKINGS(3) = 1
              END IF
          ELSE IF (TIMES(3) .LT. TIMES(1)) THEN
              ALGO_RANKINGS(1) = 3
              IF (TIMES(1) .LT. TIMES(2)) THEN
                  ALGO_RANKINGS(2) = 1
                  ALGO_RANKINGS(3) = 2
              ELSE
                  ALGO_RANKINGS(2) = 2
                  ALGO_RANKINGS(3) = 1
              END IF
          END IF

          DO I = 1, NUM_ALGOS
              WRITE(REPORT_UNIT, '(A,I0,A,A,A,F8.3,A,F8.3,A)')
     +            '  #', I, ': ', TRIM(ALGO_NAMES(ALGO_RANKINGS(I))),
     +            ' (', TIMES(ALGO_RANKINGS(I)), 's, ',
     +            GFLOPS(ALGO_RANKINGS(I)), ' GFLOPS)'
          END DO

*         Update winner statistics
          IF (FASTEST_ALGO .EQ. 1) THEN
              WINS_DGEMM = WINS_DGEMM + 1
          ELSE IF (FASTEST_ALGO .EQ. 2) THEN
              WINS_ALPHA = WINS_ALPHA + 1
          ELSE
              WINS_OPT = WINS_OPT + 1
          END IF

*         ============================================================
*         TEST CASE ASSESSMENT
*         ============================================================
          WRITE(REPORT_UNIT, '(A)') ''
          WRITE(REPORT_UNIT, '(A)') 'TEST CASE ASSESSMENT:'

*         DGEMM_ALPHA assessment
          IF (SPEEDUP_ALPHA .GT. 1.0D+0 .AND.
     $        MAX_A .LT. 1.0D-10) THEN
              WRITE(REPORT_UNIT, '(A)') '  DGEMM_ALPHA: PASSED'
              PASSED_TESTS_ALPHA = PASSED_TESTS_ALPHA + 1
          ELSE
              WRITE(REPORT_UNIT, '(A)') '  DGEMM_ALPHA: FAILED'
              FAILED_TESTS_ALPHA = FAILED_TESTS_ALPHA + 1
              IF (SPEEDUP_ALPHA .LE. 1.0D+0) THEN
                  WRITE(REPORT_UNIT, '(A)') '    - Performance below'
              END IF
              IF (MAX_A .GE. 1.0D-10) THEN
                  WRITE(REPORT_UNIT, '(A)') '    - Accuracy concerns'
              END IF
          END IF

*         DGEMM_ALPHA_OPTIMIZED assessment
          IF (SPEEDUP_OPT .GT. 1.0D+0 .AND.
     $        MAX_O .LT. 1.0D-10) THEN
              WRITE(REPORT_UNIT, '(A)') '  DGEMM_ALPHA_OPT: PASSED'
              PASSED_TESTS_OPT = PASSED_TESTS_OPT + 1
          ELSE
              WRITE(REPORT_UNIT, '(A)') '  DGEMM_ALPHA_OPT: FAILED'
              FAILED_TESTS_OPT = FAILED_TESTS_OPT + 1
              IF (SPEEDUP_OPT .LE. 1.0D+0) THEN
                  WRITE(REPORT_UNIT, '(A)') '    - Performance below'
              END IF
              IF (MAX_O .GE. 1.0D-10) THEN
                  WRITE(REPORT_UNIT, '(A)') '    - Accuracy concerns'
              END IF
          END IF

          WRITE(REPORT_UNIT, '(A)') ''

*         Update running statistics
          TOTAL_SPEEDUP_ALPHA = TOTAL_SPEEDUP_ALPHA + SPEEDUP_ALPHA
          TOTAL_SPEEDUP_OPT = TOTAL_SPEEDUP_OPT + SPEEDUP_OPT
          TOTAL_ACCURACY_ALPHA = TOTAL_ACCURACY_ALPHA + MAX_A
          TOTAL_ACCURACY_OPT = TOTAL_ACCURACY_OPT + MAX_O

*         Console progress update
          WRITE(*,'(A,F6.2,A,F6.2,A)') '  ALPHA: ', SPEEDUP_ALPHA,
     +        'x, OPT: ', SPEEDUP_OPT, 'x'

      END DO

*     ============================================================
*     ULTIMATE COMPREHENSIVE SUMMARY
*     ============================================================
      WRITE(REPORT_UNIT, '(A)')
     +  '=============================================================='
      WRITE(REPORT_UNIT, '(A)')
     +  '                   ULTIMATE SUMMARY'
      WRITE(REPORT_UNIT, '(A)')
     +  '=============================================================='

      WRITE(REPORT_UNIT, '(A)') 'OVERALL PERFORMANCE CHAMPION:'
      IF (WINS_DGEMM .GT. WINS_ALPHA .AND.
     +    WINS_DGEMM .GT. WINS_OPT) THEN
          WRITE(REPORT_UNIT, '(A,I0,A,I0,A)')
     +        '  DGEMM (Reference) - ', WINS_DGEMM, ' / ',
     +        NUM_TEST_CASES, ' wins'
      ELSE IF (WINS_ALPHA .GT. WINS_OPT) THEN
          WRITE(REPORT_UNIT, '(A,I0,A,I0,A)')
     +        '  DGEMM_ALPHA - ', WINS_ALPHA, ' / ',
     +        NUM_TEST_CASES, ' wins'
      ELSE
          WRITE(REPORT_UNIT, '(A,I0,A,I0,A)')
     +        '  DGEMM_ALPHA_OPTIMIZED - ', WINS_OPT, ' / ',
     +        NUM_TEST_CASES, ' wins'
      END IF

      WRITE(REPORT_UNIT, '(A)') ''
      WRITE(REPORT_UNIT, '(A)') 'DETAILED RESULTS:'
      WRITE(REPORT_UNIT, '(A,I0,A,I0)') 'DGEMM_ALPHA Tests Passed: ',
     +    PASSED_TESTS_ALPHA, ' / ', NUM_TEST_CASES
      WRITE(REPORT_UNIT, '(A,I0,A,I0)') 'DGEMM_ALPHA Tests Failed: ',
     +    FAILED_TESTS_ALPHA, ' / ', NUM_TEST_CASES
      WRITE(REPORT_UNIT, '(A,F8.3,A)') 'DGEMM_ALPHA Average Speedup: ',
     +    TOTAL_SPEEDUP_ALPHA / DBLE(NUM_TEST_CASES), 'x'
      WRITE(REPORT_UNIT, '(A,ES15.8)') 'DGEMM_ALPHA Max Error: ',
     +    TOTAL_ACCURACY_ALPHA / DBLE(NUM_TEST_CASES)

      WRITE(REPORT_UNIT, '(A)') ''
      WRITE(REPORT_UNIT, '(A,I0,A,I0)') 'DGEMM_OPT Tests Passed: ',
     +    PASSED_TESTS_OPT, ' / ', NUM_TEST_CASES
      WRITE(REPORT_UNIT, '(A,I0,A,I0)') 'DGEMM_OPT Tests Failed: ',
     +    FAILED_TESTS_OPT, ' / ', NUM_TEST_CASES
      WRITE(REPORT_UNIT, '(A,F8.3,A)') 'DGEMM_OPT Speedup: ',
     +    TOTAL_SPEEDUP_OPT / DBLE(NUM_TEST_CASES), 'x'
      WRITE(REPORT_UNIT, '(A,ES15.8)') 'DGEMM_OPT Max Error: ',
     +    TOTAL_ACCURACY_OPT / DBLE(NUM_TEST_CASES)

      WRITE(REPORT_UNIT, '(A)') ''
      WRITE(REPORT_UNIT, '(A)') 'RECOMMENDATION:'
      IF (PASSED_TESTS_OPT .EQ. NUM_TEST_CASES .AND.
     +    TOTAL_SPEEDUP_OPT .GT. TOTAL_SPEEDUP_ALPHA) THEN
          WRITE(REPORT_UNIT, '(A)')
     +      'EXCELLENT: Use DGEMM_ALPHA_OPTIMIZED for production!'
          WRITE(REPORT_UNIT, '(A)')
     +      'Superior performance with perfect accuracy.'
      ELSE IF (PASSED_TESTS_ALPHA .EQ. NUM_TEST_CASES) THEN
          WRITE(REPORT_UNIT, '(A)')
     +      'GOOD: Use DGEMM_ALPHA for development and debugging.'
          WRITE(REPORT_UNIT, '(A)')
     +      'Reliable with logging for analysis.'
      ELSE
          WRITE(REPORT_UNIT, '(A)')
     +      'CAUTION: Stick with DGEMM until issues resolved.'
      END IF

      WRITE(REPORT_UNIT, '(A)') ''
      WRITE(REPORT_UNIT, '(A)') 'NEXT OPTIMIZATION TARGETS:'
      WRITE(REPORT_UNIT, '(A)') '1. Algorithm vectorization (SIMD)'
      WRITE(REPORT_UNIT, '(A)') '2. Memory layout optimization'
      WRITE(REPORT_UNIT, '(A)') '3. Cache-aware data structures'
      WRITE(REPORT_UNIT, '(A)') '4. Extended matrix size support'
      WRITE(REPORT_UNIT, '(A)')
     +  '=============================================================='

*     Close comprehensive report
      CLOSE(REPORT_UNIT)

*     Final console summary
      WRITE(*,*) '=================================================='
      WRITE(*,*) 'ULTIMATE TESTING COMPLETE!'
      WRITE(*,'(A,I0,A,I0,A,I0,A)') 'Performance wins: DGEMM=',
     +    WINS_DGEMM, ', ALPHA=', WINS_ALPHA, ', OPT=', WINS_OPT
      WRITE(*,'(A,F6.2,A,F6.2,A)') 'Average speedups: ALPHA=',
     +    TOTAL_SPEEDUP_ALPHA / DBLE(NUM_TEST_CASES), 'x, OPT=',
     +    TOTAL_SPEEDUP_OPT / DBLE(NUM_TEST_CASES), 'x'
      WRITE(*,*) 'Report: ultimate_4x4_alphatensor_report.txt'
      WRITE(*,*) '=================================================='

      END

*     ============================================================
*     SUBROUTINE: Generate Test Matrices (Enhanced Version)
*     ============================================================
      SUBROUTINE GENERATE_TEST_MATRICES(TEST_CASE, A, B, ALPHA, BETA)
      IMPLICIT NONE
      INTEGER TEST_CASE, I, J
      DOUBLE PRECISION A(4,4), B(4,4), ALPHA, BETA
      DOUBLE PRECISION PI
      PARAMETER (PI=3.14159265358979323846D+0)

*     Default coefficient values for comprehensive testing
      ALPHA = 1.5D+0
      BETA = 0.25D+0

      IF (TEST_CASE .EQ. 1) THEN
*         Identity matrices - Perfect for algorithm verification
          DO J = 1, 4
              DO I = 1, 4
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
          DO J = 1, 4
              DO I = 1, 4
                  A(I,J) = 0.0D+0
                  B(I,J) = 0.0D+0
              END DO
          END DO

      ELSE IF (TEST_CASE .EQ. 3) THEN
*         Random dense matrices - Real-world simulation
          DO J = 1, 4
              DO I = 1, 4
                  A(I,J) = DBLE(I + 2*J - 1) / 7.0D+0
                  B(I,J) = DBLE(3*I - J + 5) / 11.0D+0
              END DO
          END DO

      ELSE IF (TEST_CASE .EQ. 4) THEN
*         Diagonal matrices - Sparse structure testing
          DO J = 1, 4
              DO I = 1, 4
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
          DO J = 1, 4
              DO I = 1, 4
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
          DO J = 1, 4
              DO I = 1, 4
                  A(I,J) = 0.0D+0
                  B(I,J) = 0.0D+0
              END DO
          END DO
          A(1,1) = 3.2D+0
          A(2,3) = 1.7D+0
          A(4,2) = -2.1D+0
          B(1,4) = 2.8D+0
          B(3,1) = -1.3D+0
          B(4,4) = 4.5D+0

      ELSE IF (TEST_CASE .EQ. 7) THEN
*         Large value matrices - Overflow resistance
          DO J = 1, 4
              DO I = 1, 4
                  A(I,J) = DBLE(I*J) * 1.0D+6
                  B(I,J) = DBLE(I+J) * 2.5D+5
              END DO
          END DO

      ELSE IF (TEST_CASE .EQ. 8) THEN
*         Small value matrices - Underflow resistance
          DO J = 1, 4
              DO I = 1, 4
                  A(I,J) = DBLE(I*J) * 1.0D-6
                  B(I,J) = DBLE(I+J) * 2.5D-7
              END DO
          END DO

      ELSE IF (TEST_CASE .EQ. 9) THEN
*         Mixed sign matrices - Sign handling verification
          DO J = 1, 4
              DO I = 1, 4
                  A(I,J) = DBLE((-1)**(I+J)) * DBLE(I*J) / 4.0D+0
                  B(I,J) = DBLE((-1)**I) * DBLE(I+J) / 3.0D+0
              END DO
          END DO

      ELSE IF (TEST_CASE .EQ. 10) THEN
*         Ill-conditioned matrices - Numerical stability
          DO J = 1, 4
              DO I = 1, 4
                  A(I,J) = 1.0D+0 / DBLE(I + J - 1)
                  B(I,J) = DBLE(I)**DBLE(J-1)
              END DO
          END DO

      ELSE IF (TEST_CASE .EQ. 11) THEN
*         Integer matrices - Exact arithmetic verification
          DO J = 1, 4
              DO I = 1, 4
                  A(I,J) = DBLE(I * J)
                  B(I,J) = DBLE(I + J)
              END DO
          END DO

      ELSE IF (TEST_CASE .EQ. 12) THEN
*         Stress test matrices - Complex trigonometric patterns
          DO J = 1, 4
              DO I = 1, 4
                  A(I,J) = SIN(DBLE(I) * PI / 6.0D+0) +
     +                     COS(DBLE(J) * PI / 4.0D+0)
                  B(I,J) = COS(DBLE(I) * PI / 8.0D+0) -
     +                     SIN(DBLE(J) * PI / 3.0D+0)
              END DO
          END DO

      END IF

      RETURN
      END
