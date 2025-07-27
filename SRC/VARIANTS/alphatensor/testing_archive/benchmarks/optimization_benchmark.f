      PROGRAM ALPHATENSOR_OPTIMIZATION_BENCHMARK
*
*  -- AlphaTensor Optimization Benchmark --
*  -- Focus: Beat DGEMM performance systematically --
*  -- Strategy: Detailed profiling of specific bottlenecks --
*  -- Target: 4x4 matrices where AlphaTensor should dominate --
*
      IMPLICIT NONE
*
*     .. Parameters ..
      INTEGER NUM_TEST_CASES, REPORT_UNIT, NUM_SIZES
      PARAMETER (NUM_TEST_CASES=12, NUM_SIZES=4, REPORT_UNIT=10)
      DOUBLE PRECISION ONE, ZERO, PI
      PARAMETER (ONE=1.0D+0, ZERO=0.0D+0, PI=3.14159265358979323846D+0)
*     ..
*     .. Local Arrays ..
      DOUBLE PRECISION A(32,32), B(32,32)
      DOUBLE PRECISION C_DGEMM(32,32), C_ALPHA(32,32)
      DOUBLE PRECISION INITIAL_C(32,32)
      CHARACTER*50 TEST_NAMES(12)
      INTEGER MATRIX_SIZES(4), NRUNS_BY_SIZE(4)
*     ..
*     .. Local Scalars ..
      INTEGER I, J, RUN, TEST_CASE, SIZE_IDX
      INTEGER CURRENT_SIZE, CURRENT_NRUNS
      DOUBLE PRECISION ALPHA, BETA
      DOUBLE PRECISION START_TIME, END_TIME
      DOUBLE PRECISION TIME_DGEMM, TIME_ALPHA
      DOUBLE PRECISION GFLOPS_DGEMM, GFLOPS_ALPHA, SPEEDUP
      DOUBLE PRECISION ERROR_MAX, ERROR_AVG, ERROR_SUM, ERROR_VAL
      DOUBLE PRECISION FLOPS_CURRENT
      DOUBLE PRECISION TOTAL_SPEEDUP, TOTAL_ERROR
      INTEGER WINS_DGEMM, WINS_ALPHA, TESTS_PASSED, TESTS_FAILED
      LOGICAL IS_ALPHATENSOR_ACTIVE
*     ..
*     .. External Subroutines ..
      EXTERNAL DGEMM, DGEMM_ALPHA
*     ..

*     Initialize matrix sizes and iteration counts
      MATRIX_SIZES(1) = 4     ! Primary target
      MATRIX_SIZES(2) = 8     ! Fallback case
      MATRIX_SIZES(3) = 16    ! Scaling test
      MATRIX_SIZES(4) = 32    ! Large fallback

*     More iterations for smaller matrices, fewer for larger
      NRUNS_BY_SIZE(1) = 100000  ! 4x4: maximum precision
      NRUNS_BY_SIZE(2) = 20000   ! 8x8: good precision
      NRUNS_BY_SIZE(3) = 5000    ! 16x16: moderate
      NRUNS_BY_SIZE(4) = 1000    ! 32x32: basic

*     Initialize test names
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

*     Open optimization report
      OPEN(UNIT=REPORT_UNIT,
     $     FILE='alphatensor_optimization_report.txt',
     $     STATUS='REPLACE')

*     Header
      WRITE(REPORT_UNIT, '(A)')
     +  '================================================='
      WRITE(REPORT_UNIT, '(A)')
     +  '   ALPHATENSOR OPTIMIZATION BENCHMARK'
      WRITE(REPORT_UNIT, '(A)')
     +  '   Goal: Beat DGEMM systematically'
      WRITE(REPORT_UNIT, '(A)')
     +  '================================================='
      WRITE(REPORT_UNIT, '(A)') ''

      WRITE(*,*) '======================================='
      WRITE(*,*) 'ALPHATENSOR OPTIMIZATION BENCHMARK'
      WRITE(*,*) 'Goal: Beat DGEMM systematically'
      WRITE(*,*) '======================================='

*     Initialize statistics
      TOTAL_SPEEDUP = ZERO
      TOTAL_ERROR = ZERO
      WINS_DGEMM = 0
      WINS_ALPHA = 0
      TESTS_PASSED = 0
      TESTS_FAILED = 0

*     Main test loop
      DO SIZE_IDX = 1, NUM_SIZES
          CURRENT_SIZE = MATRIX_SIZES(SIZE_IDX)
          CURRENT_NRUNS = NRUNS_BY_SIZE(SIZE_IDX)
          IS_ALPHATENSOR_ACTIVE = (CURRENT_SIZE .EQ. 4)

          WRITE(*,'(A)') '======================================='
          IF (IS_ALPHATENSOR_ACTIVE) THEN
              WRITE(*,'(A,I0,A,I0,A)') 'TESTING ', CURRENT_SIZE, 'x',
     +            CURRENT_SIZE, ' (AlphaTensor TARGET)'
          ELSE
              WRITE(*,'(A,I0,A,I0,A)') 'TESTING ', CURRENT_SIZE, 'x',
     +            CURRENT_SIZE, ' (Fallback mode)'
          END IF
          WRITE(*,'(A,I0,A)') 'Iterations: ', CURRENT_NRUNS, ' per test'

          WRITE(REPORT_UNIT, '(A)')
     +      '================================================='
          WRITE(REPORT_UNIT, '(A,I0,A,I0,A)') 'MATRIX SIZE: ',
     +        CURRENT_SIZE, 'x', CURRENT_SIZE
          IF (IS_ALPHATENSOR_ACTIVE) THEN
              WRITE(REPORT_UNIT, '(A)') 'STATUS: AlphaTensor TARGET'
          ELSE
              WRITE(REPORT_UNIT, '(A)') 'STATUS: Fallback to DGEMM'
          END IF
          WRITE(REPORT_UNIT, '(A,I0)') 'Iterations per test: ',
     +        CURRENT_NRUNS
          WRITE(REPORT_UNIT, '(A)')
     +      '================================================='

          DO TEST_CASE = 1, NUM_TEST_CASES
              WRITE(*,'(A,I0,A,A)') 'Test ', TEST_CASE, ': ',
     +            TRIM(TEST_NAMES(TEST_CASE))

              WRITE(REPORT_UNIT, '(A)') ''
              WRITE(REPORT_UNIT, '(A,I0,A,A)') 'TEST ', TEST_CASE,
     +            ': ', TRIM(TEST_NAMES(TEST_CASE))
              WRITE(REPORT_UNIT, '(A)')
     +            '--------------------------------'

*             Generate test matrices
              CALL GENERATE_TEST_MATRICES_VARIABLE_SIZE(TEST_CASE, A, B,
     +            ALPHA, BETA, CURRENT_SIZE)

*             Initialize consistent C matrix
              DO J = 1, CURRENT_SIZE
                  DO I = 1, CURRENT_SIZE
                      INITIAL_C(I,J) = 0.1D+0 * DBLE(I * J)
                  END DO
              END DO

*             ==============================================
*             BENCHMARK 1: Standard DGEMM (Reference)
*             ==============================================
              DO J = 1, CURRENT_SIZE
                  DO I = 1, CURRENT_SIZE
                      C_DGEMM(I,J) = INITIAL_C(I,J)
                  END DO
              END DO

*             Warm-up
              DO RUN = 1, MIN(1000, CURRENT_NRUNS/50)
                  CALL DGEMM('N','N',CURRENT_SIZE,CURRENT_SIZE,
     +                CURRENT_SIZE,ALPHA,A,32,B,32,BETA,C_DGEMM,32)
              END DO

*             Timed benchmark
              CALL CPU_TIME(START_TIME)
              DO RUN = 1, CURRENT_NRUNS
                  CALL DGEMM('N','N',CURRENT_SIZE,CURRENT_SIZE,
     +                CURRENT_SIZE,ALPHA,A,32,B,32,BETA,C_DGEMM,32)
              END DO
              CALL CPU_TIME(END_TIME)
              TIME_DGEMM = END_TIME - START_TIME

*             ==============================================
*             BENCHMARK 2: DGEMM_ALPHA (Optimized AlphaTensor)
*             ==============================================
              DO J = 1, CURRENT_SIZE
                  DO I = 1, CURRENT_SIZE
                      C_ALPHA(I,J) = INITIAL_C(I,J)
                  END DO
              END DO

*             Warm-up
              DO RUN = 1, MIN(1000, CURRENT_NRUNS/50)
                  CALL DGEMM_ALPHA('N','N',CURRENT_SIZE,CURRENT_SIZE,
     +                CURRENT_SIZE,ALPHA,A,32,B,32,BETA,C_ALPHA,32)
              END DO

*             Timed benchmark
              CALL CPU_TIME(START_TIME)
              DO RUN = 1, CURRENT_NRUNS
                  CALL DGEMM_ALPHA('N','N',CURRENT_SIZE,CURRENT_SIZE,
     +                CURRENT_SIZE,ALPHA,A,32,B,32,BETA,C_ALPHA,32)
              END DO
              CALL CPU_TIME(END_TIME)
              TIME_ALPHA = END_TIME - START_TIME

*             ==============================================
*             PERFORMANCE ANALYSIS
*             ==============================================
              FLOPS_CURRENT = 2.0D+0 * DBLE(CURRENT_SIZE) *
     +                        DBLE(CURRENT_SIZE) * DBLE(CURRENT_SIZE)
              GFLOPS_DGEMM = (FLOPS_CURRENT * DBLE(CURRENT_NRUNS)) /
     +                       (TIME_DGEMM * 1.0D+9)
              GFLOPS_ALPHA = (FLOPS_CURRENT * DBLE(CURRENT_NRUNS)) /
     +                       (TIME_ALPHA * 1.0D+9)
              SPEEDUP = TIME_DGEMM / TIME_ALPHA

*             ==============================================
*             ACCURACY ANALYSIS
*             ==============================================
              ERROR_MAX = ZERO
              ERROR_SUM = ZERO
              DO J = 1, CURRENT_SIZE
                  DO I = 1, CURRENT_SIZE
                      ERROR_VAL = ABS(C_ALPHA(I,J) - C_DGEMM(I,J))
                      ERROR_SUM = ERROR_SUM + ERROR_VAL
                      IF (ERROR_VAL .GT. ERROR_MAX) THEN
                          ERROR_MAX = ERROR_VAL
                      END IF
                  END DO
              END DO
              ERROR_AVG = ERROR_SUM / DBLE(CURRENT_SIZE * CURRENT_SIZE)

*             ==============================================
*             RESULTS REPORTING
*             ==============================================
              WRITE(REPORT_UNIT, '(A,F12.6,A,F8.3,A)')
     +            'DGEMM:       ', TIME_DGEMM, 's  ', GFLOPS_DGEMM,
     +            ' GFLOPS'
              WRITE(REPORT_UNIT, '(A,F12.6,A,F8.3,A)')
     +            'DGEMM_ALPHA: ', TIME_ALPHA, 's  ', GFLOPS_ALPHA,
     +            ' GFLOPS'
              WRITE(REPORT_UNIT, '(A,F8.3,A)') 'SPEEDUP: ', SPEEDUP, 'x'
              WRITE(REPORT_UNIT, '(A,ES12.5)') 'MAX ERROR: ', ERROR_MAX
              WRITE(REPORT_UNIT, '(A,ES12.5)') 'AVG ERROR: ', ERROR_AVG

*             Update statistics
              TOTAL_SPEEDUP = TOTAL_SPEEDUP + SPEEDUP
              TOTAL_ERROR = TOTAL_ERROR + ERROR_MAX

              IF (SPEEDUP .GT. 1.0D+0 .AND. ERROR_MAX .LT. 1.0D-10) THEN
                  WRITE(REPORT_UNIT, '(A)') 'RESULT: PASS (ALPHA WINS!)'
                  TESTS_PASSED = TESTS_PASSED + 1
                  WINS_ALPHA = WINS_ALPHA + 1
              ELSE
                  WRITE(REPORT_UNIT, '(A)') 'RESULT: FAIL (DGEMM wins)'
                  TESTS_FAILED = TESTS_FAILED + 1
                  WINS_DGEMM = WINS_DGEMM + 1
                  IF (SPEEDUP .LE. 1.0D+0) THEN
                      WRITE(REPORT_UNIT, '(A)') '  Issue: Performance'
                  END IF
                  IF (ERROR_MAX .GE. 1.0D-10) THEN
                      WRITE(REPORT_UNIT, '(A)') '  Issue: Accuracy'
                  END IF
              END IF

*             Console progress
              IF (SPEEDUP .GT. 1.0D+0) THEN
                  WRITE(*,'(A,F6.2,A)') '  ALPHA WINS: ', SPEEDUP, 'x'
              ELSE
                  WRITE(*,'(A,F6.2,A)') '  DGEMM wins: ', 1.0D+0/SPEEDUP, 'x'
              END IF

          END DO
      END DO

*     ==============================================
*     OPTIMIZATION SUMMARY & RECOMMENDATIONS
*     ==============================================
      WRITE(REPORT_UNIT, '(A)') ''
      WRITE(REPORT_UNIT, '(A)')
     +  '================================================='
      WRITE(REPORT_UNIT, '(A)')
     +  '              OPTIMIZATION SUMMARY'
      WRITE(REPORT_UNIT, '(A)')
     +  '================================================='

      WRITE(REPORT_UNIT, '(A,I0,A,I0)') 'AlphaTensor Wins: ',
     +    WINS_ALPHA, ' / ', TESTS_PASSED + TESTS_FAILED
      WRITE(REPORT_UNIT, '(A,I0,A,I0)') 'DGEMM Wins: ',
     +    WINS_DGEMM, ' / ', TESTS_PASSED + TESTS_FAILED
      WRITE(REPORT_UNIT, '(A,F8.3,A)') 'Average Speedup: ',
     +    TOTAL_SPEEDUP / DBLE(TESTS_PASSED + TESTS_FAILED), 'x'
      WRITE(REPORT_UNIT, '(A,ES12.5)') 'Average Max Error: ',
     +    TOTAL_ERROR / DBLE(TESTS_PASSED + TESTS_FAILED)

      WRITE(REPORT_UNIT, '(A)') ''
      WRITE(REPORT_UNIT, '(A)') 'NEXT OPTIMIZATION TARGETS:'
      IF (WINS_ALPHA .LT. (TESTS_PASSED + TESTS_FAILED) / 2) THEN
          WRITE(REPORT_UNIT, '(A)') '1. CRITICAL: Fix 4x4 performance'
          WRITE(REPORT_UNIT, '(A)') '2. Profile memory access patterns'
          WRITE(REPORT_UNIT, '(A)') '3. Optimize cache utilization'
          WRITE(REPORT_UNIT, '(A)') '4. Consider vectorization (SIMD)'
      ELSE
          WRITE(REPORT_UNIT, '(A)') '1. Fine-tune winning cases'
          WRITE(REPORT_UNIT, '(A)') '2. Extend to larger matrix sizes'
          WRITE(REPORT_UNIT, '(A)') '3. Advanced optimizations'
      END IF

      CLOSE(REPORT_UNIT)

*     Final console summary
      WRITE(*,*) '======================================='
      WRITE(*,*) 'OPTIMIZATION BENCHMARK COMPLETE!'
      WRITE(*,'(A,I0,A,I0)') 'AlphaTensor wins: ', WINS_ALPHA,
     +    ' / ', TESTS_PASSED + TESTS_FAILED
      WRITE(*,'(A,F6.2,A)') 'Average speedup: ',
     +    TOTAL_SPEEDUP / DBLE(TESTS_PASSED + TESTS_FAILED), 'x'
      WRITE(*,*) 'Report: alphatensor_optimization_report.txt'
      WRITE(*,*) '======================================='

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

*     Default values
      ALPHA = 1.5D+0
      BETA = 0.25D+0

*     Initialize to zero
      DO J = 1, 32
          DO I = 1, 32
              A(I,J) = 0.0D+0
              B(I,J) = 0.0D+0
          END DO
      END DO

      IF (TEST_CASE .EQ. 1) THEN
*         Identity matrices
          DO J = 1, MATRIX_SIZE
              DO I = 1, MATRIX_SIZE
                  IF (I .EQ. J) THEN
                      A(I,J) = 1.0D+0
                      B(I,J) = 1.0D+0
                  END IF
              END DO
          END DO

      ELSE IF (TEST_CASE .EQ. 2) THEN
*         Zero matrices (already initialized)
          CONTINUE

      ELSE IF (TEST_CASE .EQ. 3) THEN
*         Random dense matrices
          DO J = 1, MATRIX_SIZE
              DO I = 1, MATRIX_SIZE
                  A(I,J) = DBLE(I + 2*J - 1) / 7.0D+0
                  B(I,J) = DBLE(3*I - J + 5) / 11.0D+0
              END DO
          END DO

      ELSE IF (TEST_CASE .EQ. 4) THEN
*         Diagonal matrices
          DO J = 1, MATRIX_SIZE
              DO I = 1, MATRIX_SIZE
                  IF (I .EQ. J) THEN
                      A(I,J) = DBLE(I) * 2.5D+0
                      B(I,J) = DBLE(I) * 1.8D+0
                  END IF
              END DO
          END DO

      ELSE IF (TEST_CASE .EQ. 5) THEN
*         Symmetric matrices
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
*         Sparse matrices
          A(1,1) = 3.2D+0
          B(1,MATRIX_SIZE) = 2.8D+0
          IF (MATRIX_SIZE .GT. 2) THEN
              A(2,MATRIX_SIZE-1) = 1.7D+0
              B(MATRIX_SIZE-1,1) = -1.3D+0
          END IF
          A(MATRIX_SIZE,MATRIX_SIZE) = 4.5D+0
          B(MATRIX_SIZE,MATRIX_SIZE) = -2.1D+0

      ELSE IF (TEST_CASE .EQ. 7) THEN
*         Large value matrices
          DO J = 1, MATRIX_SIZE
              DO I = 1, MATRIX_SIZE
                  A(I,J) = DBLE(I*J) * 1.0D+6
                  B(I,J) = DBLE(I+J) * 2.5D+5
              END DO
          END DO

      ELSE IF (TEST_CASE .EQ. 8) THEN
*         Small value matrices
          DO J = 1, MATRIX_SIZE
              DO I = 1, MATRIX_SIZE
                  A(I,J) = DBLE(I*J) * 1.0D-6
                  B(I,J) = DBLE(I+J) * 2.5D-7
              END DO
          END DO

      ELSE IF (TEST_CASE .EQ. 9) THEN
*         Mixed sign matrices
          DO J = 1, MATRIX_SIZE
              DO I = 1, MATRIX_SIZE
                  A(I,J) = DBLE((-1)**(I+J)) * DBLE(I*J) / 4.0D+0
                  B(I,J) = DBLE((-1)**I) * DBLE(I+J) / 3.0D+0
              END DO
          END DO

      ELSE IF (TEST_CASE .EQ. 10) THEN
*         Ill-conditioned matrices
          DO J = 1, MATRIX_SIZE
              DO I = 1, MATRIX_SIZE
                  A(I,J) = 1.0D+0 / DBLE(I + J - 1)
                  B(I,J) = DBLE(I)**DBLE(J-1)
              END DO
          END DO

      ELSE IF (TEST_CASE .EQ. 11) THEN
*         Integer matrices
          DO J = 1, MATRIX_SIZE
              DO I = 1, MATRIX_SIZE
                  A(I,J) = DBLE(I * J)
                  B(I,J) = DBLE(I + J)
              END DO
          END DO

      ELSE IF (TEST_CASE .EQ. 12) THEN
*         Stress test matrices
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
