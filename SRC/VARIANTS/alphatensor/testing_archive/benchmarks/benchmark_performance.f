      PROGRAM BENCHMARK_PERFORMANCE
*
*  -- AlphaTensor Performance Benchmarking Tool --
*  -- Compares: Standard DGEMM vs Original AlphaTensor vs Optimized AlphaTensor --
*  -- Measures: Execution time, GFLOPS, and speedup ratios --
*
      IMPLICIT NONE
*
*     .. Parameters ..
      INTEGER NRUNS, MATRIX_SIZE
      PARAMETER (NRUNS=10000, MATRIX_SIZE=4)
      DOUBLE PRECISION ONE, ZERO
      PARAMETER (ONE=1.0D+0, ZERO=0.0D+0)
*     ..
*     .. Local Arrays ..
      DOUBLE PRECISION A(MATRIX_SIZE,MATRIX_SIZE)
      DOUBLE PRECISION B(MATRIX_SIZE,MATRIX_SIZE)
      DOUBLE PRECISION C_DGEMM(MATRIX_SIZE,MATRIX_SIZE)
      DOUBLE PRECISION C_ALPHA(MATRIX_SIZE,MATRIX_SIZE)
      DOUBLE PRECISION C_OPTIM(MATRIX_SIZE,MATRIX_SIZE)
*     ..
*     .. Local Scalars ..
      INTEGER I, J, RUN
      DOUBLE PRECISION ALPHA, BETA
      DOUBLE PRECISION START_TIME, END_TIME
      DOUBLE PRECISION TIME_DGEMM, TIME_ALPHA, TIME_OPTIM
      DOUBLE PRECISION GFLOPS_DGEMM, GFLOPS_ALPHA, GFLOPS_OPTIM
      DOUBLE PRECISION SPEEDUP_ALPHA, SPEEDUP_OPTIM
      DOUBLE PRECISION FLOPS_4X4
      DOUBLE PRECISION ERROR_ALPHA, ERROR_OPTIM, MAX_ERROR
*     ..
*     .. External Functions ..
      DOUBLE PRECISION CPU_TIME_FUNC
      EXTERNAL CPU_TIME_FUNC
*     ..
*     .. External Subroutines ..
      EXTERNAL DGEMM, DGEMM_ALPHA, DGEMM_ALPHA_OPTIMIZED
*     ..
*
      WRITE(*,*) '======================================================='
      WRITE(*,*) '      AlphaTensor Performance Benchmark'
      WRITE(*,*) '======================================================='
      WRITE(*,*) ' Comparing 4x4 matrix multiplication algorithms:'
      WRITE(*,*) ' 1. Standard DGEMM (64 operations)'
      WRITE(*,*) ' 2. Original AlphaTensor (49 operations + logging)'
      WRITE(*,*) ' 3. Optimized AlphaTensor (49 operations, no logging)'
      WRITE(*,*) ''
      WRITE(*,*) ' Number of iterations:', NRUNS
      WRITE(*,*) '======================================================='
*
*     Initialize test matrices with deterministic values
      ALPHA = 1.0D+0
      BETA = 0.5D+0
*
*     Create test matrices (same pattern as comprehensive_test.f)
      DO J = 1, MATRIX_SIZE
          DO I = 1, MATRIX_SIZE
              A(I,J) = DBLE(I + J) / 10.0D+0
              B(I,J) = DBLE(I * J) / 5.0D+0
          END DO
      END DO
*
*     Calculate theoretical FLOPS for 4x4 matrix multiplication
*     Formula: 2*M*N*K operations (multiply + add)
      FLOPS_4X4 = 2.0D+0 * MATRIX_SIZE * MATRIX_SIZE * MATRIX_SIZE
*
*     ====================================================
*     BENCHMARK 1: Standard DGEMM
*     ====================================================
      WRITE(*,*) ''
      WRITE(*,*) ' Benchmarking Standard DGEMM...'
*
*     Initialize result matrix
      DO J = 1, MATRIX_SIZE
          DO I = 1, MATRIX_SIZE
              C_DGEMM(I,J) = 0.1D+0 * DBLE(I + J)
          END DO
      END DO
*
*     Warm-up run
      CALL DGEMM('N','N',MATRIX_SIZE,MATRIX_SIZE,MATRIX_SIZE,ALPHA,
     +           A,MATRIX_SIZE,B,MATRIX_SIZE,BETA,C_DGEMM,MATRIX_SIZE)
*
*     Timed benchmark
      CALL CPU_TIME(START_TIME)
      DO RUN = 1, NRUNS
          CALL DGEMM('N','N',MATRIX_SIZE,MATRIX_SIZE,MATRIX_SIZE,ALPHA,
     +               A,MATRIX_SIZE,B,MATRIX_SIZE,BETA,C_DGEMM,
     +               MATRIX_SIZE)
      END DO
      CALL CPU_TIME(END_TIME)
      TIME_DGEMM = END_TIME - START_TIME
*
*     Calculate performance metrics
      GFLOPS_DGEMM = (FLOPS_4X4 * NRUNS) / (TIME_DGEMM * 1.0D+9)
*
      WRITE(*,*) ' Standard DGEMM Results:'
      WRITE(*,*) '   Total time (s):', TIME_DGEMM
      WRITE(*,*) '   Performance (GFLOPS):', GFLOPS_DGEMM
*
*     ====================================================
*     BENCHMARK 2: Original AlphaTensor (with logging)
*     ====================================================
      WRITE(*,*) ''
      WRITE(*,*) ' Benchmarking Original AlphaTensor...'
*
*     Initialize result matrix (same initial values)
      DO J = 1, MATRIX_SIZE
          DO I = 1, MATRIX_SIZE
              C_ALPHA(I,J) = 0.1D+0 * DBLE(I + J)
          END DO
      END DO
*
*     Warm-up run
      CALL DGEMM_ALPHA('N','N',MATRIX_SIZE,MATRIX_SIZE,MATRIX_SIZE,
     +                 ALPHA,A,MATRIX_SIZE,B,MATRIX_SIZE,BETA,C_ALPHA,
     +                 MATRIX_SIZE)
*
*     Timed benchmark
      CALL CPU_TIME(START_TIME)
      DO RUN = 1, NRUNS
          CALL DGEMM_ALPHA('N','N',MATRIX_SIZE,MATRIX_SIZE,MATRIX_SIZE,
     +                     ALPHA,A,MATRIX_SIZE,B,MATRIX_SIZE,BETA,
     +                     C_ALPHA,MATRIX_SIZE)
      END DO
      CALL CPU_TIME(END_TIME)
      TIME_ALPHA = END_TIME - START_TIME
*
*     Calculate performance metrics
      GFLOPS_ALPHA = (FLOPS_4X4 * NRUNS) / (TIME_ALPHA * 1.0D+9)
      SPEEDUP_ALPHA = TIME_DGEMM / TIME_ALPHA
*
*     Calculate numerical accuracy
      MAX_ERROR = ZERO
      DO J = 1, MATRIX_SIZE
          DO I = 1, MATRIX_SIZE
              ERROR_ALPHA = ABS(C_ALPHA(I,J) - C_DGEMM(I,J))
              IF (ERROR_ALPHA .GT. MAX_ERROR) THEN
                  MAX_ERROR = ERROR_ALPHA
              END IF
          END DO
      END DO
*
      WRITE(*,*) ' Original AlphaTensor Results:'
      WRITE(*,*) '   Total time (s):', TIME_ALPHA
      WRITE(*,*) '   Performance (GFLOPS):', GFLOPS_ALPHA
      WRITE(*,*) '   Speedup vs DGEMM:', SPEEDUP_ALPHA
      WRITE(*,*) '   Max error vs DGEMM:', MAX_ERROR
*
*     ====================================================
*     BENCHMARK 3: Optimized AlphaTensor
*     ====================================================
      WRITE(*,*) ''
      WRITE(*,*) ' Benchmarking Optimized AlphaTensor...'
*
*     Initialize result matrix (same initial values)
      DO J = 1, MATRIX_SIZE
          DO I = 1, MATRIX_SIZE
              C_OPTIM(I,J) = 0.1D+0 * DBLE(I + J)
          END DO
      END DO
*
*     Warm-up run
      CALL DGEMM_ALPHA_OPTIMIZED('N','N',MATRIX_SIZE,MATRIX_SIZE,
     +                           MATRIX_SIZE,ALPHA,A,MATRIX_SIZE,B,
     +                           MATRIX_SIZE,BETA,C_OPTIM,MATRIX_SIZE)
*
*     Timed benchmark
      CALL CPU_TIME(START_TIME)
      DO RUN = 1, NRUNS
          CALL DGEMM_ALPHA_OPTIMIZED('N','N',MATRIX_SIZE,MATRIX_SIZE,
     +                               MATRIX_SIZE,ALPHA,A,MATRIX_SIZE,B,
     +                               MATRIX_SIZE,BETA,C_OPTIM,
     +                               MATRIX_SIZE)
      END DO
      CALL CPU_TIME(END_TIME)
      TIME_OPTIM = END_TIME - START_TIME
*
*     Calculate performance metrics
      GFLOPS_OPTIM = (FLOPS_4X4 * NRUNS) / (TIME_OPTIM * 1.0D+9)
      SPEEDUP_OPTIM = TIME_DGEMM / TIME_OPTIM
*
*     Calculate numerical accuracy
      MAX_ERROR = ZERO
      DO J = 1, MATRIX_SIZE
          DO I = 1, MATRIX_SIZE
              ERROR_OPTIM = ABS(C_OPTIM(I,J) - C_DGEMM(I,J))
              IF (ERROR_OPTIM .GT. MAX_ERROR) THEN
                  MAX_ERROR = ERROR_OPTIM
              END IF
          END DO
      END DO
*
      WRITE(*,*) ' Optimized AlphaTensor Results:'
      WRITE(*,*) '   Total time (s):', TIME_OPTIM
      WRITE(*,*) '   Performance (GFLOPS):', GFLOPS_OPTIM
      WRITE(*,*) '   Speedup vs DGEMM:', SPEEDUP_OPTIM
      WRITE(*,*) '   Max error vs DGEMM:', MAX_ERROR
*
*     ====================================================
*     SUMMARY COMPARISON
*     ====================================================
      WRITE(*,*) ''
      WRITE(*,*) '======================================================='
      WRITE(*,*) '                    PERFORMANCE SUMMARY'
      WRITE(*,*) '======================================================='
      WRITE(*,*) ' Algorithm              | Time (s) | GFLOPS | Speedup'
      WRITE(*,*) '------------------------|----------|--------|--------'
      WRITE(*,'(1X,A,F8.4,A,F7.2,A,F7.2)')
     +    'Standard DGEMM         |', TIME_DGEMM, ' |', GFLOPS_DGEMM,
     +    ' |   1.00'
      WRITE(*,'(1X,A,F8.4,A,F7.2,A,F7.2)')
     +    'Original AlphaTensor   |', TIME_ALPHA, ' |', GFLOPS_ALPHA,
     +    ' |', SPEEDUP_ALPHA
      WRITE(*,'(1X,A,F8.4,A,F7.2,A,F7.2)')
     +    'Optimized AlphaTensor  |', TIME_OPTIM, ' |', GFLOPS_OPTIM,
     +    ' |', SPEEDUP_OPTIM
      WRITE(*,*) '======================================================='
*
*     Performance Analysis
      WRITE(*,*) ''
      WRITE(*,*) ' ANALYSIS:'
      IF (SPEEDUP_ALPHA .LT. 1.0D+0) THEN
          WRITE(*,*) ' - Original AlphaTensor is SLOWER than DGEMM'
          WRITE(*,*) '   (Likely due to logging overhead)'
      ELSE
          WRITE(*,*) ' - Original AlphaTensor achieves speedup!'
      END IF
*
      IF (SPEEDUP_OPTIM .GT. SPEEDUP_ALPHA) THEN
          WRITE(*,*) ' - Optimized version shows improvement'
          WRITE(*,'(1X,A,F6.2,A)')
     +        ' - Optimization gain: ', SPEEDUP_OPTIM/SPEEDUP_ALPHA,
     +        'x faster'
      END IF
*
      IF (SPEEDUP_OPTIM .GT. 1.0D+0) THEN
          WRITE(*,*) ' - AlphaTensor achieves target speedup!'
      ELSE
          WRITE(*,*) ' - Further optimization needed'
          WRITE(*,*) '   Recommendations:'
          WRITE(*,*) '   1. Reduce memory access overhead'
          WRITE(*,*) '   2. Improve cache locality'
          WRITE(*,*) '   3. Consider vectorization'
      END IF
*
      WRITE(*,*) '======================================================='
*
      END
*
*     Simple CPU timer function
      DOUBLE PRECISION FUNCTION CPU_TIME_FUNC()
      DOUBLE PRECISION TIME
      CALL CPU_TIME(TIME)
      CPU_TIME_FUNC = TIME
      RETURN
      END
