      PROGRAM REALISTIC_BENCHMARK
*
*  -- Realistic AlphaTensor vs DGEMM Performance Comparison --
*
      IMPLICIT NONE
*
*     .. Parameters ..
      INTEGER NRUNS, MATRIX_SIZE
      PARAMETER (NRUNS=100000, MATRIX_SIZE=4)
      DOUBLE PRECISION ONE, ZERO
      PARAMETER (ONE=1.0D+0, ZERO=0.0D+0)
*     ..
*     .. Local Arrays ..
      DOUBLE PRECISION A(MATRIX_SIZE,MATRIX_SIZE)
      DOUBLE PRECISION B(MATRIX_SIZE,MATRIX_SIZE)
      DOUBLE PRECISION C_DGEMM(MATRIX_SIZE,MATRIX_SIZE)
      DOUBLE PRECISION C_ORIG(MATRIX_SIZE,MATRIX_SIZE)
      DOUBLE PRECISION C_OPT(MATRIX_SIZE,MATRIX_SIZE)
*     ..
*     .. Local Scalars ..
      INTEGER I, J, RUN
      DOUBLE PRECISION ALPHA, BETA
      DOUBLE PRECISION START_TIME, END_TIME
      DOUBLE PRECISION TIME_DGEMM, TIME_ORIG, TIME_OPT
      DOUBLE PRECISION OPS_DGEMM, OPS_ORIG, OPS_OPT
      DOUBLE PRECISION SPEEDUP_ORIG, SPEEDUP_OPT, SPEEDUP_GAIN
      DOUBLE PRECISION ERROR_ORIG, ERROR_OPT, MAX_ERR_ORIG, MAX_ERR_OPT
      DOUBLE PRECISION FLOPS_4X4, GFLOPS_DGEMM, GFLOPS_ORIG, GFLOPS_OPT
*     ..
*     .. External Subroutines ..
      EXTERNAL DGEMM, DGEMM_ALPHA, DGEMM_ALPHA_OPTIMIZED
*     ..

      WRITE(*,*) '================================================='
      WRITE(*,*) '    REALISTIC ALPHATENSOR vs DGEMM BENCHMARK'
      WRITE(*,*) '================================================='
      WRITE(*,*) 'BASELINE: Standard DGEMM (industry reference)'
      WRITE(*,*) 'TEST 1:   Original AlphaTensor (49 ops + logs)'
      WRITE(*,*) 'TEST 2:   Memory-Optimized AlphaTensor'
      WRITE(*,*) ''
      WRITE(*,*) 'Matrix Size: 4x4'
      WRITE(*,*) 'Iterations:', NRUNS
      WRITE(*,*) '================================================='

*     Initialize test data
      ALPHA = 1.5D+0
      BETA = 0.25D+0

*     Create identical test matrices
      DO J = 1, MATRIX_SIZE
          DO I = 1, MATRIX_SIZE
              A(I,J) = DBLE(I + 2*J - 1) / 7.0D+0
              B(I,J) = DBLE(3*I - J + 5) / 11.0D+0
          END DO
      END DO

*     Calculate theoretical FLOPS
      FLOPS_4X4 = 2.0D+0 * MATRIX_SIZE * MATRIX_SIZE * MATRIX_SIZE

*     ================================================
*     BENCHMARK 1: Standard DGEMM (Baseline)
*     ================================================
      WRITE(*,*) ''
      WRITE(*,*) 'Testing Standard DGEMM...'

*     Initialize result matrix
      DO J = 1, MATRIX_SIZE
          DO I = 1, MATRIX_SIZE
              C_DGEMM(I,J) = 0.1D+0 * DBLE(I * J)
          END DO
      END DO

*     Warm-up runs
      DO RUN = 1, 1000
          CALL DGEMM('N','N',MATRIX_SIZE,MATRIX_SIZE,MATRIX_SIZE,ALPHA,
     +               A,MATRIX_SIZE,B,MATRIX_SIZE,BETA,C_DGEMM,
     +               MATRIX_SIZE)
      END DO

*     Timed benchmark - DGEMM baseline
      CALL CPU_TIME(START_TIME)
      DO RUN = 1, NRUNS
          CALL DGEMM('N','N',MATRIX_SIZE,MATRIX_SIZE,MATRIX_SIZE,ALPHA,
     +               A,MATRIX_SIZE,B,MATRIX_SIZE,BETA,C_DGEMM,
     +               MATRIX_SIZE)
      END DO
      CALL CPU_TIME(END_TIME)
      TIME_DGEMM = END_TIME - START_TIME

      OPS_DGEMM = DBLE(NRUNS) / TIME_DGEMM
      GFLOPS_DGEMM = (FLOPS_4X4 * DBLE(NRUNS)) / (TIME_DGEMM * 1.0D+9)

      WRITE(*,*) 'DGEMM Results:'
      WRITE(*,*) '  Time (s):', TIME_DGEMM
      WRITE(*,*) '  Ops/sec:', OPS_DGEMM
      WRITE(*,*) '  GFLOPS: ', GFLOPS_DGEMM

*     ================================================
*     BENCHMARK 2: Original AlphaTensor
*     ================================================
      WRITE(*,*) ''
      WRITE(*,*) 'Testing Original AlphaTensor...'

*     Initialize result matrix (same as DGEMM)
      DO J = 1, MATRIX_SIZE
          DO I = 1, MATRIX_SIZE
              C_ORIG(I,J) = 0.1D+0 * DBLE(I * J)
          END DO
      END DO

*     Warm-up runs
      DO RUN = 1, 1000
          CALL DGEMM_ALPHA('N','N',MATRIX_SIZE,MATRIX_SIZE,MATRIX_SIZE,
     +                     ALPHA,A,MATRIX_SIZE,B,MATRIX_SIZE,BETA,
     +                     C_ORIG,MATRIX_SIZE)
      END DO

*     Timed benchmark - Original AlphaTensor
      CALL CPU_TIME(START_TIME)
      DO RUN = 1, NRUNS
          CALL DGEMM_ALPHA('N','N',MATRIX_SIZE,MATRIX_SIZE,MATRIX_SIZE,
     +                     ALPHA,A,MATRIX_SIZE,B,MATRIX_SIZE,BETA,
     +                     C_ORIG,MATRIX_SIZE)
      END DO
      CALL CPU_TIME(END_TIME)
      TIME_ORIG = END_TIME - START_TIME

      OPS_ORIG = DBLE(NRUNS) / TIME_ORIG
      SPEEDUP_ORIG = OPS_ORIG / OPS_DGEMM
      GFLOPS_ORIG = (FLOPS_4X4 * DBLE(NRUNS)) / (TIME_ORIG * 1.0D+9)

      WRITE(*,*) 'Original AlphaTensor Results:'
      WRITE(*,*) '  Time (s):', TIME_ORIG
      WRITE(*,*) '  Ops/sec:', OPS_ORIG
      WRITE(*,*) '  GFLOPS: ', GFLOPS_ORIG
      WRITE(*,*) '  vs DGEMM:', SPEEDUP_ORIG, 'x'

*     ================================================
*     BENCHMARK 3: Memory-Optimized AlphaTensor
*     ================================================
      WRITE(*,*) ''
      WRITE(*,*) 'Testing Memory-Optimized AlphaTensor...'

*     Initialize result matrix (same as others)
      DO J = 1, MATRIX_SIZE
          DO I = 1, MATRIX_SIZE
              C_OPT(I,J) = 0.1D+0 * DBLE(I * J)
          END DO
      END DO

*     Warm-up runs
      DO RUN = 1, 1000
          CALL DGEMM_ALPHA_OPTIMIZED('N','N',MATRIX_SIZE,MATRIX_SIZE,
     +                               MATRIX_SIZE,ALPHA,A,MATRIX_SIZE,B,
     +                               MATRIX_SIZE,BETA,C_OPT,MATRIX_SIZE)
      END DO

*     Timed benchmark - Memory-Optimized AlphaTensor
      CALL CPU_TIME(START_TIME)
      DO RUN = 1, NRUNS
          CALL DGEMM_ALPHA_OPTIMIZED('N','N',MATRIX_SIZE,MATRIX_SIZE,
     +                               MATRIX_SIZE,ALPHA,A,MATRIX_SIZE,B,
     +                               MATRIX_SIZE,BETA,C_OPT,MATRIX_SIZE)
      END DO
      CALL CPU_TIME(END_TIME)
      TIME_OPT = END_TIME - START_TIME

      OPS_OPT = DBLE(NRUNS) / TIME_OPT
      SPEEDUP_OPT = OPS_OPT / OPS_DGEMM
      SPEEDUP_GAIN = OPS_OPT / OPS_ORIG
      GFLOPS_OPT = (FLOPS_4X4 * DBLE(NRUNS)) / (TIME_OPT * 1.0D+9)

      WRITE(*,*) 'Memory-Optimized AlphaTensor Results:'
      WRITE(*,*) '  Time (s):', TIME_OPT
      WRITE(*,*) '  Ops/sec:', OPS_OPT
      WRITE(*,*) '  GFLOPS: ', GFLOPS_OPT
      WRITE(*,*) '  vs DGEMM:', SPEEDUP_OPT, 'x'
      WRITE(*,*) '  vs Original:', SPEEDUP_GAIN, 'x'

*     ================================================
*     ACCURACY VERIFICATION
*     ================================================
      MAX_ERR_ORIG = ZERO
      MAX_ERR_OPT = ZERO
      DO J = 1, MATRIX_SIZE
          DO I = 1, MATRIX_SIZE
*             Compare Original vs DGEMM
              ERROR_ORIG = ABS(C_ORIG(I,J) - C_DGEMM(I,J))
              IF (ERROR_ORIG .GT. MAX_ERR_ORIG) THEN
                  MAX_ERR_ORIG = ERROR_ORIG
              END IF
*             Compare Optimized vs DGEMM
              ERROR_OPT = ABS(C_OPT(I,J) - C_DGEMM(I,J))
              IF (ERROR_OPT .GT. MAX_ERR_OPT) THEN
                  MAX_ERR_OPT = ERROR_OPT
              END IF
          END DO
      END DO

*     ================================================
*     PERFORMANCE SUMMARY
*     ================================================
      WRITE(*,*) ''
      WRITE(*,*) '================================================='
      WRITE(*,*) '           BENCHMARK RESULTS SUMMARY'
      WRITE(*,*) '================================================='
      WRITE(*,*) 'Algorithm            | Time    | vs DGEMM | GFLOPS'
      WRITE(*,*) '---------------------|---------|----------|-------'
      WRITE(*,'(1X,A,F8.4,A,F8.3,A,F7.2)')
     +    'DGEMM (Baseline)     |', TIME_DGEMM, ' |   1.000x |',
     +    GFLOPS_DGEMM
      WRITE(*,'(1X,A,F8.4,A,F8.3,A,F7.2)')
     +    'Original AlphaTensor |', TIME_ORIG, ' |', SPEEDUP_ORIG,
     +    'x |', GFLOPS_ORIG
      WRITE(*,'(1X,A,F8.4,A,F8.3,A,F7.2)')
     +    'Optimized AlphaTensor|', TIME_OPT, ' |', SPEEDUP_OPT,
     +    'x |', GFLOPS_OPT
      WRITE(*,*) '================================================='

*     Accuracy Results
      WRITE(*,*) ''
      WRITE(*,*) 'ACCURACY vs DGEMM:'
      WRITE(*,'(1X,A,ES12.5)') 'Original error:  ', MAX_ERR_ORIG
      WRITE(*,'(1X,A,ES12.5)') 'Optimized error: ', MAX_ERR_OPT

*     Performance Analysis
      WRITE(*,*) ''
      WRITE(*,*) 'ANALYSIS:'
      IF (SPEEDUP_OPT .GT. 1.05D+0) THEN
          WRITE(*,'(1X,A,F6.1,A)') 'SUCCESS: AlphaTensor ',
     +        (SPEEDUP_OPT-1.0D+0)*100.0D+0, '% faster than DGEMM!'
      ELSE IF (SPEEDUP_OPT .GT. 0.95D+0) THEN
          WRITE(*,*) 'COMPETITIVE: Matches DGEMM performance'
      ELSE
          WRITE(*,'(1X,A,F6.1,A)') 'SLOWER: AlphaTensor ',
     +        (1.0D+0-SPEEDUP_OPT)*100.0D+0, '% slower than DGEMM'
      END IF

      IF (SPEEDUP_GAIN .GT. 1.1D+0) THEN
          WRITE(*,'(1X,A,F6.1,A)') 'Phase 8.1 improved performance by ',
     +        (SPEEDUP_GAIN-1.0D+0)*100.0D+0, '%'
      END IF

      WRITE(*,*) ''
      WRITE(*,*) 'NEXT: Phase 8.2 Vectorization for DGEMM parity'
      WRITE(*,*) '================================================='

      END
