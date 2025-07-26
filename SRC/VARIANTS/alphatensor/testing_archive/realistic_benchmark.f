      PROGRAM REALISTIC_BENCHMARK
*
*  -- CORRECTED: True Head-to-Head AlphaTensor vs DGEMM Benchmark --
*  -- Compares ONLY: Phase 8.3 DGEMM_ALPHA vs Standard DGEMM --
*  -- Eliminates misleading comparisons of same function --
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
      DOUBLE PRECISION C_ALPHA(MATRIX_SIZE,MATRIX_SIZE)
*     ..
*     .. Local Scalars ..
      INTEGER I, J, RUN
      DOUBLE PRECISION ALPHA, BETA
      DOUBLE PRECISION START_TIME, END_TIME
      DOUBLE PRECISION TIME_DGEMM, TIME_ALPHA
      DOUBLE PRECISION OPS_DGEMM, OPS_ALPHA
      DOUBLE PRECISION SPEEDUP_ALPHA, ERROR_ALPHA, MAX_ERR_ALPHA
      DOUBLE PRECISION FLOPS_4X4, GFLOPS_DGEMM, GFLOPS_ALPHA
*     ..
*     .. External Subroutines ..
      EXTERNAL DGEMM, DGEMM_ALPHA
*     ..

      WRITE(*,*) '================================================='
      WRITE(*,*) '    CORRECTED ALPHATENSOR vs DGEMM BENCHMARK'
      WRITE(*,*) '================================================='
      WRITE(*,*) 'TRUE HEAD-TO-HEAD COMPARISON:'
      WRITE(*,*) 'Phase 8.3 DGEMM_ALPHA vs Standard DGEMM'
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
              A(I,J) = DBLE((I-1)*MATRIX_SIZE + J) / 16.0D+0
              B(I,J) = DBLE(I*J) / 10.0D+0 + DBLE(I+J) / 20.0D+0
              C_DGEMM(I,J) = 0.1D+0 * DBLE(I * J)
              C_ALPHA(I,J) = C_DGEMM(I,J)
          END DO
      END DO

*     Calculate 4x4 matrix multiplication FLOPS
      FLOPS_4X4 = 2.0D+0 * DBLE(MATRIX_SIZE)**3

*     ================================================
*     BENCHMARK 1: Standard DGEMM (Reference)
*     ================================================
      WRITE(*,*) ''
      WRITE(*,*) 'Testing Standard DGEMM...'

*     Warm-up runs
      DO RUN = 1, 1000
          CALL DGEMM('N','N',MATRIX_SIZE,MATRIX_SIZE,MATRIX_SIZE,
     +               ALPHA,A,MATRIX_SIZE,B,MATRIX_SIZE,BETA,
     +               C_DGEMM,MATRIX_SIZE)
      END DO

*     Timed benchmark - Standard DGEMM
      CALL CPU_TIME(START_TIME)
      DO RUN = 1, NRUNS
          CALL DGEMM('N','N',MATRIX_SIZE,MATRIX_SIZE,MATRIX_SIZE,
     +               ALPHA,A,MATRIX_SIZE,B,MATRIX_SIZE,BETA,
     +               C_DGEMM,MATRIX_SIZE)
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
*     BENCHMARK 2: Phase 8.3 DGEMM_ALPHA
*     ================================================
      WRITE(*,*) ''
      WRITE(*,*) 'Testing Phase 8.3 DGEMM_ALPHA...'

*     Warm-up runs
      DO RUN = 1, 1000
          CALL DGEMM_ALPHA('N','N',MATRIX_SIZE,MATRIX_SIZE,MATRIX_SIZE,
     +                     ALPHA,A,MATRIX_SIZE,B,MATRIX_SIZE,BETA,
     +                     C_ALPHA,MATRIX_SIZE)
      END DO

*     Timed benchmark - Phase 8.3 DGEMM_ALPHA
      CALL CPU_TIME(START_TIME)
      DO RUN = 1, NRUNS
          CALL DGEMM_ALPHA('N','N',MATRIX_SIZE,MATRIX_SIZE,MATRIX_SIZE,
     +                     ALPHA,A,MATRIX_SIZE,B,MATRIX_SIZE,BETA,
     +                     C_ALPHA,MATRIX_SIZE)
      END DO
      CALL CPU_TIME(END_TIME)
      TIME_ALPHA = END_TIME - START_TIME

      OPS_ALPHA = DBLE(NRUNS) / TIME_ALPHA
      SPEEDUP_ALPHA = OPS_ALPHA / OPS_DGEMM
      GFLOPS_ALPHA = (FLOPS_4X4 * DBLE(NRUNS)) / (TIME_ALPHA * 1.0D+9)

      WRITE(*,*) 'DGEMM_ALPHA Results:'
      WRITE(*,*) '  Time (s):', TIME_ALPHA
      WRITE(*,*) '  Ops/sec:', OPS_ALPHA
      WRITE(*,*) '  GFLOPS: ', GFLOPS_ALPHA
      WRITE(*,*) '  vs DGEMM:', SPEEDUP_ALPHA, 'x'

*     ================================================
*     ACCURACY VERIFICATION
*     ================================================
      MAX_ERR_ALPHA = ZERO
      DO J = 1, MATRIX_SIZE
          DO I = 1, MATRIX_SIZE
*             Compare DGEMM_ALPHA vs DGEMM
              ERROR_ALPHA = ABS(C_ALPHA(I,J) - C_DGEMM(I,J))
              IF (ERROR_ALPHA .GT. MAX_ERR_ALPHA) THEN
                  MAX_ERR_ALPHA = ERROR_ALPHA
              END IF
          END DO
      END DO

*     ================================================
*     FINAL RESULTS SUMMARY
*     ================================================
      WRITE(*,*) ''
      WRITE(*,*) '================================================='
      WRITE(*,*) '           CORRECTED BENCHMARK SUMMARY'
      WRITE(*,*) '================================================='
      WRITE(*,*) 'Algorithm            | Time    | vs DGEMM | GFLOPS'
      WRITE(*,*) '---------------------|---------|----------|-------'
      WRITE(*,'(A,F8.4,A,F9.3,A,F8.3)') ' DGEMM (Baseline)     |',
     +    TIME_DGEMM, ' |   1.000x |', GFLOPS_DGEMM
      WRITE(*,'(A,F8.4,A,F9.3,A,F8.3)') ' DGEMM_ALPHA (Phase8.3)|',
     +    TIME_ALPHA, ' |', SPEEDUP_ALPHA, 'x |', GFLOPS_ALPHA
      WRITE(*,*) '================================================='
      WRITE(*,*) ''
      WRITE(*,*) 'ACCURACY vs DGEMM:'
      WRITE(*,'(A,E12.5)') ' DGEMM_ALPHA error: ', MAX_ERR_ALPHA
      WRITE(*,*) ''
      WRITE(*,*) 'ANALYSIS:'
      IF (SPEEDUP_ALPHA .GT. 1.0D+0) THEN
          WRITE(*,'(A,F5.1,A)') ' SUCCESS: DGEMM_ALPHA is ',
     +        (SPEEDUP_ALPHA-1.0D+0)*100.0D+0, '% faster than DGEMM!'
      ELSE
          WRITE(*,'(A,F5.1,A)') ' DGEMM_ALPHA is ',
     +        (1.0D+0-SPEEDUP_ALPHA)*100.0D+0, '% slower than DGEMM'
      END IF
      WRITE(*,*) ''
      WRITE(*,*) 'THEORETICAL: AlphaTensor uses 49 operations vs'
      WRITE(*,*) 'DGEMM 64 operations (23.4% theoretical reduction)'
      WRITE(*,*) ''
      WRITE(*,*) 'CORRECTED HEAD-TO-HEAD BENCHMARK COMPLETE!'
      WRITE(*,*) '================================================='

      END
