      PROGRAM BENCHMARK_DGEMM_ALPHA
*
*  Phase 5.2 CPU Performance Benchmark
*  Measure execution time for 4x4 matrices
*  Compare AlphaTensor vs standard DGEMM performance
*
      IMPLICIT NONE
*
      INTEGER NRUNS, LDIM, I, J, RUN
      PARAMETER (NRUNS=10000, LDIM=4)
      DOUBLE PRECISION ALPHA, BETA
      DOUBLE PRECISION START_TIME, END_TIME
      DOUBLE PRECISION ALPHA_TIME, STD_TIME
      DOUBLE PRECISION SPEEDUP, ALPHA_OPS, STD_OPS
      DOUBLE PRECISION MAX_ERROR, ERROR
*
      DOUBLE PRECISION A(LDIM,LDIM), B(LDIM,LDIM)
      DOUBLE PRECISION C_ALPHA(LDIM,LDIM), C_STD(LDIM,LDIM)
*
      EXTERNAL DGEMM_ALPHA, DGEMM, CPU_TIME
*
      WRITE(*,*) '================================================='
      WRITE(*,*) 'PHASE 5.2 CPU PERFORMANCE BENCHMARK'
      WRITE(*,*) 'AlphaTensor vs Standard DGEMM Timing Analysis'
      WRITE(*,*) '================================================='
      WRITE(*,*) 'Matrix Size: 4x4'
      WRITE(*,*) 'Benchmark runs:', NRUNS
      WRITE(*,*) 'Target: 10-20% improvement over DGEMM'
      WRITE(*,*) ''
*
*     Initialize test matrices
      DO I = 1, LDIM
          DO J = 1, LDIM
              A(I,J) = DBLE(I*J) / 10.0D+0
              B(I,J) = DBLE(I+J) / 5.0D+0
          END DO
      END DO
      ALPHA = 1.5D+0
      BETA = 0.5D+0
*
*     Benchmark Optimized AlphaTensor
      WRITE(*,*) 'Benchmarking Optimized AlphaTensor...'
      CALL CPU_TIME(START_TIME)
      DO RUN = 1, NRUNS
          DO I = 1, LDIM
              DO J = 1, LDIM
                  C_ALPHA(I,J) = DBLE(I+J)
              END DO
          END DO
          CALL DGEMM_ALPHA('N','N',LDIM,LDIM,LDIM,
     $         ALPHA,A,LDIM,B,LDIM,BETA,C_ALPHA,LDIM)
      END DO
      CALL CPU_TIME(END_TIME)
      ALPHA_TIME = END_TIME - START_TIME
      ALPHA_OPS = DBLE(NRUNS) / ALPHA_TIME
*
*     Benchmark Standard DGEMM
      WRITE(*,*) 'Benchmarking Standard DGEMM...'
      CALL CPU_TIME(START_TIME)
      DO RUN = 1, NRUNS
          DO I = 1, LDIM
              DO J = 1, LDIM
                  C_STD(I,J) = DBLE(I+J)
              END DO
          END DO
          CALL DGEMM('N','N',LDIM,LDIM,LDIM,ALPHA,A,LDIM,
     $              B,LDIM,BETA,C_STD,LDIM)
      END DO
      CALL CPU_TIME(END_TIME)
      STD_TIME = END_TIME - START_TIME
      STD_OPS = DBLE(NRUNS) / STD_TIME
*
*     Accuracy validation
      WRITE(*,*) 'Validating numerical accuracy...'
      MAX_ERROR = 0.0D+0
      DO I = 1, LDIM
          DO J = 1, LDIM
              ERROR = ABS(C_ALPHA(I,J) - C_STD(I,J))
              IF (ERROR .GT. MAX_ERROR) MAX_ERROR = ERROR
          END DO
      END DO
*
*     Performance analysis
      SPEEDUP = STD_TIME / ALPHA_TIME
*
      WRITE(*,*) ''
      WRITE(*,*) '================================================='
      WRITE(*,*) 'PHASE 5.2 BENCHMARK RESULTS'
      WRITE(*,*) '================================================='
      WRITE(*,*) ''
      WRITE(*,*) 'TIMING RESULTS:'
      WRITE(*,*) 'Optimized AlphaTensor time:', ALPHA_TIME, ' sec'
      WRITE(*,*) 'Standard DGEMM time:       ', STD_TIME, ' sec'
      WRITE(*,*) ''
      WRITE(*,*) 'OPERATIONS PER SECOND:'
      WRITE(*,*) 'Optimized AlphaTensor:', ALPHA_OPS, ' ops/sec'
      WRITE(*,*) 'Standard DGEMM:       ', STD_OPS, ' ops/sec'
      WRITE(*,*) ''
      WRITE(*,*) 'SPEEDUP ANALYSIS:'
      WRITE(*,*) 'Optimized vs Standard DGEMM:', SPEEDUP, 'x'
      WRITE(*,*) 'Performance improvement:',
     $          (SPEEDUP-1.0D+0)*100.0D+0, '%'
*
*     Target validation
      WRITE(*,*) ''
      WRITE(*,*) 'TARGET VALIDATION:'
      IF (SPEEDUP .GE. 1.10D+0) THEN
          WRITE(*,*) 'TARGET ACHIEVED: >=10% improvement'
          IF (SPEEDUP .GE. 1.20D+0) THEN
              WRITE(*,*) 'EXCEEDS TARGET: >=20% improvement!'
          END IF
      ELSE
          WRITE(*,*) 'TARGET MISSED: <10% improvement'
      END IF
*
*     Accuracy validation
      WRITE(*,*) ''
      WRITE(*,*) 'ACCURACY VALIDATION:'
      WRITE(*,*) 'Maximum numerical error:', MAX_ERROR
      IF (MAX_ERROR .LT. 1.0D-12) THEN
          WRITE(*,*) 'ACCURACY: PASSED (<1e-12)'
      ELSE
          WRITE(*,*) 'ACCURACY: FAILED (>=1e-12)'
      END IF
*
*     Algorithm analysis
      WRITE(*,*) ''
      WRITE(*,*) 'ALGORITHM ANALYSIS:'
      WRITE(*,*) 'AlphaTensor operations: 49'
      WRITE(*,*) 'Standard DGEMM operations: 64'
      WRITE(*,*) 'Theoretical improvement: 23.4%'
      WRITE(*,*) ''
      WRITE(*,*) '================================================='
      WRITE(*,*) 'PHASE 5.2 BENCHMARK COMPLETED'
      WRITE(*,*) '================================================='
*
      END
