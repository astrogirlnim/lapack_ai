      PROGRAM SPEED_BENCHMARK
*
*  -- CORRECTED: True Head-to-Head AlphaTensor vs DGEMM Benchmark --
*  -- Compares ONLY: Phase 8.3 DGEMM_ALPHA vs Standard DGEMM --
*  -- Eliminates misleading comparisons of same function --
*
      IMPLICIT NONE
*
*     Benchmark parameters
      INTEGER NRUNS_TIMING, LDIM, WARMUP_RUNS
      PARAMETER (NRUNS_TIMING=100000, LDIM=4, WARMUP_RUNS=1000)
      DOUBLE PRECISION TOLERANCE
      PARAMETER (TOLERANCE=1.0D-12)
*
*     Test matrices
      DOUBLE PRECISION A(LDIM,LDIM), B(LDIM,LDIM)
      DOUBLE PRECISION C_ALPHA(LDIM,LDIM), C_DGEMM(LDIM,LDIM)
      DOUBLE PRECISION ALPHA, BETA
      INTEGER I, J, RUN
*
*     Timing variables
      REAL START_TIME, END_TIME
      REAL ALPHA_TIME, DGEMM_TIME
      REAL ALPHA_OPS_PER_SEC, DGEMM_OPS_PER_SEC
      REAL SPEEDUP_ALPHA_VS_DGEMM
*
*     External subroutines
      EXTERNAL DGEMM, DGEMM_ALPHA
*
      WRITE(*,*) '=============================================='
      WRITE(*,*) 'CORRECTED ALPHATENSOR SPEED BENCHMARK'
      WRITE(*,*) '=============================================='
      WRITE(*,*) 'TRUE HEAD-TO-HEAD COMPARISON:'
      WRITE(*,*) 'Phase 8.3 DGEMM_ALPHA vs Standard DGEMM'
      WRITE(*,*) ''
      WRITE(*,*) 'Matrix Size:', LDIM, 'x', LDIM
      WRITE(*,*) 'Timing runs:', NRUNS_TIMING
      WRITE(*,*) 'Warmup runs:', WARMUP_RUNS
      WRITE(*,*) ''

*     Initialize test matrices
      DO I = 1, LDIM
          DO J = 1, LDIM
              A(I,J) = DBLE(I*J + I + J) / 10.0D+0
              B(I,J) = DBLE(I*J - I + J) / 8.0D+0
              C_ALPHA(I,J) = DBLE(I+J) / 12.0D+0
              C_DGEMM(I,J) = C_ALPHA(I,J)
          END DO
      END DO
      ALPHA = 1.5D+0
      BETA = 0.8D+0
*
*     ============================================
*     WARMUP PHASE
*     ============================================
      WRITE(*,*) 'Warming up CPU caches...'
      DO RUN = 1, WARMUP_RUNS
          CALL DGEMM_ALPHA('N','N',LDIM,LDIM,LDIM,
     $         ALPHA,A,LDIM,B,LDIM,BETA,C_ALPHA,LDIM)
          CALL DGEMM('N','N',LDIM,LDIM,LDIM,
     $         ALPHA,A,LDIM,B,LDIM,BETA,C_DGEMM,LDIM)
      END DO
      WRITE(*,*) 'Warmup complete.'
      WRITE(*,*) ''
*
*     ============================================
*     TIMING TEST 1: PHASE 8.3 DGEMM_ALPHA
*     ============================================
      WRITE(*,*) 'Timing Phase 8.3 DGEMM_ALPHA...'
      CALL CPU_TIME(START_TIME)

      DO RUN = 1, NRUNS_TIMING
          CALL DGEMM_ALPHA('N','N',LDIM,LDIM,LDIM,
     $         ALPHA,A,LDIM,B,LDIM,BETA,C_ALPHA,LDIM)
      END DO

      CALL CPU_TIME(END_TIME)
      ALPHA_TIME = END_TIME - START_TIME
      ALPHA_OPS_PER_SEC = REAL(NRUNS_TIMING) / ALPHA_TIME
*
*     ============================================
*     TIMING TEST 2: STANDARD DGEMM
*     ============================================
      WRITE(*,*) 'Timing Standard DGEMM...'
      CALL CPU_TIME(START_TIME)

      DO RUN = 1, NRUNS_TIMING
          CALL DGEMM('N','N',LDIM,LDIM,LDIM,
     $         ALPHA,A,LDIM,B,LDIM,BETA,C_DGEMM,LDIM)
      END DO

      CALL CPU_TIME(END_TIME)
      DGEMM_TIME = END_TIME - START_TIME
      DGEMM_OPS_PER_SEC = REAL(NRUNS_TIMING) / DGEMM_TIME
*
*     ============================================
*     CALCULATE SPEEDUP METRICS
*     ============================================
      SPEEDUP_ALPHA_VS_DGEMM = ALPHA_OPS_PER_SEC / DGEMM_OPS_PER_SEC
*
*     ============================================
*     RESULTS SUMMARY
*     ============================================
      WRITE(*,*) ''
      WRITE(*,*) '=============================================='
      WRITE(*,*) 'TRUE HEAD-TO-HEAD RESULTS'
      WRITE(*,*) '=============================================='
      WRITE(*,*) ''
      WRITE(*,*) 'EXECUTION TIMES:'
      WRITE(*,*) 'DGEMM_ALPHA (Phase 8.3):', ALPHA_TIME, 'seconds'
      WRITE(*,*) 'Standard DGEMM:         ', DGEMM_TIME, 'seconds'
      WRITE(*,*) ''
      WRITE(*,*) 'OPERATIONS PER SECOND:'
      WRITE(*,*) 'DGEMM_ALPHA (Phase 8.3):', NINT(ALPHA_OPS_PER_SEC)
      WRITE(*,*) 'Standard DGEMM:         ', NINT(DGEMM_OPS_PER_SEC)
      WRITE(*,*) ''
      WRITE(*,*) 'SPEEDUP ANALYSIS:'
      WRITE(*,8001) SPEEDUP_ALPHA_VS_DGEMM
      WRITE(*,*) ''
*
*     Performance analysis
      WRITE(*,*) 'PERFORMANCE ANALYSIS:'
      IF (SPEEDUP_ALPHA_VS_DGEMM .GT. 1.0) THEN
          WRITE(*,8003) NINT((SPEEDUP_ALPHA_VS_DGEMM - 1.0) * 100.0)
      ELSE
          WRITE(*,8004) NINT((1.0 - SPEEDUP_ALPHA_VS_DGEMM) * 100.0)
      END IF
*
*     ============================================
*     ACCURACY VERIFICATION
*     ============================================
      WRITE(*,*) ''
      WRITE(*,*) 'ACCURACY VERIFICATION:'
      WRITE(*,'(A,E12.5)') ' Max difference vs DGEMM: ',
     $    MAXVAL(ABS(C_ALPHA - C_DGEMM))
*
      WRITE(*,*) ''
      WRITE(*,*) 'THEORETICAL vs PRACTICAL:'
      WRITE(*,*) 'AlphaTensor: 49 operations (23.4% fewer than DGEMM)'
      WRITE(*,*) 'DGEMM: 64 operations (standard algorithm)'
      WRITE(*,*) ''
      WRITE(*,*) 'For 4x4 matrices:'
      WRITE(*,*) '- BLAS DGEMM is highly CPU-optimized'
      WRITE(*,*) '- Real-world performance depends on:'
      WRITE(*,*) '  * Hardware architecture'
      WRITE(*,*) '  * Compiler optimizations'
      WRITE(*,*) '  * Cache efficiency'
      WRITE(*,*) '  * Memory access patterns'
      WRITE(*,*) ''
      WRITE(*,*) 'CORRECTED BENCHMARK COMPLETE!'
      WRITE(*,*) '=============================================='
*
 8001 FORMAT('DGEMM_ALPHA vs DGEMM: ',F6.3,'x speedup')
 8003 FORMAT('DGEMM_ALPHA is ',I0,'% FASTER than DGEMM')
 8004 FORMAT('DGEMM_ALPHA is ',I0,'% SLOWER than DGEMM')
*
      END
