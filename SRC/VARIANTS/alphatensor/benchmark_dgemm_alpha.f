      PROGRAM BENCHMARK_DGEMM_ALPHA
*
*  -- Phase 5.2 CPU Performance Benchmark --
*  -- Measure execution time for 4x4 matrices --
*  -- Compare AlphaTensor vs standard DGEMM performance --
*  -- Target: 10-20% improvement validation --
*  -- Log timing results for all test runs --
*
      IMPLICIT NONE
*
*     .. Parameters ..
      INTEGER NRUNS_SMALL, NRUNS_LARGE
      PARAMETER (NRUNS_SMALL=1000, NRUNS_LARGE=100000)
      INTEGER LDIM
      PARAMETER (LDIM=4)
      DOUBLE PRECISION ONE, TWO
      PARAMETER (ONE=1.0D+0, TWO=2.0D+0)
*
*     .. Local Scalars ..
      INTEGER I, J, RUN
      DOUBLE PRECISION ALPHA, BETA
      DOUBLE PRECISION START_TIME, END_TIME
      DOUBLE PRECISION ALPHA_TIME, STD_TIME, ORIG_TIME
      DOUBLE PRECISION SPEEDUP_VS_STD, SPEEDUP_VS_ORIG
      DOUBLE PRECISION ALPHA_OPS_SEC, STD_OPS_SEC, ORIG_OPS_SEC
      DOUBLE PRECISION MAX_ERROR, ERROR
*
*     .. Local Arrays ..
      DOUBLE PRECISION A(LDIM,LDIM), B(LDIM,LDIM)
      DOUBLE PRECISION C_ALPHA(LDIM,LDIM), C_STD(LDIM,LDIM)
      DOUBLE PRECISION C_ORIG(LDIM,LDIM)
*
*     .. External Subroutines ..
      EXTERNAL DGEMM, DGEMM_ALPHA, DGEMM_ALPHA_OPTIMIZED
      EXTERNAL CPU_TIME
*
*     ===============================================
*     PHASE 5.2 CPU PERFORMANCE BENCHMARK
*     ===============================================
      WRITE(*,*) '================================================='
      WRITE(*,*) 'PHASE 5.2 CPU PERFORMANCE BENCHMARK'
      WRITE(*,*) 'AlphaTensor vs Standard DGEMM Timing Analysis'
      WRITE(*,*) '================================================='
      WRITE(*,*) 'Matrix Size: 4x4'
      WRITE(*,*) 'Warm-up runs:', NRUNS_SMALL
      WRITE(*,*) 'Benchmark runs:', NRUNS_LARGE
      WRITE(*,*) 'Target: 10-20% improvement over standard DGEMM'
      WRITE(*,*) ''
*
*     Initialize test matrices with realistic data
      DO I = 1, LDIM
          DO J = 1, LDIM
              A(I,J) = DBLE(I*J) / 10.0D+0
              B(I,J) = DBLE(I+J) / 5.0D+0
          END DO
      END DO
      ALPHA = 1.5D+0
      BETA = 0.5D+0
*
*     ===============================================
*     WARM-UP PHASE
*     ===============================================
      WRITE(*,*) 'Warming up CPU caches...'
      DO RUN = 1, NRUNS_SMALL
          DO I = 1, LDIM
              DO J = 1, LDIM
                  C_ALPHA(I,J) = DBLE(I+J)
                  C_STD(I,J) = DBLE(I+J)
                  C_ORIG(I,J) = DBLE(I+J)
              END DO
          END DO
          CALL DGEMM_ALPHA_OPTIMIZED('N','N',LDIM,LDIM,LDIM,ALPHA,
     \$                             A,LDIM,B,LDIM,BETA,C_ALPHA,LDIM)
          CALL DGEMM('N','N',LDIM,LDIM,LDIM,ALPHA,A,LDIM,B,LDIM,
     \$               BETA,C_STD,LDIM)
          CALL DGEMM_ALPHA('N','N',LDIM,LDIM,LDIM,ALPHA,A,LDIM,B,LDIM,
     \$                   BETA,C_ORIG,LDIM)
      END DO
*
*     ===============================================
*     BENCHMARK 1: OPTIMIZED ALPHATENSOR
*     ===============================================
      WRITE(*,*) 'Benchmarking Optimized AlphaTensor...'
      CALL CPU_TIME(START_TIME)
      DO RUN = 1, NRUNS_LARGE
          DO I = 1, LDIM
              DO J = 1, LDIM
                  C_ALPHA(I,J) = DBLE(I+J)
              END DO
          END DO
          CALL DGEMM_ALPHA_OPTIMIZED('N','N',LDIM,LDIM,LDIM,ALPHA,
     \$                             A,LDIM,B,LDIM,BETA,C_ALPHA,LDIM)
      END DO
      CALL CPU_TIME(END_TIME)
      ALPHA_TIME = END_TIME - START_TIME
      ALPHA_OPS_SEC = DBLE(NRUNS_LARGE) / ALPHA_TIME
*
*     ===============================================
*     BENCHMARK 2: STANDARD DGEMM
*     ===============================================
      WRITE(*,*) 'Benchmarking Standard DGEMM...'
      CALL CPU_TIME(START_TIME)
      DO RUN = 1, NRUNS_LARGE
          DO I = 1, LDIM
              DO J = 1, LDIM
                  C_STD(I,J) = DBLE(I+J)
              END DO
          END DO
          CALL DGEMM('N','N',LDIM,LDIM,LDIM,ALPHA,A,LDIM,B,LDIM,
     \$               BETA,C_STD,LDIM)
      END DO
      CALL CPU_TIME(END_TIME)
      STD_TIME = END_TIME - START_TIME
      STD_OPS_SEC = DBLE(NRUNS_LARGE) / STD_TIME
*
*     ===============================================
*     BENCHMARK 3: ORIGINAL ALPHATENSOR (with logging)
*     ===============================================
      WRITE(*,*) 'Benchmarking Original AlphaTensor (with logging)...'
      CALL CPU_TIME(START_TIME)
      DO RUN = 1, NRUNS_SMALL
          DO I = 1, LDIM
              DO J = 1, LDIM
                  C_ORIG(I,J) = DBLE(I+J)
              END DO
          END DO
          CALL DGEMM_ALPHA('N','N',LDIM,LDIM,LDIM,ALPHA,A,LDIM,B,LDIM,
     \$                   BETA,C_ORIG,LDIM)
      END DO
      CALL CPU_TIME(END_TIME)
*     Scale to equivalent iterations
      ORIG_TIME = (END_TIME - START_TIME) * DBLE(NRUNS_LARGE) /
     \$            DBLE(NRUNS_SMALL)
      ORIG_OPS_SEC = DBLE(NRUNS_LARGE) / ORIG_TIME
*
*     ===============================================
*     ACCURACY VALIDATION
*     ===============================================
      WRITE(*,*) 'Validating numerical accuracy...'
      MAX_ERROR = 0.0D+0
      DO I = 1, LDIM
          DO J = 1, LDIM
              ERROR = ABS(C_ALPHA(I,J) - C_STD(I,J))
              IF (ERROR .GT. MAX_ERROR) MAX_ERROR = ERROR
          END DO
      END DO
*
*     ===============================================
*     PERFORMANCE ANALYSIS & RESULTS
*     ===============================================
      SPEEDUP_VS_STD = STD_TIME / ALPHA_TIME
      SPEEDUP_VS_ORIG = ORIG_TIME / ALPHA_TIME
*
      WRITE(*,*) ''
      WRITE(*,*) '================================================='
      WRITE(*,*) 'PHASE 5.2 BENCHMARK RESULTS'
      WRITE(*,*) '================================================='
      WRITE(*,*) ''
      WRITE(*,*) 'TIMING RESULTS:'
      WRITE(*,*) '---------------'
      WRITE(*,'(A,F12.6,A)') ' Optimized AlphaTensor: ', ALPHA_TIME,
     \$                      ' seconds'
      WRITE(*,'(A,F12.6,A)') ' Standard DGEMM:        ', STD_TIME,
     \$                      ' seconds'
      WRITE(*,'(A,F12.6,A)') ' Original AlphaTensor:  ', ORIG_TIME,
     \$                      ' seconds (scaled)'
      WRITE(*,*) ''
      WRITE(*,*) 'OPERATIONS PER SECOND:'
      WRITE(*,*) '----------------------'
      WRITE(*,'(A,F15.0,A)') ' Optimized AlphaTensor: ', ALPHA_OPS_SEC,
     \$                      ' ops/sec'
      WRITE(*,'(A,F15.0,A)') ' Standard DGEMM:        ', STD_OPS_SEC,
     \$                      ' ops/sec'
      WRITE(*,'(A,F15.0,A)') ' Original AlphaTensor:  ', ORIG_OPS_SEC,
     \$                      ' ops/sec'
      WRITE(*,*) ''
      WRITE(*,*) 'SPEEDUP ANALYSIS:'
      WRITE(*,*) '-----------------'
      WRITE(*,'(A,F8.3,A)') ' Optimized vs Standard DGEMM: ',
     \$                    SPEEDUP_VS_STD, 'x'
      WRITE(*,'(A,F8.3,A)') ' Optimized vs Original:       ',
     \$                    SPEEDUP_VS_ORIG, 'x'
      WRITE(*,'(A,F8.3,A)') ' Standard vs Original:        ',
     \$                    ORIG_TIME/STD_TIME, 'x'
*
*     Performance target validation
      WRITE(*,*) ''
      WRITE(*,*) 'TARGET VALIDATION:'
      WRITE(*,*) '------------------'
      WRITE(*,'(A,F6.1,A)') ' Performance improvement: ',
     \$                   (SPEEDUP_VS_STD-1.0D+0)*100.0D+0, '%'
      IF (SPEEDUP_VS_STD .GE. 1.10D+0) THEN
          WRITE(*,*) ' ✅ TARGET ACHIEVED: >=10% improvement'
          IF (SPEEDUP_VS_STD .GE. 1.20D+0) THEN
              WRITE(*,*) ' 🎉 EXCEEDS TARGET: >=20% improvement!'
          END IF
      ELSE
          WRITE(*,*) ' ❌ TARGET MISSED: <10% improvement'
      END IF
*
*     Accuracy validation
      WRITE(*,*) ''
      WRITE(*,*) 'ACCURACY VALIDATION:'
      WRITE(*,*) '--------------------'
      WRITE(*,'(A,E15.6)') ' Maximum numerical error: ', MAX_ERROR
      IF (MAX_ERROR .LT. 1.0D-12) THEN
          WRITE(*,*) ' ✅ ACCURACY: PASSED (<1e-12)'
      ELSE
          WRITE(*,*) ' ❌ ACCURACY: FAILED (>=1e-12)'
      END IF
*
*     Algorithm efficiency analysis
      WRITE(*,*) ''
      WRITE(*,*) 'ALGORITHM ANALYSIS:'
      WRITE(*,*) '-------------------'
      WRITE(*,*) ' AlphaTensor operations: 49'
      WRITE(*,*) ' Standard DGEMM operations: 64'
      WRITE(*,'(A,F6.1,A)') ' Theoretical improvement: ',
     \$                   (1.0D+0 - 49.0D+0/64.0D+0)*100.0D+0, '%'
      WRITE(*,*) ''
      WRITE(*,*) '================================================='
      WRITE(*,*) 'PHASE 5.2 BENCHMARK COMPLETED'
      WRITE(*,*) '================================================='
*
      END
