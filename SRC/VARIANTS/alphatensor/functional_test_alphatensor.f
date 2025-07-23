      PROGRAM FUNCTIONAL_TEST_ALPHATENSOR
*
*  -- Functional Testing for AlphaTensor DGEMM_ALPHA Implementation --
*  -- LAPACK AI Modernization Project --
*  -- Verifies: Algorithm triggering, correctness, and performance --
*
*     .. Parameters ..
      INTEGER            MAXDIM
      PARAMETER          ( MAXDIM = 10 )
      DOUBLE PRECISION   ZERO, ONE, TWO
      PARAMETER          ( ZERO = 0.0D+0, ONE = 1.0D+0, TWO = 2.0D+0 )
      DOUBLE PRECISION   TOLERANCE
      PARAMETER          ( TOLERANCE = 1.0D-12 )
*     ..
*     .. Local Scalars ..
      INTEGER            I, J, K, TRIALS, TRIAL
      DOUBLE PRECISION   ALPHA, BETA
      DOUBLE PRECISION   START_TIME, END_TIME, ALPHA_TIME, DGEMM_TIME
      DOUBLE PRECISION   SPEEDUP, MAX_ERROR, ERROR
      LOGICAL            TEST_PASSED
*     ..
*     .. Local Arrays ..
      DOUBLE PRECISION   A_4X4(4,4), B_4X4(4,4), C_ALPHA(4,4)
      DOUBLE PRECISION   C_DGEMM(4,4), C_TEMP(4,4)
      DOUBLE PRECISION   A_5X5(5,5), B_5X5(5,5), C_5X5(5,5)
*     ..
*     .. External Functions ..
      DOUBLE PRECISION   DSECND
      EXTERNAL           DSECND
*     ..
*     .. External Subroutines ..
      EXTERNAL           DGEMM_ALPHA, DGEMM
*     ..
*
      WRITE(*,*) '=================================================='
      WRITE(*,*) 'FUNCTIONAL TEST: AlphaTensor DGEMM_ALPHA'
      WRITE(*,*) '=================================================='
      WRITE(*,*)
*
*     === TEST 1: Algorithm Triggering Verification ===
*
      WRITE(*,*) 'TEST 1: Verifying AlphaTensor optimization'
      WRITE(*,*) '------------------------------------------'
*
*     Initialize 4x4 test matrices with known values
      DO J = 1, 4
          DO I = 1, 4
              A_4X4(I,J) = DBLE(I + J)
              B_4X4(I,J) = DBLE(I * J)
              C_ALPHA(I,J) = DBLE(I - J)
              C_TEMP(I,J) = C_ALPHA(I,J)
          END DO
      END DO
*
      ALPHA = 1.5D+0
      BETA = 0.5D+0
*
      WRITE(*,*) 'Calling DGEMM_ALPHA with 4x4 matrices:'
      WRITE(*,*) '(should trigger AlphaTensor optimization)'
      WRITE(*,*) 'Matrix dimensions: M=4, N=4, K=4'
      WRITE(*,*) 'ALPHA =', ALPHA, ', BETA =', BETA
      WRITE(*,*)
*
*     Call AlphaTensor implementation
      CALL DGEMM_ALPHA('N', 'N', 4, 4, 4, ALPHA, A_4X4, 4, B_4X4, 4,
     +                 BETA, C_ALPHA, 4)
*
      WRITE(*,*) 'AlphaTensor call completed.'
      WRITE(*,*) 'Check logs above for algorithm confirmation.'
      WRITE(*,*)
*
*     === TEST 2: Correctness Verification ===
*
      WRITE(*,*) 'TEST 2: Verifying numerical correctness'
      WRITE(*,*) '---------------------------------------'
*
*     Reset matrices for comparison
      DO J = 1, 4
          DO I = 1, 4
              C_DGEMM(I,J) = C_TEMP(I,J)
          END DO
      END DO
*
*     Call standard DGEMM for comparison
      CALL DGEMM('N', 'N', 4, 4, 4, ALPHA, A_4X4, 4, B_4X4, 4,
     +           BETA, C_DGEMM, 4)
*
*     Compare results
      MAX_ERROR = ZERO
      DO J = 1, 4
          DO I = 1, 4
              ERROR = ABS(C_ALPHA(I,J) - C_DGEMM(I,J))
              IF (ERROR .GT. MAX_ERROR) MAX_ERROR = ERROR
          END DO
      END DO
*
      WRITE(*,*) 'Max error between AlphaTensor and standard DGEMM:'
      WRITE(*,*) 'Max error =', MAX_ERROR
      WRITE(*,*) 'Tolerance =', TOLERANCE
*
      IF (MAX_ERROR .LE. TOLERANCE) THEN
          WRITE(*,*) 'PASSED: Numerical accuracy within tolerance'
          TEST_PASSED = .TRUE.
      ELSE
          WRITE(*,*) 'FAILED: Numerical accuracy exceeded tolerance'
          TEST_PASSED = .FALSE.
      END IF
      WRITE(*,*)
*
*     === TEST 3: Fallback Verification ===
*
      WRITE(*,*) 'TEST 3: Verifying fallback for non-4x4 matrices'
      WRITE(*,*) '-----------------------------------------------'
*
*     Initialize 5x5 matrices (should trigger fallback)
      DO J = 1, 5
          DO I = 1, 5
              A_5X5(I,J) = DBLE(I + J - 1)
              B_5X5(I,J) = DBLE(I * J)
              C_5X5(I,J) = DBLE(I - J + 1)
          END DO
      END DO
*
      WRITE(*,*) 'Calling DGEMM_ALPHA with 5x5 matrices:'
      WRITE(*,*) '(should use standard DGEMM fallback)'
      WRITE(*,*) 'Matrix dimensions: M=5, N=5, K=5'
      WRITE(*,*)
*
      CALL DGEMM_ALPHA('N', 'N', 5, 5, 5, ALPHA, A_5X5, 5, B_5X5, 5,
     +                 BETA, C_5X5, 5)
*
      WRITE(*,*) 'Non-4x4 call completed.'
      WRITE(*,*) 'Check logs above for fallback confirmation.'
      WRITE(*,*)
*
*     === TEST 4: Performance Benchmarking ===
*
      WRITE(*,*) 'TEST 4: Performance benchmarking (4x4 matrices)'
      WRITE(*,*) '----------------------------------------------'
*
      TRIALS = 10000
      WRITE(*,*) 'Running', TRIALS, ' trials of each algorithm...'
*
*     Benchmark AlphaTensor implementation
      START_TIME = DSECND()
      DO TRIAL = 1, TRIALS
          DO J = 1, 4
              DO I = 1, 4
                  C_ALPHA(I,J) = C_TEMP(I,J)
              END DO
          END DO
          CALL DGEMM_ALPHA('N', 'N', 4, 4, 4, ALPHA, A_4X4, 4, B_4X4,
     +                     4, BETA, C_ALPHA, 4)
      END DO
      END_TIME = DSECND()
      ALPHA_TIME = END_TIME - START_TIME
*
*     Benchmark standard DGEMM
      START_TIME = DSECND()
      DO TRIAL = 1, TRIALS
          DO J = 1, 4
              DO I = 1, 4
                  C_DGEMM(I,J) = C_TEMP(I,J)
              END DO
          END DO
          CALL DGEMM('N', 'N', 4, 4, 4, ALPHA, A_4X4, 4, B_4X4, 4,
     +               BETA, C_DGEMM, 4)
      END DO
      END_TIME = DSECND()
      DGEMM_TIME = END_TIME - START_TIME
*
*     Calculate speedup
      IF (DGEMM_TIME .GT. ZERO) THEN
          SPEEDUP = (DGEMM_TIME / ALPHA_TIME - ONE) * 100.0D+0
      ELSE
          SPEEDUP = ZERO
      END IF
*
      WRITE(*,*) 'Performance Results:'
      WRITE(*,*) 'AlphaTensor time:', ALPHA_TIME, ' seconds'
      WRITE(*,*) 'Standard DGEMM time:', DGEMM_TIME, ' seconds'
      WRITE(*,*) 'Speedup: ', SPEEDUP, '%'
*
      IF (SPEEDUP .GT. ZERO) THEN
          WRITE(*,*) 'RESULT: AlphaTensor is FASTER than DGEMM'
      ELSE
          WRITE(*,*) 'RESULT: AlphaTensor is SLOWER than DGEMM'
          WRITE(*,*) 'Note: May be due to implementation overhead'
      END IF
      WRITE(*,*)
*
*     === TEST 5: Transpose Fallback Verification ===
*
      WRITE(*,*) 'TEST 5: Verifying transpose fallback'
      WRITE(*,*) '------------------------------------'
*
      WRITE(*,*) 'Calling DGEMM_ALPHA with TRANSA=T:'
      WRITE(*,*) '(should trigger fallback)'
*
      DO J = 1, 4
          DO I = 1, 4
              C_ALPHA(I,J) = C_TEMP(I,J)
          END DO
      END DO
*
      CALL DGEMM_ALPHA('T', 'N', 4, 4, 4, ALPHA, A_4X4, 4, B_4X4, 4,
     +                 BETA, C_ALPHA, 4)
*
      WRITE(*,*) 'Transpose call completed.'
      WRITE(*,*) 'Check logs above for fallback confirmation.'
      WRITE(*,*)
*
*     === SUMMARY ===
*
      WRITE(*,*) '=================================================='
      WRITE(*,*) 'FUNCTIONAL TEST SUMMARY:'
      WRITE(*,*) '=================================================='
      WRITE(*,*) '1. Algorithm Triggering:'
      WRITE(*,*) '   Check logs for "Using AlphaTensor 4x4"'
      WRITE(*,*) '2. Numerical Correctness:'
      IF (TEST_PASSED) THEN
          WRITE(*,*) '   PASSED (error =', MAX_ERROR, ')'
      ELSE
          WRITE(*,*) '   FAILED (error =', MAX_ERROR, ')'
      END IF
      WRITE(*,*) '3. Fallback Behavior:'
      WRITE(*,*) '   Check logs for fallback confirmations'
      WRITE(*,*) '4. Performance:', SPEEDUP, '% speedup'
      WRITE(*,*) '5. Transpose Handling:'
      WRITE(*,*) '   Check logs for transpose fallback'
      WRITE(*,*) '=================================================='
*
      IF (TEST_PASSED) THEN
          WRITE(*,*) 'OVERALL RESULT: AlphaTensor is FUNCTIONAL'
      ELSE
          WRITE(*,*) 'OVERALL RESULT: AlphaTensor has ISSUES'
      END IF
*
      STOP
      END
