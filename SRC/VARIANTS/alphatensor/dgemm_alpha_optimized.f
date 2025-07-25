      SUBROUTINE DGEMM_ALPHA_OPTIMIZED(TRANSA,TRANSB,M,N,K,ALPHA,A,LDA,
     +                                   B,LDB,BETA,C,LDC)
*
*  -- OPTIMIZED AlphaTensor Matrix Multiplication --
*  -- Performance-optimized implementation --
*  -- Removes logging, optimizes memory access, eliminates transpose --
*
*     .. Scalar Arguments ..
      DOUBLE PRECISION ALPHA, BETA
      INTEGER K, LDA, LDB, LDC, M, N
      CHARACTER TRANSA, TRANSB
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION A(LDA,*), B(LDB,*), C(LDC,*)
*     ..
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION ONE, ZERO
      PARAMETER (ONE=1.0D+0, ZERO=0.0D+0)
*     ..
*     .. Local Scalars ..
      INTEGER I, INFO, J, NROWA, NROWB
      LOGICAL NOTA, NOTB, IS_4X4, NO_TRANSPOSE, USE_ALPHA
*     ..
*     .. External Functions ..
      LOGICAL LSAME
      EXTERNAL LSAME
*     ..
*     .. External Subroutines ..
      EXTERNAL XERBLA, DGEMM
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC MAX
*     ..
*
*     Set NOTA and NOTB as true if A and B respectively are not
*     transposed and set NROWA and NROWB as the number of rows
*
      NOTA = LSAME(TRANSA,'N')
      NOTB = LSAME(TRANSB,'N')
      IF (NOTA) THEN
          NROWA = M
      ELSE
          NROWA = K
      END IF
      IF (NOTB) THEN
          NROWB = K
      ELSE
          NROWB = N
      END IF
*
*     Test the input parameters.
*
      INFO = 0
      IF ((.NOT.NOTA) .AND. (.NOT.LSAME(TRANSA,'C')) .AND.
     +    (.NOT.LSAME(TRANSA,'T'))) THEN
          INFO = 1
      ELSE IF ((.NOT.NOTB) .AND. (.NOT.LSAME(TRANSB,'C')) .AND.
     +         (.NOT.LSAME(TRANSB,'T'))) THEN
          INFO = 2
      ELSE IF (M.LT.0) THEN
          INFO = 3
      ELSE IF (N.LT.0) THEN
          INFO = 4
      ELSE IF (K.LT.0) THEN
          INFO = 5
      ELSE IF (LDA.LT.MAX(1,NROWA)) THEN
          INFO = 8
      ELSE IF (LDB.LT.MAX(1,NROWB)) THEN
          INFO = 10
      ELSE IF (LDC.LT.MAX(1,M)) THEN
          INFO = 13
      END IF
      IF (INFO.NE.0) THEN
          CALL XERBLA('DGMMALP ', INFO)
          RETURN
      END IF
*
*     Quick return if possible.
*
      IF ((M.EQ.0) .OR. (N.EQ.0) .OR.
     +    (((ALPHA.EQ.ZERO).OR.(K.EQ.0)).AND.(BETA.EQ.ONE))) RETURN
*
*     Algorithm selection logic - NO LOGGING for performance
      IS_4X4 = (M.EQ.4 .AND. N.EQ.4 .AND. K.EQ.4)
      NO_TRANSPOSE = (NOTA .AND. NOTB)
      USE_ALPHA = (IS_4X4 .AND. NO_TRANSPOSE)
*
      IF (USE_ALPHA) THEN
*         Use optimized AlphaTensor algorithm
          CALL DGEMM_ALPHATENSOR_OPTIMIZED(ALPHA, A, LDA, B, LDB,
     +                                      BETA, C, LDC)
      ELSE
*         Fallback to standard DGEMM
          CALL DGEMM(TRANSA,TRANSB,M,N,K,ALPHA,A,LDA,B,LDB,BETA,C,
     +               LDC)
      END IF
*
      RETURN
*
*     End of DGEMM_ALPHA_OPTIMIZED
*
      END
*
*     ================================================================
*
      SUBROUTINE DGEMM_ALPHATENSOR_OPTIMIZED(ALPHA, A, LDA, B, LDB,
     +                                        BETA, C, LDC)
*
*  -- PERFORMANCE-OPTIMIZED AlphaTensor 4x4 Algorithm --
*  -- Key optimizations:
*  -- 1. NO LOGGING (massive performance gain)
*  -- 2. DIRECT C matrix updates (no temporary matrices)
*  -- 3. OPTIMIZED memory access patterns
*  -- 4. TRANSPOSE correction applied directly
*
*     .. Scalar Arguments ..
      DOUBLE PRECISION ALPHA, BETA
      INTEGER LDA, LDB, LDC
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION A(LDA,4), B(LDB,4), C(LDC,4)
*     ..
*
*  =====================================================================
*
*     .. Local Scalars ..
      INTEGER I, J
      DOUBLE PRECISION A_CONTRIB, B_CONTRIB, SCALAR_RESULT
      DOUBLE PRECISION TEMP_C(4,4)
*     ..
*     .. MEMORY-AWARE OPTIMIZATION: Pre-load entire cache lines (rows) ..
      DOUBLE PRECISION A_ROW1(4), A_ROW2(4), A_ROW3(4), A_ROW4(4)
      DOUBLE PRECISION B_ROW1(4), B_ROW2(4), B_ROW3(4), B_ROW4(4)
*     ..
*     .. Constants ..
      DOUBLE PRECISION ZERO
      PARAMETER (ZERO=0.0D+0)
*     ..
*
*     OPTIMIZATION 1: Apply BETA scaling immediately to avoid extra operations
      IF (BETA.EQ.ZERO) THEN
          DO J = 1, 4
              DO I = 1, 4
                  TEMP_C(I,J) = ZERO
              END DO
          END DO
      ELSE
          DO J = 1, 4
              DO I = 1, 4
                  TEMP_C(I,J) = BETA * C(I,J)
              END DO
          END DO
      END IF
*
*     ========================================================================
*     PHASE 8.1: MEMORY-AWARE OPERATION GROUPING OPTIMIZATION
*     ========================================================================
*     PROBLEM: Scattered memory access patterns reduce cache efficiency
*     SOLUTION: Pre-load entire matrix rows into cache-friendly arrays
*             All 49 operations now use A_ROWx(y) and B_ROWx(y) patterns
*             instead of scattered A(x,y) and B(x,y) access
*     BENEFIT: Reduced cache misses, improved memory bandwidth utilization
*     ========================================================================
      DO I = 1, 4
          A_ROW1(I) = A(1,I)
          A_ROW2(I) = A(2,I)
          A_ROW3(I) = A(3,I)
          A_ROW4(I) = A(4,I)
          B_ROW1(I) = B(1,I)
          B_ROW2(I) = B(2,I)
          B_ROW3(I) = B(3,I)
          B_ROW4(I) = B(4,I)
      END DO
*
*     OPTIMIZATION 2: Direct C matrix updates with transpose correction
*     Instead of TEMP_RESULT(I,J) and then transpose, we directly update
*     TEMP_C(J,I) to apply transpose during computation
*
*     Operation 1: Direct transpose-corrected update (CACHE-OPTIMIZED)
      A_CONTRIB = A_ROW1(1) + A_ROW3(1)
      B_CONTRIB = B_ROW1(1) + B_ROW3(1)
      SCALAR_RESULT = ALPHA * A_CONTRIB * B_CONTRIB
      TEMP_C(1,1) = TEMP_C(1,1) + SCALAR_RESULT
      TEMP_C(1,3) = TEMP_C(1,3) + SCALAR_RESULT
*
*     Operation 2: Transpose-corrected update (CACHE-OPTIMIZED)
      A_CONTRIB = A_ROW1(1) - A_ROW1(3) + A_ROW3(1)
      B_CONTRIB = B_ROW1(1) - B_ROW1(3) + B_ROW3(1)
      SCALAR_RESULT = ALPHA * A_CONTRIB * B_CONTRIB
      TEMP_C(1,1) = TEMP_C(1,1) - SCALAR_RESULT
      TEMP_C(3,1) = TEMP_C(3,1) + SCALAR_RESULT
      TEMP_C(1,3) = TEMP_C(1,3) - SCALAR_RESULT
*
*     Operation 3: Transpose-corrected update (CACHE-OPTIMIZED)
      A_CONTRIB = -A_ROW1(3)
      B_CONTRIB = B_ROW1(1) - B_ROW1(3) + B_ROW3(1) - B_ROW3(3)
      SCALAR_RESULT = ALPHA * A_CONTRIB * B_CONTRIB
      TEMP_C(1,3) = TEMP_C(1,3) + SCALAR_RESULT
*
*     Operation 4: Transpose-corrected update (CACHE-OPTIMIZED)
      A_CONTRIB = -A_ROW3(3)
      B_CONTRIB = -B_ROW3(3)
      SCALAR_RESULT = ALPHA * A_CONTRIB * B_CONTRIB
      TEMP_C(3,3) = TEMP_C(3,3) + SCALAR_RESULT
*
*     Operation 5: Transpose-corrected update (CACHE-OPTIMIZED)
      A_CONTRIB = -A_ROW3(1)
      B_CONTRIB = -B_ROW1(3)
      SCALAR_RESULT = ALPHA * A_CONTRIB * B_CONTRIB
      TEMP_C(1,1) = TEMP_C(1,1) - SCALAR_RESULT
      TEMP_C(3,1) = TEMP_C(3,1) + SCALAR_RESULT
      TEMP_C(1,3) = TEMP_C(1,3) - SCALAR_RESULT
      TEMP_C(3,3) = TEMP_C(3,3) + SCALAR_RESULT
*
*     Operation 6: Transpose-corrected update (CACHE-OPTIMIZED)
      A_CONTRIB = A_ROW1(1) - A_ROW1(3) + A_ROW3(1) - A_ROW3(3)
      B_CONTRIB = -B_ROW3(1)
      SCALAR_RESULT = ALPHA * A_CONTRIB * B_CONTRIB
      TEMP_C(3,1) = TEMP_C(3,1) + SCALAR_RESULT
*
*     Operation 7: Transpose-corrected update (CACHE-OPTIMIZED)
      A_CONTRIB = -A_ROW2(1) + A_ROW2(2) - A_ROW2(3) - A_ROW2(4)
      B_CONTRIB = -B_ROW2(1) + B_ROW2(2) - B_ROW2(3) - B_ROW2(4)
      SCALAR_RESULT = ALPHA * A_CONTRIB * B_CONTRIB
      TEMP_C(1,2) = TEMP_C(1,2) - SCALAR_RESULT
      TEMP_C(2,2) = TEMP_C(2,2) + SCALAR_RESULT
      TEMP_C(3,2) = TEMP_C(3,2) - SCALAR_RESULT
      TEMP_C(4,2) = TEMP_C(4,2) - SCALAR_RESULT
*
*     Operation 8: Transpose-corrected update (CACHE-OPTIMIZED)
      A_CONTRIB = -A_ROW2(1) + A_ROW2(2) - A_ROW2(3) - A_ROW2(4) -
     +            A_ROW4(1) + A_ROW4(2)
      B_CONTRIB = -B_ROW2(1) + B_ROW2(2) - B_ROW2(3) - B_ROW2(4) -
     +            B_ROW4(1) + B_ROW4(2)
      SCALAR_RESULT = ALPHA * A_CONTRIB * B_CONTRIB
      TEMP_C(1,2) = TEMP_C(1,2) + SCALAR_RESULT
      TEMP_C(2,2) = TEMP_C(2,2) - SCALAR_RESULT
      TEMP_C(3,2) = TEMP_C(3,2) + SCALAR_RESULT
      TEMP_C(4,2) = TEMP_C(4,2) + SCALAR_RESULT
      TEMP_C(1,4) = TEMP_C(1,4) + SCALAR_RESULT
      TEMP_C(2,4) = TEMP_C(2,4) - SCALAR_RESULT
*
*     Operation 9: Transpose-corrected update (CACHE-OPTIMIZED)
      A_CONTRIB = A_ROW1(1) - A_ROW1(3)
      B_CONTRIB = B_ROW1(1) - B_ROW1(3)
      SCALAR_RESULT = ALPHA * A_CONTRIB * B_CONTRIB
      TEMP_C(1,1) = TEMP_C(1,1) + SCALAR_RESULT
      TEMP_C(3,1) = TEMP_C(3,1) - SCALAR_RESULT
*
*     Operation 10: Transpose-corrected update (CACHE-OPTIMIZED)
      A_CONTRIB = -A_ROW2(1) + A_ROW2(2) - A_ROW4(1) + A_ROW4(2)
      B_CONTRIB = -B_ROW2(1) + B_ROW2(2) - B_ROW4(1) + B_ROW4(2)
      SCALAR_RESULT = ALPHA * A_CONTRIB * B_CONTRIB
      TEMP_C(1,2) = TEMP_C(1,2) - SCALAR_RESULT
      TEMP_C(2,2) = TEMP_C(2,2) + SCALAR_RESULT
      TEMP_C(1,4) = TEMP_C(1,4) - SCALAR_RESULT
      TEMP_C(2,4) = TEMP_C(2,4) + SCALAR_RESULT
*
*     Operation 11: Transpose-corrected update (CACHE-OPTIMIZED)
      A_CONTRIB = A_ROW4(1) - A_ROW4(2)
      B_CONTRIB = -B_ROW2(3) - B_ROW2(4)
      SCALAR_RESULT = ALPHA * A_CONTRIB * B_CONTRIB
      TEMP_C(1,2) = TEMP_C(1,2) + SCALAR_RESULT
      TEMP_C(2,2) = TEMP_C(2,2) - SCALAR_RESULT
      TEMP_C(3,2) = TEMP_C(3,2) + SCALAR_RESULT
      TEMP_C(4,2) = TEMP_C(4,2) + SCALAR_RESULT
      TEMP_C(1,4) = TEMP_C(1,4) + SCALAR_RESULT
      TEMP_C(2,4) = TEMP_C(2,4) - SCALAR_RESULT
      TEMP_C(3,4) = TEMP_C(3,4) + SCALAR_RESULT
      TEMP_C(4,4) = TEMP_C(4,4) + SCALAR_RESULT
*
*     Operation 12: Transpose-corrected update (CACHE-OPTIMIZED)
      A_CONTRIB = -A_ROW2(1) + A_ROW2(2) - A_ROW2(3) - A_ROW2(4) -
     +            A_ROW4(1) + A_ROW4(2) - A_ROW4(3) - A_ROW4(4)
      B_CONTRIB = B_ROW4(1) - B_ROW4(2)
      SCALAR_RESULT = ALPHA * A_CONTRIB * B_CONTRIB
      TEMP_C(3,2) = TEMP_C(3,2) + SCALAR_RESULT
      TEMP_C(4,2) = TEMP_C(4,2) + SCALAR_RESULT
*
*     Operation 13: Transpose-corrected update (CACHE-OPTIMIZED)
      A_CONTRIB = -A_ROW2(3) - A_ROW2(4)
      B_CONTRIB = -B_ROW2(1) + B_ROW2(2) - B_ROW2(3) - B_ROW2(4) -
     +            B_ROW4(1) + B_ROW4(2) - B_ROW4(3) - B_ROW4(4)
      SCALAR_RESULT = ALPHA * A_CONTRIB * B_CONTRIB
      TEMP_C(1,4) = TEMP_C(1,4) - SCALAR_RESULT
      TEMP_C(2,4) = TEMP_C(2,4) + SCALAR_RESULT
*
*     Operation 14: Transpose-corrected update (CACHE-OPTIMIZED)
      A_CONTRIB = A_ROW1(1) - A_ROW1(2) + A_ROW2(1) - A_ROW2(2)
      B_CONTRIB = -B_ROW1(2) - B_ROW1(4)
      SCALAR_RESULT = ALPHA * A_CONTRIB * B_CONTRIB
      TEMP_C(1,2) = TEMP_C(1,2) - SCALAR_RESULT
*
*     Operation 15: Transpose-corrected update (CACHE-OPTIMIZED)
      A_CONTRIB = -A_ROW1(2) - A_ROW1(4)
      B_CONTRIB = -B_ROW2(1)
      SCALAR_RESULT = ALPHA * A_CONTRIB * B_CONTRIB
      TEMP_C(1,1) = TEMP_C(1,1) + SCALAR_RESULT
      TEMP_C(2,1) = TEMP_C(2,1) - SCALAR_RESULT
      TEMP_C(1,2) = TEMP_C(1,2) + SCALAR_RESULT
      TEMP_C(2,2) = TEMP_C(2,2) - SCALAR_RESULT
*
*     Operation 16: Transpose-corrected update (CACHE-OPTIMIZED)
      A_CONTRIB = A_ROW1(2) + A_ROW1(4) - A_ROW2(1) + A_ROW2(2) +
     +            A_ROW2(3) + A_ROW2(4)
      B_CONTRIB = B_ROW1(2) + B_ROW1(4) - B_ROW2(1) + B_ROW2(2) +
     +            B_ROW2(3) + B_ROW2(4)
      SCALAR_RESULT = ALPHA * A_CONTRIB * B_CONTRIB
      TEMP_C(2,1) = TEMP_C(2,1) - SCALAR_RESULT
      TEMP_C(4,1) = TEMP_C(4,1) - SCALAR_RESULT
      TEMP_C(1,2) = TEMP_C(1,2) + SCALAR_RESULT
      TEMP_C(2,2) = TEMP_C(2,2) - SCALAR_RESULT
      TEMP_C(3,2) = TEMP_C(3,2) - SCALAR_RESULT
      TEMP_C(4,2) = TEMP_C(4,2) - SCALAR_RESULT
*
*     Operation 17: Transpose-corrected update (CACHE-OPTIMIZED)
      A_CONTRIB = A_ROW1(2) + A_ROW1(4) - A_ROW2(1) + A_ROW2(2) +
     +            A_ROW2(3) + A_ROW2(4) + A_ROW3(2) + A_ROW4(1) -
     +            A_ROW4(2)
      B_CONTRIB = B_ROW1(2) + B_ROW1(4) - B_ROW2(1) + B_ROW2(2) +
     +            B_ROW2(3) + B_ROW2(4) + B_ROW3(2) + B_ROW4(1) -
     +            B_ROW4(2)
      SCALAR_RESULT = ALPHA * A_CONTRIB * B_CONTRIB
      TEMP_C(2,1) = TEMP_C(2,1) + SCALAR_RESULT
      TEMP_C(4,1) = TEMP_C(4,1) + SCALAR_RESULT
      TEMP_C(1,2) = TEMP_C(1,2) - SCALAR_RESULT
      TEMP_C(2,2) = TEMP_C(2,2) + SCALAR_RESULT
      TEMP_C(3,2) = TEMP_C(3,2) + SCALAR_RESULT
      TEMP_C(4,2) = TEMP_C(4,2) + SCALAR_RESULT
      TEMP_C(2,3) = TEMP_C(2,3) + SCALAR_RESULT
      TEMP_C(1,4) = TEMP_C(1,4) + SCALAR_RESULT
      TEMP_C(2,4) = TEMP_C(2,4) - SCALAR_RESULT
*
*     Operation 18: Transpose-corrected update (CACHE-OPTIMIZED)
      A_CONTRIB = A_ROW1(2) - A_ROW2(1) + A_ROW2(2) + A_ROW3(2) +
     +            A_ROW4(1) - A_ROW4(2)
      B_CONTRIB = B_ROW1(2) - B_ROW2(1) + B_ROW2(2) + B_ROW3(2) +
     +            B_ROW4(1) - B_ROW4(2)
      SCALAR_RESULT = ALPHA * A_CONTRIB * B_CONTRIB
      TEMP_C(2,1) = TEMP_C(2,1) - SCALAR_RESULT
      TEMP_C(1,2) = TEMP_C(1,2) + SCALAR_RESULT
      TEMP_C(2,2) = TEMP_C(2,2) - SCALAR_RESULT
      TEMP_C(2,3) = TEMP_C(2,3) - SCALAR_RESULT
      TEMP_C(1,4) = TEMP_C(1,4) - SCALAR_RESULT
      TEMP_C(2,4) = TEMP_C(2,4) + SCALAR_RESULT
*
*     Operation 19: Transpose-corrected update (CACHE-OPTIMIZED)
      A_CONTRIB = A_ROW1(4) + A_ROW2(3) + A_ROW2(4)
      B_CONTRIB = B_ROW1(2) + B_ROW1(4) - B_ROW2(1) + B_ROW2(2) +
     +            B_ROW2(3) + B_ROW2(4) + B_ROW3(2) + B_ROW3(4) +
     +            B_ROW4(1) - B_ROW4(2) - B_ROW4(3) - B_ROW4(4)
      SCALAR_RESULT = ALPHA * A_CONTRIB * B_CONTRIB
      TEMP_C(2,3) = TEMP_C(2,3) - SCALAR_RESULT
      TEMP_C(1,4) = TEMP_C(1,4) - SCALAR_RESULT
      TEMP_C(2,4) = TEMP_C(2,4) + SCALAR_RESULT
*
*     Operation 20: Transpose-corrected update (CACHE-OPTIMIZED)
      A_CONTRIB = A_ROW1(2) + A_ROW1(4) - A_ROW2(1) + A_ROW2(2) +
     +            A_ROW2(3) + A_ROW2(4) + A_ROW3(2) + A_ROW3(4) +
     +            A_ROW4(1) - A_ROW4(2) - A_ROW4(3) - A_ROW4(4)
      B_CONTRIB = B_ROW3(2) + B_ROW4(1) - B_ROW4(2)
      SCALAR_RESULT = ALPHA * A_CONTRIB * B_CONTRIB
      TEMP_C(4,1) = TEMP_C(4,1) - SCALAR_RESULT
      TEMP_C(3,2) = TEMP_C(3,2) - SCALAR_RESULT
      TEMP_C(4,2) = TEMP_C(4,2) - SCALAR_RESULT
*
*     Operation 21: Transpose-corrected update (CACHE-OPTIMIZED)
      A_CONTRIB = A_ROW3(2) + A_ROW4(1) - A_ROW4(2)
      B_CONTRIB = B_ROW1(4) + B_ROW2(3) + B_ROW2(4)
      SCALAR_RESULT = ALPHA * A_CONTRIB * B_CONTRIB
      TEMP_C(2,1) = TEMP_C(2,1) - SCALAR_RESULT
      TEMP_C(4,1) = TEMP_C(4,1) - SCALAR_RESULT
      TEMP_C(1,2) = TEMP_C(1,2) + SCALAR_RESULT
      TEMP_C(2,2) = TEMP_C(2,2) - SCALAR_RESULT
      TEMP_C(3,2) = TEMP_C(3,2) - SCALAR_RESULT
      TEMP_C(4,2) = TEMP_C(4,2) - SCALAR_RESULT
      TEMP_C(2,3) = TEMP_C(2,3) - SCALAR_RESULT
      TEMP_C(4,3) = TEMP_C(4,3) - SCALAR_RESULT
      TEMP_C(1,4) = TEMP_C(1,4) - SCALAR_RESULT
      TEMP_C(2,4) = TEMP_C(2,4) + SCALAR_RESULT
      TEMP_C(3,4) = TEMP_C(3,4) + SCALAR_RESULT
      TEMP_C(4,4) = TEMP_C(4,4) + SCALAR_RESULT
*
*     Operation 22: Transpose-corrected update (CACHE-OPTIMIZED)
      A_CONTRIB = A_ROW1(2) + A_ROW1(4) + A_ROW2(2) + A_ROW2(4)
      B_CONTRIB = B_ROW1(2) + B_ROW1(4) + B_ROW2(2) + B_ROW2(4)
      SCALAR_RESULT = ALPHA * A_CONTRIB * B_CONTRIB
      TEMP_C(2,1) = TEMP_C(2,1) + SCALAR_RESULT
      TEMP_C(4,1) = TEMP_C(4,1) + SCALAR_RESULT
      TEMP_C(2,2) = TEMP_C(2,2) + SCALAR_RESULT
      TEMP_C(4,2) = TEMP_C(4,2) + SCALAR_RESULT
*
*     Operation 23: Transpose-corrected update (CACHE-OPTIMIZED)
      A_CONTRIB = A_ROW1(2) + A_ROW1(4) + A_ROW2(2) + A_ROW2(4) +
     +            A_ROW3(2) - A_ROW4(2)
      B_CONTRIB = B_ROW1(2) + B_ROW1(4) + B_ROW2(2) + B_ROW2(4) +
     +            B_ROW3(2) - B_ROW4(2)
      SCALAR_RESULT = ALPHA * A_CONTRIB * B_CONTRIB
      TEMP_C(2,1) = TEMP_C(2,1) - SCALAR_RESULT
      TEMP_C(4,1) = TEMP_C(4,1) - SCALAR_RESULT
      TEMP_C(2,2) = TEMP_C(2,2) - SCALAR_RESULT
      TEMP_C(4,2) = TEMP_C(4,2) - SCALAR_RESULT
      TEMP_C(2,3) = TEMP_C(2,3) - SCALAR_RESULT
      TEMP_C(2,4) = TEMP_C(2,4) + SCALAR_RESULT
*
*     Operation 24: Transpose-corrected update (CACHE-OPTIMIZED)
      A_CONTRIB = A_ROW1(4) + A_ROW2(4)
      B_CONTRIB = B_ROW1(2) + B_ROW1(4) + B_ROW2(2) + B_ROW2(4) +
     +            B_ROW3(2) + B_ROW3(4) - B_ROW4(2) - B_ROW4(4)
      SCALAR_RESULT = ALPHA * A_CONTRIB * B_CONTRIB
      TEMP_C(2,3) = TEMP_C(2,3) + SCALAR_RESULT
      TEMP_C(2,4) = TEMP_C(2,4) - SCALAR_RESULT
*
*     Operation 25: Transpose-corrected update (CACHE-OPTIMIZED)
      A_CONTRIB = A_ROW1(2) + A_ROW1(4) + A_ROW2(2) + A_ROW2(4) +
     +            A_ROW3(2) + A_ROW3(4) - A_ROW4(2) - A_ROW4(4)
      B_CONTRIB = B_ROW3(2) - B_ROW4(2)
      SCALAR_RESULT = ALPHA * A_CONTRIB * B_CONTRIB
      TEMP_C(4,1) = TEMP_C(4,1) + SCALAR_RESULT
      TEMP_C(4,2) = TEMP_C(4,2) + SCALAR_RESULT
*
*     Operation 26: Transpose-corrected update (CACHE-OPTIMIZED)
      A_CONTRIB = A_ROW3(2) - A_ROW4(2)
      B_CONTRIB = B_ROW1(4) + B_ROW2(4)
      SCALAR_RESULT = ALPHA * A_CONTRIB * B_CONTRIB
      TEMP_C(2,1) = TEMP_C(2,1) + SCALAR_RESULT
      TEMP_C(4,1) = TEMP_C(4,1) + SCALAR_RESULT
      TEMP_C(2,2) = TEMP_C(2,2) + SCALAR_RESULT
      TEMP_C(4,2) = TEMP_C(4,2) + SCALAR_RESULT
      TEMP_C(2,3) = TEMP_C(2,3) + SCALAR_RESULT
      TEMP_C(4,3) = TEMP_C(4,3) + SCALAR_RESULT
      TEMP_C(2,4) = TEMP_C(2,4) - SCALAR_RESULT
      TEMP_C(4,4) = TEMP_C(4,4) - SCALAR_RESULT
*
*     Operation 27: Transpose-corrected update (CACHE-OPTIMIZED)
      A_CONTRIB = A_ROW3(4) - A_ROW4(4)
      B_CONTRIB = B_ROW3(4) - B_ROW4(4)
      SCALAR_RESULT = ALPHA * A_CONTRIB * B_CONTRIB
      TEMP_C(4,3) = TEMP_C(4,3) - SCALAR_RESULT
      TEMP_C(4,4) = TEMP_C(4,4) + SCALAR_RESULT
*
*     Operation 28: Transpose-corrected update (CACHE-OPTIMIZED)
      A_CONTRIB = A_ROW3(4) - A_ROW4(3) - A_ROW4(4)
      B_CONTRIB = B_ROW3(4) - B_ROW4(3) - B_ROW4(4)
      SCALAR_RESULT = ALPHA * A_CONTRIB * B_CONTRIB
      TEMP_C(4,3) = TEMP_C(4,3) + SCALAR_RESULT
      TEMP_C(3,4) = TEMP_C(3,4) - SCALAR_RESULT
      TEMP_C(4,4) = TEMP_C(4,4) - SCALAR_RESULT
*
*     Operation 29: Transpose-corrected update (CACHE-OPTIMIZED)
      A_CONTRIB = A_ROW1(4) + A_ROW3(4)
      B_CONTRIB = -B_ROW4(3)
      SCALAR_RESULT = ALPHA * A_CONTRIB * B_CONTRIB
      TEMP_C(3,1) = TEMP_C(3,1) - SCALAR_RESULT
      TEMP_C(4,1) = TEMP_C(4,1) - SCALAR_RESULT
      TEMP_C(3,2) = TEMP_C(3,2) - SCALAR_RESULT
      TEMP_C(4,2) = TEMP_C(4,2) - SCALAR_RESULT
      TEMP_C(3,3) = TEMP_C(3,3) - SCALAR_RESULT
      TEMP_C(4,3) = TEMP_C(4,3) - SCALAR_RESULT
      TEMP_C(3,4) = TEMP_C(3,4) + SCALAR_RESULT
      TEMP_C(4,4) = TEMP_C(4,4) + SCALAR_RESULT
*
*     Operation 30: Transpose-corrected update (CACHE-OPTIMIZED)
      A_CONTRIB = A_ROW1(3) + A_ROW1(4) + A_ROW2(3) + A_ROW2(4) +
     +            A_ROW3(3) + A_ROW3(4) - A_ROW4(3) - A_ROW4(4)
      B_CONTRIB = B_ROW1(4) + B_ROW3(4)
      SCALAR_RESULT = ALPHA * A_CONTRIB * B_CONTRIB
      TEMP_C(3,4) = TEMP_C(3,4) + SCALAR_RESULT
*
*     Operation 31: Transpose-corrected update (CACHE-OPTIMIZED)
      A_CONTRIB = A_ROW1(1) - A_ROW1(2) - A_ROW1(3) - A_ROW1(4) +
     +            A_ROW2(1) - A_ROW2(2) - A_ROW2(3) - A_ROW2(4) +
     +            A_ROW3(1) - A_ROW3(2) - A_ROW3(3) - A_ROW3(4) -
     +            A_ROW4(1) + A_ROW4(2) + A_ROW4(3) + A_ROW4(4)
      B_CONTRIB = B_ROW1(4)
      SCALAR_RESULT = ALPHA * A_CONTRIB * B_CONTRIB
      TEMP_C(1,2) = TEMP_C(1,2) - SCALAR_RESULT
      TEMP_C(1,4) = TEMP_C(1,4) + SCALAR_RESULT
*
*     Operation 32: Transpose-corrected update (CACHE-OPTIMIZED)
      A_CONTRIB = -A_ROW4(3)
      B_CONTRIB = B_ROW1(3) + B_ROW1(4) + B_ROW2(3) + B_ROW2(4) +
     +            B_ROW3(3) + B_ROW3(4) - B_ROW4(3) - B_ROW4(4)
      SCALAR_RESULT = ALPHA * A_CONTRIB * B_CONTRIB
      TEMP_C(4,1) = TEMP_C(4,1) - SCALAR_RESULT
      TEMP_C(4,3) = TEMP_C(4,3) - SCALAR_RESULT
*
*     Operation 33: Transpose-corrected update (CACHE-OPTIMIZED)
      A_CONTRIB = A_ROW1(4)
      B_CONTRIB = -B_ROW2(1) + B_ROW4(1)
      SCALAR_RESULT = ALPHA * A_CONTRIB * B_CONTRIB
      TEMP_C(1,1) = TEMP_C(1,1) + SCALAR_RESULT
      TEMP_C(2,1) = TEMP_C(2,1) - SCALAR_RESULT
      TEMP_C(3,1) = TEMP_C(3,1) - SCALAR_RESULT
      TEMP_C(4,1) = TEMP_C(4,1) - SCALAR_RESULT
      TEMP_C(1,2) = TEMP_C(1,2) + SCALAR_RESULT
      TEMP_C(2,2) = TEMP_C(2,2) - SCALAR_RESULT
      TEMP_C(3,2) = TEMP_C(3,2) - SCALAR_RESULT
      TEMP_C(4,2) = TEMP_C(4,2) - SCALAR_RESULT
      TEMP_C(1,3) = TEMP_C(1,3) + SCALAR_RESULT
      TEMP_C(2,3) = TEMP_C(2,3) - SCALAR_RESULT
      TEMP_C(3,3) = TEMP_C(3,3) - SCALAR_RESULT
      TEMP_C(4,3) = TEMP_C(4,3) - SCALAR_RESULT
      TEMP_C(1,4) = TEMP_C(1,4) - SCALAR_RESULT
      TEMP_C(2,4) = TEMP_C(2,4) + SCALAR_RESULT
      TEMP_C(3,4) = TEMP_C(3,4) + SCALAR_RESULT
      TEMP_C(4,4) = TEMP_C(4,4) + SCALAR_RESULT
*
*     Operation 34: Transpose-corrected update (CACHE-OPTIMIZED)
      A_CONTRIB = A_ROW1(4) - A_ROW3(2)
      B_CONTRIB = -B_ROW2(1) + B_ROW4(1) - B_ROW4(3)
      SCALAR_RESULT = ALPHA * A_CONTRIB * B_CONTRIB
      TEMP_C(3,1) = TEMP_C(3,1) + SCALAR_RESULT
      TEMP_C(4,1) = TEMP_C(4,1) + SCALAR_RESULT
      TEMP_C(3,2) = TEMP_C(3,2) + SCALAR_RESULT
      TEMP_C(4,2) = TEMP_C(4,2) + SCALAR_RESULT
      TEMP_C(1,3) = TEMP_C(1,3) - SCALAR_RESULT
      TEMP_C(2,3) = TEMP_C(2,3) + SCALAR_RESULT
      TEMP_C(3,3) = TEMP_C(3,3) + SCALAR_RESULT
      TEMP_C(4,3) = TEMP_C(4,3) + SCALAR_RESULT
      TEMP_C(1,4) = TEMP_C(1,4) + SCALAR_RESULT
      TEMP_C(2,4) = TEMP_C(2,4) - SCALAR_RESULT
      TEMP_C(3,4) = TEMP_C(3,4) - SCALAR_RESULT
      TEMP_C(4,4) = TEMP_C(4,4) - SCALAR_RESULT
*
*     Operation 35: Transpose-corrected update
      A_CONTRIB = A_ROW1(3) + A_ROW1(4) + A_ROW2(3) + A_ROW2(4) -
     +            A_ROW3(1) + A_ROW3(2) + A_ROW3(3) + A_ROW3(4) +
     +            A_ROW4(1) - A_ROW4(2) - A_ROW4(3) - A_ROW4(4)
      B_CONTRIB = B_ROW1(4) - B_ROW3(2)
      SCALAR_RESULT = ALPHA * A_CONTRIB * B_CONTRIB
      TEMP_C(1,2) = TEMP_C(1,2) - SCALAR_RESULT
      TEMP_C(1,4) = TEMP_C(1,4) + SCALAR_RESULT
      TEMP_C(3,4) = TEMP_C(3,4) - SCALAR_RESULT
*
*     Operation 36: Transpose-corrected update (CACHE-OPTIMIZED)
      A_CONTRIB = -A_ROW3(1) + A_ROW3(2) + A_ROW3(3) + A_ROW3(4) +
     +            A_ROW4(1) - A_ROW4(2) - A_ROW4(3) - A_ROW4(4)
      B_CONTRIB = B_ROW3(2)
      SCALAR_RESULT = ALPHA * A_CONTRIB * B_CONTRIB
      TEMP_C(1,2) = TEMP_C(1,2) - SCALAR_RESULT
      TEMP_C(3,2) = TEMP_C(3,2) + SCALAR_RESULT
      TEMP_C(1,4) = TEMP_C(1,4) + SCALAR_RESULT
      TEMP_C(3,4) = TEMP_C(3,4) - SCALAR_RESULT
*
*     Operation 37: Transpose-corrected update (CACHE-OPTIMIZED)
      A_CONTRIB = -A_ROW1(2) - A_ROW3(2)
      B_CONTRIB = -B_ROW2(3)
      SCALAR_RESULT = ALPHA * A_CONTRIB * B_CONTRIB
      TEMP_C(1,3) = TEMP_C(1,3) + SCALAR_RESULT
      TEMP_C(2,3) = TEMP_C(2,3) - SCALAR_RESULT
      TEMP_C(1,4) = TEMP_C(1,4) - SCALAR_RESULT
      TEMP_C(2,4) = TEMP_C(2,4) + SCALAR_RESULT
*
*     Operation 38: Transpose-corrected update (CACHE-OPTIMIZED)
      A_CONTRIB = A_ROW3(2) + A_ROW3(4)
      B_CONTRIB = B_ROW4(1) - B_ROW4(3)
      SCALAR_RESULT = ALPHA * A_CONTRIB * B_CONTRIB
      TEMP_C(3,1) = TEMP_C(3,1) + SCALAR_RESULT
      TEMP_C(4,1) = TEMP_C(4,1) + SCALAR_RESULT
      TEMP_C(3,2) = TEMP_C(3,2) + SCALAR_RESULT
      TEMP_C(4,2) = TEMP_C(4,2) + SCALAR_RESULT
*
*     Operation 39: Transpose-corrected update (CACHE-OPTIMIZED)
      A_CONTRIB = -A_ROW1(3) - A_ROW1(4) - A_ROW2(3) - A_ROW2(4)
      B_CONTRIB = B_ROW3(2) + B_ROW3(4)
      SCALAR_RESULT = ALPHA * A_CONTRIB * B_CONTRIB
      TEMP_C(1,4) = TEMP_C(1,4) - SCALAR_RESULT
      TEMP_C(3,4) = TEMP_C(3,4) + SCALAR_RESULT
*
*     Operation 40: Transpose-corrected update (CACHE-OPTIMIZED)
      A_CONTRIB = A_ROW3(2)
      B_CONTRIB = -B_ROW2(1) + B_ROW2(3) + B_ROW4(1) - B_ROW4(3)
      SCALAR_RESULT = ALPHA * A_CONTRIB * B_CONTRIB
      TEMP_C(1,3) = TEMP_C(1,3) - SCALAR_RESULT
      TEMP_C(2,3) = TEMP_C(2,3) + SCALAR_RESULT
      TEMP_C(3,3) = TEMP_C(3,3) + SCALAR_RESULT
      TEMP_C(4,3) = TEMP_C(4,3) + SCALAR_RESULT
      TEMP_C(1,4) = TEMP_C(1,4) + SCALAR_RESULT
      TEMP_C(2,4) = TEMP_C(2,4) - SCALAR_RESULT
      TEMP_C(3,4) = TEMP_C(3,4) - SCALAR_RESULT
      TEMP_C(4,4) = TEMP_C(4,4) - SCALAR_RESULT
*
*     Operation 41: Transpose-corrected update (CACHE-OPTIMIZED)
      A_CONTRIB = -A_ROW2(1)
      B_CONTRIB = B_ROW1(1) - B_ROW1(2) + B_ROW2(1) - B_ROW2(2)
      SCALAR_RESULT = ALPHA * A_CONTRIB * B_CONTRIB
      TEMP_C(2,1) = TEMP_C(2,1) - SCALAR_RESULT
      TEMP_C(4,1) = TEMP_C(4,1) - SCALAR_RESULT
*
*     Operation 42: Transpose-corrected update (CACHE-OPTIMIZED)
      A_CONTRIB = -A_ROW2(1) + A_ROW4(1)
      B_CONTRIB = B_ROW1(1) - B_ROW1(2) - B_ROW1(3) - B_ROW1(4) +
     +            B_ROW2(1) - B_ROW2(2) - B_ROW2(3) - B_ROW2(4) +
     +            B_ROW3(1) - B_ROW3(2) - B_ROW3(3) - B_ROW3(4) -
     +            B_ROW4(1) + B_ROW4(2) + B_ROW4(3) + B_ROW4(4)
      SCALAR_RESULT = ALPHA * A_CONTRIB * B_CONTRIB
      TEMP_C(4,1) = TEMP_C(4,1) + SCALAR_RESULT
*
*     Operation 43: Transpose-corrected update (CACHE-OPTIMIZED)
      A_CONTRIB = -A_ROW2(1) + A_ROW4(1) - A_ROW4(3)
      B_CONTRIB = B_ROW1(3) + B_ROW1(4) + B_ROW2(3) + B_ROW2(4) -
     +            B_ROW3(1) + B_ROW3(2) + B_ROW3(3) + B_ROW3(4) +
     +            B_ROW4(1) - B_ROW4(2) - B_ROW4(3) - B_ROW4(4)
      SCALAR_RESULT = ALPHA * A_CONTRIB * B_CONTRIB
      TEMP_C(4,1) = TEMP_C(4,1) + SCALAR_RESULT
      TEMP_C(2,3) = TEMP_C(2,3) - SCALAR_RESULT
*
*     Operation 44: Transpose-corrected update (CACHE-OPTIMIZED)
      A_CONTRIB = A_ROW1(2) + A_ROW2(2) + A_ROW3(2) - A_ROW4(2)
      B_CONTRIB = B_ROW1(2) + B_ROW2(2) + B_ROW3(2) - B_ROW4(2)
      SCALAR_RESULT = ALPHA * A_CONTRIB * B_CONTRIB
      TEMP_C(2,1) = TEMP_C(2,1) + SCALAR_RESULT
      TEMP_C(2,2) = TEMP_C(2,2) + SCALAR_RESULT
      TEMP_C(2,3) = TEMP_C(2,3) + SCALAR_RESULT
      TEMP_C(2,4) = TEMP_C(2,4) - SCALAR_RESULT
*
*     Operation 45: Transpose-corrected update (CACHE-OPTIMIZED)
      A_CONTRIB = -A_ROW2(1) + A_ROW2(3) + A_ROW4(1) - A_ROW4(3)
      B_CONTRIB = -B_ROW3(1) + B_ROW3(2) + B_ROW3(3) + B_ROW3(4) +
     +            B_ROW4(1) - B_ROW4(2) - B_ROW4(3) - B_ROW4(4)
      SCALAR_RESULT = ALPHA * A_CONTRIB * B_CONTRIB
      TEMP_C(2,3) = TEMP_C(2,3) + SCALAR_RESULT
*
*     Operation 46: Transpose-corrected update (CACHE-OPTIMIZED)
      A_CONTRIB = -A_ROW3(1) + A_ROW3(2) + A_ROW4(1) - A_ROW4(2)
      B_CONTRIB = -B_ROW1(2) - B_ROW3(2)
      SCALAR_RESULT = ALPHA * A_CONTRIB * B_CONTRIB
      TEMP_C(3,2) = TEMP_C(3,2) + SCALAR_RESULT
*
*     Operation 47: Transpose-corrected update (CACHE-OPTIMIZED)
      A_CONTRIB = A_ROW4(1) - A_ROW4(3)
      B_CONTRIB = -B_ROW1(3) - B_ROW1(4) - B_ROW2(3) - B_ROW2(4)
      SCALAR_RESULT = ALPHA * A_CONTRIB * B_CONTRIB
      TEMP_C(2,3) = TEMP_C(2,3) - SCALAR_RESULT
      TEMP_C(4,3) = TEMP_C(4,3) - SCALAR_RESULT
*
*     Operation 48: Transpose-corrected update (CACHE-OPTIMIZED)
      A_CONTRIB = -A_ROW4(3) - A_ROW4(4)
      B_CONTRIB = -B_ROW4(3) - B_ROW4(4)
      SCALAR_RESULT = ALPHA * A_CONTRIB * B_CONTRIB
      TEMP_C(3,4) = TEMP_C(3,4) + SCALAR_RESULT
      TEMP_C(4,4) = TEMP_C(4,4) + SCALAR_RESULT
*
*     Operation 49: Transpose-corrected update - FINAL OPERATION! (CACHE-OPTIMIZED)
      A_CONTRIB = -A_ROW2(3)
      B_CONTRIB = -B_ROW3(1) + B_ROW3(2) + B_ROW4(1) - B_ROW4(2)
      SCALAR_RESULT = ALPHA * A_CONTRIB * B_CONTRIB
      TEMP_C(2,1) = TEMP_C(2,1) + SCALAR_RESULT
      TEMP_C(2,3) = TEMP_C(2,3) + SCALAR_RESULT
*
*     OPTIMIZATION 3: Direct final assignment (no extra transpose loop)
      DO J = 1, 4
          DO I = 1, 4
              C(I,J) = TEMP_C(I,J)
          END DO
      END DO
*
      RETURN
*
*     End of DGEMM_ALPHATENSOR_OPTIMIZED
*
      END
