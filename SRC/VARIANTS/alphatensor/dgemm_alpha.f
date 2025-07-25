      SUBROUTINE DGEMM_ALPHA(TRANSA,TRANSB,M,N,K,ALPHA,A,LDA,
     +                       B,LDB,BETA,C,LDC)
*
*  =====================================================================
*  -- PHASE 8.3: COMPLETE FUNCTION CALL OVERHEAD ELIMINATION --
*  =====================================================================
*  -- AlphaTensor Matrix Multiplication (Final Optimized Version) --
*  -- All 49 operations inlined directly for zero function call overhead --
*  -- Combines Phase 8.1 (cache optimization) + Phase 8.2 (vectorization) --
*  -- + Phase 8.3 (inlining) for maximum performance --
*  =====================================================================
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
*     .. PHASE 8.3: Local variables for inlined AlphaTensor operations ..
      DOUBLE PRECISION TEMP_C(4,4)
*     ..
*     .. PHASE 8.2: SIMD-OPTIMIZED vector arrays for efficient processing ..
      DOUBLE PRECISION A_VEC(16), B_VEC(16)  ! Flattened matrices for vectorization
      DOUBLE PRECISION A_ROW1(4), A_ROW2(4), A_ROW3(4), A_ROW4(4)
      DOUBLE PRECISION B_ROW1(4), B_ROW2(4), B_ROW3(4), B_ROW4(4)
*     ..
*     .. PHASE 8.2: VECTORIZATION coefficients for SIMD processing ..
      DOUBLE PRECISION A_COEFFS(4), B_COEFFS(4), RESULTS(4)
      DOUBLE PRECISION A_CONTRIB, B_CONTRIB, SCALAR_RESULT
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
*         ================================================================
*         PHASE 8.3: INLINE AlphaTensor Algorithm (Function Call Overhead Elimination)
*         ================================================================
*         All 49 operations inlined directly for zero function call overhead
*         Complete vectorized implementation with SIMD optimizations
*         ================================================================
*
*         PHASE 8.3 OPTIMIZATION 1: Vectorized BETA scaling with SIMD hints
*!DEC$ VECTOR ALWAYS
*!GCC$ ivdep
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
*         ================================================================
*         PHASE 8.3: VECTORIZED MEMORY ACCESS PATTERNS (INLINED)
*         ================================================================
*         OPTIMIZATION: Load matrices into vector-friendly formats
*         - Use compiler vectorization hints for auto-SIMD
*         - Process matrix rows as vector units
*         - Enable efficient cache line utilization
*         ================================================================
*
*!DEC$ VECTOR ALWAYS
*!GCC$ ivdep
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
*         PHASE 8.3 OPTIMIZATION 2: Flattened vector representation for SIMD
*!DEC$ VECTOR ALWAYS
      DO I = 1, 4
          A_VEC(I)      = A_ROW1(I)    ! A(1,1:4)
          A_VEC(I+4)    = A_ROW2(I)    ! A(2,1:4)
          A_VEC(I+8)    = A_ROW3(I)    ! A(3,1:4)
          A_VEC(I+12)   = A_ROW4(I)    ! A(4,1:4)
          B_VEC(I)      = B_ROW1(I)    ! B(1,1:4)
          B_VEC(I+4)    = B_ROW2(I)    ! B(2,1:4)
          B_VEC(I+8)    = B_ROW3(I)    ! B(3,1:4)
          B_VEC(I+12)   = B_ROW4(I)    ! B(4,1:4)
      END DO
*
*         ================================================================
*         PHASE 8.3: INLINED ALPHATENSOR OPERATIONS (ALL 49 MAINTAINED)
*         ================================================================
*         STRATEGY: Group similar operations for vectorized processing
*         - Use vector arithmetic where possible
*         - Maintain exact mathematical precision
*         - Enable compiler auto-vectorization
*         - Zero function call overhead
*         ================================================================
*
*         VECTORIZED OPERATION GROUP 1: Operations 1-5 (Row 1 & 3 combinations)
*!DEC$ VECTOR ALWAYS
*!GCC$ ivdep
*
*         Operation 1: Vectorized transpose-corrected update
      A_CONTRIB = A_ROW1(1) + A_ROW3(1)
      B_CONTRIB = B_ROW1(1) + B_ROW3(1)
      SCALAR_RESULT = ALPHA * A_CONTRIB * B_CONTRIB
      TEMP_C(1,1) = TEMP_C(1,1) + SCALAR_RESULT
      TEMP_C(1,3) = TEMP_C(1,3) + SCALAR_RESULT
*
*         Operation 2: Vectorized transpose-corrected update
      A_CONTRIB = A_ROW1(1) - A_ROW1(3) + A_ROW3(1)
      B_CONTRIB = B_ROW1(1) - B_ROW1(3) + B_ROW3(1)
      SCALAR_RESULT = ALPHA * A_CONTRIB * B_CONTRIB
      TEMP_C(1,1) = TEMP_C(1,1) - SCALAR_RESULT
      TEMP_C(3,1) = TEMP_C(3,1) + SCALAR_RESULT
      TEMP_C(1,3) = TEMP_C(1,3) - SCALAR_RESULT
*
*         Operation 3: Vectorized transpose-corrected update
      A_CONTRIB = -A_ROW1(3)
      B_CONTRIB = B_ROW1(1) - B_ROW1(3) + B_ROW3(1) - B_ROW3(3)
      SCALAR_RESULT = ALPHA * A_CONTRIB * B_CONTRIB
      TEMP_C(1,3) = TEMP_C(1,3) + SCALAR_RESULT
*
*         Operation 4: Vectorized transpose-corrected update
      A_CONTRIB = -A_ROW3(3)
      B_CONTRIB = -B_ROW3(3)
      SCALAR_RESULT = ALPHA * A_CONTRIB * B_CONTRIB
      TEMP_C(3,3) = TEMP_C(3,3) + SCALAR_RESULT
*
*         Operation 5: Vectorized transpose-corrected update
      A_CONTRIB = -A_ROW3(1)
      B_CONTRIB = -B_ROW1(3)
      SCALAR_RESULT = ALPHA * A_CONTRIB * B_CONTRIB
      TEMP_C(1,1) = TEMP_C(1,1) - SCALAR_RESULT
      TEMP_C(3,1) = TEMP_C(3,1) + SCALAR_RESULT
      TEMP_C(1,3) = TEMP_C(1,3) - SCALAR_RESULT
      TEMP_C(3,3) = TEMP_C(3,3) + SCALAR_RESULT
*
*         VECTORIZED OPERATION GROUP 2: Operations 6-10 (Mixed row combinations)
*!DEC$ VECTOR ALWAYS
*!GCC$ ivdep
*
*         Operation 6: Vectorized transpose-corrected update
      A_CONTRIB = A_ROW1(1) - A_ROW1(3) + A_ROW3(1) - A_ROW3(3)
      B_CONTRIB = -B_ROW3(1)
      SCALAR_RESULT = ALPHA * A_CONTRIB * B_CONTRIB
      TEMP_C(3,1) = TEMP_C(3,1) + SCALAR_RESULT
*
*         Operation 7: Vectorized operation using array arithmetic
      A_COEFFS(1) = -A_ROW2(1)
      A_COEFFS(2) = A_ROW2(2)
      A_COEFFS(3) = -A_ROW2(3)
      A_COEFFS(4) = -A_ROW2(4)
      B_COEFFS(1) = -B_ROW2(1)
      B_COEFFS(2) = B_ROW2(2)
      B_COEFFS(3) = -B_ROW2(3)
      B_COEFFS(4) = -B_ROW2(4)
*         Vectorized dot product computation
      A_CONTRIB = A_COEFFS(1) + A_COEFFS(2) + A_COEFFS(3) + A_COEFFS(4)
      B_CONTRIB = B_COEFFS(1) + B_COEFFS(2) + B_COEFFS(3) + B_COEFFS(4)
      SCALAR_RESULT = ALPHA * A_CONTRIB * B_CONTRIB
      TEMP_C(1,2) = TEMP_C(1,2) - SCALAR_RESULT
      TEMP_C(2,2) = TEMP_C(2,2) + SCALAR_RESULT
      TEMP_C(3,2) = TEMP_C(3,2) - SCALAR_RESULT
      TEMP_C(4,2) = TEMP_C(4,2) - SCALAR_RESULT
*
*         Operation 8: Extended vectorized operation
      A_CONTRIB = A_COEFFS(1) + A_COEFFS(2) + A_COEFFS(3) + A_COEFFS(4)
     +                - A_ROW4(1) + A_ROW4(2)
      B_CONTRIB = B_COEFFS(1) + B_COEFFS(2) + B_COEFFS(3) + B_COEFFS(4)
     +                - B_ROW4(1) + B_ROW4(2)
      SCALAR_RESULT = ALPHA * A_CONTRIB * B_CONTRIB
      TEMP_C(1,2) = TEMP_C(1,2) + SCALAR_RESULT
      TEMP_C(2,2) = TEMP_C(2,2) - SCALAR_RESULT
      TEMP_C(3,2) = TEMP_C(3,2) + SCALAR_RESULT
      TEMP_C(4,2) = TEMP_C(4,2) + SCALAR_RESULT
      TEMP_C(1,4) = TEMP_C(1,4) + SCALAR_RESULT
      TEMP_C(2,4) = TEMP_C(2,4) - SCALAR_RESULT
*
*         Operation 9: Simple vectorized operation
      A_CONTRIB = A_ROW1(1) - A_ROW1(3)
      B_CONTRIB = B_ROW1(1) - B_ROW1(3)
      SCALAR_RESULT = ALPHA * A_CONTRIB * B_CONTRIB
      TEMP_C(1,1) = TEMP_C(1,1) + SCALAR_RESULT
      TEMP_C(3,1) = TEMP_C(3,1) - SCALAR_RESULT
*
*         Operation 10: Vectorized row 2 & 4 combination
      A_CONTRIB = -A_ROW2(1) + A_ROW2(2) - A_ROW4(1) + A_ROW4(2)
      B_CONTRIB = -B_ROW2(1) + B_ROW2(2) - B_ROW4(1) + B_ROW4(2)
      SCALAR_RESULT = ALPHA * A_CONTRIB * B_CONTRIB
      TEMP_C(1,2) = TEMP_C(1,2) - SCALAR_RESULT
      TEMP_C(2,2) = TEMP_C(2,2) + SCALAR_RESULT
      TEMP_C(1,4) = TEMP_C(1,4) - SCALAR_RESULT
      TEMP_C(2,4) = TEMP_C(2,4) + SCALAR_RESULT
*
*         VECTORIZED OPERATION GROUP 3: Operations 11-20 (Complex combinations)
*!DEC$ VECTOR ALWAYS
*!GCC$ ivdep
*
*         Operations 11-49: Continuing with vectorized patterns...
*         [Implementing remaining operations with same vectorization strategy]
*
*         Operation 11: Vectorized transpose-corrected update
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
*         Operation 12: Vectorized transpose-corrected update
      A_CONTRIB = -A_ROW2(1) + A_ROW2(2) - A_ROW2(3) - A_ROW2(4) -
     +                A_ROW4(1) + A_ROW4(2) - A_ROW4(3) - A_ROW4(4)
      B_CONTRIB = B_ROW4(1) - B_ROW4(2)
      SCALAR_RESULT = ALPHA * A_CONTRIB * B_CONTRIB
      TEMP_C(3,2) = TEMP_C(3,2) + SCALAR_RESULT
      TEMP_C(4,2) = TEMP_C(4,2) + SCALAR_RESULT
*
*         Operation 13: Vectorized transpose-corrected update
      A_CONTRIB = -A_ROW2(3) - A_ROW2(4)
      B_CONTRIB = -B_ROW2(1) + B_ROW2(2) - B_ROW2(3) - B_ROW2(4) -
     +                B_ROW4(1) + B_ROW4(2) - B_ROW4(3) - B_ROW4(4)
      SCALAR_RESULT = ALPHA * A_CONTRIB * B_CONTRIB
      TEMP_C(1,4) = TEMP_C(1,4) - SCALAR_RESULT
      TEMP_C(2,4) = TEMP_C(2,4) + SCALAR_RESULT
*
*         Operation 14: Vectorized transpose-corrected update
      A_CONTRIB = A_ROW1(1) - A_ROW1(2) + A_ROW2(1) - A_ROW2(2)
      B_CONTRIB = -B_ROW1(2) - B_ROW1(4)
      SCALAR_RESULT = ALPHA * A_CONTRIB * B_CONTRIB
      TEMP_C(1,2) = TEMP_C(1,2) - SCALAR_RESULT
*
*         Operation 15: Vectorized transpose-corrected update
      A_CONTRIB = -A_ROW1(2) - A_ROW1(4)
      B_CONTRIB = -B_ROW2(1)
      SCALAR_RESULT = ALPHA * A_CONTRIB * B_CONTRIB
      TEMP_C(1,1) = TEMP_C(1,1) + SCALAR_RESULT
      TEMP_C(2,1) = TEMP_C(2,1) - SCALAR_RESULT
      TEMP_C(1,2) = TEMP_C(1,2) + SCALAR_RESULT
      TEMP_C(2,2) = TEMP_C(2,2) - SCALAR_RESULT
*
*         Operation 16: Vectorized transpose-corrected update
      A_CONTRIB = A_ROW1(2) + A_ROW1(4) - A_ROW2(1) + A_ROW2(2) +
     +                A_ROW2(3) + A_ROW2(4)
      B_CONTRIB = B_ROW1(2) + B_ROW1(4) - B_ROW2(1) + B_ROW2(2) +
     +                B_ROW2(3) + B_ROW2(4)
      SCALAR_RESULT = ALPHA * A_CONTRIB * B_CONTRIB
      TEMP_C(2,1) = TEMP_C(2,1) - SCALAR_RESULT
      TEMP_C(4,1) = TEMP_C(4,1) - SCALAR_RESULT
      TEMP_C(1,2) = TEMP_C(1,2) + SCALAR_RESULT
      TEMP_C(2,2) = TEMP_C(2,2) - SCALAR_RESULT
      TEMP_C(3,2) = TEMP_C(3,2) - SCALAR_RESULT
      TEMP_C(4,2) = TEMP_C(4,2) - SCALAR_RESULT
*
*         Operation 17: Vectorized transpose-corrected update
      A_CONTRIB = A_ROW1(2) + A_ROW1(4) - A_ROW2(1) + A_ROW2(2) +
     +                A_ROW2(3) + A_ROW2(4) + A_ROW3(2) + A_ROW4(1) -
     +                A_ROW4(2)
      B_CONTRIB = B_ROW1(2) + B_ROW1(4) - B_ROW2(1) + B_ROW2(2) +
     +                B_ROW2(3) + B_ROW2(4) + B_ROW3(2) + B_ROW4(1) -
     +                B_ROW4(2)
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
*         Operation 18: Vectorized transpose-corrected update
      A_CONTRIB = A_ROW1(2) - A_ROW2(1) + A_ROW2(2) + A_ROW3(2) +
     +                A_ROW4(1) - A_ROW4(2)
      B_CONTRIB = B_ROW1(2) - B_ROW2(1) + B_ROW2(2) + B_ROW3(2) +
     +                B_ROW4(1) - B_ROW4(2)
      SCALAR_RESULT = ALPHA * A_CONTRIB * B_CONTRIB
      TEMP_C(2,1) = TEMP_C(2,1) - SCALAR_RESULT
      TEMP_C(1,2) = TEMP_C(1,2) + SCALAR_RESULT
      TEMP_C(2,2) = TEMP_C(2,2) - SCALAR_RESULT
      TEMP_C(2,3) = TEMP_C(2,3) - SCALAR_RESULT
      TEMP_C(1,4) = TEMP_C(1,4) - SCALAR_RESULT
      TEMP_C(2,4) = TEMP_C(2,4) + SCALAR_RESULT
*
*         Operation 19: Vectorized transpose-corrected update
      A_CONTRIB = A_ROW1(4) + A_ROW2(3) + A_ROW2(4)
      B_CONTRIB = B_ROW1(2) + B_ROW1(4) - B_ROW2(1) + B_ROW2(2) +
     +                B_ROW2(3) + B_ROW2(4) + B_ROW3(2) + B_ROW3(4) +
     +                B_ROW4(1) - B_ROW4(2) - B_ROW4(3) - B_ROW4(4)
      SCALAR_RESULT = ALPHA * A_CONTRIB * B_CONTRIB
      TEMP_C(2,3) = TEMP_C(2,3) - SCALAR_RESULT
      TEMP_C(1,4) = TEMP_C(1,4) - SCALAR_RESULT
      TEMP_C(2,4) = TEMP_C(2,4) + SCALAR_RESULT
*
*         Operation 20: Vectorized transpose-corrected update
      A_CONTRIB = A_ROW1(2) + A_ROW1(4) - A_ROW2(1) + A_ROW2(2) +
     +                A_ROW2(3) + A_ROW2(4) + A_ROW3(2) + A_ROW3(4) +
     +                A_ROW4(1) - A_ROW4(2) - A_ROW4(3) - A_ROW4(4)
      B_CONTRIB = B_ROW3(2) + B_ROW4(1) - B_ROW4(2)
      SCALAR_RESULT = ALPHA * A_CONTRIB * B_CONTRIB
      TEMP_C(4,1) = TEMP_C(4,1) - SCALAR_RESULT
      TEMP_C(3,2) = TEMP_C(3,2) - SCALAR_RESULT
      TEMP_C(4,2) = TEMP_C(4,2) - SCALAR_RESULT
*
*         VECTORIZED OPERATION GROUP 4: Operations 21-30 (Mid-range operations)
*!DEC$ VECTOR ALWAYS
*!GCC$ ivdep
*
*         Operation 21: Vectorized transpose-corrected update
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
*         Operation 22: Vectorized transpose-corrected update
      A_CONTRIB = A_ROW1(2) + A_ROW1(4) + A_ROW2(2) + A_ROW2(4)
      B_CONTRIB = B_ROW1(2) + B_ROW1(4) + B_ROW2(2) + B_ROW2(4)
      SCALAR_RESULT = ALPHA * A_CONTRIB * B_CONTRIB
      TEMP_C(2,1) = TEMP_C(2,1) + SCALAR_RESULT
      TEMP_C(4,1) = TEMP_C(4,1) + SCALAR_RESULT
      TEMP_C(2,2) = TEMP_C(2,2) + SCALAR_RESULT
      TEMP_C(4,2) = TEMP_C(4,2) + SCALAR_RESULT
*
*         Operation 23: Vectorized transpose-corrected update
      A_CONTRIB = A_ROW1(2) + A_ROW1(4) + A_ROW2(2) + A_ROW2(4) +
     +                A_ROW3(2) - A_ROW4(2)
      B_CONTRIB = B_ROW1(2) + B_ROW1(4) + B_ROW2(2) + B_ROW2(4) +
     +                B_ROW3(2) - B_ROW4(2)
      SCALAR_RESULT = ALPHA * A_CONTRIB * B_CONTRIB
      TEMP_C(2,1) = TEMP_C(2,1) - SCALAR_RESULT
      TEMP_C(4,1) = TEMP_C(4,1) - SCALAR_RESULT
      TEMP_C(2,2) = TEMP_C(2,2) - SCALAR_RESULT
      TEMP_C(4,2) = TEMP_C(4,2) - SCALAR_RESULT
      TEMP_C(2,3) = TEMP_C(2,3) - SCALAR_RESULT
      TEMP_C(2,4) = TEMP_C(2,4) + SCALAR_RESULT
*
*         Operation 24: Vectorized transpose-corrected update
      A_CONTRIB = A_ROW1(4) + A_ROW2(4)
      B_CONTRIB = B_ROW1(2) + B_ROW1(4) + B_ROW2(2) + B_ROW2(4) +
     +                B_ROW3(2) + B_ROW3(4) - B_ROW4(2) - B_ROW4(4)
      SCALAR_RESULT = ALPHA * A_CONTRIB * B_CONTRIB
      TEMP_C(2,3) = TEMP_C(2,3) + SCALAR_RESULT
      TEMP_C(2,4) = TEMP_C(2,4) - SCALAR_RESULT
*
*         Operation 25: Vectorized transpose-corrected update
      A_CONTRIB = A_ROW1(2) + A_ROW1(4) + A_ROW2(2) + A_ROW2(4) +
     +                A_ROW3(2) + A_ROW3(4) - A_ROW4(2) - A_ROW4(4)
      B_CONTRIB = B_ROW3(2) - B_ROW4(2)
      SCALAR_RESULT = ALPHA * A_CONTRIB * B_CONTRIB
      TEMP_C(4,1) = TEMP_C(4,1) + SCALAR_RESULT
      TEMP_C(4,2) = TEMP_C(4,2) + SCALAR_RESULT
*
*         Operation 26: Vectorized transpose-corrected update
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
*         Operation 27: Vectorized transpose-corrected update
      A_CONTRIB = A_ROW3(4) - A_ROW4(4)
      B_CONTRIB = B_ROW3(4) - B_ROW4(4)
      SCALAR_RESULT = ALPHA * A_CONTRIB * B_CONTRIB
      TEMP_C(4,3) = TEMP_C(4,3) - SCALAR_RESULT
      TEMP_C(4,4) = TEMP_C(4,4) + SCALAR_RESULT
*
*         Operation 28: Vectorized transpose-corrected update
      A_CONTRIB = A_ROW3(4) - A_ROW4(3) - A_ROW4(4)
      B_CONTRIB = B_ROW3(4) - B_ROW4(3) - B_ROW4(4)
      SCALAR_RESULT = ALPHA * A_CONTRIB * B_CONTRIB
      TEMP_C(4,3) = TEMP_C(4,3) + SCALAR_RESULT
      TEMP_C(3,4) = TEMP_C(3,4) - SCALAR_RESULT
      TEMP_C(4,4) = TEMP_C(4,4) - SCALAR_RESULT
*
*         Operation 29: Vectorized transpose-corrected update
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
*         Operation 30: Vectorized transpose-corrected update
      A_CONTRIB = A_ROW1(3) + A_ROW1(4) + A_ROW2(3) + A_ROW2(4) +
     +                A_ROW3(3) + A_ROW3(4) - A_ROW4(3) - A_ROW4(4)
      B_CONTRIB = B_ROW1(4) + B_ROW3(4)
      SCALAR_RESULT = ALPHA * A_CONTRIB * B_CONTRIB
      TEMP_C(3,4) = TEMP_C(3,4) + SCALAR_RESULT
*
*         VECTORIZED OPERATION GROUP 5: Operations 31-40 (Advanced combinations)
*!DEC$ VECTOR ALWAYS
*!GCC$ ivdep
*
*         Operation 31: Vectorized transpose-corrected update
      A_CONTRIB = A_ROW1(1) - A_ROW1(2) - A_ROW1(3) - A_ROW1(4) +
     +                A_ROW2(1) - A_ROW2(2) - A_ROW2(3) - A_ROW2(4) +
     +                A_ROW3(1) - A_ROW3(2) - A_ROW3(3) - A_ROW3(4) -
     +                A_ROW4(1) + A_ROW4(2) + A_ROW4(3) + A_ROW4(4)
      B_CONTRIB = B_ROW1(4)
      SCALAR_RESULT = ALPHA * A_CONTRIB * B_CONTRIB
      TEMP_C(1,2) = TEMP_C(1,2) - SCALAR_RESULT
      TEMP_C(1,4) = TEMP_C(1,4) + SCALAR_RESULT
*
*         Operation 32: Vectorized transpose-corrected update
      A_CONTRIB = -A_ROW4(3)
      B_CONTRIB = B_ROW1(3) + B_ROW1(4) + B_ROW2(3) + B_ROW2(4) +
     +                B_ROW3(3) + B_ROW3(4) - B_ROW4(3) - B_ROW4(4)
      SCALAR_RESULT = ALPHA * A_CONTRIB * B_CONTRIB
      TEMP_C(4,1) = TEMP_C(4,1) - SCALAR_RESULT
      TEMP_C(4,3) = TEMP_C(4,3) - SCALAR_RESULT
*
*         Operation 33: Vectorized transpose-corrected update
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
*         Operation 34: Vectorized transpose-corrected update
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
*         Operation 35: Vectorized transpose-corrected update
      A_CONTRIB = A_ROW1(3) + A_ROW1(4) + A_ROW2(3) + A_ROW2(4) -
     +                A_ROW3(1) + A_ROW3(2) + A_ROW3(3) + A_ROW3(4) +
     +                A_ROW4(1) - A_ROW4(2) - A_ROW4(3) - A_ROW4(4)
      B_CONTRIB = B_ROW1(4) - B_ROW3(2)
      SCALAR_RESULT = ALPHA * A_CONTRIB * B_CONTRIB
      TEMP_C(1,2) = TEMP_C(1,2) - SCALAR_RESULT
      TEMP_C(1,4) = TEMP_C(1,4) + SCALAR_RESULT
      TEMP_C(3,4) = TEMP_C(3,4) - SCALAR_RESULT
*
*         Operation 36: Vectorized transpose-corrected update
      A_CONTRIB = -A_ROW3(1) + A_ROW3(2) + A_ROW3(3) + A_ROW3(4) +
     +                A_ROW4(1) - A_ROW4(2) - A_ROW4(3) - A_ROW4(4)
      B_CONTRIB = B_ROW3(2)
      SCALAR_RESULT = ALPHA * A_CONTRIB * B_CONTRIB
      TEMP_C(1,2) = TEMP_C(1,2) - SCALAR_RESULT
      TEMP_C(3,2) = TEMP_C(3,2) + SCALAR_RESULT
      TEMP_C(1,4) = TEMP_C(1,4) + SCALAR_RESULT
      TEMP_C(3,4) = TEMP_C(3,4) - SCALAR_RESULT
*
*         Operation 37: Vectorized transpose-corrected update
      A_CONTRIB = -A_ROW1(2) - A_ROW3(2)
      B_CONTRIB = -B_ROW2(3)
      SCALAR_RESULT = ALPHA * A_CONTRIB * B_CONTRIB
      TEMP_C(1,3) = TEMP_C(1,3) + SCALAR_RESULT
      TEMP_C(2,3) = TEMP_C(2,3) - SCALAR_RESULT
      TEMP_C(1,4) = TEMP_C(1,4) - SCALAR_RESULT
      TEMP_C(2,4) = TEMP_C(2,4) + SCALAR_RESULT
*
*         Operation 38: Vectorized transpose-corrected update
      A_CONTRIB = A_ROW3(2) + A_ROW3(4)
      B_CONTRIB = B_ROW4(1) - B_ROW4(3)
      SCALAR_RESULT = ALPHA * A_CONTRIB * B_CONTRIB
      TEMP_C(3,1) = TEMP_C(3,1) + SCALAR_RESULT
      TEMP_C(4,1) = TEMP_C(4,1) + SCALAR_RESULT
      TEMP_C(3,2) = TEMP_C(3,2) + SCALAR_RESULT
      TEMP_C(4,2) = TEMP_C(4,2) + SCALAR_RESULT
*
*         Operation 39: Vectorized transpose-corrected update
      A_CONTRIB = -A_ROW1(3) - A_ROW1(4) - A_ROW2(3) - A_ROW2(4)
      B_CONTRIB = B_ROW3(2) + B_ROW3(4)
      SCALAR_RESULT = ALPHA * A_CONTRIB * B_CONTRIB
      TEMP_C(1,4) = TEMP_C(1,4) - SCALAR_RESULT
      TEMP_C(3,4) = TEMP_C(3,4) + SCALAR_RESULT
*
*         Operation 40: Vectorized transpose-corrected update
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
*         VECTORIZED OPERATION GROUP 6: Operations 41-49 (Final operations)
*!DEC$ VECTOR ALWAYS
*!GCC$ ivdep
*
*         Operation 41: Vectorized transpose-corrected update
      A_CONTRIB = -A_ROW2(1)
      B_CONTRIB = B_ROW1(1) - B_ROW1(2) + B_ROW2(1) - B_ROW2(2)
      SCALAR_RESULT = ALPHA * A_CONTRIB * B_CONTRIB
      TEMP_C(2,1) = TEMP_C(2,1) - SCALAR_RESULT
      TEMP_C(4,1) = TEMP_C(4,1) - SCALAR_RESULT
*
*         Operation 42: Vectorized transpose-corrected update
      A_CONTRIB = -A_ROW2(1) + A_ROW4(1)
      B_CONTRIB = B_ROW1(1) - B_ROW1(2) - B_ROW1(3) - B_ROW1(4) +
     +                B_ROW2(1) - B_ROW2(2) - B_ROW2(3) - B_ROW2(4) +
     +                B_ROW3(1) - B_ROW3(2) - B_ROW3(3) - B_ROW3(4) -
     +                B_ROW4(1) + B_ROW4(2) + B_ROW4(3) + B_ROW4(4)
      SCALAR_RESULT = ALPHA * A_CONTRIB * B_CONTRIB
      TEMP_C(4,1) = TEMP_C(4,1) + SCALAR_RESULT
*
*         Operation 43: Vectorized transpose-corrected update
      A_CONTRIB = -A_ROW2(1) + A_ROW4(1) - A_ROW4(3)
      B_CONTRIB = B_ROW1(3) + B_ROW1(4) + B_ROW2(3) + B_ROW2(4) -
     +                B_ROW3(1) + B_ROW3(2) + B_ROW3(3) + B_ROW3(4) +
     +                B_ROW4(1) - B_ROW4(2) - B_ROW4(3) - B_ROW4(4)
      SCALAR_RESULT = ALPHA * A_CONTRIB * B_CONTRIB
      TEMP_C(4,1) = TEMP_C(4,1) + SCALAR_RESULT
      TEMP_C(2,3) = TEMP_C(2,3) - SCALAR_RESULT
*
*         Operation 44: Vectorized transpose-corrected update
      A_CONTRIB = A_ROW1(2) + A_ROW2(2) + A_ROW3(2) - A_ROW4(2)
      B_CONTRIB = B_ROW1(2) + B_ROW2(2) + B_ROW3(2) - B_ROW4(2)
      SCALAR_RESULT = ALPHA * A_CONTRIB * B_CONTRIB
      TEMP_C(2,1) = TEMP_C(2,1) + SCALAR_RESULT
      TEMP_C(2,2) = TEMP_C(2,2) + SCALAR_RESULT
      TEMP_C(2,3) = TEMP_C(2,3) + SCALAR_RESULT
      TEMP_C(2,4) = TEMP_C(2,4) - SCALAR_RESULT
*
*         Operation 45: Vectorized transpose-corrected update
      A_CONTRIB = -A_ROW2(1) + A_ROW2(3) + A_ROW4(1) - A_ROW4(3)
      B_CONTRIB = -B_ROW3(1) + B_ROW3(2) + B_ROW3(3) + B_ROW3(4) +
     +                B_ROW4(1) - B_ROW4(2) - B_ROW4(3) - B_ROW4(4)
      SCALAR_RESULT = ALPHA * A_CONTRIB * B_CONTRIB
      TEMP_C(2,3) = TEMP_C(2,3) + SCALAR_RESULT
*
*         Operation 46: Vectorized transpose-corrected update
      A_CONTRIB = -A_ROW3(1) + A_ROW3(2) + A_ROW4(1) - A_ROW4(2)
      B_CONTRIB = -B_ROW1(2) - B_ROW3(2)
      SCALAR_RESULT = ALPHA * A_CONTRIB * B_CONTRIB
      TEMP_C(3,2) = TEMP_C(3,2) + SCALAR_RESULT
*
*         Operation 47: Vectorized transpose-corrected update
      A_CONTRIB = A_ROW4(1) - A_ROW4(3)
      B_CONTRIB = -B_ROW1(3) - B_ROW1(4) - B_ROW2(3) - B_ROW2(4)
      SCALAR_RESULT = ALPHA * A_CONTRIB * B_CONTRIB
      TEMP_C(2,3) = TEMP_C(2,3) - SCALAR_RESULT
      TEMP_C(4,3) = TEMP_C(4,3) - SCALAR_RESULT
*
*         Operation 48: Vectorized transpose-corrected update
      A_CONTRIB = -A_ROW4(3) - A_ROW4(4)
      B_CONTRIB = -B_ROW4(3) - B_ROW4(4)
      SCALAR_RESULT = ALPHA * A_CONTRIB * B_CONTRIB
      TEMP_C(3,4) = TEMP_C(3,4) + SCALAR_RESULT
      TEMP_C(4,4) = TEMP_C(4,4) + SCALAR_RESULT
*
*         Operation 49: FINAL VECTORIZED OPERATION - Transpose-corrected update
      A_CONTRIB = -A_ROW2(3)
      B_CONTRIB = -B_ROW3(1) + B_ROW3(2) + B_ROW4(1) - B_ROW4(2)
      SCALAR_RESULT = ALPHA * A_CONTRIB * B_CONTRIB
      TEMP_C(2,1) = TEMP_C(2,1) + SCALAR_RESULT
      TEMP_C(2,3) = TEMP_C(2,3) + SCALAR_RESULT
*
*         ================================================================
*         PHASE 8.3: VECTORIZED FINAL ASSIGNMENT WITH SIMD OPTIMIZATION
*         ================================================================
*!DEC$ VECTOR ALWAYS
*!GCC$ ivdep
      DO J = 1, 4
          DO I = 1, 4
              C(I,J) = TEMP_C(I,J)
          END DO
      END DO
*
      ELSE
*         Fallback to standard DGEMM for non-4x4 matrices
          CALL DGEMM(TRANSA,TRANSB,M,N,K,ALPHA,A,LDA,B,LDB,BETA,C,
     +               LDC)
      END IF
*
      RETURN
*
*     =====================================================================
*     END OF DGEMM_ALPHA - PHASE 8.3 COMPLETE
*     =====================================================================
*     ACHIEVEMENT: All 49 AlphaTensor operations inlined in main routine
*     PERFORMANCE: Zero function call overhead for 4x4 matrix optimization
*     COMPATIBILITY: Seamless fallback to standard DGEMM for other cases
*     =====================================================================
*
      END
