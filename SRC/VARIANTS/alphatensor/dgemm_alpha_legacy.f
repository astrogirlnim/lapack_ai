      SUBROUTINE DGEMM_ALPHA(TRANSA,TRANSB,M,N,K,ALPHA,A,LDA,B,LDB,
     +                        BETA,C,LDC)
*
*  -- LAPACK AI Modernization Project --
*  -- AlphaTensor Matrix Multiplication Implementation --
*  -- CORRECT implementation based on DeepMind's algorithm_from_factors --
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
      INTEGER ALGO_CHOICE
      INTEGER LOG_UNIT
      PARAMETER (LOG_UNIT=6)
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
      WRITE(LOG_UNIT,*) 'DGEMM_ALPHA: Algorithm selection analysis'
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
*     Algorithm selection logic
      IS_4X4 = (M.EQ.4 .AND. N.EQ.4 .AND. K.EQ.4)
      NO_TRANSPOSE = (NOTA .AND. NOTB)
      USE_ALPHA = (IS_4X4 .AND. NO_TRANSPOSE)
*
      WRITE(LOG_UNIT,*) 'DGEMM_ALPHA: Matrix dimensions M=',M,
     +                  ' N=',N,' K=',K
      WRITE(LOG_UNIT,*) 'DGEMM_ALPHA: IS_4X4=',IS_4X4,
     +                  ' NO_TRANSPOSE=',NO_TRANSPOSE
      WRITE(LOG_UNIT,*) 'DGEMM_ALPHA: USE_ALPHA=',USE_ALPHA
*
      IF (USE_ALPHA) THEN
*
*         === CORRECT ALPHATENSOR ALGORITHM ===
*
          ALGO_CHOICE = 1
          WRITE(LOG_UNIT,*) 'DGEMM_ALPHA: Using CORRECT AlphaTensor'
          WRITE(LOG_UNIT,*) 'DGEMM_ALPHA: Linear combination algorithm'
*
*         Call CORRECT AlphaTensor implementation
          CALL DGEMM_ALPHATENSOR_CORRECT(ALPHA, A, LDA, B, LDB, BETA,
     +                                   C, LDC)
*
          WRITE(LOG_UNIT,*) 'DGEMM_ALPHA: CORRECT computation complete'
*
      ELSE
*
*         === STANDARD DGEMM FALLBACK PATH ===
*
          ALGO_CHOICE = 2
          WRITE(LOG_UNIT,*) 'DGEMM_ALPHA: Using standard DGEMM fallback'
*
          CALL DGEMM(TRANSA,TRANSB,M,N,K,ALPHA,A,LDA,B,LDB,BETA,C,
     +               LDC)
*
          WRITE(LOG_UNIT,*) 'DGEMM_ALPHA: Standard DGEMM complete'
*
      END IF
*
      WRITE(LOG_UNIT,*) 'DGEMM_ALPHA: Algorithm choice =',ALGO_CHOICE
*
      RETURN
*
*     End of DGEMM_ALPHA
*
      END
*
*     ================================================================
*
      SUBROUTINE DGEMM_ALPHATENSOR_CORRECT(ALPHA, A, LDA, B, LDB,
     +                                     BETA, C, LDC)
*
*  -- CORRECT AlphaTensor 4x4 Algorithm --
*  -- Using DeepMind's exact factor extraction approach --
*  -- Raw u[:,r], v[:,r], w[:,r] coefficients applied directly --
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
      INTEGER I, J, R, ROW, COL, LINEAR_IDX
      DOUBLE PRECISION A_CONTRIB, B_CONTRIB, SCALAR_RESULT
      DOUBLE PRECISION TEMP_VALUE
      DOUBLE PRECISION TEMP_RESULT(4,4), TRANSPOSED_RESULT(4,4)
      INTEGER LOG_UNIT
      PARAMETER (LOG_UNIT=6)
*     ..
*     .. Constants ..
      DOUBLE PRECISION ZERO
      PARAMETER (ZERO=0.0D+0)
*     ..
*
      WRITE(LOG_UNIT,*) 'DIRECT2D: Starting mapping approach'
      WRITE(LOG_UNIT,*) 'DIRECT2D: No flattening required'
      WRITE(LOG_UNIT,*) 'DIRECT2D: Using DeepMind coefficients'
      WRITE(LOG_UNIT,*) 'DIRECT2D: Direct 2D matrix access'
      WRITE(LOG_UNIT,*) 'DIRECT2D: TRANSPOSE FIX APPLIED'
*
*     Initialize temporary result matrix to zeros
      DO I = 1, 4
          DO J = 1, 4
              TEMP_RESULT(I,J) = ZERO
          END DO
      END DO
*
*     Apply all 49 DeepMind operations using raw factors
*     Each operation r: a_contrib = u[:,r]·A_flat, b_contrib = v[:,r]·B_flat
*     scalar = a_contrib * b_contrib, result_flat += w[:,r] * scalar
*
*     Operation 1 (r=0): u[:,0], v[:,0], w[:,0]
*     A_FLAT(1)→A(1,1), A_FLAT(9)→A(3,1), RESULT_FLAT(1)→C(1,1), RESULT_FLAT(9)→C(3,1)
      A_CONTRIB = A(1,1) + A(3,1)
      B_CONTRIB = B(1,1) + B(3,1)
      SCALAR_RESULT = A_CONTRIB * B_CONTRIB
      TEMP_RESULT(1,1) = TEMP_RESULT(1,1) + SCALAR_RESULT
      TEMP_RESULT(3,1) = TEMP_RESULT(3,1) + SCALAR_RESULT
*
*     Operation 2 (r=1): u[:,1], v[:,1], w[:,1]
*     A_FLAT(1)→A(1,1), A_FLAT(3)→A(1,3), A_FLAT(9)→A(3,1)
      A_CONTRIB = A(1,1) - A(1,3) + A(3,1)
      B_CONTRIB = B(1,1) - B(1,3) + B(3,1)
      SCALAR_RESULT = A_CONTRIB * B_CONTRIB
      TEMP_RESULT(1,1) = TEMP_RESULT(1,1) - SCALAR_RESULT
      TEMP_RESULT(1,3) = TEMP_RESULT(1,3) + SCALAR_RESULT
      TEMP_RESULT(3,1) = TEMP_RESULT(3,1) - SCALAR_RESULT
*
*     Operation 3 (r=2): u[:,2], v[:,2], w[:,2]
*     A_FLAT(3)→A(1,3), B_FLAT(1)→B(1,1), B_FLAT(3)→B(1,3), B_FLAT(9)→B(3,1), B_FLAT(11)→B(3,3)
      A_CONTRIB = -A(1,3)
      B_CONTRIB = B(1,1) - B(1,3) + B(3,1) - B(3,3)
      SCALAR_RESULT = A_CONTRIB * B_CONTRIB
      TEMP_RESULT(3,1) = TEMP_RESULT(3,1) + SCALAR_RESULT
*
*     Operation 4 (r=3): u[:,3], v[:,3], w[:,3]
*     A_FLAT(11)→A(3,3), B_FLAT(11)→B(3,3)
      A_CONTRIB = -A(3,3)
      B_CONTRIB = -B(3,3)
      SCALAR_RESULT = A_CONTRIB * B_CONTRIB
      TEMP_RESULT(3,3) = TEMP_RESULT(3,3) + SCALAR_RESULT
*
*     Operation 5 (r=4): u[:,4], v[:,4], w[:,4]
*     A_FLAT(9)→A(3,1), B_FLAT(3)→B(1,3)
      A_CONTRIB = -A(3,1)
      B_CONTRIB = -B(1,3)
      SCALAR_RESULT = A_CONTRIB * B_CONTRIB
      TEMP_RESULT(1,1) = TEMP_RESULT(1,1) - SCALAR_RESULT
      TEMP_RESULT(1,3) = TEMP_RESULT(1,3) + SCALAR_RESULT
      TEMP_RESULT(3,1) = TEMP_RESULT(3,1) - SCALAR_RESULT
      TEMP_RESULT(3,3) = TEMP_RESULT(3,3) + SCALAR_RESULT
*
*     Operation 6 (r=5): u[:,5], v[:,5], w[:,5]
*     A_FLAT(1)→A(1,1), A_FLAT(3)→A(1,3), A_FLAT(9)→A(3,1), A_FLAT(11)→A(3,3), B_FLAT(9)→B(3,1)
      A_CONTRIB = A(1,1) - A(1,3) + A(3,1) - A(3,3)
      B_CONTRIB = -B(3,1)
      SCALAR_RESULT = A_CONTRIB * B_CONTRIB
      TEMP_RESULT(1,3) = TEMP_RESULT(1,3) + SCALAR_RESULT
      WRITE(LOG_UNIT,*) 'OP6: A_CONTRIB=',A_CONTRIB,' B_CONTRIB=',
     +                  B_CONTRIB,' SCALAR=',SCALAR_RESULT
      WRITE(LOG_UNIT,*) 'OP6: C(1,3)=',C(1,3)
*
*     Operation 7 (r=6): Key operation for row 2
*     A_FLAT(5-8)→A(2,1-4), B_FLAT(5-8)→B(2,1-4)
      A_CONTRIB = -A(2,1) + A(2,2) - A(2,3) - A(2,4)
      B_CONTRIB = -B(2,1) + B(2,2) - B(2,3) - B(2,4)
      SCALAR_RESULT = A_CONTRIB * B_CONTRIB
      TEMP_RESULT(2,1) = TEMP_RESULT(2,1) - SCALAR_RESULT
      TEMP_RESULT(2,2) = TEMP_RESULT(2,2) + SCALAR_RESULT
      TEMP_RESULT(2,3) = TEMP_RESULT(2,3) - SCALAR_RESULT
      TEMP_RESULT(2,4) = TEMP_RESULT(2,4) - SCALAR_RESULT
      WRITE(LOG_UNIT,*) 'OP7: A_CONTRIB=',A_CONTRIB,' B_CONTRIB=',
     +                  B_CONTRIB,' SCALAR=',SCALAR_RESULT
*
*
*     Operation 8 (r=7): Important for matrix structure
*     A_FLAT(5-8,13-14)→A(2,1-4,4,1-2), RESULT_FLAT(5-8,13-14)→C(2,1-4,4,1-2)
      A_CONTRIB = -A(2,1) + A(2,2) - A(2,3) - A(2,4) -
     +            A(4,1) + A(4,2)
      B_CONTRIB = -B(2,1) + B(2,2) - B(2,3) - B(2,4) -
     +            B(4,1) + B(4,2)
      SCALAR_RESULT = A_CONTRIB * B_CONTRIB
      TEMP_RESULT(2,1) = TEMP_RESULT(2,1) + SCALAR_RESULT
      TEMP_RESULT(2,2) = TEMP_RESULT(2,2) - SCALAR_RESULT
      TEMP_RESULT(2,3) = TEMP_RESULT(2,3) + SCALAR_RESULT
      TEMP_RESULT(2,4) = TEMP_RESULT(2,4) + SCALAR_RESULT
      TEMP_RESULT(4,1) = TEMP_RESULT(4,1) + SCALAR_RESULT
      TEMP_RESULT(4,2) = TEMP_RESULT(4,2) - SCALAR_RESULT
      WRITE(LOG_UNIT,*) 'OP8: A_CONTRIB=',A_CONTRIB,' B_CONTRIB=',
     +                  B_CONTRIB,' SCALAR=',SCALAR_RESULT
      WRITE(LOG_UNIT,*) 'OP8: C matrix row 2 after op8:'
      WRITE(LOG_UNIT,*) (C(2,J), J=1,4)
      WRITE(LOG_UNIT,*) 'OP8: C matrix row 4 after op8:'
      WRITE(LOG_UNIT,*) (C(4,J), J=1,2)
*
*     Operation 9 (r=8): Key for corners
*     A_FLAT(1,3)→A(1,1,3), RESULT_FLAT(1,3)→C(1,1,3)
      A_CONTRIB = A(1,1) - A(1,3)
      B_CONTRIB = B(1,1) - B(1,3)
      SCALAR_RESULT = A_CONTRIB * B_CONTRIB
      TEMP_RESULT(1,1) = TEMP_RESULT(1,1) + SCALAR_RESULT
      TEMP_RESULT(1,3) = TEMP_RESULT(1,3) - SCALAR_RESULT
      WRITE(LOG_UNIT,*) 'OP9: A_CONTRIB=',A_CONTRIB,' B_CONTRIB=',
     +                  B_CONTRIB,' SCALAR=',SCALAR_RESULT
*
*     Operation 10 (r=9): Row pattern
*     A_FLAT(5,6,13,14)→A(2,1,2,4,1,2), RESULT_FLAT(5,6,13,14)→C(2,1,2,4,1,2)
      A_CONTRIB = -A(2,1) + A(2,2) - A(4,1) + A(4,2)
      B_CONTRIB = -B(2,1) + B(2,2) - B(4,1) + B(4,2)
      SCALAR_RESULT = A_CONTRIB * B_CONTRIB
      TEMP_RESULT(2,1) = TEMP_RESULT(2,1) - SCALAR_RESULT
      TEMP_RESULT(2,2) = TEMP_RESULT(2,2) + SCALAR_RESULT
      TEMP_RESULT(4,1) = TEMP_RESULT(4,1) - SCALAR_RESULT
      TEMP_RESULT(4,2) = TEMP_RESULT(4,2) + SCALAR_RESULT
      WRITE(LOG_UNIT,*) 'OP10: A_CONTRIB=',A_CONTRIB,' B_CONTRIB=',
     +                  B_CONTRIB,' SCALAR=',SCALAR_RESULT
*
*     Operation 11 (r=10): Complex pattern
*     A_FLAT(13,14)→A(4,1,2), B_FLAT(7,8)→B(2,3,4), Multiple C updates
      A_CONTRIB = A(4,1) - A(4,2)
      B_CONTRIB = -B(2,3) - B(2,4)
      SCALAR_RESULT = A_CONTRIB * B_CONTRIB
      TEMP_RESULT(2,1) = TEMP_RESULT(2,1) + SCALAR_RESULT
      TEMP_RESULT(2,2) = TEMP_RESULT(2,2) - SCALAR_RESULT
      TEMP_RESULT(2,3) = TEMP_RESULT(2,3) + SCALAR_RESULT
      TEMP_RESULT(2,4) = TEMP_RESULT(2,4) + SCALAR_RESULT
      TEMP_RESULT(4,1) = TEMP_RESULT(4,1) + SCALAR_RESULT
      TEMP_RESULT(4,2) = TEMP_RESULT(4,2) - SCALAR_RESULT
      TEMP_RESULT(4,3) = TEMP_RESULT(4,3) + SCALAR_RESULT
      TEMP_RESULT(4,4) = TEMP_RESULT(4,4) + SCALAR_RESULT
*
*     Operation 12 (r=11): Pair operation
*     Complex A pattern, B_FLAT(13,14)→B(4,1,2), RESULT_FLAT(7,8)→C(2,3,4)
      A_CONTRIB = -A(2,1) + A(2,2) - A(2,3) - A(2,4) -
     +            A(4,1) + A(4,2) - A(4,3) - A(4,4)
      B_CONTRIB = B(4,1) - B(4,2)
      SCALAR_RESULT = A_CONTRIB * B_CONTRIB
      TEMP_RESULT(2,3) = TEMP_RESULT(2,3) + SCALAR_RESULT
      TEMP_RESULT(2,4) = TEMP_RESULT(2,4) + SCALAR_RESULT
*
*     Operation 13 (r=12): Exact DeepMind coefficients
*     A_FLAT(7,8)→A(2,3,4), Complex B pattern, RESULT_FLAT(13,14)→C(4,1,2)
      A_CONTRIB = -A(2,3) - A(2,4)
      B_CONTRIB = -B(2,1) + B(2,2) - B(2,3) - B(2,4) -
     +            B(4,1) + B(4,2) - B(4,3) - B(4,4)
      SCALAR_RESULT = A_CONTRIB * B_CONTRIB
      TEMP_RESULT(4,1) = TEMP_RESULT(4,1) - SCALAR_RESULT
      TEMP_RESULT(4,2) = TEMP_RESULT(4,2) + SCALAR_RESULT
*
*     Operation 14 (r=13): Simple pattern
*     A_FLAT(1,2,5,6)→A(1,1,2,2,1,2), B_FLAT(2,4)→B(1,2,4), RESULT_FLAT(5)→C(2,1)
      A_CONTRIB = A(1,1) - A(1,2) + A(2,1) - A(2,2)
      B_CONTRIB = -B(1,2) - B(1,4)
      SCALAR_RESULT = A_CONTRIB * B_CONTRIB
      TEMP_RESULT(2,1) = TEMP_RESULT(2,1) - SCALAR_RESULT
*
*     Operation 15 (r=14): Symmetric
*     A_FLAT(2,4)→A(1,2,4), B_FLAT(5)→B(2,1), Multiple C updates
      A_CONTRIB = -A(1,2) - A(1,4)
      B_CONTRIB = -B(2,1)
      SCALAR_RESULT = A_CONTRIB * B_CONTRIB
      TEMP_RESULT(1,1) = TEMP_RESULT(1,1) + SCALAR_RESULT
      TEMP_RESULT(1,2) = TEMP_RESULT(1,2) - SCALAR_RESULT
      TEMP_RESULT(2,1) = TEMP_RESULT(2,1) + SCALAR_RESULT
      TEMP_RESULT(2,2) = TEMP_RESULT(2,2) - SCALAR_RESULT
*
*     Operation 16 (r=15): Exact DeepMind coefficients
*     Complex pattern: A_FLAT(2,4,5-8)→A(1,2,4,2,1-4), Multiple C updates
      A_CONTRIB = A(1,2) + A(1,4) - A(2,1) + A(2,2) +
     +            A(2,3) + A(2,4)
      B_CONTRIB = B(1,2) + B(1,4) - B(2,1) + B(2,2) +
     +            B(2,3) + B(2,4)
      SCALAR_RESULT = A_CONTRIB * B_CONTRIB
      TEMP_RESULT(1,2) = TEMP_RESULT(1,2) - SCALAR_RESULT
      TEMP_RESULT(1,4) = TEMP_RESULT(1,4) - SCALAR_RESULT
      TEMP_RESULT(2,1) = TEMP_RESULT(2,1) + SCALAR_RESULT
      TEMP_RESULT(2,2) = TEMP_RESULT(2,2) - SCALAR_RESULT
      TEMP_RESULT(2,3) = TEMP_RESULT(2,3) - SCALAR_RESULT
      TEMP_RESULT(2,4) = TEMP_RESULT(2,4) - SCALAR_RESULT
*
*     Operation 17 (r=16): Exact DeepMind coefficients
*     Very complex pattern with many elements from multiple rows/cols
      A_CONTRIB = A(1,2) + A(1,4) - A(2,1) + A(2,2) +
     +            A(2,3) + A(2,4) + A(3,2) + A(4,1) -
     +            A(4,2)
      B_CONTRIB = B(1,2) + B(1,4) - B(2,1) + B(2,2) +
     +            B(2,3) + B(2,4) + B(3,2) + B(4,1) -
     +            B(4,2)
      SCALAR_RESULT = A_CONTRIB * B_CONTRIB
      TEMP_RESULT(1,2) = TEMP_RESULT(1,2) + SCALAR_RESULT
      TEMP_RESULT(1,4) = TEMP_RESULT(1,4) + SCALAR_RESULT
      TEMP_RESULT(2,1) = TEMP_RESULT(2,1) - SCALAR_RESULT
      TEMP_RESULT(2,2) = TEMP_RESULT(2,2) + SCALAR_RESULT
      TEMP_RESULT(2,3) = TEMP_RESULT(2,3) + SCALAR_RESULT
      TEMP_RESULT(2,4) = TEMP_RESULT(2,4) + SCALAR_RESULT
      TEMP_RESULT(3,2) = TEMP_RESULT(3,2) + SCALAR_RESULT
      TEMP_RESULT(4,1) = TEMP_RESULT(4,1) + SCALAR_RESULT
      TEMP_RESULT(4,2) = TEMP_RESULT(4,2) - SCALAR_RESULT
*
*     Operation 18 (r=17): Exact DeepMind coefficients
*     A_FLAT(2,5,6,10,13,14)→A(1,2,2,1,2,3,2,4,1,2), Multiple C updates
      A_CONTRIB = A(1,2) - A(2,1) + A(2,2) + A(3,2) +
     +            A(4,1) - A(4,2)
      B_CONTRIB = B(1,2) - B(2,1) + B(2,2) + B(3,2) +
     +            B(4,1) - B(4,2)
      SCALAR_RESULT = A_CONTRIB * B_CONTRIB
      TEMP_RESULT(1,2) = TEMP_RESULT(1,2) - SCALAR_RESULT
      TEMP_RESULT(2,1) = TEMP_RESULT(2,1) + SCALAR_RESULT
      TEMP_RESULT(2,2) = TEMP_RESULT(2,2) - SCALAR_RESULT
      TEMP_RESULT(3,2) = TEMP_RESULT(3,2) - SCALAR_RESULT
      TEMP_RESULT(4,1) = TEMP_RESULT(4,1) - SCALAR_RESULT
      TEMP_RESULT(4,2) = TEMP_RESULT(4,2) + SCALAR_RESULT
*
*     Operation 19 (r=18): Exact DeepMind coefficients
*     Simple A, very complex B pattern, limited C updates
      A_CONTRIB = A(1,4) + A(2,3) + A(2,4)
      B_CONTRIB = B(1,2) + B(1,4) - B(2,1) + B(2,2) +
     +            B(2,3) + B(2,4) + B(3,2) + B(3,4) +
     +            B(4,1) - B(4,2) - B(4,3) - B(4,4)
      SCALAR_RESULT = A_CONTRIB * B_CONTRIB
      TEMP_RESULT(3,2) = TEMP_RESULT(3,2) - SCALAR_RESULT
      TEMP_RESULT(4,1) = TEMP_RESULT(4,1) - SCALAR_RESULT
      TEMP_RESULT(4,2) = TEMP_RESULT(4,2) + SCALAR_RESULT
*
*     Operation 20 (r=19): Exact DeepMind coefficients
*     Very complex A pattern, simple B pattern, limited C updates
      A_CONTRIB = A(1,2) + A(1,4) - A(2,1) + A(2,2) +
     +            A(2,3) + A(2,4) + A(3,2) + A(3,4) +
     +            A(4,1) - A(4,2) - A(4,3) - A(4,4)
      B_CONTRIB = B(3,2) + B(4,1) - B(4,2)
      SCALAR_RESULT = A_CONTRIB * B_CONTRIB
      TEMP_RESULT(1,4) = TEMP_RESULT(1,4) - SCALAR_RESULT
      TEMP_RESULT(2,3) = TEMP_RESULT(2,3) - SCALAR_RESULT
      TEMP_RESULT(2,4) = TEMP_RESULT(2,4) - SCALAR_RESULT
      WRITE(LOG_UNIT,*) 'OP20: A_CONTRIB=',A_CONTRIB,' B_CONTRIB=',
     +                  B_CONTRIB,' SCALAR=',SCALAR_RESULT
*
      WRITE(LOG_UNIT,*) 'ALPHATENSOR_CORRECT: 20 ops complete'
*
*     Operation 21 (r=20): Exact DeepMind coefficients
*     Simple A pattern, simple B pattern, but many C updates
      A_CONTRIB = A(3,2) + A(4,1) - A(4,2)
      B_CONTRIB = B(1,4) + B(2,3) + B(2,4)
      SCALAR_RESULT = A_CONTRIB * B_CONTRIB
      TEMP_RESULT(1,2) = TEMP_RESULT(1,2) - SCALAR_RESULT
      TEMP_RESULT(1,4) = TEMP_RESULT(1,4) - SCALAR_RESULT
      TEMP_RESULT(2,1) = TEMP_RESULT(2,1) + SCALAR_RESULT
      TEMP_RESULT(2,2) = TEMP_RESULT(2,2) - SCALAR_RESULT
      TEMP_RESULT(2,3) = TEMP_RESULT(2,3) - SCALAR_RESULT
      TEMP_RESULT(2,4) = TEMP_RESULT(2,4) - SCALAR_RESULT
      TEMP_RESULT(3,2) = TEMP_RESULT(3,2) - SCALAR_RESULT
      TEMP_RESULT(3,4) = TEMP_RESULT(3,4) - SCALAR_RESULT
      TEMP_RESULT(4,1) = TEMP_RESULT(4,1) - SCALAR_RESULT
      TEMP_RESULT(4,2) = TEMP_RESULT(4,2) + SCALAR_RESULT
      TEMP_RESULT(4,3) = TEMP_RESULT(4,3) + SCALAR_RESULT
      TEMP_RESULT(4,4) = TEMP_RESULT(4,4) + SCALAR_RESULT
*
*     Operation 22 (r=21): Exact DeepMind coefficients
*     Simple symmetric pattern: corners and sides
      A_CONTRIB = A(1,2) + A(1,4) + A(2,2) + A(2,4)
      B_CONTRIB = B(1,2) + B(1,4) + B(2,2) + B(2,4)
      SCALAR_RESULT = A_CONTRIB * B_CONTRIB
      TEMP_RESULT(1,2) = TEMP_RESULT(1,2) + SCALAR_RESULT
      TEMP_RESULT(1,4) = TEMP_RESULT(1,4) + SCALAR_RESULT
      TEMP_RESULT(2,2) = TEMP_RESULT(2,2) + SCALAR_RESULT
      TEMP_RESULT(2,4) = TEMP_RESULT(2,4) + SCALAR_RESULT
*
*     Operation 23 (r=22): Exact DeepMind coefficients
*     Extended pattern from Op22 with additional elements
      A_CONTRIB = A(1,2) + A(1,4) + A(2,2) + A(2,4) +
     +            A(3,2) - A(4,2)
      B_CONTRIB = B(1,2) + B(1,4) + B(2,2) + B(2,4) +
     +            B(3,2) - B(4,2)
      SCALAR_RESULT = A_CONTRIB * B_CONTRIB
      TEMP_RESULT(1,2) = TEMP_RESULT(1,2) - SCALAR_RESULT
      TEMP_RESULT(1,4) = TEMP_RESULT(1,4) - SCALAR_RESULT
      TEMP_RESULT(2,2) = TEMP_RESULT(2,2) - SCALAR_RESULT
      TEMP_RESULT(2,4) = TEMP_RESULT(2,4) - SCALAR_RESULT
      TEMP_RESULT(3,2) = TEMP_RESULT(3,2) - SCALAR_RESULT
      TEMP_RESULT(4,2) = TEMP_RESULT(4,2) + SCALAR_RESULT
*
*     Operation 24 (r=23): Exact DeepMind coefficients
*     Simple A pattern, complex B pattern, limited C updates
      A_CONTRIB = A(1,4) + A(2,4)
      B_CONTRIB = B(1,2) + B(1,4) + B(2,2) + B(2,4) +
     +            B(3,2) + B(3,4) - B(4,2) - B(4,4)
      SCALAR_RESULT = A_CONTRIB * B_CONTRIB
      TEMP_RESULT(3,2) = TEMP_RESULT(3,2) + SCALAR_RESULT
      TEMP_RESULT(4,2) = TEMP_RESULT(4,2) - SCALAR_RESULT
*
*     Operation 25 (r=24): Exact DeepMind coefficients
*     Very complex A pattern, simple B pattern, limited C updates
      A_CONTRIB = A(1,2) + A(1,4) + A(2,2) + A(2,4) +
     +            A(3,2) + A(3,4) - A(4,2) - A(4,4)
      B_CONTRIB = B(3,2) - B(4,2)
      SCALAR_RESULT = A_CONTRIB * B_CONTRIB
      TEMP_RESULT(1,4) = TEMP_RESULT(1,4) + SCALAR_RESULT
      TEMP_RESULT(2,4) = TEMP_RESULT(2,4) + SCALAR_RESULT
*
*     Operation 26 (r=25): Exact DeepMind coefficients
*     Simple A and B patterns, many C updates
      A_CONTRIB = A(3,2) - A(4,2)
      B_CONTRIB = B(1,4) + B(2,4)
      SCALAR_RESULT = A_CONTRIB * B_CONTRIB
      TEMP_RESULT(1,2) = TEMP_RESULT(1,2) + SCALAR_RESULT
      TEMP_RESULT(1,4) = TEMP_RESULT(1,4) + SCALAR_RESULT
      TEMP_RESULT(2,2) = TEMP_RESULT(2,2) + SCALAR_RESULT
      TEMP_RESULT(2,4) = TEMP_RESULT(2,4) + SCALAR_RESULT
      TEMP_RESULT(3,2) = TEMP_RESULT(3,2) + SCALAR_RESULT
      TEMP_RESULT(3,4) = TEMP_RESULT(3,4) + SCALAR_RESULT
      TEMP_RESULT(4,2) = TEMP_RESULT(4,2) - SCALAR_RESULT
      TEMP_RESULT(4,4) = TEMP_RESULT(4,4) - SCALAR_RESULT
*
*     Operation 27 (r=26): Exact DeepMind coefficients
*     Simple symmetric pattern: bottom-right corner elements
      A_CONTRIB = A(3,4) - A(4,4)
      B_CONTRIB = B(3,4) - B(4,4)
      SCALAR_RESULT = A_CONTRIB * B_CONTRIB
      TEMP_RESULT(3,4) = TEMP_RESULT(3,4) - SCALAR_RESULT
      TEMP_RESULT(4,4) = TEMP_RESULT(4,4) + SCALAR_RESULT
*
*     Operation 28 (r=27): Exact DeepMind coefficients
*     Bottom-right 3 elements: (3,4), (4,3), (4,4)
      A_CONTRIB = A(3,4) - A(4,3) - A(4,4)
      B_CONTRIB = B(3,4) - B(4,3) - B(4,4)
      SCALAR_RESULT = A_CONTRIB * B_CONTRIB
      TEMP_RESULT(3,4) = TEMP_RESULT(3,4) + SCALAR_RESULT
      TEMP_RESULT(4,3) = TEMP_RESULT(4,3) - SCALAR_RESULT
      TEMP_RESULT(4,4) = TEMP_RESULT(4,4) - SCALAR_RESULT
*
*     Operation 29 (r=28): Exact DeepMind coefficients
*     Simple A and B patterns, many C updates (right side and bottom)
      A_CONTRIB = A(1,4) + A(3,4)
      B_CONTRIB = -B(4,3)
      SCALAR_RESULT = A_CONTRIB * B_CONTRIB
      TEMP_RESULT(1,3) = TEMP_RESULT(1,3) - SCALAR_RESULT
      TEMP_RESULT(1,4) = TEMP_RESULT(1,4) - SCALAR_RESULT
      TEMP_RESULT(2,3) = TEMP_RESULT(2,3) - SCALAR_RESULT
      TEMP_RESULT(2,4) = TEMP_RESULT(2,4) - SCALAR_RESULT
      TEMP_RESULT(3,3) = TEMP_RESULT(3,3) - SCALAR_RESULT
      TEMP_RESULT(3,4) = TEMP_RESULT(3,4) - SCALAR_RESULT
      TEMP_RESULT(4,3) = TEMP_RESULT(4,3) + SCALAR_RESULT
      TEMP_RESULT(4,4) = TEMP_RESULT(4,4) + SCALAR_RESULT
*
*     Operation 30 (r=29): Exact DeepMind coefficients
*     Right side and bottom elements, simple B pattern, limited C update
      A_CONTRIB = A(1,3) + A(1,4) + A(2,3) + A(2,4) +
     +            A(3,3) + A(3,4) - A(4,3) - A(4,4)
      B_CONTRIB = B(1,4) + B(3,4)
      SCALAR_RESULT = A_CONTRIB * B_CONTRIB
      TEMP_RESULT(4,3) = TEMP_RESULT(4,3) + SCALAR_RESULT
      WRITE(LOG_UNIT,*) 'OP30: A_CONTRIB=',A_CONTRIB,' B_CONTRIB=',
     +                  B_CONTRIB,' SCALAR=',SCALAR_RESULT
*
      WRITE(LOG_UNIT,*) 'ALPHATENSOR_CORRECT: 30 ops complete'
*
*     Operation 31 (r=30): Exact DeepMind coefficients
*     FULL MATRIX A contribution (all 16 elements!), simple B, limited C
      A_CONTRIB = A(1,1) - A(1,2) - A(1,3) - A(1,4) +
     +            A(2,1) - A(2,2) - A(2,3) - A(2,4) +
     +            A(3,1) - A(3,2) - A(3,3) - A(3,4) -
     +            A(4,1) + A(4,2) + A(4,3) + A(4,4)
      B_CONTRIB = B(1,4)
      SCALAR_RESULT = A_CONTRIB * B_CONTRIB
      TEMP_RESULT(2,1) = TEMP_RESULT(2,1) - SCALAR_RESULT
      TEMP_RESULT(4,1) = TEMP_RESULT(4,1) + SCALAR_RESULT
*
*     Operation 32 (r=31): Exact DeepMind coefficients
*     Simple A pattern, complex B pattern (right side and bottom), limited C
      A_CONTRIB = -A(4,3)
      B_CONTRIB = B(1,3) + B(1,4) + B(2,3) + B(2,4) +
     +            B(3,3) + B(3,4) - B(4,3) - B(4,4)
      SCALAR_RESULT = A_CONTRIB * B_CONTRIB
      TEMP_RESULT(1,4) = TEMP_RESULT(1,4) - SCALAR_RESULT
      TEMP_RESULT(3,4) = TEMP_RESULT(3,4) - SCALAR_RESULT
*
*     Operation 33 (r=32): Exact DeepMind coefficients
*     Simple A and B patterns, many C updates (scattered pattern)
      A_CONTRIB = A(1,4)
      B_CONTRIB = -B(2,1) + B(4,1)
      SCALAR_RESULT = A_CONTRIB * B_CONTRIB
      TEMP_RESULT(1,1) = TEMP_RESULT(1,1) + SCALAR_RESULT
      TEMP_RESULT(1,2) = TEMP_RESULT(1,2) - SCALAR_RESULT
      TEMP_RESULT(1,3) = TEMP_RESULT(1,3) - SCALAR_RESULT
      TEMP_RESULT(1,4) = TEMP_RESULT(1,4) - SCALAR_RESULT
      TEMP_RESULT(2,1) = TEMP_RESULT(2,1) + SCALAR_RESULT
      TEMP_RESULT(2,2) = TEMP_RESULT(2,2) - SCALAR_RESULT
      TEMP_RESULT(2,3) = TEMP_RESULT(2,3) - SCALAR_RESULT
      TEMP_RESULT(2,4) = TEMP_RESULT(2,4) - SCALAR_RESULT
      TEMP_RESULT(3,1) = TEMP_RESULT(3,1) + SCALAR_RESULT
      TEMP_RESULT(3,2) = TEMP_RESULT(3,2) - SCALAR_RESULT
      TEMP_RESULT(3,3) = TEMP_RESULT(3,3) - SCALAR_RESULT
      TEMP_RESULT(3,4) = TEMP_RESULT(3,4) - SCALAR_RESULT
      TEMP_RESULT(4,1) = TEMP_RESULT(4,1) - SCALAR_RESULT
      TEMP_RESULT(4,2) = TEMP_RESULT(4,2) + SCALAR_RESULT
      TEMP_RESULT(4,3) = TEMP_RESULT(4,3) + SCALAR_RESULT
      TEMP_RESULT(4,4) = TEMP_RESULT(4,4) + SCALAR_RESULT
*
*     Operation 34 (r=33): Exact DeepMind coefficients
*     Simple A pattern, simple B pattern, many C updates (right/bottom area)
      A_CONTRIB = A(1,4) - A(3,2)
      B_CONTRIB = -B(2,1) + B(4,1) - B(4,3)
      SCALAR_RESULT = A_CONTRIB * B_CONTRIB
      TEMP_RESULT(1,3) = TEMP_RESULT(1,3) + SCALAR_RESULT
      TEMP_RESULT(1,4) = TEMP_RESULT(1,4) + SCALAR_RESULT
      TEMP_RESULT(2,3) = TEMP_RESULT(2,3) + SCALAR_RESULT
      TEMP_RESULT(2,4) = TEMP_RESULT(2,4) + SCALAR_RESULT
      TEMP_RESULT(3,1) = TEMP_RESULT(3,1) - SCALAR_RESULT
      TEMP_RESULT(3,2) = TEMP_RESULT(3,2) + SCALAR_RESULT
      TEMP_RESULT(3,3) = TEMP_RESULT(3,3) + SCALAR_RESULT
      TEMP_RESULT(3,4) = TEMP_RESULT(3,4) + SCALAR_RESULT
      TEMP_RESULT(4,1) = TEMP_RESULT(4,1) + SCALAR_RESULT
      TEMP_RESULT(4,2) = TEMP_RESULT(4,2) - SCALAR_RESULT
      TEMP_RESULT(4,3) = TEMP_RESULT(4,3) - SCALAR_RESULT
      TEMP_RESULT(4,4) = TEMP_RESULT(4,4) - SCALAR_RESULT
*
*     Operation 35 (r=34): Exact DeepMind coefficients
*     Complex A pattern (most elements), simple B pattern, limited C updates
      A_CONTRIB = A(1,3) + A(1,4) + A(2,3) + A(2,4) -
     +            A(3,1) + A(3,2) + A(3,3) + A(3,4) +
     +            A(4,1) - A(4,2) - A(4,3) - A(4,4)
      B_CONTRIB = B(1,4) - B(3,2)
      SCALAR_RESULT = A_CONTRIB * B_CONTRIB
      TEMP_RESULT(2,1) = TEMP_RESULT(2,1) - SCALAR_RESULT
      TEMP_RESULT(4,1) = TEMP_RESULT(4,1) + SCALAR_RESULT
      TEMP_RESULT(4,3) = TEMP_RESULT(4,3) - SCALAR_RESULT
*
      WRITE(LOG_UNIT,*) 'ALPHATENSOR_CORRECT: 35 ops complete'
*
*     Operation 36 (r=35): Exact DeepMind coefficients
*     Bottom rows pattern, simple B, limited C updates
      A_CONTRIB = -A(3,1) + A(3,2) + A(3,3) + A(3,4) +
     +            A(4,1) - A(4,2) - A(4,3) - A(4,4)
      B_CONTRIB = B(3,2)
      SCALAR_RESULT = A_CONTRIB * B_CONTRIB
      TEMP_RESULT(2,1) = TEMP_RESULT(2,1) - SCALAR_RESULT
      TEMP_RESULT(2,3) = TEMP_RESULT(2,3) + SCALAR_RESULT
      TEMP_RESULT(4,1) = TEMP_RESULT(4,1) + SCALAR_RESULT
      TEMP_RESULT(4,3) = TEMP_RESULT(4,3) - SCALAR_RESULT
*
*     Operation 37 (r=36): Exact DeepMind coefficients
*     Simple A and B patterns, column-based C updates
      A_CONTRIB = -A(1,2) - A(3,2)
      B_CONTRIB = -B(2,3)
      SCALAR_RESULT = A_CONTRIB * B_CONTRIB
      TEMP_RESULT(3,1) = TEMP_RESULT(3,1) + SCALAR_RESULT
      TEMP_RESULT(3,2) = TEMP_RESULT(3,2) - SCALAR_RESULT
      TEMP_RESULT(4,1) = TEMP_RESULT(4,1) - SCALAR_RESULT
      TEMP_RESULT(4,2) = TEMP_RESULT(4,2) + SCALAR_RESULT
*
*     Operation 38 (r=37): Exact DeepMind coefficients
*     Simple A and B patterns, limited C updates (right side)
      A_CONTRIB = A(3,2) + A(3,4)
      B_CONTRIB = B(4,1) - B(4,3)
      SCALAR_RESULT = A_CONTRIB * B_CONTRIB
      TEMP_RESULT(1,3) = TEMP_RESULT(1,3) + SCALAR_RESULT
      TEMP_RESULT(1,4) = TEMP_RESULT(1,4) + SCALAR_RESULT
      TEMP_RESULT(2,3) = TEMP_RESULT(2,3) + SCALAR_RESULT
      TEMP_RESULT(2,4) = TEMP_RESULT(2,4) + SCALAR_RESULT
*
*     Operation 39 (r=38): Exact DeepMind coefficients
*     Top-right corner elements, simple B pattern, limited C updates
      A_CONTRIB = -A(1,3) - A(1,4) - A(2,3) - A(2,4)
      B_CONTRIB = B(3,2) + B(3,4)
      SCALAR_RESULT = A_CONTRIB * B_CONTRIB
      TEMP_RESULT(4,1) = TEMP_RESULT(4,1) - SCALAR_RESULT
      TEMP_RESULT(4,3) = TEMP_RESULT(4,3) + SCALAR_RESULT
*
*     Operation 40 (r=39): Exact DeepMind coefficients
*     Simple A pattern, moderate B pattern, many bottom-right C updates
      A_CONTRIB = A(3,2)
      B_CONTRIB = -B(2,1) + B(2,3) + B(4,1) - B(4,3)
      SCALAR_RESULT = A_CONTRIB * B_CONTRIB
      TEMP_RESULT(3,1) = TEMP_RESULT(3,1) - SCALAR_RESULT
      TEMP_RESULT(3,2) = TEMP_RESULT(3,2) + SCALAR_RESULT
      TEMP_RESULT(3,3) = TEMP_RESULT(3,3) + SCALAR_RESULT
      TEMP_RESULT(3,4) = TEMP_RESULT(3,4) + SCALAR_RESULT
      TEMP_RESULT(4,1) = TEMP_RESULT(4,1) + SCALAR_RESULT
      TEMP_RESULT(4,2) = TEMP_RESULT(4,2) - SCALAR_RESULT
      TEMP_RESULT(4,3) = TEMP_RESULT(4,3) - SCALAR_RESULT
      TEMP_RESULT(4,4) = TEMP_RESULT(4,4) - SCALAR_RESULT
      WRITE(LOG_UNIT,*) 'OP40: A_CONTRIB=',A_CONTRIB,' B_CONTRIB=',
     +                  B_CONTRIB,' SCALAR=',SCALAR_RESULT
*
      WRITE(LOG_UNIT,*) 'ALPHATENSOR_CORRECT: 40 ops complete'
*
*     Operation 41 (r=40): Exact DeepMind coefficients
*     Simple A pattern, top-left B pattern, limited C updates
      A_CONTRIB = -A(2,1)
      B_CONTRIB = B(1,1) - B(1,2) + B(2,1) - B(2,2)
      SCALAR_RESULT = A_CONTRIB * B_CONTRIB
      TEMP_RESULT(1,2) = TEMP_RESULT(1,2) - SCALAR_RESULT
      TEMP_RESULT(1,4) = TEMP_RESULT(1,4) - SCALAR_RESULT
*
*     Operation 42 (r=41): Exact DeepMind coefficients
*     Simple A pattern, FULL MATRIX B contribution (all 16 elements!), limited C
      A_CONTRIB = -A(2,1) + A(4,1)
      B_CONTRIB = B(1,1) - B(1,2) - B(1,3) - B(1,4) +
     +            B(2,1) - B(2,2) - B(2,3) - B(2,4) +
     +            B(3,1) - B(3,2) - B(3,3) - B(3,4) -
     +            B(4,1) + B(4,2) + B(4,3) + B(4,4)
      SCALAR_RESULT = A_CONTRIB * B_CONTRIB
      TEMP_RESULT(1,4) = TEMP_RESULT(1,4) + SCALAR_RESULT
*
*     Operation 43 (r=42): Exact DeepMind coefficients
*     Simple A pattern, complex B pattern (right/bottom area), limited C updates
      A_CONTRIB = -A(2,1) + A(4,1) - A(4,3)
      B_CONTRIB = B(1,3) + B(1,4) + B(2,3) + B(2,4) -
     +            B(3,1) + B(3,2) + B(3,3) + B(3,4) +
     +            B(4,1) - B(4,2) - B(4,3) - B(4,4)
      SCALAR_RESULT = A_CONTRIB * B_CONTRIB
      TEMP_RESULT(1,4) = TEMP_RESULT(1,4) + SCALAR_RESULT
      TEMP_RESULT(3,2) = TEMP_RESULT(3,2) - SCALAR_RESULT
*
*     Operation 44 (r=43): Exact DeepMind coefficients
*     Symmetric pattern: column 2 elements
      A_CONTRIB = A(1,2) + A(2,2) + A(3,2) - A(4,2)
      B_CONTRIB = B(1,2) + B(2,2) + B(3,2) - B(4,2)
      SCALAR_RESULT = A_CONTRIB * B_CONTRIB
      TEMP_RESULT(1,2) = TEMP_RESULT(1,2) + SCALAR_RESULT
      TEMP_RESULT(2,2) = TEMP_RESULT(2,2) + SCALAR_RESULT
      TEMP_RESULT(3,2) = TEMP_RESULT(3,2) + SCALAR_RESULT
      TEMP_RESULT(4,2) = TEMP_RESULT(4,2) - SCALAR_RESULT
*
*     Operation 45 (r=44): Exact DeepMind coefficients
*     Moderate A pattern, complex B pattern (bottom half), limited C update
      A_CONTRIB = -A(2,1) + A(2,3) + A(4,1) - A(4,3)
      B_CONTRIB = -B(3,1) + B(3,2) + B(3,3) + B(3,4) +
     +            B(4,1) - B(4,2) - B(4,3) - B(4,4)
      SCALAR_RESULT = A_CONTRIB * B_CONTRIB
      TEMP_RESULT(3,2) = TEMP_RESULT(3,2) + SCALAR_RESULT
      WRITE(LOG_UNIT,*) 'OP45: A_CONTRIB=',A_CONTRIB,' B_CONTRIB=',
     +                  B_CONTRIB,' SCALAR=',SCALAR_RESULT
*
*     Operation 46 (r=45): Exact DeepMind coefficients
*     Bottom-left 2x2 A pattern, simple B pattern, limited C update
      A_CONTRIB = -A(3,1) + A(3,2) + A(4,1) - A(4,2)
      B_CONTRIB = -B(1,2) - B(3,2)
      SCALAR_RESULT = A_CONTRIB * B_CONTRIB
      TEMP_RESULT(2,3) = TEMP_RESULT(2,3) + SCALAR_RESULT
      WRITE(LOG_UNIT,*) 'OP46: A_CONTRIB=',A_CONTRIB,' B_CONTRIB=',
     +                  B_CONTRIB,' SCALAR=',SCALAR_RESULT
*
*     Operation 47 (r=46): Exact DeepMind coefficients
*     Simple A pattern, top-right corner B pattern, limited C updates
      A_CONTRIB = A(4,1) - A(4,3)
      B_CONTRIB = -B(1,3) - B(1,4) - B(2,3) - B(2,4)
      SCALAR_RESULT = A_CONTRIB * B_CONTRIB
      TEMP_RESULT(3,2) = TEMP_RESULT(3,2) - SCALAR_RESULT
      TEMP_RESULT(3,4) = TEMP_RESULT(3,4) - SCALAR_RESULT
      WRITE(LOG_UNIT,*) 'OP47: A_CONTRIB=',A_CONTRIB,' B_CONTRIB=',
     +                  B_CONTRIB,' SCALAR=',SCALAR_RESULT
*
*     Operation 48 (r=47): Exact DeepMind coefficients
*     Simple symmetric pattern: bottom-right corner
      A_CONTRIB = -A(4,3) - A(4,4)
      B_CONTRIB = -B(4,3) - B(4,4)
      SCALAR_RESULT = A_CONTRIB * B_CONTRIB
      TEMP_RESULT(4,3) = TEMP_RESULT(4,3) + SCALAR_RESULT
      TEMP_RESULT(4,4) = TEMP_RESULT(4,4) + SCALAR_RESULT
      WRITE(LOG_UNIT,*) 'OP48: A_CONTRIB=',A_CONTRIB,' B_CONTRIB=',
     +                  B_CONTRIB,' SCALAR=',SCALAR_RESULT
*
*     Operation 49 (r=48): Exact DeepMind coefficients - FINAL OPERATION!
*     Simple A pattern, moderate B pattern, limited C updates
      A_CONTRIB = -A(2,3)
      B_CONTRIB = -B(3,1) + B(3,2) + B(4,1) - B(4,2)
      SCALAR_RESULT = A_CONTRIB * B_CONTRIB
      TEMP_RESULT(1,2) = TEMP_RESULT(1,2) + SCALAR_RESULT
      TEMP_RESULT(3,2) = TEMP_RESULT(3,2) + SCALAR_RESULT
      WRITE(LOG_UNIT,*) 'OP49: A_CONTRIB=',A_CONTRIB,' B_CONTRIB=',
     +                  B_CONTRIB,' SCALAR=',SCALAR_RESULT
*
      WRITE(LOG_UNIT,*) 'DIRECT2D: ALL 49 operations complete!'
      WRITE(LOG_UNIT,*) 'DIRECT2D: Applying transpose fix'
*
*     Apply transpose to convert (A@B)^T -> A@B
      DO I = 1, 4
          DO J = 1, 4
              TRANSPOSED_RESULT(I,J) = TEMP_RESULT(J,I)
          END DO
      END DO
*
*     Apply final scaling: C = ALPHA * A@B + BETA * C_initial
      DO I = 1, 4
          DO J = 1, 4
              C(I,J) = ALPHA * TRANSPOSED_RESULT(I,J) + BETA * C(I,J)
          END DO
      END DO
*
      WRITE(LOG_UNIT,*) 'DIRECT2D: Transpose fix applied successfully'
      WRITE(LOG_UNIT,*) 'DIRECT2D: Algorithm completed'
*
      RETURN
*
*     End of DGEMM_ALPHATENSOR_CORRECT
*
      END
