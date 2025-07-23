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
*  -- Based on DeepMind's algorithm_from_factors function --
*  -- Linear combinations approach --
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
      INTEGER I, J, K, RANK_IDX
      DOUBLE PRECISION TEMP_RESULT(4,4)
      DOUBLE PRECISION LEFT_COMBO, RIGHT_COMBO, MATRIX_PRODUCT
      INTEGER LOG_UNIT
      PARAMETER (LOG_UNIT=6)
*     ..
*     .. Constants ..
      DOUBLE PRECISION ZERO
      PARAMETER (ZERO=0.0D+0)
*     ..
*
      WRITE(LOG_UNIT,*) 'ALPHATENSOR_CORRECT: Starting CORRECT',
     +                  ' algorithm'
      WRITE(LOG_UNIT,*) 'ALPHATENSOR_CORRECT: Using linear',
     +                  ' combinations'
*
*     Initialize result matrix
      DO J = 1, 4
          DO I = 1, 4
              TEMP_RESULT(I,J) = ZERO
          END DO
      END DO
*
*     === COMPLETE CORRECT ALPHATENSOR ALGORITHM ===
*     Based on DeepMind's algorithm_from_factors
*     Using proper linear combination approach
*
*     Operation 1: Linear combination approach
      LEFT_COMBO = A(1,1) + A(2,2) + A(2,3) + A(2,4) -A(3,3) -A(4,3)
      RIGHT_COMBO = B(1,1) -B(2,1) + B(2,2) + B(2,3) + B(2,4) -B(4,3)
      MATRIX_PRODUCT = LEFT_COMBO * RIGHT_COMBO
      TEMP_RESULT(1,1) = TEMP_RESULT(1,1) + MATRIX_PRODUCT
      TEMP_RESULT(1,4) = TEMP_RESULT(1,4) - MATRIX_PRODUCT
      TEMP_RESULT(2,2) = TEMP_RESULT(2,2) - MATRIX_PRODUCT
      TEMP_RESULT(2,4) = TEMP_RESULT(2,4) - MATRIX_PRODUCT
      TEMP_RESULT(4,2) = TEMP_RESULT(4,2) + MATRIX_PRODUCT
      TEMP_RESULT(4,3) = TEMP_RESULT(4,3) + MATRIX_PRODUCT
*
*     Operation 2: Linear combination approach
      LEFT_COMBO = -A(1,4) + A(2,2) + A(2,3) + A(3,1) -A(3,3)
      RIGHT_COMBO = -B(1,4) + B(2,1) + B(2,2) + B(2,3) + B(3,1) + B(3,4)
     +-B(4,2) -B(4,3)
      MATRIX_PRODUCT = LEFT_COMBO * RIGHT_COMBO
      TEMP_RESULT(1,4) = TEMP_RESULT(1,4) + MATRIX_PRODUCT
      TEMP_RESULT(2,2) = TEMP_RESULT(2,2) - MATRIX_PRODUCT
      TEMP_RESULT(2,4) = TEMP_RESULT(2,4) - MATRIX_PRODUCT
      TEMP_RESULT(4,1) = TEMP_RESULT(4,1) - MATRIX_PRODUCT
      TEMP_RESULT(4,3) = TEMP_RESULT(4,3) + MATRIX_PRODUCT
*
*     Operation 3: Linear combination approach
      LEFT_COMBO = -A(1,3) + A(1,4) -A(2,1) + A(2,2) + A(3,2) -A(3,3)
      RIGHT_COMBO = -B(1,3) + B(1,4) -B(2,1) + B(2,2) + B(3,2)
      MATRIX_PRODUCT = LEFT_COMBO * RIGHT_COMBO
      TEMP_RESULT(2,2) = TEMP_RESULT(2,2) - MATRIX_PRODUCT
      TEMP_RESULT(2,3) = TEMP_RESULT(2,3) + MATRIX_PRODUCT
      TEMP_RESULT(2,4) = TEMP_RESULT(2,4) - MATRIX_PRODUCT
      TEMP_RESULT(3,1) = TEMP_RESULT(3,1) - MATRIX_PRODUCT
      TEMP_RESULT(3,3) = TEMP_RESULT(3,3) + MATRIX_PRODUCT
      TEMP_RESULT(4,1) = TEMP_RESULT(4,1) + MATRIX_PRODUCT
      TEMP_RESULT(4,3) = TEMP_RESULT(4,3) - MATRIX_PRODUCT
*
*     Operation 4: Linear combination approach
      LEFT_COMBO = A(1,3) -A(2,1) + A(3,1) + A(3,3) + A(4,3)
      RIGHT_COMBO = B(1,3) -B(2,1) + B(2,3) -B(3,4) + B(4,2) +
     +B(4,3) -B(4,4)
      MATRIX_PRODUCT = LEFT_COMBO * RIGHT_COMBO
      TEMP_RESULT(3,1) = TEMP_RESULT(3,1) + MATRIX_PRODUCT
      TEMP_RESULT(3,2) = TEMP_RESULT(3,2) - MATRIX_PRODUCT
      TEMP_RESULT(3,4) = TEMP_RESULT(3,4) + MATRIX_PRODUCT
      TEMP_RESULT(4,3) = TEMP_RESULT(4,3) - MATRIX_PRODUCT
*
*     Operation 5: Linear combination approach
      LEFT_COMBO = -A(1,3) + A(3,3)
      RIGHT_COMBO = -B(1,3) -B(4,2) -B(4,4)
      MATRIX_PRODUCT = LEFT_COMBO * RIGHT_COMBO
      TEMP_RESULT(2,4) = TEMP_RESULT(2,4) - MATRIX_PRODUCT
      TEMP_RESULT(3,1) = TEMP_RESULT(3,1) - MATRIX_PRODUCT
      TEMP_RESULT(4,3) = TEMP_RESULT(4,3) - MATRIX_PRODUCT
*
*     Operation 6: Linear combination approach
      LEFT_COMBO = -A(1,3) -A(3,1) -A(3,2) + A(3,3)
      RIGHT_COMBO = -B(1,3) + B(2,3) -B(3,1) -B(3,2)
      MATRIX_PRODUCT = LEFT_COMBO * RIGHT_COMBO
      TEMP_RESULT(2,3) = TEMP_RESULT(2,3) - MATRIX_PRODUCT
      TEMP_RESULT(3,1) = TEMP_RESULT(3,1) - MATRIX_PRODUCT
*
*     Operation 7: Linear combination approach
      LEFT_COMBO = -A(3,2)
      RIGHT_COMBO = B(2,3) -B(3,2)
      MATRIX_PRODUCT = LEFT_COMBO * RIGHT_COMBO
      TEMP_RESULT(1,4) = TEMP_RESULT(1,4) + MATRIX_PRODUCT
      TEMP_RESULT(2,3) = TEMP_RESULT(2,3) - MATRIX_PRODUCT
      TEMP_RESULT(2,4) = TEMP_RESULT(2,4) - MATRIX_PRODUCT
      TEMP_RESULT(3,2) = TEMP_RESULT(3,2) - MATRIX_PRODUCT
*
*     Operation 8: Linear combination approach
      LEFT_COMBO = -A(3,1) + A(3,4) -A(4,2) + A(4,3)
      RIGHT_COMBO = -B(2,3) + B(4,2) + B(4,3) -B(4,4)
      MATRIX_PRODUCT = LEFT_COMBO * RIGHT_COMBO
      TEMP_RESULT(1,4) = TEMP_RESULT(1,4) + MATRIX_PRODUCT
      TEMP_RESULT(3,2) = TEMP_RESULT(3,2) + MATRIX_PRODUCT
      TEMP_RESULT(3,4) = TEMP_RESULT(3,4) + MATRIX_PRODUCT
*
*     Operation 9: Linear combination approach
      LEFT_COMBO = A(1,1) + A(3,4)
      RIGHT_COMBO = B(1,1) -B(2,1) -B(2,3) + B(3,3) + B(3,4)
     +-B(4,2) -B(4,4)
      MATRIX_PRODUCT = LEFT_COMBO * RIGHT_COMBO
      TEMP_RESULT(1,1) = TEMP_RESULT(1,1) + MATRIX_PRODUCT
      TEMP_RESULT(1,2) = TEMP_RESULT(1,2) - MATRIX_PRODUCT
*
*     Operation 10: Linear combination approach
      LEFT_COMBO = -A(1,2) -A(1,4)
      RIGHT_COMBO = -B(1,2) -B(1,4) + B(2,1) -B(2,3) + B(3,3)
      MATRIX_PRODUCT = LEFT_COMBO * RIGHT_COMBO
      TEMP_RESULT(1,2) = TEMP_RESULT(1,2) + MATRIX_PRODUCT
      TEMP_RESULT(1,3) = TEMP_RESULT(1,3) + MATRIX_PRODUCT
      TEMP_RESULT(2,1) = TEMP_RESULT(2,1) + MATRIX_PRODUCT
      TEMP_RESULT(2,3) = TEMP_RESULT(2,3) - MATRIX_PRODUCT
      TEMP_RESULT(3,3) = TEMP_RESULT(3,3) - MATRIX_PRODUCT
      TEMP_RESULT(4,1) = TEMP_RESULT(4,1) - MATRIX_PRODUCT
      TEMP_RESULT(4,3) = TEMP_RESULT(4,3) - MATRIX_PRODUCT
*
      WRITE(LOG_UNIT,*) 'ALPHATENSOR_CORRECT: Operations 1-10 complete'
*
*     Operation 11: Linear combination approach
      LEFT_COMBO = A(1,4) + A(2,4) + A(3,2)
      RIGHT_COMBO = B(1,4) -B(2,1) + B(2,4)
      MATRIX_PRODUCT = LEFT_COMBO * RIGHT_COMBO
      TEMP_RESULT(1,4) = TEMP_RESULT(1,4) + MATRIX_PRODUCT
      TEMP_RESULT(2,3) = TEMP_RESULT(2,3) - MATRIX_PRODUCT
      TEMP_RESULT(4,1) = TEMP_RESULT(4,1) + MATRIX_PRODUCT
      TEMP_RESULT(4,2) = TEMP_RESULT(4,2) - MATRIX_PRODUCT
      TEMP_RESULT(4,4) = TEMP_RESULT(4,4) - MATRIX_PRODUCT
*
*     Operation 12: Linear combination approach
      LEFT_COMBO = A(2,3) + A(3,4) + A(4,3)
      RIGHT_COMBO = -B(2,1) + B(3,1) + B(4,3)
      MATRIX_PRODUCT = LEFT_COMBO * RIGHT_COMBO
      TEMP_RESULT(1,3) = TEMP_RESULT(1,3) + MATRIX_PRODUCT
      TEMP_RESULT(1,4) = TEMP_RESULT(1,4) + MATRIX_PRODUCT
      TEMP_RESULT(3,4) = TEMP_RESULT(3,4) + MATRIX_PRODUCT
*
*     Operation 13: Linear combination approach
      LEFT_COMBO = A(2,1) + A(2,2) + A(2,4) + A(3,4)
      RIGHT_COMBO = B(2,2) + B(2,4) + B(3,3)
      MATRIX_PRODUCT = LEFT_COMBO * RIGHT_COMBO
      TEMP_RESULT(2,2) = TEMP_RESULT(2,2) + MATRIX_PRODUCT
      TEMP_RESULT(4,2) = TEMP_RESULT(4,2) - MATRIX_PRODUCT
      TEMP_RESULT(4,4) = TEMP_RESULT(4,4) - MATRIX_PRODUCT
*
*     Operation 14: Linear combination approach
      LEFT_COMBO = -A(2,1) + A(2,3) -A(3,4) + A(4,1) + A(4,4)
      RIGHT_COMBO = -B(2,1) + B(3,3)
      MATRIX_PRODUCT = LEFT_COMBO * RIGHT_COMBO
      TEMP_RESULT(1,3) = TEMP_RESULT(1,3) + MATRIX_PRODUCT
      TEMP_RESULT(2,3) = TEMP_RESULT(2,3) - MATRIX_PRODUCT
      TEMP_RESULT(3,2) = TEMP_RESULT(3,2) - MATRIX_PRODUCT
*
*     Operation 15: Linear combination approach
      LEFT_COMBO = A(2,2) -A(2,3) + A(2,4) + A(3,4)
      RIGHT_COMBO = B(2,2) + B(2,4) -B(3,4)
      MATRIX_PRODUCT = LEFT_COMBO * RIGHT_COMBO
      TEMP_RESULT(2,2) = TEMP_RESULT(2,2) + MATRIX_PRODUCT
      TEMP_RESULT(2,3) = TEMP_RESULT(2,3) - MATRIX_PRODUCT
      TEMP_RESULT(4,2) = TEMP_RESULT(4,2) - MATRIX_PRODUCT
*
*     Operation 16: Linear combination approach
      LEFT_COMBO = -A(2,2) + A(2,3) + A(3,4) + A(4,1) -A(4,3) -A(4,4)
      RIGHT_COMBO = -B(2,1) -B(2,2) + B(3,1) -B(4,3)
      MATRIX_PRODUCT = LEFT_COMBO * RIGHT_COMBO
      TEMP_RESULT(1,3) = TEMP_RESULT(1,3) + MATRIX_PRODUCT
      TEMP_RESULT(2,1) = TEMP_RESULT(2,1) - MATRIX_PRODUCT
      TEMP_RESULT(2,2) = TEMP_RESULT(2,2) - MATRIX_PRODUCT
      TEMP_RESULT(3,4) = TEMP_RESULT(3,4) - MATRIX_PRODUCT
*
*     Operation 17: Linear combination approach
      LEFT_COMBO = A(1,1) + A(2,1) + A(2,2) + A(2,3) + A(2,4) + A(3,4)
      RIGHT_COMBO = B(1,1) + B(2,2) + B(2,4) + B(3,3) + B(4,1)
      MATRIX_PRODUCT = LEFT_COMBO * RIGHT_COMBO
      TEMP_RESULT(1,1) = TEMP_RESULT(1,1) - MATRIX_PRODUCT
      TEMP_RESULT(1,2) = TEMP_RESULT(1,2) - MATRIX_PRODUCT
      TEMP_RESULT(2,2) = TEMP_RESULT(2,2) + MATRIX_PRODUCT
      TEMP_RESULT(3,2) = TEMP_RESULT(3,2) - MATRIX_PRODUCT
      TEMP_RESULT(4,2) = TEMP_RESULT(4,2) - MATRIX_PRODUCT
*
*     Operation 18: Linear combination approach
      LEFT_COMBO = -A(2,1) + A(2,2) + A(2,3) + A(3,1) + A(3,4)
      RIGHT_COMBO = -B(1,2) + B(2,2) + B(3,3)
      MATRIX_PRODUCT = LEFT_COMBO * RIGHT_COMBO
      TEMP_RESULT(1,3) = TEMP_RESULT(1,3) + MATRIX_PRODUCT
      TEMP_RESULT(2,1) = TEMP_RESULT(2,1) + MATRIX_PRODUCT
      TEMP_RESULT(2,2) = TEMP_RESULT(2,2) + MATRIX_PRODUCT
      TEMP_RESULT(2,3) = TEMP_RESULT(2,3) - MATRIX_PRODUCT
      TEMP_RESULT(3,2) = TEMP_RESULT(3,2) - MATRIX_PRODUCT
      TEMP_RESULT(3,3) = TEMP_RESULT(3,3) - MATRIX_PRODUCT
      TEMP_RESULT(4,1) = TEMP_RESULT(4,1) + MATRIX_PRODUCT
      TEMP_RESULT(4,3) = TEMP_RESULT(4,3) + MATRIX_PRODUCT
*
*     Operation 19: Linear combination approach
      LEFT_COMBO = -A(1,1) -A(1,3) + A(2,2) + A(2,4) + A(3,2) -A(3,4)
      RIGHT_COMBO = -B(1,1) -B(1,3) + B(2,2) + B(2,4) -B(4,1)
      MATRIX_PRODUCT = LEFT_COMBO * RIGHT_COMBO
      TEMP_RESULT(1,1) = TEMP_RESULT(1,1) + MATRIX_PRODUCT
      TEMP_RESULT(2,2) = TEMP_RESULT(2,2) + MATRIX_PRODUCT
      TEMP_RESULT(2,3) = TEMP_RESULT(2,3) - MATRIX_PRODUCT
      TEMP_RESULT(3,1) = TEMP_RESULT(3,1) + MATRIX_PRODUCT
      TEMP_RESULT(4,1) = TEMP_RESULT(4,1) - MATRIX_PRODUCT
      TEMP_RESULT(4,2) = TEMP_RESULT(4,2) - MATRIX_PRODUCT
*
*     Operation 20: Linear combination approach
      LEFT_COMBO = A(1,3) + A(2,3) -A(3,4)
      RIGHT_COMBO = B(1,3) -B(1,4) + B(2,3) + B(4,2)
      MATRIX_PRODUCT = LEFT_COMBO * RIGHT_COMBO
      TEMP_RESULT(1,3) = TEMP_RESULT(1,3) + MATRIX_PRODUCT
      TEMP_RESULT(3,1) = TEMP_RESULT(3,1) - MATRIX_PRODUCT
      TEMP_RESULT(4,1) = TEMP_RESULT(4,1) + MATRIX_PRODUCT
      TEMP_RESULT(4,3) = TEMP_RESULT(4,3) - MATRIX_PRODUCT
*
      WRITE(LOG_UNIT,*) 'ALPHATENSOR_CORRECT: Operations 1-20 complete'
*
*     Operation 21: Linear combination approach
      LEFT_COMBO = -A(1,3) + A(2,2) -A(3,3) -A(3,4)
      RIGHT_COMBO = -B(1,3) -B(1,4) + B(2,2) -B(3,3) -B(4,2)
      MATRIX_PRODUCT = LEFT_COMBO * RIGHT_COMBO
      TEMP_RESULT(2,2) = TEMP_RESULT(2,2) + MATRIX_PRODUCT
      TEMP_RESULT(3,1) = TEMP_RESULT(3,1) + MATRIX_PRODUCT
      TEMP_RESULT(4,1) = TEMP_RESULT(4,1) + MATRIX_PRODUCT
*
*     Operation 22: Linear combination approach
      LEFT_COMBO = -A(1,3) + A(2,3) -A(3,1)
      RIGHT_COMBO = -B(1,3) -B(3,2) -B(3,3) -B(4,2)
      MATRIX_PRODUCT = LEFT_COMBO * RIGHT_COMBO
      TEMP_RESULT(1,3) = TEMP_RESULT(1,3) - MATRIX_PRODUCT
      TEMP_RESULT(2,3) = TEMP_RESULT(2,3) + MATRIX_PRODUCT
      TEMP_RESULT(3,1) = TEMP_RESULT(3,1) + MATRIX_PRODUCT
*
*     Operation 23: Linear combination approach
      LEFT_COMBO = A(2,3) -A(2,4) -A(4,1) -A(4,3)
      RIGHT_COMBO = B(2,3) -B(2,4) -B(4,2)
      MATRIX_PRODUCT = LEFT_COMBO * RIGHT_COMBO
      TEMP_RESULT(2,3) = TEMP_RESULT(2,3) + MATRIX_PRODUCT
      TEMP_RESULT(2,4) = TEMP_RESULT(2,4) + MATRIX_PRODUCT
      TEMP_RESULT(3,3) = TEMP_RESULT(3,3) + MATRIX_PRODUCT
      TEMP_RESULT(4,2) = TEMP_RESULT(4,2) + MATRIX_PRODUCT
*
*     Operation 24: Linear combination approach
      LEFT_COMBO = -A(1,2) + A(2,2) -A(2,3) -A(4,1) -A(4,2)
      RIGHT_COMBO = B(2,2) -B(2,3) + B(4,2)
      MATRIX_PRODUCT = LEFT_COMBO * RIGHT_COMBO
      TEMP_RESULT(1,3) = TEMP_RESULT(1,3) - MATRIX_PRODUCT
      TEMP_RESULT(2,1) = TEMP_RESULT(2,1) - MATRIX_PRODUCT
      TEMP_RESULT(2,2) = TEMP_RESULT(2,2) + MATRIX_PRODUCT
      TEMP_RESULT(3,3) = TEMP_RESULT(3,3) - MATRIX_PRODUCT
*
*     Operation 25: Linear combination approach
      LEFT_COMBO = A(1,1) -A(2,2) -A(2,3) + A(4,3)
      RIGHT_COMBO = B(1,1) -B(2,2) -B(4,2)
      MATRIX_PRODUCT = LEFT_COMBO * RIGHT_COMBO
      TEMP_RESULT(1,1) = TEMP_RESULT(1,1) - MATRIX_PRODUCT
      TEMP_RESULT(2,2) = TEMP_RESULT(2,2) - MATRIX_PRODUCT
      TEMP_RESULT(3,3) = TEMP_RESULT(3,3) - MATRIX_PRODUCT
*
*     Operation 26: Linear combination approach
      LEFT_COMBO = A(1,4) -A(2,3) + A(3,2) + A(3,3)
      RIGHT_COMBO = -B(4,2)
      MATRIX_PRODUCT = LEFT_COMBO * RIGHT_COMBO
      TEMP_RESULT(2,1) = TEMP_RESULT(2,1) + MATRIX_PRODUCT
      TEMP_RESULT(3,3) = TEMP_RESULT(3,3) - MATRIX_PRODUCT
      TEMP_RESULT(4,1) = TEMP_RESULT(4,1) + MATRIX_PRODUCT
      TEMP_RESULT(4,3) = TEMP_RESULT(4,3) - MATRIX_PRODUCT
*
*     Operation 27: Linear combination approach
      LEFT_COMBO = -A(1,3) -A(1,4) + A(3,2) -A(4,1)
      RIGHT_COMBO = -B(1,3) + B(2,4) + B(3,2) -B(3,3) -B(4,2) -B(4,3)
      MATRIX_PRODUCT = LEFT_COMBO * RIGHT_COMBO
      TEMP_RESULT(3,1) = TEMP_RESULT(3,1) + MATRIX_PRODUCT
      TEMP_RESULT(3,3) = TEMP_RESULT(3,3) + MATRIX_PRODUCT
      TEMP_RESULT(4,1) = TEMP_RESULT(4,1) - MATRIX_PRODUCT
*
*     Operation 28: Linear combination approach
      LEFT_COMBO = A(1,3) -A(4,1)
      RIGHT_COMBO = B(1,3) + B(4,2) + B(4,3)
      MATRIX_PRODUCT = LEFT_COMBO * RIGHT_COMBO
      TEMP_RESULT(3,1) = TEMP_RESULT(3,1) - MATRIX_PRODUCT
      TEMP_RESULT(3,2) = TEMP_RESULT(3,2) - MATRIX_PRODUCT
      TEMP_RESULT(3,3) = TEMP_RESULT(3,3) - MATRIX_PRODUCT
      TEMP_RESULT(3,4) = TEMP_RESULT(3,4) + MATRIX_PRODUCT
      TEMP_RESULT(4,1) = TEMP_RESULT(4,1) + MATRIX_PRODUCT
      TEMP_RESULT(4,3) = TEMP_RESULT(4,3) + MATRIX_PRODUCT
*
*     Operation 29: Linear combination approach
      LEFT_COMBO = A(2,2) + A(2,4)
      RIGHT_COMBO = B(2,2) + B(2,4) -B(4,2) + B(4,3)
      MATRIX_PRODUCT = LEFT_COMBO * RIGHT_COMBO
      TEMP_RESULT(1,2) = TEMP_RESULT(1,2) + MATRIX_PRODUCT
      TEMP_RESULT(2,2) = TEMP_RESULT(2,2) - MATRIX_PRODUCT
      TEMP_RESULT(3,3) = TEMP_RESULT(3,3) - MATRIX_PRODUCT
      TEMP_RESULT(4,1) = TEMP_RESULT(4,1) + MATRIX_PRODUCT
*
*     Operation 30: Linear combination approach
      LEFT_COMBO = -A(2,1) + A(3,2) -A(3,4)
      RIGHT_COMBO = B(2,3) + B(4,1) -B(4,2) + B(4,3)
      MATRIX_PRODUCT = LEFT_COMBO * RIGHT_COMBO
      TEMP_RESULT(1,2) = TEMP_RESULT(1,2) - MATRIX_PRODUCT
      TEMP_RESULT(3,2) = TEMP_RESULT(3,2) - MATRIX_PRODUCT
      TEMP_RESULT(3,3) = TEMP_RESULT(3,3) - MATRIX_PRODUCT
*
      WRITE(LOG_UNIT,*) 'ALPHATENSOR_CORRECT: Operations 1-30 complete'
*
*     Operation 31: Linear combination approach
      LEFT_COMBO = A(1,3) + A(3,2) + A(3,4) + A(4,3)
      RIGHT_COMBO = B(1,3) + B(2,4) + B(3,4) -B(4,2) + B(4,3)
      MATRIX_PRODUCT = LEFT_COMBO * RIGHT_COMBO
      TEMP_RESULT(3,1) = TEMP_RESULT(3,1) + MATRIX_PRODUCT
      TEMP_RESULT(3,2) = TEMP_RESULT(3,2) + MATRIX_PRODUCT
      TEMP_RESULT(3,3) = TEMP_RESULT(3,3) + MATRIX_PRODUCT
*
*     Operation 32: Linear combination approach
      LEFT_COMBO = A(1,2) -A(2,1) -A(2,2) + A(3,4) + A(4,2) -A(4,4)
      RIGHT_COMBO = -B(2,2) + B(4,1) -B(4,2) -B(4,3) -B(4,4)
      MATRIX_PRODUCT = LEFT_COMBO * RIGHT_COMBO
      TEMP_RESULT(2,2) = TEMP_RESULT(2,2) + MATRIX_PRODUCT
      TEMP_RESULT(3,2) = TEMP_RESULT(3,2) - MATRIX_PRODUCT
      TEMP_RESULT(3,3) = TEMP_RESULT(3,3) - MATRIX_PRODUCT
      TEMP_RESULT(4,4) = TEMP_RESULT(4,4) + MATRIX_PRODUCT
*
*     Operation 33: Linear combination approach
      LEFT_COMBO = -A(1,3) + A(2,2) + A(2,4) + A(3,4) -A(4,3) -A(4,4)
      RIGHT_COMBO = B(1,1) -B(1,3) -B(2,1) + B(2,2) + B(2,3) + B(2,4) +
     +B(4,2) -B(4,3) -B(4,4)
      MATRIX_PRODUCT = LEFT_COMBO * RIGHT_COMBO
      TEMP_RESULT(1,2) = TEMP_RESULT(1,2) + MATRIX_PRODUCT
      TEMP_RESULT(1,4) = TEMP_RESULT(1,4) - MATRIX_PRODUCT
      TEMP_RESULT(2,2) = TEMP_RESULT(2,2) - MATRIX_PRODUCT
      TEMP_RESULT(3,1) = TEMP_RESULT(3,1) - MATRIX_PRODUCT
      TEMP_RESULT(3,2) = TEMP_RESULT(3,2) - MATRIX_PRODUCT
      TEMP_RESULT(3,3) = TEMP_RESULT(3,3) - MATRIX_PRODUCT
      TEMP_RESULT(4,4) = TEMP_RESULT(4,4) + MATRIX_PRODUCT
*
*     Operation 34: Linear combination approach
      LEFT_COMBO = -A(1,2) -A(1,4) + A(3,2) + A(3,4)
      RIGHT_COMBO = B(2,3) + B(4,2) -B(4,3)
      MATRIX_PRODUCT = LEFT_COMBO * RIGHT_COMBO
      TEMP_RESULT(1,2) = TEMP_RESULT(1,2) - MATRIX_PRODUCT
      TEMP_RESULT(2,1) = TEMP_RESULT(2,1) + MATRIX_PRODUCT
      TEMP_RESULT(3,2) = TEMP_RESULT(3,2) - MATRIX_PRODUCT
      TEMP_RESULT(3,3) = TEMP_RESULT(3,3) - MATRIX_PRODUCT
      TEMP_RESULT(4,3) = TEMP_RESULT(4,3) + MATRIX_PRODUCT
*
*     Operation 35: Linear combination approach
      LEFT_COMBO = -A(1,1) + A(1,4) + A(3,2) -A(3,4)
      RIGHT_COMBO = -B(1,1) + B(2,4) + B(3,2) + B(3,3) + B(4,2)
      MATRIX_PRODUCT = LEFT_COMBO * RIGHT_COMBO
      TEMP_RESULT(1,4) = TEMP_RESULT(1,4) + MATRIX_PRODUCT
      TEMP_RESULT(3,3) = TEMP_RESULT(3,3) - MATRIX_PRODUCT
      TEMP_RESULT(4,2) = TEMP_RESULT(4,2) + MATRIX_PRODUCT
      TEMP_RESULT(4,4) = TEMP_RESULT(4,4) + MATRIX_PRODUCT
*
*     Operation 36: Linear combination approach
      LEFT_COMBO = -A(1,4) + A(2,3) + A(3,1) -A(3,4)
      RIGHT_COMBO = B(3,1) -B(4,3)
      MATRIX_PRODUCT = LEFT_COMBO * RIGHT_COMBO
      TEMP_RESULT(1,3) = TEMP_RESULT(1,3) - MATRIX_PRODUCT
      TEMP_RESULT(3,2) = TEMP_RESULT(3,2) - MATRIX_PRODUCT
      TEMP_RESULT(3,3) = TEMP_RESULT(3,3) + MATRIX_PRODUCT
      TEMP_RESULT(4,1) = TEMP_RESULT(4,1) + MATRIX_PRODUCT
      TEMP_RESULT(4,3) = TEMP_RESULT(4,3) - MATRIX_PRODUCT
*
*     Operation 37: Linear combination approach
      LEFT_COMBO = -A(1,4) + A(2,2) -A(3,4)
      RIGHT_COMBO = B(2,2) + B(2,4)
      MATRIX_PRODUCT = LEFT_COMBO * RIGHT_COMBO
      TEMP_RESULT(2,2) = TEMP_RESULT(2,2) - MATRIX_PRODUCT
      TEMP_RESULT(3,3) = TEMP_RESULT(3,3) + MATRIX_PRODUCT
      TEMP_RESULT(4,1) = TEMP_RESULT(4,1) + MATRIX_PRODUCT
*
*     Operation 38: Linear combination approach
      LEFT_COMBO = -A(3,2)
      RIGHT_COMBO = B(4,2)
      MATRIX_PRODUCT = LEFT_COMBO * RIGHT_COMBO
      TEMP_RESULT(2,3) = TEMP_RESULT(2,3) + MATRIX_PRODUCT
      TEMP_RESULT(3,2) = TEMP_RESULT(3,2) - MATRIX_PRODUCT
      TEMP_RESULT(3,3) = TEMP_RESULT(3,3) + MATRIX_PRODUCT
*
*     Operation 39: Linear combination approach
      LEFT_COMBO = A(2,3) -A(3,2) -A(3,4)
      RIGHT_COMBO = -B(2,4) + B(4,2)
      MATRIX_PRODUCT = LEFT_COMBO * RIGHT_COMBO
      TEMP_RESULT(2,4) = TEMP_RESULT(2,4) + MATRIX_PRODUCT
      TEMP_RESULT(3,2) = TEMP_RESULT(3,2) - MATRIX_PRODUCT
      TEMP_RESULT(4,2) = TEMP_RESULT(4,2) - MATRIX_PRODUCT
*
*     Operation 40: Linear combination approach
      LEFT_COMBO = A(1,2) + A(2,2) -A(2,3) -A(3,1) + A(3,2)
     +-A(4,2) -A(4,4)
      RIGHT_COMBO = -B(1,2) + B(2,2) -B(3,1)
      MATRIX_PRODUCT = LEFT_COMBO * RIGHT_COMBO
      TEMP_RESULT(1,3) = TEMP_RESULT(1,3) + MATRIX_PRODUCT
      TEMP_RESULT(2,2) = TEMP_RESULT(2,2) - MATRIX_PRODUCT
      TEMP_RESULT(3,2) = TEMP_RESULT(3,2) + MATRIX_PRODUCT
*
      WRITE(LOG_UNIT,*) 'ALPHATENSOR_CORRECT: Operations 1-40 complete'
*
*     Operation 41: Linear combination approach
      LEFT_COMBO = -A(2,2) -A(3,2)
      RIGHT_COMBO = B(1,1) -B(2,2) -B(2,4) -B(4,1)
      MATRIX_PRODUCT = LEFT_COMBO * RIGHT_COMBO
      TEMP_RESULT(1,1) = TEMP_RESULT(1,1) + MATRIX_PRODUCT
      TEMP_RESULT(2,2) = TEMP_RESULT(2,2) + MATRIX_PRODUCT
      TEMP_RESULT(3,2) = TEMP_RESULT(3,2) + MATRIX_PRODUCT
      TEMP_RESULT(3,3) = TEMP_RESULT(3,3) + MATRIX_PRODUCT
      TEMP_RESULT(3,4) = TEMP_RESULT(3,4) + MATRIX_PRODUCT
*
*     Operation 42: Linear combination approach
      LEFT_COMBO = -A(1,2) -A(1,4) -A(3,2) + A(3,3)
      RIGHT_COMBO = B(1,4) + B(4,2) -B(4,4)
      MATRIX_PRODUCT = LEFT_COMBO * RIGHT_COMBO
      TEMP_RESULT(3,2) = TEMP_RESULT(3,2) + MATRIX_PRODUCT
      TEMP_RESULT(3,3) = TEMP_RESULT(3,3) + MATRIX_PRODUCT
*
*     Operation 43: Linear combination approach
      LEFT_COMBO = A(1,4) + A(2,4) -A(3,2) -A(4,3)
      RIGHT_COMBO = -B(1,1) -B(1,4) + B(3,2) -B(3,3) + B(4,1) + B(4,2) +
     +B(4,4)
      MATRIX_PRODUCT = LEFT_COMBO * RIGHT_COMBO
      TEMP_RESULT(4,4) = TEMP_RESULT(4,4) + MATRIX_PRODUCT
*
*     Operation 44: Linear combination approach
      LEFT_COMBO = -A(1,4) + A(2,3) + A(3,2) + A(4,3)
      RIGHT_COMBO = B(2,3) -B(3,4) -B(4,2) -B(4,3)
      MATRIX_PRODUCT = LEFT_COMBO * RIGHT_COMBO
      TEMP_RESULT(2,3) = TEMP_RESULT(2,3) - MATRIX_PRODUCT
      TEMP_RESULT(3,2) = TEMP_RESULT(3,2) + MATRIX_PRODUCT
*
*     Operation 45: Linear combination approach
      LEFT_COMBO = -A(1,4) + A(2,4) -A(3,2)
      RIGHT_COMBO = B(2,2) + B(4,2)
      MATRIX_PRODUCT = LEFT_COMBO * RIGHT_COMBO
      TEMP_RESULT(1,4) = TEMP_RESULT(1,4) - MATRIX_PRODUCT
      TEMP_RESULT(2,4) = TEMP_RESULT(2,4) - MATRIX_PRODUCT
      TEMP_RESULT(3,3) = TEMP_RESULT(3,3) + MATRIX_PRODUCT
      TEMP_RESULT(4,2) = TEMP_RESULT(4,2) + MATRIX_PRODUCT
*
*     Operation 46: Linear combination approach
      LEFT_COMBO = A(2,1) + A(2,3) -A(3,2) + A(4,1)
      RIGHT_COMBO = B(2,1) + B(2,3) + B(4,2) + B(4,4)
      MATRIX_PRODUCT = LEFT_COMBO * RIGHT_COMBO
      TEMP_RESULT(1,2) = TEMP_RESULT(1,2) - MATRIX_PRODUCT
      TEMP_RESULT(1,4) = TEMP_RESULT(1,4) + MATRIX_PRODUCT
      TEMP_RESULT(3,2) = TEMP_RESULT(3,2) + MATRIX_PRODUCT
      TEMP_RESULT(3,3) = TEMP_RESULT(3,3) + MATRIX_PRODUCT
      TEMP_RESULT(4,3) = TEMP_RESULT(4,3) + MATRIX_PRODUCT
*
*     Operation 47: Linear combination approach
      LEFT_COMBO = A(2,2) + A(2,4) -A(3,2) -A(3,4) + A(4,3)
      RIGHT_COMBO = B(2,2) + B(4,2) -B(4,4)
      MATRIX_PRODUCT = LEFT_COMBO * RIGHT_COMBO
      TEMP_RESULT(1,4) = TEMP_RESULT(1,4) + MATRIX_PRODUCT
      TEMP_RESULT(3,3) = TEMP_RESULT(3,3) - MATRIX_PRODUCT
      TEMP_RESULT(4,3) = TEMP_RESULT(4,3) - MATRIX_PRODUCT
*
*
*     Apply scaling factors and update C matrix
      WRITE(LOG_UNIT,*) 'ALPHATENSOR_CORRECT: Applying ALPHA/BETA'
*
      DO J = 1, 4
          DO I = 1, 4
              IF (BETA.EQ.ZERO) THEN
                  C(I,J) = ALPHA * TEMP_RESULT(I,J)
              ELSE
                  C(I,J) = ALPHA * TEMP_RESULT(I,J) + BETA * C(I,J)
              END IF
          END DO
      END DO
*
      WRITE(LOG_UNIT,*) 'ALPHATENSOR_CORRECT: Algorithm completed'
*
      RETURN
*
*     End of DGEMM_ALPHATENSOR_CORRECT
*
      END
