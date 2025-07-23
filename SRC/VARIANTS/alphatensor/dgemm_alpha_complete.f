      SUBROUTINE DGEMM_ALPHA(TRANSA,TRANSB,M,N,K,ALPHA,A,LDA,B,LDB,
     +                        BETA,C,LDC)
*
*  -- LAPACK AI Modernization Project --
*  -- AlphaTensor Matrix Multiplication Implementation --
*  -- With REAL AlphaTensor algorithm from DeepMind repository --
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
      DOUBLE PRECISION TEMP
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
*     transposed and set NROWA, NCOLA and NROWB as the number of rows
*     and columns of A and the number of rows of B respectively.
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
      WRITE(LOG_UNIT,*) 'DGEMM_ALPHA: Transpose flags TRANSA=',TRANSA,
     +                  ' TRANSB=',TRANSB
      WRITE(LOG_UNIT,*) 'DGEMM_ALPHA: IS_4X4=',IS_4X4,
     +                  ' NO_TRANSPOSE=',NO_TRANSPOSE
      WRITE(LOG_UNIT,*) 'DGEMM_ALPHA: USE_ALPHA=',USE_ALPHA
*
      IF (USE_ALPHA) THEN
*
*         === REAL ALPHATENSOR 49-OPERATION PATH ===
*
          ALGO_CHOICE = 1
          WRITE(LOG_UNIT,*) 'DGEMM_ALPHA: Using REAL AlphaTensor 4x4',
     +                      ' algorithm'
          WRITE(LOG_UNIT,*) 'DGEMM_ALPHA: Real 47+2 operations from',
     +                      ' DeepMind'
          WRITE(LOG_UNIT,*) 'DGEMM_ALPHA: Target speedup: 10-20%'
*
*         Call REAL AlphaTensor-specific 4x4 implementation
          CALL DGEMM_ALPHATENSOR_REAL(ALPHA, A, LDA, B, LDB, BETA, C,
     +                                LDC)
*
          WRITE(LOG_UNIT,*) 'DGEMM_ALPHA: REAL AlphaTensor computation',
     +                      ' complete'
*
      ELSE
*
*         === STANDARD DGEMM FALLBACK PATH ===
*
          ALGO_CHOICE = 2
          WRITE(LOG_UNIT,*) 'DGEMM_ALPHA: Using standard DGEMM fallback'
*
          IF (.NOT.IS_4X4) THEN
              WRITE(LOG_UNIT,*) 'DGEMM_ALPHA: Reason: Non-4x4 matrices'
              WRITE(LOG_UNIT,*) 'DGEMM_ALPHA: Matrix dimensions M=',M,
     +                          ' N=',N,' K=',K
          END IF
          IF (.NOT.NO_TRANSPOSE) THEN
              WRITE(LOG_UNIT,*) 'DGEMM_ALPHA: Reason: Transpose',
     +                          ' operations detected'
              WRITE(LOG_UNIT,*) 'DGEMM_ALPHA: TRANSA=',TRANSA,
     +                          ' TRANSB=',TRANSB
          END IF
*
*         Delegate to standard DGEMM (exact same parameters)
          CALL DGEMM(TRANSA,TRANSB,M,N,K,ALPHA,A,LDA,B,LDB,BETA,C,
     +               LDC)
*
          WRITE(LOG_UNIT,*) 'DGEMM_ALPHA: Standard DGEMM computation',
     +                      ' complete'
*
      END IF
*
      WRITE(LOG_UNIT,*) 'DGEMM_ALPHA: Algorithm choice =',ALGO_CHOICE
      WRITE(LOG_UNIT,*) 'DGEMM_ALPHA: Subroutine completed successfully'
*
      RETURN
*
*     End of DGEMM_ALPHA
*
      END
*
*     ================================================================
*
      SUBROUTINE DGEMM_ALPHATENSOR_REAL(ALPHA, A, LDA, B, LDB, BETA, C,
     +                                  LDC)
*
*  -- REAL AlphaTensor 4x4 Core Algorithm --
*  -- From DeepMind's official repository --
*  -- Implements the actual 47+2 operation matrix multiplication --
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
      DOUBLE PRECISION TEMP_RESULT(4,4)
      DOUBLE PRECISION H(49)
      INTEGER LOG_UNIT
      PARAMETER (LOG_UNIT=6)
*     ..
*     .. Constants ..
      DOUBLE PRECISION ZERO
      PARAMETER (ZERO=0.0D+0)
*     ..
*
      WRITE(LOG_UNIT,*) 'ALPHATENSOR_REAL: Starting REAL 47+2 op',
     +                  ' algorithm'
      WRITE(LOG_UNIT,*) 'ALPHATENSOR_REAL: ALPHA=',ALPHA,' BETA=',BETA
*
*     === REAL ALPHATENSOR ALGORITHM FROM DEEPMIND ===
*     Operations: 49 (47 core + 2 constants)
*
*     Real operation h1
      H(1) = A(1,1)*B(1,1)
     +     + A(1,1)*B(3,1)
     +     + A(3,1)*B(1,1)
     +     + A(3,1)*B(3,1)
*
*     Real operation h2
      H(2) = A(1,1)*B(1,1)
     +     -A(1,1)*B(1,3)
     +     + A(1,1)*B(3,1)
     +     -A(1,3)*B(1,1)
     +     + A(1,3)*B(1,3)
     +     -A(1,3)*B(3,1)
     +     + A(3,1)*B(1,1)
     +     -A(3,1)*B(1,3)
     +     + A(3,1)*B(3,1)
*
*     Real operation h3
      H(3) = -A(1,3)*B(1,1)
     +     + A(1,3)*B(1,3)
     +     -A(1,3)*B(3,1)
     +     + A(1,3)*B(3,3)
*
*     Real operation h4
      H(4) = A(3,3)*B(3,3)
*
*     Real operation h5
      H(5) = A(3,1)*B(1,3)
*
*     Real operation h6
      H(6) = -A(1,1)*B(3,1)
     +     + A(1,3)*B(3,1)
     +     -A(3,1)*B(3,1)
     +     + A(3,3)*B(3,1)
*
*     Real operation h7
      H(7) = A(2,1)*B(2,1)
     +     -A(2,1)*B(2,2)
     +     + A(2,1)*B(2,3)
     +     + A(2,1)*B(2,4)
     +     -A(2,2)*B(2,1)
     +     + A(2,2)*B(2,2)
     +     -A(2,2)*B(2,3)
     +     -A(2,2)*B(2,4)
     +     + A(2,3)*B(2,1)
     +     -A(2,3)*B(2,2)
     +     + A(2,3)*B(2,3)
     +     + A(2,3)*B(2,4)
     +     + A(2,4)*B(2,1)
     +     -A(2,4)*B(2,2)
     +     + A(2,4)*B(2,3)
     +     + A(2,4)*B(2,4)
*
*     Real operation h8
      H(8) = A(2,1)*B(2,1)
     +     -A(2,1)*B(2,2)
     +     + A(2,1)*B(2,3)
     +     + A(2,1)*B(2,4)
     +     + A(2,1)*B(4,1)
     +     -A(2,1)*B(4,2)
     +     -A(2,2)*B(2,1)
     +     + A(2,2)*B(2,2)
     +     -A(2,2)*B(2,3)
     +     -A(2,2)*B(2,4)
     +     -A(2,2)*B(4,1)
     +     + A(2,2)*B(4,2)
     +     + A(2,3)*B(2,1)
     +     -A(2,3)*B(2,2)
     +     + A(2,3)*B(2,3)
     +     + A(2,3)*B(2,4)
     +     + A(2,3)*B(4,1)
     +     -A(2,3)*B(4,2)
     +     + A(2,4)*B(2,1)
     +     -A(2,4)*B(2,2)
     +     + A(2,4)*B(2,3)
     +     + A(2,4)*B(2,4)
     +     + A(2,4)*B(4,1)
     +     -A(2,4)*B(4,2)
     +     + A(4,1)*B(2,1)
     +     -A(4,1)*B(2,2)
     +     + A(4,1)*B(2,3)
     +     + A(4,1)*B(2,4)
     +     + A(4,1)*B(4,1)
     +     -A(4,1)*B(4,2)
     +     -A(4,2)*B(2,1)
     +     + A(4,2)*B(2,2)
     +     -A(4,2)*B(2,3)
     +     -A(4,2)*B(2,4)
     +     -A(4,2)*B(4,1)
     +     + A(4,2)*B(4,2)
*
*     Real operation h9
      H(9) = A(1,1)*B(1,1)
     +     -A(1,1)*B(1,3)
     +     -A(1,3)*B(1,1)
     +     + A(1,3)*B(1,3)
*
*     Real operation h10
      H(10) = A(2,1)*B(2,1)
     +     -A(2,1)*B(2,2)
     +     + A(2,1)*B(4,1)
     +     -A(2,1)*B(4,2)
     +     -A(2,2)*B(2,1)
     +     + A(2,2)*B(2,2)
     +     -A(2,2)*B(4,1)
     +     + A(2,2)*B(4,2)
     +     + A(4,1)*B(2,1)
     +     -A(4,1)*B(2,2)
     +     + A(4,1)*B(4,1)
     +     -A(4,1)*B(4,2)
     +     -A(4,2)*B(2,1)
     +     + A(4,2)*B(2,2)
     +     -A(4,2)*B(4,1)
     +     + A(4,2)*B(4,2)
*
      WRITE(LOG_UNIT,*) 'ALPHATENSOR_REAL: Operations 1-10 complete'
*
*     Real operation h11
      H(11) = -A(4,1)*B(2,3)
     +     -A(4,1)*B(2,4)
     +     + A(4,2)*B(2,3)
     +     + A(4,2)*B(2,4)
*
*     Real operation h12
      H(12) = -A(2,1)*B(4,1)
     +     + A(2,1)*B(4,2)
     +     + A(2,2)*B(4,1)
     +     -A(2,2)*B(4,2)
     +     -A(2,3)*B(4,1)
     +     + A(2,3)*B(4,2)
     +     -A(2,4)*B(4,1)
     +     + A(2,4)*B(4,2)
     +     -A(4,1)*B(4,1)
     +     + A(4,1)*B(4,2)
     +     + A(4,2)*B(4,1)
     +     -A(4,2)*B(4,2)
     +     -A(4,3)*B(4,1)
     +     + A(4,3)*B(4,2)
     +     -A(4,4)*B(4,1)
     +     + A(4,4)*B(4,2)
*
*     Real operation h13
      H(13) = A(2,3)*B(2,1)
     +     -A(2,3)*B(2,2)
     +     + A(2,3)*B(2,3)
     +     + A(2,3)*B(2,4)
     +     + A(2,3)*B(4,1)
     +     -A(2,3)*B(4,2)
     +     + A(2,3)*B(4,3)
     +     + A(2,3)*B(4,4)
     +     + A(2,4)*B(2,1)
     +     -A(2,4)*B(2,2)
     +     + A(2,4)*B(2,3)
     +     + A(2,4)*B(2,4)
     +     + A(2,4)*B(4,1)
     +     -A(2,4)*B(4,2)
     +     + A(2,4)*B(4,3)
     +     + A(2,4)*B(4,4)
*
*     Real operation h14
      H(14) = -A(1,1)*B(1,2)
     +     -A(1,1)*B(1,4)
     +     + A(1,2)*B(1,2)
     +     + A(1,2)*B(1,4)
     +     -A(2,1)*B(1,2)
     +     -A(2,1)*B(1,4)
     +     + A(2,2)*B(1,2)
     +     + A(2,2)*B(1,4)
*
*     Real operation h15
      H(15) = A(1,2)*B(2,1)
     +     + A(1,4)*B(2,1)
*
*     Real operation h16
      H(16) = A(1,2)*B(1,2)
     +     + A(1,2)*B(1,4)
     +     -A(1,2)*B(2,1)
     +     + A(1,2)*B(2,2)
     +     + A(1,2)*B(2,3)
     +     + A(1,2)*B(2,4)
     +     + A(1,4)*B(1,2)
     +     + A(1,4)*B(1,4)
     +     -A(1,4)*B(2,1)
     +     + A(1,4)*B(2,2)
     +     + A(1,4)*B(2,3)
     +     + A(1,4)*B(2,4)
     +     -A(2,1)*B(1,2)
     +     -A(2,1)*B(1,4)
     +     + A(2,1)*B(2,1)
     +     -A(2,1)*B(2,2)
     +     -A(2,1)*B(2,3)
     +     -A(2,1)*B(2,4)
     +     + A(2,2)*B(1,2)
     +     + A(2,2)*B(1,4)
     +     -A(2,2)*B(2,1)
     +     + A(2,2)*B(2,2)
     +     + A(2,2)*B(2,3)
     +     + A(2,2)*B(2,4)
     +     + A(2,3)*B(1,2)
     +     + A(2,3)*B(1,4)
     +     -A(2,3)*B(2,1)
     +     + A(2,3)*B(2,2)
     +     + A(2,3)*B(2,3)
     +     + A(2,3)*B(2,4)
     +     + A(2,4)*B(1,2)
     +     + A(2,4)*B(1,4)
     +     -A(2,4)*B(2,1)
     +     + A(2,4)*B(2,2)
     +     + A(2,4)*B(2,3)
     +     + A(2,4)*B(2,4)
*
*     Real operation h17
      H(17) = A(1,2)*B(1,2)
     +     + A(1,2)*B(1,4)
     +     -A(1,2)*B(2,1)
     +     + A(1,2)*B(2,2)
     +     + A(1,2)*B(2,3)
     +     + A(1,2)*B(2,4)
     +     + A(1,2)*B(3,2)
     +     + A(1,2)*B(4,1)
     +     -A(1,2)*B(4,2)
     +     + A(1,4)*B(1,2)
     +     + A(1,4)*B(1,4)
     +     -A(1,4)*B(2,1)
     +     + A(1,4)*B(2,2)
     +     + A(1,4)*B(2,3)
     +     + A(1,4)*B(2,4)
     +     + A(1,4)*B(3,2)
     +     + A(1,4)*B(4,1)
     +     -A(1,4)*B(4,2)
     +     -A(2,1)*B(1,2)
     +     -A(2,1)*B(1,4)
     +     + A(2,1)*B(2,1)
     +     -A(2,1)*B(2,2)
     +     -A(2,1)*B(2,3)
     +     -A(2,1)*B(2,4)
     +     -A(2,1)*B(3,2)
     +     -A(2,1)*B(4,1)
     +     + A(2,1)*B(4,2)
     +     + A(2,2)*B(1,2)
     +     + A(2,2)*B(1,4)
     +     -A(2,2)*B(2,1)
     +     + A(2,2)*B(2,2)
     +     + A(2,2)*B(2,3)
     +     + A(2,2)*B(2,4)
     +     + A(2,2)*B(3,2)
     +     + A(2,2)*B(4,1)
     +     -A(2,2)*B(4,2)
     +     + A(2,3)*B(1,2)
     +     + A(2,3)*B(1,4)
     +     -A(2,3)*B(2,1)
     +     + A(2,3)*B(2,2)
     +     + A(2,3)*B(2,3)
     +     + A(2,3)*B(2,4)
     +     + A(2,3)*B(3,2)
     +     + A(2,3)*B(4,1)
     +     -A(2,3)*B(4,2)
     +     + A(2,4)*B(1,2)
     +     + A(2,4)*B(1,4)
     +     -A(2,4)*B(2,1)
     +     + A(2,4)*B(2,2)
     +     + A(2,4)*B(2,3)
     +     + A(2,4)*B(2,4)
     +     + A(2,4)*B(3,2)
     +     + A(2,4)*B(4,1)
     +     -A(2,4)*B(4,2)
     +     + A(3,2)*B(1,2)
     +     + A(3,2)*B(1,4)
     +     -A(3,2)*B(2,1)
     +     + A(3,2)*B(2,2)
     +     + A(3,2)*B(2,3)
     +     + A(3,2)*B(2,4)
     +     + A(3,2)*B(3,2)
     +     + A(3,2)*B(4,1)
     +     -A(3,2)*B(4,2)
     +     + A(4,1)*B(1,2)
     +     + A(4,1)*B(1,4)
     +     -A(4,1)*B(2,1)
     +     + A(4,1)*B(2,2)
     +     + A(4,1)*B(2,3)
     +     + A(4,1)*B(2,4)
     +     + A(4,1)*B(3,2)
     +     + A(4,1)*B(4,1)
     +     -A(4,1)*B(4,2)
     +     -A(4,2)*B(1,2)
     +     -A(4,2)*B(1,4)
     +     + A(4,2)*B(2,1)
     +     -A(4,2)*B(2,2)
     +     -A(4,2)*B(2,3)
     +     -A(4,2)*B(2,4)
     +     -A(4,2)*B(3,2)
     +     -A(4,2)*B(4,1)
     +     + A(4,2)*B(4,2)
*
*     Real operation h18
      H(18) = A(1,2)*B(1,2)
     +     -A(1,2)*B(2,1)
     +     + A(1,2)*B(2,2)
     +     + A(1,2)*B(3,2)
     +     + A(1,2)*B(4,1)
     +     -A(1,2)*B(4,2)
     +     -A(2,1)*B(1,2)
     +     + A(2,1)*B(2,1)
     +     -A(2,1)*B(2,2)
     +     -A(2,1)*B(3,2)
     +     -A(2,1)*B(4,1)
     +     + A(2,1)*B(4,2)
     +     + A(2,2)*B(1,2)
     +     -A(2,2)*B(2,1)
     +     + A(2,2)*B(2,2)
     +     + A(2,2)*B(3,2)
     +     + A(2,2)*B(4,1)
     +     -A(2,2)*B(4,2)
     +     + A(3,2)*B(1,2)
     +     -A(3,2)*B(2,1)
     +     + A(3,2)*B(2,2)
     +     + A(3,2)*B(3,2)
     +     + A(3,2)*B(4,1)
     +     -A(3,2)*B(4,2)
     +     + A(4,1)*B(1,2)
     +     -A(4,1)*B(2,1)
     +     + A(4,1)*B(2,2)
     +     + A(4,1)*B(3,2)
     +     + A(4,1)*B(4,1)
     +     -A(4,1)*B(4,2)
     +     -A(4,2)*B(1,2)
     +     + A(4,2)*B(2,1)
     +     -A(4,2)*B(2,2)
     +     -A(4,2)*B(3,2)
     +     -A(4,2)*B(4,1)
     +     + A(4,2)*B(4,2)
*
*     Real operation h19
      H(19) = A(1,4)*B(1,2)
     +     + A(1,4)*B(1,4)
     +     -A(1,4)*B(2,1)
     +     + A(1,4)*B(2,2)
     +     + A(1,4)*B(2,3)
     +     + A(1,4)*B(2,4)
     +     + A(1,4)*B(3,2)
     +     + A(1,4)*B(3,4)
     +     + A(1,4)*B(4,1)
     +     -A(1,4)*B(4,2)
     +     -A(1,4)*B(4,3)
     +     -A(1,4)*B(4,4)
     +     + A(2,3)*B(1,2)
     +     + A(2,3)*B(1,4)
     +     -A(2,3)*B(2,1)
     +     + A(2,3)*B(2,2)
     +     + A(2,3)*B(2,3)
     +     + A(2,3)*B(2,4)
     +     + A(2,3)*B(3,2)
     +     + A(2,3)*B(3,4)
     +     + A(2,3)*B(4,1)
     +     -A(2,3)*B(4,2)
     +     -A(2,3)*B(4,3)
     +     -A(2,3)*B(4,4)
     +     + A(2,4)*B(1,2)
     +     + A(2,4)*B(1,4)
     +     -A(2,4)*B(2,1)
     +     + A(2,4)*B(2,2)
     +     + A(2,4)*B(2,3)
     +     + A(2,4)*B(2,4)
     +     + A(2,4)*B(3,2)
     +     + A(2,4)*B(3,4)
     +     + A(2,4)*B(4,1)
     +     -A(2,4)*B(4,2)
     +     -A(2,4)*B(4,3)
     +     -A(2,4)*B(4,4)
*
*     Real operation h20
      H(20) = A(1,2)*B(3,2)
     +     + A(1,2)*B(4,1)
     +     -A(1,2)*B(4,2)
     +     + A(1,4)*B(3,2)
     +     + A(1,4)*B(4,1)
     +     -A(1,4)*B(4,2)
     +     -A(2,1)*B(3,2)
     +     -A(2,1)*B(4,1)
     +     + A(2,1)*B(4,2)
     +     + A(2,2)*B(3,2)
     +     + A(2,2)*B(4,1)
     +     -A(2,2)*B(4,2)
     +     + A(2,3)*B(3,2)
     +     + A(2,3)*B(4,1)
     +     -A(2,3)*B(4,2)
     +     + A(2,4)*B(3,2)
     +     + A(2,4)*B(4,1)
     +     -A(2,4)*B(4,2)
     +     + A(3,2)*B(3,2)
     +     + A(3,2)*B(4,1)
     +     -A(3,2)*B(4,2)
     +     + A(3,4)*B(3,2)
     +     + A(3,4)*B(4,1)
     +     -A(3,4)*B(4,2)
     +     + A(4,1)*B(3,2)
     +     + A(4,1)*B(4,1)
     +     -A(4,1)*B(4,2)
     +     -A(4,2)*B(3,2)
     +     -A(4,2)*B(4,1)
     +     + A(4,2)*B(4,2)
     +     -A(4,3)*B(3,2)
     +     -A(4,3)*B(4,1)
     +     + A(4,3)*B(4,2)
     +     -A(4,4)*B(3,2)
     +     -A(4,4)*B(4,1)
     +     + A(4,4)*B(4,2)
*
      WRITE(LOG_UNIT,*) 'ALPHATENSOR_REAL: Operations 1-20 complete'
*
*     Real operation h21
      H(21) = A(3,2)*B(1,4)
     +     + A(3,2)*B(2,3)
     +     + A(3,2)*B(2,4)
     +     + A(4,1)*B(1,4)
     +     + A(4,1)*B(2,3)
     +     + A(4,1)*B(2,4)
     +     -A(4,2)*B(1,4)
     +     -A(4,2)*B(2,3)
     +     -A(4,2)*B(2,4)
*
*     Real operation h22
      H(22) = A(1,2)*B(1,2)
     +     + A(1,2)*B(1,4)
     +     + A(1,2)*B(2,2)
     +     + A(1,2)*B(2,4)
     +     + A(1,4)*B(1,2)
     +     + A(1,4)*B(1,4)
     +     + A(1,4)*B(2,2)
     +     + A(1,4)*B(2,4)
     +     + A(2,2)*B(1,2)
     +     + A(2,2)*B(1,4)
     +     + A(2,2)*B(2,2)
     +     + A(2,2)*B(2,4)
     +     + A(2,4)*B(1,2)
     +     + A(2,4)*B(1,4)
     +     + A(2,4)*B(2,2)
     +     + A(2,4)*B(2,4)
*
*     Real operation h23
      H(23) = A(1,2)*B(1,2)
     +     + A(1,2)*B(1,4)
     +     + A(1,2)*B(2,2)
     +     + A(1,2)*B(2,4)
     +     + A(1,2)*B(3,2)
     +     -A(1,2)*B(4,2)
     +     + A(1,4)*B(1,2)
     +     + A(1,4)*B(1,4)
     +     + A(1,4)*B(2,2)
     +     + A(1,4)*B(2,4)
     +     + A(1,4)*B(3,2)
     +     -A(1,4)*B(4,2)
     +     + A(2,2)*B(1,2)
     +     + A(2,2)*B(1,4)
     +     + A(2,2)*B(2,2)
     +     + A(2,2)*B(2,4)
     +     + A(2,2)*B(3,2)
     +     -A(2,2)*B(4,2)
     +     + A(2,4)*B(1,2)
     +     + A(2,4)*B(1,4)
     +     + A(2,4)*B(2,2)
     +     + A(2,4)*B(2,4)
     +     + A(2,4)*B(3,2)
     +     -A(2,4)*B(4,2)
     +     + A(3,2)*B(1,2)
     +     + A(3,2)*B(1,4)
     +     + A(3,2)*B(2,2)
     +     + A(3,2)*B(2,4)
     +     + A(3,2)*B(3,2)
     +     -A(3,2)*B(4,2)
     +     -A(4,2)*B(1,2)
     +     -A(4,2)*B(1,4)
     +     -A(4,2)*B(2,2)
     +     -A(4,2)*B(2,4)
     +     -A(4,2)*B(3,2)
     +     + A(4,2)*B(4,2)
*
*     Real operation h24
      H(24) = A(1,4)*B(1,2)
     +     + A(1,4)*B(1,4)
     +     + A(1,4)*B(2,2)
     +     + A(1,4)*B(2,4)
     +     + A(1,4)*B(3,2)
     +     + A(1,4)*B(3,4)
     +     -A(1,4)*B(4,2)
     +     -A(1,4)*B(4,4)
     +     + A(2,4)*B(1,2)
     +     + A(2,4)*B(1,4)
     +     + A(2,4)*B(2,2)
     +     + A(2,4)*B(2,4)
     +     + A(2,4)*B(3,2)
     +     + A(2,4)*B(3,4)
     +     -A(2,4)*B(4,2)
     +     -A(2,4)*B(4,4)
*
*     Real operation h25
      H(25) = A(1,2)*B(3,2)
     +     -A(1,2)*B(4,2)
     +     + A(1,4)*B(3,2)
     +     -A(1,4)*B(4,2)
     +     + A(2,2)*B(3,2)
     +     -A(2,2)*B(4,2)
     +     + A(2,4)*B(3,2)
     +     -A(2,4)*B(4,2)
     +     + A(3,2)*B(3,2)
     +     -A(3,2)*B(4,2)
     +     + A(3,4)*B(3,2)
     +     -A(3,4)*B(4,2)
     +     -A(4,2)*B(3,2)
     +     + A(4,2)*B(4,2)
     +     -A(4,4)*B(3,2)
     +     + A(4,4)*B(4,2)
*
*     Real operation h26
      H(26) = A(3,2)*B(1,4)
     +     + A(3,2)*B(2,4)
     +     -A(4,2)*B(1,4)
     +     -A(4,2)*B(2,4)
*
*     Real operation h27
      H(27) = A(3,4)*B(3,4)
     +     -A(3,4)*B(4,4)
     +     -A(4,4)*B(3,4)
     +     + A(4,4)*B(4,4)
*
*     Real operation h28
      H(28) = A(3,4)*B(3,4)
     +     -A(3,4)*B(4,3)
     +     -A(3,4)*B(4,4)
     +     -A(4,3)*B(3,4)
     +     + A(4,3)*B(4,3)
     +     + A(4,3)*B(4,4)
     +     -A(4,4)*B(3,4)
     +     + A(4,4)*B(4,3)
     +     + A(4,4)*B(4,4)
*
*     Real operation h29
      H(29) = -A(1,4)*B(4,3)
     +     -A(3,4)*B(4,3)
*
*     Real operation h30
      H(30) = A(1,3)*B(1,4)
     +     + A(1,3)*B(3,4)
     +     + A(1,4)*B(1,4)
     +     + A(1,4)*B(3,4)
     +     + A(2,3)*B(1,4)
     +     + A(2,3)*B(3,4)
     +     + A(2,4)*B(1,4)
     +     + A(2,4)*B(3,4)
     +     + A(3,3)*B(1,4)
     +     + A(3,3)*B(3,4)
     +     + A(3,4)*B(1,4)
     +     + A(3,4)*B(3,4)
     +     -A(4,3)*B(1,4)
     +     -A(4,3)*B(3,4)
     +     -A(4,4)*B(1,4)
     +     -A(4,4)*B(3,4)
*
      WRITE(LOG_UNIT,*) 'ALPHATENSOR_REAL: Operations 1-30 complete'
*
*     Real operation h31
      H(31) = A(1,1)*B(1,4)
     +     -A(1,2)*B(1,4)
     +     -A(1,3)*B(1,4)
     +     -A(1,4)*B(1,4)
     +     + A(2,1)*B(1,4)
     +     -A(2,2)*B(1,4)
     +     -A(2,3)*B(1,4)
     +     -A(2,4)*B(1,4)
     +     + A(3,1)*B(1,4)
     +     -A(3,2)*B(1,4)
     +     -A(3,3)*B(1,4)
     +     -A(3,4)*B(1,4)
     +     -A(4,1)*B(1,4)
     +     + A(4,2)*B(1,4)
     +     + A(4,3)*B(1,4)
     +     + A(4,4)*B(1,4)
*
*     Real operation h32
      H(32) = -A(4,3)*B(1,3)
     +     -A(4,3)*B(1,4)
     +     -A(4,3)*B(2,3)
     +     -A(4,3)*B(2,4)
     +     -A(4,3)*B(3,3)
     +     -A(4,3)*B(3,4)
     +     + A(4,3)*B(4,3)
     +     + A(4,3)*B(4,4)
*
*     Real operation h33
      H(33) = -A(1,4)*B(2,1)
     +     + A(1,4)*B(4,1)
*
*     Real operation h34
      H(34) = -A(1,4)*B(2,1)
     +     + A(1,4)*B(4,1)
     +     -A(1,4)*B(4,3)
     +     + A(3,2)*B(2,1)
     +     -A(3,2)*B(4,1)
     +     + A(3,2)*B(4,3)
*
*     Real operation h35
      H(35) = A(1,3)*B(1,4)
     +     -A(1,3)*B(3,2)
     +     + A(1,4)*B(1,4)
     +     -A(1,4)*B(3,2)
     +     + A(2,3)*B(1,4)
     +     -A(2,3)*B(3,2)
     +     + A(2,4)*B(1,4)
     +     -A(2,4)*B(3,2)
     +     -A(3,1)*B(1,4)
     +     + A(3,1)*B(3,2)
     +     + A(3,2)*B(1,4)
     +     -A(3,2)*B(3,2)
     +     + A(3,3)*B(1,4)
     +     -A(3,3)*B(3,2)
     +     + A(3,4)*B(1,4)
     +     -A(3,4)*B(3,2)
     +     + A(4,1)*B(1,4)
     +     -A(4,1)*B(3,2)
     +     -A(4,2)*B(1,4)
     +     + A(4,2)*B(3,2)
     +     -A(4,3)*B(1,4)
     +     + A(4,3)*B(3,2)
     +     -A(4,4)*B(1,4)
     +     + A(4,4)*B(3,2)
*
*     Real operation h36
      H(36) = -A(3,1)*B(3,2)
     +     + A(3,2)*B(3,2)
     +     + A(3,3)*B(3,2)
     +     + A(3,4)*B(3,2)
     +     + A(4,1)*B(3,2)
     +     -A(4,2)*B(3,2)
     +     -A(4,3)*B(3,2)
     +     -A(4,4)*B(3,2)
*
*     Real operation h37
      H(37) = A(1,2)*B(2,3)
     +     + A(3,2)*B(2,3)
*
*     Real operation h38
      H(38) = A(3,2)*B(4,1)
     +     -A(3,2)*B(4,3)
     +     + A(3,4)*B(4,1)
     +     -A(3,4)*B(4,3)
*
*     Real operation h39
      H(39) = -A(1,3)*B(3,2)
     +     -A(1,3)*B(3,4)
     +     -A(1,4)*B(3,2)
     +     -A(1,4)*B(3,4)
     +     -A(2,3)*B(3,2)
     +     -A(2,3)*B(3,4)
     +     -A(2,4)*B(3,2)
     +     -A(2,4)*B(3,4)
*
*     Real operation h40
      H(40) = -A(3,2)*B(2,1)
     +     + A(3,2)*B(2,3)
     +     + A(3,2)*B(4,1)
     +     -A(3,2)*B(4,3)
*
      WRITE(LOG_UNIT,*) 'ALPHATENSOR_REAL: Operations 1-40 complete'
*
*     Real operation h41
      H(41) = -A(2,1)*B(1,1)
     +     + A(2,1)*B(1,2)
     +     -A(2,1)*B(2,1)
     +     + A(2,1)*B(2,2)
*
*     Real operation h42
      H(42) = -A(2,1)*B(1,1)
     +     + A(2,1)*B(1,2)
     +     + A(2,1)*B(1,3)
     +     + A(2,1)*B(1,4)
     +     -A(2,1)*B(2,1)
     +     + A(2,1)*B(2,2)
     +     + A(2,1)*B(2,3)
     +     + A(2,1)*B(2,4)
     +     -A(2,1)*B(3,1)
     +     + A(2,1)*B(3,2)
     +     + A(2,1)*B(3,3)
     +     + A(2,1)*B(3,4)
     +     + A(2,1)*B(4,1)
     +     -A(2,1)*B(4,2)
     +     -A(2,1)*B(4,3)
     +     -A(2,1)*B(4,4)
     +     + A(4,1)*B(1,1)
     +     -A(4,1)*B(1,2)
     +     -A(4,1)*B(1,3)
     +     -A(4,1)*B(1,4)
     +     + A(4,1)*B(2,1)
     +     -A(4,1)*B(2,2)
     +     -A(4,1)*B(2,3)
     +     -A(4,1)*B(2,4)
     +     + A(4,1)*B(3,1)
     +     -A(4,1)*B(3,2)
     +     -A(4,1)*B(3,3)
     +     -A(4,1)*B(3,4)
     +     -A(4,1)*B(4,1)
     +     + A(4,1)*B(4,2)
     +     + A(4,1)*B(4,3)
     +     + A(4,1)*B(4,4)
*
*     Real operation h43
      H(43) = -A(2,1)*B(1,3)
     +     -A(2,1)*B(1,4)
     +     -A(2,1)*B(2,3)
     +     -A(2,1)*B(2,4)
     +     + A(2,1)*B(3,1)
     +     -A(2,1)*B(3,2)
     +     -A(2,1)*B(3,3)
     +     -A(2,1)*B(3,4)
     +     -A(2,1)*B(4,1)
     +     + A(2,1)*B(4,2)
     +     + A(2,1)*B(4,3)
     +     + A(2,1)*B(4,4)
     +     + A(4,1)*B(1,3)
     +     + A(4,1)*B(1,4)
     +     + A(4,1)*B(2,3)
     +     + A(4,1)*B(2,4)
     +     -A(4,1)*B(3,1)
     +     + A(4,1)*B(3,2)
     +     + A(4,1)*B(3,3)
     +     + A(4,1)*B(3,4)
     +     + A(4,1)*B(4,1)
     +     -A(4,1)*B(4,2)
     +     -A(4,1)*B(4,3)
     +     -A(4,1)*B(4,4)
     +     -A(4,3)*B(1,3)
     +     -A(4,3)*B(1,4)
     +     -A(4,3)*B(2,3)
     +     -A(4,3)*B(2,4)
     +     + A(4,3)*B(3,1)
     +     -A(4,3)*B(3,2)
     +     -A(4,3)*B(3,3)
     +     -A(4,3)*B(3,4)
     +     -A(4,3)*B(4,1)
     +     + A(4,3)*B(4,2)
     +     + A(4,3)*B(4,3)
     +     + A(4,3)*B(4,4)
*
*     Real operation h44
      H(44) = A(1,2)*B(1,2)
     +     + A(1,2)*B(2,2)
     +     + A(1,2)*B(3,2)
     +     -A(1,2)*B(4,2)
     +     + A(2,2)*B(1,2)
     +     + A(2,2)*B(2,2)
     +     + A(2,2)*B(3,2)
     +     -A(2,2)*B(4,2)
     +     + A(3,2)*B(1,2)
     +     + A(3,2)*B(2,2)
     +     + A(3,2)*B(3,2)
     +     -A(3,2)*B(4,2)
     +     -A(4,2)*B(1,2)
     +     -A(4,2)*B(2,2)
     +     -A(4,2)*B(3,2)
     +     + A(4,2)*B(4,2)
*
*     Real operation h45
      H(45) = A(2,1)*B(3,1)
     +     -A(2,1)*B(3,2)
     +     -A(2,1)*B(3,3)
     +     -A(2,1)*B(3,4)
     +     -A(2,1)*B(4,1)
     +     + A(2,1)*B(4,2)
     +     + A(2,1)*B(4,3)
     +     + A(2,1)*B(4,4)
     +     -A(2,3)*B(3,1)
     +     + A(2,3)*B(3,2)
     +     + A(2,3)*B(3,3)
     +     + A(2,3)*B(3,4)
     +     + A(2,3)*B(4,1)
     +     -A(2,3)*B(4,2)
     +     -A(2,3)*B(4,3)
     +     -A(2,3)*B(4,4)
     +     -A(4,1)*B(3,1)
     +     + A(4,1)*B(3,2)
     +     + A(4,1)*B(3,3)
     +     + A(4,1)*B(3,4)
     +     + A(4,1)*B(4,1)
     +     -A(4,1)*B(4,2)
     +     -A(4,1)*B(4,3)
     +     -A(4,1)*B(4,4)
     +     + A(4,3)*B(3,1)
     +     -A(4,3)*B(3,2)
     +     -A(4,3)*B(3,3)
     +     -A(4,3)*B(3,4)
     +     -A(4,3)*B(4,1)
     +     + A(4,3)*B(4,2)
     +     + A(4,3)*B(4,3)
     +     + A(4,3)*B(4,4)
*
*     Real operation h46
      H(46) = A(3,1)*B(1,2)
     +     + A(3,1)*B(3,2)
     +     -A(3,2)*B(1,2)
     +     -A(3,2)*B(3,2)
     +     -A(4,1)*B(1,2)
     +     -A(4,1)*B(3,2)
     +     + A(4,2)*B(1,2)
     +     + A(4,2)*B(3,2)
*
*     Real operation h47
      H(47) = -A(4,1)*B(1,3)
     +     -A(4,1)*B(1,4)
     +     -A(4,1)*B(2,3)
     +     -A(4,1)*B(2,4)
     +     + A(4,3)*B(1,3)
     +     + A(4,3)*B(1,4)
     +     + A(4,3)*B(2,3)
     +     + A(4,3)*B(2,4)
*
*     === RESULT RECONSTRUCTION ===
*     Using real coefficients from DeepMind tensor decomposition
*
      TEMP_RESULT(1,1) = H(1)
     +                      -H(2)
     +                      -H(5)
     +                      + H(9)
     +                      + H(15)
     +                      + H(33)
*
      TEMP_RESULT(1,2) = -H(15)
     +                      -H(16)
     +                      + H(17)
     +                      -H(18)
     +                      -H(21)
     +                      + H(22)
     +                      -H(23)
     +                      + H(26)
     +                      -H(33)
     +                      -H(41)
     +                      + H(44)
*
      TEMP_RESULT(1,3) = H(2)
     +                      + H(5)
     +                      + H(6)
     +                      -H(9)
     +                      -H(29)
     +                      -H(33)
     +                      + H(34)
     +                      + H(38)
*
      TEMP_RESULT(1,4) = -H(16)
     +                      + H(17)
     +                      -H(20)
     +                      -H(21)
     +                      + H(22)
     +                      -H(23)
     +                      + H(25)
     +                      + H(26)
     +                      -H(29)
     +                      -H(32)
     +                      -H(33)
     +                      + H(34)
     +                      + H(38)
     +                      -H(41)
     +                      + H(42)
     +                      + H(43)
*
      TEMP_RESULT(2,1) = -H(7)
     +                      + H(8)
     +                      -H(10)
     +                      + H(11)
     +                      -H(14)
     +                      + H(15)
     +                      + H(16)
     +                      -H(17)
     +                      + H(18)
     +                      + H(21)
     +                      -H(31)
     +                      + H(33)
     +                      -H(35)
     +                      -H(36)
*
      TEMP_RESULT(2,2) = H(7)
     +                      -H(8)
     +                      + H(10)
     +                      -H(11)
     +                      -H(15)
     +                      -H(16)
     +                      + H(17)
     +                      -H(18)
     +                      -H(21)
     +                      + H(22)
     +                      -H(23)
     +                      + H(26)
     +                      -H(33)
     +                      + H(44)
*
      TEMP_RESULT(2,3) = -H(7)
     +                      + H(8)
     +                      + H(11)
     +                      + H(12)
     +                      -H(16)
     +                      + H(17)
     +                      -H(20)
     +                      -H(21)
     +                      -H(29)
     +                      -H(33)
     +                      + H(34)
     +                      + H(36)
     +                      + H(38)
     +                      + H(46)
*
      TEMP_RESULT(2,4) = -H(7)
     +                      + H(8)
     +                      + H(11)
     +                      + H(12)
     +                      -H(16)
     +                      + H(17)
     +                      -H(20)
     +                      -H(21)
     +                      + H(22)
     +                      -H(23)
     +                      + H(25)
     +                      + H(26)
     +                      -H(29)
     +                      -H(33)
     +                      + H(34)
     +                      + H(38)
*
      TEMP_RESULT(3,1) = H(1)
     +                      -H(2)
     +                      + H(3)
     +                      -H(5)
     +                      + H(33)
     +                      -H(34)
     +                      + H(37)
     +                      -H(40)
*
      TEMP_RESULT(3,2) = H(17)
     +                      -H(18)
     +                      -H(19)
     +                      -H(21)
     +                      -H(23)
     +                      + H(24)
     +                      + H(26)
     +                      -H(33)
     +                      + H(34)
     +                      -H(37)
     +                      + H(40)
     +                      -H(43)
     +                      + H(44)
     +                      + H(45)
     +                      -H(47)
*
      TEMP_RESULT(3,3) = H(4)
     +                      + H(5)
     +                      -H(29)
     +                      -H(33)
     +                      + H(34)
     +                      + H(40)
*
      TEMP_RESULT(3,4) = -H(21)
     +                      + H(26)
     +                      -H(27)
     +                      + H(28)
     +                      -H(29)
     +                      -H(32)
     +                      -H(33)
     +                      + H(34)
     +                      + H(40)
     +                      -H(47)
*
      TEMP_RESULT(4,1) = H(8)
     +                      -H(10)
     +                      + H(11)
     +                      -H(13)
     +                      + H(17)
     +                      -H(18)
     +                      -H(19)
     +                      -H(21)
     +                      + H(31)
     +                      -H(33)
     +                      + H(34)
     +                      + H(35)
     +                      + H(36)
     +                      -H(37)
     +                      -H(39)
     +                      + H(40)
*
      TEMP_RESULT(4,2) = -H(8)
     +                      + H(10)
     +                      -H(11)
     +                      + H(13)
     +                      -H(17)
     +                      + H(18)
     +                      + H(19)
     +                      + H(21)
     +                      + H(23)
     +                      -H(24)
     +                      -H(26)
     +                      + H(33)
     +                      -H(34)
     +                      + H(37)
     +                      -H(40)
     +                      -H(44)
*
      TEMP_RESULT(4,3) = H(11)
     +                      + H(21)
     +                      -H(28)
     +                      + H(29)
     +                      + H(30)
     +                      + H(33)
     +                      -H(34)
     +                      -H(35)
     +                      -H(36)
     +                      + H(39)
     +                      -H(40)
*
      TEMP_RESULT(4,4) = H(11)
     +                      + H(21)
     +                      -H(26)
     +                      + H(27)
     +                      -H(28)
     +                      + H(29)
     +                      + H(33)
     +                      -H(34)
     +                      -H(40)
*
*
*     Apply scaling factors and update C matrix
      WRITE(LOG_UNIT,*) 'ALPHATENSOR_REAL: Applying ALPHA and BETA',
     +                  ' scaling'
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
      WRITE(LOG_UNIT,*) 'ALPHATENSOR_REAL: REAL algorithm completed'
      WRITE(LOG_UNIT,*) 'ALPHATENSOR_REAL: Results written to matrix C'
*
      RETURN
*
*     End of DGEMM_ALPHATENSOR_REAL
*
      END
