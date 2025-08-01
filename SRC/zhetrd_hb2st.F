*> \brief \b ZHETRD_HB2ST reduces a complex Hermitian band matrix A to real symmetric tridiagonal form T
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*> Download ZHETRD_HB2ST + dependencies
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.tgz?format=tgz&filename=/lapack/lapack_routine/zhetrd_hb2st.f">
*> [TGZ]</a>
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.zip?format=zip&filename=/lapack/lapack_routine/zhetrd_hb2st.f">
*> [ZIP]</a>
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.txt?format=txt&filename=/lapack/lapack_routine/zhetrd_hb2st.f">
*> [TXT]</a>
*
*  Definition:
*  ===========
*
*       SUBROUTINE ZHETRD_HB2ST( STAGE1, VECT, UPLO, N, KD, AB, LDAB,
*                               D, E, HOUS, LHOUS, WORK, LWORK, INFO )
*
*       #if defined(_OPENMP)
*       use omp_lib
*       #endif
*
*       IMPLICIT NONE
*
*       .. Scalar Arguments ..
*       CHARACTER          STAGE1, UPLO, VECT
*       INTEGER            N, KD, IB, LDAB, LHOUS, LWORK, INFO
*       ..
*       .. Array Arguments ..
*       DOUBLE PRECISION   D( * ), E( * )
*       COMPLEX*16         AB( LDAB, * ), HOUS( * ), WORK( * )
*       ..
*
*
*> \par Purpose:
*  =============
*>
*> \verbatim
*>
*> ZHETRD_HB2ST reduces a complex Hermitian band matrix A to real symmetric
*> tridiagonal form T by a unitary similarity transformation:
*> Q**H * A * Q = T.
*> \endverbatim
*
*  Arguments:
*  ==========
*
*> \param[in] STAGE1
*> \verbatim
*>          STAGE1 is CHARACTER*1
*>          = 'N':  "No": to mention that the stage 1 of the reduction
*>                  from dense to band using the zhetrd_he2hb routine
*>                  was not called before this routine to reproduce AB.
*>                  In other term this routine is called as standalone.
*>          = 'Y':  "Yes": to mention that the stage 1 of the
*>                  reduction from dense to band using the zhetrd_he2hb
*>                  routine has been called to produce AB (e.g., AB is
*>                  the output of zhetrd_he2hb.
*> \endverbatim
*>
*> \param[in] VECT
*> \verbatim
*>          VECT is CHARACTER*1
*>          = 'N':  No need for the Housholder representation,
*>                  and thus LHOUS is of size max(1, 4*N);
*>          = 'V':  the Householder representation is needed to
*>                  either generate or to apply Q later on,
*>                  then LHOUS is to be queried and computed.
*>                  (NOT AVAILABLE IN THIS RELEASE).
*> \endverbatim
*>
*> \param[in] UPLO
*> \verbatim
*>          UPLO is CHARACTER*1
*>          = 'U':  Upper triangle of A is stored;
*>          = 'L':  Lower triangle of A is stored.
*> \endverbatim
*>
*> \param[in] N
*> \verbatim
*>          N is INTEGER
*>          The order of the matrix A.  N >= 0.
*> \endverbatim
*>
*> \param[in] KD
*> \verbatim
*>          KD is INTEGER
*>          The number of superdiagonals of the matrix A if UPLO = 'U',
*>          or the number of subdiagonals if UPLO = 'L'.  KD >= 0.
*> \endverbatim
*>
*> \param[in,out] AB
*> \verbatim
*>          AB is COMPLEX*16 array, dimension (LDAB,N)
*>          On entry, the upper or lower triangle of the Hermitian band
*>          matrix A, stored in the first KD+1 rows of the array.  The
*>          j-th column of A is stored in the j-th column of the array AB
*>          as follows:
*>          if UPLO = 'U', AB(kd+1+i-j,j) = A(i,j) for max(1,j-kd)<=i<=j;
*>          if UPLO = 'L', AB(1+i-j,j)    = A(i,j) for j<=i<=min(n,j+kd).
*>          On exit, the diagonal elements of AB are overwritten by the
*>          diagonal elements of the tridiagonal matrix T; if KD > 0, the
*>          elements on the first superdiagonal (if UPLO = 'U') or the
*>          first subdiagonal (if UPLO = 'L') are overwritten by the
*>          off-diagonal elements of T; the rest of AB is overwritten by
*>          values generated during the reduction.
*> \endverbatim
*>
*> \param[in] LDAB
*> \verbatim
*>          LDAB is INTEGER
*>          The leading dimension of the array AB.  LDAB >= KD+1.
*> \endverbatim
*>
*> \param[out] D
*> \verbatim
*>          D is DOUBLE PRECISION array, dimension (N)
*>          The diagonal elements of the tridiagonal matrix T.
*> \endverbatim
*>
*> \param[out] E
*> \verbatim
*>          E is DOUBLE PRECISION array, dimension (N-1)
*>          The off-diagonal elements of the tridiagonal matrix T:
*>          E(i) = T(i,i+1) if UPLO = 'U'; E(i) = T(i+1,i) if UPLO = 'L'.
*> \endverbatim
*>
*> \param[out] HOUS
*> \verbatim
*>          HOUS is COMPLEX*16 array, dimension (MAX(1,LHOUS))
*>          Stores the Householder representation.
*> \endverbatim
*>
*> \param[in] LHOUS
*> \verbatim
*>          LHOUS is INTEGER
*>          The dimension of the array HOUS.
*>          If N = 0 or KD <= 1, LHOUS >= 1, else LHOUS = MAX(1, dimension).
*>
*>          If LWORK = -1, or LHOUS = -1,
*>          then a query is assumed; the routine
*>          only calculates the optimal size of the HOUS array, returns
*>          this value as the first entry of the HOUS array, and no error
*>          message related to LHOUS is issued by XERBLA.
*>          LHOUS = MAX(1, dimension) where
*>          dimension = 4*N if VECT='N'
*>          not available now if VECT='H'
*> \endverbatim
*>
*> \param[out] WORK
*> \verbatim
*>          WORK is COMPLEX*16 array, dimension (MAX(1,LWORK)).
*>          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
*> \endverbatim
*>
*> \param[in] LWORK
*> \verbatim
*>          LWORK is INTEGER
*>          The dimension of the array WORK.
*>          If N = 0 or KD <= 1, LWORK >= 1, else LWORK = MAX(1, dimension).
*>
*>          If LWORK = -1, or LHOUS = -1,
*>          then a workspace query is assumed; the routine
*>          only calculates the optimal size of the WORK array, returns
*>          this value as the first entry of the WORK array, and no error
*>          message related to LWORK is issued by XERBLA.
*>          LWORK = MAX(1, dimension) where
*>          dimension   = (2KD+1)*N + KD*NTHREADS
*>          where KD is the blocking size of the reduction,
*>          FACTOPTNB is the blocking used by the QR or LQ
*>          algorithm, usually FACTOPTNB=128 is a good choice
*>          NTHREADS is the number of threads used when
*>          openMP compilation is enabled, otherwise =1.
*> \endverbatim
*>
*> \param[out] INFO
*> \verbatim
*>          INFO is INTEGER
*>          = 0:  successful exit
*>          < 0:  if INFO = -i, the i-th argument had an illegal value
*> \endverbatim
*
*  Authors:
*  ========
*
*> \author Univ. of Tennessee
*> \author Univ. of California Berkeley
*> \author Univ. of Colorado Denver
*> \author NAG Ltd.
*
*> \ingroup hetrd_hb2st
*
*> \par Further Details:
*  =====================
*>
*> \verbatim
*>
*>  Implemented by Azzam Haidar.
*>
*>  All details are available on technical report, SC11, SC13 papers.
*>
*>  Azzam Haidar, Hatem Ltaief, and Jack Dongarra.
*>  Parallel reduction to condensed forms for symmetric eigenvalue problems
*>  using aggregated fine-grained and memory-aware kernels. In Proceedings
*>  of 2011 International Conference for High Performance Computing,
*>  Networking, Storage and Analysis (SC '11), New York, NY, USA,
*>  Article 8 , 11 pages.
*>  http://doi.acm.org/10.1145/2063384.2063394
*>
*>  A. Haidar, J. Kurzak, P. Luszczek, 2013.
*>  An improved parallel singular value algorithm and its implementation
*>  for multicore hardware, In Proceedings of 2013 International Conference
*>  for High Performance Computing, Networking, Storage and Analysis (SC '13).
*>  Denver, Colorado, USA, 2013.
*>  Article 90, 12 pages.
*>  http://doi.acm.org/10.1145/2503210.2503292
*>
*>  A. Haidar, R. Solca, S. Tomov, T. Schulthess and J. Dongarra.
*>  A novel hybrid CPU-GPU generalized eigensolver for electronic structure
*>  calculations based on fine-grained memory aware tasks.
*>  International Journal of High Performance Computing Applications.
*>  Volume 28 Issue 2, Pages 196-209, May 2014.
*>  http://hpc.sagepub.com/content/28/2/196
*>
*> \endverbatim
*>
*  =====================================================================
      SUBROUTINE ZHETRD_HB2ST( STAGE1, VECT, UPLO, N, KD, AB, LDAB,
     $                         D, E, HOUS, LHOUS, WORK, LWORK, INFO )
*
*
#if defined(_OPENMP)
      use omp_lib
#endif
*
      IMPLICIT NONE
*
*  -- LAPACK computational routine --
*  -- LAPACK is a software package provided by Univ. of Tennessee,    --
*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
*
*     .. Scalar Arguments ..
      CHARACTER          STAGE1, UPLO, VECT
      INTEGER            N, KD, LDAB, LHOUS, LWORK, INFO
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   D( * ), E( * )
      COMPLEX*16         AB( LDAB, * ), HOUS( * ), WORK( * )
*     ..
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   RZERO
      COMPLEX*16         ZERO, ONE
      PARAMETER          ( RZERO = 0.0D+0,
     $                   ZERO = ( 0.0D+0, 0.0D+0 ),
     $                   ONE  = ( 1.0D+0, 0.0D+0 ) )
*     ..
*     .. Local Scalars ..
      LOGICAL            LQUERY, WANTQ, UPPER, AFTERS1
      INTEGER            I, M, K, IB, SWEEPID, MYID, SHIFT, STT, ST,
     $                   ED, STIND, EDIND, BLKLASTIND, COLPT, THED,
     $                   STEPERCOL, GRSIZ, THGRSIZ, THGRNB, THGRID,
     $                   NBTILES, TTYPE, TID, NTHREADS,
     $                   ABDPOS, ABOFDPOS, DPOS, OFDPOS, AWPOS,
     $                   INDA, INDW, APOS, SIZEA, LDA, INDV, INDTAU,
     $                   SIZEV, SIZETAU, LDV, LHMIN, LWMIN
      DOUBLE PRECISION   ABSTMP
      COMPLEX*16         TMP
*     ..
*     .. External Subroutines ..
      EXTERNAL           ZHB2ST_KERNELS, ZLACPY,
     $                   ZLASET, XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MIN, MAX, CEILING, DBLE, REAL
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      INTEGER            ILAENV2STAGE
      EXTERNAL           LSAME, ILAENV2STAGE
*     ..
*     .. Executable Statements ..
*
*     Determine the minimal workspace size required.
*     Test the input parameters
*
      INFO    = 0
      AFTERS1 = LSAME( STAGE1, 'Y' )
      WANTQ   = LSAME( VECT, 'V' )
      UPPER   = LSAME( UPLO, 'U' )
      LQUERY  = ( LWORK.EQ.-1 ) .OR. ( LHOUS.EQ.-1 )
*
*     Determine the block size, the workspace size and the hous size.
*
      IB       = ILAENV2STAGE( 2, 'ZHETRD_HB2ST', VECT, N, KD,
     $                         -1, -1 )
      IF( N.EQ.0 .OR. KD.LE.1 ) THEN
         LHMIN = 1
         LWMIN = 1
      ELSE
         LHMIN = ILAENV2STAGE( 3, 'ZHETRD_HB2ST', VECT, N, KD, IB,
     $                         -1 )
         LWMIN = ILAENV2STAGE( 4, 'ZHETRD_HB2ST', VECT, N, KD, IB,
     $                         -1 )
      END IF
*
      IF( .NOT.AFTERS1 .AND. .NOT.LSAME( STAGE1, 'N' ) ) THEN
         INFO = -1
      ELSE IF( .NOT.LSAME( VECT, 'N' ) ) THEN
         INFO = -2
      ELSE IF( .NOT.UPPER .AND. .NOT.LSAME( UPLO, 'L' ) ) THEN
         INFO = -3
      ELSE IF( N.LT.0 ) THEN
         INFO = -4
      ELSE IF( KD.LT.0 ) THEN
         INFO = -5
      ELSE IF( LDAB.LT.(KD+1) ) THEN
         INFO = -7
      ELSE IF( LHOUS.LT.LHMIN .AND. .NOT.LQUERY ) THEN
         INFO = -11
      ELSE IF( LWORK.LT.LWMIN .AND. .NOT.LQUERY ) THEN
         INFO = -13
      END IF
*
      IF( INFO.EQ.0 ) THEN
         HOUS( 1 ) = LHMIN
         WORK( 1 ) = LWMIN
      END IF
*
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'ZHETRD_HB2ST', -INFO )
         RETURN
      ELSE IF( LQUERY ) THEN
         RETURN
      END IF
*
*     Quick return if possible
*
      IF( N.EQ.0 ) THEN
          HOUS( 1 ) = 1
          WORK( 1 ) = 1
          RETURN
      END IF
*
*     Determine pointer position
*
      LDV      = KD + IB
      SIZETAU  = 2 * N
      SIZEV    = 2 * N
      INDTAU   = 1
      INDV     = INDTAU + SIZETAU
      LDA      = 2 * KD + 1
      SIZEA    = LDA * N
      INDA     = 1
      INDW     = INDA + SIZEA
      NTHREADS = 1
      TID      = 0
*
      IF( UPPER ) THEN
          APOS     = INDA + KD
          AWPOS    = INDA
          DPOS     = APOS + KD
          OFDPOS   = DPOS - 1
          ABDPOS   = KD + 1
          ABOFDPOS = KD
      ELSE
          APOS     = INDA
          AWPOS    = INDA + KD + 1
          DPOS     = APOS
          OFDPOS   = DPOS + 1
          ABDPOS   = 1
          ABOFDPOS = 2

      ENDIF
*
*     Case KD=0:
*     The matrix is diagonal. We just copy it (convert to "real" for
*     complex because D is double and the imaginary part should be 0)
*     and store it in D. A sequential code here is better or
*     in a parallel environment it might need two cores for D and E
*
      IF( KD.EQ.0 ) THEN
          DO 30 I = 1, N
              D( I ) = DBLE( AB( ABDPOS, I ) )
   30     CONTINUE
          DO 40 I = 1, N-1
              E( I ) = RZERO
   40     CONTINUE
*
          HOUS( 1 ) = 1
          WORK( 1 ) = 1
          RETURN
      END IF
*
*     Case KD=1:
*     The matrix is already Tridiagonal. We have to make diagonal
*     and offdiagonal elements real, and store them in D and E.
*     For that, for real precision just copy the diag and offdiag
*     to D and E while for the COMPLEX case the bulge chasing is
*     performed to convert the hermetian tridiagonal to symmetric
*     tridiagonal. A simpler conversion formula might be used, but then
*     updating the Q matrix will be required and based if Q is generated
*     or not this might complicate the story.
*
      IF( KD.EQ.1 ) THEN
          DO 50 I = 1, N
              D( I ) = DBLE( AB( ABDPOS, I ) )
   50     CONTINUE
*
*         make off-diagonal elements real and copy them to E
*
          IF( UPPER ) THEN
              DO 60 I = 1, N - 1
                  TMP = AB( ABOFDPOS, I+1 )
                  ABSTMP = ABS( TMP )
                  AB( ABOFDPOS, I+1 ) = ABSTMP
                  E( I ) = ABSTMP
                  IF( ABSTMP.NE.RZERO ) THEN
                     TMP = TMP / ABSTMP
                  ELSE
                     TMP = ONE
                  END IF
                  IF( I.LT.N-1 )
     $               AB( ABOFDPOS, I+2 ) = AB( ABOFDPOS, I+2 )*TMP
C                  IF( WANTZ ) THEN
C                     CALL ZSCAL( N, DCONJG( TMP ), Q( 1, I+1 ), 1 )
C                  END IF
   60         CONTINUE
          ELSE
              DO 70 I = 1, N - 1
                 TMP = AB( ABOFDPOS, I )
                 ABSTMP = ABS( TMP )
                 AB( ABOFDPOS, I ) = ABSTMP
                 E( I ) = ABSTMP
                 IF( ABSTMP.NE.RZERO ) THEN
                    TMP = TMP / ABSTMP
                 ELSE
                    TMP = ONE
                 END IF
                 IF( I.LT.N-1 )
     $              AB( ABOFDPOS, I+1 ) = AB( ABOFDPOS, I+1 )*TMP
C                 IF( WANTQ ) THEN
C                    CALL ZSCAL( N, TMP, Q( 1, I+1 ), 1 )
C                 END IF
   70         CONTINUE
          ENDIF
*
          HOUS( 1 ) = 1
          WORK( 1 ) = 1
          RETURN
      END IF
*
*     Main code start here.
*     Reduce the hermitian band of A to a tridiagonal matrix.
*
      THGRSIZ   = N
      GRSIZ     = 1
      SHIFT     = 3
      NBTILES   = CEILING( REAL(N)/REAL(KD) )
      STEPERCOL = CEILING( REAL(SHIFT)/REAL(GRSIZ) )
      THGRNB    = CEILING( REAL(N-1)/REAL(THGRSIZ) )
*
      CALL ZLACPY( "A", KD+1, N, AB, LDAB, WORK( APOS ), LDA )
      CALL ZLASET( "A", KD,   N, ZERO, ZERO, WORK( AWPOS ), LDA )
*
*
*     openMP parallelisation start here
*
#if defined(_OPENMP)
!$OMP PARALLEL PRIVATE( TID, THGRID, BLKLASTIND )
!$OMP$         PRIVATE( THED, I, M, K, ST, ED, STT, SWEEPID )
!$OMP$         PRIVATE( MYID, TTYPE, COLPT, STIND, EDIND )
!$OMP$         SHARED ( UPLO, WANTQ, INDV, INDTAU, HOUS, WORK)
!$OMP$         SHARED ( N, KD, IB, NBTILES, LDA, LDV, INDA )
!$OMP$         SHARED ( STEPERCOL, THGRNB, THGRSIZ, GRSIZ, SHIFT )
!$OMP MASTER
#endif
*
*     main bulge chasing loop
*
      DO 100 THGRID = 1, THGRNB
          STT  = (THGRID-1)*THGRSIZ+1
          THED = MIN( (STT + THGRSIZ -1), (N-1))
          DO 110 I = STT, N-1
              ED = MIN( I, THED )
              IF( STT.GT.ED ) EXIT
              DO 120 M = 1, STEPERCOL
                  ST = STT
                  DO 130 SWEEPID = ST, ED
                      DO 140 K = 1, GRSIZ
                          MYID  = (I-SWEEPID)*(STEPERCOL*GRSIZ)
     $                           + (M-1)*GRSIZ + K
                          IF ( MYID.EQ.1 ) THEN
                              TTYPE = 1
                          ELSE
                              TTYPE = MOD( MYID, 2 ) + 2
                          ENDIF

                          IF( TTYPE.EQ.2 ) THEN
                              COLPT      = (MYID/2)*KD + SWEEPID
                              STIND      = COLPT-KD+1
                              EDIND      = MIN(COLPT,N)
                              BLKLASTIND = COLPT
                          ELSE
                              COLPT      = ((MYID+1)/2)*KD + SWEEPID
                              STIND      = COLPT-KD+1
                              EDIND      = MIN(COLPT,N)
                              IF( ( STIND.GE.EDIND-1 ).AND.
     $                            ( EDIND.EQ.N ) ) THEN
                                  BLKLASTIND = N
                              ELSE
                                  BLKLASTIND = 0
                              ENDIF
                          ENDIF
*
*                         Call the kernel
*
#if defined(_OPENMP) &&  _OPENMP >= 201307

                          IF( TTYPE.NE.1 ) THEN
!$OMP TASK DEPEND(in:WORK(MYID+SHIFT-1))
!$OMP$     DEPEND(in:WORK(MYID-1))
!$OMP$     DEPEND(out:WORK(MYID))
                              TID      = OMP_GET_THREAD_NUM()
                              CALL ZHB2ST_KERNELS(
     $                             UPLO, WANTQ, TTYPE,
     $                             STIND, EDIND, SWEEPID, N, KD, IB,
     $                             WORK ( INDA ), LDA,
     $                             HOUS( INDV ), HOUS( INDTAU ), LDV,
     $                             WORK( INDW + TID*KD ) )
!$OMP END TASK
                          ELSE
!$OMP TASK DEPEND(in:WORK(MYID+SHIFT-1))
!$OMP$     DEPEND(out:WORK(MYID))
                              TID      = OMP_GET_THREAD_NUM()
                              CALL ZHB2ST_KERNELS(
     $                             UPLO, WANTQ, TTYPE,
     $                             STIND, EDIND, SWEEPID, N, KD, IB,
     $                             WORK ( INDA ), LDA,
     $                             HOUS( INDV ), HOUS( INDTAU ), LDV,
     $                             WORK( INDW + TID*KD ) )
!$OMP END TASK
                          ENDIF
#else
                          CALL ZHB2ST_KERNELS(
     $                         UPLO, WANTQ, TTYPE,
     $                         STIND, EDIND, SWEEPID, N, KD, IB,
     $                         WORK ( INDA ), LDA,
     $                         HOUS( INDV ), HOUS( INDTAU ), LDV,
     $                         WORK( INDW ) )
#endif
                          IF ( BLKLASTIND.GE.(N-1) ) THEN
                              STT = STT + 1
                              EXIT
                          ENDIF
  140                 CONTINUE
  130             CONTINUE
  120         CONTINUE
  110     CONTINUE
  100 CONTINUE
*
#if defined(_OPENMP)
!$OMP END MASTER
!$OMP END PARALLEL
#endif
*
*     Copy the diagonal from A to D. Note that D is REAL thus only
*     the Real part is needed, the imaginary part should be zero.
*
      DO 150 I = 1, N
          D( I ) = DBLE( WORK( DPOS+(I-1)*LDA ) )
  150 CONTINUE
*
*     Copy the off diagonal from A to E. Note that E is REAL thus only
*     the Real part is needed, the imaginary part should be zero.
*
      IF( UPPER ) THEN
          DO 160 I = 1, N-1
             E( I ) = DBLE( WORK( OFDPOS+I*LDA ) )
  160     CONTINUE
      ELSE
          DO 170 I = 1, N-1
             E( I ) = DBLE( WORK( OFDPOS+(I-1)*LDA ) )
  170     CONTINUE
      ENDIF
*
      WORK( 1 ) = LWMIN
      RETURN
*
*     End of ZHETRD_HB2ST
*
      END
