*> \brief <b> CHESV_AA computes the solution to system of linear equations A * X = B for HE matrices</b>
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*> Download CHESV_AA + dependencies
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.tgz?format=tgz&filename=/lapack/lapack_routine/chesv_aa.f">
*> [TGZ]</a>
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.zip?format=zip&filename=/lapack/lapack_routine/chesv_aa.f">
*> [ZIP]</a>
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.txt?format=txt&filename=/lapack/lapack_routine/chesv_aa.f">
*> [TXT]</a>
*
*  Definition:
*  ===========
*
*       SUBROUTINE CHESV_AA( UPLO, N, NRHS, A, LDA, IPIV, B, LDB, WORK,
*                            LWORK, INFO )
*
*       .. Scalar Arguments ..
*       CHARACTER          UPLO
*       INTEGER            INFO, LDA, LDB, LWORK, N, NRHS
*       ..
*       .. Array Arguments ..
*       INTEGER            IPIV( * )
*       COMPLEX            A( LDA, * ), B( LDB, * ), WORK( * )
*       ..
*
*
*> \par Purpose:
*  =============
*>
*> \verbatim
*>
*> CHESV_AA computes the solution to a complex system of linear equations
*>    A * X = B,
*> where A is an N-by-N Hermitian matrix and X and B are N-by-NRHS
*> matrices.
*>
*> Aasen's algorithm is used to factor A as
*>    A = U**H * T * U,  if UPLO = 'U', or
*>    A = L * T * L**H,  if UPLO = 'L',
*> where U (or L) is a product of permutation and unit upper (lower)
*> triangular matrices, and T is Hermitian and tridiagonal. The factored form
*> of A is then used to solve the system of equations A * X = B.
*> \endverbatim
*
*  Arguments:
*  ==========
*
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
*>          The number of linear equations, i.e., the order of the
*>          matrix A.  N >= 0.
*> \endverbatim
*>
*> \param[in] NRHS
*> \verbatim
*>          NRHS is INTEGER
*>          The number of right hand sides, i.e., the number of columns
*>          of the matrix B.  NRHS >= 0.
*> \endverbatim
*>
*> \param[in,out] A
*> \verbatim
*>          A is COMPLEX array, dimension (LDA,N)
*>          On entry, the Hermitian matrix A.  If UPLO = 'U', the leading
*>          N-by-N upper triangular part of A contains the upper
*>          triangular part of the matrix A, and the strictly lower
*>          triangular part of A is not referenced.  If UPLO = 'L', the
*>          leading N-by-N lower triangular part of A contains the lower
*>          triangular part of the matrix A, and the strictly upper
*>          triangular part of A is not referenced.
*>
*>          On exit, if INFO = 0, the tridiagonal matrix T and the
*>          multipliers used to obtain the factor U or L from the
*>          factorization A = U**H*T*U or A = L*T*L**H as computed by
*>          CHETRF_AA.
*> \endverbatim
*>
*> \param[in] LDA
*> \verbatim
*>          LDA is INTEGER
*>          The leading dimension of the array A.  LDA >= max(1,N).
*> \endverbatim
*>
*> \param[out] IPIV
*> \verbatim
*>          IPIV is INTEGER array, dimension (N)
*>          On exit, it contains the details of the interchanges, i.e.,
*>          the row and column k of A were interchanged with the
*>          row and column IPIV(k).
*> \endverbatim
*>
*> \param[in,out] B
*> \verbatim
*>          B is COMPLEX array, dimension (LDB,NRHS)
*>          On entry, the N-by-NRHS right hand side matrix B.
*>          On exit, if INFO = 0, the N-by-NRHS solution matrix X.
*> \endverbatim
*>
*> \param[in] LDB
*> \verbatim
*>          LDB is INTEGER
*>          The leading dimension of the array B.  LDB >= max(1,N).
*> \endverbatim
*>
*> \param[out] WORK
*> \verbatim
*>          WORK is COMPLEX array, dimension (MAX(1,LWORK))
*>          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
*> \endverbatim
*>
*> \param[in] LWORK
*> \verbatim
*>          LWORK is INTEGER
*>          The length of WORK.  LWORK >= MAX(1,2*N,3*N-2), and for best
*>          performance LWORK >= MAX(1,N*NB), where NB is the optimal
*>          blocksize for CHETRF.
*>
*>          If LWORK = -1, then a workspace query is assumed; the routine
*>          only calculates the optimal size of the WORK array, returns
*>          this value as the first entry of the WORK array, and no error
*>          message related to LWORK is issued by XERBLA.
*> \endverbatim
*>
*> \param[out] INFO
*> \verbatim
*>          INFO is INTEGER
*>          = 0: successful exit
*>          < 0: if INFO = -i, the i-th argument had an illegal value
*>          > 0: if INFO = i, D(i,i) is exactly zero.  The factorization
*>               has been completed, but the block diagonal matrix D is
*>               exactly singular, so the solution could not be computed.
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
*> \ingroup hesv_aa
*
*  =====================================================================
      SUBROUTINE CHESV_AA( UPLO, N, NRHS, A, LDA, IPIV, B, LDB, WORK,
     $                     LWORK, INFO )
*
*  -- LAPACK driver routine --
*  -- LAPACK is a software package provided by Univ. of Tennessee,    --
*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
*
*     .. Scalar Arguments ..
      CHARACTER          UPLO
      INTEGER            INFO, LDA, LDB, LWORK, N, NRHS
*     ..
*     .. Array Arguments ..
      INTEGER            IPIV( * )
      COMPLEX            A( LDA, * ), B( LDB, * ), WORK( * )
*     ..
*
*  =====================================================================
*
*     .. Local Scalars ..
      LOGICAL            LQUERY
      INTEGER            LWKMIN, LWKOPT, LWKOPT_HETRF, LWKOPT_HETRS
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      INTEGER            ILAENV
      REAL               SROUNDUP_LWORK
      EXTERNAL           LSAME, ILAENV, SROUNDUP_LWORK
*     ..
*     .. External Subroutines ..
      EXTERNAL           XERBLA, CHETRF_AA, CHETRS_AA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters.
*
      INFO = 0
      LQUERY = ( LWORK.EQ.-1 )
      LWKMIN = MAX( 1, 2*N, 3*N-2 )
      IF( .NOT.LSAME( UPLO, 'U' ) .AND.
     $    .NOT.LSAME( UPLO, 'L' ) ) THEN
         INFO = -1
      ELSE IF( N.LT.0 ) THEN
         INFO = -2
      ELSE IF( NRHS.LT.0 ) THEN
         INFO = -3
      ELSE IF( LDA.LT.MAX( 1, N ) ) THEN
         INFO = -5
      ELSE IF( LDB.LT.MAX( 1, N ) ) THEN
         INFO = -8
      ELSE IF( LWORK.LT.LWKMIN .AND. .NOT.LQUERY ) THEN
         INFO = -10
      END IF
*
      IF( INFO.EQ.0 ) THEN
         CALL CHETRF_AA( UPLO, N, A, LDA, IPIV, WORK, -1, INFO )
         LWKOPT_HETRF = INT( WORK( 1 ) )
         CALL CHETRS_AA( UPLO, N, NRHS, A, LDA, IPIV, B, LDB, WORK,
     $                   -1, INFO )
         LWKOPT_HETRS = INT( WORK( 1 ) )
         LWKOPT = MAX( LWKMIN, LWKOPT_HETRF, LWKOPT_HETRS )
         WORK( 1 ) = SROUNDUP_LWORK( LWKOPT )
      END IF
*
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'CHESV_AA ', -INFO )
         RETURN
      ELSE IF( LQUERY ) THEN
         RETURN
      END IF
*
*     Compute the factorization A = U**H*T*U or A = L*T*L**H.
*
      CALL CHETRF_AA( UPLO, N, A, LDA, IPIV, WORK, LWORK, INFO )
      IF( INFO.EQ.0 ) THEN
*
*        Solve the system A*X = B, overwriting B with X.
*
         CALL CHETRS_AA( UPLO, N, NRHS, A, LDA, IPIV, B, LDB, WORK,
     $                      LWORK, INFO )
*
      END IF
*
      WORK( 1 ) = SROUNDUP_LWORK( LWKOPT )
*
      RETURN
*
*     End of CHESV_AA
*
      END
