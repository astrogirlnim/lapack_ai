*> \brief \b CLASCL2 performs diagonal scaling on a matrix.
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*> Download CLASCL2 + dependencies
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.tgz?format=tgz&filename=/lapack/lapack_routine/clascl2.f">
*> [TGZ]</a>
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.zip?format=zip&filename=/lapack/lapack_routine/clascl2.f">
*> [ZIP]</a>
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.txt?format=txt&filename=/lapack/lapack_routine/clascl2.f">
*> [TXT]</a>
*
*  Definition:
*  ===========
*
*       SUBROUTINE CLASCL2 ( M, N, D, X, LDX )
*
*       .. Scalar Arguments ..
*       INTEGER            M, N, LDX
*       ..
*       .. Array Arguments ..
*       REAL               D( * )
*       COMPLEX            X( LDX, * )
*       ..
*
*
*> \par Purpose:
*  =============
*>
*> \verbatim
*>
*> CLASCL2 performs a diagonal scaling on a matrix:
*>   x <-- D * x
*> where the diagonal REAL matrix D is stored as a matrix.
*>
*> Eventually to be replaced by BLAS_cge_diag_scale in the new BLAS
*> standard.
*> \endverbatim
*
*  Arguments:
*  ==========
*
*> \param[in] M
*> \verbatim
*>          M is INTEGER
*>     The number of rows of D and X. M >= 0.
*> \endverbatim
*>
*> \param[in] N
*> \verbatim
*>          N is INTEGER
*>     The number of columns of X. N >= 0.
*> \endverbatim
*>
*> \param[in] D
*> \verbatim
*>          D is REAL array, length M
*>     Diagonal matrix D, stored as a vector of length M.
*> \endverbatim
*>
*> \param[in,out] X
*> \verbatim
*>          X is COMPLEX array, dimension (LDX,N)
*>     On entry, the matrix X to be scaled by D.
*>     On exit, the scaled matrix.
*> \endverbatim
*>
*> \param[in] LDX
*> \verbatim
*>          LDX is INTEGER
*>     The leading dimension of the matrix X. LDX >= M.
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
*> \ingroup lascl2
*
*  =====================================================================
      SUBROUTINE CLASCL2 ( M, N, D, X, LDX )
*
*  -- LAPACK computational routine --
*  -- LAPACK is a software package provided by Univ. of Tennessee,    --
*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
*
*     .. Scalar Arguments ..
      INTEGER            M, N, LDX
*     ..
*     .. Array Arguments ..
      REAL               D( * )
      COMPLEX            X( LDX, * )
*     ..
*
*  =====================================================================
*
*     .. Local Scalars ..
      INTEGER            I, J
*     ..
*     .. Executable Statements ..
*
      DO J = 1, N
         DO I = 1, M
            X( I, J ) = X( I, J ) * D( I )
         END DO
      END DO

      RETURN
      END
