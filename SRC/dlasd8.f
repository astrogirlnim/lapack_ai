*> \brief \b DLASD8 finds the square roots of the roots of the secular equation, and stores, for each element in D, the distance to its two nearest poles. Used by sbdsdc.
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*> Download DLASD8 + dependencies
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.tgz?format=tgz&filename=/lapack/lapack_routine/dlasd8.f">
*> [TGZ]</a>
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.zip?format=zip&filename=/lapack/lapack_routine/dlasd8.f">
*> [ZIP]</a>
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.txt?format=txt&filename=/lapack/lapack_routine/dlasd8.f">
*> [TXT]</a>
*
*  Definition:
*  ===========
*
*       SUBROUTINE DLASD8( ICOMPQ, K, D, Z, VF, VL, DIFL, DIFR, LDDIFR,
*                          DSIGMA, WORK, INFO )
*
*       .. Scalar Arguments ..
*       INTEGER            ICOMPQ, INFO, K, LDDIFR
*       ..
*       .. Array Arguments ..
*       DOUBLE PRECISION   D( * ), DIFL( * ), DIFR( LDDIFR, * ),
*      $                   DSIGMA( * ), VF( * ), VL( * ), WORK( * ),
*      $                   Z( * )
*       ..
*
*
*> \par Purpose:
*  =============
*>
*> \verbatim
*>
*> DLASD8 finds the square roots of the roots of the secular equation,
*> as defined by the values in DSIGMA and Z. It makes the appropriate
*> calls to DLASD4, and stores, for each  element in D, the distance
*> to its two nearest poles (elements in DSIGMA). It also updates
*> the arrays VF and VL, the first and last components of all the
*> right singular vectors of the original bidiagonal matrix.
*>
*> DLASD8 is called from DLASD6.
*> \endverbatim
*
*  Arguments:
*  ==========
*
*> \param[in] ICOMPQ
*> \verbatim
*>          ICOMPQ is INTEGER
*>          Specifies whether singular vectors are to be computed in
*>          factored form in the calling routine:
*>          = 0: Compute singular values only.
*>          = 1: Compute singular vectors in factored form as well.
*> \endverbatim
*>
*> \param[in] K
*> \verbatim
*>          K is INTEGER
*>          The number of terms in the rational function to be solved
*>          by DLASD4.  K >= 1.
*> \endverbatim
*>
*> \param[out] D
*> \verbatim
*>          D is DOUBLE PRECISION array, dimension ( K )
*>          On output, D contains the updated singular values.
*> \endverbatim
*>
*> \param[in,out] Z
*> \verbatim
*>          Z is DOUBLE PRECISION array, dimension ( K )
*>          On entry, the first K elements of this array contain the
*>          components of the deflation-adjusted updating row vector.
*>          On exit, Z is updated.
*> \endverbatim
*>
*> \param[in,out] VF
*> \verbatim
*>          VF is DOUBLE PRECISION array, dimension ( K )
*>          On entry, VF contains  information passed through DBEDE8.
*>          On exit, VF contains the first K components of the first
*>          components of all right singular vectors of the bidiagonal
*>          matrix.
*> \endverbatim
*>
*> \param[in,out] VL
*> \verbatim
*>          VL is DOUBLE PRECISION array, dimension ( K )
*>          On entry, VL contains  information passed through DBEDE8.
*>          On exit, VL contains the first K components of the last
*>          components of all right singular vectors of the bidiagonal
*>          matrix.
*> \endverbatim
*>
*> \param[out] DIFL
*> \verbatim
*>          DIFL is DOUBLE PRECISION array, dimension ( K )
*>          On exit, DIFL(I) = D(I) - DSIGMA(I).
*> \endverbatim
*>
*> \param[out] DIFR
*> \verbatim
*>          DIFR is DOUBLE PRECISION array,
*>                   dimension ( LDDIFR, 2 ) if ICOMPQ = 1 and
*>                   dimension ( K ) if ICOMPQ = 0.
*>          On exit, DIFR(I,1) = D(I) - DSIGMA(I+1), DIFR(K,1) is not
*>          defined and will not be referenced.
*>
*>          If ICOMPQ = 1, DIFR(1:K,2) is an array containing the
*>          normalizing factors for the right singular vector matrix.
*> \endverbatim
*>
*> \param[in] LDDIFR
*> \verbatim
*>          LDDIFR is INTEGER
*>          The leading dimension of DIFR, must be at least K.
*> \endverbatim
*>
*> \param[in] DSIGMA
*> \verbatim
*>          DSIGMA is DOUBLE PRECISION array, dimension ( K )
*>          On entry, the first K elements of this array contain the old
*>          roots of the deflated updating problem.  These are the poles
*>          of the secular equation.
*> \endverbatim
*>
*> \param[out] WORK
*> \verbatim
*>          WORK is DOUBLE PRECISION array, dimension (3*K)
*> \endverbatim
*>
*> \param[out] INFO
*> \verbatim
*>          INFO is INTEGER
*>          = 0:  successful exit.
*>          < 0:  if INFO = -i, the i-th argument had an illegal value.
*>          > 0:  if INFO = 1, a singular value did not converge
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
*> \ingroup lasd8
*
*> \par Contributors:
*  ==================
*>
*>     Ming Gu and Huan Ren, Computer Science Division, University of
*>     California at Berkeley, USA
*>
*  =====================================================================
      SUBROUTINE DLASD8( ICOMPQ, K, D, Z, VF, VL, DIFL, DIFR, LDDIFR,
     $                   DSIGMA, WORK, INFO )
*
*  -- LAPACK auxiliary routine --
*  -- LAPACK is a software package provided by Univ. of Tennessee,    --
*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
*
*     .. Scalar Arguments ..
      INTEGER            ICOMPQ, INFO, K, LDDIFR
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   D( * ), DIFL( * ), DIFR( LDDIFR, * ),
     $                   DSIGMA( * ), VF( * ), VL( * ), WORK( * ),
     $                   Z( * )
*     ..
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ONE
      PARAMETER          ( ONE = 1.0D+0 )
*     ..
*     .. Local Scalars ..
      INTEGER            I, IWK1, IWK2, IWK2I, IWK3, IWK3I, J
      DOUBLE PRECISION   DIFLJ, DIFRJ, DJ, DSIGJ, DSIGJP, RHO, TEMP
*     ..
*     .. External Subroutines ..
      EXTERNAL           DCOPY, DLASCL, DLASD4, DLASET,
     $                   XERBLA
*     ..
*     .. External Functions ..
      DOUBLE PRECISION   DDOT, DLAMC3, DNRM2
      EXTERNAL           DDOT, DLAMC3, DNRM2
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, SIGN, SQRT
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters.
*
      INFO = 0
*
      IF( ( ICOMPQ.LT.0 ) .OR. ( ICOMPQ.GT.1 ) ) THEN
         INFO = -1
      ELSE IF( K.LT.1 ) THEN
         INFO = -2
      ELSE IF( LDDIFR.LT.K ) THEN
         INFO = -9
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DLASD8', -INFO )
         RETURN
      END IF
*
*     Quick return if possible
*
      IF( K.EQ.1 ) THEN
         D( 1 ) = ABS( Z( 1 ) )
         DIFL( 1 ) = D( 1 )
         IF( ICOMPQ.EQ.1 ) THEN
            DIFL( 2 ) = ONE
            DIFR( 1, 2 ) = ONE
         END IF
         RETURN
      END IF
*
*     Book keeping.
*
      IWK1 = 1
      IWK2 = IWK1 + K
      IWK3 = IWK2 + K
      IWK2I = IWK2 - 1
      IWK3I = IWK3 - 1
*
*     Normalize Z.
*
      RHO = DNRM2( K, Z, 1 )
      CALL DLASCL( 'G', 0, 0, RHO, ONE, K, 1, Z, K, INFO )
      RHO = RHO*RHO
*
*     Initialize WORK(IWK3).
*
      CALL DLASET( 'A', K, 1, ONE, ONE, WORK( IWK3 ), K )
*
*     Compute the updated singular values, the arrays DIFL, DIFR,
*     and the updated Z.
*
      DO 40 J = 1, K
         CALL DLASD4( K, J, DSIGMA, Z, WORK( IWK1 ), RHO, D( J ),
     $                WORK( IWK2 ), INFO )
*
*        If the root finder fails, report the convergence failure.
*
         IF( INFO.NE.0 ) THEN
            RETURN
         END IF
         WORK( IWK3I+J ) = WORK( IWK3I+J )*WORK( J )*WORK( IWK2I+J )
         DIFL( J ) = -WORK( J )
         DIFR( J, 1 ) = -WORK( J+1 )
         DO 20 I = 1, J - 1
            WORK( IWK3I+I ) = WORK( IWK3I+I )*WORK( I )*
     $                        WORK( IWK2I+I ) / ( DSIGMA( I )-
     $                        DSIGMA( J ) ) / ( DSIGMA( I )+
     $                        DSIGMA( J ) )
   20    CONTINUE
         DO 30 I = J + 1, K
            WORK( IWK3I+I ) = WORK( IWK3I+I )*WORK( I )*
     $                        WORK( IWK2I+I ) / ( DSIGMA( I )-
     $                        DSIGMA( J ) ) / ( DSIGMA( I )+
     $                        DSIGMA( J ) )
   30    CONTINUE
   40 CONTINUE
*
*     Compute updated Z.
*
      DO 50 I = 1, K
         Z( I ) = SIGN( SQRT( ABS( WORK( IWK3I+I ) ) ), Z( I ) )
   50 CONTINUE
*
*     Update VF and VL.
*
      DO 80 J = 1, K
         DIFLJ = DIFL( J )
         DJ = D( J )
         DSIGJ = -DSIGMA( J )
         IF( J.LT.K ) THEN
            DIFRJ = -DIFR( J, 1 )
            DSIGJP = -DSIGMA( J+1 )
         END IF
         WORK( J ) = -Z( J ) / DIFLJ / ( DSIGMA( J )+DJ )
*
*        Use calls to the subroutine DLAMC3 to enforce the parentheses
*        (x+y)+z. The goal is to prevent optimizing compilers
*        from doing x+(y+z).
*
         DO 60 I = 1, J - 1
            WORK( I ) = Z( I ) / ( DLAMC3( DSIGMA( I ),
     $            DSIGJ )-DIFLJ )
     $                   / ( DSIGMA( I )+DJ )
   60    CONTINUE
         DO 70 I = J + 1, K
            WORK( I ) = Z( I ) / ( DLAMC3( DSIGMA( I ),
     $            DSIGJP )+DIFRJ )
     $                   / ( DSIGMA( I )+DJ )
   70    CONTINUE
         TEMP = DNRM2( K, WORK, 1 )
         WORK( IWK2I+J ) = DDOT( K, WORK, 1, VF, 1 ) / TEMP
         WORK( IWK3I+J ) = DDOT( K, WORK, 1, VL, 1 ) / TEMP
         IF( ICOMPQ.EQ.1 ) THEN
            DIFR( J, 2 ) = TEMP
         END IF
   80 CONTINUE
*
      CALL DCOPY( K, WORK( IWK2 ), 1, VF, 1 )
      CALL DCOPY( K, WORK( IWK3 ), 1, VL, 1 )
*
      RETURN
*
*     End of DLASD8
*
      END
