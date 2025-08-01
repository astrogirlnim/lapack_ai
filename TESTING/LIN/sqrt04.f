*> \brief \b SQRT04
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*  Definition:
*  ===========
*
*       SUBROUTINE SQRT04(M,N,NB,RESULT)
*
*       .. Scalar Arguments ..
*       INTEGER M, N, NB, LDT
*       .. Return values ..
*       REAL RESULT(6)
*
*
*> \par Purpose:
*  =============
*>
*> \verbatim
*>
*> SQRT04 tests SGEQRT and SGEMQRT.
*> \endverbatim
*
*  Arguments:
*  ==========
*
*> \param[in] M
*> \verbatim
*>          M is INTEGER
*>          Number of rows in test matrix.
*> \endverbatim
*>
*> \param[in] N
*> \verbatim
*>          N is INTEGER
*>          Number of columns in test matrix.
*> \endverbatim
*>
*> \param[in] NB
*> \verbatim
*>          NB is INTEGER
*>          Block size of test matrix.  NB <= Min(M,N).
*> \endverbatim
*>
*> \param[out] RESULT
*> \verbatim
*>          RESULT is REAL array, dimension (6)
*>          Results of each of the six tests below.
*>
*>          RESULT(1) = | A - Q R |
*>          RESULT(2) = | I - Q^H Q |
*>          RESULT(3) = | Q C - Q C |
*>          RESULT(4) = | Q^H C - Q^H C |
*>          RESULT(5) = | C Q - C Q |
*>          RESULT(6) = | C Q^H - C Q^H |
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
*> \ingroup single_lin
*
*  =====================================================================
      SUBROUTINE SQRT04(M,N,NB,RESULT)
      IMPLICIT NONE
*
*  -- LAPACK test routine --
*  -- LAPACK is a software package provided by Univ. of Tennessee,    --
*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
*
*     .. Scalar Arguments ..
      INTEGER M, N, NB, LDT
*     .. Return values ..
      REAL RESULT(6)
*
*  =====================================================================
*
*     ..
*     .. Local allocatable arrays
      REAL, ALLOCATABLE :: AF(:,:), Q(:,:),
     $  R(:,:), RWORK(:), WORK( : ), T(:,:),
     $  CF(:,:), DF(:,:), A(:,:), C(:,:), D(:,:)
*
*     .. Parameters ..
      REAL ONE, ZERO
      PARAMETER( ZERO = 0.0, ONE = 1.0 )
*     ..
*     .. Local Scalars ..
      INTEGER INFO, J, K, L, LWORK
      REAL   ANORM, EPS, RESID, CNORM, DNORM
*     ..
*     .. Local Arrays ..
      INTEGER            ISEED( 4 )
*     ..
*     .. External Subroutine ..
      EXTERNAL SGEMM, SLACPY, SLARNV, SGEMQRT, SLASET, SGEQRT, SSYRK
*     ..
*     .. External Functions ..
      REAL SLAMCH
      REAL SLANGE, SLANSY
      LOGICAL  LSAME
      EXTERNAL SLAMCH, SLANGE, SLANSY, LSAME
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC  MAX, MIN
*     ..
*     .. Data statements ..
      DATA ISEED / 1988, 1989, 1990, 1991 /
*
      EPS = SLAMCH( 'Epsilon' )
      K = MIN(M,N)
      L = MAX(M,N)
      LWORK = MAX(2,L)*MAX(2,L)*NB
*
*     Dynamically allocate local arrays
*
      ALLOCATE ( A(M,N), AF(M,N), Q(M,M), R(M,L), RWORK(L),
     $           WORK(LWORK), T(NB,N), C(M,N), CF(M,N),
     $           D(N,M), DF(N,M) )
*
*     Put random numbers into A and copy to AF
*
      LDT=NB
      DO J=1,N
         CALL SLARNV( 2, ISEED, M, A( 1, J ) )
      END DO
      CALL SLACPY( 'Full', M, N, A, M, AF, M )
*
*     Factor the matrix A in the array AF.
*
      CALL SGEQRT( M, N, NB, AF, M, T, LDT, WORK, INFO )
*
*     Generate the m-by-m matrix Q
*
      CALL SLASET( 'Full', M, M, ZERO, ONE, Q, M )
      CALL SGEMQRT( 'R', 'N', M, M, K, NB, AF, M, T, LDT, Q, M,
     $              WORK, INFO )
*
*     Copy R
*
      CALL SLASET( 'Full', M, N, ZERO, ZERO, R, M )
      CALL SLACPY( 'Upper', M, N, AF, M, R, M )
*
*     Compute |R - Q'*A| / |A| and store in RESULT(1)
*
      CALL SGEMM( 'T', 'N', M, N, M, -ONE, Q, M, A, M, ONE, R, M )
      ANORM = SLANGE( '1', M, N, A, M, RWORK )
      RESID = SLANGE( '1', M, N, R, M, RWORK )
      IF( ANORM.GT.ZERO ) THEN
         RESULT( 1 ) = RESID / (EPS*MAX(1,M)*ANORM)
      ELSE
         RESULT( 1 ) = ZERO
      END IF
*
*     Compute |I - Q'*Q| and store in RESULT(2)
*
      CALL SLASET( 'Full', M, M, ZERO, ONE, R, M )
      CALL SSYRK( 'U', 'C', M, M, -ONE, Q, M, ONE, R, M )
      RESID = SLANSY( '1', 'Upper', M, R, M, RWORK )
      RESULT( 2 ) = RESID / (EPS*MAX(1,M))
*
*     Generate random m-by-n matrix C and a copy CF
*
      DO J=1,N
         CALL SLARNV( 2, ISEED, M, C( 1, J ) )
      END DO
      CNORM = SLANGE( '1', M, N, C, M, RWORK)
      CALL SLACPY( 'Full', M, N, C, M, CF, M )
*
*     Apply Q to C as Q*C
*
      CALL SGEMQRT( 'L', 'N', M, N, K, NB, AF, M, T, NB, CF, M,
     $             WORK, INFO)
*
*     Compute |Q*C - Q*C| / |C|
*
      CALL SGEMM( 'N', 'N', M, N, M, -ONE, Q, M, C, M, ONE, CF, M )
      RESID = SLANGE( '1', M, N, CF, M, RWORK )
      IF( CNORM.GT.ZERO ) THEN
         RESULT( 3 ) = RESID / (EPS*MAX(1,M)*CNORM)
      ELSE
         RESULT( 3 ) = ZERO
      END IF
*
*     Copy C into CF again
*
      CALL SLACPY( 'Full', M, N, C, M, CF, M )
*
*     Apply Q to C as QT*C
*
      CALL SGEMQRT( 'L', 'T', M, N, K, NB, AF, M, T, NB, CF, M,
     $             WORK, INFO)
*
*     Compute |QT*C - QT*C| / |C|
*
      CALL SGEMM( 'T', 'N', M, N, M, -ONE, Q, M, C, M, ONE, CF, M )
      RESID = SLANGE( '1', M, N, CF, M, RWORK )
      IF( CNORM.GT.ZERO ) THEN
         RESULT( 4 ) = RESID / (EPS*MAX(1,M)*CNORM)
      ELSE
         RESULT( 4 ) = ZERO
      END IF
*
*     Generate random n-by-m matrix D and a copy DF
*
      DO J=1,M
         CALL SLARNV( 2, ISEED, N, D( 1, J ) )
      END DO
      DNORM = SLANGE( '1', N, M, D, N, RWORK)
      CALL SLACPY( 'Full', N, M, D, N, DF, N )
*
*     Apply Q to D as D*Q
*
      CALL SGEMQRT( 'R', 'N', N, M, K, NB, AF, M, T, NB, DF, N,
     $             WORK, INFO)
*
*     Compute |D*Q - D*Q| / |D|
*
      CALL SGEMM( 'N', 'N', N, M, M, -ONE, D, N, Q, M, ONE, DF, N )
      RESID = SLANGE( '1', N, M, DF, N, RWORK )
      IF( CNORM.GT.ZERO ) THEN
         RESULT( 5 ) = RESID / (EPS*MAX(1,M)*DNORM)
      ELSE
         RESULT( 5 ) = ZERO
      END IF
*
*     Copy D into DF again
*
      CALL SLACPY( 'Full', N, M, D, N, DF, N )
*
*     Apply Q to D as D*QT
*
      CALL SGEMQRT( 'R', 'T', N, M, K, NB, AF, M, T, NB, DF, N,
     $             WORK, INFO)
*
*     Compute |D*QT - D*QT| / |D|
*
      CALL SGEMM( 'N', 'T', N, M, M, -ONE, D, N, Q, M, ONE, DF, N )
      RESID = SLANGE( '1', N, M, DF, N, RWORK )
      IF( CNORM.GT.ZERO ) THEN
         RESULT( 6 ) = RESID / (EPS*MAX(1,M)*DNORM)
      ELSE
         RESULT( 6 ) = ZERO
      END IF
*
*     Deallocate all arrays
*
      DEALLOCATE ( A, AF, Q, R, RWORK, WORK, T, C, D, CF, DF)
*
      RETURN
      END
