/*****************************************************************************
  Copyright (c) 2014, Intel Corp.
  All rights reserved.

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright notice,
      this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright
      notice, this list of conditions and the following disclaimer in the
      documentation and/or other materials provided with the distribution.
    * Neither the name of Intel Corporation nor the names of its contributors
      may be used to endorse or promote products derived from this software
      without specific prior written permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
  ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
  SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
  CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
  ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF
  THE POSSIBILITY OF SUCH DAMAGE.
*****************************************************************************
* Contents: Native high-level C interface to LAPACK function dgedmd
* Author: Intel Corporation
*****************************************************************************/

#include "lapacke_utils.h"

lapack_int API_SUFFIX(LAPACKE_dgedmd)( int matrix_layout, char jobs, char jobz, char jobr,
			   char jobf, lapack_int whtsvd, lapack_int m,
			   lapack_int n, double* x, lapack_int ldx, double* y,
			   lapack_int ldy, lapack_int nrnk, double* tol,
			   lapack_int k, double* reig, double* imeig,
			   double* z, lapack_int ldz,
			   double* res, double* b, lapack_int ldb,
               double* w, lapack_int ldw, double* s, lapack_int lds)
{
    lapack_int info = 0;
    lapack_int lwork = -1;
    lapack_int liwork = -1;
    double* work = NULL;
    lapack_int* iwork = NULL;
    double work_query;
    lapack_int iwork_query;
    if( matrix_layout != LAPACK_COL_MAJOR && matrix_layout != LAPACK_ROW_MAJOR ) {
        API_SUFFIX(LAPACKE_xerbla)( "LAPACKE_dgedmd", -1 );
        return -1;
    }
#ifndef LAPACK_DISABLE_NAN_CHECK
    if( LAPACKE_get_nancheck() ) {
        /* Optionally check input matrices for NaNs */
        if( API_SUFFIX(LAPACKE_dge_nancheck)( matrix_layout, m, n, x, ldx ) ) {
            return -8;
        }
        if( API_SUFFIX(LAPACKE_dge_nancheck)( matrix_layout, m, n, y, ldy ) ) {
            return -10;
        }
        if( API_SUFFIX(LAPACKE_dge_nancheck)( matrix_layout, m, n, z, ldz ) ) {
            return -15;
        }
        if( API_SUFFIX(LAPACKE_dge_nancheck)( matrix_layout, m, n, b, ldb ) ) {
            return -18;
        }
        if( API_SUFFIX(LAPACKE_dge_nancheck)( matrix_layout, m, n, s, lds ) ) {
            return -20;
        }
        if( API_SUFFIX(LAPACKE_dge_nancheck)( matrix_layout, m, n, w, ldw ) ) {
            return -22;
        }
    }
#endif
    /* Query optimal working array(s) size */
    info = API_SUFFIX(LAPACKE_dgedmd_work)( matrix_layout, jobs, jobz, jobr, jobf, whtsvd,
				m, n, x, ldx, y, ldy, nrnk, tol, k, reig, imeig, z, ldz,
				res, b, ldb, w, ldw, s, lds, &work_query,
				lwork, &iwork_query, liwork );

    if( info != 0 ) {
        goto exit_level_0;
    }
    lwork  = (lapack_int) work_query;
    liwork = iwork_query;
    /* Allocate memory for work arrays */
    work  = (double*)LAPACKE_malloc( sizeof(double) * lwork );
    if( work == NULL ) {
        info = LAPACK_WORK_MEMORY_ERROR;
        goto exit_level_0;
    }
    iwork = (lapack_int*)LAPACKE_malloc( sizeof(lapack_int) * liwork );
    if( iwork == NULL ) {
        info = LAPACK_WORK_MEMORY_ERROR;
        goto exit_level_1;
    }
    /* Call middle-level interface */
    info = API_SUFFIX(LAPACKE_dgedmd_work)( matrix_layout, jobs, jobz, jobr, jobf, whtsvd,
				m, n, x, ldx, y, ldy, nrnk, tol, k, reig, imeig, z, ldz,
				res, b, ldb, w, ldw, s, lds, work, lwork,
				iwork, liwork );
    /* Release memory and exit */
    LAPACKE_free( iwork );
exit_level_1:
    LAPACKE_free( work );
exit_level_0:
    if( info == LAPACK_WORK_MEMORY_ERROR ) {
        API_SUFFIX(LAPACKE_xerbla)( "LAPACKE_dgedmd", info );
    }
    return info;
}
