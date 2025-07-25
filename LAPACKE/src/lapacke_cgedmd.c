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
* Contents: Native high-level C interface to LAPACK function cgedmd
* Author: Intel Corporation
*****************************************************************************/

#include "lapacke_utils.h"

lapack_int API_SUFFIX(LAPACKE_cgedmd)( int matrix_layout, char jobs, char jobz, char jobr,
			   char jobf, lapack_int whtsvd, lapack_int m,
		 	   lapack_int n, lapack_complex_float* x,
			   lapack_int ldx, lapack_complex_float* y,
			   lapack_int ldy, lapack_int nrnk, float* tol,
			   lapack_int k, lapack_complex_float* eigs,
               lapack_complex_float* z, lapack_int ldz,
               float* res, lapack_complex_float* b,
               lapack_int ldb, lapack_complex_float* w,
               lapack_int ldw, lapack_complex_float* s, lapack_int lds)
{
    lapack_int info = 0;
    lapack_int lwork = -1;
    lapack_int liwork = -1;
    lapack_int lzwork = -1;
    lapack_complex_float* zwork = NULL;
    float* work = NULL;
    lapack_int* iwork = NULL;
    lapack_complex_float zwork_query;
    float work_query;
    lapack_int iwork_query;
    if( matrix_layout != LAPACK_COL_MAJOR && matrix_layout != LAPACK_ROW_MAJOR ) {
        API_SUFFIX(LAPACKE_xerbla)( "LAPACKE_cgedmd", -1 );
        return -1;
    }
#ifndef LAPACK_DISABLE_NAN_CHECK
    if( LAPACKE_get_nancheck() ) {
        /* Optionally check input matrices for NaNs */
        if( API_SUFFIX(LAPACKE_cge_nancheck)( matrix_layout, m, n, x, ldx ) ) {
            return -8;
        }
        if( API_SUFFIX(LAPACKE_cge_nancheck)( matrix_layout, m, n, y, ldy ) ) {
            return -10;
        }
        if( API_SUFFIX(LAPACKE_cge_nancheck)( matrix_layout, m, n, z, ldz ) ) {
            return -15;
        }
        if( API_SUFFIX(LAPACKE_cge_nancheck)( matrix_layout, m, n, b, ldb ) ) {
            return -18;
        }
        if( API_SUFFIX(LAPACKE_cge_nancheck)( matrix_layout, m, n, w, ldw ) ) {
            return -20;
        }
        if( API_SUFFIX(LAPACKE_cge_nancheck)( matrix_layout, m, n, s, lds ) ) {
            return -22;
        }
    }
#endif
    /* Query optimal working array(s) size */
    info = API_SUFFIX(LAPACKE_cgedmd_work)( matrix_layout, jobs, jobz, jobr, jobf, whtsvd,
				m, n, x, ldx, y, ldy, nrnk, tol, k, eigs, z, ldz,
				res, b, ldb, w, ldw, s, lds, &zwork_query,
				lzwork, &work_query, lwork, &iwork_query, liwork );

    if( info != 0 ) {
        goto exit_level_0;
    }
    lzwork  = LAPACK_C2INT( zwork_query );
    lwork  = LAPACK_C2INT( work_query );
    liwork = iwork_query;
    /* Allocate memory for work arrays */
    zwork  = (lapack_complex_float*)LAPACKE_malloc( sizeof(lapack_complex_float) * lzwork );
    if( zwork == NULL ) {
        info = LAPACK_WORK_MEMORY_ERROR;
        goto exit_level_0;
    }
    work  = (float*)LAPACKE_malloc( sizeof(float) * lwork );
    if( work == NULL ) {
        info = LAPACK_WORK_MEMORY_ERROR;
        goto exit_level_1;
    }
    iwork = (lapack_int*)LAPACKE_malloc( sizeof(lapack_int) * liwork );
    if( iwork == NULL ) {
        info = LAPACK_WORK_MEMORY_ERROR;
        goto exit_level_2;
    }
    /* Call middle-level interface */
    info = API_SUFFIX(LAPACKE_cgedmd_work)( matrix_layout, jobs, jobz, jobr, jobf, whtsvd,
				m, n, x, ldx, y, ldy, nrnk, tol, k, eigs, z, ldz,
				res, b, ldb, w, ldw, s, lds, zwork, lzwork,
				work, lwork, iwork, liwork );
    /* Release memory and exit */
    LAPACKE_free( iwork );
exit_level_2:
    LAPACKE_free( work );
exit_level_1:
    LAPACKE_free( zwork );
exit_level_0:
    if( info == LAPACK_WORK_MEMORY_ERROR ) {
        API_SUFFIX(LAPACKE_xerbla)( "LAPACKE_cgedmd", info );
    }
    return info;
}
