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
* Contents: Native high-level C interface to LAPACK function zsysv_aa
* Author: Intel Corporation
*****************************************************************************/

#include "lapacke_utils.h"

lapack_int API_SUFFIX(LAPACKE_zsysv_aa_2stage)( int matrix_layout, char uplo, lapack_int n,
                          lapack_int nrhs, lapack_complex_double* a, lapack_int lda,
                          lapack_complex_double* tb, lapack_int ltb, lapack_int* ipiv,
                          lapack_int* ipiv2, lapack_complex_double* b, lapack_int ldb )
{
    lapack_int info = 0;
    lapack_int lwork = -1;
    lapack_complex_double* work = NULL;
    lapack_complex_double work_query;
    if( matrix_layout != LAPACK_COL_MAJOR && matrix_layout != LAPACK_ROW_MAJOR ) {
        API_SUFFIX(LAPACKE_xerbla)( "LAPACKE_zsysv_aa_2stage", -1 );
        return -1;
    }
#ifndef LAPACK_DISABLE_NAN_CHECK
    if( LAPACKE_get_nancheck() ) {
        /* Optionally check input matrices for NaNs */
        if( API_SUFFIX(LAPACKE_zsy_nancheck)( matrix_layout, uplo, n, a, lda ) ) {
            return -5;
        }
        if( API_SUFFIX(LAPACKE_zge_nancheck)( matrix_layout, 4*n, 1, tb, ltb ) ) {
            return -7;
        }
        if( API_SUFFIX(LAPACKE_zge_nancheck)( matrix_layout, n, nrhs, b, ldb ) ) {
            return -11;
        }
    }
#endif
    /* Query optimal working array(s) size */
    info = API_SUFFIX(LAPACKE_zsysv_aa_2stage_work)( matrix_layout, uplo, n, nrhs,
    						   a, lda, tb, ltb, ipiv, ipiv2, b,
                               ldb, &work_query, lwork );
    if( info != 0 ) {
        goto exit_level_0;
    }
    lwork = LAPACK_Z2INT( work_query );
    /* Allocate memory for work arrays */
    work = (lapack_complex_double*)
        LAPACKE_malloc( sizeof(lapack_complex_double) * lwork );
    if( work == NULL ) {
        info = LAPACK_WORK_MEMORY_ERROR;
        goto exit_level_0;
    }
    /* Call middle-level interface */
    info = API_SUFFIX(LAPACKE_zsysv_aa_2stage_work)( matrix_layout, uplo, n, nrhs,
                               a, lda, tb, ltb, ipiv, ipiv2, b,
                               ldb, work, lwork );
    /* Release memory and exit */
    LAPACKE_free( work );
exit_level_0:
    if( info == LAPACK_WORK_MEMORY_ERROR ) {
        API_SUFFIX(LAPACKE_xerbla)( "LAPACKE_zsysv_aa_2stage", info );
    }
    return info;
}
