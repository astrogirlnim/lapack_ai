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
* Contents: Native middle-level C interface to LAPACK function csytrf_aa
* Author: Intel Corporation
*****************************************************************************/

#include "lapacke_utils.h"

lapack_int API_SUFFIX(LAPACKE_csytrf_aa_2stage_work)( int matrix_layout, char uplo, lapack_int n,
                               lapack_complex_float* a, lapack_int lda,
                               lapack_complex_float* tb, lapack_int ltb,
                               lapack_int* ipiv, lapack_int* ipiv2,
                               lapack_complex_float* work, lapack_int lwork )
{
    lapack_int info = 0;
    if( matrix_layout == LAPACK_COL_MAJOR ) {
        /* Call LAPACK function and adjust info */
        LAPACK_csytrf_aa_2stage( &uplo, &n, a, &lda, tb,
        				 &ltb, ipiv, ipiv2, work, &lwork,
                         &info );
        if( info < 0 ) {
            info = info - 1;
        }
    } else if( matrix_layout == LAPACK_ROW_MAJOR ) {
        lapack_int lda_t = MAX(1,n);
        lapack_complex_float* a_t = NULL;
        lapack_complex_float* tb_t = NULL;
        /* Check leading dimension(s) */
        if( lda < n ) {
            info = -6;
            API_SUFFIX(LAPACKE_xerbla)( "LAPACKE_csytrf_aa_2stage_work", info );
            return info;
        }
        if( ltb < 4*n ) {
            info = -8;
            API_SUFFIX(LAPACKE_xerbla)( "LAPACKE_csytrf_aa_2stage_work", info );
            return info;
        }

        /* Query optimal working array(s) size if requested */
        if( lwork == -1 ) {
            LAPACK_csytrf_aa_2stage( &uplo, &n, a, &lda_t,
            			 tb, &ltb, ipiv, ipiv2, work,
                          &lwork, &info );
            return (info < 0) ? (info - 1) : info;
        }
        /* Allocate memory for temporary array(s) */
        a_t = (lapack_complex_float*)LAPACKE_malloc( sizeof(lapack_complex_float) * lda_t * MAX(1,n) );
        if( a_t == NULL ) {
            info = LAPACK_TRANSPOSE_MEMORY_ERROR;
            goto exit_level_0;
        }
        tb_t = (lapack_complex_float*)LAPACKE_malloc( sizeof(lapack_complex_float) * ltb );
        if( tb_t == NULL ) {
            info = LAPACK_TRANSPOSE_MEMORY_ERROR;
            goto exit_level_1;
        }
        /* Transpose input matrices */
        API_SUFFIX(LAPACKE_csy_trans)( matrix_layout, uplo, n, a, lda, a_t, lda_t );
        /* Call LAPACK function and adjust info */
        LAPACK_csytrf_aa_2stage( &uplo, &n, a_t, &lda_t,
        			  tb_t, &ltb, ipiv, ipiv2, work,
                      &lwork, &info );
        if( info < 0 ) {
            info = info - 1;
        }
        /* Transpose output matrices */
        API_SUFFIX(LAPACKE_csy_trans)( LAPACK_COL_MAJOR, uplo, n, a_t, lda_t, a, lda );
        /* Release memory and exit */
        LAPACKE_free( tb_t );
exit_level_1:
        LAPACKE_free( a_t );
exit_level_0:
        if( info == LAPACK_TRANSPOSE_MEMORY_ERROR ) {
            API_SUFFIX(LAPACKE_xerbla)( "LAPACKE_csytrf_aa_2stage_work", info );
        }
    } else {
        info = -1;
        API_SUFFIX(LAPACKE_xerbla)( "LAPACKE_csytrf_aa_2stage_work", info );
    }
    return info;
}
