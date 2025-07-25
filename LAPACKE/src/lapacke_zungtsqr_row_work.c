/*****************************************************************************
  Copyright (c) 2020, Intel Corp.
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
* Contents: Native middle-level C interface to LAPACK function zungtsqr_row
* Author: Intel Corporation
*****************************************************************************/

#include "lapacke_utils.h"

lapack_int API_SUFFIX(LAPACKE_zungtsqr_row_work)( int matrix_layout, lapack_int m, lapack_int n,
                                      lapack_int mb, lapack_int nb,
                                      lapack_complex_double* a, lapack_int lda,
                                      const lapack_complex_double* t, lapack_int ldt,
                                      lapack_complex_double* work, lapack_int lwork )
{
    lapack_int info = 0;
    if (matrix_layout == LAPACK_COL_MAJOR) {
        /* Call LAPACK function and adjust info */
        LAPACK_zungtsqr_row( &m, &n, &mb, &nb, a, &lda, t, &ldt,
                             work, &lwork, &info);
        if (info < 0) {
            info = info - 1;
        }
    } else if (matrix_layout == LAPACK_ROW_MAJOR) {
        lapack_int lda_t = MAX(1,m);
        lapack_complex_double* a_t = NULL;
        /* Check leading dimension(s) */
        if( lda < n ) {
            info = -7;
            API_SUFFIX(LAPACKE_xerbla)( "LAPACKE_zungtsqr_row_work", info );
            return info;
        }
        lapack_int ldt_t = MAX(1,nb);
        lapack_complex_double* t_t = NULL;
        /* Check leading dimension(s) */
        if( ldt < n ) {
            info = -9;
            API_SUFFIX(LAPACKE_xerbla)( "LAPACKE_zungtsqr_row_work", info );
            return info;
        }
        /* Query optimal working array(s) size if requested */
        if( lwork == -1 ) {
            LAPACK_zungtsqr_row( &m, &n, &mb, &nb, a, &lda_t, t, &ldt_t,
                                 work, &lwork, &info );
            return (info < 0) ? (info - 1) : info;
        }
        /* Allocate memory for temporary array(s) */
        a_t = (lapack_complex_double*)
            LAPACKE_malloc( sizeof(lapack_complex_double) * lda_t * MAX(1,n) );
        if( a_t == NULL ) {
            info = LAPACK_TRANSPOSE_MEMORY_ERROR;
            goto exit_level_0;
        }
        t_t = (lapack_complex_double*)
            LAPACKE_malloc( sizeof(lapack_complex_double) * ldt_t * MAX(1,n) );
        if( t_t == NULL ) {
            info = LAPACK_TRANSPOSE_MEMORY_ERROR;
            goto exit_level_1;
        }
        /* Transpose input matrices */
        API_SUFFIX(LAPACKE_zge_trans)( matrix_layout, m, n, a, lda, a_t, lda_t );
        API_SUFFIX(LAPACKE_zge_trans)( matrix_layout, nb, n, a, lda, t_t, ldt_t );
        /* Call LAPACK function and adjust info */
        LAPACK_zungtsqr_row( &m, &n, &mb, &nb, a_t, &lda_t, t_t, &ldt_t,
                             work, &lwork, &info );
        if( info < 0 ) {
            info = info - 1;
        }
        /* Transpose output matrices */
        API_SUFFIX(LAPACKE_zge_trans)( LAPACK_COL_MAJOR, m, n, a_t, lda_t, a, lda );
        /* Release memory and exit */
        LAPACKE_free( t_t );
exit_level_1:
        LAPACKE_free( a_t );
exit_level_0:
        if( info == LAPACK_TRANSPOSE_MEMORY_ERROR ) {
            API_SUFFIX(LAPACKE_xerbla)( "LAPACKE_zungtsqr_row_work", info );
        }
    } else {
        info = -1;
        API_SUFFIX(LAPACKE_xerbla)( "LAPACKE_zungtsqr_row_work", info );
    }
    return info;
}
