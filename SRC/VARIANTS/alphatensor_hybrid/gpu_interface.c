/*
 * GPU Interface for AlphaTensor Hybrid Implementation
 *
 * This module provides the C-Fortran interface for GPU-accelerated AlphaTensor
 * matrix multiplication, following CBLAS interface patterns for compatibility.
 *
 * Author: LAPACK AI Development Team
 * Based on: CBLAS interface patterns (cblas_dgemm.c) and Phase 9.1 plan
 * Reference: Fortran-C interoperability standards for LAPACK integration
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#include "opencl_manager.h"
#include "gpu_interface.h"

/* Forward declarations for matrix computation functions */
static int alphatensor_gpu_compute_4x4_internal(
    double alpha, const double* A, int lda,
    const double* B, int ldb, double beta,
    double* C, int ldc);

static int alphatensor_gpu_compute_batch_internal(
    int batch_size, const double* alpha_array,
    const double* A_batch, int lda,
    const double* B_batch, int ldb,
    const double* beta_array, double* C_batch, int ldc);

/*
 * =============================================================================
 * FORTRAN INTERFACE FUNCTIONS (Following CBLAS Pattern)
 * =============================================================================
 *
 * These functions are callable from Fortran using standard
 * underscore naming convention. They handle parameter marshalling
 * and provide seamless integration with LAPACK VARIANTS system.
 */

/*
 * GPU availability check for Fortran
 *
 * Returns: 1 if GPU available, 0 if not available
 *
 * Fortran calling example:
 *   EXTERNAL ALPHATENSOR_GPU_AVAILABLE
 *   INTEGER ALPHATENSOR_GPU_AVAILABLE
 *   IF (ALPHATENSOR_GPU_AVAILABLE() .EQ. 1) THEN
 *       ! Use GPU path
 *   END IF
 */
int alphatensor_gpu_available_(void) {
    int available = alphatensor_gpu_is_available();

    /* Log GPU availability status for debugging */
    if (available) {
        alphatensor_device_info_t device_info;
        if (alphatensor_gpu_get_device_info(&device_info) == 0) {
            fprintf(stdout, "[AlphaTensor GPU] GPU available: %s\n", device_info.device_name);
        }
    } else {
        fprintf(stdout, "[AlphaTensor GPU] GPU not available, falling back to CPU\n");
    }

    return available;
}

/*
 * Initialize GPU context for Fortran
 *
 * Returns: 0 on success, -1 on failure
 *
 * Fortran calling example:
 *   EXTERNAL ALPHATENSOR_GPU_INIT_FORTRAN
 *   INTEGER ALPHATENSOR_GPU_INIT_FORTRAN
 *   IF (ALPHATENSOR_GPU_INIT_FORTRAN() .EQ. 0) THEN
 *       ! GPU successfully initialized
 *   END IF
 */
int alphatensor_gpu_init_fortran_(void) {
    return alphatensor_gpu_global_init();
}

/*
 * Clean up GPU resources for Fortran
 *
 * Fortran calling example:
 *   EXTERNAL ALPHATENSOR_GPU_CLEANUP_FORTRAN
 *   CALL ALPHATENSOR_GPU_CLEANUP_FORTRAN()
 */
void alphatensor_gpu_cleanup_fortran_(void) {
    alphatensor_gpu_global_cleanup();
}

/*
 * Main GPU matrix multiplication interface for Fortran
 *
 * This is the primary function called from the Fortran DGEMM_ALPHA routine
 * when GPU acceleration is available and appropriate.
 *
 * Parameters follow DGEMM convention:
 *   alpha: Scaling factor for A*B
 *   A: 4x4 matrix A (column-major storage)
 *   lda: Leading dimension of A (should be 4 for 4x4)
 *   B: 4x4 matrix B (column-major storage)
 *   ldb: Leading dimension of B (should be 4 for 4x4)
 *   beta: Scaling factor for C
 *   C: 4x4 result matrix C (column-major storage)
 *   ldc: Leading dimension of C (should be 4 for 4x4)
 *
 * Returns: 0 on success, -1 on failure
 *
 * Fortran calling example:
 *   EXTERNAL DGEMM_ALPHA_GPU
 *   INTEGER DGEMM_ALPHA_GPU
 *   IF (DGEMM_ALPHA_GPU(ALPHA,A,LDA,B,LDB,BETA,C,LDC) .NE. 0) THEN
 *       ! GPU computation failed, use CPU fallback
 *   END IF
 */
int dgemm_alpha_gpu_(
    const double* alpha, const double* A, const int* lda,
    const double* B, const int* ldb, const double* beta,
    double* C, const int* ldc) {

    /* Parameter validation */
    if (!alpha || !A || !lda || !B || !ldb || !beta || !C || !ldc) {
        fprintf(stderr, "[AlphaTensor GPU] ERROR: Invalid parameters passed to dgemm_alpha_gpu_\n");
        return -1;
    }

    /* Validate matrix dimensions for 4x4 operation */
    if (*lda < 4 || *ldb < 4 || *ldc < 4) {
        fprintf(stderr, "[AlphaTensor GPU] ERROR: Invalid leading dimensions (lda=%d, ldb=%d, ldc=%d)\n",
                *lda, *ldb, *ldc);
        return -1;
    }

    /* Log operation for debugging */
    fprintf(stdout, "[AlphaTensor GPU] Computing 4x4 DGEMM with ALPHA=%f, BETA=%f\n", *alpha, *beta);

    /* Delegate to internal computation function */
    return alphatensor_gpu_compute_4x4_internal(*alpha, A, *lda, B, *ldb, *beta, C, *ldc);
}

/*
 * Batched matrix multiplication interface for Fortran
 *
 * Processes multiple 4x4 matrix multiplications in a single GPU kernel call
 * for maximum efficiency. This is particularly useful for ML workloads.
 *
 * Parameters:
 *   batch_size: Number of matrix multiplications to perform
 *   alpha_array: Array of scaling factors for A*B (size: batch_size)
 *   A_batch: Batch of 4x4 matrices A (size: 16 * batch_size)
 *   lda: Leading dimension of A matrices (should be 4)
 *   B_batch: Batch of 4x4 matrices B (size: 16 * batch_size)
 *   ldb: Leading dimension of B matrices (should be 4)
 *   beta_array: Array of scaling factors for C (size: batch_size)
 *   C_batch: Batch of result matrices C (size: 16 * batch_size)
 *   ldc: Leading dimension of C matrices (should be 4)
 *
 * Returns: 0 on success, -1 on failure
 */
int dgemm_alpha_gpu_batch_(
    const int* batch_size, const double* alpha_array,
    const double* A_batch, const int* lda,
    const double* B_batch, const int* ldb,
    const double* beta_array, double* C_batch, const int* ldc) {

    /* Parameter validation */
    if (!batch_size || *batch_size <= 0 || *batch_size > ALPHATENSOR_MAX_BATCH_SIZE) {
        fprintf(stderr, "[AlphaTensor GPU] ERROR: Invalid batch size %d\n",
                batch_size ? *batch_size : -1);
        return -1;
    }

    if (!alpha_array || !A_batch || !lda || !B_batch || !ldb ||
        !beta_array || !C_batch || !ldc) {
        fprintf(stderr, "[AlphaTensor GPU] ERROR: Invalid parameters for batch operation\n");
        return -1;
    }

    /* Validate dimensions */
    if (*lda < 4 || *ldb < 4 || *ldc < 4) {
        fprintf(stderr, "[AlphaTensor GPU] ERROR: Invalid leading dimensions for batch\n");
        return -1;
    }

    /* Log batch operation */
    fprintf(stdout, "[AlphaTensor GPU] Computing batch of %d 4x4 DGEMM operations\n", *batch_size);

    /* Delegate to internal batch computation function */
    return alphatensor_gpu_compute_batch_internal(*batch_size, alpha_array,
                                                 A_batch, *lda, B_batch, *ldb,
                                                 beta_array, C_batch, *ldc);
}

/*
 * =============================================================================
 * INTERNAL COMPUTATION FUNCTIONS
 * =============================================================================
 *
 * These functions handle the actual GPU computation logic and will be
 * fully implemented in Phase 9.2 when OpenCL kernels are added.
 * For Phase 9.1, they provide the interface framework.
 */

/*
 * Internal 4x4 matrix multiplication using GPU
 *
 * This function will implement the full 49-operation AlphaTensor algorithm
 * on the GPU using OpenCL kernels. For now, it provides the framework.
 */
static int alphatensor_gpu_compute_4x4_internal(
    double alpha, const double* A, int lda,
    const double* B, int ldb, double beta,
    double* C, int ldc) {

    /* Get OpenCL context */
    alphatensor_opencl_t* ctx = alphatensor_gpu_get_context();
    if (!ctx || !ctx->gpu_available) {
        fprintf(stderr, "[AlphaTensor GPU] ERROR: GPU context not available\n");
        return -1;
    }

    /* Matrix size validation */
    const size_t matrix_size = ALPHATENSOR_MATRIX_4X4_SIZE;

    /* Log computation details */
    fprintf(stdout, "[AlphaTensor GPU] Starting 4x4 computation on device\n");
    fprintf(stdout, "[AlphaTensor GPU] Matrix A (lda=%d):\n", lda);
    for (int i = 0; i < 4; i++) {
        fprintf(stdout, "  [");
        for (int j = 0; j < 4; j++) {
            fprintf(stdout, " %8.3f", A[j * lda + i]);
        }
        fprintf(stdout, " ]\n");
    }

    /*
     * PHASE 9.2 IMPLEMENTATION: ACTUAL OPENCL KERNEL EXECUTION
     *
     * Execute the 49-operation AlphaTensor algorithm on GPU using
     * the dgemm_alpha_4x4 kernel from dgemm_alpha.cl
     */

    cl_int err;

    /* Create OpenCL buffers for 4x4 matrices (16 elements each) */
    cl_mem buffer_A = clCreateBuffer(ctx->context, CL_MEM_READ_ONLY,
                                     16 * sizeof(double), NULL, &err);
    if (err != CL_SUCCESS) {
        fprintf(stderr, "[AlphaTensor GPU] ERROR: Failed to create buffer A (%d)\n", err);
        return -1;
    }

    cl_mem buffer_B = clCreateBuffer(ctx->context, CL_MEM_READ_ONLY,
                                     16 * sizeof(double), NULL, &err);
    if (err != CL_SUCCESS) {
        fprintf(stderr, "[AlphaTensor GPU] ERROR: Failed to create buffer B (%d)\n", err);
        clReleaseMemObject(buffer_A);
        return -1;
    }

    cl_mem buffer_C = clCreateBuffer(ctx->context, CL_MEM_READ_WRITE,
                                     16 * sizeof(double), NULL, &err);
    if (err != CL_SUCCESS) {
        fprintf(stderr, "[AlphaTensor GPU] ERROR: Failed to create buffer C (%d)\n", err);
        clReleaseMemObject(buffer_A);
        clReleaseMemObject(buffer_B);
        return -1;
    }

    /* Convert 4x4 matrices from column-major (Fortran) to row-major (OpenCL) */
    double A_flat[16], B_flat[16], C_flat[16];
    for (int i = 0; i < 4; i++) {
        for (int j = 0; j < 4; j++) {
            A_flat[i * 4 + j] = A[j * lda + i];  /* Column-major to row-major */
            B_flat[i * 4 + j] = B[j * ldb + i];
            C_flat[i * 4 + j] = C[j * ldc + i];
        }
    }

    /* Transfer matrices to GPU memory */
    err = clEnqueueWriteBuffer(ctx->queue, buffer_A, CL_TRUE, 0,
                               16 * sizeof(double), A_flat, 0, NULL, NULL);
    if (err != CL_SUCCESS) {
        fprintf(stderr, "[AlphaTensor GPU] ERROR: Failed to write buffer A (%d)\n", err);
        goto cleanup;
    }

    err = clEnqueueWriteBuffer(ctx->queue, buffer_B, CL_TRUE, 0,
                               16 * sizeof(double), B_flat, 0, NULL, NULL);
    if (err != CL_SUCCESS) {
        fprintf(stderr, "[AlphaTensor GPU] ERROR: Failed to write buffer B (%d)\n", err);
        goto cleanup;
    }

    err = clEnqueueWriteBuffer(ctx->queue, buffer_C, CL_TRUE, 0,
                               16 * sizeof(double), C_flat, 0, NULL, NULL);
    if (err != CL_SUCCESS) {
        fprintf(stderr, "[AlphaTensor GPU] ERROR: Failed to write buffer C (%d)\n", err);
        goto cleanup;
    }

    /* Set kernel arguments for dgemm_alpha_4x4 kernel */
    err = clSetKernelArg(ctx->kernel_4x4, 0, sizeof(cl_mem), &buffer_A);
    err |= clSetKernelArg(ctx->kernel_4x4, 1, sizeof(cl_mem), &buffer_B);
    err |= clSetKernelArg(ctx->kernel_4x4, 2, sizeof(cl_mem), &buffer_C);
    err |= clSetKernelArg(ctx->kernel_4x4, 3, sizeof(double), &alpha);
    err |= clSetKernelArg(ctx->kernel_4x4, 4, sizeof(double), &beta);

    if (err != CL_SUCCESS) {
        fprintf(stderr, "[AlphaTensor GPU] ERROR: Failed to set kernel arguments (%d)\n", err);
        goto cleanup;
    }

    /* Execute the AlphaTensor kernel (single work-item for 4x4 matrix) */
    size_t global_work_size = 1;
    err = clEnqueueNDRangeKernel(ctx->queue, ctx->kernel_4x4, 1, NULL,
                                 &global_work_size, NULL, 0, NULL, NULL);
    if (err != CL_SUCCESS) {
        fprintf(stderr, "[AlphaTensor GPU] ERROR: Failed to execute kernel (%d)\n", err);
        goto cleanup;
    }

    /* Wait for kernel completion */
    err = clFinish(ctx->queue);
    if (err != CL_SUCCESS) {
        fprintf(stderr, "[AlphaTensor GPU] ERROR: Failed to finish kernel execution (%d)\n", err);
        goto cleanup;
    }

    /* Read result back from GPU memory */
    err = clEnqueueReadBuffer(ctx->queue, buffer_C, CL_TRUE, 0,
                              16 * sizeof(double), C_flat, 0, NULL, NULL);
    if (err != CL_SUCCESS) {
        fprintf(stderr, "[AlphaTensor GPU] ERROR: Failed to read result buffer (%d)\n", err);
        goto cleanup;
    }

    /* Convert result from row-major (OpenCL) back to column-major (Fortran) */
    for (int i = 0; i < 4; i++) {
        for (int j = 0; j < 4; j++) {
            C[j * ldc + i] = C_flat[i * 4 + j];  /* Row-major to column-major */
        }
    }

    fprintf(stdout, "[AlphaTensor GPU] GPU computation completed successfully\n");

    /* Clean up OpenCL buffers */
cleanup:
    clReleaseMemObject(buffer_A);
    clReleaseMemObject(buffer_B);
    clReleaseMemObject(buffer_C);

    return (err == CL_SUCCESS) ? 0 : -1;
}

/*
 * Internal batched matrix multiplication using GPU
 *
 * This function will implement batched processing of multiple 4x4 matrices
 * for maximum GPU efficiency. Implementation deferred to Phase 9.2.
 */
static int alphatensor_gpu_compute_batch_internal(
    int batch_size, const double* alpha_array,
    const double* A_batch, int lda,
    const double* B_batch, int ldb,
    const double* beta_array, double* C_batch, int ldc) {

    /* Get OpenCL context */
    alphatensor_opencl_t* ctx = alphatensor_gpu_get_context();
    if (!ctx || !ctx->gpu_available) {
        fprintf(stderr, "[AlphaTensor GPU] ERROR: GPU context not available for batch\n");
        return -1;
    }

    /* Log batch operation details */
    fprintf(stdout, "[AlphaTensor GPU] Starting batch computation of %d matrices\n", batch_size);

    /*
     * PHASE 9.2 BATCH IMPLEMENTATION: ACTUAL OPENCL BATCHED KERNEL EXECUTION
     *
     * Execute the 49-operation AlphaTensor algorithm on multiple 4x4 matrices
     * simultaneously using the dgemm_alpha_4x4_batch kernel from dgemm_alpha.cl
     * This is where GPU shows maximum advantage over CPU (10-20x speedup expected).
     */

    cl_int err;
    const size_t batch_matrix_size = batch_size * 16;  /* 16 elements per 4x4 matrix */

    /* Create large OpenCL buffers for entire batch */
    cl_mem buffer_A_batch = clCreateBuffer(ctx->context, CL_MEM_READ_ONLY,
                                           batch_matrix_size * sizeof(double), NULL, &err);
    if (err != CL_SUCCESS) {
        fprintf(stderr, "[AlphaTensor GPU] ERROR: Failed to create batch buffer A (%d)\n", err);
        return -1;
    }

    cl_mem buffer_B_batch = clCreateBuffer(ctx->context, CL_MEM_READ_ONLY,
                                           batch_matrix_size * sizeof(double), NULL, &err);
    if (err != CL_SUCCESS) {
        fprintf(stderr, "[AlphaTensor GPU] ERROR: Failed to create batch buffer B (%d)\n", err);
        clReleaseMemObject(buffer_A_batch);
        return -1;
    }

    cl_mem buffer_C_batch = clCreateBuffer(ctx->context, CL_MEM_READ_WRITE,
                                           batch_matrix_size * sizeof(double), NULL, &err);
    if (err != CL_SUCCESS) {
        fprintf(stderr, "[AlphaTensor GPU] ERROR: Failed to create batch buffer C (%d)\n", err);
        clReleaseMemObject(buffer_A_batch);
        clReleaseMemObject(buffer_B_batch);
        return -1;
    }

    /* Convert entire batch from column-major to row-major layout */
    double* A_batch_flat = (double*)malloc(batch_matrix_size * sizeof(double));
    double* B_batch_flat = (double*)malloc(batch_matrix_size * sizeof(double));
    double* C_batch_flat = (double*)malloc(batch_matrix_size * sizeof(double));

    if (!A_batch_flat || !B_batch_flat || !C_batch_flat) {
        fprintf(stderr, "[AlphaTensor GPU] ERROR: Failed to allocate temporary batch memory\n");
        goto batch_cleanup;
    }

    /* Convert all matrices in batch */
    for (int batch_idx = 0; batch_idx < batch_size; batch_idx++) {
        const double* A_matrix = A_batch + batch_idx * 16;
        const double* B_matrix = B_batch + batch_idx * 16;
        const double* C_matrix = C_batch + batch_idx * 16;

        double* A_flat = A_batch_flat + batch_idx * 16;
        double* B_flat = B_batch_flat + batch_idx * 16;
        double* C_flat = C_batch_flat + batch_idx * 16;

        for (int i = 0; i < 4; i++) {
            for (int j = 0; j < 4; j++) {
                A_flat[i * 4 + j] = A_matrix[j * lda + i];
                B_flat[i * 4 + j] = B_matrix[j * ldb + i];
                C_flat[i * 4 + j] = C_matrix[j * ldc + i];
            }
        }
    }

    /* Transfer entire batch to GPU memory in single operations */
    err = clEnqueueWriteBuffer(ctx->queue, buffer_A_batch, CL_TRUE, 0,
                               batch_matrix_size * sizeof(double), A_batch_flat, 0, NULL, NULL);
    if (err != CL_SUCCESS) {
        fprintf(stderr, "[AlphaTensor GPU] ERROR: Failed to write batch buffer A (%d)\n", err);
        goto batch_cleanup;
    }

    err = clEnqueueWriteBuffer(ctx->queue, buffer_B_batch, CL_TRUE, 0,
                               batch_matrix_size * sizeof(double), B_batch_flat, 0, NULL, NULL);
    if (err != CL_SUCCESS) {
        fprintf(stderr, "[AlphaTensor GPU] ERROR: Failed to write batch buffer B (%d)\n", err);
        goto batch_cleanup;
    }

    err = clEnqueueWriteBuffer(ctx->queue, buffer_C_batch, CL_TRUE, 0,
                               batch_matrix_size * sizeof(double), C_batch_flat, 0, NULL, NULL);
    if (err != CL_SUCCESS) {
        fprintf(stderr, "[AlphaTensor GPU] ERROR: Failed to write batch buffer C (%d)\n", err);
        goto batch_cleanup;
    }

    /* Use first alpha/beta values for entire batch (simplified interface) */
    double alpha = alpha_array[0];
    double beta = beta_array[0];

    /* Set kernel arguments for dgemm_alpha_4x4_batch kernel */
    err = clSetKernelArg(ctx->kernel_batch, 0, sizeof(cl_mem), &buffer_A_batch);
    err |= clSetKernelArg(ctx->kernel_batch, 1, sizeof(cl_mem), &buffer_B_batch);
    err |= clSetKernelArg(ctx->kernel_batch, 2, sizeof(cl_mem), &buffer_C_batch);
    err |= clSetKernelArg(ctx->kernel_batch, 3, sizeof(double), &alpha);
    err |= clSetKernelArg(ctx->kernel_batch, 4, sizeof(double), &beta);
    err |= clSetKernelArg(ctx->kernel_batch, 5, sizeof(int), &batch_size);

    if (err != CL_SUCCESS) {
        fprintf(stderr, "[AlphaTensor GPU] ERROR: Failed to set batch kernel arguments (%d)\n", err);
        goto batch_cleanup;
    }

    /* Execute batched kernel - each work-item processes one 4x4 matrix */
    size_t global_work_size = batch_size;
    size_t local_work_size = (batch_size >= 64) ? 64 : batch_size;  /* Optimize for GPU occupancy */

    err = clEnqueueNDRangeKernel(ctx->queue, ctx->kernel_batch, 1, NULL,
                                 &global_work_size, &local_work_size, 0, NULL, NULL);
    if (err != CL_SUCCESS) {
        fprintf(stderr, "[AlphaTensor GPU] ERROR: Failed to execute batch kernel (%d)\n", err);
        goto batch_cleanup;
    }

    /* Wait for all batch processing to complete */
    err = clFinish(ctx->queue);
    if (err != CL_SUCCESS) {
        fprintf(stderr, "[AlphaTensor GPU] ERROR: Failed to finish batch execution (%d)\n", err);
        goto batch_cleanup;
    }

    /* Read entire batch result back in single operation */
    err = clEnqueueReadBuffer(ctx->queue, buffer_C_batch, CL_TRUE, 0,
                              batch_matrix_size * sizeof(double), C_batch_flat, 0, NULL, NULL);
    if (err != CL_SUCCESS) {
        fprintf(stderr, "[AlphaTensor GPU] ERROR: Failed to read batch result (%d)\n", err);
        goto batch_cleanup;
    }

    /* Convert all results back to column-major layout */
    for (int batch_idx = 0; batch_idx < batch_size; batch_idx++) {
        double* C_matrix = C_batch + batch_idx * 16;
        const double* C_flat = C_batch_flat + batch_idx * 16;

        for (int i = 0; i < 4; i++) {
            for (int j = 0; j < 4; j++) {
                C_matrix[j * ldc + i] = C_flat[i * 4 + j];
            }
        }
    }

    fprintf(stdout, "[AlphaTensor GPU] Batch computation completed successfully\n");

batch_cleanup:
    /* Clean up all batch resources */
    if (A_batch_flat) free(A_batch_flat);
    if (B_batch_flat) free(B_batch_flat);
    if (C_batch_flat) free(C_batch_flat);
    clReleaseMemObject(buffer_A_batch);
    clReleaseMemObject(buffer_B_batch);
    clReleaseMemObject(buffer_C_batch);

    return (err == CL_SUCCESS) ? 0 : -1;

    /* TODO: Remove this placeholder when Phase 9.2 kernels are implemented */
    return 0;
}

/*
 * =============================================================================
 * UTILITY AND DEBUGGING FUNCTIONS
 * =============================================================================
 */

/*
 * Print GPU device information for debugging
 *
 * Fortran callable function to display current GPU configuration.
 */
void alphatensor_gpu_print_info_(void) {
    alphatensor_device_info_t info;

    if (alphatensor_gpu_get_device_info(&info) == 0) {
        fprintf(stdout, "\n[AlphaTensor GPU] Device Information:\n");
        fprintf(stdout, "  Device Name: %s\n", info.device_name);
        fprintf(stdout, "  Device Type: %s\n", info.is_gpu ? "GPU" : "CPU/Other");
        fprintf(stdout, "  Global Memory: %zu MB\n", info.global_memory_size / (1024 * 1024));
        fprintf(stdout, "  Local Memory: %zu KB\n", info.local_memory_size / 1024);
        fprintf(stdout, "  Compute Units: %u\n", info.compute_units);
        fprintf(stdout, "  Max Work Group Size: %zu\n", info.max_work_group_size);
        fprintf(stdout, "\n");
    } else {
        fprintf(stdout, "[AlphaTensor GPU] No GPU device available\n");
    }
}

/*
 * Test GPU context initialization
 *
 * Utility function for validating GPU setup during development.
 */
int alphatensor_gpu_test_context_(void) {
    fprintf(stdout, "[AlphaTensor GPU] Testing GPU context initialization...\n");

    /* Test initialization */
    if (alphatensor_gpu_global_init() != 0) {
        fprintf(stderr, "[AlphaTensor GPU] GPU initialization failed\n");
        return -1;
    }

    /* Test availability */
    if (!alphatensor_gpu_is_available()) {
        fprintf(stderr, "[AlphaTensor GPU] GPU not available after initialization\n");
        return -1;
    }

    /* Print device info */
    alphatensor_gpu_print_info_();

    fprintf(stdout, "[AlphaTensor GPU] GPU context test completed successfully\n");
    return 0;
}
