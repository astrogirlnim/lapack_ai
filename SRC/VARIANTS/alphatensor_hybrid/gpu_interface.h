/*
 * GPU Interface Header for AlphaTensor Hybrid Implementation
 *
 * This header defines the C-Fortran interface for GPU-accelerated AlphaTensor
 * matrix multiplication operations, following CBLAS patterns for compatibility.
 *
 * Author: LAPACK AI Development Team
 * Based on: CBLAS interface patterns and Phase 9.1 implementation plan
 */

#ifndef ALPHATENSOR_GPU_INTERFACE_H
#define ALPHATENSOR_GPU_INTERFACE_H

#ifdef __cplusplus
extern "C" {
#endif

/*
 * =============================================================================
 * FORTRAN INTERFACE DECLARATIONS
 * =============================================================================
 *
 * These functions are callable from Fortran using the standard underscore
 * naming convention. They follow CBLAS patterns for parameter handling.
 */

/*
 * GPU availability check for Fortran
 *
 * Returns: 1 if GPU available, 0 if not available
 *
 * This function can be called safely from Fortran to determine
 * whether GPU acceleration is available for AlphaTensor operations.
 */
extern int alphatensor_gpu_available_(void);

/*
 * Initialize GPU context for Fortran
 *
 * Returns: 0 on success, -1 on failure
 *
 * Initializes the global OpenCL context for GPU operations.
 * This function is idempotent and safe to call multiple times.
 */
extern int alphatensor_gpu_init_fortran_(void);

/*
 * Clean up GPU resources for Fortran
 *
 * Releases all GPU resources and cleans up the OpenCL context.
 * Should be called during program termination for proper cleanup.
 */
extern void alphatensor_gpu_cleanup_fortran_(void);

/*
 * Main GPU matrix multiplication interface for Fortran
 *
 * Performs C = alpha * A * B + beta * C for 4x4 matrices using
 * GPU-accelerated AlphaTensor algorithm with 49 operations.
 *
 * Parameters:
 *   alpha: Scaling factor for A*B product
 *   A: Input matrix A (4x4, column-major)
 *   lda: Leading dimension of A (must be >= 4)
 *   B: Input matrix B (4x4, column-major)
 *   ldb: Leading dimension of B (must be >= 4)
 *   beta: Scaling factor for C matrix
 *   C: Input/output matrix C (4x4, column-major)
 *   ldc: Leading dimension of C (must be >= 4)
 *
 * Returns: 0 on success, -1 on failure
 *
 * Note: This function assumes 4x4 matrices. For other sizes,
 * the caller should use standard DGEMM fallback.
 */
extern int dgemm_alpha_gpu_(
    const double* alpha, const double* A, const int* lda,
    const double* B, const int* ldb, const double* beta,
    double* C, const int* ldc);

/*
 * Batched GPU matrix multiplication interface for Fortran
 *
 * Performs multiple 4x4 matrix multiplications in a single GPU call
 * for maximum efficiency. Ideal for ML workloads with many small matrices.
 *
 * Parameters:
 *   batch_size: Number of matrix operations (max ALPHATENSOR_MAX_BATCH_SIZE)
 *   alpha_array: Array of scaling factors for each A*B (size: batch_size)
 *   A_batch: Batch of input matrices A (size: 16 * batch_size)
 *   lda: Leading dimension for all A matrices
 *   B_batch: Batch of input matrices B (size: 16 * batch_size)
 *   ldb: Leading dimension for all B matrices
 *   beta_array: Array of scaling factors for each C (size: batch_size)
 *   C_batch: Batch of input/output matrices C (size: 16 * batch_size)
 *   ldc: Leading dimension for all C matrices
 *
 * Returns: 0 on success, -1 on failure
 *
 * Expected GPU speedup: 10-20x over sequential CPU operations
 * for batch sizes >= 100 matrices.
 */
extern int dgemm_alpha_gpu_batch_(
    const int* batch_size, const double* alpha_array,
    const double* A_batch, const int* lda,
    const double* B_batch, const int* ldb,
    const double* beta_array, double* C_batch, const int* ldc);

/*
 * =============================================================================
 * UTILITY AND DEBUGGING FUNCTIONS
 * =============================================================================
 */

/*
 * Print GPU device information for debugging
 *
 * Displays detailed information about the selected GPU device,
 * including name, memory, compute units, and capabilities.
 * Useful for development and troubleshooting.
 */
extern void alphatensor_gpu_print_info_(void);

/*
 * Test GPU context initialization
 *
 * Comprehensive test of GPU setup and initialization.
 * Returns: 0 on success, -1 on failure
 *
 * This function validates:
 * - OpenCL platform detection
 * - Device selection and context creation
 * - Basic GPU functionality
 *
 * Useful for validating GPU setup during development.
 */
extern int alphatensor_gpu_test_context_(void);

/*
 * =============================================================================
 * IMPLEMENTATION STATUS AND PHASE INFORMATION
 * =============================================================================
 */

/*
 * PHASE 9.1 STATUS: INFRASTRUCTURE COMPLETE
 *
 * The following components are implemented and functional:
 * - OpenCL context management and device selection
 * - C-Fortran interface following CBLAS patterns
 * - Parameter validation and error handling
 * - Comprehensive logging and debugging support
 * - Build system integration framework
 *
 * PHASE 9.2 REQUIREMENTS: KERNEL IMPLEMENTATION
 *
 * The following components need to be implemented in Phase 9.2:
 * - OpenCL kernel source code for 49 AlphaTensor operations
 * - GPU memory management and buffer allocation
 * - Host-device data transfer optimization
 * - Actual GPU computation in alphatensor_gpu_compute_4x4_internal()
 * - Batched processing implementation
 *
 * CURRENT BEHAVIOR:
 *
 * In Phase 9.1, the GPU interface functions perform:
 * - Complete parameter validation
 * - OpenCL context verification
 * - Detailed logging of operations
 * - Success return codes (no actual computation)
 *
 * This allows the hybrid variant to be built, tested, and integrated
 * with the LAPACK VARIANTS system before GPU kernels are implemented.
 *
 * INTEGRATION STRATEGY:
 *
 * The Fortran DGEMM_ALPHA_HYBRID routine can call these functions
 * and gracefully fall back to CPU implementation when:
 * - GPU is not available (alphatensor_gpu_available_() returns 0)
 * - GPU computation fails (return code != 0)
 *
 * This ensures backward compatibility and robust operation across
 * all systems, whether GPU-enabled or CPU-only.
 */

#ifdef __cplusplus
}
#endif

#endif /* ALPHATENSOR_GPU_INTERFACE_H */
