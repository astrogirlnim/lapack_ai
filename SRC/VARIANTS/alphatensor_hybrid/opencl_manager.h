/*
 * OpenCL Manager Header for AlphaTensor Hybrid Implementation
 *
 * This header defines the interface for OpenCL infrastructure supporting
 * GPU-accelerated AlphaTensor matrix multiplication in LAPACK VARIANTS.
 *
 * Author: LAPACK AI Development Team
 * Based on: AlphaTensor implementation plan Phase 9.1
 */

#ifndef ALPHATENSOR_OPENCL_MANAGER_H
#define ALPHATENSOR_OPENCL_MANAGER_H

#ifdef __cplusplus
extern "C" {
#endif

/* Include OpenCL headers with platform compatibility */
#ifdef __APPLE__
#include <OpenCL/opencl.h>
#else
#include <CL/cl.h>
#endif

#include <stddef.h>

/* Forward declarations */
typedef struct alphatensor_opencl_t alphatensor_opencl_t;
typedef struct alphatensor_device_info_t alphatensor_device_info_t;

/*
 * Main OpenCL context structure for AlphaTensor operations
 *
 * This structure encapsulates all OpenCL resources needed for
 * GPU-accelerated matrix multiplication operations.
 */
struct alphatensor_opencl_t {
    /* OpenCL runtime objects */
    cl_platform_id platform;           /* Selected OpenCL platform */
    cl_device_id device;               /* Selected compute device */
    cl_context context;                /* OpenCL execution context */
    cl_command_queue queue;            /* Command queue for operations */

    /* Compiled kernels */
    cl_program program;                /* Compiled OpenCL program */
    cl_kernel kernel_4x4;              /* Single 4x4 matrix kernel */
    cl_kernel kernel_batch;            /* Batched operations kernel */

    /* Status flags */
    int initialized;                   /* Context initialization flag */
    int gpu_available;                 /* GPU availability flag */
};

/*
 * Device information structure for capability queries
 *
 * Contains essential device properties for optimization
 * and compatibility decisions.
 */
struct alphatensor_device_info_t {
    char device_name[256];             /* Device name string */
    int is_gpu;                        /* 1 if GPU, 0 if CPU/other */
    size_t global_memory_size;         /* Global memory in bytes */
    size_t local_memory_size;          /* Local/shared memory in bytes */
    unsigned int compute_units;        /* Number of compute units */
    size_t max_work_group_size;        /* Maximum work group size */
};

/*
 * =============================================================================
 * CONTEXT MANAGEMENT FUNCTIONS
 * =============================================================================
 */

/*
 * Initialize OpenCL context with automatic platform/device selection
 *
 * @param ctx: Pointer to context structure to initialize
 * @return: 0 on success, -1 on failure
 *
 * This function:
 * - Enumerates available OpenCL platforms
 * - Selects best available device (GPU preferred)
 * - Creates OpenCL context and command queue
 * - Sets up initial state for GPU operations
 */
int alphatensor_gpu_init(alphatensor_opencl_t* ctx);

/*
 * Clean up OpenCL context and release all resources
 *
 * @param ctx: Pointer to context structure to clean up
 * @return: 0 on success, -1 on failure
 *
 * This function releases all OpenCL objects in proper order:
 * kernels -> program -> command queue -> context
 */
int alphatensor_gpu_cleanup(alphatensor_opencl_t* ctx);

/*
 * =============================================================================
 * GLOBAL CONTEXT MANAGEMENT (SINGLETON PATTERN)
 * =============================================================================
 */

/*
 * Initialize global OpenCL context (singleton)
 *
 * @return: 0 on success, -1 on failure
 *
 * Manages a single global context for efficiency.
 * Safe to call multiple times - only initializes once.
 */
int alphatensor_gpu_global_init(void);

/*
 * Clean up global OpenCL context
 *
 * Should be called at program termination to properly
 * release OpenCL resources.
 */
void alphatensor_gpu_global_cleanup(void);

/*
 * Get pointer to global OpenCL context
 *
 * @return: Pointer to global context, or NULL if not available
 *
 * Automatically initializes context if not already done.
 */
alphatensor_opencl_t* alphatensor_gpu_get_context(void);

/*
 * =============================================================================
 * AVAILABILITY AND CAPABILITY FUNCTIONS
 * =============================================================================
 */

/*
 * Check if GPU acceleration is available
 *
 * @return: 1 if GPU available, 0 if not available
 *
 * This function can be called safely without initialization.
 * It will attempt initialization if needed.
 */
int alphatensor_gpu_is_available(void);

/*
 * Get detailed device information for optimization decisions
 *
 * @param info: Pointer to device info structure to fill
 * @return: 0 on success, -1 on failure
 *
 * Provides device capabilities for making informed decisions
 * about algorithm selection and work group sizing.
 */
int alphatensor_gpu_get_device_info(alphatensor_device_info_t* info);

/*
 * =============================================================================
 * MEMORY MANAGEMENT UTILITIES
 * =============================================================================
 */

/*
 * Create OpenCL buffer with specified size and flags
 *
 * @param size: Buffer size in bytes
 * @param flags: OpenCL memory flags (CL_MEM_READ_ONLY, etc.)
 * @return: OpenCL buffer object, or NULL on failure
 *
 * Wrapper for clCreateBuffer with error handling and logging.
 */
cl_mem alphatensor_gpu_create_buffer(size_t size, cl_mem_flags flags);

/*
 * Release OpenCL buffer object
 *
 * @param buffer: Buffer to release
 * @return: 0 on success, -1 on failure
 *
 * Safe wrapper for clReleaseMemObject with error handling.
 */
int alphatensor_gpu_release_buffer(cl_mem buffer);

/*
 * =============================================================================
 * FORTRAN INTERFACE COMPATIBILITY
 * =============================================================================
 */

/*
 * C functions callable from Fortran (following CBLAS patterns)
 *
 * These functions will be implemented in gpu_interface.c
 * and provide the bridge between Fortran code and OpenCL.
 */

/* GPU availability check for Fortran */
extern int alphatensor_gpu_available_(void);

/* GPU initialization for Fortran */
extern int alphatensor_gpu_init_fortran_(void);

/* GPU cleanup for Fortran */
extern void alphatensor_gpu_cleanup_fortran_(void);

/*
 * =============================================================================
 * CONFIGURATION AND CONSTANTS
 * =============================================================================
 */

/* OpenCL device selection preferences */
#define ALPHATENSOR_PREFER_GPU          1
#define ALPHATENSOR_PREFER_CPU          0
#define ALPHATENSOR_MIN_GLOBAL_MEM_MB   256
#define ALPHATENSOR_MIN_COMPUTE_UNITS   4

/* Buffer management constants */
#define ALPHATENSOR_MATRIX_4X4_SIZE     (4 * 4 * sizeof(double))
#define ALPHATENSOR_MAX_BATCH_SIZE      1024
#define ALPHATENSOR_BUFFER_ALIGNMENT    64

/* Logging levels */
#define OPENCL_LOG_LEVEL_ERROR          0
#define OPENCL_LOG_LEVEL_WARNING        1
#define OPENCL_LOG_LEVEL_INFO           2
#define OPENCL_LOG_LEVEL_DEBUG          3

#ifdef __cplusplus
}
#endif

#endif /* ALPHATENSOR_OPENCL_MANAGER_H */
