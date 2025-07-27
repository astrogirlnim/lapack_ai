/*
 * OpenCL Manager for AlphaTensor Hybrid Implementation
 *
 * This module provides OpenCL infrastructure for GPU-accelerated AlphaTensor
 * matrix multiplication, following LAPACK VARIANTS pattern with CPU fallback.
 *
 * Author: LAPACK AI Development Team
 * Based on: AlphaTensor implementation plan Phase 9.1
 * Reference: CBLAS interface patterns for C-Fortran integration
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef __APPLE__
#include <OpenCL/opencl.h>
#else
#include <CL/cl.h>
#endif

#include "opencl_manager.h"

/* Global OpenCL context - managed as singleton for efficiency */
static alphatensor_opencl_t g_opencl_ctx = {0};
static int g_opencl_initialized = 0;

/* Error handling macros for comprehensive debugging */
#define OPENCL_CHECK_ERROR(err, msg) \
    do { \
        if ((err) != CL_SUCCESS) { \
            fprintf(stderr, "[AlphaTensor OpenCL] ERROR: %s (error code: %d)\n", (msg), (err)); \
            return -1; \
        } \
    } while(0)

#define OPENCL_LOG_INFO(msg, ...) \
    fprintf(stdout, "[AlphaTensor OpenCL] INFO: " msg "\n", ##__VA_ARGS__)

#define OPENCL_LOG_WARNING(msg, ...) \
    fprintf(stderr, "[AlphaTensor OpenCL] WARNING: " msg "\n", ##__VA_ARGS__)

/* Platform and device enumeration */
static const char* get_device_type_string(cl_device_type type) {
    switch(type) {
        case CL_DEVICE_TYPE_GPU: return "GPU";
        case CL_DEVICE_TYPE_CPU: return "CPU";
        case CL_DEVICE_TYPE_ACCELERATOR: return "ACCELERATOR";
        case CL_DEVICE_TYPE_DEFAULT: return "DEFAULT";
        default: return "UNKNOWN";
    }
}

static const char* get_opencl_error_string(cl_int error) {
    switch(error) {
        case CL_SUCCESS: return "Success";
        case CL_DEVICE_NOT_FOUND: return "Device not found";
        case CL_DEVICE_NOT_AVAILABLE: return "Device not available";
        case CL_COMPILER_NOT_AVAILABLE: return "Compiler not available";
        case CL_MEM_OBJECT_ALLOCATION_FAILURE: return "Memory object allocation failure";
        case CL_OUT_OF_RESOURCES: return "Out of resources";
        case CL_OUT_OF_HOST_MEMORY: return "Out of host memory";
        case CL_PROFILING_INFO_NOT_AVAILABLE: return "Profiling information not available";
        case CL_MEM_COPY_OVERLAP: return "Memory copy overlap";
        case CL_IMAGE_FORMAT_MISMATCH: return "Image format mismatch";
        case CL_IMAGE_FORMAT_NOT_SUPPORTED: return "Image format not supported";
        case CL_BUILD_PROGRAM_FAILURE: return "Build program failure";
        case CL_MAP_FAILURE: return "Map failure";
        case CL_INVALID_VALUE: return "Invalid value";
        case CL_INVALID_DEVICE_TYPE: return "Invalid device type";
        case CL_INVALID_PLATFORM: return "Invalid platform";
        case CL_INVALID_DEVICE: return "Invalid device";
        case CL_INVALID_CONTEXT: return "Invalid context";
        case CL_INVALID_QUEUE_PROPERTIES: return "Invalid queue properties";
        case CL_INVALID_COMMAND_QUEUE: return "Invalid command queue";
        case CL_INVALID_HOST_PTR: return "Invalid host ptr";
        case CL_INVALID_MEM_OBJECT: return "Invalid memory object";
        case CL_INVALID_IMAGE_FORMAT_DESCRIPTOR: return "Invalid image format descriptor";
        case CL_INVALID_IMAGE_SIZE: return "Invalid image size";
        case CL_INVALID_SAMPLER: return "Invalid sampler";
        case CL_INVALID_BINARY: return "Invalid binary";
        case CL_INVALID_BUILD_OPTIONS: return "Invalid build options";
        case CL_INVALID_PROGRAM: return "Invalid program";
        case CL_INVALID_PROGRAM_EXECUTABLE: return "Invalid program executable";
        case CL_INVALID_KERNEL_NAME: return "Invalid kernel name";
        case CL_INVALID_KERNEL_DEFINITION: return "Invalid kernel definition";
        case CL_INVALID_KERNEL: return "Invalid kernel";
        case CL_INVALID_ARG_INDEX: return "Invalid argument index";
        case CL_INVALID_ARG_VALUE: return "Invalid argument value";
        case CL_INVALID_ARG_SIZE: return "Invalid argument size";
        case CL_INVALID_KERNEL_ARGS: return "Invalid kernel arguments";
        case CL_INVALID_WORK_DIMENSION: return "Invalid work dimension";
        case CL_INVALID_WORK_GROUP_SIZE: return "Invalid work group size";
        case CL_INVALID_WORK_ITEM_SIZE: return "Invalid work item size";
        case CL_INVALID_GLOBAL_OFFSET: return "Invalid global offset";
        case CL_INVALID_EVENT_WAIT_LIST: return "Invalid event wait list";
        case CL_INVALID_EVENT: return "Invalid event";
        case CL_INVALID_OPERATION: return "Invalid operation";
        case CL_INVALID_GL_OBJECT: return "Invalid OpenGL object";
        case CL_INVALID_BUFFER_SIZE: return "Invalid buffer size";
        case CL_INVALID_MIP_LEVEL: return "Invalid mip level";
        case CL_INVALID_GLOBAL_WORK_SIZE: return "Invalid global work size";
        default: return "Unknown error";
    }
}

/* Device selection strategy - prefer discrete GPUs, then integrated GPUs, then CPU */
static int select_best_device(cl_platform_id platform, cl_device_id* out_device) {
    cl_uint num_devices = 0;
    cl_int err;

    OPENCL_LOG_INFO("Enumerating devices for platform");

    /* Get device count */
    err = clGetDeviceIDs(platform, CL_DEVICE_TYPE_ALL, 0, NULL, &num_devices);
    if (err != CL_SUCCESS || num_devices == 0) {
        OPENCL_LOG_WARNING("No devices found on platform");
        return -1;
    }

    OPENCL_LOG_INFO("Found %d devices", num_devices);

    /* Get all devices */
    cl_device_id* devices = (cl_device_id*)malloc(num_devices * sizeof(cl_device_id));
    if (!devices) {
        OPENCL_LOG_WARNING("Failed to allocate memory for device list");
        return -1;
    }

    err = clGetDeviceIDs(platform, CL_DEVICE_TYPE_ALL, num_devices, devices, NULL);
    if (err != CL_SUCCESS) {
        OPENCL_LOG_WARNING("Failed to get device IDs: %s", get_opencl_error_string(err));
        free(devices);
        return -1;
    }

    /* Selection priority: Discrete GPU > Integrated GPU > CPU */
    cl_device_id best_device = NULL;
    int best_score = -1;

    for (cl_uint i = 0; i < num_devices; i++) {
        cl_device_type type;
        char device_name[256];
        cl_ulong global_mem_size;
        cl_uint compute_units;

        /* Get device properties */
        clGetDeviceInfo(devices[i], CL_DEVICE_TYPE, sizeof(type), &type, NULL);
        clGetDeviceInfo(devices[i], CL_DEVICE_NAME, sizeof(device_name), device_name, NULL);
        clGetDeviceInfo(devices[i], CL_DEVICE_GLOBAL_MEM_SIZE, sizeof(global_mem_size), &global_mem_size, NULL);
        clGetDeviceInfo(devices[i], CL_DEVICE_MAX_COMPUTE_UNITS, sizeof(compute_units), &compute_units, NULL);

        OPENCL_LOG_INFO("Device %d: %s [%s] - Memory: %llu MB, Compute Units: %u",
                       i, device_name, get_device_type_string(type),
                       global_mem_size / (1024 * 1024), compute_units);

        /* Score devices by preference */
        int score = 0;
        if (type == CL_DEVICE_TYPE_GPU) {
            score = 100; /* Prefer GPUs */
        } else if (type == CL_DEVICE_TYPE_CPU) {
            score = 10;  /* CPU as fallback */
        }

        /* Bonus for more compute units and memory */
        score += compute_units;
        if (global_mem_size > 1024 * 1024 * 1024) score += 10; /* Bonus for >1GB memory */

        if (score > best_score) {
            best_score = score;
            best_device = devices[i];
        }
    }

    if (best_device) {
        *out_device = best_device;
        char selected_name[256];
        clGetDeviceInfo(best_device, CL_DEVICE_NAME, sizeof(selected_name), selected_name, NULL);
        OPENCL_LOG_INFO("Selected device: %s (score: %d)", selected_name, best_score);
        free(devices);
        return 0;
    }

    free(devices);
    OPENCL_LOG_WARNING("No suitable device found");
    return -1;
}

/* Platform enumeration and selection */
static int select_best_platform(cl_platform_id* out_platform) {
    cl_uint num_platforms = 0;
    cl_int err;

    OPENCL_LOG_INFO("Enumerating OpenCL platforms");

    /* Get platform count */
    err = clGetPlatformIDs(0, NULL, &num_platforms);
    OPENCL_CHECK_ERROR(err, "Failed to get platform count");

    if (num_platforms == 0) {
        OPENCL_LOG_WARNING("No OpenCL platforms found");
        return -1;
    }

    OPENCL_LOG_INFO("Found %d OpenCL platforms", num_platforms);

    /* Get all platforms */
    cl_platform_id* platforms = (cl_platform_id*)malloc(num_platforms * sizeof(cl_platform_id));
    if (!platforms) {
        OPENCL_LOG_WARNING("Failed to allocate memory for platform list");
        return -1;
    }

    err = clGetPlatformIDs(num_platforms, platforms, NULL);
    if (err != CL_SUCCESS) {
        OPENCL_LOG_WARNING("Failed to get platform IDs: %s", get_opencl_error_string(err));
        free(platforms);
        return -1;
    }

    /* Select first platform with suitable devices */
    for (cl_uint i = 0; i < num_platforms; i++) {
        char platform_name[256];
        char platform_vendor[256];
        char platform_version[256];

        clGetPlatformInfo(platforms[i], CL_PLATFORM_NAME, sizeof(platform_name), platform_name, NULL);
        clGetPlatformInfo(platforms[i], CL_PLATFORM_VENDOR, sizeof(platform_vendor), platform_vendor, NULL);
        clGetPlatformInfo(platforms[i], CL_PLATFORM_VERSION, sizeof(platform_version), platform_version, NULL);

        OPENCL_LOG_INFO("Platform %d: %s (%s) - %s", i, platform_name, platform_vendor, platform_version);

        /* Test if platform has suitable devices */
        cl_device_id test_device;
        if (select_best_device(platforms[i], &test_device) == 0) {
            *out_platform = platforms[i];
            OPENCL_LOG_INFO("Selected platform: %s", platform_name);
            free(platforms);
            return 0;
        }
    }

    free(platforms);
    OPENCL_LOG_WARNING("No platform with suitable devices found");
    return -1;
}

/* Main initialization function */
int alphatensor_gpu_init(alphatensor_opencl_t* ctx) {
    cl_int err;

    if (!ctx) {
        OPENCL_LOG_WARNING("Invalid context pointer");
        return -1;
    }

    /* Clear context */
    memset(ctx, 0, sizeof(alphatensor_opencl_t));

    OPENCL_LOG_INFO("Initializing AlphaTensor OpenCL context");

    /* Select platform */
    if (select_best_platform(&ctx->platform) != 0) {
        OPENCL_LOG_WARNING("Failed to select OpenCL platform");
        ctx->gpu_available = 0;
        return -1;
    }

    /* Select device */
    if (select_best_device(ctx->platform, &ctx->device) != 0) {
        OPENCL_LOG_WARNING("Failed to select OpenCL device");
        ctx->gpu_available = 0;
        return -1;
    }

    /* Create context */
    ctx->context = clCreateContext(NULL, 1, &ctx->device, NULL, NULL, &err);
    OPENCL_CHECK_ERROR(err, "Failed to create OpenCL context");

    /* Create command queue */
    ctx->queue = clCreateCommandQueue(ctx->context, ctx->device, 0, &err);
    OPENCL_CHECK_ERROR(err, "Failed to create command queue");

    /*
     * PHASE 9.2: COMPILE AND LOAD ALPHATENSOR OPENCL KERNELS
     *
     * Load the dgemm_alpha.cl file and compile the kernels for
     * single 4x4 matrix and batched operations.
     */

    /* Read kernel source from dgemm_alpha.cl file */
    FILE* kernel_file = fopen("SRC/VARIANTS/alphatensor_hybrid/dgemm_alpha.cl", "r");
    if (!kernel_file) {
        /* Try relative path from different working directories */
        kernel_file = fopen("../SRC/VARIANTS/alphatensor_hybrid/dgemm_alpha.cl", "r");
        if (!kernel_file) {
            kernel_file = fopen("dgemm_alpha.cl", "r");
        }
    }

    if (!kernel_file) {
        OPENCL_LOG_WARNING("Failed to open dgemm_alpha.cl kernel file");
        return -1;
    }

    /* Get file size */
    fseek(kernel_file, 0, SEEK_END);
    size_t kernel_source_size = ftell(kernel_file);
    fseek(kernel_file, 0, SEEK_SET);

    /* Read entire file */
    char* kernel_source = (char*)malloc(kernel_source_size + 1);
    if (!kernel_source) {
        OPENCL_LOG_WARNING("Failed to allocate memory for kernel source");
        fclose(kernel_file);
        return -1;
    }

    size_t bytes_read = fread(kernel_source, 1, kernel_source_size, kernel_file);
    kernel_source[bytes_read] = '\0';
    fclose(kernel_file);

    OPENCL_LOG_INFO("Loaded %zu bytes of kernel source from dgemm_alpha.cl", bytes_read);

    /* Create and compile program */
    ctx->program = clCreateProgramWithSource(ctx->context, 1,
                                              (const char**)&kernel_source,
                                              &kernel_source_size, &err);
    free(kernel_source);

    if (err != CL_SUCCESS) {
        OPENCL_LOG_WARNING("Failed to create program: %s", get_opencl_error_string(err));
        return -1;
    }

    /* Build program with optimization flags */
    const char* build_options = "-cl-mad-enable -cl-fast-relaxed-math -cl-std=CL1.2";
    err = clBuildProgram(ctx->program, 1, &ctx->device, build_options, NULL, NULL);

    if (err != CL_SUCCESS) {
        /* Get build log for debugging */
        size_t log_size;
        clGetProgramBuildInfo(ctx->program, ctx->device, CL_PROGRAM_BUILD_LOG,
                             0, NULL, &log_size);

        if (log_size > 0) {
            char* build_log = (char*)malloc(log_size + 1);
            if (build_log) {
                clGetProgramBuildInfo(ctx->program, ctx->device, CL_PROGRAM_BUILD_LOG,
                                     log_size, build_log, NULL);
                build_log[log_size] = '\0';
                OPENCL_LOG_WARNING("Kernel build failed:\n%s", build_log);
                free(build_log);
            }
        }

        OPENCL_LOG_WARNING("Failed to build program: %s", get_opencl_error_string(err));
        return -1;
    }

    OPENCL_LOG_INFO("Successfully compiled AlphaTensor kernels");

    /* Create kernels from compiled program */
    ctx->kernel_4x4 = clCreateKernel(ctx->program, "dgemm_alpha_4x4", &err);
    if (err != CL_SUCCESS) {
        OPENCL_LOG_WARNING("Failed to create single 4x4 kernel: %s", get_opencl_error_string(err));
        return -1;
    }

    ctx->kernel_batch = clCreateKernel(ctx->program, "dgemm_alpha_4x4_batch", &err);
    if (err != CL_SUCCESS) {
        OPENCL_LOG_WARNING("Failed to create batch kernel: %s", get_opencl_error_string(err));
        return -1;
    }

    OPENCL_LOG_INFO("Successfully created AlphaTensor kernels:");
    OPENCL_LOG_INFO("  - dgemm_alpha_4x4 (single matrix)");
    OPENCL_LOG_INFO("  - dgemm_alpha_4x4_batch (batched processing)");

    /* Mark as initialized */
    ctx->initialized = 1;
    ctx->gpu_available = 1;

    OPENCL_LOG_INFO("OpenCL context successfully initialized with AlphaTensor kernels");
    return 0;
}

/* Cleanup function */
int alphatensor_gpu_cleanup(alphatensor_opencl_t* ctx) {
    if (!ctx || !ctx->initialized) {
        return 0;
    }

    OPENCL_LOG_INFO("Cleaning up OpenCL context");

    /* Release kernels */
    if (ctx->kernel_4x4) {
        clReleaseKernel(ctx->kernel_4x4);
        ctx->kernel_4x4 = NULL;
    }

    if (ctx->kernel_batch) {
        clReleaseKernel(ctx->kernel_batch);
        ctx->kernel_batch = NULL;
    }

    /* Release program */
    if (ctx->program) {
        clReleaseProgram(ctx->program);
        ctx->program = NULL;
    }

    /* Release command queue */
    if (ctx->queue) {
        clReleaseCommandQueue(ctx->queue);
        ctx->queue = NULL;
    }

    /* Release context */
    if (ctx->context) {
        clReleaseContext(ctx->context);
        ctx->context = NULL;
    }

    /* Clear flags */
    ctx->initialized = 0;
    ctx->gpu_available = 0;

    OPENCL_LOG_INFO("OpenCL context cleaned up");
    return 0;
}

/* Global context management functions */
int alphatensor_gpu_global_init(void) {
    if (g_opencl_initialized) {
        return 0; /* Already initialized */
    }

    if (alphatensor_gpu_init(&g_opencl_ctx) == 0) {
        g_opencl_initialized = 1;
        return 0;
    }

    return -1;
}

void alphatensor_gpu_global_cleanup(void) {
    if (g_opencl_initialized) {
        alphatensor_gpu_cleanup(&g_opencl_ctx);
        g_opencl_initialized = 0;
    }
}

alphatensor_opencl_t* alphatensor_gpu_get_context(void) {
    if (!g_opencl_initialized) {
        alphatensor_gpu_global_init();
    }

    return g_opencl_initialized ? &g_opencl_ctx : NULL;
}

/* Availability check function */
int alphatensor_gpu_is_available(void) {
    if (!g_opencl_initialized) {
        alphatensor_gpu_global_init();
    }

    return g_opencl_initialized && g_opencl_ctx.gpu_available;
}

/* Device capability queries */
int alphatensor_gpu_get_device_info(alphatensor_device_info_t* info) {
    if (!info) return -1;

    alphatensor_opencl_t* ctx = alphatensor_gpu_get_context();
    if (!ctx || !ctx->gpu_available) return -1;

    cl_int err;

    /* Get device name */
    err = clGetDeviceInfo(ctx->device, CL_DEVICE_NAME,
                         sizeof(info->device_name), info->device_name, NULL);
    if (err != CL_SUCCESS) return -1;

    /* Get device type */
    cl_device_type type;
    err = clGetDeviceInfo(ctx->device, CL_DEVICE_TYPE, sizeof(type), &type, NULL);
    if (err != CL_SUCCESS) return -1;
    info->is_gpu = (type == CL_DEVICE_TYPE_GPU);

    /* Get memory info */
    err = clGetDeviceInfo(ctx->device, CL_DEVICE_GLOBAL_MEM_SIZE,
                         sizeof(info->global_memory_size), &info->global_memory_size, NULL);
    if (err != CL_SUCCESS) return -1;

    err = clGetDeviceInfo(ctx->device, CL_DEVICE_LOCAL_MEM_SIZE,
                         sizeof(info->local_memory_size), &info->local_memory_size, NULL);
    if (err != CL_SUCCESS) return -1;

    /* Get compute info */
    err = clGetDeviceInfo(ctx->device, CL_DEVICE_MAX_COMPUTE_UNITS,
                         sizeof(info->compute_units), &info->compute_units, NULL);
    if (err != CL_SUCCESS) return -1;

    err = clGetDeviceInfo(ctx->device, CL_DEVICE_MAX_WORK_GROUP_SIZE,
                         sizeof(info->max_work_group_size), &info->max_work_group_size, NULL);
    if (err != CL_SUCCESS) return -1;

    return 0;
}

/* Buffer management utilities */
cl_mem alphatensor_gpu_create_buffer(size_t size, cl_mem_flags flags) {
    alphatensor_opencl_t* ctx = alphatensor_gpu_get_context();
    if (!ctx || !ctx->gpu_available) return NULL;

    cl_int err;
    cl_mem buffer = clCreateBuffer(ctx->context, flags, size, NULL, &err);
    if (err != CL_SUCCESS) {
        OPENCL_LOG_WARNING("Failed to create buffer of size %zu: %s",
                          size, get_opencl_error_string(err));
        return NULL;
    }

    return buffer;
}

int alphatensor_gpu_release_buffer(cl_mem buffer) {
    if (buffer) {
        cl_int err = clReleaseMemObject(buffer);
        if (err != CL_SUCCESS) {
            OPENCL_LOG_WARNING("Failed to release buffer: %s", get_opencl_error_string(err));
            return -1;
        }
    }
    return 0;
}
