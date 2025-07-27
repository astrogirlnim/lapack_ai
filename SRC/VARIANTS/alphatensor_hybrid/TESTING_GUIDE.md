# Phase 9.2 OpenCL AlphaTensor Testing Guide

This guide provides comprehensive testing instructions for the Phase 9.2 OpenCL AlphaTensor implementation.

## Quick Start Testing

### 1. **Automated Testing (Recommended)**

```bash
# Navigate to the test directory
cd SRC/VARIANTS/alphatensor_hybrid

# Make the test script executable
chmod +x test_phase_9_2.sh

# Run comprehensive test suite
./test_phase_9_2.sh
```

This script will:
- ✅ Validate your OpenCL environment
- ✅ Test compilation of all components  
- ✅ Verify OpenCL functionality
- ✅ Run accuracy tests comparing GPU vs CPU
- ✅ Measure performance baselines
- ✅ Provide detailed diagnostic information

### 2. **Manual Step-by-Step Testing**

If you prefer manual control or the automated script fails:

#### Step 1: Environment Check
```bash
# Check OpenCL availability
# macOS:
ls -la /System/Library/Frameworks/OpenCL.framework

# Linux:
clinfo

# Check compilers
gfortran --version
gcc --version
```

#### Step 2: Component Compilation
```bash
# Compile OpenCL components
gcc -c opencl_manager.c -I. -framework OpenCL  # macOS
gcc -c opencl_manager.c -I. -lOpenCL           # Linux

gcc -c gpu_interface.c -I. -framework OpenCL   # macOS  
gcc -c gpu_interface.c -I. -lOpenCL            # Linux

# Compile CPU reference implementation
gfortran -c ../alphatensor/dgemm_alpha.f -o dgemm_alpha_cpu.o

# Compile test program
gfortran test_phase_9_2.f dgemm_alpha_cpu.o gpu_interface.o opencl_manager.o \
         -framework OpenCL -o test_phase_9_2  # macOS
gfortran test_phase_9_2.f dgemm_alpha_cpu.o gpu_interface.o opencl_manager.o \
         -lOpenCL -o test_phase_9_2           # Linux
```

#### Step 3: Run Tests
```bash
# Execute the comprehensive test
./test_phase_9_2
```

## Expected Test Results

### ✅ **Successful Test Output**
```
=======================================================
PHASE 9.2 OPENCL ALPHATENSOR COMPREHENSIVE TEST SUITE
=======================================================

TEST 1: OpenCL Environment Validation
======================================
[PASS] GPU detected and available
[PASS] GPU context initialized successfully

TEST 2: Single 4x4 Matrix Accuracy Testing
==========================================
Test 2.1: Identity matrices
[PASS] Identity test - Max error: 1.42E-14

Test 2.2: Random matrices  
[PASS] Random test - Max error: 3.67E-13

Test 2.3: Edge case (ALPHA=0, BETA=1)
[PASS] Edge case test - Max error: 0.00E+00

TEST 3: Performance Benchmarking
================================
GPU Time (1000 ops): 0.045 seconds
CPU Time (1000 ops): 0.120 seconds  
Speedup: 2.67x
[PASS] Performance within acceptable range

=================================================
PHASE 9.2 TEST RESULTS SUMMARY
=================================================
Tests Passed: 4 / 4
OVERALL RESULT: ALL TESTS PASSED
Phase 9.2 OpenCL implementation VERIFIED
```

### ⚠️ **Common Issues and Solutions**

#### Issue 1: OpenCL Not Found
```
[FAIL] OpenCL library not found
```
**Solution:**
- **macOS**: Ensure you're on macOS 10.6+ (OpenCL built-in)
- **Linux**: Install OpenCL drivers
  ```bash
  # Ubuntu/Debian
  sudo apt install opencl-headers ocl-icd-opencl-dev
  
  # NVIDIA GPUs
  sudo apt install nvidia-opencl-dev
  
  # AMD GPUs  
  sudo apt install amd-opencl-dev
  ```

#### Issue 2: No GPU Devices Found
```
[WARNING] No OpenCL platforms found
```
**Solutions:**
- **Check GPU drivers**: Ensure latest GPU drivers installed
- **Verify GPU support**: Your GPU must support OpenCL 1.2+
- **Intel integrated graphics**: May require specific Intel OpenCL runtime

#### Issue 3: Kernel Compilation Failed
```
[FAIL] Failed to build program: -11
```
**Solutions:**
- Check `dgemm_alpha.cl` file exists and is readable
- Verify OpenCL compiler supports double precision (`cl_khr_fp64`)
- Try simpler OpenCL test to isolate the issue

#### Issue 4: Accuracy Test Failed
```
[FAIL] Random test - Max error: 1.23E-06
```
**Solutions:**
- Check matrix layout conversion (column-major ↔ row-major)
- Verify all 49 operations implemented correctly in GPU kernel
- Compare intermediate results between CPU and GPU

## Advanced Testing

### 1. **Custom Matrix Testing**

Create your own test matrices:

```fortran
! Add to test_phase_9_2.f
SUBROUTINE SETUP_CUSTOM_TEST(A, B, ALPHA, BETA)
    ! Your custom matrix setup
    A(1,1) = 1.5D+0  ! Customize as needed
    ! ... set other elements
    ALPHA = 2.0D+0
    BETA = 1.0D+0
END
```

### 2. **Performance Profiling**

For detailed GPU performance analysis:

```bash
# Linux with NVIDIA GPU
nvprof ./test_phase_9_2

# macOS Instruments
instruments -t "GPU Compute" ./test_phase_9_2
```

### 3. **Debugging GPU Kernels**

Add debug output to kernels:

```c
// In dgemm_alpha.cl, add debugging
printf("Work-item %d: A[0]=%f, B[0]=%f\n", get_global_id(0), A[0], B[0]);
```

### 4. **Batch Testing**

Test batched operations (when implemented):

```fortran
! Test multiple 4x4 matrices simultaneously
BATCH_SIZE = 100
! ... setup batch arrays
CALL DGEMM_ALPHA_BATCH(BATCH_SIZE, ...)
```

## Performance Expectations

### **GPU vs CPU Speedup Expectations**

| Scenario | Expected GPU Speedup | Notes |
|----------|---------------------|-------|
| Single 4×4 matrix | 0.5x - 2x | GPU overhead dominates small matrices |
| Small batch (10 matrices) | 1x - 3x | GPU starts to break even |
| Medium batch (100 matrices) | 3x - 10x | GPU parallelism becomes effective |
| Large batch (1000+ matrices) | 10x - 20x | GPU optimal performance range |

### **Accuracy Expectations**

- **Excellent**: < 1e-12 error (numerical precision limit)
- **Good**: < 1e-9 error (acceptable for most applications)
- **Acceptable**: < 1e-6 error (meets original target)
- **Poor**: > 1e-6 error (indicates implementation bug)

## Troubleshooting Checklist

- [ ] **OpenCL installed and working**: `clinfo` shows platforms and devices
- [ ] **Compilers available**: `gcc --version` and `gfortran --version` work
- [ ] **File permissions**: `dgemm_alpha.cl` is readable
- [ ] **Library paths**: LD_LIBRARY_PATH includes LAPACK libraries if needed
- [ ] **GPU memory**: Sufficient GPU memory for buffers (minimal for 4×4)
- [ ] **Kernel syntax**: OpenCL kernel compiles without syntax errors
- [ ] **Interface compatibility**: C-Fortran interface matches expectations

## Integration Testing

### Testing with Your Application

```fortran
! Example integration in your Fortran code
EXTERNAL ALPHATENSOR_GPU_AVAILABLE
INTEGER ALPHATENSOR_GPU_AVAILABLE

IF (ALPHATENSOR_GPU_AVAILABLE() .EQ. 1) THEN
    ! Use GPU-accelerated path
    CALL DGEMM_ALPHA_GPU(ALPHA, A, LDA, B, LDB, BETA, C, LDC)
ELSE
    ! Fallback to CPU implementation  
    CALL DGEMM_ALPHA('N', 'N', M, N, K, ALPHA, A, LDA, B, LDB, BETA, C, LDC)
END IF
```

### Continuous Integration

For automated testing in CI/CD:

```bash
# Add to your CI script
cd SRC/VARIANTS/alphatensor_hybrid
./test_phase_9_2.sh
if [ $? -eq 0 ]; then
    echo "✅ Phase 9.2 GPU tests passed"
else
    echo "❌ Phase 9.2 GPU tests failed"
    exit 1
fi
```

## Next Steps After Testing

1. **Integration**: Integrate with your application workflow
2. **Optimization**: Profile and optimize for your specific use cases  
3. **Scaling**: Test with larger batch sizes for maximum GPU utilization
4. **Production**: Deploy with proper error handling and fallback mechanisms

## Support and Debugging

If tests fail, check:

1. **Environment logs**: Look for OpenCL platform/device information
2. **Compilation logs**: Check for kernel compilation errors
3. **Runtime logs**: Examine GPU execution error messages
4. **Comparison logs**: Verify CPU vs GPU result differences

The implementation includes comprehensive logging to help diagnose issues. 
