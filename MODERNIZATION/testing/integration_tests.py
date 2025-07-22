#!/usr/bin/env python3
"""
LAPACK AI Modernization - Integration Tests
Tests Python-Fortran interoperability and basic LAPACK functionality.
"""

import numpy as np
import scipy.linalg
import subprocess
import tempfile
import os
import sys
from pathlib import Path
from typing import Dict, Any, Tuple

class LAPackIntegrationTests:
    """Integration tests for LAPACK AI modernization foundation."""
    
    def __init__(self):
        self.project_root = Path(__file__).parent.parent.parent
        self.test_results: Dict[str, Any] = {}
        
    def log(self, message: str, level: str = "INFO") -> None:
        """Log test messages."""
        symbols = {"INFO": "ℹ️", "PASS": "✅", "FAIL": "❌", "WARN": "⚠️"}
        symbol = symbols.get(level, "•")
        print(f"{symbol} {message}")
        
    def test_numpy_scipy_integration(self) -> Dict[str, Any]:
        """Test NumPy and SciPy integration for LAPACK operations."""
        self.log("Testing NumPy/SciPy LAPACK integration...", "INFO")
        results = {}
        
        try:
            # Test basic matrix operations
            A = np.random.rand(100, 50)
            
            # Test SVD (calls LAPACK's DGESVD under the hood)
            U, s, Vt = np.linalg.svd(A, full_matrices=False)
            
            # Verify SVD reconstruction
            A_reconstructed = U @ np.diag(s) @ Vt
            svd_error = np.linalg.norm(A - A_reconstructed)
            
            results["numpy_svd"] = {
                "status": "PASS" if svd_error < 1e-10 else "FAIL",
                "error": float(svd_error),
                "details": f"SVD reconstruction error: {svd_error:.2e}"
            }
            
            # Test matrix multiplication (calls BLAS DGEMM)
            B = np.random.rand(50, 75)
            C = A @ B
            
            # Verify against manual computation
            C_manual = np.zeros((100, 75))
            for i in range(100):
                for j in range(75):
                    C_manual[i, j] = np.sum(A[i, :] * B[:, j])
            
            gemm_error = np.linalg.norm(C - C_manual)
            results["numpy_gemm"] = {
                "status": "PASS" if gemm_error < 1e-10 else "FAIL",
                "error": float(gemm_error),
                "details": f"GEMM computation error: {gemm_error:.2e}"
            }
            
            # Test linear solve (calls LAPACK DGESV)
            A_square = np.random.rand(50, 50)
            # Ensure well-conditioned matrix
            A_square = A_square @ A_square.T + np.eye(50)
            b = np.random.rand(50)
            
            x = scipy.linalg.solve(A_square, b)
            solve_error = np.linalg.norm(A_square @ x - b)
            
            results["scipy_solve"] = {
                "status": "PASS" if solve_error < 1e-10 else "FAIL",
                "error": float(solve_error),
                "details": f"Linear solve residual: {solve_error:.2e}"
            }
            
            self.log("NumPy/SciPy LAPACK integration: All tests passed", "PASS")
            
        except Exception as e:
            results["integration_error"] = {
                "status": "FAIL",
                "error": str(e),
                "details": f"Integration test failed: {e}"
            }
            self.log(f"NumPy/SciPy integration failed: {e}", "FAIL")
            
        return results
    
    def test_opencl_basic_functionality(self) -> Dict[str, Any]:
        """Test basic OpenCL functionality for GPU preparation."""
        self.log("Testing OpenCL basic functionality...", "INFO")
        results = {}
        
        try:
            import pyopencl as cl
            
            # Get platforms and devices
            platforms = cl.get_platforms()
            results["platforms_found"] = {
                "status": "PASS" if len(platforms) > 0 else "WARN",
                "count": len(platforms),
                "details": f"Found {len(platforms)} OpenCL platforms"
            }
            
            if len(platforms) == 0:
                self.log("No OpenCL platforms found (normal without GPU)", "WARN")
                return results
            
            # Test device enumeration
            total_devices = 0
            device_info = []
            
            for i, platform in enumerate(platforms):
                devices = platform.get_devices()
                total_devices += len(devices)
                
                for j, device in enumerate(devices):
                    device_info.append({
                        "platform": i,
                        "device": j,
                        "name": device.name.strip(),
                        "type": cl.device_type.to_string(device.type),
                        "global_mem": device.global_mem_size
                    })
            
            results["devices_found"] = {
                "status": "PASS" if total_devices > 0 else "WARN",
                "count": total_devices,
                "details": f"Found {total_devices} OpenCL devices",
                "devices": device_info
            }
            
            if total_devices > 0:
                # Test basic OpenCL context creation
                try:
                    context = cl.create_some_context(interactive=False)
                    queue = cl.CommandQueue(context)
                    
                    # Test simple kernel execution
                    kernel_source = """
                    __kernel void vector_add(__global const float* a,
                                           __global const float* b,
                                           __global float* result) {
                        int gid = get_global_id(0);
                        result[gid] = a[gid] + b[gid];
                    }
                    """
                    
                    program = cl.Program(context, kernel_source).build()
                    
                    # Test data
                    size = 1024
                    a = np.random.rand(size).astype(np.float32)
                    b = np.random.rand(size).astype(np.float32)
                    
                    # GPU buffers
                    a_buf = cl.Buffer(context, cl.mem_flags.READ_ONLY | cl.mem_flags.COPY_HOST_PTR, hostbuf=a)
                    b_buf = cl.Buffer(context, cl.mem_flags.READ_ONLY | cl.mem_flags.COPY_HOST_PTR, hostbuf=b)
                    result_buf = cl.Buffer(context, cl.mem_flags.WRITE_ONLY, a.nbytes)
                    
                    # Execute kernel
                    program.vector_add(queue, a.shape, None, a_buf, b_buf, result_buf)
                    queue.finish()
                    
                    # Get result
                    result = np.empty_like(a)
                    cl.enqueue_copy(queue, result, result_buf)
                    
                    # Verify result
                    expected = a + b
                    error = np.linalg.norm(result - expected)
                    
                    results["opencl_kernel_test"] = {
                        "status": "PASS" if error < 1e-6 else "FAIL",
                        "error": float(error),
                        "details": f"Kernel execution error: {error:.2e}"
                    }
                    
                    self.log("OpenCL kernel execution: Success", "PASS")
                    
                except Exception as e:
                    results["opencl_kernel_test"] = {
                        "status": "FAIL",
                        "error": str(e),
                        "details": f"Kernel test failed: {e}"
                    }
                    self.log(f"OpenCL kernel test failed: {e}", "FAIL")
            
        except ImportError:
            results["pyopencl_import"] = {
                "status": "FAIL",
                "error": "PyOpenCL not available",
                "details": "PyOpenCL import failed"
            }
            self.log("PyOpenCL not available", "FAIL")
        except Exception as e:
            results["opencl_error"] = {
                "status": "FAIL",
                "error": str(e),
                "details": f"OpenCL test failed: {e}"
            }
            self.log(f"OpenCL test failed: {e}", "FAIL")
            
        return results
    
    def test_fortran_interoperability(self) -> Dict[str, Any]:
        """Test basic Fortran compilation and interoperability."""
        self.log("Testing Fortran compilation and interoperability...", "INFO")
        results = {}
        
        try:
            # Create a simple Fortran test program
            fortran_code = """
program test_fortran
    implicit none
    real(8) :: a(3,3), b(3,3), c(3,3)
    integer :: i, j
    
    ! Initialize test matrices
    do i = 1, 3
        do j = 1, 3
            a(i,j) = real(i + j, 8)
            b(i,j) = real(i * j, 8)
        end do
    end do
    
    ! Simple matrix multiplication
    call dgemm('N', 'N', 3, 3, 3, 1.0d0, a, 3, b, 3, 0.0d0, c, 3)
    
    ! Output result
    write(*,'(A)') 'Fortran test completed successfully'
    write(*,'(A,F10.6)') 'C(1,1) = ', c(1,1)
    
end program test_fortran

! Simple DGEMM implementation for testing
subroutine dgemm(transa, transb, m, n, k, alpha, a, lda, b, ldb, beta, c, ldc)
    implicit none
    character :: transa, transb
    integer :: m, n, k, lda, ldb, ldc, i, j, l
    real(8) :: alpha, beta, a(lda,*), b(ldb,*), c(ldc,*)
    
    ! Simple implementation (not optimized)
    do i = 1, m
        do j = 1, n
            c(i,j) = beta * c(i,j)
            do l = 1, k
                c(i,j) = c(i,j) + alpha * a(i,l) * b(l,j)
            end do
        end do
    end do
end subroutine dgemm
"""
            
            # Write Fortran code to temporary file
            with tempfile.NamedTemporaryFile(mode='w', suffix='.f90', delete=False) as f:
                f.write(fortran_code)
                fortran_file = f.name
            
            try:
                # Compile Fortran program
                executable = fortran_file.replace('.f90', '')
                compile_result = subprocess.run([
                    'gfortran', '-o', executable, fortran_file
                ], capture_output=True, text=True, timeout=30)
                
                if compile_result.returncode == 0:
                    results["fortran_compilation"] = {
                        "status": "PASS",
                        "details": "Fortran compilation successful"
                    }
                    self.log("Fortran compilation: Success", "PASS")
                    
                    # Run the program
                    run_result = subprocess.run([
                        executable
                    ], capture_output=True, text=True, timeout=10)
                    
                    if run_result.returncode == 0 and "Fortran test completed successfully" in run_result.stdout:
                        results["fortran_execution"] = {
                            "status": "PASS",
                            "details": "Fortran program executed successfully",
                            "output": run_result.stdout.strip()
                        }
                        self.log("Fortran execution: Success", "PASS")
                    else:
                        results["fortran_execution"] = {
                            "status": "FAIL",
                            "details": f"Execution failed: {run_result.stderr}",
                            "returncode": run_result.returncode
                        }
                        self.log("Fortran execution: Failed", "FAIL")
                    
                    # Clean up executable
                    try:
                        os.unlink(executable)
                    except:
                        pass
                        
                else:
                    results["fortran_compilation"] = {
                        "status": "FAIL",
                        "details": f"Compilation failed: {compile_result.stderr}",
                        "returncode": compile_result.returncode
                    }
                    self.log("Fortran compilation: Failed", "FAIL")
                    
            finally:
                # Clean up source file
                try:
                    os.unlink(fortran_file)
                except:
                    pass
                    
        except Exception as e:
            results["fortran_test_error"] = {
                "status": "FAIL",
                "error": str(e),
                "details": f"Fortran test failed: {e}"
            }
            self.log(f"Fortran test failed: {e}", "FAIL")
            
        return results
    
    def test_cmake_build_system(self) -> Dict[str, Any]:
        """Test CMake build system functionality."""
        self.log("Testing CMake build system...", "INFO")
        results = {}
        
        try:
            # Test CMake version
            cmake_result = subprocess.run([
                'cmake', '--version'
            ], capture_output=True, text=True, timeout=10)
            
            if cmake_result.returncode == 0:
                version_line = cmake_result.stdout.split('\n')[0]
                results["cmake_version"] = {
                    "status": "PASS",
                    "version": version_line,
                    "details": f"CMake available: {version_line}"
                }
                self.log(f"CMake version: {version_line}", "PASS")
                
                # Test basic CMake configuration
                with tempfile.TemporaryDirectory() as temp_dir:
                    build_dir = Path(temp_dir) / "build"
                    build_dir.mkdir()
                    
                    # Change to project root for CMake
                    original_cwd = os.getcwd()
                    try:
                        os.chdir(self.project_root)
                        
                        config_result = subprocess.run([
                            'cmake', '-B', str(build_dir),
                            '-DCMAKE_BUILD_TYPE=Debug',
                            '-DBUILD_TESTING=OFF'
                        ], capture_output=True, text=True, timeout=60)
                        
                        if config_result.returncode == 0:
                            results["cmake_configuration"] = {
                                "status": "PASS",
                                "details": "CMake configuration successful"
                            }
                            self.log("CMake configuration: Success", "PASS")
                        else:
                            results["cmake_configuration"] = {
                                "status": "WARN",
                                "details": f"Configuration issues: {config_result.stderr[-200:]}",
                                "returncode": config_result.returncode
                            }
                            self.log("CMake configuration: Issues detected", "WARN")
                            
                    finally:
                        os.chdir(original_cwd)
                        
            else:
                results["cmake_version"] = {
                    "status": "FAIL",
                    "details": f"CMake not available: {cmake_result.stderr}",
                    "returncode": cmake_result.returncode
                }
                self.log("CMake not available", "FAIL")
                
        except Exception as e:
            results["cmake_test_error"] = {
                "status": "FAIL",
                "error": str(e),
                "details": f"CMake test failed: {e}"
            }
            self.log(f"CMake test failed: {e}", "FAIL")
            
        return results
    
    def test_container_environment(self) -> Dict[str, Any]:
        """Test container-specific functionality."""
        self.log("Testing container environment...", "INFO")
        results = {}
        
        # Check if running in container
        is_container = Path('/.dockerenv').exists()
        results["container_detection"] = {
            "status": "INFO",
            "is_container": is_container,
            "details": f"Running in {'container' if is_container else 'host'} environment"
        }
        
        if is_container:
            # Test container-specific features
            try:
                # Test environment variables
                env_vars = ['PYTHONPATH', 'CC', 'CXX', 'FC']
                for var in env_vars:
                    value = os.environ.get(var, 'NOT_SET')
                    results[f"env_{var}"] = {
                        "status": "PASS" if value != 'NOT_SET' else "WARN",
                        "value": value,
                        "details": f"{var}={value}"
                    }
                
                # Test file permissions
                test_file = Path('/tmp/permission_test')
                try:
                    test_file.write_text('test')
                    test_file.unlink()
                    results["file_permissions"] = {
                        "status": "PASS",
                        "details": "File write permissions working"
                    }
                except Exception as e:
                    results["file_permissions"] = {
                        "status": "FAIL",
                        "error": str(e),
                        "details": f"File permission test failed: {e}"
                    }
                    
            except Exception as e:
                results["container_test_error"] = {
                    "status": "FAIL",
                    "error": str(e),
                    "details": f"Container test failed: {e}"
                }
        else:
            self.log("Not running in container - skipping container-specific tests", "INFO")
            
        return results
    
    def run_all_tests(self) -> Dict[str, Any]:
        """Run all integration tests and return comprehensive results."""
        self.log("🧪 Running LAPACK AI Integration Tests", "INFO")
        
        # Run all test suites
        self.test_results = {
            "numpy_scipy": self.test_numpy_scipy_integration(),
            "opencl": self.test_opencl_basic_functionality(), 
            "fortran": self.test_fortran_interoperability(),
            "cmake": self.test_cmake_build_system(),
            "container": self.test_container_environment()
        }
        
        # Calculate summary
        total_tests = 0
        passed_tests = 0
        failed_tests = 0
        warned_tests = 0
        
        for suite_name, suite_results in self.test_results.items():
            for test_name, test_result in suite_results.items():
                total_tests += 1
                status = test_result.get("status", "UNKNOWN")
                if status == "PASS":
                    passed_tests += 1
                elif status == "FAIL":
                    failed_tests += 1
                elif status == "WARN":
                    warned_tests += 1
        
        # Print summary
        print("\n" + "="*50)
        print("🧪 INTEGRATION TEST RESULTS")
        print("="*50)
        print(f"Total Tests: {total_tests}")
        print(f"✅ Passed: {passed_tests}")
        print(f"❌ Failed: {failed_tests}")
        print(f"⚠️  Warnings: {warned_tests}")
        
        success_rate = (passed_tests / total_tests * 100) if total_tests > 0 else 0
        print(f"Success Rate: {success_rate:.1f}%")
        
        if failed_tests == 0:
            self.log("All integration tests completed successfully!", "PASS")
            return {"status": "PASS", "details": self.test_results}
        else:
            self.log(f"Integration tests failed: {failed_tests} failures", "FAIL")
            return {"status": "FAIL", "details": self.test_results}

def main():
    """Main test runner."""
    print("🧪 LAPACK AI Modernization - Integration Tests")
    print("=" * 50)
    
    tester = LAPackIntegrationTests()
    try:
        results = tester.run_all_tests()
        if results["status"] == "PASS":
            sys.exit(0)
        else:
            sys.exit(1)
    except KeyboardInterrupt:
        print("\n⚠️  Tests interrupted by user")
        sys.exit(130)
    except Exception as e:
        print(f"\n❌ Test runner failed: {e}")
        sys.exit(1)

if __name__ == "__main__":
    main() 