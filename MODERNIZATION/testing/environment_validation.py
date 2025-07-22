#!/usr/bin/env python3
"""
LAPACK AI Modernization - Environment Validation Script
Validates all Phase 1 deliverables and containerized environment components.
"""

import sys
import subprocess
import os
import platform
import time
from pathlib import Path
from typing import Dict, List, Tuple, Optional

class EnvironmentValidator:
    """Comprehensive validation of LAPACK AI modernization environment."""
    
    def __init__(self):
        self.results: Dict[str, Dict] = {}
        self.project_root = Path(__file__).parent.parent.parent
        self.is_container = Path('/.dockerenv').exists()
        
    def log(self, message: str, level: str = "INFO") -> None:
        """Log validation messages with timestamps."""
        timestamp = time.strftime("%H:%M:%S")
        symbols = {"INFO": "ℹ️", "PASS": "✅", "FAIL": "❌", "WARN": "⚠️"}
        symbol = symbols.get(level, "•")
        print(f"[{timestamp}] {symbol} {message}")
        
    def run_command(self, cmd: List[str], check: bool = True) -> Tuple[bool, str]:
        """Execute command and return success status and output."""
        try:
            result = subprocess.run(
                cmd, capture_output=True, text=True, check=check, timeout=30
            )
            return True, result.stdout.strip()
        except (subprocess.CalledProcessError, subprocess.TimeoutExpired, FileNotFoundError) as e:
            return False, str(e)
            
    def validate_phase_1a(self) -> Dict:
        """Validate Phase 1A: Containerized Development Environment."""
        self.log("🚀 Phase 1A: Containerized Development Environment", "INFO")
        results = {}
        
        # Test Docker availability
        success, output = self.run_command(["docker", "--version"])
        results["docker_available"] = {
            "status": "PASS" if success else "FAIL",
            "details": output[:100],
            "required": True
        }
        if success:
            self.log(f"Docker version: {output}", "PASS")
        else:
            self.log("Docker not available", "FAIL")
            
        # Test container builds
        if success:
            # Test base container
            self.log("Testing base container build...")
            build_success, build_output = self.run_command([
                "docker", "build", "-f", "MODERNIZATION/_docs/Dockerfile.base", 
                "-t", "lapack-ai-base:test", ".", "--quiet"
            ])
            results["base_container_build"] = {
                "status": "PASS" if build_success else "FAIL",
                "details": build_output,
                "required": True
            }
            
            # Test development container
            if build_success:
                self.log("Testing development container build...")
                dev_success, dev_output = self.run_command([
                    "docker", "build", "-f", "MODERNIZATION/dev_environment/Dockerfile.dev",
                    "-t", "lapack-ai-dev:test", ".", "--quiet"
                ])
                results["dev_container_build"] = {
                    "status": "PASS" if dev_success else "FAIL",
                    "details": dev_output,
                    "required": True
                }
        
        # Test Python dependencies in container
        if self.is_container:
            self.log("Testing Python dependencies in container...")
            deps_to_test = [
                ("numpy", "import numpy; print(numpy.__version__)"),
                ("scipy", "import scipy; print(scipy.__version__)"),
                ("pybind11", "import pybind11; print(pybind11.__version__)"),
                ("pyopencl", "import pyopencl; print(pyopencl.VERSION_TEXT)"),
                ("flask", "import flask; print(flask.__version__)")
            ]
            
            for dep_name, test_code in deps_to_test:
                dep_success, dep_output = self.run_command([
                    "python", "-c", test_code
                ])
                results[f"dependency_{dep_name}"] = {
                    "status": "PASS" if dep_success else "FAIL",
                    "details": dep_output,
                    "required": True
                }
                if dep_success:
                    self.log(f"{dep_name}: {dep_output}", "PASS")
                else:
                    self.log(f"{dep_name}: Failed - {dep_output}", "FAIL")
        
        return results
    
    def validate_phase_1b(self) -> Dict:
        """Validate Phase 1B: Legacy Codebase Analysis."""
        self.log("📊 Phase 1B: Legacy Codebase Analysis", "INFO")
        results = {}
        
        # Check analysis documents exist
        analysis_docs = [
            "MODERNIZATION/_docs/codebase_analysis.md",
            "MODERNIZATION/_docs/function_interface_mapping.md",
            "MODERNIZATION/_docs/modernization_strategy.md"
        ]
        
        for doc_path in analysis_docs:
            doc_file = self.project_root / doc_path
            exists = doc_file.exists()
            
            doc_name = doc_path.split('/')[-1]
            results[f"document_{doc_name}"] = {
                "status": "PASS" if exists else "FAIL",
                "details": f"File size: {doc_file.stat().st_size if exists else 0} bytes",
                "required": True
            }
            
            if exists:
                self.log(f"{doc_name}: Present ({doc_file.stat().st_size} bytes)", "PASS")
            else:
                self.log(f"{doc_name}: Missing", "FAIL")
        
        # Validate LAPACK source files accessibility
        lapack_files = [
            "SRC/dgesvd.f",
            "BLAS/SRC/dgemm.f",
            "LAPACKE/src"
        ]
        
        for lapack_file in lapack_files:
            file_path = self.project_root / lapack_file
            exists = file_path.exists()
            
            results[f"lapack_file_{lapack_file.replace('/', '_')}"] = {
                "status": "PASS" if exists else "FAIL",
                "details": f"Path: {file_path}",
                "required": True
            }
            
            if exists:
                self.log(f"LAPACK file {lapack_file}: Accessible", "PASS")
            else:
                self.log(f"LAPACK file {lapack_file}: Not found", "FAIL")
        
        return results
    
    def validate_phase_1c(self) -> Dict:
        """Validate Phase 1C: Docker Infrastructure."""
        self.log("🐳 Phase 1C: Docker Infrastructure", "INFO")
        results = {}
        
        # Check Docker files exist
        docker_files = [
            "MODERNIZATION/_docs/Dockerfile.base",
            "MODERNIZATION/dev_environment/Dockerfile.dev",
            "MODERNIZATION/_docs/Dockerfile.prod",
            "MODERNIZATION/_docs/.dockerignore",
            "docker-compose.dev.yml"
        ]
        
        for docker_file in docker_files:
            file_path = self.project_root / docker_file
            exists = file_path.exists()
            
            file_name = docker_file.split('/')[-1]
            results[f"docker_file_{file_name}"] = {
                "status": "PASS" if exists else "FAIL",
                "details": f"Size: {file_path.stat().st_size if exists else 0} bytes",
                "required": True
            }
            
            if exists:
                self.log(f"{file_name}: Present", "PASS")
            else:
                self.log(f"{file_name}: Missing", "FAIL")
        
        # Test container functionality if in container
        if self.is_container:
            self.log("Testing container environment...")
            
            # Test build tools
            build_tools = [
                (["gcc", "--version"], "GCC compiler"),
                (["gfortran", "--version"], "Gfortran compiler"),
                (["cmake", "--version"], "CMake build system"),
                (["python", "--version"], "Python interpreter")
            ]
            
            for cmd, tool_name in build_tools:
                tool_success, tool_output = self.run_command(cmd)
                results[f"build_tool_{tool_name.lower().replace(' ', '_')}"] = {
                    "status": "PASS" if tool_success else "FAIL",
                    "details": tool_output.split('\n')[0] if tool_success else tool_output,
                    "required": True
                }
                
                if tool_success:
                    version_line = tool_output.split('\n')[0]
                    self.log(f"{tool_name}: {version_line}", "PASS")
                else:
                    self.log(f"{tool_name}: Not available - {tool_output}", "FAIL")
        
        # Test container health if running containers
        self.log("Checking container health...")
        health_success, health_output = self.run_command([
            "python", "-c", 
            "import numpy, scipy, flask; print('✅ Container environment healthy')"
        ])
        results["container_health"] = {
            "status": "PASS" if health_success else "FAIL",
            "details": health_output,
            "required": True
        }
        
        if health_success:
            self.log("Container health check: Passed", "PASS")
        else:
            self.log(f"Container health check: Failed - {health_output}", "FAIL")
        
        return results
    
    def validate_gpu_support(self) -> Dict:
        """Validate GPU/OpenCL support."""
        self.log("🎮 GPU/OpenCL Support", "INFO")
        results = {}
        
        # Test OpenCL availability
        opencl_success, opencl_output = self.run_command([
            "python", "-c", """
import pyopencl as cl
try:
    platforms = cl.get_platforms()
    print(f'OpenCL platforms: {len(platforms)}')
    for i, platform in enumerate(platforms):
        devices = platform.get_devices()
        print(f'Platform {i}: {platform.name} ({len(devices)} devices)')
        for j, device in enumerate(devices):
            print(f'  Device {j}: {device.name}')
except Exception as e:
    print(f'OpenCL error: {e}')
"""
        ])
        
        results["opencl_detection"] = {
            "status": "PASS" if opencl_success else "WARN",
            "details": opencl_output,
            "required": False  # GPU is optional
        }
        
        if opencl_success and "OpenCL platforms: 0" not in opencl_output:
            self.log("OpenCL devices detected", "PASS")
        elif "PLATFORM_NOT_FOUND_KHR" in opencl_output:
            self.log("No OpenCL devices found (normal without GPU)", "WARN")
        else:
            self.log(f"OpenCL issue: {opencl_output}", "WARN")
        
        # Test clinfo if available
        clinfo_success, clinfo_output = self.run_command(["clinfo", "-l"], check=False)
        results["clinfo_available"] = {
            "status": "PASS" if clinfo_success else "WARN",
            "details": clinfo_output[:200],
            "required": False
        }
        
        return results
    
    def validate_documentation(self) -> Dict:
        """Validate documentation completeness."""
        self.log("📚 Documentation Completeness", "INFO")
        results = {}
        
        # Check all required documentation
        required_docs = [
            ("MODERNIZATION/_docs/phase1_implementation_plan.md", "Implementation plan"),
            ("MODERNIZATION/_docs/codebase_analysis.md", "Codebase analysis"),
            ("MODERNIZATION/_docs/function_interface_mapping.md", "Interface mapping"),
            ("MODERNIZATION/_docs/modernization_strategy.md", "Modernization strategy"),
            ("MODERNIZATION/_docs/docker_configuration.md", "Docker configuration"),
            ("MODERNIZATION/codebase-research/dev_environment_setup.md", "Environment setup"),
            ("README.md", "Project README")
        ]
        
        for doc_path, doc_description in required_docs:
            doc_file = self.project_root / doc_path
            exists = doc_file.exists()
            
            if exists:
                size = doc_file.stat().st_size
                # Check if document has substantial content (>1KB)
                substantial = size > 1024
                
                results[f"doc_{doc_path.split('/')[-1]}"] = {
                    "status": "PASS" if substantial else "WARN",
                    "details": f"{size} bytes",
                    "required": True
                }
                
                if substantial:
                    self.log(f"{doc_description}: Complete ({size} bytes)", "PASS")
                else:
                    self.log(f"{doc_description}: Too small ({size} bytes)", "WARN")
            else:
                results[f"doc_{doc_path.split('/')[-1]}"] = {
                    "status": "FAIL",
                    "details": "File not found",
                    "required": True
                }
                self.log(f"{doc_description}: Missing", "FAIL")
        
        return results
    
    def validate_build_system(self) -> Dict:
        """Validate LAPACK build system functionality."""
        self.log("🔨 Build System Validation", "INFO")
        results = {}
        
        if not self.is_container:
            self.log("Build system validation requires container environment", "WARN")
            return {"build_system_skipped": {"status": "WARN", "details": "Not in container", "required": False}}
        
        # Test CMake configuration
        cmake_success, cmake_output = self.run_command([
            "cmake", "--version"
        ])
        results["cmake_available"] = {
            "status": "PASS" if cmake_success else "FAIL",
            "details": cmake_output.split('\n')[0] if cmake_success else cmake_output,
            "required": True
        }
        
        # Test basic CMake configuration (dry run)
        if cmake_success:
            self.log("Testing CMake configuration...")
            build_dir = Path("/tmp/lapack_build_test")
            build_dir.mkdir(exist_ok=True)
            
            config_success, config_output = self.run_command([
                "cmake", "-B", str(build_dir), 
                "-DCMAKE_BUILD_TYPE=Debug", 
                "-DBUILD_TESTING=OFF",
                "."
            ])
            results["cmake_configuration"] = {
                "status": "PASS" if config_success else "WARN",
                "details": "Configuration successful" if config_success else config_output[-200:],
                "required": False
            }
            
            if config_success:
                self.log("CMake configuration: Success", "PASS")
            else:
                self.log("CMake configuration: Issues detected", "WARN")
            
            # Clean up
            subprocess.run(["rm", "-rf", str(build_dir)], capture_output=True)
        
        return results
    
    def generate_report(self) -> None:
        """Generate comprehensive validation report."""
        self.log("📋 Generating Validation Report", "INFO")
        
        # Collect all validation results
        self.results["phase_1a"] = self.validate_phase_1a()
        self.results["phase_1b"] = self.validate_phase_1b()
        self.results["phase_1c"] = self.validate_phase_1c()
        self.results["gpu_support"] = self.validate_gpu_support()
        self.results["documentation"] = self.validate_documentation()
        self.results["build_system"] = self.validate_build_system()
        
        # Calculate summary statistics
        total_tests = 0
        passed_tests = 0
        failed_tests = 0
        warned_tests = 0
        
        for phase, tests in self.results.items():
            for test_name, test_result in tests.items():
                total_tests += 1
                status = test_result["status"]
                if status == "PASS":
                    passed_tests += 1
                elif status == "FAIL":
                    failed_tests += 1
                elif status == "WARN":
                    warned_tests += 1
        
        # Print summary
        print("\n" + "="*60)
        print("🎯 LAPACK AI MODERNIZATION - PHASE 1 VALIDATION REPORT")
        print("="*60)
        print(f"Environment: {'Container' if self.is_container else 'Host'}")
        print(f"Platform: {platform.system()} {platform.release()}")
        print(f"Python: {platform.python_version()}")
        print("-"*60)
        print(f"Total Tests: {total_tests}")
        print(f"✅ Passed: {passed_tests}")
        print(f"❌ Failed: {failed_tests}")
        print(f"⚠️  Warnings: {warned_tests}")
        
        # Calculate success rate
        success_rate = (passed_tests / total_tests * 100) if total_tests > 0 else 0
        print(f"Success Rate: {success_rate:.1f}%")
        
        # Determine overall status
        if failed_tests == 0:
            if warned_tests == 0:
                status_emoji = "🎉"
                status_text = "EXCELLENT - All validations passed!"
            else:
                status_emoji = "✅"
                status_text = "GOOD - Core functionality validated"
        else:
            status_emoji = "❌"
            status_text = "ISSUES DETECTED - Review failed tests"
        
        print(f"\n{status_emoji} Overall Status: {status_text}")
        
        # Show failed tests
        if failed_tests > 0:
            print("\n❌ Failed Tests:")
            for phase, tests in self.results.items():
                for test_name, test_result in tests.items():
                    if test_result["status"] == "FAIL":
                        print(f"  • {phase}.{test_name}: {test_result['details']}")
        
        # Show warnings
        if warned_tests > 0:
            print("\n⚠️  Warnings:")
            for phase, tests in self.results.items():
                for test_name, test_result in tests.items():
                    if test_result["status"] == "WARN":
                        print(f"  • {phase}.{test_name}: {test_result['details']}")
        
        print("\n" + "="*60)
        
        # Exit with appropriate code
        if failed_tests > 0:
            print("❌ Validation failed - review issues above")
            sys.exit(1)
        else:
            print("✅ Phase 1 validation completed successfully!")
            print("🚀 Ready for Phase 2 implementation!")
            sys.exit(0)

def main():
    """Main validation entry point."""
    print("🚀 LAPACK AI Modernization - Environment Validation")
    print("=" * 50)
    
    validator = EnvironmentValidator()
    try:
        validator.generate_report()
    except KeyboardInterrupt:
        print("\n⚠️  Validation interrupted by user")
        sys.exit(130)
    except Exception as e:
        print(f"\n❌ Validation failed with error: {e}")
        sys.exit(1)

if __name__ == "__main__":
    main() 