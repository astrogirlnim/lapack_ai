# LAPACK AI Modernization - Development Environment Setup

## Overview

This document provides a complete guide for setting up a production-like development environment for the LAPACK AI Modernization project. The environment supports GPU-accelerated linear algebra operations, Python integration, and modern development practices.

## Environment Architecture

```
LAPACK AI Development Environment
├── Host System (macOS/Linux)
│   ├── Build Tools (CMake, GCC, gfortran)
│   ├── GPU/OpenCL Support
│   └── Python 3.11+ Runtime
├── Python Virtual Environment
│   ├── Scientific Computing (NumPy, SciPy)
│   ├── GPU Computing (PyOpenCL)
│   ├── Web Framework (Flask)
│   └── Development Tools (pytest, black, mypy)
└── Production Simulation
    ├── Docker Container Support
    ├── Performance Monitoring
    └── Cross-Platform Compatibility
```

## Prerequisites

### System Requirements

**Hardware:**
- x86_64 or ARM64 processor
- 8+ GB RAM (16+ GB recommended)
- GPU with OpenCL 1.2+ support (optional but recommended)
- 5+ GB free disk space

**Operating System:**
- macOS 11.0+ (Big Sur or later)
- Ubuntu 20.04 LTS+ / Debian 11+
- CentOS 8+ / RHEL 8+

## Installation Guide

### Step 1: Install System Dependencies

#### macOS (using Homebrew)

```bash
# Install Homebrew if not already installed
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

# Install development tools and libraries
brew install cmake gcc gfortran openblas lapack opencl-headers

# Verify installations
cmake --version    # Should be 4.0.3+
gfortran --version # Should be GNU Fortran 15.1.0+
gcc --version      # Should show Homebrew GCC
```

#### Ubuntu/Debian

```bash
# Update package manager
sudo apt update

# Install development tools
sudo apt install -y \
    build-essential \
    gfortran \
    cmake \
    python3.11-dev \
    python3-pip \
    opencl-headers \
    ocl-icd-opencl-dev \
    libblas-dev \
    liblapack-dev \
    libopenblas-dev

# Install Python virtual environment tools
sudo apt install -y python3.11-venv
```

### Step 2: Set Up Python Virtual Environment

```bash
# Navigate to the project directory
cd /path/to/lapack_ai

# Create virtual environment in the dev_environment folder
cd MODERNIZATION/dev_environment
python3 -m venv venv

# Activate the virtual environment
source venv/bin/activate

# Upgrade pip and install dependencies
pip install --upgrade pip
pip install -r requirements.txt
```

### Step 3: Configure Environment Variables

The project includes an automated setup script that configures all necessary environment variables:

```bash
# Make the setup script executable
chmod +x MODERNIZATION/dev_environment/setup_env.sh

# Source the environment setup (run from project root)
source MODERNIZATION/dev_environment/setup_env.sh
```

### Step 4: Verify Installation

```bash
# Test Python dependencies
python -c "import pyopencl; print('✅ OpenCL available!')"
python -c "import numpy; print('✅ NumPy:', numpy.__version__)"
python -c "import pybind11; print('✅ pybind11:', pybind11.__version__)"

# Test LAPACK build system
mkdir -p build_test
cd build_test
cmake .. -DCMAKE_BUILD_TYPE=Debug -DBUILD_TESTING=OFF
cd .. && rm -rf build_test
```

## Environment Configuration Details

### Python Dependencies

The development environment includes:

**Core Libraries:**
- `pybind11>=2.10.0` - C++/Python bindings
- `numpy>=1.24.0` - Array operations
- `scipy>=1.10.0` - Scientific computing
- `pyopencl>=2022.2` - GPU computing

**Web Framework:**
- `flask>=2.3.0` - Web dashboard
- `flask-socketio>=5.3.0` - Real-time communication
- `gunicorn>=20.1.0` - Production WSGI server

**Development Tools:**
- `pytest>=7.0.0` - Testing framework
- `black>=23.0.0` - Code formatter
- `mypy>=1.0.0` - Type checker
- `flake8>=6.0.0` - Linting

**Monitoring & Debugging:**
- `psutil>=5.9.0` - System monitoring
- `jupyter>=1.0.0` - Interactive development
- `matplotlib>=3.6.0` - Visualization

### Environment Variables

The setup script automatically configures:

**Build Configuration:**
```bash
export CC="gcc-15"                    # C compiler
export CXX="g++-15"                   # C++ compiler  
export FC="gfortran-15"               # Fortran compiler
export CMAKE_BUILD_TYPE="Debug"       # Build type
```

**Library Paths (macOS):**
```bash
export LDFLAGS="-L/opt/homebrew/opt/openblas/lib -L/opt/homebrew/opt/lapack/lib"
export CPPFLAGS="-I/opt/homebrew/opt/openblas/include -I/opt/homebrew/opt/lapack/include"
export PKG_CONFIG_PATH="/opt/homebrew/opt/openblas/lib/pkgconfig:/opt/homebrew/opt/lapack/lib/pkgconfig"
```

**Development Settings:**
```bash
export PYTHONPATH="${PROJECT_ROOT}/src:${PYTHONPATH}"
export OCL_ENABLE_DEBUG=1
export OCL_DEBUG_LEVEL=2
export PYTEST_ADDOPTS="--tb=short --strict-markers"
```

## Daily Development Workflow

### 1. Activate Environment

```bash
# From project root
source MODERNIZATION/dev_environment/setup_env.sh
```

### 2. Development Commands

```bash
# Run tests
pytest

# Format code
black src/
isort src/

# Type checking
mypy src/

# Build LAPACK with custom features
mkdir build && cd build
cmake .. -DCMAKE_BUILD_TYPE=Debug \
         -DWITH_GPU_SUPPORT=ON \
         -DWITH_PYTHON_BINDINGS=ON \
         -DWITH_DASHBOARD=ON
make -j$(nproc)
```

### 3. GPU Development

```bash
# Test OpenCL availability
python -c "
import pyopencl as cl
platforms = cl.get_platforms()
print(f'Found {len(platforms)} OpenCL platforms')
for platform in platforms:
    devices = platform.get_devices()
    print(f'Platform: {platform.name}, Devices: {len(devices)}')
"

# Enable detailed GPU debugging
export OCL_ENABLE_DEBUG=1
export OCL_DEBUG_LEVEL=3
```

## Troubleshooting

### Common Issues

**1. OpenCL not found**
```bash
# macOS: Install OpenCL headers
brew install opencl-headers

# Linux: Install OpenCL development packages
sudo apt install opencl-headers ocl-icd-opencl-dev
```

**2. LAPACK libraries not found**
```bash
# Check library paths
pkg-config --libs lapack
pkg-config --libs openblas

# Add to environment if needed
export PKG_CONFIG_PATH="/usr/local/lib/pkgconfig:$PKG_CONFIG_PATH"
```

**3. Fortran compiler issues**
```bash
# Verify Fortran compiler
gfortran --version

# Test simple compilation
echo "program hello; print *, 'Hello'; end program" > test.f90
gfortran test.f90 -o test && ./test && rm test test.f90
```

**4. Python import errors**
```bash
# Reinstall dependencies
pip install --force-reinstall -r requirements.txt

# Check virtual environment
which python
python -c "import sys; print(sys.path)"
```

### Performance Optimization

**CMake Build Options:**
```bash
# Release build for performance testing
cmake .. -DCMAKE_BUILD_TYPE=Release \
         -DWITH_OPENMP=ON \
         -DWITH_VECTORIZATION=ON

# Debug build for development
cmake .. -DCMAKE_BUILD_TYPE=Debug \
         -DWITH_ASSERTIONS=ON \
         -DWITH_PROFILING=ON
```

**GPU Memory Management:**
```bash
# Monitor GPU memory usage
python -c "
import pyopencl as cl
ctx = cl.create_some_context()
print(f'GPU Memory: {ctx.devices[0].global_mem_size // 1024**3} GB')
"
```

## Testing the Environment

### Unit Tests

```bash
# Run all tests
pytest

# Run specific test categories
pytest tests/unit/           # Unit tests
pytest tests/integration/    # Integration tests
pytest tests/performance/    # Performance benchmarks
```

### Performance Benchmarks

```bash
# GPU vs CPU SVD benchmark
python benchmarks/svd_performance.py --matrix-size 1024 --iterations 100

# Memory usage profiling
python -m memory_profiler scripts/profile_memory.py
```

### Code Quality Checks

```bash
# Full quality check pipeline
black --check src/
isort --check-only src/
flake8 src/
mypy src/
```

## Production Simulation

### Docker Development

```bash
# Build development Docker image
docker build -f MODERNIZATION/dev_environment/Dockerfile.dev -t lapack-ai-dev .

# Run in container
docker run -it --gpus all -v $(pwd):/workspace lapack-ai-dev
```

### Cloud Deployment Testing

```bash
# AWS ECS simulation
docker run --platform linux/amd64 lapack-ai-dev

# Kubernetes resource limits
docker run --memory=2g --cpus=2 lapack-ai-dev
```

## Environment Maintenance

### Dependency Updates

```bash
# Update Python packages
pip list --outdated
pip install --upgrade pip
pip install --upgrade -r requirements.txt

# Update system packages (macOS)
brew update && brew upgrade

# Update system packages (Ubuntu)
sudo apt update && sudo apt upgrade
```

### Environment Reset

```bash
# Complete environment reset
rm -rf MODERNIZATION/dev_environment/venv
python3 -m venv MODERNIZATION/dev_environment/venv
source MODERNIZATION/dev_environment/venv/bin/activate
pip install -r MODERNIZATION/dev_environment/requirements.txt
```

## File Structure

```
MODERNIZATION/dev_environment/
├── venv/                          # Python virtual environment
├── requirements.txt               # Python dependencies
├── setup_env.sh                   # Environment setup script
├── Dockerfile.dev                 # Development container
├── .env.example                   # Environment template
└── scripts/
    ├── install_opencl.sh          # OpenCL setup script
    ├── test_gpu.py                # GPU functionality test
    └── benchmark.py               # Performance benchmarks
```

## Next Steps

1. **Start Development**: Activate environment and begin implementing features
2. **GPU Programming**: Develop OpenCL kernels for SVD and matrix operations
3. **Python Integration**: Create pybind11 wrappers for LAPACK functions
4. **Dashboard Development**: Build Flask-based monitoring interface
5. **Containerization**: Package complete solution in Docker

## Support

For environment setup issues:
1. Check system requirements and dependencies
2. Verify all installation steps were completed
3. Review troubleshooting section for common issues
4. Test individual components (Python, CMake, OpenCL) separately

---

**Environment Status**: ✅ Production-Ready  
**Last Updated**: January 2025  
**Version**: 1.0.0  
**Compatibility**: macOS 11+, Ubuntu 20.04+, Python 3.11+ 