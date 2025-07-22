#!/bin/bash
# LAPACK AI Modernization Development Environment Setup
# This script sets up all necessary environment variables for containerized development

echo "🚀 Setting up LAPACK AI Modernization Development Environment..."

# Get the directory of this script
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"
PROJECT_ROOT="$(dirname "$(dirname "$SCRIPT_DIR")")"

# Check if we're in a container (Python dependencies are already available)
if [ -f /.dockerenv ]; then
    echo "📦 Running in containerized environment - Python dependencies ready!"
else
    echo "🏠 Running on host system"
    # Try to activate venv if it exists for backward compatibility
    if [ -f "$SCRIPT_DIR/venv/bin/activate" ]; then
        echo "📦 Activating Python virtual environment..."
        source "$SCRIPT_DIR/venv/bin/activate"
    else
        echo "⚠️  No virtual environment found - assuming system Python"
    fi
fi

# Configure build environment based on host vs container
if [ -f /.dockerenv ]; then
    echo "🐳 Setting up containerized build environment..."

    # Container environment uses system packages
    export CC="gcc"
    export CXX="g++"
    export FC="gfortran"
    export LIBRARY_PATH="/usr/local/lib:/usr/lib/x86_64-linux-gnu"
    export LD_LIBRARY_PATH="/usr/local/lib:/usr/lib/x86_64-linux-gnu"
    export PKG_CONFIG_PATH="/usr/local/lib/pkgconfig:/usr/lib/x86_64-linux-gnu/pkgconfig"

    echo "✅ Container build environment configured"
elif [[ "$OSTYPE" == "darwin"* ]]; then
    echo "🍎 Setting up macOS-specific library paths..."

    # OpenBLAS (keg-only)
    export LDFLAGS="${LDFLAGS} -L/opt/homebrew/opt/openblas/lib"
    export CPPFLAGS="${CPPFLAGS} -I/opt/homebrew/opt/openblas/include"
    export PKG_CONFIG_PATH="${PKG_CONFIG_PATH}:/opt/homebrew/opt/openblas/lib/pkgconfig"

    # LAPACK (keg-only)
    export LDFLAGS="${LDFLAGS} -L/opt/homebrew/opt/lapack/lib"
    export CPPFLAGS="${CPPFLAGS} -I/opt/homebrew/opt/lapack/include"
    export PKG_CONFIG_PATH="${PKG_CONFIG_PATH}:/opt/homebrew/opt/lapack/lib/pkgconfig"

    # OpenCL headers (keg-only)
    export CPPFLAGS="${CPPFLAGS} -I/opt/homebrew/opt/opencl-headers/include"

    # Use Homebrew's GCC for Fortran compatibility
    export CC="gcc-15"
    export CXX="g++-15"
    export FC="gfortran-15"

    echo "✅ macOS library paths configured"
else
    echo "🐧 Setting up Linux build environment..."
    export CC="gcc"
    export CXX="g++"
    export FC="gfortran"
    echo "✅ Linux build environment configured"
fi

# CMake configuration for LAPACK AI modernization
echo "⚙️  Setting up CMake configuration..."
export CMAKE_PREFIX_PATH="${CMAKE_PREFIX_PATH}:/opt/homebrew"
export CMAKE_BUILD_TYPE="Debug"

# Python development settings
echo "🐍 Configuring Python development settings..."
export PYTHONPATH="${PROJECT_ROOT}/src:${PYTHONPATH}"
export MYPYPATH="${PROJECT_ROOT}/src"

# GPU/OpenCL debugging
echo "🔧 Setting up GPU debugging..."
export OCL_ENABLE_DEBUG=1
export OCL_DEBUG_LEVEL=2

# Development quality settings
echo "📝 Setting up code quality tools..."
export PYTEST_ADDOPTS="--tb=short --strict-markers"
export BLACK_LINE_LENGTH=88
export ISORT_PROFILE="black"

# Performance monitoring
echo "📊 Setting up performance monitoring..."
export BENCHMARK_DISABLE_GC=1
export BENCHMARK_WARMUP=true

echo ""
echo "🎉 Development environment ready!"
echo "📍 Project root: $PROJECT_ROOT"
echo "🐍 Python: $(python --version)"
echo "🔨 CMake: $(cmake --version | head -n1)"
echo "🏗️  GCC: $(gcc --version | head -n1)"
echo "🔧 Fortran: $(gfortran --version | head -n1)"
echo ""
echo "🚀 Ready to start LAPACK modernization development!"
echo "💡 Run 'python -c \"import pyopencl; print('OpenCL available!')\"' to test GPU support"
