# LAPACK Modernization PRD (Revised with AlphaTensor)

## Project Overview
Modernize LAPACK (~1.5-2M lines of Fortran 90) for AI/ML data scientists. Transform legacy numerical library into GPU-accelerated, Python-friendly, cloud-ready solution with AlphaTensor's advanced matrix multiplication.

**Implementation Strategy**: Features are ordered by dependency to ensure stable foundation-first development. AI-driven development (Claude Code, Cursor) accelerates implementation by generating 80% of code, saving ~10 hours across the 5-day timeline.

## Target Users
- **Primary**: Data scientists & ML engineers in AI/ML pipelines
- **Use Cases**: Recommender systems, PCA, transformer models, 4×4 matrix operations
- **Pain Points**: No GPU support, complex Fortran interfaces, cryptic errors, difficult deployment
- **Market Opportunity**: Compete with cuSOLVER/MAGMA/SciPy via vendor-agnostic, Python-accessible solution

## Key Features
*Ordered by implementation dependency*

### F1: Enhanced Error Handling
- **Description**: Human-readable error messages and comprehensive diagnostics
- **Technical**: Map INFO codes to descriptive messages, DGECON diagnostics
- **Value**: 50% faster debugging, 95% error coverage
- **Dependency**: Foundation for stable debugging across all features

### F2: Python-Friendly API  
- **Description**: pybind11 wrapper with NumPy integration
- **Technical**: `lapack.svd()`, `lapack.solve()`, `lapack.dgemm_alpha()` functions
- **Value**: 80% reduction in setup complexity
- **Dependency**: Required for all GPU features and dashboard to be Python-accessible

### F3: GPU-Accelerated SVD
- **Description**: OpenCL-based SVD with CPU fallback via clBLAS
- **Technical**: `DGESVDOCL` routine using OpenCL SVD reductions
- **Value**: 5-10x speedup for PCA and dimensionality reduction
- **Dependency**: Establishes OpenCL toolchain and GPU infrastructure

### F4: AlphaTensor Matrix Multiplication
- **Description**: Advanced 4×4 matrix multiplication using AlphaTensor's algorithm
- **Technical**: `DGEMM_ALPHA` with 47 multiplications (vs standard 64) in Fortran/OpenCL
- **Value**: 10-20% speedup for 4×4 matrices, optimal for ML workloads
- **Dependency**: Reuses GPU infrastructure and patterns from F3

### F5: Real-time Performance Dashboard
- **Description**: Flask-based monitoring for GPU/CPU metrics and AlphaTensor performance
- **Technical**: psutil + pyopencl integration, key metrics focus
- **Value**: <5% runtime overhead, real-time optimization insights
- **Dependency**: Requires GPU metrics from F3/F4 to display meaningful data

### F6: Containerized Deployment
- **Description**: Docker container with all dependencies
- **Technical**: <500MB image with LAPACK + OpenBLAS + OpenCL + Python
- **Value**: One-command cloud deployment, vendor-agnostic
- **Dependency**: Packages the complete, tested feature stack

## Implementation Phases
*Aligned with feature dependency order and AI-accelerated development*

### Phase 1: Foundation & Analysis (Day 1)
- **Goals**: Analyze legacy systems, setup environment, study AlphaTensor
- **Tasks**: Map DGESVD/DGEMM dependencies, study AlphaTensor's 4×4 algorithm, install toolchain
- **Deliverables**: Codebase analysis, functional dev environment, AlphaTensor understanding
- **Features**: Environment setup, AlphaTensor algorithm analysis
- **AI Utilization**: Cursor for code navigation, Claude Code for architecture analysis

### Phase 2: Core Infrastructure (Day 2)  
- **Goals**: Build foundational APIs and error handling
- **Tasks**: Implement F1 (error mapping), create F2 (pybind11 bindings with DGEMM_ALPHA)
- **Deliverables**: Enhanced error framework, `lapack-py` module with core functions
- **Features**: F1 (Enhanced Error Handling), F2 (Python-Friendly API)
- **AI Utilization**: Claude Code for bindings and error logic, Cursor for NumPy integration

### Phase 3: GPU Foundation (Day 3)
- **Goals**: Implement OpenCL features with SVD and AlphaTensor
- **Tasks**: Build F3 (DGESVDOCL), implement F4 (DGEMM_ALPHA for 4×4 matrices)
- **Deliverables**: GPU-accelerated SVD, AlphaTensor matrix multiplication
- **Features**: F3 (GPU-Accelerated SVD), F4 (AlphaTensor Matrix Multiplication)
- **AI Utilization**: Claude Code for OpenCL kernels, Cursor for optimization

### Phase 4: Monitoring & Deployment (Day 4)
- **Goals**: Add dashboard and containerization
- **Tasks**: Build F5 (Flask dashboard), create F6 (Docker container)
- **Deliverables**: Performance monitoring, production container
- **Features**: F5 (Performance Dashboard), F6 (Containerized Deployment)
- **AI Utilization**: Claude Code for Flask templates, Cursor for Dockerfile

### Phase 5: Testing & Validation (Day 5)
- **Goals**: Validate all features and prepare demo
- **Tasks**: Accuracy testing (1e-6), performance benchmarks, AI utilization documentation
- **Deliverables**: Tested system, demo showcasing SVD speedup and AlphaTensor efficiency
- **Features**: Full system validation, demo preparation
- **AI Utilization**: Claude Code for test generation, comprehensive AI usage logging

## Success Metrics

### Performance Targets
- **SVD (F3)**: 5-10x speedup vs baseline DGESVD
- **AlphaTensor (F4)**: 10-20% speedup for 4×4 matrices vs standard DGEMM
- **Python Overhead (F2)**: <1% vs direct Fortran calls
- **Dashboard Overhead (F5)**: <5% runtime impact

### Usability Targets  
- **Setup Time (F2)**: 80% reduction via Python API
- **Debug Time (F1)**: 50% reduction via enhanced errors
- **Error Coverage (F1)**: 95% of common LAPACK error codes
- **Container Size (F6)**: <500MB for full deployment

### Quality Gates
- **Numerical Accuracy**: Within 1e-6 of reference implementation
- **Memory Safety**: No leaks or buffer overflows  
- **Cross-platform**: Linux/Windows/macOS compatibility
- **Cloud Ready**: AWS/GCP/Azure deployment tested
- **AI Integration**: Documented AI utilization for 80% of code generation

## Key Revisions from Original Plan

### AlphaTensor Integration
- **Replaces**: Batched GPU matrix multiplication
- **Focus**: 4×4 matrices with 47 multiplications (vs 64 standard)
- **Benefit**: 10-20% speedup, simplified implementation for 5-day timeline
- **Algorithm**: Based on AlphaTensor Page 12 (h_1 to h_47 decomposition)

### Scope Reductions
- **GPU**: Single-GPU OpenCL focus (vs multi-GPU)
- **Dashboard**: Key metrics only (vs comprehensive monitoring)
- **Testing**: Core accuracy and performance cases (vs exhaustive edge cases)
- **Matrix Size**: 4×4 focus for AlphaTensor (vs arbitrary batch sizes)

### AI-Driven Efficiency
- **Code Generation**: 80% of Fortran, OpenCL, Python, Flask code via AI
- **Time Savings**: ~10 hours across 5-day timeline
- **Documentation**: Comprehensive AI prompt/output logging for evaluation
- **Optimization**: AI-generated kernel optimizations and test cases

## Risk Mitigation
- **AlphaTensor Complexity**: Focus on 4×4 matrices only, reference implementation fallback
- **OpenCL Stability**: CPU fallback for all GPU features
- **Numerical Accuracy**: Extensive testing against reference (1e-6 tolerance)
- **Timeline Pressure**: AI acceleration for rapid development
- **Integration Risks**: Minimal changes to core LAPACK, clean abstraction layers 