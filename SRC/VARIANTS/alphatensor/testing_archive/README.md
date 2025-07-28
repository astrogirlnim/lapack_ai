# AlphaTensor Testing Archive

This directory contains historical testing files and performance reports from the AlphaTensor implementation development.

## Directory Structure

### 📊 `reports/`
Performance reports and analysis from different optimization phases:
- `phase8_6_complete_report.txt` - Final Phase 8.6 Strassen-AlphaTensor hybrid results
- `ultimate_multi_size_alphatensor_report.txt` - Comprehensive multi-size performance analysis
- `corrected_multi_size_alphatensor_report.txt` - Corrected performance measurements
- `alphatensor_optimization_report.txt` - Optimization progression report
- `all_correct_c_mappings.txt` - Coefficient mapping validation results

### 🏃 `benchmarks/`
Performance benchmark test files:
- `phase8_1_benchmark.f` - Phase 8.1 memory optimization benchmarks
- `phase8_6_complete_benchmark.f` - Complete Phase 8.6 hybrid benchmarks  
- `optimization_benchmark.f` - General optimization performance tests
- `benchmark_dgemm_alpha.f` - Core AlphaTensor vs DGEMM benchmarks
- `speed_benchmark.f` - Speed comparison tests
- `realistic_benchmark.f` - Real-world performance scenarios

### 🧪 `tests/`
Functional test files:
- `comprehensive_test.f` - Complete accuracy validation
- `strassen_test.f` - Strassen-AlphaTensor hybrid testing
- `mixed_precision_test.f` - Mixed precision optimization testing
- `robustness_test.f` - Algorithm robustness validation
- `comprehensive_performance_test_fixed.f` - Fixed performance tests

### 🗄️ `obsolete/`
Legacy and debugging files:
- `dgemm_alpha_legacy.f` - Original implementation (33KB)
- `debug_test.f` - Temporary debugging code
- `failure_recreation.f` - Error reproduction test

## Historical Context

This archive documents the complete evolution of the AlphaTensor implementation from initial algorithm research through Phase 8.6 Strassen-AlphaTensor hybrid completion. The performance reports show the progression from 14.2% slower than DGEMM to achieving significant speedups through systematic optimization.

## Key Achievements Documented

- **Phase 8.1**: Memory access pattern optimization
- **Phase 8.2**: SIMD vectorization (42% improvement)
- **Phase 8.3**: Function call overhead elimination
- **Phase 8.4**: Common subexpression elimination (27% speedup in optimal conditions)
- **Phase 8.5**: Compiler-specific optimization (2.137x peak speedup)
- **Phase 8.6**: Strassen-AlphaTensor hybrid (world's first implementation)

All files preserved for historical reference and future optimization research. 
