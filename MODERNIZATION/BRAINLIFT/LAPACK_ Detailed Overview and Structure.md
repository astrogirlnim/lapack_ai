<img src="https://r2cdn.perplexity.ai/pplx-full-logo-primary-dark%402x.png" class="logo" width="120"/>

# LAPACK: Detailed Overview and Structure

## What Is LAPACK?

**LAPACK** (Linear Algebra PACKage) is a high-performance software library for numerical linear algebra. It provides a comprehensive suite of routines for solving many of the most common problems in linear algebra, including:

- Systems of linear equations
- Least-squares solutions of linear systems
- Eigenvalue problems
- Singular value decomposition (SVD)
- Associated matrix factorizations (LU, QR, Cholesky, Schur, and others)

Originally written in Fortran 77 and later migrated to Fortran 90, LAPACK is widely adopted in both academic research and industry. It is designed to be highly efficient and portable, relying on highly optimized implementations of the Basic Linear Algebra Subprograms (BLAS) for its computational core[^1_1][^1_2][^1_3].

LAPACK superseded earlier libraries like LINPACK and EISPACK, offering improved performance by optimizing for modern, cache-based computing architectures[^1_1][^1_2].

## Core Features

- Supports real and complex matrices, in both single and double precision
- Handles dense and banded matrices (not general sparse matrices)
- Extensible to distributed-memory architectures (e.g., ScaLAPACK)
- Freely available under a permissive BSD-style license[^1_1][^1_2][^1_3]


## Structure of LAPACK

LAPACK is organized into several layers, each fulfilling a distinct role in the library:

### 1. Routine Levels

LAPACK routines are grouped into three major categories:

- **Driver routines**: High-level routines that solve complete problems (e.g., solving a system of equations).
- **Computational routines**: Perform major computational tasks such as matrix factorizations (e.g., LU, QR).
- **Auxiliary routines**: Perform subtasks (such as error checks, or lower-level manipulations) in support of computational routines[^1_4][^1_5].


### 2. Naming Scheme

Each LAPACK routine has a standardized six-character name (occasionally five), encoding the data type, matrix type, and operation:

- **First character**: Data type
    - S = Single precision real
    - D = Double precision real
    - C = Single precision complex
    - Z = Double precision complex
- **Next two characters**: Matrix type (e.g., GE for general, SY for symmetric)
- **Last three characters**: Operation performed (e.g., SVX for expert driver, QRF for QR factorization)

**Example**: `DGESV` solves a system of linear equations with a general matrix in double precision[^1_4].

### 3. Use of BLAS

LAPACK is fundamentally built on top of BLAS, making calls to high-level matrix operations (especially Level 3 BLAS, i.e., operations involving matrix-matrix multiplication and solution of triangular systems with multiple right-hand sides). BLAS provides the performance-critical building blocks, allowing LAPACK to achieve high efficiency across different hardware architectures, so long as a tuned BLAS is available[^1_1][^1_2][^1_6].

### 4. Data Types and Matrix Formats

LAPACK supports multiple data and matrix types:

- **Data types**: Single/double precision, real/complex
- **Matrix formats**: Dense, banded, packed, tridiagonal, triangular, and others[^1_2][^1_7]


### 5. Modular Structure

LAPACK is modular, with files and routines arranged according to problem type (e.g., solvers for linear systems, eigenproblems), matrix type, and supported data type and precision. For each high-level mathematical operation, there are often specialized routines tailored to the properties of specific matrices (e.g., positive-definite, banded)[^1_2][^1_7].

## Extensibility and Use

- LAPACK forms the basis for many higher-level mathematical environments and libraries (MATLAB, SciPy, Intel MKL, etc.)[^1_8].
- Parallel/distributed extensions are available (e.g., ScaLAPACK for distributed memory, PLAPACK).
- Widely used in scientific computing, engineering, and data science.


## Summary Table: LAPACK Structure

| Routine Type | Function | Example |
| :-- | :-- | :-- |
| Driver routines | Complete problem solvers | DGESV |
| Computational routines | Matrix factorizations, eigen, SVD, etc. | DGETRF |
| Auxiliary routines | Subtasks, low-level operations | DLASWP |

## Conclusion

LAPACK is a foundational software library for numerical linear algebra, renowned for its robust algorithms and efficient use of modern hardware. Its structured organization, flexible naming scheme, and reliance on BLAS make it both powerful and adaptable in a wide array of scientific and engineering applications[^1_1][^1_2][^1_4].

[^1_1] https://en.wikipedia.org/wiki/LAPACK
[^1_2] https://www.netlib.org/lapack/
[^1_3] https://hpc.llnl.gov/software/mathematical-software/lapack
[^1_8] https://www.mathworks.com/help/matlab/math/lapack-in-matlab.html
[^1_4] http://www.hpc.lsu.edu/training/weekly-materials/Past Tutorials/Intro-LAPACK-0309.pdf
[^1_6] https://www.netlib.org/lapack/old_website/
[^1_5] http://www.cs.ox.ac.uk/janice.giudice/files/lapack
[^1_7] https://icl.utk.edu/graphics/posters/files/Sca-LAPACK-2008.pdf

<div style="text-align: center">⁂</div>

[^1_1]: https://en.wikipedia.org/wiki/LAPACK

[^1_2]: https://www.netlib.org/lapack/

[^1_3]: https://hpc.llnl.gov/software/mathematical-software/lapack

[^1_4]: http://www.hpc.lsu.edu/training/weekly-materials/Past Tutorials/Intro-LAPACK-0309.pdf

[^1_5]: http://www.cs.ox.ac.uk/janice.giudice/files/lapack

[^1_6]: https://www.netlib.org/lapack/old_website/

[^1_7]: https://icl.utk.edu/graphics/posters/files/Sca-LAPACK-2008.pdf

[^1_8]: https://www.mathworks.com/help/matlab/math/lapack-in-matlab.html

[^1_9]: https://en.wikipedia.org/wiki/Lapack

[^1_10]: https://www2.nau.edu/stat-lic/statlibs/lapack.html

[^1_11]: https://nrel.github.io/HPC/Documentation/Development/Libraries/lapack/

[^1_12]: https://www.netlib.org/lapack/explore-html/

[^1_13]: https://gist.github.com/rgommers/e10c7cf3ebd88883458544e535d7e54c

[^1_14]: https://web.stanford.edu/class/me200c/tutorial_77/18.2_lapack.html

[^1_15]: https://www.netlib.org/lapack/lug/node21.html

[^1_16]: https://www.osc.edu/resources/available_software/software_list/lapack

[^1_17]: https://www.sc.fsu.edu/computing/tech-docs/342-lapack

[^1_18]: https://www.netlib.org/lapack/lug/node24.html

[^1_19]: https://www.intel.com/content/www/us/en/docs/onemkl/developer-reference-c/2025-0/lapack-routines.html

[^1_20]: https://hpc.nih.gov/development/LAPACK.html


---

## Software That Uses LAPACK

Many prominent scientific and engineering software packages and libraries rely on LAPACK for their numerical linear algebra capabilities. Here’s a list of widely used software and environments that depend on LAPACK, either directly or through bindings:

- **MATLAB and GNU Octave**: MATLAB’s core matrix operations and solvers are built on LAPACK routines, enabling fast solutions for linear systems, eigenproblems, and decompositions.
- **NumPy/SciPy (Python)**: These Python libraries use LAPACK (often via wrappers) for array operations involving linear algebra, eigendecomposition, and singular value decomposition.
- **R**: The statistical computing environment utilizes LAPACK for core operations like regression, PCA, SVD, and matrix factorization.
- **Intel Math Kernel Library (MKL) and AMD Core Math Library (ACML)**: Provide highly optimized implementations and interfaces to LAPACK routines.
- **Eigen (C++)**: Though it has its own algorithms, Eigen can interface with LAPACK for certain advanced operations.
- **LAPACK++**: A C++ wrapper and interface for LAPACK functionalities[^2_1][^2_2].
- **Virtual Client and benchmarking suites**: Use LAPACK to measure compute performance for various matrix and numerical routines[^2_3].
- **High Performance Computing (HPC) applications**: Many domain-specific and engineering simulation tools link LAPACK directly for linear solvers.
- **Database environments**: Some database engines (e.g., via aggregate user-defined functions in SQL or stored procedures) implement advanced analytics by invoking LAPACK routines for things like large-scale PCA and SVD[^2_4].
- **Other technical and engineering applications**: Finite element analysis (FEA), signal processing, and computational physics codes often link directly to LAPACK for problem-specific solvers[^2_1][^2_2].


## Use Cases Where LAPACK is the Endpoint of Data Pipelines

LAPACK is directly used as the computational endpoint in numerous applied scientific, data analytics, and engineering scenarios, especially when the workflow culminates in a core numerical linear algebra operation. Common use cases include:

### 1. Solving Linear Systems and Least Squares

- **Scientific computing workflows:** Simulations of physical systems, engineering analyses, or fitting mathematical models often prepare input matrices, then finish by calling LAPACK solvers (e.g., `DGESV`, `DGELS`) for the central calculation[^2_1][^2_5].
- **Calibration and parameter estimation in engineering:** Experimental data feeds into a pipeline, culminating in linear least-squares fitting, where LAPACK provides the numerical solution.


### 2. Eigenvalue and Eigenvector Computation

- **Principal Component Analysis (PCA) and dimensionality reduction:** Preprocessed data sets are passed to LAPACK’s eigenvalue/SVD routines to extract dominant components or features, often in analytics or statistical databases[^2_4].
- **Spectral analysis:** Physics, chemistry, and engineering codes may preprocess sensor or simulation data, then compute eigenmodes directly via LAPACK SVD/eigenvalue routines.


### 3. Singular Value Decomposition (SVD) and Matrix Decomposition

- **Recommender systems and latent factor models:** After matrix construction and normalization, final computation of factors is performed by LAPACK SVD routines (`DGESVD`).
- **Noise reduction and image processing:** Prepared data (e.g., denoised, normalized images) is directly factorized using LAPACK at the end of the processing pipeline.


### 4. Condition Number Estimation and Matrix Diagnostics

- **Pipeline for matrix testing, validation, or optimization:** Once the matrix is assembled (possibly from large-scale computations or data reduction), LAPACK routines such as `DGECON` are called to assess conditioning, which determines next steps in optimization or regularization[^2_5].


### 5. Integrated in Database Analytics

- **In-database analytics:** Aggregate user-defined functions (UDFs) collect and summarize data, and LAPACK is invoked via stored procedures or computational extensions to compute PCA, SVD, or solve equations directly within the database—ending the pipeline at LAPACK[^2_4].


## Summary Table

| Software/Environment | LAPACK Role | Example Use Case |
| :-- | :-- | :-- |
| MATLAB, Octave | Backend for matrix ops | Linear regression, eigensolvers |
| NumPy/SciPy (Python) | High-level API, calls LAPACK | Data analysis, scientific modeling |
| R | Internal computations | PCA, regression |
| Intel MKL, AMD ACML | Distribution + optimization | HPC, large-scale simulation |
| Database engines (UDFs) | Called from stored procedures | In-database analytics: PCA, SVD |
| Custom engineering codes | Direct calls in C/Fortran/C++ | Finite element analysis, control system design |

LAPACK’s mature, robust routines make it a cornerstone of reliable linear algebra in scientific, analytical, and engineering data pipelines, frequently representing the final step in transforming, modeling, or extracting structure from large data sets[^2_1][^2_2][^2_4].

<div style="text-align: center">⁂</div>

[^2_1]: https://en.wikipedia.org/wiki/LAPACK

[^2_2]: https://math.nist.gov/lapack++/index.html

[^2_3]: https://microsoft.github.io/VirtualClient/docs/workloads/lapack/

[^2_4]: https://www2.cs.uh.edu/~ordonez/pdfwww/w-2012-CIKM-udfsvd.pdf

[^2_5]: https://faculty.washington.edu/rjl/classes/am583s2014/notes/lapack_examples.html

[^2_6]: https://www.netlib.org/lapack/

[^2_7]: https://docs.hpc.udel.edu/software/lapack/lapack

[^2_8]: https://en.wikipedia.org/wiki/Lapack

[^2_9]: https://scicomp.stackexchange.com/questions/26395/how-to-start-using-lapack-in-c

[^2_10]: https://arxiv.org/pdf/1610.08705.pdf

[^2_11]: https://docs.nersc.gov/development/libraries/lapack/

[^2_12]: https://stackoverflow.com/questions/10112135/understanding-lapack-calls-in-c-with-a-simple-example

[^2_13]: https://www.netlib.org/lapack/lawnspdf/lawn58.pdf

[^2_14]: https://resources.tasking.com/p/using-lapack-can-improve-efficiency-embedded-software

[^2_15]: https://www.intel.com/content/www/us/en/docs/onemkl/developer-guide-linux/2024-2/call-lapack-blas-and-cblas-from-c-language-env.html

[^2_16]: https://stackoverflow.com/questions/31186750/why-does-this-lapack-program-work-correctly-when-i-provide-the-matrix-directly

[^2_17]: https://www.netlib.org/lapack/lug/node18.html

[^2_18]: https://www.physics.utah.edu/~detar/phys6720/handouts/lapack.html

[^2_19]: https://arxiv.org/abs/1610.08705

[^2_20]: https://hpc.llnl.gov/software/mathematical-software/lapack


---

## Modernizing LAPACK: At Least 10 Practical Strategies

Given your interest in computational science and linear algebra libraries, here are concrete, high-impact ways you can modernize LAPACK:

### 1. Adopt Modern Fortran Standards

- Refactor LAPACK codebase to free-form Fortran 90/2008.
- Use explicit typing and modern array constructs.
- Eliminate obsolescent control flow and non-standard features[^3_1].


### 2. Improve Language Interoperability

- Develop comprehensive, well-tested C, C++, and Python bindings.
- Standardize C interfaces (e.g., LAPACKE) and ensure ABI compatibility[^3_2].
- Explore GPU/CUDA support and integrations for languages like Julia and Rust[^3_3][^3_4].


### 3. Expand Parallelism and Heterogeneity Support

- Enable efficient multi-core and many-core CPU parallelism via OpenMP, MPI, or task-based runtimes.
- Add support for heterogeneous architectures (GPUs, FPGAs, cloud)[^3_3][^3_5].
- Integrate with distributed-memory extensions such as ScaLAPACK.


### 4. Enhance Performance and Auto-Tuning

- Incorporate automated performance tuning tools to optimize block sizes and thresholds for new architectures[^3_5].
- Leverage performance models to select optimal algorithms at runtime.
- Support mixed-precision and higher/lower precision numerical kernels for emerging applications[^3_3].


### 5. Modernize and Modularize the Build System

- Adopt CMake or Meson for flexible, cross-platform builds.
- Provide pre-built binaries and easy packaging for common platforms[^3_6].
- Organize code into well-defined, reusable modules[^3_7].


### 6. Strengthen Testing, CI, and Quality Assurance

- Set up robust CI pipelines for multi-platform, multi-language automated testing[^3_6].
- Increase test coverage, including numerical accuracy and regression tests.


### 7. Expand Functionality and Algorithms

- Implement new factorizations, eigenvalue solvers, and iterative methods based on recent research.
- Provide explicit routines for matrix inversion, condition estimation, and extended support for banded/sparse formats[^3_2][^3_3].
- Support updating/downdating of factorizations and new eigenproblems[^3_2].


### 8. Improve Documentation and Developer Resources

- Develop in-depth, accessible online documentation and API guides.
- Include migration guides and examples for using LAPACK in modern computing workflows[^3_7].


### 9. Optimize Memory Hierarchy and Data Layouts

- Redesign routines and drivers to automatically select optimal in-memory matrix layouts.
- Provide routines for layout conversion and efficient data movement on modern hardware[^3_5].
- Enable seamless operation on hierarchical and distributed storage[^3_3].


### 10. Boost Ease of Use and Accessibility

- Provide higher-level interfaces and APIs, making LAPACK more user-friendly for scientists and AI developers.
- Offer ready-to-use containerized deployments (e.g., Docker, Singularity).
- Engage the user and developer community for roadmap input and contributions[^3_3].


### 11. Future-Proof and Ensure Sustainability

- Create contributor guidelines and update program style documentation[^3_7].
- Maintain backward compatibility while facilitating migration paths to new APIs where possible[^3_8].
- Participate in or lead cross-disciplinary collaborations to keep the library relevant to modern scientific computing needs[^3_3].

By implementing these modernization steps, you can help ensure that LAPACK remains a top-tier library for computational science, suitable for advanced AI development, large-scale simulation, and data-driven research[^3_1][^3_2][^3_3][^3_5].

<div style="text-align: center">⁂</div>

[^3_1]: https://fortran-lang.discourse.group/t/resistance-to-modernization/4136

[^3_2]: https://www.maths.ox.ac.uk/node/8956

[^3_3]: https://www.stat.berkeley.edu/~mmahoney/pubs/lawn297.pdf

[^3_4]: https://news.ycombinator.com/item?id=16257216

[^3_5]: https://www.netlib.org/lapack/lawnspdf/lawn181.pdf

[^3_6]: https://www.netlib.org/lapack/improvement.html

[^3_7]: https://www.netlib.org/lapack-dev/lapack-coding/program-style.html

[^3_8]: https://docs.oracle.com/cd/E77782_01/html/E77802/z400049317985.html

[^3_9]: https://www.netlib.org/lapack/

[^3_10]: https://csantill.github.io/RPerformanceWBLAS/

[^3_11]: https://github.com/bmad-sim/bmad-external-packages/issues/5

[^3_12]: https://resources.tasking.com/p/using-lapack-can-improve-efficiency-embedded-software

[^3_13]: https://labs.quansight.org/blog/lapack-in-stdlib

[^3_14]: https://stackoverflow.com/questions/33895423/what-is-high-performance-version-of-lapack-and-blas

[^3_15]: https://github.com/stdlib-js/google-summer-of-code/issues/95

[^3_16]: https://github.com/scipy/scipy/issues/17244

[^3_17]: https://web.cels.anl.gov/projects/petsc/vault/petsc-3.17/docs/docs/manual/blas-lapack.html

[^3_18]: https://github.com/perazz/fortran-lapack

[^3_19]: https://www.mathworks.com/company/technical-articles/matlab-incorporates-lapack.html

[^3_20]: https://www.netlib.org/lapack/faq.html


---

## Most Requested Unimplemented Features in LAPACK

As someone with a deep interest in computational science and linear algebra libraries, you'll appreciate how LAPACK has continually evolved—yet the community often highlights persistent gaps and new demands. Based on user forums, release notes, and ongoing discussions, here are prominent requested features that remain either outstanding or only partially addressed:

### 1. Native Support for Sparse Matrices

- LAPACK focuses on dense and banded matrices; general sparse matrix routines are still not available, although they are critical in scientific computing and large-scale simulation[^4_1][^4_2].


### 2. GPU and Accelerator Integration

- Direct support for GPUs, FPGAs, or heterogeneous architectures is missing. Users want seamless acceleration with CUDA, HIP, or SYCL for modern hardware environments[^4_3].


### 3. Modern Language Interfaces and Bindings

- First-class, native bindings for Python, Julia, and Rust.
- Easier, better-documented C/C++ interfaces (beyond LAPACKE), with robust ABI compatibility[^4_3].


### 4. Mixed-Precision and Arbitrary Precision Arithmetic

- Built-in support for mixed-precision algorithms or arbitrary/higher/lower floating-point precision, which are increasingly important for AI and HPC applications[^4_3][^4_4].


### 5. Asynchronous and Batched/Streamed Operations

- Routines that allow for non-blocking, batched, or streamed computations—especially useful in data pipelines and parallel simulation[^4_3].


### 6. Automatic Performance Tuning Tools

- Tools for auto-tuning block sizes, algorithm choices, and thresholds to optimize performance for any given hardware without manual intervention[^4_4].


### 7. Dynamic Memory Management and Workspace Allocation

- High-level APIs that handle workspace memory automatically or use modern dynamic allocation, reducing the burden of managing parameters such as `WORK` arrays[^4_5].


### 8. Enhanced Error Handling and Diagnostics

- More informative error messages, exception hierarchies, and runtime diagnostics rather than cryptic error codes.


### 9. Improved Algorithms \& Functional Expansion

- Newer, highly accurate algorithms for decomposition, eigenproblems, or iterative refinement.
- Routines for explicit matrix inversion and broader condition number estimation.
- Native support for updating/downdating of matrix factorizations and additional eigenproblems[^4_4].


### 10. Comprehensive, User-Friendly Documentation \& Tutorials

- Modernized documentation with beginner-friendly guides, migration advice, and practical code examples in multiple programming languages.


### 11. Advanced Matrix Layout and Storage Options

- Native handling for advanced matrix layouts (e.g., hierarchical, tile, and block-sparse formats).
- Built-in conversion routines for interoperability with modern distributed and storage-efficient formats[^4_4].


### 12. Robust Continuous Integration \& Testing Infrastructure

- Expanded automated testing, platform coverage, and CI pipelines to quickly catch numerical or compatibility issues in diverse environments.

**Summary Table: Key Unimplemented LAPACK Features**


| Feature | Description |
| :-- | :-- |
| Sparse matrix support | Efficient routines for arbitrary sparse matrices |
| GPU/Accelerator integration | Direct kernels for GPUs, FPGAs, and heterogenous hardware |
| Modern language interfaces | High-level bindings for Python, Julia, Rust, modern C++ |
| Mixed/arbitrary precision arithmetic | Built-in support for mixed and non-standard precision |
| Asynchronous/batched routines | Non-blocking and batched interface options |
| Auto-tuning/performance tooling | Automated hardware adaptation and tuning utilities |
| Dynamic memory management | Eliminate manual workspace parameters in high-level APIs |
| Enhanced diagnostics/error handling | More helpful error and exception handling |
| Expanded and modernized algorithms | New numerical methods and updated factorizations |
| Improved documentation | Practical, accessible, and modern tutorials and guides |

These gaps represent top opportunities if you're modernizing LAPACK or building new linear algebra tools for research and application development[^4_1][^4_3][^4_4][^4_5].

[^4_1] https://www.netlib.org/lapack/
[^4_3] https://fortran-lang.discourse.group/t/resistance-to-modernization/4136
[^4_4] https://www.stat.berkeley.edu/~mmahoney/pubs/lawn297.pdf
[^4_2] https://resources.tasking.com/p/using-lapack-can-improve-efficiency-embedded-software
[^4_5] https://stackoverflow.com/questions/46618391/what-is-the-use-of-the-work-parameters-in-lapack-routines

<div style="text-align: center">⁂</div>

[^4_1]: https://www.netlib.org/lapack/

[^4_2]: https://resources.tasking.com/p/using-lapack-can-improve-efficiency-embedded-software

[^4_3]: https://www.netlib.org/lapack/faq.html

[^4_4]: https://hpc.llnl.gov/software/mathematical-software/lapack

[^4_5]: https://stackoverflow.com/questions/46618391/what-is-the-use-of-the-work-parameters-in-lapack-routines

[^4_6]: https://hpc.nih.gov/development/LAPACK.html

[^4_7]: https://www.intel.com/content/www/us/en/docs/onemkl/developer-reference-fortran/2024-2/lapack-utility-functions-and-routines.html

[^4_8]: https://en.wikipedia.org/wiki/LAPACK

[^4_9]: https://github.com/Reference-LAPACK/lapack/pulls

[^4_10]: https://github.com/igraph/igraph/issues/1491

[^4_11]: https://www.reddit.com/r/Cplusplus/comments/1ykz85/does_anyone_have_a_good_reference_on_how_to_use/

[^4_12]: https://icl.utk.edu/lapack-forum/viewforum.php%3Ff=3.html

[^4_13]: https://github.com/chapel-lang/chapel/issues/10251

[^4_14]: https://stackoverflow.com/questions/8961340/blas-and-lapack-libraries-required-for-compiling

[^4_15]: https://scicomp.stackexchange.com/questions/26395/how-to-start-using-lapack-in-c

[^4_16]: https://cvxopt.org/userguide/lapack.html

[^4_17]: https://fortran-lang.discourse.group/t/lapack-interfaces/2462?page=3

[^4_18]: https://github.com/kivy/python-for-android/issues/2824

[^4_19]: https://cdrdv2-public.intel.com/834387/onemkl_developer-reference-dpcpp_2025.0-772045-834387.pdf

[^4_20]: https://docs.rcc.fsu.edu/software/lapack/


---

## LAPACK’s Fortran Version

**LAPACK** was originally written in **Fortran 77**. In version 3.2 (released in 2008), the codebase was migrated to Fortran 90, which enabled the use of free-form source, more explicit typing, and other modern language features. As of recent releases, LAPACK is primarily maintained in **Fortran 90**.

- **Older versions:** Fortran 77
- **Modern versions (3.2+):** Fortran 90


## Is Fortran 90/2008 the Newest Version?

No, **Fortran 2008** is not the newest version of the language. While many scientific codes—including LAPACK—are maintained in Fortran 90 or with some Fortran 2008 features, the Fortran standards committee has continued to release new versions:


| Fortran Version | Year | Key Additions |
| :-- | :-- | :-- |
| Fortran 77 | 1978 | Structured programming |
| Fortran 90 | 1991 | Free-form source, modules |
| Fortran 95 | 1997 | Minor improvements |
| Fortran 2003 | 2004 | Object-oriented programming |
| Fortran 2008 | 2010 | Coarrays, parallel features |
| Fortran 2018 | 2018 | Further enhancements |

**The most recent published version is Fortran 2018.** LAPACK itself is not yet written with Fortran 2003, 2008, or 2018 features as its main standard; its main body is in Fortran 90.

**In summary:**

- LAPACK is written and maintained in Fortran 90 (originally Fortran 77).
- Fortran 90/2008 is not the newest version; Fortran 2018 is the latest official standard.

https://en.wikipedia.org/wiki/LAPACK
https://www.netlib.org/lapack/

---

# Differences Between Fortran 2008 and Fortran 2018

Fortran 2018 is a relatively minor evolution of the language compared to previous updates, building directly upon Fortran 2008. The differences are mostly focused on enhancing parallelism, interoperability with C, and minor corrections or clarifications to previous standards.

## Key New Features in Fortran 2018

### 1. Parallel Programming Enhancements

- **Coarray Teams:** Fortran 2018 introduces "teams" for coarrays, greatly improving support for parallel programming. This includes new statements (`TEAM_TYPE`, `CHANGE TEAM`, `END TEAM`, `FORM TEAM`, `SYNC TEAM`), intrinsic functions like `TEAM_NUMBER()` and `GET_TEAM()`[^6_1][^6_2][^6_3][^6_4].
- **Additional Parallel Intrinsics:** Improved DO CONCURRENT loop locality features and team-based communication for coarrays.


### 2. Improved C Interoperability

- Expanded support for interoperating with C, integrated from Technical Specification ISO/IEC TS 29113. This allows more flexible procedure interfaces and is especially useful for high-performance computing and library development[^6_5][^6_2][^6_3].


### 3. Floating-Point Enhancements

- Updates aligning with newer IEEE standards for floating-point arithmetic.
- Enhanced IEEE modules, supporting improved compatibility and precision control[^6_2][^6_3].


### 4. Syntax and Usability Improvements

- Enhancements to existing syntax, such as allowing the `GENERIC` statement to declare interfaces, making code more maintainable and user-friendly.
- More flexible `IMPLICIT NONE` usage to ensure external procedures are declared explicitly.
- Improvements to the formatting and edit descriptors for input/output statements (e.g., field width of zero, rounding directives)[^6_4].


### 5. Minor Corrections and Clarifications

- Many small changes address deficiencies or inconsistencies found in Fortran 2008, streamlining the language for consistency and reliability[^6_2][^6_3].


## Summary Table: Major Differences

| Area | Fortran 2008 | Fortran 2018 |
| :-- | :-- | :-- |
| Parallelism | Coarrays (basic) | Coarray teams, more parallel intrinsics |
| C Interoperability | ISO_C_BINDING standard | Further expanded, TS 29113 included |
| Floating Point | IEEE modules (2003/2008) | Updated for IEEE 2011 standard |
| Syntax Improvements | Moderately improved | Additional syntax for GENERIC, IMPLICIT NONE |
| Formatting/IO | Prior edit descriptors | More flexible edit descriptors and rounding |
| Corrections/Clarifications | Fortran 2003 → 2008 changes | Further streamlining and minor improvements |

## In Context

Most users will find Fortran 2018 to be very similar to Fortran 2008 for everyday tasks. The main advantages come in high-performance, parallel, or interoperable programming scenarios where the new team-based coarray features and extended C interoperability can have a significant impact. Adoption of Fortran 2018 features depends on compiler support, but major compilers are already compliant or close to it[^6_5][^6_1][^6_2][^6_4].

[^6_5] https://fortranwiki.org/fortran/show/Fortran+2018
[^6_1] https://www.intel.com/content/www/us/en/developer/articles/technical/fortran-compiler-support-for-fortran-language-standards.html
[^6_2] https://wg5-fortran.org/N2151-N2200/N2161.pdf
[^6_3] https://wg5-fortran.org/N2151-N2200/ISO-IECJTC1-SC22-WG5_N2161_The_New_Features_of_Fortran_2018.pdf
[^6_4] https://www.intel.com/content/www/us/en/docs/fortran-compiler/developer-guide-reference/2023-2/conformance-compatibility-and-fortran-features.html

<div style="text-align: center">⁂</div>

[^6_1]: https://www.intel.com/content/www/us/en/developer/articles/technical/fortran-compiler-support-for-fortran-language-standards.html

[^6_2]: https://wg5-fortran.org/N2151-N2200/N2161.pdf

[^6_3]: https://wg5-fortran.org/N2151-N2200/ISO-IECJTC1-SC22-WG5_N2161_The_New_Features_of_Fortran_2018.pdf

[^6_4]: https://www.intel.com/content/www/us/en/docs/fortran-compiler/developer-guide-reference/2023-2/conformance-compatibility-and-fortran-features.html

[^6_5]: https://fortranwiki.org/fortran/show/Fortran+2018

[^6_6]: https://www.reddit.com/r/fortran/comments/187ohwh/what_version_of_fortran_should_i_learn/

[^6_7]: https://forums.developer.nvidia.com/t/fortran-2008-2018-support/164306

[^6_8]: https://www.admin-magazine.com/HPC/Articles/Modern-Fortran-Part-3

[^6_9]: https://fortran-lang.discourse.group/t/whats-the-best-version-of-fortran-to-use/7192

[^6_10]: https://fortran-lang.discourse.group/t/should-i-learn-fortran-2003-or-2008-before-learning-fortran-2018/337


---

## Fortran 2018 Backward Compatibility with 2008

### High-Level Overview

**Fortran 2018** is designed as an incremental update to **Fortran 2008**, and it retains strong backward compatibility with 2008. The intent of the Fortran standards committee was to ensure that code written in Fortran 2008 would, with very rare exceptions, also compile and run unchanged under a conforming Fortran 2018 compiler.

### What Does This Mean?

- Most valid Fortran 2008 source code will work without modification in Fortran 2018 environments.
- The new features added in Fortran 2018 do not break or redefine existing Fortran 2008 language features.
- Any incompatibilities are extremely rare and focus on clarifications or corrections to ambiguous situations, not typical code usage.


### Practical Perspective

- All major Fortran compilers that support Fortran 2018 strive for full compatibility with Fortran 2008 applications.
- Fortran 2018 adds features (such as expanded parallelism and C interoperability), but leaves the Fortran 2008 core language intact.


### Summary Table

| Fortran 2008 Code | Fortran 2018 Compiler |
| :-- | :-- |
| Should compile \& run | Yes |
| Feature removals? | No |
| Breaking changes? | None by design |

### Conclusion

**Fortran 2018 is backward compatible with Fortran 2008.** Upgrading to Fortran 2018 should not break existing Fortran 2008 code for the vast majority of projects. The evolution is mostly additive and clarificatory, ensuring a stable path forward for Fortran developers.

https://fortranwiki.org/fortran/show/Fortran+2018
https://wg5-fortran.org/N2151-N2200/N2161.pdf
https://www.intel.com/content/www/us/en/developer/articles/technical/fortran-compiler-support-for-fortran-language-standards.html

