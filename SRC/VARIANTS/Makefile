#######################################################################
#  This is the makefile to create a the variants libraries for LAPACK.
#  The files are organized as follows:
#       CHOLRL -- Right looking block version of the algorithm, calling Level 3 BLAS
#       CHOLTOP -- Top looking block version of the algorithm, calling Level 3 BLAS
#       LUCR -- Crout Level 3 BLAS version of LU factorization
#       LULL -- left-looking Level 3 BLAS version of LU factorization
#       QRLL -- left-looking Level 3 BLAS version of QR factorization
#       LUREC -- an iterative version of Sivan Toledo's recursive LU algorithm[1].
#       For square matrices, this iterative versions should
#       be within a factor of two of the optimum number of memory transfers.
#       ALPHATENSOR -- AlphaTensor optimized 4x4 matrix multiplication using DeepMind's
#       49-operation algorithm[2]. Provides 24% operation reduction vs standard DGEMM.
#
# [1] Toledo, S. 1997. Locality of Reference in LU Decomposition with
#  Partial Pivoting. SIAM J. Matrix Anal. Appl. 18, 4 (Oct. 1997),
#  1065-1081. http://dx.doi.org/10.1137/S0895479896297744
# [2] Fawzi, A. et al. Discovering faster matrix multiplication algorithms
#  with reinforcement learning. Nature 610, 47-53 (2022).
#######################################################################

TOPSRCDIR = ../..
include $(TOPSRCDIR)/make.inc

# OpenCL compilation flags (conditional)
OPENCL_CFLAGS = $(shell pkg-config --cflags OpenCL 2>/dev/null || echo "-I/System/Library/Frameworks/OpenCL.framework/Headers")
OPENCL_LIBS = $(shell pkg-config --libs OpenCL 2>/dev/null || echo "-framework OpenCL")

CHOLRL = cholesky/RL/cpotrf.o cholesky/RL/dpotrf.o cholesky/RL/spotrf.o cholesky/RL/zpotrf.o

CHOLTOP = cholesky/TOP/cpotrf.o cholesky/TOP/dpotrf.o cholesky/TOP/spotrf.o cholesky/TOP/zpotrf.o

LUCR = lu/CR/cgetrf.o lu/CR/dgetrf.o lu/CR/sgetrf.o lu/CR/zgetrf.o

LULL = lu/LL/cgetrf.o lu/LL/dgetrf.o lu/LL/sgetrf.o lu/LL/zgetrf.o

LUREC = lu/REC/cgetrf.o lu/REC/dgetrf.o lu/REC/sgetrf.o lu/REC/zgetrf.o

QRLL = qr/LL/cgeqrf.o qr/LL/dgeqrf.o qr/LL/sgeqrf.o qr/LL/zgeqrf.o

LARFTL2 = larft/LL-LVL2/clarft.o larft/LL-LVL2/dlarft.o larft/LL-LVL2/slarft.o larft/LL-LVL2/zlarft.o

ALPHATENSOR = alphatensor/dgemm_alpha.o

ALPHATENSOR_HYBRID = alphatensor_hybrid/dgemm_alpha_hybrid.o \
                     alphatensor_hybrid/opencl_manager.o \
                     alphatensor_hybrid/gpu_interface.o \
                     alphatensor/dgemm_alpha.o


.PHONY: all
all: cholrl.a choltop.a lucr.a lull.a lurec.a qrll.a larftl2.a alphatensor.a alphatensor_hybrid.a

cholrl.a: $(CHOLRL)
	$(AR) $(ARFLAGS) $@ $^
	$(RANLIB) $@

choltop.a: $(CHOLTOP)
	$(AR) $(ARFLAGS) $@ $^
	$(RANLIB) $@

lucr.a: $(LUCR)
	$(AR) $(ARFLAGS) $@ $^
	$(RANLIB) $@

lull.a: $(LULL)
	$(AR) $(ARFLAGS) $@ $^
	$(RANLIB) $@

lurec.a: $(LUREC)
	$(AR) $(ARFLAGS) $@ $^
	$(RANLIB) $@

qrll.a: $(QRLL)
	$(AR) $(ARFLAGS) $@ $^
	$(RANLIB) $@

larftl2.a: $(LARFTL2)
	$(AR) $(ARFLAGS) $@ $^
	$(RANLIB) $@

alphatensor.a: $(ALPHATENSOR)
	$(AR) $(ARFLAGS) $@ $^
	$(RANLIB) $@

alphatensor_hybrid.a: $(ALPHATENSOR_HYBRID)
	$(AR) $(ARFLAGS) $@ $^
	$(RANLIB) $@

# Ensure AlphaTensor CPU implementation is built before hybrid
alphatensor_hybrid/dgemm_alpha_hybrid.o: alphatensor/dgemm_alpha.o

# C compilation rules for AlphaTensor Hybrid with OpenCL
alphatensor_hybrid/%.o: alphatensor_hybrid/%.c
	$(CC) $(CFLAGS) $(OPENCL_CFLAGS) -c $< -o $@

.PHONY: clean cleanobj cleanlib
clean: cleanobj cleanlib
cleanobj:
	rm -f $(CHOLRL) $(CHOLTOP) $(LUCR) $(LULL) $(LUREC) $(QRLL) $(LARFTL2) $(ALPHATENSOR) $(ALPHATENSOR_HYBRID)
cleanlib:
	rm -f *.a
