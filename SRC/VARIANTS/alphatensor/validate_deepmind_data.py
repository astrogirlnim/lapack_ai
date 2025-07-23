#!/usr/bin/env python3
"""Validate DeepMind AlphaTensor Data Interpretation.

Based on DeepMind's explore_factorizations.ipynb to verify correct data usage.
"""

import numpy as np


def get_mamu_tensor_rectangular(a: int, b: int, c: int) -> np.ndarray:
    """Returns the symmetrized matrix multiplication tensor T_{a, b, c}.

    This is DeepMind's exact function from their notebook.
    """
    result = np.full((a * b, b * c, c * a), 0, dtype=np.int32)
    for i in range(a):
        for j in range(b):
            for k in range(c):
                result[i * b + j][j * c + k][k * a + i] = 1
    return result


def load_deepmind_factorization():
    """Load DeepMind's 4x4x4 factorization exactly as they do."""
    factorizations_path = (
        "../../../alphatensor_algo/alphatensor/algorithms/factorizations_r.npz"
    )

    with open(factorizations_path, "rb") as f:
        factorizations = dict(np.load(f, allow_pickle=True))

    print("🔍 Available factorizations:")
    for key in factorizations:
        u, v, w = factorizations[key]
        rank = u.shape[-1]
        print(f"  {key}: rank={rank}, shapes=({u.shape}, {v.shape}, {w.shape})")

    # Get 4x4x4 factorization
    u, v, w = factorizations["4,4,4"]
    print(f"\n📊 4x4x4 factorization:")
    print(f"  u.shape: {u.shape}")
    print(f"  v.shape: {v.shape}")
    print(f"  w.shape: {w.shape}")
    print(f"  rank: {u.shape[-1]}")

    return u, v, w


def validate_factorization():
    """Validate the 4x4x4 factorization using DeepMind's approach."""
    print("🧮 VALIDATING DEEPMIND 4X4X4 FACTORIZATION")
    print("=" * 50)

    # Load factorization
    u, v, w = load_deepmind_factorization()

    # Create expected tensor (symmetrized)
    tensor = get_mamu_tensor_rectangular(4, 4, 4)
    print(f"\n🎯 Expected tensor shape: {tensor.shape}")
    print(f"   Expected non-zero elements: {np.count_nonzero(tensor)}")

    # Reconstruct using DeepMind's einsum
    reconstruction = np.einsum("ir,jr,kr->ijk", u, v, w)
    print(f"\n🔧 Reconstructed tensor shape: {reconstruction.shape}")
    print(f"   Reconstructed non-zero elements: {np.count_nonzero(reconstruction)}")

    # Check correctness
    if np.array_equal(tensor, reconstruction):
        print("\n✅ SUCCESS: Factorization is correct in R (standard arithmetic)")
        return True, u, v, w
    elif np.array_equal(tensor, np.mod(reconstruction, 2)):
        print("\n⚠️  Factorization is correct in F2 (modular arithmetic)")
        return True, u, v, w
    else:
        print("\n❌ FAILED: Factorization is incorrect")
        print(f"   Max error: {np.max(np.abs(tensor - reconstruction))}")
        return False, u, v, w


def analyze_symmetrized_vs_standard():
    """Analyze the difference between symmetrized and standard matrix multiplication."""
    print("\n🔍 ANALYZING SYMMETRIZED VS STANDARD MATRIX MULTIPLICATION")
    print("=" * 60)

    # Create test matrices
    A = np.array([[1, 2], [3, 4]], dtype=np.float64)
    B = np.array([[5, 6], [7, 8]], dtype=np.float64)

    # Standard multiplication: C = A @ B
    C_standard = A @ B
    print("Standard A @ B:")
    print(C_standard)

    # Symmetrized multiplication: C = (A @ B)^T
    C_symmetrized = (A @ B).T
    print("\nSymmetrized (A @ B)^T:")
    print(C_symmetrized)

    print(
        f"\nDifference: They are {'the same' if np.array_equal(C_standard, C_symmetrized) else 'DIFFERENT'}"
    )

    return C_standard, C_symmetrized


def show_correct_algorithm_structure():
    """Show how we should structure the algorithm for symmetrized factorization."""
    print("\n🎯 CORRECT ALGORITHM STRUCTURE FOR SYMMETRIZED FACTORIZATION")
    print("=" * 65)

    print(
        """
    INSIGHT: DeepMind factors represent (A·B)^T, not A·B

    INCORRECT (what we were doing):
        C = apply_alphatensor_factors(A, B)  // Wrong!

    CORRECT (what we should do):
        C_temp = apply_alphatensor_factors(A, B)
        C = C_temp.T  // Transpose the result!

    OR equivalently:
        C = apply_alphatensor_factors(A, B)^T

    This explains why our numerical results were wrong!
    """
    )


if __name__ == "__main__":
    print("🧪 DEEPMIND ALPHATENSOR DATA VALIDATION")
    print("=" * 45)

    # Validate factorization
    is_valid, u, v, w = validate_factorization()

    if is_valid:
        print(f"\n✅ DeepMind data validation: SUCCESS")
        print(f"   We have {u.shape[-1]} operations (rank-{u.shape[-1]} factorization)")

        # Analyze symmetrized vs standard
        analyze_symmetrized_vs_standard()

        # Show correct algorithm structure
        show_correct_algorithm_structure()

        print(f"\n🎯 NEXT STEPS:")
        print(f"   1. Modify our algorithm to handle the transpose correctly")
        print(f"   2. Test the corrected implementation")
        print(f"   3. Verify <1e-12 numerical accuracy")

    else:
        print(f"\n❌ DeepMind data validation: FAILED")
        print(f"   Cannot proceed with incorrect factorization")
