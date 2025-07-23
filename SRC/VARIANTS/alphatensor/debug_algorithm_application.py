#!/usr/bin/env python3
"""Debug Algorithm Application - Compare Our Method vs DeepMind's."""

import numpy as np


def load_deepmind_factorization():
    """Load DeepMind's 4x4x4 factorization exactly as they do."""
    factorizations_path = (
        "../../../alphatensor_algo/alphatensor/algorithms/factorizations_r.npz"
    )

    with open(factorizations_path, "rb") as f:
        factorizations = dict(np.load(f, allow_pickle=True))

    # Get raw factorization (as DeepMind stores it)
    u, v, w = factorizations["4,4,4"]
    print(f"🔍 Raw DeepMind factors:")
    print(f"  u.shape: {u.shape} = {u.shape[0]}x{u.shape[1]}")
    print(f"  v.shape: {v.shape} = {v.shape[0]}x{v.shape[1]}")
    print(f"  w.shape: {w.shape} = {w.shape[0]}x{w.shape[1]}")

    return u, v, w


def load_our_factors():
    """Load factors the way our current algorithm does."""
    factorizations_path = (
        "../../../alphatensor_algo/alphatensor/algorithms/factorizations_r.npz"
    )

    with open(factorizations_path, "rb") as f:
        factorizations = dict(np.load(f, allow_pickle=True))

    algorithm_4x4 = factorizations["4,4,4"]

    # Our current approach (with transposes)
    factors = [
        algorithm_4x4[0].T.reshape(4, 4, 49),  # A factors [4, 4, 49]
        algorithm_4x4[1].T.reshape(4, 4, 49),  # B factors [4, 4, 49]
        algorithm_4x4[2].T.reshape(4, 4, 49),  # C factors [4, 4, 49]
    ]

    # Transpose C factors as per DeepMind's code
    factors[2] = factors[2].transpose(1, 0, 2)

    print(f"\n🔧 Our processed factors:")
    print(f"  a_factors.shape: {factors[0].shape}")
    print(f"  b_factors.shape: {factors[1].shape}")
    print(f"  c_factors.shape: {factors[2].shape}")

    return factors[0], factors[1], factors[2]


def deepmind_approach(A, B):
    """Apply matrix multiplication using DeepMind's einsum approach."""
    u, v, w = load_deepmind_factorization()

    # DeepMind's approach: einsum reconstruction
    # First, flatten the matrices
    A_flat = A.flatten()  # 4x4 -> 16 elements
    B_flat = B.flatten()  # 4x4 -> 16 elements

    # Apply the factorization using einsum
    # This is how DeepMind validates their factorization
    result_flat = np.zeros(16)

    for r in range(u.shape[1]):  # For each rank (49 operations)
        # u[:,r] are the A coefficients for operation r
        # v[:,r] are the B coefficients for operation r
        # w[:,r] are the C coefficients for operation r
        a_contrib = np.dot(u[:, r], A_flat)  # Linear combination of A
        b_contrib = np.dot(v[:, r], B_flat)  # Linear combination of B
        scalar_product = a_contrib * b_contrib  # Scalar result
        result_flat += w[:, r] * scalar_product  # Distribute to result

    result = result_flat.reshape(4, 4)
    return result


def our_approach(A, B):
    """Apply matrix multiplication using our current approach."""
    a_factors, b_factors, c_factors = load_our_factors()

    result = np.zeros((4, 4))

    for op in range(49):  # All 49 operations
        # Create linear combinations (our approach)
        left_combo = 0.0
        right_combo = 0.0

        # Left combination (A matrix)
        for i in range(4):
            for j in range(4):
                coef = a_factors[i, j, op]
                if coef != 0:
                    left_combo += coef * A[i, j]

        # Right combination (B matrix)
        for i in range(4):
            for j in range(4):
                coef = b_factors[i, j, op]
                if coef != 0:
                    right_combo += coef * B[i, j]

        # Scalar product
        scalar_product = left_combo * right_combo

        # Distribute to result
        for i in range(4):
            for k in range(4):
                coef = c_factors[i, k, op]
                if coef != 0:
                    result[i, k] += coef * scalar_product

    # Apply transpose (as we discovered)
    result = result.T

    return result


def compare_approaches():
    """Compare our approach vs DeepMind's."""
    print("🔍 COMPARING ALGORITHM APPROACHES")
    print("=" * 40)

    # Test matrices
    A = np.array(
        [[1, 2, 3, 4], [5, 6, 7, 8], [9, 10, 11, 12], [13, 14, 15, 16]],
        dtype=np.float64,
    )

    B = np.array(
        [[1, 1, 1, 1], [2, 2, 2, 2], [3, 3, 3, 3], [4, 4, 4, 4]], dtype=np.float64
    )

    print("🧮 Test matrices:")
    print("A =")
    print(A)
    print("B =")
    print(B)

    # Standard result
    standard_result = A @ B
    print(f"\n✅ Standard A @ B:")
    print(standard_result)

    # DeepMind approach
    deepmind_result = deepmind_approach(A, B)
    print(f"\n🎯 DeepMind approach result:")
    print(deepmind_result)

    # Our approach
    our_result = our_approach(A, B)
    print(f"\n🔧 Our approach result:")
    print(our_result)

    # Compare errors
    deepmind_error = np.max(np.abs(standard_result - deepmind_result))
    our_error = np.max(np.abs(standard_result - our_result))

    print(f"\n📊 ERROR ANALYSIS:")
    print(f"  DeepMind approach error: {deepmind_error}")
    print(f"  Our approach error: {our_error}")

    if deepmind_error < 1e-10:
        print("  ✅ DeepMind approach: CORRECT")
    else:
        print("  ❌ DeepMind approach: INCORRECT")

    if our_error < 1e-10:
        print("  ✅ Our approach: CORRECT")
    else:
        print("  ❌ Our approach: INCORRECT")

    # Symmetrized check
    symmetrized_standard = (A @ B).T
    deepmind_vs_symmetrized = np.max(np.abs(symmetrized_standard - deepmind_result))
    our_vs_symmetrized = np.max(np.abs(symmetrized_standard - our_result))

    print(f"\n🔄 SYMMETRIZED COMPARISON:")
    print(f"  DeepMind vs (A@B).T error: {deepmind_vs_symmetrized}")
    print(f"  Our vs (A@B).T error: {our_vs_symmetrized}")


if __name__ == "__main__":
    compare_approaches()
