#!/usr/bin/env python3
"""Validate DeepMind Transformation Against DGEMM.

Test if: DeepMind_result.T == DGEMM_result
"""

import numpy as np


def load_deepmind_factorization():
    """Load DeepMind's 4x4x4 factorization."""
    factorizations_path = (
        "../../../alphatensor_algo/alphatensor/algorithms/factorizations_r.npz"
    )

    with open(factorizations_path, "rb") as f:
        factorizations = dict(np.load(f, allow_pickle=True))

    u, v, w = factorizations["4,4,4"]
    return u, v, w


def deepmind_algorithm(A, B, debug=False):
    """Apply DeepMind's exact algorithm."""
    u, v, w = load_deepmind_factorization()

    # Flatten matrices
    A_flat = A.flatten()
    B_flat = B.flatten()

    if debug:
        print("🔍 DEBUGGING DEEPMIND ALGORITHM OPERATIONS")
        print(f"A_flat[0:6]: {A_flat[:6]}")
        print(f"B_flat[0:6]: {B_flat[:6]}")
        print(
            f"A_flat[8]: {A_flat[8]}, B_flat[8]: {B_flat[8]}"
        )  # index 8 = FORTRAN index 9

    # Apply factorization
    result_flat = np.zeros(16)
    for r in range(u.shape[1]):  # All 49 operations
        a_contrib = np.dot(u[:, r], A_flat)
        b_contrib = np.dot(v[:, r], B_flat)
        scalar_product = a_contrib * b_contrib
        result_flat += w[:, r] * scalar_product

        # Debug specific operations
        if debug and r in [
            0,
            4,
            5,
            6,
            7,
            8,
            9,
            19,
            29,
            39,
            44,
            45,
            46,
            47,
            48,
        ]:  # Operations 1, 5, 6, 7, 8, 9, 10, 20, 30, 40, 45, 46, 47, 48, 49 in FORTRAN (0-indexed here)
            print(f"\nOperation {r+1} (r={r}):")
            print(f"  a_contrib = {a_contrib:.6f}")
            print(f"  b_contrib = {b_contrib:.6f}")
            print(f"  scalar = {scalar_product:.6f}")
            if r == 4:  # After operation 5
                print(f"  result_flat[0:6] after op{r+1}: {result_flat[:6]}")
            if r == 5:  # After operation 6
                print(f"  result_flat[2] after op{r+1}: {result_flat[2]}")
            if r == 6:  # After operation 7
                print(f"  result_flat[4:8] after op{r+1}: {result_flat[4:8]}")
            if r == 7:  # After operation 8
                print(f"  result_flat[4:8] after op{r+1}: {result_flat[4:8]}")
                print(f"  result_flat[12:14] after op{r+1}: {result_flat[12:14]}")
            if r == 8:  # After operation 9
                print(f"  result_flat[0] after op{r+1}: {result_flat[0]}")
                print(f"  result_flat[2] after op{r+1}: {result_flat[2]}")
            if r == 9:  # After operation 10
                print(f"  result_flat[4:6] after op{r+1}: {result_flat[4:6]}")
                print(f"  result_flat[12:14] after op{r+1}: {result_flat[12:14]}")
            if r == 19:  # After operation 20
                print(f"  result_flat[3] after op{r+1}: {result_flat[3]}")
                print(f"  result_flat[6:8] after op{r+1}: {result_flat[6:8]}")
            if r == 29:  # After operation 30
                print(f"  result_flat[14] after op{r+1}: {result_flat[14]}")
            if r == 39:  # After operation 40
                print(f"  result_flat[8:12] after op{r+1}: {result_flat[8:12]}")
                print(f"  result_flat[14:16] after op{r+1}: {result_flat[14:16]}")
            if r == 44:  # After operation 45
                print(f"  result_flat[9] after op{r+1}: {result_flat[9]}")
            if r == 45:  # After operation 46
                print(f"  result_flat[6] after op{r+1}: {result_flat[6]}")
            if r == 46:  # After operation 47
                print(f"  result_flat[9] after op{r+1}: {result_flat[9]}")
                print(f"  result_flat[11] after op{r+1}: {result_flat[11]}")
            if r == 47:  # After operation 48
                print(f"  result_flat[14:16] after op{r+1}: {result_flat[14:16]}")
            if r == 48:  # After operation 49
                print(f"  result_flat[1] after op{r+1}: {result_flat[1]}")
                print(f"  result_flat[9] after op{r+1}: {result_flat[9]}")

    # Convert back to 4x4 matrix
    result = result_flat.reshape(4, 4)
    return result


def test_with_fortran_matrices():
    """Test using the exact matrices from our FORTRAN tests."""
    print("🧪 TESTING DEEPMIND TRANSFORMATION")
    print("=" * 50)

    # Test 2 matrices (the failing case)
    A = np.zeros((4, 4))
    B = np.zeros((4, 4))
    C_initial = np.zeros((4, 4))

    for i in range(1, 5):
        for j in range(1, 5):
            A[i - 1, j - 1] = (i + j) / 10.0
            B[i - 1, j - 1] = (i * j) / 5.0
            C_initial[i - 1, j - 1] = (i - j) / 3.0

    ALPHA = 2.0
    BETA = 1.0

    print("Test matrices:")
    print("A =")
    print(A)
    print("B =")
    print(B)
    print("C_initial =")
    print(C_initial)
    print(f"ALPHA = {ALPHA}, BETA = {BETA}")

    # Standard DGEMM result
    standard_result = ALPHA * (A @ B) + BETA * C_initial
    print(f"\n✅ Standard DGEMM result (A @ B):")
    print(standard_result)

    # DeepMind algorithm result
    print("\n" + "=" * 50)
    print("🔍 RUNNING DEEPMIND WITH DEBUG")
    print("=" * 50)
    deepmind_ab_transpose = deepmind_algorithm(A, B, debug=True)
    deepmind_result = ALPHA * deepmind_ab_transpose + BETA * C_initial
    print(f"\n🔬 DeepMind result (produces (A@B)^T):")
    print(deepmind_result)

    # Apply transpose to DeepMind result
    deepmind_transposed = ALPHA * deepmind_ab_transpose.T + BETA * C_initial
    print(f"\n🔄 DeepMind result TRANSPOSED:")
    print(deepmind_transposed)

    # Compare errors
    error_direct = np.max(np.abs(standard_result - deepmind_result))
    error_transposed = np.max(np.abs(standard_result - deepmind_transposed))

    print(f"\n📊 ERROR ANALYSIS:")
    print(f"  Standard vs DeepMind direct: {error_direct:.6f}")
    print(f"  Standard vs DeepMind.T: {error_transposed:.6f}")

    if error_transposed < 1e-12:
        print("  ✅ SUCCESS: DeepMind.T matches Standard DGEMM perfectly!")
        return True
    elif error_direct < error_transposed:
        print("  🤔 DeepMind direct is closer to standard (unexpected)")
        return False
    else:
        print("  ❌ Neither matches - something is wrong")
        return False


def test_simple_case():
    """Test with a simple case for verification."""
    print("\n🧪 TESTING SIMPLE CASE")
    print("=" * 30)

    A = np.array([[1, 2], [3, 4]], dtype=np.float64)
    B = np.array([[5, 6], [7, 8]], dtype=np.float64)

    # Note: We only have 4x4 factorization, so pad to 4x4
    A_4x4 = np.zeros((4, 4))
    B_4x4 = np.zeros((4, 4))
    A_4x4[:2, :2] = A
    B_4x4[:2, :2] = B

    print("A_4x4 =")
    print(A_4x4)
    print("B_4x4 =")
    print(B_4x4)

    # Standard result
    standard_4x4 = A_4x4 @ B_4x4
    print(f"\nStandard A @ B:")
    print(standard_4x4)

    # DeepMind result
    deepmind_4x4 = deepmind_algorithm(A_4x4, B_4x4)
    print(f"\nDeepMind result:")
    print(deepmind_4x4)

    # Check if DeepMind.T matches standard
    error = np.max(np.abs(standard_4x4 - deepmind_4x4.T))
    print(f"\nError between Standard and DeepMind.T: {error:.6f}")

    if error < 1e-12:
        print("✅ Simple case confirms: DeepMind produces (A@B)^T")
        return True
    else:
        print("❌ Simple case failed")
        return False


if __name__ == "__main__":
    print("🔬 DEEPMIND TRANSFORMATION VALIDATION")
    print("=" * 60)

    # Test simple case first
    simple_ok = test_simple_case()

    # Test with FORTRAN matrices
    fortran_ok = test_with_fortran_matrices()

    print(f"\n🎯 CONCLUSION:")
    if simple_ok and fortran_ok:
        print("✅ DeepMind's algorithm is mathematically correct")
        print("✅ Transformation relationship confirmed: DeepMind.T = Standard")
        print("❌ Our FORTRAN implementation has bugs")
    else:
        print("❌ Fundamental issue with DeepMind algorithm or our understanding")
