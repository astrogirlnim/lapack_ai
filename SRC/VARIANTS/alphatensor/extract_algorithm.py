#!/usr/bin/env python3
"""
Extract AlphaTensor 4x4 Matrix Multiplication Algorithm
From DeepMind's official repository data
"""

import os
import sys

import numpy as np


def load_factorizations(filename):
    """Load factorizations from DeepMind's .npz file"""
    print(f"Loading factorizations from {filename}...")

    with open(filename, "rb") as f:
        factorizations = dict(np.load(f, allow_pickle=True))

    print(f"Loaded {len(factorizations)} factorizations")
    return factorizations


def inspect_factorization_structure(factorizations):
    """Inspect the structure of the factorizations"""
    print("\n=== FACTORIZATION STRUCTURE ===")

    for key, value in factorizations.items():
        print(f"Key: {key}")
        print(f"  Type: {type(value)}")
        if hasattr(value, "shape"):
            print(f"  Shape: {value.shape}")
        if hasattr(value, "dtype"):
            print(f"  Dtype: {value.dtype}")
        print()


def find_4x4_algorithms(factorizations):
    """Find 4x4 matrix multiplication algorithms"""
    print("\n=== SEARCHING FOR 4x4 ALGORITHMS ===")

    algorithms_4x4 = []

    for key, value in factorizations.items():
        # Look for keys or shapes that suggest 4x4 matrix multiplication
        if "4" in str(key).lower() or "matrix" in str(key).lower():
            print(f"Potential 4x4 algorithm: {key}")
            print(f"  Shape: {getattr(value, 'shape', 'No shape')}")
            print(f"  Type: {type(value)}")
            algorithms_4x4.append((key, value))

    return algorithms_4x4


def extract_tensor_decomposition(algorithm_data):
    """Extract the tensor decomposition for 4x4 matrix multiplication"""
    print("\n=== EXTRACTING TENSOR DECOMPOSITION ===")

    # The tensor factorization should give us three matrices:
    # A factors, B factors, C factors for the bilinear algorithm
    # C = sum_r (A_factor_r ⊗ B_factor_r) * alpha_r

    if hasattr(algorithm_data, "shape"):
        print(f"Algorithm data shape: {algorithm_data.shape}")

        # Typical tensor decomposition format:
        # Shape might be (R, d1, d2) where R is rank (47 for AlphaTensor)
        # d1, d2 are matrix dimensions

        if len(algorithm_data.shape) >= 2:
            rank = algorithm_data.shape[0]  # Number of rank-1 terms
            print(f"Tensor rank: {rank}")

            if rank == 47:  # This is likely the AlphaTensor 47-operation algorithm!
                print("🎯 FOUND: 47-operation AlphaTensor algorithm!")
                return algorithm_data
            elif rank < 64:  # Any improvement over standard algorithm
                print(f"📈 FOUND: {rank}-operation algorithm (better than standard 64)")
                return algorithm_data

    return None


def convert_to_fortran_operations(tensor_data):
    """Convert tensor decomposition to Fortran operations"""
    print("\n=== CONVERTING TO FORTRAN OPERATIONS ===")

    if tensor_data is None:
        print("No tensor data to convert")
        return []

    operations = []
    rank = tensor_data.shape[0]

    print(f"Converting {rank} tensor operations...")

    # Each rank-1 component gives us one multiplication operation
    for r in range(rank):
        operation = {
            "index": r + 1,
            "data": tensor_data[r],
            "description": f"h{r+1} operation",
        }
        operations.append(operation)

    return operations


def generate_fortran_code(operations):
    """Generate Fortran code for the AlphaTensor algorithm"""
    print("\n=== GENERATING FORTRAN CODE ===")

    fortran_lines = []
    fortran_lines.append("*     === REAL ALPHATENSOR ALGORITHM ===")
    fortran_lines.append("*     From DeepMind's official repository")
    fortran_lines.append("*")

    for i, op in enumerate(operations):
        fortran_lines.append(f"*     Operation {op['index']}: {op['description']}")

        # Generate actual Fortran multiplication based on tensor data
        # This needs to be customized based on the actual tensor format
        fortran_lines.append(
            f"      H({op['index']}) = ! TODO: Extract from tensor data"
        )

    fortran_lines.append("*")
    fortran_lines.append("*     Result reconstruction (TODO: from tensor C factors)")

    return fortran_lines


def main():
    """Main extraction process"""
    print("🔬 EXTRACTING REAL ALPHATENSOR ALGORITHM")
    print("=" * 50)

    # Try standard arithmetic algorithms first
    algo_file = "/tmp/alphatensor/algorithms/factorizations_r.npz"
    if not os.path.exists(algo_file):
        print(f"❌ Algorithm file not found: {algo_file}")
        return

    # Load factorizations
    factorizations = load_factorizations(algo_file)

    # Inspect structure
    inspect_factorization_structure(factorizations)

    # Find 4x4 algorithms
    algorithms_4x4 = find_4x4_algorithms(factorizations)

    if not algorithms_4x4:
        print("🔍 No obvious 4x4 algorithms found, checking all entries...")
        # Check all entries for potential algorithms
        for key, value in factorizations.items():
            print(f"\nAnalyzing: {key}")
            tensor_data = extract_tensor_decomposition(value)
            if tensor_data is not None:
                print(f"✅ Found algorithm in '{key}'")
                operations = convert_to_fortran_operations(tensor_data)
                fortran_code = generate_fortran_code(operations)

                # Write to file
                output_file = "real_alphatensor_algorithm.f"
                with open(output_file, "w") as f:
                    f.write("\n".join(fortran_code))

                print(f"📝 Fortran code written to: {output_file}")
                print("\n🎯 SUMMARY:")
                print(f"   - Found {len(operations)} operations")
                print(f"   - Generated Fortran template")
                print(f"   - Manual tensor interpretation required")
                return
    else:
        print(f"✅ Found {len(algorithms_4x4)} potential 4x4 algorithms")
        for key, algo_data in algorithms_4x4:
            print(f"\nProcessing: {key}")
            tensor_data = extract_tensor_decomposition(algo_data)
            if tensor_data is not None:
                operations = convert_to_fortran_operations(tensor_data)
                fortran_code = generate_fortran_code(operations)
                print("Generated Fortran template")
                return

    print("\n❌ No suitable algorithms found")
    print("💡 The data format may require manual inspection")


if __name__ == "__main__":
    main()
