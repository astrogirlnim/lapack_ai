#!/usr/bin/env python3
"""Extract Exact DeepMind Coefficients for FORTRAN Implementation."""

import numpy as np


def extract_coefficients():
    """Extract exact u, v, w coefficients for all 49 operations."""
    # Load DeepMind factorization
    factorizations_path = (
        "../../../alphatensor_algo/alphatensor/algorithms/factorizations_r.npz"
    )

    with open(factorizations_path, "rb") as f:
        factorizations = dict(np.load(f, allow_pickle=True))

    # Get 4x4x4 factorization
    u, v, w = factorizations["4,4,4"]

    print("EXACT DEPMIND COEFFICIENTS FOR FORTRAN IMPLEMENTATION")
    print("=" * 60)
    print(f"Found {u.shape[1]} operations (rank-{u.shape[1]} factorization)")
    print()

    # Generate FORTRAN code for all 49 operations
    for r in range(u.shape[1]):  # All 49 operations
        print(f"*     Operation {r+1} (r={r}): Exact DeepMind coefficients")

        # A coefficients (u[:,r])
        a_coeffs = u[:, r]
        a_terms = []
        for i, coeff in enumerate(a_coeffs):
            if coeff != 0:
                if coeff == 1:
                    a_terms.append(f"A_FLAT({i+1})")
                elif coeff == -1:
                    a_terms.append(f"-A_FLAT({i+1})")
                else:
                    a_terms.append(f"{coeff}*A_FLAT({i+1})")

        if a_terms:
            a_contrib = " + ".join(a_terms).replace("+ -", "- ")
            print(f"      A_CONTRIB = {a_contrib}")
        else:
            print("      A_CONTRIB = 0.0")

        # B coefficients (v[:,r])
        b_coeffs = v[:, r]
        b_terms = []
        for i, coeff in enumerate(b_coeffs):
            if coeff != 0:
                if coeff == 1:
                    b_terms.append(f"B_FLAT({i+1})")
                elif coeff == -1:
                    b_terms.append(f"-B_FLAT({i+1})")
                else:
                    b_terms.append(f"{coeff}*B_FLAT({i+1})")

        if b_terms:
            b_contrib = " + ".join(b_terms).replace("+ -", "- ")
            print(f"      B_CONTRIB = {b_contrib}")
        else:
            print("      B_CONTRIB = 0.0")

        print("      SCALAR_RESULT = A_CONTRIB * B_CONTRIB")

        # C coefficients (w[:,r])
        c_coeffs = w[:, r]
        for i, coeff in enumerate(c_coeffs):
            if coeff != 0:
                if coeff == 1:
                    print(
                        f"      RESULT_FLAT({i+1}) = RESULT_FLAT({i+1}) + SCALAR_RESULT"
                    )
                elif coeff == -1:
                    print(
                        f"      RESULT_FLAT({i+1}) = RESULT_FLAT({i+1}) - SCALAR_RESULT"
                    )
                else:
                    print(
                        f"      RESULT_FLAT({i+1}) = RESULT_FLAT({i+1}) + "
                        f"{coeff}*SCALAR_RESULT"
                    )

        print("*")

        # Break lines every 10 operations for readability
        if (r + 1) % 10 == 0:
            print(f"      WRITE(LOG_UNIT,*) 'ALPHATENSOR_CORRECT: {r+1} ops complete'")
            print("*")


if __name__ == "__main__":
    extract_coefficients()
