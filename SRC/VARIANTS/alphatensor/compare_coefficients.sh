#!/bin/bash

# Coefficient Comparison Script
# Compare FORTRAN implementation C coefficients with correct DeepMind coefficients

echo "🔍 ALPHATENSOR COEFFICIENT COMPARISON ANALYSIS"
echo "=================================================================="

# Extract C coefficient patterns from FORTRAN file
echo "📄 Extracting C coefficients from FORTRAN implementation..."

# Function to extract C updates for a specific operation
extract_fortran_operation() {
    local op_num=$1
    echo "=== Operation $op_num ==="

    # Extract the operation block from FORTRAN file
    awk "/Operation $op_num \(r=$((op_num-1))\):/,/Operation $((op_num+1)) \(r=$op_num\):/" dgemm_alpha_fixed.f | \
    grep "C(" | \
    grep -v "WRITE" | \
    sed 's/.*C(\([0-9],[0-9]\)) = C(\([0-9],[0-9]\)) \([+-]\) ALPHA \* SCALAR_RESULT.*/      C(\1) \3= SCALAR_RESULT/' | \
    sort
}

# Function to extract correct coefficients for a specific operation
extract_correct_operation() {
    local op_num=$1
    echo "=== Correct Operation $op_num ==="

    # Extract from correct mappings file
    awk "/Operation $op_num: C coefficient mapping/,/Operation $((op_num+1)): C coefficient mapping/" all_correct_c_mappings.txt | \
    grep "TEMP_RESULT" | \
    sed 's/.*TEMP_RESULT(\([0-9],[0-9]\)) = TEMP_RESULT(\([0-9],[0-9]\)) \([+-]\) SCALAR_RESULT.*/      C(\1) \3= SCALAR_RESULT/' | \
    sort
}

# Compare coefficients for each operation
compare_operations() {
    local errors=0

    for op in {1..49}; do
        echo ""
        echo "🔬 COMPARING OPERATION $op"
        echo "----------------------------------------"

        # Extract patterns
        fortran_pattern=$(extract_fortran_operation $op)
        correct_pattern=$(extract_correct_operation $op)

        echo "FORTRAN Implementation:"
        echo "$fortran_pattern"
        echo ""
        echo "Correct DeepMind Pattern:"
        echo "$correct_pattern"
        echo ""

        # Compare patterns
        if [ "$fortran_pattern" = "$correct_pattern" ]; then
            echo "✅ MATCH: Operation $op coefficients are CORRECT"
        else
            echo "❌ MISMATCH: Operation $op coefficients are INCORRECT"
            errors=$((errors + 1))
            echo "📊 DETAILED DIFF:"
            echo "FORTRAN has:"
            echo "$fortran_pattern"
            echo "Should be:"
            echo "$correct_pattern"
        fi
        echo "----------------------------------------"
    done

    echo ""
    echo "📊 FINAL SUMMARY"
    echo "=================================================================="
    echo "Total Operations: 49"
    echo "Correct Operations: $((49 - errors))"
    echo "Incorrect Operations: $errors"
    echo "Success Rate: $(( (49 - errors) * 100 / 49 ))%"

    if [ $errors -eq 0 ]; then
        echo "🎉 ALL COEFFICIENTS MATCH! Algorithm is mathematically correct."
    else
        echo "⚠️  $errors operations have coefficient errors that need fixing."
    fi
}

# Check if required files exist
if [ ! -f "dgemm_alpha_fixed.f" ]; then
    echo "❌ Error: dgemm_alpha_fixed.f not found"
    exit 1
fi

if [ ! -f "all_correct_c_mappings.txt" ]; then
    echo "❌ Error: all_correct_c_mappings.txt not found"
    exit 1
fi

# Run comparison
compare_operations

echo ""
echo "🏁 Coefficient comparison complete!"
