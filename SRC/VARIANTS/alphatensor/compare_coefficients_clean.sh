#!/bin/bash

# Clean Coefficient Comparison Script
# Compare FORTRAN implementation C coefficients with correct DeepMind coefficients

echo "🔍 ALPHATENSOR COEFFICIENT COMPARISON ANALYSIS (CLEAN)"
echo "=================================================================="

# Function to extract clean C updates for a specific operation from FORTRAN
extract_fortran_operation_clean() {
    local op_num=$1

    # Extract the operation block from FORTRAN file, only C updates
    awk "/Operation $op_num \(r=$((op_num-1))\):/,/Operation $((op_num+1)) \(r=$op_num\):/" dgemm_alpha_fixed.f | \
    grep "C([0-9],[0-9]) = C([0-9],[0-9]) [+-] ALPHA \* SCALAR_RESULT" | \
    sed 's/.*C(\([0-9],[0-9]\)) = C(\([0-9],[0-9]\)) \([+-]\) ALPHA \* SCALAR_RESULT.*/C(\1) \3= SCALAR_RESULT/' | \
    sort
}

# Function to extract correct coefficients for a specific operation
extract_correct_operation_clean() {
    local op_num=$1

    # Extract from correct mappings file, only TEMP_RESULT updates
    awk "/Operation $op_num: C coefficient mapping/,/Operation $((op_num+1)): C coefficient mapping/" all_correct_c_mappings.txt | \
    grep "TEMP_RESULT([0-9],[0-9]) = TEMP_RESULT([0-9],[0-9]) [+-] SCALAR_RESULT" | \
    sed 's/.*TEMP_RESULT(\([0-9],[0-9]\)) = TEMP_RESULT(\([0-9],[0-9]\)) \([+-]\) SCALAR_RESULT.*/C(\1) \3= SCALAR_RESULT/' | \
    sort
}

# Compare coefficients for each operation
compare_operations_clean() {
    local errors=0
    local matches=0

    echo "📊 DETAILED OPERATION ANALYSIS"
    echo "=================================================================="

    for op in {1..49}; do
        # Extract clean patterns
        fortran_pattern=$(extract_fortran_operation_clean $op)
        correct_pattern=$(extract_correct_operation_clean $op)

        # Compare patterns
        if [ "$fortran_pattern" = "$correct_pattern" ]; then
            matches=$((matches + 1))
            echo "✅ Operation $op: CORRECT"
        else
            errors=$((errors + 1))
            echo "❌ Operation $op: INCORRECT"
            echo "   FORTRAN: $fortran_pattern"
            echo "   CORRECT: $correct_pattern"
            echo ""
        fi
    done

    echo ""
    echo "📊 FINAL SUMMARY"
    echo "=================================================================="
    echo "Total Operations: 49"
    echo "Correct Operations: $matches"
    echo "Incorrect Operations: $errors"
    echo "Success Rate: $(( matches * 100 / 49 ))%"

    if [ $errors -eq 0 ]; then
        echo ""
        echo "🎉 ALL COEFFICIENTS MATCH PERFECTLY!"
        echo "✅ Algorithm C coefficients are mathematically correct."
        echo "✅ All 49 operations have correct coefficient patterns."
    else
        echo ""
        echo "⚠️  $errors operations have coefficient errors."
        echo "🔧 These operations need coefficient corrections."
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

# Run clean comparison
compare_operations_clean

echo ""
echo "🏁 Clean coefficient comparison complete!"
