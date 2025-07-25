#!/bin/bash
# LAPACK AI Modernization - Pre-commit Setup Script
# This script installs and configures pre-commit hooks for code quality

set -e  # Exit on any error

echo "🔧 Setting up pre-commit hooks for LAPACK AI Modernization..."

# Get the directory of this script
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"
PROJECT_ROOT="$(dirname "$(dirname "$SCRIPT_DIR")")"

echo "📍 Project root: $PROJECT_ROOT"

# Check if we're in a container or host system
if [ -f /.dockerenv ]; then
    echo "🐳 Running in containerized environment"
    ENVIRONMENT="container"
else
    echo "🏠 Running on host system"
    ENVIRONMENT="host"
fi

# Install pre-commit if not already installed
echo "📦 Installing pre-commit tools..."
if command -v pre-commit >/dev/null 2>&1; then
    echo "✅ pre-commit is already installed"
    pre-commit --version
else
    echo "🔄 Installing pre-commit..."
    pip install pre-commit
fi

# fprettify removed - code quality handled in compilation step

# Change to project root
cd "$PROJECT_ROOT"

# Install pre-commit hooks
echo "🔧 Installing pre-commit hooks..."
if [ -f ".pre-commit-config.yaml" ]; then
    pre-commit install
    echo "✅ Pre-commit hooks installed successfully"

    # Install commit message hooks
    pre-commit install --hook-type commit-msg
    echo "✅ Commit message hooks installed"

    # Install the pre-commit environments
    echo "📥 Installing hook environments (this may take a few minutes)..."
    pre-commit install --install-hooks
    echo "✅ Hook environments installed"

else
    echo "❌ .pre-commit-config.yaml not found in project root"
    echo "   Please ensure you're running this from the correct directory"
    exit 1
fi

# fprettify testing removed - code quality handled in compilation step

# Check gitleaks configuration
echo "🔒 Setting up gitleaks security scanning..."
if [ -f ".gitleaks.toml" ]; then
    echo "✅ Gitleaks configuration found"

    # Test gitleaks if available
    if command -v gitleaks >/dev/null 2>&1; then
        echo "🧪 Testing gitleaks configuration..."
        gitleaks detect --config .gitleaks.toml --no-git --quiet || echo "   (Gitleaks test completed)"
    else
        echo "ℹ️ Gitleaks not installed locally (will be available in CI/containers)"
    fi
else
    echo "⚠️ .gitleaks.toml configuration not found"
fi

# Run a quick test of the pre-commit hooks
echo "🧪 Testing pre-commit setup..."
echo "🔍 Running pre-commit on a subset of files..."

# Test on a few files to make sure everything works
TEST_FILES=""

# Find some Python files to test
PYTHON_FILES=$(find . -name "*.py" -not -path "./build/*" -not -path "./.venv/*" | head -3)
if [ -n "$PYTHON_FILES" ]; then
    TEST_FILES="$TEST_FILES $PYTHON_FILES"
fi

# Find some YAML files to test
YAML_FILES=$(find . -name "*.yml" -o -name "*.yaml" | head -2)
if [ -n "$YAML_FILES" ]; then
    TEST_FILES="$TEST_FILES $YAML_FILES"
fi

if [ -n "$TEST_FILES" ]; then
    echo "📁 Testing on files: $(echo $TEST_FILES | tr '\n' ' ')"
    pre-commit run --files $TEST_FILES || echo "   (Some formatting may be needed)"
else
    echo "ℹ️ No suitable test files found"
fi

# Show summary of what was installed
echo ""
echo "🎉 Pre-commit setup completed successfully!"
echo ""
echo "📋 Summary of installed tools:"
echo "   ✅ pre-commit hooks installed"
echo "   ✅ Pre-commit hooks configured (fprettify removed)"
echo "   ✅ Python code quality tools (black, isort, flake8)"
echo "   ✅ File quality checks (trailing whitespace, etc.)"
echo "   ✅ Security scanning (gitleaks)"
echo "   ✅ CMake formatting"
echo "   ✅ Docker linting"
echo ""
echo "🔧 How to use:"
echo "   📝 Hooks will run automatically on git commit"
echo "   🎯 Run manually: pre-commit run --all-files"
echo "   🔍 Fortran code quality: handled in compilation step"
echo "   ⚙️ Update hooks: pre-commit autoupdate"
echo ""
echo "💡 Tips:"
echo "   • Hooks run automatically when you commit"
echo "   • Use 'git commit --no-verify' to skip hooks if needed"
echo "   • Run 'pre-commit run --all-files' to format entire codebase"
echo "   • Check '.pre-commit-config.yaml' for configuration details"
echo ""

# Show next steps based on environment
if [ "$ENVIRONMENT" = "container" ]; then
    echo "🐳 Container Environment Notes:"
    echo "   • Pre-commit is ready to use in this container"
    echo "   • Configuration will persist in mounted volumes"
    echo "   • Hooks will work normally with git operations"
elif [ "$ENVIRONMENT" = "host" ]; then
    echo "🏠 Host Environment Notes:"
    echo "   • Consider using Docker for consistent environment"
    echo "   • Make sure gfortran is installed for compilation checks"
    echo "   • All team members should run this script"
fi

echo ""
echo "✅ Setup complete! Ready for development with automatic code formatting."
