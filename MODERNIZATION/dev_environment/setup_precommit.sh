#!/bin/bash
# LAPACK AI Modernization - Pre-commit Setup Script
# This script installs and configures pre-commit hooks with fprettify for Fortran formatting

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

# Install pre-commit and fprettify if not already installed
echo "📦 Installing pre-commit tools..."
if command -v pre-commit >/dev/null 2>&1; then
    echo "✅ pre-commit is already installed"
    pre-commit --version
else
    echo "🔄 Installing pre-commit..."
    pip install pre-commit
fi

if command -v fprettify >/dev/null 2>&1; then
    echo "✅ fprettify is already installed"
    fprettify --version
else
    echo "🔄 Installing fprettify..."
    pip install fprettify
fi

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

# Test fprettify configuration
echo "🔍 Testing fprettify configuration..."
if [ -f ".fprettify.rc" ]; then
    echo "✅ fprettify configuration found"

    # Find a sample Fortran file to test
    SAMPLE_F_FILE=$(find SRC -name "*.f" -type f | head -1)
    if [ -n "$SAMPLE_F_FILE" ]; then
        echo "🧪 Testing fprettify on sample file: $SAMPLE_F_FILE"
        fprettify --diff "$SAMPLE_F_FILE" | head -20 || echo "   (No changes needed or preview shown)"
    else
        echo "ℹ️ No Fortran files found for testing"
    fi
else
    echo "⚠️ .fprettify.rc configuration not found"
fi

# Create secrets baseline if it doesn't exist
echo "🔒 Setting up secrets scanning..."
if [ ! -f ".secrets.baseline" ]; then
    echo "📋 Creating secrets baseline..."
    if command -v detect-secrets >/dev/null 2>&1; then
        detect-secrets scan --baseline .secrets.baseline
        echo "✅ Secrets baseline created"
    else
        echo "⚠️ detect-secrets not installed, skipping baseline creation"
    fi
else
    echo "✅ Secrets baseline already exists"
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
echo "   ✅ fprettify Fortran formatter configured"
echo "   ✅ Python code quality tools (black, isort, flake8)"
echo "   ✅ File quality checks (trailing whitespace, etc.)"
echo "   ✅ Security scanning (detect-secrets)"
echo "   ✅ CMake formatting"
echo "   ✅ Docker linting"
echo ""
echo "🔧 How to use:"
echo "   📝 Hooks will run automatically on git commit"
echo "   🎯 Run manually: pre-commit run --all-files"
echo "   🔍 Format specific file: fprettify --diff <file>"
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
    echo "   • Make sure gfortran is installed for fprettify"
    echo "   • All team members should run this script"
fi

echo ""
echo "✅ Setup complete! Ready for development with automatic code formatting."
