# Pre-commit Hooks with fprettify for LAPACK AI

## Overview

This document describes the pre-commit hook setup for the LAPACK AI Modernization project, with a focus on automatic Fortran code formatting using `fprettify`.

## What's Included

The pre-commit configuration includes comprehensive code quality checks for the multi-language LAPACK codebase:

### Fortran Formatting with fprettify
- **Tool**: [fprettify](https://github.com/fortran-lang/fprettify) - The de facto standard Fortran formatter
- **Configuration**: Conservative settings optimized for legacy LAPACK code
- **Features**:
  - 2-space indentation for readability
  - 132-character line length (modern Fortran standard)
  - Conservative whitespace handling
  - Safe for legacy code (disables potentially breaking transformations)

### Python Code Quality
- **Black**: Automatic Python code formatting
- **isort**: Import statement sorting
- **flake8**: Python linting and style checking

### General File Quality
- **Trailing whitespace removal**
- **Newline normalization**
- **YAML/JSON/TOML validation**
- **Large file prevention**
- **Merge conflict detection**

### Additional Tools
- **CMake formatting**: Consistent build script formatting
- **Docker linting**: Dockerfile quality checks
- **Security scanning**: Gitleaks for detecting hardcoded secrets and credentials

## Fortran Module Dependencies

### Important Note on CI Validation

The LAPACK codebase contains Fortran modules with dependencies:
- `la_constants.f90` defines the `LA_CONSTANTS` module
- 8 other .f90 files depend on this module: `clartg.f90`, `classq.f90`, `zlassq.f90`, `dlassq.f90`, `dlartg.f90`, `zlartg.f90`, `slartg.f90`, `slassq.f90`

When testing individual Fortran files, **always compile required modules first**:

```bash
# Correct way to test dependent files
gfortran -c la_constants.f90    # Creates la_constants.mod
gfortran -fsyntax-only clartg.f90  # Now works correctly

# Cleanup
rm -f *.mod *.o
```

The CI system automatically handles this by compiling `la_constants.f90` before testing other files.

## Installation and Setup

### Option 1: Automated Setup Script

```bash
# Run the automated setup script
./MODERNIZATION/dev_environment/setup_precommit.sh
```

This script will:
- Install pre-commit and fprettify
- Install all pre-commit hooks
- Test the configuration
- Provide usage instructions

### Option 2: Manual Setup

#### 1. Install Dependencies

```bash
# Install pre-commit and fprettify
pip install pre-commit fprettify

# Or install via requirements.txt
pip install -r MODERNIZATION/dev_environment/requirements.txt
```

#### 2. Install Pre-commit Hooks

```bash
# Install the git hooks
pre-commit install

# Install all hook environments (may take a few minutes)
pre-commit install --install-hooks
```

#### 3. Verify Installation

```bash
# Test the hooks
pre-commit run --all-files

# Or test on specific files
pre-commit run --files SRC/example.f
```

### Option 3: Container-based Setup

```bash
# Build the development container (includes pre-commit)
docker build -f MODERNIZATION/infrastructure/Dockerfile.dev -t lapack-ai-dev .

# Run setup inside container
docker run -it --rm -v $(pwd):/workspace lapack-ai-dev bash
cd /workspace
./MODERNIZATION/dev_environment/setup_precommit.sh
```

## Configuration Details

### fprettify Configuration (`.fprettify.rc`)

The project uses a conservative fprettify configuration designed for legacy LAPACK code:

```ini
# Conservative settings for legacy compatibility
indent = 2                    # 2-space indentation
line-length = 132            # Modern Fortran line length
whitespace = 1               # Conservative whitespace handling
case = 3 3 3 3              # Keep existing case unchanged
enable-decl = false         # Disable declaration formatting
c-relations = false         # Keep legacy operators (.gt., .lt., etc.)
enable-replacements = false # Don't modernize operators automatically
strict-indent = false       # Be lenient with legacy indentation
disable-fypp = true         # Disable fypp preprocessing
disable-indent-mod = true   # Don't change module indentation
```

### Pre-commit Configuration (`.pre-commit-config.yaml`)

Key fprettify hook configuration:

```yaml
- repo: https://github.com/fortran-lang/fprettify
  rev: v0.3.7
  hooks:
    - id: fprettify
      name: 'Format Fortran code with fprettify (conservative)'
      files: \.(f|f90|f95|f03|f08|F|F90|F95|F03|F08)$
      exclude: |
        (?x)^(
          .*/DEPRECATED/.*|
          .*/build/.*|
          .*/CMakeFiles/.*|
          .*\.mod$
        )$
      args:
        - --indent=2
        - --line-length=132
        - --whitespace=1
        - --disable-fypp
        - --disable-indent-mod
```

## Usage

### Automatic on Commit

Once installed, the hooks run automatically when you commit:

```bash
git add .
git commit -m "Add new feature"
# Pre-commit hooks run automatically and may modify files
# If files are modified, commit again to include the changes
```

### Manual Execution

```bash
# Run all hooks on all files
pre-commit run --all-files

# Run specific hook
pre-commit run fprettify
pre-commit run black
pre-commit run flake8

# Run on specific files
pre-commit run --files SRC/dgesvd.f SRC/dgemm.f

# Skip hooks for emergency commits
git commit --no-verify -m "Emergency fix"
```

### fprettify-specific Commands

```bash
# Preview formatting changes
fprettify --diff SRC/dgesvd.f

# Apply formatting in-place
fprettify SRC/dgesvd.f

# Format multiple files
fprettify SRC/*.f

# Use custom configuration
fprettify --config custom.rc SRC/dgesvd.f
```

## CI/CD Integration

### GitHub Actions

The project includes a comprehensive GitHub Actions workflow (`.github/workflows/code-quality.yml`) that:

- Runs all pre-commit hooks on every PR
- Validates Fortran syntax
- Checks Python code quality
- Scans for security issues with gitleaks
- Provides detailed feedback on code quality

### Local Testing

Before pushing, test the same checks that run in CI:

```bash
# Run the full quality check suite
pre-commit run --all-files

# Test specific file types
find SRC -name "*.f" | head -5 | xargs pre-commit run fprettify --files
find . -name "*.py" | head -5 | xargs pre-commit run black --files
```

## Troubleshooting

### Common Issues

#### fprettify Fails on Legacy Code

**Issue**: fprettify encounters parsing errors on complex legacy Fortran code.

**Solution**: 
1. The configuration is already conservative to handle most legacy code
2. For problematic files, you can exclude them in `.pre-commit-config.yaml`
3. Use `git commit --no-verify` for emergency commits
4. Report parsing issues to the fprettify project

#### Pre-commit Hooks Are Slow

**Issue**: First run takes a long time to install hook environments.

**Solution**:
```bash
# Cache the environments
pre-commit install --install-hooks

# Use Docker for consistent environments
docker run -v $(pwd):/workspace lapack-ai-dev pre-commit run --all-files
```

#### Formatting Conflicts

**Issue**: Different team members have different formatting.

**Solution**:
```bash
# Everyone should run the same setup
./MODERNIZATION/dev_environment/setup_precommit.sh

# Use container environment for consistency
docker run -v $(pwd):/workspace lapack-ai-dev bash
```

### Skipping Specific Hooks

```bash
# Skip fprettify for specific files
git commit -m "Fix urgent bug" --no-verify

# Or exclude in .pre-commit-config.yaml:
exclude: |
  (?x)^(
    SRC/problematic_file\.f|
    path/to/legacy/.*
  )$
```

### Updating Hook Versions

```bash
# Update all hooks to latest versions
pre-commit autoupdate

# Update specific hook
pre-commit autoupdate --repo https://github.com/fortran-lang/fprettify
```

## Best Practices

### For Fortran Development

1. **Run fprettify before committing**: `fprettify --diff` to preview changes
2. **Test compilation after formatting**: Ensure formatting doesn't break functionality
3. **Use conservative settings**: The current configuration is designed for safety
4. **Exclude generated files**: Build artifacts shouldn't be formatted

### For Team Collaboration

1. **Consistent setup**: All team members should use the same setup script
2. **Container development**: Use Docker for consistent environments
3. **Regular updates**: Keep hooks updated but test thoroughly
4. **Documentation**: Update this guide when changing configurations

### For Legacy Code Migration

1. **Incremental adoption**: Format files as you modify them
2. **Separate formatting commits**: Keep formatting changes separate from functional changes
3. **Backup before bulk formatting**: Always backup before running on many files
4. **Test thoroughly**: Verify that formatting doesn't break compilation or tests

## Integration with Development Workflow

### Container Development

The pre-commit hooks are integrated into the containerized development environment:

```bash
# Development container includes pre-commit
docker run -it --rm -v $(pwd):/workspace lapack-ai-dev bash

# Hooks work normally in container
git commit -m "New feature"  # Hooks run automatically
```

### VS Code Integration

For VS Code users, consider these extensions:
- **Modern Fortran**: Syntax highlighting and language support
- **fprettify**: Direct fprettify integration
- **Pre-commit**: Pre-commit hook management

### Performance Considerations

- **First-time setup**: May take 5-10 minutes to install all hook environments
- **Daily usage**: Hooks typically add 1-5 seconds to commit time
- **Large changesets**: Use `--files` flag to limit scope for faster iteration
- **Container caching**: Mount `.cache/pre-commit` for faster container startup

## Examples

### Before and After fprettify

**Before**:
```fortran
program example
implicit none
real::x,y
integer::i
do i=1,10
if(x.gt.y)then
print*,"result"
endif
enddo
end program
```

**After**:
```fortran
program example
  implicit none
  real :: x, y
  integer :: i
  do i = 1, 10
    if (x .gt. y) then
      print *, "result"
    endif
  enddo
end program
```

### Typical Workflow

```bash
# 1. Make changes to Fortran files
vim SRC/dgesvd.f

# 2. Preview formatting changes
fprettify --diff SRC/dgesvd.f

# 3. Commit (hooks run automatically)
git add SRC/dgesvd.f
git commit -m "Improve SVD algorithm"

# 4. If hooks modify files, commit again
git add .
git commit -m "Apply automatic formatting"

# 5. Push to trigger CI
git push origin feature-branch
```

## Contributing

When contributing to the pre-commit configuration:

1. **Test thoroughly**: Ensure changes don't break existing functionality
2. **Update documentation**: Keep this guide current
3. **Consider legacy compatibility**: LAPACK has decades of legacy code
4. **Performance impact**: Consider the impact on development workflow
5. **Team consensus**: Discuss major changes with the team

## Resources

- [fprettify Documentation](https://github.com/fortran-lang/fprettify)
- [Pre-commit Documentation](https://pre-commit.com/)
- [Fortran Style Guides](https://fortran-lang.org/en/learn/best_practices/style_guide/)
- [LAPACK Coding Standards](http://www.netlib.org/lapack/lapack_lug.html)

---

This comprehensive pre-commit setup ensures consistent code quality across the LAPACK AI Modernization project while respecting the legacy nature of the LAPACK codebase. 
