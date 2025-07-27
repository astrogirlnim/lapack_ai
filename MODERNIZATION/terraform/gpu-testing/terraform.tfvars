# =================================================================
# ALPHATENSOR GPU TESTING - CONFIGURATION
# =================================================================

# AWS Configuration - Use us-east-1 for lowest costs
aws_region = "us-east-1"

# Instance Configuration - Cost-optimized for testing
instance_type    = "g4dn.xlarge"    # NVIDIA T4 - $0.526/hr on-demand, ~$0.20/hr spot
max_spot_price   = "0.25"           # Conservative spot price limit

# Repository Settings - Update these for your repo
github_repo_url = "https://github.com/astrogirlnim/lapack_ai.git"  # ⚠️ UPDATE THIS
test_branch     = "more-optimization-and-gpu-testing"

# S3 Configuration - Must be globally unique
results_bucket_name = "nataly-alphatensor-gpu-test-results"

# Safety Configuration - Prevent runaway costs
test_timeout_minutes  = 20    # Max time for tests to run
auto_shutdown_minutes = 25    # Force shutdown after this time (safety net)

# Monitoring
enable_detailed_monitoring = true

# Environment
environment = "dev"

# SSH Access for debugging (set to true if you need to debug)
enable_ssh_access = false

# =================================================================
# ESTIMATED COSTS (Conservative Limits)
# =================================================================
#
# Instance: g4dn.xlarge @ $0.25/hr spot = $0.08 for 20-min test
# S3 Storage: ~$0.01 for results
# Total Cost: ~$0.10 per test run
#
# Safety: Auto-shutdown after 25 minutes maximum
# =================================================================
