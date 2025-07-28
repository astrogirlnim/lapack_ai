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

# Instance Lifecycle (OPTIMIZATION FEATURE)
# - Instance will be reused across terraform applies (cost optimization)
# - Use 'terraform destroy' then 'terraform apply' if you need fresh instance

# =================================================================
# USAGE PATTERNS (OPTIMIZED FOR COST & EFFICIENCY)
# =================================================================
#
# DEVELOPMENT WORKFLOW:
# 1. terraform apply     # Creates instance once
# 2. [make code changes]
# 3. terraform apply     # Reuses same instance (NO replacement!)
# 4. [more testing...]
# 5. terraform destroy   # Clean up when done
#
# COST OPTIMIZATION:
# - Instance reused across test runs = faster setup + lower cost
# - Auto-shutdown after 25 minutes = prevents runaway charges
# - Spot pricing @ $0.25 max = ~60% savings vs on-demand
#
# =================================================================

# =================================================================
# ESTIMATED COSTS (Conservative Limits)
# =================================================================
#
# Instance: g4dn.xlarge @ $0.25/hr spot = $0.08 for 20-min test
# S3 Storage: ~$0.01 for results
# Total Cost: ~$0.10 per test run
#
# Safety: Auto-shutdown after 25 minutes maximum
# Instance Reuse: Multiple tests on same instance = lower total cost
# =================================================================
